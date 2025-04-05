#![allow(dead_code)]
#![feature(trait_alias)]

use ndarray::{Array2, Zip};
use rand::{thread_rng, Rng};
use std::sync::Arc;

pub mod pattern;

type KernelCore = dyn Fn(f32, f32) -> f32;
type KernelShell = dyn Fn(f32, Vec<f32>, Arc<KernelCore>) -> f32;

pub struct Channel<'a> {
    /// d-dimensional *lattice* or *grid* denoted 𝓛.
    pub lattice: Array2<f32>,

    growth_function: GrowthFunction,
    pub kernel: &'a Kernel,

    /// Steps per unit time
    /// Δ𝑡 = 1/T (time step) and 𝑑𝑡 when T → ∞
    t: u32,

    /// Corresponds to state resolutions
    /// Δ𝑝 = 1/P (state precision) and 𝑑𝑝 when P → ∞
    p: u32,

    /// State set, often [0;1] in Lenia.
    /// 𝐒 = { 0/P, 1/P, 2/P, ..., P-1/P, P/P }
    /// e.g. with P=4, we have S = { 0, 0.25, 0.5, 0.75, 1 }
    s: Vec<f32>,

    /// Growth center : μ ∈ ℝ
    mu: f32,

    /// Growth width : σ ∈ ℝ
    sigma: f32,
}

impl<'a> Channel<'a> {
    pub fn new(
        (width, height): (usize, usize),
        g: GrowthFunction,
        k: &'a Kernel,
        time: u32,
        precision: u32,
        state_set: Vec<f32>,
        mu: f32,
        sigma: f32,
    ) -> Self {
        Self {
            lattice: Array2::zeros((width, height))
                .map_mut(|_: &mut f32| thread_rng().gen_range(0.0..1.)),
            growth_function: g,
            kernel: k,
            t: time,
            p: precision,
            s: state_set,
            mu,
            sigma,
        }
    }
}

/// The kernel 𝐊 is composed of a kernel core, which specifies its intricate "texture",
/// and a kernel shell, which defines its overarching "structure" (especially concentric rings).
#[derive(Clone)]
pub struct Kernel {
    /// Cells per kernel radius
    /// Δ𝑥 = 1/𝑅 (site distance) and 𝑑𝑥 when R → ∞
    r: u32,

    /// Kernel peak vector β = (β₁, β₂, ..., βb) representing the peak value of each ring.
    beta: Vec<f32>,

    /// The kernel core function is any unimodal function of the form 𝐊c : [0,1] → [0,1] and which
    /// verifies 𝐊c(0) = 𝐊c(1) = 0 and where usually 𝐊(1/2) = 1.
    kc: Arc<KernelCore>,

    /// 𝐊s transforms the kernel's core function into a series of concentric rings controlled by β.
    /// It is defined as 𝐊s : [0,1] → [0,1]
    ks: Arc<KernelShell>,

    pub kernel: Array2<f32>,
    pub kernel_sum: f32,
    pub kernel_norm: Array2<f32>,
}

impl Kernel {
    pub fn new(
        r: u32,
        beta: Vec<f32>,
        kc: Arc<dyn Fn(f32, f32) -> f32 + 'static>,
        ks: Arc<dyn Fn(f32, Vec<f32>, Arc<KernelCore>) -> f32 + 'static>,
    ) -> Self {
        let (kernel, kernel_sum, kernel_norm) =
            Self::build_kernel(r, &beta, &kc, &ks);
        Self {
            r,
            beta,
            kc,
            ks,
            kernel,
            kernel_sum,
            kernel_norm,
        }
    }
    fn build_kernel(
        r: u32,
        beta: &Vec<f32>,
        kc: &Arc<dyn Fn(f32, f32) -> f32 + 'static>,
        ks: &Arc<dyn Fn(f32, Vec<f32>, Arc<KernelCore>) -> f32 + 'static>,
    ) -> (Array2<f32>, f32, Array2<f32>) {
        static SIZE: u16 = 1 << 8;
        let mid: u16 = SIZE / 2;
        let i = Array2::from_shape_fn((SIZE as usize, SIZE as usize), |(i, _)| i as f32);
        let x = (i - mid as f32) / r as f32;
        let y = x.t();
        let d = {
            let mut d = Array2::zeros((SIZE as usize, SIZE as usize));
            Zip::from(&mut d)
                .and(&x)
                .and(&y)
                .for_each(|d, &x_val, &y_val| {
                    *d = (x_val.powi(2) + y_val.powi(2)).sqrt();
                });
            d
        };
        let kernel = d.mapv(|elem| ks(elem, beta.clone(), kc.clone()));
        let kernel_sum = kernel.sum();
        let kernel_norm = kernel.clone() / kernel_sum;

        (kernel, kernel_sum, kernel_norm)
    }
    pub fn set_kernel_core(
        &mut self,
        new_kc: Arc<dyn Fn(f32, f32) -> f32 + 'static>
    ) {
        let (new_kernel, new_kernel_sum, new_kernel_norm) =
            Self::build_kernel(self.r, &self.beta, &new_kc, &self.ks);
        self.kernel = new_kernel;
        self.kernel_sum = new_kernel_sum;
        self.kernel_norm = new_kernel_norm;
        self.kc = new_kc;
    }

    pub fn set_kernel_shell(
        &mut self,
        new_ks: Arc<dyn Fn(f32, Vec<f32>, Arc<KernelCore>) -> f32 + 'static>,
    ) {
        let (new_kernel, new_kernel_sum, new_kernel_norm) =
            Self::build_kernel(self.r, &self.beta, &self.kc, &new_ks);
        self.kernel = new_kernel;
        self.kernel_sum = new_kernel_sum;
        self.kernel_norm = new_kernel_norm;
        self.ks = new_ks;
    }
    pub fn set_radius(
        &mut self,
        new_r: u32,
    ) {
        let (new_kernel, new_kernel_sum, new_kernel_norm) =
            Self::build_kernel(new_r, &self.beta, &self.kc, &self.ks);
        self.kernel = new_kernel;
        self.kernel_sum = new_kernel_sum;
        self.kernel_norm = new_kernel_norm;
        self.r = new_r;
    }
    pub fn set_beta(
        &mut self,
        new_beta: Vec<f32>,
    ) {
        let (new_kernel, new_kernel_sum, new_kernel_norm) =
            Self::build_kernel(self.r, &new_beta, &self.kc, &self.ks);
        self.kernel = new_kernel;
        self.kernel_sum = new_kernel_sum;
        self.kernel_norm = new_kernel_norm;
        self.beta = new_beta;
    }
}

/// The growth function is any unimodal function (with μ, σ ∈ ℝ) which sums the
/// weights returned by the convolution 𝐊 ∗ 𝐀.
pub struct GrowthFunction(
    /// G : [0,1] → [-1, 1]
    Arc<dyn FnOnce(f32) -> f32>,
);

impl GrowthFunction {
    pub fn new(g: impl FnOnce(f32) -> f32 + 'static) -> Self {
        Self(Arc::from(g))
    }
}
