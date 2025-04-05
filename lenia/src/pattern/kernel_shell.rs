use std::{cmp, sync::Arc};

pub fn unknow_kernel_shell(
    r: f32,
    beta: Vec<f32>,
    kc: Arc<dyn Fn(f32, f32) -> f32 + 'static>,
) -> f32 {
    let k = beta.len();
    let kr = (k as f32) * r;
    let idx = cmp::min(k - 1, kr.floor() as usize);
    let peak = beta[idx];

    if r < 1.0 {
        r * kc((kr % 1.0) as f32, 4f32) * peak
    } else {
        0.0
    }
}

pub fn unimodal(r: f32, _: Vec<f32>, kc: Arc<dyn Fn(f32, f32) -> f32 + 'static>) -> f32 {
    kc(r, 4.)
}

pub fn bimodal(r: f32, beta: Vec<f32>, kc: Arc<dyn Fn(f32, f32) -> f32 + 'static>) -> f32 {
    if r <= 0.5 {
        beta[0] * kc(2. * r, 4f32)
    } else if 1. / 2. <= r && r <= 1. {
        beta[1] * kc(2. * r - 1., 4f32)
    } else {
        0.
    }
}

pub fn trimodal(r: f32, beta: Vec<f32>, kc: Arc<dyn Fn(f32, f32) -> f32 + 'static>) -> f32 {
    if r <= 1. / 3. {
        beta[0] * kc(3. * r, 4f32)
    } else if r <= 2. / 3. {
        beta[1] * kc(3. * r - 1., 4f32)
    } else if r <= 1. {
        beta[2] * kc(3. * r - 2., 4f32)
    } else {
        0.
    }
}

pub fn tetramodal(r: f32, beta: Vec<f32>, kc: Arc<dyn Fn(f32, f32) -> f32 + 'static>) -> f32 {
    if r <= 1. / 4. {
        beta[0] * kc(4. * r, 4f32)
    } else if r <= 2. / 4. {
        beta[1] * kc(4. * r - 1., 4f32)
    } else if r <= 3. / 4. {
        beta[2] * kc(4. * r - 2., 4f32)
    } else if r <= 1. {
        beta[3] * kc(4. * r - 3., 4f32)
    } else {
        0.
    }
}
