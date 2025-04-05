use egui_macroquad::macroquad;
use lenia::pattern::{kernel_core::*, kernel_shell::*};
use lenia::{Channel, GrowthFunction, Kernel};
use macroquad::prelude::*;
use std::sync::Arc;

const WIDTH: u16 = 1 << 8;
const HEIGHT: u16 = WIDTH;

#[macroquad::main("Lenia")]
async fn main() {
    let growth_mapping = GrowthFunction::new(|x| x);
    let mut kernel = Kernel::new(
        150,
        Vec::from([1., 0., 0., 0.]),
        Arc::from(trapezoidal),
        Arc::from(bimodal),
    );
    let _channel = Channel::new(
        (WIDTH as usize, HEIGHT as usize),
        growth_mapping,
        &kernel,
        10,
        1e4 as u32,
        Vec::<f32>::new(),
        0.3,
        0.03,
    );

    kernel.set_beta(Vec::from([0.; 4])); // example

    let mut cpu_img = Image::gen_image_color(WIDTH, HEIGHT, BLACK);
    let gpu_img = Texture2D::from_image(&cpu_img);

    loop {
        egui_macroquad::ui(|_| {});
        egui_macroquad::draw();

        // for (sv, dst) in std::iter::zip(&channel.lattice, cpu_img.get_image_data_mut()) {
        //     let color = colorous::RAINBOW.eval_continuous(*sv as f64);
        //     dst[..3].copy_from_slice(&color.as_array());
        // }

        for (sv, dst) in std::iter::zip(&kernel.kernel, cpu_img.get_image_data_mut()) {
            let color = colorous::RAINBOW.eval_continuous(*sv as f64);
            dst[..3].copy_from_slice(&color.as_array());
        }

        gpu_img.update(&cpu_img);

        draw_texture(gpu_img, 0., 0., WHITE);

        next_frame().await;
    }
}
