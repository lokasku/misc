pub fn unknow_kernel_core(r: f32, _: f32) -> f32 {
    let rm = r.min(1.0);
    (4.0 * rm * (1.0 - rm)).powi(5)
}

pub fn gaussian_bump(r: f32, alpha: f32) -> f32 {
    let k = 4. * r * (1. - r);
    (alpha as f32 * (1. - 1. / k)).exp()
}

pub fn polynomial(r: f32, alpha: f32) -> f32 {
    (4. * r * (1. - r)).powf(alpha)
}

pub fn trapezoidal(r: f32, _: f32) -> f32 {
    let q: f32 = 1. / 5.;
    if q <= r && r <= 2. * q {
        (r - q) / q
    } else if 2. * q < r && r <= 1. - 2. * q {
        1.
    } else if 1. - 2. * q < r && r <= 1. - q {
        (1. - q - r) / q
    } else {
        0.
    }
}

pub fn step(r: f32, _: f32) -> f32 {
    let q = 1. / 4.;
    if (q <= r) && (r <= 1. - q) {
        1.
    } else {
        0.
    }
}
