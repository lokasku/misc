import numpy as np
import matplotlib.pyplot as plt


def create_complex_matrix(x1, x2, y1, y2, pixel_density):
    x = np.linspace(x1, x2, int((x2 - x1) * pixel_density))
    y = np.linspace(y1, y2, int((y2 - y1) * pixel_density))
    return x[np.newaxis, :] + y[:, np.newaxis] * 1j 

@np.vectorize
def new(z0, c, i):
    z = z0
    for it in range(i):
        z = z ** 2 + c
        if abs(z) > 2: break
    return it
