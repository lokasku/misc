import numpy as np
import matplotlib.pyplot as plt


def create_complex_matrix(x1, x2, y1, y2, quality):
    x = np.linspace(x1, x2, int((x2 - x1) * quality))
    y = np.linspace(y1, y2, int((y2 - y1) * quality))
    return x[np.newaxis, :] + y[:, np.newaxis] * 1j

def is_stable(c, i):
    z = c
    for _ in range(i): z = z ** 2 + c
    return abs(z) <= 2

plan = create_complex_matrix(-2, 0.5, -1.5, 1.5, quality=1000)

plt.imshow(is_stable(plan, i=30), cmap="binary")
plt.show()