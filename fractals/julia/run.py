#!/usr/bin/env python3

import argparse
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path
from src.julia import create_complex_matrix, new

def parse_arguments():
    parser = argparse.ArgumentParser(
        prog="jfg",
        description="Julia Fractal Generator - Create beautiful Julia set fractals",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )

    parser.add_argument(
        "pixeldensity",
        type=int,
        help="Resolution of the output image"
    )

    parser.add_argument(
        "detaildensity",
        type=int,
        help="Level of detail in the fractal"
    )

    parser.add_argument(
        "c",
        type=float,
        nargs=2,
        help="Complex parameter c (real and imaginary parts)"
    )

    parser.add_argument(
        "-c", "--colors",
        type=str,
        default="gray",
        help="Color theme for the fractal (matplotlib colormap)"
    )

    parser.add_argument(
        "-s", "--save",
        type=str,
        help="Save the figure to specified filename"
    )

    parser.add_argument(
        "-b", "--bounds",
        type=float,
        nargs=4,
        default=[-2, 2, -2, 2],
        help="Plot bounds: xmin xmax ymin ymax"
    )

    args = parser.parse_args()

    if args.pixeldensity <= 0 or args.detaildensity <= 0:
        parser.error("Density values must be positive")

    return args

def generate_julia_set(args):
    c = complex(args.c[0], args.c[1])
    xmin, xmax, ymin, ymax = args.bounds

    try:
        plan = create_complex_matrix(xmin, xmax, ymin, ymax, args.pixeldensity)
        return new(plan, c, args.detaildensity)
    except Exception as e:
        print(f"Error generating Julia set: {e}")
        exit(1)

def plot_julia_set(julia_set, args):
    plt.figure(figsize=(10, 10))
    plt.axis('off')

    try:
        plt.imshow(julia_set, cmap=args.colors)

        if args.save:
            save_path = Path(args.save)
            if not save_path.parent.exists():
                save_path.parent.mkdir(parents=True)
            plt.savefig(save_path, bbox_inches='tight', dpi=300)
            print(f"Figure saved as {args.save}")

        plt.show()

    except Exception as e:
        print(f"Error plotting/saving the fractal: {e}")
        exit(1)

def main():
    args = parse_arguments()
    julia_set = generate_julia_set(args)
    plot_julia_set(julia_set, args)

if __name__ == "__main__":
    main()
