<!--
SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>

SPDX-License-Identifier: CC-BY-SA-4.0
-->

# Contributing to Lys

Thank you for your interest in contributing to Lys! We welcome contributions from everyone. This document outlines the process for contributing to the project.

## Setting up the development environment

To contribute to Lys, you’ll need to set up an OCaml development environment. If you are using [nix-direnv](https://github.com/nix-community/nix-direnv), you can skip this section.

Follow these steps:

1. [Install](https://ocaml.org/docs/installing-ocaml) [OCaml](https://ocaml.org/) version 5.0.0 or later along side [opam](https://opam.ocaml.org/) (which is the preferred install method).

2. Clone the Lys repository:

   ```sh
   git clone https://github.com/lyslang/lys.git
   cd lys
   ```

3. Install project dependencies:

   ```sh
   opam install --deps-only .
   ```

4. Build the project:

   ```sh
   dune build
   ```

## Contributing workflow

1. Fork the repository and create your branch from `main`.
2. Make your changes, ensuring they adhere to the project’s coding style and conventions.
3. Update the documentation if necessary.
4. Update the `.gitignore` if you think it will be useful for others.
5. Test your changes thoroughly.
6. Create a pull request with a clear description of your changes.

## Code style

Please adhere to the coding style used throughout the project. Consistency in code style makes the project easier to read and maintain. Some key points:

- Use functional programming paradigms where appropriate.
- Write concise and elegant code.
- Leverage pattern matching and other functional features of OCaml.

## Documentation

If you’re adding new features or making significant changes, please update the relevant documentation. This includes:

- Updating the `README.md` if necessary.
- Adding or updating comments in the code.
- Updating or creating new documentation in the `docs` directory.

## Testing

Before submitting your changes, ensure the executable still runs as expected. It’s a good idea to add tests if you’re introducing new features or fixing a bug, helping to ensure the stability of Lys in the future.

## Licensing and REUSE compliance

Our project follows the [REUSE initiative](https://reuse.software/) guidelines to make handling licensing straightforward and transparent.

Please include a license header in every file you create or modify. A tutorial on how to do this is [available here](https://reuse.software/tutorial/).

If you’re unsure about which license to use or how to add it, please ask for guidance in your pull request or issue.

## Getting help

If you need help or have questions, you can:

- Open an issue in the [GitHub repository](https://github.com/lyslang/lys/issues).
- Leave comments on existing issues or pull requests.
- Reach out to the maintainers directly.

## Additional resources

- [Lys wiki](https://github.com/lyslang/lys/wiki)
- [VS Code extension](https://github.com/lyslang/vscode-lys)
- [OCaml documentation](https://ocaml.org/docs/)
- [Dune documentation](https://dune.readthedocs.io/)

We appreciate your contributions and are excited to see what you bring to Lys. Thank you for contributing!
