<!--
SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>

SPDX-License-Identifier: CC-BY-SA-4.0
-->

# Lys

The Lys purely functional programming language.

## Features

- **Purely functional**
  - No side effects
  - Pattern matching
  - Polymorphism
- **Practical syntax**
  - Concise
  - Elegant
  - Seamless currying support
- **Type inference** (WIP)
  - Hindley-Milner type system
- **Helpful error messages**
- **Unicode support**

## Examples

### Summation
```lys
def sum lst =
  match lst {
    [] -> 0;
    [hd, tl...] -> hd + sum tl;
  }
```

### Fibonacci
```lys
def fib n : Int -> Int =
  if n <= 1 then n else fib (n - 1) + fib (n - 2)
```

## Usage
Lys files use the `.lys` extension.

### Basic Commands
| Command                | Description                                       |
| :--------------------- | :------------------------------------------------ |
| `lys <file>`           | Parse a file and display the AST                  |
| `lys fmt <file>`       | Format a file                                     |
| `lys transpile <file>` | Generate OCaml code from a Lys file               |

### Running Transpiled Code
After transpiling, run the generated OCaml code with:

```sh
ocaml <file.ml>
```

## Installation
1. Download the Lys executable for your system
2. Make it executable: chmod +x /path/to/lys
3. Add it to your PATH


## Credits
The Lys programming language was developed by [Lukas](https://github.com/lokasku) and [Aljebriq](https://github.com/aljebriq).

## License
This project uses multiple licenses following the REUSE Initiative. See individual files for license information.