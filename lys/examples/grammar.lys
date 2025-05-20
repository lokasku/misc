`
  SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
  SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>

  SPDX-License-Identifier: CC0-1.0
`

`
  Only multiline comments are supported, surrounded by backticks.
`

`
  Type aliases allow defining new names for existing types.
  Here we define a 2D point as a tuple of two Float values.
`

def Point := (Float, Float)

`
  Algebraic Data Types (ADTs) enable defining custom types with multiple variants.
  Each variant can optionally carry data of any type.
  Type constructors must start with an uppercase letter, PascalCase is preferred.
`

def Shape := {
  Circle as Float;
  Rectangle as (Float, Float);
  Square as (Float, Float);
}

`
  Parametric polymorphism is supported through type parameters.
  The single lowercase ‘a’ is a type variable that can be any type.
`

def Option := a {
  Some as a;
  None;
}

`
  Constants are defined using ‘def’ and starts with a lowercase letter, camelCase is preferred.
  The type annotation is optional and can be inferred by the compiler.
  Basic types include Float, Int, String, Bool and Unit.
`

def pi : Float = 3.14159

`
  Integer literals can be signed.
`

def offset : Int = -16

`
  Strings are enclosed in double quotes.
`

def greeting : String = "Hello"

`
  Multiline strings and escape sequences are supported by default.
`

def loremIpsum : String = "Lorem \"ipsum\" dolor sit amet.
Lorem ipsum dolor sit amet."

`
  Functions can take multiple parameters using currying.
  Type signatures are specified with the ‘->’ notation.
  Tuples can be destructured directly in parameter position.
`

def distance (x1, y1) (x2, y2) : Point -> Point -> Float =
  let dx = x2 - x1; dy = y2 - y1
  in sqrt (dx ** 2.0 + dy ** 2.0)

`
  Pattern matching allows handling different cases based on data structure.
  Guard expressions can be added with ‘if’ to further refine patterns.
  The wildcard ‘_’ matches any value.
`

def area shape : Shape -> Float =
  match shape {
    Circle r if r > 0.0 -> π * r * r;
    Rectangle (w, h); Square (w, h) -> w * h;
    _ -> -1;
  }

`
  Conditional expressions use standard if-then-else syntax.
`

def safeDivide x y : Int -> Float -> Option Float =
  if y == 0.0 then None else Some (x / y)

`
  Local bindings are defined using ‘let … in’ expressions.
  The underscore ‘_’ as a function name indicates a top-level expression
  that will be evaluated at runtime but not exported.

  Standard library functions include ‘map’ for list transformation.
`

def _ =
  let
    point1 = (0.0, 0.0);
    point2 = (3.0, 4.0);
    circle = Circle 5.0;
    rectangle = Rectangle (2.0, 3.0)
  in map printNumber [distance point1 point2, area circle, area rectangle]

`
  Lambda (anonymous) functions are defined with backslash syntax ‘\param1 p2 ... pn -> body’.
  List literals are created with square brackets.
`

def mapped = map (\z -> 2 * 2) [1, 2, 3, 4]

`
  The standard library includes common functional programming utilities
  like ‘filter’ for selecting elements that satisfy a predicate.
`

def filtered = filter (\x -> x > 2) [1, 2, 3, 4]

`
  ‘foldLeft’ accumulates values from left to right with an initial value.
  The accumulator comes first in the function parameter order.
`

def foldedLeft = foldLeft (\acc x -> acc + x) 0 [1, 2, 3, 4]

`
  ‘foldRight’ accumulates values from right to left.
  Note the parameter order difference compared to foldLeft.
`

def foldedRight = foldRight (\x acc -> x + acc) [1, 2, 3, 4] 0

`
  Pattern matching on lists supports empty list and head/tail destructuring.
  The ‘...’ syntax in patterns denotes the tail/rest of the list.
  Recursive functions are naturally expressed through self-reference.
`

def product lst = match lst { [] -> 0; [hd ...tl] -> hd * product tl; }

def sum lst = match lst { [] -> 0; [hd ...tl] -> hd + sum tl; }

`
  Recursive functions with termination conditions are a common pattern.
  No explicit ‘rec’ keyword is needed for recursion.
`

def fib n : Int -> Int = if n <= 1 then n else fib (n - 1) + fib (n - 2)
