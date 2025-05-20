(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

open Utils

let%test_module "Occurrence Analysis" =
  ( module struct
    let%test "Value definition" = run_test (make_test "def x = 2" ())

    let%test "Function definition" =
      run_test (make_test "def add x y = x + y" ())

    let%test "Let bindings" =
      run_test
        (make_test
           {|def main =
            let x = 1;
                f = \y -> x + y
            in f 2|}
           () )

    let%test "Mutual recursion" =
      run_test
        (make_test
           {|
         def even n = if n == 0 then True else odd (n - 1)
         def odd n = if n == 0 then False else even (n - 1)
         |}
           () )
  end )
