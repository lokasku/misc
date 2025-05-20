(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

(* TODO: Give expected AST. *)

open Utils

let%test_module "Parser" =
  ( module struct
    let%test "Basic binding" = run_test (make_test "def x = 42" ())

    let%test "Function definition" =
      run_test (make_test "def add x y = x + y" ())

    let%test "Type definition" =
      run_test (make_test "def Option := a { Some as a; None; }" ())

    let%test "Pattern matching" =
      run_test
        (make_test
           {|def sum xs = match xs {
           [] -> 0;
           [x ...xs] -> x + sum xs;
         }|}
           () )

    let%test "Lambda expression" =
      run_test (make_test "def compose f g = \\x -> f (g x)" ())

    let%test "Invalid syntax" =
      let test = make_test "def actoriser blajlbalblalb ((((((pega))))))" () in
      run_test {test with expect= {default_expect with should_parse= false}}
  end )
