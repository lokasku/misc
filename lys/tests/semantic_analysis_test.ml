(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

open Utils
open Lys.Semantic_analysis

let%test_module "Semantic Analysis" =
  ( module struct
    let%test "Undefined variable" =
      run_test
        (make_test "def x = y"
           ~errors:[Undefined {name= "y"; span= make_span 8 9}]
           () )

    let%test "Duplicate binding" =
      run_test
        (make_test "def x = 1\ndef x = 2"
           ~errors:
             [ AlreadyDefined
                 {prev= make_span 4 5; new'= "x"; newest_span= make_span 14 15}
             ]
           () )

    let%test "Unused variable warning" =
      run_test
        (make_test "def x = 42"
           ~warnings:[("x", Unused {name= "x"; span= make_span 4 5})]
           () )

    let%test "Valid recursion" =
      run_test
        (make_test
           {|
         def even n = if n == 0 then True else odd (n - 1)
         def odd n = if n == 0 then False else even (n - 1)
         |}
           () )
  end )
