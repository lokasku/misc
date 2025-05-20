(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

open Utils
open Lys.Menhir_parser

let%test_module "Lexer" =
  ( module struct
    let%test "Keywords" =
      run_test
        (make_test "def let in if then else match as"
           ~tokens:[KWDEF; KWLET; KWIN; KWIF; KWTHEN; KWELSE; KWMATCH; KWAS; EOF]
           () )

    let%test "Types" =
      run_test
        (make_test "Int Float Bool String Unit"
           ~tokens:[KWINT; KWFLOAT; KWBOOL; KWSTRING; KWUNIT; EOF]
           () )

    let%test "Operators" =
      run_test
        (make_test "= := : -> |> || && == != > >= < <= ++ \\"
           ~tokens:
             [ EQUAL
             ; COLONEQUAL
             ; COLON
             ; ARROW
             ; TRIANGLE
             ; BARBAR
             ; ANDAND
             ; EQUALEQUAL
             ; BANGEQUAL
             ; GT
             ; GEQ
             ; LT
             ; LEQ
             ; PLUSPLUS
             ; BACKSLASH
             ; EOF ]
           () )

    let%test "Numbers" =
      run_test
        (make_test "42 3.14 -1.5 0.0"
           ~tokens:[INT 42; FLOAT 3.14; MINUS; FLOAT 1.5; FLOAT 0.0; EOF]
           () )

    let%test "Strings" =
      run_test
        (make_test "\"a\" \"b\" \"c \\\"d\\\"\""
           ~tokens:[STRING "a"; STRING "b"; STRING "c \"d\""; EOF]
           () )

    let%test "Unicode" =
      run_test
        (make_test "π α β µ"
           ~tokens:[LID "π"; LID "α"; LID "β"; LID "µ"; EOF]
           () )
  end )
