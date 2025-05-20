(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

open Lys.Ast
open Lys.Semantic_analysis

type token = Lys.Menhir_parser.token

type expectation =
  { should_parse: bool
  ; should_compile: bool
  ; tokens: token list option
  ; errors: error list
  ; warnings: (string * warning) list }

type test_case = {src: string; expect: expectation}

type test_result =
  {ast: program; semantics: analysis_output; occurrences: program}

val default_expect : expectation

val lex_all : string -> token list

val analyze : string -> test_result

val make_span : int -> int -> span

val make_test :
     string
  -> ?tokens:token list
  -> ?errors:error list
  -> ?warnings:(string * warning) list
  -> unit
  -> test_case

val run_test : test_case -> bool
