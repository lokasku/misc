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

let default_expect =
  { should_parse= true
  ; should_compile= true
  ; tokens= None
  ; errors= []
  ; warnings= [] }

let lex_all src =
  let buf = Sedlexing.Utf8.from_string src in
  let lexer = Lys.Lexer.lexer buf in
  let rec aux acc =
    match[@warning "-4"] lexer () with
    | Lys.Menhir_parser.EOF, _, _ ->
        (* Because of the way we were accumulating the tokens, we need to
           reverse the order *)
        List.rev (Lys.Menhir_parser.EOF :: acc)
    | tok, _, _ ->
        aux (tok :: acc)
  in
  aux []

let analyze src =
  let buf = Sedlexing.Utf8.from_string src in
  let ast = Lys.Parser.parse buf in
  let semantics = Lys.Semantic_analysis.analyze_program ast in
  let occurrences = Lys.Occurrence_analysis.analyze ast in
  {ast; semantics; occurrences}

let make_span start fin : span = {start; fin}

(**
 * Handy functon which builds a test that can validate lexing, parsing, and semantic analysis
 * against expected outputs (called "predicates").
 *
 * @param src The Lys src code to analyze
 * @param tokens Optional list of expected lexical tokens
 * @param errors Optional list of expected semantic errors
 * @param warnings Optional list of expected warnings with their associated names
 * @return A test_case record that can be passed to run_test
*)
let make_test src ?tokens ?(errors = []) ?(warnings = []) () =
  let expect = {default_expect with tokens; errors; warnings} in
  {src; expect}

let run_test case =
  try
    match case.expect.tokens with
    | Some expected_tokens ->
        lex_all case.src = expected_tokens
    | None ->
        let result = analyze case.src in
        (* Check that all the expected errors effectively occur in the result *)
        let errors_match =
          List.for_all
            (fun expected ->
              List.exists (( = ) expected) result.semantics.errors )
            case.expect.errors
        in
        (* Idem *)
        let warnings_match =
          List.for_all
            (fun (name, expected) ->
              Hashtbl.find_opt result.semantics.warnings name = Some expected )
            case.expect.warnings
        in
        (* If expected errors and warnings were correctly found, then the test
           passes *)
        errors_match && warnings_match
  with _ -> not case.expect.should_parse
