(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

open Lys

let parse ?(json = false) buf =
  try Parser.parse buf with
  | Lexer.Lexing_error (start, fin) ->
      let code = 1 in
      let msg = "Unexpected character." in
      Reporter.create_report Reporter.Error code msg (start, fin) ~json ;
      exit 1
  | Parser.Syntax_error (code, msg, start, fin) ->
      let code = code + 2 in
      Reporter.create_report Reporter.Error code msg (start, fin) ~json ;
      exit code

let analyze ast =
  let output = Semantic_analysis.analyze_program ast in
  Semantic_analysis.debug_output output ;
  if Semantic_analysis.get_errors output = [] then
    Occurrence_analysis.analyze ast |> Ast.show_program |> print_endline

let transpile_file file =
  Sedlexing.Utf8.from_channel (open_in_bin file)
  |> parse ~json:true |> Transpiler.build_program
  |> Printf.fprintf (open_out_bin (file ^ ".ml")) "%s%!"

let transpile_stdin () =
  Sedlexing.Utf8.from_channel stdin
  |> parse ~json:true |> Transpiler.build_program |> print_string ;
  flush stdout

let format_file file =
  Sedlexing.Utf8.from_channel (open_in_bin file)
  |> parse ~json:true |> Formatter.format
  |> Printf.fprintf (open_out_bin file) "%s%!"

let format_stdin () =
  Sedlexing.Utf8.from_channel stdin
  |> parse ~json:true |> Formatter.format |> print_string ;
  flush stdout

let parse_file file =
  let buf = Sedlexing.Utf8.from_channel (open_in_bin file) in
  Sedlexing.set_filename buf file ;
  let ast = parse buf in
  Ast.show_program ast |> print_endline ;
  analyze ast

let parse_stdin () =
  let ast = Sedlexing.Utf8.from_channel stdin |> parse in
  Ast.show_program ast |> print_endline ;
  analyze ast

let print_usage () =
  print_endline
    {|╭─╴Error╶──────────────────────────────────────────────────────────────────╮
│ Invalid arguments.                                                       │
╰──────────────────────────────────────────────────────────────────────────╯

╭──────────────────────┬───────────────────────────────────────────────────╮
│ Command              │ Description                                       │
├──────────────────────┼───────────────────────────────────────────────────┤
│ lys                  │ Parse standard input and display the AST.         │
├──────────────────────┼───────────────────────────────────────────────────┤
│ lys <file>           │ Parse a file and display the AST.                 │
├──────────────────────┼───────────────────────────────────────────────────┤
│ lys fmt              │ Format standard input and output it.              │
├──────────────────────┼───────────────────────────────────────────────────┤
│ lys fmt <file>       │ Format a file and overwrite it.                   │
├──────────────────────┼───────────────────────────────────────────────────┤
│ lys transpile        │ Transpile standard input and output it.           │
├──────────────────────┼───────────────────────────────────────────────────┤
│ lys transpile <file> │ Transpile a file and write its OCaml counterpart. │
╰──────────────────────┴───────────────────────────────────────────────────╯|} ;
  exit 1

let () =
  match Sys.argv with
  | [|_; "transpile"; file|] ->
      transpile_file file
  | [|_; "transpile"|] ->
      transpile_stdin ()
  | [|_; "fmt"; file|] ->
      format_file file
  | [|_; "fmt"|] ->
      format_stdin ()
  | [|_; file|] ->
      parse_file file
  | [|_|] ->
      parse_stdin ()
  | _ ->
      print_usage ()
