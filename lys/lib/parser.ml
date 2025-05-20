(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

open MenhirLib.General
open Menhir_parser.MenhirInterpreter

exception Syntax_error of int * string * Lexing.position * Lexing.position

let state checkpoint =
  match Lazy.force (stack checkpoint) with
  | Nil ->
      0
  | Cons (Element (s, _, _, _), _) ->
      number s

let handle_syntax_error buf checkpoint =
  let code = state checkpoint in
  let msg =
    try Parser_errors.message code with Not_found -> "Unknown syntax error."
  in
  let start, fin = Sedlexing.lexing_positions buf in
  raise @@ Syntax_error (code, msg, start, fin)

let rec loop next_token buf (checkpoint : Ast.program checkpoint) =
  match checkpoint with
  | InputNeeded _ ->
      next_token () |> offer checkpoint |> loop next_token buf
  | Shifting _ | AboutToReduce _ ->
      resume checkpoint |> loop next_token buf
  | HandlingError env ->
      handle_syntax_error buf env
  | Accepted prog ->
      prog
  | Rejected ->
      assert false

let parse buf =
  let lexer = Lexer.lexer buf in
  let checkpoint =
    Sedlexing.lexing_positions buf |> fst |> Menhir_parser.Incremental.program
  in
  loop lexer buf checkpoint
