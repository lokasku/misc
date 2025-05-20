(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

exception Lexing_error of Lexing.position * Lexing.position

val lexer :
     Sedlexing.lexbuf
  -> unit
  -> Menhir_parser.token * Lexing.position * Lexing.position
