(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

open Sedlexing.Utf8
open Menhir_parser

exception Lexing_error of Lexing.position * Lexing.position

let digit = [%sedlex.regexp? '0' .. '9']

let exp = [%sedlex.regexp? 'e' | 'E', Opt ('+' | '-'), Plus digit]

let integer = [%sedlex.regexp? Plus digit, Opt exp]

let floating = [%sedlex.regexp? Plus digit, '.', Plus digit, Opt exp]

let alnum = [%sedlex.regexp? lowercase | uppercase | digit]

let uid = [%sedlex.regexp? uppercase, Star alnum, Star '\'']

let lid = [%sedlex.regexp? (lowercase | '_'), Star alnum, Star '\'']

let string buf =
  let buffer = Buffer.create 16 in
  let rec aux buf =
    match%sedlex buf with
    | {|\"|} ->
        Buffer.add_char buffer '"' ; aux buf
    | Compl '"' ->
        Buffer.add_string buffer (lexeme buf) ;
        aux buf
    | '"' ->
        STRING (Buffer.contents buffer)
    | _ ->
        assert false
  in
  aux buf

let comment buf =
  let buffer = Buffer.create 160 in
  let rec aux buf =
    match%sedlex buf with
    | Compl '`' ->
        Buffer.add_string buffer (lexeme buf) ;
        aux buf
    | '`' ->
        COMMENT (Buffer.contents buffer)
    | _ ->
        assert false
  in
  aux buf

let rec tokenizer buf =
  match%sedlex buf with
  | "def" ->
      KWDEF
  | "let" ->
      KWLET
  | "in" ->
      KWIN
  | "if" ->
      KWIF
  | "then" ->
      KWTHEN
  | "else" ->
      KWELSE
  | "Int" ->
      KWINT
  | "Float" ->
      KWFLOAT
  | "Bool" ->
      KWBOOL
  | "String" ->
      KWSTRING
  | "Unit" ->
      KWUNIT
  | "match" ->
      KWMATCH
  | "as" ->
      KWAS
  | '=' ->
      EQUAL
  | ":=" ->
      COLONEQUAL
  | ':' ->
      COLON
  | '{' ->
      LBRACE
  | '}' ->
      RBRACE
  | '(' ->
      LPAREN
  | ')' ->
      RPAREN
  | '[' ->
      LBRACKET
  | ']' ->
      RBRACKET
  | ',' ->
      COMMA
  | "->" ->
      ARROW
  | ';' ->
      SEMICOLON
  | "()" ->
      UNIT
  | '\\' ->
      BACKSLASH
  | "..." ->
      ELLIPSIS
  | "|>" ->
      TRIANGLE
  | "||" ->
      BARBAR
  | "&&" ->
      ANDAND
  | "==" ->
      EQUALEQUAL
  | "!=" ->
      BANGEQUAL
  | '>' ->
      GT
  | ">=" ->
      GEQ
  | '<' ->
      LT
  | "<=" ->
      LEQ
  | "++" ->
      PLUSPLUS
  | '+' ->
      PLUS
  | '-' ->
      MINUS
  | '*' ->
      STAR
  | '/' ->
      SLASH
  | '%' ->
      PERCENT
  | "**" ->
      STARSTAR
  | '!' ->
      BANG
  | white_space ->
      tokenizer buf
  | eof ->
      EOF
  | '`' ->
      comment buf
  | integer ->
      INT (lexeme buf |> int_of_string)
  | floating ->
      FLOAT (lexeme buf |> float_of_string)
  | "True" ->
      BOOL true
  | "False" ->
      BOOL false
  | '"' ->
      string buf
  | uid ->
      UID (lexeme buf)
  | lid ->
      LID (lexeme buf)
  | _ ->
      let start, fin = Sedlexing.lexing_positions buf in
      raise @@ Lexing_error (start, fin)

let lexer buf = Sedlexing.with_tokenizer tokenizer buf
