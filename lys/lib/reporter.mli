(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

type severity = Info | Warning | Error

val create_report :
     ?json:bool
  -> ?hint:string
  -> ?note:string
  -> severity
  -> int
  -> string
  -> Lexing.position * Lexing.position
  -> unit
