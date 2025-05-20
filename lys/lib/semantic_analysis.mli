(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

type arity = int

val pp_arity : Format.formatter -> arity -> unit

val show_arity : arity -> string

type info = Ast.span * arity

type name = string

val pp_name : Format.formatter -> name -> unit

val show_name : name -> string

type map = (name, info) Hashtbl.t

type error =
  | AlreadyDefined of {prev: Ast.span; new': name; newest_span: Ast.span}
  | Undefined of {name: name; span: Ast.span}
  | ReservedName of {name: name; span: Ast.span}
  | NotACallee of {name: name; span: Ast.span}
  | ArityMismatch of {name: name; expected: arity; span: Ast.span}

val pp_error : Format.formatter -> error -> unit

val show_error : error -> string

type warning = Unused of {name: name; span: Ast.span}

val pp_warning : Format.formatter -> warning -> unit

val show_warning : warning -> string

type analysis_output =
  { mutable errors: error list
  ; mutable warnings: (name, warning) Hashtbl.t
  ; mutable types: map
  ; mutable variants: map }

val analyze_program : Ast.program -> analysis_output

val debug_output : analysis_output -> unit

val get_errors : analysis_output -> error list
