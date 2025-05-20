(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

type span = {start: int; fin: int}

and 'a located = {loc: span; value: 'a}

and uid = string

and lid = string

and program = declaration list

and declaration =
  | Comment of string
  | ValueBinding of binding
  | TypeDefinition of {id: uid located; body: typing}
  | FunctionDefinition of
      { id: lid located
      ; parameters: parameter list
      ; signature: signature
      ; body: expression }
  | AdtDefinition of
      {id: uid located; polymorphics: lid located list; variants: variant list}

and binding = {id: lid located; signature: signature; body: expression}

and signature = typing option

and parameter = ALid of lid located | ATuple of parameter list

and typing =
  | TInt
  | TFloat
  | TBool
  | TString
  | TUnit
  | TConstructor of {id: uid located; typing: typing option}
  | TPolymorphic of lid located
  | TTuple of typing list
  | TList of typing
  | TFunction of {l: typing; r: typing}

and expression =
  | Int of int
  | Float of float
  | Bool of bool
  | String of string
  | Unit
  | Uid of uid located
  | Lid of lid located
  | Tuple of expression list
  | List of expression list
  | BinaryOperation of {l: expression; operator: binary_operator; r: expression}
  | UnaryOperation of {operator: unary_operator; body: expression}
  | Let of {bindings: binding list; body: expression}
  | If of {predicate: expression; truthy: expression; falsy: expression}
  | Match of {body: expression; cases: case list}
  | Lambda of {parameters: parameter list; body: expression}
  | Application of {body: expression; argument: expression}
  | Expression of {body: expression; signature: signature}

and binary_operator =
  | BPipe
  | BOr
  | BAnd
  | BEqual
  | BNotEqual
  | BGreaterThan
  | BGreaterOrEqual
  | BLessThan
  | BLessOrEqual
  | BConcatenate
  | BAdd
  | BSubstract
  | BMultiply
  | BDivide
  | BModulo
  | BExponentiate

and unary_operator = UPlus | UMinus | UNot

and variant = {id: uid located; typing: typing option}

and case = {pattern: pattern; guard: expression option; body: expression}

and pattern =
  | PInt of int
  | PFloat of float
  | PBool of bool
  | PString of string
  | PLid of lid located
  | PTuple of pattern list
  | PList of pattern list
  | PListSpread of pattern list
  | PConstructor of {id: uid located; pattern: pattern option}
  | POr of {l: pattern; r: pattern}
[@@deriving show {with_path= false}]
