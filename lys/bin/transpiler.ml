(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

open Lys.Ast

let first_type = ref true

let first_bind = ref true

let buffer = Buffer.create 1024

let add s = Buffer.add_string buffer s

let add_space () = Buffer.add_char buffer ' '

let add_newline () = Buffer.add_char buffer '\n'

let add_list sep f lst =
  match lst with
  | [] ->
      ()
  | [x] ->
      f x
  | x :: xs ->
      f x ;
      List.iter (fun y -> add sep ; f y) xs

let encode_lid str =
  match str with
  | "sqrt" ->
      "Float.sqrt"
  | "\u{03C0}" ->
      "Float.pi"
  | "map" ->
      "List.map"
  | "filter" ->
      "List.filter"
  | "foldLeft" ->
      "List.fold_left"
  | "foldRight" ->
      "List.fold_right"
  | "printString" ->
      "print_endline"
  | "printNumber" ->
      {|(Format.printf "%f@.")|}
  | _ ->
      "l" ^ string_of_int (String.hash str)

let encode_uid str = "U" ^ string_of_int (String.hash str)

let scoped content = add "(" ; content () ; add ")"

let is_type_decl decl =
  match decl with
  | Comment _ | ValueBinding _ | FunctionDefinition _ ->
      false
  | TypeDefinition _ | AdtDefinition _ ->
      true

let is_entry_point decl =
  match decl with
  | ValueBinding {id; signature= _; body= _} ->
      id.value = "_"
  | Comment _ | FunctionDefinition _ | TypeDefinition _ | AdtDefinition _ ->
      false

let rec build_program decls =
  Buffer.clear buffer ;
  first_type := true ;
  first_bind := true ;
  let type_decls, non_type_decls = List.partition is_type_decl decls in
  let entry_point_decls, other_decls =
    List.partition is_entry_point non_type_decls
  in
  let ordered = type_decls @ other_decls @ entry_point_decls in
  List.iter (fun d -> build_declaration d ; add_newline ()) ordered ;
  Buffer.contents buffer

and build_declaration declaration =
  match declaration with
  | Comment _ ->
      ()
  | ValueBinding {id; signature= _; body} ->
      if id.value = "_" then add "let"
      else if !first_bind then (
        add "let rec" ;
        first_bind := false )
      else add "and" ;
      add_space () ;
      add @@ encode_lid id.value ;
      add_space () ;
      add "=" ;
      add_space () ;
      build_expression body
  | TypeDefinition {id; body} ->
      if !first_type then (
        add "type" ;
        first_type := false )
      else add "and" ;
      add_space () ;
      add @@ encode_lid id.value ;
      add_space () ;
      add "=" ;
      add_space () ;
      build_typing body
  | FunctionDefinition {id; parameters; signature= _; body} ->
      if id.value = "_" then add "let"
      else if !first_bind then (
        add "let rec" ;
        first_bind := false )
      else add "and" ;
      add_space () ;
      add @@ encode_lid id.value ;
      add_space () ;
      List.iter (fun p -> build_parameter p ; add_space ()) parameters ;
      add "=" ;
      add_space () ;
      build_expression body
  | AdtDefinition {id; polymorphics; variants} ->
      if !first_type then (
        add "type" ;
        first_type := false )
      else add "and" ;
      add_space () ;
      List.iter
        (fun p ->
          add "'" ;
          add @@ encode_lid p.value ;
          add_space () )
        polymorphics ;
      add @@ encode_lid id.value ;
      add_space () ;
      add "=" ;
      add_space () ;
      add_list " " build_variant variants

and build_parameter parameter =
  match parameter with
  | ALid str ->
      add @@ encode_lid str.value
  | ATuple tuple_param ->
      scoped (fun () -> add_list "," build_parameter tuple_param)

and build_typing typing =
  match typing with
  | TInt ->
      add "float"
  | TFloat ->
      add "float"
  | TString ->
      add "string"
  | TBool ->
      add "bool"
  | TUnit ->
      add "unit"
  | TList t ->
      build_typing t ; add_space () ; add "list"
  | TTuple ts ->
      add_list "*" build_typing ts
  | TFunction {l; r} ->
      build_typing l ; add_space () ; add "->" ; add_space () ; build_typing r
  | TPolymorphic p ->
      add "'" ;
      add @@ encode_lid p.value
  | TConstructor {id; typing} -> (
      add @@ encode_lid id.value ;
      match typing with
      | Some t ->
          add_space () ; add "of" ; add_space () ; build_typing t
      | None ->
          () )

and build_expression expr =
  match expr with
  | Expression {body; signature= _} ->
      scoped (fun () -> build_expression body)
  | Int i ->
      float_of_int i |> string_of_float |> add
  | Float f ->
      add @@ string_of_float f
  | Bool b ->
      add @@ string_of_bool b
  | String s ->
      add "\"" ;
      add @@ String.escaped s ;
      add "\""
  | Unit ->
      add "()"
  | Uid id ->
      add @@ encode_uid id.value
  | Lid id ->
      add @@ encode_lid id.value
  | Tuple exprs ->
      scoped (fun () -> add_list "," build_expression exprs)
  | List exprs ->
      add "[" ;
      add_list ";" build_expression exprs ;
      add "]"
  | BinaryOperation {l; operator; r} ->
      build_expression l ;
      add_space () ;
      build_binary_operator operator ;
      add_space () ;
      build_expression r
  | UnaryOperation {operator; body} ->
      build_unary_operator operator ;
      add_space () ;
      build_expression body
  | Let {bindings; body} ->
      List.iter build_binding bindings ;
      build_expression body
  | If {predicate; truthy; falsy} ->
      add "if" ;
      add_space () ;
      build_expression predicate ;
      add_space () ;
      add "then" ;
      add_space () ;
      build_expression truthy ;
      add_space () ;
      add "else" ;
      add_space () ;
      build_expression falsy
  | Match {body; cases} ->
      add "match" ;
      add_space () ;
      build_expression body ;
      add_space () ;
      add "with" ;
      add_space () ;
      add_list " " build_case cases
  | Lambda {parameters; body} ->
      add "fun" ;
      add_space () ;
      List.iter (fun p -> build_parameter p ; add_space ()) parameters ;
      add "->" ;
      add_space () ;
      build_expression body
  | Application {body; argument} ->
      build_expression body ; add_space () ; build_expression argument

and build_binary_operator op =
  match op with
  | BPipe ->
      add "|>"
  | BOr ->
      add "||"
  | BAnd ->
      add "&&"
  | BEqual ->
      add "="
  | BNotEqual ->
      add "<>"
  | BGreaterThan ->
      add ">"
  | BGreaterOrEqual ->
      add ">="
  | BLessThan ->
      add "<"
  | BLessOrEqual ->
      add "<="
  | BConcatenate ->
      print_endline
        "\027[31mERROR: The transpiler does not yet support the concatenation \
         operator.\027[0m" ;
      add "(* TODO: Waiting on type checker. *)"
  | BAdd ->
      add "+."
  | BSubstract ->
      add "-."
  | BMultiply ->
      add "*."
  | BDivide ->
      add "/."
  | BModulo ->
      add "mod"
  | BExponentiate ->
      add "**"

and build_unary_operator op =
  match op with UPlus -> add "+." | UMinus -> add "-." | UNot -> add "not"

and build_binding binding =
  add "let" ;
  add_space () ;
  add @@ encode_lid binding.id.value ;
  add_space () ;
  add "=" ;
  add_space () ;
  build_expression binding.body ;
  add_space () ;
  add "in" ;
  add_space ()

and build_case case =
  add "|" ;
  add_space () ;
  build_pattern case.pattern ;
  ( match case.guard with
  | Some guard ->
      add_space () ; add "when" ; add_space () ; build_expression guard
  | None ->
      () ) ;
  add_space () ; add "->" ; add_space () ; build_expression case.body

and build_pattern pat =
  match pat with
  | PInt i ->
      float_of_int i |> string_of_float |> add
  | PFloat f ->
      add @@ string_of_float f
  | PBool b ->
      add @@ string_of_bool b
  | PString s ->
      add "\"" ;
      add @@ String.escaped s ;
      add "\""
  | PLid lid ->
      add @@ encode_lid lid.value
  | PTuple pats ->
      scoped (fun () -> add_list "," build_pattern pats)
  | PList pats ->
      add "[" ;
      add_list ";" build_pattern pats ;
      add "]"
  | PListSpread pats ->
      add_list "::" build_pattern pats
  | PConstructor {id; pattern} -> (
      add @@ encode_uid id.value ;
      match pattern with Some p -> add_space () ; build_pattern p | None -> () )
  | POr {l; r} ->
      build_pattern l ; add_space () ; add "|" ; add_space () ; build_pattern r

and build_variant variant =
  add "|" ;
  add_space () ;
  add @@ encode_uid variant.id.value ;
  match variant.typing with
  | Some t ->
      add_space () ; add "of" ; add_space () ; build_typing t
  | None ->
      ()
