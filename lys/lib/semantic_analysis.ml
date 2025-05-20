(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

open Ast

type arity = int [@@deriving show {with_path= false}]

type info = span * arity

type name = string [@@deriving show {with_path= false}]

type map = (name, info) Hashtbl.t

type scope = Scope of map * scope | Root

type error =
  | AlreadyDefined of {prev: span; new': name; newest_span: span}
  | Undefined of {name: name; span: span}
  | ReservedName of {name: name; span: span}
  | NotACallee of {name: name; span: span}
  | ArityMismatch of {name: name; expected: arity; span: span}
[@@deriving show {with_path= false}]

type warning = Unused of {name: name; span: span}
[@@deriving show {with_path= false}]

type analysis_output =
  { mutable errors: error list
  ; mutable warnings: (name, warning) Hashtbl.t
  ; mutable types: map
  ; mutable variants: map }

let output =
  { errors= []
  ; warnings= Hashtbl.create 100
  ; types= Hashtbl.create 100
  ; variants= Hashtbl.create 500 }

let builtins = ["Int"; "Float"; "String"; "Bool"; "Unit"]

let stdlib = Hashtbl.create 20

let _ =
  List.iter
    (fun (name, arity) -> Hashtbl.add stdlib name ({start= 0; fin= 0}, arity))
    [ ("sqrt", 1)
    ; ("\u{03C0}", 0)
    ; ("map", 2)
    ; ("filter", 2)
    ; ("foldLeft", 3)
    ; ("foldRight", 3)
    ; ("printString", 1)
    ; ("printNumber", 1) ]

let new_map () = Hashtbl.create 100

let rec find_identifier scope name =
  match scope with
  | Root ->
      Hashtbl.find_opt stdlib name
  | Scope (map, parent) -> (
    match Hashtbl.find_opt map name with
    | Some info ->
        Some info
    | None ->
        find_identifier parent name )

let add_identifier scope name info =
  match scope with
  | Scope (map, _) ->
      Hashtbl.replace map name info
  | Root ->
      assert false

let rec analyze_parameter scope param =
  match param with
  | ALid lid ->
      if not (String.starts_with lid.value ~prefix:"_") then (
        add_identifier scope lid.value (lid.loc, 0) ;
        Hashtbl.add output.warnings lid.value
          (Unused {name= lid.value; span= lid.loc}) )
  | ATuple lst ->
      List.iter (fun param -> analyze_parameter scope param) lst

let rec analyze_pattern scope pattern =
  match pattern with
  | PInt _ | PFloat _ | PBool _ | PString _ ->
      ()
  | PLid lid ->
      if not (String.starts_with lid.value ~prefix:"_") then (
        add_identifier scope lid.value (lid.loc, 0) ;
        Hashtbl.add output.warnings lid.value
          (Unused {name= lid.value; span= lid.loc}) )
  | PConstructor {id; pattern} -> (
      ( match Hashtbl.find_opt output.variants id.value with
      | Some _ ->
          ()
      | None ->
          output.errors <-
            Undefined {name= id.value; span= id.loc} :: output.errors ) ;
      match pattern with Some p -> analyze_pattern scope p | None -> () )
  | PTuple lst | PList lst | PListSpread lst ->
      List.iter (fun pattern -> analyze_pattern scope pattern) lst
  | POr {l; r} ->
      analyze_pattern scope l ;
      analyze_pattern scope r ;
      let rec vars_collected pattern =
        match pattern with
        | PInt _ | PFloat _ | PString _ | PBool _ ->
            []
        | PLid lid ->
            [lid.value]
        | PTuple lst | PList lst | PListSpread lst ->
            List.concat_map vars_collected lst
        | PConstructor {pattern= Some p; _} ->
            vars_collected p
        | PConstructor {pattern= None; _} ->
            []
        | POr {l; r} ->
            vars_collected l @ vars_collected r
      in
      List.iter
        (fun var -> Hashtbl.remove output.warnings var)
        (vars_collected l @ vars_collected r)

let rec analyze_type typing =
  match typing with
  | TList t ->
      analyze_type t
  | TTuple lst ->
      List.iter (fun typing -> analyze_type typing) lst
  | TFunction {l; r} ->
      analyze_type l ; analyze_type r
  | TConstructor {id; typing} -> (
    match Hashtbl.find_opt output.types id.value with
    | Some (_, arity) ->
        let expected_arity = if Option.is_some typing then 1 else 0 in
        if expected_arity <> arity then
          output.errors <-
            ArityMismatch {name= id.value; expected= arity; span= id.loc}
            :: output.errors ;
        Hashtbl.remove output.warnings id.value
    | None ->
        output.errors <-
          Undefined {name= id.value; span= id.loc} :: output.errors )
  | TPolymorphic _ | TInt | TFloat | TString | TBool | TUnit ->
      ()

let rec check_poly_pattern pattern =
  match pattern with
  | PInt _ | PFloat _ | PBool _ | PString _ | PLid _ ->
      None
  | PConstructor {id; pattern= p} -> (
    match Hashtbl.find_opt output.types id.value with
    | Some (_, arity) when arity > 0 ->
        Some (id.value, id.loc)
    | _ ->
        Option.bind p check_poly_pattern )
  | PTuple lst | PList lst | PListSpread lst ->
      List.find_map check_poly_pattern lst
  | POr {l; r} -> (
    match check_poly_pattern l with
    | Some res ->
        Some res
    | None ->
        check_poly_pattern r )

(* Check if the pattern has a guard to avoid undertermined behavior in the type
   checking *)
let rec analyze_case scope case =
  let scope' = Scope (new_map (), scope) in
  analyze_pattern scope' case.pattern ;
  (* si polymorphique ... *)
  ( match check_poly_pattern case.pattern with
  | Some (name, loc) when Option.is_none case.guard ->
      (* si aucune guard, erreur car ambiguitée au typechecking *)
      output.errors <-
        ArityMismatch {name; expected= 1; span= loc} :: output.errors
  | _ ->
      () ) ;
  Option.iter (analyze_expression scope') case.guard ;
  analyze_expression scope' case.body

and analyze_binding scope ({id; signature; body} : binding) =
  (match signature with Some sign -> analyze_type sign | None -> ()) ;
  if not (String.starts_with id.value ~prefix:"_") then
    Hashtbl.add output.warnings id.value (Unused {name= id.value; span= id.loc}) ;
  analyze_expression scope body

and analyze_expression scope expr =
  match expr with
  | Int _ | Float _ | Bool _ | String _ | Unit ->
      ()
  | Uid uid -> (
    match Hashtbl.find_opt output.variants uid.value with
    | Some _ ->
        Hashtbl.remove output.warnings uid.value
    | None ->
        output.errors <-
          Undefined {name= uid.value; span= uid.loc} :: output.errors )
  | Lid lid -> (
    match find_identifier scope lid.value with
    | Some _ ->
        Hashtbl.remove output.warnings lid.value
    | None ->
        output.errors <-
          Undefined {name= lid.value; span= lid.loc} :: output.errors )
  | Tuple lst | List lst ->
      List.iter (fun expr -> analyze_expression scope expr) lst
  | BinaryOperation {l; r; _} ->
      analyze_expression scope l ; analyze_expression scope r
  | UnaryOperation {body; _} ->
      analyze_expression scope body
  | Application {body; argument} ->
      analyze_expression scope body ;
      analyze_expression scope argument
  | Lambda {parameters; body} ->
      let scope' = Scope (new_map (), scope) in
      List.iter (fun param -> analyze_parameter scope' param) parameters ;
      analyze_expression scope' body
  | Match {body; cases} ->
      analyze_expression scope body ;
      List.iter (fun case -> analyze_case scope case) cases
  | Let {bindings; body} ->
      let scope' = Scope (new_map (), scope) in
      List.iter
        (fun {id; signature= _; body= _} ->
          match find_identifier scope id.value with
          | Some (span, _) ->
              output.errors <-
                AlreadyDefined {prev= span; new'= id.value; newest_span= id.loc}
                :: output.errors
          | None ->
              add_identifier scope id.value (id.loc, 0) )
        bindings ;
      List.iter
        (fun binding ->
          let scope'' = Scope (new_map (), scope') in
          analyze_binding scope'' binding )
        bindings ;
      analyze_expression scope' body
  | If {predicate; truthy; falsy} ->
      analyze_expression scope predicate ;
      analyze_expression scope truthy ;
      analyze_expression scope falsy
  | Expression {body; signature} -> (
      analyze_expression scope body ;
      match signature with Some typ -> analyze_type typ | None -> () )

let rec analyze_variant_type scope typing =
  match typing with
  | TPolymorphic lid -> (
    match find_identifier scope lid.value with
    | Some _ ->
        Hashtbl.remove output.warnings lid.value
    | None ->
        output.errors <-
          Undefined {name= lid.value; span= lid.loc} :: output.errors )
  | TList t ->
      analyze_variant_type scope t
  | TTuple lst ->
      List.iter (fun typing -> analyze_variant_type scope typing) lst
  | TFunction {l; r} ->
      analyze_variant_type scope l ;
      analyze_variant_type scope r
  | TConstructor {id; typing} -> (
    match Hashtbl.find_opt output.types id.value with
    | Some (_, arity) ->
        let expected_arity = if Option.is_some typing then 1 else 0 in
        if expected_arity <> arity then
          output.errors <-
            ArityMismatch {name= id.value; expected= arity; span= id.loc}
            :: output.errors ;
        Hashtbl.remove output.warnings id.value
    | None ->
        output.errors <-
          Undefined {name= id.value; span= id.loc} :: output.errors )
  | TInt | TFloat | TString | TBool | TUnit ->
      ()

and analyze_variant scope (variant : variant) =
  match Hashtbl.find_opt output.variants variant.id.value with
  | Some (variant_span, _) ->
      output.errors <-
        AlreadyDefined
          { prev= variant_span
          ; new'= variant.id.value
          ; newest_span= variant.id.loc }
        :: output.errors
  | None ->
      ( match variant.typing with
      | Some typing ->
          analyze_variant_type scope typing
      | None ->
          () ) ;
      Hashtbl.add output.variants variant.id.value
        (variant.id.loc, if Option.is_some variant.typing then 1 else 0) ;
      Hashtbl.add output.warnings variant.id.value
        (Unused {name= variant.id.value; span= variant.id.loc})

let analyze_declaration scope decl =
  match decl with
  | FunctionDefinition {id; parameters; signature; body} ->
      (match signature with Some sign -> analyze_type sign | None -> ()) ;
      ( match find_identifier scope id.value with
      | Some (span, _) ->
          output.errors <-
            AlreadyDefined {prev= span; new'= id.value; newest_span= id.loc}
            :: output.errors
      | None ->
          if not (String.starts_with id.value ~prefix:"_") then (
            add_identifier scope id.value (id.loc, List.length parameters) ;
            Hashtbl.add output.warnings id.value
              (Unused {name= id.value; span= id.loc}) ) ) ;
      let scope' = Scope (new_map (), scope) in
      List.iter (fun param -> analyze_parameter scope' param) parameters ;
      analyze_expression scope' body
  | ValueBinding binding ->
      let id = binding.id in
      ( match find_identifier scope id.value with
      | Some (span, _) ->
          output.errors <-
            AlreadyDefined {prev= span; new'= id.value; newest_span= id.loc}
            :: output.errors
      | None ->
          if not (String.starts_with id.value ~prefix:"_") then (
            add_identifier scope id.value (id.loc, 0) ;
            Hashtbl.add output.warnings id.value
              (Unused {name= id.value; span= id.loc}) ) ) ;
      (match binding.signature with Some typ -> analyze_type typ | None -> ()) ;
      analyze_expression scope binding.body
  | AdtDefinition {id; polymorphics; variants} ->
      (* Name *)
      ( match Hashtbl.find_opt output.types id.value with
      (* Was not already defined *)
      | Some (span, _) ->
          output.errors <-
            AlreadyDefined {prev= span; new'= id.value; newest_span= id.loc}
            :: output.errors
      | None -> (
        match List.exists (fun builtin -> builtin = id.value) builtins with
        (* Is not a reserved name *)
        | true ->
            output.errors <-
              ReservedName {name= id.value; span= id.loc} :: output.errors
        | false ->
            Hashtbl.add output.types id.value (id.loc, List.length polymorphics)
        ) ) ;
      (* Polymorphic variables *)
      let scope' = Scope (new_map (), Root) in
      List.iter
        (fun poly ->
          match find_identifier scope' poly.value with
          | Some (span, _) ->
              output.errors <-
                AlreadyDefined
                  {prev= span; new'= poly.value; newest_span= poly.loc}
                :: output.errors
          | None ->
              add_identifier scope' poly.value
                (poly.loc, List.length polymorphics) )
        polymorphics ;
      (* Variants *)
      List.iter (fun variant -> analyze_variant scope' variant) variants
  | TypeDefinition {id; body} -> (
      analyze_type body ;
      match Hashtbl.find_opt output.types id.value with
      | Some (span, _) ->
          output.errors <-
            AlreadyDefined {prev= span; new'= id.value; newest_span= id.loc}
            :: output.errors
      | None ->
          Hashtbl.add output.types id.value (id.loc, 0) )
  | Comment _ ->
      ()

let analyze_program (program : Ast.program) =
  let root_scope = Scope (new_map (), Root) in
  List.iter (fun decl -> analyze_declaration root_scope decl) program ;
  output

let debug_output output =
  List.iter
    (fun error -> Printf.printf "\n\027[31m%s\027[0m" (show_error error))
    output.errors ;
  Hashtbl.iter
    (fun _ warning -> Printf.printf "\n\027[33m%s\027[0m" (show_warning warning))
    output.warnings ;
  Printf.printf "\n%!"

let get_errors output = output.errors
