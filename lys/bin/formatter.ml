(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

let limit = 80

let indent = "  "

module IntSet = Set.Make (Int)

module Node = struct
  let unicode_width str =
    String.split_on_char '\n' str
    |> List.hd
    |> Uuseg_string.fold_utf_8 `Grapheme_cluster (fun n _ -> n + 1) 0

  type t =
    | Nodes of t list
    | Group of int * t list
    | Fill of t list
    | Text of string * int
    | SpaceOrLine
    | Line
    | HardLine
    | EmptyLine
    | Indent of t list
    | IndentNext of t list
    | Empty

  let text str = Text (str, unicode_width str)

  let rec width wrapped = function
    | Nodes nodes
    | Group (_, nodes)
    | Fill nodes
    | Indent nodes
    | IndentNext nodes ->
        List.fold_left (fun acc node -> acc + width wrapped node) 0 nodes
    | Text (_, width) ->
        width
    | HardLine | EmptyLine ->
        limit
    | SpaceOrLine ->
        1
    | Line | Empty ->
        0
end

module Wrap = struct
  type t = Detect | Enable | Disable | Force
end

module Generator = struct
  open Node

  type t =
    { buffer: Buffer.t
    ; mutable depth: int
    ; mutable size: int
    ; mutable wrapped: IntSet.t
    ; mutable pending_indents: int }

  let create =
    { buffer= Buffer.create 1024
    ; depth= 0
    ; size= 0
    ; wrapped= IntSet.empty
    ; pending_indents= 0 }

  let text t str width =
    t.size <- t.size + width ;
    Buffer.add_string t.buffer str

  let single_space t = text t " " 1

  let new_line t =
    t.size <- String.length indent * t.depth ;
    Buffer.add_char t.buffer '\n' ;
    if t.pending_indents > 0 then (
      t.size <- t.size + 2 ;
      t.depth <- t.depth + 1 ;
      t.pending_indents <- t.pending_indents - 1 ) ;
    for _ = 1 to t.depth do
      Buffer.add_string t.buffer indent
    done

  let rec node_gen t wrap = function
    | Nodes nodes ->
        List.iter (fun node -> node_gen t wrap node) nodes
    | Group (id, nodes) ->
        let width =
          List.fold_left (fun acc node -> acc + width t.wrapped node) 0 nodes
        in
        let wrap =
          match wrap with
          | Wrap.Disable ->
              Wrap.Disable
          | w when w = Wrap.Force || t.size + width > limit ->
              t.wrapped <- IntSet.add id t.wrapped ;
              Wrap.Enable
          | Wrap.Detect ->
              Wrap.Detect
          | Wrap.Enable ->
              Wrap.Detect
          | Wrap.Force ->
              Wrap.Detect
        in
        List.iter (fun node -> node_gen t wrap node) nodes
    | Fill nodes ->
        let wrap = ref wrap in
        let rec aux = function
          | [] ->
              ()
          | hd :: tl ->
              if hd = SpaceOrLine then
                let width =
                  try List.hd tl |> width t.wrapped with Failure _ -> 0
                in
                if t.size + width > limit then (
                  if !wrap = Wrap.Detect then wrap := Wrap.Enable ;
                  new_line t )
                else single_space t
              else node_gen t !wrap hd ;
              aux tl
        in
        aux nodes
    | Text (str, width) ->
        text t str width
    | Line when wrap = Wrap.Enable ->
        new_line t
    | HardLine ->
        new_line t
    | EmptyLine ->
        Buffer.add_char t.buffer '\n' ;
        new_line t
    | SpaceOrLine when wrap = Wrap.Enable ->
        new_line t
    | SpaceOrLine ->
        single_space t
    | Indent nodes when wrap = Wrap.Enable ->
        t.size <- t.size + String.length indent ;
        t.depth <- t.depth + 1 ;
        Buffer.add_string t.buffer indent ;
        List.iter (fun node -> node_gen t wrap node) nodes ;
        t.depth <- t.depth - 1
    | Indent nodes ->
        List.iter (fun node -> node_gen t wrap node) nodes
    | IndentNext nodes when wrap = Wrap.Enable ->
        t.pending_indents <- t.pending_indents + 1 ;
        let before = t.pending_indents in
        List.iter (fun node -> node_gen t wrap node) nodes ;
        if t.pending_indents = before then
          t.pending_indents <- t.pending_indents - 1
        else t.depth <- t.depth - 1
    | IndentNext nodes ->
        List.iter (fun node -> node_gen t wrap node) nodes
    | Line | Empty ->
        ()

  let generate t node =
    node_gen t Wrap.Detect node ;
    Buffer.contents t.buffer
end

module Builder = struct
  open Lys.Ast
  open Node

  type t = {mutable id: int}

  let create = {id= 0}

  let new_id (t : t) =
    t.id <- t.id + 1 ;
    t.id

  let spaced nodes = if nodes = [] then [] else SpaceOrLine :: nodes

  let separated_nodes sep nodes =
    List.mapi
      (fun i node ->
        if i = List.length nodes - 1 then node else Node.Nodes [node; sep] )
      nodes

  let delimited_nodes t left right nodes =
    Group (new_id t, [left; Line; Indent nodes; Line; right])

  let escape str = String.split_on_char '"' str |> String.concat "\\\""

  let quoted_string t str =
    Group (new_id t, [text "\""; text (escape str); text "\""])

  let boolean_string b = if b then text "True" else text "False"

  let float_to_string f =
    let str = string_of_float f in
    if String.ends_with ~suffix:"." str then str ^ "0" else str

  let rec build_program t program =
    Nodes
      ( separated_nodes EmptyLine
          (List.map (fun d -> build_declaration t d) program)
      @ [HardLine] )

  and build_declaration t decl =
    match decl with
    | Comment str ->
        Group
          ( new_id t
          , [ text "`"
            ; (if String.contains str '\n' then HardLine else SpaceOrLine)
            ; Indent [text (String.trim str)]
            ; (if String.contains str '\n' then HardLine else SpaceOrLine)
            ; text "`" ] )
    | ValueBinding binding ->
        Nodes [text "def"; SpaceOrLine; build_binding t binding]
    | TypeDefinition {id; body} ->
        Nodes
          [ text "def"
          ; SpaceOrLine
          ; text id.value
          ; SpaceOrLine
          ; text ":="
          ; Group (new_id t, [SpaceOrLine; Indent [build_typing t body]]) ]
    | FunctionDefinition {id; parameters; signature; body} ->
        Nodes
          [ text "def"
          ; SpaceOrLine
          ; text id.value
          ; Nodes
              (spaced
                 (separated_nodes SpaceOrLine
                    (List.map (fun a -> build_parameter t a) parameters) ) )
          ; build_signature t signature
          ; SpaceOrLine
          ; text "="
          ; Group (new_id t, [SpaceOrLine; Indent [build_expression t body]]) ]
    | AdtDefinition {id; polymorphics; variants} ->
        Nodes
          [ text "def"
          ; SpaceOrLine
          ; text id.value
          ; SpaceOrLine
          ; text ":="
          ; Nodes
              (spaced
                 (separated_nodes SpaceOrLine
                    (List.map (fun poly -> text poly.value) polymorphics) ) )
          ; SpaceOrLine
          ; text "{"
          ; Group
              ( new_id t
              , spaced
                  (List.map
                     (fun v -> Nodes [Indent [build_variant t v]; HardLine])
                     variants ) )
          ; text "}" ]

  and build_binding t binding =
    Fill
      [ text binding.id.value
      ; build_signature t binding.signature
      ; SpaceOrLine
      ; text "="
      ; Group (new_id t, [SpaceOrLine; Indent [build_expression t binding.body]])
      ]

  and build_signature t signature =
    match signature with
    | None ->
        Empty
    | Some typing ->
        Nodes [SpaceOrLine; text ":"; SpaceOrLine; build_typing t typing]

  and build_parameter t param =
    match param with
    | ALid lid ->
        text lid.value
    | ATuple params -> (
      match params with
      | [param] ->
          Nodes [text "("; build_parameter t param; text ",)"]
      | _ ->
          delimited_nodes t (text "(") (text ")")
            (separated_nodes
               (Nodes [text ","; SpaceOrLine])
               (List.map (fun p -> build_parameter t p) params) ) )

  and build_typing t typing =
    match typing with
    | TInt ->
        text "Int"
    | TFloat ->
        text "Float"
    | TString ->
        text "String"
    | TBool ->
        text "Bool"
    | TUnit ->
        text "Unit"
    | TList typing ->
        Nodes [text "["; build_typing t typing; text "]"]
    | TTuple typings -> (
      match typings with
      | [typing] ->
          Nodes [text "("; build_typing t typing; text ",)"]
      | _ ->
          delimited_nodes t (text "(") (text ")")
            (separated_nodes
               (Nodes [text ","; SpaceOrLine])
               (List.map (fun ty -> build_typing t ty) typings) ) )
    | TFunction {l; r} ->
        Nodes
          [ build_typing t l
          ; SpaceOrLine
          ; text "->"
          ; SpaceOrLine
          ; build_typing t r ]
    | TPolymorphic lid ->
        text lid.value
    | TConstructor {id; typing} ->
        Nodes
          [ text id.value
          ; ( match typing with
            | Some ty ->
                Nodes [SpaceOrLine; build_typing t ty]
            | None ->
                Empty ) ]

  and build_expression t expr =
    match expr with
    | Int n ->
        text (string_of_int n)
    | Float f ->
        text (float_to_string f)
    | Bool b ->
        boolean_string b
    | String str ->
        quoted_string t str
    | Unit ->
        text "()"
    | Uid uid ->
        text uid.value
    | Lid lid ->
        text lid.value
    | Tuple exprs -> (
      match exprs with
      | [expr] ->
          Nodes [text "("; build_expression t expr; text ",)"]
      | _ ->
          delimited_nodes t (text "(") (text ")")
            (separated_nodes
               (Nodes [text ","; SpaceOrLine])
               (List.map (fun e -> build_expression t e) exprs) ) )
    | List exprs ->
        delimited_nodes t (text "[") (text "]")
          (separated_nodes
             (Nodes [text ","; SpaceOrLine])
             (List.map (fun e -> build_expression t e) exprs) )
    | BinaryOperation {l; operator; r} ->
        Fill
          [ build_expression t l
          ; SpaceOrLine
          ; build_binary_operator operator
          ; SpaceOrLine
          ; build_expression t r ]
    | UnaryOperation {operator; body} ->
        Fill [build_unary_operator operator; build_expression t body]
    | Let {bindings; body} ->
        Nodes
          [ text "let"
          ; Group
              ( new_id t
              , [ IndentNext
                    (spaced
                       (separated_nodes
                          (Nodes [text ";"; SpaceOrLine])
                          (List.map (fun b -> build_binding t b) bindings) ) )
                ] )
          ; SpaceOrLine
          ; text "in"
          ; Group (new_id t, [SpaceOrLine; Indent [build_expression t body]]) ]
    | If {predicate; truthy; falsy} ->
        Group
          ( new_id t
          , [ text "if"
            ; Group
                (new_id t, [SpaceOrLine; Indent [build_expression t predicate]])
            ; SpaceOrLine
            ; text "then"
            ; Group (new_id t, [SpaceOrLine; Indent [build_expression t truthy]])
            ; SpaceOrLine
            ; text "else"
            ; Group (new_id t, [SpaceOrLine; Indent [build_expression t falsy]])
            ] )
    | Match {body; cases} ->
        Nodes
          [ Fill
              [ text "match"
              ; SpaceOrLine
              ; build_expression t body
              ; SpaceOrLine
              ; text "{" ]
          ; Group
              ( new_id t
              , spaced
                  (List.map
                     (fun c -> Nodes [Indent [build_case t c]; SpaceOrLine])
                     cases ) )
          ; text "}" ]
    | Lambda {parameters; body} ->
        Nodes
          [ text "\\"
          ; Nodes
              (separated_nodes SpaceOrLine
                 (List.map (fun p -> build_parameter t p) parameters) )
          ; SpaceOrLine
          ; text "->"
          ; Group (new_id t, [SpaceOrLine; Indent [build_expression t body]]) ]
    | Application {body; argument} ->
        Fill [build_expression t body; SpaceOrLine; build_expression t argument]
    | Expression {body; signature} ->
        Nodes
          [ text "("
          ; build_expression t body
          ; build_signature t signature
          ; text ")" ]

  and build_binary_operator = function
    | BPipe ->
        text "|>"
    | BOr ->
        text "||"
    | BAnd ->
        text "&&"
    | BEqual ->
        text "=="
    | BNotEqual ->
        text "!="
    | BGreaterThan ->
        text ">"
    | BGreaterOrEqual ->
        text ">="
    | BLessThan ->
        text "<"
    | BLessOrEqual ->
        text "<="
    | BConcatenate ->
        text "++"
    | BAdd ->
        text "+"
    | BSubstract ->
        text "-"
    | BMultiply ->
        text "*"
    | BDivide ->
        text "/"
    | BModulo ->
        text "%"
    | BExponentiate ->
        text "**"

  and build_unary_operator = function
    | UPlus ->
        text "+"
    | UMinus ->
        text "-"
    | UNot ->
        text "!"

  and build_case t case =
    let build_guard guard =
      match guard with
      | Some guard_expr ->
          Fill
            [SpaceOrLine; text "if"; SpaceOrLine; build_expression t guard_expr]
      | None ->
          Empty
    in
    Fill
      [ build_pattern t case.pattern
      ; build_guard case.guard
      ; SpaceOrLine
      ; text "->"
      ; Group (new_id t, [SpaceOrLine; Indent [build_expression t case.body]])
      ; text ";" ]

  and build_pattern t pat =
    match pat with
    | PInt n ->
        text (string_of_int n)
    | PFloat f ->
        text (float_to_string f)
    | PString str ->
        quoted_string t str
    | PBool b ->
        boolean_string b
    | PLid lid ->
        text lid.value
    | PTuple patterns -> (
      match patterns with
      | [pattern] ->
          Nodes [text "("; build_pattern t pattern; text ",)"]
      | _ ->
          delimited_nodes t (text "(") (text ")")
            (separated_nodes
               (Nodes [text ","; SpaceOrLine])
               (List.map (fun p -> build_pattern t p) patterns) ) )
    | PList patterns ->
        delimited_nodes t (text "[") (text "]")
          (separated_nodes
             (Nodes [text ","; SpaceOrLine])
             (List.map (fun p -> build_pattern t p) patterns) )
    | PListSpread patterns ->
        let regular_patterns = List.rev (List.tl (List.rev patterns)) in
        let spread_pattern = List.hd (List.rev patterns) in
        delimited_nodes t (text "[") (text "]")
          ( separated_nodes
              (Nodes [text ","; SpaceOrLine])
              (List.map (fun p -> build_pattern t p) regular_patterns)
          @ [Nodes [SpaceOrLine; text "..."; build_pattern t spread_pattern]] )
    | PConstructor {id; pattern} ->
        Fill
          [ text id.value
          ; ( match pattern with
            | Some p ->
                Fill [SpaceOrLine; build_pattern t p]
            | None ->
                Empty ) ]
    | POr {l; r} ->
        Fill [build_pattern t l; text ";"; SpaceOrLine; build_pattern t r]

  and build_variant t variant =
    Group
      ( new_id t
      , [ text variant.id.value
        ; ( match variant.typing with
          | Some ty ->
              Nodes [SpaceOrLine; text "as"; SpaceOrLine; build_typing t ty]
          | None ->
              Empty )
        ; text ";" ] )
end

let format program =
  let build = Builder.create in
  let node = Builder.build_program build program in
  let gen = Generator.create in
  Generator.generate gen node
