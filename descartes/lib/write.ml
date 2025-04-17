open Printf
open Types
open Format
open Charset
open Label

let header (report : Report.t) =
  let color = color_of_severity report.severity in
  let severity = string_of_severity report.severity in
  let code = match report.code with Some code -> code | None -> -1 in
  let msg = match report.msg with Some msg -> msg | None -> "" in
  eprintf "%s" (color severity) ;
  if code <> -1 then eprintf "%s" (color @@ string_of_int code) ;
  if msg <> "" then (
    eprintf "%s" (bright_black ": ") ;
    eprintf "%s\n" msg )

let check_range range string_len =
  if range.start < 0 || range.fin < 0 || range.fin > string_len then
    raise (Invalid_argument "Invalid range")

let locate rng lines =
  let rec find_line_index char_index lines current_line =
    match lines with
    | [] ->
        (current_line, char_index)
    | line :: rest ->
        let line_length = String.length line + 1 in
        if char_index < line_length then (current_line, char_index)
        else find_line_index (char_index - line_length) rest (current_line + 1)
  in
  let start_line, start_index = find_line_index rng.start lines 0 in
  let end_line, end_index = find_line_index rng.fin lines 0 in
  let span = range start_index end_index in
  {start_line; end_line; span}

let body (report : Report.t) (charset : charset) =
  let lines = String.split_on_char '\n' report.source.contents in
  let contents_len = String.length report.source.contents in
  (* Add locations *)
  List.iter
    (fun label ->
      check_range label.range contents_len ;
      label.loc <- locate label.range lines )
    report.labels ;
  (* Labels sorted by range.start *)
  let sorted_labels =
    List.sort (fun a b -> compare a.range.start b.range.start) report.labels
  in
  (* Size of the largest line number to be displayed *)
  let last_number_len =
    let last_loc =
      let max_label_range =
        List.fold_left
          (fun rng label ->
            if label.range.fin > rng.fin then label.range else rng )
          (range 0 0) report.labels
      in
      check_range max_label_range (String.length report.source.contents) ;
      locate max_label_range lines
    in
    String.length (string_of_int (last_loc.end_line + 1))
  in
  (* Number of multiline label *)
  let _multiline_number =
    List.fold_left
      (fun acc label ->
        if label.loc.start_line <> label.loc.end_line then acc + 1 else acc )
      0 report.labels
  in
  (* File informations to be displayed *)
  let file_info =
    String.make (last_number_len + 2) ' '
    ^ charset.tlangle ^ charset.hbar ^ " " ^ report.source.name
  in
  eprintf "%s\n" file_info ;
  let _prefix n =
    " "
    ^ String.make (last_number_len - (String.length @@ string_of_int n)) ' '
    ^ string_of_int (n + 1)
    ^ " " ^ charset.vbar
  in
  let insert_by_priority label labels =
    let rec loop left right =
      match right with
      | [] ->
          List.rev_append left [label]
      | label' :: rest ->
          if label.display_info.priority > label'.display_info.priority then
            List.rev_append left (label :: label' :: rest)
          else loop (label' :: left) rest
    in
    loop [] labels
  in
  let loop i multiline labels =
    let grouped, rest =
      let rec loop grouped = function
        | [] ->
            (grouped, [])
        | label :: rest ->
            if label.loc.start_line = label.loc.end_line then
              loop (insert_by_priority label grouped) rest
            else (List.rev_append grouped [label], rest)
      in
      loop [] labels
    in
    0
  in
  0
