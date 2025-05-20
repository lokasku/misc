(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

open Lexing
open Printf

let reset = "\027[0m"

let colorize color txt = color ^ txt ^ reset

let underline = colorize "\027[4m"

let black = colorize "\027[30m"

let red = colorize "\027[31m"

let orange = colorize "\027[33m"

let blue = colorize "\027[34m"

let purple = colorize "\027[35m"

let white = colorize "\027[37m"

let bright_black = colorize "\027[90m"

type severity = Info | Warning | Error

let string_of_severity = function Info -> "I" | Warning -> "W" | Error -> "E"

let color_of_severity = function
  | Info ->
      blue
  | Warning ->
      orange
  | Error ->
      red

let rec space n = if n <= 0 then "" else " " ^ space (n - 1)

let column pos = pos.pos_cnum - pos.pos_bol

let create_report ?(json = false) ?(hint = "") ?(note = "") severity code msg
    range =
  let hint = String.trim hint in
  let note = String.trim note in
  let msg = String.trim msg in
  let start, fin = range in
  if json then (
    Yojson.Safe.to_channel stderr
      (`Assoc
         [ ("code", `Int code)
         ; ("msg", `String msg)
         ; ("startLine", `Int (start.pos_lnum - 1))
         ; ("startCol", `Int (start.pos_cnum - start.pos_bol))
         ; ("endLine", `Int (fin.pos_lnum - 1))
         ; ("endCol", `Int (fin.pos_cnum - fin.pos_bol)) ] ) ;
    flush stderr )
  else
    let margin = String.length (string_of_int (fin.pos_lnum + 1)) in
    let file_name = start.pos_fname in
    let up = "\u{2575}" in
    let vertical_right = "\u{251C}" in
    let arc_down_right = "\u{256D}" in
    let horizontal = "\u{2574}" in
    let vertical = "\u{2502}" in
    let print_header () =
      let line_pos =
        if start.pos_lnum = fin.pos_lnum then string_of_int start.pos_lnum
        else string_of_int start.pos_lnum ^ "-" ^ string_of_int fin.pos_lnum
      in
      eprintf "%s%s\n"
        (color_of_severity severity
           (string_of_severity severity ^ sprintf "%04d" code) )
        (white (": " ^ msg)) ;
      eprintf "%s %s%s:%s:%d-%d\n"
        (space (if file_name = "" then 0 else margin))
        ( black
        @@ (if file_name = "" then vertical_right else arc_down_right)
        ^ horizontal )
        (if file_name = "" then "stdin" else Filename.basename file_name)
        line_pos
        (column start + 1)
        (column fin + 1)
    in
    let print_line n line =
      let n_str = string_of_int n in
      let margin_space = space (margin - String.length n_str) in
      let line_prefix =
        sprintf "%s%s %s " margin_space (bright_black n_str) (black vertical)
      in
      if n = start.pos_lnum - 1 then (
        if line <> "" then eprintf "%s %s\n" (space margin) (black vertical) ;
        eprintf "%s%s\n" line_prefix line )
      else if n >= start.pos_lnum && n <= fin.pos_lnum then
        let error_range =
          if n = start.pos_lnum && n = fin.pos_lnum then
            (column start, column fin - column start)
          else if n = start.pos_lnum then
            (column start, String.length line - column start)
          else if n = fin.pos_lnum then (0, column fin)
          else (0, String.length line)
        in
        eprintf "%s%s%s%s\n" line_prefix
          (String.sub line 0 (fst error_range))
          (underline
             (red (String.sub line (fst error_range) (snd error_range))) )
          (String.sub line
             (fst error_range + snd error_range)
             (String.length line - (fst error_range + snd error_range)) )
      else if n = fin.pos_lnum + 1 then
        eprintf "%s%s\n%s %s\n%!" line_prefix line (space margin)
          (black (if hint <> "" || note <> "" then vertical else up))
    in
    let print_footer () =
      if hint <> "" then
        eprintf "%s %s %s: %s\n"
          (space (if file_name = "" then 0 else margin))
          (black vertical_right) (purple "Hint") hint ;
      if note <> "" then
        eprintf "%s %s %s: %s\n"
          (space (if file_name = "" then 0 else margin))
          (black vertical_right) (blue "Note") note ;
      if hint <> "" || note <> "" || file_name = "" then
        eprintf "%s %s\n%!"
          (space (if file_name = "" then 0 else margin))
          (black up)
    in
    print_header () ;
    if file_name <> "" then (
      let file = open_in_bin file_name in
      for n = 1 to fin.pos_lnum + 1 do
        match In_channel.input_line file with
        | None ->
            ()
        | Some line ->
            print_line n line
      done ;
      close_in file ) ;
    print_footer ()
