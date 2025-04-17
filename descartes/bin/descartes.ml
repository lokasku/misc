open Descartes.Types
open Descartes.Label
open Descartes.Report
open Descartes.Write
open Descartes.Charset

let report =
  create_report Error
    {name= "foo.ml"; contents= "let x = 1\nlet y = 2\nlet z = 3"}
  |> with_code 0205
  |> with_msg "This is an error"
  |> with_info "This is an information"
  |> with_hint "Some useful hint"
  |> add_label
       ( create_label (range 8 9)
       |> Descartes.Label.with_msg "This is a label"
       |> with_color "green" |> with_priority 1 )
  |> add_label
       ( create_label (range 16 24)
       |> Descartes.Label.with_msg "This is another label" )

let () = header report ; body report unicode
