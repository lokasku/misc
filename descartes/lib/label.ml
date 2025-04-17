open Types

type display_info = {msg: string; color: string; priority: int}
[@@deriving show]

type t = {mutable loc: loc; range: range; display_info: display_info}

let create_label rng =
  { loc= {start_line= 0; end_line= 0; span= range 0 0}
  ; range= rng
  ; display_info= {msg= ""; color= ""; priority= 0} }

let with_msg msg label = {label with display_info= {label.display_info with msg}}

let with_color color label =
  {label with display_info= {label.display_info with color}}

let with_priority priority label =
  {label with display_info= {label.display_info with priority}}
