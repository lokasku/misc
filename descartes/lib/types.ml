type severity = Error | Warning | Info [@@deriving show]

let string_of_severity = function Error -> "E" | Warning -> "W" | Info -> "I"

let color_of_severity = function
  | Error ->
      Format.red
  | Warning ->
      Format.yellow
  | Info ->
      Format.blue

type range = {start: int; fin: int} [@@deriving show]

let range start fin : range = {start; fin}

type loc = {start_line: int; end_line: int; span: range}

type source = {name: string; contents: string} [@@deriving show]
