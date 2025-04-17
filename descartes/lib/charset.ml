open Format

type charset =
  { hbar: string
  ; vbar: string
  ; blangle: string
  ; tlangle: string
  ; uarrow: string
  ; darrow: string }
[@@deriving show]

let ascii =
  { hbar= black "-"
  ; vbar= black "|"
  ; blangle= black "+"
  ; tlangle= black "+"
  ; uarrow= black "^"
  ; darrow= black "v" }

let unicode =
  { hbar= black "─"
  ; vbar= black "│"
  ; blangle= black "╰"
  ; tlangle= black "╭"
  ; uarrow= "▲"
  ; darrow= "▼" }
