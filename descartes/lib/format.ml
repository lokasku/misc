let reset = "\027[0m"

let hex_to_rgb hex =
  let hex =
    if String.get hex 0 = '#' then String.sub hex 1 (String.length hex - 1)
    else hex
  in
  let r = int_of_string ("0x" ^ String.sub hex 0 2) in
  let g = int_of_string ("0x" ^ String.sub hex 2 2) in
  let b = int_of_string ("0x" ^ String.sub hex 4 2) in
  (r, g, b)

let rgb_to_ansi (r, g, b) = Printf.sprintf "\027[38;2;%d;%d;%dm" r g b

let colorize hex text =
  let rgb = hex_to_rgb hex in
  let ansi_code = rgb_to_ansi rgb in
  ansi_code ^ text ^ reset

let bold text = "\027[1m" ^ text ^ reset

let italic text = "\027[3m" ^ text ^ reset

let underline text = "\027[4m" ^ text ^ reset

let red text = "\027[31m" ^ text ^ reset

let green text = "\027[32m" ^ text ^ reset

let yellow text = "\027[33m" ^ text ^ reset

let blue text = "\027[34m" ^ text ^ reset

let magenta text = "\027[35m" ^ text ^ reset

let cyan text = "\027[36m" ^ text ^ reset

let bright_black text = "\027[90m" ^ text ^ reset

let black text = "\027[30m" ^ text ^ reset

let white text = "\027[37m" ^ text ^ reset

(* let random_color () =
   let r = Random.int 256 in
   let g = Random.int 256 in
   let b = Random.int 256 in
   rgb_to_ansi (r, g, b) *)
