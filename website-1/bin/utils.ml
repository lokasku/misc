open Website.Article

let current_year () =
  let time = Unix.time () in
  let tm = Unix.localtime time in
  tm.Unix.tm_year + 1900

let get_style tag =
  let base = "rounded-lg border text-base text-gray-500 mr-0.5 py-0.5 px-1.5 text-center" in
  match List.find_opt (fun (x, _) -> x = tag) common_tags with
  | Some (_, (background, border, text_color)) ->
      Printf.sprintf "%s %s %s %s" base border background text_color
  | None -> base

let index_of item lst =
  let indexed_lst = List.mapi (fun i x -> (i, x)) lst in
  match List.find_opt (fun (_, x) -> x = item) indexed_lst with
  | Some (i, _) -> Some i
  | None -> None