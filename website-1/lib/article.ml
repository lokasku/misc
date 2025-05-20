type tag = string

type article = {
  title : string;
  date : string;
  edited : string option;
  content : Dream_html.node;
  tags : tag array;
}

let common_tags =
  [ (* name_tag, (background, border, text-color)*)
    (* ("nix", ("bg-cyan-100/50", "border-sky-200/50", "text-sky-500"));
    ("math", ("bg-violet-200/40", "border-violet-300/25", "text-violet-500")); *) ]