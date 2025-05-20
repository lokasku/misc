open Website.Article
open Website.Collection
open Utils
open Dream_html
open HTML

let search request =
  div
    [ class_ "flex justify-center mb-5 mt-3" ]
    [
      form
        [ class_ "w-full"; method_ `POST; action "/" ]
        [
          csrf_tag request;
          input
            [
              class_
                "w-full border rounded-lg px-2 py-1 focus:outline-none \
                 focus:border-slate-300";
              name "search";
              placeholder "Search for tags";
              type_ "text";
            ];
        ];
    ]

let back_to_homepage =
  p
    [ class_ "mb-5" ]
    [
      a [ class_ "hover:no-underline"; href "/" ] [ txt "<- Back to home page" ];
    ]

let presentation =
  div []
    [
      h1 [ class_ "text-4xl font-bold text-slate-700" ] [ txt "lokasku" ];
      br [];
      p []
        [
          txt
            "Welcome, my name is Lokasku, I'm 17 years old and I'm currently \
             in 12th grade.\n\
            \      I am a French citizen with dual French and Polish \
             nationality. My main interests are computer science,\n\
            \      mathematics and linguistics, but I mainly do programming.";
        ];
      a
        [ href "/article/0" ]
        [
          txt
            "A more suitable presentation of myself and this blog can be found \
             here.";
        ];
      p []
        [
          txt "You can contact me at lukasku at proton dot me and find me on ";
          a [ href "https://github.com/lokasku" ] [ txt "GitHub" ];
          txt " or ";
          a [ href "https://x.com/lokasku" ] [ txt "X" ];
          txt ".";
        ];
    ]

let footer =
  div
    [ class_ "mt-5" ]
    [
      br [];
      hr [];
      p []
        [
          txt "© %s Lokasku" (string_of_int (current_year ()));
          br [];
          txt "Licensed under ";
          a
            [ href "https://creativecommons.org/licenses/by-nc/4.0/deed.en" ]
            [ txt "CC BY-NC 4.0 DEED" ];
          txt ".";
        ];
    ]

let art_list arts =
  div []
    (List.rev
       (List.map
          (fun article ->
            div
              [
                class_
                  "my-5 p-3 rounded-md border border-gray-200 bg-gray-100/25";
              ]
              [
                p
                  [ class_ "inline" ]
                  [
                    a
                      [
                        class_
                          "break-words pr-2 text-xl font-semibold text-gray-600";
                        href "/article/%d"
                        @@ Option.get (Utils.index_of article all_article);
                      ]
                      [ txt "%s" article.title ];
                    span [ class_ "text-gray-400" ] [ txt " — %s" article.date ];
                  ];
                (if Array.length article.tags > 0 then
                   ul
                     [
                       class_
                         "flex gap-1 flex-wrap pt-3 font-mono text-gray-500 ml \
                          list-none p-0";
                     ]
                     (List.map
                        (fun tag ->
                          li [ class_ "%s" (get_style tag) ] [ txt "%s" tag ])
                        (Array.to_list article.tags))
                 else div [] []);
              ])
          arts))

let art_view article =
  div []
    [
      back_to_homepage;
      p
        [ class_ "text-4xl font-bold text-slate-700 mr-2" ]
        [ txt "%s" article.title ];
      p [ class_ "text-gray-500 text-base py-4" ] [ txt "%s" article.date ];
      (match article.edited with
      | Some date ->
          span
            [ class_ "text-gray-500 text-base" ]
            [ i [] [ txt " - Edited on %s" date ] ]
      | None -> span [] []);
      (if Array.length article.tags > 0 then
         ul
           [
             class_
               "flex gap-1 flex-wrap pt-1 font-mono text-gray-500 list-none \
                p-0 py-2";
           ]
           (List.map
              (fun tag -> li [ class_ "%s" (get_style tag) ] [ txt "%s" tag ])
              (Array.to_list article.tags))
       else div [] []);
      div [ class_ "mt-3" ] [ article.content ];
    ]

let layout ctt =
  html
    [ lang "en" ]
    [
      head []
        [
          title [] "Lokasku";
          meta [ charset "utf-8" ];
          meta
            [ name "viewport"; content "width=device-width, maximum-scale=1" ];
          link [ rel "stylesheet"; href "/static/output.css" ];
        ];
      body
        [ class_ "mx-auto min-h-svh max-w-prose py-12 px-4" ]
        [ div [] [ ctt; footer ] ];
    ]
