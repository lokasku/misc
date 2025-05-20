open Website.Collection
open Dream_html
open HTML

let () =
  Dream.run (* ~interface:"0.0.0.0" *) ~port:2048
  @@ Dream.logger @@ Dream.memory_sessions
  @@ Dream.router
       [
         Dream.get "/" (fun req ->
             Dream_html.respond
               (Template.layout
                  (div []
                     [
                       Template.presentation;
                       Template.search req;
                       Template.art_list all_article;
                     ])));
         Dream.post "/" (fun req ->
             match%lwt Dream.form req with
             | `Ok [ ("search", search) ] ->
                 let splited_tags = Str.split (Str.regexp " ") search in
                 let filtered_articles =
                   filter_articles_by_tags (Array.of_list splited_tags)
                 in
                 Dream_html.respond
                   (Template.layout
                   @@ div []
                        [
                          Template.back_to_homepage;
                          Template.search req;
                          (if Array.length (Array.of_list filtered_articles) = 0
                           then p [] [ txt "No article found." ]
                           else Template.art_list filtered_articles);
                        ])
             | _ -> Dream.empty `Bad_Request);
         Dream.get "/article/:id" (fun req ->
             try
               let index = int_of_string (Dream.param req "id") in
               let article = List.nth all_article index in
               Dream_html.respond (Template.layout @@ Template.art_view article)
             with Failure _ ->
               Dream_html.respond
                 (p []
                    [
                      txt
                        "Sorry, article %s does not exist in this space-time \
                         continuum."
                      @@ Dream.param req "id";
                    ]));
         Dream.get "/static/**" (Dream.static "static/");
         Dream.get "/assets/**" (Dream.static "lib/articles/assets/");
       ]
