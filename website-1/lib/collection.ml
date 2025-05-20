open Article

let all_article =
  [
    Presentation.article;
    Efficient_memory_approach_for_conways_game_of_life.article;
  ]

let filter_articles_by_tags tag_list =
  let filter_article article =
    List.exists
      (fun tag -> List.mem tag (Array.to_list article.tags))
      (Array.to_list tag_list)
  in
  List.filter filter_article all_article