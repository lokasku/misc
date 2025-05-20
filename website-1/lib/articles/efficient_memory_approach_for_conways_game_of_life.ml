open Dream_html
open HTML

let article : Article.article =
  {
    title = "Efficient Memory-Free Approach for Conway's Game of Life";
    date = "May 3, 2024";
    edited = None;
    tags = [| "conway" |];
    content =
      div []
        [
          p []
            [
              txt
                "This paper introduces a straightforward method for \
                 transitioning between generations in John Horton Conway's \
                 Game of Life by directly manipulating the matrix representing \
                 the cell environment.";
            ];
          p []
            [
              txt
                "This approach, devoid of any additional memory allocation \
                 during grid manipulation, proves advantageous in scenarios \
                 where memory constraints pose limitations.";
            ];
          h2 [] [ txt "Method" ];
          p []
            [
              txt
                "To initiate, consider a matrix filled with values denoting at \
                 least four distinct states, typically represented as 0, 1, 2, \
                 and 3. These values correspond to dead, alive, ";
              i [] [ txt "just born" ];
              txt ", and ";
              i [] [ txt "just died" ];
              txt
                " states, respectively. For each cell within the matrix, the \
                 number of its neighbors is determined, and its value is \
                 updated according to the prescribed rules:";
            ];
          ul []
            [
              li []
                [
                  txt
                    "If the cell is alive and its neighborhood is either less \
                     than 2 or exceeds 3, assign it the value corresponding to \
                     the ";
                  i [] [ txt "just died" ];
                  txt " state.";
                ];
              li []
                [
                  txt
                    "Conversely, if the cell is dead and its neighborhood \
                     count equals 3, assign it the value corresponding to the ";
                  i [] [ txt "just born" ];
                  txt " state.";
                ];
              li []
                [
                  txt
                    "In cases where the cell neither dies nor is born, its \
                     state remains unchanged.";
                ];
            ];
          p []
            [
              txt
                "Furthermore, during neighborhood calculation, both currently \
                 alive cells and those ";
              i [] [ txt "just died" ];
              txt " are considered alive.";
            ];
          p []
            [
              txt
                "Lastly, a concluding pass is requisite, which may be executed \
                 seamlessly as an independent step. This involves \
                 substituting ";
              i [] [ txt "just died" ];
              txt " cells with dead cells and ";
              i [] [ txt "just born" ];
              txt
                " cells with alive cells, as these states merely signify \
                 promises regarding their subsequent status.";
            ];
          h2 [] [ txt "Motivation" ];
          p []
            [
              txt
                "The initiation of my search for such a method stemmed from \
                 the desire to emulate the Game of Life on my Numworks \
                 calculator, integrating user-adjustable cell dimensions. \
                 However, I faced a practical challenge — the calculator's \
                 320x222 screen lacked the memory capacity to store even a \
                 small portion of the desired cell count. Hence, I began \
                 shaping an approach that obviated the need for memory \
                 allocation.";
            ];
          p []
            [
              txt "You can find an implementation of this method ";
              a
                [ href "https://my.numworks.com/python/lokasku/conway" ]
                [ txt "here" ];
              txt ".";
            ];
        ];
  }
