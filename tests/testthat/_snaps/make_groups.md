# grouped variants are consistent across R sessions

    Code
      analysis(rs$splits[[1]])
    Output
      # A tibble: 87 x 14
         name        height  mass hair_~1 skin_~2 eye_c~3 birth~4 sex   gender homew~5
         <chr>        <int> <dbl> <chr>   <chr>   <chr>     <dbl> <chr> <chr>  <chr>  
       1 Luke Skywa~    172    77 blond   fair    blue       19   male  mascu~ Tatooi~
       2 C-3PO          167    75 <NA>    gold    yellow    112   none  mascu~ Tatooi~
       3 Darth Vader    202   136 none    white   yellow     41.9 male  mascu~ Tatooi~
       4 Leia Organa    150    49 brown   light   brown      19   fema~ femin~ Aldera~
       5 Leia Organa    150    49 brown   light   brown      19   fema~ femin~ Aldera~
       6 Leia Organa    150    49 brown   light   brown      19   fema~ femin~ Aldera~
       7 Beru White~    165    75 brown   light   blue       47   fema~ femin~ Tatooi~
       8 Biggs Dark~    183    84 black   light   brown      24   male  mascu~ Tatooi~
       9 Biggs Dark~    183    84 black   light   brown      24   male  mascu~ Tatooi~
      10 Biggs Dark~    183    84 black   light   brown      24   male  mascu~ Tatooi~
      # ... with 77 more rows, 4 more variables: species <chr>, films <list>,
      #   vehicles <list>, starships <list>, and abbreviated variable names
      #   1: hair_color, 2: skin_color, 3: eye_color, 4: birth_year, 5: homeworld

---

    Code
      analysis(rs$splits[[1]])
    Output
      # A tibble: 86 x 14
         name        height  mass hair_~1 skin_~2 eye_c~3 birth~4 sex   gender homew~5
         <chr>        <int> <dbl> <chr>   <chr>   <chr>     <dbl> <chr> <chr>  <chr>  
       1 Luke Skywa~    172    77 blond   fair    blue       19   male  mascu~ Tatooi~
       2 C-3PO          167    75 <NA>    gold    yellow    112   none  mascu~ Tatooi~
       3 R2-D2           96    32 <NA>    white,~ red        33   none  mascu~ Naboo  
       4 Darth Vader    202   136 none    white   yellow     41.9 male  mascu~ Tatooi~
       5 Leia Organa    150    49 brown   light   brown      19   fema~ femin~ Aldera~
       6 Owen Lars      178   120 brown,~ light   blue       52   male  mascu~ Tatooi~
       7 Beru White~    165    75 brown   light   blue       47   fema~ femin~ Tatooi~
       8 R5-D4           97    32 <NA>    white,~ red        NA   none  mascu~ Tatooi~
       9 Biggs Dark~    183    84 black   light   brown      24   male  mascu~ Tatooi~
      10 Obi-Wan Ke~    182    77 auburn~ fair    blue-g~    57   male  mascu~ Stewjon
      # ... with 76 more rows, 4 more variables: species <chr>, films <list>,
      #   vehicles <list>, starships <list>, and abbreviated variable names
      #   1: hair_color, 2: skin_color, 3: eye_color, 4: birth_year, 5: homeworld

---

    Code
      analysis(rs$splits[[1]])
    Output
      # A tibble: 65 x 14
         name        height  mass hair_~1 skin_~2 eye_c~3 birth~4 sex   gender homew~5
         <chr>        <int> <dbl> <chr>   <chr>   <chr>     <dbl> <chr> <chr>  <chr>  
       1 C-3PO          167    75 <NA>    gold    yellow    112   none  mascu~ Tatooi~
       2 R2-D2           96    32 <NA>    white,~ red        33   none  mascu~ Naboo  
       3 Leia Organa    150    49 brown   light   brown      19   fema~ femin~ Aldera~
       4 Owen Lars      178   120 brown,~ light   blue       52   male  mascu~ Tatooi~
       5 Beru White~    165    75 brown   light   blue       47   fema~ femin~ Tatooi~
       6 R5-D4           97    32 <NA>    white,~ red        NA   none  mascu~ Tatooi~
       7 Biggs Dark~    183    84 black   light   brown      24   male  mascu~ Tatooi~
       8 Obi-Wan Ke~    182    77 auburn~ fair    blue-g~    57   male  mascu~ Stewjon
       9 Anakin Sky~    188    84 blond   fair    blue       41.9 male  mascu~ Tatooi~
      10 Greedo         173    74 <NA>    green   black      44   male  mascu~ Rodia  
      # ... with 55 more rows, 4 more variables: species <chr>, films <list>,
      #   vehicles <list>, starships <list>, and abbreviated variable names
      #   1: hair_color, 2: skin_color, 3: eye_color, 4: birth_year, 5: homeworld

---

    Code
      analysis(rs$splits[[1]])
    Output
      # A tibble: 65 x 14
         name        height  mass hair_~1 skin_~2 eye_c~3 birth~4 sex   gender homew~5
         <chr>        <int> <dbl> <chr>   <chr>   <chr>     <dbl> <chr> <chr>  <chr>  
       1 C-3PO          167    75 <NA>    gold    yellow    112   none  mascu~ Tatooi~
       2 Darth Vader    202   136 none    white   yellow     41.9 male  mascu~ Tatooi~
       3 Leia Organa    150    49 brown   light   brown      19   fema~ femin~ Aldera~
       4 Owen Lars      178   120 brown,~ light   blue       52   male  mascu~ Tatooi~
       5 Beru White~    165    75 brown   light   blue       47   fema~ femin~ Tatooi~
       6 R5-D4           97    32 <NA>    white,~ red        NA   none  mascu~ Tatooi~
       7 Biggs Dark~    183    84 black   light   brown      24   male  mascu~ Tatooi~
       8 Obi-Wan Ke~    182    77 auburn~ fair    blue-g~    57   male  mascu~ Stewjon
       9 Wilhuff Ta~    180    NA auburn~ fair    blue       64   male  mascu~ Eriadu 
      10 Chewbacca      228   112 brown   unknown blue      200   male  mascu~ Kashyy~
      # ... with 55 more rows, 4 more variables: species <chr>, films <list>,
      #   vehicles <list>, starships <list>, and abbreviated variable names
      #   1: hair_color, 2: skin_color, 3: eye_color, 4: birth_year, 5: homeworld

