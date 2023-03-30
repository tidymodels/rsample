# grouped variants are consistent across R sessions

    Code
      analysis(rs$splits[[1]])
    Output
      # A tibble: 87 x 14
         name     height  mass hair_color skin_color eye_color birth_year sex   gender
         <chr>     <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
       1 Luke Sk~    172    77 blond      fair       blue            19   male  mascu~
       2 C-3PO       167    75 <NA>       gold       yellow         112   none  mascu~
       3 Darth V~    202   136 none       white      yellow          41.9 male  mascu~
       4 Leia Or~    150    49 brown      light      brown           19   fema~ femin~
       5 Leia Or~    150    49 brown      light      brown           19   fema~ femin~
       6 Leia Or~    150    49 brown      light      brown           19   fema~ femin~
       7 Beru Wh~    165    75 brown      light      blue            47   fema~ femin~
       8 Biggs D~    183    84 black      light      brown           24   male  mascu~
       9 Biggs D~    183    84 black      light      brown           24   male  mascu~
      10 Biggs D~    183    84 black      light      brown           24   male  mascu~
      # i 77 more rows
      # i 5 more variables: homeworld <chr>, species <chr>, films <list>,
      #   vehicles <list>, starships <list>

---

    Code
      analysis(rs$splits[[1]])
    Output
      # A tibble: 86 x 14
         name     height  mass hair_color skin_color eye_color birth_year sex   gender
         <chr>     <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
       1 Luke Sk~    172    77 blond      fair       blue            19   male  mascu~
       2 C-3PO       167    75 <NA>       gold       yellow         112   none  mascu~
       3 R2-D2        96    32 <NA>       white, bl~ red             33   none  mascu~
       4 Darth V~    202   136 none       white      yellow          41.9 male  mascu~
       5 Leia Or~    150    49 brown      light      brown           19   fema~ femin~
       6 Owen La~    178   120 brown, gr~ light      blue            52   male  mascu~
       7 Beru Wh~    165    75 brown      light      blue            47   fema~ femin~
       8 R5-D4        97    32 <NA>       white, red red             NA   none  mascu~
       9 Biggs D~    183    84 black      light      brown           24   male  mascu~
      10 Obi-Wan~    182    77 auburn, w~ fair       blue-gray       57   male  mascu~
      # i 76 more rows
      # i 5 more variables: homeworld <chr>, species <chr>, films <list>,
      #   vehicles <list>, starships <list>

---

    Code
      analysis(rs$splits[[1]])
    Output
      # A tibble: 65 x 14
         name     height  mass hair_color skin_color eye_color birth_year sex   gender
         <chr>     <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
       1 C-3PO       167    75 <NA>       gold       yellow         112   none  mascu~
       2 R2-D2        96    32 <NA>       white, bl~ red             33   none  mascu~
       3 Leia Or~    150    49 brown      light      brown           19   fema~ femin~
       4 Owen La~    178   120 brown, gr~ light      blue            52   male  mascu~
       5 Beru Wh~    165    75 brown      light      blue            47   fema~ femin~
       6 R5-D4        97    32 <NA>       white, red red             NA   none  mascu~
       7 Biggs D~    183    84 black      light      brown           24   male  mascu~
       8 Obi-Wan~    182    77 auburn, w~ fair       blue-gray       57   male  mascu~
       9 Anakin ~    188    84 blond      fair       blue            41.9 male  mascu~
      10 Greedo      173    74 <NA>       green      black           44   male  mascu~
      # i 55 more rows
      # i 5 more variables: homeworld <chr>, species <chr>, films <list>,
      #   vehicles <list>, starships <list>

---

    Code
      analysis(rs$splits[[1]])
    Output
      # A tibble: 65 x 14
         name     height  mass hair_color skin_color eye_color birth_year sex   gender
         <chr>     <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
       1 C-3PO       167    75 <NA>       gold       yellow         112   none  mascu~
       2 Darth V~    202   136 none       white      yellow          41.9 male  mascu~
       3 Leia Or~    150    49 brown      light      brown           19   fema~ femin~
       4 Owen La~    178   120 brown, gr~ light      blue            52   male  mascu~
       5 Beru Wh~    165    75 brown      light      blue            47   fema~ femin~
       6 R5-D4        97    32 <NA>       white, red red             NA   none  mascu~
       7 Biggs D~    183    84 black      light      brown           24   male  mascu~
       8 Obi-Wan~    182    77 auburn, w~ fair       blue-gray       57   male  mascu~
       9 Wilhuff~    180    NA auburn, g~ fair       blue            64   male  mascu~
      10 Chewbac~    228   112 brown      unknown    blue           200   male  mascu~
      # i 55 more rows
      # i 5 more variables: homeworld <chr>, species <chr>, films <list>,
      #   vehicles <list>, starships <list>

