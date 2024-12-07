# add_header_above accepts p{3cm}

    Code
      kbl(mtcars[1:3, 1:3], "latex") %>% add_header_above(c(" ",
        `This is a very long header that will need to be wrapped` = 3), align = c("l",
        "p{3cm}"))
    Output
      
      \begin{tabular}[t]{l|r|r|r}
      \hline
      \multicolumn{1}{l|}{ } & \multicolumn{3}{p{3cm}}{This is a very long header that will need to be wrapped} \\
      \cline{2-4}
        & mpg & cyl & disp\\
      \hline
      Mazda RX4 & 21.0 & 6 & 160\\
      \hline
      Mazda RX4 Wag & 21.0 & 6 & 160\\
      \hline
      Datsun 710 & 22.8 & 4 & 108\\
      \hline
      \end{tabular}

