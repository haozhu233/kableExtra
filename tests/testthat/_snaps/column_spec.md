# Rmarkdown example from inst/

    Code
      column_spec(kbl(df, format = "latex"), 2, bold = TRUE, monospace = TRUE,
      underline = TRUE, italic = TRUE, color = "red", background = "#FFFF00", width = "3in",
      border_right = TRUE)
    Output
      
      \begin{tabular}[t]{r|>{\raggedleft\arraybackslash}p{3in}|}
      \hline
      a & b\\
      \hline
      1 & \cellcolor[HTML]{FFFF00}{\textcolor{red}{\underline{\ttfamily{\em{\textbf{4}}}}}}\\
      \hline
      2 & \cellcolor[HTML]{FFFF00}{\textcolor{red}{\underline{\ttfamily{\em{\textbf{5}}}}}}\\
      \hline
      3 & \cellcolor[HTML]{FFFF00}{\textcolor{red}{\underline{\ttfamily{\em{\textbf{6}}}}}}\\
      \hline
      4 & \cellcolor[HTML]{FFFF00}{\textcolor{red}{\underline{\ttfamily{\em{\textbf{7}}}}}}\\
      \hline
      \end{tabular}

---

    Code
      kbl(dt, format = "latex", booktabs = TRUE) %>% kable_styling(full_width = TRUE) %>%
        column_spec(1, width = "8cm")
    Output
      
      \begin{tabu} to \linewidth {>{\raggedright\arraybackslash}p{8cm}>{\raggedleft}X>{\raggedleft}X>{\raggedleft}X>{\raggedleft}X>{\raggedleft}X>{\raggedleft}X}
      \toprule
        & mpg & cyl & disp & hp & drat & wt\\
      \midrule
      Mazda RX4 & 21.0 & 6 & 160 & 110 & 3.90 & 2.620\\
      Mazda RX4 Wag & 21.0 & 6 & 160 & 110 & 3.90 & 2.875\\
      Datsun 710 & 22.8 & 4 & 108 & 93 & 3.85 & 2.320\\
      Hornet 4 Drive & 21.4 & 6 & 258 & 110 & 3.08 & 3.215\\
      Hornet Sportabout & 18.7 & 8 & 360 & 175 & 3.15 & 3.440\\
      \bottomrule
      \end{tabu}

