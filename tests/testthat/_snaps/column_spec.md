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

