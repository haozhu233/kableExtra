# LaTeX: basic argument tests

    Code
      row_spec(kbl(df, format = "latex"), 3, bold = TRUE, monospace = TRUE,
      underline = TRUE, italic = TRUE)
    Output
      
      \begin{tabular}[t]{r|r}
      \hline
      X1.4 & X4.7\\
      \hline
      1 & 4\\
      \hline
      2 & 5\\
      \hline
      \underline{\ttfamily{\em{\textbf{3}}}} & \underline{\ttfamily{\em{\textbf{6}}}}\\
      \hline
      4 & 7\\
      \hline
      \end{tabular}

---

    Code
      row_spec(kbl(df, format = "latex"), 3, angle = 45)
    Output
      
      \begin{tabular}[t]{r|r}
      \hline
      X1.4 & X4.7\\
      \hline
      1 & 4\\
      \hline
      2 & 5\\
      \hline
      \rotatebox{45}{3} & \rotatebox{45}{6}\\
      \hline
      4 & 7\\
      \hline
      \end{tabular}

---

    Code
      row_spec(kbl(df, format = "latex"), 3, font_size = 10)
    Output
      
      \begin{tabular}[t]{r|r}
      \hline
      X1.4 & X4.7\\
      \hline
      1 & 4\\
      \hline
      2 & 5\\
      \hline
      \begingroup\fontsize{10}{12}\selectfont 3\endgroup & \begingroup\fontsize{10}{12}\selectfont 6\endgroup\\
      \hline
      4 & 7\\
      \hline
      \end{tabular}

---

    Code
      row_spec(kbl(df, format = "latex"), 3, color = "blue", background = "pink")
    Output
      
      \begin{tabular}[t]{r|r}
      \hline
      X1.4 & X4.7\\
      \hline
      1 & 4\\
      \hline
      2 & 5\\
      \hline
      \cellcolor{pink}{\textcolor{blue}{3}} & \cellcolor{pink}{\textcolor{blue}{6}}\\
      \hline
      4 & 7\\
      \hline
      \end{tabular}

---

    Code
      row_spec(kable_classic(kbl(df, format = "latex", booktabs = TRUE)), 3,
      hline_after = TRUE)
    Output
      \begin{table}
      \centering
      \begin{tabular}[t]{rr}
      \toprule
      X1.4 & X4.7\\
      \midrule
      1 & 4\\
      2 & 5\\
      3 & 6\\
      \midrule
      4 & 7\\
      \bottomrule
      \end{tabular}
      \end{table}

---

    Code
      row_spec(row_spec(kbl(df, format = "latex"), 1, align = "r"), 2, align = "c")
    Output
      
      \begin{tabular}[t]{l|l}
      \hline
      a & b\\
      \hline
      \multicolumn{1}{r}{ab} & \multicolumn{1}{r}{abcd}\\
      \hline
      \multicolumn{1}{c}{abc} & \multicolumn{1}{c}{abcde}\\
      \hline
      \end{tabular}

# extra_latex_after: Example from documentation

    Code
      kbl(collapse_rows_dt[-1], format = "latex", align = "c", booktabs = TRUE) %>%
        column_spec(1, bold = T, width = "5em") %>% row_spec(c(1:7, 11:12) - 1,
      extra_latex_after = "\\rowcolor{gray!6}") %>% collapse_rows(1, latex_hline = "none")
    Output
      
      \begin{tabular}[t]{>{\centering\arraybackslash}p{5em}cc}
      \toprule
      C2 & C3 & C4\\
      \rowcolor{gray!6}
      \midrule
      \textbf{c} & 1 & 1\\
      \rowcolor{gray!6}
      \textbf{c} & 2 & 0\\
      \rowcolor{gray!6}
      \textbf{c} & 3 & 0\\
      \rowcolor{gray!6}
      \textbf{c} & 4 & 0\\
      \rowcolor{gray!6}
      \textbf{c} & 5 & 1\\
      \rowcolor{gray!6}
      \textbf{c} & 6 & 1\\
      \rowcolor{gray!6}
      \textbf{c} & 7 & 1\\
      \textbf{d} & 8 & 1\\
      \textbf{d} & 9 & 1\\
      \textbf{d} & 10 & 0\\
      \rowcolor{gray!6}
      \textbf{c} & 11 & 0\\
      \rowcolor{gray!6}
      \textbf{c} & 12 & 0\\
      \textbf{d} & 13 & 1\\
      \textbf{d} & 14 & 0\\
      \textbf{d} & 15 & 1\\
      \bottomrule
      \end{tabular}

# issue #582: RMD kable styling repeates rows when rendering striped table to LaTeX in some data combination

    Code
      kable_styling(kbl(dfWithDot, format = "latex"), latex_options = "striped")
    Output
      \begin{table}
      \centering
      \begin{tabular}[t]{l|l|l|l|l}
      \hline
      A & B1 & B2 & B3 & B.\\
      \hline
      \cellcolor{gray!10}{A1} & \cellcolor{gray!10}{A1B1} & \cellcolor{gray!10}{A1B2} & \cellcolor{gray!10}{A1B3} & \cellcolor{gray!10}{A1B.}\\
      \hline
      A2 & A2B1 & A2B2 & A2B3 & A2B.\\
      \hline
      \cellcolor{gray!10}{A.} & \cellcolor{gray!10}{A.B1} & \cellcolor{gray!10}{A.B2} & \cellcolor{gray!10}{A.B3} & \cellcolor{gray!10}{A.B.}\\
      \hline
      \end{tabular}
      \end{table}

