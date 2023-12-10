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

