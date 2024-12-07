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

# awesome table example

    Code
      kbl(collapse_rows_dt[-1], align = "c", booktabs = T, format = "latex") %>%
        column_spec(1, bold = T, width = "5em") %>% row_spec(c(1:7, 11:12) - 1,
      extra_latex_after = "\\rowcolor{gray!6}") %>% collapse_rows(1, latex_hline = "none")
    Output
      
      \begin{tabular}[t]{>{\centering\arraybackslash}p{5em}cc}
      \toprule
      C2 & C3 & C4\\
      \rowcolor{gray!6}
      \midrule
       & 1 & 0\\
      
      \rowcolor{gray!6}
       & 2 & 1\\
      
      \rowcolor{gray!6}
       & 3 & 0\\
      
      \rowcolor{gray!6}
       & 4 & 0\\
      
      \rowcolor{gray!6}
       & 5 & 1\\
      
      \rowcolor{gray!6}
       & 6 & 0\\
      
      \rowcolor{gray!6}
      \multirow{-7}{5em}[\normalbaselineskip]{\centering\arraybackslash \textbf{c}} & 7 & 1\\
      
       & 8 & 0\\
      
       & 9 & 1\\
      
      \multirow{-3}{5em}[\normalbaselineskip]{\centering\arraybackslash \textbf{d}} & 10 & 0\\
      
      \rowcolor{gray!6}
       & 11 & 1\\
      
      \rowcolor{gray!6}
      \multirow{-2}{5em}[\normalbaselineskip]{\centering\arraybackslash \textbf{c}} & 12 & 1\\
      
       & 13 & 0\\
      
       & 14 & 0\\
      
      \multirow{-3}{5em}[\normalbaselineskip]{\centering\arraybackslash \textbf{d}} & 15 & 1\\
      \bottomrule
      \end{tabular}

# Issue #576: string font_size in html

    Code
      cat(data.frame(a = c("foo", "bar", "foo", "bar", "foo", "bar")) %>% kbl() %>%
        row_spec(3, font_size = "xx-large"))
    Output
      <table>
       <thead>
        <tr>
         <th style="text-align:left;"> a </th>
        </tr>
       </thead>
      <tbody>
        <tr>
         <td style="text-align:left;"> foo </td>
        </tr>
        <tr>
         <td style="text-align:left;"> bar </td>
        </tr>
        <tr>
         <td style="text-align:left;font-size: xx-large;"> foo </td>
        </tr>
        <tr>
         <td style="text-align:left;"> bar </td>
        </tr>
        <tr>
         <td style="text-align:left;"> foo </td>
        </tr>
        <tr>
         <td style="text-align:left;"> bar </td>
        </tr>
      </tbody>
      </table>

