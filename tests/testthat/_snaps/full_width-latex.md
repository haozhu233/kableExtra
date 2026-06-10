# LaTeX full_width: basic table

    Code
      kable_styling(kbl(df, format = "latex"), full_width = TRUE)
    Output
      
      \begin{tabularx}{\linewidth}{>{\raggedleft\arraybackslash}X>{\raggedleft\arraybackslash}X}
      \hline
      a & b\\
      \hline
      1 & 4\\
      \hline
      2 & 5\\
      \hline
      3 & 6\\
      \hline
      4 & 7\\
      \hline
      \end{tabularx}

---

    Code
      kable_styling(kbl(df, format = "latex", booktabs = TRUE), full_width = TRUE)
    Output
      
      \begin{tabularx}{\linewidth}{>{\raggedleft\arraybackslash}X>{\raggedleft\arraybackslash}X}
      \toprule
      a & b\\
      \midrule
      1 & 4\\
      2 & 5\\
      3 & 6\\
      4 & 7\\
      \bottomrule
      \end{tabularx}

# LaTeX full_width: alignment is mapped onto X columns

    Code
      kable_styling(kbl(df, format = "latex", align = "lcr"), full_width = TRUE)
    Output
      
      \begin{tabularx}{\linewidth}{>{\raggedright\arraybackslash}X>{\centering\arraybackslash}X>{\raggedleft\arraybackslash}X}
      \hline
      a & b & c\\
      \hline
      1 & 4 & a\\
      \hline
      2 & 5 & b\\
      \hline
      3 & 6 & c\\
      \hline
      4 & 7 & d\\
      \hline
      \end{tabularx}

# LaTeX full_width: longtable

    Code
      kable_styling(kbl(df, format = "latex", longtable = TRUE, booktabs = TRUE),
      full_width = TRUE)
    Output
      
      \begin{xltabular}{\linewidth}{>{\raggedleft\arraybackslash}X>{\raggedleft\arraybackslash}X}
      \toprule
      a & b\\
      \midrule
      1 & 4\\
      2 & 5\\
      3 & 6\\
      4 & 7\\
      \bottomrule
      \end{xltabular}

---

    Code
      kable_styling(kbl(df, format = "latex", longtable = TRUE, booktabs = TRUE,
        caption = "A long table"), full_width = TRUE, latex_options = "repeat_header")
    Output
      
      \begin{xltabular}{\linewidth}{>{\raggedleft\arraybackslash}X>{\raggedleft\arraybackslash}X}
      \caption{A long table}\\
      \toprule
      a & b\\
      \midrule
      \endfirsthead
      \caption[]{A long table \textit{(continued)}}\\
      \toprule
      a & b\\
      \midrule
      \endhead
      
      \endfoot
      \bottomrule
      \endlastfoot
      1 & 4\\
      2 & 5\\
      3 & 6\\
      4 & 7\\*
      \end{xltabular}

# LaTeX full_width: striped and caption

    Code
      kable_styling(kbl(df, format = "latex", booktabs = TRUE, caption = "Hello"),
      full_width = TRUE, latex_options = "striped")
    Output
      \begin{table}
      \centering
      \caption{Hello}
      \centering
      \begin{tabularx}{\linewidth}{>{\raggedleft\arraybackslash}X>{\raggedleft\arraybackslash}X}
      \toprule
      a & b\\
      \midrule
      \cellcolor{gray!10}{1} & \cellcolor{gray!10}{4}\\
      2 & 5\\
      \cellcolor{gray!10}{3} & \cellcolor{gray!10}{6}\\
      4 & 7\\
      \bottomrule
      \end{tabularx}
      \end{table}

# LaTeX full_width: downstream spec functions still work

    Code
      row_spec(column_spec(kable_styling(kbl(df, format = "latex", booktabs = TRUE),
      full_width = TRUE), 1, width = "3cm"), 2, bold = TRUE)
    Output
      
      \begin{tabularx}{\linewidth}{>{\raggedleft\arraybackslash}p{3cm}>{\raggedleft\arraybackslash}X>{\raggedright\arraybackslash}X}
      \toprule
      a & b & c\\
      \midrule
      1 & 4 & a\\
      \textbf{2} & \textbf{5} & \textbf{b}\\
      3 & 6 & c\\
      4 & 7 & d\\
      \bottomrule
      \end{tabularx}

---

    Code
      add_header_above(kable_styling(kbl(df, format = "latex", booktabs = TRUE),
      full_width = TRUE), c(` ` = 1, Group = 2))
    Output
      
      \begin{tabularx}{\linewidth}{>{\raggedleft\arraybackslash}X>{\raggedleft\arraybackslash}X>{\raggedright\arraybackslash}X}
      \toprule
      \multicolumn{1}{c}{ } & \multicolumn{2}{c}{Group} \\
      \cmidrule(l{3pt}r{3pt}){2-3}
      a & b & c\\
      \midrule
      1 & 4 & a\\
      2 & 5 & b\\
      3 & 6 & c\\
      4 & 7 & d\\
      \bottomrule
      \end{tabularx}

---

    Code
      footnote(kable_styling(kbl(df, format = "latex", booktabs = TRUE), full_width = TRUE),
      general = "A general footnote.")
    Output
      
      \begin{tabularx}{\linewidth}{>{\raggedleft\arraybackslash}X>{\raggedleft\arraybackslash}X>{\raggedright\arraybackslash}X}
      \toprule
      a & b & c\\
      \midrule
      1 & 4 & a\\
      2 & 5 & b\\
      3 & 6 & c\\
      4 & 7 & d\\
      \bottomrule
      \multicolumn{3}{l}{\rule{0pt}{1em}\textit{Note: }}\\
      \multicolumn{3}{l}{\rule{0pt}{1em}A general footnote.}\\
      \end{tabularx}

---

    Code
      pack_rows(kable_styling(kbl(df, format = "latex", booktabs = TRUE), full_width = TRUE),
      "Group 1", 1, 2)
    Output
      
      \begin{tabularx}{\linewidth}{>{\raggedleft\arraybackslash}X>{\raggedleft\arraybackslash}X>{\raggedright\arraybackslash}X}
      \toprule
      a & b & c\\
      \midrule
      \addlinespace[0.3em]
      \multicolumn{3}{l}{\textbf{Group 1}}\\
      \hspace{1em}1 & 4 & a\\
      \hspace{1em}2 & 5 & b\\
      3 & 6 & c\\
      4 & 7 & d\\
      \bottomrule
      \end{tabularx}

