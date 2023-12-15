# Issue #806: custom rule widths

    Code
      footnote(add_footnote(add_header_above(kable_styling(kbl(mtcars[1:3, 1:4],
      caption = "kable vary line thickness", format = "latex", booktabs = TRUE,
      toprule = "\\toprule[4pt]", midrule = "\\midrule[3pt]", bottomrule = "\\bottomrule[5pt]",
      linesep = "\\midrule[2pt]"), repeat_header_text = TRUE), c("", `Group 1` = 2,
        `Group 2` = 2)), "The footnote"), "Another footnote")
    Output
      \begin{table}
      \centering
      \caption{kable vary line thickness}
      \centering
      \begin{tabular}[t]{lrrrr}
      \toprule[4pt]
      \multicolumn{1}{c}{} & \multicolumn{2}{c}{Group 1} & \multicolumn{2}{c}{Group 2} \\
      \cmidrule(l{3pt}r{3pt}){2-3} \cmidrule(l{3pt}r{3pt}){4-5}
        & mpg & cyl & disp & hp\\
      \midrule[3pt]
      Mazda RX4 & 21.0 & 6 & 160 & 110\\
      \midrule[2pt]
      Mazda RX4 Wag & 21.0 & 6 & 160 & 110\\
      \midrule[2pt]
      Datsun 710 & 22.8 & 4 & 108 & 93\\
      \bottomrule[5pt]
      \multicolumn{5}{l}{\rule{0pt}{1em}\textit{Note: }}\\
      \multicolumn{5}{l}{\rule{0pt}{1em}Another footnote}\\
      \multicolumn{5}{l}{\textsuperscript{a} The footnote}\\
      \end{tabular}
      \end{table}

# Issue #796

    Code
      kable_styling(kbl(mtcars[1:3, 1:4], caption = "Demo table", booktabs = TRUE,
      format = "latex"), latex_options = c("striped", "hold_position"))
    Output
      \begin{table}[!h]
      \centering
      \caption{Demo table}
      \centering
      \begin{tabular}[t]{lrrrr}
      \toprule
        & mpg & cyl & disp & hp\\
      \midrule
      \cellcolor{gray!10}{Mazda RX4} & \cellcolor{gray!10}{21.0} & \cellcolor{gray!10}{6} & \cellcolor{gray!10}{160} & \cellcolor{gray!10}{110}\\
      Mazda RX4 Wag & 21.0 & 6 & 160 & 110\\
      \cellcolor{gray!10}{Datsun 710} & \cellcolor{gray!10}{22.8} & \cellcolor{gray!10}{4} & \cellcolor{gray!10}{108} & \cellcolor{gray!10}{93}\\
      \bottomrule
      \end{tabular}
      \end{table}

