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

