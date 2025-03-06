# Issue #887: multiple escapes

    Code
      kbl(x = head(mtcars), format = "latex") %>% pack_rows(group_label = "Group $\\Delta = \\text{A}^1$",
        start_row = 2, end_row = 5, escape = FALSE)
    Output
      
      \begin{tabular}[t]{l|r|r|r|r|r|r|r|r|r|r|r}
      \hline
        & mpg & cyl & disp & hp & drat & wt & qsec & vs & am & gear & carb\\
      \hline
      Mazda RX4 & 21.0 & 6 & 160 & 110 & 3.90 & 2.620 & 16.46 & 0 & 1 & 4 & 4\\
      \hline
      \multicolumn{12}{l}{\textbf{Group $\Delta = \text{A}^1$}}\\
      \hline
      \hspace{1em}Mazda RX4 Wag & 21.0 & 6 & 160 & 110 & 3.90 & 2.875 & 17.02 & 0 & 1 & 4 & 4\\
      \hline
      \hspace{1em}Datsun 710 & 22.8 & 4 & 108 & 93 & 3.85 & 2.320 & 18.61 & 1 & 1 & 4 & 1\\
      \hline
      \hspace{1em}Hornet 4 Drive & 21.4 & 6 & 258 & 110 & 3.08 & 3.215 & 19.44 & 1 & 0 & 3 & 1\\
      \hline
      \hspace{1em}Hornet Sportabout & 18.7 & 8 & 360 & 175 & 3.15 & 3.440 & 17.02 & 0 & 0 & 3 & 2\\
      \hline
      Valiant & 18.1 & 6 & 225 & 105 & 2.76 & 3.460 & 20.22 & 1 & 0 & 3 & 1\\
      \hline
      \end{tabular}

# Issue #876: complex alignment

    Code
      kbl(x = mtcars[1:2, 1:2], format = "latex", align = rep("p{2cm}", 2)) %>%
        kable_styling(latex_options = "scale_down")
    Output
      \begin{table}
      \centering
      \resizebox{\ifdim\width>\linewidth\linewidth\else\width\fi}{!}{
      \begin{tabular}[t]{l|p{2cm}|p{2cm}}
      \hline
        & mpg & cyl\\
      \hline
      Mazda RX4 & 21 & 6\\
      \hline
      Mazda RX4 Wag & 21 & 6\\
      \hline
      \end{tabular}}
      \end{table}

# Issue #861:  pack_rows with tabularx

    Code
      kbl(mtcars, format = "latex", tabular = "tabularx", valign = "{\\textwidth}") %>%
        kableExtra::pack_rows("XXX", 1, 2)
    Output
      
      \begin{tabularx}{\textwidth}{l|r|r|r|r|r|r|r|r|r|r|r}
      \hline
        & mpg & cyl & disp & hp & drat & wt & qsec & vs & am & gear & carb\\
      \hline
      \multicolumn{12}{l}{\textbf{XXX}}\\
      \hline
      \hspace{1em}Mazda RX4 & 21.0 & 6 & 160.0 & 110 & 3.90 & 2.620 & 16.46 & 0 & 1 & 4 & 4\\
      \hline
      \hspace{1em}Mazda RX4 Wag & 21.0 & 6 & 160.0 & 110 & 3.90 & 2.875 & 17.02 & 0 & 1 & 4 & 4\\
      \hline
      Datsun 710 & 22.8 & 4 & 108.0 & 93 & 3.85 & 2.320 & 18.61 & 1 & 1 & 4 & 1\\
      \hline
      Hornet 4 Drive & 21.4 & 6 & 258.0 & 110 & 3.08 & 3.215 & 19.44 & 1 & 0 & 3 & 1\\
      \hline
      Hornet Sportabout & 18.7 & 8 & 360.0 & 175 & 3.15 & 3.440 & 17.02 & 0 & 0 & 3 & 2\\
      \hline
      Valiant & 18.1 & 6 & 225.0 & 105 & 2.76 & 3.460 & 20.22 & 1 & 0 & 3 & 1\\
      \hline
      Duster 360 & 14.3 & 8 & 360.0 & 245 & 3.21 & 3.570 & 15.84 & 0 & 0 & 3 & 4\\
      \hline
      Merc 240D & 24.4 & 4 & 146.7 & 62 & 3.69 & 3.190 & 20.00 & 1 & 0 & 4 & 2\\
      \hline
      Merc 230 & 22.8 & 4 & 140.8 & 95 & 3.92 & 3.150 & 22.90 & 1 & 0 & 4 & 2\\
      \hline
      Merc 280 & 19.2 & 6 & 167.6 & 123 & 3.92 & 3.440 & 18.30 & 1 & 0 & 4 & 4\\
      \hline
      Merc 280C & 17.8 & 6 & 167.6 & 123 & 3.92 & 3.440 & 18.90 & 1 & 0 & 4 & 4\\
      \hline
      Merc 450SE & 16.4 & 8 & 275.8 & 180 & 3.07 & 4.070 & 17.40 & 0 & 0 & 3 & 3\\
      \hline
      Merc 450SL & 17.3 & 8 & 275.8 & 180 & 3.07 & 3.730 & 17.60 & 0 & 0 & 3 & 3\\
      \hline
      Merc 450SLC & 15.2 & 8 & 275.8 & 180 & 3.07 & 3.780 & 18.00 & 0 & 0 & 3 & 3\\
      \hline
      Cadillac Fleetwood & 10.4 & 8 & 472.0 & 205 & 2.93 & 5.250 & 17.98 & 0 & 0 & 3 & 4\\
      \hline
      Lincoln Continental & 10.4 & 8 & 460.0 & 215 & 3.00 & 5.424 & 17.82 & 0 & 0 & 3 & 4\\
      \hline
      Chrysler Imperial & 14.7 & 8 & 440.0 & 230 & 3.23 & 5.345 & 17.42 & 0 & 0 & 3 & 4\\
      \hline
      Fiat 128 & 32.4 & 4 & 78.7 & 66 & 4.08 & 2.200 & 19.47 & 1 & 1 & 4 & 1\\
      \hline
      Honda Civic & 30.4 & 4 & 75.7 & 52 & 4.93 & 1.615 & 18.52 & 1 & 1 & 4 & 2\\
      \hline
      Toyota Corolla & 33.9 & 4 & 71.1 & 65 & 4.22 & 1.835 & 19.90 & 1 & 1 & 4 & 1\\
      \hline
      Toyota Corona & 21.5 & 4 & 120.1 & 97 & 3.70 & 2.465 & 20.01 & 1 & 0 & 3 & 1\\
      \hline
      Dodge Challenger & 15.5 & 8 & 318.0 & 150 & 2.76 & 3.520 & 16.87 & 0 & 0 & 3 & 2\\
      \hline
      AMC Javelin & 15.2 & 8 & 304.0 & 150 & 3.15 & 3.435 & 17.30 & 0 & 0 & 3 & 2\\
      \hline
      Camaro Z28 & 13.3 & 8 & 350.0 & 245 & 3.73 & 3.840 & 15.41 & 0 & 0 & 3 & 4\\
      \hline
      Pontiac Firebird & 19.2 & 8 & 400.0 & 175 & 3.08 & 3.845 & 17.05 & 0 & 0 & 3 & 2\\
      \hline
      Fiat X1-9 & 27.3 & 4 & 79.0 & 66 & 4.08 & 1.935 & 18.90 & 1 & 1 & 4 & 1\\
      \hline
      Porsche 914-2 & 26.0 & 4 & 120.3 & 91 & 4.43 & 2.140 & 16.70 & 0 & 1 & 5 & 2\\
      \hline
      Lotus Europa & 30.4 & 4 & 95.1 & 113 & 3.77 & 1.513 & 16.90 & 1 & 1 & 5 & 2\\
      \hline
      Ford Pantera L & 15.8 & 8 & 351.0 & 264 & 4.22 & 3.170 & 14.50 & 0 & 1 & 5 & 4\\
      \hline
      Ferrari Dino & 19.7 & 6 & 145.0 & 175 & 3.62 & 2.770 & 15.50 & 0 & 1 & 5 & 6\\
      \hline
      Maserati Bora & 15.0 & 8 & 301.0 & 335 & 3.54 & 3.570 & 14.60 & 0 & 1 & 5 & 8\\
      \hline
      Volvo 142E & 21.4 & 4 & 121.0 & 109 & 4.11 & 2.780 & 18.60 & 1 & 1 & 4 & 2\\
      \hline
      \end{tabularx}

# Issue #836:  latex allowed in add_header_above

    Code
      add_header_above(kbl(mtcars[1:2, 1:2], col.names = NULL, format = "latex"),
      "\\textbf{HEADER}", escape = FALSE)
    Output
      
      \begin{tabular}[t]{l|r|r}
      \hline
      \multicolumn{1}{c}{\textbf{HEADER}} \\
      \cline{1-1}
      Mazda RX4 & 21 & 6\\
      \hline
      Mazda RX4 Wag & 21 & 6\\
      \hline
      \end{tabular}

# Issue #806: custom rule widths

    Code
      footnote(add_footnote(add_header_above(kable_styling(kbl(mtcars[1:3, 1:4],
      caption = "kable vary line thickness", booktabs = TRUE, toprule = "\\toprule[4pt]",
      midrule = "\\midrule[3pt]", bottomrule = "\\bottomrule[5pt]", linesep = "\\midrule[2pt]"),
      repeat_header_text = TRUE), c("", `Group 1` = 2, `Group 2` = 2)),
      "The footnote"), "Another footnote")
    Output
      <table class="table" style="margin-left: auto; margin-right: auto;border-bottom: 0;">
      <caption>kable vary line thickness</caption>
       <thead>
      <tr>
      <th style="empty-cells: hide;border-bottom:hidden;" colspan="1"></th>
      <th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Group 1</div></th>
      <th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Group 2</div></th>
      </tr>
        <tr>
         <th style="text-align:left;">   </th>
         <th style="text-align:right;"> mpg </th>
         <th style="text-align:right;"> cyl </th>
         <th style="text-align:right;"> disp </th>
         <th style="text-align:right;"> hp </th>
        </tr>
       </thead>
      <tbody>
        <tr>
         <td style="text-align:left;"> Mazda RX4 </td>
         <td style="text-align:right;"> 21.0 </td>
         <td style="text-align:right;"> 6 </td>
         <td style="text-align:right;"> 160 </td>
         <td style="text-align:right;"> 110 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> Mazda RX4 Wag </td>
         <td style="text-align:right;"> 21.0 </td>
         <td style="text-align:right;"> 6 </td>
         <td style="text-align:right;"> 160 </td>
         <td style="text-align:right;"> 110 </td>
        </tr>
        <tr>
         <td style="text-align:left;"> Datsun 710 </td>
         <td style="text-align:right;"> 22.8 </td>
         <td style="text-align:right;"> 4 </td>
         <td style="text-align:right;"> 108 </td>
         <td style="text-align:right;"> 93 </td>
        </tr>
      </tbody>
      <tfoot>
      <tr>
      <td style="padding: 0; border:0;" colspan="100%">
      <sup>a</sup> The footnote</td>
      </tr>
      </tfoot>
      <tfoot>
      <tr><td style="padding: 0; " colspan="100%"><span style="font-style: italic;">Note: </span></td></tr>
      <tr><td style="padding: 0; " colspan="100%">
      <sup></sup> Another footnote</td></tr>
      </tfoot>
      </table>

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

