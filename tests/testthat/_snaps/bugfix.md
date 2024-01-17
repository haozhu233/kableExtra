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

