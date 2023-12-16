# basic

    Code
      kbl(df, format = "latex", tabular = "tblr", booktabs = TRUE)
    Output
      
      \begin{tblr}[         % tabularray outer open
      ]                     % tabularray outer close
      {                     % tabularray inner open
      colspec={Q[halign=l]Q[halign=r]Q[halign=r]Q[halign=r]},
      rowspec={Q[]Q[]Q[]Q[]Q[]Q[]},
      }                     % tabularray inner close
      \toprule
      car & mpg & cyl & disp\\
      \midrule
      Mazda RX4 & 21.0 & 6 & 160\\
      Mazda RX4 Wag & 21.0 & 6 & 160\\
      Datsun 710 & 22.8 & 4 & 108\\
      Hornet 4 Drive & 21.4 & 6 & 258\\
      \bottomrule
      \end{tblr}

---

    Code
      row_spec(kbl(df, format = "latex", tabular = "tblr", booktabs = TRUE), 2:3,
      bold = TRUE, background = "pink")
    Output
      
      \begin{tblr}[         % tabularray outer open
      ]                     % tabularray outer close
      {                     % tabularray inner open
      colspec={Q[halign=l]Q[halign=r]Q[halign=r]Q[halign=r]},
      rowspec={Q[]Q[]Q[bg=pink, font=\bfseries]Q[bg=pink, font=\bfseries]Q[]Q[]},
      }                     % tabularray inner close
      \toprule
      car & mpg & cyl & disp\\
      \midrule
      Mazda RX4 & 21.0 & 6 & 160\\
      Mazda RX4 Wag & 21.0 & 6 & 160\\
      Datsun 710 & 22.8 & 4 & 108\\
      Hornet 4 Drive & 21.4 & 6 & 258\\
      \bottomrule
      \end{tblr}

---

    Code
      row_spec(kbl(df, format = "latex", booktabs = TRUE), 2:3, bold = TRUE,
      background = "pink")
    Output
      
      \begin{tabular}[t]{lrrr}
      \toprule
      car & mpg & cyl & disp\\
      \midrule
      Mazda RX4 & 21.0 & 6 & 160\\
      \cellcolor{pink}{\textbf{Mazda RX4 Wag}} & \cellcolor{pink}{\textbf{21.0}} & \cellcolor{pink}{\textbf{6}} & \cellcolor{pink}{\textbf{160}}\\
      \cellcolor{pink}{\textbf{Datsun 710}} & \cellcolor{pink}{\textbf{22.8}} & \cellcolor{pink}{\textbf{4}} & \cellcolor{pink}{\textbf{108}}\\
      Hornet 4 Drive & 21.4 & 6 & 258\\
      \bottomrule
      \end{tabular}

# style and color

    Code
      column_spec(row_spec(kbl(df, format = "latex", tabular = "tblr", booktabs = TRUE),
      2:3, bold = TRUE), 1, strikeout = TRUE)
    Output
      
      \begin{tblr}[         % tabularray outer open
      ]                     % tabularray outer close
      {                     % tabularray inner open
      rowspec={Q[]Q[]Q[font=\bfseries]Q[font=\bfseries]Q[]Q[]},
      colspec={Q[halign=l, valign=m, cmd=\kableExtraTabularrayStrikeout{#1}]Q[halign=r]Q[halign=r]Q[halign=r]},
      }                     % tabularray inner close
      \toprule
      car & mpg & cyl & disp\\
      \midrule
      Mazda RX4 & 21.0 & 6 & 160\\
      Mazda RX4 Wag & 21.0 & 6 & 160\\
      Datsun 710 & 22.8 & 4 & 108\\
      Hornet 4 Drive & 21.4 & 6 & 258\\
      \bottomrule
      \end{tblr}

---

    Code
      row_spec(column_spec(kbl(df, format = "latex", tabular = "tblr", booktabs = TRUE),
      1, color = "green!30!black"), 2:3, background = "azure9")
    Output
      
      \begin{tblr}[         % tabularray outer open
      ]                     % tabularray outer close
      {                     % tabularray inner open
      colspec={Q[halign=l, fg=green!30!black, valign=m]Q[halign=r]Q[halign=r]Q[halign=r]},
      rowspec={Q[]Q[]Q[bg=azure9]Q[bg=azure9]Q[]Q[]},
      }                     % tabularray inner close
      \toprule
      car & mpg & cyl & disp\\
      \midrule
      Mazda RX4 & 21.0 & 6 & 160\\
      Mazda RX4 Wag & 21.0 & 6 & 160\\
      Datsun 710 & 22.8 & 4 & 108\\
      Hornet 4 Drive & 21.4 & 6 & 258\\
      \bottomrule
      \end{tblr}

---

    Code
      column_spec(row_spec(kbl(df, format = "latex", tabular = "tblr", booktabs = TRUE),
      2:3, background = "azure9"), 1, color = "green!30!black", strikeout = TRUE)
    Output
      
      \begin{tblr}[         % tabularray outer open
      ]                     % tabularray outer close
      {                     % tabularray inner open
      rowspec={Q[]Q[]Q[bg=azure9]Q[bg=azure9]Q[]Q[]},
      colspec={Q[halign=l, fg=green!30!black, valign=m, cmd=\kableExtraTabularrayStrikeout{#1}]Q[halign=r]Q[halign=r]Q[halign=r]},
      }                     % tabularray inner close
      \toprule
      car & mpg & cyl & disp\\
      \midrule
      Mazda RX4 & 21.0 & 6 & 160\\
      Mazda RX4 Wag & 21.0 & 6 & 160\\
      Datsun 710 & 22.8 & 4 & 108\\
      Hornet 4 Drive & 21.4 & 6 & 258\\
      \bottomrule
      \end{tblr}

---

    Code
      row_spec(row_spec(column_spec(column_spec(kbl(df, format = "latex", tabular = "tblr",
        booktabs = TRUE), 1, background = "#FFC0CB", color = "blue"), 3, background = "#FFC0CB",
      color = "#964B00"), 2, background = "azure9"), 3, color = "green!70!black")
    Output
      
      \begin{tblr}[         % tabularray outer open
      ]                     % tabularray outer close
      {                     % tabularray inner open
      colspec={Q[halign=l, fg=blue, bg=cFFC0CB, valign=m]Q[halign=r]Q[halign=r, fg=c964B00, bg=cFFC0CB, valign=m]Q[halign=r]},
      rowspec={Q[]Q[]Q[bg=azure9]Q[fg=green!70!black]Q[]Q[]},
      }                     % tabularray inner close
      \kableExtraDefineColor{cFFC0CB}{HTML}{FFC0CB}\kableExtraDefineColor{c964B00}{HTML}{964B00}
      \kableExtraDefineColor{cFFC0CB}{HTML}{FFC0CB}
      \toprule
      car & mpg & cyl & disp\\
      \midrule
      Mazda RX4 & 21.0 & 6 & 160\\
      Mazda RX4 Wag & 21.0 & 6 & 160\\
      Datsun 710 & 22.8 & 4 & 108\\
      Hornet 4 Drive & 21.4 & 6 & 258\\
      \bottomrule
      \end{tblr}

# tabularray outer options

    Code
      kable_styling(kbl(df, format = "latex", tabular = "talltblr", booktabs = TRUE),
      latex_options = list(tabularray_outer = c("caption = {A caption.}",
        "label = {tab:mytable}", "headsep = 12pt")))
    Output
      
      \begin{talltblr}[         % tabularray outer open
      caption = {A caption.}, label = {tab:mytable}, headsep = 12pt,
      ]                     % tabularray outer close
      {                     % tabularray inner open
      colspec={Q[halign=l]Q[halign=r]Q[halign=r]Q[halign=r]},
      rowspec={Q[]Q[]Q[]Q[]Q[]Q[]},
      }                     % tabularray inner close
      \toprule
      car & mpg & cyl & disp\\
      \midrule
      Mazda RX4 & 21.0 & 6 & 160\\
      Mazda RX4 Wag & 21.0 & 6 & 160\\
      Datsun 710 & 22.8 & 4 & 108\\
      Hornet 4 Drive & 21.4 & 6 & 258\\
      \bottomrule
      \end{talltblr}

# lines

    Code
      row_spec(kbl(df, format = "latex", tabular = "tblr", booktabs = TRUE), 2,
      background = "pink, ht=3.5em, valign=f")
    Output
      
      \begin{tblr}[         % tabularray outer open
      ]                     % tabularray outer close
      {                     % tabularray inner open
      colspec={Q[halign=l]Q[halign=r]Q[halign=r]Q[halign=r]},
      rowspec={Q[]Q[]Q[bg=pink, ht=3.5em, valign=f]Q[]Q[]Q[]},
      }                     % tabularray inner close
      \toprule
      car & mpg & cyl & disp\\
      \midrule
      Mazda RX4 & 21.0 & 6 & 160\\
      Mazda RX4 Wag & 21.0 & 6 & 160\\
      Datsun 710 & 22.8 & 4 & 108\\
      Hornet 4 Drive & 21.4 & 6 & 258\\
      \bottomrule
      \end{tblr}

---

    Code
      kable_styling(kbl(df, format = "latex", tabular = "tblr", vline = "", linesep = "",
        toprule = "", midrule = "", bottomrule = ""), latex_options = list(
        tabularray_inner = c("hlines={dash=dotted, fg=brown6}",
          "vlines={dash=dashed, fg=green4, wd=2pt}")))
    Output
      
      \begin{tblr}[         % tabularray outer open
      ]                     % tabularray outer close
      {                     % tabularray inner open
      hlines={dash=dotted, fg=brown6}, vlines={dash=dashed, fg=green4, wd=2pt},
      colspec={Q[halign=l]Q[halign=r]Q[halign=r]Q[halign=r]},
      rowspec={Q[]Q[]Q[]Q[]Q[]Q[]},
      }                     % tabularray inner close
      
      car & mpg & cyl & disp\\
      
      Mazda RX4 & 21.0 & 6 & 160\\
      Mazda RX4 Wag & 21.0 & 6 & 160\\
      Datsun 710 & 22.8 & 4 & 108\\
      Hornet 4 Drive & 21.4 & 6 & 258\\
      
      \end{tblr}

---

    Code
      kable_styling(kbl(df, format = "latex", tabular = "tblr", vline = "", toprule = "",
        midrule = "", bottomrule = "", linesep = ""), latex_options = list(
        tabularray_inner = c("hline{1-6}={dash=solid, fg=brown6}",
          "vline{2,3}={dash=dotted, fg=green4}")))
    Output
      
      \begin{tblr}[         % tabularray outer open
      ]                     % tabularray outer close
      {                     % tabularray inner open
      hline{1-6}={dash=solid, fg=brown6}, vline{2,3}={dash=dotted, fg=green4},
      colspec={Q[halign=l]Q[halign=r]Q[halign=r]Q[halign=r]},
      rowspec={Q[]Q[]Q[]Q[]Q[]Q[]},
      }                     % tabularray inner close
      
      car & mpg & cyl & disp\\
      
      Mazda RX4 & 21.0 & 6 & 160\\
      Mazda RX4 Wag & 21.0 & 6 & 160\\
      Datsun 710 & 22.8 & 4 & 108\\
      Hornet 4 Drive & 21.4 & 6 & 258\\
      
      \end{tblr}

# add_header_above

    Code
      add_header_above(add_header_above(kbl(mtcars[1:4, 1:5], format = "latex",
      tabular = "tblr", align = "c", booktabs = TRUE), c(` ` = 1, `\\alpha` = 2,
        `\\beta` = 3), escape = FALSE), c(`First Three` = 3, ` ` = 1, Penultimate = 1,
        ` ` = 1), italic = TRUE)
    Output
      
      \begin{tblr}[         % tabularray outer open
      ]                     % tabularray outer close
      {                     % tabularray inner open
      colspec={Q[halign=l]Q[halign=c]Q[halign=c]Q[halign=c]Q[halign=c]Q[halign=c]},
      rowspec={Q[]Q[]Q[]Q[]Q[]Q[]Q[]Q[]},
      }                     % tabularray inner close
      \toprule
      \SetCell[r=1, c=3]{font=\itshape, halign=c}First Three &&& \SetCell{font=\itshape, halign=c}  & \SetCell{font=\itshape, halign=c}Penultimate & \SetCell{font=\itshape, halign=c}  \\
      \cmidrule[lr]{1-3} \cmidrule[lr]{5-5}
      \SetCell{halign=c}  & \SetCell[r=1, c=2]{halign=c}\alpha && \SetCell[r=1, c=3]{halign=c}\beta \\
      \cmidrule[lr]{2-3} \cmidrule[lr]{4-6}
        & mpg & cyl & disp & hp & drat\\
      \midrule
      Mazda RX4 & 21.0 & 6 & 160 & 110 & 3.90\\
      Mazda RX4 Wag & 21.0 & 6 & 160 & 110 & 3.90\\
      Datsun 710 & 22.8 & 4 & 108 & 93 & 3.85\\
      Hornet 4 Drive & 21.4 & 6 & 258 & 110 & 3.08\\
      \bottomrule
      \end{tblr}

# issue #616

    Code
      row_spec(row_spec(mtcars %>% head(n = 10) %>% kbl(format = "latex", tabular = "tblr") %>%
        kable_styling(latex_options = "HOLD_position"), seq(1, 10, by = 2),
      background = "gray8, ht=1cm"), seq(2, 10, by = 2), background = "white, ht=1cm")
    Output
      
      \begin{tblr}[         % tabularray outer open
      ]                     % tabularray outer close
      {                     % tabularray inner open
      colspec={Q[halign=l]|Q[halign=r]|Q[halign=r]|Q[halign=r]|Q[halign=r]|Q[halign=r]|Q[halign=r]|Q[halign=r]|Q[halign=r]|Q[halign=r]|Q[halign=r]|Q[halign=r]},
      rowspec={Q[]Q[bg=gray8, ht=1cm]Q[bg=white, ht=1cm]Q[bg=gray8, ht=1cm]Q[bg=white, ht=1cm]Q[bg=gray8, ht=1cm]Q[bg=white, ht=1cm]Q[bg=gray8, ht=1cm]Q[bg=white, ht=1cm]Q[bg=gray8, ht=1cm]Q[bg=white, ht=1cm]Q[]},
      }                     % tabularray inner close
      \hline
        & mpg & cyl & disp & hp & drat & wt & qsec & vs & am & gear & carb\\
      \hline
      Mazda RX4 & 21.0 & 6 & 160.0 & 110 & 3.90 & 2.620 & 16.46 & 0 & 1 & 4 & 4\\
      Mazda RX4 Wag & 21.0 & 6 & 160.0 & 110 & 3.90 & 2.875 & 17.02 & 0 & 1 & 4 & 4\\
      Datsun 710 & 22.8 & 4 & 108.0 & 93 & 3.85 & 2.320 & 18.61 & 1 & 1 & 4 & 1\\
      Hornet 4 Drive & 21.4 & 6 & 258.0 & 110 & 3.08 & 3.215 & 19.44 & 1 & 0 & 3 & 1\\
      Hornet Sportabout & 18.7 & 8 & 360.0 & 175 & 3.15 & 3.440 & 17.02 & 0 & 0 & 3 & 2\\
      \addlinespace
      Valiant & 18.1 & 6 & 225.0 & 105 & 2.76 & 3.460 & 20.22 & 1 & 0 & 3 & 1\\
      Duster 360 & 14.3 & 8 & 360.0 & 245 & 3.21 & 3.570 & 15.84 & 0 & 0 & 3 & 4\\
      Merc 240D & 24.4 & 4 & 146.7 & 62 & 3.69 & 3.190 & 20.00 & 1 & 0 & 4 & 2\\
      Merc 230 & 22.8 & 4 & 140.8 & 95 & 3.92 & 3.150 & 22.90 & 1 & 0 & 4 & 2\\
      Merc 280 & 19.2 & 6 & 167.6 & 123 & 3.92 & 3.440 & 18.30 & 1 & 0 & 4 & 4\\
      \hline
      \end{tblr}

# issue #636

    Code
      kable_styling(kbl(DF, format = "latex", tabular = "tblr", digits = 2, escape = FALSE),
      latex_options = "striped")
    Output
      
      \begin{tblr}[         % tabularray outer open
      ]                     % tabularray outer close
      {                     % tabularray inner open
      colspec={Q[halign=l]|Q[halign=l]|Q[halign=l]},
      rowspec={Q[]Q[bg=gray!10]Q[]Q[bg=gray!10]Q[]Q[bg=gray!10]Q[]Q[bg=gray!10]Q[]Q[bg=gray!10]Q[]Q[]},
      }                     % tabularray inner close
      \hline
      V1 & V2 & V3\\
      \hline
      u & \SetCell{bg=red, halign=c}1.74 & \SetCell{bg=yellow, halign=c}0.38\\
      v & \SetCell{bg=yellow, halign=c}0.11 & \SetCell{bg=red, halign=c}0.84\\
      u & \SetCell{bg=red, halign=c}0.91 & \SetCell{bg=red, halign=c}0.58\\
      m & \SetCell{bg=red, halign=c}1.06 & \SetCell{bg=red, halign=c}1.45\\
      o & \SetCell{bg=yellow, halign=c}0.02 & \SetCell{bg=yellow, halign=c}0.28\\
      \addlinespace
      x & \SetCell{bg=yellow, halign=c}0.29 & \SetCell{bg=red, halign=c}1.12\\
      m & \SetCell{bg=yellow, halign=c}0.03 & \SetCell{bg=red, halign=c}1.32\\
      p & \SetCell{bg=red, halign=c}0.73 & \SetCell{bg=red, halign=c}1.41\\
      g & \SetCell{bg=red, halign=c}0.53 & \SetCell{bg=red, halign=c}0.94\\
      h & \SetCell{bg=yellow, halign=c}0.20 & \SetCell{bg=red, halign=c}2.28\\
      \hline
      \end{tblr}

# issue #738

    Code
      kable_styling(kbl(t, format = "latex", tabular = "tblr", escape = FALSE),
      latex_options = c("striped"))
    Output
      
      \begin{tblr}[         % tabularray outer open
      ]                     % tabularray outer close
      {                     % tabularray inner open
      colspec={Q[halign=r]|Q[halign=l]},
      rowspec={Q[]Q[bg=gray!10]Q[]Q[bg=gray!10]Q[]Q[bg=gray!10]Q[]Q[]},
      }                     % tabularray inner close
      \hline
      speed & dist\\
      \hline
      4 & \SetCell{bg=red, halign=c}2\\
      4 & \SetCell{bg=red, halign=c}10\\
      7 & \SetCell{bg=red, halign=c}4\\
      7 & \SetCell{bg=red, halign=c}22\\
      8 & \SetCell{bg=red, halign=c}16\\
      \addlinespace
      9 & \SetCell{bg=red, halign=c}10\\
      \hline
      \end{tblr}

