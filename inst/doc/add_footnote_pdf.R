## ----through_pandoc------------------------------------------------------
library(knitr)
library(kableExtra)
dt <- mtcars[1:5, 1:2]
colnames(dt)[1] <- c("mpg[note]")
rownames(dt)[2] <- c("Mazda RX4 Wag[note]")
dt[3, 2] <- paste0(dt[3, 2], "[note]")

kable(dt) %>%
  add_footnote(c("footnote 1", "footnote 2", "footnote 3"))

## ----through_latex_plain-------------------------------------------------
kable(dt, format = "latex") %>%
  add_footnote(c("footnote 1", "footnote 2", "footnote 3"))

## ----through_latex_booktabs----------------------------------------------
kable(dt, format = "latex", booktabs = T) %>%
  add_footnote(c("footnote 1", "footnote 2", "footnote 3"))

## ----through_latex_longtable---------------------------------------------
kable(dt, format = "latex", longtable = T, booktabs = T) %>%
  add_footnote(c("footnote 1", "footnote 2", "footnote 3"))

