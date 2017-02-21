## ------------------------------------------------------------------------
library(knitr)
library(kableExtra)
dt <- mtcars[1:5, 1:2]

kable(dt, format = "html")

## ------------------------------------------------------------------------
kable(dt, format = "html", table.attr = htmlTable_styling())

## ------------------------------------------------------------------------
kable(dt, format = "html", table.attr = htmlTable_styling(bootstrap_options = "striped"))

## ------------------------------------------------------------------------
kable(dt, format = "html", table.attr = htmlTable_styling(c("striped", "hover")))

## ------------------------------------------------------------------------
kable(dt, format = "html", 
      table.attr = htmlTable_styling(c("striped", "bordered", "hover", "condensed", "responsive")))

## ------------------------------------------------------------------------
kable(dt, format = "html", 
      table.attr = htmlTable_styling(
        c("striped", "bordered", "condensed"), 
        full_width = F, 
        float = "center"
      ))

