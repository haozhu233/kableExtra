context("Add_Footnote")
#####

rtable <- data.frame(variable = c("mpg", "wt"), mean = c(20.091, 3.217), sd = c(6.027, 0.978))
htmltable_1 <- kable(rtable, format = "html")
htmltable_2 <- kable(rtable, format = "html", caption = "Table", table.attr = "class = 'table table-striped table-hover'")
htmltable_3 <- kable(rtable, row.names = T, format = "html", caption = "Table", table.attr = "class = 'table table-striped table-hover'")
latextable_1 <- kable(rtable, format = "latex")
latextable_2 <- kable(rtable, format = "latex", caption = "Table", row.names = T, align = c("l", "c", "c"))
latextable_3 <- kable(rtable, format = "latex", booktab = T)
latextable_4 <- kable(rtable, format = "latex", booktab = T, caption = "Table")
latextable_5 <- kable(rtable, format = "latex", longtable = T)
latextable_6 <- kable(rtable, format = "latex", longtable = T, caption = "Table long")
