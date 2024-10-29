test_kable_md_parser_case_base <- data.frame(
  Name = c('John', 'Jane', 'Adam'),
  Age = c(30, 28, 35),
  City = c('NY', 'LA', '')
)

md_table = as.character(knitr::kable(test_kable_md_parser_case_base, caption='sss', label='ss', align = c('l', 'c', 'r')))


