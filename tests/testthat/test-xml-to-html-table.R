expected_table <- mtcars[1:3, 1:3]

test_xml_to_html_table_case_base <- kbl(expected_table, 'html')
test_xml_to_html_table_case_with_two_header_rows <- kbl(
  expected_table, 'html'
  ) %>%
  add_header_above(c(' ', 'group' = 3))
test_xml_to_html_table_case_with_pack_rows <- kbl(
  expected_table, 'html'
  ) %>%
  pack_rows('g1', 1, 2) %>%
  pack_rows('g2', 3, 3)
test_xml_to_html_table_case_with_color <- kbl(
  expected_table, 'html'
) %>%
  column_spec(1, color='red')
test_xml_to_html_table_case_with_footnote <- kbl(expected_table, 'html') %>%
  footnote('footnote test.')

for (test_case in list(
  test_xml_to_html_table_case_base,
  test_xml_to_html_table_case_with_two_header_rows,
  test_xml_to_html_table_case_with_pack_rows,
  test_xml_to_html_table_case_with_color,
  test_xml_to_html_table_case_with_footnote
)) {
  returned_table <- read_table_data_from_xml(kable_as_xml(test_case))
  returned_table[] <- lapply(returned_table, as.numeric)
  testthat::expect_equivalent(returned_table,expected_table)
}
