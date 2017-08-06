kableExtra 0.4.0
--------------------------------------------------------------------------------
* Add scroll_box for HTML table for extremely long/wide tables

* Fixed a bug in collapse_row on removing addlinespace

* Removed addlinespace from group_rows

* Added monospace to column_spec & row_spec

* Lowered R version requirement to R 3.1.0

* Added testthat (#28, thank you @wibeasley)

* For all HTML table functions, changed reading method from `xml2::read_xml` to
`xml2::read_html` plus some extra steps to avoid errors when HTML contents
don't fulfill XHTML requirements (#24)

* For all HTML table functions, improved exportation method to remove the XML
declaration. (Stackoverflow: https://stackoverflow.com/questions/45262308/knit2wp-adds-text-with-kableextra)

* Added `repeat_header_method` in kable_styling (#25)

* Fixed a bug in `position` in `kable_styling()` (https://stackoverflow.com/questions/45378664/kableextra-rmarkdown-tables-aligning-grouping-row-labels-and-footnotes)

* Improved striped line look on tables with multiple layers of header rows. (#31)

* Added escape to `add_header_above` and `group_rows`

* Added PDF vignette to the CRAN vignette folder

kableExtra 0.3.0
--------------------------------------------------------------------------------
* Improved the look of HTML grouped header row (again) by adding spaces between
groups

* Fixed a bug in grouped header row.

* Fixed a bug in grouped rows: https://stackoverflow.com/questions/44360040/group-rows-function-in-kableextra-package-not-grouping-rows

* Fixed a bug in grouped rows with ()[].

* Added a new LaTeX option `repeat_header` in `kable_styling` for repeating
header rows in a longtable environment.

* Fixed a bug in add_header_above to allow special symbol in extra header rows.

* Allow column_spec automatically align when width is specified.

* Added bold/italic options to add_header_above.

* Added `collapse_rows` to collapse repeated rows to multirow cell

* Improve package-level documentation based on @wibeasley's suggestion

kableExtra 0.2.1
--------------------------------------------------------------------------------

* Added `column_spec()` to customize the look of the selected column.

* Improved the look of HTML grouped header row.

* Fixed an error in a documentation.


kableExtra 0.2.0
--------------------------------------------------------------------------------

* Added `add_indent()` feature to add indentations to rows in HTML and LaTeX.

* Added `group_rows()` feature to group rows together in HTML and LaTeX.

* Changing `font_size` in `kable_styling` won't affect table caption any more.

* Fixed a bug in `scale_down` in `kable_styling` on landscape page in LaTeX.

* Changed `stop` to `message` on format checking.

* Added an example for how to use it in Shiny.

* Fixed a few errors in the LaTeX vignette file.


kableExtra 0.1.0
--------------------------------------------------------------------------------

* Initial Release
