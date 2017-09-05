kableExtra 0.5.0.9000
--------------------------------------------------------------------------------
* Added documentations about those color/background color options in column_spec 
and row_spec

* Added the kableExtra_latex_packages function to quickly print all necessary 
LaTeX packages on the screen. 

kableExtra 0.5.0 (a big LaTeX update)
--------------------------------------------------------------------------------
* Now column_spec & row_spec can customize font & cell background color with 
2 new options `color` & `background`. Also, you can draw border lines using 
`border_left` or `border_right` when you are using `column_spec`.

* Making it optional to pre-load LaTeX package on package startup

* Adding documentations about dependencies on LaTeX packages and the auto-load 
behavior of kableExtra

* Improved the look of full_width in kable_styling LaTeX and add tabu to LaTeX
dependency

* Added enc2utf8() to improve behavior in non-UTF8 machines. 

* Added HOLD_position, which uses LaTeX package `float` and H, for a stronger 
hold on table position. (Thanks @robshep)

* Move conditional usepackage_latex() calls to zzz.R for more consistant 
behavior

* Now you can change strip color for LaTeX tables. 

* Added support to tables with caption.short. 

* Added kable_as_image() for LaTeX tables. This function will render the LaTeX
code snippet to a piece of standalone PDF file and then convert that to an 
image, which will then be included in the rmarkdown document, if the 
environment exists. It's useful for people who have a strong need of 
generating Word documents and producing well-formatted LaTeX table at the same
time.

* For both HTML & LaTeX, added escape option for add_footnote

* Fixed a bug in LaTeX for the processing of the + sign

* Fixed a bug with the [ symbol in escape_regex



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
