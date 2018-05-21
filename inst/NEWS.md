kableExtra 0.9.0
--------------------------------------------------------------------------------
* Major Change: Now when you load `kableExtra` package, it will automatically 
set format for `kable()` based on the working environment: only if you are using
rmarkdown/r sweave to render PDFs, the default format will be set to "latex". 
Otherwise it will be set to "html".

* Major Change: Now `kableExtra` will try to load the table section of the 
bootstrap stylesheet when you are rendering slidy_presentation & 
ioslides_presentation. You can also choose to load it manually via the
global option `kableExtra.html.bsTable`.

* Added support to the `tables` package. (Thank you @dmurdoch)

* Added a save_kable function to save HTML table as independent HTML pages. 

* Added min_width and max_width to column_spec.

* Added documentation about 100% width in scroll_box (Thank you @isteves!)

* Added `include_thead` to `column_spec`. (#177)

* Fixed a few bugs in `linebreak` (#167, #180)

* Fixed a bug when there is no column header row in LaTeX (Thank you @leovan)

* Fixed header formatting for repeated header row in latex longtable (#183)

* Removed the \small tag in threeparttable (#162)

* Added valign to collapse_rows to adjust vertical position of the contents. 
Change default setting from middle to top. (#191)

* Added an auto_index function to facilitate group_rows. (#193) 

* Added a title_format option to footnote and changed default format from 
bold to italic. (#200)

kableExtra 0.8.0
--------------------------------------------------------------------------------
* Now kableExtra imports & exports knitr::kable so users don't need to load knitr entirely to NAMESPACE when it's not necessary, for example, in shiny. 

* Fixed #115, a bug in latex footnote about escaping.

* Fixed #116, a bug about the position of footnote in longtable with booktabs

* Fixed #119, a bug between collapse_rows and add_header_above

* Improve footnotes in longtable and replace threeparttable with threeparttablex. Now footnotes works smoothly in longtable, even with full width in tabu.

* Fixed #135, a bug in footnote_marker_symbol

* Added a new layout for collapse_rows, thanks @georgegui! #159

* Added the linebreak function to support adding linebreaks to LaTeX table. 

* Added internal support to convert \n to linebreak in table to all kableExtra functions for both LaTeX and HTML

* Added a document about wrapping texts and adding linebreaks. 

* Fixed a bug related to the handling of special characters

* Remove HTML to Word doc from vignette because it's too large. 

* Fixed varies other bugs. 


kableExtra 0.7.0
--------------------------------------------------------------------------------
* Now HTML table will be previewed in Viewer panel

* Added HTML color code support to kableExtra

* Added footnote as a more flexible replacement for add_footnote. `Add_footnote()` will be kept maintained for a while. 

* Fixed bug #105: bold/italic/monospace cannot accept T/F as vector

* Added extra_css to cell_spec, row_spec & column_spec

* Fixed bug #88: add_footnote doesn't support full width

* Added hline_after and extra_latex_after to row_spec #101

* Improved warning message for kables not in `html` or `latex`

* Added latex_hline to collapse_rows so users can choose from full, major or none

* Added strikeout and underline to column_spec, row_spec and cell_spec. 

* Added extra_css to column_spec, row_spec and cell_spec.

* Added a vignette about how to copy tables from HTML to Word. 

* Change some read_xml to read_html

* Added scale_from to some spec_tools

* Quite a few minor bug fixes


kableExtra 0.6.1
--------------------------------------------------------------------------------
* Fixed a bug in column_spec width introduced in ver 0.6.0

* Fixed a bug in add_header_above. #90


kableExtra 0.6.0
--------------------------------------------------------------------------------
* Added cell_spec for HTML & LaTeX

* Added helper functions including spec_color, spec_font_size, spec_angle, 
spec_tooltip & spec_popover to facilitate the cell_spec function.

* Added align/rotate/font_size to row_spec

* Allowed row_spec to modify header row with row_spec(0, ...)

* Improved `repeat_header` in LaTeX for longtables. Now you can write texts like
"continued on next page"

* Fixed a critical bug with latex group_rows (#68)

* Escape question mark ? in regex_escape (thanks @nichtleiter)

* Switched from readr::write_file to base::writeLines (for support in earlier 
version of readr)

* Switched from sub to str_replace in some places due to a bug in sub on 
Windows.



kableExtra 0.5.2
--------------------------------------------------------------------------------
* Request from CRAN: Changed dependency on `magick` from Imports to Suggest. 
Added error message when users don't have magick installed. 

* Added argument `index` to `group_rows` so users can build multiple row groups
in one step. The syntax is the same with `add_header_above`

* Now `row_spec` and `column_spec` can take vectors so users can customized 
multiple row/columns at the same time. 

* Fixed a bug for `kable_as_image` on Windows. Improved documentations and 
error messages. 

kableExtra 0.5.1
--------------------------------------------------------------------------------
* Added documentations about those color/background color options in column_spec 
and row_spec

* Added the kableExtra_latex_packages function to quickly print all necessary 
LaTeX packages on the screen. 

* Hot fix a bug in full_width in LaTeX

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
