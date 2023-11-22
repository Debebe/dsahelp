# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

hello <- function() {
  print("Hello, world!")
}


#' Title
#'
#' @param data data.frame for tabble creation
#' @import ftExtra, officer, flextable
#' @return
#' @export
#'
#' @examples
tabling <- function(data){
  comb <- as_grouped_data(x = data, groups = c("Category"))
  ft <- comb%>%
    as_flextable(hide_grouplabel = TRUE)%>%
    padding(padding = 0, part = "all") %>%
    separate_header()%>%
    span_header()%>%
    autofit()
  # bolding the grouping variable
  ft <- ft%>%
    bold(j = 1, i = ~ !is.na(Category), bold = TRUE, part = "body")
  ft <- align(ft, i = 1, NULL, align = "center", part = "header")

  ft <- padding(ft, padding.top = 0, part = "header")
  ft <- padding(ft, padding.bottom = 0, part = "header")
  ft <- bold(ft, bold = TRUE, part = "header")
  ft <- font(ft, fontname = "Arial", part = "header")
  ft <- fontsize(ft, i = NULL, j = NULL, size = 9, part = "body")

  # Adding footer
  ft <-add_footer_lines(ft, "") %>%
    font(part = "footer", fontname = "Arial") %>%
    bold(bold = FALSE, part = "footer")
  # adding font size for footer
  ft <-fontsize(ft, i = NULL, j = NULL, size = 7, part = "footer")
  set_table_properties(ft,layout = "autofit")
}
