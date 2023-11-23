
#' Title Create publication quality table
#' @param data Data.frame for table creation
#' @import ftExtra, officer, flextable
#' @return
#' @export
#' @examples
create_table <- function(data){
  comb <- as_grouped_data(x = data, groups = c("Category"))
  ft <- comb%>%
    as_flextable(hide_grouplabel = TRUE)%>%
    padding(padding = 0, part = "all") %>%
    #separate_header()%>%
    ftExtra::split_header()%>%
    ftExtra::span_header()%>%
    flextable::hline(i = 1, part = 'header')%>%
    autofit()
  # bolding the grouping variable
  ft <- ft%>%
    bold(j = 1, i = ~ !is.na(Category), bold = TRUE, part = "body")

  ft <- align(ft, i = 1, j=-1,  align = "center", part = "header")
  ft <- align(ft, i = 2, j=-1, align = "center", part = "header")
  ft <- align(ft, j = -1, align = "center", part = "body")

  ft <- padding(ft, padding.top = 0, part = "header")
  ft <- padding(ft, padding.bottom = 0, part = "header")
  ft <- bold(ft, bold = TRUE, part = "header")
  ft <- font(ft, fontname = "Arial", part = "header")
  ft <- fontsize(ft, i = NULL, j = NULL, size = 9, part = "body")
  ft <- fontsize(ft, i = NULL, j = NULL, size = 9, part = "header")

  # Adding footer
  ft <-add_footer_lines(ft, "") %>%
    font(part = "footer", fontname = "Arial") %>%
    bold(bold = FALSE, part = "footer")
  # adding font size for footer
  ft <-fontsize(ft, i = NULL, j = NULL, size = 7, part = "footer")
  set_table_properties(ft,layout = "autofit")
}
