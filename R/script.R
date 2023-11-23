
#' Title Create publication quality tables
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
  ft <- align(ft, j=-1,  align = "center", part = "header")
  ft <- align(ft, j=-1, align = "center", part = "body")

  ft <- padding(ft, padding.top = 0, part = "header")
  ft <- padding(ft, padding.bottom = 0, part = "header")
  ft <- bold(ft, bold = TRUE, part = "header")
  ft <- font(ft, fontname = "Arial", part = "header")
  ft <- fontsize(ft, i = NULL, j = NULL, size = 9, part = "all")

  # Adding footer
  ft <-add_footer_lines(ft, "NB") %>%
    font(part = "footer", fontname = "Arial") %>%
    bold(bold = FALSE, part = "footer")
  # adding font size for footer
  ft <-fontsize(ft, i = NULL, j = NULL, size = 7, part = "footer")
  set_table_properties(ft,layout = "autofit")
}


#' Title Cleans variable names from regression outputs
#'
#' @param tab  data.frame with messy variable names and regression coefficients
#'
#' @return data.frame with clean variable names and coefficients
#' @export
#'
#' @examples
clean_regression_variables <- function(tab) {
  tab <- tab %>%
    # separate variables separated by "`"
    tidyr::separate(rn, into=c('old_category', 'Category',"Variable"), sep='`')%>%
    #create white space between lowercase followed by Uppercase,by number
    mutate(old_category_spaced = gsub("(?<=[a-z])(?=[A-Z0-9])", " ", old_category, perl = TRUE)) %>%
    # separate at white space
    tidyr::extract(old_category_spaced, into = c("Category", "Variable"), regex = "^(\\w+)\\s?(.*)$")%>%
    mutate(Category=ifelse(grepl("Intercept", old_category), "Intercept", Category),
           Variable=ifelse(grepl("Intercept", old_category), "Intercept", Variable))%>%
    dplyr::select(-c("old_category"))

  return(tab)
}

#' Title
#'
#' @param glm_object glm object as outputed from glm call
#'
#' @return Data frame with clean variable names and coefficients
#' @export
#'
#' @examples
clean_glm_outputs <- function(glm_object){

  # Odds ratios of regression coefficients
  out<- exp(cbind(coef(glm_object), confint.default(glm_object)))
  out <-out%>%
    as.data.frame()
  out <-setDT(out, keep.rownames = TRUE)[]
  out <- out%>%
    clean_regression_variables()
}


#' Title Binds mean and 95% CI
#'
#' @param V1 Mean regression coefficient
#' @param ci_lower Lower CI bound
#' @param ci_upper upper confidence interval bound
#'
#' @return Data frame with pasted coefficients
#' @export
#'
#' @examples
bind_conf_interval <- function(V1, ci_lower, ci_upper) {
  return(paste(round(V1, 2), "(", round(ci_lower, 2), ",", round(ci_upper, 2), ")", sep = ""))
}

#' Title Runs crude odds ratios for each predictor variable in one execution
#'
#' @param data  Data.frame containing response variable and all predictor variables
#' @param response_var Response variable for glm regression
#' @param predictor_vars Predictor variables for glm variables
#'
#' @return  data.frame with cude regression coefficients for all predictor variables involved
#' @export
#'
#' @examples
calculate_crude_OR <- function(data, response_var, predictor_vars) {
  output <- lapply(predictor_vars, function(var) {
    formula_text <- ifelse(grepl("\\s", var), paste(response_var, "~", "`", var, "`", sep = ""),
                           paste(response_var, "~", var))
    formula <- as.formula(formula_text)
    out <- glm(formula, family = binomial(link = logit), data = data)
    tab <- exp(cbind(coef(out), confint.default(out)))
    tab <- as.data.frame(tab)
    setDT(tab, keep.rownames = TRUE)

    xlevels=out$xlevels
  })

  crude_OR <- do.call("rbind", output)
  return(crude_OR)
}

#' Title This prepares regression outputs
#'
#' @param glm_object This is  glm or lm class as outputed by glm
#'
#' @return Clean data.frame with for table
#' @export
#'
#' @examples
prepare_adjusted_regression <-function(glm_object){

  out<- exp(cbind(coef(glm_object), confint.default(glm_object)))
  out <-out%>%
    as.data.frame()
  out <-setDT(out, keep.rownames = TRUE)[]

  # Extracts variable category and levels
  glm_vars <-glm_object$xlevels
  # capture in a data frame
  df <- data.frame(
    Category = rep(names(glm_vars), lengths(glm_vars)),
    Variable = unlist(glm_vars))
  # clean variable names in the summary table using xlevels variables
  reg_summary <- out%>%
    mutate(Category= str_extract(rn, paste(df$Category, collapse = "|")),
           Variable= str_extract(rn, paste(df$Variable, collapse = "|")),
           Category= ifelse(grepl("Intercept", rn), "Intercept", Category),
           Variable= ifelse(grepl("Intercept", rn), "Intercept", Variable))%>%
    select(-rn)
  # merge xlevels variables with summary table
  reg_out <-full_join(df, reg_summary)%>%
    mutate_if(is.numeric, round, 2)%>%
    mutate(Estimate = paste(V1, "(", `2.5 %`, ", ", `97.5 %`, ")", sep = ""))%>%
    mutate(Estimate=ifelse(grepl("NA", Estimate), "1.00", Estimate))%>%
    dplyr::select(Category, Variable, Estimate)%>%
    mutate(id=case_when(Category=="Intercept" ~1,
                        Category=="Sex" ~2,
                        Category=="Age" ~ 3,
                        Category=="Race"~4,
                        Category=="Wealth"~5,
                        Category=="Residence"~6
    ))%>%
    arrange(id, Category)%>% dplyr::select(-id)
return(reg_out)

}


#' Title This fits glm and outputs Crude OR for for each variable
#'
#' @param data Data.frame containing response variable and predictors
#' @param response_var
#' @param predictor_vars
#'
#' @return Data.frame containing processed coeffcients and variables
#' @export
#'
#' @examples
prepare_crude_regression <- function(data, response_var, predictor_vars) {
  output <- lapply(predictor_vars, function(var) {
    formula_text <- ifelse(grepl("\\s", var), paste(response_var, "~", "`", var, "`", sep = ""),
                           paste(response_var, "~", var))
    formula <- as.formula(formula_text)
    out <- glm(formula, family = binomial(link = logit), data = data)
    tab <- exp(cbind(coef(out), confint.default(out)))
    tab <- as.data.frame(tab)
    setDT(tab, keep.rownames = TRUE)

    glm_vars <-out$xlevels
    # capture in a data frame
    df <- data.frame(
      Category = rep(names(glm_vars), lengths(glm_vars)),
      Variable = unlist(glm_vars))
    # clean variable names in the summary table using xlevels variables
    tab <- tab%>%
      mutate(Category= str_extract(rn, paste(df$Category, collapse = "|")),
             Variable= str_extract(rn, paste(df$Variable, collapse = "|")),
             Category= ifelse(grepl("Intercept", rn), "Intercept", Category),
             Variable= ifelse(grepl("Intercept", rn), "Intercept", Variable))%>%
      select(-rn)%>%
      full_join(df)
    return(tab)
  })

  crude_OR <- do.call("rbind", output)%>%
    distinct(Variable, .keep_all = TRUE)%>%
    mutate_if(is.numeric, round, 2)%>%
    mutate(Estimate = paste(V1, "(", `2.5 %`, ", ", `97.5 %`, ")", sep = ""))%>%
    mutate(Estimate=ifelse(grepl("NA", Estimate), "1.00", Estimate))%>%
    dplyr::select(Category, Variable, Estimate)%>%
    # if the following are in the output, order as follows
    mutate(id=case_when(Category=="Intercept" ~1,
                        Category=="Sex" ~2,
                        Category=="Age" ~ 3,
                        Category=="Race"~4,
                        Category=="Wealth"~5,
                        Category=="Residence"~6))%>%
    arrange(id, Category)%>% dplyr::select(-id)
  return(crude_OR)
}

