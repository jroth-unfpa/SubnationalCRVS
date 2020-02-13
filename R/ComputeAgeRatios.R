#' Compute age-ratios for adjacent five-year age groups
#'
#' This function computes an age ratio, defined as asdf
#' 
#' @param data data frame that contains at least seven columns representing: (1) five-year age groups, 
#' (2) sex,
#' (3, 4) population counts collected at two different time points (typically adjacent Census years)
#' (5, 6) dates of two different time points
#' (7) the level of subnational disaggregation in additino to sex (e.g. a geographic unit such as a province/state, 
#' a sociodemographic category such as education level, or combinations thereof). 
#' @param name.disaggregations Character string providing the name of the variable in `data` that represents the levels of subnational disaggregation
#' @param name.age Character string providing the name of the variable in `data` that represents age
#' @param name.sex Character string providing the name of the variable in `data` that represents sex
#' @param name.males Character string providing the name of the value of `name.sex` variable that represents males
#' @param name.females Character string providing the name of the value of `name.sex` variable that represents females
#' @param name.date1 Character string providing the name of the variable in `data` that represents the earlier time period
#' @param name.date2 Character string providing the name of the variable in `data` that represents the later time period
#' @param name.population.year1 Character string providing the name of the variable in `data` that represents the population count in the earlier time period
#' @param name.population.year2 Character string providing the name of the variable in `data` that represents the population count in the later time period
#' @examples
#' ecuador_age_ratios <- ComputeAgeRatios(data=example_data_ecuador,
#'                                        name.disaggregations="province_name_short",
#'                                        name.age="age",
#'                                        name.sex="sex",
#'                                        name.males="m",
#'                                        name.females="f",
#'                                        name.population.year1="pop1",
#'                                        name.population.year2="pop2",
#'                                        name.date1="date1",
#'                                        name.date2="date2")
#' head(ecuador_age_ratios)
#' tail(ecuador_age_ratios)
#' @import dplyr
#' @export
#' @md

ComputeAgeRatios <- function(data, 
                             name.disaggregations,
                             name.age,
                             name.sex,
                             name.males,
                             name.females,
                             name.date1,
                             name.date2,
                             name.population.year1,
                             name.population.year2) {
  # variable checks (should just call another function to do the checks that doesn't need to be documented)
  data <- arrange(data, get(name.disaggregations), get(name.sex), get(name.age)) #it's important that the age variable is sorted
  if (length(unique(data[, name.date1])) != 1) {
    stop("date1 variable must contain only one unique value")
    date.1 <- data[1, name.date1]
  }
  if (length(unique(data[, name.date2])) != 1) {
    stop("date2 variable must contain only one unique value")
    date.2 <- data[1, name.date2]
  }
  
  if(class(data[, name.age]) %in% c("integer", "numeric") == FALSE) {
    stop("age variable must be integer or numeric") 
  }
  # compute age_ratio within sex and within levels of disaggregation
  data_with_age_ratio <- data %>% group_by(get(name.disaggregations), 
                                           get(name.sex)) %>%
    mutate("age_ratio_1"=GetOneAgeRatio(vec_ages=get(name.age),
                                        vec_counts=get(name.population.year1)),
           "age_ratio_2"=GetOneAgeRatio(vec_ages=get(name.age),
                                        vec_counts=get(name.population.year2))) %>%
    as.data.frame()
  
  data_with_age_ratio[, "get(name.age)"] <- NULL
  data_with_age_ratio[, "get(name.sex)"] <- NULL
  data_with_age_ratio[, "get(name.disaggregations)"] <- NULL
  return(data_with_age_ratio)
}
