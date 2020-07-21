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
#' @param name.population.year1 Character string providing the name of the variable in `data` that represents the population count in the earlier time period
#' @param name.population.year2 Character string providing the name of the variable in `data` that represents the population count in the later time period
#' @param name.year1 Character string providing the name of the variable in `data` that represents the year of the earlier of the two time periods (e.g. year of the earlier Census)
#' @param name.month1 Character string providing the name of the variable in `data` that represents the month of the earlier of the two time periods (e.g. month of the earlier Census)
#' @param name.day1 Character string providing the name of the variable in `data` that represents the day of the earlier of the two time periods (e.g. day of the earlier Census)
#' @param name.year2 Character string providing the name of the variable in `data` that represents the year of the later of the two time periods (e.g. year of the later Census)
#' @param name.month2 Character string providing the name of the variable in `data` that represents the month of the later of the two time periods (e.g. month of the later Census)
#' @param name.day2 Character string providing the name of the variable in `data` that represents the day of the later of the two time periods (e.g. day of the later Census)
#' 
#' @examples
#' ecuador_age_ratios <- ComputeAgeRatios(data=ecuador_five_year_ages,
#'                                        name.disaggregations="province_name",
#'                                        name.age="age",
#'                                        name.sex="sex",
#'                                        name.males="m",
#'                                        name.females="f",
#'                                        name.population.year1="pop1",
#'                                        name.population.year2="pop2",
#'                                        name.year1="year1"
#'                                        name.month1="month1",
#'                                        name.day1="day1",
#'                                        name.year2="year2",
#'                                        name.month2="month2",
#'                                        name.day2="day2")
#' head(ecuador_age_ratios)
#' tail(ecuador_age_ratios)
#' @import dplyr
#' @export

ComputeAgeRatios <- function(data, 
                             name.disaggregations,
                             name.age,
                             name.sex,
                             name.males,
                             name.females,
                             name.population.year1,
                             name.population.year2,
                             name.year1,
                             name.month1,
                             name.day1,
                             name.year2,
                             name.month2,
                             name.day2) {
  if (!is.data.frame(data)) {
    stop("the dataset provided in the 'data' argument needs to be a data frame")
  }
  data <- arrange(data, get(name.disaggregations), get(name.sex), get(name.age)) #it's important that the age variable is sorted
  data <- CreateDateVariable(data=data,
                           name.disaggregations=name.disaggregations,
                           name.year1=name.year1,
                           name.month1=name.month1,
                           name.day1=name.day1,
                           name.year2=name.year2,
                           name.month2=name.month2,
                           name.day2=name.day2)
  
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
  data_with_age_ratio[, c("year1", "month1", "day1")] <- NULL
  data_with_age_ratio[, c("year2", "month2", "day2")] <- NULL
  return(data_with_age_ratio)
}
