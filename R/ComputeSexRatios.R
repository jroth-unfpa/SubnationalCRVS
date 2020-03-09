#' Compute sex ratios for adjacent five-year age groups
#'
#' asdf
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
#' ecuador_sex_ratios <- ComputeSexRatios(data=ecuador_ecuador_single_year_ages,
#'                                        name.disaggregations="province_name",
#'                                        name.males="m",
#'                                        name.females="f",
#'                                        name.age="age",
#'                                        name.sex="sex",
#'                                        name.date1="date1",
#'                                        name.date2="date2",
#'                                        name.population.year1="pop1",
#'                                        name.population.year2="pop2")
#' head(ecuador_sex_ratios)
#' tail(ecuador_sex_ratios)
#' @import dplyr
#' @export

ComputeSexRatios <- function(data, 
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
  if (length(unique(data[, name.date1])) != 1) {
    stop("date1 variable must contain only one unique value")
    date.1 <- data[1, name.date1]
  }
  if (length(unique(data[, name.date2])) != 1) {
    stop("date2 variable must contain only one unique value")
  date.2 <- data[1, name.date2]
  }

  # compute sex ratio within age groups and levels of disaggregation
  data_with_sex_ratio <- data %>% 
    group_by(get(name.age), get(name.disaggregations)) %>%
    mutate("sex_ratio_1"= 100 * 
             get(name.population.year1)[get(name.sex) == name.males] / 
             get(name.population.year1)[get(name.sex) == name.females],
           "sex_ratio_2"= 100 * 
             get(name.population.year2)[get(name.sex) == name.males] / 
             get(name.population.year2)[get(name.sex) == name.females]) %>%
    as.data.frame()
  data_with_sex_ratio[, "get(name.age)"] <- NULL
  data_with_sex_ratio[, "get(name.disaggregations)"] <- NULL
  
  # each age group only needs to be represented once (not separately for males and females)
  # but it might end up being convenient to have separate rows for males and females
  data_with_sex_ratio <- data_with_sex_ratio %>% filter(sex == "f")
  data_with_sex_ratio[, name.sex] <- NULL
  return(data_with_sex_ratio)
}