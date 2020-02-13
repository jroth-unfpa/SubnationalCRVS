#' Compute summaries of age heaping (wrapper for DemoTools::check_heaping_*)
#'
#' This function serves as a wrapper for five DemoTools functions -- 
#' check_heaping_roughness(), check_heaping_sawtooth(), check_heaping_whipple(), check_heaping_myers(), check_heaping_noumbissi() -- 
#' and returns the five summary statistics within specified levels of disaggregation in a given dataset 
#' (which must contain single-year age counts) separately for males and females.
#' @param data data frame that contains at least seven columns representing: (1) single-year age,
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
#' @param roughness.age.min=NULL Equivalent to the `ageMin` argument of `Demotools::check_heaping_roughness`. Defaults to NULL, which then uses the `DemoTools` default of 20
#' @param roughness.age.max=NULL Equivalent to the `ageMax` argument of `Demotools::check_heaping_roughness`. Defaults to NULL, which then uses the `DemoTools` default of the highest age that is a multiple of 10
#' @param sawtooth.age.min=NULL Equivalent to the `ageMin` argument of `Demotools::check_heaping_sawtooth`. Defaults to NULL, which then uses the `DemoTools` default of 40
#' @param sawtooth.age.max=NULL Equivalent to the `ageMax` argument of `Demotools::check_heaping_sawtooth`. Defaults to NULL, which then uses the `DemoTools` default of the highest age that is a multiple of 10
#' @param Whipple.age.min=NULL Equivalent to the `ageMin` argument of `Demotools::check_heaping_whipple`. Defaults to NULL, which then uses the `DemoTools` default of 25
#' @param Whipple.age.max=NULL Equivalent to the `ageMax` argument of `Demotools::check_heaping_whipple`. Defaults to NULL, which then uses the `DemoTools` default of 65
#' @param Whipple.digit=NULL Equivalent to the `digit` argument of `Demotools::check_heaping_whipple`. Defaults to NULL, which then uses the `DemoTools` default of c(0, 5)
#' @param Myers.age.min=NULL Equivalent to the `ageMin` argument of `Demotools::check_heaping_myers`. Defaults to NULL, which then uses the `DemoTools` default of 10
#' @param Myers.age.max=NULL Equivalent to the `ageMax` argument of `Demotools::check_heaping_myers`. Defaults to NULL, which then uses the `DemoTools` default of 89
#' @param Noumbissi.age.min=NULL Equivalent to the `ageMin` argument of `Demotools::check_heaping_noumbissi`. Defaults to NULL, which then uses the `DemoTools` default of 20
#' @param Noumbissi.age.max=NULL Equivalent to the `ageMax` argument of `Demotools::check_heaping_noumbissi`. Defaults to NULL, which then uses the `DemoTools` default of 64
#' @param Noumbissi.digit=NULL Equivalent to the `digit` argument of `Demotools::check_heaping_noumbissi`. Defaults to NULL, which then uses the `DemoTools` default of 0
#' @examples
#' ecuador_age_heaping_scores <- ComputeAgeHeapingScores(data=ecuador_age_tabulation,
#'                                               name.disaggregations="province",
#'                                               name.males="m",
#'                                               name.females="f",
#'                                               name.age="age",
#'                                               name.sex="sex",
#'                                               name.date1="date1",
#'                                               name.date2="date2",
#'                                               name.population.year1="pop1",
#'                                               name.population.year2="pop2")
#' head(ecuador_age_heaping_scores)
#' tail(ecuador_age_heaping_scores)
#' @import dplyr
#' @import DemoTools
#' @export

ComputeAgeHeapingScores <- function(data, 
                          name.disaggregations,
                          name.age,
                          name.sex,
                          name.males,
                          name.females,
                          name.date1,
                          name.date2,
                          name.population.year1,
                          name.population.year2,
                          roughness.age.min=NULL,
                          roughness.age.max=NULL,
                          sawtooth.age.min=NULL,
                          sawtooth.age.max=NULL,
                          Whipple.age.min=NULL,
                          Whipple.age.max=NULL,
                          Whipple.digit=NULL,
                          Myers.age.min=NULL,
                          Myers.age.max=NULL,
                          Noumbissi.age.min=NULL,
                          Noumbissi.age.max=NULL,
                          Noumbissi.digit=NULL) {
  # variable checks (should just call another function to do the checks that doesn't need to be documented)
  data[, name.disaggregations] <- as.factor(data[, name.disaggregations]) # should we requrie that the disaggregations are a factor variable with informative labels?
  if (length(unique(data[, name.date1])) != 1) {
    stop("date1 variable must contain only one unique value")
  }
  if (length(unique(data[, name.date2])) != 1) {
    stop("date2 variable must contain only one unique value")
  }
  date.1 <- data[1, name.date1]
  date.2 <- data[1, name.date2]
  
  # verify that the age variable is single-year ages and not groups of multiple ages (e.g. 5-year age groups)
  # and also emphasize that only the "deaths" column is actually not required
  print("need to add a way to check for single-year ages")
  
  # convert data into long format (more convenient for ggplot2)
  long_year1 <- data %>% 
    select(name.disaggregations,
           name.sex, 
           name.age,
           name.population.year1,
           name.date1) %>%
    rename(pop=pop1,
           date=date1)
  
  long_year2 <- data %>% 
    select(name.disaggregations,
           name.sex, 
           name.age,
           name.population.year2,
           name.date2) %>%
    rename(pop=pop2,
           date=date2)
  data_long <- rbind(long_year1, long_year2)
  
  # compute age heaping statistics based on data in long format
  data_with_age_heaping_long <- data_long %>%
                           group_by(date, get(name.sex), get(name.disaggregations)) %>% 
                           summarise("roughness"= 
                             myRoughness(Value=pop, ## missing values lead to an error here (just want to return NA, I think)
                                      Age=age,
                                      ageMin=roughness.age.min,
                                      ageMax=roughness.age.max),
                                      "sawtooth"=
                              mySawtooth(Value=pop, ## missing values lead to an error here (just want to return NA, I think)
                                      Age=age,
                                      ageMin=sawtooth.age.min,
                                      ageMax=sawtooth.age.max),
                                      "Whipple"=
                              myWhipple(Value=pop, ## missing values lead to an error here (just want to return NA, I think)
                                        Age=age,
                                        ageMin=Whipple.age.min,
                                        ageMax=Whipple.age.max,
                                        digit=Whipple.digit),
                                    "Myers"=
                              myMyers(Value=pop,
                                      Age=age,
                                      ageMin=Myers.age.min,
                                      ageMax=Myers.age.max),
                             "Noumbissi"=
                               myNoumbissi(Value=pop,
                                       Age=age,
                                       ageMin=Noumbissi.age.min,
                                       ageMax=Noumbissi.age.max,
                                       digit=Noumbissi.digit)) %>%
                              as.data.frame()
  names(data_with_age_heaping_long)[names(data_with_age_heaping_long) == "get(name.sex)"] = name.sex
  names(data_with_age_heaping_long)[names(data_with_age_heaping_long) == "get(name.disaggregations)"] = name.disaggregations

  return(data_with_age_heaping_long)
}
