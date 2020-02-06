#' Compute summary statistics for single-year age heaping
#'
#' asdf
#' 
#' @param data asdf
#' @param name.disaggregations asdf
#' @param name.age asdf
#' @param name.sex asdf
#' @param name.males asdf
#' @param name.females asdf
#' @param name.date1 asdf
#' @param name.date2 asdf
#' @param name.population.year1 asdf
#' @param name.population.year2 asdf
#' @param roughness.age.min=NULL asdf
#' @param roughness.age.max=NULL asdf
#' @param sawtooth.age.min=NULL asdf
#' @param sawtooth.age.max=NULL asdf
#' @param Whipple.age.min=NULL asdf
#' @param Whipple.age.max=NULL asdf
#' @param Whipple.digit=NULL asdf
#' @param Myers.age.min=NULL asdf
#' @param Myers.age.max=NULL asdf
#' @param Noumbissi.age.min=NULL asdf
#' @param Noumbissi.age.max=NULL asdf
#' @param Noumbissi.digit=NULL asdf
#' @examples
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
    rename(sex=name.sex,
           pop=pop1,
           date=date1)
  
  long_year2 <- data %>% 
    select(name.disaggregations,
           name.sex, 
           name.age,
           name.population.year2,
           name.date2) %>%
    rename(sex=name.sex,
           pop=pop2,
           date=date2)
  data_long <- rbind(long_year1, long_year2)
  
  # compute age heaping statistics based on data in long format
  data_with_age_heaping_long <- data_long %>%
                           group_by(date, sex, province) %>% 
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
  return(data_with_age_heaping_long)
}
