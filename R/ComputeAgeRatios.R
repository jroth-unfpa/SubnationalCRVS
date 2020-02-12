#' Compute age-ratios for adjacent five-year age groups
#'
#' This function computes an age ratio, defined as
#' @details
#' \deqn{\frac{1}{2} x^2_n}
#' where , for adjacent age groups
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
#' @examples
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
