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