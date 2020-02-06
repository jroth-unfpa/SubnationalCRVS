#' @import dplyr
#' @import DemoTools

FormatVariablesDDM <- function(data, 
                              name.disaggregations,
                              name.age,
                              name.sex,
                              name.males,
                              name.females,
                              name.date1,
                              name.date2,
                              name.population.year1,
                              name.population.year2,
                              name.deaths) {
  # re-naming varaibles to match requirements of ddm()
  data_for_ddm <- data %>% select(cod=name.disaggregations,
                                 pop1=name.population.year1,
                                 pop2=name.population.year2,
                                 deaths=name.deaths,
                                 date1=name.date1,
                                 date2=name.date2,
                                 age=name.age,
                                 sex=name.sex)
  data_for_ddm_males <- data_for_ddm %>% filter(sex == name.males)
  data_for_ddm_females <- data_for_ddm %>% filter(sex == name.females)
  return(list("data_for_ddm"=data_for_ddm,
              "data_for_ddm_males"=data_for_ddm_males,
              "data_for_ddm_females"=data_for_ddm_females))
}

FormatOutputDDM <- function(result_ddm_females,
                            result_ddm_males) {
  result_ddm_females$sex <- "Females"
  result_ddm_males$sex <- "Males"
  result_ddm_combined_sexes <- as.data.frame(rbind(result_ddm_females,
                                                   result_ddm_males))
  result_ddm_combined_sexes_formatted <- 
    result_ddm_combined_sexes %>%
    select(cod, 
           sex, 
           ggbseg, # maybe only display ggb-seg?
           ggb,
           seg,
           lower_age_range=lower,
           upper_age_range=upper) %>%
    arrange(cod, sex)
  return(result_ddm_combined_sexes_formatted)
}


## DemoTools::check_heaping_roughness modified to just return NA whenever there are any missing population counts
## rewrite as one function eith 4 options (argument: stat ="five_year", etc.) so code isn't dup;licated
myRoughness <- function(Value, 
                           Age,
                           ageMin,
                           ageMax) {
  if (sum(is.na(Value)) != 0) {
    result <- NA 
  } else {
     if (is.null(ageMin)) {
      ageMin <- 20  ## default from DemoTools::five_year_roughness()
     } 
     if (is.null(ageMax)) {
      ageMax <-  max(Age[Age%%5 == 0]) ## default from DemoTools::five_year_roughness()
     }
     result <- check_heaping_roughness(Value=Value,
                         Age=Age,
                         ageMin=ageMin,
                         ageMax=ageMax)
  }
  return(result)
}

mySawtooth <- function(Value, 
                       Age,
                       ageMin,
                       ageMax) {
  if (sum(is.na(Value)) != 0) {
    result <- NA 
  } else {
    if (is.null(ageMin)) {
      ageMin <- 40  ## default from DemoTools::check_heaping_sawtooth()
    } 
    if (is.null(ageMax)) {
      ageMax = max(Age[Age%%5== 0]) - 10 ## default from DemoTools::check_heaping_sawtooth()
    }
    result <- check_heaping_sawtooth(Value=Value,
                                  Age=Age,
                                  ageMin=ageMin,
                                  ageMax=ageMax)
  }
  return(result)
}

myWhipple <- function(Value, 
                      Age,
                      ageMin,
                      ageMax,
                      digit) {
  if (is.null(digit)) {
    digit <- c(0, 5) 
  }
  if (sum(is.na(Value)) != 0) {
    result <- NA 
  } else {
    if (is.null(ageMin)) {
      ageMin <- 25  ## default from DemoTools::check_heaping_whipple()
    } 
    if (is.null(ageMax)) {
      ageMax <- 65 ## default from DemoTools::check_heaping_whipple()
    }
    result <- check_heaping_whipple(Value=Value,
                                    Age=Age,
                                    ageMin=ageMin,
                                    ageMax=ageMax,
                                    digit=digit)
  }
  return(result)
}

myMyers <- function(Value, 
                    Age,
                    ageMin,
                    ageMax) {
  if (sum(is.na(Value)) != 0) {
    result <- NA 
  } else {
    if (is.null(ageMin)) {
      ageMin <- 10  ## default from DemoTools::check_heaping_myers()
    } 
    if (is.null(ageMax)) {
      ageMax <-  max(Age) ## default from DemoTools::check_heaping_myers()
    }
    result <- check_heaping_myers(Value=Value,
                                  Age=Age,
                                  ageMin=ageMin,
                                  ageMax=ageMax)
  }
  return(result)
}

myNoumbissi <- function(Value, 
                      Age,
                      ageMin,
                      ageMax,
                      digit) {
  if (is.null(digit)) {
    digit <- 0
  }
  if (sum(is.na(Value)) != 0) {
    result <- NA 
  } else {
    if (is.null(ageMin)) {
      ageMin <- 20  ## default from DemoTools::check_heaping_noumbissi()
    } 
    if (is.null(ageMax)) {
      ageMax <- 64 ## default from DemoTools::check_heaping_noumbissi()
    }
    result <- check_heaping_noumbissi(Value=Value,
                                      Age=Age,
                                      ageMin=ageMin,
                                      ageMax=ageMax,
                                      digit=digit)
  }
  return(result)
}

GetOneAgeRatio <- function(vec_ages,
                           vec_counts) {
  n_age_groups <- length(vec_ages)
  vec_age_ratios <- rep(NA, n_age_groups)
  for (v in 1:n_age_groups) {
    one_age <- vec_ages[v]
    one_idx <- which(vec_ages == one_age)
    if (length(one_idx) != 0) {
      if (one_idx != 1 & one_idx != n_age_groups) {
        vec_age_ratios[one_idx] <- 2 * vec_counts[one_idx] / 
          (vec_counts[(one_idx - 1)] + vec_counts[(one_idx + 1)])
      } else {
        vec_age_ratios[one_idx] <- NA
      }
    } else {
      vec_age_ratios[one_idx] <- NA
    }
  }
  return(100*vec_age_ratios)
}
