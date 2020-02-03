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


## DemoTools::five_year_roughness modified to just return NA whenever there are any missing population counts
## rewrite as one function eith 4 options (argument: stat ="five_year", etc.) so code isn't dup;licated
myFiveYearRoughness <- function(Value, 
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
     result <- five_year_roughness(Value=Value,
                         Age=Age,
                         ageMin=ageMin,
                         ageMax=ageMax)
  }
  return(result)
}

myZeroPrefSawtooth <- function(Value, 
                                Age,
                                ageMin,
                                ageMax) {
  if (sum(is.na(Value)) != 0) {
    result <- NA 
  } else {
    if (is.null(ageMin)) {
      ageMin <- 40  ## default from DemoTools::zero_pref_sawtooth()
    } 
    if (is.null(ageMax)) {
      ageMax = max(Age[Age%%5== 0]) - 10 ## default from DemoTools::zero_pref_sawtooth()
    }
    result <- zero_pref_sawtooth(Value=Value,
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
      ageMin <- 25  ## default from DemoTools::Whipple()
    } 
    if (is.null(ageMax)) {
      ageMax <- 65 ## default from DemoTools::Whipple()
    }
    result <- Whipple(Value=Value,
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
      ageMin <- 10  ## default from DemoTools::Myers()
    } 
    if (is.null(ageMax)) {
      ageMax <-  max(Age) ## default from DemoTools::Myers()
    }
    result <- Myers(Value=Value,
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
      ageMin <- 20  ## default from DemoTools::Noumbissi()
    } 
    if (is.null(ageMax)) {
      ageMax <- 64 ## default from DemoTools::Noumbissi()
    }
    result <- Noumbissi(Value=Value,
                        Age=Age,
                        ageMin=ageMin,
                        ageMax=ageMax,
                        digit=digit)
  }
  return(result)
}

