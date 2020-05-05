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

FormatOutputGGBSEG <- function(result_ggbseg_females,
                               result_ggbseg_males) {
  result_ggbseg_females$sex <- "Females"
  result_ggbseg_males$sex <- "Males"
  result_ggbseg_combined_sexes <- as.data.frame(rbind(result_ggbseg_females,
                                                      result_ggbseg_males))
  result_ggbseg_combined_sexes_formatted <- 
    result_ggbseg_combined_sexes %>%
    select(cod, 
           sex, 
           ggbseg=coverage,
           lower_age_range=lower,
           upper_age_range=upper) %>%
    arrange(cod, sex)
  result_ggbseg_combined_sexes_formatted$ggbseg <- signif(result_ggbseg_combined_sexes_formatted$ggbseg, 
                                                         2)
  return(result_ggbseg_combined_sexes_formatted)
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
  result_ddm_combined_sexes_formatted$ggbseg <- signif(result_ddm_combined_sexes_formatted$ggbseg, 2)
  result_ddm_combined_sexes_formatted$ggb <- signif(result_ddm_combined_sexes_formatted$ggb, 2)
  result_ddm_combined_sexes_formatted$seg <- signif(result_ddm_combined_sexes_formatted$seg, 2)
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
  return(round(result, 2))
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
  return(round(result, 2))
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
  return(round(result, 2))
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
  return(round(result, 2))
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
  return(round(100*vec_age_ratios, 1))
}

CheckSingleYearAges <- function(data,
                                name.disaggregations,
                                name.sex,
                                confirm_single_year_ages) {
   data_with_count_groups <- data %>%
    group_by(get(name.disaggregations), get(name.sex)) %>%
    summarise("count"=n())
   if (max(data_with_count_groups$count, na.rm=TRUE) < 30) {
    if ((23 %in% data[, name.age] == FALSE) & (37 %in% data[name.age] == FALSE)) {
      if (confirm_single_year_ages == FALSE) {
        stop("It looks like your age variable may not represent single-year ages, so we stopped the function from executing. 
             To force the function run anyway, please manually set the argument have_single_year_ages=TRUE.")
      } else {
        warning("Function continuing to run because confirm_single_year_ages=TRUE")
      }
    }
  } 
}


CallggbgetRMS <- function(my.ddm.data, 
                          age.range,
                          min.age.in.search,
                          max.age.in.search,
                          deaths.summed) {
  unique_cod <- unique(my.ddm.data$cod)
  unique_sex <- unique(my.ddm.data$sex)
  if (length(unique_sex) != 1) {
    stop("only data from one sex should be provided to CallggbgetRMS() function") 
  }
  n_cod <- length(unique_cod)
  df_cod <- as.data.frame(matrix(NA, nrow=n_cod, ncol=2))
  names(df_cod) <- c("cod", "RMSE")
  for (i in 1:n_cod) {
    one_ddm_data <- my.ddm.data[my.ddm.data$cod == unique_cod[i], ]
    one_codi <- ggbMakeColumns(codi=one_ddm_data, 
                           minA=min.age.in.search, 
                           maxA=max.age.in.search,
                           deaths.summed=deaths.summed)
    df_cod[i, "cod"] <- unique_cod[i]
    df_cod[i, "RMSE"] <- signif(ggbgetRMS(agesi=age.range,
                                         codi=one_codi),
                               4)
    df_cod[i, ]
  }
  df_cod$cod <- as.factor(df_cod$cod)
  return(df_cod)
}


MakeOneSensitivityPlot <- function(sensitivity.estimates,
                                   point.estimates,
                                   output.type,
                                   one.sex,
                                   one.level,
                                   label.completeness,
                                   label.RMSE,
                                   base.size.sensitivity) {
  stopifnot(output.type %in% c("ggbseg","RMSE"))
  stopifnot(one.sex %in% c("Females", "Males"))
  
  # set up y-axis
  sensitivity_estimates_one <- sensitivity.estimates[sensitivity.estimates$cod == one.level &
                                                     sensitivity.estimates$sex == one.sex, 
                                                     ]
  one_ylim <- c(0.9 * min(sensitivity_estimates_one[, output.type],
                          na.rm=TRUE),
                1.1 * max(sensitivity_estimates_one[, output.type], 
                          na.rm=TRUE))
  mean_outcome <- as.numeric(unique(sensitivity_estimates_one[, paste0("mean_", output.type)]))
  sd_outcome <- as.numeric(unique(sensitivity_estimates_one[, paste0("sd_", output.type)]))
  stopifnot(length(mean_outcome) == 1 & length(sd_outcome) == 1)
  if (output.type == "ggbseg") {
    y_label <- label.completeness
    y_title <- paste0("Estimated completeness in\n",
                      one.level,
                      "--",
                      one.sex,
                      "\n",
                      "(Sample Mean: ",
                      mean_outcome,
                      ", Sample SD: ",
                      sd_outcome, 
                      ")")

  } else if (output.type == "RMSE") {
    y_label <- label.RMSE
    y_title <- paste0("RMSE for age-range selection in\n",
                      one.level,
                      "--",
                      one.sex,
                      "\n",
                      "(Sample Mean: ",
                      mean_outcome,
                      ", Sample SD: ",
                      sd_outcome, 
                      ")")
  }
  g_sensitivity <- ggplot(data=sensitivity.estimates %>%
                               filter(cod == one.level & 
                               sex == one.sex),
                          aes_string(x="lower_age_range",
                                     y=output.type))
  g_sensitivity <- g_sensitivity + 
    geom_point(aes(col=upper_age_range),
               size=1.5,
               alpha=0.9)  +
    scale_linetype_manual(name=" Point estimate",
                          values=c(1,1)) +
    labs(x="Lower limit
         of age range",
         y=y_label,
         title=y_title,
         col="Upper limit of age range") +
    theme_classic(base_size=base.size.sensitivity) +
    theme(legend.box="vertical",
          legend.text=element_text(size=rel(0.8))) +
    coord_cartesian(ylim=one_ylim)
  if (output.type == "ggbseg") {
    y_intercept = point.estimates %>% 
                  filter(cod == one.level & 
                         sex == one.sex) %>% 
                  select(output.type) %>%
                  as.numeric() 
  } else if (output.type == "RMSE") {
    # computing RMSE corresponding to selected age range (should be the minimum)
    selected_lower_age <- point.estimates %>% 
                          filter(cod == one.level & 
                                 sex == one.sex) %>%
                          select(lower_age_range) %>%
                          as.numeric()
    selected_upper_age <- point.estimates %>% 
                          filter(cod == one.level & 
                                  sex == one.sex) %>%
                          select(upper_age_range) %>%
                          as.numeric()
    y_intercept <- sensitivity.estimates %>%
                         filter(cod == one.level &
                                sex == one.sex &
                                lower_age_range == selected_lower_age &
                                upper_age_range == selected_upper_age) %>%
                         select(RMSE) %>%
                         as.numeric()
  }
  g_sensitivity <- g_sensitivity + 
                   geom_hline(aes(yintercept=y_intercept,
                              linetype=""),
                              size=1.2) 
  return(g_sensitivity)
}




