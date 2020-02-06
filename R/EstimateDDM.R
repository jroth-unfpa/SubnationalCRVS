#' Estimate death registration between two Census years with a wrapper of the ddm() function from the DDM
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
#' @param deaths.summed asdf
#' @param show_age_range_sensitivity asdf
#' @param min.age.in.search asdf
#' @param max.age.in.search asdf
#' @param min.number.of.ages asdf
#' @param exact.ages.to.use asdf
#' @param largest.lower.limit.sensitivity asdf
#' @param smallest.upper.limit.sensitivity asdf
#' @param life.expectancy.in.open.group asdf
#' @examples
#' @import dplyr
#' @export

EstimateDDM <- function(data, 
                        name.disaggregations,
                        name.age,
                        name.sex,
                        name.males,
                        name.females,
                        name.date1,
                        name.date2,
                        name.population.year1,
                        name.population.year2,
                        name.deaths,
                        deaths.summed, # should not have a default
                        show_age_range_sensitivity=TRUE,
                        min.age.in.search=15,
                        max.age.in.search=75,
                        min.number.of.ages=8,
                        exact.ages.to.use=NULL,
                        largest.lower.limit.sensitivity=45,
                        smallest.upper.limit.sensitivity=50,
                        life.expectancy.in.open.group=NULL) {
  # variable checks -- again, this should call a common VariableChecks() function, maybe with an argument that can take the value "DDM"
  if (length(unique(data[, name.date1])) != 1) {
    stop("date1 variable must contain only one unique value")
  }
  if (length(unique(data[, name.date2])) != 1) {
    stop("date2 variable must contain only one unique value")
  }
  date.1 <- data[1, name.date1]
  date.2 <- data[1, name.date2]
  # re-formatting variables (this is the key purpose because the ddm() function is very picky)
  data_formatted <- FormatVariablesDDM(data=data, 
                     name.disaggregations=name.disaggregations,
                     name.age=name.age,
                     name.sex=name.sex,
                     name.males=name.males,
                     name.females=name.females,
                     name.date1=name.date1,
                     name.date2=name.date2,
                     name.population.year1=name.population.year1,
                     name.population.year2=name.population.year2,
                     name.deaths=name.deaths)
  # estimate completeness with ddm() function from DDM package
  ## males
  result_ddm_males <- ddm(X=data_formatted$data_for_ddm_males,
                    deaths.summed=deaths.summed,
                    minA=min.age.in.search,
                    maxA=max.age.in.search,
                    minAges=min.number.of.ages,
                    exact.ages=exact.ages.to.use,
                    eOpen=life.expectancy.in.open.group)
  
  ## females
  result_ddm_females <- ddm(X=data_formatted$data_for_ddm_females,
                          deaths.summed=deaths.summed,
                          minA=min.age.in.search,
                          maxA=max.age.in.search,
                          minAges=min.number.of.ages,
                          exact.ages=exact.ages.to.use,
                          eOpen=life.expectancy.in.open.group)
  # summarize and refomat results of call to ddm()
  ddm_estimates <- FormatOutputDDM(result_ddm_females=result_ddm_females,
                                   result_ddm_males=result_ddm_males)
  if (show_age_range_sensitivity == TRUE) {
    # perform DDM estimation for a sequence of age-range parameters to give a sense of estimation sensitivity
    # setting up before loop
    data_for_ddm_females <- data_formatted$data_for_ddm_females
    data_for_ddm_males <- data_formatted$data_for_ddm_males
    lower.limits.age.sensitivity <- seq(from=min.age.in.search,
                                        to=largest.lower.limit.sensitivity,
                                        by=5)
    upper.limits.age.sensitivity <- seq(from=smallest.upper.limit.sensitivity,
                                        to=max.age.in.search,
                                        by=5)
    possible_age_range_endpoints <- expand.grid(lower.limits.age.sensitivity,
                                       upper.limits.age.sensitivity)
    possible_age_range_sequences <- apply(possible_age_range_endpoints, 
                                          1, 
                                          function(x) seq(from=x[1], 
                                                           to=x[2], 
                                                           by=5))
    idx_acceptable_age_range_sequences <- sapply(possible_age_range_sequences,
                                                 function(x) length(x) >= 8)
    acceptable_age_range_sequences <- possible_age_range_sequences[idx_acceptable_age_range_sequences]
    n_age_combinations <- length(acceptable_age_range_sequences)
    sensitivity_ddm_estimates <- matrix(NA, nrow=1, ncol=ncol(ddm_estimates))
    colnames(sensitivity_ddm_estimates) <- colnames(ddm_estimates)
    # performing DDM estimation across all combinations of exact.ages sequences
    print(paste("performing DDM estimation within each of", 
                 n_age_combinations, 
                 "possible age ranges..."))
    for (seq in 1:length(acceptable_age_range_sequences)) {
      one_exact_ages <- acceptable_age_range_sequences[[seq]]
      one_ddm_females <- ddm(X=data_formatted$data_for_ddm_females,
                             deaths.summed=deaths.summed,
                             exact.ages=one_exact_ages,
                             eOpen=life.expectancy.in.open.group)
      one_ddm_males <- ddm(X=data_formatted$data_for_ddm_males,
                           deaths.summed=deaths.summed,
                           exact.ages=one_exact_ages,
                           eOpen=life.expectancy.in.open.group)
      one_ddm_estimates <- FormatOutputDDM(result_ddm_females=one_ddm_females,
                                           result_ddm_males=one_ddm_males)
      sensitivity_ddm_estimates <- rbind(sensitivity_ddm_estimates,
                                         one_ddm_estimates)
    }
    sensitivity_ddm_estimates <- sensitivity_ddm_estimates[-1, ]
    return(list("show_age_range_sensitivity"=show_age_range_sensitivity,
                "name_disaggregations"=name.disaggregations,
                "sensitivity_ddm_estimates"=sensitivity_ddm_estimates,
                "ddm_estimates"=ddm_estimates))
  }
}
