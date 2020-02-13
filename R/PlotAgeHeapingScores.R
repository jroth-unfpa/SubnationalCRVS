#' Plot age heaping scores
#'
#' asdf
#' 
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
#' @param fig.nrow An integer fed to `gridExtra::arrangeGrob(nrow)` to indicate how many rows on each page should be used to display the 5 plots
#' @param fig.ncol An integer fed to `gridExtra::arrangeGrob(ncol)` to indicate how many columns on each page should be used to display the 5 plots
#' @param print.plots A logical indicating whether the plots should be printed in the R session. Defaults to TRUE
#' @param save.plots A logical indicating whether the plots should be saved on the local file system. Defaults to TRUE
#' @param save.name.plots A character specifying a custom file name for the plots saved on the local file system. Defaults to NULL, which combines `name.disaggregations` and the current date
#' @param plots.dir A character specifying the directory where plots should be saved. Defaults to "", saving the plots in the working directory
#' @examples
#' age_heaping_plotting <- PlotAgeHeapingScores(data=ecuador_age_tabulation,
#'                                                   name.disaggregations="province_name_short",
#'                                                   name.males="m",
#'                                                   name.females="f",
#'                                                   name.age="age",
#'                                                   name.sex="sex",
#'                                                   name.date1="date1",
#'                                                   name.date2="date2",
#'                                                   name.population.year1="pop1",
#'                                                   name.population.year2="pop2",
#'                                                   plots.dir="Plots/")
#' head(age_heaping_plotting)
#' @import dplyr
#' @import ggplot2
#' @import ggpubr
#' @import gridExtra
#' @export

PlotAgeHeapingScores <- function(data, 
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
                          fig.nrow=3,
                          fig.ncol=1,
                          print.plots=TRUE,
                          save.plots=TRUE,
                          save.name.plots=NULL,
                          plots.dir="") {
  # compute age heaping scores by calling ComputeAgeHeapingScores()
  ## variable checks performed within ComputeAgeHeapingScores() (should just call another function to do the checks that doesn't need to be documented)
  data_with_age_heaping_long <- ComputeAgeHeapingScores(data=data,
                                    name.disaggregations=name.disaggregations,
                                    name.age=name.age,
                                    name.sex=name.sex,
                                    name.males=name.males,
                                    name.females=name.females,
                                    name.date1=name.date1,
                                    name.date2=name.date2,
                                    name.population.year1=name.population.year1,
                                    name.population.year2=name.population.year2,
                                    roughness.age.min=roughness.age.min,
                                    roughness.age.max=roughness.age.max,
                                    sawtooth.age.min=sawtooth.age.min,
                                    sawtooth.age.max=sawtooth.age.max,
                                    Whipple.age.min=Whipple.age.min,
                                    Whipple.age.max=Whipple.age.max,
                                    Whipple.digit=Whipple.digit,
                                    Myers.age.min=Myers.age.min,
                                    Myers.age.max=Myers.age.max)
  
  # make plots
  ## roughness
  g_roughness <- ggplot(data=data_with_age_heaping_long,
                                  aes(x=get(name.disaggregations),
                                      y=roughness)) +
                           geom_point(aes(col=get(name.sex),
                                          shape=date)) +
    labs(x=name.disaggregations,
         y="roughness",
         title=paste0("roughness \n", "by ", name.disaggregations)) +
    scale_colour_discrete(name=name.sex)
  ## sawtooth
  g_sawtooth <- ggplot(data=data_with_age_heaping_long,
                                  aes(x=get(name.disaggregations),
                                      y=sawtooth)) +
                          geom_point(aes(col=sex,
                                         shape=date)) +
    labs(x=name.disaggregations,
         y="sawtooth",
         title=paste0("sawtooth \n", "by ", name.disaggregations)) +
    scale_colour_discrete(name=name.sex)
  ## Whipple
  g_Whipple <- ggplot(data=data_with_age_heaping_long,
                                 aes(x=get(name.disaggregations),
                                     y=Whipple)) +
    geom_point(aes(col=sex,
                   shape=date)) +
    labs(x=name.disaggregations,
         y="Whipple's index",
         title=paste0("Whipple's index \n", "by ", name.disaggregations)) +
    scale_colour_discrete(name=name.sex)
  ## Myers
  g_Myers <- ggplot(data=data_with_age_heaping_long,
                      aes(x=get(name.disaggregations),
                          y=Myers)) +
    geom_point(aes(col=sex,
                   shape=date)) +
    labs(x=name.disaggregations,
         y="Myers' blendex index",
         title=paste0("Myers' blended index \n", "by ", name.disaggregations)) +
    scale_colour_discrete(name=name.sex)
  ## Noumbissi
  g_Noumbissi <- ggplot(data=data_with_age_heaping_long,
                    aes(x=get(name.disaggregations),
                        y=Noumbissi)) +
    geom_point(aes(col=sex,
                   shape=date)) +
    labs(x=name.disaggregations,
         y="Noumbissi's digit heaping index",
         title=paste0("Noumbissi's digit heaping index \n", "by ", name.disaggregations)) +
    scale_colour_discrete(name=name.sex)
  
  list_plots <- list(g_roughness,
                     g_sawtooth,
                     g_Whipple,
                     g_Noumbissi,
                     g_Myers)
  overall <- marrangeGrob(list_plots,
                          nrow=fig.nrow,
                          ncol=fig.ncol)
  
  # print/save plots according to specified arguments
  graphics.off()
  if (save.plots == TRUE) {
    if (is.null(save.name.plots) == FALSE) {
      pdf(paste0(save.name.plots, ".pdf")) 
    } else {
      pdf(paste0(plots.dir, "age_heaping_scores_combined_", 
                 name.disaggregations, "_", Sys.Date(), ".pdf"))
    }
    print(overall)
    graphics.off()
  }
  if (print.plots == TRUE) {
    print(overall)
  } 
  return(data_with_age_heaping_long)
}
