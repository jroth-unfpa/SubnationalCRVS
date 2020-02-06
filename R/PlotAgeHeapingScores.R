#' Plot age heaping scores
#'
#' asdf
#' @param data sadf
#' @param name.disaggregations asdf
#' @param name.age asdf
#' @param name.sex asdf
#' @param name.males asdf
#' @param name.females asdf
#' @param name.date1 asdf
#' @param name.date2 asdf
#' @param name.population.year1 asdf
#' @param name.population.year2 asdf
#' @param roughness.age.min asdf
#' @param roughness.age.max asdf
#' @param sawtooth.age.min asdf
#' @param sawtooth.age.max asdf
#' @param Whipple.age.min asdf
#' @param Whipple.age.max asdf
#' @param Whipple.digit asdf
#' @param Myers.age.min asdf
#' @param Myers.age.max asdf
#' @param fig.nrow asdf
#' @param fig.ncol asdf
#' @param print.plots asdf
#' @param save.plots asdf
#' @param save.name.plots asdf
#' @param plots.dir asdf
#' @examples
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
                           geom_point(aes(col=sex,
                                          shape=date)) +
    labs(x=name.disaggregations,
         y="roughness",
         title=paste0("roughness \n", "by ", name.disaggregations)) +
    scale_colour_discrete(name=name.disaggregations)
  ## sawtooth
  g_sawtooth <- ggplot(data=data_with_age_heaping_long,
                                  aes(x=get(name.disaggregations),
                                      y=sawtooth)) +
                          geom_point(aes(col=sex,
                                         shape=date)) +
    labs(x=name.disaggregations,
         y="sawtooth",
         title=paste0("sawtooth \n", "by ", name.disaggregations)) +
    scale_colour_discrete(name=name.disaggregations)
  ## Whipple
  g_Whipple <- ggplot(data=data_with_age_heaping_long,
                                 aes(x=get(name.disaggregations),
                                     y=Whipple)) +
    geom_point(aes(col=sex,
                   shape=date)) +
    labs(x=name.disaggregations,
         y="Whipple's index",
         title=paste0("Whipple's index \n", "by ", name.disaggregations)) +
    scale_colour_discrete(name=name.disaggregations)
  ## Myers
  g_Myers <- ggplot(data=data_with_age_heaping_long,
                      aes(x=get(name.disaggregations),
                          y=Myers)) +
    geom_point(aes(col=sex,
                   shape=date)) +
    labs(x=name.disaggregations,
         y="Myers' blendex index",
         title=paste0("Myers' blended index \n", "by ", name.disaggregations)) +
    scale_colour_discrete(name=name.disaggregations)
  ## Noumbissi
  g_Noumbissi <- ggplot(data=data_with_age_heaping_long,
                    aes(x=get(name.disaggregations),
                        y=Noumbissi)) +
    geom_point(aes(col=sex,
                   shape=date)) +
    labs(x=name.disaggregations,
         y="Noumbissi's digit heaping index",
         title=paste0("Noumbissi's digit heaping index \n", "by ", name.disaggregations)) +
    scale_colour_discrete(name=name.disaggregations)
  
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
