#' Plot age heaping scores
#'
#' asdf
#' 
#' @param data data frame that contains at least seven columns representing: (1) single-year age,
#' (2) sex,
#' (3, 4) population counts collected at two different time points (typically adjacent Census years)
#' (5, 6) dates of two different time points
#' (7) the level of subnational disaggregation in addition to sex (e.g. a geographic unit such as a province/state, 
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
#' @param name.national A character string providing the value of `name.disaggregations` variable that indicates national-level results (e.g. "Overall" or "National"). Defaults to NULL, implying `name.disaggregations` variable in `data` only includes values for subnational levels. Defaults to NULL
#' @param show.size.population A logical indixating whether the size of plotted points should vary according to the total population size in the second data year. Defaults to TRUE
#' @param roughness.age.min=NULL Equivalent to the `ageMin` argument of `Demotools::check_heaping_roughness`. Defaults to NULL, which then uses the `DemoTools` default of 20
#' @param roughness.age.max=NULL Equivalent to the `ageMax` argument of `Demotools::check_heaping_roughness`. Defaults to NULL, which then uses the `DemoTools` default of the highest age that is a multiple of 10
#' @param Whipple.age.min=NULL Equivalent to the `ageMin` argument of `Demotools::check_heaping_whipple`. Defaults to NULL, which then uses the `DemoTools` default of 25
#' @param Whipple.age.max=NULL Equivalent to the `ageMax` argument of `Demotools::check_heaping_whipple`. Defaults to NULL, which then uses the `DemoTools` default of 65
#' @param Whipple.digit=NULL Equivalent to the `digit` argument of `Demotools::check_heaping_whipple`. Defaults to NULL, which then uses the `DemoTools` default of c(0, 5)
#' @param Myers.age.min=NULL Equivalent to the `ageMin` argument of `Demotools::check_heaping_myers`. Defaults to NULL, which then uses the `DemoTools` default of 10
#' @param Myers.age.max=NULL Equivalent to the `ageMax` argument of `Demotools::check_heaping_myers`. Defaults to NULL, which then uses the `DemoTools` default of 89
#' @param label.subnational.level A character label for the axis showing the level of subnational disaggregation present in the data. Defaults to `name.disaggregations` 
#' @param base.size A numeric fed to `ggplot2::theme_classic(base_size)` for the plot of point estimates. Defaults to 12
#' @param fig.nrow An integer fed to `gridExtra::arrangeGrob(nrow)` to indicate how many rows on each page should be used to display the 5 plots
#' @param fig.ncol An integer fed to `gridExtra::arrangeGrob(ncol)` to indicate how many columns on each page should be used to display the 5 plots
#' @param save.name.plots A character specifying a custom file name for the plots saved on the local file system. Defaults to NULL, which combines `name.disaggregations` and the current date
#' @param plots.dir A character specifying the directory where plots should be saved. Defaults to "", saving the plots in the working directory
#' @examples
#' age_heaping_plotting <- PlotAgeHeapingScores(data=ecuador_single_year_ages
#'                                                   name.disaggregations="province_name",
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
#' @import reshape2
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
                          name.national=NULL,
                          show.size.population=TRUE,
                          roughness.age.min=NULL,
                          roughness.age.max=NULL,
                          Whipple.age.min=NULL,
                          Whipple.age.max=NULL,
                          Whipple.digit=NULL,
                          Myers.age.min=NULL,
                          Myers.age.max=NULL,
                          label.subnational.level=name.disaggregations,
                          base.size=12,
                          fig.nrow=1,
                          fig.ncol=1,
                          save.name.plots=NULL,
                          plots.dir="") {
 data[, name.disaggregations] <- as.factor(data[, name.disaggregations])
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
                                    Whipple.age.min=Whipple.age.min,
                                    Whipple.age.max=Whipple.age.max,
                                    Whipple.digit=Whipple.digit,
                                    Myers.age.min=Myers.age.min,
                                    Myers.age.max=Myers.age.max)
  data_with_age_heaping_long[, name.disaggregations] <- factor(data_with_age_heaping_long[, name.disaggregations],
                                                               levels=rev(levels(data_with_age_heaping_long[, name.disaggregations])))

  # make plots
  all_levels <- unique(levels(data[, name.disaggregations]))
  n_disaggregations <- length(all_levels)
  list_plots <- vector("list", length=n_disaggregations)
  national_check <- FALSE ## needed here?
  max_roughness <- max( data_with_age_heaping_long$roughness, na.rm=TRUE)
  max_Whipple <- max(data_with_age_heaping_long$Whipple, na.rm=TRUE)
  max_Myers <- max(data_with_age_heaping_long$Myers, na.rm=TRUE)
      
  if (n_disaggregations > 1) { 
    if (is.null(name.national) == FALSE) {
      national_check <- TRUE ## needed here?
      data_with_age_heaping_long_for_overall <- data_with_age_heaping_long[data_with_age_heaping_long[, name.disaggregations] != name.national, ]
    } else {
      data_with_age_heaping_long_for_overall <-  data_with_age_heaping_long
    }
    ## roughness
    g_roughness <- ggplot(data=data_with_age_heaping_long_for_overall,
                          aes(x=roughness,
                              y=get(name.disaggregations)))
    if (show.size.population == TRUE) {
      g_roughness <-g_roughness + 
        geom_point(aes(col=sex,
                       shape=date,
                       size=total_pop)) +
        scale_size_continuous(labels = comma,
                              range=c(1.5, 7),
                              name="Population")
    } else {
      g_roughness <- g_roughness + 
        geom_point(aes(col=sex,
                       shape=date),
                   size=3)
    }
    g_roughness <- g_roughness +
      labs(x="roughness",
           y=label.subnational.level,
           title=paste0("roughness by ", label.subnational.level)) +
      theme_classic(base_size=base.size) +
      scale_color_discrete(name="Sex") +
      scale_shape_discrete(name="Date") +
      coord_cartesian(xlim=c(0, max_roughness))
    
    ## Whipple
    g_Whipple <- ggplot(data=data_with_age_heaping_long_for_overall,
                        aes(x=Whipple,
                            y=get(name.disaggregations)))
    if (show.size.population == TRUE) {
      g_Whipple <- g_Whipple + 
        geom_point(aes(col=sex,
                       shape=date,
                       size=total_pop)) +
        scale_size_continuous(labels = comma,
                              range=c(1.5, 7),
                              name="Population")
    } else {
      g_Whipple <- g_Whipple + 
        geom_point(aes(col=sex,
                       shape=date),
                   size=3)
    }
    g_Whipple <- g_Whipple + 
      labs(x="Whipple's index",
           y=label.subnational.level,
           title=paste0("Whipple's index by ", label.subnational.level)) +
      theme_classic(base_size=base.size) +
      scale_color_discrete(name="Sex") +
      scale_shape_discrete(name="Date") +
      coord_cartesian(xlim=c(0, max_Whipple))
      
    ## Myers
    g_Myers <- ggplot(data=data_with_age_heaping_long_for_overall,
                      aes(x=Myers,
                          y=get(name.disaggregations)))
    if (show.size.population == TRUE) {
      g_Myers <-g_Myers + 
        geom_point(aes(col=sex,
                       shape=date,
                       size=total_pop)) +
        scale_size_continuous(labels = comma,
                              range=c(1.5, 7),
                              name="Population")
    } else {
      g_Myers <- g_Myers + 
        geom_point(aes(col=sex,
                       shape=date),
                   size=3)
    }
    g_Myers <- g_Myers +
      labs(x="Myers' blended index",
           y=label.subnational.level,
           title=paste0("Myers' blended index by ", label.subnational.level)) +
      theme_classic(base_size=base.size) +
      scale_color_discrete(name="Sex") +
      scale_shape_discrete(name="Date") + 
      coord_cartesian(xlim=c(0, max_Myers))
    
    list_plots <- list(g_roughness,
                       g_Whipple,
                       g_Myers)

    overall <- marrangeGrob(list_plots,
                            nrow=fig.nrow,
                            ncol=fig.ncol)
  } 
  
  if (is.null(name.national) == FALSE) {
    national_check <- TRUE
    data_with_age_heaping_long_for_national <- data_with_age_heaping_long[data_with_age_heaping_long[, name.disaggregations] == name.national, ]
    data_with_age_heaping_long_for_national$date <- as.factor(data_with_age_heaping_long_for_national$date)
    head(data_with_age_heaping_long_for_national)
    max_index <- max(data_with_age_heaping_long_for_national[, c("roughness", "Whipple", "Myers")],
                     na.rm=TRUE)
    
    ## reshape
    head(data_with_age_heaping_long_for_national)
    reshape_national <- melt(data_with_age_heaping_long_for_national,
               id.vars=c(name.disaggregations, "sex", "date"),
               measure.vars=c("roughness", "Whipple", "Myers"),
               variable.name="index_name",
               value.name="index_value")
    
    g_national <- ggplot(data=reshape_national,
                aes(x=index_value,
                    y=index_name))
    g_national <- g_national + 
              geom_point(size=2,
                         aes(col=sex,
                             shape=date)) +
              labs(x="Value",
                   y="Index",
                   title=paste0("Age heaping indices -- ", name.national)) +
              theme_classic(base_size=base.size) +
              scale_color_discrete(name="Sex") + 
              coord_cartesian(xlim=c(0, max_index))
  }
  
  # print/save plots according to specified arguments
  graphics.off()
  if (is.null(save.name.plots) == FALSE) {
    if (n_disaggregations > 1) {
      ggsave(paste0(plots.dir, save.name.plots, 
                    name.disaggregations, "_", Sys.Date(), ".pdf"),
             overall)
    }
    if (national_check == TRUE) {
      ggsave(paste0(plots.dir, save.name.plots, 
                    "_", name.national, "_", Sys.Date(), ".pdf"),
             g_national)
    }
  } else {
    if (n_disaggregations > 1) {
      ggsave(paste0(plots.dir, "age_heaping_scores_combined_", 
                    name.disaggregations, "_", Sys.Date(), ".pdf"),
             overall)
    }
    if (national_check == TRUE) {
      ggsave(paste0(plots.dir, "age_heaping_scores_", 
                    name.national, "_", Sys.Date(), ".pdf"),
             g_national) 
    }
  }
  graphics.off()
  # arrange data to return
  data_to_return <- data_with_age_heaping_long %>%
                    select(name.disaggregations,
                           date,
                           total_pop,
                           name.sex,
                           roughness,
                           Whipple,
                           Myers)
  return(data_to_return)
}
