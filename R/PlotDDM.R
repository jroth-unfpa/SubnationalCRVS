#' Plot DDM estimates and their sensitivity to choice of age range
#'
#' asdf
#' @param ddm_results The object returned by the `EstimateDDM()` function
#' @param size.text.sensitivity An integer fed to `ggplot2::theme(text = element_text())` for the sensitivity plots. Defaults to 8
#' @param fig.nrow An integer fed to `gridExtra::arrangeGrob(nrow)` to indicate how many rows of plots should appear in the visualizations of the sensitivity results by level of disaggregation (males and females in a given level of disaggregation will always be included in the same row). Defaults to 2
#' @param fig.ncol An integer fed to `gridExtra::arrangeGrob(ncol)` to indicate how many times the side-by-side male/female sensitivity plots should appear in each of `fig.nrow` rows. Defaults to 1
#' @param print.plot.point.estimates A logical indicating whether the plot of point estimates across the levels of disaggregation should be printed in the R session. Defaults to TRUE
#' @param save.plot.point.estimates  A logical indicating whether the plot of point estaimtes across the levels of disaggregation should be saved on the local file system. Defaults to TRUE
#' @param save.name.plot.point.estimates A character specifying a custom file name for the plot of point estimates across the levels of disaggregation saved on the local file system. Defaults to NULL, which combines `name.disaggregations` and the current date
#' @param print.plot.sensitivity A logical indicating whether the plots of sensitivity across the levels of disaggregation should be printed in the R session. Defaults to FALSE
#' @param save.plot.point.sensitivity  A logical indicating whether the plots of sensitivity estimates across the levels of disaggregation should be saved on the local file system. Defaults to TRUE
#' @param save.name.plot.point.sensitivity A character specifying a custom file name for the plots of sensitivity estimates across the levels of disaggregation saved on the local file system. Defaults to NULL, which combines `name.disaggregations` and the current date
#' @param plots.dir A character specifying the directory where plots should be saved. Defaults to "", saving the plots in the working directory
#' @examples
#' # It takes about 45 seconds to run this example with show.age.range.sensitivity=TRUE; to run in under 5 seconds please instead use show.age.range.sensitivity=FALSE
#' ecuador_ddm_results_with_sensitivity <- EstimateDDM(data=example_data_ecuador, 
#'                                                     name.disaggregations="province_name",
#'                                                     name.age="age",
#'                                                     name.sex="sex",
#'                                                     name.males="m",
#'                                                     name.females="f",
#'                                                     name.date1="date1",
#'                                                     name.date2="date2",
#'                                                     name.population.year1="pop1",
#'                                                     name.population.year2="pop2",
#'                                                     name.deaths="deaths",
#'                                                     deaths.summed=TRUE,
#'                                                     show.age.range.sensitivity=TRUE)
#' PlotDDM(ddm_results=ecuador_ddm_results_with_sensitivity,
#'         save.plot.point.estimates=FALSE)
#' # Example with show.age.range.sensitivity=FALSE. Also better to use "province_name_short" as disaggregations name here to prevent overcrowding of x-axis.
#' ecuador_ddm_results_no_sensitivity <- EstimateDDM(data=example_data_ecuador, 
#'                                                   name.disaggregations="province_name_short",
#'                                                   name.age="age",
#'                                                   name.sex="sex",
#'                                                   name.males="m",
#'                                                   name.females="f",
#'                                                   name.date1="date1",
#'                                                   name.date2="date2",
#'                                                   name.population.year1="pop1",
#'                                                   name.population.year2="pop2",
#'                                                   name.deaths="deaths",
#'                                                   deaths.summed=TRUE,
#'                                                   show.age.range.sensitivity=FALSE)
#' PlotDDM(ddm_results=ecuador_ddm_results_no_sensitivity,
#'         save.plot.point.estimates=TRUE)
#' @import DDM
#' @import dplyr
#' @import ggplot2
#' @import ggpubr
#' @import gridExtra
#' @export

PlotDDM <- function(ddm_results,
                    size.text.sensitivity=8,
                    fig.nrow=2,
                    fig.ncol=1,
                    print.plot.point.estimates=TRUE,
                    save.plot.point.estimates=TRUE,
                    save.name.plot.point.estimates=NULL,
                    print.plots.sensitivity=FALSE,
                    save.plots.sensitivity=TRUE,
                    save.name.plots.sensitivity=NULL,
                    plots.dir="") {
  # setting up
  name_disaggregations <- ddm_results$name_disaggregations
  
  # plot DDM point estimates
  ddm_point_estimates <- ddm_results$ddm_estimates
  g_point_estimate <- ggplot(data=ddm_point_estimates %>%
                                  filter(cod != 90),
                              aes(x=cod,
                                 y=ggbseg))
  g_point_estimate <- g_point_estimate + 
                      geom_point(aes(col=sex),
                                 size=3,
                                 alpha=0.7) +
                      labs(x=name_disaggregations,
                           y="Estimated death registration completeness (GGBSEG)")
  # plot DDM estimates for all possible age ranges considered in the search that generated the point estimate
  if (ddm_results$show.age.range.sensitivity == TRUE) {
    ddm_sensitivity_estimates <- ddm_results$sensitivity_ddm_estimates
    ddm_sensitivity_estimates[, "lower_age_range"] <- 
      as.factor(ddm_sensitivity_estimates[, "lower_age_range"])
    ddm_sensitivity_estimates[, "upper_age_range"] <- 
      as.factor(ddm_sensitivity_estimates[, "upper_age_range"])
  
    all_levels <- unique(ddm_sensitivity_estimates$cod)
    n_disaggregations <- length(all_levels)
    list_plots_sensitivity <- vector("list", length=n_disaggregations)
    for (i in 1:n_disaggregations) {
      one_level <- all_levels[i]
      one_ylim <- c(0.9 * min(ddm_sensitivity_estimates[ddm_sensitivity_estimates$cod == one_level, "ggbseg"]),
                    1.1 * max(ddm_sensitivity_estimates[ddm_sensitivity_estimates$cod == one_level, "ggbseg"]))
      ## women
      g_sensitivity_females <- ggplot(data=ddm_sensitivity_estimates %>%
                                           filter(cod == one_level & 
                                                sex == "Females"),
                                      aes(x=lower_age_range,
                                          y=ggbseg))
      g_sensitivity_females <- g_sensitivity_females + 
                              geom_point(aes(col=upper_age_range),
                                         size=1.5,
                                         alpha=0.9) +
                              geom_hline(aes(yintercept=ddm_point_estimates %>% 
                                                         filter(cod == one_level & 
                                                                sex == "Females") %>% 
                                                         select(ggbseg) %>%
                                                         as.numeric(),
                                          linetype="")) +
                              scale_linetype_manual(name=" Point estimate",
                                                    values=c(1,1)) +
                              labs(x="Lower limit of age range",
                                  y="Estimated completeness (GGB-SEG)",
                                  title=paste("Estimated completeness in\n",
                                              one_level,
                                              "-- Females"),
                                  col="Upper limit of age range") +
                              theme(text = element_text(size=size.text.sensitivity),
                                    legend.box="vertical",
                                    legend.text=element_text(size=rel(0.8))) + 
                              coord_cartesian(ylim=one_ylim)
    
      ## men
      g_sensitivity_males <- ggplot(data=ddm_sensitivity_estimates %>%
                                        filter(cod == one_level & 
                                              sex == "Males"),
                                      aes(x=lower_age_range,
                                          y=ggbseg))
     g_sensitivity_males <- g_sensitivity_males + 
                              geom_point(aes(col=upper_age_range),
                                          size=1.5,
                                          alpha=0.9) +
                              geom_hline(aes(yintercept=ddm_point_estimates %>% 
                                                        filter(cod == one_level & 
                                                                sex == "Males") %>% 
                                                        select(ggbseg) %>%
                                                        as.numeric(),
                                              linetype="")) +
                              scale_linetype_manual(name=" Point estimate",
                                                     values=c(1,1)) +
                              labs(x="Lower limit of age range",
                                    y="Estimated completeness (GGB-SEG)",
                                    title=paste("Estimated completeness in\n",
                                                one_level,
                                                "-- Males"),
                                    col="Upper limit of age range") +
                              theme(text = element_text(size=size.text.sensitivity),
                                    legend.box="vertical",
                                    legend.text=element_text(size=rel(0.8))) + 
                              coord_cartesian(ylim=one_ylim)
    
    
      g_sensitivity_combined <- ggarrange(g_sensitivity_females,
                                          g_sensitivity_males,
                                          nrow=1)  
      list_plots_sensitivity[[i]] <- g_sensitivity_combined
    }
    overall <- marrangeGrob(list_plots_sensitivity,
                            nrow=fig.nrow,
                            ncol=fig.ncol)
    ## print/save plots of sensitivity according to specified arguments 
    graphics.off()
    if (save.plots.sensitivity == TRUE) {
      if (is.null(save.name.plots.sensitivity) == FALSE) {
        pdf(paste0(save.name.plots.sensitivity, ".pdf")) 
      } else {
        pdf(paste0(plots.dir, "ddm_sensitivity_", 
                   name_disaggregations, "_", Sys.Date(), ".pdf"))
      }
      print(overall)
      graphics.off()
      if (print.plots.sensitivity == TRUE) {
        print(overall)
      } 
    }
  }
  # print/save plots of point estimates according to specified arguments
  graphics.off()
  if (save.plot.point.estimates == TRUE) {
    if (is.null(save.name.plot.point.estimates) == FALSE) {
      pdf(paste0(save.name.plots, ".pdf")) 
    } else {
      pdf(paste0(plots.dir, "ddm_point_estimates_combined_", 
                 name_disaggregations, "_", Sys.Date(), ".pdf"))
    }
    print(g_point_estimate)
    graphics.off()
  }
  if (print.plot.point.estimates == TRUE) {
    print(g_point_estimate)
  }   
}
