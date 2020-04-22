#' Plot DDM estimates and their sensitivity to choice of age range
#'
#' asdf
#' @param ddm_results The object returned by the `EstimateDDM()` function
#' @param base.size.point.estimates A numeric fed to `ggplot2::theme_classic(base_size)` for the plot of point estimates. Defaults to 13
#' @param base.size.sensitivity A numeric fed to `ggplot2::theme_classic(base_size)` for the plot of point estimates. Defaults to 9
#' @param fig.nrow An integer fed to `gridExtra::arrangeGrob(nrow)` to indicate how many rows of plots should appear in the visualizations of the sensitivity results by level of disaggregation (males and females in a given level of disaggregation will always be included in the same row). Defaults to 2
#' @param fig.ncol An integer fed to `gridExtra::arrangeGrob(ncol)` to indicate how many times the side-by-side male/female sensitivity plots should appear in each of `fig.nrow` rows. Defaults to 1
#' @param show.lines.sex.differential A logical indixating whether vertical lines connecting the estimated completenss for males and females should be drawn. Defaults to TRUE
#' @param show.size.population A logical indicating whether the size of plotted points should vary according to the total population size in the second data year. Defaults to TRUE
#' @param label.completeness A character label for the axis showing estimated completeness of death registration completeness (on a scale of 0 to 100). Default is "Estimated death registration completeness (GGB-SEG)"
#' @param label.subnational.level A character label for the axis showing the level of subnational disaggregation present in the data. Default is the value of the `name.disaggregations` argument supplied to EstimateDDM()
#' @param save.name.plot.point.estimates A character specifying a custom file name for the plot of point estimates across the levels of disaggregation saved on the local file system. Defaults to NULL, which combines `name.disaggregations` and the current date
#' @param save.name.plot.point.sensitivity A character specifying a custom file name for the plots of sensitivity estimates across the levels of disaggregation saved on the local file system. Defaults to NULL, which combines `name.disaggregations` and the current date
#' @param plots.dir A character specifying the directory where plots should be saved. Defaults to "", saving the plots in the working directory
#' @examples
#' # It takes about 45 seconds to run this example with show.age.range.sensitivity=TRUE; to run in under 5 seconds please instead use show.age.range.sensitivity=FALSE
#' ecuador_ddm_results_with_sensitivity <- EstimateDDM(data=ecuador_five_year_ages, 
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
#' ecuador_ddm_results_no_sensitivity <- EstimateDDM(data=ecuador_five_year_ages, 
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
#' @import scales
#' @export

PlotDDM <- function(ddm_results,
                    base.size.point.estimates=13,
                    base.size.sensitivity=9,
                    fig.nrow=2,
                    fig.ncol=1,
                    show.lines.sex.differential=TRUE,
                    show.size.population=TRUE,
                    label.completeness="Estimated death registration completeness (GGB-SEG)",
                    label.subnational.levels=ddm_results$name_disaggregations,
                    save.name.plot.point.estimates=NULL,
                    save.name.plots.sensitivity=NULL,
                    plots.dir="") {
  # setting up
  name_disaggregations <- ddm_results$name_disaggregations
  date1 <- ddm_results$date1
  date2 <- ddm_results$date2
  name_national <- ddm_results$name.national
  
  # plot GGB-SEG point estimates
  ggbseg_point_estimates <- ddm_results$ggbseg_estimates
  ggbseg_point_estimates$ggbseg <- ggbseg_point_estimates$ggbseg * 100
  test <- ggbseg_point_estimates %>%
          filter(sex == "Females") %>%
          arrange(ggbseg) %>%
          select(cod) %>%
          pull() %>%
          as.character()
  ggbseg_point_estimates$cod <- factor(as.character(ggbseg_point_estimates$cod),
                                    levels=test)
  if (is.null(name_national) == FALSE) {
    ggbseg_point_estimates_for_plot <- ggbseg_point_estimates[ggbseg_point_estimates[, "cod"] != name_national, ]
  } else {
    ggbseg_point_estimates_for_plot <-  ggbseg_point_estimates
  }
  
  g_point_estimate <- ggplot(data=ggbseg_point_estimates_for_plot,
                             aes(x=ggbseg,
                                 y=cod))
  if (show.size.population == TRUE) {
    g_point_estimate <- g_point_estimate + 
                      geom_point(aes(col=sex,
                                     size=total_pop2),
                                 alpha=0.7) +
    scale_size_continuous(labels=comma,
                          range=c(1.5, 7),
                          name=paste0("Pop. (", date2, ")"))
  } else {
    g_point_estimate <- g_point_estimate + 
      geom_point(aes(col=sex),
                 size=3,
                 alpha=0.7)
  }
  g_point_estimate <- g_point_estimate + 
                      labs(x=label.completeness,
                           y=label.subnational.levels) +
                      scale_color_discrete(name="Sex") +
                      theme_classic(base_size=base.size.point.estimates)
  if (show.lines.sex.differential == TRUE) {
    g_build <- ggplot_build(g_point_estimate)
    g_colors <- g_build$data[[1]]["colour"]$colour
    g_color_males <- unique(g_colors[g_point_estimate$data$sex == "Males"])
    g_color_females <- unique(g_colors[g_point_estimate$data$sex == "Females"])
    
    if (length(g_color_males) == 1 & length(g_color_females) == 1) {
      identify_sex_larger_completeness <- ggbseg_point_estimates_for_plot %>%
        group_by(cod) %>%
        summarise("males_larger"=ggbseg[sex == "Males"] >
                    ggbseg[sex == "Females"])
      ggbseg_point_estimates_for_plot_larger <- left_join(x=ggbseg_point_estimates_for_plot,
                                              y=identify_sex_larger_completeness,
                                              by="cod")
      g_point_estimate <- g_point_estimate +
                          geom_line(data=ggbseg_point_estimates_for_plot_larger %>%
                                       filter(males_larger == TRUE),
                                    col=g_color_males) +
                          geom_line(data=ggbseg_point_estimates_for_plot_larger %>%
                                       filter(males_larger == FALSE),
                                    col=g_color_females)
    } else {
      g_point_estimate <- g_point_estimate +
                          geom_line(data=ggbseg_point_estimates_for_plot,
                                    col="gray")
    }
  }
  if (is.null(name_national) == FALSE) {
    g_point_estimate <-      g_point_estimate + 
      geom_vline(aes(xintercept=ggbseg_point_estimates %>% 
                       filter(cod == name_national & 
                                sex == "Males") %>% 
                       select(ggbseg) %>%
                       as.numeric()),
                 linetype="dashed",
                 col=g_color_males,
                 alpha=0.8,
                 size=1.2) +
      geom_vline(aes(xintercept=ggbseg_point_estimates %>% 
                       filter(cod == name_national & 
                                sex == "Females") %>% 
                       select(ggbseg) %>%
                       as.numeric()),
                 linetype="dashed",
                 color=g_color_females,
                 alpha=0.8,
                 size=1.2) +
      labs(caption=paste("*Dashed lines show",
                         name_national,
                         "sex-specific estimates of completeness"))
  }
  # plot GGB-SEG estimates for all possible age ranges considered in the search that generated the point estimate
  if (ddm_results$show.age.range.sensitivity == TRUE) {
    ggbseg_sensitivity_estimates <- ddm_results$sensitivity_ggbseg_estimates
    ggbseg_sensitivity_estimates$ggbseg <- ggbseg_sensitivity_estimates$ggbseg * 100
    ggbseg_sensitivity_estimates[, "lower_age_range"] <- 
      as.factor(ggbseg_sensitivity_estimates[, "lower_age_range"])
    ggbseg_sensitivity_estimates[, "upper_age_range"] <- 
      as.factor(ggbseg_sensitivity_estimates[, "upper_age_range"])
  
    all_levels <- unique(ggbseg_sensitivity_estimates$cod)
    n_disaggregations <- length(all_levels)
    list_plots_sensitivity <- vector("list", length=n_disaggregations)
    national_check <- FALSE
    if (n_disaggregations == 1 & is.null(name_national) == TRUE) {
      stop(paste("Only one level of disaggregation,",
                 all_levels,
                 ", was detected in the variable",
                 name_disaggregations,
                 "but its value is not",
                 name_national))
    }
    if (is.null(name_national) == FALSE) {
      if (name_national %in% unique(ggbseg_point_estimates[, "cod"]) == FALSE) {
        stop(paste("The value",
                   name.national,
                   "was not found in the variable",
                   name.disaggregations))
      }
    }
    
    for (i in 1:n_disaggregations) {
      one_level <- all_levels[i]
      one_ylim <- c(0.9 * min(ggbseg_sensitivity_estimates[ggbseg_sensitivity_estimates$cod == one_level, "ggbseg"], na.rm=TRUE),
                    1.1 * max(ggbseg_sensitivity_estimates[ggbseg_sensitivity_estimates$cod == one_level, "ggbseg"], na.rm=TRUE))
      ## women
      g_sensitivity_females <- ggplot(data=ggbseg_sensitivity_estimates %>%
                                           filter(cod == one_level & 
                                                sex == "Females"),
                                      aes(x=lower_age_range,
                                          y=ggbseg))
      g_sensitivity_females <- g_sensitivity_females + 
                              geom_point(aes(col=upper_age_range),
                                         size=1.5,
                                         alpha=0.9) +
                              geom_hline(aes(yintercept=ggbseg_point_estimates %>% 
                                                         filter(cod == one_level & 
                                                                sex == "Females") %>% 
                                                         select(ggbseg) %>%
                                                         as.numeric(),
                                         linetype=""),
                                         size=1.2) +
                              scale_linetype_manual(name=" Point estimate",
                                                    values=c(1,1)) +
                              labs(x="Lower limit of age range",
                                  y=label.completeness,
                                  title=paste("Estimated completeness in\n",
                                              one_level,
                                              "-- Females"),
                                  col="Upper limit of age range") +
                             theme_classic(base_size=base.size.sensitivity) +
                             theme(legend.box="vertical",
                                   legend.text=element_text(size=rel(0.8))) +
                             coord_cartesian(ylim=one_ylim) 
    
      ## men
      g_sensitivity_males <- ggplot(data=ggbseg_sensitivity_estimates %>%
                                        filter(cod == one_level & 
                                              sex == "Males"),
                                      aes(x=lower_age_range,
                                          y=ggbseg))
     g_sensitivity_males <- g_sensitivity_males + 
                              geom_point(aes(col=upper_age_range),
                                          size=1.5,
                                          alpha=0.9) +
                              geom_hline(aes(yintercept=ggbseg_point_estimates %>% 
                                                        filter(cod == one_level & 
                                                                sex == "Males") %>% 
                                                        select(ggbseg) %>%
                                                        as.numeric(),
                                              linetype=""),
                                         size=1.2) +
                              scale_linetype_manual(name=" Point estimate",
                                                     values=c(1,1)) +
                              labs(x="Lower limit of age range",
                                    y=label.completeness,
                                    title=paste("Estimated completeness in\n",
                                                one_level,
                                                "-- Males"),
                                    col="Upper limit of age range") +
                               theme_classic(base_size=base.size.sensitivity) +
                               theme(legend.box="vertical",
                                     legend.text=element_text(size=rel(0.8))) +
                              coord_cartesian(ylim=one_ylim)
    
    
      g_sensitivity_combined <- ggarrange(g_sensitivity_females,
                                          g_sensitivity_males,
                                          nrow=1)  
      
      if (is.null(name_national) == FALSE) {
        if (one_level == name_national) {
          national_check <- TRUE
          disaggregated_plot_national <- g_sensitivity_combined
          i_national <- i 
        } else {
          list_plots_sensitivity[[i]] <- g_sensitivity_combined
        }
      } else {
        list_plots_sensitivity[[i]] <- g_sensitivity_combined
      }
    }
    if (national_check == TRUE) {
      list_plots_sensitivity[i_national] <- NULL ## removing the national level plot expected in the list 
    }
    if (n_disaggregations > 1) {
     overall <- marrangeGrob(list_plots_sensitivity,
                             nrow=fig.nrow,
                             ncol=fig.ncol)
    }
    ## print/save plots of sensitivity according to specified arguments 
    graphics.off()
    if (is.null(save.name.plots.sensitivity) == FALSE) {
      if (n_disaggregations > 1) {
        ggsave(paste0(save.name.plots.sensitivity, 
                      "_by_", name_disaggregations, "_", Sys.Date(), ".pdf"),
               overall)
      }
      if (national_check == TRUE) {
        ggsave(paste0(save.name.plots.sensitivity, 
                      "_", name_national, "_", Sys.Date(), ".pdf"),
               disaggregated_plot_national)
      }
    } else {
      if (n_disaggregations > 1) {
        ggsave(paste0(plots.dir, "ggbseg_sensitivity_", 
                      name_disaggregations, "_", Sys.Date(), ".pdf"),
               overall)
      }
      if (national_check == TRUE) {
        ggsave(paste0(plots.dir, "ggbseg_sensitivity_", 
                      name_national, "_", Sys.Date(), ".pdf"),
               disaggregated_plot_national)
      }
    }
  }
  # print/save plots of point estimates according to specified arguments
  if (is.null(save.name.plot.point.estimates) == FALSE) {
    if (n_disaggregations > 1) {
      ggsave(paste0(plots.dir, save.name.plots, 
                    "_combined", name_disaggregations, "_", Sys.Date(), ".pdf"),
             g_point_estimate)
    }
  } else {
    if (n_disaggregations > 1) {
      ggsave(paste0(plots.dir, "ggbseg_point_estimates_combined_", 
                    name_disaggregations, "_", Sys.Date(), ".pdf"),
             g_point_estimate)
    }
  }
}

