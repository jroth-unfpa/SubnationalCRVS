#' Plot DDM estimates and their sensitivity to choice of age range
#'
#' asdf
#' @param ddm_results asdf
#' @param size.text.sensitivity
#' @param fig.nrow
#' @param fig.ncol
#' @param print.plot.point.estimates
#' @param save.plot.point.estimates
#' @param save.name.plot.point.estimates
#' @param print.plots.sensitivity
#' @param save.plots.sensitivity
#' @param plots.dir
#' @examples
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
  if (ddm_results$show_age_range_sensitivity == TRUE) {
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
                                 title=paste(name_disaggregations,
                                              one_level,
                                              "\n-- Females"),
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
                                  title=paste(name_disaggregations,
                                              one_level,
                                              "\n-- Males"),
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
  }
  # print/save plots according to specified arguments
  ## point estimates
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
  ## sensitivity 
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
  }
  if (print.plots.sensitivity == TRUE) {
    print(overall)
  } 
}
