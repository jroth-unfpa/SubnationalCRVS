#' Plot sex ratios
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
#' @param ylim.disaggregated=NULL,
#' @param ylim.overall=NULL,
#' @param line.size.disaggregated=0.8,
#' @param line.size.overall=0.8,
#' @param line.size=1.1,
#' @param fig.nrow=3,
#' @param fig.ncol=2,
#' @param print.disaggregated=TRUE,
#' @param save.disaggregated=TRUE,
#' @param save.name_disaggregated=NULL,
#' @param print.overall=TRUE,
#' @param save.overall=TRUE,
#' @param save.name_overall=NULL,
#' @param plots.dir="
#' @examples 
#' @import dplyr
#' @import ggplot2
#' @import ggpubr
#' @import gridExtra
#' @export


PlotSexRatios <- function(data, 
                          name.disaggregations,
                          name.age,
                          name.sex,
                          name.males,
                          name.females,
                          name.date1,
                          name.date2,
                          name.population.year1,
                          name.population.year2,
                          ylim.disaggregated=NULL,
                          ylim.overall=NULL,
                          line.size.disaggregated=0.8,
                          line.size.overall=0.8,
                          line.size=1.1,
                          fig.nrow=3,
                          fig.ncol=2,
                          print.disaggregated=TRUE,
                          save.disaggregated=TRUE,
                          save.name_disaggregated=NULL,
                          print.overall=TRUE,
                          save.overall=TRUE,
                          save.name_overall=NULL,
                          plots.dir="") {
  # variable checks (should just call another function to do the checks that doesn't need to be documented)
  data[, name.disaggregations] <- as.factor(data[, name.disaggregations]) # should we requrie that the disaggregations are a factor variable with informative labels?
  if (length(unique(data[, name.date1])) != 1) {
    stop("date1 variable must contain only one unique value")
  }
  if (length(unique(data[, name.date2])) != 1) {
    stop("date2 variable must contain only one unique value")
  }
  date.1 <- data[1, name.date1]
  date.2 <- data[1, name.date2]
  
  # compute sex ratio within age groups and levels of disaggregation
  data_with_sex_ratio <- ComputeSexRatios(data=data,
                                          name.disaggregations=name.disaggregations,
                                          name.males=name.males,
                                          name.females=name.females,
                                          name.age=name.age,
                                          name.sex=name.sex,
                                          name.date1=name.date1,
                                          name.date2=name.date2,
                                          name.population.year1=name.population.year1,
                                          name.population.year2=name.population.year2)
  
  # for each level of disaggregation, create a plot showing sex ratios in year1 and year2
  all_levels <- unique(levels(data[, name.disaggregations]))
  n_disaggregations <- length(all_levels)
  list_plots <- vector("list", length=n_disaggregations)
  for (i in 1:n_disaggregations) {
    one_level <- all_levels[i]
    data_with_sex_ratio_one_level <- data_with_sex_ratio %>% 
                                     filter(get(name.disaggregations) == one_level)
    if (is.null(ylim.disaggregated)) {
      ylim.disaggregated <- c(min(data_with_sex_ratio_one_level[, c("sex_ratio_1", "sex_ratio_2")], na.rm=TRUE),
                max(data_with_sex_ratio_one_level[, c("sex_ratio_1", "sex_ratio_2")], na.rm=TRUE))

    }
    g_one_level <- ggplot(data=data_with_sex_ratio_one_level,
                                 aes(x=get(name.age)))
    g_one_level <- g_one_level + 
                    geom_line(aes(y=sex_ratio_1,
                                  col="orange"),
                              size=line.size.disaggregated) + 
                    geom_line(aes(y=sex_ratio_2,
                                  col="blue"),
                              size=line.size.disaggregated) + 
                 geom_hline(yintercept=100,
                            size=line.size.disaggregated) +
                 coord_cartesian(ylim=ylim.disaggregated) +
                 labs(x=name.age,
                      y="Sex ratio",
                      title=paste("Sex ratio in", name.disaggregations, one_level)) +
                  scale_colour_manual(name="", 
                                      values=c("orange"="orange",
                                               "blue"="blue"), 
                                      labels = c(date.1,
                                                 date.2))
    list_plots[[i]] <- g_one_level
    ylim.disaggregated <- NULL
  }
  arranged_plots <- marrangeGrob(list_plots, 
               nrow=fig.nrow, 
               ncol=fig.ncol)
  
  # for each of the two Census years, create a plot showing sex ratios in the different levels of disaggregation
  if (is.null(ylim.overall)) {
    ylim.overall <- c(min(data_with_sex_ratio[, c("sex_ratio_1", "sex_ratio_2")], na.rm=TRUE),
                      max(data_with_sex_ratio[, c("sex_ratio_1", "sex_ratio_2")], na.rm=TRUE))
  }
  g_year1 <- ggplot(data=data_with_sex_ratio,
              aes(x=get(name.age),
                  y=sex_ratio_1))
  g_year1 <- g_year1 + geom_line(aes(col=get(name.disaggregations)),
                                 size=line.size.overall) +
      geom_hline(yintercept=100,
                 size=line.size.overall) +
    coord_cartesian(ylim=ylim.overall) +
      labs(x=name.age,
           y="Sex ratio",
           title=paste0("Sex ratio \n", date.1)) +
      scale_colour_discrete(name=name.disaggregations)
  
  g_year2 <- ggplot(data=data_with_sex_ratio,
                    aes(x=get(name.age),
                        y=sex_ratio_2))
  g_year2 <- g_year2 + geom_line(aes(col=get(name.disaggregations)),
                                 size=line.size.overall) +
    geom_hline(yintercept=100,
               size=line.size.overall) +
    coord_cartesian(ylim=ylim.overall) +
    labs(x=name.age,
         y="Sex ratio",
         title=paste0("Sex ratio \n",  date.2)) +
    scale_colour_discrete(name=name.disaggregations)

  # print/save plots according to specified arguments
  graphics.off()
  if (print.disaggregated == TRUE) {
    print(arranged_plots)
    graphics.off()
  } 
  if (save.disaggregated == TRUE) {
    if (is.null(save.name_disaggregated) == FALSE) {
      pdf(paste0(save.name_disaggregated, ".pdf")) 
    } else {
      pdf(paste0(plots.dir, "sex_ratios_by_", 
                 name.disaggregations, "_", Sys.Date(), ".pdf"))
    }
    print(arranged_plots)
    graphics.off()
  }
  if (save.overall == TRUE) {
    if (is.null(save.name_overall) == FALSE) {
      pdf(paste0(save.name_overall, ".pdf")) 
    } else {
      pdf(paste0(plots.dir, "sex_ratios_combined_", 
                 name.disaggregations, "_", Sys.Date(), ".pdf"))
    }
    overall <- ggarrange(g_year2, g_year1,
                         nrow=2, ncol=1)
    print(overall)
    graphics.off()
  }
  return(data_with_sex_ratio)
}
