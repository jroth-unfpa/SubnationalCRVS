#' Plot sex ratios
#'
#' asdf
#' 
#' @param data data frame that contains at least seven columns representing: (1) five-year age groups,
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
#' @param label.subnational.level A character label for the legend showing level of subnational disaggregation present in the data. Defaults to `name.disaggregations` 
#' @param ylim.disaggregated A vector with two numeric entries indicating the minimum and maximum values of the y-axis for the sex ratios plotted on a separate graph within each level of disaggregation. Default to NULL, which uses the smallest and largest sex ratios within each level
#' @param ylim.overall A vector with two numeric entries indicating the minimum and maximum values of the y-axis for the overall sex ratio where all levels of disaggregation are plotted on the same graph. Defaults to NULL,which uses the smallest and largest sex ratios in the entire dataset
#' @param line.size.disaggregated Numeric fed into ggplot2::geom_line(size)) for the disaggregated plots (i.e. sex ratio plotted separately within each level). Defaults to 0.8
#' @param line.size.overall Numeric fed into ggplot2::geom_line(size)) for the overall plot (i.e. sex ratios from all levels plotted on the same graph). Defaults to 0.8
#' @param fig.nrow.disaggregated An integer fed to `gridExtra::arrangeGrob(nrow)` to indicate how many rows should be used on each page to display the disaggregated plots. Defaults to 3
#' @param fig.ncol.disaggregated An integer fed to `gridExtra::arrangeGrob(ncol)` to indicate how many columns should be used on each page to display the disaggregated plots. Defaults to 2
#' @param fig.nrow.overall An integer fed to `gridExtra::arrangeGrob(nrow)` to indicate how many rows should be used on each page to display the overall plot. Defaults to 2
#' @param fig.ncol.overall An integer fed to `gridExtra::arrangeGrob(ncol)` to indicate how many columns should be used on each page to display the overall plot. Defaults to 1
#' @param print.disaggregated A logical indicating whether the disaggregated plots should be printed in the R session. Defaults to FALSE
#' @param save.disaggregated A logical indicating whether the disaggregated plots should be saved on the local file system. Defaults to TRUE
#' @param save.name.disaggregated A character specifying a custom file name for the disaggregated plots saved on the local file system. Defaults to NULL, which combines `name.disaggregations` and the current date
#' @param print.overall A logical indicating whether the overall plots should be printed in the R session. Defaults to TRUE
#' @param save.overall A logical indicating whether the overall plots should be saved on the local file system. Defaults to TRUE
#' @param save.name.overall A character specifying a custom file name for the overall plots saved on the local file system. Defaults to NULL, which combines `name.disaggregations` and the current date
#' @param plots.dir A character specifying the directory where plots should be saved. Defaults to "", saving the plots in the working directory
#' @examples 
#' ecuador_plot_sex_ratios <- PlotSexRatios(data=example_data_ecuador,
#'                                          name.disaggregations="province_name",
#'                                          name.males="m",
#'                                          name.females="f",
#'                                          name.age="age",
#'                                          name.sex="sex",
#'                                          name.date1="date1",
#'                                          name.date2="date2",
#'                                          name.population.year1="pop1",
#'                                          name.population.year2="pop2",
#'                                          label.subnational.level="Province")
#' head(ecuador_plot_sex_ratios)
#' tail(ecuador_plot_sex_ratios)
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
                          label.subnational.level=name.disaggregations,
                          ylim.disaggregated=NULL,
                          ylim.overall=NULL,
                          line.size.disaggregated=0.8,
                          line.size.overall=0.8,
                          fig.nrow=3,
                          fig.ncol=2,
                          print.disaggregated=TRUE,
                          save.disaggregated=TRUE,
                          save.name.disaggregated=NULL,
                          print.overall=TRUE,
                          save.overall=TRUE,
                          save.name.overall=NULL,
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
                      title=paste("Sex ratio in\n", one_level)) +
                  scale_colour_manual(name="Date", 
                                      values=c("orange"="orange",
                                               "blue"="blue"), 
                                      labels = c(date.1,
                                                 date.2)) +
                  theme_classic()
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
      scale_colour_discrete(name=label.subnational.level) +
      theme_classic()
  
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
    scale_colour_discrete(name=label.subnational.level) +
    theme_classic()

  # print/save plots according to specified arguments
  graphics.off()
  if (print.disaggregated == TRUE) {
    print(arranged_plots)
    graphics.off()
  } 
  if (save.disaggregated == TRUE) {
    if (is.null(save.name.disaggregated) == FALSE) {
            pdf(paste0(plots.dir, save.name.disaggregated, 
                       "_by_", name.disaggregations, "_", Sys.Date(), ".pdf"))

    } else {
      pdf(paste0(plots.dir, "sex_ratios_by_", 
                 name.disaggregations, "_", Sys.Date(), ".pdf"))
    }
    print(arranged_plots)
    graphics.off()
  }
  if (save.overall == TRUE) {
    if (is.null(save.name.overall) == FALSE) {
      pdf(paste0(plots.dir, save.name.overall, "_", 
                 "combined_", name.disaggregations, Sys.Date(), ".pdf"))
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
