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
#' @param name.national A character string providing the value of `name.disaggregations` variable that indicates national-level results (e.g. "Overall" or "National"). Defaults to NULL, implying `name.disaggregations` variable in `data` only includes values for subnational levels. Defaults to NULL
#' @param label.subnational.level A character label for the legend showing level of subnational disaggregation present in the data. Defaults to `name.disaggregations` 
#' @param show.disaggregated.population A logical indicated whether the population in date2 should be displayed on the disaggreagted plots (in the title of the plot). Defaults to TRUE
#' @param ylim.disaggregated A vector with two numeric entries indicating the minimum and maximum values of the y-axis for the sex ratios plotted on a separate graph within each level of disaggregation. Default to NULL, which uses the smallest and largest sex ratios within each level
#' @param ylim.overall A vector with two numeric entries indicating the minimum and maximum values of the y-axis for the overall sex ratio where all levels of disaggregation are plotted on the same graph. Defaults to NULL,which uses the smallest and largest sex ratios in the entire dataset
#' @param line.size.disaggregated Numeric fed into ggplot2::geom_line(size)) for the disaggregated plots (i.e. sex ratio plotted separately within each level). Defaults to 0.8
#' @param line.size.overall Numeric fed into ggplot2::geom_line(size)) for the overall plot (i.e. sex ratios from all levels plotted on the same graph). Defaults to 0.6
#' @param fig.nrow.disaggregated An integer fed to `gridExtra::arrangeGrob(nrow)` to indicate how many rows should be used on each page to display the disaggregated plots. Defaults to 3
#' @param fig.ncol.disaggregated An integer fed to `gridExtra::arrangeGrob(ncol)` to indicate how many columns should be used on each page to display the disaggregated plots. Defaults to 2
#' @param fig.nrow.overall An integer fed to `gridExtra::arrangeGrob(nrow)` to indicate how many rows should be used on each page to display the overall plot. Defaults to 2
#' @param fig.ncol.overall An integer fed to `gridExtra::arrangeGrob(ncol)` to indicate how many columns should be used on each page to display the overall plot. Defaults to 1
#' @param save.name.disaggregated A character specifying a custom file name for the disaggregated plots saved on the local file system. Defaults to NULL, which combines `name.disaggregations` and the current date
#' @param save.name.overall A character specifying a custom file name for the overall plots saved on the local file system. Defaults to NULL, which combines `name.disaggregations` and the current date
#' @param plots.dir A character specifying the directory where plots should be saved. Defaults to "", saving the plots in the working directory
#' @examples 
#' ecuador_plot_sex_ratios <- PlotSexRatios(data=ecuador_single_year_ages,
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
#' @import scales
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
                          name.national=NULL,
                          label.subnational.level=name.disaggregations,
                          show.disaggregated.population=TRUE,
                          ylim.disaggregated=NULL,
                          ylim.overall=NULL,
                          line.size.disaggregated=0.8,
                          line.size.overall=0.6,
                          fig.nrow=3,
                          fig.ncol=2,
                          save.name.disaggregated=NULL,
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
  national_check <- FALSE
  
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
    if (show.disaggregated.population == TRUE) {
      pop1_one_level <- comma(sum(data_with_sex_ratio_one_level$pop1, na.rm=TRUE))
      pop2_one_level <- comma(sum(data_with_sex_ratio_one_level$pop2, na.rm=TRUE))
      x.sort <- sort(unique(g_one_level$data[, name.age]))
      g_one_level <- g_one_level + 
                     labs(x=name.age,
                          y="Sex ratio",
                          title=paste0("Sex ratio in\n", 
                                      one_level, 
                                      "\n",
                                      "(Pop: ",
                                      pop2_one_level,
                                      ")"))
    }
    if (is.null(name.national) == FALSE) {
      if (one_level == name.national) {
        national_check <- TRUE
        disaggregated_plot_national <- g_one_level
        i_national <- i  # needed so the level corresponding to national-level results is completely removed, not just storing the value NULL which gives error in marrangeGrob() function
      } else {
        list_plots[[i]] <- g_one_level
      }
    } else {
      list_plots[[i]] <- g_one_level
    }
    ylim.disaggregated <- NULL
  }
  if (national_check == TRUE) {
    list_plots[i_national] <- NULL ## removing the national level plot expected in the list 
  }
  if (n_disaggregations > 1) {
    arranged_plots <- marrangeGrob(list_plots, 
               nrow=fig.nrow, 
               ncol=fig.ncol)
  }
  
  # for each of the two Census years, create a plot showing sex ratios in the different levels of disaggregation
  if (n_disaggregations > 1) {
    if (is.null(name.national) == FALSE) {
      data_with_sex_ratio_for_overall <- data_with_sex_ratio[data_with_sex_ratio[, name.disaggregations] != name.national, ]
    } else {
      data_with_sex_ratio_for_overall <-  data_with_sex_ratio
    }
    if (is.null(ylim.overall)) {
      ylim.overall <- c(min(data_with_sex_ratio_for_overall[, c("sex_ratio_1", "sex_ratio_2")], na.rm=TRUE),
                        max(data_with_sex_ratio_for_overall[, c("sex_ratio_1", "sex_ratio_2")], na.rm=TRUE))
    }
    g_year1 <- ggplot(data=data_with_sex_ratio_for_overall,
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
    
    g_year2 <- ggplot(data=data_with_sex_ratio_for_overall,
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
    overall <- ggarrange(g_year2, g_year1,
                         nrow=2, ncol=1)
    graphics.off()
    if (is.null(save.name.overall) == FALSE) {
      ggsave(paste0(plots.dir, save.name.overall, "_", 
                    "combined_", name.disaggregations, Sys.Date(), ".pdf"),
             overall)
    } else {
      ggsave(paste0(plots.dir, "sex_ratios_combined_", 
                    name.disaggregations, "_", Sys.Date(), ".pdf"),
             overall)
    }
    graphics.off()
  }

  # print/save plots according to specified arguments
  graphics.off()
  if (is.null(save.name.disaggregated) == FALSE) {
    if (n_disaggregations > 1) {
        ggsave(paste0(plots.dir, save.name.disaggregated, 
                     "_by_", name.disaggregations, "_", Sys.Date(), ".pdf"),
                arranged_plots)
    }
    if (national_check == TRUE) {
      ggsave(paste0(plots.dir, save.name.disaggregated, 
                    "_", name.national, "_", Sys.Date(), ".pdf"),
             disaggregated_plot_national)
    }
  } else {
    if (n_disaggregations > 1) {
      ggsave(paste0(plots.dir, "sex_ratios_by_", 
                    name.disaggregations, "_", Sys.Date(), ".pdf"),
             arranged_plots)
    }
    if (national_check == TRUE) {
      ggsave(paste0(plots.dir, "sex_ratios_", 
                    name.national, "_", Sys.Date(), ".pdf"),
             disaggregated_plot_national) 
    }
  }
  return(data_with_sex_ratio)
}
