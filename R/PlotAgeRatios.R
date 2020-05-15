#' Plot age ratios
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
#' @param name.population.year1 Character string providing the name of the variable in `data` that represents the population count in the earlier time period
#' @param name.population.year2 Character string providing the name of the variable in `data` that represents the population count in the later time period
#' @param name.year1 Character string providing the name of the variable in `data` that represents the year of the earlier of the two time periods (e.g. year of the earlier Census)
#' @param name.month1 Character string providing the name of the variable in `data` that represents the month of the earlier of the two time periods (e.g. month of the earlier Census)
#' @param name.day1 Character string providing the name of the variable in `data` that represents the day of the earlier of the two time periods (e.g. day of the earlier Census)
#' @param name.year2 Character string providing the name of the variable in `data` that represents the year of the later of the two time periods (e.g. year of the later Census)
#' @param name.month2 Character string providing the name of the variable in `data` that represents the month of the later of the two time periods (e.g. month of the later Census)
#' @param name.day2 Character string providing the name of the variable in `data` that represents the day of the later of the two time periods (e.g. day of the later Census)
#' @param name.national A character string providing the value of `name.disaggregations` variable that indicates national-level results (e.g. "Overall" or "National"). Defaults to NULL, implying `name.disaggregations` variable in `data` only includes values for subnational levels. Defaults to NULL
#' @param label.subnational.level A character label for the legend showing level of subnational disaggregation present in the data. Defaults to `name.disaggregations` 
#' @param show.disaggregated.population A logical indicated whether the population in date2 should be displayed on the disaggreagted plots (in the title of the plot). Defaults to TRUE#' 
#' @param ylim.disaggregated A vector with two numeric entries indicating the minimum and maximum values of the y-axis for the age ratios plotted on a separate graph within each level of disaggregation. Default to NULL, which uses the smallest and largest age ratios within each level
#' @param ylim.overall A vector with two numeric entries indicating the minimum and maximum values of the y-axis for the overall age ratio where all levels of disaggregation are plotted on the same graph. Defaults to NULL,which uses the smallest and largest age ratios in the entire dataset
#' @param line.size.disaggregated Numeric fed into ggplot2::geom_line(size)) for the disaggregated plots (i.e. age ratio plotted separately within each level). Defaults to 0.6
#' @param line.size.overall Numeric fed into ggplot2::geom_line(size)) for the overall plot (i.e. age ratios from all levels plotted on the same graph). Defaults to 0.6
#' @param fig.nrow.disaggregated An integer fed to `gridExtra::arrangeGrob(nrow)` to indicate how many rows should be used on each page to display the disaggregated plots. Defaults to 3
#' @param fig.ncol.disaggregated An integer fed to `gridExtra::arrangeGrob(ncol)` to indicate how many columns should be used on each page to display the disaggregated plots. Defaults to 2
#' @param fig.nrow.overall An integer fed to `gridExtra::arrangeGrob(nrow)` to indicate how many rows should be used on each page to display the overall plot. Defaults to 2
#' @param fig.ncol.overall An integer fed to `gridExtra::arrangeGrob(ncol)` to indicate how many columns should be used on each page to display the overall plot. Defaults to 1
#' @param save.name.disaggregated A character specifying a custom file name for the disaggregated plots saved on the local file system. Defaults to NULL, which combines `name.disaggregations` and the current date
#' @param save.name.overall A character specifying a custom file name for the overall plots saved on the local file system. Defaults to NULL, which combines `name.disaggregations` and the current date
#' @param plots.dir A character specifying the directory where plots should be saved. Defaults to "", saving the plots in the working directory
#' @examples 
#' ecuador_plot_age_ratios <- PlotAgeRatios(data=ecuador_five_year_ages,
#'                                          name.disaggregations="province_name",
#'                                          name.males="m",
#'                                          name.females="f",
#'                                          name.age="age",
#'                                          name.sex="sex",
#'                                          name.population.year1="pop1",
#'                                          name.population.year2="pop2",
#'                                          name.year1="year1"
#'                                          name.month1="month1",
#'                                          name.day1="day1",
#'                                          name.year2="year2",
#'                                          name.month2="month2",
#'                                          name.day2="day2"
#'                                          label.subnational.level="Province")
#' head(ecuador_plot_age_ratios)
#' tail(ecuador_plot_age_ratios)
#' @import dplyr
#' @import ggplot2
#' @import ggpubr
#' @import gridExtra
#' @import scales
#' @export

PlotAgeRatios <- function(data, 
                          name.disaggregations,
                          name.age,
                          name.sex,
                          name.males,
                          name.females,
                          name.population.year1,
                          name.population.year2,
                          name.year1,
                          name.month1,
                          name.day1,
                          name.year2,
                          name.month2,
                          name.day2,
                          name.national=NULL,
                          label.subnational.level=name.disaggregations,
                          show.disaggregated.population=TRUE,
                          ylim.disaggregated=NULL,
                          ylim.overall=NULL,
                          line.size.disaggregated=0.6,
                          line.size.overall=0.6,
                          fig.nrow.disaggregated=3,
                          fig.ncol.disaggregated=2,
                          fig.nrow.overall=2,
                          fig.ncol.overall=1,
                          save.name.disaggregated=NULL,
                          save.name.overall=NULL,
                          plots.dir="") {
  # variable checks (should just call another function to do the checks that doesn't need to be documented)
  data[, name.disaggregations] <- as.factor(data[, name.disaggregations]) # should we requrie that the disaggregations are a factor variable with informative labels?
  
  # compute age ratio within age groups and levels of disaggregation
  data_with_age_ratio <- ComputeAgeRatios(data=data,
                                          name.disaggregations=name.disaggregations,
                                          name.males=name.males,
                                          name.females=name.females,
                                          name.age=name.age,
                                          name.sex=name.sex,
                                          name.population.year1=name.population.year1,
                                          name.population.year2=name.population.year2,
                                          name.year1=name.year1,
                                          name.month1=name.month1,
                                          name.day1=name.day1,
                                          name.year2=name.year2,
                                          name.month2=name.month2,
                                          name.day2=name.day2)
  date.1 <- as.character(unique(data_with_age_ratio$date1))
  date.2 <- as.character(unique(data_with_age_ratio$date2))
  
  
  # convert data into long format (more convenient for ggplot2)
  long_year1 <- data_with_age_ratio %>% 
    select(name.disaggregations,
           name.sex, 
           name.age,
           name.population.year1,
           date1,
           age_ratio_1) %>%
    rename(sex=name.sex,
           pop=pop1,
           date=date1,  # name comes from CreateDateVariable()
           age_ratio=age_ratio_1)
  
  long_year2 <- data_with_age_ratio %>% 
    select(name.disaggregations,
           name.sex, 
           name.age,
           name.population.year2,
           date2, # name comes from CreateDateVariable()
           age_ratio_2) %>%
    rename(sex=name.sex,
           pop=pop2,
           date=date2,
           age_ratio=age_ratio_2)
  data_with_age_ratio_long <- rbind(long_year1, long_year2)
  data_with_age_ratio_long$date <- as.factor(data_with_age_ratio_long$date) # ggplot has issues with Date class
  
  # for each level of disaggregation, create a plot showing age ratios in year1 and year2
  all_levels <- unique(levels(data[, name.disaggregations]))
  n_disaggregations <- length(all_levels)
  list_plots <- vector("list", length=n_disaggregations)
  national_check <- FALSE
  if (n_disaggregations == 1 & is.null(name.national) == TRUE) {
    stop(paste("Only one level of disaggregation,",
               all_levels,
               ", was detected in the variable",
               name.disaggregations,
               "but its value is not",
               name.national))
  }
  if (is.null(name.national) == FALSE) {
    if (name.national %in% unique(data[, name.disaggregations]) == FALSE) {
      stop(paste("The value",
                 name.national,
                 "was not found in the variable",
                 name.disaggregations))
    }
  }

    for (i in 1:n_disaggregations) {
    one_level <- all_levels[i]
    data_with_age_ratio_long_one_level <- data_with_age_ratio_long %>% 
      filter(get(name.disaggregations) == one_level)
    if (is.null(ylim.disaggregated)) {
      ylim.disaggregated <- c(min(data_with_age_ratio_long_one_level$age_ratio, na.rm=TRUE),
                              max(data_with_age_ratio_long_one_level$age_ratio, na.rm=TRUE))
      
    }
    g_one_level <- ggplot(data=data_with_age_ratio_long_one_level,
                          aes(x=get(name.age)))
    g_one_level <- g_one_level + 
      geom_line(aes(y=age_ratio,
                    col=sex,
                    linetype=date),
                size=line.size.disaggregated) + 
      geom_hline(yintercept=100,
                 size=line.size.disaggregated) +
      coord_cartesian(ylim=ylim.disaggregated) +
      labs(x=name.age,
           y="Age ratio",
           title=paste("Age ratio in\n", one_level)) +
      theme_classic() +
      scale_color_discrete(name="Sex") +
      scale_linetype_discrete(name="Date")
    
    if (show.disaggregated.population == TRUE) {
      pop1_one_level <- comma(sum(data_with_age_ratio_long_one_level[data_with_age_ratio_long_one_level$date == date.1,
                                         "pop"], 
                                   na.rm=TRUE))
      pop2_one_level <- comma(sum(data_with_age_ratio_long_one_level[data_with_age_ratio_long_one_level$date == date.2,
                                                                     "pop"], 
                                  na.rm=TRUE))
      
      x.sort <- sort(unique(g_one_level$data[, name.age]))
      g_one_level <- g_one_level + 
        labs(x=name.age,
             y="Sex ratio",
             title=paste0("Age ratio in\n", 
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
                                   nrow=fig.nrow.disaggregated, 
                                   ncol=fig.ncol.disaggregated)
  }
  
  # for each of the two Census years, create a plot showing age ratios in the different levels of disaggregation
  if (n_disaggregations > 1) { 
    if (is.null(name.national) == FALSE) {
      data_with_age_ratio_long_for_overall <- data_with_age_ratio_long[data_with_age_ratio_long[, name.disaggregations] != name.national, ]
    } else {
     data_with_age_ratio_long_for_overall <-  data_with_age_ratio_long
    }
    if (is.null(ylim.overall)) {
      ylim.overall <- c(min(data_with_age_ratio_long_for_overall$age_ratio, na.rm=TRUE),
                        max(data_with_age_ratio_long_for_overall$age_ratio, na.rm=TRUE))
    }
    ## Census year 1
    g_year1_males <- ggplot(data=data_with_age_ratio_long_for_overall %>%
                              filter(sex == name.males & date == date.1),
                            aes(x=get(name.age),
                                y=age_ratio))
    g_year1_males <- g_year1_males + geom_line(aes(col=get(name.disaggregations)),
                                               size=line.size.overall) +
      geom_hline(yintercept=100,
                 size=line.size.overall) +
      coord_cartesian(ylim=ylim.overall) +
      labs(x=name.age,
           y="age ratio",
           title=paste0("males -- age ratio \n", date.1)) +
      scale_colour_discrete(name=label.subnational.level) +
      theme_classic()
    
    g_year1_females <- ggplot(data=data_with_age_ratio_long_for_overall %>%
                                filter(sex == name.females & date == date.1),
                              aes(x=get(name.age),
                                  y=age_ratio))
    g_year1_females <- g_year1_females + geom_line(aes(col=get(name.disaggregations)),
                                                   size=line.size.overall) +
      geom_hline(yintercept=100,
                 size=line.size.overall) +
      coord_cartesian(ylim=ylim.overall) +
      labs(x=name.age,
           y="age ratio",
           title=paste0("females -- age ratio \n", date.1)) +
      scale_colour_discrete(name=label.subnational.level) +
      theme_classic()
    
    
    ## Census year 2
    g_year2_males <- ggplot(data=data_with_age_ratio_long_for_overall %>%
                              filter(sex == name.males & date == date.2),
                            aes(x=get(name.age),
                                y=age_ratio))
    g_year2_males <- g_year2_males + geom_line(aes(col=get(name.disaggregations)),
                                               size=line.size.overall) +
      geom_hline(yintercept=100,
                 size=line.size.overall) +
      coord_cartesian(ylim=ylim.overall) +
      labs(x=name.age,
           y="age ratio",
           title=paste0("males -- age ratio \n", date.2)) +
      scale_colour_discrete(name=label.subnational.level) +
      theme_classic()
    
    g_year2_females <- ggplot(data=data_with_age_ratio_long_for_overall %>%
                                filter(sex == name.females & date == date.2),
                              aes(x=get(name.age),
                                  y=age_ratio))
    g_year2_females <- g_year2_females + geom_line(aes(col=get(name.disaggregations)),
                                                   size=line.size.overall) +
      geom_hline(yintercept=100,
                 size=line.size.overall) +
      coord_cartesian(ylim=ylim.overall) +
      labs(x=name.age,
           y="age ratio",
           title=paste0("females -- age ratio \n", date.2)) +
      scale_colour_discrete(name=label.subnational.level) +
      theme_classic()
    
    list_plots_overall <- list(g_year2_females, 
                               g_year2_males,
                               g_year1_females, 
                               g_year1_males)
    overall <- marrangeGrob(list_plots_overall,
                            nrow=fig.nrow.overall,
                            ncol=fig.ncol.overall)
    graphics.off()
    if (is.null(save.name.overall) == FALSE) {
      ggsave(paste0(plots.dir, save.name_overall, "_", name.disaggregations, "_",
                 Sys.Date(), ".pdf"),
             overall)
    } else {
      ggsave(paste0(plots.dir, "age_ratios_combined_", 
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
      ggsave(paste0(plots.dir, "age_ratios_by_", 
                    name.disaggregations, "_", Sys.Date(), ".pdf"),
            arranged_plots)
    }
    if (national_check == TRUE) {
      ggsave(paste0(plots.dir, "age_ratios_", 
                    name.national, "_", Sys.Date(), ".pdf"),
             disaggregated_plot_national) 
    }
  }
  for (i in 1:10) graphics.off()

  return(data_with_age_ratio)
}


