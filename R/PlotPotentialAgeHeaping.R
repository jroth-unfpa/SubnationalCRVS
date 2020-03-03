#' Plot counts of single-year ages to reveal potential age heaping
#'
#' asdf
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
#' @param confirm_single_year_ages Logical indicating whether (in contrast to result of variable checks) the `name.age` does in fact represent single-year ages and the error thrown by the variable checks should be overwritten. Default is FALSE
#' @param mark_multiples_of_5_disaggregated Logical indicating whether dashed vertical lines should be overlaid on the disaggregated (i.e. separate plot within each level of disaggregation) plots at ages that are multiples of 5. Defaults to FALSE
#' @param mark_multiples_of_5_overall Logical indicating whether dashed vertical lines should be overlaid on the overall (i.e. all levels of disaggregation are plotted on the same graph) plots at ages that are multiples of 5. Defaults to TRUE
#' @param ylim.disaggregated A vector with two numeric entries indicating the minimum and maximum values of the y-axis for the age-specific population counts plotted on a separate graph within each level of disaggregation. Defaults to NULL, which uses the smallest and largest counts within each level
#' @param ylim.overall A vector with two numeric entries indicating the minimum and maximum values of the y-axis for the overall age-specific population counts where all levels of disaggregation are plotted on the same graph. Defaults to NULL, which uses the smallest and largest counts in the entire dataset
#' @param line.size.disaggregated Numeric fed into ggplot2::geom_line(size)) for the disaggregated plots. Defaults to 0.6
#' @param line.size.overall Numeric fed into ggplot2::geom_line(size)) for the overall plot. Defaults to 0.6
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
#' PlotPotentialAgeHeaping(data=ecuador_age_tabulation,
#'                         name.disaggregations="province_name_short",
#'                         name.males="m",
#'                         name.females="f",
#'                         name.age="age",
#'                         name.sex="sex",
#'                         name.date1="date1",
#'                         name.date2="date2",
#'                         name.population.year1="pop1",
#'                         name.population.year2="pop2")
#' @import ggplot2
#' @import ggpubr
#' @import gridExtra
#' @export

PlotPotentialAgeHeaping <- function(data, 
                          name.disaggregations,
                          name.age,
                          name.sex,
                          name.males,
                          name.females,
                          name.date1,
                          name.date2,
                          name.population.year1,
                          name.population.year2,
                          confirm_single_year_ages=FALSE,
                          mark_multiples_of_5_disaggregated=FALSE,
                          mark_multiples_of_5_overall=TRUE,
                          ylim.disaggregated=NULL,
                          ylim.overall=NULL,
                          line.size.disaggregated=0.6,
                          line.size.overall=0.6,
                          fig.nrow.disaggregated=3,
                          fig.ncol.disaggregated=2,
                          fig.nrow.overall=2,
                          fig.ncol.overall=1,
                          print.disaggregated=FALSE,
                          save.disaggregated=TRUE,
                          save.name_disaggregated=NULL,
                          print.overall=FALSE,
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
  
  # verify that the age variable is single-year ages and not groups of multiple ages (e.g. 5-year age groups)
  # and also emphasize that only the "deaths" column is actually not required
  CheckSingleYearAges(data,
                      name.disaggregations=name.disaggregations,
                      name.sex=name.sex,
                      confirm_single_year_ages=confirm_single_year_ages) 
  # convert data into long format (more convenient for ggplot2)
  long_year1 <- data %>% 
    select(name.disaggregations,
           name.sex, 
           name.age,
           name.population.year1,
           name.date1) %>%
    rename(sex=name.sex,
           pop=pop1,
           date=date1)
  
  long_year2 <- data %>% 
    select(name.disaggregations,
           name.sex, 
           name.age,
           name.population.year2,
           name.date2) %>%
    rename(sex=name.sex,
           pop=pop2,
           date=date2)
  data_long <- rbind(long_year1, long_year2)
  
  # for each level of disaggregation, create a plot in year1 and year2
  all_levels <- unique(levels(data[, name.disaggregations]))
  n_disaggregations <- length(all_levels)
  list_plots <- vector("list", length=n_disaggregations)
  for (i in 1:n_disaggregations) {
    one_level <- all_levels[i]
    data_one_level <- data_long %>% 
                      filter(get(name.disaggregations) == one_level)
    if (is.null(ylim.disaggregated)) {
      ylim.disaggregated <- c(min(data_one_level$pop, na.rm=TRUE),
                              max(data_one_level$pop, na.rm=TRUE))
    }
    g_one_level <- ggplot(data=data_one_level,
                          aes(x=get(name.age)))
    g_one_level <- g_one_level + 
      geom_line(aes(y=pop,
                    col=sex,
                    linetype=date),
                size=line.size.disaggregated) 
    if (mark_multiples_of_5_disaggregated == TRUE) {
        g_one_level <- g_one_level + 
                       geom_vline(xintercept=seq(from=0, 
                                                 to=round(max(data_one_level[, name.age]/5)*5), 
                                                 by=5),
                                  col="darkgray",
                                  linetype="dashed",
                                  size=line.size.disaggregated)
      }
     g_one_level <-  g_one_level + 
      coord_cartesian(ylim=ylim.disaggregated) +
      labs(x=name.age,
           y="estimated population",
           title=paste("estimated population by age \n in", one_level)) + 
       theme_classic()
    
        list_plots[[i]] <- g_one_level
    ylim.disaggregated <- NULL
  }
  arranged_plots <- marrangeGrob(list_plots, 
                                 nrow=fig.nrow.disaggregated, 
                                 ncol=fig.ncol.disaggregated)
  
  # for each of the two Census years, create a plot for the different levels of disaggregation
  if (is.null(ylim.overall)) {
    ylim.overall <- c(min(data_long$pop, na.rm=TRUE),
                      max(data_long$pop, na.rm=TRUE))
  }
  ## Census year 1
  ### males
  g_year1_males <- ggplot(data=data_long %>%
                               filter(sex == name.males & date == date.1),
                          aes(x=get(name.age),
                          y=pop))
  g_year1_males <- g_year1_males + 
                  geom_line(aes(col=get(name.disaggregations)),
                                size=line.size.overall)
  if (mark_multiples_of_5_overall == TRUE) {
     g_year1_males <- g_year1_males + 
        geom_vline(xintercept=seq(from=0, 
                                  to=round(max(data_one_level[, name.age]/5)*5), 
                                  by=5),
                   col="darkgray",
                   linetype="dashed",
                   size=line.size.overall)
    }
   g_year1_males <- g_year1_males + coord_cartesian(ylim=ylim.overall) +
    labs(x=name.age,
         y="estimated population",
         title=paste0("males -- estimated population \n", date.1)) +
    scale_colour_discrete(name=name.disaggregations) +
    theme_classic()
   
   ### females
   g_year1_females <- ggplot(data=data_long %>%
                             filter(sex == name.females & date == date.1),
                           aes(x=get(name.age),
                               y=pop))
   g_year1_females <- g_year1_females + 
     geom_line(aes(col=get(name.disaggregations)),
               size=line.size.overall)
   if (mark_multiples_of_5_overall == TRUE) {
     g_year1_females <- g_year1_females + 
       geom_vline(xintercept=seq(from=0, 
                                 to=round(max(data_one_level[, name.age]/5)*5), 
                                 by=5),
                  col="darkgray",
                  linetype="dashed",
                  size=line.size.overall)
   }
   g_year1_females <- g_year1_females + coord_cartesian(ylim=ylim.overall) +
     labs(x=name.age,
          y="estimated population",
          title=paste0("females -- estimated population \n", date.1)) +
     scale_colour_discrete(name=name.disaggregations) +
     theme_classic()
  
   ## Census year 2
   ### males
   g_year2_males <- ggplot(data=data_long %>%
                             filter(sex == name.males & date == date.2),
                           aes(x=get(name.age),
                               y=pop))
   g_year2_males <- g_year2_males + 
     geom_line(aes(col=get(name.disaggregations)),
               size=line.size.overall)
   if (mark_multiples_of_5_overall == TRUE) {
     g_year2_males <- g_year2_males + 
       geom_vline(xintercept=seq(from=0, 
                                 to=round(max(data_one_level[, name.age]/5)*5), 
                                 by=5),
                  col="darkgray",
                  linetype="dashed",
                  size=line.size.overall)
   }
   g_year2_males <- g_year2_males + coord_cartesian(ylim=ylim.overall) +
     labs(x=name.age,
          y="estimated population",
          title=paste0("males -- estimated population \n", date.2)) +
     scale_colour_discrete(name=name.disaggregations) +
     theme_classic()

   ### females
   g_year2_females <- ggplot(data=data_long %>%
                               filter(sex == name.females & date == date.2),
                             aes(x=get(name.age),
                                 y=pop))
   g_year2_females <- g_year2_females + 
     geom_line(aes(col=get(name.disaggregations)),
               size=line.size.overall)
   if (mark_multiples_of_5_overall == TRUE) {
     g_year2_females <- g_year2_females + 
       geom_vline(xintercept=seq(from=0, 
                                 to=round(max(data_one_level[, name.age]/5)*5), 
                                 by=5),
                  col="darkgray",
                  linetype="dashed",
                  size=line.size.overall)
   }
   g_year2_females <- g_year2_females + coord_cartesian(ylim=ylim.overall) +
     labs(x=name.age,
          y="estimated population",
          title=paste0("females -- estimated population \n", date.2)) +
     scale_colour_discrete(name=name.disaggregations) +
     theme_classic()
   
   
    list_plots_overall <- list(g_year2_females, 
                               g_year2_males,
                               g_year1_females, 
                               g_year1_males)
    overall <- marrangeGrob(list_plots_overall,
                            nrow=fig.nrow.overall,
                            ncol=fig.ncol.overall)
  
  # print/save plots according to specified arguments
  if (save.disaggregated == TRUE) {
    if (is.null(save.name_disaggregated) == FALSE) {
      pdf(paste0(save.name_disaggregated, 
                 "_by_", name.disaggregations, "_", Sys.Date(), ".pdf")) 
    } else {
      pdf(paste0(plots.dir, "potential_age_heaping_by_", 
                 name.disaggregations, "_", Sys.Date(), ".pdf"))
    }
    print(arranged_plots)
    for (i in 1:10) graphics.off()
  }
  graphics.off()
  if (save.overall == TRUE) {
    if (is.null(save.name_overall) == FALSE) {
      pdf(paste0(save.name_overall, 
                 "_combined_", name.disaggregations, "_", Sys.Date(), ".pdf"))
    } else {
      pdf(paste0(plots.dir, "potential_age_heaping_combined_", 
                 name.disaggregations, "_", Sys.Date(), ".pdf"))
    }
    print(overall)
    graphics.off()
  }
  if (print.disaggregated == TRUE) {
    print(arranged_plots)
    graphics.off()
  } 
  if (print.overall == TRUE) {
    print(overall)
  } 
}
