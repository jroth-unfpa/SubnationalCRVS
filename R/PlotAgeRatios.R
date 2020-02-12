#' Plot age ratios
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
#' @param ylim.disaggregated asdf
#' @param ylim.overall asdf
#' @param line.size.disaggregated asdf
#' @param line.size.overall asdf
#' @param fig.nrow.disaggregated asdf
#' @param fig.ncol.disaggregated asdf
#' @param fig.nrow.overall asdf
#' @param fig.ncol.overall asdf
#' @param print.disaggregated asdf 
#' @param save.disaggregated asdf
#' @param save.name_disaggregated=NULL,
#' @param print.overall=TRUE,
#' @param save.overall=TRUE,
#' @param save.name_overall=NULL,
#' @param plots.dir asdf
#' @import dplyr
#' @import ggplot2
#' @import ggpubr
#' @import gridExtra
#' @export

PlotAgeRatios <- function(data, 
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
                          fig.nrow.disaggregated=3,
                          fig.ncol.disaggregated=2,
                          fig.nrow.overall=2,
                          fig.ncol.overall=1,
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
  
  # compute age ratio within age groups and levels of disaggregation
  data_with_age_ratio <- ComputeAgeRatios(data=data,
                                          name.disaggregations=name.disaggregations,
                                          name.males=name.males,
                                          name.females=name.females,
                                          name.age=name.age,
                                          name.sex=name.sex,
                                          name.date1=name.date1,
                                          name.date2=name.date2,
                                          name.population.year1=name.population.year1,
                                          name.population.year2=name.population.year2)
  
  
  # convert data into long format (more convenient for ggplot2)
  long_year1 <- data_with_age_ratio %>% 
    select(name.disaggregations,
           name.sex, 
           name.age,
           name.population.year1,
           name.date1,
           age_ratio_1) %>%
    rename(sex=name.sex,
           pop=pop1,
           date=date1,
           age_ratio=age_ratio_1)
  
  long_year2 <- data_with_age_ratio %>% 
    select(name.disaggregations,
           name.sex, 
           name.age,
           name.population.year2,
           name.date2,
           age_ratio_2) %>%
    rename(sex=name.sex,
           pop=pop2,
           date=date2,
           age_ratio=age_ratio_2)
  data_with_age_ratio_long <- rbind(long_year1, long_year2)
  
  # for each level of disaggregation, create a plot showing age ratios in year1 and year2
  all_levels <- unique(levels(data[, name.disaggregations]))
  n_disaggregations <- length(all_levels)
  list_plots <- vector("list", length=n_disaggregations)
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
           y="age ratio",
           title=paste("age ratio in\n", one_level))
    
        list_plots[[i]] <- g_one_level
    ylim.disaggregated <- NULL
  }
  arranged_plots <- marrangeGrob(list_plots, 
                                 nrow=fig.nrow.disaggregated, 
                                 ncol=fig.ncol.disaggregated)
  
  # for each of the two Census years, create a plot showing age ratios in the different levels of disaggregation
  if (is.null(ylim.overall)) {
    ylim.overall <- c(min(data_with_age_ratio_long$age_ratio, na.rm=TRUE),
                      max(data_with_age_ratio_long$age_ratio, na.rm=TRUE))
  }
  ## Census year 1
  g_year1_males <- ggplot(data=data_with_age_ratio_long %>%
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
    scale_colour_discrete(name=name.disaggregations)
  
  g_year1_females <- ggplot(data=data_with_age_ratio_long %>%
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
    scale_colour_discrete(name=name.disaggregations)
  

  ## Census year 2
  g_year2_males <- ggplot(data=data_with_age_ratio_long %>%
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
    scale_colour_discrete(name=name.disaggregations)
  
  g_year2_females <- ggplot(data=data_with_age_ratio_long %>%
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
    scale_colour_discrete(name=name.disaggregations)
  
    list_plots_overall <- list(g_year2_females, 
                               g_year2_males,
                               g_year1_females, 
                               g_year1_males)
    overall <- marrangeGrob(list_plots_overall,
                            nrow=fig.nrow.overall,
                            ncol=fig.ncol.overall)
  
  # print/save plots according to specified arguments
  graphics.off()
  if (save.disaggregated == TRUE) {
    if (is.null(save.name_disaggregated) == FALSE) {
      pdf(paste0(save.name_disaggregated, ".pdf")) 
    } else {
      pdf(paste0(plots.dir, "age_ratios_by_", 
                 name.disaggregations, "_", Sys.Date(), ".pdf"))
    }
    print(arranged_plots)
    for (i in 1:10) graphics.off()
  }
  graphics.off()
  if (save.overall == TRUE) {
    if (is.null(save.name_overall) == FALSE) {
      pdf(paste0(save.name_overall, ".pdf")) 
    } else {
      pdf(paste0(plots.dir, "age_ratios_combined_", 
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
  return(data_with_age_ratio)
}


