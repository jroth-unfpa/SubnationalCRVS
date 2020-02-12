#' Plot counts of single-year ages to reveal potential age heaping
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
#' @param mark_multiples_of_5_disaggregated asdf
#' @param mark_multiples_of_5_overall asdf
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
#' @param save.name_disaggregated asdf
#' @param print.overall asdf
#' @param save.overall asdf
#' @param save.name_overall asdf
#' @param plots.dir asdf
#' @examples 
#' @import dplyr
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
  
  # verify that the age variable is single-year ages and not groups of multiple ages (e.g. 5-year age groups)
  # and also emphasize that only the "deaths" column is actually not required
  print("need to add a way to check for single-year ages")
  
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
           title=paste("estimated population by age \n in", one_level))
    
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
    scale_colour_discrete(name=name.disaggregations)
   
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
     scale_colour_discrete(name=name.disaggregations)
  
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
     scale_colour_discrete(name=name.disaggregations)

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
      pdf(paste0(plots.dir, "potential_age_heaping_by_", 
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
