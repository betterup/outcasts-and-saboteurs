library(dplyr)
source("data/constants.R")

format_desc <- function(col, df) df %>% 
  tabyl({{col}}) %>% 
  mutate(across(where(is.numeric), round, 4))

demog_prep <- function(df, survey_data) {
  df %>% 
    select(workerId) %>% 
    left_join(survey_data, by = c("workerId" = "workerid")) %>% 
    group_by(workerId) %>%
    select(workerId, sex, race, edu, age, age_range) %>% 
    distinct()
}

full_demographics <- function(df) {
  list(
    format_desc(sex, df),
    format_desc(race, df),
    format_desc(edu, df) %>% mutate(across(edu, ~fct_recode(as.factor(.), !!!edu_recodes()))),
    format_desc(age_range, df)
  )
}

summarise_demographics <- function(df) {
  n <- nrow(df)
  mean_age <- mean(df$age) %>% round(2)
  sd_age <- sd(df$age) %>% round(2)
  glue("Total N = {n}")
  glue("Mean age (SD) in years: {mean_age} ({sd_age})")
}

# Create function for simpler manipulation check with t-tests:
manip_check_t <- function(d) {
  mchecks <- c("ignored", "excluded", "belonged", "mc_percent_ball", names(select(d, starts_with("aas")))) %>%
    map(~t.test(as.formula(paste(.x, "~ condition")), data = d, var.equal = TRUE))
  return(mchecks)
}

create_manip_plot <- function(manip_data, fill_colors = viridis(n_distinct(manip_data$condition)), aas = TRUE) {
  if (aas) {
    manip_vars <- c("ignored", "excluded", "belonged", "mc_percent_ball", "aas_pos", "aas_neg")
    manip_names <- c("Ignored", "Excluded", "Belonged", 
                     "% Ball Tosses", "Positive Emotions", "Negative Emotions")
  } else {
    manip_vars <- c("ignored", "excluded", "belonged", "mc_percent_ball")
    manip_names <- c("Ignored", "Excluded", "Belonged", "% Ball Tosses")
  }
  
  manip_recodes <- list(
    "ignored" = "Ignored",
    "excluded" = "Excluded",
    "belonged" = "Belonged",
    "mc_percent_ball" = "% Ball Tosses",
    "aas_pos" = "Positive Emotions",
    "aas_neg" = "Negative Emotions"
  )
  
  vlines <- data.frame(var = manip_names,
                       x_vline = manip_data %>% 
                         select(all_of(manip_vars)) %>% 
                         summarise_all(~mean(.x, na.rm=T)) %>% 
                         as.double()
                       )
  
  manip_data %>% 
    select(condition, manip_vars) %>% 
    pivot_longer(-condition, names_to = "var", values_to = "val") %>% 
    mutate(condition = recode(condition, "0" = "Excluded", "1" = "Included"),
           var = recode(var, !!!manip_recodes)) %>% 
    ggplot(aes(x=val, fill=condition)) + 
    facet_wrap("var", scales = "free", nrow = 2) +
    geom_density(color="black", alpha=.7) +
    geom_vline(data=vlines, aes(xintercept=x_vline), color="black", linetype="dashed", size=.5) + 
    scale_fill_manual(values = fill_colors) +
    labs(x = "Response values",
         y = "Distribution density estimate",
         fill = "Condition") +
    theme_bw() +
    theme(
      axis.title = element_text(size=11, face="bold"),
      axis.text = element_text(size=10),
      panel.grid = element_blank(),
      strip.text.x = element_text(size=10, face="bold"),
      legend.text = element_text(size=9),
      legend.title = element_text(size=9, face="bold"),
      legend.position = "top",
    )
}


create_interaction_plot <- function(d, anno_df, max_tasks) {
  d %>% 
    ggplot(aes(x=condition, y=n_tasks, fill=condition)) + 
    facet_wrap("play_for") +
    stat_summary(fun.data = "pois_se", fun.args = list(n=max_tasks, prop=F),
                 geom = "errorbar", color="black", width=.075, size=.75) +
    geom_point(stat="summary", fun="mean", shape=21, color="black", size=3, stroke=1) +
    geom_text(data = anno_df, aes(x = anno_x, y = anno_y, label = pval)) +
    scale_fill_manual(values = c("red", "blue")) +
    labs(x = "Condition",
         y = "Number of trials completed\nduring reward task") +
    theme_bw() +
    theme(
      axis.title = element_text(size=11, face="bold"),
      axis.text = element_text(size=10),
      panel.grid = element_blank(),
      strip.text.x = element_text(size=10, face="bold"),
      legend.position = "blank"
    )
}


create_survplot <- function(survfit_obj, title) {
  plot <- ggsurvplot(fit = survfit_obj,
                     title = title,         
                     xlab = "Number of trials during reward task",
                     ylab = "Probability of continuing",
                     font.main = c(12, "bold"),
                     font.x = c(11, "bold"),
                     font.y = c(11, "bold"),
                     font.tickslab = c(10, "plain"),
                     legend = "top", 
                     legend.title = "Condition",
                     legend.labs = c("Excluded", "Included"),
                     size = 1,
                     linetype = "strata",
                     palette = c("red", "blue"),
                     conf.int = TRUE,
                     pval = TRUE,
                     pval.size = 5,
                     pval.coord = c(50, .75),
                     risk.table = TRUE,
                     risk.table.title = "Number of participants at risk of dropout",
                     risk.table.fontsize = 4,
                     risk.table.y.text.col = TRUE,
                     risk.table.col = "strata",
                     conf.int.alpha = .2)
  plot$table <- ggpubr::ggpar(plot$table,
                              font.title = list(size = 11, face = "bold"),
                              font.tickslab = c(10, "plain"),
                              font.x = c(11, face = "bold"),
                              font.y = c(11, face = "bold"))
  return(plot)
}

# see: https://stackoverflow.com/questions/32616639/in-a-custom-r-function-that-calls-ezanova-how-do-i-parameterize-the-dv
anova_wrapper <- function(data, dv, interv) {
  eval(
    substitute(
      ezANOVA(data = data, dv = dv, 
              wid = workerId, between = interv, 
              detailed = TRUE, return_aov = TRUE)    
    )
  )
}

