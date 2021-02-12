edu_recodes <- function() list(
  "Some high school"= "1",
  "High school" = "2",
  "Some college" = "3",
  "Associates" = "4",
  "Bachelors" = "5",
  "Masters" = "6",
  "Professional (MD, JD)" = "7",
  "Doctoral" = "8"
)

aas_recode <- list(
  "Not at all" = "1", 
  "Very slightly" = "2", 
  "Somewhat" = "3",
  "Moderately" = "4", 
  "Much" = "5", 
  "Very much" = "6", 
  "Extremely" = "7"
)
aas_pos <- c("aas_happy", "aas_pleased", "aas_joyful", "aas_enjoyment", "aas_peaceful", "aas_relaxed")
aas_neg <- c("aas_anxious", "aas_angry", "aas_frustrated", "aas_depressed", "aas_unhappy", "aas_bored")

godard_recode <- list(
  "Strongly disagree" = 1, 
  "Somewhat disagree" = 2, 
  "Neutral" = 3,
  "Somewhat agree" = 4, 
  "Strongly agree" = 5
)
