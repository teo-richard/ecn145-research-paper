library(tidyverse)
library(knitr)
library(kableExtra)

fd_guber_data = read_csv("cleaned_data/merged_datasets/fd_g_inc.csv") %>% 
  select(-d_manufacturing, -d_edu_health, -d_percent_car)

fd_turnout = read_csv("cleaned_data/merged_datasets/fd_turnout.csv") %>% 
  select(-d_manufacturing, -d_edu_health, -d_percent_car)

# :::::::::::::::::::: INCUMBENT ::::::::::::::::::::
# ----- SUMMARY TABLE -----

df = fd_guber_data %>% select(d_gas, d_inc_vote) %>% summary() %>% as.data.frame()

df = df %>% 
    separate(Freq, into = c("Statistic", "Value"), sep = ":") %>% 
    pivot_wider(names_from = Var2, values_from = Value)

df = df[, -1]
names(df) = c("Statistic", "d_gas", "d_inc_vote")

df = df %>% mutate(across(2:3, as.numeric)) %>% 
    add_row(Statistic = "SD", d_gas = sd(fd_guber_data$d_gas), d_inc_vote = sd(fd_guber_data$d_inc_vote))

kable(
  df,
  format = "latex",
  caption = "Summary Statistics",
  booktabs = TRUE,  # Professional horizontal lines
  digits = 2,        # Round to 2 decimal places
  align = 'lcccc'    # Left align first column, center rest
) %>%
  kable_styling(
    latex_options = c("HOLD_position"),  # Add stripes
    font_size = 10,
    position = "center"
  ) %>%
  footnote(
    general = "All variables are first-differenced. Gas prices measured in 2024 dollars. Incumbent vote share measured as proportion [0,1].",
    general_title = "Note:",
    threeparttable = TRUE
  )

# ----- PLOT OF DEPENDENT VAR -----

# Histogram
fd_guber_data %>% ggplot(aes(x = d_inc_vote)) +
  geom_histogram(fill = "steelblue", color = "black") +
  geom_hline(yintercept = mean(fd_guber_data$d_inc_vote)) +
  labs(
    x = "Change in Incumbent Vote Share",
    y = "",
    title = "") +
  theme_minimal()

ggsave("g_histogram.png", width = 8, height = 6)


# :::::::::::::::::::: TURNOUT ::::::::::::::::::::
# ----- SUMMARY TABLE -----

df = fd_turnout %>% select(d_gas, d_turnout) %>% summary() %>% as.data.frame()

df = df %>% 
    separate(Freq, into = c("Statistic", "Value"), sep = ":") %>% 
    pivot_wider(names_from = Var2, values_from = Value)

df = df[, -1]
names(df) = c("Statistic", "d_gas", "d_turnout")

df = df %>% mutate(across(2:3, as.numeric)) %>% 
    add_row(Statistic = "SD", d_gas = sd(fd_turnout$d_gas), d_turnout = sd(fd_turnout$d_turnout))

kable(
  df,
  format = "latex",
  caption = "Summary Statistics",
  booktabs = TRUE,  # Professional horizontal lines
  digits = 2,        # Round to 2 decimal places
  align = 'lcccc'    # Left align first column, center rest
) %>%
  kable_styling(
    latex_options = c("HOLD_position"),  # Add stripes
    font_size = 10,
    position = "center"
  )   %>% footnote(
    general = "All variables are first-differenced. Gas prices measured in 2024 dollars. Turnout measured as proportion [0,1].",
    general_title = "Note:",
    threeparttable = TRUE
  )


# Histogram
fd_turnout %>% ggplot(aes(x = d_turnout)) +
  geom_histogram(fill = "steelblue", color = "black") +
  labs(
    x = "Change in Incumbent Vote Share",
    y = "",
    title = "") +
  theme_minimal()


ggsave("t_histogram.png", width = 8, height = 6)
