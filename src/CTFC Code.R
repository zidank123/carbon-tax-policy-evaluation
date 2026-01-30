# Carbon Tax and Gasoline Consumption in Quebec
# Author: Kareem Zidan
# Description: Data cleaning and DiD estimation of the impact of Quebec’s carbon tax
# Data source: Statistics Canada (see README)

data <- read.csv("736_Dataset.csv")
head(data)
summary(data)

library(tidyverse)
library(haven)
library(janitor)
library(estimatr)
library(ggplot2)
library(dplyr)
library(knitr)

data %>% tabyl(Quebec)
data %>% tabyl(Post, Quebec)
hist(data$Per_Capita_Cons[data$Quebec ==0])

average_cons <- data %>%
  group_by(Quebec, Post) %>%
  summarise(avg_cons = mean(Per_Capita_Cons, na.rm = TRUE)) %>%
  arrange(Quebec, Post)

print(average_cons)

#Summary stats table
library(dplyr)
summary(data$Per_Capita_Cons)
data %>%
  group_by(Quebec) %>%
  summarise(
    Mean = mean(Per_Capita_Cons, na.rm = TRUE),
    SD = sd(Per_Capita_Cons, na.rm = TRUE),
    Min = min(Per_Capita_Cons, na.rm = TRUE),
    Max = max(Per_Capita_Cons, na.rm = TRUE),
    Median = median(Per_Capita_Cons, na.rm = TRUE),
    N = n()
  )

summary_table <- data %>%
  group_by(Quebec) %>%
  summarise(
    Mean = mean(Per_Capita_Cons, na.rm = TRUE),
    SD = sd(Per_Capita_Cons, na.rm = TRUE),
    Min = min(Per_Capita_Cons, na.rm = TRUE),
    Max = max(Per_Capita_Cons, na.rm = TRUE),
    Median = median(Per_Capita_Cons, na.rm = TRUE),
    N = n()
  ) %>%
  mutate(Province = ifelse(Quebec == 1, "Quebec", "Ontario")) %>%
  select(Province, everything(), -Quebec)

cat("**Monthly Gasoline Consumption Per Capita (liters)**\n\n")
kable(summary_table, digits = 2, align = 'c')

boxplot(Per_Capita_Cons ~ Quebec, data = data, main = "Fuel Consumption Per Capita by Province")
hist(data$Per_Capita_Cons, breaks = 30, main = "Histogram of Fuel Consumption Per Capita", xlab = "Consumption")

ggplot(data, aes(x = Per_Capita_Cons)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  facet_wrap(~ Province, scales = "free_y") +
  labs(title = "Distribution of Monthly Fuel Consumption Per Capita by Province",
       x = "Per Capita Fuel Consumption (Liters)",
       y = "Frequency") +
  theme_minimal()

ggplot(data, aes(x = Per_Capita_Cons, fill = Province)) +
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 2) +
  labs(title = "Histogram of Monthly Fuel Consumption Per Capita by Province",
       x = "Per Capita Fuel Consumption (Liters)",
       y = "Frequency") +
  theme_minimal()

#Regressions
DiD_model1 <- lm_robust(Per_Capita_Cons ~ Quebec + Post + Quebec:Post, data = data)
summary(DiD_model1)

#For Quebec, the carbon tax led to a 1.2 liter increase in fuel consumption per capita relative to Ontario, on average, compared to what it would have been if the tax had not been implemented.

#Actual model including month fixed effects
year_and_month_model <- lm_robust(Per_Capita_Cons ~ Quebec + factor(Month) + factor(Year) + Quebec:Post, data = data)
summary(year_and_month_model)

year_only_model <- lm_robust(Per_Capita_Cons ~ Quebec + factor(Year) + Quebec:Post, data = data)
summary(year_only_model)

no_FE_model <- lm_robust(Per_Capita_Cons ~ Quebec + Quebec:Post, data = data)
summary(no_FE_model)

models <- list(
  "No Fixed Effects" = no_FE_model,
  "Year Fixed Effects" = year_only_model,
  "Year & Month Fixed Effects" = year_and_month_model
)

modelsummary(models,
             stars = TRUE,
             gof_omit = "IC|Log|AIC|BIC",  # Clean up GOF section
             coef_map = c("Quebec" = "Quebec",
                          "Quebec:Post" = "Quebec × Post (DiD Estimate)"),
             title = "Regression Results: Effect of Carbon Tax on Fuel Consumption",
             notes = "Note: Dependent variable is monthly fuel consumption per capita. Month and Year fixed effects are included where indicated.")

#For Quebec, the carbon tax led to a 1.1094 liter increase in fuel consumption per capita relative to Ontario, on average, compared to what it would have been if the tax had not been implemented.

#Given the p-value of 6.140e-04 (which is 0.000614), the result is statistically significant at both the 1% level (p < 0.01) and the 5% level (p < 0.05).


#DiD Plot
data |> 
  group_by(Quebec, Year) |>
  summarize(Epcc = mean(Per_Capita_Cons)) |> 
  ggplot(aes(x = Year, y = Epcc, color = Quebec, group = Quebec)) + 
  geom_point(size = 4) + 
  geom_line() + 
  geom_vline(xintercept = 2007.75, linetype = "dashed", color = "red") + 
  scale_x_continuous(breaks = seq(min(data$Year), max(data$Year), by = 1)) + 
  labs(title = "Fuel Consumption Per Capita",
       x = "Year",
       y = "Fuel Consumption per Capita",
       caption = "Dashed red line indicates carbon tax implementation in October 2007")

data |> 
  group_by(Quebec, Year) |>
  summarize(Epcc = mean(Per_Capita_Cons)) |> 
  ggplot(aes(x = Year, y = Epcc, color = Quebec, group = Quebec)) + 
  geom_point(size = 4) + 
  geom_line() + 
  geom_vline(xintercept = 2007.75, linetype = "dashed", color = "red") + 
  scale_x_continuous(breaks = seq(min(data$Year), max(data$Year), by = 1)) + 
  #geom_smooth(method = "lm", se = FALSE, aes(group = Quebec), linetype = "dashed") +  # Adds dashed line of best fit
  labs(x = "Year",
       y = "Monthly Fuel Consumption per Capita (Liters)",
       caption = "Dashed red line indicates carbon tax implementation in October 2007")

data |> 
  mutate(Period = ifelse(Year + 0.5 < 2007.75, "Pre", "Post")) |> 
  group_by(Quebec, Year) |> 
  summarize(Epcc = mean(Per_Capita_Cons), .groups = "drop") |> 
  mutate(Period = ifelse(Year + 0.5 < 2007.75, "Pre", "Post")) |> 
  ggplot(aes(x = Year, y = Epcc, color = Quebec)) +
  geom_point(size = 4) +
  geom_line(aes(group = Quebec)) +
  geom_vline(xintercept = 2007.75, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(min(data$Year), max(data$Year), by = 1)) +
  geom_smooth(data = subset(data, Year + 0.5 < 2007.75),
              aes(x = Year, y = Per_Capita_Cons, group = Quebec, color = Quebec),
              method = "lm", se = FALSE, linetype = "dashed") +
  geom_smooth(data = subset(data, Year + 0.5 >= 2007.75),
              aes(x = Year, y = Per_Capita_Cons, group = Quebec, color = Quebec),
              method = "lm", se = FALSE, linetype = "dashed") +
  labs(
    x = "Year",
    y = "Monthly Fuel Consumption per Capita (Liters)",
    caption = "Dashed red line indicates carbon tax implementation in October 2007"
  )

#Testing parallel trends
PT_model <- lm_robust(Per_Capita_Cons ~ Quebec + factor(Month) + factor(Year) + Quebec * Pre, data = data |> filter(Year<2008) |> mutate (Pre = Year - 2001)) 
summary(PT_model)

PT_flexible_model <- lm_robust(Per_Capita_Cons ~ Quebec + factor(Month) + factor(Year) + Quebec*factor(Year), data = data |> filter(Year<2008) |> mutate (Pre = Year - 2001)) 
summary(PT_flexible_model)

coef_map <- c(
  "Quebec:Pre" = "Quebec × Pre",
  "Quebec:factor(Year)2002" = "Quebec × 2002",
  "Quebec:factor(Year)2003" = "Quebec × 2003",
  "Quebec:factor(Year)2004" = "Quebec × 2004",
  "Quebec:factor(Year)2005" = "Quebec × 2005",
  "Quebec:factor(Year)2006" = "Quebec × 2006"
)

# Create the table with only selected terms
modelsummary(PT_flexible_model,
             coef_map = coef_map,
             statistic = "std.error",
             stars = TRUE,
             gof_omit = 'AIC|BIC|Log.Lik|F|RMSE|R2',
             title = "Table for Year-by-Year Deviations (PT Assumption)"
)

coef <- summary(PT_model)$coefficients["Quebec:Pre", ]

# Create a table manually
PT_table <- data.frame(
  Variable = "Quebec × Pre",
  Estimate = round(coef["Estimate"], 3),
  Std.Error = round(coef["Std. Error"], 3),
  t_value = round(coef["t value"], 2),
  p_value = round(coef["Pr(>|t|)"], 3)
)

# View the table
print(PT_table)

#Testing no anticipation
data |>  mutate(T1 = ifelse(Year == 2001, 1,0), 
                T2 = ifelse(Year == 2002,1,0),
                T3 = ifelse(Year == 2003,1,0),
                T4 = ifelse(Year == 2004,1,0),
                T5 = ifelse(Year == 2005,1,0),
                T6 = ifelse(Year == 2006,1,0),
                T7 = ifelse(Year == 2007,1,0),
                ) -> data_expanded


NA_model = lm_robust(Per_Capita_Cons ~ Quebec + factor(Month) + Quebec*factor(Year), data = data_expanded |> filter(Year<2008)) 
summary(NA_model)


coef_map <- c(
  "Quebec:factor(Year)2002" = "Quebec × 2002",
  "Quebec:factor(Year)2003" = "Quebec × 2003",
  "Quebec:factor(Year)2004" = "Quebec × 2004",
  "Quebec:factor(Year)2005" = "Quebec × 2005",
  "Quebec:factor(Year)2006" = "Quebec × 2006",
  "Quebec:factor(Year)2007" = "Quebec × 2007"
)
modelsummary(NA_model,
  coef_map = coef_map,
  gof_omit = "Adj|F|Std|AIC|BIC|RMSE",               
  statistic = "({std.error})",                       
  stars = TRUE
)
