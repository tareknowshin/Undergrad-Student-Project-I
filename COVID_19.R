
library(tidyverse)
library(finalfit)
library(haven)
library(here)
library(flextable)

data <- read.csv("COVID_19_Data.csv")

glimpse(data)

# Socio-demographic variables percentage computing

data <- data %>% 
  mutate(gender.factor =
           Gender %>% 
           factor() %>% 
           fct_recode(
             "Male"  = "1", 
             "Female"= "2"
           ))

data <- data %>% 
  mutate(
    age.factor = 
      Age %>% 
      cut(breaks = c (11,17,30,50,75), include.lowest = TRUE) %>% 
      fct_recode(
        "<18"      = "[11,17]", # warning: no space between values!!!
        "18-30"     = "(17,30]", 
        "31-50"     = "(30,50]", 
        ">50"       = "(50,75]"
      ) %>% 
      ff_label("Age (years)")
  )

data <- data %>% 
  mutate(profession.factor =
           Profession %>% 
           factor() %>% 
           fct_recode(
             "Student" = "1", 
             "Formal Job" = "2", 
             "Informal Job" = "5", 
             "Informal Job" = "6", 
             "Informal Job" = "7", 
             "Farmer"  = "3", 
             "Housewife" = "4"
           ))

age_group_percentage <- data %>%
  group_by(age.factor) %>%
  summarise(Number = n()) %>%
  mutate(Percentage = round((Number / sum(Number)) * 100, 1))


gender_percentage <- data %>% 
  group_by(gender.factor) %>% 
  summarize(Number = n()) %>% 
  mutate(Percentage = round((Number / sum(Number)) * 100, 1))

profession_percentage <- data %>% 
  group_by(profession.factor) %>% 
  summarise(Number = n()) %>% 
  mutate(Percentage = round((Number / sum(Number)) * 100, 1))

data <- data %>% 
  mutate(education.factor =
           Edu_Level %>% 
           factor() %>% 
           fct_recode(
             "Primary" = "1", 
             "Secondary" = "2", 
             "College" = "3", 
             "University" = "4"
           ))

education_percentage <- data %>% 
  group_by(education.factor) %>% 
  summarise(Number = n()) %>% 
  mutate(Percentage = round((Number / sum(Number)) * 100, 1))

data <- data %>% 
  mutate(residence.factor =
           Residence %>% 
           factor() %>% 
           fct_recode(
             "Urban" = "1", 
             "Rural" = "2", 
             "Semi-urban" = "3"
           ))

residence_percentage <- data %>% 
  group_by(residence.factor) %>% 
  summarise(Number = n()) %>% 
  mutate(Percentage = round((Number / sum(Number)) * 100, 1))

data <- data %>% 
  mutate(covid_status.factor =
           Covid_Sta %>% 
           factor() %>% 
           fct_recode(
             "Covid Positive" = "1", 
             "Covid Negative" = "2"
           ))

covid_status_percentage <- data %>% 
  group_by(covid_status.factor) %>% 
  summarise(Number = n()) %>% 
  mutate(Percentage = round((Number / sum(Number)) * 100, 1))

socio_demo_table <- data.frame( # Alhamdulillah!
  Characteristic = c("Age Group", "", "", "", "Gender", "", "Profession", 
                     "", "", "", "", "Education Level", "", "", "", "Residence", 
                     "", "", "Covid Status", ""
                     ),
  Category = c("<18", "18-30", "31-50", ">50", "Male", "Female", "Student", 
               "Formal Job", "Farmer", "Housewife", "Informal Job", "Primary", 
               "Secondary", "College", "University", "Urban", "Rural", "Semi-urban",
               "Covid Positive", "Covid Negative"
               
               ),
  Count = c(27, 221, 109, 61, 264, 154, 199, 61, 32, 67, 59, 81, 105, 55, 177, 
            163, 197, 58 , 26, 392),
  Percentage = c(6.5, 52.9, 26.1, 14.6, 63.2, 36.8, 47.6, 14.6, 7.7, 16.0, 14.1,
                 
                 19.4, 25.1, 13.2, 42.3, 39.0, 47.1, 13.9, 6.2, 93.8),
  stringsAsFactors = FALSE
)

ft <- socio_demo_table %>% flextable()

final_table <- set_header_labels(ft, 
                  Percentage = "Percentage (%)") %>% autofit()

library(officer)

doc <- read_docx() %>% 
  body_add_flextable(final_table)

print(doc, target = "socio_demo_table.docx")

# Exercise, smoking, and sleeping habit before and during COVID-19 lockdown

contingency_table1 <- table(data$Execise_B_Crna, data$Execise_Currnt)

mcnemar_test_result1 <- mcnemar.test(contingency_table)

contingency_table2 <- table(data$Smking_B_Crna, data$Smking_Crnt)

mcnemar_test_result2 <- mcnemar.test(contingency_table2)

contingency_table3 <- table(data$SlpBnEW, data$SlpcrrntNew)

mcnemar_test_result3 <- mcnemar.test(contingency_table3)

exdata$WeighChngeStatusNew

frequency_table <- table(exdata$WeighChngeStatusNew)

# Weight change status during COVID-19 lockdown

data <- data %>% 
  mutate(
    Weight.Change = 
      WeighChngeStatusNew %>% 
      factor() %>% 
      fct_recode(
        "Decrease" = "1", 
        "Stable"   = "2", 
        "Increase" = "3"
      ))

weightchange_percentage <- data %>% 
  group_by(Weight.Change) %>% 
  summarise(Number = n()) %>% 
  mutate(Percentage = round((Number / sum(Number)) * 100, 1)) %>% as.data.frame()

weightchange_percentage <- weightchange_percentage %>%
  mutate(Weight.Change = factor(Weight.Change, levels = c("Increase", "Decrease", "Stable")))

p1 <- ggplot(weightchange_percentage, aes(x = Weight.Change, y = Percentage, fill = Weight.Change)) +
  geom_bar(stat = "identity") +
  labs(title = "Weight change status",
       x = "Weight change category",
       y = "Percentage (%)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(legend.position = "none") 

 
# Weight change plot

 result <- data$WeightChnge %>%
   filter(values > 0) %>%  
   summarize(
     mean_value = mean(values, na.rm = TRUE),  
     sd_value = sd(values, na.rm = TRUE)      
   )

weight_change <- data$WeightChnge

mean_weight_increase <- mean(weight_change[weight_change > 0], na.rm = TRUE)
sd_weight_change_increase <- sd(weight_change[weight_change > 0], na.rm = TRUE)

mean_weight_decrease <- mean(weight_change[weight_change < 0], na.rm = TRUE)
sd_weight_change_decrease <- sd(weight_change[weight_change < 0], na.rm = TRUE)

weight_change <- data.frame(weight_change = weight_change)

weight_change <- weight_change %>%
  mutate(weight.change.category = ifelse(weight_change > 0, "Weight Increase", 
                                       ifelse(weight_change < 0, "Weight Decrease", "No change")))

summary_increase_weight <- weight_change %>%
  filter(weight.change.category == "Weight Increase") %>%
  summarize(
    mean_weight_increase = mean(weight_change, na.rm = TRUE),
    sd_weight_increase = sd(weight_change, na.rm = TRUE),
    se_weight_increase = sd(weight_change, na.rm = TRUE) / sqrt(n())
  )

summary_decrease_weight <- weight_change %>%
  filter(weight.change.category == "Weight Decrease") %>%
  summarize(
    mean_weight_decrease = mean(weight_change, na.rm = TRUE),
    sd_weight_decrease = sd(weight_change, na.rm = TRUE),
    se_weight_decrease = sd(weight_change, na.rm = TRUE) / sqrt(n())
  )

increase_weight <- data.frame(summary_increase_weight)

decrease_weight <- data.frame(summary_decrease_weight)

summary_weight$Category <- c ("Increase", "Decrease")

p2 <- ggplot(summary_weight, aes(x = Category, y = mean_weight, fill = Category)) +
  geom_bar(stat = "identity", width = 0.6) +  
  geom_errorbar(aes(ymin = mean_weight - se_weight, 
                    ymax = mean_weight + se_weight), 
                width = 0.2) + 
  labs(title = "Mean weight change",
       x = "Weight change category",
       y = "Mean weight change (kg)") +
  theme_minimal() +
  scale_fill_manual(values = c("Increase" = "blue", "Decrease" = "green")) +
  ylim(-4, 4) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") + 
  theme(legend.position = "none")
  

library(patchwork)

p1 + p2 # Alhamdulillah











