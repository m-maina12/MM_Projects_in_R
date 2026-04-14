library(lubridate)
library(tidyverse)

insurance_data_TD <- read_csv("data/raw/insurance_with_date.csv",
                              show_col_types = F)

insurance_data_R_base <- read.csv("data/raw/insurance_with_date.csv")

insurance_data_R_base$date <- as.Date.character(insurance_data_R_base$date)


insurance_data <- insurance_data_TD

insurance_data$sex <- as.factor(insurance_data$sex)
insurance_data$region <- as.factor(insurance_data$region)

insurance_data_2 <- insurance_data_TD %>% 
  mutate(sex = as.factor(sex),
         region = as.factor(region),
         child2_smokes = (children > 2 & smoker == "yes"),
         date.LD = date %m+% months(6),
         date.RB = date + 6 * 30.4)

sum(insurance_data_2$date.LD == insurance_data_2$date.RB)
