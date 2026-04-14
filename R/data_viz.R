rm(list = ls())

ebola_raw <- read.csv("data/raw/ebola.csv")

countries_to_keep <- c("Guinea", "Sierra Leone", "Liberia")
ebola <- ebola_raw %>% arrange(Date) %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>% 
  filter(Country %in% countries_to_keep & Date < ymd("2015-03-31"))

ebola %>% group_by(Country) %>% summarize(x = sum(Cum_conf_cases)) %>% arrange(desc(x))

library(ggplot2)
p1 <- ggplot(ebola, aes(x = Date, y = Cum_conf_cases,
                  fill = Country)) +
  geom_point(shape = 21, color = "black") +
  labs(x = "Time", y = "# conf. cases",
       title = "") + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
  theme(legend.position = "")

p2 <- ggplot(ebola, aes(x = Date, y = Cum_conf_cases,
                  col = Country)) +
  geom_line(lwd = 1, lty = 8) +
  labs(x = "Time", y = "# conf. cases",
       title = "Confirmed covid cases in three countries") + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
  theme(legend.position = "bottom")

p3 <- ggplot(ebola, aes(x = Date, y = Cum_conf_cases,
                  col = Country, fill = Country)) +
  geom_col(position = "stack") +
  labs(x = "Time", y = "# conf. cases",
       title = "") + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
  theme(legend.position = "")

library(patchwork)
p1+p2+p3

library(unibeCols)


ggplot(ebola, aes(x = Date, y = Cum_conf_cases,
                        fill = Country)) +
  geom_point(shape = 21, color = "black", alpha = 0.95,
             size = 2, stroke = 1) +
  scale_fill_manual(name = "Country",
                    breaks = countries_to_keep,
                    values = c(unibeRedS()[1], unibeMustardS()[1], unibeIceS()[1]),
                    labels = c("GIN", "SLE", "LBR")) +
  labs(x = "Time", y = "# conf. cases",
       title = "Confirmed Ebola cases Sep 2014-Mar 2015") + 
  scale_x_date(breaks = ymd(c("2014-08-29", "2014-10-01", "2014-12-01", 
                              "2015-02-01", "2015-04-01")),
               labels = c("29 Aug", "1 Oct", "1 Dec", "1 Feb", "1 Apr"),
               limits = ymd(c("2014-08-23", "2015-04-01"))) +
  scale_y_continuous(breaks = seq(from = 0, to = 1e4, by = 25e2),
                     limits = c(0, 1e4)) +
  theme(legend.position = "bottom")

ggplot(ebola, aes(x = Date, y = Cum_conf_cases,
                        col = Country)) +
  geom_line(linewidth = 1, linetype = "dashed",
            alpha = 0.75) +
  scale_color_manual(name = "Country",
                    breaks = countries_to_keep,
                    values = c(unibeRedS()[1], unibeMustardS()[1], unibeIceS()[1]),
                    labels = c("GIN", "SLE", "LBR")) +
  labs(x = "Time", y = "# conf. cases",
       title = "Confirmed Ebola cases") + 
  scale_x_date(breaks = ymd(c("2014-08-29", "2014-10-01", "2014-12-01", 
                              "2015-02-01", "2015-04-01")),
               labels = c("29 Aug", "1 Oct", "1 Dec", "1 Feb", "1 Apr"),
               limits = ymd(c("2014-08-23", "2015-04-01"))) +
  scale_y_continuous(breaks = seq(from = 0, to = 1e4, by = 25e2),
                     limits = c(0, 1e4)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
  theme(legend.position = "bottom")

ggplot(ebola, aes(x = Date, y = Cum_conf_cases,
                        col = Country, fill = Country)) +
  geom_col(position = "stack", alpha = 0.7, linewidth = 0.2, linetype = 1,
           width = 1.7) +
  scale_fill_manual(name = "Country",
                    breaks = countries_to_keep,
                    values = c(unibeRedS()[1], unibeMustardS()[1], unibeIceS()[1]),
                    labels = c("GIN", "SLE", "LBR")) +
  scale_color_manual(name = "Country",
                    breaks = countries_to_keep,
                    values = c(unibeRedS()[1], unibeMustardS()[1], unibeIceS()[1]),
                    labels = c("GIN", "SLE", "LBR")) +
  scale_x_date(breaks = ymd(c("2014-08-29", "2014-10-01", "2014-12-01", 
                              "2015-02-01", "2015-04-01")),
               labels = c("29 Aug", "1 Oct", "1 Dec", "1 Feb", "1 Apr"),
               limits = ymd(c("2014-08-23", "2015-04-01"))) +
  scale_y_continuous(breaks = seq(from = 0, to = 15e3, by = 25e2),
                     limits = c(0, 15e3)) +
  labs(x = "Time", y = "# conf. cases",
       title = "Confirmed Ebola cases") + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
  theme(legend.position = "bottom")

# insurance dataset
insurance_data_TD <- readr::read_csv("data/raw/insurance_with_date.csv",
                              show_col_types = F)

# First plot
library(ggplot2)
library(unibeCols)

ggplot(data = insurance_data_TD, aes(x = bmi, col = sex,
                                     fill = sex)) +
  geom_density(alpha = 0.4) +
  labs(x = expression(paste( "BMI (kg/", m^2,")"))) +
  scale_color_manual(breaks = c("female", "male"),
                     name = "",
                     values = c(unibeRedS()[1], unibeIceS()[1]),
                     labels = c("Female", "Male")) +
  scale_fill_manual(breaks = c("female", "male"),
                     name = "",
                     values = c(unibeRedS()[1], unibeIceS()[1]),
                    labels = c("Female", "Male"))

ggplot(data = insurance_data_TD, aes(x = charges, col = sex,
                                     fill = sex)) +
  geom_histogram(alpha = 0.4, position = "stack", 
                 aes(y = after_stat(density))) +
  geom_density(alpha = 0) +
  labs(x = "Charges in Dollars") +
  geom_vline(aes(xintercept = median(charges)), col = unibePastelS()[1],
             linewidth = 1) +
  scale_color_manual(breaks = c("female", "male"),
                     name = "",
                     values = c(unibeRedS()[1], unibeIceS()[1]),
                     labels = c("Female", "Male")) +
  scale_fill_manual(breaks = c("female", "male"),
                    name = "",
                    values = c(unibeRedS()[1], unibeIceS()[1]),
                    labels = c("Female", "Male")) +
  theme(legend.position = "top")





