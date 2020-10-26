library(readr)
library(tidyverse)
library(lubridate)
library(purrr)

read_cdc_data <- function(path, year) {
  read.table(path, header = TRUE, sep = " ") %>% 
    mutate(year = year) %>%
    fill(year)
}

filenames = c("Nat2017PublicUS.c20180516.r20180808.txt",
              "Nat2018PublicUS.c20190509.r20190717.txt",
              "Nat2019PublicUS.c20200506.r20200915.txt") %>% 
  map(., ~ paste("extracted-", ., sep=""))

years = c(2017, 2018, 2019)
NOT_STATED_BIRTHWEIGHT <- 9999

birth_df <- map2(filenames, years, ~ read_cdc_data(.x, .y)) %>% 
  do.call('rbind', .) %>%
  mutate(birth_month = month(birth_month, label = TRUE),
         day_of_birth = wday(day_of_birth, label = TRUE))

# boys to girls portion yearly
bg_pp <- birth_df %>%
  ggplot(aes(x=year, fill=sex)) +
  geom_bar(position = "fill") +
  ggtitle("Proportion of boys to girls yearly") +
  xlab("Year") + 
  ylab("Proportion of boys to girls")

# average weight at birth by gender
avg_w_g <- birth_df %>%
  select(sex,year, birth_month, birth_weight) %>%
  group_by(year, birth_month, sex) %>%
  summarise(mean = mean(birth_weight)) %>%
  ungroup() %>%
  ggplot(aes(x=birth_month, y=mean, group=sex)) +
  geom_line(aes(color=sex), size=1) +
  facet_wrap(~year) +
  ggtitle("Average birth weight in grams by gender") +
  xlab("Month") + 
  ylab("Weight in grams")

# boys to girls proportion by day of birth
b_g_pp <- birth_df %>%
  ggplot(aes(x=day_of_birth, fill=sex)) +
  geom_bar(position = "fill") +
  facet_wrap(~year) +
  ggtitle("Proportion of boys to girls day of birth") +
  xlab("Day of week") + 
  ylab("Proportion of boys to girls")
