library(tidyverse)
library(readxl)
library(scales)

theme_set(theme_bw())

rm(list = ls())

df1 <- read_excel("passing_stats.xlsx", 1)
df2 <- read_excel("passing_stats.xlsx", 2)
df3 <- read_excel("passing_stats.xlsx", 3)
df4 <- read_excel("passing_stats.xlsx", 4)

files <- list(df1, df2, df3, df4)
data <- bind_cols(files)



df_raw <- df1 %>% 
  left_join(df2) %>% 
  left_join(df3) %>% 
  left_join(df4) %>% 
  mutate(pass_percentage = pass_percentage / 100)


df_raw %>% 
  ggplot(aes(shots, goals))

df_raw %>% 
  ggplot(aes(passes, pass_percentage, label = team)) +
  geom_label() +
  geom_smooth() +
  scale_y_continuous(labels = percent)

df_gathered <- df_raw %>% 
  gather(metric, measure, -c(team, goals))

df_gathered %>% 
  mutate(pit = team == "Pittsburgh Riverhounds") %>% 
  ggplot(aes(measure, goals, label = team)) +
  geom_label(aes(fill = pit), size = 3) +
  geom_smooth() +
  facet_wrap(~metric, scales = "free_x")
