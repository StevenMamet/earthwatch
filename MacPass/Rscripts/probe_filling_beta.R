#______________________________----
# Libraries ----
library(tidyverse)    # Tidy data
library(splines)      # Spline-fitting
library(psych)        # Pairs plot
library(lubridate)    # Wrangling dates
library(dplyr)        # Tidy notation
library(scales)       # Rescaling data and alpha
library(forecast)     # tsclean()
library(purrr)        # Functional programming tools
library(weathercan)   # Get weather station details from Environment Canada
library(patchwork)    # Tidy organizing of plots
library(wesanderson)  # Nice color palettes
library(broom)        # Tidy output of lm() into nice tibble

rm(list = ls())

#______________________________----
# Constants ----
setwd("~/Desktop/Workspace")

#______________________________----
# Read in probe data ----
df <- read.csv("./earthwatch/MacPass/data/pipeline_plateau.csv")

df <- df %>% 
  mutate(year = str_sub(date, 1, 4),
         month = str_sub(date, 5, 6),
         day = str_sub(date, 7, 8),)

df_sub <- df %>% 
  select(where(is_character)) %>% 
  mutate(year = as.numeric(year))

df_sub_long <- df_sub %>%
  select(-c(month, day)) %>% 
  pivot_longer(!year, names_to = "pt", values_to = "depth") %>% 
  mutate(across(matches("year","depth"), as.numeric))

df_sub_long$depth[grep(">|<|200+",df_sub_long$depth)] <- NA
df_sub_long$depth[grep("Missing|in water |in water|missing |off feature",df_sub_long$depth)] <- NA

df_sub_long <- df_sub_long %>% 
  mutate(depth = as.numeric(ifelse(depth == "", NA, depth))) %>% 
  arrange(pt, year) %>% 
  group_by(pt) %>% 
  mutate(blanks = sum(is.na(depth))) %>% 
  ungroup() %>% 
  filter(blanks != 0, blanks <= 11)

lm_df <- df_sub_long %>% 
  # filter(events > 2, events <= 280) %>%
  nest(data = -pt) %>% 
  mutate(model = map(data, ~lm(depth ~ year, data = .)), tidied = map(model, tidy)) %>% 
  unnest(tidied)

df_a24 <- df_sub_long %>% 
  filter(pt == unique(lm_df$pt)[1]) %>% 
  mutate(pred = lm_df$estimate[lm_df$term == "(Intercept)" & lm_df$pt == unique(lm_df$pt)[1]] + lm_df$estimate[lm_df$term == "year" & lm_df$pt == unique(lm_df$pt)[1]]*year)
df_a25 <- df_sub_long %>% 
  filter(pt == unique(lm_df$pt)[2]) %>% 
  mutate(pred = lm_df$estimate[lm_df$term == "(Intercept)" & lm_df$pt == unique(lm_df$pt)[2]] + lm_df$estimate[lm_df$term == "year" & lm_df$pt == unique(lm_df$pt)[2]]*year)
df_a26 <- df_sub_long %>% 
  filter(pt == unique(lm_df$pt)[3]) %>% 
  mutate(pred = lm_df$estimate[lm_df$term == "(Intercept)" & lm_df$pt == unique(lm_df$pt)[3]] + lm_df$estimate[lm_df$term == "year" & lm_df$pt == unique(lm_df$pt)[3]]*year)
df_c20 <- df_sub_long %>% 
  filter(pt == unique(lm_df$pt)[4]) %>% 
  mutate(pred = lm_df$estimate[lm_df$term == "(Intercept)" & lm_df$pt == unique(lm_df$pt)[4]] + lm_df$estimate[lm_df$term == "year" & lm_df$pt == unique(lm_df$pt)[4]]*year)
df_c7 <- df_sub_long %>% 
  filter(pt == unique(lm_df$pt)[5]) %>% 
  mutate(pred = lm_df$estimate[lm_df$term == "(Intercept)" & lm_df$pt == unique(lm_df$pt)[5]] + lm_df$estimate[lm_df$term == "year" & lm_df$pt == unique(lm_df$pt)[5]]*year)
df_e15 <- df_sub_long %>% 
  filter(pt == unique(lm_df$pt)[6]) %>% 
  mutate(pred = lm_df$estimate[lm_df$term == "(Intercept)" & lm_df$pt == unique(lm_df$pt)[6]] + lm_df$estimate[lm_df$term == "year" & lm_df$pt == unique(lm_df$pt)[6]]*year)
df_e16 <- df_sub_long %>% 
  filter(pt == unique(lm_df$pt)[7]) %>% 
  mutate(pred = lm_df$estimate[lm_df$term == "(Intercept)" & lm_df$pt == unique(lm_df$pt)[7]] + lm_df$estimate[lm_df$term == "year" & lm_df$pt == unique(lm_df$pt)[7]]*year)
df_e17 <- df_sub_long %>% 
  filter(pt == unique(lm_df$pt)[8]) %>% 
  mutate(pred = lm_df$estimate[lm_df$term == "(Intercept)" & lm_df$pt == unique(lm_df$pt)[8]] + lm_df$estimate[lm_df$term == "year" & lm_df$pt == unique(lm_df$pt)[8]]*year)
df_e19 <- df_sub_long %>% 
  filter(pt == unique(lm_df$pt)[9]) %>% 
  mutate(pred = lm_df$estimate[lm_df$term == "(Intercept)" & lm_df$pt == unique(lm_df$pt)[9]] + lm_df$estimate[lm_df$term == "year" & lm_df$pt == unique(lm_df$pt)[9]]*year)
df_e28 <- df_sub_long %>% 
  filter(pt == unique(lm_df$pt)[10]) %>% 
  mutate(pred = lm_df$estimate[lm_df$term == "(Intercept)" & lm_df$pt == unique(lm_df$pt)[10]] + lm_df$estimate[lm_df$term == "year" & lm_df$pt == unique(lm_df$pt)[10]]*year)
df_1.11 <- df_sub_long %>% 
  filter(pt == unique(lm_df$pt)[11]) %>% 
  mutate(pred = lm_df$estimate[lm_df$term == "(Intercept)" & lm_df$pt == unique(lm_df$pt)[11]] + lm_df$estimate[lm_df$term == "year" & lm_df$pt == unique(lm_df$pt)[11]]*year)
df_1.2.1 <- df_sub_long %>% 
  filter(pt == unique(lm_df$pt)[12]) %>% 
  mutate(pred = lm_df$estimate[lm_df$term == "(Intercept)" & lm_df$pt == unique(lm_df$pt)[12]] + lm_df$estimate[lm_df$term == "year" & lm_df$pt == unique(lm_df$pt)[12]]*year)
df_1.9 <- df_sub_long %>% 
  filter(pt == unique(lm_df$pt)[13]) %>% 
  mutate(pred = lm_df$estimate[lm_df$term == "(Intercept)" & lm_df$pt == unique(lm_df$pt)[13]] + lm_df$estimate[lm_df$term == "year" & lm_df$pt == unique(lm_df$pt)[13]]*year)
df_2.1.1 <- df_sub_long %>% 
  filter(pt == unique(lm_df$pt)[14]) %>% 
  mutate(pred = lm_df$estimate[lm_df$term == "(Intercept)" & lm_df$pt == unique(lm_df$pt)[14]] + lm_df$estimate[lm_df$term == "year" & lm_df$pt == unique(lm_df$pt)[14]]*year)
df_2.11 <- df_sub_long %>% 
  filter(pt == unique(lm_df$pt)[15]) %>% 
  mutate(pred = lm_df$estimate[lm_df$term == "(Intercept)" & lm_df$pt == unique(lm_df$pt)[15]] + lm_df$estimate[lm_df$term == "year" & lm_df$pt == unique(lm_df$pt)[15]]*year)
df_3.12 <- df_sub_long %>% 
  filter(pt == unique(lm_df$pt)[16]) %>% 
  mutate(pred = lm_df$estimate[lm_df$term == "(Intercept)" & lm_df$pt == unique(lm_df$pt)[16]] + lm_df$estimate[lm_df$term == "year" & lm_df$pt == unique(lm_df$pt)[16]]*year)
df_3.13 <- df_sub_long %>% 
  filter(pt == unique(lm_df$pt)[17]) %>% 
  mutate(pred = lm_df$estimate[lm_df$term == "(Intercept)" & lm_df$pt == unique(lm_df$pt)[17]] + lm_df$estimate[lm_df$term == "year" & lm_df$pt == unique(lm_df$pt)[17]]*year)
df_3.14 <- df_sub_long %>% 
  filter(pt == unique(lm_df$pt)[18]) %>% 
  mutate(pred = lm_df$estimate[lm_df$term == "(Intercept)" & lm_df$pt == unique(lm_df$pt)[18]] + lm_df$estimate[lm_df$term == "year" & lm_df$pt == unique(lm_df$pt)[18]]*year)
df_3.23 <- df_sub_long %>% 
  filter(pt == unique(lm_df$pt)[19]) %>% 
  mutate(pred = lm_df$estimate[lm_df$term == "(Intercept)" & lm_df$pt == unique(lm_df$pt)[19]] + lm_df$estimate[lm_df$term == "year" & lm_df$pt == unique(lm_df$pt)[19]]*year)
df_4.12 <- df_sub_long %>% 
  filter(pt == unique(lm_df$pt)[20]) %>% 
  mutate(pred = lm_df$estimate[lm_df$term == "(Intercept)" & lm_df$pt == unique(lm_df$pt)[20]] + lm_df$estimate[lm_df$term == "year" & lm_df$pt == unique(lm_df$pt)[20]]*year)
df_4.21 <- df_sub_long %>% 
  filter(pt == unique(lm_df$pt)[21]) %>% 
  mutate(pred = lm_df$estimate[lm_df$term == "(Intercept)" & lm_df$pt == unique(lm_df$pt)[21]] + lm_df$estimate[lm_df$term == "year" & lm_df$pt == unique(lm_df$pt)[21]]*year)
df_4.22 <- df_sub_long %>% 
  filter(pt == unique(lm_df$pt)[22]) %>% 
  mutate(pred = lm_df$estimate[lm_df$term == "(Intercept)" & lm_df$pt == unique(lm_df$pt)[22]] + lm_df$estimate[lm_df$term == "year" & lm_df$pt == unique(lm_df$pt)[22]]*year)
df_4.24 <- df_sub_long %>% 
  filter(pt == unique(lm_df$pt)[23]) %>% 
  mutate(pred = lm_df$estimate[lm_df$term == "(Intercept)" & lm_df$pt == unique(lm_df$pt)[23]] + lm_df$estimate[lm_df$term == "year" & lm_df$pt == unique(lm_df$pt)[23]]*year)
df_4.3 <- df_sub_long %>% 
  filter(pt == unique(lm_df$pt)[24]) %>% 
  mutate(pred = lm_df$estimate[lm_df$term == "(Intercept)" & lm_df$pt == unique(lm_df$pt)[24]] + lm_df$estimate[lm_df$term == "year" & lm_df$pt == unique(lm_df$pt)[24]]*year)
df_4.8 <- df_sub_long %>% 
  filter(pt == unique(lm_df$pt)[25]) %>% 
  mutate(pred = lm_df$estimate[lm_df$term == "(Intercept)" & lm_df$pt == unique(lm_df$pt)[25]] + lm_df$estimate[lm_df$term == "year" & lm_df$pt == unique(lm_df$pt)[25]]*year)
df_4.9 <- df_sub_long %>% 
  filter(pt == unique(lm_df$pt)[26]) %>% 
  mutate(pred = lm_df$estimate[lm_df$term == "(Intercept)" & lm_df$pt == unique(lm_df$pt)[26]] + lm_df$estimate[lm_df$term == "year" & lm_df$pt == unique(lm_df$pt)[26]]*year)

df_pred <- bind_rows(df_a24,df_a25,df_a26,df_c20,df_c7,df_e15,
                     df_e16,df_e17,df_e19,df_e28,df_1.11,df_1.2.1,
                     df_1.9,df_2.1.1,df_2.11,df_3.12,df_3.13,df_3.14,
                     df_3.23,df_4.12,df_4.21,df_4.22,df_4.24,df_4.3,
                     df_4.8,df_4.9)

df_final <- df_sub_long %>% 
  left_join(df_pred, by = c("pt", "year")) %>% 
  select(-c(blanks.x, depth.y, blanks.y)) %>% 
  rename(depth = depth.x)# %>% 
  # mutate(depth = ifelse(is.na(depth), pred, depth))

df_final %>% 
  filter(pt == pt_list[26]) %>%
  ggplot(aes(x = year, y = depth, color = pt)) + geom_line() + geom_point() + 
  geom_line(aes(y = pred))
