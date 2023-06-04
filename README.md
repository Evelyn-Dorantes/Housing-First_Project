# "Housing First" Project
The purpose of this project is to evaluate whether permanent housing units under “Housing First” policies alleviate homelessness despite its clash with the anti-housing movement, NIMBY (Not In My Backyard).

# First Draft
rm(list=ls())

library(lfe) 
library(dplyr)
library(stargazer)
library(ggplot2)

df <- read.csv("group5_dataset.csv")

unique(df$NEIGHBORHOOD_ID)

unique(df$YEAR)

df$log_pop <- log(df$population)
df$log_housein <- log(df$household_income)
df$log_medrent <- log(df$median_rent)
df$log_pov <- log(df$poverty)
df$log_evict <- log(df$evictions)
df$log_sheltbeds <- log(df$shelter_beds)
df$log_hometot <- log(df$homeless_total)
df$log_homeshelt <- log(df$sheltered_homeless)
df$log_homeunshelt <- log(df$unsheltered_homeless)

df$NIMBYism <- df$NIMBY_INDEX == "HIGH"

df1 <- df %>%
  group_by(NIMBY_INDEX, YEAR) %>%
  summarize(
    avg_log_sheltbeds = mean(log_sheltbeds)
  )

ggplot(data=df1) +
  geom_line(aes(x=YEAR, y=avg_log_sheltbeds, color=NIMBY_INDEX)) +
  geom_vline(xintercept=2014, linetype='dashed') + 
  theme_bw()

df2 <- df %>%
  group_by(NIMBY_INDEX, YEAR) %>%
  summarize(
    avg_log_hometot = mean(log_hometot)
  )

ggplot(data=df2) +
  geom_line(aes(x=YEAR, y=avg_log_hometot, color=NIMBY_INDEX)) +
  geom_vline(xintercept=2014, linetype='dashed') + 
  theme_bw()

df3 <- df %>%
  group_by(NIMBY_INDEX, YEAR) %>%
  summarize(
    avg_log_homeshelt = mean(log_homeshelt)
  )

ggplot(data=df3) +
  geom_line(aes(x=YEAR, y=avg_log_homeshelt, color=NIMBY_INDEX)) +
  geom_vline(xintercept=2014, linetype='dashed') + 
  theme_bw()

df4 <- df %>%
  group_by(NIMBY_INDEX, YEAR) %>%
  summarize(
    avg_log_homeunshelt = mean(log_homeunshelt)
  )

ggplot(data=df4) +
  geom_line(aes(x=YEAR, y=avg_log_homeunshelt, color=NIMBY_INDEX)) +
  geom_vline(xintercept=2014, linetype='dashed') + 
  theme_bw()

df$post <- df$YEAR>2014
df$treatXpost <- df$YEAR>2014 & df$NIMBYism

r1 <- felm(log_sheltbeds ~ treatXpost + NIMBYism + post + log_pop + log_housein + log_medrent + log_pov + log_evict, data=df) 
r2 <- felm(log_hometot ~ treatXpost + NIMBYism + post + log_pop + log_housein + log_medrent + log_pov + log_evict, data=df)
r3 <- felm(log_homeshelt ~ treatXpost + NIMBYism + post + log_pop + log_housein + log_medrent + log_pov + log_evict, data=df)
r4 <- felm(log_homeunshelt ~ treatXpost + NIMBYism + post + log_pop + log_housein + log_medrent + log_pov + log_evict, data=df)

stargazer(r1, r2, r3, r4, type="text")
