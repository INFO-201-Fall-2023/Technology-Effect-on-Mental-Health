library(dplyr)
library(stringr)

df_1 <- read.csv("social media usage.csv")
df_2 <- read.csv("anxiety report.csv")

usage_df <- df_1
anxiety_df <- df_2

anxiety_df$Year <- as.numeric(substr(anxiety_df$Time.Period.End.Date, nchar(anxiety_df$Time.Period.End.Date)-3, nchar(anxiety_df$Time.Period.End.Date)))
df <- left_join(anxiety_df, usage_df, by = "Year")

df <- df[!is.na(df$Value), ]

df$SnapCorr <- df$Value * df$Snapchat
df$InstaCorr <- df$Value * df$Instagram
df$TikTokCorr <- df$Value * df$TikTok
df$FbCorr <- df$Value * df$Facebook
df$PinCorr <- df$Value * df$Pinterest
df$TwitCorr <- df$Value * df$Twitter
df$RedditCorr <- df$Value * df$Reddit

df$DoesSnapAffect <- ifelse(df$SnapCorr > 800, "yes", "no")
df$DoesInstaAffect <- ifelse(df$InstaCorr > 800, "yes", "no")
df$DoesTikTokAffect <- ifelse(df$TikTokCorr > 800, "yes", "no")
df$DoesFbAffect <- ifelse(df$FbCorr > 800, "yes", "no")
df$DoesPinAffect <- ifelse(df$PinCorr > 800, "yes", "no")
df$DoesTwitAffect <- ifelse(df$TwitCorr > 800, "yes", "no")
df$DoesRedditAffect <- ifelse(df$RedditCorr > 800, "yes", "no")

snap_table <- df %>%
  summarize(median_snap = median(SnapCorr), mean_snap = mean(SnapCorr), min_snap = min(SnapCorr), max_snap = max(SnapCorr))
insta_table <- df %>%
  summarize(median_insta = median(InstaCorr), mean_insta = mean(InstaCorr), min_insta = min(InstaCorr), max_insta = max(InstaCorr))
tiktok_table <- df %>%
  summarize(median_tiktok = median(TikTokCorr), mean_tiktok = mean(TikTokCorr), min_tiktok = min(TikTokCorr), max_tiktok = max(TikTokCorr))
fb_table <- df %>%
  summarize(median_fb = median(FbCorr), mean_fb = mean(FbCorr), min_fb = min(FbCorr), max_fb = max(FbCorr))
pin_table <- df %>%
  summarize(median_pin = median(PinCorr), mean_pin = mean(PinCorr), min_pin = min(PinCorr), max_pin = max(PinCorr))
twit_table <- df %>%
  summarize(median_twit = median(TwitCorr), mean_twit = mean(TwitCorr), min_twit = min(TwitCorr), max_twit = max(TwitCorr))
reddit_table <- df %>%
  summarize(median_reddit = median(RedditCorr), mean_reddit = mean(RedditCorr), min_reddit = min(RedditCorr), max_reddit = max(RedditCorr))
