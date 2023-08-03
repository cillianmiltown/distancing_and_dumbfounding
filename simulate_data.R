suppressMessages(library(simulator))
library(tidyverse)
library(desnum)
library(truncnorm)
library(psych)
suppressMessages(library(faux))


source("set_up_sample_data.R")

N <- 4000
session_seed <- 4

df <- sample_data

y <- df

empty_df <- df[0,]
x <- empty_df


x <- data.frame(
  matrix(NA,nrow = N,ncol = length(df))
) %>% 
  `colnames<-`(colnames(empty_df)) %>% 
  # set the scenarios and conditions
  mutate(
    condition5 = 
      rep(levels(empty_df$condition5), N/5)
  ) %>% 
  # use the condition5 variable to code the IVs
  # create a "psych" variable (psychological distance)
  mutate(psych = 
           dplyr::recode(condition5
                         , "ctrl" = "control"
                         , "oth_fut" = "other"
                         , "oth_now" = "other"
                         , "slf_fut" = "self"
                         , "slf_now" = "self"
           )
         # create a "temp variable for temporal distance
         , temp = 
           dplyr::recode(condition5
                         , "ctrl" = "control"
                         , "oth_fut" = "future"
                         , "oth_now" = "now"
                         , "slf_fut" = "future"
                         , "slf_now" = "now"
           )
  ) %>% 
  select(-ends_with("_DO")) %>% 
  select(-c(attn_1,attn_2,before,before_dtls)) %>% 
  select(-c(rsn_st1, ca1, sw1, rsn1, ca2, sw2, rsn2, ca3, sw3, rsn3, rsn_st2))



head(x)
x$ResponseId <- c(1:length(x$ResponseId))


control <- x[which(x$condition5=="ctrl"),]
oth_fut <- x[which(x$condition5=="oth_fut"),]
oth_now <- x[which(x$condition5=="oth_now"),]
slf_fut <- x[which(x$condition5=="slf_fut"),]
slf_now <- x[which(x$condition5=="slf_now"),]


y <- control
y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T, prob=c(0.67, 0.20, 0.13))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
control <- y
  
y <- oth_fut
y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T, prob=c(0.76, 0.9, 0.15))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
oth_fut <- y

y <- oth_now
y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T, prob=c(0.65, 0.12, 0.23))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
oth_now <- y

y <- slf_fut
y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T, prob=c(0.64, 0.12, 0.24))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
slf_fut <- y

y <- slf_now
y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T, prob=c(0.64, 0.12, 0.24))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
slf_now <- y


x <- rbind(control
           , oth_fut
           , oth_now
           , slf_fut
           , slf_now)

head(x)


simulated_data <- x

rm(list=setdiff(ls(), c("simulated_data")))
