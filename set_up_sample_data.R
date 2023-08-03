

rm(list=ls())

library(tidyverse)


df <- read_csv("sample_data/sample_data.csv")

head(df)
variable.names(df)

df <- df[-c(1:2),]


df %>% select(starts_with("Try"))
