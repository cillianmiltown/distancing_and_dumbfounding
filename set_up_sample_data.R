

rm(list=ls())

library(tidyverse)


df <- read_csv("sample_data/sample_data.csv")

head(df)
variable.names(df)

df <- df[-c(1:2),]

df$ctrl
df$slf_fut
df$slf_now
df$oth_fut
df$oth_now

df$condition5 <- NA
df$scenario <- NA

ctrl <- df[which(is.na(df$ctrl)==F),]
ctrl$condition5 <- rep("ctrl")

slf_fut <- df[which(is.na(df$slf_fut)==F),]
slf_fut$condition5 <- rep("slf_fut")

slf_now <- df[which(is.na(df$slf_now)==F),]
slf_now$condition5 <- rep("slf_now")

oth_fut <- df[which(is.na(df$oth_fut)==F),]
oth_fut$condition5 <- rep("oth_fut")

oth_now <- df[which(is.na(df$oth_now)==F),]
oth_now$condition5 <- rep("oth_now")


df <- rbind(ctrl, slf_fut, slf_now, oth_fut, oth_now)


Jen <- df %>% select(
  "ResponseId"
  , "condition5"
  , "scenario"
  , "attn_1","attn_2"
  , "before", "before_dtls"
  , starts_with("Jen"))
Jen$scenario <- rep("Jennifer")
Jen <- `colnames<-`(Jen, c("ResponseId"
                           ,"condition5"
                           ,"scenario"
                           ,"attn_1"
                           ,"attn_2"
                           ,"before"
                           ,"before_dtls"
                           ,"ju1_2"
                           ,"cf1_2"  
                           ,"rsn_st1"
                           ,"ca1"
                           ,"ca1_DO"
                           ,"sw1"
                           ,"sw1_DO"
                           ,"rsn1"
                           ,"rsn1_DO"
                           ,"ca2"    
                           ,"ca2_DO"
                           ,"sw2"
                           ,"sw2_DO"
                           ,"rsn2"
                           ,"ca3"
                           ,"sw3"
                           ,"sw3_DO"
                           ,"rsn3"   
                           ,"rsn3_DO"
                           ,"cs"
                           ,"cs_DO"
                           ,"rsn_st2"
                           ,"ju2_2"
                           ,"cf2_2"))

Jmk <- df %>% select(
  "ResponseId"
  , "condition5"
  , "scenario"
  , "attn_1","attn_2"
  , "before", "before_dtls"
  , starts_with("Jmk"))
Jmk$scenario <- rep("Julie and Mark")
Jmk <- `colnames<-`(Jmk, c("ResponseId"
                           ,"condition5"
                           ,"scenario"
                           ,"attn_1"
                           ,"attn_2"
                           ,"before"
                           ,"before_dtls"
                           ,"ju1_2"
                           ,"cf1_2"  
                           ,"rsn_st1"
                           ,"ca1"
                           ,"ca1_DO"
                           ,"sw1"
                           ,"sw1_DO"
                           ,"rsn1"
                           ,"rsn1_DO"
                           ,"ca2"    
                           ,"ca2_DO"
                           ,"sw2"
                           ,"sw2_DO"
                           ,"rsn2"
                           ,"ca3"
                           ,"sw3"
                           ,"sw3_DO"
                           ,"rsn3"   
                           ,"rsn3_DO"
                           ,"cs"
                           ,"cs_DO"
                           ,"rsn_st2"
                           ,"ju2_2"
                           ,"cf2_2"))

Try <- df %>% select(
  "ResponseId"
  , "condition5"
  , "scenario"
  , "attn_1","attn_2"
  , "before", "before_dtls"
  , starts_with("Try"))
Try$scenario <- rep("Trolley")
Try <- `colnames<-`(Try, c("ResponseId"
                           ,"condition5"
                           ,"scenario"
                           ,"attn_1"
                           ,"attn_2"
                           ,"before"
                           ,"before_dtls"
                           ,"ju1_2"
                           ,"cf1_2"  
                           ,"rsn_st1"
                           ,"ca1"
                           ,"ca1_DO"
                           ,"sw1"
                           ,"sw1_DO"
                           ,"rsn1"
                           ,"rsn1_DO"
                           ,"ca2"    
                           ,"ca2_DO"
                           ,"sw2"
                           ,"sw2_DO"
                           ,"rsn2"
                           ,"ca3"
                           ,"sw3"
                           ,"sw3_DO"
                           ,"rsn3"   
                           ,"rsn3_DO"
                           ,"cs"
                           ,"cs_DO"
                           ,"rsn_st2"
                           ,"ju2_2"
                           ,"cf2_2"))

Hnz <- df %>% select(
  "ResponseId"
  , "condition5"
  , "scenario"
  , "attn_1","attn_2"
  , "before", "before_dtls"
  , starts_with("Hnz"))
Hnz$scenario <- rep("Heinz")
Hnz <- `colnames<-`(Hnz, c("ResponseId"
                           ,"condition5"
                           ,"scenario"
                           ,"attn_1"
                           ,"attn_2"
                           ,"before"
                           ,"before_dtls"
                           ,"ju1_2"
                           ,"cf1_2"  
                           ,"rsn_st1"
                           ,"ca1"
                           ,"ca1_DO"
                           ,"sw1"
                           ,"sw1_DO"
                           ,"rsn1"
                           ,"rsn1_DO"
                           ,"ca2"    
                           ,"ca2_DO"
                           ,"sw2"
                           ,"sw2_DO"
                           ,"rsn2"
                           ,"ca3"
                           ,"sw3"
                           ,"sw3_DO"
                           ,"rsn3"   
                           ,"rsn3_DO"
                           ,"cs"
                           ,"cs_DO"
                           ,"rsn_st2"
                           ,"ju2_2"
                           ,"cf2_2"))


df <- rbind(Jen, Jmk, Try, Hnz)

table(df$ResponseId)
df <- df[which(is.na(df$ju1_2)==F),]
table(df$ResponseId)


df$psych <- rep(NA)
df$temp <- rep(NA)

ctrl <- df[which(df$condition5=="ctrl"),]
oth_fut <- df[which(df$condition5=="oth_fut"),]
oth_now <- df[which(df$condition5=="oth_now"),]
slf_fut <- df[which(df$condition5=="slf_fut"),]
slf_now <- df[which(df$condition5=="slf_now"),]

ctrl$psych <- rep("control")
ctrl$temp <- rep("control")

oth_fut$psych <- rep("other")
oth_now$psych <- rep("other")
slf_fut$psych <- rep("self")
slf_now$psych <- rep("self")
oth_fut$temp <- rep("future")
oth_now$temp <- rep("now")
slf_fut$temp <- rep("future")
slf_now$temp <- rep("now")

df <- rbind(
  ctrl
  , oth_fut
  , oth_now
  , slf_fut
  , slf_now
)



df$ju1_2
as.numeric(df$ju1_2)
df$ju1_2 <- as.numeric(df$ju1_2)

df$ju2_2
as.numeric(df$ju2_2)
df$ju2_2 <- as.numeric(df$ju2_2)


df$cf1_2
as.numeric(df$cf1_2)
df$cf1_2 <- as.numeric(df$cf1_2)

df$cf2_2
as.numeric(df$cf2_2)
df$cf2_2 <- as.numeric(df$cf2_2)


df$scenario <- as.factor(df$scenario)
df$condition5 <- as.factor(df$condition5)
df$psych <- as.factor(df$psych)
df$temp <- as.factor(df$temp)
df$cs <- as.factor(df$cs)

table(df$condition5)
table(df$scenario)
table(df$scenario,df$condition5)

sample_data <- df

rm(list=setdiff(ls(), c("sample_data")))

