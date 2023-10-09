c("affiliation_namer" ,"c"
, "character2name" ,"correspondings"
, "d_author_affiliations" ,"d_j1"
, "d_j2" ,"d_j3"
, "df3" ,"df4"
, "df5" ,"display_abstract"
, "display_affiliations" ,"display_author_affiliations"
, "display_author_note" ,"display_authors"
, "display_corresponding_author" ,"display_keywords"
, "display_orcids" ,"display_title"
, "display_title_heading" ,"f_abstract_display"
, "f_affiliations_display" ,"f_author_affiliations"
, "f_author_affiliations_display" ,"f_author_display"
, "f_author_note_blanks" ,"f_author_note_display"
, "f_author_note_second_paragraph" ,"f_author_note_third_paragraph"
, "f_author_roles" ,"f_corresponding_author_display"
, "f_correspondings" ,"f_keywords_display"
, "f_orcids_display" ,"f_title_blanks"
, "f_title_display" ,"f_title_heading_display"
, "get_metadata" ,"has_annotations"
, "is_docx" ,"is_empty"
, "is_pdf" ,"large"
, "med" ,"one"
, "pw" ,"small"
, "t_j1" ,"t_j2"
, "t_j3" ,"w"
, "yml_metadata")

setdiff(
c("affiliation_namer" ,"c"
, "character2name" ,"correspondings"
, "d_author_affiliations" ,"d_j1"
, "d_j2" ,"d_j3"
, "df3" ,"df4"
, "df5" ,"display_abstract"
, "display_affiliations" ,"display_author_affiliations"
, "display_author_note" ,"display_authors"
, "display_corresponding_author" ,"display_keywords"
, "display_orcids" ,"display_title"
, "display_title_heading" ,"f_abstract_display"
, "f_affiliations_display" ,"f_author_affiliations"
, "f_author_affiliations_display" ,"f_author_display"
, "f_author_note_blanks" ,"f_author_note_display"
, "f_author_note_second_paragraph" ,"f_author_note_third_paragraph"
, "f_author_roles" ,"f_corresponding_author_display"
, "f_correspondings" ,"f_keywords_display"
, "f_orcids_display" ,"f_title_blanks"
, "f_title_display" ,"f_title_heading_display"
, "get_metadata" ,"has_annotations"
, "is_docx" ,"is_empty"
, "is_pdf" ,"large"
, "med" ,"one"
, "pw" ,"small"
, "t_j1" ,"t_j2"
, "t_j3" ,"w"
, "yml_metadata"),
c("affiliation_namer" 
  , "character2name" ,"correspondings"
  , "d_author_affiliations","display_abstract"
  , "display_affiliations" ,"display_author_affiliations"
  , "display_author_note" ,"display_authors"
  , "display_corresponding_author" ,"display_keywords"
  , "display_orcids" ,"display_title"
  , "display_title_heading" ,"f_abstract_display"
  , "f_affiliations_display" ,"f_author_affiliations"
  , "f_author_affiliations_display" ,"f_author_display"
  , "f_author_note_blanks" ,"f_author_note_display"
  , "f_author_note_second_paragraph" ,"f_author_note_third_paragraph"
  , "f_author_roles" ,"f_corresponding_author_display"
  , "f_correspondings" ,"f_keywords_display"
  , "f_orcids_display" ,"f_title_blanks"
  , "f_title_display" ,"f_title_heading_display"
  , "get_metadata" ,"has_annotations"
  , "is_docx" ,"is_empty"
  , "is_pdf" 
  , "yml_metadata"))



##### Logistic Regression prep 7 Aug 2023 #####



# 
# x <- rbind(simulated_data,simulated_data)
# 
# lme4::lmer(cs ~ 
#               psych
#             + temp
#             + (1|ResponseId)
#            , data = x
#            #, family = gaussian
#            )
# 
# help(lmer)
# x$ResponseId
# 
# nlme::lme(cs ~ 
#             ju1_2
#             #   psych
#             # + temp
#     ,
#           data = x, random = ~ 1 | ResponseId)
# 
# 
# 
# model0 <- lmerTest::lmer(cs ~ 1
#                 #   condition
#                  + (1|ResponseId)
#                 # + (1|ResponseId:condition)
#                 , data = x
#           #      , contrasts = list(condition = contr.sum  , valence = contr.sum)
#             )
# 
# 
# 
# summary(model0)
# model1 <- lmerTest::lmer(ju1_2 ~
#                   condition5#*scenario
#                  + (1|ResponseId)
#                 # + (1|ResponseId:condition)
#                 , data = x
#                 #, contrasts = list(condition = contr.sum  )#, valence = contr.sum)
#             )
# summary(model1)
# anova(model1)



#df3$cs <- relevel(df3$cs, ref = 2)
df3 <- x
df3a <- mlogit.data(df3, choice = "cs", shape = "wide")

x_logit <- mlogit.data(x, choice = "cs", shape = "wide")

logitmodel <- mlogit(cs ~ 1 | psych+temp, data = x_logit)# , reflevel = 2)
logitmodel <- mlogit(cs ~ 1 | psych + temp + scenario + psych:temp, data = x_logit)# , reflevel = 2)
mlog

logitmodel


summary_logitmodel <- summary(logitmodel)
summary_logitmodel

summary_logitmodel$lratio$parameter
summary_logitmodel$lratio$statistic
summary_logitmodel$lratio$p.value

logitmodel$coefficients[3]
logitmodel$coefficients[4]

cox <- DescTools::PseudoR2(multinom(cs~psych,df3), "all")

cox[3]
cox[4]
#PseudoR2(x, "all")
#summary_InCS_model


wald1 <- 
  summary_InCS_model$CoefTable[3]^2 /
  summary_InCS_model$CoefTable[7]^2

wald2 <- 
  summary_InCS_model$CoefTable[4]^2 /
  summary_InCS_model$CoefTable[8]^2


summary_InCS_model
summary_InCS_model$coefficients[3]
data.frame(exp(InCSModel$coefficients))

exp(InCSModel$coefficients)[3]


a <- exp(confint(InCSModel))
c(a[3],a[7])

residuals(InCSModel)
fitted(InCSModel, outcome = F)

c <- summary_InCS_model$lratio$statistic
w <- sqrt(c/length(df3$gender))
pw <- pwr.chisq.test(w=w,N=length(df3$cs),df=(2),sig.level = .05)

pw$power

revised_PseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  modelN <- length(LogModel$fitted.values)
  R.l <- 1 - dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  
  all <- list(hosmer_and_lemeshow = as.numeric(R.l), mcfadden = NA, cox_and_snell = as.numeric(R.cs), nagelkerke = as.numeric(R.n))
  all
}

logits_rsquared <- glm(cs~psych*temp*scenario,x, family = binomial(link = "logit"))


logits_rsquared <- glm(cs~psych+temp,x, family = binomial(link = "logit"))

cox <- revised_PseudoR2s(logits_rsquared)
cox
summary(logits_rsquared)

summary_InCS_model

model0 <- glm(cs~1,x, family = binomial(link = "logit"))
model0
summary(model0)
model0

anova(model1, test = "Chisq")

anova(model0,model1, test = "Chisq")

model1 <- glm(cs~psych+temp,x, family = binomial(link = "logit"))
summary(model1)


multi1 <- multinom(cs ~ psych+temp, data = x)
summary(multi1)
multi1$
  
  glm1 <- glm(cs~psych+temp,x, family = binomial(link = "logit"))
glm0 <- update(glm1, . ~ 1)
anova(glm0,glm1,test="Chisq")

# https://stats.stackexchange.com/questions/69664/comparing-nested-glms-via-chi-squared-and-loglikelihood#69777
x <- df3




model0 <- glm(cs~1,x, family = binomial(link = "logit"))
model0
summary(model0)

model1 <- glm(cs~psych+temp,x, family = binomial(link = "logit"))
summary(model1)



model2 <- glm(cs~psych*temp,x, family = binomial(link = "logit"))
summary(model2)

model3 <- glm(cs~psych*temp+scenario,x, family = binomial(link = "logit"))

anova(model0,model1, test = "Chisq")
anova(model1,model2, test = "Chisq")
anova(model0,model1,model2, test = "Chisq")
anova(model0,model1,model2,model3, test = "Chisq")



summary(model3)

revised_PseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  modelN <- length(LogModel$fitted.values)
  R.l <- 1 - dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  
  all <- list(hosmer_and_lemeshow = as.numeric(R.l), mcfadden = NA, cox_and_snell = as.numeric(R.cs), nagelkerke = as.numeric(R.n))
  all
}

cox <- revised_PseudoR2s(model1)

aov1 <- anova(model0,model1, test = "Chisq")

aov1$Df[2]
aov1$`Pr(>Chi)`[2]
aov1$Deviance[2]

aov1[2,]$Deviance

a <- exp(confint(model1))





model0 <- glm(cs~1,x, family = binomial(link = "logit"))
model1 <- glm(cs~psych+temp,x, family = binomial(link = "logit"))

chi1 <- anova(model0,model1, test = "Chisq")[2,]
a <- exp(confint(model1))

cox <- revised_PseudoR2s(model1)


chi1$Deviance

summary(model1)

c <- summary_InCS_model$lratio$statistic
w <- sqrt(c/length(df3$gender))
pw <- pwr.chisq.test(w=w,N=length(df3$cs),df=(2),sig.level = .05)

c_full <- chisq.test(table(x$cs,x$psych))
c_full

c_full$statistic
c <- c_full$statistic
w <- sqrt(c/length(x$cs))
pw <- pwr.chisq.test(w=w,N=length(df3$cs),df=(2),sig.level = .05)
pw


#model1_summary <- 

test <- as.data.frame(summary(model1)$coefficients)
test <- as.data.frame(test)

test$`z value`
model1_summary$coefficients[2,]



model0 <- nnet::multinom(cs~1,x) 
model1 <- nnet::multinom(cs~psych+temp+psych*temp,x) 

anova(model0,model1)

model0$deviance

summary(model1)

model5$deviance - model0$deviance
broom::tidy(model5)
summary(model5)
model5$deviance



#### 8th August 2023 ####



revised_PseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  modelN <- length(LogModel$fitted.values)
  R.l <- 1 - dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  
  all <- list(hosmer_and_lemeshow = as.numeric(R.l), mcfadden = NA, cox_and_snell = as.numeric(R.cs), nagelkerke = as.numeric(R.n))
  all
}


model0 <- glm(cs~1,x, family = binomial(link = "logit"))
model1 <- glm(cs~psych+temp,x, family = binomial(link = "logit"))

chi1 <- anova(model0,model1, test = "Chisq")[2,]

c <- chi1$Deviance
w <- sqrt(c/length(x$cs))
pw <- pwr.chisq.test(w=w,N=length(x$cs),df=(chi1$Df),sig.level = .05)

a <- exp(confint(model1))

cox <- revised_PseudoR2s(model1)
cox

test <- as.data.frame(summary(model1)$coefficients)
psych_coeffs <- test[2,] # model1_summary$coefficients[2,]

psych_coeffs$`z value`





#| include: false

# mlogit_PseudoR2s <- function(model0R2,model1R2) {
#   dev <- abs(model1R2$logLik)
#   nullDev <- abs(model0R2$logLik)
#   modelN <- length(model1R2$fitted.values)
#   R.l <- 1 - dev / nullDev
#   R.cs <- 1- exp ( -(nullDev - dev) / modelN)
#   R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
#   
#   all <- list(hosmer_and_lemeshow = as.numeric(R.l), mcfadden = NA, cox_and_snell = as.numeric(R.cs), nagelkerke = as.numeric(R.n))
#   all
# }
# 
# x_interaction <- x[which(x$temp!="control"),]
# x_interaction$temp <- droplevels(x_interaction$temp)
# x_interaction <- 
#   x_interaction %>% mutate(
#     temp_recode = recode(temp, "now" = 0, "future" = 1)
#     ,psych_recode = recode(psych, "self" = 0, "other" = 1)
#   )
# 
# x_logit <- mlogit.data(x, choice = "cs", shape = "wide")
# 
# #logitmodel <- mlogit(cs ~ 1 | psych+temp, data = x_logit)# , reflevel = 2)
# model0 <- mlogit(cs ~ 1| 1 , data = x_logit)# , reflevel = 2)
# model1 <- mlogit(cs ~ 1 |  psych + temp + psych:temp, data = x_logit , reflevel = 2)
# 
# 
# summary(model1)
# summary(model0)
# 
# model1$logLik
# model0$logLik
# 
# 
# summary_model1 <- summary(model1)
# 
# 
# 
# c <- summary_model1$lratio$statistic
# w <- sqrt(c/length(x$cs))
# pw <- pwr.chisq.test(w=w,N=length(x$cs),df=(chi1$Df),sig.level = .05)
# 
# 
# 
# 
# 
# summary_model1 <- summary(model1)
# summary_model1$CoefTable
# 
# summary_model1$lratio$parameter
# summary_model1$lratio$statistic
# summary_model1$lratio$p.value
# 
# summary_model1$lratio$statistic
# 
# 
# a <- exp(confint(model1))
# 
# cox <- mlogit_PseudoR2s(model0,model1)
# cox
# 
# test <- as.data.frame(summary_model1$CoefTable)
# 
# future <- test[]




# 
# # 
# 
# 
# 
# presented a dual-process explanation of moral dumbfounding. According to this view, moral dumbfounding occurs as a result of a conflict in dual processes
# 
# In-line with a dual-process explanation of moral dumbfounding, previous research has 
# 
# The aim of the current research is to test 
# 
# Below we present eight pilot studies to test the hypothesized relationship between distancing and dumbfounded responding. 
# Recent work [@mchugh_cognitive_2023] has proposed a conflict in dual-processes [e.g., @bonner_conflict_2010] explanation of 
# moral dumbfounding. According to this view, dumbfounding occurs when a habitual response (moral judgment) is in conflict with 
# a deliberative response (providing reasons). This explanation is consistent with dual-process approaches to moral judgment
# [e.g., @bago_intuitive_2019; @cushman_action_2013; @greene_secret_2008], as well as with a unimodel [@kruglanski_intuitive_2011] 
# and categorization [@mchugh_moral_2022-1] approaches.
# 
# A key prediction of this explanation is that rates of reason-giving should be influenced by experimental manipulations that
# impact intuitive vs deliberative thinking. Previous work has demonstrated that inhibiting deliberative thinking through a 
# cognitive load manipulation can reduce reason-giving, leading to higher rates of dumbfounding [@mchugh_cognitive_2023]. 
# A corollary of this finding is that reason-giving should be increased under manipulations that encourage deliberative thinking.
# Drawing on construal-level theory [@liberman_effect_2002; @forster_temporal_2004], we predict that increased distance will 
# facilitate the identification of reasons, leading to lower levels of dumbfounded responding.



