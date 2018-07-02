library(readxl)
library(readr)
library(lme4) # this is the one to use for modeling
library(ggplot2)
library(dplyr)
library(tidyr)
library(psych)
library(gdata)
library(R.matlab)
library(xtable)
library(Hmisc)
library(foreign)
library(MASS)
library("lsmeans")
library(effects)
library(arm)

## set working directory (where you have the participants' output data files )
setwd("~/Dropbox/USA/Pittsburgh/GitHub/reversal")

# wd for Alex
#setwd("~/code//dominance")

# clear environment
rm(list=ls())

## set working directory (where you have the participants' output data files )
setwd("~/Dropbox/USA/Pittsburgh/GitHub/reversal")

# wd for Alex
#setwd("~/code/reversal")

load('rev.Rda')

rev_long <- read.csv(file='rev_long.csv', header = TRUE, sep=';')
rev_short <- read.csv(file='rev_short.csv', header = TRUE, sep=';')
#rev_spss is actually the same data as rev_short...
#rev_spss <- read.delim('rev2spss.dat', header=TRUE)
rev <- left_join(rev_long, rev_short, by = 'ID')
View(rev)

design <- read_csv("data/rev_design.csv")
rev <- left_join(rev, design, by = 'trial')
rev$reinf <- rev$stim_choice==rev$corr_stim

test <- rev[,c(2,3,20:23)]
View(test)
### quality checks

table(rev$ID)
table(rev$trial)


rev$choice <- rev$stim_choice
# remove trials/subjects who selected '3'
rev$choice[rev$choice=='3'] <- NA
#paticipant 46069 has two times the  number of trials, but different ones.
rev <- rev[rev$trial<81,]
plot(table(rev$stim_choice,rev$trial))

# for Anna's records
# p <- ggplot(rev,aes(stim_choice)) + geom_bar() + facet_wrap(~ID)
# ggsave("choice_test.pdf",plot = p, width = 20, height = 20)

rev <- rev[rev$stim_choice!='3',]
rev$choice <- factor(rev$choice)


#missingness
library(mice)
md.pattern(rev)

library(VIM)
rev_aggr = aggr(rev, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(rev), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

#data processing
summary(rev)

rev <- transform(rev, ID = as.factor(ID), stim_choice = as.factor(stim_choice), stim1pos = as.factor(stim1pos), stim2pos = as.factor(stim2pos))

rev <- rev %>% group_by(ID) %>% mutate(choice.lag1 = lag(choice, n=1, order_by=trial),
                                                 choice.lag2 = lag(choice, n=2, order_by=trial),
                                       choice.lead1 = lead(choice, n=1, order_by=trial),
                                                 RT.lag1 = lag(RT, n=1, order_by = trial),
                                                 RT.lag2 = lag(RT, n=2, order_by = trial),
                                                 reinf.lag1 = lag(reinf, n=1, order_by = trial),
                                                 stim1pos.lag1 = lag(stim1pos, n=1, order_by = trial),
                                                 stim1pos.lag2 = lag(stim1pos, n=2, order_by = trial),
                                                 stim2pos.lag1 = lag(stim2pos, n=1, order_by=trial),
                                                 stim2pos.lag2 = lag(stim2pos, n=2, order_by=trial))
View(rev)
rev$stay <- rev$choice==rev$choice.lead1
rev <- rev %>% group_by(ID) %>% mutate(stay.lag1 = lag(stay, n=1, order_by=trial),
                                       stay.lag2 = lag(stay, n=2, order_by=trial))
                                       
save(rev, file = 'rev.Rda')


#correlations, plots and histograms
barchart(rev$choice)
hist(rev$RT)

# learning curves
p <- ggplot(rev,aes(trial,as.numeric(choice))) + geom_line() + facet_wrap(~ID)
ggsave("rev_learning_curves_individual.pdf", p, width = 20, height = 20)

# inspect RT timecourses
p <- ggplot(rev,aes(trial,1000/RT)) + geom_line() + facet_wrap(~ID)
ggsave("rev_rt_timecourses_individual.pdf", p, width = 20, height = 20)


# toy regressions
# choice
c0 <-   glmer(
  stay ~  scale(trial) * reinf  +  
    (1 | ID),
  family = binomial(),
  data = rev,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(c0)
car::Anova(c0, type = 'III')

c0post <-   glmer(
  stay ~  scale(trial) * reinf  +  
    (1 | ID),
  family = binomial(),
  data = rev[rev$trial>40,],
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(c0post)
car::Anova(c0post, type = 'III')

c0pre <-   glmer(
  stay ~  scale(trial) * reinf  +  
    (1 | ID),
  family = binomial(),
  data = rev[rev$trial<41,],
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(c0pre)
car::Anova(c0pre, type = 'III')

# diagnose RT data, deal with outliers
hist(rev$RT[rev$RT<4000])

rt_dist <- psych::describe(rev$RT, IQR = TRUE, quant = c(.9, .95,.99))
upper <- 4000
lower <- 200
revclean <- rev[rev$RT>lower & rev$RT<upper,]
# inspect RT timecourses
p <- ggplot(revclean,aes(trial,1000/RT)) + geom_line() + facet_wrap(~ID)
ggsave("censored_rev_rt_timecourses_individual.pdf", p, width = 20, height = 20)
p <- ggplot(revclean,aes(trial,1000/RT)) + geom_smooth(method = 'loess')
ggsave("censored_rev_rt_timecourses_group.pdf", p, width = 8, height = 6)

# example LME on censored RT data

r0 <- lme4::lmer(1000/(RT) ~ I(1000/(RT.lag1)) + scale(trial) + reinf.lag1 * stay.lag1 +
                       (1 | ID),
                     data = revclean)
summary(r0)
car::Anova(r0,'3')

# rev_id <- read.csv('rev_ID.csv')
# rev_id$ID <- as.character(rev_id$ID)
# 
# View(rev_id)
# for(i in 1:length(rev_id))
#   x <- rev[rev$ID == rev_id[i,],]
# { ggplot(data=x, aes(x=trial, y=stim_choice)) +
#     geom_line()
#   
# }



rev$stim_choice <- as.factor(rev$stim_choice)
# Use the dataframe in ggplot
ggplot(data=rev, aes(x=trial, y=stim_choice, color=ID, group = ID)) +
  geom_line()


library(corrplot)

corrplot.mixed(cor(rev[,c(2,4,7:11)], method = "spearman", use = "na.or.complete"), lower.col = "black")

