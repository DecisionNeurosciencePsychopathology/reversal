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
#setwd("~/code//dominance")

load('rev.Rda')

rev_long <- read.csv(file='rev_long.csv', header = TRUE, sep=';')
rev_short <- read.csv(file='rev_short.csv', header = TRUE, sep=';')
#rev_spss is actually the same data as rev_short...
#rev_spss <- read.delim('rev2spss.dat', header=TRUE)
rev <- left_join(rev_long, rev_short, by = 'ID')
View(rev)


### quality checks

table(rev$ID)
table(rev$trial)

#paticipant 46069 has two times the  number of trials, but different ones.

#missingness
library(mice)
md.pattern(rev)

library(VIM)
rev_aggr = aggr(rev, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(rev), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

#data processing
summary(rev)

rev <- transform(rev, ID = as.factor(ID), stim_choice = as.factor(stim_choice), stim1pos = as.factor(stim1pos), stim2pos = as.factor(stim2pos))

rev <- rev %>% group_by(ID) %>% mutate(stim_choice.minus1 = lag(stim_choice, n=1, order_by=trial),
                                                 stim_choice.minus2 = lag(stim_choice, n=2, order_by=trial),
                                                 RT.minus1 = lag(RT, n=1, order_by = trial),
                                                 RT.minus2 = lag(RT, n=2, order_by = trial),
                                                 stim1pos.minus1 = lag(stim1pos, n=1, order_by = trial),
                                                 stim1pos.minus2 = lag(stim1pos, n=2, order_by = trial),
                                                 stim2pos.minus1 = lag(stim2pos, n=1, order_by=trial),
                                                 stim2pos.minus2 = lag(stim2pos, n=2, order_by=trial))
View(rev)

save(rev, file = 'rev.Rda')

#correlations, plots and histograms
barchart(rev$stim_choice)
hist(rev$RT)

rev_id <- read.csv('rev_ID.csv')
rev_id$ID <- as.character(rev_id$ID)

View(rev_id)
for(i in 1:length(rev_id))
  x <- rev[rev$ID == rev_id[i,],]
{ ggplot(data=x, aes(x=trial, y=stim_choice)) +
    geom_line()
  
}



rev$stim_choice <- as.factor(rev$stim_choice)
# Use the dataframe in ggplot
ggplot(data=rev, aes(x=trial, y=stim_choice, color=ID, group = ID)) +
  geom_line()


library(corrplot)

corrplot.mixed(cor(rev[,c(2,4,7:11)], method = "spearman", use = "na.or.complete"), lower.col = "black")

