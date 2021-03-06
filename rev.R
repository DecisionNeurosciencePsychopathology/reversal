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
setwd("~/code/reversal")

load('rev.Rda')


############# Data processing - skip and go to analysis ##############

rev_long <- read.csv(file='rev_long.csv', header = TRUE, sep=';')
rev_short <- read.csv(file='rev_short.csv', header = TRUE, sep=';')
#rev_spss is actually the same data as rev_short...
#rev_spss <- read.delim('rev2spss.dat', header=TRUE)
rev_prov <- left_join(rev_long, rev_short, by = 'ID')
View(rev_prov)

design <- read_csv("data/rev_design.csv")
rev_prov <- left_join(rev_prov, design, by = 'trial')
rev_prov$reinf <- rev_prov$stim_choice==rev_prov$corr_stim

test <- rev_prov[,c(2,3,20:23)]
View(test)
### quality checks

table(rev_prov$ID)
table(rev_prov$trial)


rev_prov$choice <- rev_prov$stim_choice
# remove trials/subjects who selected '3'
rev_prov$choice[rev_prov$choice=='3'] <- NA
#paticipant 46069 has two times the  number of trials, but different ones.
rev_prov <- rev_prov[rev_prov$trial<81,]
plot(table(rev_prov$stim_choice,rev_prov$trial))

# for Anna's records
# p <- ggplot(rev_prov,aes(stim_choice)) + geom_bar() + facet_wrap(~ID)
# ggsave("choice_test.pdf",plot = p, width = 20, height = 20)

rev_prov <- rev_prov[rev_prov$stim_choice!='3',]
rev_prov$choice <- factor(rev_prov$choice)


#missingness
library(mice)
md.pattern(rev_prov)

library(VIM)
rev_aggr = aggr(rev_prov, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(rev_prov), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

#data processing
summary(rev_prov)

rev_prov <- transform(rev_prov, ID = as.factor(ID), stim_choice = as.factor(stim_choice), stim1pos = as.factor(stim1pos), stim2pos = as.factor(stim2pos))

rev_prov <- rev_prov %>% group_by(ID) %>% mutate(choice.lag1 = lag(choice, n=1, order_by=trial),
                                                 choice.lag2 = lag(choice, n=2, order_by=trial),
                                       choice.lead1 = lead(choice, n=1, order_by=trial),
                                                 RT.lag1 = lag(RT, n=1, order_by = trial),
                                                 RT.lag2 = lag(RT, n=2, order_by = trial),
                                                 reinf.lag1 = lag(reinf, n=1, order_by = trial),
                                                 stim1pos.lag1 = lag(stim1pos, n=1, order_by = trial),
                                                 stim1pos.lag2 = lag(stim1pos, n=2, order_by = trial),
                                                 stim2pos.lag1 = lag(stim2pos, n=1, order_by=trial),
                                                 stim2pos.lag2 = lag(stim2pos, n=2, order_by=trial))
View(rev_prov)
rev_prov$stay <- rev_prov$choice==rev_prov$choice.lead1
rev_prov <- rev_prov %>% group_by(ID) %>% mutate(stay.lag1 = lag(stay, n=1, order_by=trial),
                                       stay.lag2 = lag(stay, n=2, order_by=trial))


#adding demographic variables
rev_demog <- readxl::read_excel("rev_iowa_demog.xlsx")
rev_demog <- transform(rev_demog, ID = as.factor(ID), race = as.factor(race), ethnicity = as.factor(ethnicity), gender = as.factor(gender),
                       marital = as.factor(marital), group1_5 = as.factor(group1_5), group1_7 = as.factor(group1_7), group1_5text = as.factor(group1_5text),  group1_5text2 = as.factor(group1_5text2),
                       method_most_lethal = as.factor(method_most_lethal), anxiety_lifetime = as.factor(anxiety_lifetime),substance_lifetime = as.factor(substance_lifetime), anxiety_current = as.factor(anxiety_current),substance_current = as.factor(substance_current))
rev_demog <- transform(rev_demog, consent_date_baseline = as.Date(consent_date_baseline), EXIT_date = as.Date(EXIT_date), DRS_date = as.Date(DRS_date))
rev_demog$lethality_max <- as.numeric(rev_demog$lethality_max)
rev_demog$intent_worst_planning <- as.numeric(rev_demog$intent_worst_planning)
summary(rev_demog)



#creating additional group subdivisions
# creating labels for low- and high-lethality groups
rev_demog$group1_7_labels[rev_demog$group1_7 == '1'] <- 'healthy controls'
rev_demog$group1_7_labels[rev_demog$group1_7 == '2'] <- 'depressed controls'
rev_demog$group1_7_labels[rev_demog$group1_7 == '4'] <- 'ideators'
rev_demog$group1_7_labels[rev_demog$group1_7 == '6'] <- 'low-lethality attempters'
rev_demog$group1_7_labels[rev_demog$group1_7 == '7'] <- 'high-lethality attempters'

rev_demog$group1_7_labels <- factor(rev_demog$group1_7_labels)

# early vs. late-onset = before or after 50 years of age (median of age at 1st attempt = 50.5)
median(rev_demog$age_first_attempt[rev_demog$group1_5=='5'])

rev_demog$gp_age <- factor(rev_demog$group1_5, levels=c(levels(rev_demog$group1_5), '10', '11'))
rev_demog$gp_age[rev_demog$age_first_attempt < 51] <- '10'
rev_demog$gp_age[rev_demog$age_first_attempt > 50] <- '11'

rev_demog$gp_age <- factor(rev_demog$gp_age)

rev_demog$gp_age_labels[rev_demog$gp_age == '1'] <- 'healthy controls'
rev_demog$gp_age_labels[rev_demog$gp_age == '2'] <- 'depressed controls'
rev_demog$gp_age_labels[rev_demog$gp_age == '4'] <- 'ideators'
rev_demog$gp_age_labels[rev_demog$gp_age == '10'] <- 'early-onset attempters'
rev_demog$gp_age_labels[rev_demog$gp_age == '11'] <- 'late-onset attempters'

rev_demog$gp_age_labels <- factor(rev_demog$gp_age_labels)

# low- vs. high-planning = intent scale, planning subscale (median of planning score in all attempters = 8)
median(rev_demog$intent_worst_planning[rev_demog$group1_5=='5'], na.rm = TRUE)

rev_demog$gp_planning <- factor(rev_demog$group1_5, levels=c(levels(rev_demog$group1_5), '14', '15'))

rev_demog$gp_planning[rev_demog$intent_worst_planning < 9] <- '14'
rev_demog$gp_planning[rev_demog$intent_worst_planning > 8] <- '15'

rev_demog$gp_planning <- factor(rev_demog$gp_planning)

rev_demog$gp_planning_labels[rev_demog$gp_planning == '1'] <- 'healthy controls'
rev_demog$gp_planning_labels[rev_demog$gp_planning == '2'] <- 'depressed controls'
rev_demog$gp_planning_labels[rev_demog$gp_planning == '4'] <- 'ideators'
rev_demog$gp_planning_labels[rev_demog$gp_planning == '14'] <- 'low-planning attempters'
rev_demog$gp_planning_labels[rev_demog$gp_planning == '15'] <- 'high-planning attempters'

rev_demog$gp_planning_labels <- factor(rev_demog$gp_planning_labels)

# saving dataset
save(rev_demog, file = 'rev_demog.Rda')

rev <- left_join(rev_prov, rev_demog, by=c("ID"))
DEMO<-readxl::read_xlsx('ALL_SUBJECTS_DEMO.xlsx')
rev$group1_7[which(is.na(rev$group1_7))]<-DEMO$GROUP12467[match(rev$ID[which(is.na(rev$group1_7))],DEMO$ID)]

rev$group1_7_labels[rev$group1_7 == '1'] <- 'healthy controls'
rev$group1_7_labels[rev$group1_7 == '2'] <- 'depressed controls'
rev$group1_7_labels[rev$group1_7 == '4'] <- 'ideators'
rev$group1_7_labels[rev$group1_7 == '6'] <- 'low-lethality attempters'
rev$group1_7_labels[rev$group1_7 == '7'] <- 'high-lethality attempters'
rev<-rev[which(!is.na(rev$group1_7)),] #221595 is actually from BPD? not even old enough...lol
save(rev, file = 'rev.Rda')
write.csv(rev,file="rev.csv")



#adding demographic variables to short version of dataset (no trial-by-trial variables)
rev_short$ID <- as.factor(rev_short$ID)
rev_short <- left_join(rev_short, rev_demog, by=c("ID"))

save(rev_short, file = 'rev_short.Rda')

######################## Analysis begins here ###########################

#correlations, plots and histograms
barchart(rev$choice)
hist(rev$RT)

# learning curves
p <- ggplot(rev,aes(trial,as.numeric(choice))) + geom_line() + facet_wrap(~ID)
ggsave("rev_learning_curves_individual.pdf", p, width = 20, height = 20)

# giant spaghetti plot -- some variability, not a whole lot

p <- ggplot(rev,aes(trial,as.numeric(choice), color = group1_7_labels)) + geom_smooth(method = 'loess') 
ggsave("rev_smooth_learning_curves_individual.pdf", p, width = 20, height = 20)

p <- ggplot(rev[!is.na(rev$group1_7_labels) & rev$trial>41,],aes(trial,as.numeric(choice)-1, color = group1_7_labels)) +
  geom_smooth(method="glm", method.args = list(family = "binomial"), formula = y ~ splines::ns(x, 3)) 
ggsave("post_rev_smooth_learning_curves_by_group.pdf", p, width = 6, height = 6)

# reward
p <- ggplot(rev[!is.na(rev$group1_7_labels),],aes(trial,as.numeric(reinf), color = group1_7_labels)) + 
  geom_smooth(method="glm", method.args = list(family = "binomial"), formula = y ~ splines::ns(x, 6)) 
ggsave("rev_smooth_reinf_by_group.pdf", p, width = 20, height = 20)


# inspect RT timecourses
p <- ggplot(rev,aes(trial,1000/RT)) + geom_line() + facet_wrap(~ID)
ggsave("rev_rt_timecourses_individual.pdf", p, width = 20, height = 20)


p <- ggplot(rev[rev$trial %in% 11:80,],aes(trial/10,1000/RT)) + geom_line() + facet_wrap(~ID)
# demographic table
library(compareGroups)
chars <- rev_short[,c('age_baseline','race','gender','education','marital','HRSD_no_suic', 'anxiety_lifetime', 'substance_lifetime','EXIT_total', 'DRS_total', 'age_first_attempt','attempts_tot_baseline','attempts_tot_followUp','ideation_current_baseline','intent_worst_total','intent_worst_planning','lethality_max')]
# describe.by(chars,group = df$group_early_no_break)

rev_short$HRSD_no_suic[rev_short$group1_5 == '1'] <- NA
rev_short$substance_lifetime[rev_short$group1_5 == '1'] <- NA
rev_short$anxiety_lifetime[rev_short$group1_5 == '1'] <- NA
rev_short$ideation_current_baseline[rev_short$group1_5 == '1'| rev_short$group1_5 == '2'] <- NA
rev_short$lethality_max[rev_short$group1_5 == '1'| rev_short$group1_5 == '2'| rev_short$group1_5 == '4'] <- NA
rev_short$age_first_attempt[rev_short$group1_5 == '1'| rev_short$group1_5 == '2'| rev_short$group1_5 == '4'] <- NA
rev_short$intent_worst_planning[rev_short$group1_5 == '1'| rev_short$group1_5 == '2'| rev_short$group1_5 == '4'] <- NA
rev_short$intent_worst_total[rev_short$group1_5 == '1'| rev_short$group1_5 == '2'| rev_short$group1_5 == '4'] <- NA
rev_short$attempts_tot_baseline[rev_short$group1_5 == '1'| rev_short$group1_5 == '2'| rev_short$group1_5 == '4'] <- NA
rev_short$attempts_tot_followUp[rev_short$group1_5 == '1'| rev_short$group1_5 == '2'| rev_short$group1_5 == '4'] <- NA

c <- compareGroups(chars,rev_short$group1_5, show.descr = TRUE)
tc <- createTable(c, hide.no = 0, digits = 1, show.p.mul = TRUE)
tc

export2html(tc, "reversal_table1_groups1_5.html")

#missingness for demographics
library(mice)
md.pattern(rev_short)

library(VIM)
rev_short_aggr = aggr(rev_short, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(rev_short), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

rev_short$group1_5[is.na(rev_short$HRSD)]

# make HL the reference group
rev$group1_7_labels <- relevel(rev$group1_7_labels, ref = 'high-lethality attempters')
rev$group1_7_txt_LL <- relevel(rev$group1_7_labels, ref = 'low-lethality attempters')


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

post0 <-   glmer(
  stay ~  scale(trial) * reinf  +
    (1 | ID),
  family = binomial(),
  data = rev[rev$trial>40,],
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(c0post)
car::Anova(c0post, type = 'III')

pre0 <-   glmer(
  stay ~  scale(trial) * reinf  +
    (1 | ID),
  family = binomial(),
  data = rev[rev$trial<41,],
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(c0pre)
car::Anova(c0pre, type = 'III')

# add group
post1 <-   glmer(
  stay ~  (scale(-1/trial) + reinf + group1_7_labels)^2  +
    (1 | ID),
  family = binomial(),
  data = rev[rev$trial>40,],
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(post1)
car::Anova(post1, '3')

# how about simple choice
post1a <-   glmer(
  as.factor(stim_choice) ~  scale(-1/trial) * group1_5  +
    (1 | ID),
  family = binomial(),
  data = rev[rev$trial>40,],


  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(post1a)
car::Anova(post1a, '3')

post1b <-   glmer(
  as.factor(stim_choice) ~  scale(-1/trial) * group1_7_labels  +
    (1 | ID),
  family = binomial(),
  data = rev[rev$trial>41,],
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(post1b)
car::Anova(post1b, '3')

post1c <-   glmer(
  as.factor(stim_choice) ~  scale(-1/trial) * group1_7_labels  + scale(-1/trial) * scale(age_baseline) +
    (1 | ID),
  family = binomial(),
  data = rev[rev$trial>41,],
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(post1c)
car::Anova(post1c, '3')

post1d <-   glmer(
  as.factor(stim_choice) ~  scale(-1/trial) * group1_7_txt_LL  + 
    scale(-1/trial) * scale(age_baseline) + scale(-1/trial) * scale(education) +
     (1 | ID),
  family = binomial(),
  data = rev[rev$trial>41,],
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(post1d)
car::Anova(post1c, '3')


pre1b <-   glmer(
  stim_choice ~  scale(-1/trial) * group1_7  +
    (1 | ID),
  family = binomial(),
  data = rev[rev$trial<41,],
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(pre1b)
car::Anova(pre1b, type = 'III')


# diagnose RT data, deal with outliers
hist(rev$RT[rev$RT<4000])

rt_dist <- psych::describe(rev$RT, IQR = TRUE, quant = c(.9, .95,.99))
upper <- 4000
lower <- 200
revclean <- rev[rev$RT>lower & rev$RT<upper,]
revclean$newgrp<-factor(revclean$group1_7_labels,levels = c("healthy controls","depressed controls","ideators","low-lethality attempters","high-lethality attempters"))

# inspect RT timecourses
p <- ggplot(revclean,aes(trial,1000/RT,color = group1_7_labels)) + geom_line() + facet_wrap(~ID)
ggsave("censored_rev_rt_timecourses_individual.pdf", p, width = 20, height = 20)
p <- ggplot(revclean,aes(trial,1000/RT)) + geom_smooth(method = 'loess')
ggsave("censored_rev_rt_timecourses_group.pdf", p, width = 8, height = 6)

p <- ggplot(revclean[!is.na(revclean$group1_7),],aes(trial,-1000/RT, color = group1_7_labels)) + geom_smooth(method = 'loess')
ggsave("censored_rev_rt_timecourses_by_study_group.pdf", p, width = 8, height = 6)


# example LME on censored RT data
load("/Volumes/bek/reversal/reversalvba.rdata")
rev_vba_df<-do.call(rbind,lapply(as.list(tempenvir),function(dfx){dfx$PE_lag<-dplyr::lag(scale(dfx$PE));dfx$trial<-1:nrow(dfx);dfx$pre_post<-as.numeric(dfx$trial>40);return(dfx)}))
revclean_wvba<-merge(revclean,rev_vba_df,by.x = c("ID","trial"),by.y = c("ID","trial"),all.x = T)
revclean_wvba$pre_post[is.na(revclean_wvba$pre_post)]<-1
revclean_wvba$pre_post<-as.factor(revclean_wvba$pre_post)
r0 <- lme4::lmer(-1000/(RT) ~ scale(-1000/(RT.lag1))*pre_post + scale(-1/trial)*pre_post + reinf.lag1* newgrp*pre_post + stay.lag1* newgrp*pre_post + abs(PE_lag) * newgrp*pre_post +
                       (1 | ID),
                     data = revclean_wvba[revclean$trial<=40,])
summary(r0)
car::Anova(r0,'3')

r1 <- lmerTest::lmer(-1000/(RT) ~ scale(-1000/(RT.lag1)) + scale(-1/trial) + 
                       (reinf.lag1 * stay.lag1 * newgrp) +
                   (1 | ID),
                 data = revclean[revclean$trial<=40,])
summary(r1)
car::Anova(r0,'3')



r2<- lmerTest::lmer(-1000/(RT) ~ scale(-1000/(RT.lag1)) + scale(-1/trial) + 
                        (reinf.lag1 + stay.lag1 + newgrp + pre_post) ^2 +reinf.lag1 * newgrp * pre_post
                        (1 | ID),
                      data = rev_2)
summary(r2)
car::Anova(r2,'3')



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

revclean_sp<-split(revclean,revclean$ID)
rev_ref<-data.frame(trial=1:80,pre_post=c(rep(0,40),rep(1,40)))

rev_2<-do.call(rbind,lapply(revclean_sp,function(dfx){
  #message(unique(dfx$ID))
  dfx$pre_post<-as.factor(rev_ref$pre_post[match(dfx$trial,rev_ref$trial)])
  return(dfx)
}))


rev$stim_choice <- as.factor(rev$stim_choice)
# Use the dataframe in ggplot
ggplot(data=rev, aes(x=trial, y=stim_choice, color=ID, group = ID)) +
  geom_line()


library(corrplot)

corrplot.mixed(cor(rev[,c(2,4,7:11)], method = "spearman", use = "na.or.complete"), lower.col = "black")







###@#In connection to Matlab:
rev$rein<-as.numeric(rev$reinf)
rev$rein[which(rev$rein==0)]<- -1
rev_sp<-split(rev,rev$ID)
NU<-lapply(rev_sp,function(x) {
  unique(x$ID)->idx
  write.table(data.frame(ID=as.numeric(x$ID),action=as.numeric(x$stim_choice),reinf=as.numeric(x$rein)),file.path(getwd(),"vba_b",paste0(idx,"rev_vba.csv")),row.names = F,col.names = F,sep = ",")
})



####Let's sample 10 or 20 trials;
samplerate<-4
gx<-rep(1:(80/samplerate),samplerate)
gx<-gx[order(gx)]
dfgx<-data.frame(trial=1:80,gx=gx)
rev_sumstats<-lapply(rev_sep,function(dfx){
  dfx$gxindx<-dfgx$gx [match(dfx$trial,dfgx$trial)]
  dfx$switch<- dfx$stim_choice!=lag(dfx$stim_choice)
  lsx<-split(dfx,dfx$gxindx)
  dfy_all<-do.call(rbind,lapply(lsx,function(dfy){
    data.frame(
    stim1_picked=length(which(as.character(dfy$stim_choice)=='1'))/samplerate,
    stim2_picked=length(which(as.character(dfy$stim_choice)=='2'))/samplerate,
    swtichrate=length(which(dfy$switch))/samplerate,
    winstay=length(which(!dfy$switch & dfy$reinf.lag1))/length(which(dfy$reinf.lag1)),
    winstay_s1=length(which(!dfy$switch & dfy$reinf.lag1 & as.character(dfy$stim_choice)=='1'))/length(which(dfy$reinf.lag1 & as.character(dfy$stim_choice)=='1')),
    winstay_s2=length(which(!dfy$switch & dfy$reinf.lag1 & as.character(dfy$stim_choice)=='2'))/length(which(dfy$reinf.lag1 & as.character(dfy$stim_choice)=='2')),
    loseswitch=length(which(dfy$switch & !dfy$reinf.lag1))/samplerate,
    winswitch=length(which(dfy$switch & dfy$reinf.lag1))/samplerate,
    losestay=length(which(!dfy$switch & !dfy$reinf.lag1))/samplerate,
    losestay_s1=length(which(!dfy$switch & !dfy$reinf.lag1 & as.character(dfy$stim_choice)=='1'))/samplerate,
    losestay_s2=length(which(!dfy$switch & !dfy$reinf.lag1 & as.character(dfy$stim_choice)=='2'))/samplerate,
    u_rt_overall=mean(dfy$RT,na.rm=T),
    u_rt_s1=mean(dfy$RT[dfy$stim_choice=="1"],na.rm=T),
    u_rt_s2=mean(dfy$RT[dfy$stim_choice=="2"],na.rm=T),
    u_rt_switch=mean(dfy$RT[dfy$switch],na.rm=T),
    u_rt_reinf=mean(dfy$RT[dfy$reinf],na.rm=T),
    u_rt_winstay=mean(dfy$RT[!dfy$switch & dfy$reinf.lag1],na.rm=T),
    u_rt_loseswitch=mean(dfy$RT[dfy$switch & !dfy$reinf.lag1],na.rm=T),
    u_rt_winswitch=mean(dfy$RT[dfy$switch & dfy$reinf.lag1],na.rm=T),
    u_rt_losestay=mean(dfy$RT[!dfy$switch & !dfy$reinf.lag1],na.rm=T)
    )
  })
  )
  dfy_all$epo_indx<-1: (80/samplerate)
  dfy_all$ID<-unique(dfx$ID)
  dfy_all$grp<-unique(dfx$group1_7_labels)
  dfy_all$one_over_two<-dfy_all$stim1_picked > dfy_all$stim2_picked
  return(dfy_all)
})

rev_sum<-do.call(rbind,rev_sumstats)
rev_sum<-rev_sum[!is.na(rev_sum$grp),]
ggplot(rev_sum,aes(x=epo_indx,y = loseswitch,fill=grp))+geom_histogram(stat="summary",fun.y="mean",position="dodge")




# plot moving stats for win-switches and lose-switches
rev<-rev[order(rev$ID),]
rev_sp<-split(rev,rev$ID)
rev<-do.call(rbind,lapply(rev_sp,function(dfx){
  dfx$RT_sc<-scale(dfx$RT,center = T,scale = T)
  dfx$pre_post<-as.numeric(dfx$trial>40)
  return(dfx)
}))

rev$reinf_lag_char<-NA
rev$reinf_lag_char[rev$reinf.lag1]<-"Reinf"
rev$reinf_lag_char[!rev$reinf.lag1]<-"No_reinf"

ggplot(rev[!is.na(rev$reinf.lag1),], aes(trial,as.numeric(stay),  color = group1_7_labels)) + geom_smooth(method = "glm", method.args = list(family = "binomial"), formula = y ~ splines::ns(x, 8)) + 
  facet_wrap(~reinf.lag1)

ggplot(rev[!is.na(rev$reinf.lag1) & !is.na(rev$stay),], aes(trial,RT_sc,  color = group1_7_labels)) + geom_smooth() + 
  facet_wrap(~reinf_lag_char+stay)

ggplot(rev[!is.na(rev$reinf.lag1),], aes(trial,RT_sc,  color = group1_7_labels)) + geom_smooth() + 
  facet_wrap(~reinf_lag_char+stim_choice)

c0 <-   glmer(
  stay ~  scale(trial) * reinf  +
    (1 | ID),
  family = binomial(),
  data = rev,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(c0)
car::Anova(c0, type = 'III')


rev_design <- read.csv("data/rev_design.csv",stringsAsFactors = F)
ggplot(rev_design, aes(trial,as.numeric(corr_stim))) + geom_point() + facet_wrap(~stim1feed)


