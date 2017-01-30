source('bs_ggplot_theme.R')
source('bs_paper_main.r')

setwd('/net/store/nbp/users/behinger/projects/blindspot/git/scripts')


library(boot)
library(ggplot2)
library(plyr)
library(ggmcmc)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores()-1)


reload_data = 0
if(reload_data){
 source('bs_load_all_data.R')
 allDat = bs_load_all_data()
 save('allDat',file='../data/behavioural/2017-01-27_data.RDATA') #allDataFrame <- bs_load_all_data() from bs_load_all_data
}

load(file='../data/behavioural/2017-01-27_data.RDATA') #allDataFrame <- bs_load_all_data() from bs_load_all_data


dat      = subset(allDat,allDat$remove==F&allDat$success==1);
dat.orth = subset(dat,dat$stimPVisible == 2)
dat.uni  = subset(dat,dat$stimPVisible != 2)

dat.uni$subjectIndex = as.numeric(factor(dat.uni$subject))
dat.uni$controlTrial = as.factor(dat.uni$controlTrial)

dat.orth$subjectIndex = as.numeric(factor(dat.orth$subject))
dat.orth$controlTrial = as.factor(dat.orth$controlTrial)


dat.exp1 = subset(dat.uni,dat.uni$experiment=='EEG')
dat.exp1$subject = factor(dat.exp1$subject)
dat.exp2 = subset(dat.uni,dat.uni$experiment=='Control')
dat.exp2$subject = factor(dat.exp2$subject)
dat.exp2$controlTrial = factor(dat.exp2$controlTrial)
dat.exp3 = subset(dat.uni,dat.uni$experiment=='Horizontal')
dat.exp3$subject = factor(dat.exp3$subject)
dat.exp3$controlTrial = factor(dat.exp3$controlTrial)
dat.exp4a = subset(dat.uni,dat.uni$experiment=='Inset4a')
dat.exp4a$subject = factor(dat.exp4a$subject)
dat.exp4b = subset(dat.uni,dat.uni$experiment=='Inset4b')
dat.exp4b$subject = factor(dat.exp4b$subject)
dat.exp4b$controlTrial = factor(dat.exp4b$controlTrial)
