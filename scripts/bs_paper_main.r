#source('bsHoriz_processAllSubjects_allExperiments.R')
#source('./plots_paper/bs_ggplot_theme.R')
#load('AllDataFrame.RDATA') #allDataFrame <- createAllDataFrame()
library(ggplot2)
library(plyr)
#dat.raw = subset(allDat,allDat$removeReason!='Single');
#dat.raw = allDat
#dat.raw = allDataFrame;

#dat = subset(dat.raw,with(dat.raw,success==1))#subset(dat.raw, with(dat.raw,subject!=26&subject!='M'&success == 1))
#dat$subject = factor(dat$subject)

#dat.orth = subset(dat,dat$stimPVisible == 2)
#dat.uni  = subset(dat,dat$stimPVisible != 2)
#----
get_prob_answer = function(dat){ ddply(dat, c("stimLLoc", "stimRLoc", "controlTrial", "subject"), summarise, prob=mean(answer))}
get_prob_correct = function(dat){ ddply(dat, c("stimLLoc", "stimRLoc", "controlTrial", "subject"), summarise, prob=mean(correct))}
normalize_answer = function(probdat){
  subjectNumbers = as.vector(unique(probdat$subject))
  means = tapply(probdat$prob,probdat$subject,FUN=mean)       # Errechnet Mittelwerte von Answer der einzelnen Subjects
  probMinusMean = NULL
  
  for(subject in subjectNumbers){                                         # Zieht bei jeder Antworthaeufigkeit des Subjects seinen Mean ab (normalisiert Bias)
    probMinusMean[probdat$subject == subject] = probdat$prob[probdat$subject == subject] - means[match(subject, names(means))]
  }
  probdat$prob = probMinusMean #Fuegt probMinusMean dem Data Frame hinzu
  return(probdat)
}
#----
# 
# plot_data = function(dat){
#   ggplot(dat, aes(x = factor(controlTrial), y = prob*100, color=factor(2*stimRLoc+stimLLoc),fill=factor(2*stimRLoc+stimLLoc)))+
#     geom_jitter(alpha=.3,aes(color=factor(2*stimRLoc+stimLLoc)),position = position_jitterdodge(jitter.width = .1,jitter.height=0)) +
#     stat_summary(fun.data = "mean_cl_boot", geom = "errorbar",position=position_dodge(width=.75))+
#     stat_summary(fun.data = "mean_cl_boot", geom = "point",position=position_dodge(width=.75),size=5)+
#     #geom_boxplot(outlier.shape = NA,fill=NA)+
#     
#     scale_x_discrete(breaks = 0:3, labels=c("outward \n (O . . + . . O)", "Blind Spot \n (. O . + . O .)","Inward \n (. . O + O . .)","Above\n (. ° . + . ° .)")) +
#     labs(x =NULL, y = "Answer 'right' (normalized) [%]") +
#     coord_cartesian(ylim = c(55, 105)) +
#     scale_color_discrete(name = "Stimulus 'Einteilung'", 
#                         labels = c("Both Nasal","Left Nasal / Right Temporal ", "Right Nasal / Left Temporal", "Both Temporal") ) +tBE(base_family = 'Helvetica Neue')
# }
# 
# plot_data(get_prob_correct(dat.orth))+labs(y='Inset correctly Identified [%]')+geom_abline(intercept = 50, slope = 0) +coord_cartesian(ylim=c(0,110))
# plot_data((get_prob_answer(dat.uni)))+coord_cartesian(ylim = c(-50,50))+geom_abline(intercept = 0, slope = 0)
# 

