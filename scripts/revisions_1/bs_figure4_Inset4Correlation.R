library(ggplot2)
library(magrittr)
source('revisions_1/bs_init.R')


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


d4a = get_prob_answer(subset(dat.uni,dat.uni$experiment == 'Inset4a'))
d4b = get_prob_answer(subset(dat.uni,dat.uni$controlTrial == 1 & dat.uni$experiment == 'Inset4b'&dat.uni$stimLLoc!=dat.uni$stimRLoc))

d4a = normalize_answer(d4a)
d4b = normalize_answer(d4b)
d4a = subset(d4a,d4a$stimLLoc!=d4a$stimRLoc & d4a$subject != 'Inset4a.14')
d4b = subset(d4b,d4b$stimLLoc!=d4b$stimRLoc & d4b$subject != 'Inset4a.14')



v = cbind(d4a$prob[d4a$stimLLoc == 0],d4b$prob[d4b$stimLLoc == 0])%>%prcomp%$%rotation
x1x2corstimLLoc0 = bCor = v[2,1]/v[1,1]
v = cbind(d4a$prob[d4a$stimLLoc == 1],d4b$prob[d4b$stimLLoc == 1])%>%prcomp%$%rotation
x1x2corstimLLoc1 = bCor = v[2,1]/v[1,1]




qplot(d4a$prob,d4b$prob,color=factor(d4b$stimLLoc))+
  geom_hline(yintercept = 0,color='lightgray')+geom_vline(xintercept = 0,color='lightgray')+
  geom_abline(slope=-1,intercept = 0)+
  coord_cartesian(xlim=c(-.5,.5),ylim=c(-.5,.5)) + 
  geom_abline(intercept = 0,slope = x1x2corstimLLoc0,color='red')+
  geom_abline(intercept = 0,slope = x1x2corstimLLoc1,color='turquoise')+
  tBE(20)+coord_fixed(xlim=c(-0.5,0.5),ylim=c(-0.5,0.5))+
  annotate('text',x= 0.4,y=0.4,
           label=paste0('corr:',round(cor(d4a$prob,d4b$prob),2)))+
  annotate('text',x= 0.4,y=0.35,color='turquoise',
           label=paste0('corr:',round(cor(d4a$prob[d4a$stimLLoc==1],d4b$prob[d4b$stimLLoc==1]),2)))+
  annotate('text',x= 0.4,y=0.3,color='red',
           label=paste0('corr:',round(cor(d4a$prob[d4a$stimLLoc==0],d4b$prob[d4b$stimLLoc==0]),2)))+
  xlab('Inset 4a') + ylab('Inset 4b')


cor(d4a$prob,d4b$prob)

cor(d4a$prob[d4a$stimLLoc==1],d4b$prob[d4b$stimLLoc==1])
cor(d4a$prob[d4a$stimLLoc==0],d4b$prob[d4b$stimLLoc==0])

ggsave('../export/Figure4_inset.pdf',width=6,height=4,useDingbats=FALSE,scale=1.2)
