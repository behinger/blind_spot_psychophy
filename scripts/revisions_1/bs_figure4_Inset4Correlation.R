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


d4aAgg = ddply(d4a,.(subject),summarise,prob = (mean(prob[stimLLoc==0&stimRLoc==1])-mean(prob[stimLLoc==1&stimRLoc==0]))/2*100,.drop=T)
d4bAgg = ddply(d4b,.(subject),summarise,prob = (mean(prob[stimLLoc==0&stimRLoc==1])-mean(prob[stimLLoc==1&stimRLoc==0]))/2*100,.drop=T)


v = cbind(d4a$prob,d4b$prob)%>%prcomp%$%rotation
x1x2cor = v[2,1]/v[1,1]




qplot(d4aAgg$prob,d4bAgg$prob)+
  geom_hline(yintercept = 0,color='lightgray')+geom_vline(xintercept = 0,color='lightgray')+
  geom_abline(slope=-1,intercept = 0)+ 
  geom_abline(intercept = 0,slope = x1x2cor,color='red')+
  tBE(20)+coord_fixed(xlim=c(-50,25),ylim=c(-25,50))+
  annotate('text',x= 25,y=40,
           label=paste0('corr:',round(cor(d4aAgg$prob,d4bAgg$prob),2)))+
  xlab('Inset 4a') + ylab('Inset 4b')


cor(d4aAgg$prob,d4bAgg$prob)

ggsave('../export/Figure4_inset.pdf',width=6,height=4,useDingbats=FALSE,scale=1.2)
