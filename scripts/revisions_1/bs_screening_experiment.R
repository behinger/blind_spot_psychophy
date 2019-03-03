
metaData = gdata::read.xls('data/Subject Overview_v4.xlsx',sheet=1)

allDataFrame = data.frame()
for(k in 1:length(metaData$VP)){
  number = metaData$VP[k]
  experiment =tolower(metaData$Experiment[k])
  subject = ifelse(experiment=='eeg',tolower(strtrim(as.character(number),1)),tolower(as.character(number)))
  subject = ifelse((experiment=='psyphy')&(as.numeric(subject)<10),paste0(0,subject),subject)
  subject = ifelse((experiment=='horizontal'),substr(subject, nchar(subject)-2+1, nchar(subject)),subject)
  subject = ifelse((experiment=='inset'),as.character(number),subject)
  
  if((experiment=='control')&((subject=='h8')|(subject=='h11')|(subject=='h9')|(subject=='i2')|(subject=='i7')|(subject=='i8'))){next} # we have the written data, but not the file
  
  if((experiment=='eeg')&((subject=='i'))){next}  # ET problems
  if((experiment=='')&((subject=='q2?'))){next}  # ET problems
  if((experiment=='eeg')&((subject=='r'))){next}  # Technical problems
  if((experiment=='eeg')&((subject=='s'))){next}  # missing
  if((experiment=='eeg')&((subject=='w'))){next}  # technical problems
  if((experiment=='horizontal')&((subject=='15'))){next}  # technical problems
  if((experiment=='inset')&((subject=='1.2.3'))){next}  # ???
  if((experiment=='inset')&((subject=='18'))){next}  # technical problems
  
  
    fPath = paste0("/net/store/nbp/projects/EEG/blind_spot/data/single/",experiment,'/')
    files = dir(fPath,pattern=paste0('bs_',subject,'.*'))
    
    if(length(files)==0){
      stop('not found')
    }
    print(paste("Processing",experiment," Subject", number," files", length(files)))
    singleSubData = NULL
    for(f in files){
      data = R.matlab::readMat(paste0(fPath,f))$bs.result
      names = attr(data,"dimnames")[[1]]                              # Speichert Variablennamen in einem Vektor ab
      data = data[,,1]
      
      lengthDiff = length(data$rt) - length(data$trialTime) 
      if(lengthDiff>0){
        # there is a bug in the experimental code, if the trial is stopped early, no trialtime is recorded. thus if the last trial(s) are stopped, the trialtime is shorter than the other files
        data$trialTime[(length(data$rt)-lengthDiff+1):length(data$rt)] = NA
        data$trialTime = t(data$trialTime) #somehow the above transposes it -.-
      }
      
      if((experiment=='control') & (subject=='h4')){
        data$trial = data$trial[1:(length(data$trial)-1)]
      }
      
      data = data.frame(sapply(data,t))
      if(!is.null(singleSubData )){
        # in EEG we have multiple session, we concatenate them
        data$trial = data$trial + max(singleSubData$trial)
      }
      if('error'%in% colnames(data)){
        data = data[,!colnames(data)%in%c('error')]
      }
      singleSubData = rbind(data)
      
    }
    allDataFrame = rbind(allDataFrame, with(subset(metaData,tolower(metaData$Experiment) == experiment & metaData$VP == number), 
                                            cbind(singleSubData, subject=VP,age=Alter,sex=Geschlecht,handedness=Handedness,dominantEye=DominantEye, experiment=Experiment,
                                                  remove=remove.,removeReason=Remove.Reas)))
    
}

# Convert pixel to degree
px2deg = function(x,d) { 
  2*atan2(x*0.276/2,d)*180/pi}  # Rechnet Pixel in Grad um (d beschreibt den Bildschirmabstand in mm)

allDataFrame[allDataFrame$experiment=='Horizontal','bsSL'] = px2deg(allDataFrame[allDataFrame$experiment=='Horizontal','bsSL'],500)
allDataFrame[allDataFrame$experiment=='Horizontal','bsSR'] = px2deg(allDataFrame[allDataFrame$experiment=='Horizontal','bsSR'],500)
allDataFrame[allDataFrame$experiment=='Horizontal','bsXL'] = px2deg(allDataFrame[allDataFrame$experiment=='Horizontal','bsXL'],500)
allDataFrame[allDataFrame$experiment=='Horizontal','bsXR'] = px2deg(allDataFrame[allDataFrame$experiment=='Horizontal','bsXR'],500)
allDataFrame[allDataFrame$experiment=='Horizontal','bsYL'] = px2deg(allDataFrame[allDataFrame$experiment=='Horizontal','bsYL'],500)
allDataFrame[allDataFrame$experiment=='Horizontal','bsYR'] = px2deg(allDataFrame[allDataFrame$experiment=='Horizontal','bsYR'],500)

allDataFrame[allDataFrame$experiment!='Horizontal','bsSL'] = px2deg(allDataFrame[allDataFrame$experiment!='Horizontal','bsSL'],600)
allDataFrame[allDataFrame$experiment!='Horizontal','bsSR'] = px2deg(allDataFrame[allDataFrame$experiment!='Horizontal','bsSR'],600)
allDataFrame[allDataFrame$experiment!='Horizontal','bsXL'] = px2deg(allDataFrame[allDataFrame$experiment!='Horizontal','bsXL'],600)
allDataFrame[allDataFrame$experiment!='Horizontal','bsXR'] = px2deg(allDataFrame[allDataFrame$experiment!='Horizontal','bsXR'],600)
allDataFrame[allDataFrame$experiment!='Horizontal','bsYL'] = px2deg(allDataFrame[allDataFrame$experiment!='Horizontal','bsYL'],600)
allDataFrame[allDataFrame$experiment!='Horizontal','bsYR'] = px2deg(allDataFrame[allDataFrame$experiment!='Horizontal','bsYR'],600)


allDataFrame = allDataFrame %>% 
  mutate(subject = interaction(subject,experiment))


allDataFrame = allDataFrame%>% 
  mutate(inset = oStim != iStim) %>%
   mutate(correct = answer == 2 &  inset & locCond %in% c(2,3) |
                    answer == 1 &  inset & locCond %in% c(1,4) | 
                    answer == 1 & !inset & locCond %in% c(1,2,3,4))
                            #((answer==2)==inset&(locCond%in%c(2,3)))| (((answer==1)==inset)&(locCond%in%c(1,4))))

y = singleSubData%>%subset(block==2)%>% mutate(inset = oStim != iStim) %>%
      mutate(correct = answer == 2 &  inset & locCond %in% c(2,3)     |
                       answer == 1 &  inset & locCond %in% c(1,4) | 
                       answer == 1 & !inset & locCond %in% c(1,2,3,4))
x = allDataFrame%>%subset(subject=='C2.Control')%>%
  subset(block==2)
cbind(x[,c('trial','correct')],y[,c('trial','correct')])

a = allDataFrame%>%
  subset(block==2&success==1)%>%
  group_by(subject,removeReason,inset,locCond)%>%
  summarise(correct = mean(correct,na.rm=T))%>%
  group_by(removeReason,subject)%>%
  summarise(correct=sum(correct,na.rm = T))
# Reconstruct removal criterion
ggplot(allDataFrame%>%
         subset(block==2&success==1&experiment=='PsyPhy')%>%
         group_by(locCond,removeReason,subject,inset)%>%
         summarise(correct = mean(correct,na.rm=T))%>%
         group_by(removeReason,subject)%>%
         summarise(correct=sum(correct,na.rm = T)),aes(x=subject,y=correct,color=removeReason))+
  geom_point()+
  facet_grid(removeReason~.)+geom_hline(yintercept = 0.94/6*48)

  



dat.single.rem = allDataFrame[allDataFrame$block == 2&as.character(allDataFrame$removeReason)=='Single',]
dat.single.kep = allDataFrame[allDataFrame$block == 2&allDataFrame$remove==F,]

binom_errorbars = function(x){
  ci = binom::binom.confint(sum(x,na.rm=TRUE),sum(!is.na(x)),conf.level = 0.95,method='logit');
  return(data.frame(y = ci$mean,ymin = ci$lower,ymax=ci$upper))
}




# How many correct?
ddply(subset(dat.single.kep,dat.single.kep$success==1 & dat.single.kep$block == 2),.(subject),summarise,mean(correct))
ddply(subset(dat.single.rem,dat.single.rem$success==1 & dat.single.rem$block == 2),.(subject),summarise,mean(correct))

# Plot all single subject answers
ggplot(dat.single.kep,aes(x=locCond,y=correct,color=inset))+geom_point(position=position_dodge(0.5),alpha=0.3)+facet_wrap(~subject,ncol = 7)
ggplot(dat.single.rem,aes(x=locCond,y=answer,color=inset))+geom_point(position=position_dodge(0.5),alpha=0.3)+facet_wrap(~interaction(subject,experiment),ncol = 7)+stat_summary(position=position_dodge(width=0.3))#fun.data=binom_errorbars) 

# Left
failReasons = ddply(dat.single.rem,.(subject),summarise,
      leftBSFail  = sum(answer[inset==T&locCond==1]==2,na.rm = T),
      rightBSFail = sum(answer[inset==T&locCond==4]==2,na.rm = T),
      outsideBSInsetFail  = sum(answer[inset==T&locCond%in%c(2,3)]==1,na.rm = T),
      noInsetFail = sum(answer[inset==F]==2,na.rm = T)
      )
failReasonsKep = ddply(dat.single.kep,.(subject),summarise,
                    leftBSFail  = sum(answer[inset==T&locCond==1]==2,na.rm = T),
                    rightBSFail = sum(answer[inset==T&locCond==4]==2,na.rm = T),
                    outsideBSInsetFail  = sum(answer[inset==T&locCond%in%c(2,3)]==1,na.rm = T),
                    noInsetFail = sum(answer[inset==F]==2,na.rm = T)
)

ddply(reshape2::melt(failReasons),.(variable),summarise,sum = sum(value!=0),percentage = round(mean(value!=0)*100))
ddply(reshape2::melt(failReasonsKep),.(variable),summarise,sum = sum(value!=0),percentage = round(mean(value!=0)*100))
# What is the population difference between keep and remove?
ggplot(allDataFrame,aes(x=locCond,y=correct,color=inset)) + stat_summary(position = position_dodge(width=0.3),fun.data=binom_errorbars) + facet_grid(.~remove)+ylim(c(0,1))

# Where do keep/remove people lay on bsXR?
#ggplot(subset(allDataFrame,allDataFrame$locCond==4),aes(x=bsXR,y=correct,group=remove)) + stat_smooth() + facet_grid(.~inset)+geom_point(alpha=0.1,position=position_jitter(width=0.1))
#ggplot(subset(allDataFrame,allDataFrame$locCond==4),aes(x=bsYR,color=remove)) + geom_histogram() + facet_grid(.~inset)
#ggplot(subset(allDataFrame,allDataFrame$locCond==4),aes(x=bsYR,color=remove)) + geom_histogram() + facet_grid(.~inset)


# Compare all densities

cowplot::plot_grid(
ggplot(allDataFrame,aes(x=bsSL,group=remove)) + geom_density(),
ggplot(allDataFrame,aes(x=bsYL,group=remove)) + geom_density(),
ggplot(allDataFrame,aes(x=bsXL,group=remove)) + geom_density(),

ggplot(allDataFrame,aes(x=bsSR,group=remove)) + geom_density(),
ggplot(allDataFrame,aes(x=bsYR,group=remove)) + geom_density(),
ggplot(allDataFrame,aes(x=bsXR,group=remove)) + geom_density())




# Histogram to show the bimodality
#ggplot(allDataFrame,aes(x=bsXR,color=remove)) + geom_histogram()

## Answer LM
#dat = subset(allDataFrame,allDataFrame$success==1)
#dat = subset(dat,(dat$dominantEye %in%c('R', 'L')) &(dat$sex %in%c('F', 'M')))
#dat$correct = as.numeric(dat$correct)
#dat$locCond = factor(dat$locCond,levels=c(2,3,1,4))

#s = function(x){
#  scale(x,center=T,scale=T)}
#forcats::fct_reorder(dat$locCond,
#mres = glmer(correct~(dominantEye)*locCond+(1|subject),dat,subset=dat$remove==1&dat$block==2,family = 'binomial')
#summary(mres)


