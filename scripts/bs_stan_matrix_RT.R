#!/usr/bin/Rscript

setwd('/net/store/nbp/projects/EEG/blind_spot/bs_svn/analysis/r-analysis')

GRIDMODE = TRUE
GRIDMODE = FALSE

source('grid_tools.R')
cfg_grid = list(gridOutputPath = file.path('./gridoutput'),
                requirements = 'mem=5G,h=!ramsauer.ikw.uni-osnabrueck.de',
                add_to_filename = '',
                script_name = 'bs_stan_matrix_RT.R')

if (1==0){  run_on_grid(cfg_grid)  }



library("ggplot2")
library(plyr)
library(rstan)
source('plots_paper/bs_ggplot_theme.R')

load('AllDataFrame_alsoBad.RDATA') #allDataFrame <- createAllDataFrame()


# 2. Make design matrix.

ciAll = NULL
# 1 -> EEG only
# 2 -> Control only
# 3 -> Horizontal only
# 4 -> All Experiments

# 5 -> EEG only, no outlier
# 6 -> All Experiments, no outlier
for (exp in c(1,2,3,4,5,6)){
  
  
  dat= subset(allDat,allDat$remove==F&allDat$success==1);
  
  dat$temporalSelected = ((dat$stimLLoc == 1) &(dat$answer==0)) | ((dat$stimRLoc ==1)& (dat$answer==1))
  
  if(exp %in% c(5,6)){
    outlier = dat[(dat$subject %in% c('U','N','R17')),]
    outlier  = subset(outlier,outlier$stimPVisible != 2)
    outlier$subjectIndex = as.numeric(factor(outlier$subject))
    outlier$controlTrial = as.factor(outlier$controlTrial)
    
    dat2 = dat[!(dat$subject %in% c('U','N','R17')),]
  }
  
  
  
  dat.orth = subset(dat,dat$stimPVisible == 2)
  dat.uni  = subset(dat,dat$stimPVisible != 2)
  
  dat.uni$subjectIndex = as.numeric(factor(dat.uni$subject))
  dat.uni$controlTrial = as.factor(dat.uni$controlTrial)
  
  
  
  
  if(exp %in% c(5,6)){
    cfg_grid$add_to_filename = paste0('remove_outlier_exp',exp)
  } else{
    cfg_grid$add_to_filename = paste0('exp',exp)
    } 
  
  
  formula = ~1+temporalSelected*controlTrial # same formula for all experiments except EEG
  if(exp==1||exp==5){ 
    dat.exp = subset(dat.uni,dat.uni$experiment=='EEG')
    dat.exp$subject = factor(dat.exp$subject)
    formula = ~1+temporalSelected
  }else if(exp==2){
    dat.exp = subset(dat.uni,dat.uni$experiment=='Control')
    dat.exp$subject = factor(dat.exp$subject)
    dat.exp$controlTrial = factor(dat.exp$controlTrial)
    
  }else if(exp==3){
    dat.exp = subset(dat.uni,dat.uni$experiment=='Horizontal')
    dat.exp$subject = factor(dat.exp$subject)
    dat.exp$controlTrial = factor(dat.exp$controlTrial)
  }
  else if(exp==4||exp==6){
    dat.exp = dat.uni # combined
  }
  
  
  
  #formula = ~1+stimLLoc*controlTrial*stimRLoc +oneBack
  
  X <- unname(model.matrix(formula, dat.exp))
  
  
  betaNames = colnames( model.matrix(formula, dat.exp))
  
  
  if (GRIDMODE){
  # 4. Make Stan data.
  stanDat <- within(list(),
                    {
                      N<-nrow(X)
                      P <- n_u <- n_w <- ncol(X)
                      X <- Z_u <- Z_w <- X
                      J <- length(unique(dat.exp$subject))
                      rt <- (dat.exp$rt)
                      subj <- as.numeric(factor(dat.exp$subject,labels=1:length(unique(dat.exp$subject))))
                    }
  )
  
  

    
    #m2_model_hierarchical <- rstan::stan_model(file = 'm2_hierarchical.stan')
    
    rstan::rstan_options(auto_write = TRUE)
    options(mc.cores = parallel::detectCores())
    
    fit <- stan(file="stan_2015/matrix_model_RT.stan",
                data=stanDat,
                iter=2000,
                warmup=1000,
                refresh=1,
                chains=1)
    #control=list(max_treedepth=10,adapt_delta=0.8))
    
    
    filename = paste0('grid_',cfg_grid$add_to_filename,format(Sys.time(), '%Y-%m-%d_%H-%M-%S'),'_',cfg_grid$script_name,'Data')
    save(file=filename,fit)
    #stop('grid execution ended sucessfully (even though this is an error')
  }else{
    fit = load_grid_STAN(cfg_grid)
    
    label_dataframe = data.frame(Parameter=sprintf('beta.%i.',1:length(betaNames)),Label=betaNames)
    S = ggmcmc::ggs(fit,par_labels = label_dataframe,family = 'beta')
    
    ciS = ggmcmc::ci(S)
    
    ciAll = rbind(ciAll,cbind(ciS,experiment=exp))
    
    
    
    
  }
  
}
if(GRIDMODE){
stop('grid execution ended sucessfully (even though this is an error')
}
 

#BS Exp 1
calc_contrast = function(contrastIdx){
  contrast = rep(0,dim(ciAll)[1])%>%replace(contrastIdx,rep(1,length(contrastIdx)))
  data.frame(
    low =contrast%*% ciAll$low,
    median =contrast %*% ciAll$median,
    high =contrast %*% ciAll$high)
    
    
}

ciContrast = rbind(
cbind(outlier=0,exp=1,loc=1,calc_contrast(2)),
cbind(outlier=0,exp=2,loc=1,calc_contrast(5)),
cbind(outlier=0,exp=3,loc=1,calc_contrast(c(10,11))),
cbind(outlier=0,exp=2,loc=3,calc_contrast(c(5,6))),
cbind(outlier=0,exp=3,loc=0,calc_contrast(10)),
cbind(outlier=0,exp=3,loc=2,calc_contrast(c(10,12))),

cbind(outlier=0,exp=4,loc=0,calc_contrast(17)), # 4, all with outlier
cbind(outlier=0,exp=4,loc=1,calc_contrast(c(17,18))),
cbind(outlier=0,exp=4,loc=2,calc_contrast(c(17,19))),
cbind(outlier=0,exp=4,loc=3,calc_contrast(c(17,20))),

cbind(outlier=1,exp=1,loc=1,calc_contrast(22)), #5, exp 1 without outler

cbind(outlier=1,exp=4,loc=0,calc_contrast(27)), # 4, all with outlier
cbind(outlier=1,exp=4,loc=1,calc_contrast(c(27,28))),
cbind(outlier=1,exp=4,loc=2,calc_contrast(c(27,29))),
cbind(outlier=1,exp=4,loc=3,calc_contrast(c(27,30))))



datContrast = rbind(
  data.frame(outlier=1,exp=1,loc=1,ddply(outlier,.(subject),summarise,diff = mean(rt[experiment=='EEG'&controlTrial==1&temporalSelected==1])-mean(rt[experiment=='EEG'&controlTrial==1&temporalSelected==0]))),
  data.frame(outlier=0,exp=1,loc=1,ddply(dat.uni,.(subject),summarise,diff = mean(rt[experiment=='EEG'&controlTrial==1&temporalSelected==1])-mean(rt[experiment=='EEG'&controlTrial==1&temporalSelected==0]))),
  data.frame(outlier=0,exp=2,loc=1,ddply(dat.uni,.(subject),summarise,diff = mean(rt[experiment=='Control'&controlTrial==1&temporalSelected==1])-mean(rt[experiment=='Control'&controlTrial==1&temporalSelected==0]))),
  data.frame(outlier=0,exp=3,loc=1,ddply(dat.uni,.(subject),summarise,diff = mean(rt[experiment=='Horizontal'&controlTrial==1&temporalSelected==1])-mean(rt[experiment=='Horizontal'&controlTrial==1&temporalSelected==0]))),
  data.frame(outlier=0,exp=2,loc=3,ddply(dat.uni,.(subject),summarise,diff = mean(rt[experiment=='Control'&controlTrial==3&temporalSelected==1])-mean(rt[experiment=='Control'&controlTrial==3&temporalSelected==0]))),
  data.frame(outlier=0,exp=3,loc=0,ddply(dat.uni,.(subject),summarise,diff = mean(rt[experiment=='Horizontal'&controlTrial==0&temporalSelected==1])-mean(rt[experiment=='Horizontal'&controlTrial==0&temporalSelected==0]))),
  data.frame(outlier=0,exp=3,loc=2,ddply(dat.uni,.(subject),summarise,diff = mean(rt[experiment=='Horizontal'&controlTrial==2&temporalSelected==1])-mean(rt[experiment=='Horizontal'&controlTrial==2&temporalSelected==0]))),
  data.frame(outlier=0,exp=4,loc=1,subject=10,diff=NA)
  )
  
p2 = 
  ggplot(ciContrast,aes(x=loc,y=-median,
                      ymax=-high,ymin=-low,
                      color=factor(exp),
                      #shape=factor(exp),
                      fill = factor(exp)),
                      group=factor(exp))+
  geom_hline(yintercept=0)+
  geom_jitter(inherit.aes = F,data=datContrast[!is.nan(datContrast$diff),],
              aes(x=loc,
                  y=-diff,group=exp,fill=factor(exp),
                  color=factor(outlier)),
             alpha=0.5,position=position_jitterdodge(jitter.width = 0.1,dodge.width=0.6))+
  geom_pointrange(position=position_dodge(width=0.5),size=0.8)+
  tBE()+
scale_color_manual(breaks=datContrast[!is.nan(datContrast$diff),]$outlier,
                                                        values=c('gray','red','darkgreen'),guide=F)+
  annotate("text",x='1.1',y=0.4,label='outlier',color='red')
  

x = fd_helper_ddply_MeanCIoverSubjects(dat.uni,c("experiment","controlTrial","temporalSelected"),'rt')
xOut = fd_helper_ddply_MeanCIoverSubjects(rbind(dat.uni,outlier),c("experiment","controlTrial","temporalSelected"),'rt')
xOut = subset(xOut,xOut$experiment=='EEG')
xComb = rbind(cbind(x,outlier=0),
              cbind(xOut,outlier=1))
xComb$outlier = factor(xComb$outlier)
p1 = 
  ggplot(xComb,aes(x=interaction(experiment,controlTrial),
             #color=factor(controlTrial,labels = c("outward","bs","inward","above")),
             group=interaction(outlier,temporalSelected),alpha=temporalSelected,
             y=mean,ymax=conf.high,ymin=conf.low))+
  geom_jitter(alpha=0.5,position=position_dodge(width=0.8),
              data=ddply(dat,.(experiment,subject,controlTrial,temporalSelected),summarise,rt=mean(rt)),
              aes(group=temporalSelected,ymin=rt,ymax=rt,y=rt,color='gray'))+
  geom_jitter(alpha=0.5,position=position_dodge(width=0.8),
              data=ddply(outlier,.(experiment,subject,controlTrial,temporalSelected),summarise,rt=mean(rt)),
              aes(group=temporalSelected,ymin=rt,ymax=rt,y=rt,color='red'))+
  geom_pointrange(position=position_dodge(width=0.3))+
  theme(legend.title=element_blank())+scale_alpha_discrete(guide=F,range=c(0.3,1))+tBE()+
 
  annotate("text",x='EEG.1',y=1.6,label='outlier',color='red')+
  scale_color_manual(values=c('gray','red','darkgreen'),guide=F)+
  coord_cartesian(ylim=c(0,2))
cowplot::plot_grid(p1,p2,labels=c('A','B'))

ggsave(file='rt_plot.pdf',useDingbats=F,width=6,height=2,scale=1.5)