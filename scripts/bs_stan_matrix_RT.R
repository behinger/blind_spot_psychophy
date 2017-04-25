#!/usr/bin/Rscript

#setwd('/net/store/nbp/projects/EEG/blind_spot/bs_svn/analysis/r-analysis')
setwd('/net/store/nbp/users/behinger/projects/blindspot/git/scripts')
#GRIDMODE = TRUE
GRIDMODE = FALSE

source('grid_tools.R')
cfg_grid = list(gridOutputPath = file.path('../tmp/gridoutput'),
                requirements = 'mem=5G,h=!ramsauer.ikw.uni-osnabrueck.de',
                add_to_filename = '',
                script_name = 'bs_stan_matrix_RT.R')

if (1==0){  run_on_grid(cfg_grid)  }


source('revisions_1/bs_init.R')

#load('AllDataFrame_alsoBad.RDATA') #allDataFrame <- createAllDataFrame()


# 1 -> EEG only
# 2 -> Control only
# 3 -> Horizontal only
# 4 -> All Experiments (except 4a)

# 5 -> EEG only, no outlier
# 6 -> All Experiments, no outlier (except 4a)

# 7 -> Inset4a
# 8 -> Inset4b
ciAll = NULL
for (exp in c(1,2,3,4,5,6,7,8)){
  
  
  dat= subset(allDat,allDat$remove==F&allDat$success==1);
  
  dat$temporalSelected = ((dat$stimLLoc == 1) &(dat$answer==0)) | ((dat$stimRLoc ==1)& (dat$answer==1))
  
  if(exp %in% c(5,6)){
    outlier = dat[(dat$subject %in% c('EEG.U','EEG.N','EEG.R17')),]
    outlier  = subset(outlier,outlier$stimPVisible != 2)
    outlier$subjectIndex = as.numeric(factor(outlier$subject))
    outlier$controlTrial = as.factor(outlier$controlTrial)
    
    dat = dat[!(dat$subject %in% c('EEG.U','EEG.N','EEG.R17')),]
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
  
  
  formula = ~1+temporalSelected*controlTrial # same formula for all experiments except EEG(1/5) + Inset4a (7)
  if(exp==1||exp==5){ 
    dat.exp = subset(dat.uni,dat.uni$experiment=='EEG')
    dat.exp$subject = factor(dat.exp$subject)
    formula = ~1+temporalSelected
  }else if(exp==2||exp==8){
    dat.exp = subset(dat.uni,dat.uni$experiment=='Control')
    dat.exp$subject = factor(dat.exp$subject)
    dat.exp$controlTrial = factor(dat.exp$controlTrial)
    
  }else if(exp==3){
    dat.exp = subset(dat.uni,dat.uni$experiment=='Horizontal')
    dat.exp$subject = factor(dat.exp$subject)
    dat.exp$controlTrial = factor(dat.exp$controlTrial)
  }else if(exp==4||exp==6){
    dat.exp = dat.uni # combined
  }else if(exp==7){
    dat.exp = subset(dat.uni,dat.uni$experiment=='Inset4a')
    dat.exp$subject = factor(dat.exp$subject)
    formula = ~1+temporalSelected
    
  }else if(exp==8){
    dat.exp = subset(dat.uni,dat.uni$experiment=='Inset4b')
    dat.exp$subject = factor(dat.exp$subject)
    dat.exp$controlTrial = factor(dat.exp$controlTrial)
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
    
    fit <- stan(file="matrix_model_RT.stan",
                data=stanDat,
                iter=2000,
                warmup=1000,
                refresh=10,
                chains=1)
    #control=list(max_treedepth=10,adapt_delta=0.8))
    
    
    filename = paste0('grid_',cfg_grid$add_to_filename,format(Sys.time(), '%Y-%m-%d_%H-%M-%S'),'_',cfg_grid$script_name,'Data')
    save(file=filename,fit)
    #stop('grid execution ended sucessfully (even though this is an error')
  }else{
    fit = load_grid_STAN(cfg_grid)
    
    label_dataframe = data.frame(Parameter=sprintf('beta[%i]',1:length(betaNames)),Label=betaNames)
    S = ggmcmc::ggs(fit,par_labels = label_dataframe,family = 'beta')
    
    ciExp = NULL
    if(exp %in%c(1,5,7)){ # only BS
      ciExp = data.frame(            loc = 1 , t(quantile(S$value[S$Parameter == "temporalSelectedTRUE"],c(0.025,0.50,0.975))))
    }else if(exp %in% c(2,8)){# control =3
      ciExp = data.frame(            loc = 1 , t(quantile(S$value[S$Parameter == "temporalSelectedTRUE"],c(0.025,0.50,0.975))))
      ciExp = rbind(ciExp,data.frame(loc = 3 , t(quantile(S$value[S$Parameter == "temporalSelectedTRUE"]+S$value[S$Parameter == "temporalSelectedTRUE:controlTrial3"],c(0.025,0.50,0.975)))))
    }else if(exp==3){# Horizontal
      ciExp = data.frame(            loc = 0 , t(quantile(S$value[S$Parameter == "temporalSelectedTRUE"],c(0.025,0.50,0.975))))
      ciExp = rbind(ciExp,data.frame(loc = 1 , t(quantile(S$value[S$Parameter == "temporalSelectedTRUE"]+S$value[S$Parameter == "temporalSelectedTRUE:controlTrial1"],c(0.025,0.50,0.975)))))
      ciExp = rbind(ciExp,data.frame(loc = 2 , t(quantile(S$value[S$Parameter == "temporalSelectedTRUE"]+S$value[S$Parameter == "temporalSelectedTRUE:controlTrial2"],c(0.025,0.50,0.975)))))
      
    }else if(exp%in%c(4,6)){# all experiments
      ciExp = data.frame(            loc = 0 , t(quantile(S$value[S$Parameter == "temporalSelectedTRUE"],c(0.025,0.50,0.975))))
      ciExp = rbind(ciExp,data.frame(loc = 1 , t(quantile(S$value[S$Parameter == "temporalSelectedTRUE"]+S$value[S$Parameter == "temporalSelectedTRUE:controlTrial1"],c(0.025,0.50,0.975)))))
      ciExp = rbind(ciExp,data.frame(loc = 2 , t(quantile(S$value[S$Parameter == "temporalSelectedTRUE"]+S$value[S$Parameter == "temporalSelectedTRUE:controlTrial2"],c(0.025,0.50,0.975)))))
      ciExp = rbind(ciExp,data.frame(loc = 3 , t(quantile(S$value[S$Parameter == "temporalSelectedTRUE"]+S$value[S$Parameter == "temporalSelectedTRUE:controlTrial3"],c(0.025,0.50,0.975)))))
      
    }
    ciExp = cbind(ciExp,exp = exp,outlier= (exp%in%c(1,4)))
    ciAll = rbind(ciAll,ciExp)

  }
  
}
colnames(ciAll) =  c("controlTrial","low","median","high","experiment","outlier")
if(GRIDMODE){
stop('grid execution ended sucessfully (even though this is an error')
}
 






datContrast = rbind(
  data.frame(outlier=1,exp=1,loc=1,ddply(outlier,.(subject),summarise,diff = median(rt[experiment=='EEG'&controlTrial==1&temporalSelected==1])-median(rt[experiment=='EEG'&controlTrial==1&temporalSelected==0]))),
  data.frame(outlier=0,exp=1,loc=1,ddply(dat.uni,.(subject),summarise,diff = median(rt[experiment=='EEG'&controlTrial==1&temporalSelected==1])-median(rt[experiment=='EEG'&controlTrial==1&temporalSelected==0]))),
  data.frame(outlier=0,exp=2,loc=1,ddply(dat.uni,.(subject),summarise,diff = median(rt[experiment=='Control'&controlTrial==1&temporalSelected==1])-median(rt[experiment=='Control'&controlTrial==1&temporalSelected==0]))),
  data.frame(outlier=0,exp=3,loc=1,ddply(dat.uni,.(subject),summarise,diff = median(rt[experiment=='Horizontal'&controlTrial==1&temporalSelected==1])-median(rt[experiment=='Horizontal'&controlTrial==1&temporalSelected==0]))),
  data.frame(outlier=0,exp=2,loc=3,ddply(dat.uni,.(subject),summarise,diff = median(rt[experiment=='Control'&controlTrial==3&temporalSelected==1])-median(rt[experiment=='Control'&controlTrial==3&temporalSelected==0]))),
  data.frame(outlier=0,exp=3,loc=0,ddply(dat.uni,.(subject),summarise,diff = median(rt[experiment=='Horizontal'&controlTrial==0&temporalSelected==1])-median(rt[experiment=='Horizontal'&controlTrial==0&temporalSelected==0]))),
  data.frame(outlier=0,exp=3,loc=2,ddply(dat.uni,.(subject),summarise,diff = median(rt[experiment=='Horizontal'&controlTrial==2&temporalSelected==1])-median(rt[experiment=='Horizontal'&controlTrial==2&temporalSelected==0]))),
  data.frame(outlier=0,exp=5,loc=1,ddply(dat.uni,.(subject),summarise,diff = median(rt[experiment=='Inset4a'&controlTrial==1&temporalSelected==1])-median(rt[experiment=='Inset4a'&controlTrial==1&temporalSelected==0]))),
  data.frame(outlier=0,exp=6,loc=1,ddply(dat.uni,.(subject),summarise,diff = median(rt[experiment=='Inset4b'&controlTrial==1&temporalSelected==1])-median(rt[experiment=='Inset4b'&controlTrial==1&temporalSelected==0]))),
  data.frame(outlier=0,exp=6,loc=3,ddply(dat.uni,.(subject),summarise,diff = median(rt[experiment=='Inset4b'&controlTrial==3&temporalSelected==1])-median(rt[experiment=='Inset4b'&controlTrial==3&temporalSelected==0]))),
  data.frame(outlier=0,exp=4,loc=1,subject=10,diff=NA)
  )
  
# Plot of median reaction time differences between temporal selected and nasal selected

ggplot(subset(ciAll,!(ciAll$experiment %in% c(1,4))),
       aes(x=controlTrial,y=-median,
           ymax=-high,ymin=-low,
           group=factor(experiment)))+
  geom_hline(yintercept=0)+
  geom_jitter(inherit.aes = F,data=datContrast[!is.nan(datContrast$diff),],
              aes(x=loc,
                  y=-diff,group=exp,
                  color=factor(exp)),
              alpha=0.5,position=position_jitterdodge(jitter.width = 0.1,dodge.width=0.6))+
  geom_pointrange(aes(shape=factor(experiment)),position=position_dodge(width=0.6),size=0.8)+
  tBE()+
  annotate("text",x=1.1,y=0.4,label='outlier',color='red')+coord_flip()



ggsave(file='../export/Figure6_rt.pdf',useDingbats=F,width=6,height=2,scale=1.5)

ggplot(subset(ciAll,!(ciAll$experiment %in% c(1,4))),
       aes(x=controlTrial,y=-median,
           ymax=-high,ymin=-low,
           group=factor(experiment)))+
  geom_hline(yintercept=0)+
  geom_jitter(inherit.aes = F,data=datContrast[!is.nan(datContrast$diff),],
              aes(x=loc,
                  y=-diff,group=exp,
                  color=factor(exp)),
              alpha=0.5,position=position_jitterdodge(jitter.width = 0.1,dodge.width=0.6))+
  stat_summary(inherit.aes = F,data=datContrast[!is.nan(datContrast$diff),],aes(x=loc,
                                                                                y=-diff,group=exp,
                                                                                color=factor(exp)),)+
              
  geom_pointrange(aes(group=factor(experiment)),position=position_dodge(width=0.6),size=0.8)+
  tBE()+
  annotate("text",x=1.1,y=0.4,label='outlier',color='red')+coord_flip()



ggsave(file='../export/Figure6_rt_simple.pdf',useDingbats=F,width=6,height=2,scale=1.5)
