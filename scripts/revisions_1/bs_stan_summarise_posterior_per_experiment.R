 bs_stan_summarise_posterior_per_experiment = function(factorialFit.exp1,factorialFit.exp2,factorialFit.exp3,factorialFit.exp4a,factorialFit.exp4b,dat.exp1,dat.exp2,dat.exp3,dat.exp4a,dat.exp4b){
  source('./revisions_1/bs_load_stan.R')
   get_label = function(exchangeIdx,exchangeVal,formula,dat){
     betaNames = colnames( model.matrix(formula, dat))
     betaNames[exchangeIdx] = exchangeVal
     beta_fix_Names = c('handedness','dominantEye')
     label_dataframe = data.frame(Parameter=sprintf('beta[%i]',1:length(betaNames)),Label=betaNames)
     label_dataframe = rbind(label_dataframe,data.frame(Parameter=sprintf('beta_fix[%i]',1:length(beta_fix_Names)),Label=beta_fix_Names))
   }
   
   
   
   
S.exp1 = ggs(factorialFit.exp1,par_labels = get_label(exchangeIdx=NULL,
                                                      exchangeVal= NULL,
                                                      formula = ~1+stimLLoc + stimRLoc +oneBack,
                                                      dat=dat.exp1),              family = 'beta')
show(unique(S.exp1$Parameter))
S.exp2 = ggs(factorialFit.exp2,par_labels = get_label(exchangeIdx=c(7),
                                                      exchangeVal= c("stimRLoc:controlTrial3"),
                                                      formula =  ~1+stimLLoc*controlTrial + controlTrial*stimRLoc +oneBack,
                                                      dat=dat.exp2),             family = 'beta')
show(unique(S.exp2$Parameter))

S.exp3 = ggs(factorialFit.exp3,par_labels = get_label(exchangeIdx=c(9,10),
                                                      exchangeVal= c("stimRLoc:controlTrial1","stimRLoc:controlTrial2"),
                                                      formula = ~1+stimLLoc*controlTrial + controlTrial*stimRLoc +oneBack,
                                                      dat=dat.exp3),              family = 'beta')
show(unique(S.exp3$Parameter))

S.exp4a = ggs(factorialFit.exp4a,par_labels = get_label(exchangeIdx=NULL,
                                                        exchangeVal= NULL,
                                                        formula = ~1+stimLLoc + stimRLoc +oneBack,
                                                        dat=dat.exp4a),              family = 'beta')
S.exp4a$Parameter = revalue(S.exp4a$Parameter,c('handedness'='dominantEye'))

warning('removing handedness in 4a, only right handed so far')
show(unique(S.exp4a$Parameter))

S.exp4b = ggs(factorialFit.exp4b,par_labels = get_label(exchangeIdx=c(7),
                                                        exchangeVal= c("stimRLoc:controlTrial3"),
                                                        formula = ~1+stimLLoc*controlTrial + controlTrial*stimRLoc +oneBack,
                                                        dat=dat.exp4b),              family = 'beta')
S.exp4b$Parameter = revalue(S.exp4b$Parameter,c('handedness'='dominantEye'))

warning('removing handedness in 4b, only right handed so far')
show(unique(S.exp4b$Parameter))




## Experiment 1
Sfun = S.exp1
extP = function(x){
  return(Sfun[Sfun$Parameter==x,'value'][[1]])
}

mcmccontrast.1 = rbind(
  data.frame(Parameter='location BS',value= (extP('stimLLoc') - (extP('stimRLoc')))/2))


estimates.1 = ddply(mcmccontrast.1,.(Parameter),function(x){
  cbind(data.frame(median=-median(x$value)),
        t(quantile(-x$value,c(0.01,0.025,0.975,0.99))))
})
colnames(estimates.1) = c('Parameter','median','hdi0.01','hdi0.025','hdi0.975','hdi0.99')

## Experiment 2
Sfun = S.exp2
extP = function(x){
  return(Sfun[Sfun$Parameter==x,'value'][[1]])
}

mcmccontrast.2 = rbind(
  data.frame(Parameter='location BS',value= (extP('stimLLoc') - (extP('stimRLoc')))/2),
  data.frame(Parameter='location above',value= ( ((extP('stimLLoc') + extP('stimLLoc:controlTrial3')) - (extP('stimRLoc') + extP('stimRLoc:controlTrial3'))))/2),
  data.frame(Parameter='BS - above',value = -( extP('stimLLoc:controlTrial3') - extP('stimRLoc:controlTrial3') )/2) ) # minus because we want to know BS - above, i.e. BS - (BS+BS:above) > 0 => BS cancels and we are left with -BS:above >0


estimates.2 = ddply(mcmccontrast.2,.(Parameter),function(x){
  cbind(data.frame(median=-median(x$value)),
        t(quantile(-x$value,c(0.01,0.025,0.975,0.99))))
})
colnames(estimates.2) = c('Parameter','median','hdi0.01','hdi0.025','hdi0.975','hdi0.99')


## Experiment 3

Sfun = S.exp3
extP = function(x){
  return(Sfun[Sfun$Parameter==x,'value'][[1]])
}

mcmccontrast.3 = rbind( data.frame(Parameter='location outside',value= (extP('stimLLoc') - (extP('stimRLoc')))/2),
                        data.frame(Parameter='location BS',value= ( ((extP('stimLLoc') + extP('stimLLoc:controlTrial1')) - (extP('stimRLoc') + extP('stimRLoc:controlTrial1'))))/2),
                        data.frame(Parameter='location inside',value= ( ((extP('stimLLoc') + extP('stimLLoc:controlTrial2')) - (extP('stimRLoc') + extP('stimRLoc:controlTrial2'))))/2),
                        data.frame(Parameter='BS - outside',value = ((extP('stimLLoc:controlTrial1')) - extP('stimRLoc:controlTrial1'))/2),
                        data.frame(Parameter='BS - inside',value = ((extP('stimLLoc:controlTrial1') - extP('stimLLoc:controlTrial2')) - (extP('stimRLoc:controlTrial1') - extP('stimRLoc:controlTrial2')))/2) )



estimates.3 = ddply(mcmccontrast.3,.(Parameter),function(x){
  cbind(data.frame(median=-median(x$value)),
        t(quantile(-x$value,c(0.01,0.025,0.975,0.99))))
})
colnames(estimates.3) = c('Parameter','median','hdi0.01','hdi0.025','hdi0.975','hdi0.99')



## Experiment 4a

Sfun = S.exp4a
extP = function(x){
  return(Sfun[Sfun$Parameter==x,'value'][[1]])
}

mcmccontrast.4a = rbind(
  data.frame(Parameter='location BS',value= (extP('stimLLoc') - (extP('stimRLoc')))/2))


estimates.4a = ddply(mcmccontrast.4a,.(Parameter),function(x){
  cbind(data.frame(median=-median(x$value)),
        t(quantile(-x$value,c(0.01,0.025,0.975,0.99))))
})
colnames(estimates.4a) = c('Parameter','median','hdi0.01','hdi0.025','hdi0.975','hdi0.99')



## Experiment 4b


Sfun = S.exp4b
extP = function(x){
  return(Sfun[Sfun$Parameter==x,'value'][[1]])
}

mcmccontrast.4b = rbind(
  data.frame(Parameter='location BS',value= (extP('stimLLoc') - (extP('stimRLoc')))/2),
  data.frame(Parameter='location above',value= ( ((extP('stimLLoc') + extP('stimLLoc:controlTrial3')) - (extP('stimRLoc') + extP('stimRLoc:controlTrial3'))))/2),
  data.frame(Parameter='BS - above',value = -( extP('stimLLoc:controlTrial3') - extP('stimRLoc:controlTrial3') )/2) ) # minus because we want to know BS - above, i.e. BS - (BS+BS:above) > 0 => BS cancels and we are left with -BS:above >0


estimates.4b = ddply(mcmccontrast.4b,.(Parameter),function(x){
  cbind(data.frame(median=-median(x$value)),
        t(quantile(-x$value,c(0.01,0.025,0.975,0.99))))
})
colnames(estimates.4b) = c('Parameter','median','hdi0.01','hdi0.025','hdi0.975','hdi0.99')



# Combine and move to probabilities
invlogit <- function(x){exp(x)/(1+exp(x))}
estimates.exp = rbind(estimates.1,estimates.2,estimates.3,estimates.4a,estimates.4b)
estimates.exp = cbind(experiment=c(rep('EEG',length(estimates.1[,1])),
                                   rep('Control',length(estimates.2[,1])),
                                   rep('Horiz',length(estimates.3[,1])),
                                   rep('InsetA',length(estimates.4a[,1])),
                                   rep('InsetB',length(estimates.4b[,1]))
),estimates.exp)
estimates.exp.prob = cbind(Parameter=estimates.exp[,1:2],round(invlogit(estimates.exp[,3:7])*100-50,2))

return(estimates.exp.prob)
}
