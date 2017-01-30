
if(1==0){
  postPredAll = NULL
  for(exp in c("EEG","Control","Inset4b",'Inset4a')){
    if(exp=='EEG'){ 
      stanfit = factorialFit.exp1
      data = dat.exp1
      }
    if(exp=='Control'){
      stanfit = factorialFit.exp2
      data = dat.exp2
      }
    if(exp=='Horizontal'){
      stanfit = factorialFit.exp3
      data = dat.exp3
      }
    if(exp=='Inset4b'){ 
      stanfit = factorialFit.exp4b
      data = dat.exp4b
      data = subset(data,!(data$subject%in%c('Inset4b.35','Inset4b.38','Inset4b.39','Inset4b.40','Inset4b.41'))) # I did not yet put the dmoinant eye / handedness
      }
    if(exp=='Inset4a'){ 
      stanfit = factorialFit.exp4a
      data = dat.exp4a
      data = subset(data,!(data$subject%in%c('Inset4a.35','Inset4a.38','Inset4a.39','Inset4a.40','Inset4a.41'))) # I did not yet put the dmoinant eye / handedness
    }
    
    
    if (exp =='EEG' | exp == 'Inset4a'){
    formRan = ~1+stimLLoc + stimRLoc +oneBack 
    }else{
    formRan = ~1+stimLLoc*controlTrial + controlTrial*stimRLoc +oneBack
    }
    if(exp=='Inset4b'|exp=='Inset4a'){
      formFix = ~0+dominantEye
    }else{
      formFix = ~0+handedness+dominantEye
    }
    #current.na.action <- options('na.action')
    #options(na.action='na.pass')
    
    ranX <- model.matrix(formRan, data)
    fixX <- model.matrix(formFix, data)
    #options(na.action=current.na.action)
    subjectIndexVector = factor(data$subject)
    options(warn=2)
    postPred = bs_posteriorPredictive(stanfit,ranX=ranX,fixX = fixX,subjectIndexVector = subjectIndexVector)
    
    if(exp=='Control' || exp == 'Inset4b'){
    postPredSub = subset(postPred,postPred$controlTrial3 == 0)
    }                     
    if(exp=='EEG' | exp =='Inset4a'){
      postPredSub = postPred
    }            
    if(exp=='Horizontal' ){
      postPredSub = subset(postPred,postPred$controlTrial1 == 1)
    }                     
    postPredCum = ddply(postPredSub,.(stimLLoc,stimRLoc,subject,postPredIteration,type),summarise,answer=mean(answer))
    #levels(postPredCum$subject) <- levels(data$subject)
    
    postPredAll = rbind(postPredAll,cbind(postPredCum,experiment=exp))
  }


  tmp = subset(dat.uni,dat.uni$controlTrial==1 & dat.uni$experiment!='Horizontal')
  
  ggplot(subset(postPredAll,postPredAll$type =='sameSubjectPP'),aes(x=interaction(stimLLoc,stimRLoc),y=answer))+
    geom_violin(scale='width')+
    geom_point(data = ddply(tmp,.(stimLLoc,stimRLoc,subject),summarise,answer=mean(answer)))+facet_wrap(~subject)+tBE()+
    theme(strip.background = element_blank(),
          strip.text.x = element_blank())
  
  
  postPredCumNewSubject = ddply(subset(postPredCum,postPredCum$type =='newSubjectPP'),.(subject,postPredIteration,stimLLoc,stimRLoc),summarize,answer = mean(answer))
  
  # We do not want inset 4a in this plot, it has the reverse bias
  ppSubjectDiff = ddply(subset(postPredCum,postPredCum$type =='newSubjectPP' & experiment!='Inset4a'),.(subject,postPredIteration),summarize,answerDiff = mean(answer[stimLLoc==0 & stimRLoc==1] - answer[stimLLoc==1 & stimRLoc==0]))

  subjectDiff = ddply(tmp,.(subject,experiment),summarise,answerDiff=mean(answer[stimLLoc==0 & stimRLoc==1] - answer[stimLLoc==1 & stimRLoc==0]))
  subjectDiff = subset(subjectDiff,subjectDiff$experiment != 'Inset4a')
  ggplot(subjectDiff,aes(x=100*answerDiff))+
    geom_histogram(aes(y=..density..))+
    geom_density(color='red',data=ppSubjectDiff,size=1)+tBE(20) + 
    geom_density(size=1)+geom_point(aes(y=-0.003),position=position_jitter(height=0.001))+
    xlab('mean Blind Spot Effect [%]')
  
  

  
}
bs_posteriorPredictive = function(stanfit,ranX,fixX,subjectIndexVector,nIter = 100){
  #browser()
  nSub = length(unique(subjectIndexVector))
  check_param_in_model = function(param,model){
    return(length(grep(param,colnames(model),fixed=T))>0)
  }
  
  library(ggmcmc)
  S = ggs(stanfit)
  #browser()
  subjectLevels = levels(subjectIndexVector)
  #subjectIndexVector = factor(subjectIndexVector) #get rid of old levels
  #levels(subjectIndexVector) = 1:length(unique(subjectIndexVector)) #convert them to 1:N
  #subjectIndexVector = as.numeric(subjectIndexVector)  # make them numerical (which they shouldbe already)
  
  #parmList = unique(S$Parameter)
  #fit@par_dims
  # beta = fixed effects
  # sigma_u = random-variances
  # L_u = random-correlations (half of matrix
  # z_u = ?
  # u = subjRanef
  
  predOut = predNewSub = NULL
  S$InteractionChainIter = (S$Chain-1)*max(S$Iteration) + S$Iteration
  
  iter_list =  sample.int(max(S$InteractionChainIter),nIter)
  
  for (it in 1:nIter){
  
    nTrials = length(ranX[,1])
    show(sprintf('iteration %i',it))
    
    it_idx = iter_list[it]
    
    k = S[S$InteractionChainIter==it_idx,]
    beta = k$value[grep('beta',k$Parameter)]
    n_beta_fix = length(grep('beta_fix',k$Parameter))
    assertthat::are_equal(dim(fixX)[2],n_beta_fix)
    assertthat::are_equal(dim(ranX)[2],beta-n_beta_fix)
    # beta[c(1:3,5:20)] = 0
    sigma = k$value[grep('sigma.u',k$Parameter)]
    u = k$value[grep('^u.',k$Parameter)]%>%matrix(ncol=length(grep('sigma.u',k$Parameter)))
    covmat= k$value[grep('L.u',k$Parameter)]%>%matrix(nrow=length(grep('sigma.u',k$Parameter)))
    covmat = (covmat)%*%t(covmat)
    #sigma[1:length(sigma)] = 0.01
    #browser()
   # browser()
    
    # This is pulling new subjects out of the estimated random effects matrix, i.e. measuring nSub new subjects
    ran = mnormt::rmnorm(n=nSub,mean=beta[1:(length(beta)-n_beta_fix)],varcov=diag(sigma) %*% covmat %*%diag(sigma))
    
    # This is taking the estimates of the subjects values, i.e. measuring the same subjects again
    ran.sub = t(t(u)+beta[1:(length(beta)-length(fixX[1,]))])
    fix = beta[(length(beta)-length(fixX[1,])+1):length(beta)]
    # This is a bit ugly because I did not know how to use adply with an additional loop-index (which one needs to tile down the fullModelMatrix.
    # Thus the head/tail combo
    #ran = cbind(ran,1:nSub)
    #ran.sub = cbind(ran.sub,1:nSub)
    
    get_pred = function(ran,fix,ranX,fixX){
      pred = NULL
      for(rI in 1:dim(ran)[1]){

       X = ranX[subjectIndexVector == subjectLevels[rI],] 
       fX = fixX[subjectIndexVector == subjectLevels[rI],]
       #browser()
       Xb = ran[rI,]%*%t(X) + fix %*%t(fX)
       sim = sapply(plogis(Xb),function(p)rbinom(1,1,p))
       pred = rbind(pred,cbind(data.frame(answer=sim,trial=1:dim(X)[1]),X,fX,subject=subjectLevels[rI]))
      }
      #pred = adply(ran,1,function(x){
        #X=ranX[subjectIndexVector==tail(x,1),]
        #fX=fixX[subjectIndexVector==tail(x,1),]
        
        #Simulate data
        
        #return(dat)},
        #.id='subject',.inform=F)
      
      return(pred)
      }
    
    pred = get_pred(ran,fix,ranX,fixX) # for now we don't want PostPred for 'new'-subjects
    pred.sub = get_pred(ran.sub,fix,ranX,fixX)
    
    #browser()
    #pred$subject = factor(subjectIndexVector[pred$subject])
    #pred.sub$subject = factor(subjectIndexVector[pred.sub$subject])
    predOut = rbind(predOut,cbind(postPredIteration = it,pred.sub))
    
    predNewSub = rbind(predNewSub,cbind(postPredIteration = it,pred))

  }
  #levels(predOut$subject) = subjectLevels
  #levels(predNewSub$subject) = subjectLevels
  return(rbind(cbind(type = 'sameSubjectPP',predOut),
               cbind(type=  'newSubjectPP', predNewSub)))
    
    

}


