fd_helper_ddply_MeanCIoverSubjects = function(df,name,yname="choicetime",fun="mean"){
  source('calc_boot_ci.R')
  library(plyr)
  
#browser()
  
  if (fun=="mean"){
  subjectMean = ddply(df,"subject",function(x){mean(x[[yname]],na.rm=TRUE)})
  df = ddply(df,"subject",function(x){
    ddply(x,name,function(y){mean = mean(y[[yname]],na.rm = TRUE)})
  })
  }else if(fun=="sd"){
    subjectMean = ddply(df,"subject",function(x){sd(x[[yname]],na.rm=TRUE)})
    df = ddply(df,"subject",function(x){
      ddply(x,name,function(y){mean = sd(y[[yname]],na.rm = TRUE)})
    })
  } else if(fun=="mad"){
      subjectMean = ddply(df,"subject",function(x){mad(x[[yname]],na.rm=TRUE)})
      df = ddply(df,"subject",function(x){
        ddply(x,name,function(y){mean = mad(y[[yname]],na.rm = TRUE)})
      })
  }
 

  colnames(df)[length(df)]<-yname
  #http://drsmorey.org/bibtex/upload/Morey:2008.pdf
  print("Calculating Cousineau Corrected CI (subject intercept normalized)")
  x = ddply(df,name,function(x,sMeans){
    if(length(x[,1])<1) return(data.frame(mean = NA,conf.low=NA,conf.high=NA))
    if(length(x[,1]) <2){
      
      warning('sorry cant calculate the CI, not enough subjects,filling NA')
      ci = c(NA,NA)
      
    }else{
      
      # why the hell the matching? I don't understand... but it also does not hurt
      match = sMeans$subject%in%x$subject
      ci = calc_boot_ci(x[[yname]]-sMeans$V1[match]+mean(sMeans$V1[match]))# cousineau correction (normalize e.g. get rid of subject intercept)
    }
    data.frame(mean = mean(x[[yname]],na.rm = TRUE),
               conf.low   = ci[1],
               conf.high  = ci[2])},
    subjectMean)
}