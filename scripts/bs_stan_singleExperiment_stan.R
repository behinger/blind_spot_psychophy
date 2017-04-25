#!/usr/bin/Rscript

setwd('/net/store/nbp/users/behinger/projects/blindspot/git/scripts')

source('./revisions_1/bs_init.R')


#setwd('/net/store/nbp/projects/EEG/blind_spot/bs_svn/analysis/r-analysis')
GRIDMODE = TRUE
#GRIDMODE = FALSE
GRIDMODE = '4b'
GRIDMODE = 'all'
source('grid_tools.R')
cfg_grid = list(gridOutputPath = file.path('../tmp/gridoutput'),
                requirements = 'mem=10G,h=!ramsauer.ikw.uni-osnabrueck.de',
                add_to_filename = GRIDMODE,
                t = '1',
                script_name = 'bs_stan_singleExperiment_stan.R')

if (1==0){  run_on_grid(cfg_grid)  }


if(GRIDMODE == '1'){
  # #----
  # # Experiment 1
  formula = ~1+stimLLoc + stimRLoc +oneBack
  # formula = ~1+stimLLoc*controlTrial + controlTrial*stimRLoc +oneBack
  X <- unname(model.matrix(formula, dat.exp1))
  X_fix = with(dat.exp1,cbind(handedness,dominantEye))#,experiment=='Control',experiment=='Horizontal'))
  
  betaNames = colnames( model.matrix(formula, dat.exp1))
  betaNames[2:3] = c("stimLLoc:controlTrial1", "stimRLoc:controlTrial1")
  beta_fix_Names = c('handedness','dominantEye')
  label_dataframe = data.frame(Parameter=sprintf('beta[%i]',1:length(betaNames)),Label=betaNames)
  label_dataframe = rbind(label_dataframe,data.frame(Parameter=sprintf('beta_fix.%i.',1:length(beta_fix_Names)),Label=beta_fix_Names))
  attr(X,"assign") <- NULL
  attr(X_fix,"assign") <- NULL
  
  # 4. Make Stan data.
  stanDat <- within(list(),
                    {
                      N<-nrow(X)
                      P <- n_u <- n_w <- ncol(X)
                      P2 <- ncol(X_fix)
                      X <- Z_u <- Z_w <- X
                      X_fix <- X_fix
                      J <- length(levels(dat.exp1$subject))
                      answer <- as.logical(dat.exp1$answer)
                      subj <- as.integer(dat.exp1$subject)
                    }
  )
  
  factorialFit.exp1 <- stan(file="matrix_model.stan",data=stanDat,iter=8000, chains=6,cores = 6,open_progress = F)
  save(factorialFit.exp1,file=('stan_exp1_25_09_2015'))
}

if(GRIDMODE == '2'){
  
  #----
  # Experiment 2

  # 2. Make design matrix.
  formula = ~1+stimLLoc*controlTrial + controlTrial*stimRLoc +oneBack

  X <- unname(model.matrix(formula, dat.exp2))
  X_fix = with(dat.exp2,cbind(handedness,dominantEye))

  betaNames = colnames( model.matrix(formula, dat.exp2))
  betaNames[7] = c("stimRLoc:controlTrial3")
  beta_fix_Names = c('handedness','dominantEye')
  label_dataframe = data.frame(Parameter=sprintf('beta[%i]',1:length(betaNames)),Label=betaNames)
  label_dataframe = rbind(label_dataframe,data.frame(Parameter=sprintf('beta_fix.%i.',1:length(beta_fix_Names)),Label=beta_fix_Names))
  attr(X,"assign") <- NULL
  attr(X_fix,"assign") <- NULL

  # 4. Make Stan data.
  stanDat <- within(list(),
                    {
                      N<-nrow(X)
                      P <- n_u <- n_w <- ncol(X)
                      P2 <- ncol(X_fix)
                      X <- Z_u <- Z_w <- X
                      X_fix <- X_fix
                      J <- length(levels(dat.exp2$subject))
                      answer <- as.logical(dat.exp2$answer)
                      subj <- as.integer(dat.exp2$subject)
                    }
  )
  # 5. Fit the model.
  factorialFit.exp2 <- stan(file="matrix_model.stan",data=stanDat,iter=8000, chains=6,cores = 6,open_progress = T)
  save(factorialFit.exp2,file=('stan_exp2_25_09_2015'))

}

# #----

if(GRIDMODE == '3'){
  
  # Experiment 3


  # 2. Make design matrix.
  formula = ~1+stimLLoc*controlTrial + controlTrial*stimRLoc +oneBack

  X <- unname(model.matrix(formula, dat.exp3))
  X_fix = with(dat.exp3,cbind(handedness,dominantEye))

  betaNames = colnames( model.matrix(formula, dat.exp3))
  betaNames[9:10] = c("stimRLoc:controlTrial1","stimRLoc:controlTrial2")
  beta_fix_Names = c('handedness','dominantEye')
  label_dataframe = data.frame(Parameter=sprintf('beta[%i]',1:length(betaNames)),Label=betaNames)
  label_dataframe = rbind(label_dataframe,data.frame(Parameter=sprintf('beta_fix.%i.',1:length(beta_fix_Names)),Label=beta_fix_Names))
  attr(X,"assign") <- NULL
  attr(X_fix,"assign") <- NULL

  # 4. Make Stan data.
  stanDat <- within(list(),
                    {
                      N<-nrow(X)
                      P <- n_u <- n_w <- ncol(X)
                      P2 <- ncol(X_fix)
                      X <- Z_u <- Z_w <- X
                      X_fix <- X_fix
                      J <- length(levels(dat.exp3$subject))
                      answer <- as.logical(dat.exp3$answer)
                      subj <- as.integer(dat.exp3$subject)
                    }
  )
  # 5. Fit the model.
  factorialFit.exp3 <- stan(file="matrix_model.stan",data=stanDat,iter=8000, chains=6,cores = 6,open_progress = T)
  save(factorialFit.exp3,file=('stan_exp3_25_09_2015'))
}
#----
if(GRIDMODE == '4a'){
  
  # Experiment 4a
  formula = ~1+stimLLoc + stimRLoc +oneBack
  # formula = ~1+stimLLoc*controlTrial + controlTrial*stimRLoc +oneBack
  X <- unname(model.matrix(formula, dat.exp4a))
  X_fix = with(dat.exp4a,cbind(handedness,dominantEye))#,experiment=='Control',experiment=='Horizontal'))
  
  betaNames = colnames( model.matrix(formula, dat.exp4a))
  betaNames[2:3] = c("stimLLoc:controlTrial1", "stimRLoc:controlTrial1")
  beta_fix_Names = c('handedness','dominantEye')
  label_dataframe = data.frame(Parameter=sprintf('beta[%i]',1:length(betaNames)),Label=betaNames)
  label_dataframe = rbind(label_dataframe,data.frame(Parameter=sprintf('beta_fix.%i.',1:length(beta_fix_Names)),Label=beta_fix_Names))
  attr(X,"assign") <- NULL
  attr(X_fix,"assign") <- NULL
  #X_fix[is.na(X_fix)] = 0
  # 4. Make Stan data.
  stanDat <- within(list(),
                    {
                      N<-nrow(X)
                      P <- n_u <- n_w <- ncol(X)
                      P2 <- ncol(X_fix)
                      X <- Z_u <- Z_w <- X
                      X_fix <- X_fix
                      J <- length(levels(dat.exp4a$subject))
                      answer <- as.logical(dat.exp4a$answer)
                      subj <- as.integer(dat.exp4a$subject)
                    }
  )
  
  factorialFit.exp4a <- stan(file="matrix_model.stan",data=stanDat,iter=8000, chains=6,cores = 6,open_progress = F)
  
  
  
  save(factorialFit.exp4a,file=('stan_exp4a_01_11_2017'))
}

if(GRIDMODE == '4b'){
  
  # # Experiment 4b
  # 
  # 2. Make design matrix.
  formula = ~1+stimLLoc*controlTrial + controlTrial*stimRLoc +oneBack
  
  X <- unname(model.matrix(formula, dat.exp4b))
  X_fix = with(dat.exp4b,cbind(handedness,dominantEye))
  
  
  betaNames = colnames( model.matrix(formula, dat.exp4b))
  betaNames[7] = c("stimRLoc:controlTrial3")
  beta_fix_Names = c('handedness','dominantEye')
  label_dataframe = data.frame(Parameter=sprintf('beta[%i]',1:length(betaNames)),Label=betaNames)
  label_dataframe = rbind(label_dataframe,data.frame(Parameter=sprintf('beta_fix.%i.',1:length(beta_fix_Names)),Label=beta_fix_Names))
  attr(X,"assign") <- NULL
  attr(X_fix,"assign") <- NULL
  
  # 4. Make Stan data.
  stanDat <- within(list(),
                    {
                      N<-nrow(X)
                      P <- n_u <- n_w <- ncol(X)
                      P2 <- ncol(X_fix)
                      X <- Z_u <- Z_w <- X
                      X_fix <- X_fix
                      J <- length(levels(dat.exp4b$subject))
                      answer <- as.logical(dat.exp4b$answer)
                      subj <- as.integer(dat.exp4b$subject)
                    }
  )
  # 5. Fit the model.
  factorialFit.exp4b <- stan(file="matrix_model.stan",data=stanDat,iter=8000, chains=6,cores = 6,open_progress = T)
  save(factorialFit.exp4b,file=('stan_exp4b_18_01_2017'))
}
#----
# Experiment All

if(GRIDMODE == 'all'){
  
  # 2. Make design matrix.
  formula = ~1+stimLLoc*controlTrial + controlTrial*stimRLoc +oneBack+experiment
  datAllStan = subset(dat.uni,dat.uni$experiment!='Inset4a')
  datAllStan$experiment = factor(datAllStan$experiment)
  X <- unname(model.matrix(formula, datAllStan))
  X_fix = with(datAllStan,cbind(handedness,dominantEye))
  
  
  attr(X,"assign") <- NULL
  attr(X_fix,"assign") <- NULL
  
  # 4. Make Stan data.
  stanDat <- within(list(),
                    {
                      N<-nrow(X)
                      P <- n_u <- n_w <- ncol(X)
                      P2 <- ncol(X_fix)
                      X <- Z_u <- Z_w <- X
                      X_fix <- X_fix
                      J <- length(levels(datAllStan$subject))
                      answer <- as.logical(datAllStan$answer)
                      subj <- as.integer(datAllStan$subject)
                    }
  )
  # 5. Fit the model.
  factorialFit.expAll <- stan(file="matrix_model.stan",data=stanDat,iter=2000, chains=6,cores = 6,open_progress = F)
  save(factorialFit.expAll,file=('stan_expAll_06_02_2017'))
}
# 
# 
# #----
# # Experiment All Subjects
# dat.allSub = subset(allDat,allDat$removeReason!='performance' &allDat$removeReason!='Performance'&allDat$removeReason!='task inverse'&allDat$removeReason!='Task inverse'&removeReason!='technical'&allDat$success==1&allDat$stimPVisible!=2)
# dat.allSub$controlTrial = factor(dat.allSub$controlTrial)
# dat.allSub$subject = factor(dat.allSub$subject)
# 
# # 2. Make design matrix.
# formula = ~1+stimLLoc*controlTrial + controlTrial*stimRLoc +oneBack+experiment
# 
# X <- unname(model.matrix(formula, dat.allSub))
# X_fix = with(dat.allSub,cbind(handedness,dominantEye))
# 
# attr(X,"assign") <- NULL
# attr(X_fix,"assign") <- NULL
# 
# # 4. Make Stan data.
# stanDat <- within(list(),
#                   {
#                     N<-nrow(X)
#                     P <- n_u <- n_w <- ncol(X)
#                     P2 <- ncol(X_fix)
#                     X <- Z_u <- Z_w <- X
#                     X_fix <- X_fix
#                     J <- length(levels(dat.allSub$subject))
#                     answer <- as.logical(dat.allSub$answer)
#                     subj <- as.integer(dat.allSub$subject)
#                   }
# )
# # 5. Fit the model.
# factorialFit.expAll_allSub <- stan(file="matrix_model.stan",data=stanDat,iter=2000, chains=6,cores = 6,open_progress = T)
# save(factorialFit.expAll_allSub,file=('stan_expAll_allSub_29_09_2015'))
