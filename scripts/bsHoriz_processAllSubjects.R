############################################################################
###                     FUNKTIONEN FUER ALLE SUBJECTS                    ###
###                 FUER BLINDSPOT HORIZONTAL EXPERIMENT                 ###
############################################################################

source("bsHoriz_processOneSubject.R")

#---------------------------------------------------------------------------
#
# Gibt Liste der auszuwertenden Subject-Nummern zurueck
#
getSubjectNumbers <- function(){

  return(c("03", "04", "05", "06", "09", "10", "13", "14", "16", "17", "18", "19", "20", "21", "22", "25", "26", "27", "28", "29", "30", "32", "34"))
}


createHorizontalDataFrame <- function(){
  horizontalDataFrame = data.frame()
  
  subjectNumbersHorizontal = getSubjectNumbers()                                      
  
  for(number in subjectNumbersHorizontal){
    print(paste("Processing Horizontal Experiment Subject", number))
    subject = rep(number, 720)
    experiment = rep("Horizontal", 720)
    horizontalDataFrame = rbind(horizontalDataFrame, 
                          cbind(processStimuli(paste("/net/store/nbp/projects/EEG/blind_spot/data/horizontal/", number, "/bs_", number, "_hz.mat", sep = ""), "Horizontal"), 
                                subject, 
                                experiment))
  }
  return(horizontalDataFrame)
}

#---------------------------------------------------------------------------
#
# Fuegt alle Data Frames von processStimuli zusammen und ergaenzt sie um die Variablen subject und oneBack
#
createAllDataFrame <- function(){
  
#   allDataFrame$subject = as.vector(allDataFrame$subject)
  
  allDataFrame = data.frame()
  # Vier Experimente: EEG, Replication, Control, Horizontal
  # Variable "experiment" hinzufuegen
  
  
  #------------------ EEG
  subjectNumbersEEG = c('A1','B1','B2','C1','C2','D1','D1','D2','E1','E2','G1','G2','H1','H2','J1','J2','K1','K2','L1','L2','M1','M2','N1','N2','T1','T2','U1','U2','V1','V2') 
  subjectFileNames = c('a1','b1','b2','c1','c2','D1a','D1b','d5','e2','e5','g2','g5','h2','h5','j2','j5','k2','k5','l2','l5','m2','m6','n1','n6','t2','t6','u2','u6','v1','v6')
  
  for(i in 1:length(subjectNumbersEEG)){
    print(paste("Processing EEG Experiment Subject", subjectNumbersEEG[i]))
    if(subjectNumbersEEG[i] == 'A1'){
      subject = rep(substring(subjectNumbersEEG[i], 1, 1), 600)
      experiment = rep("EEG", 600)
    } else if(subjectFileNames[i] == 'D1a'){
      subject = rep(substring(subjectNumbersEEG[i], 1, 1), 120)
      experiment = rep("EEG", 120)
    } else if(subjectFileNames[i] == 'D1b'){
      subject = rep(substring(subjectNumbersEEG[i], 1, 1), 240)
      experiment = rep("EEG", 240)     
    } else{
      subject = rep(substring(subjectNumbersEEG[i], 1, 1), 360)
      experiment = rep("EEG", 360)
    }
    if(subjectFileNames[i] == 'D1a'){
      fileName = "bs_d_2_v_r.mat_error"
    } else if(subjectFileNames[i] == 'D1b'){
      fileName = "bs_d3_vr.mat_error"
    } else{
      fileName = paste("bs_",subjectFileNames[i],"_vr.mat", sep = "")
    }

    allDataFrame = rbind(allDataFrame, 
                         cbind(processStimuli(paste("/net/store/nbp/projects/EEG/blind_spot/data/VP",subjectNumbersEEG[i],"/psyphy/", fileName, sep = ""), "EEG"), 
                               subject,
                               experiment))
  }

  
  #------------------ REPLICATION
  # subjectNumbersReplication = c('01','50','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','19','20','21','22','23','24','25','26') # alle Subjects
  # nach Rausschmeissen unbrauchbarer Subjects:
  subjectNumbersReplication = c('01','50','02','04','05','21','23','25','26') 
  
  for(number in subjectNumbersReplication){

    if(number == '01' || number == '50'){
      print("Processing Replication Experiment Subject R01")
      subject = rep("R01", 360)
      experiment = rep("Replication", 360)
    } else{
    print(paste("Processing Replication Experiment Subject", paste("R",number, sep="")))
    subject = rep(paste("R",number, sep=""), 720)
    experiment = rep("Replication", 720)
    }
    allDataFrame = rbind(allDataFrame, 
                         cbind(processStimuli(paste("/net/store/nbp/projects/EEG/blind_spot/data/psyphy/bs_", number, "_vr.mat", sep = ""), "Replication"), 
                               subject, 
                               experiment))
  }
  
  
  #------------------ CONTROL
  subjectNumbersControl = c('c1','c2','c3','c4','c5','c6','c7','c8','c9','d1','d2','d3','d4','e1','e3','e4','e6','e7','e8','e9','f2','f3','f6','g1','h1','h4','h5','i1','i3','i6','i9','j1','j2','j3','j4')                                      
  subjectNumbersControl = subjectNumbersControl[-c(2,6,8,9,10,12,26,28)]

  for(number in subjectNumbersControl){
    print(paste("Processing Control Experiment Subject", number))
    subject = rep(number, 720)
    experiment = rep("Control", 720)
    allDataFrame = rbind(allDataFrame, 
                         cbind(processStimuli(paste("/net/store/nbp/projects/EEG/blind_spot/data/psyphycontrol/bs_", number, "_vr.mat", sep = ""), "Control"), 
                               subject, 
                               experiment))
  }
  
  
  #------------------ HORIZONTAL
  subjectNumbersHorizontal = getSubjectNumbers()                                      

  for(number in subjectNumbersHorizontal){
    print(paste("Processing Horizontal Experiment Subject", number))
    subject = rep(number, 720)
    experiment = rep("Horizontal", 720)
    allDataFrame = rbind(allDataFrame, 
                         cbind(processStimuli(paste("/net/store/nbp/projects/EEG/blind_spot/data/horizontal/", number, "/bs_", number, "_hz.mat", sep = ""), "Horizontal"), 
                               subject, 
                               experiment))
  }

  return(allDataFrame) 
}


#---------------------------------------------------------------------------
#
# Erstellt einen Data Frame, der die Haeufigkeiten der moeglichen Kombinationen von stimLLoc, stimRLoc, controlTrial und subject enthaelt
#
createPropDataFrame <- function(){
  
  library("plyr")
  if(exists("allDataFrame") == FALSE) {allDataFrame <- createAllDataFrame()}
  
  subsetAllDataFrame = subset(allDataFrame, (allDataFrame$success == 1 & allDataFrame$stimPVisible != 2))     # Behalte nur Trials, die nicht abgebrochen wurden und die keine sichtbaren Perpendicular enthalten
  smallAllDataFrame = subsetAllDataFrame[c("stimLLoc", "stimRLoc", "controlTrial", "subject", "answer")]      # Erstelle neuen Data Frame, der nur die relevanten Variablen enthaelt
  propDataFrame = ddply(smallAllDataFrame, c("stimLLoc", "stimRLoc", "controlTrial", "subject"), summarise, prop=mean(answer))  # Berechne die Haeufigkeiten der moeglichen Kombinationen 
  
  return(propDataFrame)
}


#---------------------------------------------------------------------------
#
# Erstellt einen Scatterplot, der rechts temporal (x) gegen links temporal (y) plottet
#
createScatterPlot <- function(){
 
  
  library("ggplot2")
  if(exists("propDataFrame") == FALSE) {propDataFrame <- createPropDataFrame()} 
  
  controlsDataFrame = data.frame()
  subjectNumbers = tapply(propDataFrame$subject, propDataFrame$controlTrial, FUN = unique)
  
  for(i in 0:3){
    x = propDataFrame$prop[propDataFrame$stimLLoc == 0 & propDataFrame$stimRLoc == 1 & propDataFrame$controlTrial == i]
    y = (1-propDataFrame$prop[propDataFrame$stimLLoc == 1 & propDataFrame$stimRLoc == 0 & propDataFrame$controlTrial == i])
    subject = as.vector(subjectNumbers[[as.character(i)]])
    controlTrial = rep(i, length(x))
    controlsDataFrame = rbind(controlsDataFrame, data.frame(x, y, subject, controlTrial))
  }
  
  ggplot(controlsDataFrame) +
    geom_abline(intercept = 1, slope = -1) +
    geom_abline(intercept = 0, slope = 1) +
    geom_text(aes(x = x, y = y, label = subject, colour = factor(controlTrial))) +
    labs(x = "Answer when right temporal, left nasal (0 = left, 1 = right)", 
         y = "Answer when left temporal, right nasal (0 = right, 1 = left)", 
         title = "Biases") +
    xlim(0,1) + ylim(0,1) +
    coord_fixed() +
    scale_colour_discrete(name = "Control", breaks = c("0", "1", "2", "3"), 
                          labels = c("Control 0 (O . . + . . O)","Control 1 (. O . + . O .)", "Control 2 (. . O + O . .)", "Control 3 (. ° . + . ° .)") )

}







#---------------------------------------------------------------------------
#
# Erstellt Boxplots, die den Bias der einzelnen Conditions zeigen
#
createBoxPlot <- function(){
  
  library("ggplot2")
  if(exists("propDataFrame") == FALSE) {propDataFrame <- createPropDataFrame()} 
  
  ggplot(propDataFrame, aes(x = factor(2*stimLLoc+stimRLoc), y = prop*100, fill = factor(controlTrial))) + geom_boxplot() +  # 2*stimLLoc+stimRLoc erzeugt Werte 0, 1, 2, 3 fuer die einzelnen Kombinationen von stimLLoc und stimRLoc
    geom_abline(intercept = 50, slope = 0) +                              # Zeichnet Linie durch Chance-Level (50%)
    facet_grid(controlTrial ~ .) +                                        # Plottet drei Plots fuer die verschiedenen Controls                                                       
    geom_jitter(position = position_jitter(width = .1)) +                 # Fuegt Punkte fuer die einzelnen Subjectwerte hinzu
    scale_x_discrete(breaks = 0:3, labels=c("both Nasal", "left Nasal \n right Temporal", "left Temporal \n right Nasal", "both Temporal")) +
    labs(x =NULL, y = "Answer 'right' [%]", title = "Biases") +  
    scale_fill_discrete(name = "Control", breaks = c("0", "1", "2", "3"), # Aendert Beschriftung der Legende
                        labels = c("Control 0 (O . . + . . O)","Control 1 (. O . + . O .)", "Control 2 (. . O + O . .)", "Control 3 (. ° . + . ° .)") )
}


#---------------------------------------------------------------------------
#
# Erstellt Boxplots, bei denen der Bias mit den Mittelwerten der einzelnen Subjects normalisiert wurde
#
createBoxPlotNormalized <- function(){
  
  library("ggplot2")
  if(exists("propDataFrame") == FALSE) {propDataFrame <- createPropDataFrame()} 
  
  subjectNumbers = as.vector(unique(propDataFrame$subject))
  means = tapply(propDataFrame$prop,propDataFrame$subject,FUN=mean)       # Errechnet Mittelwerte von Answer der einzelnen Subjects
  propMinusMean = NULL
  
  for(subject in subjectNumbers){                                         # Zieht bei jeder Antworthaeufigkeit des Subjects seinen Mean ab (normalisiert Bias)
    propMinusMean[propDataFrame$subject == subject] = propDataFrame$prop[propDataFrame$subject == subject] - means[match(subject, names(means))]
  }
  propDataFrame = cbind(propDataFrame, propMinusMean)                     # Fuegt propMinusMean dem Data Frame hinzu
  
  ggplot(propDataFrame, aes(x = factor(2*stimLLoc+stimRLoc), y = propMinusMean*100, fill = factor(controlTrial))) + geom_boxplot() + 
    geom_abline(intercept = 0, slope = 0) +                               # Zeichnet Linie durch Chance-Level (0%)
    facet_grid(controlTrial ~ .) +                                        # Plottet drei Plots fuer die verschiedenen Controls
    geom_jitter(position = position_jitter(width = .1)) +                 # Fuegt Punkte fuer die einzelnen Subjectwerte hinzu
    scale_x_discrete(breaks = 0:3, labels=c("both Nasal", "left Nasal \n right Temporal", "left Temporal \n right Nasal", "both Temporal")) +
    labs(x =NULL, y = "Answer 'right' normalized [%]", title = "Nomalized Biases (Substracted Subjects' Means)") +
    scale_fill_discrete(name = "Control", breaks = c("0", "1", "2", "3"), # Aendert Beschriftung der Legende
                        labels = c("Control 0 (O . . + . . O)","Control 1 (. O . + . O .)", "Control 2 (. . O + O . .)", "Control 3 (. ° . + . ° .)") )
}


createBlindspotPlot <- function(){
  
  #geht nicht mit allen, da Bildschirmabstand sich veraendert hat. In Degrees umrechnen und dann entsprechend skalieren
  library("ggplot2")
  if(exists("allDataFrame") == FALSE) {allDataFrame <- createAllDataFrame()} 
  
  bsXmean = c(tapply(allDataFrame$bsXL, allDataFrame$subject, FUN = mean), tapply(allDataFrame$bsXR, allDataFrame$subject, FUN = mean))
  bsYmean = -1 * c(tapply(allDataFrame$bsYL, allDataFrame$subject, FUN = mean), tapply(allDataFrame$bsYR, allDataFrame$subject, FUN = mean))
  bsSmean = c(tapply(allDataFrame$bsSL, allDataFrame$subject, FUN = mean), tapply(allDataFrame$bsSR, allDataFrame$subject, FUN = mean))
 
  blindspotDataFrame = data.frame(bsXmean, bsYmean, bsSmean)

  ggplot(blindspotDataFrame) +
    labs(x = "X Coordinate", y = "Y Coordinate", title = "Positions of Blindspots") +
    coord_fixed(xlim = c(-960, 960), ylim = c(-540, 540)) +
    geom_point(aes(x = bsXmean, y = bsYmean, size = bsSmean), alpha = 0.05) +
    geom_point(aes(x = mean(bsXmean[1:(length(bsXmean)/2)], na.rm = TRUE), y = mean(bsYmean[1:(length(bsYmean)/2)], na.rm = TRUE), size = mean(bsSmean[1:(length(bsSmean)/2)], na.rm = TRUE)), colour = "red") +
    geom_point(aes(x = mean(bsXmean[(length(bsXmean)/2):length(bsYmean)], na.rm = TRUE), y = mean(bsYmean[(length(bsYmean)/2):length(bsYmean)], na.rm = TRUE), size = mean(bsSmean[(length(bsSmean)/2):length(bsSmean)], na.rm = TRUE)), colour = "red") +
    scale_size_continuous(range = c(min(bsSmean,na.rm=TRUE), max(bsSmean,na.rm=TRUE)))

#----------------------
  
  library("ggplot2")
  if(exists("horizontalDataFrame") == FALSE) {horizontalDataFrame <- createHorizontalDataFrame()} 
  
  bsXmean = c(tapply(horizontalDataFrame$bsXL, horizontalDataFrame$subject, FUN = mean), tapply(horizontalDataFrame$bsXR, horizontalDataFrame$subject, FUN = mean))
  bsYmean = -1 * c(tapply(horizontalDataFrame$bsYL, horizontalDataFrame$subject, FUN = mean), tapply(horizontalDataFrame$bsYR, horizontalDataFrame$subject, FUN = mean))
  bsSmean = c(tapply(horizontalDataFrame$bsSL, horizontalDataFrame$subject, FUN = mean), tapply(horizontalDataFrame$bsSR, horizontalDataFrame$subject, FUN = mean))  

  blindspotDataFrame = data.frame(bsXmean, bsYmean, bsSmean)
  
  ggplot(blindspotDataFrame) +
    labs(x = "X Coordinate", y = "Y Coordinate", title = "Positions of Blindspots") +
    coord_fixed(xlim = c(-960, 960), ylim = c(-540, 540)) +
    geom_point(aes(x = bsXmean, y = bsYmean, size = bsSmean), alpha = 0.05) +
    geom_point(aes(x = mean(bsXmean[1:(length(bsXmean)/2)], na.rm = TRUE), y = mean(bsYmean[1:(length(bsYmean)/2)], na.rm = TRUE), size = mean(bsSmean[1:(length(bsSmean)/2)], na.rm = TRUE)), colour = "red") +
    geom_point(aes(x = mean(bsXmean[(length(bsXmean)/2):length(bsYmean)], na.rm = TRUE), y = mean(bsYmean[(length(bsYmean)/2):length(bsYmean)], na.rm = TRUE), size = mean(bsSmean[(length(bsSmean)/2):length(bsSmean)], na.rm = TRUE)), colour = "red") +
    scale_size_continuous(range = c(min(bsSmean,na.rm=TRUE), max(bsSmean,na.rm=TRUE)))
  
}



#---------------------------------------------------------------------------
#
# Analysiert die Daten und zeigt Effektgroessen an
#
mixedEffectsLogisticRegression <-  function(){
  
  library("lme4")
  if(exists("allDataFrame") == FALSE) {allDataFrame <- createAllDataFrame()} 

  subsetAllDataFrame = subset(allDataFrame, (allDataFrame$success == 1 & allDataFrame$stimPVisible != 2))     # Behalte nur Trials, die nicht abgebrochen wurden und die keine sichtbaren Perpendicular enthalten
  # Mixed Effects Logistic Regression
  # Fit a generalized linear mixed model, which incorporates both fixed-effects parameters and random effects in a linear predictor, via maximum likelihood
  mres = glmer(formula = answer ~ stimLLoc * stimRLoc * factor(controlTrial) + oneBack + (1|subject), data = subsetAllDataFrame, family = binomial)
  # answer ist abhaengige Variable, haengt ab von stimLLoc, stimRLoc, controlTrial und subject
  # stimLLoc und stimRLoc werden nur berechnet, wenn sie 1 (also temporal) ist, da 0 wegfaellt
  # controlTrial hat drei Werte, muss als Faktor betrachtet werden, auch hier faellt 0 (Stimuli weit aussen) weg
  # (1|subject) addiert eine Biaskonstante. Diese Biaskonstante ist fuer jedes Subject anders
  # data = subsetAllDataFrame beschreibt, welches Data Frame analysiert werden soll
  # family = binomial beschreibt, dass die zu analysierenden Daten binomial sind (answer immer 0 oder 1)
  
  return(mres)
}


#---------------------------------------------------------------------------
#
# Plottet die Fixed Effects
#
showEffects <- function(){
  library("coefplot2")
  if(exists("mres") == FALSE) {mres <- mixedEffectsLogisticRegression()} 
  coefplot2(mres)
}


# Erstelle DataFrame mit Daten aller Subjects
if(exists("allDataFrame") == FALSE) {allDataFrame <- createAllDataFrame()} 






########################################## OLD CODE



# #---------------------------------------------------------------------------
# #
# # Erstellt einen Data Frame, der Werte aus analyseData() fuer alle Subjects enthaelt
# #
# createAnalyseDataFrame <- function(){
#   
#   subjectNumbers = getSubjectNumbers()                                      
#   analyseDataMatrix = NULL
#   subjectNames = NULL
#   
#   for(number in subjectNumbers){
#     print(paste("Processing subject", number))
#     analyseDataMatrix = rbind(analyseDataMatrix, analyseData(paste("/net/store/nbp/projects/EEG/blind_spot/data/horizontal/",number,"/bs_",number,"_hz.mat", sep = "")))
#     subjectNames = c(subjectNames, paste("subject",number, sep = ""))
#   }
#   
#   rownames(analyseDataMatrix) = subjectNames
#   analyseDataFrame = data.frame(analyseDataMatrix)
#   
#   return(analyseDataFrame) 
#   
#   # apply(analyseDataMatrix, MARGIN = 1, FUN = mean)   # Gibt Mittelwerte fuer alle Variablen aus
# }



#   
#   library("ggplot2")
#   if(exists("propDataFrame") == FALSE) {propDataFrame <- createPropDataFrame()}  
#   
#   subjectNumbers = tapply(propDataFrame$subject, propDataFrame$controlTrial, FUN = unique)
#   
#   control0 = data.frame(xc0 = propDataFrame$prop[propDataFrame$stimLLoc == 0 & propDataFrame$stimRLoc == 1 & propDataFrame$controlTrial == 0],
#                         yc0 = (1-propDataFrame$prop[propDataFrame$stimLLoc == 1 & propDataFrame$stimRLoc == 0 & propDataFrame$controlTrial == 0]))
#                         
#   control1 = data.frame(xc1 = propDataFrame$prop[propDataFrame$stimLLoc == 0 & propDataFrame$stimRLoc == 1 & propDataFrame$controlTrial == 1],
#                         yc1 = (1-propDataFrame$prop[propDataFrame$stimLLoc == 1 & propDataFrame$stimRLoc == 0 & propDataFrame$controlTrial == 1]))
#                         
#   control2 = data.frame(xc2 = propDataFrame$prop[propDataFrame$stimLLoc == 0 & propDataFrame$stimRLoc == 1 & propDataFrame$controlTrial == 2],
#                         yc2 = (1-propDataFrame$prop[propDataFrame$stimLLoc == 1 & propDataFrame$stimRLoc == 0 & propDataFrame$controlTrial == 2]))
#                         
#   control3 = data.frame(xc3 = propDataFrame$prop[propDataFrame$stimLLoc == 0 & propDataFrame$stimRLoc == 1 & propDataFrame$controlTrial == 3],
#                         yc3 = (1-propDataFrame$prop[propDataFrame$stimLLoc == 1 & propDataFrame$stimRLoc == 0 & propDataFrame$controlTrial == 3]))
#   
# 
# ggplot() +
#   geom_point(data = control0, aes(x = xc0, y = yc0, colour = "a")) +
#   geom_point(data = control1, aes(x = xc1, y = yc1, colour = "b")) +
#   geom_point(data = control2, aes(x = xc2, y = yc2, colour = "c")) +
#   geom_point(data = control3, aes(x = xc3, y = yc3, colour = "d")) +
#   geom_point(data = control0, aes(x = xc0, y = yc0),colour='lightgray') +
#   geom_point(data = control1, aes(x = xc1, y = yc1),colour='lightgray') +
#   geom_point(data = control2, aes(x = xc2, y = yc2),colour='lightgray') +
#   geom_point(data = control3, aes(x = xc3, y = yc3),colour='lightgray') +
#   labs(x = "Answer when right temporal, left nasal (0 = left, 1 = right)", 
#        y = "Answer when left temporal, right nasal (0 = right, 1 = left)", 
#        title = "Biases") +
#   xlim(0,1) + ylim(0,1) +
#   coord_fixed()+
#   geom_abline(intercept = 1, slope = -1) +
#   geom_abline(intercept = 0, slope = 1) +
#   geom_text(data = control0, aes(x = xc0, y = yc0), color = "#F8766D", label = as.vector(subjectNumbers$'0')) +
#   geom_text(data = control1, aes(x = xc1, y = yc1), color = "#7CAE00", label = as.vector(subjectNumbers$'1')) +
#   geom_text(data = control2, aes(x = xc2, y = yc2), color = "#00BFC4", label = as.vector(subjectNumbers$'2')) +
#   geom_text(data = control3, aes(x = xc3, y = yc3), color = "#C77CFF", label = as.vector(subjectNumbers$'3')) +
#   scale_colour_manual(name = 'Conditions', guide = 'legend',
#                       values =c('a'="#F8766D",'b'="#7CAE00", 'c' = "#00BFC4", 'd' = "#C77CFF"),    # Default colors for red, green, blue, purple
#                       labels = c("Condition 0 (O . . + . . O)","Condition 1 (. O . + . O .)", "Condition 2 (. . O + O . .)", "Condition 3 (. ° . + . ° .)"))


#   library("ggplot2")
#   randomEffects <- ranef(mres)$subject[,1]       # Erstellt Vektor der random Effects
#   barplot(randomEffects)
#   fixedEffects <- fixef(mres)                    # Erstellt Vektor der fixed Effects
#   fixedEffectsDataFrame <- data.frame(fixedEffects)
#   ggplot(fixedEffectsDataFrame) + 
#     geom_bar(aes(x = rownames(fixedEffectsDataFrame), y = fixedEffects), stat = "identity") + 
#     coord_flip() +
#     labs(x = NULL, y = "Log Value", title = "Fixed Effects")