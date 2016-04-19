############################################################################
###                     FUNKTIONEN FUER ALLE SUBJECTS                    ###
###                    FUER ALLE BLIND SPOT EXPERIMENTE                  ###
############################################################################

source("bsHoriz_processOneSubject.R")
# load("/net/store/nbp/projects/EEG/blind_spot/data/horizontal/AllDataFrame.RDATA")      # Enthaelt alle Daten der Double Experimente
# load("AllDataFrame.RDATA")

# DATEN ####################################################################

#---------------------------------------------------------------------------
#
# Fuegt alle Data Frames von processStimuli zusammen und ergaenzt sie um die Variablen subject und experiment
#
createAllDataFrame <- function(){
  
  allDataFrame = data.frame()
  
  #------------------ EEG
  age = c(22,24,21,21,28,23,20,24,23,20,25,26,18,23,20)
  handedness = c(1,1,1,1,0,1,1,1,1,1,1,1,1,1,1)  
  dominantEye =c(0,1,0,1,0,1,1,0,1,1,0,0,1,1,1)
  eegSubjectDataFrame = data.frame(subject = c('A1','B1','B2','C1','C2','D1a','D1b','D2','E1','E2','G1','G2','H1','H2','J1','J2','K1','K2','L1','L2','M1','M2','N1','N2','T1','T2','U1','U2','V1','V2'), 
                                   subjectFileName = c('a1','b1','b2','c1','c2','D1a','D1b','d5','e2','e5','g2','g5','h2','h5','j2','j5','k2','k5','l2','l5','m2','m6','n1','n6','t2','t6','u2','u6','v1','v6'), 
                                   age = c(age[1], rep(age[2:3], each = 2), rep(age[4], 3), rep(age[5:length(age)], each = 2)), 
                                   handedness = c(handedness[1], rep(handedness[2:3], each = 2), rep(handedness[4], 3), rep(handedness[5:length(handedness)], each = 2)), 
                                   dominantEye = c(dominantEye[1], rep(dominantEye[2:3], each = 2), rep(dominantEye[4], 3), rep(dominantEye[5:length(dominantEye)], each = 2)))
  
  
  for(number in eegSubjectDataFrame$subject){
    print(paste("Processing EEG Experiment Subject", number))
    if(number == 'A1'){
      subject = rep(substring(number, 1, 1), 600)
      age = rep(eegSubjectDataFrame[eegSubjectDataFrame$subject == number,]$age, 600)
      handedness = rep(eegSubjectDataFrame[eegSubjectDataFrame$subject == number,]$handedness, 600)
      dominantEye = rep(eegSubjectDataFrame[eegSubjectDataFrame$subject == number,]$dominantEye, 600)
      experiment = rep("EEG", 600)
    } else if(number == 'D1a'){
      subject = rep(substring(number, 1, 1), 120)
      age = rep(eegSubjectDataFrame[eegSubjectDataFrame$subject == number,]$age, 120)
      handedness = rep(eegSubjectDataFrame[eegSubjectDataFrame$subject == number,]$handedness, 120)
      dominantEye = rep(eegSubjectDataFrame[eegSubjectDataFrame$subject == number,]$dominantEye, 120)
      experiment = rep("EEG", 120)
    } else if(number == 'D1b'){
      subject = rep(substring(number, 1, 1), 240)
      age = rep(eegSubjectDataFrame[eegSubjectDataFrame$subject == number,]$age, 240)
      handedness = rep(eegSubjectDataFrame[eegSubjectDataFrame$subject == number,]$handedness, 240)
      dominantEye = rep(eegSubjectDataFrame[eegSubjectDataFrame$subject == number,]$dominantEye, 240)
      experiment = rep("EEG", 240)     
    } else{
      subject = rep(substring(number, 1, 1), 360)
      age = rep(eegSubjectDataFrame[eegSubjectDataFrame$subject == number,]$age, 360)
      handedness = rep(eegSubjectDataFrame[eegSubjectDataFrame$subject == number,]$handedness, 360)
      dominantEye = rep(eegSubjectDataFrame[eegSubjectDataFrame$subject == number,]$dominantEye, 360)
      experiment = rep("EEG", 360)
    }
    if(number == 'D1a'){
      fileName = "bs_d_2_v_r.mat_error"
    } else if(number == 'D1b'){
      fileName = "bs_d3_vr.mat_error"
    } else{
      fileName = paste("bs_",eegSubjectDataFrame[eegSubjectDataFrame$subject == number,]$subjectFileName,"_vr.mat", sep = "")
    }
    allDataFrame = rbind(allDataFrame, 
                         cbind(processStimuli(paste("/net/store/nbp/projects/EEG/blind_spot/data/VP", substring(number, 1, 2),"/psyphy/", fileName, sep = ""), "EEG"), 
                               subject, age, handedness, dominantEye, experiment))
  }
  
  
  #------------------ REPLICATION
  subjectNumbersReplication = c('01','50','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24','25','26') # alle Subjects
  bad = c('03','06','07','08','09','10','11','12','13','14','15','16','17', '18', '19','20','24')
  age = c(22,24,19,22,21,19,19,23,21,21,25,25,23,26,21,22,NaN,20,22,21,19,20,19,20,19,26)
  handedness = c(1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,0,0,1,1,1,0,0,1,1,1)
  dominantEye =c(0,0,1,1,1,1,1,0,1,1,1,1,0,1,1,1,0,1,1,1,1,1,1,1,1,0)
  replicationSubjectDataFrame <- data.frame(subject = subjectNumbersReplication[-match(bad, subjectNumbersReplication)], 
                                            age = c(age[1],age[-match(bad, subjectNumbersReplication)]),
                                            handedness = c(handedness[1],handedness[-match(bad, subjectNumbersReplication)]), 
                                            dominantEye = c(dominantEye[1],dominantEye[-match(bad, subjectNumbersReplication)]))
  
  
  for(number in replicationSubjectDataFrame$subject){
    
    if(number == '01' || number == '50'){
      print("Processing Replication Experiment Subject R01")
      subject = rep("R01", 360)
      age = rep(replicationSubjectDataFrame[replicationSubjectDataFrame$subject == number,]$age, 360)
      handedness = rep(replicationSubjectDataFrame[replicationSubjectDataFrame$subject == number,]$handedness, 360)
      dominantEye = rep(replicationSubjectDataFrame[replicationSubjectDataFrame$subject == number,]$dominantEye, 360)
      experiment = rep("EEG", 360)
    } else{
      print(paste("Processing Replication Experiment Subject", paste("R",number, sep="")))
      subject = rep(paste("R",number, sep=""), 720)
      age = rep(replicationSubjectDataFrame[replicationSubjectDataFrame$subject == number,]$age, 720)
      handedness = rep(replicationSubjectDataFrame[replicationSubjectDataFrame$subject == number,]$handedness, 720)
      dominantEye = rep(replicationSubjectDataFrame[replicationSubjectDataFrame$subject == number,]$dominantEye, 720)
      experiment = rep("EEG", 720)
    }
    allDataFrame = rbind(allDataFrame, 
                         cbind(processStimuli(paste("/net/store/nbp/projects/EEG/blind_spot/data/psyphy/bs_", number, "_vr.mat", sep = ""), "Replication"), 
                               subject, age, handedness, dominantEye, experiment))
  }
  
  
  #------------------ CONTROL
  subjectNumbersControl = c('c1','c2','c3','c4','c5','c6','c7','c8','c9','d1','d2','d3','d4','e1','e3','e4','e6','e7','e8','e9','f2','f3','f6','g1','h1','h4','h5','i1','i3','i6','i9','j1','j2','j3','j4')                                      
  bad = c(2,6,8,9,10,12,26,28)
  age = c(19,18,20,22,22,20,20,23,22,26,21,19,22,23,24,19,23,22,24,21,24,20,22,25,23,21,23,21,20,21,33,21,24,20,28)
  handedness =c(1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1)
  dominantEye =c(1,1,1,0,1,0,1,0,1,1,0,1,1,1,1,1,1,0,1,0,0,1,0,1,1,1,1,1,1,1,0,0,1,1,1)
  controlSubjectDataFrame = data.frame(subject = subjectNumbersControl[-bad], 
                                       age = age[-bad], 
                                       handedness = handedness[-bad], 
                                       dominantEye = dominantEye[-bad])
  
  
  for(number in controlSubjectDataFrame$subject){
    print(paste("Processing Control Experiment Subject", number))
    subject = rep(number, 720)
    age = rep(controlSubjectDataFrame[controlSubjectDataFrame$subject == number,]$age, 720)
    handedness = rep(controlSubjectDataFrame[controlSubjectDataFrame$subject == number,]$handedness, 720)
    dominantEye = rep(controlSubjectDataFrame[controlSubjectDataFrame$subject == number,]$dominantEye, 720)
    experiment = rep("Control", 720)
    allDataFrame = rbind(allDataFrame, 
                         cbind(processStimuli(paste("/net/store/nbp/projects/EEG/blind_spot/data/psyphycontrol/bs_", number, "_vr.mat", sep = ""), "Control"), 
                               subject, age, handedness, dominantEye, experiment))
  }
  
  
  #------------------ HORIZONTAL
  subjectNumbersHoriz = c("03", "04", "05", "06", "09", "10", "13", "14", "16", "17", "18", "19", "20", "21", "22", "25", "26", "27", "28", "29", "30", "32", "34", "40", "43")                                      
  age = c(23, 23, 22, 23, 20, 21, 21, 22, 23, 22, 20, 19, 25, 22, 20, 23, 20, 27, 21, 23, 19, 19, 19, 22, 26)
  handedness = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1)
  dominantEye = c(1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0)
  horizSubjectDataFrame = data.frame(subject = subjectNumbersHoriz, age, handedness, dominantEye)
  
  
  for(number in horizSubjectDataFrame$subject){
    print(paste("Processing Horizontal Experiment Subject", number))
    subject = rep(number, 720)
    age = rep(horizSubjectDataFrame[horizSubjectDataFrame$subject == number,]$age, 720)
    handedness = rep(horizSubjectDataFrame[horizSubjectDataFrame$subject == number,]$handedness, 720)
    dominantEye = rep(horizSubjectDataFrame[horizSubjectDataFrame$subject == number,]$dominantEye, 720)
    experiment = rep("Horizontal", 720)
    allDataFrame = rbind(allDataFrame, 
                         cbind(processStimuli(paste("/net/store/nbp/projects/EEG/blind_spot/data/horizontal/", number, "/bs_", number, "_hz.mat", sep = ""), "Horizontal"), 
                               subject, age, handedness, dominantEye, experiment))
  }
  
  allDataFrame = allDataFrame[!is.na(allDataFrame$success),]   # Bei Zeile 2400 wurde in processOneSubject eine Dummy-Zeile eingefuegt, da dort ein Trial fehlt. Diese muss nun geloescht werden.
  
  return(allDataFrame) 
}


#---------------------------------------------------------------------------
#
# Gibt Prozentwerte der richtig erkannten Perpendiculars (im Double Experiment) fuer jedes Subject zurueck
#
perpendicularsRecognized <- function(){
  if(!exists("allDataFrame")) {allDataFrame <- createAllDataFrame()}
  
  recognized = tapply(allDataFrame[allDataFrame$stimPVisible == 2,]$correct,                         # Berechnet pro Subject, wie viele Perpendiculars korrekt erkannt wurden
                      allDataFrame[allDataFrame$stimPVisible == 2,]$subject, FUN = sum, na.rm = TRUE)
  perpendicular = tapply(allDataFrame[allDataFrame$stimPVisible == 2,]$stimPVisible,                 # Berechnet pro Subject, wie viele Perpendiculars es gab
                         allDataFrame[allDataFrame$stimPVisible == 2,]$subject, FUN = length)
  perpendicularAborted = tapply(allDataFrame[(allDataFrame$stimPVisible == 2 & allDataFrame$success != 1),]$stimPVisible, # Berechnet pro Subject, wie viele Perpendicular-Trials abgebrochen wurden 
                                allDataFrame[(allDataFrame$stimPVisible == 2 & allDataFrame$success != 1),]$subject,
                                FUN = length)
  perpendicularAborted = replace(perpendicularAborted, is.na(perpendicularAborted), 0)               # Wurde kein Perpendicular-Trial abgebrochen, muss NA im Vektor durch 0 ersetzt werden
  perpendicularUnaborted = perpendicular - perpendicularAborted                                      # Gesamtzahl der Perpendicular-Trials ist die Menge der unabgebrochenen Perpendicular-Trials
  percentageRecognized = round(recognized/perpendicularUnaborted, 2)                                 # Prozentsatz der erkannten Perpendiculars pro Subject
  
  return(percentageRecognized)
}


#---------------------------------------------------------------------------
#
# Erstellt einen Data Frame, der die Haeufigkeiten der moeglichen Kombinationen von stimLLoc, stimRLoc, controlTrial und subject enthaelt
#
createProbDataFrame <- function(){
  
  library("plyr")
  if(!exists("allDataFrame")) {allDataFrame <- createAllDataFrame()}
  
  subsetAllDataFrame = subset(allDataFrame, (allDataFrame$success == 1 & allDataFrame$stimPVisible != 2))     # Behalte nur Trials, die nicht abgebrochen wurden und die keine sichtbaren Perpendicular enthalten
  smallAllDataFrame = subsetAllDataFrame[c("stimLLoc", "stimRLoc", "controlTrial", "subject", "answer")]      # Erstelle neuen Data Frame, der nur die relevanten Variablen enthaelt
  probDataFrame = ddply(smallAllDataFrame, c("stimLLoc", "stimRLoc", "controlTrial", "subject"), summarise, prob=mean(answer))  # Berechne die Haeufigkeiten der moeglichen Kombinationen 
  
  return(probDataFrame)
}




# PLOTS ####################################################################

#---------------------------------------------------------------------------
#
# Erstellt einen Scatterplot, der rechts temporal (x) gegen links temporal (y) plottet
#
createScatterPlot <- function(){
 
  library("ggplot2")
  if(!exists("probDataFrame")) {probDataFrame <- createProbDataFrame()} 
  
  controlsDataFrame = data.frame()
  subjectNumbers = tapply(probDataFrame$subject, probDataFrame$controlTrial, FUN = unique)
  
  for(i in 0:3){
    x = probDataFrame$prob[probDataFrame$stimLLoc == 0 & probDataFrame$stimRLoc == 1 & probDataFrame$controlTrial == i]
    y = (1-probDataFrame$prob[probDataFrame$stimLLoc == 1 & probDataFrame$stimRLoc == 0 & probDataFrame$controlTrial == i])
    subject = as.vector(subjectNumbers[[as.character(i)]])
    controlTrial = rep(i, length(x))
    controlsDataFrame = rbind(controlsDataFrame, data.frame(x, y, subject, controlTrial))
  }
  
  ggplot(controlsDataFrame) +
    geom_abline(intercept = 100, slope = -1, linetype="longdash") +
    geom_abline(intercept = 0, slope = 1, linetype="dotted") +
    geom_text(aes(x = x*100, y = y*100, label = subject, colour = factor(controlTrial)), size = 6) +
    labs(x = "Choosing Temporal [%] \n Right Temporal, Left Nasal", 
         y = "Choosing Temporal [%] \n Left Temporal, Right Nasal") +
    theme(text = element_text(size = 25)) +
    xlim(0,100) + ylim(0,100) +
    coord_fixed() +
    scale_colour_manual(name = "Stimulus Position", breaks = c("0", "1", "2", "3"), values = c("#F8766D","#7CAE00", "#00BFC4", "#C77CFF"),
                        labels = c("Outward (O . . + . . O)","Blind Spot (. O . + . O .)", "Inward (. . O + O . .)", "Above (. ° . + . ° .)") ) +
    theme(legend.position = "none")                                         # Legende kann bei Bedarf wieder eingefügt werden

}


#---------------------------------------------------------------------------
#
# Erstellt Boxplots, die den Bias der einzelnen Conditions zeigen
#
createBoxPlot <- function(){
  
  library("ggplot2")
  if(!exists("probDataFrame")) {probDataFrame <- createProbDataFrame()} 

  ggplot(probDataFrame, aes(x = factor(2*stimRLoc+stimLLoc), y = prob*100, fill = factor(controlTrial))) + geom_boxplot() +  # 2*stimLLoc+stimRLoc erzeugt Werte 0, 1, 2, 3 fuer die einzelnen Kombinationen von stimLLoc und stimRLoc
    geom_abline(intercept = 50, slope = 0) +                               # Zeichnet Linie durch Chance-Level (50%)
    facet_grid(controlTrial ~ .) +                                         # Plottet drei Plots fuer die verschiedenen Controls                                                       
    geom_jitter(position = position_jitter(width = .1)) +                  # Fuegt Punkte fuer die einzelnen Subjectwerte hinzu
    scale_x_discrete(breaks = 0:3, labels=c("Both Temporal", "Left Temporal \n Right Nasal", "Right Temporal \n Left Nasal", "Both Nasal")) +
    labs(x =NULL, y = "Answer 'right' [%]") +  
    theme(text = element_text(size = 20)) + 
    coord_cartesian(ylim = c(-5, 105)) +
    scale_fill_discrete(name = "Stimulus Position", breaks = c("0", "1", "2", "3"), # Aendert Beschriftung der Legende
                        labels = c("Outward (O . . + . . O)","Blind Spot (. O . + . O .)", "Inward (. . O + O . .)", "Above (. Â° . + . Â° .)") ) +
  theme(legend.position = "none")                                          # Legende kann bei Bedarf wieder eingefügt werden
}


#---------------------------------------------------------------------------
#
# Erstellt Boxplots, bei denen der Bias mit den Mittelwerten der einzelnen Subjects normalisiert wurde
#
createBoxPlotNormalized <- function(){
  
  library("ggplot2")
  if(!exists("probDataFrame")) {probDataFrame <- createProbDataFrame()} 
#   probDataFrame = adply(probDataFrame,1,function(x){allDataFrame$dominantEye[which(x$subject[1] == allDataFrame$subject)[1]]})   # Ermittelt Augendominanz der Subjects
#   probDataFrame = probDataFrame[probDataFrame$V1==1,]                                                                            # Nur rechtsdominante Subjects angucken
  
  subjectNumbers = as.vector(unique(probDataFrame$subject))
  means = tapply(probDataFrame$prob,probDataFrame$subject,FUN=mean)         # Errechnet Mittelwerte von Answer der einzelnen Subjects
  probMinusMean = NULL
  
  for(subject in subjectNumbers){                                           # Zieht bei jeder Antworthaeufigkeit des Subjects seinen Mean ab (normalisiert Bias)
    probMinusMean[probDataFrame$subject == subject] = probDataFrame$prob[probDataFrame$subject == subject] - means[match(subject, names(means))]
  }
  probDataFrame = cbind(probDataFrame, probMinusMean)                       # Fuegt probMinusMean dem Data Frame hinzu
  
  ggplot(probDataFrame, aes(x = factor(2*stimRLoc+stimLLoc), y = probMinusMean*100, fill = factor(controlTrial))) + geom_boxplot() + 
    geom_abline(intercept = 0, slope = 0) +                                 # Zeichnet Linie durch Chance-Level (0%)
    facet_grid(controlTrial ~ .) +                                          # Plottet drei Plots fuer die verschiedenen Controls
    geom_jitter(position = position_jitter(width = .1)) +                   # Fuegt Punkte fuer die einzelnen Subjectwerte hinzu
    scale_x_discrete(breaks = 0:3, labels=c("Both Temporal", "Left Temporal \n Right Nasal", "Right Temporal \n Left Nasal", "Both Nasal")) +
    labs(x =NULL, y = "Answer 'right' normalized [%]") +
    theme(text = element_text(size = 20)) + 
    coord_cartesian(ylim = c(-55, 55)) +
    scale_fill_discrete(name = "Stimulus Position", breaks = c("0", "1", "2", "3"), # Aendert Beschriftung der Legende
                        labels = c("Outward (O . . + . . O)","Blind Spot (. O . + . O .)", "Inward (. . O + O . .)", "Above (. ° . + . ° .)") ) +
    theme(legend.position = "none")                                         # Legende kann bei Bedarf wieder eingefügt werden
}


#---------------------------------------------------------------------------
#
# Erstellt einen Plot, der die Blind Spot Positionen der Subjects in Degree of Visual Angle anzeigt
#
deg2px = function(x,d=600) { tan(x/180*pi)*d/0.276}    # Rechnet Grad in Pixel um (d beschreibt den Bildschirmabstand in mm)
px2deg = function(x,d=600) { atan2(x*0.276,d)*180/pi}  # Rechnet Pixel in Grad um (d beschreibt den Bildschirmabstand in mm)
#
createBlindSpotPlot <- function(){
  
  library("ggplot2")
  if(!exists("allDataFrame")) {allDataFrame <- createAllDataFrame()} 
  
  subsetAllDataFrame = subset(allDataFrame, (allDataFrame$success == 1))
  
  # Beim Horizontal Experiment war der Bildschirmabstand 50cm, bei den EEG, Replication und Control Experimenten war der Bildschirmabstand 60cm
  bsXmeanLeft  = c(px2deg(tapply(subsetAllDataFrame[subsetAllDataFrame$experiment == "Horizontal",]$bsXL, as.vector(subsetAllDataFrame[subsetAllDataFrame$experiment == "Horizontal",]$subject), FUN = mean, na.rm = TRUE), d = 500), 
                   px2deg(tapply(subsetAllDataFrame[subsetAllDataFrame$experiment != "Horizontal",]$bsXL, as.vector(subsetAllDataFrame[subsetAllDataFrame$experiment != "Horizontal",]$subject), FUN = mean, na.rm = TRUE), d = 600))
  bsXmeanRight = c(px2deg(tapply(subsetAllDataFrame[subsetAllDataFrame$experiment == "Horizontal",]$bsXR, as.vector(subsetAllDataFrame[subsetAllDataFrame$experiment == "Horizontal",]$subject), FUN = mean, na.rm = TRUE), d = 500),
                   px2deg(tapply(subsetAllDataFrame[subsetAllDataFrame$experiment != "Horizontal",]$bsXR, as.vector(subsetAllDataFrame[subsetAllDataFrame$experiment != "Horizontal",]$subject), FUN = mean, na.rm = TRUE), d = 600))
  
  bsYmeanLeft  = c(px2deg(tapply(subsetAllDataFrame[subsetAllDataFrame$experiment == "Horizontal",]$bsYL, as.vector(subsetAllDataFrame[subsetAllDataFrame$experiment == "Horizontal",]$subject), FUN = mean, na.rm = TRUE), d = 500), 
                   px2deg(tapply(subsetAllDataFrame[subsetAllDataFrame$experiment != "Horizontal",]$bsYL, as.vector(subsetAllDataFrame[subsetAllDataFrame$experiment != "Horizontal",]$subject), FUN = mean, na.rm = TRUE), d = 600)) * -1
  bsYmeanRight = c(px2deg(tapply(subsetAllDataFrame[subsetAllDataFrame$experiment == "Horizontal",]$bsYR, as.vector(subsetAllDataFrame[subsetAllDataFrame$experiment == "Horizontal",]$subject), FUN = mean, na.rm = TRUE), d = 500),
                   px2deg(tapply(subsetAllDataFrame[subsetAllDataFrame$experiment != "Horizontal",]$bsYR, as.vector(subsetAllDataFrame[subsetAllDataFrame$experiment != "Horizontal",]$subject), FUN = mean, na.rm = TRUE), d = 600)) * -1   
  
  bsSmeanLeft  = c(px2deg(tapply(subsetAllDataFrame[subsetAllDataFrame$experiment == "Horizontal",]$bsSL, as.vector(subsetAllDataFrame[subsetAllDataFrame$experiment == "Horizontal",]$subject), FUN = mean, na.rm = TRUE), d = 500), 
                   px2deg(tapply(subsetAllDataFrame[subsetAllDataFrame$experiment != "Horizontal",]$bsSL, as.vector(subsetAllDataFrame[subsetAllDataFrame$experiment != "Horizontal",]$subject), FUN = mean, na.rm = TRUE), d = 600))
  bsSmeanRight = c(px2deg(tapply(subsetAllDataFrame[subsetAllDataFrame$experiment == "Horizontal",]$bsSR, as.vector(subsetAllDataFrame[subsetAllDataFrame$experiment == "Horizontal",]$subject), FUN = mean, na.rm = TRUE), d = 500),
                   px2deg(tapply(subsetAllDataFrame[subsetAllDataFrame$experiment != "Horizontal",]$bsSR, as.vector(subsetAllDataFrame[subsetAllDataFrame$experiment != "Horizontal",]$subject), FUN = mean, na.rm = TRUE), d = 600))    
  
  blindspotDataFrame = data.frame(bsXmeanLeft, bsXmeanRight, bsYmeanLeft, bsYmeanRight, bsSmeanLeft, bsSmeanRight)
  
  drawCircle <- function(xPos, yPos, radius){
    tt <- seq(0,2*pi,length.out = 100)
    xx <- xPos + radius * cos(tt)
    yy <- yPos + radius * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  e = environment()

  plot = ggplot(environment = e) + 
         geom_rect(aes(xmin = -px2deg(960, 600), xmax = px2deg(960, 600), ymin = -px2deg(540, 600), ymax = px2deg(540, 600)), colour = "lightgrey", alpha = 0.2) +                             # Bildschirmdimensionen bei 60cm Bildschirmabstand     
         coord_fixed(xlim = c(-px2deg(960, 500), px2deg(960, 500)), ylim = c(-px2deg(540, 500), px2deg(540, 500)))  +                                                                         # Bildschirmdimensionen bei 50cm Bildschirmabstand
         labs(x = "Horizontal Position [°]", y = "Vertical Position [°]") +
         theme(text = element_text(size = 30)) +
         annotate("point", x = 0, y = 0,  shape = 3, size = 4)   # Zeichnet Fixationskreuz
  for(i in 1:length(bsXmeanLeft)){
    plot = plot + geom_polygon(data = drawCircle(xPos = bsXmeanLeft[i], yPos = bsYmeanLeft[i], radius = bsSmeanLeft[i]), aes(x = x, y = y), alpha = 0.05)
    plot = plot + geom_polygon(data = drawCircle(xPos = bsXmeanRight[i], yPos = bsYmeanRight[i], radius = bsSmeanRight[i]), aes(x = x, y = y), alpha = 0.05)
  }
  plot = plot + geom_polygon(data = drawCircle(xPos = mean(bsXmeanLeft),               # Plotte roten Kreis fuer durchschnittliche Blindspot Position links
                                               yPos = mean(bsYmeanLeft), 
                                               radius = mean(bsSmeanLeft)), 
                             aes(x = x, y = y), fill = "red", alpha = 0.3) +
                geom_polygon(data = drawCircle(xPos = mean(bsXmeanRight),              # Plotte roten Kreis fuer durchschnittliche Blindspot Position rechts 
                                               yPos = mean(bsYmeanRight), 
                                               radius = mean(bsSmeanRight)), 
                             aes(x = x, y = y), fill = "red", alpha = 0.3)
  plot
}




# ANALYSE ##################################################################

# Funktionen werden benoetigt, um die Ergebnisse in mres interpretieren zu koennen
# ohne invlogit gilt, dass negative Werte ein Bias nach links bedeuten, positive ein Bias nach rechts
# nach Umrechnen mit invlogit kriegt man das genaue Bias (0 = links, 1 = rechts)
logit<- function(p){ log(p / (1-p))}
invlogit <- function(x){exp(x)/(1+exp(x))}


#---------------------------------------------------------------------------
#
# Analysiert die Daten und zeigt Effektgroessen an
#
mixedEffectsLogisticRegression <-  function(){
  
  library("lme4")
  if(!exists("allDataFrame")) {allDataFrame <- createAllDataFrame()} 
  
  subsetAllDataFrame = subset(allDataFrame, (allDataFrame$success == 1 & allDataFrame$stimPVisible != 2))     # Behalte nur Trials, die nicht abgebrochen wurden und die keine sichtbaren Perpendicular enthalten
  subsetAllDataFrame$subjExpInter = interaction(subsetAllDataFrame$subject,subsetAllDataFrame$experiment,sep="_")   # subjExpInter beschreibt die Interaktion zwischen Subject und Experiment als neuen Faktor (Subject ist nested in Experiment)
  subsetAllDataFrame = data.frame(lapply(subsetAllDataFrame, as.factor))
  
  # Mixed Effects Logistic Regression
  # Fit a generalized linear mixed model, which incorporates both fixed-effects parameters and random effects in a linear predictor, via maximum likelihood 
  mres = glmer(formula = answer ~ stimLLoc * stimRLoc * controlTrial + dominantEye + oneBack + (1|experiment) + (1|subjExpInter), data = subsetAllDataFrame, family = binomial)
  
  # # Influence versucht herauszufinden, wie gross der Einfluss eines einzelnen Subjects auf das Model ist
  # # Dafuer wird fuer jedes Subject das Model ohne dieses Subject berechnet und verglichen, inwiefern sich die Parameter des Models ohne das eine Subject
  # # von den Parametern des Models mit allen Subjects unterscheiden   
  #   library("influence.ME")
  #   infl = influence(mres,group='subjExpInter',count=TRUE)
  
  
  # # Plottet die Fixed Effects
  #   library("coefplot2")
  #   coefplot2(mres)
  
  return(mres)
}


#---------------------------------------------------------------------------
#
# Erstellt Design Matrix, welche eine Uebersicht aller moeglichen fixed Effects und deren Kombinationen anzeigt
#
plotDesignMatrix <- function(){
  if(!exists("mres")) {mres <- mixedEffectsLogisticRegression()} 
  modelMatrix <- t(model.matrix(mres))
  modelMatrixSorted <- modelMatrix[, do.call("order", split(modelMatrix, row(modelMatrix))), drop=FALSE] 
  image(modelMatrixSorted,  col = c("black", "lightgrey"), axes = FALSE)
  axis(1, at=seq(0,1,length.out=nrow(modelMatrixSorted)), labels = attr(mres@pp$X,'dimnames')[[2]], las = 2, cex.axis=0.7)
  axis(2, at=seq(0,1,by=0.2), labels = seq(0,1,by=0.2), las = 2, cex.axis=0.7)
  par(mar = c(18,4,4,2) + 0.1)  # c(15,4,4,2) zum Speichern als PDF, c(12,4,4,2) zum Speichern als Image, mar aendert den Rahmen des Plots, sonst waeren die Labels nicht sichtbar
}


#---------------------------------------------------------------------------
#
# Zeigt Effektgroessen aller Kontraste an
#
contrast <- function(){
  library("multcomp")
  if(!exists("mres")) {mres <- mixedEffectsLogisticRegression()} 
  
  results = glht(mres,c('((stimLLoc1) - (stimRLoc1))/2==0',
                        '((stimLLoc1 + stimLLoc1:controlTrial1) - (stimRLoc1 + stimRLoc1:controlTrial1))/2==0',
                        '((stimLLoc1 + stimLLoc1:controlTrial2) - (stimRLoc1 + stimRLoc1:controlTrial2))/2==0',
                        '((stimLLoc1 + stimLLoc1:controlTrial3) - (stimRLoc1 + stimRLoc1:controlTrial3))/2==0',
                        '((stimLLoc1:controlTrial1) - (stimRLoc1:controlTrial1))/2==0',
                        '((stimLLoc1:controlTrial1 - stimLLoc1:controlTrial2) - (stimRLoc1:controlTrial1 - stimRLoc1:controlTrial2))/2==0',
                        '((stimLLoc1:controlTrial1 - stimLLoc1:controlTrial3) - (stimRLoc1:controlTrial1 - stimRLoc1:controlTrial3))/2==0',
                        '((stimLLoc1:controlTrial1 - (stimLLoc1:controlTrial2 + stimLLoc1:controlTrial3)/3) - (stimRLoc1:controlTrial1 - (stimRLoc1:controlTrial2 + stimRLoc1:controlTrial3)/3))/2==0'))
  attr(results$linfct,'dimnames')[[1]] = c('Control 0','Control 1','Control 2','Control 3','Control 1 vs Control 0', 'Control 1 vs Control 2', 'Control 1 vs Control 3', 'Control 1 vs Control 0, 2 & 3')
  results = summary(results,test=adjusted(type="bonferroni"))

  return(results)
}


#---------------------------------------------------------------------------
#
# Erstellt einen Plot der Effektgroessen der berechneten Kontraste
#
plotContrasts <- function(){
  library("ggplot2")
  if(!exists("results")) {results <- contrast()} 
  
  resultsDataFrame = data.frame(controlName = c("Outward", "Blind Spot", "Above", "Inward"), #attr(results$linfct,'dimnames')[[1]][1:4], 
                                controlEstimate = - results$test$coefficients[c(1,2,4,3)], 
                                minLimit = - confint(results,calpha=univariate_calpha())$confint[,"upr"][c(1,2,4,3)], 
                                maxLimit = - confint(results,calpha=univariate_calpha())$confint[,"lwr"][c(1,2,4,3)],
                                minLimitBonf = - confint(results,calpha=-qnorm(0.05/8/2))$confint[,"upr"][c(1,2,4,3)], 
                                maxLimitBonf = - confint(results,calpha=-qnorm(0.05/8/2))$confint[,"lwr"][c(1,2,4,3)])
  resultsDataFrame = data.frame(controlName=resultsDataFrame$controlName,lapply(resultsDataFrame[,2:6],function(x){100*invlogit(x)-50}))
  
  pathHeight = 1     # Distance from upper end of bonferroni confidence interval to path
  pathEnd = 0.3      # Downwards movement of path ends
  textHeight = 0.2   # Distance from upper end of bonferroni confidence interval to text
  .e = environment()
  
  ggplot(resultsDataFrame, environment = .e) +
    geom_point(aes(x = controlName[c(3,2,4,1)], y = controlEstimate), size = 4, colour = c("#F8766D", "#7CAE00", "#C77CFF", "#00BFC4")) +
    geom_errorbar(aes(x = controlName[c(3,2,4,1)], ymin = minLimitBonf, ymax = maxLimitBonf), size = 0.75, width = 0.15, 
                  colour = c("#F8766D", "#7CAE00", "#C77CFF", "#00BFC4"), alpha = 0.5) +
    geom_errorbar(aes(x = controlName[c(3,2,4,1)], ymin = minLimit, ymax = maxLimit), size = 1, width = 0.4, 
                  colour = c("#F8766D","#7CAE00", "#C77CFF", "#00BFC4")) +
    scale_x_discrete(label = resultsDataFrame$controlName) +
    labs(x = NULL, y = "Temporal Over Nasal Preference [%]") +
    theme(text = element_text(size = 25)) +
    geom_abline(intercept = 0, slope = 0) +
    geom_path(aes(x = c(1, 1, 2-0.02, 2-0.02), y = c(maxLimitBonf[2]+pathHeight-pathEnd, maxLimitBonf[2]+pathHeight, maxLimitBonf[2]+pathHeight, maxLimitBonf[2]+pathHeight-pathEnd)), size = 1) +
    geom_path(aes(x = c(2+0.02, 2+0.02, 3, 3), y = c(maxLimitBonf[2]+pathHeight-pathEnd, maxLimitBonf[2]+pathHeight, maxLimitBonf[2]+pathHeight, maxLimitBonf[2]+pathHeight-pathEnd)), size = 1) +
    geom_path(aes(x = c(2, 2, 4, 4), y = c(maxLimitBonf[2]+pathHeight*2-pathEnd, maxLimitBonf[2]+pathHeight*2, maxLimitBonf[2]+pathHeight*2, maxLimitBonf[2]+pathHeight*2-pathEnd)), size = 1) +
    annotate("text", x= 1, y= resultsDataFrame$maxLimitBonf[1] + textHeight, label="***", size = 10) +
    annotate("text", x= 2, y= resultsDataFrame$maxLimitBonf[2] + textHeight, label="***", size = 10) +
    annotate("text", x= 3, y= resultsDataFrame$maxLimitBonf[3] + textHeight, label="***", size = 10) +
    annotate("text", x= 4, y= resultsDataFrame$maxLimitBonf[4] + textHeight*2, label="n. s.", size = 6) +
    annotate("text", x=(1 + 2)/2, y= resultsDataFrame$maxLimitBonf[2]+pathHeight+textHeight, label="***", size = 10) +
    annotate("text", x=(2 + 3)/2, y= resultsDataFrame$maxLimitBonf[2]+pathHeight+textHeight, label="***", size = 10) +
    annotate("text", x=(2 + 4)/2, y= resultsDataFrame$maxLimitBonf[2]+pathHeight*2+textHeight, label="***", size = 10)
}


# #---------------------------------------------------------------------------
# #
# # Zeigt den Effekt des Faktors Dominant Eye
# #
# plotDominantEyeEffects <- function(){
#   if(!exists("subsetAllDataFrame")) { 
#   subsetAllDataFrame = subset(allDataFrame, (allDataFrame$success == 1 & allDataFrame$stimPVisible != 2))     # Behalte nur Trials, die nicht abgebrochen wurden und die keine sichtbaren Perpendicular enthalten
#   subsetAllDataFrame$subjExpInter = interaction(subsetAllDataFrame$subject,subsetAllDataFrame$experiment,sep="_")
#   subsetAllDataFrame = data.frame(lapply(subsetAllDataFrame, as.factor))}
#   
#   library("effects")
#   dfL = as.data.frame((effect('stimLLoc:dominantEye',mres)))  # Funktioniert nur, wenn stimLLoc:dominantEye so im GLMM spezifiziert wird
#   dfR = as.data.frame((effect('stimRLoc:dominantEye',mres)))
#   
#   library("ggplot2")
#   ggplot() +
#     geom_point(data = dfL, aes(x = -as.numeric(stimLLoc), y = fit, colour = dominantEye), size = 5) +
#     geom_path(data = dfL, aes(x = -as.numeric(stimLLoc), y = fit, colour = dominantEye))+
#     geom_errorbar(data = dfL, aes(x = -as.numeric(stimLLoc), ymin = lower, ymax = upper, colour = dominantEye),alpha=0.3) +
#     geom_point(data = dfR, aes(x = as.numeric(stimRLoc), y = fit, colour = dominantEye), size = 5) +
#     geom_path(data = dfR, aes(x = as.numeric(stimRLoc), y = fit, colour = dominantEye))+
#     geom_errorbar(data = dfR, aes(x = as.numeric(stimRLoc), ymin = lower, ymax = upper, colour = dominantEye),alpha=0.3) +
#     labs(x = NULL, y = "Answer", title = "Effect of Dominant Eye")
# }