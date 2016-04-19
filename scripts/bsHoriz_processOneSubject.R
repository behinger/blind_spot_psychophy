############################################################################
###                     FUNKTIONEN FUER EIN SUBJECT                      ###
###                    FUER ALLE BLIND SPOT EXPERIMENTE                  ###
############################################################################

#---------------------------------------------------------------------------
#
# Liest eine Matlab Datei ein und speichert die Daten in einem Data Frame
#

loadData <- function (file = file){
  library("R.matlab")
  #  data = readMat("/net/store/nbp/projects/EEG/blind_spot/data/horizontal/04/bs_04_hz.mat")   
  data = readMat(file)                                                     # Liest das uebergebene Matlab-File ein
  names = attr(data$bs.result,"dimnames")[[1]]                              # Speichert Variablennamen in einem Vektor ab
  drawStim = which(names == "drawStim")
  data = data$bs.result
  if (length(drawStim)!=0){
    names = names[-drawStim]                                                # Loescht den Namen einer Variable, die nicht benoetigt wird
    data = data[-drawStim]                                                  # Loescht die nicht benoetigte Variable aus den Daten
  } else {
    data = data[,,1]
  }
  m = NULL                                                                  # Legt eine Variable m an (ist spaeter eine Matrix)
  for(variable in data){                                                    # Geht durch alle Variablen in den Daten
    if(length(variable) != length(data[[1]])) {variable = t(c(variable[1:length(variable)],rep(NaN,(length(data[[1]])-length(variable)))))}     
    if(length(data[[1]]) == 362){variable = t(variable[1:360])}             # Bei Subject r50 (ist r01) gibt es 362 Trials, loesche die letzten zwei
    m = cbind(m, t(variable))}                                              # Speichert Werte der Variablen als Spaltenvektoren in Matrix
  returnDataFrame = data.frame(m)                                           # Erstellt Data Frame aus Matrix
  colnames(returnDataFrame) = names                                         # Setzt Variablennamen aus Namensvektor als Spaltennamen
  return(returnDataFrame)
}


#---------------------------------------------------------------------------
#
# Berechnet Position (Temporal/Nasal), Textur (Continuous/Perpendicular) und Sichtbarkeit (bei Perpendicular) der Stimuli
#
processStimuli <- function (file = file, experiment = experiment){
  
  dataset = loadData(file)
  
  trials = length(dataset$trial)                                            # Anzahl der Trials 
  stimLLoc = matrix(NaN, trials, 1)                                         # Erstelle Spaltenvektoren
  stimRLoc = matrix(NaN, trials, 1)
  stimLCP = matrix(NaN, trials, 1)                                  
  stimRCP = matrix(NaN, trials, 1)
  stimPVisible = matrix(NaN, trials, 1)
  stimOrient = matrix(NaN, trials, 1)
  oneBack = matrix(NaN, trials, 1)  
  correct = matrix(NaN, trials, 1)
  
  # Beschreibt fuer jeden Trial, ob die (Aussenraender der) Stimuli vertikal (0) oder horizontal (1) waren (urspruenglich mit 1 und 2 codiert)
  stimOrient = dataset$oStimL - 1
  
  if(identical(experiment, "EEG") || identical(experiment, "Replication")){
    controlTrial = matrix(rep(1, trials), trials, 1)
  } else{
    controlTrial = matrix(dataset$controlTrial, trials, 1)
  }
  
  #Setze answer-Werte auf 0 (links) und 1 (rechts)
  dataset$answer = dataset$answer - 1
  
  # OneBack Variable: Beschreibt fuer jeden Trial, was die Antwort des Vorgaenger-Trials war
  # Fuer den ersten Trial wird die Antwort des letzten gueltigen Trials genommen
  lastAnswer = NaN
  k = 0
  while (identical(lastAnswer,NaN)){
    lastAnswer = dataset$answer[length(dataset$answer)-k]
    k = k + 1
  }
  
  for(i in 1:trials){
    
    #Aendere Codierung der Stimulusposition (1 = ueber Blindspot wird zu 3; 0 = Blindspot Position wird zu 1)
    if(identical(experiment,"Control")){
      if(controlTrial[i] == 1) {controlTrial[i] = 3}
      else if(controlTrial[i] == 0) {controlTrial[i] = 1}
    }
    
    #Berechne aus LocCond (1, 2, 3 oder 4), ob der Stimulus nasal (0) oder temporal (1) angezeigt wird
    if(identical(dataset$locCond[i],1)){stimLLoc[i,] = 0                            # LocCond = 1 --> beide Stimuli nasal, in jedem Auge einer          
                                        stimRLoc[i,] = 0}
    else if(identical(dataset$locCond[i],2)){stimLLoc[i,] = 1                       # LocCond = 2 --> beide Stimuli temporal, in jedem Auge einer
                                             stimRLoc[i,] = 1}
    else if(identical(dataset$locCond[i],3)){stimLLoc[i,] = 1                       # LocCond = 3 --> linker Stimulus temporal, rechter nasal, beide im linken Auge
                                             stimRLoc[i,] = 0}
    else if(identical(dataset$locCond[i],4)){stimLLoc[i,] = 0                       # LocCond = 4 --> linker Stimulus nasal, rechter temporal, beide im rechten Auge
                                             stimRLoc[i,] = 1}
    
    #Berechne aus der Ausrichtung des inneren und aeusseren Teils des Stimulus, ob dieser continuous (0) oder perpendicular (1) ist
    if(identical(dataset$iStimL[i],dataset$oStimL[i])){stimLCP[i,] = 0}             # Ausrichtung innen und aussen gleich = continuous
    else {stimLCP[i,] = 1}                                                          # Ausrichtung innen und aussen unterschiedlich = perpendicular
    if(identical(dataset$iStimR[i],dataset$oStimR[i])){stimRCP[i,] = 0}
    else {stimRCP[i,] = 1}
    
    #Berechne mithilfe von Stimulus Location, ob ein perpendicular Stimulus unsichtbar (1) oder sichtbar (2) ist (oder 0 wenn continuous)
    if((stimLCP[i,] == 1 && controlTrial[i] == 1 && stimLLoc[i,] == 1) ||           # Perpendicular + Blindspot Position + Temporal --> unsichtbar
         (stimRCP[i,] == 1 && controlTrial[i] == 1 && stimRLoc[i,] == 1)) {stimPVisible[i,] = 1}
    else if(stimLCP[i,] == 1 || stimRCP[i,] == 1) {stimPVisible[i,] = 2}            # Perpendicular, aber nicht Blindspot Position oder Temporal --> sichtbar
    else {stimPVisible[i,] = 0}  
    
    if(!identical(dataset$answer[i],NaN)){
      if((stimPVisible[i,] == 2 && stimLCP[i,] == 1 && dataset$answer[i] == 1) ||     # Perpendicular links sichtbar, richtige Antwort muss rechts sein
           (stimPVisible[i,] == 2 && stimRCP[i,] == 1 && dataset$answer[i] == 0) ||     # Perpendicular rechts sichtbar, richtige Antwort muss links sein
           stimPVisible[i,] != 2) {correct[i,] = 1}                                    # Ohne sichtbaren Perpendicular sind beide Antworten richtig
      else {correct[i,] = 0}
    }
    
    #Bestimme fuer jeden Trial die Antwort des vorherigen Trials
    oneBack[i,] = lastAnswer                                                        # Setze oneBack auf die Antwort des vorherigen Trials
    if(!identical(dataset$answer[i],NaN)) lastAnswer = dataset$answer[i] # Wurde der vorherige Trial abgebrochen, setze oneBack auf den Wert der letzten gueltigen Antwort
    
  }
  
  dataFrame = data.frame(dataset[1], stimLLoc, stimRLoc, stimLCP, stimRCP, stimPVisible, stimOrient, dataset[7:16], oneBack, correct, rt = dataset$rt,  trialTime = dataset$trialTime, controlTrial)
  return(dataFrame)
}


#---------------------------------------------------------------------------
#
# Bestimmt, ob Stimulus temporal oder nasal war und ermittelt, ob er richtig eingestuft wurde
#
processSingleStimuli <- function(file = file){
  
  dataset = loadData(file)
  
  trials = length(dataset$trial)                                            # Anzahl der Trials 
  stimCP = matrix(NaN, trials, 1)                                  
  correct = matrix(NaN, trials, 1)
  
  dataset$answer = dataset$answer - 1
  
  for(i in 1:trials){
    
    #Berechne aus der Ausrichtung des inneren und aeusseren Teils des Stimulus, ob dieser continuous (0) oder perpendicular (1) ist
    if(identical(dataset$iStim[i],dataset$oStim[i])){stimCP[i,] = 0}             # Ausrichtung innen und aussen gleich = continuous
    else {stimCP[i,] = 1}                                                        # Ausrichtung innen und aussen unterschiedlich = perpendicular
    
    if(!identical(dataset$answer[i],NaN)){
      if(dataset$locCond[i] == 1 && stimCP[i,] == 0 && dataset$answer[i] == 0) {correct[i,] = 1}
      else if(dataset$locCond[i] == 1 && stimCP[i,] == 1 && dataset$answer[i] == 0) {correct[i,] = 1}
      else if(dataset$locCond[i] == 2 && stimCP[i,] == 0 && dataset$answer[i] == 0) {correct[i,] = 1}
      else if(dataset$locCond[i] == 2 && stimCP[i,] == 1 && dataset$answer[i] == 1) {correct[i,] = 1}
      else if(dataset$locCond[i] == 3 && stimCP[i,] == 0 && dataset$answer[i] == 0) {correct[i,] = 1}
      else if(dataset$locCond[i] == 3 && stimCP[i,] == 1 && dataset$answer[i] == 1) {correct[i,] = 1}
      else if(dataset$locCond[i] == 4 && stimCP[i,] == 0 && dataset$answer[i] == 0) {correct[i,] = 1}
      else if(dataset$locCond[i] == 4 && stimCP[i,] == 1 && dataset$answer[i] == 0) {correct[i,] = 1}
      else {correct[i,] = 0}
    }
  }
  dataFrame = data.frame(dataset[1:2], stimCP, correct, dataset[5:17])
  
  return(dataFrame)
}
