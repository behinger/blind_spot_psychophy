
source("bsHoriz_processOneSubject.R")
# load("/net/store/nbp/projects/EEG/blind_spot/data/horizontal/AllDataFrame.RDATA")      # Enthaelt alle Daten der Double Experimente
# load("AllDataFrame.RDATA")

# DATEN ####################################################################

#---------------------------------------------------------------------------
#
# Fuegt alle Data Frames von processStimuli zusammen und ergaenzt sie um die Variablen subject und experiment
#
bs_load_all_data <- function(){
  
  allDataFrame = data.frame()
  library(gdata)
  metaData = read.xls('/net/store/nbp/users/behinger/projects/blindspot/git/data/Subject Overview_v4.xlsx',sheet=1)
  metaEEG = subset(metaData,metaData$Experiment=='EEG')
  metaCon = subset(metaData,metaData$Experiment=='Control')
  metaPsy = subset(metaData,metaData$Experiment=='PsyPhy')
  metaHor = subset(metaData,metaData$Experiment=='Horizontal')
  metaIns = subset(metaData,metaData$Experiment=='Inset')
  

  
  for(number in metaEEG$VP){
    print(paste("Processing EEG Experiment Subject", number))
    
    for(ses in c(1,2)){
      fPath = paste0("/net/store/nbp/projects/EEG/blind_spot/data/VP", paste0(number,ses),"/psyphy/")
      if(!file.exists(fPath)){next}
      if(number == 'A'&ses==2){next}
      if(number == 'D'& ses==1){
        
        fileName = "bs_d_2_v_r.mat_error"
        tmp1 =   processStimuli(paste0(fPath,fileName), "EEG")  
        fileName = "bs_d3_vr.mat_error"
        tmp2 =  processStimuli(paste0(fPath,fileName), "EEG")  
        singleSubData = rbind(tmp1,tmp2)
        
      } else{
        fileName = list.files(fPath)[grep(paste0('bs.',number,'..vr.mat'),list.files(fPath),ignore.case = T)]
        singleSubData =  processStimuli(paste0(fPath,fileName), "EEG")  
      }
      if(ses==2){singleSubData$trial = singleSubData$trial + 360} 
      allDataFrame = rbind(allDataFrame, with(subset(metaEEG,metaEEG$VP == number), 
                                              cbind(singleSubData, subject=VP,age=Alter,sex=Geschlecht,handedness=Handedness,dominantEye=DominantEye, experiment=Experiment,
                                                     remove=remove.,removeReason=Remove.Reas)))
      
    }
  }
  
  for(number in as.numeric(as.character(metaPsy$VP))){
    print(paste("Processing PSYPHY Experiment Subject", number))
    
    
      fPath = paste0("/net/store/nbp/projects/EEG/blind_spot/data/psyphy/")
      fileName = list.files(fPath)[grep(paste0('bs.',sprintf('%02d',number),'.vr.mat'),list.files(fPath),ignore.case = T)]
      if (length(fileName)==0){warning(paste0('subject',number,'not found'))}
        singleSubData =  processStimuli(paste0(fPath,fileName), "Replication")  
      if (number == 1){
        fileName = list.files(fPath)[grep(paste0('bs.',sprintf('%02d',50),'.vr.mat'),list.files(fPath),ignore.case = T)]  
        tmp =  processStimuli(paste0(fPath,fileName), "Replication")  
        singleSubData = rbind(singleSubData,tmp)
      }
      
      allDataFrame = rbind(allDataFrame, with(subset(metaPsy,metaPsy$VP == number), 
                                              cbind(singleSubData, subject=sprintf('R%02d',number),age=Alter,sex=Geschlecht,handedness=Handedness,dominantEye=DominantEye, experiment=Experiment,
                                                    remove=remove.,removeReason=Remove.Reas)))
      
    }
  
  
  #------------------ CONTROL
  
  for(number in as.character(metaCon$VP)){
    print(paste("Processing CONTROL Experiment Subject", number))
    
    
    fPath = paste0("/net/store/nbp/projects/EEG/blind_spot/data/psyphycontrol/")
    fileName = list.files(fPath)[grep(paste0('bs.',number,'.vr.mat'),list.files(fPath),ignore.case = T)]
    if (length(fileName)==0){warning(paste('subject',number,'not found'));next}
    
    singleSubData =  processStimuli(paste0(fPath,fileName), "Control")  

    allDataFrame = rbind(allDataFrame, with(subset(metaCon,metaCon$VP == number), 
                                            cbind(singleSubData, subject=number,age=Alter,sex=Geschlecht,handedness=Handedness,dominantEye=DominantEye, experiment=Experiment,
                                                  remove=remove.,removeReason=Remove.Reas)))
    
  }
  
  #------------------ HORIZONTAL
  
  for(number in as.character(metaHor$VP)){
    print(paste("Processing HORIZ Experiment Subject", number))
    
    
    fPath = paste0("/net/store/nbp/projects/EEG/blind_spot/data/horizontal/")
    folderList = list.files(fPath)
    numStrip = substr(number,4,5)
    fPath = paste0(fPath,folderList[grep(numStrip, folderList)],'/')
    fileName = list.files(fPath)[grep(paste0('bs.',numStrip,'.hz.mat(?!.tmp)'),list.files(fPath),ignore.case = T,perl = T)]
    if (length(fileName)==0){warning(paste('subject',number,'not found'));next}
    
    singleSubData =  processStimuli(paste0(fPath,fileName), "Horizontal")  
    
    allDataFrame = rbind(allDataFrame, with(subset(metaHor,metaHor$VP == number), 
                                            cbind(singleSubData, subject=numStrip,age=Alter,sex=Geschlecht,handedness=Handedness,dominantEye=DominantEye, experiment=Experiment,
                                                  remove=remove.,removeReason=Remove.Reas)))
    
  }
  
  for(number in as.character(metaIns$VP)){
    print(paste("Processing Inset Experiment Subject", number))
    
    
    fPath = paste0("/net/store/nbp/projects/EEG/blind_spot/data/inset/")
    folderList = list.files(fPath)
    
    fPath = paste0(fPath,folderList[grep(number, folderList)],'/')
    fileName = list.files(fPath)[grep(paste0('bs.',number,'.iO.mat(?!.tmp)'),list.files(fPath),ignore.case = T,perl = T)]
    if (length(fileName)==0){warning(paste('subject',number,'not found for insetA'));next}
    
    singleSubData =  processStimuli(paste0(fPath,fileName), "EEG")  
    expName = 'Inset4a'
    allDataFrame = rbind(allDataFrame, with(subset(metaIns,metaIns$VP == number), 
                                            cbind(singleSubData, subject=number,age=Alter,sex=Geschlecht,handedness=Handedness,dominantEye=DominantEye, experiment=expName,
                                                  remove=remove.,removeReason=Remove.Reas)))
    
    fileName = list.files(fPath)[grep(paste0('bs.',number,'.iC.mat(?!.tmp)'),list.files(fPath),ignore.case = T,perl = T)]
    if (length(fileName)==0){warning(paste('subject',number,'not found for insetB'));next}
    
    singleSubData =  processStimuli(paste0(fPath,fileName), "Control")  
    expName = 'Inset4b'
    allDataFrame = rbind(allDataFrame, with(subset(metaIns,metaIns$VP == number), 
                                            cbind(singleSubData, subject=number,age=Alter,sex=Geschlecht,handedness=Handedness,dominantEye=DominantEye, experiment=expName,
                                                  remove=remove.,removeReason=Remove.Reas)))
    
  }
  
  allDataFrame$experiment[allDataFrame$experiment=='PsyPhy'] = 'EEG'
  allDataFrame$experiment = factor(allDataFrame$experiment)
  allDataFrame$subject = interaction(allDataFrame$experiment,allDataFrame$subject)
  
  allDataFrame$handedness = as.numeric(factor(allDataFrame$handedness))-1
  allDataFrame$dominantEye = as.numeric(factor(allDataFrame$dominantEye))-1
  return(allDataFrame) 
}