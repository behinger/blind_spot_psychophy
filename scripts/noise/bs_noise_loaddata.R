
source('/net/store/nbp/projects/EEG/blind_spot/bs_svn/analysis/r-analysis/bsHoriz_processOneSubject.R')

bs_noise_loaddata = function(pilot=F){
  loadData <- function(file){
    library(R.matlab)
    data = readMat(file)
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
  
  
  path_to_files = '/net/store/nbp/projects/EEG/blind_spot/data/'
  res = NULL
  if (pilot){
    path_to_files = paste0(path_to_files,'noise_pilots/noise/')
  }else{
    path_to_files = paste0(path_to_files,'noise/')
  }
  
  subjectList =  list.files(path_to_files,pattern='[0-9]{1,3}')
  subjectList = as.numeric(stringr::str_extract(subjectList,'[0-9]{1,3}'))
  for(sub in subjectList){ #101 had only no noise and lots of full noise
    if (pilot & sub %in% c(11,21,101,30)){  
      print('removing pilot subject 11 + 21 (no data), 101 (only full and no noise) and 30 (effect in opposite direction)')
      next
    }
      res = tryCatch(
              rbind(res,cbind(subject = sub,
                              processStimuli(paste0(path_to_files,sprintf("%02i/bs_%02i_ns.mat",sub,sub)),experiment='EEG')))
              ,error = function(e){
                rbind(res,cbind(subject = sub,
                                processStimuli(paste0(path_to_files,sprintf("%02i/bs_%i_ns.mat",sub,sub)),experiment='EEG')))
              })

  }
  res$experiment = 'noise'
  return(res)
}