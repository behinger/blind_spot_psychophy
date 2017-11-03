
source('/net/store/nbp/projects/EEG/blind_spot/bs_svn/analysis/r-analysis/bsHoriz_processOneSubject.R')

bs_noise_loaddata_pilot = function(){
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
  
  path_to_files = '/net/store/nbp/projects/EEG/blind_spot/data/noise/'
  
  res = NULL
  for(sub in c(1,3,20,22,23,24,27,3,5,10,13,27,29,30,31)){
    res = tryCatch(
    rbind(res,cbind(subject = sub,processStimuli(paste0(path_to_files,sprintf("%02i/bs_%02i_ns.mat",sub,sub)),experiment='EEG')))
    ,error = function(e){
      rbind(res,cbind(subject = sub,processStimuli(paste0(path_to_files,sprintf("%02i/bs_%i_ns.mat",sub,sub)),experiment='EEG')))
    })
  }
  res$experiment = 'noise'
  return(res)
}