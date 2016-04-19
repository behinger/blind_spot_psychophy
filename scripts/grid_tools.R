load_grid_STAN = function(cfg_grid){
  addToFilename = ifelse(assertthat::has_name(cfg_grid,'add_to_filename'),cfg_grid$add_to_filename,'')
  filename = paste0('grid_',addToFilename,'.{20}', cfg_grid$script_name,'Data')
  files = list.files('./',patter=filename)
  
  show(paste0('Loading ',length(files),' files'))
  fitList = NULL
  
  
  for(k in files){
    load(paste0('./',k))
    fitList = c(fitList,fit) 
  }
  return(rstan::sflist2stanfit(fitList))
  
}

run_on_grid = function(cfg_grid){
  cmd=paste0('qsub -cwd -t ',"1:5",
             ' -o ', cfg_grid$gridOutputPath, '/ -e ', cfg_grid$gridOutputPath, '/',
             ' -l ', cfg_grid$requirements, 
             ' -N ', cfg_grid$script_name, 
             ' -pe default ', 1,
             ' -q nbp.q ',cfg_grid$script_name)
  system(cmd)
  
}