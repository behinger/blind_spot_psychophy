load_grid_STAN = function(cfg_grid){
  addToFilename = ifelse(assertthat::has_name(cfg_grid,'add_to_filename'),cfg_grid$add_to_filename,'')
  filename = paste0('grid_',addToFilename,'.{20}', cfg_grid$script_name,'Data')
  files = list.files('../cache/stan/',patter=filename)
  
  show(paste0('Loading ',length(files),' files'))
  fitList = NULL
  
  
  for(k in files){
    load(paste0('../cache/stan/',k))
    fitList = c(fitList,fit) 
  }
  return(rstan::sflist2stanfit(fitList))
  
}

run_on_grid = function(cfg_grid){
  if(is.null(cfg_grid$t)){
    cfg_grid$t = '1:6'
  }
  if(is.null(cfg_grid$parallel)){
    cfg_grid$parallel = '1'
  }
  cmd=paste0('qsub -cwd -t ',cfg_grid$t,
             ' -o ', cfg_grid$gridOutputPath, '/ -e ', cfg_grid$gridOutputPath, '/',
             ' -l ', cfg_grid$requirements, 
             ' -N ', cfg_grid$script_name, 
             ' -pe default ', cfg_grid$parallel,
             ' -q nbp.q ',cfg_grid$script_name)
  system(cmd)
  
}