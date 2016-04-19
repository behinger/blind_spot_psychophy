calc_boot_ci = function(dat){
  mean.fun <- function(dat, idx) mean(dat[idx], na.rm = TRUE)
  
  b = boot::boot(data=dat,statistic = mean.fun,R=1000)
  boot::boot.ci(b,type='bca')$bca[c(4,5)]
}
