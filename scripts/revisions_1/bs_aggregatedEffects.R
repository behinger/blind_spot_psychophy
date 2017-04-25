bs_aggregatedEffects = function(dat.uni){
# Take the raw 0 / 1 trials to percentage, and subtract subjectwise the mean
get_prob_answer = function(dat){ ddply(dat, c("stimLLoc", "stimRLoc", "controlTrial", "subject","stimPVisible","stimLCP","stimRCP"), summarise, prob=mean(answer))}
x = get_prob_answer(dat.uni)
#x$prob = x$answer # for function normalize_answer
x$subject = factor(x$subject)
x$experiment = factor(dat.uni$experiment[match(x$subject,dat.uni$subject)],levels=c("EEG","Control","Horizontal","Inset4a","Inset4b"),labels =c('Exp.1','Exp.2','Exp.3','Exp.4a','Exp.4b'))
# We addback the experiment information and rename EEG/Control/Horizontal to Exp1/Exp2/exp3

data_to_plot = normalize_answer(x)
# Get the mean over the conditions (collapsing)
datAgg = ddply(data_to_plot,.(subject,controlTrial,experiment),summarise,(mean(prob[stimLLoc==0&stimRLoc==1])-mean(prob[stimLLoc==1&stimRLoc==0]))/2*100,.drop=T)
colnames(datAgg)=c('subject','controlTrial','experiment','prob')

# Get the difference between the collapsed values
diffAbove = cbind(ddply(subset(datAgg,datAgg$experiment=='Exp.2'),.(subject),function(x){x$prob[x$controlTrial==1] - x$prob[x$controlTrial==3]}),type='difference',experiment='Exp.2')
diffOutward = cbind(ddply(subset(datAgg,datAgg$experiment=='Exp.3'),.(subject),function(x){x$prob[x$controlTrial==1] - x$prob[x$controlTrial==0]}),type='difference',experiment='Exp.3')
diffInward = cbind(ddply(subset(datAgg,datAgg$experiment=='Exp.3'),.(subject),function(x){x$prob[x$controlTrial==1] - x$prob[x$controlTrial==2]}),type='difference',experiment='Exp.3')
diffAbove4b = cbind(ddply(subset(datAgg,datAgg$experiment=='Exp.4b'),.(subject),function(x){x$prob[x$controlTrial==1] - x$prob[x$controlTrial==3]}),type='difference',experiment='Exp.4b')


colnames(diffAbove)[2] = 'prob'
colnames(diffOutward)[2] = 'prob'
colnames(diffInward)[2] = 'prob'
colnames(diffAbove4b)[2] = 'prob'


# For plotting, this gives the position in the plot
diffAbove$controlTrial = 7
diffOutward$controlTrial = 5
diffInward$controlTrial = 6
diffAbove4b$controlTrial = 8

datAgg$controlTrial = as.numeric(datAgg$controlTrial)
# combine the collapsed data with the difference data
datAgg = rbind.fill(cbind(datAgg,type='raw'),diffAbove,diffOutward,diffInward,diffAbove4b)
myMean = function(x,d){return(mean(x[d],trim=0.0))}

# Calc bootstrapped BCA 95%CI of the mean
aggCI = ddply(datAgg,.(controlTrial,experiment),function(x){
  ci = boot.ci(boot(x$prob,myMean,10000),type='bca')
  return(cbind(`hdi0.025`=ci$bca[4],`hdi0.975`=ci$bca[5],median=ci$t0,type=x$type[1]))
})


# We label which are differences and which are collapsed
aggCI$type = factor(aggCI$type,levels=c(1,2),labels=c('raw','difference'))

return(list(datAgg=  datAgg,aggCI = aggCI))
}