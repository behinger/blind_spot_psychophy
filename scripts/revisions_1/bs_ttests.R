source('revisions_1/bs_init.R')

# Collapse over blind spots and calculate mean answer
subjectDiff = ddply(dat.uni,.(subject,experiment,controlTrial),summarise,answerDiff=mean(answer[stimLLoc==0 & stimRLoc==1]) - mean(answer[stimLLoc==1 & stimRLoc==0]),.inform = T)

# remove the experiment with the opposing task
subjectDiff = subset(subjectDiff,subjectDiff$experiment != 'Inset4a')


t.test(subset(subjectDiff,subjectDiff$controlTrial==1)$answerDiff/2) # divided by two, because we want the average effect, not the difference between sides



# Collapse over the controlLocations and calculate pairwise difference
d1 = ddply(subjectDiff,.(subject),summarise,meanOutside = mean(answerDiff[controlTrial!=1]),inside=answerDiff[controlTrial==1])
t.test(d1$inside - d1$meanOutside)
