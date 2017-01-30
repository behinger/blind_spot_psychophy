source('bs_init')

# Count the number of trials
exp.trial.count = ddply(subset(allDat,allDat$subject %in% c('EEG.A','Control.C6','Horizontal.03','Inset4a.31','Inset4b.31')), # select 1 subject per experiment
          .(experiment,stimLLoc,stimRLoc,stimRCP,stimLCP,controlTrial), 
          summarise,count=length(answer))

exp.trial.count$stimPVisible = 0
exp.trial.count$stimPVisible[with(exp.trial.count,stimLCP == 1 | stimRCP == 1)] = 2
exp.trial.count$stimPVisible[with(exp.trial.count,controlTrial == 1 & ((stimLCP == 1 & stimLLoc==1) | (stimRCP == 1 & stimRLoc == 1)))] = 1

# add the ratio
exp.trial.count = ddply(exp.trial.count,.(experiment,controlTrial),mutate,ratio = count/sum(count))

# this is the actual stimulation, perception is a bit different
p.physical = ggplot(exp.trial.count,aes(x=interaction(stimLLoc,stimRLoc,stimLCP,stimRCP),
                           y=interaction(controlTrial,experiment),
                           label = paste0(sprintf('%i\n%.3g%%',count,ratio*100))))+
  geom_tile(aes(fill=factor(stimPVisible)))+
  geom_text()+tBE()


p = exp.trial.count
    # Add the inset stimuli that cannot be perceived in the BS to the other stimuli
    # One Inset inside the Left BS
    p$count[p$stimLLoc == 1 & p$stimRLoc == 0 & p$stimLCP == 0 & p$stimRCP == 0 & p$controlTrial == 1] = p$count[p$stimLLoc == 1 & p$stimRLoc == 0 & p$stimLCP == 1& p$controlTrial == 1] + p$count[p$stimLLoc == 1 & p$stimRLoc == 0 & p$stimLCP == 0& p$stimRCP == 0 & p$controlTrial == 1]
    # One Inset inside the Right BS
    p$count[p$stimLLoc == 0 & p$stimRLoc == 1 & p$stimLCP == 0 & p$stimRCP == 0 & p$controlTrial == 1] = p$count[p$stimLLoc == 0 & p$stimRLoc == 1 & p$stimRCP == 1& p$controlTrial == 1] + p$count[p$stimLLoc == 0 & p$stimRLoc == 1 & p$stimLCP == 0& p$stimRCP == 0 & p$controlTrial == 1] # Stimulus in Right BS
    
    # One Inset inside the Left  BS, right stimulus also In BS
    p$count[p$stimLLoc == 1 & p$stimRLoc == 1  & p$stimLCP == 0 & p$stimRCP == 0 & p$controlTrial == 1] = c(p$count[p$stimLLoc == 1 & p$stimRLoc == 1 & p$stimLCP == 1& p$controlTrial == 1],0) + p$count[p$stimLLoc == 1 & p$stimRLoc == 1 & p$stimLCP == 0& p$stimRCP == 0 & p$controlTrial == 1] # Stimulus in Left BS
    # One Inset inside the Right BS, left  stimulus also In BS
    p$count[p$stimLLoc == 1 & p$stimRLoc == 1  & p$stimLCP == 0 & p$stimRCP == 0 & p$controlTrial == 1] = c(p$count[p$stimLLoc == 1 & p$stimRLoc == 1 & p$stimRCP == 1& p$controlTrial == 1],0) + p$count[p$stimLLoc == 1 & p$stimRLoc == 1 & p$stimLCP == 0& p$stimRCP == 0 & p$controlTrial == 1] # Stimulus in Right BS

p = subset(p,p$stimPVisible != 1)

p = ddply(p,.(experiment,controlTrial),mutate,ratio = count/sum(count))

p.perceptual = ggplot(p,aes(x=interaction(stimLLoc,stimRLoc,stimLCP,stimRCP),
                           y=interaction(controlTrial,experiment),
                           label = paste0(sprintf('%i\n%.3g%%',count,ratio*100))))+
  geom_tile(aes(fill=factor(stimPVisible)))+
  geom_text()+tBE()

plotDat = rbind(cbind(type = 'physical',exp.trial.count),
      cbind(type = 'perceptual',p))
ggplot(plotDat,aes(x=interaction(stimLLoc,stimRLoc,stimLCP,stimRCP),
                            y=controlTrial,
                            label = paste0(sprintf('%i\n%.3g%%',count,ratio*100))))+
  geom_tile(aes(fill=factor(stimPVisible)))+
  geom_text(lineheight=.8)+tBE() + facet_grid(experiment~type,scales = 'free_y')+
  scale_fill_manual(values = c('#34BFED','#037DA6','#67CC45'),name='Inset Visible',labels = c('No','Filled-In','Yes'))
ggsave('../export/FigureB-balancing.pdf',width = 6,height= 3,scale=3)

