# This figure shows all normalized results (e.g. control locations) that couldn't be included in the main manuscript
source('./revisions_1/bs_init.R')

get_prob_answer = function(dat){ ddply(dat, c("stimLLoc", "stimRLoc", "controlTrial", "subject"), summarise, answer=mean(answer))}

get_prob_correct = function(dat){ ddply(dat, c("stimLLoc", "stimRLoc", "controlTrial", "subject"), summarise, prob=mean(correct))}

        

#plot_data(get_prob_correct(dat.orth))+labs(y='Inset correctly Identified [%]')+geom_abline(intercept = 50, slope = 0) +coord_cartesian(ylim=c(0,110))
x = get_prob_answer(dat.uni)
x$prob = x$answer
x$subject = factor(x$subject)
data_to_plot = normalize_answer(x)
data_to_plot$experiment = factor(dat.uni$experiment[match(data_to_plot$subject,dat.uni$subject)],levels=c("EEG","Control","Horizontal",'Inset4b','Inset4a'),labels =c('Exp.1','Exp.2','Exp.3','Exp.4b','Exp.4a'))
#data_to_plot$experiment =order(data_to_plot$experiment,c(2,1,3))

labels = c("Both Nasal","Left Temporal / Right Nasal ", "Right Temporal / Left Nasal", "Both Temporal")

cbbPalette <- c("#56B4E9","#009E73", "#D55E00" , "#0072B2", "#CC79A7")

data_to_plot$BSeffect = with(data_to_plot,(stimRLoc != stimLLoc) & (controlTrial == 1))
data_to_plot$tempoEffect = with(data_to_plot,stimRLoc != stimLLoc)
#data_to_plot$exp4a = with(data_to_plot,experiment == 'Exp.4a')

ggplot(data_to_plot, aes(x = interaction(controlTrial,experiment,tempoEffect,BSeffect),
                y = prob*100, color=interaction(stimRLoc,stimLLoc),
                shape=experiment,
                fill=factor(controlTrial)))+
  geom_hline(yintercept = 0,alpha=0.3)+
  geom_point(alpha=.3,
             position = position_jitterdodge(jitter.width = .05,jitter.height=0,dodge.width = .5)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "pointrange",position=position_dodge(width=.5),size=.8)+
  coord_cartesian(ylim = c(55, 105))  +tBE(base_family = 'sans',base_size = 16)+
  coord_cartesian(ylim = c(-55,55))+scale_y_continuous(breaks=c(-50,-25,0,25,50)) +theme(axis.text.y = element_blank(),axis.title.y = element_blank(),axis.ticks.y=element_blank())+coord_flip()

ggsave('../export/FigureC-normalized.pdf',width=6,height=4,useDingbats=FALSE,scale=1.8)




ggplot(dat.orth%>%subset(experiment=='Inset4b'), aes(x = interaction(controlTrial,stimLCP,stimRCP),
                         y = answer, color=interaction(stimRLoc,stimLLoc),
                         shape=experiment,
                         fill=factor(controlTrial)))+
  geom_hline(yintercept = 0,alpha=0.3)+
  geom_point(alpha=.3,
             position = position_jitterdodge(jitter.width = .2,jitter.height=.2,dodge.width = .5)) +
  facet_wrap(~subject)