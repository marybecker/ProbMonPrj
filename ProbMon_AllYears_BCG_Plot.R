library(ggplot2)

assessment <- read.csv('cat_ests_bcg.csv', header = TRUE)

assessment <- assessment[assessment$Category %in% c("BCG2", "BCG34", "BCG56"), ]


AssessmentPlotBCG <- ggplot(assessment,aes(x=Subpopulation,y=Estimate.P,fill=Category))+
  geom_bar(position=position_dodge(),stat="identity",colour="black",linewidth=0.3,
           width=0.7)+
  geom_errorbar(aes(ymin=LCB95Pct.P,ymax=UCB95Pct.P),width=0.2,
                position=position_dodge(0.7))+
  lims(y = c(0,100)) +
  scale_fill_manual(values=c("#3182bd","#9ecae1","#deebf7"),
                    label=c("BCG 2", "BCG 3 & 4", "BCG 5 & 6"))+
  labs(x= "Survey",y="Estimated Proportion of Stream Length")+
  theme_bw()+
  theme(legend.position=c(0.15,0.85),legend.title=element_blank(),
        legend.background=element_rect(fill=alpha("transparent",0)))

AssessmentPlotBCG