assessment <- read.csv('ProbMonAssessment.csv', header = TRUE)

assessment <- assessment[assessment$Category %in% c("Pass", "Fail"), ]
  
  
Assessment<-assessment[assessment$Category=='Pass'|assessment$Category=='Fail',]
#AddAmbig<- data.frame(Type="Survey",Subpopulation="2001-2005",Indicator="Assessment",
#Category="Ambiguous",NResp=0,
#Estimate.P=0,StdError.P=0,LCB95Pct.P=0,UCB95Pct.P=0,Estimate.U=0,
#StdError.U=0,LCB95Pct.U=0,UCB95Pct.U=0)#Pad Ambig data to give plot equal bar width
#Assessment<- rbind(Assessment,AddAmbig)

AssessmentPlotMMI<- ggplot(assessment,aes(x=Subpopulation,y=Estimate.P,fill=Category))+
  geom_bar(position=position_dodge(),stat="identity",colour="black",linewidth=0.3,
           width=0.7)+
  geom_errorbar(aes(ymin=LCB95Pct.P,ymax=UCB95Pct.P),width=0.2,
                position=position_dodge(0.7))+
  scale_fill_manual(values=c("#3182bd","#deebf7"))+
  labs(x= "Survey",y="Estimated Proportion of Stream Length")+
  lims(y = c(0,100)) +
  theme_bw()+
  theme(legend.position=c(0.12,0.85),legend.title=element_blank(),
        legend.background=element_rect(fill=alpha("transparent",0)))