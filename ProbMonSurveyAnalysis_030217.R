setwd("/Volumes/LINUX/ProbMon/022417")
library(spsurvey)
library(ggplot2)
library(grid)
library(gridExtra)
library(plyr)
library(reshape2)

# Read data
data <- read.table("ProbMonDesign_2001_2015_022417.csv",sep=",",header=TRUE)
dim(data)
data[1:10,]

# look a site evaluation and asssessment status
addmargins(table(data$EVALSTATUS, data$BugAssess))

#Subsample Only 1 Survey
S1data<-data[which(data$SurveyName=="2001-2005"),]
S2data<-data[which(data$SurveyName=="2006-2010"),]
S3data<-data[which(data$SurveyName=="2011-2015"),]


#######Adjust sample weights to account for use of oversample sites and differing GIS stream length#######
#Survey 1
framesize <- c("CT_Streams1"=9931.3570)##Total Design Stream Length KM
S1data$AdjWGT <- adjwgt(rep(TRUE, nrow(S1data)), 
                        S1data$WGT,
                        rep("CT_Streams1", nrow(S1data)),
                        framesize)
sum(S1data$AdjWGT)

#Survey 2
framesize <- c("CT_Streams2"=12508.5465)
S2data$AdjWGT <- adjwgt(rep(TRUE, nrow(S2data)), 
                        S2data$WGT,
                        rep("CT_Streams2", nrow(S2data)),
                        framesize)
sum(S2data$AdjWGT)

#Survey 3
framesize <- c("CT_Streams2"=12508.5465)
S3data$AdjWGT <- adjwgt(rep(TRUE, nrow(S3data)), 
                        S3data$WGT,
                        rep("CT_Streams2", nrow(S3data)),
                        framesize)
sum(S3data$AdjWGT)

Sdata<- rbind(S1data,S2data,S3data)##Combine Surveys Back Together 

############Set up data frames needed for analysis#############

sites<- data.frame(siteID=Sdata$SITEID,Use=Sdata$EVALSTATUS=="CMPLTE")
subpop<- data.frame(siteID=Sdata$SITEID,Survey=Sdata$SurveyName)
design <- data.frame(siteID=Sdata$SITEID,
                     wgt=Sdata$AdjWGT,
                     xcoord=Sdata$XlongDD,
                     ycoord=Sdata$YlatDD)
framesize<- c("2001-2005"=9931.3570,"2006-2010"=12508.5465,"2011-2015"=12508.5465)

######### Estimate assessment decision based on Macro-Invert assessment data (p/f/a)#########

sites.bugs<- data.frame(siteID=Sdata$SITEID, Use=Sdata$BugAssess=="Pass"| Sdata$BugAssess=="Fail"| 
                          Sdata$BugAssess=="Ambiguous")
data.assess <- data.frame(siteID=Sdata$SITEID,Assessment=Sdata$BugAssess)
AssessmentExtent <- cat.analysis(sites.bugs, subpop, design, data.assess,popsize=list
                                 (Survey=as.list(framesize)))
AssessmentExtent
Assessment<-AssessmentExtent[c(1:2,4:6,8:10),]
AddAmbig<- data.frame(Type="Survey",Subpopulation="2001-2005",Indicator="Assessment",
                      Category="Ambiguous",NResp=0,
                      Estimate.P=0,StdError.P=0,LCB95Pct.P=0,UCB95Pct.P=0,Estimate.U=0,
                      StdError.U=0,LCB95Pct.U=0,UCB95Pct.U=0)#Pad Ambig data to give plot equal bar width
Assessment<- rbind(Assessment,AddAmbig)

AssessmentPlot<- ggplot(Assessment,aes(x=Subpopulation,y=Estimate.P,fill=Category))+
                        geom_bar(position=position_dodge(),stat="identity",colour="black",size=0.3,
                                 width=0.7)+
                        geom_errorbar(aes(ymin=LCB95Pct.P,ymax=UCB95Pct.P),width=0.2,
                                      position=position_dodge(0.7))+
                        scale_fill_manual(values=c("grey","darkseagreen","deepskyblue4"))+
                        labs(x= "Survey",y="Estimated Proportion of Stream Length")+
                        theme_bw()+
                        theme(legend.position=c(0.12,0.85),legend.title=element_blank(),
                          legend.background=element_rect(fill=alpha("transparent",0)))

######### Estimate CDF of Chem and Cont Bio Data#########

data.chem <- data.frame(siteID=Sdata$SITEID,CA=Sdata$CA,Chloride=Sdata$Chloride,
                        TP=Sdata$TP,TN=Sdata$TN,TS=Sdata$TS,MMI=Sdata$MMI,DiatomI=Sdata$DiatomI,
                        DiatomH=Sdata$DiatomH,DiatomL=Sdata$DiatomL)
CDFest<- cont.analysis(sites,subpop,design,data.chem,popsize=list(Survey=as.list(framesize)))
CDFTable<- (CDFest$CDF)
CDFPct<- (CDFest$Pct)

CDFTP0.01<- CDFTable[which(CDFTable$Indicator=="TP"& CDFTable$Value==0.01),]
CDFMMI50<- CDFTable[which(CDFTable$Indicator=="MMI"&CDFTable$Value==49.92),]
CDFPctCL<- CDFPct[which(CDFPct$Indicator=="Chloride"&CDFPct$Statistic=="50Pct"),]
CDFPctCA<- CDFPct[which(CDFPct$Indicator=="CA"&CDFPct$Statistic=="50Pct"),]

tiff(file="TPBar.tiff",width=2000,height=1800,res=300)
TPBarPlot<- ggplot(CDFTP0.01,aes(x=Subpopulation,y=Estimate.P,fill=Subpopulation))+
            geom_bar(position=position_dodge(),stat="identity",colour="black",size=0.3,width=0.7)+
            geom_errorbar(aes(ymin=LCB95Pct.P,ymax=UCB95Pct.P),width=0.2,
                position=position_dodge(0.7))+
            scale_fill_manual(values=c("grey","darkseagreen","deepskyblue4"))+
            labs(title="% Stream Length with TP <= 0.01 mg/L")+
            theme_bw()+
            theme(legend.position ="none",axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  text=element_text(size=20,face="bold"))
TPBarPlot
dev.off()

tiff(file="MMIBar.tiff",width=2000,height=1800,res=300)
MMIBarPlot<-  ggplot(CDFMMI50,aes(x=Subpopulation,y=Estimate.P,fill=Subpopulation))+
              geom_bar(position=position_dodge(),stat="identity",colour="black",size=0.3,width=0.7)+
              geom_errorbar(aes(ymin=LCB95Pct.P,ymax=UCB95Pct.P),width=0.2,
                    position=position_dodge(0.7))+
              scale_fill_manual(values=c("grey","deepskyblue4"))+
              labs(title="% Stream Length with MMI < 50")+
              theme_bw()+
              theme(legend.position ="none",axis.title.x=element_blank(),
                    axis.title.y=element_blank(),
                    text=element_text(size=20,face="bold"))
MMIBarPlot
dev.off()

tiff(file="CLBar.tiff",width=2000,height=1800,res=300)
CLBarPlot<- ggplot(CDFPctCL,aes(x=Subpopulation,y=Estimate,fill=Subpopulation))+
            geom_bar(position=position_dodge(),stat="identity",colour="black",size=0.3,width=0.7)+
            geom_errorbar(aes(ymin=LCB95Pct,ymax=UCB95Pct),width=0.2,
                  position=position_dodge(0.7))+
            scale_fill_manual(values=c("grey","darkseagreen","deepskyblue4"))+
            labs(title="50th Percentile Chloride (mg/L)")+
            theme_bw()+
            theme(legend.position ="none",axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  text=element_text(size=20,face="bold"))
CLBarPlot
dev.off()

tiff(file="CABar.tiff",width=2000,height=1800,res=300)
CABarPlot<- ggplot(CDFPctCA,aes(x=Subpopulation,y=Estimate,fill=Subpopulation))+
            geom_bar(position=position_dodge(),stat="identity",colour="black",size=0.3,width=0.7)+
            geom_errorbar(aes(ymin=LCB95Pct,ymax=UCB95Pct),width=0.2,
                position=position_dodge(0.7))+
            scale_fill_manual(values=c("grey","darkseagreen","deepskyblue4"))+
            labs(title=(expression(paste("50th Percentile Calcium (",mu,"g/L)"),sep="")))+
            theme_bw()+
            theme(legend.position ="none",axis.title.x=element_blank(),
                  axis.title.y=element_blank(),text=element_text(size=20,face="bold"))

CABarPlot
dev.off()
######PLOTS################################
##Multiplot Function Used to Create a Series of Different Types of Plots##
#call this with p1,p,2,... cols=4
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
##########CDF and Boxplots of Variables################

ChemVar<- data.frame(Chem=I(c("CA","Chloride","TP","TN","TS")),
                     Name=I(c("Calcium (ug/L)","Chloride (mg/L)",
                              "Total Phosphorus (mg/L)","Total Nitrogen (mg/L)","Total Solids (mg/L)")))

BioVar<- data.frame(Chem=I(c("MMI","DiatomI","DiatomH","DiatomL")),
                    Name=I(c("Macro-Invertebrate Multi-Metric Index","Diatom TP Index","High TP Diatoms",
                             "Low TP Diatoms")),
                    Max=c(100,10,1,1))


#loop here with loop counter i to do up to six plots at a time
j <- 1 #to name the number of multiplots
n<-dim(ChemVar)[1]

for(i in 1:n){

plot_name <- paste('ProbMonChemNoLog',ChemVar[i,1],'.tiff',sep="")  

x<- ChemVar[i,1]
BoxVar<- Sdata[c(1,8,14:29)]
BoxVar<- subset(BoxVar,!is.na(get(x)))
col<- BoxVar[,x]
BoxVarLD<- subset(BoxVar,LeastDisturbed==1)
colLD<- BoxVarLD[,x]

CDFVar<- CDFTable[which(CDFTable$Indicator==x),]

p1  <- ggplot(BoxVar,aes(x=SurveyName,y=col,fill=SurveyName))+
  geom_boxplot(fill=c("grey","darkseagreen","deepskyblue4"))+
  #scale_y_log10()+
  theme_bw()+
  theme(legend.position="none",axis.title.x=element_blank(),
        axis.title.y=element_blank(),text=element_text(size=20,face="bold"))
  # annotate("text",x=0.8,y=(max(col)*0.9),label="1")

# p2<- ggplot(BoxVarLD,aes(x=SurveyName,y=colLD,fill=SurveyName))+
#   geom_boxplot(fill=c("grey","darkseagreen","deepskyblue4"))+
#   scale_y_log10()+
#   theme_bw()+
#   theme(legend.position="none",axis.title.x=element_blank(),axis.title.y=element_blank())+
#   annotate("text",x=1.2,y=(max(col)*0.9),label="Least Disturbed Sites")

p3<- ggplot(CDFVar,aes(Value,Estimate.P,colour=Subpopulation))+
  geom_step()+
  geom_step(data=CDFVar,aes(Value,LCB95Pct.P,colour=Subpopulation),linetype=2,alpha=0.4)+
  geom_step(data=CDFVar,aes(Value,UCB95Pct.P,colour=Subpopulation),linetype=2,alpha=0.4)+
  #scale_x_log10()+
  scale_color_manual(values=c("grey","green","blue"))+
  labs(y="Cumulative Proportion of Stream Length")+
  theme_bw()+
  theme(legend.position=c(0.8,0.2),legend.title=element_blank(),axis.title.x=element_blank(),
        legend.background=element_rect(fill=alpha("transparent",0)),
        text=element_text(size=20,face="bold"))

tiff(file=plot_name,width=3600,height=1800,res=300)
multiplot(p3,p1, cols=2)
dev.off()
j <- j+1
}
      
####CONT BIO PLOTS###################
#loop here with loop counter i to do up to six plots at a time
j <- 1 #to name the number of multiplots
n<-dim(BioVar)[1]

for(i in 1:n){
  
  plot_name <- paste('BioProbMon',BioVar[i,1],'.tiff',sep="")  
  
  x<- BioVar[i,1]
  BoxVar<- Sdata[c(1,8,14:29)]
  BoxVar<- subset(BoxVar,!is.na(get(x)))
  col<- BoxVar[,x]
  BoxVarLD<- subset(BoxVar,LeastDisturbed==1)
  colLD<- BoxVarLD[,x]
  
  CDFVar<- CDFTable[which(CDFTable$Indicator==x),]
  
  p1  <- ggplot(BoxVar,aes(x=SurveyName,y=col,fill=SurveyName))+
    geom_boxplot(fill=c("grey","deepskyblue4"))+
    #scale_y_log10()+
    ylim(0,BioVar[i,3])+
    theme_bw()+
    theme(legend.position="none",axis.title.x=element_blank(),
          axis.title.y=element_blank(),text=element_text(size=20,face="bold"))
  
  # p2<- ggplot(BoxVarLD,aes(x=SurveyName,y=colLD,fill=SurveyName))+
  #   geom_boxplot(fill=c("grey","deepskyblue4"))+
  #   #scale_y_log10()+
  #   ylim(0,BioVar[i,3])+
  #   theme_bw()+
  #   theme(legend.position="none",axis.title.x=element_blank(),axis.title.y=element_blank())+
  #   annotate("text",x=1.2,y=(BioVar[i,3]*0.95),label="Least Disturbed Sites")
  
  p3<- ggplot(CDFVar,aes(Value,Estimate.P,colour=Subpopulation))+
    geom_step()+
    geom_step(data=CDFVar,aes(Value,LCB95Pct.P,colour=Subpopulation),linetype=2,alpha=0.4)+
    geom_step(data=CDFVar,aes(Value,UCB95Pct.P,colour=Subpopulation),linetype=2,alpha=0.4)+
    #scale_x_log10()+
    scale_color_manual(values=c("grey","blue"))+
    labs(y="Cumulative Proportion of Stream Length")+
    theme_bw()+
    theme(legend.position=c(0.8,0.2),legend.title=element_blank(),axis.title.x=element_blank(),
          legend.background=element_rect(fill=alpha("transparent",0)),text=element_text(size=20,face="bold"))
  
  tiff(file=plot_name,width=3600,height=1800,res=300)
  multiplot(p3,p1, cols=2)
  dev.off()
  j <- j+1
}

