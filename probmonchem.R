##Set Working Directory##
setwd("/home/mbecker/Documents/Projects/2017/ProbMon/010617")

##Load Libraries Needed for Session##
library(NADA)  
library(ggplot2)
library(grid)
library(gridExtra)
library(plyr)
library(reshape2)

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

##Read in Data##
PMC<- read.table("data/ProbMonChem_SatValue.csv",header=TRUE,sep=",")

##Check First 10 Rows of Data and Get Names of Columns##
PMC[1:10,]
names(PMC)

##Summary statistics for all parameters and save in external table##
sum_PMC<- summary(PMC)
write.table(sum_PMC,"ChemistrySummary011117.csv",sep=",",row.names=FALSE)

##Subset Data where Needed##
PMC<- PMC[c(1:2,5:6,10:13)]#Subset data to only include variables without a lot of non-detects
PMC<-na.omit(PMC)#Omit rows with NAs
PMC_ref<-PMC[which(PMC$REF==1),]# ref sites when bettered IDed try again

#Calculate the median for all variables
m<- ddply(PMC,.(GRP),numcolwise(median))

#loop here with loop counter i to do up to six plots at a time
j <- 1 #to name the number of multiplots
n <- dim(PMC)[2]-2#the total individual plots

for(i in 1:n){
  
  plot_name <- paste('ProbMonChem',j,'.pdf',sep='')
  
  col<- PMC[,i+2]
  med<- m[,i+1]
  
  p1  <- ggplot(PMC,aes(x=GRP,y=col,fill=GRP))+
    geom_boxplot()+
    scale_y_log10()+
    geom_text(data=m,aes(x=GRP,y=med),label=round(med,
                                                  3),size=3,vjust=-1)+
    labs(title=colnames(PMC)[i+2])+
    theme(legend.position="none",axis.title.x=element_blank(),
          axis.title.y=element_blank())
  
  p2<- ggplot(PMC,aes(col,colour=GRP))+
    stat_ecdf(geom="step")+
    scale_x_log10()+
    labs(y="Cumulative Distribution")+
    theme(legend.position=c(0.2,0.85),legend.title=element_blank(),
          axis.title.x=element_blank())
  
  cairo_pdf(file=plot_name,width=8,height=4)
  multiplot(p1,p2,cols=2)
  dev.off()
  j <- j+1
}