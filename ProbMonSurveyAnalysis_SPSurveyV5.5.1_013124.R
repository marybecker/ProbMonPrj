##########CT Probabilistic-Based Stream Survey Project#############

library(spsurvey)
library(ggplot2)

### Load data #####################################################

data <- read.csv('data/ProbMonDesign_2001_2020_013124.csv', header = TRUE)

s3 <- data[data$SurveyName == "2011-2015" & data$EVALSTATUS == "CMPLTE",]

a  <- c("Pass","Fail","Ambiguous")
s3 <- s3[s3$BugAssess %in% a ,]

s4 <- data[data$SurveyName == "2016-2020" & data$EVALSTATUS == "CMPLTE",]
s4$WGT <- 141.3091  # adj wgt for total number of samples (e.g. 7772 / 55)

cat_ests <- cat_analysis(s4, siteID = "SITEID",
                         vars = "BugAssess", 
                         weight = "WGT",
                         sizeweight = FALSE,
                         xcoord = "XlongDD",
                         ycoord = "YlatDD")

write.csv(cat_ests, 'cat_ests.csv', row.names = FALSE)

ggplot(cat_ests[1:2,], aes(x = Estimate.P, y = Category, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(xmin = LCB95Pct.P, xmax = UCB95Pct.P)) +
  labs(x = "Probablistic Estimate (%)", y = NULL)  +
  theme_classic() 

