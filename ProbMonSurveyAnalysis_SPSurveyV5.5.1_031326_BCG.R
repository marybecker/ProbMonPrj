##########CT Probabilistic-Based Stream Survey Project#############

library(spsurvey)
library(ggplot2)

### Load data #####################################################

data <- read.csv('data/ProbMonDesign_2001_2020_013124.csv', header = TRUE)


s1 <- data[data$SurveyName == "2001-2005" & data$EVALSTATUS == "CMPLTE",]
s1 <- s1[s1$BugBCG != "", ]
s3 <- data[data$SurveyName == "2011-2015" & data$EVALSTATUS == "CMPLTE",]
s3 <- s3[s3$BugBCG != "", ]
s4 <- data[data$SurveyName == "2016-2020" & data$EVALSTATUS == "CMPLTE",]
s4 <- s4[s4$BugBCG != "", ]

s1$WGT <- 7772/dim(s1)[1]
s3$WGT <- 7772/dim(s3)[1]
s4$WGT <- 7772/dim(s4)[1]  # adj wgt for total number of samples (e.g. 7772 / 55)

s1$BugAssess <- ifelse(s1$BugBCG == "BCG2", "BCG2", 
                ifelse(s1$BugBCG == "BCG3" | s1$BugBCG == "BCG4", "BCG34",
                       "BCG56"))
s3$BugAssess <- ifelse(s3$BugBCG == "BCG2", "BCG2", 
                       ifelse(s3$BugBCG == "BCG3" | s3$BugBCG == "BCG4", "BCG34",
                              "BCG56"))
s4$BugAssess <- ifelse(s4$BugBCG == "BCG2", "BCG2", 
                       ifelse(s4$BugBCG == "BCG3" | s4$BugBCG == "BCG4", "BCG34",
                              "BCG56"))

cat_ests_bcg <- cat_analysis(s4, siteID = "SITEID",
                         vars = "BugAssess", 
                         weight = "WGT",
                         sizeweight = FALSE,
                         xcoord = "XlongDD",
                         ycoord = "YlatDD")

cat_ests_bcg$Subpopulation <-  "2016-2020"

cat_ests_bcg_s4 <- cat_ests_bcg

cat_ests_bcg <- rbind(cat_ests_bcg_s4, cat_ests_bcg_s3, cat_ests_bcg_s1)

write.csv(cat_ests_bcg, 'cat_ests_bcg.csv', row.names = FALSE)

ggplot(cat_ests[1:2,], aes(x = Estimate.P, y = Category, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(xmin = LCB95Pct.P, xmax = UCB95Pct.P)) +
  labs(x = "Probablistic Estimate (%)", y = NULL)  +
  theme_classic() 

