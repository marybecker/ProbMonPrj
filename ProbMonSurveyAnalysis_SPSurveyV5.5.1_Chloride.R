##########CT Probabilistic-Based Stream Survey Project - Chloride###############

library(spsurvey)
library(ggplot2)

### Load data ##################################################################

data <- read.csv("data/ProbMonDesign_2001_2020_081824.csv", header = TRUE)
dcmp <- data[data$EVALSTATUS == "CMPLTE",]

cont_ests <- cont_analysis(dcmp, siteID = "SITEID",
                          vars = "Chloride", 
                          weight = "WGT",
                          xcoord = "XlongDD",
                          ycoord = "YlatDD",
                          subpops = "SurveyName")

cont_ests_p50 <- cont_ests$Pct[cont_ests$Pct$Statistic == "50Pct",]
cont_mean     <- cont_ests$Mean

d <- list(cont_ests_p50, cont_mean)
t <- c("Median", "Average")
p <- c("Chloride (ppm)")
l <- paste("Concentration of", p ,"\n in Connecticut Streams")
i <- 1

ggplot(d[[i]],aes(x = Subpopulation, y = Estimate))+
  geom_bar(position = position_dodge(),
           stat = "identity",
           colour = "black",
           fill = "#0D2D6C", 
           size = 0.3,
           width = 0.7) +
  # geom_errorbar(aes(ymin = LCB95Pct, ymax = UCB95Pct),
  #               width = 0.2,
  #               position = position_dodge(0.7)) +
  labs(title = paste(t[i], l), 
       y = p, 
       x = "5-Year Interval Statewide Stream Surveys") +
  annotate("text", 
           x = 1:4, 
           y = 5, 
           color = "white",
           label = c(round(d[[i]]$Estimate[1], digits = 1), 
                     round(d[[i]]$Estimate[2], digits = 1), 
                     round(d[[i]]$Estimate[3], digits = 1), 
                     round(d[[i]]$Estimate[4], digits = 1)))+
  theme_bw()+
  theme(legend.position ="none",
        plot.title = element_text(hjust = 0.5),
        text=element_text(size=10,face="bold"))

ggsave(paste0("results/chloride_",t[i],"_2001_2020.png"))

ggplot(d[[i]],aes(x = Subpopulation, y = Estimate, group = 1))+
  geom_line(linetype = 2) +
  geom_point(colour = "#0D2D6C", size = 3) +
  ylim(0, max(d[[i]]$Estimate))+
  labs(title = paste(t[i], l), 
       y = p, 
       x = "5-Year Interval Statewide Stream Surveys") +
  annotate("text", 
           x = 1:4, 
           y = c(d[[i]]$Estimate[1] - 3, 
                 d[[i]]$Estimate[2] - 3,
                 d[[i]]$Estimate[3] - 3,
                 d[[i]]$Estimate[4] - 3),
           color = "black",
           label = c(round(d[[i]]$Estimate[1], digits = 1), 
                     round(d[[i]]$Estimate[2], digits = 1), 
                     round(d[[i]]$Estimate[3], digits = 1), 
                     round(d[[i]]$Estimate[4], digits = 1)))+
  theme_bw()+
  theme(legend.position ="none",
        plot.title = element_text(hjust = 0.5),
        text=element_text(size=10,face="bold"))

ggsave(paste0("results/chloride_point_",t[i],"_2001_2020.png"))

