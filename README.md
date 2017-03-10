#Probablistic Monitoring Project
* R Version 3.2.5
* Platform: x86_64-pc-linux-gnu (64-bit)
* spsurvey, ggplot2, grid, gridExtra, plyr, reshape2

##Overview
Probability-based survey designs provide a scientifically rigorous way to sample a subset of all
waters and then provide an estimate of the quality of all waters along with a statement about the
uncertainty surrounding that estimate. In a probability survey, a subset of waters is randomly selected.
This ensures the “representativeness” or unbiased nature of the samples. The CT DEEP
Monitoring and Assessment Program has conducted three periodic probability-based surveys between
2001 and 2015. These surveys targeted wadeable perennial streams within the geographic
boundaries of Connecticut. Biological and chemical samples were collected as part of
these surveys. We calculated population estimates for all available data and made note of areas
for further exploration. Select results are presented.

##Design and Sampling Details

**Target Population:**  All wadeable perennial streams within the geographic boundaries of Connecticut.  Assumed that streams less than Strahler Order 1 for 2001 – 2005 survey and less than 1 square mile drainage area were not perennial.

**Timeframe:**  Three surveys were conducted within 5-year monitoring periods:  2001 – 2005, 2006 – 2010, 2011 – 2015

**Sample frame:**  2001 – 2005 survey used US EPA’s Reach File Version 3 (RF3) 1:100,000 scale with reaches coded as perennial.   2006 – 2010 and 2011 - 2015 surveys used the National Hydrography Dataset (NHD) Flowline 1:24,000 scale with reaches coded as perennial.

* 2001 – 2005 – Total stream length (km) = 9931.3570*
* 2006 – 2010 – Total stream length (km) = 12508.5465
* 2011 – 2015 – Total stream length (km) = 12508.5465

*Total stream length is different from other surveys because a different GIS layer was used.

**Survey Design:**  2001 – 2005 used geographically stratified hex-design for a linear continuous network with probability based on stream length in hex and over-sample.  Later surveys used Generalized Random Tessellation Stratified (GRTS) (Stevens and Olsen, 2004) for a linear continuous network with equal probability and over-sample.

##Data and Analysis

**Data:**  ProbMonDesign_2001_2015_022417 - Includes prob mon design information, such as weights, needed for population estimates.  Also includes water chemistry, categorical biological WQ values and biological metrics.

**Description of Statistical Methods:** Population estimates are based on samples from eachstream and use weighted Horvitz-Thompson estimation. We used the local mean variance estimatorto develop confidence intervals around the population estimates. Computations were conductedusing the spsurvey package in R.



