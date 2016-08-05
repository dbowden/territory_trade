#### Script to create data for Bowden and Diehl R&P submission: Territorial Transfers and Trade #####

#load packages
library(dplyr)

# 1. Clean territorial change data ----

#load COW territorial transfer data (with our extension through 2008)
tc <- read.csv("tc2008.csv", stringsAsFactors = F, na.strings = c(NA, ".", -9))

# limit to state-to-state transfers
tc <- subset(tc, indep == 0) #remove territories that became independent
tc <- subset(tc, exit == 0) #remove territories that were absorbed after state deaths
tc <- subset(tc, loser > 0) #some cases have NA for loser, and the Leage of Nations (0) is loser in one case

##extract legal agreements from "procedur" variable

#they are all included under the "cession" category
tc$agreement <- ifelse(tc$procedur == 3, 1, 0) # remove plebiscites

#but there are also some plebiscites that need to be removed to islate agreements. These are determined by COW code sheets and Beigbeder (1994). 
tc[289,20] <- 0 #390 from 255 in 1920
tc[295,20] <- 0 #310 from 305 in 1921
tc[277,20] <- 0 #305 from 345 in 1920
tc[377,20] <- 0 #750 from 220 in 1950
tc[93,20] <- 0 #325 from 327 in 1870
tc[407,20] <- 0 #471 from 200 in 1961
tc[416,20] <- 0 #820 from 200 in 1963
tc[417,20] <- 0 #820 from 200 in 1963

#create dyad numbers
tc$dyad <- as.numeric(ifelse(tc$gainer > tc$loser, paste(tc$loser,tc$gainer,sep=""), paste(tc$gainer,tc$loser,sep="")))
