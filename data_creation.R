#### Script to create data for Bowden and Diehl R&P submission: Territorial Transfers and Trade #####
# This uses the territorial transfer as the unit of analysis. A panel format might be a reasonable alternative.

#load packages
library(dplyr)
library(readxl)
library(haven)
library(countrycode)
library(tidyr)

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

#remove 1 case where gainer is non-state (discovered this after I set the hard codings)
tc <- subset(tc, gainer>0)

#create dyad numbers
tc$dyad <- as.numeric(ifelse(tc$gainer > tc$loser, paste(tc$loser,tc$gainer,sep=""), paste(tc$gainer,tc$loser,sep="")))

#2. Merge in Rivalry data ------
riv <- read.csv("riv5.10all.csv")

#extract start and end years
riv$beginr <- as.numeric(substr(riv$beginr, 1, 4))
riv$endr <- as.numeric(substr(riv$endr, 1, 4))

#expand to yearly data
all.years <- riv %>%
  group_by(rivnumb) %>%
  do(data.frame(year=seq(.$beginr,.$endr)))

#merge
riv <- merge(riv,all.years,all=T)
riv <- subset(riv, select = -c(beginr,endr))

#create dyad nums
riv$dyad <- ifelse(riv$rivala > riv$rivalb, paste(riv$rivalb,riv$rivala,sep=""), paste(riv$rivala,riv$rivalb,sep=""))

riv <- subset(riv, select = -c(rivala,rivalb))

#merge into territorial change data
tc <- merge(tc,riv,all.x=T,all.y=F,sort=F)

rm(riv,all.years)

## extend thru 2008 w/ peace scale data
#create peace scale measures
ps <- read.csv("cm550n4.csv",header = F)
colnames(ps) <- c("dyad","year","ps")
ps$ps[ps$ps<0] <- NA

#standardize dyadnum
ps$dyad <- as.character(ps$dyad)

ps$dyad <- ifelse(nchar(ps$dyad)==4, paste("00",ps$dyad,sep=""), ps$dyad)
ps$dyad <- ifelse(nchar(ps$dyad)==5,paste("0",ps$dyad,sep=""),ps$dyad)

ps$ccode1 <- as.numeric(substr(ps$dyad,1,3))
ps$ccode2 <- as.numeric(substr(ps$dyad,4,6))

ps$dyad <- as.numeric(ifelse(ps$ccode1 > ps$ccode2, paste(ps$ccode2,ps$ccode1,sep=""), paste(ps$ccode1,ps$ccode2,sep="")))

ps <- ps[,1:3]

tc <- merge(tc,ps,all.x=T,all.y=F,sort=F)

rm(ps)

tc <- subset(tc, duplicated(tc[,1:26])==F)

#create rivalry var
tc$rivals <- ifelse(tc$rivtyp2=="RIVALRY" | tc$rivtyp2=="ISOLATED" | tc$ps>0.5, 1, 0)
tc$rivals[is.na(tc$rivals)] <- 0

# 3. Merge in contiguity data -----
contig <- read.csv("contdir.csv")

contig <- subset(contig,select=c(conttype,begin,end,dyad))

contig$begin <- as.numeric(substr(contig$begin,1,4))
contig$end <- as.numeric(substr(contig$end,1,4))

#expand to dyad years
all.years <- contig %>%
  group_by(dyad, begin) %>%
  do(data.frame(year=seq(.$begin,.$end)))

contig <- merge(all.years,contig,all=T)

contig <- subset(contig,select=c(dyad,year,conttype))

tc <- merge(tc,contig,all.x=T,all.y=F,sort=F)

rm(all.years,contig)

#A few cases are duplicated due to contigutiy changing as a result of the transfer. I'll use the resulting contiguity.
tc <- tc[with(tc, order(number)),]
tc[235,29] <- 2
tc[238,29] <- 2
tc[435,29] <- 1
tc <- tc[-c(236,239,436),]

#assume missing values are zero
tc$conttype[is.na(tc$conttype)] <- 0

# 4. Merge in capability data (NMC 4.0) -----
cinc <- read.csv("NMC_v4_0.csv")

cinc <- subset(cinc,select=c(ccode,year,cinc,tpop))

#convert pop from thousands to raw
cinc$tpop <- cinc$tpop * 1000

colnames(cinc) <- c("gainer","year","cap_gainer","pop_gainer")
tc <- merge(tc,cinc, all.x=T,all.y=F)

colnames(cinc) <- c("loser","year","cap_loser","pop_loser")
tc <- merge(tc, cinc, all.x=T,all.y=F)

tc$caprat <- (tc$cap_loser/tc$cap_gainer)

rm(cinc)

# 5. Merge in Polity IV --------
polity <- read_excel("p4v2015.xls")

polity <- subset(polity, select=c(ccode,year,polity2))

colnames(polity) <- c("gainer","year","polity.gainer")
tc <- merge(tc,polity,all.x=T,all.y=F)

colnames(polity) <- c("loser","year","polity.loser")
tc <- merge(tc,polity,all.x=T,all.y=F)

rm(polity)

tc$joint.dem <- ifelse(tc$polity.gainer >= 6 & tc$polity.loser >= 6, 1, 0)

# 6. Merge in Owsiak border settlement data ----
settle <- read_dta("Replication - IBAD Full Settle Dyad-Year.dta")

settle <- settle[,c(2:4,6:7)]

settle$new.border.settle1 <- ifelse(settle$settle==1 & settle$obs < 2, 1, 0)
settle$new.border.settle5 <- ifelse(settle$settle==1 & settle$obs < 6, 1, 0)

settle$dyad <- ifelse(settle$ccode1 < settle$ccode2, paste(settle$ccode1, settle$ccode2, sep=""), paste(settle$ccode2, settle$ccode1, sep=""))

settle <- subset(settle, select=c(dyad, year, settle, new.border.settle1, new.border.settle5))

tc <- merge(tc, settle, all.x=T, all.y=F)

rm(settle)

#Note: there isn't a great deal of overlap w/ transfers (the CMPS article discusses this), so the settlement IV should be a separate data set with all border settlements.

# 7. Merge in ARCHIGOS -------
arch <- read_dta("Archigos_v.2.9_tv-Public.dta")

arch$startdate <- as.Date(arch$startdate,"%d/%m/%Y")
arch$sty <- as.numeric(format(arch$startdate,"%Y"))

arch$tenure <- arch$year - arch$sty

arch <- subset(arch,select=c(ccode,year,tenure))

arch <- arch %>%
  group_by(ccode,year) %>%
  summarize(tenure=min(tenure))

colnames(arch) <- c("loser","year","tenure.loser")
tc <- merge(tc,arch,all.x=T,all.y=F)

colnames(arch) <- c("gainer","year","tenure.gainer")
tc <- merge(tc,arch,all.x=T,all.y=F)
rm(arch)

# 8. Count previous transfers within dyad----

#sort data
tc <- tc[order(tc$dyad,tc$year),]

tc <- tc %>%
  group_by(dyad,year) %>%
  mutate(simul.transfers=n_distinct(entity))

tc$prev.transfers <- with(tc, ave(dyad, dyad, FUN=seq_along)) -1

tc <- tc %>%
  group_by(dyad,year) %>%
  mutate(min.transfers=min(prev.transfers))

tc$prev.transfers <- ifelse(tc$simul.transfers>1,tc$min.transfers,tc$prev.transfers)

tc <- subset(tc,select=-min.transfers)

# 9. Merge in trade data -----
trade <- read.csv("dyadic_trade_3.0.csv")

trade$dyad <- as.numeric(paste(trade$ccode1,trade$ccode2,sep=""))

trade <- subset(trade,select=c(year,dyad,flow1,flow2))

# Note: ccodes always listed low to high, so flow1 is always flows from ccode2 to ccode1

trade$flow1[trade$flow1 < 0] <- NA
trade$flow2[trade$flow2 < 0] <- NA

#lag the trade measure
trade <- trade %>% 
  group_by(dyad) %>% 
  mutate(flow1.lag1=lag(flow1,1),flow2.lag1=lag(flow2,1),flow1.lag2=lag(flow1,2),flow2.lag2=lag(flow2,2),flow1.lag3=lag(flow1,3),flow2.lag3=lag(flow2,3),flow1.lag4=lag(flow1,4),flow2.lag4=lag(flow2,4),flow1.lag5=lag(flow1,5),flow2.lag5=lag(flow2,5))

#create leads
trade <- trade %>% 
  group_by(dyad) %>% 
  mutate(flow1.lead1=lead(flow1,1),flow2.lead1=lead(flow2,1),flow1.lead2=lead(flow1,2),flow2.lead2=lead(flow2,2),flow1.lead3=lead(flow1,3),flow2.lead3=lead(flow2,3),flow1.lead4=lead(flow1,4),flow2.lead4=lead(flow2,4),flow1.lead5=lead(flow1,5),flow2.lead5=lead(flow2,5))

tc <- merge(tc,trade,all.x=T,all.y=F)
rm(trade)

## create aggregated trade measures

# DVs
tc$gainer_loser_lead3 <- ifelse(tc$loser < tc$gainer, rowMeans(tc[,c("flow1.lead1","flow1.lead2","flow1.lead3")], na.rm=T), rowMeans(tc[,c("flow2.lead1","flow2.lead2","flow2.lead3")], na.rm=T))

tc$loser_gainer_lead3 <- ifelse(tc$loser > tc$gainer, rowMeans(tc[,c("flow1.lead1","flow1.lead2","flow1.lead3")], na.rm=T), rowMeans(tc[,c("flow2.lead1","flow2.lead2","flow2.lead3")], na.rm=T))

tc$gainer_loser_lead5 <- ifelse(tc$loser < tc$gainer, rowMeans(tc[,c("flow1.lead1","flow1.lead2","flow1.lead3","flow1.lead4","flow1.lead5")], na.rm=T), rowMeans(tc[,c("flow2.lead1","flow2.lead2","flow2.lead3","flow2.lead4","flow2.lead5")], na.rm=T))

tc$loser_gainer_lead5 <- ifelse(tc$loser > tc$gainer, rowMeans(tc[,c("flow1.lead1","flow1.lead2","flow1.lead3","flow1.lead4","flow1.lead5")], na.rm=T), rowMeans(tc[,c("flow2.lead1","flow2.lead2","flow2.lead3","flow2.lead4","flow2.lead5")], na.rm=T))

# IVs
tc$gainer_loser_lag3 <- ifelse(tc$loser < tc$gainer, rowMeans(tc[,c("flow1.lag1","flow1.lag2","flow1.lag3")], na.rm=T), rowMeans(tc[,c("flow2.lag1","flow2.lag2","flow2.lag3")], na.rm=T))

tc$loser_gainer_lag3 <- ifelse(tc$loser > tc$gainer, rowMeans(tc[,c("flow1.lag1","flow1.lag2","flow1.lag3")], na.rm=T), rowMeans(tc[,c("flow2.lag1","flow2.lag2","flow2.lag3")], na.rm=T))

tc$gainer_loser_lag5 <- ifelse(tc$loser < tc$gainer, rowMeans(tc[,c("flow1.lag1","flow1.lag2","flow1.lag3","flow1.lag4","flow1.lag5")], na.rm=T), rowMeans(tc[,c("flow2.lag1","flow2.lag2","flow2.lag3","flow2.lag4","flow2.lag5")], na.rm=T))

tc$loser_gainer_lag5 <- ifelse(tc$loser > tc$gainer, rowMeans(tc[,c("flow1.lag1","flow1.lag2","flow1.lag3","flow1.lag4","flow1.lag5")], na.rm=T), rowMeans(tc[,c("flow2.lag1","flow2.lag2","flow2.lag3","flow2.lag4","flow2.lag5")], na.rm=T))

tc <- subset(tc, select=-c(flow1.lag2,flow1.lag3,flow1.lag4,flow1.lag5,flow2.lag2,flow2.lag3,flow2.lag4,flow2.lag5,flow1.lead2,flow1.lead3,flow1.lead4,flow1.lead5,flow2.lead2,flow2.lead3,flow2.lead4,flow2.lead5))

# 10. Merge in distance between capitals ----
cap <- read.csv("capdist.csv") #from K. Gleditsch

cap$dyad <- as.numeric(ifelse(cap$numa < cap$numb, paste(cap$numa,cap$numb,sep=""), paste(cap$numb,cap$numa,sep="")))

cap <- subset(cap,select=c(dyad,kmdist))

cap <- subset(cap,duplicated(cap)==F)

tc <- merge(tc,cap,all.x=T,all.y=F)

rm(cap)

# 11. Merge in GDP data ----

###Pre 1950: maddison 2013 version
maddison <- read_excel("mpd_2013-01.xlsx",skip = 2)

colnames(maddison)[1] <- "year"

maddison <- maddison[,1:181]

#convert to long format
maddison <- gather(maddison, country, gdp.loser, 2:181)

#remove obs outside time period
maddison <- subset(maddison, year>=1816 & year<1950)

#create cow codes
maddison$loser <- countrycode(maddison$country, "country.name", "cown")

#fix a few ccodes
maddison$loser[maddison$country=="England/GB/UK"] <- 200
maddison$loser[maddison$country=="N. Zealand "] <- 920
maddison$loser[maddison$country=="Czecho-slovakia"] <- 315
maddison$loser[maddison$country=="Serbia"] <- 340
maddison$loser[maddison$country=="Turk-menistan "] <- 701
maddison$loser[maddison$country=="Haïti"] <- 41

maddison <- subset(maddison, is.na(loser)==F)

#a couple things are duplicated
maddison <- subset(maddison, country!="Russia" & country!="F. Yugoslavia ")

maddison <- subset(maddison, select=c(year,gdp.loser,loser))

###Post-1950: PWT 9.0 (Real GDP at constant national prices in millions of 2011 USD)
pwt <- read.csv("pwt9.csv")

pwt <- pwt[,2:4]

#convert to cow code
pwt$loser <- countrycode(pwt$RegionCode, "iso3c", "cown")

pwt <- pwt[,2:4]

pwt <- subset(pwt, is.na(loser)==F)

colnames(pwt) <- c("year","gdp.loser","loser")

gdp <- rbind(maddison,pwt)

tc <- merge(tc, gdp, all.x=T, all.y=F)

colnames(gdp) <- c("year","gdp.gainer","gainer")
tc <- merge(tc, gdp, all.x=T, all.y=F)

#convert maddison to raw gdp from per capita
tc <- tc %>% 
  mutate(gdp.loser=ifelse(year < 1950, gdp.loser * pop_loser / 1000000, gdp.loser))

tc <- tc %>% 
  mutate(gdp.gainer=ifelse(year < 1950, gdp.gainer * pop_gainer / 1000000, gdp.gainer))

rm(gdp,maddison,pwt)

# 12. Write data ----
write.csv(tc, "transfer_as_unit.csv",row.names = F)
