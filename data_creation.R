#### Script to create data for Bowden and Diehl R&P submission: Territorial Transfers and Trade #####
# This uses the territorial transfer as the unit of analysis. A panel format might be a reasonable alternative.

#load packages
library(readr)
library(dplyr)
library(readxl)
library(haven)
library(countrycode)
library(tidyr)

# 1. Clean territorial change data ----

#load COW territorial transfer data (with our extension through 2008)
tc <- read_csv("tc2008.csv", na = c(NA, ".", -9))

# limit to state-to-state transfers
tc <- filter(tc, indep == 0) #remove territories that became independent
tc <- filter(tc, exit == 0) #remove territories that were absorbed after state deaths
tc <- filter(tc, loser > 0) #some cases have NA for loser, and the Leage of Nations (0) is loser in one case

##extract legal agreements from "procedur" variable

#they are all included under the "cession" category
tc$agreement <- ifelse(tc$procedur == 3, 1, 0)

#but there are also some plebiscites that need to be removed to islate agreements. These are determined by COW code sheets and Beigbeder (1994). 
tc[289, 20] <- 0 #390 from 255 in 1920
tc[295, 20] <- 0 #310 from 305 in 1921
tc[277, 20] <- 0 #305 from 345 in 1920
tc[377, 20] <- 0 #750 from 220 in 1950
tc[93, 20] <- 0 #325 from 327 in 1870
tc[407, 20] <- 0 #471 from 200 in 1961
tc[416, 20] <- 0 #820 from 200 in 1963
tc[417, 20] <- 0 #820 from 200 in 1963

#remove 1 case where gainer is non-state (discovered this after I set the hard codings)
tc <- filter(tc, gainer > 0)

#create dyad numbers
tc$dyad <- as.numeric(ifelse(tc$gainer > tc$loser, paste(tc$loser, tc$gainer, sep = ""), paste(tc$gainer, tc$loser, sep = "")))

#2. Merge in Rivalry data ------
riv <- read_csv("riv5.10all.csv")

#extract start and end years
riv$beginr <- as.numeric(substr(riv$beginr, 1, 4))
riv$endr <- as.numeric(substr(riv$endr, 1, 4))

#expand to yearly data
all.years <- riv %>%
  group_by(rivnumb) %>%
  do(data.frame(year=seq(.$beginr, .$endr)))

#merge
riv <- full_join(riv, all.years, all=T)
riv <- select(riv, -c(beginr,endr))

#create dyad nums
riv$dyad <- as.numeric(ifelse(riv$rivala > riv$rivalb, paste(riv$rivalb, riv$rivala, sep = ""), paste(riv$rivala, riv$rivalb, sep = "")))

riv <- select(riv, -c(rivala,rivalb))

#merge into territorial change data
tc <- left_join(tc, riv)

rm(riv, all.years)

## extend thru 2008 w/ peace scale data
#create peace scale measures
ps <- read_csv("cm550n4.csv", col_names = F)
colnames(ps) <- c("dyad", "year", "ps")
ps$ps[ps$ps < 0] <- NA

#standardize dyadnum
ps$dyad <- as.character(ps$dyad)

ps$dyad <- ifelse(nchar(ps$dyad) == 4, paste("00", ps$dyad, sep = ""),  ps$dyad)
ps$dyad <- ifelse(nchar(ps$dyad) == 5, paste("0", ps$dyad, sep = ""), ps$dyad)

ps$ccode1 <- as.numeric(substr(ps$dyad, 1, 3))
ps$ccode2 <- as.numeric(substr(ps$dyad, 4, 6))

ps$dyad <- as.numeric(ifelse(ps$ccode1 > ps$ccode2, paste(ps$ccode2, ps$ccode1, sep = ""), paste(ps$ccode1, ps$ccode2, sep = "")))

ps <- ps[,1:3]

tc <- left_join(tc, ps)

rm(ps)

tc <- filter(tc, duplicated(tc[,1:26])==F)

#create rivalry var
tc$rivals <- ifelse(tc$rivtyp2 == "RIVALRY" | tc$rivtyp2 == "ISOLATED" | tc$ps > 0.5, 1, 0)
tc$rivals[is.na(tc$rivals)] <- 0

# 3. Merge in contiguity data -----
contig <- read_csv("contdir.csv")

contig <- select(contig, c(conttype,begin,end,dyad))

contig$begin <- as.numeric(substr(contig$begin, 1, 4))
contig$end <- as.numeric(substr(contig$end, 1, 4))

#expand to dyad years
all.years <- contig %>%
  group_by(dyad, begin) %>%
  do(data.frame(year=seq(.$begin, .$end)))

contig <- full_join(all.years, contig)

contig <- select(contig, c(dyad,year,conttype))

tc <- left_join(tc, contig)

rm(all.years, contig)

#A few cases are duplicated due to contigutiy changing as a result of the transfer. I'll use the resulting contiguity.
tc <- tc[with(tc, order(number)),]
tc[235, 29] <- 2
tc[238, 29] <- 2
tc[435,29] <- 1
tc <- tc[-c(236, 239, 436),]

#assume missing values are zero
tc$conttype[is.na(tc$conttype)] <- 0

## and add colonial contiguity
colcont <- read_csv("Colonial Contiguity v3.0.csv")

colcont$dyad <- as.numeric(paste(colcont$CCode1, colcont$CCode2, sep = ""))

colcont <- select(colcont, dyad, BegYear, EndYear, CType)

colcont$id <- seq(1:length(colcont$dyad))

all.years <- colcont %>% 
  group_by(id) %>% 
  do(data.frame(year=seq(.$BegYear, .$EndYear)))

colcont <- right_join(colcont, all.years)

#create min when there are multiple connections in a year
colcont <- colcont %>% 
  group_by(dyad, year) %>% 
  summarize(col.conttype=min(CType))

tc <- left_join(tc, colcont)

tc$col.conttype[is.na(tc$col.conttype)] <- 0

rm(colcont,all.years)

#code a measure for either type of contiguity
tc$any.cont <- ifelse(tc$conttype > 0 | tc$col.conttype > 0, 1, 0)

# 4. Merge in capability data (NMC 4.0) -----
cinc <- read_csv("NMC_v4_0.csv")

cinc <- select(cinc, c(ccode,year,cinc,tpop))

#convert pop from thousands to raw
cinc$tpop <- cinc$tpop * 1000

colnames(cinc) <- c("gainer", "year", "cap_gainer", "pop_gainer")
tc <- left_join(tc, cinc)

colnames(cinc) <- c("loser", "year", "cap_loser", "pop_loser")
tc <- left_join(tc, cinc)

#caprat will always be loser:gainer
tc$caprat <- (tc$cap_loser/tc$cap_gainer)

rm(cinc)

# 5. Merge in Polity IV --------
polity <- read_excel("p4v2015.xls")

polity <- select(polity, ccode, year, polity2)

colnames(polity) <- c("gainer", "year", "polity.gainer")
tc <- left_join(tc, polity)

colnames(polity) <- c("loser", "year", "polity.loser")
tc <- left_join(tc, polity)

rm(polity)

tc$joint.dem <- ifelse(tc$polity.gainer >= 6 & tc$polity.loser >= 6, 1, 0)

# 6. Merge in Owsiak border settlement data ----
settle <- read_dta("Replication - IBAD Full Settle Dyad-Year.dta")

settle <- settle[,c(2:4, 6:7)]

settle$new.border.settle1 <- ifelse(settle$settle==1 & settle$obs < 2, 1, 0)
settle$new.border.settle5 <- ifelse(settle$settle==1 & settle$obs < 6, 1, 0)

settle$dyad <- as.numeric(ifelse(settle$ccode1 < settle$ccode2, paste(settle$ccode1, settle$ccode2, sep = ""), paste(settle$ccode2, settle$ccode1, sep = "")))

settle <- subset(settle, select=c(dyad, year, settle, new.border.settle1, new.border.settle5))

tc <- left_join(tc, settle)

rm(settle)

#Note: there isn't a great deal of overlap w/ transfers (the CMPS article discusses this), so the settlement IV should be a separate data set with all border settlements.

# 7. Merge in ARCHIGOS -------
arch <- read_dta("Archigos_v.2.9_tv-Public.dta")

arch$startdate <- as.Date(arch$startdate,"%d/%m/%Y")
arch$sty <- as.numeric(format(arch$startdate,"%Y"))

arch$tenure <- arch$year - arch$sty

arch <- select(arch, ccode, year, tenure)

arch <- arch %>%
  group_by(ccode, year) %>%
  summarize(tenure=min(tenure))

colnames(arch) <- c("loser", "year", "tenure.loser")
tc <- left_join(tc, arch)

colnames(arch) <- c("gainer", "year", "tenure.gainer")
tc <- left_join(tc, arch)
rm(arch)

# 8. Count previous transfers within dyad----

#sort data
tc <- tc[order(tc$dyad, tc$year),]

tc <- tc %>%
  group_by(dyad, year) %>%
  mutate(simul.transfers=n_distinct(entity))

tc$prev.transfers <- with(tc, ave(dyad, dyad, FUN=seq_along)) -1

tc <- tc %>%
  group_by(dyad, year) %>%
  mutate(min.transfers=min(prev.transfers))

tc$prev.transfers <- ifelse(tc$simul.transfers > 1, tc$min.transfers, tc$prev.transfers)

tc <- select(tc, -min.transfers)

# 9. Merge in trade data -----
trade <- read_csv("dyadic_trade_3.0.csv")

trade <- filter(trade, is.na(ccode1)==F)

trade$dyad <- as.numeric(paste(trade$ccode1, trade$ccode2, sep = ""))

trade <- select(trade, year, dyad, flow1, flow2)

# Note: ccodes always listed low to high, so flow1 is always flows from ccode2 to ccode1

trade$flow1[trade$flow1 < 0] <- NA
trade$flow2[trade$flow2 < 0] <- NA

#lag the trade measure
trade <- trade %>% 
  group_by(dyad) %>% 
  mutate(flow1.lag1 = lag(flow1), flow2.lag1 = lag(flow2, 1), flow1.lag2 = lag(flow1, 2), flow2.lag2 = lag(flow2, 2), flow1.lag3 = lag(flow1, 3), flow2.lag3 = lag(flow2, 3), flow1.lag4 = lag(flow1, 4), flow2.lag4 = lag(flow2, 4), flow1.lag5 = lag(flow1, 5), flow2.lag5 = lag(flow2, 5))

#create leads
trade <- trade %>% 
  group_by(dyad) %>% 
  mutate(flow1.lead1 = lead(flow1, 1), flow2.lead1 = lead(flow2, 1), flow1.lead2 = lead(flow1, 2), flow2.lead2 = lead(flow2, 2), flow1.lead3 = lead(flow1, 3), flow2.lead3 = lead(flow2, 3), flow1.lead4 = lead(flow1, 4), flow2.lead4 = lead(flow2, 4), flow1.lead5 = lead(flow1, 5), flow2.lead5 = lead(flow2, 5))

tc <- left_join(tc, trade)
rm(trade)

## create aggregated trade measures

# DVs
tc$gainer_loser_lead3 <- ifelse(tc$loser < tc$gainer, rowMeans(tc[,c("flow1.lead1", "flow1.lead2", "flow1.lead3")], na.rm=F), rowMeans(tc[,c("flow2.lead1", "flow2.lead2", "flow2.lead3")], na.rm=F))

tc$loser_gainer_lead3 <- ifelse(tc$loser > tc$gainer, rowMeans(tc[,c("flow1.lead1", "flow1.lead2", "flow1.lead3")], na.rm=F), rowMeans(tc[,c("flow2.lead1", "flow2.lead2", "flow2.lead3")], na.rm=F))

tc$gainer_loser_lead5 <- ifelse(tc$loser < tc$gainer, rowMeans(tc[,c("flow1.lead1", "flow1.lead2", "flow1.lead3", "flow1.lead4", "flow1.lead5")], na.rm=F), rowMeans(tc[,c("flow2.lead1", "flow2.lead2", "flow2.lead3", "flow2.lead4", "flow2.lead5")], na.rm=F))

tc$loser_gainer_lead5 <- ifelse(tc$loser > tc$gainer, rowMeans(tc[,c("flow1.lead1", "flow1.lead2", "flow1.lead3", "flow1.lead4", "flow1.lead5")], na.rm=F), rowMeans(tc[,c("flow2.lead1", "flow2.lead2", "flow2.lead3", "flow2.lead4", "flow2.lead5")], na.rm=F))

# IVs
tc$gainer_loser_lag3 <- ifelse(tc$loser < tc$gainer, rowMeans(tc[,c("flow1.lag1", "flow1.lag2", "flow1.lag3")], na.rm=F), rowMeans(tc[,c("flow2.lag1", "flow2.lag2", "flow2.lag3")], na.rm=F))

tc$loser_gainer_lag3 <- ifelse(tc$loser > tc$gainer, rowMeans(tc[,c("flow1.lag1", "flow1.lag2", "flow1.lag3")], na.rm=F), rowMeans(tc[,c("flow2.lag1", "flow2.lag2", "flow2.lag3")], na.rm=F))

tc$gainer_loser_lag5 <- ifelse(tc$loser < tc$gainer, rowMeans(tc[,c("flow1.lag1", "flow1.lag2", "flow1.lag3", "flow1.lag4", "flow1.lag5")], na.rm=F), rowMeans(tc[,c("flow2.lag1", "flow2.lag2", "flow2.lag3", "flow2.lag4", "flow2.lag5")], na.rm=F))

tc$loser_gainer_lag5 <- ifelse(tc$loser > tc$gainer, rowMeans(tc[,c("flow1.lag1", "flow1.lag2", "flow1.lag3", "flow1.lag4", "flow1.lag5")], na.rm=F), rowMeans(tc[,c("flow2.lag1", "flow2.lag2", "flow2.lag3", "flow2.lag4", "flow2.lag5")], na.rm=F))

#create dyadic measures
tc$agg_lead3 <- tc$loser_gainer_lead3 + tc$gainer_loser_lead3
tc$agg_lead5 <- tc$loser_gainer_lead5 + tc$gainer_loser_lead5
tc$agg_lag3 <- tc$loser_gainer_lag3 + tc$gainer_loser_lag3
tc$agg_lag5 <- tc$loser_gainer_lag5 + tc$loser_gainer_lag5

tc <- select(tc,  -c(flow1.lag2, flow1.lag3, flow1.lag4, flow1.lag5, flow2.lag2, flow2.lag3, flow2.lag4, flow2.lag5, flow1.lead2, flow1.lead3, flow1.lead4, flow1.lead5, flow2.lead2, flow2.lead3, flow2.lead4, flow2.lead5))

# 10. Merge in distance between capitals ----
cap <- read_csv("capdist.csv") #from K. Gleditsch

cap$dyad <- as.numeric(ifelse(cap$numa < cap$numb, paste(cap$numa, cap$numb, sep = ""), paste(cap$numb, cap$numa, sep = "")))

cap <- select(cap, dyad, kmdist)

cap <- filter(cap, duplicated(cap)==F)

tc <- left_join(tc, cap)

rm(cap)

# 11. Merge in GDP data ----

###Pre 1950: maddison 2013 version
maddison <- read_excel("mpd_2013-01.xlsx", skip = 2)

colnames(maddison)[1] <- "year"

maddison <- maddison[,1:181]

#convert to long format
maddison <- gather(maddison, country, gdp.loser, 2:181)

#remove obs outside time period
maddison <- subset(maddison, year >= 1816 & year < 1950)

#create cow codes
maddison$loser <- countrycode(maddison$country, "country.name", "cown")

#fix a few ccodes
maddison$loser[maddison$country == "England/GB/UK"] <- 200
maddison$loser[maddison$country == "N. Zealand "] <- 920
maddison$loser[maddison$country == "Czecho-slovakia"] <- 315
maddison$loser[maddison$country == "Serbia"] <- 340
maddison$loser[maddison$country == "Turk-menistan "] <- 701
maddison$loser[maddison$country == "Haïti"] <- 41

maddison <- filter(maddison, is.na(loser)==F)

#a couple things are duplicated
maddison <- filter(maddison, country!="Russia" & country!="F. Yugoslavia ")

maddison <- select(maddison, year, gdp.loser, loser)

###Post-1950: PWT 9.0 (Real GDP at constant national prices in millions of 2011 USD)
pwt <- read_csv("pwt9.csv")

pwt <- pwt[,2:4]

#convert to cow code
pwt$loser <- countrycode(pwt$RegionCode, "iso3c", "cown")

pwt <- pwt[,2:4]

pwt <- filter(pwt, is.na(loser)==F)

colnames(pwt) <- c("year","gdp.loser","loser")

## Post 2003: World bank
wb <- read_csv("API_NY.GDP.MKTP.CD_DS2_en_csv_v2.csv", skip = 4)

wb <- wb[,-c(3:4,61)]

wb$loser <- countrycode(wb$`Country Code`, "iso3c", "cown")

wb$loser[wb$`Country Name` == "Serbia"] <- 340

wb <- select(wb, -`Country Name`, -`Country Code`)

wb <- filter(wb, is.na(loser) == F)

wb <- gather(wb, year, gdp.loser, 1:56)

wb <- filter(wb, year > 2002 & year <= 2009)

wb$year <- as.numeric(wb$year)

#merge gdp sources
gdp <- rbind(maddison,pwt,wb)

tc <- left_join(tc, gdp)

colnames(gdp) <- c("year", "gdp.gainer", "gainer")
tc <- left_join(tc, gdp)

#convert maddison to raw gdp from per capita, from 1990 to 2011 dollars
#inflation calc uses http://data.bls.gov/cgi-bin/cpicalc.pl?cost1=100&year1=1990&year2=2011
tc <- tc %>% 
  mutate(gdp.loser=ifelse(year < 1950, gdp.loser * pop_loser * 1.721 / 1000000, gdp.loser))

tc <- tc %>% 
  mutate(gdp.gainer=ifelse(year < 1950, gdp.gainer * pop_gainer * 1.721 / 1000000, gdp.gainer))

#convert World Bank numbers to millions
tc <- tc %>% 
  mutate(gdp.loser=ifelse(year > 2002, gdp.loser / 1000000, gdp.loser))

tc <- tc %>% 
  mutate(gdp.gainer=ifelse(year > 2002, gdp.gainer / 1000000, gdp.gainer))

rm(gdp,maddison,pwt,wb)

tc$agg_pop <- tc$pop_loser + tc$pop_gainer

# 12. Use Gleditsch data to fill in some NAs ----
gled <- read.table("exptradegdpv4/expdata.asc", header=T, na.strings = ".")

gled$dyad <- as.numeric(paste(gled$numa, gled$numb, sep = ""))

#adjust for inflation (data in per capita 1996 dollars)
gled$rgdpca <- gled$rgdpca * gled$popa * 1.433
gled$rgdpcb <- gled$rgdpcb * gled$popb * 1.433

gled <- select(gled, dyad, year, rgdpca, rgdpcb)

tc <- left_join(tc, gled)

tc$gdp.loser.alt <- ifelse(tc$loser < tc$gainer, tc$rgdpca, tc$rgdpcb)
tc$gdp.gainer.alt <- ifelse(tc$loser < tc$gainer, tc$rgdpcb, tc$rgdpca)

tc$gdp.loser <- ifelse(is.na(tc$gdp.loser) == T, tc$gdp.loser.alt, tc$gdp.loser)
tc$gdp.gainer <- ifelse(is.na(tc$gdp.gainer) == T, tc$gdp.gainer.alt, tc$gdp.gainer)

tc$agg_gdp <- tc$gdp.loser + tc$gdp.gainer

tc <- select(tc, -rgdpca, -rgdpcb, -gdp.gainer.alt, -gdp.loser.alt)

rm(gled)

# 13. Add PTA data ----
pta <- read_dta("TGR_AER2007_merged/TGR2007.dta")

pta$cty1name[pta$cty1name=="KYRQYZ REPUBLIC"] <- "Kyrgyzstan"
pta$cty2name[pta$cty2name=="KYRQYZ REPUBLIC"] <- "Kyrgyzstan"
pta$cty1name[pta$cty1name=="MOLDVA"] <- "Moldova"
pta$cty2name[pta$cty2name=="MOLDVA"] <- "Moldova"

pta$ccode1 <- countrycode(pta$cty1name, "country.name", "cown")
pta$ccode2 <- countrycode(pta$cty2name, "country.name", "cown")

pta$dyad <- as.numeric(ifelse(pta$ccode1 < pta$ccode2, paste(pta$ccode1, pta$ccode2, sep = ""), paste(pta$ccode2, pta$ccode1, sep = "")))

pta <- select(pta, year, dyad, regional, gattmbr1, gattmbr2, comlang, comcol, colony, curcol, custrict)

colnames(pta) <- c("year","dyad", "fta", "gatt1", "gatt2", "comlang", "comcol", "colony", "curcol", "currencyu")

tc <- left_join(tc, pta)

rm(pta)

#fill the colonial history and common language backwards, as they should be constant
tc <- tc %>%
  group_by(dyad) %>% 
  mutate(comlang = na.locf(comlang, fromLast = T, na.rm = F), comcol = na.locf(comcol, fromLast = T, na.rm = F), colony = na.locf(colony, fromLast = T, na.rm = F))

tc <- tc %>%
  group_by(dyad) %>% 
  mutate(comlang = na.locf(comlang, na.rm = F), comcol = na.locf(comcol, na.rm = F), colony = na.locf(colony, na.rm = F))

tc$joint.gatt.any <- ifelse(tc$gatt1 != "out" & tc$gatt2 != "out", 1, 0)
tc$joint.gatt.formal <- ifelse((tc$gatt1 == "wto" | tc$gatt1 == "art33" | tc$gatt1 == "art26:5") & (tc$gatt2 == "wto" | tc$gatt2 == "art33" | tc$gatt2 == "art26:5"), 1, 0)

# 14. Write data ----
write_csv(tc, "transfer_as_unit.csv")
