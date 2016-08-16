#### Script to create panel data for Bowden and Diehl R&P submission ####
library(readxl)
library(countrycode)
library(dplyr)
library(tidyr)
library(readr)
library(haven)
library(zoo)

# 1. (D) Load trade data and create summaries ----
trade <- read_csv("dyadic_trade_3.0.csv", na="-9")

trade$dyad <- as.numeric(paste(trade$ccode1,trade$ccode2,sep=""))

trade <- select(trade, year, dyad, flow1, flow2, ccode1, ccode2)

# Note: ccodes always listed low to high, so flow1 is always flows from ccode2 to ccode1

trade <- filter(trade, is.na(flow1)==F & is.na(flow2)==F)

# 2. (M) Merge in capability data (NMC 4.0) -----
cinc <- read_csv("NMC_v4_0.csv")

cinc <- select(cinc, ccode, year, cinc, tpop)

#convert pop from thousands to raw
cinc$tpop <- cinc$tpop * 1000

colnames(cinc) <- c("ccode1","year","cap1","pop1")
trade <- left_join(trade,cinc)

colnames(cinc) <- c("ccode2","year","cap2","pop2")
trade <- left_join(trade, cinc)

rm(cinc)

# 3. (M) Merge in GDP data ----

###Pre 1950: maddison 2013 version
maddison <- read_excel("mpd_2013-01.xlsx",skip = 2)

colnames(maddison)[1] <- "year"

maddison <- maddison[,1:181]

#convert to long format
maddison <- gather(maddison, country, gdp1, 2:181)

#remove obs outside time period
maddison <- filter(maddison, year>=1870 & year<1950)

#create cow codes
maddison$ccode1 <- countrycode(maddison$country, "country.name", "cown")

#fix a few ccodes
maddison$ccode1[maddison$country=="England/GB/UK"] <- 200
maddison$ccode1[maddison$country=="N. Zealand "] <- 920
maddison$ccode1[maddison$country=="Czecho-slovakia"] <- 315
maddison$ccode1[maddison$country=="Serbia"] <- 340
maddison$ccode1[maddison$country=="Turk-menistan "] <- 701
maddison$ccode1[maddison$country=="Haïti"] <- 41

maddison <- filter(maddison, is.na(ccode1)==F)

#a couple things are duplicated
maddison <- filter(maddison, country!="Russia" & country!="F. Yugoslavia ")

maddison <- select(maddison, year,gdp1,ccode1)

###Post-1950: PWT 9.0 (Real GDP at constant national prices in millions of 2011 USD)
pwt <- read_csv("pwt9.csv")

pwt <- pwt[,2:4]

#convert to cow code
pwt$ccode1 <- countrycode(pwt$RegionCode, "iso3c", "cown")

pwt <- pwt[,2:4]

pwt <- filter(pwt, is.na(ccode1)==F)

colnames(pwt) <- c("year","gdp1","ccode1")

## Post 2003: World bank
wb <- read_csv("API_NY.GDP.MKTP.CD_DS2_en_csv_v2.csv", skip = 4)

wb <- wb[,-c(3:4,61)]

wb$ccode1 <- countrycode(wb$`Country Code`, "iso3c", "cown")

wb$ccode1[wb$`Country Name`=="Serbia"] <- 340

wb <- select(wb, -`Country Name`, -`Country Code`)

wb <- filter(wb, is.na(ccode1)==F)

wb <- gather(wb, year, gdp1, 1:56)

wb <- filter(wb, year > 2002 & year <= 2009)

wb$year <- as.numeric(wb$year)

#merge gdp sources
gdp <- rbind(maddison,pwt,wb)

trade <- left_join(trade, gdp)

colnames(gdp) <- c("year","gdp2","ccode2")
trade <- left_join(trade, gdp)

#convert maddison to raw gdp from per capita, from 1990 to 2011 dollars
#inflation calc uses http://data.bls.gov/cgi-bin/cpicalc.pl?cost1=100&year1=1990&year2=2011
trade <- trade %>% 
  mutate(gdp1=ifelse(year < 1950, gdp1 * pop1 * 1.721 / 1000000, gdp1))

trade <- trade %>% 
  mutate(gdp2=ifelse(year < 1950, gdp2 * pop2 * 1.721 / 1000000, gdp2))

#convert World Bank numbers to millions
trade <- trade %>% 
  mutate(gdp1=ifelse(year > 2002, gdp1 / 1000000, gdp1))

trade <- trade %>% 
  mutate(gdp2=ifelse(year > 2002, gdp2 / 1000000, gdp2))

rm(gdp,maddison,pwt,wb)

# 4. (D) Merge in distance between capitals ----
cap <- read_csv("capdist.csv") #from K. Gleditsch

cap$dyad <- as.numeric(ifelse(cap$numa < cap$numb, paste(cap$numa,cap$numb,sep=""), paste(cap$numb,cap$numa,sep="")))

cap <- select(cap, dyad, kmdist)

cap <- filter(cap, duplicated(cap)==F)

trade <- left_join(trade,cap)

rm(cap)

# 5. (D) Clean territorial change data ----

#load COW territorial transfer data (with our extension through 2008)
tc <- read_csv("tc2008.csv", na = c(".", -9))

# limit to state-to-state transfers
tc <- filter(tc, indep == 0) #remove territories that became independent
tc <- filter(tc, exit == 0) #remove territories that were absorbed after state deaths
tc <- filter(tc, loser > 0) #some cases have NA for loser, and the Leage of Nations (0) is loser in one case

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
tc <- filter(tc, gainer>0)

#create dyad numbers
tc$dyad <- as.numeric(ifelse(tc$gainer > tc$loser, paste(tc$loser,tc$gainer,sep=""), paste(tc$gainer,tc$loser,sep="")))

#collapse to dyad years - lots of cases of multiple transfers
tc <- tc %>% 
  group_by(dyad, year) %>% 
  summarize(transfers=n_distinct(number), agreement=sum(agreement), conflict=sum(conflict), lo_hi=sum(loser < gainer), hi_lo=sum(loser > gainer), pop=sum(pop), area=sum(area), contgain=sum(contgain), contlose=sum(contlose),portion=sum(portion),gaintype=sum(gaintype),losetype=sum(losetype))

trade <- left_join(trade, tc)

rm(tc)

#6. (D) Merge in Rivalry data ------
riv <- read_csv("riv5.10all.csv")

#extract start and end years
riv$beginr <- as.numeric(substr(riv$beginr, 1, 4))
riv$endr <- as.numeric(substr(riv$endr, 1, 4))

#expand to yearly data
all.years <- riv %>%
  group_by(rivnumb) %>%
  do(data.frame(year=seq(.$beginr,.$endr)))

#merge
riv <- right_join(riv,all.years)
riv <- select(riv, -beginr, -endr)

#create dyad nums
riv$dyad <- as.numeric(ifelse(riv$rivala > riv$rivalb, paste(riv$rivalb,riv$rivala,sep=""), paste(riv$rivala,riv$rivalb,sep="")))

riv <- select(riv, -rivala, -rivalb)

#remove two duplicated values - resulting from change during the year. Since we are interested in outcomes following the year of the transfer, we'll keep the later observation.
riv <- filter(riv, duplicated(riv[,c("dyad","year")],fromLast = T)==F)

#merge into territorial change data
trade <- left_join(trade,riv)

rm(riv,all.years)

## extend thru 2008 w/ peace scale data
#create peace scale measures
ps <- read_csv("cm550n4.csv", col_names = F)
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

#remove a few duplicated values - resulting from change during the year. Since we are interested in outcomes following the year of the transfer, we'll keep the later observation.
ps$dup <- ifelse(duplicated(ps[,1:2],fromLast = T)==T, 1, 0)

ps <- filter(ps, dup==0)

ps <- select(ps, -dup)

trade <- left_join(trade,ps)

rm(ps)

#create rivalry var
trade$rivals <- ifelse(trade$rivtyp2=="RIVALRY" | trade$rivtyp2=="ISOLATED" | trade$ps>0.5, 1, 0)
trade$rivals[is.na(trade$rivals)] <- 0

# 7. (D) Merge in contiguity data -----
contig <- read_csv("contdir.csv")

contig <- select(contig,conttype,begin,end,dyad)

contig$begin <- as.numeric(substr(contig$begin,1,4))
contig$end <- as.numeric(substr(contig$end,1,4))

#expand to dyad years
all.years <- contig %>%
  group_by(dyad, begin) %>%
  do(data.frame(year=seq(.$begin,.$end)))

contig <- full_join(all.years,contig)

contig <- subset(contig,select=c(dyad,year,conttype))

#remove a few duplicated values - resulting from change during the year. Since we are interested in outcomes following the year of the transfer, we'll keep the later observation.
contig <- filter(contig, duplicated(contig[,1:2], fromLast = T)==F)

trade <- left_join(trade,contig)

rm(all.years,contig)

## and add colonial contiguity
colcont <- read_csv("Colonial Contiguity v3.0.csv")

colcont$dyad <- as.numeric(paste(colcont$CCode1,colcont$CCode2,sep=""))

colcont <- select(colcont, dyad, BegYear, EndYear, CType)

colcont$id <- seq(1:length(colcont$dyad))

all.years <- colcont %>% 
  group_by(id) %>% 
  do(data.frame(year=seq(.$BegYear,.$EndYear)))

colcont <- right_join(colcont, all.years)

#create min when there are multiple connections in a year
colcont <- colcont %>% 
  group_by(dyad, year) %>% 
  summarize(col.conttype=min(CType))

trade <- left_join(trade, colcont)

trade$col.conttype[is.na(trade$col.conttype)] <- 0

rm(colcont,all.years)

#assume missing values are zero
trade$conttype[is.na(trade$conttype)] <- 0

# 8. (M) Merge in Polity IV --------
polity <- read_excel("p4v2015.xls")

polity <- select(polity, ccode,year,polity2)

colnames(polity) <- c("ccode1","year","polity1")
trade <- left_join(trade,polity)

colnames(polity) <- c("ccode2","year","polity2")
trade <- left_join(trade,polity)

rm(polity)

trade$joint.dem <- ifelse(trade$polity1 >= 6 & trade$polity2 >= 6, 1, 0)

# 9. (D) Merge in Owsiak border settlement data ----
settle <- read_dta("Replication - IBAD Full Settle Dyad-Year.dta")

settle <- settle[,c(2:4,6:7)]

settle$new.border.settle <- ifelse(settle$settle==1 & settle$obs==1, 1, 0)

settle$dyad <- as.numeric(ifelse(settle$ccode1 < settle$ccode2, paste(settle$ccode1, settle$ccode2, sep=""), paste(settle$ccode2, settle$ccode1, sep="")))

settle <- select(settle, dyad, year, settle, new.border.settle)

trade <- left_join(trade, settle)

rm(settle)

#Note: there isn't a great deal of overlap w/ transfers (the CMPS article discusses this), so the settlement IV should be a separate data set with all border settlements.

# 10. (M) Merge in ARCHIGOS -------
arch <- read_dta("Archigos_v.2.9_tv-Public.dta")

arch$startdate <- as.Date(arch$startdate,"%d/%m/%Y")
arch$sty <- as.numeric(format(arch$startdate,"%Y"))

arch$tenure <- arch$year - arch$sty

arch <- select(arch,ccode,year,tenure)

arch <- arch %>%
  group_by(ccode,year) %>%
  summarize(tenure=min(tenure))

colnames(arch) <- c("ccode1","year","tenure1")
trade <- left_join(trade,arch)

colnames(arch) <- c("ccode2","year","tenure2")
trade <- left_join(trade,arch)
rm(arch)

# 11. Create transfer variables ----

# there is only 1 case with mix of agreements and no agreements, so I'll just treat any case w/ agreements as having them

trade <- trade %>% 
  group_by(dyad) %>% 
  mutate(transfer5=ifelse(is.na(lag(transfers))==F | is.na(lag(transfers,2))==F | is.na(lag(transfers,3))==F | is.na(lag(transfers,4))==F | is.na(lag(transfers,5))==F, 1, 0), agreement5=ifelse(lag(agreement)==1 | lag(agreement,2)==1 | lag(agreement,3)==1 | lag(agreement,4)==1 | lag(agreement,5)==1, 1, 0), noagreement5=ifelse(lag(agreement)==0 | lag(agreement,2)==0 | lag(agreement,3)==0 | lag(agreement,4)==0 | lag(agreement,5)==0, 1, 0),conflict5=ifelse(lag(conflict)==1 | lag(conflict,2)==1 | lag(conflict,3)==1 | lag(conflict,4)==1 | lag(conflict,5)==1, 1, 0))

trade$agreement5[is.na(trade$agreement5)] <- 0
trade$conflict5[is.na(trade$conflict5)] <- 0

#create trade vars
trade$tot.flow <- trade$flow1 + trade$flow2

##rolling mean

#remove cases with less than 3 consecutive years
trade <- trade %>% 
  group_by(dyad) %>% 
  mutate(cases=length(year))

trade <- filter(trade, cases>=3)

#rolling mean
trade <- trade %>% 
  group_by(dyad) %>% 
  mutate(tot.flow.mean=rollmean(tot.flow, 3, fill=NA))

trade$tot.gdp <- trade$gdp1 + trade$gdp2

trade$postww2 <- ifelse(trade$year > 1945, 1, 0)

# 12. Write data ----
trade <- filter(trade, is.na(tot.flow.mean)==F & is.na(gdp1)==F & is.na(gdp2)==F & is.na(kmdist)==F)


#trade <- filter(trade, conttype!=0 | col.conttype!=0)

write_csv(trade, "full_panel.csv")
