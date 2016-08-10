# OLS models of the effect of legalized territorial trasnfers on trade
library(texreg)

tc <- read.csv("transfer_as_unit.csv")

# 1. Trade from loser to gainer ----

#first let's find the right baseline
l1 <- lm(loser_gainer_lead3 ~ log10(gdp.gainer) + kmdist, data=tc)

l2 <- lm(log10(loser_gainer_lead3+.001) ~ log10(gdp.gainer) + kmdist, data=tc)

l3 <- lm(log10(loser_gainer_lead3+.001) ~ log10(gdp.gainer) + kmdist + year, data=tc)

screenreg(list(l1,l2,l3))

# and look at different DVs
l5 <- lm(log10(loser_gainer_lead3+.001) ~ log10(gdp.gainer) + log10(kmdist) + (year>1945) + agreement, data=tc)

l6 <- lm(log10(loser_gainer_lead5+.001) ~ log10(gdp.gainer) + log10(kmdist) + (year>1945) + agreement, data=tc)

l7 <- lm(log10(gainer_loser_lead3+.001) ~ log10(gdp.gainer) + log10(kmdist) + (year>1945) + agreement, data=tc)

l8 <- lm(log10(gainer_loser_lead5+.001) ~ log10(gdp.gainer) + log10(kmdist) + (year>1945) + agreement, data=tc)

screenreg(list(l5,l6,l7,l8))

#2. aggregate the flows ----
tc$agg_lead3 <- tc$loser_gainer_lead3 + tc$gainer_loser_lead3
tc$agg_lead5 <- tc$loser_gainer_lead5 + tc$gainer_loser_lead5
tc$agg_lag3 <- tc$loser_gainer_lag3 + tc$gainer_loser_lag5
tc$agg_lag5 <- tc$loser_gainer_lag5 + tc$gainer_loser_lag5

tc$post3  <- (tc$flow1.lead1 + tc$flow2.lead1 + tc$flow1.lead2 + tc$flow2.lead2 + tc$flow1.lead3 + tc$flow2.lead3) / 3

tc$agg.gdp <- tc$gdp.loser + tc$gdp.gainer

l9 <- lm(log10(agg_lead3+.001) ~ log10(agg.gdp) + log10(kmdist) + (year) + agreement, data=tc)

l10 <- lm(log10(agg_lead3+.001) ~ log10(agg_lag3+.001) + log10(agg.gdp) + log10(kmdist) + (year) + agreement, data=tc)

l11 <- lm(log10(agg_lead5+.001) ~ log10(agg.gdp) + log10(kmdist) + (year) + agreement, data=tc)

l12 <- lm(log10(agg_lead5+.001) ~ log10(agg_lag5+.001) + log10(agg.gdp) + log10(kmdist) + (year) + agreement, data=tc)

screenreg(list(l9,l10,l11,l12))

# try differences in trade
tc$diff3 <- tc$agg_lead3 - tc$agg_lag3
tc$diff5 <- tc$agg_lead5 - tc$agg_lag5

l13 <- lm(log10(diff3+.001) ~ log10(agg.gdp) + log10(kmdist) + agreement + year, data=tc)

screenreg(list(l13))


l14 <- lm(log(post3+.01) ~ log(agg.gdp) + log(kmdist) + agreement + rivals, data=tc)

screenreg(list(l14))
