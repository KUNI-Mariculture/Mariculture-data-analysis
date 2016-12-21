## NEW DATASETS 12.20.2016 ##
## TRADE DATA: MINUS NON-EDIBLE SPECIES, INCL. FORAGE FISH
## PRODUCTION DATA: MINUS NON-EDIBLE AND FRESHWATER SPECIES

# Packages 

library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)

# Load data
WD <- getwd()

# Production <- read.csv(paste0(WD,"/data/global_capture_production_filtered.csv"), stringsAsFactors=FALSE)
# Value <- read.csv(paste0(WD, "/data/global_trade_value_filtered.csv", stringsAsFactors=FALSE)
# Catch <- read.csv("~/Dropbox/Proyectos/Aquaculture/Catch.csv", stringsAsFactors=FALSE)
# 
# # Flip databases
# 
# Production<-melt(Production, id.vars=c('country','species','area'),var='year')
# names(Production)[5]<-paste('production') 
# Production$year<-as.numeric(gsub( "X", "", as.character(Production$year)))
# 
# Value<-melt(Value, id.vars=c('country','species','area'),var='year')
# names(Value)[5]<-paste("value") 
# Value$year<-as.numeric(gsub( "X", "", as.character(Value$year)))
# 
# Catch<-melt(Catch, id.vars=c('country','species','area'),var='year')
# names(Catch)[5]<-paste("catch") 
# Catch$year<-as.numeric(gsub( "X", "", as.character(Catch$year)))
# 
# # years do not match so let's use data from 2000 onwards
# 
# Production<-Production[Production$year>=2000,]
# Value<-Value[Value$year>=2000,]
# Catch<-Catch[Catch$year>=2000,]
# 
# Value$value<-suppressWarnings(as.numeric(Value$value))
# Production$production<-suppressWarnings(as.numeric(Production$production))
# Catch$catch<-suppressWarnings(as.numeric(Catch$catch))
# 
# # Also let's create a database with a summary per country
# 
# ValSum=ddply(Value, .(year,country),
#              summarize, 
#              value = sum(value,na.rm=TRUE))
# ValSum$value<-suppressWarnings(as.numeric(ValSum$value))
# 
# # Let's create a column  with indicators for each species
# for (i in 1:nrow(Value)){
#   c=Value$country[i]
#   s=Value$species[i]
#   a=Value$area[i]
#   y=Value$year[i]
#   # First indicator, value per kg
#   Value$value.kg[i]=Value[Value$country==c & Value$species==s & Value$area==a & Value$year==y,5]/
#     Production[Production$country==c & Production$species==s & Production$area==a & Production$year==y,5]
#   # Second indicator, relative value importance
#   Value$relative.value[i]=Value[Value$country==c & Value$species==s & Value$area==a & Value$year==y,5]/
#     ValSum[ValSum$country==c & ValSum$year==y,3]
# }
# 
# #####

# Now let's deal with trade:

TradeQ <- read.csv(paste0(WD, "/data/global_trade_quantity_filtered.csv"), stringsAsFactors=FALSE)
TradeV <- read.csv(paste0(WD, "/data/global_trade_value_filtered.csv"), stringsAsFactors=FALSE)  

TradeQ<-melt(TradeQ, id.vars=c('country','commodity','flow'),var='year')
names(TradeQ)[5]<-paste('quantity') 
TradeQ$year<-as.numeric(gsub( "X", "", as.character(TradeQ$year)))

TradeV<-melt(TradeV, id.vars=c('country','commodity','flow'),var='year')
names(TradeV)[5]<-paste('value') 
TradeV$year<-as.numeric(gsub( "X", "", as.character(TradeV$year)))

TradeQ<-TradeQ[TradeQ$year>=2000,]
TradeV<-TradeV[TradeV$year>=2000,]

TradeQ$quantity<-suppressWarnings(as.numeric(TradeQ$quantity))
TradeV$value<-suppressWarnings(as.numeric(TradeV$value))


# These data are not listed by species so we can just aggregate by country, let's get this over with then...


QEXP=ddply(TradeQ[TradeQ$flow=='Exports',], .(year,country),
           summarize, 
           quantity = sum(quantity,na.rm=TRUE))
QIMP=ddply(TradeQ[TradeQ$flow=='Imports',], .(year,country),
           summarize, 
           quantity = sum(quantity,na.rm=TRUE))

VEXP=ddply(TradeV[TradeV$flow=='Exports',], .(year,country),
           summarize, 
           quantity = sum(value,na.rm=TRUE))
VIMP=ddply(TradeV[TradeV$flow=='Imports',], .(year,country),
           summarize, 
           quantity = sum(value,na.rm=TRUE))

# Ok, so those are just summaries for trade flows by country. Note that the panel is unbalanced
# so I'll build on exports

TRADE<-QEXP 
names(TRADE)[3]<-paste('Q.exp') 

# to deal with the differences in sizes I just run a simple loop to populate the database
# Same idea as before

for (i in 1:nrow(TRADE)){
  c=TRADE$country[i]
  y=TRADE$year[i]
  TRADE$Q.imp[i]=QIMP[QIMP$country==c & QIMP$year==y,3]
  TRADE$V.exp[i]=VEXP[VEXP$country==c & VEXP$year==y,3]
  TRADE$V.imp[i]=VIMP[VIMP$country==c & VIMP$year==y,3]
}


# Finally let's estimate trade budgets

TRADE$Q.balance=TRADE$Q.exp/TRADE$Q.imp
TRADE$V.balance=TRADE$V.exp/TRADE$V.imp
TRADE$UV.balance=(TRADE$V.exp/TRADE$Q.exp)/(TRADE$V.imp/TRADE$Q.imp)

# Let's save the databases


write.csv(TRADE, file = "WORLD_TRADE_122016.csv")
# write.csv(Value, file = "WORLD_VALUE.csv")
# write.csv(Production, file = "WORLD_AQ_PRODUCTION.csv")
# write.csv(CATCH, file = "WORLD_CATCH.csv")




