
rm(list = ls())
library(plyr)
library(dplyr)
library(reshape2)

Aquaculture <- read.csv("~/Dropbox/Proyectos/Aquaculture/Production datasets/aquaculture_production_1950_2014.csv")


Aquaculture <- melt(Aquaculture, id.vars=c('country','species','a.area','environment','unit'),var='year')
names(Aquaculture)[7] <- paste('production') 
Aquaculture$year <- as.numeric(gsub( "X", "", as.character(Aquaculture$year)))


AQ = ddply(Aquaculture, .(year,country),
             summarize, 
             a.production = sum(production,na.rm=TRUE),
             a.species = length(unique(species,na.rm=TRUE)))
Fisheries <- read.csv("~/Dropbox/Proyectos/Aquaculture/Production datasets/capture_production_1950_2014.csv")

Fisheries <- melt(Fisheries, id.vars=c('country','species','f.area','measure'),var='year')
names(Fisheries)[6] <- paste('production') 
Fisheries$year <- as.numeric(gsub( "X", "", as.character(Fisheries$year)))


FS = ddply(Fisheries, .(year,country),
         summarize, 
         f.production = sum(production,na.rm=TRUE),
         f.species = length(unique(species,na.rm=TRUE)))

PD <- merge(FS,AQ,by.x=c('year','country'),by.y=c('year','country'),all="TRUE")
PD$p.ratio <- PD$a.production/PD$f.production
PD$p.species <- PD$a.species/PD$f.species

write.csv(PD,'PD.csv')


















