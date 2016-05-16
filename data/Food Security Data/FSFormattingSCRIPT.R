# Molly Wilson & Patricia Faundez
# FAO Food Security Data

library(dplyr)
library(reshape2)
library(ggplot2)

#load files, remove aggregated rows, melt

#set working directory: ~/Google Drive/Kuni Fellowship 2016/Data sources/Food Security Data
setwd("~/Google Drive/***/Kuni Fellowship 2016/Data sources/Food Security Data") #wd for molly's comp

#####

EnergyAdequacy <- read.csv(file="EnergyAdequacy.csv", header=FALSE, stringsAsFactors=FALSE)
EnergyAdequacy <- EnergyAdequacy[-c(1, 2, 3, 4, 5, 6, 7, 14, 15, 16, 38, 54, 72, 73, 82, 88, 99, 111, 125, 126, 151, 152, 161, 175, 197, 255, 256, 257, 258, 259, 260),-1] # drop rows with heading, aggregated regional data from original FAO datasets
names(EnergyAdequacy) <- c('country',1992:2016) # adjust by years available

EnergyAdequacy<-melt(EnergyAdequacy, id.vars=c('country'),var='year')
names(EnergyAdequacy)[3]<-paste('energy_adequacy') 
EnergyAdequacy$year <- as.numeric(as.character(EnergyAdequacy$year))

#####

FoodDeficit <- read.csv(file="FoodDeficit.csv", header=FALSE, stringsAsFactors=FALSE)
FoodDeficit <- FoodDeficit[-c(1, 2, 3, 4, 5, 6, 7, 14, 15, 16, 38, 54, 72, 73, 82, 88, 99, 111, 125, 126, 151, 152, 161, 175, 197, 255, 256, 257, 258, 259, 260),-1]
names(FoodDeficit) <- c('country',1992:2016)

FoodDeficit<-melt(FoodDeficit, id.vars=c('country'),var='year')
names(FoodDeficit)[3]<-paste('food_deficit') 
FoodDeficit$year <- as.numeric(as.character(FoodDeficit$year))

#####

FoodPrice <- read.csv(file="FoodPrice.csv", header=FALSE, stringsAsFactors=FALSE)
FoodPrice <- FoodPrice[-c(1, 2, 3, 4, 5, 6, 7, 14, 15, 16, 38, 54, 72, 73, 82, 88, 99, 111, 125, 126, 151, 152, 161, 175, 197, 255, 256, 257, 258, 259, 260),-1]
names(FoodPrice) <- c('country',1990:2014)

FoodPrice<-melt(FoodPrice, id.vars=c('country'),var='year')
FoodPrice$year <- as.numeric(as.character(FoodPrice$year))
FoodPrice1 <- filter(FoodPrice, year>1999)
names(FoodPrice)[3]<-paste('food_price') 

#####

FoodProduction <- read.csv(file="FoodProduction.csv", header=FALSE, stringsAsFactors=FALSE)
FoodProduction <- FoodProduction[-c(1, 2, 3, 4, 5, 6, 7, 14, 15, 16, 38, 54, 72, 73, 82, 88, 99, 111, 125, 126, 151, 152, 161, 175, 197, 255, 256, 257, 258, 259, 260),-1]
names(FoodProduction) <- c('country',1992:2013)

FoodProduction<-melt(FoodProduction, id.vars=c('country'),var='year')
names(FoodProduction)[3]<-paste('food_production') 
FoodProduction$year <- as.numeric(as.character(FoodProduction$year))

#####

GDP <- read.csv(file="GDP.csv", header=FALSE, stringsAsFactors=FALSE)
GDP <- GDP[-c(1, 2, 3, 4, 5, 6, 7, 14, 15, 16, 38, 54, 72, 73, 82, 88, 99, 111, 125, 126, 151, 152, 161, 175, 197, 255, 256, 257, 258, 259, 260),-1]
names(GDP) <- c('country',1990:2014)

GDP<-melt(GDP, id.vars=c('country'),var='year')
names(GDP)[3]<-paste('gdp') 
GDP$year <- as.numeric(as.character(GDP$year))

#####

Protein <- read.csv(file="Protein.csv", header=FALSE, stringsAsFactors=FALSE)
Protein <- Protein[-c(1, 2, 3, 4, 5, 6, 7, 14, 15, 16, 38, 54, 72, 73, 82, 88, 99, 111, 125, 126, 151, 152, 161, 175, 197, 255, 256, 257, 258, 259, 260),-1]
names(Protein) <- c('country',1992:2011)

Protein<-melt(Protein, id.vars=c('country'),var='year')
names(Protein)[3]<-paste('protein') 
Protein$year <- as.numeric(as.character(Protein$year))

#####

ProteinAnimal <- read.csv(file="ProteinAnimal.csv", header=FALSE, stringsAsFactors=FALSE)
ProteinAnimal <- ProteinAnimal[-c(1, 2, 3, 4, 5, 6, 7, 14, 15, 16, 38, 54, 72, 73, 82, 88, 99, 111, 125, 126, 151, 152, 161, 175, 197, 255, 256, 257, 258, 259, 260),-1]
names(ProteinAnimal) <- c('country',1992:2011)

ProteinAnimal<-melt(ProteinAnimal, id.vars=c('country'),var='year')
names(ProteinAnimal)[3]<-paste('protein_animal') 
ProteinAnimal$year <- as.numeric(as.character(ProteinAnimal$year))

#####

Undernourish <- read.csv(file="Undernourish.csv", header=FALSE, stringsAsFactors=FALSE)
Undernourish <- Undernourish[-c(1, 2, 3, 4, 5, 6, 7, 14, 15, 16, 38, 54, 72, 73, 82, 88, 99, 111, 125, 126, 151, 152, 161, 175, 197, 255, 256, 257, 258, 259, 260),-1]
names(Undernourish) <- c('country',1992:2016)

Undernourish<-melt(Undernourish, id.vars=c('country'),var='year')
names(Undernourish)[3]<-paste('undernourish') 
Undernourish$year <- as.numeric(as.character(Undernourish$year))

#####

FoodPriceVolatility <- read.csv(file="FoodPriceVolatility.csv", header=FALSE, stringsAsFactors=FALSE)
FoodPriceVolatility <- FoodPriceVolatility[-c(1, 2, 3, 4, 5, 6, 7, 14, 15, 16, 38, 54, 72, 73, 82, 88, 99, 111, 125, 126, 151, 152, 161, 175, 197, 255, 256, 257, 258, 259, 260),-1]
names(FoodPriceVolatility) <- c('country',1990:2014)

FoodPriceVolatility<-melt(FoodPriceVolatility, id.vars=c('country'),var='year')
names(FoodPriceVolatility)[3]<-paste('food_price_volatility') 
FoodPriceVolatility$year <- as.numeric(as.character(FoodPriceVolatility$year))
FoodPriceVolatility <- filter(FoodPriceVolatility, year>1999)

#####

FoodProductionVariability <- read.csv(file="FoodProductionVariability.csv", header=FALSE, stringsAsFactors=FALSE)
FoodProductionVariability <- FoodProductionVariability[-c(1, 2, 3, 4, 5, 6, 7, 14, 15, 16, 38, 54, 72, 73, 82, 88, 99, 111, 125, 126, 151, 152, 161, 175, 197, 255, 256, 257, 258, 259, 260),-1]
names(FoodProductionVariability) <- c('country',1990:2013)

FoodProductionVariability<-melt(FoodProductionVariability, id.vars=c('country'),var='year')
names(FoodProductionVariability)[3]<-paste('food_production_variability') 
FoodProductionVariability$year <- as.numeric(as.character(FoodProductionVariability$year))

#####

FoodSupplyVariability <- read.csv(file="FoodSupplyVariability.csv", header=FALSE, stringsAsFactors=FALSE)
FoodSupplyVariability <- FoodSupplyVariability[-c(1, 2, 3, 4, 5, 6, 7, 14, 15, 16, 38, 54, 72, 73, 82, 88, 99, 111, 125, 126, 151, 152, 161, 175, 197, 255, 256, 257, 258, 259, 260),-1]
names(FoodSupplyVariability) <- c('country',1990:2011)

FoodSupplyVariability<-melt(FoodSupplyVariability, id.vars=c('country'),var='year')
FoodSupplyVariability$year <- as.numeric(as.character(FoodSupplyVariability$year))
names(FoodSupplyVariability)[3]<-paste('food_supply_variability') 

#####

TotalPopulation <- read.csv(file="TotalPopulation.csv", header=FALSE, stringsAsFactors=FALSE)
TotalPopulation <- TotalPopulation[-c(1, 2, 3, 4, 5, 6, 7, 14, 15, 16, 38, 54, 72, 73, 82, 88, 99, 111, 125, 126, 151, 152, 161, 175, 197, 255, 256, 257, 258, 259, 260),-1]
names(TotalPopulation) <- c('country',1992:2016)

TotalPopulation<-melt(TotalPopulation, id.vars=c('country'),var='year')
names(TotalPopulation)[3]<-paste('total_population') 
TotalPopulation$year <- as.numeric(as.character(TotalPopulation$year))

#####

NumberUndernourished <- read.csv(file="NumberUndernourished.csv", header=FALSE, stringsAsFactors=FALSE)
NumberUndernourished <- NumberUndernourished[-c(1, 2, 3, 4, 5, 6, 7, 14, 15, 16, 38, 54, 72, 73, 82, 88, 99, 111, 125, 126, 151, 152, 161, 175, 197, 255, 256, 257, 258, 259, 260),-1]
names(NumberUndernourished) <- c('country',1992:2016)

NumberUndernourished<-melt(NumberUndernourished, id.vars=c('country'),var='year')
names(NumberUndernourished)[3]<-paste('number_undernourished') 
NumberUndernourished$year <- as.numeric(as.character(NumberUndernourished$year))

#####

DietaryEnergySupply <- read.csv(file="DietaryEnergySupply.csv", header=FALSE, stringsAsFactors=FALSE)
DietaryEnergySupply <- DietaryEnergySupply[-c(1, 2, 3, 4, 5, 6, 7, 14, 15, 16, 38, 54, 72, 73, 82, 88, 99, 111, 125, 126, 151, 152, 161, 175, 197, 255, 256, 257, 258, 259, 260),-1]
names(DietaryEnergySupply) <- c('country',1992:2016)

DietaryEnergySupply<-melt(DietaryEnergySupply, id.vars=c('country'),var='year')
names(DietaryEnergySupply)[3]<-paste('dietary_energy_supply') 
DietaryEnergySupply$year <- as.numeric(as.character(DietaryEnergySupply$year))

#####

FatSupply <- read.csv(file="FatSupply.csv", header=FALSE, stringsAsFactors=FALSE)
FatSupply <- FatSupply[-c(1, 2, 3, 4, 5, 6, 7, 14, 15, 16, 38, 54, 72, 73, 82, 88, 99, 111, 125, 126, 151, 152, 161, 175, 197, 255, 256, 257, 258, 259, 260),-1]
names(FatSupply) <- c('country',1992:2011)

FatSupply<-melt(FatSupply, id.vars=c('country'),var='year')
names(FatSupply)[3]<-paste('fat_supply') 
FatSupply$year <- as.numeric(as.character(FatSupply$year))

#####

FoodOverAcquisition <- read.csv(file="FoodOverAcquisition.csv", header=FALSE, stringsAsFactors=FALSE)
FoodOverAcquisition <- FoodOverAcquisition[-c(1, 2, 3, 4, 5, 6, 7, 14, 15, 16, 38, 54, 72, 73, 82, 88, 99, 111, 125, 126, 151, 152, 161, 175, 197, 255, 256, 257, 258, 259, 260),-1]
names(FoodOverAcquisition) <- c('country',1992:2016)

FoodOverAcquisition<-melt(FoodOverAcquisition, id.vars=c('country'),var='year')
names(FoodOverAcquisition)[3]<-paste('food_over_acquisition') 
FoodOverAcquisition$year <- as.numeric(as.character(FoodOverAcquisition$year))

#############

# join variables into one data frame, write .csv

FS1 <- dplyr::full_join(EnergyAdequacy, FoodDeficit,  by = c("country", "year"))
FS2 <- dplyr::full_join(FS1, FoodPrice,  by = c("country", "year"))
FS3 <- dplyr::full_join(FS2, FoodProduction,  by = c("country", "year"))
FS4 <- dplyr::full_join(FS3, GDP,  by = c("country", "year"))
FS5 <- dplyr::full_join(FS4, Protein,  by = c("country", "year"))
FS6 <- dplyr::full_join(FS5, ProteinAnimal,  by = c("country", "year"))
FS7 <- dplyr::full_join(FS6, Undernourish,  by = c("country", "year"))
FS8 <- dplyr::full_join(FS7, FoodPriceVolatility,  by = c("country", "year"))
FS9 <- dplyr::full_join(FS8, FoodSupplyVariability,  by = c("country", "year"))
FS10 <- dplyr::full_join(FS9, TotalPopulation,  by = c("country", "year"))
FS11 <- dplyr::full_join(FS10, NumberUndernourished,  by = c("country", "year"))
FS12 <- dplyr::full_join(FS11, DietaryEnergySupply,  by = c("country", "year"))
FS13 <- dplyr::full_join(FS12, FatSupply,  by = c("country", "year"))
FS14 <- dplyr::full_join(FS13, FoodOverAcquisition,  by = c("country", "year"))
FS15 <- dplyr::full_join(FS14, FoodProductionVariability,  by = c("country", "year"))

FoodSecurityDF <- as.data.frame(FS15)

FoodSecurity <- dplyr::filter(FoodSecurityDF, country !="") # drop extra rows with no data

write.csv(FoodSecurity, file="FOODSECURITY")


