### Full Dataset Construction ###

## Load packages ##
library(reshape2)
library(plyr)
library(tidyverse)


#### Set working directory ####
# all raw files should be in this directory for script to run #
WD <- getwd()
setwd(paste0(WD,"/master dataset construction"))
#### IMPORT RAW DATA ####

# Raw aquaculture production from FishStat
Production <- read.csv("AQ_production.csv", stringsAsFactors=FALSE)
# Raw fisheries production from FishStat
Catch <- read.csv("F_production.csv", stringsAsFactors=FALSE)
# Trade data in quantity and value #
Trade.Q <- read.csv("Trade_Q.csv", stringsAsFactors=FALSE)
Trade.V <- read.csv("Trade_V.csv", stringsAsFactors=FALSE)  
# Population #
Pop <- read.csv("Pop.csv", stringsAsFactors=FALSE)
# GDP #
GDP <- read.csv("GDP.csv", stringsAsFactors=FALSE)
# Doing Business Indicator #
DBI <- read.csv("DBI.csv", stringsAsFactors=FALSE)
# Good governance metric #
GOV <- read.csv("Gov.csv", stringsAsFactors=FALSE)
# Nutrition and seafood reliance data
## data source: GENus, Planetary Health Alliance, Harvard (http://planetaryhealthalliance.org/genus) and FAO
nutr_reliance <- read.csv(paste0("FOOD SECURITY and NUTRIENT INTAKES.csv"),stringsAsFactors = F)

#### COUNTRY NAME CONVERSION ####
# We have to make sure all data have the same identifiers (especially consistent country IDs) ##
## We do this manually by finding the full list of unique character names in the datasets##
## Then, assigning a number to each unique country manually, accounting for spelling diffs etc. ##
## Later, master country names will be re-assigned, once data are joined by ID # ##

# first, all datasets have a country column. find unique country names for each #
find_names <- function(dataset) sort(unique(dataset$country))
raw.all <- list(Production,Catch,Trade.Q,Trade.V,Pop,GDP,DBI,GOV,nutr_reliance)

# apply name-finding function, coerce to character vector of ALL unique country names in all data #
names.all <- unlist(purrr::map(raw.all,find_names))
names.all <- data_frame(name=sort(unique(names.all)),country_ID=numeric(length=length(name)))

# manually look at the names and assign unique number to each country #
# accounting for spelling differences, specical characters etc. #
write.csv(names.all,"unique_country_names.csv")

# import ID conversion key #
name_conversion <- read.csv("country_name_conversion.csv",stringsAsFactors = F)

# function takes a dataset, finds and applies unique numeric ID #
assign_ID <- function(dataset) {
  names <- data.frame(name=sort(unique(dataset$country)))
  IDs <- suppressWarnings(left_join(names,name_conversion))
  newdat <- suppressWarnings(left_join(dataset,IDs,by=c("country"="name")))
  return(newdat)
}

# Assign country IDs to datasets #
# We will use these to join data later #
Production <- assign_ID(Production)
Catch <- assign_ID(Catch)
Trade.Q <- assign_ID(Trade.Q)
Trade.V <- assign_ID(Trade.V)
Pop <- assign_ID(Pop)
GDP <- assign_ID(GDP)
DBI <- assign_ID(DBI)
GOV <- assign_ID(GOV)
nutr_reliance <- assign_ID(nutr_reliance)

#### DATA CLEANING AND JOINING ####
#### ECONOMIC VARIABLES ####

#### Fisheries and aquaculture production ####
# Remove freshwater species and remove unneeded columns
Production <- subset(Production, Production[ , 4] != "Freshwater")
Production[,3:5]<- NULL

Catch[,3:4]<- NULL

# Flip databases to long form
Production<-melt(Production, id.vars=c('country','country_ID','species'),var='year')
names(Production)[5]<-paste('Aq.prod') # in tons 
Production$year<-as.numeric(gsub( "X", "", as.character(Production$year)))

Catch<-melt(Catch, id.vars=c('country','country_ID','species'),var='year')
names(Catch)[5]<-paste("F.prod") 
Catch$year<-as.numeric(gsub( "X", "", as.character(Catch$year)))

# Aggregate production by year for aquaculture and fisheries

Production <- ddply(Production, .(year,country,country_ID),
                    summarize, 
                    Aq.prod = sum(Aq.prod,na.rm=TRUE))

Catch <- ddply(Catch, .(year,country,country_ID),
               summarize, 
               F.prod = sum(F.prod,na.rm=TRUE))

# Leave only observations from 2011

Production<-Production[Production$year==2011,]

Catch<-Catch[Catch$year==2011,]

# Remove country name and merge production and catch by country ID and year
Production <- select(Production,-country)
Catch <- select(Catch,-country)

ECON <- merge(Production, Catch,by.x=c('country_ID','year'),by.y=c('country_ID','year'),all=TRUE)

##### Trade ####

# Remove unneeded columns
Trade.Q <- select(Trade.Q,-country,-Unit)
Trade.V <- select(Trade.V,-country,-unit)

# Flip databases to long form
Trade.Q<-melt(Trade.Q, id.vars=c('country_ID','commodity','flow'),var='year')
names(Trade.Q)[5]<-paste('Trade.quantity')
Trade.Q$year<-as.numeric(gsub( "X", "", as.character(Trade.Q$year)))

Trade.V<-melt(Trade.V, id.vars=c('country_ID','commodity','flow'),var='year')
names(Trade.V)[5]<-paste('Trade.value') 
Trade.V$year<-as.numeric(gsub( "X", "", as.character(Trade.V$year)))

# Aggregate trade by country, year, and flow (imports versus exports)

Trade.Q <- ddply(Trade.Q, .(year,country_ID,flow),
                 summarize, 
                 Trade.quantity = sum(Trade.quantity,na.rm=TRUE))

Trade.V <- ddply(Trade.V, .(year,country_ID,flow),
                 summarize, 
                 Trade.value = sum(Trade.value,na.rm=TRUE))

# Exports in quantity and value #
Exp.Q <- Trade.Q[Trade.Q$flow=='Exports' | Trade.Q$flow=='Reexports',]
Exp.Q <- ddply(Exp.Q, .(year,country_ID),
               summarize, 
               Q.Exp = sum(Trade.quantity,na.rm=TRUE))

Exp.V <- Trade.V[Trade.V$flow=='Exports' | Trade.V$flow=='Reexports',]
Exp.V <- ddply(Exp.V, .(year,country_ID),
               summarize, 
               V.Exp = sum(Trade.value,na.rm=TRUE))

# Imports in quantity and value #
Imp.Q <- Trade.Q[Trade.Q$flow=='Imports',]
names(Imp.Q)[4] <- paste('Q.imp')
Imp.Q[3] <- NULL
Imp.V <- Trade.V[Trade.V$flow=='Imports',]
names(Imp.V)[4] <- paste('V.imp')
Imp.V[3] <- NULL

# Leave only observations from 2011
Exp.Q <- Exp.Q[Exp.Q$year==2011,]
Exp.V <- Exp.V[Exp.V$year==2011,]

Imp.Q <- Imp.Q[Imp.Q$year==2011,]
Imp.V <- Imp.V[Imp.V$year==2011,]

# Merge with the master database by country and year

ECON <- merge(ECON, Exp.Q,by.x=c('country_ID','year'),by.y=c('country_ID','year'),all=TRUE)
ECON <- merge(ECON, Exp.V,by.x=c('country_ID','year'),by.y=c('country_ID','year'),all=TRUE)

ECON <- merge(ECON, Imp.Q,by.x=c('country_ID','year'),by.y=c('country_ID','year'),all=TRUE)
ECON <- merge(ECON, Imp.V,by.x=c('country_ID','year'),by.y=c('country_ID','year'),all=TRUE)


#### Population ####
# remove country name and flip to long form
Pop <- select(Pop,-country) %>% melt(id.vars=c('country_ID'),var='year',all=TRUE)
names(Pop)[3] <- paste('Population') 
Pop$year<-as.numeric(gsub( "X", "", as.character(Pop$year)))

# Pull out 2011 data #
Pop <- Pop[Pop$year==2011,]

# Join with master database by country and year
ECON <- merge(ECON, Pop,by.x=c('country_ID','year'),by.y=c('country_ID','year'),all=TRUE)

#### GDP ####
# flip to long form #
GDP <- select(GDP,-country) %>% melt(id.vars=c('country_ID'),var='year')
names(GDP)[3] <- paste('GDP') 
GDP$year<-as.numeric(gsub( "X", "", as.character(GDP$year)))

# pull out 2011 data #
GDP <- GDP[GDP$year==2011,]

# join with master database by country and year #
ECON <- merge(ECON, GDP,by.x=c('country_ID','year'),by.y=c('country_ID','year'),all=TRUE)


#### Calculate economic metrics ####

# production ratio aquaculture divided by fisheries
ECON$gross.production.ratio <- ECON$Aq.prod/ECON$F.prod   

# trade balance in quantity, exports divided by imports
ECON$Q.trade.deficit <- ECON$Q.Exp/ECON$Q.imp

# trade balance in value, exports divided by imports
ECON$V.trade.deficit <- ECON$V.Exp/ECON$V.imp

# "willingness to pay", equal to GDP divided by total value of imports
ECON$willingness <- ECON$GDP/ECON$V.imp

# GDP per capita #
ECON$gdppc <- ECON$GDP/ECON$Population

#### Other databases  ####

# Doing Business metric for 2011 #
DBI <- DBI %>% 
  distinct(country_ID,year,.keep_all=TRUE) %>% 
  select(-country) %>% 
  filter(year==2011)

# Good governance metric #
# remove country name and flip to long form #
GOV <- GOV %>% 
  select(-country) %>% 
  melt(id.vars=c('country_ID','indicator'),var='year')
GOV$year<-as.numeric(gsub( "X", "", as.character(GOV$year)))
GOV <- GOV[GOV$year==2011,]
# switch back to indicators as columns
GOV <- dcast(GOV,country_ID~indicator)
names(GOV) <- c("country_ID","corruption","gov_effectiveness","pol_stab","reg_qual","rule_law","voice")

# Join DBI and GOV to master database by country and year
ECON <- merge(ECON,DBI,by.x=c('country_ID','year'),by.y=c('country_ID','year'),all=TRUE)
ECON <- merge(ECON,GOV,by.x=c('country_ID'),by.y=c('country_ID'),all=TRUE)

#### NUTRITION ####
# select country, energy adequacy, protein, fatty acids, vitamin A, zinc, and iron
NUTRITION <- nutr_reliance %>% 
  select(country_ID, energy_adequacy,protein,polyunsatFA,vitaminA,zinc,iron)

# remove records for which all variables data are missing (except country ID)
nvars <- ncol(NUTRITION) - 1
NUTRITION <- NUTRITION[rowSums(is.na(NUTRITION))!=nvars,]


#### SEAFOOD RELIANCE ####
# uses same raw data as the food security metrics #

# select country and reliance metrics from the data #
RELIANCE <- nutr_reliance %>% 
  select(country_ID, calories_percentseafood,protein_percentseafood,polyunsatFA_percentseafood,
         vitaminA_percentseafood,zinc_percentseafood,iron_percentseafood)

# remove records for which all variables data are missing (except country ID)
nvars <- ncol(RELIANCE) - 1
RELIANCE <- RELIANCE[rowSums(is.na(RELIANCE))!=nvars,]

#### JOIN DATASETS ####
# Here, we join the data together again, and add a country name #
FULLDAT <- inner_join(ECON,NUTRITION,by=c("country_ID"))
FULLDAT <- inner_join(FULLDAT,RELIANCE,by=c("country_ID"))

# add back a country name. Also includes IUCN region #
# region2 variable has fewer categories (for plotting)
region2 <- function(x) {
  out <- x
  out[out=="North Asia"] <-"Asia"
  out[out=="East Asia"] <- "Asia"
  out[out=="West & Central Asia"] <- "Asia"
  out[out=="Sub-Saharan Africa"] <- "Africa"
  out[out=="North Africa"] <- "Africa"
  out[out=="Mesoamerica"] <- "Latin America and Caribbean"
  out[out=="South America"] <- "Latin America and Caribbean"
  out[out=="Caribbean Islands"] <- "Latin America and Caribbean"
  out[out=="Oceania"] <- "South East Asia and Oceania"
  out[out=="South & South East Asia"] <- "South East Asia and Oceania"
  return(out)
}
master_names <- read.csv("master_names.csv",stringsAsFactors = F)
FULLDAT <- FULLDAT %>% left_join(master_names) %>% 
  dplyr::rename(country_name=master) %>%
  mutate(region2=region2(region))

#### REMOVE LANDLOCKED COUNTRIES ####
FULLDAT <- FULLDAT [ ! FULLDAT$country_name %in% c("Afghanistan", "Andorra","Armenia","Austria",
                                              "Azerbaijan","Belarus","Bhutan","Bolivia","Botswana",
                                              "Burkina Faso", "Burundi", "Central African Republic",
                                              "Chad","Czech Republic","Czechoslovakia","Ethiopia","Ethiopia PDR",
                                              "Hungary","Kazakhstan","Kosovo","Kyrgyzstan","Laos","Lesotho",
                                              "Liechtenstein","Luxembourg","Macedonia","Malawi","Mali",
                                              "Moldova","Mongolia","Nepal","Niger","Paraguay","Rwanda",
                                              "San Marino","Serbia","Serbia and Montenegro","Slovakia",
                                              "South Ossetia","South Sudan","Swaziland","Switzerland",
                                              "Tajikistan","Turkmenistan","Uganda","Uzbekistan","Vatican City",
                                              "Zambia","Zimbabwe","Southern Africa","Southern Asia","Eastern Asia"),]
#### REORDER VARIABLES ####
# We only use some of the variables for scoring #
# The others, we put as the first columns of the dataset #
# Then, the econ, nutrition, and reliance variables from left to right #
FULLDAT <- FULLDAT %>% select(country_name,country_ID,region,regions2,year,Population,GDP,
                              Aq.prod:V.imp,DTF:`Voice and accountability`,
                              gross.production.ratio:gdppc,energy_adequacy:iron_percentseafood)

#### NORMALIZE VARIABLES ####
# for most of the variables, we normalize
# to 80th percentile. This means that we'll divide everything by the *80th* percentile country in the data. 
# All countries above this cutoff get forced to 1.

norm_90 <- function(variable) {
  quant90 <- quantile(variable,probs=.8,na.rm=T)
  out <- variable/quant90
  out[out>1] <- 1
  return(out)
}


# Add new, normalized columns for all variables (except those that have already been normalized)
FULLDAT.NORM <- FULLDAT %>%
  mutate_at(vars(gross.production.ratio:iron_percentseafood),funs("norm"=norm_90))

#### CALCULATE AGGREGATE SCORES FOR THREE CATEGORIES ####
# Add aggregated scores for each category, based on 80th percentile normalized scores. 
# For these scores, disregard NA values in individual vars 
# (although this is important to note)

FULLDAT.NORM$mean_econ <- rowMeans(select(FULLDAT.NORM,gross.production.ratio_norm:gdppc_norm),na.rm=T)
FULLDAT.NORM$mean_nutrition <- rowMeans(select(FULLDAT.NORM,energy_adequacy_norm:iron_norm),na.rm=T)
FULLDAT.NORM$mean_reliance <- rowMeans(select(FULLDAT.NORM,calories_percentseafood_norm:iron_percentseafood_norm),na.rm=T)


## FOR NUTRITION, RESCALE TO 0-1. BECAUSE CALORIES, ETC. DON'T SCALE DOWN TO ZERO
FULLDAT.NORM$mean_nutrition <- (FULLDAT.NORM$mean_nutrition
                                -min(FULLDAT.NORM$mean_nutrition,na.rm=T))/(max(FULLDAT.NORM$mean_nutrition,na.rm=T)-
                                                                              min(FULLDAT.NORM$mean_nutrition,na.rm=T))


#### FINAL SCORES ####
## for econ and nutrition, the opportunity score is actually one minus the calculated score above
# Include a geometric mean of reliance and malnutrition, for visualization's sake (not used for final score) #
FULLDAT.NORM <- FULLDAT.NORM %>% mutate(econ_opportunity=1-mean_econ,mean_malnutrition=1-mean_nutrition,
                                        reliance_mal=sqrt(mean_reliance*mean_malnutrition))

# Final score
FULLDAT.NORM$mariculture_opportunity <- rowMeans(select(FULLDAT.NORM,mean_reliance,econ_opportunity,mean_malnutrition))

### write the data ####
write.csv(FULLDAT.NORM,file="final_country_scores.csv",row.names=F)

#### VISUALIZATION ####
require(ggplot2)

# table of final metrics #
finalmetrics <- select(FULLDAT.NORM,country_name,mean_reliance,econ_opportunity:mariculture_opportunity) %>%
  arrange(desc(mariculture_opportunity))

glimpse(finalmetrics)

## Number of NA values in each variables ##
NA_count <- FULLDAT.NORM %>% summarise_all(funs(sum(is.na(.))))

#### Biplots of final scores ####
# econ vs. malnutrition
econ_mal <-ggplot(FULLDAT.NORM, aes(x=econ_opportunity,y=mean_malnutrition)) + 
  geom_point(aes(col=region2,size=mean_reliance))

# mal vs. reliance
mal_rel <- ggplot(FULLDAT.NORM, aes(x=mean_reliance,y=mean_malnutrition)) + 
  geom_point(aes(col=region2),size=3)

# econ vs. (reliance*malnutrition)
econ_rel_mal <- ggplot(FULLDAT.NORM, aes(x=econ_opportunity,y=reliance_mal)) + 
  geom_point(aes(col=region2),size=3)

### For Presentation ###
library(extrafont)
library(ggplot2)
library(ggthemes)
pal <- c("#9E0142", "#D53E4F", "#F46D43","#66C2A5", "#3288BD", "#5E4FA2")

kobe2 <- econ_rel_mal+
  coord_fixed(xlim=c(0,1),ylim=c(0,1))+
  geom_hline(yintercept=0.5,linetype=2)+
  geom_vline(xintercept = 0.5,linetype=2)+
  ggtitle("Global Mariculture Opportunity")+
  xlab("Economic Opportunity")+
  ylab("Reliance and Nutrition Opportunity")+
  scale_color_manual(values = pal,name="")+
  theme_few()+
  theme(legend.position = "right",
        text = element_text(size = 18,color="black",family="Rockwell"),
        panel.border = element_blank(),
        legend.key = element_blank(),
        legend.title=element_blank(),
        plot.title = element_text(
          colour = "black",
          size = 20,
          hjust = 0))
kobe2

kobe3 <- mal_rel+
  # geom_smooth(aes(color=region2),method="lm",se=F)+
  coord_fixed(xlim=c(0,1),ylim=c(0,1))+
  geom_hline(yintercept=0.5,linetype=2)+
  geom_vline(xintercept = 0.5,linetype=2)+
  ylab("Nutrition Opportunity")+
  xlab("Reliance on Seafood")+
  scale_color_manual(values = pal,name="")+
  theme_few()+
  theme(legend.position = "right",
        text = element_text(size = 18,color="black",family="Rockwell"),
        panel.border = element_blank(),
        legend.key = element_blank(),
        legend.title=element_blank())
kobe3
ggsave("mal_rel.png",plot=kobe3,width=8)
### Econ, Mal, Reliance as size

econ_mal2 <- econ_mal+
  coord_fixed(xlim=c(0,1),ylim=c(0,1))+
  geom_hline(yintercept=0.5,linetype=2)+
  geom_vline(xintercept = 0.5,linetype=2)+
  xlab("Economic Opportunity")+
  ylab("Nutrition Opportunity")+
  scale_size_continuous(name="Reliance on Seafood")+
  scale_color_manual(values = pal,name="")+
  guides(color=guide_legend(override.aes = list(size=3,linetype=0)))+
  theme_few()+
  theme(legend.position = "right",
        text = element_text(size = 18,color="black",family="Rockwell"),
        panel.border = element_blank(),
        legend.key = element_blank(),
        plot.background = element_rect(linetype = 0))

econ_mal2
# ggsave("econ_mal.png", plot=econ_mal2,  width=8)

#blank plot (for presentations)
blank_kobe <-ggplot(FULLDAT.NORM, aes(x=econ_opportunity,y=mean_malnutrition)) +
  geom_blank(aes(col=region2,size=mean_reliance))
econ_mal3 <- blank_kobe+
  coord_fixed(xlim=c(0,1),ylim=c(0,1))+
  geom_hline(yintercept=0.5,linetype=2)+
  geom_vline(xintercept = 0.5,linetype=2)+
  xlab("Economic Opportunity")+
  ylab("Nutrition Opportunity")+
  scale_size_continuous(name="Reliance on Seafood")+
  scale_color_manual(values = pal,name="")+
  guides(color=guide_legend(override.aes = list(size=3,linetype=0)))+
  theme_few()+
  theme(legend.position = "right",
        text = element_text(size = 24,color="black",family="Rockwell"),
        panel.border = element_blank(),
        legend.key = element_blank(),
        plot.background = element_rect(linetype = 0),
        plot.title = element_text(
          colour = "black",
          size = 20,
          hjust = -3))
econ_mal3
# ggsave("kobe_blank.png", plot=econ_mal3,  width=8)

#### Plots Relative to Economic Indicators ####

# Political stability vs. mariculture opportunity
polstab_opportunity <- ggplot(FULLDAT.NORM, aes(x=pol_stab,y=mariculture_opportunity))+
  ylim(0,1)+
  geom_point(size=2)+
  xlab("Political Stability")+
  ylab("Mariculture Opportunity")+
  geom_smooth(method="lm",se=T,col="darkgreen",linetype=2)+
  theme_few()+
  theme(legend.position = "right",
        text = element_text(size = 18,color="black",family="Rockwell"),
        panel.border = element_blank(),
        legend.key = element_blank(),
        plot.background = element_rect(linetype = 0),
        plot.title = element_text(
          colour = "black",
          size = 20,
          hjust = -3))
polstab_opportunity

# Control of corruption vs. mariculture opportunity
corruption_opportunity <- ggplot(FULLDAT.NORM, aes(x=corruption,y=mariculture_opportunity))+
  ylim(0,1)+
  geom_point(size=2)+
  xlab("Corruption")+
  ylab("Mariculture Opportunity")+
  geom_smooth(method="lm",se=T,col="darkgreen",linetype=2)+
  theme_few()+
  theme(legend.position = "right",
        text = element_text(size = 18,color="black",family="Rockwell"),
        panel.border = element_blank(),
        legend.key = element_blank(),
        plot.background = element_rect(linetype = 0),
        plot.title = element_text(
          colour = "black",
          size = 20,
          hjust = -3))
corruption_opportunity

# rule of law vs. mariculture opportunity
law_opportunity <- ggplot(FULLDAT.NORM, aes(x=rule_law,y=mariculture_opportunity))+
  ylim(0,1)+
  geom_point(size=2)+
  xlab("Rule of Law")+
  ylab("Mariculture Opportunity")+
  geom_smooth(method="lm",se=T,col="darkgreen",linetype=2)+
  theme_few()+
  theme(legend.position = "right",
        text = element_text(size = 18,color="black",family="Rockwell"),
        panel.border = element_blank(),
        legend.key = element_blank(),
        plot.background = element_rect(linetype = 0),
        plot.title = element_text(
          colour = "black",
          size = 20,
          hjust = -3))
law_opportunity

#government effectiveness vs mariculture opportunity
gov_opportunity <- ggplot(FULLDAT.NORM, aes(x=gov_effectiveness,y=mariculture_opportunity))+
  ylim(0,1)+
  geom_point(size=2)+
  xlab("Government Effectiveness")+
  ylab("Mariculture Opportunity")+
  geom_smooth(method="lm",se=T,col="darkgreen",linetype=2)+
  theme_few()+
  theme(legend.position = "right",
        text = element_text(size = 18,color="black",family="Rockwell"),
        panel.border = element_blank(),
        legend.key = element_blank(),
        plot.background = element_rect(linetype = 0),
        plot.title = element_text(
          colour = "black",
          size = 20,
          hjust = -3))
gov_opportunity

# voice and accountability vs. mariculture opportunity
voice_opportunity <- ggplot(FULLDAT.NORM, aes(x=voice,y=mariculture_opportunity))+
  ylim(0,1)+
  geom_point(size=2)+
  xlab("Voice and Accountability")+
  ylab("Mariculture Opportunity")+
  geom_smooth(method="lm",se=T,col="darkgreen",linetype=2)+
  theme_few()+
  theme(legend.position = "right",
        text = element_text(size = 18,color="black",family="Rockwell"),
        panel.border = element_blank(),
        legend.key = element_blank(),
        plot.background = element_rect(linetype = 0),
        plot.title = element_text(
          colour = "black",
          size = 20,
          hjust = -3))
voice_opportunity

# voice and accountability vs. mariculture opportunity
dbi_opportunity <- ggplot(FULLDAT.NORM, aes(x=DTF,y=mariculture_opportunity))+
  ylim(0,1)+
  geom_point(size=2)+
  xlab("Doing Business Indicator")+
  ylab("Mariculture Opportunity")+
  geom_smooth(method="lm",se=T,col="darkgreen",linetype=2)+
  theme_few()+
  theme(legend.position = "right",
        text = element_text(size = 18,color="black",family="Rockwell"),
        panel.border = element_blank(),
        legend.key = element_blank(),
        plot.background = element_rect(linetype = 0),
        plot.title = element_text(
          colour = "black",
          size = 20,
          hjust = -3))
dbi_opportunity

# significant?
summary(lm(mariculture_opportunity~DTF+voice+gov_effectiveness+rule_law+corruption+pol_stab,data=FULLDAT.NORM))
summary(lm(mariculture_opportunity~pol_stab,data=FULLDAT.NORM))
summary(lm(mariculture_opportunity~gov_effectiveness,data=FULLDAT.NORM))
summary(lm(mariculture_opportunity~voice,data=FULLDAT.NORM))
summary(lm(mariculture_opportunity~rule_law,data=FULLDAT.NORM))
summary(lm(mariculture_opportunity~corruption,data=FULLDAT.NORM))
summary(lm(mariculture_opportunity~DTF,data=FULLDAT.NORM))



#### Sensitivity Analyses ####
## Remove one variable at a time and look at how the mean and SD changes for the three scores ##
# Function to remove and re-calculate #

# variables to test, and categories of variables
testvars <- FULLDAT.NORM %>% 
  select(country_name,contains("_norm")) %>% names()
testvars <- testvars[-(7:13)]
econvars <- testvars[2:6]
nutvars <- testvars[7:12]
reliancevars <- testvars[13:18]

# scores calc
calc_scores <- function(dat) {
  out<-dat
  
  # variables of interest
  econvars <- names(out)[names(out) %in% econvars]
  nutvars <- names(out)[names(out) %in% nutvars]
  reliancevars <- names(out)[names(out) %in% reliancevars]
  
  # recalculate scores
  out$mean_econ <- rowMeans(select(out,one_of(econvars)),na.rm=T)
  out$mean_nutrition <- rowMeans(select(out,one_of(nutvars)),na.rm=T)
  # rescale nutrition
  out$mean_nutrition <- (out$mean_nutrition
                                  -min(out$mean_nutrition,na.rm=T))/(max(out$mean_nutrition,na.rm=T)-
                                                                                min(out$mean_nutrition,na.rm=T))
  out$mean_reliance <- rowMeans(select(out,one_of(reliancevars)),na.rm=T)
  
  out <- out %>% mutate(econ_opportunity=1-mean_econ,mean_malnutrition=1-mean_nutrition)
  out$mariculture_opportunity <- rowMeans(select(out,mean_reliance,econ_opportunity,mean_malnutrition))
  return(out)
}

# calculate differences from full model
calc_diffs <- function(dat.full, variable) {
  # remove variable from full data and recalculate scores
  dat.reduced <- dat.full %>% select(-contains(variable)) %>% calc_scores()
  
  # for each aggregate score, calculate mean change across countries
  econdiff <- dat.reduced$econ_opportunity - dat.full$econ_opportunity
  nutdiff <- dat.reduced$mean_malnutrition - dat.full$mean_malnutrition
  reliancediff <- dat.reduced$mean_reliance - dat.full$mean_reliance
  oppdiff <- dat.reduced$mariculture_opportunity-dat.full$mariculture_opportunity
  
  # Return mean and sd of changes
  out <- data_frame(variable=variable, 
                    delta_econ=mean(econdiff,na.rm=T),sd_econ=sd(econdiff,na.rm=T),
                    delta_nut=mean(nutdiff,na.rm=T),sd_nut=sd(nutdiff,na.rm=T),
                    delta_rel=mean(reliancediff,na.rm=T),sd_rel=sd(reliancediff,na.rm=T),
                    delta_opp=mean(oppdiff,na.rm=T),sd_opp=sd(oppdiff,na.rm=T))
  return(out)
}


# map calc diff function to all variables (i.e., removing one at a time and calculating scores diffs)
sensitivity <- map_df(testvars,calc_diffs,dat.full=FULLDAT.NORM)
write.csv(sensitivity,file="score_sensitivity.csv",row.names = F)

## Countries in Each quadrant of kobe plot
up_right <- FULLDAT.NORM %>%
  filter(mean_malnutrition>0.5,econ_opportunity>0.5) %>%
  select(country_name,mean_reliance:mariculture_opportunity) %>%
  mutate(quadrant="Upper Right")
up_left <- FULLDAT.NORM %>%
  filter(mean_malnutrition>0.5,econ_opportunity<0.5) %>%
  select(country_name,mean_reliance:mariculture_opportunity)%>%
  mutate(quadrant="Upper Left")
lower_right <- FULLDAT.NORM %>%
  filter(mean_malnutrition<0.5,econ_opportunity>0.5) %>%
  select(country_name,mean_reliance:mariculture_opportunity)%>%
  mutate(quadrant="Lower Right")
lower_left <- FULLDAT.NORM %>%
  filter(mean_malnutrition<0.5,econ_opportunity<0.5) %>%
  select(country_name,mean_reliance:mariculture_opportunity)%>%
  mutate(quadrant="Lower Left")

quads <- bind_rows(list(up_right,up_left,lower_right,lower_left))
write.csv(quads, file="final_kobe_quadrants.csv")
