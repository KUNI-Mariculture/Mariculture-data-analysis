### Full Dataset Construction ###

ptm <- proc.time()
## Load packages ##
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
# Upsides data, from Costello et al. 2016
upsides <- read.csv("renato_upside_results.csv",stringsAsFactors = FALSE)
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
raw.all <- list(Production,Catch,Trade.Q,Trade.V,Pop,GDP,DBI,GOV,nutr_reliance,upsides)

# apply name-finding function, coerce to character vector of ALL unique country names in all data #
names.all <- unlist(purrr::map(raw.all,find_names))
names.all <- data_frame(name=sort(unique(names.all)),country_ID=numeric(length=length(name)))

# manually look at the names and assign unique number to each country #
# accounting for spelling differences, specical characters etc. #
## NOT RUN: ##
# write.csv(names.all,"unique_country_names.csv")

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
raw.all <- purrr::map(raw.all,assign_ID)
Production <- assign_ID(Production)
Catch <- assign_ID(Catch)
Trade.Q <- assign_ID(Trade.Q)
Trade.V <- assign_ID(Trade.V)
Pop <- assign_ID(Pop)
GDP <- assign_ID(GDP)
DBI <- assign_ID(DBI)
GOV <- assign_ID(GOV)
nutr_reliance <- assign_ID(nutr_reliance)
upsides <- assign_ID(upsides)

#### DATA CLEANING AND JOINING ####
#### ECONOMIC VARIABLES ####

#### Fisheries and aquaculture production ####

## Production
Production <- Production %>%
  
  # Remove freshwater species and remove unneeded columns
  filter(environment != "Freshwater") %>%
  select(-(area:unit)) %>%
  
  # Switch to long form
  gather("year","Aq.prod",-country,-country_ID,-species) %>%
  mutate(year=as.numeric(gsub("X","",year))) %>%
  
  # Summarize by year
  group_by(year,country,country_ID) %>%
  summarise(Aq.prod=sum(Aq.prod,na.rm=T)) %>%
  
  # just year 2011 and remove country name (will use country ID for joining)
  filter(year==2011) %>%
  ungroup() %>%
  select(-country)


## Catch
Catch <- Catch %>%
  
  # remove unneeded columns
  select(-area,-unit) %>%
  
  # Switch to long form
  gather("year","F.prod",-country,-country_ID,-species) %>%
  mutate(year=as.numeric(gsub("X","",year))) %>%
  
  # Summarize by year
  group_by(year,country,country_ID) %>%
  summarise(F.prod=sum(F.prod,na.rm=T)) %>%
  
  # just year 2011 and remove country name (will use country ID for joining)
  filter(year==2011) %>%
  ungroup() %>%
  select(-country)

# Merge production and catch by country ID and year

ECON <- full_join(Production, Catch,by=c("country_ID","year"))

##### Trade ####
# Trade in quantity
Trade.Q <- Trade.Q %>%
  
  # Remove unneeded columns
  select(-country,-Unit) %>%
  
  # Switch to long form
  gather("year","Trade.quantity",-country_ID,-commodity,-flow) %>%
  mutate(year=as.numeric(gsub("X","",year))) %>%
  
  # Summarize by year
  group_by(year,country_ID,flow) %>%
  summarise(Trade.quantity=sum(Trade.quantity,na.rm=T))

# Trade in value
Trade.V <- Trade.V %>%
  
  # Remove unneeded columns
  select(-country,-unit) %>%
  
  # Switch to long form
  gather("year","Trade.value",-country_ID,-commodity,-flow) %>%
  mutate(year=as.numeric(gsub("X","",year))) %>%
  
  # Summarize by year
  group_by(year,country_ID,flow) %>%
  summarise(Trade.value=sum(Trade.value,na.rm=T))


# Exports in quantity
Exp.Q <- Trade.Q %>%
  
  # Trade flow "exports"
  filter(flow=="Exports" | flow=="Reexports") %>%
  
  # Summarize by year
  group_by(year,country_ID) %>%
  summarise(Q.Exp=sum(Trade.quantity,na.rm=T)) %>%
  
  # Select just year 2011
  filter(year==2011)

# Exports in value
Exp.V <- Trade.V %>%
  
  # Trade flow "exports"
  filter(flow=="Exports" | flow=="Reexports") %>%
  
  # Summarize by year
  group_by(year,country_ID) %>%
  summarise(V.Exp=sum(Trade.value,na.rm=T)) %>%
  
  # Select just year 2011
  filter(year==2011)

# Imports in quantity
Imp.Q <- Trade.Q %>%
  
  # Trade flow "imports"
  filter(flow=="Imports") %>%
  
  # Rename trade quantity to specify imports
  rename(Q.imp=Trade.quantity) %>%
  select(-flow) %>%
  
  # Select just year 2011
  filter(year==2011)

# Imports in value
Imp.V <- Trade.V %>%
  
  # Trade flow "imports"
  filter(flow=="Imports") %>%
  
  # Rename trade value to specify imports
  rename(V.imp=Trade.value) %>%
  select(-flow) %>%
  
  # Select just year 2011
  filter(year==2011)

# Merge with the master database by country and year
ECON <- full_join(ECON,Exp.Q,by=c("country_ID","year"))
ECON <- full_join(ECON,Exp.V,by=c("country_ID","year"))
ECON <- full_join(ECON,Imp.Q,by=c("country_ID","year"))
ECON <- full_join(ECON,Imp.V,by=c("country_ID","year")) %>% arrange(country_ID)

#### Population ####
# remove country name and flip to long form
Pop <- Pop %>%
  
  # remove country name
  select(-country) %>%
  
  # switch to long form
  gather("year","Population",-country_ID) %>%
  mutate(year=as.numeric(gsub("X","",year))) %>%
  
  # just 2011
  filter(year==2011)

# Join with master database by country and year
ECON <- full_join(ECON,Pop,by=c("country_ID","year"))

#### GDP ####
# flip to long form #
GDP <- GDP %>%
  select(-country) %>%
  
  # switch to long form
  gather("year","GDP",-country_ID) %>%
  mutate(year=as.numeric(gsub("X","",year))) %>%
  
  # just 2011
  filter(year==2011)

# join with master database by country and year #
ECON <- full_join(ECON,GDP,by=c("country_ID","year"))


#### CALCULATE ECONOMIC METRICS ####

# production ratio aquaculture divided by fisheries
ECON <- ECON %>%
  mutate(gross.production.ratio=Aq.prod/F.prod,
         Q.trade.deficit=Q.Exp/Q.imp,
         V.trade.deficit=V.Exp/V.imp,
         willingness=GDP/V.imp,
         gdppc=GDP/Population)

#### Other databases  ####
upsides <- upsides %>%
  distinct(country_ID,.keep_all=TRUE) %>%
  filter(!is.na(country_ID))%>%
  select(-country)

# Doing Business metric for 2011 #
DBI <- DBI %>% 
  distinct(country_ID,year,.keep_all=TRUE) %>% 
  select(-country) %>% 
  filter(year==2011)

# Good governance metric for 2011#
GOV <- GOV %>% 
  select(-country) %>% 
  gather("year","value",-country_ID,-indicator) %>%
  mutate(year=as.numeric(gsub("X","",year))) %>%
  filter(year==2011) %>%
  spread(indicator,value) %>%
  select(-year)
  
names(GOV) <- c("country_ID","corruption","gov_effectiveness","pol_stab","reg_qual","rule_law","voice")

# Join upsides, DBI and GOV to master database by country and year
ECON <- full_join(ECON,DBI,by=c("country_ID","year")) %>%
  full_join(GOV,by="country_ID") %>%
  full_join(upsides,by="country_ID")

#### NUTRITION ####
# select country, energy adequacy, protein, fatty acids, vitamin A, zinc, and iron
NUTRITION_RELIANCE <- nutr_reliance %>% 
  select(country_ID, energy_adequacy,protein,polyunsatFA,vitaminA,zinc,iron,
         calories_percentseafood,protein_percentseafood,polyunsatFA_percentseafood,
         vitaminA_percentseafood,zinc_percentseafood,iron_percentseafood)


#### JOIN DATASETS ####
# Here, we join the data together again, and add a country name, keeping only those countries in both datasets #
FULLDAT <- inner_join(ECON,NUTRITION_RELIANCE,by="country_ID")

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
FULLDAT <- FULLDAT %>% left_join(master_names,by="country_ID") %>% 
  rename(country_name=master) %>%
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
FULLDAT <- FULLDAT %>% select(
  # general country stats
  country_name,country_ID,region,region2,year,Population,GDP,
  #upsides metrics
  stocks:perc_catch_below_05,
  DTF:voice,
  # economic metrics
  Aq.prod:V.imp,gross.production.ratio:gdppc,
  
 
  
  #nutrition and reliance metrics
  energy_adequacy:iron_percentseafood
)

#### NORMALIZE VARIABLES ####
# for most of the variables, we normalize
# to 80th percentile. This means that we'll divide everything by the *80th* percentile country in the data. 
# All countries above this cutoff get forced to 1.

norm_80 <- function(variable) {
  quant80 <- quantile(variable,probs=.8,na.rm=T)
  out <- variable/quant80
  out[out>1] <- 1
  return(out)
}


# Add new, normalized columns for all variables (except those that have already been normalized)
FULLDAT.NORM <- FULLDAT %>%
  mutate_at(vars(gross.production.ratio:iron_percentseafood),funs("norm"=norm_80))

#### REMOVE COUNTRIES WITH MISSING DATA BEFORE CALCULATING SCORES ####
## Number of NA values in each variable ##
NA_count <- FULLDAT.NORM %>% 
  summarise_all(funs(sum(is.na(.))))

#### CALCULATE AGGREGATE SCORES FOR THREE CATEGORIES ####
# Add aggregated scores for each category, based on 80th percentile normalized scores. 
# For these scores, disregard NA values in individual vars FOR ECON, but NOT FOR NUTRITION AND RELIANCE

FULLDAT.NORM$mean_econ <- rowMeans(select(FULLDAT.NORM,gross.production.ratio_norm:gdppc_norm),na.rm=T)
FULLDAT.NORM$mean_nutrition <- rowMeans(select(FULLDAT.NORM,energy_adequacy_norm:iron_norm),na.rm=F)
FULLDAT.NORM$mean_reliance <- rowMeans(select(FULLDAT.NORM,calories_percentseafood_norm:iron_percentseafood_norm),na.rm=F)

# For countries with missing energy adequacy data, this is okay (recalculate nutrition score)
FULLDAT.NORM$mean_nutrition[is.na(FULLDAT.NORM$energy_adequacy_norm)] <- rowMeans(select(FULLDAT.NORM,energy_adequacy_norm:iron_norm),na.rm=T)[is.na(FULLDAT.NORM$energy_adequacy_norm)]
FULLDAT.NORM$mean_econ[is.nan(FULLDAT.NORM$mean_econ)] <- NA
FULLDAT.NORM$mean_nutrition[is.nan(FULLDAT.NORM$mean_nutrition)] <- NA


## FOR NUTRITION, RESCALE TO 0-1. BECAUSE CALORIES, ETC. DON'T SCALE DOWN TO ZERO
FULLDAT.NORM$mean_nutrition <- (FULLDAT.NORM$mean_nutrition
                                -min(FULLDAT.NORM$mean_nutrition,na.rm=T))/(max(FULLDAT.NORM$mean_nutrition,na.rm=T)-
                                                                              min(FULLDAT.NORM$mean_nutrition,na.rm=T))


#### FINAL SCORES ####
## for econ and nutrition, the opportunity score is actually one minus the calculated score above
# Include a geometric mean of reliance and malnutrition, for visualization's sake (not used for final score) #
FULLDAT.NORM <- FULLDAT.NORM %>% 
  mutate(econ_opportunity=1-mean_econ,mean_malnutrition=1-mean_nutrition,
                                        reliance_mal=sqrt(mean_reliance*mean_malnutrition)) %>%
  select(-mean_econ,-mean_nutrition)

# Final score
FULLDAT.NORM$mariculture_opportunity <- rowMeans(select(FULLDAT.NORM,mean_reliance,econ_opportunity,mean_malnutrition))

# Reduced to just countries with a final score
FULLDAT.NORM.reduced <- FULLDAT.NORM %>% filter(!is.na(mariculture_opportunity))

### write the data ####
# write.csv(FULLDAT.NORM,file="final_country_scores.csv",row.names=F)
# write.csv(FULLDAT.NORM.reduced,file="final_country_scores_completecases.csv",row.names=F)


#### Sensitivity Analyses ####
## Remove one variable at a time and look at how the mean and SD changes for the three scores ##
# Function to remove and re-calculate #

# variables to test, and categories of variables
testvars <- FULLDAT.NORM %>% 
  select(country_name,contains("_norm")) %>% names()
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
sensitivity <- map_df(testvars,calc_diffs,dat.full=FULLDAT.NORM.reduced)
# write.csv(sensitivity,file="score_sensitivity.csv",row.names = F)

## Countries in Each quadrant of kobe plot
up_right <- FULLDAT.NORM.reduced %>%
  filter(mean_malnutrition>0.5,econ_opportunity>0.5) %>%
  select(country_name,mean_reliance:mariculture_opportunity) %>%
  mutate(quadrant="Upper Right")
up_left <- FULLDAT.NORM.reduced %>%
  filter(mean_malnutrition>0.5,econ_opportunity<0.5) %>%
  select(country_name,mean_reliance:mariculture_opportunity)%>%
  mutate(quadrant="Upper Left")
lower_right <- FULLDAT.NORM.reduced %>%
  filter(mean_malnutrition<0.5,econ_opportunity>0.5) %>%
  select(country_name,mean_reliance:mariculture_opportunity)%>%
  mutate(quadrant="Lower Right")
lower_left <- FULLDAT.NORM.reduced %>%
  filter(mean_malnutrition<0.5,econ_opportunity<0.5) %>%
  select(country_name,mean_reliance:mariculture_opportunity)%>%
  mutate(quadrant="Lower Left")

quads <- bind_rows(list(up_right,up_left,lower_right,lower_left))
# write.csv(quads, file="final_kobe_quadrants.csv")

#### GAP ANALYSIS ####
# where did countries drop out??

## step 1: raw data (after joining all econ variables and governance indicators, but removing none)
econ_all_countries <- select(ECON,country_ID) %>%
  left_join(master_names,by="country_ID") %>%
  mutate(econ="included") %>%
  select(-region) %>%
  arrange(country_ID)
# nutrition and reliance ARE THE SAME (same nations, because they come from same data)
nutrition_all_countries <- select(NUTRITION_RELIANCE,country_ID) %>%
  left_join(master_names,by="country_ID") %>%
  mutate(nutrition="included") %>%
  select(-region) %>%
  arrange(country_ID)

# first join (what is removed when econ and nutrition/reliance are joined?)
# this is after removing landlocked countries as well
first_join <- select(FULLDAT,country_ID) %>%
  left_join(master_names,by="country_ID") %>%
  mutate(join="included") %>%
  select(-region) %>%
  arrange(country_ID)

# After final metric calculation, with the following conditions:
# 1. Remove countries with missing data for ANY metric, UNLESS
      # A. the country is only missing Aquaculture production data (keep it)
      # B. the country is only missing energy adequacy (keep it)
      # C. the country is only missing GDP data (keep it)

final_set <- select(FULLDAT.NORM.reduced,country_ID) %>%
  left_join(master_names,by="country_ID") %>%
  mutate(final="included") %>%
  select(-region) %>%
  arrange(country_ID)

data_gaps <- full_join(econ_all_countries,nutrition_all_countries,by=c("country_ID","master")) %>%
  full_join(first_join,by=c("country_ID","master")) %>%
  full_join(final_set,by=c("country_ID","master"))
data_gaps[is.na(data_gaps)] <- "REMOVED"

## what are still missing in the final (reduced) data?
NA_count_final <- FULLDAT.NORM.reduced %>% 
  summarise_all(funs(sum(is.na(.))))
# which countries are missing GDP data?
FULLDAT.NORM.reduced$country_name[is.na(FULLDAT.NORM.reduced$GDP)]
# which countries are missing Aq.prod data?
FULLDAT.NORM.reduced$country_name[is.na(FULLDAT.NORM.reduced$Aq.prod)]
# which countries are missing F.prod data?
FULLDAT.NORM.reduced$country_name[is.na(FULLDAT.NORM.reduced$F.prod)]
# which countries are missing energy adequacy data?
FULLDAT.NORM.reduced$country_name[is.na(FULLDAT.NORM.reduced$energy_adequacy)]

proc.time() - ptm

#### WHAT IS THE EFFECT OF NORM80 INSTEAD OF NORM90? ####
norm_90 <- function(variable) {
  quant90 <- quantile(variable,probs=0.9,na.rm=T)
  out <- variable/quant90
  out[out>1] <- 1
  return(out)
}


# Add new, normalized columns for all variables (except those that have already been normalized)
FULLDAT.NORM90 <- FULLDAT %>%
  mutate_at(vars(gross.production.ratio:iron_percentseafood),funs("norm"=norm_90))

#REMOVE COUNTRIES WITH MISSING DATA BEFORE CALCULATING SCORES #


# CALCULATE AGGREGATE SCORES FOR THREE CATEGORIES #
# Add aggregated scores for each category, based on 90th percentile normalized scores. 
# For these scores, disregard NA values in individual vars FOR ECON, but NOT FOR NUTRITION AND RELIANCE

FULLDAT.NORM90$mean_econ <- rowMeans(select(FULLDAT.NORM90,gross.production.ratio_norm:gdppc_norm),na.rm=T)
FULLDAT.NORM90$mean_nutrition <- rowMeans(select(FULLDAT.NORM90,energy_adequacy_norm:iron_norm),na.rm=F)
FULLDAT.NORM90$mean_reliance <- rowMeans(select(FULLDAT.NORM90,calories_percentseafood_norm:iron_percentseafood_norm),na.rm=F)

# For countries with missing energy adequacy data, this is okay (recalculate nutrition score)
FULLDAT.NORM90$mean_nutrition[is.na(FULLDAT.NORM90$energy_adequacy_norm)] <- rowMeans(select(FULLDAT.NORM90,energy_adequacy_norm:iron_norm),na.rm=T)[is.na(FULLDAT.NORM90$energy_adequacy_norm)]
FULLDAT.NORM90$mean_econ[is.nan(FULLDAT.NORM90$mean_econ)] <- NA
FULLDAT.NORM90$mean_nutrition[is.nan(FULLDAT.NORM90$mean_nutrition)] <- NA


## FOR NUTRITION, RESCALE TO 0-1. BECAUSE CALORIES, ETC. DON'T SCALE DOWN TO ZERO
FULLDAT.NORM90$mean_nutrition <- (FULLDAT.NORM90$mean_nutrition
                                -min(FULLDAT.NORM90$mean_nutrition,na.rm=T))/(max(FULLDAT.NORM90$mean_nutrition,na.rm=T)-
                                                                              min(FULLDAT.NORM90$mean_nutrition,na.rm=T))


# FINAL SCORES #
## for econ and nutrition, the opportunity score is actually one minus the calculated score above
# Include a geometric mean of reliance and malnutrition, for visualization's sake (not used for final score) #
FULLDAT.NORM90 <- FULLDAT.NORM90 %>% 
  mutate(econ_opportunity=1-mean_econ,mean_malnutrition=1-mean_nutrition,
         reliance_mal=sqrt(mean_reliance*mean_malnutrition)) %>%
  select(-mean_econ,-mean_nutrition)

# Final score
FULLDAT.NORM90$mariculture_opportunity <- rowMeans(select(FULLDAT.NORM90,mean_reliance,econ_opportunity,mean_malnutrition))

#### What about un-normalized? ####
norm_100 <- function(variable) {
  quant100 <- quantile(variable,probs=1,na.rm=T)
  out <- variable/quant100
  out[out>1] <- 1
  return(out)
}


# Add new, normalized columns for all variables (except those that have already been normalized)
FULLDAT.NORM100 <- FULLDAT %>%
  mutate_at(vars(gross.production.ratio:iron_percentseafood),funs("norm"=norm_100))

#REMOVE COUNTRIES WITH MISSING DATA BEFORE CALCULATING SCORES #


# CALCULATE AGGREGATE SCORES FOR THREE CATEGORIES #
# Add aggregated scores for each category, based on 100th percentile normalized scores. 
# For these scores, disregard NA values in individual vars FOR ECON, but NOT FOR NUTRITION AND RELIANCE

FULLDAT.NORM100$mean_econ <- rowMeans(select(FULLDAT.NORM100,gross.production.ratio_norm:gdppc_norm),na.rm=T)
FULLDAT.NORM100$mean_nutrition <- rowMeans(select(FULLDAT.NORM100,energy_adequacy_norm:iron_norm),na.rm=F)
FULLDAT.NORM100$mean_reliance <- rowMeans(select(FULLDAT.NORM100,calories_percentseafood_norm:iron_percentseafood_norm),na.rm=F)

# For countries with missing energy adequacy data, this is okay (recalculate nutrition score)
FULLDAT.NORM100$mean_nutrition[is.na(FULLDAT.NORM100$energy_adequacy_norm)] <- rowMeans(select(FULLDAT.NORM100,energy_adequacy_norm:iron_norm),na.rm=T)[is.na(FULLDAT.NORM100$energy_adequacy_norm)]
FULLDAT.NORM100$mean_econ[is.nan(FULLDAT.NORM100$mean_econ)] <- NA
FULLDAT.NORM100$mean_nutrition[is.nan(FULLDAT.NORM100$mean_nutrition)] <- NA


## FOR NUTRITION, RESCALE TO 0-1. BECAUSE CALORIES, ETC. DON'T SCALE DOWN TO ZERO
FULLDAT.NORM100$mean_nutrition <- (FULLDAT.NORM100$mean_nutrition
                                  -min(FULLDAT.NORM100$mean_nutrition,na.rm=T))/(max(FULLDAT.NORM100$mean_nutrition,na.rm=T)-
                                                                                  min(FULLDAT.NORM100$mean_nutrition,na.rm=T))


# FINAL SCORES #
## for econ and nutrition, the opportunity score is actually one minus the calculated score above
# Include a geometric mean of reliance and malnutrition, for visualization's sake (not used for final score) #
FULLDAT.NORM100 <- FULLDAT.NORM100 %>% 
  mutate(econ_opportunity=1-mean_econ,mean_malnutrition=1-mean_nutrition,
         reliance_mal=sqrt(mean_reliance*mean_malnutrition)) %>%
  select(-mean_econ,-mean_nutrition)

# Final score
FULLDAT.NORM100$mariculture_opportunity <- rowMeans(select(FULLDAT.NORM100,mean_reliance,econ_opportunity,mean_malnutrition))


#### COMPARE RANKS AND SCORES ####
norm80.scores <- FULLDAT.NORM %>% 
  select(country_name,mean_reliance:mariculture_opportunity)%>%
  mutate_if(is.numeric,funs(rnk=min_rank(desc(.))))%>%
  gather("category","score",mean_reliance:mariculture_opportunity)%>%
  gather("rnk_cat","rank",mean_reliance_rnk:mariculture_opportunity_rnk)
norm90.scores <- FULLDAT.NORM90 %>% 
  select(country_name,mean_reliance:mariculture_opportunity)%>%
  mutate_if(is.numeric,funs(rnk=min_rank(desc(.))))%>%
  gather("category","score",mean_reliance:mariculture_opportunity)%>%
  gather("rnk_cat","rank",mean_reliance_rnk:mariculture_opportunity_rnk)
norm100.scores <- FULLDAT.NORM100 %>% 
  select(country_name,mean_reliance:mariculture_opportunity)%>%
  mutate_if(is.numeric,funs(rnk=min_rank(desc(.))))%>%
  gather("category","score",mean_reliance:mariculture_opportunity)%>%
  gather("rnk_cat","rank",mean_reliance_rnk:mariculture_opportunity_rnk)


## Top and Bottom 10 countries for each calculation
topbot10 <- function(dat) {
  temp <- dat %>%
    select(country_name,rnk_cat,rank)%>%
    filter(rnk_cat=="mariculture_opportunity_rnk")%>%
    distinct()%>%
    arrange(rank)
  top10<-temp %>%
    top_n(-10,rank)%>%
    mutate(tier="Top 10")
  bot10<-temp %>%
    top_n(10,rank)%>%
    mutate(tier="Bottom 10")
  out<-bind_rows(top10,bot10)
  return(out)
}
topbot10_unfilt<-topbot10(norm100.scores) %>% select(rank,country_name) %>% rename(norm100=country_name)
topbot10_filt90<-topbot10(norm90.scores) %>% select(country_name) %>% rename(norm90=country_name)
topbot10_filt80 <- topbot10(norm80.scores) %>% select(country_name) %>% rename(norm80=country_name)
topbot10_all <- bind_cols(topbot10_unfilt,topbot10_filt90,topbot10_filt80)

# write.csv(topbot10_all,file="top_and_bot_10_sensitivity.csv",row.names = F)

## Kendall's Tau between scores? ##
normscores_all <- norm80.scores %>%
  filter(category=="mariculture_opportunity")%>%
  select(country_name,score) %>%
  distinct()%>%
  rename(score80=score)%>%
  left_join((norm90.scores %>%
               filter(category=="mariculture_opportunity")%>%
               select(country_name,score) %>%
               distinct()%>%
               rename(score90=score)),
            by="country_name")%>%
  left_join((norm100.scores %>%
               filter(category=="mariculture_opportunity")%>%
               select(country_name,score) %>%
               distinct()%>%
               rename(score100=score)),
            by="country_name")
ktau <- cor(select(normscores_all,-country_name),method="kendall",use="pairwise.complete.obs")

# Kendall's W between ranks
library(irr)
normranks_all <- norm80.scores %>%
  filter(rnk_cat=="mariculture_opportunity_rnk")%>%
  select(country_name,rank) %>%
  distinct()%>%
  rename(rank80=rank)%>%
  left_join((norm90.scores %>%
               filter(rnk_cat=="mariculture_opportunity_rnk")%>%
               select(country_name,rank) %>%
               distinct()%>%
               rename(rank90=rank)),
            by="country_name")%>%
  left_join((norm100.scores %>%
               filter(rnk_cat=="mariculture_opportunity_rnk")%>%
               select(country_name,rank) %>%
               distinct()%>%
               rename(rank100=rank)),
            by="country_name")%>%
  filter(!is.na(rank80))

kw <- normranks_all %>% select(-country_name) %>% kendall()

plot(normscores_all$score80,normscores_all$score100)
# norm90.score.change <- FULLDAT.NORM90 %>% 
#   select(country_name,mean_reliance:mariculture_opportunity)%>%
#   # rank countries by each score
#   mutate_if(is.numeric,funs(rnk=min_rank(desc(.))))%>%
#   # long form for comparison
#   gather("category_90","score_90",mean_reliance:mariculture_opportunity)%>%
#   gather("rnk_cat_90","rank_90",mean_reliance_rnk:mariculture_opportunity_rnk)%>%
#   # join the original (normalized to 80th percentile) scores
#   bind_cols(norm80.scores)%>%
#   select(-country_name1)%>%
#   # calculate a percent difference in score, and the absolute change in rank for each category
#   mutate(score_change=score-score_90,rnk_change=rank-rank_90)%>%
#   arrange(country_name,category)

# #spread to wideform
# score_changes <- norm90.score.change %>%
#   select(country_name,category,score_change)%>%
#   distinct()
# rank_changes <- norm90.score.change %>%
#   select(country_name,rnk_cat,rnk_change)%>%
#   distinct()%>%
#   filter(rnk_cat!="reliance_mal_rnk")
# 
# # plot?
# require(ggplot2)
# require(extrafont)
# theme_rockwell <- function() {
#   theme_minimal()+
#     theme(text=element_text(family="Rockwell",size=12),
#           axis.title = element_text(family="Rockwell",size=14),
#           strip.background = element_rect(colour="black"),
#           panel.border = element_rect(color="black",fill=NA))
# }
# 
# rank_change_plot <- ggplot(rank_changes,aes(x=rnk_change,y=..count..))+
#   geom_histogram(binwidth=5)+
#   labs(x="Absolute Change in Rank",y="Number of Nations")+
#   geom_vline(xintercept=0,linetype=2)+
#   facet_wrap(~rnk_cat)+
#   theme_rockwell()
# rank_change_plot
# 
# score_change_plot <- ggplot(score_changes,aes(x=score_change,y=..count..))+
#   geom_histogram(binwidth=0.05)+
#   labs(x="Absolute Score Change",y="Number of Nations")+
#   geom_vline(xintercept=0,linetype=2)+
#   facet_wrap(~category)+
#   theme_rockwell()
# score_change_plot
