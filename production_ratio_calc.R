## Production ratio, mariculture/fisheries

library(dplyr)
## Capture fisheries dataset

WD <- getwd()
capture <- read.csv(paste0(WD,"/data/global_capture_production_filtered.csv"))
capture[capture=="..."]<-NA
capture[capture=="-"]<-NA
capture$X2014 <- as.numeric(capture$X2014)

## Aquaculture dataset
aquaculture <- read.csv(paste0(getwd(),"/data/Production datasets/aquaculture_production_1950_2014.csv"))

# Total production of latest year (2014) and only marine/brackish aquaculture
total_capture <- capture %>% 
  select(country,species,measure,X2014) %>%
  filter(measure=="Quantity (tonnes)") %>%
  select(-measure) %>%
  rename(capture=X2014) %>%
  group_by(country) %>%
  summarise(tot.capture=sum(capture,na.rm=T))

total_mariculture <- aquaculture %>%
  select(country,species,environment,X2014) %>%
  filter(environment %in% c("Marine","Brackishwater")) %>%
  rename(mariculture=X2014) %>%
  group_by(country) %>%
  summarise(tot.mariculture=sum(mariculture,na.rm=T))

# Join data and output
prod_ratio <- full_join(total_capture,total_mariculture,by="country") %>%
  mutate(prod_ratio=tot.mariculture/tot.capture) %>%
  filter(country != "Other nei")

# Write output
write.csv(prod_ratio,file=paste0(WD,"/data/production_ratio_2014.csv"))
