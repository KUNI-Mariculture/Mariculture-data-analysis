## Aquaculture species production diversity

## Simpson index of diversity (complement of the index, 1-D)
simpson <- function(production) {
  if(all(is.na(production))) return(NA)
  if(sum(production,na.rm=T)==0) return(NA)
  else {
    tot <- sum(production,na.rm=T)
    props <- production/tot
    out <- 1 - (sum(props^2,na.rm=T))
    return(out)
  }
}

# Data of aquaculture production by species/year
dat <- read.csv(paste0(getwd(),"/data/Production datasets/aquaculture_production_1950_2014.csv"))

library(dplyr)

# Only Marine and Brackishwater aquaculture, only year 2014
dat2 <- dat %>%
  filter(environment %in% c("Brackishwater","Marine")) %>%
  select(country,species,environment,X2014) %>%
  group_by(country) %>%
  
  #apply simpson metric
  summarise(prod_diversity=simpson(X2014))

write.csv(dat2,file=paste0(getwd(),"/data/production_diversity_2014.csv"),row.names = F)
