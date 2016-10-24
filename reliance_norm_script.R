## Owen Liu
## Kuni Mariculture - Reliance on seafood for nutrients
## data source: GENus, Planetary Health Alliance, Harvard (http://planetaryhealthalliance.org/genus)

library(dplyr)
library(reshape2)


### Normalize percent nutrients obtained from seafood across countries (to 90th percentile)
### Variables: calories_percentseafood, protein_percentseafood, polyunsatFA_percentseafood, vitaminA_percentseafood, zinc_percentseafood, iron_percentseafood
reliance <- read.csv(paste0(getwd(),"/data/FOOD SECURITY and NUTRIENT INTAKES.csv"))

# normalize to 90th percentile. This means that we'll divide everything by the 90th percentile country in the data. All countries above this
# cutoff get forced to 1.
norm_90 <- function(nutrient) {
 quant90 <- quantile(nutrient,probs=.9,na.rm=T)
 out <- nutrient/quant90
 out[out>1] <- 1
 return(out)
}

norm_reliance <- reliance %>% 
  select(country, calories_percentseafood,protein_percentseafood,polyunsatFA_percentseafood,vitaminA_percentseafood,zinc_percentseafood,iron_percentseafood) %>%
  mutate_at(vars(-country),funs("norm"=norm_90))

norm_reliance$mean_reliance <- rowMeans(norm_reliance[,8:13])

# write output
write.csv(norm_reliance, file=paste0(getwd(),"/data/reliance_norm.csv"),row.names = F)
