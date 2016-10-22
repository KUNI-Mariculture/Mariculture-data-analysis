## Molly Wilson and Patricia Faundez
## Kuni Mariculture - Nutrient intake variables
## data source: GENus, Planetary Health Alliance, Harvard (http://planetaryhealthalliance.org/genus)

library(dplyr)
library(reshape2)
library(readr)

# set working directory: ~/Google Drive/Kuni Fellowship 2016/Data sources/Nutrient Intake Data
setwd("~/KUNI/Mariculture-data-analysis/Nutrients")

### Normalize percent nutrients obtained from seafood across countries (to 90th percentile)
### Variables: calories_percentseafood, protein_percentseafood, polyunsatFA_percentseafood, vitaminA_percentseafood, zinc_percentseafood, iron_percentseafood
FOOD_SECURITY_and_NUTRIENT_INTAKES <- read_csv("FOOD SECURITY and NUTRIENT INTAKES.csv")
                                              

calories_reliance <- FOOD_SECURITY_and_NUTRIENT_INTAKES$calories_percentseafood
calories90 <- quantile(calories_reliance, probs=.90, na.rm=TRUE)  #https://ww2.coastal.edu/kingw/statistics/R-tutorials/prob.html
calories_reliancenorm <- calories_reliance/calories90


protein_reliance <- FOOD_SECURITY_and_NUTRIENT_INTAKES$protein_percentseafood
protein90 <- quantile(protein_reliance, probs=.90, na.rm=TRUE)  
protein_reliancenorm <- protein_reliance/protein90


polyunsatFA_reliance <- FOOD_SECURITY_and_NUTRIENT_INTAKES$polyunsatFA_percentseafood
polyunsatFA90 <- quantile(polyunsatFA_reliance, probs=.90, na.rm=TRUE)  
polyunsatFA_reliancenorm <- polyunsatFA_reliance/polyunsatFA90


vitaminA_reliance <- FOOD_SECURITY_and_NUTRIENT_INTAKES$vitaminA_percentseafood
vitaminA90 <- quantile(vitaminA_reliance, probs=.90, na.rm=TRUE) 
vitaminA_reliancenorm <- vitaminA_reliance/vitaminA90


zinc_reliance <- FOOD_SECURITY_and_NUTRIENT_INTAKES$zinc_percentseafood
zinc90 <- quantile(zinc_reliance, probs=.90, na.rm=TRUE)  
zinc_reliancenorm <- zinc_reliance/zinc90


iron_reliance <- FOOD_SECURITY_and_NUTRIENT_INTAKES$iron_percentseafood
iron90 <- quantile(iron_reliance, probs=.90, na.rm=TRUE)  
iron_reliancenorm <- iron_reliance/iron90

### Average normalized scores by country
## combining vectors

country <- FOOD_SECURITY_and_NUTRIENT_INTAKES %>% select(country)
reliance <- cbind(country, calories_reliancenorm, protein_reliancenorm, polyunsatFA_reliancenorm, vitaminA_reliancenorm, zinc_reliancenorm, iron_reliancenorm)

#calculating average 

reliance$mean <- rowMeans(reliance[,2:7])

write.csv(reliance, file="reliance_nutrition.csv")
