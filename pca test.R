source("final_opportunity_metrics.Rmd")
library(vegan)
test<-dat.norm %>% select(contains("_norm"))
test <- test[complete.cases(test),]
trial_pca <- rda(test,scale=F)
summary(trial_pca)

vegan:::biplot.rda(trial_pca,type=c("text","points"))
