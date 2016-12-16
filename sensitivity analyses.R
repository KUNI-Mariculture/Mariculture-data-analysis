## sENSITIVITY ###

library(dplyr)
library(ggplot2)

dat.norm <- read.csv(paste0(getwd(),"/mariculture_opportunity_metrics_10.23.16.csv"))
dat.new <- dat.norm %>% select(country,contains("_norm")) #just the normalized variables

# variables to test, and categories of variables
testvars <- names(dat.new)
econvars <- testvars[2:5]
nutvars <- testvars[6:12]
reliancevars <- testvars[13:18]


# Final score
calc_scores <- function(dat) {
  out<-dat
  
  econvars <- names(out)[names(out) %in% econvars]
  nutvars <- names(out)[names(out) %in% nutvars]
  reliancevars <- names(out)[names(out) %in% reliancevars]
  
  out$mean_econ <- rowMeans(select(out,one_of(econvars)),na.rm=T)
  out$mean_nutrition <- rowMeans(select(out,one_of(nutvars)),na.rm=T)
  out$mean_nutrition[is.nan(out$mean_nutrition)] <- NA
  out$mean_reliance <- rowMeans(select(out,one_of(reliancevars)),na.rm=T)
  out <- out %>% mutate(econ_opportunity=1-mean_econ,mean_malnutrition=1-mean_nutrition)
  out$mariculture_opportunity <- rowMeans(select(out,mean_reliance,econ_opportunity,mean_malnutrition))
  return(out)
}

# calculate differences from full model
calc_diffs <- function(dat.full,dat.reduced,title) {
  # for each aggregate score, calculate mean change across countries
  econdiff <- dat.reduced$mean_econ - dat.full$mean_econ
  nutdiff <- dat.reduced$mean_nutrition - dat.full$mean_nutrition
  reliancediff <- dat.reduced$mean_reliance - dat.full$mean_reliance
  oppdiff <- dat.reduced$mariculture_opportunity-dat.full$mariculture_opportunity
  par(mfrow=c(2,2),oma=c(0,0,2,0))
  
  hist(econdiff,main="Econ Change",xlim=c(-0.5,0.5))
  hist(nutdiff,main="Nutrition Change",xlim=c(-0.5,0.5))
  hist(reliancediff,main="Rel Change",xlim=c(-0.5,0.5))
  hist(oppdiff,main="Opp change",xlim=c(-0.5,0.5))
  title(title,outer=T)
  #out <- c(econdiff,nutdiff,reliancediff,oppdiff)
  
  # names(out) <- c("econdiff","nutdiff","reliancediff","oppdiff")
  # return(out)
}

# test for removing one variable
test <- dat.new %>% select(-prod_diversity_norm) %>% calc_scores(.) %>% calc_diffs(dat.norm,.,title="prod_diversity_norm")

setwd(paste0(getwd(),"/sensitivity data"))
for(i in 1:length(testvars)) {
  filename <- paste0(testvars[i],"_sens.png")
  png(filename)
  dat.new[-i] %>% calc_scores(.) %>% calc_diffs(dat.norm,.,title=testvars[i])
  dev.off()
}
