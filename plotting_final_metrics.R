## Plots for presentation and publication
## Data
FULLDAT.NORM <- read.csv("master dataset construction/final_country_scores.csv",stringsAsFactors = F)

library(tidyverse)
library(ggthemes)
library(extrafont)

pal <- c("#9E0142", "#D53E4F", "#F46D43","#66C2A5", "#3288BD", "#5E4FA2")

#### Biplots of final scores ####
# econ vs. malnutrition
econ_mal <-ggplot(FULLDAT.NORM, aes(x=econ_opportunity,y=mean_malnutrition)) + 
  geom_point(aes(col=region2,size=mean_reliance))

# mal vs. reliance
mal_rel <- ggplot(FULLDAT.NORM, aes(x=mean_malnutrition,y=mean_reliance)) + 
  geom_point(aes(col=region2),size=3)

# econ vs. (reliance*malnutrition)
econ_rel_mal <- ggplot(FULLDAT.NORM, aes(x=econ_opportunity,y=reliance_mal)) + 
  geom_point(aes(col=region2),size=3)

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
  xlab("Nutrition Opportunity")+
  ylab("Reliance on Seafood")+
  geom_smooth(method="lm",se=F)+
  scale_color_manual(values = pal,name="")+
  theme_few()+
  theme(legend.position = "right",
        text = element_text(size = 18,color="black",family="Rockwell"),
        panel.border = element_blank(),
        legend.key = element_blank(),
        legend.title=element_blank())
kobe3
# ggsave("mal_rel.png",plot=kobe3,width=8)
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

#### Mariculture Opportunity Relative to Economic Indicators ####

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



#### Economic Opportunity Relative to Economic Indicators ####

# Political stability vs. mariculture opportunity
polstab_econopp <- ggplot(FULLDAT.NORM, aes(x=pol_stab,y=econ_opportunity))+
  ylim(0,1)+
  geom_point(size=2)+
  xlab("Political Stability")+
  ylab("Economic Opportunity")+
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
polstab_econopp

# Control of corruption vs. mariculture opportunity
corruption_econopp <- ggplot(FULLDAT.NORM, aes(x=corruption,y=econ_opportunity))+
  ylim(0,1)+
  geom_point(size=2)+
  xlab("Corruption")+
  ylab("Economic Opportunity")+
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
corruption_econopp

# rule of law vs. mariculture opportunity
law_econopp <- ggplot(FULLDAT.NORM, aes(x=rule_law,y=econ_opportunity))+
  ylim(0,1)+
  geom_point(size=2)+
  xlab("Rule of Law")+
  ylab("Economic Opportunity")+
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
law_econopp

#government effectiveness vs mariculture opportunity
gov_econopp<- ggplot(FULLDAT.NORM, aes(x=gov_effectiveness,y=econ_opportunity))+
  ylim(0,1)+
  geom_point(size=2)+
  xlab("Government Effectiveness")+
  ylab("Economic Opportunity")+
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
gov_econopp

# voice and accountability vs. mariculture opportunity
voice_econopp <- ggplot(FULLDAT.NORM, aes(x=voice,y=econ_opportunity))+
  ylim(0,1)+
  geom_point(size=2)+
  xlab("Voice and Accountability")+
  ylab("Economic Opportunity")+
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
voice_econopp

# voice and accountability vs. mariculture opportunity
dbi_econopp <- ggplot(FULLDAT.NORM, aes(x=DTF,y=econ_opportunity))+
  ylim(0,1)+
  geom_point(size=2)+
  xlab("Doing Business Indicator")+
  ylab("Economic Opportunity")+
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
dbi_econopp

# significant?
summary(lm(econ_opportunity~DTF+voice+gov_effectiveness+rule_law+corruption+pol_stab,data=FULLDAT.NORM))
summary(lm(econ_opportunity~pol_stab,data=FULLDAT.NORM))
summary(lm(econ_opportunity~gov_effectiveness,data=FULLDAT.NORM))
summary(lm(econ_opportunity~voice,data=FULLDAT.NORM))
summary(lm(econ_opportunity~rule_law,data=FULLDAT.NORM))
summary(lm(econ_opportunity~corruption,data=FULLDAT.NORM))
summary(lm(econ_opportunity~DTF,data=FULLDAT.NORM))
#hump shaped?

summary(lm(econ_opportunity~I(corruption^2),data=FULLDAT.NORM))
summary(lm(econ_opportunity~I(rule_law^2),data=FULLDAT.NORM))
summary(lm(econ_opportunity~I(gov_effectiveness^2),data=FULLDAT.NORM))

#### Histograms of opportunity scores ####
opps_long <- FULLDAT.NORM %>% 
  select(country_ID,country_name,region2,econ_opportunity,mean_malnutrition,mean_reliance,mariculture_opportunity) %>%
  gather("metric","score",econ_opportunity:mariculture_opportunity)
opp_names=c(`econ_opportunity`="Economic Opportunity",
            `mean_malnutrition`="Nutritional Opportunity",
            `mean_reliance`="Seafood Reliance",
            `mariculture_opportunity`="Mariculture Opportunity")
scorehist<-ggplot(opps_long,aes(x=score))+
  geom_vline(xintercept=0.5,linetype=2,col="gray70")+
  geom_histogram(bins=10)+
  facet_wrap(~metric,labeller = as_labeller(opp_names))+
  ylab("Number of Countries")+
  xlab("Score")+
  theme_few()+
  theme(text = element_text(size = 18,color="black",family="Rockwell"),
        panel.border = element_blank(),
        legend.key = element_blank(),
        plot.background = element_rect(linetype = 0))
scorehist
#ggsave("final_hists.png",scorehist,width=8)


#### Regional Means, histograms, and correlations ####
region_means <- FULLDAT.NORM %>% group_by(region2) %>% 
  select(region2,econ_opportunity:mariculture_opportunity) %>% 
  summarise_each(funs(mean,sd))
region_means

lac <- FULLDAT.NORM %>% filter(region2=="Latin America and Caribbean")
cor.test(lac$mean_malnutrition,lac$econ_opportunity)
cor.test(lac$mean_malnutrition,lac$mean_reliance)


africa <- FULLDAT.NORM %>% filter(region2=="Africa")
cor.test(africa$mean_malnutrition,africa$econ_opportunity)

asia <- FULLDAT.NORM %>% filter(region2=="Asia")
cor.test(asia$mean_malnutrition,asia$econ_opportunity)

seao <- FULLDAT.NORM %>% filter(region2=="South East Asia and Oceania")
cor.test(seao$mean_malnutrition,seao$econ_opportunity)
cor.test(seao$mean_malnutrition,seao$mean_reliance)

## histogram of overall score by region ##
mariculture_opp<- opps_long %>% filter(metric=="mariculture_opportunity")
regionhist<-ggplot(mariculture_opp,aes(x=score,fill=region2))+
  geom_vline(xintercept=0.5,linetype=2,col="gray70")+
  geom_density(alpha=0.3)+
  facet_wrap(~region2)+
  xlim(0,1)+
  guides(fill=F)+
  ylab("")+
  xlab("")+
  theme_few()+
  theme(text = element_text(size = 16,color="black",family="Rockwell"),
        strip.text = element_text(size=12),
        plot.background = element_rect(linetype = 1),
        panel.spacing = unit(1,"lines"))
regionhist
#ggsave("opp_hist_region.png",regionhist,width=8)
FULLDAT.NORM %>% group_by(region2) %>% summarise(n())
