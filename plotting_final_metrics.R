## Plots for presentation and publication
## Data
FULLDAT.NORM <- read.csv("master dataset construction/final_country_scores_completecases.csv",stringsAsFactors = F)

library(tidyverse)
library(ggthemes)
library(extrafont)

pal <- c("#9E0142", "#D53E4F", "#F46D43","#66C2A5", "#3288BD", "#5E4FA2")

#### Biplots of final scores ####
# econ vs. malnutrition
econ_mal <-ggplot(FULLDAT.NORM, aes(x=econ_opportunity,y=mean_malnutrition)) + 
  geom_point(aes(fill=region2,size=mean_reliance,alpha=mean_reliance),shape=21,color="black")

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
  geom_smooth(aes(color=region2),method="lm",se=F,linetype=1)+
  coord_fixed(xlim=c(0,1),ylim=c(0,1))+
  geom_hline(yintercept=0.5,linetype=2)+
  geom_vline(xintercept = 0.5,linetype=2)+
  xlab("Nutrition Opportunity")+
  ylab("Reliance on Seafood")+
  # geom_smooth(method="lm",se=F)+
  scale_color_manual(values = pal,name="")+
  theme_few()+
  theme(legend.position = "right",
        text = element_text(size = 18,color="black",family="Rockwell"),
        panel.border = element_blank(),
        legend.key = element_blank(),
        legend.title=element_blank())
kobe3
#ggsave("mal_rel.png",plot=kobe3,width=8)
### Econ, Mal, Reliance as size

econ_mal2 <- econ_mal+
  coord_fixed(xlim=c(0,1),ylim=c(0,1))+
  geom_hline(yintercept=0.5,linetype=2)+
  geom_vline(xintercept = 0.5,linetype=2)+
  xlab("Economic Opportunity")+
  ylab("Nutrition Opportunity")+
  scale_size_continuous(name="Reliance on Seafood",range=c(0.5,10))+
  scale_alpha(name="",breaks=c(0.25,0.5,0.75,1),range=c(0.1,0.9),guide=F)+
  scale_fill_manual(values = pal,name="")+
  guides(fill=guide_legend(override.aes = list(size=5,linetype=0,alpha=0.9)),
         size=guide_legend(override.aes=list(fill="#5E4FA2",alpha=c(0.2,0.4,0.6,0.9))))+
  theme_few()+
  theme(legend.position = "right",
        text = element_text(size = 18,color="black",family="Rockwell"),
        legend.text = element_text(size=12,color="black",family="Rockwell"),
        legend.title = element_text(size=12),
        panel.border = element_blank(),
        legend.key = element_blank(),
        plot.background = element_rect(linetype = 0))

econ_mal2
# ggsave("econ_mal.png", plot=econ_mal2,  width=8,height=8.25)

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
# #ggsave("kobe_blank.png", plot=econ_mal3,  width=8)

#### Mariculture Opportunity Relative to Economic Indicators ####

# Political stability vs. mariculture opportunity
polstab_opportunity <- ggplot(FULLDAT.NORM, aes(x=pol_stab,y=mariculture_opportunity))+
  ylim(0,1)+
  geom_point(size=2)+
  xlab("Political Stability")+
  ylab("Mariculture Opportunity")+
  geom_smooth(method="lm",se=F,col="darkgreen",linetype=2)+
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
  geom_smooth(method="lm",se=F,col="darkgreen",linetype=2)+
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
  geom_smooth(method="lm",se=F,col="darkgreen",linetype=2)+
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
  geom_smooth(method="lm",se=F,col="darkgreen",linetype=2)+
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
  geom_smooth(method="lm",se=F,col="darkgreen",linetype=2)+
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
  geom_smooth(method="lm",se=F,col="darkgreen",linetype=2)+
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

##save plots
#ggsave("master dataset construction/extrafigs/dbi_opp.png",plot=dbi_opportunity)
#ggsave("master dataset construction/extrafigs/voice_opp.png",plot=voice_opportunity)
#ggsave("master dataset construction/extrafigs/gov_opp.png",plot=gov_opportunity)
#ggsave("master dataset construction/extrafigs/law_opp.png",plot=law_opportunity)
#ggsave("master dataset construction/extrafigs/corrup_opp.png",plot=corruption_opportunity)
#ggsave("master dataset construction/extrafigs/pols_opp.png",plot=polstab_opportunity)

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
polstab_econopp <- ggplot(FULLDAT.NORM, aes(x=pol_stab,y=econ_opportunity,group=region2,col=region2))+
  ylim(0,1)+
  geom_point(size=4)+
  xlab("Political Stability")+
  ylab("Economic Opportunity")+
  geom_smooth(method="lm",se=F,col="darkgreen",linetype=2)+
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
  geom_smooth(method="lm",se=F,col="darkgreen",linetype=2)+
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
  geom_smooth(method="lm",se=F,col="darkgreen",linetype=2)+
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
  geom_smooth(method="lm",se=F,col="darkgreen",linetype=2)+
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
  geom_smooth(method="lm",se=F,col="darkgreen",linetype=2)+
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
  geom_smooth(method="lm",se=F,col="darkgreen",linetype=2)+
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
opps_long$opp_f = factor(opps_long$metric, levels=c('econ_opportunity','mean_malnutrition','mean_reliance','mariculture_opportunity'))
opp_names=c(`econ_opportunity`="Economic Opportunity",
            `mean_malnutrition`="Nutritional Opportunity",
            `mean_reliance`="Seafood Reliance",
            `mariculture_opportunity`="Mariculture Opportunity")
scorehist<-ggplot(opps_long,aes(x=score,fill=region2))+
  geom_vline(xintercept=0.5,linetype=2,col="gray70")+
  geom_histogram(bins=5,position = "dodge",alpha=0.8)+
  scale_fill_manual(values=pal,name="")+
  facet_wrap(~opp_f,labeller = as_labeller(opp_names))+
  ylab("Number of Countries")+
  xlab("Score")+
  scale_x_continuous(breaks=c(0,0.25,0.5,0.75,1))+
  theme_few()+
  theme(text = element_text(size = 18,color="black",family="Rockwell"),
        panel.border = element_blank(),
        legend.text = element_text(size=10),
        legend.position = "bottom",
        legend.key = element_blank(),
        plot.background = element_rect(linetype = 0))
scorehist
scorehist_stacked <- ggplot(opps_long,aes(x=score,fill=region2))+
  geom_vline(xintercept=0.5,linetype=2,col="gray70")+
  geom_histogram(bins=5,position = "stack",alpha=0.8)+
  scale_fill_manual(values=pal,name="")+
  facet_wrap(~opp_f,labeller = as_labeller(opp_names))+
  ylab("Number of Countries")+
  xlab("Score")+
  scale_x_continuous(breaks=c(0,0.25,0.5,0.75,1))+
  theme_few()+
  theme(text = element_text(size = 18,color="black",family="Rockwell"),
        panel.border = element_blank(),
        legend.text = element_text(size=10),
        legend.position = "bottom",
        legend.key = element_blank(),
        plot.background = element_rect(linetype = 0))
scorehist_stacked
# #ggsave("final_hists.png",scorehist,width=8)
# #ggsave("final_hists_stack.png",scorehist_stacked,width=8)


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
##ggsave("opp_hist_region.png",regionhist,width=8)
FULLDAT.NORM %>% group_by(region2) %>% summarise(n())


#### MAPS ####
library(tmap)
library(tmaptools)
library(rgdal)
library(extrafont)
library(raster)

#import shapefile and match the data to the polygons
worldmap <- readOGR(paste0(getwd(),"/mapdata"),layer="TM_WORLD_BORDERS-0.3")
mapdat <- worldmap@data
mar_dat <- FULLDAT.NORM %>% left_join(dplyr::select(mapdat,NAME,ISO3),by=c("country_name"="NAME"))
mar_dat$ISO3[mar_dat$country_name=="Sudan (former)"] <- "SDN"
mar_dat$ISO3[mar_dat$country_name=="Tanzania"] <- "TZA"
mar_dat$ISO3[mar_dat$country_name=="Cabo Verde"] <- "CPV"
mar_dat$ISO3[mar_dat$country_name=="The Gambia"] <- "GMB"
mar_dat$ISO3[mar_dat$country_name=="Iran"] <- "IRN"
mar_dat$ISO3[mar_dat$country_name=="South Korea"] <- "KOR"
mar_dat$ISO3[mar_dat$country_name=="Libya"] <- "LBY"
mar_dat$ISO3[mar_dat$country_name=="Russian Federation"] <- "RUS"

mapdat <- mapdat %>% 
  full_join(mar_dat,by="ISO3") %>%
  dplyr::select(-(FIPS:ISO2),-UN,-(NAME:POP2005))
worldmap@data <- mapdat

# world
opp_world <- tm_shape(worldmap,bbox=c(-180,180,-60,80))+
  tm_polygons("mariculture_opportunity",
          title="",colorNA="gray80",
          auto.palette.mapping = F,
          palette=rev(brewer.pal(5,"RdYlGn")),
          legend.is.portrait=T)+
  tm_layout(frame="gray80",
            bg.color = "lightblue1",
            aes.color = c(borders="grey70"),
            legend.position = c("left","center"),
            legend.text.size = 1)+
  tm_legend(title="",fontfamily="Rockwell")
opp_world

# regions
bb_car <- extent(worldmap %>% subset(SUBREGION==29))
opp_caribbean <- tm_shape(worldmap,bbox=bb_car)+
  tm_polygons("mariculture_opportunity",
              title="",colorNA="gray80",
              legend.show=F,
              auto.palette.mapping = F,
              palette=rev(brewer.pal(5,"RdYlGn")))+
  tm_layout(frame=T,
            bg.color = "lightblue1",
            aes.color = c(borders="grey60"))
opp_caribbean


bb_sea<-c(92,160,-14,20)
opp_sea <-tm_shape(worldmap,bbox=bb_sea)+
  tm_polygons("mariculture_opportunity",
              title="",colorNA="gray80",
              legend.show=F,
              auto.palette.mapping = F,
              palette=brewer.pal(5,"RdYlGn"))+
  tm_layout(frame=T,
            bg.color = "lightblue1",
            aes.color = c(borders="grey60"))
opp_sea

bb_med <-c(-9,39,28,48)
opp_med <-  tm_shape(worldmap,bbox=bb_med)+
  tm_polygons("mariculture_opportunity",
              title="",colorNA="gray80",
              legend.show=F,
              auto.palette.mapping = F,
              palette=brewer.pal(5,"RdYlGn"))+
  tm_layout(frame=T,
            bg.color = "lightblue1",
            aes.color = c(borders="grey60"))
opp_med

# Pacific islands
bb_pac<-c(155,200,-14,18)
opp_pac <-tm_shape(worldmap,bbox=bb_pac)+
  tm_polygons("mariculture_opportunity",
              title="",colorNA="gray80",
              legend.show=F,
              auto.palette.mapping = F,
              palette=brewer.pal(5,"RdYlGn"))+
  tm_layout(frame=T,
            bg.color = "lightblue1",
            aes.color = c(borders="grey60"))
opp_pac

# save maps
# save_tmap(opp_world,"world_opp.png",width=1920)
# save_tmap(opp_sea,"sea_opp.png",width=1920)
# save_tmap(opp_med,"med_opp.png",width=1920)
# save_tmap(opp_caribbean,"carr_opp.png",width=1920)

#### Ordered bar graph ####
dat <- FULLDAT.NORM
sorted_countries <-dat$country_name[match(sort(dat$mariculture_opportunity,decreasing=T),dat$mariculture_opportunity)]
dat$opp_f <- factor(dat$country_name,levels=sorted_countries)
opp_bar <- dat %>% 
  ggplot(aes(x=opp_f,y=mariculture_opportunity,fill=region2))+
  geom_bar(stat="identity")+
  xlab("Country")+
  ylab("Mariculture Opportunity")+
  ylim(0,1)+
  coord_flip()+
  scale_fill_manual(values=pal,name="")+
  theme_few()+
  theme(legend.position = c(0.8,0.9),
        legend.background = element_rect(fill=NA),
        legend.text = element_text(family="Rockwell"),
        axis.text.x = element_text(size=18,family="Rockwell"),
        axis.text.y= element_text(size=8,family="Rockwell"),
        axis.title = element_text(size=24,color="black",family="Rockwell"))
opp_bar

#ggsave("opportunity_bars.png",opp_bar,height=10)
