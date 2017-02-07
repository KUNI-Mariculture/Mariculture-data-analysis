# Clean environment

rm(list=ls(all=TRUE))

# Packages 

library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)

#####

# Set production databases 

Production <- read.csv("~/Box Sync/Proyectos/Aquaculture/Aq./AQ_production.csv", stringsAsFactors=FALSE)
  Production <- subset(Production, Production[ , 4] != "Freshwater")
  Production[,3:5]<- NULL
  
Catch <- read.csv("~/Box Sync/Proyectos/Aquaculture/Aq./F_production.csv", stringsAsFactors=FALSE)
  Catch[,3:4]<- NULL

# Flip databases
  
Production<-melt(Production, id.vars=c('country','species'),var='year')
  names(Production)[4]<-paste('Aq.prod') # in tons 
  Production$year<-as.numeric(gsub( "X", "", as.character(Production$year)))

Catch<-melt(Catch, id.vars=c('country','species'),var='year')
  names(Catch)[4]<-paste("F.prod") 
  Catch$year<-as.numeric(gsub( "X", "", as.character(Catch$year)))

# Aggregate production

Production <- ddply(Production, .(year,country),
             summarize, 
             Aq.prod = sum(Aq.prod,na.rm=TRUE))

Catch <- ddply(Catch, .(year,country),
                    summarize, 
                    F.prod = sum(F.prod,na.rm=TRUE))

# Leave only observations from 2011

Production<-Production[Production$year==2011,]

Catch<-Catch[Catch$year==2011,]

# Merge production and catch

MDB <- merge(Production, Catch,by.x=c('country','year'),by.y=c('country','year'))

#####

# Now let's deal with trade:

Trade.Q <- read.csv("~/Box Sync/Proyectos/Aquaculture/Aq./Trade_Q.csv", stringsAsFactors=FALSE)
  Trade.Q[,4]<- NULL

Trade.V <- read.csv("~/Box Sync/Proyectos/Aquaculture/Aq./Trade_Q.csv", stringsAsFactors=FALSE)  
  Trade.V[,4]<- NULL

# Flip databases as we did before

Trade.Q<-melt(Trade.Q, id.vars=c('country','commodity','flow'),var='year')
  names(Trade.Q)[5]<-paste('Trade.quantity') 
  Trade.Q$year<-as.numeric(gsub( "X", "", as.character(Trade.Q$year)))

Trade.V<-melt(Trade.V, id.vars=c('country','commodity','flow'),var='year')
  names(Trade.V)[5]<-paste('Trade.value') 
  Trade.V$year<-as.numeric(gsub( "X", "", as.character(Trade.V$year)))

# Aggregate trade

Trade.Q <- ddply(Trade.Q, .(year,country,flow),
                    summarize, 
                    Trade.quantity = sum(Trade.quantity,na.rm=TRUE))

Trade.V <- ddply(Trade.V, .(year,country,flow),
                 summarize, 
                 Trade.value = sum(Trade.value,na.rm=TRUE))

Exp.Q <- Trade.Q[Trade.Q$flow=='Exports' | Trade.Q$flow=='Reexports',]
Exp.Q <- ddply(Exp.Q, .(year,country),
                 summarize, 
                 Q.Exp = sum(Trade.quantity,na.rm=TRUE))

Exp.V <- Trade.V[Trade.V$flow=='Exports' | Trade.V$flow=='Reexports',]
Exp.V <- ddply(Exp.V, .(year,country),
               summarize, 
               V.Exp = sum(Trade.value,na.rm=TRUE))

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

# Merge with the master database

MDB <- merge(MDB, Exp.Q,by.x=c('country','year'),by.y=c('country','year'))
MDB <- merge(MDB, Exp.V,by.x=c('country','year'),by.y=c('country','year'))

MDB <- merge(MDB, Imp.Q,by.x=c('country','year'),by.y=c('country','year'))
MDB <- merge(MDB, Imp.V,by.x=c('country','year'),by.y=c('country','year'))
#####

# Population

Pop <- read.csv("~/Box Sync/Proyectos/Aquaculture/Aq./Pop.csv", stringsAsFactors=FALSE)
  Pop <- melt(Pop, id.vars=c('country'),var='year')
  names(Pop)[3] <- paste('Population') 
  Pop$year<-as.numeric(gsub( "X", "", as.character(Pop$year)))

Pop <- Pop[Pop$year==2011,]

MDB <- merge(MDB, Pop,by.x=c('country','year'),by.y=c('country','year'))

# GDP

GDP <- read.csv("~/Box Sync/Proyectos/Aquaculture/Aq./GDP.csv", stringsAsFactors=FALSE)
GDP <- melt(GDP, id.vars=c('country'),var='year')
names(GDP)[3] <- paste('GDP') 
GDP$year<-as.numeric(gsub( "X", "", as.character(GDP$year)))

GDP <- GDP[GDP$year==2011,]

MDB <- merge(MDB, GDP,by.x=c('country','year'),by.y=c('country','year'))


#####
  
# Indicators

MDB$gross.production.ratio <- MDB$Aq.prod/MDB$F.prod   

MDB$Q.trade.deficit <- MDB$Q.imp/MDB$Q.Exp

MDB$V.trade.deficit <- MDB$V.imp/MDB$V.Exp
  
MDB$willingness <- MDB$V.imp/MDB$Population

#####  

# Other databases  

DBI <- read.csv("~/Box Sync/Proyectos/Aquaculture/Aq./DBI.csv", stringsAsFactors=FALSE)

DBI <- DBI[DBI$year==2011,]

  
GOV <- read.csv("~/Box Sync/Proyectos/Aquaculture/Aq./Gov.csv", stringsAsFactors=FALSE)
  GOV<-melt(GOV, id.vars=c('country','indicator'),var='year')
  GOV$year<-as.numeric(gsub( "X", "", as.character(GOV$year)))

GOV <- GOV[GOV$year==2011,]

GOV <- dcast(GOV,country~indicator) # Converts rows into columns


MDB <- merge(MDB,DBI,by.x=c('country','year'),by.y=c('country','year'))
MDB <- merge(MDB,GOV,by.x=c('country'),by.y=c('country'))

write.csv(MDB, file = "Master_Database.csv")




