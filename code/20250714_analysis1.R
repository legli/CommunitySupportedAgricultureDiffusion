####
## written by Lukas Egli
## 14 July 2025

#### load libraries
library(xlsx)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)

#### working directory; parameters
vecCol1 <- brewer.pal(11,"PuOr")
setwd("data")

#### Temporal dynamics of the number of farms regarding different size classes and organic management
# farm size classes
dfDynamik <- read.xlsx("data/DataSource3_1_AgriculturalFarmsSizeClasses.xlsx",sheetIndex = 1)

# organic management
dfOekolandbau <- read.xlsx("DataSource2_1_OrganicFarms.xlsx",sheetIndex = 1)
dfOekolandbau$Betriebsgröße <- "organic"

# combine datasets
dfDynamik <- rbind(dfDynamik,dfOekolandbau)
dfDynamik$Betriebsgröße <- factor(dfDynamik$Betriebsgröße,levels = c("Sehr_klein", "Klein","Mittel","Groß","organic"))

# plot
fig1a <- ggplot(dfDynamik[which(dfDynamik$Jahr%in%1995:2023),], aes(x=Jahr, y=Anzahl,colour=Betriebsgröße)) + 
  geom_line(aes(linetype=Betriebsgröße))+
  labs(x="Year", y = "Number of  farms")+
  theme_bw() +
  # scale_colour_manual(values=c('#E69F00','darkred',"grey","black","green"),guide="none")+
  scale_colour_manual(values=c(vecCol1[c(10,8,2,4)],"black"),guide="none")+
  scale_linetype_manual(values=c(1,1,1,1,2),guide="none")+
    theme(text=element_text(size=8))+
  scale_x_continuous(breaks=c(1995,2000,2005,2010,2015,2020), labels = c(1995,2000,2005,2010,2015,2020))



#### Temporal dynamics of vegetable and CSA farms
# vegetable farms
dfDynamikVeg <- read.xlsx("DataSource3_3_VegetableFarms.xlsx",sheetIndex = 1)
dfDynamikVeg$type <- "Vegetable farms"

# CSSA farms
dfCSA <- read.xlsx("DataSource2_1_CSAFarms.xlsx",sheetIndex = 1)
dfCSA <- dfCSA[order(dfCSA$Jahr),]
dfCSA$Anzahl <- cumsum(dfCSA$AnzahlJahr)
dfCSA <- dfCSA[which(dfCSA$Jahr%in%2010:2023),]
head(dfCSA)
dfCSA$type <- "CSAs"

# combine datasets
dfFinal <- rbind(dfDynamikVeg[,c("Jahr","Anzahl","type")],dfCSA[,c("Jahr","Anzahl","type")])

# plot
fig1b <- ggplot(dfFinal, aes(x=Jahr, y=Anzahl,colour=type)) + 
  geom_line(aes(linetype=type))+
  labs(x="Year", y = "Number of vegetable farms")+
  theme_bw() +
  scale_colour_manual(values=vecCol1[c(10,2)],guide="none")+
  scale_linetype_manual(values=c(2,1),guide="none")+
  theme(text=element_text(size=8))+
  scale_x_continuous(breaks=c(2010,2015,2020), labels = c(2010,2015,2020))


#### Export figure
setwd("C:/Users/egli/sciebo - Egli, Lukas (lukas.egli@ufz.de)@gast.sciebo.de/00 Datenerhebung/02 Empirie/08 Potenzialanalyse/Ergebnisse")

jpeg("20250611_QuantitativeUmfrage_KurzumfragePotenziale_Figure1_v1.jpeg",width = 16.9*(2/3), height = 8,units = 'cm', res = 600)
  ggarrange(fig1a, fig1b,ncol = 2, nrow = 1,labels=c("a","b"),
            font.label = list(size = 8, color = "black", face = "bold", family = NULL))
dev.off()



rm(list=ls())

