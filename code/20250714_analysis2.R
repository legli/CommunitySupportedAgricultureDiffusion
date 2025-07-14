####
## written by Lukas Egli
## 14 July 2025


#### load libraries
library(xlsx)
library(ggplot2)
library(lme4)
library(dplyr)
library(tidyr)
library(ggpubr)


#### Import and prepare data
## parameters
numberCSA <- 481
populationGermany <- 83456045
vssActual <- 0.38

## working directory
setwd("C:/Users/egli/sciebo - Egli, Lukas (lukas.egli@ufz.de)@gast.sciebo.de/00 Datenerhebung/02 Empirie/08 Potenzialanalyse/Daten")

## proportion of vegetable farms
dfVegetablesProp <- read.xlsx("DataSource3_5_VegetableFarmsFarmType.xlsx",sheetIndex = 1)

## farm area CSA
dfVeg <- read.xlsx("DataSource2_23_CSAFarmsProportionVegetables.xlsx",sheetIndex=1)

## area demand per state
dfFlaechenbedarf <- read.xlsx("DataSource3_67_PerCapitaAreaDemand.xlsx",sheetIndex=1)
# interpolate missing values with mean values
dfFlaechenbedarf <- rbind(dfFlaechenbedarf,data.frame(Bundesland=c("Bremen","Berlin"),Flaechenbedarf_ha=c(mean(dfFlaechenbedarf$Flaechenbedarf_ha),mean(dfFlaechenbedarf$Flaechenbedarf_ha))))

## vegetable farms
dfDynamikVeg <- read.xlsx("DataSource3_30_VegetableFarms.xlsx",sheetIndex = 1)
vegetableFarmsAll2023 <- dfDynamikVeg[which(dfDynamikVeg$Jahr==2023),"Anzahl"]

## agrarstrukturerhebung
dfAgrarFinal <- read.xlsx("DataSource3_40_AgriculturalFarmsFarmType.xlsx",sheetIndex = 1)

# adapt categories based on model
dfAgrarFinal$BetriebsgrößeOrig <- dfAgrarFinal$Betriebsgröße
dfAgrarFinal$Betriebsgröße[which(dfAgrarFinal$Betriebsgröße%in%c("Weniger als 5 Hektar","5 bis 10 Hektar"))] <- "sehr_klein"
dfAgrarFinal$Betriebsgröße[which(dfAgrarFinal$Betriebsgröße%in%c("10 bis 20 Hektar","20 bis 50 Hektar"))] <- "klein"
dfAgrarFinal$Betriebsgröße[which(dfAgrarFinal$Betriebsgröße%in%c("50 bis 100 Hektar"))] <- "mittel"
dfAgrarFinal$Betriebsgröße[which(dfAgrarFinal$Betriebsgröße%in%c("100 bis 200 Hektar","200 bis 500 Hektar","500 bis 1000 Hektar","1000 Hektar und mehr"))]<- "groß"
table(dfAgrarFinal$Betriebsgröße)
head(dfAgrarFinal)

## add regions (https://apps.datev.de/help-center/documents/1022544)
dfAgrarFinal[which(dfAgrarFinal$Gebiet%in%c("Nordrhein-Westfalen","Hessen","Rheinland-Pfalz","Saarland")),"Region"] <- "West"
dfAgrarFinal[which(dfAgrarFinal$Gebiet%in%c("Bremen","Hamburg","Schleswig-Holstein","Niedersachsen")),"Region"] <- "Nord"
dfAgrarFinal[which(dfAgrarFinal$Gebiet%in%c("Bayern","Baden-Württemberg")),"Region"] <- "Süd"
dfAgrarFinal[which(dfAgrarFinal$Gebiet%in%c("Berlin","Brandenburg","Sachsen","Thüringen","Sachsen-Anhalt","Mecklenburg-Vorpommern")),"Region"] <- "Ost"
dfAgrarFinal$Region <- factor(dfAgrarFinal$Region, levels = c("Nord","Ost","Süd","West"))
table(dfAgrarFinal$Region )

dfAgrarFinal$BetriebswirtschaftlicheAusrichtungAgg <- dfAgrarFinal$BetriebswirtschaftlicheAusrichtung
dfAgrarFinal$BetriebswirtschaftlicheAusrichtungAgg[which(dfAgrarFinal$BetriebswirtschaftlicheAusrichtungAgg%in%c("Gartenbaubetrieb","Dauerkulturbetrieb"))]<- "BWA_häufig"
dfAgrarFinal$BetriebswirtschaftlicheAusrichtungAgg[which(dfAgrarFinal$BetriebswirtschaftlicheAusrichtungAgg%in%c("Pflanzenbauverbundbetrieb","Viehhaltungsverbundbetrieb","Ackerbaubetrieb","Pflanzenbau-Viehhaltungsverbundbetrieb"))]<- "BWA_mittel"
dfAgrarFinal$BetriebswirtschaftlicheAusrichtungAgg[which(dfAgrarFinal$BetriebswirtschaftlicheAusrichtungAgg%in%c("Futterbaubetrieb","Veredlungsbetrieb"))]<- "BWA_selten"
table(dfAgrarFinal$BetriebswirtschaftlicheAusrichtungAgg)
sum(dfAgrarFinal$Anzahl)

############################ Run analysis

#### make model predictions
bootnum <- 100
set.seed(9999)

modelLME <- readRDS(file = "modelConversion.rda")

dfPredictionBase <- dfAgrarFinal
dfPredictionBase$wahrscheinlichkeit <- predict(modelLME,dfPredictionBase,type="response")
dfPredictionBase$AnzahlUmstellung <- round(dfPredictionBase$Anzahl*dfPredictionBase$wahrscheinlichkeit,0)
dfPredictionBase$FlaecheTotal <- dfPredictionBase$Anzahl*dfPredictionBase$BetriebsgrößeDurchschnitt
dfPredictionBase$FlaecheUmstellung <- dfPredictionBase$AnzahlUmstellung*dfPredictionBase$BetriebsgrößeDurchschnitt

boot_preds <- bootMer(modelLME,
                      FUN = function(fit) {
                        predict(fit, newdata = dfAgrarFinal, re.form = NULL,type="response")
                      },
                      nsim = bootnum,
                      type = "parametric",
                      use.u = TRUE,
                      seed = 123)


lsPrediction <-   lapply(1:bootnum,function(i){
  print(i)
  dfRun <- dfAgrarFinal
  # predict model with German-wide data
  dfRun$wahrscheinlichkeit <-  boot_preds$t[i,]
  head(dfRun)

    # calculate number of farms
  dfRun$AnzahlUmstellung <- round(dfRun$Anzahl*dfRun$wahrscheinlichkeit,0)
  sum(is.na(dfRun$AnzahlUmstellung))
  sum(dfRun$AnzahlUmstellung)
  nrow(dfRun)
  head(dfRun)
  
  nrow(unique(dfRun[,c("Gebiet","BetriebswirtschaftlicheAusrichtung","Betriebsgröße","Wirtschaftsweise")]))
  head(dfRun)
  dfRun$FlaecheTotal <- dfRun$Anzahl*dfRun$BetriebsgrößeDurchschnitt
  dfRun$FlaecheUmstellung <- dfRun$AnzahlUmstellung*dfRun$BetriebsgrößeDurchschnitt
  
  sum(dfRun$FlaecheUmstellung,na.rm = T)/sum(dfRun$FlaecheTotal,na.rm = T)
  sum(dfRun$AnzahlUmstellung,na.rm=T)/sum(dfRun$Anzahl,na.rm=T)
  
  dfRun <- dfRun[,c("Gebiet","BetriebswirtschaftlicheAusrichtung","BetriebswirtschaftlicheAusrichtungAgg","Betriebsgröße","BetriebsgrößeOrig","Wirtschaftsweise","Anzahl","AnzahlUmstellung","FlaecheTotal","FlaecheUmstellung")]
  dfRun$run <- i
  dfRun
  # }
})

dfPrediction <- do.call(rbind,lsPrediction) 
head(dfPrediction)
nrow(dfPrediction)
aggregate(AnzahlUmstellung~run,dfPrediction,sum)
mean(aggregate(AnzahlUmstellung~run,dfPrediction,sum)[,2])/sum(dfPredictionBase$Anzahl)
sd(aggregate(AnzahlUmstellung~run,dfPrediction,sum)[,2])/sum(dfPredictionBase$Anzahl)

sum(dfPredictionBase$AnzahlUmstellung)
sum(dfPredictionBase$AnzahlUmstellung)/sum(dfPredictionBase$Anzahl)
sum(dfPredictionBase$FlaecheUmstellung)/sum(dfPredictionBase$FlaecheTotal)



##### Description of converted farms

dfBWA <- dfPrediction %>%
  group_by(BetriebswirtschaftlicheAusrichtung,run) %>%
  summarise(
    Anzahl  = sum(Anzahl,na.rm=T),
    Flaeche  = sum(FlaecheTotal,na.rm=T),
    AnzahlUmstellung  = sum(AnzahlUmstellung,na.rm=T),
    FlaecheUmstellung  = sum(FlaecheUmstellung,na.rm=T)
  ) %>% 
  group_by(BetriebswirtschaftlicheAusrichtung)  %>%
  summarise(
    Anzahl_mean  = mean(Anzahl,na.rm=T),
    Flaeche_mean  = mean(Flaeche,na.rm=T),
    AnzahlUmstellung_mean = mean(AnzahlUmstellung,na.rm=T),
    FlaecheUmstellung_mean = mean(FlaecheUmstellung,na.rm=T),
    AnzahlUmstellung_sd = sd(AnzahlUmstellung,na.rm=T),
    FlaecheUmstellung_sd = sd(FlaecheUmstellung,na.rm=T)
    )
names(dfBWA)[1] <- "Kategorie"

dfBWAReg <- dfPrediction %>%
  group_by(BetriebswirtschaftlicheAusrichtung,run,Gebiet) %>%
  summarise(
    Anzahl  = sum(Anzahl,na.rm=T),
    Flaeche  = sum(FlaecheTotal,na.rm=T),
    AnzahlUmstellung  = sum(AnzahlUmstellung,na.rm=T),
    FlaecheUmstellung  = sum(FlaecheUmstellung,na.rm=T)
  ) %>% 
  group_by(BetriebswirtschaftlicheAusrichtung,Gebiet)  %>%
  summarise(
    Anzahl_mean  = mean(Anzahl,na.rm=T),
    Flaeche_mean  = mean(Flaeche,na.rm=T),
    AnzahlUmstellung_mean = mean(AnzahlUmstellung,na.rm=T),
    FlaecheUmstellung_mean = mean(FlaecheUmstellung,na.rm=T),
    AnzahlUmstellung_sd = sd(AnzahlUmstellung,na.rm=T),
    FlaecheUmstellung_sd = sd(FlaecheUmstellung,na.rm=T)
  )
names(dfBWAReg)[1] <- "Kategorie"


dfBetriebsgröße <- dfPrediction %>%
  group_by(Betriebsgröße,run) %>%
  summarise(
    Anzahl  = sum(Anzahl,na.rm=T),
    Flaeche  = sum(FlaecheTotal,na.rm=T),
    AnzahlUmstellung  = sum(AnzahlUmstellung,na.rm=T),
    FlaecheUmstellung  = sum(FlaecheUmstellung,na.rm=T)
  ) %>% 
  group_by(Betriebsgröße)  %>%
  summarise(
    Anzahl_mean  = mean(Anzahl,na.rm=T),
    Flaeche_mean  = mean(Flaeche,na.rm=T),
    AnzahlUmstellung_mean = mean(AnzahlUmstellung,na.rm=T),
    FlaecheUmstellung_mean = mean(FlaecheUmstellung,na.rm=T),
    AnzahlUmstellung_sd = sd(AnzahlUmstellung,na.rm=T),
    FlaecheUmstellung_sd = sd(FlaecheUmstellung,na.rm=T)
  )
names(dfBetriebsgröße)[1] <- "Kategorie"

dfBetriebsgrößeReg <- dfPrediction %>%
  group_by(Betriebsgröße,run,Gebiet) %>%
  summarise(
    Anzahl  = sum(Anzahl,na.rm=T),
    Flaeche  = sum(FlaecheTotal,na.rm=T),
    AnzahlUmstellung  = sum(AnzahlUmstellung,na.rm=T),
    FlaecheUmstellung  = sum(FlaecheUmstellung,na.rm=T)
  ) %>% 
  group_by(Betriebsgröße,Gebiet)  %>%
  summarise(
    Anzahl_mean  = mean(Anzahl,na.rm=T),
    Flaeche_mean  = mean(Flaeche,na.rm=T),
    AnzahlUmstellung_mean = mean(AnzahlUmstellung,na.rm=T),
    FlaecheUmstellung_mean = mean(FlaecheUmstellung,na.rm=T),
    AnzahlUmstellung_sd = sd(AnzahlUmstellung,na.rm=T),
    FlaecheUmstellung_sd = sd(FlaecheUmstellung,na.rm=T)
  )
names(dfBetriebsgrößeReg)[1] <- "Kategorie"


dfWirtschaftsweise <- dfPrediction %>%
  group_by(Wirtschaftsweise,run) %>%
  summarise(
    Anzahl  = sum(Anzahl,na.rm=T),
    Flaeche  = sum(FlaecheTotal,na.rm=T),
    AnzahlUmstellung  = sum(AnzahlUmstellung,na.rm=T),
    FlaecheUmstellung  = sum(FlaecheUmstellung,na.rm=T)
  ) %>% 
  group_by(Wirtschaftsweise)  %>%
  summarise(
    Anzahl_mean  = mean(Anzahl,na.rm=T),
    Flaeche_mean  = mean(Flaeche,na.rm=T),
    AnzahlUmstellung_mean = mean(AnzahlUmstellung,na.rm=T),
    FlaecheUmstellung_mean = mean(FlaecheUmstellung,na.rm=T),
    AnzahlUmstellung_sd = sd(AnzahlUmstellung,na.rm=T),
    FlaecheUmstellung_sd = sd(FlaecheUmstellung,na.rm=T)
  )
names(dfWirtschaftsweise)[1] <- "Kategorie"

dfWirtschaftsweiseReg <- dfPrediction %>%
  group_by(Wirtschaftsweise,run,Gebiet) %>%
  summarise(
    Anzahl  = sum(Anzahl,na.rm=T),
    Flaeche  = sum(FlaecheTotal,na.rm=T),
    AnzahlUmstellung  = sum(AnzahlUmstellung,na.rm=T),
    FlaecheUmstellung  = sum(FlaecheUmstellung,na.rm=T)
  ) %>% 
  group_by(Wirtschaftsweise,Gebiet)  %>%
  summarise(
    Anzahl_mean  = mean(Anzahl,na.rm=T),
    Flaeche_mean  = mean(Flaeche,na.rm=T),
    AnzahlUmstellung_mean = mean(AnzahlUmstellung,na.rm=T),
    FlaecheUmstellung_mean = mean(FlaecheUmstellung,na.rm=T),
    AnzahlUmstellung_sd = sd(AnzahlUmstellung,na.rm=T),
    FlaecheUmstellung_sd = sd(FlaecheUmstellung,na.rm=T)
  )
names(dfWirtschaftsweiseReg)[1] <- "Kategorie"


dfGebiet <- dfPrediction %>%
  group_by(Gebiet,run) %>%
  summarise(
    Anzahl  = sum(Anzahl,na.rm=T),
    Flaeche  = sum(FlaecheTotal,na.rm=T),
    AnzahlUmstellung  = sum(AnzahlUmstellung,na.rm=T),
    FlaecheUmstellung  = sum(FlaecheUmstellung,na.rm=T)
  ) %>% 
  group_by(Gebiet)  %>%
  summarise(
    Anzahl_mean  = mean(Anzahl,na.rm=T),
    Flaeche_mean  = mean(Flaeche,na.rm=T),
    AnzahlUmstellung_mean = mean(AnzahlUmstellung,na.rm=T),
    FlaecheUmstellung_mean = mean(FlaecheUmstellung,na.rm=T),
    AnzahlUmstellung_sd = sd(AnzahlUmstellung,na.rm=T),
    FlaecheUmstellung_sd = sd(FlaecheUmstellung,na.rm=T)
  )
names(dfGebiet)[1] <- "Kategorie"


table1 <- rbind(dfBWA,dfBetriebsgröße,dfWirtschaftsweise)
table1$ratioUmstellung <- (table1$AnzahlUmstellung_mean/table1$Anzahl_mean)*100
table1$ratioUmstellung_sd <- (table1$AnzahlUmstellung_sd/table1$Anzahl_mean)*100

table1$ratioUmstellungFlaeche <- (table1$FlaecheUmstellung_mean/table1$Flaeche_mean)*100
table1$ratioUmstellungFlaeche_sd <- (table1$FlaecheUmstellung_sd/table1$Flaeche_mean)*100

table1$ratioUmstellungFlaecheTotal <- (table1$FlaecheUmstellung_mean/sum(dfWirtschaftsweise$Flaeche_mean))*100
sum(table1$ratioUmstellungFlaecheTotal)
table1$ratioUmstellungFlaecheTotal_sd <- (table1$FlaecheUmstellung_sd/sum(dfWirtschaftsweise$Flaeche_mean))*100

table1[,4:13] <- round(table1[,4:13],2)
table1$ratioUmstellung_all <- paste0(table1$ratioUmstellung," (",table1$ratioUmstellung_sd,")")
table1$ratioUmstellungFlaeche_all <- paste0(table1$ratioUmstellungFlaeche," (",table1$ratioUmstellungFlaeche_sd,")")
table1$ratioUmstellungFlaecheTotal_all <- paste0(table1$ratioUmstellungFlaecheTotal," (",table1$ratioUmstellungFlaecheTotal_sd,")")

# reg

table1Reg <- rbind(dfBWAReg,dfBetriebsgrößeReg,dfWirtschaftsweiseReg)
table1Reg$ratioUmstellung <- (table1Reg$AnzahlUmstellung_mean/table1Reg$Anzahl_mean)*100
table1Reg$ratioUmstellung_sd <- (table1Reg$AnzahlUmstellung_sd/table1Reg$Anzahl_mean)*100

table1Reg$ratioUmstellungFlaeche <- (table1Reg$AnzahlUmstellung_mean/table1Reg$Flaeche_mean)*100
table1Reg$ratioUmstellungFlaeche_sd <- (table1Reg$FlaecheUmstellung_sd/table1Reg$Flaeche_mean)*100

dfWirtschaftsweiseRegAgg <- aggregate(Flaeche_mean~Gebiet,dfWirtschaftsweiseReg,sum)
names(dfWirtschaftsweiseRegAgg)[2] <- "FlaecheTotalGebiet"
table1Reg <- merge(table1Reg,dfWirtschaftsweiseRegAgg)
table1Reg$ratioUmstellungFlaecheTotal <- (table1Reg$FlaecheUmstellung_mean/table1Reg$FlaecheTotalGebiet)*100
sum(table1Reg$ratioUmstellungFlaecheTotal)
table1Reg$ratioUmstellungFlaecheTotal_sd <- (table1Reg$FlaecheUmstellung_sd/table1Reg$FlaecheTotalGebiet)*100

names(table1Reg)
table1Reg[,4:15] <- round(table1Reg[,4:15],2)
table1Reg$ratioUmstellung_all <- paste0(table1Reg$ratioUmstellung," (",table1Reg$ratioUmstellung_sd,")")
table1Reg$ratioUmstellungFlaeche_all <- paste0(table1Reg$ratioUmstellungFlaeche," (",table1Reg$ratioUmstellungFlaeche_sd,")")
table1Reg$ratioUmstellungFlaecheTotal_all <- paste0(table1Reg$ratioUmstellungFlaecheTotal," (",table1Reg$ratioUmstellungFlaecheTotal_sd,")")
table1Reg$Kategorie <- factor(table1Reg$Kategorie, levels= table1$Kategorie)
table1Reg <- table1Reg[order(table1Reg$Kategorie),]
table1Reg <- table1Reg[order(table1Reg$Gebiet),]


####### vegetable self sufficiency 
## parameters

baseMenschen <- vssActual*populationGermany

## calculate vss for different model runs
lsSupply <-   lapply(1:bootnum,function(i){
  print(i)
  dfRun <- dfPrediction[which(dfPrediction$run==i),]
  dfRun <- merge(dfRun,dfVegetablesProp,all.x=T)
  dfRun[which(is.na(dfRun$propNotVeg)),"propNotVeg"] <- 1
  
  lsSample <- lapply(1:bootnum, function(j){
    dfVegSample <- dfVeg
    dfVegSample$propVeg <- rnorm(nrow(dfVegSample),dfVegSample$propVeg,dfVegSample$propVegSD)
    dfVegSample[which(dfVegSample$propVeg<0),"propVeg"] <- 0
    dfVegSample[which(dfVegSample$propVeg>1),"propVeg"] <- 1
    dfRun <- merge(dfRun,dfVegSample[,c("BetriebsgrößeOrig","propVeg")])
    dfRun$AnzahlGemuese <- dfRun$Anzahl*(1-dfRun$propNotVeg) # existing vegetable farms
    dfRun$AnzahlGemueseTot <- rowSums(dfRun[,c("AnzahlGemuese","AnzahlUmstellung")])
    dfRun$FlaecheUmstellungsbetriebeTot <- dfRun$FlaecheUmstellung*dfRun$propVeg*dfRun$propNotVeg # actual area that becomes newly available for vegetable production
  
    # maximum on full area
    dfPredictionRunMax <- aggregate(cbind(AnzahlUmstellung,AnzahlGemueseTot,FlaecheUmstellungsbetriebeTot)~Gebiet,dfRun,sum)
    dfPredictionRunMax <- merge(dfPredictionRunMax,dfFlaechenbedarf,by.x="Gebiet",by.y="Bundesland")
    dfPredictionRunMax$AnzahlMenschen <- dfPredictionRunMax$FlaecheUmstellungsbetriebe/dfPredictionRunMax$Flaechenbedarf_ha
    dfPredictionRunMax$FlaecheUmstellungsbetriebe <- dfPredictionRunMax$FlaecheUmstellungsbetriebeTot
  
    dfFinalSample <- dfPredictionRunMax[,c("Gebiet","AnzahlUmstellung","AnzahlGemueseTot","FlaecheUmstellungsbetriebe","AnzahlMenschen")]
    dfFinalSample$sample <- j
    dfFinalSample
  })
  dfFinal <- do.call(rbind,lsSample) 
  
  dfFinal$run <- i
  dfFinal
})
dfSupply <- do.call(rbind,lsSupply) 
head(dfSupply)

## calculate vss
dfSVG <- aggregate(cbind(AnzahlUmstellung,AnzahlGemueseTot,AnzahlMenschen,AnzahlMenschen,FlaecheUmstellungsbetriebe)~run+sample,dfSupply,sum)
head(dfSVG)
dfSVGSummary <- data.frame(anzahlGemuese=mean(dfSVG$AnzahlGemueseTot),
                           sdAnzahlGemuese=sd(dfSVG$AnzahlGemueseTot),
                           anzahlUmstellung=mean(dfSVG$AnzahlUmstellung),
                           sdAnzahlUmstellung=sd(dfSVG$AnzahlUmstellung),
                           menschen=mean(dfSVG$AnzahlMenschen)/populationGermany,
                           sdMenschen=sd(dfSVG$AnzahlMenschen)/populationGermany,
                           flaeche=mean(dfSVG$FlaecheUmstellungsbetriebe),
                           sdFlaeche=sd(dfSVG$FlaecheUmstellungsbetriebe),
                           vss=mean(dfSVG$AnzahlMenschen+baseMenschen)/populationGermany,
                           sdVSS =sd(dfSVG$AnzahlMenschen+baseMenschen)/populationGermany,
                           scenario="Modell"
)

# dfSVGfinal <- rbind(dfSVGSummary,dfSVGAreaSummary)
dfSVGfinal <- dfSVGSummary[,c(1:6,9:11)]
dfSVGfinal$menschen <- dfSVGfinal$menschen*100
dfSVGfinal$sdMenschen <- dfSVGfinal$sdMenschen*100
dfSVGfinal$vss <- dfSVGfinal$vss*100
dfSVGfinal$sdVSS <- dfSVGfinal$sdVSS*100
# calculate differences
dfSVGfinal$anzahlGemuese <- dfSVGfinal$anzahlGemuese-dfSVGfinal$anzahlUmstellung
dfSVGfinal$menschen <- dfSVGfinal$menschen-0.25
dfSVGfinal$vss <- dfSVGfinal$vss-(vssActual*100)

dfSVGfinal <- rbind(dfSVGfinal,data.frame(anzahlGemuese=vegetableFarmsAll2023,sdAnzahlGemuese=0,
                    anzahlUmstellung=numberCSA,sdAnzahlUmstellung=0,
                    menschen=0.25, sdMenschen = 0,
                    vss = vssActual*100,sdVSS =0,
                    scenario="StatusQuo"))
dfSVGfinalAnzahl <- rbind(dfSVGfinal[1,c(1:4,9)],dfSVGfinal[2,c(1:4,9)])
dfSVGfinalAnzahl1 <- dfSVGfinalAnzahl[,c(1,2,5)]
names(dfSVGfinalAnzahl1) <- c("Anzahl","SD","scenario")
dfSVGfinalAnzahl1$Variable <- "Vegetable farms"
dfSVGfinalAnzahl2 <- dfSVGfinalAnzahl[,c(3,4,5)]
names(dfSVGfinalAnzahl2) <- c("Anzahl","SD","scenario")
dfSVGfinalAnzahl2$Variable <- "Converted farms"
dfSVGfinalAnzahl <- rbind(dfSVGfinalAnzahl1,dfSVGfinalAnzahl2)
dfSVGfinalAnzahl$Variable <- factor(dfSVGfinalAnzahl$Variable,levels = c("Vegetable farms","Converted farms"))

dfSVGfinalAnzahl$AnzahlAdapted <- dfSVGfinalAnzahl$Anzahl
dfSVGfinalAnzahl[which(dfSVGfinalAnzahl$scenario=="Modell"&dfSVGfinalAnzahl$Variable=="Vegetable farms"),"AnzahlAdapted"] <- dfSVGfinalAnzahl[which(dfSVGfinalAnzahl$scenario=="StatusQuo"&dfSVGfinalAnzahl$Variable=="Vegetable farms"),"AnzahlAdapted"]+dfSVGfinalAnzahl[which(dfSVGfinalAnzahl$scenario=="Modell"&dfSVGfinalAnzahl$Variable=="Vegetable farms"),"AnzahlAdapted"]
dfSVGfinalAnzahl[which(dfSVGfinalAnzahl$scenario=="Modell"&dfSVGfinalAnzahl$Variable=="Converted farms"),"AnzahlAdapted"] <- dfSVGfinalAnzahl[which(dfSVGfinalAnzahl$scenario=="StatusQuo"&dfSVGfinalAnzahl$Variable=="Converted farms"),"AnzahlAdapted"]+dfSVGfinalAnzahl[which(dfSVGfinalAnzahl$scenario=="Modell"&dfSVGfinalAnzahl$Variable=="Converted farms"),"AnzahlAdapted"]


fig2a <- ggplot(dfSVGfinalAnzahl, aes(x=Variable, y=Anzahl,fill=scenario)) + 
  geom_bar(stat="identity", color="black", 
           position="stack",alpha=0.5) +
  geom_errorbar(
    data = subset(dfSVGfinalAnzahl, scenario == "Modell"), 
    aes(ymin = AnzahlAdapted-SD, ymax = AnzahlAdapted+SD), 
    width = 0.2, size = 0.3
  ) +
  labs(x="", y = "Number")+
  theme_bw() +
  scale_fill_manual(values=c('#E69F00','#999999'),guide="none")+
  ylim(0,25000)+
  theme(text=element_text(size=8))

dfSVGfinalRel <- rbind(dfSVGfinal[1,c(5:9)],dfSVGfinal[2,c(5:9)])
dfSVGfinalRel1 <- dfSVGfinalRel[,c(1,2,5)]
names(dfSVGfinalRel1) <- c("Anzahl","SD","scenario")
dfSVGfinalRel1$Variable <- "Members"
dfSVGfinalRel2 <- dfSVGfinalRel[,c(3,4,5)]
names(dfSVGfinalRel2) <- c("Anzahl","SD","scenario")
dfSVGfinalRel2$Variable <- "VSS"
dfSVGfinalRel <- rbind(dfSVGfinalRel1,dfSVGfinalRel2)
dfSVGfinalRel$Variable <- factor(dfSVGfinalRel$Variable,levels = c("Members","VSS"))
dfSVGfinalRel$AnzahlAdapted <- dfSVGfinalRel$Anzahl
dfSVGfinalRel[which(dfSVGfinalRel$scenario=="Modell"&dfSVGfinalRel$Variable=="Members"),"AnzahlAdapted"] <- dfSVGfinalRel[which(dfSVGfinalRel$scenario=="StatusQuo"&dfSVGfinalRel$Variable=="Members"),"AnzahlAdapted"]+dfSVGfinalRel[which(dfSVGfinalRel$scenario=="Modell"&dfSVGfinalRel$Variable=="Members"),"AnzahlAdapted"]
dfSVGfinalRel[which(dfSVGfinalRel$scenario=="Modell"&dfSVGfinalRel$Variable=="VSS"),"AnzahlAdapted"] <- dfSVGfinalRel[which(dfSVGfinalRel$scenario=="StatusQuo"&dfSVGfinalRel$Variable=="VSS"),"AnzahlAdapted"]+dfSVGfinalRel[which(dfSVGfinalRel$scenario=="Modell"&dfSVGfinalRel$Variable=="VSS"),"AnzahlAdapted"]


fig2b <- ggplot(dfSVGfinalRel, aes(x=Variable, y=Anzahl,fill=scenario)) + 
  geom_bar(stat="identity", color="black", 
           position="stack",alpha=0.5) +
  geom_errorbar(
    data = subset(dfSVGfinalRel, scenario == "Modell"), 
    aes(ymin = AnzahlAdapted-SD, ymax = AnzahlAdapted+SD), 
    width = 0.2, size = 0.3
  )+
  labs(x="", y = "Percentage")+
  theme_bw() +
  scale_fill_manual(values=c('#E69F00','#999999'),guide="none")+
  ylim(0,60)+
  theme(text=element_text(size=8))


## different target scenarios

dfBarrier <- read.xlsx("C:/Users/egli/sciebo - Egli, Lukas (lukas.egli@ufz.de)@gast.sciebo.de/00 Datenerhebung/02 Empirie/05 Umfragen/AP44_Clusteranalyse/Tabelle2.xlsx",sheetIndex=1)
head(dfBarrier)
dfPredictionScenario <- merge(unique(dfAgrarFinal[,c("BetriebswirtschaftlicheAusrichtungAgg","Betriebsgröße","Wirtschaftsweise")]),dfBarrier,all.x=T)
nrow(dfPredictionScenario)
rowNA <- which(is.na(dfPredictionScenario$meanBarrier))
dfPredictionScenario[which(is.na(dfPredictionScenario$meanBarrier)),c("BetriebswirtschaftlicheAusrichtungAgg","Betriebsgröße","Wirtschaftsweise")]
# interpolate mean barriers
for (row in rowNA){
  scores <- apply(dfBarrier[,c("BetriebswirtschaftlicheAusrichtungAgg","Betriebsgröße","Wirtschaftsweise")], 1, function(row2) sum(dfPredictionScenario[row,c("BetriebswirtschaftlicheAusrichtungAgg","Betriebsgröße","Wirtschaftsweise")] == row2))
  print(max(scores))
  best_indices <- which(scores == max(scores))
  dfPredictionScenario[row,"meanBarriers"] <- mean(dfBarrier[best_indices,"meanBarriers"],na.rm=T)
}
sum(is.na(dfPredictionScenario$meanBarrier))
mean(dfPredictionScenario$meanBarrier,na.rm=T)
range(dfPredictionScenario$meanBarrier,na.rm=T)
minBarrier <- min(dfPredictionScenario$meanBarrier,na.rm=T)
diffRel <- diff(range(dfPredictionScenario$meanBarrier,na.rm=T))/3
dfPredictionScenario$barrierCat <- "low"
# dfPredictionScenario[which(dfPredictionScenario$meanBarriers>=1),"barrierCat"] <- "very_low"
dfPredictionScenario[which(dfPredictionScenario$meanBarriers>=3),"barrierCat"] <- "medium"
dfPredictionScenario[which(dfPredictionScenario$meanBarriers>=3.5),"barrierCat"] <- "high"
table(dfPredictionScenario$barrierCat)
sum(is.na(dfPredictionScenario$barrierCat))
head(dfPredictionScenario)


dfPrediction <- merge(dfPrediction,dfPredictionScenario)

## function to model for different target farms 
functionScenario <-   function(columnName, vecCategory, scenarioName, scenarioNumber){
  
  lsSupply <-   lapply(1:bootnum,function(i){
    print(i)
    dfRun <- dfPrediction[which(dfPrediction$run==i&dfPrediction[,columnName]%in%c(vecCategory)),]
    dfRun <- merge(dfRun,dfVegetablesProp,all.x=T)
    dfRun[which(is.na(dfRun$propNotVeg)),"propNotVeg"] <- 1
    
    lsSample <- lapply(1:bootnum, function(j){
      dfVegSample <- dfVeg
      dfVegSample$propVeg <- rnorm(nrow(dfVegSample),dfVegSample$propVeg,dfVegSample$propVegSD)
      dfVegSample[which(dfVegSample$propVeg<0),"propVeg"] <- 0
      dfVegSample[which(dfVegSample$propVeg>1),"propVeg"] <- 1
      dfRun <- merge(dfRun,dfVegSample[,c("BetriebsgrößeOrig","propVeg")])
      dfRun$AnzahlGemuese <- dfRun$Anzahl*(1-dfRun$propNotVeg) # existing vegetable farms
      dfRun$AnzahlGemueseTot <- rowSums(dfRun[,c("AnzahlGemuese","AnzahlUmstellung")])
      dfRun$FlaecheUmstellungsbetriebeTot <- dfRun$FlaecheUmstellung*dfRun$propVeg*dfRun$propNotVeg # actual area that becomes newly available for vegetable production
      
      # maximum on full area
      dfPredictionRunMax <- aggregate(cbind(AnzahlUmstellung,AnzahlGemueseTot,FlaecheUmstellungsbetriebeTot)~Gebiet,dfRun,sum)
      dfPredictionRunMax <- merge(dfPredictionRunMax,dfFlaechenbedarf,by.x="Gebiet",by.y="Bundesland")
      dfPredictionRunMax$AnzahlMenschen <- dfPredictionRunMax$FlaecheUmstellungsbetriebe/dfPredictionRunMax$Flaechenbedarf_ha
      dfPredictionRunMax$FlaecheUmstellungsbetriebe <- dfPredictionRunMax$FlaecheUmstellungsbetriebeTot
      
      dfFinalSample <- dfPredictionRunMax[,c("Gebiet","AnzahlUmstellung","AnzahlGemueseTot","FlaecheUmstellungsbetriebe","AnzahlMenschen")]
      data.frame(svg=(sum(dfFinalSample$AnzahlMenschen)+baseMenschen)/populationGermany,sample=j)
    })
    dfFinal <- do.call(rbind,lsSample) 
    dfFinal$run <- i
    dfFinal
  })
  dfSupply <- do.call(rbind,lsSupply) 

  dfSupply$scenario <- scenarioName
  dfSupply$xvalue <- scenarioNumber
  dfSupply
}

dfScenario <- rbind(
  functionScenario("BetriebswirtschaftlicheAusrichtungAgg","BWA_häufig","BWA",1),
  functionScenario("BetriebswirtschaftlicheAusrichtungAgg",c("BWA_häufig","BWA_mittel"),"BWA",2),
  # functionScenario("BetriebswirtschaftlicheAusrichtungAgg",c("BWA_häufig","BWA_mittel","BWA_selten"),"BWA",3),
  functionScenario("Betriebsgröße","sehr_klein","Size",0.75),
  functionScenario("Betriebsgröße",c("sehr_klein","klein"),"Size",1.5),
  functionScenario("Betriebsgröße",c("sehr_klein","klein","mittel"),"Size",2.25),
  # functionScenario("Betriebsgröße",c("sehr_klein","klein","mittel","groß"),"Size",3),
  functionScenario("Wirtschaftsweise","ökologisch","Wirtschaftsweise",1), # addressing organic farms only is not so effective, because many of them already produce vegetables
  functionScenario("Wirtschaftsweise",c("ökologisch","teilweise_ökologisch"),"Wirtschaftsweise",2),
  # functionScenario("Wirtschaftsweise",c("ökologisch","teilweise_ökologisch","konventionell"),"Wirtschaftsweise",3),
  functionScenario("barrierCat","low","Barriers",1),
  functionScenario("barrierCat",c("low","medium"),"Barriers",2)
  # functionScenario("barrierCat",c("low","medium","high"),"Barriers",3)
  
)
head(dfScenario)

dfSVGScenario <- dfScenario %>%
  group_by(scenario,xvalue) %>%
  summarise(
    svgMean  = mean(svg,na.rm=T)*100,
    svgSD  = sd(svg,na.rm=T)*100
  )


dfSVGScenario <- rbind(
  dfSVGScenario,
  data.frame(scenario=dfSVGScenario$scenario,xvalue=0,vssMean =vssActual*100,svgSD=0),
  data.frame(scenario=dfSVGScenario$scenario,xvalue=3,vssMean =dfSVGfinalRel[which(dfSVGfinalRel$scenario=="Modell"&dfSVGfinalRel$Variable=="VSS"),"AnzahlAdapted"],svgSD=dfSVGfinalRel[which(dfSVGfinalRel$scenario=="Modell"&dfSVGfinalRel$Variable=="VSS"),"SD"])
)

dfSVGScenario$scenario <- factor(dfSVGScenario$scenario, levels=c("BWA","Size","Wirtschaftsweise","Barriers"))

fig2c <- ggplot(dfSVGScenario, aes(x=xvalue, y=vssMean,colour=scenario)) + 
  geom_line()+
  geom_ribbon(aes(y = vssMean, ymin = vssMean - vssSD, ymax = vssMean + vssSD, fill = scenario), alpha = .1, colour = NA) +
  geom_point(aes(shape=scenario))+
  labs(x="Priority", y = "VSS (%)")+
  theme_bw() +
  scale_colour_manual(values=c('#E69F00',"darkorchid4",'black',"darkred"),guide="none")+
  scale_fill_manual(values=c('#E69F00',"darkorchid4",'black',"darkred"),guide="none")+
  scale_shape_manual(values=c(15,16,17,18),guide="none")+
  ylim(0,60)+
  scale_x_continuous(
    breaks = 0:3, 
    labels = c("No","High","Medium","Low")) +
  theme(text=element_text(size=8))




################## diversity data
vegetablesCSA <- mean(c(60,60,40,51,55,50))
vegetablesCSASD <- sd(c(60,60,40,51,55,50))
vegetablesCurrent <- weighted.mean(c(3,25),c(0.76,0.24))

vegetablesModell <- weighted.mean(c(vegetablesCSA, vegetablesCurrent),c(dfSVGSummary$flaeche,dfDynamikVeg[which(dfDynamikVeg$Jahr==2023),"Flaeche"]))
weighted.mean(c((vegetablesCSA+vegetablesCSASD), vegetablesCurrent),c(dfSVGSummary$flaeche,dfDynamikVeg[which(dfDynamikVeg$Jahr==2023),"Flaeche"]))-vegetablesModell

##################################### save results
setwd("C:/Users/egli/sciebo - Egli, Lukas (lukas.egli@ufz.de)@gast.sciebo.de/00 Datenerhebung/02 Empirie/08 Potenzialanalyse/Ergebnisse")


write.xlsx(data.frame(table1[,c("Kategorie","Anzahl_mean","AnzahlUmstellung_mean","Flaeche_mean","FlaecheUmstellung_mean","ratioUmstellung_all","ratioUmstellungFlaeche_all","ratioUmstellungFlaecheTotal_all")]),"20250613_QuantitativeUmfrage_KurzumfragePotenziale_Table1_v1.xlsx",row.names = F)
write.xlsx(table1Reg[,c("Gebiet","Kategorie","Anzahl_mean","AnzahlUmstellung_mean","Flaeche_mean","FlaecheUmstellung_mean","ratioUmstellung_all","ratioUmstellungFlaeche_all","ratioUmstellungFlaecheTotal_all")],"20250613_QuantitativeUmfrage_KurzumfragePotenziale_TableS2_v1.xlsx",row.names = F)
names(table1Reg)


jpeg("20250611_QuantitativeUmfrage_KurzumfragePotenziale_Figure2_v1.jpeg",width = 16.9, height = 8,units = 'cm', res = 600)
ggarrange(fig2a, fig2b,fig2c, ncol = 3, nrow = 1,labels=c("a","b","c","d"),
          font.label = list(size = 8, color = "black", face = "bold", family = NULL))
dev.off()


rm(list=ls())
                                                                                                                                                                                                                                                                                                                                            