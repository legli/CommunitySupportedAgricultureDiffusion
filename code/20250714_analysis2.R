####
## written by Lukas Egli
## 15 July 2025


#### load libraries
library(xlsx)
library(ggplot2)
library(lme4)
library(dplyr)
library(tidyr)
library(ggpubr)


#### Import and prepare data
## parameters
numberCSA <- 481 # number of CSAs in Germany in 2023
populationGermany <- 83456045 # german population in 2023
vssActual <- 0.38 # current vegetable self-sufficiency
csaProportionActual <- 0.0025
basePopulationSupplied <- vssActual*populationGermany # number of people currently supplied
dfVegetableFarms <- read.xlsx("data/DataSource3_30_VegetableFarms.xlsx",sheetIndex = 1)
vegetableFarms2023 <- dfVegetableFarms[which(dfVegetableFarms$Jahr==2023),"Anzahl"] # number of vegetable producing farms in 2023
vegetableArea2023 <- dfVegetableFarms[which(dfVegetableFarms$Jahr==2023),"Flaeche"]
  

## vegetable area in CSA relative to size class
dfVegetableAreaProp <- read.xlsx("data/DataSource2_23_CSAFarmsProportionVegetables.xlsx",sheetIndex=1)


## agricultural structural survey (number and area per farm type)
dfFarmTypes <- read.xlsx("data/DataSource3_40_AgriculturalFarmsFarmType.xlsx",sheetIndex = 1)

# adapt size class categories according to the model
dfFarmTypes$BetriebsgrößeOrig <- dfFarmTypes$Betriebsgröße
dfFarmTypes$Betriebsgröße[which(dfFarmTypes$Betriebsgröße%in%c("Weniger als 5 Hektar","5 bis 10 Hektar"))] <- "sehr_klein"
dfFarmTypes$Betriebsgröße[which(dfFarmTypes$Betriebsgröße%in%c("10 bis 20 Hektar","20 bis 50 Hektar"))] <- "klein"
dfFarmTypes$Betriebsgröße[which(dfFarmTypes$Betriebsgröße%in%c("50 bis 100 Hektar"))] <- "mittel"
dfFarmTypes$Betriebsgröße[which(dfFarmTypes$Betriebsgröße%in%c("100 bis 200 Hektar","200 bis 500 Hektar","500 bis 1000 Hektar","1000 Hektar und mehr"))]<- "groß"
table(dfFarmTypes$Betriebsgröße)
head(dfFarmTypes)

# adapt economic orientation categories according to the model
dfFarmTypes$BetriebswirtschaftlicheAusrichtungAgg <- dfFarmTypes$BetriebswirtschaftlicheAusrichtung
dfFarmTypes$BetriebswirtschaftlicheAusrichtungAgg[which(dfFarmTypes$BetriebswirtschaftlicheAusrichtungAgg%in%c("Gartenbaubetrieb","Dauerkulturbetrieb"))]<- "BWA_häufig"
dfFarmTypes$BetriebswirtschaftlicheAusrichtungAgg[which(dfFarmTypes$BetriebswirtschaftlicheAusrichtungAgg%in%c("Pflanzenbauverbundbetrieb","Viehhaltungsverbundbetrieb","Ackerbaubetrieb","Pflanzenbau-Viehhaltungsverbundbetrieb"))]<- "BWA_mittel"
dfFarmTypes$BetriebswirtschaftlicheAusrichtungAgg[which(dfFarmTypes$BetriebswirtschaftlicheAusrichtungAgg%in%c("Futterbaubetrieb","Veredlungsbetrieb"))]<- "BWA_selten"
table(dfFarmTypes$BetriebswirtschaftlicheAusrichtungAgg)
sum(dfFarmTypes$Anzahl)

# add regions 
dfFarmTypes[which(dfFarmTypes$Gebiet%in%c("Nordrhein-Westfalen","Hessen","Rheinland-Pfalz","Saarland")),"Region"] <- "West"
dfFarmTypes[which(dfFarmTypes$Gebiet%in%c("Bremen","Hamburg","Schleswig-Holstein","Niedersachsen")),"Region"] <- "Nord"
dfFarmTypes[which(dfFarmTypes$Gebiet%in%c("Bayern","Baden-Württemberg")),"Region"] <- "Süd"
dfFarmTypes[which(dfFarmTypes$Gebiet%in%c("Berlin","Brandenburg","Sachsen","Thüringen","Sachsen-Anhalt","Mecklenburg-Vorpommern")),"Region"] <- "Ost"
dfFarmTypes$Region <- factor(dfFarmTypes$Region, levels = c("Nord","Ost","Süd","West"))
table(dfFarmTypes$Region )


## proportion of vegetable farms for different farm types
dfVegetableFarmsProp <- read.xlsx("data/DataSource3_50_VegetableFarmsFarmType.xlsx",sheetIndex = 1)


## area demand per state
dfAreaDemand <- read.xlsx("data/DataSource3_67_PerCapitaAreaDemand.xlsx",sheetIndex=1)
# interpolate missing values with mean values
dfAreaDemand <- rbind(dfAreaDemand,data.frame(Bundesland=c("Bremen","Berlin"),Flaechenbedarf_ha=c(mean(dfAreaDemand$Flaechenbedarf_ha),mean(dfAreaDemand$Flaechenbedarf_ha))))

## load model
modelLME <- readRDS(file = "data/modelConversion.rda")



#### Analysis 1: identify farms suitable for conversion to CSA

## model predictions
bootnum <- 100
set.seed(9999)


# baseline prediction
dfPredictionBase <- dfFarmTypes
dfPredictionBase$wahrscheinlichkeit <- predict(modelLME,dfPredictionBase,type="response")
dfPredictionBase$AnzahlUmstellung <- round(dfPredictionBase$Anzahl*dfPredictionBase$wahrscheinlichkeit,0)
dfPredictionBase$FlaecheTotal <- dfPredictionBase$Anzahl*dfPredictionBase$BetriebsgrößeDurchschnitt
dfPredictionBase$FlaecheUmstellung <- dfPredictionBase$AnzahlUmstellung*dfPredictionBase$BetriebsgrößeDurchschnitt

sum(dfPredictionBase$AnzahlUmstellung)
sum(dfPredictionBase$AnzahlUmstellung)/sum(dfPredictionBase$Anzahl)
sum(dfPredictionBase$FlaecheUmstellung)/sum(dfPredictionBase$FlaecheTotal)

# bootstrapped prediction
boot_preds <- bootMer(modelLME,
                      FUN = function(fit) {
                        predict(fit, newdata = dfFarmTypes, re.form = NULL,type="response")
                      },
                      nsim = bootnum,
                      type = "parametric",
                      use.u = TRUE,
                      seed = 123)


## extract major variables for each bootstrapping run: number and area of suitable farms
lsPrediction <-   lapply(1:bootnum,function(i){
  print(i)
  dfRun <- dfFarmTypes
  # predict model with German-wide data
  dfRun$wahrscheinlichkeit <-  boot_preds$t[i,]
  head(dfRun)

  # calculate number and area of suitable farms
  dfRun$AnzahlUmstellung <- round(dfRun$Anzahl*dfRun$wahrscheinlichkeit,0) # number of converted farms per farm type
  dfRun$FlaecheTotal <- dfRun$Anzahl*dfRun$BetriebsgrößeDurchschnitt # multiply number of farms by average farm size to calculate total area of all farms
  dfRun$FlaecheUmstellung <- dfRun$AnzahlUmstellung*dfRun$BetriebsgrößeDurchschnitt # multiply number of converted farms by average farm size to calculate total area of converted farms
  
  # select relevant variables
  dfRun <- dfRun[,c("Gebiet","BetriebswirtschaftlicheAusrichtung","BetriebswirtschaftlicheAusrichtungAgg","Betriebsgröße","BetriebsgrößeOrig","Wirtschaftsweise","Anzahl","AnzahlUmstellung","FlaecheTotal","FlaecheUmstellung")]
  dfRun$run <- i
  dfRun
  # }
})

dfPrediction <- do.call(rbind,lsPrediction) 
head(dfPrediction)
nrow(dfPrediction)


## characteristics of suitable farms with respect to economic orientation, farm size and management at national and regional level

dfEconomicOrientation <- dfPrediction %>%
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
names(dfEconomicOrientation)[1] <- "Kategorie"


dfFarmSize <- dfPrediction %>%
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
names(dfFarmSize)[1] <- "Kategorie"


dfManagement <- dfPrediction %>%
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
names(dfManagement)[1] <- "Kategorie"

# combine tables
table1 <- rbind(dfEconomicOrientation,dfFarmSize,dfManagement)
# number of farms suitable for conversion relative to all farms of each farm type
table1$ratioUmstellung <- (table1$AnzahlUmstellung_mean/table1$Anzahl_mean)*100
table1$ratioUmstellung_sd <- (table1$AnzahlUmstellung_sd/table1$Anzahl_mean)*100

# respective proportion of total area for each farm tpye
table1$ratioUmstellungFlaeche <- (table1$FlaecheUmstellung_mean/table1$Flaeche_mean)*100
table1$ratioUmstellungFlaeche_sd <- (table1$FlaecheUmstellung_sd/table1$Flaeche_mean)*100

# respective proportion of total area for each farm tpye relative to total agricultural land in Germany
table1$ratioUmstellungFlaecheTotal <- (table1$FlaecheUmstellung_mean/sum(dfManagement$Flaeche_mean))*100
sum(table1$ratioUmstellungFlaecheTotal)
table1$ratioUmstellungFlaecheTotal_sd <- (table1$FlaecheUmstellung_sd/sum(dfManagement$Flaeche_mean))*100

table1[,4:13] <- round(table1[,4:13],2)
table1$ratioUmstellung_all <- paste0(table1$ratioUmstellung," (",table1$ratioUmstellung_sd,")")
table1$ratioUmstellungFlaeche_all <- paste0(table1$ratioUmstellungFlaeche," (",table1$ratioUmstellungFlaeche_sd,")")
table1$ratioUmstellungFlaecheTotal_all <- paste0(table1$ratioUmstellungFlaecheTotal," (",table1$ratioUmstellungFlaecheTotal_sd,")")

# select relevant columns
table1Final <- data.frame(table1[,c("Kategorie","Anzahl_mean","AnzahlUmstellung_mean","Flaeche_mean","FlaecheUmstellung_mean","ratioUmstellung_all","ratioUmstellungFlaeche_all","ratioUmstellungFlaecheTotal_all")])


#### Analysis 2:  calculate vegetable self sufficiency 

## calculate vss for different bootstrapping runs
lsSupply <-   lapply(1:bootnum,function(i){
  print(i)
  dfRun <- dfPrediction[which(dfPrediction$run==i),]
  dfRun <- merge(dfRun,dfVegetableFarmsProp,all.x=T) # add proportion of farms not cultivating vegetables
  dfRun[which(is.na(dfRun$propNotVeg)),"propNotVeg"] <- 1
  
  lsSample <- lapply(1:bootnum, function(j){
    # sample proportion of farm area used for vegetable cultivation
    dfVegSample <- dfVegetableAreaProp
    dfVegSample$propVeg <- rnorm(nrow(dfVegSample),dfVegSample$propVeg,dfVegSample$propVegSD)
    dfVegSample[which(dfVegSample$propVeg<0),"propVeg"] <- 0
    dfVegSample[which(dfVegSample$propVeg>1),"propVeg"] <- 1
    dfRun <- merge(dfRun,dfVegSample[,c("BetriebsgrößeOrig","propVeg")])
    
    # consider proportion of farms not cultivating vegetables and proportion used for vegetable cultivation
    dfRun$AnzahlGemuese <- dfRun$Anzahl*(1-dfRun$propNotVeg) # existing vegetable farms
    dfRun$AnzahlGemueseNew <- dfRun$AnzahlUmstellung*dfRun$propNotVeg # new vegetable farms
    dfRun$AnzahlGemueseTot <- rowSums(dfRun[,c("AnzahlGemuese","AnzahlGemueseNew")]) # future number of vegetable producing farms
    dfRun$FlaecheUmstellungsbetriebe <- dfRun$FlaecheUmstellung*dfRun$propNotVeg*dfRun$propVeg # actual area that becomes newly available for vegetable production: area of converted farms multiplied by proportion of farms not yet producing vegetables multiplied by the proportional area used for vegetable cultivation
  
    # aggregate numbers per state
    dfRunAgg <- aggregate(cbind(AnzahlUmstellung,AnzahlGemueseTot,FlaecheUmstellungsbetriebe)~Gebiet,dfRun,sum)
    dfRunAgg <- merge(dfRunAgg,dfAreaDemand,by.x="Gebiet",by.y="Bundesland")
    dfRunAgg$AnzahlMenschen <- dfRunAgg$FlaecheUmstellungsbetriebe/dfRunAgg$Flaechenbedarf_ha # calculate number of newly supplied people

    dfFinalSample <- dfRunAgg[,c("Gebiet","AnzahlUmstellung","AnzahlGemueseTot","FlaecheUmstellungsbetriebe","AnzahlMenschen")]
    dfFinalSample$sample <- j
    dfFinalSample
  })
  dfFinal <- do.call(rbind,lsSample) 
  
  dfFinal$run <- i
  dfFinal
})
dfSupply <- do.call(rbind,lsSupply) 
head(dfSupply)

## calculate variables at national level
dfSVG <- aggregate(cbind(AnzahlUmstellung,AnzahlGemueseTot,AnzahlMenschen,AnzahlMenschen,FlaecheUmstellungsbetriebe)~run+sample,dfSupply,sum)
head(dfSVG)

dfSVGSummary <- data.frame(meanModel= c(mean(dfSVG$AnzahlGemueseTot),(mean(dfSVG$AnzahlUmstellung)+numberCSA),((mean(dfSVG$AnzahlMenschen)/populationGermany)*100),mean(dfSVG$FlaecheUmstellungsbetriebe)+vegetableArea2023,((mean(dfSVG$AnzahlMenschen+basePopulationSupplied)/populationGermany)*100)),
                           sdModel= c(sd(dfSVG$AnzahlGemueseTot),sd(dfSVG$AnzahlUmstellung),((sd(dfSVG$AnzahlMenschen)/populationGermany)*100),sd(dfSVG$FlaecheUmstellungsbetriebe),((sd(dfSVG$AnzahlMenschen+basePopulationSupplied)/populationGermany)*100)),
                           meanBaseline = c(vegetableFarms2023,numberCSA,csaProportionActual*100,vegetableArea2023,vssActual*100), 
                           sdBaseline = 0,
                           Variable = c("numberVegetableFarms","numberCSA","populationSupplied","areaVegetables","VSS")
)

dfSVGSummary$meanModelDifference <- dfSVGSummary$meanModel-dfSVGSummary$meanBaseline

dfSVGSummaryModel <- dfSVGSummary[,c("Variable","meanModelDifference","sdModel")]
names(dfSVGSummaryModel) <- c("Variable","mean","sd")
dfSVGSummaryModel$scenario <- "model"
dfSVGSummaryOriginal <- dfSVGSummary[,c("Variable","meanModel","sdModel")]
names(dfSVGSummaryOriginal) <- c("Variable","mean","sd")
dfSVGSummaryOriginal$scenario <- "modelOrig"
dfSVGSummaryBaseline <- dfSVGSummary[,c("Variable","meanBaseline","sdBaseline")]
names(dfSVGSummaryBaseline) <- c("Variable","mean","sd")
dfSVGSummaryBaseline$scenario <- "statusQuo"

dfSVGFinal <- rbind(dfSVGSummaryModel,dfSVGSummaryOriginal,dfSVGSummaryBaseline)
dfSVGFinal$Variable <- factor(dfSVGFinal$Variable,levels = c("numberVegetableFarms","numberCSA","populationSupplied","areaVegetables","VSS"))
dfSVGFinal$scenario <- factor(dfSVGFinal$scenario,levels=c("model","statusQuo","modelOrig"))


fig2a <- ggplot(dfSVGFinal[dfSVGFinal$Variable%in%c("numberVegetableFarms","numberCSA")&dfSVGFinal$scenario!="modelOrig",], aes(x=Variable, y=mean,fill=scenario)) + 
  geom_bar(stat="identity", color="black", 
           position="stack",alpha=0.5) +
  geom_errorbar(
    data = subset(dfSVGFinal[dfSVGFinal$Variable%in%c("numberVegetableFarms","numberCSA"),], scenario == "modelOrig"),
    aes(ymin = mean-sd, ymax = mean+sd),
    width = 0.2, size = 0.3
  ) +
  labs(x="", y = "Number")+
  theme_bw() +
  scale_fill_manual(values=c('#E69F00','#999999',"white"),guide="none")+
  ylim(0,25100)+
  scale_x_discrete(
    breaks = c("numberVegetableFarms","numberCSA"), 
    labels = c("Vegetable farms","CSA farms"))+
  theme(text=element_text(size=8))


fig2b <- ggplot(dfSVGFinal[dfSVGFinal$Variable%in%c("populationSupplied","VSS")&dfSVGFinal$scenario!="modelOrig",], aes(x=Variable, y=mean,fill=scenario)) + 
  geom_bar(stat="identity", color="black", 
           position="stack",alpha=0.5) +
  geom_errorbar(
    data = subset(dfSVGFinal[dfSVGFinal$Variable%in%c("populationSupplied","VSS"),], scenario == "modelOrig"),
    aes(ymin = mean-sd, ymax = mean+sd), 
    width = 0.2, size = 0.3
  )+
  labs(x="", y = "Percentage")+
  theme_bw() +
  scale_fill_manual(values=c('#E69F00','#999999',"white"),guide="none")+
  ylim(0,60)+
  scale_x_discrete(
    breaks = c("populationSupplied","VSS"), 
    labels = c("Members","VSS"))+
  theme(text=element_text(size=8))


## different target scenarios

# function to model for different target farms 
functionScenario <-   function(columnName, vecCategory, scenarioName, scenarioNumber){
  
  lsSupply <-   lapply(1:bootnum,function(i){
    print(i)
    dfRun <- dfPrediction[which(dfPrediction$run==i&dfPrediction[,columnName]%in%c(vecCategory)),] # subset target farms
    dfRun <- merge(dfRun,dfVegetableFarmsProp,all.x=T)
    dfRun[which(is.na(dfRun$propNotVeg)),"propNotVeg"] <- 1
    
    lsSample <- lapply(1:bootnum, function(j){
      # sample proportion of farm area used for vegetable cultivation
      dfVegSample <- dfVegetableAreaProp
      dfVegSample$propVeg <- rnorm(nrow(dfVegSample),dfVegSample$propVeg,dfVegSample$propVegSD)
      dfVegSample[which(dfVegSample$propVeg<0),"propVeg"] <- 0
      dfVegSample[which(dfVegSample$propVeg>1),"propVeg"] <- 1
      dfRun <- merge(dfRun,dfVegSample[,c("BetriebsgrößeOrig","propVeg")])
      
      # consider proportion of farms not cultivating vegetables and proportion used for vegetable cultivation
      dfRun$FlaecheUmstellungsbetriebe <- dfRun$FlaecheUmstellung*dfRun$propVeg*dfRun$propNotVeg # actual area that becomes newly available for vegetable production
      
      # aggregate numbers per state
      dfRunAgg <- aggregate(FlaecheUmstellungsbetriebe~Gebiet,dfRun,sum)
      dfRunAgg <- merge(dfRunAgg,dfAreaDemand,by.x="Gebiet",by.y="Bundesland")
      dfRunAgg$AnzahlMenschen <- dfRunAgg$FlaecheUmstellungsbetriebe/dfRunAgg$Flaechenbedarf_ha

      dfFinalSample <- dfRunAgg[,c("Gebiet","AnzahlMenschen")]
      data.frame(svg=(sum(dfFinalSample$AnzahlMenschen)+basePopulationSupplied)/populationGermany,sample=j)
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
  functionScenario("Betriebsgröße","sehr_klein","Size",0.75),
  functionScenario("Betriebsgröße",c("sehr_klein","klein"),"Size",1.5),
  functionScenario("Betriebsgröße",c("sehr_klein","klein","mittel"),"Size",2.25),
  functionScenario("Wirtschaftsweise","ökologisch","Wirtschaftsweise",1), # addressing organic farms only is not so effective, because many of them already produce vegetables
  functionScenario("Wirtschaftsweise",c("ökologisch","teilweise_ökologisch"),"Wirtschaftsweise",2))
head(dfScenario)

## aggreagte mean and sd
dfSVGScenario <- dfScenario %>%
  group_by(scenario,xvalue) %>%
  summarise(
    vssMean  = mean(svg,na.rm=T)*100,
    vssSD  = sd(svg,na.rm=T)*100
  )

## add status quo and maximum VSS
dfSVGScenario <- rbind(
  dfSVGScenario,
  data.frame(scenario=dfSVGScenario$scenario,xvalue=0,vssMean =vssActual*100,vssSD=0),
  data.frame(scenario=dfSVGScenario$scenario,xvalue=3,vssMean =dfSVGSummary[which(dfSVGSummary$Variable=="VSS"),"meanModel"],vssSD=dfSVGSummary[which(dfSVGSummary$Variable=="VSS"),"sdModel"])
)

dfSVGScenario$scenario <- factor(dfSVGScenario$scenario, levels=c("BWA","Size","Wirtschaftsweise"))

fig2c <- ggplot(dfSVGScenario, aes(x=xvalue, y=vssMean,colour=scenario)) + 
  geom_line()+
  geom_ribbon(aes(y = vssMean, ymin = vssMean - vssSD, ymax = vssMean + vssSD, fill = scenario), alpha = .1, colour = NA) +
  geom_point(aes(shape=scenario))+
  labs(x="Priority", y = "VSS (%)")+
  theme_bw() +
  scale_colour_manual(values=c('#E69F00',"darkorchid4","darkred"),guide="none")+
  scale_fill_manual(values=c('#E69F00',"darkorchid4","darkred"),guide="none")+
  scale_shape_manual(values=c(15,16,17,18),guide="none")+
  ylim(0,60)+
  scale_x_continuous(
    breaks = 0:3, 
    labels = c("No","High","Medium","Low")) +
  theme(text=element_text(size=8))



#### Analysis 3:  vegetable crop diversity

# approximation of overall vegetable crop diversity
vegetableDiversityCurrent <- weighted.mean(c(3,25),c(0.76,0.24)) # large farms covering 76% of area cultivate 3 crops, other farms 25 

# vegetable crop diversity  for CSAs (based on expert survey)
vegetableDiversityCSA <- mean(c(60,60,40,51,55,50))
vegetableDiversityCSASD <- sd(c(60,60,40,51,55,50))

# weighted mean of non-CSA and CSA diversity 
vegetableDiversityModel <- weighted.mean(c(vegetableDiversityCSA, vegetableDiversityCurrent),c(dfSVGSummary[which(dfSVGSummary$Variable=="areaVegetables"),"meanModelDifference"],vegetableArea2023))
weighted.mean(c((vegetableDiversityCSA+vegetableDiversityCSASD), vegetableDiversityCurrent),c(dfSVGSummary[which(dfSVGSummary$Variable=="areaVegetables"),"meanModelDifference"],vegetableArea2023))-vegetableDiversityModel

##################################### save results
write.xlsx(table1Final,"results/Table1.xlsx",row.names = F)

jpeg("results/Figure2.jpeg",width = 16.9, height = 8,units = 'cm', res = 600)
  ggarrange(fig2a, fig2b,fig2c, ncol = 3, nrow = 1,labels=c("a","b","c","d"),
            font.label = list(size = 8, color = "black", face = "bold", family = NULL))
dev.off()


rm(list=ls())
