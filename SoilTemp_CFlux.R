setwd("~/Downloads/Data_Project")

#download and load packages
install.packages("neonUtilities")
install.packages("neonOS")
install.packages("terra")
install.packages("ggplot2")
install.packages("dplyr")
install.packages('BiocManager')
library(BiocManager)
BiocManager::install('rhdf5')
install.packages("data.table")

library(neonUtilities)
library(neonOS)
library(terra)
library(ggplot2)
library(dplyr)
library(data.table)

#unzip and stack eddy flux tower data
flux <- stackEddy(filepath='~/Downloads/Data_Project/NEON_eddy-flux.zip', 
                  level='dp04', var=NA, avg=NA)
names(flux)

#create objects for flux data in each site
flux_BART <- flux$BART
flux_SERC <- flux$SERC
  #SERC has a crazy outlier in late december. let's remove it
  flux_SERC <- flux_SERC %>% filter(data.fluxCo2.nsae.flux > -3000)
flux_OSBS <- flux$OSBS
flux_STEI <- flux$STEI
flux_KONZ <- flux$KONZ
flux_MLBS <- flux$MLBS
flux_TALL <- flux$TALL
flux_NOGP <- flux$NOGP
flux_STER <- flux$STER
flux_OAES <- flux$OAES
flux_JORN <- flux$JORN
flux_ONAQ <- flux$ONAQ
flux_SJER <- flux$SJER
flux_HEAL <- flux$HEAL

#create new column in each site expl siteID
IDflux_BART <- flux_BART %>% 
  mutate(siteID="BART")
IDflux_HEAL <- flux_HEAL %>% 
  mutate(siteID="HEAL")
IDflux_JORN <- flux_JORN %>% 
  mutate(siteID="JORN")
IDflux_KONZ <- flux_KONZ %>% 
  mutate(siteID="KONZ")
IDflux_MLBS <- flux_BART %>% 
  mutate(siteID="MLBS")
IDflux_NOGP <- flux_NOGP %>% 
  mutate(siteID="NOGP")
IDflux_OAES <- flux_OAES %>% 
  mutate(siteID="OAES")
IDflux_ONAQ <- flux_ONAQ %>% 
  mutate(siteID="ONAQ")
IDflux_OSBS <- flux_OSBS %>% 
  mutate(siteID="OSBS")
IDflux_SERC <- flux_SERC %>% 
  mutate(siteID="SERC")
IDflux_SJER <- flux_SJER %>% 
  mutate(siteID="SJER")
IDflux_STER <- flux_STER %>% 
  mutate(siteID="STER")
IDflux_STEI <- flux_STEI %>% 
  mutate(siteID="STEI")
IDflux_TALL <- flux_TALL %>% 
  mutate(siteID="TALL")

#stack them into one df so we can PLOT it!!!
flux_all <- bind_rows(IDflux_BART, IDflux_HEAL, IDflux_JORN, IDflux_KONZ, IDflux_MLBS, 
                      IDflux_NOGP, IDflux_OAES, IDflux_ONAQ, IDflux_OSBS, IDflux_SERC, 
                      IDflux_SJER, IDflux_STEI, IDflux_STER, IDflux_TALL)
#removing rows with QF=1
filtered_flux_all <- flux_all %>% filter(qfqm.fluxCo2.nsae.qfFinl == 0)

#plot it
flux_TimePlot <- ggplot(data = flux_all, mapping = aes(x = timeBgn, y = data.fluxCo2.nsae.flux)) +
  geom_point() +
  geom_point(colour = 'black', size = 0.5) +
  ylim(-100, 100) +
  facet_wrap(~siteID)
flux_TimePlot

#download Temp Data
SoilTempBART <- loadByProduct(dpID = "DP1.00041.001",
                           site=c("BART"),
                           startdate = "2023-01",
                           enddate = "2023-12",
                           package="basic", 
                           timeIndex="1",
                           check.size=T)
list2env(SoilTempBART, .GlobalEnv) #bring the downloaded data into the environment
SoilTempHEAL <- loadByProduct(dpID = "DP1.00041.001",
                              site=c("HEAL"),
                              startdate = "2023-01",
                              enddate = "2023-12",
                              package="basic", 
                              timeIndex="1",
                              check.size=T)
list2env(SoilTempHEAL, .GlobalEnv) #bring the downloaded data into the environment
SoilTempJORN <- loadByProduct(dpID = "DP1.00041.001",
                              site=c("JORN"),
                              startdate = "2023-01",
                              enddate = "2023-12",
                              package="basic", 
                              timeIndex="1",
                              check.size=T)
list2env(SoilTempJORN, .GlobalEnv) #bring the downloaded data into the environment
SoilTempKONZ <- loadByProduct(dpID = "DP1.00041.001",
                              site=c("KONZ"),
                              startdate = "2023-01",
                              enddate = "2023-12",
                              package="basic", 
                              timeIndex="1",
                              check.size=T)
list2env(SoilTempKONZ, .GlobalEnv) #bring the downloaded data into the environment
SoilTempMLBS <- loadByProduct(dpID = "DP1.00041.001",
                              site=c("MLBS"),
                              startdate = "2023-01",
                              enddate = "2023-12",
                              package="basic", 
                              timeIndex="1",
                              check.size=T)
list2env(SoilTempMLBS, .GlobalEnv) #bring the downloaded data into the environment
SoilTempNOGP <- loadByProduct(dpID = "DP1.00041.001",
                              site=c("NOGP"),
                              startdate = "2023-01",
                              enddate = "2023-12",
                              package="basic", 
                              timeIndex="1",
                              check.size=T)
list2env(SoilTempNOGP, .GlobalEnv) #bring the downloaded data into the environment
SoilTempOAES <- loadByProduct(dpID = "DP1.00041.001",
                              site=c("OAES"),
                              startdate = "2023-01",
                              enddate = "2023-12",
                              package="basic", 
                              timeIndex="1",
                              check.size=T)
list2env(SoilTempOAES, .GlobalEnv) #bring the downloaded data into the environment
SoilTempONAQ <- loadByProduct(dpID = "DP1.00041.001",
                              site=c("ONAQ"),
                              startdate = "2023-01",
                              enddate = "2023-12",
                              package="basic", 
                              timeIndex="1",
                              check.size=T)
list2env(SoilTempONAQ, .GlobalEnv) #bring the downloaded data into the environment
SoilTempOSBS <- loadByProduct(dpID = "DP1.00041.001",
                              site=c("OSBS"),
                              startdate = "2023-01",
                              enddate = "2023-12",
                              package="basic", 
                              timeIndex="1",
                              check.size=T)
list2env(SoilTempOSBS, .GlobalEnv) #bring the downloaded data into the environment
SoilTempSERC <- loadByProduct(dpID = "DP1.00041.001",
                              site=c("SERC"),
                              startdate = "2023-01",
                              enddate = "2023-12",
                              package="basic", 
                              timeIndex="1",
                              check.size=T)
list2env(SoilTempSERC, .GlobalEnv) #bring the downloaded data into the environment
SoilTempSJER <- loadByProduct(dpID = "DP1.00041.001",
                              site=c("SJER"),
                              startdate = "2023-01",
                              enddate = "2023-12",
                              package="basic", 
                              timeIndex="1",
                              check.size=T)
list2env(SoilTempSJER, .GlobalEnv) #bring the downloaded data into the environment
SoilTempSTEI <- loadByProduct(dpID = "DP1.00041.001",
                              site=c("STEI"),
                              startdate = "2023-01",
                              enddate = "2023-12",
                              package="basic", 
                              timeIndex="1",
                              check.size=T)
list2env(SoilTempSTEI, .GlobalEnv) #bring the downloaded data into the environment
SoilTempSTER <- loadByProduct(dpID = "DP1.00041.001",
                              site=c("STER"),
                              startdate = "2023-01",
                              enddate = "2023-12",
                              package="basic", 
                              timeIndex="1",
                              check.size=T)
list2env(SoilTempSTER, .GlobalEnv) #bring the downloaded data into the environment
SoilTempTALL <- loadByProduct(dpID = "DP1.00041.001",
                              site=c("TALL"),
                              startdate = "2023-01",
                              enddate = "2023-12",
                              package="basic", 
                              timeIndex="1",
                              check.size=T)
list2env(SoilTempTALL, .GlobalEnv) #bring the downloaded data into the environment

#extract ST_1_minute
SoilTemp_BART <- SoilTempBART$ST_1_minute
SoilTemp_HEAL <- SoilTempHEAL$ST_1_minute
SoilTemp_JORN <- SoilTempJORN$ST_1_minute
SoilTemp_KONZ <- SoilTempKONZ$ST_1_minute
SoilTemp_MLBS <- SoilTempMLBS$ST_1_minute
SoilTemp_NOGP <- SoilTempNOGP$ST_1_minute
SoilTemp_OAES <- SoilTempOAES$ST_1_minute
SoilTemp_ONAQ <- SoilTempONAQ$ST_1_minute
SoilTemp_OSBS <- SoilTempOSBS$ST_1_minute
SoilTemp_SERC <- SoilTempSERC$ST_1_minute
SoilTemp_STEI <- SoilTempSTEI$ST_1_minute
SoilTemp_STER <- SoilTempSTER$ST_1_minute
SoilTemp_TALL <- SoilTempTALL$ST_1_minute


#change names to match that of cflux
names(SoilTemp_BART)[names(SoilTemp_BART) == "startDateTime"] <- "timeBgn"
names(SoilTemp_HEAL)[names(SoilTemp_HEAL) == "startDateTime"] <- "timeBgn"
names(SoilTemp_JORN)[names(SoilTemp_JORN) == "startDateTime"] <- "timeBgn"
names(SoilTemp_KONZ)[names(SoilTemp_KONZ) == "startDateTime"] <- "timeBgn"
names(SoilTemp_MLBS)[names(SoilTemp_MLBS) == "startDateTime"] <- "timeBgn"
names(SoilTemp_NOGP)[names(SoilTemp_NOGP) == "startDateTime"] <- "timeBgn"
names(SoilTemp_OAES)[names(SoilTemp_OAES) == "startDateTime"] <- "timeBgn"
names(SoilTemp_ONAQ)[names(SoilTemp_ONAQ) == "startDateTime"] <- "timeBgn"
names(SoilTemp_OSBS)[names(SoilTemp_OSBS) == "startDateTime"] <- "timeBgn"
names(SoilTemp_SERC)[names(SoilTemp_SERC) == "startDateTime"] <- "timeBgn"
names(SoilTemp_STEI)[names(SoilTemp_STEI) == "startDateTime"] <- "timeBgn"
names(SoilTemp_STER)[names(SoilTemp_STER) == "startDateTime"] <- "timeBgn"
names(SoilTemp_TALL)[names(SoilTemp_TALL) == "startDateTime"] <- "timeBgn"
#merge based on matching time stamps
fx.tempBART <- right_join(SoilTemp_BART,flux_BART, by="timeBgn")
fx.tempHEAL <- right_join(SoilTemp_HEAL,flux_HEAL, by="timeBgn")
fx.tempJORN <- right_join(SoilTemp_JORN,flux_JORN, by="timeBgn")
fx.tempKONZ <- right_join(SoilTemp_KONZ,flux_KONZ, by="timeBgn")
fx.tempMLBS <- right_join(SoilTemp_MLBS,flux_MLBS, by="timeBgn")
fx.tempNOGP <- right_join(SoilTemp_NOGP,flux_NOGP, by="timeBgn")
fx.tempOAES <- right_join(SoilTemp_OAES,flux_OAES, by="timeBgn")
fx.tempONAQ <- right_join(SoilTemp_ONAQ,flux_ONAQ, by="timeBgn")
fx.tempOSBS <- right_join(SoilTemp_OSBS,flux_OSBS, by="timeBgn")
fx.tempSERC <- right_join(SoilTemp_SERC,flux_SERC, by="timeBgn")
fx.tempSTEI <- right_join(SoilTemp_STEI,flux_STEI, by="timeBgn")
fx.tempSTER <- right_join(SoilTemp_STER,flux_STER, by="timeBgn")
fx.tempTALL <- right_join(SoilTemp_TALL,flux_TALL, by="timeBgn")

#stack rows under same variables (column names)
fx.temp <- bind_rows(fx.tempSERC, fx.tempSTER, fx.tempSTEI, fx.tempTALL)

#select the columns we want to make things smaller
fx.temp <- fx.temp %>% 
          select(siteID, timeBgn, soilTempMean, soilTempVariance, finalQF, 
          data.fluxCo2.nsae.flux, qfqm.fluxCo2.nsae.qfFinl)

#SAVE this
write.csv(fx.temp,"~/Downloads/Data_Project/fx.tempSERC.STEI.STER.TALL.csv", row.names = FALSE)

#now bring in the different csv's to stack (this should be easier since they're smaller)
fx.tempBART.HEAL.JORN.KONZ <- read.csv("fx.tempBART.HEAL.JORN.KONZ.csv")
fx.tempMLBS.NOGP.OAES.ONAQ.OSBS <- read.csv("fx.tempMLBS.NOGP.OAES.ONAQ.OSBS.csv")
fx.tempSERC.STEI.STER.TALL <- read.csv("fx.tempSERC.STEI.STER.TALL.csv")

#stack rows like we did before
fx.temp.all <- bind_rows(fx.tempBART.HEAL.JORN.KONZ, fx.tempMLBS.NOGP.OAES.ONAQ.OSBS, 
                     fx.tempSERC.STEI.STER.TALL)


#maybe we should clean the data :3
sum(fx.temp$finalQF==1)/nrow(fx.temp) #returns 0.2320006 we should probably clean this up UGH
#removing rows with QF=1
  filtered_fx.temp <- fx.temp.all %>% 
   filter(finalQF == 0) 
  filtered_fx.temp <- filtered_fx.temp %>% 
    filter(qfqm.fluxCo2.nsae.qfFinl == 0)
  
#SAVE this
write.csv(filtered_fx.temp,"~/Downloads/Data_Project/filtered_fx.temp.csv", row.names = FALSE)


#PLot c flux against temp
fx.temp_plot <- ggplot(filtered_fx.temp, aes(x=soilTempMean, y=data.fluxCo2.nsae.flux)) + 
  geom_point() +
  ylim(-75, 75)
print(fx.temp_plot)

#PLot c flux against temp VARIANCE
fx.temp_plot <- ggplot(filtered_fx.temp, aes(x=soilTempMean, y=data.fluxCo2.nsae.flux)) + 
  geom_point() +
  ylim(-75, 75) +
  facet_wrap(~siteID)
print(fx.temp_plot) 

#violin plot for different sites
fx.site_violin <- ggplot(filtered_fx.temp, aes(data.fluxCo2.nsae.flux, factor(siteID))) +
  geom_violin() +
  labs(y= "Site", x = "Carbon Flux (petagrams of carbon per year)")
print(fx.site_violin)

fx.site_box <- ggplot(filtered_fx.temp, aes(data.fluxCo2.nsae.flux, factor(siteID))) +
  geom_boxplot() +
  labs(y= "Site", x = "Carbon Flux (petagrams of carbon per year)") +
  xlim(-75, 75)
print(fx.site_box)
#see if variance is the same across site
# Levene's test
install.packages("car")
library(car)
leveneTest(data.fluxCo2.nsae.flux ~ siteID,
           data = filtered_fx.temp)
#Levene's Test for Homogeneity of Variance (center = median)
#           Df F value    Pr(>F)    
#group      12   74040 < 2.2e-16 ***
#      3021941                      
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# ANOVA
oneway.test(data.fluxCo2.nsae.flux ~ siteID,
            data = filtered_fx.temp,
            var.equal = TRUE)


# Violin plots with continuous x:
fx.temp_violin <- ggplot(filtered_fx.temp, aes(soilTempMean, data.fluxCo2.nsae.flux)) +
  geom_violin(aes(group = cut_width(soilTempMean, 10)), scale = "width") +
  labs(y= "Carbon Flux (petagrams of carbon per year)", x = "Mean Soil Temperature (Degrees Celsius)") 
fx.temp_violin

#Bix plots with continuous x:
fx.temp_box <- ggplot(filtered_fx.temp, aes(soilTempMean, data.fluxCo2.nsae.flux)) +
  geom_boxplot(aes(group = cut_width(soilTempMean, 10)), scale = "width") +
  labs(y= "Carbon Flux (petagrams of carbon per year)", x = "Mean Soil Temperature (Degrees Celsius)") +
  ylim(-75, 75)
fx.temp_box

#linear model timeeeee
fx.temp.lm <- lm(data.fluxCo2.nsae.flux ~ soilTempMean, data = filtered_fx.temp)
summary(fx.temp.lm)

####### OLD STUFF #########


#load soil temp data for multiple sites
SoilTemp1 <- loadByProduct(dpID = "DP1.00041.001",
                          site=c("BART", "HEAL", "JORN", "KONZ", "MLBS", 
                                 "NOGP", "OAES", "ONAQ", "OSBS", "SERC",
                                 "SJER", "STEI", "STER", "TALL"),
                          startdate = "2024-01",
                          enddate = "2025-01",
                          package="basic", 
                          timeIndex="1",
                          check.size=T)
SoilTempHEAL <- loadByProduct(dpID = "DP1.00041.001",
                          site=c("HEAL"),
                          startdate = "2024-01",
                          enddate = "2025-01",
                          package="basic", 
                          timeIndex="1",
                          check.size=T)


list2env(SoilTempHEAL, .GlobalEnv) #bring the downloaded data into the environment

#now check data quality, 1 is bad, 0 is good
sum(TALLSoilTemp$finalQF==1)/nrow(TALLSoilTemp)
sum(BARTSoilTemp$finalQF==1)/nrow(BARTSoilTemp)
sum(TREESoilTemp$finalQF==1)/nrow(TREESoilTemp)

#download carbon flux data for same sites
  #defining our specs (this is how the NEON tutorial did it)
  startDate <- "2024-01"
  endDate <- "2025-01"
  site <-c("TALL","BART","TREE","BARR")
  #tell R where to download data to
  dirFile <- ("~/Downloads/Data_Project")
zipsByProduct(dpID = "DP4.00200.001", package = "basic",
              site=site,
              startdate = startDate, enddate = endDate,
              savepath = dirFile,
              check.size = TRUE)
#another site
startDate <- "2024-01"
endDate <- "2025-01"
site <-c("HEAL")
#tell R where to download data to
dirFile <- ("~/Downloads/Data_Project")
zipsByProduct(dpID = "DP4.00200.001", package = "basic",
              site=site,
              startdate = startDate, enddate = endDate,
              savepath = dirFile,
              check.size = TRUE)
#stack files and extract level 4 (flux) data
flux <- neonUtilities::stackEddy(filepath = paste0(dirFile,"/filesToStack00200"),
                                 level = "dp04")

#BARR doesn't have flux data and it's causing problems. remove it
# Remove the 'BART' element from the list
flux$BARR <- NULL

#what do we have?
head(flux)
names(flux)
flux$variables #turb is turbulent flux, stor is storage, and nsae is net surface atmospheric exchange

#can we check this data quality too?
sum(flux$finalQF==1)/nrow(flux) #YES and it returns numeric(0) meaning the data looks good!

#clean up flux data
  # Access BART, TALL, and TREE data frames
  flux_BART <- flux$BART
  flux_TALL <- flux$TALL
  flux_TREE <- flux$TREE
  flux_HEAL <- fluxHEAL$HEAL
  # Check the structure of each data frame
  str(flux_BART)
  str(flux_TALL)
  str(flux_TREE)
  # Check the column names of each data frame
  names(flux_BART)
  names(flux_TALL)
  names(flux_TREE)
  
# Extract the ST_1_minute data frame
  soilTempDataHEAL <- SoilTempHEAL$ST_1_minute
  # Subset the data for each site
  temp_BART <- subset(soilTempData, siteID == "BART")[, c("startDateTime", "siteID", "soilTempMean")]
  temp_TALL <- subset(soilTempData, siteID == "TALL")[, c("startDateTime", "siteID", "soilTempMean")]
  temp_TREE <- subset(soilTempData, siteID == "TREE")[, c("startDateTime", "siteID", "soilTempMean")]
  temp_HEAL <- subset(soilTempDataHEAL, siteID == "HEAL")[, c("startDateTime", "siteID", "soilTempMean")]
  
  # View the first few rows of each subset to ensure correctness
  head(temp_BART)
  head(temp_TALL)
  head(temp_TREE)
  
#merge SoilTemp and flux data
  #first rename column to match flux
  names(temp_BART)[names(temp_BART) == "startDateTime"] <- "timeBgn"
  names(temp_TALL)[names(temp_TALL) == "startDateTime"] <- "timeBgn"
  names(temp_TREE)[names(temp_TREE) == "startDateTime"] <- "timeBgn"
  names(temp_HEAL)[names(temp_HEAL) == "startDateTime"] <- "timeBgn"
#merge based on matching time stamps
fx.tempBART <- right_join(temp_BART,flux_BART, by="timeBgn")
fx.tempTALL <- right_join(temp_TALL,flux_TALL, by="timeBgn")
fx.tempTREE <- right_join(temp_TREE,flux_TREE, by="timeBgn")
fx.tempHEAL <- right_join(temp_HEAL,flux_HEAL, by="timeBgn")

#stack rows under same variables (column names)
fx.temp <- bind_rows(fx.tempTALL,
                        fx.tempTREE)

#rename the column oh noooooo
names(fx.temp)[names(fx.temp) == "data.fluxCo2.nsae.flux$flux$BART$data.fluxCo2.nsae.flux"] <- "heehee"

#plot carbon exchange against soil temp <3
plot(filtered_fx.temp$data.fluxCo2.nsae.flux~filtered_fx.temp$soilTempMean,
     pch=".", ylim=c(-20,20),
     xlab="Soil Temp", ylab="CO2 Flux")

ggplot(data = filtered_fx.temp, aes(soilTempMean,data.fluxCo2.nsae.flux)) +
  geom_point(color = "black") +
  labs(title = title) 

#linear model timeeeee
fx.temp.lm <- lm(data.fluxCo2.nsae.flux ~ soilTempMean, data = filtered_fx.temp)
summary(fx.temp.lm)



PRCP <- lm(y_Live_n~y_total_PRCP, SalamanderAnnualTotals)
summary(PRCP)

SNOW<- lm(y_Live_n~y_total_SNOW, SalamanderAnnualTotals)
summary(SNOW)

SNOWTEMPINTERSpecies<- lm(y_Live_n~y_total_SNOW  + y_total_SNOW*y_avg_TMAX + CommonName, SalamanderAnnualTotals)
summary(SNOWTEMPINTERSpecies)

##AIC test on previous models
AIC(SNOWTEMPINTERSpecies)
AIC(SNOW)
#best model
AIC(SNOW,SNOWTEMP, SNOWTEMPINTER, SNOWTEMPINTERSpecies)
#significant interaction between snow/prcp and salamander populations




