#read in csv with fx and temp data
filtered_fx.temp <- read.csv("filtered_fx.temp.csv")

install.packages("lubridate")
library(lubridate)

library(neonUtilities)
library(neonOS)
library(terra)
library(ggplot2)
library(dplyr)
library(data.table)
library(lubridate)

#download soil moisture
SMBART <- loadByProduct(dpID = "DP1.00094.001",
                              site=c("BART"),
                              startdate = "2023-01",
                              enddate = "2023-12",
                              package="basic", 
                              timeIndex="30",
                              check.size=F)
list2env(SMBART, .GlobalEnv) #bring the downloaded data into the environment
SMHEAL <- loadByProduct(dpID = "DP1.00094.001",
                              site=c("HEAL"),
                              startdate = "2024-01",
                              enddate = "2024-12",
                              package="basic", 
                              timeIndex="30",
                              check.size=F)
list2env(SMHEAL, .GlobalEnv) #bring the downloaded data into the environment
SMJORN <- loadByProduct(dpID = "DP1.00094.001",
                              site=c("JORN"),
                              startdate = "2023-01",
                              enddate = "2023-12",
                              package="basic", 
                              timeIndex="30",
                              check.size=F)
list2env(SMJORN, .GlobalEnv) #bring the downloaded data into the environment
SMKONZ <- loadByProduct(dpID = "DP1.00094.001",
                              site=c("KONZ"),
                              startdate = "2023-01",
                              enddate = "2023-12",
                              package="basic", 
                              timeIndex="30",
                              check.size=F)
list2env(SMKONZ, .GlobalEnv) #bring the downloaded data into the environment
SMMLBS <- loadByProduct(dpID = "DP1.00094.001",
                              site=c("MLBS"),
                              startdate = "2023-01",
                              enddate = "2023-12",
                              package="basic", 
                              timeIndex="30",
                              check.size=F)
list2env(SMMLBS, .GlobalEnv) #bring the downloaded data into the environment
SMNOGP <- loadByProduct(dpID = "DP1.00094.001",
                              site=c("NOGP"),
                              startdate = "2023-01",
                              enddate = "2023-12",
                              package="basic", 
                              timeIndex="30",
                              check.size=F)
list2env(SMNOGP, .GlobalEnv) #bring the downloaded data into the environment
SMOAES <- loadByProduct(dpID = "DP1.00094.001",
                              site=c("OAES"),
                              startdate = "2023-01",
                              enddate = "2023-12",
                              package="basic", 
                              timeIndex="30",
                              check.size=F)
list2env(SMOAES, .GlobalEnv) #bring the downloaded data into the environment
SMONAQ <- loadByProduct(dpID = "DP1.00094.001",
                              site=c("ONAQ"),
                              startdate = "2023-01",
                              enddate = "2023-12",
                              package="basic", 
                              timeIndex="30",
                              check.size=F)
list2env(SMONAQ, .GlobalEnv) #bring the downloaded data into the environment
SMOSBS <- loadByProduct(dpID = "DP1.00094.001",
                              site=c("OSBS"),
                              startdate = "2023-01",
                              enddate = "2023-12",
                              package="basic", 
                              timeIndex="30",
                              check.size=F)
list2env(SMOSBS, .GlobalEnv) #bring the downloaded data into the environment
SMSERC <- loadByProduct(dpID = "DP1.00094.001",
                              site=c("SERC"),
                              startdate = "2023-01",
                              enddate = "2023-12",
                              package="basic", 
                              timeIndex="30",
                              check.size=F)
list2env(SMSERC, .GlobalEnv) #bring the downloaded data into the environment
SMSJER <- loadByProduct(dpID = "DP1.00094.001",
                              site=c("SJER"),
                              startdate = "2023-01",
                              enddate = "2023-12",
                              package="basic", 
                              timeIndex="30",
                              check.size=F)
list2env(SMSJER, .GlobalEnv) #bring the downloaded data into the environment
SMSTEI <- loadByProduct(dpID = "DP1.00094.001",
                              site=c("STEI"),
                              startdate = "2023-01",
                              enddate = "2023-12",
                              package="basic", 
                              timeIndex="30",
                              check.size=F)
list2env(SMSTEI, .GlobalEnv) #bring the downloaded data into the environment
SMSTER <- loadByProduct(dpID = "DP1.00094.001",
                              site=c("STER"),
                              startdate = "2023-01",
                              enddate = "2023-12",
                              package="basic", 
                              timeIndex="30",
                              check.size=F)
list2env(SMSTER, .GlobalEnv) #bring the downloaded data into the environment
SMTALL <- loadByProduct(dpID = "DP1.00094.001",
                              site=c("TALL"),
                              startdate = "2023-01",
                              enddate = "2023-12",
                              package="basic", 
                              timeIndex="30",
                              check.size=F)
list2env(SMTALL, .GlobalEnv) #bring the downloaded data into the environment

#extract ST_1_minute
SMBART <- SMBART$SWS_30_minute
SMHEAL <- SMHEAL$SWS_30_minute
SMJORN <- SMJORN$SWS_30_minute
SMKONZ <- SMKONZ$SWS_30_minute
SMMLBS <- SMMLBS$SWS_30_minute
SMNOGP <- SMNOGP$SWS_30_minute
SMOAES <- SMOAES$SWS_30_minute
SMONAQ <- SMONAQ$SWS_30_minute
SMOSBS <- SMOSBS$SWS_30_minute
SMSERC <- SMSERC$SWS_30_minute
SMSJER <- SMSJER$SWS_30_minute
SMSTEI <- SMSTEI$SWS_30_minute
SMSTER <- SMSTER$SWS_30_minute
SMTALL <- SMTALL$SWS_30_minute


#create new column in each site expl siteID
SMBART <- SMBART %>% 
  mutate(siteID="BART")
SMHEAL <- SMHEAL %>% 
  mutate(siteID="HEAL")
SMJORN <- SMJORN %>% 
  mutate(siteID="JORN")
SMKONZ <- SMKONZ %>% 
  mutate(siteID="KONZ")
SMMLBS <- SMBART %>% 
  mutate(siteID="MLBS")
SMNOGP <- SMNOGP %>% 
  mutate(siteID="NOGP")
SMOAES <- SMOAES %>% 
  mutate(siteID="OAES")
SMONAQ <- SMONAQ %>% 
  mutate(siteID="ONAQ")
SMOSBS <- SMOSBS %>% 
  mutate(siteID="OSBS")
SMSERC <- SMSERC %>% 
  mutate(siteID="SERC")
SMSJER <- SMSJER %>% 
  mutate(siteID="SJER")
SMSTEI <- SMSTEI %>% 
  mutate(siteID="STEI")
SMSTER <- SMSTER %>% 
  mutate(siteID="STER")
SMTALL <- SMTALL %>% 
  mutate(siteID="TALL")

#change names to match that of cflux
names(SMBART)[names(SMBART) == "startDateTime"] <- "timeBgn"
names(SMHEAL)[names(SMHEAL) == "startDateTime"] <- "timeBgn"
names(SMJORN)[names(SMJORN) == "startDateTime"] <- "timeBgn"
names(SMKONZ)[names(SMKONZ) == "startDateTime"] <- "timeBgn"
names(SMMLBS)[names(SMMLBS) == "startDateTime"] <- "timeBgn"
names(SMNOGP)[names(SMNOGP) == "startDateTime"] <- "timeBgn"
names(SMOAES)[names(SMOAES) == "startDateTime"] <- "timeBgn"
names(SMONAQ)[names(SMONAQ) == "startDateTime"] <- "timeBgn"
names(SMOSBS)[names(SMOSBS) == "startDateTime"] <- "timeBgn"
names(SMSERC)[names(SMSERC) == "startDateTime"] <- "timeBgn"
names(SMSJER)[names(SMSJER) == "startDateTime"] <- "timeBgn"
names(SMSTEI)[names(SMSTEI) == "startDateTime"] <- "timeBgn"
names(SMSTER)[names(SMSTER) == "startDateTime"] <- "timeBgn"
names(SMTALL)[names(SMTALL) == "startDateTime"] <- "timeBgn"


#select the columns we want to make things smaller
SMBART <- SMBART %>% 
  select(siteID, timeBgn, VSWCMean, VSWCFinalQF)
SMHEAL <- SMHEAL %>% 
  select(siteID, timeBgn, VSWCMean, VSWCFinalQF)
SMJORN <- SMJORN %>% 
  select(siteID, timeBgn, VSWCMean, VSWCFinalQF)
SMMLBS <- SMMLBS %>% 
  select(siteID, timeBgn, VSWCMean, VSWCFinalQF)
SMKONZ <- SMKONZ %>% 
  select(siteID, timeBgn, VSWCMean, VSWCFinalQF)
SMNOGP <- SMNOGP %>% 
  select(siteID, timeBgn, VSWCMean, VSWCFinalQF)
SMOAES <- SMOAES %>% 
  select(siteID, timeBgn, VSWCMean, VSWCFinalQF)
SMONAQ <- SMONAQ %>% 
  select(siteID, timeBgn, VSWCMean, VSWCFinalQF)
SMOSBS <- SMOSBS %>% 
  select(siteID, timeBgn, VSWCMean, VSWCFinalQF)
SMSERC <- SMSERC %>% 
  select(siteID, timeBgn, VSWCMean, VSWCFinalQF)
SMSJER <- SMSJER %>% 
  select(siteID, timeBgn, VSWCMean, VSWCFinalQF)
SMSTEI <- SMSTEI %>% 
  select(siteID, timeBgn, VSWCMean, VSWCFinalQF)
SMSTER <- SMSTER %>% 
  select(siteID, timeBgn, VSWCMean, VSWCFinalQF)
SMTALL <- SMTALL %>% 
  select(siteID, timeBgn, VSWCMean, VSWCFinalQF)

# convert to date, easier to work with
SMBART$Date <- as.Date(SMBART$timeBgn)
SMHEAL$Date <- as.Date(SMHEAL$timeBgn)
SMJORN$Date <- as.Date(SMJORN$timeBgn)
SMKONZ$Date <- as.Date(SMKONZ$timeBgn)
SMMLBS$Date <- as.Date(SMMLBS$timeBgn)
SMNOGP$Date <- as.Date(SMNOGP$timeBgn)
SMOAES$Date <- as.Date(SMOAES$timeBgn)
SMONAQ$Date <- as.Date(SMONAQ$timeBgn)
SMOSBS$Date <- as.Date(SMOSBS$timeBgn)
SMSERC$Date <- as.Date(SMSERC$timeBgn)
SMSJER$Date <- as.Date(SMSJER$timeBgn)
SMSTEI$Date <- as.Date(SMSTEI$timeBgn)
SMSTER$Date <- as.Date(SMSTER$timeBgn)
SMTALL$Date <- as.Date(SMTALL$timeBgn)

filtered_fx.temp$Date <- as.Date(filtered_fx.temp$timeBgn)


# mean temp each day
BARTtemp_day <- SMBART %>%
  group_by(Date) %>%
  distinct(Date, .keep_all=T) %>%
  mutate(dayMean=mean(VSWCMean))
HEALtemp_day <- SMHEAL %>%
  group_by(Date) %>%
  distinct(Date, .keep_all=T) %>%
  mutate(dayMean=mean(VSWCMean))
JORNtemp_day <- SMJORN %>%
  group_by(Date) %>%
  distinct(Date, .keep_all=T) %>%
  mutate(dayMean=mean(VSWCMean))
KONZtemp_day <- SMKONZ %>%
  group_by(Date) %>%
  distinct(Date, .keep_all=T) %>%
  mutate(dayMean=mean(VSWCMean))
MLBStemp_day <- SMMLBS %>%
  group_by(Date) %>%
  distinct(Date, .keep_all=T) %>%
  mutate(dayMean=mean(VSWCMean))
NOGPtemp_day <- SMNOGP %>%
  group_by(Date) %>%
  distinct(Date, .keep_all=T) %>%
  mutate(dayMean=mean(VSWCMean))
OAEStemp_day <- SMOAES %>%
  group_by(Date) %>%
  distinct(Date, .keep_all=T) %>%
  mutate(dayMean=mean(VSWCMean))
ONAQtemp_day <- SMONAQ %>%
  group_by(Date) %>%
  distinct(Date, .keep_all=T) %>%
  mutate(dayMean=mean(VSWCMean))
OSBStemp_day <- SMOSBS %>%
  group_by(Date) %>%
  distinct(Date, .keep_all=T) %>%
  mutate(dayMean=mean(VSWCMean))
SERCtemp_day <- SMSERC %>%
  group_by(Date) %>%
  distinct(Date, .keep_all=T) %>%
  mutate(dayMean=mean(VSWCMean))
SJERtemp_day <- SMSJER %>%
  group_by(Date) %>%
  distinct(Date, .keep_all=T) %>%
  mutate(dayMean=mean(VSWCMean))
STEItemp_day <- SMSTEI %>%
  group_by(Date) %>%
  distinct(Date, .keep_all=T) %>%
  mutate(dayMean=mean(VSWCMean))
STERtemp_day <- SMSTER %>%
  group_by(Date) %>%
  distinct(Date, .keep_all=T) %>%
  mutate(dayMean=mean(VSWCMean))
TALLtemp_day <- SMTALL %>%
  group_by(Date) %>%
  distinct(Date, .keep_all=T) %>%
  mutate(dayMean=mean(VSWCMean))

#THE PROBLEM
filtered_fx.temp <- filtered_fx.temp %>%
  group_by (siteID, Date) %>%
  distinct(Date, .keep_all=T) %>%
  mutate(dayMean=mean(data.fluxCo2.nsae.flux))

#stack rows under same variables (column names)
SM_all <- bind_rows(BARTtemp_day, HEALtemp_day, JORNtemp_day, KONZtemp_day, MLBStemp_day, NOGPtemp_day,
                    OAEStemp_day, ONAQtemp_day, OSBStemp_day, SERCtemp_day, SJERtemp_day, STEItemp_day,
                    STERtemp_day, TALLtemp_day)

# Convert timeBgn in filtered_fx.temp to datetime
filtered_fx.temp$timeBgn <- ymd_hms(filtered_fx.temp$timeBgn)

filtered_fx.temp <- na.omit(filtered_fx.temp, cols = c("soilTempMean", "data.fluxCo2.nsae.flux"))

# Now perform the right join
fx.temp.moist <- right_join(filtered_fx.temp, SM_all, by = c("Date", "siteID"))

#removing rows with QF=1
filtered_fx.moist <- fx.temp.moist %>% 
  filter(VSWCFinalQF == 0) 

#SAVE this
write.csv(fx.temp.moist,"~/Downloads/Data_Project/fx.temp.moist", row.names = FALSE)

#PLot c flux against moisture
fx.moist_plot <- ggplot(filtered_fx.moist, aes(x=VSWCMean, y=data.fluxCo2.nsae.flux)) + 
  geom_point() +
  labs(y= "Carbon Flux (petagrams of carbon per year)", x = "Proportion of soil that is water")
print(fx.moist_plot)

#Box plots with continuous x:
fx.moist_box <- ggplot(fx.temp.moist, aes(VSWCMean, data.fluxCo2.nsae.flux)) +
  geom_boxplot(aes(group = cut_width(VSWCMean, 0.1))) +
  labs(y= "Carbon Flux (petagrams of carbon per year)", x = "Proportion of soil that is water")
fx.moist_box

#wait does this change temp relationship
#PLot c flux against temp
fx.temp_plot <- ggplot(filtered_fx.moist, aes(x=soilTempMean, y=data.fluxCo2.nsae.flux)) + 
  geom_point() +
  labs(y= "Carbon Flux (petagrams of carbon per year)", x = "Soil Temperature (C)") 
print(fx.temp_plot)
fx.temp_box <- ggplot(fx.temp.moist, aes(soilTempMean, data.fluxCo2.nsae.flux)) +
  geom_boxplot(aes(group = cut_width(soilTempMean, 10))) +
  labs(y= "Carbon Flux (petagrams of carbon per year)", x = "Soil temperature (C)")
fx.temp_box

#linear model timeeeee
fx.moist.lm <- lm(data.fluxCo2.nsae.flux ~ VSWCMean, data = fx.temp.moist)
summary(fx.moist.lm)

fx.temp.lm <- lm(data.fluxCo2.nsae.flux ~ soilTempMean, data = fx.temp.moist)
summary(fx.temp.lm)

