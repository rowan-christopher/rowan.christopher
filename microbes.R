library(neonUtilities)
library(dplyr)
library(readr)
library(tidyverse)
library(vegan)
library(stringr)
library(ggplot2) 
library(data.table)

# load in microbe data from my sites
microbes <- loadByProduct(dpID = "DP1.10081.001", 
                          site = c("HEAL", "JORN", "KONZ", "MLBS", "NOGP", "OAES", "ONAQ", "OSBS", 
                                   "SERC", "SJER", "STEI", "STER", "TALL"),
                          release = "RELEASE-2024", 
                          check.size = F)

# accesses the 16S (bacteria and archaea) data from microbe NEON dataset and 
##loads it as a data table
table1 <- as.data.frame(microbes$mcc_soilSeqVariantMetadata_16S)

# accesses ITS (fungi) data from microbe NEON dataset as a data table
table2 <- as.data.frame(microbes$mcc_soilSeqVariantMetadata_ITS)

# excludes rows containing "below threshold" from 16S data table
filtered_table1 <- table1[table1$sequenceCountQF == "OK", ]

# merging data files keeping plotIDs and siteIDs
microbe_links <- filtered_table1$downloadFileUrl # creates object with all links to CSV files

read_csv_from_url <- function(url, plotID, siteID) {
  temp_file <- tempfile()  # create a temporary file
  download.file(url, temp_file, quiet = TRUE)  # downloads the file
  data <- read.csv(temp_file)  # reads the CSV
  data$plotID <- plotID  # adds plotID column to the dataset
  data$siteID <- siteID # adds siteID column to dataset
  return(data)  # returns the modified dataset
}

# processes each link, downloads the contents of CSV file, and merges all files 
# into one data table
merged_data <- bind_rows(mapply(read_csv_from_url, 
                                microbe_links, 
                                filtered_table1$plotID, 
                                filtered_table1$siteID, 
                                SIMPLIFY = FALSE))

head(merged_data)

#excludes rows containing "below threshold" from ITS data table
filtered_table2 <- table2[table1$sequenceCountQF == "OK", ]

#merge data files keeping plotIDs and siteIDs
microbe_links2 <- filtered_table2$downloadFileUrl # creates object with all links to CSV files

read_csv_from_url2 <- function(url, plotID, siteID) {
  temp_file <- tempfile()  # create a temporary file
  download.file(url, temp_file, quiet = TRUE)  # downloads the file
  data <- read.csv(temp_file)  # reads the CSV
  data$plotID <- plotID  # adds plotID column to the dataset
  data$siteID <- siteID # adds siteID column to dataset
  return(data)  # returns the modified dataset
}

# processes each link, downloads the contents of CSV file, and merges all files 
# into one data table
merged_data2 <- bind_rows(mapply(read_csv_from_url2, 
                                 microbe_links2, 
                                 filtered_table2$plotID, 
                                 filtered_table2$siteID, 
                                 SIMPLIFY = FALSE))

head(merged_data2)

# combines 16S data table and ITS data table
microbes_total <- bind_rows(merged_data,merged_data2)

# make separate data tables for each site
microbe_TALL <- filter(merged_data, siteID=="TALL")
microbe_STER <- filter(merged_data, siteID=="STER")
microbe_STEI <- filter(merged_data, siteID=="STEI")
microbe_SERC <- filter(merged_data, siteID=="SERC")
microbe_OSBS <- filter(merged_data, siteID=="OSBS")
microbe_ONAQ <- filter(merged_data, siteID=="ONAQ")
microbe_OAES <- filter(merged_data, siteID=="OAES")
microbe_NOGP <- filter(merged_data, siteID=="NOGP")
microbe_MLBS <- filter(merged_data, siteID=="MLBS")
microbe_KONZ <- filter(merged_data, siteID=="KONZ")
microbe_JORN <- filter(merged_data, siteID=="JORN")
microbe_HEAL <- filter(merged_data, siteID=="HEAL")
microbe_BART <- filter(merged_data, siteID=="BART")


## make data tables of the number of each species per plot
microbe_matrix_TALL <- microbe_TALL %>% group_by(plotID, scientificName) %>% 
  summarise(count=n()) %>%
  spread(scientificName,count)
microbe_matrix_TALL[is.na(microbe_matrix_TALL)] <- 0
microbe_matrix_TALL <- microbe_matrix_TALL[,-1]

microbe_matrix_STER <- microbe_STER %>% group_by(plotID, scientificName) %>% 
  summarise(count=n()) %>%
  spread(scientificName,count)
microbe_matrix_STER[is.na(microbe_matrix_STER)] <- 0
microbe_matrix_STER <- microbe_matrix_STER[,-1]

microbe_matrix_STEI <- microbe_STEI %>% group_by(plotID, scientificName) %>% 
  summarise(count=n()) %>%
  spread(scientificName,count)
microbe_matrix_STEI[is.na(microbe_matrix_STEI)] <- 0
microbe_matrix_STEI <- microbe_matrix_STEI[,-1]

microbe_matrix_SERC <- microbe_SERC %>% group_by(plotID, scientificName) %>% 
  summarise(count=n()) %>%
  spread(scientificName,count)
microbe_matrix_SERC[is.na(microbe_matrix_SERC)] <- 0
microbe_matrix_SERC <- microbe_matrix_SERC[,-1]

microbe_matrix_OSBS <- microbe_OSBS %>% group_by(plotID, scientificName) %>% 
  summarise(count=n()) %>%
  spread(scientificName,count)
microbe_matrix_OSBS[is.na(microbe_matrix_OSBS)] <- 0
microbe_matrix_OSBS <- microbe_matrix_OSBS[,-1]

microbe_matrix_ONAQ <- microbe_ONAQ %>% group_by(plotID, scientificName) %>% 
  summarise(count=n()) %>%
  spread(scientificName,count)
microbe_matrix_ONAQ[is.na(microbe_matrix_ONAQ)] <- 0
microbe_matrix_ONAQ <- microbe_matrix_ONAQ[,-1]

microbe_matrix_OAES <- microbe_OAES %>% group_by(plotID, scientificName) %>% 
  summarise(count=n()) %>%
  spread(scientificName,count)
microbe_matrix_OAES[is.na(microbe_matrix_OAES)] <- 0
microbe_matrix_OAES <- microbe_matrix_OAES[,-1]

microbe_matrix_NOGP <- microbe_NOGP %>% group_by(plotID, scientificName) %>% 
  summarise(count=n()) %>%
  spread(scientificName,count)
microbe_matrix_NOGP[is.na(microbe_matrix_NOGP)] <- 0
microbe_matrix_NOGP <- microbe_matrix_NOGP[,-1]

microbe_matrix_MLBS <- microbe_MLBS %>% group_by(plotID, scientificName) %>% 
  summarise(count=n()) %>%
  spread(scientificName,count)
microbe_matrix_MLBS[is.na(microbe_matrix_MLBS)] <- 0
microbe_matrix_MLBS <- microbe_matrix_MLBS[,-1]

microbe_matrix_KONZ <- microbe_KONZ %>% group_by(plotID, scientificName) %>% 
  summarise(count=n()) %>%
  spread(scientificName,count)
microbe_matrix_KONZ[is.na(microbe_matrix_KONZ)] <- 0
microbe_matrix_KONZ <- microbe_matrix_KONZ[,-1]

microbe_matrix_JORN <- microbe_JORN %>% group_by(plotID, scientificName) %>% 
  summarise(count=n()) %>%
  spread(scientificName,count)
microbe_matrix_JORN[is.na(microbe_matrix_JORN)] <- 0
microbe_matrix_JORN <- microbe_matrix_JORN[,-1]

microbe_matrix_HEAL <- microbe_HEAL %>% group_by(plotID, scientificName) %>% 
  summarise(count=n()) %>%
  spread(scientificName,count)
microbe_matrix_HEAL[is.na(microbe_matrix_HEAL)] <- 0
microbe_matrix_HEAL <- microbe_matrix_HEAL[,-1]

microbe_matrix_BART <- microbe_BART %>% group_by(plotID, scientificName) %>% 
  summarise(count=n()) %>%
  spread(scientificName,count)
microbe_matrix_BART[is.na(microbe_matrix_BART)] <- 0
microbe_matrix_BART <- microbe_matrix_BART[,-1]

##calculating shannon diversity and richness for each site
shd_TALLmicrobes <- diversity(microbe_matrix_TALL, index = "shannon", MARGIN = 1, base = exp(1))
shd_STERmicrobes <- diversity(microbe_matrix_STER, index = "shannon", MARGIN = 1, base = exp(1))
shd_STEImicrobes <- diversity(microbe_matrix_STEI, index = "shannon", MARGIN = 1, base = exp(1))
shd_SERCmicrobes <- diversity(microbe_matrix_SERC, index = "shannon", MARGIN = 1, base = exp(1))
shd_OSBSmicrobes <- diversity(microbe_matrix_OSBS, index = "shannon", MARGIN = 1, base = exp(1))
shd_ONAQmicrobes <- diversity(microbe_matrix_ONAQ, index = "shannon", MARGIN = 1, base = exp(1))
shd_OAESmicrobes <- diversity(microbe_matrix_OAES, index = "shannon", MARGIN = 1, base = exp(1))
shd_NOGPmicrobes <- diversity(microbe_matrix_NOGP, index = "shannon", MARGIN = 1, base = exp(1))
shd_MLBSmicrobes <- diversity(microbe_matrix_MLBS, index = "shannon", MARGIN = 1, base = exp(1))
shd_KONZmicrobes <- diversity(microbe_matrix_KONZ, index = "shannon", MARGIN = 1, base = exp(1))
shd_JORNmicrobes <- diversity(microbe_matrix_JORN, index = "shannon", MARGIN = 1, base = exp(1))
shd_HEALmicrobes <- diversity(microbe_matrix_HEAL, index = "shannon", MARGIN = 1, base = exp(1))
# shd_BARTmicrobes <- diversity(microbe_matrix_BART, index = "shannon", MARGIN = 1, base = exp(1)) #NO DATA FOR BART

rich_TALLmicrobes <- specnumber(microbe_matrix_TALL, MARGIN = 1)
rich_HEALmicrobes <- specnumber(microbe_matrix_HEAL, MARGIN = 1)
rich_STERmicrobes <- specnumber(microbe_matrix_STER, MARGIN = 1)
rich_STEImicrobes <- specnumber(microbe_matrix_STEI, MARGIN = 1)
rich_SERCmicrobes <- specnumber(microbe_matrix_SERC, MARGIN = 1)
rich_OSBSmicrobes <- specnumber(microbe_matrix_OSBS, MARGIN = 1)
rich_ONAQmicrobes <- specnumber(microbe_matrix_ONAQ, MARGIN = 1)
rich_OAESmicrobes <- specnumber(microbe_matrix_OAES, MARGIN = 1)
rich_NOGPmicrobes <- specnumber(microbe_matrix_NOGP, MARGIN = 1)
rich_MLBSmicrobes <- specnumber(microbe_matrix_MLBS, MARGIN = 1)
rich_KONZmicrobes <- specnumber(microbe_matrix_KONZ, MARGIN = 1)
rich_JORNmicrobes <- specnumber(microbe_matrix_JORN, MARGIN = 1)
rich_HEALmicrobes <- specnumber(microbe_matrix_HEAL, MARGIN = 1)
rich_BARTmicrobes <- specnumber(microbe_matrix_BART, MARGIN = 1)


### TALL data table with shannon and diversity
plotID <- c("TALL_001","TALL_002","TALL_003","TALL_004","TALL_006","TALL_007",
            "TALL_042","TALL_043","TALL_048","TALL_049")
siteID <- rep("TALL", length(plotID))
species <- rep("microbe", length(plotID))
x_TALL_microbes <- data.frame(plotID, siteID, shd_TALLmicrobes, rich_TALLmicrobes,
                              species)
colnames(x_TALL_microbes)<- c("plotID","siteID", "shd", "rich","species")

### HEAL data table with shannon and diversity
plotID <- c("HEAL_001","HEAL_004","HEAL_005","HEAL_010","HEAL_011","HEAL_025",
            "HEAL_045","HEAL_046","HEAL_047","HEAL_048")
siteID <- rep("HEAL", length(plotID))
species <- rep("microbe", length(plotID))
x_HEAL_microbes <- data.frame(plotID, siteID, shd_HEALmicrobes, rich_HEALmicrobes,
                              species)
colnames(x_HEAL_microbes)<- c("plotID","siteID", "shd", "rich","species")

### STER data table with shannon and diversity
plotID <- c("STER_026", "STER_018", "STER_006", "STER_031", "STER_035", "STER_029", 
            "STER_028", "STER_032", "STER_005", "STER_034", "STER_027", "STER_033", 
            "STER_011", "STER_012", "STER_010", "STER_016")
siteID <- rep("STER", length(plotID))
species <- rep("microbe", length(plotID))
x_STER_microbes <- data.frame(plotID, siteID, shd_STERmicrobes, rich_STERmicrobes,
                              species)
colnames(x_STER_microbes)<- c("plotID","siteID", "shd", "rich","species")

### STEI data table with shannon and diversity
plotID <- c("STEI_047", "STEI_053", "STEI_003", "STEI_005", "STEI_001", "STEI_002", 
            "STEI_004", "STEI_008", "STEI_059", "STEI_060", "STEI_021")
siteID <- rep("STEI", length(plotID))
species <- rep("microbe", length(plotID))
x_STEI_microbes <- data.frame(plotID, siteID, shd_STEImicrobes, rich_STEImicrobes,
                              species)
colnames(x_STEI_microbes)<- c("plotID","siteID", "shd", "rich","species")

### SERC data table with shannon and diversity
plotID <- c("SERC_003", "SERC_045", "SERC_002", "SERC_046", "SERC_044", "SERC_047", 
            "SERC_001", "SERC_007", "SERC_049", "SERC_004", "SERC_005")
siteID <- rep("SERC", length(plotID))
species <- rep("microbe", length(plotID))
x_SERC_microbes <- data.frame(plotID, siteID, shd_SERCmicrobes, rich_SERCmicrobes,
                              species)
colnames(x_SERC_microbes)<- c("plotID","siteID", "shd", "rich","species")

### OSBS data table with shannon and diversity
plotID <- c("OSBS_002", "OSBS_003", "OSBS_004", "OSBS_022", "OSBS_001", "OSBS_031", 
            "OSBS_026", "OSBS_027", "OSBS_023", "OSBS_029", "OSBS_005")
siteID <- rep("OSBS", length(plotID))
species <- rep("microbe", length(plotID))
x_OSBS_microbes <- data.frame(plotID, siteID, shd_OSBSmicrobes, rich_OSBSmicrobes,
                              species)
colnames(x_OSBS_microbes)<- c("plotID","siteID", "shd", "rich","species")

### OAES data table with shannon and diversity
plotID <- c("OAES_001","OAES_002","OAES_003","OAES_004","OAES_007","OAES_009",
            "OAES_042","OAES_043","OAES_044","OAES_045")
siteID <- rep("OAES", length(plotID))
species <- rep("microbe", length(plotID))
x_OAES_microbes <- data.frame(plotID, siteID, shd_OAESmicrobes, rich_OAESmicrobes,
                              species)
colnames(x_OAES_microbes)<- c("plotID","siteID", "shd", "rich","species")


### NOGP data table with shannon and diversity
plotID <- c("NOGP_064", "NOGP_061", "NOGP_006", "NOGP_065", "NOGP_062", "NOGP_041", 
            "NOGP_005", "NOGP_002", "NOGP_001", "NOGP_003", "NOGP_012")
siteID <- rep("NOGP", length(plotID))
species <- rep("microbe", length(plotID))
x_NOGP_microbes <- data.frame(plotID, siteID, shd_NOGPmicrobes, rich_NOGPmicrobes,
                              species)
colnames(x_NOGP_microbes)<- c("plotID","siteID", "shd", "rich","species")

### MLBS data table with shannon and diversity
plotID <- c("MLBS_002", "MLBS_003", "MLBS_067", "MLBS_005", "MLBS_001", "MLBS_006", "MLBS_007")
siteID <- rep("MLBS", length(plotID))
species <- rep("microbe", length(plotID))
x_MLBS_microbes <- data.frame(plotID, siteID, shd_MLBSmicrobes, rich_MLBSmicrobes,
                              species)
colnames(x_MLBS_microbes)<- c("plotID","siteID", "shd", "rich","species")

### KONZ data table with shannon and diversity
plotID <- c("KONZ_002", "KONZ_004", "KONZ_024", "KONZ_046", "KONZ_042", "KONZ_001", 
            "KONZ_045", "KONZ_005", "KONZ_003", "KONZ_043")
siteID <- rep("KONZ", length(plotID))
species <- rep("microbe", length(plotID))
x_KONZ_microbes <- data.frame(plotID, siteID, shd_KONZmicrobes, rich_KONZmicrobes,
                              species)
colnames(x_KONZ_microbes)<- c("plotID","siteID", "shd", "rich","species")

### JORN data table with shannon and diversity
plotID <- c("JORN_004", "JORN_006", "JORN_002", "JORN_047", "JORN_045", "JORN_001", 
            "JORN_042", "JORN_044", "JORN_003", "JORN_005")
siteID <- rep("JORN", length(plotID))
species <- rep("microbe", length(plotID))
x_JORN_microbes <- data.frame(plotID, siteID, shd_JORNmicrobes, rich_JORNmicrobes,
                              species)
colnames(x_JORN_microbes)<- c("plotID","siteID", "shd", "rich","species")

### ONAQ data table with shannon and diversity
plotID <- c("ONAQ_006", "ONAQ_008", "ONAQ_002", "ONAQ_003", "ONAQ_004", "ONAQ_010", 
            "ONAQ_007", "ONAQ_009", "ONAQ_005", "ONAQ_044", "ONAQ_041", "ONAQ_043", 
            "ONAQ_017", "ONAQ_012", "ONAQ_042")
siteID <- rep("ONAQ", length(plotID))
species <- rep("microbe", length(plotID))
x_ONAQ_microbes <- data.frame(plotID, siteID, shd_ONAQmicrobes, rich_ONAQmicrobes,
                              species)
colnames(x_ONAQ_microbes)<- c("plotID","siteID", "shd", "rich","species")


# combine data tables from each site
x_microbes <- bind_rows(x_HEAL_microbes,x_JORN_microbes,x_KONZ_microbes,
                        x_MLBS_microbes,x_NOGP_microbes,x_OAES_microbes,x_ONAQ_microbes,
                        x_OSBS_microbes,x_SERC_microbes,x_STEI_microbes,x_STER_microbes,
                        x_TALL_microbes)

#SAVE this
write.csv(x_microbes,"~/Downloads/Data_Project/microbe_diversity", row.names = FALSE)
