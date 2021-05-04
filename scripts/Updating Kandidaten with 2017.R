

setwd("/Users/cornelius/erststimme2017.de/R")

library(readstata13)
library(readxl)
library(plyr)
library(stringr)

kandidatinnen_90_13 <- read.dta13("Wahlkreispanel_ab1990.dta")


kandidatinnen_17 <- read_excel("Kandidatinnen 2017_v4.xlsx")


# Dropping source columns
kandidatinnen_17 <- kandidatinnen_17[, c(1:6, 8:10, 12:14, 16:18, 20:22, 24:26)]

# Making ids for matching
kandidatinnen_17$cdu_k_idname <- paste0(tolower(kandidatinnen_17$cdu_k_nname), "_", tolower(kandidatinnen_17$cdu_k_vname))
kandidatinnen_17$cdu_k_idname <- gsub("ü", "ue", kandidatinnen_17$cdu_k_idname)
kandidatinnen_17$cdu_k_idname <- gsub("ä", "ae", kandidatinnen_17$cdu_k_idname)
kandidatinnen_17$cdu_k_idname <- gsub("ö", "oe", kandidatinnen_17$cdu_k_idname)
kandidatinnen_17$cdu_k_idname <- gsub("ß", "ss", kandidatinnen_17$cdu_k_idname)
kandidatinnen_17$cdu_k_idname <- gsub(" ", "", kandidatinnen_17$cdu_k_idname)
for (i in 1:299) {
  if(kandidatinnen_17$cdu_k_idname[i] == "NA_NA") kandidatinnen_17$cdu_k_idname[i] <- NA
}

kandidatinnen_17$csu_k_idname <- paste0(tolower(kandidatinnen_17$csu_k_nname), "_", tolower(kandidatinnen_17$csu_k_vname))
kandidatinnen_17$csu_k_idname <- gsub("ü", "ue", kandidatinnen_17$csu_k_idname)
kandidatinnen_17$csu_k_idname <- gsub("ä", "ae", kandidatinnen_17$csu_k_idname)
kandidatinnen_17$csu_k_idname <- gsub("ö", "oe", kandidatinnen_17$csu_k_idname)
kandidatinnen_17$csu_k_idname <- gsub("ß", "ss", kandidatinnen_17$csu_k_idname)
kandidatinnen_17$csu_k_idname <- gsub(" ", "", kandidatinnen_17$csu_k_idname)
for (i in 1:299) {
  if(kandidatinnen_17$csu_k_idname[i] == "NA_NA") kandidatinnen_17$csu_k_idname[i] <- NA
}

kandidatinnen_17$spd_k_idname <- paste0(tolower(kandidatinnen_17$spd_k_nname), "_", tolower(kandidatinnen_17$spd_k_vname))
kandidatinnen_17$spd_k_idname <- gsub("ü", "ue", kandidatinnen_17$spd_k_idname)
kandidatinnen_17$spd_k_idname <- gsub("ä", "ae", kandidatinnen_17$spd_k_idname)
kandidatinnen_17$spd_k_idname <- gsub("ö", "oe", kandidatinnen_17$spd_k_idname)
kandidatinnen_17$spd_k_idname <- gsub("ß", "ss", kandidatinnen_17$spd_k_idname)
kandidatinnen_17$spd_k_idname <- gsub(" ", "", kandidatinnen_17$spd_k_idname)
for (i in 1:299) {
  if(kandidatinnen_17$spd_k_idname[i] == "NA_NA") kandidatinnen_17$spd_k_idname[i] <- NA
}

kandidatinnen_17$gru_k_idname <- paste0(tolower(kandidatinnen_17$gru_k_nname), "_", tolower(kandidatinnen_17$gru_k_vname))
kandidatinnen_17$gru_k_idname <- gsub("ü", "ue", kandidatinnen_17$gru_k_idname)
kandidatinnen_17$gru_k_idname <- gsub("ä", "ae", kandidatinnen_17$gru_k_idname)
kandidatinnen_17$gru_k_idname <- gsub("ö", "oe", kandidatinnen_17$gru_k_idname)
kandidatinnen_17$gru_k_idname <- gsub("ß", "ss", kandidatinnen_17$gru_k_idname)
kandidatinnen_17$gru_k_idname <- gsub(" ", "", kandidatinnen_17$gru_k_idname)
for (i in 1:299) {
  if(kandidatinnen_17$gru_k_idname[i] == "NA_NA") kandidatinnen_17$gru_k_idname[i] <- NA
}

kandidatinnen_17$pds_k_idname <- paste0(tolower(kandidatinnen_17$pds_k_nname), "_", tolower(kandidatinnen_17$pds_k_vname))
kandidatinnen_17$pds_k_idname <- gsub("ü", "ue", kandidatinnen_17$pds_k_idname)
kandidatinnen_17$pds_k_idname <- gsub("ä", "ae", kandidatinnen_17$pds_k_idname)
kandidatinnen_17$pds_k_idname <- gsub("ö", "oe", kandidatinnen_17$pds_k_idname)
kandidatinnen_17$pds_k_idname <- gsub("ß", "ss", kandidatinnen_17$pds_k_idname)
kandidatinnen_17$pds_k_idname <- gsub(" ", "", kandidatinnen_17$pds_k_idname)
for (i in 1:299) {
  if(kandidatinnen_17$pds_k_idname[i] == "NA_NA") kandidatinnen_17$pds_k_idname[i] <- NA
}

kandidatinnen_17$fdp_k_idname <- paste0(tolower(kandidatinnen_17$fdp_k_nname), "_", tolower(kandidatinnen_17$fdp_k_vname))
kandidatinnen_17$fdp_k_idname <- gsub("ü", "ue", kandidatinnen_17$fdp_k_idname)
kandidatinnen_17$fdp_k_idname <- gsub("ä", "ae", kandidatinnen_17$fdp_k_idname)
kandidatinnen_17$fdp_k_idname <- gsub("ö", "oe", kandidatinnen_17$fdp_k_idname)
kandidatinnen_17$fdp_k_idname <- gsub("ß", "ss", kandidatinnen_17$fdp_k_idname)
kandidatinnen_17$fdp_k_idname <- gsub(" ", "", kandidatinnen_17$fdp_k_idname)
for (i in 1:299) {
  if(kandidatinnen_17$fdp_k_idname[i] == "NA_NA") kandidatinnen_17$fdp_k_idname[i] <- NA
}

# Adding 2013 wkr_nummer_13 to data
wkr_nummer_13 <- read.csv2("districts02_17.csv", sep = ";", stringsAsFactors = FALSE, encoding="utf-8")
wkr_nummer_13 <- wkr_nummer_13[, 2:3]
colnames(wkr_nummer_13) <- c("wkr_nummer_13", "wkr_nummer")
kandidatinnen_17 <- merge(kandidatinnen_17, wkr_nummer_13, by="wkr_nummer")

# Merging old and new candidate names
kandidatinnen_90_17 <- data.frame(rbind.fill(kandidatinnen_17, kandidatinnen_90_13))

# Setting all "NA" strings to NA value
make.true.NA <- function(x) if(is.character(x)||is.factor(x)){
  is.na(x) <- x=="NA"; x} else {
    x}
kandidatinnen_90_17 <- data.frame(lapply(kandidatinnen_90_17, make.true.NA))

# Comparing the first part of candidate IDs from 2013 and 2017
# If distance < 3.1 the 2017 ID is replaced by 2013 ID for each Wahlkreis
# e.g 2013 ID "leyen_ursula" and new 2017 ID "vonderleyen_ursula" -> this will be replaced
for (i in 22:27) {
  for (k in 1:299) {
    wkr_13 <- kandidatinnen_90_17$wkr_nummer_13[k]
    id_13 <- as.character(kandidatinnen_90_17[kandidatinnen_90_17$year==2013 & kandidatinnen_90_17$wkr_nummer==wkr_13, i])
    id_17 <- as.character(kandidatinnen_90_17[k, i])
    if(agrepl(str_extract(id_13, ".*(?<=_)"), str_extract(id_17, ".*(?<=_)"), max.distance = 3.1) & !is.na(id_13) & !is.na(id_17)) kandidatinnen_90_17[k, i] <- id_13
      }
}

# Correcting some false cell information
kandidatinnen_90_17$cdu_k_vname[kandidatinnen_90_17$cdu_k_nname=="leyen"] <- "ursula"
kandidatinnen_90_17$cdu_k_idname[kandidatinnen_90_17$cdu_k_nname=="leyen"] <- "leyen_ursula"

# Updating the 2017 incumbency variables
# Determining 2013 Wahlkreis winners
kandidatinnen_90_17$winner_13[kandidatinnen_90_17$cdu_erst > kandidatinnen_90_17$spd_erst &
                                kandidatinnen_90_17$cdu_erst > kandidatinnen_90_17$gru_erst &
                                kandidatinnen_90_17$cdu_erst > kandidatinnen_90_17$pds_erst &
                                kandidatinnen_90_17$cdu_erst > kandidatinnen_90_17$fdp_erst &
                                kandidatinnen_90_17$year == 2013 &
                                kandidatinnen_90_17$bula != "Bayern"] <- "cdu"

kandidatinnen_90_17$winner_13[kandidatinnen_90_17$csu_erst > kandidatinnen_90_17$spd_erst &
                                kandidatinnen_90_17$csu_erst > kandidatinnen_90_17$gru_erst &
                                kandidatinnen_90_17$csu_erst > kandidatinnen_90_17$pds_erst &
                                kandidatinnen_90_17$csu_erst > kandidatinnen_90_17$fdp_erst &
                                kandidatinnen_90_17$year == 2013 &
                                kandidatinnen_90_17$bula == "Bayern"] <- "csu"

kandidatinnen_90_17$winner_13[kandidatinnen_90_17$spd_erst > kandidatinnen_90_17$cdu_erst &
                                kandidatinnen_90_17$spd_erst > kandidatinnen_90_17$gru_erst &
                                kandidatinnen_90_17$spd_erst > kandidatinnen_90_17$pds_erst &
                                kandidatinnen_90_17$spd_erst > kandidatinnen_90_17$fdp_erst &
                                kandidatinnen_90_17$year == 2013 &
                                kandidatinnen_90_17$bula != "Bayern"] <- "spd"
kandidatinnen_90_17$winner_13[kandidatinnen_90_17$spd_erst > kandidatinnen_90_17$csu_erst &
                                kandidatinnen_90_17$spd_erst > kandidatinnen_90_17$gru_erst &
                                kandidatinnen_90_17$spd_erst > kandidatinnen_90_17$pds_erst &
                                kandidatinnen_90_17$spd_erst > kandidatinnen_90_17$fdp_erst &
                                kandidatinnen_90_17$year == 2013 &
                                kandidatinnen_90_17$bula == "Bayern"] <- "spd"

kandidatinnen_90_17$winner_13[kandidatinnen_90_17$gru_erst > kandidatinnen_90_17$cdu_erst &
                                kandidatinnen_90_17$gru_erst > kandidatinnen_90_17$spd_erst &
                                kandidatinnen_90_17$gru_erst > kandidatinnen_90_17$pds_erst &
                                kandidatinnen_90_17$gru_erst > kandidatinnen_90_17$fdp_erst &
                                kandidatinnen_90_17$year == 2013 &
                                kandidatinnen_90_17$bula != "Bayern"] <- "gru"
kandidatinnen_90_17$winner_13[kandidatinnen_90_17$gru_erst > kandidatinnen_90_17$csu_erst &
                                kandidatinnen_90_17$gru_erst > kandidatinnen_90_17$spd_erst &
                                kandidatinnen_90_17$gru_erst > kandidatinnen_90_17$pds_erst &
                                kandidatinnen_90_17$gru_erst > kandidatinnen_90_17$fdp_erst &
                                kandidatinnen_90_17$year == 2013 &
                                kandidatinnen_90_17$bula == "Bayern"] <- "gru"

kandidatinnen_90_17$winner_13[kandidatinnen_90_17$pds_erst > kandidatinnen_90_17$cdu_erst &
                                kandidatinnen_90_17$pds_erst > kandidatinnen_90_17$gru_erst &
                                kandidatinnen_90_17$pds_erst > kandidatinnen_90_17$spd_erst &
                                kandidatinnen_90_17$pds_erst > kandidatinnen_90_17$fdp_erst &
                                kandidatinnen_90_17$year == 2013 &
                                kandidatinnen_90_17$bula != "Bayern"] <- "pds"
kandidatinnen_90_17$winner_13[kandidatinnen_90_17$pds_erst > kandidatinnen_90_17$csu_erst &
                                kandidatinnen_90_17$pds_erst > kandidatinnen_90_17$gru_erst &
                                kandidatinnen_90_17$pds_erst > kandidatinnen_90_17$spd_erst &
                                kandidatinnen_90_17$pds_erst > kandidatinnen_90_17$fdp_erst &
                                kandidatinnen_90_17$year == 2013 &
                                kandidatinnen_90_17$bula == "Bayern"] <- "pds"

kandidatinnen_90_17$winner_13[kandidatinnen_90_17$fdp_erst > kandidatinnen_90_17$cdu_erst &
                                kandidatinnen_90_17$fdp_erst > kandidatinnen_90_17$gru_erst &
                                kandidatinnen_90_17$fdp_erst > kandidatinnen_90_17$pds_erst &
                                kandidatinnen_90_17$fdp_erst > kandidatinnen_90_17$spd_erst &
                                kandidatinnen_90_17$year == 2013 &
                                kandidatinnen_90_17$bula != "Bayern"] <- "fdp"
kandidatinnen_90_17$winner_13[kandidatinnen_90_17$fdp_erst > kandidatinnen_90_17$csu_erst &
                                kandidatinnen_90_17$fdp_erst > kandidatinnen_90_17$gru_erst &
                                kandidatinnen_90_17$fdp_erst > kandidatinnen_90_17$pds_erst &
                                kandidatinnen_90_17$fdp_erst > kandidatinnen_90_17$spd_erst &
                                kandidatinnen_90_17$year == 2013 &
                                kandidatinnen_90_17$bula == "Bayern"] <- "fdp"

# Two remain NA
kandidatinnen_90_17$winner_13[kandidatinnen_90_17$year == 2013 & kandidatinnen_90_17$wkr_nummer == 112] <- "cdu"
kandidatinnen_90_17$winner_13[kandidatinnen_90_17$year == 2013 & kandidatinnen_90_17$wkr_nummer == 229] <- "csu"


# If IDs of 2013 and 2017 match, and if the candidate was a winner_13

remove(id_13, id_17, wkr_13, winner)
kandidatinnen_90_17$cdu_k_inc[kandidatinnen_90_17$year == 2017] <- 0
kandidatinnen_90_17$csu_k_inc[kandidatinnen_90_17$year == 2017] <- 0
kandidatinnen_90_17$spd_k_inc[kandidatinnen_90_17$year == 2017] <- 0
kandidatinnen_90_17$gru_k_inc[kandidatinnen_90_17$year == 2017] <- 0
kandidatinnen_90_17$pds_k_inc[kandidatinnen_90_17$year == 2017] <- 0
kandidatinnen_90_17$fdp_k_inc[kandidatinnen_90_17$year == 2017] <- 0

# Set all incumbents in 2017 to 1
for (i in 22:27) {
  for (k in 1:299) {
    wkr_13 <- kandidatinnen_90_17$wkr_nummer_13[k]
    id_13 <- as.character(kandidatinnen_90_17[kandidatinnen_90_17$year==2013 & kandidatinnen_90_17$wkr_nummer==wkr_13, i])
    id_17 <- as.character(kandidatinnen_90_17[k, i])
    winner <- kandidatinnen_90_17$winner_13[kandidatinnen_90_17$wkr_nummer==wkr_13 & kandidatinnen_90_17$year==2013]
    
    if(!is.na(id_13) & !is.na(id_17) & !is.na(wkr_13) & !is.na(winner)) {
    if(id_13 == id_17 & winner == "cdu" & i == 22) kandidatinnen_90_17$cdu_k_inc[k] <- 1
    if(id_13 == id_17 & winner == "csu" & i == 23) kandidatinnen_90_17$csu_k_inc[k] <- 1
    if(id_13 == id_17 & winner == "spd" & i == 24) kandidatinnen_90_17$spd_k_inc[k] <- 1
    if(id_13 == id_17 & winner == "gru" & i == 25) kandidatinnen_90_17$gru_k_inc[k] <- 1
    if(id_13 == id_17 & winner == "pds" & i == 26) kandidatinnen_90_17$pds_k_inc[k] <- 1
    if(id_13 == id_17 & winner == "fdp" & i == 27) kandidatinnen_90_17$fdp_k_inc[k] <- 1
    }
    
    wkr_13 <- NA
    id_13 <- NA
    id_17 <- NA
    winner <- NA
  }
}

# Set all csu_k_inc for non Bayern to NA
kandidatinnen_90_17$csu_k_inc[kandidatinnen_90_17$bula != "Bayern"] <- NA

# Set all cdu_k_inc for Bayern to NA
kandidatinnen_90_17$cdu_k_inc[kandidatinnen_90_17$bula == "Bayern"] <- NA



write.csv2(kandidatinnen_90_17, file="kandidatinnen_90_17.csv", quote=FALSE) 







