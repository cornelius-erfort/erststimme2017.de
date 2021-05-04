
setwd("/Users/cornelius/erststimme2017.de/R")


## Reshaping for analysis

kandidatinnen_90_17_master <- read.csv2("kandidatinnen_90_17.csv", sep = ";", stringsAsFactors = FALSE)
districts02_17 <- read.csv2("districts02_17.csv", sep = ";", stringsAsFactors = FALSE)

# Adding bula_2013
districts02_17$bula_2013 <- 1
districts02_17$bula_2013[districts02_17$wkr_nr2013 > 11 & districts02_17$wkr_nr2013 < 18] <- 13 #MV 
districts02_17$bula_2013[districts02_17$wkr_nr2013 > 17 & districts02_17$wkr_nr2013 < 24] <- 2 #HH
districts02_17$bula_2013[districts02_17$wkr_nr2013 > 23 & districts02_17$wkr_nr2013 < 54] <- 3 #NieSa 
districts02_17$bula_2013[districts02_17$wkr_nr2013 > 53 & districts02_17$wkr_nr2013 < 56] <- 4 #Br 
districts02_17$bula_2013[districts02_17$wkr_nr2013 > 55 & districts02_17$wkr_nr2013 < 66] <- 12 #BraB 
districts02_17$bula_2013[districts02_17$wkr_nr2013 > 65 & districts02_17$wkr_nr2013 < 75] <- 15 #SA
districts02_17$bula_2013[districts02_17$wkr_nr2013 > 74 & districts02_17$wkr_nr2013 < 87] <- 11 #Ber 
districts02_17$bula_2013[districts02_17$wkr_nr2013 > 86 & districts02_17$wkr_nr2013 < 151] <- 5 #NRW
districts02_17$bula_2013[districts02_17$wkr_nr2013 > 150 & districts02_17$wkr_nr2013 < 167] <- 14 #Sachs 
districts02_17$bula_2013[districts02_17$wkr_nr2013 > 166 & districts02_17$wkr_nr2013 < 189] <- 6 #He
districts02_17$bula_2013[districts02_17$wkr_nr2013 > 188 & districts02_17$wkr_nr2013 < 198] <- 16 #Th 
districts02_17$bula_2013[districts02_17$wkr_nr2013 > 187 & districts02_17$wkr_nr2013 < 213] <- 7 #RP
districts02_17$bula_2013[districts02_17$wkr_nr2013 > 212 & districts02_17$wkr_nr2013 < 258] <- 9 #Bay 
districts02_17$bula_2013[districts02_17$wkr_nr2013 > 257 & districts02_17$wkr_nr2013 < 296] <- 8 #BW
districts02_17$bula_2013[districts02_17$wkr_nr2013 > 295] <- 10 #Saar 

# Adding ost_2013
districts02_17$ost_2013 <- 0
districts02_17$ost_2013[districts02_17$bula_2013 > 10] <- 1



# Dropping some (incumbency) columns (that I don't understand...)
kandidatinnen_90_17 <- kandidatinnen_90_17_master[, c(2:57, 76)]


# Reordering name columns to the very right (because they aren't needed for analysis)
kandidatinnen_90_17 <- kandidatinnen_90_17[, c(1:3, 6, 9, 12, 15, 18, 21, 30:48, 51:57, 4, 7, 10, 13, 16, 19, 5, 8, 11, 14, 17, 20, 22:27)]



# Dropping ungueltige Stimmen
kandidatinnen_90_17 <- kandidatinnen_90_17[, c(1:12,15:53)]




# Adding districts information from other data (especially corresponding 2017 Wahlkreis for each obs)
# Adding Wahlkreisnamen for 2017 obs
wkr_names_17 <- districts02_17[, 3:4]
wkr_names_17$year <- 2017
colnames(wkr_names_17) <- c("wkr_nummer", "wkr_name", "year")
kandidatinnen_90_17 <- merge(kandidatinnen_90_17, wkr_names_17, by.x = c("year", "wkr_nummer"), by.y = c("year", "wkr_nummer"), all.x = TRUE)

kandidatinnen_90_17$wkr_name.x[kandidatinnen_90_17$year == 2017] <- kandidatinnen_90_17$wkr_name.y[kandidatinnen_90_17$year == 2017]
kandidatinnen_90_17 <- kandidatinnen_90_17[, 1:51]
colnames(kandidatinnen_90_17)[10] <- "wkr_name"

# Adding wkr_change, wkr_new, and wkr_ost for 2017
wkr_info_17 <- districts02_17[, c(3, 8:10)]
wkr_info_17$year <- 2017
kandidatinnen_90_17 <- merge(kandidatinnen_90_17, wkr_info_17, by.x = c("year", "wkr_nummer"), by.y = c("year", "wkr_nr2017"), all.x = TRUE)
colnames(kandidatinnen_90_17)[52:54] <- c("wkr_change", "wkr_new", "bula_ost")

# Adding wkr_nr17, wkr_change, wkr_new, and wkr_ost for 2013
wkr_info_13 <- districts02_17[, c(2, 3, 15, 16, 113)]
wkr_info_13$year <- 2013
kandidatinnen_90_17 <- merge(kandidatinnen_90_17, wkr_info_13, by.x = c("year", "wkr_nummer"), by.y = c("year", "wkr_nr2013"), all.x = TRUE)
kandidatinnen_90_17$bula_ost[kandidatinnen_90_17$year == 2013] <- kandidatinnen_90_17$ost_2013[kandidatinnen_90_17$year == 2013]
kandidatinnen_90_17$wkr_change[kandidatinnen_90_17$year == 2013] <- kandidatinnen_90_17$change2013[kandidatinnen_90_17$year == 2013]
kandidatinnen_90_17$wkr_new[kandidatinnen_90_17$year == 2013] <- kandidatinnen_90_17$new2013[kandidatinnen_90_17$year == 2013]
kandidatinnen_90_17 <- kandidatinnen_90_17[, 1:55]

# Setting wkr_nr17 to wkr_nummer for 2017
kandidatinnen_90_17$wkr_nr2017[kandidatinnen_90_17$year == 2017] <- kandidatinnen_90_17$wkr_nummer[kandidatinnen_90_17$year == 2017]




# Adding wkr_nr17, wkr_change, wkr_new, and wkr_ost for 2009
wkr_info_09 <- districts02_17[, c(3, 11,  17, 18, 64)]
wkr_info_09$year <- 2009
kandidatinnen_90_17 <- merge(kandidatinnen_90_17, wkr_info_09, by.x = c("year", "wkr_nummer"), by.y = c("year", "wkr_nr2009"), all.x = TRUE)
kandidatinnen_90_17$bula_ost[kandidatinnen_90_17$year == 2009] <- kandidatinnen_90_17$ost_2009[kandidatinnen_90_17$year == 2009]
kandidatinnen_90_17$wkr_change[kandidatinnen_90_17$year == 2009] <- kandidatinnen_90_17$change2009[kandidatinnen_90_17$year == 2009]
kandidatinnen_90_17$wkr_new[kandidatinnen_90_17$year == 2009] <- kandidatinnen_90_17$new2009[kandidatinnen_90_17$year == 2009]
kandidatinnen_90_17$wkr_nr2017.x[kandidatinnen_90_17$year == 2009] <- kandidatinnen_90_17$wkr_nr2017.y[kandidatinnen_90_17$year == 2009]
kandidatinnen_90_17 <- kandidatinnen_90_17[, 1:55]
colnames(kandidatinnen_90_17)[55] <- "wkr_nr2017"



# Adding wkr_nr17, wkr_change, wkr_new, and wkr_ost for 2005
wkr_info_05 <- districts02_17[, c(3, 12, 19, 20, 45)]
wkr_info_05$year <- 2005
kandidatinnen_90_17 <- merge(kandidatinnen_90_17, wkr_info_05, by.x = c("year", "wkr_nummer"), by.y = c("year", "wkr_nr2005"), all.x = TRUE)
kandidatinnen_90_17$bula_ost[kandidatinnen_90_17$year == 2005] <- kandidatinnen_90_17$ost_2005[kandidatinnen_90_17$year == 2005]
kandidatinnen_90_17$wkr_change[kandidatinnen_90_17$year == 2005] <- kandidatinnen_90_17$change2005[kandidatinnen_90_17$year == 2005]
kandidatinnen_90_17$wkr_new[kandidatinnen_90_17$year == 2005] <- kandidatinnen_90_17$new2005[kandidatinnen_90_17$year == 2005]
kandidatinnen_90_17$wkr_nr2017.x[kandidatinnen_90_17$year == 2005] <- kandidatinnen_90_17$wkr_nr2017.y[kandidatinnen_90_17$year == 2005]
kandidatinnen_90_17 <- kandidatinnen_90_17[, 1:55]
colnames(kandidatinnen_90_17)[55] <- "wkr_nr2017"



# Adding wkr_nr17, wkr_change, wkr_new, and wkr_ost for 2002
wkr_info_02 <- districts02_17[, c(3, 13, 21, 22, 26)]
wkr_info_02$year <- 2002
kandidatinnen_90_17 <- merge(kandidatinnen_90_17, wkr_info_02, by.x = c("year", "wkr_nummer"), by.y = c("year", "wkr_nr2002"), all.x = TRUE)
kandidatinnen_90_17$bula_ost[kandidatinnen_90_17$year == 2002] <- kandidatinnen_90_17$ost_2002[kandidatinnen_90_17$year == 2002]
kandidatinnen_90_17$wkr_change[kandidatinnen_90_17$year == 2002] <- kandidatinnen_90_17$change2002[kandidatinnen_90_17$year == 2002]
kandidatinnen_90_17$wkr_new[kandidatinnen_90_17$year == 2002] <- kandidatinnen_90_17$new2002[kandidatinnen_90_17$year == 2002]
kandidatinnen_90_17$wkr_nr2017.x[kandidatinnen_90_17$year == 2002] <- kandidatinnen_90_17$wkr_nr2017.y[kandidatinnen_90_17$year == 2002]
kandidatinnen_90_17 <- kandidatinnen_90_17[, 1:55]
colnames(kandidatinnen_90_17)[55] <- "wkr_nr2017"


###
kandidatinnen_90_17 <- kandidatinnen_90_17[, c(1:32, 34:55)]



# Assuming 0 votes is actually "didn't run"
kandidatinnen_90_17$cdu_erst[kandidatinnen_90_17$cdu_erst == 0] <- NA
kandidatinnen_90_17$csu_erst[kandidatinnen_90_17$csu_erst == 0] <- NA
kandidatinnen_90_17$spd_erst[kandidatinnen_90_17$spd_erst == 0] <- NA
kandidatinnen_90_17$gru_erst[kandidatinnen_90_17$gru_erst == 0] <- NA
kandidatinnen_90_17$pds_erst[kandidatinnen_90_17$pds_erst == 0] <- NA
kandidatinnen_90_17$fdp_erst[kandidatinnen_90_17$fdp_erst == 0] <- NA



# Determining the winner of each race

# CDU winners
kandidatinnen_90_17$cdu_k_winner <- NA
for (i in 1:2491) {
    if(!is.na(kandidatinnen_90_17$cdu_erst[i])) {
  
      kandidatinnen_90_17$cdu_k_winner[i] <- 1
 
  if(!is.na(kandidatinnen_90_17$spd_erst[i])) {
    if(as.numeric(kandidatinnen_90_17$cdu_erst[i]) < as.numeric(kandidatinnen_90_17$spd_erst[i])) {kandidatinnen_90_17$cdu_k_winner[i] <- 0}
    }
  if(!is.na(kandidatinnen_90_17$gru_erst[i])) {
    if(as.numeric(kandidatinnen_90_17$cdu_erst[i]) < as.numeric(kandidatinnen_90_17$gru_erst[i])) {kandidatinnen_90_17$cdu_k_winner[i] <- 0}
    }
  if(!is.na(kandidatinnen_90_17$pds_erst[i])) {
    if(as.numeric(kandidatinnen_90_17$cdu_erst[i]) < as.numeric(kandidatinnen_90_17$pds_erst[i])) {kandidatinnen_90_17$cdu_k_winner[i] <- 0}
    }
  if(!is.na(kandidatinnen_90_17$fdp_erst[i])) {
    if(as.numeric(kandidatinnen_90_17$cdu_erst[i]) < as.numeric(kandidatinnen_90_17$fdp_erst[i])) {kandidatinnen_90_17$cdu_k_winner[i] <- 0}
    }
  
  }
}

# CSU winners
kandidatinnen_90_17$csu_k_winner <- NA
for (i in 1:2491) {
  if(!is.na(kandidatinnen_90_17$csu_erst[i])) {
    
    kandidatinnen_90_17$csu_k_winner[i] <- 1
    
    if(!is.na(kandidatinnen_90_17$spd_erst[i])) {
      if(as.numeric(kandidatinnen_90_17$csu_erst[i]) < as.numeric(kandidatinnen_90_17$spd_erst[i])) {kandidatinnen_90_17$csu_k_winner[i] <- 0}
    }
    if(!is.na(kandidatinnen_90_17$gru_erst[i])) {
      if(as.numeric(kandidatinnen_90_17$csu_erst[i]) < as.numeric(kandidatinnen_90_17$gru_erst[i])) {kandidatinnen_90_17$csu_k_winner[i] <- 0}
    }
    if(!is.na(kandidatinnen_90_17$pds_erst[i])) {
      if(as.numeric(kandidatinnen_90_17$csu_erst[i]) < as.numeric(kandidatinnen_90_17$pds_erst[i])) {kandidatinnen_90_17$csu_k_winner[i] <- 0}
    }
    if(!is.na(kandidatinnen_90_17$fdp_erst[i])) {
      if(as.numeric(kandidatinnen_90_17$csu_erst[i]) < as.numeric(kandidatinnen_90_17$fdp_erst[i])) {kandidatinnen_90_17$csu_k_winner[i] <- 0}
    }
    
  }
}

# SPD winners
kandidatinnen_90_17$spd_k_winner <- NA
for (i in 1:2491) {
  if(!is.na(kandidatinnen_90_17$spd_erst[i])) {
    
    kandidatinnen_90_17$spd_k_winner[i] <- 1
    
    if(!is.na(kandidatinnen_90_17$cdu_erst[i])) {
      if(as.numeric(kandidatinnen_90_17$spd_erst[i]) < as.numeric(kandidatinnen_90_17$cdu_erst[i])) {kandidatinnen_90_17$spd_k_winner[i] <- 0}
    }
    if(!is.na(kandidatinnen_90_17$gru_erst[i])) {
      if(as.numeric(kandidatinnen_90_17$spd_erst[i]) < as.numeric(kandidatinnen_90_17$gru_erst[i])) {kandidatinnen_90_17$spd_k_winner[i] <- 0}
    }
    if(!is.na(kandidatinnen_90_17$csu_erst[i])) {
      if(as.numeric(kandidatinnen_90_17$spd_erst[i]) < as.numeric(kandidatinnen_90_17$csu_erst[i])) {kandidatinnen_90_17$spd_k_winner[i] <- 0}
    }
    if(!is.na(kandidatinnen_90_17$pds_erst[i])) {
      if(as.numeric(kandidatinnen_90_17$spd_erst[i]) < as.numeric(kandidatinnen_90_17$pds_erst[i])) {kandidatinnen_90_17$spd_k_winner[i] <- 0}
    }
    if(!is.na(kandidatinnen_90_17$fdp_erst[i])) {
      if(as.numeric(kandidatinnen_90_17$spd_erst[i]) < as.numeric(kandidatinnen_90_17$fdp_erst[i])) {kandidatinnen_90_17$spd_k_winner[i] <- 0}
    }
    
  }
}

# GRU winners
kandidatinnen_90_17$gru_k_winner <- NA
for (i in 1:2491) {
  if(!is.na(kandidatinnen_90_17$gru_erst[i])) {
    
    kandidatinnen_90_17$gru_k_winner[i] <- 1
    
    if(!is.na(kandidatinnen_90_17$cdu_erst[i])) {
      if(as.numeric(kandidatinnen_90_17$gru_erst[i]) < as.numeric(kandidatinnen_90_17$cdu_erst[i])) {kandidatinnen_90_17$gru_k_winner[i] <- 0}
    }
    if(!is.na(kandidatinnen_90_17$spd_erst[i])) {
      if(as.numeric(kandidatinnen_90_17$gru_erst[i]) < as.numeric(kandidatinnen_90_17$spd_erst[i])) {kandidatinnen_90_17$gru_k_winner[i] <- 0}
    }
    if(!is.na(kandidatinnen_90_17$csu_erst[i])) {
      if(as.numeric(kandidatinnen_90_17$gru_erst[i]) < as.numeric(kandidatinnen_90_17$csu_erst[i])) {kandidatinnen_90_17$gru_k_winner[i] <- 0}
    }
    if(!is.na(kandidatinnen_90_17$pds_erst[i])) {
      if(as.numeric(kandidatinnen_90_17$gru_erst[i]) < as.numeric(kandidatinnen_90_17$pds_erst[i])) {kandidatinnen_90_17$gru_k_winner[i] <- 0}
    }
    if(!is.na(kandidatinnen_90_17$fdp_erst[i])) {
      if(as.numeric(kandidatinnen_90_17$gru_erst[i]) < as.numeric(kandidatinnen_90_17$fdp_erst[i])) {kandidatinnen_90_17$gru_k_winner[i] <- 0}
    }
    
  }
}

# PDS winners
kandidatinnen_90_17$pds_k_winner <- NA
for (i in 1:2491) {
  if(!is.na(kandidatinnen_90_17$pds_erst[i])) {
    
    kandidatinnen_90_17$pds_k_winner[i] <- 1
    
    if(!is.na(kandidatinnen_90_17$cdu_erst[i])) {
      if(as.numeric(kandidatinnen_90_17$pds_erst[i]) < as.numeric(kandidatinnen_90_17$cdu_erst[i])) {kandidatinnen_90_17$pds_k_winner[i] <- 0}
    }
    if(!is.na(kandidatinnen_90_17$spd_erst[i])) {
      if(as.numeric(kandidatinnen_90_17$pds_erst[i]) < as.numeric(kandidatinnen_90_17$spd_erst[i])) {kandidatinnen_90_17$pds_k_winner[i] <- 0}
    }
    if(!is.na(kandidatinnen_90_17$csu_erst[i])) {
      if(as.numeric(kandidatinnen_90_17$pds_erst[i]) < as.numeric(kandidatinnen_90_17$csu_erst[i])) {kandidatinnen_90_17$pds_k_winner[i] <- 0}
    }
    if(!is.na(kandidatinnen_90_17$gru_erst[i])) {
      if(as.numeric(kandidatinnen_90_17$pds_erst[i]) < as.numeric(kandidatinnen_90_17$gru_erst[i])) {kandidatinnen_90_17$pds_k_winner[i] <- 0}
    }
    if(!is.na(kandidatinnen_90_17$fdp_erst[i])) {
      if(as.numeric(kandidatinnen_90_17$pds_erst[i]) < as.numeric(kandidatinnen_90_17$fdp_erst[i])) {kandidatinnen_90_17$pds_k_winner[i] <- 0}
    }
    
  }
}

# FDP winners
kandidatinnen_90_17$fdp_k_winner <- NA
for (i in 1:2491) {
  if(!is.na(kandidatinnen_90_17$fdp_erst[i])) {
    
    kandidatinnen_90_17$fdp_k_winner[i] <- 1
    
    if(!is.na(kandidatinnen_90_17$cdu_erst[i])) {
      if(as.numeric(kandidatinnen_90_17$fdp_erst[i]) < as.numeric(kandidatinnen_90_17$cdu_erst[i])) {kandidatinnen_90_17$fdp_k_winner[i] <- 0}
    }
    if(!is.na(kandidatinnen_90_17$spd_erst[i])) {
      if(as.numeric(kandidatinnen_90_17$fdp_erst[i]) < as.numeric(kandidatinnen_90_17$spd_erst[i])) {kandidatinnen_90_17$fdp_k_winner[i] <- 0}
    }
    if(!is.na(kandidatinnen_90_17$csu_erst[i])) {
      if(as.numeric(kandidatinnen_90_17$fdp_erst[i]) < as.numeric(kandidatinnen_90_17$csu_erst[i])) {kandidatinnen_90_17$fdp_k_winner[i] <- 0}
    }
    if(!is.na(kandidatinnen_90_17$gru_erst[i])) {
      if(as.numeric(kandidatinnen_90_17$fdp_erst[i]) < as.numeric(kandidatinnen_90_17$gru_erst[i])) {kandidatinnen_90_17$fdp_k_winner[i] <- 0}
    }
    if(!is.na(kandidatinnen_90_17$pds_erst[i])) {
      if(as.numeric(kandidatinnen_90_17$fdp_erst[i]) < as.numeric(kandidatinnen_90_17$pds_erst[i])) {kandidatinnen_90_17$fdp_k_winner[i] <- 0}
    }
    
  }
}


# RESHAPE wide to long, 6 parties, 4 variables by party: _platz, _zweit, _erst, _inc, _vname

# 1 CDU, 2 CSU, 3 SPD, 4, GRU, 5 PDS, 6 FDP

kandidatinnen_90_17_wide <- kandidatinnen_90_17
colnames(kandidatinnen_90_17)

colnames(kandidatinnen_90_17_wide) <-c(
"year"     ,    "wkr_nummer"  , "bula"      ,   "k_platz.1" , "k_platz.2" , "k_platz.3" , "k_platz.4" ,"k_platz.5"  ,"k_platz.6" ,
"wkr_name"  ,   "wbr"      ,    "wlr"     ,     "glt_erst" ,    "glt_zweit"   , "erst.1"  ,   "zweit.1"  ,  "erst.3"  ,   "zweit.3"  , 
"erst.6" ,    "zweit.6" ,   "erst.5"  ,   "zweit.5"  ,  "erst.4"   ,  "zweit.4"  ,  "erst.2"   ,  "zweit.2"  ,  "k_inc.1"  , 
"k_inc.2"   , "k_inc.3" ,   "k_inc.6"  ,  "k_inc.4"  ,  "k_inc.5",  "k_vname.1" , "k_vname.2" , "k_vname.3" , "k_vname.4" ,
"k_vname.5" , "k_vname.6" , "k_nname.1" , "k_nname.2" , "k_nname.3" , "k_nname.4",  "k_nname.5" , "k_nname.6",  "k_idname.1",
"k_idname.2" ,"k_idname.3" ,"k_idname.4" ,"k_idname.5" ,"k_idname.6",  "wkr_change"  , "wkr_new"  ,    "bula_ost"   ,  "wkr_nr2017" , 
"k_winner.1" ,"k_winner.2", "k_winner.3" ,"k_winner.4", "k_winner.5" ,"k_winner.6")

kandidatinnen_90_17_wide$party.1 <- "CDU"
kandidatinnen_90_17_wide$party.2 <- "CSU"
kandidatinnen_90_17_wide$party.3 <- "SPD"
kandidatinnen_90_17_wide$party.4 <- "GRU"
kandidatinnen_90_17_wide$party.5 <- "PDS"
kandidatinnen_90_17_wide$party.6 <- "FDP"

# Reordering data
kandidatinnen_90_17_wide <- kandidatinnen_90_17_wide[, c(1:14, 15, 25, 17, 23, 21, 19, 16, 26, 18, 24, 22, 20, 27, 28, 29, 31, 32, 30, 33:66)]


kandidatinnen_90_17_long <- reshape(kandidatinnen_90_17_wide, varying = names(kandidatinnen_90_17_wide)[grep(".*\\..*", names(kandidatinnen_90_17_wide), value = FALSE)], direction = "long", sep = ".")

# Setting all zweit == 0 to NA (assuming that mostly this is CSU in other Bundeslaender)
kandidatinnen_90_17_long$zweit[kandidatinnen_90_17_long$zweit == 0] <- NA

# Correcting candidate incumbency (since the existing variables seem flawed)
# 2013

kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "merkel_angela" & kandidatinnen_90_17_long$year == 2013] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "lietz_matthias" & kandidatinnen_90_17_long$year == 2013] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "rehberg_eckhardt" & kandidatinnen_90_17_long$year == 2013] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "kruse_ruediger" & kandidatinnen_90_17_long$year == 2013] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "brandt_helmut" & kandidatinnen_90_17_long$year == 2013] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "braun_helge" & kandidatinnen_90_17_long$year == 2013] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "brand_michael" & kandidatinnen_90_17_long$year == 2013] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "schroeder_kristina" & kandidatinnen_90_17_long$year == 2013] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "bleser_peter" & kandidatinnen_90_17_long$year == 2013] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "hasselfeldt_gerda" & kandidatinnen_90_17_long$year == 2013] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "brandl_reinhard" & kandidatinnen_90_17_long$year == 2013] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "hahn_florian" & kandidatinnen_90_17_long$year == 2013] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "kahrs_johannes" & kandidatinnen_90_17_long$year == 2013] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "bockhahn_steffen" & kandidatinnen_90_17_long$year == 2013] <- 1

# 2009
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "landgraf_katharina" & kandidatinnen_90_17_long$year == 2009] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "michalk_maria" & kandidatinnen_90_17_long$year == 2009] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "kretschmer_michael" & kandidatinnen_90_17_long$year == 2009] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "braehmig_klaus" & kandidatinnen_90_17_long$year == 2009] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "vaatz_arnold" & kandidatinnen_90_17_long$year == 2009] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "bellmann_veronika" & kandidatinnen_90_17_long$year == 2009] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "wanderwitz_marco" & kandidatinnen_90_17_long$year == 2009] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "baumann_guenter" & kandidatinnen_90_17_long$year == 2009] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "luther_michael" & kandidatinnen_90_17_long$year == 2009] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "hochbaum_robert" & kandidatinnen_90_17_long$year == 2009] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "schockenhoff_andreas" & kandidatinnen_90_17_long$year == 2009] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "bareiss_thomas" & kandidatinnen_90_17_long$year == 2009] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "hiller-ohm_gabriele" & kandidatinnen_90_17_long$year == 2009] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "wetzel_margrit" & kandidatinnen_90_17_long$year == 2009] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "stuenker_joachim" & kandidatinnen_90_17_long$year == 2009] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "griefahn_monika" & kandidatinnen_90_17_long$year == 2009] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "wolff_waltraud" & kandidatinnen_90_17_long$year == 2009] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "steppuhn_andreas" & kandidatinnen_90_17_long$year == 2009] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "wistuba_engelbert" & kandidatinnen_90_17_long$year == 2009] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "huebner_klaas" & kandidatinnen_90_17_long$year == 2009] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "reichel_maik" & kandidatinnen_90_17_long$year == 2009] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "schmidt_silvia" & kandidatinnen_90_17_long$year == 2009] <- 1

# 2005
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "merkel_petra-evelyne" & kandidatinnen_90_17_long$year == 2005] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "deittert_hubert" & kandidatinnen_90_17_long$year == 2005] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "waechter_gerhard" & kandidatinnen_90_17_long$year == 2005] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "haibach_holger-heinrich" & kandidatinnen_90_17_long$year == 2005] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "grund_manfred" & kandidatinnen_90_17_long$year == 2005] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "kranz_ernst" & kandidatinnen_90_17_long$year == 2005] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "schneider_carsten" & kandidatinnen_90_17_long$year == 2005] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "baetzing_sabine" & kandidatinnen_90_17_long$year == 2005] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "mayer_stephan" & kandidatinnen_90_17_long$year == 2005] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "obermeier_franz" & kandidatinnen_90_17_long$year == 2005] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "aigner_ilse" & kandidatinnen_90_17_long$year == 2005] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "ramsauer_peter" & kandidatinnen_90_17_long$year == 2005] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "oswald_eduard" & kandidatinnen_90_17_long$year == 2005] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "raidel_hans" & kandidatinnen_90_17_long$year == 2005] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "binninger_clemens" & kandidatinnen_90_17_long$year == 2005] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "hennrich_michael" & kandidatinnen_90_17_long$year == 2005] <- 1
kandidatinnen_90_17_long$k_inc[kandidatinnen_90_17_long$k_idname == "gienger_eberhard" & kandidatinnen_90_17_long$year == 2005] <- 1




# Party incumbency

# 2005
remove(wkr_17, winner_02, party_05)
kandidatinnen_90_17_long$party_inc <- NA
for (i in unique(kandidatinnen_90_17_long$k_idname[kandidatinnen_90_17_long$year==2005])){
  # Checkin for NAs in used data
  if(!is.na(i)) {
    wkr_17 <- subset(kandidatinnen_90_17_long$wkr_nr2017, (kandidatinnen_90_17_long$k_idname == i) & (kandidatinnen_90_17_long$year == 2005) & !is.na(kandidatinnen_90_17_long$wkr_nr2017))
    party_05 <- subset(kandidatinnen_90_17_long$party, (kandidatinnen_90_17_long$k_idname == i) & (kandidatinnen_90_17_long$year == 2005) & !is.na(kandidatinnen_90_17_long$party))
    if(length(wkr_17) > 1) wkr_17 <- wkr_17[1]
    if(length(party_05) != 0) kandidatinnen_90_17_long$party_inc[kandidatinnen_90_17_long$party == party_05 & kandidatinnen_90_17_long$year == 2005 & kandidatinnen_90_17_long$wkr_nr2017 == wkr_17] <- 0
    winner_02 <-   subset(kandidatinnen_90_17_long$k_winner, (kandidatinnen_90_17_long$party == party_05) & (kandidatinnen_90_17_long$year == 2002) & (kandidatinnen_90_17_long$wkr_nr2017 == wkr_17))
    if(length(winner_02) != 0) kandidatinnen_90_17_long$party_inc[kandidatinnen_90_17_long$party == party_05 & kandidatinnen_90_17_long$year == 2005 & kandidatinnen_90_17_long$wkr_nr2017 == wkr_17] <- winner_02
    remove(wkr_17, winner_02, party_05)
  }
}
remove(wkr_17, winner_02, party_05)


# 2009
remove(wkr_17, winner_05, party_09)
for (i in unique(kandidatinnen_90_17_long$k_idname[kandidatinnen_90_17_long$year==2009])){
  # Checkin for NAs in used data
  if(!is.na(i)) {
    wkr_17 <- subset(kandidatinnen_90_17_long$wkr_nr2017, (kandidatinnen_90_17_long$k_idname == i) & (kandidatinnen_90_17_long$year == 2009) & !is.na(kandidatinnen_90_17_long$wkr_nr2017))
    party_09 <- subset(kandidatinnen_90_17_long$party, (kandidatinnen_90_17_long$k_idname == i) & (kandidatinnen_90_17_long$year == 2009) & !is.na(kandidatinnen_90_17_long$party))
    if(length(wkr_17) > 1) wkr_17 <- wkr_17[1]
    if(length(party_09) != 0) kandidatinnen_90_17_long$party_inc[kandidatinnen_90_17_long$party == party_09 & kandidatinnen_90_17_long$year == 2009 & kandidatinnen_90_17_long$wkr_nr2017 == wkr_17] <- 0
    winner_05 <-   subset(kandidatinnen_90_17_long$k_winner, (kandidatinnen_90_17_long$party == party_09) & (kandidatinnen_90_17_long$year == 2005) & (kandidatinnen_90_17_long$wkr_nr2017 == wkr_17))
    if(length(winner_05) != 0) kandidatinnen_90_17_long$party_inc[kandidatinnen_90_17_long$party == party_09 & kandidatinnen_90_17_long$year == 2009 & kandidatinnen_90_17_long$wkr_nr2017 == wkr_17] <- winner_05
    remove(wkr_17, winner_05, party_09)
  }
}
remove(wkr_17, winner_05, party_09)

# 2013
remove(wkr_17, winner_09, party_13)
for (i in unique(kandidatinnen_90_17_long$k_idname[kandidatinnen_90_17_long$year==2013])){
  # Checkin for NAs in used data
  if(!is.na(i)) {
    wkr_17 <- subset(kandidatinnen_90_17_long$wkr_nr2017, (kandidatinnen_90_17_long$k_idname == i) & (kandidatinnen_90_17_long$year == 2013) & !is.na(kandidatinnen_90_17_long$wkr_nr2017))
    party_13 <- subset(kandidatinnen_90_17_long$party, (kandidatinnen_90_17_long$k_idname == i) & (kandidatinnen_90_17_long$year == 2013) & !is.na(kandidatinnen_90_17_long$party))
    if(length(wkr_17) > 1) wkr_17 <- wkr_17[1]
    if(length(party_13) != 0) kandidatinnen_90_17_long$party_inc[kandidatinnen_90_17_long$party == party_13 & kandidatinnen_90_17_long$year == 2013 & kandidatinnen_90_17_long$wkr_nr2017 == wkr_17] <- 0
    winner_09 <-   subset(kandidatinnen_90_17_long$k_winner, (kandidatinnen_90_17_long$party == party_13) & (kandidatinnen_90_17_long$year == 2009) & (kandidatinnen_90_17_long$wkr_nr2017 == wkr_17))
    if(length(winner_09) != 0) kandidatinnen_90_17_long$party_inc[kandidatinnen_90_17_long$party == party_13 & kandidatinnen_90_17_long$year == 2013 & kandidatinnen_90_17_long$wkr_nr2017 == wkr_17] <- winner_09
    remove(wkr_17, winner_09, party_13)
  }
}
remove(wkr_17, winner_09, party_13)


# 2017
remove(wkr_17, winner_13, party_17)
for (i in unique(kandidatinnen_90_17_long$k_idname[kandidatinnen_90_17_long$year==2017])){
  # Checkin for NAs in used data
  if(!is.na(i)) {
    wkr_17 <- subset(kandidatinnen_90_17_long$wkr_nr2017, (kandidatinnen_90_17_long$k_idname == i) & (kandidatinnen_90_17_long$year == 2017) & !is.na(kandidatinnen_90_17_long$wkr_nr2017))
    party_17 <- subset(kandidatinnen_90_17_long$party, (kandidatinnen_90_17_long$k_idname == i) & (kandidatinnen_90_17_long$year == 2017) & !is.na(kandidatinnen_90_17_long$party))
    if(length(wkr_17) > 1) wkr_17 <- wkr_17[1]
    if(length(party_17) != 0) kandidatinnen_90_17_long$party_inc[kandidatinnen_90_17_long$party == party_17 & kandidatinnen_90_17_long$year == 2017 & kandidatinnen_90_17_long$wkr_nr2017 == wkr_17] <- 0
    winner_13 <-   subset(kandidatinnen_90_17_long$k_winner, (kandidatinnen_90_17_long$party == party_17) & (kandidatinnen_90_17_long$year == 2013) & (kandidatinnen_90_17_long$wkr_nr2017 == wkr_17))
    if(length(winner_13) != 0) kandidatinnen_90_17_long$party_inc[kandidatinnen_90_17_long$party == party_17 & kandidatinnen_90_17_long$year == 2017 & kandidatinnen_90_17_long$wkr_nr2017 == wkr_17] <- winner_13
    remove(wkr_17, winner_13, party_17)
  }
}
remove(wkr_17, winner_13, party_17)




#write.csv2(kandidatinnen_90_17_long, file="kandidatinnen_90_17_long.csv", quote=FALSE) 


View(kandidatinnen_90_17_long)






