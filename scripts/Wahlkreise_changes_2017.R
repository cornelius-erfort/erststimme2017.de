### Making a file to track Wahlkreis changes for 2017
### Final goal: Create file for 2002-2017 Wahlkreise and changes for use in models

# setwd("/Users/cornelius/Desktop/erststimme2017.de/R")

library(R2HTML)
library(rgdal)
library(spdep)
library(rgeos)
library(plyr)
library(readstata13)

wkrnamen2017 <- read.csv2("btw17_wahlkreisnamen.csv", sep = ";", skip = 5, stringsAsFactors = FALSE, encoding="latin1")
#View(wkrnamen2017)

districts02_13 <- read.dta13("districts_2002_2013.dta")
#View(districts02_13)


vars <- c("wkr_nr2013", 
          "change2013", 
          "new2013", 
          "wkr_name_2013"
)

districts2013 <- districts02_13[, vars]
#View(districts2013)

districts2013[districts2013$new2013==1, ]


# wkrnamen2017
change2017 <- vector(mode="numeric", length=299)
new2017 <- vector(mode="numeric", length=299)
wkr_nr2013 <- vector(mode="numeric", length=299)
ost_2017 <- vector(mode="numeric", length=299)
wkr2017<- cbind(wkrnamen2017, change2017, new2017, wkr_nr2013, ost_2017)
colnames(wkr2017)[2] <- "wkr_name_2017"
colnames(wkr2017)[1] <- "wkr_nr2017"
colnames(wkr2017)[3] <- "bula_2017"
colnames(wkr2017)[4] <- "bula_name_2017"
colnames(wkr2017)[5] <- "bula_abk_2017"

#View(wkr2017)



# change2017: 
# The following Wahlkreise had changes in boundaries according to Bundeswahlleiter:
# 12, 13, 18, 22, 56:62, 189:191, 194:196, 205, 206, 214, 215, 221, 223, 224, 226, 216, 227:230, 238, 239, 260, 265

changes <- c(12, 13, 18, 22, seq(56,62,1), seq(189,191,1), seq(194, 196, 1), 205, 206, 214, 215, 221, 223, 224, 226, 216, seq(227,230,1), 238, 239, 260, 265)
wkr2017$change2017[changes] <- c(1)

# wkr_nr2013
wkr2017$wkr_nr2013[1:188] <- wkr2017$wkr_nr2017[1:188] #all Wahlkreise up to Thüringen are numbers unchanged..
wkr2017$wkr_nr2013[225:299] <- wkr2017$wkr_nr2017[225:299] #all Wahlkreise following new 224 Starnberg Wahlkreis remain unchanged


wkr2017$wkr_nr2013[197:222] <- wkr2017$wkr_nr2017[197:222]+1 # from Thüringen to new 224 numbers dropped by 1 from 2013 to 2017

wkr2017$wkr_nr2013[223:224] <- c(224) #new 224 and 223 are assigned the old 224, although Bundeswahlleiter says: 223 is old 224, 224 is new

wkr2017$wkr_nr2013[192:193] <- wkr2017$wkr_nr2017[192:193] #Wahlkreise 192, 193 in Thüringen retain old number, are unchanged

wkr2017$wkr_nr2013[c(189, 190, 191)] <- wkr2017$wkr_nr2017[c(189, 190, 191)] #Wahlkreise 189, 190, 191 have been changed by entire Landkreise, but are IMHO best described by the equal numbers in 2013

wkr2017$wkr_nr2013[c(194, 195, 196)] <- wkr2017$wkr_nr2017[c(195, 196, 197)] #Wahlkreise 194, 195, 196 have been changed by entire Landkreise, but are IMHO best described by the numbers in 2013 plus 1

#This leaves the old Wahlkreis 194 without a new Wahlkreis, although its population has been divided up into the other Wahlkreise in Thüringen

##### TODO:
# Bayern:     look at exact changes of Wahlkreise and maybe assign factors
# Thueringen: look at exact changes of Wahlkreise and maybe assign factors

# new2013
wkr2017$new2017[224] <- c(1) 

# ost_2017
wkr2017$ost_2017[wkr2017$bula_2017 > 10 & wkr2017$bula_2017 < 17] <- c(1) # bula_2017 11:16 are ost_2017 (including all of Berlin)


# shaping other columns to fit to Simon's district history data



districts02_17 <- merge(wkr2017, districts02_13,by="wkr_nr2013")
#View(districts02_17)

write.csv2(districts02_17, file="districts02_17.csv", sep=";", quote=FALSE) 

districts02_17 <- read.csv2("districts02_17.csv", sep = ";", stringsAsFactors = FALSE)
View(districts02_17)
