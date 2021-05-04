
### Creating graphs

library(shapefiles)
library(rgdal)
library(spdep)
library(maptools)
library(rgeos)
library(ggplot2)
library(plyr)
library(raster)
library(dplyr)
library(stringr)



# Reading in shapefile from Bundeswahlleiter
shape <- readOGR(dsn="btw17_geometrie_wahlkreise_shp", layer="Geometrie_Wahlkreise_19DBT", stringsAsFactors = FALSE)

plot(shape)

#######
# WKR_NR = 18 (Hamburg) contains islands distorting the visualization
# I create a new shape (old shape: shape_old) without the island polygons

shape_old <- shape
hhs18 <- slot(shape, "polygons")[[18]]
hh18 <- slot(hhs18, "Polygons")
length(hh18)
sapply(hh18, function(x) {slot(x, "area")[1]})

res <- hh18[sapply(hh18, function(x) {slot(x, "area")[1]}) > 2957780]

slot(hhs18, "Polygons") <- res
slot(shape, "polygons")[[18]] <- hhs18
#######




# Reading in forecast data for colors
load("data/btw17_forecast.RData")

color_subset <- btw2017[btw2017$rank == 1, ]
head(color_subset)
color_subset$color <- NA
color_subset$color[color_subset$party == "CDU" | color_subset$party == "CSU"] <- "black"
color_subset$color[color_subset$party == "SPD"] <- "red"
color_subset$color[color_subset$party == "GRU"] <- "green"
color_subset$color[color_subset$party == "PDS"] <- "purple"

shape$color <- color_subset$color

# Map eith all Wahlkreise
png(filename = "map_germany.png", width = 500, height = 700, bg = "#18bc9c", res = 1)
plot(shape, col = "lightgrey", lwd = 2, border="white")
for (i in 1:299){
  plot(shape[shape$WKR_NR == i, ], col = shape$color[i], add = TRUE, lwd = 3, border="white")
}
dev.off()



# Exporting one file each for each Bundesland (small and large)
if(!dir.exists("Bundeslaender_small")) dir.create("Bundeslaender_small")
for (k in unique(shape$LAND_NAME)){
  if(k!="Thüringen" & k!="Baden-Württemberg") png(filename = paste0("Bundeslaender_small/", k, ".png"), width = 500, height = 360)
  if(k=="Thüringen") png(filename = paste0("Bundeslaender_small/", "Thueringen", ".png"), width = 500, height = 360)
  if(k=="Baden-Württemberg") png(filename = paste0("Bundeslaender_small/", "Baden-Wuerttemberg", ".png"), width = 500, height = 360) 
  sel <- shape$LAND_NAME == k
  plot(shape[sel, ], col = "lightgrey", lwd = 2, border="white")
  for (i in shape$WKR_NR[sel]){
    plot(shape[shape$WKR_NR == i, ], col = shape$color[shape$WKR_NR == i], add = TRUE, lwd = 2, border="white")
  }
  dev.off()
}



if(!dir.exists("Bundeslaender_large")) dir.create("Bundeslaender_large")
for (k in unique(shape$LAND_NAME)){
  if(k!="Thüringen" & k!="Baden-Württemberg") png(filename = paste0("Bundeslaender_large/", k, ".png"), width = 1000, height = 700)
  if(k=="Thüringen") png(filename = paste0("Bundeslaender_large/", "Thueringen", ".png"), width = 1000, height = 700)
  if(k=="Baden-Württemberg") png(filename = paste0("Bundeslaender_large/", "Baden-Wuerttemberg", ".png"), width = 1000, height = 700)   
  sel <- shape$LAND_NAME == k
  plot(shape[sel, ], col = "lightgrey", lwd = 4, border="white")
  for (i in shape$WKR_NR[sel]){
    plot(shape[shape$WKR_NR == i, ], col = shape$color[shape$WKR_NR == i], add = TRUE, lwd = 2, border="white")
  }
  dev.off()
}
  
  

# Horizontal bar plot for summary of Direktmandate by parties: CDU/CSU, SPD, Linke, Grüne, Andere

seatsummary <- data.frame(num=c(1,2,3,4,5), partyname=c("CDU/CSU ", "SPD ", "Linke ", "Grüne ", "Andere "), color=c("black", "red", "purple", "green", "lightgrey"), partyseats=(c(
  length(color_subset$party[color_subset$party == "CDU"| color_subset$party == "CSU"]), 
  length(color_subset$party[color_subset$party == "SPD"]), 
  length(color_subset$party[color_subset$party == "PDS"]),
  length(color_subset$party[color_subset$party == "GRU"]),
  0
  )), 
  seats13 = c(236, 58, 4, 1, 0)
  )



# Seat summary

png(filename = "seatsummary.png", width = 1100, height = 1000, bg = "#18bc9c")


ggplot(seatsummary, aes(x=reorder(partyname, -num), y=partyseats, color="white", fill=partyname))+
  geom_bar(stat='identity', width=.5, size=1)+
  coord_flip()+
  theme(axis.line.y=element_line(color="white", size=5), axis.text.x=element_blank(),
        axis.text.y=element_text(size=80, color="white", family="Helvetica Neue", face="bold"), axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_rect(fill = "#18bc9c"))+
        scale_color_manual(values="white")+
        scale_fill_manual(values=c("CDU/CSU "= "black", "SPD " = "red", "Linke " = "purple", "Grüne "="green", "Andere "="lightgrey"))+
        geom_text(nudge_y = ifelse(seatsummary$partyseats > 200, 170, 160), angle = 0, aes(x = partyname, y = partyseats, label = 
                                    paste0(partyseats, " (", ifelse(seatsummary$partyseats >= seatsummary$seats13, "+", ""), partyseats-seats13, ")" )), size=25)+
        scale_y_continuous(limits=c(0, 500))



dev.off.crop()
dev.off()


# Map for each Wahlkreis

if(!dir.exists("Wahlkreise/")) dir.create("Wahlkreise/")
if(!dir.exists("Wahlkreise/maps/")) dir.create("Wahlkreise/maps/")
for (k in unique(shape$LAND_NAME)){
  sel <- shape$LAND_NAME == k
  for (i in shape$WKR_NR[sel]){
    png(filename = paste0("Wahlkreise/maps/", i, ".png"), width = 1000, height = 700)
    
    plot(shape[sel, ], col = "lightgrey", lwd = 4, border="white")
    plot(shape[shape$WKR_NR == i, ], col = shape$color[shape$WKR_NR == i], add = TRUE, lwd = 2, border="white")
    
    dev.off()
  }
}


# Forecast bar plot for each Wahlkreis
btw2017$num <- 0
btw2017$num[btw2017$party == "CDU"] <- 1
  btw2017$num[btw2017$party == "CSU"] <- 1
  btw2017$num[btw2017$party == "SPD"] <- 2
  btw2017$num[btw2017$party == "GRU"] <- 3
  btw2017$num[btw2017$party == "PDS"] <- 4
  btw2017$num[btw2017$party == "FDP"] <- 5
  btw2017$num[btw2017$party == "Andere"] <- 6
  
btw2017$k_nname[btw2017$k_nname == "…zoguz"]   <- "Özoguz"
btw2017$k_nname[btw2017$k_nname == "…tinger"]   <- "Ötinger"
btw2017$k_nname[btw2017$k_nname == "…zkan"]   <- "Özkan"
btw2017$k_vname[btw2017$k_vname == "…zden"]   <- "Özden"
btw2017$k_nname[btw2017$k_nname == "…zdemir"]   <- "Özdemir"
btw2017$k_vname[btw2017$k_vname == "…zcan"]   <- "Özcan"


# Changing NA first and last names to ""
btw2017$k_vname[is.na(btw2017$k_vname)] <- ""
btw2017$k_nname[is.na(btw2017$k_nname)] <- ""

# Making character var for .fitted
btw2017$.fitted <- round(btw2017$.fitted, digits = 1)
btw2017$fitted_char <- as.character(btw2017$.fitted)
btw2017$fitted_char[is.na(btw2017$fitted_char)] <- ""
for (i in 1:length(btw2017$fitted_char)) {
  if (!is.na(btw2017$.fitted[i])) if(btw2017$.fitted[i] != "") if(btw2017$.fitted[i] %% 1 == 0) btw2017$fitted_char[i] <- paste0(btw2017$fitted_char[i], ".0")
  btw2017$fitted_char[i] <- str_replace(btw2017$fitted_char[i], "\\.", ",")
}
btw2017$fitted_char <- paste0(btw2017$fitted_char, "%")
btw2017$fitted_char[btw2017$fitted_char == "%"] <- ""




# Bar plot for each Wahlkreis
if(!dir.exists("Wahlkreise/bar_forecast/")) dir.create("Wahlkreise/bar_forecast/")
for (i in shape$WKR_NR) {
  
  png(filename = paste0("Wahlkreise/bar_forecast/", i, ".png"), width = 1000, height = 500, bg = "white", res = 20) #18bc9c
  
  if(shape$LAND_NAME[i] != "Bayern") {
  one <- ggplot(btw2017[btw2017$wkr_nr2017 == i & btw2017$party != "CSU", ], aes(x=reorder(party, -num), y=.fitted, color="white", fill=party))+
    geom_bar(stat='identity', width=.5, size=5)+
    coord_flip()+
    theme(axis.line.y= element_line(color="black", size=2.5), 
          axis.line.x = element_line(color="black", size=2.5), 
          axis.text.x=element_text(size=80, face = "plain", margin = margin(t = 30, r = 0, b = 0, l = 0)),
          axis.text.y=element_text(size=80, color="black", family="Helvetica Neue", face="plain"), 
          axis.ticks.y=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          plot.margin=unit(c(1.5,1.5,1.5,1.5),"cm"),
          panel.grid.major=element_line(size = 3))+
    scale_color_manual(values="white")+
    scale_fill_manual(values=c("grey", "black", "yellow", "green", "purple", "red"))+
    geom_text(angle = 0, aes(x = party, y = 70, label = btw2017$fitted_char[btw2017$wkr_nr2017 == i & btw2017$party != "CSU"]), size=25, color = "black")+
    scale_y_continuous(labels = function(x){ paste0(x, "%") }, limits=c(0, 75))+
    scale_x_discrete(labels=c(paste0("(Andere) "), 
                              paste0(btw2017$k_vname[btw2017$wkr_nr2017 == i & btw2017$party == "FDP"], " ", btw2017$k_nname[btw2017$wkr_nr2017 == i & btw2017$party == "FDP"], " (FDP) "), 
                              paste0(btw2017$k_vname[btw2017$wkr_nr2017 == i & btw2017$party == "PDS"], " ", btw2017$k_nname[btw2017$wkr_nr2017 == i & btw2017$party == "PDS"], " (Linke) "), 
                              paste0(btw2017$k_vname[btw2017$wkr_nr2017 == i & btw2017$party == "GRU"], " ", btw2017$k_nname[btw2017$wkr_nr2017 == i & btw2017$party == "GRU"], " (Grüne) "), 
                              paste0(btw2017$k_vname[btw2017$wkr_nr2017 == i & btw2017$party == "SPD"], " ", btw2017$k_nname[btw2017$wkr_nr2017 == i & btw2017$party == "SPD"], " (SPD) "), 
                              paste0(btw2017$k_vname[btw2017$wkr_nr2017 == i & btw2017$party == "CDU"], " ", btw2017$k_nname[btw2017$wkr_nr2017 == i & btw2017$party == "CDU"], " (CDU) ")
                              ))+
    geom_errorbar(aes(ymax = predict_pred[[1]][btw2017$wkr_nr2017 == i & btw2017$party != "CSU", 2], ymin = predict_pred[[1]][btw2017$wkr_nr2017 == i & btw2017$party != "CSU", 3]), color = "darkgrey", size = 3, width = .2)
  
  
  
  print(one)
  } else {
    two <- ggplot(btw2017[btw2017$wkr_nr2017 == i & btw2017$party != "CDU", ], aes(x=reorder(party, -num), y=.fitted, color="white", fill=party))+
      geom_bar(stat='identity', width=.5, size=5)+
      coord_flip()+
      theme(axis.line.y= element_line(color="black", size=2.5), 
            axis.line.x = element_line(color="black", size=2.5), 
            axis.text.x=element_text(size=80, face = "plain", margin = margin(t = 30, r = 0, b = 0, l = 0)),
            axis.text.y=element_text(size=80, color="black", family="Helvetica Neue", face="plain"), 
            axis.ticks.y=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            plot.margin=unit(c(1.5,1.5,1.5,1.5),"cm"),
            panel.grid.major=element_line(size = 3))+
      scale_color_manual(values="white")+
      scale_fill_manual(values=c("grey", "black", "yellow", "green", "purple", "red", "grey"))+
      geom_text(angle = 0, aes(x = party, y = 70), label = btw2017$fitted_char[btw2017$wkr_nr2017 == i & btw2017$party != "CDU"], size=25, color = "black")+
      scale_y_continuous(limits=c(0, 75))+
      scale_x_discrete(labels=c(paste0("(Andere) "),
                                paste0(btw2017$k_vname[btw2017$wkr_nr2017 == i & btw2017$party == "FDP"], " ", btw2017$k_nname[btw2017$wkr_nr2017 == i & btw2017$party == "FDP"], " (FDP) "), 
                                
                                paste0(btw2017$k_vname[btw2017$wkr_nr2017 == i & btw2017$party == "PDS"], " ", btw2017$k_nname[btw2017$wkr_nr2017 == i & btw2017$party == "PDS"], " (Linke) "), 
                                
                                paste0(btw2017$k_vname[btw2017$wkr_nr2017 == i & btw2017$party == "GRU"], " ", btw2017$k_nname[btw2017$wkr_nr2017 == i & btw2017$party == "GRU"], " (Grüne) "), 
                                
                                paste0(btw2017$k_vname[btw2017$wkr_nr2017 == i & btw2017$party == "SPD"], " ", btw2017$k_nname[btw2017$wkr_nr2017 == i & btw2017$party == "SPD"], " (SPD) "), 
                                
                                paste0(btw2017$k_vname[btw2017$wkr_nr2017 == i & btw2017$party == "CSU"], " ", btw2017$k_nname[btw2017$wkr_nr2017 == i & btw2017$party == "CSU"], " (CSU) ")
                                ))+
      geom_errorbar(aes(ymax = predict_pred[[1]][btw2017$wkr_nr2017 == i & btw2017$party != "CDU", 2], ymin = predict_pred[[1]][btw2017$wkr_nr2017 == i & btw2017$party != "CDU", 3]), color = "darkgrey", size = 3, width = .2)
    
    print(two)
    
  }

  dev.off()
  
}



# Making character var for erst_l1
btw2017$erst_l1 <- round(btw2017$erst_l1, digits = 1)
btw2017$erst_l1_char <- as.character(btw2017$erst_l1)
btw2017$erst_l1_char[is.na(btw2017$erst_l1_char)] <- ""
for (i in 1:length(btw2017$erst_l1_char)) {
  if (!is.na(btw2017$erst_l1[i])) if(btw2017$erst_l1[i] != "") if(btw2017$erst_l1[i] %% 1 == 0) btw2017$erst_l1_char[i] <- paste0(btw2017$erst_l1_char[i], ".0")
  btw2017$erst_l1_char[i] <- str_replace(btw2017$erst_l1_char[i], "\\.", ",")
}
btw2017$erst_l1_char <- paste0(btw2017$erst_l1_char, "%")
btw2017$erst_l1_char[btw2017$erst_l1_char == "%"] <- ""



# Bar plot for each Wahlkreis for 2013 results
if(!dir.exists("Wahlkreise/bar_13/")) dir.create("Wahlkreise/bar_13/")
for (i in shape$WKR_NR) {
  
  png(filename = paste0("Wahlkreise/bar_13/", i, ".png"), width = 1000, height = 500, bg = "white", res = 20) #18bc9c
  
  if(shape$LAND_NAME[i] != "Bayern") {
    one <- ggplot(btw2017[btw2017$wkr_nr2017 == i & btw2017$party != "CSU", ], aes(x=reorder(party, -num), y=erst_l1, color="white", fill=party))+
      geom_bar(stat='identity', width=.5, size=5)+
      coord_flip()+
      theme(axis.line.y= element_line(color="black", size=2.5), 
            axis.line.x = element_line(color="black", size=2.5), 
            axis.text.x=element_text(size=80, face = "plain", margin = margin(t = 30, r = 0, b = 0, l = 0)),
            axis.text.y=element_text(size=80, color="black", family="Helvetica Neue", face="plain"), 
            axis.ticks.y=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            plot.margin=unit(c(1.5,1.5,1.5,1.5),"cm"),
            panel.grid.major=element_line(size = 3))+
      scale_color_manual(values="white")+
      scale_fill_manual(values=c("grey", "black", "yellow", "green", "purple", "red"))+
      geom_text(angle = 0, aes(x = party, y = 70), label = btw2017$erst_l1_char[btw2017$wkr_nr2017 == i & btw2017$party != "CSU"], size=25, color = "black")+
      scale_y_continuous(labels = function(x){ paste0(x, "%") }, limits=c(0, 75))+
      scale_x_discrete(labels=c("Andere ", "FDP ", "Linke ", "Grüne ", "SPD ", "CDU "))
     
    
    
    print(one)
  } else {
    two <- ggplot(btw2017[btw2017$wkr_nr2017 == i & btw2017$party != "CDU", ], aes(x=reorder(party, -num), y=erst_l1, color="white", fill=party))+
      geom_bar(stat='identity', width=.5, size=5)+
      coord_flip()+
      theme(axis.line.y= element_line(color="black", size=2.5), 
            axis.line.x = element_line(color="black", size=2.5), 
            axis.text.x=element_text(size=80, face = "plain", margin = margin(t = 30, r = 0, b = 0, l = 0)),
            axis.text.y=element_text(size=80, color="black", family="Helvetica Neue", face="plain"), 
            axis.ticks.y=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            plot.margin=unit(c(1.5,1.5,1.5,1.5),"cm"),
            panel.grid.major=element_line(size = 3))+
      scale_color_manual(values="white")+
      scale_fill_manual(values=c("grey", "black", "yellow", "green", "purple", "red", "grey"))+
      geom_text(angle = 0, aes(x = party, y = 70, label = btw2017$erst_l1_char[btw2017$wkr_nr2017 == i & btw2017$party != "CDU"]), size=25, color = "black")+
      scale_y_continuous(limits=c(0, 75))+
      scale_x_discrete(labels=c("Andere ", "FDP ", "Linke ", "Grüne ", "SPD ", "CSU "))
    
    
    print(two)
    
  }
  
  dev.off()
  
}


save(btw2017, file = "btw17_forecast_c.RData")
save(predict_pred, file = "prediction_c.RData")


load("btw17_forecast_c.RData")
load("prediction_c.RData")







