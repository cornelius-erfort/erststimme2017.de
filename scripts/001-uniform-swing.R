####################################
### Election Forecasting Project ###
### HU Seminar SoSe 2017         ###
####################################

### Requierements ------------------
source("packages.r")
source("functions.r")



### Data Manipulation --------------

# Import
df <- read.csv2("data/kandidatinnen_90_17_long.csv",
                 sep = ";", stringsAsFactors = FALSE)

# Setting some missing party and candidate inc
df$party_inc[is.na(df$party_inc)] <- 0
df$k_inc[is.na(df$k_inc)] <- 0

# Add k_platz_dum (dummy)
df$k_platz_dum <- NA
df$k_platz_dum[!is.na(df$k_platz)] <- 1
df$k_platz_dum[is.na(df$k_platz)] <- 0
df$k_platz[is.na(df$k_platz)] <- 0
df$k_platz <- as.integer(df$k_platz)


# Generate 'election id'
election_years <- unique(df$year)
election_years_id <- seq_along(election_years)
election_years_df <- data.frame(year = election_years, election_id = election_years_id)
head(election_years_df)

df <- merge(df, election_years_df, by = c("year"), all.x = TRUE)


# Add BTW results
btw <- read.csv2("data/btw_zweit.csv",
                 sep = ";", stringsAsFactors = FALSE)
df <- merge(df, btw, by = c("year", "party"), all.x = TRUE)


# Generate Percentage of Erst- and Zweitsimme
df <- mutate(df, per_erst = erst / glt_erst, per_zweit = zweit / glt_zweit)
df %>% mutate(per_erst = round(per_erst, 3) * 100) -> df
df %>% mutate(per_zweit = round(per_zweit, 3) * 100) -> df


# Generate lag Variables
df$btw_zweit <- as.numeric(df$btw_zweit)
df <- group_by(df, wkr_nr2017, party) %>% arrange(year) %>% 
  mutate(btw_l1 = lag(btw_zweit, 1), 
         btw_l2 = lag(btw_zweit, 2),
         btw_l3 = lag(btw_zweit, 3)
         )

df <- group_by(df, wkr_nr2017, party) %>% arrange(year) %>% 
  mutate(erst_l1 = lag(per_erst, 1)
  )

df <- arrange(df, wkr_nr2017, party, year)


# Save
save(df, file = "data/uniform_data_complete.RData")


# Order Data
df  %>%  select(wkr_nr2017, year, election_id, party, k_idname,
                per_erst, erst_l1, k_winner,
                wkr_name, wkr_new, wkr_change, wkr_nr2017, bula,
                bula_ost,
                k_vname, k_nname, k_inc, k_platz, k_platz_dum,
                party_inc, per_zweit, btw_zweit,
                btw_l1, btw_l2, btw_l3) -> btw_data
                
btw_data <- arrange(btw_data, year, wkr_nr2017, party)


#  Generate Swing Variable
btw_data <- mutate(btw_data, btw_swing = btw_zweit - btw_l1)


# Split Sample
btw_data %>% filter(between(year, 2005, 2013)) -> btw0513
btw_data %>% filter(year == 2017) -> btw2017


# save
save(btw0513, file = "data/btw0513.RData")


# Restrict sample to the two most competitive candidates
btw0513_2k <- group_by(btw0513, year, wkr_nr2017) %>% arrange(desc(per_erst)) %>% mutate(rank = seq_along(per_erst))

btw0513_2k <- filter(btw0513_2k, rank <= 2)
btw0513_2k <- arrange(btw0513_2k, year, wkr_nr2017, party)


# save
save(btw0513_2k, file = "data/btw0513_2k.RData")



### Uniform Swing Model ------------------

# Model
model_out <- lm(per_erst ~ erst_l1 + btw_swing + party_inc + k_inc + k_platz + k_platz_dum - 1, data = btw0513)
summary(model_out)
# plot(model_out)

# >Comment: 
# Adj. R-squared of simple uniform swing model is 0.9745. 
# How much room for improvement does this leave for our
# gtrends approach?
# <


# Evaluate Fit
model_out_fit <- augment(model_out)
model_out_fit$party <- btw0513$party[as.numeric(model_out_fit$.rownames)]
model_out_fit$year <- btw0513$year[as.numeric(model_out_fit$.rownames)]
model_out_fit$wkr <- btw0513$wkr_nr2017[as.numeric(model_out_fit$.rownames)]


# MAE 
mean(abs(model_out_fit$.resid))
group_by(model_out_fit, year, wkr) %>% summarize(mae = mean(abs(.resid)))
group_by(model_out_fit, year, party) %>% summarize(mae = mean(abs(.resid)))


# Plot
plot(model_out_fit$.fitted, model_out_fit$per_erst, cex = .5, pch = 20)
text(model_out_fit$.fitted, model_out_fit$per_erst, paste0(model_out_fit$party, str_sub(as.character(model_out_fit$year), -2, -1)), pos = 3, offset = .15, cex = .6)
grid()
abline(0, 1)





# >Working Points: 

# Extend simple uniform swing model (i.e including incumbency, pioneer status etc.)
# Run out-of-sample checks
# Build graphs
# Identify subsample of most competitive constituencies



### 2017 Forecast ------------------

# Forecast from http://zweitstimme.org , accessed 16.08.2017
forecast_2017 <- c(37.5, 37.5, 25.6, 8.0, 7.6, 8.8, 12.5)
party <- c("CDU", "CSU", "SPD", "FDP", "GRU", "PDS", "Andere")
year <- "2017"

forecast <- data.frame(party = party,
                       btw17_zweitstimme_org = forecast_2017,
                       year = year)

# per_erst = pastvoteshare + national-level vote swing
btw2017 <- merge(btw2017, forecast, by = c("year", "party"), all.x = TRUE)
btw2017 %>% mutate(btw_swing = btw17_zweitstimme_org - btw_l1) -> btw2017
# btw2017 %>% mutate(per_erst = erst_l1 + btw_swing) -> btw2017


model_out <- lm(per_erst ~ erst_l1 + btw_swing + party_inc + k_inc + k_platz + k_platz_dum - 1, data = btw0513)
summary(model_out)

btw2017 <- augment(model_out, newdata = btw2017)
predict_conf <- predict(model_out, btw2017, se.fit = TRUE, interval = "confidence")
predict_pred <- predict(model_out, btw2017, se.fit = TRUE, interval = "prediction", level = .83)

predict_pred[[1]][(predict_pred[[1]][, 2] < 0), 2] <- 0
predict_pred$se_char <- round((predict_pred[[1]][, 3] - predict_pred[[1]][, 1]), digits = 1)


# Where's the difference between 'conf' and 'pred'?
# conf is just uncertainty of estimated coefficients (do the coefficients really lie there, or could it be the result of random noise), pred also includes the noise/variance of the observations (will a future value lie there, given the uncertainty of the coefficients and the variance/noise in the observed data)


# Sort Data
btw2017 <- arrange(btw2017, year, wkr_nr2017, party)
btw2017 %>% 
  mutate(.fitted = round(.fitted, 1),
         .se.fit = round(.se.fit, 2)) -> btw2017

# Mark winning candidate
btw2017 <- group_by(btw2017, year, wkr_nr2017) %>% arrange(desc(.fitted)) %>% mutate(rank = seq_along(.fitted))

btw2017 <- arrange(btw2017, year, wkr_nr2017, party)


# Add party "Andere"
names(btw2017)
# party = "Andere"
# Method: Duplicate SPD rows, then change vars accordingly
# Blank colums 5:8, 14:26, 29
# .fitted = (100 - (.fitted of all other parties))


btw2017$dupl <- 1
btw2017$dupl[btw2017$party == "SPD"] <- 2
btw2017.expanded <- expandRows(btw2017, "dupl")

btw2017.expanded <- group_by(btw2017.expanded, year, wkr_nr2017, party) %>% mutate(oth = seq_along(party))

btw2017.expanded[btw2017.expanded$oth == 2, c(5:8, 14:29)] <- NA
btw2017.expanded$party[btw2017.expanded$oth == 2] <- "Andere"
btw2017.expanded$rank[is.na(btw2017.expanded$rank)] <- 99


for (i in 1:length(btw2017.expanded$oth)) {
  
  if(btw2017.expanded$oth[i] == 2) btw2017.expanded$.fitted[i] <- (100 - sum(btw2017.expanded$.fitted[btw2017.expanded$wkr_nr2017 == btw2017.expanded$wkr_nr2017[i]], na.rm = TRUE))
  if(btw2017.expanded$oth[i] == 2) btw2017.expanded$erst_l1[i] <- (100 - sum(btw2017.expanded$erst_l1[btw2017.expanded$wkr_nr2017 == btw2017.expanded$wkr_nr2017[i]], na.rm = TRUE))  
  
}


btw2017 <- btw2017.expanded

# Add first name

btw2017$k_vname[btw2017$k_idname == "kretschmer_michael"] <- "Michael"

# Save forecast Data as btw17_forecast
save(btw2017, file = "data/btw17_forecast.RData")



# End
