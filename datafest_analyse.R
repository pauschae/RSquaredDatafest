library(rjags)
library(dplyr)
library(lme4)
library(MCMCpack)
library(superdiag)

#source("https://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")
#biocLite("graph")

#library(memisc)
#library(Hmisc)
#library(stringr)
#library(rvest)

## ---- load data ----
#library(data.table)
# adwords <- fread("./data and codebook/approved_adwords_v3.csv")
# data_purchase <- fread("./data and codebook/approved_data_purchase-v5.csv")
# ga_data <- fread("./data and codebook/approved_ga_data_v2.csv")
# 
# datafest_db <- src_sqlite("datafest.db", create = TRUE)
# copy_to(datafest_db, adwords, temporary = FALSE)
# copy_to(datafest_db, data_purchase, temporary = FALSE)
# copy_to(datafest_db, ga_data, temporary = FALSE)

## ---- connect to database ----
datafest_db <- src_sqlite("datafest.db")
src_tbls(datafest_db)

## ---- select data ----
unique_venue_states <- tbl(datafest_db, sql("SELECT DISTINCT venue_state FROM data_purchase")) %>% 
  collect() %>% 
  unlist(., use.names = FALSE)

exclude_states <- c("SASKATCHEWAN", 
                    "ALBERTA", 
                    "ONTARIO", 
                    "QUEBEC", 
                    "BRITISH COLUMBIA", 
                    "MANITOBA", 
                    "PRINCE EDWARD ISLAND")

keep_states <- unique_venue_states[unique_venue_states %in% exclude_states == FALSE]

## ---- event data subset ----
# get event_data
event_data <- tbl(datafest_db, sql("SELECT * FROM data_purchase")) %>% 
  filter(., la_valid_tkt_event_flg == "Y ", major_cat_name == "CONCERTS") %>% 
  collect()

event_data_subset <- event_data[event_data$venue_state %in% keep_states, ]

unique(event_data_subset$venue_postal_cd_sgmt_1)

## ---- zip codes ----
zip_codes <- unique(event_data_subset$venue_postal_cd_sgmt_1) %>% sort
save(zip_codes, file = "zip_codes.RData")

# demographic variables
# almost no data

## ---- merge concerts and artists ----
# load data
# load("dfartists.RData")
# dfartists$obs <- c(1:nrow(dfartists))
# #write.csv(dfartists, file = "dfartists.csv")
# artists_rename <- read.csv("dfartists_encoding.csv", sep = ";")
# 
# dfartists_neu <- merge(dfartists, artists_rename, by = c("obs"))
# dfartists_neu <- dfartists_neu[, -4]
# names(dfartists_neu)[3] <- "artists"
# 
# save(dfartists_neu, file = "dfartists_neu.RData")
# 
# rm(artists_rename)
# rm(dfartists)
# 
# unique(event_data_subset$primary_act_name)

unique(event_data_subset$minor_cat_name) %>% length
table(event_data_subset$gndr_cd, useNA = "always")

concerts <- event_data_subset %>% select(., event_name,
                                         primary_act_name,
                                         secondary_act_name,
                                         venue_city,
                                         venue_state,
                                         venue_postal_cd_sgmt_1,
                                         minor_cat_name,
                                         tickets_purchased_qty)

save(concerts, file = "concerts.RData")

## ---- data analysis ----
load("DataForAnalysis.RData")
daten <- datAggSTCD
daten$D.VotePrct <- daten$D.VotePrct/100

daten$country_more_25 <- 0
daten$country_more_25[daten$Country >= 0.25] <- 1

daten$africanamerican_more_25 <- 0
daten$africanamerican_more_25[daten$AfricanAmerican >= 0.25] <- 1

## ---- exploratory models ----
# lm(D.VotePrct ~ Metal + White + Black + Asian + Hispanic, data = daten) %>% summary
#
# lm(D.VotePrct ~ Latin + White + Black + Asian +  Hispanic,  data = daten) %>% summary
# 
# lm(D.VotePrct ~ Folk + White + Black + Asian + Hispanic, data = daten) %>% summary
# 
# lm(D.VotePrct ~ Country + White + Black + Asian + Hispanic, data = daten) %>% summary
# 
# lm(D.VotePrct ~ Pop + White + Black + Asian + Hispanic, data = daten) %>% summary
# 
# lm(D.VotePrct ~ Classical + White + Black + Asian + Hispanic, data = daten) %>% summary
# 
# lm(D.VotePrct ~ BluesJazz + White + Black + Asian + Hispanic, data = daten) %>% summary
# 
# lm(D.VotePrct ~ AfricanAmerican + White + Black + Asian + Hispanic, data = daten) %>% summary
# 
# lm(D.VotePrct ~ Rock + White + Black + Asian + Hispanic, data = daten) %>% summary
# 
# lm(D.VotePrct ~ Comedy + White + Black + Asian + Hispanic, data = daten) %>% summary
# 
# lm(D.VotePrct ~ ELectronic + White + Black + Asian + Hispanic, data = daten) %>% summary
# 
# lm(D.VotePrct ~ Comedy + White + Black + Asian + Hispanic, data = daten) %>% summary

## ---- final models ----
# ols
# genre: country
lm(D.VotePrct ~ 
     Country, data = daten) %>% summary

lm(D.VotePrct ~ 
     Country + 
     White, data = daten) %>% summary

lm(D.VotePrct ~ 
     Country + 
     White + 
     Black + 
     Asian + 
     Hispanic, data = daten) %>% summary

# tobit models
censReg(D.VotePrct ~ 
          Country + 
          White + 
          Black + 
          Asian + 
          Hispanic, data = daten, left = 0, right = 1) %>% summary

# add interaction
lm(D.VotePrct ~ 
     Country * country_more_25 + 
     White + 
     Black + 
     Asian + 
     Hispanic, data = daten) %>% summary

# genre: "african-american"
lm(D.VotePrct ~ 
     AfricanAmerican + 
     White + 
     Black + 
     Asian + 
     Hispanic, data = daten) %>% summary

censReg(D.VotePrct ~ 
          AfricanAmerican + 
          White + 
          Black + 
          Asian + 
          Hispanic, data = daten, left = 0, right = 1) %>% summary

lm(D.VotePrct ~ 
     AfricanAmerican * africanamerican_more_25 + 
     White + 
     Black + 
     Asian + 
     Hispanic, data = daten) %>% summary

# check correlations between music genres
names(daten)
cor(daten[, c(3:13)]*daten$totalSales) %>% round(., 2)
cor(daten[, c(3:13)]) %>% round(., 2)

# Bayesian models
mcmc_1 <- MCMCregress(D.VotePrct ~ 
                        Country + 
                        White + 
                        Black + 
                        Asian + 
                        Hispanic, data = daten, burnin = 10000, mcmc = 10000)

mcmc_2 <- MCMCregress(D.VotePrct ~ 
                        AfricanAmerican + 
                        White + 
                        Black + 
                        Asian + 
                        Hispanic, data = daten, burnin = 10000, mcmc = 10000)

# diagnostics
superdiag(mcmc_1, burnin = 5000)
superdiag(mcmc_2, burnin = 5000)

# results
summary(mcmc_1)$statistics %>% round(., 3)
summary(mcmc_2)$statistics %>% round(., 3)

# ---- plot some quantities of interest ----
# marginal effect
betasim_1 <- mcmc_1[, 1:6]
X_1 <- cbind(1, seq(0, 1, length.out = 100),
             median(daten$White, na.rm = TRUE), 
             median(daten$Black, na.rm = TRUE),
             median(daten$Asian, na.rm = TRUE), 
             median(daten$Hispanic, na.rm = TRUE))

mu <- X_1 %*% t(betasim_1)

plot(density(mu[1,] - mu[100, ]))

x <- seq(0, 1, length.out = 100)
q <- t(apply(mu, 1, quantile, c(0.05, 0.5, 0.95)))

plot(x, q[, 2], type = "l", 
     xlim = c(0, 1),
     ylim = c(0, 1))
lines(x, q[, 1], lty = 2)
lines(x, q[, 3], lty = 2)
