###############################################
# Data Preparation
###############################################
rm(list=ls())

# Load libraries
library(haven)
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(did)

# Set working directory
setwd("~/OneDrive - The Pennsylvania State University/MA Thesis/Datasets")

# Load datasets
ecav <- read_excel("ECAV datatset_Version 1.2.xls")
vdem <- read_csv("V-Dem-CY-Full+Others-v12.csv")
qog <- read_csv("qog_std_ts_jan23.csv")

# Cleaning year variables
ecav.c <- ecav %>%
  mutate(year = format(as.Date(Date, format="%Y/%m/%d"),"%Y"), # Extract year from date variable
         year = as.numeric(year),
         election_year = format(as.Date(Electiondate, format="%Y/%m/%d"),"%Y"),
         election_year = as.numeric(election_year)) %>%
  dplyr::select(country, year, election_year, EventViolence)

# Selecting necessary variables from vdem: regime, electoral system, party competition, monitoring, ethnic inclusion (2)
vdem.c <- vdem %>%
  dplyr::select(year, country_name, e_polity2, v2elloelsy, e_polcomp, v2elwestmon, v2elrstrct, v2pepwrsoc)

# Selecting necessary variables from qog: gdppc
qog.c <- qog %>%
  dplyr::select(cname, year, wdi_gdpcapcur)


# Joining datasets
df <- left_join(ecav.c, vdem.c, 
                by=c('country'='country_name', "year" = "year"))

df <- left_join(df, qog.c, by=c('country'='cname', "year" = "year"))

# Recoding missings 
df.c <- df %>%
  mutate(regime_type = case_when(
    e_polity2 < -10 ~ NA,
    e_polity2 >= -10 & e_polity2 <= 10 ~ e_polity2
  )) %>%
  na.omit()

colnames(df.c) <- c("country", "yr", "elect_yr", "elect_vio", "polity2", "elect_sys",
                    "party_comp", "elect_monitor", "cand_restrict", "social_power", "gdppc", "regime_type")

m2 <- plm(elect_vio ~ polity2 + elect_sys + party_comp + elect_monitor + cand_restrict + 
      social_power + log(gdppc) , data=df.c, model = "within")
summary(m2)

# social power, candidate restriction, electoral system

write.csv(df.c, "~/OneDrive - The Pennsylvania State University/PLSC 597/Final Paper/dalton_df.csv", row.names=FALSE)

###############################################
# Accuracy Scores Table (for Overleaf)
###############################################

## Part I
models <- c("L2 Logit", "SVM", "Random Forest")
mse <- c(0.25698, 0.25698, 0.24651)
train_acc <- c(0.75131, 0.75112, 0.74944)
test_acc <- c(0.74302, 0.74302, 0.75349)

scores <- cbind(models, mse, train_acc, test_acc)
scores_out <- knitr::kable(scores, booktabs=T, caption="Accuracy Scores", format='latex',
                           col.names = c("Model", "MSE", "Training Acc.", "Test Acc.")) %>% 
  kableExtra::row_spec(row = 0, bold = TRUE)

readr::write_file(scores_out, "~/Dropbox/Apps/Overleaf/Machine Learning Final/Figures/scores_out.txt")

## Part II
models <- c("Logit", "Random Forest")
train_acc <- c(0.97591, 0.97428)
test_acc <- c(0.97570, 0.97494)

scores2 <- cbind(models, train_acc, test_acc)
scores_out2 <- knitr::kable(scores2, booktabs=T, caption="Accuracy Scores", format='latex',
                           col.names = c("Model", "Training Acc.", "Test Acc.")) %>% 
  kableExtra::row_spec(row = 0, bold = TRUE)

readr::write_file(scores_out2, "~/Dropbox/Apps/Overleaf/Machine Learning Final/Figures/scores_out2.txt")

