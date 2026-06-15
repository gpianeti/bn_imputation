library(here)
library(tidyverse)
library(forcats)
library(bnlearn)
library(Rgraphviz)
library(gRbase)
library(randomForest)
library(BART)
library(foreach)
library(doParallel)
library(tidyr)

rm(list = ls())
# set the scenario to create the proper dataset
# scenario <- 0 # main analysis
# scenario <- 1 # complete cases only
# scenario <- 2 # mean/mode imputation
# scenario <- 3 # partial data
# scenario <- 4 # slope data
scenario <- 0


#####################################################
### DATA LOADING AND CLEANING #######################
#####################################################

# load the data
hd_cl <- read.csv(here("data/processed.cleveland.data"), header = F)
hd_hu <- read.csv(here("data/processed.hungarian.data"), header = F)
hd_ch <- read.csv(here("data/processed.switzerland.data"), header = F)
hd_va <- read.csv(here("data/processed.va.data"), header = F)

# add location variable for each dataset
hd_ch$location = "ch"
hd_cl$location = "cl"
hd_hu$location = "hu"
hd_va$location = "va"

# combine the four locations into one dataset
hd = rbind(hd_cl, hd_ch, hd_hu, hd_va)
rm(hd_ch, hd_cl, hd_hu, hd_va)

# add column names
colnames(hd) = c(
  "age",
  "sex",
  "cp",
  "trestbps",
  "chol",
  "fbs",
  "restecg",
  "thalach",
  "exang",
  "oldpeak",
  "slope",
  "ca",
  "thal",
  "num",
  "location"
)

# set "?" and inconsistencies to NA
hd[hd == "?"] = NA
hd$chol[hd$chol == 0] <- NA
hd$trestbps[hd$trestbps == 0] <- NA

# convert format 
hd$sex <- as.factor(hd$sex)
hd$cp <- as.factor(hd$cp)
hd$trestbps <- as.numeric(hd$trestbps)
hd$chol <- as.numeric(hd$chol)
hd$fbs <- as.factor(hd$fbs)
hd$restecg <- as.factor(hd$restecg)
hd$thalach <- as.numeric(hd$thalach)
hd$exang <- as.factor(hd$exang)
hd$oldpeak <- as.numeric(hd$oldpeak)
hd$slope <- as.factor(hd$slope)
hd$ca <- as.factor(hd$ca)
hd$thal <- as.factor(as.integer(hd$thal))
levels(hd$thal) <- c("normal", "fixed", "revers")
hd$num <- as.factor(hd$num)
hd$dis = as.factor(ifelse(as.integer(as.character(hd$num)) > 0, 1, 0))
hd$location = as.factor(hd$location)

hd <- if (scenario == 0 | scenario == 2) {
  hd %>% mutate(ca = fct_collapse(ca,
                                  `0` = c("0", "0.0"),
                                  `1` = c("1", "1.0"),
                                  `2` = c("2", "2.0"),
                                  `3` = c("3.0"))) %>% select(-c("num"))
} else if (scenario == 1) {
  hd %>% mutate(ca = fct_collapse(ca,
                                  `0` = c("0", "0.0"),
                                  `1` = c("1", "1.0"),
                                  `2` = c("2", "2.0"),
                                  `3` = c("3.0"))) %>% select(-c("num")) %>%
      na.omit()
} else if (scenario == 3) {
  hd %>% select(-c("num", "thal", "ca", "slope"))
} else if (scenario == 4) {
  hd %>% select(-c("num", "thal", "ca"))
}



