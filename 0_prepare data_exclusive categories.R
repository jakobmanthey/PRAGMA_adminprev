# ==================================================================================================================================================================
# ==================================================================================================================================================================

# PROJECT TITLE:  PRAGMA
# CODE AUTHORS:    JM + CK
# DATE STARTED:   2024/01/25
# DATE LAST MODIFIED:   2024/10/04

# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 0) ESSENTIALS
# ______________________________________________________________________________________________________________________

# clean workspace
rm(list=ls())

# input path


# date
DATE <- "2024-08-19"

# load libraries
library( data.table )
library( ggplot2 )
library( ggthemes )
library( tidyr )
library( stringr )
library( lubridate )

options(scipen = 999)

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 1) LOAD DATA
# ______________________________________________________________________________________________________________________

# GKV Stammdaten
filename <- paste0("data/input/0_pragma_id_GKV with Stammdata_", DATE,".rds")
id.dat <- readRDS(filename)

# Insurance period
filename <- paste0("data/input/1_data_insurance periods_", DATE,".rds")
ins.dat <- readRDS(filename)

# Versicherte
##  AOK
pop.aok.17 <- data.table(read.csv("data/input/population/GRUND_2017.csv"))
pop.aok.17$year <- 2017
pop.aok.18 <- data.table(read.csv("data/input/population/GRUND_2018.csv"))
pop.aok.18$year <- 2018
pop.aok.19 <- data.table(read.csv("data/input/population/GRUND_2019.csv"))
pop.aok.19$year <- 2019
pop.aok.20 <- data.table(read.csv("data/input/population/GRUND_2020.csv"))
pop.aok.20$year <- 2020
pop.aok.21 <- data.table(read.csv("data/input/population/GRUND_2021.csv"))
pop.aok.21$year <- 2021

##  DAK
pop.dak.17 <- data.table(openxlsx::read.xlsx("data/input/population/DAK_Versichertenzahlen Prävalenzschätzung PRAGMA 2017_2022.xlsx",
                                            sheet = "2017", cols = 1:3))
pop.dak.17$year <- 2017
pop.dak.18 <- data.table(openxlsx::read.xlsx("data/input/population/DAK_Versichertenzahlen Prävalenzschätzung PRAGMA 2017_2022.xlsx",
                                             sheet = "2018", cols = 1:3))
pop.dak.18$year <- 2018
pop.dak.19 <- data.table(openxlsx::read.xlsx("data/input/population/DAK_Versichertenzahlen Prävalenzschätzung PRAGMA 2017_2022.xlsx",
                                             sheet = "2019", cols = 1:3))
pop.dak.19$year <- 2019
pop.dak.20 <- data.table(openxlsx::read.xlsx("data/input/population/DAK_Versichertenzahlen Prävalenzschätzung PRAGMA 2017_2022.xlsx",
                                             sheet = "2020", cols = 1:3))
pop.dak.20$year <- 2020
pop.dak.21 <- data.table(openxlsx::read.xlsx("data/input/population/DAK_Versichertenzahlen Prävalenzschätzung PRAGMA 2017_2022.xlsx",
                                             sheet = "2021", cols = 1:3))
pop.dak.21$year <- 2021

# Alkohol Diagnosen
filename <- paste0("data/input/1_data_alcohol diagnoses_", DATE,".rds")
diag.dat <- readRDS(filename)
rm(filename)

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 2) PREPARE POPULATION DATA
# ______________________________________________________________________________________________________________________

# Select those insured between 2017 and 2021 ??? remove
# ..............

years <- 2017:2021

in.dat <- copy(ins.dat)
in.dat[, y2017 := between(years[1],year(date.ins.start),year(date.ins.end))]
in.dat[, y2018 := between(years[2],year(date.ins.start),year(date.ins.end))]
in.dat[, y2019 := between(years[3],year(date.ins.start),year(date.ins.end))]
in.dat[, y2020 := between(years[4],year(date.ins.start),year(date.ins.end))]
in.dat[, y2021 := between(years[5],year(date.ins.start),year(date.ins.end))]

in.dat <- melt(in.dat[,.(gkv,pragmaid,yob,y2017,y2018,y2019,y2020,y2021)], 
               id.vars = c("gkv","pragmaid","yob"), variable.name = "year", value.name = "ins")
in.dat[, year := as.numeric(gsub("y","",year))]

# age check
in.dat[, table(ins)]
in.dat[, ins := ifelse(year-yob>=18,ins,F), by = .(gkv,pragmaid,year)]
in.dat[, table(ins)]
in.dat <- unique(in.dat[ins == T,.(gkv,pragmaid,year)])

id.select <- unique(in.dat$pragmaid)
length(id.select) # 24976

# Sex and age data
# ..............

ses.dat <- id.dat[pragmaid %in% id.select,.(pragmaid,gkv,sex,yob)][!is.na(pragmaid)]
ses.dat[, length(unique(pragmaid))] # 24976

# Select relevant diagnoses
# ..............

alc.diag <- c("F10.0","T51.0","T51.9",
              "F10.1",
              "F10.2",
              "F10.3","F10.4","F10.5","F10.6","F10.7","F10.8","F10.9",
              "E24.4","G31.2","G62.1","G72.1","I42.6",
              "K29.2","K70.0","K70.1","K70.2","K70.3","K70.4","K70.9","K85.2","K85.20","K86.0")

##  not considered:
diag.dat[icd %like% "O35.4|R78.0", table(icd)] 

##  keep relevant diagnoses, settings and define date variable
diag.dat <- diag.dat[icd %in% alc.diag & setting %like% "inpat|outpat" & setting != "outpatient surgery",
                     .(gkv,pragmaid,setting,case.id,icd,icd_type,date.aud = fifelse(setting == "outpatient", date.diag.median,
                                                                            fifelse(setting == "inpatient", date.diag.start, NA)))]
diag.dat[, year := year(date.aud)]
diag.dat <- merge(in.dat,diag.dat,by = c("gkv","pragmaid","year"), all = T)
diag.dat <- diag.dat[!is.na(setting)]
                  
## keep only inpatient = admission/primary/secondary & outpatient = confirmed
diag.dat[, table(setting, icd_type)] # 
diag.dat[setting %like% "inpat|outpat" & setting != "outpatient surgery", 
        prop.table(table(setting, icd_type %like% "admission|primary|secondary|confirmed"),1)] # include > 90% of diagnoses in both settings
diag.dat <- diag.dat[setting %like% "inpat|outpat" & setting != "outpatient surgery" & icd_type %like% "admission|primary|secondary|confirmed"]
nrow(diag.dat) # 312491
length(unique(diag.dat$pragmaid)) # 24192

aud.dat <- copy(diag.dat)

# Population
# ..............

## AOK:
pop.aok <- rbind(pop.aok.17,pop.aok.18,pop.aok.19,pop.aok.20,pop.aok.21)
names(pop.aok) <- c("age","pop","sex","year")
pop.aok[, sex := dplyr::recode(sex, "M" = "male", "W" ="female")]

##  collapse 90+
pop.aok.90 <- pop.aok[age %like% "90|95",.(age = "90+", pop = sum(pop)), by = .(year,sex)]
pop.aok <- rbind(pop.aok[!age %like% "90|95"],
                 pop.aok.90)
pop.aok <- pop.aok[,.(gkv = "aok",year,sex,age,pop)]

# DAK:
pop.dak <- rbind(pop.dak.17,pop.dak.18,pop.dak.19,pop.dak.20,pop.dak.21)
names(pop.dak) <- c("age","male","female","year")
pop.dak <- pop.dak[!(is.na(age) | age == "*jeweils von bis unter")]
pop.dak <- melt(pop.dak, id.vars = c("year","age"), variable.name = "sex", value.name = "pop")
pop.dak[, age := gsub(" bis ","-",age)]
pop.dak[, age := gsub(" und älter","+",age)]
pop.dak[, upper := as.numeric(substr(age,4,5))] # upper age limit should be "bis unter" -> minus 1
pop.dak[!is.na(upper), age := paste0(substr(age,1,3),upper-1)]
pop.dak[, table(age)]
pop.dak$upper <- NULL
pop.dak <- pop.dak[order(year,sex,age)]

# sum pop because they list "Mitglieder" and "Familienversicherte" separately
pop.dak <- pop.dak[, .(pop = sum(pop)), by = .(year,sex,age)]
pop.dak[, sum(pop), by = year] # 2016: 163181

# split youngest age group in DAK according to ratio of "18-19" to "20-24" in AOK:
split.dat <- copy(pop.aok[age == "18-19" | age == "20-24"])
split.dat <- dcast(split.dat, year + sex ~ age, value.var = "pop")
split.dat$splitfactor <- split.dat$`18-19`/split.dat$`20-24`

pop.dak.1819 <- merge(pop.dak[age == "20-24"],
                      split.dat[,.(year,sex,splitfactor)])
pop.dak.1819 <- pop.dak.1819[,.(year,sex,age = "18-19",pop = round(pop*splitfactor,0))]

pop.dak <- rbind(pop.dak,
                 pop.dak.1819)[order(year,sex,age)]
pop.dak[age %like% "18-"]$pop / pop.dak[age %like% "15-"]$pop ## % of 18-19 in 15-19 = between 37 and 45% -> ok
pop.dak <- pop.dak[age != "15-19",.(gkv = "dak",year,sex,age,pop)]
rm(split.dat, pop.dak.1819)

# combine AOK and DAK:
unique(pop.aok$age)[!unique(pop.aok$age) %in% unique(pop.dak$age)] # none
unique(pop.dak$age)[!unique(pop.dak$age) %in% unique(pop.aok$age)] # none

pop.dat <- rbind(pop.aok, pop.dak)
pop.dat[, .N, by = .(gkv,year)] # all gkv-years = 32

pop.dat[year == 2018, sum(pop), by = .(gkv)] # 2018: AOK=249216  // DAK=156494

rm(list = ls()[ls() %like% "pop.aok|pop.dak"])

# get fewer age groups:
pop.dat[,age.group := ifelse(age %like% "^18|^20", "18-24",
                             ifelse(age %like% "^25|^30", "25-34",
                                    ifelse(age %like% "^35|^40", "35-44",
                                           ifelse(age %like% "^45|^50", "45-54",
                                                  ifelse(age %like% "^55|^60", "55-64",
                                                         ifelse(age %like% "^65|^70", "65-74","75+"))))))]
pop.dat[, table(age, age.group, useNA = "always")]

# get sum pop for both GKV
pop.dat <- pop.dat[,.(pop = sum(pop)), by = .(year,sex,age.group)]



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 3) PREPARE EXPOSURE DEFINITION 1: DIAGNOSES
# ______________________________________________________________________________________________________________________

# vector of all relevant diagnoses
# ..............

alc.diag_1 <- c("F10.0","T51.0","T51.9")
alc.diag_2 <- "F10.1"
alc.diag_3 <- "F10.2"
alc.diag_4 <- c("F10.3","F10.4","F10.5","F10.6","F10.7","F10.8","F10.9",
                "E24.4","G31.2","G62.1","G72.1","I42.6",
                "K29.2","K70.0","K70.1","K70.2","K70.3","K70.4","K70.9","K85.2","K85.20","K86.0")



expdat1 <- data.table()

for(y in years){
  
  sub <- unique(aud.dat[year == y,.(pragmaid,gkv,icd)])
  sub$year <- y
  sub$expdim <- "type"
  
  # define exposure levels:
  sub[, exp := ifelse(any(icd %in% alc.diag_4), 4,
                      ifelse(any(icd %in% alc.diag_3), 3,
                             ifelse(any(icd %in% alc.diag_2), 2,
                                    ifelse(any(icd %in% alc.diag_1), 1, NA)))), by = .(pragmaid,gkv)]
  
  sub <- unique(sub[,.(pragmaid,gkv,year,expdim,exp)])
  
  # add
  expdat1 <- rbind(expdat1,sub)
  
}

# check:
select <- expdat1[year == 2019 & exp == 1]$pragmaid
aud.dat[year == 2019 & pragmaid %in% select, table(icd)] # should be only F10.0, T51.0, T51.9
select <- expdat1[year == 2019 & exp == 2]$pragmaid[10]
aud.dat[year == 2019 & pragmaid == select, table(icd)] # should be minimally F10.1 but no higher-level codes
select <- expdat1[year == 2019 & exp == 3]$pragmaid[10]
aud.dat[year == 2019 & pragmaid == select, table(icd)] # should be minimally F10.2 but no higher-level codes
select <- expdat1[year == 2019 & exp == 4]$pragmaid[10]
aud.dat[year == 2019 & pragmaid == select, table(icd)] # should be minimally F10.3-9 etc.
rm(select, sub, y, alc.diag_1, alc.diag_2, alc.diag_3, alc.diag_4)


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 4) PREPARE EXPOSURE DEFINITION 2: SETTING
# ______________________________________________________________________________________________________________________

setting_1 <- "outpatient"
setting_2 <- "inpatient"

expdat2 <- data.table()

for(y in years){
  
  sub <- unique(aud.dat[year == y & icd == "F10.2",.(pragmaid,gkv,setting)])
  sub$year <- y
  sub$expdim <- "setting"
  
  # define exposure levels:
  sub[, exp := ifelse(any(setting %in% setting_2), 2,
                      ifelse(any(setting %in% setting_1), 1, NA)), by = .(pragmaid,gkv)]
  
  sub <- unique(sub[,.(pragmaid,gkv,year,expdim,exp)])
  
  # add
  expdat2 <- rbind(expdat2,sub)
  
}

# check:
select <- expdat2[year == 2019 & exp == 1]$pragmaid#[3]
aud.dat[year == 2019 & pragmaid %in% select & icd == "F10.2", table(setting)] # should be only outpatient
select <- expdat2[year == 2019 & exp == 2]$pragmaid[10]
aud.dat[year == 2019 & pragmaid == select & icd == "F10.2", table(setting)] # should contain inpatient but can also have outpatient

rm(select, sub, y, setting_1, setting_2)

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 5) PREPARE EXPOSURE DEFINITION 3: PATTERN
# ______________________________________________________________________________________________________________________

expdat3 <- data.table()

##  note: do not consider type --> same diag in same case (e.g. inpatient) but different type (e.g. secondary and admission) will count as 1 diag!

for(y in years){
  
  print(y)
  sub <- unique(aud.dat[year == y & icd == "F10.2",.(pragmaid,gkv,case.id,date.aud)])[order(gkv,pragmaid,date.aud)]
  sub$year <- y
  sub$expdim <- "pattern"
  
  # define exposure levels:
  ##  M1D
  sub[, exp_m1d := T]
  
  ##  M2D
  sub[, exp_m2d := .N >= 2, by = .(gkv,pragmaid)]
  
  ##  M2Q
  sub[, date.aud.q := quarter(date.aud, type = "quarter")]
  sub[, exp_m2q := length(unique(date.aud.q)) >= 2, by = .(gkv,pragmaid)]
  # sub[exp_m2d != exp_m2q] # only few differences between these definitions
  
  ##  M2QF
  sub[, date.aud.q := quarter(date.aud, type = "quarter")]
  sub[, diff := c(NA,diff(date.aud.q)), by = .(gkv,pragmaid)]
  sub[, exp_m2qf := any(diff) == 1, by = .(gkv,pragmaid)]
  
  ##  select highest
  sub[, exp := ifelse(any(exp_m2qf == T, na.rm = T), 4,
                      ifelse(any(exp_m2q == T), 3,
                             ifelse(any(exp_m2d == T), 2,
                                    ifelse(any(exp_m1d == T), 1, NA)))), by = .(gkv,pragmaid)]
  
  sub <- unique(sub[,.(pragmaid,gkv,year,expdim,exp)])
  sub[, table(exp)]
  
  # add
  expdat3 <- rbind(expdat3,sub)
  
}

expdat3[, table(year,exp)] # only 1,2,4 --> recode 4 into 3
expdat3[ exp == 4, exp := 3]

# check:
select <- expdat3[year == 2019 & exp == 1]$pragmaid
unique(aud.dat[year == 2019 & pragmaid %in% select & icd == "F10.2",.(pragmaid,gkv,case.id,date.aud)]) # should be only one row per person
anyDuplicated(unique(aud.dat[year == 2019 & pragmaid %in% select & icd == "F10.2",.(pragmaid,gkv,case.id,date.aud)])) # should be 0!

select <- expdat3[year == 2019 & exp == 2]$pragmaid[10]
unique(aud.dat[year == 2019 & pragmaid %in% select & icd == "F10.2",.(pragmaid,gkv,case.id,date.aud)]) # should have >=2 diagnoses (two different dates/two different case.ids) but not in subsequent quarters (will be same quarter)

select <- expdat3[year == 2019 & exp == 3]$pragmaid[10]
unique(aud.dat[year == 2019 & pragmaid %in% select & icd == "F10.2",.(pragmaid,gkv,case.id,date.aud)]) # should have >=2 diagnoses (two different dates/two different case.ids) in subsequent quarters
rm(select, sub, y)

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 6) COMBINE EXPOSURE DATA
# ______________________________________________________________________________________________________________________

expdat <- rbind(expdat1,
                expdat2,
                expdat3)

# number of persons captured in each exposure dimension:
expdat[,length(unique(pragmaid)), by = expdim] # about double by type vs setting/pattern - ok


##  TESTPLOT - to be removed
library(ggforce)
library(ggraph)

pdat <- copy(expdat[expdim == "pattern"])
pdat <- pdat[order(pragmaid, year)]
pdat[, next_exp := shift(exp, type = "lead"), by = pragmaid]
pdat <- pdat[!is.na(next_exp) | !is.na(pragmaid)] 

pdat <- pdat[, .N, by = .(year, exp, next_exp)]
pdat[, next_year := year + 1]
#pdat[, exp := factor(exp)]

ggplot(pdat, aes(x = year, y = exp)) +
  geom_segment(aes(xend = next_year, yend = next_exp, size = N, color = exp),
               arrow = arrow(length = unit(0.2, "inches"), type = "closed")) +
  scale_size_continuous(range = c(0.5, 3), guide = "none") + # Adjust for better visualization
  labs(title = "Category Transitions Over Time",
       x = "Year",
       y = "Category",
       size = "Number of Transitions") +
  theme_minimal() +
  theme(legend.position = "bottom")



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 4) SAFE OUTPUT FILES
# ______________________________________________________________________________________________________________________

saveRDS(pop.dat, paste0("data/output/preprocessed/", Sys.Date(), "_insurance_population.RDS"))
saveRDS(expdat, paste0("data/output/preprocessed/", Sys.Date(), "exposure data_exclusive categories.RDS"))
