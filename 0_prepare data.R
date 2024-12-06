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

# Employment period
filename <- paste0("data/input/1_data_employment periods_", DATE,".rds")
emp.dat <- readRDS(filename)

# Alcohol diagnoses
filename <- paste0("data/input/1_data_alcohol diagnoses_", DATE,".rds")
alc.diag.dat <- readRDS(filename)
rm(filename)

# All diagnoses
filename <- paste0("data/input/1_data_all diagnoses_",DATE,".rds")
diag.dat <- readRDS(filename)
rm(filename)

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



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 2) PREPARE DIAGNOSES DATA
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

length(unique(in.dat$pragmaid)) # 24976

# Select relevant diagnoses
# ..............

alc.diag_0 <- c("F10.0","T51.0","T51.9",
                "F10.1",
                "F10.2",
                "F10.3","F10.4","F10.5","F10.6","F10.7","F10.8","F10.9",
                "E24.4","G31.2","G62.1","G72.1","I42.6",
                "K29.2","K70.0","K70.1","K70.2","K70.3","K70.4","K70.9","K85.2","K85.20","K86.0")

##  not considered:
alc.diag.dat[icd %like% "O35.4|R78.0", table(icd)]

##  keep relevant diagnoses, settings and define date variable
alc.diag.dat <- alc.diag.dat[icd %in% alc.diag_0 & setting %like% "inpat|outpat" & setting != "outpatient surgery",
                     .(gkv,pragmaid,setting,case.id,icd,icd_type,date.aud = fifelse(setting == "outpatient", date.diag.median,
                                                                            fifelse(setting == "inpatient", date.diag.start, NA)))]
alc.diag.dat[, year := year(date.aud)]
alc.diag.dat <- merge(in.dat,alc.diag.dat,by = c("gkv","pragmaid","year"), all.x = T)
alc.diag.dat <- alc.diag.dat[!is.na(setting)]
                  
## keep only inpatient = admission/primary/secondary & outpatient = confirmed
alc.diag.dat[, table(setting, icd_type)] # 
alc.diag.dat[setting %like% "inpat|outpat" & setting != "outpatient surgery", 
        prop.table(table(setting, icd_type %like% "admission|primary|secondary|confirmed"),1)] # include > 90% of diagnoses in both settings
alc.diag.dat <- alc.diag.dat[setting %like% "inpat|outpat" & setting != "outpatient surgery" & icd_type %like% "admission|primary|secondary|confirmed"]

##  keep relevant years
alc.diag.dat <- alc.diag.dat[year %in% years]

##  remove people without ID
alc.diag.dat[is.na(pragmaid)] # 0 persons
#alc.diag.dat <- alc.diag.dat[!is.na(pragmaid)]

nrow(alc.diag.dat) # 256947
length(unique(alc.diag.dat$pragmaid)) # 21954

aud.dat <- copy(alc.diag.dat)

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 3) PREPARE EXPOSURE DEFINITION 1: DIAGNOSES vs. SETTING
# ______________________________________________________________________________________________________________________

# vector of all relevant diagnoses
# ..............

alc.diag_1 <- c("F10.0","T51.0","T51.9")
alc.diag_2 <- "F10.1"
alc.diag_3 <- "F10.2"
alc.diag_4 <- c("F10.3","F10.4")
alc.diag_5 <- c("F10.5","F10.6","F10.7","F10.8","F10.9",
                "E24.4","G31.2","G62.1","G72.1","I42.6",
                "K29.2","K70.0","K70.1","K70.2","K70.3","K70.4","K70.9","K85.2","K85.20","K86.0")

setting_0 <- "any setting"
setting_1 <- "outpatient"
setting_2 <- "inpatient"
setting_3 <- "inpatient&outpatient"

expdat1 <- data.table()
y <- 2017

for(y in years){
  
  print(y)
  sub <- unique(aud.dat[year == y,.(pragmaid,gkv,setting,icd)])
  
  # define diag type levels:
  subt0 <- sub[icd %in% alc.diag_0, .(pragmaid,gkv,diag = 0,setting)]
  subt1 <- sub[icd %in% alc.diag_1, .(pragmaid,gkv,diag = 1,setting)]
  subt2 <- sub[icd %in% alc.diag_2, .(pragmaid,gkv,diag = 2,setting)]
  subt3 <- sub[icd %in% alc.diag_3, .(pragmaid,gkv,diag = 3,setting)]
  subt4 <- sub[icd %in% alc.diag_4, .(pragmaid,gkv,diag = 4,setting)]
  subt5 <- sub[icd %in% alc.diag_5, .(pragmaid,gkv,diag = 5,setting)]
  rm(sub)
  
  # within each diag type: determine setting:
  ##  type 0
  subt0s0 <- subt0[,.(pragmaid,gkv,diag,setting = 0)]
  subt0s1 <- subt0[setting %in% setting_1,.(pragmaid,gkv,diag,setting = 1)]
  subt0s2 <- subt0[setting %in% setting_2,.(pragmaid,gkv,diag,setting = 2)]
  subt0s3 <- unique(subt0[,.(temp = any(setting %in% setting_1) & any(setting %in% setting_2)), by = .(pragmaid,gkv,diag)])
  subt0s3 <- subt0s3[temp == T,.(pragmaid,gkv,diag,setting = 3)]
  rm(subt0)
  
  ##  type 1
  subt1s0 <- subt1[,.(pragmaid,gkv,diag,setting = 0)]
  subt1s1 <- subt1[setting %in% setting_1,.(pragmaid,gkv,diag,setting = 1)]
  subt1s2 <- subt1[setting %in% setting_2,.(pragmaid,gkv,diag,setting = 2)]
  subt1s3 <- unique(subt1[,.(temp = any(setting %in% setting_1) & any(setting %in% setting_2)), by = .(pragmaid,gkv,diag)])
  subt1s3 <- subt1s3[temp == T,.(pragmaid,gkv,diag,setting = 3)]
  rm(subt1)
  
  ##  type 2
  subt2s0 <- subt2[,.(pragmaid,gkv,diag,setting = 0)]
  subt2s1 <- subt2[setting %in% setting_1,.(pragmaid,gkv,diag,setting = 1)]
  subt2s2 <- subt2[setting %in% setting_2,.(pragmaid,gkv,diag,setting = 2)]
  subt2s3 <- unique(subt2[,.(temp = any(setting %in% setting_1) & any(setting %in% setting_2)), by = .(pragmaid,gkv,diag)])
  subt2s3 <- subt2s3[temp == T,.(pragmaid,gkv,diag,setting = 3)]
  rm(subt2)
  
  ##  type 3
  subt3s0 <- subt3[,.(pragmaid,gkv,diag,setting = 0)]
  subt3s1 <- subt3[setting %in% setting_1,.(pragmaid,gkv,diag,setting = 1)]
  subt3s2 <- subt3[setting %in% setting_2,.(pragmaid,gkv,diag,setting = 2)]
  subt3s3 <- unique(subt3[,.(temp = any(setting %in% setting_1) & any(setting %in% setting_2)), by = .(pragmaid,gkv,diag)])
  subt3s3 <- subt3s3[temp == T,.(pragmaid,gkv,diag,setting = 3)]
  rm(subt3)
  
  ##  type 4
  subt4s0 <- subt4[,.(pragmaid,gkv,diag,setting = 0)]
  subt4s1 <- subt4[setting %in% setting_1,.(pragmaid,gkv,diag,setting = 1)]
  subt4s2 <- subt4[setting %in% setting_2,.(pragmaid,gkv,diag,setting = 2)]
  subt4s3 <- unique(subt4[,.(temp = any(setting %in% setting_1) & any(setting %in% setting_2)), by = .(pragmaid,gkv,diag)])
  subt4s3 <- subt4s3[temp == T,.(pragmaid,gkv,diag,setting = 3)]
  rm(subt4)
  
  ##  type 5
  subt5s0 <- subt5[,.(pragmaid,gkv,diag,setting = 0)]
  subt5s1 <- subt5[setting %in% setting_1,.(pragmaid,gkv,diag,setting = 1)]
  subt5s2 <- subt5[setting %in% setting_2,.(pragmaid,gkv,diag,setting = 2)]
  subt5s3 <- unique(subt5[,.(temp = any(setting %in% setting_1) & any(setting %in% setting_2)), by = .(pragmaid,gkv,diag)])
  subt5s3 <- subt5s3[temp == T,.(pragmaid,gkv,diag,setting = 3)]
  rm(subt5)
  
  # combine
  add <- rbind(subt0s0,subt0s1,subt0s2,subt0s3,
               subt1s0,subt1s1,subt1s2,subt1s3,
               subt2s0,subt2s1,subt2s2,subt2s3,
               subt3s0,subt3s1,subt3s2,subt3s3,
               subt4s0,subt4s1,subt4s2,subt4s3,
               subt5s0,subt5s1,subt5s2,subt5s3)
  add$year <- y
  
  # remove duplicates (multiple settings per diag type):
  add <- unique(add)
  
  # add
  expdat1 <- rbind(expdat1,add)
  
  # remove
  rm(subt0s0,subt0s1,subt0s2,subt0s3,
     subt1s0,subt1s1,subt1s2,subt1s3,
     subt2s0,subt2s1,subt2s2,subt2s3,
     subt3s0,subt3s1,subt3s2,subt3s3,
     subt4s0,subt4s1,subt4s2,subt4s3,
     subt5s0,subt5s1,subt5s2,subt5s3)
  
}

expdat1[,table(diag,setting)] # should have all combinations 0:5 * 0:3
nrow(expdat1) # 315526

# check:
length(unique(expdat1$pragmaid)) # 21954
length(unique(expdat1[diag == 0 & setting == 0]$pragmaid)) # 21954
length(unique(aud.dat$pragmaid)) # 21954 --> all persons included

select <- expdat1[year == 2019 & diag == 2 & setting == 3]$pragmaid[2]
aud.dat[year == 2019 & pragmaid %in% select, table(icd)] # should have F10.1
aud.dat[year == 2019 & pragmaid %in% select, table(icd,setting)] # should have both inpatient and outpatient for F10.1

select <- expdat1[year == 2020 & diag == 4 & setting == 2]$pragmaid[10]
aud.dat[year == 2020 & pragmaid %in% select, table(icd)] # should have F10.3
aud.dat[year == 2020 & pragmaid %in% select, table(icd,setting)] # should have inpatient for F10.3

select0 <- expdat1[year == 2021 & diag == 0 & setting == 3]$pragmaid
select1 <- expdat1[year == 2021 & diag == 1 & setting == 3]$pragmaid
select2 <- expdat1[year == 2021 & diag == 2 & setting == 3]$pragmaid
select3 <- expdat1[year == 2021 & diag == 3 & setting == 3]$pragmaid
select4 <- expdat1[year == 2021 & diag == 4 & setting == 3]$pragmaid
select5 <- expdat1[year == 2021 & diag == 5 & setting == 3]$pragmaid

select0[!select0 %in% c(select1,select2,select3,select4,select5)]
aud.dat[year == 2021 & pragmaid == "zyZdblYtRu"]
expdat1[year == 2021 & pragmaid == "zyZdblYtRu"]

# factor
expdat1$diag <- factor(expdat1$diag, levels = c(0:5))
expdat1$setting <- factor(expdat1$setting, levels = c(0:3))

rm(select, y,
   select0,select1,select2,select3,select4,select5)

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 5) PREPARE EXPOSURE DEFINITION 2: DIAGNOSES vs. PATTERN
# ______________________________________________________________________________________________________________________

pattern_0 <- "any pattern"
pattern_1 <- "m2d"
pattern_2 <- "m2q"
pattern_3 <- "m2qf"

expdat2 <- data.table()
y <- 2017

for(y in years){
  
  print(y)
  sub <- unique(aud.dat[year == y,.(pragmaid,gkv,case.id,date.aud,icd)])[order(gkv,pragmaid,date.aud)]
  
  # define diag type levels:
  subt0 <- unique(sub[icd %in% alc.diag_0, .(pragmaid,gkv,diag = 0,case.id,date.aud)])
  subt1 <- unique(sub[icd %in% alc.diag_1, .(pragmaid,gkv,diag = 1,case.id,date.aud)])
  subt2 <- unique(sub[icd %in% alc.diag_2, .(pragmaid,gkv,diag = 2,case.id,date.aud)])
  subt3 <- unique(sub[icd %in% alc.diag_3, .(pragmaid,gkv,diag = 3,case.id,date.aud)])
  subt4 <- unique(sub[icd %in% alc.diag_4, .(pragmaid,gkv,diag = 4,case.id,date.aud)])
  subt5 <- unique(sub[icd %in% alc.diag_5, .(pragmaid,gkv,diag = 5,case.id,date.aud)])
  rm(sub)
  
  # within each diag type: determine pattern of diagnoses
  ##  type 0
  subt0p0 <- subt0[,.(pragmaid,gkv,diag,pattern = 0)]
  subt0p1 <- subt0[, .(temp = .N >= 2), by = .(pragmaid,gkv,diag)] # M2D
  subt0p1 <- subt0p1[temp == T,.(pragmaid,gkv,diag,pattern = 1)]
  
  subt0p2 <- subt0[,.(pragmaid,gkv,diag,date.aud.q = quarter(date.aud, type = "quarter"))] # M2Q
  subt0p2[, m2q := length(unique(date.aud.q)) >= 2, by = .(gkv,pragmaid,diag)]
  subt0p2 <- subt0p2[m2q == T,.(pragmaid,gkv,diag,pattern = 2)]
  
  subt0p3 <- subt0[,.(pragmaid,gkv,diag,date.aud.q = quarter(date.aud, type = "quarter"))] # M2QF
  subt0p3[, m2qf := shift(date.aud.q), by = .(gkv,pragmaid,diag)]
  subt0p3 <- subt0p3[m2qf == 1,.(pragmaid,gkv,diag,pattern = 3)]
  
  rm(subt0)
  
  ##  type 1
  subt1p0 <- subt1[,.(pragmaid,gkv,diag,pattern = 0)]
  subt1p1 <- subt1[, .(temp = .N >= 2), by = .(pragmaid,gkv,diag)] # M2D
  subt1p1 <- subt1p1[temp == T,.(pragmaid,gkv,diag,pattern = 1)]
  
  subt1p2 <- subt1[,.(pragmaid,gkv,diag,date.aud.q = quarter(date.aud, type = "quarter"))] # M2Q
  subt1p2[, m2q := length(unique(date.aud.q)) >= 2, by = .(gkv,pragmaid,diag)]
  subt1p2 <- subt1p2[m2q == T,.(pragmaid,gkv,diag,pattern = 2)]
  
  subt1p3 <- subt1[,.(pragmaid,gkv,diag,date.aud.q = quarter(date.aud, type = "quarter"))] # M2QF
  subt1p3[, m2qf := shift(date.aud.q), by = .(gkv,pragmaid,diag)]
  subt1p3 <- subt1p3[m2qf == 1,.(pragmaid,gkv,diag,pattern = 3)]
  
  rm(subt1)
  
  ##  type 2
  subt2p0 <- subt2[,.(pragmaid,gkv,diag,pattern = 0)]
  subt2p1 <- subt2[, .(temp = .N >= 2), by = .(pragmaid,gkv,diag)] # M2D
  subt2p1 <- subt2p1[temp == T,.(pragmaid,gkv,diag,pattern = 1)]
  
  subt2p2 <- subt2[,.(pragmaid,gkv,diag,date.aud.q = quarter(date.aud, type = "quarter"))] # M2Q
  subt2p2[, m2q := length(unique(date.aud.q)) >= 2, by = .(gkv,pragmaid,diag)]
  subt2p2 <- subt2p2[m2q == T,.(pragmaid,gkv,diag,pattern = 2)]
  
  subt2p3 <- subt2[,.(pragmaid,gkv,diag,date.aud.q = quarter(date.aud, type = "quarter"))] # M2QF
  subt2p3[, m2qf := shift(date.aud.q), by = .(gkv,pragmaid,diag)]
  subt2p3 <- subt2p3[m2qf == 1,.(pragmaid,gkv,diag,pattern = 3)]
  
  rm(subt2)
  
  ##  type 3
  subt3p0 <- subt3[,.(pragmaid,gkv,diag,pattern = 0)]
  subt3p1 <- subt3[, .(temp = .N >= 2), by = .(pragmaid,gkv,diag)] # M2D
  subt3p1 <- subt3p1[temp == T,.(pragmaid,gkv,diag,pattern = 1)]
  
  subt3p2 <- subt3[,.(pragmaid,gkv,diag,date.aud.q = quarter(date.aud, type = "quarter"))] # M2Q
  subt3p2[, m2q := length(unique(date.aud.q)) >= 2, by = .(gkv,pragmaid,diag)]
  subt3p2 <- subt3p2[m2q == T,.(pragmaid,gkv,diag,pattern = 2)]
  
  subt3p3 <- subt3[,.(pragmaid,gkv,diag,date.aud.q = quarter(date.aud, type = "quarter"))] # M2QF
  subt3p3[, m2qf := shift(date.aud.q), by = .(gkv,pragmaid,diag)]
  subt3p3 <- subt3p3[m2qf == 1,.(pragmaid,gkv,diag,pattern = 3)]
  
  rm(subt3)
  
  ##  type 4
  subt4p0 <- subt4[,.(pragmaid,gkv,diag,pattern = 0)]
  subt4p1 <- subt4[, .(temp = .N >= 2), by = .(pragmaid,gkv,diag)] # M2D
  subt4p1 <- subt4p1[temp == T,.(pragmaid,gkv,diag,pattern = 1)]
  
  subt4p2 <- subt4[,.(pragmaid,gkv,diag,date.aud.q = quarter(date.aud, type = "quarter"))] # M2Q
  subt4p2[, m2q := length(unique(date.aud.q)) >= 2, by = .(gkv,pragmaid,diag)]
  subt4p2 <- subt4p2[m2q == T,.(pragmaid,gkv,diag,pattern = 2)]
  
  subt4p3 <- subt4[,.(pragmaid,gkv,diag,date.aud.q = quarter(date.aud, type = "quarter"))] # M2QF
  subt4p3[, m2qf := shift(date.aud.q), by = .(gkv,pragmaid,diag)]
  subt4p3 <- subt4p3[m2qf == 1,.(pragmaid,gkv,diag,pattern = 3)]
  
  rm(subt4)
  
  ##  type 4
  subt5p0 <- subt5[,.(pragmaid,gkv,diag,pattern = 0)]
  subt5p1 <- subt5[, .(temp = .N >= 2), by = .(pragmaid,gkv,diag)] # M2D
  subt5p1 <- subt5p1[temp == T,.(pragmaid,gkv,diag,pattern = 1)]
  
  subt5p2 <- subt5[,.(pragmaid,gkv,diag,date.aud.q = quarter(date.aud, type = "quarter"))] # M2Q
  subt5p2[, m2q := length(unique(date.aud.q)) >= 2, by = .(gkv,pragmaid,diag)]
  subt5p2 <- subt5p2[m2q == T,.(pragmaid,gkv,diag,pattern = 2)]
  
  subt5p3 <- subt5[,.(pragmaid,gkv,diag,date.aud.q = quarter(date.aud, type = "quarter"))] # M2QF
  subt5p3[, m2qf := shift(date.aud.q), by = .(gkv,pragmaid,diag)]
  subt5p3 <- subt5p3[m2qf == 1,.(pragmaid,gkv,diag,pattern = 3)]
  
  rm(subt5)
  
  # 
  add <- rbind(subt0p0,subt0p1,subt0p2,subt0p3,
               subt1p0,subt1p1,subt1p2,subt1p3,
               subt2p0,subt2p1,subt2p2,subt2p3,
               subt3p0,subt3p1,subt3p2,subt3p3,
               subt4p0,subt4p1,subt4p2,subt4p3,
               subt5p0,subt5p1,subt5p2,subt5p3)
  add$year <- y
  
  # remove duplicates (multiple dates/case.ids per diag type):
  add <- unique(add)
  
  # add
  expdat2 <- rbind(expdat2,add)
  
  # remove
  rm(subt0p0,subt0p1,subt0p2,subt0p3,
     subt1p0,subt1p1,subt1p2,subt1p3,
     subt2p0,subt2p1,subt2p2,subt2p3,
     subt3p0,subt3p1,subt3p2,subt3p3,
     subt4p0,subt4p1,subt4p2,subt4p3,
     subt5p0,subt5p1,subt5p2,subt5p3)
  
}

expdat2[,table(diag,pattern)] # should have all combinations 0:4 * 0:3
nrow(expdat2) # 396572

# check:
length(unique(expdat2$pragmaid)) # 21954
length(unique(expdat2[diag == 0 & pattern == 0]$pragmaid)) # 21954
length(unique(aud.dat$pragmaid)) # 21954 --> all persons included

# check:
select <- expdat2[year == 2019 & diag == 2 & pattern == 3]$pragmaid[2]
aud.dat[year == 2019 & pragmaid %in% select, table(icd)] # should have F10.1
aud.dat[year == 2019 & pragmaid %in% select, table(icd,date.aud)] # should have F10.1 in two subsequent quarters

select <- expdat2[year == 2020 & diag == 4 & pattern == 2]$pragmaid[10]
aud.dat[year == 2020 & pragmaid %in% select, table(icd)] # should have F10.3
aud.dat[year == 2020 & pragmaid %in% select, table(icd,date.aud)] # should have F10.3 in two quarters

# factor
expdat2$diag <- factor(expdat2$diag, levels = c(0:5))
expdat2$pattern <- factor(expdat2$pattern, levels = c(0:3))

rm(select, y)

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 6) EXAMINE EXPOSURE DATA
# ______________________________________________________________________________________________________________________

unique(expdat1[year == 2019, .(diag,pragmaid)])[, table(diag)] # 
unique(expdat2[year == 2019, .(diag,pragmaid)])[, table(diag)] # should be the same; 1587 persons with F10.1 (diag = 1) in 2019

# test
aud.dat[year == 2019 & icd %in% alc.diag_1, length(unique(pragmaid))] # 1587 -> as above

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 7) ADD SEX/AGE/EMP/COMORB TO EXPOSURE DATA
# ______________________________________________________________________________________________________________________

# AGE AND SEX
# ............................................

# add sex and yob
expdat1 <- merge(expdat1, id.dat[,.(pragmaid,sex,yob)], by = c("pragmaid"), all.x = T)
expdat1[is.na(sex) | is.na(yob)]
expdat2 <- merge(expdat2, id.dat[,.(pragmaid,sex,yob)], by = c("pragmaid"), all.x = T)
expdat2[is.na(sex) | is.na(yob)]

# age and age group
expdat1[, age := year - yob]
expdat1[, age.group := ifelse(age<25,"18-24",
                              ifelse(age<35,"25-34",
                                     ifelse(age<45,"35-44",
                                            ifelse(age<55,"45-54",
                                                   ifelse(age<65,"55-64",
                                                          ifelse(age<75,"65-74","75-99"))))))]
expdat1[, table(age,age.group, useNA = "always")] # remove ages <18
expdat1 <- expdat1[!age<18]
length(unique(expdat1$pragmaid)) # 21984

expdat2[, age := year - yob]
expdat2[, age.group := ifelse(age<25,"18-24",
                              ifelse(age<35,"25-34",
                                     ifelse(age<45,"35-44",
                                            ifelse(age<55,"45-54",
                                                   ifelse(age<65,"55-64",
                                                          ifelse(age<75,"65-74","75-99"))))))]
expdat2[, table(age,age.group, useNA = "always")]
expdat2 <- expdat2[!age<18]
length(unique(expdat1$pragmaid)) # 21,954

nrow(aud.dat[pragmaid %in% unique(expdat1$pragmaid)]) # 256,947

# EMPLOYMENT
# ............................................

# add employment for each year:

# combine with years and keep only emp periods falling into the years
add <- merge(unique(expdat1[,.(pragmaid,gkv,year)]),
                 emp.dat, by = c("pragmaid","gkv"), all.x = T, allow.cartesian = T)
add <- add[ year %between% list(year(date.emp.start), year(date.emp.end))]

##  calculate days of period between start and end to select relevant periods
add[, year.start := as.Date(paste0(year,"-01-01"))]
add[, year.end := as.Date(paste0(year,"-12-31"))]
add[, days_overlap := pmin(year.end, date.emp.end) - pmax(year.start, date.emp.start) + 1]
add <- add[days_overlap >=0]

##  determine dominant period in each year
add[, days_all := sum(days_overlap), by = .(pragmaid,gkv,year)]
add[, table(days_all)] # max 365/366 days 
add[, days_prop := as.numeric(days_overlap) / as.numeric(days_all)]
add[, select := max(days_prop), by = .(pragmaid,gkv,year)]
add[, select := days_prop == select]
add <- add[select == T,.(pragmaid,gkv,year,emp.type,days_prop)]


### more than 1 period: employed > unemployed > retired > other
select <- add[, .N, by = .(pragmaid,gkv,year)][N>1]$pragmaid
add[pragmaid %in% select]
add[pragmaid %in% select, check := ifelse(any(emp.type == "employed"), "employed",
                                          ifelse(any(emp.type == "unemployed"), "unemployed",
                                                 ifelse(any(emp.type == "retired"), "retired", "other"))), 
    by = .(pragmaid,gkv,year)]
add <- unique(add[is.na(check) | check == emp.type,.(pragmaid,gkv,year,emp.type)])

##  check
add[, .N, by = .(pragmaid,gkv,year)][N!=1] # none
add[, .N, by = .(pragmaid,gkv)][N>5] # none

expdat1 <- merge(expdat1,add,by = c("pragmaid","gkv","year"), all.x = T)

expdat2 <- merge(expdat2,add,by = c("pragmaid","gkv","year"), all.x = T)
rm(select,add)

##  missing
expdat1[is.na(emp.type)] # none

# dimensions
length(unique(expdat1$pragmaid)) # 21954
length(unique(expdat2$pragmaid)) # 21954


# COMORDITY
# ............................................

comorb.dat <- diag.dat[pragmaid %in% unique(expdat1$pragmaid)]

## keep only inpatient = admission/primary/secondary & outpatient = confirmed
comorb.dat[, table(setting, icd_type)] # 
comorb.dat[setting %like% "inpat|outpat" & setting != "outpatient surgery", 
           prop.table(table(setting, icd_type %like% "admission|primary|secondary|confirmed"),1)] # include > 90% of diagnoses in both settings
comorb.dat <- comorb.dat[setting %like% "inpat|outpat" & setting != "outpatient surgery" & icd_type %like% "admission|primary|secondary|confirmed"]
nrow(comorb.dat) # 5807851

##  remove year < 2017
comorb.dat <- comorb.dat[year(date.diag.start) >=2017]
nrow(comorb.dat) # 4910666
comorb.dat[, year := year(date.diag.start)]
c2017 <- comorb.dat[year == 2017]
c2018 <- comorb.dat[year == 2018]
c2019 <- comorb.dat[year == 2019]
c2020 <- comorb.dat[year == 2020]
c2021 <- comorb.dat[year == 2021]

##  get comorb for each year:
e2017 <- comorbidity::comorbidity(x = c2017,
                                  id = "pragmaid",
                                  code = "icd",
                                  map = "elixhauser_icd10_quan",
                                  assign0 = T,
                                  tidy.codes = T)
e2018 <- comorbidity::comorbidity(x = c2018,
                                  id = "pragmaid",
                                  code = "icd",
                                  map = "elixhauser_icd10_quan",
                                  assign0 = T,
                                  tidy.codes = T)
e2019 <- comorbidity::comorbidity(x = c2019,
                                  id = "pragmaid",
                                  code = "icd",
                                  map = "elixhauser_icd10_quan",
                                  assign0 = T,
                                  tidy.codes = T)
e2020 <- comorbidity::comorbidity(x = c2020,
                                  id = "pragmaid",
                                  code = "icd",
                                  map = "elixhauser_icd10_quan",
                                  assign0 = T,
                                  tidy.codes = T)
e2021 <- comorbidity::comorbidity(x = c2021,
                                  id = "pragmaid",
                                  code = "icd",
                                  map = "elixhauser_icd10_quan",
                                  assign0 = T,
                                  tidy.codes = T)

names(e2017)[!names(e2017) %like% "pragmaid"] <- 
  names(e2018)[!names(e2018) %like% "pragmaid"] <- 
  names(e2019)[!names(e2019) %like% "pragmaid"] <- 
  names(e2020)[!names(e2020) %like% "pragmaid"] <- 
  names(e2021)[!names(e2021) %like% "pragmaid"] <- 
  paste0("elix_",names(e2017)[!names(e2017) %like% "pragmaid"])

e2017sum <- data.table(e2017)
e2018sum <- data.table(e2018)
e2019sum <- data.table(e2019)
e2020sum <- data.table(e2020)
e2021sum <- data.table(e2021)

## get sumscore
e2017sum$elix_sum_all <- rowSums(e2017sum[,.SD, .SDcols = names(e2017sum)[names(e2017sum) %like% "^elix"]])
e2018sum$elix_sum_all <- rowSums(e2018sum[,.SD, .SDcols = names(e2018sum)[names(e2018sum) %like% "^elix"]])
e2019sum$elix_sum_all <- rowSums(e2019sum[,.SD, .SDcols = names(e2019sum)[names(e2019sum) %like% "^elix"]])
e2020sum$elix_sum_all <- rowSums(e2020sum[,.SD, .SDcols = names(e2020sum)[names(e2020sum) %like% "^elix"]])
e2021sum$elix_sum_all <- rowSums(e2021sum[,.SD, .SDcols = names(e2021sum)[names(e2021sum) %like% "^elix"]])

e2017sum$year <- 2017
e2018sum$year <- 2018
e2019sum$year <- 2019
e2020sum$year <- 2020
e2021sum$year <- 2021

elix_all <- rbind(e2017sum[,.(pragmaid,year,elix_sum_all)],
                  e2018sum[,.(pragmaid,year,elix_sum_all)],
                  e2019sum[,.(pragmaid,year,elix_sum_all)],
                  e2020sum[,.(pragmaid,year,elix_sum_all)],
                  e2021sum[,.(pragmaid,year,elix_sum_all)])

expdat1 <- merge(expdat1, elix_all, by = c("pragmaid","year"), all.x = T)
expdat1[is.na(elix_sum_all)]
expdat2 <- merge(expdat2, elix_all, by = c("pragmaid","year"), all.x = T)
expdat2[is.na(elix_sum_all)]

##  get data.table with information for all disease groups
elix_cats <- rbind(e2017sum,
                   e2018sum,
                   e2019sum,
                   e2020sum,
                   e2021sum)

names(elix_cats)
attr(e2017, "variable.labels")
names(elix_cats) <- c("pragmaid",attr(e2017, "variable.labels")[2:32],"elix_sum_all","year")

elix_cats <- melt(elix_cats,id.vars = c("pragmaid","year","elix_sum_all"), variable.name = "disease",value.name = "elix")
elix_cats[, disease_nr := 1:.N, by = .(pragmaid,year)]
elix_cats[, table(disease,disease_nr)]
elix_cats <- elix_cats[order(pragmaid,year,disease_nr)]

rm(comorb.dat,elix_all,
   c2017,c2018,c2019,c2020,c2021,
   e2017,e2018,e2019,e2020,e2021,
   e2017sum,e2018sum,e2019sum,e2020sum,e2021sum)




# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 8) PREPARE POPULATION DATA
# ______________________________________________________________________________________________________________________

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
                                                         ifelse(age %like% "^65|^70", "65-74","75-99"))))))]
pop.dat[, table(age, age.group, useNA = "always")]

# get sum pop for both GKV
pop.dat <- pop.dat[,.(pop = sum(pop)), by = .(year,sex,age.group)]

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 9) SAFE OUTPUT FILES
# ______________________________________________________________________________________________________________________

saveRDS(pop.dat, paste0("data/output/preprocessed/", Sys.Date(), "_insurance_population.RDS"))
saveRDS(aud.dat, paste0("data/output/preprocessed/", Sys.Date(), "_alcohol diagnoses data.RDS"))
saveRDS(expdat1, paste0("data/output/preprocessed/", Sys.Date(), "_exposure data_type and setting.RDS"))
saveRDS(expdat2, paste0("data/output/preprocessed/", Sys.Date(), "_exposure data_type and pattern.RDS"))
saveRDS(elix_cats, paste0("data/output/preprocessed/", Sys.Date(), "_elixhauser data.RDS"))
