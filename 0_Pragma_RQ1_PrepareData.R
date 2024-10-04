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
#inpath <- paste0("/Users/carolinkilian/Desktop/PRAGMA/Data/")

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


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 2) PREPARE POPULATION DATA
# ______________________________________________________________________________________________________________________

# Sex and age data

ses.dat <- id.dat[,.(pragmaid,gkv,sex,yob)]

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

# 3) PREPARE AUD DEFINITION DATA
# ______________________________________________________________________________________________________________________

# Only F10.2 to F10.4 diagnoses
# ..............

aud.dat <- copy(diag.dat[,.(pragmaid,gkv,setting,case.id,icd,icd_type,date.aud = date.diag.start)])
nrow(aud.dat) # 360630
length(unique(aud.dat$pragmaid)) # 25293

# remove years <2017
aud.dat <- aud.dat[!year(date.aud) <2017]
nrow(aud.dat) # 296948
length(unique(aud.dat$pragmaid)) # 23172

# add sex/age and remove <18y
aud.dat <- merge(aud.dat,ses.dat, by = c("gkv","pragmaid"), all.x = T)
aud.dat[, age := year(date.aud)-yob]
aud.dat[, summary(age)]
aud.dat <- aud.dat[!age < 18]
nrow(aud.dat) # 296351
length(unique(aud.dat$pragmaid)) # 23092

# keep any F10 diagnosis
aud.dat <- aud.dat[icd %like% "F10"]
nrow(aud.dat) # 255221
length(unique(aud.dat$pragmaid)) # 21229

# keep only inpatient = admission/primary/secondary & outpatient = confirmed
aud.dat[, table(setting, icd_type)] # 
aud.dat[setting %like% "inpat|outpat" & setting != "outpatient surgery", 
        prop.table(table(setting, icd_type %like% "admission|primary|secondary|confirmed"),1)] # include > 90% of diagnoses in both settings
aud.dat <- aud.dat[setting %like% "inpat|outpat" & setting != "outpatient surgery" & icd_type %like% "admission|primary|secondary|confirmed"]
nrow(aud.dat) # 219389
length(unique(aud.dat$pragmaid)) # 20134

##################
# F10dat
f10.M1D.dat <- unique(aud.dat[!is.na(date.aud),.(gkv,pragmaid,sex,age,setting,year = year(date.aud))])
f10.M1D.dat <- f10.M1D.dat[order(setting,year)]

nrow(f10.M1D.dat) # 60966
length(unique(f10.M1D.dat$pragmaid)) # 20134
##################

# keep any F10.2|F10.3|F10.4 diagnosis
select <-  "F10.2|F10.3|F10.4"
aud.dat[!icd %like% select, table(icd)]
aud.dat[icd == "F10.0", length(unique(pragmaid))] # 4691
aud.dat[icd == "F10.1", length(unique(pragmaid))] # 11335
aud.dat <- aud.dat[icd %like% select]
nrow(aud.dat) # 122680
length(unique(aud.dat$pragmaid)) # 11882
aud.dat[, table(gkv, icd, useNA = "always")]


##  1) M1D
#      mindestens einmalige Diagnose
# ..............

# keep each person if they have diagnoses in that year
aud.M1D.dat <- unique(aud.dat[!is.na(date.aud),.(gkv,pragmaid,sex,age,setting,year = year(date.aud))])
aud.M1D.dat <- aud.M1D.dat[order(setting,year,gkv)]

nrow(aud.M1D.dat) # 36911
length(unique(aud.M1D.dat$pragmaid)) # 11882

##  2) M2D
#      mindestens zwei Diagnosen
# ..............

addM2D <- copy(aud.dat[,.(pragmaid,gkv,
                          setting2 = setting,
                          case.id2 = case.id,
                          icd2 = icd,
                          date.aud2 = date.aud)])

#addM2D <- addM2D[,.(pragmaid,gkv,setting2 = setting,case.id2 = case.id,icd2 = icd,date.aud2)]

aud.M2D.dat <- merge(aud.dat,
                      addM2D, 
                      by = c("pragmaid","gkv"), 
                      allow.cartesian = T,
                      all.x = T)
aud.M2D.dat[is.na(date.aud2)] # none
rm(addM2D)
#aud.M2D.dat <- aud.M2D.dat[!is.na(date.aud2)]

nrow(aud.M2D.dat) # 
length(unique(aud.M2D.dat$pragmaid)) # 11882

# diagnosis2 maximum one year apart
aud.M2D.dat <- aud.M2D.dat[order(pragmaid,date.aud,date.aud2)]
aud.M2D.dat[, interval := as.numeric(date.aud2) - as.numeric(date.aud)]
aud.M2D.dat[interval > 365, ]
aud.M2D.dat <- aud.M2D.dat[!(interval > 365 | interval < 0)]

nrow(aud.M2D.dat) # 
length(unique(aud.M2D.dat$pragmaid)) # 11882 (none removed because the same diagnoses is also considered - next step)

# remove rows with same ICD code, case IDs and setting if interval == 0
aud.M2D.dat[interval == 0 & icd2 == icd & case.id2 == case.id & setting2 == setting] # exact same diagnosis in same setting and case -> remove
aud.M2D.dat <- aud.M2D.dat[!(interval == 0 & icd2 == icd & case.id2 == case.id & setting2 == setting)] # remove
aud.M2D.dat[interval == 0 & case.id2 == case.id & setting2 == setting] # similar diagnosis in same setting and case -> yes!
aud.M2D.dat[interval == 0 & case.id2 == case.id & setting2 == setting, length(unique(pragmaid))] # similar diagnosis in same setting and case -> yes! n=2372 persons
#add.M2D <- aud.M2D.dat[interval == 0 & icd2 != icd | interval == 0 & case.id2 != case.id | interval == 0 & setting2 != setting] # caros definition
#aud.M2D.dat <- rbind(aud.M2D.dat[interval > 0,],add.M2D)

nrow(aud.M2D.dat) # 
length(unique(aud.M2D.dat$pragmaid)) # 8758

# keep only info on first AUD diagnosis
aud.M2D.dat.full <- copy(aud.M2D.dat)
aud.M2D.dat <- unique(aud.M2D.dat[,.(pragmaid,setting,gkv,case.id,date.aud,sex,age)])

nrow(aud.M2D.dat) # 
length(unique(aud.M2D.dat$pragmaid)) # 8758

# keep each person if they have diagnoses in that year
aud.M2D.dat <- unique(aud.M2D.dat[!is.na(date.aud),.(gkv,pragmaid,sex,age,setting,year = year(date.aud))])
aud.M2D.dat <- aud.M2D.dat[order(year,gkv)]

nrow(aud.M2D.dat) # 
length(unique(aud.M2D.dat$pragmaid)) # 8758

##  3) M2BF
#      mindestens zwei Diagnosen in unterschiedlichen Behandlungsfällen
# ..............

addM2BF <- copy(aud.dat[,.(pragmaid,gkv,
                           setting2 = setting,
                           case.id2 = case.id,
                           icd2 = icd,
                           icd_type2 = icd_type,
                           date.aud2 = date.aud)])

#addM2BF <- addM2BF[,.(pragmaid,gkv,case.id2 = case.id,date.aud2 = date.diag.start)]

aud.M2BF.dat <- merge(aud.dat,
                      addM2BF, 
                      by = c("pragmaid","gkv"), 
                      allow.cartesian = T,
                      all.x = T)
aud.M2BF.dat[is.na(date.aud2)] # none
rm(addM2BF)

nrow(aud.M2BF.dat) # 
length(unique(aud.M2BF.dat$pragmaid)) # 11882

# diagnosis2 maximum one year apart
aud.M2BF.dat <- aud.M2BF.dat[order(pragmaid,date.aud,date.aud2)]
aud.M2BF.dat[, interval := as.numeric(date.aud2) - as.numeric(date.aud)]
aud.M2BF.dat[interval > 365, ]
aud.M2BF.dat <- aud.M2BF.dat[!(interval > 365 | interval < 0)]

nrow(aud.M2BF.dat) # 
length(unique(aud.M2BF.dat$pragmaid)) # 11882 (none removed because the same diagnoses is also considered - next step)

# remove rows with same case IDs
aud.M2BF.dat <- aud.M2BF.dat[!case.id == case.id2]

nrow(aud.M2BF.dat) # 
length(unique(aud.M2BF.dat$pragmaid)) # 8539

# keep only info on first AUD diagnosis
aud.M2BF.dat.full <- copy(aud.M2BF.dat)
aud.M2BF.dat <- unique(aud.M2BF.dat[,.(pragmaid,setting,gkv,case.id,date.aud,sex,age)])

nrow(aud.M2BF.dat) # 
length(unique(aud.M2BF.dat$pragmaid)) # 8539

# keep each person if they have diagnoses in that year
aud.M2BF.dat <- unique(aud.M2BF.dat[!is.na(date.aud),.(gkv,pragmaid,sex,age,setting,year = year(date.aud))])
aud.M2BF.dat <- aud.M2BF.dat[order(year,gkv)]

nrow(aud.M2BF.dat) # 
length(unique(aud.M2BF.dat$pragmaid)) # 8539

##  4) M2Q
#      mindestens zwei Diagnosen in zwei unterschiedlichen Quartalen
# ..............

addM2Q <- copy(aud.dat[,.(pragmaid,gkv,
                          setting2 = setting,
                          case.id2 = case.id,
                          icd2 = icd,
                          icd_type2 = icd_type,
                          date.aud2 = date.aud,
                          aud.yq2 = quarter(date.aud, type = "year.quarter"))])

aud.M2Q.dat <- copy(aud.dat)
aud.M2Q.dat[, aud.yq := quarter(date.aud, type = "year.quarter")]
aud.M2Q.dat <- merge(aud.M2Q.dat,
                      addM2Q, 
                      by = c("pragmaid","gkv"), 
                      allow.cartesian = T,
                      all.x = T)
aud.M2Q.dat[is.na(date.aud2)] # none
rm(addM2Q)

nrow(aud.M2Q.dat) # 
length(unique(aud.M2Q.dat$pragmaid)) # 11882

# diagnosis2 maximum one year apart
aud.M2Q.dat <- aud.M2Q.dat[order(pragmaid,date.aud,date.aud2)]
aud.M2Q.dat[, interval := as.numeric(date.aud2) - as.numeric(date.aud)]
aud.M2Q.dat[interval > 365]
aud.M2Q.dat <- aud.M2Q.dat[!(interval > 365 | interval < 0)]

nrow(aud.M2Q.dat) # 
length(unique(aud.M2Q.dat$pragmaid)) # 11882 (none removed because the same diagnoses is also considered - next step)

# remove rows with diagnosis within same quarter
aud.M2Q.dat <- aud.M2Q.dat[!aud.yq == aud.yq2]

nrow(aud.M2Q.dat) # 
length(unique(aud.M2Q.dat$pragmaid)) # 8163

# keep only info on first AUD diagnosis
aud.M2Q.dat <- unique(aud.M2Q.dat[,.(pragmaid,setting,gkv,case.id,date.aud,sex,age)])

nrow(aud.M2Q.dat) # 
length(unique(aud.M2Q.dat$pragmaid)) # 8163

# keep each person if they have diagnoses in that year
aud.M2Q.dat.full <- copy(aud.M2Q.dat)
aud.M2Q.dat <- unique(aud.M2Q.dat[!is.na(date.aud),.(gkv,pragmaid,sex,age,setting,year = year(date.aud))])
aud.M2Q.dat <- aud.M2Q.dat[order(year,gkv)]

nrow(aud.M2Q.dat) # 
length(unique(aud.M2Q.dat$pragmaid)) # 8163

##  5) M2QF
#      mindestens zwei Diagnosen in zwei aufeinander folgenden Quartalen
# ..............

addM2QF <- copy(aud.dat[,.(pragmaid,gkv,
                           setting2 = setting,
                           case.id2 = case.id,
                           icd2 = icd,
                           icd_type2 = icd_type,
                           date.aud2 = date.aud,
                           aud.yq2 = quarter(date.aud, type = "year.quarter"))])

aud.M2QF.dat <- copy(aud.dat)
aud.M2QF.dat[, aud.yq := quarter(date.aud, type = "year.quarter")]
aud.M2QF.dat <- merge(aud.M2QF.dat,
                     addM2QF, 
                     by = c("pragmaid","gkv"), 
                     allow.cartesian = T,
                     all.x = T)
aud.M2QF.dat[is.na(date.aud2)] # none
rm(addM2QF)

nrow(aud.M2QF.dat) # 
length(unique(aud.M2QF.dat$pragmaid)) # 11882

# remove rows with diagnosis not within subsequent quarters
aud.M2QF.dat[!(aud.yq == (aud.yq2 + 0.1) | aud.yq == (aud.yq2 - 0.1))] # same Q or Q with >1Q difference to be removed
aud.M2QF.dat <- aud.M2QF.dat[aud.yq == (aud.yq2 + 0.1) | aud.yq == (aud.yq2 - 0.1)]

nrow(aud.M2QF.dat) # 
length(unique(aud.M2QF.dat$pragmaid)) # 11882

# keep only info on first AUD diagnosis
aud.M2QF.dat.full <- copy(aud.M2QF.dat)
aud.M2QF.dat <- unique(aud.M2QF.dat[,.(pragmaid,sex,age,setting,gkv,case.id,date.aud)])

nrow(aud.M2QF.dat) # 
length(unique(aud.M2QF.dat$pragmaid)) # 6141

# keep each person if they have diagnoses in that year
aud.M2QF.dat <- unique(aud.M2QF.dat[!is.na(date.aud),.(gkv,pragmaid,sex,age,setting,year = year(date.aud))])
aud.M2QF.dat <- aud.M2QF.dat[order(year,gkv)]

nrow(aud.M2QF.dat) # 
length(unique(aud.M2QF.dat$pragmaid)) # 6141

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 4) SAFE OUTPUT FILES
# ______________________________________________________________________________________________________________________

saveRDS(pop.dat, paste0("data/output/preprocessed/", Sys.Date(), "_insurance_population.RDS"))
saveRDS(f10.M1D.dat, paste0("data/output/preprocessed/", Sys.Date(), "_F10_M1D.RDS"))
saveRDS(aud.M1D.dat, paste0("data/output/preprocessed/", Sys.Date(), "_AUD_M1D.RDS"))
saveRDS(aud.M2D.dat, paste0("data/output/preprocessed/", Sys.Date(), "_AUD_M2D.RDS"))
saveRDS(aud.M2BF.dat, paste0("data/output/preprocessed/", Sys.Date(), "_AUD_M2BF.RDS"))
saveRDS(aud.M2Q.dat, paste0("data/output/preprocessed/", Sys.Date(), "_AUD_M2Q.RDS"))
saveRDS(aud.M2QF.dat, paste0("data/output/preprocessed/", Sys.Date(), "_AUD_M2QF.RDS"))
