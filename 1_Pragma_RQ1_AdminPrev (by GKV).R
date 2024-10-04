# ==================================================================================================================================================================
# ==================================================================================================================================================================

# PROJECT TITLE:  PRAGMA
# CODE AUTHOR:    JM + CK
# DATE STARTED:   2024/01/25
# DATE LAST MODIFIED:   2024/08/29

# ==================================================================================================================================================================
# ==================================================================================================================================================================

# clean workspace
rm(list=ls())

# input path
inpath <- paste0("/Users/carolinkilian/Desktop/PRAGMA/Data/")

# output path
outpath <- paste0("/Users/carolinkilian/Desktop/PRAGMA/Output/Administrative Prävalenz/")

# date
DATE <- "2024-08-29"

# load libraries
library( data.table )
library( ggplot2 )
library( ggthemes )
library( tidyr )
library( stringr )
library( openxlsx )

# themes and options
theme_set( theme_gdocs() )
options(scipen = 999)
blue_shades <- colorRampPalette(c("lightblue", "darkblue"))(5)
green_shades <- colorRampPalette(c("#DAF2D0", "#12501A"))(5)

# 1) LOAD DATA

pop.dat <- readRDS(paste0(inpath, "preprocessed data/", DATE, "_RQ1_insurance_population.RDS"))
M1D.dat <- readRDS(paste0(inpath, "preprocessed data/", DATE, "_RQ1_M1D_alcohol.RDS"))
M2D.dat <- readRDS(paste0(inpath, "preprocessed data/", DATE, "_RQ1_M2D_alcohol.RDS"))
M2BF.dat <- readRDS(paste0(inpath, "preprocessed data/", DATE, "_RQ1_M2BF_alcohol.RDS"))
M2Q.dat <- readRDS(paste0(inpath, "preprocessed data/", DATE, "_RQ1_M2Q_alcohol.RDS"))
M2QF.dat <- readRDS(paste0(inpath, "preprocessed data/", DATE, "_RQ1_M2QF_alcohol.RDS"))

 
# 2) PREVALENCE BY DEFINITION

## 1) M1D 

M1D.dat[, age.group := ifelse(age<25,"18-24",
                              ifelse(age<35,"25-34",
                                     ifelse(age<45,"35-44",
                                            ifelse(age<55,"45-54",
                                                   ifelse(age<65,"55-64",
                                                          ifelse(age<75,"65-74","75+"))))))]
M1D.dat[, table(age,age.group, useNA = "always")]

# aggregate by sex and age groups:
M1D.dat <- M1D.dat[,.N, by = .(gkv,year,sex,age.group,setting)][order(gkv,year,sex,age.group,setting)]
M1D.dat <- dcast(M1D.dat, gkv + year + sex + age.group ~ setting, value.var = "N")

# calculate prevalence
prev.M1D    <- merge(pop.dat[,.(pop = sum(pop)), by = .(gkv,year,sex,age.group)],
                     M1D.dat,
                     by = c("gkv","year","sex","age.group"), all.x = T)
prev.M1D[is.na(outpatient)] # none
prev.M1D[is.na(inpatient)] # none
prev.M1D[, ':=' (out.prev = outpatient/pop,
                 inp.prev = inpatient/pop)]

# Plots
pdat <- copy(prev.M1D)
pdat$year <- as.factor(pdat$year)
pdat[, gkv := dplyr::recode(gkv, "aok" = "AOK", "dak" = "DAK")]
pdat[, sex := dplyr::recode(sex, "female" = "Frauen", "male" = "Männer")]

#  AMBULANT
ggplot(pdat, aes(x = age.group, y = out.prev, fill = year)) + 
  ggtitle("Prävalenz gesicherter F10 Diagnosen (M1D) in der ambulanten Versorgung",
          "nach Krankenkasse, Geschlecht, Altersgruppe und Jahr") +
  facet_grid(sex ~ gkv) + 
  geom_col(position = position_dodge()) + 
  scale_fill_manual("", values = blue_shades) +
  scale_x_discrete("") + 
  scale_y_continuous("", labels = scales::percent) + 
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(paste0(outpath, Sys.Date(), "/Prävalenz_M1D_ambulant.png"),width = 10, height = 6)

#  INPATIENT
ggplot(pdat, aes(x = age.group, y = inp.prev, fill = year)) + 
  ggtitle("Prävalenz gesicherter F10 Diagnosen (M1D) in der stationären Versorgung",
          "nach Krankenkasse, Geschlecht, Altersgruppe und Jahr") +
  facet_grid(sex ~ gkv) + 
  geom_col(position = position_dodge()) + 
  scale_fill_manual("", values = blue_shades) +
  scale_x_discrete("") + 
  scale_y_continuous("", labels = scales::percent) + 
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(paste0(outpath, Sys.Date(), "/Prävalenz_M1D_stationär.png"),width = 10, height = 6)


## 2) M2D 

M2D.dat[, age.group := ifelse(age<25,"18-24",
                              ifelse(age<35,"25-34",
                                     ifelse(age<45,"35-44",
                                            ifelse(age<55,"45-54",
                                                   ifelse(age<65,"55-64",
                                                          ifelse(age<75,"65-74","75+"))))))]
M2D.dat[, table(age,age.group, useNA = "always")]

# aggregate by sex and age groups:
M2D.dat <- M2D.dat[,.N, by = .(gkv,year,sex,age.group,setting)][order(gkv,year,sex,age.group,setting)]
M2D.dat <- dcast(M2D.dat, gkv + year + sex + age.group ~ setting, value.var = "N")

# calculate prevalence
prev.M2D    <- merge(pop.dat[,.(pop = sum(pop)), by = .(gkv,year,sex,age.group)],
                     M2D.dat,
                     by = c("gkv","year","sex","age.group"), all.x = T)
prev.M2D[is.na(outpatient)] # none
prev.M2D[is.na(inpatient)] # none
prev.M2D[, ':=' (out.prev = outpatient/pop,
                 inp.prev = inpatient/pop)]

# Plots
pdat <- copy(prev.M2D)
pdat$year <- as.factor(pdat$year)
pdat[, gkv := dplyr::recode(gkv, "aok" = "AOK", "dak" = "DAK")]
pdat[, sex := dplyr::recode(sex, "female" = "Frauen", "male" = "Männer")]

#  AMBULANT
ggplot(pdat, aes(x = age.group, y = out.prev, fill = year)) + 
  ggtitle("Prävalenz gesicherter F10 Diagnosen (M2D) in der ambulanten Versorgung",
          "nach Krankenkasse, Geschlecht, Altersgruppe und Jahr") +
  facet_grid(sex ~ gkv) + 
  geom_col(position = position_dodge()) + 
  scale_fill_manual("", values = blue_shades) +
  scale_x_discrete("") + 
  scale_y_continuous("", labels = scales::percent) + 
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(paste0(outpath, Sys.Date(), "/Prävalenz_M2D_ambulant.png"),width = 10, height = 6)

#  INPATIENT
ggplot(pdat, aes(x = age.group, y = inp.prev, fill = year)) + 
  ggtitle("Prävalenz gesicherter F10 Diagnosen (M2D) in der stationären Versorgung",
          "nach Krankenkasse, Geschlecht, Altersgruppe und Jahr") +
  facet_grid(sex ~ gkv) + 
  geom_col(position = position_dodge()) + 
  scale_fill_manual("", values = blue_shades) +
  scale_x_discrete("") + 
  scale_y_continuous("", labels = scales::percent) + 
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(paste0(outpath, Sys.Date(), "/Prävalenz_M2D_stationär.png"),width = 10, height = 6)


## 3) M2BF 

M2BF.dat[, age.group := ifelse(age<25,"18-24",
                              ifelse(age<35,"25-34",
                                     ifelse(age<45,"35-44",
                                            ifelse(age<55,"45-54",
                                                   ifelse(age<65,"55-64",
                                                          ifelse(age<75,"65-74","75+"))))))]
M2BF.dat[, table(age,age.group, useNA = "always")]

# aggregate by sex and age groups:
M2BF.dat <- M2BF.dat[,.N, by = .(gkv,year,sex,age.group,setting)][order(gkv,year,sex,age.group,setting)]
M2BF.dat <- dcast(M2BF.dat, gkv + year + sex + age.group ~ setting, value.var = "N")

# calculate prevalence
prev.M2BF    <- merge(pop.dat[,.(pop = sum(pop)), by = .(gkv,year,sex,age.group)],
                     M2BF.dat,
                     by = c("gkv","year","sex","age.group"), all.x = T)
prev.M2BF[is.na(outpatient)] # none
prev.M2BF[is.na(inpatient)] # none
prev.M2BF[, ':=' (out.prev = outpatient/pop,
                 inp.prev = inpatient/pop)]

# Plots
pdat <- copy(prev.M2BF)
pdat$year <- as.factor(pdat$year)
pdat[, gkv := dplyr::recode(gkv, "aok" = "AOK", "dak" = "DAK")]
pdat[, sex := dplyr::recode(sex, "female" = "Frauen", "male" = "Männer")]

#  AMBULANT
ggplot(pdat, aes(x = age.group, y = out.prev, fill = year)) + 
  ggtitle("Prävalenz gesicherter F10 Diagnosen (M2BF) in der ambulanten Versorgung",
          "nach Krankenkasse, Geschlecht, Altersgruppe und Jahr") +
  facet_grid(sex ~ gkv) + 
  geom_col(position = position_dodge()) + 
  scale_fill_manual("", values = blue_shades) +
  scale_x_discrete("") + 
  scale_y_continuous("", labels = scales::percent) + 
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(paste0(outpath, Sys.Date(), "/Prävalenz_M2BF_ambulant.png"),width = 10, height = 6)

#  INPATIENT
ggplot(pdat, aes(x = age.group, y = inp.prev, fill = year)) + 
  ggtitle("Prävalenz gesicherter F10 Diagnosen (M2BF) in der stationären Versorgung",
          "nach Krankenkasse, Geschlecht, Altersgruppe und Jahr") +
  facet_grid(sex ~ gkv) + 
  geom_col(position = position_dodge()) + 
  scale_fill_manual("", values = blue_shades) +
  scale_x_discrete("") + 
  scale_y_continuous("", labels = scales::percent) + 
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(paste0(outpath, Sys.Date(), "/Prävalenz_M2BF_stationär.png"),width = 10, height = 6)


## 4) M2Q 

M2Q.dat[, age.group := ifelse(age<25,"18-24",
                              ifelse(age<35,"25-34",
                                     ifelse(age<45,"35-44",
                                            ifelse(age<55,"45-54",
                                                   ifelse(age<65,"55-64",
                                                          ifelse(age<75,"65-74","75+"))))))]
M2Q.dat[, table(age,age.group, useNA = "always")]

# aggregate by sex and age groups:
M2Q.dat <- M2Q.dat[,.N, by = .(gkv,year,sex,age.group,setting)][order(gkv,year,sex,age.group,setting)]
M2Q.dat <- dcast(M2Q.dat, gkv + year + sex + age.group ~ setting, value.var = "N")

# calculate prevalence
prev.M2Q    <- merge(pop.dat[,.(pop = sum(pop)), by = .(gkv,year,sex,age.group)],
                     M2Q.dat,
                     by = c("gkv","year","sex","age.group"), all.x = T)
prev.M2Q[is.na(outpatient)] # none
prev.M2Q[is.na(inpatient)] # none
prev.M2Q[, ':=' (out.prev = outpatient/pop,
                 inp.prev = inpatient/pop)]

# Plots
pdat <- copy(prev.M2Q)
pdat$year <- as.factor(pdat$year)
pdat[, gkv := dplyr::recode(gkv, "aok" = "AOK", "dak" = "DAK")]
pdat[, sex := dplyr::recode(sex, "female" = "Frauen", "male" = "Männer")]

#  AMBULANT
ggplot(pdat, aes(x = age.group, y = out.prev, fill = year)) + 
  ggtitle("Prävalenz gesicherter F10 Diagnosen (M2Q) in der ambulanten Versorgung",
          "nach Krankenkasse, Geschlecht, Altersgruppe und Jahr") +
  facet_grid(sex ~ gkv) + 
  geom_col(position = position_dodge()) + 
  scale_fill_manual("", values = blue_shades) +
  scale_x_discrete("") + 
  scale_y_continuous("", labels = scales::percent) + 
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(paste0(outpath, Sys.Date(), "/Prävalenz_M2Q_ambulant.png"),width = 10, height = 6)

#  INPATIENT
ggplot(pdat, aes(x = age.group, y = inp.prev, fill = year)) + 
  ggtitle("Prävalenz gesicherter F10 Diagnosen (M2Q) in der stationären Versorgung",
          "nach Krankenkasse, Geschlecht, Altersgruppe und Jahr") +
  facet_grid(sex ~ gkv) + 
  geom_col(position = position_dodge()) + 
  scale_fill_manual("", values = blue_shades) +
  scale_x_discrete("") + 
  scale_y_continuous("", labels = scales::percent) + 
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(paste0(outpath, Sys.Date(), "/Prävalenz_M2Q_stationär.png"),width = 10, height = 6)


## 2) M2QF 

M2QF.dat[, age.group := ifelse(age<25,"18-24",
                              ifelse(age<35,"25-34",
                                     ifelse(age<45,"35-44",
                                            ifelse(age<55,"45-54",
                                                   ifelse(age<65,"55-64",
                                                          ifelse(age<75,"65-74","75+"))))))]
M2QF.dat[, table(age,age.group, useNA = "always")]

# aggregate by sex and age groups:
M2QF.dat <- M2QF.dat[,.N, by = .(gkv,year,sex,age.group,setting)][order(gkv,year,sex,age.group,setting)]
M2QF.dat <- dcast(M2QF.dat, gkv + year + sex + age.group ~ setting, value.var = "N")

# calculate prevalence
prev.M2QF    <- merge(pop.dat[,.(pop = sum(pop)), by = .(gkv,year,sex,age.group)],
                     M2QF.dat,
                     by = c("gkv","year","sex","age.group"), all.x = T)
prev.M2QF[is.na(outpatient)] # none
prev.M2QF[is.na(inpatient)] # none
prev.M2QF[, ':=' (out.prev = outpatient/pop,
                 inp.prev = inpatient/pop)]

# Plots
pdat <- copy(prev.M2QF)
pdat$year <- as.factor(pdat$year)
pdat[, gkv := dplyr::recode(gkv, "aok" = "AOK", "dak" = "DAK")]
pdat[, sex := dplyr::recode(sex, "female" = "Frauen", "male" = "Männer")]

#  AMBULANT
ggplot(pdat, aes(x = age.group, y = out.prev, fill = year)) + 
  ggtitle("Prävalenz gesicherter F10 Diagnosen (M2QF) in der ambulanten Versorgung",
          "nach Krankenkasse, Geschlecht, Altersgruppe und Jahr") +
  facet_grid(sex ~ gkv) + 
  geom_col(position = position_dodge()) + 
  scale_fill_manual("", values = blue_shades) +
  scale_x_discrete("") + 
  scale_y_continuous("", labels = scales::percent) + 
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(paste0(outpath, Sys.Date(), "/Prävalenz_M2QF_ambulant.png"),width = 10, height = 6)

#  INPATIENT
ggplot(pdat, aes(x = age.group, y = inp.prev, fill = year)) + 
  ggtitle("Prävalenz gesicherter F10 Diagnosen (M2QF) in der stationären Versorgung",
          "nach Krankenkasse, Geschlecht, Altersgruppe und Jahr") +
  facet_grid(sex ~ gkv) + 
  geom_col(position = position_dodge()) + 
  scale_fill_manual("", values = blue_shades) +
  scale_x_discrete("") + 
  scale_y_continuous("", labels = scales::percent) + 
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(paste0(outpath, Sys.Date(), "/Prävalenz_M2QF_stationär.png"),width = 10, height = 6)


# 3) PREVALENCE ACROSS DEFINITIONS

prev.dat <- rbind(prev.M1D[, definition := "M1D"], 
                  prev.M2D[, definition := "M2D"],
                  prev.M2BF[, definition := "M2BF"],
                  prev.M2Q[, definition := "M2Q"],
                  prev.M2QF[, definition := "M2QF"])

prev.dat <- prev.dat[,.(outpatient = sum(outpatient),
                        inpatient = sum(inpatient),
                        pop = sum(pop)),
                     by = .(gkv,year,sex,definition)][, ':=' (out.prev = outpatient/pop,
                                                              inp.prev = inpatient/pop)]

pdat <- copy(prev.dat)
pdat$year <- as.factor(pdat$year)
pdat[, gkv := dplyr::recode(gkv, "aok" = "AOK", "dak" = "DAK")]
pdat[, sex := dplyr::recode(sex, "female" = "Frauen", "male" = "Männer")]

#  AMBULANT
ggplot(pdat, aes(x = year, y = out.prev, fill = definition)) + 
  ggtitle("Prävalenz gesicherter F10 Diagnosen in der ambulanten Versorgung",
          "nach Krankenkasse, Geschlecht, Aufgreifkriterium und Jahr") +
  facet_grid(sex ~ gkv) + 
  geom_col(position = position_dodge()) + 
  scale_fill_manual("", values = green_shades) +
  scale_x_discrete("") + 
  scale_y_continuous("", labels = scales::percent) + 
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(paste0(outpath, Sys.Date(), "/Prävalenz_ambulant.png"),width = 10, height = 6)

#  INPATIENT
ggplot(pdat, aes(x = year, y = inp.prev, fill = definition)) + 
  ggtitle("Prävalenz gesicherter F10 Diagnosen in der stationären Versorgung",
          "nach Krankenkasse, Geschlecht, Altersgruppe und Jahr") +
  facet_grid(sex ~ gkv) + 
  geom_col(position = position_dodge()) + 
  scale_fill_manual("", values = green_shades) +
  scale_x_discrete("") + 
  scale_y_continuous("", labels = scales::percent) + 
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(paste0(outpath, Sys.Date(), "/Prävalenz_stationär.png"),width = 10, height = 6)

# 4) SAFE OUTPUT FILES

out_by_definition <- list('M1D' = prev.M1D, 'M2D' = prev.M2D, 'M2BF' = prev.M2BF, 
                          'M2Q' = prev.M2Q, 'M2QF' = prev.M2QF)
write.xlsx(out_by_definition, file = paste0(outpath, Sys.Date(), "/Prävalenz_BeiAltersgruppe.xlsx"))

write.xlsx(prev.dat, file = paste0(outpath, Sys.Date(), "/Prävalenz_Gesamt.xlsx"))

# ==================================================================================================================================================================
# Technical report

# Total for report:
prev.allalc[year == 2021,.(outpatient = sum(outpatient),
                           inpatient = sum(inpatient),
                           pop = sum(pop)),
            by = .(gkv,sex)][,.(gkv,sex,
                                out.prev = outpatient/pop,
                                inp.prev = inpatient/pop)]

# Total for report:
prev.f102[year == 2021,.(outpatient = sum(outpatient),
                         inpatient = sum(inpatient),
                         pop = sum(pop)),
          by = .(gkv,sex)][,.(gkv,sex,
                              out.prev = outpatient/pop,
                              inp.prev = inpatient/pop)]
# Total numbers for cross-validation with DAK:
#prev.f102[gkv == "dak",.(out = sum(out)), by = year]

