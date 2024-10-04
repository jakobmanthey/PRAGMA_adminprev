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
inpath <- paste0("data/output/preprocessed/")

# output path
#outpath <- paste0("/Users/carolinkilian/Desktop/PRAGMA/Output/Administrative Prävalenz/")

# date
DATE <- "2024-08-19"
DATE2 <- "2024-10-04"
DATE2 <- Sys.Date()

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
blue_shades_5 <- colorRampPalette(c("lightblue", "darkblue"))(5)
blue_shades_6 <- colorRampPalette(c("lightblue", "darkblue"))(6)
green_shades_5 <- colorRampPalette(c("#DAF2D0", "#12501A"))(5)
green_shades_6 <- colorRampPalette(c("#DAF2D0", "#12501A"))(6)


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 1) LOAD AND COMBINE DATA
# ______________________________________________________________________________________________________________________

# All diagnoses
filename <- paste0("data/input/1_data_all diagnoses_",DATE,".rds")
diag.dat <- readRDS(filename)
rm(filename)

##  preprocessed
pop.dat <- readRDS(paste0(inpath, DATE2, "_insurance_population.RDS"))
M1D.dat_f10 <- readRDS(paste0(inpath, DATE2, "_F10_M1D.RDS"))
M1D.dat <- readRDS(paste0(inpath, DATE2, "_AUD_M1D.RDS"))
M2D.dat <- readRDS(paste0(inpath, DATE2, "_AUD_M2D.RDS"))
M2BF.dat <- readRDS(paste0(inpath,DATE2, "_AUD_M2BF.RDS"))
M2Q.dat <- readRDS(paste0(inpath, DATE2, "_AUD_M2Q.RDS"))
M2QF.dat <- readRDS(paste0(inpath,DATE2, "_AUD_M2QF.RDS"))

M1D.dat_f10$definition <- "M1D (F10)"
M1D.dat$definition <- "M1D"
M2D.dat$definition <- "M2D"
M2BF.dat$definition <- "M2BF"
M2Q.dat$definition <- "M2Q"
M2QF.dat$definition <- "M2QF"

data <- rbind(M1D.dat_f10, M1D.dat,
              M2D.dat, M2BF.dat,
              M2Q.dat, M2QF.dat)


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 2) PREPARE DATA
# ______________________________________________________________________________________________________________________

# age
data[, age.group := ifelse(age<25,"18-24",
                              ifelse(age<35,"25-34",
                                     ifelse(age<45,"35-44",
                                            ifelse(age<55,"45-54",
                                                   ifelse(age<65,"55-64",
                                                          ifelse(age<75,"65-74","75+"))))))]
data[, table(age,age.group, useNA = "always")]

# Comorbidity
comorb.dat <- diag.dat[pragmaid %in% unique(data$pragmaid)]

##  remove excluded diagnoses
comorb.dat[, table(setting, icd_type)]
comorb.dat <- comorb.dat[!icd_type %in% c("excluded")]
nrow(comorb.dat) # 5817262

##  remove year < 2017
comorb.dat <- comorb.dat[year(date.diag.start) >=2017]
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

data <- merge(data, elix_all, by = c("pragmaid","year"), all.x = T)

rm(comorb.dat,elix_all,
   c2017,c2018,c2019,c2020,c2021,
   e2017,e2018,e2019,e2020,e2021,
   e2017sum,e2018sum,e2019sum,e2020sum,e2021sum)

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 3) FIGURES
# ______________________________________________________________________________________________________________________

##  1) FIG 1
# ..............

# number of people in VENN (revise!)

pdat <- copy(data[year == 2019 & definition == "M1D (F10)",.(pragmaid,M1D_F10 = T)])
pdat <- merge(pdat, data[year == 2019 & definition == "M1D",.(pragmaid,M1D = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat <- merge(pdat, data[year == 2019 & definition == "M2D",.(pragmaid,M2D = T)], by = "pragmaid", all = T, allow.cartesian = T)
#pdat <- merge(pdat, data[year == 2019 & definition == "M2BF",.(pragmaid,M2BF = T)], by = "pragmaid", all = T, allow.cartesian = T)
#pdat <- merge(pdat, data[year == 2019 & definition == "M2Q",.(pragmaid,M2Q = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat <- merge(pdat, data[year == 2019 & definition == "M2QF",.(pragmaid,M2QF = T)], by = "pragmaid", all = T, allow.cartesian = T)

pdat <- unique(pdat)
pdat$pragmaid <- NULL
pdat[, (names(pdat)) := lapply(.SD, function(x) ifelse(is.na(x), FALSE, x))]

#v <- venneuler::venneuler(pdat)
#plot(v)


png(filename = paste0("figs/", Sys.Date(), "_fig_1_N Venn.png"),width = 500, height = 500)
plot(eulerr::euler(pdat))
dev.off()


##  2) FIG 2
# ..............

# number of people in BARCHART

pdat <- copy(data[year == 2019][order(pragmaid,year,definition,setting)])
pdat <- pdat[, .(setting = paste0(setting,collapse = " & ")), by = .(pragmaid,year,definition)]
pdat[, table(setting)]
pdat <- pdat[, .N, by = .(year,definition,setting)]
pdat$setting <- factor(pdat$setting, c("outpatient","inpatient","inpatient & outpatient"))
pdat[, setting := dplyr::recode(setting, 
                                "inpatient" = "Stationär", "outpatient" = "Ambulant", "inpatient & outpatient" = "Ambulant & Stationär")]
pdat$definition <- factor(pdat$definition,
                          c("M1D (F10)","M1D","M2D","M2BF","M2Q","M2QF"))
pdat$definition <- factor(pdat$definition, levels = rev(levels(pdat$definition)))
                          
ggplot(pdat, aes(x = definition, y = N, fill = definition)) + 
  ggtitle("Anzahl Versicherte mit F10.2-F10.4 Diagnose",
          "nach Aufgreifkriterium [M1D für F10.x als Vergleich], Setting; Jahr: 2019") +
  facet_grid(setting ~ .) + 
  geom_col(position = position_dodge()) + 
  scale_fill_manual("", values = rev(blue_shades_6)) +
  scale_x_discrete("") + 
  scale_y_continuous("") + 
  theme(legend.position = "bottom", legend.direction = "horizontal") + 
  guides(fill = guide_legend(reverse=TRUE)) +
  coord_flip()
ggsave(paste0("figs/", Sys.Date(), "_fig_2_N by definition.png"),width = 8, height = 8)



##  3) FIG 3
# ..............

# admin prev in barchart

pdat <- copy(data)[order(pragmaid,year,definition,setting)]

pdat <- pdat[, .(setting = paste0(setting,collapse = " & ")), by = .(pragmaid,year,definition,age.group)]
pdat[, table(setting)]
pdat <- pdat[, .N, by = .(year,age.group,definition,setting)]

add <- pop.dat[, .(pop = sum(pop)), by = .(year,age.group)]
pdat <- merge(pdat, add, 
                     by = c("year","age.group"), all.x = T)
pdat[is.na(N)] # none
pdat[, prev := N/pop]

pdat$definition <- factor(pdat$definition,
                             c("M1D (F10)","M1D","M2D","M2BF","M2Q","M2QF"))
pdat$definition <- factor(pdat$definition, levels = rev(levels(pdat$definition)))
pdat$age.group <- factor(pdat$age.group, levels = rev(unique(pdat$age.group)))
#pdat[, sex := dplyr::recode(sex, "female" = "Frauen", "male" = "Männer")]
pdat$setting <- factor(pdat$setting, c("outpatient","inpatient","inpatient & outpatient"))
pdat[, setting := dplyr::recode(setting, 
                                "inpatient" = "Stationär", "outpatient" = "Ambulant", "inpatient & outpatient" = "Ambulant & Stationär")]

#pdat[, setting := dplyr::recode(setting, "inpatient" = "Stationär", "outpatient" = "Ambulant")]

ggplot(pdat[year == 2019], aes(x = age.group, y = prev, fill = definition)) + 
  ggtitle("Administrative Prävalenz von F10.2-F10.4 Diagnosen",
          "nach Aufgreifkriterium [M1D für F10.x als Vergleich], Setting, Altersgruppe; Geschlecht: alle, Jahr: 2019") +
  facet_grid(. ~ setting) + 
  geom_col(position = position_dodge()) + 
  scale_fill_manual("", values = rev(blue_shades_6)) +
  scale_x_discrete("") + 
  scale_y_continuous("", labels = scales::percent) + 
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  guides(fill = guide_legend(reverse=TRUE)) +
  coord_flip()
ggsave(paste0("figs/", Sys.Date(), "_fig_3_prev by definition.png"),width = 12, height = 6)

add <- pop.dat[!age.group  %like% "65-74|75+", .(pop.1864 = sum(pop)), by = .(year)]
pdat <- merge(pdat, add, by = "year")
unique(pdat[year == 2019 & !age.group  %like% "65-74|75+", .(N = sum(N),pop.1864), by = .(definition)])[,.(definition,N/pop.1864)]



##  4) FIG 4
# ..............

# age/sex

pdat <- copy(data)
pdat[,min := min(year), by = pragmaid]
pdat <- pdat[year == min,.(pragmaid,sex,age,year,setting,definition)][order(pragmaid,setting)]

pdat <- pdat[, .(setting = paste0(setting,collapse = " & ")), by = .(pragmaid,year,sex,age,definition)]
pdat[, table(setting)]

pdat$definition <- factor(pdat$definition,
                          c("M1D (F10)","M1D","M2D","M2BF","M2Q","M2QF"))
pdat$def_rev <- factor(pdat$definition, levels = rev(levels(pdat$definition)))

pdat_prop <- unique(pdat[,.(pragmaid,def_rev,sex)])[,.(n_female = sum(sex == "female"),n_group = .N), by = .(def_rev)]
pdat_prop[, prop_class_female := round(n_female/n_group,2)]

ggplot(pdat, aes(x = def_rev, y = age)) +
  ggtitle("Alter (boxplot) und Frauenanteil (grünes Label) unter Versicherten mit F10.2-4 Diagnose",
          "nach Aufgreifkriterium [M1D für F10.x als Vergleich]; erstes dokumentierte Jahr pro Person") +
  geom_boxplot(aes(fill = def_rev)) + 
  geom_label(data = pdat_prop, aes(x = def_rev,y = 80, label = scales::percent(prop_class_female), group = def_rev), 
             position = position_dodge(width = 0.75), fill = green_shades_5[1]) +
  scale_fill_manual("", values = rev(blue_shades_6)) +
  scale_x_discrete("") +
  scale_y_continuous("Age in years") +
  theme(legend.position = "bottom", legend.direction = "horizontal") + 
  guides(fill = guide_legend(reverse=TRUE)) +
  coord_flip()

ggsave(paste0("figs/", Sys.Date(), "_fig_4_age and sex by definition.png"),width = 12, height = 6)

pdat[, mean(year), by = def_rev]
pdat[, sum(year == 2017)/.N, by = def_rev]

########

##  5) FIG 5
# ..............

# comorbidity

pdat <- copy(data)
pdat[,min := min(year), by = pragmaid]
pdat <- pdat[year == min,.(pragmaid,elix_sum_all,year,setting,definition)][order(pragmaid,setting)]

pdat <- pdat[, .(setting = paste0(setting,collapse = " & ")), by = .(pragmaid,year,definition,elix_sum_all)]
pdat[, table(setting)]

pdat$definition <- factor(pdat$definition,
                          c("M1D (F10)","M1D","M2D","M2BF","M2Q","M2QF"))
pdat$def_rev <- factor(pdat$definition, levels = rev(levels(pdat$definition)))

pdat[, mean := mean(elix_sum_all), by = def_rev]

ggplot(pdat, aes(x = def_rev, y = elix_sum_all, fill = def_rev)) +
  ggtitle("Elixhauser Komorbiditätsindex im Jahr der F10.2-4 Diagnose",
          "nach Aufgreifkriterium [M1D für F10.x als Vergleich]; erstes dokumentierte Jahr pro Person\nSenkrechte Striche: Quantile 25%/50%/75%") +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), color = "white") + 
  #geom_point(aes(y = mean), size = 3, shape = 25, fill = "black") +
  scale_fill_manual("", values = rev(blue_shades_6)) +
  scale_x_discrete("") +
  scale_y_continuous("Elixhauser sum score (0-31)") +
  theme(legend.position = "bottom", legend.direction = "horizontal") + 
  guides(fill = guide_legend(reverse=TRUE)) +
  coord_flip()

ggsave(paste0("figs/", Sys.Date(), "_fig_5_elix by definition.png"),width = 12, height = 6)



##  5) FIG 6
# ..............

# comorbidity by setting

pdat <- copy(data)[order(pragmaid,setting)]
pdat <- pdat[, .(setting = paste0(setting,collapse = " & ")), by = .(pragmaid,year,definition,elix_sum_all)]
pdat[, table(setting)]

pdat[,min := min(year), by = pragmaid]
pdat <- pdat[year == min,.(pragmaid,elix_sum_all,year,setting,definition)]

pdat$definition <- factor(pdat$definition,
                          c("M1D (F10)","M1D","M2D","M2BF","M2Q","M2QF"))
pdat$def_rev <- factor(pdat$definition, levels = rev(levels(pdat$definition)))

pdat$setting <- factor(pdat$setting, c("outpatient","inpatient","inpatient & outpatient"))
pdat[, setting := dplyr::recode(setting, 
                                "inpatient" = "Stationär", "outpatient" = "Ambulant", "inpatient & outpatient" = "Ambulant & Stationär")]

#pdat[, mean := mean(elix_sum_all), by = def_rev]

ggplot(pdat, aes(x = definition, y = elix_sum_all, fill = setting)) +
  ggtitle("Elixhauser Komorbiditätsindex im Jahr der F10.2-4 Diagnose",
          "nach Setting, Aufgreifkriterium [M1D für F10.x als Vergleich]; erstes dokumentierte Jahr pro Person\nSenkrechte Striche: Quantile 25%/50%/75%") +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), color = "black") + 
  scale_fill_manual("", values = green_shades_6[c(1,3,6)]) +
  scale_x_discrete("") +
  scale_y_continuous("Elixhauser sum score (0-31)") +
  theme(legend.position = "bottom", legend.direction = "horizontal") 

ggsave(paste0("figs/", Sys.Date(), "_fig_6_elix by setting and definition.png"),width = 12, height = 6)











# Y) CAROS CODE FOR FIGURES
# ______________________________________________________________________________________________________________________



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
M1D.dat <- M1D.dat[,.N, by = .(year,sex,age.group,setting)][order(year,sex,age.group,setting)]
#M1D.dat <- dcast(M1D.dat, year + sex + age.group ~ setting, value.var = "N")

# calculate prevalence
prev.M1D    <- merge(pop.dat[,.(pop = sum(pop)), by = .(year,sex,age.group)],
                     M1D.dat,
                     by = c("year","sex","age.group"), all.x = T)
prev.M1D[is.na(N)] # none
prev.M1D[, prev := N/pop]

# Plots
pdat <- copy(prev.M1D)
pdat$year <- as.factor(pdat$year)
pdat[, sex := dplyr::recode(sex, "female" = "Frauen", "male" = "Männer")]
pdat[, setting := dplyr::recode(setting, "inpatient" = "Stationär", "outpatient" = "Ambulant")]

ggplot(pdat, aes(x = age.group, y = prev, fill = year)) + 
  ggtitle("Administrative Prävalenz gesicherter F10.2-F10.4 Diagnosen (M1D)",
          "nach Versorgung, Geschlecht, Altersgruppe und Jahr") +
  facet_grid(sex ~ setting) + 
  geom_col(position = position_dodge()) + 
  scale_fill_manual("", values = blue_shades) +
  scale_x_discrete("") + 
  scale_y_continuous("", labels = scales::percent) + 
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(paste0(outpath, Sys.Date(), "/Prävalenz_M1D_F10.2-F10.4.png"),width = 10, height = 6)


## 2) M2D 

M2D.dat[, age.group := ifelse(age<25,"18-24",
                              ifelse(age<35,"25-34",
                                     ifelse(age<45,"35-44",
                                            ifelse(age<55,"45-54",
                                                   ifelse(age<65,"55-64",
                                                          ifelse(age<75,"65-74","75+"))))))]
M2D.dat[, table(age,age.group, useNA = "always")]

# aggregate by sex and age groups:
M2D.dat <- M2D.dat[,.N, by = .(year,sex,age.group,setting)][order(year,sex,age.group,setting)]
#M2D.dat <- dcast(M2D.dat, year + sex + age.group ~ setting, value.var = "N")

# calculate prevalence
prev.M2D    <- merge(pop.dat[,.(pop = sum(pop)), by = .(year,sex,age.group)],
                     M2D.dat,
                     by = c("year","sex","age.group"), all.x = T)
prev.M2D[is.na(N)] # none
prev.M2D[, prev := N/pop]

# Plots
pdat <- copy(prev.M2D)
pdat$year <- as.factor(pdat$year)
pdat[, sex := dplyr::recode(sex, "female" = "Frauen", "male" = "Männer")]
pdat[, setting := dplyr::recode(setting, "inpatient" = "Stationär", "outpatient" = "Ambulant")]

ggplot(pdat, aes(x = age.group, y = prev, fill = year)) + 
  ggtitle("Administrative Prävalenz gesicherter F10.2-F10.4 Diagnosen (M2D)",
          "nach Versorgung, Geschlecht, Altersgruppe und Jahr") +
  facet_grid(sex ~ setting) + 
  geom_col(position = position_dodge()) + 
  scale_fill_manual("", values = blue_shades) +
  scale_x_discrete("") + 
  scale_y_continuous("", labels = scales::percent) + 
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(paste0(outpath, Sys.Date(), "/Prävalenz_M2D_F10.2-F10.4.png"),width = 10, height = 6)

## 3) M2BF 

M2BF.dat[, age.group := ifelse(age<25,"18-24",
                              ifelse(age<35,"25-34",
                                     ifelse(age<45,"35-44",
                                            ifelse(age<55,"45-54",
                                                   ifelse(age<65,"55-64",
                                                          ifelse(age<75,"65-74","75+"))))))]
M2BF.dat[, table(age,age.group, useNA = "always")]

# aggregate by sex and age groups:
M2BF.dat <- M2BF.dat[,.N, by = .(year,sex,age.group,setting)][order(year,sex,age.group,setting)]
#M2BF.dat <- dcast(M2BF.dat, gkv + year + sex + age.group ~ setting, value.var = "N")

# calculate prevalence
prev.M2BF    <- merge(pop.dat[,.(pop = sum(pop)), by = .(year,sex,age.group)],
                     M2BF.dat,
                     by = c("year","sex","age.group"), all.x = T)
prev.M2BF[is.na(N)] # none
prev.M2BF[, prev := N/pop]

# Plots
pdat <- copy(prev.M2BF)
pdat$year <- as.factor(pdat$year)
pdat[, sex := dplyr::recode(sex, "female" = "Frauen", "male" = "Männer")]
pdat[, setting := dplyr::recode(setting, "inpatient" = "Stationär", "outpatient" = "Ambulant")]

ggplot(pdat, aes(x = age.group, y = prev, fill = year)) + 
  ggtitle("Administrative Prävalenz gesicherter F10.2-F10.4 Diagnosen (M2BF)",
          "nach Versorgung, Geschlecht, Altersgruppe und Jahr") +
  facet_grid(sex ~ setting) + 
  geom_col(position = position_dodge()) + 
  scale_fill_manual("", values = blue_shades) +
  scale_x_discrete("") + 
  scale_y_continuous("", labels = scales::percent) + 
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(paste0(outpath, Sys.Date(), "/Prävalenz_M2BF_F10.2-F10.4.png"),width = 10, height = 6)


## 4) M2Q 

M2Q.dat[, age.group := ifelse(age<25,"18-24",
                              ifelse(age<35,"25-34",
                                     ifelse(age<45,"35-44",
                                            ifelse(age<55,"45-54",
                                                   ifelse(age<65,"55-64",
                                                          ifelse(age<75,"65-74","75+"))))))]
M2Q.dat[, table(age,age.group, useNA = "always")]

# aggregate by sex and age groups:
M2Q.dat <- M2Q.dat[,.N, by = .(year,sex,age.group,setting)][order(year,sex,age.group,setting)]
#M2Q.dat <- dcast(M2Q.dat, gkv + year + sex + age.group ~ setting, value.var = "N")

# calculate prevalence
prev.M2Q    <- merge(pop.dat[,.(pop = sum(pop)), by = .(year,sex,age.group)],
                     M2Q.dat,
                     by = c("year","sex","age.group"), all.x = T)
prev.M2Q[is.na(N)] # none
prev.M2Q[, prev := N/pop]

# Plots
pdat <- copy(prev.M2Q)
pdat$year <- as.factor(pdat$year)
pdat[, sex := dplyr::recode(sex, "female" = "Frauen", "male" = "Männer")]
pdat[, setting := dplyr::recode(setting, "inpatient" = "Stationär", "outpatient" = "Ambulant")]

ggplot(pdat, aes(x = age.group, y = prev, fill = year)) + 
  ggtitle("Adiministrative Prävalenz gesicherter F10.2-F10.4 Diagnosen (M2Q)",
          "nach Versorgung, Geschlecht, Altersgruppe und Jahr") +
  facet_grid(sex ~ setting) + 
  geom_col(position = position_dodge()) + 
  scale_fill_manual("", values = blue_shades) +
  scale_x_discrete("") + 
  scale_y_continuous("", labels = scales::percent) + 
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(paste0(outpath, Sys.Date(), "/Prävalenz_M2Q_F10.2-F10.4.png"),width = 10, height = 6)


## 2) M2QF 

M2QF.dat[, age.group := ifelse(age<25,"18-24",
                              ifelse(age<35,"25-34",
                                     ifelse(age<45,"35-44",
                                            ifelse(age<55,"45-54",
                                                   ifelse(age<65,"55-64",
                                                          ifelse(age<75,"65-74","75+"))))))]
M2QF.dat[, table(age,age.group, useNA = "always")]

# aggregate by sex and age groups:
M2QF.dat <- M2QF.dat[,.N, by = .(year,sex,age.group,setting)][order(year,sex,age.group,setting)]
#M2QF.dat <- dcast(M2QF.dat, gkv + year + sex + age.group ~ setting, value.var = "N")

# calculate prevalence
prev.M2QF    <- merge(pop.dat[,.(pop = sum(pop)), by = .(year,sex,age.group)],
                     M2QF.dat,
                     by = c("year","sex","age.group"), all.x = T)
prev.M2QF[is.na(N)] # none
prev.M2QF[, prev := N/pop]

# Plots
pdat <- copy(prev.M2QF)
pdat$year <- as.factor(pdat$year)
pdat[, sex := dplyr::recode(sex, "female" = "Frauen", "male" = "Männer")]
pdat[, setting := dplyr::recode(setting, "inpatient" = "Stationär", "outpatient" = "Ambulant")]

ggplot(pdat, aes(x = age.group, y = prev, fill = year)) + 
  ggtitle("Administrative Prävalenz gesicherter F10.2-F10.4 Diagnosen (M2QF)",
          "nach Versorgung, Geschlecht, Altersgruppe und Jahr") +
  facet_grid(sex ~ setting) + 
  geom_col(position = position_dodge()) + 
  scale_fill_manual("", values = blue_shades) +
  scale_x_discrete("") + 
  scale_y_continuous("", labels = scales::percent) + 
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(paste0(outpath, Sys.Date(), "/Prävalenz_M2QF_F10.2-F10.4.png"),width = 10, height = 6)


# 3) PREVALENCE ACROSS DEFINITIONS

prev.dat <- rbind(prev.M1D[, definition := "M1D"], 
                  prev.M2D[, definition := "M2D"],
                  prev.M2BF[, definition := "M2BF"],
                  prev.M2Q[, definition := "M2Q"],
                  prev.M2QF[, definition := "M2QF"])

prev.dat <- prev.dat[,.(N = sum(N),
                        pop = sum(pop)),
                     by = .(year,setting,sex,definition)][, prev := N/pop]

pdat <- copy(prev.dat)
pdat$year <- as.factor(pdat$year)
pdat[, sex := dplyr::recode(sex, "female" = "Frauen", "male" = "Männer")]
pdat[, setting := dplyr::recode(setting, "inpatient" = "Stationär", "outpatient" = "Ambulant")]

ggplot(pdat, aes(x = year, y = prev, fill = definition)) + 
  ggtitle("Administrative Prävalenz gesicherter F10.2-F10.4 Diagnosen",
          "nach Versorgung, Geschlecht, Aufgreifkriterium und Jahr") +
  facet_grid(sex ~ setting) + 
  geom_col(position = position_dodge()) + 
  scale_fill_manual("", values = green_shades) +
  scale_x_discrete("") + 
  scale_y_continuous("", labels = scales::percent) + 
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(paste0(outpath, Sys.Date(), "/Prävalenz_F10.2-F10.4.png"),width = 10, height = 6)


# 4) SAFE OUTPUT FILES

out_by_definition <- list('M1D' = prev.M1D, 'M2D' = prev.M2D, 'M2BF' = prev.M2BF, 
                          'M2Q' = prev.M2Q, 'M2QF' = prev.M2QF)
write.xlsx(out_by_definition, file = paste0(outpath, Sys.Date(), "/Prävalenz_F10.2-F10.4_BeiAltersgruppe.xlsx"))

write.xlsx(prev.dat, file = paste0(outpath, Sys.Date(), "/Prävalenz_F10.2-F10.4_Gesamt.xlsx"))

