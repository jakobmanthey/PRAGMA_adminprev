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
DATE2 <- "2024-11-11"
#DATE2 <- Sys.Date()

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
expdat1 <- readRDS(paste0(inpath, DATE2, "_exposure data_type and setting.RDS"))
expdat2 <- readRDS(paste0(inpath, DATE2, "_exposure data_type and pattern.RDS"))


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 2) REPORT DATA
# ______________________________________________________________________________________________________________________

# number of people:
length(unique(expdat1$pragmaid)) # 21984
length(unique(expdat2$pragmaid)) # 21984

# diag type table:
unique(expdat1[setting == 0, .(pragmaid,diag)])[, table(diag)]
unique(expdat2[pattern == 0, .(pragmaid,diag)])[, table(diag)]

unique(expdat1[year == 2017 & setting == 0, .(pragmaid,diag)])[, table(diag)] # lead: 3
unique(expdat1[year == 2018 & setting == 0, .(pragmaid,diag)])[, table(diag)] # lead: 3
unique(expdat1[year == 2019 & setting == 0, .(pragmaid,diag)])[, table(diag)] # lead: 3
unique(expdat1[year == 2020 & setting == 0, .(pragmaid,diag)])[, table(diag)] # lead: 3
unique(expdat1[year == 2021 & setting == 0, .(pragmaid,diag)])[, table(diag)] # lead: 3


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 3) FIGURES
# ______________________________________________________________________________________________________________________

##  1) FIG 1 - ...
# ..............



# 4) SUPPL FIGURES
# ______________________________________________________________________________________________________________________

##  1) SUPP FIG 1 - VENN DIAG X SETTING
# ..............

##  diagnosis type for setting = any
pdat0 <- merge(expdat1[year == 2019 & setting == 0 & diag == 0,.(pragmaid,any = T)],
              expdat1[year == 2019 & setting == 0 & diag == 1,.(pragmaid,f10_0 = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat0 <- merge(pdat0,
              expdat1[year == 2019 & setting == 0 & diag == 2,.(pragmaid,f10_1 = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat0 <- merge(pdat0,
              expdat1[year == 2019 & setting == 0 & diag == 3,.(pragmaid,f10_2 = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat0 <- merge(pdat0,
              expdat1[year == 2019 & setting == 0 & diag == 4,.(pragmaid,f10_3 = T)], by = "pragmaid", all = T, allow.cartesian = T)

pdat0 <- unique(pdat0)
pdat0$pragmaid <- NULL
pdat0[, (names(pdat0)) := lapply(.SD, function(x) ifelse(is.na(x), FALSE, x))]

##  diagnosis type for setting = outpatient
pdat1 <- merge(expdat1[year == 2019 & setting == 1 & diag == 0,.(pragmaid,any = T)],
              expdat1[year == 2019 & setting == 1 & diag == 1,.(pragmaid,f10_0 = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat1 <- merge(pdat1,
              expdat1[year == 2019 & setting == 1 & diag == 2,.(pragmaid,f10_1 = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat1 <- merge(pdat1,
              expdat1[year == 2019 & setting == 1 & diag == 3,.(pragmaid,f10_2 = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat1 <- merge(pdat1,
              expdat1[year == 2019 & setting == 1 & diag == 4,.(pragmaid,f10_3 = T)], by = "pragmaid", all = T, allow.cartesian = T)

pdat1 <- unique(pdat1)
pdat1$pragmaid <- NULL
pdat1[, (names(pdat1)) := lapply(.SD, function(x) ifelse(is.na(x), FALSE, x))]

##  diagnosis type for setting = inpatient
pdat2 <- merge(expdat1[year == 2019 & setting == 2 & diag == 0,.(pragmaid,any = T)],
               expdat1[year == 2019 & setting == 2 & diag == 1,.(pragmaid,f10_0 = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat2 <- merge(pdat2,
               expdat1[year == 2019 & setting == 2 & diag == 2,.(pragmaid,f10_1 = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat2 <- merge(pdat2,
               expdat1[year == 2019 & setting == 2 & diag == 3,.(pragmaid,f10_2 = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat2 <- merge(pdat2,
               expdat1[year == 2019 & setting == 2 & diag == 4,.(pragmaid,f10_3 = T)], by = "pragmaid", all = T, allow.cartesian = T)

pdat2 <- unique(pdat2)
pdat2$pragmaid <- NULL
pdat2[, (names(pdat2)) := lapply(.SD, function(x) ifelse(is.na(x), FALSE, x))]


##  diagnosis type for setting = outpatient&inpatient
pdat3 <- merge(expdat1[year == 2019 & setting == 3 & diag == 0,.(pragmaid,any = T)],
               expdat1[year == 2019 & setting == 3 & diag == 1,.(pragmaid,f10_0 = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat3 <- merge(pdat3,
               expdat1[year == 2019 & setting == 3 & diag == 2,.(pragmaid,f10_1 = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat3 <- merge(pdat3,
               expdat1[year == 2019 & setting == 3 & diag == 3,.(pragmaid,f10_2 = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat3 <- merge(pdat3,
               expdat1[year == 2019 & setting == 3 & diag == 4,.(pragmaid,f10_3 = T)], by = "pragmaid", all = T, allow.cartesian = T)

pdat3 <- unique(pdat3)
pdat3$pragmaid <- NULL
pdat3[, (names(pdat3)) := lapply(.SD, function(x) ifelse(is.na(x), FALSE, x))]

##  PLOTS

nrow(pdat0) #  11748
nrow(pdat1) #  10642
nrow(pdat2) #  3104
nrow(pdat3) #  1998

p0 <- plot(eulerr::euler(pdat0, shape = "ellipse"), main = "any setting (n=11748)")
p1 <- plot(eulerr::euler(pdat1, shape = "ellipse"), main = "outpatient (n=10642)")
p2 <- plot(eulerr::euler(pdat2, shape = "ellipse"), main = "inpatient (n=3104)")
p3 <- plot(eulerr::euler(pdat3, shape = "ellipse"), main = "outpatient & inpatient (n=1998)")

png(filename = paste0("figs/", Sys.Date(), "_fig_1_Venn diag by setting.png"),width = 1400, height = 1200)

gridExtra::grid.arrange(p0, p1, p2, p3, ncol = 2, nrow = 2)

dev.off()
rm(pdat0, pdat1, pdat2, pdat3,
   p0, p1, p2, p3)


##  2) SUPP FIG 2 - VENN DIAG X PATTERN
# ..............

##  diagnosis type for pattern = any
pdat0 <- merge(expdat2[year == 2019 & pattern == 0 & diag == 0,.(pragmaid,any = T)],
               expdat2[year == 2019 & pattern == 0 & diag == 1,.(pragmaid,f10_0 = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat0 <- merge(pdat0,
               expdat2[year == 2019 & pattern == 0 & diag == 2,.(pragmaid,f10_1 = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat0 <- merge(pdat0,
               expdat2[year == 2019 & pattern == 0 & diag == 3,.(pragmaid,f10_2 = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat0 <- merge(pdat0,
               expdat2[year == 2019 & pattern == 0 & diag == 4,.(pragmaid,f10_3 = T)], by = "pragmaid", all = T, allow.cartesian = T)

pdat0 <- unique(pdat0)
pdat0$pragmaid <- NULL
pdat0[, (names(pdat0)) := lapply(.SD, function(x) ifelse(is.na(x), FALSE, x))]

##  diagnosis type for pattern = m2d
pdat1 <- merge(expdat2[year == 2019 & pattern == 1 & diag == 0,.(pragmaid,any = T)],
               expdat2[year == 2019 & pattern == 1 & diag == 1,.(pragmaid,f10_0 = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat1 <- merge(pdat1,
               expdat2[year == 2019 & pattern == 1 & diag == 2,.(pragmaid,f10_1 = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat1 <- merge(pdat1,
               expdat2[year == 2019 & pattern == 1 & diag == 3,.(pragmaid,f10_2 = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat1 <- merge(pdat1,
               expdat2[year == 2019 & pattern == 1 & diag == 4,.(pragmaid,f10_3 = T)], by = "pragmaid", all = T, allow.cartesian = T)

pdat1 <- unique(pdat1)
pdat1$pragmaid <- NULL
pdat1[, (names(pdat1)) := lapply(.SD, function(x) ifelse(is.na(x), FALSE, x))]

##  diagnosis type for pattern = m2q
pdat2 <- merge(expdat2[year == 2019 & pattern == 2 & diag == 0,.(pragmaid,any = T)],
               expdat2[year == 2019 & pattern == 2 & diag == 1,.(pragmaid,f10_0 = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat2 <- merge(pdat2,
               expdat2[year == 2019 & pattern == 2 & diag == 2,.(pragmaid,f10_1 = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat2 <- merge(pdat2,
               expdat2[year == 2019 & pattern == 2 & diag == 3,.(pragmaid,f10_2 = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat2 <- merge(pdat2,
               expdat2[year == 2019 & pattern == 2 & diag == 4,.(pragmaid,f10_3 = T)], by = "pragmaid", all = T, allow.cartesian = T)

pdat2 <- unique(pdat2)
pdat2$pragmaid <- NULL
pdat2[, (names(pdat2)) := lapply(.SD, function(x) ifelse(is.na(x), FALSE, x))]


##  diagnosis type for pattern = m2qf
pdat3 <- merge(expdat2[year == 2019 & pattern == 3 & diag == 0,.(pragmaid,any = T)],
               expdat2[year == 2019 & pattern == 3 & diag == 1,.(pragmaid,f10_0 = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat3 <- merge(pdat3,
               expdat2[year == 2019 & pattern == 3 & diag == 2,.(pragmaid,f10_1 = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat3 <- merge(pdat3,
               expdat2[year == 2019 & pattern == 3 & diag == 3,.(pragmaid,f10_2 = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat3 <- merge(pdat3,
               expdat2[year == 2019 & pattern == 3 & diag == 4,.(pragmaid,f10_3 = T)], by = "pragmaid", all = T, allow.cartesian = T)

pdat3 <- unique(pdat3)
pdat3$pragmaid <- NULL
pdat3[, (names(pdat3)) := lapply(.SD, function(x) ifelse(is.na(x), FALSE, x))]

##  PLOTS

nrow(pdat0) #  11748
nrow(pdat1) #  8455
nrow(pdat2) #  8053
nrow(pdat3) #  6798

p0 <- plot(eulerr::euler(pdat0, shape = "ellipse"), main = "any pattern (n=11748)")
p1 <- plot(eulerr::euler(pdat1, shape = "ellipse"), main = "m2d (n=8455)")
p2 <- plot(eulerr::euler(pdat2, shape = "ellipse"), main = "m2q (n=8053)")
p3 <- plot(eulerr::euler(pdat3, shape = "ellipse"), main = "m2qf (n=6798)")

png(filename = paste0("figs/", Sys.Date(), "_fig_1_Venn diag by pattern.png"),width = 1400, height = 1200)

gridExtra::grid.arrange(p0, p1, p2, p3, ncol = 2, nrow = 2)

dev.off()

rm(pdat0, pdat1, pdat2, pdat3,
   p0, p1, p2, p3)






# 5) OLD FIGURES
# ______________________________________________________________________________________________________________________




##  setting type (any diagnosis)
pdat <- merge(expdat1[year == 2019 & setting == 1 & diag == 0,.(pragmaid,any = T)],
              expdat1[year == 2019 & setting == 1 & diag == 1,.(pragmaid,f10_0 = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat <- merge(pdat,
              expdat1[year == 2019 & setting == 1 & diag == 2,.(pragmaid,f10_1 = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat <- merge(pdat,
              expdat1[year == 2019 & setting == 1 & diag == 3,.(pragmaid,f10_2 = T)], by = "pragmaid", all = T, allow.cartesian = T)
pdat <- merge(pdat,
              expdat1[year == 2019 & setting == 1 & diag == 4,.(pragmaid,f10_3 = T)], by = "pragmaid", all = T, allow.cartesian = T)

pdat <- unique(pdat)
pdat$pragmaid <- NULL
pdat[, (names(pdat)) := lapply(.SD, function(x) ifelse(is.na(x), FALSE, x))]

plot(eulerr::euler(pdat, shape = "ellipse"))

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

ggplot(pdat, aes(x = age, y = def_rev, fill = def_rev)) +
  ggtitle("Alter und Frauenanteil (grünes Label) unter Versicherten mit F10.2-4 Diagnose",
          "nach Aufgreifkriterium [M1D für F10.x als Vergleich]; erstes dokumentierte Jahr pro Person") +
  ggridges::geom_density_ridges() +
  geom_label(data = pdat_prop, aes(y = def_rev,x = 100, label = scales::percent(prop_class_female), group = def_rev), 
             position = position_dodge(width = 0.75), fill = green_shades_5[1]) +
  scale_fill_manual("", values = rev(blue_shades_6)) +
  scale_x_continuous("Age in years") +
  scale_y_discrete("") +
  theme(legend.position = "bottom", legend.direction = "horizontal") + 
  guides(fill = guide_legend(reverse=TRUE)) 

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

ggplot(pdat, aes(x = elix_sum_all, y = def_rev, fill = def_rev)) +
  ggtitle("Elixhauser Komorbiditätsindex im Jahr der F10.2-4 Diagnose",
          "nach Aufgreifkriterium [M1D für F10.x als Vergleich]; erstes dokumentierte Jahr pro Person") +
  ggridges::geom_density_ridges() +
  scale_fill_manual("", values = rev(blue_shades_6)) +
  scale_x_continuous("Elixhauser sum score (0-31)") +
  scale_y_discrete("") +
  theme(legend.position = "bottom", legend.direction = "horizontal") + 
  guides(fill = guide_legend(reverse=TRUE))

ggsave(paste0("figs/", Sys.Date(), "_fig_5_elix by definition.png"),width = 12, height = 6)

pdat[, mean(elix_sum_all), by = definition]


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


ggplot(pdat, aes(x = elix_sum_all, y = setting, fill = setting)) +
  ggtitle("Elixhauser Komorbiditätsindex im Jahr der F10.2-4 Diagnose",
          "nach Setting, Aufgreifkriterium [M1D für F10.x als Vergleich]; erstes dokumentierte Jahr pro Person") +
  facet_wrap(definition ~.) + 
  ggridges::geom_density_ridges() +
  scale_fill_manual("", values = green_shades_6[c(1,3,6)]) +
  scale_x_continuous("Elixhauser sum score (0-31)") +
  scale_y_discrete("") +
  theme(legend.position = "bottom", legend.direction = "horizontal") 

pdat[, mean(elix_sum_all), by = setting]

  
ggsave(paste0("figs/", Sys.Date(), "_fig_6_elix by setting and definition.png"),width = 12, height = 6)











# Y) CAROS CODE FOR FIGURES
# ______________________________________________________________________________________________________________________


