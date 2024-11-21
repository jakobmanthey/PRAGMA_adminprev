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
DATE2 <- "2024-11-19"
#DATE2 <- Sys.Date()

# load libraries
library( data.table )
library( ggplot2 )
library( ggthemes )
library( tidyr )
library( stringr )
library( openxlsx )
library( lme4 )
library( gee )

# themes and options
theme_set( theme_gdocs() )
options(scipen = 999)
blue_shades_5 <- colorRampPalette(c("lightblue", "darkblue"))(5)
blue_shades_6 <- colorRampPalette(c("lightblue", "darkblue"))(6)
green_shades_5 <- colorRampPalette(c("#DAF2D0", "#12501A"))(5)
green_shades_6 <- colorRampPalette(c("#DAF2D0", "#12501A"))(6)
three_colors <- c("#FFB3BA", "#B3FFB3", "#B3D9FF")
three_colors <- c("#FF7F0E", "#2CA02C", "#1F77B4")

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 1) LOAD AND COMBINE DATA
# ______________________________________________________________________________________________________________________

# All diagnoses
filename <- paste0("data/input/1_data_all diagnoses_",DATE,".rds")
#diag.dat <- readRDS(filename)
rm(filename)

##  preprocessed
pop.dat <- readRDS(paste0(inpath, DATE2, "_insurance_population.RDS"))
expdat1 <- readRDS(paste0(inpath, DATE2, "_exposure data_type and setting.RDS"))
expdat2 <- readRDS(paste0(inpath, DATE2, "_exposure data_type and pattern.RDS"))

##  unique AUD dat across all years
aud.dat <- unique(expdat1[setting == 0 & diag == 1,.(pragmaid,f10_0 = T)])
aud.dat <- merge(aud.dat,
              unique(expdat1[setting == 0 & diag == 2,.(pragmaid,f10_1 = T)]), 
              by = "pragmaid", all = T, allow.cartesian = T)
aud.dat <- merge(aud.dat,
              unique(expdat1[setting == 0 & diag == 3,.(pragmaid,f10_2 = T)]), 
              by = "pragmaid", all = T, allow.cartesian = T)
aud.dat <- merge(aud.dat,
              unique(expdat1[setting == 0 & diag == 4,.(pragmaid,f10_3 = T)]), 
              by = "pragmaid", all = T, allow.cartesian = T)

aud.dat <- unique(aud.dat)
nrow(aud.dat) #  21984
aud.dat[, (names(aud.dat)) := lapply(.SD, function(x) ifelse(is.na(x), FALSE, x))]

##  prev.dat
prev.dat <- expdat1[setting == 0,.(n = .N), by = .(year,diag,sex,age.group)][order(year,diag)]
prev.dat <- merge(prev.dat, pop.dat, by = c("year","sex","age.group"))[order(year,sex,age.group,diag)]
  
  ### add any sex and total age
  add1 <- prev.dat[,.(sex = "any", n = sum(n), pop = sum(pop)), by = .(year,age.group,diag)]
  add2 <- prev.dat[,.(age.group = "18-99", n = sum(n), pop = sum(pop)), by = .(year,sex,diag)]
  add3 <- prev.dat[,.(sex = "any", age.group = "18-99", n = sum(n), pop = sum(pop)), by = .(year,diag)]
  prev.dat <- rbind(prev.dat,
                    add1,add2,add3)
  rm(add1,add2,add3)
  
  ### get administrative prevalence
  prev.dat[, prev := n/pop]
  prev.dat$n <- prev.dat$pop <- NULL
  
##  prev.dat2
prev.dat2 <- rbind(expdat1[,.(var = "setting", n = .N), by = .(year,diag,val = setting,sex,age.group)],
                   expdat2[,.(var = "pattern", n = .N), by = .(year,diag,val = pattern,sex,age.group)])
                  
prev.dat2 <- merge(prev.dat2, pop.dat, by = c("year","sex","age.group"))[order(year,sex,age.group,diag)]

  ### add any sex and total age
  add1 <- prev.dat2[,.(sex = "any", n = sum(n), pop = sum(pop)), by = .(year,age.group,diag,var,val)]
  add2 <- prev.dat2[,.(age.group = "18-99", n = sum(n), pop = sum(pop)), by = .(year,sex,diag,var,val)]
  add3 <- prev.dat2[,.(sex = "any", age.group = "18-99", n = sum(n), pop = sum(pop)), by = .(year,diag,var,val)]
  prev.dat2 <- rbind(prev.dat2,
                    add1,add2,add3)
  rm(add1,add2,add3)
  
  ### get administrative prevalence
  prev.dat2[, prev := n/pop]
  prev.dat2$n <- prev.dat2$pop <- NULL
  prev.dat2 <- prev.dat2[order(diag,var,val)]
  
##  ses.dat: only one person per diag - first year
ses.dat <- copy(expdat1[setting == 0][order(diag)])
ses.dat[,min := min(year), by = .(pragmaid,diag)]
ses.dat <- unique(ses.dat[year == min,.(pragmaid,year,diag,sex,age,age.group,elix_sum_all)][order(pragmaid,diag)])
ses.dat[, .N, by = .(pragmaid,diag)][N!=1] 
ses.dat[, mean(year), by = diag][order(diag)]
  
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 2) REPORT DATA
# ______________________________________________________________________________________________________________________

##  1) DESCRIPTION OF AUD SEVERITY
# ..............

# number of people:
length(unique(expdat1$pragmaid)) # 21984
length(unique(expdat2$pragmaid)) # 21984

# diag type table:
melt(aud.dat)[, table(variable, value)]
unique(ses.dat[, .(pragmaid,diag)])[, table(diag)]
unique(ses.dat[, .(pragmaid,diag)])[, prop.table(table(diag))]

unique(expdat1[year == 2017 & setting == 0, .(pragmaid,diag)])[, table(diag)] # lead: 3
unique(expdat1[year == 2018 & setting == 0, .(pragmaid,diag)])[, table(diag)] # lead: 3
unique(expdat1[year == 2019 & setting == 0, .(pragmaid,diag)])[, table(diag)] # lead: 3
unique(expdat1[year == 2020 & setting == 0, .(pragmaid,diag)])[, table(diag)] # lead: 3
unique(expdat1[year == 2021 & setting == 0, .(pragmaid,diag)])[, table(diag)] # lead: 3

# overlaps
sum(aud.dat$f10_0 & aud.dat$f10_1) / nrow(aud.dat) # 10.3%
sum(aud.dat$f10_0 & aud.dat$f10_2) / nrow(aud.dat) # 12.1%
sum(aud.dat$f10_0 & aud.dat$f10_3) / nrow(aud.dat) # 8.6%
sum(aud.dat$f10_1 & aud.dat$f10_2) / nrow(aud.dat) # 23.4%
sum(aud.dat$f10_1 & aud.dat$f10_3) / nrow(aud.dat) # 16.7%
sum(aud.dat$f10_2 & aud.dat$f10_3) / nrow(aud.dat) # 21.8%

aud.dat[f10_0 == T, sum(f10_1 == T | f10_2 == T | f10_3 == T)/.N] # 59.7%
aud.dat[f10_1 == T, sum(f10_0 == T | f10_2 == T | f10_3 == T)/.N] # 55.3%
aud.dat[f10_2 == T, sum(f10_0 == T | f10_1 == T | f10_3 == T)/.N] # 64.8%
aud.dat[f10_3 == T, sum(f10_0 == T | f10_1 == T | f10_2 == T)/.N] # 73.0%


##  2) ADMINISTRATIVE PREVALENCE OF AUD SEVERITY
# ..............

# manual
expdat1[setting == 0 & diag == 0 & year == 2017, length(unique(pragmaid))] / 
  pop.dat[year == 2017, sum(pop)] # 2.9%

# from prev.dat
prev.dat[sex == "any" & age.group == "18-99" & diag == 0] # 2.9%
prev.dat[sex == "any" & age.group == "18-99" & diag != 0, mean(prev), by = diag] # mean % by diag across years
prev.dat[sex == "any" & age.group == "18-24" & diag != 0, mean(prev), by = diag] # mean % by diag across years among 18-24
prev.dat[age.group == "18-99" & diag == 0, mean(prev), by = sex]
prev.dat[sex == "any" & age.group != "18-99" & diag == 0, mean(prev), by = age.group]


##  3) AUD SEVERITY AND COMORBIDITY
# ..............

com.dat <- unique(copy(expdat1[setting == 0 & diag != 0,.(pragmaid,year,sex,age.group,diag,elix_sum_all)]))
com.dat[, diag_num := as.numeric(as.character(diag))]
com.dat[, max := max(as.numeric(diag_num)), by = .(pragmaid,year)]
com.dat <- com.dat[diag_num == max,.(pragmaid,year,sex,age.group,diag,elix_sum_all)]
com.dat$year <- factor(com.dat$year)
com.dat[, .N, by = pragmaid][, table(N)]
com.dat$diag <- factor(com.dat$diag)
com.dat$pragmaid <- factor(com.dat$pragmaid)


### TEST DIFFERENCES IN ECS BY AUD SEVERITY WITH POISSON MODEL
# overdispersion?
mean(com.dat$elix_sum_all) > var(com.dat$elix_sum_all)

# simple glm
mod.com1 <- glm(elix_sum_all ~ sex + age.group + year + diag, data = com.dat, family = "poisson")
summary(mod.com1)

# glmm
#mod.com2 <- glmer(elix_sum_all ~ sex + age.group + year + diag + (1|pragmaid), data = com.dat, family = "poisson")
#summary(mod.com2) # not converged
#residuals <- residuals(mod.com2, type = "pearson")
#dispersion <- sum(residuals^2) / df.residual(mod.com2)
#print(dispersion)  # less than 1, no overdispersion

# gee
mod.com3 <- gee(elix_sum_all ~ sex + age.group + year + diag, 
                 data = com.dat, 
                 id = pragmaid, 
                 family = poisson, 
                 corstr = "exchangeable")
summary(mod.com3)
sjPlot::tab_model(mod.com3, 
                  robust = T,
                  file = "tabs/Supp_Table_1.html") 


##  4) AUD SEVERITY AND TREATMENT UTILISATION
# ..............

# any AUD prev by setting
prev.dat2[diag == 0 & var == "setting" & sex == "any" & age.group == "18-99", mean(prev) , by = val] # 2.9%

# rel share of AUD severity levels across settings by diag
temp <- unique(expdat1[,.(year,setting,diag,pragmaid)])[order(year,diag,setting)]
temp <- temp[,.N, by = .(year,diag,setting)][order(year,diag,setting)]
temp[setting == 0, n_sum := N]
temp[, n_sum := mean(n_sum, na.rm = T), by = .(year,diag)]
temp[, set_share := N/n_sum]
temp <- temp[,.(set_share = mean(set_share)), by = .(diag,setting)]

# within each AUD severity level: how many diagnosed in outpatient settings?
temp[setting == 1]

# within each AUD severity level: how many diagnosed in inpatient settings?
temp[setting == 2]

# within each AUD severity level: how many diagnosed in outpatient & inpatient settings?
temp[setting == 3]


##  5) Intersection of AUD severity with setting of diagnoses
# ..............

prev.dat

ses.dat[,.(.N, mean(elix_sum_all)), by = diag][order(diag)]


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 3) TABLES
# ______________________________________________________________________________________________________________________

##  1) TAB 1 - Definitions of domains
# ..............


##  2) TAB 2 - Description of study population
# ..............

tabdat <- ses.dat[,.(.N, 
                     mean(sex == "female"),
                     mean(age),
                     mean(elix_sum_all)) , by = .(diag)][order(diag)]

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 4) FIGURES
# ______________________________________________________________________________________________________________________

##  1) FIG 1 - Prevalence
# ..............

pdat <- copy(prev.dat[age.group != "18-99" & sex != "any"])
pdat$year <- factor(pdat$year)
pdat$diag <- factor(pdat$diag)
pdat$diag_lab <- factor(pdat$diag, labels = c("any","F10.0","F10.1","F10.2","F10.3+"))

ggplot(pdat, aes(x = diag_lab, y = prev, fill = year)) +
  facet_grid(sex ~ age.group) + 
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = blue_shades_5) +
  scale_x_discrete("") + 
  scale_y_continuous("",labels = scales::percent) + 
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.text.x = element_text(angle = 45, vjust = 0.5))

ggsave(filename = paste0("figs/", Sys.Date(), "_fig_1_Bar AUD severity sex age year.png"),
       width = 12, height = 6)

rm(pdat)


##  2) FIG 2 - Comorbidity
# ..............

# comorbidity

pdat <- copy(expdat1[setting == 0][order(diag)])
pdat[,min := min(year), by = .(pragmaid,diag)]
pdat <- pdat[year == min,.(pragmaid,elix_sum_all,year,diag)][order(pragmaid,diag)]
pdat$diag_lab <- factor(pdat$diag, labels = c("0 (any AUD)","1 (F10.0)","2 (F10.1)","3 (F10.2)","4 (F10.3+)"))
pdat$diag_lab_rev <- factor(pdat$diag_lab, levels = rev(levels(pdat$diag_lab)))

pdat[, mean := mean(elix_sum_all), by = diag_lab_rev]

ggplot(pdat, aes(x = elix_sum_all, y = diag_lab_rev, fill = diag_lab_rev)) +
  ggridges::geom_density_ridges() +
  scale_fill_manual("", values = rev(green_shades_6)) +
  scale_x_continuous("Elixhauser comorbidity score (0-31)") +
  scale_y_discrete("") +
  theme(legend.position = "bottom", legend.direction = "horizontal") + 
  guides(fill = guide_legend(reverse=TRUE))

ggsave(filename = paste0("figs/", Sys.Date(), "_fig_2_Ridge AUD severity comorbidity.png"),
       width = 12, height = 6)

##  reporting
pdat[, mean(elix_sum_all), by = diag_lab][order(diag_lab)]


##  2) FIG 2 - Comorbidity by setting/pattern
# ..............

# comorbidity

pdat <- copy(expdat1[diag != 0 & setting != 0,.(pragmaid,year,diag,var = "setting", val = setting,elix_sum_all)])
pdat <- rbind(pdat,
              expdat2[diag != 0 & pattern != 0,.(pragmaid,year,diag,var = "pattern", val = pattern,elix_sum_all)])

pdat[,min := min(year), by = .(pragmaid,diag,var)]
pdat <- unique(pdat[(var == "setting" & year == min) | (var == "pattern" & year == min),.(pragmaid,elix_sum_all,diag,var,val)][order(pragmaid,diag,var,val)])

##  check
pdat[pragmaid == "zy9RsMdBCr"]
expdat1[pragmaid == "zy9RsMdBCr"]

##  factor and labels
pdat$var <- factor(pdat$var, labels = c("setting","pattern"))

pdat$diag_lab <- factor(pdat$diag, labels = c("1:F10.0","2:F10.1","3:F10.2","4:F10.3+"))
pdat$diag_lab_rev <- factor(pdat$diag_lab, levels = rev(levels(pdat$diag_lab)))

pdat[, val_lab := as.character(val)]
pdat[, val_lab := fifelse(var == "setting", dplyr::recode(val_lab, "0" = "0:any setting", "1" = "1:outpatient","2" = "2:inpatient","3" = "3:outpatient&inpatient"),
                          dplyr::recode(val_lab, "0" = "0:any pattern", "1" = "1:M2D", "2" = "2:M2Q", "3" = "3:M2QF"))]
pdat$val_lab <- factor(pdat$val_lab, levels = c("1:outpatient","2:inpatient","3:outpatient&inpatient",
                                                "1:M2D","2:M2Q","3:M2QF"))

#pdat$setting_lab <- factor(pdat$setting, labels = c("1 (outpatient)","2 (inpatient)","3 (out/inpatient)"))
#pdat$setting_lab_rev <- factor(pdat$setting_lab, levels = rev(levels(pdat$setting_lab)))

ggplot(pdat, aes(x = elix_sum_all, y = diag_lab_rev, fill = val_lab)) +
  facet_grid(var ~ .) +
  ggridges::geom_density_ridges() +
  scale_fill_manual("", values = c(green_shades_6[1:3],blue_shades_6[1:3])) +
  scale_x_continuous("Elixhauser comorbidity score (0-31)") +
  scale_y_discrete("") +
  theme(legend.position = "bottom", legend.direction = "horizontal") #+ 
  #guides(fill = guide_legend(reverse=TRUE))

ggsave(filename = paste0("figs/", Sys.Date(), "_fig_3_Ridge AUD severity comorbidity by setting-pattern.png"),
       width = 10, height = 8)




# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 5) SUPPL FIGURES
# ______________________________________________________________________________________________________________________

##  1) SUPP FIG 1 - VENN AUD SEVERITY
# ..............

##  PLOT
pdat <- copy(aud.dat)
pdat$pragmaid <- NULL

png(filename = paste0("figs/", Sys.Date(), "_Suppl fig_1_Venn AUD severity.png"),width = 600, height = 600)

plot(eulerr::euler(pdat, shape = "ellipse"), main = "AUD severity (n=21,984; any setting/pattern)")

dev.off()
rm(pdat)

##  2) SUPP FIG 2 - VENN DIAG X SETTING
# ..............

##  setting type for diagnoses = any
pdat0 <- unique(expdat1[diag == 0 & setting == 1,.(pragmaid,setting_1 = T)])
pdat0 <- merge(pdat0,
               unique(expdat1[diag == 0 & setting == 2,.(pragmaid,setting_2 = T)]),
               by = "pragmaid", all = T, allow.cartesian = T)
pdat0 <- merge(pdat0,
               unique(expdat1[diag == 0 & setting == 3,.(pragmaid,setting_3 = T)]),
               by = "pragmaid", all = T, allow.cartesian = T)

pdat0 <- unique(pdat0)
pdat0$pragmaid <- NULL
pdat0[, (names(pdat0)) := lapply(.SD, function(x) ifelse(is.na(x), FALSE, x))]


##  setting type for diagnosis = F10.0
pdat1 <- unique(expdat1[diag == 1 & setting == 1,.(pragmaid,setting_1 = T)])
pdat1 <- merge(pdat1,
               unique(expdat1[diag == 1 & setting == 2,.(pragmaid,setting_2 = T)]),
               by = "pragmaid", all = T, allow.cartesian = T)
pdat1 <- merge(pdat1,
               unique(expdat1[diag == 1 & setting == 3,.(pragmaid,setting_3 = T)]),
               by = "pragmaid", all = T, allow.cartesian = T)

pdat1 <- unique(pdat1)
pdat1$pragmaid <- NULL
pdat1[, (names(pdat1)) := lapply(.SD, function(x) ifelse(is.na(x), FALSE, x))]


##  setting type for diagnosis = F10.1
pdat2 <- unique(expdat1[diag == 2 & setting == 1,.(pragmaid,setting_1 = T)])
pdat2 <- merge(pdat2,
               unique(expdat1[diag == 2 & setting == 2,.(pragmaid,setting_2 = T)]),
               by = "pragmaid", all = T, allow.cartesian = T)
pdat2 <- merge(pdat2,
               unique(expdat1[diag == 2 & setting == 3,.(pragmaid,setting_3 = T)]),
               by = "pragmaid", all = T, allow.cartesian = T)

pdat2 <- unique(pdat2)
pdat2$pragmaid <- NULL
pdat2[, (names(pdat2)) := lapply(.SD, function(x) ifelse(is.na(x), FALSE, x))]


##  setting type for diagnosis = F10.2
pdat3 <- unique(expdat1[diag == 3 & setting == 1,.(pragmaid,setting_1 = T)])
pdat3 <- merge(pdat3,
               unique(expdat1[diag == 3 & setting == 2,.(pragmaid,setting_2 = T)]),
               by = "pragmaid", all = T, allow.cartesian = T)
pdat3 <- merge(pdat3,
               unique(expdat1[diag == 3 & setting == 3,.(pragmaid,setting_3 = T)]),
               by = "pragmaid", all = T, allow.cartesian = T)

pdat3 <- unique(pdat3)
pdat3$pragmaid <- NULL
pdat3[, (names(pdat3)) := lapply(.SD, function(x) ifelse(is.na(x), FALSE, x))]


##  setting type for diagnosis = F10.3+
pdat4 <- unique(expdat1[diag == 4 & setting == 1,.(pragmaid,setting_1 = T)])
pdat4 <- merge(pdat4,
               unique(expdat1[diag == 4 & setting == 2,.(pragmaid,setting_2 = T)]),
               by = "pragmaid", all = T, allow.cartesian = T)
pdat4 <- merge(pdat4,
               unique(expdat1[diag == 4 & setting == 3,.(pragmaid,setting_3 = T)]),
               by = "pragmaid", all = T, allow.cartesian = T)

pdat4 <- unique(pdat4)
pdat4$pragmaid <- NULL
pdat4[, (names(pdat4)) := lapply(.SD, function(x) ifelse(is.na(x), FALSE, x))]

##  PLOTS
nrow(pdat0) #  21984
nrow(pdat1) #  5347
nrow(pdat2) #  11342
nrow(pdat3) #  11500
nrow(pdat4) #  7662

p0 <- plot(eulerr::euler(pdat1, shape = "ellipse"), main = "0-any (n=21,984)")
p1 <- plot(eulerr::euler(pdat1, shape = "ellipse"), main = "1-F10.0 (n=5,347)")
p2 <- plot(eulerr::euler(pdat2, shape = "ellipse"), main = "2-F10.1 (n=11,342)")
p3 <- plot(eulerr::euler(pdat3, shape = "ellipse"), main = "3-F10.2 (n=11,500)")
p4 <- plot(eulerr::euler(pdat3, shape = "ellipse"), main = "3-F10.3+ (n=7,662)")

png(filename = paste0("figs/", Sys.Date(), "_Suppl fig_2_Venn diag by setting.png"),width = 1400, height = 1200)

gridExtra::grid.arrange(p0, p1, p2, p3, p4, ncol = 3, nrow = 2)

dev.off()

##  reporting see above

rm(pdat0, pdat1, pdat2, pdat3,
   p0, p1, p2, p3)


##  4) SUPP FIG 4 - AUD SEVERITY X SETTING X AGE
# ..............

pdat <- copy(expdat1[diag != 0 & setting != 0][order(diag)])
pdat[,min := min(year), by = .(pragmaid,diag,setting)]
pdat <- pdat[year == min,.(pragmaid,diag,setting,age)][order(pragmaid,diag)]

pdat$diag_lab <- factor(pdat$diag, labels = c("1 (F10.0)","2 (F10.1)","3 (F10.2)","4 (F10.3+)"))
pdat$diag_lab_rev <- factor(pdat$diag_lab, levels = rev(levels(pdat$diag_lab)))

pdat$setting_lab <- factor(pdat$setting, labels = c("1 (outpatient)","2 (inpatient)","3 (out/inpatient)"))
pdat$setting_lab_rev <- factor(pdat$setting_lab, levels = rev(levels(pdat$setting_lab)))

ggplot(pdat, aes(x = age, y = diag_lab_rev, fill = setting_lab, color = setting_lab)) +
  ggridges::geom_density_ridges(alpha = 0.5) +
  scale_fill_manual("", values = three_colors)+
  scale_color_manual("", values = three_colors)+
  scale_x_continuous("Age") +
  scale_y_discrete("") +
  theme(legend.position = "bottom", legend.direction = "horizontal") 

ggsave(filename = paste0("figs/", Sys.Date(), "_Suppl fig_3_Ridge AUD severity setting and age.png"),
       width = 12, height = 6)

##  reporting
pdat[, mean(age), by = .(diag_lab,setting)][order(diag_lab,setting)]



##  3) SUPP FIG 3 - VENN DIAG X PATTERN
# ..............


!!! REVISE BASED ON SETTING !!!

##  diagnosis type for pattern = m1d
pdat0 <- expdat2[year == 2019 & pattern == 0 & diag == 1,.(pragmaid,f10_0 = T)]
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
pdat1 <- expdat2[year == 2019 & pattern == 1 & diag == 1,.(pragmaid,f10_0 = T)]
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
pdat2 <- expdat2[year == 2019 & pattern == 2 & diag == 1,.(pragmaid,f10_0 = T)]
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
pdat3 <- expdat2[year == 2019 & pattern == 3 & diag == 1,.(pragmaid,f10_0 = T)]
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

p0 <- plot(eulerr::euler(pdat0, shape = "ellipse"), main = "0-m1d (n=11,748)")
p1 <- plot(eulerr::euler(pdat1, shape = "ellipse"), main = "1-m2d (n=8,455)")
p2 <- plot(eulerr::euler(pdat2, shape = "ellipse"), main = "2-m2q (n=8,053)")
p3 <- plot(eulerr::euler(pdat3, shape = "ellipse"), main = "3-m2qf (n=6,798)")

png(filename = paste0("figs/", Sys.Date(), "_Suppl fig_3_Venn diag by pattern.png"),width = 1400, height = 1200)

gridExtra::grid.arrange(p0, p1, p2, p3, ncol = 2, nrow = 2)

dev.off()

##  reporting

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


