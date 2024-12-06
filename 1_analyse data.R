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
DATE2 <- "2024-12-06"
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
five_colors <- c("#FF7F0E", "#2CA02C", "#1F77B4", "#9467BD", "#8C564B")

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 1) LOAD AND COMBINE DATA
# ______________________________________________________________________________________________________________________

##  prepared data
pop.dat <- readRDS(paste0(inpath, DATE2, "_insurance_population.RDS"))
diag.dat <- readRDS(paste0(inpath, DATE2, "_alcohol diagnoses data.RDS"))
expdat <- readRDS(paste0(inpath, DATE2, "_exposure data_type and setting.RDS"))
elixdat <- readRDS(paste0(inpath, DATE2, "_elixhauser data.RDS"))

##  remove setting information
expdat <- expdat[setting == 0,.(pragmaid,year,gkv,diag,sex,yob,age,age.group,emp.type,elix_sum_all)]

##  unique AUD dat across all years
aud.dat <- unique(expdat[diag == 1,.(pragmaid,f10_0 = T)])
aud.dat <- merge(aud.dat,
              unique(expdat[diag == 2,.(pragmaid,f10_1 = T)]), 
              by = "pragmaid", all = T, allow.cartesian = T)
aud.dat <- merge(aud.dat,
              unique(expdat[diag == 3,.(pragmaid,f10_2 = T)]), 
              by = "pragmaid", all = T, allow.cartesian = T)
aud.dat <- merge(aud.dat,
              unique(expdat[diag == 4,.(pragmaid,f10_3 = T)]), 
              by = "pragmaid", all = T, allow.cartesian = T)
aud.dat <- merge(aud.dat,
                 unique(expdat[diag == 5,.(pragmaid,k70 = T)]), 
                 by = "pragmaid", all = T, allow.cartesian = T)

aud.dat <- unique(aud.dat)
nrow(aud.dat) #  21954
aud.dat[, (names(aud.dat)) := lapply(.SD, function(x) ifelse(is.na(x), FALSE, x))]

##  prev.dat
prev.dat <- expdat[,.(n = .N), by = .(year,diag,sex,age.group)][order(year,diag)]
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
  
##  ses.dat: only one person per diag - first year
ses.dat <- copy(expdat[][order(diag)])
ses.dat[,min := min(year), by = .(pragmaid,diag)]
ses.dat <- unique(ses.dat[year == min,.(pragmaid,year,diag,sex,age,age.group,emp.type,elix_sum_all)][order(pragmaid,diag)])
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
length(unique(expdat$pragmaid)) # 21954

# diag type table:
melt(aud.dat)[, table(variable, value)]
unique(ses.dat[, .(pragmaid,diag)])[, table(diag)]
unique(ses.dat[, .(pragmaid,diag)])[, prop.table(table(diag))]

unique(expdat[year == 2017, .(pragmaid,diag)])[, table(diag)] # lead: 3
unique(expdat[year == 2018, .(pragmaid,diag)])[, table(diag)] # lead: 3
unique(expdat[year == 2019, .(pragmaid,diag)])[, table(diag)] # lead: 3
unique(expdat[year == 2020, .(pragmaid,diag)])[, table(diag)] # lead: 3
unique(expdat[year == 2021, .(pragmaid,diag)])[, table(diag)] # lead: 3

# overlaps
aud.dat[f10_0 == T, sum(f10_1 == T | f10_2 == T | f10_3 == T | k70 == T)/.N] # 59.7%
aud.dat[f10_1 == T, sum(f10_0 == T | f10_2 == T | f10_3 == T | k70 == T)/.N] # 55.2%
aud.dat[f10_2 == T, sum(f10_0 == T | f10_1 == T | f10_3 == T | k70 == T)/.N] # 64.8%
aud.dat[f10_3 == T, sum(f10_0 == T | f10_1 == T | f10_2 == T | k70 == T)/.N] # 94.4%
aud.dat[k70 == T, sum(f10_0 == T | f10_1 == T | f10_2 == T | f10_3 == T)/.N] # 69.0%

sum(aud.dat$f10_0 & aud.dat$f10_1) / nrow(aud.dat) # 10.3%
sum(aud.dat$f10_0 & aud.dat$f10_2) / nrow(aud.dat) # 12.0%
sum(aud.dat$f10_0 & aud.dat$f10_3) / nrow(aud.dat) # 6.6%
sum(aud.dat$f10_0 & aud.dat$k70) / nrow(aud.dat) # 5.4%
sum(aud.dat$f10_1 & aud.dat$f10_2) / nrow(aud.dat) # 23.4%
sum(aud.dat$f10_1 & aud.dat$f10_3) / nrow(aud.dat) # 8.7%
sum(aud.dat$f10_1 & aud.dat$k70) / nrow(aud.dat) # 12.3%
sum(aud.dat$f10_2 & aud.dat$f10_3) / nrow(aud.dat) # 11.9%
sum(aud.dat$f10_2 & aud.dat$k70) / nrow(aud.dat) # 15.6%
sum(aud.dat$f10_3 & aud.dat$k70) / nrow(aud.dat) # 6.2%

##  2) ADMINISTRATIVE PREVALENCE OF AUD SEVERITY
# ..............

# manual
expdat[diag == 0 & year == 2017, length(unique(pragmaid))] / 
  pop.dat[year == 2017, sum(pop)] # 2.9%

# from prev.dat
prev.dat[sex == "any" & age.group == "18-99" & diag == 0] # 2.9% to 2.7%
prev.dat[sex == "any" & age.group == "18-99" & diag != 0, mean(prev), by = diag] # mean % by diag across years
prev.dat[sex == "any" & age.group == "18-24" & diag != 0, mean(prev), by = diag] # mean % by diag across years among 18-24
prev.dat[age.group == "18-99" & diag == 0, mean(prev), by = sex]
prev.dat[sex == "any" & age.group != "18-99" & diag == 0, mean(prev), by = age.group]


##  3) AUD SEVERITY AND COMORBIDITY
# ..............

com.dat <- unique(copy(expdat[diag != 0,.(pragmaid,year,sex,age.group,emp.type,diag,elix_sum_all)]))
com.dat[, diag_num := as.numeric(as.character(diag))]
com.dat[, max := max(as.numeric(diag_num)), by = .(pragmaid,year)]
com.dat <- com.dat[diag_num == max,.(pragmaid,year,sex,age.group,emp.type,diag,elix_sum_all)]
com.dat$year <- factor(com.dat$year)
com.dat[, .N, by = pragmaid][, table(N)]
com.dat$diag <- factor(com.dat$diag)
com.dat$pragmaid <- factor(com.dat$pragmaid)


### TEST DIFFERENCES IN ECS BY AUD SEVERITY WITH POISSON MODEL
# overdispersion?
mean(com.dat$elix_sum_all) > var(com.dat$elix_sum_all)

# simple glm
mod.com1 <- glm(elix_sum_all ~ sex + age.group + emp.type + year + diag, data = com.dat, family = "poisson")
summary(mod.com1)

# glmm
#mod.com2 <- glmer(elix_sum_all ~ sex + age.group + year + diag + (1|pragmaid), data = com.dat, family = "poisson")
#summary(mod.com2) # not converged
#residuals <- residuals(mod.com2, type = "pearson")
#dispersion <- sum(residuals^2) / df.residual(mod.com2)
#print(dispersion)  # less than 1, no overdispersion

# gee
mod.com3 <- gee(elix_sum_all ~ sex + age.group + emp.type + year + diag, 
                 data = com.dat, 
                 id = pragmaid, 
                 family = poisson, 
                 corstr = "exchangeable")
summary(mod.com3)
sjPlot::tab_model(mod.com3, 
                  robust = T,
                  file = paste0("tabs/", Sys.Date(), "_supp table_1.html"))


# diseases:
elixdat
temp <- copy(ses.dat[,.(pragmaid,year,diag)])
temp <- merge(temp, elixdat, 
              by = c("pragmaid","year"), all.x = T) 

# no comorbidity except alcohol?
nrow(temp[diag == 1 & disease != "Alcohol abuse", sum(elix), by = pragmaid][V1 == 0])/5324 # 21%
nrow(temp[diag == 2 & disease != "Alcohol abuse", sum(elix), by = pragmaid][V1 == 0])/11332 # 9%
nrow(temp[diag == 3 & disease != "Alcohol abuse", sum(elix), by = pragmaid][V1 == 0])/11478 # 7%
nrow(temp[diag == 4 & disease != "Alcohol abuse", sum(elix), by = pragmaid][V1 == 0])/3016 # 5%
nrow(temp[diag == 5 & disease != "Alcohol abuse", sum(elix), by = pragmaid][V1 == 0])/6003 # 3%
  
# percentages
temp[, N:=length(unique(pragmaid)), by = diag]
temp[, perc := sum(elix==1)/N,by = .(diag,disease)]

temp <- unique(temp[,.(diag,N,disease,perc)])
temp[diag == 0][order(perc)]
temp[diag == 1][order(perc)]
temp[diag == 2][order(perc)]
temp[diag == 3][order(perc)]
temp[diag == 4][order(perc)]
temp[diag == 5][order(perc)]

rm(temp)

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
                     sex = mean(sex == "female"),
                     age = paste0(round(mean(age),1),"(",
                            round(quantile(age,0.25)),"-",
                            round(quantile(age,0.75)),")"),
                     employed = mean(emp.type == "employed"),
                     unemployed = mean(emp.type == "unemployed"),
                     retired = mean(emp.type == "retired"),
                     other = mean(emp.type == "other"),
                     ecs = paste0(round(mean(elix_sum_all),1),"(",
                            round(quantile(elix_sum_all,0.25)),"-",
                            round(quantile(elix_sum_all,0.75)),")")), by = .(diag)][order(diag)]

tabdat <- t(tabdat)

write.csv(tabdat, 
          file = paste0("tabs/", Sys.Date(), "_table_1_description.csv"))


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 4) FIGURES
# ______________________________________________________________________________________________________________________

##  1) FIG 1 - VENN AUD SEVERITY
# ..............

##  PLOT
pdat <- copy(aud.dat)
pdat$pragmaid <- NULL
sum(pdat$k70)

names(pdat) <- c("1:F10.0\n(n=5,324)",
                 "2:F10.1\n(n=11,332)",
                 "3:F10.2\n(n=11,478)",
                 "4:F10.3\n(n=3,016)",
                 "5:K70+\n(n=6,003)")

png(filename = paste0("figs/", Sys.Date(), "_fig_1_Venn AUD severity.png"),width = 1200, height = 1200)

plot(eulerr::euler(pdat, shape = "ellipse"), 
     fills = five_colors,
     labels = list(cex = 2))

dev.off()
rm(pdat)


##  2) FIG 2 - Prevalence
# ..............

pdat <- copy(prev.dat[age.group != "18-99" & sex != "any"])
pdat$year <- factor(pdat$year)
pdat$diag <- factor(pdat$diag)
pdat$diag_lab <- factor(pdat$diag, labels = c("0:any","1:F10.0","2:F10.1","3:F10.2","4:F10.3","5:K70+"))

ggplot(pdat, aes(x = diag_lab, y = prev, fill = year)) +
  facet_grid(sex ~ age.group) + 
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = blue_shades_5) +
  scale_x_discrete("") + 
  scale_y_continuous("",labels = scales::percent) + 
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.text.x = element_text(angle = 45, vjust = 0.5))

ggsave(filename = paste0("figs/", Sys.Date(), "_fig_2_Bar AUD severity sex age year.png"),
       width = 12, height = 6)

rm(pdat)


##  3) FIG 3 - Comorbidity heatmap
# ..............

pdat <- copy(ses.dat[,.(pragmaid,year,diag)])
pdat <- merge(pdat, elixdat, 
              by = c("pragmaid","year"), all.x = T) 
pdat[, .N, by = .(pragmaid,diag)][, table(N)] ## all 31 observations within each diag!

pdat[, n_aud := length(unique(pragmaid)), by = diag]

pdat2 <- unique(pdat[, .(percentage = sum(elix == 1)/n_aud,n_aud), by = .(diag,disease,disease_nr)])

pdat2$label <- factor(pdat2$disease, levels = rev(levels(pdat2$disease)))
pdat2[, table(diag,n_aud)]
pdat2$group <- factor(pdat2$diag, labels = c("0:any\n(n=21,954)",
                                             "1:F10.0\n(n=5,324)",
                                             "2:F10.1\n(n=11,332)",
                                             "3:F10.2\n(n=11,478)",
                                             "4:F10.3\n(n=3,016)",
                                             "5:K70+\n(n=6,003)"))

ggplot(pdat2, aes(x = group, y = label, fill = percentage)) +
  geom_tile(show.legend = F) + 
  geom_text(aes(label = scales::percent(percentage, accuracy = 1)), color = "black") +
  scale_fill_gradient(low = "#f2ff00", high = "#ff3c00") +
  scale_x_discrete("") +
  scale_y_discrete("")
  
ggsave(filename = paste0("figs/", Sys.Date(), "_fig_3_heatmap AUD severity and Elix.png"),
       width = 12, height = 8)



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 5) SUPPL FIGURES
# ______________________________________________________________________________________________________________________

##  1) SUPP FIG 1 - TYPE OF DIAG IN AUD LEVEL 5
# ..............

# get only AUD level 5 diagnoses from those concerned:
select <- unique(expdat[diag == 5]$pragmaid)
alc.diag_5 <- c("F10.5","F10.6","F10.7","F10.8","F10.9",
                "E24.4","G31.2","G62.1","G72.1","I42.6",
                "K29.2","K70.0","K70.1","K70.2","K70.3","K70.4","K70.9","K85.2","K85.20","K86.0")

pdat <- unique(diag.dat[pragmaid %in% select & icd %in% alc.diag_5,.(pragmaid,icd)])
pdat[, length(unique(pragmaid))] # 6003

# collapse diagnoses
pdat[, table(icd)]
pdat[, icd_group := icd]
pdat[, icd_group := ifelse(icd_group %like% "F10","F10.5-9",
                           ifelse(icd_group %like% "K70","K70.1-9",icd_group))]
pdat[, table(icd,icd_group)]
pdat <- unique(pdat[,.(pragmaid,icd_group)])

# collapse combinations
pdat <- pdat[order(pragmaid,icd_group)]
pdat <- pdat[, .(icd_group = paste0(icd_group, collapse = "&")), by = pragmaid]

pdat <- pdat[, .N, by = icd_group]

# mosaic plot
require( treemapify )

ggplot(pdat, aes(area = N)) +
  geom_treemap(fill = green_shades_5[5]) +
  geom_treemap_text(aes(label = icd_group),
                    color = "white")

ggsave(filename = paste0("figs/", Sys.Date(), "_Suppl fig_1_Diagnoses AUD level 5.png"),
       width = 10, height = 6)

pdat[order(N, decreasing = T),.(icd_group,prop = N/sum(N))][1:10]

rm(pdat)

##  2) SUPP FIG 2 - COMORBIDITY X PATTERN/SETTING
# ..............

pdat <- copy(ses.dat[diag != 0,.(pragmaid,year,diag)])
pdat <- merge(pdat, unique(elixdat[,.(pragmaid,year,elix_sum_all)]), 
              by = c("pragmaid","year"), all.x = T) 
pdat[, .N, by = .(pragmaid,diag)][, table(N)] ## all 1 observation within each diag!

pdat$diag_lab <- factor(pdat$diag, labels = c("1:F10.0","2:F10.1","3:F10.2","4:F10.3","5:K70+"))
pdat$diag_lab_rev <- factor(pdat$diag_lab, levels = rev(levels(pdat$diag_lab)))

ggplot(pdat, aes(x = diag_lab, y = elix_sum_all, fill = diag_lab)) +
  geom_jitter(alpha = 0.01, show.legend = F) + 
  geom_boxplot(show.legend = F) +
  scale_fill_manual("", values = c(green_shades_6[1:5])) +
  scale_x_discrete("") +
  scale_y_continuous("Elixhauser comorbidity score (0-31)")

ggsave(filename = paste0("figs/", Sys.Date(), "_Suppl fig_2_AUD severity comorbidity by diag.png"),
       width = 10, height = 6)


