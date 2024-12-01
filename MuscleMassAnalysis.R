### This is the code used in analyzing various factors and the muscle mass


# set working directory
setwd("C:/Users/user/Desktop/Osteoporosis_DataAnalysis/KoGES - 3rd")


# remove history
rm()

#download csv files
base_data1 <- read.csv(file = "AS3_01_EXAMINEE.csv.gz", header = TRUE)
base_data2 <- read.csv(file = "AS3_03_DRSM.csv.gz", header = TRUE, fileEncoding = "ISO-8859-1")
base_data3 <- read.csv(file = "AS3_04_ACTIVE.csv.gz", header = TRUE)
base_data4 <- read.csv(file = "AS3_09_DRUG.csv.gz", header = TRUE)
base_data5 <- read.csv(file = "AS3_11_FEMD.csv.gz", header = TRUE, fileEncoding = "ISO-8859-1")
base_data6 <- read.csv(file = "AS3_19_FFQNUTRI.csv.gz", header = TRUE)
base_data7 <- read.csv(file = "AS1_11_BREATH.csv.gz", header = TRUE, fileEncoding = "ISO-8859-1")
base_data8 <- read.csv(file = "AS1_19_ANTHRO.csv.gz", header = TRUE)
base_data9 <- read.csv(file = "AS1_03_DRSM.csv.gz", header = TRUE, fileEncoding = "ISO-8859-1")
base_data10 <- read.csv(file = "AS7_33_ANTHRO.csv.gz", header = TRUE)
base_data11 <- read.csv(file = "AS7_03_HABIT.csv.gz", header = TRUE, fileEncoding = "ISO-8859-1")



#MM2_4omics: Check omics data from 9351 samples
MM2_4omics <- read.csv(gzfile("MM2_4omics.txt.gz"), header = TRUE, sep = "\t")
head(MM2_4omics)


# sort out the variables to be used
install.packages("dplyr")      # install package
library(dplyr)                 # load package

select1 <- base_data1 %>% dplyr::select(DIST_ID, AS3_SEX, AS3_AGE)
select2 <- base_data2 %>% dplyr::select(DIST_ID, AS3_DRINK, AS3_DRDU, AS3_TOTALC, AS3_SMOKE, AS3_SMDU, AS3_SMAM, AS3_SMIN, AS3_PACKYR)
select3 <- base_data3 %>% dplyr::select(DIST_ID, AS3_EXER, AS3_EXERFQ, AS3_EXERDU)

# estrogen
select4 <- base_data4 %>% dplyr::select(DIST_ID, AS3_DRUGF, AS3_DRUGFFH, AS3_DRUGFFHAM, AS3_DRUGFFHFQ, AS3_DRUGFFHCU) 
# supplements 
select5 <- base_data4 %>% dplyr::select(DIST_ID, AS3_SUPPL, AS3_CA, AS3_CAAM, AS3_VITC, AS3_VITCAM) 

select6 <- base_data5 %>% dplyr::select(DIST_ID, AS3_MENYN, AS3_PMAG, AS3_MNSAG)
select7 <- base_data6 %>% dplyr::select(DIST_ID, AS3_B02, AS3_B03, AS3_B04, AS3_B05, AS3_B14, AS3_B23)
select8 <- base_data7 %>% dplyr::select(DIST_ID, AS1_CHEMJOB, AS1_CHEMJOBDU, AS1_CHEMK, AS1_DUSTJOB, AS1_DUSTJOBDU, AS1_DUSTK)

# Muscle mass related variables
select9 <- base_data8 %>% dplyr::select(DIST_ID, AS1_BDCOTWT, AS1_WEIGHT, AS1_HEIGHT, AS1_BMI, AS1_BDCEXFT, AS1_BDCINWT, AS1_BDCOTWT, AS1_BDCWT)
select10 <- base_data9 %>% dplyr::select(DIST_ID, AS1_DRDUA, AS1_TOTALC, AS1_SMOKEA, AS1_PACKYR)
select11 <- base_data10 %>% dplyr::select(DIST_ID, AS7_IB1_3, AS7_IB1_4, AS7_EXCELL, AS7_IB1_2, AS7_INCELL)
select12 <- base_data11 %>% dplyr::select(DIST_ID, AS7_DRDU, AS7_TOTALC, AS7_SMOKE, AS7_SMAM) 


# merge the tables
merge1 <- dplyr::left_join(select1, select2)
merge2 <- dplyr::left_join(merge1, select3)
merge3 <- dplyr::left_join(merge2, select4)
merge4 <- dplyr::left_join(merge3, select5)
merge5 <- dplyr::left_join(merge4, select6)
merge6 <- dplyr::left_join(merge5, select7)
merge7 <- dplyr::left_join(merge6, select8)
merge8 <- dplyr::left_join(merge7, select9)
merge9 <- dplyr::left_join(merge8, select10)
merge10 <- dplyr::left_join(merge9, select11)
merge11 <- dplyr::left_join(merge10, select12)

base_data <- merge11

# Quality Control (exclude rows with empty fields)
base_data_null <- base_data
base_data_null[base_data_null == 66666 | base_data_null == 77777 | base_data_null == 99999] <- NA



######################
# Change Variable Type
base_data_type <- base_data_null

base_data_type$AS3_DRUGF <- as.factor(base_data_type$AS3_DRUGF) # numerical -> factor
base_data_type$AS3_DRUGFFH <- as.factor(base_data_type$AS3_DRUGFFH)
base_data_type$AS3_DRUGFFHAM <- as.factor(base_data_type$AS3_DRUGFFHAM)
base_data_type$AS3_DRUGFFHCU <- as.factor(base_data_type$AS3_DRUGFFHCU)


# variable type change (for supplements)
base_data_type$AS3_SUPPL <- as.factor(base_data_type$AS3_SUPPL)
base_data_type$AS3_CA <- as.factor(base_data_type$AS3_CA)
base_data_type$AS3_CAAM <- as.factor(base_data_type$AS3_CAAM)
base_data_type$AS3_VITC <- as.factor(base_data_type$AS3_VITC)
base_data_type$AS3_VITCAM <- as.factor(base_data_type$AS3_VITCAM)

base_data_type$AS3_SEX <- as.factor(base_data_type$AS3_SEX)
base_data_type$AS3_MENYN <- as.factor(base_data_type$AS3_MENYN)
base_data_type$AS3_EXER <- as.factor(base_data_type$AS3_EXER)

base_data_type$AS1_CHEMJOB <- as.factor(base_data_type$AS1_CHEMJOB)
base_data_type$AS1_CHEMK <- as.factor(base_data_type$AS1_CHEMK)
base_data_type$AS1_DUSTJOB <- as.factor(base_data_type$AS1_DUSTJOB)
base_data_type$AS1_DUSTK <- as.factor(base_data_type$AS1_DUSTK)

base_data_type$AS1_DRDUA <- as.factor(base_data_type$AS1_DRDUA)
base_data_type$AS1_SMOKEA <- as.factor(base_data_type$AS1_SMOKEA)
base_data_type$AS1_DUSTJOB <- as.factor(base_data_type$AS7_DRDU)
base_data_type$AS1_DUSTK <- as.factor(base_data_type$AS7_SMOKE)







##################
# New Variables
##################

# 1st
base_data_weight1 <- base_data_type %>% dplyr::mutate(MMI1 = AS1_BDCOTWT / AS1_WEIGHT)       # unit: kg
base_data_height1 <- base_data_weight1 %>% dplyr::mutate(MMI2 = AS1_BDCOTWT / AS1_HEIGHT ** 2)  # unit: m
base_data_BMI1 <- base_data_height1 %>% dplyr::mutate(MMI3 = AS1_BDCOTWT / AS1_BMI)  
base_data_weight2 <- base_data_BMI1 %>% dplyr::mutate(FFMI1 = AS1_BDCEXFT / AS1_WEIGHT)      # unit: kg
base_data_height2 <- base_data_weight2 %>% dplyr::mutate(FFMI2 = AS1_BDCEXFT / AS1_HEIGHT ** 2)
base_data_BMI2 <- base_data_height2 %>% dplyr::mutate(FFMI3 = AS1_BDCEXFT / AS1_BMI)
base_data_ECW <- base_data_BMI2 %>% dplyr::mutate(ECW_TW = AS1_BDCINWT / AS1_BDCWT)
base_data_ICW <- base_data_ECW %>% dplyr::mutate(ICW_TW = AS1_BDCOTWT / AS1_BDCWT)

base_data_smoke1 <- base_data_ICW %>% dplyr::mutate(AS1_SMOKE_Status = ifelse(AS1_SMOKEA == 0, "Group A",
                                                                             ifelse(AS1_SMOKEA == 1 & AS1_SMOKEA == 2, "Group B",
                                                                                    ifelse(AS1_SMOKEA == 3, "Group C", NA))))

base_data_aldura1 <- base_data_smoke1 %>% dplyr::mutate(AS1_AL_DURA = ifelse(is.na(AS1_DRDUA), "Group A",
                                                                       ifelse(AS1_DRDUA >= 2 & AS1_DRDUA <= 4, "Group B",
                                                                              ifelse(AS1_DRDUA == 5, "Group C", NA))))

base_data_packyr1 <- base_data_aldura1 %>% dplyr::mutate(AS1_PACKYR_Cont = ifelse(is.na(AS1_PACKYR), 0, AS1_PACKYR))

base_data_null <- base_data_packyr1 %>% dplyr::mutate(AS1_AL_TOTALC_Cont = ifelse(is.na(AS1_TOTALC), 0, AS1_TOTALC))


# grouping top 30% and bottom 30%
base_data_null$MMI1_group <- ifelse(base_data_null$MMI1< quantile(base_data_null$MMI1, probs = c(0.3)), "M_B",
                                    ifelse(base_data_null$MMI1 > quantile(base_data_null$MMI1, probs = c(0.7)), "M_T", "M_M"))
base_data_null$MMI2_group <- ifelse(base_data_null$MMI2 < quantile(base_data_null$MMI2, probs = c(0.3)), "M_B",
                                    ifelse(base_data_null$MMI2 > quantile(base_data_null$MMI2, probs = c(0.7)), "M_T", "M_M"))
base_data_null$MMI3_group <- ifelse(base_data_null$MMI3 < quantile(base_data_null$MMI3, probs = c(0.3)), "M_B",
                                    ifelse(base_data_null$MMI3 > quantile(base_data_null$MMI3, probs = c(0.7)), "M_T", "M_M"))



# 7th
base_data_null <- base_data_null %>% dplyr::mutate(MMI1_7 = AS7_IB1_3 / AS1_WEIGHT)        # unit: kg
base_data_null <- base_data_null %>% dplyr::mutate(MMI2_7 = AS7_IB1_3 / AS1_HEIGHT ** 2)  # unit: m
base_data_null <- base_data_null %>% dplyr::mutate(MMI3_7 = AS7_IB1_3 / AS1_BMI)  
base_data_null <- base_data_null %>% dplyr::mutate(FFMI1_7 = AS7_IB1_4 / AS1_WEIGHT)      # unit: kg
base_data_null <- base_data_null %>% dplyr::mutate(FFMI2_7 = AS7_IB1_4 / AS1_HEIGHT ** 2)
base_data_null <- base_data_null %>% dplyr::mutate(FFMI3_7 = AS7_IB1_4 / AS1_BMI)
base_data_null <- base_data_null %>% dplyr::mutate(ECW_TW_7 = AS7_EXCELL / AS7_IB1_2)
base_data_null <- base_data_null %>% dplyr::mutate(ICW_TW_7 = AS7_INCELL / AS7_IB1_2)

base_data_null <- base_data_null %>% dplyr::mutate(AS7_SMOKE_Status = ifelse(AS7_SMOKE == 0, "Group A",
                                                                             ifelse(AS7_SMOKE == 1 & AS7_SMOKE == 2, "Group B",
                                                                                    ifelse(AS7_SMOKE == 3, "Group C", NA))))

base_data_null <- base_data_null %>% dplyr::mutate(AS7_AL_DURA = ifelse(is.na(AS7_DRDU), "Group A",
                                                                        ifelse(AS7_DRDU >= 2 & AS7_DRDU <= 4, "Group B",
                                                                               ifelse(AS7_DRDU == 5, "Group C", NA))))

base_data_null <- base_data_null %>% dplyr::mutate(AS7_PACKYR_Cont = ifelse(is.na(AS7_SMAM), 0, AS7_SMAM))


base_data_final <- base_data_null %>% dplyr::mutate(AL7_AL_TOTAL_Cont = ifelse(is.na(AS7_TOTALC), 0, AS7_TOTALC))


# grouping top 30% and bottom 30%
base_data_null$MMI1_7_group <- ifelse(base_data_null$MMI1_7< quantile(base_data_null$MMI1_7, probs = c(0.3)), "M_B",
                                    ifelse(base_data_null$MMI1_7 > quantile(base_data_null$MMI1_7, probs = c(0.7)), "M_T", "M_M"))
base_data_null$MMI2_7_group <- ifelse(base_data_null$MMI2_7 < quantile(base_data_null$MMI2_7, probs = c(0.3)), "M_B",
                                    ifelse(base_data_null$MMI2_7 > quantile(base_data_null$MMI2_7, probs = c(0.7)), "M_T", "M_M"))
base_data_null$MMI3_7_group <- ifelse(base_data_null$MMI3_7 < quantile(base_data_null$MMI3_7, probs = c(0.3)), "M_B",
                                    ifelse(base_data_null$MMI3_7 > quantile(base_data_null$MMI3_7, probs = c(0.7)), "M_T", "M_M"))


# final dataset save / check
save(base_data_final, file = "base_data_final.RData") # save RData
str(base_data_final)
head(base_data_final)


############################
# Linear Regression Analysis


# AS3_B14, MMI1 (Vitamin C intake / MMI)
base_data_type$AS3_B14 <- as.numeric(base_data_type$AS3_B14)
base_data_final$MMI1 <- as.numeric(base_data_final$MMI1)

reg.vitc <- lm(MMI1 ~ AS3_B14, data = base_data_final)
summary(reg.vitc)

plot(MMI1 ~ AS3_B14, data = base_data_final)
abline(coef(reg.vitc))


# AS3_B02, MMI1 (Protein intake / MMI)
base_data_final$AS3_B02 <- as.numeric(base_data_final$AS3_B02)

reg.protein <- lm(MMI1 ~ AS3_B02, data = base_data_final)
summary(reg.protein)

plot(MMI1 ~ AS3_B02, data = base_data_final)
abline(coef(reg.protein))




# AS3_B03, MMI1 (Fat intake / MMI)
base_data_final$AS3_B03 <- as.numeric(base_data_final$AS3_B03)

reg.fat <- lm(MMI1 ~ AS3_B03, data = base_data_final)
summary(reg.fat)

plot(MMI1 ~ AS3_B03, data = base_data_final)
abline(coef(reg.fat))



# AS3_B04, MMI1 (Carb intake / MMI)
base_data_final$AS3_B04 <- as.numeric(base_data_final$AS3_B04)

reg.carb <- lm(MMI1 ~ AS3_B04, data = base_data_final)
summary(reg.carb)

plot(MMI1 ~ AS3_B04, data = base_data_final)
abline(coef(reg.carb))


# AS3_B23, MMI1 (VitE intake / MMI)
base_data_final$AS3_B23 <- as.numeric(base_data_final$AS3_B23)

reg.vite <- lm(MMI1 ~ AS3_B23, data = base_data_final)
summary(reg.vite)

plot(MMI1 ~ AS3_B23, data = base_data_final)
abline(coef(reg.vite))



###############################
## Cross table analysis / Chi-squared test
# supplement vs muscle mass
gmodels::CrossTable(base_data_null$MMI1_group, base_data_null$AS3_SUPPL) 
chisq.test(base_data_null$MMI1_group, base_data_null$AS3_SUPPL)

gmodels::CrossTable(base_data_null$MMI2_group, base_data_null$AS3_SUPPL) 
chisq.test(base_data_null$MMI2_group, base_data_null$AS3_SUPPL)

gmodels::CrossTable(base_data_null$MMI3_group, base_data_null$AS3_SUPPL) 
chisq.test(base_data_null$MMI1_group, base_data_null$AS3_SUPPL)


# exercise frequency vs muscle mass
gmodels::CrossTable(base_data_null$MMI1_group, base_data_null$AS3_EXERFQ) 
chisq.test(base_data_null$MMI1_group, base_data_null$AS3_EXERFQ)

gmodels::CrossTable(base_data_null$MMI2_group, base_data_null$AS3_EXERFQ) 
chisq.test(base_data_null$MMI2_group, base_data_null$AS3_EXERFQ)

gmodels::CrossTable(base_data_null$MMI3_group, base_data_null$AS3_EXERFQ) 
chisq.test(base_data_null$MMI1_group, base_data_null$AS3_EXERFQ)

# XXX <- chisq.test(base_data_null$MMI1_group, base_data_null$AS3_EXERFQ)
# XXX$p.value
# XXX$statistic


# drink Y/N vs muscle mass
gmodels::CrossTable(base_data_null$MMI1_group, base_data_null$AS3_DRINK) 
chisq.test(base_data_null$MMI1_group, base_data_null$AS3_DRINK)

gmodels::CrossTable(base_data_null$MMI2_group, base_data_null$AS3_DRINK) 
chisq.test(base_data_null$MMI2_group, base_data_null$AS3_DRINK)

gmodels::CrossTable(base_data_null$MMI3_group, base_data_null$AS3_DRINK) 
chisq.test(base_data_null$MMI1_group, base_data_null$AS3_DRINK)



# smoke Y/N vs muscle mass
gmodels::CrossTable(base_data_null$MMI1_group, base_data_null$AS3_SMOKE) 
chisq.test(base_data_null$MMI1_group, base_data_null$AS3_SMOKE)

gmodels::CrossTable(base_data_null$MMI2_group, base_data_null$AS3_SMOKE) 
chisq.test(base_data_null$MMI2_group, base_data_null$AS3_SMOKE)

gmodels::CrossTable(base_data_null$MMI3_group, base_data_null$AS3_SMOKE) 
chisq.test(base_data_null$MMI1_group, base_data_null$AS3_SMOKE)




PVAL <- data.frame()
PVAL[1,1] <- chisq.test(base_data_null$MMI1_group, base_data_null$AS3_SUPPL)$p.value
PVAL[2,1] <- chisq.test(base_data_null$MMI2_group, base_data_null$AS3_SUPPL)$p.value
PVAL[3,1] <- chisq.test(base_data_null$MMI1_group, base_data_null$AS3_SUPPL)$p.value
PVAL[1,2] <- chisq.test(base_data_null$MMI1_group, base_data_null$AS3_EXERFQ)$p.value
PVAL[2,2] <- chisq.test(base_data_null$MMI2_group, base_data_null$AS3_EXERFQ)$p.value
PVAL[3,2] <- chisq.test(base_data_null$MMI3_group, base_data_null$AS3_EXERFQ)$p.value
PVAL[1,3] <- chisq.test(base_data_null$MMI1_group, base_data_null$AS3_DRINK)$p.value
PVAL[2,3] <- chisq.test(base_data_null$MMI2_group, base_data_null$AS3_DRINK)$p.value
PVAL[3,3] <- chisq.test(base_data_null$MMI3_group, base_data_null$AS3_DRINK)$p.value
PVAL[1,4] <- chisq.test(base_data_null$MMI1_group, base_data_null$AS3_SMOKE)$p.value
PVAL[2,4] <- chisq.test(base_data_null$MMI2_group, base_data_null$AS3_SMOKE)$p.value
PVAL[3,4] <- chisq.test(base_data_null$MMI1_group, base_data_null$AS3_SMOKE)$p.value






fisher1 <- fisher.test(table(base_data_null$MMI1_group, base_data_null$AS3_SUPPL))
fisher1

ttest1<- t.test(AS3_SUPPL ~ MMI1_group, data = base_data_null, na.action = na.omit)
ttest1


install.packages("ggplot2")
library(ggplot2)
library(reshape2)



base_data_null <- base_data_null %>% dplyr::mutate(AS1_DRDUA = ifelse(is.na(AS1_DRDUA), 0, AS1_DRDUA))
base_data_null$AS1_DRDUA <- as.factor(base_data_null$AS1_DRDUA)

table.AS1_DRDUA <- descr::freq(base_data_null$AS1_DRDUA) # estrogen Y/N 
round(table.AS1_DRDUA, digits = 2)

# Boxplot of MMI1 by AS1_DRDUA (drink duration)
DRDU1 <- ggplot(base_data_null, aes(x = AS1_DRDUA, y = MMI1, fill = AS1_DRDUA)) +
           geom_boxplot() +
           labs(x = "Drink Duration", y = "MMI1", fill = "AS1_DRDUA") +
           theme_minimal()
DRDU1


# Boxplot of MMI2 by AS1_DRDUA (drink duration)
DRDU2 <- ggplot(base_data_null, aes(x = AS1_DRDUA, y = MMI2, fill = AS1_DRDUA)) +
  geom_boxplot() +
  labs(x = "Drink Duration", y = "MMI2", fill = "AS1_DRDUA") +
  theme_minimal()
DRDU2


# Boxplot of MMI3 by AS1_DRDUA (drink duration)
DRDU3 <- ggplot(base_data_null, aes(x = AS1_DRDUA, y = MMI3, fill = AS1_DRDUA)) +
  geom_boxplot() +
  labs(x = "Drink Duration", y = "MMI3", fill = "AS1_DRDUA") +
  theme_minimal()
DRDU3




# Violin plot of base_data_null by AS3_SUPPL
ggplot(base_data_null, aes(x = AS3_DRDU, y = MMI1, fill = AS3_DRDU)) +
  geom_violin(trim = FALSE) +
  labs(x = "Drink Duration", y = "Muscle Mass", fill = "AS3_DRDU") +
  theme_minimal() 




# VitC & VitE VS MMI (Heatmap)
# MMI1
filtered_data <- base_data_null %>% dplyr::filter(!(is.na(MMI1)))

# Bin the Vitamin C and Vitamin E levels into categories for the heatmap
filtered_data <- filtered_data %>%
  mutate(
    AS3_B14_bin = cut(AS3_B14, breaks = seq(0, 300, by = 10), include.lowest = TRUE),
    AS3_B23_bin = cut(AS3_B23, breaks = seq(0, 20, by = 1), include.lowest = TRUE)
  )

# Aggregate data to calculate mean bone density for each combination of binned Vitamin C and Vitamin E levels
agg_data <- filtered_data %>%
  group_by(AS3_B14_bin, AS3_B23_bin) %>%
  summarise(MMI1 = mean(MMI1), .groups = 'drop')

# Create heatmap
ggplot(agg_data, aes(x = AS3_B14_bin, y = AS3_B23_bin, fill = MMI1)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of Muscle Mass by Vitamin C and Vitamin E Levels",
       x = "Vitamin C (mg)",
       y = "Vitamin E (mg)",
       fill = "MMI1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# MMI2
filtered_data2 <- base_data_null %>% dplyr::filter(!(is.na(MMI2)))

# Bin the Vitamin C and Vitamin E levels into categories for the heatmap
filtered_data2 <- filtered_data2 %>%
  mutate(
    AS3_B14_bin = cut(AS3_B14, breaks = seq(0, 300, by = 10), include.lowest = TRUE),
    AS3_B23_bin = cut(AS3_B23, breaks = seq(0, 20, by = 1), include.lowest = TRUE)
  )

# Aggregate data to calculate mean bone density for each combination of binned Vitamin C and Vitamin E levels
agg_data2 <- filtered_data2 %>%
  group_by(AS3_B14_bin, AS3_B23_bin) %>%
  summarise(MMI2 = mean(MMI2), .groups = 'drop')

# Create heatmap
ggplot(agg_data2, aes(x = AS3_B14_bin, y = AS3_B23_bin, fill = MMI2)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of Muscle Mass by Vitamin C and Vitamin E Levels",
       x = "Vitamin C (mg)",
       y = "Vitamin E (mg)",
       fill = "MMI2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# MMI3
filtered_data3 <- base_data_null %>% dplyr::filter(!(is.na(MMI3)))

# Bin the Vitamin C and Vitamin E levels into categories for the heatmap
filtered_data3 <- filtered_data3 %>%
  mutate(
    AS3_B14_bin = cut(AS3_B14, breaks = seq(0, 300, by = 10), include.lowest = TRUE),
    AS3_B23_bin = cut(AS3_B23, breaks = seq(0, 20, by = 1), include.lowest = TRUE)
  )

# Aggregate data to calculate mean bone density for each combination of binned Vitamin C and Vitamin E levels
agg_data3 <- filtered_data3 %>%
  group_by(AS3_B14_bin, AS3_B23_bin) %>%
  summarise(MMI3 = mean(MMI3), .groups = 'drop')

# Create heatmap
ggplot(agg_data3, aes(x = AS3_B14_bin, y = AS3_B23_bin, fill = MMI3)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of Muscle Mass by Vitamin C and Vitamin E Levels",
       x = "Vitamin C (mg)",
       y = "Vitamin E (mg)",
       fill = "MMI3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






# MMI1 vs Exercise Duration (scatter plot)
ggplot(base_data_null , aes(x = AS3_EXERDU, y = MMI1)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "MMI1 vs Exercise Duration",
       x = "Exercise Duration", y = "MMI1") +
  theme_minimal() 

base_data_null$AS3_EXERDU <- as.numeric(base_data_null$AS3_EXERDU)
base_data_null$MMI1 <- as.numeric(base_data_null$MMI1)

reg.exerdu1 <- lm(MMI1 ~ AS3_EXERDU, data = base_data_null)
summary(reg.exerdu1)
abline(coef(reg.exerdu1))


# MMI2 vs Exercise Duration (scatter plot)
ggplot(base_data_null , aes(x = AS3_EXERDU, y = MMI2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "MMI2 vs Exercise Duration",
       x = "Exercise Duration", y = "MMI2") +
  theme_minimal()

base_data_null$MMI2 <- as.numeric(base_data_null$MMI2)

reg.exerdu2 <- lm(MMI2 ~ AS3_EXERDU, data = base_data_null)
summary(reg.exerdu2)
abline(coef(reg.exerdu2))


# MMI3 vs Exercise Duration (scatter plot)
ggplot(base_data_null , aes(x = AS3_EXERDU, y = MMI3)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "MMI3 vs Exercise Duration",
       x = "Exercise Duration", y = "MMI3") +
  theme_minimal()

base_data_null$MMI3 <- as.numeric(base_data_null$MMI3)

reg.exerdu3 <- lm(MMI3 ~ AS3_EXERDU, data = base_data_null)
summary(reg.exerdu3)
abline(coef(reg.exerdu3))



# MMI1 vs SMOKE PACKYR (AS1_PACKYR) (Scatterplot)
reg.packyr1 <- lm(MMI1 ~ AS1_PACKYR, data = base_data_null)
summary(reg.packyr1)
plot(MMI1 ~ AS1_PACKYR, data = base_data_null)
abline(coef(reg.packyr1))



# MMI2 vs SMOKE PACKYR (AS1_PACKYR) (Scatterplot)
reg.packyr2 <- lm(MMI2 ~ AS1_PACKYR, data = base_data_null)
summary(reg.packyr2)
plot(MMI2 ~ AS1_PACKYR, data = base_data_null)
abline(coef(reg.packyr2))

# MMI3 vs SMOKE PACKYR (AS1_PACKYR) (Scatterplot)
reg.packyr3 <- lm(MMI3 ~ AS1_PACKYR, data = base_data_null)
summary(reg.packyr3)
plot(MMI3 ~ AS1_PACKYR, data = base_data_null)
abline(coef(reg.packyr3))


















  
