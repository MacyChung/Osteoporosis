### This is the code used in analyzing the relationship between steroid and osteoporosis using the KoGES 3rd data


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
base_data7 <- read.csv(file = "AS3_21_ANTHRO.csv.gz", header = TRUE)
base_data8 <- read.csv(file = "AS1_11_BREATH.csv.gz", header = TRUE, fileEncoding = "ISO-8859-1")



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
select8 <- base_data7 %>% dplyr::select(DIST_ID, AS3_DT, AS3_MT)
select9 <- base_data8 %>% dplyr::select(DIST_ID, AS1_CHEMJOB, AS1_CHEMJOBDU, AS1_CHEMK, AS1_DUSTJOB, AS1_DUSTJOBDU, AS1_DUSTK)




# merge the tables
merge1 <- dplyr::left_join(select1, select2)
merge2 <- dplyr::left_join(merge1, select3)
merge3 <- dplyr::left_join(merge2, select4)
merge4 <- dplyr::left_join(merge3, select5)
merge5 <- dplyr::left_join(merge4, select6)
merge6 <- dplyr::left_join(merge5, select7)
merge7 <- dplyr::left_join(merge6, select8)
merge8 <- dplyr::left_join(merge7, select9)

base_data <- merge8


# check the merged table
str(base_data)
base_data

# Quality Control (exclude rows with empty fields)
base_data_null <- base_data
base_data_null[base_data_null == 66666 | base_data_null == 77777 | base_data_null == 99999] <- NA 

#########################################################

# change variable type (for drugs)
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


sapply(base_data_null, class) # variable types before change
sapply(base_data_type, class) # variable types after change





# exclude rows with null bone density related var
base_data_t_del <- base_data_type %>% dplyr::filter(! (is.na(AS3_DT)))
dim(base_data_t_del) # 5758


############
# new variables
############

## osteoporosis var

# osteoporosis: t-score <= -2.5
# osteopenia: t-score -2.5 ~ -1.0  
# healthy: t-score >= -1.0


## based on the distal radius t-score
base_data_osteo <- base_data_t_del %>%
  dplyr::mutate(AS3_OSTEO_DT = ifelse(AS3_DT <= -2.5, 1,
                                ifelse(AS3_DT > -2.5 & AS3_DT < -1.0, 2,
                                       ifelse(AS3_DT >= -1.0, 3, NA))))
base_data_osteo$AS3_OSTEO_DT <- as.factor(base_data_osteo$AS3_OSTEO_DT) # categorical var



## age variable
base_data_age <- base_data_osteo %>%
  dplyr::mutate(AS3_AGE_factor = ifelse(AS3_AGE < 55, 1,
                                 ifelse(AS3_AGE >= 55, 2, NA)))
base_data_age$AS3_AGE_factor <- as.factor(base_data_age$AS3_AGE_factor) # categorical var


## exercise time
base_data_exer <- base_data_age %>%
  dplyr::mutate(AS3_EXERDU_factor = ifelse(AS3_EXERDU < 60, 1,
                                           ifelse(AS3_EXERDU >= 60 & AS3_EXERDU < 120, 2,
                                                  ifelse(AS3_EXERDU >= 120, 3, NA))))
base_data_exer$AS3_EXERDU_factor <- as.factor(base_data_exer$AS3_EXERDU_factor)



# final dataset save / check
base_data_final <- base_data_exer
save(base_data_final, file = "base_data_final.RData") # save RData
str(base_data_final)
head(base_data_final)


###################


# FREQUENCY ANALYSIS of the variables

install.packages("descr")
library(descr)

# only consider the data of the female
base_data_final <- base_data_final %>% dplyr::filter(AS3_SEX == 2)
dim(base_data_final)  # 3258

base_data_final$AS3_SUPPL <- factor(base_data_final$AS3_SUPPL, 
                                    levels = c("1", "2"),
                                    labels = c("No", "Yes"))

base_data_final$AS3_DRUGFFH <- factor(base_data_final$AS3_DRUGFFH, 
                                    levels = c("1", "2"),
                                    labels = c("No", "Yes"))

base_data_final$AS3_OSTEO_DT <- factor(base_data_final$AS3_OSTEO_DT, 
                                      levels = c("1", "2", "3"),
                                      labels = c("osteoporosis", "골감소증", "정상군 "))

base_data_final$AS3_AGE_factor <- factor(base_data_final$AS3_AGE_factor,
                                  levels = c("1", "2"),
                                  labels = c("55-", "55+"))

base_data_final$AS3_EXERDU_factor <- factor(base_data_final$AS3_EXERDU_factor,
                                         levels = c("1", "2", "3"),
                                         labels = c("60-", "60-119", "120+"))




table.AS3_SUPPL <- descr::freq(base_data_final$AS3_SUPPL) # supplement  Y / N
round(table.AS3_SUPPL, digits = 2)

table.AS3_DRUGFFH <- descr::freq(base_data_final$AS3_DRUGFFH) # estrogen Y/N 
round(table.AS3_DRUGFFH, digits = 2)

table.AS3_DRUGFFHFQ <- descr::freq(base_data_final$AS3_DRUGFFHFQ) # estrogen frequency
round(table.AS3_DRUGFFHFQ, digits = 2)

# osteoporosis Y/N based on the t-score of the distal radius
table.AS3_OSTEO_DT <- descr::freq(base_data_final$AS3_OSTEO_DT) 
round(table.AS3_OSTEO_DT, digits = 2)

# exercise Y/N
table.AS3_EXER <- descr::freq(base_data_final$AS3_EXER)
round(table.AS3_EXER, digits = 2)

# exercise frequency (per week)
table.AS3_EXERFQ <- descr::freq(base_data_final$AS3_EXERFQ)
round(table.AS3_EXERFQ, digits = 2)

# exercise duration
table.AS3_EXERDU_factor <- descr::freq(base_data_final$AS3_EXERDU_factor)
round(table.AS3_EXERDU_factor, digits = 2)


# menopause age   AS3_PMAG
table.AS3_PMAG <- descr::freq(base_data_final$AS3_PMAG)
round(table.AS3_PMAG, digits = 2)

# menarche age  AS3_MNSAG
table.AS3_MNSAG <- descr::freq(base_data_final$AS3_MNSAG)
round(table.AS3_MNSAG, digits = 2)



# work experience with chemical (Y/N)
table.AS1_CHEMJOB <- descr::freq(base_data_final$AS1_CHEMJOB)
round(table.AS1_CHEMJOB, digits = 2)
# duration
table.AS1_CHEMJOBDU <- descr::freq(base_data_final$AS1_CHEMJOBDU)
round(table.AS1_CHEMJOBDU, digits = 2)
# type of chemical
table.AS1_CHEMK <- descr::freq(base_data_final$AS1_CHEMK)
round(table.AS1_CHEMK, digits = 2)



# work experience in a dusty environment (Y/N)
table.AS1_DUSTJOB <- descr::freq(base_data_final$AS1_DUSTJOB)
round(table.AS1_DUSTJOB, digits = 2)
# duration
table.AS1_DUSTJOBDU <- descr::freq(base_data_final$AS1_DUSTJOBDU)
round(table.AS1_DUSTJOBDU, digits = 2)
# type of work
table.AS1_DUSTK <- descr::freq(base_data_final$AS1_DUSTK)
round(table.AS1_DUSTK, digits = 2)


# type of work
table.AS3_DRDU <- descr::freq(base_data_final$AS3_DRDU)
round(table.AS3_DRDU, digits = 2)















install.packages("gmodels")
library(gmodels)


#### Cross Table analysis

# bone density VS age
gmodels::CrossTable(base_data_final$AS3_OSTEO_DT, base_data_final$AS3_AGE_factor) 
chisq.test(base_data_final$AS3_OSTEO_DT, base_data_final$AS3_AGE_factor)


# bone density VS estrogen 
gmodels::CrossTable(base_data_final$AS3_OSTEO_DT, base_data_final$AS3_DRUGFFH)
chisq.test(base_data_final$AS3_OSTEO_DT, base_data_final$AS3_DRUGFFH)


# bone density VS supplement 
gmodels::CrossTable(base_data_final$AS3_OSTEO_DT, base_data_final$AS3_SUPPL)
chisq.test(base_data_final$AS3_OSTEO_DT, base_data_final$AS3_SUPPL)

# bone density VS exercise 
gmodels::CrossTable(base_data_final$AS3_OSTEO_DT, base_data_final$AS3_EXERDU_factor)
chisq.test(base_data_final$AS3_OSTEO_DT, base_data_final$AS3_EXERDU_factor)


# bone density vs work experience in a dusty environment
gmodels::CrossTable(base_data_final$AS3_OSTEO_DT, base_data_final$AS1_DUSTJOB)
chisq.test(base_data_final$AS3_OSTEO_DT, base_data_final$AS1_DUSTJOB)
# duration
gmodels::CrossTable(base_data_final$AS3_OSTEO_DT, base_data_final$AS1_DUSTJOBDU)
chisq.test(base_data_final$AS3_OSTEO_DT, base_data_final$AS1_DUSTJOBDU)
# work type
gmodels::CrossTable(base_data_final$AS3_OSTEO_DT, base_data_final$AS1_DUSTK)
chisq.test(base_data_final$AS3_OSTEO_DT, base_data_final$AS1_DUSTK)



##### linear regression analysis

# AS3_AGE & AS3_DT (Age / Bone Density)
base_data_type$AS3_DRUGFFHFQ <- as.numeric(base_data_type$AS3_AGE)
base_data_type$AS3_DT <- as.numeric(base_data_type$AS3_DT)


reg.age <- lm(AS3_DT ~ AS3_AGE, data = base_data_final)
summary(reg.age)

plot(AS3_DT ~ AS3_AGE, data = base_data_final)
abline(coef(reg.age))


reg.estrogen <- lm(AS3_DT ~ AS3_DRUGFFHFQ, data = base_data_final)
summary(reg.estrogen)

plot(AS3_DT ~ AS3_DRUGFFHFQ, data = base_data_final)
abline(coef(reg.estrogen))


# AS3_SUPPL, AS3_DT (Supplement Y/N / Bone Density)
base_data_type$AS3_SUPPL <- as.numeric(base_data_type$AS3_SUPPL)
base_data_type$AS3_DT <- as.numeric(base_data_type$AS3_DT)

reg.supplement <- lm(AS3_DT ~ AS3_SUPPL, data = base_data_final)
summary(reg.supplement)

plot(AS3_DT ~ AS3_SUPPL, data = base_data_final)
abline(coef(reg.supplement))



## AS3_B14, AS3_DT (VitC Intake / Bone Density)
base_data_type$AS3_B14 <- as.numeric(base_data_type$AS3_B14)
base_data_type$AS3_DT <- as.numeric(base_data_type$AS3_DT)

reg.vitc <- lm(AS3_DT ~ AS3_B14, data = base_data_final)
summary(reg.vitc)

plot(AS3_DT ~ AS3_B14, data = base_data_final)
abline(coef(reg.vitc))



## AS3_B03, AS3_DT (Fat Intake / Bone Density)
base_data_type$AS3_B03 <- as.numeric(base_data_type$AS3_B03)
base_data_type$AS3_DT <- as.numeric(base_data_type$AS3_DT)

reg.fat <- lm(AS3_DT ~ AS3_B03, data = base_data_final)
summary(reg.fat)

plot(AS3_DT ~ AS3_B03, data = base_data_final)
abline(coef(reg.fat))




## AS3_B23, AS3_DT (VitE Intake / Bone Density)
base_data_type$AS3_B23 <- as.numeric(base_data_type$AS3_B23)
base_data_type$AS3_DT <- as.numeric(base_data_type$AS3_DT)

reg.vite <- lm(AS3_DT ~ AS3_B23, data = base_data_final)
summary(reg.vite)

plot(AS3_DT ~ AS3_B23, data = base_data_final)
abline(reg.vite)




## AS3_MENYN, AS3_DT (최근 3개월간 생리 여부 / Bone Density)
base_data_type$AS3_MENYN <- as.numeric(base_data_type$AS3_MENYN)
base_data_type$AS3_DT <- as.numeric(base_data_type$AS3_DT)

reg.menyn <- lm(AS3_DT ~ AS3_MENYN, data = base_data_final)
summary(reg.menyn)

plot(AS3_DT ~ AS3_MENYN, data = base_data_final)
abline(reg.menyn)


## AS3_EXERDU, AS3_DT (몸에 땀이 날 정도의 운동 시간 / Bone Density)
base_data_type$AS3_EXERDU <- as.numeric(base_data_type$AS3_EXERDU)
base_data_type$AS3_DT <- as.numeric(base_data_type$AS3_DT)

reg.exer <- lm(AS3_DT ~ AS3_EXERDU, data = base_data_final)
summary(reg.exer)

plot(AS3_DT ~ AS3_EXERDU, data = base_data_final)
abline(reg.exer)



## AS1_DUSTJOBDU, AS3_DT (먼지가 많은 직장 근무 년수 / Bone Density) 
base_data_type$AS1_DUSTJOBDU <- as.numeric(base_data_type$AS1_DUSTJOBDU)
base_data_type$AS3_DT <- as.numeric(base_data_type$AS3_DT)

reg.exer <- lm(AS3_DT ~ AS1_DUSTJOBDU, data = base_data_final)
summary(reg.exer)

plot(AS3_DT ~ AS1_DUSTJOBDU, data = base_data_final)
abline(reg.exer)


## AS1_DUSTK, AS3_DT (먼지가 많은 직장 근무 년수 / Bone Density) 
base_data_type$AS1_DUSTK <- as.numeric(base_data_type$AS1_DUSTK)
base_data_type$AS3_DT <- as.numeric(base_data_type$AS3_DT)

reg.exer <- lm(AS3_DT ~ AS1_DUSTK, data = base_data_final)
summary(reg.exer)

plot(AS3_DT ~ AS1_DUSTK, data = base_data_final)
abline(reg.exer)




install.packages("ggplot2")
library(ggplot2)
library(reshape2)



unique(base_data_final$AS3_DT)
unique(base_data_final$AS3_SEX)
unique(base_data_final$AS3_SUPPL)


# Perform regression analysis
reg.fit <- lm(AS3_AGE ~ AS3_DT + AS3_SUPPL + AS3_MENYN, data = base_data_final)
summary(reg.fit)



ggplot(base_data_final, aes(x = AS3_SEX, y = AS3_AGE, fill = AS3_SEX)) +
  geom_boxplot() +
  labs(x = "AS3_SEX", y = "AS3_AGE", fill = "AS3_SEX") +
  theme_minimal()




# exclude rows with null supplement related var
base_data_final1 <- base_data_final %>% dplyr::filter(! (is.na(AS3_SUPPL)))


# Boxplot of AS3_DT by AS3_SUPPL
ggplot(base_data_final1, aes(x = AS3_SUPPL, y = AS3_DT, fill = AS3_SUPPL)) +
  geom_boxplot() +
  labs(x = "Supplement Y/N", y = "Bone Density", fill = "AS3_SUPPL") +
  theme_minimal()


# Violin plot of AS3_DT by AS3_SUPPL
ggplot(base_data_final1, aes(x = AS3_SUPPL, y = AS3_DT, fill = AS3_SUPPL)) +
  geom_violin(trim = FALSE) +
  labs(x = "Supplement Y/N", y = "Bone Density", fill = "AS3_SUPPL") +
  theme_minimal() 








# Bone Density VS Dusty Job Experience (Boxplot)
ggplot(base_data_final, aes(x = AS1_DUSTJOB, y = AS3_DT, fill = AS1_DUSTJOB)) +
  geom_boxplot() +
  labs(x = "Dusty Job Experience", y = "Bone Density (AS3_DT)", fill = "Dusty Job Experience") +
  theme_minimal()



# Mean Bone Density VS Dusty Job Experience (Heatmap)
heatmap_data <- dcast(base_data_final, AS1_DUSTK ~ AS1_DUSTJOB, value.var = "AS3_DT")

heatmap_matrix <- as.matrix(heatmap_data[, -1])
rownames(heatmap_matrix) <- heatmap_data$AS1_DUSTK

heatmap_df <- melt(heatmap_matrix, varnames = c("Workplace", "Dusty Job Experience"))

ggplot(heatmap_df, aes(x = `Dusty Job Experience`, y = Workplace, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Mean Bone Density by Dusty Job Experience and Workplace",
       x = "Dusty Job Experience", y = "Workplace", fill = "Mean Bone Density") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Bone density VS Dusty Job Experience (Heatmap)
ggplot(base_data_final, aes(x = AS1_DUSTJOB, y = AS1_DUSTK, fill = AS3_DT)) +
  geom_tile(color = "white", size = 0.2) +  # base tile layer
  geom_jitter(aes(color = AS3_DT), width = 0.2, height = 0.2, size = 2) +  # jittered points for bone density
  scale_fill_gradient(low = "blue", high = "red", name = "Bone Density") +  # color gradient for fill
  scale_color_gradient(low = "blue", high = "red", name = "Bone Density") +  # color gradient for points
  labs(title = "Bone Density by Dusty Job Experience and Workplace",
       x = "Dusty Job Experience", y = "Workplace") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Bone Density vs Exercise Duration (Scatter plot)
ggplot(base_data_final, aes(x = AS3_EXERDU, y = AS3_DT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Bone Density vs Exercise Duration",
       x = "Exercise Duration (minutes)",
       y = "Bone Density (AS3_DT)") +
  theme_minimal()


# Bone Density vs Exercise Frequency (Scatter plot)
ggplot(base_data_final , aes(x = AS3_EXERFQ, y = AS3_DT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Bone Density vs Exercise Frequency",
       x = "Exercise Frequency", y = "Bone Density") +
  theme_minimal()


# Bone Density vs Exercise Duration by Exercise Frequency (Scatter plot)
ggplot(base_data_final, aes(x = AS3_EXERDU, y = AS3_DT, color = AS3_EXERFQ)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Bone Density vs Exercise Duration by Exercise Frequency",
       x = "Exercise Duration (minutes)",
       y = "Bone Density (AS3_DT)",
       color = "Exercise Frequency") +
  theme_minimal()



# VitC & VitE VS Bone Density (Heatmap)

# Bin the Vitamin C and Vitamin E levels into categories for the heatmap
filtered_data <- filtered_data %>%
  mutate(
    AS3_B14_bin = cut(AS3_B14, breaks = seq(0, 300, by = 10), include.lowest = TRUE),
    AS3_B23_bin = cut(AS3_B23, breaks = seq(0, 20, by = 1), include.lowest = TRUE)
  )

# Aggregate data to calculate mean bone density for each combination of binned Vitamin C and Vitamin E levels
agg_data <- filtered_data %>%
  group_by(AS3_B14_bin, AS3_B23_bin) %>%
  summarise(mean_bone_density = mean(AS3_DT), .groups = 'drop')

# Create heatmap
ggplot(agg_data, aes(x = AS3_B14_bin, y = AS3_B23_bin, fill = mean_bone_density)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of Bone Density by Vitamin C and Vitamin E Levels",
       x = "Vitamin C (mg)",
       y = "Vitamin E (mg)",
       fill = "Mean Bone Density") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Menarche to Menopause age per individual VS Bone Density
menstrual_data <- base_data_final %>% 
  dplyr::filter(! (is.na(AS3_PMAG))) %>% 
  dplyr::filter(! (is.na(AS3_MNSAG)))%>% 
  dplyr::filter(AS3_MNSAG < AS3_PMAG) %>% 
  dplyr::arrange(AS3_DT)


# Convert DIST_ID to a factor with levels ordered by AS3_DT
menstrual_data$DIST_ID <- factor(menstrual_data$DIST_ID, levels = unique(menstrual_data$DIST_ID))


print(dim(menstrual_data)) # 2214


ggplot(menstrual_data, aes(x = AS3_MNSAG, xend = AS3_PMAG, y = DIST_ID, color = AS3_DT)) +
  geom_segment(size = 0.01) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Menarche to Menopause age per individual",
       x = "Years", 
       y = "Individual",
       color = "Bone Density") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text.y = element_blank())

# ggsave("menstrual_plot.png", plot = m, width = 8, height = 6, dpi = 1200)





# 음주기간 & 총 섭취 알코올량 VS Bone Density (Heatmap)

# Bin the Vitamin C and Vitamin E levels into categories for the heatmap
noalc_data <- base_data_final %>%
  mutate(
    AS3_TOTALC_bin = cut(AS3_TOTALC, breaks = seq(0, 194, by = 4), include.lowest = TRUE)
  )

# Aggregate data to calculate mean bone density for each combination of binned Vitamin C and Vitamin E levels
agg_data1 <- noalc_data %>%
  group_by(AS3_TOTALC_bin, AS3_DRDU) %>%
  summarise(mean_bone_density = mean(AS3_DT), .groups = 'drop') %>% dplyr::filter(!(is.na(AS3_TOTALC_bin)))


# Create heatmap
ggplot(agg_data1, aes(x = AS3_TOTALC_bin, y = AS3_DRDU, fill = mean_bone_density)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of Bone Density by Alcohol drinking period and amount consumed per day",
       x = "Alcohol consumed per day (g)",
       y = "Average drinking period",
       fill = "Average Bone Density") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
















