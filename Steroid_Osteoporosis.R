### This is the code used in analyzing the relationship between steroid and osteoporosis using the KoGES 3rd data


# set working directory
setwd("D:/Data/11_KoGES/31_Epidemiology_Sheets/3rd")

#download csv files
base_data1 <- read.csv(file = "AS3_01_EXAMINEE.csv.gz", header = TRUE)
base_data2 <- read.csv(file = "AS3_04_ACTIVE.csv.gz", header = TRUE)
base_data3 <- read.csv(file = "AS3_09_DRUG.csv.gz", header = TRUE)
base_data4 <- read.csv(file = "AS3_11_FEMD.csv.gz", header = TRUE, fileEncoding = "ISO-8859-1")
base_data5 <- read.csv(file = "AS3_19_FFQNUTRI.csv.gz", header = TRUE)
base_data6 <- read.csv(file = "AS3_21_ANTHRO.csv.gz", header = TRUE)


# sort out the variables to be used
install.packages("dplyr")      # install package
library(dplyr)                 # load package

select1 <- base_data1 %>% dplyr::select(DIST_ID, AS3_SEX, AS3_AGE)
select2 <- base_data2 %>% dplyr::select(DIST_ID, AS3_EXER)

# estrogen
select3 <- base_data3 %>% dplyr::select(DIST_ID, AS3_DRUGF, AS3_DRUGFFH, AS3_DRUGFFHAM, AS3_DRUGFFHFQ, AS3_DRUGFFHCU) #여성호르몬제

select4 <- base_data4 %>% dplyr::select(DIST_ID, AS3_MENYN)
select5 <- base_data5 %>% dplyr::select(DIST_ID, AS3_B05)
select6 <- base_data6 %>% dplyr::select(DIST_ID, AS3_DT, AS3_MT)

# supplements 
select7 <- base_data3 %>% dplyr::select(DIST_ID, AS3_SUPPL, AS3_CA, AS3_CAAM, AS3_VITC, AS3_VITCAM) 



# merge the tables
merge1 <- dplyr::left_join(select1, select2)
merge2 <- dplyr::left_join(merge1, select3)
merge3 <- dplyr::left_join(merge2, select4)
merge4 <- dplyr::left_join(merge3, select5)
merge5 <- dplyr::left_join(merge4, select6)
merge6 <- dplyr::left_join(merge5, select7)

base_data <- merge6


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


# variable type change for supplements
base_data_type$AS3_SUPPL <- as.factor(base_data_type$AS3_SUPPL)
base_data_type$AS3_CA <- as.factor(base_data_type$AS3_CA)
base_data_type$AS3_CAAM <- as.factor(base_data_type$AS3_CAAM)
base_data_type$AS3_VITC <- as.factor(base_data_type$AS3_VITC)
base_data_type$AS3_VITCAM <- as.factor(base_data_type$AS3_VITCAM)



base_data_type$AS3_SEX <- as.factor(base_data_type$AS3_SEX)
base_data_type$AS3_MENYN <- as.factor(base_data_type$AS3_MENYN)
base_data_type$AS3_EXER <- as.factor(base_data_type$AS3_EXER)



sapply(base_data_null, class) # variable types before change
sapply(base_data_type, class) # variable types after change



# 여성호르몬제 관련 변수가 모두 결측인 대상자 제외
base_data_drug_del <- base_data_type %>% dplyr::filter(! (is.na(AS3_DRUGFFH)))
dim (base_data_drug_del) # 3421



# 골밀도 관련 변수가 모두 결측인 대상자 제외
base_data_t_del <- base_data_drug_del %>% dplyr::filter(! (is.na(AS3_DT)))
dim(base_data_t_del) # 2239


############
# new variables
############

## 골다공증 여부 변수 생성 기준

# osteoporosis: t-score <= -2.5
# 골 감소증: t-score -2.5 ~ -1.0  
# 정상군: t-score >= -1.0


## based on the distal radius t-score
base_data_osteo <- base_data_t_del %>%
  dplyr::mutate(AS3_OSTEO_DT = ifelse(AS3_DT <= -2.5, 1,
                                ifelse(AS3_DT > -2.5 & AS3_DT < -1.0, 2,
                                       ifelse(AS3_DT >= -1.0, 3, NA))))
base_data_osteo$AS3_OSTEO_DT <- as.factor(base_data_osteo$AS3_OSTEO_DT) # 범주형



## age variable
base_data_age <- base_data_osteo %>%
  dplyr::mutate(AS3_AGE_factor = ifelse(AS3_AGE < 55, 1,
                                 ifelse(AS3_AGE >= 55, 2, NA)))
base_data_age$AS3_AGE_factor <- as.factor(base_data_age$AS3_AGE_factor) # 범주형



# final dataset save / check
base_data_final <- base_data_age
save(base_data_final, file = "base_data_final.RData") # RData 저장(메모리 -> 하드)
str(base_data_final)
head(base_data_final)


###################


# frequency analysis of the variables

install.packages("descr")
library(descr)

# only consider the data of the female
base_data_final <- base_data_final %>% dplyr::filter(AS3_SEX == 2)

base_data_final$AS3_SUPPL <- factor(base_data_final$AS3_SUPPL, 
                                    levels = c("1", "2"),
                                    labels = c("No", "Yes"))

base_data_final$AS3_DRUGFFH <- factor(base_data_final$AS3_DRUGFFH, 
                                    levels = c("1", "2"),
                                    labels = c("No", "Yes"))

base_data_final$AS3_OSTEO_DT <- factor(base_data_final$AS3_OSTEO_DT, 
                                      levels = c("1", "2", "3"),
                                      labels = c("골다공증", "골감소증", "정상군 "))

base_data_final$AS3_AGE_factor <- factor(base_data_final$AS3_AGE_factor,
                                  levels = c("1", "2"),
                                  labels = c("55세 이하", "55세 이상"))


table.AS3_SUPPL <- descr::freq(base_data_final$AS3_SUPPL) # supplement  Y / N
round(table.AS3_SUPPL, digits = 2)

table.AS3_DRUGFFH <- descr::freq(base_data_final$AS3_DRUGFFH) # 여성호르몬제 Y/N 
round(table.AS3_DRUGFFH, digits = 2)

table.AS3_DRUGFFHFQ <- descr::freq(base_data_final$AS3_DRUGFFHFQ)
round(table.AS3_DRUGFFHFQ, digits = 2)

# osteoporosis Y/N based on the t-score of the distal radius
table.AS3_OSTEO_DT <- descr::freq(base_data_final$AS3_OSTEO_DT) 
round(table.AS3_OSTEO_DT, digits = 2)




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



##### linear regression analysis

# AS3_AGE & AS3_DT (연령 / 골밀도)
base_data_type$AS3_DRUGFFHFQ <- as.numeric(base_data_type$AS3_AGE)
base_data_type$AS3_DT <- as.numeric(base_data_type$AS3_DT)


reg.simple <- lm(AS3_DT ~ AS3_AGE, data = base_data_final)
summary(reg.simple)

plot(AS3_DT ~ AS3_AGE, data = base_data_final)
abline(coef(reg.simple))


# AS3_DRUGFFHFQ & AS3_DT (여성호르몬제 복용 횟수 / 골밀도)
base_data_type$AS3_DRUGFFHFQ <- as.numeric(base_data_type$AS3_DRUGFFHFQ)
base_data_type$AS3_DT <- as.numeric(base_data_type$AS3_DT)


reg.simple1 <- lm(AS3_DT ~ AS3_DRUGFFHFQ, data = base_data_final)
summary(reg.simple1)

plot(AS3_DT ~ AS3_DRUGFFHFQ, data = base_data_final)
abline(coef(reg.simple1))


# AS3_SUPPL, AS3_DT (영양제 섭취 여부 / 골밀도)
base_data_type$AS3_SUPPL <- as.numeric(base_data_type$AS3_SUPPL)
base_data_type$AS3_DT <- as.numeric(base_data_type$AS3_DT)

reg.simple2 <- lm(AS3_DT ~ AS3_SUPPL, data = base_data_final)
summary(reg.simple2)

plot(AS3_DT ~ AS3_SUPPL, data = base_data_final)
abline(coef(reg.simple2))



## AS3_B05, AS3_DT (칼슘 섭취량 / 골밀도)
base_data_type$AS3_B05 <- as.numeric(base_data_type$AS3_B05)
base_data_type$AS3_DT <- as.numeric(base_data_type$AS3_DT)

reg.simple3 <- lm(AS3_B05 ~ AS3_DT, data = base_data_final)
summary(reg.simple3)

plot(AS3_B05 ~ AS3_DT, data = base_data_final)
abline(coef(reg.simple3))























































