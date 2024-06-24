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
# 여성호르몬제 (에스트로겐)
# select3 <- base_data3 %>% dplyr::select(DIST_ID, AS3_DRUGF, AS3_DRUGFFH, AS3_DRUGFFHAM, AS3_DRUGFFHFQ, AS3_DRUGFFHCU) 

# 갑상선약
# select3 <- base_data3 %>% dplyr::select(DIST_ID, AS3_DRUGF, AS3_DRUGFTH, AS3_DRUGFTHAM, AS3_DRUGFTHFQ, AS3_DRUGFTHCU) 

# 이뇨제
# select3 <- base_data3 %>% dplyr::select(DIST_ID, AS3_DRUGF, AS3_DRUGFDI, AS3_DRUGFDIAM, AS3_DRUGFDIFQ, AS3_DRUGFDICU) 

# 항응고제
# select3 <- base_data3 %>% dplyr::select(DIST_ID, AS3_DRUGF, AS3_DRUGFSL, AS3_DRUGFSLAM, AS3_DRUGFSLFQ, AS3_DRUGFSLCU) 

# 칼슘제재
select3 <- base_data3 %>% dplyr::select(DIST_ID, AS3_SUPPL, AS3_CA, AS3_CAAM) 

# 비타민C
# select3 <- base_data3 %>% dplyr::select(DIST_ID, AS3_SUPPL, AS3_VITC, AS3_VITCAM) 

# 비타민E
# select3 <- base_data3 %>% dplyr::select(DIST_ID, AS3_SUPPL, AS3_VITE, AS3_VITEAM) 


select4 <- base_data4 %>% dplyr::select(DIST_ID, AS3_MENYN)
select5 <- base_data5 %>% dplyr::select(DIST_ID, AS3_B05)
select6 <- base_data6 %>% dplyr::select(DIST_ID, AS3_DT, AS3_MT)


# merge the tables
merge1 <- dplyr::left_join(select1, select2)
merge2 <- dplyr::left_join(merge1, select3)
merge3 <- dplyr::left_join(merge2, select4)
merge4 <- dplyr::left_join(merge3, select5)
merge5 <- dplyr::left_join(merge4, select6)

base_data <- merge5


# check the merged table
str(base_data)
base_data

# Quality Control (exclude rows with empty fields)
base_data_null <- base_data
base_data_null[base_data_null == 66666 | base_data_null == 77777 | base_data_null == 99999] <- NA 

#########################################################

# change variable type (for drugs)
base_data_type <- base_data_null

base_data_type$AS3_DRUG <- as.factor(base_data_type$AS3_DRUGF) # numerical -> factor
base_data_type$AS3_DRUGST <- as.factor(base_data_type$AS3_DRUGFSL)
base_data_type$AS3_DRUGSTAM <- as.factor(base_data_type$AS3_DRUGFSLAM)
base_data_type$AS3_DRUGSTCU <- as.factor(base_data_type$AS3_DRUGFSLCU)

base_data_type$AS3_SEX <- as.factor(base_data_type$AS3_SEX)
base_data_type$AS3_MENYN <- as.factor(base_data_type$AS3_MENYN)
base_data_type$AS3_EXER <- as.factor(base_data_type$AS3_EXER)



sapply(base_data_null, class) # variable types before change
sapply(base_data_type, class) # variable types after change



# 약물 관련 변수가 모두 결측인 대상자 제외
base_data_drug_del <- base_data_type %>% dplyr::filter(! (is.na(AS3_DRUGF)))
dim (base_data_drug_del) # 총 3421개의 데이터


# 골밀도 관련 변수가 모두 결측인 대상자 제외
base_data_t_del <- base_data_drug_del %>% dplyr::filter(! (is.na(AS3_DT) & is.na(AS3_MT)))
dim(base_data_t_del) # 총 2246개의 데이터

################################################################

# change variable type (for supplies)
base_data_type <- base_data_null

base_data_type$AS3_SUPPL <- as.factor(base_data_type$AS3_SUPPL) # numerical -> factor
base_data_type$AS3_NUT <- as.factor(base_data_type$AS3_CA)
base_data_type$AS3_NUTOFTEN <- as.factor(base_data_type$AS3_CAAM)

base_data_type$AS3_SEX <- as.factor(base_data_type$AS3_SEX)
base_data_type$AS3_MENYN <- as.factor(base_data_type$AS3_MENYN)
base_data_type$AS3_EXER <- as.factor(base_data_type$AS3_EXER)


# 영양제제 관련 변수가 모두 결측인 대상자 제외
base_data_suppl_del <- base_data_type %>% dplyr::filter(! (is.na(AS3_SUPPL)))
dim (base_data_suppl_del) # 총 3415개의 데이터


# 골밀도 관련 변수가 모두 결측인 대상자 제외
base_data_t_del <- base_data_suppl_del %>% dplyr::filter(! (is.na(AS3_DT) & is.na(AS3_MT)))
dim(base_data_t_del) # 총 2240개의 데이터



############
# 변수 생성 (new variables)
############

## 골다공증 여부 변수 생성 기준

# 골다공증: t-score <= -2.5
# 골 감소증: t-score -2.5 ~ -1.0  
# 정상군: t-score >= -1.0


# based on the distal radius t-score
base_data_osteo <- base_data_t_del %>%
  dplyr::mutate(AS3_OSTEO_DT = ifelse(AS3_DT <= -2.5, 1,
                                ifelse(AS3_DT > -2.5 & AS3_DT < -1.0, 2,
                                       ifelse(AS3_DT >= -1.0, 3, NA))))
base_data_osteo$AS3_OSTEO_DT <- as.factor(base_data_osteo$AS3_OSTEO_DT) # 범주형


# based on the midshaft tibia t-score
base_data_osteo_mt <- base_data_osteo %>%
  dplyr::mutate(AS3_OSTEO_MT = ifelse(AS3_MT <= -2.5, 1,
                                      ifelse(AS3_MT > -2.5 & AS3_MT < -1.0, 2,
                                             ifelse(AS3_MT >= -1.0, 3, NA))))
base_data_osteo_mt$AS3_OSTEO_MT <- as.factor(base_data_osteo_mt$AS3_OSTEO_MT) # 범주형


# final dataset 저장 및 확인
base_data_final <- base_data_osteo_mt
save(base_data_final, file = "base_data_final.RData") # RData 저장(메모리 -> 하드)
str(base_data_final)
head(base_data_final)

###################


# frequency analysis (빈도분석) of the variables

install.packages("descr")
library(descr)

table.AS3_DRUGST <- descr::freq(base_data_final$AS3_CA) # 칼슘제재재 복용 Y / N
round(table.AS3_DRUGST, digits = 2)


table.AS3_OSTEO_DT <- descr::freq(base_data_final$AS3_OSTEO_DT) # osteoporosis Y/N based on the t-score of the distal radius
round(table.AS3_OSTEO_DT, digits = 2)






















































