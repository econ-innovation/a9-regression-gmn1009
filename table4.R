# 加载所需的包
library(haven)
library(dplyr)
library(tidyr)
library(plm)
library(fixest)
library(lmtest)
library(sandwich)
library(stargazer)
library(Hmisc)

# 设置工作路径
main <- "/Users/popo/Alldoc/R/Azoulayetal_2019_AER"
setwd(paste0(main, "/aer_2016-1574_replication_set"))

# 读取Stata数据集
data <- read_dta("aer_2016-1574_replication_set.dta")

# 定义变量标签
label(data$after_death) <- "After Death"

# 生成二进制变量
data$after_death_cmmn <- as.integer(data$year > data$death_year)
data$allcod <- as.integer(data$cod <= 3)

# 创建一个空列表来存储回归结果
results_list <- list()

# 定义聚类变量和ID分组变量
cluster_var <- "star_id"
id_var <- "id"

# 拟合固定效应模型
fe_model_pmra3 <- fepois(nb_pmra_ncoauth ~ after_death + after_death_cmmn |
                           field_age + year + id, 
                         data = data[data$allcod == 1, ], 
                         vcov = ~star_id)

pmra_qrt1 <- fepois(nb_pmra_qrt1_ncoauth ~ after_death + after_death_cmmn |
                      field_age + year + id, 
                    data = data[data$allcod == 1, ], 
                    vcov = ~star_id)

pmra_qrt2 <- fepois(nb_pmra_qrt2_ncoauth ~ after_death + after_death_cmmn |
                      field_age + year + id, 
                    data = data[data$allcod == 1, ], 
                    vcov = ~star_id)

pmra_qrt3 <- fepois(nb_pmra_qrt3_ncoauth ~ after_death + after_death_cmmn |
                      field_age + year + id, 
                    data = data[data$allcod == 1, ], 
                    vcov = ~star_id)

pmra_75to95 <- fepois(nb_pmra_75to95_ncoauth ~ after_death + after_death_cmmn |
                        field_age + year + id, 
                      data = data[data$allcod == 1, ], 
                      vcov = ~star_id)

pmra_95to99 <- fepois(nb_pmra_95to99_ncoauth ~ after_death + after_death_cmmn |
                        field_age + year + id, 
                      data = data[data$allcod == 1, ], 
                      vcov = ~star_id)

pmra_abv99 <- fepois(nb_pmra_abv99_ncoauth ~ after_death + after_death_cmmn |
                        field_age + year + id, 
                      data = data[data$allcod == 1, ], 
                      vcov = ~star_id)

# 展示回归结果
summary(fe_model_pmra3)
summary(pmra_qrt1)
summary(pmra_qrt2)
summary(pmra_qrt3)
summary(pmra_75to95)
summary(pmra_95to99)
summary(pmra_abv99)

# 输出回归结果
etable(fe_model_pmra3, pmra_qrt1, pmra_qrt2, pmra_qrt3, pmra_75to95, pmra_95to99, pmra_abv99,
       style.tex = style.tex("aer"), 
       signif.code = NA, 
       drop = "^after_death_cmmn$",
       cluster= ~star_id,
       title = "Table 3"
       )


