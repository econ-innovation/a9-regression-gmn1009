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

data$antcpt <- as.integer(data$cod == 1)
data$sudden <- as.integer(data$cod == 2)
data$allcod <- as.integer(data$cod <= 3)

data$upr_strs <- as.integer(data$index_cites > 10)
data$lwr_strs <- as.integer(data$index_cites <=10)
data$all_strs <- 1


# 定义聚类变量和ID分组变量
cluster_var <- "star_id"
id_var <- "id"


# 拟合固定效应模型
lo_crwd_sbfld <- fepois(nb_pmra_ncoauth ~ after_death + after_death_cmmn | field_age + year + id, 
                        data = data[data$allcod == 1 & data$index_crwd_sbfld <= 50 & data$upr_strs == 1, ], vcov = ~star_id)
hi_crwd_sbfld <- fepois(nb_pmra_ncoauth ~ after_death + after_death_cmmn | field_age + year + id, 
                        data = data[data$allcod == 1 & data$index_crwd_sbfld  > 50 & data$upr_strs == 1, ], vcov = ~star_id)

lo_avg_field <- fepois(nb_pmra_ncoauth ~ after_death + after_death_cmmn | field_age + year + id, 
                       data = data[data$allcod == 1 & data$index_avg_field_slf_rfrntl > 5 & data$upr_strs == 1, ], vcov = ~star_id)
hi_avg_field <- fepois(nb_pmra_ncoauth ~ after_death + after_death_cmmn | field_age + year + id, 
                       data = data[data$allcod == 1 & data$index_avg_field_slf_rfrntl <= 5 & data$upr_strs == 1, ], vcov = ~star_id)

lo_clqshnss <- fepois(nb_pmra_ncoauth ~ after_death + after_death_cmmn | field_age + year + id, 
                      data = data[data$allcod == 1 & data$index_clqshnss <= 10 & data$upr_strs == 1, ], vcov = ~star_id)
hi_clqshnss <- fepois(nb_pmra_ncoauth ~ after_death + after_death_cmmn | field_age + year + id, 
                      data = data[data$allcod == 1 & data$index_clqshnss > 10 & data$upr_strs == 1, ], vcov = ~star_id)


# 展示回归结果
summary(lo_crwd_sbfld)
summary(hi_crwd_sbfld)
summary(lo_avg_field)
summary(hi_avg_field)
summary(lo_clqshnss)
summary(hi_clqshnss)

# 输出回归结果
etable(lo_crwd_sbfld, hi_crwd_sbfld, lo_avg_field, hi_avg_field, lo_clqshnss, hi_clqshnss,
       style.tex = style.tex("aer"), 
       signif.code = NA, 
       drop = "^after_death_cmmn$",
       cluster= ~star_id,
       title = "Table 3")


