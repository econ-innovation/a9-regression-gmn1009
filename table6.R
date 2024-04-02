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

# 定义聚类变量和ID分组变量
cluster_var <- "star_id"
id_var <- "id"

# 拟合固定效应模型
lo_stk_inv_pubs <- fepois(nb_pmra_ncoauth ~ after_death + after_death_cmmn | field_age + year + id, 
                          data = data[data$allcod == 1 & data$index_pubs <= 10, ], vcov = ~star_id)
hi_stk_inv_pubs <- fepois(nb_pmra_ncoauth ~ after_death + after_death_cmmn | field_age + year + id, 
                          data = data[data$allcod == 1 & data$index_pubs  > 10, ], vcov = ~star_id)


lo_stk_inv_cites <- fepois(nb_pmra_ncoauth ~ after_death + after_death_cmmn | field_age + year + id, 
                           data = data[data$allcod == 1 & data$index_cites <= 10, ], vcov = ~star_id)
hi_stk_inv_cites <- fepois(nb_pmra_ncoauth ~ after_death + after_death_cmmn | field_age + year + id, 
                           data = data[data$allcod == 1 & data$index_cites  > 10, ], vcov = ~star_id)

# 筛选数据
df_data <- data[!is.na(data$index_amount), ]

lo_stk_inv_nih <- fepois(nb_pmra_ncoauth ~ after_death + after_death_cmmn | field_age + year + id, 
                           data = data[df_data$allcod == 1 & df_data$index_cites <= 10, ], vcov = ~star_id)
hi_stk_inv_nih <- fepois(nb_pmra_ncoauth ~ after_death + after_death_cmmn | field_age + year + id, 
                           data = data[df_data$allcod == 1 & df_data$index_amount > 10, ], vcov = ~star_id)


lo_imprtnc <- fepois(nb_pmra_ncoauth ~ after_death + after_death_cmmn | field_age + year + id, 
                           data = data[data$allcod == 1 & data$index_imprtnc  > 10, ], vcov = ~star_id)
hi_imprtnc <- fepois(nb_pmra_ncoauth ~ after_death + after_death_cmmn | field_age + year + id, 
                           data = data[data$allcod == 1 & data$index_imprtnc <= 10, ], vcov = ~star_id)



# 展示回归结果
summary(lo_stk_inv_pubs)
summary(hi_stk_inv_pubs)
summary(lo_stk_inv_cites)
summary(hi_stk_inv_cites)
summary(lo_stk_inv_nih)
summary(hi_stk_inv_nih)
summary(lo_imprtnc)
summary(hi_imprtnc)

# 输出回归结果
etable(lo_stk_inv_pubs, hi_stk_inv_pubs, lo_stk_inv_cites, hi_stk_inv_cites, 
       lo_stk_inv_nih, hi_stk_inv_nih, lo_imprtnc, hi_imprtnc,
       style.tex = style.tex("aer"), 
       signif.code = NA, 
       drop = "^after_death_cmmn$",
       cluster= ~star_id,
       title = "Table 3")



