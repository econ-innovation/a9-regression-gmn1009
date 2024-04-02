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

data$upr_strs <- as.integer(data$index_cites > 10)

# 定义聚类变量和ID分组变量
cluster_var <- "star_id"
id_var <- "id"



lo_clgs_edtrls <- fepois(nb_pmra_ncoauth ~ after_death + after_death_cmmn | field_age + year + id, 
                         data = data[data$allcod == 1 & data$index_frac_collabs_field_nih <= 45 & data$upr_strs == 1, ], vcov = ~star_id)

hi_clgs_edtrls <- fepois(nb_pmra_ncoauth ~ after_death + after_death_cmmn | field_age + year + id, 
                         data = data[data$allcod == 1 & data$index_frac_collabs_field_nih > 45 & data$upr_strs == 1, ], vcov = ~star_id)


lo_clgs_on <- fepois(nb_pmra_ncoauth ~ after_death + after_death_cmmn | field_age + year + id, 
                     data = data[data$allcod == 1 & data$index_clgs_edtrls <= 10 & data$upr_strs == 1, ], vcov = ~star_id)


hi_clgs_on <- fepois(nb_pmra_ncoauth ~ after_death + after_death_cmmn | field_age + year + id, 
                     data = data[data$allcod == 1 & data$index_clgs_edtrls > 10 & data$upr_strs == 1, ], vcov = ~star_id)



lo_frac <- fepois(nb_pmra_ncoauth ~ after_death + after_death_cmmn | field_age + year + id, 
                  data = data[data$allcod == 1 & data$index_clgs_on_ss == 1 & data$upr_strs == 1, ], vcov = ~star_id)


hi_frac <- fepois(nb_pmra_ncoauth ~ after_death + after_death_cmmn | field_age + year + id, 
                  data = data[data$allcod == 1 & data$index_clgs_on_ss > 1 & data$upr_strs == 1, ], vcov = ~star_id)



# 展示回归结果
summary(lo_clgs_edtrls)
summary(hi_clgs_edtrls)
summary(lo_clgs_on)
summary(hi_clgs_on)
summary(lo_frac)
summary(hi_frac)

# 输出回归结果
etable(lo_clgs_edtrls, hi_clgs_edtrls, lo_clgs_on, hi_clgs_on, lo_frac, hi_frac,
       style.tex = style.tex("aer"), 
       signif.code = NA, 
       drop = "^after_death_cmmn$",
       cluster= ~star_id,
       title = "Table 3")

