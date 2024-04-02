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
yciteinfield <- fepois(nb_pmra_yciteinfield_ncoauth ~ after_death + after_death_cmmn | 
                         field_age + year + id, data = data[data$allcod == 1, ], vcov = ~star_id)

nciteinfield <- fepois(nb_pmra_nciteinfield_ncoauth ~ after_death + after_death_cmmn | 
                         field_age + year + id, data = data[data$allcod == 1, ], vcov = ~star_id)

ycitethestar <- fepois(nb_pmra_ycitethestar_ncoauth ~ after_death + after_death_cmmn | 
                         field_age + year + id, data = data[data$allcod == 1, ], vcov = ~star_id)

ncitethestar <- fepois(nb_pmra_ncitethestar_ncoauth ~ after_death + after_death_cmmn | 
                         field_age + year + id, data = data[data$allcod == 1, ], vcov = ~star_id)


# 展示回归结果
summary(yciteinfield)
summary(nciteinfield)
summary(ycitethestar)
summary(ncitethestar)

# 输出回归结果
etable(yciteinfield, nciteinfield, ycitethestar, ncitethestar,
       style.tex = style.tex("aer"), 
       signif.code = NA, 
       drop = "^after_death_cmmn$",
       cluster= ~star_id,
       title = "Table 3"
)


