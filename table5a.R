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
pmra_hi_prxmt_crdnl <- fepois(nb_pmra_hi_prxmt_crdnl_ncoauth ~ after_death + after_death_cmmn |
                           field_age + year + id, data = data[data$allcod == 1, ], vcov = ~star_id)

pmra_lo_prxmt_crdnl <- fepois(nb_pmra_lo_prxmt_crdnl_ncoauth ~ after_death + after_death_cmmn |
                                field_age + year + id, data = data[data$allcod == 1, ], vcov = ~star_id)

pmra_hi_prxmt_ordnl <- fepois(nb_pmra_hi_prxmt_ordnl_ncoauth ~ after_death + after_death_cmmn |
                                field_age + year + id, data = data[data$allcod == 1, ], vcov = ~star_id)

pmra_lo_prxmt_ordnl <- fepois(nb_pmra_lo_prxmt_ordnl_ncoauth ~ after_death + after_death_cmmn |
                               field_age + year + id, data = data[data$allcod == 1, ], vcov = ~star_id)

# 展示回归结果
summary(pmra_hi_prxmt_crdnl)
summary(pmra_lo_prxmt_crdnl)
summary(pmra_hi_prxmt_ordnl)
summary(pmra_lo_prxmt_ordnl)


# 输出回归结果
etable(pmra_hi_prxmt_crdnl, pmra_lo_prxmt_crdnl, pmra_hi_prxmt_ordnl, pmra_lo_prxmt_ordnl,
       style.tex = style.tex("aer"), 
       signif.code = NA, 
       drop = "^after_death_cmmn$",
       cluster= ~star_id,
       title = "Table 3"
)



