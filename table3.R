# 加载所需的包
library(haven)
library(dplyr)
library(tidyr)
library(plm)
library(fixest)
library(lmtest)
library(sandwich)
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

# 拟合QLM固定效应模型
fe_model_pmra1 <- fepois(nb_pmra_tcoauth ~ after_death + after_death_cmmn | field_age + year + id, 
                         data = data[data$allcod == 1, ], vcov = ~star_id)

fe_model_pmra2 <- fepois(nb_pmra_ycoauth ~ after_death + after_death_cmmn | field_age + year + id, 
                         data = data[data$allcod == 1, ], vcov = ~star_id)

fe_model_pmra3 <- fepois(nb_pmra_ncoauth ~ after_death + after_death_cmmn | field_age + year + id, 
                         data = data[data$allcod == 1, ], vcov = ~star_id)

fe_model_nbgrants1 <- fepois(nbgrants_tcoauth ~ after_death + after_death_cmmn | field_age + year + id, 
                             data = data[data$allcod == 1 & data$year >= 1975 & data$year < 2006, ], vcov = ~star_id)

fe_model_nbgrants2 <- fepois(nbgrants_ycoauth ~ after_death + after_death_cmmn | field_age + year + id, 
                             data = data[data$allcod == 1 & data$year >= 1975 & data$year < 2006, ], vcov = ~star_id)

fe_model_nbgrants3 <- fepois(nbgrants_ncoauth ~ after_death + after_death_cmmn | field_age + year + id, 
                             data = data[data$allcod == 1 & data$year >= 1975 & data$year < 2006, ], vcov = ~star_id)
# 展示回归结果
summary(fe_model_pmra1)
summary(fe_model_pmra2)
summary(fe_model_pmra3)
summary(fe_model_nbgrants1)
summary(fe_model_nbgrants2)
summary(fe_model_nbgrants3)


# 输出回归结果
etable(fe_model_pmra1, fe_model_pmra2, fe_model_pmra3, fe_model_nbgrants1, fe_model_nbgrants2, fe_model_nbgrants3,
       style.tex = style.tex("aer"), 
       signif.code = NA, 
       drop = "^after_death_cmmn$",
       cluster= ~star_id,
       title = "Table 3",
       )
