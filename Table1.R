# 加载必要的R包
library(haven)  # 用于读取Stata数据文件
library(dplyr)  # 数据处理
library(psych)  # 描述性统计分析

# 读取数据
data <- read_dta("aer_2016-1574_replication_set.dta")

# 数据预处理及变量处理
data_processed <- data %>%
  filter(treat == 1) %>%
  mutate(
    deg_recode = case_when(
      deg == 6 ~ 2,
      deg == 4 ~ 1,
      TRUE ~ deg
    ),
    MD = as.numeric(deg_recode == 1),
    PhD = as.numeric(deg_recode == 2),
    MDPhD = as.numeric(deg_recode == 3),
    sudden = as.numeric(cod == 2)
  ) %>%
  group_by(star_id) %>%
  mutate(nb_srce = n_distinct(srce_pmid)) %>%
  ungroup()

# 使用psych包计算描述性统计
descriptive_stats <- describe(data_processed[,c("yob", "deg_year", "death_year", "aad", "female", "MD", "PhD", "MDPhD", "sudden", "nb_srce", "inv_stk_pubs", "inv_stk_cites", "inv_stk_nih", "star_on_ss_wthn5", "stk_edtrls_at_death")])

# 重命名描述性统计表的行名以匹配指定的变量名
rownames(descriptive_stats) <- c("Year of Birth", "Degree year", "Year of Death", "Age at Death", "Female", 
                                 "MD Degree", "PhD Degree", "MD/PhD Degree", "Sudden Death", "Nb. of Subfields", 
                                 "Career Nb. of Pubs.", "Career Nb. of Citations", "Career NIH Funding", 
                                 "Sits on NIH Study Section", "Career Nb. of Editorials")

# 选择需要的统计量
selected_stats <- descriptive_stats[, c("mean", "median", "sd", "min", "max")]

# 输出描述性统计结果到CSV文件
write.csv(selected_stats, "Table1 Descriptive_Statistics.csv", row.names = TRUE)
