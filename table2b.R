library(dplyr)
library(haven)
library(psych)

# 直接读取数据

data_processed <- data %>%
  arrange(id, year) %>% 
  group_by(id) %>%
  mutate(
    stk_nbpmra_tcoauth = cumsum(nb_pmra_tcoauth),
    stk_nbpmra_ncoauth = cumsum(nb_pmra_ncoauth),
    stk_nbpmra_ycoauth = cumsum(nb_pmra_ycoauth),
    stk_nbgrants_tcoauth = cumsum(nbgrants_tcoauth)
  ) %>%
  ungroup() %>%
  filter(year == death_year) %>%
  group_by(xid, treat) %>%
  mutate(
    nbfields_inxid = n_distinct(srce_pmid),
    weight = ifelse(nbfields_inxid != 0, 1 / nbfields_inxid, NA)
  ) %>%
  ungroup()



# 筛选treat=1的数据
data_processed_treat_0 <- data_processed %>% 
  filter(treat == 0)

# 使用psych包计算描述性统计
descriptive_stats <- describe(data_processed_treat_1[,c("stk_nbpmra_tcoauth", "stk_nbpmra_ncoauth", "stk_nbpmra_ycoauth", "nbauthors", "srce_stkcites", "srce_lngrn_cites", "female", "deg_year", "death_year", "aad", "inv_stk_pubs", "inv_stk_nih", "inv_stk_cites")])

# 重命名描述性统计表的行名以匹配指定的变量名
rownames(descriptive_stats) <- c("Baseline Stock of Related Articles in the Field",
                                 "Baseline Stock of Related Articles in the Field, Non-Collaborators",
                                 "Baseline Stock of Related Articles in the Field, Collaborators",
                                 "Investigator Gender",
                                 "Death Year",
                                 "Age at Death",
                                 "Investigator Cuml. Nb. of Publications",
                                 "Investigator Cuml. Nb. of Citations",
                                 "Source Article Nb. of Authors",
                                 "Source Article Long-run Citations",
                                 "Source Article Citations at Baseline",
                                 "Investigator Cuml. NIH Funding at Baseline",
                                 "Investigator Year of Degree")


# 选择需要的统计量
selected_stats <- descriptive_stats[, c("mean", "median", "sd", "min", "max")]

# 输出描述性统计结果到CSV文件
write.csv(selected_stats, "Table2 Descriptive_Statistics treat0.csv", row.names = TRUE)
