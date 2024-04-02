library(haven)

# 加载数据
data <- read_dta("aer_2016-1574_srce_rltd_pairs_replication_set.dta")

# 保留特定的列
data <- data[c("xid", "srce_pmid", "srce_setnb", "srce_pubyear", "treat", "death_year", "pmid", "r1_pubyear", "prox")]

# 移除prox为缺失的行
data <- na.omit(data, cols="prox")


# 生成新变量
data$zero_prox <- as.numeric(data$prox == 0)
data$new_scientist_prx <- as.numeric(data$prox == -1)

# 排序
data <- data[order(data$prox),]

# 计算百分位数
data$pctile_prx <- ifelse(data$prox > -1, ceiling(100 * rank(data$prox) / nrow(data)), NA)

# 为每个百分位数组计算prox的最大值
library(dplyr)
data <- data %>%
  group_by(pctile_prx) %>%
  mutate(max_prx = max(prox, na.rm = TRUE))





# 定义因子级别的标签
data$treat <- factor(data$treat, levels = c(0, 1), labels = c("Control", "Treated"))

# 计算统计量
frac_zero_prox <- round(100 * mean(data$zero_prox), 2)
new_scientist_prox <- round(100 * mean(data$new_scientist_prx), 2)

print(frac_zero_prox)
print(new_scientist_prox)




# 重新编码prox
data$prox[data$prox == -1] <- -0.10

# 绘图
library(ggplot2)

ggplot(data, aes(x=prox)) +
  geom_histogram(binwidth = 0.02, fill="gray") +
  scale_y_continuous("Number of Source Paper/Related Paper Pairs", breaks=seq(0, 0.6, by=0.1)) +
  scale_x_continuous("Average % Field Overlap between Related Authors and the Star", breaks=seq(-0.1, 1, by=0.25)) +
  labs(title="", x="", y="Number of Source Paper/Related Paper Pairs") +
  theme_minimal()
