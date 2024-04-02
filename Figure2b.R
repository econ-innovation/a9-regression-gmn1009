

library(ggplot2)
library(haven)
library(dplyr)
library(plm)
library(fixest)
library(openxlsx)

# 假设已经成功加载了数据集 "aer_2016-1574_replication_set.dta"
data <- read_dta("aer_2016-1574_replication_set.dta")

# 数据预处理，包括生成yr_to_death和处理组标记
data <- data %>%
  mutate(
    yr_to_death = year - death_year,
    treat = if_else(!is.na(death_year), 1, 0)  # 标记处理组
  )


# 修正后的代码，直接在循环中添加每个新变量
for (i in -10:10) {
  var_name <- paste("yr_to_death", i, sep = "")
  data[[var_name]] <- as.numeric(data$yr_to_death == i)
}

# 分别为yr_to_death < -10 和 > 10的情况添加变量
data$yr_to_death_11 <- as.numeric(data$yr_to_death < -10)
data$yr_to_death11 <- as.numeric(data$yr_to_death > 10)

# 检查新变量是否已成功添加
head(data)

 


# 检查数据集中的变量名称
names(data)

# 运行泊松回归模型
model <- fepois(nb_pmra_ycoauth ~ `yr_to_death_11` + `yr_to_death-10` + `yr_to_death-9` + 
                  `yr_to_death-8` + `yr_to_death-7` + `yr_to_death-6` + `yr_to_death-5` + `yr_to_death-4` + 
                  `yr_to_death-3` + `yr_to_death-2`  + yr_to_death0 + 
                  yr_to_death1 + yr_to_death2 + yr_to_death3 + yr_to_death4 + 
                  yr_to_death5 + yr_to_death6 + yr_to_death7 + yr_to_death8 + 
                  yr_to_death9 + yr_to_death10 + yr_to_death11  | field_age + year + id, data = data, vcov = ~star_id)
 
# nb_pmra_ycoauth nb_pmra_tcoauth nb_pmra_ncoauth


# 提取模型的系数
coefficients <- coef(model)

# 获取变量名
variable_names <- names(coefficients)

# 提取模型系数的置信区间
conf_intervals <- confint(model)

# 创建包含系数、置信区间和变量名的数据框
results_df <- data.frame(
  variable = variable_names,
  coefficient = coefficients,
  ci_lower = conf_intervals[, 1],  # 置信区间下限
  ci_upper = conf_intervals[, 2]   # 置信区间上限
)



# 创建一个Excel工作簿
wb <- createWorkbook()

# 添加一个工作表
addWorksheet(wb, "Confidence Intervals")

# 将置信区间数据写入工作表
writeData(wb, "Confidence Intervals", results_df)

# 保存工作簿为xlsx文件
saveWorkbook(wb, "figure2nb_pmra_ncoauth.xlsx", overwrite = TRUE)

 
# 创建系数图，并按照指定顺序对 variable 变量排序
coeff_plot <- ggplot(results_df, aes(x = reorder(variable, as.numeric(gsub("yr_to_death", "", variable))), y = coefficient)) +
  geom_point() +  # 点图表示系数
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.1) +  # 误差棒表示置信区间
  labs(x = "Variable", y = "Coefficient") +  # 添加轴标签
  ggtitle("Coefficient Plot") +  # 添加标题
  theme_minimal() +  # 使用简洁的主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # 调整 x 轴标签角度

# 显示系数图
print(coeff_plot)

 