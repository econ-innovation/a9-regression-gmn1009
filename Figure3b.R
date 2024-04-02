library(haven)
library(dplyr)
library(plm)
library(broom)


data <- read_dta("aer_2016-1574_replication_set.dta")


# 创建变量
data <- data %>%
  mutate(
    after_death_cmmn = as.numeric(year > death_year),
    allcod = as.numeric(cod <= 3),
    after_death = "After Death" # 这里假设after_death是已存在的变量
  )

data$after_death <- ifelse(data$after_death == "After Death", 1, 0)




-----------------
library(fixest)
library(haven)
library(dplyr)
library(plm)
library(broom)


data <- read_dta("aer_2016-1574_replication_set.dta")


# 创建变量
data <- data %>%
  mutate(
    after_death_cmmn = as.numeric(year > death_year),
    allcod = as.numeric(cod <= 3),
    after_death = "After Death" # 这里假设after_death是已存在的变量
  )

data$after_death <- ifelse(data$after_death == "After Death", 1, 0)





# 生成所有反双曲正弦变换变量
pctiles <- c("0", "belowmed", "50to55", "55to60", "60to65", "65to70", "70to75", "75to80", "80to85", "85to90", "90to95", "95to99", "above99")
for (pctl in pctiles) {
  trans_var_name <- paste0("ihs_prox_", pctl, "_ncoauth")
  orig_var_name <- paste0("nb_pmra_prox_", pctl, "_ncoauth")
  data[[trans_var_name]] <- asinh(data[[orig_var_name]])
}

# 准备模型的固定效应部分
fe_formula <- "~ after_death + after_death_cmmn | field_age + year + id"

# 初始化结果列表
results <- list()

# 对每个变量进行回归分析
for (trans_var_name in names(data)[grepl("^ihs_prox_.*_ncoauth$", names(data))]) {
  # 构建模型公式
  formula <- as.formula(paste(trans_var_name, fe_formula))
  
  # 执行面板回归
  model <- feols(formula, data = data, vcov = ~star_id)
  
  # 存储模型结果
  results[[trans_var_name]] <- summary(model)
}



model <- feols(ihs_prox_0_ncoauth ~ after_death + after_death_cmmn | field_age + year + id, data = data, vcov = ~star_id)

------------
  library(openxlsx)

# 创建一个空的数据框来存储所有回归结果
all_results <- data.frame()

# 对每个变量进行回归分析
for (trans_var_name in names(data)[grepl("^ihs_prox_.*_ncoauth$", names(data))]) {
  # 构建模型公式
  formula <- as.formula(paste(trans_var_name, fe_formula))
  
  # 执行面板回归
  model <- feols(formula, data = data, vcov = ~star_id)
  
  # 提取系数和置信区间
  beta <- coef(model)
  ci <- confint(model)
  
  # 将系数和置信区间合并为一个数据框
  results_df <- data.frame(
    beta = beta,
    ci_lb = ci[, 1],  # 置信区间下限
    ci_ub = ci[, 2]   # 置信区间上限
  )
  
  # 添加变量名称列
  results_df$variable <- trans_var_name
  
  # 将结果追加到所有结果数据框中
  all_results <- rbind(all_results, results_df)
}

# 创建一个Excel工作簿
wb <- createWorkbook()

# 添加一个工作表
addWorksheet(wb, "Regression Results")

# 将结果写入工作表
writeData(wb, "Regression Results", all_results)

# 保存工作簿为xlsx文件
saveWorkbook(wb, "all_regression_results.xlsx", overwrite = TRUE)


-----------
  library(ggplot2)

# 假设您的系数数据框为 coeff_data
# 您可以直接使用您提供的数据框 coeff_data

# 创建系数图
coeff_plot <- ggplot(all_results, aes(x = variable, y = beta)) +
  geom_point() +  # 点图表示系数
  geom_errorbar(aes(ymin = ci_lb, ymax = ci_ub), width = 0.2) +  # 误差棒表示置信区间
  coord_flip() +  # 翻转坐标轴，使图形更易读
  labs(x = "Variable", y = "Coefficient") +  # 添加轴标签
  ggtitle("Coefficient Plot") +  # 添加标题
  theme_minimal() +  # 使用简洁的主题
  scale_y_continuous(limits = c(-0.0025, 0.0025))  # 设置y轴刻度范围为-0.2到0.2

# 显示系数图
print(coeff_plot)
