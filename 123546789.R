library(tidyverse)

# 讀取檔案
file_path <- "a143-.csv"  # 檔案名稱
raw_data <- read_csv(file_path)

# 檢視讀取的資料
print("原始資料：")
print(raw_data)

# 將中文欄位名稱替換為英文
colnames(raw_data) <- c(
  "year",
  "female_foreign_marriages",
  "male_foreign_marriages",
  "female_foreign_divorces",
  "male_foreign_divorces"
)

# 檢視替換後的資料
print("欄位名稱替換後的資料：")
print(raw_data)

# 新增總結婚與總離婚人數欄位
analyzed_data <- raw_data %>%
  mutate(
    total_marriages = female_foreign_marriages + male_foreign_marriages,
    total_divorces = female_foreign_divorces + male_foreign_divorces
  )

# 檢視新增欄位後的資料
print("新增總結婚與總離婚人數後的資料：")
print(analyzed_data)

# 繪製結婚與離婚人數趨勢圖
analyzed_data %>%
  pivot_longer(cols = c(total_marriages, total_divorces), names_to = "category", values_to = "count") %>%
  ggplot(aes(x = year, y = count, color = category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Marriage and Divorce Trends (2013-2023)", x = "Year", y = "Count", color = "Category") +
  theme_minimal()

# 新增離婚/結婚比率欄位
analyzed_data <- analyzed_data %>%
  mutate(
    divorce_to_marriage_ratio = total_divorces / total_marriages
  )

# 繪製離婚與結婚比率趨勢圖
analyzed_data %>%
  ggplot(aes(x = year, y = divorce_to_marriage_ratio)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "red", size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Divorce to Marriage Ratio Trends (2013-2023)", x = "Year", y = "Divorce/Marriage Ratio") +
  theme_minimal()

# 計算變動幅度（與前一年相比的差異）
analyzed_data <- analyzed_data %>%
  mutate(
    marriage_change = total_marriages - lag(total_marriages),
    divorce_change = total_divorces - lag(total_divorces)
  )

# 繪製變動幅度圖
analyzed_data %>%
  pivot_longer(cols = c(marriage_change, divorce_change), names_to = "change_category", values_to = "change_count") %>%
  ggplot(aes(x = year, y = change_count, fill = change_category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Annual Changes in Marriage and Divorce Counts (2013-2023)", x = "Year", y = "Change in Count", fill = "Change Category") +
  theme_minimal()

# 完成分析並檢視最終資料
print("最終分析後的資料：")
print(analyzed_data)
