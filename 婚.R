library(tidyverse)

# 將資料整理成 tidy 格式的 tibble
tidy_marriage_data <- tibble(
  年別 = c(2013:2023),
  女性與外國人結婚人數 = c(453, 573, 654, 756, 890, 862, 920, 446, 307, 508, 825),
  男性與外國人結婚人數 = c(321, 384, 340, 347, 401, 447, 432, 321, 262, 364, 549),
  女性與外國人離婚人數 = c(459, 417, 437, 445, 461, 401, 415, 353, 321, 309, 338),
  男性與外國人離婚人數 = c(93, 81, 77, 70, 81, 96, 77, 65, 67, 84, 82)
)

# 檢視資料
print(tidy_marriage_data)
# 新增總結婚與總離婚人數欄位
tidy_marriage_data <- tidy_marriage_data %>%
  mutate(
    總結婚人數 = 女性與外國人結婚人數 + 男性與外國人結婚人數,
    總離婚人數 = 女性與外國人離婚人數 + 男性與外國人離婚人數
  )

# 檢視新增的資料框
print(tidy_marriage_data)
# 繪製結婚與離婚人數趨勢圖
tidy_marriage_data %>%
  pivot_longer(cols = c(總結婚人數, 總離婚人數), names_to = "類別", values_to = "人數") %>%
  ggplot(aes(x = 年別, y = 人數, color = 類別)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "2013-2023年外國人結婚與離婚趨勢", x = "年份", y = "人數", color = "類別") +
  theme_minimal()
# 新增離婚/結婚比率欄位
tidy_marriage_data <- tidy_marriage_data %>%
  mutate(
    離婚結婚比率 = 總離婚人數 / 總結婚人數
  )

# 檢視結果
print(tidy_marriage_data)

# 繪製離婚與結婚比率趨勢圖
tidy_marriage_data %>%
  ggplot(aes(x = 年別, y = 離婚結婚比率)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "red", size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "離婚與結婚比率趨勢 (2013-2023)", x = "年份", y = "離婚/結婚比率") +
  theme_minimal()
# 計算變動幅度（與前一年相比的差異）
tidy_marriage_data <- tidy_marriage_data %>%
  mutate(
    結婚變動幅度 = 總結婚人數 - lag(總結婚人數),
    離婚變動幅度 = 總離婚人數 - lag(總離婚人數)
  )

# 繪製變動幅度圖
tidy_marriage_data %>%
  pivot_longer(cols = c(結婚變動幅度, 離婚變動幅度), names_to = "變動類別", values_to = "人數變化") %>%
  ggplot(aes(x = 年別, y = 人數變化, fill = 變動類別)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "結婚與離婚人數年度變動幅度 (2013-2023)", x = "年份", y = "人數變動", fill = "變動類別") +
  theme_minimal()
