install.packages("tidyverse")

# 載入所需的套件
library(tidyverse)

# 生成模擬資料集，假設有30位同學
set.seed(123) # 為了結果可重現
tidy_students_interests <- tibble(
  student_id = 1:30,
  interest = sample(
    c("看電影", "游泳", "打電動", "逛街", "閱讀", "畫畫", "其他"),
    size = 30,
    replace = TRUE
  )
)

# 計算興趣的比例
tidy_interest_proportion <- tidy_students_interests %>%
  count(interest) %>%
  mutate(proportion = n / sum(n) * 100)

# 繪製圓餅圖
tidy_interest_proportion %>%
  ggplot(aes(x = "", y = proportion, fill = interest)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  labs(
    title = "班上同學興趣比例",
    fill = "興趣",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )


