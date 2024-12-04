library(tidyverse) 
# a data frame with 4 observations 
string_df <- tibble( 
  # 台灣地址 
  taiwan_address =  
    c("新竹市東區食品路228號", 
      "臺中市大甲區蔣公路140號", 
      "台南市北區中華北路一段85號", 
      "嘉義縣大林鎮民權路53號", 
      "雲林縣西螺鎮福興里中山路70號", 
      "宜蘭縣礁溪鄉德陽路102號", 
      "南投縣竹山鎮集山路一段2071號", 
      "台中市西區精誠路11-4號"), 
  high_school =  
    c("台北市立第一女子高級中學", 
      "北一女", # 台 北 市立第 一 女 子高級中學 
      "北一女中", # 台 北 市立第 一 女 子高級 中 學 
      "北一女高", # 台 北 市立第 一 女 子 高 級 中 學 
      "台北市立松山高級中學",  
      "新北市立一級棒女子高級中學", 
      "新北一女", 
      "一級棒女中"), 
  school_id =  
    c("411273008", 
      "411382009", 
      "411274010", 
      "411263011", 
      "411044103", 
      "411055104", 
      "411066105", 
      "411077106"), 
  skill =  
    c("C++, R", 
      "R, Python", 
      "R", 
      "Python, Java", 
      "Julia, Python", 
      "Julia", 
      "","R") 
) 
# location ----- 
string_df %>%  
  mutate( 
    location = str_sub(taiwan_address, 1, 3), 
    department = str_sub(school_id, 5, 6) 
  ) 
string_df$from_taichung_city <- stringr::str_detect( 
  string_df$taiwan_address, 
  "(台|臺)中市" 
) 

string_df |> glimpse()
string_df$matches_pattern <- stringr::str_detect( 
  string_df$high_school, 
  "台?北(市立第)?一女子?高?級?中?學?" 
) 

string_df |> glimpse() 
string_df$matches_pattern <- stringr::str_detect( 
  string_df$high_school, 
  "^台?北(市立第)?一女子?高?級?中?學?$" 
) 

string_df |> glimpse() 
baby <- read_csv("https://raw.githubusercontent.com/tpemartin/113-1-R/refs/heads/main/data-public/%E5%85%AC%E8%BE%A6%E6%B0%91%E7%87%9F%E6%89%98%E5%AC%B0%E4%B8%AD%E5%BF%83.csv") 

glimpse(baby) 
