if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  "readr",
  "dplyr",
  "tidyverse",
  "rvest"
)

content <- xml2::read_html("https://www.basketball-reference.com/leagues/NBA_2023_totals.html")
tables <- content %>% html_table(fill = TRUE)
players <- tables[[1]]
players <- players %>% 
  filter(Rk != "Rk")


players %>% write.csv("./data/player_stats.csv")


