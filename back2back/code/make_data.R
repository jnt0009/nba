#### SETUP ####
pacman::p_load(
  "rvest",
  "janitor",
  "tidyverse"
)


abbv <- read_delim("abbv.txt", delim = "-", 
                   escape_double = FALSE, col_names = FALSE, 
                   trim_ws = TRUE)


url <- "https://www.basketball-reference.com/leagues/NBA_2024_games-october.html"
url <- "https://www.basketball-reference.com/leagues/NBA_2024_games-november.html"
url <- "https://www.basketball-reference.com/leagues/NBA_2024_games-december.html"
url <- "https://www.basketball-reference.com/leagues/NBA_2024_games-january.html"
webpage <- read_html(url)
table <- html_table(html_nodes(webpage, "table"))
df <- table[[1]] |> clean_names()
df <- df |> select(-c(x, x_2, notes, attend))
df <- df |>
  mutate(
    date = as.Date(date, format = "%a, %b %d, %Y"),
    winner = if_else(pts_2 > pts, "home", "visitor")
  ) |> 
  pivot_longer(cols = c(visitor_neutral, home_neutral), names_to = "court", values_to = "Teams") |>
  mutate(
    court = gsub("_neutral", "", court),
    points_for = if_else(court == "home", pts, pts_2),
    points_against = if_else(court == "home", pts_2, pts)
  ) |> 
  group_by(Teams) |> 
  arrange(date) |> 
  mutate(
    last_game = lag(date),
    days_rest = as.numeric(date - last_game),
    back2back = if_else(days_rest == 1, TRUE, FALSE),
    W_or_L = if_else(winner == court, 1, 0)
  ) |> 
  filter(!is.na(last_game)) # Drop the first game

df <- df |> left_join(abbv, by = join_by(Teams == X2))

# df |> write_csv(file = "back2back/data/nov.csv")


  

