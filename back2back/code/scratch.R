#### Setup ####
pacman::p_load(
  "tidyverse"
)

source("./back2back/data/nov.csv")
df <- read_csv("back2back/data/nov.csv")

mdf <- df |>
  mutate(
    points_for = if_else(court == "home", pts, pts_2),
    points_against = if_else(court == "home", pts_2, pts)
  ) |>
  select(-c(pts, pts_2)) |>
  print()

mdf |>
  group_by(Teams) |>
  summarise_if(is.numeric, mean) |>
  pivot_longer(cols = c(contains("points"))) |>
  ggplot(aes(value, name, fill = name)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Teams, scales = "free")

mdf |>
  group_by(Teams) |>
  summarise_if(is.numeric, mean) |>
  mutate(diff = points_for - points_against) |>
  ggplot(aes(diff, reorder(Teams, diff))) +
  geom_bar(stat = "identity")

# mdf <- mdf |> 
#   # filter(Teams == "New Orleans Pelicans") |> 
#   group_by(Teams) |> 
#   arrange(date) |> 
#   mutate(
#     last_game = lag(date),
#     days_rest = as.numeric(date - last_game),
#     back2back = if_else(days_rest == 1, TRUE, FALSE)
#   ) |> 
#   filter(!is.na(last_game)) # Drop the first game



mdf |> 
  group_by(Teams, back2back) |> 
  filter(back2back == TRUE) |> 
  tally() |> 
  ggplot(aes(n, reorder(Teams, n))) +
  geom_bar(stat = "identity")


mdf |> 
  group_by(Teams, back2back) |> 
  summarise(
    points_for = mean(points_for),
    points_against = mean(points_against)
  ) |> 
  pivot_longer(cols = c(contains("point"))) |> 
  ggplot(aes(Teams, value, group = name)) +
  geom_bar(stat = "identity") +
  facet_wrap(~back2back*name)

glm(W_or_L ~ back2back, data = mdf, family = "binomial") |> coef() |> exp()

mdf |> 
  group_by(Teams) |> 
  summarise(
    W_or_L = mean(W_or_L),
    back2back = sum(back2back) |> as.character()
  ) |> 
  ggplot(aes(W_or_L, fill = back2back, color = back2back)) +
  geom_boxplot() +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(name = "Number of Back 2 Backs") +
  annotate("text", y = .2, x = .07, label = "The Detroit Pistons")
