library(tidyverse)
library(devtools)
library(fpp3)
load_all()

tourism_full <- tourism %>% 
  aggregate_key((State/Region), Trips = sum(Trips))

fit <- tourism_full %>% 
  slide_tsibble(.size = 60) %>% 
  relocate(.id) %>% 
  filter(.id <= 2) %>% # only size 2 for Cross-validation
  # modelling
  model(base = ETS(Trips)) |>
  reconcile(
    ols = min_trace(base, method = "ols"),
    mint = min_trace(base, method = "mint_shrink")
  )

fc <- fit %>% 
  forecast(h = 4) %>% 
  group_by(.id, .model, State, Region) %>% 
  mutate(h = row_number()) %>%
  ungroup() %>%
  as_fable(response = "Trips", distribution = Trips)


accuracy_1 <- fc %>% 
  accuracy(
    tourism_full,
    by = c("h", ".model"),
    measures = list(RMSE = RMSE)
  )

accuracy_2 <- fc %>% 
  accuracy(
    tourism_full,
    by = c("h", ".model", "State", "Region"),
    measures = list(RMSE = RMSE)
  ) %>% 
  group_by(.model, h) %>%
  summarise(RMSE = sqrt(mean(RMSE^2)), .groups = "drop")


accuracy_1 %>% 
  ggplot(aes(x = h, y = RMSE, color = .model)) +
  geom_line() +
  labs(title = "RMSE by model, h, State, and Region")

accuracy_2 %>%
  ggplot(aes(x = h, y = RMSE, color = .model)) +
  geom_line() +
  labs(title = "RMSE by model and h")

