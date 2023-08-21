library(tidyverse)
library(brms)

items <- read_csv("data-raw/item_details.csv")
judgements <- read_csv("data-raw/judgements_all.csv")

# https://discourse.mc-stan.org/t/bradley-terry-model-in-brms-or-rstanarm-lme4-notation/28932/4

data_for_model <- judgements %>%
  filter(study == "vehicle_pairs") %>%
  select(judgement_id, judge_id, left, right, won, lost) %>%
  filter(left != 0, right != 0) %>%
  mutate(Choice = if_else(won == right, 1, 0)) %>%
  select(-won, -lost) %>%
  pivot_longer(cols = c(left, right), names_to = "position", values_to = "item") %>%
  mutate(item = paste0("V", item)) %>%
  mutate(indicator = if_else(position == "left", -1, 1)) %>%
  select(-position) %>%
  pivot_wider(names_from = item, values_from = indicator, values_fill = 0)

options(mc.cores = parallel::detectCores())

model_results <- brm(
                              # V6 is removed so scores are relative to it = bicycle
    Choice ~ V1 + V2 + V3 + V4 + V5 + V7 + V8 + V9 + V10 +
            V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 +
            V21 + V22 + V23 + V24 + V25,
    family = bernoulli(link = "logit"),
    iter = 6000,
    data = data_for_model,
    file = "brms_vehicle_pairs.rds"
  )

plot(model_results)
print(model_results)
summary(model_results)

stanplot(model_results,
         type = "areas",
         prob = 0.95)

item_scores <- fixef(model_results) %>%
  as_tibble(rownames = "item") %>%
  filter(item != "Intercept") %>%
  left_join(items %>% mutate(item = paste0("V", item_num)), join_by(item))

item_scores %>%
  mutate(item_name = fct_reorder(item_name, Estimate)) %>%
  ggplot(aes(x = item_name, y = Estimate, ymin = `Q2.5`, ymax = `Q97.5`)) +
  geom_pointrange() +
  coord_flip() +
  theme_minimal()

item_scores_btm <- read_csv("data-processed/btm_estimates.csv") %>%
  filter(study == "vehicle_pairs") %>%
  select(item_num = individual, item_name, theta, se.theta)

item_scores_btm %>%
  left_join(item_scores, join_by(item_num, item_name)) %>%
  mutate(item_name = fct_reorder(item_name, theta)) %>%
  ggplot() +
  geom_pointrange(aes(x = item_name, y = Estimate, ymin = `Q2.5`, ymax = `Q97.5`)) +
  geom_pointrange(alpha = 0.2, colour = "#003399", aes(x = item_name, y = theta, ymin = theta-se.theta, ymax = theta+se.theta)) +
  coord_flip() +
  theme_minimal() +
  labs(title = "BRM (black) agrees quite well with\nBTM values (blue)", x = "")
