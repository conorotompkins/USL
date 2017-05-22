library(modelr)

mod_lm1 <- lm(goals ~ shots, data = df_raw)
mod_lm2 <- lm(goals ~ shots + passes + pass_percentage, data = df_raw)

test <- df_raw %>% 
  select(team, goals, shots, passes, pass_percentage) %>% 
  spread_predictions(mod_lm1, mod_lm2) %>% 
  #spread_residuals(mod_lm1, mod_lm2) %>% 
  gather(model, pred, -c(team, goals, shots, passes, pass_percentage))

%>% 
  spread_residuals(mod_lm1, mod_lm2) %>% 
  gather(model, resid, -c(team, goals, shots, passes, pass_percentage, pred))

ggplot(test, aes(shots, goals)) +
  geom_point() +
  geom_smooth()

ggplot(test, aes(pred, goals)) +
  geom_point() +
  facet_wrap(~model) +
  geom_smooth()

test_cor <- test %>% 
  filter(model == "mod_lm2")

cor(test_cor$goals, test_cor$pred)


test %>% 
  add_residuals(mod_lm1) %>% 
  ggplot(aes(goals, resid)) +
  geom_line() +
  geom_ref_line(h = 0, colour = "grey")
?geom_ref_line
