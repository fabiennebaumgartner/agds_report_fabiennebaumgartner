# ./R/ml_utils.R

# for re_ml_01.Rmd

eval_model <- function(mod, df_train, df_test, return_metrics = FALSE){
  # Clean and predict
  df_train <- df_train %>%
    drop_na() %>%
    mutate(fitted = predict(mod, newdata = .))
  
  df_test <- df_test %>%
    drop_na() %>%
    mutate(fitted = predict(mod, newdata = .))
  
  # Compute metrics
  metrics_train <- yardstick::metrics(df_train, truth = GPP_NT_VUT_REF, estimate = fitted)
  metrics_test  <- yardstick::metrics(df_test, truth = GPP_NT_VUT_REF, estimate = fitted)
  
  if (return_metrics) {
    return(list(train = metrics_train, test = metrics_test))
  }
  
  # If not returning metrics, return plots (for visual inspection)
  gg1 <- df_train %>%
    ggplot(aes(GPP_NT_VUT_REF, fitted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    labs(subtitle = bquote(italic(R)^2 == .(format(metrics_train %>% filter(.metric == "rsq") %>% pull(.estimate), digits = 2)) ~~
                             RMSE == .(format(metrics_train %>% filter(.metric == "rmse") %>% pull(.estimate), digits = 2))),
         title = "Training set") +
    theme_classic()
  
  gg2 <- df_test %>%
    ggplot(aes(GPP_NT_VUT_REF, fitted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    labs(subtitle = bquote(italic(R)^2 == .(format(metrics_test %>% filter(.metric == "rsq") %>% pull(.estimate), digits = 2)) ~~
                             RMSE == .(format(metrics_test %>% filter(.metric == "rmse") %>% pull(.estimate), digits = 2))),
         title = "Test set") +
    theme_classic()
  
  return(cowplot::plot_grid(gg1, gg2))
}
