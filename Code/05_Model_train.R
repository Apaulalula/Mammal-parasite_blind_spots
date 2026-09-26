# Packages
pacman::p_load(xgboost,tidyverse, caret, Matrix, fastDummies, yardstick, ggplot2)

# Load data ----
bmpo_biome_final <- read.csv2("./Data/Bmpo_Biome_all.csv", sep = ",") %>% 
  glimpse()

Host_traits <- read.csv2("./Data/Host_traits.csv", sep = ",") %>%  
  mutate(across(
    c(
      diet_shannon,
      diet_axis1,
      diet_axis2,
      PhyloPC1,
      PhyloPC2,
      PhyloPC3,
      BodyMass.g
      ),
    as.numeric
    )) %>% 
  glimpse()

# Gather and transform data ----
  
bmpo_ml <- bmpo_biome_final %>% 
  filter(!BIOMAS %in% c("ocean_islands", "not_found")) %>% 
 left_join(Host_traits, by= "HostSpecies_SBMz") %>%
  select(- c(diet_axis2, PhyloPC3, threatened)) %>%
  dplyr::group_by(HostSpecies_SBMz, Parasite, sinantropia, BIOMAS) %>%
  dplyr::summarise(
    across(where(is.double), first),
    n_study = dplyr::n_distinct(ReferenceUsedParasite, na.rm = TRUE),
    parasite_richness = dplyr::n_distinct(ParasiteCurrentName, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  glimpse()

unique(bmpo_ml$HostSpecies_SBMz)

# Build model features ----
df_model <- bmpo_ml %>% 
  dummy_cols(select_columns = c("Parasite", "BIOMAS"),
             remove_first_dummy = TRUE,
             remove_selected_columns = TRUE)


write.csv(df_model, "./data/df_model.csv", row.names = FALSE)

# Make X and Y for both models ----
str(df_model)

y <- df_model$parasite_richness

X_study <- df_model %>%
  select(-HostSpecies_SBMz, -parasite_richness) %>%
  mutate_if(is.character, as.numeric) %>% 
  as.matrix()

X <- df_model %>%
  select(-HostSpecies_SBMz, -parasite_richness, - n_study) %>%
  mutate_if(is.character, as.numeric) %>% 
  as.matrix()

## divide train and test data

set.seed(42)

train_index <- caret::createDataPartition(y, p = 0.8, list = FALSE)

X_train <- X[train_index, ]
X_test  <- X[-train_index, ]

X_train_s <- X_study[train_index, ]
X_test_s  <- X_study[-train_index, ]

y_train <- y[train_index]
y_test  <- y[-train_index]

summary(y_train) # mean 4.01
sd(y_train) # 4.95

# Train model without n_study ----
# Preparando os dados
dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)

# building watchlist
watchlist <- list(train = dtrain, test = dtest)

# setting parameters
params_tweedie <- list(
  objective = "reg:tweedie",
  tweedie_variance_power = 1.4,
  eval_metric = "tweedie-nloglik@1.4",
  max_depth = 4,
  eta = 0.05,
  subsample = 0.8
)

# Cross-validation analysis
cv_model_ns <- xgb.cv(
  params = params_tweedie,
  data = dtrain,
  nrounds = 1000,
  nfold = 5,
  early_stopping_rounds = 20,
  stratified = FALSE,
  verbose = 1,
  prediction = TRUE # Retorna predições out-of-fold para calcular métricas globais
)

print(cv_model_ns, verbose = TRUE)

nrounds_cross_ns <- cv_model_ns$early_stop$best_iteration # 198

# Train model
model_ns <- xgb.train(
  params = params_tweedie,
  data = dtrain,
  nrounds = nrounds_cross_ns,
  evals = list(train = dtrain, test = dtest),
  verbose = 1
)

# evaluate model without n_study
pred <- predict(model_ns, dtest)

# Seeing model metrics
mae_test  <- mean(abs(pred - y_test)) #2.263 
medae_test <- median(abs(pred - y_test)) #1.42
rmse_test <- sqrt(mean((pred - y_test)^2)) #3.43

cat("MAE no Teste:", round(mae_test, 3), "\n")
cat("Erro Absoluto Mediano (MedAE):", round(medae_test, 3), "\n")
cat("RMSE no Teste:", round(rmse_test, 3), "\n")

correlation <- cor(pred, y_test) # 0.58
correlation
correlation^2 #0.33

importance_ns <- xgb.importance(model = model_ns)
importance_ns
#  1:             BodyMass.g 0.30159857 0.317158975 0.27681842
# 2:               PhyloPC1 0.15656911 0.149188467 0.17687075
# 3:             diet_axis1 0.12209737 0.113088365 0.09000523

#saving model without n_study

saveRDS(model_ns, file = "./Output/model_ns.model")

# Train model without n_study ----
# Preparing the data
dtrain_s <- xgb.DMatrix(data = as.matrix(X_train_s), label = y_train)
dtest_s <- xgb.DMatrix(data = as.matrix(X_test_s), label = y_test)

# Cross-validation
cv_model <- xgb.cv(
  params = params_tweedie,
  data = dtrain_s,
  nrounds = 1000,
  nfold = 5,
  early_stopping_rounds = 20,
  stratified = FALSE,
  verbose = 1,
  prediction = TRUE # Retorna predições out-of-fold para calcular métricas globais
)

print(cv_model, verbose = TRUE)

nrounds_cross <- cv_model$early_stop$best_iteration # 192

# Training model
model_s <- xgb.train(
  params = params_tweedie,
  data = dtrain_s,
  nrounds = nrounds_cross,
  evals = list(train = dtrain_s, test = dtest_s),
  verbose = 1
)

# evaluate model with n_study

pred <- predict(model_s, dtest_s)

# 4. Avaliar métricas na escala original dos dados

mae_test  <- mean(abs(pred - y_test)) #1.28
medae_test <- median(abs(pred - y_test)) #0.673
rmse_test <- sqrt(mean((pred - y_test)^2)) #2.21

cat("MAE no Teste:", round(mae_test, 3), "\n")
cat("Erro Absoluto Mediano (MedAE):", round(medae_test, 3), "\n")
cat("RMSE no Teste:", round(rmse_test, 3), "\n")

correlation <- cor(pred, y_test) #0.85
correlation^2 #0.72

importance_s <- xgb.importance(model = model_s)
importance_s

# 1:                n_study 0.754306082 0.288482281 0.19712644
# 2:             BodyMass.g 0.070923249 0.206958751 0.20402299
# 3:               PhyloPC1 0.045466983 0.118788043 0.15804598

# Saving model with n_study
saveRDS(model_s, file = "./Output/model_s.model")

## Visualize correlation ----

df_eval <- data.frame(
  Observado = y_test,
  Predito = pred,
  Residuo = y_test - pred
)

# 1. Gráfico Observado vs Predito
ggplot(df_eval, aes(x = Observado, y = Predito)) +
  geom_point(alpha = 0.6, color = "#2c3e50") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
  theme_minimal() +
  labs(title = "Valores Observados vs. Preditos no Teste",
       x = "Valor Real (y)", y = "Predição (y_hat)")

# 2. Resíduos vs Preditos
ggplot(df_eval, aes(x = Predito, y = Residuo)) +
  geom_point(alpha = 0.6, color = "#2980b9") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = FALSE, color = "orange") +
  theme_minimal() +
  labs(title = "Resíduos vs. Valores Preditos",
       x = "Predição (y_hat)", y = "Resíduo (y - y_hat)")
