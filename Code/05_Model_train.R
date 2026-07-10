# Packages
pacman::p_load(xgboost, caret, Matrix, fastDummies, yardstick)

# Load data

bmpo_biome_final <- read.csv2("./Data/Bmpo_Biome_all.csv", sep = ",") %>% 
  glimpse() 
Host_traits <- read.csv2("./Data/Host_traits.csv", sep = ",") %>% 
  glimpse()

# Transform data
bmpo_biome_final <- bmpo_biome_final %>% 
  select(- BIOMAS)

bmpo_ml <- Host_traits %>% 
  right_join(bmpo_biome_final) %>%
  select(- c(threatened, diet_axis2)) %>% 
  mutate(across(c(diet_shannon, diet_axis1, sinantropia, threat_score, TaxoPC1, TaxoPC2), as.numeric)) %>% 
  group_by(HostSpecies_SBMz, Parasite, sinantropia, BIOMAS) %>%
  summarise(
    across(where(is.double), first),
    n_study = n_distinct(ReferenceUsedParasite),
    parasite_richness = n_distinct(ParasiteCurrentName),
    .groups = "drop"
  ) %>% 
  ungroup() %>% 
  glimpse()
  
unique(bmpo_ml$HostSpecies_SBMz)

# model
df_model <- bmpo_ml %>% 
  dummy_cols(select_columns = c("Parasite", "BIOMAS"),
             remove_first_dummy = TRUE,
             remove_selected_columns = TRUE)


write.csv(df_model, "./data/df_model.csv", row.names = FALSE)

# divide x and y
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

# divide train and test data

set.seed(42)

train_index <- caret::createDataPartition(y, p = 0.8, list = FALSE)

X_train <- X[train_index, ]
X_test  <- X[-train_index, ]

X_train_s <- X_study[train_index, ]
X_test_s  <- X_study[-train_index, ]

y_train <- y[train_index]
y_test  <- y[-train_index]

# Train model without n_study ----
# Preparando os dados
dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)

# Criando uma lista de monitoramento (watchlist)
watchlist <- list(train = dtrain, test = dtest)

# O output no console mostrará o erro de treino e teste a cada iteração.
# Se o 'test-rmse' começar a subir enquanto o 'train-rmse' cai, há overfitting.

# Define effort to be fixed

params <- list(
  objective = "count:poisson",
  eval_metric = "rmse",
  max_depth = 6,
  eta = 0.1
)

# Train model
model_ns <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 200,
  evals = list(train = dtrain, test = dtest),
  early_stopping_rounds = 20,
  verbose = 1
)

# evaluate model without n_study
summary(y_train)
sd(y_train)

pred <- predict(model_ns, dtest)

# Seeing model metrics
rmse <- sqrt(mean((pred - y_test)^2)) # 5.517982
rmse

correlation <- cor(pred, y_test) # 0.8287364
correlation

importance <- xgb.importance(model = model_ns)
importance


save(model_ns, file = "./Output/model_ns.RData")

# Train model without n_study ----
# Preparing the data
dtrain_s <- xgb.DMatrix(data = as.matrix(X_train_s), label = y_train)
dtest_s <- xgb.DMatrix(data = as.matrix(X_test_s), label = y_test)

# Training model
model_s <- xgb.train(
  params = params,
  data = dtrain_s,
  nrounds = 200,
  evals = list(train = dtrain_s, test = dtest_s),
  early_stopping_rounds = 20,
  verbose = 1
)

# Cross validation
cv_model <- xgb.cv(
  params = params,
  data = dtrain_s,
  nrounds = 150,
  nfold = 5,
  metrics = "rmse",
  early_stopping_rounds = 10 # Para o treino se o teste parar de melhorar
)

print(cv_model, verbose = TRUE)

# evaluate model with n_study

summary(y_train)
sd(y_train)

pred <- predict(model_s, dtest_s)

rmse <- sqrt(mean((pred - y_test)^2)) #1.098
rmse

correlation <- cor(pred, y_test) #0.99
correlation^2 #0.98

importance <- xgb.importance(model = model_s)
importance

save(model_s, file = "./Output/model_s.RData")
