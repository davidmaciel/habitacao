make_workflow_ict <- function(treino_ict){

  dummy_cols <- sapply(treino_ict, is.factor)
  dummy_cols <- dummy_cols[dummy_cols == T] |> names()
  pesos <- treino_ict |>
    tabyl(intermediacao_acesso_terra)
  peso <- pesos$n[2]/pesos$n[1]

  treino_ict <- treino_ict |>
    mutate(importancia = if_else(intermediacao_acesso_terra == "sim", peso, 1),
           importancia = importance_weights(importancia))

  rec <- recipe(intermediacao_acesso_terra ~.,
                     data = treino_ict) |>
    update_role(den_cd, den_texto, new_role = "ID") |>

    step_dummy(all_of(dummy_cols)) |>

    textrecipes::step_tokenize(den_texto_norm, token = "words") |>
    textrecipes::step_ngram(den_texto_norm, num_tokens = 1, min_num_tokens = 1) |>
    textrecipes::step_tokenfilter(den_texto_norm,
                                  min_times = 1,
                                  max_tokens = tune::tune()
                                  ) |>
    textrecipes::step_tfidf(den_texto_norm) |>
    step_zv(all_predictors()) |>
    recipes::step_normalize(recipes::all_predictors()) |>
    recipes::step_pca(all_predictors(),
                      num_comp = tune::tune()
                      ) |>
    themis::step_smote(recipes::all_outcomes(),
                       over_ratio = tune::tune(),
                       neighbors = tune::tune()
                       )
# rec |> prep() |> bake(new_data = treino_ict)

modelo <- parsnip::boost_tree(mtry = tune::tune(), trees = tune::tune(), tree_depth = tune::tune(),
                              learn_rate = tune::tune(), loss_reduction = tune::tune(),
                              sample_size = tune::tune()) |>
  parsnip::set_engine("xgboost") |>
  parsnip::set_mode("classification")

workflow() |>
  add_recipe(rec) |>
  add_model(modelo) |>
  add_case_weights(importancia)


}


tune_ict <- function(wflow_ict, vfold, metrics, grid_size){
  #parâmetros

  param <- hardhat::extract_parameter_set_dials(wflow_ict) |>
    update(mtry = dials::mtry(range = c(1,100)),
           trees = dials::trees(range = c(1,100)),
           tree_depth = dials::tree_depth(range = c(1,25)),
           learn_rate = dials::learn_rate(range = c(0.001, 0.3), trans = NULL),
           loss_reduction = dials::loss_reduction(range = c(0,1), trans = NULL),
           sample_size = dials::sample_prop(range= c(0.5,1))) |>
    dials::finalize()

  #grade para tunagem
  grade <- grid_latin_hypercube(param, size = 10)


  tune_grid(wflow_ict,
                            resamples = fold_ict,
                            metrics = metric_set(sensitivity, spec),
                            param_info = param,
                            grid = grade)


}


