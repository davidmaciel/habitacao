make_rec <- function(treino, col){


  #formula para a receita
  form <- as.formula(paste0(col, " ~ ."))
  #variáveis para dumificar
  dummy_cols <- sapply(treino, is.factor)
  dummy_cols <- dummy_cols[dummy_cols == T] |> names()
  dummy_cols <- dummy_cols[!dummy_cols == col]

  #variáveis de id
  id_cols <- c("den_cd","den_texto")

  recipe(formula = form ,
                data = treino) |>
    update_role(all_of(id_cols), new_role = "ID") |>
    step_dummy(all_of(dummy_cols)) |>
    textrecipes::step_tokenize(den_texto_norm, token = "words") |>
    textrecipes::step_ngram(den_texto_norm,
                            num_tokens = 1,
                            min_num_tokens = 1) |>

    step_tfidf(den_texto_norm) |>
    step_zv(all_double_predictors()) |>
    step_normalize(all_double_predictors()) |>
    step_pca(all_double_predictors(), threshold = tune()) |>
    step_smote(has_role("outcome"),over_ratio = tune(), neighbors = tune(), seed = 22)


}

finalize_param <- function(wkl){
  hardhat::extract_parameter_set_dials(wkl) |>
    update(mtry = dials::mtry(range = c(1,100)),
           trees = dials::trees(range = c(1,100)),
           tree_depth = dials::tree_depth(range = c(1,25)),
           learn_rate = dials::learn_rate(range = c(0.001, 0.3), trans = NULL),
           loss_reduction = dials::loss_reduction(range = c(0,1), trans = NULL),
           sample_size = dials::sample_prop(range= c(0.5,1)),
           over_ratio = dials::over_ratio(range = c(0.5, 1), trans = NULL),
           neighbors = dials::neighbors(range = c(5, 15), trans = NULL)) |>
    dials::finalize()

}

make_fold <- function(data, v, repeats, strata_var){
  vfold_cv(data, v = 5, repeats = 6, strata = {{strata_var}})
}



first_tune <- function(wkl, fold, metrics, param, grade, cores = 5){
  # tar_load(c(wkl_ict, param_ict, grade_ict, fold_ict))

  future::plan(multisession, workers = cores)
  on.exit(future::plan(sequential))
  print(plan())

  finetune::tune_race_anova(object = wkl,
                  param_info = param,
                  grid = grade,
                  resamples = fold,
                  metrics = metrics,
                  control = control_race(verbose = T, verbose_elim = T, allow_par = T)
  )


}

second_tune <- function(wkl, fold, metrics, param, initial, cores = 5){

  future::plan(multisession, workers = cores)
  on.exit(future::plan(sequential))
  print(plan())

  tune_bayes(wkl,
             resamples = fold,
             param_info = param,
             metrics = metrics,
             initial = initial,
             iter = 100,
             control = control_bayes(verbose = T, no_improve = 100,
                                     uncertain = 10, seed = 22, allow_par = T)
             )


}

ajuste_final <- function(tuned,
                         best_metric,
                         wkl,
                         split,
                         metrics = yardstick::metric_set(
                           bal_accuracy,
                           precision,
                           average_precision,
                           pr_auc,
                           f_meas,
                           roc_auc,
                           sens,
                           yardstick::spec
                         )){

  best_result <- tuned |>
    select_best(metric = best_metric)
  best_wkl <- wkl |>
    finalize_workflow(best_result)
  last_fit(best_wkl, split = split, metrics = metrics)


}

# acha_limite <- function(ajustado, metrics =
#                           metrics = yardstick::metric_set(
#                             precision,
#                             sens,
#                             yardstick::spec
#                           )){
#   test_results <- ajustado$.predictions[[1]] |>
#     mutate(no_skill = sample(c("sim","não"), replace = T, size = nrow(test_results)) |>
#              factor(levels = c("sim","não")))
#
#   metrics(data = test_results, truth = intermediacao_acesso_terra, estimate = no_skill)
#   x <- test_results |>
#     threshold_perf(truth = intermediacao_acesso_terra, estimate = .pred_sim,
#                    thresholds = seq(0.5,1, by = 0.01), metrics = metrics) |>
#     filter(.metric != "distance")
#   ggplot(x, aes(x = .threshold, y = .estimate, col = .metric)) +
#     geom_line() +
#     scale_x_continuous("Limite", breaks = seq(0.5, 1, by = 0.05)) +
#     scale_y_continuous("Estimativa", breaks = seq(0, 1, by = 0.05)) +
#     geom_hline(yintercept = 0.486)
#
# }

write_file <- function(x, path){
  write_rds(x, file = path)
  path
}
