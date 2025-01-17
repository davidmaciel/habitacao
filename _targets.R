# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble") # Packages that your targets need for their tasks.
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  #
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
googledrive::drive_auth()
set.seed(22)
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
tar_plan(
  #importação####
  drive_trafico = "https://drive.google.com/drive/folders/1hObuFljCRorN9kT6lDyM8phurqODt6m2",
  drive_milicia = "https://drive.google.com/drive/folders/1t2dO0kc-Q9RLs5vyjTZFh9oEzWELA3Oj",
  file_amostra = "https://docs.google.com/spreadsheets/d/1i-lv310xUTSiS8SNisLh_ied_Z8Jg9dg",
  file_dd = "https://drive.google.com/file/d/1PnKd0BwUCIy3tiwTget-hL_cc4Y2XJwC",
  file_pt = "pt_BR.dic",
  file_shp= "https://drive.google.com/file/d/1-xDpaKYi7fcJsIrpJRMIrKQLFhSlADcr",
  dd = le_dd(file_dd),
  ass_trafic = le_assunto(drive_trafico),
  ass_milic = le_assunto(drive_milicia),
  am_raw = le_amostra(file_amostra),
  shp_rio = le_shp(file_shp),
  #construção e limpeza####
  assunto = faz_assunto(ass_milic, ass_trafic),
  amostra = faz_amostra(am_raw, dd, assunto),
  amostra_clean  = limpa_amostra(amostra, stopwords_source = "stopwords-iso"),
  nova_amostra = tira_nova_amostra(dd, amostra_clean, assunto,file = "nova_amostra.csv"),
  dd4hab = make_dd4hab(dd, ass_trafic, ass_milic, shp_rio),
  #modelos gerais###########
  gbt =   modelo <-
    parsnip::boost_tree(
      mtry = tune::tune(),
      trees = tune::tune(),
      tree_depth = tune::tune(),
      learn_rate = tune::tune(),
      loss_reduction = tune::tune(),
      sample_size = tune::tune()
    ) |>
    parsnip::set_engine("xgboost") |>
    parsnip::set_mode("classification"),
  #wkl para intermediação de acesso à terra####

  split_inic_ict = initial_split(amostra_clean, strata =intermediacao_acesso_terra),
  treino_ict  = training(split_inic_ict),
  rec_ict = make_rec(treino_ict, col = "intermediacao_acesso_terra"),
  wkl_ict = workflow() |> add_recipe(rec_ict) |> add_model(gbt),
  param_ict = finalize_param(wkl_ict),
  grade_ict = grid_latin_hypercube(param_ict, size = 384),
  fold_ict = make_fold(treino_ict, v = 5, repeats = 6, strata_var = intermediacao_acesso_terra),
  tune1st_ict = first_tune(wkl_ict, fold = fold_ict, param = param_ict, grade = grade_ict, cores = 4,
                           metrics = metric_set(f_meas,
                                                sens,
                                                spec,
                                                precision,
                                                pr_auc,
                                                roc_auc,
                                                brier_class,
                                                bal_accuracy)),
  tune2nd_ict = second_tune(wkl = wkl_ict,
                            fold = fold_ict,
                            metrics = metric_set(f_meas,
                                                           sens,
                                                           spec,
                                                           precision,
                                                           pr_auc,
                                                           roc_auc,
                                                           brier_class,
                                                           bal_accuracy),
                            param = param_ict,  initial = tune1st_ict, cores = 5),
  ajustado1 = ajuste_final(tuned = tune2nd_ict,
                           wkl = wkl_ict,
                           split = split_inic_ict,
                           best_metric = "f_meas"),
  tar_file(out_ajustado1, write_file(ajustado1, "output/ajustado1.rds"))



)
