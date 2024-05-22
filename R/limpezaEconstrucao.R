faz_assunto <- function(ass_milic, ass_trafic){
  ass_milic <- ass_milic |>
    mutate(tipo_de_assunto =
             if_else(is.na(tipo_de_assunto), tpa_ds, tipo_de_assunto)) |>
    select(den_cd, tipo_de_assunto)
  bind_rows(ass_milic, ass_trafic) |>
    filter(!is.na(den_cd))
}

faz_amostra <- function(am_raw, dd, assunto){
  # tar_load(c(am_raw, dd, assunto))
  am_raw <- am_raw |>
    mutate(across(where(is.numeric), ~replace_na(.x, 0)))
  dd2 <- dd |>
    select(den_cd, ano, den_logr_uf, den_logr_bairro, den_logr_mun,
           matches("_prob"), cv, tcp, ada, milic, matches("trafic_"),
           sem_grupo, policia, origem) |> distinct()

  #limpeza do assunto
  assunto2 <- assunto |>
    filter(den_cd %in% unique(am_raw$den_cd)) |>
    fastDummies::dummy_cols(select_columns = "tipo_de_assunto", remove_first_dummy = T,
                            remove_selected_columns = T) |>
    clean_names() |>
    group_by(den_cd) |>
    summarise(across(is.numeric, ~sum(.x, na.rm = T))) |>
    ungroup() |>
    mutate(across(-den_cd, function(x){ifelse(x >=1, 1, 0)}))



  left_join(am_raw, assunto2, by = "den_cd") |>
    mutate(
      tipo_de_assunto_si = if_else(
        is.na(tipo_de_assunto_ameaca), 1, 0)) |>
    mutate(across(matches("tipo_de_"), ~replace_na(.x, 0))) |>
    inner_join(dd2, by = "den_cd")

}

limpa_amostra <- function(x,
                            stopwords_source = c("stopwords-iso",
                                                 "snowball",
                                                 "nltk")){

  stopwords_source <- match.arg(stopwords_source)

  #stopwords para remoção
  s <- stopwords::stopwords(language = "pt", source = stopwords_source)
  s <- glue::glue("\\b{s}\\b")  |>
    stringr::str_c(collapse = "|")
  s <- stringr::str_c("(",s, ")")

  #dicionário para lematização


  dic <- textstem::make_lemma_dictionary(x[["den_texto"]], lang = "pt_BR")

  x  <- x |>
    dplyr::mutate(den_texto_norm =
                    stringr::str_replace_all(
                      den_texto,
                      '"\\w+"|"\\w+\\s\\w+"|\\w+\\s\\w+\\s\\w+"',
                      'pessoa'
                    ) |>
                    stringr::str_remove_all("[:PUNCT:]") |>
                    stringr::str_replace_all('[\\w\\-.]+?@\\w+?\\.\\w{2,4}\\b', 'emailaddr') |>
                    stringr::str_replace_all('(http[s]?\\S+)|(\\w+\\.[A-Za-z]{2,4}\\S*)', 'httpaddr') |>
                    stringr::str_replace_all('(\\d{1,2}[h:]\\d{1,2}[m.\\s])|(\\d{1,2}[h:])', 'timex') |>
                    stringr::str_replace_all('(\\d{1,2}\\/\\d{1,2}\\/\\d{2,4})', 'date') |>
                    stringr::str_replace_all('(R[$]\\s)|(R[$])', 'reais') |>
                    stringr::str_replace_all('\\b(\\d{2}\\s\\d{4,5}\\-\\d{4})', 'phonenumber') |>
                    stringr::str_replace_all('\\d+(\\.\\d+)?', 'numbr') |>
                    stringr::str_replace_all('[^a-zA-ZÀ-ÿ]', ' ') |>
                    stringr::str_squish() |>
                    stringr::str_trim()) |>
    dplyr::mutate(den_texto_norm =
                    stringr::str_remove_all(den_texto_norm,
                                            stringr::regex(s, ignore_case = T)) |>
                    stringr::str_trim() |>  stringr::str_squish() |>
                    stringr::str_to_lower() |>
                    textstem::lemmatize_strings(dictionary = dic) |>
                    stringi::stri_trans_general(id = "Latin-ASCII")) |>
    #transformação das variáveis em fator
    select(-origem.x, -den_logr_uf, -den_logr_bairro) |>
    rename("origem" = origem.y) |>
    mutate(origem = factor(origem),
           den_logr_mun = factor(den_logr_mun)) |>
    clean_names()
  vars <- c("cv","tcp","ada","milic","trafic_na","trafic_terge","trafic_noun",
            "sem_grupo","policia", "intermediacao_acesso_terra",
            "produca_habitacional", "intermediacao_producao_publica", "controle_producao_privada",
            "construcao_de_barricadas_e_fechamento_de_ruas", "intervencao_associativismo",
            "colaboracao_associativismo")
  x |>
    mutate(across(all_of(vars), ~factor(.x, levels = c(1,0), labels = c("sim","não")))) |>
    mutate(ano = ano - min(ano)) #|>
    # mutate(peso = if_else(origem == "trafico", 9.975, 1))


}

faz_treino_ict <- function(split){
  # tar_load(split_inic_ict)
  treino_ict <- training(split)

  pesos <- treino_ict |>
    tabyl(intermediacao_acesso_terra)

  peso <- pesos$n[2]/pesos$n[1]

   treino_ict |>
    mutate(case_wts = if_else(intermediacao_acesso_terra == "sim", peso, 1))
}

tira_nova_amostra <- function(dd, amostra_clean, file){
  set.seed(22)
  ids <- amostra_clean %>%
    pull(den_cd) %>%
    unique()
  novo <- dd %>%
    select(den_cd, den_texto, origem) %>%
    distinct() %>%
    filter(!(den_cd %in% ids) & origem == "milicia") %>%
    slice_sample(n = 5000)
  readr::write_excel_csv2(novo, file = file)
  file

}
