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
  pesos <- dd2 |>
    select(den_cd, origem) |>
    distinct() |>
    group_by(origem) |>
    count() |>
    ungroup()

  left_join(am_raw, assunto, by = "den_cd") |>
    mutate(tipo_de_assunto = replace_na(tipo_de_assunto, "nao_informado")) |>
    inner_join(dd2, by = "den_cd")

}

limpa_texto <- function(x,
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

  x |>
    dplyr::mutate(origem = factor(origem)) |>
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
    dplyr::select(-den_texto)



}
