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

tira_nova_amostra <- function(dd, amostra_clean, assunto,file){
  set.seed(22)

  cat <- c(
    "ATERRAMENTO DE RIO/MANGUE/LAGOA",
    "CONSTRUÇÃO IRREGULAR",
    "DESMATAMENTO FLORESTAL",
    "DANOS A PATRIMÔNIO PÚBLICO",
    "DESPEJO DE ESGOTO CLANDESTINO",
    "EXTRAÇÃO IRREGULAR DE ÁRVORES",
    "EXTRAÇÃO IRREGULAR DE SOLO",
    "INVASÃO DE PROPRIEDADE",
    "LOTEAMENTO IRREGULAR",
    "OBRA IRREGULAR",
    "OBSTRUÇÃO DE VIAS PÚBLICAS",
    "QUEIMADAS",
    "INCÊNDIO E PERIGO",
    "LIXO ACUMULADO",
    "DESABAMENTO E PERIGO",
    "ESTACIONAMENTO IRREGULAR"
  )
  filtro <- assunto %>% filter(
    tipo_de_assunto %in% cat
  )
  dd2 <- inner_join(dd, filtro)
  ids <- amostra_clean %>%
    pull(den_cd) %>%
    unique()
  #4050 denúncias de milícia que caem nas categorias prováveis
  novo1 <- dd2 %>%
    select(den_cd, den_texto, origem) %>%
    distinct() %>%
    filter(!(den_cd %in% ids) & origem == "milicia") %>%
    slice_sample(n = 4050)
  #450 denúncias de tráfico que caem nas categorias prováveis
  novo2 <- dd2 %>%
    select(den_cd, den_texto, origem) %>%
    distinct() %>%
    filter(!(den_cd %in% ids) & origem == "trafico") %>%
    slice_sample(n = 450)
  #500 denúncias aleatórias
  novo3 <- dd %>%
    select(den_cd, den_texto, origem) %>%
    distinct() %>%
    filter(!(den_cd %in% ids) &
             !(den_cd%in%unique(novo2$den_cd)) &
             !(den_cd %in% unique(novo2$den_cd))
    )%>%
    slice_sample(n = 500)
  novo <- bind_rows(novo1, novo2, novo3)
  readr::write_excel_csv2(novo, file = file)
  file

}

make_dd4hab <- function(dd, ass_trafic, ass_milic, shp_rio){
  a1 <- ass_trafic %>%
    rename("assunto" = tipo_de_assunto) %>%
    distinct()
  a2 <- ass_milic %>%
    mutate(assunto = if_else(is.na(tpa_ds), tipo_de_assunto, tpa_ds)) %>%
    select(den_cd, assunto) %>% distinct()
  a <- bind_rows(a1, a2) %>%
    distinct()
  dd2 <- dd %>% distinct() %>%
    inner_join(a)
  #total de denúncias antes do join
  t_den1 <- dd %>% pull(den_cd) %>% unique() %>% length()
  t_den2 <- dd2 %>% pull(den_cd) %>% unique() %>% length()



  #distribuição por ano das denúnicas perdidas
  ind <- !dd$den_cd %in% dd2$den_cd
  x <- dd %>%
    filter(ind) %>%
    select(den_cd, ano) %>%
    distinct() %>%
    tabyl(ano)
  #distribuição por tráfico ou milícia
  ind <- !dd$den_cd %in% dd2$den_cd
  x2 <- dd %>%
    filter(ind) %>%
    select(den_cd, origem) %>%
    distinct() %>%
    tabyl(origem)


  obs <- str_c("Inicialmente, são ", t_den1, " denúncias únicas. O join com o assunto reduz esse total para ", t_den2, " - uma redução de ",
               scales::percent((t_den2 - t_den1)/t_den1, accuracy = 0.01),
               ". A média do percentual de denúncias perdidas por ano é de ",
               scales::percent(mean(x$percent), 0.01), ", com desvio padrão de ",
               scales::percent(sd(x$percent), 0.01), " Todas as ", t_den1 - t_den2, ". perdidas são do tráfico")

  dd3 <- dd2 %>%
    select(den_cd, den_dt_rec, ano, den_texto, "endereco" = endereco_geocode, lat_here, lon_here, lat_ggmap, lon_ggmap,
           den_gps_lat, den_gps_long, assunto, cv, tcp, ada, milic, origem,
           matches("trafic_")) %>%
    mutate(lat = case_when(
      is.na(lat_here) & is.na(lat_ggmap) ~ den_gps_lat,
      is.na(lat_here) & !is.na(lat_ggmap) ~ lat_ggmap,
      T ~ lat_here),
      lon = case_when(
        is.na(lon_here) & is.na(lon_ggmap) ~ den_gps_long,
        is.na(lon_here) & !is.na(lon_ggmap) ~ lon_ggmap,
        T ~ lon_here)) %>%
    select(-c(lat_here, lon_here, lat_ggmap, lon_ggmap, den_gps_lat, den_gps_long)) %>%
    distinct()
  assuntos <- dd3 %>%
    select(den_cd, assunto) %>%
    distinct()
  denuncias <- dd3 %>% select(-c(assunto, cv, tcp, ada, milic, matches("trafic_"))) %>% distinct() %>%
    mutate(state = str_extract(endereco, "\\w{2}$"))
  grar <- dd3 %>% select(den_cd, cv, tcp, ada, milic, matches("trafic_")) %>%
    pivot_longer(cols = c(cv, tcp, ada, milic, matches("trafic_"))) %>%
    mutate(name = if_else(str_detect(name, "trafic_"), "facção do tráfico indefinida", name)) %>%
    distinct() %>%
    filter(value == 1) %>%
    select(-value)

  dd_sf <- denuncias %>%
    st_as_sf(coords = c("lon", "lat"))
  st_crs(dd_sf) <- 4326
  dd_sf <- st_make_valid(dd_sf)
  shp_rio <- st_make_valid(shp_rio)
  x <- st_join(dd_sf, shp_rio)
  coords <- x %>% st_coordinates() %>% as_tibble()
  x2 <- x %>% st_drop_geometry() %>%
    bind_cols(coords) %>%
    rename("lon" = X, "lat" = Y)


  names(grar) <- c("den_cd", "grar")

  x2 <- x2 %>% as.data.table()
  x2 <- x2[,den_texto  := str_replace_all(
    str_wrap(den_texto, width = 300),
    "\\n", "<br>")]



  list("denuncias" = x2,
       "assuntos" = assuntos,
       "grupos_armados" = grar,
       "obs" = obs)

}
