baixa_excel <- function(id, name){
  drive_download(id,
                 path = name, overwrite = T)
}


le_e_limpa <- function(file){
  read_xlsx(file) |>
    clean_names() |>
    select(den_cd, matches("assunto|tpa_")) |>
    distinct() |>
    mutate(den_cd = as.numeric(den_cd))
}



le_assunto <- function(drive_dir){

  temp_dir <- tempdir()
  x<-drive_ls(drive_dir)
  nomes <- file.path(temp_dir, x$name)
  walk2(x$id, nomes, .f = baixa_excel)
  arqs <- list.files(temp_dir, full.names = T)
  arqs <- arqs[str_detect(arqs,"xlsx$")]
  map_dfr(arqs, le_e_limpa)
}

le_dd <- function(url){
  temp <- tempfile()
  drive_download(url, path = temp)
  fst::read_fst(temp)
}

le_amostra <- function(url){
  temp <- tempfile()
  drive_download("https://docs.google.com/spreadsheets/d/1Nia56Mi9Qqguge-ZW6PSHIC-i33WXHJD",
                 path = paste0(temp, ".xlsx"))
  read_xlsx(paste0(temp, ".xlsx"))
}

le_shp <- function(url){
  temp <- tempfile()
  temp_dir <- tempdir()
  drive_download(url,
                 path = paste0(temp, ".zip"))
  unzip(paste0(temp, ".zip"), exdir = temp_dir)
  arquivos <- list.files(temp_dir, full.names = T)
  arquivo <- arquivos[str_detect(arquivos, "shp$")]
  x <- st_read(arquivo)
  x <- st_transform(x,crs = 4326)
  x2 <- x %>% st_union()
  x2 %>% st_as_sf()
}
