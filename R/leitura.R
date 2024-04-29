
le_assunto <- function(file){
  temp <- tempfile()
  drive_download(url, path = temp)
  readxl::read_excel(temp) |>
    clean_names() |>
    select(den_cd, matches("assunto|tpa")) |>
    distinct()
}


make_milic <- function(){
  milic <- list.files(here("data-raw/milicia/"), full.names = T)
  trafic <- list.files(here("data-raw/trafico/"), full.names = T)


}

x<-drive_ls("https://drive.google.com/drive/folders/1t2dO0kc-Q9RLs5vyjTZFh9oEzWELA3Oj")
drive_download(x$id[1], path = "temp.xlsx")
