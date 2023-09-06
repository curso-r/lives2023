# Scraping

res <- httr::GET("https://www.gov.br/anac/pt-br/assuntos/dados-e-estatisticas/historico-de-voos")

links <- res |>
  httr::content() |>
  rvest::html_nodes("table.plain") |>
  rvest::html_nodes("a") |>
  rvest::html_attr("href") |>
  unique()

fazer_download <- purrr::possibly(
  function(url) {
    Sys.sleep(2)
    download.file(
      url = url,
      destfile = glue::glue("2023-09-06/data-raw/{basename(url)}")
    )
  },
  otherwise = 1
)

tab_download <- tibble::tibble(
  url = links,
  flag_erro = purrr::map_dbl(links, fazer_download)
)

saveRDS(tab_download, "2023-09-06/tab_download.rds")

tab_download |>
  dplyr::filter(flag_erro == 1) |>
  View()

# Importação manual
readr::read_csv2(
  "https://www.gov.br/anac/pt-br/assuntos/dados-e-estatisticas/base-historica-1/vra/2016/VRA_do_Ms_052016.csv",
  locale = readr::locale(encoding = "latin1")
) |> 
write.csv("2023-09-06/data-raw/vra_05_2016.csv")

readr::read_csv2(
  "https://www.gov.br/anac/pt-br/assuntos/dados-e-estatisticas/base-historica-1/vra/2017/vra_do_mes082017bic.csv",
  locale = readr::locale(encoding = "latin1")
) |> 
write.csv("2023-09-06/data-raw/vra_08_2017.csv")


# Tidy

tab <- readr::read_csv2("2023-09-06/data-raw/vra_01_2022.csv")
dplyr::glimpse(tab)

tab2 <- readr::read_csv2("2023-09-06/data-raw/VRA_AbriL_2019.csv")
dplyr::glimpse(tab2)

tab_depara <- readxl::read_excel(
  "2023-09-06/glossario_de_aerodromo.xls",
  skip = 3
)
dplyr::glimpse(tab_depara)

tab |> dplyr::filter(`Código Tipo Linha` == "N")
