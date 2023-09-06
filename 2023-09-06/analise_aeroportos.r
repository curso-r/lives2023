# Scrape

url <- "https://www.gov.br/anac/pt-br/assuntos/dados-e-estatisticas/historico-de-voos"

res <- httr::GET(url)
res

links <- res |>
  httr::content() |>
  rvest::html_nodes("table") |>
  rvest::html_nodes("a") |>
  rvest::html_attr("href") |>
  unique()

links

# safe_sum <- purrr::possibly(
#   sum,
#   otherwise = NA
# )

# safe_sum(c(1, 2, 3))
# safe_sum(c("a", "b"))



fazer_download <- purrr::possibly(
  function(url) {
    sys.sleep(2)
    download.file(
      url = url,
      destfile = glue::glue("2023-09-06/data-raw/{basename(url)}"),
    )
  },
  otherwise = 1
)

tab_download <- tibble::tibble(
  url = links,
  flag_erro = purrr::map_dbl(url, fazer_download)
)

tab_download <- readRDS("2023-09-06/tab_download.rds")
View(tab_download)

tab_download |>
  dplyr::filter(flag_erro == 1) |>
  View()

# Tidy

tab_01_2021 <- readr::read_csv2(
  "2023-09-06/data-raw/vra_01_2021.csv",
)

View(tab_01_2021)
dplyr::glimpse(tab_01_2021)


tab_12_2017 <- readr::read_csv2(
  "2023-09-06/data-raw/VRA12_2017.csv",
  locale = readr::locale(encoding = "latin1")
)

tab_12_2017 <- data.table::fread(
   "2023-09-06/data-raw/VRA12_2017.csv",
   encoding = "Latin-1"
)

View(tab_12_2017)

arquivos <- list.files(
  "2023-09-06/data-raw/",
  pattern = "_2023_",
  full.names = TRUE
)

tab <- purrr::map(
  arquivos,
  ~ readr::read_csv2(.x, col_types = paste(rep("c", 11), collapse = ""))
) |>
  dplyr::bind_rows() |>
  janitor::clean_names()

dplyr::glimpse(tab)


tab_depara_aero <- readxl::read_excel(
  "2023-09-06/glossario_de_aerodromo.xls",
  skip = 3
) |>
  janitor::clean_names()

tab <- tab |>
  dplyr::left_join(
    tab_depara_aero |> dplyr::select(sigla_oaci, descricao, pais),
    by = c("sigla_icao_aeroporto_origem" = "sigla_oaci")
  )

tab |>
  dplyr::filter(
    pais == "BRASIL",
    codigo_tipo_linha %in% c("I")
  ) |>
  dplyr::distinct(sigla_icao_aeroporto_origem, descricao) |> 
  View()
