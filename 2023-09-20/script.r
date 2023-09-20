# Importação

dados <- readr::read_csv(
  "https://raw.githubusercontent.com/williamorim/brasileirao/master/data-raw/csv/matches.csv"
)

View(dados)

# Manipulação

time <- "Vasco da Gama"

tab_grafico <- dados |>
  dplyr::filter(
    season == 2023,
    score != "x",
    home == time | away == time
  ) |>
  tidyr::separate_wider_delim(
    cols = score,
    delim = "x",
    names = c("gols_mandante", "gols_visitante"),
    cols_remove = FALSE
  ) |>
  dplyr::arrange(date) |>
  dplyr::mutate(
    pontos = dplyr::case_when(
      home == time & gols_mandante > gols_visitante ~ 3,
      home == time & gols_mandante < gols_visitante ~ 0,
      away == time & gols_visitante > gols_mandante ~ 3,
      away == time & gols_visitante < gols_mandante ~ 0,
      TRUE ~ 1
    ),
    totalPontos = cumsum(pontos),
    rodada = dplyr::row_number()
  )

## ggplot2

library(ggplot2)

tab_grafico |>
  ggplot(aes(x = rodada, y = totalPontos)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "Rodada", y = "Total de pontos")


## echarts (primeira versão)

library(echarts4r)


tab_grafico |>
  e_chart(x = rodada) |>
  e_line(serie = totalPontos) |>
  e_color(color = "black") |>
  e_legend(show = FALSE) |>
  e_x_axis(
    name = "Rodada",
    nameLocation = "center",
    nameGap = 30
  ) |>
  e_y_axis(
    name = "Total de pontos",
    nameLocation = "center",
    nameGap = 30
  ) |>
  e_grid(
    containLabel = TRUE
  ) |>
  e_tooltip(
    formatter = htmlwidgets::JS(
      "function(params) {
        var text = '<b>Rodada</b>: ' + params.value[0];
        text += '<br>';
        text += '<b>Total de pontos</b>: ' + params.value[1];
        return text;
      }"
    )
  )

lista_dados <- tab_grafico |>
  dplyr::mutate(
    jogo = paste(home, score, away),
    date = format(date, "%d/%m/%Y")
  ) |>
  dplyr::select(
    name = rodada,
    value = totalPontos,
    date,
    jogo
  ) |>
  purrr::transpose()

View(tab_grafico)

e_chart() |>
  e_list(
    list(
      series = list(
        type = "line",
        data = lista_dados
      ),
      xAxis = list(
        type = "category",
        name = "Rodada",
        nameLocation = "center",
        nameGap = 30
      ),
      yAxis = list(
        type = "value",
        name = "Total de pontos",
        nameLocation = "center",
        nameGap = 30
      ),
      grid = list(
        containLabel = TRUE
      ),
      color = list(
        color = "black"
      ),
      tooltip = list(
        formatter = htmlwidgets::JS(
          "function(params) {
            var text = params.data.date;
            text += '<br>';
            text += params.data.jogo;
            return text;
          }"
        )
      )
    )
  )

# Função

dados <- readr::read_csv(
  "https://raw.githubusercontent.com/williamorim/brasileirao/master/data-raw/csv/matches.csv"
)

fazer_grafico <- function(dados, time) {
  tab_grafico <- dados |>
    dplyr::filter(
      season == 2023,
      score != "x",
      home == time | away == time
    ) |>
    tidyr::separate_wider_delim(
      cols = score,
      delim = "x",
      names = c("gols_mandante", "gols_visitante"),
      cols_remove = FALSE
    ) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      pontos = dplyr::case_when(
        home == time & gols_mandante > gols_visitante ~ 3,
        home == time & gols_mandante < gols_visitante ~ 0,
        away == time & gols_visitante > gols_mandante ~ 3,
        away == time & gols_visitante < gols_mandante ~ 0,
        TRUE ~ 1
      ),
      totalPontos = cumsum(pontos),
      rodada = dplyr::row_number()
    )

  lista_dados <- tab_grafico |>
    dplyr::mutate(
      jogo = paste(home, score, away),
      date = format(date, "%d/%m/%Y")
    ) |>
    dplyr::select(
      name = rodada,
      value = totalPontos,
      date,
      jogo
    ) |>
    purrr::transpose()

  echarts4r::e_chart() |>
    echarts4r::e_list(
      list(
        series = list(
          type = "line",
          data = lista_dados
        ),
        xAxis = list(
          type = "category",
          name = "Rodada",
          nameLocation = "center",
          nameGap = 30
        ),
        yAxis = list(
          type = "value",
          name = "Total de pontos",
          nameLocation = "center",
          nameGap = 30
        ),
        grid = list(
          containLabel = TRUE
        ),
        color = list(
          color = "black"
        ),
        tooltip = list(
          formatter = htmlwidgets::JS(
            "function(params) {
            var text = params.data.date;
            text += '<br>';
            text += params.data.jogo;
            return text;
          }"
          )
        )
      )
    )
}

fazer_grafico(dados, "Coritiba")

brasileirao::teams
