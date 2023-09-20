# manip

dados <- readr::read_csv("https://raw.githubusercontent.com/williamorim/brasileirao/master/data-raw/csv/matches.csv")

tab_grafico <- dados |>
  dplyr::filter(
    season == 2023,
    score != "x",
    home == "São Paulo" | away == "São Paulo"
  ) |>
  tidyr::separate_wider_delim(
    cols = "score",
    names = c("home_score", "away_score"),
    delim = "x"
  ) |>
  dplyr::arrange(date) |>
  dplyr::mutate(
    pontos = dplyr::case_when(
      home == "São Paulo" & home_score > away_score ~ 3,
      away == "São Paulo" & away_score > home_score ~ 3,
      home == "São Paulo" & home_score == away_score ~ 1,
      away == "São Paulo" & home_score == away_score ~ 1,
      TRUE ~ 0
    ),
    total_pontos = cumsum(pontos),
    rodada = dplyr::row_number()
  )

# ggplot

library(ggplot2)

tab_grafico |>
  ggplot(aes(x = rodada, y = total_pontos)) +
  geom_line(color = "red") +
  geom_point() +
  labs(y = "Total de pontos", x = "Rodada") +
  scale_x_continuous(breaks = c(1, 10, 20, 30)) +
  theme_minimal()

# echarts simples

library(echarts4r)

tab_grafico |>
  e_charts(x = rodada) |>
  e_line(
    serie = total_pontos,
    itemStyle = list(color = "black"),
    lineStyle = list(color = "red")
  ) |>
  e_legend(show = FALSE) |>
  e_tooltip(
    formatter = htmlwidgets::JS(
      "function(params) {
        var text = 'Rodada: ' + params.value[0] + '<br>';
        text += 'Total de pontos: ' + params.value[1];
        return text;
      }"
    )
  )

# echarts com e_list()



e_charts() |>
  e_list(
    list(
      series = list(
        list(
          type = "line",
          data = tab_grafico |>
            dplyr::select(
              name = rodada,
              value = total_pontos
            ) |>
            purrr::transpose(),
          itemStyle = list(color = "black"),
          lineStyle = list(color = "red")
        )
      ),
      xAxis = list(
        type = "category"
      ),
      yAxis = list(
        type = "value"
      )
    )
  )
