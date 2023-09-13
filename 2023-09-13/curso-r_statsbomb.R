#' @description
#'  Script para explorar dados de jogos do Brasil na Copa do Mundo Feminina 2023

# Executar os comandos abaixo caso as bibliotecas não estejam instaladas
# install.packages("devtools")
# devtools::install_github("statsbomb/SDMTools")
# devtools::install_github("statsbomb/StatsBombR")
# install.packages("ggplot2")
# install.packages("ggsoccer")

# Coletar dados -----------------------------------------------------------

competicoes <- StatsBombR::FreeCompetitions()
competicoes |> dplyr::glimpse()

copa_2023 <- competicoes |>
  dplyr::filter(competition_name == "Women's World Cup" &
                  season_name == "2023")

a_maior_selecao_de_futebol_do_planeta <- "Brazil"
jogos_copa_23 <- StatsBombR::FreeMatches(copa_2023)
jogos_brasil <- jogos_copa_23 |>
  dplyr::filter(home_team.country.name == a_maior_selecao_de_futebol_do_planeta |
                  away_team.country.name == a_maior_selecao_de_futebol_do_planeta)

dados_jogos_brasil <- StatsBombR::free_allevents(jogos_brasil)

# Ações em um jogo --------------------------------------------------------

dados_jogos_brasil |>
  dplyr::distinct(match_id, team.name) |>
  dplyr::filter(!stringr::str_detect(team.name, a_maior_selecao_de_futebol_do_planeta))

brasil_panama <- dados_jogos_brasil |>
  dplyr::filter(match_id == 3893801)

# Quantaas ações de cada tipo aconteceram no jogo
brasil_panama |>
  dplyr::count(type.name) |>
  dplyr::arrange(dplyr::desc(n))

# Visualizar a quantidade de ações por time
brasil_panama |>
  dplyr::filter(type.name %in% c("Pass", "Carry", "Pressure",
                                 "Duel", "Dribble", "Clearance",
                                 "Shot", "Block", "Interception", "Foul Won")) |>
  dplyr::mutate(team_period = stringr::str_c(team.name, " ", period, " tempo")) |>
  dplyr::count(type.name, team_period) |>
  dplyr::arrange(dplyr::desc(n)) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = type.name, y = n, fill = type.name) +
  ggplot2::geom_col() +
  ggplot2::scale_fill_viridis_d() +
  ggplot2::facet_wrap(~ team_period) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 60, vjust = 0.5),
  )

# Action map --------------------------------------------------------------

acao <- "Shot"
brasil_panama |>
  StatsBombR::cleanlocations() |>
  dplyr::filter(type.name == acao) |>
  dplyr::mutate(
    location.x = ifelse(
      stringr::str_detect(team.name, a_maior_selecao_de_futebol_do_planeta),
      location.x, 120 - location.x),
    location.y = ifelse(
      stringr::str_detect(team.name, a_maior_selecao_de_futebol_do_planeta),
      location.y, 80 - location.y)
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = location.x, y = location.y, size = shot.statsbomb_xg,
      color = team.name) +
  ggsoccer::annotate_pitch(
    dimensions = ggsoccer::pitch_statsbomb
  ) +
  ggsoccer::theme_pitch() +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::scale_color_viridis_d(option = "H")

# Visualizar uma posse ----------------------------------------------------

posse <- 94 #Terceiro gol do Brasil
brasil_panama |>
  dplyr::filter(type.name %in% c("Pass", "Carry", "Shot")) |>
  StatsBombR::cleanlocations() |>
  dplyr::filter(possession == posse) |>
  dplyr::rowwise() |>
  dplyr::mutate(end_location.x = sum(pass.end_location.x,
                                     carry.end_location.x,
                                     shot.end_location.x, na.rm = T),
                end_location.y = sum(pass.end_location.y,
                                     carry.end_location.y,
                                     shot.end_location.y, na.rm = T)) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = location.x, y = location.y,
               xend = end_location.x, yend = end_location.y,
               linetype = type.name, shape = type.name) +
  ggsoccer::annotate_pitch(
    dimensions = ggsoccer::pitch_statsbomb
  ) +
  ggsoccer::theme_pitch() +
  ggplot2::geom_segment() +
  ggplot2::scale_linetype_manual(
    breaks = c("Carry", "Pass", "Shot"),
    values = c("dotted", "solid", NA)
  ) +
  ggplot2::geom_point() +
  ggplot2::scale_shape_manual(
    breaks = c("Carry", "Pass", "Shot"),
    values = c(16, 16, 9)
  )


