# install.packages("pokemon")

library(ggplot2)

cor_azul <- "#3a62b2"
cor_amarela <- "#eebd05"

base <- pokemon::pokemon_ptbr

base_charizard <- base |>
  dplyr::filter(pokemon == "charizard")

cor_pokemon <- base |>
  dplyr::filter(pokemon == "charizard") |>
  dplyr::pull(cor_1)

sysfonts::font_add("pokemon_solid", "2023-06-21/Pokemon Solid.ttf")

showtext::showtext_auto()

base |>
  ggplot(aes(x = ataque, y = defesa)) +
  geom_point(aes(color = cor_1)) +
  geom_hline(yintercept = 100, size = 0.2) +
  geom_vline(xintercept = 100, size = 0.2) +
  ggpath::geom_from_path(
    data = base_charizard,
    aes(x = 175, y = 200, path = url_imagem),
    width = 0.35,
    height = 0.35
  ) +
  scale_color_identity() +
  labs(
    x = "Ataque",
    y = "Defesa",
    title = "Ataque vs Defesa",
    subtitle = "Charizard"
  ) +
  gghighlight::gghighlight(
    pokemon == "charizard",
    use_direct_label = FALSE
  ) +
  theme(
    panel.background = element_rect(
      fill = "white",
      color = "black"
    ),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(
      hjust = 0.5,
      color = cor_amarela,
      family = "pokemon_solid"
    ),
    plot.background = element_rect(
      fill = cor_azul,
      color = cor_azul
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 8,
      color = cor_pokemon,
      family = "pokemon_solid"
    ),
    axis.title = element_text(
      color = cor_amarela,
      family = "pokemon_solid"
    ),
    axis.text = element_text(color = cor_amarela)
  )


# -------------------------------------------------------------------------

library(ggplot2)

sysfonts::font_add(
  "pokemon_solid",
  "2023-06-21/Pokemon Solid.ttf"
)

showtext::showtext_auto()

criar_grafico_pokemon <- function(base, nome_pokemon) {
  cor_azul <- "#3a62b2"
  cor_amarela <- "#eebd05"

  base <- pokemon::pokemon_ptbr

  base_pokemon_selecionado <- base |>
    dplyr::filter(pokemon == nome_pokemon)

  cor_pokemon <- base |>
    dplyr::filter(pokemon == nome_pokemon) |>
    dplyr::pull(cor_1)

  base |>
    ggplot(aes(x = ataque, y = defesa)) +
    geom_point(aes(color = cor_1)) +
    geom_hline(yintercept = 100, size = 0.2) +
    geom_vline(xintercept = 100, size = 0.2) +
    ggpath::geom_from_path(
      data = base_pokemon_selecionado,
      aes(x = 175, y = 200, path = url_imagem),
      width = 0.35,
      height = 0.35
    ) +
    scale_color_identity() +
    labs(
      x = "Ataque",
      y = "Defesa",
      title = "Ataque vs Defesa",
      subtitle = nome_pokemon
    ) +
    gghighlight::gghighlight(
      pokemon == nome_pokemon,
      use_direct_label = FALSE
    ) +
    theme(
      panel.background = element_rect(
        fill = "white",
        color = "black"
      ),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(
        hjust = 0.5,
        color = cor_amarela,
        family = "pokemon_solid"
      ),
      plot.background = element_rect(
        fill = cor_azul,
        color = cor_azul
      ),
      plot.subtitle = element_text(
        hjust = 0.5,
        size = 8,
        color = cor_pokemon,
        family = "pokemon_solid"
      ),
      axis.title = element_text(
        color = cor_amarela,
        family = "pokemon_solid"
      ),
      axis.text = element_text(color = cor_amarela)
    )
}

base <- pokemon::pokemon_ptbr
criar_grafico_pokemon(base, nome_pokemon = "charizard")
criar_grafico_pokemon(base, nome_pokemon = "arcanine")
criar_grafico_pokemon(base, nome_pokemon = "rapidash")
criar_grafico_pokemon(base, nome_pokemon = "blastoise")
criar_grafico_pokemon(base, nome_pokemon = "starmie")
criar_grafico_pokemon(base, nome_pokemon = "snorlax")
criar_grafico_pokemon(base, nome_pokemon = "onix")
criar_grafico_pokemon(base, nome_pokemon = "mewtwo")
criar_grafico_pokemon(base, nome_pokemon = "entei")
criar_grafico_pokemon(base, nome_pokemon = "raikou")
criar_grafico_pokemon(base, nome_pokemon = "articuno")






