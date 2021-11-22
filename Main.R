library(tidyverse)

df_students <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")

df_world_tile <- readr::read_csv("https://gist.githubusercontent.com/maartenzam/787498bbc07ae06b637447dbd430ea0a/raw/9a9dafafb44d8990f85243a9c7ca349acd3a0d07/worldtilegrid.csv") %>% 
  mutate(
    ## Namibias two-digit country code is handled as `NA` - let us fix that
    alpha.2 = if_else(name == "Namibia", "NA", alpha.2),
    ## We are going to split "Americas" into "North America" and "South America"
    region = if_else(region == "Americas", sub.region, region),
    region = if_else(region %in% c("Northern America", "Central America", "Caribbean"), 
                     "North America", region),
    region = if_else(region == "Southern America", "South America", region),
    ## to join both data sets, we need a id column
    country_code = alpha.3
  )

df_ratios <- df_students %>% 
  ## Let's keep only the most recent data per country
  group_by(country, indicator) %>% 
  filter(year == max(year)) %>% 
  ungroup() %>% 
  # Create `NA`s for countries which do not have any data 2012-2018
  complete(indicator, nesting(country, country_code)) %>% 
  ## Let's focus on primary education and keep only countries (coded by letters)
  filter(
    indicator == "Primary Education",
    str_detect(country_code, "[A-Z]")
  ) %>% 
  ## merge with world tile map data
  full_join(df_world_tile) %>%
  filter(
    !is.na(region),
    !is.na(indicator)
  ) %>% 
  group_by(region) %>% 
  # Acá calculamos el PROMEDIO pero podemos calcular también la MEDIANA
  mutate(student_ratio_region = mean(student_ratio, na.rm = T)) %>% 
  ungroup()




# The Default ----
df_sorted <-
  df_ratios %>%
  mutate(region = fct_reorder(region, -student_ratio_region))


ggplot(df_sorted, aes(x = region, y = student_ratio)) +
  geom_boxplot()


ggplot(df_sorted, aes(x = region, y = student_ratio)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(limits = c(0, 90))


# Get rid of the default ----

# Para las fuentes
library(showtext)
font_add_google("Poppins", "Poppins")
font_add_google("Roboto Mono", "Roboto Mono")
showtext_auto()

theme_set(theme_light(base_size = 18, base_family = "Poppins"))

# Para paleta de colores
library(ggsci)

g <-
  ggplot(df_sorted, aes(x = region, y = student_ratio, color = region)) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 90), expand = c(0.005, 0.005)) +
  scale_color_uchicago() +
  labs(x = NULL, y = "Student to teacher ratio") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 12),
    panel.grid = element_blank()
  )

# Starting ----

g + geom_point(size = 3, alpha = 0.15)

g +
  geom_boxplot(color = "gray60", outlier.alpha = 0) +
  geom_point(size = 3, alpha = 0.15)


set.seed(2019)
g + geom_jitter(size = 2, alpha = 0.25, width = 0.2)

# alternativa usando "position_jitter"
g + geom_jitter(position = position_jitter(seed = 2019, width = 0.2), size = 2, alpha = 0.25)


# Adding Summary Statistics! ----
g +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  stat_summary(fun = mean, geom = "point", size = 5)


# Relate to baseline

# calculamos la mediana de ratios estudiante - profesor
world_avg <-
  df_ratios %>%
  summarize(avg = mean(student_ratio, na.rm = T)) %>%
  pull(avg)

g +
  geom_hline(aes(yintercept = world_avg), color = "gray70", size = 0.6) +
  stat_summary(fun = mean, geom = "point", size = 5) +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2)

# Agregamos "distancias" al promedio
g +
  geom_segment(
    aes(x = region, xend = region,
        y = world_avg, yend = student_ratio_region),
    size = 0.8
  ) +
  geom_hline(aes(yintercept = world_avg), color = "gray70", size = 0.6) +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  stat_summary(fun = mean, geom = "point", size = 5)


# Adding text boxes "let the Plot speak for Itself"! ----
(g_text <-
   g +
   geom_segment(
     aes(x = region, xend = region,
         y = world_avg, yend = student_ratio_region),
     size = 0.8
   ) +
   geom_hline(aes(yintercept = world_avg), color = "gray70", size = 0.6) +
   stat_summary(fun = mean, geom = "point", size = 5) +
   geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
   annotate(
     "text", x = 6.3, y = 35, family = "Poppins", size = 2.8, color = "gray20", lineheight = .9,
     label = glue::glue("Worldwide average:\n{round(world_avg, 1)} students per teacher")
   ) +
   annotate(
     "text", x = 3.5, y = 10, family = "Poppins", size = 2.8, color = "gray20",
     label = "Continental average"
   ) +
   annotate(
     "text", x = 1.7, y = 11, family = "Poppins", size = 2.8, color = "gray20",
     label = "Countries per continent"
   ) +
   annotate(
     "text", x = 1.9, y = 64, family = "Poppins", size = 2.8, color = "gray20", lineheight = .9,
     label = "The Central African Republic has by far\nthe most students per teacher")
)

# Agregamos flechas
arrows <-
  tibble(
    x1 = c(6.2, 3.5, 1.7, 1.7, 1.9),
    x2 = c(5.6, 4, 1.9, 2.9, 1.1),
    y1 = c(35, 10, 11, 11, 73),
    y2 = c(world_avg, 19.4, 14.16, 12, 83.4)
  )

g_text +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
    color = "gray20", curvature = -0.3
  )

# Ajustamos posiciones hasta lograr lo deseado

arrows <-
  tibble(
    x1 = c(6.1, 3.62, 1.8, 1.8, 1.8),
    x2 = c(5.6, 4, 2.18, 2.76, 1.2),
    y1 = c(world_avg + 6, 10.5, 9, 9, 77),
    y2 = c(world_avg + 0.1, 18.4, 14.16, 12, 82.45)
  )

(g_arrows <-
    g_text +
    geom_curve(
      data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
      arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
      color = "gray20", curvature = -0.3
    )
)


# ratio de 0 no tiene mucho sentido en este contexto... hacemos que parta de 1 
# y agregamos más cositas

(g_final <-
    g_arrows +
    scale_y_continuous(
      limits = c(0, 90), expand = c(0.005, 0.005),
      breaks = c(1, seq(20, 80, by = 20))
    ) +
    labs(caption = "Data: UNESCO Institute for Statistics") +
    theme(plot.caption = element_text(size = 9, color = "gray50"))
)

# Bonus: Add Tile Map as Legend ----

# Primero veamos el mapa

(map_regions <-
    df_sorted %>%
    ggplot(aes(x = x, y = y, fill = region, color = region)) +
    geom_tile(color = "white") +
    scale_y_reverse() +
    ggsci::scale_fill_uchicago(guide = F) +
    coord_equal() +
    theme(line = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = "transparent"),
          panel.border = element_rect(color = "transparent"),
          strip.background = element_rect(color = "gray20"),
          axis.text = element_blank(),
          plot.margin = margin(0, 0, 0, 0)) +
    labs(x = NULL, y = NULL)
)
