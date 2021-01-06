library(tidyverse)
library(hexSticker)
library(showtext)

font_add_google("Indie Flower")

showtext_auto()

d <- expand_grid(item = 1:100,
                 x = seq(0, 20, 0.005)) %>%
  group_by(item) %>%
  mutate(scal = rnorm(n = 1, 3, 0.1),
         xmid = rnorm(n = 1, 10, 1.5)) %>%
  ungroup() %>%
  mutate(y = SSlogis(input = x, xmid = xmid, Asym = 1, scal = scal))

p <- ggplot(d, aes(x, y, colour = y, group = item)) +
  geom_line(size = 0.5, alpha = 0.5) +
  theme_void() +
  scale_color_distiller(palette = "YlOrRd") +
  theme(legend.position = "none")

s <- sticker(
  p,
  package = "MultiLex",
  filename = "media/logo.png",
  p_size = 40,
  p_color = "black",
  p_family = "Indie Flower",
  p_x = 1,
  p_y = 1,
  s_x = 1,
  s_y = 1,
  s_width = 1.5,
  s_height = 1.5,
  h_fill = "white",
  h_color = "black"
)

usethis::use_logo("media/logo.png")
