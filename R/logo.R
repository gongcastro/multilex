library(tidyverse)
library(hexSticker)
library(showtext)

font_add_google("Nunito", "nunito")

showtext_auto()

d <- expand_grid(item = 1:100,
                 x = seq(0, 20, 0.005)) %>%
  group_by(item) %>%
  mutate(scal = rnorm(n = 1, 3, 0.1),
         xmid = rnorm(n = 1, 10, 1.5)) %>%
  ungroup() %>%
  mutate(y = SSlogis(input = x, xmid = xmid, Asym = 0.75, scal = scal))

p <- ggplot(d, aes(x, y, colour = y, group = item)) +
  geom_line(size = 0.5, alpha = 0.5) +
  theme_void() +
  scale_color_distiller(palette = "YlOrRd") +
  scale_y_continuous(limits = c(0, 1)) +
  theme(legend.position = "none")

s <- sticker(
  p,
  package = "MultiLex",
  filename = "media/logo.png",
  p_size = 30,
  p_color = "black",
  p_family = "nunito",
  p_x = 1,
  p_y = 1,
  s_x = 1,
  s_y = 1.1,
  s_width = 1.9,
  s_height = 1.4,
  h_fill = "grey",
  h_color = "black"
)

usethis::use_logo("media/logo.png")
