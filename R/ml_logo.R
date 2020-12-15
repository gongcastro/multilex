library(tidyverse)

d <- expand_grid(item = 1:50,
                 x = seq(0, 20, 0.005)) %>%
  group_by(item) %>%
  mutate(scal = rnorm(n = 1, 1, 0.1),
         xmid = rnorm(n = 1, 10, 1)) %>%
  ungroup() %>%
  mutate(y = SSlogis(input = x, xmid = xmid, Asym = 1, scal = scal))

ggplot(d, aes(x, y, colour = y)) +
  geom_line(size = 2) +
  theme_void() +
  annotate(geom = "text", x = 9.5, y = 0.5,
           label = "bold(MultiLex)", parse = TRUE,
           size = 10, colour = "black") +
  scale_color_distiller(palette = "YlOrRd") +
  theme(legend.position = "none") +
  ggsave("media/logo.png")
