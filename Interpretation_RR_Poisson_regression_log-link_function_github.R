# Load tidyverse package
library(tidyverse)

# Create data
rr.data <- tibble(
  x1 = 10^seq(from = 0, to = 3, length = 100),
  x2 = 2*x1,
  "x2/x1" = x2/x1,
  "(x2 + 1)/(x1 + 1)" = (x2 + 1)/(x1 + 1)) %>%
  gather(
    key = "Manier",
    value = "Factor",
    "x2/x1", "(x2 + 1)/(x1 + 1)")

# Visualize the factor as function of x1
ggplot(
  data = rr.data,
  mapping = aes(x = x1, y = Factor, colour = Manier, linetype = Manier)) +
  geom_line() +
  scale_x_continuous(
    breaks = c(outer(c(1, 2, 5), 10^(0:3))),
    minor_breaks = NULL) +
  coord_trans(x = "log10") +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.title = element_blank()
    ) +
  scale_color_manual(name = "Manier",
    values = c('darkgrey', 'black')) +
  scale_linetype_manual(name = "Manier",
    values = c("solid", "dotted"))
