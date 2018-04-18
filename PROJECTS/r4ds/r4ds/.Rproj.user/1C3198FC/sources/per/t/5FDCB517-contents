getwd()

library(tidyverse)

ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_hex()
ggsave("diamonds.pdf")

write_csv(diamonds, "diamonds.csv")

