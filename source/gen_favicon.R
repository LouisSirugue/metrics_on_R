
library(tidyverse)
source(paste0(getwd(), "/../source/style.R"))

dat <- tibble(x = rchisq(10000, 30, ncp = 0)) 

dat <- tibble(x = rnorm(1000, 0, 1)) 

ggplot(dat, aes(x = x)) + 
  
  geom_histogram(aes(y = ..density..), bins = 8, fill = "#014D64", color = NA, alpha = 1) +
  geom_density(bw = .4, fill = "#6794A7", color =NA, trim = F, alpha = .8) +
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(75, 0, 75, 0))

ggsave("metrics_logo.png", height = 5, width = 5)

test <- tibble(x = rnorm(2000, 0, 1),
               y = 4*x + rnorm(2000, 0, 4))

ggplot(test, aes(x = x, y = y)) + 
  geom_point(alpha = .3, size = 3, color = "#014D64") +
  geom_segment(aes(x = min(x), xend = max(x),
                   y = lm(y~x,test)$coefficients[1] + min(x) * lm(y~x,test)$coefficients[2],
                   yend = lm(y~x,test)$coefficients[1] + max(x) * lm(y~x,test)$coefficients[2]),
               size = 7, color = "#86D7F2") +
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank())

ggsave("metrics_logo2.png", height = 5, width = 5)

norm_distribs <- tibble(d3 = rnorm(10000, -4, 1.5),
                        d1 = rnorm(10000, 7, 0.1),
                        d2 = rnorm(10000, 3, 4)) %>%
  pivot_longer(everything(), names_to = "dist", values_to = "val")

ggplot(norm_distribs, aes(x = val, fill = dist, color = dist)) +
  geom_density(bw = 1, alpha = .7, show.legend = F) +
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank())

ggsave("metrics_logo3.png", height = 4.8, width = 4.8)
