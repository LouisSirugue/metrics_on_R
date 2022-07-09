
library(tidyverse)

folder <- "homework3"

for (i in 1:4) {assign(paste0("f", i), jitter(sample(c(2 , 3), 1)))}
for (i in 1:4) {assign(paste0("d", i), runif(1, 0, .01))}
for (i in 1:4) {assign(paste0("p", i), runif(1, 0, pi))}

xt = function(t) exp(-d1*t)*sin(t*f1+p1)+exp(-d2*t)*sin(t*f2+p2)
yt = function(t) exp(-d3*t)*sin(t*f3+p3)+exp(-d4*t)*sin(t*f4+p4)
t = seq(1, 100, by = .001)
dat = data.frame(t = t, x = xt(t), y = yt(t))

ggplot(dat, aes(x = x, y = y)) +
  geom_point(size = .1, alpha = .1, color = "#327f97") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#014D64", color = "#014D64", linetype = 0),
        plot.background = element_rect(fill = "#014D64", colour = "#014D64"),
        panel.border = element_blank())

ggsave(paste0("../", folder, "/title_bg.png"), width = 16, height = 9)
