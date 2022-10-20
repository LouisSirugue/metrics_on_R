
library(tidyverse)
library(ggthemr)
library(viridis)

ggthemr("dust")

mtcars %>% 
  select(am, mpg, cyl, disp, hp) %>% 
  pivot_longer(!am, names_to = "variable", values_to = "value") %>%
  group_by(variable, am) %>%
  summarise(mean = mean(value, na.rm = T),
            sd = sd(value, na.rm = T),
            n = sum(!is.na(value)))  %>%
  mutate(se = sd / sqrt(n),
         t = qt(1 - ((1 - .97)/2), n - 1),
         lb = mean - t*se,
         ub = mean + t*se) %>%
  ggplot(aes(x = variable, y = mean, color = as.factor(am))) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(aes(xmin = variable, xmax = variable, ymin = lb, ymax = ub), position = position_dodge(.5), width = .25) + 
  scale_colour_viridis_d(option = "E", begin = .2, end = .8) + 
  coord_flip() +
  labs(color = "AM") + 
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

ggsave("C:/Users/l.sirugue/Desktop/Teaching/metrics_on_R-main/lecture7/example_plot.png", width = 5, height = 5)
