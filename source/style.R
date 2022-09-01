
lapply(c("tidyverse", "ggthemes", "kableExtra", "countdown"), library, character.only = TRUE)

color_names <- c("dark-blue-gray", "light-blue-gray", "solid-blue", "light-blue")
color_hex <- c("#014D64", "#00A2D9", "#6794A7", "#86D7F2")

theme_Rcourse <- function (horizontal = TRUE, bgcolor = "#DFE6EB", base_size = 10) {
  
  bgcolors <- deframe(tibble(name  = color_names, value = color_hex))
  
  ret <- theme_foundation(base_size = base_size, base_family = "sans") + 
    theme(line = element_line(colour = "#333333"), 
          rect = element_rect(fill = bgcolor, colour = NA, linetype = 1), 
          text = element_text(colour = "#333333"), 
          axis.line = element_line(size = rel(0.8)), 
          axis.line.y = element_blank(), 
          axis.text = element_text(size = rel(1)), 
          axis.text.x = element_text(size = base_size, vjust = 0, margin = margin(t = base_size, unit = "pt")), 
          axis.text.x.top = element_text(vjust = 0, margin = margin(b = base_size, unit = "pt")), 
          axis.text.y = element_text(size = base_size, hjust = 0, margin = margin(r = base_size, unit = "pt")), 
          axis.ticks = element_line(), 
          axis.ticks.y = element_blank(), 
          axis.title = element_text(size = base_size + 4, face = "bold"), 
          axis.title.x = element_text(margin = margin(t = 10)), 
          axis.title.y = element_text(angle = 90, margin = margin(r = 10)), 
          axis.ticks.length = unit(-base_size * 0.5, "points"),
          legend.background = element_rect(linetype = 0), 
          legend.spacing = unit(base_size * 1.5, "points"), 
          legend.key = element_rect(linetype = 0), 
          legend.key.size = unit(1.2, "lines"), 
          legend.key.height = NULL, 
          legend.key.width = NULL, 
          legend.text = element_text(size = base_size + 2), 
          legend.text.align = NULL, 
          legend.title = element_text(size = base_size + 4, hjust = 0), 
          legend.title.align = NULL, 
          legend.position = "top", 
          legend.direction = NULL, 
          legend.justification = "left", 
          panel.background = element_rect(linetype = 0), 
          panel.border = element_blank(), 
          panel.grid.major = element_line(colour = "#DFE6EB", size = rel(1.75)), 
          panel.grid.minor = element_blank(), 
          panel.spacing = unit(0.75, "lines"), 
          strip.background = element_rect(fill = bgcolor, colour = NA, linetype = 0), 
          strip.text = element_text(size = base_size + 4), 
          strip.text.x = element_text(), 
          strip.text.y = element_text(angle = -90), 
          plot.background = element_rect(fill = bgcolor, colour = bgcolor), 
          plot.title = element_text(size = rel(1.5), hjust = 0, face = "bold"), 
          plot.margin = unit(c(6, 5, 6, 5) * 2, "points"), 
          complete = TRUE)
  if (horizontal) {
    ret <- ret + theme(panel.grid.major.x = element_blank())
  }
  else {
    ret <- ret + theme(panel.grid.major.y = element_blank())
  }
  ret
}

theme_set(theme_Rcourse())

scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = color_hex)
}

scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = color_hex)
}

kable <- function(data, caption, ...) {
  knitr::kable(data, caption = caption, booktabs = TRUE, digits = 2, ...) %>% 
    kable_styling(bootstrap_options = c('hover', 'condensed'), full_width = F)
}

stargazer <- function(..., dep.var.labels) {
  stargazer::stargazer(..., dep.var.labels = dep.var.labels, type = "html", 
                       keep.stat = c("n", "adj.rsq"), notes.append = FALSE, 
                       notes = paste0("<i><sup>&sstarf;</sup>p<0.1;",  
                                      "<sup>&sstarf;&sstarf;</sup>p<0.05;", 
                                      "<sup>&sstarf;&sstarf;&sstarf;</sup>p<0.01</i>"))
}
