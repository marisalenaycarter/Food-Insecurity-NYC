# 
project_pal <- rev(c("#9585AB", "#915C83", "#864C67", "#394C73", "#073552", "#000033"))
background <- "#bbd9eb"

library(ggplot2)
project_theme <- 
  ggthemes::theme_tufte(ticks = FALSE) +
  # get open sans: https://cran.r-project.org/web/packages/gfonts/vignettes/gfonts.html
  ggplot2::theme(text = element_text(family = "sans"),
                 plot.title = element_text(hjust = 0.5),
                 plot.subtitle = element_text(hjust = 0.5),
                 plot.background = element_rect(fill = background, color = background))

                 #"#bbd9eb")

theme_set(project_theme)
