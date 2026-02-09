
vary_viridis <- function(alpha = 1, begin = 0, end = 1, 
                         direction = 1, option = "D", mix = "white", 
                         amount = 0){
  
  scales::pal_viridis(alpha = alpha, begin = begin, end = end, 
              direction = direction, option = option)(6) |> 
    scales::col_mix(b = mix, amount = amount) |>
    scales::pal_gradient_n(values = NULL, space = "Lab")
  
}

magma_mod <- vary_viridis(begin = .1, end = .9, option = "magma", alpha = .8)

# library(scales)
# magma_mod <- pal_gradient_n(pal_viridis(alpha = .9,
#         begin = 0, end = .95, direction = 1, option = "D")(6) |> scales::col_mix("lightyellow", .4), values = NULL, space = "Lab")



#' @export
theme_map <- function(...) {
  theme_minimal() +
  theme(
    palette.fill.continuous = magma_mod,
    palette.fill.discrete = magma_mod,
    palette.fill.binned = magma_mod,
    palette.color.continuous = magma_mod,
    palette.color.discrete = magma_mod,
    palette.color.binned = magma_mod,
    text = element_text(color = "#4e4d47"),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(color = "#dbdbd9", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#f5f5f2",
                                   color = NA),
    panel.background = element_rect(fill = "#f5f5f2",
                                    color = NA),
    legend.background = element_rect(fill = "#f5f5f2",
                                     color = NA),
    plot.margin = unit(c(.5, .5, .2, .5), "cm"),
    panel.border = element_blank(),
    panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 9, hjust = 0,
                               color = "#4e4d47"),
    plot.title = element_text(size = 15, hjust = 0.5,
                              color = "#4e4d47"),
    plot.subtitle = element_text(size = 10, hjust = 0.5,
                                 color = "#4e4d47",
                                 margin = margin(b = -0.1,
                                                 t = -0.1,
                                                 l = 2,
                                                 unit = "cm"),
                                 debug = F),
    plot.caption = element_text(size = 7,
                                hjust = .5,
                                margin = margin(t = 0.2,
                                                b = 0,
                                                unit = "cm"),
                                color = "#939184"),
    ...
  )
}
