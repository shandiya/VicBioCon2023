
library(gt)
library(dplyr)

# theme for gt tables -----
gt_mytheme <- function(data, ...){
  data |> 
    opt_table_font(
      font = list(
        google_font("Lato"),
        default_fonts())) |> 
    opt_row_striping() |> 
    tab_options(
      table.font.size = 24,
      table.font.weight = "lighter",
      column_labels.font.size = 28,
      column_labels.font.weight = "lighter",
      row.striping.background_color = "#fffaf3",
      table_body.hlines.color = "transparent",
      table.font.color = "#413f42",
      data_row.padding = 3,
      column_labels.background.color = "#666",
      ...
    ) 
}


# plot heatmap ------
plot_heatmap <- function(data, region, fill_var, legend_title, pal, n_breaks) {
  
  ggplot(data = data,
         aes(x = period,
             y = reorder({{region}}, desc({{region}})),
             fill = {{fill_var}})) +
    geom_tile() +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_fill_distiller(name = legend_title,
                         type = "seq",
                         palette = pal,
                         direction = 1,
                         trans = "log",
                         breaks = scales::breaks_log(n = n_breaks),
                         guide = guide_colorbar(direction = "horizontal",
                                                label.position = "bottom",
                                                draw.ulim = FALSE, 
                                                draw.llim = FALSE,
                                                title.position = "top",
                                                ticks = FALSE,
                                                barwidth = 16)) +
    
    theme_classic() +
    theme(text = element_text(family = "lato"),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", size = 12, colour = "#444444"),
          legend.title.align = 0.5,
          plot.background = element_rect(fill = "#fffaf3", colour = NA),
          panel.background = element_rect(fill = "#fffaf3", colour = NA),
          legend.background = element_rect(fill = "#fffaf3", colour = NA),
          legend.box.background = element_rect(fill = "#fffaf3", colour = NA))
  
}

