  
theme_tuleva <- function(base_size = 11,
                       base_family = "",
                       base_line_size = base_size / 170,
                       base_rect_size = base_size / 170){
  
  #Set theme_classic as base
  theme_classic(base_size = base_size, 
                base_family = base_family,
                base_line_size = base_line_size) %+replace%
    theme(
      text = element_text(family = base_family, size = base_size, color = "#002F63",
                          face = "plain", lineheight = 0.9, 
                          hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), 
                          debug = FALSE),
      panel.grid.major.y = element_line(color = 'grey80'),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.background = element_rect(fill = '#dfeaf2', color = NA),
      plot.background = element_rect(fill = '#dfeaf2', color = NA), 
      plot.title = element_text(hjust = 0.5),
      plot.subtitle  = element_text(hjust = 0.5),
      axis.line = element_line(color = "grey80"),
      axis.ticks = element_blank(),
      complete = F
    )
}
  