# ## Load packages
# library(tidyverse)
# library(plotly)
# library(viridisLite)
# library(scales)
# 
# ## ggplot theme
# plot_theme <- theme_classic() +
#   theme(axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.line.y = element_blank(),
#         legend.position = "none",
#         plot.background = element_rect(fill = "black"),
#         panel.background = element_rect(fill = "black"),
#         axis.text.x = element_text(color = "white"))
# 
# ## Build spatial plotly for trend data
# buildTrendMapOutput <- function(global_trends, land_toggle) {
#   # Build base spatial plot for trends
#   plot <- ggplot(data = global_trends, aes(x=long, y = lat, color = value)) +
#   geom_point() +
#   theme_void() +
#   theme(panel.background = element_rect(fill = "black"),
#         plot.background = element_rect(fill = "black"),
#         legend.text = element_text(color = "white"),
#         legend.title = element_text(color = "white")) +
#   labs(color = "Trend") +
#   scale_color_viridis_c()
#   
#   # Toggle country polygons
#   if (land_toggle) {
#     plot <- plot + borders("world", fill="black")
#   }
#   
#   # Output as interactive plotly
#   ggplotly(plot)
# }
# 
# ## Build boxplot for selected values
# buildTrendBoxOutput <- function(global_trends, selected_points) {
#   if (!is.null(selected_points)) {
#     # Bump selected points to match indexes
#     selected_rows <- selected_points$pointNumber + 1
#     
#     # Filter on selected points
#     selected_data <- global_trends %>%
#     filter(row_number() %in% selected_rows)
#     
#     # Rescale value for mean of selected points
#     min <- min(global_trends$value)
#     max <- max(global_trends$value)
#     mean <- mean(selected_data$value)
#     mean_scaled <- ceiling((mean-min)/(max-min) * 100)
#     
#     # Derive color from continuous palette for rescaled mean
#     col <- viridis_pal(begin = 0, end = 1)(100)[mean_scaled]
#     
#     # Build boxplot
#     ggplot(data = selected_data, aes(x = value)) +
#     geom_boxplot(color = "white", fill= col, size = 1.5) +
#     plot_theme +
#     xlim(min, max)
#     } else {
#     # Rescale value for mean of all points
#     min <- min(global_trends$value)
#     max <- max(global_trends$value)
#     mean <- mean(global_trends$value)
#     mean_scaled <- ceiling((mean-min)/(max-min)  * 100)
#     
#     # Derive color from continuous palette for rescaled mean
#     col <- viridis_pal(begin = 0, end = 1)(100)[mean_scaled]
#     
#     # Build boxplot
#     ggplot(data = global_trends, aes(x = value)) +
#     geom_boxplot(color = "white", fill= col, size = 1.5) +
#     plot_theme +
#     xlim(min, max)
#   }
# }
# 
# ## Build trend caption
# buildTrendCaption <- function(selected_points) {
#   if (!is.null(selected_points)) {
#     "Distribution of average annual changes (trend) for selected area."
#     } else {
#       "Distribution of average annual changes (trend) globally."
#     }
#   }