#!/usr/bin/env Rscript
# Activation Functions Visualization
# Creates a 2x3 grid plot of common activation functions

library(ggplot2)
library(grid)

# Define activation functions
relu <- function(x) pmax(0, x)
sigmoid <- function(x) 1 / (1 + exp(-x))
gelu <- function(x) x * pnorm(x)  # GELU: x * Φ(x)
silu <- function(x) x * sigmoid(x)  # SiLU / Swish: x * σ(x)

# Generate x values
x <- seq(-5, 5, length.out = 500)

# Create data frames for each function
data_linear <- data.frame(x = x, y = x, function_name = "Linear")
data_relu <- data.frame(x = x, y = relu(x), function_name = "ReLU")
data_sigmoid <- data.frame(x = x, y = sigmoid(x), function_name = "Sigmoid")
data_tanh <- data.frame(x = x, y = tanh(x), function_name = "Tanh")
data_gelu <- data.frame(x = x, y = gelu(x), function_name = "GELU")
data_silu <- data.frame(x = x, y = silu(x), function_name = "SiLU")

# Color palette - rainbow colors
colors <- c(
  "Linear" = "#E41A1C",   # Red
  "ReLU" = "#377EB8",     # Blue
  "Sigmoid" = "#4DAF4A",  # Green
  "Tanh" = "#984EA3",     # Purple
  "GELU" = "#FF7F00",     # Orange
  "SiLU" = "#F781BF"      # Pink
)

# Function to create individual plot
create_plot <- function(data, y_limits = NULL) {
  p <- ggplot(data, aes(x = x, y = y, color = function_name)) +
    geom_line(linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5) +
    scale_color_manual(values = colors) +
    labs(title = data$function_name[1],
         x = "x",
         y = "f(x)") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "none",
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95"),
      panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.5),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 9)
    )
  
  if (!is.null(y_limits)) {
    p <- p + coord_cartesian(ylim = y_limits)
  }
  
  return(p)
}

# Create individual plots
# Linear, ReLU, GELU, SiLU have wider y-range
# Sigmoid, Tanh are bounded

p1 <- create_plot(data_linear)
p2 <- create_plot(data_relu)
p3 <- create_plot(data_sigmoid, y_limits = c(-0.2, 1.2))
p4 <- create_plot(data_tanh, y_limits = c(-1.2, 1.2))
p5 <- create_plot(data_gelu)
p6 <- create_plot(data_silu)

# Function to convert ggplot to grob
ggplot_to_grob <- function(plot) {
  ggplotGrob(plot)
}

# Convert all plots to grobs
grobs <- lapply(list(p1, p2, p3, p4, p5, p6), ggplot_to_grob)

# Save as PDF
pdf("activation_functions.pdf", width = 12, height = 8)

# Create layout grid
# Set up the page
grid.newpage()

# Add title
title <- textGrob("Activation Functions", 
                  gp = gpar(fontsize = 16, fontface = "bold"))
pushViewport(viewport(height = 0.95, y = 0.975, just = "top"))
grid.draw(title)
upViewport()

# Create viewport for plots
pushViewport(viewport(
  x = 0.02, y = 0.02, 
  width = 0.96, height = 0.88,
  just = c("left", "bottom"),
  layout = grid.layout(2, 3, widths = unit(rep(1, 3), "null"), 
                       heights = unit(rep(1, 2), "null"))
))

# Place plots in grid
positions <- list(
  c(1, 1), c(1, 2), c(1, 3),  # Row 1
  c(2, 1), c(2, 2), c(2, 3)   # Row 2
)

for (i in 1:6) {
  pushViewport(viewport(layout.pos.row = positions[[i]][1], 
                        layout.pos.col = positions[[i]][2]))
  grid.draw(grobs[[i]])
  upViewport()
}

upViewport()
dev.off()

cat("Plot saved to: activation_functions.pdf\n")
