# Plot geochemical relationships for identifying CO2 leakages (deep sources)
# Natural emissions oh H2 (associated to CO2?)

# Libraries ----
library(tidyverse)
library(patchwork)


# Data ----
data_nat_path <- "data/natural_dry_h2_seepages.xlsx"

h2_breaks <- c(0, 5, 20, 100, 10000)

data_nat <- readxl::read_xlsx(data_nat_path) |> 
  janitor::clean_names() |> 
  # remove 2 rows without O2 data
  drop_na(o2_ar_percent) |> 
  # Add breaks
  mutate(h2_breaks = cut(h2_ppm,
                         h2_breaks,
                         include.lowest = T,
                         dig.lab = 10))

data_nat 

# Plots ----

# General parameters 
alpha <- 0.5
shape <- 19
size <- 2
option <-  "viridis"

# Set default theme
my_theme <- theme_bw(base_size = 12) +
  theme(legend.text  = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        axis.title   = element_text(size = 12)) 
theme_set(my_theme)



# CO2 vs. O2
interpretation <- tribble(~x, ~y, ~label,
                          5,  2, "Diss.",  # Dissolution
                          14,  2, "Mix.",  # Mixture
                          25,  2, "Exog.") # Exogenous addition 

p1 <- ggplot() +
  geom_point(data = data_nat ,
             aes(x = co2_percent,, 
                 y = o2_ar_percent,
                 colour = h2_breaks),
             size = size,
             shape = shape,
             alpha = alpha
  ) + 
  # Biological respiration line
  geom_abline(intercept = 21, slope = -1, color = "red") + 
  # Oxidation of CH4 line
  geom_abline(intercept = 21, slope = -2, color = "blue") +
  # Format axes
  coord_cartesian(ylim = c(0,25),
                  xlim = c(0,100),
                  expand = FALSE) +
  labs(x = expression("CO"[2] * " [% volumen]"),
       y = expression("O"[2] * " [% volumen]")) +
  # Description areas
  geom_text(data = interpretation,
            aes(x, y, label = label)) +
  # Atmospheric concentration
  annotate("text",
           x = 4,
           y = 23,
           label = "Atmospheric concentration",
           col = "black",
           hjust = 0) + 
  annotate(
    'curve',
    x = 4, 
    y = 23,
    xend = 0,
    yend = 21,
    curvature = 0.3,
    arrow = arrow(length = unit(0.2, 'cm'))
  ) +
  scale_colour_viridis_d(name = expression("H"[2] * " [ppm]"),
                         option = option,
                         direction = -1) 



# CO2 vs. N2
p2 <- ggplot() +
  geom_point(data = data_nat, 
             aes(x = n2_percent,
                 y = co2_percent,
                 colour = h2_breaks),
             size = size,
             shape = shape,
             alpha = alpha
  ) +
  # Format x and y axes
  labs(x = expression("N"[2] * " [% volumen]"),
       y = expression("CO"[2] * " [% volumen]")) +
  coord_cartesian(ylim = c(0,100),
                  xlim = c(0,100),
                  expand = FALSE) + 
  # coord_equal(ylim = c(0,100),
  #             xlim = c(0,100),
  #             expand = FALSE) +
  # n2 depleted leakage signal
  annotate("text",
           x = 39,
           y = 70,
           vjust = 0,
           label = "N2 depleted\nleakage signal\n ",
           size = 4) +
  # n2 enriched natural signal
  annotate("text",
           x = 90,
           y = 70,
           vjust = 0,
           label = "N2 enriched\nnatural\nsignal",
           size = 4) +
  # Respiration / mixing with air
  annotate("text",
           x = 76,
           y = 50,
           angle = 90,
           vjust = 0,
           label = "Respiration and/or mixing with air",
           size = 3, 
           col = "red") +
  geom_vline(xintercept = 78, color="red") +
  scale_colour_viridis_d(name = expression("H"[2] * " [ppm]"),
                         option = option,
                         direction = -1) 

# ratio
p3 <- ggplot() +
  geom_point(data =  data_nat, 
             aes(x = n2_percent / o2_ar_percent, 
                 y = co2_percent,
                 colour = h2_breaks
             ),
             size = size,
             shape = shape,
             alpha = alpha
  )  + 
  # Format x and y axes
  labs(x = expression("N"[2] * "/" * "O"[2] ),
       y = expression("CO"[2] * " [% volumen]")) +
  coord_cartesian(ylim = c(0,50),
                  xlim = c(0,30),
                  expand = FALSE) + 
  # Biological respiration line
  geom_line(data = tibble(co2 = seq(0, 20, 0.1),
                          ratio = 78 / (21 - 1 * co2)),
            aes(x = ratio,
                y = co2), 
            col = "red") +
  # Methane oxidation line
  geom_line(data = tibble(co2 = seq(0, 10, 0.1),
                          ratio = 78 / (21 - 2 * co2)),
            aes(x = ratio, 
                y = co2), 
            col = "blue") + 
  annotate("text",
           x = Inf,
           y = 20,
           hjust = 1,
           label = "Biological respiration   ",
           col = "red") +
  annotate("text",
           x = Inf,
           y = 10,
           hjust = 1,
           label = "Methane oxidation   ",
           col = "blue") +
  # Interpretation
  annotate("text",
           x = 20.5,
           y = 50 - 10,
           hjust = 0,
           label = "O2 consumption",
           size = 3) +
  annotate("curve",
           x = 15, 
           y = 50 - 10,
           xend = 20,
           yend = 50 - 10,
           curvature = 0,
           arrow = arrow(length = unit(0.2, 'cm'))
  ) +
  annotate("text",
           x = 15,
           y = 50 - 4,
           label = "CO2 leakage",
           size = 3) +
  annotate("curve",
           x = 15, 
           y = 50 - 10,
           xend = 15,
           yend = 50 - 5 ,
           curvature = 0,
           arrow = arrow(length = unit(0.2, 'cm'))
  ) +
  annotate("text",
           x = 15,
           y = 50 - 16,
           label = "CO2 dissolution",
           size = 3) +
  annotate("curve",
           x = 15, 
           y = 50 - 10,
           xend = 15,
           yend = 50 - 15,
           curvature = 0,
           arrow = arrow(length = unit(0.2, 'cm'))
  ) +
  scale_colour_viridis_d(name = expression("H"[2] * " [ppm]"),
                         option = option,
                         direction = -1) 

# Plot together
p1 / (p2 | p3) + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A", tag_suffix = ")")
ggsave(filename = "results/figures/nat_h2_geoche_relations.png", 
       dpi = 300,
       height = 8, 
       width = 11,
       units = "in")

# Zoom
interpretation <- tribble(~x, ~y, ~label,
                          2, 15, "Diss.", # Dissolution
                          4, 15, "Mix.",  # Mixture
                          4, 21, "Exog.") # Exogenous addition 
p1_zoom <- ggplot() +
  geom_point(data = data_nat ,
             aes(x = co2_percent,, 
                 y = o2_ar_percent,
                 colour = h2_breaks),
             size = size,
             shape = shape,
             alpha = alpha
  ) + 
  # Biological respiration line
  geom_abline(intercept = 21, slope = -1, color = "red") +
  # Oxidation of CH4 line
  geom_abline(intercept = 21, slope = -2, color = "blue") +
  # Description areas
  geom_text(data = interpretation,
            aes(x, y, label = label)) +
  # Format axes
  coord_cartesian(ylim = c(14,22),
                  xlim = c(0,5),
                  expand = FALSE) +
  labs(x = expression("CO"[2] * " [% volumen]"),
       y = expression("O"[2] * " [% volumen]")) +
  scale_colour_viridis_d(name = expression("H"[2] * " [ppm]"),
                         option = option,
                         direction = -1)  

(p1 | p1_zoom) / (p2 | p3) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A", tag_suffix = ")")
ggsave(filename = "results/figures/nat_h2_geoche_relations_zoom.png", 
       dpi = 300,
       height = 8, 
       width = 12,
       units = "in")



