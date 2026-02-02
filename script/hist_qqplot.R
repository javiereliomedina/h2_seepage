# Libraries
library(tidyverse)
library(patchwork)
library(ggpmisc )

# Set default theme in ggplot2
my_theme <- theme_bw(base_size = 12) +
  theme(legend.text  = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        axis.title   = element_text(size = 12), 
        plot.margin = margin(t = 0.5, 
                             r = 0.5, 
                             b = 0.5, 
                             l = 0.5, 
                             unit = "cm") # Increase right margin to 3cm
  )
theme_set(my_theme)

# Probabilities for Q-Q plot  
log10_breaks <- c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000)

minor_breaks <- c(seq(0.001,  0.01, by = 0.001),
                  seq( 0.01,   0.1, by = 0.01),
                  seq(  0.1,     1, by = 0.1),
                  seq(    1,    10, by = 1),
                  seq(   10,   100, by = 10),
                  seq(  100,  1000, by = 100),
                  seq( 1000, 10000, by = 1000))

# Aux. functions for plotting hist and qq-plot
my_geom_histogram <- function(data, y, ...){ 
  ggplot(data, aes( {{ y }} )) + 
    geom_histogram(binwidth = 0.25, fill = "#0072B2", colour = "grey") +
    scale_x_log10(breaks = log10_breaks,
                  minor_breaks = minor_breaks,
                  labels = scales::label_number(drop0trailing = TRUE)) +
    labs(title = "Histogram",
         x = expression("H"[2] * " [ppm]")) +
    coord_cartesian(xlim = c(0.0009, 10000),
                    ylim = c(0, 150))
}

my_geom_qq <- function(data, y) {
  ggplot(data, aes( sample = {{y}} )) +
    geom_qq(colour = "#D55E00") + 
    geom_qq_line(colour = "darkgrey") +
    labs(title = "Normal probability plot") +
    guides(colour = "none") +
    scale_y_continuous(name = expression("H"[2] * " [ppm]"),
                       breaks = log10(log10_breaks),
                       minor_breaks = log10(minor_breaks),
                       labels = log10_breaks) +
    scale_x_continuous(name = "Cumulative probability [%]",
                       breaks = seq(-5, 5, by = 1),
                       labels = seq(0, 100, by = 10),
                       expand = FALSE) +
    coord_cartesian(xlim = c(-5, 5),
                    ylim = c(-2.1, 4))
}


# Data -----
data_nat_path <- "data/natural_dry_h2_seepages.xlsx"

h2_breaks <- c(0, 5, 20, 100, 10000)

data_nat <- readxl::read_xlsx(data_nat_path) |> 
  janitor::clean_names() |> 
  # remove 2 rows without O2 data
  drop_na(o2_ar_percent) |> 
  # Add breaks and add 0.1 to H2 = 0 ppm (avoid -Inf in log tranaformation)
  mutate(h2_ppm_0.1 = h2_ppm + 0.01,
         h2_breaks = cut(h2_ppm,
                         h2_breaks,
                         include.lowest = T,
                         dig.lab = 10))

# Hist and q-q plot
p_hist <- my_geom_histogram(data_nat, h2_ppm_0.1)
p_qq <- my_geom_qq(data_nat, log10(h2_ppm_0.1))

# Imputation ----
# Many values reported as 0 --> input based on the distribution (assuming a LoD) 
# Based on: https://stackoverflow.com/questions/76346589/in-r-how-to-impute-left-censored-missing-data-to-be-within-a-desired-range-e-g/76351041?noredirect=1#comment134713497_76351041
# Assuming a log-normal distribution, assign random values in the tail of the population 

LoD = 1 # ppm

impute_data <- function(data) {
  
  # For reproducibility
  set.seed(42)
  
  # Apply a lower limit of quantification to observations
  lloq <- LoD
  data_imp <- data |> 
    mutate(h2_ppm_imp = h2_ppm) |> 
    mutate(h2_ppm_imp = ifelse(h2_ppm_imp <= lloq, NA, h2_ppm_imp ))
  
  # Any missing values in x are assumed to be known to be < LLOQ
  left_censored_log_normal_log_likelihood <- function(mu, sigma, x, lloq) {
    sum(dlnorm(na.omit(x), mu, sigma, log = TRUE)) +
      sum(is.na(x)) * plnorm(lloq, mu, sigma, log = TRUE)
  }
  
  # Then we maximize the log-likelihood, given the observed data:
  mean_sd <- function(x, ...) {  c(mean(x, ...), sd(x, ...))  }
  
  # Initial values from observed data 
  theta0 <- mean_sd(log(data_imp$h2_ppm_imp), na.rm = TRUE)
  
  fit <- optim(theta0, function(theta) {
    -left_censored_log_normal_log_likelihood(theta[1],
                                             theta[2],
                                             data_imp$h2_ppm_imp,
                                             lloq)
  })
  
  # Sample from the < LLOQ region of the fitted distribution and create a completed dataset:
  n <- sum(is.na(data_imp$h2_ppm_imp))
  p <- runif(n, 0, plnorm(lloq, fit$par[1], fit$par[2]))
  y <- qlnorm(p, fit$par[1], fit$par[2])
  
  # Create final table
  data_imp <- data_imp |> 
    mutate(h2_ppm_imp = ifelse(is.na(h2_ppm_imp),
                               y,
                               h2_ppm_imp))
  
  return(data_imp)
  
} 

# Impute data
# Based on the qq plot, it seems that there is two populations (input with values <= 10)
limit_pop <- 10 
dat_imp <- data_nat |> 
  filter(h2_ppm <= limit_pop) |> 
  impute_data() |> 
  mutate(imputation = TRUE)

data_nat <- data_nat |> 
  filter(h2_ppm > limit_pop) |> 
  bind_rows(dat_imp) |> 
  mutate(h2_ppm_imp = if_else(is.na(h2_ppm_imp), h2_ppm, h2_ppm_imp), 
         imputation = if_else(is.na(imputation), FALSE, imputation))

# Imputed data
p_hist_imp <- my_geom_histogram(data_nat, h2_ppm_imp)
p_qq_imp <- my_geom_qq(data_nat, log10(h2_ppm_imp))


# Mix populations ----
# Two populations with different sigma
pop_mix <- mixtools::normalmixEM(log(data_nat$h2_ppm_imp),
                                 mu = c(0, 1), 
                                 sigma = c(1, 1))
summary(pop_mix)

# Back transformation
# Back transformation
pop_bt <- tibble(
  Pop = c("Ba", "An"),
  Lambda = round(pop_mix$lambda, 2),
  Mean =  round(exp(pop_mix$mu + (pop_mix$sigma^2/2)), 2),
  SD = round(sqrt(exp(2*pop_mix$mu + pop_mix$sigma^2) * (exp(pop_mix$sigma^2) - 1)), 3)
)

## Assign data to background/anomaly population 
data_nat <- data_nat |> 
  mutate(population = if_else(pop_mix$posterior[,1] > 0.5, "Background", "Anomaly"),
         pupulation = as.factor(population))

# Histogram
p_hist_2pop <- ggplot(data_nat, aes(x = h2_ppm_imp, fill = population)) +
  geom_histogram(binwidth = 0.25, color = "#e9ecef", alpha = 0.6, position = "identity") +
  scale_fill_manual(name = "Population",
                    values = c("#404080", "#69b3a2")) +
  scale_x_log10(breaks = log10_breaks,
                minor_breaks = minor_breaks,
                labels = scales::label_number(drop0trailing = TRUE)) +
  labs(title = "Histogram",
       x = expression("H"[2] * " [ppm]")) +
  coord_cartesian(xlim = c(0.0009, 10000),
                  ylim = c(0, 150)) +
  theme(legend.position = "bottom") + 
  annotate(geom = "table", 
           x = 0.0009, 
           y = 150, 
           label = list(pop_bt), 
           vjust = 1,
           hjust = 0,
           table.theme = ttheme_gtbw(base_size = 9))

# Q-Q plot
p_qq_2pop <- ggplot(data_nat) +
  geom_qq(aes(sample = log10(h2_ppm_imp)), colour = "#D55E00", alpha = 0.6) +
  geom_qq(aes(sample = log10(h2_ppm_imp), colour = population), alpha = 0.6) +
  geom_qq_line(aes(sample = log10(h2_ppm_imp), colour = population), alpha = 0.6) +        
  labs(title = "Normal probability plot") +
  scale_colour_manual(values = c("#404080", "#69b3a2")) +
  guides(colour = "none") +
  scale_y_continuous(name = expression("H"[2] * " [ppm]"),
                     breaks = log10(log10_breaks),
                     minor_breaks = log10(minor_breaks),
                     labels = log10_breaks) +
  scale_x_continuous(name = "Cumulative probability [%]",
                     breaks = seq(-5, 5, by = 1),
                     labels = seq(0, 100, by = 10),
                     expand = FALSE) +
  coord_cartesian(xlim = c(-5, 5),
                  ylim = c(-2.1, 4))

# Final plot with all together
p <- (p_hist + p_qq) / (p_hist_2pop + p_qq_2pop) +
  plot_annotation(title = paste0("Hydrogen concentration in soil gas ",
                                 "(N = ", length(data_nat$h2_ppm), ")"),
                  subtitle = "A) Original data, B) Imputed data (< 1 ppm) and partitioning into two log-normal populations",
                  tag_levels = "A") 
p[[1]] <- p[[1]] + plot_layout(tag_level = "new")
p[[2]] <- p[[2]] + plot_layout(tag_level = "new")

p + plot_annotation(tag_levels = c("A", "1"), tag_suffix = ")") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(filename = "results/figures/nat_h2_data_imp.png", 
       dpi = 300,
       height = 9, 
       width = 12,
       units = "in")
1