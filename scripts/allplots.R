# =========================================================
# COMMON THEME
# =========================================================

common_theme <- theme(
  
  # Legend
  legend.position = "right",
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 14),
  
  # Axis titles
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  
  # Axis tick labels
  axis.text.x = element_text(size = 14),
  axis.text.y = element_text(size = 14),
  
  # Remove gridlines
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  
  # Keep only left and bottom axis lines
  axis.line.x.bottom = element_line(
    color = "black",
    linewidth = 1
  ),
  
  axis.line.y.left = element_line(
    color = "black",
    linewidth = 1
  ),
  
  # Remove panel border
  panel.border = element_blank(),
  
  # Center titles
  plot.title = element_text(
    hjust = 0.5
  )
)


# =========================================================
# 1. BARPLOT OF PARTICLE ABUNDANCE PER FARM
# =========================================================

bar_data <- main_data %>%
  group_by(farm) %>%
  summarise(
    total_count = sum(raw_count, na.rm = TRUE)
  )

ppg <- ggplot(bar_data,
              aes(x = farm,
                  y = total_count,
                  fill = farm)) +
  
  geom_col(
    width = 0.7,
    color = "black",
    linewidth = 0.5
  ) +
  
  geom_text(aes(label = total_count),
            vjust = -0.5,
            size = 5) +
  
  scale_fill_manual(
    name = "Farm",
    values = c(
      "1" = "#ed5353",
      "2" = "#619cff",
      "3" = "#F5BB00"
    )
  ) +
  
  coord_cartesian(ylim = c(0, 40)) +
  
  labs(
    x = "Farm",
    y = "Total Count per Farm"
  ) +
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  theme_minimal() +
  common_theme

ppg


# =========================================================
# 2. BOXPLOT OF PARTICLE ABUNDANCE PER FARM
# =========================================================

mp_count_plot <- ggplot(main_data,
                        aes(x = farm,
                            y = mp_count,
                            fill = farm)) +
  
  geom_boxplot(
    color = "black",
    alpha = 0.8,
    linewidth = 0.6
  ) +
  
  scale_fill_manual(
    name = "Farm",
    values = c(
      "1" = "#ed5353",
      "2" = "#619cff",
      "3" = "#F5BB00"
    )
  ) +
  
  geom_jitter(
    width = 0.1,
    alpha = 0.6,
    color = "black"
  ) +
  
  labs(
    x = "Farm",
    y = expression("Abundance (p g"^{-1}*")")
  ) +
  
  theme_minimal() +
  common_theme

mp_count_plot


# =========================================================
# 3. STACKED BARPLOT OF PARTICLE COLOR PER FARM
# =========================================================

color_farm_plot <- ggplot(
  mp_data %>% filter(!is.na(mp_color)),
  aes(x = farm_mp,
      fill = mp_color)
) +
  
  geom_bar(
    position = "fill",
    color = "black",
    linewidth = 0.2
  ) +
  
  scale_fill_manual(
    name = "Color",
    values = c(
      "blue" = "#619cff",
      "black" = "#202020",
      "red" = "#ed5353",
      "purple" = "#b071e3"
    )
  ) +
  
  labs(
    x = "Farm",
    y = "Proportion"
  ) +
  
  scale_y_continuous(
    labels = scales::percent
  ) +
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  theme_minimal() +
  common_theme

color_farm_plot


# =========================================================
# 4. STACKED BARPLOT OF PARTICLE SHAPE PER FARM
# =========================================================

shape_farm_plot <- ggplot(
  mp_data %>% filter(!is.na(mp_shape)),
  aes(x = farm_mp,
      fill = mp_shape)
) +
  
  geom_bar(
    position = "fill",
    color = "black",
    linewidth = 0.3
  ) +
  
  scale_y_continuous(
    labels = scales::percent
  ) +
  
  scale_fill_manual(
    name = "Shape",
    values = c(
      "fiber" = "#619cff",
      "fragment" = "#ed5353"
    )
  ) +
  
  labs(
    x = "Farm",
    y = "Proportion"
  ) +
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  theme_minimal() +
  common_theme

shape_farm_plot


# =========================================================
# 5. BARPLOT OF PARTICLE LENGTH DISTRIBUTION PER FARM
# =========================================================

length_bin_plot <- ggplot(
  mp_data %>% filter(!is.na(length_bin)),
  aes(x = length_bin,
      fill = farm_mp)
) +
  
  geom_bar(
    position = position_dodge2(
      width = 0.7,
      preserve = "single"
    ),
    color = "gray",
    linewidth = 0.1
  ) +
  
  scale_fill_manual(
    name = "Farm",
    values = c(
      "1" = "#ed5353",
      "2" = "#619cff",
      "3" = "#F5BB00"
    )
  ) +
  
  scale_x_discrete(drop = FALSE) +
  
  labs(
    x = "Length Range",
    y = "Frequency"
  ) +
  
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  
  theme_minimal() +
  
  common_theme +
  
  theme(
    axis.text.x = element_text(
      size = 14,
      angle = 45,
      hjust = 1
    )
  )

length_bin_plot


# =========================================================
# 6. CORRELATION PLOT OF WET WEIGHT AND PARTICLE ABUNDANCE
# =========================================================

correlation_plot <- ggplot(main_data,
                           aes(x = wet_weight,
                               y = mp_count)) +
  
  geom_point(size = 3,
             alpha = 0.7) +
  
  geom_smooth(method = "lm",
              se = TRUE) +
  
  labs(
    x = "Wet Weight (g)",
    y = expression("Abundance (p g"^{-1}*")")
  ) +
  
  theme_minimal() +
  common_theme

correlation_plot

# =========================================================
# SAVE ALL PLOTS
# =========================================================

plot_list <- list(
  ppg = ppg,
  bpcount = mp_count_plot,
  color_comparison = color_farm_plot,
  shape_comparison = shape_farm_plot,
  length_distribution = length_bin_plot,
  correlation_plot = correlation_plot
)

for (plot_name in names(plot_list)) {
  
  ggsave(
    filename = paste0("plots/", plot_name, ".png"),
    plot = plot_list[[plot_name]],
    width = 8,
    height = 5,
    dpi = 300
  )
}
