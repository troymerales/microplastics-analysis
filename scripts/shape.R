shape_summary <- mp_data %>%
  count(mp_shape)

shape_summary

shape_summary <- mp_data %>%
  count(mp_shape)

shape_pie <- ggplot(shape_summary,
                    aes(x = "",
                        y = n,
                        fill = mp_shape)) +
  geom_col(width = 1,
           color = "white",
           linewidth = 0.3) +
  
  coord_polar("y") +
  scale_fill_manual(
    name = "Shape",
    values = c(
      "fragment" = "#619cff",
      "fiber" = "#ed5353"
      ),
    na.value="lightgrey") +
  labs(
    title = "Overall Particle Shape Summary"
  ) +
  theme_void()

shape_pie

# Save plot
ggsave(
  filename = "plots/shape_pie.png",
  plot = shape_pie,
  width = 7,
  height = 7,
  dpi = 900
)

# =========================================================
# SHAPE COMPARISON BETWEEN FARMS
# =========================================================

shape_farm_plot <- ggplot(mp_data,
                          aes(x = farm_mp,
                              fill = mp_shape)) +
  geom_bar(position = "fill",
           color = "white",
           linewidth = 0.3
           ) +
  labs(
    title = "MP Shape Comparison Between Farms",
    x = "Farm",
    y = "Proportion"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    name = "Shape",
    values = c(
    "fragment" = "#619cff",
    "fiber" = "#f8766d"
  ),
  na.value="lightgrey") +
  theme_minimal()

shape_farm_plot

ggsave(
  filename = "plots/shape_comparison.png",
  plot = shape_farm_plot,
  width = 8,
  height = 5,
  dpi = 300
)


shape_table <- table(mp_data$farm_mp,
                     mp_data$mp_shape)

shape_table

chisq.test(shape_table)
fisher.test(shape_table)
