color_summary <- mp_data %>%
  count(mp_color)

color_summary <- color_summary %>%
  arrange(desc(mp_color)) %>%
  mutate(
    percent = round(n / sum(n) * 100, 1),
    label = paste0(n, " (", percent, "%)"),
    ypos = cumsum(n) - 0.5 * n
  )

color_pie <- ggplot(color_summary,
                    aes(x = "",
                        y = n,
                        fill = mp_color)) +
  
  geom_col(width = 1,
           color = "white",
           linewidth = 0.3) +
  
  coord_polar(theta = "y") +
  
  scale_fill_manual(
    name = "Color",
    values = c(
      "black" = "#202020",
      "blue" = "#619cff",
      "purple" = "#b071e3",
      "red" = "#ed5353"
    ),
    na.value = "lightgray"
  ) +
  
  theme_void() +
  
  labs(
    title = "Overall Particle Color Summary"
  )

color_pie
# Save plot
ggsave(
  filename = "plots/color_pie.png",
  plot = color_pie,
  width = 7,
  height = 7,
  dpi = 900
)

#------------------------------------------------------------

color_farm_plot <- ggplot(mp_data,
                          aes(x = farm_mp,
                              fill = mp_color)) +
  geom_bar(position = "fill",
           color = "white",
           linewidth = 0.1) +
  scale_fill_manual(name = "Color",
    values = c(
    "blue" = "#619cff",
    "black" = "#202020",
    "red" = "#ed5353",
    "purple" = "#b071e3"
  ),
  na.value="lightgray"
  ) +
  labs(
    title = "Particle Color Comparison Between Farms",
    x = "Farm",
    y = "Proportion"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

color_farm_plot

ggsave(
  filename = "plots/color_comparison.png",
  plot = color_farm_plot,
  width = 8,
  height = 5,
  dpi = 300
)

#------------------------------------------------------------

color_table <- table(mp_data$farm_mp,
                     mp_data$mp_color)

color_table

chisq.test(color_table)
fisher.test(color_table)
