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



# =========================================================
# SHAPE COMPARISON BETWEEN FARMS
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
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(
      hjust = 0.5
    ),
    
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 1
    )
  )

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


chisq.test(shape_table)
fisher.test(shape_table)
