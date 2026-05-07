# =========================================================
# BIN MP LENGTHS
# =========================================================

# Convert to numeric first if needed
mp_data$mp_length <- as.numeric(mp_data$mp_length)

# Create bins
mp_data$length_bin <- cut(
  mp_data$mp_length,
  breaks = c(
    0,
    0.5,
    1.0,
    1.5,
    2.0,
    2.5,
    3.0,
    Inf
  ),
  labels = c(
    "0-0.5",
    "0.5-1.0",
    "1.0-1.5",
    "1.5-2.0",
    "2.0-2.5",
    "2.5-3.0",
    ">3.0"
  ),
  include.lowest = TRUE
)

# =========================================================
# LENGTH BIN COMPARISON BETWEEN FARMS
# =========================================================

length_bin_plot <- ggplot(mp_data,
                          aes(x = length_bin,
                              fill = farm_mp)) +
  
  geom_bar(
    position = position_dodge2(
      width = 0.7,
      preserve = "single"
    ),
    width = 0.5,
    color = "white"
  ) +
  
  scale_x_discrete(drop = FALSE) +
  
  labs(
    title = "MP Length Comparison Between Farms",
    x = "Length Range",
    y = "Frequency",
    fill = "Farm"
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

length_bin_plot

# Save plot
ggsave(
  filename = "plots/mp_length_bins.png",
  plot = length_bin_plot,
  width = 10,
  height = 6,
  dpi = 300
)

length_table <- table(
  mp_data$farm_mp,
  mp_data$length_bin
)

length_table

mp_data$mp_length

length_data <- mp_data %>%
  drop_na(mp_length, farm_mp)

kruskal.test(mp_length ~ farm_mp,
             data = length_data)
dunnTest(mp_length ~ farm_mp,
         data = length_data,
         method = "bonferroni")
