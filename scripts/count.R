mp_count_plot <- ggplot(main_data,
                        aes(x = farm,
                            y = mp_count,
                            fill = farm)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1,
              alpha = 0.5) +
  labs(
    title = "MP Count Comparison Between Farms",
    x = "Farm",
    y = "MP Count (particles per 20g)"
  ) +
  theme_minimal()

mp_count_plot

ggsave(
  filename = "plots/bpcount.png",
  plot = mp_count_plot,
  width = 7,
  height = 5,
  dpi = 300
)


kruskal.test(mp_count ~ farm,
             data = main_data)

#X2 = 2.3673. df=2, p=0.3062

#dunnTest(mp_count ~ farm,
#         data = main_data,
#         method = "bonferroni")

bar_data <- main_data %>%
  group_by(farm) %>%
  summarise(
    total_mp = sum(mp_count, na.rm = TRUE)
  )

ppg <- ggplot(bar_data,
       aes(x = farm,
           y = total_mp,
           fill = farm)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = total_mp),
            vjust = -0.5,
            size = 5) +
  coord_cartesian(ylim = c(0, 2)) +
  labs(
    title = "Total Particle/g Per Farm",
    x = "Farm",
    y = "Total Particle/g"
  ) +
  theme_minimal()

ggsave(
  filename = "plots/ppg.png",
  plot = ppg,
  width = 7,
  height = 5,
  dpi = 300
)

wet_weight_plot <- ggplot(main_data,
                          aes(x = farm,
                              y = wet_weight,
                              fill = farm)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1,
              alpha = 0.5) +
  labs(
    title = "Wet Weight Comparison Between Farms",
    x = "Farm",
    y = "Wet Weight (g)"
  ) +
  coord_cartesian(ylim = c(50, 800)) +
  theme_minimal()

wet_weight_plot

# Save plot
ggsave(
  filename = "plots/wet_weight_comparison.png",
  plot = wet_weight_plot,
  width = 7,
  height = 5,
  dpi = 300
)

# =========================================================
# CORRELATION BETWEEN WET WEIGHT AND MP COUNT
# =========================================================

correlation_plot <- ggplot(main_data,
                           aes(x = wet_weight,
                               y = mp_count)) +
  geom_point(size = 3,
             alpha = 0.7) +
  geom_smooth(method = "lm",
              se = TRUE) +
  labs(
    title = "Correlation Between Wet Weight and MP Count",
    x = "Wet Weight (g)",
    y = "MP Count (particles per 20g)"
  ) +
  theme_minimal()

correlation_plot

# Save plot
ggsave(
  filename = "plots/weight_mpcount_correlation.png",
  plot = correlation_plot,
  width = 7,
  height = 5,
  dpi = 300
)

# =========================================================
# NORMALITY CHECK
# =========================================================

shapiro.test(main_data$wet_weight)

shapiro.test(main_data$mp_count)

# =========================================================
# SPEARMAN CORRELATION
# (recommended if non-normal)
# =========================================================

cor.test(
  main_data$wet_weight,
  main_data$mp_count,
  method = "spearman"
)

# =========================================================
# PEARSON CORRELATION
# (only if normal)
# =========================================================

cor.test(
  main_data$wet_weight,
  main_data$mp_count,
  method = "pearson"
)

