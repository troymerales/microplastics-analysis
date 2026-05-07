#-----------------------------------------------------------

#MICRO PARTICLE COUNT
kruskal.test(raw_count ~ farm,
             data = main_data)
#X2 = 2.3673. df=2, p=0.3062

#-----------------------------------------------------------

#MICRO PARTICLE COUNT AND WEIGHT
shapiro.test(main_data$wet_weight)
#W = 0.81358, p-value = 2.994e-07

shapiro.test(main_data$mp_count)
#W = 0.83937, p-value = 1.501e-06

cor.test(
  main_data$wet_weight,
  main_data$mp_count,
  method = "spearman"
)
# S = 29566, p-value = 0.1724, rho = 0.178385

main_data %>%
  group_by(farm) %>%
  summarise(
    mean_mp_count = mean(mp_count, na.rm = TRUE)
  )
# 1-0.07752, 2-0.08253, 3-0.05 

#-----------------------------------------------------------

#MICRO PARTICLE COLOR

color_table <- table(mp_data$farm_mp,
                     mp_data$mp_color)

chisq.test(color_table)
#X2 = Na, p-value =Na

fisher.test(color_table)
#p-value =6.077e-09

#-----------------------------------------------------------

#MICRO PARTICLE SHAPE

shape_table <- table(mp_data$farm_mp,
                     mp_data$mp_shape)


chisq.test(shape_table)
#X2 = Na, p-value =Na

fisher.test(shape_table)
#p-value =0.07841

#-----------------------------------------------------------

#MICRO PARTICLE LENGTH
mp_data$mp_length <- as.numeric(mp_data$mp_length)

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

length_table <- table(
  mp_data$farm_mp,
  mp_data$length_bin
)

length_data <- mp_data %>%
  drop_na(mp_length, farm_mp)

kruskal.test(mp_length ~ farm_mp,
             data = length_data)
#X2 = 1.5833, df = 2, p-value = 0.4531

