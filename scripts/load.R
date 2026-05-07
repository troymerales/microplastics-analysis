packages <- c(
  "tidyverse",
  "readxl",
  "ggpubr",
  "rstatix",
  "FSA",
  "patchwork"
)

installed_packages <- rownames(installed.packages())

for (pkg in packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
}

# Load packages
lapply(packages, library, character.only = TRUE)

# =========================
# IMPORT DATA
# =========================

# Main spreadsheet
main_raw <- read_excel("data/data.xlsx")

# MP characteristics spreadsheet
mp_raw <- read_excel("data/data_mp.xlsx")

# =========================
# CLEAN DATA
# =========================

farm <- as.factor(main_raw$FARM)

sample_id <- as.factor(main_raw$SAMPLE)

wet_weight <- main_raw$"Wet Weight (g)"

status <- as.factor(main_raw$STATUS)

mp_count <- main_raw$`COUNT (particles per 20g)` / 20

control_group <- main_raw$"CONTROL GROUPS"

# ---------- MP CHARACTERISTICS DATA ----------

farm_mp <- as.factor(mp_raw$"FARM #")

sample_mp <- as.factor(mp_raw$"SAMPLE #")

mp_color <- as.factor(mp_raw$COLOR)

mp_length <- mp_raw$"LENGTH (mm)"

mp_shape <- as.factor(mp_raw$SHAPE)

main_data <- data.frame(
  farm,
  sample_id,
  wet_weight,
  status,
  mp_count,
  control_group
)

mp_data <- data.frame(
  farm_mp,
  sample_mp,
  mp_color,
  mp_length,
  mp_shape
)

mp_data$mp_length <- as.numeric(mp_data$mp_length)
mp_data$mp_color <- droplevels(mp_data$mp_color)
mp_data$mp_shape <- droplevels(mp_data$mp_shape)
mp_data[mp_data == "N/A"] <- NA
main_data[main_data == "N/A"] <- NA
