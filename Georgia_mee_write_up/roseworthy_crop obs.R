library(tidyverse)

# ---- User settings (same values as the climate script) ----
season_year  <- 2026
sow_date     <- as.Date("2026-05-21")
sample_dates <- as.Date(c("2026-08-13", "2026-09-24", "2026-10-22"))
plot_dir     <- "C:/Users/ouz001/working_from_home_post_Sep2022/Georgia_M/Georgia_mee_write_up/plot/climate"

# Helper used in the figure subtitles, e.g. "21 May"
dm <- function(d) paste(as.integer(format(d, "%d")), format(d, "%b"))



# ---- Chunk 5: NDVI through the season (lentil plots, crop monitoring data sheet) ----
library(readxl)

xlsx_file <- "C:/Users/ouz001/working_from_home_post_Sep2022/Georgia_M/Georgia_mee_write_up/Crop monitoring plan and data sheet post week10 data 20261030 version.xlsx"
crop_dir  <- file.path(dirname(plot_dir), "crop")       # .../plot/crop
dir.create(crop_dir, recursive = TRUE, showWarnings = FALSE)

# One sheet per sampling date (two sheet names have trailing spaces, so match on trimmed names)
sheet_dates <- c("13 Aug" = "2026-08-13", "24 Sep" = "2026-09-24", "22 Oct" = "2026-10-22")
sheet_names <- excel_sheets(xlsx_file)

read_ndvi <- function(sheet_label) {
  sh <- sheet_names[trimws(sheet_names) == sheet_label]
  read_excel(xlsx_file, sheet = sh, range = "B4:I8", col_names = FALSE, col_types = "text") %>%
    set_names(c("seed_rate", paste0("group_", 1:7))) %>%
    pivot_longer(-seed_rate, names_to = "group", values_to = "ndvi") %>%
    mutate(seed_rate = as.integer(seed_rate),
           ndvi      = as.numeric(ndvi),
           date      = as.Date(sheet_dates[[sheet_label]]))
}

ndvi_all <- map_dfr(names(sheet_dates), read_ndvi) %>%
  filter(!is.na(ndvi))

# Check: expect 30 plots per date (6 groups x 5 seeding rates; group 4 has no data)
ndvi_all %>% count(date)

# Means by seeding rate, and one overall mean per date
ndvi_by_rate <- ndvi_all %>%
  group_by(date, seed_rate) %>%
  summarise(mean_ndvi = round(mean(ndvi), 2), n = n(), .groups = "drop")

ndvi_mean_all <- ndvi_all %>%
  group_by(date) %>%
  summarise(mean_ndvi = mean(ndvi), se = sd(ndvi) / sqrt(n()), n = n(), .groups = "drop")

ndvi_tbl <- ndvi_by_rate %>%
  mutate(sampling = dm(date)) %>%
  select(seed_rate, sampling, mean_ndvi) %>%
  pivot_wider(names_from = sampling, values_from = mean_ndvi) %>%
  arrange(seed_rate)
ndvi_tbl

ndvi_caption <- "Source: class crop monitoring data, East 2 paddock, Roseworthy"

# Figure A: one average across all plots, with every plot shown as a grey point
plot_ndvi_mean <- ggplot() +
  geom_jitter(data = ndvi_all, aes(date, ndvi), colour = "grey65",
              width = 1.5, height = 0, size = 1.6) +
  geom_line(data = ndvi_mean_all, aes(date, mean_ndvi), colour = "black", linewidth = 1.2) +
  geom_errorbar(data = ndvi_mean_all,
                aes(date, ymin = mean_ndvi - se, ymax = mean_ndvi + se),
                width = 2, colour = "black") +
  geom_point(data = ndvi_mean_all, aes(date, mean_ndvi), colour = "black", size = 3) +
  scale_x_date(breaks = sample_dates, date_labels = "%d %b",
               expand = expansion(mult = 0.08)) +
  scale_y_continuous(limits = c(0.3, 0.9)) +
  theme_classic() +
  labs(title = paste0("NDVI through the season, lentils at Roseworthy ", season_year),
       subtitle = paste0("Thunder XT lentils sown ", dm(sow_date),
                         ". Black: mean of all plots (\u00b1SE). Grey: individual plots"),
       x = NULL, y = "NDVI", caption = ndvi_caption)
plot_ndvi_mean

# Figure B: one line per seeding rate (mean of the groups at that rate)
rate_levels <- sort(unique(ndvi_all$seed_rate))
rate_cols   <- setNames(grey.colors(length(rate_levels), start = 0.75, end = 0.1),
                        as.character(rate_levels))

plot_ndvi_rate <- ndvi_all %>%
  mutate(seed_rate = factor(seed_rate, levels = rate_levels),
         sampling  = factor(dm(date), levels = dm(sample_dates))) %>%
  ggplot(aes(seed_rate, ndvi)) +
  geom_jitter(width = 0.12, height = 0, colour = "grey60", size = 1.8) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.25, colour = "black") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, colour = "black") +
  facet_wrap(~ sampling, nrow = 1) +
  scale_y_continuous(limits = c(0.3, 0.9)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold")) +
  labs(title = paste0("NDVI by seeding rate, lentils at Roseworthy ", season_year),
       subtitle = "Black diamonds: mean of the groups at each seeding rate (error bars = standard error). Grey: individual plots",
       x = "Seeds/m\u00b2", y = "NDVI", caption = ndvi_caption)
plot_ndvi_rate

# Save
ggsave(file.path(crop_dir, "lentil_ndvi_mean.png"),
       plot_ndvi_mean, width = 20, height = 12, units = "cm", dpi = 300)
ggsave(file.path(crop_dir, "lentil_ndvi_by_seeding_rate.png"),
       plot_ndvi_rate, width = 20, height = 12, units = "cm", dpi = 300)
write_csv(ndvi_tbl, file.path(crop_dir, "lentil_ndvi_by_seeding_rate.csv"))

# ---- Chunk 6: plants/m2, fresh biomass and dry matter by seeding rate ----
# First row of each 5-row block (30, 60, 120, 180, 240 seeds/m2) on each sheet
block_rows <- tribble(
  ~measure,       ~sheet,   ~start,
  "plants",       "13 Aug", 19,
  "plants",       "24 Sep", 19,
  "plants",       "22 Oct", 19,
  "fresh_weight", "13 Aug", 44,
  "fresh_weight", "24 Sep", 44,
  "fresh_weight", "22 Oct", 39,
  "dry_matter",   "13 Aug", 54,    # weighed on 24 Sep (groups 2, 5, 7 only)
  "dry_matter",   "24 Sep", 54)    # weighed on 22 Oct (groups 1, 2, 7 only)

read_block <- function(measure, sheet, start) {
  sh <- sheet_names[trimws(sheet_names) == sheet]
  read_excel(xlsx_file, sheet = sh, range = paste0("B", start, ":I", start + 4),
             col_names = FALSE, col_types = "text") %>%
    set_names(c("seed_rate", paste0("group_", 1:7))) %>%
    pivot_longer(-seed_rate, names_to = "group", values_to = "value") %>%
    mutate(measure   = measure,
           seed_rate = as.integer(seed_rate),
           value     = parse_number(value),
           date      = as.Date(sheet_dates[[sheet]]))
}

meas_all <- pmap_dfr(block_rows, read_block) %>%
  filter(!is.na(value)) %>%
  # 13 Aug fresh weight, group 1 at 240 seeds/m2 is 9336 g/m2 (others are 1000 to 1900):
  # treated as an entry error and left out
  filter(!(measure == "fresh_weight" & date == as.Date("2026-08-13") & value > 5000))

# Check: expect 30 per date for plants and fresh weight (29 for fresh weight on 13 Aug), 15 for dry matter
meas_all %>% count(measure, date)

# One function draws the same figure for any measure
plot_by_rate <- function(m, ylab, title) {
  meas_all %>%
    filter(measure == m) %>%
    mutate(seed_rate = factor(seed_rate, levels = rate_levels),
           sampling  = factor(dm(date), levels = dm(sample_dates))) %>%
    ggplot(aes(seed_rate, value)) +
    geom_jitter(width = 0.12, height = 0, colour = "grey60", size = 1.8) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.25, colour = "black") +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 4, colour = "black") +
    facet_wrap(~ sampling, nrow = 1) +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.text = element_text(face = "bold")) +
    labs(title = paste0(title, ", lentils at Roseworthy ", season_year),
         subtitle = "Black diamonds: mean (error bars = standard error). Grey: individual plots",
         x = "Seeds/m\u00b2", y = ylab, caption = ndvi_caption)
}

plot_plants <- plot_by_rate("plants",       "Plants/m\u00b2",           "Plant density")
plot_fresh  <- plot_by_rate("fresh_weight", "Fresh weight (g/m\u00b2)", "Fresh biomass")
plot_dm     <- plot_by_rate("dry_matter",   "Dry matter (g/m\u00b2)",   "Dry matter") +
  labs(caption = paste0(ndvi_caption, ". Dry matter reported by 3 groups per date"))

plot_plants
plot_fresh
plot_dm

# Mean by seeding rate and date, for the report tables
meas_tbl <- meas_all %>%
  group_by(measure, date, seed_rate) %>%
  summarise(mean = round(mean(value), 1), n = n(), .groups = "drop")
meas_tbl

# Save
ggsave(file.path(crop_dir, "lentil_plants_by_seeding_rate.png"),
       plot_plants, width = 20, height = 12, units = "cm", dpi = 300)
ggsave(file.path(crop_dir, "lentil_fresh_weight_by_seeding_rate.png"),
       plot_fresh, width = 20, height = 12, units = "cm", dpi = 300)
ggsave(file.path(crop_dir, "lentil_dry_matter_by_seeding_rate.png"),
       plot_dm, width = 20, height = 12, units = "cm", dpi = 300)
write_csv(meas_tbl, file.path(crop_dir, "lentil_growth_means_by_seeding_rate.csv"))
