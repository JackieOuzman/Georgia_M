library(tidyverse)
library(lubridate)

# ---- User settings ----
sim_file       <- "C:/Users/ouz001/working_from_home_post_Sep2022/Georgia_M/Georgia_mee_write_up/23122.sim"
season_year    <- 2026   # the year your crop was grown
gs_start_month <- 4      # April
gs_end_month   <- 10     # October

# ---- Read the .sim file (find the header row so we don't hard-code skip) ----
sim_lines <- readLines(sim_file)
hdr_row   <- grep("^year\\s+day", sim_lines)[1]

met <- read.table(sim_file,
                  skip = hdr_row + 1,        # skips the header and the units row
                  header = FALSE,
                  col.names = c("year", "day", "radn", "maxt", "mint",
                                "rain", "evap", "vp", "code"),
                  colClasses = c(rep("numeric", 8), "character")) %>%
  as_tibble() %>%
  mutate(date  = make_date(year, 1, 1) + day - 1,
         month = month(date))

# ---- Growing season (Apr-Oct) subset ----
gs <- met %>% filter(month >= gs_start_month, month <= gs_end_month)

# Total rain per year, keeping only complete seasons (Apr-Oct = 214 days)
gs_total <- gs %>%
  group_by(year) %>%
  summarise(gs_rain = sum(rain), n_days = n(), .groups = "drop") %>%
  filter(n_days == 214)

# ---- Deciles (based on all complete seasons in the file) ----
breaks <- quantile(gs_total$gs_rain, probs = seq(0, 1, 0.1))

gs_total <- gs_total %>%
  mutate(decile = as.integer(as.character(
    cut(gs_rain, breaks = breaks, labels = 1:10, include.lowest = TRUE))))

decile_table <- tibble(decile = 1:10,
                       from_mm = round(breaks[-11], 1),
                       to_mm   = round(breaks[-1], 1))
decile_table

# ---- Your season: total, decile, and long-term comparison ----
gs_total %>% filter(year == season_year)

gs_total %>% summarise(mean_mm   = round(mean(gs_rain), 1),
                       median_mm = round(median(gs_rain), 1),
                       driest    = year[which.min(gs_rain)],
                       wettest   = year[which.max(gs_rain)])

# ---- Monthly distribution: your season vs long-term mean ----
monthly <- gs %>%
  group_by(year, month) %>%
  summarise(rain = sum(rain), .groups = "drop")

monthly_ltm <- monthly %>%
  filter(year != season_year) %>%
  group_by(month) %>%
  summarise(long_term_mean = mean(rain), .groups = "drop")

monthly_tbl <- monthly %>%
  filter(year == season_year) %>%
  left_join(monthly_ltm, by = "month") %>%
  mutate(month_name = month(month, label = TRUE)) %>%
  select(month_name, rain, long_term_mean) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))
monthly_tbl

# ---- Figure: monthly rain vs long-term mean ----
# NEW: text describing the months and years used
season_label <- paste0(month.name[gs_start_month], " to ", month.name[gs_end_month], " ", season_year)
record_label <- paste0(min(gs_total$year), " to ", max(gs_total$year[gs_total$year != season_year]))
season_total <- round(gs_total$gs_rain[gs_total$year == season_year], 0)   # NEW
season_decile <- gs_total$decile[gs_total$year == season_year]             # NEW

plot_monthly_rain <- monthly_tbl %>%
  pivot_longer(c(rain, long_term_mean), names_to = "series", values_to = "mm") %>%
  mutate(series = recode(series, rain = paste("Rain", season_year),
                         long_term_mean = "Long-term mean"),
         series = factor(series, levels = c(paste("Rain", season_year), "Long-term mean"))) %>%  # NEW: fixes bar order
  ggplot(aes(month_name, mm, fill = series)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("grey30", "grey75")) +   # NEW: grey bars
  theme_classic() +                                      # NEW: classic theme
  labs(title = "Roseworthy growing season rainfall",
       subtitle = paste0("Season: ", season_label, " (total ", season_total, " mm, decile ", season_decile, ")"),  # NEW
       x = NULL, y = "Rainfall (mm)", fill = NULL,
       caption = paste0("Long-term mean based on ", record_label, ". Source: SILO (BoM only), Roseworthy AWS 023122"))  # NEW

# ---- Save the figure ----
plot_dir <- "C:/Users/ouz001/working_from_home_post_Sep2022/Georgia_M/Georgia_mee_write_up/plot/climate"
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
  
ggsave(filename = file.path(plot_dir, "roseworthy_gs_rain_monthly.png"),
       plot = plot_monthly_rain, width = 20, height = 12, units = "cm", dpi = 300)

# ---- Chunk 2: compare with a low and a high decile year ----
# Picks the year closest to the middle of decile 2 (low) and decile 9 (high).
# To choose your own, overwrite low_year and high_year, e.g. low_year <- 2006
pick_year <- function(d) {
  x <- gs_total %>% filter(decile == d, year != season_year)
  x$year[which.min(abs(x$gs_rain - median(x$gs_rain)))]
}
low_year  <- pick_year(2)
high_year <- pick_year(9)
compare_years <- c(low_year, season_year, high_year)

# Comparison table
compare_tbl <- gs_total %>%
  filter(year %in% compare_years) %>%
  mutate(group = case_when(year == low_year  ~ "Low decile year",
                           year == high_year ~ "High decile year",
                           TRUE              ~ "Your season"),
         gs_rain = round(gs_rain, 1)) %>%
  select(group, year, gs_rain, decile) %>%
  arrange(gs_rain)
compare_tbl

# Label each year with its decile, in the order low, season, high
year_levels <- gs_total %>%
  filter(year %in% compare_years) %>%
  arrange(gs_rain) %>%
  mutate(label = paste0(year, " (decile ", decile, ")")) %>%
  pull(label)

compare_labels <- gs_total %>%
  filter(year %in% compare_years) %>%
  mutate(year_label = factor(paste0(year, " (decile ", decile, ")"), levels = year_levels)) %>%
  select(year, year_label)

# Figure A: monthly rain for the three years
plot_compare_monthly <- monthly %>%
  filter(year %in% compare_years) %>%
  left_join(compare_labels, by = "year") %>%
  mutate(month_name = month(month, label = TRUE)) %>%
  ggplot(aes(month_name, rain, fill = year_label)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("grey80", "grey50", "grey20")) +
  theme_classic() +
  labs(title = "Roseworthy monthly rainfall: low, current and high decile seasons",
       subtitle = paste0("Season: ", season_label),
       x = NULL, y = "Rainfall (mm)", fill = NULL,
       caption = "Source: SILO (BoM only), Roseworthy AWS 023122")

# Figure B: cumulative rain through the season
# Colours and line widths: your season red and thick, comparison years grey
season_lab  <- year_levels[grepl(paste0("^", season_year), year_levels)]
  line_cols   <- setNames(c("grey70", "grey40", "grey20"), year_levels)
  line_cols[season_lab] <- "red"
  line_widths <- setNames(rep(0.8, length(year_levels)), year_levels)
  line_widths[season_lab] <- 1.8
plot_compare_cum <- gs %>%
  filter(year %in% compare_years) %>%
  arrange(year, date) %>%
  group_by(year) %>%
  mutate(cum_rain = cumsum(rain),
         plot_date = as.Date(paste0("2000-", format(date, "%m-%d")))) %>%
  ungroup() %>%
  left_join(compare_labels, by = "year") %>%
  ggplot(aes(plot_date, cum_rain, colour = year_label, linetype = year_label)) +
  geom_line(aes(linewidth = year_label)) +
  scale_colour_manual(values = line_cols) +
  scale_linewidth_manual(values = line_widths) +
  guides(linewidth = "none") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_classic() +
  labs(title = "Cumulative growing season rainfall",
       subtitle = paste0("Season: ", season_label),
       x = NULL, y = "Cumulative rainfall (mm)", colour = NULL, linetype = NULL,
       caption = "Source: SILO (BoM only), Roseworthy AWS 023122")

plot_compare_cum
# Save both figures and the table
ggsave(file.path(plot_dir, "roseworthy_compare_monthly.png"),
       plot_compare_monthly, width = 20, height = 12, units = "cm", dpi = 300)
ggsave(file.path(plot_dir, "roseworthy_compare_cumulative.png"),
       plot_compare_cum, width = 20, height = 12, units = "cm", dpi = 300)
write_csv(compare_tbl, file.path(plot_dir, "roseworthy_compare_years.csv"))

# ---- Chunk 3: timing of rain against crop stage ----
# Dates from the crop monitoring data sheet (East 2 paddock, Thunder XT lentils)
sow_date     <- as.Date("2026-05-21")
sample_dates <- as.Date(c("2026-08-13", "2026-09-24", "2026-10-22"))

dm <- function(d) paste(as.integer(format(d, "%d")), format(d, "%b"))   # e.g. "21 May"

# Stage windows run between the sampling dates, because those are the dates with recorded growth stages
period_start <- c(as.Date(paste0(season_year, "-04-01")), sow_date, sample_dates + 1)
period_end   <- c(sow_date - 1, sample_dates, as.Date(paste0(season_year, "-10-31")))

period_defs <- tibble(
  short = c("Pre-sowing", "Vegetative", "Late veg. to early bloom", "Pod and seed fill", "Maturity"),
  stage = c("Fallow and break of season",
            "Vegetative (V6 to V12 at 1st sampling)",
            "Late vegetative to early bloom (R1 to R3 at 2nd sampling)",
            "Pod set to maturity (R6 to R8 at 3rd sampling)",
            "Maturity and harvest"),
  dates    = paste(dm(period_start), "to", dm(period_end)),
  start_md = format(period_start, "%m-%d"),
  end_md   = format(period_end, "%m-%d")
)
period_defs

# Rain in each window, for a given year (same calendar dates in every year)
period_rain <- function(yr) {
  period_defs %>%
    mutate(start = as.Date(paste0(yr, "-", start_md)),
           end   = as.Date(paste0(yr, "-", end_md))) %>%
    rowwise() %>%
    mutate(rain_mm = round(sum(met$rain[met$date >= start & met$date <= end]), 1)) %>%
    ungroup() %>%
    mutate(year = yr)
}

timing_long <- map_dfr(compare_years, period_rain) %>%
  left_join(compare_labels, by = "year")

# Check: the windows should add up to the season totals (within 0.1 mm)
timing_long %>%
  group_by(year) %>%
  summarise(sum_of_windows = sum(rain_mm), .groups = "drop") %>%
  left_join(select(gs_total, year, gs_rain), by = "year")

# Table: rain per stage window, low year / your season / high year
timing_wide <- timing_long %>%
  select(short, stage, dates, year_label, rain_mm) %>%
  pivot_wider(names_from = year_label, values_from = rain_mm) %>%
  select(short, stage, dates, all_of(year_levels))

timing_wide <- bind_rows(
  timing_wide,
  timing_wide %>%
    summarise(across(all_of(year_levels), ~ round(sum(.x), 1))) %>%
    mutate(short = "Total", stage = "April to October")
)
timing_wide

# Rain around sowing (establishment)
est_tbl <- tibble(year = compare_years) %>%
  rowwise() %>%
  mutate(sow = as.Date(paste0(year, "-", format(sow_date, "%m-%d"))),
         rain_7d_before_sowing = round(sum(met$rain[met$date >= sow - 7 & met$date < sow]), 1),
         rain_14d_after_sowing = round(sum(met$rain[met$date >= sow & met$date < sow + 14]), 1),
         rain_days_14d_after   = sum(met$rain[met$date >= sow & met$date < sow + 14] >= 1)) %>%   # days with 1 mm or more
  ungroup() %>%
  left_join(compare_labels, by = "year") %>%
  arrange(year_label) %>%
  select(year_label, rain_7d_before_sowing, rain_14d_after_sowing, rain_days_14d_after)
est_tbl

# Figure: daily rain for your season with crop stage bands
bands <- period_defs %>%
  left_join(filter(timing_long, year == season_year) %>% select(short, rain_mm), by = "short") %>%
  mutate(xmin  = as.Date(paste0(season_year, "-", start_md)),
         xmax  = as.Date(paste0(season_year, "-", end_md)) + 1,
         shade = factor(row_number() %% 2),
         label = paste0(str_wrap(short, 10), "\n", rain_mm, " mm"))

plot_timing <- ggplot() +
  geom_rect(data = bands,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = shade)) +
  scale_fill_manual(values = c("0" = "grey95", "1" = "grey85"), guide = "none") +
  geom_col(data = filter(gs, year == season_year), aes(date, rain),
           fill = "blue", width = 1) +
  geom_vline(xintercept = c(sow_date, sample_dates),
             linetype = c("solid", "dashed", "dashed", "dashed"), colour = "grey20") +
  geom_text(data = bands, aes(x = xmin + (xmax - xmin) / 2, y = Inf, label = label),
            vjust = 1.2, size = 3, colour = "grey20", lineheight = 0.9) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               expand = expansion(mult = c(0, 0.04))) +
  theme_classic() +
  labs(title = paste0("Daily rainfall and crop stage, Roseworthy ", season_year),
       subtitle = paste0("Thunder XT lentils sown ", dm(sow_date),
                         ". Bands show crop stage and rainfall (mm) in each"),
       x = NULL, y = "Daily rainfall (mm)",
       caption = str_wrap(paste0("Solid line: sowing. Dashed lines: sampling dates (",
                                 paste(dm(sample_dates), collapse = ", "),
                                 "). Source: SILO (BoM only), Roseworthy AWS 023122"), 110))

plot_timing
ggsave(file.path(plot_dir, "roseworthy_rain_timing_stages.png"),
       plot_timing, width = 20, height = 12, units = "cm", dpi = 300)
write_csv(timing_wide, file.path(plot_dir, "roseworthy_rain_by_stage.csv"))
write_csv(est_tbl,     file.path(plot_dir, "roseworthy_rain_at_sowing.csv"))

# ---- Chunk 4: temperature extremes, frost and heat ----
frost_thr <- 2     # minimum at or below this (deg C) = frost-risk day
heat_thr  <- 30    # maximum at or above this (deg C) = heat day

# Temperature summary for each stage window, for a given year
temp_window <- function(yr) {
  period_defs %>%
    mutate(start = as.Date(paste0(yr, "-", start_md)),
           end   = as.Date(paste0(yr, "-", end_md))) %>%
    mutate(stats = map2(start, end, function(s, e) {
      w <- filter(met, date >= s, date <= e)
      tibble(frost_days    = sum(w$mint <= frost_thr),
             frost_days_0C = sum(w$mint <= 0),
             heat_days     = sum(w$maxt >= heat_thr),
             heat_days_35C = sum(w$maxt >= 35),
             mean_max      = round(mean(w$maxt), 1),
             mean_min      = round(mean(w$mint), 1),
             lowest_min    = min(w$mint),
             highest_max   = max(w$maxt))
    })) %>%
    unnest(stats) %>%
    mutate(year = yr)
}

temp_long <- map_dfr(compare_years, temp_window) %>%
  left_join(compare_labels, by = "year")

temp_tbl <- temp_long %>%
  select(short, dates, year_label, frost_days, frost_days_0C, heat_days, heat_days_35C,
         mean_max, mean_min, lowest_min, highest_max) %>%
  arrange(factor(short, levels = period_defs$short), year_label)
print(temp_tbl, n = Inf, width = Inf)

# Figure: daily max and min temperature for your season, with stage bands
temp_daily <- gs %>%
  filter(year == season_year) %>%
  select(date, maxt, mint) %>%
  pivot_longer(c(maxt, mint), names_to = "series", values_to = "temp") %>%
  mutate(series = recode(series, maxt = "Daily maximum", mint = "Daily minimum"))

bands_temp <- bands %>% mutate(label = str_wrap(short, 10))

plot_temp <- ggplot() +
  geom_rect(data = bands_temp,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = shade)) +
  scale_fill_manual(values = c("0" = "grey95", "1" = "grey85"), guide = "none") +
  geom_hline(yintercept = c(frost_thr, heat_thr), linetype = "dotted", colour = "grey40") +
  geom_vline(xintercept = c(sow_date, sample_dates),
             linetype = c("solid", "dashed", "dashed", "dashed"), colour = "grey20") +
  geom_line(data = temp_daily, aes(date, temp, colour = series), linewidth = 0.5) +
  scale_colour_manual(values = c("Daily maximum" = "red", "Daily minimum" = "grey30")) +
  geom_text(data = bands_temp, aes(x = xmin + (xmax - xmin) / 2, y = Inf, label = label),
            vjust = 1.2, size = 3, colour = "grey20", lineheight = 0.9) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.15))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               expand = expansion(mult = c(0, 0.04))) +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(title = paste0("Daily temperature and crop stage, Roseworthy ", season_year),
       subtitle = paste0("Thunder XT lentils sown ", dm(sow_date)),
       x = NULL, y = "Temperature (\u00b0C)", colour = NULL,
       caption = str_wrap(paste0("Dotted lines: frost-risk (", frost_thr, " \u00b0C) and heat (", heat_thr,
                                 " \u00b0C) thresholds. Solid vertical line: sowing. Dashed vertical lines: sampling dates (",
                                 paste(dm(sample_dates), collapse = ", "),
                                 "). Source: SILO (BoM only), Roseworthy AWS 023122"), 110))

plot_temp

# ---- Appendix tables: detailed weather data for your season ----
appendix_monthly <- gs %>%
  filter(year == season_year) %>%
  group_by(month) %>%
  summarise(rain_mm        = round(sum(rain), 1),
            rain_days_1mm  = sum(rain >= 1),
            mean_max_C     = round(mean(maxt), 1),
            mean_min_C     = round(mean(mint), 1),
            highest_max_C  = max(maxt),
            lowest_min_C   = min(mint),
            frost_days     = sum(mint <= frost_thr),
            heat_days      = sum(maxt >= heat_thr),
            mean_radn_MJm2 = round(mean(radn), 1),
            evap_mm        = round(sum(evap), 1),
            .groups = "drop") %>%
  mutate(month_name = month(month, label = TRUE, abbr = FALSE)) %>%
  select(month_name, everything(), -month)
print(appendix_monthly, width = Inf)

appendix_daily <- gs %>%
  filter(year == season_year) %>%
  select(date, radn, maxt, mint, rain, evap, vp)

# ---- Save ----
ggsave(file.path(plot_dir, "roseworthy_temp_timing_stages.png"),
       plot_temp, width = 20, height = 12, units = "cm", dpi = 300)
write_csv(temp_tbl,          file.path(plot_dir, "roseworthy_temp_by_stage.csv"))
write_csv(appendix_monthly,  file.path(plot_dir, "roseworthy_appendix_monthly_weather.csv"))
write_csv(appendix_daily,    file.path(plot_dir, "roseworthy_appendix_daily_weather.csv"))

# ---- Summary table: frost and heat in the late veg. to early bloom and pod fill windows ----
measure_labels <- c(
  frost_days    = paste0("Frost-risk days (", frost_thr, " \u00b0C or below)"),
  frost_days_0C = "Days at or below 0 \u00b0C",
  heat_days     = paste0("Days at or above ", heat_thr, " \u00b0C"),
  heat_days_35C = "Days at or above 35 \u00b0C",
  highest_max   = "Highest maximum (\u00b0C)",
  mean_min      = "Mean minimum (\u00b0C)")

summary_tbl <- temp_long %>%
  filter(short %in% c("Late veg. to early bloom", "Pod and seed fill")) %>%
  mutate(window = paste0(short, " (", dates, ")")) %>%
  select(window, short, year_label, all_of(names(measure_labels))) %>%
  pivot_longer(all_of(names(measure_labels)), names_to = "measure", values_to = "value") %>%
  mutate(measure = factor(measure, levels = names(measure_labels),
                          labels = unname(measure_labels))) %>%
  pivot_wider(names_from = year_label, values_from = value) %>%
  arrange(factor(short, levels = period_defs$short), measure) %>%
  select(window, measure, all_of(year_levels))
print(summary_tbl, n = Inf, width = Inf)

# write_excel_csv keeps the degree symbol readable when the file is opened in Excel
write_excel_csv(summary_tbl, file.path(plot_dir, "roseworthy_summary_frost_heat.csv"))
