# Генерация демонстрационных данных для отчётов по контролю качества.
# Запуск из корня проекта: Rscript tools/generate_data.R

set.seed(42)

out_dir <- if (dir.exists("data")) "data" else "."

n_items <- 3000L
n_output_rows <- 6000L

series_levels <- c(
  "Серия_1", "Серия_2", "Серия_3",
  paste0("Серия_", LETTERS[c(1:13)])
)
plant_levels <- paste0("Площадка_", 1:6)
mark_levels <- c("Неизвестно", "Марка_X", "Марка_Y", "Марка_Z", "Марка_M")
method_levels <- c("Стандартный", "Ускоренный", "Специальный")
class_levels <- c("Неизвестно", "Класс_A_тек", "Класс_B_тек", "Класс_A", "Класс_B")
type_levels <- c("A", "B")
year_levels <- 2012:2024
factory_levels <- c("Завод_А", "Завод_Б", "Завод_В")

year_prob <- c(0.025, 0.025, 0.03, 0.03, 0.04, 0.05, 0.07, 0.08, 0.10, 0.12, 0.13, 0.14, 0.16)

clip01 <- function(x) pmin(pmax(x, 0.01), 0.99)

item_id <- seq_len(n_items)
rec_id <- 100000L + item_id

type <- sample(type_levels, n_items, replace = TRUE, prob = c(0.55, 0.45))
year <- sample(year_levels, n_items, replace = TRUE, prob = year_prob)
series <- sample(series_levels, n_items, replace = TRUE)
plant <- sample(plant_levels, n_items, replace = TRUE)
mark <- sample(mark_levels, n_items, replace = TRUE, prob = c(0.18, 0.22, 0.22, 0.22, 0.16))
method <- sample(method_levels, n_items, replace = TRUE, prob = c(0.55, 0.30, 0.15))
class <- sample(class_levels, n_items, replace = TRUE, prob = c(0.12, 0.22, 0.22, 0.22, 0.22))
certified <- sample(c("Да", "Нет"), n_items, replace = TRUE, prob = c(0.35, 0.65))

series_bonus <- as.numeric(factor(series, levels = series_levels)) / 20
plant_bonus <- as.numeric(factor(plant, levels = plant_levels)) / 30
year_bonus <- (year - 2012) / 40
type_bonus <- ifelse(type == "A", 0.08, -0.04)
method_bonus <- ifelse(method == "Ускоренный", 0.12, ifelse(method == "Стандартный", 0.04, -0.02))

output <- 88 + 12 * year_bonus + 8 * series_bonus + 6 * type_bonus + rnorm(n_items, 0, 8)
related <- 90 + 10 * year_bonus + 6 * plant_bonus + 0.35 * (output - 88) + rnorm(n_items, 0, 7)
frequency <- 1.4 + 0.6 * year_bonus + rnorm(n_items, 0, 0.35)
yield <- 86 + 9 * year_bonus + 5 * method_bonus * 10 + 0.25 * (output - 88) + rnorm(n_items, 0, 7)
volume <- 2.1 + 0.4 * plant_bonus + rnorm(n_items, 0, 0.45)
defect <- 18 - 6 * year_bonus - 4 * method_bonus * 10 + rnorm(n_items, 0, 6)

output <- pmax(output, 40)
related <- pmax(related, 40)
frequency <- pmax(frequency, 0.2)
yield <- pmin(pmax(yield, 40), 99)
volume <- pmax(volume, 0.3)
defect <- pmin(pmax(defect, 1), 60)

overall_index <- 0.28 * output + 0.18 * related + 0.16 * yield +
  0.12 * (frequency * 40) + 0.10 * (volume * 20) - 0.35 * defect + rnorm(n_items, 0, 4)
process_index <- 0.35 * output + 0.25 * related + 0.20 * (frequency * 40) +
  0.20 * yield + rnorm(n_items, 0, 4)
product_index <- 0.40 * output + 0.30 * related - 0.35 * defect + rnorm(n_items, 0, 4)

parent_a <- ifelse(runif(n_items) < 0.65, sample(rec_id, n_items, replace = TRUE), NA_integer_)
parent_b <- ifelse(runif(n_items) < 0.65, sample(rec_id, n_items, replace = TRUE), NA_integer_)
parent_a[parent_a == rec_id] <- NA_integer_
parent_b[parent_b == rec_id] <- NA_integer_

recent <- year >= 2020
p_out <- ifelse(type == "B", 0.75, 0.62)
p_out <- ifelse(recent, pmax(p_out, 0.82), p_out * 0.35)
p_rel <- ifelse(type == "A", 0.70, 0.18)
p_yld <- ifelse(type == "A", 0.68, 0.20)
p_vol <- ifelse(type == "A", 0.65, 0.18)
p_frq <- ifelse(type == "A", 0.63, 0.15)
p_def <- ifelse(type == "B", 0.28, 0.08)

is_output <- runif(n_items) < p_out
is_related <- runif(n_items) < p_rel
is_yield <- runif(n_items) < p_yld
is_volume <- runif(n_items) < p_vol
is_frequency <- runif(n_items) < p_frq
is_defect <- runif(n_items) < p_def

rel_draw <- function(has_measure, certified_yes, recent_flag) {
  base <- ifelse(has_measure, rbeta(n_items, 9, 2), rbeta(n_items, 3, 5))
  base <- base + 0.05 * as.integer(certified_yes == "Да") + 0.04 * as.integer(recent_flag)
  clip01(base + rnorm(n_items, 0, 0.04))
}

rel_output <- rel_draw(is_output, certified, recent)
rel_related <- rel_draw(is_related, certified, recent)
rel_yield <- rel_draw(is_yield, certified, recent)
rel_volume <- rel_draw(is_volume, certified, recent)
rel_frequency <- rel_draw(is_frequency, certified, recent)
rel_defect <- rel_draw(is_defect, certified, recent)
rel_defect <- clip01(rel_defect - 0.12)

write_csv <- function(df, name) {
  path <- file.path(out_dir, name)
  write.csv(df, path, row.names = FALSE, fileEncoding = "UTF-8")
  message("Wrote ", path, " (", nrow(df), " rows)")
}

scores <- data.frame(
  item_id, overall_index, process_index, product_index,
  output, related, frequency, yield, volume, defect,
  type, year, series, class, plant, mark, method, certified,
  stringsAsFactors = FALSE
)
write_csv(scores, "scores.csv")

scores_norm <- scores
write_csv(scores_norm, "scores_norm.csv")

metric_averages <- data.frame(
  item_id,
  output = output / 1000 + rnorm(n_items, 0, 0.004),
  related = related / 1000 + rnorm(n_items, 0, 0.004),
  frequency = frequency + rnorm(n_items, 0, 0.05),
  yield = yield / 100 + rnorm(n_items, 0, 0.01),
  volume = volume + rnorm(n_items, 0, 0.05),
  defect = defect + rnorm(n_items, 0, 1.5),
  stringsAsFactors = FALSE
)
write_csv(metric_averages, "metric_averages.csv")

scores_accuracy <- data.frame(
  rec_id, item_id, type, series, year, plant, method, parent_a, parent_b, certified,
  output, related, frequency, yield, volume, defect,
  rel_output, rel_related, rel_frequency, rel_yield, rel_volume, rel_defect,
  stringsAsFactors = FALSE
)
write_csv(scores_accuracy, "scores_accuracy.csv")

registry <- data.frame(rec_id, item_id)
write_csv(registry, "registry.csv")

make_metric_file <- function(ids, n_extra, z_sd) {
  keep <- item_id[ids]
  extra <- sample(keep, max(0, n_extra - length(keep)), replace = TRUE)
  ids_all <- c(keep, extra)
  n <- length(ids_all)
  data.frame(
    item_id = ids_all,
    target = exp(rnorm(n, 1.1, z_sd)),
    stringsAsFactors = FALSE
  )
}

metric_related <- make_metric_file(is_related, sum(is_related), 0.45)
metric_yield <- make_metric_file(is_yield, sum(is_yield), 0.40)
metric_volume <- make_metric_file(is_volume, sum(is_volume), 0.40)
metric_frequency <- make_metric_file(is_frequency, sum(is_frequency), 0.50)
metric_defect <- make_metric_file(is_defect, sum(is_defect), 0.55)
write_csv(metric_related, "metric_related.csv")
write_csv(metric_yield, "metric_yield.csv")
write_csv(metric_volume, "metric_volume.csv")
write_csv(metric_frequency, "metric_frequency.csv")
write_csv(metric_defect, "metric_defect.csv")

factory <- sample(factory_levels, n_output_rows, replace = TRUE, prob = c(0.45, 0.35, 0.20))
quarter <- sample(1:4, n_output_rows, replace = TRUE)
meas_year <- sample(year_levels, n_output_rows, replace = TRUE, prob = year_prob)
batch_no <- sample(1:5, n_output_rows, replace = TRUE)
age <- pmax(round(rnorm(n_output_rows, 18, 6)), 1)
batch_size <- pmax(round(rnorm(n_output_rows, 12, 4)), 1)
mass <- pmax(rnorm(n_output_rows, 4.2, 0.9), 0.5)
temp <- rnorm(n_output_rows, 21, 3)
pressure <- rnorm(n_output_rows, 101.3, 4)
humidity <- pmin(pmax(rnorm(n_output_rows, 55, 12), 10), 95)

factory_mu <- c("Завод_А" = 3.4, "Завод_Б" = 3.9, "Завод_В" = 3.1)
factory_sd <- c("Завод_А" = 0.35, "Завод_Б" = 0.70, "Завод_В" = 0.45)
mu <- factory_mu[factory] + 0.08 * (quarter == 4) + 0.04 * (meas_year - 2018) / 4
sdev <- factory_sd[factory]
target <- exp(rnorm(n_output_rows, mu, sdev))
n_outliers <- 80L
target[sample.int(n_output_rows, n_outliers)] <- target[sample.int(n_output_rows, n_outliers)] * runif(n_outliers, 4, 9)

metric_output <- data.frame(
  item_id = sample(item_id[is_output], n_output_rows, replace = TRUE),
  target, quarter, factory, year = meas_year, batch_no,
  age, batch_size, mass, temp, pressure, humidity,
  stringsAsFactors = FALSE
)
write_csv(metric_output, "metric_output.csv")

# --- Лабораторные панели маркеров (отчёт 06) ---------------------------------

n_samples <- 220L
n_markers <- 48L
sample_ids <- sprintf("S%04d", seq_len(n_samples))
marker_names <- sprintf("M%03d", seq_len(n_markers))
n_channels <- 8L

marker_map <- data.frame(
  name = marker_names,
  channel = ((seq_len(n_markers) - 1L) %% n_channels) + 1L,
  position = 100L + seq_len(n_markers) * 37L,
  stringsAsFactors = FALSE
)
marker_map$channel[c(5, 19)] <- 0L
write_csv(marker_map, "marker_map.csv")

maf <- runif(n_markers, 0.08, 0.48)
true_state <- matrix(NA_character_, n_samples, n_markers)
for (j in seq_len(n_markers)) {
  p <- maf[j]
  probs <- c((1 - p)^2, 2 * p * (1 - p), p^2)
  true_state[, j] <- sample(c("AA", "AB", "BB"), n_samples, replace = TRUE, prob = probs)
}
true_state[, 7] <- "AA"
true_state[, 31] <- sample(c("AA", "AB"), n_samples, replace = TRUE, prob = c(0.97, 0.03))

make_lab_year <- function(year, sample_keep, extra_noise = 0.04) {
  si <- match(sample_keep, sample_ids)
  grid <- expand.grid(si = si, j = seq_len(n_markers), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  n <- nrow(grid)
  call <- true_state[cbind(grid$si, grid$j)]
  flip <- runif(n) < extra_noise
  call[flip] <- sample(c("AA", "AB", "BB"), sum(flip), replace = TRUE)
  qc <- rbeta(n, 12, 2)
  low <- runif(n) < 0.06
  qc[low] <- rbeta(sum(low), 2, 6)
  keep <- runif(n) >= 0.04
  grid <- grid[keep, , drop = FALSE]
  call <- call[keep]
  qc <- qc[keep]
  base <- data.frame(
    sample_id = sample_ids[grid$si],
    marker_name = marker_names[grid$j],
    allele1 = substr(call, 1, 1),
    allele2 = substr(call, 2, 2),
    qc_score = round(qc, 4),
    year = year,
    stringsAsFactors = FALSE
  )
  dup_n <- max(1L, round(0.03 * nrow(base)))
  dup_idx <- sample.int(nrow(base), dup_n)
  alt <- sample(c("AA", "AB", "BB"), dup_n, replace = TRUE)
  extra <- base[dup_idx, ]
  extra$allele1 <- substr(alt, 1, 1)
  extra$allele2 <- substr(alt, 2, 2)
  extra$qc_score <- round(rbeta(dup_n, 8, 3), 4)
  rbind(base, extra)
}

lab_2020 <- make_lab_year(2020, sample_ids[1:140], 0.05)
lab_2021 <- make_lab_year(2021, sample_ids[40:180], 0.04)
lab_2022 <- make_lab_year(2022, sample_ids[80:210], 0.035)
lab_2023 <- make_lab_year(2023, sample_ids[100:220], 0.03)
write_csv(lab_2020, "lab_report_2020.csv")
write_csv(lab_2021, "lab_report_2021.csv")
write_csv(lab_2022, "lab_report_2022.csv")
write_csv(lab_2023, "lab_report_2023.csv")

# --- Кривые процесса и среда (отчёт 07) --------------------------------------

n_curve <- 1800L
curve_item <- sample(item_id, n_curve, replace = TRUE)
curve_age <- pmax(round(rgamma(n_curve, shape = 3.2, scale = 18)), 1)
curve_factory <- sample(factory_levels, n_curve, replace = TRUE, prob = c(0.42, 0.35, 0.23))
curve_mark <- sample(mark_levels, n_curve, replace = TRUE, prob = c(0.10, 0.24, 0.24, 0.24, 0.18))
curve_type <- sample(type_levels, n_curve, replace = TRUE, prob = c(0.55, 0.45))
factory_shift <- c("Завод_А" = 0.0, "Завод_Б" = 2.4, "Завод_В" = -1.6)
mark_shift <- c(
  "Неизвестно" = -0.8, "Марка_X" = 1.5, "Марка_Y" = 0.4,
  "Марка_Z" = -0.3, "Марка_M" = 0.9
)
type_shift <- c("A" = 0.8, "B" = -0.5)
mass_hat <- 8.5 + 0.22 * curve_age - 0.00028 * curve_age^2 +
  factory_shift[curve_factory] + mark_shift[curve_mark] + type_shift[curve_type]
curve_mass <- pmax(mass_hat + rnorm(n_curve, 0, 2.4), 0.4)
n_mass_out <- 70L
curve_mass[sample.int(n_curve, n_mass_out)] <- curve_mass[sample.int(n_curve, n_mass_out)] *
  runif(n_mass_out, 2.2, 4.5)

process_curve <- data.frame(
  item_id = curve_item,
  age = curve_age,
  mass = round(curve_mass, 3),
  factory = curve_factory,
  mark = curve_mark,
  type = curve_type,
  stringsAsFactors = FALSE
)
write_csv(process_curve, "process_curve.csv")

n_env_days <- 900L
env_dates <- seq.Date(as.Date("2022-01-01"), by = "day", length.out = n_env_days)
wind_full <- c(
  "Ветер, дующий с севера",
  "Ветер, дующий с северо-востока",
  "Ветер, дующий с востока",
  "Ветер, дующий с юго-востока",
  "Ветер, дующий с юга",
  "Ветер, дующий с юго-запада",
  "Ветер, дующий с запада",
  "Ветер, дующий с северо-запада",
  "Штиль, безветрие",
  "Переменное направление"
)
env_grid <- expand.grid(
  place = factory_levels,
  date = env_dates,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
env_grid <- env_grid[runif(nrow(env_grid)) >= 0.08, ]
n_env <- nrow(env_grid)
base_t <- c("Завод_А" = 9, "Завод_Б" = 12, "Завод_В" = 6)[env_grid$place]
doy <- as.integer(format(env_grid$date, "%j"))
seasonal <- 11 * sin(2 * pi * (doy - 80) / 365)
temp <- round(base_t + seasonal + rnorm(n_env, 0, 2.2), 1)
hour <- sample(c(3, 9, 15, 21), n_env, replace = TRUE)
vv_num <- as.character(round(runif(n_env, 0.2, 18), 1))
vv_txt <- sample(c("10.0 и более", "менее 0.1", "менее 0.05"), n_env, replace = TRUE)
use_txt <- runif(n_env) < 0.08
environment_raw <- data.frame(
  datetime = format(
    as.POSIXct(paste(env_grid$date, sprintf("%02d:00", hour))),
    "%d.%m.%Y %H:%M"
  ),
  temp = temp,
  P = round(1013 + rnorm(n_env, 0, 8), 1),
  U = round(pmin(pmax(62 + rnorm(n_env, 0, 14), 18), 96), 0),
  DD = sample(wind_full, n_env, replace = TRUE),
  Ff = round(pmax(rnorm(n_env, 3.2, 1.6), 0), 1),
  VV = ifelse(use_txt, vv_txt, vv_num),
  Td = round(temp - runif(n_env, 1, 6), 1),
  place = env_grid$place,
  stringsAsFactors = FALSE
)
write_csv(environment_raw, "environment_raw.csv")

n_events <- 1600L
event_dates <- sample(env_dates, n_events, replace = TRUE)
event_factory <- sample(factory_levels, n_events, replace = TRUE, prob = c(0.42, 0.35, 0.23))
event_year <- as.integer(format(event_dates, "%Y"))
event_season <- as.integer(cut(as.integer(format(event_dates, "%m")), breaks = c(0, 3, 6, 9, 12), labels = FALSE))
event_temp <- 10 + 8 * sin(2 * pi * (as.integer(format(event_dates, "%j")) - 80) / 365) +
  c("Завод_А" = 0, "Завод_Б" = 2.5, "Завод_В" = -3)[event_factory] + rnorm(n_events, 0, 1.8)
event_p <- 1013 + rnorm(n_events, 0, 6)
event_u <- pmin(pmax(60 + rnorm(n_events, 0, 12), 20), 95)
event_trait <- 42 + 0.18 * (event_temp - 10) - 0.04 * (event_p - 1013) - 0.05 * (event_u - 60) +
  c("Завод_А" = 1.2, "Завод_Б" = -0.4, "Завод_В" = 0.3)[event_factory] +
  0.8 * (event_season == 3) + rnorm(n_events, 0, 3.1)

process_events <- data.frame(
  item_id = sample(item_id, n_events, replace = TRUE),
  event_year = event_year,
  event_season = event_season,
  factory = event_factory,
  temp = round(as.numeric(event_temp), 2),
  pressure = round(as.numeric(event_p), 2),
  humidity = round(as.numeric(event_u), 2),
  trait = round(as.numeric(event_trait), 3),
  stringsAsFactors = FALSE
)
write_csv(process_events, "process_events.csv")

message("Done.")
