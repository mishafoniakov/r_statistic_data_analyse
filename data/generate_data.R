# Генерация демонстрационных данных для отчётов по контролю качества.
# Запуск из корня проекта: Rscript data/generate_data.R

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

message("Done.")
