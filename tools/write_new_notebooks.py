#!/usr/bin/env python3
"""Write notebooks 06 and 07 in the same JSON shape as 01–05."""

from __future__ import annotations

import json
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

METADATA = {
    "kernelspec": {
        "display_name": "R",
        "language": "r",
        "name": "ir",
    },
    "language_info": {
        "name": "R",
        "codemirror_mode": "r",
        "file_extension": ".r",
        "mimetype": "text/x-r-source",
        "pygments_lexer": "r",
    },
}


def src(text: str) -> list[str]:
    text = text.strip("\n")
    if not text:
        return ["\n"]
    lines = text.split("\n")
    out = [line + "\n" for line in lines[:-1]]
    out.append(lines[-1] + "\n")
    return out


def md(text: str) -> dict:
    return {"cell_type": "markdown", "metadata": {}, "source": src(text)}


def code(text: str) -> dict:
    return {
        "cell_type": "code",
        "execution_count": None,
        "metadata": {},
        "outputs": [],
        "source": src(text),
    }


def dump(path: Path, cells: list[dict]) -> None:
    nb = {
        "nbformat": 4,
        "nbformat_minor": 5,
        "metadata": METADATA,
        "cells": cells,
    }
    path.write_text(json.dumps(nb, ensure_ascii=False, indent=1) + "\n", encoding="utf-8")
    print("wrote", path.relative_to(ROOT), "cells", len(cells))


def notebook_06() -> list[dict]:
    return [
        md("# 06. Лабораторные панели маркеров"),
        code(
            """knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE,
  fig.align = "center",
  fig.width = 9,
  fig.height = 5
)"""
        ),
        md(
            """# О документе

Отчёт собирает сырые лабораторные панели в матрицу маркеров, пригодную для смешанной модели качества.

| | |
|---|---|
| **Данные** | `data/lab_report_2020.csv` … `data/lab_report_2023.csv`, `data/marker_map.csv` |
| **Цель** | Отфильтровать нестабильные и пустые вызовы, выровнять кодировку, посчитать call rate и индекс однородности |
| **Результат** | Числовая матрица `0/1/2/5` и карта маркеров по каналам; решение, какие образцы и маркеры оставлять в модели |"""
        ),
        code(
            """library(dplyr)
library(tidyr)
library(ggplot2)"""
        ),
        code(
            """threshold_equal <- 0.75
threshold_qc <- 0.75
min_callrate <- 0.85

lab_2020 <- read.csv("data/lab_report_2020.csv")
lab_2021 <- read.csv("data/lab_report_2021.csv")
lab_2022 <- read.csv("data/lab_report_2022.csv")
lab_2023 <- read.csv("data/lab_report_2023.csv")
marker_map <- read.csv("data/marker_map.csv")

raw <- bind_rows(lab_2020, lab_2021, lab_2022, lab_2023)"""
        ),
        md(
            """# 1. Характеристика сырых отчётов

Четыре годовых выгрузки лаборатории в длинном формате: образец × маркер × вызов. Повторяющиеся строки — повторные прогоны одной и той же пары."""
        ),
        code("summary(raw)"),
        code(
            """raw %>%
  count(year) %>%
  knitr::kable(caption = "Число записей по годам")"""
        ),
        code(
            """ggplot(raw, aes(x = qc_score, fill = factor(year))) +
  geom_histogram(bins = 40, color = "white", alpha = 0.9) +
  facet_wrap(~year, ncol = 2) +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  labs(title = "Распределение qc_score по годам", x = "qc_score", y = "Число записей") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))"""
        ),
        md(
            """# 2. Фильтрация вызовов

Оставляем маркеры, у которых нет доминирующего единственного вызова (`n_equals / n_calls ≤ 0.75`), и строки с `qc_score ≥ 0.75`. Для пары образец–маркер берём уникальную запись, пропуски кодируем как `--`."""
        ),
        code(
            """filtered <- raw %>%
  group_by(marker_name, allele1, allele2) %>%
  mutate(n_equals = n()) %>%
  group_by(marker_name) %>%
  mutate(n_calls = n()) %>%
  filter(n_equals / n_calls <= threshold_equal) %>%
  ungroup() %>%
  filter(qc_score >= threshold_qc) %>%
  mutate(call = paste0(allele1, allele2)) %>%
  select(sample_id, marker_name, call) %>%
  distinct(sample_id, marker_name, .keep_all = TRUE) %>%
  pivot_wider(names_from = marker_name, values_from = call) %>%
  mutate(across(-sample_id, ~ ifelse(is.na(.), "--", as.character(.))))

n_raw_markers <- length(unique(raw$marker_name))
n_wide_markers <- ncol(filtered) - 1
c(
  samples = nrow(filtered),
  markers = n_wide_markers,
  raw_markers = n_raw_markers
)"""
        ),
        md(
            """# 3. Выравнивание кодировки

Если гомозигота `BB` встречается чаще `AA`, пары меняются местами. Так мажорное состояние становится референсом `AA`, и матрица сравнивается между партиями."""
        ),
        code(
            """flip_reference <- function(df) {
  marker_cols <- setdiff(names(df), "sample_id")
  for (col in marker_cols) {
    x <- df[[col]]
    n_aa <- sum(x == "AA", na.rm = TRUE)
    n_bb <- sum(x == "BB", na.rm = TRUE)
    if (n_bb > n_aa) {
      df[[col]] <- dplyr::case_when(
        x == "AA" ~ "BB",
        x == "BB" ~ "AA",
        TRUE ~ x
      )
    }
  }
  df
}

wide <- flip_reference(filtered)
wide$sample_id <- as.factor(wide$sample_id)
marker_cols <- setdiff(names(wide), "sample_id")
head(wide[, c("sample_id", marker_cols[1:6])])"""
        ),
        md(
            """# 4. Call rate и числовая кодировка

`AA → 0`, `AB → 1`, `BB → 2`, пропуск `-- → 5`. Call rate — доля непропущенных вызовов по маркеру и по образцу. Порог отсечения `0.85`."""
        ),
        code(
            """encode_panel <- function(df, marker_cols) {
  mat <- as.matrix(df[, marker_cols])
  num <- matrix(5, nrow = nrow(mat), ncol = ncol(mat))
  num[mat == "AA"] <- 0
  num[mat == "AB" | mat == "BA"] <- 1
  num[mat == "BB"] <- 2
  colnames(num) <- marker_cols
  num
}

panel <- encode_panel(wide, marker_cols)
callrate_marker <- colMeans(panel != 5)
callrate_sample <- rowMeans(panel != 5)

keep_marker <- callrate_marker >= min_callrate
keep_sample <- callrate_sample >= min_callrate
panel_qc <- panel[keep_sample, keep_marker, drop = FALSE]
wide_qc <- wide[keep_sample, c("sample_id", names(keep_marker)[keep_marker]), drop = FALSE]

data.frame(
  metric = c("callrate_marker_mean", "callrate_sample_mean", "markers_kept", "samples_kept"),
  value = c(
    mean(callrate_marker),
    mean(callrate_sample),
    sum(keep_marker),
    sum(keep_sample)
  )
)"""
        ),
        code(
            """cr_marker <- data.frame(marker = names(callrate_marker), callrate = as.numeric(callrate_marker))
ggplot(cr_marker, aes(x = callrate)) +
  geom_histogram(bins = 20, fill = "#66C2A5", color = "white") +
  geom_vline(xintercept = min_callrate, linetype = "dashed", color = "firebrick") +
  labs(title = "Call rate по маркерам", x = "call rate", y = "Число маркеров") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))"""
        ),
        code(
            """cr_sample <- data.frame(sample_id = wide$sample_id, callrate = as.numeric(callrate_sample))
ggplot(cr_sample, aes(x = callrate)) +
  geom_histogram(bins = 20, fill = "#FC8D62", color = "white") +
  geom_vline(xintercept = min_callrate, linetype = "dashed", color = "firebrick") +
  labs(title = "Call rate по образцам", x = "call rate", y = "Число образцов") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))"""
        ),
        md(
            """# 5. Карта маркеров

Маркеры с каналом `0` отбрасываются как служебные. Оставшиеся выстраиваются в том же порядке, что и столбцы матрицы."""
        ),
        code(
            """map_clean <- marker_map %>%
  filter(channel != 0) %>%
  filter(name %in% colnames(panel_qc)) %>%
  arrange(name)

panel_aligned <- panel_qc[, map_clean$name, drop = FALSE]
map_clean$number <- seq_len(nrow(map_clean))

knitr::kable(head(map_clean, 10), caption = "Карта маркеров после очистки")
c(markers_on_map = nrow(map_clean), matrix_cols = ncol(panel_aligned))"""
        ),
        md(
            """# 6. Индекс однородности

Для каждого образца считаем долю гомозиготных вызовов (`0` или `2`) среди успешных. Индекс — относительное отклонение от среднего по панели: положительное значение значит, что изделие однороднее типичного."""
        ),
        code(
            """hom_mat <- (panel_aligned == 0) | (panel_aligned == 2)
called_mat <- panel_aligned != 5
hom_count <- rowSums(hom_mat)
called <- rowSums(called_mat)
hom_rate <- hom_count / pmax(called, 1)
avg_hom <- mean(hom_rate)
purity <- data.frame(
  sample_id = wide_qc$sample_id,
  hom_rate = hom_rate,
  purity_index = (hom_rate - avg_hom) / avg_hom
)

summary(purity$purity_index)"""
        ),
        code(
            """ggplot(purity, aes(x = purity_index)) +
  geom_histogram(bins = 25, fill = "#8DA0CB", color = "white") +
  labs(title = "Индекс однородности панели", x = "purity_index", y = "Число образцов") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))"""
        ),
        md(
            """# 7. Матрица для модели

Строка — образец, столбцы склеены в одну строку кодов. Пропуски остаются `5`, чтобы смешанная модель отличала их от гетерозиготы."""
        ),
        code(
            """encoded <- data.frame(
  sample_id = wide_qc$sample_id,
  combined = apply(panel_aligned, 1, paste0, collapse = ""),
  stringsAsFactors = FALSE
)
head(encoded, 8)
nchar(encoded$combined[1])"""
        ),
        md(
            """# Заключение

Сырые годовые отчёты нельзя подавать в модель как есть: часть маркеров почти мономорфна, часть вызовов с низким `qc_score`, кодировка `AA`/`BB` плавает между партиями.

После порогов `qc_score ≥ 0.75`, нестабильности вызова и `call rate ≥ 0.85` остаётся выровненная матрица `0/1/2/5` и карта по каналам. Индекс однородности показывает, какие образцы выбиваются из типичного профиля панели — их стоит смотреть отдельно, а не смешивать с основным контуром оценки."""
        ),
    ]


def notebook_07() -> list[dict]:
    return [
        md("# 07. Кривые процесса и средовые эффекты"),
        code(
            """knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE,
  fig.align = "center",
  fig.width = 9,
  fig.height = 5
)"""
        ),
        md(
            """# О документе

Отчёт подбирает полином «измерение ~ возраст процесса», отсекает выбросы по остаткам и проверяет, какие средовые факторы стоит класть в смешанную модель как фиксированные эффекты.

| | |
|---|---|
| **Данные** | `data/process_curve.csv`, `data/environment_raw.csv`, `data/process_events.csv` |
| **Цель** | Выбрать степень полинома, очистить кривую, оценить связь `trait` с температурой, давлением, влажностью, заводом, годом и сезоном |
| **Результат** | Спецификация эффектов: какие ковариаты оставлять в модели качества, какие центрировать, какие отбросить |"""
        ),
        code(
            """library(dplyr)
library(stringr)
library(ggplot2)"""
        ),
        code(
            """curve <- read.csv("data/process_curve.csv")
env_raw <- read.csv("data/environment_raw.csv")
events <- read.csv("data/process_events.csv")
curve$factory <- as.factor(curve$factory)
curve$mark <- as.factor(curve$mark)
curve$type <- as.factor(curve$type)"""
        ),
        md(
            """# 1. Кривая процесса

Зависимость измеренной массы партии `mass` от возраста процесса `age`. Сначала смотрим сырой разброс по заводам и маркам."""
        ),
        code("summary(curve)"),
        code(
            """ggplot(curve, aes(x = age, y = mass, color = factory)) +
  geom_point(alpha = 0.35, size = 1.4) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "mass от age по заводам", x = "age", y = "mass", color = "Завод") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))"""
        ),
        md(
            """# 2. Выбор степени полинома

10-fold CV для степеней `1…5`. На каждом фолде модель учится на train, MSE считается на test. Берём степень с наименьшим средним MSE."""
        ),
        code(
            """set.seed(42)
K <- 10
max_degree <- 5
shuffled <- curve[sample(nrow(curve)), ]
folds <- cut(seq_len(nrow(shuffled)), breaks = K, labels = FALSE)
mse <- matrix(NA_real_, nrow = K, ncol = max_degree)

for (i in seq_len(K)) {
  test_idx <- which(folds == i)
  test_data <- shuffled[test_idx, ]
  train_data <- shuffled[-test_idx, ]
  for (j in seq_len(max_degree)) {
    fit <- lm(mass ~ poly(age, j, raw = TRUE), data = train_data)
    pred <- predict(fit, newdata = test_data)
    mse[i, j] <- mean((pred - test_data$mass)^2)
  }
}

cv_mse <- data.frame(degree = seq_len(max_degree), mse = colMeans(mse))
best_degree <- cv_mse$degree[which.min(cv_mse$mse)]
cv_mse
best_degree"""
        ),
        code(
            """ggplot(cv_mse, aes(x = degree, y = mse)) +
  geom_line(color = "#66C2A5", linewidth = 1) +
  geom_point(size = 3, color = "#66C2A5") +
  labs(title = "CV MSE по степени полинома", x = "Степень", y = "MSE") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))"""
        ),
        md(
            """Полином степени 2 обычно достаточен: кривая процесса вогнутая, старшие степени на CV не выигрывают и только ловят выбросы."""
        ),
        code(
            """best <- lm(mass ~ poly(age, 2, raw = TRUE), data = curve)
summary(best)$coefficients
curve$fitted <- predict(best)
curve$resid <- residuals(best)"""
        ),
        code(
            """ggplot(curve, aes(x = age, y = mass)) +
  geom_point(aes(color = factory), alpha = 0.3, size = 1.3) +
  geom_line(aes(y = fitted), color = "grey15", linewidth = 1.1) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Полином 2-й степени", x = "age", y = "mass", color = "Завод") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))"""
        ),
        code(
            """ggplot(curve, aes(x = age, y = mass)) +
  geom_point(aes(color = mark), alpha = 0.3, size = 1.3) +
  geom_line(aes(y = fitted), color = "grey15", linewidth = 1.1) +
  labs(title = "Та же кривая, цвет — марка", x = "age", y = "mass", color = "Марка") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))"""
        ),
        md(
            """# 3. Фильтр выбросов по остаткам

Точки с `|resid| > 2.5 · sd(resid)` считаем сбоем измерения, а не формой кривой. Их убираем до смешанной модели."""
        ),
        code(
            """resid_cut <- 2.5 * sd(curve$resid)
curve_clean <- curve %>% filter(abs(resid) <= resid_cut)
c(
  n_raw = nrow(curve),
  n_clean = nrow(curve_clean),
  n_removed = nrow(curve) - nrow(curve_clean),
  resid_sd = sd(curve$resid)
)"""
        ),
        code(
            """ggplot(curve, aes(x = fitted, y = resid, color = abs(resid) > resid_cut)) +
  geom_point(alpha = 0.4, size = 1.4) +
  geom_hline(yintercept = c(-resid_cut, resid_cut), linetype = "dashed") +
  scale_color_manual(values = c("grey50", "firebrick"), guide = "none") +
  labs(title = "Остатки полинома", x = "fitted", y = "resid") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))"""
        ),
        md(
            """# 4. Разбор сырой среды

Файл `environment_raw.csv` имитирует выгрузку метеостанции: дата в `дд.мм.гггг чч:мм`, видимость текстом (`10.0 и более`), направление ветра фразой. Приводим к числам и суточным средним по площадке."""
        ),
        code(
            """env <- env_raw %>%
  mutate(
    datetime = as.POSIXct(datetime, format = "%d.%m.%Y %H:%M"),
    date = as.Date(datetime),
    hour = as.numeric(format(datetime, "%H")),
    VV = case_when(
      VV == "10.0 и более" ~ "10",
      VV == "менее 0.1" ~ "0.1",
      VV == "менее 0.05" ~ "0.05",
      str_detect(VV, fixed("<")) ~ NA_character_,
      TRUE ~ as.character(VV)
    ),
    DD = case_when(
      DD == "Ветер, дующий с востока" ~ "В",
      DD == "Ветер, дующий с северо-востока" ~ "СВ",
      DD == "Ветер, дующий с юго-востока" ~ "ЮВ",
      DD == "Ветер, дующий с запада" ~ "З",
      DD == "Ветер, дующий с северо-запада" ~ "СЗ",
      DD == "Ветер, дующий с юго-запада" ~ "ЮЗ",
      DD == "Ветер, дующий с севера" ~ "С",
      DD == "Ветер, дующий с юга" ~ "Ю",
      DD == "Переменное направление" ~ "Переменное",
      DD == "Штиль, безветрие" ~ "Штиль",
      DD == "" ~ NA_character_,
      TRUE ~ DD
    ),
    Ff = as.numeric(Ff),
    VV = as.numeric(VV),
    Td = as.numeric(Td)
  ) %>%
  group_by(place, date) %>%
  summarise(
    temp = mean(temp, na.rm = TRUE),
    pressure = mean(P, na.rm = TRUE),
    humidity = mean(U, na.rm = TRUE),
    wind = first(DD),
    .groups = "drop"
  )

summary(env)"""
        ),
        code(
            """ggplot(env, aes(x = date, y = temp, color = place)) +
  geom_line(alpha = 0.8) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Температура по площадкам", x = "Дата", y = "temp", color = "Площадка") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))"""
        ),
        md(
            """# 5. Эффекты для смешанной модели

Берём события с уже сопоставленной средой. Пропуски выкидываем. Температуру, давление и влажность центрируем: нулевой уровень должен быть рабочим, а не «0 °C / 0 гПа / 0 %»."""
        ),
        code(
            """num_variables <- c("temp", "pressure", "humidity")
cat_variables <- c("event_year", "event_season", "factory")
base_values <- c("2023", "1", "Завод_А")
center_at <- c(temp = 10, pressure = 1013, humidity = 60)

pipe <- events %>%
  filter(complete.cases(.)) %>%
  mutate(across(all_of(cat_variables), as.factor))

for (v in num_variables) {
  pipe[[v]] <- pipe[[v]] - center_at[[v]]
}

summary(pipe)"""
        ),
        code(
            """pairs(
  pipe[, c(num_variables, "trait")],
  main = "trait и центрированная среда"
)"""
        ),
        md(
            """## 5.1. Связь `trait` с числовыми ковариатами

Шапиро–Уилк на выборке до 2000 наблюдений. Если хотя бы одна переменная не нормальна — Кендалл, иначе Пирсон."""
        ),
        code(
            """correlation_coefficient <- function(dataset, var_1, var_2) {
  x <- dataset[[var_1]]
  y <- dataset[[var_2]]
  n <- min(length(x), 2000)
  idx <- seq_len(n)
  p1 <- shapiro.test(x[idx])$p.value
  p2 <- shapiro.test(y[idx])$p.value
  if (p1 < 0.05 || p2 < 0.05) {
    ct <- cor.test(x, y, method = "kendall")
    return(c(method = "kendall", estimate = unname(ct$estimate), p_value = ct$p.value))
  }
  ct <- cor.test(x, y, method = "pearson")
  c(method = "pearson", estimate = unname(ct$estimate), p_value = ct$p.value)
}

corr_tbl <- as.data.frame(t(sapply(num_variables, function(v) {
  correlation_coefficient(pipe, v, "trait")
})))
corr_tbl$variable <- num_variables
corr_tbl <- corr_tbl[, c("variable", "method", "estimate", "p_value")]
corr_tbl"""
        ),
        md(
            """## 5.2. Связь `trait` с категориальными эффектами

При отклонении от нормальности — Краскел–Уоллис, иначе однофакторный ANOVA."""
        ),
        code(
            """category_dependency <- function(dataset, y_name, x_name) {
  y <- dataset[[y_name]]
  x <- dataset[[x_name]]
  n <- min(length(y), 2000)
  p_norm <- shapiro.test(y[seq_len(n)])$p.value
  if (p_norm < 0.05) {
    kt <- kruskal.test(y ~ x)
    return(c(test = "kruskal", statistic = unname(kt$statistic), p_value = kt$p.value))
  }
  av <- summary.aov(aov(y ~ x))[[1]]
  c(test = "anova", statistic = av[["F value"]][1], p_value = av[["Pr(>F)"]][1])
}

cat_tbl <- as.data.frame(t(sapply(cat_variables, function(v) {
  category_dependency(pipe, "trait", v)
})))
cat_tbl$variable <- cat_variables
cat_tbl <- cat_tbl[, c("variable", "test", "statistic", "p_value")]
cat_tbl"""
        ),
        md("Проверка, не является ли погода просто прокси сезона или года:"),
        code(
            """env_season <- as.data.frame(t(sapply(num_variables, function(v) {
  category_dependency(pipe, v, "event_season")
})))
env_season$variable <- num_variables
env_season"""
        ),
        md(
            """## 5.3. Линейные вклады

Коэффициенты после центрирования читаются как сдвиг `trait` при отклонении среды от рабочего уровня. Для категорий референс: год `2023`, сезон `1`, завод `Завод_А`."""
        ),
        code(
            """num_coef <- as.data.frame(t(sapply(num_variables, function(v) {
  fit <- lm(pipe$trait ~ pipe[[v]])
  c(intercept = unname(coef(fit)[1]), slope = unname(coef(fit)[2]))
})))
num_coef$variable <- num_variables
num_coef"""
        ),
        code(
            """cat_model_analysis <- function(effect, base_value) {
  x <- pipe[[effect]]
  x <- relevel(x, ref = base_value)
  fit <- lm(pipe$trait ~ x)
  df <- data.frame(term = names(coef(fit)), coef = as.numeric(coef(fit)))
  df$effect <- effect
  df
}

cat_coef <- do.call(
  rbind,
  mapply(cat_model_analysis, cat_variables, base_values, SIMPLIFY = FALSE)
)
cat_coef"""
        ),
        md(
            """# Заключение

Кривая `mass ~ age` хорошо описывается полиномом 2-й степени: CV не требует старших членов. Выбросы по остаткам лучше снимать до модели, иначе фиксированные эффекты завода и марки поплывут.

Среду нельзя подставлять «как в выгрузке»: видимость и ветер — текст, температура и влажность имеют рабочий ноль не в нуле шкалы. После очистки и центрирования в смешанную модель имеет смысл брать завод, сезон и температуру; давление и влажность проверять на коллинеарность с сезоном и не дублировать, если они только повторяют календарь."""
        ),
    ]


if __name__ == "__main__":
    dump(ROOT / "06_marker_panel.ipynb", notebook_06())
    dump(ROOT / "07_process_environment.ipynb", notebook_07())
