require(readr)
require(dplyr)
require(feasts)
require(tsibble)
require(ggplot2)
require(gridExtra)
require(ggrepel)


# ---------------------------- Загрузка данных ---------------------------

dat <- read_csv("data/cypto_coins.csv") %>% 
  as_tsibble(., key = coin, index = ds) %>% 
  mutate(y = log(y))

glimpse(dat)


# ------------------------ Функция features() ----------------------------

# Неправильный способ подать дополнительный аргумент на функцию,
# которая вычисляет тот или иной признак (здесь - среднее значение) - 
# при наличии пропущенных наблюдений (NA) эта команда не
# выполнила бы расчет среднего:

## features(dat, y, features = list(mean), na.rm = TRUE)

# Правильный способ, с использованием анонимной функции:
features(dat, y, features = list(~ mean(., na.rm = TRUE)))


# Расчет признака с использованием пользовательской функции:
my_func <- function(x) {
  m <- mean(x, na.rm = TRUE)
  v <- var(x, na.rm = TRUE)
  return(c(avg = m, var = v))
}

features(dat, y, features = list(~my_func(.)))


# Использование встроенных функций для расчета признаков:
dat %>% features(., y, list(coef_hurst, feat_spectral))

dat %>% 
  features(., y, list(feat_stl)) %>% 
  glimpse()


# Одновременный расчет всех признаков, реализованных в пакете feasts:
dat %>% 
  features(., y, feature_set(pkgs = "feasts")) %>% 
  glimpse()

# Регистрация пользовательской функции:
register_feature(my_func, tags = c("avg_and_var"))

dat %>% 
  features(., y, feature_set(tags = "avg_and_var")) %>% 
  glimpse()


# -------------- Примеры использования признаков --------------------

# Выявление рядов с наибольшей и с наименьшей выраженностью тренда:
dat_trend <- dat %>% 
  features(., y, feature_set(tags = "stl"))

min_trend <- dat_trend %>% 
  filter(trend_strength == min(trend_strength)) %>% 
  select(coin) %>% 
  left_join(., dat, by = "coin") %>% 
  ggplot(., aes(ds, y)) + 
  geom_line() + facet_grid(~coin) +
  theme_bw() +
  ggtitle(paste0("Минимальный тренд"))

max_trend <- dat_trend %>% 
  filter(trend_strength == max(trend_strength)) %>% 
  select(coin) %>% 
  left_join(., dat, by = "coin") %>% 
  ggplot(., aes(ds, y)) + 
  geom_line() + facet_grid(~coin) +
  theme_bw() +
  ggtitle(paste0("Максимальный тренд"))

grid.arrange(min_trend, max_trend, ncol = 2)


# Сравнение рядов по двум признакам:
dat_trend %>% 
  ggplot(., aes(trend_strength, spikiness, label = coin)) +
  geom_point() + 
  geom_text_repel(force = 10) +
  theme_bw()

dat %>% 
  filter(coin == "tron") %>% 
  ggplot(., aes(ds, y)) +
  geom_line() +
  theme_bw()


# Применение метода главных компонент:
all_features <- dat %>%
  features(., y, feature_set(pkgs = "feasts"))

# Обратите внимание: признаки, в названии которых есть "_pvalue"
# или "diffs" удалены в связи тем, что их дисперсия равна нулю:
pc <- all_features %>% 
  select(-coin, -contains("_pvalue"), 
         -contains("diffs")) %>%
  prcomp(scale = TRUE)

summary(pc)


# Визуализация распределения рядов в системе координат,
# образованной первыми двумя главными компонентами:
pc <- all_features %>% 
  select(coin) %>% 
  bind_cols(., as_tibble(pc$x))

pc %>% 
  ggplot(., aes(PC1, PC2, label = coin)) +
  geom_point() +
  geom_text_repel(force = 10) +
  theme_bw()


# Кластеризация временных рядов:
clust <- pc %>% 
  select(PC1:PC9) %>%
  kmeans(., centers = 4, nstart = 10)


# Визуализация временных рядов с учетом их групповой 
# принадлежности:
pc %>% 
  mutate(cluster = paste0("кластер ", clust$cluster)) %>% 
  select(coin, cluster) %>% 
  inner_join(., dat, by = "coin") %>% 
  ggplot(., aes(ds, y, group = coin)) +
  geom_line() +
  facet_wrap(~cluster, scales = "free_y") +
  theme_bw()
