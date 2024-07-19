setwd('d:\\Giovanni\\Downloads\\texto - salários')

library(tidyverse)
library(readxl)
library(magrittr)

## 1º exercício: ranking salário ----

salário <- read_excel('salário.xlsx') %>%
  filter(classif2.label == 'Currency: 2021 PPP $',
         classif1.label == 'Economic activity (Aggregate): Total',
         sex.label == 'Sex: Total') %>%
  select(ref_area.label, time, obs_value)

salário$time <- as.numeric(salário$time)

salário %<>%
  group_by(ref_area.label) %>%
  filter(time == max(time)) %>%
  ungroup() %>%
  filter(time > 2021,
         ref_area.label != 'Belarus') %>%
  mutate(ref_area.label = case_when(
    ref_area.label == 'Occupied Palestinian Territory' ~ 'Palestine',
    ref_area.label == 'United Kingdom of Great Britain and Northern Ireland' ~ 'United Kingdom',
    ref_area.label == 'Bolivia (Plurinational State of)' ~ 'Bolivia',
    ref_area.label == 'Republic of Moldova' ~ 'Moldova',
    ref_area.label == "Lao People's Democratic Republic" ~ 'Laos',
    ref_area.label == 'Republic of Korea' ~ 'South Korea',
    ref_area.label == 'United States of America' ~ 'United States',
    TRUE ~ ref_area.label))

salário %<>%arrange(desc(obs_value))

## 2º exercício: regressão ----

# Produtividade
produtividade <- read_excel('produtividade.xlsx', sheet = 'Data') %>%
  select(country, year, rgdpe, emp) %>%
  group_by(country) %>%
  filter(year == max(year), !is.na(emp)) %>%
  ungroup() %>%
  mutate(pme = rgdpe/emp) %>%
  arrange(desc(pme)) %>%
  select(country, pme)

# Salário
salário <- read_excel('salário.xlsx') %>%
  filter(classif2.label == 'Currency: 2021 PPP $',
         classif1.label == 'Economic activity (Aggregate): Total',
         sex.label == 'Sex: Total') %>%
  select(ref_area.label, time, obs_value)

salário$time <- as.numeric(salário$time)

salário %<>%
  group_by(ref_area.label) %>%
  ungroup() %>%
  filter(time == 2019, # Ano que possui dado de produtividade
         ref_area.label != 'Belarus') %>%
  mutate(ref_area.label = case_when(
    ref_area.label == 'Occupied Palestinian Territory' ~ 'State of Palestine',
    ref_area.label == 'United Kingdom of Great Britain and Northern Ireland' ~ 'United Kingdom',
    ref_area.label == "Lao People's Democratic Republic" ~ 'Laos',
    ref_area.label == 'United States of America' ~ 'United States',
    ref_area.label == 'Türkiye' ~ 'Turkey',
    TRUE ~ ref_area.label)) %>%
  arrange(desc(obs_value)) %>%
  select(-time) %>%
  rename(salario = obs_value)

# Participação
participação <- read_excel('participação.xlsx') %>%
  select(ref_area.label, time, obs_value)

participação$time <- as.numeric(participação$time)

participação %<>%
  group_by(ref_area.label) %>%
  filter(time == 2019) %>%
  ungroup() %>%
  mutate(ref_area.label = case_when(
    ref_area.label == 'Occupied Palestinian Territory' ~ 'State of Palestine',
    ref_area.label == 'United Kingdom of Great Britain and Northern Ireland' ~ 'United Kingdom',
    ref_area.label == "Lao People's Democratic Republic" ~ 'Laos',
    ref_area.label == 'United States of America' ~ 'United States',
    ref_area.label == 'Türkiye' ~ 'Turkey',
    TRUE ~ ref_area.label)) %>%
  select(-time) %>%
  rename(participacao = obs_value)

# Juntando
base <- left_join(salário, participação, by = c('ref_area.label'))
base <- left_join(base, produtividade, by = c('ref_area.label' = 'country')) %>%
  filter(!is.na(pme), !is.na(participacao))

# Regressão
reg <- lm(salario ~ participacao + pme, data = base)
summary(reg)

## 3º exercício: comparando média e correlação ----

mean(base$participacao)
sd(base$participacao)

ocde <- read_excel('ocde.xlsx') %>%
  mutate(country = case_when(
    country == 'Slovak Republic' ~ 'Slovakia',
    country == 'Korea' ~ 'Republic of Korea',
    country == 'Türkiye' ~ 'Turkey',
    TRUE ~ country))

teste <- left_join(ocde, base, by = c('country' = 'ref_area.label')) %>%
  filter(!is.na(salario))

mean(teste$participacao)
sd(teste$participacao)

ggplot(base, aes(x = 12*salario, y = pme)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  geom_point(data = base %>% filter(ref_area.label == 'Brazil'), 
             aes(x = 12*salario, y = pme), 
             color = "red", size = 3) +
  labs(x = "Salário médio anual",
       y = "Produtividade média do trabalho") +
  annotate("text", x = 24300, y = 10000, label = "Brasil", color = "black",
           size = 5, angle = 0, vjust = 1) +
  annotate("segment", x = 22000, xend = 12*1260, y = 9000, yend = 31000, 
           arrow = arrow(length = unit(0.3, "cm")), color = "black") +
  theme_minimal() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

ggsave(plot = last_plot(), file = "correlação.png", width = 10, height = 5, bg = 'white')

## 4º exercício: gráfico de países ----

produtividade <- read_excel('produtividade.xlsx', sheet = 'Data') %>%
  select(country, rgdpe, emp, year) %>%
  filter(country %in% c('Republic of Korea', 'Taiwan', 'Botswana', 'Brazil', 'Malaysia')) %>%
  mutate(pme = rgdpe/emp) %>%
  mutate(country = case_when(country == 'Republic of Korea' ~ 'Coreia do Sul',
                             country == 'Brazil' ~ 'Brasil',
                             country == 'Malaysia' ~ 'Malásia',
                             TRUE ~ country))

ggplot(produtividade, aes(x = year, y = pme, color = country, group = country)) +
  geom_line(size = .7) +
  labs(y = "Produtividade média do trabalho",
       color = "País") +
  scale_x_continuous(limits = c(1950, 2020), breaks = seq(1950, 2020, by = 10)) +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 14))

ggsave(plot = last_plot(), file = "países.png", width = 10, height = 5, bg = 'white')
