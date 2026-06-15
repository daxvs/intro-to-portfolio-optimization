library(fPortfolio)
library(tidyverse)
library(zoo)

# DADOS ----
# Dados com os preços de fechamento de 114 papéis
data <- read_delim("data/Cotacoes18a25.csv") |>
  mutate(
    date = dmy(Data)
  ) |>
  rename(
    CDI = `CDI 252 dias`
  ) |> 
  select(-Data) |> 
  relocate(date)

data_longer <- 
  data |>
  dplyr::select(-c(CDI, IBOV, IBRA)) |> 
  pivot_longer(
    cols = -date,
    names_to = "codigo",
    values_to = "value"
  )

# Retornos Diários
retornos_dia <- data_longer |> 
  group_by(codigo) |> 
  mutate(
    retornos = (value/lag(value))-1,
    l_retornos = log(value/lag(value))
  ) |> 
  ungroup() |> 
  drop_na()

# Otimização ----
source("src/optim_function.R")

dados.f <- retornos_dia |> # retornos das ações
  pivot_wider(
    id_cols = date,
    names_from = codigo,
    values_from = l_retornos # utilizaremos apenas os log-retornos
  )

# Definir o período de teste: Janeiro 2020 a Dezembro 2024 (5 anos)
datas_rebalanceamento <- seq(
  from = as.Date("2020-01-01"), 
  to   = as.Date("2024-12-01"), 
  by   = "1 month"
)

# Rodar o loop: isso vai gerar um data frame onde cada linha é um mês com os pesos para aquele mês específico
portfolio <- map(
  .x = datas_rebalanceamento,
  .f = optim,
  dados_completo = dados.f
) |> 
  list_rbind()

retornos_dia <- retornos_dia |> # retornos das ações
  pivot_wider(
    id_cols = date,
    names_from = codigo,
    values_from = retornos # e não mais os log-retornos (l_retornos)
  ) |> 
  pivot_longer(
    cols = -date,
    names_to = "ativo",
    values_to = "retorno_diario"
  ) |> 
  mutate(
    date = floor_date(date, "month") # para obter compatibilidade com a data de referência do tibble dos pesos, pois todos estão como o dia 1 do mês
  )

# Dados dos pesos obtidos a partir da otimização para cada mês
pesos_mes <- portfolio |> 
  pivot_longer(
    cols = -date_ref,
    names_to = "ativo",
    values_to = "peso"
  ) |> 
  rename(date = date_ref)

# ------------------------------- #

# Portfólio
retorno_portfolio <- retornos_dia |> 
  filter(date >= min(datas_rebalanceamento)) |> 
  group_by(date, ativo) |> 
  
  # Acumulo o retorno diário de cada ativo específico naquele mês. Outra forma poderia ser simplesmente a variação entre o preço do ativo no primeiro e último dia do mês (matematicamente equivalente a acumular a variação de preços no mês).
  summarise(
    retorno_mensal_ativo = prod(1 + retorno_diario) - 1,
    .groups = "drop"
  ) |> 
  inner_join(pesos_mes, by = c("date", "ativo")) |> # os pesos de um ativo pra aquele mês são repetidos em cada retorno diário daquele mês 
  
  group_by(date) |> 
  summarise(
    retorno_mensal = sum(peso*retorno_mensal_ativo), # multiplico o peso pela variação acumualda no mês daquele ativo específico
    .groups = "drop"  
  ) |> 
  mutate(
    retorno_acumulado = cumprod(1 + retorno_mensal)
  )

# ----------------------------- #

# CDI
retorno_cdi <- data |>
  filter(between(date, as.Date("2020-01-01"), as.Date("2024-12-31"))) |>
  select(date, CDI) |> 
  drop_na(CDI) |> 
  mutate(
    CDI = (((1+(CDI/100))^(1/252))-1) # taxa a.a. para taxa a.d. (Over: mês com 21 dias e ano com 252)
  ) |> 
  group_by(date = floor_date(date, "month")) |> 
  summarise(
    ret_mensal_cdi = last(cumprod(1 + CDI))-1
  ) |> 
  mutate(
    ret_acum_cdi = cumprod(1+ret_mensal_cdi)
  )

# ------------------------------- #

# IBrA
retorno_ibra <- data |> 
  select(date, IBRA) |> 
  drop_na(IBRA) |> 
  mutate(
    retorno_ibra = (IBRA/lag(IBRA))-1,
    date = floor_date(date, "month")
  ) |> 
  filter(between(date, as.Date("2020-01-01"), as.Date("2024-12-31"))) |> 
  group_by(date) |> 
  summarise(
    retorno_ibra = prod(1+retorno_ibra)-1
  ) |> 
  ungroup() |> 
  mutate(
    ret_acum_ibra = cumprod(1 + retorno_ibra)
  )

# ------------------------------ #

# Comparação ----
retornos <- list(
  retorno_ibra |> select(date, ret_acum_ibra),
  retorno_portfolio |> select(date, retorno_acumulado),
  retorno_cdi |> select(date, ret_acum_cdi)
)

retornos_comparacao <- reduce(
  .x = retornos,
  .f = inner_join,
  by = "date"
) |> 
  rename(
    CARTEIRA = retorno_acumulado,
    CDI = ret_acum_cdi,
    IBRA = ret_acum_ibra
  )

retornos_comparacao |> 
  pivot_longer(
    cols = -c(date),
    names_to = "retornos",
    values_to = "value"
  ) |> 
  mutate(
    date = as.yearmon(date) # necessário para poder utilizar scale_x_yearmon() no ggplot
  )	|>  
  ggplot(aes(x = date)) +
  geom_line(aes(y = value, color = retornos), linewidth = 1) +
  labs(
    x = "",
    y = "Retorno Acumulado (Índice)",
    color = NULL
  ) +
  theme_bw() +
  scale_x_yearmon(
    format = '%Y/%b', 
    n = 10) +
  scale_y_continuous(
    n.breaks = 10
  ) +
  theme(
    legend.position = "bottom"
  )

# ------------------------------ #

# Medidads de Performance ----
library(gt)
ret_med <- mean(retorno_portfolio$retorno_mensal)
std     <- sd(retorno_portfolio$retorno_mensal)
cdi_med <- mean(retorno_cdi$ret_mensal_cdi)

descr <- tibble(
  Metrica = c("Retorno Médio", "Desvio padrão", "Razão de Sharpe"),
  Mensal  = c(
    ret_med,
    std,
    (ret_med - cdi_med) / std
  ),
  Anual   = c(
    12 * ret_med,
    sqrt(12) * std,
    sqrt(12) * ((ret_med - cdi_med) / std)
  )
)

gt_tbl <- gt(descr) |> 
  tab_header(
    title = "Medidas de Perfomance",
    subtitle = "Análise do retorno relativo ao risco, retorno médio e o risco do portfólio"
  ) |> 
  cols_label(
    Metrica = ""
  ) |> 
  fmt_percent(
    rows = Metrica != "Razão de Sharpe",
    decimals = 2
  ) |> 
  fmt_number(
    columns = c(Mensal, Anual),
    rows = Metrica == "Razão de Sharpe",
    decimals = 2
  ) |> 
  tab_footnote(
    footnote = md("Razão de Sharpe: qual o retorno entregue acima da taxa livre de risco (CDI) <br> para cada 1% de risco desse portfólio."),
    locations = cells_body(columns = Metrica, rows = 3)
  )
gt_tbl