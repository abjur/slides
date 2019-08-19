library(tidyverse)
library(formattable)

d_emp <- readr::read_rds("d_parsed_emp.rds") %>% 
  filter(str_detect(distribuicao, "VARA EMPRESARIAL")) %>% 
  mutate(dt_dist = lubridate::dmy(str_sub(distribuicao, 1, 10)),
         valor_da_acao = valor_da_acao %>%
           str_remove_all("R\\$") %>%
           str_replace_all("\\.", "") %>%
           str_replace_all(",", ".") %>%
           as.numeric())

fator <- 3.4

p_vol <- function(dados = d_emp) {
  
  da <- dados %>% 
    mutate(month = lubridate::floor_date(dt_dist, "month")) %>%
    count(month) %>% 
    mutate(hoje = month == "2019-08-01", 
           n = if_else(hoje, n * fator, as.numeric(n)))
  
  tb <- tibble(
    lab = c("2018", "2019"), 
    month = c(as.Date("2018-01-01") + lubridate::years(0:1)),
    n = rep(max(da$n), 2),
    hoje = 0
  )
  
  da %>% 
    ggplot(aes(x = month, y = n, alpha = -hoje)) +
    geom_vline(xintercept = c(as.Date("2018-01-01") + lubridate::years(0:2)),
               linetype = 1, colour = "gray70") +
    geom_line(size = 1.5, colour = "#007A74") +
    geom_point(size = 2, colour = "#004D49") +
    geom_text(aes(label = lab), nudge_x = 30, size = 5, data = tb) +
    scale_x_date(breaks = scales::date_breaks("1 month"),
                 labels = scales::date_format("%b")) +
    guides(alpha = FALSE) +
    theme_minimal(16) +
    geom_smooth(alpha = 0.1, linetype = 3, method = 'loess') +
    theme(panel.grid.minor.x = element_blank()) +
    labs(x = "Mês", y = "Quantidade de processos") +
    ggtitle("Distribuições mensais",
            "Varas empresariais da Comarca de São Paulo")
}

p_vol_ano <- function(dados = d_emp) {
  dados %>% 
    arrange(dt_dist) %>% 
    mutate(month = lubridate::floor_date(dt_dist, "month"),
           ano = lubridate::year(month),
           hoje = month == "2019-08-01",
           month = month - lubridate::years(ano - 2018)) %>%
    filter(ano > 2017) %>% 
    count(ano, hoje, month) %>% 
    mutate(n = if_else(hoje, n * fator, as.numeric(n))) %>% 
    ggplot(aes(x = month, y = n, alpha = -hoje, colour = factor(ano))) +
    geom_line(size = 1.5) +
    geom_point(size = 2) +
    scale_x_date(breaks = scales::date_breaks("1 month"),
                 labels = scales::date_format("%b")) +
    guides(alpha = FALSE) +
    scale_colour_viridis_d(begin = 0.2, end = .8) +
    theme_minimal(16) +
    labs(x = "Mês", y = "Quantidade de processos",
         colour = "Ano") +
    theme(legend.position = c(0.9, 0.2)) +
    ggtitle("Distribuições mensais",
            "Varas empresariais da Comarca de São Paulo")
}

dinheiro <- function(v){
  formattable::currency(v, symbol = "R$", big.mark = ".", decimal.mark = ",")
}

montar_tabela <- function(v = classe, dados = d_emp, n = 10) {
  dados %>% 
    mutate({{v}} := fct_infreq({{v}}) %>% 
             fct_infreq() %>% 
             fct_lump(n, other_level = "Outros")) %>% 
    group_by({{v}}) %>% 
    summarise(valor = median(valor_da_acao, na.rm = TRUE), n = n()) %>% 
    mutate(prop = percent(n / sum(n))) %>% 
    set_names(str_to_title) %>% 
    mutate(Valor = dinheiro(Valor)) %>% 
    formattable(list(
      area(col = Prop) ~ normalize_bar("pink", 0.2),
      area(col = N) ~ normalize_bar("gold", 0.2),
      area(col = Valor) ~ normalize_bar("lightgreen", 0.2))) 
}

# tb_classes <- montar_tabela(classe)
# tb_assuntos <- montar_tabela(assunto)
# 
# tb_assuntos <- montar_tabela(assunto, n = 100)

# p_vol()
# p_vol_ano()

p_vol_filtro <- function(...) {
  el <- list(...)
  da <- d_emp
  if (length(el) > 0) da <- filter(da, .data[[names(el)]] %in% el[[1]])
  p <- p_vol(da)
  if (length(el) > 0) p <- p + labs(
    caption = paste(str_to_title(names(el)), paste(el[[1]], collapse = "\n"), 
                    sep = ": ")
  )
  p
}


## tempos ----------------------------------------------------------------------
d_modelo <- d_emp %>% 
  unnest(movimentacoes) %>% 
  mutate(
    data = lubridate::dmy(data),
    teve_gratuidade = str_detect(paste(movimento, descricao), regex("gratui", ignore_case = TRUE)),
    nao_gratuidade = str_detect(movimento, regex("não concedida a assist|revogada a assist", ignore_case = TRUE)),
    arquivado = str_detect(movimento, regex("arquiv", ignore_case = TRUE)),
    sentenca = str_detect(movimento, regex("senten|acordo", ignore_case = TRUE))
  ) %>% 
  arrange(data) %>% 
  group_by(id_processo) %>% 
  summarise(dt_dist = first(dt_dist),
            dt_arqv = first(data[arquivado]),
            dt_sent = first(data[sentenca]),
            teve_gratuidade = any(teve_gratuidade),
            nao_gratuidade = any(nao_gratuidade)) %>% 
  mutate(morreu_arqv = !is.na(dt_arqv),
         morreu_sent = !is.na(dt_sent)) %>% 
  replace_na(list(dt_arqv = as.Date("2019-08-07"),
                  dt_sent = as.Date("2019-08-07"))) %>% 
  mutate(tempo_arqv = dt_arqv - dt_dist,
         tempo_sent = dt_sent - dt_dist) %>% 
  filter(tempo_arqv > 0, tempo_sent > 0)

d_modelo %>% 
  count(teve_gratuidade, nao_gratuidade) %>% 
  filter(teve_gratuidade) %>% 
  mutate(prop = n/sum(n))

# d_modelo %>% 
#   filter(!morreu_sent, lubridate::year(dt_dist) == 2018) %>% 
#   sample_n(1) %>% 
#   glimpse()

library(survival)
surv_arqv <- survfit(Surv(tempo_arqv, morreu_arqv) ~ 1, data = d_modelo)
surv_sent <- survfit(Surv(tempo_sent, morreu_sent) ~ 1, data = d_modelo)



plot(surv_arqv)
plot(surv_sent)


# library(patchwork)
# p_vol_filtro() +
#   (p_vol_filtro(assunto = tb_assuntos$assunto[1]) + ggtitle("", "") + labs(x='')) +
#   (p_vol_filtro(assunto = tb_assuntos$assunto[-1]) + ggtitle("", "") + labs(x='')) +
#   (p_vol_filtro(classe = tb_classes$classe[2]) + ggtitle("", "") + labs(x='')) +
#   plot_layout(ncol = 2)
# 
# assuntos <- tb_assuntos %>% 
#   as_tibble() %>% 
#   mutate(assunto = as.character(assunto)) %>% 
#   mutate(grupo = case_when(
#     str_detect(assunto, "Propriedade Intelectual|Desenho Industrial|Marca|Patente") ~ "Propriedade Intelectual",
#     str_detect(assunto, regex("Em comum / De fato|Anônima|Limitada|Conta de Participação|Transferência de cotas|Dissolução|Sócios|haveres", ignore_case = TRUE)) ~ "Direito Societário",
#     #str_detect(assunto, regex("Em comum / De fato|Anônima|Limitada", ignore_case = TRUE)) ~  "Espécies de Sociedades",
#     str_detect(assunto, regex("Transação|Rescisão do contrato e devolução do dinheiro|Rescisão / Resolução|Locação de Imóvel|Debêntures|Compra e Venda|Defeito, nulidade ou anulação", ignore_case = TRUE)) ~ "Contratos em geral",
#     str_detect(assunto, regex("Indeniz", ignore_case = TRUE)) ~ "Indenizações",
#     str_detect(assunto, regex("Tutela", ignore_case = TRUE)) ~ "Antecipação de Tutela",
#     TRUE ~ assunto
#   )) %>% 
#   filter(grupo == "Direito Societário") %>% 
#   with(assunto)
# 
# assuntos_not <- tb_assuntos %>% 
#   as_tibble() %>% 
#   mutate(assunto = as.character(assunto)) %>% 
#   mutate(grupo = case_when(
#     str_detect(assunto, "Propriedade Intelectual|Desenho Industrial|Marca|Patente") ~ "Propriedade Intelectual",
#     str_detect(assunto, regex("Em comum / De fato|Anônima|Limitada|Conta de Participação|Transferência de cotas|Dissolução|Sócios|haveres", ignore_case = TRUE)) ~ "Direito Societário",
#     #str_detect(assunto, regex("Em comum / De fato|Anônima|Limitada", ignore_case = TRUE)) ~  "Espécies de Sociedades",
#     str_detect(assunto, regex("Transação|Rescisão do contrato e devolução do dinheiro|Rescisão / Resolução|Locação de Imóvel|Debêntures|Compra e Venda|Defeito, nulidade ou anulação", ignore_case = TRUE)) ~ "Contratos em geral",
#     str_detect(assunto, regex("Indeniz", ignore_case = TRUE)) ~ "Indenizações",
#     str_detect(assunto, regex("Tutela", ignore_case = TRUE)) ~ "Antecipação de Tutela",
#     TRUE ~ assunto
#   )) %>% 
#   filter(grupo != "Direito Societário") %>% 
#   with(assunto)
# 
# 
# 
# 
# 
# dados <- d_emp
# da <- dados %>% 
#   mutate(month = lubridate::floor_date(dt_dist, "month")) %>%
#   mutate(grupo = case_when(
#     str_detect(assunto, "Propriedade Intelectual|Desenho Industrial|Marca|Patente") ~ "Propriedade Intelectual",
#     str_detect(assunto, regex("Em comum / De fato|Anônima|Limitada|Conta de Participação|Transferência de cotas|Dissolução|Sócios|haveres", ignore_case = TRUE)) ~ "Direito Societário",
#     #str_detect(assunto, regex("Em comum / De fato|Anônima|Limitada", ignore_case = TRUE)) ~  "Espécies de Sociedades",
#     str_detect(assunto, regex("Transação|Rescisão do contrato e devolução do dinheiro|Rescisão / Resolução|Locação de Imóvel|Debêntures|Compra e Venda|Defeito, nulidade ou anulação", ignore_case = TRUE)) ~ "Contratos em geral",
#     str_detect(assunto, regex("Indeniz", ignore_case = TRUE)) ~ "Indenizações",
#     str_detect(assunto, regex("Tutela", ignore_case = TRUE)) ~ "Antecipação de Tutela",
#     TRUE ~ assunto
#   )) %>% 
#   mutate(grupo = if_else(grupo == "Direito Societário", "Direito Societário", "Outros")) %>% 
#   count(grupo, month) %>% 
#   mutate(hoje = month == "2019-08-01", 
#          n = if_else(hoje, n * fator, as.numeric(n)))
# 
# tb <- tibble(
#   lab = c("2018", "2019"), 
#   month = c(as.Date("2018-01-01") + lubridate::years(0:1)),
#   n = rep(max(da$n), 2),
#   hoje = 0
# )
# 
# da %>% 
#   ggplot(aes(x = month, y = n, alpha = -hoje, colour = grupo)) +
#   geom_vline(xintercept = c(as.Date("2018-01-01") + lubridate::years(0:2)),
#              linetype = 1, colour = "gray70") +
#   geom_line(size = 1.5) +
#   geom_point(size = 2) +
#   # geom_text(aes(label = lab), nudge_x = 30, size = 5, data = tb, colour = "black") +
#   scale_x_date(breaks = scales::date_breaks("1 month"),
#                labels = scales::date_format("%b")) +
#   scale_colour_viridis_d(begin = 0.2, end = 0.8) +
#   guides(alpha = FALSE) +
#   theme_minimal() +
#   geom_smooth(alpha = 0.1, linetype = 3, method = 'loess') +
#   # facet_wrap(~grupo, scales = "free_y") +
#   theme(panel.grid.minor.x = element_blank()) +
#   labs(x = "Mês", y = "Quantidade de processos") +
#   ggtitle("Distribuições mensais",
#           "Varas empresariais da Comarca de São Paulo")
# 
# 
# 
# 
# 
# (p_vol_filtro(assunto = assuntos) + labs(x='', caption = "")) + 
#   (p_vol_filtro(assunto = assuntos_not) + labs(x='', caption = "")) +
#   plot_layout(ncol = 1)
