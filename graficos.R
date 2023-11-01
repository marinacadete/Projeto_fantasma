library(readr)
library(dplyr)
library(ggplot2)
library(stringr)


devolução <- read_csv("devolucao.csv")
devolucao_atualizada <- read.csv("devolucao_atualizado.csv")
vendas <- read_delim("vendas.csv", delim = ";",
                     escape_double = FALSE, col_types = cols(`Data Venda` = col_date(format = "%m/%d/%Y"),
                                                             `User ID` = col_character(), `Product ID` = col_character()),
                     trim_ws = TRUE)

# Organizando os dados
dados <- vendas %>%
  distinct(`Unique ID`, .keep_all = T) %>% # Retirar repetições
  mutate(`Motivo devolução` = tidyr::replace_na(`Motivo devolução`, "Não devolução"),
         Mes = format(`Data Venda`, "%m")) # Substituindo NA's e Criando coluna com mes

# Padrão ESTAT
cores_estat <- c("#A11D21","#003366","#CC9900","#663333","#FF6600","#CC9966",
                 "#999966","#006606","#008091","#041835","#666666")

theme_estat <- function (...) {
  theme <- ggplot2::theme_bw () +
    ggplot2::theme (
      axis.title.y = ggplot2::element_text(colour="black",size=12),
      axis.title.x = ggplot2::element_text(colour="black",size=12),
      axis.text = ggplot2::element_text(colour="black", size=9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colou ="black"),
      legend.position = "top",
      ...
    )
  return (
    list(
      theme ,
      scale_fill_manual(values=cores_estat),
      scale_colour_manual(values=cores_estat)
    )
  )
}


# Análise 1 - Faturamento anual por categoria
venda_ano <- dados %>%
  select(Category, Mes, Price, `Motivo devolução`) %>%
  tidyr::drop_na(Price, Mes, Category) %>%
  group_by(Category, Mes) %>%
  summarise(Price = sum(Price), .groups = "keep")

categ_max <- venda_ano %>%
  group_by(Category) %>%
  slice_max(Price, n = 1)
categ_min <- venda_ano %>%
  group_by(Category) %>%
  slice_min(Price, n = 1)

min_max <- bind_rows(categ_max, categ_min)

# Gráfico
ggplot(venda_ano, aes(Mes, Price, group = Category, color = Category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_discrete(labels = month.abb) +
  scale_y_continuous(breaks = seq(200, 3700, by = 500)) +
  labs(x = "Mês", y = "Total Vendido") +
  theme_estat(legend.title = element_blank())

ggsave("gráficolinha.png", width = 158, height = 93, units = "mm")

# Mêses com maior faturamento por Categoria
xtable::xtable(min_max)

# Análise 2 - Variação do preço por marca
preco_marca <- dados %>%
  select(Price, Brand) %>%
  tidyr::drop_na(Price, Brand)

# Gráfico
ggplot(preco_marca, aes(Brand, Price)) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Marca", y = "Média de Preço") +
  theme_estat ()

ggsave("boxplot.png", width = 158, height = 93, units = "mm")

quadro_resumo <- preco_marca %>%
  group_by(Brand) %>% # caso mais de uma categoria
  summarize(Média = round(mean(Price),2),
              `Desvio Padrão` = round(sd(Price),2),
              `Variância` = round(var(Price),2),
              `Mínimo` = round(min(Price),2),
              `1º Quartil` = round(quantile(Price, probs = .25),2),
              Mediana = round(quantile(Price, probs = .5),2),
              `3º Quartil` = round(quantile(Price, probs = .75),2),
              `Máximo` = round(max(Price),2)) %>% t() %>% as.data.frame() %>%
  mutate(V1 = str_replace (V1,"\\.",","))

xtable::xtable(quadro_resumo)

rstatix::kruskal_test(preco_marca, Price ~ Brand)
rstatix::kruskal_effsize(preco_marca, Price ~ Brand)


# Análise 3 - Relação entre categorias (apenas marculino e feminino) e cor
categoria_cor <- dados %>%
  select(Category, Color) %>%
  tidyr::drop_na(Color) %>%
  filter(Category != "Moda Infantil") %>%
  group_by(Color, Category) %>%
  summarise(freq = n()) %>%
  mutate(perc = round(freq/sum(freq),4)*100)

porcentagens <- str_c(categoria_cor$perc, "%") %>% str_replace("\\.", ",")
legendas <- str_squish(str_c(categoria_cor$freq, " (", porcentagens, ")"))

# Gráfico
ggplot(categoria_cor, aes(forcats::fct_reorder(Color, freq, .desc = T),
                          y = freq, fill = Category, label = legendas)) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, hjust = 0.5, size = 2) +
  labs(x = "Cor", y = "Frequência") +
  scale_y_continuous(breaks = seq(0, 70, by = 5)) +
  theme_estat(legend.title = element_blank()) +
  scale_fill_manual(values = c("#003366","#A11D21"))

ggsave("coluna.png", width = 158, height = 93, units = "mm")

