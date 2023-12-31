library(readr)
library(dplyr)
library(ggplot2)
library(stringr)


devolucao_atualizada <- read.csv("devolucao_atualizado.csv")
vendas <- read_delim("vendas_traduzido.csv", delim = ";", escape_double = FALSE,
                     col_types = cols(`Data Venda` = col_date(format = "%m/%d/%Y"),
                                      `User ID` = col_character(), `Product ID` = col_character(),
                                      Price = col_number(), Rating = col_number(), `Motivo devolução` = col_skip()),
                     trim_ws = TRUE)

# Organizando os dados
dados <- vendas %>%
  distinct(`Unique ID`, .keep_all = T) %>% # Retirar repetições
  rename(Unique.ID = "Unique ID") %>%
  mutate(Motivo.devolução = devolucao_atualizada$Motivo.devolução[match(Unique.ID, devolucao_atualizada$Unique.ID)]) %>%
  mutate(Mes = lubridate::month(`Data Venda`, label = T))  # Criando coluna com o mes

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


### Análise 1 - Faturamento anual por categoria
faturamento_anual <- dados %>%
  select(Category, Mes, Price) %>%
  tidyr::drop_na(Price, Mes, Category) %>%
  group_by(Category, Mes) %>%
  summarise(Price = sum(Price), .groups = "keep")

# Gráfico
ggplot(faturamento_anual, aes(Mes, Price, group = Category, color = Category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(breaks = seq(200, 3700, by = 500)) +
  labs(x = "Mês", y = "Total Vendido") +
  theme_estat(legend.title = element_blank())

ggsave("gráficolinha.png", width = 158, height = 93, units = "mm")

# Tabela Total
xtable::xtable(faturamento_anual %>%
                 group_by(Category) %>%
                 summarise(Price = sum(Price)) %>%
                 mutate(Porcentagem =  round(Price/sum(Price),4)*100))

### Análise 2 - Variação do preço por marca
preco_marca <- dados %>%
  select(Price, Brand) %>%
  tidyr::drop_na(Price, Brand)

# Gráfico
ggplot(preco_marca, aes(Brand, Price)) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Marca", y = "Preço dos Produtos") +
  theme_estat ()

ggsave("boxplot.png", width = 158, height = 93, units = "mm")

# Quadro medidas resumo
quadro_resumo <- preco_marca %>%
  group_by(Brand) %>% # caso mais de uma categoria
  summarize(Média = round(mean(Price),2),
              `Desvio Padrão` = round(sd(Price),2),
              `Mínimo` = round(min(Price),2),
              `1º Quartil` = round(quantile(Price, probs = .25),2),
              Mediana = round(quantile(Price, probs = .5),2),
              `3º Quartil` = round(quantile(Price, probs = .75),2),
              `Máximo` = round(max(Price),2)) %>% t() %>% as.data.frame() %>%
  mutate(V1 = str_replace(V1,"\\.",",")) %>%
  mutate(V2 = str_replace(V2,"\\.",",")) %>%
  mutate(V3 = str_replace(V3,"\\.",",")) %>%
  mutate(V4 = str_replace(V4,"\\.",",")) %>%
  mutate(V5 = str_replace(V5,"\\.",","))

xtable::xtable(quadro_resumo)

# Teste de Kruskal-Wallis
rstatix::kruskal_test(preco_marca, Price ~ Brand)
rstatix::kruskal_effsize(preco_marca, Price ~ Brand)


### Análise 3 - Relação entre categorias (apenas marculino e feminino) e cor
categoria_cor <- dados %>%
  select(Category, Color) %>%
  filter(Category != "Moda Infantil") %>%
  tidyr::drop_na() %>%
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
  theme_estat(legend.title = element_blank())

ggsave("coluna.png", width = 158, height = 93, units = "mm")

# Teste
categoria_cor2 <- dados %>%
  select(Category, Color) %>%
  filter(Category != "Moda Infantil") %>%
  tidyr::drop_na()

categoria <- factor(categoria_cor2$Category)
cor <- factor(categoria_cor2$Color)
tabela <- table(categoria, cor)
chisq.test(tabela)

### Análise 4 - Relação entre preço e avaliação
preco_avaliacao <- dados %>%
  select(Price, Rating) %>%
  tidyr::drop_na()

# Gráfico
ggplot(preco_avaliacao, aes(Price, Rating)) +
  geom_point(color = c("#A11D21"), size = 3, alpha = .3) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  labs(x = "Preço", y = "Avaliação") +
  theme_estat()

ggsave("dispersao.png", width = 158, height = 93, units = "mm")

# Quadro de medidas resumo
quadro_resumo2 <- preco_avaliacao %>%
  summarize(Média = round(mean(Price),2),
            `Desvio Padrão` = round(sd(Price),2),
            `Mínimo` = round(min(Price),2),
            `1º Quartil` = round(quantile(Price, probs = .25),2),
            Mediana = round(quantile(Price, probs = .5),2),
            `3º Quartil` = round(quantile(Price, probs = .75),2),
            `Máximo` = round(max(Price),2)) %>% t() %>% as.data.frame() %>%
  mutate(V1 = str_replace (V1,"\\.",","))

xtable::xtable(quadro_resumo2)

quadro_resumo3 <- preco_avaliacao %>%
  summarize(Média = round(mean(Rating),2),
            `Desvio Padrão` = round(sd(Rating),2),
            `Variância` = round(var(Rating),2),
            `Mínimo` = round(min(Rating),2),
            `1º Quartil` = round(quantile(Rating, probs = .25),2),
            Mediana = round(quantile(Rating, probs = .5),2),
            `3º Quartil` = round(quantile(Rating, probs = .75),2),
            `Máximo` = round(max(Rating),2)) %>% t() %>% as.data.frame() %>%
  mutate(V1 = str_replace (V1,"\\.",","))

xtable::xtable(quadro_resumo3)

# Teste de Correlação
cor(preco_avaliacao$Price, preco_avaliacao$Rating)

# Teste de Normalidade
shapiro.test(dados$Price)
shapiro.test(dados$Rating)


### Análise 5 - Frequência de cada tipo de devolução por marca
freq_devol <- dados %>%
  select(Brand, Motivo.devolução) %>%
  tidyr::drop_na() %>%
  group_by(Brand, Motivo.devolução) %>%
  summarise(freq = n()) %>%
  mutate(perc = round(freq/sum(freq),4)*100)

porcentagens2 <- str_c(freq_devol$perc, "%") %>% str_replace("\\.", ",")
legendas2 <- str_squish(str_c(freq_devol$freq, " (", porcentagens2, ")"))

# Grafico
ggplot(freq_devol, aes(forcats::fct_reorder(Brand, freq, .desc = T),
                       y = freq, fill = Motivo.devolução, label = legendas2)) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, hjust = 0.5, size = 1.55) +
  labs(x = "Marca", y = "Frequência") +
  scale_y_continuous(breaks = seq(0, 40, by = 5)) +
  theme_estat(legend.title = element_blank())

ggsave("coluna2.png", width = 158, height = 93, units = "mm")

# Tabela
xtable::xtable(freq_devol)

### Análise 6 - Avaliação média por marca
avaliacao_marca <- dados %>%
  select(Brand, Rating) %>%
  tidyr::drop_na() %>%
  group_by(Brand) %>%
  summarise(media = round(mean(Rating), 2))

# Gráfico
legendas3 <- as.vector(str_squish(str_c(avaliacao_marca$media))) %>% str_replace("\\.", ",")

ggplot(avaliacao_marca, aes(forcats::fct_reorder(Brand, media, .desc = T),
                            media, fill = "#A11D21", label = legendas3)) +
  geom_col() +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  labs(x = "Marcas", y = "Média da Marca") +
  coord_cartesian(ylim = c(2, 2.7)) +
  theme_estat() +
  theme(legend.position = "none")

ggsave("coluna3.png", width = 158, height = 93, units = "mm")

# Tabela
quadro_resumo4 <- avaliacao_marca %>%
  group_by(Brand) %>%
  summarize(Média = round(mean(Rating),2),
            `Desvio Padrão` = round(sd(Rating),2),
            `Variância` = round(var(Rating),2),
            `Mínimo` = round(min(Rating),2),
            `1º Quartil` = round(quantile(Rating, probs = .25),2),
            Mediana = round(quantile(Rating, probs = .5),2),
            `3º Quartil` = round(quantile(Rating, probs = .75),2),
            `Máximo` = round(max(Rating),2)) %>% t() %>% as.data.frame() %>%
  mutate(V1 = str_replace (V1,"\\.",",")) %>%
  mutate(V2 = str_replace (V2,"\\.",",")) %>%
  mutate(V3 = str_replace (V3,"\\.",",")) %>%
  mutate(V4 = str_replace (V4,"\\.",",")) %>%
  mutate(V5 = str_replace (V5,"\\.",","))

xtable::xtable(quadro_resumo4)

# Teste
rstatix::kruskal_test(dados, Rating ~ Brand)
