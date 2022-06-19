# Carregando Pacotes
library(basedosdados)
library(tidyverse)
library(readxl)


# Definindo chave de endereço

basedosdados::set_billing_id("educacao-353422")




# Carregando base de dados - IDEB

"SELECT ano, id_municipio, id_escola, rede, anos_escolares, ideb 
FROM `basedosdados.br_inep_ideb.escola` 
  WHERE id_municipio = '4316907'" %>% 
    read_sql() %>% 
    mutate_if(bit64::is.integer64, as.integer)-> ideb


install.packages("bit64")
library(bit64)


# Gráfico IDEB 1 - versão preliminar

ideb %>% 
  mutate_if(bit64::is.integer64, as.integer) %>% 
  group_by(ano, rede) %>% 
  summarise(media = mean(ideb, na.rm = T)) %>% 
  ggplot(aes(x = ano, y = media, color = rede)) +
  geom_line(lwd = 0.8) +
  theme_minimal() 
  

ideb %>% glimpse()


# Comparando a média do IDEB entre as redes de ensino em SM

ideb %>% 
  filter(rede != "privada",
         ano < 2021) %>% 
  mutate(rede = factor(rede,
                       levels = c("federal",
                                  "estadual",
                                  "municipal"),
                       labels = c("Federal",
                                  "Estadual",
                                  "Municipal"))) %>% 
  group_by(ano, rede) %>% 
  summarise(media = mean(ideb, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media, color = rede)) +
  geom_line(size = 1) +
  theme_bw(base_size = 14) +
  scale_x_continuous(breaks = seq(2005, 2020, 2)) +
  scale_y_continuous(breaks = seq(4, 8, 1),
                     limits = c(4, 8)) +
  labs(title = "Média do IDEB para Santa Maria - RS",
       color = "Rede de Ensino",
       x = "Ano",
       y = "Média do IDEB")
  


# Carregando base de dados - INSE

INSE_2019_ESCOLAS <- read_excel("C:/Users/acer/Downloads/INSE_2019_ESCOLAS.xlsx")




# Filtrando os dados - MINHAS TENTATIVAS

# Média IDEB entre os anos escolares (iniciais e finais) da rede municipal para SM

ideb %>%
  filter(rede == "municipal") %>% 
  mutate(anos_escolares = factor(anos_escolares,
                       levels = c("iniciais (1-5)",
                                  "finais (6-9)"))) %>% 
  group_by(ano, anos_escolares) %>% 
  summarise(media = mean(ideb, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media, color = anos_escolares))+
  geom_line(size = 1) +
  theme_bw(base_size = 14) +
  scale_x_continuous(breaks = seq(2005, 2020, 2)) +
  scale_y_continuous(breaks = seq(3, 7, 1),
                     limits = c(3, 7)) +
  labs(title = "Diferença IDEB para Anos Escolares da Rede Municipal de SM",
         color = "Anos Escolares",
         x = "Ano",
         y = "Média do IDEB")


# Média IDEB entre os anos escolares (iniciais e finais) da rede estadual para SM

ideb %>%
  filter(rede == "estadual") %>% 
  mutate(anos_escolares = factor(anos_escolares,
                                 levels = c("iniciais (1-5)",
                                            "finais (6-9)"))) %>% 
  group_by(ano, anos_escolares) %>% 
  summarise(media = mean(ideb, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media, color = anos_escolares))+
  geom_line(size = 1) +
  theme_bw(base_size = 14) +
  scale_x_continuous(breaks = seq(2005, 2020, 2)) +
  scale_y_continuous(breaks = seq(3, 7, 1),
                     limits = c(3, 7)) +
  labs(title = "Diferença IDEB para Anos Escolares da Rede Estadual de SM",
       color = "Anos Escolares",
       x = "Ano",
       y = "Média do IDEB") 


# Média IDEB entre os anos escolares (iniciais e finais) da rede federal para SM

ideb %>%
  filter(rede == "federal") %>% 
  mutate(anos_escolares = factor(anos_escolares,
                                 levels = c("iniciais (1-5)",
                                            "finais (6-9)"))) %>% 
  group_by(ano, anos_escolares) %>% 
  summarise(media = mean(ideb, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = media, color = anos_escolares))+
  geom_line(size = 1) +
  theme_bw(base_size = 14) +
  scale_x_continuous(breaks = seq(2005, 2020, 2)) +
  scale_y_continuous(breaks = seq(5, 9, 1),
                     limits = c(5, 9)) +
  labs(title = "Diferença IDEB para Anos Escolares da Rede Federal de SM",
       color = "Anos Escolares",
       x = "Ano",
       y = "Média do IDEB")
