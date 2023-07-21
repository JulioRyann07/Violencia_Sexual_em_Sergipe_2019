#https://www.gov.br/mdh/pt-br/acesso-a-informacao/dados-abertos/disque100

library(tidyverse)
disque <- read.csv2('E:/Códigos/R/Violencia/disque100-2019.csv', header = T, sep = ';', dec = ',')

# Filtragem das vítimas por idadeado - SE

vit_SE <- disque %>%
  filter(disque$vitima_uf == 'SE')
attach(vit_SE)
# Gênero das vitimas

v_genero <- table(vitima_sexo)
v_genero <- data.frame(v_genero) %>%
  rename(sexo = vitima_sexo, freq = Freq) %>%
  mutate(sexo = case_when(sexo == 'Não informado' ~ 'Indefinido',
                          T ~ sexo)) %>%
  mutate(proporcao = freq / sum(freq)) %>%
  mutate(label = paste0(round(100*freq/sum(freq),1), "%\n", sexo)) %>%
  mutate(f_proporcao = proporcao / 2, f_proporcao = round(proporcao / 2, 3)) %>%
  mutate(f_proporcao = case_when(f_proporcao == max(f_proporcao) ~ 0.5,
                                 T ~ f_proporcao))

ggplot(v_genero, aes(x = 1, y = proporcao)) +
  geom_rect(aes(xmin = 0.9, xmax = 1.1, ymin = 0, ymax = f_proporcao, fill = sexo),
            alpha = 1, color = "white") +
  geom_rect(aes(xmin = 0, xmax = 0.8, ymin = cumsum(proporcao) - proporcao, ymax =
                  cumsum(proporcao), fill = sexo),
            color = "white") +
  geom_text(aes(x = c(0.4, 0.4, 0.5), y = cumsum(proporcao) - proporcao / 2, label = label),
            color = 'black', size = c(8, 6, 5), angle = c(0, 0,304)) +
  scale_fill_manual(values = c("#096ad3","#5199e9","#99c8ff")) +
  scale_color_manual(values = c("#096ad3","#5199e9","#99c8ff")) +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.position = "none")

# Gênero das suspeitos

s_genero <- table(suspeito_pf_sexo)
s_genero <- data.frame(s_genero) %>%
  rename(sexo = suspeito_pf_sexo, freq = Freq) %>%
  mutate(sexo = case_when(sexo == 'Não informado' ~ 'Indefinido',
                          T ~ sexo)) %>%
  mutate(proporcao = freq / sum(freq)) %>%
  mutate(label= paste0(round(100*freq/sum(freq),1), "%\n", sexo)) %>%
  mutate(f_proporcao = round(proporcao / 2, 3)) %>%
  mutate(f_proporcao = case_when(f_proporcao == max(f_proporcao) ~ 0.5,
                                 T ~ f_proporcao))

ggplot(s_genero, aes(x = 1, y = proporcao)) +
  geom_rect(aes(xmin = 0.9, xmax = 1.1, ymin = 0, ymax = f_proporcao, fill = sexo),
            alpha = 1, color = "white") +
  geom_rect(aes(xmin = 0, xmax = 0.8, ymin = cumsum(proporcao) - proporcao, ymax =
                  cumsum(proporcao), fill = sexo),
            color = "white") +
  geom_text(aes(x = c(0.4, 0.4, 0.5), y = cumsum(proporcao) - proporcao / 2, label = label),
            color = 'black', size = c(8, 6, 5), angle = c(0, 0,316)) +
  scale_fill_manual(values = c("#096ad3","#5199e9","#99c8ff")) +
  scale_color_manual(values = c("#096ad3","#5199e9","#99c8ff")) +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.position = "none")

# Idade das vitimas

v_idade <- table(vit_SE$vitima_faixa_etaria)
v_idade <- data.frame(v_idade) %>%
  rename(idade = Var1, freq = Freq) %>%
  mutate(faixa = case_when(idade == 'Recém-nascido' | idade == '0 a 3 anos' ~ '3 anos ou menos',
                           idade == '4 a 7 anos' | idade == '8 a 11 anos' ~ '4 a 11 anos',
                           idade == '12 a 14 anos' | idade == '15 a 17 anos' ~ '12 a 17 anos',
                           idade == '31 a 35 anos' | idade == '36 a 40 anos' ~ '31 a 40 anos',
                           idade == '41 a 45 anos' | idade == '46 a 50 anos' ~ '41 a 50 anos',
                           idade == '51 a 55 anos' | idade == '56 a 60 anos' ~ '51 a 60 anos',
                           idade == '61 a 65 anos' | idade == '66 a 70 anos' ~ '61 a 70 anos',
                           idade == '71 a 75 anos' | idade == '76 a 80 anos' ~ '71 a 80 anos',
                           idade == '81 a 85 anos' | idade == '85 a 90 anos' | idade == '91 anos ou mais' ~ '81 anos ou mais',
                           T ~ idade))

v_idade <- aggregate(v_idade$freq, by = list(v_idade$faixa), FUN = sum) %>%
  rename(idade = Group.1, freq = x) %>%
  filter(idade != 'N/D', idade != 'Nascituro') %>%
  mutate(proporcao = 100 * freq / sum(freq), proporcao = round(proporcao, 3)) %>%
  mutate(ordem = idade) %>%
  separate(ordem, into = c('ordem', 'resto'), sep = 2, convert = T) %>%
  select(-resto)

ggplot(v_idade, aes(y = reorder(idade, ordem), x = freq, fill = freq)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = sprintf("%1.1f%%", round(proporcao, 1))), nudge_y = 0.03, nudge_x = 55,
            color = '#054FB9', size = 4.5, fontface = "bold") +
  labs(x = "", y = '') +
  scale_fill_gradient(low = "#99c8ff", high = "#096ad3") +
  theme_minimal() +
  cowplot::theme_minimal_hgrid(line_size = 1) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.025, 0.05))) 

# Idade das suspeitos

s_idade <- table(vit_SE$suspeito_pf_faixa_etaria)
s_idade <- data.frame(s_idade) %>%
  rename(idade = Var1, freq = Freq) %>%
  mutate(faixa = case_when(idade == 'Recém-nascido' | idade == '0 a 3 anos' |
                             idade == '4 a 7 anos' | idade == '8 a 11 anos' ~ '11 anos ou menos',
                           idade == '12 a 14 anos' | idade == '15 a 17 anos' ~ '12 a 17 anos',
                           idade == '31 a 35 anos' | idade == '36 a 40 anos' ~ '31 a 40 anos',
                           idade == '41 a 45 anos' | idade == '46 a 50 anos' ~ '41 a 50 anos',
                           idade == '51 a 55 anos' | idade == '56 a 60 anos' ~ '51 a 60 anos',
                           idade == '61 a 65 anos' | idade == '66 a 70 anos' ~ '61 a 70 anos',
                           idade == '71 a 75 anos' | idade == '76 a 80 anos' ~ '71 a 80 anos',
                           idade == '81 a 85 anos' | idade == '85 a 90 anos' | idade == '91 anos ou mais' ~
                             '81 anos ou mais',
                           T ~ idade))

s_idade <- aggregate(s_idade$freq, by = list(s_idade$faixa), FUN = sum) %>%
  rename(idade = Group.1, freq = x) %>%
  filter(idade != 'N/D', idade != 'Nascituro') %>%
  mutate(proporcao = 100 * freq / sum(freq), proporcao = round(proporcao, 3))

ggplot(s_idade, aes(y = idade, x = freq, fill = freq)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = sprintf("%1.1f%%", round(proporcao, 1))), nudge_y = 0.03, nudge_x =
              55,
            color = '#054FB9', size = 4.5, fontface = "bold") +
  labs(x = "", y = '') +
  scale_fill_gradient(low = "#99c8ff", high = "#096ad3") +
  theme_minimal() +
  cowplot::theme_minimal_hgrid(line_size = 1) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.025, 0.05)))

# Frequencia de violência

frequencia <- table(vit_SE$cenario_ocorrencia)
frequencia <- data.frame(frequencia) %>%
  rename(ocorrencia = Var1, freq = Freq) %>%
  mutate(ocorrencia = case_when(ocorrencia == 'Toda tarde' | ocorrencia == 'Toda manhã' ~
                                  'Única vez',
                                ocorrencia == 'NULL' | ocorrencia == 'Não informado' ~ 'Não informado',
                                T ~ ocorrencia))
frequencia <- aggregate(frequencia$freq, by = list(frequencia$ocorrencia), FUN = sum) %>%
  rename(idade = Group.1, freq = x) %>%
  mutate(proporcao = 100 * freq / sum(freq), proporcao = round(proporcao, 3))

ggplot(frequencia, aes(y = reorder(idade, -freq), x = freq, fill = freq)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = sprintf("%1.1f%%", round(proporcao, 1))), nudge_y = 0.03, nudge_x =
              155,
            color = '#054FB9', size = 4.5, fontface = "bold") +
  labs(x = "", y = '') +
  scale_fill_gradient(low = "#99c8ff", high = "#096ad3") +
  theme_minimal() +
  cowplot::theme_minimal_hgrid(line_size = 1) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.025, 0.05)))

# Tabela, relacionamento com o suspeito

relacao_S <- table(vit_SE$relacao_suspeito)
relacao_S <- data.frame(relacao_S) %>%
  rename(relacao = Var1) %>%
  mutate(O_relacao = fct_collapse(relacao,
                                  'Parentes' = c('Avó', 'Avô', 'Bisneto(a)', 'Companheiro (a)',
                                                 'Cunhado (a)', 'Enteado(a)', 'Esposa', 'Familiares',
                                                 'Genro/Nora', 'Irmão (ã)', 'Madrasta', 'Marido', 'Neto(a)',
                                                 'Padrasto', 'Padrinho/Madrinha', 'Primo(a)', 'Sobrinho(a)',
                                                 'Sogro(a)', 'Tio (a)', 'Ex-Companheiro (a)', 'Ex-Esposa', 'Ex-Marido', 'Filho (a)'),
                                  'Conhecido' = c('Amigo (a)', 'Cuidador (a)', 'Diretor(a) de escola',
                                                  'Diretor(a) de Unidade Prisional', 'Empregado (a)', 'Empregador',
                                                  'Namorado(a)', 'Professor(a)', 'Vizinho (a)'),
                                  'Desconhecido(a)' = c('Não informado', 'Não se aplica')))

relacao_S <- aggregate(relacao_S$Freq, by = list(relacao_S$O_relacao), FUN = sum) %>%
  rename(relacao = Group.1, freq = x) %>%
  mutate(proporcao = 100 * freq / sum(freq), proporcao = round(proporcao, 3))

ggplot(relacao_S, aes(y = reorder(relacao, -proporcao), x = proporcao, fill = proporcao)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = sprintf("%1.1f%%", round(proporcao, 5))), nudge_y = 0.03, nudge_x =
              2.5,
            color = '#054FB9', size = 4.5, fontface = "bold") +
  labs(x = "", y = '') +
  scale_fill_gradient(low = "#99c8ff", high = "#096ad3") +
  theme_minimal() +
  cowplot::theme_minimal_hgrid(line_size = 1) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.025, 0.05)))

# Mapa dos municipios

library(sf)

# URL do arquivo shapefile dos municípios de Sergipe no portal do IBGE

url <-
  "https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2021/UFs/SE/SE_Municipios_2021.zip"

tmp <- tempfile()
download.file(url, tmp)
unzip(tmp)
mapa_se <- sf::st_read("SE_Municipios_2021.shp")
cod <- table(disque$vitima_cod_municipio)
cod <- data.frame(cod) %>%
  rename(CD_MUN = Var1) %>%
  mutate(proporcao = Freq / sum(Freq) * 100)
mapa <- left_join(mapa_se, cod)

ggplot() +
  geom_sf(data = mapa, aes(fill = proporcao)) +
  scale_fill_gradient(low = "#d2e4f3", high = "#096ad3") +
  theme_void() +
  theme(legend.position = "none")


# Tabela municipios

f_municipio <- table(vit_SE$vitima_municipio)

f_municipio <- data.frame(f_municipio) %>%
  rename(municipio = Var1) %>%
  separate(municipio, into = c('municipio', 'se'), sep = -23) %>%
  mutate(proporcao = Freq / sum(Freq) * 100) %>%
  arrange(-proporcao) %>%
  slice(1:5)

ggplot(f_municipio, aes(y = reorder(municipio, -proporcao), x = proporcao, fill = proporcao)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = sprintf("%1.1f%%", round(proporcao, 5))), nudge_y = 0.03, nudge_x =
              2.5,
            color = '#054FB9', size = 4.5, fontface = "bold") +
  labs(x = "", y = '') +
  scale_fill_gradient(low = "#99c8ff", high = "#096ad3") +
  theme_minimal() +
  cowplot::theme_minimal_hgrid(line_size = 1) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.025, 0.05))) 
