# Autora: Taciara Zborowski Horst
# Laboratório de Pedometria - UTFPR
# Data: 2021-09-01

# COMPUTA:
# área de solo hidromórfico em diferentes niveis
# calculado a partir dos arquivos
# legenda total - com base na coluna 'filtered_soil_legenda.rds'
# inclu_1 'filtered_soil_inclu_1.rds'
# inclu_2 'filtered_soil_inclu_2.rds'
# recortado do mapa de Pedologia do IBGE
# disponível em https://www.ibge.gov.br/geociencias/


#######
# R-Packages
# Check if the sf, data.table, and RColorBrewer packages are installed.
# If not, install them.
if (!require(sf)) {
  install.packages("sf")
}
if (!require(data.table)) {
  install.packages("data.table")
}
if (!require(RColorBrewer)) {
  install.packages("RColorBrewer")
}
if (!require(dplyr)) {
  install.packages("dplyr")
}
if (!require(stringr)) {
  install.packages("stringr")
}

# Importar arquivo
filtered_soil <- readRDS("results/definicoes/filtered_soil_legenda.rds")
filtered_soil_inclu_1 <- readRDS("results/definicoes/filtered_soil_inclu_1.rds")
filtered_soil_inclu_2 <- readRDS("results/definicoes/filtered_soil_inclu_2.rds")


########################
# Reprojetar os dados de solos e áreas de inclusão
# para o mesmo sistema de coordenadas UTM
filtered_soil_utm <- st_transform(filtered_soil, crs = 32723)
filtered_soil_inclu_1_utm <- st_transform(filtered_soil_inclu_1, crs = 32723)
filtered_soil_inclu_2_utm <- st_transform(filtered_soil_inclu_2, crs = 32723)


# Area legenda
# mapa filtrado no nível geral (termos de 'hidro_features')
total_area_legenda_mha <- as.numeric(
  sum(
    st_area(filtered_soil_utm)
  ) / 1e10
)

# Resultado - a area da legenda e 113 Mha
# O Brasil tem aproximadamente 851 Mha
# A área de solos hidromórficos representa 13% do território nacional
# (nivel total da legenda com termos explicitos de 'hidro_features')

# Calculo das areas considerando as inclusoes
# nesse passo precisamos fazer a união das geometrias
# entre legenda e inclusões
# para garantir que a area seja computada apenas uma vez

# Combine geometries before union
combined_features_legenda_p1 <- rbind(
  filtered_soil_utm,
  filtered_soil_inclu_1_utm
)

# Combine geometries before union
combined_features_legenda_p1_p2 <- rbind( # nolint
  filtered_soil_utm,
  filtered_soil_inclu_1_utm,
  filtered_soil_inclu_2_utm
)

# Perform union operation
merged_legenda_p1 <- st_union(combined_features_legenda_p1)
merged_legenda_p1_p2 <- st_union(combined_features_legenda_p1_p2)

str(merged_legenda_p1)



# mapa filtrado no nível 'inclu_1' (termos de 'hidro_features')
total_inclu1_area_mha <- as.numeric(
  sum(
    st_area(merged_legenda_p1)
  ) / 1e10
)


# resultado 212 Mha
# mapa filtrado no nível 'inclu_1' (termos de 'hidro_features')
# representa 25% do território nacional.
# Considerando que destes 212 Mha, 113 estao contemplados
# no nivel da legenda.
# ou seja,
# 212 - 113 = 99 Mha
# 20% de 99 Mha = 19,8 Mha
# 113 + 19,8 = 132,8 Mha

# Considerando a legenda + incl 1, estima-se 
# 132,8 Mha de solos hidromórficos no Brasil
# ou seja, 16% do território nacional

# mapa filtrado no nível 'inclu_2'  (termos de 'hidro_features')
total_inclu2_area_mha <- as.numeric(
  sum(
    st_area(merged_legenda_p1_p2)
  ) / 1e10
)

# area calculada
area_cal_inclu2 <- total_inclu2_area_mha - total_area_legenda_mha
# considera que a area de inclusao de solos hidromorficos
# em outras unidades de mapeamento
# pode chegar ate 20%
sum_area_leg_inclu1_inclu2 <- (20*area_cal_inclu2)/100


# area total estimada
area_estimada <- total_area_legenda_mha + sum_area_leg_inclu1_inclu2

# resultado 134,4589 Mha de solos hidromorficos

brazil_area <- 851.287 #Mha milhoes de hectares

# calcular proporcoes

prop_legend <- (total_area_legenda_mha *100)/brazil_area
prop_incl2 <- (area_estimada *100)/brazil_area  

# resultado = 15% 

# Armazenando os resultados
total_area <- cbind(total_area_legenda_mha,
  total_inclu1_area_mha,
  total_inclu2_area_mha)

write.csv(cbind(total_area), "results/total_area_hidromorficos.csv")


