# Autora: Taciara Zborowski Horst
# Laboratório de Pedometria - UTFPR
# Data: 2021-09-01


# DEFINE:
# Filtro para solos hidromórficos no segundo nível
# e em inclusões buscando 'hidro_features'
# no mapa de Pedologia do IBGE
# disponível em https://www.ibge.gov.br/geociencias/

# EXPORTA:
# arquivo com dados filtrados em .rds na pasta 'results'
# 'filtered_soil_legenda' 'filtered_soil_inclu_p1' 'filtered_soil_inclu_p2'

#######
# Pacotes R
# Verifica se os pacotes sf, data.table e RColorBrewer estão instalados.
# Se não, instala-os.
if (!require(sf)) {
  install.packages("sf")
}
if (!require(data.table)) {
  install.packages("data.table")
}
if (!require(RColorBrewer)) {
  install.packages("RColorBrewer")
}
if(!require(dplyr)){
  install.packages("dplyr")
}
if(!require(stringr)){
  install.packages("stringr")
}

#######
# Load geospatial data
soil_map <- sf::read_sf("data/ibge/pedo_area.shp")

# Importar definições de solos hidromórficos
hidro_class <- readRDS("results/definicoes/hidro_class.rds")
hidro_features <- readRDS("results/definicoes/hidro_features.rds")


##########################
# Criar a expressão regular
# Combina os termos com "|"
# Ela busca a presença de qualquer termo de `hidro_features`
hidro_features <- paste(hidro_features, collapse = "|")


##########################
# CLASSE LEGENDA (toda classificação)

# Filtrar o shapefile para exibir apenas as classes definidas
# Limpar e padronizar a coluna 'legenda'
soil_map$legenda <- str_trim(soil_map$legenda)  # Remove espaços em branco extras
soil_map$legenda <- str_to_upper(soil_map$legenda)  # Converte para maiúsculas para padronização

# Aplicar o filtro para selecionar registros onde `legenda`
# contém qualquer termo de `hidro_features`
filtered_soil_legenda <- soil_map %>%
  filter(grepl(hidro_features, legenda, ignore.case = TRUE))  # Busca qualquer termo no texto

# Acessar os dados
# Verificar a quantidade de registros filtrados
filtered_soil_legenda_data <- sf::st_drop_geometry(filtered_soil_legenda)

# Retornar as classes únicas que foram filtradas
# Ordenar alfabeticamente
print(table(sort(unique(filtered_soil_legenda_data$legenda))))

# Salvar os objetos para utilizar em outro código
saveRDS(filtered_soil_legenda, "results/definicoes/filtered_soil_legenda.rds")

##########################
# INCLUSÃO 1 (tem um dos termos na coluna `inclu_p1`)

# Filtrar o shapefile para exibir apenas as classes definidas
# Limpar e padronizar a coluna 'inclu_p1'
soil_map$inclu_p1 <- str_trim(soil_map$inclu_p1)  # Remove espaços em branco extras
soil_map$inclu_p1 <- str_to_upper(soil_map$inclu_p1)  # Converte para maiúsculas para padronização

# Aplicar o filtro para selecionar registros onde `inclu_p1`
# contém qualquer termo de `hidro_features`
filtered_soil_inclu_p1 <- soil_map %>%
  filter(grepl(hidro_features, inclu_p1, ignore.case = TRUE))  # Busca qualquer termo no texto

# Acessar os dados
# Verificar a quantidade de registros filtrados
filtered_soil_inclu_1_data <- sf::st_drop_geometry(filtered_soil_inclu_p1)

# Retornar as classes únicas que foram filtradas
# Ordenar alfabeticamente
print(table(sort(unique(filtered_soil_inclu_1_data$inclu_p1))))

# Salvar os objetos para utilizar em outro código
saveRDS(filtered_soil_inclu_p1, "results/definicoes/filtered_soil_inclu_1.rds")


##########################
# INCLUSÃO 2 (tem um dos termos na coluna `inclu_p2`)

# Filtrar o shapefile para exibir apenas as classes definidas
# Limpar e padronizar a coluna 'inclu_p2'
soil_map$inclu_p2 <- str_trim(soil_map$inclu_p2)  # Remove espaços em branco extras
soil_map$inclu_p2 <- str_to_upper(soil_map$inclu_p2)  # Converte para maiúsculas para padronização

# Aplicar o filtro para selecionar registros onde `inclu_p2`
# contém qualquer termo de `hidro_features`
filtered_soil_inclu_p2 <- soil_map %>%
  filter(grepl(hidro_features, inclu_p2, ignore.case = TRUE))  # Busca qualquer termo no texto

# Acessar os dados
# Verificar a quantidade de registros filtrados
filtered_soil_inclu_2_data <- sf::st_drop_geometry(filtered_soil_inclu_p2)

# Retornar as classes únicas que foram filtradas
# Ordenar alfabeticamente
print(table(sort(unique(filtered_soil_inclu_2_data$inclu_p2))))

# Salvar os objetos para utilizar em outro código
saveRDS(filtered_soil_inclu_p2, "results/definicoes/filtered_soil_inclu_2.rds")

##########################
#x11()




################################# nova definicao 20-09

##########################
# INCLUSÃO 1 (tem um dos termos na coluna `inclu_p1`)

# Filtrar o shapefile para exibir apenas as classes definidas
# Limpar e padronizar a coluna 'inclu_p1'
soil_map$componente <- str_trim(soil_map$componente)  # Remove espaços em branco extras
soil_map$componente <- str_to_upper(soil_map$componente)  # Converte para maiúsculas para padronização

# Aplicar o filtro para selecionar registros onde `inclu_p1`
# contém qualquer termo de `hidro_features`
filtered_soil_componente <- soil_map %>%
  filter(grepl(hidro_features, componente, ignore.case = TRUE))  # Busca qualquer termo no texto

# Acessar os dados
# Verificar a quantidade de registros filtrados
filtered_soil_componente_data <- sf::st_drop_geometry(filtered_soil_componente)

# Retornar as classes únicas que foram filtradas
# Ordenar alfabeticamente
print(table(sort(unique(filtered_soil_componente_data$componente))))

# Salvar os objetos para utilizar em outro código
saveRDS(filtered_soil_componente, "results/definicoes/filtered_soil_filtered_componente.rds")
