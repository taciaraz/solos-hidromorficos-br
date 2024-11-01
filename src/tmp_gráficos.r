
# Carregar pacotes necessários
library(sf)
library(dplyr)

# Ler o shapefile dos solos
soil_class <- sf::read_sf("data_soil/pedo_area.shp")
country <- sf::read_sf("data/country2020.geojson")


# Definir as classes de solos hidromórficos
solos_hidromorficos <- c("PLINTOSSOLO ARGILÚVICO", "NEOSSOLO FLÚVICO", "PLANOSSOLO HÁPLICO",
  "GLEISSOLO HÁPLICO", "ORGANOSSOLO HÁPLICO", "PLINTOSSOLO HÁPLICO",
  "GLEISSOLO MELÂNICO", "VERTISSOLO HIDROMÓRFICO",
  "GLEISSOLO SÁLICO", "ESPODOSSOLO HUMILÚVICO", "GLEISSOLO TIOMÓRFICO",
  "ESPODOSSOLO FERRI-HUMILÚVICO", "PLANOSSOLO NÁTRICO", "CAMBISSOLO FLÚVICO",
  "ORGANOSSOLO TIOMÓRFICO", "ESPODOSSOLO FERRILÚVICO"
)

# Filtrar o shapefile para incluir apenas as classes especificadas
soil_class_hidromorficos <- soil_class %>%
  filter(legenda_2 %in% solos_hidromorficos)  # Ajuste 'legenda_2' se necessário

#####


####################################
# Plotar o mapa com as classes de solos hidromórficos
legend_classes <- unique(soil_class_hidromorficos$legenda_2)    
legend_colors <- color_pal[match(legend_classes, soil_class_hidromorficos$legenda_2)]

# Plotar o mapas


# Exportar o gráfico em alta resolução
png("área_soloshidromorficos.png", width = 3000, height = 2000, res = 300)  # Definir o arquivo PNG com alta resolução

# Definir o layout com duas áreas: a primeira para o mapa e a segunda para a legenda
layout(matrix(1:2, nrow = 1), widths = c(3, 1))  # 3 partes para o mapa, 1 parte para a legenda

# Configurar parâmetros gráficos para o mapa
par(mar = c(5, 5, 5, 1))  # Ajustar margens para o mapa

# Plotar o mapa
plot(st_geometry(country), col = "white", border = "gray")
plot(st_geometry(soil_class_hidromorficos), col = legend_colors, border = NA, add = TRUE)

# Capturar os limites do layer do Brasil
bbox_country <- st_bbox(country)

# Adicionar uma caixa ajustada aos limites do layer do Brasil
rect(bbox_country["xmin"], bbox_country["ymin"], 
     bbox_country["xmax"], bbox_country["ymax"], border = NA, lwd = 2)

# Adicionar coordenadas automaticamente ao redor da caixa ajustada ao Brasil
axis(1, at = pretty(bbox_country["xmin"]:bbox_country["xmax"]), labels = TRUE, tck = -0.02) # Parte inferior
axis(2, at = pretty(bbox_country["ymin"]:bbox_country["ymax"]), labels = TRUE, tck = -0.02, las = 1) # Parte esquerda

#dev.off()

# Mudar para o segundo painel para a legenda
par(mar = c(5, 0, 5, 5))  # Ajustar margens para a legenda

# Ordenar a legenda em ordem alfabética
ordem_alfabetica <- order(legend_classes)
legend_classes_ordenadas <- legend_classes[ordem_alfabetica]
legend_colors_ordenadas <- legend_colors[ordem_alfabetica]

# Criar a área de legenda separada
plot.new()  # Criar uma nova área de plotagem para a legenda
legend("left", legend = legend_classes_ordenadas, fill = legend_colors_ordenadas, 
       title = "Solos Hidromórficos", cex = 0.8, xjust = 0, yjust = 1)

# Salvar o arquivo e finalizar
dev.off()


###################
# Calculando a área total dos solos hidromorficos
# Certificar-se de que o objeto está em UTM para cálculo preciso de área
soil_class_hidromorficos_utm <- st_transform(soil_class_hidromorficos, crs = 32723)


# Calcular a área de cada feição dos solos hidromórficos em metros quadrados
areas_m2 <- st_area(soil_class_hidromorficos_utm)

# Converter áreas para hectares
areas_ha <- as.numeric(areas_m2) / 10000

# Converter áreas para quilômetros quadrados
areas_km2 <- as.numeric(areas_m2) / 1e6

# Exibir as áreas calculadas em diferentes unidades
print(data.frame(
  Area_m2 = as.numeric(areas_m2),
  Area_ha = areas_ha,
  Area_km2 = areas_km2
))

# Calcular a área total em metros quadrados, hectares e quilômetros quadrados
area_total_m2 <- sum(areas_m2)
area_total_ha <- area_total_m2 / 10000
area_total_km2 <- area_total_m2 / 1e6

# Exibir a área total em todas as unidades
print(paste("Área total dos solos hidromórficos:", area_total_m2, "m²"))
print(paste("Área total dos solos hidromórficos:", area_total_ha, "ha"))
print(paste("Área total dos solos hidromórficos:", area_total_km2, "km²"))

##############################
##############################
##############################

# Criar um gráfico de pizza com a distribuição das áreas dos solos hidromórficos

library(sf)

# Certificar-se de que o objeto está em UTM para cálculos precisos de área
soil_class_hidromorficos_utm <- st_transform(soil_class_hidromorficos, crs = 32723)

# Calcular a área de cada feição dos solos hidromórficos em hectares
areas_ha <- as.numeric(st_area(soil_class_hidromorficos_utm)) / 10000

# Criar um vetor com as áreas associadas a cada classe de solo
area_por_classe <- tapply(areas_ha, soil_class_hidromorficos$legenda_2, sum)

# Ordenar as classes de solo e suas áreas em ordem decrescente (maiores áreas primeiro)
ordem_decrescente <- order(area_por_classe, decreasing = TRUE)
classes_ordenadas <- names(area_por_classe)[ordem_decrescente]
areas_ordenadas <- area_por_classe[ordem_decrescente]
cores_ordenadas <- legend_colors[ordem_decrescente]

# Calcular as porcentagens de cada classe
percentagens <- round((areas_ordenadas / sum(areas_ordenadas)) * 100, 1)

# Identificar classes com <1% e agrupar em "Outras Classes"
menores_que_1 <- percentagens < 1
area_outras <- sum(areas_ordenadas[menores_que_1])
classes_final <- c(classes_ordenadas[!menores_que_1], "OUTRAS CLASSES")
areas_final <- c(areas_ordenadas[!menores_que_1], area_outras)

# Definir novas cores, incluindo uma nova cor para "Outras Classes"
cor_outras <- "#D3D3D3"  # Cinza claro para "Outras Classes"
cores_final <- c(cores_ordenadas[!menores_que_1], cor_outras)

# Calcular as porcentagens atualizadas para o gráfico
percentagens_final <- round((areas_final / sum(areas_final)) * 100, 1)
labels_percent <- paste0(percentagens_final, "%")


##############################

# Exportar o gráfico em alta resolução
png("grafico_soloshidromorficos.png", width = 3000, height = 2000, res = 300)  # Definir o arquivo PNG com alta resolução


# Configurar layout do gráfico para ajustar o espaço da legenda
layout(matrix(1:2, ncol = 2), widths = c(3, 2))  # Configura um layout com duas colunas, uma para o gráfico e outra para a legenda

# Criar o gráfico de pizza com as porcentagens internas e o grupo "Outras Classes"
par(mar = c(5, 5, 5, 1))  # Ajusta as margens para o gráfico de pizza
pie(areas_final, labels = labels_percent, col = cores_final, 
    main = "Distribuição das Áreas dos Solos Hidromórficos")

# Criar a área de legenda separada no layout
par(mar = c(5, 0, 5, 5))  # Ajustar margens para garantir que a legenda fique à direita, sem sobrepor o gráfico
plot.new()  # Cria uma nova área para a legenda
# Adicionar a legenda centralizada verticalmente
legend("center", legend = classes_final, fill = cores_final, 
       title = "CLASSES DE SOLO\n(SIBCS 2º nível categórico)", 
       cex = 0.8, bty = "n", xjust = -1, yjust = 0)

dev.off()  

##############################
##############################
##############################

library(sf)

# Transformar áreas para milhares de hectares
areas_final_milhares <- areas_final / 1000

# Exportar o gráfico em alta resolução
png("grafico_soloshidromorficos_barras.png", width = 3000, height = 2000, res = 300)  # Definir o arquivo PNG com alta resolução

# Criar o gráfico de barras
par(mar = c(10, 5, 5, 5))  # Ajusta as margens para o gráfico de barras
barplot(areas_final_milhares, names.arg = classes_final, col = cores_final, las = 2, 
        main = "Distribuição das Áreas dos Solos Hidromórficos", 
        ylab = "Área (milhares de ha)", cex.names = 0.8, cex.axis = 0.8, border = NA)

# Adicionar a legenda com o título justificado à esquerda
legend("topright", legend = classes_final, fill = cores_final, 
       title = "CLASSES DE SOLO\n(SIBCS 2º nível categórico)", 
       cex = 0.8, bty = "n", xjust = 0)

dev.off()  # Finalizar o dispositivo gráfico e salvar o arquivo

##############################
library(sf)

# Certificar-se de que o objeto está em UTM para cálculos precisos de área
soil_class_hidromorficos_utm <- st_transform(soil_class_hidromorficos, crs = 32723)

# Calcular a área de cada feição dos solos hidromórficos em hectares
areas_ha <- as.numeric(st_area(soil_class_hidromorficos_utm)) / 10000

# Criar um vetor com as áreas associadas a cada classe de solo
area_por_classe <- tapply(areas_ha, soil_class_hidromorficos$legenda_2, sum)

# Somar a área total dos solos hidromórficos
area_total_hidromorficos <- sum(area_por_classe)

# Área total do Brasil em hectares
area_brasil <- 851487700  # Aproximadamente 851 milhões de hectares

# Calcular a porcentagem da área dos solos hidromórficos em relação ao Brasil
percentagem_hidromorficos_brasil <- (area_total_hidromorficos / area_brasil) * 100

# Exibir os resultados
cat("Área total dos solos hidromórficos:", round(area_total_hidromorficos / 1000, 2), "milhares de ha\n")
cat("Área total do Brasil:", round(area_brasil / 1000, 2), "milhares de ha\n")
cat("Porcentagem dos solos hidromórficos em relação ao Brasil:", round(percentagem_hidromorficos_brasil, 4), "%\n")



###################
# Calculando a área total dos solos hidromorficos
# Certificar-se de que o objeto está em UTM para cálculo preciso de área
soil_class_hidromorficos_utm <- st_transform(soil_class_hidromorficos, crs = 32723)


# Calcular a área de cada feição dos solos hidromórficos em metros quadrados
areas_m2 <- st_area(soil_class_hidromorficos_utm)

# Converter áreas para hectares
areas_ha <- as.numeric(areas_m2) / 10000

# Converter áreas para quilômetros quadrados
areas_km2 <- as.numeric(areas_m2) / 1e6

# Exibir as áreas calculadas em diferentes unidades
print(data.frame(
  Area_m2 = as.numeric(areas_m2),
  Area_ha = areas_ha,
  Area_km2 = areas_km2
))

# Calcular a área total em metros quadrados, hectares e quilômetros quadrados
area_total_m2 <- sum(areas_m2)
area_total_ha <- area_total_m2 / 10000
area_total_km2 <- area_total_m2 / 1e6

# Exibir a área total em todas as unidades
print(paste("Área total dos solos hidromórficos:", area_total_m2, "m²"))
print(paste("Área total dos solos hidromórficos:", area_total_ha, "ha"))
print(paste("Área total dos solos hidromórficos:", area_total_km2, "km²"))
print(paste("Área total dos solos hidromórficos:", areas_final_milhares, "Milhares ha"))

##############################
##############################
##############################

# Criar um gráfico de pizza com a distribuição das áreas dos solos hidromórficos

library(sf)

# Certificar-se de que o objeto está em UTM para cálculos precisos de área
soil_class_hidromorficos_utm <- st_transform(soil_class_hidromorficos, crs = 32723)

# Calcular a área de cada feição dos solos hidromórficos em hectares
areas_ha <- as.numeric(st_area(soil_class_hidromorficos_utm)) / 10000

# Criar um vetor com as áreas associadas a cada classe de solo
area_por_classe <- tapply(areas_ha, soil_class_hidromorficos$legenda_2, sum)

# Ordenar as classes de solo e suas áreas em ordem decrescente (maiores áreas primeiro)
ordem_decrescente <- order(area_por_classe, decreasing = TRUE)
classes_ordenadas <- names(area_por_classe)[ordem_decrescente]
areas_ordenadas <- area_por_classe[ordem_decrescente]
cores_ordenadas <- legend_colors[ordem_decrescente]

# Calcular as porcentagens de cada classe
percentagens <- round((areas_ordenadas / sum(areas_ordenadas)) * 100, 1)

# Identificar classes com <1% e agrupar em "Outras Classes"
menores_que_1 <- percentagens < 1
area_outras <- sum(areas_ordenadas[menores_que_1])
classes_final <- c(classes_ordenadas[!menores_que_1], "OUTRAS CLASSES")
areas_final <- c(areas_ordenadas[!menores_que_1], area_outras)

# Definir novas cores, incluindo uma nova cor para "Outras Classes"
cor_outras <- "#D3D3D3"  # Cinza claro para "Outras Classes"
cores_final <- c(cores_ordenadas[!menores_que_1], cor_outras)

# Calcular as porcentagens atualizadas para o gráfico
percentagens_final <- round((areas_final / sum(areas_final)) * 100, 1)
labels_percent <- paste0(percentagens_final, "%")


##############################

# Exportar o gráfico em alta resolução
png("grafico_soloshidromorficos.png", width = 3000, height = 2000, res = 300)  # Definir o arquivo PNG com alta resolução


# Configurar layout do gráfico para ajustar o espaço da legenda
layout(matrix(1:2, ncol = 2), widths = c(3, 2))  # Configura um layout com duas colunas, uma para o gráfico e outra para a legenda

# Criar o gráfico de pizza com as porcentagens internas e o grupo "Outras Classes"
par(mar = c(5, 5, 5, 1))  # Ajusta as margens para o gráfico de pizza
pie(areas_final, labels = labels_percent, col = cores_final, 
    main = "Distribuição das Áreas dos Solos Hidromórficos")

# Criar a área de legenda separada no layout
par(mar = c(5, 0, 5, 5))  # Ajustar margens para garantir que a legenda fique à direita, sem sobrepor o gráfico
plot.new()  # Cria uma nova área para a legenda
# Adicionar a legenda centralizada verticalmente
legend("center", legend = classes_final, fill = cores_final, 
       title = "CLASSES DE SOLO\n(SIBCS 2º nível categórico)", 
       cex = 0.8, bty = "n", xjust = -1, yjust = 0)

dev.off()  

##############################
##############################
##############################

library(sf)

# Transformar áreas para milhares de hectares
areas_final_milhares <- areas_final / 1000

# Exportar o gráfico em alta resolução
png("grafico_soloshidromorficos_barras.png", width = 3000, height = 2000, res = 300)  # Definir o arquivo PNG com alta resolução

# Criar o gráfico de barras
par(mar = c(10, 5, 5, 5))  # Ajusta as margens para o gráfico de barras
barplot(areas_final_milhares, names.arg = classes_final, col = cores_final, las = 2, 
        main = "Distribuição das Áreas dos Solos Hidromórficos", 
        ylab = "Área (milhares de ha)", cex.names = 0.8, cex.axis = 0.8, border = NA)

# Adicionar a legenda com o título justificado à esquerda
legend("topright", legend = classes_final, fill = cores_final, 
       title = "CLASSES DE SOLO\n(SIBCS 2º nível categórico)", 
       cex = 0.8, bty = "n", xjust = 0)

dev.off()  # Finalizar o dispositivo gráfico e salvar o arquivo

##############################
library(sf)

# Certificar-se de que o objeto está em UTM para cálculos precisos de área
soil_class_hidromorficos_utm <- st_transform(soil_class_hidromorficos, crs = 32723)

# Calcular a área de cada feição dos solos hidromórficos em hectares
areas_ha <- as.numeric(st_area(soil_class_hidromorficos_utm)) / 10000

# Criar um vetor com as áreas associadas a cada classe de solo
area_por_classe <- tapply(areas_ha, soil_class_hidromorficos$legenda_2, sum)

# Somar a área total dos solos hidromórficos
area_total_hidromorficos <- sum(area_por_classe)

# Área total do Brasil em hectares
area_brasil <- 851487700  # Aproximadamente 851 milhões de hectares

# Calcular a porcentagem da área dos solos hidromórficos em relação ao Brasil
percentagem_hidromorficos_brasil <- (area_total_hidromorficos / area_brasil) * 100

# Exibir os resultados
cat("Área total dos solos hidromórficos:", round(area_total_hidromorficos / 1000, 2), "milhares de ha\n")
cat("Área total do Brasil:", round(area_brasil / 1000, 2), "milhares de ha\n")
cat("Porcentagem dos solos hidromórficos em relação ao Brasil:", round(percentagem_hidromorficos_brasil, 4), "%\n")




#### Plotar o mapa com as classes hidromórficas filtradas e as áreas de inclusão 1

### SOLOS HIDROMÓRFICOS

png("solos_hidro_nivel2.png",
    width = 3000, height = 2000, res = 300) # Definir o arquivo PNG com alta resolução

# Plotar o mapa base do país com eixos, grade e posição da legenda
plot(
  st_geometry(country), 
  col = "white", 
  border = "gray", 
  main = "Solos Hidromórficos do Brasil (nivel 2)",
  axes = TRUE,              # Adiciona os eixos ao mapa
  graticule = TRUE,         # Adiciona linhas de grade (graticule)
  key.pos = 4               # Define a posição da legenda à direita
)



# Plotar solos hidromórficos
plot(
  st_geometry(filtered_soil),  
  border = FALSE,
  col = "#090748",                   
  add = TRUE                         
)

# Adicionar a legenda para a camada plotada
legend(
  "topright",                         # Posição da legenda no canto superior direito
  legend = c("2º Nível Categórico"),  # Descrição de filtered_soil
  fill = c("#090748"),                # Cor correspondente à camada
  border = "black",                   # Cor das bordas dos itens da legenda
  cex = 0.8,                          # Tamanho do texto da legenda
  bty = "n",                          # Sem borda ao redor da legenda
  y.intersp = 1.2                     # Espaçamento vertical entre os itens
)

# Finalizar a exportação do gráfico
dev.off()


### SOLOS HIDROMÓRFICOS + INCLUSÃO 1

png("solos_hidro_incl1.png",
    width = 3000, height = 2000, res = 300) # Definir o arquivo PNG com alta resolução

# Plotar o mapa base do país com eixos, grade e posição da legenda
plot(
  st_geometry(country), 
  col = "white", 
  border = "gray", 
  main = "Solos Hidromórficos do Brasil",
  axes = TRUE,              # Adiciona os eixos ao mapa
  graticule = TRUE,         # Adiciona linhas de grade (graticule)
  key.pos = 4               # Define a posição da legenda à direita
)

# Plotar áreas de inclusão 1
plot(
  st_geometry(filtered_feature_p1),  
  border = FALSE,
  col = "#433f8d",                   
  add = TRUE                         
)

# Plotar a camada de solos hidromórficos 2 nivel
plot(
  st_geometry(filtered_soil),  
  border = FALSE,
  col = "#090748",                   
  add = TRUE                         
)

# Adicionar a legenda para as camadas plotadas
legend(
  "topright",                         # Posição da legenda no canto superior direito
  legend = c("2º Nível Categórico",   # Descrição de filtered_soil
             "Inclusão (inclu_p1)"),   # Descrição de filtered_feature_p1
  fill = c("#090748", "#433f8d"),  # Cores correspondentes às camadas
  border = "black",                   # Cor das bordas dos itens da legenda
  cex = 0.8,                          # Tamanho do texto da legenda
  bty = "n",                          # Sem borda ao redor da legenda
  y.intersp = 1.2                     # Espaçamento vertical entre os itens
)

# Finalizar a exportação do gráfico
dev.off()


### SOLOS HIDROMÓRFICOS + INCLUSÃO 1 + INCLUSÃO 2 + INCLUSÃO 3

png("solos_hidro_incl1_2_3.png",
    width = 3000, height = 2000, res = 300) # Definir o arquivo PNG com alta resolução

# Plotar o mapa base do país com eixos, grade e posição da legenda
plot(
  st_geometry(country), 
  col = "white", 
  border = "gray", 
  main = "Solos Hidromórficos do Brasil",
  axes = TRUE,              # Adiciona os eixos ao mapa
  graticule = TRUE,         # Adiciona linhas de grade (graticule)
  key.pos = 4               # Define a posição da legenda à direita
)


# Plotar áreas de inclusão 2
plot(
  st_geometry(filtered_feature_p2),  
  border = FALSE,
  col = "#91919b",                   
  add = TRUE                         
)


# Plotar áreas de inclusão 1
plot(
  st_geometry(filtered_feature_p1),  
  border = FALSE,
  col = "#433f8d",                   
  add = TRUE                         
)

# Adicionar a camada de solos hidromórficos sobre o mapa existente
plot(
  st_geometry(filtered_soil),        
  border = FALSE,                   
  col = "#090748",                  
  add = TRUE                       
)

# Adicionar a legenda para as camadas plotadas
legend(
  "topright",                         # Posição da legenda no canto superior direito
  legend = c("2º Nível Categórico",   # Descrição de filtered_soil
             "Inclusão (inclu_p1)",   # Descrição de filtered_feature_p1
             "Inclusão (inclu_p2)"),  # Descrição de filtered_feature_p2
  fill = c("#090748", "#433f8d", "#91919b"),  # Cores correspondentes às camadas
  border = "black",                   # Cor das bordas dos itens da legenda
  cex = 0.8,                          # Tamanho do texto da legenda
  bty = "n",                          # Sem borda ao redor da legenda
  y.intersp = 1.2                     # Espaçamento vertical entre os itens
)

# Finalizar a exportação do gráfico
dev.off()


####