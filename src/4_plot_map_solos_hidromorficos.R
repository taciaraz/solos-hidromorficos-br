
# SUMMARY
# Mapa de classes de solo do SIBCS - IBGE 2024

# Mapa de solos hidromórficos do Brasil
# Taciara Zborowski Horst

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
if(!require(dplyr)){
  install.packages("dplyr")
}
if(!require(stringr)){
  install.packages("stringr")
}

#######
# Load geospatial data
soil_map <- sf::read_sf("data/ibge/pedo_area.shp")
country <- sf::read_sf("data/limites/country2020.geojson")

# Importar legenda
legenda <- readRDS('data/ibge/soil_classes_legend.rds')

# Definir cores com base no objeto legenda
colors <- legenda$Color
names(colors) <- legenda$Class

# Verificar valores únicos em filtered_soil$legenda_2
unique_legenda_2 <- unique(filtered_soil$legenda_2)
# Filtrar legenda para incluir apenas os valores existentes em legenda_2
filtered_legenda <- legenda[legenda$Class %in% unique_legenda_2, ]

# Adicionar nova entrada à legenda
new_legend_entry <- data.frame(
  Class = "INCLUSÕES",
  Color = "#676876"
)

# Combinar a nova entrada com a legenda existente
filtered_legenda_incl <- rbind(filtered_legenda, new_legend_entry)

# Importar arquivos de feicoes hidromórficas em diferentes niveis da legenda
filtered_soil <- readRDS("results/definicoes/filtered_soil_legenda.rds")
filtered_feature_p1 <- readRDS("results/definicoes/filtered_soil_inclu_1.rds")
filtered_feature_p2 <- readRDS("results/definicoes/filtered_soil_inclu_2.rds")

# Mapa indicador
png("results/figures/solos_hidromorficos_area_indicacao_inclusao.png",
    width = 2600, height = 2500, res = 300) # Define arquivo PNG

# Plotar o mapa base
plot(st_geometry(country),
  col = "white",
  border = "darkgray",
  axes = TRUE,
  graticule = TRUE,
  main = "Área de solos hidromórficos no Brasil")

plot(
  filtered_feature_p1["legenda_2"],
  col = "#676876",
  border = FALSE,
  add = TRUE
)

plot(
  filtered_feature_p2["legenda_2"],
  col = "#676876",
  border = FALSE,
  add = TRUE
)

plot(
  filtered_soil["legenda_2"],
  col = "#2b2d5c",
  border = FALSE,
  add = TRUE
)

# Adicionar escala
raster::scalebar(d = 1000, xy = c(-73, -30), type = "bar", divs = 8, below = "km")
# Adicionar norte
raster::north.arrow(x = -70, y = -30, len = 0.1, col = "black")

# Definir classes e cores manualmente
classes <- c("Unidade taxonômica", "Área de inclusão")
#classes <- c("Unidade taxonômica = 113,5 Mha", "Área de inclusão =  104,7 Mha")
cores <- c("#2b2d5c", "#676876")  # Cores correspondentes às classes

# Adicionar legenda ao lado do mapa, incluindo a área de inclusão
legend("bottomright",
  legend = classes,  # Classes definidas manualmente
  fill = cores,      # Cores definidas manualmente
  title = "Legenda",
  title.adj = 0.05,
  cex = 1)

# Adicionar observação na parte inferior do mapa
mtext("Estimativa calculada com base no Mapa de pedologia do IBGE, disponível no BDIA.\nCódigos e definições estão disponíveis em: https://github.com/taciaraz/solos_hidromorficos_br",
      side = 1, line = 3, cex = 0.6, adj = 0)

dev.off()

####################################################
# sem area de inclusao 

png("results/figures/solos_hidromorficos_legenda_seminclusao.png",
    width = 2600, height = 2500, res = 300) # Define arquivo PNG

# Plotar o mapa base
plot(st_geometry(country),
  col = "white",
  border = "darkgray",
  axes = TRUE,
  graticule = TRUE,
 main = "Mapa de solos hidromórficos do Brasil")

# Plotar as classes hidromórficas com base no atributo legenda_2
plot(
  filtered_soil["legenda_2"],
  col = colors[filtered_soil$legenda_2],
  border = colors[filtered_soil$legenda_2],  # Set border color to match fill color
  lwd = 0.8,
  add = TRUE
)

# Adicionar escala
raster::scalebar(d = 1000, xy = c(-73, -30), type = "bar", divs = 8, below = "km")
# Adicionar norte

# Adicionar legenda ao lado do mapa
legend("bottomright",
  legend = filtered_legenda$Class,
  fill = filtered_legenda$Color,
  title = "Legenda",
  title.adj = 0.05,
  cex = 0.6)

# Adicionar observação na parte inferior do mapa
mtext("Estimativa calculada com base no Mapa de pedologia do IBGE, disponível no BDIA.\nCódigos e definições estão disponíveis em: https://github.com/taciaraz/solos_hidromorficos_br",
      side = 1, line = 3, cex = 0.6, adj = 0)

dev.off()
x11()
# COM INCLUSAO
#########################################
png("results/figures/solos_hidromorficos_legenda.png",
    width = 2600, height = 2500, res = 300) # Define arquivo PNG

# Plotar o mapa base
plot(st_geometry(country),
  col = "white",
  border = "darkgray",
  axes = TRUE,
  graticule = TRUE,
  main = "Mapa de solos hidromórficos do Brasil")

plot(
  filtered_feature_p1["legenda_2"],
  col = "#676876",
  border = FALSE,
  add = TRUE
)

plot(
  filtered_feature_p2["legenda_2"],
  col = "#676876",
  border = FALSE,
  add = TRUE
)

# Plotar as classes hidromórficas com base no atributo legenda_2
plot(
  filtered_soil["legenda_2"],
  col = colors[filtered_soil$legenda_2],
  border = colors[filtered_soil$legenda_2],  # Set border color to match fill color
  lwd = 0.8,
  add = TRUE
)

# Adicionar escala
raster::scalebar(d = 1000, xy = c(-73, -30), type = "bar", divs = 8, below = "km")
# Adicionar norte

# Adicionar legenda ao lado do mapa
legend("bottomright",
  legend = filtered_legenda_incl$Class,
  fill = filtered_legenda_incl$Color,
  title = "Legenda",
  title.adj = 0.05,
  cex = 0.6)


# Adicionar observação na parte inferior do mapa
mtext("Estimativa calculada com base no Mapa de pedologia do IBGE, disponível no BDIA.\nCódigos e definições estão disponíveis em: https://github.com/taciaraz/solos_hidromorficos_br",
      side = 1, line = 3, cex = 0.6, adj = 0)

dev.off()
x11()

unique(filtered_soil$legenda)
unique(filtered_feature_p1$legenda_2)


# Escrever os vetores em shapefile
sf::write_sf(filtered_soil, "results/shapefiles/filtered_soil.shp")
sf::write_sf(filtered_feature_p1, "results/shapefiles/filtered_feature_p1.shp")
sf::write_sf(filtered_feature_p2, "results/shapefiles/filtered_feature_p2.shp")


###################################

# mapa do pantanal


# Carregar dados do Pantanal
biomas <- sf::read_sf("data/limites/biomas_2019.geojson")

# Filtrar dados de solo para o Pantanal
pantanal <- biomas[biomas$Bioma == "Pantanal", ]

# Ensure both spatial objects have the same CRS
pantanal <- sf::st_transform(pantanal, sf::st_crs(filtered_soil))

# Validate and simplify geometries
pantanal <- sf::st_make_valid(pantanal)
filtered_soil <- sf::st_make_valid(filtered_soil)
pantanal <- sf::st_simplify(pantanal, dTolerance = 0.001)
filtered_soil <- sf::st_simplify(filtered_soil, dTolerance = 0.001)

# Perform the spatial intersection
pantanal_soil <- sf::st_intersection(filtered_soil, pantanal)

# filtrar legenda
legenda <- readRDS('data/ibge/soil_classes_legend.rds')

# Definir cores com base no objeto legenda
colors <- legenda$Color
names(colors) <- legenda$Class

# Verificar valores únicos em filtered_soil$legenda_2
unique_legenda_2 <- unique(filtered_soil$legenda_2)
# Filtrar legenda para incluir apenas os valores existentes em legenda_2
filtered_legenda <- legenda[legenda$Class %in% unique_legenda_2, ]



# Mapa do Pantanal

# CLASSE DE SOLO


png("results/figures/solos_hidromorficos_pantanal.png",
  width = 2600, height = 2500, res = 300) # Define arquivo PNG

# Plotar o mapa base do Pantanal
plot(st_geometry(pantanal),
  col = "white",
  border = "darkgray",
  axes = TRUE,
  graticule = TRUE,
  xlim = c(-60, -52),  # Ajustar os limites do eixo x
  main = "Área de solos hidromórficos no Pantanal")

plot(
  pantanal_soil["legenda_2"],
  col = colors[pantanal_soil$legenda_2],
  border = colors[pantanal_soil$legenda_2],  # Set border color to match fill color
  lwd = 0.8,
  add = TRUE
)

# Adicionar escala
raster::scalebar(d = 1000, xy = c(-73, -30), type = "bar", divs = 8, below = "km")
# Adicionar norte

# Adicionar legenda ao lado do mapa
legend("bottomright",
  legend = filtered_legenda$Class,
  fill = filtered_legenda$Color,
  title = "Legenda",
  title.adj = 0.05,
  cex = 0.6)


# Adicionar observação na parte inferior do mapa
mtext("Estimativa calculada com base na versão beta do MapBiomas solo.\nCódigos e definições estão disponíveis em: https://github.com/taciaraz/solos_hidromorficos_br",
      side = 1, line = 3, cex = 0.6, adj = 0)

dev.off()
