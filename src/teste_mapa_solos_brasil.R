
# SUMMARY
# Mapa de classes de solo do SIBCS - IBGE 2024
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
if (!require(dplyr)) {
  install.packages("dplyr")
}
if (!require(stringr)) {
  install.packages("stringr")
}

#######
# Load geospatial data
soil_map <- sf::read_sf("data/ibge/pedo_area.shp")
country <- sf::read_sf("data/country2020.geojson")
south_america <- sf::read_sf("data/south_america.geojson")

# Ver estrutura dos dados de solo
str(soil_map)

#######
# Plotar o mapa de solos do Brasil, sem definir legenda

# Exportar o gráfico em alta resolução
png("results/solos_brasil.png",
    width = 4000,
    height = 2000,
    res = 300) # Definir o arquivo PNG com alta resolução

# Plot the shapefile based on 'legenda_2'
plot(soil_map["legenda_2"], border =  FALSE,
     main = "Solos do Brasil (SIBCS 2º nível categórico)",
     axes = TRUE,
     graticule = TRUE,
     key.pos = 4)  # `key.pos` determines where the legend is placed
#(4 is for the right side)

dev.off()