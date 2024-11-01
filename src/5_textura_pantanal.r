# Textura do solo (0-30 cm)


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
if(!require(raster)){
  install.packages("raster")
}
if (!require(viridis)) {
  install.packages("viridis")
}

# Carregar os rasters e shapes
bioma <- sf::read_sf("data/limites/biomas_2019.geojson")
agua <- sf::read_sf("data/limites/ana/Curso_d'µgua.shp")
raster_textura <- raster("results/textura/textura_30cm_pantantal.tif")
raster_areia <- raster("results/textura/areia_pantanal.tif")

# Definir a região de interesse
pantanal <- bioma[bioma$Bioma == "Pantanal", ]
raster_areia <- mask(raster_areia, pantanal)
textura_pantanal <- mask(raster_textura, pantanal)

# Definir os rios específicos que devem ser coloridos em azul
specific_rivers <- c("Rio Cuiabá ou Cuiabazinho", "São Lourenzo", "Rio Taquari", "Rio Paraguai", "Rio Correntes")
# Definir cores para os rios com base no nome (HDS_NM)
agua_colors <- ifelse(agua$HDS_NM %in% specific_rivers, "blue", "cyan")

# Definir a paleta de cores para o teor de areia
colors <- colorRampPalette(c("yellow", "orange", "red", "darkred"))(100)

x11()
png("results/figures/areia_30cm_pantanal_rios.png",
  width = 2600, height = 2500, res = 300) # Define arquivo PNG

# Plotar o mapa base do Pantanal
plot(st_geometry(pantanal),
  col = "white",
  border = "darkgray",
  axes = TRUE,
  graticule = TRUE,
  main = "Teor de areia no solo (0-30 cm) no Pantanal")

# Plotar o raster de areia
plot(raster_areia, add = TRUE, col = colors)

# Plotar a camada de água usando a propriedade HDS_NM
plot(st_geometry(agua), add = TRUE, col = agua_colors, lwd = 2)  # lwd ajusta a largura da linha

# Adicionar legenda personalizada com Classe dentro do mapa
legend("bottomright", 
       legend = c(specific_rivers, "Outros rios"),
       fill = c(rep("blue", length(specific_rivers)), "cyan"),
       title = "Rios",
       cex = 0.7,  # Reduzir o tamanho da fonte da legenda
       bty = "n")  # Remover a caixa ao redor da legenda

# Adicionar escala
raster::scalebar(d = 1000, xy = c(-73, -30), type = "bar", divs = 8, below = "km")

# Adicionar observação na parte inferior do mapa
mtext("Versão beta dos mapas de granulometria do MapBiomas solo e Plano de Recursos Hídricos da Região Hidrográfica do Paraguai (PRH)\nCódigos e definições estão disponíveis em: https://github.com/taciaraz/solos_hidromorficos_br",
      side = 1, line = 3, cex = 0.6, adj = 0)

dev.off()
x11()


###### classe textural


x11()
# Plotar o raster de textura
plot(as.factor(textura_pantanal), col = colors, legend = FALSE)

# Adicionar legenda personalizada com Classe dentro do mapa
legend("topright", 
       legend = texture_classes$Classe,
       fill = texture_classes$Cor,
       title = "Classes de Textura",
       cex = 0.8,  # Diminuir o tamanho da fonte da legenda
       y.intersp = 1.2,  # Aumentar o espaçamento vertical entre os itens da legenda
       title.adj = 0.05,
       bty = "n")  # Remover a caixa ao redor da legenda

# legenda
# Criar um data frame com as classes texturais baseadas no triângulo textural e suas cores correspondentes
texture_legend_triangular <- data.frame(
  Classe = c("Areia", "Franco Arenoso", "Franco", 
             "Franco Argiloso", "Argila Franco-Arenosa", "Argila"),
  Definicao = c(
    "Solo com mais de 85% de areia",
    "Solo com 70-85% de areia e 10-20% de argila",
    "Solo com proporções equilibradas de areia, silte e argila (25-50%)",
    "Solo com 35-50% de argila e proporções moderadas de areia e silte",
    "Solo com 35-60% de argila e 20-35% de areia",
    "Solo com mais de 60% de argila"
  ),
  Cor = c("#F2BC59", "#FFD9A0", "#AD9DD0", 
          "#FF5757", "#94350C", "#701F09")
)

# Concatenar Classe e Definicao para a legenda e quebrar linhas para limitar a largura
legend_labels <- paste(texture_legend_triangular$Classe, "-", texture_legend_triangular$Definicao)
wrapped_legend_labels <- sapply(legend_labels, function(x) paste(strwrap(x, width = 25), collapse = "\n"))

x11()
png("results/figures/textura_30cm_pantanal_rios.png",
  width = 2600, height = 2500, res = 300) # Define arquivo PNG

# Plotar o mapa base do Pantanal
plot(st_geometry(pantanal),
  col = "white",
  border = "darkgray",
  axes = TRUE,
  graticule = TRUE,
  xlim = c(-60, -50),  # Ajustar os limites do eixo x
  main = "Textura do solo (0-30 cm) no Pantanal")

# Clip the raster by the Pantanal region
raster_textura_clipped <- mask(raster_textura, pantanal)

# Definir a paleta de cores
colors <- texture_legend_triangular$Cor

# Plotar o raster de textura sem a legenda numérica
plot(as.factor(raster_textura_clipped), add = TRUE, col = colors, legend = FALSE)


# Adicionar legenda personalizada com Classe e Definicao dentro do mapa

legend("topright", 
       legend = wrapped_legend_labels,
       fill = texture_legend_triangular$Cor,
       title = "Classes de Textura",
       cex = 0.7,  # Reduzir o tamanho da fonte da legenda
       bty = "n")  # Remover a caixa ao redor da legenda


# Plotar a camada de água usando a propriedade HDS_NM
plot(st_geometry(agua), add = TRUE, col = agua_colors, lwd = 1)  # lwd ajusta a largura da linha

# Adicionar legenda personalizada com Classe dentro do mapa
legend("bottomright", 
       legend = c(specific_rivers, "Outros rios"),
       fill = c(rep("blue", length(specific_rivers)), "cyan"),
       title = "Rios",
       cex = 0.7,  # Reduzir o tamanho da fonte da legenda
       bty = "n")  # Remover a caixa ao redor da legenda


# Adicionar observação na parte inferior do mapa
mtext("Versão beta dos mapas de granulometria do MapBiomas solo e Plano de Recursos Hídricos da Região Hidrográfica do Paraguai (PRH)\nCódigos e definições estão disponíveis em: https://github.com/taciaraz/solos_hidromorficos_br",
      side = 1, line = 3, cex = 0.6, adj = 0)


dev.off()

x11()

# Transformar o raster de textura em um shapefile conforme a classificação da legenda

# Reclassificar o raster de textura com base nas classes definidas
reclass_matrix <- matrix(c(
  1, 1,  # Areia
  2, 2,  # Franco Arenoso
  3, 3,  # Franco
  4, 4,  # Franco Argiloso
  5, 5,  # Argila Franco-Arenosa
  6, 6   # Argila
), ncol = 2, byrow = TRUE)

raster_reclassified <- reclassify(raster_textura_clipped, reclass_matrix)

# Converter o raster reclassificado em um shapefile
shape_textura <- rasterToPolygons(raster_reclassified, dissolve = TRUE)

# Adicionar a coluna de classe ao shapefile
shape_textura$Classe <- factor(shape_textura$layer, 
                               levels = 1:6, 
                               labels = texture_legend_triangular$Classe)

# Salvar o shapefile
sf::st_write(st_as_sf(shape_textura), "results/shapefiles/textura_30cm_pantanal.shp")
