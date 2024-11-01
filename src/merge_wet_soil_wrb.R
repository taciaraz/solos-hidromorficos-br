
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

if (!require(raster)) {
  install.packages("raster")
}
if (!require(pbapply)) {
  install.packages("pbapply")
}

# Importar e mesclar rasters de diferentes diretórios
merge_rasters <- function(directory) {
  files <- list.files(directory, pattern = "\\.tif$", full.names = TRUE)
  rasters <- lapply(files, raster::raster)
  merged <- do.call(raster::merge, rasters)
  return(merged)
}

# Mesclar rasters de cada diretório
merged_wrb_glei <- merge_rasters("wrb/glei")
str(merged_wrb_glei)

merged_wrb_plan <- merge_rasters("wrb/plan")
merged_wrb_stag <- merge_rasters("wrb/stag")
merged_wrb_wet <- merge_rasters("wrb/wet")

# Mesclar todos os rasters em um único raster
merged_wrb_glei <- do.call(raster::merge, list(merged_wrb_glei))
merged_wrb_plan <- do.call(raster::merge, list(merged_wrb_plan))
merged_wrb_stag <- do.call(raster::merge, list(merged_wrb_stag))
merged_wrb_wet <- do.call(raster::merge, list(merged_wrb_wet))

# Salvar o raster mesclado em um único arquivo TIFF
output_file <- "wrb/merged_brazil.tif"
raster::writeRaster(all_merged,
  filename = output_file,
  format = "GTiff", overwrite = TRUE)

# Plotar o raster mesclado
plot(merged_wrb_glei, main = "Mapa de Gleysols Mesclado")
dev.new()


str(merged_wrb_glei)
View(merged_wrb_glei)
