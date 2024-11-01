#######
# Plot the soil map using the SIBCS legend

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


# Load the soil classes legend
soil_map <- sf::read_sf("data_soil/pedo_area.shp")

# Load necessary libraries
library(sf)
library(dplyr)
library(stringr)


# Prepare the color mapping dataframe
soil_legend2 <- data.frame(
  Class = c(
    "ARGISSOLO BRUNO-ACINZENTADO","ARGISSOLO ACINZENTADO","ARGISSOLO AMARELO","ARGISSOLO VERMELHO","ARGISSOLO VERMELHO-AMARELO",
    "CAMBISSOLO HÚMICO","CAMBISSOLO FLÚVICO","CAMBISSOLO HÁPLICO",
    "CHERNOSSOLO RÊNDZICO","CHERNOSSOLO EBÂNICO","CHERNOSSOLO ARGILÚVICO","CHERNOSSOLO HÁPLICO",
    "ESPODOSSOLO HUMILÚVICO","ESPODOSSOLO FERRILÚVICO","ESPODOSSOLO FERRI-HUMILÚVICO",
    "GLEISSOLO TIOMÓRFICO","GLEISSOLO SÁLICO","GLEISSOLO MELÂNICO","GLEISSOLO HÁPLICO",
    "LATOSSOLO BRUNO","LATOSSOLO AMARELO","LATOSSOLO VERMELHO","LATOSSOLO VERMELHO-AMARELO",
    "LUVISSOLO CRÔMICO","LUVISSOLO HÁPLICO",
    "NEOSSOLO LITÓLICO","NEOSSOLO FLÚVICO","NEOSSOLO REGOLÍTICO","NEOSSOLO QUARTZARÊNICO",
    "NITOSSOLO BRUNO","NITOSSOLO VERMELHO","NITOSSOLO HÁPLICO",
    "VERTISSOLO HÁPLICO","VERTISSOLO EBÂNICO","VERTISSOLO HIDROMÓRFICO",
    "PLINTOSSOLO HÁPLICO","PLINTOSSOLO ARGILÚVICO", "PLINTOSSOLO PÉTRICO",
    "PLANOSSOLO HÁPLICO","PLANOSSOLO NÁTRICO",
    "ORGANOSSOLO HÁPLICO","ORGANOSSOLO FÓLICO", "ORGANOSSOLO TIOMÓRFICO",
    "ÁREA URBANA","AFLORAMENTO", "DUNAS", "CORPO D'ÁGUA CONTINENTAL"
  ),
  Color = c(
# ARGISSOLO BRUNO-ACINZENTADO", "ARGISSOLO ACINZENTADO"
# ARGISSOLO AMARELO",  "ARGISSOLO VERMELHO", "ARGISSOLO VERMELHO-AMARELO",
    "#FFC8FA", "#FDF1F0", "#F1CCC8", "#F07F7F", "#FFA77F",
# "CAMBISSOLO HÚMICO","CAMBISSOLO FLÚVICO","CAMBISSOLO HÁPLICO",
    "#CFB680", "#EBDBBF","#D7C5A5",
# "CHERNOSSOLO RÊNDZICO","CHERNOSSOLO EBÂNICO","CHERNOSSOLO ARGILÚVICO","CHERNOSSOLO HÁPLICO",
    "#8E6856", "#AA8686", "#9C4A4E", "#A86A70",
# "ESPODOSSOLO HUMILÚVICO","ESPODOSSOLO FERRILÚVICO","ESPODOSSOLO FERRI-HUMILÚVICO",
    "#9AAABE", "#B6BEC5", "#9AA8BC",
# "GLEISSOLO TIOMÓRFICO","GLEISSOLO SÁLICO","GLEISSOLO MELÂNICO","GLEISSOLO HÁPLICO",
    "#6CA3CD", "#5EB4E6", "#78FCFA", "#B6D8EE",
# "LATOSSOLO BRUNO","LATOSSOLO AMARELO","LATOSSOLO VERMELHO","LATOSSOLO VERMELHO-AMARELO",
    "#A87000", "#FEC05C", "#F4B980", "#F7D1A6",
# "LUVISSOLO CRÔMICO","LUVISSOLO HÁPLICO",
    "#D49616", "#D7B27A",
# "NEOSSOLO LITÓLICO","NEOSSOLO FLÚVICO","NEOSSOLO REGOLÍTICO","NEOSSOLO QUARTZARÊNICO",
    "#969595", "#EEEBDC", "#CFCFCE", "#FFFE73",
# "NITOSSOLO BRUNO","NITOSSOLO VERMELHO","NITOSSOLO HÁPLICO", 
    "#68350A", "#A83800", "#734C00",
# "VERTISSOLO HÁPLICO","VERTISSOLO EBÂNICO","VERTISSOLO HIDROMÓRFICO",
    "#C0C091", "#868F72", "#9EAA85",
# "PLINTOSSOLO HÁPLICO","PLINTOSSOLO ARGILÚVICO","PLINTOSSOLO PÉTRICO",
    "#D6BAC9", "#E3B3CD", "#ECACCB", 
# "PLANOSSOLO HÁPLICO","PLANOSSOLO NÁTRICO",
    "#B5D6AE", "#89CAC7",
# "ORGANOSSOLO HÁPLICO","ORGANOSSOLO FÓLICO","ORGANOSSOLO TIOMÓRFICO",
    "#A7B3D5", "#8596C1", "#5E81A1",
# "ÁREA URBANA","AFLORAMENTO","DUNAS","CORPO D'ÁGUA CONTINENTAL"
    "red", "white", "white", "blue"
  ),
  stringsAsFactors = FALSE
)

# Clean and prepare the 'legenda_2' in soil_map for merging
#soil_map$legenda_2 <- str_trim(soil_map$legenda_2) %>% str_to_upper()

# Ensure 'Class' column in soil_legend2 matches the format
#soil_legend2$Class <- str_trim(soil_legend2$Class) %>% str_to_upper()

# Check for mismatched values
missing_classes <- setdiff(unique(soil_map$legenda_2), unique(soil_legend2$Class))
print(missing_classes)  # This will show values that don't match


# Calcular a frequência de cada categoria para garantir que apenas categorias presentes sejam usadas na legenda
category_freq <- soil_map %>%
  as.data.frame() %>%
  count(legenda_2, Color) %>%
  filter(n > 0)  # Filtra apenas as categorias que aparecem no mapa


# Merge the color information into soil_map based on 'legenda_2'
soil_map <- left_join(soil_map, soil_legend2, by = c("legenda_2" = "Class"))

# Verify the merged Color column
head(soil_map$Color)


#######
# Plot the south_america map, brazil contourn and the soil classes
# Exportar o gráfico em alta resolução
png("results/solos_brasil_legenda2.png",
width = 3000, height = 2000, res = 300) # Definir o arquivo PNG com alta resolução


# Plot the shapefile based on 'legenda_2'
plot(soil_map["legenda_2"], border =  FALSE,
     main = "Solos do Brasil (SIBCS 2º nível categórico)", 
     axes = TRUE, 
     col = soil_map$Color, 
     graticule = TRUE, 
     key.pos = 4)  # `key.pos` determines where the legend is placed
     #(4 is for the right side)


# Add the legend directly on the plot
legend(
  "topright",                               # Posicione a legenda no canto superior direito (ajuste conforme necessário) # nolint: line_length_linter.
  legend = category_freq$legenda_2,         # Usa os rótulos das categorias presentes no mapa # nolint
  fill = category_freq$Color,               # Usa as cores correspondentes a essas categorias
  title = "Legenda",
  title.adj = 0.1,                         # Ajuste da posição do título
  cex = 0.7,                                # Ajuste do tamanho do texto
  bty = "n",                                # Sem borda
  y.intersp = 0.8,                           # Espaçamento vertical ajustado
  x.intersp = 0.5,                       # Adjusts horizontal spacing inside the legend
  inset = c(-0.02, -0.02),               # Slightly moves the legend further right and up
  xpd = TRUE                             # Allows legend to be placed outside the plot area
)

dev.off()