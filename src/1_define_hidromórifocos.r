# Define solos hidromórifocos
#######
# R-Packages
# Check if the sf, data.table, and RColorBrewer packages are installed.
# If not, install them.
if (!require(sf)) {
  install.packages("sf")
}
if(!require(dplyr)){
  install.packages("dplyr")
}

# Load geospatial data
soil_map <- sf::read_sf("data/ibge/pedo_area.shp")
# Identifica as classes únicas que aparecem na legenda do mapa
unique(soil_map$legenda)


# Solos hidromórficos
# Lista contendo o nome da classes de solos
# (segundo nível categórico, legenda_2)
# que explicitamente referem-se a solos hidromórficos
hidro_class <- c(
  "NEOSSOLO FLÚVICO",  "CAMBISSOLO FLÚVICO",
  "PLINTOSSOLO ARGILÚVICO", "PLINTOSSOLO HÁPLICO",
  "PLANOSSOLO HÁPLICO", "PLANOSSOLO NÁTRICO",
  "GLEISSOLO HÁPLICO", "GLEISSOLO SÁLICO", "GLEISSOLO MELÂNICO", "GLEISSOLO TIOMÓRFICO", # nolint: line_length_linter.
  "ORGANOSSOLO TIOMÓRFICO", "ORGANOSSOLO HÁPLICO", "ORGANOSSOLO FÓLICO",
  "ESPODOSSOLO FERRILÚVICO", "ESPODOSSOLO HUMILÚVICO", "ESPODOSSOLO FERRI-HUMILÚVICO", # nolint: line_length_linter.
  "VERTISSOLO HIDROMÓRFICO",
  "NEOSSOLO QUARTZARÊNICO HIDROMÓRFICO" #único que está no nível geral (legenda)
)


# FILTRAR ÁREAS DE INCLUSÃO
# Criar um vetor com os termos que indicam solos hidromórficos
# que incluem a palavra "hidromórfico" em alguma classe de inclusão na legenda

hidro_features <- c(
  "HIDROMÓRFICO",
  "HIDRO",
  "NEOSSOLO QUARTZARÊNICO HIDROMÓRFICO",
  "NEOSSOLO FLÚVICO",  "CAMBISSOLO FLÚVICO",
  "PLINTOSSOLO ARGILÚVICO", "PLINTOSSOLO HÁPLICO",
  "PLANOSSOLO",
  "GLEISSOLO", # nolint: line_length_linter.
  "ORGANOSSOLO",
  "ESPODOSSOLO"# nolint: line_length_linter.
)


# Cria a expressão regular
# Combina os termos com "|"
# ela busca a presença de qualquer termo de `hidro_features`
hidro_features <- paste(hidro_features, collapse = "|")
print(hidro_features)

# Salvar os vetores hidro_class e hidro_features para uso em outro código
saveRDS(hidro_class, file = "results/definicoes/hidro_class.rds")
saveRDS(hidro_features, file = "results/definicoes/hidro_features.rds")
