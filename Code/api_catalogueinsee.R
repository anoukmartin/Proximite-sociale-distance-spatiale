library(httr)
library(jsonlite)
library(dplyr)

# Récupération des données JSON via l'API de l'INSEE
api_url <- "https://api.insee.fr/melodi/catalog/all"
res <- GET(api_url)
json_raw <- rawToChar(res$content)

# Conversion de l'objet JSON en une structure R en aplatissant les sous-niveaux
json_data <- fromJSON(json_raw, flatten = TRUE)

# Conversion en dataframe (chaque élément représente une ligne)
df <- as.data.frame(json_data)

# Fonction pour aplatir récursivement les colonnes imbriquées
flatten_recursive <- function(data) {
  # Tant qu'une ou plusieurs colonnes sont des listes ou dataframes,
  # on essaie de les développer
  while(any(sapply(data, function(x) is.list(x) || is.data.frame(x)))) {
    # Sélectionner les colonnes à déplier
    cols_to_unnest <- names(data)[sapply(data, function(x) is.list(x) || is.data.frame(x))]
    # Pour chacune, on utilise unnest_wider qui créera des colonnes supplémentaires
    data <- data %>%
      unnest_wider(all_of(cols_to_unnest), names_sep = ".")
  }
  data
}

# Application de l'aplatissement récursif
df_flat <- flatten_recursive(df)

# Affichage des premières lignes du dataframe aplati
head(df_flat)
names(df_flat)
df_french <- df_flat %>%
  select(-ends_with("lang.2")) %>%
  select(-ends_with("content.2")) %>%
  select(-ends_with("lang.1"))
names(df_french) <- str_remove_all(names(df_french), ".content.1")

catalogue <- df_french
rm(df, df_flat, df_french, json_data, res, api_url, json_raw, flatten_recursive)
