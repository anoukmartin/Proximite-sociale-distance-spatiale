
library(dplyr)
library(httr2)
library(httr)
library(jsonlite)
library(stringr)

dir.create("Data")
dir.create("Data/brutes")

listdataset <- catalogue %>%
  filter(if_any(starts_with("spatialResolution.id"), ~ . == "COM"))



lapply(listdataset$identifier, function(x) {
  
  url <- paste0("https://api.insee.fr/melodi/file/", x, "/", x, "_CSV_FR")
  
  
  # Nommer le fichier zip téléchargé
  zip_file <- "data.zip"
  
  # Télécharger le fichier zip (mode binaire)
  download.file(url, destfile = zip_file, mode = "wb")
  
  # Lister tous les fichiers présents dans l'archive
  file_list <- unzip(zip_file, list = TRUE)$Name
  
  # Extraire tous les fichiers dans le dossier "Data" en supprimant la structure des dossiers
  unzip(zip_file, files = file_list, exdir = "Data/brutes")
  # Optionnel : supprimer le fichier zip téléchargé après extraction
  unlink(zip_file)
})

