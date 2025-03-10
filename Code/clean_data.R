
x <- listdataset$identifier[2]
function(x){
  df <- read_csv2(paste0("Data/", x, "_data.csv")) 
  df <- df %>%
    filter(GEO_OBJECT == "COM")
  meta <- read_csv2(paste0("Data/", x, "_metadata.csv")) %>%
    filter(COD_VAR != "GEO")
  
  
  # Recodage complêt 
  
  # On suppose que df et meta sont déjà chargés dans votre environnement
  
  # 1. Construction du dictionnaire de variables : mapping code variable -> libellé variable
  var_dict <- meta %>%
    distinct(COD_VAR, LIB_VAR) %>%
    deframe()  # Crée un vecteur nommé, ex: c("AGE" = "Âge", "EMPFORM" = "Forme d'emploi", ...)
  
  # 2. Construction du dictionnaire de modalités : pour chaque variable, mapping code modalité -> libellé modalité
  modality_dict <- meta %>%
    select(COD_VAR, COD_MOD, LIB_MOD) %>%
    group_by(COD_VAR) %>%
    summarise(mapping = list(deframe(tibble(COD_MOD, LIB_MOD))), .groups = "drop") %>%
    deframe()  
  # Par exemple, modality_dict[["EMPFORM"]] pourra être :
  # c("1" = "Non Salariés", "2" = "Salariés", "_T" = "Total")
  
  # 3. Recodage des modalités dans df en utilisant le dictionnaire
  # On ne recode que les colonnes présentes dans le dictionnaire (les variables pour lesquelles on a des mappings)
  cols_to_recode <- intersect(names(df), names(modality_dict))
  
  df_recoded <- df %>%
    mutate(across(
      .cols = all_of(cols_to_recode),
      .fns = ~ {
        mapping <- modality_dict[[cur_column()]]
        # Pour chaque valeur, on cherche la correspondance dans mapping ;
        # si elle n'existe pas, on conserve la valeur d'origine.
        out <- mapping[.];
        out[is.na(out)] <- .[is.na(out)]
        out
      }
    ))
  
  # 4. Renommage des colonnes selon le dictionnaire de variables
  names(df_recoded) <- sapply(names(df_recoded), function(x) {
    if (x %in% names(var_dict)) var_dict[[x]] else x
  })
  
  saveRDS(df, )
}


library(dplyr)
library(purrr)


# On pivote les données pour être en format tidy : 
df_wide <- df_recoded %>%
  pivot_wider(
    id_cols = c("GEO", "Période temporelle"),
    names_from = c("Mesure du recensement",
                   "Âge", "Forme d'emploi",
                   "Activité économique des emplois",  
                   "Profession et catégorie socioprofessionnelle (PCS)",
                   "Sexe", "Fréquence"),   
    values_from = "OBS_VALUE", 
    values_fn = unique)
names(df_wide) <- str_remove_all(names(df_wide), "_Total")
}

ano <- df_recoded %>%
  dplyr::summarise(n = dplyr::n(), .by = c(GEO, 
                                           `Période temporelle`, 
                                           `Mesure du recensement`, 
                                           Âge, 
                                           "Forme d'emploi", 
                                           `Activité économique des emplois`,
                                           Fréquence,
                                           `Profession et catégorie socioprofessionnelle (PCS)`,
                                           Sexe)) |>
  dplyr::filter(n > 1L) 

unique(names(df_recoded))




### fin du test  ####
head(df)
meta

