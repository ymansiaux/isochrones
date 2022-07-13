# library(tibble)
# library(tidyr)
# library(dplyr)
# library(xml2)
# library(stringr)
# library(sf)
# 
# url <- "https://data.bordeaux-metropole.fr/wps?key=INTERNEUSR&service=wps&version=1.0.0&request=execute&identifier=geocodeur&datainputs=input=35%20rue%20neuve,%20Bordeaux"
# 
# res_xml <- read_xml(url)
# 
# list_res_xml <- as_list(res_xml)
# 
# tb <-  as_tibble(list_res_xml)
# 
# tb_clean <- tb %>% 
#   unnest_longer(ExecuteResponse) %>% 
#   filter(ExecuteResponse_id == "Output") %>% 
#   unnest_wider(ExecuteResponse) %>% 
#   unnest_wider(Data) %>% 
#   unnest_wider(ComplexData) %>% 
#   unnest_wider(featureMember) %>% 
#   unnest_wider(default) %>% 
#   unnest(cols = names(.)) %>% 
#   unnest(cols = names(.)) 
# 
# geom <- tb_clean %>% 
#   unnest(geometry) %>% 
#   unnest(geometry)
# 
# coordinates <-  str_split_fixed(string = geom$geometry, pattern = " ", n = 2)
# colnames(coordinates) <- c("x", "y")
# 
# sf_geoloc <- bind_cols(tb_clean, coordinates) %>% 
#   st_as_sf(., coords = c("x", "y"), crs = 3945) %>% 
#   st_transform(crs = 4326)
# 
# leaflet(sf_geoloc) %>% 
#   addTiles() %>% 
#   addMarkers()
# 
# sf_geoloc %>% slice_max(PERTINENCE)
# 
# faudrait uploader un csv
# faire la géoloc sur tous les points
# choisir une catégorie d'équipement
# appeler xtradata pour récupérer leur position
# calculer les isochrones et croiser avec les positions
# 
# marker bleu pour l'adresse
# marker rouge pour les équipements
# 
# 
# tb %>% unnest_longer(ExecuteResponse) %>% filter(ExecuteResponse_id == "Output") -> aaa
# aaa %>% unnest_wider(ExecuteResponse) %>% unnest_wider(Data) %>% unnest_wider(ComplexData) %>% unnest_wider(featureMember) %>% unnest_wider(default) %>% unnest(cols = names(.)) %>% unnest(cols = names(.)) -> bbb
# bbb %>% unnest(geometry) %>% unnest(geometry) -> ccc
# 
# ccc$geometry
# ccc$geometry[1] %>% stringr::str_split_fixed(string = ., pattern = " ", n = 2)
# 
# ccc$geometry[1] %>% stringr::str_split(string = ., pattern = " ")
# ## si GID = NA : pas de réponse
# 
# url <- "https://data.bordeaux-metropole.fr/wps?key=INTERNEUSR&service=wps&version=1.0.0&request=execute&identifier=geocodeur&datainputs=input=prout"


# url <- "https://data.bordeaux-metropole.fr/wps?key=INTERNEUSR&service=wps&version=1.0.0&request=execute&identifier=geocodeur&datainputs=input=35%20rue%20neuve,%2033000%20Bordeaux"
