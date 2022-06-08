library(osrm)
library(xtradata)
library(leaflet)

equipements <- xtradata_requete_features(key = Sys.getenv('XTRADATA_KEY'),
                                         typename = "TO_EQPUB_P",
                                         filter = list("theme" = "A"))


iso <- osrmIsochrone(equipements[1,], breaks = c(5,10,15), osrm.profile = c("bike"))

leaflet() %>% 
  addTiles() %>% 
  addMarkers(data = equipements[1,]) %>% 
  addPolygons(data = iso)




https://data.bordeaux-metropole.fr/wps?key=DATAZBOUBB&service=wps&version=1.0.0&request=execute&identifier=geocodeur&datainputs=input=35%20rue%20neuve,%20Bordeaux

https://data.bordeaux-metropole.fr/wps?key=INTRANEUSR&service=wps&version=1.0.0&request=describeprocess&identifier=geocodeur

input
commune
limit
crs
property

library(tibble)
library(tidyr)
library(dplyr)

url <- "https://data.bordeaux-metropole.fr/wps?key=INTERNEUSR&service=wps&version=1.0.0&request=execute&identifier=geocodeur&datainputs=input=35%20rue%20neuve,%20Bordeaux"

res_xml <- read_xml(url)

list_res_xml <- as_list(res_xml)

tb <-  tibble::as_tibble(list_res_xml)

tb %>% unnest_longer(ExecuteResponse) %>% filter(ExecuteResponse_id == "Output") -> aaa
aaa %>% unnest_wider(ExecuteResponse) %>% unnest_wider(Data) %>% unnest_wider(ComplexData) %>% unnest_wider(featureMember) %>% unnest_wider(default) %>% unnest(cols = names(.)) %>% unnest(cols = names(.)) -> bbb
bbb %>% unnest(geometry) %>% unnest(geometry) -> ccc

ccc$geometry
ccc$geometry[1] %>% stringr::str_split_fixed(string = ., pattern = " ", n = 2)
ccc$geometry[1] %>% stringr::str_extract_all(string = ., pattern  = "[:digit:]{1,}\\.[:digit:]{1,}") %>% unlist() %>% rbind


##
tb %>% unnest_longer(ExecuteResponse) %>% filter(ExecuteResponse_id == "Output") -> aaa
aaa[5,] %>% unnest_wider(ExecuteResponse) %>% unnest_wider(Data) %>% unnest_wider(ComplexData) %>% unnest_wider(featureMember) %>% unnest_wider(default) %>% unnest(cols = names(.)) %>% unnest(cols = names(.))
##



tb2 <- tb %>% mutate(xml_levels = as.vector(sapply(list_res_xml, names)))

tb3 <- tb2 %>% filter(xml_levels == "ProcessOutputs")

tb4 <- unnest_longer(tb3, "ExecuteResponse")

tb5 <- tb4[4,] %>% select(ExecuteResponse) %>% sapply(., "[")

tb2[5,] %>% dplyr::select(ExecuteResponse) %>% unnest("ExecuteResponse") %>% unnest_wider(ExecuteResponse) %>% dplyr::select(ComplexData) -> a

sapply(a[4,], "[")


mp <- tb %>% unnest_wider(ExecuteResponse) %>% select(contains("Output"))

mp <- aaa %>% filter(ExecuteResponse_id == "Output")  %>% select(-ExecuteResponse_id) %>% rownames_to_column() %>% pivot_longer(-rowname) %>% pivot_wider(names_from = rowname, values_from = value) %>% select(-name)
map(mp, function(.x) {
  browser()
})

tb %>% unnest_longer(ExecuteResponse) %>% filter(ExecuteResponse_id == "Output") -> aaa
aaa[5,] %>% unnest_wider(ExecuteResponse) %>% unnest_wider(Data) %>% unnest_wider(ComplexData) %>% unnest_wider(featureMember) %>% unnest_wider(default) %>% unnest(cols = names(.)) %>% unnest(cols = names(.))

aaa %>% unnest_wider(ExecuteResponse) %>% unnest_wider(Data) %>% unnest_wider(ComplexData) %>% unnest_wider(featureMember) %>% unnest_wider(default) %>% unnest(cols = names(.)) %>% unnest(cols = names(.)) -> bbb
bbb %>% unnest(geometry) %>% unnest(geometry) -> ccc

ccc$geometry
ccc$geometry[1] %>% stringr::str_split_fixed(string = ., pattern = " ", n = 2)
ccc$geometry[1] %>% stringr::str_extract_all(string = ., pattern  = "[:digit:]{1,}\\.[:digit:]{1,}") %>% unlist() %>% rbind

list_res_xml <- as_list(res_xml)

a <- tibble::as_tibble(list_res_xml) %>% tidyr::unnest_longer('ExecuteResponse')
b <- tibble::as_tibble(list_res_xml) %>% tidyr::unnest_wider('ExecuteResponse')

tb <-  tibble::as_tibble(list_res_xml)
tb <- tb %>% mutate(xml_levels = as.vector(sapply(list_res_xml, names)))

tb2 <- filter(tb, xml_levels == "ProcessOutputs")


tb2 <- tb %>% unnest_longer("ExecuteResponse")


tb2[5,] %>% dplyr::select(ExecuteResponse) %>% unnest("ExecuteResponse") %>% unnest_wider(ExecuteResponse)

df_driver = b %>% unnest(cols = names(.)) %>% unnest(cols = names(.)) %>% readr::type_convert()

a %>% dplyr::filter(ExecuteResponse_id %in% "Output") %>% unnest(col = ExecuteResponse) %>% unnest(col = ExecuteResponse)

##

df <- read_xml(url)

xml_find_all(df, xpath = ".//Output")
driver_id <- xml_attr(driver, "driverId")
url_name <- xml_attr(driver, "url")


library(curl)
library(httr)

curl::curl("https://data.bordeaux-metropole.fr/wps?key=DATAZBOUBB&service=wps&version=1.0.0&request=execute&identifier=geocodeur&datainputs=input=35%20rue%20neuve,%20Bordeaux")



request <- curl_fetch_memory(url)
response <- rawToChar(request$content)

df <- try(geojson_sf(response), silent = TRUE)

library(xml2)

url <- "http://data.bordeaux-metropole.fr/wps?key=DATAZBOUBB&service=wps&version=1.0.0&request=execute&identifier=geocodeur&datainputs=input=35%20rue%20neuve,%20Bordeaux"

xml_find_all(df, ".//bm:GID") %>% xml_text()
xml_find_all(df, ".//bm")
