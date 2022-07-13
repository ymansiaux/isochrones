## code to prepare `dicopub_TO_EQPUB_P` dataset goes here
library(xml2)
library(magrittr)
xml_dicopub_TO_EQPUB_P <-read_xml("https://data.bordeaux-metropole.fr/wps?key=INTERNEUSR&service=wps&version=1.0.0&request=execute&identifier=dico_couches&datainputs=couche=TO_EQPUB_P")

# les infos qu'on cherche sont dans les noeuds "bm:Attribut"

nodesAttribut <- xml_find_all(xml_dicopub_TO_EQPUB_P, ".//bm:Attribut")

# on cherche les catégories THEME et SOUS THEME
theme_ss_theme <- which((nodesAttribut %>% xml_attr("nom")) %in% c("THEME", "SSTHEME"))


dicopub_TO_EQPUB_P <- lapply(seq_len(length(theme_ss_theme)), function(i) {
  
  node <- nodesAttribut[[theme_ss_theme[i]]]
  
  df <- data.frame(
    "value" = xml_child(node) %>% xml_children() %>% xml_attr("value"),
    "alias" = xml_child(node) %>% xml_children() %>% xml_attr("alias")
  )
  
  if(i == 2) {
    
    df$alias <- paste(df$value, df$alias, sep = " : ")
  }
  
  df
  
  
})

names(dicopub_TO_EQPUB_P) <- c("theme", "sstheme")

usethis::use_data(dicopub_TO_EQPUB_P, overwrite = TRUE)


# 
# main_theme <- filter(isochrones::dicopub_TO_EQPUB_P$theme, alias %in% input$equipement_theme) %>% 
#   +     pull(value)
# 
# 
# dicopub_TO_EQPUB_P$sstheme %>% filter(stringr::str_starts(string = value, pattern = main_theme))