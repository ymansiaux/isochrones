#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import lwgeom
#' @importFrom tibble as_tibble
#' @importFrom xml2 read_xml as_list
#' @importFrom tidyr unnest_longer unnest_wider unnest 
#' @importFrom stringr str_split_fixed
#' @importFrom dplyr filter bind_cols slice
#' @importFrom sf st_as_sf st_transform st_join sf_use_s2
#' @importFrom leaflet leaflet addTiles addMarkers renderLeaflet addPolygons addAwesomeMarkers awesomeIcons
#' @importFrom osrm osrmIsochrone
#' @importFrom xtradata xtradata_requete_features
#' @noRd
app_server <- function( input, output, session ) {
  
  data_geo <- reactiveValues(geocoding = NULL,
                             isochrone = NULL,
                             equipements = NULL)
  
  # Your application server logic 
  observeEvent(input$run_geocoding, {
    
     # adress <- "23 rue neuve, 33000 Bordeaux"

     encodedURL <- URLencode(input$adress)
     
     url <- paste0("https://data.bordeaux-metropole.fr/wps?key=INTERNEUSR&service=wps&version=1.0.0&request=execute&identifier=geocodeur&datainputs=input=", encodedURL)
    print(url)
     res_xml <- read_xml(url)
    
     list_res_xml <- as_list(res_xml)
    
     tb <-  as_tibble(list_res_xml)
    
     tb_clean <- tb %>%
       unnest_longer(ExecuteResponse) %>%
       filter(ExecuteResponse_id == "Output") %>%
       unnest_wider(ExecuteResponse) %>%
       unnest_wider(Data) %>%
       unnest_wider(ComplexData) %>%
       unnest_wider(featureMember) %>%
       unnest_wider(default) %>%
       unnest(cols = names(.)) %>%
       unnest(cols = names(.))
    
     geom <- tb_clean %>%
       unnest(geometry) %>%
       unnest(geometry)
    
     coordinates <-  str_split_fixed(string = geom$geometry, pattern = " ", n = 2)
     colnames(coordinates) <- c("x", "y")
    
     sf_geoloc <- bind_cols(tb_clean, coordinates) %>%
       st_as_sf(., coords = c("x", "y"), crs = 3945) %>%
       st_transform(crs = 4326) %>% 
       slice(1)#%>% 
       # slice_max(PERTINENCE)
    
    
     data_geo$geocoding <- sf_geoloc
     print(head(data_geo$geocoding))
     
  })
  
  output$map_geocoding <- renderLeaflet({
    
    req(data_geo$geocoding)
    
    map <- data_geo$geocoding %>% 
      leaflet() %>% 
      addTiles() %>% 
      addMarkers()
    
    if(!is.null(data_geo$isochrone)) {
      
      map <- map %>% 
        addPolygons(data = data_geo$isochrone)
    }
    
    if(!is.null(data_geo$equipements)) {
      
      map <- map %>% 
        addAwesomeMarkers(data = data_geo$equipements, icon = awesomeIcons(markerColor = "red"))
      
    }
    
    map
    
  })
  
  observeEvent(input$isochrone_computing , {
    
    req(data_geo$geocoding)
    
    data_geo$isochrone <- osrmIsochrone(data_geo$geocoding, breaks = input$isochrone_size, osrm.profile = input$osrm.profile)
    
  })
  
  
  observeEvent(input$equipements_computing , {
    
    req(data_geo$geocoding)
    req(data_geo$isochrone)
    
   xtradata_call <- xtradata_requete_features(key = Sys.getenv('XTRADATA_KEY'),
                                          typename = "TO_EQPUB_P",
                                          filter = list("theme" = list("$in" = input$equipement_theme)))
   
   sf_use_s2(FALSE)
   data_geo$equipements <- st_join(xtradata_call, data_geo$isochrone) %>% 
     filter(!is.na(id))
   # https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
})
   
   observeEvent(input$pause, browser())
}
