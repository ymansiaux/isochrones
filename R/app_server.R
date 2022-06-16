#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import lwgeom
#' @importFrom tibble as_tibble
#' @importFrom xml2 read_xml as_list
#' @importFrom tidyr unnest_longer unnest_wider unnest 
#' @importFrom stringr str_split_fixed str_starts
#' @importFrom dplyr filter bind_cols slice pull
#' @importFrom sf st_as_sf st_transform st_join sf_use_s2 st_drop_geometry
#' @importFrom leaflet leaflet addTiles addMarkers renderLeaflet addPolygons addAwesomeMarkers awesomeIcons
#' @importFrom osrm osrmIsochrone
#' @importFrom xtradata xtradata_requete_features
#' @importFrom DT renderDT datatable DTOutput
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom glue glue_data
#' @importFrom cols4all c4a
#' @importFrom shinyjs runjs
#' @importFrom shinyYM add_notie_alert
#' @noRd
app_server <- function( input, output, session ) {
  
  data_geo <- reactiveValues(geocoding = NULL,
                             isochrone = NULL,
                             equipements = NULL)
  
  output$emptymap <- renderLeaflet(leaflet() %>% addTiles())
  
  ##################
  #### ONGLET 1 ####
  ##################
  
  
  # Your application server logic 
  observeEvent(input$run_geocoding, {
    
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
    
    if(!"geometry" %in% colnames(tb_clean)) {
      
      data_geo$geocoding <- NULL  
      
      add_notie_alert(type = "error", text = "Le géocodage a échoué, essayez une nouvelle adresse",
                      stay = FALSE, time = 5, position = "bottom", session)
      
    } else {
      
      geom <- tb_clean %>%
        unnest(geometry) %>%
        unnest(geometry)
      
      coordinates <-  str_split_fixed(string = geom$geometry, pattern = " ", n = 2)
      colnames(coordinates) <- c("x", "y")
      
      sf_geoloc <- bind_cols(tb_clean, coordinates) %>%
        st_as_sf(., coords = c("x", "y"), crs = 3945) %>%
        st_transform(crs = 4326) 
      
      
      data_geo$geocoding <- sf_geoloc
      print(head(data_geo$geocoding))
      
    }
    
    
  })
  
  
  output$geocoding_table <- renderDT({
    
    req(data_geo$geocoding)
    
    datatable(
      st_drop_geometry(
        data_geo$geocoding[, c("PERTINENCE", "NOM_VOIE", "COMMUNE", "CODE_INSEE", "NUMERO", "REP", "CODE_POSTAL")]
      ),
      selection = list(mode = "single", target = "row", selected = 1),
      fillContainer = TRUE
    )
  })
  
  observe(
    if(is.null(data_geo$geocoding)) {
      runjs('$("#geocoding_results").hide();')
      runjs('$("#isochrone_computing").addClass("disabled");')
      runjs('$("#equipements_computing").addClass("disabled");')
      
    } else {
      
      if(!is.null(data_geo$isochrone)) {
        runjs('$("#equipements_computing").removeClass("disabled");')
      }
      runjs('$("#isochrone_computing").removeClass("disabled");')
      runjs('$("#geocoding_results").show();')  
    }
    
  )
  
  
  output$map_geocoding <- renderLeaflet({
    
    req(data_geo$geocoding)
    req(input$geocoding_table_rows_selected)
    
    data_geo_selected_adress <- data_geo$geocoding[input$geocoding_table_rows_selected,]
    
    popup_selected_adress <- glue_data(data_geo_selected_adress, "{NUMERO} {NOM_VOIE}, {COMMUNE}")
    
    
    map <- data_geo_selected_adress %>% 
      leaflet() %>% 
      addTiles() %>% 
      addMarkers(popup = popup_selected_adress)
    
    map
    
  })
  
  
  
  ##################
  #### ONGLET 2 ####
  ##################
  observeEvent(input$equipement_theme, {
    
    main_theme <- filter(isochrones::dicopub_TO_EQPUB_P$theme, alias %in% input$equipement_theme) %>% 
      pull(value)
    
    updatePickerInput(inputId = "equipement_sstheme", session = session,
                      choices =  dicopub_TO_EQPUB_P$sstheme %>% 
                        filter(str_starts(string = value, pattern = paste(main_theme, collapse = "|"))) %>% 
                        pull(alias)
    )
    
  })
  
  observeEvent(input$isochrone_computing , {
    
    req(data_geo$geocoding)
    
    data_geo$isochrone <- osrmIsochrone(data_geo$geocoding, breaks = input$isochrone_size, osrm.profile = input$osrm.profile)
    
  })
  
  
  observeEvent(input$equipements_computing , {
    
    req(data_geo$geocoding)
    req(data_geo$isochrone)
    
    filter_xtradata <- list("theme" = list("$in" = 
                                             filter(isochrones::dicopub_TO_EQPUB_P$theme, alias %in% input$equipement_theme) %>% 
                                             pull(value)
    ))
    
    if(!is.null(input$equipement_sstheme)) {
      filter_xtradata <- c(
        filter_xtradata,
        list("sstheme" = list("$in" = 
                                filter(isochrones::dicopub_TO_EQPUB_P$sstheme, alias %in% input$equipement_sstheme) %>% 
                                pull(value) 
        )
        )
      )
    }
    
    xtradata_call <- xtradata_requete_features(key = Sys.getenv('XTRADATA_KEY'),
                                               typename = "TO_EQPUB_P",
                                               filter = filter_xtradata
    )
    
    sf_use_s2(FALSE)
    data_geo$equipements <- st_join(xtradata_call, data_geo$isochrone) %>% 
      filter(!is.na(id))
    # https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
  })
  
  
  output$map_isochrone <- renderLeaflet({
    
    req(data_geo$geocoding)
    req(input$geocoding_table_rows_selected)
    
    data_geo_selected_adress <- data_geo$geocoding[input$geocoding_table_rows_selected,]
    
    popup_selected_adress <- glue_data(data_geo_selected_adress, "{NUMERO} {NOM_VOIE}, {COMMUNE}")
    
    map <- data_geo_selected_adress %>% 
      leaflet() %>% 
      addTiles() %>% 
      addMarkers(popup = popup_selected_adress)
    
    if(!is.null(data_geo$isochrone)) {
      
      map <- map %>% 
        addPolygons(data = data_geo$isochrone)
    }
    
    if(!is.null(data_geo$equipements)) {
      
      popup_equipements <-  paste0("<strong>",
                                   data_geo$equipements$nom,
                                   "</strong>",
                                   "<br> theme : ",
                                   data_geo$equipements$theme,
                                   "<br> sous-theme : ",
                                   data_geo$equipements$sstheme)
      
      
      
      themes_uniques <- unique(data_geo$equipements$theme)
      color_equipement <- data.frame("theme" = themes_uniques, col = c4a("rainbow", length(input$equipement_theme)))
      
      equipements <- merge(data_geo$equipements, color_equipement, by = "theme")
      
      map <- map %>% 
        addAwesomeMarkers(data = equipements, 
                          icon = awesomeIcons(iconColor = equipements$col, markerColor = "beige"),
                          popup = popup_equipements)
    }
    
    map
    
  })
  
  
  observeEvent(input$pause, browser())
  
}


# https://data.bordeaux-metropole.fr/wps?key=DATAZBOUBB&service=wps&version=1.0.0&request=execute&identifier=dico_couches&datainputs=couche=TO_EQPUB_P