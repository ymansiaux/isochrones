#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bdxmetroidentity
#' @importFrom shinyjs useShinyjs
#' @importFrom shinybusy add_busy_spinner
#' @importFrom shinyYM waiter_logo add_notie_deps
#' @importFrom sass font_google
#' @importFrom leaflet leafletOutput
#' @noRd
#'

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    add_notie_deps(),
    useShinyjs(),
    
    waiter_logo(isinProd = golem::app_prod(), img_path = "www/LogoDataLab.png"),
    
    navbarPage(
      
      theme = theme_bdxmetro_shiny(
        bg = "#32A89F",
        fg = "white",
        base_font = font_google("Nunito"), 
        heading_font = font_google("Nunito"),
        primary = "#e30b5d",
        "navbar-light-bg" = "#32A89F",
        "navbar-light-color" = "black",
        "navbar-light-active-color" = "darkblue"
      ),
      
      title = "Quels sont les équipements proches de chez moi ?",
      collapsible = TRUE,
      
      footer = includeHTML(app_sys("app/www/footer.html")),
      
      tabPanel(
        "grid basique",
        fluidRow(
          column(width = 12,
                 div(class = "parent content",
                     div(class = "title_geocoding",
                         h2("title_geocoding")
                     ),
                     
                     div(class = "title_map_geocoding", 
                         h2("title_map_geocoding")
                     ),
                     
                     div(class = "map_geocoding",  
                         leafletOutput("map_geocoding")
                     ),
                     
                     div(class = "adress_to_geocode", 
                         textInput(inputId = "adress",
                                   label = "Adresse à géocoder",
                                   value = "27 rue Jean Fleuret, 33000 Bordeaux",
                                   placeholder = "Ex : 35 rue Neuve, 33000 Bordeaux")),
                     
                     div(class = "adress_to_geocode_validator",  
                         actionButton(inputId = "run_geocoding",
                                      label = "Lancer le géocodage"
                         )),
                     
                     div(class = "geocoding_results_table", 
                         DTOutput("geocoding_table")),
                     
                     div(class = "geocoded_adress_validator", 
                         h5("Valider la sélection et passer au calcul des isochrones")
                     )
                     
                     
                 )
          )
        )
      ),
      
      # tabPanel(
      #   "Accueil",
      #   div(class = "content",
      #       
      #       fluidRow(
      #         
      #         column(width = 6,
      #                textInput(inputId = "adress",
      #                          label = "Adresse à géocoder",
      #                          value = "27 rue Jean Fleuret, 33000 Bordeaux",
      #                          placeholder = "Ex : 35 rue Neuve, 33000 Bordeaux"),
      #                
      #                actionButton(inputId = "run_geocoding",
      #                             label = "Lancer le géocodage"
      #                )
      #         )
      #       )
      #   )
      # ),
      
      tabPanel(
        "calculer un isochrone",
        div(class = "content",
            
            
            sliderInput(inputId = "isochrone_size", 
                        label = "Taille de l'isochone (minutes)",
                        min = 1,
                        max = 60,
                        step = 5,
                        value = 15),
            
            selectInput(inputId = "osrm.profile",
                        label = "mode de déplacement",
                        choices = c("piéton" = "foot",
                                    "vélo" = "bike",
                                    "auto" = "car")
                        
            ),
            
            actionButton(inputId = "isochrone_computing", label = "Lancer calcul de l'isochrone")
        )
      ),
      
      tabPanel(
        "équipements",
        div(class = "content",
            
            h2("Afficher les équipements présents dans l'isochrone"),
            h3("Patienter jusqu'à l'affichage de l'isochrone sur la carte pour lancer la suite"),
            
            selectizeInput(inputId = "equipement_theme",
                           label = "Catégories d'équipements",
                           choices = c("A : Enseignement divers et formation" = "A",
                                       "B : Santé et action sociale" = "B",
                                       "C : Sport - Loisir - Socio-éducatif" = "C",
                                       "D: Culture - Patrimoine" = "D",
                                       "E : Administration" = "E",
                                       "F : Services" = "F",
                                       "G : Sécurité" = "G",
                                       "H : Espace vert ou espace urbain public" = "H",
                                       "J : Déplacements" = "J",
                                       "K : Production et transformation d'énergie - assainissement - environnement" = "K",
                                       "L : Cultuel" = "L",
                                       "M : Métropole" = "M",
                                       "N : Sénior" = "N",
                                       "O : Commune" = "O",
                                       "P : Petite enfance" = "P"),
                           multiple = TRUE
            ),
            
            actionButton(inputId = "equipements_computing", label = "Calculer les équipements présents dans la zone")
        ),
        column(width = 6, h5("titi")
               # leafletOutput("map_geocoding")
               
        ) 
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style( ".parent {
display: grid;
grid-template-columns: repeat(5, 1fr);
grid-template-rows: 0.5fr repeat(3, 1fr) 0.5fr;
grid-column-gap: 10px;
grid-row-gap: 10px;
}

.title_geocoding { grid-area: 1 / 1 / 2 / 3; }
.title_map_geocoding { grid-area: 1 / 3 / 2 / 6; }
.map_geocoding { grid-area: 2 / 3 / 5 / 7; }
.geocoded_adress_validator { grid-area: 5 / 2 / 6 / 5; }
.adress_to_geocode { grid-area: 2 / 1 / 3 / 2; }
.adress_to_geocode_validator { grid-area: 2 / 2 / 3 / 3; }
.geocoding_results_table { grid-area: 3 / 1 / 5 / 3; }
"
    ),
favicon(),
bundle_resources(
  path = app_sys('app/www'),
  app_title = 'isochrones'
)
# Add here other external resources
# for example, you can add shinyalert::useShinyalert() 
  )
}

