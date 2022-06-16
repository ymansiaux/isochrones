#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bdxmetroidentity
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyYM waiter_logo add_notie_deps
#' @importFrom sass font_google
#' @importFrom leaflet leafletOutput
#' @importFrom shinyWidgets pickerInput
#' @importFrom shinybusy add_busy_spinner
#' 
#' @noRd
#'

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    add_notie_deps(),
    useShinyjs(),
    add_busy_spinner(),
    
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
      
      tabPanel(
        "Géocodage",
        
        div(class="parent",
            div(class="div1 border", "Décrire l'objectif"),
            div(class="div2 border",  
                
                div(class = "container",
                    style = "display:flex;
                             flex-direction : column;
                             min-height:100%;
                             ",
                    
                    div(
                      h4("Préparation du géocodage"),
                      tags$br(),
                      textInput(inputId = "adress",
                                label = "Adresse à géocoder",
                                value = "27 rue Jean Fleuret, 33000 Bordeaux",
                                placeholder = "Ex : 27 rue Jean Fleuret, 33000 Bordeaux"),
                      
                      actionButton(inputId = "run_geocoding",
                                   label = "Lancer le géocodage"
                      )
                    ),
                    
                    div(id = "geocoding_results", 
                        style = "margin-bottom: auto;
                               margin-top: auto;",
                        h4("Résultat géocodage"),
                        tags$br(),
                        DTOutput("geocoding_table"),
                        tags$br(),
                        actionButton(inputId = "move_to_isochrone_computing",
                                     label = "Valider la sélection et passer à l'onglet suivant",
                                     onclick = "$('ul').find('[data-value=\"Calculer un isochrone\"]').tab('show');"
                        )
                    )
                )
            ),
            
            
            div(class="div3 border", style = "min-height:100%;", 
                leafletOutput("map_geocoding", width="100%", height="100%")
            ),
            
            
            div(class="div4 border", 
                includeHTML(app_sys("app/www/footer.html"))
                
            )
        )
      ),
      
      tabPanel(
        "Calculer un isochrone",
        
        div(class="parent",
            div(class="div1 border", "Décrire l'objectif"),
            div(class="div2 border",  
                
                div(class = "container",
                    style = "display:flex;
                             flex-direction : column;
                             min-height:100%;
                             ",
                    
                    div(
                      tags$br(),
                      actionButton(inputId = "back_to_geocoding",
                                   label = "Revenir au géocodage d'une adresse",
                                   onclick = "$('ul').find('[data-value=\"Géocodage\"]').tab('show');"
                      ),
                      
                      h4("Calcul de l'isochrone"),
                      tags$br(),
                      div(class = "container",
                          style = "display:flex;
                             flex-direction : row;
                             justify-content: space-between;
                             ",
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
                          )       
                      ),
                      
                      actionButton(inputId = "isochrone_computing", label = "Lancer calcul de l'isochrone (patience ...)")
                    ),
                    
                    div(style = "margin-bottom: auto;
                               margin-top: auto;",
                        
                        h4("Bonus : afficher les équipements présents dans mon isochrone"),
                        tags$br(),
                        
                        div(class = "container",
                            style = "display:flex;
                             flex-direction : row;
                             justify-content: space-between;
                             ",
                            
                            pickerInput(
                              inputId = "equipement_theme",
                              label = "Catégories d'équipements",
                              choices = isochrones::dicopub_TO_EQPUB_P$theme$alias,
                              options = list(
                                `actions-box` = TRUE), 
                              multiple = TRUE
                            ),
                            
                            pickerInput(
                              inputId = "equipement_sstheme",
                              label = "Sous catégories d'équipements",
                              choices = NULL, 
                              options = list(
                                `actions-box` = TRUE), 
                              multiple = TRUE
                            )
                        ),
                        
                        
                        actionButton(inputId = "equipements_computing", label = "Calculer les équipements présents dans la zone")
                        
                    )
                )
            ),
            
            
            div(class="div3 border", style = "min-height:100%;", 
                leafletOutput("map_isochrone", width="100%", height="100%")
            ),
            
            
            div(class="div4 border", 
                includeHTML(app_sys("app/www/footer.html"))
                
            )
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
    tags$style( ".parent {
                    display: grid;
                    grid-template-columns: repeat(4, 1fr);
                    grid-template-rows: 0.5fr repeat(3, 1fr) 180px;
                    grid-column-gap: 10px;
                    grid-row-gap: 10px;
                    min-height: calc(100vh - 44px);
                }
              .div1 { grid-area: 1 / 1 / 2 / 5; }
.div2 { grid-area: 2 / 1 / 6 / 3; }
.div3 { grid-area: 2 / 3 / 6 / 5; }
.div4 { grid-area: 6 / 1 / 7 / 5; }
.border {border: 2px solid white;}"
    ),
# Note the wrapping of the string in HTML()
favicon(),
bundle_resources(
  path = app_sys('app/www'),
  app_title = 'isochrones'
)
# Add here other external resources
# for example, you can add shinyalert::useShinyalert() 
  )
}

