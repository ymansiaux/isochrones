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
      
      # footer = includeHTML(app_sys("app/www/footer.html")),
      
      tabPanel(
        "grid basique",
        
        div(class="parent",
            div(class="div1", style = "background : blue;", "div1"),
            div(class="div2", style = "background : red;", "div2"),
            div(class="div3", style = "background : pink;", "div3"),
            div(class="div4", style = "background : green;", 
                includeHTML(app_sys("app/www/footer.html"))

            )
        )
      )
,

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
.div4 { grid-area: 6 / 1 / 7 / 5; }"
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

