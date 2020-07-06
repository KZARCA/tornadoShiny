library(shiny)

shinyUI(

  tabsetPanel(id="main",
    tabPanel('Paramètres',
  fluidPage(
    tags$style(type='text/css', '#erreurs {background-color: rgba(0,0,255,0.10); color: blue;}'),
    titlePanel("Diagramme en tornade", windowTitle = "Tornado"),
    br(),
    fluidRow(
      column(3, wellPanel(fileInput("dataload", label="Charger des données existantes"))),
      column(3, img(src = "logo.jpg", height = 160)),
      column(3, wellPanel(
        textInput("xlabel", label="Titre sur l'axe des x", value="Coût par acte (€)"))),
      column(3, uiOutput("erreurs"))
    ),
    fluidRow(
      column(3, wellPanel(
        numericInput("nbparam", label='Nombre de variables', value="", min = 2))
      ),
      column(3, wellPanel(
             numericInput("baseline", label='Baseline', value=""))
      ),
      column(3, wellPanel(
        radioButtons("format", label='Format', choices=list("Français", "Américain")))
      )
    ),
    hr(),
    fluidRow(
      column(2, uiOutput("ui_variable")),
      column(2, uiOutput("ui_minvar")),
      column(2, uiOutput("ui_maxvar")),
      column(2, uiOutput("ui_minICER")),
      column(2, uiOutput("ui_maxICER")),
      column(2, uiOutput("ui_unit"))
      ), 
    br(),
    fluidRow(
      column(3, offset = 3, conditionalPanel(condition = "input.nbparam != null", downloadButton("downloadData", "Télécharger le tableau"))),
      column(3, conditionalPanel(condition = "input.nbparam != null", actionButton("start", "Afficher le graphique")))
    ),
    hr()
    )
  ),
  tabPanel("Graphique", 
           sidebarLayout(
             sidebarPanel(
               selectInput("choixPalette", label = "Couleurs", choices = c(paste0("Set", 1:2), "N&B")),
               selectInput("typegraph", label = "Format d'export", choices = list("png", "pdf", "eps", "tiff", "svg"), selected ="png"),
               numericInput("nbDPI", label ="Définition (DPI)", value=300, min=300),
               downloadButton("downloadImg", "Télécharger l'image")),
             mainPanel(    
               h1("Graphique généré"),
               br(),
               plotOutput("graph")
            ),
             position="right"
           )
  )
)
)
