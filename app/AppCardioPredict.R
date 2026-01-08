

library(shiny)
library(httr)
library(jsonlite)
library(bslib)

API_URL <- "http://localhost:8088/predict"

# ---------------------------
# UI PROFESSIONNEL
# ---------------------------
ui <- page_navbar(
  title = "Prediction du risque cardiovasculaire",
  theme = bs_theme(
    version = 5,
    bootswatch = "cosmo",
    primary = "#0A5275",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  
  nav_panel(
    "Prédiction",
    
    layout_columns(
      col_widths = c(4, 8),
      
      # ---------------------------
      # FORMULAIRE (CARD)
      # ---------------------------
      card(
        full_screen = FALSE,
        card_header("Informations Patient"),
        
        card_body(
          class = "bg-light",
          
          numericInput("age", "Âge", value = 52, min = 1, max = 120),
          selectInput("gender", "Genre", choices = c("Femme" = "0", "Homme" = "1")),
          numericInput("imc", "IMC", value = 27.4, min = 10, max = 60),
          
          numericInput("ap_hi", "Pression systolique (ap_hi)", value = 135, min = 50, max = 250),
          numericInput("ap_lo", "Pression diastolique (ap_lo)", value = 85, min = 30, max = 200),
          
          selectInput("cholesterol", "Cholestérol",
                      choices = c("Normal" = "1", "Élevé" = "2", "Très élevé" = "3")),
          
          selectInput("gluc", "Glycémie",
                      choices = c("Normal" = "1", "Élevé" = "2", "Très élevé" = "3")),
          
          selectInput("smoke", "Fumeur", choices = c("Non" = "0", "Oui" = "1")),
          selectInput("alco", "Alcool", choices = c("Non" = "0", "Oui" = "1")),
          selectInput("active", "Actif physiquement", choices = c("Non" = "0", "Oui" = "1")),
          
          br(),
          actionButton("predict_btn", "Lancer la prédiction", class = "btn btn-primary btn-lg w-100")
        )
      ),
      
      # ---------------------------
      # RÉSULTAT (CARD)
      # ---------------------------
      card(
        full_screen = FALSE,
        card_header("Résultat"),
        
        card_body(
          h4("Prédiction du modèle", class = "text-primary"),
          br(),
          uiOutput("prediction_output"),
          br(),
          hr(),
          p("API utilisée :", code(API_URL), class = "text-muted small")
        )
      )
    )
  )
)

# ---------------------------
# SERVER
# ---------------------------
server <- function(input, output, session) {
  
  observeEvent(input$predict_btn, {
    
    payload <- list(
      age = input$age,
      gender = input$gender,
      imc = input$imc,
      ap_hi = input$ap_hi,
      ap_lo = input$ap_lo,
      cholesterol = input$cholesterol,
      gluc = input$gluc,
      smoke = input$smoke,
      alco = input$alco,
      active = input$active
    )
    
    json_payload <- toJSON(list(payload), auto_unbox = TRUE)
    
    res <- tryCatch({
      POST(API_URL,
           body = json_payload,
           encode = "json",
           content_type_json())
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(res)) {
      output$prediction_output <- renderUI(
        div(class = "alert alert-danger",
            "❌ Impossible de contacter l'API. Vérifiez qu'elle est bien lancée.")
      )
      return()
    }
    
    result <- content(res, as = "parsed", simplifyVector = TRUE)
    
    output$prediction_output <- renderUI({
      card(
        class = "border-success",
        card_body(
          h5("Résultat brut :", class = "text-success"),
          pre(jsonlite::toJSON(result, pretty = TRUE, auto_unbox = TRUE))
        )
      )
    })
  })
}

shinyApp(ui, server)



