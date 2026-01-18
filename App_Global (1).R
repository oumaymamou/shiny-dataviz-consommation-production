################### Application final######################

# Charger les bibliothèques nécessaires
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)
library(lubridate)
library(hms)
library(leaflet)
library(sf)
library(readxl)
library(shinythemes)
library(data.table)
library(janitor)


# chargement des données:
conso_sup36= fread("conso-sup36-region.csv")
conso= fread("conso-inf36-region.csv")
df<- readRDS("Prod_enrgies.rds")

# Nettoyage des noms de colonnes
conso <- conso %>% clean_names()
conso_sup36 <- conso_sup36 %>% clean_names()
df <- df %>% clean_names()

conso <- conso[complete.cases(conso),] %>%
  mutate(
    horodate_clean = ymd_hms(horodate + dhours(2), tz = "UTC"),
    date = as.Date(horodate_clean),
    heure = as_hms(format(horodate_clean, format = "%H:%M:%S")),
    total_energie_soutiree_wh = as.numeric(total_energie_soutiree_wh),
    courbe_moyenne_n_1_wh = as.numeric(courbe_moyenne_n_1_wh),
    courbe_moyenne_n_2_wh = as.numeric(courbe_moyenne_n_2_wh)
  ) %>%
  select(c(16:18, 2:15))

conso_sup36 <- conso_sup36[complete.cases(conso_sup36),] %>%
  mutate(
    horodate_clean = ymd_hms(horodate + dhours(2), tz = "UTC"),
    date = as.Date(horodate_clean),
    heure = format(horodate_clean , format = "%H:%M:%S"),
    total_energie_soutiree_wh = as.numeric(total_energie_soutiree_wh),
    courbe_moyenne_n_1_wh = as.numeric(courbe_moyenne_n_1_wh),
    courbe_moyenne_n_2_wh = as.numeric(courbe_moyenne_n_2_wh)
  ) %>%
  select(c(17:19, 2:16))



# Charger les frontières des regions françaises depuis le fichier GeoJSON téléchargé:
regions_geo <- st_read("regions.geojson")  
regions_geo$code<-as.numeric(regions_geo$code)

# Fonction pour générer les courbes de consommation et production:
courbe_moyenne_plots <- function(data,data1,data2,regions,filiere,puissance,puissance1,puissance2) {
  
  # Filtrer et transformer les données pour 'data'(Production)
  dd<-data %>%
    filter(region %in% regions,
           filiere_de_production == filiere,
           plage_de_puissance_injection == puissance)%>%
    mutate(total_energie=total_energie_injectee_wh/nb_points_injection)%>%
    group_by(date,region,plage_de_puissance_injection) %>%
    summarise(Total_energie = sum(total_energie, na.rm = TRUE),Courbe_Moyenne1=sum(courbe_moyenne_n_1_wh, na.rm = TRUE),Courbe_Moyenne2=sum(courbe_moyenne_n_2_wh, na.rm = TRUE))
  
  # Filtrer et transformer les données pour 'data1'(Consommation supérieur à 36K)
  dd1<-data1 %>%
    filter(region %in% regions,
           plage_de_puissance_souscrite == puissance1)%>%
    mutate(total_energie=total_energie_soutiree_wh/nb_points_soutirage)%>%
    group_by(date,region,plage_de_puissance_souscrite) %>%
    summarise(Total_energie = sum(total_energie, na.rm = TRUE),Courbe_Moyenne1=sum(courbe_moyenne_n_1_wh, na.rm = TRUE),Courbe_Moyenne2=sum(courbe_moyenne_n_2_wh, na.rm = TRUE))
  
  # Filtrer et transformer les données pour 'data2'(Consommation inférieur à 36K)
  dd2<-data2 %>%
    filter(region %in% regions,
           plage_de_puissance_souscrite == puissance2)%>%
    mutate(total_energie=total_energie_soutiree_wh/nb_points_soutirage)%>%
    group_by(date,region,plage_de_puissance_souscrite) %>%
    summarise(Total_energie = sum(total_energie, na.rm = TRUE),Courbe_Moyenne1=sum(courbe_moyenne_n_1_wh, na.rm = TRUE),Courbe_Moyenne2=sum(courbe_moyenne_n_2_wh, na.rm = TRUE))
  
  # Création des graphiques pour la consommation inférieure à 36k:
  conso_inf <- ggplot(dd1, aes(x = date)) +
    geom_line(aes(y = Total_energie, color = "Energie Moyenne soutirées")) +
    geom_line(aes(y = Courbe_Moyenne1, color = "Courbe Moyenne 1"), linetype = "dotted", size = 1) +
    geom_line(aes(y = Courbe_Moyenne2, color = "Courbe Moyenne 2"), linetype = "dashed", size = 1) +
    scale_color_manual(values = c("Energie Moyenne soutirées" = "green", 
                                  "Courbe Moyenne 1" = "red", 
                                  "Courbe Moyenne 2" = "yellow")) +
    labs(title = "Moyenne Totale (Tous profils) d'énergie soutirée <=36k",
         x = "date",
         y = "Consommation",
         color = NULL) +
    theme_minimal() +
    theme(
      legend.position = c(0, 1), 
      legend.justification = c(0, 1), 
      legend.background = element_rect(fill = "transparent", color = NA), 
    )
  
  # Création des graphiques pour la consommation supérieure à 36k:
  conso_sup <- ggplot(dd2, aes(x = date)) +
    geom_line(aes(y = Total_energie, color = "Energie Moyenne soutirées")) +
    geom_line(aes(y = Courbe_Moyenne1, color = "Courbe Moyenne 1"), linetype = "dotted", size = 1) +
    geom_line(aes(y = Courbe_Moyenne2, color = "Courbe Moyenne 2"), linetype = "dashed", size = 1) +
    scale_color_manual(values = c("Energie Moyenne soutirées" = "green", 
                                  "Courbe Moyenne 1" = "red", 
                                  "Courbe Moyenne 2" = "yellow")) +
    labs(title = "Moyenne Totale (Tous profils) d'énergie soutirée >36k",
         x = "date",
         y = "Consommation",
         color = NULL) +
    theme_minimal() +
    theme(
      legend.position = c(0, 1), 
      legend.justification = c(0, 1), 
      legend.background = element_rect(fill = "transparent", color = NA), 
    )
  
  
  # Création des graphiques pour la production:
  production <- ggplot(dd, aes(x = date)) +
    geom_line(aes(y = Total_energie, color = "Energie Moyenne injectées")) +
    geom_line(aes(y = Courbe_Moyenne1, color = "Courbe Moyenne 1"), linetype = "dotted", size = 1) +
    geom_line(aes(y = Courbe_Moyenne2, color = "Courbe Moyenne 2"), linetype = "dashed", size = 1) +
    scale_color_manual(values = c("Energie Moyenne injectées" = "green", 
                                  "Courbe Moyenne 1" = "red", 
                                  "Courbe Moyenne 2" = "yellow")) +
    labs(title = "Moyenne Totale d'énergie injectée",
         x = "date",
         y = "Production",
         color = NULL) +
    theme_minimal() +
    theme(
      legend.position = c(0, 1), 
      legend.justification = c(0, 1), 
      legend.background = element_rect(fill = "transparent", color = NA), 
    )
  
  # Retourner la liste de tous les graphiques:
  list(conso_inf, conso_sup, production)
}


# Interface utilisateur de l'application Shiny:
ui <- dashboardPage(
  dashboardHeader(title = "Analyse de la Production & la consommation d'Energie",titleWidth = 500),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      column(12, align = "center",
             actionButton("eng", "Courbes Moyenne", class = "btn-secondary"),
             actionButton("tab_36k", "Conso > 36k", class = "btn-primary"),
             actionButton("tab_36k_inf", "Conso <= 36k", class = "btn-success"),
             actionButton("tab_prod", "Production", class = "btn-warning")
      )
    ),
    br(),  
    uiOutput("main_content")
  )
)

# La fonction server de l'application shiny:
server <- function(input, output, session) {
  
  # UI pour afficher les courbes moyennes d'énergie:
  output$main_content <- renderUI({
    tagList(
      h2("Courbes moyenne d'Énergie"), # Titre de la section
      fluidRow(align = "center",
               
               # Sélection de la région:
               column(3,
                      selectInput(
                        inputId = "region",
                        label = "Sélectionner les regions",
                        choices = unique(df$region),
                        selected = unique(df$region)[1]
                      )),
               # Sélection de la filière de production:
               column(3,
                      selectInput("fliere", "Sélectionner la filère (Production)", 
                                  choices = unique(df$filiere_de_production), 
                                  selected = unique(df$filiere_de_production)[1]
                      )),
               # Sélection de la plage de puissance pour l'injection:
               column(2,
                      selectInput(
                        inputId = "plage_puissance",
                        label = "La plage de puissance",
                        choices = unique(df$plage_de_puissance_injection),
                        selected = unique(df$plage_de_puissance_injection)[1],
                      )),
               # Sélection de la plage de puissance <=36k pour la consommation:
               column(2,
                      selectInput(
                        inputId = "plage_puissance_inf",
                        label = "La plage de puissance <=36k",
                        choices = unique(conso$plage_de_puissance_souscrite),
                        selected = unique(conso$plage_de_puissance_souscrite)[1],
                      )),
               # Sélection de la plage de puissance >36k pour la consommation:
               column(2,
                      selectInput(
                        inputId = "plage_puissance_sup",
                        label = "La plage de puissance >36k",
                        choices = unique(conso_sup36$plage_de_puissance_souscrite),
                        selected = unique(conso_sup36$plage_de_puissance_souscrite)[1],
                      ))
      ),
      fluidRow(
        # Affichage des trois graphiques (consommation < 36k, consommation > 36k, production d'énergie):
        column(4, plotOutput(outputId = "conso_inf")),
        column(4, plotOutput(outputId = "conso_sup")),
        column(4, plotOutput(outputId = "Prod_Energie"))
      )
    )
  })
  
  # Observer les changements dans les sélections et générer les graphiques correspondants:
  observe({
    req(input$region,input$plage_puissance,input$fliere)
    
    # Appel de la fonction pour générer les courbes moyennes pour les trois graphiques:
    plots <- courbe_moyenne_plots(df,conso,conso_sup36, input$region,input$fliere,input$plage_puissance,input$plage_puissance_inf,input$plage_puissance_sup)
    
    # Affichage des graphiques dans l'UI:
    output$conso_inf <- renderPlot({ plots[[1]] })
    output$conso_sup <- renderPlot({ plots[[2]] })
    output$Prod_Energie <- renderPlot({ plots[[3]] })
  })
  
  # Observer un événement lié au bouton ou autre interaction pour mettre à jour l'UI:
  observeEvent(input$eng, {
    output$main_content <- renderUI({
      tagList(
        h2("Courbes moyenne d'Énergie"), # Titre de la section
        fluidRow(align = "center",
                 
                 # Sélection de la région
                 column(4,
                        selectInput(
                          inputId = "region",
                          label = "Sélectionner les regions",
                          choices = unique(df$region),
                          selected = unique(df$region)[1]
                        )),
                 
                 # Sélection de la filière de production:
                 column(3,
                        selectInput("fliere", "Sélectionner la filère (Production)", 
                                    choices = unique(df$filiere_de_production), 
                                    selected = unique(df$filiere_de_production)[1]
                        )),
                 
                 # Sélection de la plage de puissance pour l'injection:
                 column(2,
                        selectInput(
                          inputId = "plage_puissance",
                          label = "La plage de puissance",
                          choices = unique(df$plage_de_puissance_injection),
                          selected = unique(df$plage_de_puissance_injection)[1],
                        )),
                 
                 # Sélection de la plage de puissance <=36k pour la consommation:
                 column(2,
                        selectInput(
                          inputId = "plage_puissance_inf",
                          label = "La plage de puissance <=36k",
                          choices = unique(conso$plage_de_puissance_souscrite),
                          selected = unique(conso$plage_de_puissance_souscrite)[1],
                        )),
                 
                 # Sélection de la plage de puissance >36k pour la consommation:
                 column(2,
                        selectInput(
                          inputId = "plage_puissance_sup",
                          label = "La plage de puissance >36k",
                          choices = unique(conso_sup36$plage_de_puissance_souscrite),
                          selected = unique(conso_sup36$plage_de_puissance_souscrite)[1],
                        ))
        ),
        
        fluidRow(
          
          # Affichage des trois graphiques (consommation < 36k, consommation > 36k, production d'énergie)
          column(4, plotOutput(outputId = "conso_inf")),
          column(4, plotOutput(outputId = "conso_sup")),
          column(4, plotOutput(outputId = "Prod_Energie"))
        )
      )
    })
    
    # Observer à nouveau les sélections et générer les graphiques:
    observe({
      req(input$region,input$plage_puissance,input$fliere)
      
      # Appel de la fonction pour générer les courbes moyennes pour les trois graphiques:
      plots <- courbe_moyenne_plots(df,conso,conso_sup36, input$region,input$fliere,input$plage_puissance,input$plage_puissance_inf,input$plage_puissance_sup)
      
      # Affichage des graphiques dans l'UI:
      output$conso_inf <- renderPlot({ plots[[1]] }) # Consommation pour plage <= 36k
      output$conso_sup <- renderPlot({ plots[[2]] }) # Consommation pour plage > 36k
      output$Prod_Energie <- renderPlot({ plots[[3]] }) # Production d'énergie
    })
  })
  
  # Mettre à jour le contenu lorsque l'utilisateur clique sur un bouton:
  # Analyse de la Consommation supérieur à 36k: 
  observeEvent(input$tab_36k, {
    output$main_content <- renderUI({
      tagList(h2("Analyse de la Consommation supérieur à 36k "),
              radioButtons(
                inputId = "choix_pas",       # Boutons radio pour sélectionner le pas de temps
                label = "Choisissez le pas de temps :",
                choices = c("Demi-horaire" = "demi_horaire", "Quotidien" = "quotidien"),
                selected = "demi_horaire",
                inline = TRUE
              ),
              fluidRow(
                # Sélection des régions:
                column(3,
                       selectInput("region", "Sélectionner les regions", 
                                   choices = unique(conso_sup36$region), 
                                   selected = unique(conso_sup36$region)[1], 
                                   multiple = TRUE)),
                
                # Sélection du profil:
                column(2,
                       selectInput("profil", "Sélectionner le Profil", 
                                   choices = unique(conso_sup36$profil), 
                                   selected = unique(conso_sup36$profil)[1]
                       )),
                
                # Sélection de la plage de puissance:
                column(2,
                       selectInput("plage_puissance", "La plage de puissance", 
                                   choices = unique(conso_sup36$plage_de_puissance_souscrite), 
                                   selected = unique(conso_sup36$plage_de_puissance_souscrite)[1], 
                                   multiple = TRUE)),
                
                # selection de secteur d'activité:
                column(2,
                       selectInput("secteur", "Secteur d'activité", 
                                   choices = unique(conso_sup36$secteur_activite), 
                                   selected = unique(conso_sup36$secteur_activite)[1], 
                                   multiple = TRUE)),
                
                # Sélection dynamique de la date:
                column(1,
                       uiOutput("date_selector")),
                column(1, 
                       conditionalPanel(
                         condition = "input.choix_pas == 'demi_horaire'",
                         selectInput(
                           inputId = "heure_debut",     #Sélection de l'heure de début
                           label = "Heure Début", 
                           choices = format(seq(from = as.POSIXct("00:00:00", format = "%H:%M:%S"), 
                                                to = as.POSIXct("23:00:00", format = "%H:%M:%S"), 
                                                by = "30 mins"), 
                                            "%H:%M"),
                           selected = as.POSIXct("00:00:00", format = "%H:%M")
                         )
                       )),
                column(1, 
                       conditionalPanel(
                         condition = "input.choix_pas == 'demi_horaire'", 
                         selectInput(
                           inputId = "heure_fin",      # Sélection de l'heure de fin
                           label = "Heure Fin", 
                           choices = format(seq(from = as.POSIXct("00:00:00", format = "%H:%M:%S"), 
                                                to = as.POSIXct("23:30:00", format = "%H:%M:%S"), 
                                                by = "30 mins"), 
                                            "%H:%M"),
                           selected = format(as.POSIXct("23:30:00", format = "%H:%M:%S"),"%H:%M")
                         )
                       )),
                
                # Bouton pour télécharger les données:
                column(2,
                       downloadButton("download_data", "Télécharger les données", 
                                      class = "btn-success"), offset = 10)
              ),
              
              # Affchage des graphique:
              fluidRow(
                valueBoxOutput("total_energie"),       # Affichage du total de l'énergie
                valueBoxOutput("puissance_moyenne"),   # Affichage de la puissance moyenne
                valueBoxOutput("puissance_max")        # Affichage de la puissance maximale
              ),
              fluidRow(
                column(8, plotlyOutput("energy_plot")), # Affichage du graphique de l'énergie pour consosup36k
                column(4, leafletOutput("map"))         # Affichage de la carte pour consosup36k
              )
      )
    })
    
    # Sélection dynamique de la date ou plage de dates selon le choix de l'utilisateur:
    output$date_selector <- renderUI({
      if (input$choix_pas == "demi_horaire") {
        dateInput("date", "date", value = "2024-06-30")   
      } else {
        dateRangeInput("date_range", "Plage de dates", 
                       start = as.Date("2024-06-30") - 5, 
                       end = as.Date("2024-06-30"), 
                       width = 1000)    
      }
    })
    
    # Filtrage des données en fonction des choix de l'utilisateur pour consosup36k:
    filtered_data <- reactive({
      if (input$choix_pas == "demi_horaire") {
        
        # Filtrer les données selon les intervalles de temps:
        debut <- as_hms(paste0(input$heure_debut, ":00"))
        fin <- as_hms(paste0(input$heure_fin, ":00"))
        
        # Filtrage des données en fonction des paramètres choisis:
        conso_sup36_filtered <- conso_sup36 %>%
          filter(region %in% input$region,
                 plage_de_puissance_souscrite %in% input$plage_puissance,
                 secteur_activite %in% input$secteur,
                 date %in% input$date,
                 profil == input$profil,
                 as_hms(heure) >= debut & as_hms(heure) <= fin)
      }
      else {
        # Vérification si input$date_range est bien rempli
        if (!is.null(input$date_range) && length(input$date_range) == 2) {
          # Convertir les dates dans le bon format si nécessaire
          conso_sup36_filtered <- conso_sup36 %>%
            filter(region %in% input$region,
                   plage_de_puissance_souscrite %in% input$plage_puissance,
                   secteur_activite %in% input$secteur,
                   profil == input$profil,
                   as.Date(date) >= as.Date(input$date_range[1]) & as.Date(date) <= as.Date(input$date_range[2])) %>%
            group_by(date, region,secteur_activite, plage_de_puissance_souscrite) %>%
            summarise(Total_energie = sum(total_energie_soutiree_wh, na.rm = TRUE),
                      Courbe_Moyenne1 = sum(courbe_moyenne_n_1_wh, na.rm = TRUE),
                      Courbe_Moyenne2 = sum(courbe_moyenne_n_2_wh, na.rm = TRUE))
        } else {
          
          # Si input$date_range est vide ou invalide, retourner toutes les données sans filtre sur la date
          conso_sup36_filtered <- conso_sup36 %>%
            filter(region %in% input$region,
                   plage_de_puissance_souscrite %in% input$plage_puissance,
                   secteur_activite %in% input$secteur,
                   profil == input$profil) %>%
            group_by(date, region, plage_de_puissance_souscrite) %>%
            summarise(Total_energie = sum(total_energie_soutiree_wh, na.rm = TRUE),
                      Courbe_Moyenne1 = sum(courbe_moyenne_n_1_wh, na.rm = TRUE),
                      Courbe_Moyenne2 = sum(courbe_moyenne_n_2_wh, na.rm = TRUE))
        }
      }
      return(conso_sup36_filtered)
    })
    
    # ValueBox pour Total Energie soutirée consommation sup à 36: 
    output$total_energie <- renderValueBox({
      data <- filtered_data()
      if (input$choix_pas == "demi_horaire") {
        total_energie_wh <- sum(data$total_energie_soutiree_wh, na.rm = TRUE)
      } else {
        total_energie_wh <- sum(data$Total_energie, na.rm = TRUE)
      }
      
      # Conversion en kWh et arrondi
      total_energie_kwh <- round(total_energie_wh / 1000)
      
      # Formatage avec des unités adaptées
      if (total_energie_kwh < 1000) {
        total_energie_formatte <- paste(format(total_energie_kwh, big.mark = " "), " kWh") # Ajout d'un espace
      } else if (total_energie_kwh < 1000000) {
        total_energie_formatte <- paste(format(round(total_energie_kwh / 1000, 2), big.mark = " "), " MWh") # Arrondi à 2 décimales
      } else {
        total_energie_formatte <- paste(format(round(total_energie_kwh / 1000000, 2), big.mark = " "), " GWh") # Arrondi à 2 décimales
      }
      
      valueBox(
        total_energie_formatte,
        "Total Energie soutirée",
        icon = icon("bolt"),
        color = "light-blue"
      )
    })
    
    # Affichage de la puissance moyenne pour la consommation sup à 36:
    
    output$puissance_moyenne <- renderValueBox({
      data <- filtered_data()
      
      if (input$choix_pas == "demi_horaire") {
        puissance_moyenne_w <- mean(data$total_energie_soutiree_wh, na.rm = TRUE)
      } else {
        puissance_moyenne_w <- mean(data$Total_energie, na.rm = TRUE)
      }
      
      # Arrondi et conversion en kW ou MW si nécessaire
      if (puissance_moyenne_w < 1000) {
        puissance_moyenne_formatte <- paste(round(puissance_moyenne_w), " W")
      } else if (puissance_moyenne_w < 1000000) {
        puissance_moyenne_formatte <- paste(round(puissance_moyenne_w / 1000, 2), " kWh")
      } else {
        puissance_moyenne_formatte <- paste(round(puissance_moyenne_w / 1000000, 2), " MWh")
      }
      
      valueBox(
        puissance_moyenne_formatte,
        "Puissance Moyenne",
        icon = icon("tachometer-alt"),
        color = "green",
        width = NULL
      )
    })
    
    # Affichage de la puissance maximale pour la consommation sup à 36:
    output$puissance_max <- renderValueBox({
      data <- filtered_data()
      if (input$choix_pas == "demi_horaire") {
        puissance_max_wh <- max(data$total_energie_soutiree_wh, na.rm = TRUE)
        horodate_max <- data$horodate_clean[which.max(data$total_energie_soutiree_wh)]
      } else {
        puissance_max_wh <- max(data$Total_energie, na.rm = TRUE)
        horodate_max <- data$date[which.max(data$Total_energie)]
      }
      
      # Gestion des NA et Inf
      if (is.infinite(puissance_max_wh) || is.na(puissance_max_wh)) {
        puissance_max_formatte <- "N/A"
        horodate_max <- ""
      } else {
        # Arrondi et choix de l'unité (Wh, kWh, MWh)
        if (puissance_max_wh < 1000) {
          puissance_max_formatte <- paste(round(puissance_max_wh), " Wh")
        } else if (puissance_max_wh < 1000000) {
          puissance_max_formatte <- paste(round(puissance_max_wh / 1000, 2), " kWh")
        } else {
          puissance_max_formatte <- paste(round(puissance_max_wh / 1000000, 2), " MWh")
        }
      }
      
      valueBox(
        HTML(paste(puissance_max_formatte, "<br>", "à", horodate_max)), 
        "Puissance Maximale",
        icon = icon("chart-line"),
        color = "yellow"
      )
    })
    
    # Graphique de l'énergie soutirée en fonction du temps pour consommation sup à 36k:
    output$energy_plot <- renderPlotly({
      data <- filtered_data()
      View(data)
      
      if (input$choix_pas == "demi_horaire") {
        fig <- plot_ly(data, 
                       x = ~ as.POSIXct(paste(input$date, heure), format = "%Y-%m-%d %H:%M:%S"),
                       y = ~ total_energie_soutiree_wh/nb_points_soutirage, 
                       color = ~ paste(data$secteur_activite, data$plage_de_puissance_souscrite, data$region, sep = "_"),
                       type = 'scatter', mode = 'line', name = ~paste(data$secteur_activite, data$region, sep = " - ")) %>%
          add_trace(
            x = ~ as.POSIXct(paste(input$date, heure), format = "%Y-%m-%d %H:%M:%S"),
            y = ~ courbe_moyenne_n_1_wh,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.1',
            line = list(dash = 'dot')
          ) %>%
          add_trace(
            x = ~ as.POSIXct(paste(input$date, heure), format = "%Y-%m-%d %H:%M:%S"),
            y = ~ courbe_moyenne_n_2_wh,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.2',
            line = list(dash = 'dash')
          ) %>%
          layout(
            title = "Total énergie soutirée consosup",
            xaxis = list(title = "Plage horaire"),
            yaxis = list(title = "Consommation")
          )
      } else {
        fig <- plot_ly(data, 
                       x = ~ data$date,
                       y = ~ Total_energie, 
                       color = ~ paste(data$secteur_activite, data$plage_de_puissance_souscrite, data$region, sep = "_"),
                       type = 'scatter', mode = 'line', name = ~paste(data$secteur_activite, data$region, sep = " - ")) %>%
          add_trace(
            x = ~ data$date,
            y = ~ Courbe_Moyenne1,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.1',
            line = list(dash = 'dot')
          ) %>%
          add_trace(
            x = ~ data$date,
            y = ~ Courbe_Moyenne2,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.2',
            line = list(dash = 'dash')
          ) %>%
          layout(
            title = "Total Energie soutirée",
            xaxis = list(title = "date"),
            yaxis = list(title = "Consommation")
          )
      }
      
      fig
    })
    
    # Affichage de la carte pour la consommation sup à 36k:
    output$map <- renderLeaflet({
      if (input$choix_pas == "demi_horaire") {
        debut <- as_hms(paste0(input$heure_debut, ":00"))
        fin <- as_hms(paste0(input$heure_fin,":00"))
        
        conso_sup36_aggregated <- conso_sup36 %>%
          filter(plage_de_puissance_souscrite %in% input$plage_puissance,
                 secteur_activite %in% input$secteur,
                 date %in% input$date,
                 profil == input$profil,
                 heure >= debut & heure <= fin)%>%
          group_by(code_region,nb_points_soutirage,profil) %>%
          summarise(Total_energie = sum(as.numeric(total_energie_soutiree_wh), na.rm = TRUE))
        regions_geo <- regions_geo %>%
          left_join(conso_sup36_aggregated, by = c("code" = "code_region"))
        
        leaflet(data = regions_geo) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~colorBin("YlOrRd", Total_energie)(Total_energie),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            popup = ~paste("region: ", nom, ",", "\nTotal énergie soutirée: ", Total_energie,"\nNb de points d'injection : ",unique(nb_points_soutirage)),
            label = ~paste(nom, ",", "Total énergie soutirée: ", round(Total_energie/10^6,3),"Mkh"),
            labelOptions = labelOptions(
              direction = "center",  
              style = list("font-weight" = "bold", "font-size" = "12px", "color" = "black")
            )
          ) %>%
          addLegend(
            position = "bottomleft",
            pal = colorQuantile("YlOrRd", regions_geo$Total_energie, n = 5),
            values = regions_geo$Total_energie,
            title = "Total énergie soutirée",
            opacity = 0.5
          )
      }else {
        conso_sup36_aggregated <- conso_sup36 %>%
          filter(plage_de_puissance_souscrite %in% input$plage_puissance,
                 secteur_activite %in% input$secteur,
                 profil == input$profil,
                 date >= input$date_range[1] & date <= input$date_range[2])%>%
          group_by(code_region) %>%
          summarise(Total_energie = sum(as.numeric(total_energie_soutiree_wh), na.rm = TRUE))
        regions_geo <- regions_geo %>%
          left_join(conso_sup36_aggregated, by = c("code" = "code_region"))
        
        leaflet(data = regions_geo) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~colorBin("YlOrRd", Total_energie)(Total_energie),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            popup = ~paste("region: ", nom, ",", "\nTotal énergie soutirée: ", Total_energie),
            label = ~paste(nom, ",", "Total énergie soutirée: ", round(Total_energie/10^6,3),"Mkh"),  
            labelOptions = labelOptions(
              direction = "center",  
              style = list("font-weight" = "bold", "font-size" = "12px", "color" = "black") 
            )
          ) %>%
          addLegend(
            position = "bottomleft",
            pal = colorQuantile("YlOrRd", regions_geo$Total_energie, n = 5),
            values = regions_geo$Total_energie,
            title = "Total énergie soutirée",
            opacity = 0.5
          )
        
      }
    })
    
    # Telechargement des données pour la consommation sup à 36k:
    output$download_data <- downloadHandler(
      filename = function() { 
        paste("prod_energies",input$region,input$profil ,input$date, ".csv", sep = "") 
      },
      content = function(file) {
        write.csv(filtered_data(), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
  })
  
  
  # Analyse de la Consommation inférieur à 36k:
  observeEvent(input$tab_36k_inf, {
    output$main_content <- renderUI({
      tagList(h2("Analyse de la Consommation inférieur à 36k "),            
              radioButtons(
                inputId = "choix_pas",  
                label = "Choisissez le pas de temps :",
                choices = c("Demi-horaire" = "demi_horaire", "Quotidien" = "quotidien"),
                selected = "demi_horaire",
                inline = TRUE
              ),
              fluidRow(
                
                # selection de région:
                column(3,
                       selectInput("region", "Sélectionner les regions", 
                                   choices = unique(conso$region), 
                                   selected = unique(conso$region)[1], 
                                   multiple = TRUE)),
                
                # selection du profil:
                column(3,
                       selectInput("profil", "Sélectionner le Profil", 
                                   choices = unique(conso$profil), 
                                   selected = unique(conso$profil)[1]
                       )),
                
                # selection de la plage de puissance:
                column(2,
                       selectInput("plage_puissance", "La plage de puissance", 
                                   choices = unique(conso$plage_de_puissance_souscrite), 
                                   selected = unique(conso$plage_de_puissance_souscrite)[1], 
                                   multiple = TRUE)),
                
                # selection de la date:
                column(2,
                       uiOutput("date_selector")),
                column(1, 
                       conditionalPanel(
                         condition = "input.choix_pas == 'demi_horaire'",
                         selectInput(
                           inputId = "heure_debut",
                           label = "Heure Début", 
                           choices = format(seq(from = as.POSIXct("00:00:00", format = "%H:%M:%S"), 
                                                to = as.POSIXct("23:00:00", format = "%H:%M:%S"), 
                                                by = "30 mins"), 
                                            "%H:%M"),
                           selected = as.POSIXct("00:00:00", format = "%H:%M")
                         )
                       )),
                column(1, 
                       conditionalPanel(
                         condition = "input.choix_pas == 'demi_horaire'", 
                         selectInput(
                           inputId = "heure_fin",
                           label = "Heure Fin", 
                           choices = format(seq(from = as.POSIXct("00:00:00", format = "%H:%M:%S"), 
                                                to = as.POSIXct("23:30:00", format = "%H:%M:%S"), 
                                                by = "30 mins"), 
                                            "%H:%M"),
                           selected = format(as.POSIXct("23:30:00", format = "%H:%M:%S"),"%H:%M")
                         )
                       )),
                
                # boutton pour telecharger les données conso inf à 36k:
                column(2,
                       downloadButton("download_data", "Télécharger les données", 
                                      class = "btn-success"), offset = 10)
              ),
              
              # Affichage des valueBoxOutput pour la consommation inf:
              fluidRow(
                valueBoxOutput("total_energie"),      # Affichage du total de l'énergie
                valueBoxOutput("puissance_moyenne"),  # Affichage de la puissance moyenne
                valueBoxOutput("puissance_max")       # Affichage de la puissance maximale
              ),
              fluidRow(
                column(8, plotlyOutput("energy_plot")),   # Affichage du graphique de l'énergie
                column(4, leafletOutput("map"))           # Affichage de la carte
              )
      )
    })
    
    # Sélection dynamique de la date ou plage de dates selon le choix de l'utilisateur:
    output$date_selector <- renderUI({
      if (input$choix_pas == "demi_horaire") {
        dateInput("date", "date", value = "2024-06-30") 
      } else {
        dateRangeInput("date_range", "Plage de dates", 
                       start = as.Date("2024-06-30") - 5, 
                       end = as.Date("2024-06-30"), 
                       width = 1000)     
      }
    })
    
    observeEvent(input$profil, {
      
      # Filtrer les valeurs de la plage de puissance selon le profil sélectionné
      filtered_plage <- conso %>%
        filter(profil == input$profil) %>%
        pull(plage_de_puissance_souscrite) %>%
        unique()
      
      # Mettre à jour les choix dans le selectInput de la plage de puissance
      updateSelectInput(session, "plage_puissance", 
                        choices = filtered_plage,
                        selected = filtered_plage[1])
    })
    # Filtrage des données en fonction des choix de l'utilisateur:
    filtered_data <- reactive({
      if (input$choix_pas == "demi_horaire") {
        
        # Filtrer les données selon les intervalles de temps:
        debut <- as_hms(paste0(input$heure_debut, ":00"))
        fin <- as_hms(paste0(input$heure_fin,":00"))
        
        # Filtrage des données en fonction des paramètres choisis:
        conso_filtered <- conso %>%
          filter(region %in% input$region,
                 plage_de_puissance_souscrite %in% input$plage_puissance,
                 date %in% input$date,
                 profil == input$profil,
                 heure >= debut & heure <= fin)
      }
      else {
        conso_filtered <- conso %>%
          filter(region %in% input$region,
                 plage_de_puissance_souscrite %in% input$plage_puissance,
                 profil == input$profil,
                 date >= input$date_range[1] & date <= input$date_range[2])%>%
          group_by(date,region,plage_de_puissance_souscrite) %>%
          summarise(Total_energie = sum(total_energie_soutiree_wh, na.rm = TRUE),Courbe_Moyenne1=sum(courbe_moyenne_n_1_wh, na.rm = TRUE),Courbe_Moyenne2=sum(courbe_moyenne_n_2_wh, na.rm = TRUE))
      }
      return(conso_filtered)
    })
    
    
    # Affichage des valeurs de l'énergie totale:
    output$total_energie <- renderValueBox({
      data <- filtered_data()
      if (input$choix_pas == "demi_horaire") {
        total_energie_wh <- sum(data$total_energie_soutiree_wh, na.rm = TRUE)
      } else {
        total_energie_wh <- sum(data$Total_energie, na.rm = TRUE)
      }
      
      # Gestion des NA et Inf (Important !)
      if (is.infinite(total_energie_wh) || is.na(total_energie_wh)) {
        total_energie_formatte <- "N/A"
      } else {
        # Arrondi et choix de l'unité (Wh, kWh, MWh)
        if (total_energie_wh < 1000) {
          total_energie_formatte <- paste(round(total_energie_wh), " Wh")
        } else if (total_energie_wh < 1000000) {
          total_energie_formatte <- paste(round(total_energie_wh / 1000, 2), " kWh")
        } else {
          total_energie_formatte <- paste(round(total_energie_wh / 1000000, 2), " MWh")
        }
      }
      
      valueBox(
        total_energie_formatte,
        "Total Energie soutirée",
        icon = icon("bolt"),
        color = "light-blue"
      )
    })
    
    
    # Affichage de la puissance moyenne:
    
    output$puissance_moyenne <- renderValueBox({
      data <- filtered_data()
      
      if (input$choix_pas == "demi_horaire") {
        puissance_moyenne_wh <- mean(data$total_energie_soutiree_wh, na.rm = TRUE)
      } else {
        puissance_moyenne_wh <- mean(data$Total_energie, na.rm = TRUE)
      }
      
      # Gestion des NA et Inf:
      if (is.infinite(puissance_moyenne_wh) || is.na(puissance_moyenne_wh)) {
        puissance_moyenne_formatte <- "N/A"
      } else {
        # Arrondi et choix de l'unité (Wh, kWh, MWh)
        if (puissance_moyenne_wh < 1000) {
          puissance_moyenne_formatte <- paste(round(puissance_moyenne_wh), " Wh")
        } else if (puissance_moyenne_wh < 1000000) {
          puissance_moyenne_formatte <- paste(round(puissance_moyenne_wh / 1000, 2), " kWh")
        } else {
          puissance_moyenne_formatte <- paste(round(puissance_moyenne_wh / 1000000, 2), " MWh")
        }
      }
      
      valueBox(
        puissance_moyenne_formatte,
        "Puissance Moyenne",
        icon = icon("tachometer-alt"),
        color = "green",
        width = NULL
      )
    })
    
    # Affichage de la puissance maximale:
    output$puissance_max <- renderValueBox({
      data <- filtered_data()
      if (input$choix_pas == "demi_horaire") {
        puissance_max_wh <- max(data$total_energie_soutiree_wh, na.rm = TRUE)
        horodate_max <- data$horodate_clean[which.max(data$total_energie_soutiree_wh)]
      } else {
        puissance_max_wh <- max(data$Total_energie, na.rm = TRUE)
        horodate_max <- data$date[which.max(data$Total_energie)]
      }
      
      # Gestion des NA et Inf 
      if (is.infinite(puissance_max_wh) || is.na(puissance_max_wh)) {
        puissance_max_formatte <- "N/A"
        horodate_max <- "" 
      } else {
        # Arrondi et choix de l'unité (Wh, kWh, MWh)
        if (puissance_max_wh < 1000) {
          puissance_max_formatte <- paste(round(puissance_max_wh), " Wh")
        } else if (puissance_max_wh < 1000000) {
          puissance_max_formatte <- paste(round(puissance_max_wh / 1000, 2), " kWh")
        } else {
          puissance_max_formatte <- paste(round(puissance_max_wh / 1000000, 2), " MWh")
        }
      }
      
      valueBox(
        HTML(paste(puissance_max_formatte, "<br>", "à", horodate_max)),
        "Puissance Maximale",
        icon = icon("chart-line"),
        color = "yellow"
      )
    })
    
    
    
    # Graphique de l'énergie en fonction du temps pour la consommation inf:
    output$energy_plot <- renderPlotly({
      data <- filtered_data()
      View(data)
      if (input$choix_pas == "demi_horaire") {
        
        fig <- plot_ly(data, 
                       x = ~ as.POSIXct(paste(input$date, heure), format = "%Y-%m-%d %H:%M:%S"),
                       y = ~ total_energie_soutiree_wh/nb_points_soutirage, 
                       color = ~ paste(data$plage_de_puissance_souscrite, data$region, sep = "_"),
                       type = 'scatter', mode = 'line', name = ~region) %>%
          add_trace(
            x = ~ as.POSIXct(paste(input$date, heure), format = "%Y-%m-%d %H:%M:%S"),
            y = ~ courbe_moyenne_n_1_wh,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.1',
            line = list(dash = 'dot', color = ~ paste(data$plage_de_puissance_souscrite, data$region, sep = "_"))
          ) %>%
          add_trace(
            x = ~ as.POSIXct(paste(input$date, heure), format = "%Y-%m-%d %H:%M:%S"),
            y = ~ courbe_moyenne_n_2_wh,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.2',
            line = list(dash = 'dash', color = ~ paste(data$plage_de_puissance_souscrite, data$region, sep = "_"))
          ) %>%
          layout(
            title = "Total énergie soutirée consoinf",
            xaxis = list(title = "Plage horaire"),
            yaxis = list(title = "Consommation")
          )
      }else {
        fig <- plot_ly(data, 
                       x = ~ data$date,
                       y = ~ Total_energie, 
                       color = ~ paste(data$plage_de_puissance_souscrite, data$region, sep = "_"),
                       type = 'scatter', mode = 'line', name = ~region) %>%
          add_trace(
            x = ~ data$date,
            y = ~ Courbe_Moyenne1,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.1',
            line = list(dash = 'dot', color = ~ paste(data$plage_de_puissance_souscrite, data$region, sep = "_"))
          ) %>%
          add_trace(
            x = ~ data$date,
            y = ~ Courbe_Moyenne2,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.2',
            line = list(dash = 'dash', color = ~ paste(data$plage_de_puissance_souscrite, data$region, sep = "_"))
          ) %>%
          layout(
            title = "Total énergie soutirée consoinf",
            xaxis = list(title = "date"),
            yaxis = list(title = "Consommation")
          )
      }
      
      fig
      
    })
    
    # Affichage de la carte pour la consommation inf:
    output$map <- renderLeaflet({
      if (input$choix_pas == "demi_horaire") {
        debut <- as_hms(paste0(input$heure_debut, ":00"))
        fin <- as_hms(paste0(input$heure_fin,":00"))
        
        conso_aggregated <- conso %>%
          filter(plage_de_puissance_souscrite %in% input$plage_puissance,
                 date %in% input$date,
                 profil == input$profil,
                 heure >= debut & heure <= fin)%>%
          group_by(code_region,nb_points_soutirage,profil) %>%
          summarise(Total_energie = sum(as.numeric(total_energie_soutiree_wh), na.rm = TRUE))
        regions_geo <- regions_geo %>%
          left_join(conso_aggregated, by = c("code" = "code_region"))
        
        leaflet(data = regions_geo) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~colorBin("YlOrRd", Total_energie)(Total_energie),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            popup = ~paste("region: ", nom, ",", "\nTotal énergie soutirée: ", Total_energie,"\nNb de points d'injection : ",unique(nb_points_soutirage)),
            label = ~paste(nom, ",", "Total énergie soutirée: ", round(Total_energie/10^6,3),"Mkh"),
            labelOptions = labelOptions(
              direction = "center",  
              style = list("font-weight" = "bold", "font-size" = "12px", "color" = "black")
            )
          ) %>%
          addLegend(
            position = "bottomleft",
            pal = colorQuantile("YlOrRd", regions_geo$Total_energie, n = 5),
            values = regions_geo$Total_energie,
            title = "Total énergie soutirée",
            opacity = 0.5
          )
      }else {
        conso_aggregated <- conso %>%
          filter(plage_de_puissance_souscrite %in% input$plage_puissance,
                 profil == input$profil,
                 date >= input$date_range[1] & date <= input$date_range[2])%>%
          group_by(code_region) %>%
          summarise(Total_energie = sum(as.numeric(total_energie_soutiree_wh), na.rm = TRUE))
        regions_geo <- regions_geo %>%
          left_join(conso_aggregated, by = c("code" = "code_region"))
        
        leaflet(data = regions_geo) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~colorBin("YlOrRd", Total_energie)(Total_energie),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            popup = ~paste("region: ", nom, ",", "\nTotal énergie soutirée: ", Total_energie),
            label = ~paste(nom, ",", "Total énergie soutirée: ", round(Total_energie/10^6,3),"Mkh"),  
            labelOptions = labelOptions(
              direction = "center",  
              style = list("font-weight" = "bold", "font-size" = "12px", "color" = "black") 
            )
          ) %>%
          addLegend(
            position = "bottomleft",
            pal = colorQuantile("YlOrRd", regions_geo$Total_energie, n = 5),
            values = regions_geo$Total_energie,
            title = "Total énergie soutirée",
            opacity = 0.5
          )
        
      }
    })
    
    # Telechargement des données pour la consommation inf:
    output$download_data <- downloadHandler(
      filename = function() { 
        paste("prod_energies",input$region,input$profil ,input$date, ".csv", sep = "") 
      },
      content = function(file) {
        write.csv(filtered_data(), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
  })
  
  
  # Analyse de la Production d'Energie:
  observeEvent(input$tab_prod, {
    output$main_content <- renderUI({
      tagList( 
        # Titre principal de la section:
        h2("Analyse de la Production d'Energie"),
        
        # Choix du pas de temps (Demi-horaire ou Quotidien):
        radioButtons(
          inputId = "choix_pas",
          label = "Choisissez le pas de temps :",
          choices = c("Demi-horaire" = "demi_horaire", "Quotidien" = "quotidien"),
          selected = "demi_horaire",
          inline = TRUE
        ),
        fluidRow(
          # Selection de la région:
          column(3,
                 selectInput("region", "Sélectionner les regions", 
                             choices = unique(df$region), 
                             selected = unique(df$region)[1], 
                             multiple = TRUE)),
          
          # selection de la filière;
          column(2,
                 selectInput("fliere", "Sélectionner la filère", 
                             choices = unique(df$filiere_de_production), 
                             selected = unique(df$filiere_de_production)[1]
                 )),
          
          # selection de la plage de puissance:
          column(3,
                 selectInput("plage_puissance", "La plage de puissance", 
                             choices = unique(df$plage_de_puissance_injection), 
                             selected = unique(df$plage_de_puissance_injection)[1], 
                             multiple = TRUE)),
          
          
          column(2,
                 # Sélection de la date selon le pas de temps choisi:
                 uiOutput("date_selector")),
          column(1, 
                 # Sélection de l'heure de début en fonction du pas de temps:
                 conditionalPanel(
                   condition = "input.choix_pas == 'demi_horaire'",
                   selectInput(
                     inputId = "heure_debut",
                     label = "Heure Début", 
                     choices = format(seq(from = as.POSIXct("00:00:00", format = "%H:%M:%S"), 
                                          to = as.POSIXct("23:00:00", format = "%H:%M:%S"), 
                                          by = "30 mins"), 
                                      "%H:%M"),
                     selected = as.POSIXct("00:00:00", format = "%H:%M")
                   )
                 )),
          column(1, 
                 # Sélection de l'heure de fin en fonction du pas de temps:
                 conditionalPanel(
                   condition = "input.choix_pas == 'demi_horaire'", 
                   selectInput(
                     inputId = "heure_fin",
                     label = "Heure Fin", 
                     choices = format(seq(from = as.POSIXct("00:00:00", format = "%H:%M:%S"), 
                                          to = as.POSIXct("23:30:00", format = "%H:%M:%S"), 
                                          by = "30 mins"), 
                                      "%H:%M"),
                     selected = format(as.POSIXct("23:30:00", format = "%H:%M:%S"),"%H:%M")
                   )
                 )),
          
          # Bouton de téléchargement des données filtrées:
          column(2,
                 downloadButton("download_data", "Télécharger les données", 
                                class = "btn-success"), offset = 10)
        ),
        
        # Affichage des indicateurs sous forme de cartes de valeur
        fluidRow(
          valueBoxOutput("total_energie"),
          valueBoxOutput("puissance_moyenne"),
          valueBoxOutput("puissance_max")
        ),
        fluidRow(
          # Affichage du graphique de production d'énergie et de la carte géographique:
          column(8, plotlyOutput("energy_plot")),
          column(4, leafletOutput("map"))
        )
      )
    })
    
    # Sélection de la date en fonction du choix du pas de temps:
    output$date_selector <- renderUI({
      if (input$choix_pas == "demi_horaire") {
        dateInput("date", "date", value = "2024-06-30")
      } else {
        dateRangeInput("date_range", "Plage de dates", 
                       start = as.Date("2024-06-30") - 5, 
                       end = as.Date("2024-06-30"), 
                       width = 1000)
      }
    })
    filtered_data <- reactive({
      if (input$choix_pas == "demi_horaire") {
        # Filtrer les données selon les intervalles de temps
        debut <- as_hms(paste0(input$heure_debut, ":00"))
        fin <- as_hms(paste0(input$heure_fin,":00"))
        
        
        # Fonction réactive pour filtrer les données en fonction des choix:
        df_filtered <- df %>%
          filter(region %in% input$region,
                 plage_de_puissance_injection %in% input$plage_puissance,
                 date %in% input$date,
                 filiere_de_production == input$fliere,
                 heure >= debut & heure <= fin)
      } else {
        if(input$fliere != "F0 : Total toutes filières"){
          df_filtered <- df %>%
            filter(region %in% input$region,
                   plage_de_puissance_injection %in% input$plage_puissance,
                   date >= input$date_range[1] & date <= input$date_range[2],
                   filiere_de_production == input$fliere) %>%
            group_by(date,region,plage_de_puissance_injection) %>%
            summarise(Total_energie = sum(total_energie_injectee_wh/nb_points_injection, na.rm = TRUE),Courbe_Moyenne1=sum(courbe_moyenne_n_1_wh, na.rm = TRUE),Courbe_Moyenne2=sum(courbe_moyenne_n_2_wh, na.rm = TRUE))
        }else{
          df_filtered <- df %>%
            filter(region %in% input$region,
                   plage_de_puissance_injection %in% input$plage_puissance,
                   date >= input$date_range[1] & date <= input$date_range[2],
                   filiere_de_production == "F0 : Total toutes filières") %>%
            group_by(date,region,plage_de_puissance_injection) %>%
            summarise(Total_energie = sum(total_energie_injectee_wh, na.rm = TRUE),Courbe_Moyenne1=sum(courbe_moyenne_n_1_wh, na.rm = TRUE),Courbe_Moyenne2=sum(courbe_moyenne_n_2_wh, na.rm = TRUE))
        }
      }
      return(df_filtered)
    })
    
    
    # Calcul et affichage de l'indicateur Total Energie Injectée:
    output$total_energie <- renderValueBox({
      data <- filtered_data()
      if (input$choix_pas == "demi_horaire") {
        if(input$fliere == "F0 : Total toutes filières"){data%>% filter(filiere_de_production == "F0 : Total toutes filières")}
        else{data%>% filter(filiere_de_production != "F0 : Total toutes filières")}
        total_energie <- sum(data$total_energie_injectee_wh, na.rm = TRUE)
      } else {
        total_energie <- sum(data$Total_energie, na.rm = TRUE)
      }
      
      # Conversion en kWh et arrondi
      total_energie_kwh <- round(total_energie / 1000)
      
      # Formatage avec des unités adaptées
      if (total_energie_kwh < 1000) {
        total_energie_formatte <- paste(format(total_energie_kwh, big.mark = " "), " kWh") # Ajout d'un espace
      } else if (total_energie_kwh < 1000000) {
        total_energie_formatte <- paste(format(round(total_energie_kwh / 1000, 2), big.mark = " "), " MWh") # Arrondi à 2 décimales
      } else {
        total_energie_formatte <- paste(format(round(total_energie_kwh / 1000000, 2), big.mark = " "), " GWh") # Arrondi à 2 décimales
      }
      
      valueBox(
        total_energie_formatte,
        "Total Energie Injectée",
        icon = icon("bolt"),
        color = "light-blue"
      )
    })
    
    # Calcul et affichage de la Puissance Moyenne:
    output$puissance_moyenne <- renderValueBox({
      data <- filtered_data()
      
      if (input$choix_pas == "demi_horaire") {
        
        if(input$fliere == "F0 : Total toutes filières"){data%>% filter(filiere_de_production == "F0 : Total toutes filières")}       
        else{data%>% filter(filiere_de_production != "F0 : Total toutes filières")}
        puissance_moyenne <- mean(data$total_energie_injectee_wh, na.rm = TRUE)
      }else {
        puissance_moyenne <- mean(data$Total_energie, na.rm = TRUE)
      }
      # Gestion des NA et Inf:
      if (is.infinite(puissance_moyenne) || is.na(puissance_moyenne)) {
        puissance_moyenne_formatte <- "N/A"
      } else {
        # Arrondi et choix de l'unité (Wh, kWh, MWh)
        if (puissance_moyenne < 1000) {
          puissance_moyenne_formatte <- paste(round(puissance_moyenne), " Wh")
        } else if (puissance_moyenne < 1000000) {
          puissance_moyenne_formatte <- paste(round(puissance_moyenne / 1000, 2), " kWh")
        } else {
          puissance_moyenne_formatte <- paste(round(puissance_moyenne / 1000000, 2), " MWh")
        }
      }
      
      valueBox(
        puissance_moyenne_formatte,
        "Puissance Moyenne",
        icon = icon("tachometer-alt"),
        color = "green",
        width = NULL
      ) 
      
    })
    
    
    # Calcul et affichage de la Puissance Max:
    output$puissance_max <- renderValueBox({
      data <- filtered_data()
      if (input$choix_pas == "demi_horaire") {
        if(input$fliere == "F0 : Total toutes filières"){data%>% filter(filiere_de_production == "F0 : Total toutes filières")}
        else{data%>% filter(filiere_de_production != "F0 : Total toutes filières")}
        puissance_max <- max(data$total_energie_injectee_wh , na.rm = TRUE)
        horodate_max <- data$horodate_clean[which.max(data$total_energie_injectee_wh)]
        
      } else {
        puissance_max <- max(data$Total_energie, na.rm = TRUE)
        horodate_max <- data$date[which.max(data$Total_energie)]
      }
      
      # Gestion des NA et Inf 
      if (is.infinite(puissance_max) || is.na(puissance_max)) {
        puissance_max_formatte <- "N/A"
        horodate_max <- "" 
      } else {
        # Arrondi et choix de l'unité (Wh, kWh, MWh)
        if (puissance_max < 1000) {
          puissance_max_formatte <- paste(round(puissance_max), " Wh")
        } else if (puissance_max < 1000000) {
          puissance_max_formatte <- paste(round(puissance_max / 1000, 2), " kWh")
        } else {
          puissance_max_formatte <- paste(round(puissance_max / 1000000, 2), " MWh")
        }
      }
      
      valueBox(
        HTML(paste(puissance_max_formatte, "<br>", "à", horodate_max)),
        "Puissance Maximale",
        icon = icon("chart-line"),
        color = "yellow"
      )
    })
    
    
    # Graphique de production d'énergie:
    output$energy_plot <- renderPlotly({
      data <- filtered_data()
      if (input$choix_pas == "demi_horaire") {
        if(input$fliere == "F0 : Total toutes filières"){data%>% filter(filiere_de_production == "F0 : Total toutes filières")}       
        else{data%>% filter(filiere_de_production != "F0 : Total toutes filières")}
        
        fig <- plot_ly(data, 
                       x = ~ as.POSIXct(paste(input$date, heure), format = "%Y-%m-%d %H:%M:%S"),
                       y = ~ total_energie_injectee_wh/nb_points_injection, 
                       color = ~ paste(data$plage_de_puissance_injection, data$region, sep = "_"),
                       type = 'scatter', mode = 'line', name = ~region) %>%
          add_trace(
            x = ~ as.POSIXct(paste(input$date, heure), format = "%Y-%m-%d %H:%M:%S"),
            y = ~ courbe_moyenne_n_1_wh,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.1',
            line = list(dash = 'dot', color = ~ paste(data$plage_de_puissance_injection, data$region, sep = "_"))
          ) %>%
          add_trace(
            x = ~ as.POSIXct(paste(input$date, heure), format = "%Y-%m-%d %H:%M:%S"),
            y = ~ courbe_moyenne_n_2_wh,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.2',
            line = list(dash = 'dash', color = ~ paste(data$plage_de_puissance_injection, data$region, sep = "_"))
          ) %>%
          layout(
            title = "Totale d'énergie injéctées",
            xaxis = list(title = "Plage horaire"),
            yaxis = list(title = "Production")
          )
      }else {
        fig <- plot_ly(data, 
                       x = ~ data$date,
                       y = ~ Total_energie, 
                       color = ~ paste(data$plage_de_puissance_injection, data$region, sep = "_"),
                       type = 'scatter', mode = 'line', name = ~region) %>%
          add_trace(
            x = ~ data$date,
            y = ~ Courbe_Moyenne1,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.1',
            line = list(dash = 'dot', color = ~ paste(data$plage_de_puissance_injection, data$region, sep = "_"))
          ) %>%
          add_trace(
            x = ~ data$date,
            y = ~ Courbe_Moyenne2,
            type = 'scatter',
            mode = 'line',
            name = 'Courbe Moyenne n.2',
            line = list(dash = 'dash', color = ~ paste(data$plage_de_puissance_injection, data$region, sep = "_"))
          ) %>%
          layout(
            title = "Totale d'énergie injéctées",
            xaxis = list(title = "date"),
            yaxis = list(title = "Production")
          )
      }
      
      fig
      
    })
    
    
    # Carte géographique pour la production d'énergie:
    output$map <- renderLeaflet({
      if (input$choix_pas == "demi_horaire") {
        debut <- as_hms(paste0(input$heure_debut, ":00"))
        fin <- as_hms(paste0(input$heure_fin,":00"))
        
        df_aggregated <- df %>%
          filter(plage_de_puissance_injection %in% input$plage_puissance,
                 date %in% input$date,
                 filiere_de_production == input$fliere,
                 heure >= debut & heure <= fin)%>%
          group_by(code_region,nb_points_injection,filiere_de_production) %>%
          summarise(Total_energie = sum(as.numeric(total_energie_injectee_wh), na.rm = TRUE))
        regions_geo <- regions_geo %>%
          left_join(df_aggregated, by = c("code" = "code_region"))
        
        leaflet(data = regions_geo) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~colorBin("YlOrRd", Total_energie)(Total_energie),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            popup = ~paste("region: ", nom, ",", "\nTotal énergie injectée: ", Total_energie,"\nNb de points d'injection : ",unique(nb_points_injection)),
            label = ~paste(nom, ",", "Total énergie injectée: ", round(Total_energie/10^6,3),"Mkh"),
            labelOptions = labelOptions(
              direction = "center",  
              style = list("font-weight" = "bold", "font-size" = "12px", "color" = "black")
            )
          ) %>%
          addLegend(
            position = "bottomleft",
            pal = colorQuantile("YlOrRd", regions_geo$Total_energie, n = 5),
            values = regions_geo$Total_energie,
            title = "Total énergie injectée",
            opacity = 0.5
          )
      }else {
        df_aggregated <- df %>%
          filter(plage_de_puissance_injection %in% input$plage_puissance,
                 filiere_de_production == input$fliere,
                 date >= input$date_range[1] & date <= input$date_range[2])%>%
          group_by(code_region) %>%
          summarise(Total_energie = sum(as.numeric(total_energie_injectee_wh), na.rm = TRUE))
        regions_geo <- regions_geo %>%
          left_join(df_aggregated, by = c("code" = "code_region"))
        
        leaflet(data = regions_geo) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~colorBin("YlOrRd", Total_energie)(Total_energie),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            popup = ~paste("region: ", nom, ",", "\nTotal énergie injectée: ", Total_energie),
            label = ~paste(nom, ",", "Total énergie injectée: ", round(Total_energie/10^6,3),"Mkh"),  
            labelOptions = labelOptions(
              direction = "center",  
              style = list("font-weight" = "bold", "font-size" = "12px", "color" = "black") 
            )
          ) %>%
          addLegend(
            position = "bottomleft",
            pal = colorQuantile("YlOrRd", regions_geo$Total_energie, n = 5),
            values = regions_geo$Total_energie,
            title = "Total énergie injectée",
            opacity = 0.5
          )
        
      }
    })
    
    # telechargement des données:
    output$download_data <- downloadHandler(
      filename = function() { 
        paste("prod_energies",input$region,input$fliere ,input$date, ".csv", sep = "") 
      },
      content = function(file) {
        write.csv(filtered_data(), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
  })
  
}

# Lancer l'application:
shinyApp(ui, server)