# R Shiny – Datavisualisation de la Consommation et Production Électrique

Application interactive développée avec **R Shiny** permettant de visualiser la consommation et la production électrique en France à différentes échelles temporelles et géographiques.


##  Objectif
L’objectif de ce projet est de construire une application **R Shiny** permettant d’explorer et d’analyser :
- la consommation électrique (< 36 kVA et ≥ 36 kVA),
- la production électrique,

au pas **demi-horaire** ou **journalier**, pour une ou plusieurs régions françaises.

Les données sont issues du portail **Enedis Open Data** et sont téléchargées manuellement (sans API).

##  Données utilisées
Trois jeux de données Enedis :
- Production régionale  
- Consommation < 36 kVA  
- Consommation ≥ 36 kVA  

Source : https://data.enedis.fr

##  Fonctionnalités de l’application

###  Onglets
- **Consommation < 36 kVA**
- **Consommation ≥ 36 kVA**
- **Production**

###  Inputs
- Type d’affichage :
  - courbe totale
  - nombre de points
  - courbe moyenne
- Période d’intérêt (DateRangeInput)
- Une ou plusieurs régions (somme automatique)
- Profils réglementaires
- Plages de puissance
- Choix du pas de temps : demi-horaire ou journalier
- Secteurs d’activité (≥ 36 kVA)
- Filières et puissances d’injection (production)

###  Outputs
- Graphique interactif :
  - par profil × région (conso < 36 kVA)
  - par secteur × région (conso ≥ 36 kVA)
  - par filière × région (production)
- ValueBox :
  - somme totale
  - puissance moyenne
  - puissance maximale
  - horodate (ou jour) du pic
- Bouton de téléchargement des données affichées

##  Résultats et apports
- Visualisation claire et interactive de séries temporelles énergétiques
- Agrégation dynamique multi-régions
- Outil d’aide à l’analyse des tendances de consommation et production

##  Pistes d’amélioration
- Connexion directe via API Enedis
- Ajout de prévisions (ARIMA / Prophet)
- Déploiement sur shinyapps.io
- Comparaison inter-annuelle

##  Environnement technique
- **Langage** : R
- **Framework** : Shiny
- **Librairies** : shiny, ggplot2, dplyr, lubridate, plotly, DT
- **Données** : Enedis Open Data
