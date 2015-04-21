library(shiny)

shinyUI(fluidPage(
  theme = "yeti.css",
  h3("Analyse des choix post-ECN"),
#  h5("L'application peut bloquer de temps à autre, ne pas hésiter à rafraîchir, à rééssayer à 10 minutes d'intervalle."),
  br(),
  fluidRow(
    column(2,h4("Options globales :")),
    
    column(3,  
      selectInput(inputId = "Spe",
        label = "Quelle spécialité ?",
        choices = list(
          "Toutes les spécialités"=000,
          "Anatomie et cytologie pathologique"=012,
          "Anesthésie réanimation"=004,
          "Biologie médicale"=003, 
          "Cardiologie et maladies vasculaires"=013,
          "Chirurgie générale"=028, 
          "Chirurgie Orale"=032, 
          "Dermatologie et vénérologie"=014,                            
          "Endocrinologie, diabète, maladies métaboliques"=015,         
          "Gastro-entérologie et hépatologie"=016,                      
          "Génétique médicale"=017,
          "Gynécologie médicale"=005,  
          "Gynécologie obstétrique"=009, 
          "Hématologie"=018,
          "Médecine du Travail"=006,
          "Médecine générale"=011,   
          "Médecine interne"=019,                                       
          "Médecine nucléaire"=020,                                     
          "Médecine physique et de réadaptation"=021,                   
          "Néphrologie"=022,
          "Neurochirurgie"=029,  
          "Neurologie"=023,                                             
          "Oncologie"=024,
          "Ophtalmologie"=030, 
          "ORL"=031,
          "Pédiatrie"=002, 
          "Pneumologie"=025,
          "Psychiatrie"=010, 
          "Radiodiagnostic et imagerie médicale"=026,                   
          "Rhumatologie"=027,
          "Santé publique"=007
        )
      )
    ),
                
    column(3, 
      selectInput(inputId = "Ville",
        label = "Quelle ville ?",
        choices = list(
          "Toutes les villes"=000,
          "Aix Marseille"=034,
          "Amiens"=021,
          "Angers"=028,
          "Antilles-Guyane"=041,
          "Besancon"=016,
          "Bordeaux"=037,
          "Brest"=029,
          "Caen"=022,            
          "Clermont-Ferrand"=038,
          "Dijon"=017,
          "Grenoble"=025,
          "Ile de France"=015,
          "Lille"=023,
          "Limoges"=039,
          "Lyon"=026,
          "Montpellier"=035,
          "Nancy"=018,
          "Nantes"=030,
          "Nice"=036,
          "Ocean Indien"=042,
          "Poitiers"=031,
          "Reims"=019,
          "Rennes"=032,
          "Rouen"=024,
          "Saint Etienne"=027,
          "Strasbourg"=020,
          "Toulouse"=040,
          "Tours"=033
        )
      )
    )
  ),
                
  br(),

  fluidRow(
    column(2,h4("Base de données utilisée : ")),
    
    column(9,
      radioButtons(inputId = "ChoixBDD", 
        label = "",
        choices = list("2010"="affectations2010","2011"="affectations2011","2012"="affectations2012","2013"="affectations2013","01/09/2014 (simulations)"="simulations2014","27/09/2014 (affectations)"="affectations2014"),
        selected = "affectations2014", inline = TRUE
      )
    )
  ),
#  fluidRow(
#    column(2, offset=5,
#    actionButton("runButton", h4("Actualisation"))
#    )
#  ),
                  
  hr(),
              
#  p("Temps de chargement : ~20s. Données actualisées du ", textOutput("date",inline = TRUE), " à ",textOutput("h",inline = TRUE), "."),

  p(textOutput("sim1"), inline=TRUE),
  
#  p(textOutput("sim2"), inline=TRUE),

#  br(),
  
  tabsetPanel(
    tabPanel(h4("Données brutes"), 
      dataTableOutput("table")
    ),
                    
    tabPanel(h4("Données agrégées"), 
      h3("Données par spécialités"),
      dataTableOutput("agreg_spe"),
      h3("Données par villes"),
      dataTableOutput("agreg_ville")
    ),
                    
    tabPanel(h4("Distributions"),
      fluidRow(
        column(2,h4("Options des graphiques  : ")),
         
        column(5,
          selectInput(inputId = "meth.order",
            label = "Critère de classement des villes et spécialités ?",
            choices = list(
              "Médiane"="median",
              "Moyenne"="mean",
              "Maximum"="max",
              "Minimum"="min",
              "Troisième quart"="TQ",
              "Premier quart"="PQ"
            )
          )
        )
      ),
      hr(),
      h3("Classement en fonction des spécialités"),
      h6("Il est possible de classer les spécialités selon différentes méthodes"),
      plotOutput("plot_spe"),
      h3("Classement en fonction des villes"),
      h6("Il est possible de classer les villes selon différentes méthodes"),
      plotOutput("plot_ville")
    ),
    
    tabPanel(h4("Postes pourvus"),
      #conditionalPanel(
      #condition = "input.ChoixBDD == 'affectations2014' || input.ChoixBDD == 'simulations2014' || input.ChoixBDD == 'affectations2010' || input.ChoixBDD == 'affectations2011'|| input.ChoixBDD == 'affectations2012'",
      fluidRow(
        column(2,h4("Options des graphiques : ")),
               
        column(3,
          radioButtons(inputId="Choix.indic", 
            label="Indicateur :",
            choices= list("Pourcentage"="pourcent","Nombre absolu"="abs","Offre"="offre"),
            selected = "pourcent", inline = TRUE
          )
        ),
               
        column(2,
          radioButtons(inputId="Restcand", 
            label="Restreindre les candidats :",
            choices= list("oui"="oui","non"="non"),
            selected = "non", inline = TRUE
          )
        ),
               
        column(4,
          column(4,
            conditionalPanel(
              condition = "input.Restcand == 'oui'",
              sliderInput(
                inputId="Rang.min",
                label="Borne 1", 
                min=1,
                max=8304,
                value=1,
                step = 50
              )
            )
          ),
                      
          column(4, offset=2,
            conditionalPanel(
              condition = "input.Restcand == 'oui'",
              sliderInput(
                inputId="Rang.max",
                label="Borne 2", 
                min=1,
                max=8304,
                value=8304,
                step = 50
              )
            )
          )
        )
      ),
      hr(),
      h3("Nombre/pourcentage de postes pourvus par spécialités"),
      h6("Il est possible de restreindre par classement et par ville"),
      plotOutput("plot_sel_spe"),
      h3("Nombre/pourcentage de postes pourvus par villes"),
      h6("Il est possible de restreindre par classement et par spécialité"),
      plotOutput("plot_sel_ville")
    #),
    #conditionalPanel(
    #  condition = "input.ChoixBDD != 'affectations2014' && input.ChoixBDD != 'simulations2014' && input.ChoixBDD != 'affectations2010' && input.ChoixBDD != 'affectations2011' && input.ChoixBDD != 'affectations2012'",
    #  p("En travaux, non disponible pour l'instant")
    #)
    ),
    
    tabPanel(h4("Attractivités"),
      #conditionalPanel(
      #condition = "input.ChoixBDD == 'affectations2014' || input.ChoixBDD == 'simulations2014'|| input.ChoixBDD == 'affectations2010'|| input.ChoixBDD == 'affectations2011'|| input.ChoixBDD == 'affectations2012'",
               
      fluidRow(
        column(2,h4("Options des graphiques : ")),
               
        column(6,
          radioButtons(inputId="Choix.calc", 
            label="Choix de la méthode de calcul (valable que si une ville ou une spé ont été selectionnées):",
            choices= list("Rang global"="glob","Rang dans la spé/subdivision"="sel"),
            selected = "glob", inline = TRUE
          )
        )
      ),
      hr(),
      h3("Attractivité des subdivisions selon la spécialité"),
      plotOutput("plot_attr_spe"),
      h3("Attractivité des spécialités selon la subdivision"),
      plotOutput("plot_attr_ville"),
      hr(),
      h3("Explication :",style="text-decoration:underline;"),
      p("L'attractivité est gradée sur une échelle allant de 0 à 100, plus elle est proche de 100, meilleure est l'attractivité. Elle a été calculée selon la formule :"),
      code("Attractivité = 100 - (Sx - SXmin) / (SXmax - SXmin) * 100"), 
      br(),
      br(),
      p("Avec :",style="text-decoration:underline;"),
      p("Sx la somme des rangs effectifs"),
      p("SXmin la somme minimal atteignable des rangs selon la subdivision et/ou la spécialité"),
      p("SXmax la somme maximal des rangs atteignables selon la subdivision et/ou la spécialité"),
      p("Aux postes non remplis ont été affecté le dernier rang choisi + 1"),
      p("Le choix de la méthode de calcul fait varier le façon de classer les étudiants. Si on prend l'exemple du Xe du classement, classé premier dans sa spécialité. Avec la méthode \"globale\" son rang sera X avec la méthode \"dans\" son rang sera 1."),
      p("L'attactivité reflète donc soit la capacité à attirer les meilleurs compte-tenu du classement global, soit les meilleurs compte-tenu d'un classement spécifique (au sein d'une spécialité ou d'une subdivision).")
    #),  
    #conditionalPanel(
    # condition = "input.ChoixBDD != 'affectations2014' && input.ChoixBDD != 'simulations2014' && input.ChoixBDD != 'affectations2010' && input.ChoixBDD != 'affectations2011' && input.ChoixBDD != 'affectations2012'",
    #  p("En travaux, non disponible pour l'instant")
    #)
    ),
                    
    tabPanel(h4("Légendes"),
      h3("Notice",style="text-decoration:underline;"),
      p("Les étudiants CESP comme ceux des armées ne sont pas inclus aux analyses pour 2014 (y compris pour l'avancement des choix)."),
      p("Pour les autres années, ils sont incorporés aux résultats sans distinction possible. De ce fait une correction a du être faite sur les données d'offre de poste : les offres de postes classiques et celles pour les CESP ont été fusionnées."),
      p("L\'ensemble des données sont issues de : ", a("https://www.cngsante.fr/chiron2014/celine/listing.html", href="https://www.cngsante.fr/chiron2014/celine/listing.html")),
      h3("Légende spécialités"),
      tableOutput("legende_spe"),
      h3("Légende villes"),
      tableOutput("legende_ville")
    )
  )
))