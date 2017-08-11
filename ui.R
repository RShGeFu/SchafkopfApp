# ##########################################################################################################
#
# Schafkopf - App / Mitgezählt...
#
# Rechnen leicht gemacht
# 
# (c) by Gerhard Füchsl 
#
# Version 1.0
#
# ##########################################################################################################


# --- Benötigte Packages -----------------------------------------------------------------------------------
library(shiny)

# --- Aufbau des UI ----------------------------------------------------------------------------------------
shinyUI(fluidPage(
  
  # --- User-Panel -----------------------------------------------------------------------------------------
  # Hier ist der für den User angezeigte Bereich
  
  conditionalPanel("true",
  
  # --- Titel und Gruppenauswahl ---------------------------------------------------------------------------
  fluidRow(
    # --- Titel --------------------------------------------------------------------------------------------
    column(3, hr(), titlePanel("Schafkopf - mitgezählt ..."), hr()),
    
    # --- Gruppe und Tarife --------------------------------------------------------------------------------
    column(9, wellPanel(
                fluidRow(
                  column(4, selectInput("gruppeWaehlen", "Schafkopfrunde", c("A", "B", "C")) ),
                  column(1, numericInput("tarifSpiel", "Tarif Spiel", NULL)),
                  column(1, numericInput("tarifSolo", "Tarif Solo", NULL)),
                  column(4),
                  column(2, actionButton("neueGruppe","Neue Schafkopfrunde", width = "100%"), br(), br(),
                            actionButton("loescheGruppe", "Schafkkopfrunde löschen", width = "100%"))
    ))
  )),
  
  # --- Spielart und Spieler -------------------------------------------------------------------------------
  inputPanel(
              # --- Spielart -------------------------------------------------------------------------------
              wellPanel(
                  fluidRow(
                    column(6, radioButtons("spielArt1", "Spiel", choiceNames = c("Sauspiel", "Solo"), choiceValues = c(1, 2))),
                    conditionalPanel("input.spielArt1 == 2",
                      column(6, radioButtons("soloArt", "Solo", choiceNames = c("Normal", "Tout", "Si"), choiceValues = c(1, 2, 4)))
                    )
                  )
              ),
              
              # --- Spielart -------------------------------------------------------------------------------
              wellPanel(selectInput("spielArt2", "Spiel", NULL)),
              
              # --- Spieler --------------------------------------------------------------------------------
              wellPanel(h4(strong(textOutput("Spieler1"))),
                        fluidRow(
                          column(6, checkboxInput("sp1", "Spieler")), 
                          column(6, numericInput("pkt1", NULL, 0))
                        )
              ), 
              
              # --- Spieler --------------------------------------------------------------------------------
              wellPanel(h4(strong(textOutput("Spieler2"))),
                        fluidRow(
                          column(6, checkboxInput("sp2", "Spieler")), 
                          column(6, numericInput("pkt2", NULL, 0))
                        )
              ),
              
              # --- Spieler --------------------------------------------------------------------------------
              wellPanel(h4(strong(textOutput("Spieler3"))),
                        fluidRow(
                          column(6, checkboxInput("sp3", "Spieler")), 
                          column(6, numericInput("pkt3", NULL, 0))
                        )
              ), 
              
              # --- Spieler --------------------------------------------------------------------------------
              wellPanel(h4(strong(textOutput("Spieler4"))),
                        fluidRow(
                          column(6, checkboxInput("sp4", "Spieler")), 
                          column(6, numericInput("pkt4", NULL, 0))
                        )
              ),
              
              # --- Doppelungen ----------------------------------------------------------------------------
              wellPanel(fluidRow(
                          column(7, numericInput("anzahlGelegt", "Legen/Spritzen", 0)),
                          column(5, numericInput("anzahlLaufende", "Laufende", 0))
                        )
              ),
              
              # --- Spiel dokumentieren --------------------------------------------------------------------
              wellPanel(actionButton("spielAufschreiben", "Aufschreiben", width = "100%"), br(), br(),
                        actionButton("letztesKorrigieren", "Letztes Spiel korrigieren", width = "100%"),
                        checkboxInput("testPanel01", "TP01")
              )
    ),
  
    # --- Ausgabe der Spieldaten ---------------------------------------------------------------------------
  hr()
  
  ),
  
  # --- Ende User-Panel ------------------------------------------------------------------------------------
  
  
  # --- Test-Panels ----------------------------------------------------------------------------------------
  # Hier können verschiedene Outputs getestet werden
  
  conditionalPanel("input.testPanel01",
                   
                   # t001: Ausgabe der Umwandlungen
                    actionButton("testBerechnung", "Berechne"),
                    textOutput("ergebnisBerechnung"),
                    textOutput("ergebnisBerechnung2"),
                  
                   # t002: Ausgabe der Tests bzgl. der Spielart-Auswahl und der Rückumstellungen
                    hr(),
                    actionButton("testSpieleRB", "Teste Radiobuttons Spiele"),
                    textOutput("ergebnisTestRB"),
                   
                   # t003: Button-Klick -> füllt 3x Punkte aus, 4. wird berechnet
                   hr(),
                   actionButton("testSumPoints", "Teste Punktesummen"),
                   textOutput("ergebnisAusgewaehlt")
  )
  
  # --- Ende Test-Panels -----------------------------------------------------------------------------------
))
