# ----------------------------------------------------------------------------------------------------------
#
# Schafkopf - App / Mitgezählt...
#
# Rechnen leicht gemacht
# 
# (c) by Gerhard Füchsl 
#
# Version 1.0
#
# ----------------------------------------------------------------------------------------------------------

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
                  column(1, numericInput("tarifSpiel", "Tarif Spiel", 10)),
                  column(1, numericInput("tarifSolo", "Tarif Solo", 20)),
                  column(4),
                  column(2, actionButton("neueGruppe","Neue Schafkopfrunde", width = "100%"), br(), br(),
                            actionButton("loescheGruppe", "Schafkkopfrunde löschen", width = "100%"))
    ))
  )),
  
  # --- Spielart und Spieler -----------------------------------------------------------------------------
  inputPanel(
              # --- Spielart -------------------------------------------------------------------------------
              wellPanel(radioButtons("spielart1", "Spiel", c("Sauspiel","Solo"))),
              
              # --- Spielart -------------------------------------------------------------------------------
              wellPanel(selectInput("Spielart2", "Spiel", c("Eichel", "Gras", "Herz", "Schelln"))),
              
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
                          column(6, numericInput("anzahlGelegt", "Gelegt", 0)),
                          column(6, numericInput("anzahlLaufende", "Laufende", 0))
                        )
              ),
              
              # --- Spiel dokumentieren --------------------------------------------------------------------
              wellPanel(actionButton("spielAufschreiben", "Aufschreiben", width = "100%"), br(), br(),
                        actionButton("letztesKorrigieren", "Letztes Spiel korrigieren", width = "100%")
              )
    ),
  
    # --- Ausgabe der Spieldaten ---------------------------------------------------------------------------
    mainPanel(actionButton("asdf", "asdf"))
  
  
  ),
  # --- Ende User-Panel ------------------------------------------------------------------------------------
  
  
  # --- Test-Panels ----------------------------------------------------------------------------------------
  # Hier können verschiedene Outputs getestet werden
  
  # t001: Ausgabe der Umwandlungen
  conditionalPanel("false",
                    actionButton("testBerechnung", "Berechne"),
                    textOutput("ergebnisBerechnung"),
                    textOutput("ergebnisBerechnung2")
  )
  # --- Ende Test-Panels -----------------------------------------------------------------------------------
))
