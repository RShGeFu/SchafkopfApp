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
                  column(2, actionButton("neueGruppe","Neue Schafkopfrunde", width = 200),
                            actionButton("loescheGruppe", "Schafkkopfrunde löschen", width = 200))
    ))
  )),
  
  # --- Spielart und Spieler -------------------------------------------------------------------------------
  sidebarLayout(
    # --- Spielart und Spieler -----------------------------------------------------------------------------
    sidebarPanel(
              # --- Spielart -------------------------------------------------------------------------------
              wellPanel(radioButtons("spielart", "Spiel", c("Sauspiel","Solo"))),
              # --- Spieler --------------------------------------------------------------------------------
              wellPanel(fluidRow(
                          column(6, textOutput("Spieler1")),
                          column(3, checkboxInput("sp1", "Spieler")), 
                          column(3, numericInput("pkt1", "Punkte", 0))
                        )), 
              # --- Spieler --------------------------------------------------------------------------------
              wellPanel(fluidRow(
                          column(6, textOutput("Spieler2")),
                          column(3, checkboxInput("sp2", "Spieler")), 
                          column(3, numericInput("pkt2", "Punkte", 0))
                        )),
              # --- Spieler --------------------------------------------------------------------------------
              wellPanel(fluidRow(
                          column(6, textOutput("Spieler3")),
                          column(3, checkboxInput("sp3", "Spieler")),
                          column(3, numericInput("pkt3", "Punkte", 0))
              )), 
              # --- Spieler --------------------------------------------------------------------------------
              wellPanel(fluidRow(
                          column(6, textOutput("Spieler4")),
                          column(3, checkboxInput("sp4", "Spieler")), 
                          column(3, numericInput("pkt4", "Punkte", 0))
              )),
              # --- Spiel dokumentieren --------------------------------------------------------------------
              actionButton("spielAufschreiben", "Aufschreiben", width = "100%"),
              # --- Breite Sidebar -------------------------------------------------------------------------
              width = 3),
    # --- Ausgabe der Spieldaten ---------------------------------------------------------------------------
    mainPanel(actionButton("asdf", "asdf"))
  )
  
  
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
