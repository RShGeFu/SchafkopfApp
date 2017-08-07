# ----------------------------------------------------------------------------------------------------------
# Schafkopf - App
# Rechnen leicht gemacht
# ----------------------------------------------------------------------------------------------------------

library(shiny)

shinyUI(fluidPage(
  # --- Titel ----------------------------------------------------------------------------------------------
  titlePanel("Schafkopf - mitgezählt ..."),
  hr(),
  # --- Gruppenauswahl -------------------------------------------------------------------------------------
  wellPanel(
    fluidRow(
      column(2, selectInput("gruppeWaehlen", "Gruppe", c("A", "B", "C")) ),
      column(9),
      column(1, actionButton("neueGruppe","Neue Gruppe", width = 130),
                actionButton("loescheGruppe", "Gruppe löschen", width = 130))
    )
  ),
  
  # --- Test-Panels ----------------------------------------------------------------------------------------
  # Hier können verschiedene Outputs getestet werden
  
  # t001: Ausgabe der Umwandlungen
  conditionalPanel("false",
                    actionButton("testBerechnung", "Berechne"),
                    textOutput("ergebnisBerechnung"),
                    textOutput("ergebnisBerechnung2")
  )
))
