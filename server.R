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

# --- Allgemeine Variablen ---------------------------------------------------------------------------------

spiele <- list(c("Eichel", "Gras", "Herz", "Schelln"), 
               c("Eichel", "Gras", "Herz", "Schelln", "Wenz", "Farbwenz", "Geier", "Bettler", "Ramsch"))

# --- Aufbau der Reactivity --------------------------------------------------------------------------------
shinyServer(function(input, output, session) {
  
  # --- Bei Sessionende ... --------------------------------------------------------------------------------
  session$onSessionEnded(function() {
    # TODO: ggf. nochmaliges Speichern
    print("(c) SchafkopfApp by G. Füchsl - Auf Wiedersehen, bis zum nächsten Mal ...")
    stopApp()
  })
  
  # --- Sessionbezogene Variablen --------------------------------------------------------------------------
  spieler <- c("Gerhard", "Martin", "Matthias", "Tobias")
  tarif <- c(10, 20)

  # --- Sessionbezogene Funktionen -------------------------------------------------------------------------
  renderPlayer <- function() {
    output$Spieler1 <- renderText({ paste(spieler[1]) })
    output$Spieler2 <- renderText({ paste(spieler[2]) })
    output$Spieler3 <- renderText({ paste(spieler[3]) })
    output$Spieler4 <- renderText({ paste(spieler[4]) })    
  }
  
  checkNumberOfPlayers <- function() {
    
  }
  
  # --- Spieler angeben ------------------------------------------------------------------------------------
  renderPlayer()
  
  # --- Einstellen -----------------------------------------------------------------------------------------
  
  # ... der Spielart
  updateSelectInput(session, "spielArt2", choices = spiele[[1]])
  
  # ... der Tarife
  updateNumericInput(session, "tarifSpiel", value = tarif[1])
  updateNumericInput(session, "tarifSolo", value = tarif[2])
  
  # --- Events ---------------------------------------------------------------------------------------------
  
  # Spielliste einstellen
  observeEvent(input$spielArt1, {
    updateSelectInput(session, "spielArt2", choices = spiele[[as.numeric(input$spielArt1)]])
    updateRadioButtons(session, "soloArt", selected = 1)
  })
  
  
  
  
  # --- Programmtests ... ----------------------------------------------------------------------------------
  
  # t001: Zeit und Typumwandlung
  observeEvent(input$testBerechnung, {
    t0 <- system.time(
      {
        teRe <- rep(TRUE, 4)
        teRe <- c(teRe, FALSE)
        teRe <- ifelse(teRe, 1, -1)        
        teReInt <- as.integer(teRe)
      })
    t1 <- system.time(output$ergebnisBerechnung <- renderText({ paste(teRe)} ))
    t2 <- system.time(output$ergebnisBerechnung2 <- renderText({ paste(teReInt)} ))
    print(t0)
    print(t1)
    print(t2)
  })
  
  # t002:
  t_var002 <- NULL
  observeEvent(input$testSpieleRB, {
    t_var002 <<- c(t_var002, input$soloArt)
    updateRadioButtons(session, "spielArt1", selected = input$testSpieleRB %% 2 + 1)
    output$ergebnisTestRB <- renderText({ paste(t_var002) })
  })
  
})
