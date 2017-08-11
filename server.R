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
  punkte <- NULL

  # --- Sessionbezogene Funktionen -------------------------------------------------------------------------
  renderPlayer <- function() {
    output$Spieler1 <- renderText({ paste(spieler[1]) })
    output$Spieler2 <- renderText({ paste(spieler[2]) })
    output$Spieler3 <- renderText({ paste(spieler[3]) })
    output$Spieler4 <- renderText({ paste(spieler[4]) }) 
  }
  
  resetPunkte <- function() {
    
  }
  
  getParter <- function() {
    
  }
  
  checkNumberOfPlayers <- function() {
    
  }
  
  checkPoints <- function() {
    
  }
  
  is.Schneider <- function() {
    
  }
  
  is.Schwarz <- function() {
    
  }
  
  sumPoints <- function(punkteAllerSpieler) {
    pkt <- which(punkteAllerSpieler == 0)                           # Bei welchem Spieler ist noch 0 Pkt eingetragen?
    if (length(pkt) < 2) {                                          # Ist nur mehr bei einem Spieler kein Ergebnis
      punkte[pkt] <- 120 - sum(punkte)                              # eingetragen, dann berechne es 
      numInp <- c("pkt1", "pkt2", "pkt3", "pkt4")                   # suche dir aus allen Spielern das richtige Feld
      updateNumericInput(session, numInp[pkt], value = punkte[pkt]) # heraus und trage das Ergebnis ein
    }
  }
  
  calculateTarif <- function() {
    
  }
  
  findTheWinner <- function() {
    
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
    updateRadioButtons(session, "soloArt", selected = 1)    # Wieder zurückstellen auf den Basistarif
  })
  
  # Punkte in die Berechnungsliste eintragen
  observeEvent(input$pkt1, {
    punkte <<- c(input$pkt1, input$pkt2, input$pkt3, input$pkt4)  
    sumPoints(punkte)
  })
  
  observeEvent(input$pkt2, {
    punkte <<- c(input$pkt1, input$pkt2, input$pkt3, input$pkt4)
    sumPoints(puntke)
  })
  
  observeEvent(input$pkt3, {
    punkte <<- c(input$pkt1, input$pkt2, input$pkt3, input$pkt4)
    sumPoints(punkte)
  })
  
  observeEvent(input$pkt4, {
    punkte <<- c(input$pkt1, input$pkt2, input$pkt3, input$pkt4)
    sumPoints(punkte)
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
