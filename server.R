# ----------------------------------------------------------------------------------------------------------
# 
# Schafkopf - App / Mitgezählt...
#
# Rechnen leicht gemacht
#
# ----------------------------------------------------------------------------------------------------------

# --- Benötigte Packages -----------------------------------------------------------------------------------
library(shiny)

# --- Aufbau der Reactivity --------------------------------------------------------------------------------
shinyServer(function(input, output, session) {
  
  # --- Bei Sessionende ... --------------------------------------------------------------------------------
  session$onSessionEnded(function() {
    # TODO: ggf. nochmaliges Speichern
    print("(c) SchafkopfApp by G. Füchsl - Auf Wiedersehen, bis zum nächsten Mal ...")
    stopApp()
  })
  
  output$Spieler1 <- renderText({ paste("Gerhard") })
  output$Spieler2 <- renderText({ paste("Martin") })
  output$Spieler3 <- renderText({ paste("Matthias") })
  output$Spieler4 <- renderText({ paste("Tobias") })
  
  
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
  
})
