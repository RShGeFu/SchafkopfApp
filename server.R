#'##########################################################################################################
# 
# Schafkopf - App / Mitgezählt...
#
# Rechnen leicht gemacht
#
# (c) by Gerhard Füchsl 
#
# Version 1.0
#
#'##########################################################################################################


# --- Benötigte Packages -----------------------------------------------------------------------------------
library(shiny)

# --- Allgemeine Variablen ---------------------------------------------------------------------------------

spiele <- list(c("Eichel", "Gras", "Herz", "Schelln"),
               0, # Dummy, da der Index 2 nicht besetzt ist - Berechnungsgründe für den Gesamtgewinn
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
  tarif <- c(10, 
             0,  # Dummy, da der Index 2 nicht besetzt ist - Berechnungsgründe für den Gesamtgewinn
             20)
  
  # --- Sessionbezogene Funktionen -------------------------------------------------------------------------
  renderPlayer <- function() {
    output$Spieler1 <- renderText({ paste(spieler[1]) })
    output$Spieler2 <- renderText({ paste(spieler[2]) })
    output$Spieler3 <- renderText({ paste(spieler[3]) })
    output$Spieler4 <- renderText({ paste(spieler[4]) }) 
  }

#'#################################################################################################################      
#' Funktion: reset() - setzt das Inputpanel, d.h. die Eingabemaske wieder auf den Ausgangszustand
#'
#' @return leer
#' @export keiner
#'
#' @examples
#'#################################################################################################################  
  
  reset <- function() {
    updateRadioButtons(session, "spielArt1", selected = 1)
    updateRadioButtons(session, "soloArt", selected = 0)
    updateNumericInput(session, "pkt1", value = 0)
    updateNumericInput(session, "pkt2", value = 0)
    updateNumericInput(session, "pkt3", value = 0)
    updateNumericInput(session, "pkt4", value = 0)
    updateCheckboxInput(session, "sp1", value = FALSE)
    updateCheckboxInput(session, "sp2", value = FALSE)
    updateCheckboxInput(session, "sp3", value = FALSE)
    updateCheckboxInput(session, "sp4", value = FALSE)
    updateNumericInput(session, "anzahlGelegt", value = 0)
    updateNumericInput(session, "anzahlLaufende", value = 0)
  }

#'#################################################################################################################  
#' Funktion: sumPoints() - summiert die Punkte dreier Spieler und errechnet die Punkte des vierten Spielers
#'
#' @return keine, anstatt dessen Update des numericInput mit der fehlenden Punktzahl
#' @export keiner
#'
#' @examples
#'#################################################################################################################
  
  sumPoints <- function() {
    punkte <- c(input$pkt1, input$pkt2, input$pkt3, input$pkt4)     # Aktualisiere die Punkteliste
    pkt <- which(punkte == 0)                                       # Bei welchem Spieler ist noch 0 Pkt eingetragen?
    if (length(pkt) < 2) {                                          # Ist nur mehr bei einem Spieler kein Ergebnis
      punkte[pkt] <- 120 - sum(punkte)                              # eingetragen, dann berechne es 
      numInp <- c("pkt1", "pkt2", "pkt3", "pkt4")                   # suche dir aus allen Spielern das richtige Feld
      updateNumericInput(session, numInp[pkt], value = punkte[pkt]) # heraus und trage das Ergebnis ein
    }
  }

#'#################################################################################################################    
#' Funktion: checkPoints() - testet, ob die Punktesumme 120 ergibt
#'
#' @return logical - TRUE, wenn die Gesamtpunktzahl 120 ergibt, sonst FALSE
#' @export keiner
#'
#' @examples die vier Spieler zählen geben 20, 20, 60, 21 Punkte ein -> FALSE
#'#################################################################################################################  
  
  checkPoints <- function() {
    return(sum(c(input$pkt1, input$pkt2, input$pkt3, input$pkt4)) == 120)
  }
  
  getSpieler <- function() {
    partner <- c(input$sp1, input$sp2, input$sp3, input$sp4)  # Nimm alle Spieler und ...
    spielerPartner <- which(partner == TRUE)                  # ... suche die Spieler!
    return(spielerPartner)
  }
  
  getNichtSpieler <- function() {
    partner <- c(input$sp1, input$sp2, input$sp3, input$sp4)  # Nimm alle Spieler und ...
    nichtSpielerPartner <- which(partner == FALSE)            # ... und suche die Nicht-Spieler
    return(nichtSpielerPartner)
  }

  checkSpielerZahl <- function() {
    sp <- c(input$sp1, input$sp2, input$sp3, input$sp4)       # Nimm alle Spieler...
    return(
      switch(input$spielArt1,                                 # ...und je nach nach Spielart
             "1" = {
                richtigeAnzahl <- length(which(sp)) == 2      # ... gib an, ob 2 Spieler...
             },
             "3" = {
                richtigeAnzahl <- length(which(sp)) == 1      # ... oder 1 Spieler richtig ist
            },
            default = {
                richtigeAnzahl <- FALSE
            }
      )
    )
  }
  
  calculatePunkte <- function() {
    ergebnis <- c(-1, -1)
    if (checkSpielerZahl()) {                                 # Wenn die Spielerzahl stimmt ...
      p <- c(input$pkt1, input$pkt2, input$pkt3, input$pkt4)  # ... nimm alle Spieler...
      pktSpieler <- sum(p[getSpieler()])                      # ... summiere die Punkte der Spieler...
      pktNichtSpieler <- sum(p[getNichtSpieler()])            # ... und summiere die Punkte der Nicht-Spieler ...
      ergebnis <- c(pktSpieler, pktNichtSpieler)              # ... und bastle eine Ergebnisvektor
    }
    return(ergebnis)
  }
  
#'#################################################################################################################
#' Funktion: is.Schneider() - testet, ob eine Spielerpartei weniger als 30 Punkte hat
#'
#' @param punkte, voreingestellt calculatePunkte(), hier werden für die Spielerparteien bereits die Punkte
#' berechnet. 
#'
#' @return logical - wenn Spieler oder Nichtspieler weniger als 30 Pkt haben, zählt das als Schnieder: TRUE/FALSE
#' @export keiner
#'
#' @examples c(110, 10) -> TRUE, c(80, 40) -> FALSE, c(40, 80) -> FALSE
#'#################################################################################################################
  
  is.Schneider <- function(punkte = calculatePunkte()) {
    return(length(punkte[punkte < 30]) > 0)
  }
  
#'#################################################################################################################
#' Funktion: is.Schwarz() - testet, ob eine Spielerpartei 0 Punkte hat
#'
#' @param punkte, voreingestellt calculatePunkte(), hier werden für die Spielerparteien bereits die Punkte
#' berechnet.
#'
#' @return logical - wenn Spieler oder Nichtspieler 0 Pkt haben, zählt das als Schwarz: TRUE/FALSE
#' @export keiner
#'
#' @examples punkte = c(0, 120) -> TRUE, punkte = c(120, 0) -> TRUE, punkte(110, 10) -> FALSE
#'#################################################################################################################
  
  is.Schwarz <- function(punkte = calculatePunkte()) {
    return(length(punkte[punkte < 1]) > 0)
  }
  
  findWinner <- function() {
    # update der Spieler/Nichtspieler
    hatGespielt <- ifelse(c(input$sp1, input$sp2, input$sp3, input$sp4), as.numeric(input$spielArt1), -1)
    
    # rechne die Punkte aus
    pkt <- calculatePunkte()
    
    # Wenn alle Regeln eingehalten wurden und die Summe der Punkte 120 ergibt
    if ((length(which(pkt==-1)) == 0) & checkPoints()) 
    
      # dann lege die Gewinner und Verlierer mit dem jeweiligen Tariffaktor fest
      hatGespielt <- hatGespielt * ifelse(pkt[1] < 61, -1, 1)
    else
    
      # ansonsten 0, dann wird nichts berechnet
      hatGespielt <- c(0,0,0,0)
    
    return(hatGespielt)
  }
  
  calculateProfit <- function() {
    # Berechne den Tarif für das Spiel
    gt <- tarif[as.numeric(input$spielArt1)] + tarif[1] * as.numeric(is.Schneider()) + tarif[1] * as.numeric(is.Schwarz()) + tarif[1] * as.numeric(input$anzahlLaufende)
    
    # Lege fest wer was bekommt oder bezahlen muß
    profit <-  findWinner() * gt * (2 ^ (input$anzahlGelegt + as.integer(input$soloArt)))
    
    return(profit)
  }
  
  # --- Einstellen -----------------------------------------------------------------------------------------
  # ... der Spieler
  renderPlayer()
  
  # ... der Spielart
  updateSelectInput(session, "spielArt2", choices = spiele[[1]])
  
  # ... der Tarife
  updateNumericInput(session, "tarifSpiel", value = tarif[1])
  updateNumericInput(session, "tarifSolo", value = tarif[3])
  
  # --- Events ---------------------------------------------------------------------------------------------
  
  # Spielliste einstellen
  observeEvent(input$spielArt1, {
    updateSelectInput(session, "spielArt2", choices = spiele[[as.numeric(input$spielArt1)]])
    updateRadioButtons(session, "soloArt", selected = 0)    # Wieder zurückstellen auf den Basistarif
  })
  
  # Punkte in die Berechnungsliste eintragen
  observeEvent(input$pkt1, {
    sumPoints()
  })
  
  observeEvent(input$pkt2, {
    sumPoints()
  })
  
  observeEvent(input$pkt3, {
    sumPoints()
  })
  
  observeEvent(input$pkt4, {
    sumPoints()
  })
  
  # Daten speichern und Eingabe zurücksetzen
  observeEvent(input$spielAufschreiben, {
    reset()
  })
  
  # --- Ausgabe --------------------------------------------------------------------------------------------
  
  output$gewinnPlot <- renderPlot({ 
    plot(calculateProfit())
  })  
  
  # --- Modultests ... -------------------------------------------------------------------------------------
  
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
  
  # t003:
  observeEvent(input$testSumPoints, {
    p <- input$testSumPoints
    punkteInput <- c("pkt1", "pkt2", "pkt3", "pkt4")
    
    ausgewaehlt <- sample(punkteInput, 1)
    nichtAusgewaehlt <- punkteInput[punkteInput!=ausgewaehlt]
    
    updateNumericInput(session, ausgewaehlt, value = 0)
    updateNumericInput(session, nichtAusgewaehlt[1], value = p)
    updateNumericInput(session, nichtAusgewaehlt[2], value = p*2)
    updateNumericInput(session, nichtAusgewaehlt[3], value = p*4)
    
    output$ergebnisAusgewaehlt <- renderText({ paste(ausgewaehlt) })
  })
  
  # t004:
  observeEvent(input$testCheckAnzahlSpieler, {
    if (input$testCheckAnzahlSpieler)
        output$ergebnisAngehakteSpieler <- renderText({ paste(checkSpielerZahl()) })
    else
      output$ergebnisAngehakteSpieler <- renderText({ paste("") })
  })
  
  # t005:
  observeEvent(input$testCalculatePunkte, {
    if (input$testCalculatePunkte) {
      erg <- calculatePunkte()
      output$ergebnisSpieler <- renderText({ paste(erg[1]) })
      output$ergebnisNichtSpieler <- renderText({ paste(erg[2]) })
      output$ergebnisSchneider <- renderText({ paste(is.Schneider()) })
      output$ergebnisSchwarz <- renderText({ paste(is.Schwarz()) })
      output$ergebnisCheckPoints <- renderText({ paste(checkPoints()) })
    }
  })
  
  # t006:
  observeEvent(input$testFindWinner, {
    output$ergebnisFindWinner <- renderText({ paste(findWinner()) })
  })
  
  # t007:
  observeEvent(input$testCalculateProfit, {
    output$ergebnisCalculateProfit <- renderText({ paste("Gewinn: ", calculateProfit())})
  })
})
