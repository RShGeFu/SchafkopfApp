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
library(DBI)
library(RMySQL)

# --- Allgemeine Variablen ---------------------------------------------------------------------------------

spiele <- list(c("Eichel" = 1, "Gras" = 2, "Herz" = 3, "Schelln" = 4),
               0, # Dummy, da der Index 2 nicht besetzt ist - Berechnungsgründe für den Gesamtgewinn
               c("Eichel" = 1, "Gras" = 2, "Herz" = 3, "Schelln" = 4, "Wenz" = 5, "Farbwenz" = 6, "Geier" = 7, "Bettler" = 8, "Ramsch" = 9))

fileGroups <- "schafkopfrunden.csv"

# --- Aufbau der Reactivity --------------------------------------------------------------------------------
shinyServer(function(input, output, session) {
  
  # --- Sessionbezogene Variablen --------------------------------------------------------------------------
  groupsDF <- NULL
  spielrunde <- NULL
  spieler <- NULL
  spielerFarbe <- NULL
  tarif <- NULL
  spielverlauf <- NULL
  
  # --- Bei Sessionende ... --------------------------------------------------------------------------------
  session$onSessionEnded(function() {
    # TODO: ggf. nochmaliges Speichern
    print("(c) SchafkopfApp by G. Füchsl - Auf Wiedersehen, bis zum nächsten Mal ...")
    stopApp()
  })
    
  # --- Initialisierung ------------------------------------------------------------------------------------

  loadSpielverlaufbyDB <- function() {
    
  }
  
  loadSpielverlaufByFile <- function() {
    
  }
  
  loadSpielverlauf <- function(fileSpielVerlauf) {
    print("Spielverlauf laden")
    print(fileSpielVerlauf)
    return(NULL) 
  }
  
  loadGroupsByDB <- function() {
    df <- NULL

    conn <- dbConnect(
      drv = RMySQL::MySQL(),
      dbname = "test",
      host = "127.0.0.1",
      username = "",
      password = "")
    on.exit(dbDisconnect(conn), add = TRUE)
    
    if (!is.null(conn)) {
      query <- paste0("SELECT * FROM tabelle1;")
      df <- dbGetQuery(conn, query)
    }
   
    return(df)
  }
  
  loadGroupsByFile <- function (fileSKR) {
    runden <- NULL
    
    if (file.exists(fileSKR))
      runden <- read.csv(fileSKR, sep = ";", header = TRUE)
    else
      print("Gruppe erstellen")
    
    return(runden)
  }
  
  loadGroups <- function(mode = "0", fileSKR = fileGroups) {
    return(
      switch(as.character(mode),
        "0" = { loadGroupsByFile(fileSKR) },
        "1" = { loadGroupsByDB() },
        default = { NULL }
      ))
  }
  
  setActiveGroup <- function() {
    
    nr <- which(groupsDF$zuletztAktiv == 1)
    gruppe <- groupsDF[nr,]
    
    spieler <<- c(gruppe[1, 'Spieler1'], gruppe[1, 'Spieler2'], 
                  gruppe[1, 'Spieler3'], gruppe[1, 'Spieler4'])
    
    spielerFarbe <<- c(gruppe[1, 'FarbeSp1'], gruppe[1, 'FarbeSp2'], 
                       gruppe[1, 'FarbeSp3'], gruppe[1, 'FarbeSp4'])
    
    tarif <<- c(gruppe[1, 'GrundtarifSpiel'], 
                0,  # Dummy, da der Index 2 nicht besetzt ist - Berechnungsgründe für den Gesamtgewinn
                gruppe[1, 'GrundtarifSolo'])
    
    spielverlauf <<- loadSpielverlauf(gruppe[1, 'DateiSpielliste'])
    
    return(nr)
    
  }
  
  init <- function() {
    
    groupsDF <<- loadGroups()
    
    if (!is.null(groupsDF)) {
      
      groupsDF$Gruppe <<- as.character(groupsDF$Gruppe)
      groupsDF$Spieler1 <<- as.character(groupsDF$Spieler1)
      groupsDF$Spieler2 <<- as.character(groupsDF$Spieler2)
      groupsDF$Spieler3 <<- as.character(groupsDF$Spieler3)
      groupsDF$Spieler4 <<- as.character(groupsDF$Spieler4)
      groupsDF$FarbeSp1 <<- as.character(groupsDF$FarbeSp1)
      groupsDF$FarbeSp2 <<- as.character(groupsDF$FarbeSp2)
      groupsDF$FarbeSp3 <<- as.character(groupsDF$FarbeSp3)
      groupsDF$FarbeSp4 <<- as.character(groupsDF$FarbeSp4)
      groupsDF$GrundtarifSpiel <<- as.integer(groupsDF$GrundtarifSpiel)
      groupsDF$GrundtarifSolo <<- as.integer(groupsDF$GrundtarifSolo)
      groupsDF$DateiSpielliste <<- as.character(groupsDF$DateiSpielliste)

      spielrunde <<- as.list(groupsDF[,'Gruppe'])
    
      nr <- setActiveGroup()

      return(spielrunde[[nr]])
    
    } else {
      return(NULL)
    }
  }
  
  # --- Sessionbezogene Funktionen -------------------------------------------------------------------------

#'#################################################################################################################
#' Funktion: renderPlayer() - stellt die Namen der Spieler auf dem UI dar
#'
#' @return
#' @export
#'
#' @examples
#'#################################################################################################################
  
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
  
#'#################################################################################################################
#' Funktion: getSpieler() - findet den oder die Spieler, mindestens 1, maximal 2
#'
#' @return Vektor, der die Spieler enthält
#' @export keine
#'
#' @examples Spieler 1 und 3 waren Spieler -> c(1, 3)
#'#################################################################################################################

  getSpieler <- function() {
    partner <- c(input$sp1, input$sp2, input$sp3, input$sp4)  # Nimm alle Spieler und ...
    spielerPartner <- which(partner == TRUE)                  # ... suche die Spieler!
    return(spielerPartner)
  }
  
#'#################################################################################################################
#' Funktion: getNichtSpieler() - findet die Nichtspieler heraus, mindestens 2
#'
#' @return Vektor, der die Nichtspieler enthält
#' @export
#'
#' @examples Sieler 2,3 und 4 waren nicht Spieler -> c(2, 3, 4)
#'#################################################################################################################

  getNichtSpieler <- function() {
    partner <- c(input$sp1, input$sp2, input$sp3, input$sp4)  # Nimm alle Spieler und ...
    nichtSpielerPartner <- which(partner == FALSE)            # ... und suche die Nicht-Spieler
    return(nichtSpielerPartner)
  }


#'#################################################################################################################
#' Funkction: checkSpielerAnzahl() - testet, ob die ausgewälten Spieler, d.h. die Anzahl der Spieler
#'tatsächlich auch zum angegebenen Spiel passen
#'
#' @return logical - TRUE, wenn die Anzahl stimmt, FALSE, wenn die Anzahl nicht stimmt
#' @export
#'
#' @examples Solo -> 2 Spieler angeklickt -> FALSE, Sauspiel -> 2 Spieler angeklickt -> TRUE
#'#################################################################################################################

  checkSpielerZahl <- function() {
    sp <- c(input$sp1, input$sp2, input$sp3, input$sp4)       # Nimm alle Spieler...
    return(
      switch(input$spielArt1,                                 # ...und je nach nach Spielart
             "1" = {
                richtigeAnzahl <- length(which(sp)) == 2      # ... gib an, ob 2 Spieler (Sauspiel)...
             },
             "3" = {
                richtigeAnzahl <- length(which(sp)) == 1      # ... oder 1 Spieler (Solo) richtig ist
            },
            default = {
                richtigeAnzahl <- FALSE
            }
      )
    )
  }
  
#'#################################################################################################################
#' Funktion: calculatePunkte() - berechnet die Punkte von Spieler und Nichtspieler
#'
#' @return Vektor, der die Punkte enthält. 1. Stelle: Spieler, 2. Stelle: Nichtspieler
#' @export
#'
#' @examples Spieler gemeinsam 65 Punkte, Nichtspieler gemeinsam 55 Punkte -> c(65, 55), Spieleranzahl stimmt nicht
#' -> c(-1, -1)
#'#################################################################################################################

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
  
#'#################################################################################################################
#' Funktion: findWinner() - findet heraus, wer gewonnen hat, Spieler oder Nichtspieler 
#'
#' @return Vektor, der jeden Spieler den Multiplikator 1 oder -1 zurückgibt, mit der Tarif verrechnet und aufsum-
#' miert wird
#' @export
#'
#' @examples Spieler 1 und 3 haben gewonnen -< c(1, -1, 1, -1), NichtSpieler 2 und 4 haben gewonnen -> 
#' c(-1, 1, -1, 1)
#'#################################################################################################################

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
  
#'#################################################################################################################
#' Funktion: calculateProfit() - berechnet für die gespielte Runde den Gewinn
#'
#' @return Vektor, der den Gewinn jeweils für die spieler enthält - je nachdem, ob gewonnen oder verloren 
#' @export
#'
#' @examples Spieler 1 und 2 gewinnen das Sauspiel mit 3 Laufenden -> 40 Cent -> c(40, 40, -40, -40)
#'#################################################################################################################
 
  calculateProfit <- function() {
    # Berechne den Tarif für das Spiel
    gt <- tarif[as.numeric(input$spielArt1)] + tarif[1] * as.numeric(is.Schneider()) + tarif[1] * as.numeric(is.Schwarz()) + tarif[1] * as.numeric(input$anzahlLaufende)
    
    # Lege fest wer was bekommt oder bezahlen muß
    profit <-  findWinner() * gt * (2 ^ (input$anzahlGelegt + as.integer(input$soloArt)))
    
    return(profit)
  }
  
#'#################################################################################################################
#' Funktion: updateTabelleGrafik() - zeichnet die Tabelle und den graphischen Spielverlauf neu
#'
#' @return
#' @export
#'
#' @examples
#'#################################################################################################################

  updateTabelleGrafik <- function() {
    output$gewinnTabelle <- renderDataTable(spielverlauf)
    output$gewinnPlot <- renderPlot({
      y.min <- min(min(spielverlauf[,1]), min(spielverlauf[,2]), min(spielverlauf[,3]), min(spielverlauf[,4]))
      y.max <- max(max(spielverlauf[,1]), max(spielverlauf[,2]), max(spielverlauf[,3]), max(spielverlauf[,4]))
      plot(spielverlauf[,1], type = "l", lwd = 2, col = spielerFarbe[1], xlab = "Spiel", ylab = "Gewinn", main = "Spielverlauf", ylim = c(y.min, y.max))
      lines(spielverlauf[,2], col = spielerFarbe[2], lwd = 2)
      lines(spielverlauf[,3], col = spielerFarbe[3], lwd = 2)
      lines(spielverlauf[,4], col = spielerFarbe[4], lwd = 2)
      legend("topleft", spieler, col = spielerFarbe, lty = 1, lwd = 2, ncol = 4, border = NULL)
    })    
  }
  
  # --- Einstellen -----------------------------------------------------------------------------------------
  
  # Initialisierung der Variablen
  gr <- init()
  
  # ... der Spielrunde
  updateSelectInput(session, "gruppeWaehlen", choices = spielrunde, selected = gr)
  
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
    # Gewinn der Spielrunde berechnen
    p <- calculateProfit()
    
    # Hier noch Test auf Korrektheit von p, d.h. Profit darf nicht 4x 0 beinhalten
    if (length(which(p == 0)) != 4) {
    
      # Daten zusammenfassen
      df <- data.frame(p[1], p[2], p[3], p[4],
                     p[1], p[2], p[3], p[4],
                     input$spielArt1, input$spielArt2, input$soloArt,
                     input$pkt1, input$pkt2, input$pkt3, input$pkt4,
                     input$anzahlGelegt, input$anzahlLaufende,
                     as.numeric(is.Schneider()), as.numeric(is.Schwarz()), Sys.time())
      colnames(df) <- c(spieler,
                      "Gewinn Sp1", "Gewinn Sp2", "Gewinn Sp3", "Gewinn Sp4",
                      "Spielart", "Spiel", "Solotarif",
                      "Punkte Sp1", "Punkte Sp2", "Punkte Sp3", "Punkte Sp4",
                      "Gelegt", "Laufende", "Schneider", "Schwarz", "Zeit")
    
      # Neuanlegen oder Hinzufügen
      if (is.null(spielverlauf)) {
        spielverlauf <<- df
      } else {
        df[spieler] <- df[spieler] + spielverlauf[nrow(spielverlauf), spieler]
        spielverlauf <<- rbind(spielverlauf, df)
      }
      
      # --- Ausgabe ------------------------------------------------------------------------------------------
      print(df)
      updateTabelleGrafik()
      reset()
      
    } else {
      session$sendCustomMessage("saveTable", "Stimmt nicht!")
    }

  })
  
  # Letzten Eintrag zurücksetzen
  observeEvent(input$letztesKorrigieren, {
    spielverlauf <<- spielverlauf[-c(nrow(spielverlauf)),]
    
    # --- Ausgabe nach Korrektur ---------------------------------------------------------------------------
    updateTabelleGrafik()
    reset()
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
