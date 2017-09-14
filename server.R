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
library(digest)

# --- Allgemeine Variablen ---------------------------------------------------------------------------------

spiele <- list(c("Eichel" = 1, "Gras" = 2, "Herz" = 3, "Schelln" = 4),
               0, # Dummy, da der Index 2 nicht besetzt ist - Berechnungsgründe für den Gesamtgewinn
               c("Eichel" = 1, "Gras" = 2, "Herz" = 3, "Schelln" = 4, "Wenz" = 5, "Farbwenz" = 6, 
                 "Geier" = 7, "Bettler" = 8, "Ramsch" = 9))

fileGroups <- "schafkopfrunden.skr"    # Name für die Steuerungsdatei der Spielrunden
localMode <- "0"                       # Speichermodus: 0 - die Daten werden in Dateien abgelegt
dbMode <- "1"                          #                1 - Die Daten werden in einer Datenbank abgelegt
configMode <- localMode                # Konfiguriert für lokale Speicherung

# --- Aufbau der Reactivity --------------------------------------------------------------------------------
shinyServer(function(input, output, session) {
  
  # --- Sessionbezogene Variablen --------------------------------------------------------------------------

  groupsDF <- NULL         # ... beinhaltet die Spielgruppen und die jeweiligen Eigenschaften
  spielrunde <- NULL       # ... beinhaltet die Namen der Spielgruppen als Listen
  spieler <- NULL          # ... beinhaltet die Namen der Spieler
  spielerFarbe <- NULL     # ... beinhaltet die Farben für Darstellung der Spieler in der graphischen
                           #     Darstellung
  tarif <- NULL            # ... beinhaltet den für die Gruppe gültigen Tarif
  spielverlauf <- NULL     # ... beinhaltet den gesamten Spielverlauf
  
  # --- Modaldialoge definieren ----------------------------------------------------------------------------
  
  gruppeErstellen <- modalDialog(h5("Gruppenname"),
                                 textInput("grName", NULL, placeholder = "z.B. Die lustigen Vier"),
                                 h5("Tarife"),
                                 textInput("tarifSpielErst", NULL, placeholder = "Sauspiel"),
                                 textInput("tarifSoloErst", NULL, placeholder = "Solo"),
                                 h5("Spielernamen"),
                                 textInput("sp1Name", NULL, placeholder = "Spieler 1"),
                                 textInput("sp2Name", NULL, placeholder = "Spieler 2"),
                                 textInput("sp3Name", NULL, placeholder = "Spieler 3"),
                                 textInput("sp4Name", NULL, placeholder = "Spieler 4"),
                                 h5("Ausgangskapital (in Cent)"),
                                 textInput("sp1Kapital", NULL, placeholder = "Spieler 1"),
                                 textInput("sp2Kapital", NULL, placeholder = "Spieler 2"),
                                 textInput("sp3Kapital", NULL, placeholder = "Spieler 3"),
                                 textInput("sp4Kapital", NULL, placeholder = "Spieler 4"),
                                 title = "Wollen Sie eine Spielrunde aufmachen ...",
                                 footer = tagList(actionButton("modalGruppeErstellenOK", "OK"), 
                                                  modalButton("Nein")))
  
  gruppeWirklichLoeschen <- modalDialog(h4("Wollen Sie die Spielgruppe wirklich löschen?"),
                                        title = "Bitte Vorsicht!",
                                        footer = tagList(actionButton("modalWLoesch", "Ja!"),
                                                         modalButton("Nein!")))
  
  summeGroesserAls120 <- modalDialog(h4("Bereits die Summe von 3 Spielern ergibt mehr als 120!"),
                                     title = "Bitte Vorsicht!",
                                     footer = modalButton("Weiter"))
  
  # --- Manipulationen der Dataframes und Vektoren ---------------------------------------------------------
  
  #'########################################################################################################
  #' Funktion: setZuletztAktiv() - setzt die aktive Gruppe im Dataframe groupsDF
  #'
  #' @param aktiveGruppe - Nummer der Zeile der aktive Gruppe
  #'
  #' @return keine
  #' @export keine
  #'
  #' @examples setZuletztAktiv(2) -> setzt im Dataframe groupsDF in der Zeile 2 zuletzt aktiv auf 1
  #'########################################################################################################
  
  setZuletztAktiv <- function(aktiveGruppe = 1) {
    if (!is.null(groupsDF)) {
      groupsDF$zuletztAktiv <<- 0
      groupsDF[aktiveGruppe, 'zuletztAktiv'] <<- 1
    }
  }
  
  #'########################################################################################################
  #' Funktion: getZuletztAktiv() - liefert die Position der aktiven Gruppe im Dataframe zurück
  #'
  #' @return integer - Zeile der zuletzt als aktiv gesetzten Gruppe im Dataframe
  #' @export keine
  #'
  #' @examples zuletzt aktive Gruppe (groupsDF$zuletztAktiv) steht in Zeile 2 auf 1 -> Rückgabe-Wert: 2
  #'########################################################################################################
  
  getZuletztAktiv <- function() {
    return(which(groupsDF$zuletztAktiv == 1))
  }
  
  #'########################################################################################################
  #' Funktion: setActiveGroup() - setzt die Session-variablen auf die aktive Gruppe
  #'
  #' @return keine
  #' @export keine
  #'
  #' @examples
  #'########################################################################################################
  
  setActiveGroup <- function() {
    
    nr <- getZuletztAktiv()
    
    gruppe <- groupsDF[nr,]
    
    spieler <<- c(gruppe[1, 'Spieler1'], gruppe[1, 'Spieler2'], 
                  gruppe[1, 'Spieler3'], gruppe[1, 'Spieler4'])
    
    spielerFarbe <<- c(gruppe[1, 'FarbeSp1'], gruppe[1, 'FarbeSp2'], 
                       gruppe[1, 'FarbeSp3'], gruppe[1, 'FarbeSp4'])
    
    tarif <<- c(gruppe[1, 'GrundtarifSpiel'], 
                0,  # Dummy, da der Index 2 nicht besetzt ist - Berechnungsgründe für den Gesamtgewinn
                gruppe[1, 'GrundtarifSolo'])
    
    spielverlauf <<- loadSpielverlauf(fileSpielverlauf = gruppe[1, 'DateiSpielliste'])
    setTypesOfSpielverlauf()

  }
  
  #'########################################################################################################
  #' Funktion: setTypesOfGroupsDF() - ordnet den Spalten im Dataframe groupsDF bestimmte Typen zu
  #'
  #' @return keine
  #' @export keine
  #'
  #' @examples
  #'########################################################################################################
  
  setTypesOfGroupsDF <- function() {
    groupsDF$Gruppe <<- as.character(groupsDF$Gruppe)
    groupsDF$Spieler1 <<- as.character(groupsDF$Spieler1)
    groupsDF$Spieler2 <<- as.character(groupsDF$Spieler2)
    groupsDF$Spieler3 <<- as.character(groupsDF$Spieler3)
    groupsDF$Spieler4 <<- as.character(groupsDF$Spieler4)
    groupsDF$FarbeSp1 <<- as.character(groupsDF$FarbeSp1)
    groupsDF$FarbeSp2 <<- as.character(groupsDF$FarbeSp2)
    groupsDF$FarbeSp3 <<- as.character(groupsDF$FarbeSp3)
    groupsDF$FarbeSp4 <<- as.character(groupsDF$FarbeSp4)
    groupsDF$Startkapital1 <<- as.integer(as.character(groupsDF$Startkapital1))
    groupsDF$Startkapital2 <<- as.integer(as.character(groupsDF$Startkapital2))
    groupsDF$Startkapital3 <<- as.integer(as.character(groupsDF$Startkapital3))
    groupsDF$Startkapital4 <<- as.integer(as.character(groupsDF$Startkapital4))
    #groupsDF$Beginn <<- as.POSIXct(as.character(groupsDF$Beginn))
    groupsDF$GrundtarifSpiel <<- as.integer(as.character(groupsDF$GrundtarifSpiel))
    groupsDF$GrundtarifSolo <<- as.integer(as.character(groupsDF$GrundtarifSolo))
    groupsDF$DateiSpielliste <<- as.character(groupsDF$DateiSpielliste)    
  }
  
  #'########################################################################################################
  #' Funktion: setTypesOfSpielverlauf() - ordnet den Spalten im Dataframe spielverlauf bestimmte Typen zu
  #'
  #' @return keine
  #' @export keine
  #'
  #' @examples 
  #'########################################################################################################
  
  setTypesOfSpielverlauf <- function() {
    spielverlauf[,1] <<- as.integer(as.character(spielverlauf[,1]))
    spielverlauf[,2] <<- as.integer(as.character(spielverlauf[,2]))
    spielverlauf[,3] <<- as.integer(as.character(spielverlauf[,3]))
    spielverlauf[,4] <<- as.integer(as.character(spielverlauf[,4]))
    spielverlauf$GewinnSp1 <<- as.integer(as.character(spielverlauf$GewinnSp1))
    spielverlauf$GewinnSp2 <<- as.integer(as.character(spielverlauf$GewinnSp2))
    spielverlauf$GewinnSp3 <<- as.integer(as.character(spielverlauf$GewinnSp3))
    spielverlauf$GewinnSp4 <<- as.integer(as.character(spielverlauf$GewinnSp4))
    spielverlauf$Spielart <<- as.integer(as.character(spielverlauf$Spielart))
    spielverlauf$Spiel <<- as.integer(as.character(spielverlauf$Spiel))
    spielverlauf$Solotarif <<- as.integer(as.character(spielverlauf$Solotarif))
    spielverlauf$PunkteSp1 <<- as.integer(as.character(spielverlauf$PunkteSp1))
    spielverlauf$PunkteSp2 <<- as.integer(as.character(spielverlauf$PunkteSp2))
    spielverlauf$PunkteSp3 <<- as.integer(as.character(spielverlauf$PunkteSp3))
    spielverlauf$PunkteSp4 <<- as.integer(as.character(spielverlauf$PunkteSp4))
    spielverlauf$Gelegt <<- as.integer(as.character(spielverlauf$Gelegt))
    spielverlauf$Laufende <<- as.integer(as.character(spielverlauf$Laufende))
    spielverlauf$Schneider <<- as.integer(as.character(spielverlauf$Schneider))
    spielverlauf$Schwarz <<- as.integer(as.character(spielverlauf$Schwarz))
    spielverlauf$Zeit <<- as.POSIXct(as.character(spielverlauf$Zeit))
  }
  
  #'########################################################################################################
  #' Funktion: setColsGroupsDF() - definiert für das Dataframe groupsDF die Spaltennamen
  #'
  #' @param df - Dataframe, groupsDF, könnte auch global aufgerufen werden
  #'
  #' @return adaptierter Dataframe df
  #' @export keine
  #'
  #' @examples
  #'########################################################################################################
  
  setColsGroupsDF <- function(df) {
    colnames(df) <- c("Gruppe","Spieler1","Spieler2","Spieler3","Spieler4",
                     "Startkapital1","Startkapital2","Startkapital3","Startkapital4",
                     "Beginn","GrundtarifSpiel","GrundtarifSolo","DateiSpielliste",
                     "FarbeSp1","FarbeSp2","FarbeSp3","FarbeSp4","zuletztAktiv")
    return(df)
  }

  #'########################################################################################################
  #' Funktion: setColsSpielverlauf() - definiert für das Dataframe spielverlauf die Spaltennamen
  #'
  #' @param df - Dataframe, spielverlauf, könnte auch global aufgerufen werden
  #'
  #' @return adaptierter Dataframe df
  #' @export keine
  #'
  #' @examples
  #'########################################################################################################
  
  setColsSpielverlauf <- function(df) {
    colnames(df) <- c(spieler,
                      "GewinnSp1", "GewinnSp2", "GewinnSp3", "GewinnSp4",
                      "Spielart", "Spiel", "Solotarif",
                      "PunkteSp1", "PunkteSp2", "PunkteSp3", "PunkteSp4",
                      "Gelegt", "Laufende", "Schneider", "Schwarz", "Zeit")
    return(df)
  }
  
  # --- Initialisierungsfunktionen -------------------------------------------------------------------------

  loadSpielverlaufbyDB <- function() {
    
  }

  #'##########################################################################################################
  #' Funktion: loadSpielverlaufByFile() - lädt den Spielverlauf aus einer CSV-Datei, wenn diese vorhanden ist
  #'
  #' @param fileSpielVerlauf - character, Filename der CSV-Datei
  #'
  #' @return Dataframe mit den Daten des Spielverlaufs
  #' @export keine
  #'
  #' @examples
  #'##########################################################################################################

  loadSpielverlaufByFile <- function(fileSpielVerlauf) {
    spVerlauf <- NULL
    
    if (file.exists(fileSpielVerlauf)) {
      encFile <- gsub(".csv", ".skr", fileSpielVerlauf)
      if (file.exists((encFile))) {
        fl <- file.info(encFile)
        f <- readBin(encFile, "raw", fl$size)
        r <- decryptSpielverlauf(f)
        spVerlauf <- r #read.csv(fileSpielVerlauf, sep = ";", header = TRUE)
      }
    }
    return(spVerlauf)    
  }

  #'##########################################################################################################
  #' Funktion - loadSpielverlauf() - lädt für eine Gruppe den gesamten Spielverlauf
  #'
  #' @param mode - integer, je nach Wert wird entweder auf ein File oder die Datenbank zugegriffen
  #' @param fileSpielverlauf - character, Filename für die CSV-Datei des Spielverlaufs der jeweiligen Gruppe
  #'
  #' @return Dataframe mit den Daten des Spielverlaufs
  #' @export keine
  #'
  #' @examples
  #'##########################################################################################################

  loadSpielverlauf <- function(mode = configMode, fileSpielverlauf) {
    return(
      switch(as.character(mode),
             "0" = { loadSpielverlaufByFile(fileSpielverlauf) },
             "1" = { loadSpielverlaufByDB() },
             default = { NULL }
      ))
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

  #'##########################################################################################################  
  #' Funktion - loadGroupsByFile() - lädt die Spielgruppen aus einer CSV-Datei, wenn keine Datei exisitiert,
  #'                                 wird der User gebeten eine Gruppe zu erstellen
  #'
  #' @param fileSKR - character, Dateiname der CSV-Datei
  #'
  #' @return Dataframe mit den Eigenschaften der Spielgruppen
  #' @export keine
  #'
  #' @examples 
  #'##########################################################################################################

  loadGroupsByFile <- function (fileSKR) {
    runden <- NULL
    if (file.exists(fileSKR)) {
      encFile <- gsub(".csv", ".skr", fileSKR)
      if (file.exists(encFile)) {
        fl <- file.info(encFile)
        f <- readBin(encFile, "raw", fl$size)
        r <- decryptGroupsDF(f)
      }
      runden <- r #read.csv(fileSKR, sep = ";", header = TRUE)
    } else {
      showModal(gruppeErstellen)
      runden <- groupsDF
    }
    return(runden)
  }
  
  #'##########################################################################################################
  #' Funktion: loadGroups() - lädt die unterschiedlichen Spielgruppen entweder aus einer Datei oder
  #'                          einer Datenbank
  #' @param mode numeric - steuert den Modus, mit die Daten geladen werden (0: File, 1: Datenbank)
  #' @param fileSKR character - Dateiname der Datei, die die Spielgruppen enthält
  #'
  #' @return Dataframe mit den Spielgruppen
  #' @export keine
  #'
  #' @examples loadGroups(0, "schafkopfrunden.csv") -> ruft die Funktion für das Laden der Daten über ein 
  #'           File auf
  #'##########################################################################################################

  loadGroups <- function(mode = configMode, fileSKR = fileGroups) {
    return(
      switch(as.character(mode),
        "0" = { loadGroupsByFile(fileSKR) },
        "1" = { loadGroupsByDB() },
        default = { NULL }
      ))
  }

  #'##########################################################################################################
  #' Funktion: init() - Lädt die Spielgruppen, initialisiert Variablen und zeigt die Spielgruppe an
  #'
  #' @return keine
  #' @export keine
  #'
  #' @examples
  #'##########################################################################################################

  init <- function() {
    groupsDF <<- loadGroups()
    setAndDisplay()
  }

  # --- Verschlüsselung ------------------------------------------------------------------------------------
  
  encryptData <- function(toEncDF) {
    
    # Variablendefinition
    toEnc <- "µ"
    encrypted <- NULL
    
    # String aus Dataframe zusammenstellen
    for(i in seq_along(1:length(toEncDF))) {
      toEnc <- c(toEnc, as.character(toEncDF[[i]]), "µ")
    }

    # Verschlüsselung definieren
    key <- as.raw(c(0x4, 0x2, 0x5, 0x6, 0xA, 0xC, 0x1, 0x7, 0xE, 0x9, 0xF, 0x2, 0x1, 0x7, 0x3, 0xB))
    aes <- AES(key, mode = "ECB")
    
    # String zur Verschlüsselung zusammenstellen
    for(i in toEnc) {
      i <- paste0("~", i)
      to16 <- 16 - nchar(i, type = "bytes") %% 16 # auf 16 auffüllen
      for(j in 1:to16)
        i <- paste0(i, '°')                       # Füllzeichen verwenden
      encrypted <- c(encrypted, aes$encrypt(i))
    }
    
    return(encrypted)

  }
  
  decryptData <- function(toDec) {
    d <- NULL
    key <- as.raw(c(0x4, 0x2, 0x5, 0x6, 0xA, 0xC, 0x1, 0x7, 0xE, 0x9, 0xF, 0x2, 0x1, 0x7, 0x3, 0xB))
    aes <- AES(key, mode = "ECB")
    retDF <- NULL
    
    dec <- strsplit(aes$decrypt(toDec), "[~]")
    d <- gsub("°", "", c(d, dec[[1]][which(dec[[1]] != "")]))
    decL <- list()
    l <- which(d == 'µ')
    for(i in seq_along(1:(length(l)-1))) {
      decL[[i]] <- c(d[(l[i]+1):(l[i+1]-1)])
      retDF <- cbind(retDF, decL[[i]])
    }
  
    return(as.data.frame(retDF))
  }
  
  decryptGroupsDF <- function(toDec) {
    df <- setColsGroupsDF(decryptData(toDec))
    return(df)
  }
  
  decryptSpielverlauf <- function(toDec) {
    df <- setColsSpielverlauf(decryptData(toDec))
    return(df)
  }
  
  # --- Dauerhafte Speicherung -----------------------------------------------------------------------------
  
  saveActiveGroupByDB <- function() {
    return(FALSE)
  }

  #'##########################################################################################################
  #' Funktion: writeFiles() - schreibt die Daten der Dataframes verschlüsselt oder unverschlüsselt in
  #'                          die angegebene Datei
  #'
  #' @param fileSKR - character, Dateiname, in die groupsDF geschrieben werden soll
  #' @param encrypted - TRUE, Daten werden verschlüsst, FALSE, Daten werden nicht verschlüsselt
  #'
  #' @return kein
  #' @export kein
  #'
  #' @examples
  #'##########################################################################################################
  
  writeFiles <- function(fileSKR, encrypted = TRUE) {
    nr <- getZuletztAktiv()
    if (encrypted) {
      encFile <- gsub(".csv", ".skr", fileSKR)
      e <- encryptData(groupsDF)
      writeBin(e, fileSKR)
      encFile <- gsub(".csv", ".skr", groupsDF[nr, 'DateiSpielliste'])
      e <- encryptData(spielverlauf)
      writeBin(e, groupsDF[nr, 'DateiSpielliste'])
    } else {
      encFile <- gsub(".skr", ".csv", fileSKR)
      write.csv2(groupsDF, encFile, row.names = FALSE)
      encFile <- gsub(".skr", ".csv", groupsDF[nr, 'DateiSpielliste'])
      write.csv2(spielverlauf, encFile, row.names = FALSE)       
    }
  }
  
  #'##########################################################################################################
  #' Funktion: saveActiveGroupByFile() - speichert die Daten der aktuellen Gruppe in eine CSV-Datei
  #'
  #' @param fileSKR - character, Filename der CSV-Datei
  #' @param testExist - logical, TRUE: testet ob die Datei(en) existiert und speichert, 
  #'                             FALSE: speichert direkt
  #'
  #' @return logical - TRUE, wenn alles geklappt hat, FALSE, wenn ein Fehler aufgetreten ist
  #' @export keine
  #'
  #' @examples
  #'##########################################################################################################

  saveActiveGroupByFile <- function(fileSKR, testExist) {
    success <- FALSE
    
    nr <- getZuletztAktiv()
    
    if (testExist) {
      if (!is.null(groupsDF[nr, 'DateiSpielliste'])) {
        if (file.exists(groupsDF[nr, 'DateiSpielliste'])) {
          writeFiles(fileSKR)
          success <- TRUE
        }
      }
    } else {
      writeFiles(fileSKR)
      success <- TRUE      
    }
    return(success)
  }

  #'##########################################################################################################
  #' Funktion: saveActiveGroup() - speichert die Daten der aktuellen Gruppenliste und den Spielverlauf der
  #'                               aktiven Gruppe ab (File oder Datenbank)
  #'
  #' @param mode - integer, speichert entweder in eine CSV-Datei oder in eine Datenbank
  #' @param fileSKR - character, Filename der CSV-Datei
  #' @param testExist - logical, speichert entweder direkt oder nach Test auf Existenz der Files
  #'
  #' @return logical - TRUE, wenn das Speichern ausgeführt wurde, FALSE, wenn ein Problem aufgetreten ist
  #' @export keine
  #'
  #' @examples
  #'##########################################################################################################

  saveActiveGroup <- function(mode = configMode, fileSKR = fileGroups, testExist = FALSE) {
    return(
      switch(as.character(mode),
           "0" = { saveActiveGroupByFile (fileSKR, testExist) },
           "1" = { saveActiveGroupByDB() },
           default = { FALSE }
      )
    )
  }
  
  # --- Funktionen für die Berechnungslogik ------------------------------------------------------------------

  #'##########################################################################################################
  #' Funktion: sumPoints() - summiert die Punkte dreier Spieler und errechnet die Punkte des vierten Spielers
  #'
  #' @return keine, anstatt dessen Update des numericInput mit der fehlenden Punktzahl
  #' @export keiner
  #'
  #' @examples
  #'##########################################################################################################
  
  sumPoints <- function() {
    numInp <- c("pkt1", "pkt2", "pkt3", "pkt4")                     # Merke Dir die Felder des Dataframes und 
    punkte <- c(input$pkt1, input$pkt2, input$pkt3, input$pkt4)     # aktualisiere die Punkteliste
    pkt <- which(punkte == 0)                                       # Bei welchem Spieler ist noch 0 Pkt 
                                                                    # eingetragen?
    if (length(pkt) < 2) {                                          # Ist nur mehr bei einem Spieler kein 
                                                                    # Ergebnis
      deltaPkt <- 120 - sum(punkte)                                 # eingetragen, dann berechne es
      if (!is.na(deltaPkt)) {
        if (deltaPkt > -1) {                                           # wenn ein positver Wert herauskommt
          punkte[pkt] <-  deltaPkt                                     # trage diesen Wert ein                              
          updateNumericInput(session, numInp[pkt], value = punkte[pkt])# und zeige ihn an
        } else {                                                       # Ansonsten 
          showModal(summeGroesserAls120)                               # weise den User darauf hin
          updateNumericInput(session, numInp[pkt], value = "")         # und setze des Wert zurück
        }
      }
    }
  }

  #'##########################################################################################################
  #' Funktion: checkPoints() - testet, ob die Punktesumme 120 ergibt
  #'
  #' @return logical - TRUE, wenn die Gesamtpunktzahl 120 ergibt, sonst FALSE
  #' @export keiner
  #'
  #' @examples die vier Spieler zählen geben 20, 20, 60, 21 Punkte ein -> FALSE
  #'##########################################################################################################
  
  checkPoints <- function() {
    return(sum(c(input$pkt1, input$pkt2, input$pkt3, input$pkt4)) == 120)
  }
  
  #'##########################################################################################################
  #' Funktion: getSpieler() - findet den oder die Spieler, mindestens 1, maximal 2
  #'
  #' @return Vektor, der die Spieler enthält
  #' @export keine
  #'
  #' @examples Spieler 1 und 3 waren Spieler -> c(1, 3)
  #'##########################################################################################################

  getSpieler <- function() {
    partner <- c(input$sp1, input$sp2, input$sp3, input$sp4)  # Nimm alle Spieler und ...
    spielerPartner <- which(partner == TRUE)                  # ... suche die Spieler!
    return(spielerPartner)
  }
  
  #'##########################################################################################################
  #' Funktion: getNichtSpieler() - findet die Nichtspieler heraus, mindestens 2
  #'
  #' @return Vektor, der die Nichtspieler enthält
  #' @export
  #'
  #' @examples Sieler 2,3 und 4 waren nicht Spieler -> c(2, 3, 4)
  #'##########################################################################################################

  getNichtSpieler <- function() {
    partner <- c(input$sp1, input$sp2, input$sp3, input$sp4)  # Nimm alle Spieler und ...
    nichtSpielerPartner <- which(partner == FALSE)            # ... und suche die Nicht-Spieler
    return(nichtSpielerPartner)
  }


  #'##########################################################################################################
  #' Funkction: checkSpielerAnzahl() - testet, ob die ausgewälten Spieler, d.h. die Anzahl der Spieler
  #'tatsächlich auch zum angegebenen Spiel passen
  #'
  #' @return logical - TRUE, wenn die Anzahl stimmt, FALSE, wenn die Anzahl nicht stimmt
  #' @export
  #'
  #' @examples Solo -> 2 Spieler angeklickt -> FALSE, Sauspiel -> 2 Spieler angeklickt -> TRUE
  #'##########################################################################################################

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
  
  #'##########################################################################################################
  #' Funktion: calculatePunkte() - berechnet die Punkte von Spieler und Nichtspieler
  #'
  #' @return Vektor, der die Punkte enthält. 1. Stelle: Spieler, 2. Stelle: Nichtspieler
  #' @export
  #'
  #' @examples Spieler gemeinsam 65 Punkte, Nichtspieler gemeinsam 55 Punkte -> c(65, 55), Spieleranzahl 
  #'           stimmt nicht -> c(-1, -1)
  #'##########################################################################################################

  calculatePunkte <- function() {
    ergebnis <- c(-1, -1)
    if (checkSpielerZahl()) {                                 # Wenn die Spielerzahl stimmt ...
      p <- c(input$pkt1, input$pkt2, input$pkt3, input$pkt4)  # ... nimm alle Spieler...
      pktSpieler <- sum(p[getSpieler()])                      # ... summiere die Punkte der Spieler...
      pktNichtSpieler <- sum(p[getNichtSpieler()])            # ... und summiere die Punkte der 
                                                              #     Nicht-Spieler ...
      ergebnis <- c(pktSpieler, pktNichtSpieler)              # ... und bastle eine Ergebnisvektor
    }
    return(ergebnis)
  }
  
  #'##########################################################################################################
  #' Funktion: is.Schneider() - testet, ob eine Spielerpartei weniger als 30 Punkte hat
  #'
  #' @param punkte, voreingestellt calculatePunkte(), hier werden für die Spielerparteien bereits die Punkte
  #' berechnet. 
  #'
  #' @return logical - wenn Spieler oder Nichtspieler weniger als 30 Pkt haben, zählt das als Schnieder: 
  #'                   TRUE/FALSE
  #' @export keiner
  #'
  #' @examples c(110, 10) -> TRUE, c(80, 40) -> FALSE, c(40, 80) -> FALSE
  #'##########################################################################################################
  
  is.Schneider <- function(punkte = calculatePunkte()) {
    return(length(punkte[punkte < 30]) > 0)
  }
  
  #'##########################################################################################################
  #' Funktion: is.Schwarz() - testet, ob eine Spielerpartei 0 Punkte hat
  #'
  #' @param punkte, voreingestellt calculatePunkte(), hier werden für die Spielerparteien bereits die Punkte
  #' berechnet.
  #'
  #' @return logical - wenn Spieler oder Nichtspieler 0 Pkt haben, zählt das als Schwarz: TRUE/FALSE
  #' @export keiner
  #'
  #' @examples punkte = c(0, 120) -> TRUE, punkte = c(120, 0) -> TRUE, punkte(110, 10) -> FALSE
  #'##########################################################################################################
  
  is.Schwarz <- function(punkte = calculatePunkte()) {
    return(length(punkte[punkte < 1]) > 0)
  }
  
  #'##########################################################################################################
  #' Funktion: is.Bettler() - gibt an, ob ein Solo 'Bettler' gespielt wurde
  #'
  #' @return logical - TRUE für Einstellung Solo/Bettler
  #' @export kein
  #'
  #' @examples s.o.
  #'##########################################################################################################
  
  is.Bettler <- function() {
    return(input$spielArt2 == "8")
  }
  
  #'##########################################################################################################
  #' Funktion: is.Ramsch() - gibt an, ob ein 'nachträgliches' Solo 'Ramsch' mit gesondertem Traif
  #'                         gespielt wurde
  #'
  #' @return logical - TRUE für Einstellung Solo/Ramsch
  #' @export kein
  #'
  #' @examples s.o.
  #'##########################################################################################################
  
  is.Ramsch <- function() {
    return(input$spielArt2 == "9")
  }
  
  #'##########################################################################################################
  #' Funktion: findWinner() - findet heraus, wer gewonnen hat, Spieler oder Nichtspieler 
  #'
  #' @return Vektor, der jeden Spieler den Multiplikator 1 oder -1 zurückgibt, mit der Tarif verrechnet und 
  #'         aufsummiert wird
  #' @export
  #'
  #' @examples Spieler 1 und 3 haben gewonnen -< c(1, -1, 1, -1), NichtSpieler 2 und 4 haben gewonnen -> 
  #' c(-1, 1, -1, 1)
  #'##########################################################################################################

  findWinner <- function() {
    # update der Spieler/Nichtspieler
    sp <- c(input$sp1, input$sp2, input$sp3, input$sp4)
    hatGespielt <- ifelse(sp, as.numeric(input$spielArt1), -1)
    
    # rechne die Punkte aus
    pkt <- calculatePunkte()
    
    # Wenn alle Regeln eingehalten wurden und die Summe der Punkte 120 ergibt
    if ((length(which(pkt==-1)) == 0) & checkPoints()) {
    
      # dann lege die Gewinner und Verlierer mit dem jeweiligen Tariffaktor fest
      if (is.Bettler()) {   # Sondersituation 'Bettler'
        hatGespielt <- hatGespielt * ifelse(pkt[1] > 0, -1, 1)
      } else if (is.Ramsch()) {
        hatGespielt <- hatGespielt * -1
      } else {
        hatGespielt <- hatGespielt * ifelse(pkt[1] < 61, -1, 1) 
      }
    } else {
      # ansonsten 0, dann wird nichts berechnet
      hatGespielt <- c(0,0,0,0)
    }
    
    return(hatGespielt)
  }
  
  #'##########################################################################################################
  #' Funktion: calculateProfit() - berechnet für die gespielte Runde den Gewinn
  #'
  #' @return Vektor, der den Gewinn jeweils für die spieler enthält - je nachdem, ob gewonnen oder verloren 
  #' @export
  #'
  #' @examples Spieler 1 und 2 gewinnen das Sauspiel mit 3 Laufenden -> 40 Cent -> c(40, 40, -40, -40)
  #'##########################################################################################################
 
  calculateProfit <- function() {
    # Berechne den Tarif für das Spiel
    if (!is.Ramsch()) {
      gt <- tarif[as.numeric(input$spielArt1)] + tarif[1] * as.numeric(is.Schneider() & !is.Bettler()) + 
            tarif[1] * as.numeric(is.Schwarz() & !is.Bettler()) + tarif[1] * as.numeric(input$anzahlLaufende)
    } else {
      gt <- tarif[1]
    }
    
    # Lege fest wer was bekommt oder bezahlen muß
    profit <-  findWinner() * gt * (2 ^ (input$anzahlGelegt + as.integer(input$soloArt)))
    
    return(profit)
  }
  
  # --- Bildschirmanzeigen -----------------------------------------------------------------------------------

  #'##########################################################################################################
  #' Funktion: renderPlayer() - stellt die Namen der Spieler auf dem UI dar
  #'
  #' @return
  #' @export
  #'
  #' @examples
  #'##########################################################################################################
  
  renderPlayer <- function() {
    output$Spieler1 <- renderText({ paste(spieler[1]) })
    output$Spieler2 <- renderText({ paste(spieler[2]) })
    output$Spieler3 <- renderText({ paste(spieler[3]) })
    output$Spieler4 <- renderText({ paste(spieler[4]) }) 
  }
  
  #'##########################################################################################################
  #' Funktion: reset() - setzt das Inputpanel, d.h. die Eingabemaske wieder auf den Ausgangszustand
  #'
  #' @return leer
  #' @export keiner
  #'
  #' @examples
  #'##########################################################################################################
  
  reset <- function() {
    updateRadioButtons(session, "spielArt1", selected = 1)
    updateSelectInput(session, "spielArt2", selected = 1)
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
  
  #'##########################################################################################################
  #' Funktion: updateTabelleGrafik() - zeichnet die Tabelle und den graphischen Spielverlauf neu
  #'
  #' @return
  #' @export
  #'
  #' @examples
  #'##########################################################################################################

  updateTabelleGrafik <- function() {
    output$gewinnTabelle <- renderDataTable(spielverlauf)
    output$gewinnPlot <- renderPlot({
      y.min <- min(min(spielverlauf[,1]), min(spielverlauf[,2]), min(spielverlauf[,3]), min(spielverlauf[,4]))
      y.max <- max(max(spielverlauf[,1]), max(spielverlauf[,2]), max(spielverlauf[,3]), max(spielverlauf[,4]))
      plot(spielverlauf[,1], type = "l", lwd = 2, col = spielerFarbe[1], xlab = "Spiel", 
           ylab = "Gewinn", main = "Spielverlauf", ylim = c(y.min, y.max))
      lines(spielverlauf[,2], col = spielerFarbe[2], lwd = 2)
      lines(spielverlauf[,3], col = spielerFarbe[3], lwd = 2)
      lines(spielverlauf[,4], col = spielerFarbe[4], lwd = 2)
      legend("topleft", spieler, col = spielerFarbe, lty = 1, lwd = 2, ncol = 4, border = NULL)
    })    
  }
  
  #'##########################################################################################################
  #' Funktion: displayGroup() - zeigt die Spielgruppe am dem Bildschirm an
  #'
  #' @return keine
  #' @export keine
  #'
  #' @examples
  #'##########################################################################################################

  displayGroup <- function() {
    # Anzeige der Spieler
    renderPlayer()
    
    # ... der Spielart
    updateSelectInput(session, "spielArt2", choices = spiele[[1]])
    
    # ... der Tarife
    updateNumericInput(session, "tarifSpiel", value = tarif[1])
    updateNumericInput(session, "tarifSolo", value = tarif[3])
    
    # ... und der Tabelle und des graphischen Spielverlaufs
    updateTabelleGrafik()
    
  }
  
  #'##########################################################################################################
  #' Funktion: setAndDisplay() - setzt verschiedene Einstellung und zeigt die Informationen auf dem Bildschirm
  #'
  #' @return keine
  #' @export keine
  #'
  #' @examples
  #'##########################################################################################################
  setAndDisplay <- function() {
    
    if (!is.null(groupsDF)) {
      # Allgemeinen Dataframe Typen zuordnen
      setTypesOfGroupsDF()
    
      # Spielrunde aktualisieren
      spielrunde <<- as.list(groupsDF[,'Gruppe'])
    
      # Aktive Gruppe setzen bzw. aktualisieren
      setActiveGroup()
    
      # Anzeige der Spielgruppenauswahl mit der gelieferten aktiven Gruppe und Tarife
      updateNumericInput(session, "tarifSpiel", value = tarif[1])
      updateNumericInput(session, "tarifSolo", value = tarif[3])
      updateSelectInput(session, "gruppeWaehlen", choices = spielrunde, 
                        selected = groupsDF[getZuletztAktiv(), 'Gruppe'])

    
      # Anzeige der Spielgruppendaten der aktiven Gruppe
      displayGroup()
    }
    
  }
  
  #'#######################################################################################################
  #' Funktion: updateSpielerBeiRamsch() - setzt die Checkbox beim Spieler mit der höchsten Punktzahl
  #'
  #' @return kein
  #' @export kein
  #'
  #' @examples
  #'#######################################################################################################

  updateSpielerBeiRamsch <- function() {
    s <- c("sp1", "sp2", "sp3", "sp4")                      # Spieler
    p <- c(input$pkt1, input$pkt2, input$pkt3, input$pkt4)  # Punkte holen ...
    pM <- p == max(p)                                       # welcher Spieler hat dich höchste Punktzahl
    for(i in seq(1:4)) {                                    # Checkbox für Checkbox updaten...
      updateCheckboxInput(session, s[i], value = pM[i])
    }
  }
  
  # --- Support-Funktionen --------------------------------------------------------------------------------

  #'#######################################################################################################  
  #' Funktion: benenneDoppelteSpielerNeu() - prüft auf doppelt eingegebene oder fehlende Spielernamen und 
  #'                                         ergänzte diese
  #'
  #' @param neueSpieler Vector - enthält die eingegebenen Spielernamen
  #'
  #' @return Vector, ergänzte Spielernamen
  #' @export
  #'
  #' @examples neueSpieler("qw, "qw", "", "er") -> neueSpieler("qw", "qw2", "Spieler3", "er")
  #'#######################################################################################################
  
  benenneDoppelteSpielerNeu <- function(neueSpieler) {
    # Position des Spielers in der Liste ...
    addNum <- seq(1:length(neueSpieler))
    
    # Wer hat keinen Namen eingegeben bekommen?
    keinName <- which(neueSpieler == "")
    neueSpieler[keinName] <- paste0("Spieler", addNum[keinName])
    
    # Welcher Name ist doppelt eingegeben worden
    neu <- which(duplicated(neueSpieler))
    neueSpieler[neu] <- paste0(neueSpieler[neu], addNum[neu])
    
    return(neueSpieler)
  }
  
#'#######################################################################################################
#' Funktion: testeStartkapitalAufNull() - wenn für das Startkapital nichts eingegeben ist, wird dafür
#'                                        0 gesetzt
#'
#' @param sk - Vektor, beinhaltet das jeweilige Startkapital der einzelnen Spieler
#'
#' @return korrigierter Vektor mit den auf gesetzten Startkapitalen
#' @export keiner
#'
#' @examples c("", 2, "", 40) -> c(0, 2, 0, 40)
#'#######################################################################################################
  
  testeStartkapitalAufNull <- function(sk) {
    return(ifelse(sk == "", 0, sk))
  }
  
  # --- Initialisierung -----------------------------------------------------------------------------------
  
  init()

  # --- Eventhandling -------------------------------------------------------------------------------------
  
  # Tarifdaten werden von den Spielern geändert ...
  observeEvent(input$tarifSpiel, {
    nr <- getZuletztAktiv()
    groupsDF[nr, 'GrundtarifSpiel'] <<- input$tarifSpiel
    tarif[1] <<- input$tarifSpiel
  })
  
  observeEvent(input$tarifSolo, {
    nr <- getZuletztAktiv()
    groupsDF[nr, 'GrundtarifSolo'] <<- input$tarifSolo
    tarif[3] <<- input$tarifSolo   
  })
  
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
    # Nochmal auf Ramsch testen und sicherstellen, daß der Spieler mit den meisten Punkten gewählt ist
    updateSpielerBeiRamsch()
    
    # Gewinn der Spielrunde berechnen
    p <- calculateProfit()
    
    # Hier noch Test auf Korrektheit von p, d.h. Profit darf nicht 4x 0 beinhalten
    if (length(which(p == 0)) != 4) {
    
      # Daten zusammenfassen
      df <- data.frame(as.integer(p[1]), as.integer(p[2]), as.integer(p[3]), as.integer(p[4]),
                     as.integer(p[1]), as.integer(p[2]), as.integer(p[3]), as.integer(p[4]),
                     as.integer(input$spielArt1), as.integer(input$spielArt2), as.integer(input$soloArt),
                     as.integer(input$pkt1), as.integer(input$pkt2), as.integer(input$pkt3), as.integer(input$pkt4),
                     as.integer(input$anzahlGelegt), as.integer(input$anzahlLaufende),
                     as.numeric(is.Schneider()), as.numeric(is.Schwarz()), Sys.time())
      df <- setColsSpielverlauf(df)
      
      # Neuanlegen ... 
      if (is.null(spielverlauf)) {
        sk <- c('Startkapital1', 'Startkapital2','Startkapital3', 'Startkapital4')
        df[spieler] <- df[spieler] + groupsDF[getZuletztAktiv(), sk]
        spielverlauf <<- df
      } else if (nrow(spielverlauf) == 0) {
        sk <- c('Startkapital1', 'Startkapital2','Startkapital3', 'Startkapital4')
        df[spieler] <- df[spieler] + groupsDF[getZuletztAktiv(), sk]
        spielverlauf <<- df
      # ...oder Hinzufügen
      } else {
        df[spieler] <- df[spieler] + spielverlauf[nrow(spielverlauf), spieler]
        spielverlauf <<- rbind(spielverlauf, df)
      }
      
      # Ausgabe
      updateTabelleGrafik()
      
      # Speichern des aktuellen Ergebnisses
      saveActiveGroup()
      reset()
      
    } else {
      session$sendCustomMessage("saveTable", "Stimmt nicht!")
    }

  }, ignoreInit = TRUE)
  
  # Letzten Eintrag zurücksetzen
  observeEvent(input$letztesKorrigieren, {
    # Letzte Tabellenzeile löschen
    spielverlauf <<- spielverlauf[-c(nrow(spielverlauf)),]
    
    # Ausgabe nach Korrektur
    updateTabelleGrafik()
    
    # Speichern des aktuellen Ergebnisses
    saveActiveGroup()
    reset()
  }, ignoreInit = TRUE)
  
  # Spielgruppe wechseln
  observeEvent(input$gruppeWaehlen, {
    # Daten der Gruppe abspeichern
    saveActiveGroup(testExist = TRUE)
    
    # Neue Gruppe setzen
    nr <- which(groupsDF$Gruppe == input$gruppeWaehlen)
    setZuletztAktiv(nr)
    setActiveGroup()
    
    # ... und anzeigen
    displayGroup()
  }, ignoreInit = TRUE)
  
  # Neue Spielgruppe erstellen und anfügen
  observeEvent(input$neueGruppe, {
    
    showModal(gruppeErstellen)
    
  }, ignoreInit = TRUE)
  
  # Modaldialog 'Gruppe erstellen'
  observeEvent(input$modalGruppeErstellenOK, {
    # Dateiname für Spielverlauf benennen
    fileSpielverlauf <- paste0(input$grName, "_SK.skr")
    # Test auf doppelte Spielernamen und fehlendes Startkapital
    neueSpieler <- benenneDoppelteSpielerNeu(c(input$sp1Name, input$sp2Name, input$sp3Name, input$sp4Name))
    startkapital <- testeStartkapitalAufNull(c(input$sp1Kapital, input$sp2Kapital, input$sp3Kapital, input$sp4Kapital))
    
    # Dataframe herrichten
    df <- data.frame(input$grName, 
                     neueSpieler[1], neueSpieler[2], neueSpieler[3], neueSpieler[4],
                     startkapital[1], startkapital[2], startkapital[3], startkapital[4],
                     Sys.time(), input$tarifSpielErst, input$tarifSoloErst, fileSpielverlauf,
                     "red", "green", "blue", "orange", 1)
    df <- setColsGroupsDF(df)
    
    # Dataframe entweder allgemein definieren oder an bereits existenten anhängen
    if (is.null(groupsDF)) {
      groupsDF <<- df
    } else {
      groupsDF$zuletztAktiv <<- 0
      groupsDF <<- rbind(groupsDF, df)
    }
 
    # Die Parameter setzen und die Spielgruppe anzeigen
    setAndDisplay()
    
    # Modaldialog schließen
    removeModal()
    
  }, ignoreInit = TRUE)
  
  # Die ausgewählte Gruppe aus der Liste löschen: 2. Stufe - 
  #                                               Löschen einer Gruppe ausführen, wenn der User zustimmt!
  observeEvent(input$modalWLoesch, {
    
    # Löschen der Spielverlaufsdaten
    file.remove(groupsDF[getZuletztAktiv(), 'DateiSpielliste'])
    #Spielgruppe löschen
    groupsDF <<- groupsDF[c(-getZuletztAktiv()), ]
    
    # Erste Gruppe in der Liste als aktiv setzten
    setZuletztAktiv(1)
    # Daten anpassen und anzeigen
    setAndDisplay()
    # Den momentanen Stand der Spielgruppen abspeichern
    saveActiveGroup()
    
    # Modaldialog schließen
    removeModal()
  }, ignoreInit = TRUE)
  
  # Die ausgewählte Gruppe aus der Liste löschen: 1. Stufe - User fragen, ob die Gruppe gelöscht werden soll
  observeEvent(input$loescheGruppe, {
    showModal(gruppeWirklichLoeschen)
  }, ignoreInit = TRUE)
  
  # Welche Soloart wird ausgewählt? Hier wichtig für das Spiel 'Ramsch' -> Auswahl des Spielers
  observeEvent(input$spielArt2, {
    if (is.Ramsch())
      updateSpielerBeiRamsch()
  }, ignoreInit = TRUE)

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
  
  # --- Bei Sessionende ... --------------------------------------------------------------------------------
  
  session$onSessionEnded(function() {
    sch <- "Aktuellen Spielstand "
    sch <- paste0(sch, ifelse(saveActiveGroup(testExist = TRUE), "" , "nicht "))
    sch <- paste0(sch, "gespeichert! (c) SchafkopfApp by G. Füchsl - Auf Wiedersehen, bis zum nächsten Mal ...")
    print(sch)
    stopApp()
  })
  
})
