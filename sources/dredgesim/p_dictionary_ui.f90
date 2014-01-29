! -------------------------------------------------------------------------
! HeadOfPackageUserInterface ----------------------------------------------
!
!! <H2>Work Package "Dictionary"></h2>
!! @author Susanne Spohr
!! @version 1.13 vom 15.03 07, Quellcode: mod_p_dictionary_ui.f90
!! <HR>
!! <one line to give the program's name and an idea of what it does> <BR>
!
!  Copyright-Hinweis
!
!  Copyright (C) 2002 Bundesanstalt fuer Wasserbau
!
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2002-05-15 : Susanne Spohr : Re-Engineering des Moduls mod_steuerdatei.f90 der Library dictionary 
!  01.02 : 2002-06-27 : Susanne Spohr : Package-Struktur ueberarbeitet (+ _data-Modul), init_.._all_errors nun nur in UI-Modul, ...
!  01.03 : 2002-07-02 : Susanne Spohr : CALL set_file_action ( dicdat, 'READ' )
!  01.04 : 2002-07-02 : Susanne Spohr : set_file_action in USE, ONLY-Anweisung eingetragen
!  01.05 : 2002-07-03 : Susanne Spohr : Kommentarkennzeichnung !$ entfernt
!  01.06 : 2002-07-03 : Susanne Spohr : DEALLOCATE ( all_errors )
!  01.07 : 2002-07-11 : Susanne Spohr : Verbesserung Fehlertexte
!  01.08 : 2004-12-15 : Susanne Spohr : Copyright-Hinweis geaendert
!  01.09 : 2005-02-09 : Susanne Spohr : Fehlertext -2580, -2590 leicht umgestellt
!  01.10 : 2005-07-07 : G. Lang       : set_file_path_and_name in getdictionfile
!  01.11 : 2007-02-09 : P. Schade     : FM -350: Dictionary-Datei nicht vorhanden
!  01.12 : 2007-02-09 : P. Schade     : String-Kosmetik
!  01.13 : 2007-03-15 : G. Lang       : ? werden wieder durch Leerzeichen ersetzt (siehe +GL+)
!
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Packages</H3>
!! Lesen einer Eingabedatei, welche ihre Beschreibung in            <BR>
!! einer Dictionary-Datei findet.                                   <BR>
!! ...                                                              <BR>
!! ...                                                              <BR>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt keinen &ouml;ffentlich zug&auml;nglichen     <BR>
!! Datentyp zur Verf&uuml;gung.                                     <BR>
!! <BR>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>&Ouml;ffentliche Methoden</H3>
!!                                                                  <BR>
!! <UL>
!!    <LI> <EM>INIT_dictionary</EM> 
!!    <OL>
!!       <LI> Allokieren der statischen Daten des Packages;
!!       <LI> Initialisieren der statischen Daten mit Default-Werten;
!!       <LI> ggf. Initialisieren aller weiteren zu diesem Package geh&ouml;renden Module.
!!    </OL>
!!    <LI> <EM>CLEAR_dictionary</EM>
!!    <OL>
!!       <LI> De-Allokieren der statischen Daten des Moduls;
!!       <LI> Re-Initialisieren einiger statischer Daten mit Default-Werten;
!!       <LI> ggf. Re-Initialisieren aller weiteren zu diesem Package geh&ouml;renden Module.
!!    </OL>
!!    <LI> <EM>SETUP_dictionary_PRN_LUN</EM>
!!    <OL>
!!       <LI> Setzen der logischen Kanalnummer <EM>PRN_LUN</EM> f&uuml;r PRINT-Methoden;
!!       <LI> keine Ausgabe: <EM>PRN_LUN</EM> = -1;
!!       <LI> Ausgabe: <EM>PRN_LUN</EM> = >0;
!!       <LI> <EM>Hinweis</EM>: <EM>PRN_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
!!    </OL>
!!    <LI> <EM>SETUP_dictionary_TRC_LUN</EM>
!!    <OL>
!!       <LI> Setzen der logischen Kanalnummer <EM>TRC_LUN</EM> f&uuml;r TRACE-Methoden;
!!       <LI> keine Ausgabe: <EM>TRC_LUN</EM> = -1;
!!       <LI> Ausgabe: <EM>TRC_LUN</EM> = >0;
!!       <LI> <EM>Hinweis</EM>: <EM>TRC_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
!!    </OL>
!!    <LI> <EM>PRINT_dictionary_ALL_ERRORS</EM>
!!    <OL>
!!       <LI> Alle Fehlermeldungen des Packages auf <EM>PRN_LUN</EM> ausgeben;
!!       <LI> ggf. auch Fehlermeldungen aller weiteren zu diesem Package geh&ouml;renden Module ausgeben;
!!       <LI> <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
!!    </OL>
!!    <LI> <EM>PRINT_dictionary_STATIC</EM>
!!    <OL>
!!       <LI> Alle statischen Daten des Paketes die nicht zu den (Paket-) Objekten
!!            geh&ouml;ren werden auf <EM>PRN_LUN</EM> ausgeben;
!!       <LI> <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
!!    </OL>
!!    <LI> <EM>TEST_dictionary</EM>
!!    <OL>
!!       <LI> F&uuml;hre viele Methoden des Packages mit internen Testdaten aus;
!!       <LI> Ergebnisse werden auf <EM>PRN_LUN</EM> ausgegeben.
!!    </OL>
!! </UL>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Verwenden des Moduls</H3>
!!                                                                  <BR>
!! Die Leistungen des Moduls k&ouml;nnen wie folgt in Anspruch genommen werden: <BR>
!! <OL>
!!    <LI> Einbinden des Moduls mittels USE-Anweisung in der rufenden Programmeinheit;
!!    <LI> Initialisieren des Packages p_dictionary_ui mit INIT-Methode;
!!    <LI> ggf. Verwenden verschiedener SETUP-Methoden;
!!    <LI> Verwenden beliebiger Package-Methoden (nicht INIT und CLEAR);
!!    <LI> De-Initialisieren des Packages p_dictionary_ui mit CLEAR-Methode.
!! </OL>
!! <HR>
!! Als ausf&uuml;hrliches Beispiel sei an dieser Stelle auf die
!! Modulprozedur <EM>TEST_dictionary</EM> verwiesen.
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Fehlersituationen des Moduls</H3>
!!                                                                    <BR>
!! Hinweis: einige Methoden dieses Moduls erzeugen Fehlermeldungen,   <BR>
!!          andere nicht. 
!!          Routinen die Fehlermeldungen generieren m&uuml;ssen pr&uuml;fen,  <BR>
!!          ob das Modul korrekt initialisert wurde (ok_initialised)  <BR>
!!                                                                    <BR>
!! <HR>
!! Allgemein             [  0000 bis  0999 ]                        <BR>
!! 00000 = kein Fehler                                              <BR> 
!! 00001 = Modul ist nicht initialisiert.                           <BR>
!! 00002 = Modul ist schon initialisiert.                           <BR>
!! <HR>
!! INIT-Methoden         [  1000 bis  1999 ]                        <BR>
!! <HR>
!! CLEAR-Methoden        [  2000 bis  2999 ]                        <BR>
!! <HR>
!! SETUP-Methoden        [  3000 bis  3999 ]                        <BR>
!! <HR>
!! NEW-Methoden          [  4000 bis  4999 ]                        <BR>
!! <HR>
!! KILL-Methoden         [  5000 bis  5999 ]                        <BR>
!! <HR>
!! OK-Methoden           [  6000 bis  6999 ]                        <BR>
!! <HR>
!! PRINT-Methoden        [  7000 bis  7999 ]                        <BR>
!! 07500 = Drucken der statischen Daten (ohne Fehlermeldungen)      <BR>
!! <HR>
!! SET-Methoden          [  8000 bis  8999 ]                        <BR>
!! <HR>
!! GET-Methoden          [  9000 bis  9999 ]                        <BR>
!! <HR>
!! TEST-Methoden         [ 19000 bis 19999 ]                        <BR>
!! <HR>
!! modul-spezifische Methoden   [      < 0 ]                        <BR>
!! .-10000 = Fehler bei Speicher-ALLOCATE Anweisung      <BR>
!! .-20000 = Fehler bei Speicher-DEALLOCATE Anweisung<BR>
!! .-   10 = Name der Dictionary-Datei ist laenger als 80 Zeichen<BR>
!! .-   20 = Unbekannter Name eines Datenblocks (Eingabedatei)<BR>
!! .-   30 = Ein Block wurde mehrfach spezifiziert (Eingabedatei)<BR>
!! .-   40 = Ein Block fehlt (Eingabedatei)<BR>
!! .-   50 = Stringlaenge von <char_value> zu kurz in get_input_data-CALL<BR>
!! .-   60 = Rueckgabefeld ist bereits associated in get_input_data-CALL<BR>
!! .-   70 = Stringlaenge von <char_array> zu kurz in get_input_data-CALL<BR>
!! .-   80 = Block ohne Eingabezeile existiert (Eingabedatei)<BR>
!! .-   90 = Block enthaelt eine Eingabe-Zeile doppelt (Eingabedatei)<BR>
!! .-  100 = Schluesselwortzeile fehlt (Eingabedatei)<BR>
!! .-  110 = Laenge von Kommentarzeilen ueberschritten (Eingabedatei)<BR>
!! .-  120 = Lesefehler beim Lesen eines Parameters (Eingabedatei)<BR>
!! .-  130 = Zuviele Angaben in Schluesselwortzeile (Eingabedatei)<BR>
!! .-  140 = Zuwenig Angaben in Schluesselwortzeile (Eingabedatei)<BR>
!! .-  150 = Blockname zu lang in UI-Methoden-CALL<BR>
!! .-  160 = Schluesselwort zu lang in UI-Methoden-CALL<BR>
!! .-  170 = Blockname ist ungueltig in UI-Methoden-CALL<BR>
!! .-  180 = Schluesselwort ist ungueltig in UI-Methoden-CALL<BR>
!! .-  190 = Parameter-Positionsnummer ist ungueltig in UI-Methoden-CALL<BR>
!! .-  200 = Blocknummer ist kleiner als 1 in UI-Methoden-CALL<BR>
!! .-  210 = Blocknummer groesser als Blockanzahl in UI-Methoden-CALL<BR>
!! .-  220 = Parameter "dattype" hat unzulaessigenn Wert<BR>
!! .-  230 = Rueckgabewert hat falschen Typ in get_input_data-CALL<BR>
!! .-  240 = Da L_OneArray=F ist ein gemeinsamer get_input_data-CALL nicht erlaubt<BR>
!! .-  250 = Da L_Opt=T ist ein gemeinsamer get_input_data-CALL nicht erlaubt<BR>
!! .-  260 = Da L_Array = T ist ein gemeinsamer get_input_data-CALL nicht erlaubt<BR>
!! .-  270 = Zeilennummer groesser als Zeilenzahl in get_input_data-CALL<BR>
!! .-  280 = Zeilennummer negativ in get_input_data-CALL<BR>
!! .-  290 = Da L_Array = T, Rueckgabefeld benoetigt in get_input_data-CALL<BR>
!! .-  300 = Da KEY L_Single=F, Rueckgabefeld benoetigt in get_input_data-CALL<BR>
!! .-  310 = Parameter <l_exist> fehlt in get_input_data-CALL (Block optional)<BR>
!! .-  320 = Parameter <l_exist> fehlt in get_input_data-CALL (Key optional)<BR>
!! .-  330 = Parameter <l_exist> fehlt in get_input_data-CALL (Parameter optional)<BR>
!! .-  340 = Name der Dictionary-Datei zu lang<BR>
!! .-  510 = Ungueltige BEGINDATA-Struktur (Dictionary-Datei)<BR>
!! .-  520 = BLOCK-Beschreibung ohne Namen (Dictionary-Datei)<BR>
!! .-  530 = Name einer BLOCK-Beschreibung zu lang (Dictionary-Datei)<BR>
!! .-  540 = Keine BLOCK-Beschreibung (Dictionary-Datei)<BR>
!! .-  550 = Mehrfach beschriebener BLOCK (Dictionary-Datei)<BR>
!! .-  560 = Ungueltige BEGINDATA-Struktur in BLOCK-Beschreibung (Dictionary-Datei)<BR>
!! .-  570 = KEY-Beschreibung ohne Schluesselwort (Dictionary-Datei)<BR>
!! .-  580 = Schluesselwort zu lang (Dictionary-Datei)<BR>
!! .-  590 = Fehlende Schluesselwortzeile (Dictionary-Datei)<BR>
!! .-  610 = Vereinbarung als optional fehlt (Dictionary-Datei)<BR>
!! .-  620 = BLOCK ohne KEY-Beschreibung (Dictionary-Datei)<BR>
!! .-  630 = Mehrfach beschriebener KEY (Dictionary-Datei)<BR>
!! .-  640 = Ungueltige BEGINDATA-Struktur in KEY-Beschreibung (Dictionary-Datei)<BR>
!! .-  650 = PARameter-Beschreibung ohne Parameternummer (Dictionary-Datei)<BR>
!! .-  660 = Fehler beim Lesen der Parameternummer (Dictionary-Datei)<BR>
!! .-  670 = Einziger Parameter eines KEYs auf TRUE gesetzt (Dictionary-Datei)<BR>
!! .-  680 = In Kommentarzeile mehr als ein Parameter spezifiziert (Dictionary-Datei)<BR>
!! .-  690 = Falscher Parametertyp bei Kommentarzeile (Dictionary-Datei)<BR>
!! .-  700 = L_Array = True gesetzt, trotz Kommentarzeile (Dictionary-Datei)<BR>
!! .-  710 = Kein Parameter spezifiziert in KEY-Beschreibung (Dictionary-Datei)<BR>
!! .-  720 = Mehrfach beschriebener Parameter (Dictionary-Datei)<BR>
!! .-  730 = Parameter-Nummer ausserhalb des zulaessigen Wertebereiches (Dictionary-Datei)<BR>
!! .-  740 = Unzulaessige L_Array-Vereinbarung fuer Parameter (Dictionary-Datei)<BR>
!! .-  750 = Unzulaessige L_Opt-Vereinbarung fuer Parameter (Dictionary-Datei)<BR>
!! .-  760 = Unzulaessige L_File...-Vereinbarung fuer Parameter (Dictionary-Datei)<BR>
!! .-  770 = L_FileReq und L_FileNew auf TRUE gesetzt (Dictionary-Datei)<BR>
!! .-  780 = Ungueltiger Wert fuer Parameter-Datentyp (Dictionary-Datei)<BR>
!! .- 1010 = Block-Beschreibung enthaelt eine Check-Zeile doppelt (Dictionary-Datei)<BR>
!! .- 1020 = Unterprogramm-Aufruf ohne Check-Anforderung<BR>
!! .- 1030 = Parameter-Pointerfeld <bloecke> ist .not. associated<BR>
!! .- 1040 = Parameter <ck_art> hat unerlaubten Wert<BR>
!! .- 1050 = Objektexistenz haengt von Objekt derselben Adresslinie ab(Dictionary-Datei)<BR>
!! .- 1060 = Vergleichs-Objekt wurde nicht als optional vereinbart (Dictionary-Datei)<BR>
!! .- 1070 = Vergleichs-Parameter ist eine Feldgroesse (Dictionary-Datei)<BR>
!! .- 1080 = Vgl.-Parameter nicht identifizierbar, da BLOCK L_Single = F (Dictionary-Datei)<BR>
!! .- 1090 = Vgl.-Parameter nicht identifizierbar, da KEY L_Single = F (Dictionary-Datei)<BR>
!! .- 1100 = Das Parameter-Pointerfeld <bloecke> ist .not. associated<BR>
!! .- 1110 = ReqIf|NotIf-Zeilen schliessen sich gegenseitig aus (Dictionary-Datei)<BR>
!! .- 1120 = Unterprogramm-Aufruf ohne CheckIfPar-Anforderung<BR>
!! .- 1130 = CheckIfPar-Zeile enthaelt unzulaessige Vergleichsoperation (Dictionary-Datei)<BR>
!! .- 1140 = Falscher Datentyp in CheckIfPar-Zeile (Dictionary-Datei)<BR>
!! .- 1150 = Feldgroesse in CheckIfPar-Zeile (Dictionary-Datei)<BR>
!! .- 1160 = L_Single=F fuer Key des Wertes in CheckIfPar-Zeile (Dictionary-Datei)<BR>
!! .- 1170 = L_Single=F fuer Block des Wertes in CheckIfPar-Zeile (Dictionary-Datei)<BR>
!! .- 1180 = Wert in CheckIfPar-Zeile ist optional (Dictionary-Datei)<BR>
!! .- 1190 = Falscher Datentyp in CheckIfPar-Zeile (Dictionary-Datei)<BR>
!! .- 1200 = <BR>
!! .- 1210 = <BR>
!! .- 1220 = <BR>
!! .- 1230 = <BR>
!! .- 1240 = <BR>
!! .- 1250 = <BR>
!! .- 1260 = <BR>
!! .- 1270 = <BR>
!! .- 1280 = <BR>
!! .- 1290 = <BR>
!! <HR>
!
MODULE p_dictionary_ui
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] BASIS-Modul "globale Konstantwerte"
  !
  USE b_constants, ONLY : &
       ! Parameter
       single,         &
       double
  !
  ! [A.2] BASIS-Modul "Fehler"
  !
  USE b_error, ONLY :   &
       ! Typdefinitionen
       t_error,             &
       ! Routinen
       init_error,          &
       clear_error,         &
       no_error,            &
       any_error,           &
       new_error,           &
       kill_error,          &
       print_error,         &
       setup_error_act,     &
       setup_error_prn_lun, &
       setup_error_trc_lun, &
       set_error_ierr,      &
       set_error_cerr
  !
  ! [A.3] BASIS-Modul "Dateibehandlung"
  !
  USE b_file, ONLY :       &
       ! Typdefinitionen
       t_file,             &
       ! Routinen
       init_file,          &
       clear_file,         &
       setup_file_prn_lun, &
       setup_file_trc_lun
  !
  ! [A.4] BASIS-Modul "Systembefehle"
  !
  USE b_cmds, ONLY :       &
       ! Routinen
       init_cmds,          &
       clear_cmds,         &
       setup_cmds_prn_lun, &
       setup_cmds_trc_lun
  !
  ! [A.5] Basis-Modul "Zeitinkrement"
  !
  USE b_time, ONLY :       &
       ! Routinen
       init_time,          &
       clear_time,         &
       setup_time_prn_lun, &
       setup_time_trc_lun
  !
  ! [A.6] Basis-Modul "Datum+Zeit"
  !
  USE b_datetime, ONLY :       &
       ! Routinen
       init_datetime,          &
       clear_datetime,         &
       setup_datetime_prn_lun, &
       setup_datetime_trc_lun
  !
  ! [A.7] Package-Module
  !
  USE m_dictionary_data, ONLY : &
       ! Variablen
       dicfile_name,            &
       infile_name,             &
       infile_path
  !
  !
  ! Hinweis: Module anderer Packages duerfen nicht verwendet werden.
  !
  ! ----------------------------------------------------------------------
  ! [B] Module des Paketes "dictionary"
  ! ----------------------------------------------------------------------
  !
  ! [B.1] Daten-Modul des Paketes "dictionary"
  !
  USE m_dictionary_data, ONLY : &
       !  globale Variablen des Paketes "dictionary"
       initialised, & ! Indikator Paketinitialisierung
       prn_op,      & ! Indikator Druckerausgabe
       trc_op,      & ! Indikator Drucken in Trace-Datei
       prn_lun,     & ! log. Fortran-Unit fuer Druckerausgabe
       trc_lun,     & ! log. Fortran-Unit fuer Traceausgabe
       all_errors     ! Fehlermeldunegn
  !
  ! [B.4] Diverse Datentypen und ihre verketteten Listen
  !
  USE m_stdat_types, ONLY : &
       !   Typdefinitionen
       t_block,           &
       t_key,             &
       t_vl_kz,           &
       t_vl_block,        &
       t_check,           &
       !   Parameter 
       key_len,           &
       line_len,          &
       !   Routinen / Interfaces
       birth_vl_kz,                      &
       birth_vl_block,                   &
       dealloc_t_block 
  !
  ! ---------------------------------------------------------------------
  ! --> alles muss explizit deklariert werden und ist default privat
  ! ---------------------------------------------------------------------
  !
  IMPLICIT NONE
  PRIVATE
  !
  ! ---------------------------------------------------------------------
  ! [C] oeffentlich zugaengliche Deklarationen (mit PUBLIC-Attribut)
  ! ---------------------------------------------------------------------
  !
  ! [C.1] (maximal) einen oeffentlich zugaenglichen Typ deklarieren
  !
  !       Hinweis: ein Package definiert keinen oeffentlich zugaenglichen
  !                Datentyp.
  !
  ! [C.2] Konstantwerte (Parameter) [moeglichst nicht verwenden]
  !
  ! [C.3] Variablen [moeglichst nicht verwenden]
  !
  ! [C.4] Schnittstellen
  !
  ! [C.4.1] erforderliche oeffentliche Schnittstellen
  !
  ! Hinweis: verschiedene Methoden arbeiten auf Skalar und 1D-Array.
  !          Ggf. weitere ergaenzen (z.B. 2D-Array) falls sinnvoll.
  !
  !! Allokieren/Initialisieren der statischen Datenobjekte des Packages
  INTERFACE init_dictionary
     MODULE PROCEDURE init_dictionary_d ! 
  END INTERFACE
  !
  !! De-Allokieren/De-Initialisieren der statischen Datenobjekte des Packages
  INTERFACE clear_dictionary
     MODULE PROCEDURE clear_dictionary_d ! 
  END INTERFACE
  !
  !! logische Kanalnummer f&uuml;r PRINT-Methoden auf Benutzerwert setzen
  INTERFACE setup_dictionary_prn_lun
     MODULE PROCEDURE setup_dictionary_prn_lun_d ! 
  END INTERFACE
  !
  !! logische Kanalnummer f&uuml;r TRACE-Methoden auf Benutzerwert setzen
  INTERFACE setup_dictionary_trc_lun
     MODULE PROCEDURE setup_dictionary_trc_lun_d ! 
  END INTERFACE
  !
  !! Drucken aller in diesem Modul abgelegten statischen Daten 
  !! die nicht zu den (Paket-) Objekten geh&ouml;ren
  INTERFACE print_dictionary_static
     MODULE PROCEDURE print_dictionary_static_d ! 
  END INTERFACE
  !
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Packages
  INTERFACE print_dictionary_all_errors
     MODULE PROCEDURE print_dictionary_all_errors_d ! 
  END INTERFACE
  !
  !! Pr&uuml;fen der Packagefunktionen mit internen Test-Daten
!temp  INTERFACE test_dictionary
!temp     MODULE PROCEDURE test_dictionary_d ! 
!temp  END INTERFACE
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  !! Einlesen einer Eingabedatei ueber eine Dictionary-Datei
  INTERFACE read_input_file
     MODULE PROCEDURE read_input_file_d !
  END INTERFACE
  !
  !! Hole Anzahl an Eingabebloecken eines bestimmten Typs
  INTERFACE get_nof_input_blocks
     MODULE PROCEDURE get_nof_input_blocks_d !
  END INTERFACE
  !
  !! Hole Anzahl an Schluesselwortzeilen eines bestimmten Typs
  INTERFACE get_nof_input_lines
     MODULE PROCEDURE get_nof_input_lines_d !
  END INTERFACE
  !
  !! Hole einen bestimmten Parameterwert
  INTERFACE get_input_data
     MODULE PROCEDURE get_input_data_char_value  !
     MODULE PROCEDURE get_input_data_int_value   !
     MODULE PROCEDURE get_input_data_real_value  !
     MODULE PROCEDURE get_input_data_doub_value  !
     MODULE PROCEDURE get_input_data_log_value   !
     MODULE PROCEDURE get_input_data_char_array  !
     MODULE PROCEDURE get_input_data_int_array   !
     MODULE PROCEDURE get_input_data_real_array  !
     MODULE PROCEDURE get_input_data_doub_array  !
     MODULE PROCEDURE get_input_data_log_array   !
  END INTERFACE
  !
  ! [C.5] Zuweisungen
  !
  ! ... ggf. ergaenzen
  !
  ! [C.6] Operatoren (optional, falls sinnvoll)
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  ! [C.7.1] unbedingt erforderliche oeffentliche Methoden
  !
  PUBLIC :: init_dictionary                   ! Initialisieren (Package)
  PUBLIC :: clear_dictionary                  ! De-Initialisieren (Package)
  PUBLIC :: setup_dictionary_prn_lun          ! Setzen prn_lun (Package)
  PUBLIC :: setup_dictionary_trc_lun          ! Setzen trc_lun (Package)
  PUBLIC :: print_dictionary_static           ! Drucken der statischen Daten (ohne Paket-Objektdaten)
  PUBLIC :: print_dictionary_all_errors       ! Drucken aller (moeglichen) Fehlermeldungen
!temp  PUBLIC :: test_dictionary                   ! Pruefen der Package-Funktionen
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: read_input_file                   ! Lesen der Eingabedatei
  PUBLIC :: get_nof_input_blocks              ! Hole Anzahl an Eingabebloecken eines Typs
  PUBLIC :: get_nof_input_lines               ! Hole Anzahl an Schluesselwortzeilen eines Typs
  PUBLIC :: get_input_data                    ! Hole Parmaterwert

  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Datentypen, Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] lokale Typdefinitionen
  !
  ! [D.2] Konstantwerte (Parameter)
  !
  !! Name des Moduls
  CHARACTER (LEN=15), PARAMETER :: c_modname      = 'p_dictionary_ui' ! 
  !! Delimiter-Wert
  CHARACTER (LEN=1) , PARAMETER :: c_delim        = '"'               ! 
  !! temporaerer Ersetzungswert 
  CHARACTER (LEN=1) , PARAMETER :: c_repla        = '?'               ! 
  !! leeres Trennzeichen 
  CHARACTER (LEN=1) , PARAMETER :: c_sign         = ' '               ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.          ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1               ! 
  !! Anzahl der Fehlermeldungen
  INTEGER           , PARAMETER :: c_nofallerrors = 149               ! ggf. modifizieren
  !! ....
  LOGICAL           , PARAMETER :: l_wri = .False.
  !
  ! [D.3] Variablen (statische Daten des Moduls)
  !
  !! Z&auml;hler f&uuml;r Initialisierungsaufrufe
  INTEGER                , SAVE :: n_init      = 0        ! 
  !
  !! Feld mit Beschreibungen der einzelnen Eingabebloecke
  TYPE (t_block  ), DIMENSION(:), POINTER, SAVE  :: bloecke
  !! Erstes Element der verketteten Liste zum Einlesen der Key-Zeilen
  TYPE (t_vl_kz)              , POINTER          :: fst_zeil
  !! Letztes Element der verketteten Liste zum Einlesen der Key-Zeilen
  TYPE (t_vl_kz)              , POINTER          :: lst_zeil
  !! Aktuelles Element der verketteten Liste zum Einlesen der Key-Zeilen
  TYPE (t_vl_kz)              , POINTER          :: act_zeil
  !! Vergleichs-Element der verketteten Liste zum Einlesen der Key-Zeilen
  TYPE (t_vl_kz)              , POINTER          :: vgl_zeil
  !! Erstes Element der verketteten Liste zum Einlesen der Eingabebl&ouml;cke
  TYPE (t_vl_block)           , POINTER          :: fst_egb
  !! Letztes Element der verketteten Liste zum Einlesen der Eingabebl&ouml;cke
  TYPE (t_vl_block)           , POINTER          :: lst_egb
  !! Aktuelles Element der verketteten Liste zum Einlesen der Eingabebl&ouml;cke
  TYPE (t_vl_block)           , POINTER          :: act_egb
  !
  ! [D.4] Schnittstellen
  !
  INTERFACE replace_repla_in_string
     MODULE PROCEDURE replace_repla_in_string_d
  END INTERFACE
  !
  ! [D.5] Assignments
  !
  ! [D.6] Operatoren
  !
CONTAINS
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !+                                                                     +
  !+           PPPPPP   UUU  UUU  BBBBBB   LLL    III   CCCCC            +
  !+           PPP  PP  UUU  UUU  BBB  BB  LLL    III  CCC  CC           +
  !+           PPP  PP  UUU  UUU  BBB  BB  LLL    III  CCC               +
  !+           PPPPPP   UUU  UUU  BBBBBB   LLL    III  CCC               +
  !+           PPP      UUU  UUU  BBB  BB  LLL    III  CCC               +
  !+           PPP      UUU  UUU  BBB  BB  LLL    III  CCC               +
  !+           PPP       UUUUUU   BBB  BB  LLL    III  CCC  CC           +
  !+           PPP        UUUU    BBBBBB   LLLLLL III   CCCCC            +
  !+                                                                     +
  !+                                                                     +
  !+   MM     MM  EEEEEEE TTTTTTT  HHH  HH   OOOOO   DDDDDD    SSSSS     +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD  SS   SS    +
  !+   MMMM MMMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD  SS         +
  !+   MMM M MMM  EEEEEE    TTT    HHHHHHH  OOO  OO  DDD  DD   SSSSSS    +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD       SSS   +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD       SSS   +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD  SSS  SSS   +
  !+   MMM   MMM  EEEEEEE   TTT    HHH  HH   OOOOO   DDDDDD    SSSSSS    +
  !+                                                                     +
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Oeffentliche Methoden mit Zugriff ueber PUBLIC Interfaces
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-INIT-Methoden <<< [ERR_NO =  1000 bis  1999]
  ! ----------------------------------------------------------------------
  !
  !! Allokieren/Initialisieren der statischen Daten des Packages
  SUBROUTINE init_dictionary_d &
       ( )
    !
    USE m_dredgesim_data, ONLY : DEBUG_ds
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='init_dictionary_d' 
    !
    IF ( .NOT. initialised ) THEN
       !
       ! [1.1] Drucken des Copyright-Hinweises
       !
       IF (DEBUG_ds > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "p_dictionary_ui" version 1.13 of 15.03 07                  '
          WRITE(*,*) ' Copyright (C) 2002 Bundesanstalt fuer Wasserbau       '
          WRITE(*,*)
          WRITE(*,*) ' '
       END IF
       !
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       !       
       !       Anm.: hier alle Basis-Module initialisieren, die in
       !             diesem Paket (in welchem Modul auch immer)
       !             verwendet werden.
       !
       ! [1.2.1] Error-Modul zuerst initialisieren
       !
       CALL init_error ( )
       !
       ! [1.2.2] ggf. weitere Module initialisieren
       !
       IF ( no_error( ) ) CALL init_file ( )
       IF ( no_error( ) ) CALL init_cmds ( )
       IF ( no_error( ) ) CALL init_time ( )
       IF ( no_error( ) ) CALL init_datetime ( )
       !
       !
       ! [1.3] vorlaeufiges Setzen von "initialised"
       !
       initialised = .true.
       !
       ! [1.5] paketspezifische Initialisierungen
       !
       IF ( no_error( ) ) CALL init_dictionary_all_errors ( ) 
       !
       ! [1.6] Initialisieren der logischen Kanalnummern
       !
       prn_lun = c_lun
       trc_lun = c_lun
       !
       ! [1.7] endgueltiges Setzen des Initialisierungs-Indikators
       !
       initialised = MERGE( .true., .false., no_error( ) )
       !
    END IF
    !
    ! 2.0 Initialisierungszaehler heraufsetzen
    !
    n_init = n_init + 1
    !
  END SUBROUTINE init_dictionary_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls
  SUBROUTINE clear_dictionary_d &
       ( )
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='clear_dictionary_d' ! 
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       !
       ! [1.1] paketspezifische De-Initialisierungen
       !
       ! Der Speicherplatz der dynamisch allokierten Komponenten-Felder der
       ! SAVE-Variablen <bloecke> wird wieder freigegeben.
       IF ( no_error( ) ) CALL dealloc_diction ( )
       !
       IF ( no_error( ) ) CALL clear_dictionary_all_errors ( ) 
       !
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       !
       prn_lun = c_lun
       trc_lun = c_lun
       !
       ! [1.3] alle weiteren Package-Module de-initialisieren
       !
       ! [1.4] ggf. weitere De-Initialsierungsmethoden rufen
       !
       ! [1.5] Rueck-Setzen des Initialisierungs-Indikators
       !
       initialised = MERGE( .false., .true., no_error( ) )
       !
       !
       ! [1.6] alle mit USE eingebundenen Basis-Module de-initialisieren
       !
       !       Anm.: hier alle Basis-Module de-initialisieren, die in
       !             diesem Paket (in welchem Modul auch immer)
       !             verwendet werden.
       ! [1.6.1] ggf. weitere Module de-initialisieren
       !
       IF ( no_error( ) ) CALL clear_file ( )
       IF ( no_error( ) ) CALL clear_cmds ( )
       IF ( no_error( ) ) CALL clear_time ( )
       IF ( no_error( ) ) CALL clear_datetime ( )
       !
       ! [1.6.2] Error-Modul zuletzt de-initialisieren
       !
       IF ( no_error( ) ) CALL clear_error ( )
       !
    END IF
    !
    ! 2.0 Initialisierungszaehler heruntersetzen
    !
    n_init = n_init - 1
    !
  END SUBROUTINE clear_dictionary_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden
  SUBROUTINE setup_dictionary_prn_lun_d &
       ( lun )
    !
    ! Formalparameter
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='setup_dictionary_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       ! [1.1] "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       !
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_file_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_cmds_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_time_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_datetime_prn_lun ( lun )
       !
       ! [1.2] Setzen der lokalen statischen Daten
       !
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       !
    END IF
    !
  END SUBROUTINE setup_dictionary_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden
  SUBROUTINE setup_dictionary_trc_lun_d &
       ( lun )
    !
    ! Formalparameter
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='setup_dictionary_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       ! [1.1] "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       !
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_file_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_cmds_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_time_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_datetime_trc_lun ( lun )
       !
       ! [1.2] Setzen der lokalen statischen Daten
       !
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       !
    END IF
    !
  END SUBROUTINE setup_dictionary_trc_lun_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucken aller statischen Daten des Packages (ohne Daten der Package-Objekte)
  SUBROUTINE print_dictionary_static_d &
       ( )
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='print_dictionary_static_d' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )    &
           initialised, prn_op, trc_op, prn_lun, trc_lun, n_init
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       !
       IF ( no_error( ) ) CALL print_dictionary_all_errors_d ( )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Packages p_dictionary_ui   ',/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
    '#  initialised = ',L1,/ &
    '#       prn_op = ',L1,/ &
    '#       trc_op = ',L1,/ &
    '#      prn_lun = ',I5,/ &
    '#      trc_lun = ',I5,/ &
    '#       n_init = ',I5,/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
    '#------------------------------------------------------------') 
    !
  END SUBROUTINE print_dictionary_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler des Packages
  SUBROUTINE print_dictionary_all_errors_d &
       ( )
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='print_dictionary_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       IF ( no_error( ) ) CALL print_error( all_errors(:) )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
  END SUBROUTINE print_dictionary_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-TEST-Methoden <<< [ERR_NO = 19000 bis 19999]
  ! ----------------------------------------------------------------------
  !
!  !! f&uuml;hre einen Test der Methoden mit Standarddaten aus
!  SUBROUTINE test_dictionary_d &
!       ( )
!    !
!    ! Lokale Parameter / Variablen
!    !! Name der Subroutine
!    CHARACTER (LEN=31), PARAMETER  :: c_upname='test_dictionary_d' !
!    !! Kanalnummer f&uuml;r Druck-Ausgabe
!    INTEGER           , PARAMETER  :: c_prn_lun=91 ! 
!    !! Kanalnummer f&uuml;r Trace-Ausgabe
!    INTEGER           , PARAMETER  :: c_trc_lun=92 ! 
!    !! Identifikationsnummer (Skalar)
!    INTEGER :: s_id    ! 
!    !! Identifikationsnummern (Vektor)
!    INTEGER :: v_id(3) ! 
!    !! aktuelle Werte f&uuml;r Komponente "<CompConstName1>"
!    <DataType1> :: <CompConstName1>_value(:) ! 
!    !! aktuelle Werte f&uuml;r Komponente "<CompConstName2>"
!    <DataType2> :: <CompConstName2>_value(:) ! 
!    !! aktuelle Werte f&uuml;r Komponente "<CompConstName3>"
!    <DataType3> :: <CompConstName3>_value(:) ! 
!    !
!    IF ( ok_initialised( c_upname ) ) THEN
!       !
!       ! [ 1.0] SETUP-Methoden
!       !
!       WRITE(*,*) ' >>> TEST : Methoden des Packages "p_dictionary_ui"'
!       WRITE(*,*) ' '
!       WRITE(*,*) ' >>> PRINT-Kanalnummer = ',c_prn_lun
!       WRITE(*,*) ' >>> TRACE-Kanalnummer = ',c_trc_lun
!       WRITE(*,*) ' '
!       WRITE(*,*) ' >>> TEST : SETUP-Methoden des Moduls "p_dictionary_ui"'
!       IF ( no_error( ) ) THEN
!          WRITE(*,*) '            setup_dictionary_prn_lun'
!          CALL setup_dictionary_prn_lun ( c_prn_lun )
!          WRITE(*,*) '            OK = ',no_error( )
!       END IF
!       IF ( no_error( ) ) THEN
!          WRITE(*,*) '            setup_dictionary_trc_lun'
!          CALL setup_dictionary_trc_lun ( c_trc_lun )
!          WRITE(*,*) '            OK = ',no_error( )
!       END IF
!       WRITE(*,*) ' ... done '
!       !
!       ! [ 2.0] NEW-Methoden
!       !
!       ! [ 3.0] weitere SETUP-Methoden
!       !
!       !
!       ! [ 4.0] GET-Methoden
!       !
!       !
!       ! [ 5.0] OK-Methoden
!       !
!       !
!       ! [ 6.0] PRINT-Methoden
!       !
!       WRITE(*,*) ' >>> TEST : PRINT-Methoden des Moduls "p_dictionary_ui"'
!       IF ( no_error( ) ) THEN
!          WRITE(*,*) '            print_dictionary_static'
!          CALL print_dictionary_static ( )
!          WRITE(*,*) '            OK = ',no_error( )
!       END IF
!       WRITE(*,*) ' ... done '
!       !
!       ! [ 7.0] KILL-Methoden
!       !
!       !
!       WRITE(*,*) ' <<< TEST : Methoden des Moduls "p_dictionary_ui"'
!       !
!    ELSE
!       !
!       WRITE(*,*) ' >>> kein TEST - Modul "p_dictionary_ui" nicht initialisiert'
!       !
!    END IF
!    !
!  END SUBROUTINE test_dictionary_d
  !
  !! Das Unterprogramm liest und checkt die Eingabedaten
  !! einer Steuerdatei !<BR>
  !! <BR>
  !! Zunaechst wird die zur Datei gehoerige Dictionary-Datei
  !! gelesen und ausgewertet.<BR>
  !! Die Dictionary-Datei enthaelt die Beschreibung der zu
  !! lesenden Datei hinsichtlich ihres Aufbaus, ihrer
  !! Eingabegroessen und der an diese gestellten
  !! Anforderungen.<BR>
  !! <BR>
  !! Die Dictionary-Datei wird stets ueber die Kanalnummer
  !! der Steuerdatei gelesen.<BR>
  !! Eine Dictionary-Datei im lokalen Verzeichnis wird einer
  !! Datei im Standard-Dictionary-Verzeichnis
  !! ( $DICDIR/ ) vorgezogen.<BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_input_file_d &
       ( dictionary_name, input_file )
    !
    ! USE-Statements :
    !
    ! BASIS-Modul fuer das File-Handling
    !
    USE b_file, ONLY :  &
       ! Routinen
       new_file,        &
       open_file,       &
       get_file_unit,   &
       get_file_name,   &
       get_file_path,   &
       set_file_unit,   &
       set_file_access, &
       set_file_status, &
       set_file_action, &
       close_file,      &
       kill_file
    !
    ! Package-Module
    !
    USE m_lesehilfe, ONLY : &
         ! Routinen / Interfaces
         readkarte,         &
         readblockname
    !
    USE m_diction_ein, ONLY : &
         ! Routinen / Interfaces
         read_dictionary,     &
         write_dictionary
    !
    USE m_stringliste , ONLY : &
         ! Routinen / Interfaces
         make_liste
    !
    USE m_stdat_checks, ONLY : &
         ! Routinen / Interfaces
         FixValue_Check,         &
         ReqIfNotIf_Auswertung,  &  
         CheckIfPar_Auswertung,  &
         File_Checks
    !
    ! Formalparameter
    !! Name der Dictionary-Datei <BR>
    !! 1. Wahl : Datei diesen Namens im lokalen Verzeichnis <BR>
    !! sonst   : wird unter $DICDIR gesucht <BR>
    !! Kanalnummer ist stets = lkl der zu lesenden Datei  <BR>
    CHARACTER (LEN=*) , INTENT(IN)    :: dictionary_name
    !! Datei, die gelesen werden soll ! <BR>
    TYPE (t_file)   , INTENT(IN)      :: input_file
    !
    ! Lokale Parameter / Variablen
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='read_input_file_d' ! 
    !
    TYPE (t_file)            :: dicdat
    TYPE (t_file)            :: dat2read
    !
    CHARACTER (LEN=line_len)   :: karte
    CHARACTER (LEN=key_len)    :: blockname
    !
    LOGICAL                    :: doit_file
    LOGICAL                    :: doit_block
    !
    INTEGER                    :: i
    LOGICAL                    :: l_found
    LOGICAL                    :: l_mehrfach=.False.
    !
    CHARACTER (LEN=60)         :: erlaubt
    LOGICAL                    :: l_tolong
    !
    !
    IF ( ok_initialised( c_upname ) ) THEN
       !
       ! Initialisierungen
       !
       NULLIFY (fst_egb)
       NULLIFY (act_egb)
       NULLIFY (lst_egb)
       !
       ! Fehler, wenn Name der Dictionary-Datei zu lang
       ! 
       IF ( LEN_TRIM(dictionary_name) .GT. 80 ) THEN
          !
          CALL setup_error_act ( all_errors(:), -10, c_upname, c_modname )
          CALL setup_error_act ( '<name>', TRIM(dictionary_name) )
          !
          RETURN
          !
       END IF
       !
       ! Welche Datei soll gelesen werden ?
       !
       dat2read = input_file
       !
       ! Name der Input-Datei in die globale Variable infile_name/path eintragen
       !
       infile_name = REPEAT(' ',LEN(infile_name))
       infile_path = REPEAT(' ',LEN(infile_path))
       infile_name = get_file_name ( dat2read )
       infile_path = get_file_path ( dat2read )
       !
       ! t_file fuer Dictionary-Datei zusammenbauen
       !
       IF ( no_error( ) ) CALL new_file ( dicdat )
       IF ( no_error( ) ) CALL set_file_unit ( dicdat, get_file_unit(dat2read) ) ! Kanalnummer der zu lesenden Datei !
       IF ( no_error( ) ) CALL set_file_access ( dicdat, 'SEQUENTIAL' )
       IF ( no_error( ) ) CALL set_file_status ( dicdat, 'OLD' )
       IF ( no_error( ) ) CALL set_file_action ( dicdat, 'READ' )
       !
       ! Name der Dictionary-Datei ermitteln, ggf. mit Pfad $DICDIR
       ! falls die Datei lokal nicht vorhanden ist
       ! 
       IF ( no_error( ) ) CALL getdictionfile ( TRIM(dictionary_name), dicdat )
       !
       ! Name der Dictionary-Datei in die globale Variable dicfile_name eintragen
       !
       dicfile_name = REPEAT(' ',LEN(dicfile_name))
       dicfile_name = get_file_name ( dicdat )
       !
       ! Lesen der programmspezifischen Dictionary-Datei
       !
       IF ( no_error( ) ) CALL read_dictionary ( dicdat, bloecke )
       !
       IF ( no_error( ) ) CALL kill_file ( dicdat )
       !
     ! --> NUR ZU TESTZWECKEN ENTHALTEN !
     ! IF ( no_error( ) ) CALL write_dictionary ( bloecke )
       !
       !
       ! Oeffnen der Datei mit den Eingabesteuerdaten
       !
       IF ( no_error( ) ) CALL open_file ( dat2read )
       !
       ! Lesen der Eingabesteuerdaten 
       !
       IF ( no_error( ) ) THEN
          !
          doit_file  = .True.
          !
          file: DO
             !    
             IF (any_error( )  .OR.  .NOT. doit_file ) EXIT file
             ! 
             doit_block = .true.
             !
             ! Lesen von ENDFILE oder BEGINDATA <blockname>
             !
             CALL readkarte ( dat2read, 'Lesen Steuerdaten BEGINDATA/ENDFILE', karte )
             !       
             IF ( no_error() ) THEN
                !
                karte = ADJUSTL(karte)
                !
                CALL readblockname ( karte, doit_file, blockname )
                !
             END IF
             !       
             ! Lesen der blockspezifischen Daten
             !
             IF ( no_error() .AND. doit_file) THEN
                !
                l_found = .False.
                !
                DO i = 1, SIZE(bloecke) 
                   !
                   IF ( any_error() .OR. l_found) EXIT
                   !
                   IF ( bloecke(i)%Name .EQ. blockname ) THEN
                      !
                      l_found       = .True.
                      !
                      IF ( bloecke(i)%L_Single  .AND. &
                           bloecke(i)%EGBAnz .GT. 0     ) THEN
                         !                     
                         l_mehrfach = .True.
                         !
                      ELSE
                         !
                         ! Gebaeren eines Listenelementes vom Typ <t_vl_block>
                         ! inkl. Initialisierung
                         !
                         IF ( no_error( ) ) CALL birth_vl_block &
                                                  ( act_egb, fst_egb, lst_egb )
                         !  
                         ! Blockinformationen in das Listenelement uebertragen
                         !
                         IF ( no_error( ) ) CALL transfer_einleseinfo &
                                               (bloecke(i), act_egb%block )
                         !
                         IF ( no_error( ) ) THEN
                            !
                            ! Eingabeblock vom Typ bloecke(i) zaehlen
                            !
                            bloecke(i)%EGBAnz = bloecke(i)%EGBAnz + 1
                            !
                            ! Eingabeblock vom Typ bloecke(i) lesen
                            !
                            CALL read_eingabeblock &
                              ( dat2read, act_egb%block, bloecke(i)%EGBAnz )
                            !
                         END IF
                         !
                      END IF
                      !
                   END IF
                   !
                END DO
                !
                ! Fehlerfall : Blockname unbekannt
                !
                IF ( no_error( ) .AND. .NOT. l_found ) THEN
                   !
                   ! "erlaubt"-String basteln   z.B. [L_Opt,ReqIf,NotIf]
                   !
                   erlaubt = REPEAT(' ',LEN(erlaubt))
                   l_tolong = .False.
                   !
                   DO i = 1, SIZE(bloecke)
                      !
                      IF ( any_error() .OR. l_tolong ) EXIT
                      !
                      CALL make_liste &
                         ( TRIM(bloecke(i)%Name),',', erlaubt, l_tolong )
                      !
                   END DO
                   !
                   IF ( no_error( ) ) THEN
                      !
                      IF ( l_tolong  .OR.  &
                           LEN_TRIM(erlaubt) .GT. LEN(erlaubt) - 2 ) THEN
                         !
                         erlaubt = REPEAT(' ', LEN(erlaubt))
                         erlaubt(1:18) = 'siehe Muster-Datei'
                         !
                      ELSE
                         !
                         erlaubt = '['//TRIM(erlaubt)//']'
                         !
                      END IF
                      !
                      CALL setup_error_act ( all_errors(:), -20, c_upname, c_modname )
                      CALL setup_error_act ( '<file_name>', TRIM(infile_name) )
                      CALL setup_error_act ( '<blockname>', TRIM(blockname) )
                      CALL setup_error_act ( '<erlaubt>', TRIM(erlaubt) )
                      !
                      RETURN
                      !
                   END IF
                   !
                END IF ! .NOT. l_found
                !
                ! Fehlerfall : doppelter Block in EingabeDatei
                !
                IF ( no_error( ) .AND. l_mehrfach ) THEN
                   !
                   CALL setup_error_act ( all_errors(:), -30, c_upname, c_modname )
                   CALL setup_error_act ( '<file_name>', TRIM(infile_name) )
                   CALL setup_error_act ( '<blockname>', TRIM(blockname) )
                   !
                   RETURN
                   !
                END IF
                !
             END IF      ! doit_file = true 
             !
          END DO file
          !
       END IF ! no_error
       !
       ! Eingabewerte der verketteten Liste vom Typ <t_vl_block> werden nun in das
       ! Feld bloecke vom Typ <t_block> uebertragen
       !
       IF ( no_error( ) ) CALL transfer_EGB_werte ( )
       !
       ! Check, ob alle required Bloecke in der Eingabsteuer-Datei vorhanden sind
       !
       IF ( no_error( ) ) THEN
          !
          DO i = 1, SIZE(bloecke)
             !
             IF ( .NOT. bloecke(i)%L_Opt  .AND. &
                   bloecke(i)%EGBAnz  .EQ. 0      ) THEN
                !
                CALL setup_error_act ( all_errors(:), -40, c_upname, c_modname )
                CALL setup_error_act ( '<file_name>', TRIM(infile_name) )
                CALL setup_error_act ( '<blockname>', TRIM(bloecke(i)%Name) )
                RETURN
                !
             END IF
             !
          END DO
          !
       END IF
       !
       !   Checken, ob -falls FixValues vorhanden- die Eingabewerte zu den
       !   erlaubten Werten gehoeren
       !
       IF ( no_error( ) ) THEN
          !
          DO i = 1, SIZE(bloecke)
             !
             IF ( no_error( ) ) CALL FixValue_Check ( bloecke(i) )
             !  
          END DO
          !
       END IF
       !
       !
       ! ReqIf & NotIf - Bedingungen fuer Block, Key und Parameter ueberpruefen
       !
       IF ( no_error( ) ) CALL ReqIfNotIf_Auswertung ( bloecke )
       !
       ! CheckIfPar-Checks fuer die Parameter durchfuehren
       !
       IF ( no_error( ) ) CALL CheckIfPar_Auswertung ( bloecke )
       !
       ! Ggf: FILE-Checks
       !
       IF ( no_error( ) ) CALL File_Checks ( bloecke )
       !
       ! Schliessen der Datei mit den Eingabesteuerdaten
       !
       IF ( no_error( ) ) CALL close_file ( dat2read )
       !
    END IF ! ok_initialised
    !
  END SUBROUTINE read_input_file_d
  !
  !! Liefert Anzahl an Eingabebloecken des Typs mit Namen <block_name>
  !! in der Steuerdatei. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE get_nof_input_blocks_d &
       ( block_name, nof_blocks )
    !
    ! Formalparameter
    !! Name des Eingabe-Block-Types
    CHARACTER (LEN=*) , INTENT(IN)  :: block_name !
    !! Anzahl an Eingabebloecken mit Namen <block_name> in der Eingabesteuerdatei
    INTEGER           , INTENT(OUT) :: nof_blocks
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31) , PARAMETER :: c_upname='get_nof_input_blocks_d' ! 
    !
    INTEGER  :: i_bl
    INTEGER  :: i_key
    !
    IF ( ok_initialised( c_upname ) ) THEN
       !
       ! Adressnummer fuer Block X ermitteln
       !
       IF ( no_error( ) ) CALL get_adresse &
                          ( block_name, 'dummy', 0, 'BLOCK', c_upname, &
                            i_bl, i_key )
       ! 
       ! Eingabeblock-Anzahl ermitteln
       !
       IF ( no_error( ) ) nof_blocks = bloecke(i_bl)%EGBAnz
       !
    END IF
    !
  END SUBROUTINE get_nof_input_blocks_d
  !
  !
  !! Liefert Anzahl an Schluesselwortzeilen mit dem
  !! Keywort <keyword> im <block_no>'ten Eingabeblock mit
  !! Namen <block_name>. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE get_nof_input_lines_d &
       ( block_name, block_no, keyword, nof_lines )
    !
    ! Formalparameter
    !! Name des Eingabe-Block-Types
    CHARACTER (LEN=*)  , INTENT(IN   )      :: block_name
    !! Nummer des Eingabeblockes vom Typ <block_name>
    INTEGER            , INTENT(IN   )      :: block_no
    !! Schluesselwort
    CHARACTER (LEN=*)  , INTENT(IN   )      :: keyword
    !! Anzahl an Eingabezeilen mit dem Schluesselwort <keyword>
    !! in Block <block_name> der Eingabesteuerdatei
    INTEGER            , INTENT(  OUT)      :: nof_lines
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_nof_input_lines_d'
    !
    INTEGER  :: i_bl
    INTEGER  :: i_key
    !
    ! Adressnummern fuer Key X in Block Y ermitteln
    !
    IF ( no_error( ) ) CALL get_adresse &
                             ( block_name, keyword, 0, 'KEY', c_upname, &
                               i_bl, i_key )
    !
    ! block_no pruefen
    !
    IF ( no_error( ) ) CALL pruefe_egb_nr &
                             ( i_bl, block_no, c_upname )
    !
    ! Zeilenanzahl ermitteln
    !
    IF ( no_error( ) ) nof_lines = bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no)
    !
  END SUBROUTINE get_nof_input_lines_d
  !
  !! Liefert den adressierten Parameter-INTEGER-Wert
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE get_input_data_int_value &
       ( block_name, block_no, keyword, line_no, par_no, &
         int_value, l_exist )
    !
    ! Formalparameter
    !! Name des Eingabeblockes
    CHARACTER (LEN=*)  , INTENT(IN   )      :: block_name
    !! Nummer des Eingabeblockes
    INTEGER            , INTENT(IN   )      :: block_no
    !! Schluesselwort
    CHARACTER (LEN=*)  , INTENT(IN   )      :: keyword
    !! Nummer der Eingabezeile
    !! [ 0 bedeutet gleichzeitiger Zugriff auf alle Zeilen ]
    INTEGER            , INTENT(IN   )      :: line_no
    !! Positionsnummer des Parameters
    INTEGER            , INTENT(IN   )      :: par_no
    !! Rueckgabe-Parameter fuer Integer-Eingabewert
    INTEGER            , INTENT(  OUT)      :: int_value
    !! True , wenn fuer Parameter ein Wert existiert
    !! False, wenn kein Wert fuer Parameter exitiert, z.B.
    !!   - Block in Eingabedatei nicht vorhanden
    !!   - Keyzeile in Block nicht angegeben
    !!   - Parameter in Keyzeile nicht spezifiziert
    !! Dieser UP-Parameter muss stets vorhanden sein, wenn
    !! eines der Adress-Elemente (Block,Key,Parameter) als
    !! optional vereinbart wurde.
    LOGICAL , OPTIONAL , INTENT(  OUT)      :: l_exist
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_input_data_int_value'
    !
    INTEGER  :: i_bl
    INTEGER  :: i_key
    INTEGER  :: z_nr
    !
    ! Adressnummern ermitteln und Aufruf der get_input_data-Methode checken
    !
    IF ( no_error( ) ) CALL get_par_general &
                             ( block_name, block_no, keyword, par_no, line_no, 'INTEGER', &
                               .False., PRESENT(l_exist), i_bl, i_key )
    !
    ! Eingabegroesse aus der Variablen vom Typ <t_block> in den Rueckgabeparameter stopfen
    !
    IF ( no_error( ) ) THEN
       !
       IF ( PRESENT(l_exist) ) l_exist = .False.
       !
       IF ( bloecke(i_bl)%EGBAnz .GT. 0 ) THEN
          !      
          IF ( bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no) .GT. 0 ) THEN
             !
             IF ( line_no .GT. 0 ) THEN
                z_nr = line_no
             ELSE
                z_nr = 1
             END IF
             !
             IF (ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%Int)) THEN
                !
                int_value = bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%Int(1)
                !
                IF ( PRESENT(l_exist) ) l_exist = .True.
                !
             END IF
             !
          END IF
          !
       END IF
       !
    END IF ! no_error( )
    !
  END SUBROUTINE get_input_data_int_value
  !
  !! Liefert den adressierten Parameter-REAL-Wert
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE get_input_data_real_value &
       ( block_name, block_no, keyword, line_no, par_no, real_value, l_exist )
    !
    ! Formalparameter
    !! Name des Eingabeblockes
    CHARACTER (LEN=*)  , INTENT(IN   )      :: block_name
    !! Nummer des Eingabeblockes
    INTEGER            , INTENT(IN   )      :: block_no
    !! Schluesselwort
    CHARACTER (LEN=*)  , INTENT(IN   )      :: keyword
    !! Nummer der Eingabezeile <BR>
    !! [ 0 bedeutet gleichzeitiger Zugriff auf alle Zeilen ]
    INTEGER            , INTENT(IN   )      :: line_no
    !! Positionsnummer des Parameters
    INTEGER            , INTENT(IN   )      :: par_no
    !! Rueckgabe-Parameter fuer Real-Eingabewert
    REAL (KIND=Single) , INTENT(  OUT)      :: real_value
    !! True , wenn fuer Parameter ein Wert existiert
    !! False, wenn kein Wert fuer Parameter exitiert, z.B.
    !!   - Block in Eingabedatei nicht vorhanden
    !!   - Keyzeile in Block nicht angegeben
    !!   - Parameter in Keyzeile nicht spezifiziert
    !! Dieser UP-Parameter muss stets vorhanden sein, wenn
    !! eines der Adress-Elemente (Block,Key,Parameter) als
    !! optional vereinbart wurde.
    LOGICAL , OPTIONAL , INTENT(  OUT)      :: l_exist
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_input_data_real_value'
    !
    INTEGER   :: i_bl, i_key, z_nr
    !
    ! Adressnummern ermitteln und Aufruf der get_input_data-Methode checken
    !
    IF ( no_error( ) ) CALL get_par_general &
                             ( block_name, block_no, keyword, par_no, line_no, 'REAL', &
                               .False., PRESENT(l_exist), i_bl, i_key )
    !
    IF ( no_error( ) ) THEN
       !
       IF ( PRESENT(l_exist) ) l_exist = .False.
       !
       IF ( bloecke(i_bl)%EGBAnz .GT. 0 ) THEN

          IF ( bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no) .GT. 0 ) THEN

             IF ( line_no .GT. 0 ) THEN
                z_nr = line_no
             ELSE
                z_nr = 1
             END IF

             IF (ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%real)) THEN

                real_value = bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%real(1)

                IF ( PRESENT(l_exist) ) l_exist = .True.

             END IF

          END IF

       END IF
       !
    END IF ! no_error( )
    !
  END SUBROUTINE get_input_data_real_value
  !
  !! Liefert den adressierten Parameter-REAL(Double)-Wert
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE get_input_data_doub_value &
       ( block_name, block_no, keyword, line_no, par_no, doub_value, l_exist )
    !
    ! Formalparameter
    !!
    !! Name des Eingabeblockes
    CHARACTER (LEN=*)  , INTENT(IN   )      :: block_name
    !! Nummer des Eingabeblockes
    INTEGER            , INTENT(IN   )      :: block_no
    !! Schluesselwort
    CHARACTER (LEN=*)  , INTENT(IN   )      :: keyword
    !! Nummer der Eingabezeile <BR>
    !! [ 0 bedeutet gleichzeitiger Zugriff auf alle Zeilen ]
    INTEGER            , INTENT(IN   )      :: line_no
    !! Positionsnummer des Parameters
    INTEGER            , INTENT(IN   )      :: par_no
    !
    !! Rueckgabe-Parameter fuer Real (KIND=Double)-Eingabewert
    REAL (KIND=Double) , INTENT(  OUT)      :: doub_value
    !! True , wenn fuer Parameter ein Wert existiert
    !! False, wenn kein Wert fuer Parameter exitiert, z.B.
    !!   - Block in Eingabedatei nicht vorhanden
    !!   - Keyzeile in Block nicht angegeben
    !!   - Parameter in Keyzeile nicht spezifiziert
    !! Dieser UP-Parameter muss stets vorhanden sein, wenn
    !! eines der Adress-Elemente (Block,Key,Parameter) als
    !! optional vereinbart wurde.
    LOGICAL , OPTIONAL , INTENT(  OUT)      :: l_exist
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_input_data_doub_value'
    !
    INTEGER   :: i_bl, i_key, z_nr
    !
    ! Adressnummern ermitteln und Aufruf der get_input_data-Methode checken
    !
    IF ( no_error( ) ) CALL get_par_general &
                             ( block_name, block_no, keyword, par_no, line_no, 'DOUBLE', &
                               .False., PRESENT(l_exist), i_bl, i_key )
    !
    ! Eingabegroesse aus der Variablen vom Typ <t_block> in den Rueckgabeparameter stopfen
    !
    IF ( no_error( ) ) THEN
       !
       IF ( PRESENT(l_exist) ) l_exist = .False.

       IF ( bloecke(i_bl)%EGBAnz .GT. 0 ) THEN

          IF ( bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no) .GT. 0 ) THEN

             IF ( line_no .GT. 0 ) THEN
                z_nr = line_no
             ELSE
                z_nr = 1
             END IF

             IF (ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%doub)) THEN

                doub_value = bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%doub(1)

                IF ( PRESENT(l_exist) ) l_exist = .True.

             ! wenn Maschine=Cray, werden auch die normalen Reals ueber dieses UP rausgegeben
             ELSE IF (ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%real)) THEN

                doub_value = bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%real(1)

                IF ( PRESENT(l_exist) ) l_exist = .True.

             END IF

          END IF

       END IF

    END IF ! no_error( )
    !
  END SUBROUTINE get_input_data_doub_value
  !
  !! Liefert den adressierten Parameter-LOGICAL-Wert
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE get_input_data_log_value &
       ( block_name, block_no, keyword, line_no, par_no, &
         log_value, l_exist )
    !
    ! Formalparameter
    !! Name des Eingabeblockes
    CHARACTER (LEN=*)  , INTENT(IN   )      :: block_name
    !! Nummer des Eingabeblockes
    INTEGER            , INTENT(IN   )      :: block_no
    !! Schluesselwort
    CHARACTER (LEN=*)  , INTENT(IN   )      :: keyword
    !! Nummer der Eingabezeile <BR>
    !! [ 0 bedeutet gleichzeitiger Zugriff auf alle Zeilen ]
    INTEGER            , INTENT(IN   )      :: line_no
    !! Positionsnummer des Parameters
    INTEGER            , INTENT(IN   )      :: par_no
    !
    !! Rueckgabe-Parameter fuer Logical-Eingabewert
    LOGICAL            , INTENT(  OUT)      :: log_value
    !! True , wenn fuer Parameter ein Wert existiert
    !! False, wenn kein Wert fuer Parameter exitiert, z.B.
    !!   - Block in Eingabedatei nicht vorhanden
    !!   - Keyzeile in Block nicht angegeben
    !!   - Parameter in Keyzeile nicht spezifiziert
    !! Dieser UP-Parameter muss stets vorhanden sein, wenn
    !! eines der Adress-Elemente (Block,Key,Parameter) als
    !! optional vereinbart wurde.
    LOGICAL , OPTIONAL , INTENT(  OUT)      :: l_exist
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_input_data_log_value'
    !
    INTEGER :: i_bl, i_key, z_nr
    !
    ! Adressnummern ermitteln und Aufruf der get_input_data-Methode checken
    !
    IF ( no_error( ) ) CALL get_par_general &
                             ( block_name, block_no, keyword, par_no, line_no, 'LOGICAL', &
                               .False., PRESENT(l_exist), i_bl, i_key )
    !
    ! Eingabegroesse aus der Variablen vom Typ <t_block> in den Rueckgabeparameter stopfen
    ! 
    IF ( no_error( ) ) THEN
       !
       IF ( PRESENT(l_exist) ) l_exist = .False.

       IF ( bloecke(i_bl)%EGBAnz .GT. 0 ) THEN

          IF ( bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no) .GT. 0 ) THEN

             IF ( line_no .GT. 0 ) THEN
                z_nr = line_no
             ELSE
                z_nr = 1
             END IF

             IF (ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%log)) THEN

                log_value = bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%log(1)

                IF ( PRESENT(l_exist) ) l_exist = .True.

             END IF

          END IF

       END IF

    END IF ! no_error( )
    !
  END SUBROUTINE get_input_data_log_value
  !
  !! Liefert den adressierten Parameter-CHARACTER-Wert
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE get_input_data_char_value &
       ( block_name, block_no, keyword, line_no, par_no, &
         char_value, l_exist )
    !
    ! Formalparameter
    !! Name des Eingabeblockes
    CHARACTER (LEN=*)  , INTENT(IN   )      :: block_name
    !! Nummer des Eingabeblockes
    INTEGER            , INTENT(IN   )      :: block_no
    !! Schluesselwort
    CHARACTER (LEN=*)  , INTENT(IN   )      :: keyword
    !! Nummer der Eingabezeile <BR>
    !! [ 0 bedeutet gleichzeitiger Zugriff auf alle Zeilen ]
    INTEGER            , INTENT(IN   )      :: line_no
    !! Positionsnummer des Parameters
    INTEGER            , INTENT(IN   )      :: par_no
    !
    !! Rueckgabe-Parameter fuer Character-Eingabewert
    CHARACTER (LEN=*)  , INTENT(  OUT)      :: char_value
    !! True , wenn fuer Parameter ein Wert existiert
    !! False, wenn kein Wert fuer Parameter exitiert, z.B.
    !!   - Block in Eingabedatei nicht vorhanden
    !!   - Keyzeile in Block nicht angegeben
    !!   - Parameter in Keyzeile nicht spezifiziert
    !! Dieser UP-Parameter muss stets vorhanden sein, wenn
    !! eines der Adress-Elemente (Block,Key,Parameter) als
    !! optional vereinbart wurde.
    LOGICAL , OPTIONAL , INTENT(  OUT)      :: l_exist
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_input_data_char_value'
    !! Hilfsstrings fuer Fehlermeldung
    CHARACTER (LEN=6)             :: c_actlen, c_reqlen
    !
    INTEGER  :: i_bl, i_key, z_nr
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       ! Adressnummern ermitteln und Aufruf der get_input_data-Methode checken
       !
       IF ( no_error( ) ) CALL get_par_general &
                                ( block_name, block_no, keyword, par_no, line_no, 'CHARACTER', &
                                  .False., PRESENT(l_exist), i_bl, i_key )
       !
       ! Eingabegroesse aus der Variablen vom Typ <t_block> in den Rueckgabeparameter stopfen
       !
       IF ( no_error( ) ) THEN
          !
          IF ( PRESENT(l_exist) ) l_exist = .False.

          IF ( bloecke(i_bl)%EGBAnz .GT. 0 ) THEN

             IF ( bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no) .GT. 0 ) THEN

                IF ( line_no .GT. 0 ) THEN
                   z_nr = line_no
                ELSE
                   z_nr = 1
                END IF

                IF (ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%char)) THEN

                   IF ( LEN_TRIM(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%char(1)) &
                        .GT.  LEN(char_value)                         ) THEN
                      !
                      ! Fehler -50 : Stringlaenge des Rueckgabeparameters <char_value> zu kurz !
                      !
                      c_actlen = REPEAT(' ',LEN(c_actlen))
                      c_reqlen = REPEAT(' ',LEN(c_reqlen))
                      !
                      WRITE(c_actlen,'(I5)') LEN(char_value)
                      WRITE(c_reqlen,'(I5)') &
                            LEN_TRIM(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%char(1))
                      !
                      CALL setup_error_act ( all_errors(:), -50, c_upname, c_modname )
                      CALL setup_error_act ( '<act_len>', TRIM(c_actlen) )
                      CALL setup_error_act ( '<req_len>', TRIM(c_reqlen) )
                      !
                      RETURN
                      !
                   ELSE

                      char_value=bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%char(1)

                      IF ( PRESENT(l_exist) ) l_exist = .True.

                   END IF

                END IF

             END IF

          END IF

       END IF ! no_error( )
       !
    END IF ! ok_initialised
    !
  END SUBROUTINE get_input_data_char_value
  !
  !! Liefert das adressierte Parameter-INTEGER-Feld
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE get_input_data_int_array &
       ( block_name, block_no, keyword, line_no, par_no, &
       int_array, l_exist )
    !
    ! Formalparameter
    !! Name des Eingabeblockes
    CHARACTER (LEN=*)      , INTENT(IN   )      :: block_name
    !! Nummer des Eingabeblockes
    INTEGER                , INTENT(IN   )      :: block_no
    !! Schluesselwort
    CHARACTER (LEN=*)      , INTENT(IN   )      :: keyword
    !! Nummer der Eingabezeile <BR>
    !! [ 0 bedeutet gleichzeitiger Zugriff auf alle Zeilen ]
    INTEGER                , INTENT(IN   )      :: line_no
    !! Positionsnummer des Parameters
    INTEGER                , INTENT(IN   )      :: par_no
    !
    !! Rueckgabe-Parameter fuer Integer-Eingabewert
    INTEGER , DIMENSION(:) , POINTER            :: int_array
    !! True , wenn fuer Parameter ein Wert existiert
    !! False, wenn kein Wert fuer Parameter exitiert, z.B.
    !!   - Block in Eingabedatei nicht vorhanden
    !!   - Keyzeile in Block nicht angegeben
    !!   - Parameter in Keyzeile nicht spezifiziert
    !! Dieser UP-Parameter muss stets vorhanden sein, wenn
    !! eines der Adress-Elemente (Block,Key,Parameter) als
    !! optional vereinbart wurde.
    LOGICAL , OPTIONAL     , INTENT(  OUT)      :: l_exist
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_input_data_int_array'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    INTEGER  :: i, j, k
    INTEGER  :: i_bl, i_key, i_size, z_nr
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       ! Pruefen, ob das Rueckgabefeld nicht bereits associated ist
       !
       IF ( ASSOCIATED(int_array) ) THEN
          !
          CALL setup_error_act ( all_errors(:), -60, c_upname, c_modname )
          !
          RETURN
          !       
       END IF
       !
       !   Adressnummern ermitteln und Aufruf der get_input_data-Methode checken
       !
       IF ( no_error( ) ) CALL get_par_general &
                                ( block_name, block_no, keyword, par_no, line_no, 'INTEGER', &
                                  .True., PRESENT(l_exist), i_bl, i_key )
       !
       ! Eingabegroesse aus der Variablen vom Typ <t_block> in den
       ! Rueckgabeparameter stopfen
       !
       IF ( no_error( ) ) THEN
          !
          IF ( PRESENT(l_exist) ) l_exist = .False.
          !
          IF ( bloecke(i_bl)%EGBAnz .GT. 0 ) THEN

             IF ( bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no) .GT. 0 ) THEN

                IF ( bloecke(i_bl)%Key(i_key)%L_Single  .OR. &
                     line_no .GT. 0 ) THEN

                   z_nr = line_no
                   IF ( line_no .EQ. 0 ) z_nr = 1

                   ! Fall : Daten aus einzelner Zeile

                   IF (ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%Int)) THEN

                      i_size = SIZE(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%Int)
                      !
                      ALLOCATE ( int_array(i_size), STAT=stat )
                      !
                      IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
                         !
                         CALL setup_error_act &
                               ( all_errors(:), -10000, c_upname, c_modname, stat )
                         CALL setup_error_act ( '<felder>', 'int_array' )
                         !
                         RETURN
                         !
                      END IF
                      ! 
                      DO i = 1, SIZE (int_array)

                         int_array(i) = bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%Int(i)

                      END DO

                      IF ( PRESENT(l_exist) ) l_exist = .True.

                   END IF

                ELSE

                   i_size = 0

                   DO i = 1, bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no)

                      i_size = i_size + SIZE(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(i)%Int)

                   END DO
                   !
                   ALLOCATE ( int_array(i_size), STAT=stat )
                   !
                   IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
                      !
                      CALL setup_error_act &
                            ( all_errors(:), -10000, c_upname, c_modname, stat )
                      CALL setup_error_act ( '<felder>', 'int_array' )
                      !
                      RETURN
                      !
                   END IF
                   !
                   k = 0

                   DO i = 1, bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no)

                      DO j = 1, SIZE(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(i)%Int)

                         k = k + 1

                         int_array(k) = bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(i)%Int(j)

                      END DO

                   END DO

                   IF ( PRESENT(l_exist) ) l_exist = .True.

             
                END IF   ! FU : Einzelzeile | verschiedene Eingabezeilen

             END IF   ! (bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no) .GT. 0)

          END IF   ! (bloecke(i_bl)%EGBAnz .GT. 0)
          !
       END IF ! no_error( )   
       !
    END IF ! ok_initialised
    !
  END SUBROUTINE get_input_data_int_array
  !
  !! Liefert das adressierte Parameter-REAL-Feld
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE get_input_data_real_array &
       ( block_name, block_no, keyword, line_no, par_no, &
       real_array, l_exist )
    !
    ! Formalparameter
    !! Name des Eingabeblockes
    CHARACTER (LEN=*)      , INTENT(IN   )         :: block_name
    !! Nummer des Eingabeblockes
    INTEGER                , INTENT(IN   )         :: block_no
    !! Schluesselwort
    CHARACTER (LEN=*)      , INTENT(IN   )         :: keyword
    !! Nummer der Eingabezeile <BR>
    !! [ 0 bedeutet gleichzeitiger Zugriff auf alle Zeilen ]
    INTEGER                , INTENT(IN   )         :: line_no
    !! Positionsnummer des Parameters
    INTEGER                , INTENT(IN   )         :: par_no
    !! Rueckgabe-Parameter fuer Real-Eingabewerte
    REAL (KIND=Single)  , DIMENSION(:) , POINTER   :: real_array
    !! True , wenn fuer Parameter ein Wert existiert
    !! False, wenn kein Wert fuer Parameter exitiert, z.B.
    !!   - Block in Eingabedatei nicht vorhanden
    !!   - Keyzeile in Block nicht angegeben
    !!   - Parameter in Keyzeile nicht spezifiziert
    !! Dieser UP-Parameter muss stets vorhanden sein, wenn
    !! eines der Adress-Elemente (Block,Key,Parameter) als
    !! optional vereinbart wurde.
    LOGICAL , OPTIONAL     , INTENT(  OUT)         :: l_exist
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_input_data_real_array'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    INTEGER  :: i, j, k
    INTEGER  :: i_bl, i_key, i_size, z_nr
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       ! Pruefen, ob das Rueckgabefeld nicht bereits associated ist
       !
       IF ( ASSOCIATED(real_array) ) THEN
          !
          CALL setup_error_act ( all_errors(:), -60, c_upname, c_modname )
          !
          RETURN
          !       
       END IF
       !
       ! Adressnummern ermitteln und Aufruf der get_input_data-Methode checken
       !
       IF ( no_error( ) ) CALL get_par_general &
                                ( block_name, block_no, keyword, par_no, line_no, 'REAL', &
                                  .True., PRESENT(l_exist), i_bl, i_key )
       !
       ! Eingabegroesse aus der Variablen vom Typ <t_block> in den
       ! Rueckgabeparameter stopfen
       !
       IF ( no_error( ) ) THEN
          !
          IF ( PRESENT(l_exist) ) l_exist = .False.

          IF ( bloecke(i_bl)%EGBAnz .GT. 0 ) THEN

             IF ( bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no) .GT. 0 ) THEN

                IF ( bloecke(i_bl)%Key(i_key)%L_Single  .OR. &
                     line_no .GT. 0 ) THEN

                   z_nr = line_no
                   IF ( line_no .EQ. 0 ) z_nr = 1

                   ! Fall : Daten aus einzelner Zeile

                   IF (ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%real)) THEN

                      i_size = SIZE(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%real)
                      !
                      ALLOCATE ( real_array(i_size), STAT=stat )
                      !
                      IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
                         !
                         CALL setup_error_act &
                               ( all_errors(:), -10000, c_upname, c_modname, stat )
                         CALL setup_error_act ( '<felder>', 'real_array' )
                         !
                         RETURN
                         !
                      END IF
                      !
                      DO i = 1, SIZE (real_array)

                         real_array(i) = &
                              bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%real(i)

                      END DO

                      IF ( PRESENT(l_exist) ) l_exist = .True.

                   END IF

                ELSE

                   ! Fall : Daten ggf. aus mehreren Eingabezeilen

                   i_size = 0

                   DO i = 1, bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no)

                      i_size = i_size + SIZE(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(i)%real)

                   END DO

                   !             
                   ALLOCATE ( real_array(i_size), STAT=stat )
                   !
                   IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
                      !
                      CALL setup_error_act &
                            ( all_errors(:), -10000, c_upname, c_modname, stat )
                      CALL setup_error_act ( '<felder>', 'real_array' )
                      !
                      RETURN
                      !
                   END IF
                   !
                   k = 0

                   DO i = 1, bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no)

                      DO j = 1, SIZE(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(i)%real)

                         k = k + 1

                         real_array(k) = &
                              bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(i)%real(j)
                   
                      END DO
      
                   END DO

                   IF ( PRESENT(l_exist) ) l_exist = .True.


                END IF   ! FU : Einzelzeile | verschiedene Eingabezeilen

             END IF   ! (bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no) .GT. 0)

          END IF   ! (bloecke(i_bl)%EGBAnz .GT. 0 )
          !
       END IF ! no_error( )   
       !
    END IF ! ok_initialised
    !
  END SUBROUTINE get_input_data_real_array
  !
  !! Liefert das adressierte Parameter-REAL(Double)-Feld
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE get_input_data_doub_array &
       ( block_name, block_no, keyword, line_no, par_no, &
       doub_array, l_exist )
    !
    ! Formalparameter
    !! Name des Eingabeblockes
    CHARACTER (LEN=*)      , INTENT(IN   )          :: block_name
    !! Nummer des Eingabeblockes
    INTEGER                , INTENT(IN   )          :: block_no
    !! Schluesselwort
    CHARACTER (LEN=*)      , INTENT(IN   )          :: keyword
    !! Nummer der Eingabezeile <BR>
    !! [ 0 bedeutet gleichzeitiger Zugriff auf alle Zeilen ]
    INTEGER                , INTENT(IN   )          :: line_no
    !! Positionsnummer des Parameters
    INTEGER                , INTENT(IN   )          :: par_no
    !! Rueckgabe-Parameter fuer Real (KIND=Double)-Eingabewerte
    REAL (KIND=Double) , DIMENSION(:) , POINTER     :: doub_array
    !! True , wenn fuer Parameter ein Wert existiert
    !! False, wenn kein Wert fuer Parameter exitiert, z.B.
    !!   - Block in Eingabedatei nicht vorhanden
    !!   - Keyzeile in Block nicht angegeben
    !!   - Parameter in Keyzeile nicht spezifiziert
    !! Dieser UP-Parameter muss stets vorhanden sein, wenn
    !! eines der Adress-Elemente (Block,Key,Parameter) als
    !! optional vereinbart wurde.
    LOGICAL , OPTIONAL     , INTENT(  OUT)          :: l_exist
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_input_data_doub_array'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    INTEGER                            :: i, j, k
    INTEGER                            :: i_bl, i_key, i_size, z_nr
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       ! Pruefen, ob das Rueckgabefeld nicht bereits associated ist
       !
       IF ( ASSOCIATED(doub_array) ) THEN
          !
          CALL setup_error_act ( all_errors(:), -60, c_upname, c_modname )
          !
          RETURN
          !       
       END IF
       !
       !   Adressnummern ermitteln und Aufruf der get_input_data-Methode checken
       !
       IF ( no_error( ) ) CALL get_par_general &
                                ( block_name, block_no, keyword, par_no, line_no, 'DOUBLE', &
                                  .True., PRESENT(l_exist), i_bl, i_key )
       !
       ! Eingabegroesse aus der Variablen vom Typ <t_block> in den
       ! Rueckgabeparameter stopfen
       ! 
       IF ( no_error( ) ) THEN
          !
          IF ( PRESENT(l_exist) ) l_exist = .False.

          IF ( bloecke(i_bl)%EGBAnz .GT. 0 ) THEN

             IF ( bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no) .GT. 0 ) THEN

                IF ( bloecke(i_bl)%Key(i_key)%L_Single  .OR. &
                     line_no .GT. 0 ) THEN

                   z_nr = line_no
                   IF ( line_no .EQ. 0 ) z_nr = 1

                   ! Fall : Daten aus einzelner Zeile

                   IF (ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%doub)) THEN

                      i_size = SIZE(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%doub)
                      !
                      ALLOCATE ( doub_array(i_size), STAT=stat )
                      !
                      IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
                         !
                         CALL setup_error_act &
                               ( all_errors(:), -10000, c_upname, c_modname, stat )
                         CALL setup_error_act ( '<felder>', 'doub_array' )
                         !
                         RETURN
                         !
                      END IF
                      ! 
                      DO i = 1, SIZE (doub_array)

                         doub_array(i) = &
                              bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%doub(i)

                      END DO

                      IF ( PRESENT(l_exist) ) l_exist = .True.


                   ! wenn Maschine=Cray, werden auch die normalen Reals ueber dieses UP rausgegeben
                   ELSE IF &
                       (ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%real)) THEN

                      i_size = SIZE(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%real)
                      !
                      ALLOCATE ( doub_array(i_size), STAT=stat )
                      !
                      IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
                         !
                         CALL setup_error_act &
                               ( all_errors(:), -10000, c_upname, c_modname, stat )
                         CALL setup_error_act ( '<felder>', 'doub_array' )
                         !
                         RETURN
                         !
                      END IF
                      ! 
                      DO i = 1, SIZE (doub_array)

                         doub_array(i) = &
                              bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%real(i)

                      END DO

                      IF ( PRESENT(l_exist) ) l_exist = .True.

                   END IF


                ELSE


                   ! Fall : Daten ggf. aus mehreren Eingabezeilen
      
                   IF ( bloecke(i_bl)%Key(i_key)%Par(par_no)%Type .EQ. 'DOUBLE' ) THEN

                      i_size = 0
                      DO i = 1, bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no)

                         i_size = i_size + &
                                  SIZE(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(i)%doub)
                      END DO

                   ELSE IF ( bloecke(i_bl)%Key(i_key)%Par(par_no)%Type .EQ. 'REAL' ) THEN

                      i_size = 0
                      DO i = 1, bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no)

                         i_size = i_size + &
                                  SIZE(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(i)%real)
                      END DO

                   END IF
                   !
                   ALLOCATE ( doub_array(i_size), STAT=stat )
                   !   
                   IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
                      !
                      CALL setup_error_act &
                            ( all_errors(:), -10000, c_upname, c_modname, stat )
                      CALL setup_error_act ( '<felder>', 'doub_array' )
                      !
                      RETURN
                      !
                   END IF
                   ! 
                   IF ( bloecke(i_bl)%Key(i_key)%Par(par_no)%Type .EQ. 'DOUBLE' ) THEN

                      k = 0

                      DO i = 1, bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no)

                         DO j = 1, SIZE(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(i)%doub)

                            k = k + 1

                            doub_array(k) = &
                                 bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(i)%doub(j)
                   
                         END DO
      
                      END DO

                      IF ( PRESENT(l_exist) ) l_exist = .True.

                   ! wenn Maschine=Cray, werden auch die normalen Reals ueber dieses UP rausgegeben
                   ELSE IF ( bloecke(i_bl)%Key(i_key)%Par(par_no)%Type .EQ. 'REAL' ) THEN

                      k = 0

                      DO i = 1, bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no)

                         DO j = 1, SIZE(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(i)%real)

                            k = k + 1

                            doub_array(k) = &
                                 bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(i)%real(j)
                   
                         END DO
            
                      END DO

                      IF ( PRESENT(l_exist) ) l_exist = .True.

                   END IF


                END IF   ! FU : Einzelzeile | verschiedene Eingabezeilen

             END IF   ! (bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no) .GT. 0)

          END IF   ! (bloecke(i_bl)%EGBAnz .GT. 0 )
          !
       END IF ! no_error( )   
       !
    END IF ! ok_initialised
    !
  END SUBROUTINE get_input_data_doub_array
  !
  !! Liefert das adressierte Parameter-LOGICAL-Feld
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE get_input_data_log_array &
       ( block_name, block_no, keyword, line_no, par_no, &
       log_array, l_exist )
    !
    ! Formalparameter
    !! Name des Eingabeblockes
    CHARACTER (LEN=*)      , INTENT(IN   )      :: block_name
    !! Nummer des Eingabeblockes
    INTEGER                , INTENT(IN   )      :: block_no
    !! Schluesselwort
    CHARACTER (LEN=*)      , INTENT(IN   )      :: keyword
    !! Nummer der Eingabezeile <BR>
    !! [ 0 bedeutet gleichzeitiger Zugriff auf alle Zeilen ]
    INTEGER                , INTENT(IN   )      :: line_no
    !! Positionsnummer des Parameters
    INTEGER                , INTENT(IN   )      :: par_no
    !! Rueckgabe-Parameter fuer logische Eingabewerte
    LOGICAL , DIMENSION(:) , POINTER            :: log_array
    LOGICAL , OPTIONAL     , INTENT(  OUT)      :: l_exist
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_input_data_log_array'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    INTEGER  :: i, j, k
    INTEGER  :: i_bl, i_key, i_size, z_nr
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       ! Pruefen, ob das Rueckgabefeld nicht bereits associated ist
       !
       IF ( ASSOCIATED(log_array) ) THEN
          !
          CALL setup_error_act ( all_errors(:), -60, c_upname, c_modname )
          !
          RETURN
          !       
       END IF
       !
       !   Adressnummern ermitteln und Aufruf der get_input_data-Methode checken
       !
       IF ( no_error( ) ) CALL get_par_general &
                                ( block_name, block_no, keyword, par_no, line_no, 'LOGICAL', &
                                  .True., PRESENT(l_exist), i_bl, i_key )
       !
       ! Eingabegroesse aus der Variablen vom Typ <t_block> in den
       ! Rueckgabeparameter stopfen
       !
       IF ( no_error( ) ) THEN
          !
          IF ( PRESENT(l_exist) ) l_exist = .False.

          IF ( bloecke(i_bl)%EGBAnz .GT. 0 ) THEN

             IF ( bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no) .GT. 0 ) THEN

                IF ( bloecke(i_bl)%Key(i_key)%L_Single  .OR. &
                     line_no .GT. 0 ) THEN

                   z_nr = line_no
                   IF ( line_no .EQ. 0 ) z_nr = 1

                   ! Fall : Daten aus einzelner Zeile

                   IF (ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%log)) THEN

                      i_size = SIZE(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%log)
                      !
                      ALLOCATE ( log_array(i_size), STAT=stat )
                      !
                      IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
                         !
                         CALL setup_error_act &
                               ( all_errors(:), -10000, c_upname, c_modname, stat )
                         CALL setup_error_act ( '<felder>', 'log_array' )
                         !
                         RETURN
                         !
                      END IF
                      ! 
                      DO i = 1, SIZE (log_array)

                         log_array(i) = &
                              bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%log(i)

                      END DO

                      IF ( PRESENT(l_exist) ) l_exist = .True.

                   END IF

                ELSE

                   ! Fall : Daten ggf. aus mehreren Eingabezeilen

                   i_size = 0

                   DO i = 1, bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no)

                      i_size = i_size + SIZE(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(i)%log)

                   END DO
                   !
                   ALLOCATE ( log_array(i_size), STAT=stat )
                   !
                   IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
                      !
                      CALL setup_error_act &
                            ( all_errors(:), -10000, c_upname, c_modname, stat )
                      CALL setup_error_act ( '<felder>', 'log_array' )
                      !
                      RETURN
                      !
                   END IF
                   !

                   k = 0

                   DO i = 1, bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no)

                      DO j = 1, SIZE(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(i)%log)

                         k = k + 1

                         log_array(k) = &
                              bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(i)%log(j)

                      END DO

                   END DO

                   IF ( PRESENT(l_exist) ) l_exist = .True.
   

                END IF   ! FU : Einzelzeile | verschiedene Eingabezeilen

             END IF   ! (bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no) .GT. 0)

          END IF   ! (bloecke(i_bl)%EGBAnz .GT. 0 )
          !
       END IF ! no_error( )   
       !
    END IF ! ok_initialised
    !
  END SUBROUTINE get_input_data_log_array
  !
  !! Liefert das adressierte Parameter-CHARACTER-Feld
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE get_input_data_char_array &
       ( block_name, block_no, keyword, line_no, par_no, &
       char_array, l_exist )
    !
    ! Formalparameter
    !! Name des Eingabeblockes
    CHARACTER (LEN=*)      , INTENT(IN   )              :: block_name
    !! Nummer des Eingabeblockes
    INTEGER                , INTENT(IN   )              :: block_no
    !! Schluesselwort
    CHARACTER (LEN=*)      , INTENT(IN   )              :: keyword
    !! Nummer der Eingabezeile <BR>
    !! [ 0 bedeutet gleichzeitiger Zugriff auf alle Zeilen ]
    INTEGER                , INTENT(IN   )              :: line_no
    !! Positionsnummer des Parameters
    INTEGER                , INTENT(IN   )              :: par_no
    !! Rueckgabe-Parameter fuer Charakter-Eingabewerte
    !! ACHTUNG : Laenge der Strings muss im rufenden
    !!           Programm mit 80 Zeichen vereinbart sein !!
    CHARACTER (LEN=line_len) , DIMENSION(:) , POINTER   :: char_array
    !! True , wenn fuer Parameter ein Wert existiert
    !! False, wenn kein Wert fuer Parameter exitiert, z.B.
    !!   - Block in Eingabedatei nicht vorhanden
    !!   - Keyzeile in Block nicht angegeben
    !!   - Parameter in Keyzeile nicht spezifiziert
    !! Dieser UP-Parameter muss stets vorhanden sein, wenn
    !! eines der Adress-Elemente (Block,Key,Parameter) als
    !! optional vereinbart wurde.
    LOGICAL , OPTIONAL , INTENT(  OUT)                  :: l_exist
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_input_data_char_array'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsstrings fuer Fehlermeldung
    CHARACTER (LEN=6)             :: c_actlen, c_reqlen
    !
    INTEGER  :: i, j, k
    INTEGER  :: i_bl, i_key, i_size, z_nr
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       ! Pruefen, ob das Rueckgabefeld nicht bereits associated ist
       !
       IF ( ASSOCIATED(char_array) ) THEN
          !
          CALL setup_error_act ( all_errors(:), -60, c_upname, c_modname )
          !
          RETURN
          !       
       END IF
       !
       !   Adressnummern ermitteln und Aufruf der get_input_data-Methode checken
       !
       IF ( no_error( ) ) CALL get_par_general &
                                ( block_name, block_no, keyword, par_no, line_no, 'CHARACTER', &
                                  .True., PRESENT(l_exist), i_bl, i_key )
       !
       ! Eingabegroesse aus der Variablen vom Typ <t_block> in den
       ! Rueckgabeparameter stopfen
       !
       IF ( no_error( ) ) THEN
          !
          IF ( PRESENT(l_exist) ) l_exist = .False.

          IF ( bloecke(i_bl)%EGBAnz .GT. 0 ) THEN

             IF ( bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no) .GT. 0 ) THEN

                IF ( bloecke(i_bl)%Key(i_key)%L_Single  .OR. &
                     line_no .GT. 0 ) THEN

                   z_nr = line_no
                   IF ( line_no .EQ. 0 ) z_nr = 1

                   ! Fall : Daten aus einzelner Zeile

                   IF (ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%char)) THEN

                      i_size = SIZE(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%char)
                      !
                      ALLOCATE ( char_array(i_size), STAT=stat )
                      !
                      IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
                         !
                         CALL setup_error_act &
                               ( all_errors(:), -10000, c_upname, c_modname, stat )
                         CALL setup_error_act ( '<felder>', 'char_array' )
                         !
                         RETURN
                         !
                      END IF
                      ! 
                      DO i = 1, SIZE (char_array)


                         IF (LEN_TRIM(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%char(i)) &
                              .GT.  LEN(char_array(1))                               ) THEN
                            !
                            ! Fehler -70 : Stringlaenge des Rueckgabeparameters <char_array> zu kurz !
                            !
                            c_actlen = REPEAT(' ',LEN(c_actlen))
                            c_reqlen = REPEAT(' ',LEN(c_reqlen))
                            !
                            WRITE(c_actlen,'(I5)') LEN(char_array(1))
                            WRITE(c_reqlen,'(I5)') &
                                  LEN_TRIM(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%char(i))
                            !
                            CALL setup_error_act ( all_errors(:), -70, c_upname, c_modname )
                            CALL setup_error_act ( '<act_len>', TRIM(c_actlen) )
                            CALL setup_error_act ( '<req_len>', TRIM(c_reqlen) )
                            !
                            RETURN
                            !
                         ELSE

                            char_array(i) = &
                                 TRIM(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(z_nr)%char(i))

                         END IF

                      END DO

                      IF ( PRESENT(l_exist) ) l_exist = .True.

                   END IF


                ELSE


                   ! Fall : Daten ggf. aus mehreren Eingabezeilen

                   i_size = 0

                   DO i = 1, bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no)

                      i_size = i_size + SIZE(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(i)%char)

                   END DO

                   !
                   ALLOCATE ( char_array(i_size), STAT=stat )
                   !
                   IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
                      !
                      CALL setup_error_act &
                            ( all_errors(:), -10000, c_upname, c_modname, stat )
                      CALL setup_error_act ( '<felder>', 'char_array' )
                      !
                      RETURN
                      !
                   END IF
                   ! 

                   k = 0
             
                   DO i = 1, bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no)

                      DO j = 1, SIZE(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(i)%char)

                         IF (LEN_TRIM(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(i)%char(j)) &
                              .GT.  LEN(char_array(1))                                 ) THEN
         
                            !
                            ! Fehler -70 : Stringlaenge des Rueckgabeparameters <char_array> zu kurz !
                            !
                            c_actlen = REPEAT(' ',LEN(c_actlen))
                            c_reqlen = REPEAT(' ',LEN(c_reqlen))
                            !
                            WRITE(c_actlen,'(I5)') LEN(char_array(1))
                            WRITE(c_reqlen,'(I5)') &
                                  LEN_TRIM(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(i)%char(j))
                            !
                            CALL setup_error_act ( all_errors(:), -70, c_upname, c_modname )
                            CALL setup_error_act ( '<act_len>', TRIM(c_actlen) )
                            CALL setup_error_act ( '<req_len>', TRIM(c_reqlen) )
                            !
                            RETURN
                            !
                         ELSE

                            k = k + 1

                            char_array(k) = &
                                 TRIM(bloecke(i_bl)%Key(i_key)%Par(par_no)%EGB(block_no)%Wert(i)%char(j))

                         END IF

                      END DO

                   END DO

                   IF ( PRESENT(l_exist) ) l_exist = .True.


                END IF   ! FU : Einzelzeile | verschiedene Eingabezeilen

             END IF   ! (bloecke(i_bl)%Key(i_key)%ZeilAnz(block_no) .GT. 0)

          END IF   ! (bloecke(i_bl)%EGBAnz .GT. 0 )
          !
       END IF ! no_error( )   
       !
    END IF ! ok_initialised
    !
  END SUBROUTINE get_input_data_char_array
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !+                                                                     +
  !+     PPPPPP   RRRRRR   III  VVV  VVV     AA    TTTTTTT  EEEEEEE      +
  !+     PPP  PP  RRR  RR  III  VVV  VVV    AAAA     TTT    EEE          +
  !+     PPP  PP  RRR  RR  III  VVV  VVV   AAA AA    TTT    EEE          +
  !+     PPPPPP   RRRRRR   III  VVV  VVV  AAA  AAA   TTT    EEEEEE       +
  !+     PPP      RRRRR    III  VVV  VVV  AAA  AAA   TTT    EEE          +
  !+     PPP      RRR RR   III   VVV VV   AAAAAAAA   TTT    EEE          +
  !+     PPP      RRR  RR  III    VVVV    AAA  AAA   TTT    EEE          +
  !+     PPP      RRR   RR III     VV     AAA  AAA   TTT    EEEEEEE      +
  !+                                                                     +
  !+                                                                     +
  !+   MM     MM  EEEEEEE TTTTTTT  HHH  HH   OOOOO   DDDDDD    SSSSS     +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD  SS   SS    +
  !+   MMMM MMMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD  SS         +
  !+   MMM M MMM  EEEEEE    TTT    HHHHHHH  OOO  OO  DDD  DD   SSSSSS    +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD       SSS   +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD       SSS   +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD  SSS  SSS   +
  !+   MMM   MMM  EEEEEEE   TTT    HHH  HH   OOOOO   DDDDDD    SSSSSS    +
  !+                                                                     +
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Lokale Methoden (PRIVATE)
  !
  ! ----------------------------------------------------------------------
  ! >>> ALLGEMEIN-Methoden <<< [ERR_NO =  0001 bis  0999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der Fehlerbedingung 1 = Modul nicht initialisiert
  FUNCTION ok_initialised &
       ( upname )         &
       RESULT( ok )
    !
    !! Name der Subroutine die "ok_initialised" ruft
    CHARACTER (LEN=*) , INTENT(IN) :: upname ! 
    ! Rueckgabewert
    !! Testergebnis
    LOGICAL :: ok ! 
    ! lokale Variablen
    !! Fehlernummer
    INTEGER            :: ierr    ! 
    !! Fehlertext
    CHARACTER (LEN=80) :: cerr(3) ! 
    !
    ok = initialised
    !
    IF ( .NOT. ok ) THEN
       !
       WRITE(*,*) ' *** Warnung *** Modul "p_dictionary_ui" nicht initialisiert'
       !
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       !
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_dictionary ausfuehren'
       !
       CALL setup_error_act ( ierr, cerr(:), upname, c_modname )
       !
    END IF
    !
  END FUNCTION ok_initialised
  !
  !! Setzen der Fehlerbedingung 2 = Modul schon initialisiert
  FUNCTION not_initialised &
       ( upname )          &
       RESULT( ok )
    !
    !! Name der Subroutine die "not_initialised" ruft
    CHARACTER (LEN=*) , INTENT(IN) :: upname ! 
    ! Rueckgabewert
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .NOT. initialised
    !
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 2, upname, c_modname )
    !
  END FUNCTION not_initialised
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-INIT-Methoden <<< [ERR_NO =  1000 bis  1999]
  ! ----------------------------------------------------------------------
  !
  !! Allokieren/Initialisieren aller Fehlermeldungen des Packages
  SUBROUTINE init_dictionary_all_errors &
       ( )
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER  :: c_upname='init_dictionary_all_errors' !
    !
    ! --------------------------------------------------------------------
    ! [1] Allokieren/Initialisieren des Feldes "all_errors"
    ! --------------------------------------------------------------------
    !
    ALLOCATE ( all_errors( c_nofallerrors ) )
    !
    CALL new_error( all_errors(:) )
    !
    ! --------------------------------------------------------------------
    ! [2] aktuelle Fehlermeldungen speichern "all_errors"
    ! --------------------------------------------------------------------
    !
    ! Obacht(!) auf "c_nofallerrors" geben
    !
    !
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! Fehler, die im Modul MOD_P_DICTION_UI auftreten :
    ! Ausnahme : ALLOCATE und DEALLOCATE-Errormeldung, diese
    !            kann von allen Package-Modulen gesetzt werden !
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !
    ! Index 001
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors( 1), 1 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors( 1), &
         'Fehlerkategorie: ALLGEMEIN\n'//&
         'Package "p_dictionary" ist nicht initialisiert\n'//&
         '--> INIT_dictionary ausfuehren' )
    ! Index 002
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors( 2), 2 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors( 2), &
         'Fehlerkategorie: ALLGEMEIN\n'//&
         'Package "p_dictionary" ist schon initialisiert\n'//&
         '--> CLEAR_dictionary ausfuehren' )
    ! Index 003
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors( 3), 7500 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors( 3), &
         'Fehlerkategorie: PRINT-Methoden\n'//&
         'Fehler beim Drucken statischer Daten aus "p_dictionary_ui"\n'//&
         '--> Code in Modul "p_dictionary_ui" / Daten pruefen' )
    !
    ! Index 004
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors( 4), -10000 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors( 4), &
         'Fehlerkategorie: ALLOCATE-Anweisung\n'//&
         'Fehler bei Speicher-ALLOCATE Anweisung\n'//&
         'Felder         : <felder>\n'//&
         '\n'//&
         '--> Pruefe ALLOCATE-Anweisung' )
    !
    ! Index 005
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors( 5), -20000 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors( 5), &
         'Fehlerkategorie: DEALLOCATE-Anweisung\n'//&
         'Fehler bei Speicher-DEALLOCATE Anweisung\n'//&
         'Felder         : <felder>\n'//&
         '\n'//&
         '--> Pruefe DEALLOCATE-Anweisung' )
    !
    ! Index 006
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors( 6), -10 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors( 6), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Name der Dictionary-Datei ist laenger als 80 Zeichen !\n'//&
         'Name : <name>\n'//&
         '--> check program !' )
    !
    ! Index 007
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors( 7), -20 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors( 7), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Fehler in Eingabedatei <file_name> !\n'//&
         'Unbekannter Name eines Datenblocks \n'//&
         'aktuell = <blockname>\n'//&
         'erlaubt = <erlaubt>\n'//&
         '--> Eingabedatei korrigieren / Programmcode erweitern' )
    !
    ! Index 008
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors( 8), -30 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors( 8), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Fehler in Eingabedatei <file_name> !\n'//&
         'Der Block <blockname> wurde mehrfach spezifiziert !\n'//&
         '--> Eingabesteuer-Datei pruefen !' )
    !
    ! Index 009
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors( 9), -40 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors( 9), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Fehler in Eingabedatei <file_name> !\n'//&
         'Die Datei ist unvollstaendig !\n'//&
         'Es fehlt ein Block mit Namen : <blockname>!\n'//&
         '--> siehe Muster fuer Eingabedatei !' )
    !
    ! Index 010
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(10), -50 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(10), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Fehler im Aufruf der get_input_data-Methode !\n'//&
         'Stringlaenge des Rueckgabeparameters <char_value> zu kurz !\n'//&
         'aktuell   : <act_len>\n'//&
         'benoetigt : <req_len>\n'//&
         '\n'//&
         '--> check program !' )
    !
    ! Index 011
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(11), -60 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(11), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Aufruf der get_input_data-Methode fehlerhaft !\n'//&
         'Das Rueckgabefeld ist bereits associated !\n'//&
         'Das ist nicht zulaessig !\n'//&
         '\n'//&
         '-->   check program !' )
    !
    ! Index 012
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(12), -70 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(12), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Fehler im Aufruf der get_input_data-Methode !\n'//&
         'Stringlaenge des Rueckgabeparameters <char_array> zu kurz !\n'//&
         'aktuell   : <act_len>\n'//&
         'benoetigt : <req_len>\n'//&
         '\n'//&
         '--> check program !' )
    !
    ! Index 013
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(13), -80 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(13), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Fehler in Eingabedatei <file_name> !\n'//&
         'Der<nummer> <blockname>-Block der Datei\n'//&
         'enthaelt keine Eingabe-Zeile !\n'//&
         '--> Eingabesteuer-Datei pruefen !' )
    !
    ! Index 014
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(14), -90 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(14), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Fehler in Eingabedatei <file_name> !\n'//&
         'Der<nummer> <blockname>-Block der Datei\n'//&
         'enthaelt eine Eingabe-Zeile doppelt !\n'//&
         '--> Eingabesteuer-Datei pruefen !' )
    !
    ! Index 015
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(15), -100 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(15), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Fehler in Eingabedatei <file_name> !\n'//&
         'Der<nummer> <blockname>-Block der Datei\n'//&
         'ist unvollstaendig ! Es fehlt die Eingabezeile mit\n'//&
         'dem Schluesselwort : <schluessel>\n'//&
         '\n'//&
         '--> Vergleiche Eingabedaten mit Muster-Datei !' )
    !
    ! Index 016
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(16), -110 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(16), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Fehler in Eingabedatei <file_name> !\n'//&
         'Im<block_nr> <blockname>-Block der Datei ist die\n'//&
         'Eingabe in der<zeile_nr> Zeile mit dem Schluesselwort\n'//&
         '<schluessel> laenger als <kom_len> Zeichen !\n'//&
         'Die Laenge von Kommentarzeilen ist eingeschraenkt !\n'//&
         '\n'//&
         '-> Eingabedaten korrigieren !' )
    !
    ! Index 017
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(17), -120 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(17), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Fehler in Eingabedatei <file_name> !\n'//&
         'Lesefehler im<block_nr> <blockname>-Block der Datei !\n'//&
         'In der<zeile_nr> Zeile mit dem Schluesselwort <schluessel>\n'//&
         'wird fuer den<par_nr> Parameter eine Eingabe vom\n'//&
         'Typ <datentyp> erwartet !\n'//&
         '\n'//&
         '-> Eingabedaten korrigieren !' )
    !
    ! Index 018
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(18), -130 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(18), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Fehler in Eingabedatei <file_name> !\n'//&
         'In Block <blockname> enthaelt eine Zeile\n'//&
         'mit dem Schluesselwort : <schluessel>\n'//&
         'zuviele Angaben !\n'//&
         'Problemzeile : <key> = <wert>\n'//&
         '\n'//&
         '--> Pruefe Eingabesteuerdatei !' )
    !
    ! Index 019
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(19), -140 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(19), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Fehler in Eingabedatei <file_name> !\n'//&
         'In Block <blockname> enthaelt eine Zeile\n'//&
         'mit dem Schluesselwort : <schluessel>\n'//&
         'zuwenig Angaben !\n'//&
         'Problemzeile : <key> = <wert>\n'//&
         '\n'//&
         '--> Pruefe Eingabesteuerdatei !' )
    !
    ! Index 020
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(20), -150 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(20), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Fehler im Aufruf der <meth_name>-Methode !\n'//&
         'Der angegebene Blockname ist zu lang !\n'//&
         'aktuelle Stringlaenge : <akt_len>\n'//&
         'erlaubte Stringlaenge : <max_len>\n'//&
         '\n'//&
         '--> check program !' )
    !
    ! Index 021
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(21), -160 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(21), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Fehler im Aufruf der <meth_name>-Methode !\n'//&
         'Das angegebene Schluesselwort ist zu lang !\n'//&
         'aktuelle Stringlaenge : <akt_len>\n'//&
         'erlaubte Stringlaenge : <max_len>\n'//&
         '\n'//&
         '--> check program !' )
    !
    ! Index 022
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(22), -170 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(22), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Fehler im Aufruf der <meth_name>-Methode !\n'//&
         'Der angegebenen Blockname ist ungueltig !\n'//&
         'aktuell  : <akt_block>\n'//&
         'erlaubt  : siehe Dictionary-Datei !\n'//&
         '\n'//&
         '--> check program !' )
    !
    ! Index 023
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(23), -180 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(23), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Fehler im Aufruf der <meth_name>-Methode ! In Block <blockname>\n'//&
         'existiert das angegebene Schluesselwort nicht !\n'//&
         'aktuell  : <akt_key>\n'//&
         'erlaubte KEYs  : siehe Dictionary-Datei !\n'//&
         '\n'//&
         '--> check program !' )
    !
    ! Index 024
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(24), -190 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(24), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Fehler im Aufruf der <meth_name>-Methode ! In Block <blockname>\n'//&
         'existiert fuer das Schluesselwort <schluessel>\n'//&
         'kein Parameter mit der Positionsnummer <pos_nr> !\n'//&
         'erlaubte Positionsnummern  :  siehe Dictionary-Datei !\n'//&
         '\n'//&
         '--> check program !' )
    !
    ! Index 025
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(25), -200 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(25), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Fehler im Aufruf der <meth_name>-Methode !\n'//&
         'Die angegebene Eingabeblocknummer ist kleiner als 1 !\n'//&
         '--> check program !' )
    !
    ! Index 026
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(26), -210 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(26), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Fehler im Aufruf einer <meth_name>-Methode !\n'//&
         'Die angegebene Eingabeblocknummer ist groesser als die\n'//&
         'Zahl der Eingabebloecke mit Namen <blockname>\n'//&
         'in der Eingabedatei !\n'//&
         '\n'//&
         '--> check program !' )
    !
    ! Index 027
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(27), -220 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(27), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Fehler in Unterprogramm-Aufruf !\n'//&
         'Parameter "dattype" hat unzulaessigenn Wert\n'//&
         '\n'//&
         '--> check program !' )
    !
    ! Index 028
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(28), -230 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(28), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Aufruf der get_input_data-Methode ist fehlerhaft ! Der Parameter <par>\n'//&
         'des KEYs <key> in Block <block>\n'//&
         'benoetigt ein(en) Rueckgabewert, bzw. -feld vom Typ <req_typ> !\n'//&
         'Aktueller Datentyp aber ist : <akt_typ>\n'//&
         '\n'//&
         '--> check program !' )
    !
    ! Index 029
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(29), -240 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(29), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Aufruf der get_input_data-Methode ist fehlerhaft ! Fuer die Zeilen\n'//&
         'mit dem KEY <key> in Block <block>\n'//&
         'gilt : L_OneArray = False !\n'//&
         'Die Parameterwerte muessen daher zeilenweise abgefragt werden !\n'//&
         '\n'//&
         'line_no = 0 ist nicht zulaessig --> check program !' )
    !
    ! Index 030
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(30), -250 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(30), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Aufruf der get_input_data-Methode ist fehlerhaft ! Der Parameter <par>\n'//&
         'des KEYs <key> in Block <block>\n'//&
         'wurde als optional (L_Opt=True) vereinbart !\n'//&
         'Dieser Parameterwert muss daher zeilenweise abgefragt werden !\n'//&
         '\n'//&
         'line_no = 0 ist nicht zulaessig --> check program !' )
    !
    ! Index 031
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(31), -260 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(31), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Aufruf der get_input_data-Methode ist fehlerhaft ! Die Zeile mit\n'//&
         'dem KEY <key> in Block <block> enthaelt\n'//&
         'verschiedene Parameter und fuer den <par>. gilt : L_Array = True !\n'//&
         'Dieser Parameterwert muss daher zeilenweise abgefragt werden !\n'//&
         '\n'//&
         'line_no = 0 ist nicht zulaessig --> check program !' )
    !
    ! Index 032
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(32), -270 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(32), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Aufruf der get_input_data-Methode ist fehlerhaft ! Der Parameter <par>\n'//&
         'des KEYs <key> in Block <block>\n'//&
         'wurde fuer eine Zeilennummer abgefragt, welche groesser ist\n'//&
         'als die aktuelle Anzahl an Eingabezeilen mit diesem KEY !\n'//&
         '\n'//&
         '--> check program !' )
    !
    ! Index 033
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(33), -280 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(33), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Aufruf der get_input_data-Methode ist fehlerhaft ! Der Parameter <par>\n'//&
         'des KEYs <key> in Block <block>\n'//&
         'wurde fuer eine Zeilennummer abgefragt, die negativ ist !\n'//&
         'Das ist nicht zulaessig !\n'//&
         '\n'//&
         '--> check program !' )
    !
    ! Index 034
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(34), -290 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(34), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Aufruf der get_input_data-Methode ist fehlerhaft ! Der Parameter <par>\n'//&
         'des KEYs <key> in Block <block>\n'//&
         'wurde als Array-Groesse (L_Array=True) vereinbart und benoetigt\n'//&
         'daher ein Feld fuer die Rausgabe der Eingabewerte !\n'//&
         '\n'//&
         '-->    check program !' )
    !
    ! Index 035
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(35), -300 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(35), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Aufruf der get_input_data-Methode ist fehlerhaft ! Die Zeile mit dem\n'//&
         'KEY <key> in Block <block> darf mehrfach vorkommen\n'//&
         'und erfordert daher entweder ein Feld fuer die Rausgabe der\n'//&
         'Eingabewerte, oder die Direktadressierung ueber die Zeilennummer !\n'//&
         '\n'//&
         '-->    check program !' )
    !
    ! Index 036
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(36), -310 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(36), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Aufruf der get_input_data-Methode ist fehlerhaft ! Der Parameter <par>\n'//&
         'des KEYs <key> in Block <block> steht in\n'//&
         'einem optionalen Eingabeblock (L_Opt=True), daher muss der\n'//&
         'get_input_data-Aufruf den optionalen Parameter <l_exist> enthalten !\n'//&
         '\n'//&
         '-->    check program !' )
    !
    ! Index 037
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(37), -320 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(37), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Aufruf der get_input_data-Methode ist fehlerhaft ! Der Parameter <par>\n'//&
         'des KEYs <key> in Block <block> steht in\n'//&
         'einer optionalen Keyzeile (L_Opt=True), daher muss der\n'//&
         'get_input_data-Aufruf den optionalen Parameter <l_exist> enthalten !\n'//&
         '\n'//&
         '-->    check program !' )
    !
    ! Index 038
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(38), -330 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(38), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Aufruf der get_input_data-Methode ist fehlerhaft ! Der Parameter <par>\n'//&
         'des KEYs <key> in Block <block> ist als\n'//&
         'optional (L_Opt=True) vereinbart, der get_input_data-Aufruf\n'//&
         'muss daher den optionalen Parameter <l_exist> enthalten !\n'//&
         '\n'//&
         '-->    check program !' )
    !
    ! Index 039
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(39), -340 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(39), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Name der Dictionary-Datei ist unzulaessig lang !\n'//&
         'Name : <dic_name>\n'//&
         'aktuelle Laenge = <act_len> Zeichen\n'//&
         'erlaubte Laenge = <max_len> Zeichen\n'//&
         '\n'//&
         '--> kuerzeren Namen fuer die Dictionary-Datei waehlen, oder\n'//&
         '    ggf. $DICDIR-Pfad kleiner halten !' )
    !
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! Fehler, die im Modul MOD_M_DICTION_EIN auftreten :
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !
    ! Index 040
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(40), -510 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(40), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Die Datei enthaelt eine ungueltige BEGINDATA-Struktur !\n'//&
         'aktuell : BEGINDATA <akt_blockname>\n'//&
         'erlaubt : BEGINDATA BLOCK <blockname>\n'//&           ! <blockname> hier kein Platzhalter !
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 041
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(41), -520 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(41), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Die Datei enthaelt eine BLOCK-Beschreibung ohne Namen !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 042
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(42), -530 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(42), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Name einer BLOCK-Beschreibung ist laenger als\n'//&
         '<len_block_name> Zeichen !\n'//&
         'BLOCK-Name  = <blockname>\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 043
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(43), -540 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(43), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Die Datei muss mindestens eine BLOCK-Beschreibung\n'//&
         'enthalten !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 044
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(44), -550 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(44), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der BLOCK mit Namen <blockname> wurde\n'//&
         'mehrfach beschrieben !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 045
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(45), -560 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(45), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Die Datei enthaelt in BLOCK <blockname> eine ungueltige\n'//&
         'BEGINDATA-Struktur !\n'//&
         'aktuell : BEGINDATA <akt_name>\n'//&
         'erlaubt : BEGINDATA KEY <keyword>\n'//&     ! <keyword> kein Platzhalter !
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 046
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(46), -570 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(46), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Die Datei enthaelt in BLOCK <blockname> \n'//&
         'einen KEY-Block ohne Schluesselwortangabe !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 047
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(47), -580 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(47), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in BLOCK <blockname> der \n'//&
         'Dictionary-Datei <file_name> ! \n'//&
         'Die Stringlaenge eines Schluesselwortes ist\n'//&
         'laenger als <len_key_name> Zeichen !\n'//&
         'KEY  = <keyblock_name>\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 048
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(48), -590 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(48), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         '<datei-element>\n'//&
         'der Datei ist unvollstaendig !\n'//&
         'Es fehlt die Zeile mit dem Schluesselwort <keyname> !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 049
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(49), -610 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(49), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         '<dictionary-element>\n'//&
         'der Datei ist fehlerhaft !\n'//&
         'Eine <reqif_notif>-Bedingung darf nur gesetzt werden,\n'//&
         'wenn der <block_key_par> als optional vereinbart wurde !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 050
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(50), -620 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(50), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der BLOCK <blockname> muss mindestens eine\n'//&
         'KEY-Beschreibung enthalten !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 051
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(51), -630 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(51), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'In BLOCK <blockname> wurde das Schluesselwort\n'//&
         '<keyname> mehrfach beschrieben !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 052
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(52), -640 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(52), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Die KEY-Beschreibung <keyname> in BLOCK <blockname>\n'//&
         'enthaelt eine ungueltige BEGINDATA-Struktur !\n'//&
         'aktuell : BEGINDATA <akt_name>\n'//&
         'erlaubt : BEGINDATA PAR <Parameter-Nummer>\n'//&    ! <Parameter-Nummer> kein Platzhalter !
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 053
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(53), -650 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(53), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Die KEY-Beschreibung <keyname>\n'//&
         'in BLOCK <blockname>\n'//&
         'enthaelt einen PAR-Block ohne Parameternummer !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 054
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(54), -660 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(54), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Die KEY-Beschreibung <keyname>\n'//&
         'in BLOCK <blockname>\n'//&
         'enthaelt einen PAR-Block dessen Parameternummer\n'//&
         'keine ist & einen Lesefehler hervorruft !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 055
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(55), -670 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(55), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> ! \n'//&
         'In BLOCK <blockname> wurde fuer den einzigen \n'//&
         'Parameter des KEYs <keyname>\n'//&
         'das Attribut "L_Opt" auf  .True.  gesetzt.\n'//&
         'Das ist unzulaessig, da ueber KEY-Optionalitaet steuerbar !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 056
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(56), -680 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(56), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Fuer den KEY <keyname>\n'//&
         'in BLOCK <blockname> gilt : L_Comment = True !\n'//&
         'Fuer Kommentarzeilen ist es nicht zulaessig mehr als\n'//&
         'einen Parameter zu spezifizieren !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 057
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(57), -690 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(57), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Fuer den KEY <keyname>\n'//&
         'in BLOCK <blockname> gilt : L_Comment = True !\n'//&
         'Der Parameter-Typ von Kommentarzeilen muss vom Typ\n'//&
         'CHARACTER sein ==> Type = CHAR !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 058
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(58), -700 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(58), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Fuer den KEY <keyname>\n'//&
         'in BLOCK <blockname> gilt : L_Comment = True !\n'//&
         'Fuer den zugehoerigen Parameter wurde L_Array = True gesetzt !\n'//&
         'Das macht keinen Sinn !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 059
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(59), -710 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(59), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block KEY <keyname> \n'//&
         'in BLOCK <blockname> ist unvollstaendig !\n'//&
         'Es wurde kein Parameter spezifiziert !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 060
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(60), -720 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(60), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der KEY <keyname> \n'//&
         'in BLOCK <blockname> ist fehlerhaft !\n'//&
         'Der Parameter mit der Positions-Nummer = <pos_nr>\n'//&
         'wurde mehrfach spezifiziert !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 061
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(61), -730 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(61), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der KEY <keyname> \n'//&
         'in BLOCK <blockname> ist fehlerhaft !\n'//&
         'Die Parameter-Positions-Nummer = <pos_nr>\n'//&
         'liegt ausserhalb des zulaessigen Wertebereiches  !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 062
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(62), -740 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(62), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Die Beschreibung des PARAMETERs <pos_nr> des KEYs <keyname>\n'//&
         'in BLOCK <blockname> ist fehlerhaft !\n'//&
         'Der Parameter wurde als Array-Groesse vereinbart (L_Array=True),\n'//&
         'obwohl er nicht an letzter Position der Key-Zeile anzugeben ist !\n'//&
         'Das ist nicht zulaessig !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 063
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(63), -750 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(63), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Die Beschreibung des PARAMETERs <pos_nr> des KEYs <keyname>\n'//&
         'in BLOCK <blockname> ist fehlerhaft !\n'//&
         'Der Parameter wurde als optional vereinbart (L_Opt=True),\n'//&
         'obwohl er nicht an letzter Position der Key-Zeile anzugeben ist !\n'//&
         'Das ist nicht zulaessig !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 064
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(64), -760 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(64), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Die Beschreibung des PARAMETERs <pos_nr> des KEYs <keyname>\n'//&
         'in BLOCK <blockname> ist fehlerhaft !\n'//&
         'Eine L_File... -Bedingung darf nur gesetzt werden,\n'//&
         'wenn der Parameter vom Type FILE ist !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 065
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(65), -770 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(65), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Die Beschreibung des PARAMETERs <pos_nr>\n'//&
         'des KEYs <keyname>\n'//&
         'in BLOCK <blockname> ist fehlerhaft !\n'//&
         'Die FILE-Anforderungen : L_FileReq und L_FileNew \n'//&
         'duerfen nicht gleichzeitig auf True gesetzt sein !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 066
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(66), -780 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(66), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Fuer Parameter <pos_nr> des KEYs <keyname>\n'//&
         'in BLOCK <blockname> wurde fuer den Datentyp \n'//&
         'ein ungueltiger Wert angegeben !\n'//&
         'aktuell = <akt_wert>\n'//&
         'erlaubt = [INT,REAL,DOUBLE,CHAR,LOG,FILE,DATE,INCR]\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! Fehler, die im Modul MOD_M_DIC_CHECKS auftreten :
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !
    ! Index 067
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(67), -1010 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(67), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine Check-Zeile doppelt !\n'//&
         'Zeile : <akt_key> = <akt_wert>\n'//&
         '\n'//&
         '-->  Dictionary-Datei pruefen !' )
    !
    ! Index 068
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(68), -1020 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(68), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Unterprogramm wurde gerufen, ohne das fuer\n'//&
         'das Objekt vom Typ <objekt_typ> eine Check-Anforderung\n'//&
         'des Typs <check_art> existiert !\n'//&
         'Das ist nicht zulaessig !\n'//&
         '\n'//&
         '--> Unterprogramm-Aufruf pruefen !' )
    !
    ! Index 069
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(69), -1030 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(69), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Unterprogramm-Aufruf !\n'//&
         'Das Parameter-Pointerfeld <bloecke> ist\n'//&   ! <bloecke> kein Platzhalter !
         '.not. associated !\n'//&
         'Das ist nicht zulaessig !\n'//&
         '\n'//&
         '--> Unterprogramm-Aufruf pruefen !' )
    !
    ! Index 070
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(70), -1040 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(70), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Unterprogramm-Aufruf !\n'//&
         'Parameter <ck_art> hat unerlaubten Wert !\n'//&   ! <ck_art> kein Platzhalter !
         'Es wurde weder "ReqIf", noch "NotIf" angegeben !\n'//&
         'Das ist nicht zulaessig !\n'//&
         '\n'//&
         '--> Unterprogramm-Aufruf pruefen !' )
    !
    ! Index 071
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(71), -1050 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(71), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte <check_art>-Zeile !\n'//&
         'Existenz des Objektes haengt vom Zustand eines Objektes derselben\n'//&
         '"Adresslinie" ab ! Das macht keinen Sinn !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 072
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(72), -1060 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(72), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte <check_art>-Zeile !\n'//&
         'Das Vergleichs-Objekt wurde nicht als optional vereinbart !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 073
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(73), -1070 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(73), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte <check_art>-Zeile !\n'//&
         'Der Vergleichs-Parameter ist unzulaessigerweise eine Feldgroesse !\n'//&
         'Fuer ihn gilt : L_Array = True \n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 074
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(74), -1080 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(74), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte <check_art>-Zeile !\n'//&
         'Der Vergleichs-Parameter kann nicht eindeutig identifiziert\n'//&
         'werden, da fuer seine Keyzeile gilt : L_Single = False !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 075
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(75), -1090 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(75), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte <check_art>-Zeile !\n'//&
         'Das Vergleichs-Objekt kann nicht eindeutig identifiziert werden,\n'//&
         'da fuer seinen Eingabeblock-Typ gilt : L_Single = False !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 076
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(76), -1100 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(76), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Unterprogramm-Aufruf !\n'//&
         'Das Parameter-Pointerfeld <bloecke> ist\n'//&
         '.not. associated !\n'//&
         'Das ist nicht zulaessig !\n'//&
         '\n'//&
         '--> Unterprogramm-Aufruf pruefen !' )
    !
    ! Index 077
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(77), -1110 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(77), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt fehlerhafte ReqIf|NotIf-Zeilen !\n'//&
         'Die angegebenen Check-Anforderungen schliessen sich bereits\n'//&
         'gegenseitig aus ! \n'//&
         'Das macht keinen Sinn !\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 078
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(78), -1120 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(78), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Unterprogramm wurde gerufen, ohne das fuer\n'//&
         'den Parameter eine CheckIfPar-Anforderung\n'//&
         'existiert !\n'//&
         'Das ist nicht zulaessig !\n'//&
         '\n'//&
         '--> Unterprogramm-Aufruf pruefen !' )
    !
    ! Index 079
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(79), -1130 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(79), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte CheckIfPar-Zeile !\n'//&
         'Fuer den Parameter-Datentyp "<par_typ>" sind lediglich\n'//&
         'die Vergleichsoperationen [.EQ.,.NE.] zugelassen !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 080
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(80), -1140 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(80), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte CheckIfPar-Zeile !\n'//&
         'Der Datentyp des Vergleichs-Objektes stimmt nicht mit dem\n'//&
         'Datentyp des Parameters ueberein. \n'//&
         'Das ist nicht zulaessig !\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 081
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(81), -1150 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(81), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte CheckIfPar-Zeile !\n'//&
         'Der Vergleichs-Parameter ist unzulaessigerweise eine Feldgroesse !\n'//&
         'Fuer ihn gilt : L_Array = True\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 082
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(82), -1160 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(82), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte CheckIfPar-Zeile !\n'//&
         'Fuer den Vergleichs-Parameter kommen unzulaessig viele Werte in\n'//&
         'Frage, da fuer seine Keyzeile gilt : L_Single = False !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 083
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(83), -1170 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(83), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte CheckIfPar-Zeile !\n'//&
         'Fuer den Vergleichs-Parameter kommen unzulaessig viele Werte in\n'//&
         'Frage, da fuer seinen Eingabeblock gilt : L_Single = False !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 084
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(84), -1180 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(84), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte CheckIfPar-Zeile !\n'//&
         'Der Vergleichs-Parameter unterliegt einer unzulaessigen \n'//&
         'Optionalitaet, da fuer <wen_objekt> gilt : L_Opt = True !\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 085
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(85), -1190 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(85), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte Check-Vorschrift !\n'//&
         'Fehler beim Lesen des Wertes einer CheckIfPar-Anforderung !\n'//&
         'Der Datentyp des Vergleichswertes stimmt nicht mit dem\n'//&
         'Parameter-Typ ueberein !\n'//&
         '-->  Dictionary-Datei checken !' )
    !
    ! Index 086
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(86), -1200 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(86), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte Check-Vorschrift !\n'//&
         'Eine der CheckIfPar-Anforderungen enthaelt mehr als eine Angabe\n'//&
         'fuer den Vergleichswert !\n'//&
         '-->  Korrigiere Dictionary-Datei !' )
    !
    ! Index 087
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(87), -1210 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(87), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Unterprogramm-Aufruf !\n'//&
         'Das Parameter-Pointerfeld <bloecke> ist\n'//&
         '.not. associated !\n'//&
         'Das ist nicht zulaessig !\n'//&
         '\n'//&
         '--> Unterprogramm-Aufruf pruefen !' )
    !
    ! Index 088
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(88), -1220 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(88), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte <check_art>-Zeile !\n'//&
         'Das Vergleichs-Objekt ist identisch mit dem Objekt, welches die\n'//&
         'Check-Anforderung stellt.\n'//&
         'Das ist nicht zulaessig !\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 089
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(89), -1230 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(89), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte <check_art>-Zeile !\n'//&
         'Das Vergleichs-Objekt ist innerhalb der Dictionary-Datei\n'//&
         'nicht definiert.\n'//&
         'Das ist nicht zulaessig !\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 090
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(90), -1240 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(90), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Das Unterprogramm wird mit einem unzulaessigen\n'//&
         'Wert des Parameters <ck_typ> aufgerufen !\n'//&      ! <ck_typ> kein Platzhalter !
         'aktuell = <check_typ>\n'//&
         'erlaubt = [ReqIf,NotIf,CheckIfPar]\n'//&
         '\n'//&
         '--> Unterprogrammaufruf pruefen !' )
    !
    ! Index 091
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(91), -1250 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(91), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Der Wert des Parameters <anz> im Unterprogramm-Aufruf\n'//&  ! <anz> kein Platzhalter !
         'ist zu klein !\n'//&
         'Die uebergebene verkettete Liste enthaelt eine groessere\n'//&
         'Anzahl an in Frage kommenden Elementen !\n'//&
         '\n'//&
         '--> Unterprogrammaufruf pruefen !' )
    !
    ! Index 092
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(92), -1260 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(92), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte Check-Vorschrift !\n'//&
         'Der Ausdruck enthaelt eine Klammer zuviel !\n'//&
         'Zeile : <check_typ> = <ausdruck>\n'//&
         '\n'//&
         '-->  Vergleiche mit Muster-Datei !' )
    !
    ! Index 093
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(93), -1270 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(93), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte Check-Vorschrift !\n'//&
         'Der Ausdruck enthaelt ein ")" vor, bzw. ohne zugehoeriges "(" !\n'//&
         'Zeile : <check_typ> = <ausdruck>\n'//&
         '\n'//&
         '-->  Vergleiche mit Muster-Datei !' )
    !
    ! Index 094
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(94), -1280 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(94), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte Check-Vorschrift !\n'//&
         'Der 2. Punkt steht hinter den Klammern, der 1. Punkt aber davor !\n'//&
         'Zeile : <check_typ> = <ausdruck>\n'//&
         '\n'//&
         '-->  Vergleiche mit Muster-Datei !' )
    !
    ! Index 095
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(95), -1290 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(95), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte Check-Vorschrift !\n'//&
         'Dem Klammer-Ausdruck fehlt die schliessende Klammer \n'//&
         'Zeile : <check_typ> = <ausdruck>\n'//&
         '\n'//&
         '-->  Vergleiche mit Muster-Datei !' )
    !
    ! Index 096
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(96), -1300 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(96), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte Check-Vorschrift !\n'//&
         'Einer oder beide Punkte fehlen, welche die Check-Operation kapseln !\n'//&
         'Zeile : <check_typ> = <ausdruck>\n'//&
         '\n'//&
         '-->  Vergleiche mit Muster-Datei !' )
    !
    ! Index 097
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(97), -1310 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(97), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte Check-Vorschrift !\n'//&
         'Zeile : <check_typ> = <ausdruck>\n'//&
         'Es fehlt der Klammer-Ausdruck mit der Objekt-Adresse !\n'//&
         '\n'//&
         '--> Vergleiche mit Musterdatei !' )
    !
    ! Index 098
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(98), -1320 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(98), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte Check-Vorschrift !\n'//&
         'Vergleichsoperation darf nicht vor Klammer-Ausdruck stehen !\n'//&
         'Zeile : <check_typ> = <ausdruck>\n'//&
         '\n'//&
         '-->  Vergleiche mit Muster-Datei !' )
    !
    ! Index 099
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(99), -1330 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(99), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte Check-Vorschrift !\n'//&
         'Zeile : <check_typ> = <ausdruck>\n'//&
         'Unzulaessiger Check-Objekt-Typ !  Erlaubt : <erlaubt>\n'//&
         '\n'//&
         '--> Vergleiche mit Musterdatei !' )
    !
    ! Index 100
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(100), -1340 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(100), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte Check-Vorschrift !\n'//&
         'Zeile : <check_typ> = <ausdruck>\n'//&
         'Unzulaessige Check-Operation !  Erlaubt : <erlaubt>\n'//&
         '\n'//&
         '--> Vergleiche mit Musterdatei !' )
    !
    ! Index 101
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(101), -1350 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(101), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte Check-Vorschrift !\n'//&
         'Vergleichsoperation darf nicht hinter Klammer-Ausdruck stehen !\n'//&
         'Zeile : <check_typ> = <ausdruck>\n'//&
         '\n'//&
         '-->  Vergleiche mit Muster-Datei !' )
    !
    ! Index 102
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(102), -1360 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(102), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Das Unterprogramm wird mit einem unzulaessigen\n'//&
         'Wert des Parameters <ck_typ> aufgerufen !\n'//&         ! <ck_typ> kein Platzhalter !
         'aktuell = <check_typ>\n'//&
         'erlaubt = [ReqIf,NotIf,CheckIfPar]\n'//&
         '\n'//&
         '--> Unterprogrammaufruf pruefen !' )
    !
    ! Index 103
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(103), -1370 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(103), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte Check-Vorschrift !\n'//&
         'Es trat ein Fehler beim Lesen der Parameter-Positionsnummer auf !\n'//&
         'Zeile : <check_typ> = <ausdruck>\n'//&
         '\n'//&
         '--> Dictionary-Datei korrigieren !' )
    !
    ! Index 104
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(104), -1380 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(104), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte Check-Vorschrift !\n'//&
         'Der Klammer-Ausdruck mit Objekt-Adresse enthaelt zuviele Angaben !\n'//&
         'Zeile : <check_typ> = <ausdruck>\n'//&
         '\n'//&
         '--> Vergleiche mit Musterdatei !' )
    !
    ! Index 105
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(105), -1390 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(105), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte Check-Vorschrift !\n'//&
         'Die Klammern enthalten nur einen BlankString !\n'//&
         'Zeile : <check_typ> = <ausdruck>\n'//&
         '\n'//&
         '-->  Dictionary-Datei korrigieren !' )
    !
    ! Index 106
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(106), -1400 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(106), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei <file_name> !\n'//&
         'Der Block <blockname>\n'//&
         'enthaelt eine fehlerhafte Check-Vorschrift !\n'//&
         'Zeile : <check_typ> = <ausdruck>\\n'//&
         'Es wurde kein Vergleichswert angegeben !\n'//&
         '\n'//&
         '--> Dictionary-Datei korrigieren !' )
    !
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! Fehler, die im Modul MOD_M_STRINGLISTE auftreten :
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !
    ! Index 107
    ! ACHTUNG : Fehlernummer darf nicht mehr geaendert werden
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(107), -1510 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(107), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Lesefehler beim Lesen einer <Datentyp>-Zahl\n'//&
         'aus einer Stringliste !\n'//&
         'ACHTUNG : Liste darf nur <Datentyp>-Groessen enthalten !\n'//&
         'Liste = <list_string>\n'//&
         '\n'//&
         '-->  Einzulesende Liste checken !' )
    !
    ! Index 108
    ! ACHTUNG : Fehlernummer darf nicht mehr geaendert werden
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(108), -1520 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(108), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler beim Lesen einer LOGICAL-Liste !\n'//&
         'Liste darf lediglich Groessen enthalten, welche\n'//&
         'als logische Groesse interpretiert werden koennen !\n'//&
         'Liste = <list_string>\n'//&
         '\n'//&
         '-->  Einzulesende Liste checken !' )
    !
    ! Index 109
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(109), -1530 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(109), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Unterprogramm-Aufruf !\n'//&
         'Der Parameter SIGN darf nur ein Zeichen \n'//&
         'lang sein .\n'//&
         '\n'//&
         '--> Unterprogrammaufruf checken !' )
    !
    ! Index 110
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(110), -1540 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(110), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Unterprogramm-Aufruf !\n'//&
         'Als Parameter LIST_STRING wurde ein String \n'//&
         'uebergeben der nur aus Blanks besteht.\n'//&
         'Das ist nicht zulaessig !\n'//&
         '\n'//&
         '--> Unterprogrammaufruf checken !' )
    !
    ! Index 111
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(111), -1550 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(111), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Unterprogramm-Aufruf !\n'//&
         'Der Parameter <liste> vom Typ Pointerfeld\n'//&
         'ist bereits associated.\n'//&
         'Das ist nicht zulaessig !\n'//&
         '\n'//&
         '--> Unterprogrammaufruf checken !' )
    !
    ! Index 112
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(112), -1560 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(112), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Liste enthaelt ein Element mit unzulaessig langer Zeichenkette !\n'//&
         'Erlaubt sind Stringlaengen bis zu <c_listlen> Zeichen !\n'//&
         'Liste beginnt mit : \n'//&
         '<string>\n'//&
         '--> Liste korrigieren !' )
    !
    ! Index 113
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(113), -1570 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(113), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler beim Einlesen einer Liste von <dat_type>-Zahlen !\n'//&
         'Ein Listenelement schliesst mind.  ein Leerzeichen ein !\n'//&
         'ACHTUNG : Liste darf nur <dat_type>-Groessen enthalten !\n'//&
         'Liste = <list_string>\n'//&
         '\n'//&
         '-->  Einzulesende Liste checken !' )
    !
    ! Index 114
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(114), -1571 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(114), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler beim Einlesen einer Liste von logischen Groessen !\n'//&
         'Ein Listenelement schliesst mind.  ein Leerzeichen ein !\n'//&
         'Liste = <list_string>\n'//&
         '\n'//&
         '-->  Einzulesende Liste checken !' )
    !
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! Fehler, die im Modul MOD_M_FELDER auftreten :
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !
    ! Index 115
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(115), -1700 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(115), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Der Block <blockname>\n'//&
         'der Dictionary-Datei enthaelt eine FixValue-Zeile doppelt !\n'//&
         'Zeile : <key> = <wert>\n'//&
         '\n'//&
         '-->  Dictionary-Datei pruefen !' )
    !
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! Fehler, die im Modul MOD_M_LESEHILFE auftreten :
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !
    !
    ! Index 116
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(116), -1810 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(116), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'fehlerhaftes Steuerwort in Eingabedatei\n'//&
         'aktuell = <aktuell>\n'//&
         'erlaubt = [BEGINDATA,ENDFILE]\n'//&
         '\n'//&
         '--> Eingabesteuerdaten pruefen' )
    !
    ! Index 117
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(117), -1820 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(117), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Vorzeitiges Ende der Datei erreicht !\n'//&
         'Datei = <datei>\n'//&
         '\n'//&
         '--> Eingabesteuerdaten pruefen' )
    !
    ! Index 118
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(118), -1830 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(118), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Die Datei <datei> enthaelt eine\n'//&
         'unzulaessig lange Zeile !\n'//&
         'Erlaubt sind Zeilenlaengen von lediglich <zeil_len> Zeichen !\n'//&
         'Zeile beginnt mit : <zeil_anfang>\n'//&
         '--> Datei <datei> pruefen' )
    !
    ! Index 119
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(119), -1840 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(119), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Unterprogramm-Aufruf !\n'//&
         'Parameter <c_kenn> hat unzulaessigen Wert !\n'//&  ! <c_kenn> kein Platzhalter !
         '\n'//&
         '-->  check program !' )
    !
    ! Index 120
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(120), -1850 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(120), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Dictionary-Datei !\n'//&
         'Die Angabe zu einer logischen Groesse ist nicht \n'//&
         'Zeile : <key> = <wert>\n'//&
         '\n'//&
         '-->  Wert in Schluesselwort-Zeile korrigieren !' )
    !
    ! Index 121
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(121), -1860 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(121), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         '<file_typ>-Datei ist fehlerhaft !\n'//&
         'Der<block>\n'//&
         'enthaelt eine Leerzeile !\n'//&
         '\n'//&
         '--> <file_typ>-Datei pruefen !' )
    !
    ! Index 122
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(122), -1870 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(122), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehlende Angabe in <filetyp>-Datei !\n'//&
         'Der<block>\n'//&
         'enthaelt eine Eingabezeile ohne Schluesselwort !\n'//&
         '\n'//&
         '--> <filetyp>-Datei korrigieren !' )
    !
    ! Index 123
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(123), -1880 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(123), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         '<file_typ>-Datei ist fehlerhaft !\n'//&
         'Im<block>\n'//&
         'wurde das Schluesselwort <key>\n'//&
         'mehrfach spezifiziert !\n'//&
         '--> Das ist nicht zulaessig   :-[ ' )
    !
    ! Index 124
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(124), -1890 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(124), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Der<block>\n'//&
         'enthaelt ein ungueltiges Schluesselwort !\n'//&
         'aktuell = <aktuell>\n'//&
         'erlaubt = <erlaubt>\n'//&
         '--> <file_typ>-Datei pruefen !' )
    !
    ! Index 125
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(125), -1900 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(125), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehlende Angabe in <file_typ>-Datei !\n'//&
         'Im<block>\n'//&
         'wurde in der<zeil_nr> Zeile mit dem Schluesselwort\n'//&
         '"<key>" kein Wert angegeben !\n'//&
         '--> <file_typ>-Datei korrigieren !' )
    !
    ! Index 126
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(126), -1910 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(126), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Lesefehler auf logischer Kanalnummer : <lkl_nr>\n'//&
         'Datei <datei_name>\n'//&
         'beim Lesen von <xyz>\n'//&
         'zuletzt gelesene Zeile:\n'//&
         '<letzte_zeile>' )
    !
    ! Index 127
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(127), -1920 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(127), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'BEGINDATA nicht in karte vorhanden !\n'//&
         'karte(1:9) = <karte1_9>\n'//&
         'Soll       = BEGINDATA\n'//&
         '\n'//&
         '--> Eingabedaten/Aktualparameter pruefen' )
    !
    ! Index 128
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(128), -1930 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(128), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Eingabedatei !\n'//&
         'Datei enthaelt einen namenlosen DATA-Block !\n'//&
         'Damit laesst sich nix anfangen !\n'//&
         '\n'//&
         '--> Gebt der Blockstruktur einen Namen !' )
    !
    ! Index 129
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(129), -1940 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(129), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Datei !\n'//&
         'Der Name des DATA-Blockes <akt_blockname> \n'//&
         'ist zu lang !\n'//&
         'benoetigt = <act_len> Zeichen\n'//&
         'erlaubt   = <max_len> Zeichen\n'//&
         '--> Datei korrigieren !' )
    !
    ! Index 130
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(130), -1950 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(130), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehlerhafte Schluesselwortzeile in Datei !\n'//&
         'Zeile : <zeile>\n'//&
         'Die Datenzeile enthaelt kein Gleichheitszeichen,\n'//&
         'obwohl es sich um eine Schluesselwortzeile\n'//&
         '[Aufbau : Key = Wert] handeln muss !\n'//&
         '--> Datei korrigieren !' )
    !
    ! Index 131
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(131), -1960 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(131), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehlerhafte Schluesselwortzeile in Datei !\n'//&
         'Zeile : <zeile>\n'//&
         'Schluesselwort der Eingabezeile ist zu lang !\n'//&
         'benoetigt = <act_len> Zeichen\n'//&
         'erlaubt   = <max_len> Zeichen\n'//&
         '--> Datei korrigieren !' )
    !
    ! Index 132
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(132), -1970 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(132), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehlerhafte Schluesselwortzeile in Datei !\n'//&
         'Zeile : <zeile>\n'//&
         'Der Wert der Schluesselwortzeile ist zu lang !\n'//&
         'benoetigt = <act_len> Zeichen\n'//&
         'erlaubt   = <max_len> Zeichen\n'//&
         '--> Datei korrigieren !' )
    !
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! Fehler, die im Modul MOD_M_STDAT_TYPES auftreten :
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !
    ! Index 133
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(133), -2010 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(133), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Unterprogramm-Aufruf !\n'//&
         'Das Parameter-Pointerfeld <bloecke> ist\n'//&
         '.not. associated !\n'//&
         'Das ist nicht zulaessig !\n'//&
         '\n'//&
         '--> Unterprogramm-Aufruf pruefen !' )
    !
    ! Index 134
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(134), -2020 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(134), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Unterprogramm-Aufruf !\n'//&
         'Fuer das Such-Objekt wurde kein\n'//&
         'Blockname angegeben !\n'//&
         '\n'//&
         '--> Unterprogramm-Aufruf pruefen !' )
    !
    ! Index 135
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(135), -2030 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(135), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Unterprogramm-Aufruf !\n'//&
         'Fuer das Such-Objekt wurde kein\n'//&
         'Key-Name angegeben !\n'//&
         '\n'//&
         '--> Unterprogramm-Aufruf pruefen !' )
    !
    ! Index 136
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(136), -2040 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(136), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Unterprogramm-Aufruf !\n'//&
         'Fuer das Such-Objekt wurde keine\n'//&
         'Parameter-Nummer angegeben !\n'//&
         '\n'//&
         '--> Unterprogramm-Aufruf pruefen !' )
    !
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! Fehler, die im Modul MOD_M_STDAT_CHECKS auftreten :
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !
    !
    ! Index 137
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(137), -2510 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(137), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler im<block_nr> <blockname>-Block der Eingabe-Datei ! In der<key_nr> Zeile\n'//&
         'mit dem KEY <key_name> wurde <par_nr>\n'//&
         'ein ungueltiger Wert angegeben !\n'//&
         'erlaubt = <erlaubt_string>\n'//&
         '\n'//&
         '--> Eingabesteuerdaten pruefen !' )
    !
    ! Index 138
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(138), -2520 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(138), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Die Eingabesteuer-Datei ist unvollstaendig !\n'//&
         'Es fehlt ein <blockname>-Block !\n'//&
         'Das ist nicht zulaessig   :-[ \n'//&
         '\n'//&
         '--> Datei mit Eingabesteuerdaten korrigieren !' )
    !
    ! Index 139
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(139), -2530 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(139), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Die Eingabesteuer-Datei enthaelt einen unerwuenschten\n'//&
         'Eingabeblock mit Namen : <blockname> !\n'//&
         'Das ist nicht zulaessig   :-[ \n'//&
         '\n'//&
         '--> Datei mit Eingabesteuerdaten korrigieren !' )
    !
    ! Index 140
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(140), -2540 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(140), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Die Eingabesteuer-Datei ist unvollstaendig !\n'//&
         'Im<block_nr> <blockname>-Block fehlt die Zeile\n'//&
         'mit dem Schluesselwort <schluessel> !\n'//&
         'Das ist nicht zulaessig   :-[ \n'//&
         '\n'//&
         '--> Datei mit Eingabesteuerdaten korrigieren !' )
    !
    ! Index 141
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(141), -2550 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(141), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Eingabesteuer-Datei !\n'//&
         'Der<block_nr> <blockname>-Block enthaelt eine unzulaessige\n'//&
         'Eingabe-Zeile mit dem Schluesselwort <schluessel> !\n'//&
         'Das ist nicht zulaessig   :-[ \n'//&
         '\n'//&
         '--> Datei mit Eingabesteuerdaten korrigieren !' )
    !
    ! Index 142
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(142), -2560 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(142), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Die Eingabesteuer-Datei ist unvollstaendig ! Im<block_nr> <blockname>-Block\n'//&
         'fehlt in der<zeilen_nr> Zeile mit dem Schluesselwort <schluessel>\n'//&
         'die Angabe eines Wertes fuer den <par_nr>. Eingabeparameter !\n'//&
         'Das ist nicht zulaessig   :-[ \n'//&
         '\n'//&
         '--> Datei mit Eingabesteuerdaten korrigieren !' )
    !
    ! Index 143
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(143), -2570 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(143), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Die Eingabesteuer-Datei ist fehlerhaft ! Im<block_nr> <blockname>-Block\n'//&
         'enthaelt die<zeilen_nr> Zeile mit dem Schluesselwort <schluessel>\n'//&
         'die Angabe eines Wertes fuer den <par_nr>. Eingabeparameter !\n'//&
         'Das ist nicht zulaessig   :-[ \n'//&
         '\n'//&
         '--> Datei mit Eingabesteuerdaten korrigieren !' )
    !
    ! Index 144
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(144), -2580 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(144), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler im<block_nr> <blockname>-Block der Eingabe-Datei !\n'//&
         'In <einer> der Zeile<n> mit dem KEY <keyname> \n'//&
         'wurde <fuer_par_x> ein Wert angegeben,\n'//&
         'welcher <vgl_op> <als_wert_y> ist !\n'//&
         'Das ist nicht zulaessig !\n'//&
         '--> Eingabesteuerdaten korrigieren !' )
    !
    ! Index 145
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(145), -2590 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(145), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler im<block_nr> <blockname>-Block der Eingabe-Datei !\n'//&
         'In <einer> der Zeile<n> mit dem KEY <keyname>\n'//&
         'wurde <fuer_par_x> ein Wert angegeben,\n'//&
         'der <vgl_op> Wert <als_wert_y>\n'//&
         'des KEYs <vgl_key> in Block <vgl_block> ist !\n'//&
         'Das ist nicht zulaessig !\n'//&
         '--> Eingabesteuerdaten korrigieren !' )
    !
    ! Index 146
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(146), -2600 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(146), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'Fehler in Unterprogramm-Aufruf !\n'//&
         'Das Parameter-Pointerfeld par%wert ist\n'//&
         '.not. associated !\n'//&
         'Das ist nicht zulaessig !\n'//&
         '\n'//&
         '--> Unterprogramm-Aufruf pruefen !' )
    !
    ! Index 147
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(147), -2610 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(147), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'PROGRAM-INPUT-FILE wird vermisst !\n'//&
         'Path = <path>\n'//&
         'File = <file>\n'//&
         '... required, but not there \n'//&
         '\n'//&
         '--> go and look for the file' )
    !
    ! Index 148
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(148), -2620 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(148), &
         'Fehlerkategorie: PAKETSPEZIFISCHE-Methoden\n'//&
         'PROGRAM-OUTPUT-FILE bereits existent !\n'//&
         'Path = <path>\n'//&
         'File = <file>\n'//&
         '... should be new, but is already there\n'//&
         '\n'//&
         '--> go and remove the file ' )
    !
    ! Index 149
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(149), -350 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(149), &
         'Fehlerkategorie: MODULSPEZIFISCHE-Methoden\n'//&
         'Dictionary-Datei ist weder in DICDIR noch im Arbeitsverzeichnis zu finden !\n'//&
         'DICDIR = <dic_path>\n'//&
         'Datei  = <dic_file>\n'//&
         '--> Umgebungsvariable DICDIR mit passenden Pfad setzen \n'// &
         '    (auf einem WIN-PC z.B. in System -> Erweitert -> Umgebungsvariablen) \n' // &
         '--> oder alternativ die Datei ins Arbeitsverzeichnis kopieren')
    !
    ! Obacht(!) auf "c_nofallerrors" geben
    !
  END SUBROUTINE init_dictionary_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/Re-Initialisieren aller Fehlermeldungen des Moduls
  SUBROUTINE clear_dictionary_all_errors &
       ( )
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER  :: c_upname='clear_dictionary_all_errors' !
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_dictionary_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! --> nicht benoetigte SETUP-Methoden bitte unbedingt entfernen <-------
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> modulspezifische PRIVATE-GET-Methoden <<< [ERR_NO = -1 bis -999]
  ! ----------------------------------------------------------------------
  !
  !! Ermittelt den vollstaendigen Namen der
  !! Dictionary-Datei, d.h. inklusive eventueller Pfadangabe
  !! der Enviromentvariable $DICDIR. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE getdictionfile &
       ( nam_dic, dic_file )
    !
    ! USE-Statements
    !
    ! Basismodul um Systembefehle abzusetzen
    ! hier : Enviromentvariablen $DICDIR abfragen
    !
    USE b_cmds, ONLY : &
         ! Routine
         getenvv_cmds   ! Enviromentvariablen abfragen
    !
    ! BASIS-Modul fuer das File-Handling
    !
    USE b_file, ONLY :  &
       ! Routinen
       get_file_name,   &
       set_file_path_and_name
    !
    ! Formalparameter
    !! Name der Dictionary-Datei
    CHARACTER (LEN=*),    INTENT(IN)            :: nam_dic
    !! Dictionary-Datei
    TYPE (t_file),        INTENT(INOUT)           :: dic_file
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='getdictionfile'
    !! Hilfsstrings fuer Fehlermeldung
    CHARACTER (LEN=6)             :: c_actlen, c_maxlen
    !
    CHARACTER (LEN=240)                     :: filename, dicpath ! 
    INTEGER                                 :: il, jl
    LOGICAL                                 :: ex
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       ! Pruefe, ob die Datei lokal vorhanden ist
       !
       INQUIRE (FILE=TRIM(nam_dic), EXIST=ex)
       !
       ! Falls die Datei lokal vorhanden ist, wird aus dieser gelesen; falls
       ! dies nicht der Fall ist wird aus der Standard-Dictionary-Datei
       ! in $DICDIR gelesen
       !
       filename = REPEAT(' ', LEN(filename))
       dicpath  = REPEAT(' ', LEN(dicpath ))
       !
       IF (ex) THEN
          !
          filename = TRIM(nam_dic)
          !
       ELSE
          !
          ! ... ermittle Environment-Variable fuer $DICDIR
          !
          dicpath  = getenvv_cmds ( 'DICDIR' )
          !
          IF ( no_error( ) ) THEN
             !
             il = LEN_TRIM( TRIM(dicpath)//'/'//TRIM(nam_dic) )
             jl = LEN(filename)
             !
             IF (il .GT. jl) THEN
                !
                ! Fehler -340 : Name der Dictionary-Datei ist unzulaessig lang !#
                !
                c_actlen = REPEAT(' ',LEN(c_actlen))
                c_maxlen = REPEAT(' ',LEN(c_maxlen))
                !
                WRITE(c_actlen,'(I5)') il
                WRITE(c_maxlen,'(I5)') jl
                !
                CALL setup_error_act ( all_errors(:), -340, c_upname, c_modname )
                CALL setup_error_act ( '<dic_name>', TRIM( TRIM(dicpath)//'/'//TRIM(nam_dic) ) )
                CALL setup_error_act ( '<act_len>', TRIM(c_actlen) )
                CALL setup_error_act ( '<max_len>', TRIM(c_maxlen) )
                !
             ELSE
                !
                filename = TRIM( TRIM(dicpath)//'/'//TRIM(nam_dic) )
                INQUIRE (FILE=filename, EXIST=ex)
                !
                ! FM generieren, dass die Datei nicht vorhanden ist
                IF(.NOT. ex) THEN
                   CALL setup_error_act ( all_errors(:), -350, c_upname, c_modname )
                   CALL setup_error_act ( '<dic_path>', TRIM(dicpath) )
                   CALL setup_error_act ( '<dic_file>', TRIM(nam_dic) )
                   !
                END IF
             END IF
             !
          END IF ! no_error( )
          !
       END IF
       !
       ! Jetzt Namen der Dictionary-Datei umspeichern
       !
       IF ( no_error( ) ) THEN
          !
          il = LEN_TRIM(filename)
          jl = LEN( get_file_name(dic_file) )
          !
          ! ... Stringlaenge ueberpruefen
          !
          IF (il .GT. jl) THEN
             !
             !
             ! Fehler -340 : Name der Dictionary-Datei ist unzulaessig lang !
             !
             c_actlen = REPEAT(' ',LEN(c_actlen))
             c_maxlen = REPEAT(' ',LEN(c_maxlen))
             !
             WRITE(c_actlen,'(I5)') il
             WRITE(c_maxlen,'(I5)') jl
             !
             CALL setup_error_act ( all_errors(:), -340, c_upname, c_modname )
             CALL setup_error_act ( '<dic_name>', TRIM(filename) )
             CALL setup_error_act ( '<act_len>', TRIM(c_actlen) )
             CALL setup_error_act ( '<max_len>', TRIM(c_maxlen) )
             !
          ELSE
             !
             CALL set_file_path_and_name ( dic_file, TRIM(filename) )
             !
          END IF
          !
       END IF
       !
    END IF ! ok_initialised
    !
  END SUBROUTINE getdictionfile
  !
  !! Fuer das Einlesen einer Blockstruktur notwendige Daten
  !! werden aus Variable <block_A> nach Var. <block_B>
  !! kopiert. 
  !! Noetig, wenn Eingabebloecke zunaechst in eine 
  !! verkettete Liste mit Elementen vom Typ <t_block>
  !! gelesen werden.
  SUBROUTINE transfer_einleseinfo &
       ( block_A, block_B )
    !
    ! USE-Statements
    !
    USE m_stdat_types, ONLY : &
         init_key , &
         init_par
    !
    ! Formalparameter
    !! Variable mit Info-Daten zu einer Blockstruktur
    TYPE (t_block  )   , INTENT(IN   )    :: block_A
    !! 
    TYPE (t_block  )   , INTENT(INOUT)    :: block_B
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='transfer_einleseinfo'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    INTEGER   :: i, j
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       block_B%Name      = block_A%Name
       block_B%L_Single  = block_A%L_Single
       block_B%L_Opt     = block_A%L_Opt

       block_B%KeyAnz    = block_A%KeyAnz
       !
       ALLOCATE(block_B%Key(SIZE(block_A%Key)), STAT=stat )
       !
       IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
          !
          CALL setup_error_act &
                ( all_errors(:), -10000, c_upname, c_modname, stat )
          CALL setup_error_act ( '<felder>', 'block_B%Key' )
          !
       END IF
       !
       IF ( no_error( ) ) THEN
          !
          DO i = 1, SIZE(block_B%Key)

             IF ( no_error( ) ) CALL init_key ( block_B%Key(i) )

             IF ( no_error( ) ) THEN 
                !
                block_B%Key(i)%Name        = block_A%Key(i)%Name
                block_B%Key(i)%L_Single    = block_A%Key(i)%L_Single
                block_B%Key(i)%L_OneArray  = block_A%Key(i)%L_OneArray
                block_B%Key(i)%L_Comment   = block_A%Key(i)%L_Comment
                block_B%Key(i)%L_Opt       = block_A%Key(i)%L_Opt

                block_B%Key(i)%ParAnz      = block_A%Key(i)%ParAnz
                !
                ALLOCATE(block_B%Key(i)%Par(SIZE(block_A%Key(i)%Par)), &
                     STAT=stat )
                !
                IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
                   !
                   CALL setup_error_act &
                         ( all_errors(:), -10000, c_upname, c_modname, stat )
                   CALL setup_error_act ( '<felder>', 'block_B%Key(i)%Par' )
                   !
                END IF
                !
             END IF

             IF ( no_error( ) ) THEN 
                !
                DO j = 1, SIZE(block_B%Key(i)%Par)

                   IF ( no_error( ) ) CALL init_par ( block_B%Key(i)%Par(j) )

                   IF ( no_error( ) ) THEN
                      !
                      block_B%Key(i)%Par(j)%ParPos   = block_A%Key(i)%Par(j)%ParPos
                      block_B%Key(i)%Par(j)%Type     = block_A%Key(i)%Par(j)%Type
                      block_B%Key(i)%Par(j)%L_Opt    = block_A%Key(i)%Par(j)%L_Opt
                      block_B%Key(i)%Par(j)%L_Array  = block_A%Key(i)%Par(j)%L_Array
                      !
                   END IF

                END DO
                !
             END IF
             !
          END DO
          !
       END IF ! no_error( )   
       !
    END IF ! ok_initialised
    !
  END SUBROUTINE transfer_einleseinfo
  !
  !! Liest einen Block der Eingabesteuerdatei
  !! Die Zeilen-Werte werden als Character-String in das,
  !! dem jeweiligen Schluesselwort zugehoerige, dynamisch
  !! allokierte Komponenten-Feld einsortiert. 
  SUBROUTINE read_eingabeblock &
       ( dat2read, block, egb_nr )
    !
    ! USE-Statements
    !
    USE m_lesehilfe, ONLY : &
         readkarte,         &
         read_keyzeile
    !
    ! Formalparameter
    !! zu lesende Steuerdatei
    TYPE (t_file )   , INTENT(IN   )     :: dat2read 
    !! Eingabeblock-Variable
    TYPE (t_block)     , INTENT(INOUT)   :: block
    !! Nummer des Eingabeblockes
    INTEGER            , INTENT(IN   )   :: egb_nr
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='read_eingabeblock'
    !! Statusvariable
    INTEGER :: stat ! 

    CHARACTER (LEN=40)               :: ftext

    CHARACTER (LEN=line_len)         :: karte

    LOGICAL                          :: doit_block

    INTEGER                          :: i, j

    CHARACTER (LEN=key_len)          :: key
    CHARACTER (LEN=line_len)         :: wert
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       ftext = 'Lesen Daten im '//TRIM(block%Name)//'-Block'

       NULLIFY (fst_zeil)
       NULLIFY (act_zeil)
       NULLIFY (lst_zeil)
       NULLIFY (vgl_zeil)

!      ---------------------------------------------------------------------------
!      Allokieren Key(i)%ZeilAnz & Key(i)%Par(j)%EGB auf SIZE = 1,
!      da an dieser Stelle stets nur ein einzelner Eingabeblock verarbeitet 
!      wird (Element der verketteten Liste zum Lesen der Eingabebloecke)

       block%EGBAnz = 1

       DO i = 1, SIZE(block%Key)

          IF ( no_error( ) ) THEN
             !
             ! ZeilAnz-Komponentenfeld von t_key
             ALLOCATE ( block%Key(i)%ZeilAnz(1), STAT=stat )
             !
             IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
                !
                CALL setup_error_act &
                      ( all_errors(:), -10000, c_upname, c_modname, stat )
                CALL setup_error_act ( '<felder>', 'block%Key(i)%ZeilAnz' )
                !
             END IF
             !
          END IF
          !
          IF ( no_error( ) ) THEN
             !
             ! Initialisierung des KeyZeilenZaehlers
             !
             block%Key(i)%ZeilAnz(1) = 0
             !
             ! EGB-Komponentenfeld von t_par
             !
             DO j = 1, SIZE(block%Key(i)%Par)
                !
                IF ( no_error( ) ) THEN
                   !
                   ALLOCATE ( block%Key(i)%Par(j)%EGB(1), STAT=stat )
                   !
                   IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
                      !
                      CALL setup_error_act &
                            ( all_errors(:), -10000, c_upname, c_modname, stat )
                      CALL setup_error_act ( '<felder>', 'block%Key(i)%Par(j)%EGB' )
                      !
                   END IF
                   !
                END IF
                !
                IF ( no_error( ) ) NULLIFY (block%Key(i)%Par(j)%EGB(1)%Wert)

             END DO
             !
          END IF ! no_error( )
          !
       END DO ! i = 1, SIZE(block%Key)
       !

   !   ---------------------------------------------------------------------------
   !   Lesen der Blockzeilen

       IF ( no_error( )  .AND.  l_wri ) THEN

          WRITE(*,*) &
               ' read BLOCK ', TRIM(block%Name), ' der Eingabe-Datei'
       END IF


       doit_block = .true.

       loop : DO
          !
          IF ( any_error( )  .OR.  .NOT. doit_block ) EXIT loop
          !      
          IF ( no_error( ) ) CALL readkarte ( dat2read, ftext, karte )
          !      
          IF ( no_error( ) ) CALL read_keyzeile &
                                   ( 'Eingabe', TRIM(block%Name), block, &
                                      karte, key, wert, doit_block, egb_nr)
          !
          IF ( no_error( )  .AND.  doit_block ) THEN
             !
             CALL birth_vl_kz &
                  ( fst_zeil, lst_zeil, act_zeil)
             !
             IF ( no_error( ) ) THEN
                act_zeil%key  = TRIM(key)
                act_zeil%wert = TRIM(wert)
             END IF
             !
          END IF
          !      
       END DO loop
       !
       ! Checken der eingelesenen Daten :
       !  - Gabs ueberhaupt ne Schluesselwort-Zeile ?
       !  - Gabs ne Zeile doppelt ? 
       !  - Alle notwendigen Eingabezeilen vorhanden ?
       !
       IF ( no_error( ) ) CALL check_eingabeblock ( block, egb_nr )
       !
       ! Allokieren der KomponentenFelder der Variablen vom Typ t_block und
       ! Eintragen der gelesenen Werte in die entsprechenden Komponenten
       !
       IF ( no_error( ) ) CALL alloc_eingabeblock ( block, egb_nr)
       !
       IF ( no_error( )  .AND.  l_wri ) THEN
          !
          WRITE(*,*) ' done'
          !
       END IF
       !
    END IF ! ok_initialised
    !
  END SUBROUTINE read_eingabeblock
  !
  !! Ueberprueft die in einem Eingabeblock enthaltenen 
  !! Angaben, hinsichtlich folgender Kriterien :
  !!    - Gabs ueberhaupt ne Schluesselwort-Zeile ?
  !! (  - Gabs ne Zeile doppelt ?  )
  !!    - Alle notwendigen Eingabezeilen vorhanden ?
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE check_eingabeblock &
       ( block, egb_nr )
    !
    ! Formalparameter
    !! Variable ..
    TYPE (t_block  )   , INTENT(IN)    :: block
    !! Nummer des aktuellen Eingabeblockes
    INTEGER            , INTENT(IN)    :: egb_nr
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='check_eingabeblock'
    !
    INTEGER                            :: i
    LOGICAL                            :: l_dop
    CHARACTER (LEN=6)                  :: c_egb
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       ! Ggf.: String mit Eingabeblock-Nummer basteln
       !
       c_egb = REPEAT(' ',LEN(c_egb))
       IF ( .NOT. block%L_Single) THEN
          WRITE(c_egb,'(I4)') egb_nr
          c_egb  = ADJUSTL(c_egb)
          c_egb  = ' '//TRIM(c_egb)//'.'
       END IF
       !
       ! Fehler -80 : Der Block enthaelt keine Eingabezeile
       !
       IF ( .NOT. ASSOCIATED(fst_zeil) ) THEN
          !
          CALL setup_error_act ( all_errors(:), -80, c_upname, c_modname )
          CALL setup_error_act ( '<file_name>', TRIM(infile_name) )
          CALL setup_error_act ( '<nummer>', TRIM(c_egb) )
          CALL setup_error_act ( '<blockname>', TRIM(block%Name) )
          !
          RETURN
          !
       END IF
       !
       !   Ggf.: Check, ob der Block eine Eingabezeile doppelt enthaelt ?
       !   Wenn l_dop = True, dann darf der Block Eingabezeilen mit demselben
       !                      Wert doppelt enthalten
       !
       !   !! SPAETER : Als Attribut der Keyzeilen realisieren !!
       !
       l_dop = .True.
       !
       IF ( no_error( )  .AND.  .NOT. l_dop ) THEN

          act_zeil => fst_zeil

          outer: DO

             IF ( any_error( )  .OR.  .NOT. ASSOCIATED(act_zeil) ) EXIT outer

             vgl_zeil => act_zeil%next

             inner: DO 

                IF ( any_error( )  .OR.  .NOT. ASSOCIATED(vgl_zeil) ) EXIT inner
             
                IF ( TRIM(act_zeil%key)  .EQ. TRIM(vgl_zeil%key) .AND. &
                     TRIM(act_zeil%wert) .EQ. TRIM(vgl_zeil%wert)        ) THEN
                   !
                   ! Fehler -90 : Block enthaelt Eingabezeile doppelt !
                   !
                   CALL setup_error_act ( all_errors(:), -90, c_upname, c_modname )
                   CALL setup_error_act ( '<file_name>', TRIM(infile_name) )
                   CALL setup_error_act ( '<nummer>', TRIM(c_egb) )
                   CALL setup_error_act ( '<blockname>', TRIM(block%Name) )
                   !
                   RETURN
                !   
                END IF
                
                vgl_zeil => vgl_zeil%next

             END DO inner

             act_zeil => act_zeil%next

          END DO outer
          !
       END IF   ! no_error( )  .AND.  .NOT. l_dop
       !
       ! Check : Sind alle erforderlichen Schluesselwort-Zeilen ( l_opt=False )
       !         vorhanden ?
       !
       addr : DO i = 1, SIZE(block%Key)
          !
          IF ( any_error( ) ) EXIT addr
          !
          IF ( block%Key(i)%ZeilAnz(1)  .EQ. 0   .AND. &
               .NOT. block%Key(i)%L_Opt              ) THEN
             !
             ! Fehler -100 : Eine erforderliche Schluesselwort-Zeile fehlt !
             !
             CALL setup_error_act ( all_errors(:), -100, c_upname, c_modname )
             CALL setup_error_act ( '<file_name>', TRIM(infile_name) )
             CALL setup_error_act ( '<nummer>', TRIM(c_egb) )
             CALL setup_error_act ( '<blockname>', TRIM(block%Name) )
             CALL setup_error_act ( '<schluessel>', TRIM(block%Key(i)%Name) )
             !
             RETURN
             !
          END IF

       END DO addr
       !
    END IF ! ok_initialised
    !
  END SUBROUTINE check_eingabeblock
  !
  !! Allokiert die KomponentenFelder der Variablen vom
  !! Typ <t_block> und uebertraegt die gelesenen Werte in
  !! die entsprechenden Komponenten.
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_eingabeblock &
       ( block, egb_nr )
    !
    ! USE-Statements
    !
    USE b_error, ONLY :   &
         ! Routinen
         specific_error
    !
    USE m_stdat_types, ONLY : &
         ! Routinen
         init_feld
    !
    USE m_stringliste, ONLY : &
         ! Routinen
         read_liste
    !
    USE m_lesehilfe, ONLY : &
         ! Routinen
         Time_Probelesen
    !
    ! Formalparameter
    !! Variable ...
    TYPE (t_block  )   , INTENT(INOUT)            :: block
    !! Nummer des aktuellen Eingabeblockes
    INTEGER            , INTENT(IN)               :: egb_nr
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='alloc_eingabeblock'
    !! Statusvariable
    INTEGER :: stat !
    !! Lesefehler
    LOGICAL :: l_read_error = .False.
    !
    INTEGER                                            :: i, j, k, anz
    LOGICAL                                            :: l_sort
    CHARACTER (LEN=key_len) , DIMENSION(:) , POINTER   :: c_liste
    INTEGER                                            :: min
    CHARACTER (LEN=line_len)                           :: string

    CHARACTER (LEN=4)                                  :: c_lencom
    CHARACTER (LEN=6)                                  :: c_egb, c_zeil, c_par
    CHARACTER (LEN=13)                                 :: c_dtyp
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       ! Initialisierungen
       !
       NULLIFY(c_liste)
       c_dtyp = REPEAT(' ', LEN(c_dtyp))
       !
       ! Fuer evtl. Fehlermeldung : String mit Eingabeblock-Nummer basteln
       !
       c_egb = REPEAT(' ',LEN(c_egb))
       IF ( .NOT. block%L_Single) THEN
          WRITE(c_egb,'(I4)') egb_nr
          c_egb  = ADJUSTL(c_egb)
          c_egb  = ' '//TRIM(c_egb)//'.'
       END IF
       !
       !   Speicherfelder allokieren
       !
       DO i = 1, SIZE(block%Key)

          IF ( no_error( )  .AND.  block%Key(i)%ZeilAnz(1)  .GT. 0 ) THEN

             DO j = 1, SIZE(block%Key(i)%Par)
                !
                ! Wert-Feld allokieren & initialisieren
                !
                anz = block%Key(i)%ZeilAnz(1)

                ALLOCATE ( block%Key(i)%Par(j)%EGB(1)%Wert(anz), STAT=stat )
                !
                IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
                   !
                   CALL setup_error_act &
                         ( all_errors(:), -10000, c_upname, c_modname, stat )
                   CALL setup_error_act ( '<felder>', 'block%Key%Par%EGB%Wert' )
                   !
                END IF
                !
                IF ( no_error( ) ) THEN
                   !
                   DO k = 1, SIZE(block%Key(i)%Par(j)%EGB(1)%Wert)
                
                      IF ( no_error( ) ) CALL init_feld( block%Key(i)%Par(j)%EGB(1)%Wert(k) )

                   END DO
                   !
                END IF
                !
             END DO   ! Loop ueber alle Parameter des Keys
             !
             ! KeyZeilenZaehler nochmal auf Null zuruecksetzen, denn er
             ! dient beim Einsortieren der KeyWerte nochmal als Zaehler
             !
             IF ( no_error( ) ) block%Key(i)%ZeilAnz(1) = 0    
             !
          END IF   ! block%Key(i)%ZeilAnz(1)  .GT. 0

       END DO   ! Loop ueber alle Keys des Blocks
       !
       ! Elemente der verketteten Liste mit den ZeilenWerten werden nun in die
       ! dynamischen Komponenten-Felder der Variablen vom Typ <t_block> uebertragen
       ! 
       !  ... erstes Listenelement
       !
       act_zeil => fst_zeil
       
       !  ... Schleife ueber alle Listenelemente

       vlist : DO
       
          IF ( any_error( )  .OR.  .NOT. ASSOCIATED(act_zeil) ) EXIT vlist

          ! ... Einsortieren in das KomponentenFeld

          l_sort = .false.

          keyfeld : DO i = 1, SIZE(block%Key)

             IF ( any_error( )  .OR.  l_sort ) EXIT keyfeld
             
             IF ( act_zeil%key .EQ. block%Key(i)%Name ) THEN

                l_sort = .true.

                block%Key(i)%ZeilAnz(1) = block%Key(i)%ZeilAnz(1) + 1
            
                ! Fuer evtl. Fehlermeldung : String mit Keyzeilen-Nummer basteln
                c_zeil = REPEAT(' ',LEN(c_zeil))
                IF ( .NOT. block%Key(i)%L_Single) THEN
                   WRITE(c_zeil,'(I4)') block%Key(i)%ZeilAnz(1)
                   c_zeil  = ADJUSTL(c_zeil)
                   c_zeil  = ' '//TRIM(c_zeil)//'.'
                END IF


                IF ( block%Key(i)%L_Comment ) THEN
                ! --> KeyZeile ist als Kommentarzeile vereinbart

                   ALLOCATE (c_liste(1), STAT=stat )
                   !
                   IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
                      !
                      CALL setup_error_act &
                            ( all_errors(:), -10000, c_upname, c_modname, stat )
                      CALL setup_error_act ( '<felder>', 'c_liste' )
                      !
                   END IF
                   !
                   IF ( no_error( ) ) THEN
                      !
                      IF ( LEN_TRIM(act_zeil%wert) .GT. LEN(c_liste(1)) ) THEN
                         !
                         ! Fehler -110 : Stringlaenge von Kommentar-Angabe zu lang !
                         !
                         c_lencom = REPEAT(' ',LEN(c_lencom))
                         WRITE(c_lencom,'(I4)') LEN(c_liste(1))
                         c_lencom  = ADJUSTL(c_lencom)
                         !
                         CALL setup_error_act ( all_errors(:), -110, c_upname, c_modname )
                         CALL setup_error_act ( '<file_name>', TRIM(infile_name) )
                         CALL setup_error_act ( '<block_nr>', TRIM(c_egb) )
                         CALL setup_error_act ( '<blockname>', TRIM(block%Name) )
                         CALL setup_error_act ( '<zeile_nr>', TRIM(c_zeil) )
                         CALL setup_error_act ( '<schluessel>', TRIM(block%Key(i)%Name) )
                         CALL setup_error_act ( '<kom_len>', TRIM(c_lencom) )
                         !
                         RETURN
                         !
                      ELSE
                         c_liste(1) = TRIM(act_zeil%wert)
                      END IF
                      !
                   END IF
                   !
                   !
                ELSE
                ! --> KeyZeile ist keine Kommentarzeile
                   !
                   ! Splitte Keywert in einzelne Angaben-Strings
                   !
                   CALL read_liste ( TRIM(act_zeil%wert), ' ', c_liste, c_delim, c_repla ) ! +GL+
                   !
                   ! Checken : Angaben-Anzahl
                   !
                   IF ( no_error( ) ) CALL Check_AngabenAnzahl &
                                            ( TRIM(block%Name), block%Key(i), &
                                              TRIM(act_zeil%wert), SIZE(c_liste) )
                   !
                END IF
                !
                !
                IF ( no_error( ) ) THEN
                   !
                   ! Read : Parameter-Werte
                   !
                   DO j = 1, MIN( SIZE(c_liste) , SIZE(block%Key(i)%Par) )
                      !
                      IF ( any_error( ) ) EXIT
                      !
                      string = REPEAT(' ', LEN(string))

                      IF ( .NOT. block%Key(i)%Par(j)%L_Array ) THEN

                         string = TRIM(c_liste(j))

                      ELSE
                   
                         DO k= j, SIZE(c_liste)
                            IF ( k .EQ. j ) THEN

                               string = TRIM(c_liste(j))

                            ELSE

                               string = TRIM(string)//' '//TRIM(c_liste(k))

                            END IF

                         END DO
                   
                      END IF

                      anz = block%Key(i)%ZeilAnz(1)

                      SELECT CASE (block%Key(i)%Par(j)%Type)
                      CASE ('INT')
                         !   
                         CALL read_liste  &
                              ( TRIM(string), ' ', &
                                block%Key(i)%Par(j)%EGB(1)%Wert(anz)%int )
                         !
                         IF ( any_error( ) ) THEN
                            !
                            IF ( specific_error(-1510) ) THEN
                               l_read_error = .True.
                               c_dtyp = 'INTEGER'
                            END IF
                            !
                         END IF
                         !
                      CASE ('REAL')
                         !
                         CALL read_liste  &
                              ( TRIM(string), ' ', &
                                block%Key(i)%Par(j)%EGB(1)%Wert(anz)%real )
                         !
                         IF ( any_error( ) ) THEN
                            !
                            IF ( specific_error(-1510) ) THEN
                               l_read_error = .True.
                               c_dtyp = 'REAL'
                            END IF
                            !
                         END IF
                         !
                      CASE ('LOG')
                         !
                         CALL read_liste  &
                               ( TRIM(string), ' ', &
                                 block%Key(i)%Par(j)%EGB(1)%Wert(anz)%log )
                         !
                         IF ( any_error( ) ) THEN
                            !
                            IF ( specific_error(-1520) ) THEN
                               l_read_error = .True.
                               c_dtyp = 'LOGICAL'
                            END IF
                            !
                         END IF
                         !
                      CASE ('CHAR','DATE','INCR','FILE')
                         !
                         IF ( block%Key(i)%L_Comment ) THEN
                            !
                            ALLOCATE (block%Key(i)%Par(j)%EGB(1)%Wert(anz)%char(1), &
                                 STAT=stat )
                            !
                            IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
                               !
                               CALL setup_error_act &
                                     ( all_errors(:), -10000, c_upname, c_modname, stat )
                               CALL setup_error_act &
                                     ( '<felder>', 'block%Key(i)%Par(j)%EGB(1)%Wert(anz)%char' )
                               !
                            END IF
                            !
                            IF ( no_error( ) ) &
                                 block%Key(i)%Par(j)%EGB(1)%Wert(anz)%char(1) = TRIM(c_liste(1))
                            !
                         ELSE
                            !
                            CALL read_liste  &
                                 ( TRIM(string), ' ', &
                                 block%Key(i)%Par(j)%EGB(1)%Wert(anz)%char )
                            !
                            CALL replace_repla_in_string ( c_repla, c_sign, & ! +GL+
                                 block%Key(i)%Par(j)%EGB(1)%Wert(anz)%char )
                            ! 
                         END IF
                         !
                         IF ( no_error( ) ) THEN
                            !
                            SELECT CASE (block%Key(i)%Par(j)%Type)
                            CASE ('DATE')

                               CALL Time_Probelesen &
                                    ( 'DATE', &
                                       block%Key(i)%Par(j)%EGB(1)%Wert(anz)%char)

                            CASE ('INCR')

                               CALL Time_Probelesen &
                                    ( 'INCR', &
                                      block%Key(i)%Par(j)%EGB(1)%Wert(anz)%char)

                            END SELECT
                            !
                         END IF
                         !
                      CASE ('DOUBLE')
                         !
                         CALL read_liste  &
                              ( TRIM(string), ' ', &
                                block%Key(i)%Par(j)%EGB(1)%Wert(anz)%doub )
                         !
                         IF ( any_error( ) ) THEN
                            !
                            IF ( specific_error(-1510) ) THEN
                               l_read_error = .True.
                               c_dtyp = 'REAL (Double)'
                            END IF
                            !
                         END IF
                         !
                      END SELECT ! Type
                      !
                      ! Fehlermeldung bei Lesefehler der Datentypen 'INT','REAL','DOUBLE' und
                      ! 'LOG' editieren
                      IF ( l_read_error ) THEN
                         !
                         ! Fehler -120 : Lesefehler !
                         !
                         ! ... String mit Parameter-Positionsnummer basteln
                         c_par = REPEAT(' ',LEN(c_par))
                         IF ( SIZE(block%Key(i)%Par) .GT. 1 ) THEN
                            WRITE(c_par,'(I4)') j
                            c_par  = ADJUSTL(c_par)
                            c_par  = ' '//TRIM(c_par)//'.'
                         END IF
                         !
                         CALL setup_error_act ( all_errors(:), -120, c_upname, c_modname )
                         CALL setup_error_act ( '<file_name>', TRIM(infile_name) )
                         CALL setup_error_act ( '<block_nr>', TRIM(c_egb) )
                         CALL setup_error_act ( '<blockname>', TRIM(block%Name) )
                         CALL setup_error_act ( '<zeile_nr>', TRIM(c_zeil) )
                         CALL setup_error_act ( '<schluessel>', TRIM(block%Key(i)%Name) )
                         CALL setup_error_act ( '<par_nr>', TRIM(c_par) )
                         CALL setup_error_act ( '<datentyp>', TRIM(c_dtyp) )
                         !
                         RETURN
                         !
                      END IF
                      !
                   END DO  ! j = 1, MIN( SIZE(c_liste) , SIZE(block%Key(i)%Par) )
                   !
                END IF ! no_error( )
                !
                IF ( no_error( ) ) THEN
                   !
                   ! Deallocate des lokalen Feldes c_liste
                   !
                   DEALLOCATE ( c_liste, STAT=stat )
                   !
                   IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Deallocate
                      !
                      CALL setup_error_act &
                            ( all_errors(:), -20000, c_upname, c_modname, stat )
                      CALL setup_error_act &
                            ( '<felder>', 'c_liste' )
                      !
                   END IF
                   !
                END IF

                IF ( no_error( ) ) NULLIFY (c_liste)
                !
                !
             END IF   ! act_zeil%key .EQ. block%Key(i)%Name
             !
          END DO keyfeld
          !
          IF ( no_error( ) ) THEN
             !
             ! ... naechste Datei initialisieren
             !
             lst_zeil => act_zeil
             act_zeil => act_zeil%next
             !
             ! ... Deallokieren des wegsortierten Listenelementes
             DEALLOCATE (lst_zeil)
             !
          END IF 
          !       
       END DO vlist
       !
       IF ( no_error( ) ) THEN
          !
          ! Pointerverbindungen loesen
          !
          NULLIFY (fst_zeil)
          NULLIFY (act_zeil)
          NULLIFY (lst_zeil)
          NULLIFY (vgl_zeil)
          !
       END IF
       !
    END IF ! ok_initialised
    !
  END SUBROUTINE alloc_eingabeblock
  !
  !! UP checkt die Anzahl an Eingaben auf einer Zeile
  !! stimmig ist. Optionale und Array-Parameter muessen
  !! dabei insbesondere beruecksichtigt werden.
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE Check_AngabenAnzahl &
       ( blockname, key, wert, anz_angabe )
    !
    ! Formalparameter
    !! Name des aktuellen Blockes
    CHARACTER (LEN=*)                , INTENT(IN   )     :: blockname
    !! Variable fuer die Beschreibung eines Schluesselwortes
    TYPE (t_key)                     , INTENT(IN   )     :: key
    !! Wert einer Schluesselwortzeile
    CHARACTER (LEN=*)                , INTENT(IN   )     :: wert
    !! Anzahl an Angaben in einer Eingabezeile
    INTEGER                          , INTENT(IN   )     :: anz_angabe
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='Check_AngabenAnzahl'
    !
    INTEGER                                :: i
    INTEGER                                :: min, max
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       ! Check : Angaben-Anzahl
       !
       min = 0
       max = 0

       DO i = 1, SIZE(key%Par)

          IF ( key%Par(i)%L_Array ) THEN

             ! Bemerkung : L_Array = True nur moeglich
             ! bei letztem Parameter daher kann max hier = 0
             ! gesetzt werden

             max = 0

             IF ( .NOT. key%Par(i)%L_Opt ) THEN

                min = min + 1

             END IF

          ELSE

             IF ( key%Par(i)%L_Opt ) THEN

                max = max + 1   

             ELSE

                min = min + 1
                max = max + 1   

             END IF
                                      
          END IF

       END DO

       IF ( max .GT. 0 ) THEN

          IF ( anz_angabe .GT. max ) THEN
             !
             ! Fehler -130 : Eine Keyzeile enthaelt zuviele Angaben !
             !
             CALL setup_error_act ( all_errors(:), -130, c_upname, c_modname )
             CALL setup_error_act ( '<file_name>', TRIM(infile_name) )
             CALL setup_error_act ( '<blockname>', TRIM(blockname) )
             CALL setup_error_act ( '<schluessel>', TRIM(key%Name) )
             CALL setup_error_act ( '<key>', TRIM(key%Name) )
             CALL setup_error_act ( '<wert>', TRIM(wert) )
             !
             RETURN
             !
          END IF

       END IF

       IF ( min .GT. 0 ) THEN

          IF ( anz_angabe .LT. min ) THEN
             !
             ! Fehler -140 : Eine Keyzeile enthaelt zuwenig Angaben !
             !
             CALL setup_error_act ( all_errors(:), -140, c_upname, c_modname )
             CALL setup_error_act ( '<file_name>', TRIM(infile_name) )
             CALL setup_error_act ( '<blockname>', TRIM(blockname) )
             CALL setup_error_act ( '<schluessel>', TRIM(key%Name) )
             CALL setup_error_act ( '<key>', TRIM(key%Name) )
             CALL setup_error_act ( '<wert>', TRIM(wert) )
             !
             RETURN

          END IF

       END IF
       !
    END IF ! ok_initialised
    !
  END SUBROUTINE Check_AngabenAnzahl
  !
  !! Die Daten aus der verketteten Liste mit den
  !! Informationen zu den Eingabebloecken werden in das
  !! SAVE-Variablenfeld vom Typ <t_block> uebertragen
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE transfer_EGB_werte &
       ( )
    !
    ! USE-Statements
    !
    USE m_stdat_types, ONLY : &
         ! Routinen
         init_feld
    !
    ! Formalparameter
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='transfer_EGB_werte'
    !! Statusvariable
    INTEGER :: stat !

    INTEGER   :: i, j, k, l, m
    INTEGER   :: bl_anz, nr, z_anz, w_anz
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       ! Komponenten-Felder der Variablen <t_block> auf EGBAnz allokieren
       !
       DO i = 1, SIZE(bloecke)
          !
          IF ( any_error( ) ) EXIT
          !
          bl_anz = bloecke(i)%EGBAnz

          IF ( bl_anz .GT. 0 ) THEN

             DO j = 1, SIZE(bloecke(i)%Key)

                !
                IF ( any_error( ) ) EXIT
                !
                ALLOCATE ( bloecke(i)%Key(j)%ZeilAnz(bl_anz), STAT=stat )
                !
                IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
                   !
                   CALL setup_error_act &
                         ( all_errors(:), -10000, c_upname, c_modname, stat )
                   CALL setup_error_act ( '<felder>', 'bloecke(i)%Key(j)%ZeilAnz' )
                   !
                END IF
                !
                IF ( no_error( ) ) THEN
                   !
                   DO k = 1, SIZE(bloecke(i)%Key(j)%ZeilAnz)
                      bloecke(i)%Key(j)%ZeilAnz(k) = 0
                   END DO
                   !
                END IF
                !
                DO k = 1, SIZE(bloecke(i)%Key(j)%Par)
                   !
                   IF ( any_error( ) ) EXIT
                   !
                   ALLOCATE ( bloecke(i)%Key(j)%Par(k)%EGB(bl_anz), STAT=stat )
                   !
                   IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
                      !
                      CALL setup_error_act &
                            ( all_errors(:), -10000, c_upname, c_modname, stat )
                      CALL setup_error_act ( '<felder>', 'bloecke(i)%Key(j)%Par(k)%EGB' )
                      !
                   END IF
                   !
                   IF ( no_error( ) ) THEN
                      !
!                     ! ... nullify Wert
                      DO l = 1, SIZE(bloecke(i)%Key(j)%Par(k)%EGB)

                         NULLIFY (bloecke(i)%Key(j)%Par(k)%EGB(l)%Wert)

                      END DO
                   END IF
                   !
                END DO   ! k
                !
             END DO   ! j

             ! ... wird noch mal beim Einsortieren als Zaehler verwendet !
             IF ( no_error( ) ) bloecke(i)%EGBAnz = 0
             !
          END IF   ! bl_anz .GT. 0
          !
       END DO   ! i
       !
       !
       ! Eingabewerte der verketteten Liste vom Typ <t_vl_block> werden nun in das
       ! Feld bloecke vom Typ <t_block> uebertragen

       ! ... erstes Listenelement

       IF ( no_error( ) ) act_egb => fst_egb
       
       ! ... Schleife ueber alle Listenelemente

       vlist : DO
       
          IF ( any_error( )  .OR.  .NOT. ASSOCIATED(act_egb) ) EXIT vlist

          DO i = 1, SIZE(bloecke)
             !
             IF ( any_error( ) ) EXIT
             !
             IF ( bloecke(i)%Name .EQ. act_egb%block%Name ) THEN
                !
                bloecke(i)%EGBAnz = bloecke(i)%EGBAnz + 1
                nr = bloecke(i)%EGBAnz

                DO j = 1, SIZE(bloecke(i)%Key)
                   !
                   IF ( any_error( ) ) EXIT
                   !
                   DO k = 1, SIZE(bloecke(i)%Key(j)%Par)
                      !
                      IF ( any_error( ) ) EXIT
                      !
                      IF ( ASSOCIATED(act_egb%block%Key(j)%Par(k)%EGB(1)%Wert) ) THEN

                         ! ... KeyZeilenAnzahl uebertragen
                         bloecke(i)%Key(j)%ZeilAnz(nr) = act_egb%block%Key(j)%ZeilAnz(1)

                         z_anz = bloecke(i)%Key(j)%ZeilAnz(nr)
                         !
                         ALLOCATE ( bloecke(i)%Key(j)%Par(k)%EGB(nr)%Wert(z_anz), &
                                    STAT=stat )
                         !
                         IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
                            !
                            CALL setup_error_act &
                                  ( all_errors(:), -10000, c_upname, c_modname, stat )
                            CALL setup_error_act ( '<felder>', 'bloecke(i)%Key(j)%Par(k)%EGB(nr)%Wert' )
                            !
                         END IF
                         !
                         DO l = 1, SIZE(bloecke(i)%Key(j)%Par(k)%EGB(nr)%Wert)
                            !
                            IF ( any_error( ) ) EXIT
                            !
                            CALL init_feld &
                                  ( bloecke(i)%Key(j)%Par(k)%EGB(nr)%Wert(l) )
                            !
                            IF ( no_error( ) ) THEN
                               !
                               IF (ASSOCIATED(act_egb%block%Key(j)%Par(k)%EGB(1)%Wert(l)%int)) THEN
                                  !
                                  w_anz = SIZE(act_egb%block%Key(j)%Par(k)%EGB(1)%Wert(l)%int)
                                  !
                                  ALLOCATE ( bloecke(i)%Key(j)%Par(k)%EGB(nr)%Wert(l)%int(w_anz), &
                                             STAT=stat )
                                  !
                                  IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
                                     !
                                     CALL setup_error_act &
                                           ( all_errors(:), -10000, c_upname, c_modname, stat )
                                     CALL setup_error_act &
                                           ( '<felder>', 'bloecke(i)%Key(j)%Par(k)%EGB(nr)%Wert(l)%int' )
                                     !
                                  END IF
                                  !
                                  IF ( no_error( ) ) THEN
                                     !
                                     DO m = 1, w_anz
      
                                        bloecke(i)%Key(j)%Par(k)%EGB(nr)%Wert(l)%int(m) = &
                                             act_egb%block%Key(j)%Par(k)%EGB(1)%Wert(l)%int(m)

                                     END DO
                                     !
                                  END IF
                                  !
                               ELSE IF (ASSOCIATED(act_egb%block%Key(j)%Par(k)%EGB(1)%Wert(l)%real)) THEN
                                  !
                                  w_anz = SIZE(act_egb%block%Key(j)%Par(k)%EGB(1)%Wert(l)%real)
                                  !
                                  ALLOCATE ( bloecke(i)%Key(j)%Par(k)%EGB(nr)%Wert(l)%real(w_anz), &
                                             STAT=stat )
                                  !                            
                                  IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
                                     !
                                     CALL setup_error_act &
                                           ( all_errors(:), -10000, c_upname, c_modname, stat )
                                     CALL setup_error_act &
                                           ( '<felder>', 'bloecke(i)%Key(j)%Par(k)%EGB(nr)%Wert(l)%real' )
                                     !
                                  END IF
                                  !
                                  IF ( no_error( ) ) THEN
                                     !
                                     DO m = 1, w_anz
      
                                        bloecke(i)%Key(j)%Par(k)%EGB(nr)%Wert(l)%real(m) = &
                                             act_egb%block%Key(j)%Par(k)%EGB(1)%Wert(l)%real(m)

                                     END DO
                                     !
                                  END IF
                                  !
                               ELSE IF (ASSOCIATED(act_egb%block%Key(j)%Par(k)%EGB(1)%Wert(l)%log)) THEN
                                  !
                                  w_anz = SIZE(act_egb%block%Key(j)%Par(k)%EGB(1)%Wert(l)%log)
                                  !
                                  ALLOCATE ( bloecke(i)%Key(j)%Par(k)%EGB(nr)%Wert(l)%log(w_anz), &
                                             STAT=stat )
                                  !                            
                                  IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
                                     !
                                     CALL setup_error_act &
                                           ( all_errors(:), -10000, c_upname, c_modname, stat )
                                     CALL setup_error_act &
                                           ( '<felder>', 'bloecke(i)%Key(j)%Par(k)%EGB(nr)%Wert(l)%log' )
                                     !
                                  END IF
                                  !
                                  IF ( no_error( ) ) THEN
                                     !
                                     DO m = 1, w_anz
      
                                        bloecke(i)%Key(j)%Par(k)%EGB(nr)%Wert(l)%log(m) = &
                                             act_egb%block%Key(j)%Par(k)%EGB(1)%Wert(l)%log(m)

                                     END DO
                                     !
                                  END IF
                                  !
                               ELSE IF (ASSOCIATED(act_egb%block%Key(j)%Par(k)%EGB(1)%Wert(l)%char)) THEN
                                  !
                                  w_anz = SIZE(act_egb%block%Key(j)%Par(k)%EGB(1)%Wert(l)%char)
                                  !
                                  ALLOCATE ( bloecke(i)%Key(j)%Par(k)%EGB(nr)%Wert(l)%char(w_anz), &
                                             STAT=stat )
                                  !                            
                                  IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
                                     !
                                     CALL setup_error_act &
                                           ( all_errors(:), -10000, c_upname, c_modname, stat )
                                     CALL setup_error_act &
                                           ( '<felder>', 'bloecke(i)%Key(j)%Par(k)%EGB(nr)%Wert(l)%char' )
                                     !
                                  END IF
                                  !
                                  IF ( no_error( ) ) THEN
                                     !
                                     DO m = 1, w_anz
      
                                        bloecke(i)%Key(j)%Par(k)%EGB(nr)%Wert(l)%char(m) = &
                                             REPEAT(' ',LEN(bloecke(i)%Key(j)%Par(k)%EGB(nr)%Wert(l)%char(m)))

                                        bloecke(i)%Key(j)%Par(k)%EGB(nr)%Wert(l)%char(m) = &
                                             act_egb%block%Key(j)%Par(k)%EGB(1)%Wert(l)%char(m)

                                     END DO
                                     !
                                  END IF
                                  !
                               ELSE IF (ASSOCIATED(act_egb%block%Key(j)%Par(k)%EGB(1)%Wert(l)%doub)) THEN
                                  !
                                  w_anz = SIZE(act_egb%block%Key(j)%Par(k)%EGB(1)%Wert(l)%doub)
                                  !
                                  ALLOCATE ( bloecke(i)%Key(j)%Par(k)%EGB(nr)%Wert(l)%doub(w_anz), &
                                             STAT=stat )
                                  !
                                  IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
                                     !
                                     CALL setup_error_act &
                                           ( all_errors(:), -10000, c_upname, c_modname, stat )
                                     CALL setup_error_act &
                                           ( '<felder>', 'bloecke(i)%Key(j)%Par(k)%EGB(nr)%Wert(l)%doub' )
                                     !
                                  END IF
                                  !
                                  IF ( no_error( ) ) THEN
                                     !
                                     DO m = 1, w_anz
      
                                        bloecke(i)%Key(j)%Par(k)%EGB(nr)%Wert(l)%doub(m) = &
                                             act_egb%block%Key(j)%Par(k)%EGB(1)%Wert(l)%doub(m)
      
                                     END DO
                                     !
                                  END IF
                                  !
                               END IF  ! ASSO ...Wert%Komp.
                               !
                            END IF  ! no_error( )
                            !
                         END DO   ! l
                         !
                      END IF   ! ASSO (act_egb%...%Wert)
                      !
                   END DO   ! k
                   !
                END DO   ! j
                !
             END IF ! bloecke(i)%Name .EQ. act_egb%block%Name
             !
          END DO ! i
          !
          IF ( no_error( ) ) THEN
             !
             ! ... naechste Datei initialisieren
             !
             lst_egb => act_egb
             act_egb => act_egb%next
             !
             ! ... Deallokieren des wegsortierten Listenelementes
             !
             CALL dealloc_t_block ( lst_egb%block )
             !
          END IF
          !
          IF ( no_error( ) ) DEALLOCATE (lst_egb)
          !          
       END DO vlist
       !
       ! Pointerverbindungen loesen
       !
       IF ( no_error( ) ) THEN
          !
          NULLIFY (fst_egb)
          NULLIFY (act_egb)
          NULLIFY (lst_egb)
          !
       END IF
       !
    END IF ! ok_initialised
    !
  END SUBROUTINE transfer_EGB_werte
  !
  !! Einige Checks auf die Parameter eines
  !! get_input_data-Aufrufs & Adressnummern ermitteln !
  !!
  !!  - Block- und Keyname ueberpruefen
  !!  - zugehoerige Adressnummern herausfinden
  !!  - Datentyp des Rueckgabe-Parameters pruefen
  !!  - Zeilennummer pruefen
  !!  - Ggf : Pruefen ob ein Wert als Rueckgabe-Parameter ausreicht
  !!  - Ggf : Pruefen ob optionaler UP-Parameter der
  !!          get_input_data-Methode auch tatsaechlich
  !!          nicht benoetigt wird
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE get_par_general &
       ( block, egb_nr, key, par_nr, zeil_nr, dat_typ, &
         l_feld, l_exist_pres, bl_nr, key_nr )
    !
    ! Formalparameter
    !! Name des Eingabeblockes
    CHARACTER (LEN=*)  , INTENT(IN   )      :: block
    !! Nummer des Eingabeblockes mit Namen <block>
    INTEGER            , INTENT(IN   )      :: egb_nr  
    !! Schluesselwort
    CHARACTER (LEN=*)  , INTENT(IN   )      :: key
    !! Positionsnummer des Parameters
    INTEGER            , INTENT(IN   )      :: par_nr
    !! Nummer der Eingabezeile mit KEY <key> <BR>
    !! [ 0 bedeutet gleichzeitiger Zugriff auf alle Zeilen ]
    INTEGER            , INTENT(IN   )      :: zeil_nr
    !! Datentyp des im Aufruf der get_input_data-Methode <BR>
    !! fuer die Rueckgabe der Eingabegroessen vorgesehenen <BR>
    !! (Feld-) Variablen
    CHARACTER (LEN=*)  , INTENT(IN   )      :: dat_typ
    !! True , wenn UP-Parameter fuer die Rueckgabe der Eingabegroessen ein Feld ist <BR>
    !! False, wenn die Rueckgabe in einen Wert erfolgen soll
    LOGICAL            , INTENT(IN   )      :: l_feld
    !! True , wenn optionaler UP-Parameter im Aufruf der get_input_data-Methode present ist <BR>
    !! False, wenn dem nicht so ist
    LOGICAL            , INTENT(IN   )      :: l_exist_pres
    !
    !! Adress-Nummer des Eingabeblockes
    INTEGER            , INTENT(  OUT)      :: bl_nr
    !! Adress-Nummer des Schluesselwortes im Eingabeblock
    INTEGER            , INTENT(  OUT)      :: key_nr
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_par_general'
    !
    ! Adressnummern des Parameters ermitteln
    !
    CALL get_adresse &
          ( block, key, par_nr, 'PAR', 'get_input_data', &
            bl_nr, key_nr )
    !
    ! EGB_NR pruefen
    !
    IF ( no_error( ) ) CALL pruefe_egb_nr & 
                             ( bl_nr, egb_nr, 'get_input_data' )
    !
    ! Pruefe ob Datentyp des Rueckgabewertes, bzw.-feldes und Datentyp
    ! des Parameters zusammenpassen
    !
    IF ( no_error( ) ) CALL pruefe_dattype &
                             ( bl_nr, key_nr, par_nr, TRIM(dat_typ) )
    !
    ! Pruefe ob die im get_input_data-Aufruf angegebene Zeilen-Nummer korrekt ist
    !
    IF ( no_error( ) ) CALL pruefe_ZeilNr &
                             ( bl_nr, egb_nr, key_nr, par_nr, zeil_nr )
    !
    ! Ggf : Pruefe ob fuer die Rueckgabe der Eingabegroesse ein Einzelwert
    !       ausreichend ist
    !
    IF ( no_error( )  .AND.  .NOT. l_feld ) THEN

       CALL pruefe_einzelwert &
            ( bl_nr, key_nr, par_nr, zeil_nr )

    END IF
    !
    ! Ggf.: Pruefe ob ein Adressteil (Block,Key,Parameter) optional ist, denn
    !       dann muss im get_input_data-Aufruf der optionale UP-Parameter
    !       <l_exist> vorhanden sein
    !
    IF ( no_error( )  .AND.  .NOT. l_exist_pres ) THEN

       CALL pruefe_optional &
            ( bl_nr, key_nr, par_nr )

    END IF
    !
  END SUBROUTINE get_par_general
  !
  !! Liefert die zu einem Dateielement [BLOCK,KEY,PARAMETER]
  !! und dessen Adresse gehoerigen Adressnummern. <BR>
  !! Blocknahme -> bl_nr <BR>
  !! Keyword    -> key_nr <BR>
  !! Ist die angegebene Adresse ungueltig erfolgt eine
  !! entsprechende Fehlermeldung !
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE get_adresse &
       ( block, key, par_nr, c_obj, c_method, &
         bl_nr, key_nr )
    !    
    !   USE-Statements
    !
    USE m_stdat_types, ONLY : &
         ! Typdefinitionen
         t_obj, &
         ! Routinen
         init_objekt, &
         Adressnummern_suchen
    !
    ! Formalparameter
    !! Name des Eingabeblockes
    CHARACTER (LEN=*)  , INTENT(IN   )      :: block
    !! Schluesselwort
    CHARACTER (LEN=*)  , INTENT(IN   )      :: key
    !! Nummer des Parameters
    INTEGER            , INTENT(IN   )      :: par_nr
    !! Objekt-Typ fuer den die Adressnummer(n) festgestellt
    !! werden sollen [PAR,KEY,BLOCK]
    CHARACTER(LEN=*)   , INTENT(IN   )      :: c_obj
    !! Name der rufenden Subroutine (fuer Fehlermeldung)
    CHARACTER(LEN=*)   , INTENT(IN   )      :: c_method
    !! Adress-Nummer des Eingabeblockes
    INTEGER            , INTENT(  OUT)      :: bl_nr
    !! Adress-Nummer des Schluesselwortes
    INTEGER            , INTENT(  OUT)      :: key_nr
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_adresse'
    !! Hilfsstrings fuer Fehlermeldung
    CHARACTER (LEN=6)             :: c_actlen, c_maxlen
    !
    TYPE (t_obj)                            :: par_obj
    !
    CHARACTER(LEN=key_len)                  :: ad_block
    CHARACTER(LEN=key_len)                  :: ad_key
    !
    CHARACTER (LEN=3)                       :: c_par
    INTEGER                                 :: i_par
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       ! Initialisierungen
       !
       bl_nr  = 0
       key_nr = 0

       ad_block = REPEAT(' ', LEN(ad_block))
       ad_key   = REPEAT(' ', LEN(ad_key))
       !
       ! ----------------
       !
       IF ( LEN_TRIM(block) .GT. LEN(ad_block) ) THEN
          !
          ! Fehler -150 : Blockname ist zu lang !
          !
          c_actlen = REPEAT(' ',LEN(c_actlen))
          c_maxlen = REPEAT(' ',LEN(c_maxlen))
          !
          WRITE(c_actlen,'(I5)') LEN_TRIM(block)
          WRITE(c_maxlen,'(I5)') LEN(ad_block)
          !
          CALL setup_error_act ( all_errors(:), -150, c_upname, c_modname )
          CALL setup_error_act ( '<meth_name>', TRIM(c_method) )
          CALL setup_error_act ( '<akt_len>', TRIM(c_actlen) )
          CALL setup_error_act ( '<max_len>', TRIM(c_maxlen) )
          !
          RETURN
          !
       END IF

       IF ( LEN_TRIM(key) .GT. LEN(ad_key) ) THEN
          !
          ! Fehler -160 : Schluesselwort ist zu lang !
          !
          c_actlen = REPEAT(' ',LEN(c_actlen))
          c_maxlen = REPEAT(' ',LEN(c_maxlen))
          !
          WRITE(c_actlen,'(I5)') LEN_TRIM(key)
          WRITE(c_maxlen,'(I5)') LEN(ad_key)
          !
          CALL setup_error_act ( all_errors(:), -160, c_upname, c_modname )
          CALL setup_error_act ( '<meth_name>', TRIM(c_method) )
          CALL setup_error_act ( '<akt_len>', TRIM(c_actlen) )
          CALL setup_error_act ( '<max_len>', TRIM(c_maxlen) )
          !
          RETURN
          !
       END IF
       !
       ad_block = TRIM(block)
       ad_block = ADJUSTL(ad_block)
       !
       ad_key   = TRIM(key)
       ad_key   = ADJUSTL(ad_key)
       !      
       !
       ! Adressnummern des Parameters feststellen
       ! ( Nummer = 0 bedeutet Objekt-Adressteil wurde nicht gefunden )
       !
       CALL init_objekt ( par_obj )
       !
       IF ( no_error( ) ) THEN
          !
          par_obj%typ    = TRIM(c_obj)
          par_obj%block  = TRIM(ad_block)

          SELECT CASE (c_obj)
          CASE ('KEY','PAR')

             par_obj%key    = TRIM(ad_key)

             SELECT CASE (c_obj)
             CASE ('PAR')
                par_obj%par_nr = par_nr
             END SELECT

          END SELECT
          !
          CALL Adressnummern_suchen &
                ( par_obj, bloecke, bl_nr, key_nr, i_par )
          !
       END IF
       !
       IF ( no_error( ) ) THEN
          !
          IF ( bl_nr .EQ. 0 ) THEN
             ! 
             ! Fehler -170 : Blockname ist ungueltig !
             !
             CALL setup_error_act ( all_errors(:), -170, c_upname, c_modname )
             CALL setup_error_act ( '<meth_name>', TRIM(c_method) )
             CALL setup_error_act ( '<akt_block>', TRIM(ad_block) )
             !
             RETURN
             !
          END IF
          !
       END IF
       !    
       IF ( no_error( ) ) THEN
          !
          IF ( key_nr .EQ. 0  .AND.  c_obj .NE. 'BLOCK' ) THEN
             !
             ! Fehler -180 : Schluesselwort unbekannt !
             !
             CALL setup_error_act ( all_errors(:), -180, c_upname, c_modname )
             CALL setup_error_act ( '<meth_name>', TRIM(c_method) )
             CALL setup_error_act ( '<blockname>', TRIM(ad_block) )
             CALL setup_error_act ( '<akt_key>', TRIM(ad_key) )
             !
             RETURN
             !
          END IF
          !
       END IF
       !
       IF ( no_error( ) ) THEN
          !
          IF ( i_par .EQ. 0  .AND.  c_obj .EQ. 'PAR') THEN
             !
             ! Fehler -190 : Parameter unbekannt !
             !
             c_par = REPEAT(' ', LEN(c_par))
             WRITE(c_par,'(I3)') par_obj%par_nr 
             c_par = ADJUSTL(c_par)
             !
             CALL setup_error_act ( all_errors(:), -190, c_upname, c_modname )
             CALL setup_error_act ( '<meth_name>', TRIM(c_method) )
             CALL setup_error_act ( '<blockname>', TRIM(ad_block) )
             CALL setup_error_act ( '<schluessel>', TRIM(ad_key) )
             CALL setup_error_act ( '<pos_nr>', TRIM(c_par) )
             !
             RETURN
             !
          END IF
          !
       END IF
       !
       !
    END IF ! ok_initialised
    !
  END SUBROUTINE get_adresse
  !
  !! Prueft ob die im Get_???-Call enthaltene 
  !! Eingabeblock-Nummer eines Blockes im gueltigen Bereich
  !! liegt. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE pruefe_egb_nr &
       ( bl_nr, egb_nr, c_method )
    !
    ! Formalparameter
    !! Adressnummer des Block-Types
    INTEGER            , INTENT(IN   )      :: bl_nr
    !! Nummer des Eingabeblockes
    INTEGER            , INTENT(IN   )      :: egb_nr
    !! Name der rufenden Subroutine
    CHARACTER(LEN=*)   , INTENT(IN   )      :: c_method
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='pruefe_egb_nr'
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       IF ( egb_nr .LT. 1 ) THEN
          !    
          ! Fehler -200 : Die angegebene Eingabeblocknummer ist kleiner als 1
          !
          CALL setup_error_act ( all_errors(:), -200, c_upname, c_modname )
          CALL setup_error_act ( '<meth_name>', TRIM(c_method) )
          !
          RETURN
          !
       END IF
       !
       IF ( egb_nr .GT. bloecke(bl_nr)%EGBAnz ) THEN
          !    
          ! Fehler -210 : Eingabeblocknummer ist groesser als die Zahl der
          !             Eingabebloecke des Typs in der Eingabedatei
          !
          CALL setup_error_act ( all_errors(:), -210, c_upname, c_modname )
          CALL setup_error_act ( '<meth_name>', TRIM(c_method) )
          CALL setup_error_act ( '<blockname>', TRIM(bloecke(bl_nr)%Name) )
          !
          RETURN
          !
       END IF
       !
    END IF ! ok_initialised
    !
  END SUBROUTINE pruefe_egb_nr
  !
  !! Prueft, ob der Datentyp des im Get_Input_Data-Call
  !! enthaltenen Rueckgabeparameters zum Datentyp
  !! des an der Adresse positionierten Parameters passt !
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE pruefe_dattype &
       ( bl_nr, key_nr, par_nr, dattype )
    !
    ! Formalparameter
    !! Adress-Nummer des Eingabeblockes
    INTEGER            , INTENT(IN   )      :: bl_nr
    !! Adress-Nummer des Schluesselwortes
    INTEGER            , INTENT(IN   )      :: key_nr
    !! Adress-Nummer des Parameters
    INTEGER            , INTENT(IN   )      :: par_nr
    !! Datentyp des Rueckgabewertes, bzw. -feldes
    CHARACTER (LEN=*)                       :: dattype
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='pruefe_dattype'
    !! Fehlerkennung
    INTEGER                  ::  ierr=0
    !
    CHARACTER (LEN= 6)       ::  par_type
    CHARACTER (LEN=20)       ::  c_typ
    CHARACTER (LEN= 3)       ::  c_par
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       ! Vergleich der Datentypen
       !
       par_type = REPEAT(' ', LEN(par_type))
       par_type = bloecke(bl_nr)%Key(key_nr)%Par(par_nr)%Type

       SELECT CASE (dattype)
       CASE ('INTEGER')

          IF ( par_type .NE. 'INT' ) ierr  = 1

       CASE ('REAL')

          IF ( par_type .NE. 'REAL' ) ierr  = 1

       CASE ('DOUBLE')

          IF ( par_type .NE. 'REAL' .AND. &
               par_type .NE. 'DOUBLE'      ) ierr  = 1

       CASE ('LOGICAL')

          IF ( par_type .NE. 'LOG' ) ierr  = 1

       CASE ('CHARACTER')

          IF ( par_type .NE. 'CHAR'  .AND. &
               par_type .NE. 'FILE'  .AND. &
               par_type .NE. 'DATE'  .AND. &
               par_type .NE. 'INCR'           )  ierr  = 1

       CASE DEFAULT
          !    
          ! Fehler -220 : Parameter "dattype" hat unzulaessigenn Wert
          !
          CALL setup_error_act ( all_errors(:), -220, c_upname, c_modname )
          !
          RETURN
          !
       END SELECT
       !
       IF ( ierr  .EQ. 1 ) THEN
          !    
          ! Fehler -230 : Rueckgabeparameter ist von falschem Datentyp
          !
          c_typ = REPEAT(' ',LEN(c_typ))

          SELECT CASE (par_type)
          CASE ('INT')
             c_typ = 'INTEGER'
          CASE ('REAL')
             c_typ = 'REAL'
          CASE ('LOG')
             c_typ = 'LOGICAL'
          CASE ('DOUBLE')
             c_typ = 'REAL (KIND=double)'
          CASE ('CHAR','FILE','DATE','INCR')
             c_typ = 'CHARACTER'
          END SELECT

          c_par = REPEAT(' ', LEN(c_par))
          WRITE(c_par,'(I3)') par_nr 
          c_par = ADJUSTL(c_par)
          !
          CALL setup_error_act ( all_errors(:), -230, c_upname, c_modname )
          CALL setup_error_act ( '<par>', TRIM(c_par) )
          CALL setup_error_act ( '<key>', TRIM(bloecke(bl_nr)%Key(key_nr)%Name) )
          CALL setup_error_act ( '<block>', TRIM(bloecke(bl_nr)%Name) )
          CALL setup_error_act ( '<req_typ>', TRIM(c_typ) )
          CALL setup_error_act ( '<akt_typ>', TRIM(dattype) )
          !
          RETURN
       
       END IF
       !
    END IF ! ok_initialised
    !
  END SUBROUTINE pruefe_dattype
  !
  !! Prueft, ob die in der Get_Input_Data-Methode angegebene
  !! Zeilennummer im gueltigen Bereich liegt.
  !! Zeilennummer darf nicht negativ sein !
  !! Eine Zeilennummer von Null ist nur in ganz bestimmten
  !! Faellen (s.u.) zugelassen. 
  !! Die Zeilennummer darf nicht groesser sein als die Anzahl
  !! an Eingabezeilen des betreffenden Schluesselwortes.
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE pruefe_ZeilNr &
       ( bl_nr, egb_nr, key_nr, par_nr, zeil_nr )
    !
    ! Formalparameter
    !! Adress-Nummer des Blockes
    INTEGER            , INTENT(IN   )      :: bl_nr
    !! Nummer des Eingabeblockes
    INTEGER            , INTENT(IN   )      :: egb_nr
    !! Adress-Nummer des Schluesselwortes
    INTEGER            , INTENT(IN   )      :: key_nr
    !! Adress-Nummer des Parameters
    INTEGER            , INTENT(IN   )      :: par_nr
    !! Nummer der Eingabezeile
    INTEGER            , INTENT(IN   )      :: zeil_nr
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='pruefe_ZeilNr'
    CHARACTER (LEN= 3)                      :: c_par
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       IF ( zeil_nr .EQ. 0 ) THEN
          !
          IF ( .NOT. bloecke(bl_nr)%Key(key_nr)%L_Single ) THEN
             !
             IF ( .NOT. bloecke(bl_nr)%Key(key_nr)%L_OneArray ) THEN
                !    
                ! Fehler -240 : Fuer die KEY-Zeile eines angeforderten Parameters
                !               ist das Attribut L_OneArray = False gesetzt
                CALL setup_error_act ( all_errors(:), -240, c_upname, c_modname )
                CALL setup_error_act ( '<key>', TRIM(bloecke(bl_nr)%Key(key_nr)%Name) )
                CALL setup_error_act ( '<block>', TRIM(bloecke(bl_nr)%Name) )
                !
                RETURN
                !
             ELSE
                !
                IF ( bloecke(bl_nr)%Key(key_nr)%Par(par_nr)%L_Opt ) THEN
                   !    
                   ! Fehler -250 : Parameter einer KEY-Zeile mit verschiedenen PARs ist als 
                   !               optional vereinbart
                   !
                   c_par = REPEAT(' ', LEN(c_par))
                   WRITE(c_par,'(I3)') par_nr 
                   c_par = ADJUSTL(c_par)
                   !
                   CALL setup_error_act ( all_errors(:), -250, c_upname, c_modname )
                   CALL setup_error_act ( '<par>', TRIM(c_par) )
                   CALL setup_error_act ( '<key>', TRIM(bloecke(bl_nr)%Key(key_nr)%Name) )
                   CALL setup_error_act ( '<block>', TRIM(bloecke(bl_nr)%Name) )
                   !
                   RETURN
                   !
                END IF
                !
                IF ( SIZE(bloecke(bl_nr)%Key(key_nr)%Par) .GT. 1  .AND. &
                     bloecke(bl_nr)%Key(key_nr)%Par(par_nr)%L_Array    ) THEN
                   !    
                   ! Fehler -260 : Parameter einer KEY-Zeile mit verschiedenen PARs ist als 
                   !               Array-Groesse vereinbart
                   !
                   c_par = REPEAT(' ', LEN(c_par))
                   WRITE(c_par,'(I3)') par_nr 
                   c_par = ADJUSTL(c_par)
                   !
                   CALL setup_error_act ( all_errors(:), -260, c_upname, c_modname )
                   CALL setup_error_act ( '<key>', TRIM(bloecke(bl_nr)%Key(key_nr)%Name) )
                   CALL setup_error_act ( '<block>', TRIM(bloecke(bl_nr)%Name) )
                   CALL setup_error_act ( '<par>', TRIM(c_par) )
                   !
                   RETURN
                   !
                END IF
                !
             END IF
             !
          END IF
          !
       ELSE IF ( zeil_nr .GT. 0 ) THEN
          !
          IF ( zeil_nr .GT. bloecke(bl_nr)%Key(key_nr)%ZeilAnz(egb_nr) ) THEN
             !    
             ! Fehler -270 : Zeilennummer groesser als Anzahl an Eingabezeilen des KEYs
             !
             c_par = REPEAT(' ', LEN(c_par))
             WRITE(c_par,'(I3)') par_nr 
             c_par = ADJUSTL(c_par)
             !    
             CALL setup_error_act ( all_errors(:), -270, c_upname, c_modname )
             CALL setup_error_act ( '<par>', TRIM(c_par) )
             CALL setup_error_act ( '<key>', TRIM(bloecke(bl_nr)%Key(key_nr)%Name) )
             CALL setup_error_act ( '<block>', TRIM(bloecke(bl_nr)%Name) )
             !
             RETURN
             !
          END IF
          !
       ELSE
          !    
          ! Fehler -280 : negative Zeilennummer
          !
          c_par = REPEAT(' ', LEN(c_par))
          WRITE(c_par,'(I3)') par_nr 
          c_par = ADJUSTL(c_par)
          !
          CALL setup_error_act ( all_errors(:), -280, c_upname, c_modname )
          CALL setup_error_act ( '<par>', TRIM(c_par) )
          CALL setup_error_act ( '<key>', TRIM(bloecke(bl_nr)%Key(key_nr)%Name) )
          CALL setup_error_act ( '<block>', TRIM(bloecke(bl_nr)%Name) )
          !
          RETURN
          !
       END IF
       !
    END IF ! ok_initialised
    !
  END SUBROUTINE pruefe_ZeilNr
  !
  !! Unterprogramm checkt ob fuer die Rausgabe der ueber die
  !! Get_Input_Data-Methode angeforderten Eingabedaten ein
  !! Einzelwert als Rueckgabe-Parameter ausreichend ist.
  !! Falls nicht : -> Programm-Abbruch ! <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE pruefe_einzelwert &
       ( bl_nr, key_nr, par_nr, zeil_nr )
    !
    ! Formalparameter
    !! Adress-Nummer des Eingabeblockes
    INTEGER            , INTENT(IN   )      :: bl_nr
    !! Adress-Nummer des Schluesselwortes
    INTEGER            , INTENT(IN   )      :: key_nr
    !! Adress-Nummer des Parameters
    INTEGER            , INTENT(IN   )      :: par_nr
    !! Adress-Zeilennummer
    INTEGER            , INTENT(IN   )      :: zeil_nr
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='pruefe_einzelwert'
    CHARACTER (LEN= 3)                      :: c_par
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       IF ( bloecke(bl_nr)%Key(key_nr)%Par(par_nr)%L_Array ) THEN
          !
          ! Fehler -290 : Parameter ist als Array-Groesse vereinbart das erfordert
          !               ein Feld fuer die Rueckgabe der Eingabewerte
          !
          c_par = REPEAT(' ', LEN(c_par))
          WRITE(c_par,'(I3)') par_nr 
          c_par = ADJUSTL(c_par)
          !
          CALL setup_error_act ( all_errors(:), -290, c_upname, c_modname )
          CALL setup_error_act ( '<par>', TRIM(c_par) )
          CALL setup_error_act ( '<key>', TRIM(bloecke(bl_nr)%Key(key_nr)%Name) )
          CALL setup_error_act ( '<block>', TRIM(bloecke(bl_nr)%Name) )
          !               
          RETURN
          !
       END IF
       !
       IF ( .NOT. bloecke(bl_nr)%Key(key_nr)%L_Single  .AND. &
            zeil_nr .EQ. 0                                     ) THEN
          !
          ! Fehler -300 : MehrfachZeile + zeil_nr = 0 erfordert Feld fuer Rueckgabe
          !             der Eingabewerte oder Direktadressierung ueber Zeilennummer !
          !
          CALL setup_error_act ( all_errors(:), -300, c_upname, c_modname )
          CALL setup_error_act ( '<key>', TRIM(bloecke(bl_nr)%Key(key_nr)%Name) )
          CALL setup_error_act ( '<block>', TRIM(bloecke(bl_nr)%Name) )
          !
          RETURN
          !
       END IF
       !
    END IF ! ok_initialised
    !
  END SUBROUTINE pruefe_einzelwert
  !
  !! Unterprogramm prueft, ob dem Aufruf der Get_Input_Data-
  !! Methode der optionale Parameter <l_exist> zu Recht fehlt.
  !! <P>
  !! Ist naemlich eines der adressierten Dateielemente
  !! als optional vereinbart, so muss <l_exist> im UP-Call
  !! enthalten sein. Ein Programm-Abbruch folgt !<BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE pruefe_optional &
       ( bl_nr, key_nr, par_nr )
    !
    ! Formalparameter
    !! Adress-Nummer des Eingabeblockes
    INTEGER            , INTENT(IN   )      :: bl_nr
    !! Adress-Nummer des Schluesselwortes
    INTEGER            , INTENT(IN   )      :: key_nr
    !! Adress-Nummer des Parameters
    INTEGER            , INTENT(IN   )      :: par_nr
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='pruefe_optional'
    CHARACTER (LEN= 3)                      :: c_par
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       IF ( bloecke(bl_nr)%L_Opt ) THEN
          !
          ! Fehler -310 : Aufruf der get_input_data-Methode muss den optionalen
          !               Parameter <l_exist> enthalten, da der Eingabeblock (L_Opt=True)
          !               optional ist.
          !
          c_par = REPEAT(' ', LEN(c_par))
          WRITE(c_par,'(I3)') par_nr 
          c_par = ADJUSTL(c_par)
          !
          CALL setup_error_act ( all_errors(:), -310, c_upname, c_modname )
          CALL setup_error_act ( '<par>', TRIM(c_par) )
          CALL setup_error_act ( '<key>', TRIM(bloecke(bl_nr)%Key(key_nr)%Name) )
          CALL setup_error_act ( '<block>', TRIM(bloecke(bl_nr)%Name) )
          !
          RETURN
          !
       END IF
       !    
       IF ( bloecke(bl_nr)%Key(key_nr)%L_Opt ) THEN
          !
          ! Fehler -320 : Aufruf der get_input_data-Methode muss den optionalen
          !               Parameter <l_exist> enthalten, da die Keyzeile (L_Opt=True)
          !               optional ist.
          !
          c_par = REPEAT(' ', LEN(c_par))
          WRITE(c_par,'(I3)') par_nr 
          c_par = ADJUSTL(c_par)
          !
          CALL setup_error_act ( all_errors(:), -320, c_upname, c_modname )
          CALL setup_error_act ( '<par>', TRIM(c_par) )
          CALL setup_error_act ( '<key>', TRIM(bloecke(bl_nr)%Key(key_nr)%Name) )
          CALL setup_error_act ( '<block>', TRIM(bloecke(bl_nr)%Name) )
          !
          RETURN
          !
       END IF
       !
       IF ( bloecke(bl_nr)%Key(key_nr)%Par(par_nr)%L_Opt ) THEN
          !
          ! Fehler -330 : Aufruf der get_input_data-Methode muss den optionalen
          !               Parameter <l_exist> enthalten, da der Parameter (L_Opt=True)
          !               optional ist.
          !
          c_par = REPEAT(' ', LEN(c_par))
          WRITE(c_par,'(I3)') par_nr 
          c_par = ADJUSTL(c_par)
          !
          CALL setup_error_act ( all_errors(:), -330, c_upname, c_modname )
          CALL setup_error_act ( '<par>', TRIM(c_par) )
          CALL setup_error_act ( '<key>', TRIM(bloecke(bl_nr)%Key(key_nr)%Name) )
          CALL setup_error_act ( '<block>', TRIM(bloecke(bl_nr)%Name) )
          !
          RETURN
          !
       END IF
       !
    END IF ! ok_initialised
    !
  END SUBROUTINE pruefe_optional
  !
  !! Der Speicherplatz der dynamisch allokierten Komponenten-Felder der
  !! SAVE-Variablen <bloecke> wird wieder freigegeben.
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_diction &
       ( )
    !
    ! Formalparameter
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='dealloc_diction'
    !
    INTEGER :: i
    !
    ! Deallokieren des Feldes mit Elementen vom Typ t_block
    !
    DO i = 1, SIZE(bloecke)
       !  
       IF ( no_error( ) ) CALL dealloc_t_block &
                                ( bloecke(i) )
       !
    END DO
    !
  END SUBROUTINE dealloc_diction
  !
  !
  !! Ersetzen von Zeichen "c_repla" durch ein anderes Zeichen <BR>
  !! Unterprogramm erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE replace_repla_in_string_d ( repla, sign, string )
    !! zu ersetzendes Zeichen
    CHARACTER (LEN=1) , INTENT(IN)    :: repla     ! 
    !! einzusetzendes Zeichen
    CHARACTER (LEN=1) , INTENT(IN)    :: sign      ! 
    !! String in dem Zeichen ersetzt werden sollen
    CHARACTER (LEN=*) , INTENT(INOUT) :: string(:) ! 
    ! Hilfsvariablen
    INTEGER :: i, j ! 
    !
    DO j=1,SIZE(string)
       DO i=1,LEN_TRIM(string(j))
          IF ( string(j)(i:i) == repla ) string(j)(i:i) = sign
       END DO
    END DO
    !
  END SUBROUTINE replace_repla_in_string_d
  !
END MODULE p_dictionary_ui
!  ----------------------------------------------------------------------------
!  Ende des Source-Codes des Moduls
!  ----------------------------------------------------------------------------

