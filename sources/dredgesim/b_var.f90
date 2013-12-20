! -------------------------------------------------------------------------
! HeadOfBaseModule --------------------------------------------------------
!
!! <H2>Datentyp+Methoden zum Vorhalten von Angaben zu Variablen</h2>
!! @author Susanne Spohr
!! @version 1.21 vom 04/11/06, Quellcode: mod_b_var.f90
!! <HR>
!! type+methods to deal with informations about variables <BR>
!! <HR>
!! <H3>Copyright-Hinweis</H3>
!!                                                                   <BR>
!! Copyright (C) 2002 <A HREF="http://www.hamburg.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!!                                                                   <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2002-08-14 : S. Spohr    : Startversion
!  01.02 : 2002-08-15 : G. Lang     : LEN=* in set_var_type eingefuehrt
!  01.03 : 2002-08-16 : S. Spohr    : nun + Testmethode
!  01.04 : 2002-08-16 : G. Lang     : neue Funktion "is_unlimited_var"
!  01.05 : 2002-08-21 : G. Lang     : REPLACE_var_dim_id
!  01.06 : 2002-08-21 : G. Lang     : kleine Ergaenzung an REPLACE
!  01.07 : 2002-08-23 : G. Lang     : neuer Typ Integer (Short)
!  01.08 : 2002-08-28 : G. Lang     : get_var_start|step|stride
!  01.09 : 2002-08-30 : G. Lang     : in Komponente Name werden Blanks durch "_" ersetzt
!  01.10 : 2002-09-12 : G. Lang     : gen_error_if_var_not_exists
!  01.11 : 2003-01-14 : S. Spohr    : Anpassungen HTMLDOC
!  01.12 : 2003-01-16 : S. Spohr    : Testroutine ausgelagert
!  01.13 : 2003-01-16 : S. Spohr    : Begruessungsmeldung gekuerzt
!  01.14 : 2003-08-01 : S. Spohr    : Fehler in gen_error_if_var_not_exists_1_n korrigiert
!  01.15 : 2003-12-12 : G. Lang     : neue (ueberladene) Funktion "get_var_size"
!  01.16 : 2003-12-12 : G. Lang     : in get_var_size ABS(product)
!  01.17 : 2004-01-19 : H. Weilbeer : INTENT(IN) --> INTENT(OUT)
!  01.18 : 2004-01-19 : G. Lang     : neuer Variablentyp "CH"
!  01.19 : 2004-04-23 : J. Juerges  : neues Attribut con_id ; get_var_dim_id erweitert
!  01.20 : 2006-03-29 : G. Lang     : neue Funktion get_var_dim_idx und andere kleine Anpassungen
!  01.21 : 2006-04-11 : G. Lang     : get_var_start_0_i korrigiert 
!
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! Speichern von Informationen ueber Variablen unter                 <BR>
!! teilweiser Verwendung von Informationen ueber Dimensionen.        <BR>
!! <HR>
!!                                                                   <BR>
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt den selbst definierten Datentyp <EM>t_var</EM> <BR>
!! zur Verf&uuml;gung. Dieser besteht aus den folgenden Komponenten:  <BR> 
!! <OL>
!!     <LI> <TT>id</TT>     : Identifikationsnummer;
!!     <LI> <TT>name</TT>   : Name der Variablen (z.B. <EM>time</EM>);
!!     <LI> <TT>type</TT>   : Datentyp der Variablen;
!!     <LI> <TT>con_id</TT> : Id der Teilgebiets-Gesamtgebiets-Datenpunkte-Verknuepfung;
!!     <LI> <TT>dim_id</TT> : Id's der Dimensionen der Variablen (Zeiger auf ein Objekt des Typs <EM>t_dim</EM>).
!! </OL>
!!                                                                   <BR>
!! <HR>
!!                                                                   <BR>
!! <HR>
!! <H3>Verwenden des Moduls</H3>
!!                                                                  <BR>
!! Die Leistungen des Moduls k&ouml;nnen wie folgt in Anspruch genommen werden: <BR>
!! <OL>
!!    <LI> Einbinden des Moduls mittels USE-Anweisung in der rufenden Programmeinheit;
!!    <LI> Initialisieren des Moduls b_var mit INIT-Methode;
!!    <LI> ggf. Verwenden verschiedener SETUP-Methoden;
!!    <LI> Verwenden beliebiger Modul-Methoden (nicht INIT und CLEAR);
!!    <LI> De-Initialisieren des Moduls b_var mit CLEAR-Methode.
!! </OL>
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
!! 05040 = De-Allokieren der Komponente dim_id  des Typs t_var      <BR>
!! <HR>
!! OK-Methoden           [  6000 bis  6999 ]                        <BR>
!! 06010 = Fehler in Komponente id  des Typs t_var                  <BR>
!! 06011 = ein Wert von "id" in Vektor mehrfach vorhanden           <BR>
!! 06020 = Fehler in Komponente name  des Typs t_var                <BR>
!! 06021 = ein Wert von "name" in Vektor mehrfach vorhanden         <BR>
!! 06030 = Fehler in Komponente type  des Typs t_var                <BR>
!! 06040 = Fehler in Komponente dim_id  des Typs t_var              <BR>
!! 06041 = unbekannter Wert in Komponente "dim_id"                  <BR>
!! 06050 = Fehler in Komponente con_id des Typs t_var               <BR>
!! <HR>
!! PRINT-Methoden        [  7000 bis  7999 ]                        <BR>
!! 07001 = Drucken der Kopfzeilen                                   <BR>
!! 07002 = Drucken der Fu&szlig;zeilen                              <BR>
!! 07003 = Drucken des Index des Datenobjektes (1D-Array)           <BR>
!! 07010 = Drucken der Komponente id     des Typs t_var             <BR>
!! 07020 = Drucken der Komponente name   des Typs t_var             <BR>
!! 07030 = Drucken der Komponente type   des Typs t_var             <BR>
!! 07040 = Drucken der Komponente dim_id des Typs t_var             <BR>
!! 07050 = Drucken der Komponente con_id des Typs t_var             <BR>
!! 07500 = Drucken der statischen Daten (ohne Fehlermeldungen)      <BR>
!! <HR>
!! SET-Methoden          [  8000 bis  8999 ]                        <BR>
!! 08040 = Allokieren von Komponente dim_id  des Typs t_var         <BR>
!! 08500 = Fehler beim Allokieren eines Hilfsfeldes this3           <BR>
!! 08501 = Fehler beim De-Allokieren eines Pointer-Feldes this1     <BR>
!! 08502 = Fehler beim Allokieren eines Pointer-Feldes this1        <BR>
!! 08503 = Fehler beim De-Allokieren eines Hilfsfeldes this3        <BR>
!! <HR>
!! GET-Methoden          [  9000 bis  9999 ]                        <BR>
!! 09100 = Fehler beim Allokieren des RESULT-Feldes val             <BR>
!! 09500 = Fehler beim Allokieren des Shape-Feldes                  <BR>
!! 09510 = Eine oder mehrere Dimensionsangaben fehlen               <BR>
!! 09600 = Fehler beim Allokieren von "start(:)"                    <BR>
!! 09700 = Fehler beim Allokieren von "step(:)"                     <BR>
!! 09800 = Fehler beim Allokieren von "stride(:)"                   <BR>
!! <HR>
!! OPERATOR(==)-Methoden [ 10000 bis 10999 ]                        <BR>
!! <HR>
!! OPERATOR(/=)-Methoden [ 19000 bis 19999 ]                        <BR>
!! <HR>
!! TEST-Methoden         [ 20000 bis 20999 ]                        <BR>
!! <HR>
!! modul-spezifische Methoden   [      < 0 ]                        <BR>
!! <HR>
!! GEN-Error-Methoden    [ -6000 bis -6999 ]                        <BR>
!! .-06040 = vorgegebene Variable (Name) ist nicht in Objektliste enthalten <BR>
!! <HR>
!
MODULE b_var
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] BASIS-Modul mit globalen Konstantwerten
  !
  ! [A.2] BASIS-Modul mit Fehler-Typ und -Routinen 
  !
  USE b_error, ONLY :       &
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
       clear_error_act,     &
       setup_error_prn_lun, &
       setup_error_trc_lun, &
       set_error_ierr,      &
       set_error_cerr
  !
  ! [A.3] BASIS-Modul mit Dimensionsangaben zu Variablen (ONLY benutzen!) 
  !
  USE b_dim, ONLY : &
       ! Typdefinitionen
       t_dim,        &
       ! Routinen / Interfaces
       init_dim,          &
       clear_dim,         &
       setup_dim_prn_lun, &
       setup_dim_trc_lun, &
       dim_exists,        &
       get_dim_idx,       &
       get_dim_len,       &
       get_dim_id,        &
       get_dim_name,      &
       new_dim,           &
       set_dim_id,        &
       set_dim_name,      &
       set_dim_len,       &
       kill_dim,          &
       is_unlimited_dim
  !
  ! ---------------------------------------------------------------------
  ! [B] alles muss explizit deklariert werden und ist default privat
  ! ---------------------------------------------------------------------
  !
  IMPLICIT NONE
  PRIVATE
  !
  ! ---------------------------------------------------------------------
  ! [C] oeffentlich zugaengliche Deklarationen (mit PUBLIC-Attribut)
  ! ---------------------------------------------------------------------
  !
  ! [C.0] Konstantwerte ------------------------------------------------- 
  !
  !! maximale zul&auml;ssige L&auml;nge des Namens <EM>name</EM> in <EM>t_var</EM>
  INTEGER , PUBLIC , PARAMETER :: c_len_var_name=80 ! 
  !! maximale zul&auml;ssige L&auml;nge des Typs <EM>type</EM> in <EM>t_var</EM>
  INTEGER , PUBLIC , PARAMETER :: c_len_var_type=10 ! 
  !
  ! [C.1] (maximal) einen oeffentlich zugaenglichen Typ deklarieren
  !
  !! Speichern von Angaben zu Variablen  <BR>
  !! id     : Identifikationsnummer      <BR>
  !! name   : Name der Variablen (z.B. <EM>time</EM>) <BR>
  !! type   : Datentyp der Variablen     <BR>
  !! con_id : Id der Teilgebiets-Gesamtgebiets-Datenpunkte-Verknuepfung <BR>
  !! dim_id : Id's der Dimensionen der Variablen (Zeiger auf ein Objekt des Typs <EM>t_dim</EM>) <BR>
  !! <BR>
  !! (Hinweis: wenn dim_id(:) nicht allokiert wird, dann handelt es sich um eine skalare Variable)
  TYPE , PUBLIC :: t_var
     PRIVATE
     INTEGER                        :: id        ! 
     CHARACTER (LEN=c_len_var_name) :: name      ! 
     CHARACTER (LEN=c_len_var_type) :: type      ! 
     INTEGER                        :: con_id    ! 
     INTEGER  , POINTER             :: dim_id(:) ! 
  END TYPE t_var
  !
  ! [C.2] Konstantwerte (Parameter) [moeglichst nicht verwenden]
  ! [C.3] Variablen [moeglichst nicht verwenden]
  ! [C.4] Schnittstellen
  ! [C.4.1] erforderliche oeffentliche Schnittstellen
  !
  !! Allokieren/Initialisieren der statischen Datenobjekte des Moduls.<BR>
  INTERFACE init_var
     MODULE PROCEDURE init_var_d ! 
  END INTERFACE
  !! De-Allokieren/Re-Initialisieren der statischen Datenobjekte des Moduls.<BR>
  INTERFACE clear_var
     MODULE PROCEDURE clear_var_d ! 
  END INTERFACE
  !! Logische Kanalnummer f&uuml;r PRINT-Methoden auf Benutzerwert setzen.<BR>
  !! Keine Ausgabe, wenn <EM>PRN_LUN</EM> = -1 .<BR>
  !! Ausgabe nur, wenn <EM>PRN_LUN</EM> = >0 .<BR>
  !! <EM>Hinweis</EM>: <EM>PRN_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_var_prn_lun
     MODULE PROCEDURE setup_var_prn_lun_d ! 
  END INTERFACE
  !! Logische Kanalnummer f&uuml;r TRACE-Methoden auf Benutzerwert setzen.<BR>
  !! Keine Ausgabe, wenn <EM>TRC_LUN</EM> = -1 .<BR>
  !! Ausgabe nur, wenn <EM>TRC_LUN</EM> = >0 .<BR>
  !! <EM>Hinweis</EM>: <EM>TRC_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_var_trc_lun
     MODULE PROCEDURE setup_var_trc_lun_d ! 
  END INTERFACE
  !! Erzeugen von Datenobjekten "t_var" (Skalar, 1D-Array).<BR>
  !! NULLIFY f&uuml;r dynamische Komponenten-Felder.<BR>
  !! Initialisieren mit Default-Werten.
  INTERFACE new_var
     MODULE PROCEDURE new_var_0  ! Version fuer Skalar
     MODULE PROCEDURE new_var_1  ! Version fuer 1D-Array
  END INTERFACE
  !! Vernichten von Datenobjekten "t_var" (Skalar, 1D-Array).<BR>
  !! Ggf. De-Allokieren von Memory.<BR>
  !! Teilweise Re-Initialisieren mit Default-Werten.
  INTERFACE kill_var
     MODULE PROCEDURE kill_var_0 ! Version fuer Skalar
     MODULE PROCEDURE kill_var_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Pr&uuml;fen von Datenobjekten "t_var" (Skalar, 1D-Array).<BR>
  !! Werden mehrere Objekte in der Parameterliste &uuml;bergeben,
  !! so wird auch deren Eindeutigkeit hinsichtlich
  !! der Kennung <EM>id</EM> und des Namens <EM>name</EM> 
  !! gepr&uuml;ft.<BR>
  !! Wird ein Feld mit Dimensionsangaben (Typ:"t_dim") in der
  !! Parameterliste &uuml;bergeben, so wird zusaetzlich geprueft,
  !! ob alle im Komponentenfeld "dim_id" genannten IDs in diesem
  !! Feld vorkommen.
  INTERFACE ok_var
     MODULE PROCEDURE ok_var_0     ! Version fuer Skalar
     MODULE PROCEDURE ok_var_1     ! Version fuer 1D-Array
     MODULE PROCEDURE ok_var_0_dim ! Version fuer Skalar + Pruefung Dim_IDs
     MODULE PROCEDURE ok_var_1_dim ! Version fuer 1D-Array + Pruefung Dim_IDs
  END INTERFACE
  !! Drucken aller Komponenten von Datenobjekten "t_var" (Skalar, 1D-Array).<BR>
  !! Ausgabe auf <EM>PRN_LUN</EM>.<BR>
  !! <EM>Hinweis:</EM> Ausgabe nur, wenn <EM>PRN_LUN</EM> > 0 .
  INTERFACE print_var
     MODULE PROCEDURE print_var_0 ! Version fuer Skalar
     MODULE PROCEDURE print_var_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Drucken aller in diesem Modul abgelegten statischen Daten.<BR>
  !! <EM>Hinweis:</EM> Ausgabe nur, wenn <EM>PRN_LUN</EM> > 0 .
  INTERFACE print_var_static
     MODULE PROCEDURE print_var_static_d ! 
  END INTERFACE
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Moduls.<BR>
  !! Ausgabe auf EM>PRN_LUN</EM>.<BR>
  !! <EM>Hinweis:</EM> Ausgabe nur, wenn <EM>PRN_LUN</EM> > 0 .
  INTERFACE print_var_all_errors
     MODULE PROCEDURE print_var_all_errors_d ! 
  END INTERFACE
  !! Setze alle Komponenten in "t_var" auf Benutzerwerte,
  !! wobei die &Uuml;bergabe eines Wertes f&uuml;r die Komponente
  !! "dim_id" optional ist.
  INTERFACE set_var
     MODULE PROCEDURE set_var_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_var_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !! Setze Komponente "id" in "t_var" auf Benutzerwert <BR>
  !! <EM>id</EM> &#062; 0 muss erf&uuml;llt sein       <BR>
  !! in einem Feld von Variablenangaben darf jeder Wert nur ein Mal vorkommen <BR>
  !! a) eine Id auf Benutzerwert setzen <BR>
  !! b) viele Id's auf Benutzerwerte setzen
  INTERFACE set_var_id
     MODULE PROCEDURE set_var_id_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_var_id_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !! Setze Komponente "name" in "t_var" auf Benutzerwert. <BR>
  !! in einem Feld von Variablenangaben darf ein Name nur ein Mal auftreten <BR>
  !! a) einen Namen auf Benutzerwert setzen <BR>
  !! b) viele Namen auf Benutzerwerte setzen
  INTERFACE set_var_name
     MODULE PROCEDURE set_var_name_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_var_name_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !! Setze Komponente "type" in "t_var" auf Benutzerwert. <BR>
  !! <EM>type</EM> darf nur die in dem Feld "c_var_type" definierten Werte annehmen <BR>
  !! a) einen Namen auf Benutzerwert setzen <BR>
  !! b) viele Namen auf einen Benutzerwert setzen <BR>
  !! c) viele Namen auf viele Benutzerwerte setzen
  INTERFACE set_var_type
     MODULE PROCEDURE set_var_type_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_var_type_1_0 ! Objekt (Vektor) / Daten (Skalar) 
     MODULE PROCEDURE set_var_type_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !! Setze Komponente "con_id" in "t_var" auf Benutzerwert. <BR>
  !! a) eine  ID   auf Benutzerwert        setzen <BR>
  !! b) viele ID's auf einen Benutzerwert  setzen <BR>
  !! c) viele ID's auf viele Benutzerwerte setzen
  INTERFACE set_var_con_id
     MODULE PROCEDURE set_var_con_id_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_var_con_id_1_0 ! Objekt (Vektor) / Daten (Skalar) 
     MODULE PROCEDURE set_var_con_id_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !! Setze Feld-Komponente "dim_id" in "t_var" auf Benutzerwerte. <BR>
  !! Ggf. Allokieren von Memory zur Aufnahme der Benutzer-Daten.
  INTERFACE set_var_dim_id
     MODULE PROCEDURE set_var_dim_id_0_1 ! Objekt (Skalar) / Daten (Vektor)
  END INTERFACE
  !! H&auml;nge eine oder mehrere Variablen an ein Pointer-Feld
  !! des Typs "t_var" an <BR>
  !! a) Anf&uuml;gen einer Gr&ouml;&szlig;e <BR>
  !! b) Anf&uuml;gen mehrerer Gr&ouml;&szlig;en
  INTERFACE add_var
     MODULE PROCEDURE add_var_d_0
     MODULE PROCEDURE add_var_d_1
  END INTERFACE
  !! Hole Komponente "id" aus                                 <BR>
  !! a) dem   Datenobjekt      "t_var"                        <BR>
  !! b) allen Datenobjekten in "t_var(:)"                     <BR>
  !! c) dem   Datenobjekt   in "t_var(:)" mit Namen "name"    <BR>
  !! d) den   Datenobjekten in "t_var(:)" mit Namen "name(:)" <BR>
  !! Ggf. "id"=0, falls gesuchtes Objekt nicht existiert.<BR>
  !! Es wird eine Kopie der Daten zur&uuml;ckgegeben.
  INTERFACE get_var_id
     MODULE PROCEDURE get_var_id_0_0    ! Skalar
     MODULE PROCEDURE get_var_id_1_0    ! Vektor
     MODULE PROCEDURE get_var_id_1_n_0  ! Skalar, Auswahl ueber Namen
     MODULE PROCEDURE get_var_id_1_n_1  ! Vektor, Auswahl ueber Namen
  END INTERFACE
  !! Hole Komponente "name" aus                               <BR>
  !! a) dem   Datenobjekt      "t_var"                        <BR>
  !! b) allen Datenobjekten in "t_var(:)"                     <BR>
  !! c) dem   Datenobjekt   in "t_var(:)" mit der ID "id"     <BR>
  !! d) den   Datenobjekten in "t_var(:)" mit der ID "id(:)"  <BR>
  !! Ggf. "name"='undefined', falls gesuchtes Objekt nicht existiert.<BR>
  !! Es wird eine Kopie der Daten zur&uuml;ckgegeben.
  INTERFACE get_var_name
     MODULE PROCEDURE get_var_name_0_0     ! Skalar
     MODULE PROCEDURE get_var_name_1_0     ! Vektor
     MODULE PROCEDURE get_var_name_1_i_0   ! Skalar, Auswahl ueber ID
     MODULE PROCEDURE get_var_name_1_i_1   ! Vektor, Auswahl ueber ID
  END INTERFACE
  !! Hole Komponente "type" aus                               <BR>
  !! a) dem   Datenobjekt      "t_var"                        <BR>
  !! b) allen Datenobjekten in "t_var(:)"                     <BR>
  !! c) dem   Datenobjekt   in "t_var(:)" mit der ID "id"     <BR>
  !! d) den   Datenobjekten in "t_var(:)" mit der ID "id(:)"  <BR>
  !! e) dem   Datenobjekt   in "t_var(:)" mit Namen "name"    <BR>
  !! f) den   Datenobjekten in "t_var(:)" mit Namen "name(:)" <BR>
  !! Ggf. "type"='undefined', falls gesuchtes Objekt nicht existiert.<BR>
  !! Es wird eine Kopie der Daten zur&uuml;ckgegeben.
  INTERFACE get_var_type
     MODULE PROCEDURE get_var_type_0_0    ! Skalar
     MODULE PROCEDURE get_var_type_1_0    ! Vektor
     MODULE PROCEDURE get_var_type_1_i_0  ! Skalar, Auswahl ueber ID
     MODULE PROCEDURE get_var_type_1_i_1  ! Vektor, Auswahl ueber ID
     MODULE PROCEDURE get_var_type_1_n_0  ! Skalar, Auswahl ueber Namen
     MODULE PROCEDURE get_var_type_1_n_1  ! Vektor, Auswahl ueber Namen
  END INTERFACE
  !! Hole Komponente "con_id" aus                             <BR>
  !! a) dem   Datenobjekt      "t_var"                        <BR>
  !! b) allen Datenobjekten in "t_var(:)"                     <BR>
  !! c) dem   Datenobjekt   in "t_var(:)" mit der ID "id"     <BR>
  !! d) den   Datenobjekten in "t_var(:)" mit der ID "id(:)"  <BR>
  !! e) dem   Datenobjekt   in "t_var(:)" mit Namen "name"    <BR>
  !! f) den   Datenobjekten in "t_var(:)" mit Namen "name(:)" <BR>
  !! Ggf. "con_id"=0, falls gesuchtes Objekt nicht existiert. <BR>
  !! Es wird eine Kopie der Daten zur&uuml;ckgegeben.
  INTERFACE get_var_con_id
     MODULE PROCEDURE get_var_con_id_0_0    ! Skalar
     MODULE PROCEDURE get_var_con_id_1_0    ! Vektor
     MODULE PROCEDURE get_var_con_id_1_i_0  ! Skalar, Auswahl ueber ID
     MODULE PROCEDURE get_var_con_id_1_i_1  ! Vektor, Auswahl ueber ID
     MODULE PROCEDURE get_var_con_id_1_n_0  ! Skalar, Auswahl ueber Namen
     MODULE PROCEDURE get_var_con_id_1_n_1  ! Vektor, Auswahl ueber Namen
  END INTERFACE
  !! Hole Feld-Komponente "dim_id" oder ein bestimmtes Element der
  !! Feld-Komponente "dim_id" aus "t_var".<BR>
  !! a) "dim_id" aus dem Datenobjekt    "t_var"   <BR>
  !! b) "dim_id" aus dem Datenobjekt in "t_var(:)" mit der ID "id"  <BR>
  !! c) "dim_id" aus dem Datenobjekt in "t_var(:)" mit Namen "name" <BR>
  !! d) bestimmte DIM-ID aus dem Datenobjekt    "t_var"   <BR>
  !! e) bestimmte DIM-ID aus dem Datenobjekt in "t_var(:)" mit der ID "id"  <BR>
  !! f) bestimmte DIM-ID aus dem Datenobjekt in "t_var(:)" mit Namen "name" <BR>
  !! g) bestimmte DIM-ID fuer eine gebietsabh. Dim. aus dem Datenobjekt in "t_var" <BR>
  !! Ggf. RESULT=NULLIFY, falls Variable ein Skalar ist.                    <BR>
  !! Ggf. RESULT=NULLIFY, falls gesuchtes Objekt nicht existiert.           <BR>
  !! Ggf. RESULT=0, falls gesuchtes Datenobjekt nicht existiert.            <BR>
  !! Ggf. RESULT=0, falls "dim_id" des Datenobjekts nicht assoziiert.       <BR>
  !! Ggf. RESULT=0, falls Feldelement "idim" nicht existiert.               <BR>
  !! Ggf. RESULT=0, falls keine gebietsabh. Dimension existiert.            <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  INTERFACE get_var_dim_id
     MODULE PROCEDURE get_var_dim_id_0_1   ! Objekt (Skalar)            / Result (Vektor)
     MODULE PROCEDURE get_var_dim_id_1_i_1 ! Objekt-Auswahl ueber ID    / Result (Vektor)
     MODULE PROCEDURE get_var_dim_id_1_n_1 ! Objekt-Auswahl ueber Namen / Result (Vektor)
     MODULE PROCEDURE get_var_spec_dim_id_0_0   ! Objekt (Skalar)            / Result (Skalar)
     MODULE PROCEDURE get_var_spec_dim_id_1_i_0 ! Objekt-Auswahl ueber ID    / Result (Skalar)
     MODULE PROCEDURE get_var_spec_dim_id_1_n_0 ! Objekt-Auswahl ueber Namen / Result (Skalar)
     MODULE PROCEDURE get_var_tg_dim_id_0_1     ! Objekt (Skalar)            / Result (Skalar)
  END INTERFACE
  !! Ermittle die Dimension einer bestimmten Dimension in einem Feld von Dimensionen <BR>
  !! a) f&uuml;r eine Variable und einen bestimmten Namen einer Dimension <BR>
  !! b) f&uuml;r eine Variable und mehrere Dimensions-Namen 
  INTERFACE get_var_dim_idx
     MODULE PROCEDURE get_var_dim_idx_0_0_1
     MODULE PROCEDURE get_var_dim_idx_0_1_1
  END INTERFACE
  !! Ermittle die Position einer Variablen in einem Feld 
  !! f&uuml;r verschiedene Suchkriterien (Id, Name, Variable). <BR>
  !! Falls keine Position ermittelt werden kann, so wird
  !! der Wert 0 zur&uuml;ckgegeben.
  !! a) f&uuml;r eine  Identifikationsnummer  "id"    <BR>
  !! b) f&uuml;r viele Identifikationsnummern "id(:)" <BR>
  !! c) f&uuml;r einen Namen "name"                   <BR>
  !! d) f&uuml;r viele Namen "name(:)"                <BR>
  !! e) f&uuml;r ein   Objekt  vom Typ "t_var"        <BR>
  !! f) f&uuml;r viele Objekte vom Typ "t_var"
  INTERFACE get_var_idx
     MODULE PROCEDURE get_var_idx_i_0
     MODULE PROCEDURE get_var_idx_i_1
     MODULE PROCEDURE get_var_idx_n_0
     MODULE PROCEDURE get_var_idx_n_1
     MODULE PROCEDURE get_var_idx_d_0
     MODULE PROCEDURE get_var_idx_d_1
  END INTERFACE
  !! Ermittle den "Rank" (Dimensionalit&auml;t) der Variablen. <BR>
  !! Ggf. Objektauswahl &uuml;ber Suchkriterium (Id, Name).    <BR>
  !! Ggf. RESULT=-1, falls gesuchtes Variable nicht vorhanden. <BR>
  !! a) f&uuml;r ein   Objekt  vom Typ "t_var" (Skalar) <BR>
  !! b) f&uuml;r alle  Objekte vom Typ "t_var" (Vektor) <BR>
  !! c) f&uuml;r das Objekt  mit Identifikationsnummer  "id"   <BR>
  !! d) f&uuml;r die Objekte mit Identifikationsnummern "id(:)"<BR>
  !! e) f&uuml;r das Objekt  mit Namen "name"                  <BR>
  !! f) f&uuml;r die Objekte mit Namen "name(:)"
  INTERFACE get_var_rank
     MODULE PROCEDURE get_var_rank_0
     MODULE PROCEDURE get_var_rank_1
     MODULE PROCEDURE get_var_rank_i_0
     MODULE PROCEDURE get_var_rank_i_1
     MODULE PROCEDURE get_var_rank_n_0
     MODULE PROCEDURE get_var_rank_n_1
  END INTERFACE
  !! Ermittle die "Shape" einer Variablen. <BR>
  !! Ggf. Objektauswahl &uuml;ber Suchkriterium (Id, Name). <BR>
  !! Ggf. RESULT=NULLIFY, falls Objekt eine skalare Variable ist.<BR>
  !! Ggf. RESULT=NULLIFY, falls gesuchte Variable nicht vorhanden.<BR>
  !! a) f&uuml;r ein Objekt vom Typ "t_var"  (Skalar) <BR>
  !! b) f&uuml;r das Objekt  mit Identifikationsnummer  "id"<BR>
  !! c) f&uuml;r das Objekt  mit Namen "name"
  INTERFACE get_var_shape
     MODULE PROCEDURE get_var_shape_0
     MODULE PROCEDURE get_var_shape_i_0
     MODULE PROCEDURE get_var_shape_n_0
  END INTERFACE
  !! Ermittle die "Size" einer Variablen <BR>
  !! ggf. Objektauswahl &uuml;ber Suchkriterium (Id, Name) <BR>
  !! RESULT =  0, falls es sich um eine skalare Variable handelt <BR>
  !! RESULT = -1, falls die gesuchte Variable nicht vorhanden ist <BR>
  !! a) f&uuml;r ein Objekt vom Typ "t_var"  (Skalar) <BR>
  !! b) f&uuml;r das Objekt  mit Identifikationsnummer  "id"<BR>
  !! c) f&uuml;r das Objekt  mit Namen "name"
  INTERFACE get_var_size
     MODULE PROCEDURE get_var_size_0
     MODULE PROCEDURE get_var_size_i_0
     MODULE PROCEDURE get_var_size_n_0
  END INTERFACE
  !! Ermittle den Startindex einer Variablen. <BR>
  !! Der Startindex wird beim Lesen und Schreiben von Daten
  !! in Zusammenhang mit dem Paket <EM>io_dataset</EM>
  !! ben&ouml;tigt.<BR>
  !! a) Standard Startindex "start(:) = 1" <BR>
  !! b) &Uuml;berschreiben des letzten Index mit einem durch
  !!    den Anwender vorgegebenen Wert
  INTERFACE get_var_start
     MODULE PROCEDURE get_var_start_0
     MODULE PROCEDURE get_var_start_0_i
  END INTERFACE
  !! Ermittle die Anzahl der Indices entlang aller Dimensionen einer Variablen. <BR>
  !! Diese Angaben werden beim Lesen und Schreiben von Daten
  !! in Zusammenhang mit dem Paket <EM>io_dataset</EM>
  !! ben&ouml;tigt. <BR>
  !! a) Standardvorgaben setzen
  INTERFACE get_var_step
     MODULE PROCEDURE get_var_step_0
  END INTERFACE
  !! Ermittle die Schrittweite entlang aller Dimensionen einer Variablen. <BR>
  !! Diese Angaben werden beim Lesen und Schreiben von Daten
  !! in Zusammenhang mit dem Paket <EM>io_dataset</EM>
  !! ben&ouml;tigt.
  !! a) Standardvorgaben setzen
  INTERFACE get_var_stride
     MODULE PROCEDURE get_var_stride_0
  END INTERFACE
  !! Ersetzen eines alten Wertes f&uuml;r eine Dimensions-Id durch
  !! einen neuen Wert. <BR>
  !! Die Ersetzungen werden in einem Feld von Dimensionen (t_dim)
  !! und in einem Feld von Variablen (t_var) in konsistenter Weise
  !! vorgenommen.
  INTERFACE replace_var_dim_id
     MODULE PROCEDURE replace_var_dim_id_1_1
  END INTERFACE
  !! Pr&uuml;fe, ob f&uuml;r einen Namen, eine Id oder
  !! eine Variable ein entsprechender Variablen-Eintrag in 
  !! einem Feld von Variablen "var(:)" (t_var) vorhanden ist.<BR>
  !! a) f&uuml;r eine  Identifikationsnummer "id"       <BR>
  !! b) f&uuml;r viele Identifikationsnummern "id(:)"   <BR>
  !! c) f&uuml;r einen Namen "name"                     <BR>
  !! d) f&uuml;r viele Namen "name(:)"                  <BR>
  !! e) f&uuml;r eine  Variable vom Typ "t_var"         <BR>
  !! f) f&uuml;r viele Variablen vom Typ "t_var"
  INTERFACE var_exists
     MODULE PROCEDURE var_exists_i_0
     MODULE PROCEDURE var_exists_i_1
     MODULE PROCEDURE var_exists_n_0
     MODULE PROCEDURE var_exists_n_1
     MODULE PROCEDURE var_exists_d_0
     MODULE PROCEDURE var_exists_d_1
  END INTERFACE
  !! Ermittle, ob es sich um eine skalare Variable handelt, f&uuml;r :  <BR>
  !! a) ein  Variablen-Objekt  (Skalar) <BR>
  !! b) alle Variablen-Objekte (Vektor) <BR>
  !! c) das  Variablen-Objekt  in "t_var(:)" mit der ID "id"     <BR>
  !! d) die  Variablen-Objekte in "t_var(:)" mit den IDs "id(:)" <BR>
  !! e) das  Variablen-Objekt  in "t_var(:)" mit Namen "name"    <BR>
  !! f) die  Variablen-Objekte in "t_var(:)" mit Namen "name(:)" <BR>
  !! Ggf. RESULT=FALSE, weil gesuchtes Objekt nicht vorhanden ! 
  INTERFACE is_scalar_var
     MODULE PROCEDURE is_scalar_var_0    ! Datenobjekt  (Skalar)
     MODULE PROCEDURE is_scalar_var_1    ! Datenobjekte (Vektor)
     MODULE PROCEDURE is_scalar_var_i_0  ! Objekt-Auswahl ueber ID (Skalar)
     MODULE PROCEDURE is_scalar_var_i_1  ! Objekt-Auswahl ueber ID (Vektor)
     MODULE PROCEDURE is_scalar_var_n_0  ! Objekt-Auswahl ueber Namen (Skalar)
     MODULE PROCEDURE is_scalar_var_n_1  ! Objekt-Auswahl ueber Namen (Vektor)
  END INTERFACE
  !! Emittle, ob es sich um eine skalare Variable mit einer
  !! unbegrenzten (unlimited) Dimension handelt. <BR>
  !! a) ein Objekt (Skalar) <BR>
  !! b) mehrere Objekte (Vektor)
  INTERFACE is_unlimited_var
     MODULE PROCEDURE is_unlimited_var_0 ! 
     MODULE PROCEDURE is_unlimited_var_1 ! 
  END INTERFACE
  !! Erzeuge ggf. eine Fehlermeldung, falls eine Variable mit
  !! vorgegebenem Namen nicht in einer Liste von Variablen vorhanden ist. <BR>
  !! a) falls der Name bekannt ist <BR>
  !! b) falls ein Objekt (t_var) bekannt ist <BR>
  !! Anmerkung: die anderen Typ-Komponenten spielen bei diesem Test keine Rolle.
  INTERFACE gen_error_if_var_not_exists
     MODULE PROCEDURE gen_error_if_var_not_exists_1_n
     MODULE PROCEDURE gen_error_if_var_not_exists_1_0
  END INTERFACE
  ! [C.4.2] optionale oeffentliche Schnittstellen
  ! [C.5] Zuweisungen
  ! [C.6] Operatoren
  ! [C.6.1] unbedingt erforderliche oeffentliche Operatoren
  !! Zuweisung zwischen zwei Variablen des Typs "t_var".
  INTERFACE ASSIGNMENT(=)
     MODULE PROCEDURE as_var_0_0  ! Skalar / Skalar
     MODULE PROCEDURE as_var_1_1  ! Vektor / Vektor
  END INTERFACE
  !! Pr&uuml;fung zweier Datenobjekte "t_var" auf Gleichheit. <BR>
  !! Zwei Variablenbeschreibungen sind gleich, wenn sie in allen 
  !! Komponenten &uuml;bereinstimmen.
  INTERFACE OPERATOR(==)
     MODULE PROCEDURE eq_var_0_0  ! Skalar / Skalar
     MODULE PROCEDURE eq_var_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE eq_var_1_0  ! Vektor / Skalar
     MODULE PROCEDURE eq_var_1_1  ! Vektor / Vektor
  END INTERFACE
  ! [C.6.2] optional vorhandene oeffentliche Operatoren
  !! Pr&uuml;fung zweier Datenobjekte "t_var" auf Ungleichheit
  INTERFACE OPERATOR(/=)
     MODULE PROCEDURE ne_var_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ne_var_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE ne_var_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ne_var_1_1  ! Vektor / Vektor
  END INTERFACE
  ! [C.7] Liste der oeffentlichen Methoden
  ! [C.7.1] unbedingt erforderliche oeffentliche Methoden
  !
  PUBLIC :: init_var                 ! Initialisieren (Modul)
  PUBLIC :: clear_var                ! De-Initialisieren (Modul)
  PUBLIC :: setup_var_prn_lun        ! Setzen prn_lun 
  PUBLIC :: setup_var_trc_lun        ! Setzen trc_lun 
  PUBLIC :: new_var                  ! Erzeugen 
  PUBLIC :: kill_var                 ! Vernichten
  PUBLIC :: ok_var                   ! Pruefen
  PUBLIC :: print_var                ! Drucken
  PUBLIC :: print_var_static         ! Drucken aller statischen Daten
  PUBLIC :: print_var_all_errors     ! Drucken aller (moeglichen) Fehlermeldungen
  PUBLIC :: set_var                  ! Setzen aller Komponenten gleichzeitig
  PUBLIC :: set_var_id               ! Setzen der Komponente id
  PUBLIC :: set_var_name             ! Setzen der Komponente name
  PUBLIC :: set_var_type             ! Setzen der Komponente type
  PUBLIC :: set_var_con_id           ! Setzen der Komponente con_id
  PUBLIC :: set_var_dim_id           ! Setzen der Komponente dim_id
  PUBLIC :: get_var_id               ! Holen der Komponente id
  PUBLIC :: get_var_name             ! Holen der Komponente name
  PUBLIC :: get_var_type             ! Holen der Komponente type
  PUBLIC :: get_var_con_id           ! Holen der Komponente con_id
  PUBLIC :: get_var_dim_id           ! Holen der Komponente dim_id
  PUBLIC :: get_var_dim_idx          ! Holen des Positionsindex einer bestimmten Dimension
  PUBLIC :: OPERATOR(==)             ! Operator "=="
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  PUBLIC :: add_var                  ! Variablen an Liste anhaengen
  PUBLIC :: ASSIGNMENT(=)            ! Zuweisung zwischen zwei Variablen
  PUBLIC :: OPERATOR(/=)             ! Operator "/="
  PUBLIC :: get_var_idx              ! Position einer Variablen
  PUBLIC :: get_var_rank             ! "Rank"  einer Variablen
  PUBLIC :: get_var_shape            ! "Shape" einer Variablen
  PUBLIC :: get_var_size             ! "Size" einer Variablen
  PUBLIC :: get_var_start            ! Start-Index einer Variablen
  PUBLIC :: get_var_step             ! Step-Indices einer Variablen
  PUBLIC :: get_var_stride           ! Schrittweiten einer Variablen
  PUBLIC :: replace_var_dim_id       ! Konsistentes Ersetzen von Dimensions-Id's
  PUBLIC :: var_exists               ! Variablen-Eintrag vorhanden
  PUBLIC :: is_scalar_var            ! Variable ist skalar
  PUBLIC :: is_unlimited_var            ! Variable hat eine "unlimited" Dimension
  PUBLIC :: gen_error_if_var_not_exists ! erzeuge FM falls Variable (Name) nicht vorhanden
  !
  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] lokale Typdefinitionen
  !
  ! [D.2] Konstantwerte (Parameter)
  !
  !! Name des Moduls
  CHARACTER (LEN=31), PARAMETER :: c_modname      = 'b_var' ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.          ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1               ! 
  !! Anzahl der Datenkomponenten des Typs t_var
  INTEGER           , PARAMETER :: c_nofcomp      = 5                ! ggf. modifizieren
  !! maximale Anzahl der g&uuml;tigen typen f&uuml;r Variablen
  INTEGER           , PARAMETER :: c_max_var_type = 6                ! ggf. modifizieren
  !! Definition der g&uuml;ltigen Bezeichner f&uuml;r den Type  <BR>
  !! Position(01) : <EM>IN</EM>, Daten des Typs Integer;        <BR>
  !! Position(02) : <EM>RE</EM>, Daten des Typs Real;           <BR>
  !! Position(03) : <EM>DP</EM>, Daten des Typs Real (Double);  <BR>
  !! Position(04) : <EM>TI</EM>, Daten des Typs "t_datetime";   <BR>
  !! Position(05) : <EM>SH</EM>, Daten des Typs Integer (Short) <BR>
  !! Position(06) : <EM>SH</EM>, Daten des Typs CHARACTER.
  CHARACTER (LEN=c_len_var_type), PARAMETER :: c_var_type(c_max_var_type)= & ! 
       (/ 'IN        ', & ! 01
          'RE        ', & ! 02
          'DP        ', & ! 03
          'TI        ', & ! 04
          'SH        ', & ! 05
          'CH        ' /) ! 06  *** neue Bezeichner hinten anfuegen ***
  !
  ! [D.3] Variablen (statische Daten des Moduls)
  !
  !! Feld zur Aufnahme aller Fehlermeldungen des Moduls
  TYPE (t_error) , ALLOCATABLE, SAVE :: all_errors(:)! 
  !! Indikator f&uuml;r eine erfolgreich durchgef&uuml;hrte Modulinitialisierung
  LOGICAL                , SAVE :: initialised = .false.  ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung PRINT-Methoden
  LOGICAL                , SAVE :: prn_op      = c_op     ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE-Methoden
  LOGICAL                , SAVE :: trc_op      = c_op     ! 
  !! logische Kanalnummer f&uuml;r PRINT-Methoden
  INTEGER                , SAVE :: prn_lun     = c_lun    ! 
  !! logische Kanalnummer f&uuml;r TRACE-Methoden
  INTEGER                , SAVE :: trc_lun     = c_lun    ! 
  !! Z&auml;hler f&uuml;r Initialisierungsaufrufe
  INTEGER                , SAVE :: n_init      = 0        ! 
  !
  ! [D.4] Schnittstellen
  !
  !! Umsetzen eines Textes in Kleinbuchstaben
  INTERFACE get_lowercase_char
     MODULE PROCEDURE get_lowercase_char_0
     MODULE PROCEDURE get_lowercase_char_1
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
  ! Globale Methoden mit Zugriff ueber PUBLIC Interfaces
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-INIT-Methoden <<< [ERR_NO =  1000 bis  1999]
  ! ----------------------------------------------------------------------
  !
  !! Allokieren/Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE init_var_d ( )
    !
    USE b_error, ONLY : DEBUG_b
    !! Name der Subroutine
    CHARACTER (LEN=10), PARAMETER :: c_upname='init_var_d' 
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_b > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "b_var" version 1.21 of 04/11/06                 '
          WRITE(*,*) ' Copyright (C) 2002 Bundesanstalt fuer Wasserbau   '
          WRITE(*,*)
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       IF ( no_error( ) ) CALL init_dim ( )
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_var_all_errors ( ) 
       ! [1.5] Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.6] endgueltiges Setzen des Initialisierungs-Indikators
       initialised = MERGE( .true., .false., no_error( ) )
    END IF
    ! 2.0 Initialisierungszaehler heraufsetzen
    n_init = n_init + 1
    !
  END SUBROUTINE init_var_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_var_d ( )
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER :: c_upname='clear_var_d' ! 
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_var_all_errors ( ) 
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.3] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.4] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.4.1] ggf. weitere Module de-initialisieren
       IF ( no_error( ) ) CALL clear_dim ( )
       ! [1.4.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
    !
  END SUBROUTINE clear_var_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_var_prn_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER :: c_upname='setup_var_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_dim_prn_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_var_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_var_trc_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER :: c_upname='setup_var_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_dim_trc_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_var_trc_lun_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_var_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_var) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=09), PARAMETER :: c_upname='new_var_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       this%id      = 0
       this%name    = REPEAT( ' ', LEN( this%name ) )
       this%name    = 'undefined'
       this%type    = REPEAT( ' ', LEN( this%type ) )
       this%type    = 'undefined'
       this%con_id  = 0
       NULLIFY ( this%dim_id )
    END IF
    !
  END SUBROUTINE new_var_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_var_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=09), PARAMETER :: c_upname='new_var_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       DO i = 1, SIZE(this)
          IF ( any_error( ) ) EXIT
          CALL new_var_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_var_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_var_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_var) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=10), PARAMETER :: c_upname='kill_var_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       IF ( no_error( ) ) CALL dealloc_var_dim_id ( this )
       IF ( no_error( ) ) CALL new_var_0 ( this )
    END IF
    !
  END SUBROUTINE kill_var_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_var_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=10), PARAMETER :: c_upname='kill_var_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       DO i = 1, SIZE(this)
          IF ( any_error( ) ) EXIT
          CALL kill_var_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_var_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_var_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_var) , INTENT(IN) :: this ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=08), PARAMETER :: c_upname='ok_var_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok = .false.
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok(1)  = ok_var_id     ( this )
       l_ok(2)  = ok_var_name   ( this )
       l_ok(3)  = ok_var_type   ( this )
       l_ok(4)  = ok_var_con_id ( this )
       l_ok(5)  = ok_var_dim_id ( this )
    END IF
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_var_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_var_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=08), PARAMETER :: c_upname='ok_var_1'
    !! lokales logisches Feld
    LOGICAL :: l_ok(SIZE(this)) ! 
    !! Z&auml;hler      
    INTEGER :: i ! 
    !
    ok = .false.
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       DO i = 1, SIZE(this)
          ok(i) = ok_var_0 ( this(i) )
       END DO
       ! alle Id's muessen verschieden sein
       l_ok(:) = ok_var_different_id ( this(:) )
       WHERE ( .NOT. l_ok(:) )
          ok(:) = l_ok(:)
       END WHERE
       ! alle Namen muessen verschieden sein
       l_ok(:) = ok_var_different_name ( this(:) )
       WHERE ( .NOT. l_ok(:) )
          ok(:) = l_ok(:)
       END WHERE
    END IF
    !
  END FUNCTION ok_var_1
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Zusaetzlich pruefen, ob alle im Komponentenfeld "dim_id" genannten IDs
  !! im Feld mit den Dimensionsangaben (Typ: t_dim) vorhanden sind.
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_var_0_dim ( this, dim ) &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_var) , INTENT(IN) :: this   ! 
    !! Feld mit Dimensionsangaben
    TYPE (t_dim) , INTENT(IN) :: dim(:) ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=12), PARAMETER :: c_upname='ok_var_0_dim' 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    ok = .false.
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       ok = ok_var_0 ( this )
       IF ( ok  .AND.  ASSOCIATED( this%dim_id) ) THEN
          ok = ok_var_dim_id_exists ( this, dim )
       END IF
    END IF
    !
  END FUNCTION ok_var_0_dim
  !
  !! Pr&uuml;fe ob g&uuml;ltige Datenobjekte vorliegen (Vektor) <BR>
  !! Zusaetzlich pruefen, ob alle im Komponentenfeld "dim_id" genannten IDs
  !! im Feld mit den Dimensionsangaben (Typ: t_dim) vorhanden sind.
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_var_1_dim ( this, dim ) &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:) ! 
    !! Feld mit Dimensionsangaben
    TYPE (t_dim) , INTENT(IN) :: dim(:)  ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=12), PARAMETER :: c_upname='ok_var_1_dim' 
    !! Z&auml;hler      
    INTEGER :: i ! 
    !
    ok = .false.
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       ok(:) = ok_var_1 ( this(:) )
       DO i = 1, SIZE(this)
          IF ( ok(i)  .AND.  ASSOCIATED( this(i)%dim_id) ) THEN
             ok(i) = ok_var_dim_id_exists ( this(i), dim )
          END IF
       END DO
    END IF
    !
  END FUNCTION ok_var_1_dim
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_var_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_var) , INTENT(IN) :: this ! 
    !! Name der Function
    CHARACTER (LEN=11), PARAMETER :: c_upname='print_var_0' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7001, c_upname, c_modname, stat )
       !
       IF ( no_error( ) ) CALL print_var_id( this )
       IF ( no_error( ) ) CALL print_var_name( this )
       IF ( no_error( ) ) CALL print_var_type( this )
       IF ( no_error( ) ) CALL print_var_con_id( this )
       IF ( no_error( ) ) CALL print_var_dim_id( this )
       !
       IF ( no_error( ) ) THEN
          !
          WRITE &
               ( UNIT    = prn_lun,  &
                 FMT     = 8001,     & 
                 IOSTAT  = stat )
          !
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7002, c_upname, c_modname, stat )
          !
       END IF
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT('# Beginn Objekt t_var ------------------------------')
8001 FORMAT('# Ende   Objekt t_var ------------------------------')
    !
  END SUBROUTINE print_var_0
  !
  !! Drucke den Inhalt mehrerer Datenobjekte (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_var_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:) ! 
    !! Name der Function
    CHARACTER (LEN=11), PARAMETER :: c_upname='print_var_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       DO i = 1, SIZE(this)
          !
          IF ( any_error( ) ) EXIT
          !
          WRITE &
               ( UNIT    = prn_lun,  &
                 FMT     = 8000,     & 
                 IOSTAT  = stat ) i
          !
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7003, c_upname, c_modname, stat )
          !
          IF ( no_error( ) ) CALL print_var_0 ( this(i) )
          !
       END DO
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT ('# Datenobjekt-Index i = ',I10.10,' ---------------------------')
    !
  END SUBROUTINE print_var_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_var_static_d ( )
    !! Name der Function
    CHARACTER (LEN=18), PARAMETER :: c_upname='print_var_static_d' 
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )    &
           initialised, prn_op, trc_op, prn_lun, trc_lun, n_init, c_max_var_type
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       !
       WRITE(*,*) ' *** routine '//TRIM( c_upname )//' , code missing :'
       ! ACHTUNG : Format muss dann auch angepasst werden !
       WRITE(*,*) '     -> Liste erlaubter Variablen-Namen muss ggf. noch gedruckt werden '
       !
       DO i=1,c_max_var_type
          IF ( any_error( ) ) EXIT
          WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i, TRIM( c_var_type(i) )
       END DO
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       !
       IF ( no_error( ) ) WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       !
       IF ( no_error( ) ) CALL print_var_all_errors_d ( )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls b_var         ',/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
    '#    initialised = ',L1,/ &
    '#         prn_op = ',L1,/ &
    '#         trc_op = ',L1,/ &
    '#        prn_lun = ',I5,/ &
    '#        trc_lun = ',I5,/ &
    '#         n_init = ',I5,/ &
    '# c_max_var_type = ',I5,/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -')
8001 FORMAT( '#  i, c_var_type : idx = ',I5,', type = ',A )
8002 FORMAT( &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
    '#------------------------------------------------------------') 
    !
  END SUBROUTINE print_var_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_var_all_errors_d ( )
    !! Name der Function
    CHARACTER (LEN=22), PARAMETER :: c_upname='print_var_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_var_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! weise allen Komponenten des Objektes vom Typ "t_var" (Skalar) <BR>
  !! einen skalaren Benutzerwert zu. <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_var_0_0 ( this, id, name, type, dim_id, con_id )
    !! Datenobjekt (Skalar)
    TYPE (t_var)  , INTENT(INOUT)     :: this ! 
    !! Wert f&uuml;r Komponente "id"
    INTEGER       , INTENT(IN)        :: id   ! 
    !! Wert f&uuml;r Komponente "name"
    CHARACTER (LEN=*) , INTENT(IN)    :: name  ! 
    !! Wert f&uuml;r Komponente "type"
    CHARACTER (LEN=*) , INTENT(IN)    :: type  ! 
    !! Wert f&uuml;r Komponente "dim_id"
    INTEGER , OPTIONAL, INTENT(IN)    :: dim_id(:) ! 
    !! Wert f&uuml;r Komponente "con_id"
    INTEGER , OPTIONAL, INTENT(IN)    :: con_id ! 
    ! 
    CALL set_var_id_0_0   ( this, id )
    CALL set_var_name_0_0 ( this, name )
    CALL set_var_type_0_0 ( this, type )
    IF ( PRESENT( dim_id ) ) THEN
       CALL set_var_dim_id_0_1 ( this, dim_id(:) )
    END IF
    IF ( PRESENT( con_id ) ) THEN
       CALL set_var_con_id_0_0 ( this, con_id )
    END IF
    !
  END SUBROUTINE set_var_0_0
  !
  !! weise allen Komponenten der Objekte vom Typ "t_var" (Vektor) <BR>
  !! einen Benutzerwert (Vektor) zu. <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_var_1_1 ( this, id, name, type, dim_id, con_id )
    !! Datenobjekt (Vektor)
    TYPE (t_var)  , INTENT(INOUT) :: this(:)   ! 
    !! Werte f&uuml;r Komponente "id"
    INTEGER       , INTENT(IN)    :: id(:)     ! 
    !! Werte f&uuml;r Komponente "name"
    CHARACTER (LEN=*) , INTENT(IN)    :: name(:)   ! 
    !! Werte f&uuml;r Komponente "type"
    CHARACTER (LEN=*) , INTENT(IN)    :: type(:)   ! 
    !! Wert  f&uuml;r Komponente "dim_id"
    INTEGER , OPTIONAL, INTENT(IN)    :: dim_id(:) ! 
    !! Werte f&uuml;r Komponente "con_id"
    INTEGER , OPTIONAL, INTENT(IN)    :: con_id(:) ! 
    !
    !! Z&auml;hler
    INTEGER :: i !
    !
    CALL set_var_id_1_1   ( this, id   )
    CALL set_var_name_1_1 ( this, name )
    CALL set_var_type_1_1 ( this, type )
    IF ( PRESENT( dim_id ) ) THEN
       DO i = 1, SIZE(this)
          CALL set_var_dim_id_0_1 ( this(i), dim_id )
       ENDDO
    END IF
    IF ( PRESENT( con_id ) ) THEN
       CALL set_var_con_id_1_1 ( this, con_id )
    END IF
    !
  END SUBROUTINE set_var_1_1
  !
  !! weise der Komponente "id" einen skalaren Wert zu (Skalar) <BR>
  !! <EM>id</EM> &#062; 0 muss erf&uuml;llt sein                              <BR>
  !! in einem Feld von Variablenangaben darf jeder Wert nur ein Mal vorkommen <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_var_id_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_var) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "id"
    INTEGER      , INTENT(IN)    :: val  ! 
    !
    this%id = val
    !
  END SUBROUTINE set_var_id_0_0
  !
  !! weise der Komponente "id" eines Feldes Werte aus einem Vektor zu  <BR>
  !! die Zuweisung erfolgt nur f&uuml;r den Fall dass beide Felder gleich gro&szlig; sind <BR>
  !! <EM>id</EM> &#062; 0 muss erf&uuml;llt sein                               <BR>
  !! in einem Feld von Variablenangaben darf jeder Wert nur ein Mal vorkommen <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_var_id_1_1 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "id" (Vektor)
    INTEGER      , INTENT(IN)    :: val(:)  ! 
    !
    IF ( SIZE(this(:)) == SIZE(val(:)) ) this(:)%id = val(:)
    !
  END SUBROUTINE set_var_id_1_1
  !
  !! weise der Komponente "name" einen skalaren Wert zu (Skalar) <BR>
  !! in einem Feld von Variablenangaben darf ein Name nur ein Mal auftreten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_var_name_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_var)      , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "name"
    CHARACTER (LEN=*) , INTENT(IN)    :: val  ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    this%name = REPEAT( ' ', LEN(this%name) )
    this%name = val(1:MIN(LEN(val),LEN(this%name)))
    DO i=1,LEN_TRIM(this%name) ! Blanks durch "underscore" ersetzen
       IF ( this%name(i:i) == ' ' ) this%name(i:i) = '_'
    END DO
    !
  END SUBROUTINE set_var_name_0_0
  !
  !! weise der Komponente "name" eines Feldes Werte aus einem Vektor zu <BR>
  !! die Zuweisung erfolgt nur f&uuml;r den Fall dass beide Felder gleich gro&szlig; sind <BR>
  !! in einem Feld von Variablenangaben darf ein Name nur ein Mal auftreten <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_var_name_1_1 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_var)      , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "name" (Vektor)
    CHARACTER (LEN=*) , INTENT(IN)    :: val(:)  ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i = 1, MIN(SIZE(this),SIZE(val))
       CALL set_var_name_0_0 ( this(i), val(i) )
    END DO
    !
  END SUBROUTINE set_var_name_1_1
  !
  !! weise der Komponente "type" einen skalaren Wert zu (Skalar) <BR>
  !! <EM>type</EM> darf nur die in dem Feld "c_var_type" definierten Werte annehmen <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_var_type_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_var)       , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "type"
    CHARACTER (LEN=*) , INTENT(IN)    :: val  ! 
    !
    this%type = REPEAT( ' ', LEN(this%type) )
    this%type = val(1:MIN(LEN(val),LEN(this%type)))
    !
  END SUBROUTINE set_var_type_0_0
  !
  !! weise der Komponente "type" eines Feldes Werte aus einem Skalar zu <BR>
  !! <EM>type</EM> darf nur die in dem Feld "c_var_type" definierten Werte annehmen <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_var_type_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_var)       , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "type"
    CHARACTER (LEN=*) , INTENT(IN)    :: val     ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_var_type_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_var_type_1_0
  !
  !! weise der Komponente "type" eines Feldes Werte aus einem Vektor zu  <BR>
  !! die Zuweisung erfolgt nur f&uuml;r den Fall dass beide Felder gleich gro&szlig; sind <BR>
  !! <EM>type</EM> darf nur die in dem Feld "c_var_type" definierten Werte annehmen <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_var_type_1_1 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_var)       , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "type"
    CHARACTER (LEN=*) , INTENT(IN)    :: val(:)  ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,MIN(SIZE(this),SIZE(val))
       CALL set_var_type_0_0 ( this(i), val(i) )
    END DO
    !
  END SUBROUTINE set_var_type_1_1
  !
  !! weise der Komponente "con_id" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_var_con_id_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_var)       , INTENT(INOUT) :: this
    !! Wert f&uuml;r Komponente "con_id"
    INTEGER            , INTENT(IN)    :: val ! 
    !
    this%con_id = val
    !
  END SUBROUTINE set_var_con_id_0_0
  !
  !! weise der Komponente "con_id" eines Feldes Werte aus einem Skalar zu <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_var_con_id_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_var)       , INTENT(INOUT) :: this(:)
    !! Wert f&uuml;r Komponente "con_id"
    INTEGER            , INTENT(IN)    :: val
    !! Z&auml;hlervariable
    INTEGER :: i
    !
    DO i=1,SIZE(this)
       CALL set_var_con_id_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_var_con_id_1_0
  !
  !! weise der Komponente "con_id" eines Feldes Werte aus einem Vektor zu  <BR>
  !! die Zuweisung erfolgt nur f&uuml;r den Fall dass beide Felder gleich gro&szlig; sind <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_var_con_id_1_1 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_var)       , INTENT(INOUT) :: this(:)
    !! Werte f&uuml;r Komponente "con_id"
    INTEGER            , INTENT(IN)    :: val(:)
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,MIN(SIZE(this),SIZE(val))
       CALL set_var_con_id_0_0 ( this(i), val(i) )
    END DO
    !
  END SUBROUTINE set_var_con_id_1_1
  !
  !! weise der dynamischen Komponente "dim_id" einen Vektor zu <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_var_dim_id_0_1 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_var) , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "dim_id"
    INTEGER           , INTENT(IN)  :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='set_var_dim_id_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_var_dim_id ( this            )
       IF ( no_error( ) ) CALL alloc_var_dim_id   ( this, SIZE(val) )
       IF ( no_error( ) ) CALL init_var_dim_id    ( this            )
       IF ( no_error( ) ) this%dim_id(:) = val(:)
    END IF
    !
  END SUBROUTINE set_var_dim_id_0_1
  !
  !! Anh&auml;ngen einer Variable an ein Pointer-Feld mit Variablenangaben
  SUBROUTINE add_var_d_0 ( this1, this2 )
    !! Pointer-Feld an das eine weitere Variable angeh&auml;ngt werden soll
    TYPE (t_var) , POINTER     :: this1(:) !
    !! anzuh&auml;ngende Variable
    TYPE (t_var) , INTENT(IN)  :: this2    !
    !! Name der Subroutine
    CHARACTER (LEN=11) , PARAMETER :: c_upname='add_var_d_0' ! 
    !! lokales Hilfsfeld
    TYPE (t_var) , ALLOCATABLE :: this3(:) ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsvariable
    INTEGER :: n    ! 
    !
    IF ( ASSOCIATED( this1 ) ) THEN
       n = SIZE(this1)
       ALLOCATE( this3(n), STAT=stat )
       IF ( stat /= 0   ) THEN
          CALL setup_error_act ( all_errors(:), 8500, c_upname, c_modname, stat )
       ELSE
          CALL new_var( this3(:) )
          this3(:) = this1(:)
          DEALLOCATE ( this1, STAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 8501, c_upname, c_modname, stat )
          ELSE
             NULLIFY ( this1 )
          END IF
       END IF
    ELSE
       n = 0
    END IF
    IF ( no_error( ) ) THEN
       ALLOCATE ( this1(n+1), STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 8502, c_upname, c_modname, stat )
       ELSE
          IF ( ALLOCATED( this3 ) ) THEN
             this1(1:n) = this3(1:n)
             this1(n+1) = this2  
             DEALLOCATE ( this3, STAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 8503, c_upname, c_modname, stat )
             END IF
          ELSE
             this1(n+1) = this2
          END IF
       END IF
    END IF
    !
  END SUBROUTINE add_var_d_0
  !
  !! Anh&auml;ngen mehrerer Variablen an ein Pointer-Feld mit Variablenangaben
  SUBROUTINE add_var_d_1 ( this1, this2 )
    !! Pointer-Feld an das weitere Variablen angeh&auml;ngt werden sollen
    TYPE (t_var) , POINTER    :: this1(:) !
    !! anzuh&auml;ngende Variablen
    TYPE (t_var) , INTENT(IN) :: this2(:) !
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i = 1, SIZE(this2)
       IF ( any_error( ) ) EXIT
       CALL add_var_d_0 ( this1, this2(i) )
    END DO
    !
  END SUBROUTINE add_var_d_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! hole die Komponente "id" aus einem skalaren Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben         <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_id_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_var) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "id" (Skalar)
    INTEGER :: val  ! 
    !
    val = this%id
    !
  END FUNCTION get_var_id_0_0
  !
  !! hole die Komponente "id" aus einem vektoriellen Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben             <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_id_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "id"
    INTEGER :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%id
    !
  END FUNCTION get_var_id_1_0
  !
  !! hole die Komponente "id" aus dem Datenobjekt in "t_var(:)"
  !! mit Namen "name"  <BR>
  !! "id"=0, falls gesuchtes Objekt nicht existiert  <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_id_1_n_0 ( this, name ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_var)      , INTENT(IN) :: this(:) ! 
    !! Name des gesuchten Objekts
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! R&uuml;ckgabewert "id" : id=0, falls nicht gefunden
    INTEGER :: val  ! 
    !
    !! Index des Objekts : idx=0, falls nicht gefunden
    INTEGER :: idx ! 
    !
    idx = get_var_idx_n_0 ( this, name )
    IF ( idx > 0 ) THEN
       val = this(idx)%id
    ELSE
       val = 0
    END IF
    !
  END FUNCTION get_var_id_1_n_0
  !
  !! hole die Komponente "id" aus den Datenobjekten in "t_var(:)"
  !! mit Namen "name(:)" <BR>
  !! "id"=0, falls gesuchtes Objekt nicht existiert  <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_id_1_n_1 ( this, name ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_var)      , INTENT(IN)  :: this(:) ! 
    !! Namen der gesuchten Objekte
    CHARACTER (LEN=*) , INTENT(IN)  :: name(:)   ! 
    !! R&uuml;ckgabewert "id" : id=0, falls nicht gefunden
    INTEGER :: val(SIZE(name))  ! 
    !
    !! Indizes der gesuchten Objekte : idx=0, falls nicht gefunden
    INTEGER :: idx(SIZE(name)) ! 
    !! Z&auml;hler      
    INTEGER :: i ! 
    !
    idx = get_var_idx_n_1 ( this, name )
    val = 0
    DO i = 1, SIZE(idx)
       IF ( idx(i) > 0 ) val(i) = this(idx(i))%id
    END DO
    !
  END FUNCTION get_var_id_1_n_1
  !
  !! hole die Komponente "name" aus einem skalaren Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben           <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_name_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_var) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "name" (Skalar)
    CHARACTER (LEN=LEN(this%name)) :: val  ! 
    !
    val = this%name
    !
  END FUNCTION get_var_name_0_0
  !
  !! hole die Komponente "name" aus einem vektoriellen Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben           <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_name_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "name"
    CHARACTER (LEN=LEN(this%name)) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%name
    !
  END FUNCTION get_var_name_1_0
  !
  !! hole die Komponente "name" aus dem Datenobjekt in "t_var(:)"
  !! mit der ID "id"  <BR>
  !! "name"='undefined', falls gesuchtes Objekt nicht existiert  <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_name_1_i_0 ( this, id ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN)  :: this(:) ! 
    !! Identifikationsnummer des gesuchten Objekts
    INTEGER      , INTENT(IN)  :: id      ! 
    !! R&uuml;ckgabewert "name" : name='undefined', falls nicht gefunden
    CHARACTER (LEN=LEN(this%name)) :: val ! 
    !
    !! Index des Objekts : idx=0, falls nicht gefunden
    INTEGER :: idx ! 
    !
    val = REPEAT( ' ', LEN( val ) )
    idx = get_var_idx_i_0  ( this, id )
    IF ( idx > 0 ) THEN
       val = this(idx)%name
    ELSE
       val = 'undefined'
    END IF
    !
  END FUNCTION get_var_name_1_i_0
  !
  !! hole die Komponente "name" aus den Datenobjekten in "t_var(:)"
  !! mit den IDs "id(:)"  <BR>
  !! "name"='undefined', falls gesuchtes Objekt nicht existiert <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben            <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_name_1_i_1 ( this, id ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN)  :: this(:) ! 
    !! Identifikationsnummern der gesuchten Objekte
    INTEGER      , INTENT(IN)  :: id(:)   ! 
    !! R&uuml;ckgabewert "name" : name='undefined', falls nicht gefunden
    CHARACTER (LEN=LEN(this%name)) :: val(SIZE(id))  ! 
    !
    !! Index eines gesuchten Objektes : idx=0, falls nicht gefunden
    INTEGER  :: idx ! 
    !! Z&auml;hler      
    INTEGER  :: i ! 
    !
    val(:) = REPEAT(' ', LEN( val ))
    !
    DO i = 1, SIZE(id)
       idx = get_var_idx_i_0 (this, id(i))
       IF ( idx > 0 ) THEN
          val(i) = this( idx )%name
       ELSE
          val(i) = 'undefined'
       END IF
    END DO
    !
  END FUNCTION get_var_name_1_i_1
  !
  !! hole die Komponente "type" aus einem skalaren Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben           <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_type_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_var) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "type" (Skalar)
    CHARACTER (LEN=LEN(this%type)) :: val  ! 
    !
    val = this%type
    !
  END FUNCTION get_var_type_0_0
  !
  !! hole die Komponente "type" aus einem vektoriellen Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben           <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_type_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "type"
    CHARACTER (LEN=LEN(this%type)) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%type
    !
  END FUNCTION get_var_type_1_0
  !
  !! hole die Komponente "type" aus dem Datenobjekt in "t_var(:)"
  !! mit der ID "id"  <BR>
  !! "type"='undefined', falls gesuchtes Objekt nicht existiert  <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_type_1_i_0 ( this, id ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN)  :: this(:) ! 
    !! Identifikationsnummer des gesuchten Objekts
    INTEGER      , INTENT(IN)  :: id      ! 
    !! R&uuml;ckgabewert "type" : type='undefined', falls nicht gefunden
    CHARACTER (LEN=LEN(this%type)) :: val ! 
    !
    !! Index des Objekts : idx=0, falls nicht gefunden
    INTEGER :: idx ! 
    !
    val = REPEAT( ' ', LEN( val ) )
    idx = get_var_idx_i_0  ( this, id )
    IF ( idx > 0 ) THEN
       val = this(idx)%type
    ELSE
       val = 'undefined'
    END IF
    !
  END FUNCTION get_var_type_1_i_0
  !
  !! hole die Komponente "type" aus den Datenobjekten in "t_var(:)"
  !! mit der ID "id"  <BR>
  !! "type"='undefined', falls gesuchtes Objekt nicht existiert <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben            <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_type_1_i_1 ( this, id ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN)  :: this(:) ! 
    !! Identifikationsnummern der gesuchten Objekte
    INTEGER      , INTENT(IN)  :: id(:)   ! 
    !! R&uuml;ckgabewert "type" : type='undefined', falls nicht gefunden
    CHARACTER (LEN=LEN(this%type)) :: val(SIZE(id))  ! 
    !
    !! Indizes der gesuchten Objekte : idx=0, falls nicht gefunden
    INTEGER :: idx(SIZE(id)) ! 
    !! Z&auml;hler      
    INTEGER :: i ! 
    !
    val(:) = REPEAT(' ', LEN( val ))
    !
    idx    = get_var_idx_i_1  ( this, id )
    DO i = 1, SIZE(idx)
       IF ( idx(i) > 0 ) THEN
          val(i) = this(idx(i))%type
       ELSE
          val(i) = 'undefined'
       END IF
    END DO
    !
  END FUNCTION get_var_type_1_i_1
  !
  !! hole die Komponente "type" aus dem Datenobjekt in "t_var(:)"
  !! mit Namen "name"  <BR>
  !! "type"='undefined', falls gesuchtes Objekt nicht existiert  <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_type_1_n_0 ( this, name ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_var)      , INTENT(IN) :: this(:) ! 
    !! Name des gesuchten Objekts
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! R&uuml;ckgabewert "type" : type='undefined', falls nicht gefunden
    CHARACTER (LEN=LEN(this%type)) :: val  ! 
    !
    !! Index des Objekts : idx=0, falls nicht gefunden
    INTEGER :: idx ! 
    !
    val = REPEAT( ' ', LEN( val ) )
    idx = get_var_idx_n_0 ( this, name )
    IF ( idx > 0 ) THEN
       val = this(idx)%type
    ELSE
       val = 'undefined'
    END IF
    !
  END FUNCTION get_var_type_1_n_0
  !
  !! hole die Komponente "type" aus den Datenobjekten in "t_var(:)"
  !! mit Namen "name(:)" <BR>
  !! "type"='undefined', falls gesuchtes Objekt nicht existiert <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_type_1_n_1 ( this, name ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_var)      , INTENT(IN)  :: this(:) ! 
    !! Namen der gesuchten Objekte
    CHARACTER (LEN=*) , INTENT(IN)  :: name(:)   ! 
    !! R&uuml;ckgabewert "type" : type='undefined', falls nicht gefunden
    CHARACTER (LEN=LEN(this%type)) :: val(SIZE(name))  ! 
    !
    !! Indizes der gesuchten Objekte : idx=0, falls nicht gefunden
    INTEGER :: idx(SIZE(name)) ! 
    !! Z&auml;hler      
    INTEGER :: i ! 
    !
    val(:) = REPEAT(' ', LEN( val ))
    !
    idx = get_var_idx_n_1 ( this, name )
    DO i = 1, SIZE(idx)
       IF ( idx(i) > 0 ) THEN
          val(i) = this(idx(i))%type
       ELSE
          val(i) = 'undefined'
       END IF
    END DO
    !
  END FUNCTION get_var_type_1_n_1
  !
  !! hole die Komponente "con_id" aus einem skalaren Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben             <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_con_id_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_var) , INTENT(IN) :: this
    !! R&uuml;ckgabewert "con_id" (Skalar)
    INTEGER                   :: val
    !
    val = this%con_id
    !
  END FUNCTION get_var_con_id_0_0
  !
  !! hole die Komponente "con_id" aus einem vektoriellen Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben                 <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_con_id_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:)
    !! R&uuml;ckgabewert "con_id"
    INTEGER                   :: val(SIZE(this))
    !
    val(:) = this(:)%con_id
    !
  END FUNCTION get_var_con_id_1_0
  !
  !! hole die Komponente "con_id" aus dem Datenobjekt in "t_var(:)"
  !! mit der ID "id"                                     <BR>
  !! "con_id"=0, falls gesuchtes Objekt nicht existiert  <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben     <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_con_id_1_i_0 ( this, id ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:)
    !! Identifikationsnummer des gesuchten Objekts
    INTEGER      , INTENT(IN) :: id
    !! R&uuml;ckgabewert "con_id" : con_id=0, falls nicht gefunden
    INTEGER                   :: val
    !
    !! Index des Objekts : idx=0, falls nicht gefunden
    INTEGER :: idx
    !
    val = 0
    !
    idx = get_var_idx_i_0  ( this, id )
    IF ( idx > 0 ) val = this(idx)%con_id
    !
  END FUNCTION get_var_con_id_1_i_0
  !
  !! hole die Komponente "con_id" aus den Datenobjekten in "t_var(:)"
  !! mit der ID "id"                                    <BR>
  !! "con_id"=0, falls gesuchtes Objekt nicht existiert <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben    <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_con_id_1_i_1 ( this, id ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:)
    !! Identifikationsnummern der gesuchten Objekte
    INTEGER      , INTENT(IN) :: id(:)
    !! R&uuml;ckgabewert "con_id" : con_id=0, falls nicht gefunden
    INTEGER                   :: val(SIZE(id))
    !
    !! Indizes der gesuchten Objekte : idx=0, falls nicht gefunden
    INTEGER :: idx(SIZE(id))
    !! Z&auml;hler      
    INTEGER :: i
    !
    val(:) = 0
    !
    idx    = get_var_idx_i_1 ( this, id )
    DO i = 1, SIZE(idx)
       IF ( idx(i) > 0 ) val(i) = this(idx(i))%con_id
    END DO
    !
  END FUNCTION get_var_con_id_1_i_1
  !
  !! hole die Komponente "con_id" aus dem Datenobjekt in "t_var(:)"
  !! mit Namen "name"                                    <BR>
  !! "con_id"=0, falls gesuchtes Objekt nicht existiert  <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben     <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_con_id_1_n_0 ( this, name ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_var)      , INTENT(IN) :: this(:)
    !! Name des gesuchten Objekts
    CHARACTER (LEN=*) , INTENT(IN) :: name
    !! R&uuml;ckgabewert "con_id" : con_id=0, falls nicht gefunden
    INTEGER                        :: val
    !
    !! Index des Objekts : idx=0, falls nicht gefunden
    INTEGER :: idx
    !
    val = 0
    !
    idx = get_var_idx_n_0 ( this, name )
    IF ( idx > 0 ) val = this(idx)%con_id
    !
  END FUNCTION get_var_con_id_1_n_0
  !
  !! hole die Komponente "con_id" aus den Datenobjekten in "t_var(:)"
  !! mit Namen "name(:)"                                <BR>
  !! "con_id"=0, falls gesuchtes Objekt nicht existiert <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben    <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_con_id_1_n_1 ( this, name ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_var)      , INTENT(IN)  :: this(:)
    !! Namen der gesuchten Objekte
    CHARACTER (LEN=*) , INTENT(IN)  :: name(:)
    !! R&uuml;ckgabewert "con_id" : con_id=0, falls nicht gefunden
    INTEGER                          :: val(SIZE(name))
    !
    !! Indizes der gesuchten Objekte : idx=0, falls nicht gefunden
    INTEGER :: idx(SIZE(name))
    !! Z&auml;hler      
    INTEGER :: i
    !
    val(:) = 0
    !
    idx = get_var_idx_n_1 ( this, name )
    DO i = 1, SIZE(idx)
       IF ( idx(i) > 0 ) val(i) = this(idx(i))%con_id
    END DO
    !
  END FUNCTION get_var_con_id_1_n_1
  !
  !! hole die dynamische Feld-Komponente "dim_id" aus einem skalaren Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_var_dim_id_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_var) , INTENT(IN)  :: this     ! 
    !! R&uuml;ckgabewert "dim_id" (Vektor) Pointer
    INTEGER , POINTER :: val(:) ! 
    !! Name der Function
    CHARACTER (LEN=18), PARAMETER :: c_upname='get_var_dim_id_0_1' 
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler      
    INTEGER :: i ! 
    !
    NULLIFY ( val )
    !
    IF ( ASSOCIATED(this%dim_id) ) THEN
       ALLOCATE ( val(SIZE(this%dim_id)), STAT=stat )
       IF ( stat /= 0 ) THEN
          ! Fehler beim Allokieren
          CALL setup_error_act ( all_errors(:), 9100, c_upname, c_modname, stat )
       ELSE
          DO i = 1, SIZE(this%dim_id)
             val(i)  = this%dim_id(i)
          END DO
       END IF
       !
    END IF
    !
  END FUNCTION get_var_dim_id_0_1
  !
  !! hole die dynamische Feld-Komponente "dim_id" aus dem Datenobjekt in "t_var(:)"
  !! mit der ID "id"  <BR>
  !! "dim_id"=NULLIFY, falls es sich um eine skalare Variable handelt. <BR>
  !! "dim_id"=NULLIFY, falls gesuchtes Objekt nicht existiert. <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_var_dim_id_1_i_1 ( this, id ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN)  :: this(:) ! 
    !! Identifikationsnummer des gesuchten Objekts
    INTEGER      , INTENT(IN)  :: id      ! 
    !! R&uuml;ckgabewert "dim_id" (Vektor) : dim_id=NULLIFY, falls skalare Variable
    !! oder gesuchtes Objekt nicht existiert
    INTEGER , POINTER :: val(:) ! 
    !! Index des Objekts : idx=0, falls nicht gefunden
    INTEGER :: idx ! 
    !
    NULLIFY ( val )
    !
    idx = get_var_idx_i_0 ( this, id )
    IF ( idx > 0 ) THEN
       val => get_var_dim_id_0_1 ( this(idx) )
    END IF
    !
  END FUNCTION get_var_dim_id_1_i_1
  !
  !! hole die dynamische Feld-Komponente "dim_id" aus dem Datenobjekt in "t_var(:)"
  !! mit Namen "name"  <BR>
  !! "dim_id"=NULLIFY, falls es sich um eine skalare Variable handelt. <BR>
  !! "dim_id"=NULLIFY, falls gesuchtes Objekt nicht existiert. <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_var_dim_id_1_n_1 ( this, name ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_var)      , INTENT(IN) :: this(:) ! 
    !! Name des gesuchten Objekts
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! R&uuml;ckgabewert "dim_id" (Vektor) : dim_id=NULLIFY, falls skalare Variable
    !! oder gesuchtes Objekt nicht existiert
    INTEGER , POINTER :: val(:) ! 
    !
    !! Index des Objekts : idx=0, falls nicht gefunden
    INTEGER :: idx ! 
    !
    NULLIFY ( val )
    !
    idx = get_var_idx_n_0 ( this, name )
    IF ( idx > 0 ) THEN
       val => get_var_dim_id_0_1 ( this(idx) )
    END IF
    !
  END FUNCTION get_var_dim_id_1_n_1
  !
  !! hole ein bestimmtes Element der Feld-Komponente "dim_id" aus
  !! einem skalaren Datenobjekt <BR>
  !! RESULT=0, falls "dim_id" nicht assoziiert.<BR>
  !! RESULT=0, falls Feldelement "idim" nicht existiert. <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_spec_dim_id_0_0 ( this, idim ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_var) , INTENT(IN)  :: this     ! 
    !! Index der gesuchten Dimensions-ID
    INTEGER      , INTENT(IN)  :: idim     !
    !! R&uuml;ckgabewert : ein Element Feld-Komponente "dim_id" (Skalar)
    INTEGER  :: val ! 
    !
    val = get_specific_dim_id ( this%dim_id, idim )
    !
  END FUNCTION get_var_spec_dim_id_0_0
  !
  !! hole ein bestimmtes Element der Feld-Komponente "dim_id" aus
  !! dem Datenobjekt in "t_var(:)" mit der ID "id". <BR>
  !! RESULT=0, falls Datenobjekt mit der ID "id" nicht existiert. <BR>
  !! RESULT=0, falls "dim_id" des Datenobjekts nicht assoziiert.<BR>
  !! RESULT=0, falls Feldelement "idim" nicht existiert. <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_spec_dim_id_1_i_0 ( this, id, idim ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN)  :: this(:) ! 
    !! Identifikationsnummer des gesuchten Objekts
    INTEGER      , INTENT(IN)  :: id      ! 
    !! Index der gesuchten Dimensions-ID
    INTEGER      , INTENT(IN)  :: idim     !
    !! R&uuml;ckgabewert Dimensions-ID <BR>
    !! =0, falls :
    !!  - Datenobjekt mit ID="id" nicht existiert
    !!  - "dim_id" des Datenobjekts nicht assoziiert
    !!  - Feldelement "idim" in "dim_id" nicht existiert
    INTEGER  :: val ! 
    !
    !! Index des Objekts : idx=0, falls nicht gefunden
    INTEGER :: idx ! 
    !
    idx = get_var_idx_i_0  ( this, id )
    IF ( idx > 0 ) THEN
       val = get_specific_dim_id ( this(idx)%dim_id, idim )
    ELSE
       val = 0
    END IF
    !
  END FUNCTION get_var_spec_dim_id_1_i_0
  !
  !! hole ein bestimmtes Element der Feld-Komponente "dim_id" aus
  !! dem Datenobjekt in "t_var(:)" mit Namen "name".  <BR>
  !! RESULT=0, falls Datenobjekt mit Namen "name" nicht existiert. <BR>
  !! RESULT=0, falls "dim_id" des Datenobjekts nicht assoziiert.<BR>
  !! RESULT=0, falls Feldelement "idim" nicht existiert. <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_spec_dim_id_1_n_0 ( this, name, idim ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_var)      , INTENT(IN) :: this(:) ! 
    !! Name des gesuchten Objekts
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Index der gesuchten Dimensions-ID
    INTEGER           , INTENT(IN) :: idim    !
    !! R&uuml;ckgabewert Dimensions-ID <BR>
    !! =0, falls :
    !!  - Datenobjekt mit Namen="name" nicht existiert
    !!  - "dim_id" des Datenobjekts nicht assoziiert
    !!  - Feldelement "idim" in "dim_id" nicht existiert
    INTEGER :: val ! 
    !
    !! Index des Objekts : idx=0, falls nicht gefunden
    INTEGER :: idx ! 
    !
    idx = get_var_idx_n_0 ( this, name )
    IF ( idx > 0 ) THEN
       val = get_specific_dim_id ( this(idx)%dim_id, idim )
    ELSE
       val = 0
    END IF
    !
  END FUNCTION get_var_spec_dim_id_1_n_0
  !
  !! hole ein bestimmtes Element der Feld-Komponente "dim_id" aus
  !! einem skalaren Datenobjekt. Das Element soll gebietsabhaengig
  !! sein (s.a. c_tg_dim_name) <BR>
  !! RESULT=0, falls "dim_id" nicht assoziiert.<BR>
  !! RESULT=0, falls gebietsabhaengige Dimension nicht existiert. <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_tg_dim_id_0_1 ( this, dim ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_var) , INTENT(IN) :: this
    !! Liste aller Dimensionen
    TYPE (t_dim) , INTENT(IN) :: dim(:)
    !! R&uuml;ckgabewert : ein Element Feld-Komponente "dim_id" (Skalar)
    INTEGER  :: val
    !
    !! Anzahl der gebietsabh. Dimensionen
    INTEGER          , PARAMETER :: max_tg_dim_name = 6
    !! Namen der gebietsabh. Dimensionen
    CHARACTER (LEN=7), PARAMETER :: c_tg_dim_name ( max_tg_dim_name ) = &
         (/ 'node   ', 'edge   ', 'polygon', 'vedge  ', 'face   ', 'cell   ' /)
    !! Zaehler und Indizes
    INTEGER :: idim, idim_memo, dim_idx
    !
    val       = 0
    !
    idim_memo = 0
    !
    IF ( .NOT. is_scalar_var_0 (this) ) THEN
       !
       DO idim = 1, SIZE ( this%dim_id )
          !
          dim_idx = get_dim_idx ( dim, this%dim_id(idim) )
          !
          IF ( COUNT ( c_tg_dim_name == get_dim_name ( dim ( dim_idx ) ) ) == 1 ) idim_memo = idim
          !
       END DO
       !
       IF ( idim_memo > 0 ) val = this%dim_id(idim_memo)
       !
    END IF
    !
  END FUNCTION get_var_tg_dim_id_0_1
  !
  !! Ermittle die Position einer gesuchten Variablen in einem Feld
  !! f&uuml;r eine Identifikationsnummer "id" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_idx_i_0 ( this, id ) &
       RESULT( idx )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:) ! 
    !! Identifikationsnummer des gesuchten Objekts
    INTEGER      , INTENT(IN) :: id      ! 
    !! R&uuml;ckgabewert : Feldindex (idx = 0 falls nicht gefunden)
    INTEGER :: idx ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    IF ( ANY( this(:)%id == id ) ) THEN
       idx = MINVAL( MINLOC( this(:)%id, this(:)%id == id ) )
    ELSE
       idx = 0
    END IF
    ! 
  END FUNCTION get_var_idx_i_0
  !
  !! Ermittle die Positionen gesuchter Variablen in einem Feld
  !! f&uuml;r mehrere vorgegebene Identifikationsnummern "id" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_idx_i_1 ( this, id ) &
       RESULT( idx )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:) ! 
    !! Identifikationsnummern der gesuchten Objekte
    INTEGER      , INTENT(IN) :: id(:)   ! 
    !! R&uuml;ckgabewert : Feldindizes (idx = 0 falls nicht gefunden)
    INTEGER :: idx(SIZE(id)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(id)
       idx(i) = get_var_idx_i_0 ( this(:), id(i) )
    END DO
    ! 
  END FUNCTION get_var_idx_i_1
  !
  !! Ermittle die Position einer gesuchten Variablen in einem Feld
  !! f&uuml;r einen vorgegebenen Namen "name" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_idx_n_0 ( this, name ) &
       RESULT( idx )
    !! Datenobjekt (Vektor)
    TYPE (t_var)      , INTENT(IN) :: this(:) ! 
    !! Name des gesuchten Objekts
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! R&uuml;ckgabewert : Feldindex (idx = 0 falls nicht gefunden)
    INTEGER :: idx ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    idx = 0
    DO i=1,SIZE(this)
       IF ( idx /= 0 ) EXIT
       IF ( LEN_TRIM(this(i)%name) == LEN_TRIM(name) ) THEN
          IF ( TRIM(get_lowercase_char(this(i)%name)) == TRIM(get_lowercase_char(name)) ) idx = i
       END IF
    END DO
    ! 
  END FUNCTION get_var_idx_n_0
  !
  !! Ermittle die Positionen gesuchter Variablen in einem Feld
  !! f&uuml;r mehrere vorgegebene Namen "name". <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_idx_n_1 ( this, name ) &
       RESULT( idx )
    !! Datenobjekt (Vektor)
    TYPE (t_var)      , INTENT(IN) :: this(:) ! 
    !! Namen der gesuchten Objekte
    CHARACTER (LEN=*) , INTENT(IN) :: name(:)   ! 
    !! R&uuml;ckgabewert : Feldindices (idx = 0 falls nicht gefunden)
    INTEGER :: idx(SIZE(name)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i = 1, SIZE(name)
       idx(i) = get_var_idx_n_0 ( this, name(i) )
    END DO
    ! 
  END FUNCTION get_var_idx_n_1
  !
  !! Ermittle die Position einer gesuchten Variablen in einem Feld
  !! f&uuml;r eine vorgegebene Variablenangabe vom Typ "t_var" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_idx_d_0 ( this, var ) &
       RESULT( idx )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:) ! 
    !! das gesuchte Objekt
    TYPE (t_var) , INTENT(IN) :: var     ! 
    !! R&uuml;ckgabewert : Feldindex (idx = 0 falls nicht gefunden)
    INTEGER :: idx ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    idx = 0
    DO i=1,SIZE(this)
       IF ( idx /= 0       ) EXIT
       IF ( this(i) == var ) idx = i
    END DO
    ! 
  END FUNCTION get_var_idx_d_0
  !
  !! Ermittle die Positionen gesuchter Variablen in einem Feld
  !! f&uuml;r mehrere vorgegebene Variablen vom Typ "t_var" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_idx_d_1 ( this, var ) &
       RESULT( idx )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:) ! 
    !! die gesuchten Objekte
    TYPE (t_var) , INTENT(IN) :: var(:)  ! 
    !! R&uuml;ckgabewert : Feldindices (idx = 0 falls nicht gefunden)
    INTEGER :: idx(SIZE(var)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       idx(i) = get_var_idx_d_0 ( this(:), var(i) )
    END DO
    ! 
  END FUNCTION get_var_idx_d_1
  !
  !! Ermittle den "Rank" (Dimensionalit&auml;t) der Variablen (Skalar). <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_rank_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_var) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "Rank" (Skalar)
    INTEGER :: val  ! 
    !
    IF ( is_scalar_var_0 (this) ) THEN
       val = 0
    ELSE
       val = SIZE(this%dim_id)
    END IF
    !
  END FUNCTION get_var_rank_0
  !
  !! ermittle den "Rank" (Dimensionalit&auml;t) der Variablen (Vektor)<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_rank_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "Rank" (Vektor)
    INTEGER :: val(SIZE(this))  ! 
    !! Z&auml;hler      
    INTEGER :: i ! 
    !
    DO i = 1, SIZE(this)
       val(i) = get_var_rank_0 (this(i))
    END DO
    !
  END FUNCTION get_var_rank_1
  !
  !! Ermittle den "Rank" (Dimensionalit&auml;t) der Variablen
  !! mit der Identifikationsnummer "id". <BR>
  !! RESULT=-1, falls gesuchtes Objekt nicht existiert.  <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_rank_i_0 ( this, id ) &
       RESULT( val )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:) ! 
    !! Identifikationsnummer des gesuchten Objekts
    INTEGER      , INTENT(IN) :: id      ! 
    !! R&uuml;ckgabewert "Rank" : =-1, wenn Objekt nicht existent
    INTEGER :: val ! 
    !
    !! Index des Objekts : idx=0, falls nicht gefunden
    INTEGER :: idx ! 
    !
    idx = get_var_idx_i_0  ( this, id )
    IF ( idx > 0 ) THEN
       val = get_var_rank_0 ( this(idx) )
    ELSE
       val = -1
    END IF
    ! 
  END FUNCTION get_var_rank_i_0
  !
  !! Ermittle den "Rank" der Datenobjekte mit den vorgegebenen
  !! Identifikationsnummern "id(:)". <BR>
  !! RESULT=-1, falls gesuchtes Objekt nicht existiert.  <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_rank_i_1 ( this, id ) &
       RESULT( val )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:) ! 
    !! Identifikationsnummern der gesuchten Objekte
    INTEGER      , INTENT(IN) :: id(:)   ! 
    !! R&uuml;ckgabewert "Rank" : =-1, wenn Objekt nicht existent
    INTEGER :: val(SIZE(id)) ! 
    !
    !! Index eines gesuchten Objektes : idx=0, falls nicht gefunden
    INTEGER  :: idx ! 
    !! Z&auml;hler      
    INTEGER  :: i ! 
    !
    DO i = 1, SIZE(id)
       idx = get_var_idx_i_0 ( this, id(i) )
       IF ( idx > 0 ) THEN
          val(i) = get_var_rank_0 ( this(idx) )
       ELSE
          val(i) = -1
       END IF
    END DO
    !
  END FUNCTION get_var_rank_i_1
  !
  !! Ermittle den "Rank" (Dimensionalit&auml;t) der Variablen
  !! mit dem Namen "name". <BR>
  !! RESULT=-1, falls gesuchtes Objekt nicht existiert.  <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_rank_n_0 ( this, name ) &
       RESULT( val )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:) ! 
    !! Name des gesuchten Objekts
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! R&uuml;ckgabewert "Rank" : =-1, wenn Objekt nicht existent
    INTEGER :: val ! 
    !
    !! Index des Objekts : idx=0, falls nicht gefunden
    INTEGER :: idx ! 
    !
    idx = get_var_idx_n_0  ( this, name )
    IF ( idx > 0 ) THEN
       val = get_var_rank_0 ( this(idx) )
    ELSE
       val = -1
    END IF
    ! 
  END FUNCTION get_var_rank_n_0
  !
  !! Ermittle den "Rank" der Datenobjekte mit den vorgegebenen
  !! Namen "name(:)". <BR>
  !! RESULT=-1, falls gesuchtes Objekt nicht existiert.  <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_rank_n_1 ( this, name ) &
       RESULT( val )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:) ! 
    !! Namen der gesuchten Objekte
    CHARACTER (LEN=*) , INTENT(IN)  :: name(:)   ! 
    !! R&uuml;ckgabewert "Rank" : =-1, wenn Objekt nicht existent
    INTEGER :: val(SIZE(name)) ! 
    !
    !! Index eines gesuchten Objektes : idx=0, falls nicht gefunden
    INTEGER  :: idx ! 
    !! Z&auml;hler      
    INTEGER  :: i ! 
    !
    DO i = 1, SIZE(name)
       idx = get_var_idx_n_0 ( this, name(i) )
       IF ( idx > 0 ) THEN
          val(i) = get_var_rank_0 ( this(idx) )
       ELSE
          val(i) = -1
       END IF
    END DO
    !
  END FUNCTION get_var_rank_n_1
  !
  !! Ermittle die "Shape" der Variablen (Skalar). <BR>
  !! RESULT=NULLIFY, falls es sich um eine skalare Variable handelt.  <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_var_shape_0 ( this, dim ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_var) , INTENT(IN)  :: this ! 
    !! Feld mit Dimensionsangaben
    TYPE (t_dim) , INTENT(IN)  :: dim(:) ! 
    !! R&uuml;ckgabewert "Shape"-Vektor (Pointer) : =NULLIFY, bei skalarer Variablen
    INTEGER , POINTER :: val(:) !
    !
    !! Name der Subroutine
    CHARACTER (LEN=15) , PARAMETER :: c_upname='get_var_shape_0' ! 
    !! Index einer gesuchten Dimensionen
    INTEGER :: dim_idx
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !! Z&auml;hler      
    INTEGER :: i ! 
    !
    NULLIFY ( val )
    !
    IF ( .NOT. is_scalar_var_0 (this) ) THEN
       !
       IF ( ALL(dim_exists(dim, this%dim_id)) ) THEN
          !
          ALLOCATE ( val(SIZE(this%dim_id)), STAT=stat )
          !
          IF ( stat /= 0 ) THEN
             ! Fehler beim Allokieren
             CALL setup_error_act ( all_errors(:), 9500, c_upname, c_modname, stat )
             WRITE(ctxt,'(I10)') SIZE(this%dim_id)
             CALL setup_error_act ( '<AktDim>', ctxt )
          ELSE
             !
             DO i = 1, SIZE(this%dim_id)
                dim_idx = get_dim_idx ( dim, this%dim_id(i) )
                val(i)  = get_dim_len ( dim(dim_idx) )
             END DO
             !
          END IF
       ELSE
          ! Fehler : eine oder mehrere Dimensionen konnten nicht gefunden werden
          CALL setup_error_act ( all_errors(:), 9510, c_upname, c_modname, stat )
          !
       END IF
    END IF
    !
  END FUNCTION get_var_shape_0
  !
  !! Ermittle die "Shape" der Variablen mit der
  !! Identifikationsnummer "id". <BR>
  !! RESULT=NULLIFY, falls es sich um eine skalare Variable handelt.  <BR>
  !! RESULT=NULLIFY, falls gesuchtes Objekt nicht existiert.  <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_var_shape_i_0 ( this, id, dim ) &
       RESULT( val )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:) ! 
    !! Identifikationsnummer des gesuchten Objekts
    INTEGER      , INTENT(IN) :: id      ! 
    !! Feld mit Dimensionsangaben
    TYPE (t_dim) , INTENT(IN)  :: dim(:) ! 
    !! R&uuml;ckgabewert "Shape"-Vektor (Pointer) :<BR>
    !!   =NULLIFY, bei skalarer Variablen<BR>
    !!   =NULLIFY, falls t_var-Objekt nicht vorhanden
    INTEGER  , POINTER :: val(:) ! 
    !
    !! Index des Objekts : idx=0, falls nicht gefunden
    INTEGER :: idx ! 
    !
    NULLIFY ( val )
    !
    idx = get_var_idx_i_0  ( this, id )
    IF ( idx > 0 ) THEN
       val => get_var_shape_0 ( this(idx), dim )
    END IF
    ! 
  END FUNCTION get_var_shape_i_0
  !
  !! Ermittle die "Shape" der Variablen mit dem 
  !! Namen "name". <BR>
  !! RESULT=NULLIFY, falls es sich um eine skalare Variable handelt.  <BR>
  !! RESULT=NULLIFY, falls gesuchtes Objekt nicht existiert.  <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_var_shape_n_0 ( this, name, dim ) &
       RESULT( val )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:) ! 
    !! Name des gesuchten Objekts
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Feld mit Dimensionsangaben
    TYPE (t_dim) , INTENT(IN)  :: dim(:) ! 
    !! R&uuml;ckgabewert "Shape"-Vektor (Pointer) :<BR>
    !!   =NULLIFY, bei skalarer Variablen<BR>
    !!   =NULLIFY, falls t_var-Objekt nicht vorhanden
    INTEGER  , POINTER :: val(:) ! 
    !
    !! Index des Objekts : idx=0, falls nicht gefunden
    INTEGER :: idx ! 
    !
    NULLIFY ( val )
    !
    idx = get_var_idx_n_0  ( this, name )
    IF ( idx > 0 ) THEN
       val => get_var_shape_0 ( this(idx), dim )
    END IF
    ! 
  END FUNCTION get_var_shape_n_0
  !
  !! Ermittle die "Size" einer Variablen (Skalar). <BR>
  !! RESULT=0, falls es sich um eine skalare Variable handelt.  <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_var_size_0 ( this, dim )      &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_var) , INTENT(IN)  :: this   ! 
    !! Feld mit Dimensionsangaben
    TYPE (t_dim) , INTENT(IN)  :: dim(:) ! 
    !! R&uuml;ckgabewert "Size" der Variablen
    INTEGER :: val ! 
    !
    !! Name der Subroutine
    CHARACTER (LEN=14) , PARAMETER :: c_upname='get_var_size_0' ! 
    !! Hilfsfeld f&uuml;r die aktuelle "Shape" der Variablen
    INTEGER , POINTER :: sh(:) ! 
    !
    sh => get_var_shape_0( this, dim(:) )
    !
    IF ( ASSOCIATED( sh ) ) THEN
       val = ABS(PRODUCT( sh )) ; DEALLOCATE( sh ) ; NULLIFY( sh )
    ELSE
       val = 0
    END IF
    !
  END FUNCTION get_var_size_0
  !
  !! Ermittle die "Sizee" der Variablen mit der
  !! Identifikationsnummer "id". <BR>
  !! RESULT=0, falls es sich um eine skalare Variable handelt.  <BR>
  !! RESULT=-1, falls gesuchtes Objekt nicht existiert.  <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_var_size_i_0 ( this, id, dim ) &
       RESULT( val )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:) ! 
    !! Identifikationsnummer des gesuchten Objekts
    INTEGER      , INTENT(IN) :: id      ! 
    !! Feld mit Dimensionsangaben
    TYPE (t_dim) , INTENT(IN)  :: dim(:) ! 
    !! R&uuml;ckgabewert "Size" der Variablen:<BR>
    !!   =  0, bei skalarer Variablen<BR>
    !!   = -1, falls t_var-Objekt nicht vorhanden
    INTEGER :: val ! 
    !
    !! Index des Objekts : idx=0, falls nicht gefunden
    INTEGER :: idx ! 
    !
    idx = get_var_idx_i_0  ( this, id )
    IF ( idx > 0 ) THEN
       val = get_var_size_0 ( this(idx), dim )
    ELSE
       val = -1
    END IF
    ! 
  END FUNCTION get_var_size_i_0
  !
  !! Ermittle die "Size" der Variablen mit dem  Namen "name". <BR>
  !! RESULT=0, falls es sich um eine skalare Variable handelt.  <BR>
  !! RESULT=-1, falls gesuchtes Objekt nicht existiert.  <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_var_size_n_0 ( this, name, dim ) &
       RESULT( val )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:) ! 
    !! Name des gesuchten Objekts
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Feld mit Dimensionsangaben
    TYPE (t_dim) , INTENT(IN)  :: dim(:) ! 
    !! R&uuml;ckgabewert "Size" der Variablen :<BR>
    !!   =  0, bei skalarer Variablen<BR>
    !!   = -1, falls t_var-Objekt nicht vorhanden
    INTEGER :: val ! 
    !
    !! Index des Objekts : idx=0, falls nicht gefunden
    INTEGER :: idx ! 
    !
    idx = get_var_idx_n_0  ( this, name )
    IF ( idx > 0 ) THEN
       val = get_var_size_0 ( this(idx), dim )
    ELSE
       val = -1
    END IF
    ! 
  END FUNCTION get_var_size_n_0
  !
  !! Ermittle den Startindex "start(:)" einer Variablen <BR>
  !! das Feld "start(:)" wird in geeigneter Gr&ouml;&szlig;e allokiert
  !! (entsprechend der Anzahl der Dimensionen der Variablen "this") und 
  !! mit den Default-Werten "1" belegt <BR>
  !! handelt es sich bei der Variablen um eine skalare Gr&ouml;&szlig;e
  !! so wird "start(1)=1" zur&uuml;ckgegeben <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_var_start_0 ( this )            &
       RESULT(start)
    !! Objekt "Variable"
    TYPE (t_var) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Startindices "start(:)"
    INTEGER , POINTER :: start(:) ! 
    !! Name der Function
    CHARACTER (LEN=15) , PARAMETER :: c_upname='get_var_start_0' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    ! 
    ALLOCATE( start(MAX(1,get_var_rank(this))), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 9600, c_upname, c_modname, stat )
       CALL setup_error_act ( '<AktVarName>', TRIM(get_var_name(this)) )
       NULLIFY(start)
    ELSE
       start(:) = 1
    END IF
    !
  END FUNCTION get_var_start_0
  !
  !! Ermittle den Startindex "start(:)" einer Variablen und 
  !! &uuml;berschreibe die letzte Position in "start(:)" mit einem
  !! durch den Benutzer vorgegebenen Wert, wie dies z.B. f&uuml;r
  !! Record-Variablen sinnvoll sein kann; ist die letzte Dimension
  !! nicht die Record-Dimension so wird der Index durch die 
  !! maximale Gr&ouml;&szlig;e geeignet limitiert <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_var_start_0_i ( this, idx, dim )    &
       RESULT(start)
    !! Objekt "Variable"
    TYPE (t_var) , INTENT(IN) :: this     ! 
    !! benutzervorgegebener Index
    INTEGER      , INTENT(IN) :: idx      ! 
    !! Feld mit allen verf&uuml;gbaren Dimensionen
    TYPE (t_dim) , INTENT(IN) :: dim(:)   ! 
    !! R&uuml;ckgabewert : Startindices "start(:)"
    INTEGER      , POINTER    :: start(:) ! 
    !! Hilfsfeld f&uuml;r die Form des Feldes
    INTEGER      , POINTER    :: shape(:) ! 
    ! 
    start => get_var_start_0 ( this )
    IF ( ASSOCIATED( start ) ) THEN
       IF ( is_unlimited_var(this,dim(:)) ) THEN
          start(SIZE(start)) = MAX(1,idx)
       ELSE
          shape => get_var_shape(this,dim(:))
          start(SIZE(start)) = MIN(start(SIZE(start)),shape(SIZE(shape)))
          DEALLOCATE (shape)
       END IF
    END IF
    !
  END FUNCTION get_var_start_0_i
  !
  !! Ermittle die Indices "step(:)" einer Variablen <BR>
  !! die Werte werden so gesetzt, dass sie der Form (Shape) der
  !! Variablen entsprechen; falls die letzte Dimension eine
  !! sogenannte "unlimited" oder Record-Dimension ist, erh&auml;lt diese
  !! den Wert "1" ansonsten die aktuelle Gr&ouml;&szlig;e dieser Dimension <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_var_step_0 ( this, dim )      &
       RESULT(step)
    !! Objekt "Variable"
    TYPE (t_var) , INTENT(IN) :: this   ! 
    !! alle verf&uuml;gbaren Dimensionen 
    TYPE (t_dim) , INTENT(IN) :: dim(:) ! 
    !! R&uuml;ckgabewert : Anzahl der Datenwerte "step(:)"
    INTEGER , POINTER :: step(:) ! 
    !! Name der Function
    CHARACTER (LEN=15) , PARAMETER :: c_upname='step_var_step_0' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !! Shape der Variablen
    INTEGER , POINTER :: l_shape(:) ! 
    !
    ALLOCATE( step(MAX(1,get_var_rank(this))), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 9700, c_upname, c_modname, stat )
       CALL setup_error_act ( '<AktVarName>', TRIM(get_var_name(this)) )
       NULLIFY(step)
    ELSE
       step(:) = 1
       l_shape => get_var_shape(this,dim(:))
       IF ( ASSOCIATED(l_shape) ) THEN
          WHERE ( l_shape(:) > 0 ) step(:) = l_shape(:) ! Rec.Dim bleibt auf "1"
          DEALLOCATE(l_shape)
       END IF
    END IF
    !
  END FUNCTION get_var_step_0
  !
  !! Ermittle die Schrittweite "stride(:)" einer Variablen;
  !! f&uuml;r alle Dimensionen wird "stride(:)=1" gesetzt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_var_stride_0 ( this )             &
       RESULT(stride)
    !! Objekt "Variable"
    TYPE (t_var) , INTENT(IN) :: this   ! 
    !! R&uuml;ckgabewert : Schrittweite "stride(:)"
    INTEGER , POINTER :: stride(:) ! 
    !! Name der Function
    CHARACTER (LEN=17) , PARAMETER :: c_upname='step_var_stride_0' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE( stride(MAX(1,get_var_rank(this))), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 9800, c_upname, c_modname, stat )
       CALL setup_error_act ( '<AktVarName>', TRIM(get_var_name(this)) )
       NULLIFY(stride)
    ELSE
       stride(:) = 1
    END IF
    !
  END FUNCTION get_var_stride_0
  !
  !! Ermittle die Indexposition einer bestimmten f&uuml;r eine bestimmte Variable
  !! g&uuml;ltigen Dimension in einem Feld von Dimensionen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_dim_idx_0_0_1 ( this, dim_name, dim ) &
       RESULT(res)
    !! aktuelles Datenobjekt (Skalar)
    TYPE (t_var)      , INTENT(IN) :: this     ! 
    !! Name der Dimension (muss in this verwendet werden)
    CHARACTER (LEN=*) , INTENT(IN) :: dim_name ! 
    !! Liste aller Dimensionen
    TYPE (t_dim)      , INTENT(IN) :: dim(:)   ! 
    !! Ergebnis : Indexposition der gesuchten Dimension in dim(:) <BR>
    !!            -1 falls nicht vorhanden
    INTEGER               :: res      ! 
    !! Hilfsvariable
    INTEGER , ALLOCATABLE :: l_idx(:) ! 
    INTEGER :: idx ! 
    !
    res = -1
    IF ( ASSOCIATED(this%dim_id) ) THEN
       ALLOCATE(l_idx(SIZE(this%dim_id)))
       l_idx = get_dim_idx( dim, this%dim_id )
       IF ( ALL(l_idx>0) ) THEN
          idx = get_dim_idx( dim(l_idx), dim_name )
          IF ( idx > 0 ) res = l_idx(idx)
       END IF
       DEALLOCATE(l_idx)
    END IF
    !
  END FUNCTION get_var_dim_idx_0_0_1
  !
  !! Ermittle die Indexposition mehrerer bestimmter f&uuml;r eine bestimmte Variable
  !! g&uuml;ltigen Dimensionen in einem Feld von Dimensionen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_dim_idx_0_1_1 ( this, dim_name, dim ) &
       RESULT(res)
    !! aktuelles Datenobjekt (Skalar)
    TYPE (t_var)      , INTENT(IN) :: this        ! 
    !! Namen der Dimension (muss in this verwendet werden)
    CHARACTER (LEN=*) , INTENT(IN) :: dim_name(:) ! 
    !! Liste aller Dimensionen
    TYPE (t_dim)      , INTENT(IN) :: dim(:)      ! 
    !! Ergebnis : Indexpositionen der gesuchten Dimensionen in dim(:) <BR>
    !!            -1 falls nicht vorhanden
    INTEGER               :: res(SIZE(dim_name))  ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_var_dim_idx_0_0_1( this, dim_name(i), dim )
    END DO
    !
  END FUNCTION get_var_dim_idx_0_1_1
  !
  !! Konsistentes Ersetzen einer Dimensions-Id durch einen neuen
  !! Wert in einem Feld von Variablen und Dimensionen <BR>
  !! Ersetzung findet nur statt, falls sich alte und neue Id voneinander
  !! unterscheiden <BR>
  !! ist die neue Dimensions Id schon in dem Feld der ID's vorhanden, so
  !! wird diese mit einem neuen, bislang nicht vorhandenen Wert belegt <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE replace_var_dim_id_1_1 ( o_dim_id, n_dim_id, dim, var )
    !! alte Dimension-Id, die ersetzt werden soll
    INTEGER , INTENT(IN) :: o_dim_id ! 
    !! neue Dimension-Id
    INTEGER , INTENT(IN) :: n_dim_id ! 
    !! Liste der Dimensionen
    TYPE (t_dim) , INTENT(INOUT) :: dim(:) ! 
    !! Liste der Variablen
    TYPE (t_var) , INTENT(INOUT) :: var(:) ! 
    !! lokale Hilfsfelder f&uuml;r Dimensions-Id's
    INTEGER :: l_n_dim_id(SIZE(dim)) ! 
    !! maximale Dimensions id
    INTEGER :: max_dim_id ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    IF ( o_dim_id /= n_dim_id ) THEN
       l_n_dim_id(:) = get_dim_id( dim(:) )
       WHERE ( l_n_dim_id(:) ==  n_dim_id ) l_n_dim_id(:) = -n_dim_id
       WHERE ( l_n_dim_id(:) ==  o_dim_id ) l_n_dim_id(:) =  n_dim_id
       max_dim_id    = MAXVAL(l_n_dim_id(:))+1
       WHERE ( l_n_dim_id(:) == -n_dim_id ) l_n_dim_id(:) = max_dim_id
       CALL set_dim_id ( dim(:), l_n_dim_id(:) )
       !
       DO i=1,SIZE(var)
          IF ( ASSOCIATED( var(i)%dim_id ) ) THEN
             WHERE ( var(i)%dim_id(:) ==  n_dim_id ) var(i)%dim_id(:) =  -n_dim_id
             WHERE ( var(i)%dim_id(:) ==  o_dim_id ) var(i)%dim_id(:) =   n_dim_id
             WHERE ( var(i)%dim_id(:) == -n_dim_id ) var(i)%dim_id(:) = max_dim_id
          END IF
       END DO
    END IF
    !
  END SUBROUTINE replace_var_dim_id_1_1
  !
  !! Ermittle ob in einem Feld eine Variable mit vorgegebener
  !! Identifikationsnummer "id" existiert <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION var_exists_i_0 ( this, id ) &
       RESULT( ex )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:) ! 
    !! Identifikationsnummer des gesuchten Objekts
    INTEGER      , INTENT(IN) :: id      ! 
    !! R&uuml;ckgabewert : .true./.false.
    LOGICAL :: ex ! 
    !
    ex = ( get_var_idx_i_0 ( this(:), id ) > 0 )
    ! 
  END FUNCTION var_exists_i_0
  !
  !! Ermittle ob in einem Feld Variablen mit vorgegebenen
  !! Identifikationsnummern "id" vorhanden sind <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION var_exists_i_1 ( this, id ) &
       RESULT( ex )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:) ! 
    !! Identifikationsnummern der gesuchten Objekte
    INTEGER      , INTENT(IN) :: id(:)   ! 
    !! R&uuml;ckgabewert : .true./.false. f&uuml;r alle Id's
    LOGICAL :: ex(SIZE(id)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i = 1, SIZE(ex)
       ex(i) = var_exists_i_0 ( this(:), id(i) )
    END DO
    ! 
  END FUNCTION var_exists_i_1
  !
  !! Ermittle ob in einem Feld eine Variable mit vorgegebenem
  !! Namen "name" existiert <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION var_exists_n_0 ( this, name ) &
       RESULT( ex )
    !! Datenobjekt (Vektor)
    TYPE (t_var)      , INTENT(IN) :: this(:) ! 
    !! Name des gesuchten Objekts
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! R&uuml;ckgabewert : .true./.false.
    LOGICAL :: ex ! 
    !
    ex = ( get_var_idx_n_0 ( this(:), name ) > 0 )
    ! 
  END FUNCTION var_exists_n_0
  !
  !! Ermittle ob in einem Feld Variablen mit vorgegebenen
  !! Namen "name" vorhanden sind <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION var_exists_n_1 ( this, name ) &
       RESULT( ex )
    !! Datenobjekt (Vektor)
    TYPE (t_var)      , INTENT(IN) :: this(:) ! 
    !! Namen der gesuchten Objekte
    CHARACTER (LEN=*) , INTENT(IN) :: name(:) ! 
    !! R&uuml;ckgabewert : .true./.false. f&uuml;r alle Namen
    LOGICAL :: ex(SIZE(name)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i = 1, SIZE(ex)
       ex(i) = var_exists_n_0 ( this(:), name(i) )
    END DO
    ! 
  END FUNCTION var_exists_n_1
  !
  !! Ermittle ob in einem Feld eine bestimmte
  !! Variable "var" existiert <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION var_exists_d_0 ( this, var ) &
       RESULT( ex )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:) ! 
    !! das gesuchten Objekts
    TYPE (t_var) , INTENT(IN) :: var     ! 
    !! R&uuml;ckgabewert : .true./.false.
    LOGICAL :: ex ! 
    !
    ex = ( get_var_idx_d_0 ( this(:), var ) > 0 )
    ! 
  END FUNCTION var_exists_d_0
  !
  !! Ermittle ob in einem Feld bestimmte
  !! Variablen "var(:)" vorhanden sind <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION var_exists_d_1 ( this, var ) &
       RESULT( ex )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:) ! 
    !! die gesuchten Objekte
    TYPE (t_var) , INTENT(IN) :: var(:)  ! 
    !! R&uuml;ckgabewert : .true./.false. f&uuml;r alle Id's
    LOGICAL :: ex(SIZE(var)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i = 1, SIZE(ex)
       ex(i) = var_exists_d_0 ( this(:), var(i) )
    END DO
    ! 
  END FUNCTION var_exists_d_1
  !
  !! Ermittle, ob es sich bei dem Datanobjekt um eine skalare
  !! Variable handelt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_scalar_var_0 ( this )          &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_var) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = ( .NOT. ASSOCIATED( this%dim_id ) )
    !
  END FUNCTION is_scalar_var_0
  !
  !! Ermittle, ob es sich bei den Datanobjekten um skalare 
  !! Variablen handelt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_scalar_var_1 ( this )          &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:) ! 
    !! R&uuml;ckgabewert : Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Z&auml;ervariable
    INTEGER :: i ! 
    !
    DO i = 1, SIZE(ok)
       ok(i) = is_scalar_var_0 ( this(i) )
    END DO
    !
  END FUNCTION is_scalar_var_1
  !
  !! Ermittle, ob es sich bei dem Datenobjekt mit vorgegebener
  !! Identifikationsnummer "id" um eine skalare Variable handelt <BR>
  !! "ok"=FALSE, falls gesuchtes Objekt nicht existiert  <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_scalar_var_i_0 ( this, id ) &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:) ! 
    !! Identifikationsnummer des gesuchten Objekts
    INTEGER      , INTENT(IN) :: id      ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    !! Index des Objekts : idx=0, falls nicht gefunden
    INTEGER :: idx ! 
    !
    idx = get_var_idx_i_0  ( this, id )
    IF ( idx > 0 ) THEN
       ok = is_scalar_var_0 ( this(idx) )
    ELSE
       ok = .false.
    END IF
    ! 
  END FUNCTION is_scalar_var_i_0
  !
  !! Ermittle, ob es sich bei den Datenobjekten mit vorgegebenen
  !! Identifikationsnummern "id(:)" um skalare Variablen handelt <BR>
  !! "ok"=FALSE, falls gesuchtes Objekt nicht existiert  <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_scalar_var_i_1 ( this, id ) &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:) ! 
    !! Identifikationsnummern der gesuchten Objekte
    INTEGER      , INTENT(IN) :: id(:)   ! 
    !! R&uuml;ckgabewert : .true./.false. f&uuml;r alle Id's
    LOGICAL :: ok(SIZE(id)) ! 
    !
    !! Indizes der gesuchten Objekte : idx=0, falls nicht gefunden
    INTEGER :: idx(SIZE(id)) ! 
    !! Z&auml;hler      
    INTEGER           :: i ! 
    !
    idx    = get_var_idx_i_1  ( this, id )
    DO i = 1, SIZE(idx)
       IF ( idx(i) > 0 ) THEN
          ok = is_scalar_var_0 ( this(idx(i)) )
       ELSE
          ok = .false.
       END IF
    END DO
    !
  END FUNCTION is_scalar_var_i_1
  !
  !! Ermittle, ob es sich bei dem Datenobjekt mit vorgegebener
  !! Namen "name" um eine skalare Variable handelt <BR>
  !! "ok"=FALSE, falls gesuchtes Objekt nicht existiert  <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_scalar_var_n_0 ( this, name ) &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:) ! 
    !! Name des gesuchten Objekts
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    !! Index des Objekts : idx=0, falls nicht gefunden
    INTEGER :: idx ! 
    !
    idx = get_var_idx_n_0  ( this, name )
    IF ( idx > 0 ) THEN
       ok = is_scalar_var_0 ( this(idx) )
    ELSE
       ok = .false.
    END IF
    ! 
  END FUNCTION is_scalar_var_n_0
  !
  !! Ermittle, ob es sich bei den Datenobjekten mit vorgegebenen
  !! Namen "name(:)" um skalare Variablen handelt <BR>
  !! "ok"=FALSE, falls gesuchtes Objekt nicht existiert  <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_scalar_var_n_1 ( this, name ) &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:) ! 
    !! Namen der gesuchten Objekte
    CHARACTER (LEN=*) , INTENT(IN)  :: name(:)   ! 
    !! R&uuml;ckgabewert : .true./.false. f&uuml;r alle Id's
    LOGICAL :: ok(SIZE(name)) ! 
    !
    !! Indizes der gesuchten Objekte : idx=0, falls nicht gefunden
    INTEGER :: idx(SIZE(name)) ! 
    !! Z&auml;hler      
    INTEGER           :: i ! 
    !
    idx    = get_var_idx_n_1  ( this, name )
    DO i = 1, SIZE(idx)
       IF ( idx(i) > 0 ) THEN
          ok = is_scalar_var_0 ( this(idx(i)) )
       ELSE
          ok = .false.
       END IF
    END DO
    !
  END FUNCTION is_scalar_var_n_1
  !
  !! Pr&uuml;fe, ob eine Variable eine "unlimited" Dimension aufweist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_unlimited_var_0 ( this, dim ) &
       RESULT(ok)
    !! Datenobjekt (Skalar)
    TYPE (t_var) , INTENT(IN) :: this   ! 
    !! Liste aller verf&uuml;gbaren Dimensionen
    TYPE (t_dim) , INTENT(IN) :: dim(:) ! 
    !! R&uuml;ckgabewert : .true. / .false.
    LOGICAL :: ok ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    ok = .false.
    IF ( .NOT. is_scalar_var(this) ) THEN
       DO i=1,get_var_rank(this)
          IF ( ok ) EXIT
          ok = is_unlimited_dim(dim(:),this%dim_id(i))
       END DO
    END IF
    !
  END FUNCTION is_unlimited_var_0
  !
  !! Pr&uuml;fe, ob mehrere Variablen eine "unlimited" Dimension aufweisen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_unlimited_var_1 ( this, dim ) &
       RESULT(ok)
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:) ! 
    !! Liste aller verf&uuml;gbaren Dimensionen
    TYPE (t_dim) , INTENT(IN) :: dim(:)  ! 
    !! R&uuml;ckgabewert : .true. / .false.
    LOGICAL :: ok(SIZE(this)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       ok(i) = is_unlimited_var(this(i),dim(:))
    END DO
    !
  END FUNCTION is_unlimited_var_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_var_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_var) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_var) , INTENT(IN) :: this2 ! 
    !! R&uuml;ckgabewert : Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1)  = eq_var_id     ( this1, this2 )
    l_ok(2)  = eq_var_name   ( this1, this2 )
    l_ok(3)  = eq_var_type   ( this1, this2 )
    l_ok(4)  = eq_var_con_id ( this1, this2 )
    l_ok(5)  = eq_var_dim_id ( this1, this2 )
    !
    ok = ALL( l_ok )
    !
  END FUNCTION eq_var_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_var_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_var) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_var) , INTENT(IN) :: this2    ! 
    !! R&uuml;ckgabewert : Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i = 1, SIZE(ok)
       ok(i) = eq_var_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_var_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_var_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_var) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_var) , INTENT(IN) :: this2(:) ! 
    !! R&uuml;ckgabewert : Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i = 1, SIZE(ok)
       ok(i) = eq_var_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_var_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_var_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_var) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_var) , INTENT(IN) :: this2(:) ! 
    !! R&uuml;ckgabewert : Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i = 1, SIZE(ok)
       ok(i) = eq_var_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_var_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(+)-Methoden <<< [ERR_NO = 11000 bis 11999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(-)-Methoden <<< [ERR_NO = 12000 bis 12999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(*)-Methoden <<< [ERR_NO = 13000 bis 13999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(/)-Methoden <<< [ERR_NO = 14000 bis 14999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(>)-Methoden <<< [ERR_NO = 15000 bis 15999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(>=)-Methoden <<< [ERR_NO = 16000 bis 16999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(<)-Methoden <<< [ERR_NO = 17000 bis 17999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(<=)-Methoden <<< [ERR_NO = 18000 bis 18999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(/=)-Methoden <<< [ERR_NO = 19000 bis 19999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_var_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_var) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_var) , INTENT(IN) :: this2 ! 
    !! R&uuml;ckgabewert : Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_var_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_var_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_var) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_var) , INTENT(IN) :: this2    ! 
    !! R&uuml;ckgabewert : Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ok = .NOT. ( this1(:) == this2 )
    !
  END FUNCTION ne_var_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Vektor )<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION ne_var_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_var) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_var) , INTENT(IN) :: this2(:) ! 
    !! R&uuml;ckgabewert : Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ok = .NOT. ( this1 == this2(:) )
    !
  END FUNCTION ne_var_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_var_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_var) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_var) , INTENT(IN) :: this2(:) ! 
    !! R&uuml;ckgabewert : Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    ok = .NOT. ( this1(:) == this2(:) )
    !
  END FUNCTION ne_var_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-TEST-Methoden <<< [ERR_NO = 20000 bis 20999]
  ! ----------------------------------------------------------------------
  !
  !! f&uuml;hre einen Test der Methoden mit Standarddaten aus <BR>
  !! Subroutine erzeugt Fehlermeldungen
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-ASSIGNMENT(=)-Methoden <<< [ERR_NO = 21000 bis 21999]
  ! ----------------------------------------------------------------------
  !
  !! Zuweisung zwischen zwei skalaren Objekten des Typs "t_var" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE as_var_0_0 ( this1, this2 )
    !! Datenobjekt Ergebniswert
    TYPE (t_var) , INTENT(INOUT) :: this1 ! 
    !! Datenobjekt Zuweisungswert
    TYPE (t_var) , INTENT(IN)  :: this2 !
    !! Name der Subroutine
    CHARACTER (LEN=10) , PARAMETER:: c_upname='as_var_0_0' ! 
    !
    CALL new_var ( this1 )
    IF ( no_error( ) ) THEN
       this1%id     = this2%id
       this1%name   = this2%name
       this1%type   = this2%type
       this1%con_id = this2%con_id
       IF ( ASSOCIATED( this2%dim_id ) ) THEN
          CALL set_var_dim_id ( this1, this2%dim_id(:) )
       END IF
    END IF
    !
  END SUBROUTINE as_var_0_0
  !
  !! Zuweisung zwischen zwei vektoriellen Objekten des Typs "t_var" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE as_var_1_1 ( this1, this2 )
    !! Datenobjekt Ergebniswerte (Vektor)
    TYPE (t_var) , INTENT(INOUT) :: this1(:) ! 
    !! Datenobjekt Zuweisungswerte (Vektor)
    TYPE (t_var) , INTENT(IN)  :: this2(:) !
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i = 1, MIN(SIZE(this1),SIZE(this2))
       IF ( any_error( ) ) EXIT
       CALL as_var_0_0 ( this1(i), this2(i) )
    END DO
    !
  END SUBROUTINE as_var_1_1
  !
  ! ----------------------------------------------------------------------
  ! PUBLIC-GEN-ERROR-Methoden [ -6000 bis -6999 ]
  ! ----------------------------------------------------------------------
  !
  !! Erzeuge ggf. eine Fehlermeldung, falls eine Variable mit
  !! vorgegebenem Namen nicht in einer Liste von Variablen vorhanden ist <BR>
  !! Anmerkung: die anderen Typ-Komponenten spielen bei diesem Test keine Rolle <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE gen_error_if_var_not_exists_1_n ( this, name )
    !! Objektliste (t_var) in der gesucht werden soll
    TYPE (t_var)       , INTENT(IN) :: this(:) ! 
    !! gesuchter Name der Variablen
    CHARACTER (LEN=*)  , INTENT(IN) :: name    ! 
    !! Name der Subroutine
    CHARACTER (LEN=31) , PARAMETER  :: c_upname='gen_error_if_var_not_exists_1_n' ! 
    !
    IF ( .NOT. var_exists(this(:),name) ) THEN
       CALL setup_error_act ( all_errors(:), -6040, c_upname, c_modname )
       CALL setup_error_act ( '<ReqVarName>', TRIM(name) )
    END IF
    !
  END SUBROUTINE gen_error_if_var_not_exists_1_n
  !
  !! Erzeuge ggf. eine Fehlermeldung, falls eine Variable (Objekt) 
  !! mit ihrem Namen nicht in einer Liste von Variablen vorhanden ist <BR>
  !! Anmerkung: die anderen Typ-Komponenten spielen bei diesem Test keine Rolle <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE gen_error_if_var_not_exists_1_0 ( this1, this2 )
    !! Objektliste in der gesucht werden soll
    TYPE (t_var) , INTENT(IN) :: this1(:) ! 
    !! Variable, die mit ihrem Namen in der Objektliste vorhanden sein soll
    TYPE (t_var) , INTENT(IN) :: this2    ! 
    !
    CALL gen_error_if_var_not_exists_1_n ( this1(:), this2%name )
    !
  END SUBROUTINE gen_error_if_var_not_exists_1_0
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
  ! Lokale Methoden:
  !
  ! ----------------------------------------------------------------------
  ! >>> ALLGEMEIN-Methoden <<< [ERR_NO =  0001 bis  0999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der Fehlerbedingung 1 = Modul nicht initialisiert <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_initialised ( upname )         &
       RESULT( ok )
    !! Name der Subroutine die "ok_initialised" ruft
    CHARACTER (LEN=*) , INTENT(IN) :: upname ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Fehlernummer
    INTEGER            :: ierr    ! 
    !! Fehlertext
    CHARACTER (LEN=80) :: cerr(3) ! 
    !
    ok = initialised
    !
    IF ( .NOT. ok ) THEN
       WRITE(*,*) ' *** Warnung *** Modul "b_var" nicht initialisiert'
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_var ausfuehren'
       CALL setup_error_act ( ierr, cerr(:), upname, c_modname )
    END IF
    !
  END FUNCTION ok_initialised
  !
  !! Setzen der Fehlerbedingung 2 = Modul schon initialisiert <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION not_initialised ( upname ) &
       RESULT( ok )
    !! Name der Subroutine die "not_initialised" ruft
    CHARACTER (LEN=*) , INTENT(IN) :: upname ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .NOT. initialised
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 2, upname, c_modname )
    !
  END FUNCTION not_initialised
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-INIT-Methoden <<< [ERR_NO =  1000 bis  1999]
  ! ----------------------------------------------------------------------
  !
  !! Allokieren/Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE init_var_all_errors ( )
    !! Z&auml;hlervariable
    INTEGER :: i, ic ! 
    !
    DO i=1,2
       !
       ic = 0
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 1 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist nicht initialisiert\n'//&
               '--> INIT_var ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_var ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_var"\n'//&
               'Typ-Komponente = "dim_id"\n'//&
               '--> Code in Modul "b_var" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_var"\n'//&
               'Typ-Komponente = "id"\n'//&
               'aktuell        = <AktId>\n'//&
               'erforderlich   = id > 0\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6011 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_var"\n'//&
               'Typ-Komponente = "id"\n'//&
               'aktuell        = <AktId>\n'//&
               'dieser Wert kommt mehrfach in einem Vektor vor\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_var"\n'//&
               'Typ-Komponente = "name"\n'//&
               'aktuell        = undefined\n'//&
               'erforderlich   = name /= undefined\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6021 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_var"\n'//&
               'Typ-Komponente = "name"\n'//&
               'aktuell        = <AktName>\n'//&
               'dieser Wert kommt mehrfach in einem Vektor vor\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_var"\n'//&
               'Typ-Komponente = "type"\n'//&
               'aktuell        = "<AktType>"\n'//&
               'erforderlich   = siehe Werte des Feldes "c_var_type(:)"\n'//&
               '--> Daten pruefen, ggf. PRINT-/PRINT-STATIC-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_var"\n'//&
               'Typ-Komponente = "dim_id"\n'//&
               'Alle Elemente des Komponenten-Feldes "dim_id" muessen\n'//&
               'groesser als 0 sein !\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6041 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_var"\n'//&
               'Typ-Komponente = "dim_id"\n'//&
               'Das Komponenten-Feld "dim_id" enthaelt eine \n'//&
               'unbekannte Dimensions-Identifikationsnummer !\n'//&
               'ungueltige ID = <AktDimID>\n'//&
               '--> Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_var"\n'//&
               'Typ-Komponente = "con_id"\n'//&
               'aktuell        = "<AktConId>"\n'//&
               'erforderlich   = 0 oder groesser\n'//&
               '--> Daten pruefen, ggf. PRINT-/PRINT-STATIC-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code in Modul "b_var" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Fusszeilen\n'//&
               '--> Code in Modul "b_var" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Index des Datenobjektes (1D-Array)\n'//&
               '--> Code in Modul "b_var" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_var"\n'//&
               'Typ-Komponente = "id"\n'//&
               '--> Code in Modul "b_var" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_var"\n'//&
               'Typ-Komponente = "name"\n'//&
               '--> Code in Modul "b_var" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_var"\n'//&
               'Typ-Komponente = "type"\n'//&
               '--> Code in Modul "b_var" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_var"\n'//&
               'Typ-Komponente = "dim_id"\n'//&
               '--> Code in Modul "b_var" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_var"\n'//&
               'Typ-Komponente = "con_id"\n'//&
               '--> Code in Modul "b_var" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "b_var"\n'//&
               '--> Code in Modul "b_var" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_var"\n'//&
               'Typ-Komponente = "dim_id"\n'//&
               '--> Code in Modul "b_var" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ADD-Methoden\n'//&
               'Fehler beim Allokieren des Hilfsfeldes "this3(:)"\n'//&
               '--> Code in Modul "b_var" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8501 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ADD-Methoden\n'//&
               'Fehler beim De-Allokieren des Pointerfeldes "this1(:)"\n'//&
               '--> Code in Modul "b_var" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8502 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ADD-Methoden\n'//&
               'Fehler beim Allokieren des Pointerfeldes "this1(:)"\n'//&
               '--> Code in Modul "b_var" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8503 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ADD-Methoden\n'//&
               'Fehler beim De-Allokieren des Hilfsfeldes "this3(:)"\n'//&
               '--> Code in Modul "b_var" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9100 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Fehler beim Allokieren des RESULT-Feldes "val(:)"\n'//&
               'fuer die Rueckgabe eines Pointerfeldes\n'//&
               '--> Code in Modul "b_var" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Fehler beim Allokieren des Pointerfeldes "val(:)"\n'//&
               'zur Rueckgabe der Shape einer Variablen\n'//&
               'aktuelle Dimension = "<AktDim>"\n'//&
               '--> Code in Modul "b_var" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9510 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Eine oder mehrere Dimensionen konnten im uebergebenen\n'//&
               'Feld "dim(:)" nicht gefunden werden !\n'//&
               '--> Aufruf der Methode pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9600 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Fehler beim Allokieren des Pointerfeldes "start(:)"\n'//&
               'zur Rueckgabe der Startindices einer Variablen\n'//&
               'aktuelle Variable = "<AktVarName>"\n'//&
               '--> Code in Modul "b_var" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9700 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Fehler beim Allokieren des Pointerfeldes "step(:)"\n'//&
               'zur Rueckgabe der Anzahl der Daten entlang aller Dimensionen einer Variablen\n'//&
               'aktuelle Variable = "<AktVarName>"\n'//&
               '--> Code in Modul "b_var" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9800 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Fehler beim Allokieren des Pointerfeldes "stride(:)"\n'//&
               'zur Rueckgabe der Schrittweite fuer alle Dimensionen einer Variablen\n'//&
               'aktuelle Variable = "<AktVarName>"\n'//&
               '--> Code in Modul "b_var" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -6040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: modulspezifische GEN-ERROR-Methoden\n'//&
               'gewuenschte Variable (Name) nicht in Objektliste enthalten"\n'//&
               'gesuchter Name       = <ReqVarName>"\n'//&
               '--> Daten (Name) in rufender Programmeinheit pruefen' )
       END IF
       !
       ! Allokieren der Felder beim ersten Durchlauf (i==1)
       !
       IF ( i == 1 ) THEN
          ALLOCATE ( all_errors( ic ) )
          CALL new_error( all_errors(:) )
       END IF
       !
    END DO
    !
  END SUBROUTINE init_var_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_var_all_errors ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_var_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! --> nicht benoetigte ALLOC-Routinen bitte unbedingt loeschen <--------
  ! ----------------------------------------------------------------------
  !
  !! Allokieren der dynamischen Feld-Komponente "dim_id" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_var_dim_id ( this, idim )
    !! Datenobjekt
    TYPE (t_var) , INTENT(INOUT) :: this   ! 
    !! Dimension der dynamischen Feld-Komponente "dim_id"
    INTEGER               , INTENT(IN)  :: idim   ! 
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER  :: c_upname='alloc_var_dim_id' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%dim_id(idim), STAT=stat )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 8040, c_upname, c_modname, stat )
    !
  END SUBROUTINE alloc_var_dim_id
  !
  !! Initialisieren der Feld-Komponente "dim_id" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_var_dim_id ( this )
    !! Datenobjekt
    TYPE (t_var) , INTENT(INOUT) :: this   ! 
    !! Initialisierungswert dim_id
    INTEGER , PARAMETER :: c_var=0 ! 
    !
    this%dim_id(:) = c_var
    !
  END SUBROUTINE init_var_dim_id
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren der dynamischen Feld-Komponente "dim_id" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_var_dim_id ( this )
    !! Datenobjekt
    TYPE (t_var) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='dealloc_var_dim_id' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%dim_id ) ) THEN
       DEALLOCATE ( this%dim_id, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5040, c_upname, c_modname, stat )
       NULLIFY ( this%dim_id ) 
    END IF
    !
  END SUBROUTINE dealloc_var_dim_id
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe, ob die Komponente "id" eines Datenobjektes o.k. ist <BR>
  !! es wird gepr&uuml;ft, ob "id" gr&ouml;&szlig;er als 0 ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_var_id ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_var) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=09) , PARAMETER :: c_upname='ok_var_id' ! 
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !
    ok = ( this%id > 0 )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
       WRITE(ctxt,'(I10)') this%id
       CALL setup_error_act ( '<AktId>', ctxt )
    END IF
    !
  END FUNCTION ok_var_id
  !
  !! Pr&uuml;fe, ob die Komponente "name" eines Datenobjektes o.k. ist <BR>
  !! es wird gepr&uuml;ft, ob "name" ungleich 'undefined' ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_var_name ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_var) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=11) , PARAMETER :: c_upname='ok_var_name' ! 
    !
    ok = ( TRIM(this%name) /= 'undefined' )
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
    !
  END FUNCTION ok_var_name
  !
  !! Pr&uuml;fe, ob die Komponente "type" eines Datenobjektes o.k. ist <BR>
  !! es wird gepr&uuml;ft, ob "type" in dem Feld "c_var_type" vorkommt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_var_type ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_var) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=11) , PARAMETER :: c_upname='ok_var_type' ! 
    !
    ok = ( COUNT( c_var_type(:) == this%type ) == 1 )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
       CALL setup_error_act ( '<AktType>', TRIM( this%type ) )
    END IF
    !
  END FUNCTION ok_var_type
  !
  !! Pr&uuml;fe, ob die Komponente "con_id" eines Datenobjektes o.k. ist <BR>
  !! es wird gepr&uuml;ft, ob "con_id" in dem Feld "c_var_con_id" vorkommt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_var_con_id ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_var) , INTENT(IN) :: this
    !! Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=13) , PARAMETER :: c_upname='ok_var_con_id'
    !! Hilfsgroesse
    CHARACTER (LEN=10) :: char_con_id
    !
    ok = ( this%con_id >= 0 )
    IF ( .NOT. ok ) THEN
       WRITE( char_con_id ,'(I10.10)') this%con_id
       CALL setup_error_act ( all_errors(:), 6050, c_upname, c_modname )
       CALL setup_error_act ( '<AktConId>', char_con_id )
    END IF
    !
  END FUNCTION ok_var_con_id
  !
  !! Pr&uuml;fe, ob die Komponente "dim_id" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_var_dim_id ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_var) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=13) , PARAMETER :: c_upname='ok_var_dim_id' ! 
    !
    ok = .true.
    IF ( ASSOCIATED( this%dim_id ) ) THEN
       ok = ALL( this%dim_id(:) > 0 ) 
    END IF
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6040, c_upname, c_modname )
    !
  END FUNCTION ok_var_dim_id
  !
  !! Pr&uuml;fe, ob alle Id's verschieden sind
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_var_different_id ( this ) &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:) ! 
    !! R&uuml;ckgabewert: Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='ok_var_different_id' ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = ( COUNT( this(:)%id == this(i)%id ) == 1 )
       IF ( .NOT. ok(i) ) THEN
          CALL setup_error_act ( all_errors(:), 6011, c_upname, c_modname )
          WRITE(ctxt,'(I10)') this(i)%id
          CALL setup_error_act ( '<AktId>', ctxt )
       END IF
    END DO
    !
  END FUNCTION ok_var_different_id
  !
  !! Pr&uuml;fe, ob alle Namen verschieden sind
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_var_different_name ( this ) &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_var) , INTENT(IN) :: this(:) ! 
    !! R&uuml;ckgabewert: Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Funktion
    CHARACTER (LEN=21) , PARAMETER :: c_upname='ok_var_different_name' ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = ( COUNT( this(:)%name == this(i)%name ) == 1 )
       IF ( .NOT. ok(i) ) THEN
          CALL setup_error_act ( all_errors(:), 6021, c_upname, c_modname )
          CALL setup_error_act ( '<AktName>', TRIM( this(i)%name ) )
       END IF
    END DO
    !
  END FUNCTION ok_var_different_name
  !
  !! Pr&uuml;fe, ob alle im Komponentenfeld dim_id genannten IDs
  !! im Feld mit den Dimensionsangaben (Typ: t_dim) vorhanden sind.
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_var_dim_id_exists ( this, dim ) &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_var) , INTENT(IN) :: this ! 
    !! Feld mit Dimensionensangaben
    TYPE (t_dim) , INTENT(IN) :: dim(:) ! 
    !! R&uuml;ckgabewert: Testergebnis (Skalar)
    LOGICAL :: ok ! 
    ! Lokale Parameter und Variablen
    !! Name der Funktion
    CHARACTER (LEN=21) , PARAMETER :: c_upname='ok_var_dim_id_exists' ! 
    !! Lokales Feld mit Testergebnissen f&uuml;r die dim_id-Elemente
    LOGICAL :: l_ok(SIZE(this%dim_id)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !
    l_ok = .false.
    !
    DO i = 1, SIZE(this%dim_id)
       l_ok(i) = dim_exists( dim, this%dim_id(i) )
       IF ( .NOT. l_ok(i) ) THEN
          CALL setup_error_act ( all_errors(:), 6041, c_upname, c_modname )
          WRITE(ctxt,'(I10)') this%dim_id(i)
          CALL setup_error_act ( '<AktDimID>', ctxt )
       END IF
    END DO
    !
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_var_dim_id_exists
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "id" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_var_id ( this )
    !! Datenobjekt
    TYPE (t_var) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=12) , PARAMETER :: c_upname='print_var_id' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE ( UNIT=prn_lun, FMT= 8000, IOSTAT  = stat ) this%id
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente id  - - - - - - - - - - ',/&
           '# id   = ',I5,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_var_id
  !
  !! Drucke den Inhalt der Komponente "name" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_var_name ( this )
    !! Datenobjekt
    TYPE (t_var) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='print_var_name' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) this%name
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente name  - - - - - - - - - - ',/&
           '# name = ',A,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_var_name
  !
  !! Drucke den Inhalt der Komponente "type" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_var_type ( this )
    !! Datenobjekt
    TYPE (t_var) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='print_var_type' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) this%type
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente type  - - - - - - - - - - ',/&
           '# type = ',A,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_var_type
  !
  !! Drucke den Inhalt der Komponente "con_id" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_var_con_id ( this )
    !! Datenobjekt
    TYPE (t_var) , INTENT(IN) :: this
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='print_var_con_id'
    !! Statusvariable
    INTEGER :: stat
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) this%con_id
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente con_id  - - - - - - - - - ',/&
           '# con_id = ',I10,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_var_con_id
  !
  !! Drucke den Inhalt der Komponente "dim_id" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_var_dim_id ( this )
    !! Datenobjekt
    TYPE (t_var) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='print_var_dim_id' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
    IF ( ASSOCIATED(this%dim_id) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) this%dim_id
    ELSE 
       WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) 
    END IF
    WRITE ( UNIT=prn_lun, FMT=8005, IOSTAT=stat )
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7040, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente dim_id  - - - - - - - - - - ')
8001 FORMAT &
          ('# dim_id = ',10I5)
8005 FORMAT &
          ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_var_dim_id
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! hole ein bestimmtes Element aus dem Feld "dim_id".  <BR>
  !! RESULT=0, falls "dim_id" nicht assoziiert.<BR>
  !! RESULT=0, falls gesuchtes Feldelement nicht existiert.  <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_specific_dim_id ( dim_id, idim ) &
       RESULT( val ) 
    !! Pointer-Feld mit IDs der Dimensionen
    INTEGER , POINTER     :: dim_id(:) !
    !! Index der gesuchten Dimensions-ID im Feld
    INTEGER , INTENT(IN)   :: idim     !
    !! R&uuml;ckgabewert gesuchte ID (Skalar)
    INTEGER  :: val ! 
    !
    IF ( ASSOCIATED( dim_id ) ) THEN
       IF ( idim <= SIZE(dim_id) ) THEN
          val = dim_id(idim)
       ELSE
          val = 0
       END IF
    ELSE 
       val = 0
    END IF
    !
  END FUNCTION get_specific_dim_id
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe Komponente "id" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_var_id ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_var) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_var) , INTENT(IN) :: this2 ! 
    !! R&uuml;ckgabewert : Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = ( this1%id == this2%id )
    !
  END FUNCTION eq_var_id
  !
  !! pr&uuml;fe Komponente "name" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_var_name ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_var) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_var) , INTENT(IN) :: this2 ! 
    !! R&uuml;ckgabewert : Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    IF ( LEN_TRIM( this1%name ) == LEN_TRIM( this2%name ) ) THEN
       ok = ( this1%name == this2%name )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION eq_var_name 
  !
  !! pr&uuml;fe Komponente "type" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_var_type ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_var) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_var) , INTENT(IN) :: this2 ! 
    !! R&uuml;ckgabewert : Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    IF ( LEN_TRIM( this1%type ) == LEN_TRIM( this2%type ) ) THEN
       ok = ( this1%type == this2%type )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION eq_var_type 
  !
  !! pr&uuml;fe Komponente "con_id" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_var_con_id ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_var) , INTENT(IN) :: this1
    !! Vergleichsobjekt (Skalar)
    TYPE (t_var) , INTENT(IN) :: this2
    !! R&uuml;ckgabewert : Testergebnis (Skalar)
    LOGICAL :: ok
    !
    ok = ( this1%con_id == this2%con_id )
    !
  END FUNCTION eq_var_con_id 
  !
  !! pr&uuml;fe Komponente "dim_id" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_var_dim_id ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_var) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_var) , INTENT(IN) :: this2 ! 
    !! R&uuml;ckgabewert : Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    IF      ( .NOT. ASSOCIATED( this1%dim_id ) .AND. .NOT. ASSOCIATED( this2%dim_id ) ) THEN
       ok = .true.
    ELSE IF ( ASSOCIATED( this1%dim_id )       .AND. ASSOCIATED( this2%dim_id )       ) THEN
       IF ( SIZE(this1%dim_id) == SIZE(this2%dim_id) ) THEN
          ok = ALL( this1%dim_id(:) == this2%dim_id(:) )
       ELSE
          ok = .false.
       END IF
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION eq_var_dim_id 
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(+)-Methoden <<< [ERR_NO = 11000 bis 11999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(-)-Methoden <<< [ERR_NO = 12000 bis 12999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(*)-Methoden <<< [ERR_NO = 13000 bis 13999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(/)-Methoden <<< [ERR_NO = 14000 bis 14999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(>)-Methoden <<< [ERR_NO = 15000 bis 15999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(>=)-Methoden <<< [ERR_NO = 16000 bis 16999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(<)-Methoden <<< [ERR_NO = 17000 bis 17999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(>=)-Methoden <<< [ERR_NO = 18000 bis 18999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> SONSTIGES <<<
  ! ----------------------------------------------------------------------
  !
  !! Gro&szlig;buchstaben in Kleinbuchstaben umwandeln (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_lowercase_char_0 ( in ) &
       RESULT( out )
    !! Zeichenfolge (Original)
    CHARACTER (LEN=*) , INTENT(IN) :: in  ! 
    !! R&uuml;ckgabewert : Zeichenfolge (konvertiert in Kleinbuchstaben)
    CHARACTER (LEN=LEN(in))        :: out ! 
    !! Z&auml;hler 
    INTEGER :: i, ic ! 
    !
    out = in
    DO i=1,LEN(out)
       ic = IACHAR(in(i:i))
       IF ( ic >= 65 .AND. ic <= 90 ) out(i:i) = ACHAR(ic+32)
    END DO
    !
  END FUNCTION get_lowercase_char_0
  !
  !! Gro&szlig;buchstaben in Kleinbuchstaben umwandeln (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_lowercase_char_1 ( in ) &
       RESULT( out )
    !! Zeichenfolge (Original)
    CHARACTER (LEN=*) , INTENT(IN) :: in(:)         ! 
    !! R&uuml;ckgabewert : Zeichenfolge (konvertiert in Kleinbuchstaben)
    CHARACTER (LEN=LEN(in))        :: out(SIZE(in)) ! 
    !! Z&auml;hler 
    INTEGER :: i ! 
    !
    DO i=1,SIZE(out)
       out(i) = get_lowercase_char_0 ( in(i) )
    END DO
    !
  END FUNCTION get_lowercase_char_1
  !
END MODULE b_var
! TailOfBaseModule --------------------------------------------------------
