! -------------------------------------------------------------------------
! HeadOfBaseModule --------------------------------------------------------
!
!! <H2>Basismethods for modelling sediment grains</h2>
!! @author Elisabeth Rudolph / Holger Weilbeer
!! @version 1.26 vom 02/23/05, Quellcode: mod_b_grain.f90
!! <HR>
!! Defining of datatype "t_grain" and related methods <BR>
!! <HR>
!! <H3>Copyright-Hinweis</H3>
!!                                                                   <BR>
!! Copyright (C) 2002 <A HREF="http://www.hamburg.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!!                                                                   <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2002-04-12 : E. Rudolph, H. Weilbeer : Startversion
!  01.02 : 2002-05-02 : H. Weilbeer             : Schoenheitsreparaturen
!  01.03 : 2002-05-28 : G. Lang                 : Korrektur in MERGE fuer trc_op
!  01.04 : 2002-06-05 : G. Lang                 : ok_initialised modifiziert
!  01.05 : 2002-06-12 : G. Lang                 : INIT/CLEAR und SETUP_*_PRN_LUN
!  01.06 : 2002-07-01 : G. Lang                 : all_grain_names_are_unique und all_grain_mids_are_unique
!  01.07 : 2002-07-08 : E. Rudolph              : html Text fuer einige oeffentliche Methoden ergaenzt
!  01.08 : 2002-08-06 : A. Malcherek            : alle REAL's in KIND=Double umgefummelt
!  01.09 : 2002-09-11 : E. Rudolph              : html text verschoenert
!  01.10 : 2002-09-24 : H. Weilbeer             : dxx verallgemeinert
!  01.11 : 2002-10-01 : H. Weilbeer             : dxx nochmals veraendert
!  01.12 : 2002-12-03 : H. Weilbeer             : Fehlerabfrage in get_grain_dxx geaendert
!  01.13 : 2003-01-03 : A. Malcherek            : get_grain_dxx auch fuer unnormierte Korngroessenverteilungen verallgemeinert
!  01.14 : 2003-03-05 : G. Lang                 : Anpassungen TV12 vom Dez. 2002
!  01.15 : 2003-03-05 : G. Lang                 : Anpassung Entwicklungsgeschichte
!  01.16 : 2003-07-01 : G. Lang                 : Stokessche Sinkgeschwindigkeit
!  01.17 : 2003-07-01 : G. Lang                 : ... Korrekturen
!  01.18 : 2003-07-31 : G. Lang                 : vereinzelt INTENT(OUT) -> INTENT(INOUT) gewandelt
!  01.19 : 2003-10-29 : G. Lang                 : MaxFrac,MinFrac,SumFrac erweitert
!  01.20 : 2003-05-24 : G. Lang                 : Sinkgeschwindigkeit nach Dietrich
!  01.21 : 2004-06-18 : S. Spohr                : +PUBLIC-SUB: get_grain_size_sort_idx,
!                                                 get_grain_dxx_d : optionaler Parameter incr_size_idx zugefuegt
!  01.22 : 2004-11-16 : J. Juerges              : get_grain_dxx_d soll nur arbeiten, wenn es keine Fehler
!                                                 in den Eingangsdaten gefunden hat
!
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! mod_b_grain ist ein Basismodul im Rahmen der Sedimorph-Entwicklung    <BR>
!! 
!! 1. Definition des Datentyps t_grain,  mit dem ein Einzelkorn          <BR>
!!     beschrieben werden kann                                           <BR>
!!
!! 2. Bereitstellen von Operationen auf dem Datentyp t_grain             <BR>
!! 
!! Durch ein in Feld von t-grain kann das Sediment klassifiziert werden. <BR>
!! In Verbindung mit prozentualen Anteilen koennen auch Kornverteilungen <BR>
!! beschrieben werden.               <BR>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt den selbst definierten Datentyp t_grain <BR>
!! zur Verf&uuml;gung. Dieser besteht aus den folgenden Komponenten:      <BR>
!! <OL>
!!     <LI> name : Name of the sediment fraction                                       
!!     <LI> mid :  Material identification number (Sediment Class Key)                              
!!     <LI> size : Mean diameter of a sediment fraction in [m]
!!     <LI> density: Density of a sediment fraction in [kg/m**3]
!! </OL>
!!                                                                  <BR>
!! <HR>
!! <H3>Verwenden des Moduls</H3>
!!                                                                  <BR>
!! Die Leistungen des Moduls k&ouml;nnen wie folgt in Anspruch genommen werden: <BR>
!! <OL>
!!    <LI> Einbinden des Moduls mittels USE-Anweisung in der rufenden Programmeinheit;
!!    <LI> Initialisieren des Moduls b_grain mit INIT-Methode;
!!    <LI> ggf. Verwenden verschiedener SETUP-Methoden;
!!    <LI> Verwenden beliebiger Modul-Methoden (nicht INIT und CLEAR);
!!    <LI> De-Initialisieren des Moduls b_grain mit CLEAR-Methode.
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
!! 05010 = De-Allokieren der Komponente name des Typs t_grain   <BR>
!! 05020 = De-Allokieren der Komponente mid des Typs t_grain   <BR>
!! 05030 = De-Allokieren der Komponente size des Typs t_grain   <BR>
!! 05040 = De-Allokieren der Komponente density des Typs t_grain   <BR>
!! <HR>
!! OK-Methoden           [  6000 bis  6999 ]                        <BR>
!! 06010 = Fehler in Komponente name des Typs t_grain           <BR>
!! 06020 = Fehler in Komponente mid des Typs t_grain           <BR>
!! 06030 = Fehler in Komponente size des Typs t_grain           <BR>
!! 06040 = Fehler in Komponente density des Typs t_grain           <BR>
!! <HR>
!! PRINT-Methoden        [  7000 bis  7999 ]                        <BR>
!! 07001 = Drucken der Kopfzeilen                                   <BR>
!! 07002 = Drucken der Fu&szlig;zeilen                              <BR>
!! 07003 = Drucken des Index des Datenobjektes (1D-Array)           <BR>
!! 07010 = Drucken der Komponente name des Typs t_grain         <BR>
!! 07020 = Drucken der Komponente mid des Typs t_grain         <BR>
!! 07030 = Drucken der Komponente size des Typs t_grain         <BR>
!! 07040 = Drucken der Komponente density des Typs t_grain         <BR>
!! 07500 = Drucken der statischen Daten (ohne Fehlermeldungen)      <BR>
!! <HR>
!! SET-Methoden          [  8000 bis  8999 ]                        <BR>
!! 08010 = Allokieren von Komponente name des Typs t_grain      <BR>
!! 08020 = Allokieren von Komponente mid des Typs t_grain      <BR>
!! 08030 = Allokieren von Komponente size des Typs t_grain      <BR>
!! 08040 = Allokieren von Komponente density des Typs t_grain      <BR>
!! <HR>
!! GET-Methoden          [  9000 bis  9999 ]                        <BR>
!! <HR>
!! OPERATOR(==)-Methoden [ 10000 bis 10999 ]                        <BR>
!! <HR>
!! OPERATOR(>)-Methoden  [ 15000 bis 15999 ]                        <BR>
!! <HR>
!! OPERATOR(>=)-Methoden [ 16000 bis 16999 ]                        <BR>
!! <HR>
!! OPERATOR(<)-Methoden  [ 17000 bis 17999 ]                        <BR>
!! <HR>
!! OPERATOR(<=)-Methoden [ 18000 bis 18999 ]                        <BR>
!! <HR>
!! OPERATOR(/=)-Methoden [ 19000 bis 19999 ]                        <BR>
!! <HR>
!! modul-spezifische Methoden   [      < 0 ]                        <BR>
!
MODULE b_grain
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] Basis-Modul mit globalen Konstantwerten [ggf. entfernen]
  USE b_constants, ONLY : &
       ! Konstantwerte
       Double
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
       setup_error_prn_lun, &
       setup_error_trc_lun, &
       set_error_ierr,      &
       set_error_cerr
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
  ! [C.1] (maximal) einen oeffentlich zugaenglichen Typ deklarieren
  !
  !! name : Name of the sediment fraction                                       
  !! mid :  Material identification number (Sediment Class Key)                              
  !! size : Mean diameter of a sediment fraction in [m]
  !! density: Density of a sediment fraction in [kg/m**3]
  TYPE , PUBLIC :: t_grain
     PRIVATE
     CHARACTER (LEN=40) :: name 
     INTEGER :: mid 
     REAL (KIND = Double) :: size 
     REAL (KIND = Double) :: density 
  END TYPE t_grain
  !
  ! [C.2] Konstantwerte (Parameter) [moeglichst nicht verwenden]
  !
  !! <kurze Beschreibung des &ouml;ffentlichen Parameter1 angeben>
  !<DataTypeP1> , PUBLIC, PARAMETER <,AttributesP1> :: <ParameterName1> ! Wollen wir nicht ...
  !
  ! [C.3] Variablen [moeglichst nicht verwenden]
  !
  !! <kurze Beschreibung der &ouml;ffentlichen Variablen1 angeben>
  !<DataTypeV1> , PUBLIC <,AttributesV1> :: <VariableNameV1> ! Wollen wir nicht ...
  !
  ! [C.4] Schnittstellen
  !
  ! [C.4.1] erforderliche oeffentliche Schnittstellen
  !
  ! Hinweis: verschiedene Methoden arbeiten auf Skalar und 1D-Array.
  !          Ggf. weitere ergaenzen (z.B. 2D-Array) falls sinnvoll.
  !
  !! ggf. Allokieren der statischen Daten des Moduls; <BR>
  !! Initialisieren der statischen Modul-Daten mit Default-Werten.
  INTERFACE init_grain
     MODULE PROCEDURE init_grain_d !  ERHW: _d: default, keine besondere Bedeutung
  END INTERFACE 
  !! ggf. De-Allokieren der statischen Daten des Moduls; <BR>
  !! Re-Initialisieren einiger statischer Daten mit Default-Werten.
  INTERFACE clear_grain
     MODULE PROCEDURE clear_grain_d ! 
  END INTERFACE
  !! Setzen der logischen Kanalnummer <EM>PRN_LUN</EM> f&uuml;r PRINT-Methoden; <BR>
  !! keine Ausgabe: <EM>PRN_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>PRN_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>PRN_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_grain_prn_lun
     MODULE PROCEDURE setup_grain_prn_lun_d ! 
  END INTERFACE
  !! Setzen der logischen Kanalnummer <EM>TRC_LUN</EM> f&uuml;r TRACE-Methoden; <BR>
  !! keine Ausgabe: <EM>TRC_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>TRC_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>TRC_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_grain_trc_lun
     MODULE PROCEDURE setup_grain_trc_lun_d ! 
  END INTERFACE
  !! Neues Objekt vom Typ "t_grain" erzeugen; <BR>
  !! NULLIFY f&uuml;r dynamische Komponenten-Felder; <BR>
  !! Initialisieren mit Default-Werten: <BR>
  !! a) ein Objekt (Skalar) erzeugen; <BR>
  !! b) mehrere Objekte (Vektor) erzeugen. <BR>
  !! <EM>Hinweis:</EM> "t_grain" beinhaltet die Eigenschaften 
  !! <EM>einer</EM> Kornfraktion; Eine Sedimentverteilung wird 
  !! durch ein Feld vom Typ "t_grain" beschrieben.
  INTERFACE new_grain
     MODULE PROCEDURE new_grain_0  ! Version fuer Skalar
     MODULE PROCEDURE new_grain_1  ! Version fuer 1D-Array
  END INTERFACE
  !! Objekt vom Typ "t_grain" vernichten; <BR>
  !! ggf. De-Allokieren von Memory; <BR>
  !! teilweise Re-Initialisieren mit Default-Werten: <BR>
  !! a) ein Objekt (Skalar) vernichten; <BR>
  !! b) viele Objekte (Vektor) vernichten.
  INTERFACE kill_grain
     MODULE PROCEDURE kill_grain_0 ! Version fuer Skalar
     MODULE PROCEDURE kill_grain_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Objekt(e) vom Typ "t_grain" auf G&uuml;ltigkeit pr&uuml;fen: <BR>
  !! a) ein Objekt (Skalar) pr&uuml;fen; <BR>
  !! b) viele Objekte (Vektor) pr&uuml;fen. <BR>
  INTERFACE ok_grain
     MODULE PROCEDURE ok_grain_0 ! Version fuer Skalar
     MODULE PROCEDURE ok_grain_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Alle Komponenten des Typs "t_grain" auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_grain
     MODULE PROCEDURE print_grain_0 ! Version fuer Skalar
     MODULE PROCEDURE print_grain_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Alle statischen Daten des Moduls auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_grain_static
     MODULE PROCEDURE print_grain_static_d ! 
  END INTERFACE
  !! Alle Fehlermeldungen des Moduls auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_grain_all_errors
     MODULE PROCEDURE print_grain_all_errors_d ! 
  END INTERFACE
  !! Setze Komponente "name" in "t_grain" auf Benutzerwert(e): <BR>
  !! a) Komponente in einem Objekt (Skalar) setzen; <BR>
  !! b) Komponente in mehreren Objekten (Vektor) setzen.
  INTERFACE set_grain_name
     MODULE PROCEDURE set_grain_name_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_grain_name_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !! Setze Komponente "mid" in "t_grain" auf Benutzerwert(e): <BR>
  !! a) Komponente in einem Objekt (Skalar) setzen; <BR>
  !! b) Komponente in mehreren Objekten (Vektor) setzen.
  INTERFACE set_grain_mid
     MODULE PROCEDURE set_grain_mid_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_grain_mid_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !! Setze Komponente "size" in "t_grain" auf Benutzerwert(e): <BR>
  !! a) Komponente in einem Objekt (Skalar) setzen; <BR>
  !! b) Komponente in mehreren Objekten (Vektor) setzen.
  INTERFACE set_grain_size
     MODULE PROCEDURE set_grain_size_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_grain_size_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !! Setze Komponente "density" in "t_grain" auf Benutzerwert(e): <BR>
  !! a) Komponente in einem Objekt (Skalar) setzen; <BR>
  !! b) Komponente in mehreren Objekten (Vektor) setzen.
  INTERFACE set_grain_density
     MODULE PROCEDURE set_grain_density_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_grain_density_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !! Hole Komponente "name" aus "t_grain": <BR>
  !! a) Komponente aus einem Objekt (Skalar); <BR>
  !! b) Komponente aus mehreren Objekten (Vektor). <BR>
  !! <EM>Hinweis:</EM> es wird eine Kopie der Daten zur&uuml;ckgegeben.
  INTERFACE get_grain_name
     MODULE PROCEDURE get_grain_name_0_0 ! Skalar
     MODULE PROCEDURE get_grain_name_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "mid" aus "t_grain": <BR>
  !! a) Komponente aus einem Objekt (Skalar); <BR>
  !! b) Komponente aus mehreren Objekten (Vektor). <BR>
  !! <EM>Hinweis:</EM> es wird eine Kopie der Daten zur&uuml;ckgegeben.
  INTERFACE get_grain_mid
     MODULE PROCEDURE get_grain_mid_0_0 ! Skalar
     MODULE PROCEDURE get_grain_mid_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "size" aus "t_grain": <BR>
  !! a) Komponente aus einem Objekt (Skalar); <BR>
  !! b) Komponente aus mehreren Objekten (Vektor). <BR>
  !! <EM>Hinweis:</EM> es wird eine Kopie der Daten zur&uuml;ckgegeben.
  INTERFACE get_grain_size
     MODULE PROCEDURE get_grain_size_0_0 ! Skalar
     MODULE PROCEDURE get_grain_size_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "density" aus "t_grain": <BR>
  !! a) Komponente aus einem Objekt (Skalar); <BR>
  !! b) Komponente aus mehreren Objekten (Vektor). <BR>
  !! <EM>Hinweis:</EM> es wird eine Kopie der Daten zur&uuml;ckgegeben.
  INTERFACE get_grain_density
     MODULE PROCEDURE get_grain_density_0_0 ! Skalar
     MODULE PROCEDURE get_grain_density_1_0 ! Vektor
  END INTERFACE
  !! Pr&uuml;fe, ob alle Komponenten "name" verschieden sind
  INTERFACE all_grain_names_are_unique
     MODULE PROCEDURE all_grain_names_are_unique_1
  END INTERFACE
  !! Pr&uuml;fe, ob alle Komponenten "mid" verschieden sind
  INTERFACE all_grain_mids_are_unique
     MODULE PROCEDURE all_grain_mids_are_unique_1
  END INTERFACE
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  ! ... ggf. ergaenzen
  !
  ! [C.5] Zuweisungen
  !
  ! ... ggf. ergaenzen
  !
  ! [C.6] Operatoren
  !
  ! [C.6.1] unbedingt erforderliche oeffentliche Operatoren
  !
  ! Hinweis: Operatoren wurden fuer vier verschieden Parameter-
  !          Konstellationen formuliert. Ggf. nicht sinnvolle
  !          Konstellationen entfernen oder weitere sinnvolle
  !          hinzufuegen.
  !
  !! Pr&uuml;fen zweier Datenobjekte "t_grain" auf Gleichheit; <BR>
  !! zwei Objekte sind dann gleich, wenn sie in allen Komponenten
  !! &uuml;bereinstimmen: <BR>
  !! a) grain1    == grain2    ; <BR>
  !! b) grain1    == grain2(:) ; <BR>
  !! c) grain1(:) == grain2    ; <BR>
  !! d) grain1(:) == grain2(:) . <BR>
  !! <EM>Hinweis:</EM> Skalar entspricht Kornfraktion, Vektor 
  !! entspricht Kornverteilung. Ausserhalb des Moduls vergleichen 
  !! wir nur <EM>&Auml;pfel mit &Auml;pfeln</EM>, spezielles intern (s.u.)
  INTERFACE OPERATOR (==)
     MODULE PROCEDURE eq_grain_0_0  ! Skalar / Skalar
     MODULE PROCEDURE eq_grain_0_1  ! Skalar / Vektor
     MODULE PROCEDURE eq_grain_1_1  ! Vektor / Vektor
     MODULE PROCEDURE eq_grain_1_0  ! Vektor / Skalar
  END INTERFACE
  !
  ! [C.6.2] optional vorhandene oeffentliche Operatoren
  !
  !! Addition zweier Datenobjekte "t_grain"
  !! Subtraktion zweier Datenobjekte "t_grain"
  !! Multiplikation zweier Datenobjekte "t_grain"
  !! Division zweier Datenobjekte "t_grain"
  !
  !! Vergleich "&#062;" zweier Datenobjekte "t_grain": <BR>
  !! a) grain1    &#062; grain2    ; <BR>
  !! b) grain1    &#062; grain2(:) ; <BR>
  !! c) grain1(:) &#062; grain2    ; <BR>
  !! d) grain1(:) &#062; grain2(:) . <BR>
  !! <EM>Hinweis:</EM> der Vergleich wird auf Basis der 
  !! Komponente "size" durchgef&uuml;hrt.
  INTERFACE OPERATOR (>)
     MODULE PROCEDURE gt_grain_0_0  ! Skalar / Skalar
     MODULE PROCEDURE gt_grain_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE gt_grain_1_0  ! Vektor / Skalar
     MODULE PROCEDURE gt_grain_1_1  ! Vektor / Vektor
  END INTERFACE
  !! Vergleich "&#062;=" zweier Datenobjekte "t_grain": <BR>
  !! a) grain1    &#062;= grain2    ; <BR>
  !! b) grain1    &#062;= grain2(:) ; <BR>
  !! c) grain1(:) &#062;= grain2    ; <BR>
  !! d) grain1(:) &#062;= grain2(:) . <BR>
  !! <EM>Hinweis:</EM> der Vergleich wird auf Basis der 
  !! Komponente "size" durchgef&uuml;hrt.
  INTERFACE OPERATOR (>=)
     MODULE PROCEDURE ge_grain_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ge_grain_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE ge_grain_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ge_grain_1_1  ! Vektor / Vektor
  END INTERFACE
  !! Vergleich "&#060;" zweier Datenobjekte "t_grain": <BR>
  !! a) grain1    &#060; grain2    ; <BR>
  !! b) grain1    &#060; grain2(:) ; <BR>
  !! c) grain1(:) &#060; grain2    ; <BR>
  !! d) grain1(:) &#060; grain2(:) . <BR>
  !! <EM>Hinweis:</EM> der Vergleich wird auf Basis der 
  !! Komponente "size" durchgef&uuml;hrt.
  INTERFACE OPERATOR (<) 
     MODULE PROCEDURE lt_grain_0_0  ! Skalar / Skalar
     MODULE PROCEDURE lt_grain_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE lt_grain_1_0  ! Vektor / Skalar
     MODULE PROCEDURE lt_grain_1_1  ! Vektor / Vektor
  END INTERFACE
  !! Vergleich "&#060;=" zweier Datenobjekte "t_grain": <BR>
  !! a) grain1    &#060;= grain2    ; <BR>
  !! b) grain1    &#060;= grain2(:) ; <BR>
  !! c) grain1(:) &#060;= grain2    ; <BR>
  !! d) grain1(:) &#060;= grain2(:) . <BR>
  !! <EM>Hinweis:</EM> der Vergleich wird auf Basis der 
  !! Komponente "size" durchgef&uuml;hrt.
  INTERFACE OPERATOR (<=)
     MODULE PROCEDURE le_grain_0_0  ! Skalar / Skalar
     MODULE PROCEDURE le_grain_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE le_grain_1_0  ! Vektor / Skalar
     MODULE PROCEDURE le_grain_1_1  ! Vektor / Vektor
  END INTERFACE
  !! Pr&uuml;fen zweier Datenobjekte "t_grain" auf Ungleichheit; <BR>
  !! zwei Objekte sind dann verschieden, wenn sie in mindestens
  !! einer Komponente nicht &uuml;bereinstimmen: <BR>
  !! a) grain1    /= grain2    ; <BR>
  !! b) grain1    /= grain2(:) ; <BR>
  !! c) grain1(:) /= grain2    ; <BR>
  !! d) grain1(:) /= grain2(:) . 
  INTERFACE OPERATOR (/=)
     MODULE PROCEDURE ne_grain_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ne_grain_0_1  ! Skalar / Vektor
     MODULE PROCEDURE ne_grain_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ne_grain_1_1  ! Vektor / Vektor
  END INTERFACE
  !! Pr&uuml;fe, ob eine/mehrere Fraktionen in einer
  !! Kornverteilung grain(:) (t_grain) verhanden sind
  !! oder nicht: <BR>
  !! a) pr&uuml;fe, ob eine Fraktion "name" vorhanden ist; <BR>
  !! b) pr&uuml;fe, ob eine Fraktion "mid" vorhanden ist; <BR>
  !! c) pr&uuml;fe, ob mehrere Fraktionen "name(:)" vorhanden sind; <BR>
  !! d) pr&uuml;fe, ob mehrere Fraktionen "mid(:)" vorhanden sind.
  INTERFACE all_grain_available
     MODULE PROCEDURE all_grain_available_name_1_0
     MODULE PROCEDURE all_grain_available_mid_1_0
     MODULE PROCEDURE all_grain_available_name_1_1
     MODULE PROCEDURE all_grain_available_mid_1_1
  END INTERFACE
  !! Ermittle die Anzahl der zur Verf&uuml;gung stehenden Fraktionen
  !! (Gr&ouml;sse eines Feldes grain(:) (t_grain).
  INTERFACE get_nof_grain_available
     MODULE PROCEDURE get_nof_grain_available_d
  END INTERFACE
  !! Sortiere grain(:) (t_grain) nach dem Korndurchmesser (aufsteigend)
  INTERFACE get_grain_size_sorted
     MODULE PROCEDURE get_grain_size_sorted_d
  END INTERFACE
  !! Gib Indexfeld fuer Sortierung von grain(:) (t_grain) nach aufsteigendem Korndurchmesser
  INTERFACE get_grain_size_sort_idx
     MODULE PROCEDURE get_grain_size_sort_idx_d
  END INTERFACE
  !! Sortiere grain(:) (t_grain) nach der Dichte (aufsteigend)
  INTERFACE get_grain_density_sorted
     MODULE PROCEDURE get_grain_density_sorted_d
  END INTERFACE
  !! Berechne f&uuml;r grain(:) (t_grain), percent(:) den mittleren 
  !! Korndurchmesser "d_m" einer Sedimentverteilung.
  INTERFACE get_grain_mean_size
     MODULE PROCEDURE get_grain_mean_size_d
  END INTERFACE
  !! Berechne f&uuml;r grain(:) (t_grain), percent(:) die mittlere 
  !! Dichte "rho_m" einer Sedimentverteilung.
  INTERFACE get_grain_mean_density
     MODULE PROCEDURE get_grain_mean_density_d
  END INTERFACE
  !! Berechnet den Korndurchmesser dxx mit dem Siebdurchgang "xx"
  !! f&uuml;r grain(:) (t_grain), percent(:) einer Sedimentverteilung. <EM>
  !! <EM>Hinweis:</EM> Innerhalb einer Fraktion wird linear interpoliert.
  INTERFACE get_grain_dxx
     MODULE PROCEDURE get_grain_dxx_d
  END INTERFACE
  !! Bestimme die Sortierung (Standardabweichung) einer 
  !! grain(:) (t_grain), percent(:) Sedimentverteilung.
  INTERFACE get_grain_sorting
     MODULE PROCEDURE get_grain_sorting_d
  END INTERFACE
  !! Bestimme die Schiefe einer Sedimentverteilung grain(:) (t_grain), percent(:).
  INTERFACE get_grain_skewness
     MODULE PROCEDURE get_grain_skewness_d
  END INTERFACE
  !! Bestimme die Kurtosis einer Sedimentverteilung grain(:) (t_grain), percent(:).
  INTERFACE get_grain_kurtosis
     MODULE PROCEDURE get_grain_kurtosis_d
  END INTERFACE
  !! Berechne die effektive kornbezogene Rauheit (hier: K = 3 d_m) einer
  !! Sedimentverteilung grain(:) (t_grain), percent(:).
  INTERFACE get_grain_roughness
     MODULE PROCEDURE get_grain_roughness_d
  END INTERFACE
  !! Liefere eine Auswahl sel_grain(:) (t_grain) aus einer 
  !! Liste grain(:) (t_grain) anhand <BR>
  !! a) vorgegebener Namen "name(:)", oder <BR>
  !! b) vorgegebener Kennungen "mid(:)".
  INTERFACE get_grain_selection
     MODULE PROCEDURE get_grain_selection_name
     MODULE PROCEDURE get_grain_selection_mid
  END INTERFACE
  !! Liefert eine Indexliste idx(:) zur Auswahl aus einer 
  !! gr&ouml;sseren Liste grain(:) (t_grain) von Fraktionen anhand <BR>
  !! a) vorgegebener Namen "name(:)", oder <BR>
  !! b) vorgegebener Kennungen "mid(:)".
  INTERFACE get_grain_index_list
     MODULE PROCEDURE get_grain_index_list_name
     MODULE PROCEDURE get_grain_index_list_mid
  END INTERFACE
  !! berechnet die Sinkgeschwindigkeit in Wasser nach der Stokeschen Formel: <BR>
  !! a) f&uuml;r eine Fraktion "grain", oder <BR>
  !! b) f&uuml;r mehrere Fraktionen "grain(:)". 
  INTERFACE get_grain_stokes_velocity
     MODULE PROCEDURE get_grain_stokes_velocity_0
     MODULE PROCEDURE get_grain_stokes_velocity_1
  END INTERFACE
  !! berechnet die Sinkgeschwindigkeit in Wasser nach der Dietrichschen Formel: <BR>
  !! a) f&uuml;r eine Fraktion "grain", oder <BR>
  !! b) f&uuml;r mehrere Fraktionen "grain(:)". 
  INTERFACE get_grain_dietrich_velocity
     MODULE PROCEDURE get_grain_dietrich_velocity_0
     MODULE PROCEDURE get_grain_dietrich_velocity_1
  END INTERFACE
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  ! [C.7.1] unbedingt erforderliche oeffentliche Methoden
  !
  PUBLIC :: init_grain                 ! Initialisieren (Modul)
  PUBLIC :: clear_grain                ! De-Initialisieren (Modul)
  PUBLIC :: setup_grain_prn_lun        ! Setzen prn_lun 
  PUBLIC :: setup_grain_trc_lun        ! Setzen trc_lun 
  PUBLIC :: new_grain                  ! Erzeugen 
  PUBLIC :: kill_grain                 ! Vernichten
  PUBLIC :: ok_grain                   ! Pruefen
  PUBLIC :: print_grain                ! Drucken
  PUBLIC :: print_grain_static         ! Drucken aller statischen Daten
  PUBLIC :: print_grain_all_errors     ! Drucken aller (moeglichen) Fehlermeldungen
  PUBLIC :: set_grain_name             ! Setzen der Komponente name
  PUBLIC :: set_grain_mid              ! Setzen der Komponente mid
  PUBLIC :: set_grain_size             ! Setzen der Komponente size
  PUBLIC :: set_grain_density          ! Setzen der Komponente density
  PUBLIC :: get_grain_name             ! Holen der Komponente name
  PUBLIC :: get_grain_mid              ! Holen der Komponente mid
  PUBLIC :: get_grain_size             ! Holen der Komponente size
  PUBLIC :: get_grain_density          ! Holen der Komponente density
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: OPERATOR(==)                      ! Operator "=="
  PUBLIC :: OPERATOR(>)                       ! Operator ">"
  PUBLIC :: OPERATOR(>=)                      ! Operator ">="
  PUBLIC :: OPERATOR(<)                       ! Operator "<"
  PUBLIC :: OPERATOR(<=)                      ! Operator "<="
  PUBLIC :: OPERATOR(/=)                      ! Operator "/="\
  !
  ! [C.7.3] modulspezifische oeffentliche Methoden
  !
  PUBLIC :: all_grain_available               ! Tested, ob alle abgefragten Fraktionen vorhanden sind
  PUBLIC :: get_nof_grain_available           ! Ermittelt die Anzahl der zur Verfuegung stehenden Fraktionen
  PUBLIC :: get_grain_size_sorted             ! Sortierung nach dem Korndurchmesser (aufsteigend)
  PUBLIC :: get_grain_size_sort_idx           ! Indexfeld :Sortierung nach Korndurchmesser (aufsteigend)
  PUBLIC :: get_grain_density_sorted          ! Sortierung nach der Dichte (aufsteigend)
  PUBLIC :: get_grain_mean_size               ! Mittelwert Korngroesse
  PUBLIC :: get_grain_mean_density            ! Mittelwert Sedimentdichte
  PUBLIC :: get_grain_dxx                     ! dxx 
  PUBLIC :: get_grain_sorting                 ! Standardabweichung
  PUBLIC :: get_grain_skewness                ! Schiefe
  PUBLIC :: get_grain_kurtosis                ! Kurtosis (Mass fuer Abweichung von Gauss-Kurve)
  PUBLIC :: get_grain_roughness               ! Kornrauheit nach van Rijn
  PUBLIC :: get_grain_selection               ! Liefert eine Auswahl aus einer Liste
  PUBLIC :: get_grain_index_list              ! Liefert die Indexliste einer Auswahl aus einer Liste
  PUBLIC :: all_grain_names_are_unique        ! Eindeutigkeit von "name"
  PUBLIC :: all_grain_mids_are_unique         ! Eindeutigkeit von "mid"
  PUBLIC :: get_grain_stokes_velocity         ! Sinkgeschwindigkeit in Wasser nach der Stokeschen Formel
  PUBLIC :: get_grain_dietrich_velocity       ! Sinkgeschwindigkeit in Wasser nach der Dietrichschen Formel
  !
  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] lokale Typdefinitionen
  !
  !ERHW: Brauchen wir nicht ...
  !
  ! <kurze Beschreibung des Typs>
  !TYPE :: t_<DerivedTypeName>
  !   PRIVATE
  !   <DataType> <,Attributes> :: <ComponentName> ! <kurze Beschreibung>
  !END TYPE t_<DerivedTypeName>
  !
  ! [D.2] Konstantwerte (Parameter)
  !
  !! Name des Moduls
  CHARACTER (LEN=31), PARAMETER :: c_modname      = 'b_grain' ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.          ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1               ! 
  !! Anzahl der Datenkomponenten des Typs t_grain
  INTEGER           , PARAMETER :: c_nofcomp      = 4                ! ggf. modifizieren
  !! Anzahl der Fehlermeldungen
  INTEGER           , PARAMETER :: c_nofallerrors = 27               ! ggf. modifizieren
  !
  ! [D.3] Variablen (statische Daten des Moduls)
  !
  !! Feld zur Aufnahme aller Fehlermeldungen des Moduls
  TYPE (t_error) , SAVE :: all_errors(c_nofallerrors)! 
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
  SUBROUTINE init_grain_d &
       ( )
    !
    USE b_error, ONLY : DEBUG_b
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='init_grain_d' 
    !
    IF ( .NOT. initialised ) THEN
       !
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_b > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "b_grain" version 1.26 of 02/23/05                 '
          WRITE(*,*) ' Copyright (C) 2002 Bundesanstalt fuer Wasserbau'
          WRITE(*,*)
       END IF
       !
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       !
       ! [1.2.1] Error-Modul zuerst initialisieren
       !
       CALL init_error ( )
       !
       ! [1.2.2] ggf. weitere Module initialisieren
       !
       ! ... derzeit nicht erforderlich 
       !
       ! [1.3] vorlaeufiges Setzen von "initialised"
       !
       initialised = .true.
       !
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       !
       IF ( no_error( ) ) CALL init_grain_all_errors ( ) 
       !
       ! [1.5] Initialisieren der logischen Kanalnummern
       !
       prn_lun = c_lun
       trc_lun = c_lun
       !
       ! [1.5] ggf. weitere Initialsierungsmethoden rufen
       !
       ! [1.6] endgueltiges Setzen des Initialisierungs-Indikators
       !
       initialised = MERGE( .true., .false., no_error( ) )
       !
    END IF
    !
    ! 2.0 Initialisierungszaehler heraufsetzen
    !
    n_init = n_init + 1
    !
  END SUBROUTINE init_grain_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_grain_d &
       ( )
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='clear_grain_d' ! 
    !
    IF ( initialised .AND. n_init == 1) THEN
       !
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       !
       IF ( no_error( ) ) CALL clear_grain_all_errors ( ) 
       !
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       !
       prn_lun = c_lun
       trc_lun = c_lun
       !
       ! [1.3] ggf. weitere De-Initialsierungsmethoden rufen
       !
       ! [1.4] Rueck-Setzen des Initialisierungs-Indikators
       !
       initialised = MERGE( .false., .true., no_error( ) )
       !
       ! [1.5] alle mit USE eingebundenen Basis-Module de-initialisieren
       !
       ! [1.5.1] ggf. weitere Module de-initialisieren
       !
       ! ... derzeit nicht erforderlich
       !
       ! [1.5.2] Error-Modul zuletzt de-initialisieren
       !
       IF ( no_error( ) ) CALL clear_error ( )
       !
    END IF
    !
    ! 2.0 Initialisierungszaehler heruntersetzen
    !
    n_init = n_init - 1
    !
  END SUBROUTINE clear_grain_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_grain_prn_lun_d &
       ( lun )
    !
    ! Formalparameter
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='setup_grain_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       !
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       !
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
       ! ... weitere Basis-Module derzeit nicht erforderlich
       !
    END IF
    !
  END SUBROUTINE setup_grain_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_grain_trc_lun_d &
       ( lun )
    !
    ! Formalparameter
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='setup_grain_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       !
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       !
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       !
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
       ! ... weitere Basis-Module derzeit nicht erforderlich
       !
    END IF
    !
  END SUBROUTINE setup_grain_trc_lun_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_grain_0 &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_grain) , INTENT(INOUT) :: this ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='new_grain_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       !
       this%name = 'UNKNOWN'
       this%mid = 0
       this%size = 0.0
       this%density = 0.0
       !
    END IF
    !
  END SUBROUTINE new_grain_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_grain_1 &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_grain) , INTENT(INOUT) :: this(:) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='new_grain_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       !
       i = 0
       !
       DO
          !
          i = i + 1
          !
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          !
          CALL new_grain_0 ( this(i) )
          !
       END DO
       !
    END IF
    !
  END SUBROUTINE new_grain_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_grain_0 &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_grain) , INTENT(INOUT) :: this ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='kill_grain_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       !
       IF ( no_error( ) ) CALL new_grain_0 ( this )
       !
    END IF
    !
  END SUBROUTINE kill_grain_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_grain_1 &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_grain) , INTENT(INOUT) :: this(:) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='kill_grain_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       !
       i = 0
       !
       DO
          !
          i = i + 1
          !
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          !
          CALL kill_grain_0 ( this(i) )
          !
       END DO
       !
    END IF
    !
  END SUBROUTINE kill_grain_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_grain_0 &
       ( this )              &
       RESULT( ok )
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='ok_grain_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       !
       l_ok(1) = ok_grain_name( this )
       l_ok(2) = ok_grain_mid( this )
       l_ok(3) = ok_grain_size( this )
       l_ok(4) = ok_grain_density( this )
       !
    END IF
    !
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_grain_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_grain_1 &
       ( this )              &
       RESULT( ok )
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_grain) , INTENT(IN) :: this(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='ok_grain_1' 
    !! Z&auml;hler      
    INTEGER :: i ! 
    !
    ok = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       !
       i = 0
       !
       DO
          !
          i = i + 1
          !
          IF ( i > SIZE(this) ) EXIT
          !
          ok(i) = ok_grain_0 ( this(i) )
          !
       END DO
       !
    END IF
    !
  END FUNCTION ok_grain_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_grain_0 &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='print_grain_0' 
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
       IF ( no_error( ) ) CALL print_grain_name( this )
       IF ( no_error( ) ) CALL print_grain_mid( this )
       IF ( no_error( ) ) CALL print_grain_size( this )
       IF ( no_error( ) ) CALL print_grain_density( this )
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
8000 FORMAT('# Beginn Objekt t_grain ------------------------------')
8001 FORMAT('# Ende   Objekt t_grain ------------------------------')
    !
  END SUBROUTINE print_grain_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_grain_1 &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_grain) , INTENT(IN) :: this(:) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='print_grain_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       i = 0
       !
       DO
          !
          i = i + 1
          !
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          !
          WRITE &
               ( UNIT    = prn_lun,  &
                 FMT     = 8000,     & 
                 IOSTAT  = stat ) i
          !
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7003, c_upname, c_modname, stat )
          !
          IF ( no_error( ) ) CALL print_grain_0 ( this(i) )
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
  END SUBROUTINE print_grain_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_grain_static_d &
       ( )
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='print_grain_static_d' 
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
       IF ( no_error( ) ) CALL print_grain_all_errors_d ( )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls b_grain         ',/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
    '# initialised = ',L1,/ &
    '#      prn_op = ',L1,/ &
    '#      trc_op = ',L1,/ &
    '#     prn_lun = ',I5,/ &
    '#     trc_lun = ',I5,/ &
    '#      n_init = ',I5,/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
    '#------------------------------------------------------------') 
    !
  END SUBROUTINE print_grain_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_grain_all_errors_d &
       ( )
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='print_grain_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       CALL print_error( all_errors(:) )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
  END SUBROUTINE print_grain_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! --> nicht benoetigte SET-Routinen bitte unbedingt loeschen <----------
  ! ----------------------------------------------------------------------
  !
  ! --- Version(en) fuer skalare Komponente "name" [ggf. entfernen]
  !
  !! weise der Komponente "name" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_grain_name_0_0 &
       ( this, &
         val )
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_grain) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "name"
    CHARACTER (LEN=40)           , INTENT(IN)  :: val  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='set_grain_name_0_0' 
    !
    this%name = val
    !
  END SUBROUTINE set_grain_name_0_0
  !
  !! weise der Komponente "name" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_grain_name_1_0 &
       ( this, &
         val )
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_grain) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "name"
    CHARACTER (LEN=40)           , INTENT(IN)  :: val     ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='set_grain_name_1_0' 
    !
    this%name = val
    !
  END SUBROUTINE set_grain_name_1_0
  !
  ! --- Version(en) fuer skalare Komponente "mid" [ggf. entfernen]
  !
  !! weise der Komponente "mid" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_grain_mid_0_0 &
       ( this, &
         val )
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_grain) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "mid"
    INTEGER           , INTENT(IN)  :: val  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='set_grain_mid_0_0' 
    !
    this%mid = val
    !
  END SUBROUTINE set_grain_mid_0_0
  !
  !! weise der Komponente "mid" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_grain_mid_1_0 &
       ( this, &
         val )
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_grain) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "mid"
    INTEGER           , INTENT(IN)  :: val     ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='set_grain_mid_1_0' 
    !
    this%mid = val
    !
  END SUBROUTINE set_grain_mid_1_0
  !
  ! --- Version(en) fuer skalare Komponente "size" [ggf. entfernen]
  !
  !! weise der Komponente "size" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_grain_size_0_0 &
       ( this, &
         val )
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_grain)       , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "size"
    REAL (KIND = Double) , INTENT(IN)  :: val  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='set_grain_size_0_0' 
    !
    this%size = val
    !
  END SUBROUTINE set_grain_size_0_0
  !
  !! weise der Komponente "size" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_grain_size_1_0 &
       ( this, &
         val )
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_grain)       , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "size"
    REAL (KIND = Double) , INTENT(IN)  :: val     ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='set_grain_size_1_0' 
    !
    this%size = val
    !
  END SUBROUTINE set_grain_size_1_0
  !
  !
  ! --- Version(en) fuer skalare Komponente "density" [ggf. entfernen]
  !
  !! weise der Komponente "density" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_grain_density_0_0 &
       ( this, &
         val )
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_grain)       , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "density"
    REAL (KIND = Double) , INTENT(IN)  :: val  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='set_grain_density_0_0' 
    !
    this%density = val
    !
  END SUBROUTINE set_grain_density_0_0
  !
  !! weise der Komponente "density" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_grain_density_1_0 &
       ( this, &
         val )
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_grain)       , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "density"
    REAL (KIND = Double) , INTENT(IN)  :: val     ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='set_grain_density_1_0' 
    !
    this%density = val
    !
  END SUBROUTINE set_grain_density_1_0
  !
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! --> nicht benoetigte GET-Routinen bitte unbedingt loeschen <----------
  ! ----------------------------------------------------------------------
  !
  ! --- Version(en) fuer skalare Komponente "name" [ggf. entfernen]
  !
  !! hole die Komponente "name" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_grain_name_0_0 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "name" (Skalar)
    CHARACTER (LEN=40) :: val  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_grain_name_0_0' 
    !
    val = this%name
    !
  END FUNCTION get_grain_name_0_0
  !
  !! hole die Komponente "name" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_grain_name_1_0 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_grain) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "name"
    CHARACTER (LEN=40) :: val(SIZE(this))  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_grain_name_1_0' 
    !
    val = this%name
    !
  END FUNCTION get_grain_name_1_0
  !
  !
  ! --- Version(en) fuer skalare Komponente "mid" [ggf. entfernen]
  !
  !! hole die Komponente "mid" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_grain_mid_0_0 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "mid" (Skalar)
    INTEGER :: val  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_grain_mid_0_0' 
    !
    val = this%mid
    !
  END FUNCTION get_grain_mid_0_0
  !
  !! hole die Komponente "mid" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_grain_mid_1_0 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_grain) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "mid"
    INTEGER :: val(SIZE(this))  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_grain_mid_1_0' 
    !
    val = this%mid
    !
  END FUNCTION get_grain_mid_1_0
  !
  ! --- Version(en) fuer skalare Komponente "size" [ggf. entfernen]
  !
  !! hole die Komponente "size" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_grain_size_0_0 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "size" (Skalar)
    REAL (KIND = Double) :: val  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_grain_size_0_0' 
    !
    val = this%size
    !
  END FUNCTION get_grain_size_0_0
  !
  !! hole die Komponente "size" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_grain_size_1_0 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_grain) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "size"
    REAL (KIND = Double) :: val(SIZE(this))  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_grain_size_1_0' 
    !
    val = this%size
    !
  END FUNCTION get_grain_size_1_0
  !
  !
  ! --- Version(en) fuer skalare Komponente "density" [ggf. entfernen]
  !
  !! hole die Komponente "density" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_grain_density_0_0 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "density" (Skalar)
    REAL (KIND = Double) :: val  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_grain_density_0_0' 
    !
    val = this%density
    !
  END FUNCTION get_grain_density_0_0
  !
  !! hole die Komponente "density" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_grain_density_1_0 &
       ( this ) &
       RESULT( val ) 
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_grain) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "density"
    REAL (KIND = Double) :: val(SIZE(this))  ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_grain_density_1_0' 
    !
    val = this%density
    !
  END FUNCTION get_grain_density_1_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_grain_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='eq_grain_0_0' 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    ! Hinweis: sollten die zum Test der Gleichheit "==" notwendigen
    ! Operationen sehr einfach und kurz sein, so kann ggf.
    ! auf die nachfolgend gerufenen FUNCTIONs verzichtet werden.
    !
    l_ok(1) = ( this1%name == this2%name )
    l_ok(2) = ( this1%mid == this2%mid )
    l_ok(3) = ( this1%size == this2%size )
    l_ok(4) = ( this1%density == this2%density )
    !
    ok = ALL( l_ok )
    !
  END FUNCTION eq_grain_0_0 
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_grain_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    ! ERHW: 1_0 und 0_1 bleiben im Modul vorhanden, um z.B. beim Einlesen des Sediment Classification File
    ! verschiedene Kornfraktionen auf Gleichheit zu ueberpruefen.
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_grain) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this2    ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='eq_grain_1_0' 
    !! Z&auml;hler
    INTEGER :: i ! 
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = eq_grain_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_grain_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_grain_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_grain) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='eq_grain_0_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = eq_grain_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_grain_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_grain_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_grain) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_grain) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='eq_grain_1_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = eq_grain_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_grain_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> ERHW: PUBLIC-OPERATOR(+-*/)-Methoden brauchen wir nicht.
  ! ----------------------------------------------------------------------
  !
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(>)-Methoden <<< [ERR_NO = 15000 bis 15999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">" ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION gt_grain_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='gt_grain_0_0' 
    !
    ! Hinweis: sollten die zum Vergleich ">" notwendigen 
    ! Operationen sehr einfach und kurz sein, so kann ggf.
    ! auf die nachfolgend gerufenen FUNCTIONs verzichtet werden.
    !
    ! ERHW: Wir vergleichen nur nach Groesse. dto. fuer >=, < und <=
    !
    ok = ( this1%size > this2%size )
    !
  END FUNCTION gt_grain_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">" ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION gt_grain_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_grain) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this2    ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='gt_grain_1_0' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = gt_grain_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION gt_grain_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">" ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION gt_grain_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_grain) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='gt_grain_0_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = gt_grain_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION gt_grain_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">" ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION gt_grain_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_grain) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_grain) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='gt_grain_1_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = gt_grain_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION gt_grain_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(>=)-Methoden <<< [ERR_NO = 16000 bis 16999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">=" ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ge_grain_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='ge_grain_0_0' 
    !
    ok = ( this1%size >= this2%size )
    !
  END FUNCTION ge_grain_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">=" ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ge_grain_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_grain) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this2    ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='ge_grain_1_0' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = ge_grain_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION ge_grain_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">=" ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ge_grain_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_grain) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='ge_grain_0_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = ge_grain_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION ge_grain_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf ">=" ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ge_grain_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_grain) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_grain) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='ge_grain_1_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = ge_grain_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION ge_grain_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(<)-Methoden <<< [ERR_NO = 17000 bis 17999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<" ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION lt_grain_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='lt_grain_0_0' 
    !
    ok = ( this1%size < this2%size )
    !
  END FUNCTION lt_grain_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<" ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION lt_grain_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_grain) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this2    ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='lt_grain_1_0' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = lt_grain_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION lt_grain_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<" ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION lt_grain_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_grain) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='lt_grain_0_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = lt_grain_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION lt_grain_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<" ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION lt_grain_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_grain) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_grain) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='lt_grain_1_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = lt_grain_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION lt_grain_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(<=)-Methoden <<< [ERR_NO = 18000 bis 18999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<=" ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION le_grain_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='le_grain_0_0' 
    !
    ok = ( this1%size <= this2%size )
    !
  END FUNCTION le_grain_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<=" ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION le_grain_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_grain) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this2    ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='le_grain_1_0' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = le_grain_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION le_grain_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<=" ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
 FUNCTION le_grain_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_grain) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='le_grain_0_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = le_grain_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION le_grain_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf "<=" ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION le_grain_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_grain) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_grain) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='le_grain_1_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    ! bitte die DO-Schleife und den Aufruf
    ! der Skalar/Skalar-Version beibehalten
    DO i=1,SIZE(ok)
       ok(i) = le_grain_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION le_grain_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(/=)-Methoden <<< [ERR_NO = 19000 bis 19999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_grain_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='ne_grain_0_0' 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_grain_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_grain_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_grain) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this2    ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='ne_grain_1_0' 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_grain_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Vektor )<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
 FUNCTION ne_grain_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_grain) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='ne_grain_0_1' 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_grain_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_grain_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_grain) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_grain) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='ne_grain_1_1' 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_grain_1_1
  !
  ! ----------------------------------------------------------------------
  ! PUBLIC-UNIQUE-Methoden
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;ft, ob alle Komponenten "name" ein-eindeutig sind
  FUNCTION all_grain_names_are_unique_1 &
       ( this )                       &
       RESULT( ok )
    !
    !! Feld mit (verschiedenen) Korngr&ouml;&szlig;en
    TYPE (t_grain) , INTENT(IN) :: this(:) ! 
    !! R&uuml;ckgabewert
    LOGICAL :: ok ! 
    ! lokale Variablen
    INTEGER :: i ! 
    !
    ok = .true.
    !
    DO i=1,SIZE(this)
       IF ( .NOT. ok ) EXIT
       ok = ( COUNT ( this(i)%name == this%name ) == 1 )
    END DO
    !
  END FUNCTION all_grain_names_are_unique_1
  !
  !! Pr&uuml;ft, ob alle Komponenten "mid" ein-eindeutig sind
  FUNCTION all_grain_mids_are_unique_1 &
       ( this )                       &
       RESULT( ok )
    !
    !! Feld mit (verschiedenen) Korngr&ouml;&szlig;en
    TYPE (t_grain) , INTENT(IN) :: this(:) ! 
    !! R&uuml;ckgabewert
    LOGICAL :: ok ! 
    ! lokale Variablen
    INTEGER :: i ! 
    !
    ok = .true.
    !
    DO i=1,SIZE(this)
       IF ( .NOT. ok ) EXIT
       ok = ( COUNT ( this(i)%mid == this%mid ) == 1 )
    END DO
    !
  END FUNCTION all_grain_mids_are_unique_1
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
       WRITE(*,*) ' *** Warnung *** Modul "b_grain" nicht initialisiert'
       !
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       !
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_grain ausfuehren'
       !
       CALL setup_error_act ( ierr, cerr(:), upname, c_modname )
       !
    END IF
    !
  END FUNCTION ok_initialised
  !
  !! Setzen der Fehlerbedingung 2 = Modul schon initialisiert <BR>
  !! Function erzeugt Fehlermeldungen
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
  !! Allokieren/Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE init_grain_all_errors &
       ( )
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER  :: c_upname='init_grain_all_errors' !
    !
    CALL new_error( all_errors(:) )
    !
    ! Index 001
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors( 1), 1 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors( 1), &
         'Fehlerkategorie: ALLGEMEIN\n'//&
         'Modul ist nicht initialisiert\n'//&
         '--> INIT_grain ausfuehren' )
    ! Index 002
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors( 2), 2 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors( 2), &
         'Fehlerkategorie: ALLGEMEIN\n'//&
         'Modul ist schon initialisiert\n'//&
         '--> CLEAR_grain ausfuehren' )
    ! Index 003
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors( 3), 5010 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors( 3), &
         'Fehlerkategorie: KILL-Methoden\n'//&
         'De-Allocate-Fehler fuer Komponente von "t_grain"\n'//&
         'Typ-Komponente = "name"\n'//&
         '--> Code in Modul "b_grain" pruefen' )
    ! Index 004
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors( 4), 5020 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors( 4), &
         'Fehlerkategorie: KILL-Methoden\n'//&
         'De-Allocate-Fehler fuer Komponente von "t_grain"\n'//&
         'Typ-Komponente = "mid"\n'//&
         '--> Code in Modul "b_grain" pruefen' )
    ! Index 005
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors( 5), 5030 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors( 5), &
         'Fehlerkategorie: KILL-Methoden\n'//&
         'De-Allocate-Fehler fuer Komponente von "t_grain"\n'//&
         'Typ-Komponente = "size"\n'//&
         '--> Code in Modul "b_grain" pruefen' )
    ! Index 006
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors( 6), 5040 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors( 6), &
         'Fehlerkategorie: KILL-Methoden\n'//&
         'De-Allocate-Fehler fuer Komponente von "t_grain"\n'//&
         'Typ-Komponente = "density"\n'//&
         '--> Code in Modul "b_grain" pruefen' )

    ! Index 007
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors( 7), 6010 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors( 7), &
         'Fehlerkategorie: OK-Methoden\n'//&
         'Fehler in Komponente von "t_grain"\n'//&
         'Typ-Komponente = "name"\n'//&
         '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
    ! Index 008
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors( 8), 6020 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors( 8), &
         'Fehlerkategorie: OK-Methoden\n'//&
         'Fehler in Komponente von "t_grain"\n'//&
         'Typ-Komponente = "mid"\n'//&
         '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
    ! Index 009
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors( 9), 6030 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors( 9), &
         'Fehlerkategorie: OK-Methoden\n'//&
         'Fehler in Komponente von "t_grain"\n'//&
         'Typ-Komponente = "size"\n'//&
         '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
    ! Index 010
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(10), 6040 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(10), &
         'Fehlerkategorie: OK-Methoden\n'//&
         'Fehler in Komponente von "t_grain"\n'//&
         'Typ-Komponente = "density"\n'//&
         '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
    ! Index 011
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(11), 7001 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(11), &
         'Fehlerkategorie: PRINT-Methoden\n'//&
         'Drucken der Kopfzeilen\n'//&
         '--> Code in Modul "b_grain" pruefen' )
    ! Index 012
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(12), 7002 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(12), &
         'Fehlerkategorie: PRINT-Methoden\n'//&
         'Drucken der Fusszeilen\n'//&
         '--> Code in Modul "b_grain" pruefen' )
    ! Index 013
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(13), 7003 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(13), &
         'Fehlerkategorie: PRINT-Methoden\n'//&
         'Drucken des Index des Datenobjektes (1D-Array)\n'//&
         '--> Code in Modul "b_grain" pruefen' )
    ! Index 014
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(14), 7010 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(14), &
         'Fehlerkategorie: PRINT-Methoden\n'//&
         'Fehler beim Drucken von Objekt "t_grain"\n'//&
         'Typ-Komponente = "name"\n'//&
         '--> Code in Modul "b_grain" / Daten pruefen' )
    ! Index 015
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(15), 7020 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(15), &
         'Fehlerkategorie: PRINT-Methoden\n'//&
         'Fehler beim Drucken von Objekt "t_grain"\n'//&
         'Typ-Komponente = "mid"\n'//&
         '--> Code in Modul "b_grain" / Daten pruefen' )
    ! Index 016
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(16), 7030 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(16), &
         'Fehlerkategorie: PRINT-Methoden\n'//&
         'Fehler beim Drucken von Objekt "t_grain"\n'//&
         'Typ-Komponente = "size"\n'//&
         '--> Code in Modul "b_grain" / Daten pruefen' )
    ! Index 017
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(17), 7040 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(17), &
         'Fehlerkategorie: PRINT-Methoden\n'//&
         'Fehler beim Drucken von Objekt "t_grain"\n'//&
         'Typ-Komponente = "density"\n'//&
         '--> Code in Modul "b_grain" / Daten pruefen' )
    ! Index 018
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(18), 7500 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(18), &
         'Fehlerkategorie: PRINT-Methoden\n'//&
         'Fehler beim Drucken statischer Daten aus "b_grain"\n'//&
         '--> Code in Modul "b_grain" / Daten pruefen' )
    ! Index 019
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(19), 8010 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(19), &
         'Fehlerkategorie: NEW-Methoden\n'//&
         'Allocate-Fehler fuer Komponente von "t_grain"\n'//&
         'Typ-Komponente = "name"\n'//&
         '--> Code in Modul "b_grain" pruefen' )
    ! Index 020
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(20), 8020 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(20), &
         'Fehlerkategorie: NEW-Methoden\n'//&
         'Allocate-Fehler fuer Komponente von "t_grain"\n'//&
         'Typ-Komponente = "mid"\n'//&
         '--> Code in Modul "b_grain" pruefen' )
    ! Index 021
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(21), 8030 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(21), &
         'Fehlerkategorie: NEW-Methoden\n'//&
         'Allocate-Fehler fuer Komponente von "t_grain"\n'//&
         'Typ-Komponente = "size"\n'//&
         '--> Code in Modul "b_grain" pruefen' )
    ! Index 022
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(22), 8040 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(22), &
         'Fehlerkategorie: NEW-Methoden\n'//&
         'Allocate-Fehler fuer Komponente von "t_grain"\n'//&
         'Typ-Komponente = "density"\n'//&
         '--> Code in Modul "b_grain" pruefen' )
    ! Index 023
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(23), 21010 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(23), &
         'Fehlerkategorie: Modulspezifische Methode\n'//&
         'Summe der Anteile aller betrachteten Sedimentfraktionen ist groesser als 1 \n'//&
         'Minimalwert = <MinFrac> \n'//&
         'Maximalwert = <MaxFrac> \n'//&
         'Summe       = <SumFrac> \n'//&
         '--> Aufrufendes Programm pruefen' )
    ! Index 024
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(24), 21011 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(24), &
         'Fehlerkategorie: Modulspezifische Methode\n'//&
         'Wertebereich fuer dxx falsch gewaehlt \n'//&
         'Bereich: 0  >= dxx >= 100      \n'//&
         '--> Aufrufendes Programm pruefen' )
    ! Index 025
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(25), 21012 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(25), &
         'Fehlerkategorie: Modulspezifische Methode\n'//&
         'Summe der proz. Anteile erreicht nicht dxx \n'//&
         '--> Aufrufendes Programm pruefen' )
    ! Index 026
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(26), 21020 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(26), &
         'Fehlerkategorie: Modulspezifische Methode\n'//&
         'Ausgewaehlte Komponente mid ist nicht in Sedimentverteilung vorhanden\n'//&
!         'Selection: Typ-Komponente = "mid"\n'//&
         '--> Aufrufendes Programm pruefen' )
    ! Index 027
    IF ( no_error( ) ) CALL set_error_ierr ( all_errors(27), 21030 )
    IF ( no_error( ) ) CALL set_error_cerr ( all_errors(27), &
         'Fehlerkategorie: Modulspezifische Methode\n'//&
         'Ausgewaehlte Komponente name ist nicht in Sedimentverteilung vorhanden\n'//&
!         'Selection: Typ-Komponente = "name"\n'//&
         '--> Aufrufendes Programm pruefen' )
    !
  END SUBROUTINE init_grain_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_grain_all_errors &
       ( )
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER  :: c_upname='clear_grain_all_errors' !
    !
    CALL kill_error( all_errors(:) )
    !
  END SUBROUTINE clear_grain_all_errors
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
  ! --> nicht benoetigte ALLOC-Routinen bitte unbedingt loeschen <-------- ... done
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! --> nicht benoetigte INIT-Routinen bitte unbedingt loeschen <---------
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren der Feld-Komponente "name" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_grain_name &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_grain) , INTENT(INOUT) :: this   ! 
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER  :: c_upname='init_grain_name' !
    !! Initialisierungswert name
    CHARACTER (LEN=40) , PARAMETER :: c_var='UNKNOWN'
    !
    this%name = c_var
    !
  END SUBROUTINE init_grain_name
  !
  !! Initialisieren der Feld-Komponente "mid" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_grain_mid &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_grain) , INTENT(INOUT) :: this   ! 
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER  :: c_upname='init_grain_mid' !
    !! Initialisierungswert mid
    INTEGER , PARAMETER :: c_var=0
    !
    this%mid = c_var
    !
  END SUBROUTINE init_grain_mid
  !
  !! Initialisieren der Feld-Komponente "size" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_grain_size &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_grain) , INTENT(INOUT) :: this   ! 
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER  :: c_upname='init_grain_size' !
    !! Initialisierungswert size
    REAL (KIND = Double) , PARAMETER :: c_var=0.0
    !
    this%size = c_var
    !
  END SUBROUTINE init_grain_size
  !
  !! Initialisieren der Feld-Komponente "density" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_grain_density &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_grain) , INTENT(INOUT) :: this   ! 
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER  :: c_upname='init_grain_density' !
    !! Initialisierungswert density
    REAL (KIND = Double) , PARAMETER :: c_var=0.0
    !
    this%density = c_var
    !
  END SUBROUTINE init_grain_density
  !
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! --> nicht benoetigte DEALLOC-Routinen bitte unbedingt loeschen <------ ... done
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe, ob die Komponente "name" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_grain_name &
       ( this ) &
       RESULT( ok )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_grain) , INTENT(IN) :: this ! 
    !
    ! Rueckgabewert
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    ! lokale Parameter / Variablen
    !! Name der Funktion
    CHARACTER (LEN=31) , PARAMETER :: c_upname='ok_grain_name' ! 
    !
    CHARACTER (LEN=7) , PARAMETER :: test_unknown='UNKNOWN'
    !
    ! ERHW: "UNKNOWN" und leere Strings werden als Fehler bemerkt.
    !
    ok = .false.
    !
    ok = ((this%name/=test_unknown).or.(len_trim(this%name).gt.0))
    !
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
    !
  END FUNCTION ok_grain_name
  !
  !! Pr&uuml;fe, ob die Komponente "mid" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_grain_mid &
       ( this ) &
       RESULT( ok )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_grain) , INTENT(IN) :: this ! 
    !
    ! Rueckgabewert
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    ! lokale Parameter / Variablen
    !! Name der Funktion
    CHARACTER (LEN=31) , PARAMETER :: c_upname='ok_grain_mid' ! 
    !
    !ERHW: Nur Ueberpruefung nach /= 0
    !
    ok = (abs(this%mid) > 0)
    !
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
    !
  END FUNCTION ok_grain_mid
  !
  !! Pr&uuml;fe, ob die Komponente "size" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_grain_size &
       ( this ) &
       RESULT( ok )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_grain) , INTENT(IN) :: this ! 
    !
    ! Rueckgabewert
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    ! lokale Parameter / Variablen
    !! Name der Funktion
    CHARACTER (LEN=31) , PARAMETER :: c_upname='ok_grain_size' ! 
    !
    !
    !ERHW: Nur Ueberpruefung nach >= 0
    !
    ok = (this%size > 0)
    !
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
    !
  END FUNCTION ok_grain_size
  !
  !! Pr&uuml;fe, ob die Komponente "density" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_grain_density &
       ( this ) &
       RESULT( ok )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_grain) , INTENT(IN) :: this ! 
    !
    ! Rueckgabewert
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    ! lokale Parameter / Variablen
    !! Name der Funktion
    CHARACTER (LEN=31) , PARAMETER :: c_upname='ok_grain_density' ! 
    !
    !
    !ERHW: Nur Ueberpruefung nach >= 0
    !
    ok = (this%density > 0)
    !
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6040, c_upname, c_modname )
    !
  END FUNCTION ok_grain_density
  !
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "name" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_grain_name &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_grain) , INTENT(IN) :: this ! 
    ! 
    ! lokale Parameter / Variablen
    !! Name der Funktion
    CHARACTER (LEN=31) , PARAMETER :: c_upname='print_grain_name' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) this%name
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
    !
8000 FORMAT ('# Inhalt der Komponente name:',A40)
    !
  END SUBROUTINE print_grain_name
  !
  !! Drucke den Inhalt der Komponente "mid" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_grain_mid &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_grain) , INTENT(IN) :: this ! 
    ! 
    ! lokale Parameter / Variablen
    !! Name der Funktion
    CHARACTER (LEN=31) , PARAMETER :: c_upname='print_grain_mid' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) this%mid
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente mid:',I15)
    !
  END SUBROUTINE print_grain_mid
  !
  !! Drucke den Inhalt der Komponente "size" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_grain_size &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_grain) , INTENT(IN) :: this ! 
    ! 
    ! lokale Parameter / Variablen
    !! Name der Funktion
    CHARACTER (LEN=31) , PARAMETER :: c_upname='print_grain_size' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) this%size
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente size: ',F15.8)
    !
  END SUBROUTINE print_grain_size
  !
  !! Drucke den Inhalt der Komponente "density" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_grain_density &
       ( this )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_grain) , INTENT(IN) :: this ! 
    ! 
    ! lokale Parameter / Variablen
    !! Name der Funktion
    CHARACTER (LEN=31) , PARAMETER :: c_upname='print_grain_density' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) this%density
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7040, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente density: ',F15.8)
    !
  END SUBROUTINE print_grain_density
  !
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe Komponente "name" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_grain_name &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Referenzobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='eq_grain_name' 
    !
    ok = ( this1%name == this2%name )
    !
  END FUNCTION eq_grain_name
  !
  !! pr&uuml;fe Komponente "mid" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_grain_mid &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Referenzobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='eq_grain_mid' 
    !
    ok = ( this1%mid == this2%mid )
    !
  END FUNCTION eq_grain_mid 
  !
  !! pr&uuml;fe Komponente "size" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_grain_size &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Referenzobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='eq_grain_size' 
    !
    ok = ( this1%size == this2%size )
    !
  END FUNCTION eq_grain_size 
  !
  !! pr&uuml;fe Komponente "density" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_grain_density &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Referenzobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='eq_grain_density' 
    !
    ok = ( this1%density == this2%density )
    !
  END FUNCTION eq_grain_density 
  !
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(>)-Methoden <<< [ERR_NO = 15000 bis 15999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe Komponente "size" zweier Datenobjekte auf ">" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION gt_grain_size &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Referenzobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='gt_grain_size' 
    !
    ok = ( this1%size > this2%size )
    !
  END FUNCTION gt_grain_size 
  !
  !! pr&uuml;fe Komponente "density" zweier Datenobjekte auf ">" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION gt_grain_density &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Referenzobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='gt_grain_density' 
    !
    ok = ( this1%density > this2%density )
    !
  END FUNCTION gt_grain_density 
  !
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(>=)-Methoden <<< [ERR_NO = 16000 bis 16999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe Komponente "size" zweier Datenobjekte auf ">=" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ge_grain_size &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Referenzobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='ge_grain_size' 
    !
    ok = ( this1%size >= this2%size )
    !
    ! ... oder komplizierter
    !
  END FUNCTION ge_grain_size 
  !
  !! pr&uuml;fe Komponente "density" zweier Datenobjekte auf ">=" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ge_grain_density &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Referenzobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='ge_grain_density' 
    !
    ok = ( this1%density >= this2%density )
    !
    ! ... oder komplizierter
    !
  END FUNCTION ge_grain_density 
  !
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(<)-Methoden <<< [ERR_NO = 17000 bis 17999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe Komponente "size" zweier Datenobjekte auf "<" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION lt_grain_size &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Referenzobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='lt_grain_size' 
    !
    ok = ( this1%size < this2%size )
    !
  END FUNCTION lt_grain_size 
  !
  !! pr&uuml;fe Komponente "density" zweier Datenobjekte auf "<" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION lt_grain_density &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Referenzobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='lt_grain_density' 
    !
    ok = ( this1%density < this2%density )
    !
  END FUNCTION lt_grain_density 
  !
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(>=)-Methoden <<< [ERR_NO = 18000 bis 18999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe Komponente "size" zweier Datenobjekte auf "<=" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION le_grain_size &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Referenzobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='le_grain_size' 
    !
    ok = ( this1%size <= this2%size )
    !
  END FUNCTION le_grain_size 
  !
  !! pr&uuml;fe Komponente "density" zweier Datenobjekte auf "<=" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION le_grain_density &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! Referenzobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_grain) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='le_grain_density' 
    !
    ok = ( this1%density <= this2%density )
    !
  END FUNCTION le_grain_density 
  !
  !
  ! ----------------------------------------------------------------------
  ! >>> Spezielle typbezogene Methoden <<< [ERR_NO = 
  ! ----------------------------------------------------------------------
  !
!! Pr&uuml;ft &uuml;ber "name", ob alle gew&uuml;nschten Fraktionen auch in "grain" vorhanden sind
!! Funktion erzeugt keine Fehlermeldungen
  !
  FUNCTION all_grain_available_name_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! 
    TYPE (t_grain) , INTENT(IN) :: this1(:) ! 
    CHARACTER (LEN=40) , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='all_grain_available_name_1_0' 
    INTEGER :: j
    !
    ok = .false.
    do j = 1,SIZE(this1)
       if ( this1(j)%name == this2 ) ok = .true.
    enddo
    !
  END FUNCTION  all_grain_available_name_1_0
  !
  !
!! Pr&uuml;ft &uuml;ber "mid", ob alle gew&uuml;nschten Fraktionen auch in "grain" vorhanden sind
!! Funktion erzeugt keine Fehlermeldungen
  !
  FUNCTION all_grain_available_mid_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! 
    TYPE (t_grain) , INTENT(IN) :: this1(:) ! 
    INTEGER        , INTENT(IN) :: this2 ! 
    !
    ! Rueckgabewert
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='all_grain_available_mid_1_0' 
    INTEGER :: j
    !
    ok = .false.
    do j = 1,SIZE(this1)
       if  ( this1(j)%mid == this2 ) ok = .true.
    enddo
    !
  END FUNCTION  all_grain_available_mid_1_0
  !
  FUNCTION all_grain_available_name_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! 
    TYPE (t_grain) , INTENT(IN) :: this1(:) ! 
    CHARACTER (LEN=40) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='all_grain_available_name_1_1' 
    INTEGER :: i,j
    !
    ok = .false.
    do i = 1,SIZE(this2)
    do j = 1,SIZE(this1)
       if ( this1(j)%name == this2(i) ) ok(i) = .true.
    enddo
    enddo
    !
  END FUNCTION  all_grain_available_name_1_1
  !
  FUNCTION all_grain_available_mid_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !
    ! Formalparameter
    !! 
    TYPE (t_grain) , INTENT(IN) :: this1(:) ! 
    INTEGER        , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    !! Testergebnis
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='all_grain_available_mid_1_1' 
    INTEGER :: i,j
    !
    ok = .false.
    do i = 1,SIZE(this2)
    do j = 1,SIZE(this1)
       if  ( this1(j)%mid == this2(i) ) ok(i) = .true.
    enddo
    enddo
    !
  END FUNCTION  all_grain_available_mid_1_1
  !
!! Ermittelt die Anzahl der zur Verf&uuml;gung stehenden Fraktionen
!! Funktion erzeugt keine Fehlermeldungen
  !
  FUNCTION get_nof_grain_available_d &
       ( this)  &
         RESULT( val )
    !
    ! Formalparameter
    !! 
    TYPE (t_grain) , INTENT(IN) :: this(:) ! 
    !
    ! Rueckgabewert
    INTEGER :: val
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_nof_grain_available_d' 
    !
    val=SIZE(this)
    !
  END FUNCTION  get_nof_grain_available_d
  !
  !! R&uuml;ckgabewert ist ein Feld mit nach dem Durchmesser aufsteigend sortierten Fraktionen
  !! Funktion erzeugt keine Fehlermeldung
  SUBROUTINE get_grain_size_sorted_d &
       ( this1, this2, per_unsort, per_sort )   
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_grain), INTENT(IN)    :: this1(:) !
    TYPE (t_grain), INTENT(INOUT) :: this2(:) !
    REAL (KIND = Double) , INTENT(IN)   , OPTIONAL :: per_unsort(:) ! 
    REAL (KIND = Double) , INTENT(INOUT), OPTIONAL :: per_sort(:)   ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_grain_size_sorted_d'
    INTEGER  :: sort_idx(SIZE(this1))  ! 
    INTEGER  :: i ! 
    !
    CALL get_grain_size_sort_idx( this1, sort_idx )
    !
    DO i = 1, SIZE(this1)
       !
       this2(i) = this1(sort_idx(i))
       IF ( PRESENT(per_unsort) .AND. PRESENT(per_sort) ) THEN
          per_sort(i) = per_unsort(sort_idx(i))
       ENDIF
       !
    END DO
    !
  END SUBROUTINE get_grain_size_sorted_d
  !
  !! Gib Indexfeld fuer Sortierung von grain(:) (t_grain) nach aufsteigendem Korndurchmesser
  !! Funktion erzeugt keine Fehlermeldung
  SUBROUTINE get_grain_size_sort_idx_d &
       ( this, sort_idx )   
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_grain) , INTENT(IN)    :: this(:)     !
    INTEGER        , INTENT(INOUT) :: sort_idx(:) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_grain_size_sort_idx_d'
    LOGICAL  :: ok(SIZE(this)) ! 
    INTEGER  :: i              ! 
    !
    sort_idx = 0
    ok       = .true.
    !
    DO i = 1, SIZE(this)
       !
       sort_idx(i)     = minloc(this%size,1,ok)
       ok(sort_idx(i)) = .false.
       !
    END DO
    !
  END SUBROUTINE get_grain_size_sort_idx_d
  !
  !! R&uuml;ckgabewert ist ein Feld mit nach der Dichte aufsteigend sortierten Fraktionen
  !! Funktion erzeugt keine Fehlermeldung
  SUBROUTINE get_grain_density_sorted_d &
       ( this1,               &
         this2,               &
         per_unsort,            &
         per_sort )   
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_grain) , INTENT(IN)    :: this1(:) !
    TYPE (t_grain) , INTENT(INOUT) :: this2(:) !
    REAL (KIND = Double) , INTENT(IN)   , OPTIONAL  :: per_unsort(:) 
    REAL (KIND = Double) , INTENT(INOUT), OPTIONAL  :: per_sort(:) 
    LOGICAL                        :: ok(SIZE(this1))
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_grain_density_sorted_d' 
    INTEGER :: i
    !
    ok = .true.
    !
    do i = 1,SIZE(this1)
       this2(i) = this1(minloc(this1%density,1,ok))
       IF (PRESENT(per_unsort).AND.PRESENT(per_sort)) THEN
         per_sort(i) = per_unsort(minloc(this1%density,1,ok))
       ENDIF
       ok(minloc(this1%density,1,ok)) = .false.
    enddo
    !
  END SUBROUTINE get_grain_density_sorted_d
  !
!! Berechnet den mittleren Korndurchmesser dm
!! Funktion erzeugt keine Fehlermeldung
  !
  FUNCTION get_grain_mean_size_d &
       ( this, percent ) &
        RESULT (val)
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_grain) , INTENT(IN)    :: this(:)   !
    REAL (KIND = Double)           , INTENT(IN)    :: percent(:) !
    REAL (KIND = Double)                           :: val        !
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_grain_mean_size_d' 
    !
    val = 0.0_Double
    IF ( SUM(percent) > EPSILON(0._Double)) val = SUM(this%size * percent) / SUM(percent)
    !
  END FUNCTION get_grain_mean_size_d
  !
  !! Berechnet die mittlere Dichte
  !! Funktion erzeugt keine Fehlermeldung
  FUNCTION get_grain_mean_density_d &
       ( this, percent ) &
        RESULT (val)
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_grain)      , INTENT(IN)  :: this(:)    !
    REAL (KIND = Double), INTENT(IN)  :: percent(:) ! 
    REAL (KIND = Double)              :: val        ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_grain_mean_density_d' 
    !
    val = 0.0_Double
    IF ( SUM(percent) > EPSILON(0._Double)) val = SUM(this%density * percent) / SUM(percent)
    !
  END FUNCTION get_grain_mean_density_d
  !
  !! Berechnet den Korndurchmesser dxx mit dem Siebdurchgang XX
  !! Innerhalb einer Fraktion wird dann linear interpoliert
  !! Funktion erzeugt Fehlermeldung
  FUNCTION get_grain_dxx_d &
       ( this, percent, ixx, incr_size_idx ) &
        RESULT (val)
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_grain)      , INTENT(IN)  :: this(:)    !
    REAL (KIND = Double), INTENT(IN)  :: percent(:) ! 
    INTEGER , INTENT(IN)              :: ixx        ! 
    !! Sortierungsindex, aufsteigende Korngroesse
    INTEGER , INTENT(IN), OPTIONAL    :: incr_size_idx(:) ! 
    !! R&uuml;ckgabewert
    REAL (KIND = Double)              :: val        ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER  :: c_upname='get_grain_dxx_d' ! 
    CHARACTER (LEN=15)             :: rstring ! 
    INTEGER                        :: i , i_fraction       ! 
    INTEGER                        :: nof_local_present_fractions
    REAL (KIND = Double)           :: xx      ! 
    REAL (KIND = Double)           :: percent_sort(SIZE(this))   ! 
    TYPE (t_grain)                 :: grain_sort  (SIZE(this))   !
    !
    IF ( SUM(percent) > 1.001_DOUBLE ) THEN
       CALL setup_error_act( all_errors(:), 21010 , c_upname, c_modname)
       rstring = REPEAT(' ',LEN(rstring)); WRITE(rstring,'(G15.8)') MINVAL(percent(:))
       CALL setup_error_act( '<MinFrac>', rstring )
       rstring = REPEAT(' ',LEN(rstring)); WRITE(rstring,'(G15.8)') MAXVAL(percent(:))
       CALL setup_error_act( '<MaxFrac>', rstring )
       rstring = REPEAT(' ',LEN(rstring)); WRITE(rstring,'(G15.8)') SUM(percent(:))
       CALL setup_error_act( '<SumFrac>', rstring )
    ENDIF
    IF (ixx > 100 .OR. ixx < 0 ) THEN
       CALL setup_error_act( all_errors(:), 21011 , c_upname, c_modname)
    ENDIF   
    IF ( (SUM(percent) < real(ixx)*0.01_Double) .AND. (SUM(percent) >= EPSILON(0._Double) ) ) THEN
       CALL setup_error_act( all_errors(:), 21012 , c_upname, c_modname)
    ENDIF
    !
    IF ( no_error() ) THEN
       !
       IF (SUM(percent) <= EPSILON(0.0_Double) ) THEN
          val  = 0.0_Double
       ELSE
          xx   = real(ixx)*0.01_Double
          val  = 0.0_Double
          !
          IF ( PRESENT(incr_size_idx) ) THEN
             DO i = 1, SIZE(this)
                grain_sort(i)   = this   (incr_size_idx(i))
                percent_sort(i) = percent(incr_size_idx(i))
             END DO
          ELSE
             CALL get_grain_size_sorted ( this, grain_sort, per_unsort=percent, &
                  per_sort=percent_sort )
          ENDIF
          !
          ! Normierung, da Porenwasser enthalten sein kann
          !
          percent_sort = percent_sort / SUM(percent_sort)
          !
          nof_local_present_fractions = 0
          !
          DO i = 1, SIZE(this)
             IF (percent_sort(i) > EPSILON(0.0_Double) ) THEN
                nof_local_present_fractions = nof_local_present_fractions + 1
                i_fraction = i
             ENDIF
          END DO
          !
          i=1
          !
          DO WHILE( SUM(percent_sort(1:i)) < xx )
             i = i + 1
          END DO
          !
          IF ( nof_local_present_fractions == 1 ) THEN
             val = grain_sort(i_fraction)%size ! nicht interpolieren, da nur eine Fraktion vorhanden ist
          ELSEIF ( i == 1 ) THEN
             val = grain_sort(i)%size          ! nicht interpolieren, da dxx bereits im ersten Intervall liegt
          ELSEIF ( nof_local_present_fractions > 1) THEN
             val =  grain_sort(i-1)%size &     ! lineare Interpolation zwischen benachbarten Korngroessen
                  + (xx                     - SUM(percent_sort(1:(i-1)))) & 
                  / (SUM(percent_sort(1:i)) - SUM(percent_sort(1:(i-1)))) &
                  * (grain_sort(i)%size     - grain_sort(i-1)%size)
          ENDIF
          !
       ENDIF
       !
    ENDIF
    !
  END FUNCTION get_grain_dxx_d
  !
  !! Berechnet die Standardabweichung der Sedimentverteilung
  !! Funktion erzeugt keine Fehlermeldung
  FUNCTION get_grain_sorting_d &
       ( this , percent) &
        RESULT(val)
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_grain) , INTENT(IN)    :: this(:) !
    REAL (KIND = Double)           , INTENT(IN)    :: percent(:) !
    REAL (KIND = Double)                           :: val
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_grain_sorting_d' 
    REAL (KIND = Double)                           :: dm
    !
    dm = get_grain_mean_size(this,percent)
    !
    val = SUM(percent*(this%size-dm)**2._Double)
    !
  END FUNCTION get_grain_sorting_d
  !
!! Berechnet die Schiefe der Sedimentverteilung
!! Funktion erzeugt keine Fehlermeldung
  !
  FUNCTION get_grain_skewness_d &
       ( this , percent) &
        RESULT(val)
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_grain) , INTENT(IN)    :: this(:) !
    REAL (KIND = Double)           , INTENT(IN)    :: percent(:) !
    REAL (KIND = Double)                           :: val
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_grain_skewness_d' 
    REAL (KIND = Double)                           :: dm,d50,sigma_d
    !
    dm      = get_grain_mean_size(this,percent)
    d50     = get_grain_dxx(this,percent,50)
    sigma_d = get_grain_sorting(this,percent)
    !
    val = (dm - d50)/sigma_d
    !
  END FUNCTION get_grain_skewness_d
  !
!! Berechnet die Kurtosis der Sedimentverteilung
!! Funktion erzeugt keine Fehlermeldung
  !
  FUNCTION get_grain_kurtosis_d &
       ( this , percent) &
        RESULT(val)
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_grain) , INTENT(IN)    :: this(:) !
    REAL (KIND = Double)           , INTENT(IN)    :: percent(:) !
    REAL (KIND = Double)                           :: val
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_grain_kurtosis_d' 
    REAL (KIND = Double)                           :: d95,d5,sigma_d
    !
    d5      = get_grain_dxx(this,percent,5)
    d95     = get_grain_dxx(this,percent,95)
    sigma_d = get_grain_sorting(this,percent)
    !
    val = (0.5*(d95 - d5) - sigma_d) / sigma_d
    !
  END FUNCTION get_grain_kurtosis_d
  !
!! Berechnet die effektive kornbezogene Rauheit der Sedimentverteilung
!! Funktion erzeugt Fehlermeldung
  !
  FUNCTION get_grain_roughness_d &
       ( this , percent) &
        RESULT(val)
    !
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_grain) , INTENT(IN)    :: this(:) !
    REAL (KIND = Double)           , INTENT(IN)    :: percent(:) !
    REAL (KIND = Double)                           :: val
    !
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER  :: c_upname='get_grain_roughness_d' 
    REAL (KIND = Double)                           :: dm
    !
    dm      = get_grain_mean_size(this,percent)
    !
    val = 3.0*dm
    !
  END FUNCTION get_grain_roughness_d
  !
!! Selektiert &uuml;ber "mid" eine Auswahl aus einer Liste
!! Funktion erzeugt Fehlermeldung
  !
  FUNCTION get_grain_selection_mid &
       ( this1,  &
         this2 ) &
         RESULT( this3 )
    !
    ! Formalparameter
    !! 
    TYPE (t_grain) , INTENT(IN) :: this1(:) ! 
    INTEGER        , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    TYPE (t_grain)              :: this3(SIZE(this2)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_grain_selection_mid' 
    INTEGER :: i,j
    !
    CALL new_grain(this3)

    DO i = 1,SIZE(this2)
       IF (all_grain_available(this1,this2(i))) THEN
          DO j = 1,SIZE(this1)
             IF  ( this1(j)%mid == this2(i) ) this3(i) = this1(j)
          ENDDO
       ELSE
          CALL setup_error_act( all_errors(:), 21020 , c_upname, c_modname)
       ENDIF
    ENDDO
    !
  END FUNCTION  get_grain_selection_mid
  !
!! Selektiert &uuml;ber "name" eine Auswahl aus einer Liste
!! Funktion erzeugt Fehlermeldung
  !
  FUNCTION get_grain_selection_name &
       ( this1,  &
         this2 ) &
         RESULT( this3 )
    !
    ! Formalparameter
    !! 
    TYPE (t_grain)       , INTENT(IN) :: this1(:) ! 
    CHARACTER (LEN = 40) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    TYPE (t_grain)                    :: this3(SIZE(this2)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_grain_selection_name' 
    INTEGER :: i,j
    !
    CALL new_grain(this3)
    DO i = 1,SIZE(this2)
       IF (all_grain_available(this1,this2(i))) THEN
          DO j = 1,SIZE(this1)
             IF  ( this1(j)%name == this2(i) ) this3(i) = this1(j)
          ENDDO
       ELSE
          CALL setup_error_act( all_errors(:), 21030 , c_upname, c_modname)
       ENDIF
    ENDDO
    !
  END FUNCTION  get_grain_selection_name
  !
!! Liefert &uuml;ber "mid" eine Indexliste zur Auswahl aus einer gr&ouml;sseren Liste von vorhandenen Fraktionen
!! Funktion erzeugt Fehlermeldung
  !
  FUNCTION get_grain_index_list_mid &
       ( this1,  &
         this2 ) &
         RESULT( this3 )
    !
    ! Formalparameter
    !! 
    TYPE (t_grain) , INTENT(IN) :: this1(:) ! 
    INTEGER        , INTENT(IN) :: this2(:) ! 

    !
    ! Rueckgabewert
    INTEGER                     :: this3(SIZE(this2)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_grain_index_list_mid' 
    INTEGER :: i,j
    !
    this3 = 0
    !
    DO i = 1,SIZE(this2)
       IF (all_grain_available(this1,this2(i))) THEN
          DO j = 1,SIZE(this1)
             IF  ( this1(j)%mid == this2(i) ) this3(i) = j
          ENDDO
       ELSE
          CALL setup_error_act( all_errors(:), 21020 , c_upname, c_modname)
       ENDIF
    ENDDO
    !
  END FUNCTION  get_grain_index_list_mid
  !
!! Liefert &uuml;ber "name" eine Indexliste zur Auswahl aus einer gr&ouml;sseren Liste von vorhandenen Fraktionen
!! Funktion erzeugt Fehlermeldung
  !
  FUNCTION get_grain_index_list_name &
       ( this1,  &
         this2 ) &
         RESULT( this3 )
    !
    ! Formalparameter
    !! 
    TYPE (t_grain)       , INTENT(IN) :: this1(:) ! 
    CHARACTER (LEN = 40) , INTENT(IN) :: this2(:) ! 
    !
    ! Rueckgabewert
    INTEGER                     :: this3(SIZE(this2)) ! 
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_grain_index_list_name' 
    INTEGER :: i,j
    !
    this3 = 0
    !
    DO i = 1,SIZE(this2)
       IF (all_grain_available(this1,this2(i))) THEN
          DO j = 1,SIZE(this1)
             IF  ( this1(j)%name == this2(i) ) this3(i) = j
          ENDDO
       ELSE
          CALL setup_error_act( all_errors(:), 21030 , c_upname, c_modname)
       ENDIF
    ENDDO
    !
  END FUNCTION  get_grain_index_list_name
  !
  !! berechnet die Sinkgeschwindigkeit in Wasser nach der Stokeschen 
  !! Formel f&uuml;r eine Fraktion "grain" unter 
  !! Standard-Umweltbedingungen: <BR>
  !! a) Temperatur = 20 Grad Celsius,                        <BR>
  !! b) Schwerebeschleunigung = 9.81 m/s**2,                 <BR>
  !! c) Dichte des Wassers = 998.2 kg/m**3
  FUNCTION get_grain_stokes_velocity_0 &
       ( this )                        &
       RESULT( val )
    !! Sedimentfraktion
    TYPE (t_grain) , INTENT(IN) :: this
    !! R&uuml;ckgabewert: Sinkgeschwindigkeit in [m/s] <BR>
    !! a) der Wert der Sinkgeschwindigkeit ist positiv, falls die
    !!    Fraktion <EM>schwerer</EM> als die Fl&uuml;ssigkeit ist; <BR>
    !! b) der Wert der Sinkgeschwindigkeit ist negativ, falls die
    !!    Fraktion <EM>leichter</EM> als die Fl&uuml;ssigkeit ist
    REAL (KIND=Double)          :: val ! 
    ! Lokale Parameter
    !! konstante Schwerbeschleunigung
    REAL (KIND=Double) , PARAMETER :: c_g=9.81_Double       ! 
    !! konstante Temperatur
    REAL (KIND=Double) , PARAMETER :: c_t=20.0_Double       ! 
    !! konstante Dichte der umgebenden Fl&uuml;ssigkeit
    REAL (KIND=Double) , PARAMETER :: c_d=998.2_Double      ! 
    !
    val = get_grain_stokes_velocity_d ( this, c_g, c_t, c_d )
    !
  END FUNCTION get_grain_stokes_velocity_0
  !
  !! berechnet die Sinkgeschwindigkeit in Wasser nach der Stokeschen 
  !! Formel f&uuml;r mehrere Fraktionen "grain(:)" unter 
  !! Standard-Umweltbedingungen: <BR>
  !! a) Temperatur = 20 Grad Celsius,                        <BR>
  !! b) Schwerebeschleunigung = 9.81 m/s**2,                 <BR>
  !! c) Dichte des Wassers = 998.2 kg/m**3
  FUNCTION get_grain_stokes_velocity_1 &
       ( this )                        &
       RESULT( val )
    !! Sedimentfraktion
    TYPE (t_grain) , INTENT(IN) :: this(:)
    !! R&uuml;ckgabewert: Sinkgeschwindigkeit in [m/s] <BR>
    !! a) der Wert der Sinkgeschwindigkeit ist positiv, falls die
    !!    Fraktion <EM>schwerer</EM> als die Fl&uuml;ssigkeit ist; <BR>
    !! b) der Wert der Sinkgeschwindigkeit ist negativ, falls die
    !!    Fraktion <EM>leichter</EM> als die Fl&uuml;ssigkeit ist
    REAL (KIND=Double)          :: val(SIZE(this)) ! 
    ! Lokale Variablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       val(i) = get_grain_stokes_velocity_0( this(i) )
    END DO
    !
  END FUNCTION get_grain_stokes_velocity_1
  !
  !! berechnet die Sinkgeschwindigkeit in Wasser nach der 
  !! Dietrichschen Formel f&uuml;r eine Fraktion "grain" 
  !! unter Standard-Umweltbedingungen: <BR>
  !! a) Temperatur = 20 Grad Celsius,                        <BR>
  !! b) Schwerebeschleunigung = 9.81 m/s**2,                 <BR>
  !! c) Dichte des Wassers = 998.2 kg/m**3
  FUNCTION get_grain_dietrich_velocity_0 &
       ( this )                          &
       RESULT( val )
    !! Sedimentfraktion
    TYPE (t_grain) , INTENT(IN) :: this
    !! R&uuml;ckgabewert: Sinkgeschwindigkeit in [m/s] <BR>
    !! a) der Wert der Sinkgeschwindigkeit ist positiv, falls die
    !!    Fraktion <EM>schwerer</EM> als die Fl&uuml;ssigkeit ist; <BR>
    !! b) der Wert der Sinkgeschwindigkeit ist negativ, falls die
    !!    Fraktion <EM>leichter</EM> als die Fl&uuml;ssigkeit ist
    REAL (KIND=Double)          :: val ! 
    ! Lokale Parameter
    !! konstante Schwerbeschleunigung
    REAL (KIND=Double) , PARAMETER :: c_g=9.81_Double       ! 
    !! konstante Temperatur
    REAL (KIND=Double) , PARAMETER :: c_t=20.0_Double       ! 
    !! konstante Dichte der umgebenden Fl&uuml;ssigkeit
    REAL (KIND=Double) , PARAMETER :: c_d=998.2_Double      ! 
    !
    val = get_grain_dietrich_velocity_d ( this, c_g, c_t, c_d )
    !
  END FUNCTION get_grain_dietrich_velocity_0
  !
  !! berechnet die Sinkgeschwindigkeit in Wasser nach der 
  !! Dietrichschen Formel f&uuml;r mehrere Fraktionen "grain(:)" 
  !! unter Standard-Umweltbedingungen: <BR>
  !! a) Temperatur = 20 Grad Celsius,                        <BR>
  !! b) Schwerebeschleunigung = 9.81 m/s**2,                 <BR>
  !! c) Dichte des Wassers = 998.2 kg/m**3
  FUNCTION get_grain_dietrich_velocity_1 &
       ( this )                        &
       RESULT( val )
    !! Sedimentfraktion
    TYPE (t_grain) , INTENT(IN) :: this(:)
    !! R&uuml;ckgabewert: Sinkgeschwindigkeit in [m/s] <BR>
    !! a) der Wert der Sinkgeschwindigkeit ist positiv, falls die
    !!    Fraktion <EM>schwerer</EM> als die Fl&uuml;ssigkeit ist; <BR>
    !! b) der Wert der Sinkgeschwindigkeit ist negativ, falls die
    !!    Fraktion <EM>leichter</EM> als die Fl&uuml;ssigkeit ist
    REAL (KIND=Double)          :: val(SIZE(this)) ! 
    ! Lokale Variablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       val(i) = get_grain_dietrich_velocity_0( this(i) )
    END DO
    !
  END FUNCTION get_grain_dietrich_velocity_1
  !
  !! berechnet die Sinkgeschwindigkeit nach der Stokeschen Formel f&uuml;r
  !! eine Fraktion "grain" bei beliebiger Temperatur, Schwerebeschleunigung
  !! und Dichte des umgebenden Wassers.
  FUNCTION get_grain_stokes_velocity_d &
       ( this, g, t, d )               &
       RESULT( val )
    !! Sedimentfraktion
    TYPE (t_grain)     , INTENT(IN) :: this
    !! Schwerebeschleunigung in [m/s**2]
    REAL (KIND=Double) , INTENT(IN) :: g ! 
    !! Temperatur in [Grad Celsius]
    REAL (KIND=Double) , INTENT(IN) :: t ! 
    !! Dichte in [kg/m**3]
    REAL (KIND=Double) , INTENT(IN) :: d ! 
    !! R&uuml;ckgabewert: Sinkgeschwindigkeit in [m/s] <BR>
    !! a) der Wert der Sinkgeschwindigkeit ist positiv, falls die
    !!    Fraktion <EM>schwerer</EM> als die Fl&uuml;ssigkeit ist; <BR>
    !! b) der Wert der Sinkgeschwindigkeit ist negativ, falls die
    !!    Fraktion <EM>leichter</EM> als die Fl&uuml;ssigkeit ist
    REAL (KIND=Double)              :: val ! 
    ! Lokale Parameter und Variablen
    !! allgemeine Konstante der Stokeschen Formel
    REAL (KIND=Double) , PARAMETER :: c_c=18.0_Double        ! 
    !! Viskosit&auml;t bei 20 Grad Celsius
    REAL (KIND=Double) , PARAMETER :: c_20=1.0034E-06_Double ! 
    !! Abh&auml;ngigkeit der Viskosit&auml;t von der Temperatur
    REAL (KIND=Double) , PARAMETER :: c_dt=2.84E-02_Double   ! 
    !! Standardtemperatur
    REAL (KIND=Double) , PARAMETER :: c_t=20.0_Double        ! 
    !! kinematische Viskosit&auml;t des Wassers
    REAL (KIND=DOUBLE)             :: rnu             ! 
    !
    IF ( d >= EPSILON(d) ) THEN
       rnu = c_20*EXP(c_dt*(t-c_t))
       val = g*(this%density-d)*this%size*this%size/(c_c*rnu*d)
    ELSE
       val = 0.0_Double
    END IF
    !
  END FUNCTION get_grain_stokes_velocity_d
  !
  !! berechnet die Sinkgeschwindigkeit nach der Dietrichschen Formel f&uuml;r
  !! eine Fraktion "grain" bei beliebiger Temperatur, Schwerebeschleunigung
  !! und Dichte des umgebenden Wassers.
  FUNCTION get_grain_dietrich_velocity_d &
       ( this, g, t, d )                 &
       RESULT( val )
    !! Sedimentfraktion
    TYPE (t_grain)     , INTENT(IN) :: this
    !! Schwerebeschleunigung in [m/s**2]
    REAL (KIND=Double) , INTENT(IN) :: g ! 
    !! Temperatur in [Grad Celsius]
    REAL (KIND=Double) , INTENT(IN) :: t ! 
    !! Dichte in [kg/m**3]
    REAL (KIND=Double) , INTENT(IN) :: d ! 
    !! R&uuml;ckgabewert: Sinkgeschwindigkeit in [m/s] <BR>
    !! a) der Wert der Sinkgeschwindigkeit ist positiv, falls die
    !!    Fraktion <EM>schwerer</EM> als die Fl&uuml;ssigkeit ist; <BR>
    !! b) der Wert der Sinkgeschwindigkeit ist negativ, falls die
    !!    Fraktion <EM>leichter</EM> als die Fl&uuml;ssigkeit ist
    REAL (KIND=Double)              :: val ! 
    ! Lokale Parameter und Variablen
    !! allgemeine Konstante der Stokeschen Formel
    REAL (KIND=Double) , PARAMETER :: c_c=18.0_Double        ! 
    !! Viskosit&auml;t bei 20 Grad Celsius
    REAL (KIND=Double) , PARAMETER :: c_20=1.0034E-06_Double ! 
    !! Abh&auml;ngigkeit der Viskosit&auml;t von der Temperatur
    REAL (KIND=Double) , PARAMETER :: c_dt=2.84E-02_Double   ! 
    !! Standardtemperatur
    REAL (KIND=Double) , PARAMETER :: c_t=20.0_Double        ! 
    !! Coreyscher Formfaktor
    REAL (KIND=Double) , PARAMETER :: c_csf=1.0_Double       ! Kugel [ siehe unten ]
    !! Powers Rundheitsbeiwert
    REAL (KIND=Double) , PARAMETER :: c_power=6.0_Double     ! Kugel [ siehe unten ]
    !! kinematische Viskosit&auml;t des Wassers
    REAL (KIND=DOUBLE)             :: rnu                    ! 
    !! dimensionslose Sinkgeschwindigkeit
    REAL (KIND=DOUBLE)             :: wstar_b                  ! 
    !! dimensionsloser Korndurchmesser
    REAL (KIND=DOUBLE)             :: dstar                  ! 
    !! Parameter der Berechnungsformel nach Dietrich
    REAL (KIND=DOUBLE)             :: r1, r2, r3, rr         ! 
    !
    IF ( d >= EPSILON(d) ) THEN
       rnu   = c_20*EXP(c_dt*(t-c_t))
       dstar = this%size*(g*(this%density-d)/(d*rnu*rnu))**0.333333333333_Double ! 
       rr    = LOG10(dstar)
       r1    = -3.76715_Double               + &
                5.78832_Double * rr          - &
                0.88335_Double * rr*rr       - &
                0.15525_Double * rr*rr*rr    + &
                0.04536_Double * rr*rr*rr*rr
       ! ... die folgenden beiden Zahlen gelten nur fuer kugelfoermige 
       r2    = 1.0_Double
       r3    = 1.0_Double
       ! ... die nachfolgenden Formulierungen gelten fuer c_csf /= 1.0 und
       !     c_power /= 6.0 [ falls diese Parameter geandert werden sollten ]
       !gl       r2    = 1.0_Double - (1.0_Double-c_csf)/0.85_Double
       !gl       r3    = ( 0.65_Double - c_csf/2.83_Double *  &
       !gl          TANH( 3.0_Double*rr - 4.6_Double ) &
       !gl        )**(1.0_Double+(3.5_Double-c_power)/2.5_Double)
       wstar_b = r2*r3*10.0**r1
       val   = (((this%density-d)/d)*g*rnu*wstar_b)**0.333333333333_Double
    ELSE
       val = 0.0_Double
    END IF
    !
  END FUNCTION get_grain_dietrich_velocity_d
  !
END MODULE b_grain
! TailOfBaseModule --------------------------------------------------------
