! -------------------------------------------------------------------------
! HeadOfBaseModule --------------------------------------------------------
!
!! <H2>Type und Methoden zur Verwenung in Zusammenhang mit <EM>ragged arrays</EM> f&uuml;r <EM>ExchangeItems</EM></h2>
!! @author G. Lang
!! @version 2.1 vom 07/21/05, Quellcode: mod_b_omi_exch_r.f90
!! <HR>
!! <one line to give the program's name and an idea of what it does> <BR>
!! <HR>
!  Copyright-Hinweis
!                                                                    
!  Copyright (C) 2005 <A HREF="http://www.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A>
!                                                                    
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2005-03-07 : G. Lang : Erstversion (unvollstaendig)
!  01.02 : 2005-03-10 : G. Lang : Erweiterungen fuer Suche nach quant_id, elemset_id
!  01.03 : 2005-03-11 : G. Lang : OPERATORen entfernt, auf Funktionen umgestellt
!  01.04 : 2005-03-17 : G. Lang : zus. Komponente "gdx" eingerichtet
!  01.05 : 2005-05-10 : G. Lang : Korrektur an Suchfunktion
!  02.01 : 2005-07-21 : G. Lang : Anpassungen fuer korrigiertes ElementSet-Verstaendnis (GEI)
!
!!                                                                  <BR>
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!!
!! Typ und Methoden stehen in enger Verbindung zu dem Basis-Modul
!! <EM>b_omi_exch</EM> mit Typ <TT>t_omi_exch</TT>. Dieses Modul
!! definiert den Typ (und passende Methoden) <TT>t_omi_exch_r</TT>, 
!! der f&uuml;r das Arbeiten mit <EM>ragged arrays</EM>, also mit aus
!! unterschiedlich langen Feldkomponenten aufgebauten Arrays geeignet
!! ist.
!!
!! <OL>
!!    <LI> Initialisierung und De-Initialisierung von vektoriellen 
!!         Variablen des Typs "t_omi_exch_r";
!!    <LI> Setzen der Komponenten in Variablen des Typs "t_omi_exch_r";
!!    <LI> Holen der Komponenten aus Variablen des Typs "t_omi_exch_r";
!!    <LI> Drucken des Inhalts der Komponenten von Variablen des Typs "t_omi_exch_r";
!!    <LI> Pr&uuml;fen des Inhalts von Variablen des Typs "t_omi_exch_r";
!!    <LI> Vergleichen des Inhalts verschiedener Variablen des Typs "t_omi_exch_r".
!! </OL>
!!
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt den selbst definierten Datentyp t_omi_exch_r <BR>
!! zur Verf&uuml;gung. Dieser besteht aus den folgenden Komponenten:      <BR>
!! <OL>
!!     <LI> odx     : zu der Komponente <EM>exch(:)</EM> geh&ouml;render Objekt-Index
!!     <LI> gdx     : zu der Komponente <EM>exch(:)</EM> geh&ouml;render Gitter-Objekt-Index, falls
!!                    Daten und Gitter in unterschiedlichen Dateien vorgehalten werden
!!     <LI> exch(:) : Komponente zur Aufnahme von Austauschgr&ouml;&szlig;en (<EM>ExchangeItems</EM>)
!! </OL>
!!                                                                  <BR>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Verwenden des Moduls</H3>
!!                                                                  <BR>
!! Die Leistungen des Moduls k&ouml;nnen wie folgt in Anspruch genommen werden: <BR>
!! <OL>
!!    <LI> Einbinden des Moduls mittels USE-Anweisung in der rufenden Programmeinheit;
!!    <LI> Initialisieren des Moduls b_omi_exch_r mit INIT-Methode;
!!    <LI> ggf. Verwenden verschiedener SETUP-Methoden;
!!    <LI> Verwenden beliebiger Modul-Methoden (nicht INIT und CLEAR);
!!    <LI> De-Initialisieren des Moduls b_omi_exch_r mit CLEAR-Methode.
!! </OL>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Fehlersituationen des Moduls</H3>
!!                                                                    <BR>
!! Hinweis: einige Methoden dieses Moduls erzeugen Fehlermeldungen,   
!!          andere nicht. 
!!          Routinen, die Fehlermeldungen generieren m&uuml;ssen pr&uuml;fen,
!!          ob das Modul korrekt initialisert wurde (ok_initialised).  <BR>
!!          F&uuml;r eine vollst&auml;ndige &Uuml;bersicht verwende man
!!          die Methode PRINT_OMI_EXCH_R_ALL_ERRORS.
!
MODULE b_omi_exch_r
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] BASIS-Modul mit globalen Konstantwerten
  !
  USE b_constants, ONLY : &
       ! Parameter
       Double
  !
  ! [A.2] BASIS-Modul mit Fehler-Typ und -Routinen 
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
  ! [A.3] Basis-Modul mit Typ+Methoden "Austauschgroesse" (OpenMI)
  USE b_omi_exch, ONLY :             &
       !     Typdefinitionen
       t_omi_exch,                   &
       !   Parameter 
       !   Variablen mit INTENT(IN)
       !   Variablen mit INTENT(INOUT)
       !   Variablen mit INTENT(OUT)
       !   Routinen / Interfaces
       init_omi_exch,                &
       clear_omi_exch,               &
       setup_omi_exch_prn_lun,       &
       setup_omi_exch_trc_lun,       &
       setup_omi_exch_language,      &
       ok_omi_exch,                  &
       print_omi_exch,               &
       get_omi_exch_quant_ref,       &
       get_omi_exch_ele_ref,         &
       get_omi_exch_dope_ref,        &
       get_omi_exch_importing_count, &
       get_omi_exch_exporting_count, &
       get_omi_exch_importing_idx,   &
       get_omi_exch_exporting_idx,   &
       eq_omi_exch,                  &
       ne_omi_exch
  !
  ! [A.6] Basis-Modul mit Typ+Methoden "Datenoperationen" (OpenMI)
  USE b_omi_dope, ONLY :       &
       !   Typdefinitionen
       t_omi_dope,             &
       !   Konstante
       c_undef_omi_dope_int,   &
       c_undef_omi_dope_char,  &
       !   Routinen / Interfaces
       get_omi_dope_idx,       &
       get_omi_dope_args_ref
  !
  ! [A.7] Basis-Modul mit Typ+Methoden "Argumente" (OpenMI)
  USE b_omi_arg, ONLY :       &
       !   Typdefinitionen
       t_omi_arg,             &
       !   Konstante
       c_len_omi_arg_value,   &
       !   Routinen / Interfaces
       get_omi_arg_idx,       &
       get_omi_arg_value
  !
  ! [A.8] Basis-Modul mit Typ+Methoden "ElementSet" (OpenMI)
  USE b_omi_ele, ONLY :       &
       !   Typdefinitionen
       t_omi_ele,             &
       !   Konstante
       c_len_omi_ele_id,      &
       !   Routinen / Interfaces
       get_omi_ele_id
  !
  ! [A.9] Basis-Modul mit Typ+Methoden "ElementSet" (OpenMI)
  USE b_omi_quant, ONLY :     &
       !   Typdefinitionen
       t_omi_quant,           &
       !   Konstante
       c_len_omi_quant_id,    &
       !   Routinen / Interfaces
       get_omi_quant_id
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
  !! odx     : zu der Komponente <EM>exch(:)</EM> geh&ouml;render Objekt-Index <BR>
  !! gdx     : zu der Komponente <EM>exch(:)</EM> geh&ouml;render Gitter-Objekt-Index, falls
  !!           Daten und Gitter in unterschiedlichen Dateien vorgehalten werden
  !! exch(:) : Komponente zur Aufnahme von Austauschgr&ouml;&szlig;en (<EM>ExchangeItems</EM>)
  TYPE , PUBLIC :: t_omi_exch_r
     PRIVATE
     INTEGER                     :: odx     ! 
     INTEGER                     :: gdx     ! 
     TYPE (t_omi_exch) , POINTER :: exch(:) ! 
  END TYPE t_omi_exch_r
  !
  ! [C.2] Konstantwerte (Parameter) [moeglichst nicht verwenden]
  !
  !! Undefined-Wert f&uuml;r INTEGER-Komponenten
  INTEGER , PUBLIC , PARAMETER :: c_undef_omi_exch_r_int=-999              ! 
  !
  ! [C.3] Variablen [moeglichst nicht verwenden]
  !
  ! [C.4] Schnittstellen
  !
  ! [C.4.1] erforderliche oeffentliche Schnittstellen
  !
  !! Allokieren/Initialisieren der statischen Datenobjekte des Moduls; <BR>
  !! Initialisieren der statischen Modul-Daten mit Default-Werten.
  INTERFACE init_omi_exch_r
     MODULE PROCEDURE init_omi_exch_r_d ! 
  END INTERFACE
  !! De-Allokieren/De-Initialisieren der statischen Datenobjekte des Moduls; <BR>
  !! Re-Initialisieren einiger statischer Daten mit Default-Werten.
  INTERFACE clear_omi_exch_r
     MODULE PROCEDURE clear_omi_exch_r_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r PRINT-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>PRN_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>PRN_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>PRN_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_omi_exch_r_prn_lun
     MODULE PROCEDURE setup_omi_exch_r_prn_lun_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r TRACE-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>TRC_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>TRC_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>TRC_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_omi_exch_r_trc_lun
     MODULE PROCEDURE setup_omi_exch_r_trc_lun_d ! 
  END INTERFACE
  !! Index f&uuml;r Spracheinstellung setzen <BR>
  !! 1 = Deutsch (Default) <BR>
  !! 2 = Englisch         
  INTERFACE setup_omi_exch_r_language
     MODULE PROCEDURE setup_omi_exch_r_language_d ! 
  END INTERFACE
  !
  !! Erzeugen von Datenobjekten "t_omi_exch_r"; NULLIFY f&uuml;r 
  !! dynamische Komponenten-Felder und Initialisieren mit Default-Werten. <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE new_omi_exch_r
     MODULE PROCEDURE new_omi_exch_r_0  !
     MODULE PROCEDURE new_omi_exch_r_1  !
  END INTERFACE
  !! Vernichten von Datenobjekten "t_omi_exch_r"; ggf. De-Allokieren von 
  !! Memory; teilweise Re-Initialisieren mit Default-Werten. <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE kill_omi_exch_r
     MODULE PROCEDURE kill_omi_exch_r_0 !
     MODULE PROCEDURE kill_omi_exch_r_1 !
  END INTERFACE
  !! Pr&uuml;fen von Datenobjekten "t_omi_exch_r" auf G&uuml;ltigkeit <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE ok_omi_exch_r
     MODULE PROCEDURE ok_omi_exch_r_0 !
     MODULE PROCEDURE ok_omi_exch_r_1 !
  END INTERFACE
  !! Drucken von Datenobjekten "t_omi_exch_r"; Alle Komponenten des Typs 
  !! "t_omi_exch_r" auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor) <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_exch_r
     MODULE PROCEDURE print_omi_exch_r_0 !
     MODULE PROCEDURE print_omi_exch_r_1 !
  END INTERFACE
  !! Drucken aller in diesem Modul abgelegten statischen Daten; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_exch_r_static
     MODULE PROCEDURE print_omi_exch_r_static_d ! 
  END INTERFACE
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Moduls; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_exch_r_all_errors
     MODULE PROCEDURE print_omi_exch_r_all_errors_d ! 
  END INTERFACE
  !
  !! Setze Komponente "odx" in "t_omi_exch_r" auf Benutzerwert <BR>
  !! a) f&uuml;r ein Objekt <BR>
  !! b) f&uuml;r viele Objekte denselben Wert <BR>
  !! c) f&uuml;r viele Objekte verschiedene Werte <BR>
  !! die Daten werden auf die interne Komponente kopiert
  INTERFACE set_omi_exch_r_odx
     MODULE PROCEDURE set_omi_exch_r_odx_0_0 ! 
     MODULE PROCEDURE set_omi_exch_r_odx_1_0 ! 
     MODULE PROCEDURE set_omi_exch_r_odx_1_1 ! 
  END INTERFACE
  !! Setze Komponente "gdx" in "t_omi_exch_r" auf Benutzerwert <BR>
  !! a) f&uuml;r ein Objekt <BR>
  !! b) f&uuml;r viele Objekte denselben Wert <BR>
  !! c) f&uuml;r viele Objekte verschiedene Werte <BR>
  !! die Daten werden auf die interne Komponente kopiert
  INTERFACE set_omi_exch_r_gdx
     MODULE PROCEDURE set_omi_exch_r_gdx_0_0 ! 
     MODULE PROCEDURE set_omi_exch_r_gdx_1_0 ! 
     MODULE PROCEDURE set_omi_exch_r_gdx_1_1 ! 
  END INTERFACE
  !! Setze Komponente "exch(:)" in "t_omi_exch_r" auf Benutzerwert <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! es wird ein Zeiger auf die externen Daten eingerichtet
  INTERFACE set_omi_exch_r_exch_ref
     MODULE PROCEDURE set_omi_exch_r_exch_ref_0_1 ! 
  END INTERFACE
  !
  !! Index f&uuml;r Spracheinstellung ermitteln <BR>
  !! a) Deutsch (Default) <BR>
  !! b) Englisch         
  INTERFACE get_omi_exch_r_language
     MODULE PROCEDURE get_omi_exch_r_language_d ! 
  END INTERFACE
  !
  !! Hole Komponente "odx" aus "t_omi_exch_r" <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor) <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_exch_r_odx
     MODULE PROCEDURE get_omi_exch_r_odx_0_0 ! 
     MODULE PROCEDURE get_omi_exch_r_odx_1_0 ! 
  END INTERFACE
  !! Hole Komponente "gdx" aus "t_omi_exch_r" <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor) <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_exch_r_gdx
     MODULE PROCEDURE get_omi_exch_r_gdx_0_0 ! 
     MODULE PROCEDURE get_omi_exch_r_gdx_1_0 ! 
  END INTERFACE
  !! Hole Komponente "exch(:)" aus "t_omi_exch_r" <BR> 
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_exch_r_exch_ref
     MODULE PROCEDURE get_omi_exch_r_exch_ref_0_1 ! 
  END INTERFACE
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  !! ermittle die Anzahl der <EM>importierbaren</EM> Austauschgr&ouml;&szlig;en in 
  !! "ragged arrays" des Typs "t_omi_exch_r" <BR>
  !! a) f&uuml;r viele Objekte (Vektor)
  INTERFACE get_omi_exch_r_importing_count
     MODULE PROCEDURE get_omi_exch_r_importing_count1
  END INTERFACE
  !! ermittle die Anzahl der <EM>exportierbaren</EM> Austauschgr&ouml;&szlig;en in 
  !! "ragged arrays" des Typs "t_omi_exch_r" <BR>
  !! a) f&uuml;r viele Objekte (Vektor)
  INTERFACE get_omi_exch_r_exporting_count
     MODULE PROCEDURE get_omi_exch_r_exporting_count1
  END INTERFACE
  !! ermittle die Anzahl der unterschiedlichen <EM>Quantities</EM> in 
  !! "ragged arrays" des Typs "t_omi_exch_r" <BR>
  !! a) f&uuml;r viele Objekte (Vektor)
  INTERFACE get_omi_exch_r_quant_count
     MODULE PROCEDURE get_omi_exch_r_quant_count_1
  END INTERFACE
  !! ermittle die Anzahl der unterschiedlichen <EM>ElementSets</EM> in 
  !! "ragged arrays" des Typs "t_omi_exch_r" <BR>
  !! a) f&uuml;r viele Objekte (Vektor)
  INTERFACE get_omi_exch_r_elemset_count
     MODULE PROCEDURE get_omi_exch_r_elemset_count_1
  END INTERFACE
  !
  !! ermittle den Argumentwert einer bestimmten Datenoperation und eines
  !! bestimmten Keys <BR>
  !! a) f&uuml;r viele Objekte (Vektor)
  INTERFACE get_omi_exch_r_dope_arg_value
     MODULE PROCEDURE get_omi_exch_r_dope_arg_value_1
  END INTERFACE
  !! ermittle die Posistionsindices in einem "ragged array" an der eine 
  !! bestimmte <EM>importierende</EM> Austauschgr&ouml;&szlig;e abgelegt ist <BR>
  !! a) f&uuml;r viele Objekte (Vektor)
  INTERFACE get_omi_exch_r_inp_ijx
     MODULE PROCEDURE get_omi_exch_r_inp_ijx_1
  END INTERFACE
  !! ermittle die Posistionsindices in einem "ragged array" an der eine 
  !! bestimmte <EM>exportierende</EM> Austauschgr&ouml;&szlig;e abgelegt ist <BR>
  !! a) f&uuml;r viele Objekte (Vektor)
  INTERFACE get_omi_exch_r_out_ijx
     MODULE PROCEDURE get_omi_exch_r_out_ijx_1
  END INTERFACE
  !! ermittle die Posistionsindices in einem "ragged array" an der eine 
  !! bestimmte <EM>Quantity</EM> abgelegt ist <BR>
  !! a) f&uuml;r viele Objekte und eine lfd. Nummer der <EM>Quantity</EM> <BR>
  !! b) f&uuml;r viele Objekte und eine Id der <EM>Quantity</EM> <BR>
  INTERFACE get_omi_exch_r_quant_ijx
     MODULE PROCEDURE get_omi_exch_r_quant_ijx_1
     MODULE PROCEDURE get_omi_exch_r_quant_id_ijx_1
  END INTERFACE
  !! ermittle die Posistionsindices in einem "ragged array" an der ein
  !! bestimmtes <EM>ElementSet</EM> abgelegt ist <BR>
  !! a) f&uuml;r viele Objekte und eine lfd. Nummer des <EM>ElementSet</EM> <BR>
  !! b) f&uuml;r viele Objekte und eine Id des <EM>ElementSet</EM> <BR>
  INTERFACE get_omi_exch_r_elemset_ijx
     MODULE PROCEDURE get_omi_exch_r_elemset_ijx_1
     MODULE PROCEDURE get_omi_exch_r_elemset_id_ijx_1
  END INTERFACE
  !! ermittle die Positionsindices in einem "ragged array" an der ein
  !! bestimmtes <EM>ElementSet</EM> und eine bestimmte <EM>QuantityId</EM>
  !! abgelegt sind <BR>
  !! a) f&uuml;r viele Objekte und ein <EM>ElementSet</EM> sowie eine <EM>Quantity</EM>
  INTERFACE get_omi_exch_r_ijx
     MODULE PROCEDURE get_omi_exch_r_ijx_1
  END INTERFACE
  !
  !! Pr&uuml;fen zweier Datenobjekte "t_omi_exch_r" auf Gleichheit (Funktion)<BR>
  !! a) Skalar1 == Skalar2 <BR>
  !! b) Skalar1 == Vektor2 <BR>
  !! c) Vektor1 == Skalar2 <BR>
  !! d) Vektor1 == Vektor2
  INTERFACE eq_omi_exch_r
     MODULE PROCEDURE eq_omi_exch_r_0_0  ! 
     MODULE PROCEDURE eq_omi_exch_r_0_1  ! 
     MODULE PROCEDURE eq_omi_exch_r_1_0  ! 
     MODULE PROCEDURE eq_omi_exch_r_1_1  ! 
  END INTERFACE
  !! Pr&uuml;fen zweier Datenobjekte "t_omi_exch_r" auf Ungleichheit (Funktion)<BR>
  !! a) Skalar1 /= Skalar2 <BR>
  !! b) Skalar1 /= Vektor2 <BR>
  !! c) Vektor1 /= Skalar2 <BR>
  !! d) Vektor1 /= Vektor2
  INTERFACE ne_omi_exch_r
     MODULE PROCEDURE ne_omi_exch_r_0_0  !
     MODULE PROCEDURE ne_omi_exch_r_0_1  !
     MODULE PROCEDURE ne_omi_exch_r_1_0  !
     MODULE PROCEDURE ne_omi_exch_r_1_1  !
  END INTERFACE
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
  ! [C.7] Liste der oeffentlichen Methoden
  !
  ! [C.7.1] unbedingt erforderliche oeffentliche Methoden
  !
  PUBLIC :: init_omi_exch_r
  PUBLIC :: clear_omi_exch_r
  PUBLIC :: setup_omi_exch_r_prn_lun
  PUBLIC :: setup_omi_exch_r_trc_lun
  PUBLIC :: new_omi_exch_r
  PUBLIC :: kill_omi_exch_r
  PUBLIC :: ok_omi_exch_r
  PUBLIC :: print_omi_exch_r
  PUBLIC :: print_omi_exch_r_static
  PUBLIC :: print_omi_exch_r_all_errors
  PUBLIC :: set_omi_exch_r_odx
  PUBLIC :: set_omi_exch_r_gdx
  PUBLIC :: set_omi_exch_r_exch_ref
  PUBLIC :: get_omi_exch_r_odx
  PUBLIC :: get_omi_exch_r_gdx
  PUBLIC :: get_omi_exch_r_exch_ref
  PUBLIC :: eq_omi_exch_r
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: setup_omi_exch_r_language
  PUBLIC :: get_omi_exch_r_language
  PUBLIC :: get_omi_exch_r_importing_count
  PUBLIC :: get_omi_exch_r_exporting_count
  PUBLIC :: get_omi_exch_r_quant_count
  PUBLIC :: get_omi_exch_r_elemset_count
  PUBLIC :: get_omi_exch_r_dope_arg_value
  PUBLIC :: get_omi_exch_r_inp_ijx
  PUBLIC :: get_omi_exch_r_out_ijx
  PUBLIC :: get_omi_exch_r_quant_ijx
  PUBLIC :: get_omi_exch_r_elemset_ijx
  PUBLIC :: get_omi_exch_r_ijx
  PUBLIC :: ne_omi_exch_r
  !
  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] lokale Typdefinitionen
  !
  TYPE , PRIVATE :: t_string_r
     PRIVATE
     LOGICAL                          , POINTER :: l(:) ! 
     CHARACTER (LEN=c_len_omi_ele_id) , POINTER :: s(:) ! 
  END TYPE t_string_r
  !
  ! [D.2] Konstantwerte (Parameter)
  !
  !! Name des Moduls
  CHARACTER (LEN=12), PARAMETER :: c_modname      = 'b_omi_exch_r' ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.          ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1               ! 
  !! Anzahl der Datenkomponenten des Typs t_omi_exch_r
  INTEGER           , PARAMETER :: c_nofcomp      = 3                ! ggf. modifizieren
  !! Anzahl einstellbarer Sprachen
  INTEGER           , PARAMETER :: c_max_language = 2                ! [Deutsch,Englisch]
  !! Default-Language
  INTEGER           , PARAMETER :: c_def_language = 1                ! [Deutsch]
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
  !! aktuelle Spracheinstellung ( 1 = Deutsch, 2 = Englisch )
  INTEGER                , SAVE :: language=c_def_language ! 
  !
  !! Memo-Variablen zum Beschleunigen des Suchens
  INTEGER                                , SAVE :: memo_comp_idx=-1         ! 
  TYPE (t_string_r) , ALLOCATABLE, TARGET, SAVE :: memo_elemset_string_r(:) ! 
  TYPE (t_string_r) , ALLOCATABLE, TARGET, SAVE :: memo_quant_string_r(:)   ! 
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
  SUBROUTINE init_omi_exch_r_d ( )
    !
    USE b_error, ONLY : DEBUG_b
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER :: c_upname='init_omi_exch_r_d' 
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_b > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "b_omi_exch_r" version 2.1 of 07/21/05                 '
          WRITE(*,*) ' Copyright (C) 2005 Bundesanstalt fuer Wasserbau   '
          WRITE(*,*)
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       IF ( no_error( ) ) CALL init_omi_exch ( )
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_omi_exch_r_all_errors ( ) 
       ! [1.5] Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.6] endgueltiges Setzen des Initialisierungs-Indikators
       initialised = MERGE( .true., .false., no_error( ) )
       ! [1.7] Memo-Variablen
       memo_comp_idx = c_undef_omi_exch_r_int 
    END IF
    ! 2.0 Initialisierungszaehler heraufsetzen
    n_init = n_init + 1
    !
  END SUBROUTINE init_omi_exch_r_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_omi_exch_r_d ( )
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='clear_omi_exch_r_d' ! 
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] Memo-Variablen
       memo_comp_idx = c_undef_omi_exch_r_int 
       IF ( ALLOCATED( memo_elemset_string_r   ) ) THEN
          CALL dealloc_string_r ( memo_elemset_string_r )
          DEALLOCATE ( memo_elemset_string_r )
       END IF
       IF ( ALLOCATED( memo_quant_string_r   ) ) THEN
          CALL dealloc_string_r ( memo_quant_string_r )
          DEALLOCATE ( memo_quant_string_r )
       END IF
       ! [1.2] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_omi_exch_r_all_errors ( ) 
       ! [1.3] De-Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.4] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.5] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.5.1] ggf. weitere Module de-initialisieren
       IF ( no_error( ) ) CALL clear_omi_exch ( )
       ! [1.5.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
    !
  END SUBROUTINE clear_omi_exch_r_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_omi_exch_r_prn_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER :: c_upname='setup_omi_exch_r_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_omi_exch_prn_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_omi_exch_r_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_omi_exch_r_trc_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER :: c_upname='setup_omi_exch_r_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_omi_exch_trc_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_omi_exch_r_trc_lun_d
  !
  !! Setzen des Index f&uuml;r die Spracheinstellung <BR>
  !! 1 = Deutsch  <BR>
  !! 2 = Englisch 
  SUBROUTINE setup_omi_exch_r_language_d ( var )
    !! Index f&uuml;r Spracheinstellung (1 = Deutsch, 2 = Englisch )
    INTEGER , INTENT(IN) :: var ! 
    !! Name der Subroutine
    CHARACTER (LEN=27), PARAMETER :: c_upname='setup_omi_exch_r_language_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       language = MERGE ( var, c_def_language, ( 1 <= var .AND. var <= c_max_language ) )
       IF ( no_error( ) ) CALL setup_omi_exch_language ( language ) 
    END IF
    !
  END SUBROUTINE setup_omi_exch_r_language_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_omi_exch_r_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_exch_r) , INTENT(OUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER :: c_upname='new_omi_exch_r_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       this%odx = c_undef_omi_exch_r_int
       this%gdx = c_undef_omi_exch_r_int
       NULLIFY ( this%exch )
    END IF
    !
  END SUBROUTINE new_omi_exch_r_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_omi_exch_r_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch_r) , INTENT(OUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=16) , PARAMETER :: c_upname='new_omi_exch_r_1' ! 
    !! Z&auml;hler      
    INTEGER :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       DO i=1,SIZE(this)
          IF ( any_error( ) ) EXIT
          CALL new_omi_exch_r_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_omi_exch_r_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_omi_exch_r_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_exch_r) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='kill_omi_exch_r_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       IF ( no_error( ) ) CALL new_omi_exch_r_0 ( this )
    END IF
    !
  END SUBROUTINE kill_omi_exch_r_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_omi_exch_r_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch_r) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='kill_omi_exch_r_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       DO i=1,SIZE(this)
          IF ( any_error( ) ) EXIT
          CALL kill_omi_exch_r_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_omi_exch_r_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_exch_r_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=15), PARAMETER :: c_upname='ok_omi_exch_r_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok = .false.
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok(1) = ok_omi_exch_r_odx( this )
       l_ok(2) = ok_omi_exch_r_gdx( this )
       l_ok(3) = ok_omi_exch_r_exch( this )
    END IF
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_omi_exch_r_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_exch_r_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=15), PARAMETER :: c_upname='ok_omi_exch_r_1' 
    !! Z&auml;hler      
    INTEGER :: i ! 
    !
    ok = .false.
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       DO i=1,SIZE(this)
          ok(i) = ok_omi_exch_r_0 ( this(i) )
       END DO
    END IF
    !
  END FUNCTION ok_omi_exch_r_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_exch_r_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this ! 
    !! Name der Function
    CHARACTER (LEN=20), PARAMETER :: c_upname='print_omi_exch_r_0' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7001, c_upname, c_modname, stat )
       IF ( no_error( ) ) CALL print_omi_exch_r_odx ( this )
       IF ( no_error( ) ) CALL print_omi_exch_r_gdx ( this )
       IF ( no_error( ) ) CALL print_omi_exch_r_exch( this )
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7002, c_upname, c_modname, stat )
       END IF
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT('# Beginn Objekt t_omi_exch_r ------------------------------')
8001 FORMAT('# Ende   Objekt t_omi_exch_r ------------------------------')
    !
  END SUBROUTINE print_omi_exch_r_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_exch_r_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this(:) ! 
    !! Name der Function
    CHARACTER (LEN=20), PARAMETER :: c_upname='print_omi_exch_r_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       DO i=1,SIZE(this)
          IF ( any_error( ) ) EXIT
          WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) i
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7003, c_upname, c_modname, stat )
          IF ( no_error( ) ) CALL print_omi_exch_r_0 ( this(i) )
       END DO
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT ('# Datenobjekt-Index i = ',I10.10,' ---------------------------')
    !
  END SUBROUTINE print_omi_exch_r_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_exch_r_static_d ( )
    !! Name der Function
    CHARACTER (LEN=27), PARAMETER :: c_upname='print_omi_exch_r_static_d' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )    &
           initialised, prn_op, trc_op, prn_lun, trc_lun, n_init, &
           language, c_undef_omi_exch_r_int
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       !
       IF ( no_error( ) ) CALL print_omi_exch_r_all_errors_d ( )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls b_omi_exch_r         ',/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
    '# initialised = ',L1,/ &
    '#      prn_op = ',L1,/ &
    '#      trc_op = ',L1,/ &
    '#     prn_lun = ',I5,/ &
    '#     trc_lun = ',I5,/ &
    '#      n_init = ',I5,/ &
    '#    language = ',I5,/ &
    '#  undef[int] = ',I10,/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
    '#------------------------------------------------------------') 
    !
  END SUBROUTINE print_omi_exch_r_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_exch_r_all_errors_d ( )
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='print_omi_exch_r_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_omi_exch_r_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! weise der Komponente "odx" einen skalaren Wert zu <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_exch_r_odx_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_exch_r) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "odx"
    INTEGER               , INTENT(IN)    :: val  ! 
    !
    this%odx = val
    !
  END SUBROUTINE set_omi_exch_r_odx_0_0
  !
  !! weise der Komponente "odx" denselben Wert zu  <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_exch_r_odx_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch_r) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "odx"
    INTEGER               , INTENT(IN)    :: val     ! 
    !
    this(:)%odx = val
    !
  END SUBROUTINE set_omi_exch_r_odx_1_0
  !
  !! weise der Komponente "odx" verschiedene Werte zu <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_exch_r_odx_1_1 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch_r) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "odx"
    INTEGER               , INTENT(IN)    :: val(:)  ! 
    !! Hilfsvariable
    INTEGER :: l ! 
    !
    l = MIN(SIZE(val),SIZE(this))
    this(1:l)%odx = val(1:l)
    !
  END SUBROUTINE set_omi_exch_r_odx_1_1
  !
  !! weise der Komponente "gdx" einen skalaren Wert zu <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_exch_r_gdx_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_exch_r) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "gdx"
    INTEGER               , INTENT(IN)    :: val  ! 
    !
    this%gdx = val
    !
  END SUBROUTINE set_omi_exch_r_gdx_0_0
  !
  !! weise der Komponente "gdx" denselben Wert zu  <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_exch_r_gdx_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch_r) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "gdx"
    INTEGER               , INTENT(IN)    :: val     ! 
    !
    this(:)%gdx = val
    !
  END SUBROUTINE set_omi_exch_r_gdx_1_0
  !
  !! weise der Komponente "gdx" verschiedene Werte zu <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_exch_r_gdx_1_1 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch_r) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "gdx"
    INTEGER               , INTENT(IN)    :: val(:)  ! 
    !! Hilfsvariable
    INTEGER :: l ! 
    !
    l = MIN(SIZE(val),SIZE(this))
    this(1:l)%gdx = val(1:l)
    !
  END SUBROUTINE set_omi_exch_r_gdx_1_1
  !
  !! weise der Komponente "exch(:)" Werte zu <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die externen Daten eingreichtet
  SUBROUTINE set_omi_exch_r_exch_ref_0_1 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_exch_r) , INTENT(INOUT) :: this   ! 
    !! Wert f&uuml;r Komponente "exch(:)"
    TYPE (t_omi_exch)     , POINTER       :: val(:) ! 
    !
    this%exch => val
    !
  END SUBROUTINE set_omi_exch_r_exch_ref_0_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! Holen des Index f&uuml;r die Spracheinstellung <BR>
  !! 1 = Deutsch  <BR>
  !! 2 = Englisch 
  FUNCTION get_omi_exch_r_language_d ( ) &
       RESULT( res )
    !! R&uuml;ckgabewert: 
    !! Index f&uuml;r Spracheinstellung (1 = Deutsch, 2 = Englisch )
    INTEGER :: res ! 
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER :: c_upname='get_omi_exch_r_language_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       res = language
    ELSE
       res = -1
    END IF
    !
  END FUNCTION get_omi_exch_r_language_d
  !
  !! hole die Komponente "odx" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_exch_r_odx_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_exch_r) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "odx" (Skalar)
    INTEGER :: val  ! 
    !
    val = this%odx
    !
  END FUNCTION get_omi_exch_r_odx_0_0
  !
  !! hole die Komponente "odx" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_exch_r_odx_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch_r) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "odx"
    INTEGER :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%odx
    !
  END FUNCTION get_omi_exch_r_odx_1_0
  !
  !! hole die Komponente "gdx" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_exch_r_gdx_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_exch_r) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "gdx" (Skalar)
    INTEGER :: val  ! 
    !
    val = this%gdx
    !
  END FUNCTION get_omi_exch_r_gdx_0_0
  !
  !! hole die Komponente "gdx" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_exch_r_gdx_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch_r) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "gdx"
    INTEGER :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%gdx
    !
  END FUNCTION get_omi_exch_r_gdx_1_0
  !
  !! hole die Komponente "exch(:)" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_exch_r_exch_ref_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_exch_r) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "exch(:)" 
    TYPE (t_omi_exch) , POINTER :: val(:)  ! 
    !
    val => this%exch
    !
  END FUNCTION get_omi_exch_r_exch_ref_0_1
  !
  !! Ermittle die Anzahl der zu <EM>importierenden</EM> Gr&ouml;&szlig;en f&uuml;r
  !! einen "ragged array" des Typs "t_omi_exch_r" <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_exch_r_importing_count1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch_r) , INTENT(IN)  :: this(:) ! 
    !! Ergebnis: (Gesamt-) Anzahl der zu importierenden Gr&ouml;&szlig;en
    INTEGER :: res ! 
    !! Hilfsvariable
    INTEGER :: i 
    !
    res = 0
    DO i=1,SIZE(this)
       IF ( ASSOCIATED( this(i)%exch ) ) THEN
          res = res + get_omi_exch_importing_count ( this(i)%exch )
       END IF
    END DO
    !
  END FUNCTION get_omi_exch_r_importing_count1
  !
  !! Ermittle die Anzahl der zu <EM>exportierenden</EM> Gr&ouml;&szlig;en f&uuml;r
  !! einen "ragged array" des Typs "t_omi_exch_r" <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_exch_r_exporting_count1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch_r) , INTENT(IN)  :: this(:) ! 
    !! Ergebnis: (Gesamt-) Anzahl der zu exportierenden Gr&ouml;&szlig;en
    INTEGER :: res ! 
    !! Hilfsvariable
    INTEGER :: i 
    !
    res = 0
    DO i=1,SIZE(this)
       IF ( ASSOCIATED( this(i)%exch ) ) THEN
          res = res + get_omi_exch_exporting_count ( this(i)%exch )
       END IF
    END DO
    !
  END FUNCTION get_omi_exch_r_exporting_count1
  !
  !! ermittle die Anzahl der unterschiedlichen <EM>Quantities</EM> in 
  !! "ragged arrays" des Typs "t_omi_exch_r" <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_exch_r_quant_count_1 ( this, comp_idx ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch_r) , INTENT(IN)  :: this(:)  ! 
    !! Komponenten-Identifikationsnummer
    INTEGER             , INTENT(IN)  :: comp_idx ! 
    !! Ergebnis: (Gesamt-) Anzahl <EM>unterschiedlicher Quantities</EM>
    INTEGER :: res ! 
    !! Hilfsvariable
    TYPE (t_string_r) , POINTER :: string_r(:)
    !
    CALL create_memo_data ( this, comp_idx )
    string_r => memo_quant_string_r
    IF ( ASSOCIATED(string_r) ) THEN
       res = get_string_r_true_count ( string_r )
    ELSE
       res = 0
    END IF
    NULLIFY(string_r)
    !
  END FUNCTION get_omi_exch_r_quant_count_1
  !
  !! ermittle die Anzahl der unterschiedlichen <EM>ElementSets</EM> in 
  !! "ragged arrays" des Typs "t_omi_exch_r" <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_exch_r_elemset_count_1 ( this, comp_idx ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch_r) , INTENT(IN)  :: this(:) ! 
    !! Komponenten-Identifikationsnummer
    INTEGER             , INTENT(IN)  :: comp_idx ! 
    !! Ergebnis: (Gesamt-) Anzahl <EM>unterschiedlicher ElementSets</EM>
    INTEGER :: res ! 
    !! Hilfsvariable
    TYPE (t_string_r) , POINTER :: string_r(:)
    !
    CALL create_memo_data ( this, comp_idx )
    string_r => memo_elemset_string_r
    IF ( ASSOCIATED(string_r) ) THEN
       res = get_string_r_true_count ( string_r )
    ELSE
       res = 0
    END IF
    NULLIFY(string_r)
    !
  END FUNCTION get_omi_exch_r_elemset_count_1
  !
  !! Ermittle den Wert des Arguments einer Datenoperation bei vorgegebener
  !! Id der Datenoperation, der Id des Arguments, sowie der Nummer des
  !! Auftretens des Argument-Keys <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_exch_r_dope_arg_value_1 ( this, dope_id, arg_id, n_arg ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch_r)    , INTENT(IN) :: this(:) ! 
    !! Id der Datenoperation
    CHARACTER (LEN=*)      , INTENT(IN) :: dope_id ! 
    !! Id des Arguments
    CHARACTER (LEN=*)      , INTENT(IN) :: arg_id  ! 
    !! lfd. Nummer f&uuml;r das Auftreten des Arguments
    INTEGER                , INTENT(IN) :: n_arg   ! 
    !! Ergebnis : Wert des gesuchten Arguments
    CHARACTER (LEN=c_len_omi_arg_value) :: res ! 
    !! Hilfsvariable
    TYPE (t_omi_dope) , POINTER :: p_dope(:) ! 
    TYPE (t_omi_arg)  , POINTER :: p_arg(:)  ! 
    INTEGER :: i, j, idx, jdx ! 
    !
    res = REPEAT( ' ', LEN(res) )
    res = c_undef_omi_dope_char
    DO i=1,SIZE(this)
       IF ( res(1:LEN(c_undef_omi_dope_char)) /= c_undef_omi_dope_char ) EXIT
       IF ( ASSOCIATED(this(i)%exch) ) THEN
          DO j=1,SIZE(this(i)%exch)
             IF ( res(1:LEN(c_undef_omi_dope_char)) /= c_undef_omi_dope_char ) EXIT
             p_dope => get_omi_exch_dope_ref ( this(i)%exch(j) )
             IF ( ASSOCIATED( p_dope ) ) THEN
                idx = get_omi_dope_idx ( p_dope, dope_id )
                IF ( idx /= c_undef_omi_dope_int ) THEN
                   p_arg => get_omi_dope_args_ref ( p_dope(idx) )
                   IF ( ASSOCIATED( p_arg ) ) THEN
                      jdx = get_omi_arg_idx ( p_arg, arg_id )
                      IF ( jdx > 0 ) THEN
                         res = get_omi_arg_value ( p_arg(jdx) )
                      END IF
                   END IF
                END IF
             END IF
          END DO
       END IF
    END DO
    NULLIFY ( p_dope, p_arg )
    !
  END FUNCTION get_omi_exch_r_dope_arg_value_1
  !
  !! ermittle die Posistionsindices in einem "ragged array" an der 
  !! eine bestimmte <EM>importierende</EM> Austauschgr&ouml;&szlig;e
  !! abgelegt ist <BR>
  !! Funktion erzeugt keine Fehlermeldungen
  FUNCTION get_omi_exch_r_inp_ijx_1 ( this, n_inp ) &
       RESULT( ij )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this(:) ! 
    !! lfd. Nummer der zu importierenden Austauschgr&ouml;&szlig;e
    INTEGER             , INTENT(IN) :: n_inp   ! 
    !! Ergebnis: Indices f&uuml;r die Position der n_inp-ten <EM>importierenden</EM> Austauschgr&ouml;&szlig;e <BR>
    !! this( ij(1) )%exch( ij(2) ) kennzeichnet die Position
    INTEGER :: ij(2) ! 
    !! Hilfsvariablen
    INTEGER :: i, ia, ie !
    !
    ij(:) = c_undef_omi_exch_r_int
    ia    = 1 ; ie    = 0
    DO i=1,SIZE(this)
       IF ( ALL(ij /= c_undef_omi_exch_r_int ) ) EXIT
       ie = ie + get_omi_exch_importing_count( this(i)%exch )
       IF ( n_inp >= ia .AND. n_inp <= ie ) THEN
          ij(1) = i
          ij(2) = get_omi_exch_importing_idx( this(i)%exch, n_inp-ia+1 )
       END IF
       ia = ie + 1
    END DO
    !
  END FUNCTION get_omi_exch_r_inp_ijx_1
  !
  !! ermittle die Posistionsindices in einem "ragged array" an der 
  !! eine bestimmte <EM>exportierende</EM> Austauschgr&ouml;&szlig;e
  !! abgelegt ist <BR>
  !! Funktion erzeugt keine Fehlermeldungen
  FUNCTION get_omi_exch_r_out_ijx_1 ( this, n_out ) &
       RESULT( ij )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this(:) ! 
    !! lfd. Nummer der zu exportierenden Austauschgr&ouml;&szlig;e
    INTEGER             , INTENT(IN) :: n_out   ! 
    !! Ergebnis: Indices f&uuml;r die Position der n_out-ten <EM>exportierenden</EM> Austauschgr&ouml;&szlig;e <BR>
    !! this( ij(1) )%exch( ij(2) ) kennzeichnet die Position
    INTEGER :: ij(2) ! 
    !! Hilfsvariablen
    INTEGER :: i, ia, ie !
    !
    ij(:) = c_undef_omi_exch_r_int
    ia    = 1 ; ie    = 0
    DO i=1,SIZE(this)
       IF ( ALL(ij /= c_undef_omi_exch_r_int ) ) EXIT
       ie = ie + get_omi_exch_exporting_count( this(i)%exch )
       IF ( n_out >= ia .AND. n_out <= ie ) THEN
          ij(1) = i
          ij(2) = get_omi_exch_exporting_idx( this(i)%exch, n_out-ia+1 )
       END IF
       ia = ie + 1
    END DO
    !
  END FUNCTION get_omi_exch_r_out_ijx_1
  !
  !! ermittle die Posistionsindices in einem "ragged array" an der 
  !! eine bestimmte <EM>Quantity</EM> abgelegt ist anhand der lfd. 
  !! Nummer der <EM>Quantity</EM><BR>
  !! Funktion erzeugt keine Fehlermeldungen
  FUNCTION get_omi_exch_r_quant_ijx_1 ( this, n_quant, comp_idx ) &
       RESULT( ij )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this(:) ! 
    !! lfd. Nummer der <EM>Quantity</EM>
    INTEGER             , INTENT(IN) :: n_quant ! 
    !! Komponenten-Identifikationsnummer
    INTEGER             , INTENT(IN)  :: comp_idx ! 
    !! Ergebnis: Indices f&uuml;r die Position der n_quant-ten <EM>Quantity</EM> <BR>
    !! this( ij(1) )%exch( ij(2) ) kennzeichnet die Position
    INTEGER :: ij(2) ! 
    !! Hilfsvariablen
    INTEGER :: i !
    TYPE (t_string_r) , POINTER :: string_r(:)
    !
    CALL create_memo_data ( this, comp_idx )
    ij(:)    = c_undef_omi_exch_r_int
    string_r => memo_quant_string_r
    IF ( ASSOCIATED( string_r ) ) THEN
       ij(:) = get_string_r_true_ijx ( string_r, n_quant )
    END IF
    NULLIFY( string_r )
    !
  END FUNCTION get_omi_exch_r_quant_ijx_1
  !
  !! ermittle die Posistionsindices in einem "ragged array" an der 
  !! eine bestimmte <EM>Quantity</EM> abgelegt ist anhand der Id 
  !! der <EM>Quantity</EM> <BR>
  !! Funktion erzeugt keine Fehlermeldungen
  FUNCTION get_omi_exch_r_quant_id_ijx_1 ( this, quant_id, comp_idx ) &
       RESULT( ij )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this(:)  ! 
    !! Id der <EM>Quantity</EM>
    CHARACTER (LEN=*)   , INTENT(IN) :: quant_id ! 
    !! Komponenten-Identifikationsnummer
    INTEGER             , INTENT(IN)  :: comp_idx ! 
    !! Ergebnis: Indices f&uuml;r die (erste) Position der <EM>Quantity</EM> mit Id "quant_id" <BR>
    !! this( ij(1) )%exch( ij(2) ) kennzeichnet die Position
    INTEGER :: ij(2) ! 
    !! Hilfsvariablen
    INTEGER :: i !
    TYPE (t_string_r) , POINTER :: string_r(:)
    !
    CALL create_memo_data ( this, comp_idx )
    ij(:)    = c_undef_omi_exch_r_int
    string_r => memo_quant_string_r
    IF ( ASSOCIATED( string_r ) ) THEN
       ij(:) = get_string_id_ijx ( string_r, quant_id )
    END IF
    NULLIFY( string_r )
    !
  END FUNCTION get_omi_exch_r_quant_id_ijx_1
  !
  !! ermittle die Posistionsindices in einem "ragged array" an der 
  !! ein bestimmtes <EM>ElementSet</EM> abgelegt ist anhand der lfd. 
  !! Nummer des <EM>ElementSet</EM><BR>
  !! Funktion erzeugt keine Fehlermeldungen
  FUNCTION get_omi_exch_r_elemset_ijx_1 ( this, n_elemset, comp_idx ) &
       RESULT( ij )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this(:) ! 
    !! lfd. Nummer des <EM>ElementSet</EM>
    INTEGER             , INTENT(IN) :: n_elemset ! 
    !! Komponenten-Identifikationsnummer
    INTEGER             , INTENT(IN)  :: comp_idx ! 
    !! Ergebnis: Indices f&uuml;r die Position der n_elemset-ten <EM>ElementSet</EM> <BR>
    !! this( ij(1) )%exch( ij(2) ) kennzeichnet die Position
    INTEGER :: ij(2) ! 
    !! Hilfsvariablen
    INTEGER :: i !
    TYPE (t_string_r) , POINTER :: string_r(:)
    !
    CALL create_memo_data ( this, comp_idx )
    ij(:)    = c_undef_omi_exch_r_int
    string_r => memo_elemset_string_r
    IF ( ASSOCIATED( string_r ) ) THEN
       ij(:) = get_string_r_true_ijx ( string_r, n_elemset )
    END IF
    NULLIFY(string_r)
    !
  END FUNCTION get_omi_exch_r_elemset_ijx_1
  !
  !! ermittle die Posistionsindices in einem "ragged array" an der 
  !! ein bestimmtes <EM>ElementSet</EM> abgelegt ist anhand der
  !! Id des <EM>ElementSet</EM><BR>
  !! Funktion erzeugt keine Fehlermeldungen
  FUNCTION get_omi_exch_r_elemset_id_ijx_1 ( this, elemset_id, comp_idx ) &
       RESULT( ij )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this(:)    ! 
    !! lfd. Nummer des <EM>ElementSet</EM>
    CHARACTER (LEN=*)   , INTENT(IN) :: elemset_id ! 
    !! Komponenten-Identifikationsnummer
    INTEGER             , INTENT(IN)  :: comp_idx ! 
    !! Ergebnis: Indices f&uuml;r die Position des <EM>ElementSet</EM> mit Id "elemset_id" <BR>
    !! this( ij(1) )%exch( ij(2) ) kennzeichnet die Position
    INTEGER :: ij(2) ! 
    !! Hilfsvariablen
    INTEGER :: i !
    TYPE (t_string_r) , POINTER :: string_r(:)
    !
    CALL create_memo_data ( this, comp_idx )
    ij(:)    = c_undef_omi_exch_r_int
    string_r => memo_elemset_string_r
    IF ( ASSOCIATED( string_r ) ) THEN
       ij(:) = get_string_id_ijx ( string_r, elemset_id )
    END IF
    NULLIFY( string_r )
    !
  END FUNCTION get_omi_exch_r_elemset_id_ijx_1
  !
  !! ermittle die Posistionsindices in einem "ragged array" an der 
  !! ein bestimmtes <EM>ElementSet</EM> und eine bestimmte <EM>Quantity</EM>
  !! abgelegt sind anhand der Id's <BR>
  !! Funktion erzeugt keine Fehlermeldungen
  FUNCTION get_omi_exch_r_ijx_1 ( this, elemset_id, quant_id, comp_idx ) &
       RESULT( ij )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this(:)    ! 
    !! Id des <EM>ElementSet</EM>
    CHARACTER (LEN=*)   , INTENT(IN) :: elemset_id ! 
    !! Id der <EM>Quantity</EM>
    CHARACTER (LEN=*)   , INTENT(IN) :: quant_id   ! 
    !! Komponenten-Identifikationsnummer
    INTEGER             , INTENT(IN)  :: comp_idx ! 
    !! Ergebnis: Indices f&uuml;r die Position der Kombination aus 
    !! <EM>ElementSet</EM> und <EM>Quantity</EM> <BR>
    !! this( ij(1) )%exch( ij(2) ) kennzeichnet die Position
    INTEGER :: ij(2) ! 
    !! Hilfsvariablen
    INTEGER :: i ! 
    TYPE (t_string_r) , POINTER :: e_string_r(:), q_string_r(:)
    !
    CALL create_memo_data ( this, comp_idx )
    ij(:) = c_undef_omi_exch_r_int
    e_string_r => memo_elemset_string_r
    q_string_r => memo_quant_string_r 
    IF ( ASSOCIATED( e_string_r ) .AND. ASSOCIATED( q_string_r ) ) THEN
       ij(:) = get_string_r_ijx ( e_string_r, q_string_r, elemset_id, quant_id )
    END IF
    NULLIFY( e_string_r , q_string_r )
    !
  END FUNCTION get_omi_exch_r_ijx_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_exch_r_0_0 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1) = ( this1%odx == this2%odx )
    l_ok(2) = ( this1%gdx == this2%gdx )
    l_ok(3) = eq_omi_exch_r_exch ( this1, this2 )
    ok      = ALL( l_ok )
    !
  END FUNCTION eq_omi_exch_r_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_exch_r_1_0 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    ! 
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_exch_r_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_omi_exch_r_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_exch_r_0_1 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_exch_r_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_omi_exch_r_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_exch_r_1_1 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_exch_r_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_omi_exch_r_1_1
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
  FUNCTION ne_omi_exch_r_0_0 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = .NOT. eq_omi_exch_r_0_0( this1, this2 )
    !
  END FUNCTION ne_omi_exch_r_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_exch_r_1_0 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ok(:) = .NOT. eq_omi_exch_r_1_0( this1(:), this2 )
    !
  END FUNCTION ne_omi_exch_r_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Vektor )<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION ne_omi_exch_r_0_1 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ok(:) = .NOT. eq_omi_exch_r_0_1( this1, this2(:) )
    !
  END FUNCTION ne_omi_exch_r_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_exch_r_1_1 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Hilfsvariable
    INTEGER :: l ! 
    !
    l       = SIZE(ok)
    ok(1:l) = .NOT. eq_omi_exch_r_1_1( this1(1:l), this2(1:l) )
    !
  END FUNCTION ne_omi_exch_r_1_1
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
  FUNCTION ok_initialised ( upname ) &
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
       WRITE(*,*) ' *** Warnung *** Modul "b_omi_exch_r" nicht initialisiert'
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIt_omi_exch_r ausfuehren'
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
  SUBROUTINE init_omi_exch_r_all_errors ( )
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER  :: c_upname='init_omi_exch_r_all_errors' !
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
               '--> INIt_omi_exch_r ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_omi_exch_r ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_exch_r"\n'//&
               'Typ-Komponente = "odx"\n'//&
               'aktuell        = <actual>, muss > 0 sein\n'//&
               '--> SET_OMI_EXCH_R_ODX korrekt verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_exch_r"\n'//&
               'Typ-Komponente = "gdx"\n'//&
               'aktuell        = <actual>, muss > 0 sein\n'//&
               '--> SET_OMI_EXCH_R_GDX korrekt verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_exch_r"\n'//&
               'Typ-Komponente = "exch(:)"\n'//&
               'assoziiert   = <associated>"\n'//&
               'Inhalte o.k. = <ok>"\n'//&
               '--> SET_OMI_EXCH_R_EXCH_REF korrekt verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code in Modul "b_omi_exch_r" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Fusszeilen\n'//&
               '--> Code in Modul "b_omi_exch_r" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Index des Datenobjektes (1D-Array)\n'//&
               '--> Code in Modul "b_omi_exch_r" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_exch_r"\n'//&
               'Typ-Komponente = "odx"\n'//&
               '--> Code in Modul "b_omi_exch_r" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_exch_r"\n'//&
               'Typ-Komponente = "gdx"\n'//&
               '--> Code in Modul "b_omi_exch_r" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_exch_r"\n'//&
               'Typ-Komponente = "exch(:)"\n'//&
               '--> Code in Modul "b_omi_exch_r" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "b_omi_exch_r"\n'//&
               '--> Code in Modul "b_omi_exch_r" / Daten pruefen' )
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
  END SUBROUTINE init_omi_exch_r_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_omi_exch_r_all_errors ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_omi_exch_r_all_errors
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
  ! >>> PRIVATE-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe, ob die Komponente "odx" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_exch_r_odx ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_exch_r) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='ok_omi_exch_r_odx' ! 
    !! Hilfsvariable
    CHARACTER (LEN=10) :: ch ! 
    !
    ok = ( this%odx /= c_undef_omi_exch_r_int .AND. this%odx > 0 )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
       WRITE(ch,'(I10)') this%odx ; CALL  setup_error_act ( '<actual>', TRIM(ch) )
    END IF
    !
  END FUNCTION ok_omi_exch_r_odx
  !
  !! Pr&uuml;fe, ob die Komponente "gdx" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_exch_r_gdx ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_exch_r) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='ok_omi_exch_r_gdx' ! 
    !! Hilfsvariable
    CHARACTER (LEN=10) :: ch ! 
    !
    ok = ( this%gdx == c_undef_omi_exch_r_int .OR. this%gdx > 0 )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
       WRITE(ch,'(I10)') this%gdx ; CALL  setup_error_act ( '<actual>', TRIM(ch) )
    END IF
    !
  END FUNCTION ok_omi_exch_r_gdx
  !
  !! Pr&uuml;fe, ob die Komponente "exch(:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_exch_r_exch ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_exch_r) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='ok_omi_exch_r_exch' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=1) :: ch ! 
    LOGICAL           :: l_ok(2) ! 
    !
    l_ok(:) = .false.
    l_ok(1) = ASSOCIATED( this%exch )
    IF ( l_ok(1) ) l_ok(2) = ALL( ok_omi_exch ( this%exch(:) ) )
    ok = ALL( l_ok )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
       WRITE(ch,'(L1)') l_ok(1) ; CALL setup_error_act ( '<associated>', ch )
       WRITE(ch,'(L1)') l_ok(2) ; CALL setup_error_act ( '<ok>', ch )
    END IF
    !
  END FUNCTION ok_omi_exch_r_exch
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "odx" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_exch_r_odx ( this )
    !! Datenobjekt
    TYPE (t_omi_exch_r) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=20) , PARAMETER :: c_upname='print_omi_exch_r_odx' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) this%odx
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente odx - - - - - - - - - - - - - - - - - ',/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',I16)
    !
  END SUBROUTINE print_omi_exch_r_odx
  !
  !! Drucke den Inhalt der Komponente "gdx" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_exch_r_gdx ( this )
    !! Datenobjekt
    TYPE (t_omi_exch_r) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=20) , PARAMETER :: c_upname='print_omi_exch_r_gdx' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) this%gdx
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente gdx - - - - - - - - - - - - - - - - - ',/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',I16)
    !
  END SUBROUTINE print_omi_exch_r_gdx
  !
  !! Drucke den Inhalt der Komponente "exch(:)" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_exch_r_exch ( this )
    !! Datenobjekt
    TYPE (t_omi_exch_r) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=21) , PARAMETER :: c_upname='print_omi_exch_r_exch' ! 
    !
    IF ( ASSOCIATED(this%exch) ) THEN
       CALL print_omi_exch ( this%exch(:) )
       IF ( any_error( ) ) THEN
          CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname )
       END IF
    END IF
    !
  END SUBROUTINE print_omi_exch_r_exch
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe Komponente "exch(:)" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_exch_r_exch ( this1, this2 ) &
       RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this2 ! 
    !! Testergebnis
    LOGICAL :: ok 
    !! Hilfsvariable
    LOGICAL :: as(2) ! 
    !
    as(1) = ASSOCIATED( this1%exch )
    as(2) = ASSOCIATED( this2%exch )
    !
    IF ( ALL(as) ) THEN
       ok = ALL( eq_omi_exch( this1%exch(:), this2%exch(:) ) )
    ELSE IF ( ALL( .NOT.as ) ) THEN
       ok = .true.
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION eq_omi_exch_r_exch
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
  ! >>> PRIVATE-STORE-Funktionen <<<
  ! ----------------------------------------------------------------------
  !
  !! Speichere alle "quant_id" eines Feldes "t_omi_exch_r" in "t_string_r" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE create_quant_id_in_string_r ( this, string_r )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch_r) , INTENT(IN)  :: this(:)      !  
    !! Ergebnis: "ragged array" mit allen "quant_id"s
    TYPE (t_string_r) , INTENT(INOUT) :: string_r(:)  ! 
    !! Hilfsvariable
    INTEGER                      :: i, j, ns ! 
    TYPE (t_omi_quant) , POINTER :: p_quant  !  
    !
    DO i=1,MIN(SIZE(this),SIZE(string_r))
       NULLIFY ( string_r(i)%l, string_r(i)%s )
       IF ( ASSOCIATED(this(i)%exch) ) THEN
          ns = SIZE(this(i)%exch)
          ALLOCATE( string_r(i)%l(ns), string_r(i)%s(ns) )
          string_r(i)%l(:) = .false.
          string_r(i)%s(:) = REPEAT( ' ', LEN(string_r(i)%s) )
          DO j=1,ns
             p_quant => get_omi_exch_quant_ref ( this(i)%exch(j) )
             IF ( ASSOCIATED( p_quant ) ) THEN
                string_r(i)%l(j) = .true.
                string_r(i)%s(j) = get_omi_quant_id ( p_quant )
             END IF
             NULLIFY( p_quant )
          END DO
       END IF
    END DO
    !
  END SUBROUTINE create_quant_id_in_string_r
  !
  !! Speichere alle "elemset_id" eines Feldes "t_omi_exch_r" in "t_string_r" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE create_elemset_id_in_string_r ( this, string_r )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch_r) , INTENT(IN)  :: this(:) ! 
    !! Ergebnis: "ragged array" mit allen "elemset_id"s
    TYPE (t_string_r) , INTENT(INOUT) :: string_r(:)  ! 
    !! Hilfsvariable
    INTEGER                    :: i, j, ns ! 
    TYPE (t_omi_ele) , POINTER :: p_ele    !  
    !
    DO i=1,MIN(SIZE(this),SIZE(string_r))
       NULLIFY ( string_r(i)%l, string_r(i)%s )
       IF ( ASSOCIATED(this(i)%exch) ) THEN
          ns = SIZE(this(i)%exch)
          ALLOCATE( string_r(i)%l(ns), string_r(i)%s(ns) )
          string_r(i)%l(:) = .false.
          string_r(i)%s(:) = REPEAT( ' ', LEN(string_r(i)%s) )
          DO j=1,ns
             p_ele => get_omi_exch_ele_ref ( this(i)%exch(j) )
             IF ( ASSOCIATED( p_ele ) ) THEN
                string_r(i)%l(j) = .true.
                string_r(i)%s(j) = get_omi_ele_id ( p_ele )
             END IF
             NULLIFY( p_ele )
          END DO
       END IF
    END DO
    !
  END SUBROUTINE create_elemset_id_in_string_r
  !
  !! setze die Komponente "l(:)" in einem Objekt des Typs "t_string_r" so,
  !! dass jeweils das erste Auftreten eines Strings mit .true., und alle
  !! anderen Position mit .false. gekennzeichnet werden <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE update_l_in_string_r ( string_r )
    !! Datenobjekt (Vektor)
    TYPE (t_string_r) , INTENT(INOUT) :: string_r(:)
    !! Hilfsvariablen
    INTEGER :: i, j, ii, jj, ia, ja, l, ll ! 
    !
    DO i=1,SIZE(string_r)
       DO j=1,SIZE(string_r(i)%l)
          IF ( .NOT. string_r(i)%l(j) ) CYCLE
          ia = MERGE( i+1, i, j==SIZE(string_r(i)%l) )
          ja = MERGE( 1, j+1, j==SIZE(string_r(i)%l) )
          l  = LEN_TRIM(string_r(i)%s(j))
          DO ii=ia,SIZE(string_r)
             DO jj=ja,SIZE(string_r(ii)%l)
                IF ( .NOT. string_r(ii)%l(jj) ) CYCLE
                ll = LEN_TRIM(string_r(ii)%s(jj))
                IF ( l == ll ) THEN
                   IF ( string_r(i)%s(j)(1:l) == string_r(ii)%s(jj)(1:ll) ) THEN
                      string_r(ii)%l(jj) = .false.
                   END IF
                END IF
             END DO
             ja = 1
          END DO
       END DO
    END DO
    !
  END SUBROUTINE update_l_in_string_r
  !
  !! ermittle die Anzahl der .true.-Werte der Komponente "l" in Objekt "t_string_r" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_string_r_true_count ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_string_r) , INTENT(IN) :: this(:) 
    !! Ergebnis: Anzahl der true-Werte
    INTEGER :: res
    !! Hilfsvariable
    INTEGER :: i 
    !
    res = 0
    DO i=1,SIZE(this)
       IF ( ASSOCIATED(this(i)%l) ) THEN
          res = res + COUNT( this(i)%l )
       END IF
    END DO
    !
  END FUNCTION get_string_r_true_count
  !
  !! ermittle die true-Position einer bestimmten Gr&ouml;&szlig;e anhand der lfd. Nummer <BR>
  !! Funktion erzeugt keine Fehlermeldungen
  FUNCTION get_string_r_true_ijx ( this, n ) &
       RESULT( ij )
    !! Datenobjekt (Vektor)
    TYPE (t_string_r), INTENT(IN) :: this(:) 
    !! lfd. Nummer der true-Position
    INTEGER          , INTENT(IN) :: n     ! 
    !! Ergebnis: Position this( ij(1) )%s( ij(2) )
    INTEGER                       :: ij(2) ! 
    !! Hilfsvariable
    INTEGER :: i, j, ia, ie, nn ! 
    !
    ij(:) = c_undef_omi_exch_r_int
    ia    = 1
    ie    = 0
    DO i=1,SIZE(this)
       IF ( ALL( ij /= c_undef_omi_exch_r_int ) ) EXIT
       IF ( ASSOCIATED( this(i)%l ) ) THEN
          ie = ie + COUNT( this(i)%l )
          IF ( n >= ia .AND. n <= ie ) THEN
             ij(1) = i
             nn    = ia - 1
             DO j=1,SIZE( this(i)%l )
                IF ( ij(2) /= c_undef_omi_exch_r_int ) EXIT
                IF ( .NOT. this(i)%l(j)              ) CYCLE
                nn = nn + 1
                IF ( nn == n ) ij(2) = j
             END DO
          END IF
          ia = ie + 1
       END IF
    END DO
    !
  END FUNCTION get_string_r_true_ijx
  !
  !! ermittle die Position einer bestimmten Gr&ouml;&szlig;e anhand ihrer Id <BR>
  !! Funktion erzeugt keine Fehlermeldungen
  FUNCTION get_string_id_ijx ( this, id ) &
       RESULT( ij )
    !! Datenobjekt (Vektor)
    TYPE (t_string_r) , INTENT(IN) :: this(:) 
    !! Id
    CHARACTER (LEN=*) , INTENT(IN) :: id 
    !! Ergebnis: Position this( ij(1) )%s( ij(2) )
    INTEGER                       :: ij(2)
    !! Hilfsvariable
    INTEGER :: i, j, l, ll
    !
    ij(:) = c_undef_omi_exch_r_int
    l = LEN_TRIM(id)
    DO i=1,SIZE(this)
       IF ( ALL( ij /= c_undef_omi_exch_r_int ) ) EXIT
       IF ( ASSOCIATED( this(i)%s ) ) THEN
          DO j=1,SIZE(this(i)%s)
             ll = LEN_TRIM(this(i)%s(j))
             IF ( l == ll ) THEN
                IF ( id(1:l) == this(i)%s(j)(1:ll) ) THEN
                   ij(1) = i
                   ij(2) = j
                END IF
             END IF
          END DO
       END IF
    END DO
    !
  END FUNCTION get_string_id_ijx
  !
  !! ermittle die Position f&uuml;r eine bestimmte Kombination aus
  !! "e_id" und "q_id" <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_string_r_ijx ( e_string_r, q_string_r, elemset_id, quant_id ) &
       RESULT( ij )
    !! Datenobjekt "ElementSet_Id" (Vektor)
    TYPE (t_string_r) , INTENT(IN) :: e_string_r(:) ! 
    !! Datenobjekt "Quant_Id" (Vektor)
    TYPE (t_string_r) , INTENT(IN) :: q_string_r(:) ! 
    !! gesuchte "elemset_id"
    CHARACTER (LEN=*) , INTENT(IN) :: elemset_id    ! 
    !! gesuchte "quant_id"
    CHARACTER (LEN=*) , INTENT(IN) :: quant_id      ! 
    !! Ergebnis: Position this( ij(1) )%s( ij(2) )
    INTEGER                       :: ij(2)          ! 
    !! Hilfsvariablen
    INTEGER :: i, j, le(2), lq(2) ! 
    !
    ij(:) = c_undef_omi_exch_r_int
    le(1) = LEN_TRIM(elemset_id)
    lq(1) = LEN_TRIM(quant_id)
    DO i=1,MIN(SIZE(e_string_r),SIZE(q_string_r))
       IF ( ALL(ij /= c_undef_omi_exch_r_int) ) EXIT
       DO j=1,MIN(SIZE(e_string_r(i)%s),SIZE(q_string_r(i)%s))
          IF ( ALL(ij /= c_undef_omi_exch_r_int) ) EXIT
          le(2) = LEN_TRIM(e_string_r(i)%s(j))
          lq(2) = LEN_TRIM(q_string_r(i)%s(j))
          IF ( ALL(le(:)==le(1)) .AND. ALL(lq(:)==lq(1)) ) THEN
             IF ( elemset_id(1:le(1)) == e_string_r(i)%s(j)(1:le(2)) .AND. &
                  quant_id(1:lq(1))   == q_string_r(i)%s(j)(1:lq(2)) ) THEN
                ij(1) = i
                ij(2) = j
             END IF
          END IF
       END DO
    END DO
    !
  END FUNCTION get_string_r_ijx
  !
  !! Erzeuge die Felder "memo_elemset_string_r" und "memo_quant_string_r" bei Bedarf <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE create_memo_data ( this, comp_idx )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch_r) , INTENT(IN) :: this(:) ! 
    !! Nummer der Komponente
    INTEGER , INTENT(IN) :: comp_idx ! 
    !
    IF ( comp_idx == memo_comp_idx        .AND. &
         ALLOCATED(memo_elemset_string_r) .AND. &
         ALLOCATED(memo_quant_string_r)         ) THEN
       CONTINUE
!       write(*,*) ' >GL> re-use memo data in '//c_modname
    ELSE
!       write(*,*) ' >GL> build new memo data in '//c_modname
       memo_comp_idx = comp_idx
       IF ( ALLOCATED( memo_elemset_string_r   ) ) THEN
          CALL dealloc_string_r ( memo_elemset_string_r )
          DEALLOCATE ( memo_elemset_string_r )
       END IF
       IF ( ALLOCATED( memo_quant_string_r   ) ) THEN
          CALL dealloc_string_r ( memo_quant_string_r )
          DEALLOCATE ( memo_quant_string_r )
       END IF
       ALLOCATE(memo_elemset_string_r(SIZE(this)), memo_quant_string_r(SIZE(this)))
       !
       CALL create_quant_id_in_string_r   ( this, memo_quant_string_r   )
       CALL update_l_in_string_r          ( memo_quant_string_r         )
       CALL create_elemset_id_in_string_r ( this, memo_elemset_string_r )
       CALL update_l_in_string_r          ( memo_elemset_string_r       )
       !
    END IF
    ! 
  END SUBROUTINE create_memo_data

  !! De-allokiere eine Datenobjekt des Typs "t_string_r" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE dealloc_string_r ( string_r )
    !! Datenobjekt (Vektor)
    TYPE (t_string_r) , INTENT(INOUT) :: string_r(:) ! 
    !! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(string_r)
       IF ( ASSOCIATED(string_r(i)%l) ) THEN
          DEALLOCATE( string_r(i)%l )
          NULLIFY   ( string_r(i)%l )
       END IF
       IF ( ASSOCIATED(string_r(i)%s) ) THEN
          DEALLOCATE( string_r(i)%s )
          NULLIFY   ( string_r(i)%s )
       END IF
    END DO
    !
  END SUBROUTINE dealloc_string_r
  !
END MODULE b_omi_exch_r
! TailOfBaseModule --------------------------------------------------------
