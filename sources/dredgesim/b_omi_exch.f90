! -------------------------------------------------------------------------
! HeadOfBaseModule --------------------------------------------------------
!
!! <H2>Typ und Methoden analog zu der OpenMI-Schnittstelle <EM>IExchangeItem</EM></h2>
!! @author G. Lang
!! @version 2.1 vom 07/21/05, Quellcode: mod_b_omi_exch.f90
!! <HR>
!! type and methods equivalent to OpenMI-interface <EM>IExchangeItem</EM><BR>
!! <HR>
!  Copyright-Hinweis
!                                                                    
!  Copyright (C) 2005 <A HREF="http://www.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A>
!                                                                    
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2005-02-25 : G. Lang : Erstversion
!  01.02 : 2005-03-02 : G. Lang : Kopierfunktion "copy_omi_exch"
!  01.03 : 2005-03-02 : G. Lang : Schreibkorrekturen
!  01.04 : 2005-03-11 : G. Lang : OPERATORen entfernt, auf Funktionen umgestellt
!  02.01 : 2005-07-21 : G. Lang : Anpassungen fuer korrigiertes ElementSet-Verstaendnis (GEI)
!
!!                                                                  <BR>
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!!
!! Typ und Methoden analog OpenMI-Interface <EM>IExchangeItem</EM>. 
!! Impelementiert einen Typ und Methoden zur Aufnahme von Informationen 
!! &uuml;ber Austauschgr&ouml;&szlig;en f&uuml;r den Import und
!! Export von Daten und Metadaten. Hierbei werden insbesondere
!! Zusammenh&auml;nge zwischen auszutauschender Gr&ouml;&szlig;e und 
!! dem Ort an dem die Gr&ouml;&szlig;e vorliegt verwaltet.
!! Typ und Methoden erleichtern den Austausch der zwischen verschiedenen 
!! OpenMI-konformen Komponenten.
!!
!! <OL>
!!    <LI> Initialisierung und De-Initialisierung von skalaren und
!!         vektoriellen Variablen des Typs "t_omi_exch";
!!    <LI> Setzen der Komponenten in Variablen des Typs "t_omi_exch";
!!    <LI> Holen der Komponenten aus Variablen des Typs "t_omi_exch";
!!    <LI> Drucken des Inhalts der Komponenten von Variablen des Typs "t_omi_exch";
!!    <LI> Pr&uuml;fen des Inhalts von Variablen des Typs "t_omi_exch";
!!    <LI> Vergleichen des Inhalts verschiedener Variablen des Typs "t_omi_exch".
!! </OL>
!!
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt den selbst definierten Datentyp "t_omi_exch"
!! zur Verf&uuml;gung. Dieser besteht aus den folgenden Komponenten:      <BR>
!! <OL>
!!     <LI> quant   : Beschreibung der physikalischen Gr&ouml;&szlig;e, deren Daten ausgetauscht werden sollen
!!     <LI> ele     : Beschreibung des <EM>ElementSet</EM>, auf dem die Daten ausgetauscht werden sollen
!!     <LI> dope(:) : (optionale) Liste mit den Beschreibungen der f&uuml;r zu exportierende Gr&ouml;&szlig;en g&uuml';ltigen Datenoperationen
!!     <LI> role    : Charakterisierung der Art der Austauschgr&ouml;&szlig;e (Export=1, Import=2)
!! </OL>
!!                                                                  <BR>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Verwenden des Moduls</H3>
!!                                                                  <BR>
!! Die Leistungen des Moduls k&ouml;nnen wie folgt in Anspruch genommen werden:
!! <OL>
!!    <LI> Einbinden des Moduls mittels USE-Anweisung in der rufenden Programmeinheit;
!!    <LI> Initialisieren des Moduls b_omi_exch mit INIT-Methode;
!!    <LI> ggf. Verwenden verschiedener SETUP-Methoden;
!!    <LI> Verwenden beliebiger Modul-Methoden (nicht INIT und CLEAR);
!!    <LI> De-Initialisieren des Moduls b_omi_exch mit CLEAR-Methode.
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
!!          die Methode PRINT_OMI_EXCH_ALL_ERRORS.
!
MODULE b_omi_exch
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] BASIS-Modul mit globalen Konstantwerten
  !
  USE b_constants, ONLY : &
       ! Parameter
       Single,            &
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
  ! [A.3] BASIS-Modul zur Beschreibung auszutauschender Gr&ouml;&szlig;en
  !
  USE b_omi_quant, ONLY :        &
       !   Typdefinitionen
       t_omi_quant,              &
       ! Konstante
       c_len_omi_quant_id,       &
       !   Routinen / Interfaces
       init_omi_quant,           &
       clear_omi_quant,          &
       setup_omi_quant_prn_lun,  &
       setup_omi_quant_trc_lun,  &
       setup_omi_quant_language, &
       ok_omi_quant,             &
       get_omi_quant_id,         &
       is_omi_quant_scalar,      &
       is_omi_quant_vector,      &
       print_omi_quant,          &
       !   Operatoren
       eq_omi_quant
  !
  ! [A.4] BASIS-Modul zur Beschreibung der <EM>ElementSets</EM>
  !
  USE b_omi_ele, ONLY :          &
       !   Typdefinitionen
       t_omi_ele,                &
       ! Konstante
       c_len_omi_ele_id,         &
       !   Routinen / Interfaces
       init_omi_ele,             &
       clear_omi_ele,            &
       setup_omi_ele_prn_lun,    &
       setup_omi_ele_trc_lun,    &
       setup_omi_ele_language,   &
       ok_omi_ele,               &
       get_omi_ele_id,           &
       get_omi_ele_point_count,  &
       print_omi_ele,            &
       !   Operatoren
       eq_omi_ele
  !
  ! [A.5] BASIS-Modul zur Beschreibung der Datenoperationen
  !
  USE b_omi_dope, ONLY :         &
       !   Typdefinitionen
       t_omi_dope,               &
       ! Konstante
       c_len_omi_dope_id,        &
       !   Routinen / Interfaces
       init_omi_dope,            &
       clear_omi_dope,           &
       setup_omi_dope_prn_lun,   &
       setup_omi_dope_trc_lun,   &
       ok_omi_dope,              &
       get_omi_dope_id,          &
       print_omi_dope,           &
       !   Operatoren
       eq_omi_dope
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
  !! quant   : Beschreibung der physikalischen Gr&ouml;&szlig;e, deren Daten ausgetauscht werden sollen <BR>
  !! ele     : Beschreibung des <EM>ElementSet</EM>, auf dem die Daten ausgetauscht werden sollen <BR>
  !! dope(:) : (optionale) Liste mit den Beschreibungen der f&uuml;r zu exportierende Gr&ouml;&szlig;en g&uuml;ltigen Datenoperationen <BR>
  !! role    : Charakterisierung der Art der Austauschgr&ouml;&szlig;e (Export=1, Import=2)
  TYPE , PUBLIC :: t_omi_exch
     PRIVATE
     TYPE (t_omi_quant) , POINTER :: quant   ! 
     TYPE (t_omi_ele)   , POINTER :: ele     ! 
     TYPE (t_omi_dope)  , POINTER :: dope(:) ! 
     INTEGER                      :: role    ! 
  END TYPE t_omi_exch
  !
  ! [C.2] Konstantwerte (Parameter) [moeglichst nicht verwenden]
  !
  !
  !! Undefined-Wert f&uuml;r INTEGER-Komponenten
  INTEGER            , PUBLIC , PARAMETER :: c_undef_omi_exch_int=-999              ! 
  !! Undefined-Wert f&uuml;r CHARACTER-Komponenten
  CHARACTER (LEN=9)  , PUBLIC , PARAMETER :: c_undef_omi_exch_char='undefined'      ! 
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
  !! Allokieren/Initialisieren der statischen Datenobjekte des Moduls; <BR>
  !! Initialisieren der statischen Modul-Daten mit Default-Werten.
  INTERFACE init_omi_exch
     MODULE PROCEDURE init_omi_exch_d ! 
  END INTERFACE
  !! De-Allokieren/De-Initialisieren der statischen Datenobjekte des Moduls; <BR>
  !! Re-Initialisieren einiger statischer Daten mit Default-Werten.
  INTERFACE clear_omi_exch
     MODULE PROCEDURE clear_omi_exch_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r PRINT-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>PRN_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>PRN_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>PRN_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_omi_exch_prn_lun
     MODULE PROCEDURE setup_omi_exch_prn_lun_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r TRACE-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>TRC_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>TRC_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>TRC_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_omi_exch_trc_lun
     MODULE PROCEDURE setup_omi_exch_trc_lun_d ! 
  END INTERFACE
  !! Index f&uuml;r Spracheinstellung setzen <BR>
  !! 1 = Deutsch (Default) <BR>
  !! 2 = Englisch         
  INTERFACE setup_omi_exch_language
     MODULE PROCEDURE setup_omi_exch_language_d ! 
  END INTERFACE
  !
  !! Erzeugen von Datenobjekten "t_omi_exch"; NULLIFY f&uuml;r dynamische 
  !! Komponenten-Felder und Initialisieren mit Default-Werten: <BR>
  !! a) ein Datenobjekt (Skalar) <BR>
  !! b) viele Datenobjekte (Vektor)
  INTERFACE new_omi_exch
     MODULE PROCEDURE new_omi_exch_0  ! 
     MODULE PROCEDURE new_omi_exch_1  ! 
  END INTERFACE
  !! Vernichten von Datenobjekten "t_omi_exch"; ggf. De-Allokieren von 
  !! Memory und teilweise Re-Initialisieren mit Default-Werten: <BR>
  !! a) ein Datenobjekt (Skalar) <BR>
  !! b) viele Datenobjekte (Vektor)
  INTERFACE kill_omi_exch
     MODULE PROCEDURE kill_omi_exch_0 ! 
     MODULE PROCEDURE kill_omi_exch_1 ! 
  END INTERFACE
  !! Pr&uuml;fen von Datenobjekten "t_omi_exch" auf G&uuml;ltigkeit: <BR>
  !! a) ein Datenobjekt (Skalar)    <BR>
  !! b) viele Datenobjekte (Vektor)
  INTERFACE ok_omi_exch
     MODULE PROCEDURE ok_omi_exch_0 ! 
     MODULE PROCEDURE ok_omi_exch_1 ! 
  END INTERFACE
  !! Drucken von Datenobjekten "t_omi_exch": <BR>
  !! a) ein Datenobjekt (Skalar) <BR>
  !! b) viele Datenobjekte (Vektor) <BR>
  !! Alle Komponenten des Typs "t_omi_ind" auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_exch
     MODULE PROCEDURE print_omi_exch_0 ! 
     MODULE PROCEDURE print_omi_exch_1 ! 
  END INTERFACE
  !! Drucken aller in diesem Modul abgelegten statischen Daten; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_exch_static
     MODULE PROCEDURE print_omi_exch_static_d ! 
  END INTERFACE
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Moduls; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_exch_all_errors
     MODULE PROCEDURE print_omi_exch_all_errors_d ! 
  END INTERFACE
  !
  !! Setze Komponente "quant" in "t_omi_exch" auf Benutzerwert
  !! a) ein Datenobjekt und eine physikalische Gr&ouml;&szlig;e    <BR>
  !! b) viele Datenobjekte und eine physikalische Gr&ouml;&szlig;e <BR>
  !! Es wird ein Zeiger auf die externen Daten eingerichtet.
  INTERFACE set_omi_exch_quant_ref
     MODULE PROCEDURE set_omi_exch_quant_ref_0_0 ! 
     MODULE PROCEDURE set_omi_exch_quant_ref_1_0 ! 
  END INTERFACE
  !! Setze Komponente "ele" in "t_omi_exch" auf Benutzerwert
  !! a) ein Datenobjekt und ein <EM>ElementSet</EM>    <BR>
  !! b) viele Datenobjekte und ein <EM>ElementSet</EM> <BR>
  !! Es wird ein Zeiger auf die externen Daten eingerichtet.
  INTERFACE set_omi_exch_ele_ref
     MODULE PROCEDURE set_omi_exch_ele_ref_0_0 ! 
     MODULE PROCEDURE set_omi_exch_ele_ref_1_0 ! 
  END INTERFACE
  !! Setze Komponente "dope" in "t_omi_exch" auf Benutzerwert
  !! a) ein Datenobjekt und ein Satz von Datenoperationen    <BR>
  !! b) viele Datenobjekte und ein Satz von Datenoperationen <BR>
  !! Es wird ein Zeiger auf die externen Daten eingerichtet.
  INTERFACE set_omi_exch_dope_ref
     MODULE PROCEDURE set_omi_exch_dope_ref_0_1 ! 
     MODULE PROCEDURE set_omi_exch_dope_ref_1_1 ! 
  END INTERFACE
  !! Setze Komponente "role" in "t_omi_exch" auf Benutzerwert <BR>
  !! a) ein Datenobjekt und ein Wert zur Verwendung der Daten <BR>
  !! b) mehrere Datenobjekte und ein Wert zur Verwendung der Daten <BR>
  !! Die Daten werden auf die interne Komponente kopiert
  INTERFACE set_omi_exch_role
     MODULE PROCEDURE set_omi_exch_role_0_0 ! 
     MODULE PROCEDURE set_omi_exch_role_1_0 ! 
  END INTERFACE
  !
  !! Index f&uuml;r Spracheinstellung ermitteln <BR>
  !! a) Deutsch (Default) <BR>
  !! b) Englisch         
  INTERFACE get_omi_exch_language
     MODULE PROCEDURE get_omi_exch_language_d ! 
  END INTERFACE
  !! Hole Komponente "quant" aus "t_omi_exch" <BR>
  !! a) f&uuml;r ein Datenobjekt    <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_exch_quant_ref
     MODULE PROCEDURE get_omi_exch_quant_ref_0_0 ! 
  END INTERFACE
  !! Hole Komponente "ele" aus "t_omi_exch" <BR>
  !! a) f&uuml;r ein Datenobjekt    <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_exch_ele_ref
     MODULE PROCEDURE get_omi_exch_ele_ref_0_0 ! 
  END INTERFACE
  !! Hole Komponente "dope(:)" aus "t_omi_exch" <BR>
  !! a) f&uuml;r ein Datenobjekt    <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_exch_dope_ref
     MODULE PROCEDURE get_omi_exch_dope_ref_0_1 ! 
  END INTERFACE
  !! Hole Komponente "role" aus "t_omi_exch" <BR>
  !!  a) f&uuml;r ein Datenobjekt    <BR>
  !!  b) f&uuml;r viele Datenobjekte <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_exch_role
     MODULE PROCEDURE get_omi_exch_role_0_0 ! 
     MODULE PROCEDURE get_omi_exch_role_1_0 ! 
  END INTERFACE
  !
  ! ... ggf. Holen fuer weitere Komponenten des Datenobjektes ergaenzen
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  !! suche nach einer bestimmten Kombination "quant_id" und "elemset_id" in einem 
  !! 1D-Feld des Typs "t_omi_exch": <BR>
  !! a) f&uuml;r eine Kombination ( <EM>quant_id</EM> / <EM>elemset_id</EM> ) <BR>
  !! b) f&uuml;r viele Kombinationen ( <EM>quant_id</EM> / <EM>elemset_id</EM> )
  INTERFACE get_omi_exch_idx
     MODULE PROCEDURE get_omi_exch_idx_1_0_0
     MODULE PROCEDURE get_omi_exch_idx_1_1_1
  END INTERFACE
  !! suche f&uuml;r eine bestimmte zu <EM>exportierende</EM> Gr&ouml;&szlig;e 
  !! (aus der Anzahl der exportierbaren Gr&ouml;&szlig;en) die entsprechende
  !! Indexposition in einem 1D-Feld von Datenobjekten <BR>
  !! a) f&uuml;r eine exportierbare Gr&ouml;&szlig;e (Nummer) <BR>
  !! b) f&uuml;r viele exportierbare Gr&ouml;&szlig;en (Nummern)
  INTERFACE get_omi_exch_exporting_idx
     MODULE PROCEDURE get_omi_exch_exporting_idx_1_0
     MODULE PROCEDURE get_omi_exch_exporting_idx_1_1
  END INTERFACE
  !! suche f&uuml;r eine bestimmte zu <EM>importierende</EM> Gr&ouml;&szlig;e 
  !! (aus der Anzahl der importierbaren Gr&ouml;&szlig;en) die entsprechende
  !! Indexposition in einem 1D-Feld von Datenobjekten <BR>
  !! a) f&uuml;r eine exportierbare Gr&ouml;&szlig;e (Nummer) <BR>
  !! b) f&uuml;r viele exportierbare Gr&ouml;&szlig;en (Nummern)
  INTERFACE get_omi_exch_importing_idx
     MODULE PROCEDURE get_omi_exch_importing_idx_1_0
     MODULE PROCEDURE get_omi_exch_importing_idx_1_1
  END INTERFACE
  !! suche f&uuml;r eine bestimmte physikalische Gr&ouml;&szlig;e (anhand der
  !! laufenden Nummer) deren erstmaliges Auftreten in einem 1D-Feld des Typs
  !! "t_omi_exch" <BR>
  !! a) f&uuml;r eine laufende Nummer <BR>
  !! b) f&uuml;r viele laufende Nummern <BR>
  !! c) f&uuml;r eine "quant_id" <BR>
  !! d) f&uuml;r viele "quant_ids"
  INTERFACE get_omi_exch_quant_idx
     MODULE PROCEDURE get_omi_exch_quant_idx_1_0
     MODULE PROCEDURE get_omi_exch_quant_idx_1_1
     MODULE PROCEDURE get_omi_exch_quant_id_idx_1_0
     MODULE PROCEDURE get_omi_exch_quant_id_idx_1_1
  END INTERFACE
  !! suche f&uuml;r ein bestimmtes <EM>ElementSet</EM> (anhand der laufenden 
  !! Nummer) deren erstmaliges Auftreten in einem 1D-Feld des Typs "t_omi_exch" <BR>
  !! a) f&uuml;r eine laufende Nummer <BR>
  !! b) f&uuml;r viele laufende Nummern <BR>
  !! c) f&uuml;r eine "elemset_id" <BR>
  !! d) f&uuml;r viele "elemset_ids"
  INTERFACE get_omi_exch_ele_idx
     MODULE PROCEDURE get_omi_exch_ele_idx_1_0
     MODULE PROCEDURE get_omi_exch_ele_idx_1_1
     MODULE PROCEDURE get_omi_exch_ele_id_idx_1_0
     MODULE PROCEDURE get_omi_exch_ele_id_idx_1_1
  END INTERFACE
  !! ermittle die Anzahl der exportierbaren Austauschgr&ouml;&szlig;en in einem
  !! 1D-Feld des Typs "t_omi_exch"
  INTERFACE get_omi_exch_exporting_count
     MODULE PROCEDURE get_omi_exch_exporting_count_1
  END INTERFACE
  !! ermittle die Anzahl der importierbaren Austauschgr&ouml;&szlig;en in einem
  !! 1D-Feld des Typs "t_omi_exch"
  INTERFACE get_omi_exch_importing_count
     MODULE PROCEDURE get_omi_exch_importing_count_1
  END INTERFACE
  !! ermittle die Anzahl unterschiedlicher physikalischer Gr&ouml;&szlig;en 
  !! (<EM>Quantities</EM>) in einem 1D-Feld des Typs "t_omi_exch"
  INTERFACE get_omi_exch_quant_count
     MODULE PROCEDURE get_omi_exch_quant_count_1
  END INTERFACE
  !! ermittle die Anzahl unterschiedlicher <EM>ElementSets</EM> in einem 
  !! 1D-Feld des Typs "t_omi_exch"
  INTERFACE get_omi_exch_ele_count
     MODULE PROCEDURE get_omi_exch_ele_count_1
  END INTERFACE
  !! ermittle die Anzahl der Daten-Punkte der Austauschgr&ouml;&szlig;e <BR>
  !! a) f&uuml;r ein Datenobjekt <BR>
  !! b) f&uuml;r viele Datenobjekte
  INTERFACE get_omi_exch_values_count
     MODULE PROCEDURE get_omi_exch_values_count_0
     MODULE PROCEDURE get_omi_exch_values_count_1
  END INTERFACE
  !! pr&uuml;fe, ob eine Austauschgr&ouml;&szlig;e eine Export-Gr&ouml;&szlig;e ist <BR>
  !! a) f&uuml;r ein Datenobjekt <BR>
  !! b) f&uuml;r viele Datenobjekte
  INTERFACE is_omi_exch_exporting
     MODULE PROCEDURE is_omi_exch_exporting_0
     MODULE PROCEDURE is_omi_exch_exporting_1
  END INTERFACE
  !! pr&uuml;fe, ob eine Austauschgr&ouml;&szlig;e eine Import-Gr&ouml;&szlig;e ist <BR>
  !! a) f&uuml;r ein Datenobjekt <BR>
  !! b) f&uuml;r viele Datenobjekte
  INTERFACE is_omi_exch_importing
     MODULE PROCEDURE is_omi_exch_importing_0
     MODULE PROCEDURE is_omi_exch_importing_1
  END INTERFACE
  !! pr&uuml;fe, ob eine Austauschgr&ouml;szlig;e eine skalare Gr&ouml;&szlig;e ist <BR>
  !! a) f&uuml;r ein Datenobjekt <BR>
  !! b) f&uuml;r viele Datenobjekte
  INTERFACE is_omi_exch_scalar
     MODULE PROCEDURE is_omi_exch_scalar_0
     MODULE PROCEDURE is_omi_exch_scalar_1
  END INTERFACE
  !! pr&uuml;fe, ob eine Austauschgr&ouml;szlig;e eine vektorielle Gr&ouml;&szlig;e ist <BR>
  !! a) f&uuml;r ein Datenobjekt <BR>
  !! b) f&uuml;r viele Datenobjekte
  INTERFACE is_omi_exch_vector
     MODULE PROCEDURE is_omi_exch_vector_0
     MODULE PROCEDURE is_omi_exch_vector_1
  END INTERFACE
  !
  !! Kopiere den Inhalt einer Variablen des Typs "t_omi_exch" in
  !! ein anderes Objekt vom gleichen Typ <BR>
  !! a) ein Quell-Objekt wird in ein Ziel-Objekt kopiert <BR>
  !! a) mehrere Quell-Objekte werden auf mehrere Ziel-Objekte kopiert
  INTERFACE copy_omi_exch
     MODULE PROCEDURE copy_omi_exch_0_0
     MODULE PROCEDURE copy_omi_exch_1_1
  END INTERFACE
  !
  !! Pr&uuml;fen zweier Datenobjekte "t_omi_exch" auf Gleichheit (Funktion) <BR>
  !! a) Skalar1 == Skalar2 <BR>
  !! b) Skalar1 == Vektor2 <BR>
  !! c) Vektor1 == Skalar2 <BR>
  !! d) Vektor1 == Vektor2
  INTERFACE eq_omi_exch
     MODULE PROCEDURE eq_omi_exch_0_0  ! 
     MODULE PROCEDURE eq_omi_exch_0_1  ! 
     MODULE PROCEDURE eq_omi_exch_1_0  ! 
     MODULE PROCEDURE eq_omi_exch_1_1  ! 
  END INTERFACE
  !! Pr&uuml;fen zweier Datenobjekte "t_omi_exch" auf Ungleichheit (Funtion) <BR>
  !! a) Skalar1 /= Skalar2 <BR>
  !! b) Skalar1 /= Vektor2 <BR>
  !! c) Vektor1 /= Skalar2 <BR>
  !! d) Vektor1 /= Vektor2
  INTERFACE ne_omi_exch
     MODULE PROCEDURE ne_omi_exch_0_0  ! 
     MODULE PROCEDURE ne_omi_exch_0_1  ! 
     MODULE PROCEDURE ne_omi_exch_1_0  ! 
     MODULE PROCEDURE ne_omi_exch_1_1  ! 
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
  PUBLIC :: init_omi_exch
  PUBLIC :: clear_omi_exch
  PUBLIC :: setup_omi_exch_prn_lun
  PUBLIC :: setup_omi_exch_trc_lun
  PUBLIC :: setup_omi_exch_language
  PUBLIC :: new_omi_exch
  PUBLIC :: kill_omi_exch
  PUBLIC :: ok_omi_exch
  PUBLIC :: print_omi_exch
  PUBLIC :: print_omi_exch_static
  PUBLIC :: print_omi_exch_all_errors
  PUBLIC :: set_omi_exch_quant_ref
  PUBLIC :: set_omi_exch_ele_ref
  PUBLIC :: set_omi_exch_dope_ref
  PUBLIC :: set_omi_exch_role
  PUBLIC :: get_omi_exch_language
  PUBLIC :: get_omi_exch_quant_ref
  PUBLIC :: get_omi_exch_ele_ref
  PUBLIC :: get_omi_exch_dope_ref
  PUBLIC :: get_omi_exch_role
  PUBLIC :: eq_omi_exch
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: get_omi_exch_idx 
  PUBLIC :: get_omi_exch_exporting_idx
  PUBLIC :: get_omi_exch_importing_idx
  PUBLIC :: get_omi_exch_quant_idx
  PUBLIC :: get_omi_exch_ele_idx
  PUBLIC :: get_omi_exch_exporting_count
  PUBLIC :: get_omi_exch_importing_count
  PUBLIC :: get_omi_exch_quant_count 
  PUBLIC :: get_omi_exch_ele_count 
  PUBLIC :: get_omi_exch_values_count 
  PUBLIC :: is_omi_exch_exporting
  PUBLIC :: is_omi_exch_importing
  PUBLIC :: is_omi_exch_scalar
  PUBLIC :: is_omi_exch_vector
  PUBLIC :: copy_omi_exch
  PUBLIC :: ne_omi_exch
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
  CHARACTER (LEN=10), PARAMETER :: c_modname      = 'b_omi_exch' ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.          ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1               ! 
  !! Anzahl der Datenkomponenten des Typs t_omi_exch
  INTEGER           , PARAMETER :: c_nofcomp      =  4               ! ggf. modifizieren
  !! Anzahl einstellbarer Sprachen
  INTEGER           , PARAMETER :: c_max_language = 2                ! [Deutsch,Englisch]
  !! Default-Language
  INTEGER           , PARAMETER :: c_def_language = 1                ! [Deutsch]
  !! Code zur Kennzeichnung exportierender Daten
  INTEGER           , PARAMETER :: c_export_int=1 ! 
  !! Code zur Kennzeichnung importierender Daten
  INTEGER           , PARAMETER :: c_import_int=2 ! 
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
  SUBROUTINE init_omi_exch_d &
       ( )
    !
    USE b_error, ONLY : DEBUG_b
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='init_omi_exch_d' 
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_b > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "b_omi_exch" version 2.1 of 07/21/05                 '
          WRITE(*,*) ' Copyright (C) 2005 Bundesanstalt fuer Wasserbau   '
          WRITE(*,*)
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       IF ( no_error( ) ) CALL init_omi_quant ( )
       IF ( no_error( ) ) CALL init_omi_ele   ( )
       IF ( no_error( ) ) CALL init_omi_dope  ( )
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_omi_exch_all_errors ( ) 
       ! [1.5] Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.6] endgueltiges Setzen des Initialisierungs-Indikators
       initialised = MERGE( .true., .false., no_error( ) )
    END IF
    ! 2.0 Initialisierungszaehler heraufsetzen
    n_init = n_init + 1
    !
  END SUBROUTINE init_omi_exch_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_omi_exch_d &
       ( )
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER :: c_upname='clear_omi_exch_d' ! 
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_omi_exch_all_errors ( ) 
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.3] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.4] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.4.1] ggf. weitere Module de-initialisieren
       IF ( no_error( ) ) CALL clear_omi_dope  ( )
       IF ( no_error( ) ) CALL clear_omi_ele   ( )
       IF ( no_error( ) ) CALL clear_omi_quant ( )
       ! [1.4.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
    !
  END SUBROUTINE clear_omi_exch_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_omi_exch_prn_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='setup_omi_exch_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun     ( lun )
       IF ( no_error( ) ) CALL setup_omi_quant_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_omi_ele_prn_lun   ( lun )
       IF ( no_error( ) ) CALL setup_omi_dope_prn_lun  ( lun )
    END IF
    !
  END SUBROUTINE setup_omi_exch_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_omi_exch_trc_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='setup_omi_exch_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun     ( lun )
       IF ( no_error( ) ) CALL setup_omi_quant_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_omi_ele_trc_lun   ( lun )
       IF ( no_error( ) ) CALL setup_omi_dope_trc_lun  ( lun )
    END IF
    !
  END SUBROUTINE setup_omi_exch_trc_lun_d
  !
  !! Setzen des Index f&uuml;r die Spracheinstellung <BR>
  !! 1 = Deutsch  <BR>
  !! 2 = Englisch 
  SUBROUTINE setup_omi_exch_language_d ( var )
    !! Index f&uuml;r Spracheinstellung (1 = Deutsch, 2 = Englisch )
    INTEGER , INTENT(IN) :: var ! 
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER :: c_upname='setup_omi_exch_language_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       language = MERGE ( var, c_def_language, ( 1 <= var .AND. var <= c_max_language ) )
       IF ( no_error( ) ) CALL setup_omi_quant_language ( language ) 
       IF ( no_error( ) ) CALL setup_omi_ele_language   ( language ) 
    END IF
    !
  END SUBROUTINE setup_omi_exch_language_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_omi_exch_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_exch) , INTENT(OUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='new_omi_exch_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       !
       this%role = c_undef_omi_exch_int
       !
       NULLIFY ( this%quant )
       NULLIFY ( this%ele   )
       NULLIFY ( this%dope  )
       !
    END IF
    !
  END SUBROUTINE new_omi_exch_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_omi_exch_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch) , INTENT(OUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='new_omi_exch_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL new_omi_exch_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_omi_exch_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_omi_exch_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_exch) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='kill_omi_exch_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       IF ( no_error( ) ) CALL new_omi_exch_0 ( this )
    END IF
    !
  END SUBROUTINE kill_omi_exch_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_omi_exch_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='kill_omi_exch_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL kill_omi_exch_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_omi_exch_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_exch_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_exch) , INTENT(IN) :: this ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=13), PARAMETER :: c_upname='ok_omi_exch_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok(1)  = ok_omi_exch_quant ( this )
       l_ok(2)  = ok_omi_exch_ele   ( this )
       l_ok(3)  = ok_omi_exch_dope  ( this )
       l_ok(4)  = ok_omi_exch_role  ( this )
    END IF
    !
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_omi_exch_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_exch_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=13), PARAMETER :: c_upname='ok_omi_exch_1' 
    !! Z&auml;hler      
    INTEGER :: i ! 
    !
    ok = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) ) EXIT
          ok(i) = ok_omi_exch_0 ( this(i) )
       END DO
    END IF
    !
  END FUNCTION ok_omi_exch_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_exch_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_exch) , INTENT(IN) :: this ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='print_omi_exch_0' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7001, c_upname, c_modname, stat )
       IF ( no_error( ) ) CALL print_omi_exch_quant ( this )
       IF ( no_error( ) ) CALL print_omi_exch_ele   ( this )
       IF ( no_error( ) ) CALL print_omi_exch_dope  ( this )
       IF ( no_error( ) ) CALL print_omi_exch_role  ( this )
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7002, c_upname, c_modname, stat )
       END IF
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT('# Beginn Objekt t_omi_exch ------------------------------')
8001 FORMAT('# Ende   Objekt t_omi_exch ------------------------------')
    !
  END SUBROUTINE print_omi_exch_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_exch_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this(:) ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='print_omi_exch_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) i
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7003, c_upname, c_modname, stat )
          IF ( no_error( ) ) CALL print_omi_exch_0 ( this(i) )
       END DO
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT ('# Datenobjekt-Index i = ',I10.10,' ---------------------------')
    !
  END SUBROUTINE print_omi_exch_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_exch_static_d ( )
    !! Name der Function
    CHARACTER (LEN=23), PARAMETER :: c_upname='print_omi_exch_static_d' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) &
            initialised, prn_op, trc_op, prn_lun, trc_lun, n_init, language, &
            c_export_int, c_import_int, c_undef_omi_exch_char, c_undef_omi_exch_int
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       IF ( no_error( ) ) CALL print_omi_exch_all_errors_d ( )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls b_omi_exch         ',/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
    '# initialised = ',L1,/ &
    '#      prn_op = ',L1,/ &
    '#      trc_op = ',L1,/ &
    '#     prn_lun = ',I5,/ &
    '#     trc_lun = ',I5,/ &
    '#      n_init = ',I5,/ &
    '#    language = ',I5,/ &
    '# ExportCode  = ',I5,/ &
    '# ImportCode  = ',I5,/ &
    '# undef[char] = ',A,/ &
    '#  undef[int] = ',I10,/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
    '#------------------------------------------------------------') 
    !
  END SUBROUTINE print_omi_exch_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_exch_all_errors_d ( )
    !! Name der Function
    CHARACTER (LEN=27), PARAMETER :: c_upname='print_omi_exch_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_omi_exch_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! weise der Komponente "quant" in einem Datenobjekt einen Wert zu <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die externen Daten eingerichtet
  SUBROUTINE set_omi_exch_quant_ref_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_exch)  , INTENT(INOUT) :: this ! 
    !! Zeiger f&uuml;r Komponente "quant"
    TYPE (t_omi_quant) , POINTER       :: val  ! 
    !
    this%quant => val
    !
  END SUBROUTINE set_omi_exch_quant_ref_0_0
  !
  !! weise der Komponente "quant" in mehrerer Datenobjekten einen Wert zu <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die externen Daten eingerichtet
  SUBROUTINE set_omi_exch_quant_ref_1_0 ( this, val )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_exch)  , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "quant"
    TYPE (t_omi_quant) , POINTER       :: val     ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_omi_exch_quant_ref_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_omi_exch_quant_ref_1_0
  !
  !! weise der Komponente "ele" in einem Datenobjekt einen Wert zu <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die externen Daten eingerichtet
  SUBROUTINE set_omi_exch_ele_ref_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_exch) , INTENT(INOUT) :: this ! 
    !! Zeiger f&uuml;r Komponente "ele"
    TYPE (t_omi_ele)  , POINTER       :: val  ! 
    !
    this%ele => val
    !
  END SUBROUTINE set_omi_exch_ele_ref_0_0
  !
  !! weise der Komponente "ele" in mehrerer Datenobjekten einen Wert zu <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die externen Daten eingerichtet
  SUBROUTINE set_omi_exch_ele_ref_1_0 ( this, val )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_exch) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "ele"
    TYPE (t_omi_ele)  , POINTER       :: val     ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_omi_exch_ele_ref_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_omi_exch_ele_ref_1_0
  !
  !! weise der Komponente "dope(:)" in einem Datenobjekt ein Feld zu <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die externen Daten eingerichtet
  SUBROUTINE set_omi_exch_dope_ref_0_1 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_exch) , INTENT(INOUT) :: this   ! 
    !! Zeiger f&uuml;r Komponente "dope(:)"
    TYPE (t_omi_dope) , POINTER       :: val(:) ! 
    !
    this%dope => val
    !
  END SUBROUTINE set_omi_exch_dope_ref_0_1
  !
  !! weise der Komponente "dope(:)" in mehrerer Datenobjekten ein Feld zu <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die externen Daten eingerichtet
  SUBROUTINE set_omi_exch_dope_ref_1_1 ( this, val )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_exch) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "dope(:)"
    TYPE (t_omi_dope) , POINTER       :: val(:)  ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_omi_exch_dope_ref_0_1 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_omi_exch_dope_ref_1_1
  !
  !! weise der Komponente "role" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_exch_role_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_exch) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "role"
    INTEGER           , INTENT(IN)    :: val  ! 
    !
    this%role = val
    !
  END SUBROUTINE set_omi_exch_role_0_0
  !
  !! weise der Komponente "role" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_exch_role_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "role"
    INTEGER           , INTENT(IN)    :: val     ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_omi_exch_role_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_omi_exch_role_1_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! Holen des Index f&uuml;r die Spracheinstellung <BR>
  !! 1 = Deutsch  <BR>
  !! 2 = Englisch 
  FUNCTION get_omi_exch_language_d ( ) &
       RESULT( res )
    !! R&uuml;ckgabewert: 
    !! Index f&uuml;r Spracheinstellung (1 = Deutsch, 2 = Englisch )
    INTEGER :: res ! 
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER :: c_upname='get_omi_exch_language_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       res = language
    ELSE
       res = -1
    END IF
    !
  END FUNCTION get_omi_exch_language_d
  !
  !! hole die Komponente "quant" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_exch_quant_ref_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_exch)  , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "quant" (Skalar)
    TYPE (t_omi_quant) , POINTER     :: val  ! 
    !
    val => this%quant
    !
  END FUNCTION get_omi_exch_quant_ref_0_0
  !
  !! hole die Komponente "ele" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_exch_ele_ref_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_exch) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert "ele" (Skalar)
    TYPE (t_omi_ele)  , POINTER    :: val  ! 
    !
    val => this%ele
    !
  END FUNCTION get_omi_exch_ele_ref_0_0
  !
  !! hole die Komponente "dope(:)" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_exch_dope_ref_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_exch) , INTENT(IN) :: this   ! 
    !! R&uuml;ckgabewert "dope(:)" (Skalar)
    TYPE (t_omi_dope) , POINTER    :: val(:) ! 
    !
    val => this%dope
    !
  END FUNCTION get_omi_exch_dope_ref_0_1
  !
  !! hole die Komponente "role" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_exch_role_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_exch) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "role" (Skalar)
    INTEGER                         :: val  ! 
    !
    val = this%role
    !
  END FUNCTION get_omi_exch_role_0_0
  !
  !! hole die Komponente "role" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_exch_role_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this(:) ! 
    !! R&uuml;ckgabewert "role"
    INTEGER                        :: val(SIZE(this))  ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(val)
       val(i) = get_omi_exch_role_0_0 ( this(i) )
    END DO
    !
  END FUNCTION get_omi_exch_role_1_0
  !
  !! suche nach einer bestimmten Kombination "quant_id" und "elemset_id" in einem 
  !! 1D-Feld des Typs "t_omi_exch" f&uuml;r eine Kombination 
  !! ( <EM>quant_id</EM> / <EM>elemset_id</EM> ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  FUNCTION get_omi_exch_idx_1_0_0 ( this, quant_id, ele_id ) &
       RESULT ( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this(:)  ! 
    !! ein-endeutige Kurzbezeichnung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: quant_id ! 
    !! ein-endeutige Kurzbezeichnung des <EM>ElementSet</EM>
    CHARACTER (LEN=*) , INTENT(IN) :: ele_id   ! 
    !! Ergebnis: Zeiger auf einen Eintrag in this(:), deren aktuelle Werte
    !! der angeforderten Kombination entsprechen <BR>
    !! wird kein Eintrag gefunden, so wird "undefiniert" zur&uuml;ckgegeben
    INTEGER :: res ! 
    !! Hilfsvariable
    CHARACTER (LEN=c_len_omi_quant_id) :: l_quant_id ! 
    CHARACTER (LEN=c_len_omi_ele_id)   :: l_ele_id   ! 
    INTEGER :: i, lq(2), le(2) ! 
    !
    res   = c_undef_omi_exch_int
    lq(1) = LEN_TRIM(quant_id)
    le(1) = LEN_TRIM(ele_id)
    !
    DO i=1,SIZE(this)
       IF ( res /= c_undef_omi_exch_int ) EXIT
       IF ( ASSOCIATED( this(i)%quant ) .AND. ASSOCIATED( this(i)%ele ) ) THEN
          l_quant_id = get_omi_quant_id ( this(i)%quant )
          l_ele_id   = get_omi_ele_id   ( this(i)%ele   )
          lq(2)      = LEN_TRIM(l_quant_id)
          le(2)      = LEN_TRIM(l_ele_id)
          IF ( ALL(lq==lq(1)) .AND. ALL(le==le(1)) ) THEN
             IF ( quant_id(1:lq(1)) == l_quant_id(1:lq(2)) .AND. &
                    ele_id(1:le(1)) ==   l_ele_id(1:le(2)) ) res = i
          END IF
       END IF
    END DO
    !
  END FUNCTION get_omi_exch_idx_1_0_0
  !
  !! suche nach einer bestimmten Kombination "quant_id" und "elemset_id" in einem 
  !! 1D-Feld des Typs "t_omi_exch" f&uuml;r viele Kombinationen 
  !! ( <EM>quant_id</EM> / <EM>elemset_id</EM> ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  FUNCTION get_omi_exch_idx_1_1_1 ( this, quant_id, ele_id ) &
       RESULT ( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this(:)     ! 
    !! ein-endeutige Kurzbezeichnung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: quant_id(:) ! 
    !! ein-endeutige Kurzbezeichnung des <EM>ElementSet</EM>
    CHARACTER (LEN=*) , INTENT(IN) :: ele_id(:)   ! 
    !! Ergebnis: Zeiger auf einen Eintrag in this(:), deren aktuelle Werte
    !! der angeforderten Kombination entsprechen <BR>
    !! wird kein Eintrag gefunden, so wird "undefiniert" zur&uuml;ckgegeben
    INTEGER :: res(MIN(SIZE(quant_id),SIZE(ele_id))) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_exch_idx_1_0_0 ( this, quant_id(i), ele_id(i) )
    END DO
    !
  END FUNCTION get_omi_exch_idx_1_1_1
  !
  !! Ermittle die Anzahl der exportierbaren Austauschgr&ouml;&szlig;en 
  !! in einem Feld von Datenobjekten <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_exch_exporting_count_1 ( this ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this(:) ! 
    !! Ergebnis : Anzahl der exportierbaren Austauschgr&ouml;&szlig;en
    INTEGER :: res ! 
    !! Hilfsvariable
    INTEGER :: i   ! 
    !
    res = 0
    DO i=1,SIZE(this)
       IF ( is_omi_exch_exporting( this(i) ) ) res = res + 1
    END DO
    !
  END FUNCTION get_omi_exch_exporting_count_1
  !
  !! Ermittle die Anzahl der importierbaren Austauschgr&ouml;&szlig;en 
  !! in einem Feld von Datenobjekten <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_exch_importing_count_1 ( this ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this(:) ! 
    !! Ergebnis : Anzahl der importierbaren Austauschgr&ouml;&szlig;en
    INTEGER :: res ! 
    !! Hilfsvariable
    INTEGER :: i   ! 
    !
    res = 0
    DO i=1,SIZE(this)
       IF ( is_omi_exch_importing( this(i) ) ) res = res + 1
    END DO
    !
  END FUNCTION get_omi_exch_importing_count_1
  !
  !! ermittle die Anzahl unterschiedlicher physikalischer Gr&ouml;&szlig;en in 
  !! einem 1D-Feld des Typs "t_omi_exch" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_exch_quant_count_1 ( this ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Anzahl unterschiedlicher <EM>Quantities</EM>
    INTEGER :: res ! 
    !! Hilfsvariablen
    INTEGER :: i   ! 
    CHARACTER (LEN=c_len_omi_quant_id) , ALLOCATABLE :: id(:) ! 
    LOGICAL                            , ALLOCATABLE :: ld(:) ! 
    !
    ALLOCATE( id(SIZE(this)), ld(SIZE(this)) )
    DO i=1,SIZE(id)
       id(i) = get_omi_quant_id ( this(i)%quant )
    END DO
    ld(:) = is_dif_string_1 ( id )
    res   = COUNT( ld(:) )
    DEALLOCATE( id, ld )
    !
  END FUNCTION get_omi_exch_quant_count_1
  !
  !! ermittle die Anzahl unterschiedlicher <EM>ElementSets</EM> in einem 
  !! 1D-Feld des Typs "t_omi_exch" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_exch_ele_count_1 ( this ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Anzahl unterschiedlicher <EM>ElementSets</EM>
    INTEGER :: res ! 
    !! Hilfsvariablen
    INTEGER :: i   ! 
    CHARACTER (LEN=c_len_omi_ele_id) , ALLOCATABLE :: id(:) ! 
    LOGICAL                          , ALLOCATABLE :: ld(:) ! 
    !
    ALLOCATE( id(SIZE(this)), ld(SIZE(this)) )
    DO i=1,SIZE(id)
       id(i) = get_omi_ele_id ( this(i)%ele )
    END DO
    ld(:) = is_dif_string_1 ( id )
    res   = COUNT( ld(:) )
    DEALLOCATE( id, ld )
    !
  END FUNCTION get_omi_exch_ele_count_1
  !
  !! Ermittle die Indexposition einer zu exportierenden Gr&ouml;&szlig;e <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_exch_exporting_idx_1_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this(:) ! 
    !! laufende Nummer der zu exportierenden Gr&ouml;&szlig;e
    INTEGER           , INTENT(IN) :: val     ! 
    !! Ergebnis: Zeiger auf einen Eintrag in this(:), welcher der
    !! val-ten zu exportierenden Gr&ouml;&szlig;e entspricht <BR>
    !! wird kein Eintrag gefunden, so wird "undefiniert" zur&uuml;ckgegeben
    INTEGER :: res  ! 
    !! Hilfsvariable
    INTEGER :: i, n ! 
    !
    res = c_undef_omi_exch_int
    n   = 0
    IF ( val > 0 ) THEN
       DO i=1,SIZE(this)
          IF ( res /= c_undef_omi_exch_int ) EXIT
          IF ( is_omi_exch_exporting ( this(i) ) ) n = n + 1
          IF ( n == val ) res = i
       END DO
    END IF
    !
  END FUNCTION get_omi_exch_exporting_idx_1_0
  !
  !! Ermittle die Indexpositionen vieler zu exportierenden Gr&ouml;&szlig;en <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_exch_exporting_idx_1_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this(:) ! 
    !! laufende Nummern der zu exportierenden Gr&ouml;&szlig;en
    INTEGER           , INTENT(IN) :: val(:)  ! 
    !! Ergebnis: Zeiger auf die Eintr&auml;ge in this(:), welche den
    !! val(:)-ten zu exportierenden Gr&ouml;&szlig;en entsprechen <BR>
    !! wird kein Eintrag gefunden, so wird "undefiniert" zur&uuml;ckgegeben
    INTEGER :: res(SIZE(val))  ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_exch_exporting_idx_1_0 ( this, val(i) )
    END DO
    !
  END FUNCTION get_omi_exch_exporting_idx_1_1
  !
  !! Ermittle die Indexposition einer zu importierenden Gr&ouml;&szlig;e <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_exch_importing_idx_1_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this(:) ! 
    !! laufende Nummer der zu importierenden Gr&ouml;&szlig;e
    INTEGER           , INTENT(IN) :: val     ! 
    !! Ergebnis: Zeiger auf einen Eintrag in this(:), welcher der
    !! val-ten zu importierenden Gr&ouml;&szlig;e entspricht <BR>
    !! wird kein Eintrag gefunden, so wird "undefiniert" zur&uuml;ckgegeben
    INTEGER :: res  ! 
    !! Hilfsvariable
    INTEGER :: i, n ! 
    !
    res = c_undef_omi_exch_int
    n   = 0
    IF ( val > 0 ) THEN
       DO i=1,SIZE(this)
          IF ( res /= c_undef_omi_exch_int ) EXIT
          IF ( is_omi_exch_importing ( this(i) ) ) n = n + 1
          IF ( n == val ) res = i
       END DO
    END IF
    !
  END FUNCTION get_omi_exch_importing_idx_1_0
  !
  !! Ermittle die Indexpositionen vieler zu importierenden Gr&ouml;&szlig;en <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_exch_importing_idx_1_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this(:) ! 
    !! laufende Nummern der zu importierenden Gr&ouml;&szlig;en
    INTEGER           , INTENT(IN) :: val(:)  ! 
    !! Ergebnis: Zeiger auf die Eintr&auml;ge in this(:), welche den
    !! val(:)-ten zu importierenden Gr&ouml;&szlig;en entsprechen <BR>
    !! wird kein Eintrag gefunden, so wird "undefiniert" zur&uuml;ckgegeben
    INTEGER :: res(SIZE(val))  ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_exch_importing_idx_1_0 ( this, val(i) )
    END DO
    !
  END FUNCTION get_omi_exch_importing_idx_1_1
  !
  !! Ermittle die Indexposition f&uuml;r das erstmalige Auftreten einer 
  !! physikalischen Gr&ouml;&szlig;e anhand der laufenden Nummer <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_exch_quant_idx_1_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this(:) ! 
    !! laufende Nummer der physikalischen Gr&ouml;&szlig;e
    INTEGER           , INTENT(IN) :: val     ! 
    !! Ergebnis: Zeiger auf einen Eintrag in this(:), welcher der
    !! val-ten physikalischen Gr&ouml;&szlig;e entspricht <BR>
    !! wird kein Eintrag gefunden, so wird "undefiniert" zur&uuml;ckgegeben
    INTEGER :: res  ! 
    !! Hilfsvariable
    INTEGER :: i, n ! 
    CHARACTER (LEN=c_len_omi_quant_id) , ALLOCATABLE :: id(:) ! 
    LOGICAL                            , ALLOCATABLE :: ld(:) ! 
    !
    res = c_undef_omi_exch_int
    n   = 0
    IF ( val > 0 ) THEN
       ALLOCATE( id(SIZE(this)), ld(SIZE(this)) )
       DO i=1,SIZE(id)
          id(i) = get_omi_quant_id ( this(i)%quant )
       END DO
       ld(:) = is_dif_string_1 ( id )
       DO i=1,SIZE(id)
          IF ( res /= c_undef_omi_exch_int ) EXIT
          IF ( ld(i) ) n = n + 1
          IF ( val == n ) res = i
       END DO
       DEALLOCATE( id, ld )
    END IF
    !
  END FUNCTION get_omi_exch_quant_idx_1_0
  !
  !! Ermittle die Indexpositionen f&uuml;r das erstmalige Auftreten 
  !! verschiedener physikalischer Gr&ouml;&szlig;en anhand der 
  !! laufenden Nummern <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_exch_quant_idx_1_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this(:) ! 
    !! laufende Nummern der physikalischen Gr&ouml;&szlig;en
    INTEGER           , INTENT(IN) :: val(:)  ! 
    !! Ergebnis: Zeiger auf die Eintr&auml;ge in this(:), welche den
    !! val(:)-ten physikalischen Gr&ouml;&szlig;en entsprechen <BR>
    !! wird kein Eintrag gefunden, so wird "undefiniert" zur&uuml;ckgegeben
    INTEGER :: res(SIZE(val))  ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_exch_quant_idx_1_0 ( this, val(i) )
    END DO
    !
  END FUNCTION get_omi_exch_quant_idx_1_1
  !
  !! Ermittle die Indexposition f&uuml;r das erstmalige Auftreten einer 
  !! physikalischen Gr&ouml;&szlig;e anhand der "quant_id" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_exch_quant_id_idx_1_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this(:) ! 
    !! "quant_id" der zu suchenden physikalischen Gr&ouml&szlig;e
    CHARACTER (LEN=*) , INTENT(IN) :: val     ! 
    !! Ergebnis: Zeiger auf einen Eintrag in this(:), welcher der
    !! "quant_id" val entspricht <BR>
    !! wird kein Eintrag gefunden, so wird "undefiniert" zur&uuml;ckgegeben
    INTEGER :: res       ! 
    !! Hilfsvariable
    CHARACTER (LEN=c_len_omi_quant_id) :: id ! 
    INTEGER                            :: i, l1, l2 ! 
    !
    res = c_undef_omi_exch_int
    l1  = LEN_TRIM(val)
    IF ( l1 > 0 ) THEN
       DO i=1,SIZE(this)
          IF ( res /= c_undef_omi_exch_int ) EXIT
          id = get_omi_quant_id ( this(i)%quant )
          l2 = LEN_TRIM(id)
          IF ( l1 == l2 ) THEN
             IF ( val(1:l1) == id(1:l2) ) res = i
          END IF
       END DO
    END IF
    !
  END FUNCTION get_omi_exch_quant_id_idx_1_0
  !
  !! Ermittle die Indexpositionen f&uuml;r das erstmalige Auftreten 
  !! verschiedener physikalischer Gr&ouml;&szlig;en anhand der "quant_ids" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_exch_quant_id_idx_1_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this(:) ! 
    !! "quant_ids" der physikalischen Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: val(:)  ! 
    !! Ergebnis: Zeiger auf die Eintr&auml;ge in this(:), welche den
    !! "quant_ids" val(:) entsprechen <BR>
    !! wird kein Eintrag gefunden, so wird "undefiniert" zur&uuml;ckgegeben
    INTEGER :: res(SIZE(val))  ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_exch_quant_id_idx_1_0 ( this, val(i) )
    END DO
    !
  END FUNCTION get_omi_exch_quant_id_idx_1_1
  !
  !! Ermittle die Indexposition f&uuml;r das erstmalige Auftreten eines
  !! <EM>ElementSet</EM> anhand der laufenden Nummer <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_exch_ele_idx_1_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this(:) ! 
    !! laufende Nummer des <EM>ElementSet</EM>
    INTEGER           , INTENT(IN) :: val     ! 
    !! Ergebnis: Zeiger auf einen Eintrag in this(:), welcher dem
    !! val-ten <EM>ElementSet</EM> entspricht <BR>
    !! wird kein Eintrag gefunden, so wird "undefiniert" zur&uuml;ckgegeben
    INTEGER :: res  ! 
    !! Hilfsvariable
    INTEGER :: i, n ! 
    CHARACTER (LEN=c_len_omi_ele_id) , ALLOCATABLE :: id(:) ! 
    LOGICAL                          , ALLOCATABLE :: ld(:) ! 
    !
    res = c_undef_omi_exch_int
    n   = 0
    IF ( val > 0 ) THEN
       ALLOCATE( id(SIZE(this)), ld(SIZE(this)) )
       DO i=1,SIZE(id)
          id(i) = get_omi_ele_id ( this(i)%ele )
       END DO
       ld(:) = is_dif_string_1 ( id )
       DO i=1,SIZE(id)
          IF ( res /= c_undef_omi_exch_int ) EXIT
          IF ( ld(i) ) n = n + 1
          IF ( val == n ) res = i
       END DO
       DEALLOCATE( id, ld )
    END IF
    !
  END FUNCTION get_omi_exch_ele_idx_1_0
  !
  !! Ermittle die Indexpositionen f&uuml;r das erstmalige Auftreten 
  !! verschiedener <EM>ElementSets</EM> anhand der laufenden Nummern <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_exch_ele_idx_1_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this(:) ! 
    !! laufende Nummern der <EM>ElementSets</EM>
    INTEGER           , INTENT(IN) :: val(:)  ! 
    !! Ergebnis: Zeiger auf die Eintr&auml;ge in this(:), welche den
    !! val(:)-ten <EM>ElementSets</EM> entsprechen <BR>
    !! wird kein Eintrag gefunden, so wird "undefiniert" zur&uuml;ckgegeben
    INTEGER :: res(SIZE(val))  ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_exch_ele_idx_1_0 ( this, val(i) )
    END DO
    !
  END FUNCTION get_omi_exch_ele_idx_1_1
  !
  !! Ermittle die Indexposition f&uuml;r das erstmalige Auftreten eines
  !! <EM>ElementSet</EM> anhand der "elemset_id" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_exch_ele_id_idx_1_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this(:) ! 
    !! "elemset_id" des zu suchenden <EM>ElementSet</EM>
    CHARACTER (LEN=*) , INTENT(IN) :: val     ! 
    !! Ergebnis: Zeiger auf einen Eintrag in this(:), welcher der
    !! "elemset_id" val entspricht <BR>
    !! wird kein Eintrag gefunden, so wird "undefiniert" zur&uuml;ckgegeben
    INTEGER :: res       ! 
    !! Hilfsvariable
    CHARACTER (LEN=c_len_omi_ele_id) :: id ! 
    INTEGER                          :: i, l1, l2 ! 
    !
    res = c_undef_omi_exch_int
    l1  = LEN_TRIM(val)
    IF ( l1 > 0 ) THEN
       DO i=1,SIZE(this)
          IF ( res /= c_undef_omi_exch_int ) EXIT
          id = get_omi_ele_id ( this(i)%ele )
          l2 = LEN_TRIM(id)
          IF ( l1 == l2 ) THEN
             IF ( val(1:l1) == id(1:l2) ) res = i
          END IF
       END DO
    END IF
    !
  END FUNCTION get_omi_exch_ele_id_idx_1_0
  !
  !! Ermittle die Indexpositionen f&uuml;r das erstmalige Auftreten 
  !! verschiedener <EM>ElementSets</EM> anhand der "elemset_ids" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_exch_ele_id_idx_1_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this(:) ! 
    !! "elemset_ids" der <EM>ElementSets</EM>
    CHARACTER (LEN=*) , INTENT(IN) :: val(:)  ! 
    !! Ergebnis: Zeiger auf die Eintr&auml;ge in this(:), welche den
    !! "elemset_ids" val(:) entsprechen <BR>
    !! wird kein Eintrag gefunden, so wird "undefiniert" zur&uuml;ckgegeben
    INTEGER :: res(SIZE(val))  ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_exch_ele_id_idx_1_0 ( this, val(i) )
    END DO
    !
  END FUNCTION get_omi_exch_ele_id_idx_1_1
  !
  !! ermittle die Anzahl der Datenpunkte der Austauschgr&ouml;&szlig;e <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_exch_values_count_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_exch) , INTENT(IN) :: this ! 
    !! Ergebnis: Anzahl der Datenpunkte der Austauschgr&ouml;&szlig;e
    INTEGER :: res ! 
    !
    IF ( ASSOCIATED( this%ele ) ) THEN
       res = get_omi_ele_point_count ( this%ele )
    END IF
    !
  END FUNCTION get_omi_exch_values_count_0
  !
  !! ermittle die Anzahl der Datenpunkte verschiedener Austauschgr&ouml;&szlig;en <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_exch_values_count_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Anzahl der Datenpunkte der Austauschgr&ouml;&szlig;en
    INTEGER :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_exch_values_count_0 ( this(i) )
    END DO
    !
  END FUNCTION get_omi_exch_values_count_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-IS/HAS-Methoden <<<
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe, ob eine Austauschgr&ouml;&szlig;e eine Export-Gr&ouml;&szlig;e ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_omi_exch_exporting_0 ( this ) &
       RESULT( res ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_exch) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !
    res = ( this%role == c_export_int )
    !
  END FUNCTION is_omi_exch_exporting_0
  !
  !! Pr&uuml;fe, ob mehrere Austauschgr&ouml;&szlig;en Export-Gr&ouml;&szlig;en sind <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_omi_exch_exporting_1 ( this ) &
       RESULT( res ) 
    !! Datenobjekte (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = is_omi_exch_exporting_0 ( this(i) )
    END DO
    !
  END FUNCTION is_omi_exch_exporting_1
  !
  !! Pr&uuml;fe, ob eine Austauschgr&ouml;&szlig;e eine Import-Gr&ouml;&szlig;e ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_omi_exch_importing_0 ( this ) &
       RESULT( res ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_exch) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !
    res = ( this%role == c_import_int )
    !
  END FUNCTION is_omi_exch_importing_0
  !
  !! Pr&uuml;fe, ob mehrere Austauschgr&ouml;&szlig;en Import-Gr&ouml;&szlig;en sind <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_omi_exch_importing_1 ( this ) &
       RESULT( res ) 
    !! Datenobjekte (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = is_omi_exch_importing_0 ( this(i) )
    END DO
    !
  END FUNCTION is_omi_exch_importing_1
  !
  !! Pr&uuml;fe, ob eine Austauschgr&ouml;&szlig;e eine skalare Gr&ouml;&szlig;e ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_omi_exch_scalar_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_exch) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !
    IF ( ASSOCIATED( this%quant ) ) THEN
       res = is_omi_quant_scalar ( this%quant )
    ELSE
       res = .false.
    END IF
    !
  END FUNCTION is_omi_exch_scalar_0
  !
  !! Pr&uuml;fe, ob mehrere Austauschgr&ouml;&szlig;en skalare Gr&ouml;&szlig;en sind <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_omi_exch_scalar_1 ( this ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = is_omi_exch_scalar_0 ( this(i) )
    END DO
    !
  END FUNCTION is_omi_exch_scalar_1
  !
  !! Pr&uuml;fe, ob eine Austauschgr&ouml;&szlig;e eine vektorielle Gr&ouml;&szlig;e ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_omi_exch_vector_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_exch) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !
    IF ( ASSOCIATED( this%quant ) ) THEN
       res = is_omi_quant_vector ( this%quant )
    ELSE
       res = .false.
    END IF
    !
  END FUNCTION is_omi_exch_vector_0
  !
  !! Pr&uuml;fe, ob mehrere Austauschgr&ouml;&szlig;en vektorielle Gr&ouml;&szlig;en sind <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_omi_exch_vector_1 ( this ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = is_omi_exch_vector_0 ( this(i) )
    END DO
    !
  END FUNCTION is_omi_exch_vector_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_exch_0_0 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_exch) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_exch) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1)  = eq_omi_exch_quant ( this1, this2 )
    l_ok(2)  = eq_omi_exch_ele   ( this1, this2 )
    l_ok(3)  = eq_omi_exch_dope  ( this1, this2 )
    l_ok(4)  = ( this1%role  == this2%role  )
    !
    ok = ALL( l_ok )
    !
  END FUNCTION eq_omi_exch_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_exch_1_0 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_exch) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    ! 
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_exch_0_0 ( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_omi_exch_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_exch_0_1 ( this1, this2 ) &
       RESULT( ok )
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_omi_exch) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_exch_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_omi_exch_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_exch_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_exch_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_omi_exch_1_1
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
  FUNCTION ne_omi_exch_0_0 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_exch) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_exch) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = .NOT. eq_omi_exch_0_0( this1, this2 )
    !
  END FUNCTION ne_omi_exch_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_exch_1_0 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_exch) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ok(:) = .NOT. eq_omi_exch_1_0( this1(:), this2 )
    !
  END FUNCTION ne_omi_exch_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Vektor )<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION ne_omi_exch_0_1 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_exch) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ok(:) = .NOT. eq_omi_exch_0_1( this1, this2(:) )
    !
  END FUNCTION ne_omi_exch_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_exch_1_1 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_exch) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Hilfsvariable
    INTEGER :: l ! 
    !
    l       =SIZE(ok)
    ok(1:l) = .NOT. eq_omi_exch_1_1( this1(1:l), this2(1:l) )
    !
  END FUNCTION ne_omi_exch_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-Copy-Methoden <<<
  ! ----------------------------------------------------------------------
  !
  !! kopiere den Inhalt einer Komponente in eine andere Komponente <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE copy_omi_exch_0_0 ( this1, this2 )
    !! Ziel-Datenobjekt
    TYPE (t_omi_exch) , INTENT(OUT) :: this1 ! 
    !! Quell-Datenobjekt
    TYPE (t_omi_exch) , INTENT(IN)  :: this2 ! 
    !
    CALL new_omi_exch_0        ( this1 )
    CALL set_omi_exch_role_0_0 ( this1, this2%role )
    IF ( ASSOCIATED( this2%quant ) ) CALL set_omi_exch_quant_ref_0_0 ( this1, this2%quant )
    IF ( ASSOCIATED( this2%ele   ) ) CALL set_omi_exch_ele_ref_0_0   ( this1, this2%ele   )
    IF ( ASSOCIATED( this2%dope  ) ) CALL set_omi_exch_dope_ref_0_1  ( this1, this2%dope  )
    !
  END SUBROUTINE copy_omi_exch_0_0
  !
  !! kopiere den Inhalt mehrere Komponente auf andere Komponenten <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE copy_omi_exch_1_1 ( this1, this2 )
    !! Ziel-Datenobjekt
    TYPE (t_omi_exch) , INTENT(OUT) :: this1(:) ! 
    !! Quell-Datenobjekt
    TYPE (t_omi_exch) , INTENT(IN)  :: this2(:) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,MIN(SIZE(this1),SIZE(this2))
       CALL copy_omi_exch_0_0 ( this1(i), this2(i) )
    END DO
    !
  END SUBROUTINE copy_omi_exch_1_1
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
       WRITE(*,*) ' *** Warnung *** Modul "b_omi_exch" nicht initialisiert'
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_omi_exch ausfuehren'
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
  SUBROUTINE init_omi_exch_all_errors ( )
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
               '--> INIT_omi_exch ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_omi_exch ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_exch"\n'//&
               'Typ-Komponente = "quant"\n'//&
               'assoziiert     = <associated>\n'//&
               'Inhalt o.k.    = <ok>\n'//&
               '--> Daten pruefen, ggf. SET_OMI_EXCH_QUANT_REF verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_exch"\n'//&
               'Typ-Komponente = "ele"\n'//&
               'assoziiert     = <associated>\n'//&
               'Inhalt o.k.    = <ok>\n'//&
               '--> Daten pruefen, ggf. SET_OMI_EXCH_ELE_REF verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_exch"\n'//&
               'Typ-Komponente = "dope(:)"\n'//&
               'assoziiert     = <associated>\n'//&
               'Inhalt o.k.    = <ok>\n'//&
               '--> Daten pruefen, ggf. SET_OMI_EXCH_DOPE_REF verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_exch"\n'//&
               'Typ-Komponente = "role"\n'//&
               'aktuell        = <aktuell> [muss 1 oder 2 sein]\n'//&
               '--> Daten pruefen, ggf. SET_OMI_EXCH_ROLE verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code in Modul "b_omi_exch" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Fusszeilen\n'//&
               '--> Code in Modul "b_omi_exch" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Index des Datenobjektes (1D-Array)\n'//&
               '--> Code in Modul "b_omi_exch" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_exch"\n'//&
               'Typ-Komponente = "quant"\n'//&
               '--> Code in Modul "b_omi_exch" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_exch"\n'//&
               'Typ-Komponente = "ele"\n'//&
               '--> Code in Modul "b_omi_exch" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_exch"\n'//&
               'Typ-Komponente = "dope(:)"\n'//&
               '--> Code in Modul "b_omi_exch" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_exch"\n'//&
               'Typ-Komponente = "role"\n'//&
               '--> Code in Modul "b_omi_exch" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "b_omi_exch"\n'//&
               '--> Code in Modul "b_omi_exch" / Daten pruefen' )
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
  END SUBROUTINE init_omi_exch_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_omi_exch_all_errors ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_omi_exch_all_errors
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
  !! Pr&uuml;fe, ob die Komponente "quant" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_exch_quant ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_exch) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='ok_omi_exch_quant' ! 
    !! Hilfsvariable
    CHARACTER (LEN=1) :: ch      ! 
    LOGICAL           :: l_ok(2) ! 
    !
    l_ok(:) = .false.
    l_ok(1) = ASSOCIATED( this%quant )
    IF ( l_ok(1) ) l_ok(2) = ok_omi_quant ( this%quant )
    ok      = ALL( l_ok )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
       WRITE(ch,'(L1)') l_ok(1) ; CALL setup_error_act ( '<assoziiert>', ch )
       WRITE(ch,'(L1)') l_ok(2) ; CALL setup_error_act ( '<ok>', ch )
    END IF
    !
  END FUNCTION ok_omi_exch_quant
  !
  !! Pr&uuml;fe, ob die Komponente "ele" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_exch_ele ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_exch) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_omi_exch_ele' ! 
    !! Hilfsvariable
    CHARACTER (LEN=1) :: ch      ! 
    LOGICAL           :: l_ok(2) ! 
    !
    l_ok(:) = .false.
    l_ok(1) = ASSOCIATED( this%ele )
    IF ( l_ok(1) ) l_ok(2) = ok_omi_ele ( this%ele )
    ok      = ALL( l_ok )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
       WRITE(ch,'(L1)') l_ok(1) ; CALL setup_error_act ( '<assoziiert>', ch )
       WRITE(ch,'(L1)') l_ok(2) ; CALL setup_error_act ( '<ok>', ch )
    END IF
    !
  END FUNCTION ok_omi_exch_ele
  !
  !! Pr&uuml;fe, ob die Komponente "dope(:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_exch_dope ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_exch) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='ok_omi_exch_dope' ! 
    !! Hilfsvariable
    CHARACTER (LEN=1) :: ch      ! 
    LOGICAL           :: l_ok(2) ! 
    !
    l_ok(:) = .false.
    l_ok(1) = ASSOCIATED( this%dope )
    IF ( l_ok(1) ) l_ok(2) = ALL( ok_omi_dope ( this%dope ) )
    ok      = ( ALL( l_ok ) .OR. ALL( .NOT.l_ok ) )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
       WRITE(ch,'(L1)') l_ok(1) ; CALL setup_error_act ( '<assoziiert>', ch )
       WRITE(ch,'(L1)') l_ok(2) ; CALL setup_error_act ( '<ok>', ch )
    END IF
    !
  END FUNCTION ok_omi_exch_dope
  !
  !! Pr&uuml;fe, ob die Komponente "role" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_exch_role ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_exch) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='ok_omi_exch_role' ! 
    !! Hilfsvariable
    CHARACTER (LEN=10) :: ch ! 
    !
    ok = ( this%role == c_export_int .OR. this%role == c_import_int )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6040, c_upname, c_modname )
       WRITE(ch,'(I10)') this%role ; CALL setup_error_act ( '<aktuell>', ch )
    END IF
    !
  END FUNCTION ok_omi_exch_role
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "quant" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_exch_quant ( this )
    !! Datenobjekt
    TYPE (t_omi_exch) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=20) , PARAMETER :: c_upname='print_omi_exch_quant' ! 
    !
    IF ( ASSOCIATED( this%quant ) ) THEN
       CALL print_omi_quant ( this%quant )
       IF ( any_error( ) ) THEN
          CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname )
       END IF
    END IF
    !
  END SUBROUTINE print_omi_exch_quant
  !
  !! Drucke den Inhalt der Komponente "ele" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_exch_ele ( this )
    !! Datenobjekt
    TYPE (t_omi_exch) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_omi_exch_ele' ! 
    !
    IF ( ASSOCIATED( this%ele ) ) THEN
       CALL print_omi_ele ( this%ele )
       IF ( any_error( ) ) THEN
          CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname )
       END IF
    END IF
    !
  END SUBROUTINE print_omi_exch_ele
  !
  !! Drucke den Inhalt der Komponente "dope(:)" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_exch_dope ( this )
    !! Datenobjekt
    TYPE (t_omi_exch) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='print_omi_exch_dope' ! 
    !
    IF ( ASSOCIATED( this%dope ) ) THEN
       CALL print_omi_dope ( this%dope )
       IF ( any_error( ) ) THEN
          CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname )
       END IF
    END IF
    !
  END SUBROUTINE print_omi_exch_dope
  !
  !! Drucke den Inhalt der Komponente "role" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_exch_role ( this )
    !! Datenobjekt
    TYPE (t_omi_exch) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='print_omi_exch_role' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) this%role
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 7040, c_upname, c_modname, stat )
    END IF
    !
8000 FORMAT &
          ('# Inhalt der Komponente role  - - - - - - - - - - - - - - - - ',/&
           '# aktuell = ',I10,/ &
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_exch_role
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe Komponente "quant" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_exch_quant ( this1, this2 ) &
       RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_omi_exch) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_omi_exch) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Hilfsvariable
    LOGICAL :: as(2) ! 
    !
    as(1) = ASSOCIATED( this1%quant )
    as(2) = ASSOCIATED( this2%quant )
    !
    IF      ( ALL(as)      ) THEN
       ok = eq_omi_quant( this1%quant, this2%quant )
    ELSE IF ( ALL(.NOT.as) ) THEN
       ok = .true.
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION eq_omi_exch_quant
  !
  !! pr&uuml;fe Komponente "ele" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_exch_ele ( this1, this2 ) &
       RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_omi_exch) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_omi_exch) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Hilfsvariable
    LOGICAL :: as(2) ! 
    !
    as(1) = ASSOCIATED( this1%ele )
    as(2) = ASSOCIATED( this2%ele )
    !
    IF      ( ALL(as)      ) THEN
       ok = eq_omi_ele( this1%ele, this2%ele )
    ELSE IF ( ALL(.NOT.as) ) THEN
       ok = .true.
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION eq_omi_exch_ele 
  !
  !! pr&uuml;fe Komponente "dope(:)" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_exch_dope ( this1, this2 ) &
       RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_omi_exch) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_omi_exch) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Hilfsvariable
    INTEGER :: sz(2) ! 
    LOGICAL :: as(2) ! 
    !
    sz(:) = -1
    as(1) = ASSOCIATED( this1%dope )
    as(2) = ASSOCIATED( this2%dope )
    IF ( as(1) ) sz(1) = SIZE( this1%dope )
    IF ( as(2) ) sz(2) = SIZE( this2%dope )
    !
    IF      ( ALL(as) .AND. ALL(sz==sz(1)) ) THEN
       ok = ALL( eq_omi_dope( this1%dope, this2%dope ) )
    ELSE IF ( ALL(.NOT.as) ) THEN
       ok = .true.
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION eq_omi_exch_dope
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
  ! >>> PRIVATE-OPERATOR(<=)-Methoden <<< [ERR_NO = 18000 bis 18999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> HILFSMETHODEN <<<
  ! ----------------------------------------------------------------------
  !
  !! ermittle ein Indikatorfeld mit .true./.false. f&uuml;r einmaliges
  !! Auftreten von Texten in einem Feld
  FUNCTION is_dif_string_1 ( val ) &
       RESULT( res )
    !! Feld mit beliebig vielen Strings
    CHARACTER (LEN=*) , INTENT(IN) :: val(:) ! 
    !! Ergebnis: alle erstmalig auftretenden Feldelement sind mit .true.
    !! gekennzeichnet, und alle anderen mit .false.
    LOGICAL :: res(SIZE(val)) ! 
    !! Hilfsvariable
    INTEGER :: i, j, l1, l2 ! 
    !
    res(:) = .true.
    DO i=2,SIZE(res)
       l1 = LEN_TRIM(val(i))
       DO j=1,i-1
          IF ( .NOT. res(i) ) EXIT
          l2 = LEN_TRIM(val(j))
          IF ( l1 == l2 ) THEN
             IF ( val(i)(1:l1) == val(j)(1:l2) ) res(i) = .false. 
          END IF
       END DO
    END DO
    !
  END FUNCTION is_dif_string_1
  !
END MODULE b_omi_exch
! TailOfBaseModule --------------------------------------------------------
