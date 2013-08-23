! -------------------------------------------------------------------------
! HeadOfBaseModule --------------------------------------------------------
!
!! <H2>Typ und Methoden analog OpenMI-Interface "IArgument"</h2>
!! @author G&uuml;nther Lang
!! @version 2.1 vom 07/21/05, Quellcode: mod_b_omi_arg.f90
!! <HR>
!! type and methods equivalent to OpenMI interface "IArgument" <BR>
!! <HR>
!  Copyright-Hinweis
!                                                                    <BR>
!  Copyright (C) 2005 <A HREF="http://www.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!                                                                    <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2005-01-19 : G. Lang : Erstversion
!  01.02 : 2005-03-11 : G. Lang : OPERATORen entfernt, auf Funktionen umgestellt
!  02.01 : 2005-07-21 : G. Lang : Anpassungen fuer korrigiertes ElementSet-Verstaendnis (GEI)
!
!!                                                                  <BR>
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!!
!! Typ und Methoden analog OpenMI-Interface <EM>IArgument</EM>. Dient
!! dazu, <EM>Key</EM>-<EM>Value</EM>-Paare bereitzustellen, die z.B.
!! als Argumente bei Datenoperationen oder w&auml;hrend der 
!! Initialisierungsphase von Komponenten mit modellspezifischen
!! Daten ben&ouml;tigt werden.
!!
!! <OL>
!!    <LI> Initialisierung und De-Initialisierung von skalaren und
!!         vektoriellen Variablen des Typs "t_omi_arg";
!!    <LI> Setzen der Komponenten in Variablen des Typs "t_omi_arg";
!!    <LI> Holen der Komponenten in Variablen des Typs "t_omi_arg";
!!    <LI> Drucken des Inhalts der Komponenten von Variablen des Typs "t_omi_arg";
!!    <LI> Pr&uuml;fen des Inhalts von Variablen des Typs "t_omi_arg";
!!    <LI> Vergleichen des Inhalts verschiedener Variablen des Typs "t_omi_arg";
!!    <LI> Nach einem bestimmten <EM>key</EM> in einer Liste von Variablen des Typs "t_omi_arg" suchen.
!! </OL>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt den selbst definierten Datentyp "t_omi_arg"
!! zur Verf&uuml;gung. Dieser besteht aus den folgenden Komponenten:      <BR>
!! <OL>
!!     <LI> key : Schl&uuml;sselwort zur Bezeichnung des Arguments
!!     <LI> value : aktueller Wert des Arguments
!!     <LI> readonly : logischer Steuerparameter, ob der Argumentwert durch den Benutzer abgewandelt werden darf oder nicht
!!     <LI> description : verbale Beschreibung der Bedeutung des Arguments
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
!!    <LI> Initialisieren des Moduls b_omi_arg mit INIT-Methode;
!!    <LI> ggf. Verwenden verschiedener SETUP-Methoden;
!!    <LI> Verwenden beliebiger Modul-Methoden (nicht INIT und CLEAR);
!!    <LI> De-Initialisieren des Moduls b_omi_arg mit CLEAR-Methode.
!! </OL>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Fehlersituationen des Moduls</H3>
!!                                                                    <BR>
!! Hinweis: einige Methoden dieses Moduls erzeugen Fehlermeldungen,   
!!          andere nicht. 
!!          Routinen, die Fehlermeldungen generieren m&uuml;ssen pr&uuml;fen,
!!          ob das Modul korrekt initialisert wurde (ok_initialised)  <BR>
!!          F&uuml;r eine vollst&auml;ndige &Uuml;bersicht verwende man
!!          die Methode PRINT_OMI_ARG_ALL_ERRORS.
!!                                                                    <BR>
!! <HR>
!
MODULE b_omi_arg
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
  ! [C.0] Konstantwerte f&uuml;r die Typ-Definition (Parameter)
  !! max. Anzahl der Zeichen in der Komponente <EM>key</EM> 
  INTEGER , PUBLIC , PARAMETER :: c_len_omi_arg_key=40         ! 
  !! max. Anzahl der Zeichen in der Komponente <EM>value</EM> 
  INTEGER , PUBLIC , PARAMETER :: c_len_omi_arg_value=80       ! 
  !! max. Anzahl der Zeichen in der Komponente <EM>description</EM> 
  INTEGER , PUBLIC , PARAMETER :: c_len_omi_arg_description=80 ! 
  !
  ! [C.1] (maximal) einen oeffentlich zugaenglichen Typ deklarieren
  !
  !! Komponenten in Analogie zu der OpenMI-Interface <EM>IArgument</EM>: <BR>
  !! <EM>key</EM> : Schl&uuml;sselwort zur Bezeichnung des Arguments; <BR>
  !! <EM>value</EM> : aktueller Wert des Arguments; <BR>
  !! <EM>readonly</EM> : logischer Steuerparameter, ob der Argumentwert durch den Benutzer abgewandelt werden darf oder nicht; <BR>
  !! <EM>description</EM> : verbale Beschreibung der Bedeutung des Arguments.
  TYPE , PUBLIC :: t_omi_arg
     PRIVATE
     CHARACTER (LEN=c_len_omi_arg_key)         :: key         ! 
     CHARACTER (LEN=c_len_omi_arg_value)       :: value       ! 
     LOGICAL                                   :: readonly    ! 
     CHARACTER (LEN=c_len_omi_arg_description) :: description ! 
  END TYPE t_omi_arg
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
  !! Allokieren/Initialisieren der statischen Datenobjekte des Moduls; <BR>
  !! Initialisieren der statischen Modul-Daten mit Default-Werten.
  INTERFACE init_omi_arg
     MODULE PROCEDURE init_omi_arg_d ! 
  END INTERFACE
  !
  !! De-Allokieren/De-Initialisieren der statischen Datenobjekte des Moduls; <BR>
  !! Re-Initialisieren einiger statischer Daten mit Default-Werten.
  INTERFACE clear_omi_arg
     MODULE PROCEDURE clear_omi_arg_d ! 
  END INTERFACE
  !
  !! logische Kanalnummer f&uuml;r PRINT-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>PRN_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>PRN_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>PRN_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_omi_arg_prn_lun
     MODULE PROCEDURE setup_omi_arg_prn_lun_d ! 
  END INTERFACE
  !
  !! logische Kanalnummer f&uuml;r TRACE-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>TRC_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>TRC_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>TRC_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_omi_arg_trc_lun
     MODULE PROCEDURE setup_omi_arg_trc_lun_d ! 
  END INTERFACE
  !
  !! Erzeugen von Datenobjekten "t_omi_arg" und Initialisieren mit Default-Werten: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE new_omi_arg
     MODULE PROCEDURE new_omi_arg_0  ! Version fuer Skalar
     MODULE PROCEDURE new_omi_arg_1  ! Version fuer 1D-Array
  END INTERFACE
  !
  !! Vernichten von Datenobjekten "t_omi_arg" (Skalar, 1D-Array) und teilweise 
  !! Re-Initialisieren mit Default-Werten: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE kill_omi_arg
     MODULE PROCEDURE kill_omi_arg_0 ! Version fuer Skalar
     MODULE PROCEDURE kill_omi_arg_1 ! Version fuer 1D-Array
  END INTERFACE
  !
  !! Pr&uuml;fen von Datenobjekten "t_omi_arg" auf G&uuml;ltigkeit: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE ok_omi_arg
     MODULE PROCEDURE ok_omi_arg_0 ! Version fuer Skalar
     MODULE PROCEDURE ok_omi_arg_1 ! Version fuer 1D-Array
  END INTERFACE
  !
  !! Drucken von Datenobjekten "t_omi_arg" (Skalar, 1D-Array): <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) <BR>
  !! Alle Komponenten des Typs "t_omi_arg" auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0. <BR>
  INTERFACE print_omi_arg
     MODULE PROCEDURE print_omi_arg_0 ! Version fuer Skalar
     MODULE PROCEDURE print_omi_arg_1 ! Version fuer 1D-Array
  END INTERFACE
  !
  !! Drucken aller in diesem Modul abgelegten statischen Daten; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_arg_static
     MODULE PROCEDURE print_omi_arg_static_d ! 
  END INTERFACE
  !
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Moduls; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_arg_all_errors
     MODULE PROCEDURE print_omi_arg_all_errors_d ! 
  END INTERFACE
  !
  ! ----------------------------------------------------------------
  ! --> nicht benoetigte SET-Interfaces bitte unbedingt loeschen <--
  ! ----------------------------------------------------------------
  !
  !! Setze Komponente "key" in "t_omi_arg" auf Benutzerwert: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE set_omi_arg_key
     MODULE PROCEDURE set_omi_arg_key_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_omi_arg_key_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !! Setze Komponente "value" in "t_omi_arg" auf Benutzerwert: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE set_omi_arg_value
     MODULE PROCEDURE set_omi_arg_value_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_omi_arg_value_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !! Setze Komponente "readonly" in "t_omi_arg" auf Benutzerwert: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE set_omi_arg_readonly
     MODULE PROCEDURE set_omi_arg_readonly_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_omi_arg_readonly_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !! Setze Komponente "description" in "t_omi_arg" auf Benutzerwert: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE set_omi_arg_description
     MODULE PROCEDURE set_omi_arg_description_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_omi_arg_description_1_0 ! Objekt (Vektor) / Daten (Skalar) 
  END INTERFACE
  !
  ! ----------------------------------------------------------------
  ! --> nicht benoetigte GET-Interfaces bitte unbedingt loeschen <--
  ! ----------------------------------------------------------------
  !
  !! Hole Komponente "key" aus "t_omi_arg": <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE get_omi_arg_key
     MODULE PROCEDURE get_omi_arg_key_0_0 ! Skalar
     MODULE PROCEDURE get_omi_arg_key_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "value" aus "t_omi_arg": <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE get_omi_arg_value
     MODULE PROCEDURE get_omi_arg_value_0_0 ! Skalar
     MODULE PROCEDURE get_omi_arg_value_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "readonly" aus "t_omi_arg": <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE get_omi_arg_readonly
     MODULE PROCEDURE get_omi_arg_readonly_0_0 ! Skalar
     MODULE PROCEDURE get_omi_arg_readonly_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "description" aus "t_omi_arg": <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE get_omi_arg_description
     MODULE PROCEDURE get_omi_arg_description_0_0 ! Skalar
     MODULE PROCEDURE get_omi_arg_description_1_0 ! Vektor
  END INTERFACE
  !
  ! ... ggf. Holen fuer weitere Komponenten des Datenobjektes ergaenzen
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  !! suche nach einem bestimmten Wert der Komponente <EM>key</EM> in einem 
  !! 1D-Feld des Typs "t_omi_arg": <BR>
  !! a) f&uuml;r einen Wert <EM>key</EM> (Skalar) <BR>
  !! b) f&uuml;r viele Werte <EM>key(:)</EM> (Vektor)
  INTERFACE get_omi_arg_idx
     MODULE PROCEDURE get_omi_arg_idx_1_0
     MODULE PROCEDURE get_omi_arg_idx_1_1
  END INTERFACE
  !
  !! ermittle, ob ein Objekt des Typs "t_omi_arg" <EM>readonly</EM> ist: <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE is_omi_arg_readonly
     MODULE PROCEDURE is_omi_arg_readonly_0
     MODULE PROCEDURE is_omi_arg_readonly_1
  END INTERFACE
  !! ermittle, ob ein Objekt des Typs "t_omi_arg" <EM>writeable</EM> ist: <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE is_omi_arg_writeable
     MODULE PROCEDURE is_omi_arg_writeable_0
     MODULE PROCEDURE is_omi_arg_writeable_1
  END INTERFACE
  !
  !! Pr&uuml;fen zweier Datenobjekte "t_omi_arg" auf Gleichheit (Funktion) <BR>
  !! a) Skalar1 == Skalar2 <BR>
  !! b) Skalar1 == Vektor2 <BR>
  !! c) Vektor1 == Skalar2 <BR>
  !! d) Vektor1 == Vektor2
  INTERFACE eq_omi_arg
     MODULE PROCEDURE eq_omi_arg_0_0  ! 
     MODULE PROCEDURE eq_omi_arg_0_1  ! 
     MODULE PROCEDURE eq_omi_arg_1_0  ! 
     MODULE PROCEDURE eq_omi_arg_1_1  ! 
  END INTERFACE
  !! Pr&uuml;fen zweier Datenobjekte "t_omi_arg" auf Ungleichheit (Funktion) <BR>
  !! a) Skalar1 /= Skalar2 <BR>
  !! b) Skalar1 /= Vektor2 <BR>
  !! c) Vektor1 /= Skalar2 <BR>
  !! d) Vektor1 /= Vektor2
  INTERFACE ne_omi_arg
     MODULE PROCEDURE ne_omi_arg_0_0  ! 
     MODULE PROCEDURE ne_omi_arg_0_1  ! 
     MODULE PROCEDURE ne_omi_arg_1_0  ! 
     MODULE PROCEDURE ne_omi_arg_1_1  ! 
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
  !
  ! [C.6.2] optional vorhandene oeffentliche Operatoren
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  ! [C.7.1] unbedingt erforderliche oeffentliche Methoden
  !
  PUBLIC :: init_omi_arg
  PUBLIC :: clear_omi_arg
  PUBLIC :: setup_omi_arg_prn_lun
  PUBLIC :: setup_omi_arg_trc_lun
  PUBLIC :: new_omi_arg
  PUBLIC :: kill_omi_arg
  PUBLIC :: ok_omi_arg
  PUBLIC :: print_omi_arg
  PUBLIC :: print_omi_arg_static
  PUBLIC :: print_omi_arg_all_errors
  PUBLIC :: set_omi_arg_key
  PUBLIC :: set_omi_arg_value
  PUBLIC :: set_omi_arg_readonly
  PUBLIC :: set_omi_arg_description
  PUBLIC :: get_omi_arg_key
  PUBLIC :: get_omi_arg_value
  PUBLIC :: get_omi_arg_readonly
  PUBLIC :: get_omi_arg_description
  PUBLIC :: eq_omi_arg
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: ne_omi_arg
  !
  PUBLIC :: get_omi_arg_idx
  !
  PUBLIC :: is_omi_arg_readonly
  PUBLIC :: is_omi_arg_writeable
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
  CHARACTER (LEN=09), PARAMETER :: c_modname      = 'b_omi_arg' ! 
  !! Undefined-String
  CHARACTER (LEN=09), PARAMETER :: c_undefined    = 'undefined' ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.          ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1               ! 
  !! Anzahl der Datenkomponenten des Typs t_omi_arg
  INTEGER           , PARAMETER :: c_nofcomp      =  4               ! ggf. modifizieren
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
  SUBROUTINE init_omi_arg_d &
       ( )
    !
    USE b_error, ONLY : DEBUG_b
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='init_omi_arg_d' 
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_b > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "b_omi_arg" version 2.1 of 07/21/05                 '
          WRITE(*,*) ' Copyright (C) 2005 Bundesanstalt fuer Wasserbau   '
          WRITE(*,*)
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       ! IF ( no_error( ) ) CALL init_<BaseModuleName> ( )
       ! ... ggf. weitere Initialisierungen ergaenzen
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_omi_arg_all_errors ( ) 
       ! [1.5] Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.6] endgueltiges Setzen des Initialisierungs-Indikators
       initialised = MERGE( .true., .false., no_error( ) )
    END IF
    !
    ! 2.0 Initialisierungszaehler heraufsetzen
    !
    n_init = n_init + 1
    !
  END SUBROUTINE init_omi_arg_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_omi_arg_d &
       ( )
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='clear_omi_arg_d' ! 
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_omi_arg_all_errors ( ) 
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.3] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.4] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.4.1] ggf. weitere Module de-initialisieren
       ! IF ( no_error( ) ) CALL clear_<BaseModuleName> ( )
       ! ... ggf. weitere De-Initialisierungen ergaenzen
       ! [1.4.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    !
    ! 2.0 Initialisierungszaehler heruntersetzen
    !
    n_init = n_init - 1
    !
  END SUBROUTINE clear_omi_arg_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_omi_arg_prn_lun_d &
       ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER :: c_upname='setup_omi_arg_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
       ! IF ( no_error( ) ) CALL setup_<BaseModuleName>_prn_lun ( lun )
       ! ... ggf. weitere ergaenzen
    END IF
    !
  END SUBROUTINE setup_omi_arg_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_omi_arg_trc_lun_d &
       ( lun )
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER :: c_upname='setup_omi_arg_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
       ! IF ( no_error( ) ) CALL setup_<BaseModuleName>_trc_lun ( lun )
       ! ... ggf. weitere ergaenzen
    END IF
    !
  END SUBROUTINE setup_omi_arg_trc_lun_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_omi_arg_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_arg) , INTENT(OUT) :: this ! 
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=13), PARAMETER :: c_upname='new_omi_arg_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       this%key         = REPEAT( ' ', LEN(this%key)         ) ; this%key         = c_undefined ! 
       this%value       = REPEAT( ' ', LEN(this%value)       ) ; this%value       = c_undefined ! 
       this%readonly    = .true. ! 
       this%description = REPEAT( ' ', LEN(this%description) ) ; this%description = c_undefined ! 
    END IF
    !
  END SUBROUTINE new_omi_arg_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_omi_arg_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_arg) , INTENT(OUT) :: this(:) ! 
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=13), PARAMETER :: c_upname='new_omi_arg_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL new_omi_arg_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_omi_arg_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_omi_arg_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_arg) , INTENT(INOUT) :: this ! 
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='kill_omi_arg_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       IF ( no_error( ) ) CALL new_omi_arg_0 ( this )
    END IF
    !
  END SUBROUTINE kill_omi_arg_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_omi_arg_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_arg) , INTENT(INOUT) :: this(:) ! 
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='kill_omi_arg_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL kill_omi_arg_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_omi_arg_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_arg_0 &
       ( this )              &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_arg) , INTENT(IN) :: this ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=12), PARAMETER :: c_upname='ok_omi_arg_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok(1)  = ok_omi_arg_key( this )
       l_ok(2)  = ok_omi_arg_value( this )
       l_ok(3)  = ok_omi_arg_readonly( this )
       l_ok(4)  = ok_omi_arg_description( this )
    END IF
    !
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_omi_arg_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_arg_1 &
       ( this )              &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_arg) , INTENT(IN) :: this(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=12), PARAMETER :: c_upname='ok_omi_arg_1' 
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
          ok(i) = ok_omi_arg_0 ( this(i) )
       END DO
    END IF
    !
  END FUNCTION ok_omi_arg_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_arg_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_arg) , INTENT(IN) :: this ! 
    !! Name der Function
    CHARACTER (LEN=15), PARAMETER :: c_upname='print_omi_arg_0' 
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
       IF ( no_error( ) ) CALL print_omi_arg_key( this )
       IF ( no_error( ) ) CALL print_omi_arg_value( this )
       IF ( no_error( ) ) CALL print_omi_arg_readonly( this )
       IF ( no_error( ) ) CALL print_omi_arg_description( this )
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
8000 FORMAT('# Beginn Objekt t_omi_arg ------------------------------')
8001 FORMAT('# Ende   Objekt t_omi_arg ------------------------------')
    !
  END SUBROUTINE print_omi_arg_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_arg_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_arg) , INTENT(IN) :: this(:) ! 
    !! Name der Function
    CHARACTER (LEN=15), PARAMETER :: c_upname='print_omi_arg_1' 
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
          IF ( no_error( ) ) CALL print_omi_arg_0 ( this(i) )
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
  END SUBROUTINE print_omi_arg_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_arg_static_d &
       ( )
    !! Name der Function
    CHARACTER (LEN=22), PARAMETER :: c_upname='print_omi_arg_static_d' 
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
       IF ( no_error( ) ) CALL print_omi_arg_all_errors_d ( )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls b_omi_arg         ',/ &
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
  END SUBROUTINE print_omi_arg_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_arg_all_errors_d &
       ( )
    !! Name der Function
    CHARACTER (LEN=26), PARAMETER :: c_upname='print_omi_arg_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_omi_arg_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! weise der Komponente "key" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_arg_key_0_0 &
       ( this, &
       val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_arg)  , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "key"
    CHARACTER (LEN=*) , INTENT(IN)    :: val  ! 
    !
    this%key = REPEAT( ' ', LEN(this%key) )
    this%key = val(1:MAX(1,MIN(LEN(this%key),LEN_TRIM(val))))
    !
  END SUBROUTINE set_omi_arg_key_0_0
  !
  !! weise der Komponente "key" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_arg_key_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_arg)  , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "key"
    CHARACTER (LEN=*) , INTENT(IN)    :: val     ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_omi_arg_key_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_omi_arg_key_1_0
  !
  !! weise der Komponente "value" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_arg_value_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_arg)  , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "value"
    CHARACTER (LEN=*) , INTENT(IN)    :: val  ! 
    !
    this%value = REPEAT( ' ', LEN(this%value) )
    this%value = val(1:MAX(1,MIN(LEN(this%value),LEN_TRIM(val))))
    !
  END SUBROUTINE set_omi_arg_value_0_0
  !
  !! weise der Komponente "value" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_arg_value_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_arg)  , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "value"
    CHARACTER (LEN=*) , INTENT(IN)    :: val  ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_omi_arg_value_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_omi_arg_value_1_0
  !
  !! weise der Komponente "readonly" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_arg_readonly_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_arg) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "readonly"
    LOGICAL          , INTENT(IN)    :: val  ! 
    !
    this%readonly = val
    !
  END SUBROUTINE set_omi_arg_readonly_0_0
  !
  !! weise der Komponente "readonly" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_arg_readonly_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_arg) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "readonly"
    LOGICAL           , INTENT(IN)  :: val     ! 
    !
    this(:)%readonly = val
    !
  END SUBROUTINE set_omi_arg_readonly_1_0
  !
  !! weise der Komponente "description" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_arg_description_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_arg)  , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "description"
    CHARACTER (LEN=*) , INTENT(IN)    :: val  ! 
    !
    this%description = REPEAT( ' ', LEN(this%description) )
    this%description = val(1:MAX(1,MIN(LEN(this%description),LEN_TRIM(val))))
    !
  END SUBROUTINE set_omi_arg_description_0_0
  !
  !! weise der Komponente "description" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_arg_description_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_arg)  , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "description"
    CHARACTER (LEN=*) , INTENT(IN)    :: val     ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       CALL set_omi_arg_description_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_omi_arg_description_1_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! hole die Komponente "key" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_arg_key_0_0 &
       ( this ) &
         RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_arg)    , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "key" (Skalar)
    CHARACTER (LEN=c_len_omi_arg_key) :: val  ! 
    !
    val = this%key
    !
  END FUNCTION get_omi_arg_key_0_0
  !
  !! hole die Komponente "key" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_arg_key_1_0 &
       ( this ) &
         RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_arg)    , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "key"
    CHARACTER (LEN=c_len_omi_arg_key) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%key
    !
  END FUNCTION get_omi_arg_key_1_0
  !
  !! hole die Komponente "value" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_arg_value_0_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_arg)      , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "value" (Skalar)
    CHARACTER (LEN=c_len_omi_arg_value) :: val  ! 
    !
    val = this%value
    !
  END FUNCTION get_omi_arg_value_0_0
  !
  !! hole die Komponente "value" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_arg_value_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_arg)      , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "value"
    CHARACTER (LEN=c_len_omi_arg_value) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%value
    !
  END FUNCTION get_omi_arg_value_1_0
  !
  !! hole die Komponente "readonly" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_arg_readonly_0_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_arg) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "readonly" (Skalar)
    LOGICAL                        :: val  ! 
    !
    val = this%readonly
    !
  END FUNCTION get_omi_arg_readonly_0_0
  !
  !! hole die Komponente "readonly" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_arg_readonly_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_arg) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "readonly"
    LOGICAL                        :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%readonly
    !
  END FUNCTION get_omi_arg_readonly_1_0
  !
  !! hole die Komponente "description" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_arg_description_0_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_arg)            , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "description" (Skalar)
    CHARACTER (LEN=c_len_omi_arg_description) :: val  ! 
    !
    val = this%description
    !
  END FUNCTION get_omi_arg_description_0_0
  !
  !! hole die Komponente "description" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_arg_description_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_arg)            , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "description"
    CHARACTER (LEN=c_len_omi_arg_description) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%description
    !
  END FUNCTION get_omi_arg_description_1_0
  !
  !! suche nach einem bestimmten Wert der Komponente <EM>key</EM> in einem 
  !! 1D-Feld des Typs "t_omi_arg" f&uuml;r einen Wert <EM>key</EM> (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_arg_idx_1_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_arg)  , INTENT(IN) :: this(:) ! 
    !! zu suchender Wert der Komponente <EM>key</EM>
    CHARACTER (LEN=*) , INTENT(IN) :: val     ! 
    !! Ergebnis: Zeiger auf einen Eintrag in this(:), deren aktueller Wert
    !! der Komponente <EM>key</EM> identisch mit <EM>val</EM> ist <BR>
    !! wird kein Eintrag gefunden, so wird -1 zur&uuml;ckgegeben
    INTEGER :: res ! 
    ! Hilfsvariablen
    INTEGER :: i, l1, l2 ! 
    !
    res = -1
    l1  = LEN_TRIM(val)
    DO i=1,SIZE(this)
       IF ( res /= -1 ) EXIT
       l2 = LEN_TRIM(this(i)%key)
       IF ( l1 == l2 ) THEN
          IF ( val(1:l1) == this(i)%key(1:l2) ) res = i
       END IF
    END DO
    !
  END FUNCTION get_omi_arg_idx_1_0
  !
  !! suche nach einem bestimmten Wert der Komponente <EM>key</EM> in einem 
  !! 1D-Feld des Typs "t_omi_arg" f&uuml;r viele Werte <EM>key(:)</EM> (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_arg_idx_1_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_arg)  , INTENT(IN) :: this(:) ! 
    !! zu suchende Werte der Komponente <EM>key</EM>
    CHARACTER (LEN=*) , INTENT(IN) :: val(:)  ! 
    !! Ergebnis: Zeiger auf die Eintr&auml;ge in this(:), deren aktueller 
    !! Wert der Komponente <EM>key</EM> identisch mit <EM>val(:)</EM> ist <BR>
    !! wird kein Eintrag gefunden, so wird -1 zur&uuml;ckgegeben
    INTEGER :: res(SIZE(val)) ! 
    ! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(val)
       res(i) = get_omi_arg_idx_1_0 ( this(:), val(i) )
    END DO
    !
  END FUNCTION get_omi_arg_idx_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_arg_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_omi_arg) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_arg) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1)  = ( this1%key          == this2%key         )
    l_ok(2)  = ( this1%value        == this2%value       )
    l_ok(3)  = ( (       this1%readonly .AND.       this2%readonly ) .OR. &
                 ( .NOT. this1%readonly .AND. .NOT. this2%readonly ) )
    l_ok(4)  = ( this1%description  == this2%description )
    !
    ok = ALL( l_ok )
    !
  END FUNCTION eq_omi_arg_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_arg_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_arg) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_arg) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_arg_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_omi_arg_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_arg_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_arg) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_arg) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_arg_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_omi_arg_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_arg_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_arg) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_arg) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_arg_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_omi_arg_1_1
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
  FUNCTION ne_omi_arg_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_arg) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_arg) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = .NOT. eq_omi_arg_0_0( this1, this2 )
    !
  END FUNCTION ne_omi_arg_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_arg_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_arg) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_arg) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = ne_omi_arg_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION ne_omi_arg_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Vektor )<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
 FUNCTION ne_omi_arg_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_arg) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_arg) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = ne_omi_arg_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION ne_omi_arg_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_arg_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_arg) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_arg) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = ne_omi_arg_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION ne_omi_arg_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-IS-Methoden <<< 
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe, ob ein Objekt des Typs "t_omi_arg" <EM>readonly</EM> ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_omi_arg_readonly_0 ( this ) &
       RESULT ( res )
    !! Objekt (Skalar)
    TYPE (t_omi_arg) , INTENT(IN) :: this ! 
    !! Testergebnis (Skalar)
    LOGICAL                       :: res  ! 
    !
    res = this%readonly
    !
  END FUNCTION is_omi_arg_readonly_0
  !
  !! Pr&uuml;fe, ob viele Objekte des Typs "t_omi_arg" <EM>readonly</EM> sind <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_omi_arg_readonly_1 ( this ) &
       RESULT ( res )
    !! Objekte (Vektor)
    TYPE (t_omi_arg) , INTENT(IN) :: this(:) ! 
    !! Testergebnis (Skalar)
    LOGICAL                       :: res(SIZE(this)) ! 
    !
    res(:) = this(:)%readonly
    !
  END FUNCTION is_omi_arg_readonly_1
  !
  !! Pr&uuml;fe, ob ein Objekt des Typs "t_omi_arg" <EM>writeable</EM> ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_omi_arg_writeable_0 ( this ) &
       RESULT ( res )
    !! Objekt (Skalar)
    TYPE (t_omi_arg) , INTENT(IN) :: this ! 
    !! Testergebnis (Skalar)
    LOGICAL                       :: res  ! 
    !
    res = .NOT. is_omi_arg_readonly( this )
    !
  END FUNCTION is_omi_arg_writeable_0
  !
  !! Pr&uuml;fe, ob viele Objekte des Typs "t_omi_arg" <EM>writeable</EM> sind <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_omi_arg_writeable_1 ( this ) &
       RESULT ( res )
    !! Objekte (Vektor)
    TYPE (t_omi_arg) , INTENT(IN) :: this(:) ! 
    !! Testergebnis (Skalar)
    LOGICAL                       :: res(SIZE(this)) ! 
    !
    res(:) = .NOT. is_omi_arg_readonly( this(:) )
    !
  END FUNCTION is_omi_arg_writeable_1
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
       WRITE(*,*) ' *** Warnung *** Modul "b_omi_arg" nicht initialisiert'
       !
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       !
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_omi_arg ausfuehren'
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
  SUBROUTINE init_omi_arg_all_errors &
       ( )
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER  :: c_upname='init_omi_arg_all_errors' !
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
               '--> INIT_omi_arg ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_omi_arg ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_arg"\n'//&
               'Typ-Komponente = "key"\n'//&
               'aktuell        = <key>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_arg"\n'//&
               'Typ-Komponente = "value"\n'//&
               'aktuell        = <value>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_arg"\n'//&
               'Typ-Komponente = "readonly"\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_arg"\n'//&
               'Typ-Komponente = "description"\n'//&
               'aktuell        = <description>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code in Modul "b_omi_arg" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Fusszeilen\n'//&
               '--> Code in Modul "b_omi_arg" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Index des Datenobjektes (1D-Array)\n'//&
               '--> Code in Modul "b_omi_arg" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_arg"\n'//&
               'Typ-Komponente = "key"\n'//&
               '--> Code in Modul "b_omi_arg" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_arg"\n'//&
               'Typ-Komponente = "value"\n'//&
               '--> Code in Modul "b_omi_arg" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_arg"\n'//&
               'Typ-Komponente = "readonly"\n'//&
               '--> Code in Modul "b_omi_arg" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_arg"\n'//&
               'Typ-Komponente = "description"\n'//&
               '--> Code in Modul "b_omi_arg" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "b_omi_arg"\n'//&
               '--> Code in Modul "b_omi_arg" / Daten pruefen' )
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
  END SUBROUTINE init_omi_arg_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_omi_arg_all_errors &
       ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_omi_arg_all_errors
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
  !! Pr&uuml;fe, ob die Komponente "key" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_arg_key &
       ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_arg) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_omi_arg_key' ! 
    !
    ok = ( LEN_TRIM(this%key) > 0 .AND. this%key(1:LEN(c_undefined)) /= c_undefined )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
       CALL setup_error_act ( '<key>', this%key )
    END IF
    !
  END FUNCTION ok_omi_arg_key
  !
  !! Pr&uuml;fe, ob die Komponente "value" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_arg_value &
       ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_arg) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='ok_omi_arg_value' ! 
    !
    ok = ( LEN_TRIM(this%value) > 0 .AND. this%value(1:LEN(c_undefined)) /= c_undefined )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
       CALL setup_error_act ( '<value>', this%value )
    END IF
    !
  END FUNCTION ok_omi_arg_value
  !
  !! Pr&uuml;fe, ob die Komponente "readonly" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_arg_readonly &
       ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_arg) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='ok_omi_arg_readonly' ! 
    !
    ok = ( this%readonly .OR. .NOT. this%readonly )
    !
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
    !
  END FUNCTION ok_omi_arg_readonly
  !
  !! Pr&uuml;fe, ob die Komponente "description" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_arg_description &
       ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_arg) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=22) , PARAMETER :: c_upname='ok_omi_arg_description' ! 
    !
    ok = ( LEN_TRIM(this%description) > 0 .AND. this%description(1:LEN(c_undefined)) /= c_undefined )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6040, c_upname, c_modname )
       CALL setup_error_act ( '<description>', this%description )
    END IF
    !
  END FUNCTION ok_omi_arg_description
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "key" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_arg_key &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_arg) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_omi_arg_key' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) TRIM(this%key)
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente key - - - - - - - - - - - - - - - - - ',/&
           '# aktuell     = ',A,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_arg_key
  !
  !! Drucke den Inhalt der Komponente "value" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_arg_value &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_arg) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='print_omi_arg_value' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) TRIM(this%value)
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente value - - - - - - - - - - - - - - - - ',/&
           '# aktuell     = ',A,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_arg_value
  !
  !! Drucke den Inhalt der Komponente "readonly" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_arg_readonly &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_arg) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=22) , PARAMETER :: c_upname='print_omi_arg_readonly' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) this%readonly
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente readonly  - - - - - - - - - - - - - - ',/&
           '# aktuell     = ',L1,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_arg_readonly
  !
  !! Drucke den Inhalt der Komponente "description" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_arg_description &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_arg)   , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=25) , PARAMETER  :: c_upname='print_omi_arg_description' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) TRIM(this%description)
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7040, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente description - - - - - - - - - - - - - ',/&
           '# aktuell     = ',A,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_arg_description
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
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
END MODULE b_omi_arg
! TailOfBaseModule --------------------------------------------------------
