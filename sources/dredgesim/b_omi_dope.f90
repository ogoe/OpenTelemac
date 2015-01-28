! -------------------------------------------------------------------------
! HeadOfBaseModule --------------------------------------------------------
!
!! <H2>Typ und Methoden (teilweise) analog zu der OpenMI-Schnittstelle <EM>IDataOperation</EM></h2>
!! @author G. Lang
!! @version 2.1 vom 07/21/05, Quellcode: mod_b_omi_dope.f90
!! <HR>
!! type and methods (in parts) equivalent to OpenMI-interface <EM>IDataOperation</EM> <BR>
!! <HR>
!  Copyright-Hinweis
!                                                                    
!  Copyright (C) 2005 <A HREF="http://www.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A>
!                                                                    
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2005-02-24 : G. Lang : Erstversion
!  01.02 : 2005-03-01 : G. Lang : SHAPE-Befehl in Parameter SHAPE durch explizite Dimensionen ersetzt
!  01.03 : 2005-03-02 : G. Lang : Kopierfunktion "copy_omi_dope"
!  01.04 : 2005-03-03 : G. Lang : Interface "set_omi_dope_auto"
!  01.05 : 2005-03-11 : G. Lang : OPERATORen entfernt, auf Funktionen umgestellt
!  02.01 : 2005-07-21 : G. Lang : Anpassungen fuer korrigiertes ElementSet-Verstaendnis (GEI)
!
!! <HR>
!!
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!!
!! Typ und Methoden analog OpenMI-Interface <EM>IDataOperation</EM>. 
!! Impelementiert einen Typ zur Aufnahme von Informationen und Argumenten 
!! verschiedener <EM>Datenoperationen</EM>, welche in Zusammenhang mit
!! der Definition versciedener Datenoperationen ben&ouml;tigt werden.
!! Typ und Methoden erleichtern den Austausch der zwischen verschiedenen 
!! OpenMI-konformen Komponenten.
!!
!! <OL>
!!    <LI> Initialisierung und De-Initialisierung von skalaren und
!!         vektoriellen Variablen des Typs "t_omi_dope";
!!    <LI> Setzen der Komponenten in Variablen des Typs "t_omi_dope";
!!    <LI> Holen der Komponenten aus Variablen des Typs "t_omi_dope";
!!    <LI> Drucken des Inhalts der Komponenten von Variablen des Typs "t_omi_dope";
!!    <LI> Pr&uuml;fen des Inhalts von Variablen des Typs "t_omi_dope";
!!    <LI> Vergleichen des Inhalts verschiedener Variablen des Typs "t_omi_dope".
!! </OL>
!!
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt den selbst definierten Datentyp t_omi_dope 
!! zur Verf&uuml;gung. Dieser besteht aus den folgenden Komponenten:      <BR>
!! <OL>
!!     <LI> id      : kurzer Identifikationsbezeichner der Datenoperation                                       
!!     <LI> args(:) : Feld mit den zur aktuellen Datenoperation geh&ouml;renden Argumenttwerten                                       
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
!!    <LI> Initialisieren des Moduls b_omi_dope mit INIT-Methode;
!!    <LI> ggf. Verwenden verschiedener SETUP-Methoden;
!!    <LI> Verwenden beliebiger Modul-Methoden (nicht INIT und CLEAR);
!!    <LI> De-Initialisieren des Moduls b_omi_dope mit CLEAR-Methode.
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
!!          die Methode PRINT_OMI_DOPE_ALL_ERRORS.
!
MODULE b_omi_dope
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
  ! [A.3] weitere BASIS-Module (ONLY benutzen!)
  !
  USE b_omi_arg, ONLY :         &
       !   Typdefinitionen
       t_omi_arg,               &
       ! Konstante
       c_len_omi_arg_key,       &
       c_len_omi_arg_value,     &
       !   Routinen / Interfaces
       init_omi_arg,            &
       clear_omi_arg,           &
       setup_omi_arg_prn_lun,   &
       setup_omi_arg_trc_lun,   &
       set_omi_arg_key,         &
       set_omi_arg_value,       &
       set_omi_arg_readonly,    &
       set_omi_arg_description, &
       get_omi_arg_key,         &
       get_omi_arg_value,       &
       get_omi_arg_description, &
       ok_omi_arg,              &
       new_omi_arg,             &
       kill_omi_arg,            &
       print_omi_arg,           &
       eq_omi_arg
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
  !
  !! max. Anzahl der Zeichen in der Komponente <EM>id</EM> 
  INTEGER            , PUBLIC , PARAMETER :: c_len_omi_dope_id=40          ! 
  !
  ! [C.1] (maximal) einen oeffentlich zugaenglichen Typ deklarieren
  !
  !! id      : kurzer Identifikationsbezeichner der Datenoperation <BR>
  !! args(:) : Feld mit den zur aktuellen Datenoperation geh&ouml;renden Argumenttwerten
  TYPE , PUBLIC :: t_omi_dope
     PRIVATE
     CHARACTER (LEN=c_len_omi_dope_id) :: id      ! 
     TYPE (t_omi_arg) , POINTER        :: args(:) !
  END TYPE t_omi_dope
  !
  ! [C.2] Konstantwerte (Parameter) [moeglichst nicht verwenden]
  !
  !! Undefined-Wert f&uuml;r INTEGER-Komponenten
  INTEGER            , PUBLIC , PARAMETER :: c_undef_omi_dope_int=-999              ! 
  !! Undefined-Wert f&uuml;r CHARACTER-Komponenten
  CHARACTER (LEN=9)  , PUBLIC , PARAMETER :: c_undef_omi_dope_char='undefined'      ! 
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
  INTERFACE init_omi_dope
     MODULE PROCEDURE init_omi_dope_d ! 
  END INTERFACE
  !! De-Allokieren/De-Initialisieren der statischen Datenobjekte des Moduls; <BR>
  !! Re-Initialisieren einiger statischer Daten mit Default-Werten.
  INTERFACE clear_omi_dope
     MODULE PROCEDURE clear_omi_dope_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r PRINT-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>PRN_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>PRN_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>PRN_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_omi_dope_prn_lun
     MODULE PROCEDURE setup_omi_dope_prn_lun_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r TRACE-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>TRC_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>TRC_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>TRC_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_omi_dope_trc_lun
     MODULE PROCEDURE setup_omi_dope_trc_lun_d ! 
  END INTERFACE
  !
  !! Erzeugen von Datenobjekten "t_omi_dope"; NULLIFY f&uuml;r dynamische 
  !! Komponenten-Felder und Initialisieren mit Default-Werten: <BR>
  !! a) ein Datenobjekt (Skalar) <BR>
  !! b) viele Datenobjekte (Vektor)
  INTERFACE new_omi_dope
     MODULE PROCEDURE new_omi_dope_0  ! 
     MODULE PROCEDURE new_omi_dope_1  ! 
  END INTERFACE
  !! Vernichten von Datenobjekten "t_omi_dope"; ggf. De-Allokieren von 
  !! Memory und teilweise Re-Initialisieren mit Default-Werten: <BR>
  !! a) ein Datenobjekt (Skalar) <BR>
  !! b) viele Datenobjekte (Vektor)
  INTERFACE kill_omi_dope
     MODULE PROCEDURE kill_omi_dope_0 ! 
     MODULE PROCEDURE kill_omi_dope_1 ! 
  END INTERFACE
  !! Pr&uuml;fen von Datenobjekten "t_omi_dope" auf G&uuml;ltigkeit: <BR>
  !! a) ein Datenobjekt (Skalar)    <BR>
  !! b) viele Datenobjekte (Vektor)
  INTERFACE ok_omi_dope
     MODULE PROCEDURE ok_omi_dope_0 ! 
     MODULE PROCEDURE ok_omi_dope_1 ! 
  END INTERFACE
  !! Drucken von Datenobjekten "t_omi_dope": <BR>
  !! a) ein Datenobjekt (Skalar) <BR>
  !! b) viele Datenobjekte (Vektor) <BR>
  !! Alle Komponenten des Typs "t_omi_ind" auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_dope
     MODULE PROCEDURE print_omi_dope_0 ! 
     MODULE PROCEDURE print_omi_dope_1 ! 
  END INTERFACE
  !! Drucken aller in diesem Modul abgelegten statischen Daten; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_dope_static
     MODULE PROCEDURE print_omi_dope_static_d ! 
  END INTERFACE
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Moduls; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_dope_all_errors
     MODULE PROCEDURE print_omi_dope_all_errors_d ! 
  END INTERFACE
  !
  !! Setze Komponente "id" in "t_omi_dope" auf Benutzerwert: <BR>
  !! a) ein Datenobjekt (Skalar)                             <BR>
  !! b) viele Datenobjekte (Vektor)                          <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_dope_id
     MODULE PROCEDURE set_omi_dope_id_0_0 ! 
     MODULE PROCEDURE set_omi_dope_id_1_0 ! 
  END INTERFACE
  !! Setze Komponente "args(:)" in "t_omi_dope" auf Benutzerwert; <BR>
  !! Allokieren von Memory zur Aufnahme der Benutzer-Daten. <BR>
  !! Die Daten werden auf die interne Komponente kopiert.
  INTERFACE set_omi_dope_args
     MODULE PROCEDURE set_omi_dope_args_0_1 !
  END INTERFACE
  !
  !! Hole Komponente "id" aus "t_omi_dope": <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor) <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  INTERFACE get_omi_dope_id
     MODULE PROCEDURE get_omi_dope_id_0_0 ! Skalar
     MODULE PROCEDURE get_omi_dope_id_1_0 ! Vektor
  END INTERFACE
  !
  !! Hole Komponente "args(:)" aus "t_omi_dope" <BR>
  !! a) f&uuml;r ein Objekt (Skalar)            <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  INTERFACE get_omi_dope_args_ref
     MODULE PROCEDURE get_omi_dope_args_ref_0_1 !
  END INTERFACE
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  !! Erzeugen eines Objekts vom Typ "t_omi_dope" durch Verweis auf
  !! eine der vordefinierten Datenoperatione [1...c_max_dope]
  INTERFACE set_omi_dope_auto
     MODULE PROCEDURE set_omi_dope_auto_0
  END INTERFACE
  !! suche nach einem bestimmten Wert der Komponente <EM>id</EM> in einem 
  !! 1D-Feld des Typs "t_omi_dope": <BR>
  !! a) f&uuml;r einen Wert <EM>id</EM> (Skalar) <BR>
  !! b) f&uuml;r viele Werte <EM>id(:)</EM> (Vektor)
  INTERFACE get_omi_dope_idx
     MODULE PROCEDURE get_omi_dope_idx_1_0
     MODULE PROCEDURE get_omi_dope_idx_1_1
  END INTERFACE
  !! ermittle die Anzahl der Argumente: <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE get_omi_dope_args_count
     MODULE PROCEDURE get_omi_dope_args_count_0
     MODULE PROCEDURE get_omi_dope_args_count_1
  END INTERFACE
  !
  !! Kopiere den Inhalt einer Variablen des Typs "t_omi_dope" in
  !! ein anderes Objekt vom gleichen Typ <BR>
  !! a) ein Quell-Objekt wird in ein Ziel-Objekt kopiert <BR>
  !! a) mehrere Quell-Objekte werden auf mehrere Ziel-Objekte kopiert
  INTERFACE copy_omi_dope
     MODULE PROCEDURE copy_omi_dope_0_0
     MODULE PROCEDURE copy_omi_dope_1_1
  END INTERFACE
  !
  !! Pr&uuml;fen zweier Datenobjekte "t_omi_dope" auf Gleichheit (Funktion) <BR>
  !! a) Skalar1 == Skalar2 <BR>
  !! b) Skalar1 == Vektor2 <BR>
  !! c) Vektor1 == Skalar2 <BR>
  !! d) Vektor1 == Vektor2
  INTERFACE eq_omi_dope
     MODULE PROCEDURE eq_omi_dope_0_0  ! 
     MODULE PROCEDURE eq_omi_dope_0_1  ! 
     MODULE PROCEDURE eq_omi_dope_1_0  ! 
     MODULE PROCEDURE eq_omi_dope_1_1  ! 
  END INTERFACE
  !! Pr&uuml;fen zweier Datenobjekte "t_omi_dope" auf Ungleichheit (Funktion) <BR>
  !! a) Skalar1 /= Skalar2 <BR>
  !! b) Skalar1 /= Vektor2 <BR>
  !! c) Vektor1 /= Skalar2 <BR>
  !! d) Vektor1 /= Vektor2
  INTERFACE ne_omi_dope
     MODULE PROCEDURE ne_omi_dope_0_0  ! 
     MODULE PROCEDURE ne_omi_dope_0_1  ! 
     MODULE PROCEDURE ne_omi_dope_1_0  ! 
     MODULE PROCEDURE ne_omi_dope_1_1  ! 
  END INTERFACE
  !
  ! [C.5] Zuweisungen
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
  PUBLIC :: init_omi_dope
  PUBLIC :: clear_omi_dope
  PUBLIC :: setup_omi_dope_prn_lun
  PUBLIC :: setup_omi_dope_trc_lun
  PUBLIC :: new_omi_dope
  PUBLIC :: kill_omi_dope
  PUBLIC :: ok_omi_dope
  PUBLIC :: print_omi_dope
  PUBLIC :: print_omi_dope_static
  PUBLIC :: print_omi_dope_all_errors
  PUBLIC :: set_omi_dope_id
  PUBLIC :: set_omi_dope_args
  PUBLIC :: get_omi_dope_id
  PUBLIC :: get_omi_dope_args_ref
  PUBLIC :: eq_omi_dope
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: set_omi_dope_auto
  PUBLIC :: get_omi_dope_idx
  PUBLIC :: get_omi_dope_args_count
  PUBLIC :: copy_omi_dope
  PUBLIC :: ne_omi_dope
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
  CHARACTER (LEN=10), PARAMETER :: c_modname      = 'b_omi_dope' ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.          ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1               ! 
  !! Anzahl der Datenkomponenten des Typs t_omi_dope
  INTEGER           , PARAMETER :: c_nofcomp      = 2                ! ggf. modifizieren
  !! Anzahl der verschiedenen Datenoperationen
  INTEGER           , PARAMETER :: c_max_dope=9     ! 
  !! maximale Anzahl der Keys 
  INTEGER           , PARAMETER :: c_max_dope_key=2    ! 
  !! maximale Anzahl der Values
  INTEGER           , PARAMETER :: c_max_dope_value=4  ! 
  !! maximale L&auml;nge der ID (in Zeichen) einer Datenoperation
  INTEGER           , PARAMETER :: c_len_dope_id=20    ! 
  !! maximale L&auml;nge der Beschreibung (in Zeichen) einer Datenoperation
  INTEGER           , PARAMETER :: c_len_dope_descr=40 ! 
  !! maximale L&auml;nge der Keys (in Zeichen) einer Datenoperation
  INTEGER           , PARAMETER :: c_len_dope_key=10   ! 
  !! maximale L&auml;nge der Keys (in Zeichen) einer Datenoperation
  INTEGER           , PARAMETER :: c_len_dope_value=27 ! 
  !! zul&auml;ssige IDs der Datenoperationen
  CHARACTER (LEN=c_len_dope_id) , PARAMETER :: c_dope_id(c_max_dope) = (/ & ! 
       'None                ', 'Aggregate           ', 'Mapping             ', 'Complete            ', & ! 
       'ExtrapolateLinear   ', 'MapToPolyGon        ', 'MapToPolyLine       ', 'MapToPoint          ', & ! 
       'DefMissingValue     ' /) 
  !! Beschreibungen der Datenoperationen
  CHARACTER (LEN=c_len_dope_descr) , PARAMETER :: c_dope_descr(c_max_dope) = (/ & ! 
       'no operation                            ', 'aggregate values                        ', & ! 
       'do mapping                              ', 'do completion                           ', & ! 
       'do linear interpolation                 ', 'do mapping to polygon                   ', & ! 
       'do mapping to polyline                  ', 'do mapping to point                     ', & ! 
       'define missing value                    ' /) 
  !! Keys
  CHARACTER (LEN=c_len_dope_key) , PARAMETER :: c_dope_keys(c_max_dope,c_max_dope_key) = RESHAPE( (/ & ! 
       'Method    ', 'Method    ', 'Method    ', 'Method    ', 'Multiplier', & !  
       'Method    ', 'Method    ', 'Method    ', 'Value     ',               & ! 
       '          ', '          ', '          ', '          ', 'Offset    ', & !  
       '          ', '          ', '          ', '          ' /), SHAPE=(/c_max_dope,c_max_dope_key/) )
  !! Values
  CHARACTER (LEN=c_len_dope_value) , PARAMETER :: c_dope_values(c_max_dope,c_max_dope_value) = RESHAPE( (/ & ! 
       'NoDataOperation            ', 'Average                    ', 'TakeNearest                ', & ! 
       'SetMissingValue            ', 'KeyValue                   ', 'DistributeValues           ', & ! 
       'DistributeValues           ', 'TakeNearest                ', 'KeyValue                   ', & ! 
       !
       'ProvideException           ', 'Cumulate                   ', 'TakeBeginValue             ', & ! 
       'ExtrapolateWithLastGradient', 'KeyValue                   ', 'Interpolate                ', & ! 
       'Interpolate                ', 'Interpolate                ', '                           ', & ! 
       !
       'ProvideAllValues           ', '                           ', 'TakeEndValue               ', & ! 
       'ExtrapolateLinear          ', '                           ', 'UseMatrix                  ', & ! 
       'UseMatrix                  ', 'InverseDistance            ', '                           ', & ! 
       !
       '                           ', '                           ', 'Interpolate                ', & ! 
       '                           ', '                           ', '                           ', & ! 
       '                           ', '                           ', '                           ' /), &
       SHAPE=(/c_max_dope,c_max_dope_value/) )
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
  SUBROUTINE init_omi_dope_d &
       ( )
    !
    USE b_error, ONLY : DEBUG_b
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='init_omi_dope_d' 
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_b > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "b_omi_dope" version 2.1 of 07/21/05                 '
          WRITE(*,*) ' Copyright (C) 2005 Bundesanstalt fuer Wasserbau   '
          WRITE(*,*)
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       IF ( no_error( ) ) CALL init_omi_arg ( )
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_omi_dope_all_errors ( ) 
       ! [1.5] Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.6] endgueltiges Setzen des Initialisierungs-Indikators
       initialised = MERGE( .true., .false., no_error( ) )
    END IF
    ! 2.0 Initialisierungszaehler heraufsetzen
    n_init = n_init + 1
    !
  END SUBROUTINE init_omi_dope_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_omi_dope_d &
       ( )
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER :: c_upname='clear_omi_dope_d' ! 
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_omi_dope_all_errors ( ) 
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.3] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.4] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.4.1] ggf. weitere Module de-initialisieren
       IF ( no_error( ) ) CALL clear_omi_arg ( )
       ! [1.4.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
    !
  END SUBROUTINE clear_omi_dope_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_omi_dope_prn_lun_d &
       ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='setup_omi_dope_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_omi_arg_prn_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_omi_dope_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_omi_dope_trc_lun_d &
       ( lun )
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='setup_omi_dope_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_omi_arg_trc_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_omi_dope_trc_lun_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_omi_dope_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dope) , INTENT(OUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='new_omi_dope_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       this%id = REPEAT( ' ', LEN(this%id) )
       this%id = c_undef_omi_dope_char
       NULLIFY ( this%args )
    END IF
    !
  END SUBROUTINE new_omi_dope_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_omi_dope_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dope) , INTENT(OUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='new_omi_dope_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL new_omi_dope_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_omi_dope_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_omi_dope_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dope) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='kill_omi_dope_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       IF ( no_error( ) ) CALL dealloc_omi_dope_args ( this )
       IF ( no_error( ) ) CALL new_omi_dope_0        ( this )
    END IF
    !
  END SUBROUTINE kill_omi_dope_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_omi_dope_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dope) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='kill_omi_dope_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          DEALLOCATE( this(i)%args )
          CALL kill_omi_dope_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_omi_dope_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_dope_0 &
       ( this )              &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dope) , INTENT(IN) :: this ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=13), PARAMETER :: c_upname='ok_omi_dope_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok(1) = ok_omi_dope_id  ( this )
       l_ok(2) = ok_omi_dope_args( this )
    END IF
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_omi_dope_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_dope_1 &
       ( this )              &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dope) , INTENT(IN) :: this(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=13), PARAMETER :: c_upname='ok_omi_dope_1' 
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
          ok(i) = ok_omi_dope_0 ( this(i) )
       END DO
    END IF
    !
  END FUNCTION ok_omi_dope_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_dope_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dope) , INTENT(IN) :: this ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='print_omi_dope_0' 
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
       IF ( no_error( ) ) CALL print_omi_dope_id( this )
       IF ( no_error( ) ) CALL print_omi_dope_args( this )
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
8000 FORMAT('# Beginn Objekt t_omi_dope ------------------------------')
8001 FORMAT('# Ende   Objekt t_omi_dope ------------------------------')
    !
  END SUBROUTINE print_omi_dope_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_dope_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dope) , INTENT(IN) :: this(:) ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='print_omi_dope_1' 
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
          IF ( no_error( ) ) CALL print_omi_dope_0 ( this(i) )
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
  END SUBROUTINE print_omi_dope_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_dope_static_d &
       ( )
    !! Name der Function
    CHARACTER (LEN=23), PARAMETER :: c_upname='print_omi_dope_static_d' 
    !! Statusvariable
    INTEGER :: i, j, stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )    &
           initialised, prn_op, trc_op, prn_lun, trc_lun, n_init, &
           c_undef_omi_dope_char, c_undef_omi_dope_int
       !
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
          CALL setup_error_act ( '<action>', 'Schreiben (8000)' )
       ELSE
          DO i=1,SIZE(c_dope_id)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8010, IOSTAT=stat ) &
                  TRIM(c_dope_id(i)), TRIM(c_dope_descr(i))
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
                CALL setup_error_act ( '<action>', 'Schreiben (8010)' )
             ELSE
                DO j=1,SIZE(c_dope_keys,2)
                   IF ( stat /= 0 ) EXIT
                   IF ( LEN_TRIM(c_dope_keys(i,j)) > 0 ) THEN
                      WRITE ( UNIT=prn_lun, FMT=8020, IOSTAT=stat ) &
                           j, TRIM(c_dope_keys(i,j))
                   END IF
                END DO
                IF ( stat /= 0 ) THEN
                   CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
                   CALL setup_error_act ( '<action>', 'Schreiben (8020)' )
                ELSE
                   DO j=1,SIZE(c_dope_values,2)
                      IF ( stat /= 0 ) EXIT
                      IF ( LEN_TRIM(c_dope_values(i,j)) > 0 ) THEN
                         WRITE ( UNIT=prn_lun, FMT=8030, IOSTAT=stat ) &
                              j, TRIM(c_dope_values(i,j))
                      END IF
                   END DO
                   IF ( stat /= 0 ) THEN
                      CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
                      CALL setup_error_act ( '<action>', 'Schreiben (8030)' )
                   END IF
                END IF
             END IF
          END DO
       END IF
       !
       IF ( no_error( ) ) CALL print_omi_dope_all_errors_d ( )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls b_omi_dope         ',/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
    '#   initialised = ',L1,/  &
    '#        prn_op = ',L1,/  &
    '#        trc_op = ',L1,/  &
    '#       prn_lun = ',I5,/  &
    '#       trc_lun = ',I5,/  &
    '#        n_init = ',I5,/  &
    '#   undef[char] = ',A,/   &
    '#    undef[int] = ',I10,/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
    '# es folgen Informationen zu den Datenoperationen - - - - - -') 
8010 FORMAT( '# ID = ',A,' [ ',A,' ] ' )
8020 FORMAT( '#      - Key   Nr. ',I1,', Wert = ',A )
8030 FORMAT( '#      - Value Nr. ',I1,', Wert = ',A )
    !
  END SUBROUTINE print_omi_dope_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_dope_all_errors_d &
       ( )
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=27), PARAMETER :: c_upname='print_omi_dope_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_omi_dope_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! weise der Komponente "id" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_dope_id_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dope) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "id"
    CHARACTER (LEN=*) , INTENT(IN)    :: val  ! 
    !
    this%id = REPEAT( ' ', LEN(this%id) )
    this%id = val(1:MIN(LEN(this%id),LEN_TRIM(val)))
    !
  END SUBROUTINE set_omi_dope_id_0_0
  !
  !! weise der Komponente "id" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_dope_id_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dope) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "id"
    CHARACTER (LEN=*) , INTENT(IN)    :: val     ! 
    !! Hilfsvariable
    INTEGER                           :: i       ! 
    !
    DO i=1,SIZE(this)
       CALL set_omi_dope_id_0_0 ( this(i), val )
    END DO
    !
  END SUBROUTINE set_omi_dope_id_1_0
  !
  !! weise der dynamischen Komponente "args(:)" ein Feld zu (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen <BR>
  !! die Daten werden auf die interne Komponente kopiert
  SUBROUTINE set_omi_dope_args_0_1 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dope) , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "args(:)"
    TYPE (t_omi_arg)  , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='set_omi_dope_args_0_1' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_omi_dope_args ( this            )
       IF ( no_error( ) ) CALL alloc_omi_dope_args   ( this, SIZE(val) )
       IF ( no_error( ) ) CALL init_omi_dope_args    ( this            )
       IF ( no_error( ) ) this%args(:) = val(:)
    END IF
    !
  END SUBROUTINE set_omi_dope_args_0_1
  !
  !! automatisches Erzeugen eines Datenobjekts durch Verweis auf eine der
  !! vordefinierten Datenoperationen [1...c_max_dope] <BR>
  !! Unterprogramm erzeugt Fehlermeldungen
  SUBROUTINE set_omi_dope_auto_0 ( this, idx, value_d )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dope)             , INTENT(OUT) :: this    ! 
    !! Zeiger auf einen Eintrag in Konstant-Feld "c_dope_id(1:c_max_dope)"
    INTEGER                       , INTENT(IN)  :: idx     ! 
    !! (optional) Feld mit KeyValue-Werten (falls erforderlich)
    REAL (KIND=Double) , OPTIONAL , INTENT(IN)  :: value_d(:) ! 
    !! Name des Unterprogramms
    CHARACTER (LEN=19) , PARAMETER :: c_upname='set_omi_dope_auto_0' ! 
    !! Hilfsvariable
    CHARACTER (LEN=10) :: ch ! 
    CHARACTER (LEN=c_len_omi_arg_value) :: chv ! 
    INTEGER            :: i, j, nn, mm, nkr, nka, nkm, nk, nkv, narg ! 
    TYPE (t_omi_arg) , ALLOCATABLE :: args(:) ! 
    !
    CALL new_omi_dope_0 ( this )
    IF ( idx >= 1 .AND. idx <= SIZE(c_dope_id) ) THEN
       nkr = get_values_count ( c_dope_values(idx,:), 'KeyValue' )
       IF ( PRESENT( value_d ) ) THEN
          nka = SIZE(value_d)
       ELSE
          nka = 0
       END IF
       IF ( nka == nkr ) THEN
          nk   = get_values_count ( c_dope_keys(idx,:) )
          nkm  = get_values_count ( c_dope_keys(idx,:), 'Method' )
          nkv  = get_values_count ( c_dope_values(idx,:) )
          narg = MERGE( nkv, nk, nkm == 1 )
          nn   = 0
          mm   = 0
          ALLOCATE( args(narg) )
          CALL new_omi_arg ( args )
          DO j=1,nk
             DO i=1,nkv/nk
                nn = nn + 1
                CALL set_omi_arg_key         ( args(nn), c_dope_keys(idx,j)    )
                CALL set_omi_arg_description ( args(nn), c_dope_descr(idx)     )
                CALL set_omi_arg_readonly    ( args(nn), .true.                )
                IF ( get_values_count( c_dope_values(idx,nn:nn), 'KeyValue' ) == 1 ) THEN
                   mm  = mm + 1
                   chv = REPEAT( ' ', LEN(chv) )
                   WRITE(chv,'(G15.6)') value_d(mm)
                   CALL set_omi_arg_value ( args(nn), chv )
                ELSE
                   CALL set_omi_arg_value ( args(nn), c_dope_values(idx,nn) )
                END IF
             END DO
          END DO
          CALL set_omi_dope_id_0_0 ( this, c_dope_id(idx) )
          CALL set_omi_dope_args_0_1 ( this, args(:) )
          CALL kill_omi_arg ( args )
          DEALLOCATE( args )
       ELSE
          CALL setup_error_act ( all_errors(:), 8510, c_upname, c_modname )
          WRITE(ch,'(I10)') nka ; CALL setup_error_act ( '<actual>', TRIM(ch) )
          WRITE(ch,'(I10)') nkr ; CALL setup_error_act ( '<required>', TRIM(ch) )
       END IF
    ELSE
       CALL setup_error_act ( all_errors(:), 8500, c_upname, c_modname )
       WRITE(ch,'(I10)') idx ; CALL setup_error_act ( '<actual>', TRIM(ch) )
       WRITE(ch,'(I10)') SIZE(c_dope_id) ; CALL setup_error_act ( '<max>', TRIM(ch) )
    END IF
    !
  END SUBROUTINE set_omi_dope_auto_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! hole die Komponente "id" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_dope_id_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dope)   , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "id" (Skalar)
    CHARACTER (LEN=c_len_omi_dope_id) :: val  ! 
    !
    val = this%id
    !
  END FUNCTION get_omi_dope_id_0_0
  !
  !! hole die Komponente "id" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_dope_id_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dope)   , INTENT(IN)  :: this(:)          ! 
    !! R&uuml;ckgabewert "id"
    CHARACTER (LEN=c_len_omi_dope_id) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%id
    !
  END FUNCTION get_omi_dope_id_1_0
  !
  !! hole die dynamische Feld-Komponente "args(:)" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  FUNCTION get_omi_dope_args_ref_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dope) , INTENT(IN)  :: this   ! 
    !! R&uuml;ckgabewert "args(:)" (Vektor)
    TYPE (t_omi_arg)  , POINTER     :: val(:) ! 
    !
    val => this%args
    !
  END FUNCTION get_omi_dope_args_ref_0_1
  !
  !! suche nach einem bestimmten Wert der Komponente <EM>id</EM> in einem 
  !! 1D-Feld des Typs "t_omi_dope" f&uuml;r einen Wert <EM>id</EM> (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_dope_idx_1_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dope)  , INTENT(IN) :: this(:) ! 
    !! zu suchender Wert der Komponente <EM>id</EM>
    CHARACTER (LEN=*) , INTENT(IN) :: val     ! 
    !! Ergebnis: Zeiger auf einen Eintrag in this(:), deren aktueller Wert
    !! der Komponente <EM>id</EM> identisch mit <EM>val</EM> ist <BR>
    !! wird kein Eintrag gefunden, so wird "undefiniert" zur&uuml;ckgegeben
    INTEGER :: res ! 
    ! Hilfsvariablen
    INTEGER :: i, l1, l2 ! 
    !
    res = c_undef_omi_dope_int
    l1  = LEN_TRIM(val)
    DO i=1,SIZE(this)
       IF ( res /= c_undef_omi_dope_int ) EXIT
       l2 = LEN_TRIM(this(i)%id)
       IF ( l1 == l2 ) THEN
          IF ( val(1:l1) == this(i)%id(1:l2) ) res = i
       END IF
    END DO
    !
  END FUNCTION get_omi_dope_idx_1_0
  !
  !! suche nach einem bestimmten Wert der Komponente <EM>id</EM> in einem 
  !! 1D-Feld des Typs "t_omi_dope" f&uuml;r viele Werte <EM>id(:)</EM> (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_dope_idx_1_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dope)  , INTENT(IN) :: this(:) ! 
    !! zu suchende Werte der Komponente <EM>id</EM>
    CHARACTER (LEN=*) , INTENT(IN) :: val(:)  ! 
    !! Ergebnis: Zeiger auf die Eintr&auml;ge in this(:), deren aktueller 
    !! Wert der Komponente <EM>id</EM> identisch mit <EM>id(:)</EM> ist <BR>
    !! wird kein Eintrag gefunden, so wird "undefiniert" zur&uuml;ckgegeben
    INTEGER :: res(SIZE(val)) ! 
    ! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(val)
       res(i) = get_omi_dope_idx_1_0 ( this(:), val(i) )
    END DO
    !
  END FUNCTION get_omi_dope_idx_1_1
  !
  !! ermittle die Anzahl der Argumente in einem Datenobjekt <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_dope_args_count_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dope) , INTENT(IN) :: this ! 
    !! Ergebnis: Anzahl der Argumente
    INTEGER :: res ! 
    !
    IF ( ASSOCIATED( this%args ) ) THEN
       res = SIZE(this%args)
    ELSE
       res = 0
    END IF
    !
  END FUNCTION get_omi_dope_args_count_0
  !
  !! ermittle die Anzahl der Argumente in vielen Datenobjekten <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_dope_args_count_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dope) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Anzahl der Argumente
    INTEGER :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_dope_args_count_0 ( this(i) )
    END DO
    !
  END FUNCTION get_omi_dope_args_count_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_dope_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_dope) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_dope) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1) =    ( this1%id      == this2%id      )
    l_ok(2) = ALL( eq_omi_arg( this1%args(:), this2%args(:) ) )
    !
    ok = ALL( l_ok )
    !
  END FUNCTION eq_omi_dope_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_dope_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_dope) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_dope) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    ! 
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_dope_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_omi_dope_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_dope_0_1 ( this1, this2 ) &
       RESULT( ok )
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_omi_dope) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_dope) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_dope_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_omi_dope_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_dope_1_1 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_dope) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_dope) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_dope_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_omi_dope_1_1
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
  FUNCTION ne_omi_dope_0_0 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_dope) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_dope) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=15), PARAMETER :: c_upname='ne_omi_dope_0_0' 
    !
    ok = .NOT. eq_omi_dope_0_0( this1, this2 )
    !
  END FUNCTION ne_omi_dope_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_dope_1_0 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_dope) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_dope) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ok(:) = .NOT. eq_omi_dope_1_0( this1(:), this2 )
    !
  END FUNCTION ne_omi_dope_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Vektor )<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION ne_omi_dope_0_1 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_dope) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_dope) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ok(:) = .NOT. eq_omi_dope_0_1( this1, this2(:) )
    !
  END FUNCTION ne_omi_dope_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_dope_1_1 ( this1, this2 ) &
       RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_dope) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_dope) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Hilfsvariable
    INTEGER :: l ! 
    !
    l       = SIZE(ok)
    ok(1:l) = .NOT. eq_omi_dope_1_1( this1(1:l), this2(1:l) )
    !
  END FUNCTION ne_omi_dope_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-Copy-Methoden <<<
  ! ----------------------------------------------------------------------
  !
  !! kopiere den Inhalt einer Komponente in eine andere Komponente <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE copy_omi_dope_0_0 ( this1, this2 )
    !! Ziel-Datenobjekt
    TYPE (t_omi_dope) , INTENT(OUT) :: this1 ! 
    !! Quell-Datenobjekt
    TYPE (t_omi_dope) , INTENT(IN)  :: this2 ! 
    !
    CALL new_omi_dope_0      ( this1 )
    CALL set_omi_dope_id_0_0 ( this1, this2%id          )
    IF ( ASSOCIATED( this2%args ) ) CALL set_omi_dope_args_0_1 ( this1, this2%args )
    !
  END SUBROUTINE copy_omi_dope_0_0
  !
  !! kopiere den Inhalt mehrere Komponente auf andere Komponenten <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE copy_omi_dope_1_1 ( this1, this2 )
    !! Ziel-Datenobjekt
    TYPE (t_omi_dope) , INTENT(OUT) :: this1(:) ! 
    !! Quell-Datenobjekt
    TYPE (t_omi_dope) , INTENT(IN)  :: this2(:) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,MIN(SIZE(this1),SIZE(this2))
       CALL copy_omi_dope_0_0 ( this1(i), this2(i) )
    END DO
    !
  END SUBROUTINE copy_omi_dope_1_1
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
       WRITE(*,*) ' *** Warnung *** Modul "b_omi_dope" nicht initialisiert'
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_omi_dope ausfuehren'
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
  SUBROUTINE init_omi_dope_all_errors &
       ( )
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER  :: c_upname='init_omi_dope_all_errors' !
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
               '--> INIT_omi_dope ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_omi_dope ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_omi_dope"\n'//&
               'Typ-Komponente = "args(:)"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               '--> Code in Modul "b_omi_dope" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_dope"\n'//&
               'Typ-Komponente = "id"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'aktuell        = <aktuell>\n'//&
               'definiert      = <defined>\n'//&
               'gueltig        = <valid>\n'//&
               '--> es muss ein Wert aus "c_dope_id" gewaehlt werden\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_dope"\n'//&
               'Typ-Komponente = "args(:)"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'o.k.           = <ok> [ nach b_omi_arg ] \n'//&
               'Key1           = <key1>, <idx-key1>\n'//&
               'Value1         = <val1>, <idx-val1>\n'//&
               'Key2           = <key2>, <idx-key2>\n'//&
               'Value2         = <val2>, <idx-val2>\n'//&
               'Key3           = <key3>, <idx-key3>\n'//&
               'Value3         = <val3>, <idx-val3>\n'//&
               'Key4           = <key4>, <idx-key4>\n'//&
               'Value4         = <val4>, <idx-val4>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code in Modul "b_omi_dope" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Fusszeilen\n'//&
               '--> Code in Modul "b_omi_dope" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Index des Datenobjektes (1D-Array)\n'//&
               '--> Code in Modul "b_omi_dope" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_dope"\n'//&
               'Typ-Komponente = "id"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               '--> Code in Modul "b_omi_dope" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_dope"\n'//&
               'Typ-Komponente = "args(:)"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               'Aktion         = <action>\n'//&
               '--> Code in Modul "b_omi_dope" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "b_omi_dope"\n'//&
               'Aktion         = <action>\n'//&
               '--> Code in Modul "b_omi_dope" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_omi_dope"\n'//&
               'Typ-Komponente = "args(:)"\n'//&
               'Objekt-Id      = <obj-id>\n'//&
               '--> Code in Modul "b_omi_dope" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SET-Methoden (auto)\n'//&
               'fehlerhafter Zeiger auf Position in "c_dope_id(:)"\n'//&
               'aktuell      = <actual>\n'//&
               'erforderlich = [1,<max>]\n'//&
               '--> CALL SET_OMI_DOPE_AUTO mit korrekter Parameterliste rufen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8510 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SET-Methoden (auto)\n'//&
               'inkonsistent Anzahl von "KeyValues" (Parameter "value_d(:)")\n'//&
               'aktuell      = <actual>\n'//&
               'erforderlich = <required>\n'//&
               '--> CALL SET_OMI_DOPE_AUTO mit korrekter Parameterliste rufen' )
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
  END SUBROUTINE init_omi_dope_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_omi_dope_all_errors &
       ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_omi_dope_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Allokieren der dynamischen Feld-Komponente "args(:)" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_omi_dope_args ( this, idim )
    !! Datenobjekt
    TYPE (t_omi_dope) , INTENT(INOUT) :: this ! 
    !! Dimension der dynamischen Feld-Komponente "args(:)"
    INTEGER           , INTENT(IN)    :: idim ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='alloc_omi_dope_args' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%args(idim), STAT=stat )
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 8020, c_upname, c_modname, stat )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
    END IF
    !
  END SUBROUTINE alloc_omi_dope_args
  !
  !! Initialisieren der Feld-Komponente "args(:)" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_omi_dope_args ( this )
    !! Datenobjekt
    TYPE (t_omi_dope) , INTENT(INOUT) :: this   ! 
    !
    CALL new_omi_arg ( this%args(:) )
    !
  END SUBROUTINE init_omi_dope_args
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren der dynamischen Feld-Komponente "args(:)" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_omi_dope_args ( this )
    !! Datenobjekt
    TYPE (t_omi_dope) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER  :: c_upname='dealloc_omi_dope_args' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%args ) ) THEN
       DEALLOCATE ( this%args, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5020, c_upname, c_modname, stat )
          CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       END IF
       NULLIFY ( this%args ) 
    END IF
    !
  END SUBROUTINE dealloc_omi_dope_args
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe, ob die Komponente "id" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_dope_id ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_dope) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_omi_dope_id' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=1) :: ch ! 
    LOGICAL           :: l_ok(2) ! 
    !
    l_ok(1) = ( LEN_TRIM(this%id) > 0 .AND. &
         this%id(1:LEN(c_undef_omi_dope_char)) /= c_undef_omi_dope_char )
    l_ok(2) = ( get_c_dope_id_idx( this ) > 0 ) 
    ok = ALL( l_ok(:) )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
       CALL setup_error_act ( '<aktuell>', TRIM(this%id) )
       WRITE(ch,'(L1)') l_ok(1) ; CALL setup_error_act ( '<defined>', ch )
       WRITE(ch,'(L1)') l_ok(2) ; CALL setup_error_act ( '<valid>', ch )
    END IF
    !
  END FUNCTION ok_omi_dope_id
  !
  !! Pr&uuml;fe, ob die Komponente "args(:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_dope_args ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_dope) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='ok_omi_dope_args' ! 
    !! Hilfsvariable
    CHARACTER (LEN=40)                  :: ch    ! 
    CHARACTER (LEN=c_len_omi_arg_key)   :: key   ! 
    CHARACTER (LEN=c_len_omi_arg_value) :: value ! 
    LOGICAL , ALLOCATABLE               :: l_ok(:)  ! 
    INTEGER , ALLOCATABLE               :: l_key(:), l_val(:) ! 
    INTEGER                             :: i, nn ! 
    !
    IF ( ASSOCIATED( this%args) ) THEN
       ALLOCATE( l_ok(SIZE(this%args)), l_key(SIZE(this%args)), l_val(SIZE(this%args)) )
       l_ok(:)  = ok_omi_arg( this%args(:) )
       l_key(:) = -1
       nn       = get_c_dope_id_idx ( this )
       IF ( nn > 0 ) THEN
          l_key(:) = get_c_dope_keys_idx  ( this, nn )
          l_val(:) = get_c_dope_values_idx( this, nn )
       END IF
       ok      = ( ALL(l_ok) .AND. ALL(l_key>0) .AND. ALL(l_val>=0) )
       !
       IF ( .NOT. ok ) THEN
          ch = REPEAT( ' ', LEN(ch) )
          CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
          CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
          DO i=1,MIN(LEN(ch),SIZE(l_ok))
             WRITE(ch(i:i),'(L1)') l_ok(i)
          END DO
          CALL setup_error_act ( '<ok>', TRIM(ch) )
          DO i=1,SIZE(l_key)
             key   = get_omi_arg_key  ( this%args(i) )
             value = get_omi_arg_value( this%args(i) )
             WRITE(ch(1:5),'(I2,A1,I2)') l_key(i),' ',l_val(i)
             SELECT CASE ( i )
             CASE ( 1 )
                CALL setup_error_act ( '<key1>', TRIM(key) ) ; CALL setup_error_act ( '<idx-key1>', ch(1:2) )
                CALL setup_error_act ( '<val1>', TRIM(value) ) ; CALL setup_error_act ( '<idx-val1>', ch(4:5) )
             CASE ( 2 )
                CALL setup_error_act ( '<key2>', TRIM(key) ) ; CALL setup_error_act ( '<idx-key2>', ch(1:2) )
                CALL setup_error_act ( '<val2>', TRIM(value) ) ; CALL setup_error_act ( '<idx-val2>', ch(4:5) )
             CASE ( 3 )
                CALL setup_error_act ( '<key3>', TRIM(key) ) ; CALL setup_error_act ( '<idx-key3>', ch(1:2) )
                CALL setup_error_act ( '<val3>', TRIM(value) ) ; CALL setup_error_act ( '<idx-val3>', ch(4:5) )
             CASE ( 4 )
                CALL setup_error_act ( '<key4>', TRIM(key) ) ; CALL setup_error_act ( '<idx-key4>', ch(1:2) )
                CALL setup_error_act ( '<val4>', TRIM(value) ) ; CALL setup_error_act ( '<idx-val4>', ch(4:5) )
             END SELECT
          END DO
       END IF
       !
       DEALLOCATE( l_ok, l_key, l_val )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION ok_omi_dope_args
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "id" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_dope_id ( this )
    !! Datenobjekt
    TYPE (t_omi_dope) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_omi_dope_id' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) TRIM(this%id)
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat ) 
       CALL setup_error_act ( '<obj-id>', TRIM(this%id) )
    END IF
    !
8000 FORMAT &
          ('# Inhalt der Komponente id  - - - - - - - - - - - - - - - - - ',/&
           '# aktuell = ',A,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_dope_id
  !
  !! Drucke den Inhalt der Komponente "args(:)" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_dope_args ( this )
    !! Datenobjekt
    TYPE (t_omi_dope) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='print_omi_dope_args' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%args ) ) THEN
       WRITE (UNIT=prn_lun,FMT=8000,IOSTAT=stat) 
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
          CALL setup_error_act ( '<action>', 'Header-Zeile (8000)' )
       ELSE
          CALL print_omi_arg ( this%args(:) )
          IF ( any_error( ) ) THEN
             CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
             CALL setup_error_act ( '<action>', 'CALL print_omi_arg' )
          END IF
       END IF
       IF ( stat == 0 ) THEN
          WRITE ( UNIT=prn_lun, FMT=8020, IOSTAT=stat ) 
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
             CALL setup_error_act ( '<action>', 'Foot-Zeile (8020)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT &
          ('# Inhalt der Komponente args(:) - - - - - - - - - - - - - - - ')
8020 FORMAT &
          ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_dope_args
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
  ! >>> PRIVATE-OPERATOR(>=)-Methoden <<< [ERR_NO = 18000 bis 18999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-GET-Methoden <<< 
  ! ----------------------------------------------------------------------
  !
  !! Ermittle die Position der ID in "c_dope_id" <BR>
  !! Funktion erzeugt keine Fehlermeldungen
  FUNCTION get_c_dope_id_idx ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dope) , INTENT(IN) :: this ! 
    !! Ergebnis: Zeiger auf Eintrag in "c_dope_id" oder -1 falls nicht vorhanden
    INTEGER :: res  ! 
    !! Hilfsvariablen
    INTEGER :: i, l ! 
    !
    res = -1
    l   = LEN_TRIM(this%id)
    !
    DO i=1,SIZE(c_dope_id)
       IF ( res /= -1 ) EXIT
       IF ( l == LEN_TRIM(c_dope_id(i)) ) THEN
          IF ( this%id(1:l) == c_dope_id(i)(1:l) ) res = i
       END IF
    END DO
    !
  END FUNCTION get_c_dope_id_idx
  !
  !! Ermittle die Position eines Keys in "c_dope_keys(i,:)" <BR>
  !! Funktion erzeugt keine Fehlermeldungen
  FUNCTION get_c_dope_keys_idx ( this, nn ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dope) , INTENT(IN) :: this ! 
    !! Zeiger auf Position in "c_dope_keys(nn,:)" 
    INTEGER           , INTENT(IN) :: nn   ! 
    !! Ergebnis: Zeiger auf Eintrag in "c_dope_keys(nn,:)" oder -1 falls nicht vorhanden
    INTEGER :: res(SIZE(this%args)) ! 
    !! Hilfsvariablen
    CHARACTER (LEN=c_len_omi_arg_key) :: key     ! 
    INTEGER                           :: i, j, l ! 
    !
    res(:) = -1
    IF ( nn >= 1 .AND. nn <= SIZE(c_dope_keys,1) ) THEN
       DO i=1,SIZE(res)
          key = get_omi_arg_key( this%args(i) )
          l   = LEN_TRIM(key)
          DO j=1,SIZE(c_dope_keys,2)
             IF ( res(i) /= -1 ) EXIT
             IF ( LEN_TRIM(c_dope_keys(nn,j)) == 0 ) CYCLE
             IF ( l == LEN_TRIM(c_dope_keys(nn,j)) ) THEN
                IF ( key(1:l) == c_dope_keys(nn,j)(1:l) ) res(i) = j
             END IF
          END DO
       END DO
    END IF
    !
  END FUNCTION get_c_dope_keys_idx
  !
  !! Ermittle f&uuml;r Key="Method" den Zeiger auf "c_dope_value(nn,:)" <BR>
  !! Funktion erzeugt keine Fehlermeldungen
  FUNCTION get_c_dope_values_idx ( this, nn ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dope) , INTENT(IN) :: this ! 
    !! Zeiger auf Position in "c_dope_values(nn,:)" 
    INTEGER           , INTENT(IN) :: nn   ! 
    !! Ergebnis: Zeiger auf Eintrag in "c_dope_values(nn,:)" oder -1 falls nicht vorhanden
    !! oder 0 falls nicht erforderlich
    INTEGER :: res(SIZE(this%args)) ! 
    !! Hilfsvariablen
    CHARACTER (LEN=c_len_omi_arg_value) :: value   ! 
    CHARACTER (LEN=c_len_omi_arg_key  ) :: key     ! 
    INTEGER                             :: i, j, l ! 
    !
    res(:) = -1
    IF ( nn >= 1 .AND. nn <= SIZE(c_dope_values,1) ) THEN
       DO i=1,SIZE(res)
          key   = get_omi_arg_key  ( this%args(i) )
          value = get_omi_arg_value( this%args(i) )
          IF ( key(1:6) == 'Method' ) THEN
             l = LEN_TRIM(value)
             DO j=1,SIZE(c_dope_values,2)
                IF ( res(i) /= -1 ) EXIT
                IF ( LEN_TRIM(c_dope_values(nn,j)) == 0 ) CYCLE
                IF ( l == LEN_TRIM(c_dope_values(nn,j)) ) THEN
                   IF ( value(1:l) == c_dope_values(nn,j)(1:l) ) res(i) = j
                END IF
             END DO
          ELSE
             res(i) = 0
          END IF
       END DO
    END IF
    !
  END FUNCTION get_c_dope_values_idx
  !
  !! Ermittle die Anzahl der spezifischen "val" einer Datenoperation 
  !! oder, falls val nicht vorhanden, die Anzahl der von Null 
  !! verschiedenen Strings <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_values_count ( arr, val ) &
       RESULT( res )
    !! Feld in dem nach dem Auftreten von "val" gesucht werden soll
    CHARACTER (LEN=*) , INTENT(IN) :: arr(:) ! 
    !! (OPTIONAL) String nach dem gesucht werden soll, falls nicht 
    !! vorhanden, dann wird die Anzahl der von Null verschiedenen
    !! Strings zur&uuml;ckgegeben
    CHARACTER (LEN=*) , OPTIONAL, INTENT(IN) :: val ! 
    !! Ergebnis: Anzahl der "KeyValue" in "arr(:)"
    INTEGER :: res ! 
    !! Hilfsvariable
    INTEGER :: l   ! 
    !
    IF ( PRESENT(val) ) THEN
       l   = MIN(LEN_TRIM(val),LEN(arr))
       res = COUNT( arr(:)(1:l) == val(1:l) )
    ELSE
       res = COUNT( LEN_TRIM(arr) > 0 )
    END IF
    !
  END FUNCTION get_values_count
  !
END MODULE b_omi_dope
! TailOfBaseModule --------------------------------------------------------
