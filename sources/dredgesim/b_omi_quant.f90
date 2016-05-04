! -------------------------------------------------------------------------
! HeadOfBaseModule --------------------------------------------------------
!
!! <H2>Typ und Methoden analog OpenMI-Interface <EM>IQuantity</EM></h2>
!! @author G. Lang
!! @version 3.1 vom 12/21/06, Quellcode: mod_b_omi_quant.f90
!! <HR>
!! type and methods equivalent to OpenMI interface <EM>IQuantity</EM> <BR>
!! <HR>
!  Copyright-Hinweis
!                                                                    <BR>
!  Copyright (C) 2005 <A HREF="http://www.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!                                                                    <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2005-02-07 : G. Lang : Erstversion (unvollstaendig)
!  01.02 : 2005-03-01 : G. Lang : SHAPE-Befehl in Parameter SHAPE durch explizite Dimensionen ersetzt
!  01.03 : 2005-03-01 : G. Lang : + set_omi_quant[_code|_descr]
!  01.04 : 2005-03-02 : G. Lang : Kopierfunktion "copy_omi_quant"
!  01.05 : 2005-03-02 : G. Lang : Schreibkorrekturen
!  01.06 : 2005-03-11 : G. Lang : OPERATORen entfernt, auf Funktionen umgestellt
!  01.07 : 2004-05-09 : G. Lang : Erweiterungen fuer io_dataset-Einbindung
!  02.01 : 2005-07-21 : G. Lang : Anpassungen fuer korrigiertes ElementSet-Verstaendnis (GEI)
!  02.02 : 2005-08-10 : G. Lang : neue FUN is_omi_quant_magnitude, automat. Anpassung 
!                                 der Dimension sowie der Einheit bei Flussgroessen
!  02.03 : 2006-05-02 : G. Lang : get_omi_quant_magnitude_quant 
!  02.04 : 2006-05-02 : G. Lang : kleinen Indexdreher in vorgenannter Funktion entfernt
!  03.01 : 2006-12-21 : G. Lang : Umstellen der SI-Basiseinheiten auf Real-Zahlen
!
!!                                                                  <BR>
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!!
!! Typ und Methoden analog OpenMI-Interface <EM>IQuantity</EM>. Dient
!! dazu, die eine physikalische Gr&ouml;&szlig;e n&auml;her zu 
!! beschreiben, die zwischen verschiedenen OpenMI-konformen Komponenten 
!! ausgetauscht werden soll.
!!
!! <OL>
!!    <LI> Initialisierung und De-Initialisierung von skalaren und
!!         vektoriellen Variablen des Typs "t_omi_quant";
!!    <LI> Setzen der Komponenten in Variablen des Typs "t_omi_quant";
!!    <LI> Holen der Komponenten aus Variablen des Typs "t_omi_quant";
!!    <LI> Drucken des Inhalts der Komponenten von Variablen des Typs "t_omi_quant";
!!    <LI> Pr&uuml;fen des Inhalts von Variablen des Typs "t_omi_quant";
!!    <LI> Vergleichen des Inhalts verschiedener Variablen des Typs "t_omi_quant".
!! </OL>
!!                                                         
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt den selbst definierten Datentyp "t_omi_quant"
!! zur Verf&uuml;gung. Dieser besteht aus den folgenden Komponenten:
!! <OL>
!!     <LI> id          : kurzer Identifikationsbezeichner der Daten
!!     <LI> description : erg&auml;nzende verbale Beschreibung der Daten
!!     <LI> valuetype   : Typ der Daten (Skalar=1, Vektor=2)
!!     <LI> dimension   : SI-Basisdimension der Daten (Typ "t_omi_dim")
!!     <LI> unit        : aktuelle phys. Einheit der Daten (Typ "t_omi_unit")
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
!!    <LI> Initialisieren des Moduls b_omi_quant mit INIT-Methode;
!!    <LI> ggf. Verwenden verschiedener SETUP-Methoden;
!!    <LI> Verwenden beliebiger Modul-Methoden (nicht INIT und CLEAR);
!!    <LI> De-Initialisieren des Moduls b_omi_quant mit CLEAR-Methode.
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
!!          die Methode PRINT_OMI_QUANT_ALL_ERRORS.
!!                                                                    <BR>
!
MODULE b_omi_quant
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
  ! [A.3] BASIS-Modul mit grundlegenden Informationen zu physikalischen Gr&ouml;&szlig;en
  !
  USE b_phy, ONLY :                &
       !   Routinen / Interfaces
       init_phy,                   &
       clear_phy,                  &
       setup_phy_prn_lun,          &
       setup_phy_trc_lun,          &
       setup_phy_language,         &
       ok_phy,                     &
       get_phy_quant_code,         &
       get_phy_quant_ref_code,     &
       get_phy_quant_id,           &
       get_phy_quant_descr,        &
       is_phy_quant_valid,         &
       is_phy_quant_scalar,        &
       is_phy_quant_vector
  !
  ! [A.4] BASIS-Modul mit Typ und Methoden Variablenbezeichner
  !
  USE b_var, ONLY :       &
       ! Datentyp
       t_var,             &
       ! Routinen / Interfaces
       init_var,          &
       clear_var,         &
       setup_var_prn_lun, &
       setup_var_trc_lun, &
       get_var_id,        &
       get_var_name,      &
       get_var_dim_id
  !
  ! [A.5] BASIS-Modul mit Typ und Methoden Attributbezeichner
  !
  USE b_att, ONLY :       &
       ! Datentyp
       t_att,             &
       ! Konstante
       c_att_name,        &
       ! Routinen / Interfaces
       init_att,          &
       clear_att,         &
       setup_att_prn_lun, &
       setup_att_trc_lun, &
       is_att_in,         &
       is_att_ch,         &
       get_att_in,        &
       get_att_ch,        &
       get_att_idx,            &
       get_att_nof_values,     &
       get_att_idx_for_var_id 
  !
  ! [A.6] BASIS-Modul mit Typ und Methoden Dimensionsbezeichner
  !
  USE b_dim, ONLY :                 &
       ! Datentyp
       t_dim,                       &
       ! Konstante
       c_dim_name,                  &
       ! Routinen / Interfaces
       init_dim,                    &
       clear_dim,                   &
       setup_dim_prn_lun,           &
       setup_dim_trc_lun,           &
       get_dim_idx,                 &
       get_dim_len,                 &
       is_vector_class_dim,         &
       is_variant_class_dim,        &
       is_vector_class_dim,         &
       is_variant_class_dim,        &
       get_vector_class_dim_count,  &
       get_variant_class_dim_count, &
       get_vector_class_dim_idx,    &
       get_variant_class_dim_idx
  !
  ! [A.7] BASIS-Modul mit Typ und Methoden OpenMI <EM>IDimension</EM>
  !
  USE b_omi_dim, ONLY :       &
       ! Datentyp
       t_omi_dim,             &
       ! Routinen / Interfaces
       init_omi_dim,          &
       clear_omi_dim,         &
       setup_omi_dim_prn_lun, &
       setup_omi_dim_trc_lun, &
       new_omi_dim,           &
       set_omi_dim,           &
       ok_omi_dim,            &
       print_omi_dim,         &
       eq_omi_dim
  !
  ! [A.8] BASIS-Modul mit Typ und Methoden OpenMI <EM>IUnit</EM>
  !
  USE b_omi_unit, ONLY :        &
       ! Datentyp
       t_omi_unit,              &
       ! Routinen / Interfaces
       init_omi_unit,           &
       clear_omi_unit,          &
       setup_omi_unit_prn_lun,  &
       setup_omi_unit_trc_lun,  &
       setup_omi_unit_language, &
       new_omi_unit,            &
       set_omi_unit,            &
       ok_omi_unit,             &
       print_omi_unit,          &
       eq_omi_unit
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
  !! max. Anzahl der Zeichen in der Komponente <EM>id</EM> 
  INTEGER            , PUBLIC , PARAMETER :: c_len_omi_quant_id=80          ! 
  !! max. Anzahl der Zeichen in der Komponente <EM>description</EM> 
  INTEGER            , PUBLIC , PARAMETER :: c_len_omi_quant_description=80 ! 
  !
  ! [C.1] (maximal) einen oeffentlich zugaenglichen Typ deklarieren
  !
  !! id          : kurzer Identifikationsbezeichner der Daten <BR>
  !! description : erg&auml;nzende verbale Beschreibung der Daten <BR>
  !! valuetype   : Typ der Daten (Skalar=1, Vektor=2) <BR>
  !! dimension   : SI-Basisdimension der Daten (Typ "t_omi_dim") <BR>
  !! unit        : aktuelle phys. Einheit der Daten (Typ "t_omi_unit")
  TYPE , PUBLIC :: t_omi_quant
     PRIVATE
     CHARACTER (LEN=c_len_omi_quant_id)          :: id          ! 
     CHARACTER (LEN=c_len_omi_quant_description) :: description ! 
     INTEGER                                     :: valuetype   ! 
     TYPE (t_omi_dim)                            :: dimension   ! 
     TYPE (t_omi_unit)                           :: unit        ! 
  END TYPE t_omi_quant
  !
  ! [C.2] Konstantwerte (Parameter) [moeglichst nicht verwenden]
  !
  !! Undefined-Wert f&uuml;r INTEGER-Komponenten
  INTEGER            , PUBLIC , PARAMETER :: c_undef_omi_quant_int=999 ! 
  !! Undefined-Wert f&uuml;r CHARACTER-Komponenten
  CHARACTER (LEN=9)  , PUBLIC , PARAMETER :: c_undef_omi_quant_char='undefined'      ! 
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
  INTERFACE init_omi_quant
     MODULE PROCEDURE init_omi_quant_d ! 
  END INTERFACE
  !! De-Allokieren/De-Initialisieren der statischen Datenobjekte des Moduls; <BR>
  !! Re-Initialisieren einiger statischer Daten mit Default-Werten.
  INTERFACE clear_omi_quant
     MODULE PROCEDURE clear_omi_quant_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r PRINT-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>PRN_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>PRN_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>PRN_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_omi_quant_prn_lun
     MODULE PROCEDURE setup_omi_quant_prn_lun_d ! 
  END INTERFACE
  !! logische Kanalnummer f&uuml;r TRACE-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>TRC_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>TRC_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>TRC_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_omi_quant_trc_lun
     MODULE PROCEDURE setup_omi_quant_trc_lun_d ! 
  END INTERFACE
  !! Index f&uuml;r Spracheinstellung setzen <BR>
  !! 1 = Deutsch (Default) <BR>
  !! 2 = Englisch         
  INTERFACE setup_omi_quant_language
     MODULE PROCEDURE setup_omi_quant_language_d ! 
  END INTERFACE
  !
  !! Erzeugen von Datenobjekten "t_omi_quant" und Initialisieren mit Default-Werten: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE new_omi_quant
     MODULE PROCEDURE new_omi_quant_0  ! 
     MODULE PROCEDURE new_omi_quant_1  ! 
  END INTERFACE
  !! Vernichten von Datenobjekten "t_omi_quant" und Re-Initialisieren mit Default-Werten: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE kill_omi_quant
     MODULE PROCEDURE kill_omi_quant_0 ! 
     MODULE PROCEDURE kill_omi_quant_1 ! 
  END INTERFACE
  !! Pr&uuml;fen von Datenobjekten "t_omi_quant" auf G&uuml;ltigkeit: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE ok_omi_quant
     MODULE PROCEDURE ok_omi_quant_0 ! 
     MODULE PROCEDURE ok_omi_quant_1 ! 
  END INTERFACE
  !! Drucken von Datenobjekten "t_omi_quant" (Skalar, 1D-Array); <BR>
  !! Alle Komponenten des Typs "t_omi_quant" auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_quant
     MODULE PROCEDURE print_omi_quant_0 ! 
     MODULE PROCEDURE print_omi_quant_1 ! 
  END INTERFACE
  !! Drucken aller in diesem Modul abgelegten statischen Daten; <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_quant_static
     MODULE PROCEDURE print_omi_quant_static_d ! 
  END INTERFACE
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Moduls; <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_quant_all_errors
     MODULE PROCEDURE print_omi_quant_all_errors_d ! 
  END INTERFACE
  !
  !! Setze Komponente "id" in "t_omi_quant" auf Benutzerwert: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)    <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) 
  !! die Daten werden auf die interne Komponente kopiert
  INTERFACE set_omi_quant_id
     MODULE PROCEDURE set_omi_quant_id_0_0    ! 
     MODULE PROCEDURE set_omi_quant_id_1_0    ! 
  END INTERFACE
  !! Setze Komponente "description" in "t_omi_quant" auf Benutzerwert: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) <BR>
  !! die Daten werden auf die interne Komponente kopiert
  INTERFACE set_omi_quant_description
     MODULE PROCEDURE set_omi_quant_description_0_0 ! 
     MODULE PROCEDURE set_omi_quant_description_1_0 ! 
  END INTERFACE
  !! Setze Komponente "valuetype" in "t_omi_quant" auf Benutzerwert: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) <BR>
  !! die Daten werden auf die interne Komponente kopiert
  INTERFACE set_omi_quant_valuetype
     MODULE PROCEDURE set_omi_quant_valuetype_0_0 ! 
     MODULE PROCEDURE set_omi_quant_valuetype_1_0 ! 
  END INTERFACE
  !! Setze Komponente "dimension" in "t_omi_quant" auf Benutzerwert: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) <BR>
  !! die Daten werden auf die interne Komponente kopiert
  INTERFACE set_omi_quant_dimension
     MODULE PROCEDURE set_omi_quant_dimension_0_0 ! 
     MODULE PROCEDURE set_omi_quant_dimension_1_0 ! 
  END INTERFACE
  !! Setze Komponente "unit" in "t_omi_quant" auf Benutzerwert: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) <BR>
  !! die Daten werden auf die interne Komponente kopiert
  INTERFACE set_omi_quant_unit
     MODULE PROCEDURE set_omi_quant_unit_0_0 ! 
     MODULE PROCEDURE set_omi_quant_unit_1_0 ! 
  END INTERFACE
  !
  !! Index f&uuml;r Spracheinstellung ermitteln <BR>
  !! a) Deutsch (Default) <BR>
  !! b) Englisch         
  INTERFACE get_omi_quant_language
     MODULE PROCEDURE get_omi_quant_language_d ! 
  END INTERFACE
  !! Hole Komponente "id" aus "t_omi_quant" <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  INTERFACE get_omi_quant_id
     MODULE PROCEDURE get_omi_quant_id_0_0 ! 
     MODULE PROCEDURE get_omi_quant_id_1_0 ! 
  END INTERFACE
  !! Hole Komponente "description" aus "t_omi_quant" <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  INTERFACE get_omi_quant_description
     MODULE PROCEDURE get_omi_quant_description_0_0 ! 
     MODULE PROCEDURE get_omi_quant_description_1_0 ! 
  END INTERFACE
  !! Hole Komponente "valuetype" aus "t_omi_quant" <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  INTERFACE get_omi_quant_valuetype
     MODULE PROCEDURE get_omi_quant_valuetype_0_0 ! 
     MODULE PROCEDURE get_omi_quant_valuetype_1_0 ! 
  END INTERFACE
  !! Hole Komponente "dimension" aus "t_omi_quant" <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  INTERFACE get_omi_quant_dimension
     MODULE PROCEDURE get_omi_quant_dimension_0_0 ! 
     MODULE PROCEDURE get_omi_quant_dimension_1_0 ! 
  END INTERFACE
  !! Hole Komponente "unit" aus "t_omi_quant" <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  INTERFACE get_omi_quant_unit
     MODULE PROCEDURE get_omi_quant_unit_0_0 ! 
     MODULE PROCEDURE get_omi_quant_unit_1_0 ! 
  END INTERFACE
  !
  ! ... ggf. Holen fuer weitere Komponenten des Datenobjektes ergaenzen
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  !! Ermittle die Anzahl der erforderlichen Quantit&auml;ten, die f&uuml;r eine
  !! Eingangs-Variable erzeugt werden m&uuml;ssen: <BR>
  !! a) bei vorgegebener Code-Kennung einer physikalischen Gr&ouml;&szlig;e <BR>
  !! b) bei vorgegebenen Code-Kennungen mehrerer physikalischer Gr&ouml;&szlig;en <BR>
  !! c) bei vorgegebener Beschreibung einer physikalischen Gr&ouml;&szlig;e <BR>
  !! d) bei vorgegebenen Beschreibungen mehrerer physikalischer Gr&ouml;&szlig;en <BR>
  !! e) bei vorgegebener Variablenbeschreibung (t_var) und Dimensionen (t_dim) <BR>
  !! f) bei vorgegebenen Variablenbeschreibungen (t_var) und Dimensionen (t_dim)
  INTERFACE get_max_omi_quant
     MODULE PROCEDURE get_max_omi_quant_code_0
     MODULE PROCEDURE get_max_omi_quant_code_1
     MODULE PROCEDURE get_max_omi_quant_descr_0
     MODULE PROCEDURE get_max_omi_quant_descr_1
     MODULE PROCEDURE get_max_omi_quant_var_dim_0_1
     MODULE PROCEDURE get_max_omi_quant_var_dim_1_1
  END INTERFACE
  !
  !! Ermittle aus einer (Vektor-) Quantit&auml;t die entsprechende (Magnitude-) Quantit&auml;t: <BR>
  !! a) f&uuml;r eine bekannte Quantit&auml;t
  INTERFACE get_omi_quant_magnitude_quant
     MODULE PROCEDURE get_omi_quant_magnitude_quant_0
  END INTERFACE
  !
  !! Setze alle Komponenten mit Hilfe verschiedener Hilfsinformationen: <BR>
  !! a) bei vorgegebener Code-Kennung einer physikalischen Gr&ouml;&szlig;e <BR>
  !! b) bei vorgegebener Beschreibung einer physikalischen Gr&ouml;&szlig;e <BR>
  !! c) f&uuml;r eine Variablenbeschreibung, sowie Dimensionen und Attribute <BR>
  !! d) f&uuml;r viele Variablenbeschreibungen, sowie Dimensionen und Attribute 
  INTERFACE set_omi_quant
     MODULE PROCEDURE set_omi_quant_code_1_0
     MODULE PROCEDURE set_omi_quant_descr_1_0
     MODULE PROCEDURE set_omi_quant_vda_1_0_1_1
     MODULE PROCEDURE set_omi_quant_vda_1_1_1_1
  END INTERFACE
  !
  !! Ermittle die Code-Kennung einer physikalischen Gr&ouml;&szlig;e aus der ID
  !! a) f&uuml;r eine <EM>Quantity</EM> <BR>
  !! b) f&uuml;r viele <EM>Quantities</EM>
  INTERFACE get_code_from_omi_quant
     MODULE PROCEDURE get_code_from_omi_quant_0
     MODULE PROCEDURE get_code_from_omi_quant_1
  END INTERFACE
  !
  !! Ermittle die Klassen-Dimensions-Nummer einer physikalischen Gr&ouml;&szlig;e aus der ID <BR>
  !! a) f&uuml;r eine <EM>Quantity</EM>
  INTERFACE get_cla_no_from_omi_quant
     MODULE PROCEDURE get_cla_no_from_omi_quant_0
  END INTERFACE
  !! Ermittle die Varianten-Nummer einer physikalischen Gr&ouml;&szlig;e aus der ID <BR>
  !! a) f&uuml;r eine <EM>Quantity</EM>
  INTERFACE get_var_no_from_omi_quant
     MODULE PROCEDURE get_var_no_from_omi_quant_0
  END INTERFACE
  !
  !! suche nach einem bestimmten Wert der Komponente <EM>id</EM> in einem 
  !! 1D-Feld des Typs "t_omi_quant": <BR>
  !! a) f&uuml;r einen Wert <EM>id</EM> (Skalar) <BR>
  !! b) f&uuml;r viele Werte <EM>id(:)</EM> (Vektor)
  INTERFACE get_omi_quant_idx
     MODULE PROCEDURE get_omi_quant_idx_1_0
     MODULE PROCEDURE get_omi_quant_idx_1_1
  END INTERFACE
  !
  !! Ermittle die Id der zu einer Quantit&auml;t geh&ouml;renden Referenzgr&ouml;&szlig;e <BR>
  !! a) f&uuml;r eine Quantit&auml;t 
  INTERFACE get_omi_ref_quant_id
     MODULE PROCEDURE get_omi_ref_quant_id_0
  END INTERFACE
  !! Ermittle die Id der zu einer Quantit&auml;t geh&ouml;renden zeitvariablen Topographie <BR>
  !! a) f&uuml;r eine Quantit&auml;t 
  INTERFACE get_omi_bot_quant_id
     MODULE PROCEDURE get_omi_bot_quant_id_0
  END INTERFACE
  !
  !! pr&uuml;fe, ob es sich bei einer Quantit&auml;t um eine <EM>skalare</EM> Gr&ouml;&szlig;e handelt: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE is_omi_quant_scalar
     MODULE PROCEDURE is_omi_quant_scalar_0
     MODULE PROCEDURE is_omi_quant_scalar_1
  END INTERFACE
  !! pr&uuml;fe, ob es sich bei einer Quantit&auml;t um eine <EM>vektorielle</EM> Gr&ouml;&szlig;e handelt: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE is_omi_quant_vector
     MODULE PROCEDURE is_omi_quant_vector_0
     MODULE PROCEDURE is_omi_quant_vector_1
  END INTERFACE
  !! pr&uuml;fe, ob es sich bei einer Quantit&auml;t ume eine <EM>Magnitude</EM>
  !! Gr&ouml;&szlig;e handelt <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE is_omi_quant_magnitude
     MODULE PROCEDURE is_omi_quant_magnitude_0
     MODULE PROCEDURE is_omi_quant_magnitude_1
  END INTERFACE
  !
  !! Kopiere den Inhalt einer Variablen des Typs "t_omi_quant" in
  !! ein anderes Objekt vom gleichen Typ <BR>
  !! a) ein Quell-Objekt wird in ein Ziel-Objekt kopiert <BR>
  !! a) mehrere Quell-Objekte werden auf mehrere Ziel-Objekte kopiert
  INTERFACE copy_omi_quant
     MODULE PROCEDURE copy_omi_quant_0_0
     MODULE PROCEDURE copy_omi_quant_1_1
  END INTERFACE
  !
  !! Pr&uuml;fen zweier Datenobjekte "t_omi_quant" auf Gleichheit (Funktion) <BR>
  !! a) Skalar1 == Skalar2 <BR>
  !! b) Skalar1 == Vektor2 <BR>
  !! c) Vektor1 == Skalar2 <BR>
  !! d) Vektor1 == Vektor2
  INTERFACE eq_omi_quant
     MODULE PROCEDURE eq_omi_quant_0_0  ! 
     MODULE PROCEDURE eq_omi_quant_0_1  ! 
     MODULE PROCEDURE eq_omi_quant_1_0  ! 
     MODULE PROCEDURE eq_omi_quant_1_1  ! 
  END INTERFACE
  !! Pr&uuml;fen zweier Datenobjekte "t_omi_quant" auf Ungleichheit (Funktion) <BR>
  !! a) Skalar1 /= Skalar2 <BR>
  !! b) Skalar1 /= Vektor2 <BR>
  !! c) Vektor1 /= Skalar2 <BR>
  !! d) Vektor1 /= Vektor2
  INTERFACE ne_omi_quant
     MODULE PROCEDURE ne_omi_quant_0_0  ! 
     MODULE PROCEDURE ne_omi_quant_0_1  ! 
     MODULE PROCEDURE ne_omi_quant_1_0  ! 
     MODULE PROCEDURE ne_omi_quant_1_1  ! 
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
  PUBLIC :: init_omi_quant
  PUBLIC :: clear_omi_quant
  PUBLIC :: setup_omi_quant_prn_lun
  PUBLIC :: setup_omi_quant_trc_lun
  PUBLIC :: new_omi_quant
  PUBLIC :: kill_omi_quant
  PUBLIC :: ok_omi_quant
  PUBLIC :: print_omi_quant
  PUBLIC :: print_omi_quant_static
  PUBLIC :: print_omi_quant_all_errors
  PUBLIC :: set_omi_quant_id
  PUBLIC :: set_omi_quant_description
  PUBLIC :: set_omi_quant_valuetype
  PUBLIC :: set_omi_quant_dimension
  PUBLIC :: set_omi_quant_unit
  PUBLIC :: get_omi_quant_id
  PUBLIC :: get_omi_quant_description
  PUBLIC :: get_omi_quant_valuetype
  PUBLIC :: get_omi_quant_dimension
  PUBLIC :: get_omi_quant_unit
  PUBLIC :: eq_omi_quant
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: setup_omi_quant_language
  PUBLIC :: set_omi_quant  
  PUBLIC :: get_max_omi_quant
  PUBLIC :: get_omi_quant_language
  PUBLIC :: get_omi_quant_idx 
  PUBLIC :: get_omi_quant_magnitude_quant
  PUBLIC :: get_code_from_omi_quant
  PUBLIC :: get_cla_no_from_omi_quant
  PUBLIC :: get_var_no_from_omi_quant
  PUBLIC :: get_omi_ref_quant_id
  PUBLIC :: get_omi_bot_quant_id
  PUBLIC :: is_omi_quant_scalar 
  PUBLIC :: is_omi_quant_vector 
  PUBLIC :: is_omi_quant_magnitude
  PUBLIC :: copy_omi_quant
  PUBLIC :: ne_omi_quant
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
  CHARACTER (LEN=11), PARAMETER :: c_modname      = 'b_omi_quant' ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.          ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1               ! 
  !! Anzahl der Datenkomponenten des Typs t_omi_quant
  INTEGER           , PARAMETER :: c_nofcomp      =  5               ! ggf. modifizieren
  !! Anzahl einstellbarer Sprachen
  INTEGER           , PARAMETER :: c_max_language = 2                ! [Deutsch,Englisch]
  !! Default-Language
  INTEGER           , PARAMETER :: c_def_language = 1                ! [Deutsch]
  !! max. Anzahl verschiedener Type (Skalar, Vektor)
  INTEGER           , PARAMETER :: c_max_valuetype= 2                ! 
  !! Feld mit den Namen der Typen
  CHARACTER (LEN=6) , PARAMETER :: c_valuetype_name(c_max_valuetype,c_max_language)= & ! 
       RESHAPE( (/ 'Skalar', 'Vektor', 'scalar', 'vector' /), SHAPE=(/c_max_valuetype,c_max_language/) )
  !! Anzahl der Quantit&auml;ten je skalarer Gr&ouml;&szlig;e
  INTEGER           , PARAMETER :: c_quant_per_scalar=1 ! 
  !! Anzahl der Quantit&auml;ten je vektorieller Gr&ouml;&szlig;e <BR>
  !! 1 = Komponenten der vektoriellen Gr&ouml;&szlig;e <BR>
  !! 2 = Betrag der vektoriellen Gr&ouml;&szlig;e
  INTEGER           , PARAMETER :: c_quant_per_vector=2 ! 
  !! Feld mit den Bezeichnungen der Komponenten
  CHARACTER (LEN=05) , PARAMETER :: c_component_name(c_quant_per_vector,c_max_valuetype)= & ! 
       RESHAPE( (/ '(Val)', '(---)', '(Vec)', '(Mag)' /), SHAPE=(/c_quant_per_vector,c_max_valuetype/) )
  !! Feld mit den Valuetypes der Komponenten
  INTEGER            , PARAMETER :: c_component_type(c_quant_per_vector,c_max_valuetype)= & ! 
       RESHAPE( (/ 1, 1, 2, 1 /), SHAPE=(/c_quant_per_vector,c_max_valuetype/) )
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
  SUBROUTINE init_omi_quant_d &
       ( )
    !
    USE b_error, ONLY : DEBUG_b
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER :: c_upname='init_omi_quant_d' 
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_b > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "b_omi_quant" version 3.1 of 12/21/06                 '
          WRITE(*,*) ' Copyright (C) 2005 Bundesanstalt fuer Wasserbau   '
          WRITE(*,*)
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       IF ( no_error( ) ) CALL init_dim      ( )
       IF ( no_error( ) ) CALL init_var      ( )
       IF ( no_error( ) ) CALL init_att      ( )
       IF ( no_error( ) ) CALL init_phy      ( )
       IF ( no_error( ) ) CALL init_omi_dim  ( )
       IF ( no_error( ) ) CALL init_omi_unit ( )
       !
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_omi_quant_all_errors ( ) 
       ! [1.5] Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.6] endgueltiges Setzen des Initialisierungs-Indikators
       initialised = MERGE( .true., .false., no_error( ) )
    END IF
    ! 2.0 Initialisierungszaehler heraufsetzen
    n_init = n_init + 1
    !
  END SUBROUTINE init_omi_quant_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_omi_quant_d &
       ( )
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='clear_omi_quant_d' ! 
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_omi_quant_all_errors ( ) 
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.3] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.4] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.4.1] ggf. weitere Module de-initialisieren
       IF ( no_error( ) ) CALL clear_omi_unit ( )
       IF ( no_error( ) ) CALL clear_omi_dim  ( )
       IF ( no_error( ) ) CALL clear_phy      ( )
       IF ( no_error( ) ) CALL clear_att      ( )
       IF ( no_error( ) ) CALL clear_var      ( )
       IF ( no_error( ) ) CALL clear_dim      ( )
       ! [1.4.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
    !
  END SUBROUTINE clear_omi_quant_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_omi_quant_prn_lun_d &
       ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER :: c_upname='setup_omi_quant_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun    ( lun )
       IF ( no_error( ) ) CALL setup_dim_prn_lun      ( lun )
       IF ( no_error( ) ) CALL setup_var_prn_lun      ( lun )
       IF ( no_error( ) ) CALL setup_att_prn_lun      ( lun )
       IF ( no_error( ) ) CALL setup_phy_prn_lun      ( lun )
       IF ( no_error( ) ) CALL setup_omi_dim_prn_lun  ( lun )
       IF ( no_error( ) ) CALL setup_omi_unit_prn_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_omi_quant_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_omi_quant_trc_lun_d &
       ( lun )
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER :: c_upname='setup_omi_quant_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun    ( lun )
       IF ( no_error( ) ) CALL setup_dim_trc_lun      ( lun )
       IF ( no_error( ) ) CALL setup_var_trc_lun      ( lun )
       IF ( no_error( ) ) CALL setup_att_trc_lun      ( lun )
       IF ( no_error( ) ) CALL setup_phy_trc_lun      ( lun )
       IF ( no_error( ) ) CALL setup_omi_dim_trc_lun  ( lun )
       IF ( no_error( ) ) CALL setup_omi_unit_trc_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_omi_quant_trc_lun_d
  !
  !! Setzen des Index f&uuml;r die Spracheinstellung <BR>
  !! 1 = Deutsch  <BR>
  !! 2 = Englisch 
  SUBROUTINE setup_omi_quant_language_d ( var )
    !! Index f&uuml;r Spracheinstellung (1 = Deutsch, 2 = Englisch )
    INTEGER , INTENT(IN) :: var ! 
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER :: c_upname='setup_omi_quant_language_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       language = MERGE ( var, c_def_language, ( 1 <= var .AND. var <= c_max_language ) )
       IF ( no_error( ) ) CALL setup_phy_language      ( language ) 
       IF ( no_error( ) ) CALL setup_omi_unit_language ( language ) 
    END IF
    !
  END SUBROUTINE setup_omi_quant_language_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE new_omi_quant_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_quant) , INTENT(OUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=15)   , PARAMETER :: c_upname='new_omi_quant_0'
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       !
       this%id          = REPEAT( ' ', LEN(this%id)          ) 
       this%id          = c_undef_omi_quant_char 
       this%description = REPEAT( ' ', LEN(this%description) ) 
       this%description = c_undef_omi_quant_char 
       this%valuetype   = c_undef_omi_quant_int 
       CALL new_omi_dim  ( this%dimension )
       CALL new_omi_unit ( this%unit      )
    END IF
    !
  END SUBROUTINE new_omi_quant_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE new_omi_quant_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_quant) , INTENT(OUT) :: this(:) ! 
    !! Z&auml;hler      
    INTEGER                          :: i ! 
    !! Name der Subroutine
    CHARACTER (LEN=15)   , PARAMETER :: c_upname='new_omi_quant_1'
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL new_omi_quant_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_omi_quant_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE kill_omi_quant_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_quant) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=16)   , PARAMETER :: c_upname='kill_omi_quant_0'
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       IF ( no_error( ) ) CALL new_omi_quant_0 ( this )
    END IF
    !
  END SUBROUTINE kill_omi_quant_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE kill_omi_quant_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_quant) , INTENT(INOUT) :: this(:) ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !! Name der Subroutine
    CHARACTER (LEN=16)   , PARAMETER :: c_upname='kill_omi_quant_1'
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL kill_omi_quant_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_omi_quant_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_quant_0 &
       ( this )              &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_quant) , INTENT(IN) :: this ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=14), PARAMETER :: c_upname='ok_omi_quant_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok = .false.
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok(1)  = ok_omi_quant_id          ( this )
       l_ok(2)  = ok_omi_quant_description ( this )
       l_ok(3)  = ok_omi_quant_valuetype   ( this )
       l_ok(4)  = ok_omi_quant_dimension   ( this )
       l_ok(5)  = ok_omi_quant_unit        ( this )
    END IF
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_omi_quant_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_quant_1 &
       ( this )              &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_quant) , INTENT(IN) :: this(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=14), PARAMETER :: c_upname='ok_omi_quant_1' 
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
          ok(i) = ok_omi_quant_0 ( this(i) )
       END DO
    END IF
    !
  END FUNCTION ok_omi_quant_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_quant_0 &
       ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_quant) , INTENT(IN) :: this ! 
    !! Name der Function
    CHARACTER (LEN=17), PARAMETER :: c_upname='print_omi_quant_0' 
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
       IF ( no_error( ) ) CALL print_omi_quant_id          ( this )
       IF ( no_error( ) ) CALL print_omi_quant_description ( this )
       IF ( no_error( ) ) CALL print_omi_quant_valuetype   ( this )
       IF ( no_error( ) ) CALL print_omi_quant_dimension   ( this )
       IF ( no_error( ) ) CALL print_omi_quant_unit        ( this )
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
8000 FORMAT('# Beginn Objekt t_omi_quant ------------------------------')
8001 FORMAT('# Ende   Objekt t_omi_quant ------------------------------')
    !
  END SUBROUTINE print_omi_quant_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_quant_1 &
       ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_quant) , INTENT(IN) :: this(:) ! 
    !! Name der Function
    CHARACTER (LEN=17), PARAMETER :: c_upname='print_omi_quant_1' 
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
          IF ( no_error( ) ) CALL print_omi_quant_0 ( this(i) )
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
  END SUBROUTINE print_omi_quant_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_quant_static_d &
       ( )
    !! Name der Function
    CHARACTER (LEN=24), PARAMETER :: c_upname='print_omi_quant_static_d' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )    &
           initialised, prn_op, trc_op, prn_lun, trc_lun, n_init, language, &
           TRIM(c_undef_omi_quant_char), c_undef_omi_quant_int
       !
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       !
       IF ( no_error( ) ) CALL print_omi_quant_all_errors_d ( )
       !
    ELSE
       !
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
       !
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls b_omi_quant         ',/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
    '# initialised = ',L1,/ &
    '#      prn_op = ',L1,/ &
    '#      trc_op = ',L1,/ &
    '#     prn_lun = ',I5,/ &
    '#     trc_lun = ',I5,/ &
    '#      n_init = ',I5,/ &
    '#      language = ',I5,/ &
    '#   undef[char] = ',A,/ &
    '#    undef[int] = ',I10,/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
    '#------------------------------------------------------------') 
    !
  END SUBROUTINE print_omi_quant_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_quant_all_errors_d &
       ( )
    !! Name der Function
    CHARACTER (LEN=28), PARAMETER :: c_upname='print_omi_quant_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_omi_quant_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! weise der Komponente "id" einen skalaren Wert zu (Skalar) <BR>
  !! die Daten werden auf die interne Komponente kopiert <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_quant_id_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_quant) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "id"
    CHARACTER (LEN=*)  , INTENT(IN)    :: val  ! 
    !
    this%id = REPEAT( ' ', LEN(this%id) )
    this%id = val
    !
  END SUBROUTINE set_omi_quant_id_0_0
  !
  !! weise der Komponente "id" einen skalaren Wert zu (Vektor) <BR>
  !! die Daten werden auf die interne Komponente kopiert <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_quant_id_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_quant) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "id"
    CHARACTER (LEN=*)  , INTENT(IN)    :: val     ! 
    !
    this(:)%id = REPEAT( ' ', LEN(this%id) )
    this(:)%id = val
    !
  END SUBROUTINE set_omi_quant_id_1_0
  !
  !! weise der Komponente "description" einen skalaren Wert zu (Skalar) <BR>
  !! die Daten werden auf die interne Komponente kopiert <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_quant_description_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_quant) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "description"
    CHARACTER (LEN=*)  , INTENT(IN)    :: val  ! 
    !
    this%description = REPEAT( ' ', LEN(this%description) )
    this%description = val
    !
  END SUBROUTINE set_omi_quant_description_0_0
  !
  !! weise der Komponente "description" einen skalaren Wert zu (Vektor) <BR>
  !! die Daten werden auf die interne Komponente kopiert <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_quant_description_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_quant) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "description"
    CHARACTER (LEN=*)  , INTENT(IN)    :: val     ! 
    !
    this(:)%description =  REPEAT( ' ', LEN(this%description) )
    this(:)%description = val
    !
  END SUBROUTINE set_omi_quant_description_1_0
  !
  !! weise der Komponente "valuetype" einen skalaren Wert zu (Skalar) <BR>
  !! die Daten werden auf die interne Komponente kopiert <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_quant_valuetype_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_quant) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "valuetype"
    INTEGER            , INTENT(IN)    :: val  ! 
    !
    this%valuetype = val
    !
  END SUBROUTINE set_omi_quant_valuetype_0_0
  !
  !! weise der Komponente "valuetype" einen skalaren Wert zu (Vektor) <BR>
  !! die Daten werden auf die interne Komponente kopiert <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_quant_valuetype_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_quant) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "valuetype"
    INTEGER            , INTENT(IN)    :: val     ! 
    !
    this(:)%valuetype = val
    !
  END SUBROUTINE set_omi_quant_valuetype_1_0
  !
  !! weise der Komponente "dimension" einen skalaren Wert zu (Skalar) <BR>
  !! die Daten werden auf die interne Komponente kopiert <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_quant_dimension_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_quant) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "dimension"
    TYPE (t_omi_dim)   , INTENT(IN)    :: val  ! 
    !
    this%dimension = val
    !
  END SUBROUTINE set_omi_quant_dimension_0_0
  !
  !! weise der Komponente "dimension" einen skalaren Wert zu (Vektor) <BR>
  !! die Daten werden auf die interne Komponente kopiert <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_quant_dimension_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_quant) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "dimension"
    TYPE (t_omi_dim)   , INTENT(IN)    :: val     ! 
    !
    this(:)%dimension = val
    !
  END SUBROUTINE set_omi_quant_dimension_1_0
  !
  !! weise der Komponente "unit" einen skalaren Wert zu (Skalar) <BR>
  !! die Daten werden auf die interne Komponente kopiert <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_quant_unit_0_0 &
       ( this, &
         val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_quant) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "unit"
    TYPE (t_omi_unit)  , INTENT(IN)    :: val  ! 
    !
    this%unit = val
    !
  END SUBROUTINE set_omi_quant_unit_0_0
  !
  !! weise der Komponente "unit" einen skalaren Wert zu (Vektor) <BR>
  !! die Daten werden auf die interne Komponente kopiert <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_quant_unit_1_0 &
       ( this, &
         val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_quant) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "unit"
    TYPE (t_omi_unit)  , INTENT(IN)    :: val     ! 
    !
    this(:)%unit = val
    !
  END SUBROUTINE set_omi_quant_unit_1_0
  !
  !! setze alle Komponenten aus den Informationen zur Code-Kennung 
  !! der korrespondierenden physikalischen Gr&ouml;&szlig;e <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_omi_quant_code_1_0 ( this, code )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_quant) ,INTENT(INOUT) :: this(:)
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER           , INTENT(IN)    :: code ! 
    !! Name des Unterprogramms
    CHARACTER (LEN=22) , PARAMETER :: c_upname='set_omi_quant_code_1_0' ! 
    !! Hilfsvariable
    CHARACTER (LEN=10) :: ch        ! 
    INTEGER            :: nq, i     ! 
    !
    nq = get_max_omi_quant( code )
    IF ( nq /= SIZE(this) ) THEN
       CALL setup_error_act ( all_errors(:), 8000, c_upname, c_modname )
       WRITE(ch,'(I10)') code       ; CALL setup_error_act ( '<code>', TRIM(ch) )
       WRITE(ch,'(I10)') SIZE(this) ; CALL setup_error_act ( '<actual>', TRIM(ch) )
       WRITE(ch,'(I10)') nq         ; CALL setup_error_act ( '<required>', TRIM(ch) )
    ELSE
       DO i=1,nq
          IF ( any_error( ) ) EXIT
          CALL new_omi_quant_0 ( this(i) )
          this(i)%id          = get_omi_quant_id_code ( code, i ) 
          this(i)%description = get_omi_quant_description_code ( code, i ) 
          this(i)%valuetype   = get_omi_quant_valuetype_code ( code, i )
          CALL set_omi_dim  ( this(i)%dimension, code  )
          CALL set_omi_unit ( this(i)%unit, code )
       END DO
    END IF
    !
  END SUBROUTINE set_omi_quant_code_1_0
  !
  !! setze alle Komponenten aus der verbalen Beschreibung 
  !! der korrespondierenden physikalischen Gr&ouml;&szlig;e <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_omi_quant_descr_1_0 ( this, descr )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_quant) , INTENT(INOUT) :: this(:)
    !! Beschreibung einer physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*)  , INTENT(IN)    :: descr ! 
    !! Hilfsvariable
    INTEGER :: code ! 
    !
    code = get_phy_quant_code       ( descr )
    CALL set_omi_quant_code_1_0 ( this(:), code  )
    !
  END SUBROUTINE set_omi_quant_descr_1_0
  !
  !! setze alle Komponenten aus den Informationen einer Variablenbeschreibung (t_var),
  !! der Dimensionen (t_dim) sowei der Attribute (t_att) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_omi_quant_vda_1_0_1_1 ( this, var, dim, att )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_quant) , INTENT(INOUT) :: this(:) ! 
    !! Variablenbschreibung
    TYPE (t_var)       , INTENT(IN)    :: var     ! 
    !! Dimensionen
    TYPE (t_dim)       , INTENT(IN)    :: dim(:)  ! 
    !! Attribute
    TYPE (t_att)       , INTENT(IN)    :: att(:)  ! 
    !! Name des Unterprogramms
    CHARACTER (LEN=25) , PARAMETER :: c_upname='set_omi_quant_vda_1_0_1_1' ! 
    !! Hilfsvariable
    CHARACTER (LEN=10)               :: ch                                              ! 
    INTEGER                          :: nt, nq, nd, i, j, k, nr, idx, jdx, kdx, ncl, nl ! 
    INTEGER            , POINTER     :: p_att_idx(:), p_dim_id(:)                       ! 
    INTEGER            , ALLOCATABLE :: l_dim_idx(:)                                    ! 
    CHARACTER (LEN=80) , ALLOCATABLE :: l_ch(:)                                         ! 
    INTEGER                          :: code(1)                                     ! 
    !
    CALL new_omi_quant_1 ( this(:) )
    nt = get_max_omi_quant( var, dim(:) )
    IF ( nt /= SIZE(this) ) THEN
       CALL setup_error_act ( all_errors(:), 8010, c_upname, c_modname )
       CALL setup_error_act ( '<variable>', TRIM(get_var_name(var)) )
       WRITE(ch,'(I10)') SIZE(this) ; CALL setup_error_act ( '<actual>', TRIM(ch) )
       WRITE(ch,'(I10)') nq         ; CALL setup_error_act ( '<required>', TRIM(ch) )
    ELSE
       p_att_idx => get_att_idx_for_var_id ( att(:), get_var_id(var) )
       p_dim_id  => get_var_dim_id ( var )
       code = -1
       ncl  = 0
       nr   = 0
       nl   = 0
       IF ( ASSOCIATED( p_att_idx) .AND. ASSOCIATED(p_dim_id) ) THEN
          ALLOCATE ( l_dim_idx(SIZE(p_dim_id)) )
          l_dim_idx(:) = get_dim_idx( dim(:), p_dim_id(:) )
          idx = get_att_idx ( att(p_att_idx), c_att_name(41) ) ! code
          IF ( idx > 0 ) code(:) = get_att_in ( att(p_att_idx(idx)) )
          jdx = get_att_idx ( att(p_att_idx), c_att_name(56) ) ! class names
          IF ( jdx > 0 ) ncl = get_att_nof_values ( att(p_att_idx(jdx)) )
          IF ( ncl > 0 ) THEN
             ALLOCATE ( l_ch(ncl) )
             l_ch(:) = get_att_ch ( att(p_att_idx(jdx)) )
          END IF
          nq = get_max_omi_quant( code(1) ) ! 1 oder 2 fuer Skalar / Vektor
          nd = get_variant_class_dim_count ( dim(l_dim_idx(:)) )
          DO k=1,MAX(1,nd)
             IF ( nd > 0 ) THEN
                kdx = get_variant_class_dim_idx ( dim(l_dim_idx(:)), k )
                IF ( kdx > 0 ) nl = get_dim_len( dim(l_dim_idx(kdx)) )
             END IF
             DO j=1,MAX(1,nl)
                DO i=1,nq
                   IF ( any_error( ) ) EXIT
                   nr = nr + 1
                   this(nr)%id          = get_omi_quant_id_code ( code(1), i ) 
                   this(nr)%description = get_omi_quant_description_code ( code(1), i ) 
                   this(nr)%valuetype   = get_omi_quant_valuetype_code ( code(1), i )
                   CALL set_omi_dim  ( this(nr)%dimension, var, att(:)  )
                   CALL set_omi_unit ( this(nr)%unit, var, att(:), this(nr)%dimension )
                   IF ( nd > 0 .AND. nl > 0 ) THEN
                      IF ( ALLOCATED(l_ch) .AND. nl==ncl ) THEN
                         CALL modify_omi_quant_id          ( this(nr), j, k, l_ch(j) )
                         CALL modify_omi_quant_description ( this(nr), j, k, l_ch(j) )
                      ELSE
                         CALL modify_omi_quant_id          ( this(nr), j, k )
                         CALL modify_omi_quant_description ( this(nr), j, k )
                      END IF
                   END IF
                END DO
             END DO
          END DO
       END IF
       IF ( ASSOCIATED(p_att_idx) ) DEALLOCATE( p_att_idx )
       IF ( ASSOCIATED(p_dim_id ) ) DEALLOCATE( p_dim_id  )
       IF ( ALLOCATED (l_dim_idx) ) DEALLOCATE( l_dim_idx )
       IF ( ALLOCATED (l_ch     ) ) DEALLOCATE( l_ch      )
    END IF
    !
  END SUBROUTINE set_omi_quant_vda_1_0_1_1
  !
  !! setze alle Komponenten aus den Informationen der Variablenbeschreibungen (t_var),
  !! der Dimensionen (t_dim) sowei der Attribute (t_att) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_omi_quant_vda_1_1_1_1 ( this, var, dim, att )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_quant) , INTENT(INOUT) :: this(:) ! 
    !! Variablenbschreibung
    TYPE (t_var)       , INTENT(IN)    :: var(:)  ! 
    !! Dimensionen
    TYPE (t_dim)       , INTENT(IN)    :: dim(:)  ! 
    !! Attribute
    TYPE (t_att)       , INTENT(IN)    :: att(:)  ! 
    !! Name des Unterprogramms
    CHARACTER (LEN=25) , PARAMETER :: c_upname='set_omi_quant_vda_1_1_1_1' ! 
    !! Hilfsvariable
    CHARACTER (LEN=10) :: ch            ! 
    INTEGER            :: nq, i, ia, ie ! 
    !
    nq = SUM( get_max_omi_quant( var(:), dim(:) ) )
    IF ( nq /= SIZE(this) ) THEN
       CALL setup_error_act ( all_errors(:), 8010, c_upname, c_modname )
       CALL setup_error_act ( '<variable>', TRIM(get_var_name(var(1))) )
       WRITE(ch,'(I10)') SIZE(this) ; CALL setup_error_act ( '<actual>', TRIM(ch) )
       WRITE(ch,'(I10)') nq         ; CALL setup_error_act ( '<required>', TRIM(ch) )
    ELSE
       ia = 1
       ie = 0
       DO i=1,SIZE(var)
          IF ( any_error( ) ) EXIT
          ie = ie + get_max_omi_quant( var(i), dim(:) )
          CALL set_omi_quant_vda_1_0_1_1 ( this(ia:ie), var(i), dim(:), att(:) )
          ia = ie + 1
       END DO
    END IF
    !
  END SUBROUTINE set_omi_quant_vda_1_1_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! Holen des Index f&uuml;r die Spracheinstellung <BR>
  !! 1 = Deutsch  <BR>
  !! 2 = Englisch 
  FUNCTION get_omi_quant_language_d ( ) &
       RESULT( res )
    !! R&uuml;ckgabewert: 
    !! Index f&uuml;r Spracheinstellung (1 = Deutsch, 2 = Englisch )
    INTEGER :: res ! 
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER :: c_upname='get_omi_quant_language_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       res = language
    ELSE
       res = -1
    END IF
    !
  END FUNCTION get_omi_quant_language_d
  !
  !! hole die Komponente "id" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  FUNCTION get_omi_quant_id_0_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_quant)   , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "id" (Skalar)
    CHARACTER (LEN=c_len_omi_quant_id) :: val  ! 
    !
    val = this%id
    !
  END FUNCTION get_omi_quant_id_0_0
  !
  !! hole die Komponente "id" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  FUNCTION get_omi_quant_id_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_quant)   , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "id"
    CHARACTER (LEN=c_len_omi_quant_id) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%id
    !
  END FUNCTION get_omi_quant_id_1_0
  !
  !! hole die Komponente "description" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  FUNCTION get_omi_quant_description_0_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_quant)            , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "description" (Skalar)
    CHARACTER (LEN=c_len_omi_quant_description) :: val  ! 
    !
    val = this%description
    !
  END FUNCTION get_omi_quant_description_0_0
  !
  !! hole die Komponente "description" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  FUNCTION get_omi_quant_description_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_quant)            , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "description"
    CHARACTER (LEN=c_len_omi_quant_description) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%description
    !
  END FUNCTION get_omi_quant_description_1_0
  !
  !! hole die Komponente "valuetype" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  FUNCTION get_omi_quant_valuetype_0_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_quant) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "valuetype" (Skalar)
    INTEGER                          :: val  ! 
    !
    val = this%valuetype
    !
  END FUNCTION get_omi_quant_valuetype_0_0
  !
  !! hole die Komponente "valuetype" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  FUNCTION get_omi_quant_valuetype_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_quant) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "valuetype"
    INTEGER :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%valuetype
    !
  END FUNCTION get_omi_quant_valuetype_1_0
  !
  !! hole die Komponente "dimension" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  FUNCTION get_omi_quant_dimension_0_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_quant) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "dimension" (Skalar)
    TYPE (t_omi_dim)                 :: val  ! 
    !
    val = this%dimension
    !
  END FUNCTION get_omi_quant_dimension_0_0
  !
  !! hole die Komponente "dimension" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  FUNCTION get_omi_quant_dimension_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_quant) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "dimension"
    TYPE (t_omi_dim)                 :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%dimension
    !
  END FUNCTION get_omi_quant_dimension_1_0
  !
  !! hole die Komponente "unit" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  FUNCTION get_omi_quant_unit_0_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_quant) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "unit" (Skalar)
    TYPE (t_omi_unit)                :: val  ! 
    !
    val = this%unit
    !
  END FUNCTION get_omi_quant_unit_0_0
  !
  !! hole die Komponente "unit" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben
  FUNCTION get_omi_quant_unit_1_0 &
       ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_quant) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "unit"
    TYPE (t_omi_unit) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%unit
    !
  END FUNCTION get_omi_quant_unit_1_0
  !
  !! suche nach einem bestimmten Wert der Komponente <EM>id</EM> in einem 
  !! 1D-Feld des Typs "t_omi_quant" f&uuml;r einen Wert <EM>id</EM> (Skalar) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_quant_idx_1_0 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_quant) , INTENT(IN) :: this(:) ! 
    !! zu suchender Wert der Komponente <EM>id</EM>
    CHARACTER (LEN=*)  , INTENT(IN) :: val     ! 
    !! Ergebnis: Zeiger auf einen Eintrag in this(:), deren aktueller Wert
    !! der Komponente <EM>id</EM> identisch mit <EM>val</EM> ist <BR>
    !! wird kein Eintrag gefunden, so wird -1 zur&uuml;ckgegeben
    INTEGER :: res ! 
    ! Hilfsvariablen
    INTEGER :: i, l1, l2 ! 
    !
    res = -1
    l1  = LEN_TRIM(val)
    DO i=1,SIZE(this)
       IF ( res /= -1 ) EXIT
       l2 = LEN_TRIM(this(i)%id)
       IF ( l1 == l2 ) THEN
          IF ( val(1:l1) == this(i)%id(1:l2) ) res = i
       END IF
    END DO
    !
  END FUNCTION get_omi_quant_idx_1_0
  !
  !! suche nach einem bestimmten Wert der Komponente <EM>id</EM> in einem 
  !! 1D-Feld des Typs "t_omi_quant" f&uuml;r viele Werte <EM>id(:)</EM> (Vektor) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_quant_idx_1_1 ( this, val ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_quant) , INTENT(IN) :: this(:) ! 
    !! zu suchende Werte der Komponente <EM>id</EM>
    CHARACTER (LEN=*)  , INTENT(IN) :: val(:)  ! 
    !! Ergebnis: Zeiger auf die Eintr&auml;ge in this(:), deren aktueller 
    !! Wert der Komponente <EM>id</EM> identisch mit <EM>id(:)</EM> ist <BR>
    !! wird kein Eintrag gefunden, so wird -1 zur&uuml;ckgegeben
    INTEGER :: res(SIZE(val)) ! 
    ! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(val)
       res(i) = get_omi_quant_idx_1_0 ( this(:), val(i) )
    END DO
    !
  END FUNCTION get_omi_quant_idx_1_1
  !
  !! ermittle die Anzahl der Quantit&auml;ten, die f&uuml;r eine physikalische
  !! Gr&ouml;&szlig;e (Code-Kennung) bereitzustellen sind <BR>
  !! <EM>Hinweis</EM>: verschiedene Fraktionen etc. gehen nicht in die Betrachtung ein <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_max_omi_quant_code_0 ( code ) &
       RESULT ( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e 
    INTEGER , INTENT(IN) :: code ! 
    !! Ergebnis: Anzahl der f&uuml;r die physikalische Gr&ouml;&szlig;e 
    !!           zu erzeugenden Quantit&auml;ten; falls Code ung&uuml;tig
    !!           ist, wird -1 zur&uuml;ckgegeben
    INTEGER              :: res  ! 
    !
    res = -1
    !
    IF ( is_phy_quant_valid( code ) ) THEN
       res = 0
       res = MERGE( c_quant_per_scalar, res, is_phy_quant_scalar( code ) )
       res = MERGE( c_quant_per_vector, res, is_phy_quant_vector( code ) )
    END IF
    !
  END FUNCTION get_max_omi_quant_code_0
  !
  !! ermittle die Anzahl der Quantit&auml;ten, die f&uuml;r mehrere physikalische
  !! Gr&ouml;&szlig;e (Code-Kennungen) bereitzustellen sind <BR>
  !! <EM>Hinweis</EM>: verschiedene Fraktionen etc. gehen nicht in die Betrachtung ein <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_max_omi_quant_code_1 ( code ) &
       RESULT ( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;en 
    INTEGER , INTENT(IN) :: code(:) ! 
    !! Ergebnis: Anzahl der f&uuml;r die physikalische Gr&ouml;&szlig;en 
    !!           zu erzeugenden Quantit&auml;ten; falls Code ung&uuml;tig
    !!           ist, wird -1 zur&uuml;ckgegeben
    INTEGER              :: res(SIZE(code))  ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(code)
       res(i) = get_max_omi_quant_code_0 ( code(i) )
    END DO
    !
  END FUNCTION get_max_omi_quant_code_1
  !
  !! ermittle die Anzahl der Quantit&auml;ten, die f&uuml;r eine physikalische
  !! Gr&ouml;&szlig;e (Beschreibung) bereitzustellen sind <BR>
  !! <EM>Hinweis</EM>: verschiedene Fraktionen etc. gehen nicht in die Betrachtung ein <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_max_omi_quant_descr_0 ( descr ) &
       RESULT ( res )
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e 
    CHARACTER (LEN=*) , INTENT(IN) :: descr ! 
    !! Ergebnis: Anzahl der f&uuml;r die physikalische Gr&ouml;&szlig;e 
    !!           zu erzeugenden Quantit&auml;ten; falls Beschreibung ung&uuml;tig
    !!           ist, wird -1 zur&uuml;ckgegeben
    INTEGER              :: res  ! 
    !! Hilfsvariable
    INTEGER              :: code ! 
    !
    code = get_phy_quant_code       ( descr )
    res  = get_max_omi_quant_code_0 ( code  )
    !
  END FUNCTION get_max_omi_quant_descr_0
  !
  !! ermittle die Anzahl der Quantit&auml;ten, die f&uuml;r mehrere physikalische
  !! Gr&ouml;&szlig;e (Beschreibungen) bereitzustellen sind <BR>
  !! <EM>Hinweis</EM>: verschiedene Fraktionen etc. gehen nicht in die Betrachtung ein <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_max_omi_quant_descr_1 ( descr ) &
       RESULT ( res )
    !! Beschreibungen der physikalischen Gr&ouml;&szlig;en
    CHARACTER (LEN=*) , INTENT(IN) :: descr(:) ! 
    !! Ergebnis: Anzahl der f&uuml;r die physikalischen Gr&ouml;&szlig;en
    !!           zu erzeugenden Quantit&auml;ten; falls Beschreibung ung&uuml;tig
    !!           ist, wird -1 zur&uuml;ckgegeben
    INTEGER              :: res(SIZE(descr))   ! 
    !! Hilfsvariable
    INTEGER              :: i    ! 
    !
    DO i=1,SIZE(descr)
       res(i) = get_max_omi_quant_descr_0 ( descr(i) )
    END DO
    !
  END FUNCTION get_max_omi_quant_descr_1
  !
  !! ermittle die Anzahl der zu erzeugenden Quantit&auml;ten aus der
  !! Variablenbeschreibung sowie den beigelegten Dimensionen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_max_omi_quant_var_dim_0_1 ( var, dim ) &
       RESULT ( res )
    !! Variablenbeschreibung
    TYPE (t_var) , INTENT(IN) :: var    ! 
    !! Dimensionen
    TYPE (t_dim) , INTENT(IN) :: dim(:) !
    !! Ergebnis: Anzahl der Quantit&auml;ten
    INTEGER :: res ! 
    !! Hilfsvariablen
    INTEGER               :: i, idx, mul  ! 
    INTEGER , POINTER     :: p_dim_id(:)  ! 
    INTEGER , ALLOCATABLE :: l_dim_idx(:) ! 
    !
    res = 1
    p_dim_id => get_var_dim_id ( var )
    IF ( ASSOCIATED( p_dim_id ) ) THEN
       ALLOCATE ( l_dim_idx(SIZE(p_dim_id)) )
       l_dim_idx(:) = get_dim_idx( dim(:), p_dim_id(:) )
       res = MERGE( 2, 1, get_vector_class_dim_count( dim(l_dim_idx(:)) ) > 0 )
       mul = res
       DO i=1,get_variant_class_dim_count( dim(l_dim_idx(:)) ) 
          IF ( i == 1 ) res = 0
          idx = get_variant_class_dim_idx ( dim(l_dim_idx(:)), i )
          IF ( idx > 0 ) THEN
             res = res + mul*get_dim_len( dim(l_dim_idx(idx)) )
          END IF
       END DO
       DEALLOCATE( l_dim_idx )
       DEALLOCATE( p_dim_id  )
       NULLIFY   ( p_dim_id )
    END IF
    !
  END FUNCTION get_max_omi_quant_var_dim_0_1
  !
  !! ermittle die Anzahl der zu erzeugenden Quantit&auml;ten aus den
  !! Variablenbeschreibungen sowie den beigelegten Dimensionen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_max_omi_quant_var_dim_1_1 ( var, dim ) &
       RESULT ( res )
    !! Variablenbeschreibungen
    TYPE (t_var) , INTENT(IN) :: var(:) ! 
    !! Dimensionen
    TYPE (t_dim) , INTENT(IN) :: dim(:) !
    !! Ergebnis: Anzahl der Quantit&auml;ten, differenziert nach Variablen
    INTEGER :: res(SIZE(var)) ! 
    !! Hilfsvariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var)
       res(i) = get_max_omi_quant_var_dim_0_1 ( var(i), dim(:) )
    END DO
    !
  END FUNCTION get_max_omi_quant_var_dim_1_1
  !
  !! Ermittle die Code-Kennung aus der ID einer Quantity <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_code_from_omi_quant_0 ( this ) &
       RESULT( res )
    !! Datenobjekt
    TYPE (t_omi_quant) , INTENT(IN) :: this !
    !! Ergebnis: Codebezeichnung der <EM>Quantity</EM>
    INTEGER :: res ! 
    !! Hilfsvariablen
    INTEGER :: ia, ie ! 
    !
    res = c_undef_omi_quant_int
    ia  = c_undef_omi_quant_int
    ie  = c_undef_omi_quant_int
    ia  = INDEX( this%id, '[' ) 
    ie  = INDEX( this%id, ']' ) 
    IF ( ie > ia + 1 ) READ(this%id(ia+1:ie-1),*) res
    !
  END FUNCTION get_code_from_omi_quant_0
  !
  !! Ermittle die Code-Kennungen aus den IDs vieler Quantities <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_code_from_omi_quant_1 ( this ) &
       RESULT( res )
    !! Datenobjekte
    TYPE (t_omi_quant) , INTENT(IN) :: this(:) !
    !! Ergebnis: Codebezeichnungen der <EM>Quantities</EM>
    INTEGER :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_code_from_omi_quant_0 ( this(i) )
    END DO
    !
  END FUNCTION get_code_from_omi_quant_1
  !
  !! Ermittle die Varianten-Nummer aus der ID einer Quantity <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_no_from_omi_quant_0 ( this ) &
       RESULT( res )
    !! Datenobjekt
    TYPE (t_omi_quant) , INTENT(IN) :: this !
    !! Ergebnis: Klassennummer der <EM>Quantity</EM>
    INTEGER :: res ! 
    !! Hilfsvariablen
    INTEGER :: ia  ! 
    !
    res = c_undef_omi_quant_int
    ia  = c_undef_omi_quant_int
    ia  = INDEX( this%id, ',' ) 
    IF ( ia > 0 ) READ(this%id(ia-2:ia-1),*) res
    !
  END FUNCTION get_var_no_from_omi_quant_0
  !
  !! Ermittle die Klassen-Dimensions-Nummer aus der ID einer Quantity <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_cla_no_from_omi_quant_0 ( this ) &
       RESULT( res )
    !! Datenobjekt
    TYPE (t_omi_quant) , INTENT(IN) :: this !
    !! Ergebnis: Klassennummer der <EM>Quantity</EM>
    INTEGER :: res ! 
    !! Hilfsvariablen
    INTEGER :: ia  ! 
    !
    res = c_undef_omi_quant_int
    ia  = c_undef_omi_quant_int
    ia  = INDEX( this%id, ',' ) 
    IF ( ia > 0 ) READ(this%id(ia+1:ia+2),*) res
    !
  END FUNCTION get_cla_no_from_omi_quant_0
  !
  !! Ermittle die Id der Referenzgr&ouml;&szlig;e zu einer physikalischen Quantity <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION get_omi_ref_quant_id_0 ( this ) &
       RESULT( res )
    !! Datenobjekt
    TYPE (t_omi_quant)    , INTENT(IN) :: this ! 
    !! Ergebnis: Id der Referenzgr&ouml;&szlig;e
    CHARACTER (LEN=c_len_omi_quant_id) :: res  ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=22) , PARAMETER   :: c_upname='get_omi_ref_quant_id_0' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=5)                :: ch                 ! 
    TYPE (t_omi_quant) , ALLOCATABLE :: l_quant(:)         ! 
    INTEGER                          :: nq, code, code_ref ! 
    !
    res      = REPEAT( ' ', LEN(res) )
    res      = c_undef_omi_quant_char
    code     = get_code_from_omi_quant ( this )
    code_ref = get_phy_quant_ref_code ( code ) 
    IF ( code_ref > 0 ) THEN
       IF ( is_phy_quant_scalar ( code_ref ) ) THEN
          nq = get_max_omi_quant ( code_ref ) 
          ALLOCATE( l_quant(nq) )
          CALL new_omi_quant  ( l_quant )
          CALL set_omi_quant  ( l_quant, code_ref )
          res = get_omi_quant_id ( l_quant(nq) )
          CALL kill_omi_quant ( l_quant )
          DEALLOCATE( l_quant   )
      ELSE
          CALL setup_error_act ( all_errors(:), 9000, c_upname, c_modname )
          CALL setup_error_act ( '<id>', TRIM(get_omi_quant_id( this )) )
          WRITE(ch,'(I5)') code_ref ; CALL setup_error_act ( '<code>', ch )
       END IF
    END IF
    ! 
  END FUNCTION get_omi_ref_quant_id_0
  !
  !! Ermittle die Id der zeitvariablen Topographie zu einer physikalischen Quantity <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION get_omi_bot_quant_id_0 ( this ) &
       RESULT( res )
    !! Datenobjekt
    TYPE (t_omi_quant)    , INTENT(IN) :: this ! 
    !! Ergebnis: Id der Referenzgr&ouml;&szlig;e
    CHARACTER (LEN=c_len_omi_quant_id) :: res  ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=22) , PARAMETER   :: c_upname='get_omi_bot_quant_id_0' ! 
    !! Hilfsvariablen
    TYPE (t_omi_quant) , ALLOCATABLE :: l_quant(:)         ! 
    INTEGER                          :: nq, code, code_bot ! 
    !
    res      = REPEAT( ' ', LEN(res) )
    res      = c_undef_omi_quant_char
    code     = get_code_from_omi_quant ( this )
    code_bot = 617
    nq = get_max_omi_quant ( code_bot ) 
    ALLOCATE( l_quant(nq) )
    CALL new_omi_quant  ( l_quant )
    CALL set_omi_quant  ( l_quant, code_bot )
    res = get_omi_quant_id ( l_quant(nq) )
    CALL kill_omi_quant ( l_quant )
    DEALLOCATE( l_quant   )
    ! 
  END FUNCTION get_omi_bot_quant_id_0
  !
  !! Ermittle die entsprechende Magnitude Quantit&auml;t aus einer vorgegebenen Quantit&auml;t <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_omi_quant_magnitude_quant_0 ( this ) &
       RESULT( res )
    !! Datenobjekt
    TYPE (t_omi_quant)   , INTENT(IN) :: this ! 
    !! Ergebnis : entsprechende Magnitude Quantit&auml;t oder undefiniert
    TYPE (t_omi_quant) :: res ! 
    !! Hilfsvariable
    INTEGER :: id1, de1 ! 
    !
    CALL new_omi_quant ( res )
    IF ( is_omi_quant_vector(this) ) THEN
       id1 = INDEX( this%id, c_component_name(1,2) )
       de1 = INDEX( this%description, c_component_name(1,2) )
       IF ( id1 > 0 .AND. de1 > 0 ) THEN
          res%id          = this%id
          res%id(id1:id1+LEN(c_component_name)-1) = c_component_name(2,2)
          res%description = this%description
          res%description(de1:de1+LEN(c_component_name)-1) = c_component_name(2,2)
          res%valuetype   = 1
          res%dimension   = this%dimension
          res%unit        = this%unit 
       END IF
    END IF
    !
  END FUNCTION get_omi_quant_magnitude_quant_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-IS-Methoden 
  ! ----------------------------------------------------------------------
  !
  !! ist die Quantit&auml;t eine <EM>skalare</EM> Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_omi_quant_scalar_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_quant) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !
    res = ( this%valuetype == 1 )
    !
  END FUNCTION is_omi_quant_scalar_0
  !
  !! sind die Quantit&auml;ten <EM>skalare</EM> Gr&ouml;&szlig;en <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_omi_quant_scalar_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_quant) , INTENT(IN) :: this(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(this)) ! 
    !
    res(:) = ( this(:)%valuetype == 1 )
    !
  END FUNCTION is_omi_quant_scalar_1
  !
  !! ist die Quantit&auml;t eine <EM>vektorielle</EM> Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_omi_quant_vector_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_quant) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !
    res = ( this%valuetype == 2 )
    !
  END FUNCTION is_omi_quant_vector_0
  !
  !! sind die Quantit&auml;ten <EM>vektorielle</EM> Gr&ouml;&szlig;en <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_omi_quant_vector_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_quant) , INTENT(IN) :: this(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(this)) ! 
    !
    res(:) = ( this(:)%valuetype == 2 )
    !
  END FUNCTION is_omi_quant_vector_1
  !
  !! ist die Quantit&auml;t eine <EM>Magnitude</EM> Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_omi_quant_magnitude_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_quant) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !
    IF ( is_omi_quant_scalar( this ) ) THEN
       res = ( INDEX( this%id, '(Mag)' ) > 0 )
    ELSE
       res = .false.
    END IF
    !
  END FUNCTION is_omi_quant_magnitude_0
  !
  !! sind die Quantit&auml;ten <EM>Magnitude</EM> Gr&ouml;&szlig;en <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_omi_quant_magnitude_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_quant) , INTENT(IN) :: this(:) ! 
    !! Testergebnis
    LOGICAL :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       res(i) = is_omi_quant_magnitude( this(i) )
    END DO
    !
  END FUNCTION is_omi_quant_magnitude_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_quant_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_quant) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_quant) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1)  = ( this1%id          == this2%id          )
    l_ok(2)  = ( this1%description == this2%description )
    l_ok(3)  = ( this1%valuetype   == this2%valuetype   )
    l_ok(4)  = eq_omi_dim ( this1%dimension, this2%dimension )
    l_ok(5)  = eq_omi_unit( this1%unit, this2%unit )
    !
    ok = ALL( l_ok )
    !
  END FUNCTION eq_omi_quant_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_quant_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_quant) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_quant) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    ! 
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_quant_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_omi_quant_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_quant_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_quant) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_quant) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_quant_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_omi_quant_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_quant_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_quant) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_quant) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_quant_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_omi_quant_1_1
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
  FUNCTION ne_omi_quant_0_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_quant) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_quant) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = .NOT. eq_omi_quant_0_0( this1, this2 )
    !
  END FUNCTION ne_omi_quant_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_quant_1_0 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_omi_quant) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_quant) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ok(:) = .NOT. eq_omi_quant_1_0( this1(:), this2 )
    !
  END FUNCTION ne_omi_quant_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Vektor )<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
 FUNCTION ne_omi_quant_0_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_quant) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_quant) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ok(:) = .NOT. eq_omi_quant_0_1( this1, this2(:) )
    !
  END FUNCTION ne_omi_quant_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_quant_1_1 &
       ( this1,  &
         this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_quant) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_quant) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Hilfsvariable
    INTEGER :: l ! 
    !
    l  = SIZE(ok) 
    ok = .NOT. eq_omi_quant_1_1( this1(1:l), this2(1:l) )
    !
  END FUNCTION ne_omi_quant_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-Copy-Methoden <<<
  ! ----------------------------------------------------------------------
  !
  !! kopiere den Inhalt einer Komponente in eine andere Komponente <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE copy_omi_quant_0_0 ( this1, this2 )
    !! Ziel-Datenobjekt
    TYPE (t_omi_quant) , INTENT(OUT) :: this1 ! 
    !! Quell-Datenobjekt
    TYPE (t_omi_quant) , INTENT(IN)  :: this2 ! 
    !
    CALL new_omi_quant_0               ( this1 )
    CALL set_omi_quant_id_0_0          ( this1, this2%id          )
    CALL set_omi_quant_description_0_0 ( this1, this2%description )
    CALL set_omi_quant_valuetype_0_0   ( this1, this2%valuetype   )
    CALL set_omi_quant_dimension_0_0   ( this1, this2%dimension   )
    CALL set_omi_quant_unit_0_0        ( this1, this2%unit        )
    !
  END SUBROUTINE copy_omi_quant_0_0
  !
  !! kopiere den Inhalt mehrere Komponente auf andere Komponenten <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE copy_omi_quant_1_1 ( this1, this2 )
    !! Ziel-Datenobjekt
    TYPE (t_omi_quant) , INTENT(OUT) :: this1(:) ! 
    !! Quell-Datenobjekt
    TYPE (t_omi_quant) , INTENT(IN)  :: this2(:) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,MIN(SIZE(this1),SIZE(this2))
       CALL copy_omi_quant_0_0 ( this1(i), this2(i) )
    END DO
    !
  END SUBROUTINE copy_omi_quant_1_1
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
    !! Name der Subroutine die "ok_initialised" ruft
    CHARACTER (LEN=*) , INTENT(IN) :: upname ! 
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
       WRITE(*,*) ' *** Warnung *** Modul "b_omi_quant" nicht initialisiert'
       !
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       !
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_omi_quant ausfuehren'
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
  SUBROUTINE init_omi_quant_all_errors &
       ( )
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER  :: c_upname='init_omi_quant_all_errors' !
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
               '--> INIT_omi_quant ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_omi_quant ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_quant"\n'//&
               'Typ-Komponente = "id" darf nicht leer/undefined sein\n'//&
               'aktuell        = <aktuell>\n'//&
               '--> Daten pruefen ' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_quant"\n'//&
               'Typ-Komponente = "description" darf nicht leer/undefined sein\n'//&
               'aktuell        = <aktuell>\n'//&
               '--> Daten pruefen ' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_quant"\n'//&
               'Typ-Komponente = "valuetype" hat unerlaubten Wert\n'//&
               'aktuell        = <aktuell>\n'//&
               'erlaubt        = <minimum> bis <maximum>\n'//&
               '--> Daten pruefen ' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_quant"\n'//&
               'Typ-Komponente = "dimension" [t_omi_dim]\n'//&
               '--> Daten pruefen ' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_quant"\n'//&
               'Typ-Komponente = "unit" [t_omi_unit]\n'//&
               '--> Daten pruefen ' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code in Modul "b_omi_quant" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Fusszeilen\n'//&
               '--> Code in Modul "b_omi_quant" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Index des Datenobjektes (1D-Array)\n'//&
               '--> Code in Modul "b_omi_quant" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_quant"\n'//&
               'Typ-Komponente = "id"\n'//&
               '--> Code in Modul "b_omi_quant" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_quant"\n'//&
               'Typ-Komponente = "description"\n'//&
               '--> Code in Modul "b_omi_quant" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_quant"\n'//&
               'Typ-Komponente = "valuetype"\n'//&
               '--> Code in Modul "b_omi_quant" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_quant"\n'//&
               'Typ-Komponente = "dimension"\n'//&
               '--> Code in Modul "b_omi_dim" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_quant"\n'//&
               'Typ-Komponente = "unit"\n'//&
               '--> Code in Modul "b_omi_unit" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "b_omi_quant"\n'//&
               '--> Code in Modul "b_omi_quant" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SET-Methoden\n'//&
               'Inkonsistente Feldgroessen beim Erzeugen der Quantities\n'//&
               'Code        = <code>\n'//&
               'Feldgroesse : aktuell = <actual>, erforderlich = <required>\n'//&
               '--> Aktualparameter pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SET-Methoden\n'//&
               'Inkonsistente Feldgroessen beim Erzeugen der Quantities\n'//&
               'Variable    = <variable>\n'//&
               'Feldgroesse : aktuell = <actual>, erforderlich = <required>\n'//&
               '--> Aktualparameter pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Fehler beim Ermitteln der Id der Ref.Groesse einer Quantity\n'//&
               'Referenzgroesse muss unbedingt eine skalare Groesse sein\n'//&
               'Quantity Id = <id>\n'//&
               'Ref. Code   = <code>\n'//&
               '--> Aktualparameter pruefen' )
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
  END SUBROUTINE init_omi_quant_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_omi_quant_all_errors &
       ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_omi_quant_all_errors
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
  !! Pr&uuml;fe, ob die Komponente "id" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_quant_id &
       ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_quant) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_omi_quant_id' ! 
    !
    ok = ( LEN_TRIM(this%id) > 0 .AND. &
           this%id(1:LEN(c_undef_omi_quant_char)) /= c_undef_omi_quant_char )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
       CALL setup_error_act ( '<aktuell>', this%id )
    END IF
    !
  END FUNCTION ok_omi_quant_id
  !
  !! Pr&uuml;fe, ob die Komponente "description" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_quant_description &
       ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_quant) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=24) , PARAMETER :: c_upname='ok_omi_quant_description' ! 
    !
    ok = ( LEN_TRIM(this%description) > 0 .AND. &
           this%description(1:LEN(c_undef_omi_quant_char)) /= c_undef_omi_quant_char )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
       CALL setup_error_act ( '<aktuell>', this%description )
    END IF
    !
  END FUNCTION ok_omi_quant_description
  !
  !! Pr&uuml;fe, ob die Komponente "valuetype" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_quant_valuetype &
       ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_quant) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=22) , PARAMETER :: c_upname='ok_omi_quant_valuetype' ! 
    !! Hilfsvariable
    CHARACTER (LEN=5) :: ch ! 
    !
    ok = ( this%valuetype >= 1 .AND. this%valuetype <= c_max_valuetype )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
       WRITE(ch,'(I5)') this%valuetype  ; CALL setup_error_act ( '<aktuell>', ch )
       WRITE(ch,'(I5)') 1               ; CALL setup_error_act ( '<minimum>', ch )
       WRITE(ch,'(I5)') c_max_valuetype ; CALL setup_error_act ( '<maximum>', ch )
    END IF
    !
  END FUNCTION ok_omi_quant_valuetype
  !
  !! Pr&uuml;fe, ob die Komponente "dimension" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_quant_dimension &
       ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_quant) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=22) , PARAMETER :: c_upname='ok_omi_quant_dimension' ! 
    !
    ok = ok_omi_dim ( this%dimension )
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6040, c_upname, c_modname )
    !
  END FUNCTION ok_omi_quant_dimension
  !
  !! Pr&uuml;fe, ob die Komponente "unit" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_quant_unit &
       ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_quant) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='ok_omi_quant_unit' ! 
    !
    ok = ok_omi_unit ( this%unit )
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6050, c_upname, c_modname )
    !
  END FUNCTION ok_omi_quant_unit
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "id" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_quant_id &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_quant) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_omi_quant_id' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) TRIM(this%id)
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente id  - - - - - - - - - - - - - - - - - ',/&
           '# aktuell = ',A,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_quant_id
  !
  !! Drucke den Inhalt der Komponente "description" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_quant_description &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_quant) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=27) , PARAMETER :: c_upname='print_omi_quant_description' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) TRIM(this%description)
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente description - - - - - - - - - - - - - ',/&
           '# aktuell = ',A,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_quant_description
  !
  !! Drucke den Inhalt der Komponente "valuetype" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_quant_valuetype &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_quant) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=25) , PARAMETER :: c_upname='print_omi_quant_valuetype' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat ) this%valuetype, TRIM(c_valuetype_name(this%valuetype,language))
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente valuetype - - - - - - - - - - - - - - ',/&
           '# aktuell = ',I5,' ist ein ',A,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_quant_valuetype
  !
  !! Drucke den Inhalt der Komponente "dimension" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_quant_dimension &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_quant) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=25) , PARAMETER :: c_upname='print_omi_quant_dimension' ! 
    !
    IF ( no_error( )  ) CALL print_omi_dim ( this%dimension )
    IF ( any_error( ) ) CALL setup_error_act ( all_errors(:), 7040, c_upname, c_modname )
    !
  END SUBROUTINE print_omi_quant_dimension
  !
  !! Drucke den Inhalt der Komponente "unit" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_quant_unit &
       ( this )
    !! Datenobjekt
    TYPE (t_omi_quant) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=20) , PARAMETER :: c_upname='print_omi_quant_unit' ! 
    !
    IF ( no_error( )  ) CALL print_omi_unit ( this%unit )
    IF ( any_error( ) ) CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname )
    !
  END SUBROUTINE print_omi_quant_unit
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
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-GET-Methoden
  ! ----------------------------------------------------------------------
  !
  !! Funktion ermittelt die Komponente "valuetype" aus dem "code" einer
  !! physikalischen Gr&ouml;&szlig;e <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_quant_valuetype_code ( code, nc ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: code ! 
    !! lfd. Komponenten-Nummer
    INTEGER , INTENT(IN) :: nc   ! 
    !! Ergebnis: valuetype
    INTEGER              :: res  ! 
    !! Hilfsvariable
    INTEGER              :: jc   ! 
    !
    res = c_undef_omi_quant_int
    jc  = c_undef_omi_quant_int
    jc  = MERGE( 1, jc, is_phy_quant_scalar( code ) )
    jc  = MERGE( 2, jc, is_phy_quant_vector( code ) )
    IF ( jc /= c_undef_omi_quant_int ) res = c_component_type(nc,jc)
    !
  END FUNCTION get_omi_quant_valuetype_code
  !
  !! Funktion ermittelt die Komponente "description" aus dem "code" einer
  !! physikalischen Gr&ouml;&szlig;e sowie der lfd. Nummer <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_quant_description_code ( code, nc ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: code ! 
    !! lfd. Komponenten-Nummer
    INTEGER , INTENT(IN) :: nc   ! 
    !! Ergebnis: description
    CHARACTER (LEN=c_len_omi_quant_description) :: res  ! 
    !! Hilfsvariablen
    INTEGER :: jc, l ! 
    !
    res = REPEAT( ' ', LEN(res) )
    res = c_undef_omi_quant_char
    jc  = get_omi_quant_valuetype_code ( code, 1 )
    IF ( jc >= 1 .AND. jc <= 2 ) THEN
       res = REPEAT( ' ', LEN(res) )
       res = get_phy_quant_descr( code )
       l   = LEN_TRIM(res)
       res(l+1:) = c_component_name(nc,jc)
    END IF
    !
  END FUNCTION get_omi_quant_description_code
  !
  !! Funktion ermittelt die Komponente "id" aus dem "code" einer
  !! physikalischen Gr&ouml;&szlig;e sowie der lfd. Nummer <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_quant_id_code ( code, nc ) &
       RESULT( res )
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER , INTENT(IN) :: code ! 
    !! lfd. Komponenten-Nummer
    INTEGER , INTENT(IN) :: nc   ! 
    !! Ergebnis: id
    CHARACTER (LEN=c_len_omi_quant_id) :: res  ! 
    !! Hilfsvariablen
    INTEGER :: jc, l ! 
    !
    res = REPEAT( ' ', LEN(res) )
    res = c_undef_omi_quant_char
    jc  = get_omi_quant_valuetype_code ( code, 1 )
    IF ( jc >= 1 .AND. jc <= 2 ) THEN
       res = REPEAT( ' ', LEN(res) )
       res = get_phy_quant_id( code )
       l   = LEN_TRIM(res)
       res(l+1:) = c_component_name(nc,jc)
       l   = LEN_TRIM(res)
       WRITE(res(l+1:l+7),'(A1,I5.5,A1)') '[',code,']'
    END IF
    !
  END FUNCTION get_omi_quant_id_code
  !
  !! Modifiziere die Komponente Id mit variantenbezogenen Angaben <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE modify_omi_quant_id ( this, jc, kd, text )
    !! Datenobjekt
    TYPE (t_omi_quant) , INTENT(INOUT) :: this ! 
    !! aktuelle Nummer der Variante / Klasse
    INTEGER            , INTENT(IN)    :: jc   ! 
    !! aktuelle Nummer der Variantendimension
    INTEGER            , INTENT(IN)    :: kd   ! 
    !! (optional) Text fuer Varianten / Klassenbezeichnung 
    CHARACTER (LEN=*)  , INTENT(IN) , OPTIONAL :: text ! 
    !! Hilfsvariablen
    INTEGER :: l, m ! 
    !
    l = LEN_TRIM( this%id )
    IF ( c_len_omi_quant_id - l >= 7 ) THEN
       WRITE(this%id(l+1:l+7),'(A1,I2.2,A1,I2.2,A1)') '(',jc,',',kd,')'
       l = LEN_TRIM(this%id)
       IF ( PRESENT(text) ) THEN
          m = MIN(LEN_TRIM(text),c_len_omi_quant_id-l-2)
          IF ( m > 0 ) WRITE(this%id(l+1:l+m+2),'(A1,A,A1)') '"',text(1:m),'"'
       END IF
       l = LEN_TRIM(this%id)
       this%id(l+1:) = REPEAT( ' ', c_len_omi_quant_id - l )
    ELSE
       this%id(l+1:c_len_omi_quant_id) = REPEAT( '?', c_len_omi_quant_id - l )
    END IF
    !
  END SUBROUTINE modify_omi_quant_id
  !
  !! Modifiziere die Komponente Description mit variantenbezogenen Angaben <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE modify_omi_quant_description ( this, jc, kd, text )
    !! Datenobjekt
    TYPE (t_omi_quant) , INTENT(INOUT) :: this ! 
    !! aktuelle Nummer der Variante / Klasse
    INTEGER            , INTENT(IN)    :: jc   ! 
    !! aktuelle Nummer der Variantendimension
    INTEGER            , INTENT(IN)    :: kd   ! 
    !! (optional) Text fuer Varianten / Klassenbezeichnung 
    CHARACTER (LEN=*)  , INTENT(IN) , OPTIONAL :: text ! 
    !! Hilfsvariablen
    INTEGER :: l, m ! 
    !
    l = LEN_TRIM( this%description )
    IF ( c_len_omi_quant_description - l >= 8 ) THEN
       WRITE(this%description(l+1:l+8),'(1X,A1,I2.2,A1,I2.2,A1)') '(',jc,',',kd,')'
       l = LEN_TRIM(this%description)
       IF ( PRESENT(text) ) THEN
          m = MIN(LEN_TRIM(text),c_len_omi_quant_description-l-3)
          IF ( m > 0 ) WRITE(this%description(l+1:l+m+3),'(1X,A1,A,A1)') '"',text(1:m),'"'
       END IF
       l = LEN_TRIM(this%description)
       this%description(l+1:) = REPEAT( ' ', c_len_omi_quant_description - l )
    ELSE
       this%description(l+1:c_len_omi_quant_description) = REPEAT( '?', c_len_omi_quant_description - l )
    END IF
    !
  END SUBROUTINE modify_omi_quant_description
  !
END MODULE b_omi_quant
! TailOfBaseModule --------------------------------------------------------
