! -------------------------------------------------------------------------
! HeadOfBaseModule --------------------------------------------------------
!
!! <H2>Typ und Methoden analog OpenMI-Interface "IDimension"</h2>
!! @author G&uuml;nther Lang
!! @version 3.1 vom 12/21/06, Quellcode: mod_b_omi_dim.f90
!! <HR>
!! type and methods equivalent to OpenMI interface "IDimension" <BR>
!! <HR>
!! <HR>
!  Copyright-Hinweis
!                                                                    <BR>
!  Copyright (C) 2005 <A HREF="http://www.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!                                                                    <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2005-01-21 : G. Lang : Erstversion
!  01.02 : 2005-03-11 : G. Lang : OPERATORen entfernt, auf Funktionen umgestellt
!  02.01 : 2005-07-21 : G. Lang : Anpassungen fuer korrigiertes ElementSet-Verstaendnis (GEI)
!  02.02 : 2005-08-10 : G. Lang : Dimension bei Flussgroessen anpassen, interne FUN modify_omi_dim_flux
!  03.01 : 2006-12-21 : G. Lang : Umstellen der SI-Basiseinheiten auf Real-Zahlen
!
!!                                                                  <BR>
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!!
!! Typ und Methoden analog OpenMI-Interface <EM>IDimension</EM>. Dient
!! dazu, die Dimension einer physikalischen Gr&ouml;&szlig;e in den 
!! elementaren SI-Einheiten bereitzustellen.
!!
!! <OL>
!!    <LI> Initialisierung und De-Initialisierung von skalaren und
!!         vektoriellen Variablen des Typs "t_omi_dim";
!!    <LI> Setzen der Komponenten in Variablen des Typs "t_omi_dim";
!!    <LI> Holen der Komponenten aus Variablen des Typs "t_omi_dim";
!!    <LI> Drucken des Inhalts der Komponenten von Variablen des Typs "t_omi_dim";
!!    <LI> Pr&uuml;fen des Inhalts von Variablen des Typs "t_omi_dim";
!!    <LI> Vergleichen des Inhalts verschiedener Variablen des Typs "t_omi_dim";
!!    <LI> Multiplikation und Division (Addieren und Subtrahieren der Basisdimensionen).
!! </OL>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt den selbst definierten Datentyp "t_omi_dim" 
!! zur Verf&uuml;gung. Dieser besteht aus den folgenden Komponenten:      <BR>
!! <OL>
!!     <LI> length            : Basisdimension L&auml;nge (<EM>length</EM>)
!!     <LI> mass              : Basisdimension Masse (<EM>mass</EM>)
!!     <LI> time              : Basisdimension Zeit (<EM>time</EM>)
!!     <LI> electriccurrent   : Basisdimension elektr. Strom (<EM>electric current</EM>)
!!     <LI> temperature       : Basisdimension Temperatur (<EM>temperature</EM>)
!!     <LI> amountofsubstance : Basisdimension Stoffmenge (<EM>amount of substance</EM>)
!!     <LI> luminousintensity : Basisdimension Lichtintensit&auml;t (<EM>luminous intensity</EM>)
!!     <LI> currency          : Basisdimension W&auml;hrung (<EM>currency</EM>)
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
!!    <LI> Initialisieren des Moduls b_omi_dim mit INIT-Methode;
!!    <LI> ggf. Verwenden verschiedener SETUP-Methoden;
!!    <LI> Verwenden beliebiger Modul-Methoden (nicht INIT und CLEAR);
!!    <LI> De-Initialisieren des Moduls b_omi_dim mit CLEAR-Methode.
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
!!          die Methode PRINT_OMI_DIM_ALL_ERRORS.
!! <BR>
!! <HR>
!
MODULE b_omi_dim
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
       ok_phy,                     &
       get_phy_quant_code,         &
       get_phy_unit_max_si_base,   &
       get_phy_unit_si_base_power, &
       is_phy_quant_valid,         &
       is_phy_quant_flux
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
       get_var_id
  !
  ! [A.5] BASIS-Modul mit Typ und Methoden Attributbezeichner
  !
  USE b_att, ONLY :        &
       ! Datentyp
       t_att,              &
       ! Konstante
       c_att_name,         &
       ! Routinen / Interfaces
       init_att,           &
       clear_att,          &
       setup_att_prn_lun,  &
       setup_att_trc_lun,  &
       is_att_in,          &
       is_att_ch,          &
       get_att_in,         &
       get_att_ch,         &
       get_att_idx,        &
       get_att_nof_values, &
       get_att_interfaces_count, &
       has_att_cross_sectional_average
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
  !! Typ zur Aufnahme aller SI-Basisdimensionen einer physikalischen Gr&ouml;&szlig;e:     <BR>
  !! length            : Basisdimension L&auml;nge (<EM>length</EM>)                       <BR>
  !! mass              : Basisdimension Masse (<EM>mass</EM>)                              <BR>
  !! time              : Basisdimension Zeit (<EM>time</EM>)                               <BR>
  !! electriccurrent   : Basisdimension elektr. Strom (<EM>electric current</EM>)          <BR>
  !! temperature       : Basisdimension Temperatur (<EM>temperature</EM>)                  <BR>
  !! amountofsubstance : Basisdimension Stoffmenge (<EM>amount of substance</EM>)          <BR>
  !! luminousintensity : Basisdimension Lichtintensit&auml;t (<EM>luminous intensity</EM>) <BR>
  !! currency          : Basisdimension W&auml;hrung (<EM>currency</EM>) 
  TYPE , PUBLIC :: t_omi_dim
     PRIVATE
     REAL (KIND=Double) :: length             ! 
     REAL (KIND=Double) :: mass               ! 
     REAL (KIND=Double) :: time               ! 
     REAL (KIND=Double) :: electriccurrent    ! 
     REAL (KIND=Double) :: temperature        ! 
     REAL (KIND=Double) :: amountofsubstance  ! 
     REAL (KIND=Double) :: luminousintensity  ! 
     REAL (KIND=Double) :: currency           ! 
  END TYPE t_omi_dim
  !
  ! [C.2] Konstantwerte (Parameter) [moeglichst nicht verwenden]
  !
  !! Kennzeichnung undefinierter Basisdimensionen
  REAL (KIND=Double) , PUBLIC , PARAMETER :: c_omi_dim_undefined=999._Double ! 
  !
  ! [C.3] Variablen [moeglichst nicht verwenden]
  !
  ! [C.4] Schnittstellen
  !
  ! [C.4.1] erforderliche oeffentliche Schnittstellen
  !
  !! Allokieren/Initialisieren der statischen Datenobjekte des Moduls; <BR>
  !! Initialisieren der statischen Modul-Daten mit Default-Werten.
  INTERFACE init_omi_dim
     MODULE PROCEDURE init_omi_dim_d ! 
  END INTERFACE
  !
  !! De-Allokieren/De-Initialisieren der statischen Datenobjekte des Moduls; <BR>
  !! Re-Initialisieren einiger statischer Daten mit Default-Werten.
  INTERFACE clear_omi_dim
     MODULE PROCEDURE clear_omi_dim_d ! 
  END INTERFACE
  !
  !! logische Kanalnummer f&uuml;r PRINT-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>PRN_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>PRN_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>PRN_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_omi_dim_prn_lun
     MODULE PROCEDURE setup_omi_dim_prn_lun_d ! 
  END INTERFACE
  !
  !! logische Kanalnummer f&uuml;r TRACE-Methoden auf Benutzerwert setzen; <BR>
  !! keine Ausgabe: <EM>TRC_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>TRC_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>TRC_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_omi_dim_trc_lun
     MODULE PROCEDURE setup_omi_dim_trc_lun_d ! 
  END INTERFACE
  !
  !! Erzeugen von Datenobjekten "t_omi_dim" und Initialisieren mit Default-Werten: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE new_omi_dim
     MODULE PROCEDURE new_omi_dim_0  ! 
     MODULE PROCEDURE new_omi_dim_1  ! 
  END INTERFACE
  !
  !! Vernichten von Datenobjekten "t_omi_dim" (Skalar, 1D-Array) und Re-Initialisieren mit Default-Werten: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE kill_omi_dim
     MODULE PROCEDURE kill_omi_dim_0 ! 
     MODULE PROCEDURE kill_omi_dim_1 ! 
  END INTERFACE
  !
  !! Pr&uuml;fen von Datenobjekten "t_omi_dim" auf G&uuml;ltigkeit: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar) <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor)
  INTERFACE ok_omi_dim
     MODULE PROCEDURE ok_omi_dim_0 ! 
     MODULE PROCEDURE ok_omi_dim_1 ! 
  END INTERFACE
  !
  !! Drucken von Datenobjekten "t_omi_dim":  <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)    <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) <BR>
  !! Alle Komponenten des Typs "t_omi_dim" auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_dim
     MODULE PROCEDURE print_omi_dim_0 ! 
     MODULE PROCEDURE print_omi_dim_1 ! 
  END INTERFACE
  !
  !! Drucken aller in diesem Modul abgelegten statischen Daten; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_dim_static
     MODULE PROCEDURE print_omi_dim_static_d ! 
  END INTERFACE
  !
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Moduls; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_omi_dim_all_errors
     MODULE PROCEDURE print_omi_dim_all_errors_d ! 
  END INTERFACE
  !
  !! Setze Komponente "length" in "t_omi_dim" auf Benutzerwert: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)    <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) 
  INTERFACE set_omi_dim_length
     MODULE PROCEDURE set_omi_dim_length_0_0 ! 
     MODULE PROCEDURE set_omi_dim_length_1_0 ! 
  END INTERFACE
  !! Setze Komponente "mass" in "t_omi_dim" auf Benutzerwert: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)    <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) 
  INTERFACE set_omi_dim_mass
     MODULE PROCEDURE set_omi_dim_mass_0_0 ! 
     MODULE PROCEDURE set_omi_dim_mass_1_0 ! 
  END INTERFACE
  !! Setze Komponente "time" in "t_omi_dim" auf Benutzerwert: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)    <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) 
  INTERFACE set_omi_dim_time
     MODULE PROCEDURE set_omi_dim_time_0_0 ! 
     MODULE PROCEDURE set_omi_dim_time_1_0 ! 
  END INTERFACE
  !! Setze Komponente "electriccurrent" in "t_omi_dim" auf Benutzerwert: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)    <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) 
  INTERFACE set_omi_dim_electriccurrent
     MODULE PROCEDURE set_omi_dim_electriccur_0_0 ! 
     MODULE PROCEDURE set_omi_dim_electriccur_1_0 ! 
  END INTERFACE
  !! Setze Komponente "temperature" in "t_omi_dim" auf Benutzerwert: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)    <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) 
  INTERFACE set_omi_dim_temperature
     MODULE PROCEDURE set_omi_dim_temperature_0_0 ! 
     MODULE PROCEDURE set_omi_dim_temperature_1_0 ! 
  END INTERFACE
  !! Setze Komponente "amountofsubstance" in "t_omi_dim" auf Benutzerwert: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)    <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) 
  INTERFACE set_omi_dim_amountofsubstance
     MODULE PROCEDURE set_omi_dim_amountofsub_0_0 ! 
     MODULE PROCEDURE set_omi_dim_amountofsub_1_0 ! 
  END INTERFACE
  !! Setze Komponente "luminousintensity" in "t_omi_dim" auf Benutzerwert: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)    <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) 
  INTERFACE set_omi_dim_luminousintensity
     MODULE PROCEDURE set_omi_dim_luminousint_0_0 ! 
     MODULE PROCEDURE set_omi_dim_luminousint_1_0 ! 
  END INTERFACE
  !! Setze Komponente "currency" in "t_omi_dim" auf Benutzerwert: <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)    <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) 
  INTERFACE set_omi_dim_currency
     MODULE PROCEDURE set_omi_dim_currency_0_0 ! 
     MODULE PROCEDURE set_omi_dim_currency_1_0 ! 
  END INTERFACE
  !
  !! Hole Komponente "length" aus "t_omi_dim": <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)      <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) 
  INTERFACE get_omi_dim_length
     MODULE PROCEDURE get_omi_dim_length_0_0 ! 
     MODULE PROCEDURE get_omi_dim_length_1_0 ! 
  END INTERFACE
  !! Hole Komponente "mass" aus "t_omi_dim": <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)    <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) 
  INTERFACE get_omi_dim_mass
     MODULE PROCEDURE get_omi_dim_mass_0_0 ! 
     MODULE PROCEDURE get_omi_dim_mass_1_0 ! 
  END INTERFACE
  !! Hole Komponente "time" aus "t_omi_dim": <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)    <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) 
  INTERFACE get_omi_dim_time
     MODULE PROCEDURE get_omi_dim_time_0_0 ! 
     MODULE PROCEDURE get_omi_dim_time_1_0 ! 
  END INTERFACE
  !! Hole Komponente "electriccurrent" aus "t_omi_dim": <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)               <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) 
  INTERFACE get_omi_dim_electriccurrent
     MODULE PROCEDURE get_omi_dim_electriccur_0_0 ! 
     MODULE PROCEDURE get_omi_dim_electriccur_1_0 ! 
  END INTERFACE
  !! Hole Komponente "temperature" aus "t_omi_dim": <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)    <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) 
  INTERFACE get_omi_dim_temperature
     MODULE PROCEDURE get_omi_dim_temperature_0_0 ! 
     MODULE PROCEDURE get_omi_dim_temperature_1_0 ! 
  END INTERFACE
  !! Hole Komponente "amountofsubstance" aus "t_omi_dim": <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)    <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) 
  INTERFACE get_omi_dim_amountofsubstance
     MODULE PROCEDURE get_omi_dim_amountofsub_0_0 ! 
     MODULE PROCEDURE get_omi_dim_amountofsub_1_0 ! 
  END INTERFACE
  !! Hole Komponente "luminousintensity" aus "t_omi_dim": <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)    <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) 
  INTERFACE get_omi_dim_luminousintensity
     MODULE PROCEDURE get_omi_dim_luminousint_0_0 ! 
     MODULE PROCEDURE get_omi_dim_luminousint_1_0 ! 
  END INTERFACE
  !! Hole Komponente "currency" aus "t_omi_dim": <BR>
  !! a) f&uuml;r ein Datenobjekt (Skalar)        <BR>
  !! b) f&uuml;r viele Datenobjekte (Vektor) 
  INTERFACE get_omi_dim_currency
     MODULE PROCEDURE get_omi_dim_currency_0_0 ! 
     MODULE PROCEDURE get_omi_dim_currency_1_0 ! 
  END INTERFACE
  !
  ! ... ggf. Holen fuer weitere Komponenten des Datenobjektes ergaenzen
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  !! Setze alle Basisdimensionen mit Hilfe verschiedener Hilfsinformationen: <BR>
  !! a) bei vorgegebener Code-Kennung einer physikalischen Gr&ouml;&szlig;e <BR>
  !! b) bei vorgegebenen Code-Kennungen mehrerer physikalischer Gr&ouml;&szlig;en <BR>
  !! c) bei vorgegebener Beschreibung einer physikalischen Gr&ouml;&szlig;e <BR>
  !! d) bei vorgegebenen Beschreibungen mehrerer physikalischer Gr&ouml;&szlig;en <BR>
  !! e) bei vorgegebener Variablenbeschreibung (t_var) und einer Attributliste (t_att) <BR>
  !! e) bei vorgegebenen Variablenbeschreibungen (t_var) und einer Attributliste (t_att)
  INTERFACE set_omi_dim
     MODULE PROCEDURE set_omi_dim_code_0_0
     MODULE PROCEDURE set_omi_dim_code_1_1
     MODULE PROCEDURE set_omi_dim_descr_0_0
     MODULE PROCEDURE set_omi_dim_descr_1_1
     MODULE PROCEDURE set_omi_dim_var_att_0_0_1
     MODULE PROCEDURE set_omi_dim_var_att_1_1_1
  END INTERFACE
  !
  !! Ermittle die SI-Basisdimension bei vorgegebenem Index der Basisdimension <BR>
  !! Index 1 = L&auml;nge            <BR>
  !! Index 2 = Masse                 <BR>
  !! Index 3 = Zeit                  <BR>
  !! Index 4 = elektr. Strom         <BR>
  !! Index 5 = Temperatur            <BR>
  !! Index 6 = Stoffmenge            <BR>
  !! Index 7 = Lichtintensit&auml;t  <BR>
  !! Index 8 = W&auml;hrung          <BR>
  !! a) f&uuml;r ein Objekt (Skalar) <BR>
  !! b) f&uuml;r viele Objekte (Vektor)
  INTERFACE get_omi_dim_si_base_power
     MODULE PROCEDURE get_omi_dim_si_base_power_0
     MODULE PROCEDURE get_omi_dim_si_base_power_1
  END INTERFACE 
  !! Ermittle die Anzahl der SI-Basiseinheiten
  INTERFACE get_omi_dim_max_si_base
     MODULE PROCEDURE get_omi_dim_max_si_base_d
  END INTERFACE
  !
  !! Pr&uuml;fen zweier Datenobjekte "t_omi_dim" auf Gleichheit (Funktion) <BR>
  !! a) Skalar1 == Skalar2 <BR>
  !! b) Skalar1 == Vektor2 <BR>
  !! c) Vektor1 == Skalar2 <BR>
  !! d) Vektor1 == Vektor2
  INTERFACE eq_omi_dim
     MODULE PROCEDURE eq_omi_dim_0_0  ! 
     MODULE PROCEDURE eq_omi_dim_0_1  ! 
     MODULE PROCEDURE eq_omi_dim_1_0  ! 
     MODULE PROCEDURE eq_omi_dim_1_1  ! 
  END INTERFACE
  !! Multiplikation zweier Datenobjekte "t_omi_dim" (Funktion) <BR>
  !! es wird eine Addition der Exponenten durchgef&uuml;hrt, die einer
  !! Multiplikation zweier Datens&auml;tze mit entsprechender Dimension entspricht <BR>
  !! a) Skalar1 * Skalar2 <BR>
  !! b) Skalar1 * Vektor2 <BR>
  !! c) Vektor1 * Skalar2 <BR>
  !! d) Vektor1 * Vektor2
  INTERFACE mu_omi_dim
     MODULE PROCEDURE mu_omi_dim_0_0  ! 
     MODULE PROCEDURE mu_omi_dim_0_1  ! 
     MODULE PROCEDURE mu_omi_dim_1_0  ! 
     MODULE PROCEDURE mu_omi_dim_1_1  ! 
  END INTERFACE
  !! Division zweier Datenobjekte "t_omi_dim" (Funktion) <BR>
  !! es wird eine Subtraktion der Exponenten durchgef&uuml;hrt, die einer
  !! Division zweier Datens&auml;tze mit entsprechender Dimension entspricht <BR>
  !! a) Skalar1 / Skalar2 <BR>
  !! b) Skalar1 / Vektor2 <BR>
  !! c) Vektor1 / Skalar2 <BR>
  !! d) Vektor1 / Vektor2
  INTERFACE di_omi_dim
     MODULE PROCEDURE di_omi_dim_0_0  ! 
     MODULE PROCEDURE di_omi_dim_0_1  ! 
     MODULE PROCEDURE di_omi_dim_1_0  ! 
     MODULE PROCEDURE di_omi_dim_1_1  ! 
  END INTERFACE
  !! Pr&uuml;fen zweier Datenobjekte "t_omi_dim" auf Ungleichheit (Funktion) <BR>
  !! a) Skalar1 /= Skalar2 <BR>
  !! b) Skalar1 /= Vektor2 <BR>
  !! c) Vektor1 /= Skalar2 <BR>
  !! d) Vektor1 /= Vektor2
  INTERFACE ne_omi_dim
     MODULE PROCEDURE ne_omi_dim_0_0  ! 
     MODULE PROCEDURE ne_omi_dim_0_1  ! 
     MODULE PROCEDURE ne_omi_dim_1_0  ! 
     MODULE PROCEDURE ne_omi_dim_1_1  ! 
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
  PUBLIC :: init_omi_dim
  PUBLIC :: clear_omi_dim
  PUBLIC :: setup_omi_dim_prn_lun
  PUBLIC :: setup_omi_dim_trc_lun
  PUBLIC :: new_omi_dim
  PUBLIC :: kill_omi_dim
  PUBLIC :: ok_omi_dim
  PUBLIC :: print_omi_dim
  PUBLIC :: print_omi_dim_static
  PUBLIC :: print_omi_dim_all_errors
  PUBLIC :: set_omi_dim_length
  PUBLIC :: set_omi_dim_mass
  PUBLIC :: set_omi_dim_time
  PUBLIC :: set_omi_dim_electriccurrent
  PUBLIC :: set_omi_dim_temperature
  PUBLIC :: set_omi_dim_amountofsubstance
  PUBLIC :: set_omi_dim_luminousintensity
  PUBLIC :: set_omi_dim_currency
  PUBLIC :: get_omi_dim_length
  PUBLIC :: get_omi_dim_mass
  PUBLIC :: get_omi_dim_time
  PUBLIC :: get_omi_dim_electriccurrent
  PUBLIC :: get_omi_dim_temperature
  PUBLIC :: get_omi_dim_amountofsubstance
  PUBLIC :: get_omi_dim_luminousintensity
  PUBLIC :: get_omi_dim_currency
  PUBLIC :: eq_omi_dim
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: set_omi_dim
  PUBLIC :: get_omi_dim_si_base_power 
  PUBLIC :: get_omi_dim_max_si_base
  PUBLIC :: mu_omi_dim
  PUBLIC :: di_omi_dim
  PUBLIC :: ne_omi_dim
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
  CHARACTER (LEN=09), PARAMETER :: c_modname      = 'b_omi_dim' ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.          ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1               ! 
  !! Anzahl der Datenkomponenten des Typs t_omi_dim
  INTEGER           , PARAMETER :: c_nofcomp      =  8               ! ggf. modifizieren
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
  !! Modifiziere eine Dimension mit Angaben aus der Attributliste,
  !! insofern es sich um eine Flussgr&ouml;&szlig;e handelt
  INTERFACE modify_omi_dim_flux
     MODULE PROCEDURE modify_omi_dim_flux_0_1
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
  SUBROUTINE init_omi_dim_d ( )
    !
    USE b_error, ONLY : DEBUG_b
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='init_omi_dim_d' 
    !! Hilfsvariable
    LOGICAL :: ok ! 
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_b > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "b_omi_dim" version 3.1 of 12/21/06                 '
          WRITE(*,*) ' Copyright (C) 2005 Bundesanstalt fuer Wasserbau   '
          WRITE(*,*)
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       IF ( no_error( ) ) CALL init_var ( )
       IF ( no_error( ) ) CALL init_att ( )
       IF ( no_error( ) ) CALL init_phy ( )
       IF ( no_error( ) ) ok = ok_phy   ( )
       ! ... ggf. weitere Initialisierungen ergaenzen
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_omi_dim_all_errors ( ) 
       ! [1.5] Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.6] endgueltiges Setzen des Initialisierungs-Indikators
       initialised = MERGE( .true., .false., no_error( ) )
    END IF
    ! 2.0 Initialisierungszaehler heraufsetzen
    n_init = n_init + 1
    !
  END SUBROUTINE init_omi_dim_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_omi_dim_d ( )
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='clear_omi_dim_d' ! 
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_omi_dim_all_errors ( ) 
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.3] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.4] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.4.1] ggf. weitere Module de-initialisieren
       IF ( no_error( ) ) CALL clear_phy ( )
       IF ( no_error( ) ) CALL clear_att ( )
       IF ( no_error( ) ) CALL clear_var ( )
       ! ... ggf. weitere De-Initialisierungen ergaenzen
       ! [1.4.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
    !
  END SUBROUTINE clear_omi_dim_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_omi_dim_prn_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER :: c_upname='setup_omi_dim_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_var_prn_lun   ( lun )
       IF ( no_error( ) ) CALL setup_att_prn_lun   ( lun )
       IF ( no_error( ) ) CALL setup_phy_prn_lun   ( lun )
       ! ... ggf. weitere ergaenzen
    END IF
    !
  END SUBROUTINE setup_omi_dim_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_omi_dim_trc_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER :: c_upname='setup_omi_dim_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_var_trc_lun   ( lun )
       IF ( no_error( ) ) CALL setup_att_trc_lun   ( lun )
       IF ( no_error( ) ) CALL setup_phy_trc_lun   ( lun )
       ! ... ggf. weitere ergaenzen
    END IF
    !
  END SUBROUTINE setup_omi_dim_trc_lun_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_omi_dim_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dim) , INTENT(OUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=13), PARAMETER :: c_upname='new_omi_dim_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       this%length            = c_omi_dim_undefined
       this%mass              = c_omi_dim_undefined
       this%time              = c_omi_dim_undefined
       this%electriccurrent   = c_omi_dim_undefined
       this%temperature       = c_omi_dim_undefined
       this%amountofsubstance = c_omi_dim_undefined
       this%luminousintensity = c_omi_dim_undefined
       this%currency          = c_omi_dim_undefined
    END IF
    !
  END SUBROUTINE new_omi_dim_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_omi_dim_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dim) , INTENT(OUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=13), PARAMETER :: c_upname='new_omi_dim_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL new_omi_dim_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_omi_dim_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_omi_dim_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dim) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='kill_omi_dim_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       IF ( no_error( ) ) CALL new_omi_dim_0 ( this )
    END IF
    !
  END SUBROUTINE kill_omi_dim_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_omi_dim_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dim) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='kill_omi_dim_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(this) .OR. any_error( ) ) EXIT
          CALL kill_omi_dim_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_omi_dim_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_dim_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dim) , INTENT(IN) :: this ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=12), PARAMETER :: c_upname='ok_omi_dim_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok(1)  = ok_omi_dim_length( this )
       l_ok(2)  = ok_omi_dim_mass( this )
       l_ok(3)  = ok_omi_dim_time( this )
       l_ok(4)  = ok_omi_dim_electriccurrent( this )
       l_ok(5)  = ok_omi_dim_temperature( this )
       l_ok(6)  = ok_omi_dim_amountofsubstance( this )
       l_ok(7)  = ok_omi_dim_luminousintensity( this )
       l_ok(8)  = ok_omi_dim_currency( this )
    END IF
    !
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_omi_dim_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_omi_dim_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dim) , INTENT(IN) :: this(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=12), PARAMETER :: c_upname='ok_omi_dim_1' 
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
          ok(i) = ok_omi_dim_0 ( this(i) )
       END DO
    END IF
    !
  END FUNCTION ok_omi_dim_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_dim_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dim) , INTENT(IN) :: this ! 
    !! Name der Function
    CHARACTER (LEN=15), PARAMETER :: c_upname='print_omi_dim_0' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7001, c_upname, c_modname, stat )
       IF ( no_error( ) ) CALL print_omi_dim_length           ( this )
       IF ( no_error( ) ) CALL print_omi_dim_mass             ( this )
       IF ( no_error( ) ) CALL print_omi_dim_time             ( this )
       IF ( no_error( ) ) CALL print_omi_dim_electriccurrent  ( this )
       IF ( no_error( ) ) CALL print_omi_dim_temperature      ( this )
       IF ( no_error( ) ) CALL print_omi_dim_amountofsubstance( this )
       IF ( no_error( ) ) CALL print_omi_dim_luminousintensity( this )
       IF ( no_error( ) ) CALL print_omi_dim_currency         ( this )
       IF ( no_error( ) ) THEN
          WRITE &
               ( UNIT    = prn_lun,  &
                 FMT     = 8001,     & 
                 IOSTAT  = stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7002, c_upname, c_modname, stat )
       END IF
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT('# Beginn Objekt t_omi_dim ------------------------------')
8001 FORMAT('# Ende   Objekt t_omi_dim ------------------------------')
    !
  END SUBROUTINE print_omi_dim_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_dim_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dim) , INTENT(IN) :: this(:) ! 
    !! Name der Function
    CHARACTER (LEN=15), PARAMETER :: c_upname='print_omi_dim_1' 
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
          WRITE &
               ( UNIT    = prn_lun,  &
                 FMT     = 8000,     & 
                 IOSTAT  = stat ) i
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7003, c_upname, c_modname, stat )
          IF ( no_error( ) ) CALL print_omi_dim_0 ( this(i) )
       END DO
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT ('# Datenobjekt-Index i = ',I10.10,' ---------------------------')
    !
  END SUBROUTINE print_omi_dim_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_dim_static_d ( )
    !! Name der Function
    CHARACTER (LEN=22), PARAMETER :: c_upname='print_omi_dim_static_d' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       WRITE &
         ( UNIT    = prn_lun,  &
           FMT     = 8000,     & 
           IOSTAT  = stat )    &
           initialised, prn_op, trc_op, prn_lun, trc_lun, n_init
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       IF ( no_error( ) ) CALL print_omi_dim_all_errors_d ( )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls b_omi_dim         ',/ &
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
  END SUBROUTINE print_omi_dim_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_dim_all_errors_d ( )
    !! Name der Function
    CHARACTER (LEN=26), PARAMETER :: c_upname='print_omi_dim_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_omi_dim_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! weise der Komponente "length" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_dim_length_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dim)   , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "length"
    REAL (KIND=Double) , INTENT(IN)    :: val  ! 
    !
    this%length = val
    !
  END SUBROUTINE set_omi_dim_length_0_0
  !
  !! weise der Komponente "length" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_dim_length_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dim)   , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "length"
    REAL (KIND=Double) , INTENT(IN)    :: val     ! 
    !
    this(:)%length = val
    !
  END SUBROUTINE set_omi_dim_length_1_0
  !
  !! weise der Komponente "mass" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_dim_mass_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dim)   , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "mass"
    REAL (KIND=Double) , INTENT(IN)    :: val  ! 
    !
    this%mass = val
    !
  END SUBROUTINE set_omi_dim_mass_0_0
  !
  !! weise der Komponente "mass" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_dim_mass_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dim)   , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "mass"
    REAL (KIND=Double) , INTENT(IN)    :: val     ! 
    !
    this(:)%mass = val
    !
  END SUBROUTINE set_omi_dim_mass_1_0
  !
  !! weise der Komponente "time" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_dim_time_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dim)   , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "time"
    REAL (KIND=Double) , INTENT(IN)    :: val  ! 
    !
    this%time = val
    !
  END SUBROUTINE set_omi_dim_time_0_0
  !
  !! weise der Komponente "time" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_dim_time_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dim)   , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "time"
    REAL (KIND=Double) , INTENT(IN)    :: val     ! 
    !
    this(:)%time = val
    !
  END SUBROUTINE set_omi_dim_time_1_0
  !
  !! weise der Komponente "electriccurrent" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_dim_electriccur_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dim)   , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "electriccurrent"
    REAL (KIND=Double) , INTENT(IN)    :: val  ! 
    !
    this%electriccurrent = val
    !
  END SUBROUTINE set_omi_dim_electriccur_0_0
  !
  !! weise der Komponente "electriccurrent" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_dim_electriccur_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dim)   , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "electriccurrent"
    REAL (KIND=Double) , INTENT(IN)    :: val     ! 
    !
    this(:)%electriccurrent = val
    !
  END SUBROUTINE set_omi_dim_electriccur_1_0
  !
  !! weise der Komponente "temperature" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_dim_temperature_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dim)   , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "temperature"
    REAL (KIND=Double) , INTENT(IN)    :: val  ! 
    !
    this%temperature = val
    !
  END SUBROUTINE set_omi_dim_temperature_0_0
  !
  !! weise der Komponente "temperature" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_dim_temperature_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dim)   , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "temperature"
    REAL (KIND=Double) , INTENT(IN)    :: val     ! 
    !
    this(:)%temperature = val
    !
  END SUBROUTINE set_omi_dim_temperature_1_0
  !
  !! weise der Komponente "amountofsubstance" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_dim_amountofsub_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dim)   , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "amountofsubstance"
    REAL (KIND=Double) , INTENT(IN)    :: val  ! 
    !
    this%amountofsubstance = val
    !
  END SUBROUTINE set_omi_dim_amountofsub_0_0
  !
  !! weise der Komponente "amountofsubstance" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_dim_amountofsub_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dim)   , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "amountofsubstance"
    REAL (KIND=Double) , INTENT(IN)    :: val     ! 
    !
    this(:)%amountofsubstance = val
    !
  END SUBROUTINE set_omi_dim_amountofsub_1_0
  !
  !! weise der Komponente "luminousintensity" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_dim_luminousint_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dim)   , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "luminousintensity"
    REAL (KIND=Double) , INTENT(IN)    :: val  ! 
    !
    this%luminousintensity = val
    !
  END SUBROUTINE set_omi_dim_luminousint_0_0
  !
  !! weise der Komponente "luminousintensity" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_dim_luminousint_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dim)   , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "luminousintensity"
    REAL (KIND=Double) , INTENT(IN)    :: val     ! 
    !
    this(:)%luminousintensity = val
    !
  END SUBROUTINE set_omi_dim_luminousint_1_0
  !
  !! weise der Komponente "currency" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_dim_currency_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dim)   , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "currency"
    REAL (KIND=Double) , INTENT(IN)    :: val  ! 
    !
    this%currency = val
    !
  END SUBROUTINE set_omi_dim_currency_0_0
  !
  !! weise der Komponente "currency" einen skalaren Wert zu (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_omi_dim_currency_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dim)   , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "currency"
    REAL (KIND=Double) , INTENT(IN)    :: val     ! 
    !
    this(:)%currency = val
    !
  END SUBROUTINE set_omi_dim_currency_1_0
  !
  !! Setze alle Komponenten eines Datenobjektes auf der Basis der 
  !! Code-Kennung einer physikalischen Gr&ouml;&szlig;e <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_omi_dim_code_0_0 ( this, code ) 
    !! Datenobjekt (Skalar) [ falls das Objekt nicht definiert
    !! werden kann, sind alle Komponenten mit 999 initialisiert ]
    TYPE (t_omi_dim) , INTENT(INOUT) :: this ! 
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER          , INTENT(IN)    :: code ! 
    !! Name des Unterprogramms
    CHARACTER (LEN=20) , PARAMETER :: c_upname='set_omi_dim_code_0_0' ! 
    !! Z&auml;hler
    INTEGER            :: i, m  ! 
    !! Exponent
    REAL (KIND=Double) :: p ! 
    !! Hilfsfeld
    CHARACTER (LEN=5) :: ch ! 
    !
    CALL new_omi_dim_0 ( this )
    !
    IF ( is_phy_quant_valid( code ) ) THEN
       DO i=1,get_phy_unit_max_si_base( )
          IF ( any_error( ) ) EXIT
          m = i
          p = get_phy_unit_si_base_power ( code, m ) 
          SELECT CASE(i)
          CASE ( 1 )
             CALL set_omi_dim_length_0_0      ( this, p )
          CASE ( 2 )
             CALL set_omi_dim_mass_0_0        ( this, p )
          CASE ( 3 )
             CALL set_omi_dim_time_0_0        ( this, p )
          CASE ( 4 )
             CALL set_omi_dim_electriccur_0_0 ( this, p )
          CASE ( 5 )
             CALL set_omi_dim_temperature_0_0 ( this, p )
          CASE ( 6 )
             CALL set_omi_dim_amountofsub_0_0 ( this, p )
          CASE ( 7 )
             CALL set_omi_dim_luminousint_0_0 ( this, p )
          CASE ( 8 )
             CALL set_omi_dim_currency_0_0    ( this, p )
          CASE DEFAULT
             CALL setup_error_act ( all_errors(:), 8000, c_upname, c_modname )
             WRITE(ch,'(I5)') get_phy_unit_max_si_base( ) ; CALL setup_error_act ( '<nof-b-phy>', ch )
             WRITE(ch,'(I5)') c_nofcomp                   ; CALL setup_error_act ( '<nof-b-omi-dim>', ch )
          END SELECT
       END DO
    END IF
    !
  END SUBROUTINE set_omi_dim_code_0_0
  !
  !! Setze alle Komponenten mehrerer Datenobjekte auf der Basis der 
  !! Code-Kennung einer physikalischen Gr&ouml;&szlig;e <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_omi_dim_code_1_1 ( this, code ) 
    !! Datenobjekte (Vektor) [ falls ein Objekt nicht definiert
    !! werden kann, sind alle Komponenten mit 999 initialisiert ]
    TYPE (t_omi_dim) , INTENT(INOUT) :: this(:) ! 
    !! Code-Kennungen der physikalischen Gr&ouml;&szlig;en
    INTEGER          , INTENT(IN)    :: code(:) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    CALL new_omi_dim_1 ( this(:) )
    !
    DO i=1,MIN(SIZE(this),SIZE(code))
       IF ( any_error( ) ) EXIT
       CALL set_omi_dim_code_0_0 ( this(i), code(i) )
    END DO
    !
  END SUBROUTINE set_omi_dim_code_1_1
  !
  !! Setze alle Komponenten eines Datenobjektes auf der Basis der 
  !! Beschreibung einer physikalischen Gr&ouml;&szlig;e <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_omi_dim_descr_0_0 ( this, descr ) 
    !! Datenobjekt (Skalar) [ falls das Objekt nicht definiert
    !! werden kann, sind alle Komponenten mit 999 initialisiert ]
    TYPE (t_omi_dim)  , INTENT(INOUT) :: this ! 
    !! Beschreibung der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN)    :: descr ! 
    !! Code-Kennung der physikalischen Gr&ouml;&szlig;e
    INTEGER :: code ! 
    !
    code = get_phy_quant_code ( descr      )
    CALL set_omi_dim_code_0_0 ( this, code )
    !
  END SUBROUTINE set_omi_dim_descr_0_0
  !
  !! Setze alle Komponenten mehrerer Datenobjekte auf der Basis der 
  !! Beschreibungen verschiedener physikalischer Gr&ouml;&szlig;en <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_omi_dim_descr_1_1 ( this, descr ) 
    !! Datenobjekte (Vektor) [ falls das Objekt nicht definiert
    !! werden kann, sind alle Komponenten mit 999 initialisiert ]
    TYPE (t_omi_dim)  , INTENT(INOUT) :: this(:) ! 
    !! Beschreibungen der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=*) , INTENT(IN)    :: descr(:) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    CALL new_omi_dim_1 ( this(:) )
    !
    DO i=1,MIN(SIZE(this),SIZE(descr))
       IF ( any_error( ) ) EXIT
       CALL set_omi_dim_descr_0_0 ( this(i), descr(i) )
    END DO
    !
  END SUBROUTINE set_omi_dim_descr_1_1
  !
  !! Setze alle Komponenten eines Datenobjektes auf der Basis einer
  !! Variablenbeschreibung sowie einer Attributliste <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_omi_dim_var_att_0_0_1 ( this, var, att ) 
    !! Datenobjekt (Skalar) [ falls das Objekt nicht definiert
    !! werden kann, sind alle Komponenten mit 999 initialisiert ]
    TYPE (t_omi_dim)  , INTENT(INOUT) :: this     ! 
    !! Variablenbezeichnung der physikalischen Gr&ouml;&szlig;e
    TYPE (t_var)      , INTENT(IN)    :: var      ! 
    !! Attributliste der physikalischen Gr&ouml;&szlig;e
    TYPE (t_att)      , INTENT(IN)    :: att(:)   ! 
    !! Feldgr&ouml;&szlig;e
    INTEGER           , PARAMETER     :: c_max=2  ! 
    !! Id f&uuml;r ("name_id","long_name") in Feld "c_att_name(:)"
    INTEGER           , PARAMETER     :: c_idx(c_max)= (/41,38/) ! 
    !! Id der Variablen, Index
    INTEGER :: i, var_id, idx(c_max), nof ! 
    !! Hilfsfelder zur Aufnahme von Texten/Zahlen
    CHARACTER (LEN=80) , ALLOCATABLE :: ch(:) ! 
    INTEGER            , ALLOCATABLE :: in(:) ! 
    !
    CALL new_omi_dim_0 ( this )
    !
    var_id = get_var_id  ( var )
    DO i=1,c_max
       idx(i) = get_att_idx ( att(:), c_att_name(c_idx(i)), var_id ) 
       IF ( idx(i) > 0 ) THEN
          IF ( is_att_ch ( att(idx(i)) ) ) THEN
             nof = get_att_nof_values ( att(idx(i)) )
             IF ( nof == 1 ) THEN
                ALLOCATE ( ch(nof) )
                ch = get_att_ch( att(idx(i)) )
                CALL set_omi_dim_descr_0_0 ( this, ch(1) )
                IF ( is_phy_quant_flux (ch(1)) ) CALL modify_omi_dim_flux( this, att(:) )
                DEALLOCATE ( ch )
             END IF
          ELSE IF ( is_att_in ( att(idx(i)) ) ) THEN
             nof = get_att_nof_values ( att(idx(i)) )
             IF ( nof == 1 ) THEN
                ALLOCATE ( in(nof) )
                in = get_att_in( att(idx(i)) )
                CALL set_omi_dim_code_0_0 ( this, in(1) )
                IF ( is_phy_quant_flux (in(1)) ) CALL modify_omi_dim_flux( this, att(:) )
                DEALLOCATE ( in )
             END IF
          END IF
       END IF
       IF ( idx(1) > 0 ) EXIT ! preference to "name_id"
    END DO
    !
  END SUBROUTINE set_omi_dim_var_att_0_0_1
  !
  !! Setze alle Komponenten eines Datenobjektes auf der Basis vieler
  !! Variablenbeschreibungen sowie einer Attributliste <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_omi_dim_var_att_1_1_1 ( this, var, att ) 
    !! Datenobjekte (Vektor) [ falls das Objekt nicht definiert
    !! werden kann, sind alle Komponenten mit 999 initialisiert ]
    TYPE (t_omi_dim)  , INTENT(INOUT) :: this(:)  ! 
    !! Variablenbezeichnung der physikalischen Gr&ouml;&szlig;e
    TYPE (t_var)      , INTENT(IN)    :: var(:)   ! 
    !! Attributliste der physikalischen Gr&ouml;&szlig;e
    TYPE (t_att)      , INTENT(IN)    :: att(:)   ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    CALL new_omi_dim_1 ( this(:) )
    !
    DO i=1,MIN(SIZE(this),SIZE(var))
       IF ( any_error( ) ) EXIT
       CALL set_omi_dim_var_att_0_0_1 ( this(i), var(i), att(:) )
    END DO
    !
  END SUBROUTINE set_omi_dim_var_att_1_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! hole die Komponente "length" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_dim_length_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dim) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "length" (Skalar)
    REAL (KIND=Double)             :: val  ! 
    !
    val = this%length
    !
  END FUNCTION get_omi_dim_length_0_0
  !
  !! hole die Komponente "length" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_dim_length_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dim) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "length"
    REAL (KIND=Double) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%length
    !
  END FUNCTION get_omi_dim_length_1_0
  !
  !! hole die Komponente "mass" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_dim_mass_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dim) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "mass" (Skalar)
    REAL (KIND=Double) :: val  ! 
    !
    val = this%mass
    !
  END FUNCTION get_omi_dim_mass_0_0
  !
  !! hole die Komponente "mass" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_dim_mass_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dim) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "mass"
    REAL (KIND=Double) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%mass
    !
  END FUNCTION get_omi_dim_mass_1_0
  !
  !! hole die Komponente "time" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_dim_time_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dim) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "time" (Skalar)
    REAL (KIND=Double) :: val  ! 
    !
    val = this%time
    !
  END FUNCTION get_omi_dim_time_0_0
  !
  !! hole die Komponente "time" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_dim_time_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dim) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "time"
    REAL (KIND=Double) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%time
    !
  END FUNCTION get_omi_dim_time_1_0
  !
  !! hole die Komponente "electriccurrent" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_dim_electriccur_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dim) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "electriccurrent" (Skalar)
    REAL (KIND=Double) :: val  ! 
    !
    val = this%electriccurrent
    !
  END FUNCTION get_omi_dim_electriccur_0_0
  !
  !! hole die Komponente "electriccurrent" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_dim_electriccur_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dim) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "electriccurrent"
    REAL (KIND=Double) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%electriccurrent
    !
  END FUNCTION get_omi_dim_electriccur_1_0
  !
  !! hole die Komponente "temperature" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_dim_temperature_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dim) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "temperature" (Skalar)
    REAL (KIND=Double) :: val  ! 
    !
    val = this%temperature
    !
  END FUNCTION get_omi_dim_temperature_0_0
  !
  !! hole die Komponente "temperature" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_dim_temperature_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dim) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "temperature"
    REAL (KIND=Double) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%temperature
    !
  END FUNCTION get_omi_dim_temperature_1_0
  !
  !! hole die Komponente "amountofsubstance" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_dim_amountofsub_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dim) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "amountofsubstance" (Skalar)
    REAL (KIND=Double) :: val  ! 
    !
    val = this%amountofsubstance
    !
  END FUNCTION get_omi_dim_amountofsub_0_0
  !
  !! hole die Komponente "amountofsubstance" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_dim_amountofsub_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dim) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "amountofsubstance"
    REAL (KIND=Double) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%amountofsubstance
    !
  END FUNCTION get_omi_dim_amountofsub_1_0
  !
  !! hole die Komponente "luminousintensity" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_dim_luminousint_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dim) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "luminousintensity" (Skalar)
    REAL (KIND=Double) :: val  ! 
    !
    val = this%luminousintensity
    !
  END FUNCTION get_omi_dim_luminousint_0_0
  !
  !! hole die Komponente "luminousintensity" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_dim_luminousint_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dim) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "luminousintensity"
    REAL (KIND=Double) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%luminousintensity
    !
  END FUNCTION get_omi_dim_luminousint_1_0
  !
  !! hole die Komponente "currency" aus einem skalaren Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_dim_currency_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dim) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "currency" (Skalar)
    REAL (KIND=Double) :: val  ! 
    !
    val = this%currency
    !
  END FUNCTION get_omi_dim_currency_0_0
  !
  !! hole die Komponente "currency" aus einem vektoriellen Datenobjekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_dim_currency_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_omi_dim) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "currency"
    REAL (KIND=Double) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%currency
    !
  END FUNCTION get_omi_dim_currency_1_0
  !
  !! Ermitteln des Exponenten der ind-ten SI-Basisdimension <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_dim_si_base_power_0 ( this, ind ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_omi_dim) , INTENT(IN) :: this ! 
    !! Index der Basisdimension
    INTEGER          , INTENT(IN) :: ind ! 
    !! Exponent der ind-ten SI-Basisdimension oder undefiniert = 999.0
    REAL (KIND=Double) :: res ! 
    !
    SELECT CASE ( ind )
    CASE(1)
       res = this%length             ! 
    CASE(2)
       res = this%mass               ! 
    CASE(3)
       res = this%time               ! 
    CASE(4)
       res = this%electriccurrent    ! 
    CASE(5)
       res = this%temperature        ! 
    CASE(6)
       res = this%amountofsubstance  ! 
    CASE(7)
       res = this%luminousintensity  ! 
    CASE(8)
       res = this%currency           ! 
    CASE DEFAULT
       res = c_omi_dim_undefined
    END SELECT
    !
  END FUNCTION get_omi_dim_si_base_power_0
  !
  !! Ermitteln der Exponenten der ind-ten SI-Basisdimension <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_dim_si_base_power_1 ( this, ind ) &
       RESULT( res )
    !! Datenobjekte (Vektor)
    TYPE (t_omi_dim) , INTENT(IN) :: this(:) ! 
    !! Index der Basisdimension
    INTEGER          , INTENT(IN) :: ind     ! 
    !! Exponenten der ind-ten SI-Basisdimension oder undefiniert = 999.0
    REAL (KIND=Double) :: res(SIZE(this)) ! 
    ! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(res)
       res(i) = get_omi_dim_si_base_power_0( this(i), ind )
    END DO
    !
  END FUNCTION get_omi_dim_si_base_power_1
  !
  !! Ermittle die Anzahl der SI-Basiseinheiten <BR>
  !! Funktion erzeugt keine Fehlermeldungen
  FUNCTION get_omi_dim_max_si_base_d ( ) &
       RESULT( res )
    !! Ergebnis: Anzahl der SI-Basiseinheiten
    INTEGER :: res ! 
    !
    res = c_nofcomp
    !
  END FUNCTION get_omi_dim_max_si_base_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_dim_0_0 ( this1, this2 ) &
         RESULT( ok )
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_omi_dim) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_dim) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1)  = ( ABS( this1%length            - this2%length            ) <= EPSILON( this1%length            ) )
    l_ok(2)  = ( ABS( this1%mass              - this2%mass              ) <= EPSILON( this1%mass              ) )
    l_ok(3)  = ( ABS( this1%time              - this2%time              ) <= EPSILON( this1%time              ) )
    l_ok(4)  = ( ABS( this1%electriccurrent   - this2%electriccurrent   ) <= EPSILON( this1%electriccurrent   ) )
    l_ok(5)  = ( ABS( this1%temperature       - this2%temperature       ) <= EPSILON( this1%temperature       ) )
    l_ok(6)  = ( ABS( this1%amountofsubstance - this2%amountofsubstance ) <= EPSILON( this1%amountofsubstance ) )
    l_ok(7)  = ( ABS( this1%luminousintensity - this2%luminousintensity ) <= EPSILON( this1%luminousintensity ) )
    l_ok(8)  = ( ABS( this1%currency          - this2%currency          ) <= EPSILON( this1%currency          ) )
    !
    ok = ALL( l_ok )
    !
  END FUNCTION eq_omi_dim_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_dim_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_dim) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_dim) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_dim_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_omi_dim_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_dim_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_dim) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_dim) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_dim_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_omi_dim_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_omi_dim_1_1 ( this1, this2 ) &
         RESULT( ok )
    ! Formalparameter
    !! Objekt 1 (Vektor)
    TYPE (t_omi_dim) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_dim) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_omi_dim_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_omi_dim_1_1
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
  !! Multipliziere zwei Datenobjekte ( Skalar / Skalar ) <BR>
  !! es wird eine Addition der Exponenten durchgef&uuml;hrt, die einer
  !! Multiplikation zweier Datens&auml;tze mit entsprechender Dimension entspricht <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION mu_omi_dim_0_0 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_dim) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_dim) , INTENT(IN) :: this2 ! 
    !! Multiplikationsergebnis (Skalar)
    TYPE (t_omi_dim) :: this ! 
    !
    this%length            = this1%length            + this2%length
    this%mass              = this1%mass              + this2%mass
    this%time              = this1%time              + this2%time
    this%electriccurrent   = this1%electriccurrent   + this2%electriccurrent
    this%temperature       = this1%temperature       + this2%temperature
    this%amountofsubstance = this1%amountofsubstance + this2%amountofsubstance
    this%luminousintensity = this1%luminousintensity + this2%luminousintensity
    this%currency          = this1%currency          + this2%currency
    !
  END FUNCTION mu_omi_dim_0_0
  !
  !! Multipliziere zwei Datenobjekte ( Vektor / Skalar ) <BR>
  !! es wird eine Addition der Exponenten durchgef&uuml;hrt, die einer
  !! Multiplikation zweier Datens&auml;tze mit entsprechender Dimension entspricht <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION mu_omi_dim_1_0 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_dim) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_dim) , INTENT(IN) :: this2    ! 
    !! Multiplikationsergebnis (Vektor)
    TYPE (t_omi_dim) :: this(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = mu_omi_dim_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION mu_omi_dim_1_0
  !
  !! Multipliziere zwei Datenobjekte ( Skalar / Vektor ) <BR>
  !! es wird eine Addition der Exponenten durchgef&uuml;hrt, die einer
  !! Multiplikation zweier Datens&auml;tze mit entsprechender Dimension entspricht <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION mu_omi_dim_0_1 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_dim) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_dim) , INTENT(IN) :: this2(:) ! 
    !! Multiplikationsergebnis (Vektor)
    TYPE (t_omi_dim) :: this(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = mu_omi_dim_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION mu_omi_dim_0_1
  !
  !! Multipliziere zwei Datenobjekte ( Vektor / Vektor ) <BR>
  !! es wird eine Addition der Exponenten durchgef&uuml;hrt, die einer
  !! Multiplikation zweier Datens&auml;tze mit entsprechender Dimension entspricht <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION mu_omi_dim_1_1 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_dim) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_dim) , INTENT(IN) :: this2(:) ! 
    !! Multiplikationsergebnis (Vektor)
    TYPE (t_omi_dim) :: this(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = mu_omi_dim_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION mu_omi_dim_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(/)-Methoden <<< [ERR_NO = 14000 bis 14999]
  ! ----------------------------------------------------------------------
  !
  !! Dividiere zwei Datenobjekte ( Skalar / Skalar ) <BR>
  !! es wird eine Subtraktion der Exponenten durchgef&uuml;hrt, die einer
  !! Division zweier Datens&auml;tze mit entsprechender Dimension entspricht <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION di_omi_dim_0_0 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_dim) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_dim) , INTENT(IN) :: this2 ! 
    !! Divisionsergebnis (Skalar)
    TYPE (t_omi_dim) :: this ! 
    !
    this%length            = this1%length            - this2%length
    this%mass              = this1%mass              - this2%mass
    this%time              = this1%time              - this2%time
    this%electriccurrent   = this1%electriccurrent   - this2%electriccurrent
    this%temperature       = this1%temperature       - this2%temperature
    this%amountofsubstance = this1%amountofsubstance - this2%amountofsubstance
    this%luminousintensity = this1%luminousintensity - this2%luminousintensity
    this%currency          = this1%currency          - this2%currency
    !
  END FUNCTION di_omi_dim_0_0
  !
  !! Dividiere zwei Datenobjekte ( Vektor / Skalar ) <BR>
  !! es wird eine Subtraktion der Exponenten durchgef&uuml;hrt, die einer
  !! Division zweier Datens&auml;tze mit entsprechender Dimension entspricht <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION di_omi_dim_1_0 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_dim) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_dim) , INTENT(IN) :: this2    ! 
    !! Divisionsergebnis (Vektor)
    TYPE (t_omi_dim) :: this(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = di_omi_dim_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION di_omi_dim_1_0
  !
  !! Dividiere zwei Datenobjekte ( Skalar / Vektor ) <BR>
  !! es wird eine Subtraktion der Exponenten durchgef&uuml;hrt, die einer
  !! Division zweier Datens&auml;tze mit entsprechender Dimension entspricht <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION di_omi_dim_0_1 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_dim) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_dim) , INTENT(IN) :: this2(:) ! 
    !! Divisionsergebnis (Vektor)
    TYPE (t_omi_dim) :: this(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = di_omi_dim_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION di_omi_dim_0_1
  !
  !! Dividiere zwei Datenobjekte ( Vektor / Vektor ) <BR>
  !! es wird eine Subtraktion der Exponenten durchgef&uuml;hrt, die einer
  !! Division zweier Datens&auml;tze mit entsprechender Dimension entspricht <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION di_omi_dim_1_1 ( this1, this2 ) &
         RESULT( this )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_dim) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_dim) , INTENT(IN) :: this2(:) ! 
    !! Divisionsergebnis (Vektor)
    TYPE (t_omi_dim) :: this(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       this(i) = di_omi_dim_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION di_omi_dim_1_1
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
  FUNCTION ne_omi_dim_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_dim) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_dim) , INTENT(IN) :: this2 ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = .NOT. eq_omi_dim_0_0( this1, this2 )
    !
  END FUNCTION ne_omi_dim_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_dim_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_dim) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_omi_dim) , INTENT(IN) :: this2    ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = ne_omi_dim_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION ne_omi_dim_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Vektor )<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
 FUNCTION ne_omi_dim_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_omi_dim) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_dim) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = ne_omi_dim_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION ne_omi_dim_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_omi_dim_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_omi_dim) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_omi_dim) , INTENT(IN) :: this2(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = ne_omi_dim_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION ne_omi_dim_1_1
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
       WRITE(*,*) ' *** Warnung *** Modul "b_omi_dim" nicht initialisiert'
       !
       ! Anm: Wiederholung erforderlich, da "all_errors" in dieser
       !      Situation nicht initialisiert wurde
       !
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_omi_dim ausfuehren'
       !
       CALL setup_error_act ( ierr, cerr(:), upname, c_modname )
       !
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
  SUBROUTINE init_omi_dim_all_errors ( )
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
               '--> INIT_omi_dim ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_omi_dim ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_dim"\n'//&
               'Typ-Komponente = "length"\n'//&
               'aktuell        = <length>\n'//&
               'erlaubt        = <min> bis <max>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_dim"\n'//&
               'Typ-Komponente = "mass"\n'//&
               'aktuell        = <mass>\n'//&
               'erlaubt        = <min> bis <max>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_dim"\n'//&
               'Typ-Komponente = "time"\n'//&
               'aktuell        = <time>\n'//&
               'erlaubt        = <min> bis <max>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_dim"\n'//&
               'Typ-Komponente = "electriccurrent"\n'//&
               'aktuell        = <electriccurrent>\n'//&
               'erlaubt        = <min> bis <max>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_dim"\n'//&
               'Typ-Komponente = "temperature"\n'//&
               'aktuell        = <temperature>\n'//&
               'erlaubt        = <min> bis <max>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_dim"\n'//&
               'Typ-Komponente = "amountofsubstance"\n'//&
               'aktuell        = <amountofsubstance>\n'//&
               'erlaubt        = <min> bis <max>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6070 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_dim"\n'//&
               'Typ-Komponente = "luminousintensity"\n'//&
               'aktuell        = <luminousintensity>\n'//&
               'erlaubt        = <min> bis <max>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6080 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_omi_dim"\n'//&
               'Typ-Komponente = "currency"\n'//&
               'aktuell        = <currency>\n'//&
               'erlaubt        = <min> bis <max>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code in Modul "b_omi_dim" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Fusszeilen\n'//&
               '--> Code in Modul "b_omi_dim" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Index des Datenobjektes (1D-Array)\n'//&
               '--> Code in Modul "b_omi_dim" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_dim"\n'//&
               'Typ-Komponente = "length"\n'//&
               '--> Code in Modul "b_omi_dim" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_dim"\n'//&
               'Typ-Komponente = "mass"\n'//&
               '--> Code in Modul "b_omi_dim" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_dim"\n'//&
               'Typ-Komponente = "time"\n'//&
               '--> Code in Modul "b_omi_dim" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_dim"\n'//&
               'Typ-Komponente = "electriccurrent"\n'//&
               '--> Code in Modul "b_omi_dim" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_dim"\n'//&
               'Typ-Komponente = "temperature"\n'//&
               '--> Code in Modul "b_omi_dim" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_dim"\n'//&
               'Typ-Komponente = "amountofsubstance"\n'//&
               '--> Code in Modul "b_omi_dim" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7070 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_dim"\n'//&
               'Typ-Komponente = "luminousintensity"\n'//&
               '--> Code in Modul "b_omi_dim" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7080 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_omi_dim"\n'//&
               'Typ-Komponente = "currency"\n'//&
               '--> Code in Modul "b_omi_dim" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "b_omi_dim"\n'//&
               '--> Code in Modul "b_omi_dim" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: SET-Methoden\n'//&
               'das Modul "b_phy" stellt mehr Basisdimensionen bereit als\n'//&
               'eine Variable des Typs "t_omi_dim" aufnehmen kann\n'//&
               'Anzahl Basisdimensionen "b_phy"     = <nof-b-phy>\n'//&
               'Anzahl Basisdimensionen "b_omi_dim" = <nof-b-omi-dim>\n'//&
               '--> Code in Modul "b_omi_dim" pruefen' )
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
  END SUBROUTINE init_omi_dim_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_omi_dim_all_errors ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_omi_dim_all_errors
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
  !! Pr&uuml;fe, ob die Komponente "length" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_dim_length ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_dim) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='ok_omi_dim_length'        ! 
    !! minimale und maximale Exponenten
    REAL (KIND=Double) , PARAMETER :: c_min=-6.0_Double, c_max=6.0_Double ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch ! 
    !
    ok = ( this%length >= c_min .AND. this%length <= c_max )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
       WRITE(ch,'(F10.5)') this%length ; CALL setup_error_act ( '<length>', ch )
       WRITE(ch,'(F10.5)') c_min       ; CALL setup_error_act ( '<min>', ch )
       WRITE(ch,'(F10.5)') c_max       ; CALL setup_error_act ( '<max>', ch )
    END IF
    !
  END FUNCTION ok_omi_dim_length
  !
  !! Pr&uuml;fe, ob die Komponente "mass" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_dim_mass ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_dim) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_omi_dim_mass'          ! 
    !! minimale und maximale Exponenten
    REAL (KIND=Double) , PARAMETER :: c_min=-2.0_Double, c_max=2.0_Double ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch ! 
    !
    ok = ( this%mass >= c_min .AND. this%mass <= c_max )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
       WRITE(ch,'(F10.5)') this%mass ; CALL setup_error_act ( '<mass>', ch )
       WRITE(ch,'(F10.5)') c_min     ; CALL setup_error_act ( '<min>', ch )
       WRITE(ch,'(F10.5)') c_max     ; CALL setup_error_act ( '<max>', ch )
    END IF
    !
  END FUNCTION ok_omi_dim_mass
  !
  !! Pr&uuml;fe, ob die Komponente "time" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_dim_time ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_dim) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_omi_dim_time'          ! 
    !! minimale und maximale Exponenten
    REAL (KIND=Double) , PARAMETER :: c_min=-4.0_Double, c_max=4.0_Double ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch ! 
    !
    ok = ( this%time >= c_min .AND. this%time <= c_max )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
       WRITE(ch,'(F10.5)') this%time ; CALL setup_error_act ( '<time>', ch )
       WRITE(ch,'(F10.5)') c_min     ; CALL setup_error_act ( '<min>', ch )
       WRITE(ch,'(F10.5)') c_max     ; CALL setup_error_act ( '<max>', ch )
    END IF
    !
  END FUNCTION ok_omi_dim_time
  !
  !! Pr&uuml;fe, ob die Komponente "electriccurrent" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_dim_electriccurrent ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_dim) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=26) , PARAMETER :: c_upname='ok_omi_dim_electriccurrent' ! 
    !! minimale und maximale Exponenten
    REAL (KIND=Double) , PARAMETER :: c_min=-2.0_Double, c_max=2.0_Double   ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch ! 
    !
    ok = ( this%electriccurrent >= c_min .AND. this%electriccurrent <= c_max )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6040, c_upname, c_modname )
       WRITE(ch,'(F10.5)') this%electriccurrent ; CALL setup_error_act ( '<electriccurrent>', ch )
       WRITE(ch,'(F10.5)') c_min                ; CALL setup_error_act ( '<min>', ch )
       WRITE(ch,'(F10.5)') c_max                ; CALL setup_error_act ( '<max>', ch )
    END IF
    !
  END FUNCTION ok_omi_dim_electriccurrent
  !
  !! Pr&uuml;fe, ob die Komponente "temperature" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_dim_temperature ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_dim) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=22) , PARAMETER :: c_upname='ok_omi_dim_temperature'   ! 
    !! minimale und maximale Exponenten
    REAL (KIND=Double) , PARAMETER :: c_min=-2.0_Double, c_max=2.0_Double ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch ! 
    !
    ok = ( this%temperature >= c_min .AND. this%temperature <= c_max )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6050, c_upname, c_modname )
       WRITE(ch,'(F10.5)') this%temperature ; CALL setup_error_act ( '<temperature>', ch )
       WRITE(ch,'(F10.5)') c_min            ; CALL setup_error_act ( '<min>', ch )
       WRITE(ch,'(F10.5)') c_max            ; CALL setup_error_act ( '<max>', ch )
    END IF
    !
  END FUNCTION ok_omi_dim_temperature
  !
  !! Pr&uuml;fe, ob die Komponente "amountofsubstance" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_dim_amountofsubstance ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_dim) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=28) , PARAMETER :: c_upname='ok_omi_dim_amountofsubstance' ! 
    !! minimale und maximale Exponenten
    REAL (KIND=Double) , PARAMETER :: c_min=-2.0_Double, c_max=2.0_Double     ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch ! 
    !
    ok = ( this%amountofsubstance >= c_min .AND. this%amountofsubstance <= c_max )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6060, c_upname, c_modname )
       WRITE(ch,'(F10.5)') this%amountofsubstance ; CALL setup_error_act ( '<amountofsubstance>', ch )
       WRITE(ch,'(F10.5)') c_min                  ; CALL setup_error_act ( '<min>', ch )
       WRITE(ch,'(F10.5)') c_max                  ; CALL setup_error_act ( '<max>', ch )
    END IF
    !
  END FUNCTION ok_omi_dim_amountofsubstance
  !
  !! Pr&uuml;fe, ob die Komponente "luminousintensity" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_dim_luminousintensity ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_dim) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=28) , PARAMETER :: c_upname='ok_omi_dim_luminousintensity' ! 
    !! minimale und maximale Exponenten
    REAL (KIND=Double) , PARAMETER :: c_min=-2.0_Double, c_max=2.0_Double     ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch ! 
    !
    ok = ( this%luminousintensity >= c_min .AND. this%luminousintensity <= c_max )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6070, c_upname, c_modname )
       WRITE(ch,'(F10.5)') this%luminousintensity ; CALL setup_error_act ( '<luminousintensity>', ch )
       WRITE(ch,'(F10.5)') c_min                  ; CALL setup_error_act ( '<min>', ch )
       WRITE(ch,'(F10.5)') c_max                  ; CALL setup_error_act ( '<max>', ch )
    END IF
    !
  END FUNCTION ok_omi_dim_luminousintensity
  !
  !! Pr&uuml;fe, ob die Komponente "currency" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_omi_dim_currency ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_omi_dim) , INTENT(IN) :: this ! 
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='ok_omi_dim_currency' ! 
    !! minimale und maximale Exponenten
    REAL (KIND=Double) , PARAMETER :: c_min=-1.0_Double, c_max=1.0_Double ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch ! 
    !
    ok = ( this%currency >= c_min .AND. this%currency <= c_max )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6080, c_upname, c_modname )
       WRITE(ch,'(F10.5)') this%currency ; CALL setup_error_act ( '<currency>', ch )
       WRITE(ch,'(F10.5)') c_min         ; CALL setup_error_act ( '<min>', ch )
       WRITE(ch,'(F10.5)') c_max         ; CALL setup_error_act ( '<max>', ch )
    END IF
    !
  END FUNCTION ok_omi_dim_currency
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "length" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_dim_length ( this )
    !! Datenobjekt
    TYPE (t_omi_dim) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=20) , PARAMETER :: c_upname='print_omi_dim_length' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) this%length
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente length  - - - - - - - - - - - - - - - ',/&
           '# aktuell = ',F10.5,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_dim_length
  !
  !! Drucke den Inhalt der Komponente "mass" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_dim_mass ( this )
    !! Datenobjekt
    TYPE (t_omi_dim) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_omi_dim_mass' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) this%mass
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente mass  - - - - - - - - - - - - - - - - ',/&
           '# aktuell = ',F10.5,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_dim_mass
  !
  !! Drucke den Inhalt der Komponente "time" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_dim_time ( this )
    !! Datenobjekt
    TYPE (t_omi_dim) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_omi_dim_time' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) this%time
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente time  - - - - - - - - - - - - - - - - ',/&
           '# aktuell = ',F10.5,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_dim_time
  !
  !! Drucke den Inhalt der Komponente "electriccurrent" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_dim_electriccurrent ( this )
    !! Datenobjekt
    TYPE (t_omi_dim) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=29) , PARAMETER :: c_upname='print_omi_dim_electriccurrent' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) this%electriccurrent
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7040, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente electriccurrent - - - - - - - - - - - ',/&
           '# aktuell = ',F10.5,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_dim_electriccurrent
  !
  !! Drucke den Inhalt der Komponente "temperature" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_dim_temperature ( this )
    !! Datenobjekt
    TYPE (t_omi_dim) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=25) , PARAMETER :: c_upname='print_omi_dim_temperature' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) this%temperature
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente temperature - - - - - - - - - - - - - ',/&
           '# aktuell = ',F10.5,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_dim_temperature
  !
  !! Drucke den Inhalt der Komponente "amountofsubstance" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_dim_amountofsubstance ( this )
    !! Datenobjekt
    TYPE (t_omi_dim) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=31) , PARAMETER :: c_upname='print_omi_dim_amountofsubstance' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) this%amountofsubstance
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente amountofsubstance - - - - - - - - - - ',/&
           '# aktuell = ',F10.5,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_dim_amountofsubstance
  !
  !! Drucke den Inhalt der Komponente "luminousintensity" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_dim_luminousintensity ( this )
    !! Datenobjekt
    TYPE (t_omi_dim) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=31) , PARAMETER :: c_upname='print_omi_dim_luminousintensity' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) this%luminousintensity
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7070, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente luminousintensity - - - - - - - - - - ',/&
           '# aktuell = ',F10.5,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_dim_luminousintensity
  !
  !! Drucke den Inhalt der Komponente "currency" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_omi_dim_currency ( this )
    !! Datenobjekt
    TYPE (t_omi_dim) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=22) , PARAMETER :: c_upname='print_omi_dim_currency' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) this%currency
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7080, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente currency  - - - - - - - - - - - - - - ',/&
           '# aktuell = ',F10.5,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_omi_dim_currency
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
  ! >>> PRIVATE-MODIFY-Methoden <<<
  ! ----------------------------------------------------------------------
  !
  !! modifiziere die aktuelle Dimension "this" mit den Angaben in
  !! "var" und "att" insofern es sich um eine Flu&szlig;gr&ouml;&szlig;e
  !! handelt, bei vorgegebener Attributliste; diese Funktion darf nur 
  !! f&uuml;r Flu&szlig;gr&ouml;&szlig;en benutzt werden              <BR>
  !! Subroutine erzeugt keine Fehlermeldungen
  SUBROUTINE modify_omi_dim_flux_0_1 ( this, att ) 
    !! aktuelle Dimension
    TYPE (t_omi_dim)  , INTENT(INOUT) :: this   !
    !! aktuelle Attributliste
    TYPE (t_att)      , INTENT(IN)    :: att(:) ! 
    !! Hilfsvariablen
    TYPE (t_omi_dim) :: l_dim ! 
    !! Konstantwerte
    REAL (KIND=Double) , PARAMETER :: c_n=0.0_Double ! 
    !
    IF ( has_att_cross_sectional_average (att(:)) ) THEN
       CONTINUE
    ELSE
       CALL new_omi_dim                   ( l_dim    )
       CALL set_omi_dim_mass              ( l_dim, c_n )
       CALL set_omi_dim_time              ( l_dim, c_n )
       CALL set_omi_dim_electriccurrent   ( l_dim, c_n )
       CALL set_omi_dim_temperature       ( l_dim, c_n )
       CALL set_omi_dim_amountofsubstance ( l_dim, c_n )
       CALL set_omi_dim_luminousintensity ( l_dim, c_n )
       CALL set_omi_dim_currency          ( l_dim, c_n )
       IF ( get_att_interfaces_count ( att(:) ) > 2 ) THEN
          CALL set_omi_dim_length ( l_dim, -2.0_Double ) ! Division durch m**2
       ELSE
          CALL set_omi_dim_length ( l_dim, -1.0_Double ) ! Division durch m
       END IF
       this = mu_omi_dim ( this, l_dim )
       CALL kill_omi_dim ( l_dim )
    END IF
    !
  END SUBROUTINE modify_omi_dim_flux_0_1
  !
END MODULE b_omi_dim
! TailOfBaseModule --------------------------------------------------------
