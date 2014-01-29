! -------------------------------------------------------------------------
! HeadOfPackageModule -----------------------------------------------------
!
!! <H2>Datenmodul des Paketes "ipds" fuer die Definition phys. Groessen</H2>
!! @author Jens J&uuml;rges
!! @version 3.2 vom 19.03 07, Quellcode: mod_m_ipds_phydef.f90
!! <HR>
!! global phydef data for package "ipds" <BR>
!! <HR>
!
!  Copyright-Hinweis
!
!  Copyright (C) 2005 Bundesanstalt fuer Wasserbau
!
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Paket-Moduls
!
!  1.01 : 2005-08-19 : J. Juerges : Startversion
!  2.01 : 2005-08-22 : J. Juerges : Einheitliche Version 2
!  2.02 : 2005-08-25 : J. Juerges : Bezeichnung aller Anteilsgroessen aufgenommen
!  2.03 : 2007-01-10 : P. Schade  : + Groessen Nr. 14 bis 22 fuer Delft3D
!  3.01 : 2007-03-13 : G. Lang    : neue phys. Groessen u_x, u_y und depo_rate
!  3.02 : 2007-03-19 : G. Lang    : Seegangsparameter [1021,1000,1048,1049]
!
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Packages</H3>
!! <OL>
!!   <LI> Definieren elementarer phydef-Felder f&uuml;r das Paket "ipds";
!!   <LI> Speicherung globaler phydef-Felder f&uuml;r das Paket "ipds"
!!        um den Datenaustausch innerhalb des Paketes zu vereinfachen;
!!   <LI> Ein direkter Zugriff auf diese Daten/Methoden von anderen Paketen 
!!        aus ist nicht zul&auml;ssig.
!! </OL>
!! <HR>
!
MODULE m_ipds_phydef
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  IMPLICIT NONE
  PRIVATE
  !
  ! ---------------------------------------------------------------------
  ! [B] oeffentlich zugaengliche Deklarationen
  ! ---------------------------------------------------------------------
  !
  ! [B.1] Typdefinitionen -----------------------------------------------
  !
  ! [B.2] Konstantwerte (Parameter) -------------------------------------
  ! 
  !! Definition der verwendbaren physikalischen Groessen <BR>
  !! Anzahl Keynamen
  INTEGER           , PUBLIC , PARAMETER :: c_nof_key = 29                    ! 
  !! Schluesselnamen der physikalischen Groessen in einer ipds-Datei
  CHARACTER (LEN=30), PUBLIC , PARAMETER :: c_phy_keyname(c_nof_key) = (/ &   ! 
       'waterlevel                    ', & ! 01
       'salinity                      ', & ! 02
       'temperature                   ', & ! 03
       'sediment_fraction             ', & ! 04
       'porosity                      ', & ! 05
       'ripple_wavenumber             ', & ! 06
       'ripple_height                 ', & ! 07
       'dune_wavenumber               ', & ! 08
       'dune_height                   ', & ! 09
       'rigid_layer_depth             ', & ! 10
       'rigid_layer_roughness         ', & ! 11
       'suspendedload                 ', & ! 12
       'z0_roughnesslength            ', & ! 13
       'bottom_friction_chezy         ', & ! 14
       'bottom_friction_manning_str   ', & ! 15
       'bottom_friction_nikuradse     ', & ! 16
       'bottom_friction_white_colebr  ', & ! 17
       'time_dependent_bathymetry     ', & ! 18
       'critical_stress_for_deposition', & ! 19
       'critical_stress_for_erosion   ', & ! 20
       'erodibility_parameter         ', & ! 21
       'sediment_mass                 ', & ! 22
       'current_velocity_(x-dir.)     ', & ! 23
       'current_velocity_(y-dir.)     ', & ! 24
       'deposition_rate               ', & ! 25
       'mean_wave_period              ', & ! 26 [ 1000 ]
       'significant_wave_height       ', & ! 27 [ 1021 ]
       'mean_wave_direction_(x-dir.)  ', & ! 28 [ 1048 ]
       'mean_wave_direction_(y-dir.)  '/)  ! 29 [ 1049 ]
  !! Indizes aller Keys zu den jeweiligen phys. Groessen
  INTEGER           , PUBLIC , PARAMETER :: c_phy_idx(c_nof_key) = (/ &
       1,  2,  3,  4, 13,  5,  6,  7,  8,  9, &
      10, 11, 12, 14, 15, 16, 17, 18, 19, 20, &
      21, 22, 23, 24, 25, 26, 27, 28, 29 /)
  !! Anzahl physikalischer Groessen
  INTEGER           , PUBLIC , PARAMETER :: c_nof_phy = 29 ! 
  !! Deutsche Bezeichnungen der mit den Keys verbundenen physikalischen Groessen
  CHARACTER (LEN=40), PUBLIC , PARAMETER :: c_phy_name_de(c_nof_phy) = (/ &
       'Wasserstand                             ', & ! 01
       'Salzgehalt                              ', & ! 02
       'Temperatur                              ', & ! 03
       'Sedimentanteil                          ', & ! 04
       'Riffelwellenzahl                        ', & ! 05
       'Riffelhoehe                             ', & ! 06
       'Duenenwellenzahl                        ', & ! 07
       'Duenenhoehe                             ', & ! 08
       'Tiefe der unerodierbaren Schicht        ', & ! 09 
       'Rauheit der unerodierbaren Schicht      ', & ! 10
       'Schwebstoffgehalt                       ', & ! 11 
       'Rauheitslaenge z0                       ', & ! 12
       'Porositaet                              ', & ! 13
       'Bodenreibungskoeffizient (Chezy)        ', & ! 14
       'Bodenreibungskoeffizient (Manning-Str.) ', & ! 15
       'Bodenreibungskoeffizient (Nikuradse)    ', & ! 16
       'Bodenreibungskoeffizient (White-Colebr.)', & ! 17       
       'zeitvariable Topographie                ', & ! 18
       'kritische Spannung (Deposition)         ', & ! 19   
       'kritische Spannung (Erosion)            ', & ! 20  
       'Erodierbarkeit (Parameter)              ', & ! 21  
       'Sedimentmasse                           ', & ! 22
       'Stroemungsgeschwindigkeit (x-R.)        ', & ! 23
       'Stroemungsgeschwindigkeit (y-R.)        ', & ! 24
       'Depositionsrate                         ', & ! 25
       'mittlere Wellenperiode                  ', & ! 26
       'signifikante Wellenhoehe                ', & ! 27
       'mittlere Wellenrichtung (x-R.)          ', & ! 28
       'mittlere Wellenrichtung (y-R.)          ' /) ! 29
  !       
  !! Englische Bezeichnungen der mit den Keys verbundenen physikalischen Groessen
  CHARACTER (LEN=40), PUBLIC , PARAMETER :: c_phy_name_en(c_nof_phy) = (/ &
       'water level                             ', & ! 01
       'salinity                                ', & ! 02
       'temperature                             ', & ! 03
       'sediment fraction                       ', & ! 04
       'ripple wave number                      ', & ! 05
       'ripple height                           ', & ! 06
       'dune wave number                        ', & ! 07
       'dune height                             ', & ! 08
       'rigid layer depth                       ', & ! 09
       'rigid layer roughness                   ', & ! 10
       'suspended load                          ', & ! 11
       'roughness length z0                     ', & ! 12
       'porosity                                ', & ! 13
       'bottom friction (Chezy)                 ', & ! 14
       'bottom friction (Manning-Str.)          ', & ! 15
       'bottom friction (Nikuradse)             ', & ! 16
       'bottom friction (White-Colebr.)         ', & ! 17
       'time dependent bathymetry               ', & ! 18
       'critical stress for deposition          ', & ! 19
       'critical stress for erosion             ', & ! 20
       'erodibility (parameter)                 ', & ! 21
       'sediment mass                           ', & ! 22
       'current velocity (x-dir.)               ', & ! 23
       'current velocity (y-dir.)               ', & ! 24
       'deposition rate                         ', & ! 25
       'mean wave period                        ', & ! 26
       'significant wave height                 ', & ! 27
       'mean wave direction (x-dir.)            ', & ! 28
       'mean wave direction (y-dir.)            ' /) ! 29
  !
  !! maximale Anzahl der Dimensionsbezeichner die zu den vorgenannten Gr&ouml;&szlig;en geh&ouml;hren
  INTEGER           , PUBLIC , PARAMETER :: c_max_dim_per_phy=3 ! 
  !! Indexpositionen der Dimensionen in dem Feld "c_dim_name(:)" aus "mod_b_dim"
  !! xxx zeitabhängige Groessen (z.B. timedep bathymetry: dim aendern?
  INTEGER           , PUBLIC , PARAMETER :: c_dim_name_idx(c_max_dim_per_phy,c_nof_phy) = & ! 
       RESHAPE( (/ &  
       !    1      2      3       4        5       6        7       8      9     10
        1,0,0, 1,0,0, 1,0,0, 11,1,0, 13,18,1, 13,1,0, 12,18,1, 12,1,0, 1,0,0, 1,0,0, &
       !   11     12     13      14       15      16       17      18     19     20
       27,1,0, 1,0,0, 1,0,0,  1,0,0,   1,0,0,  1,0,0,  1,0,0,   1,0,0, 1,0,0, 1,0,0, &
       !   21     22     23      24       25      26      27       28     29
        1,0,0, 1,0,0, 1,0,0,  1,0,0,  27,1,0,  1,0,0,  1,0,0,   1,0,0, 1,0,0   /), &
       SHAPE=(/c_max_dim_per_phy,c_nof_phy/) )
  !
  !! Definition der physikalischen Anteils-Groessen <BR>
  !! Anzahl Anteils-Groessen
  INTEGER           , PUBLIC , PARAMETER :: c_nof_frac = 1
  !! Indexpositionen der Anteils-Groessen in "c_phy_name_de" bzw. "c_phy_name_en"
  INTEGER           , PUBLIC , PARAMETER :: c_idx_phyval_frac(c_nof_frac) = (/ 4 /)
  !
  ! [B.3] Variablen (statische Daten des Moduls)
  !
  ! [B.4] Schnittstellen
  !
  !! Allokieren/Initialisieren der statischen Datenobjekte des Moduls
  INTERFACE index_phy_name
     MODULE PROCEDURE index_phy_name_0
  END INTERFACE
  !
  ! [B.5] oeffentlich zugaengliche Methoden
  !
  PUBLIC :: index_phy_name
  !
  ! ---------------------------------------------------------------------
  ! [C] modulintern zugaengliche Datentypen, Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [C.1] lokale Typdefinitionen
  ! [C.2] Konstantwerte (Parameter)
  !! Name des Moduls
  CHARACTER (LEN=13), PRIVATE, PARAMETER :: c_modname= 'm_ipds_phydef'
  !
  ! [C.3] Schnittstellen
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
  !! Index-Berechnung fuer eine Bezeichnung einer phys. Groesse
  !! Function erzeugt keine Fehlermeldungen
  FUNCTION index_phy_name_0 ( phy_name ) &
       RESULT( idx )
    !! Bezeichnung der phys. Groesse
    CHARACTER (LEN=*) , INTENT(IN) :: phy_name
    !! R&uuml;ckgabewert : Index in c_phy_name_de oder c_phy_name_en (sonst 0)
    INTEGER :: idx
    !
    idx = 0
    DO
       !
       idx = idx + 1
       IF ( idx > c_nof_phy ) EXIT
       !
       ! Die deutsche _oder_ englische Bezeichnung muss stimmen...
       !
       IF ( TRIM(phy_name) == TRIM(c_phy_name_de(idx)) .OR. &
            TRIM(phy_name) == TRIM(c_phy_name_en(idx)) ) EXIT
       !
    END DO
    !
    IF ( idx > c_nof_phy ) idx = 0
    !
  END FUNCTION index_phy_name_0
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
END MODULE m_ipds_phydef
! TailOfPackageModule ------------------------------------------------------
