! -------------------------------------------------------------------------
! HeadOfPackageModule -----------------------------------------------------
!
!! <H2>interpolate physical data</h2>
!! @author J. J&uuml;rges
!! @version 3.1 vom 13.03 07, Quellcode: mod_m_ipds_interpolation.f90
!! <HR>
!  Copyright-Hinweis
!                                                                    <BR>
!  Copyright (C) 2005 <A HREF="http://www.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!                                                                    <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2002-08-09 : J. Juerges : Original
!  01.02 : 2003-01-10 : J. Juerges : Entwicklungsgeschichte nicht fuer f90doc
!  01.03 : 2003-01-10 : J. Juerges : Lizenzangabe durch Platzhalter ersetzt
!  01.04 : 2003-01-10 : J. Juerges : keine Beschreibungen der Interfaces dort, wo sie PUBLIC gemacht werden
!  01.05 : 2003-02-10 : J. Juerges : Fehlerdeklaration fuer Mehrprozessor-Betrieb fit gemacht
!  01.06 : 2003-09-19 : J. Juerges : get_ipol_near_point_sect verarbeitet alle Varianten in einem Durchgang
!                                    und die Berechnung der Messwerte an allen Positionen wird zentral
!                                    erledigt und nicht mehr fuer jeden Punkt p neu durchgefuehrt
!  01.07 : 2003-09-22 : J. Juerges : Mindestabstand zu Messwerten bei Interpolation eingefuehrt
!  01.08 : 2003-09-23 : J. Juerges : Testausgaben auf 99 entfernt
!  02.01 : 2005-08-10 : G. Lang    : Erweiterung fuer Export OpenMI-konformer Daten
!  02.02 : 2005-09-22 : G. Seiss   : Kleine Anpassungen fuer ALTIX in Karlsruhe
!  03.01 : 2007-03-13 : G. Lang    : neue Hauptversionsnummer 3
!
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Packages</H3>
!! <OL>
!!   <LI> Berechnet die Werte von physikalischen Groessen an gegebenen
!!        Orten unter Zuhilfenahme von Werten an Messstationen
!!   <LI> Verschiedene Interpolationsverfahren sind denkbar, z.Z.
!!        ist ein Verfahren integriert, dass die Datenwerte der
!!        Messstationen mittels des Abstandsquadrats gewichtet mittelt.
!!        Zusaetzlich werden Richtungssektoren gebildet, fuer jeden
!!        Sektor wird nur die naechstgelegene Messstation zur Interpolation
!!        herangezogen.
!! </OL>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>&Ouml;ffentliche Methoden</H3>
!!                                                                  <BR>
!! <H4>Basis-Methoden</H4>
!!                                                                  <BR>
!! <UL>
!!    <LI> <EM>GET_interpolation</EM>
!!    <OL>
!!       <LI> Die Messdaten werden in Form von
!!            physikalischen Sets bereitgestellt.
!!    </OL>
!! </UL>
!!                                                                  <BR>
!! <HR>
!! <H3>Verwenden des Moduls</H3>
!!                                                                  <BR>
!! Die Leistungen dieses Moduls werden von "io_ipds_ui" aus
!! in Anspruch genommen. Ein Verwenden der Methoden dieses Moduls
!! von anderen Paketen aus ist nicht zul&auml;ssig. F&uuml;r die
!! korrekte Verwendung der in diesem Modul befindlichen Methoden 
!! ist eine korrekte Initialisierung von "io_ipds_ui"
!! erforderlich.
!!
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Fehlersituationen des Moduls</H3>
!!                                                                    <BR>
!! siehe hierzu die Fehlermeldungen in User Interface "io_ipds_ui" 
!!
MODULE m_ipds_interpolation
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] BASIS-Modul mit globalen Konstantwerten
  !
  USE b_constants, ONLY : &
       !   Parameter
       single,            &
       double,            &
       pi
  !
  ! [A.2] Basis-Modul "Fehler"
  !
  USE b_error, ONLY :   &
       !   Routinen     
       any_error,       &
       setup_error_act
  !
  ! [A.3] Basis-Modul "2D-Punkt"
  !
  USE b_point_2d, ONLY : &
       !   Typdefinition
       t_point_2d,       &
       !   Operatoren
       OPERATOR(.dist.), &
       OPERATOR(.angle.)
  !
  ! ----------------------------------------------------------------------
  ! [B] gemeinsam genutzte Daten des Paketes "io_ipds"
  ! ----------------------------------------------------------------------
  !
  ! [B.1] Daten-Modul des Paketes "io_ipds"
  !
  USE m_ipds_data, ONLY : &
       !   Variablen
       all_errors
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
  !       Hinweis: dieses Modul definiert keinen oeffentlich 
  !                zugaenglichen Datentyp.
  !
  ! [C.2] Konstantwerte (Parameter) [moeglichst nicht verwenden]
  !
  ! [C.3] Variablen [moeglichst nicht verwenden]
  !
  ! [C.4] Schnittstellen
  !
  ! [C.4.1] erforderliche oeffentliche Schnittstellen
  !
  !! Interpolation eines Datenwertes
  INTERFACE get_interpolation
     MODULE PROCEDURE get_phyval_interpolation
  END INTERFACE
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  ! [C.5] Zuweisungen
  !
  ! [C.6] Operatoren (optional, falls sinnvoll)
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  ! [C.7.1] unbedingt erforderliche oeffentliche Methoden
  !
  PUBLIC :: get_interpolation
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Datentypen, Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] lokale Typdefinitionen
  !
  ! [D.2] Konstantwerte (Parameter)
  !
  !! Name des Moduls
  CHARACTER (LEN=20), PARAMETER :: c_modname = 'm_ipds_interpolation'
  !
  !! Definition der implementierten Interpolationsverfahren <BR>
  !! Anzahl der implementierten Interpolationsverfahren
  INTEGER           , PARAMETER :: c_max_ipol_methods = 1
  !! Bezeichnung der Interpolationsverfahren
  CHARACTER (LEN=25), PARAMETER :: c_ipol_method_name(c_max_ipol_methods) = &
       (/ 'nearest_points_in_sectors' /)
  !
  ! [D.3] Variablen (statische Daten des Moduls)
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
  ! Oeffentliche Methoden mit Zugriff ueber PUBLIC Interfaces
  !
  !! Interpolation eines Datenwertes. <BR>
  !! Die Messdaten werden in Form von physikalischen Sets bereitgestellt
  SUBROUTINE get_phyval_interpolation (     &
       p, interpol_name, mespos_maxdist,    &
       nof_mespos, mespos_coor, mespos_val, &
       nof_var, var_ex, val )
    !
    ! Formalparameter
    !! Koordinaten des Interpolationspunktes
    TYPE (t_point_2d) , INTENT(IN) :: p
    !! Bezeichnung des Interpolationsverfahrens
    CHARACTER (LEN=*) , INTENT(IN) :: interpol_name
    !! Max. erlaubter Abstand einer Messstation von p, damit diese
    !! Messstation fuer die Interpolation verwendet wird
    REAL              , INTENT(IN) :: mespos_maxdist
    !
    !! Anzahl Messstationen
    INTEGER           , INTENT(IN) :: nof_mespos
    !! Koordinaten der Messstationen
    ! Achtung: hier musste die explizite Grosse des Feldes entfernt werden
    ! um mit dem ifort-Compiler auf ALTIX zu uebersetzen
    TYPE (t_point_2d) , INTENT(IN) :: mespos_coor( : )
    !! Anzahl Daten-Varianten
    INTEGER          , INTENT(IN)    :: nof_var
    !! Phys. Messwerte aller Varianten aller Messpositionen
    REAL (KIND=Single), INTENT(IN) :: mespos_val(nof_var,nof_mespos)
    !
    !! Kennung der Existenz eines Datenwertes fuer alle Varianten
    LOGICAL          , INTENT(INOUT) :: var_ex(nof_var)
    !! Datenwerte aller Varianten
    REAL             , INTENT(OUT)   :: val(nof_var)
    !
    ! lokale Parameter / Variablen
    !! Name der Routine
    CHARACTER (LEN=24), PARAMETER :: c_upname='get_phyval_interpolation'
    !! Suchstring im Fehlertext
    CHARACTER (LEN=15) :: cs
    !! Ersetzstring im Fehlertext
    CHARACTER (LEN=10) :: cr
    !
    ! Interpolation fuer alle Varianten durchfuehren.
    ! Interpolationsverfahren beachten.
    !
    IF ( ok_ipds_ipol_method_no( interpol_name ) ) THEN
       !
       SELECT CASE ( get_ipds_ipol_method_no( interpol_name ) )
          !
       CASE ( 1 )
          !
          CALL get_ipol_near_point_sect (  &
               p, nof_mespos, mespos_coor, &
               nof_var, mespos_val,        &
               mespos_maxdist,             &
               var_ex, val )
          !
       CASE DEFAULT
          !
          !$OMP critical
          !
          CALL setup_error_act ( all_errors(:), 25001, c_upname, c_modname )
          !
          cs = '<NamIpolMethod>'
          CALL setup_error_act ( cs, TRIM( interpol_name ) )
          !
          cs = '<NumIpolMethod>'
          WRITE ( cr, '(I10)') get_ipds_ipol_method_no( interpol_name )
          CALL setup_error_act ( cs, TRIM( ADJUSTL( cr ) ) )
          !
          cs = '<MaxIpolMethod>'
          WRITE ( cr, '(I10)') c_max_ipol_methods
          CALL setup_error_act ( cs, TRIM( ADJUSTL( cr ) ) )
          !
          !$OMP end critical
          !
       END SELECT ! Hinweis: Anzahl der CASEs muss mit "c_max_ipol_methods" uebereinstimmen
       !
    END IF
    !
  END SUBROUTINE get_phyval_interpolation
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
  ! -------------------------------------------------------------------------
  ! PRIVATE-Methoden ueber implementierte Interpolationsverfahren
  ! -------------------------------------------------------------------------
  !
  !! Interpolation fuer einen Datenwert nach dem Verfahren der
  !! "Naechstgelegenen-Messstation-In-Sektoren" <BR>
  !! <UL><LI> Die Datenwerte der Messstationen werden mittels des Abstandsquadrats
  !! gewichtet mittelt.
  !!     <LI> Zusaetzlich werden Richtungssektoren gebildet. Fuer jeden Sektor
  !! wird nur die naechstgelegene Messstation zur Interpolation herangezogen.
  !!     <LI> Die Sektorgrenzen werden optimiert, so dass die Messstationen sich
  !! auf moeglichst viele Sektoren verteilen und die Abstandssumme minimiert wird.
  SUBROUTINE get_ipol_near_point_sect (              &
       p,                                            &
       nof_mespos, mespos_coor, nof_var, mespos_val, &
       mespos_maxdist,                               &
       var_ex, val )
    !
    ! Formalparameter
    !! Koordinaten des Interpolationspunktes
    TYPE (t_point_2d), INTENT(IN)    :: p
    !
    !! Anzahl Messstationen
    INTEGER,           INTENT(IN)    :: nof_mespos
    !! Koordinaten der Messstationen
    ! Achtung: hier musste die explizite Grosse des Feldes entfernt werden
    ! um mit dem ifort-Compiler auf ALTIX zu uebersetzen
    TYPE (t_point_2d), INTENT(IN)    :: mespos_coor( : )
    !! Anzahl Varianten
    INTEGER,           INTENT(IN)    :: nof_var
    !! Daten der Messstationen
    REAL             , INTENT(IN)    :: mespos_val(  nof_var, nof_mespos )
    !! Max. erlaubter Abstand einer Messstation von p, damit diese
    !! Messstation fuer die Interpolation verwendet wird
    REAL             , INTENT(IN)    :: mespos_maxdist
    !
    !! Kennung der Existenz eines Datenwertes fuer alle Varianten
    LOGICAL          , INTENT(INOUT) :: var_ex(nof_var)
    !! Datenwerte (interpoliert) aller Varianten
    REAL             , INTENT(OUT)   :: val(nof_var)
    !
    ! lokale Parameter / Variablen
    !! Name der Routine
    CHARACTER (LEN=24) , PARAMETER  :: c_upname='get_ipol_near_point_sect'
    !! Abstandswert zur Erkennung identischer Punkte
    REAL (KIND=Double) , PARAMETER  :: c_max_ident_dist=.01
    !! Anzahl Richtungssektoren
    INTEGER            , PARAMETER  :: nof_sector=6
    !! Anzahl Unterteilungen der Richtungssektoren fuer die
    !! Optimierung der Sektorengrenzen
    INTEGER            , PARAMETER  :: nof_subsector=10
    !! Zaehler fuer Messstationen
    INTEGER :: i_mespos
    !! Zaehler fuer Messstationen mit unterschrittenem Grenzabstand
    INTEGER :: i_mespos_near_p
    !! Zaehler fuer Richtungssektoren
    INTEGER :: i_sector
    !! Zaehler fuer Richtungssektor-Unterteilungen
    INTEGER :: i_subsector
    !! Index fuer die optimale Richtungssektor-Unterteilung
    INTEGER :: imem_subsector
    !! Max. erlaubter Abstand einer Messstation von p, damit diese
    !! Messstation fuer die Interpolation verwendet wird
    !! (lokale Variable; doppelt genau)
    REAL (KIND=Double) :: p_max_dist_mespos
    !! Sektor-Winkel
    REAL (KIND=Double) :: sigma
    !! Sektorunterteilungs-Winkel
    REAL (KIND=Double) :: sigma_subsector
    !! Richtungswinkel zwischen Interpolationspunkt und Messstation
    REAL (KIND=Double) :: phi
    !! Richtungswinkel zwischen Interpolationspunkt und Messstation
    !! mit Beruecksichtigung der Sektorunterteilung
    REAL (KIND=Double) :: phi_subsector
    !! Abstand zwischen Interpolationspunkt und Messstation
    REAL (KIND=Double) :: dist
    !! Zeiger auf naechstgelegene Messstation fuer jeden Sektor und jede Unterteilung
    INTEGER            :: idx_near(nof_subsector,nof_sector)
    !! Abstand der naechstgelegenen Messstation fuer jeden Sektor und jede Unterteilung
    REAL (KIND=Double) :: dist_near(nof_subsector,nof_sector)
    !! Anzahl Sektoren mit Messstationen fuer jede Unterteilung
    INTEGER            :: nsd(nof_subsector)
    !! Entfernungssumme aller Sektoren fuer jede Unterteilung
    REAL (KIND=Double) :: dsd(nof_subsector)
    !! Hilfsfeld fuer die Wichtung bei der Interpolation <BR>
    !! Gerundete Abstaende zum Interpolationspunkt fuer jeden Sektor
    INTEGER            :: sqrt_g(nof_sector)
    !! Zaehler fuer die Varianten
    INTEGER            :: i_var
    !! Zaehlersumme
    REAL (KIND=Double) :: zsum
    !! Nennersumme
    REAL (KIND=Double) :: nsum
    !
    ! [1] Initialisierungen
    !
    p_max_dist_mespos = mespos_maxdist
    !
    i_mespos_near_p = 0
    !
    idx_near = -1
    !
    sigma           = 2.*pi / REAL( nof_sector )
    sigma_subsector = sigma / REAL( nof_subsector )
    !
    ! [2] Sektordaten sammeln
    !
    DO i_mespos = 1, nof_mespos
       !
       ! [2.1] Entfernung der Messstation von p beruecksichtigen
       !
       dist = p .dist. mespos_coor( i_mespos )
       !
       IF ( dist <= p_max_dist_mespos ) THEN
          !
          i_mespos_near_p = i_mespos_near_p + 1
          !
          ! [2.2] Richtungswinkel bestimmen
          !
          phi  = 0.0
          !
          IF ( dist > c_max_ident_dist ) phi = p .angle. mespos_coor( i_mespos )
          !
          DO
             IF ( phi >= 0.0 ) EXIT
             phi = phi + 2.*pi
          END DO
          !
          ! [2.3] Sektornummer bestimmen
          !
          DO i_subsector = 1, nof_subsector
             !
             phi_subsector = phi + REAL(i_subsector-1)*sigma_subsector
             !
             i_sector = 1
             DO
                IF ( phi_subsector <= REAL(i_sector)*sigma ) EXIT
                i_sector = i_sector + 1
             END DO
             DO
                IF ( i_sector <= nof_sector ) EXIT
                i_sector = i_sector - nof_sector
             END DO
             !
             ! [2.4] Naechstgelegener Punkt im Sektor?
             !
             IF ( idx_near(i_subsector,i_sector) < 1 ) THEN
                !
                idx_near( i_subsector,i_sector) = i_mespos
                dist_near(i_subsector,i_sector) = dist
                !
             ELSE
                !
                IF ( dist < dist_near(i_subsector,i_sector) ) THEN
                   !
                   idx_near( i_subsector,i_sector) = i_mespos
                   dist_near(i_subsector,i_sector) = dist
                   !
                END IF
                !
             END IF
             !
          END DO
          !
       END IF
       !
    END DO
    !
    ! [3] Optimale Sektorunterteilung ermitteln
    !     --> 1.  Moeglichst viele mit Punkten gefuellte Sektoren
    !     --> 2.  Moeglichst kleine Entfernungssumme bei gleicher Anzahl Sektoren
    !
    IF ( i_mespos_near_p > 0 ) THEN
       !
       ! [3.1] Anzahl und Summen bilden
       !
       DO i_subsector = 1, nof_subsector
          !
          nsd(i_subsector) = COUNT( idx_near(i_subsector,1:nof_sector) > 0 )
          !
          dsd(i_subsector)=0.0
          DO i_sector = 1, nof_sector
             IF ( idx_near(i_subsector,i_sector) > 0 ) &
                  dsd(i_subsector) = dsd(i_subsector) + dist_near(i_subsector,i_sector)
          ENDDO
          !
       ENDDO
       !
       ! [3.2] Optimale Unterteilung erkennen
       !
       imem_subsector = 1
       !
       DO i_subsector = 2, nof_subsector
          !
          IF ( nsd(i_subsector) > nsd(imem_subsector) ) THEN
             imem_subsector = i_subsector
          ELSEIF ( nsd(i_subsector) == nsd(imem_subsector) ) THEN
             IF ( dsd(i_subsector) < dsd(imem_subsector) ) imem_subsector = i_subsector
          END IF
          !
       END DO
       !
    END IF
    !
    ! [4] Entfernungsgewichtete Wertemittelung durchfuehren
    !
    IF ( i_mespos_near_p > 0 ) THEN
       !
       ! [4.1] Interpolationsgewichte ermitteln
       !
       sqrt_g = 1
       !
       DO i_sector = 1, nof_sector
          !
          IF ( idx_near(imem_subsector,i_sector) > 0 ) &
               sqrt_g(i_sector) = NINT(dist_near(imem_subsector,i_sector))
          !
       END DO
       !
       ! [4.2] Interpolation durchfuehren
       !
       IF ( ANY( sqrt_g == 0 ) ) THEN
          !
          ! [4.2.1] Der Abstand zu einer Messstation ist nahe Null
          !
          DO i_sector = 1, nof_sector
             !
             IF ( idx_near( imem_subsector, i_sector ) > 0 ) THEN
                !
                IF ( sqrt_g( i_sector ) == 0 ) &
                     val = mespos_val( :, idx_near( imem_subsector, i_sector ) )
                !
             END IF
             !
          END DO
          !
       ELSE
          !
          ! [4.2.2] Gewichtete Interpolation
          !
          DO i_var = 1, nof_var
             !
             zsum = 0.0
             nsum = 0.0
             !
             DO i_sector = 1, nof_sector
                !
                IF ( idx_near( imem_subsector, i_sector ) > 0 ) THEN
                   !
                   zsum = zsum &
                        + ( mespos_val( i_var, idx_near( imem_subsector, i_sector ) ) &
                        / REAL( sqrt_g( i_sector ) )**2 )
                   !
                   nsum = nsum &
                        + (1. &
                        / REAL( sqrt_g( i_sector ) )**2 )
                   !
                END IF
                !
             END DO
             !
             val(i_var) = zsum / nsum
             !
          END DO
          !
       END IF
       !
       var_ex = .TRUE.
       !
    END IF
    !
  END SUBROUTINE get_ipol_near_point_sect
  ! 
  !! Ermittle f&uuml;r den Namen eines Interpolationsverfahrens
  !! die Verfahrens-Nummer <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipds_ipol_method_no &
       ( interpol_name ) &
       RESULT( i_method )
    !
    ! Formalparameter
    !! Name des Interpolationsverfahrens
    CHARACTER (LEN=*), INTENT(IN) :: interpol_name
    !! Ergebniswert: Nummer des Interpolationsverfahrens <BR>
    !! -1 : Verfahren nicht vorhanden
    INTEGER :: i_method
    !
    ! lokale Parameter / Variablen
    !! Name der Function
    CHARACTER (LEN=23) , PARAMETER :: c_upname='get_ipds_ipol_method_no'
    !! Z&auml;hler
    INTEGER :: i
    !
    i_method = -1
    i        = 0
    !
    DO
       !
       i = i + 1
       IF ( i > c_max_ipol_methods .OR. i_method /= -1 .OR. any_error() ) EXIT
       !
       IF ( TRIM( c_ipol_method_name( i ) ) == TRIM( interpol_name ) ) i_method = i
       !
    END DO
    !
  END FUNCTION get_ipds_ipol_method_no
  !
  !! Pr&uuml;fe ob ein implementiertes Interpolationsverfahren vorliegt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_ipds_ipol_method_no &
       ( interpol_name ) &
       RESULT( ok )
    !
    ! Formalparameter
    !! Name des Interpolationsverfahrens
    CHARACTER (LEN=*), INTENT(IN) :: interpol_name
    !! Ergebniswert: Erforderliches Interpolationsverfahren ist implementiert
    LOGICAL :: ok
    ! Lokale Variablen
    !! Name der Function
    CHARACTER (LEN=22) , PARAMETER :: c_upname='ok_ipds_ipol_method_no'
    !! Suchstring im Fehlertext
    CHARACTER (LEN=15) :: cs
    !! Ersetzstring im Fehlertext
    CHARACTER (LEN=10) :: cr
    !
    ok = ( get_ipds_ipol_method_no( interpol_name ) > 0 )
    !
    IF ( .NOT. ok ) THEN
       !
       !$OMP critical
       !
       CALL setup_error_act ( all_errors(:), 25001, c_upname, c_modname )
       !
       cs = '<NamIpolMethod>'
       CALL setup_error_act ( cs, TRIM( interpol_name ) )
       !
       cs = '<NumIpolMethod>'
       WRITE ( cr, '(I10)') get_ipds_ipol_method_no( interpol_name )
       CALL setup_error_act ( cs, TRIM( ADJUSTL( cr ) ) )
       !
       cs = '<MaxIpolMethod>'
       WRITE ( cr, '(I10)') c_max_ipol_methods
       CALL setup_error_act ( cs, TRIM( ADJUSTL( cr ) ) )
       !
       !$OMP end critical
       !
    END IF
    !
  END FUNCTION ok_ipds_ipol_method_no
  !
END MODULE m_ipds_interpolation
! TailOfPackageModule ------------------------------------------------------
