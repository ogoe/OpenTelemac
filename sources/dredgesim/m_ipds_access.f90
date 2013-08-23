! -------------------------------------------------------------------------
! HeadOfPackageModule -----------------------------------------------------
!
!! <H2>access to physical data</h2>
!! @author J. J&uuml;rges
!! @version 3.1 vom 13.03 07, Quellcode: mod_m_ipds_access.f90
!! <HR>
!! <H3>Copyright-Hinweis</H3>
!!                                                                   <BR>
!! Copyright (C) 2002 <A HREF="http://www.hamburg.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!!                                                                   <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Packages
!  01.01 : 2002-08-09 : J. Juerges : Original
!  01.02 : 2002-09-17 : J. Juerges : get_val_object wird doppelt genau
!  01.03 : 2002-10-17 : J. Juerges : Ortsabh. Aufgaben werden eingespart, wenn der Ort sich gegenueber dem Vorlauf nicht geaendert hat
!  01.04 : 2003-01-10 : J. Juerges : linklose Methodenbeschreibungen geloescht
!  01.05 : 2003-01-10 : J. Juerges : Entwicklungsgeschichte nicht fuer f90doc
!  01.06 : 2003-01-10 : J. Juerges : Lizenzangabe durch Platzhalter ersetzt
!  01.07 : 2003-01-10 : J. Juerges : keine Beschreibungen der Interfaces dort, wo sie PUBLIC gemacht werden
!  01.08 : 2003-02-10 : J. Juerges : Fehlerdeklaration fuer Mehrprozessor-Betrieb fit gemacht
!  01.09 : 2003-02-12 : J. Juerges : Die Regionenzugehoerigkeit eines Punktes wird ausserhalb berechnet /
!                                    ebenso die Default-Werte
!  01.10 : 2003-09-11 : J. Juerges : Die Berechnung der zu den Regionen zugehoerigen Messpositionen
!                                    wird zentral erledigt und nicht mehr fuer jeden Punkt p
!                                    neu durchgefuehrt
!  01.11 : 2003-09-19 : J. Juerges : Die Berechnung der Messwerte an allen Positionen wird zentral
!                                    erledigt und nicht mehr fuer jeden Punkt p neu durchgefuehrt
!  01.12 : 2003-09-22 : J. Juerges : all_var_ex wird an get_interpolation uebergeben
!  01.13 : 2004-02-02 : G. Seiss   : Umstellung von Variablendeklarationen fuer Digital Fortran (Windows)
!  01.14 : 2004-04-13 : J. Juerges : Fuer die phys. Groesse "Porositaet" wird kein Fehler mehr produziert,
!                                    wenn fuer eine Datenposition kein Datenwert ermittelt werden konnte.
!                                    Statt dessen wird ein Default-Wert (0.00) gesetzt.
!  02.01 : 2005-08-10 : G. Lang    : Erweiterung fuer Export OpenMI-konformer Daten
!  02.02 : 2005-08-25 : J. Juerges : Normalisierungsberechnung in Modul m_ipds_phyval ausgelagert
!  02.03 : 2005-09-22 : G. Seiss   : Kleine Anpassungen fuer ALTIX in Karlsruhe
!  03.01 : 2007-03-13 : G. Lang    : neue Hauptversionsnummer 3
!
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Packages</H3>
!! <OL>
!!   <LI> Holt Datenwerte einer physikalischer Groesse
!!        fuer einen Punkt mit Tiefenwert
!! </OL>
!! <HR>
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
!!                                                                  <BR>
!! siehe hierzu die Fehlermeldungen in User Interface "io_ipds_ui" 
!!
MODULE m_ipds_access
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] BASIS-Modul mit globalen Konstantwerten
  USE b_constants, ONLY : &
       ! Parameter
       single,            &
       double
  !
  ! [A.2] Basis-Modul "Fehler"
  !
  USE b_error, ONLY :   &
       !   Routinen     
       no_error,        &
       any_error,       &
       setup_error_act
  !
  ! [A.3] Basis-Modul fuer 2D-Koordinaten
  !
  USE b_point_2d, ONLY : &
       !   Typdefinitionen
       t_point_2d,       &
       !   Routinen / Interfaces
       get_point_2d_x,   &
       get_point_2d_y
  !
  ! ----------------------------------------------------------------------
  ! [B] gemeinsam genutzte Daten des Paketes "io_ipds"
  ! ----------------------------------------------------------------------
  !
  ! [B.1] Daten-Modul des Paketes "io_ipds"
  !
  USE m_ipds_data, ONLY :        &
       !   Typdefinition
       t_ipds,                   &
       !   Variablen
       all_errors,               &
       !   Routinen / Interfaces
       associated_regphyset_object
  !
  ! [B.2] PACKAGE-Modul fuer phys. Groessen und Regionen
  !
  USE m_ipds_regphyset, ONLY :      &
       !   Daten
       c_len_regphyset_interpol_name,   &
       !   Routinen / Interfaces
       get_regphyset_interpol_name, &
       get_regphyset_mespos_maxdist
  !
  ! [B.3] Daten einer physikalischen Groesse
  !
  USE m_ipds_phyval, ONLY : &
       !   Daten
       c_len_phyval_name,   &
       !   Routinen
       is_phyval_frac,      &
       normalize_phyval
  !
  ! [B.4] PACKAGE-Modul fuer Interpolationen
  !
  USE m_ipds_interpolation, ONLY : &
       !   Routinen / Interfaces
       get_interpolation
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
  !! Hole Datenwerte einer physikalischer Groesse fuer einen Punkt mit Tiefenwert
  INTERFACE get_val_object
     MODULE PROCEDURE get_val_object_0
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
  PUBLIC :: get_val_object
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
  CHARACTER (LEN=13), PARAMETER :: c_modname = 'm_ipds_access'
  !
  ! [D.3] Variablen (statische Daten des Moduls)
  !
  ! [D.4] Schnittstellen
  !
  !! Aktualisieren einer Varianten-Datenliste mit den regional abweichenden Daten eines Objekts
  INTERFACE update_region_val_object
     MODULE PROCEDURE update_region_val_object_0
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
  ! Oeffentliche Methoden mit Zugriff ueber PUBLIC Interfaces
  !
  !! Hole Datenwerte einer physikalischer Groesse fuer einen Punkt mit Tiefenwert <BR>
  !! Subroutine erzeugt Fehlermeldung
  SUBROUTINE get_val_object_0 ( &
       this, p, z, phyval_name, &
       nof_regphyset, p_in_region, &
       max_nof_mespos_phyval_dim, nof_mespos_phyval, mespos_coor, mespos_val, &
       nof_all_var, all_var_name, all_var_ex, all_val, &
       val, var_name )
    !
    ! Formalparameter
    !! aktuelles Objekt
    TYPE (t_ipds)     , POINTER       :: this
    !! Koordinaten der Datenposition
    TYPE (t_point_2d) , INTENT(IN)    :: p
    !! Tiefe der Datenposition
    REAL (KIND=Double), INTENT(IN)    :: z
    !! Bezeichnung der physikalischen Groesse
    CHARACTER (LEN=*) , INTENT(IN)    :: phyval_name
    !! Anzahl der Regionen, fuer die ermittelt worden ist, ob der Punkt innerhalb
    !! der Region liegt
    INTEGER           , INTENT(IN)    :: nof_regphyset
    !! Punkt liegt innerhalb der unterschiedlichen Regionen?
    LOGICAL           , INTENT(IN)    :: p_in_region(nof_regphyset)
    !! Max. Anzahl Mespositionen (nur fuer Dimensionierungen, also >= 1)
    INTEGER           , INTENT(IN)    :: max_nof_mespos_phyval_dim
    !! Anzahl der zu einem phys. Satz zugehoerigen Messpositionen
    INTEGER           , INTENT(IN)    :: nof_mespos_phyval(nof_regphyset)
    !! Koordinaten aller zu einem phys. Satz zugehoerigen Messpositionen
    ! mespos_coor(max_nof_mespos_phyval_dim,nof_regphyset)
    ! Achtung: hier musste die explizite Grosse des Feldes entfernt werden
    ! um mit dem ifort-Compiler auf ALTIX zu uebersetzen
    TYPE (t_point_2d) , INTENT(IN)    :: mespos_coor( :, :)
    !! Anzahl _aller_ Variationen der physikalischen Groesse
    INTEGER           , INTENT(IN)    :: nof_all_var
    !! Phys. Messwerte aller Varianten aller zu einem phys. Satz zugehoerigen Messpositionen
    REAL (KIND=Single), INTENT(IN)    :: mespos_val(nof_all_var,max_nof_mespos_phyval_dim,nof_regphyset)
    !! Bezeichnung _aller_ Varianten der physikalischen Groesse
    CHARACTER (LEN=c_len_phyval_name), INTENT(IN)    :: all_var_name(nof_all_var)
    !! Kennung der Existenz eines Datenwertes fuer _alle_ Varianten der physikalischen Groesse <BR>
    !! IN: Existenz eines Default-Datenwertes <BR>
    !! OUT: Existenz eines Datenwertes mit Beruecksichtigung der Regionen und Fraktionen
    LOGICAL           , INTENT(INOUT) :: all_var_ex(nof_all_var)
    !! Datenwerte _aller_ Varianten der physikalischen Groesse <BR>
    !! IN: Default-Datenwerte <BR>
    !! OUT: Datenwerte mit Beruecksichtigung der Regionen und Fraktionen
    REAL              , INTENT(INOUT) :: all_val(nof_all_var)
    !! Datenwerte aller Varianten der physikalischen Groesse, fuer die Daten ermittelt
    !! werden konnten <BR>
    !! Feld wird hier mit notwendigem Speicher allokiert
    REAL              , POINTER       :: val(:)
    !! Bezeichnung aller Varianten der physikalischen Groesse, fuer die Daten ermittelt
    !! werden konnten <BR>
    !! Feld wird hier mit notwendigem Speicher allokiert
    CHARACTER (LEN=c_len_phyval_name), POINTER       :: var_name(:)
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER :: c_upname='get_val_object_0' 
    !! Anzahl Variationen mit Daten
    INTEGER            :: nof_var
    !! Zaehler fuer alle Variationen
    INTEGER            :: i_all_var
    !! Zaehler fuer Variationen mit Daten
    INTEGER            :: i_var
    !! Statusvariable
    INTEGER            :: stat
    !! Ohne Fehler?
    LOGICAL            :: ok
    !! Suchstring im Fehlertext
    CHARACTER (LEN=10) :: cs
    !! Ersetzstring im Fehlertext
    CHARACTER (LEN=15) :: cr
    !
    ! [1] Initialisierungen
    !
    ok = .FALSE.
    !
    NULLIFY ( val )
    NULLIFY ( var_name )
    !
    ! [2] Regional abweichende Werte aller Varianten beruecksichtigen
    !
    IF ( no_error() ) THEN
       !
       CALL update_region_val_object ( &
            this, p, &
            nof_regphyset, p_in_region, &
            max_nof_mespos_phyval_dim, nof_mespos_phyval, mespos_coor, mespos_val, &
            nof_all_var, all_var_ex, all_val )
       !
    END IF
    !
    ! [3] Normalisieren der Variantendaten, so dass die Summe der Variantendaten 1 ergibt
    !
    IF ( no_error() ) THEN
       !
       IF ( is_phyval_frac( phyval_name ) ) CALL normalize_phyval &
            ( nof_all_var, all_var_ex, all_val, SUM=1. )
       !
    END IF
    !
    ! [4] Uebertragen der Variantendaten
    !
    DO
       !
       IF ( any_error() ) EXIT
       !
       IF ( TRIM(phyval_name) == 'Porositaet' ) THEN
          !
          nof_var = nof_all_var
          !
       ELSE
          !
          nof_var = COUNT( all_var_ex )
          !
       END IF
       !
       IF ( nof_var > 0 ) THEN
          !
          ALLOCATE ( &
               val(      nof_var ), &
               var_name( nof_var ), &
               STAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act( all_errors(:), 21040, c_upname, c_modname, stat )
          IF ( any_error() ) EXIT
          !
          i_var = 0
          !
          DO i_all_var = 1, nof_all_var
             !
             IF ( all_var_ex( i_all_var ) ) THEN
                !
                i_var = i_var + 1
                !
                val(      i_var ) = all_val(      i_all_var )
                var_name( i_var ) = all_var_name( i_all_var )
                !
             ELSE IF ( TRIM(phyval_name) == 'Porositaet' ) THEN
                !
                i_var = i_var + 1
                !
                val(      i_var ) = 0.00
                var_name( i_var ) = all_var_name( i_all_var )
                !
             END IF
             !
          END DO
          !
          IF ( i_var == nof_var ) ok = .TRUE.
          !
       END IF
       !
       EXIT
       !
    END DO
    !
    ! [5] Fehlermeldung zusammenstellen
    !
    IF( .NOT. ok ) THEN
       !
       !$OMP critical
       !
       CALL setup_error_act ( all_errors(:), 21000, c_upname, c_modname )
       !
       cs = '<xxxxxxxx>'
       WRITE( cr, '(F15.5)') get_point_2d_x( p )
       CALL setup_error_act ( cs, cr )
       !
       cs = '<yyyyyyyy>'
       WRITE( cr, '(F15.5)') get_point_2d_y( p )
       CALL setup_error_act ( cs, cr )
       !
       cs = '<zzzzzzzz>'
       WRITE( cr, '(F15.5)') z
       CALL setup_error_act ( cs, cr )
       !
       cs = '<phy_name>'
       CALL setup_error_act ( cs, TRIM( phyval_name ) )
       !
       !$OMP end critical
       !
    END IF
    !
  END SUBROUTINE get_val_object_0
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
  !! Aktualisieren einer Varianten-Datenliste mit den regional abweichenden Daten
  !! eines Objekts
  SUBROUTINE update_region_val_object_0                                 ( &
       this, p,                                                           &
       nof_regphyset, p_in_region,                                        &
       max_nof_mespos_phyval_dim, nof_mespos_phyval, mespos_coor, mespos_val, &
       nof_all_var, all_var_ex, all_val )
    !
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_ipds),      POINTER       :: this
    !! Koordinaten der Datenposition
    TYPE (t_point_2d) , INTENT(IN)    :: p
    !! Anzahl der Regionen, fuer die ermittelt worden ist, ob der Punkt innerhalb
    !! der Region liegt
    INTEGER           , INTENT(IN)    :: nof_regphyset
    !! Punkt liegt innerhalb der unterschiedlichen Regionen?
    LOGICAL           , INTENT(IN)    :: p_in_region(nof_regphyset)
    !! Max. Anzahl Mespositionen (nur fuer Dimensionierungen, also >= 1)
    INTEGER           , INTENT(IN)    :: max_nof_mespos_phyval_dim
    !! Anzahl der zu einem phys. Satz zugehoerigen Messpositionen
    INTEGER           , INTENT(IN)    :: nof_mespos_phyval(nof_regphyset)
    !! Koordinaten aller zu einem phys. Satz zugehoerigen Messpositionen
    ! mespos_coor(max_nof_mespos_phyval_dim,nof_regphyset)
    ! Achtung: hier musste die explizite Grosse des Feldes entfernt werden
    ! um mit dem ifort-Compiler auf ALTIX zu uebersetzen
    TYPE (t_point_2d) , INTENT(IN)    :: mespos_coor( :, :)
    !! Anzahl Varianten
    INTEGER           , INTENT(IN)    :: nof_all_var
    !! Phys. Messwerte aller Varianten aller zu einem phys. Satz zugehoerigen Messpositionen
    REAL (KIND=Single), INTENT(IN)    :: mespos_val(nof_all_var,max_nof_mespos_phyval_dim,nof_regphyset)
    !! Kennung der Existenz eines Datenwertes fuer alle Varianten
    LOGICAL           , INTENT(INOUT) :: all_var_ex(nof_all_var)
    !! Datenwerte aller Varianten
    REAL              , INTENT(INOUT) :: all_val(nof_all_var)
    !
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=26), PARAMETER   :: c_upname='update_region_val_object_0'
    !! Zaehler fuer Komponente "regphyset"
    INTEGER                         :: i_regphyset
    !! Bezeichnung des zu verwendenden Interpolationsverfahrens
    CHARACTER (LEN=c_len_regphyset_interpol_name) :: interpol_name
    !! Max. Abstand einer Messstation von einem Punkt p
    REAL                            :: mespos_maxdist
    !
    ! Alle regphyset durchlaufen
    !
    IF ( associated_regphyset_object( this ) ) THEN
       !
       DO i_regphyset = 1, SIZE( this%regphyset )
          !
          IF ( p_in_region( i_regphyset ) ) THEN
             !
             IF ( nof_mespos_phyval(i_regphyset) > 0 ) THEN
                !
                ! Interpolation durchfuehren
                !
                interpol_name  = get_regphyset_interpol_name( this%regphyset( i_regphyset ) )
                !
                mespos_maxdist = get_regphyset_mespos_maxdist( this%regphyset( i_regphyset ) )
                !
                CALL get_interpolation                                          ( &
                     p, interpol_name, mespos_maxdist,                            &
                     nof_mespos_phyval(i_regphyset),                              &
                     mespos_coor(1:nof_mespos_phyval(i_regphyset),i_regphyset),   &
                     mespos_val(:,1:nof_mespos_phyval(i_regphyset),i_regphyset),  &
                     nof_all_var, all_var_ex, all_val )
                !
             END IF
             !
          END IF
          !
       END DO
       !
    END IF
    !
  END SUBROUTINE update_region_val_object_0
  !
END MODULE m_ipds_access
! TailOfPackageModule ------------------------------------------------------
