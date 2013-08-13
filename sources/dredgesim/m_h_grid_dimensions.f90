! -------------------------------------------------------------------------
! HeadOfPackageModule -----------------------------------------------------
!
!! <H2>Ermitteln der Gr&ouml;szlig;e verschiedener (klassischer) Dimensionen</H2>
!! @author <A HREF="mailto:lang@hamburg.baw.de">G. Lang</A>
!! @version 4.1 vom 01/12/07, Quellcode: mod_m_h_grid_dimensions.f90
!! <HR>
!! service routines to determine classic dimensions used in different programs<BR>
!! <HR>
!!! <H3>Copyright-Hinweis</H3>
!!                                                                   <BR>
!! Copyright (C) 2007 <A HREF="http://www.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Paketes
!  01.01 : 2007-01-12 : G. Lang    : Startversion
!  04.01 : 2007-01-12 : G. Lang    : auf allgemein gueltige Hauptrevisionsnummer 4 gebracht
!                                            
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! <OL>
!!   <LI> Definition der Fehlermeldungen des Paketes "h_grid".
!! </OL>
!! <HR>
!
MODULE m_h_grid_dimensions
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] Basis-Modul mit globalen Konstantwerten [ggf. entfernen]
  USE b_constants, ONLY :   &
       Double,              &
       Short
  !
  ! [A.2] Basis-Modul mit Typ+Methoden "Fehlerbehandlung"
  USE b_error, ONLY :       &
       ! Routinen / Interfaces
       no_error,            &
       any_error,           &
       setup_error_act 
  !
  ! [A.3] Basis-Modul mit Typ+Methoden "Dateihandling"
  USE b_file, ONLY :        &
       ! Datentyp
       t_file,              &
       ! Routinen / Interfaces
       get_file_path, &
       get_file_name
  !
  ! ---------------------------------------------------------------------
  ! [B]  Module des Paketes "SediMorph"
  ! ---------------------------------------------------------------------
  !
  ! [B.1] Modul mit globalen Daten
  USE m_h_grid_data, ONLY :   &
       ! Datentyp
       t_h_grid,              &
       t_h_grid_list,         &
       ! Daten
       prn_op, prn_lun,       &
       all_errors,            &
       work_object,           &
       first_list_object,     &
       ! Interfaces / Methoden
       get_h_grid_variant_no, &
       get_nv_object, get_ne_object, get_ks_object, get_nrand_object
  !
  IMPLICIT NONE
  PRIVATE
  !
  ! ---------------------------------------------------------------------
  ! [C] oeffentlich zugaengliche Deklarationen (mit PUBLIC-Attribut)
  ! ---------------------------------------------------------------------
  !
  ! [C.1] Schnittstellen
  !
  !! Ermittle verschiedene (klassische) Dimensionen f&uuml;r die <BR>
  !! aktuell in dem Datenmodul abgelegten Datens&auml;tze        <BR>
  !! a) f&uuml;r einen klassischen Dimensionsbezeichner          <BR>
  !! b) jeweils f&uuml;r mehrere klassische Dimensionsbezeichner
  INTERFACE get_max_old_dimensions
     MODULE PROCEDURE get_max_old_dimensions_0
     MODULE PROCEDURE get_max_old_dimensions_1
  END INTERFACE
  !
  ! [C.2] Liste der oeffentlichen Methoden  
  PUBLIC :: get_max_old_dimensions ! 
  !
  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Datentypen, Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] lokale Typdefinitionen
  ! [D.2] Schnittstellen
  !
  !! Ermittle den maximalen Wert f&uuml;r Komponente "ne" in allen Datenobjekten
  INTERFACE get_max_dimensions_ne
     MODULE PROCEDURE get_max_dimensions_ne_d
  END INTERFACE
  !! Ermittle den maximalen Wert f&uuml;r Komponente "nv" in allen Datenobjekten
  INTERFACE get_max_dimensions_nv
     MODULE PROCEDURE get_max_dimensions_nv_d
  END INTERFACE
  !! Ermittle den maximalen Wert f&uuml;r Komponente "nrand" in allen Datenobjekten (falls verf&uuml;gbar)
  INTERFACE get_max_dimensions_nrand
     MODULE PROCEDURE get_max_dimensions_nrand_d
  END INTERFACE
  !! Ermittle den maximalen Wert f&uuml;r (nicht vorhandene) Komponente "ninnen" in allen Datenobjekten (falls verf&uuml;gbar)
  INTERFACE get_max_dimensions_ninnen
     MODULE PROCEDURE get_max_dimensions_ninnen_d
  END INTERFACE
  !! Ermittle den maximalen Wert f&uuml;r (nicht vorhandene) Komponente "maxele" in allen Datenobjekten
  INTERFACE get_max_dimensions_maxele
     MODULE PROCEDURE get_max_dimensions_maxele_d
  END INTERFACE
  !
  ! [D.3] Konstantwerte (Parameter)
  !! Name des Moduls
  CHARACTER (LEN=19)    , PRIVATE, PARAMETER :: c_modname='m_h_grid_dimensions'   ! 
  !! max. Anzahl unterschiedlicher (klassischer) Dimensionsbezeichner
  INTEGER               , PRIVATE, PARAMETER :: c_max_oldname=5                   ! 
  !! Namen der (klassischen) Dimensionsbezeichner
  CHARACTER (LEN=10)    , PRIVATE, PARAMETER :: c_oldname(c_max_oldname)= &       ! 
       (/ 'MAXELE    ', 'MAXKNO    ', 'MAXINF    ', 'NINNEN    ', 'NRAND     ' /) ! 01-05
  ! [D.4] Variablen die haeufig verwendet werden
  TYPE (t_h_grid_list) , PRIVATE, POINTER   :: act_list_object                   ! 
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
  ! Oeffentliche Methoden oder Methoden mit Zugriff ueber PUBLIC Interfaces
  !
  ! ----------------------------------------------------------------------
  ! aktuelle Dimensionen, die zu bestimmten klass. Dimensionsbezeichnungen
  ! gehoeren
  ! ----------------------------------------------------------------------
  !
  !! Ermitteln einer (klassischen) Fortran-Dimension f&uuml;r einen
  !! Dimensionsbezeichner in allen Datenobjekten <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_max_old_dimensions_0 ( name ) &
       RESULT( res )
    !! Name eines g&uuml;ltigen klassischen Dimensionsbezeichners                  <BR>     
    !! MAXELE  : max. Anzahl der Dreieckselemente                                  <BR>
    !!           (Vierecke werden als zwei Dreiecke gez&auml;hlt)                  <BR>
    !! MAXKNO  : max. Anzahl der Knoten des Gitters                                <BR>
    !! MAXINF  : max. Anzahl der Knoten/Kanten im Dreieck                          <BR>
    !! NINNEN  : max. Anzahl der Innenknoten eines Gitters (soweit verf&uuml;gbar) <BR>
    !! NRAND   : max. Anzahl der Randknoten eines Gitters (soweit verf&uuml;gbar)        
    CHARACTER (LEN=*) , INTENT(IN) :: name ! 
    !! aktueller Wert der angeforderten Dimension "name" <BR>
    !! falls -9, so konnte der aktuelle Wert nicht ermittelt werden
    INTEGER                        :: res ! 
    !
    !! Name der Funktion
    CHARACTER (LEN=24) , PARAMETER :: c_upname='get_max_old_dimensions_0'     ! 
    !
    SELECT CASE ( name )
    CASE ( c_oldname( 1) ) ! MAXELE
       res = MAX(1,get_max_dimensions_maxele( )) ! Vierecke werden als zwei Dreiecke gez&auml;hlt
    CASE ( c_oldname( 2) ) ! MAXKNO
       res = MAX(1,get_max_dimensions_nv( ))
    CASE ( c_oldname( 3) ) ! MAXINF
       res = 3  ! da bei klassischen Dimensionen immer in Dreiecken gerechnet wird
    CASE ( c_oldname( 4) ) ! NINNEN
       res = MAX(1,get_max_dimensions_ninnen( )) ! abgeleitet aus nv und nrand (falls vorhanden)
    CASE ( c_oldname( 5) ) ! NRAND
       res = MAX(1,get_max_dimensions_nrand( ))
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), 9200, c_upname, c_modname )
       CALL setup_error_act ( '<AktDimName>', name ) 
       res = -9
    END SELECT
    !
    IF ( prn_op ) WRITE(prn_lun,9000) c_upname, TRIM(name), res
    !
9000 FORMAT(&
          '# automatisches Ableiten klassischer Dimensionen ------------------------ ',/&
          '# Subroutine                  = ',A,/&
          '# klass. Dimensionsbezeichner = ',A,/&
          '# aktueller Wert              = ',I10,/&
          '# ----------------------------------------------------------------------- ')
    !
  END FUNCTION get_max_old_dimensions_0
  !
  !! Ermitteln mehrerer (klassischer) Fortran-Dimensionen f&uuml;r 
  !! mehrere Dimensionsbezeichner in allen Datenobjekten <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_max_old_dimensions_1 ( name ) &
       RESULT( res )
    !! Namen g&uuml;ltiger klassischer Dimensionsbezeichner <BR>
    !! siehe Function "get_max_old_dimensions_0"
    CHARACTER (LEN=*) , INTENT(IN) :: name(:) ! 
    !! aktuelle Werte der angeforderten Dimensionen "name" <BR>
    !! falls -9, so konnte der aktuelle Wert nicht ermittelt werden
    INTEGER                        :: res(SIZE(name)) ! 
    !
    !! Name der Funktion
    CHARACTER (LEN=24) , PARAMETER :: c_upname='get_max_old_dimensions_1' ! 
    !! Z&auml;hlervariable
    INTEGER                        :: i ! 
    !
    DO i=1,SIZE(name)
       res(i) = get_max_old_dimensions_0 ( name(i) )
    END DO
    !
  END FUNCTION get_max_old_dimensions_1
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
  !! Ermittle den maximalen Wert f&uuml;r Komponente "ne" in allen Datenobjekten
  FUNCTION get_max_dimensions_ne_d ( ) &
       RESULT( res )
    !! aktueller Wert der maximalen Dimension "ne" (alle Datenobjekte) <BR>
    !! -9  == Dimension konnte nicht ermittelt werden
    INTEGER           :: res   ! 
    ! lokale Hilfsvariablen
    INTEGER , POINTER :: p_ne  ! 
    !
    res = -9
    act_list_object => first_list_object
    !
    DO
       IF ( .NOT. ASSOCIATED( act_list_object%object ) ) EXIT
       p_ne => get_ne_object ( act_list_object%object )
       IF ( ASSOCIATED( p_ne ) ) res = MAX( res, p_ne )
       NULLIFY( p_ne )
       IF ( .NOT. ASSOCIATED( act_list_object%next ) ) EXIT
       act_list_object => act_list_object%next
    END DO
    !
  END FUNCTION get_max_dimensions_ne_d
  !
  !! Ermittle den maximalen Wert f&uuml;r Komponente "nv" in allen Datenobjekten
  FUNCTION get_max_dimensions_nv_d ( ) &
       RESULT( res )
    !! aktueller Wert der maximalen Dimension "nv" (alle Datenobjekte) <BR>
    !! -9  == Dimension konnte nicht ermittelt werden
    INTEGER           :: res   ! 
    ! lokale Hilfsvariablen
    INTEGER , POINTER :: p_nv  ! 
    !
    res = -9
    act_list_object => first_list_object
    !
    DO
       IF ( .NOT. ASSOCIATED( act_list_object%object ) ) EXIT
       p_nv => get_nv_object ( act_list_object%object )
       IF ( ASSOCIATED( p_nv ) ) res = MAX( res, p_nv )
       NULLIFY( p_nv )
       IF ( .NOT. ASSOCIATED( act_list_object%next ) ) EXIT
       act_list_object => act_list_object%next
    END DO
    !
  END FUNCTION get_max_dimensions_nv_d
  !
  !! Ermittle den maximalen Wert f&uuml;r Komponente "nrand" in allen Datenobjekten
  FUNCTION get_max_dimensions_nrand_d ( ) &
       RESULT( res )
    !! aktueller Wert der maximalen Dimension "nrand" (alle Datenobjekte) <BR>
    !! -9  == Dimension konnte nicht ermittelt werden
    INTEGER           :: res   ! 
    ! lokale Hilfsvariablen
    INTEGER , POINTER :: p_nrand  ! 
    !
    res = -9
    act_list_object => first_list_object
    !
    DO
       IF ( .NOT. ASSOCIATED( act_list_object%object ) ) EXIT
       p_nrand => get_nrand_object ( act_list_object%object )
       IF ( ASSOCIATED( p_nrand ) ) res = MAX( res, p_nrand )
       NULLIFY( p_nrand )
       IF ( .NOT. ASSOCIATED( act_list_object%next ) ) EXIT
       act_list_object => act_list_object%next
    END DO
    !
  END FUNCTION get_max_dimensions_nrand_d
  !
  !! Ermittle den maximalen Wert f&uuml;r (nicht vorhandene) Komponente "ninnen" in allen Datenobjekten
  FUNCTION get_max_dimensions_ninnen_d ( ) &
       RESULT( res )
    !! aktueller Wert der maximalen Dimension "ninnen" (alle Datenobjekte) <BR>
    !! -9  == Dimension konnte nicht ermittelt werden
    INTEGER           :: res   ! 
    ! lokale Hilfsvariablen
    INTEGER , POINTER :: p_nv, p_nrand  ! 
    !
    res = -9
    act_list_object => first_list_object
    !
    DO
       IF ( .NOT. ASSOCIATED( act_list_object%object ) ) EXIT
       p_nrand => get_nrand_object ( act_list_object%object )
       p_nv    => get_nv_object ( act_list_object%object )
       IF ( ASSOCIATED( p_nrand ) .AND. ASSOCIATED( p_nv ) ) res = MAX( res, p_nv - p_nrand )
       NULLIFY( p_nrand, p_nv )
       IF ( .NOT. ASSOCIATED( act_list_object%next ) ) EXIT
       act_list_object => act_list_object%next
    END DO
    !
  END FUNCTION get_max_dimensions_ninnen_d

  !! Ermittle den maximalen Wert f&uuml;r (nicht vorhandene) Komponente "maxele" in allen Datenobjekten
  FUNCTION get_max_dimensions_maxele_d ( ) &
       RESULT( res )
    !! aktueller Wert der maximalen Dimension "maxele" (alle Datenobjekte) <BR>
    !! -9  == Dimension konnte nicht ermittelt werden
    INTEGER           :: res   ! 
    ! lokale Hilfsvariablen
    INTEGER , POINTER :: p_ne, p_ks(:) ! 
    !
    res = -9
    act_list_object => first_list_object
    !
    DO
       IF ( .NOT. ASSOCIATED( act_list_object%object ) ) EXIT
       p_ne => get_ne_object ( act_list_object%object )
       p_ks => get_ks_object ( act_list_object%object )
       SELECT CASE ( get_h_grid_variant_no(act_list_object%object) )
       CASE ( 1, 2, 5 ) ! GITTER05(DAT), GITTER05(BIN), SELAFIN
          IF ( ASSOCIATED( p_ne ) ) res = MAX( res, p_ne )
       CASE ( 3, 4    ) ! UNTRIM_BAW, UNTRIM_VC
          IF ( ASSOCIATED( p_ks ) ) res = MAX( res, COUNT( p_ks == 3 ) + 2*COUNT( p_ks == 4 ) )
       CASE ( 6       ) ! DELFT3D
          IF ( ASSOCIATED( p_ne ) ) res = MAX( res, 2*p_ne )
       END SELECT
       NULLIFY( p_ne, p_ks )
       IF ( .NOT. ASSOCIATED( act_list_object%next ) ) EXIT
       act_list_object => act_list_object%next
    END DO
    !
  END FUNCTION get_max_dimensions_maxele_d
  !
END MODULE m_h_grid_dimensions
