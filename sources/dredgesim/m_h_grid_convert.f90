! -------------------------------------------------------------------------
! HeadOfPackageModule -----------------------------------------------------
!
!! <H2>Konversion von Gitternetzen zwischen verschiedenen Formaten</h2>
!! @author <A HREF="mailto:lang@hamburg.baw.de">G. Lang</A> ,
!! <A HREF="mailto:schade@hamburg.baw.de">P. Schade</A> <BR>
!! @version 4.8 vom 09/05/06, Quellcode: mod_m_h_grid_convert.f90 
!! <HR>
!! conversion of grid data between different formats <BR>
!
!  Copyright-Hinweis
!
!  Copyright (C) 2001 Bundesanstalt fuer Wasserbau
!
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!
!          2002-07-18 : G. Lang   : Startversion 
!          2002-07-24 : G. Lang   : convert_untrim_red_black 
!          2002-08-09 : P. Schade : convert_to_untrim 
!          2003-03-20 : P. Schade : convert_to_gitter05
!          2003-03-26 : P. Schade : convert_to_untrim auch fuer selafin und Veraenderung der Komponente text
!          2003-04-16 : P. Schade : convert_to_gitter05_d : Abfrage ob IPOBO assoziiert
!          2004-02-25 : G. Lang   : convert_to_untrim mit Sortieren der Kanten nach Innen- und Aussenkanten
!          2004-02-26 : G. Lang   : derive_nsi in USE ergaenzt
!          2004-02-27 : G. Lang   : "convert_untrim_nsi" wird von "convert_to_untrim" gerufen
!          2004-06-24 : G. Lang   : derive_nsf in "convert_untrim_nsi" ergaenzt
!          2004-11-11 : G. Lang   : convert_untrim_terrace 
!  02.01 : 2005-03-07 : G. Lang   : Erweiterungen OpenMI (unvollstaendig)
!  02.02 : 2005-03-10 : G. Lang   : div. Erweiterungen fuer Konfigurationsphase OpenMI
!  02.03 : 2005-03-16 : G. Lang   : Erweiterungen fuer ALLBoundaries,AllOpenBoundaries und AllClosedBoundaries 
!  03.01 : 2005-07-21 : G. Lang   : Anpassungen/Erweiterungen fuer mod. OpenMI-ElementSet-Verstaendnis
!  04.01 : 2005-08-22 : G. Lang   : Lesen/Schreiben Delft3D-Gitternetz
!  04.02 : 2005-11-13 : G. Lang   : Konversion Delft3D- nach UnTRIM-Gitternetz
!  04.03 : 2005-11-23 : G. Lang   : Erweiterungen *.thd, *.lwl, *.ext
!  04.04 : 2005-12-28 : G. Lang   : Vierteilung "refine" fuer UnTRIM-Gitternetze integriert
!  04.05 : 2006-03-23 : G. Lang   : REFINE_D - Tiefen der neu hinzukommenden Kanten werden auf mittlere Tiefe der originalen Kanten gesetzt
!  04.06 : 2006-04-13 : G. Lang   : unerodierbare Tiefen bei Refinement (optional) beruecksichtigen
!  04.07 : 2006-07-26 : G. Lang   : Vertiefen der Bathymetrie eines Gitters
!  04.08 : 2006-08-31 : G. Lang   : Beruecksichtigen der neuen Komponente "dwlp" = "Depth_At_Water_Level_Points" (fuer Delft3D-Konversion)
!
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! Das Modul stellt verschiedene Methoden zum Konvertieren von Gitterdaten <BR>
!! zwischen verschiedenen Formaten zur Verf&uuml;gung. Die <BR>
!! folgenden Konversionswege werden derzeit unterst&uuml;tzt: <BR>
!! <OL>
!!   <LI> Erzeugen einer <EM>Red-Black</EM>-Sortierung der Polygone f&uuml;r 
!!        ein UnTRIM-Gitter.
!!   <LI> Konvertieren einer TELEMAC-Ergebnisdatei des Typs SELAFIN in <BR>
!!        ein GITTER05-Gitter 
!!   <LI> Konvertieren eines GITTER05-Gitters in ein UnTRIM-Gitter.
!! </OL>
!! <HR>
!! <H3>Gebrauch der Methoden dieses Moduls</H3>
!! Die in diesem Modul bereitgestellten Methoden d&uuml;rfen nur
!! von zu dem Paket "h_grid" geh&ouml;renden Modulen direkt 
!! verwendet werden. Insbesondere ist das direkte Verwenden von
!! Methoden aus anderen Paketen unzul&auml;ssig. 
!
MODULE m_h_grid_convert
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] Basis-Modul mit globalen Konstantwerten
  !
  USE b_constants, ONLY : &
       ! Parameter
       Double
  !
  ! [A.2] Basis-Modul "Fehler"
  !
  USE b_error, ONLY :       &
       ! Routinen
       no_error,            &
       any_error,           &
       setup_error_act
  !
  ! [A.3] Basis-Modul "Datei"
  !
  USE b_file, ONLY :       &
       ! Typ
       t_file, &
       ! Routinen
       get_file_name,       &
       set_file_delim,      &
       set_file_form,       &
       set_file_name,       &
       set_file_type,       &
       set_file_status
  !
  ! ----------------------------------------------------------------------
  ! [B] gemeinsam genutzte Daten des Paketes "h_grid"
  ! ----------------------------------------------------------------------
  !
  ! [B.1] Daten-Modul des Paketes "h_grid"
  !
  USE m_h_grid_data, ONLY :   &
       ! Typdefinition
       t_h_grid,              &
       ! Daten
       prn_op, &
       prn_lun,       &
       all_errors,            &
       c_variants_type,       &
       ! Routinen / Interfaces
       get_h_grid_variant_no, &
       get_file_object,       &
       get_hland_object, get_hu_object, get_hv_object, get_dx_object, get_dy_object,         &
       get_ipobo_object, get_ie_object, get_is_object, get_je_object, get_jb_object,         &
       get_jt_object, get_ks_object, get_ne_object, get_nbc_object,   get_nv_object,         &
       get_nen_object, get_xc_object, get_xs_object, get_xy_object,   get_aa_object,         &
       get_hw_object, get_irand_object, get_nrand_object, get_text_object, get_nsi_object,   &
       get_nsf_object, get_isbnd_object, get_thd_object, get_lwl_object, get_ext_object,     &
       get_ns_object, get_huu_object, get_hvu_object, get_hwu_object, get_dwlp_code_object,  &
       setup_hv_object, setup_xy_object, setup_angle_object, setup_file_object,       &
       setup_hland_object, setup_irand_object, setup_nrand_object, setup_nbc_object,  &
       setup_nsf_object, setup_nr_object, setup_text_object, setup_dxmin_object,      &
       setup_ks_object, setup_is_object, setup_hu_object, setup_nen_object,           &
       setup_nsi_object, setup_huu_object,                                            &
       is_polyedge_water_level, is_polyedge_riemann,                                  &
       dealloc_h_grid_irand, dealloc_h_grid_m, dealloc_h_grid_n, dealloc_h_grid_enc,  &
       dealloc_h_grid_bnd, dealloc_h_grid_isbnd, dealloc_h_grid_dry, dealloc_h_grid_ncsize, &
       dealloc_h_grid_nptir, dealloc_h_grid_nptfr, dealloc_h_grid_ipobo, dealloc_h_grid_knolg, &
       dealloc_h_grid_thd, dealloc_h_grid_lwl, dealloc_h_grid_ext, dealloc_h_grid_isdam, &
       dealloc_h_grid_jt, dealloc_h_grid_jb, dealloc_h_grid_je, dealloc_h_grid_ie,       &
       dealloc_h_grid_aa, dealloc_h_grid_hw, dealloc_h_grid_hv, dealloc_h_grid_xs,       &
       dealloc_h_grid_xc, dealloc_h_grid_xg, dealloc_h_grid_dy, dealloc_h_grid_dx,       &
       dealloc_h_grid_is
  !
  ! [B.2] Berechnen unbekannter aus bekannten Groessen
  !
  USE m_h_grid_derive, ONLY : &
       ! Routinen / Interfaces
       derive_is,  derive_ie,  derive_hu,  derive_xc,  derive_dx,  derive_dy,  &
       derive_xs,  derive_nsi, derive_nsf, derive_nen, derive_ks,  derive_irand, &
       derive_hw,  derive_hv,  derive_jb,  derive_jt,  derive_je,  derive_aa,  &
       derive_hvu, derive_huu
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
  ! [C.2] Konstantwerte (Parameter) [moeglichst nicht verwenden]
  ! [C.3] Variablen [moeglichst nicht verwenden]
  ! [C.4] Schnittstellen
  !
  !! konvertiere ein Gitter in das GITTER05-Format
  INTERFACE convert_to_gitter05
     MODULE PROCEDURE convert_to_gitter05_d
  END INTERFACE
  !! konvertiere ein Gitter in das UNTRIM-Format
  INTERFACE convert_to_untrim
     MODULE PROCEDURE convert_to_untrim_d
  END INTERFACE
  !! konvertiere ein Gitter in das SELAFIN-Format
  INTERFACE convert_to_selafin
     MODULE PROCEDURE convert_to_selafin_d
  END INTERFACE
  !! erzeuge eine Red-Black-Sortierung f&uuml;r ein UnTRIM-Gitter
  INTERFACE convert_untrim_red_black
     MODULE PROCEDURE convert_untrim_red_black_d
  END INTERFACE
  !! erzeuge eine Sortierung der internen Kanten f&uuml;r ein UnTRIM-Gitter
  INTERFACE convert_untrim_nsi
     MODULE PROCEDURE convert_untrim_nsi_d
  END INTERFACE
  !! passe die Bathymetrie so an, dass keine toten Volumina (bei 3D-Anwendungen)
  INTERFACE convert_untrim_terrace
     MODULE PROCEDURE convert_untrim_terrace_d
  END INTERFACE
  !! erzeuge aus einem UnTRIM-Gitter ein durch <EM>Vierteilung</EM> verfeinertes UnTRIM-Gitter
  INTERFACE refine
     MODULE PROCEDURE refine_d
  END INTERFACE
  !! Vertiefen eines Gitters
  INTERFACE deepen
     MODULE PROCEDURE deepen_d
  END INTERFACE
  !
  ! [C.5] Zuweisungen
  ! [C.6] Operatoren (optional, falls sinnvoll)
  ! [C.7] Liste der oeffentlichen Methoden
  !
  PUBLIC :: convert_to_gitter05      ! 
  PUBLIC :: convert_to_untrim        ! 
  PUBLIC :: convert_to_selafin       ! 
  PUBLIC :: convert_untrim_red_black ! 
  PUBLIC :: convert_untrim_nsi       ! 
  PUBLIC :: convert_untrim_terrace   ! 
  PUBLIC :: refine                   ! 
  PUBLIC :: deepen                   ! 
  !
  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Datentypen, Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] lokale Typdefinitionen
  ! [D.2] Konstantwerte (Parameter)
  !
  !! Name des Moduls
  CHARACTER (LEN=16), PARAMETER :: c_modname = 'm_h_grid_convert' ! 
  !
  ! [D.3] Variablen (statische Daten des Moduls)
  ! [D.4] Schnittstellen
  ! [D.5] Assignments
  ! [D.6] Operatoren
  ! [D.7] Namelist-Definitionen
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
  !! Gitterdaten in das Format GITTER05 konvertieren <BR>
  !! Die umzuwandelnde Datei darf bislang nur vom Typ SELAFIN sein.
  !! Sie muss die Bedingungen unter [2.4] erfuellen.
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE convert_to_gitter05_d ( this )
    !! aktuelles Arbeitsobjekt
    TYPE (t_h_grid) , POINTER :: this 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER   :: c_upname='convert_to_gitter05_d' ! 
    !! Datei-Objekt
    TYPE (t_file) :: file 
    !! Dateiname
    CHARACTER (LEN=80) :: name
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt, ctxt2, ctxt3
    !! Zeiger auf h_grid-Komponente Text
    CHARACTER (LEN=80), POINTER :: p_text(:)
    !! temp. Text
    CHARACTER (LEN=80), ALLOCATABLE :: tmp_text(:)
    !! Anhang an den vorhandenen Text
    CHARACTER (LEN=80) :: append_text
    !! Anzahl der Knoten im Polygon
    INTEGER , PARAMETER :: ks=3
    !! Hilfsvariable Dateivarianten-Nummer
    INTEGER :: ivar 
    !! Statusvariable
    INTEGER :: stat, iostat
    !! letzter Knoten im TELEMAC-Gitter dessen IPOBO-Kennung von Null abweicht
    INTEGER :: last_ipobo
    !! westlichster Knoten im TELEMAC-Gitter
    INTEGER :: iv_first
    !! aktueller Randknoten
    INTEGER :: iv_edge
    !! Zaehler
    INTEGER :: iv, jv, iele, i, l, iedge, ilen
    !! ermittelte Anzahl der Randknoten
    INTEGER :: nv_edge
    !! Knotenverzeichnis der Polygone
    INTEGER , POINTER :: p_nen(:,:)
    !! Polygonverzeichnis der Polygone
    INTEGER , POINTER :: p_ie(:,:)  
    !! Kantenverzeichnis der Polygone
    INTEGER , POINTER :: p_is(:,:)  
    !! Randkennungen der Knoten
    INTEGER , POINTER :: p_ipobo(:) 
    !! Randkennungen der Polygone
    INTEGER , ALLOCATABLE :: irand(:) 
    !! jeder neue Knoten (nach der Umsortierung) zeigt auf einen alten Knoten
    INTEGER , ALLOCATABLE :: iv_old(:)
    !! Knotenkoordinaten
    REAL (KIND=Double), POINTER :: p_xy(:,:)
    !! Knotentiefen 
    REAL (KIND=Double), POINTER :: p_hv(:)
    !! log. Randknotenkennung eines Polygons
    !! z.B. l_vj = .true. : Knoten j des Polygons ist Randknoten
    LOGICAL :: l_vi, l_vj, l_vk
    !! log. Kennung, ob ein Polygon eine aeussere Randkante aufweist
    LOGICAL :: l_border_polygon
    !! log. Kennung, ob der erste Randknoten zum 2.ten Mal gefunden wurde
    LOGICAL :: l_first_again
    !! log. Kennung, ob die Randknoten entgegen dem Uhrzeigersinn <BR>
    !! aufsteigend sortiert vorliegen
    LOGICAL :: l_iv_old_sorted
    !
    ! [0] Initialsierungen
    append_text = REPEAT (' ', LEN(append_text))
    ctxt  = REPEAT (' ', LEN(ctxt))
    ctxt2 = REPEAT (' ', LEN(ctxt2))
    ctxt2 = REPEAT (' ', LEN(ctxt3))
    !
    ! [1] welche Gittervariante?
    !
    ivar = get_h_grid_variant_no ( this )
    !
    SELECT CASE ( ivar )
    !
    ! [2] TELEMAC2D-Gitter liegt vor
    ! 
    CASE(5) ! 'SELAFIN'
       !
       !
       append_text = '  aus SELAFIN konvertiertes GITTER05-Gitter'
       !
       IF (.NOT.ASSOCIATED(this%ipobo)) THEN
          CALL setup_error_act ( all_errors(:), 6780, c_upname, c_modname )
       ENDIF       
       !
       ! [2.0] Holen erforderlicher Daten
       ! 
       ipo: IF (no_error()) THEN
       NULLIFY (  p_nen, p_ie, &
            p_ipobo, p_xy, p_hv, p_text)
       !
       ! [2.1] Holen erforderlicher Daten
       p_nen   => get_nen_object ( this )
       p_ipobo => get_ipobo_object ( this )
       p_xy    => get_xy_object ( this )
       p_hv    => get_hv_object ( this )
       p_text  => get_text_object ( this )
       !
       ! [2.2] irand und iv_old allokieren und initialisieren
       ALLOCATE (irand(SIZE(p_nen, DIM=1)), &
            iv_old(get_nv_object(this)), &
            STAT=stat )
       !
       alloci: IF ( stat /= 0 ) THEN
          !
          CALL setup_error_act &
               ( all_errors(:), 26107, c_upname, c_modname, stat )
          WRITE( ctxt, '(I10)') SIZE(p_nen, DIM=1)
          WRITE( ctxt2,'(I10)') get_nv_object(this)
          CALL setup_error_act ( '<AktDim1>', ctxt )
          CALL setup_error_act ( '<AktDim2>', ctxt2 )
          !
       ELSE
          !
          DO iv=1,get_nv_object(this)
             iv_old(iv) = iv
          ENDDO
          !
          irand = 0
          !
          ! [2.3] den letzten Knoten mit Randkennung suchen
          DO iv=1, get_nv_object (this)
             
             IF (p_ipobo(iv) .NE. 0) last_ipobo = iv

          END DO
          !
          ! [2.4] Randkennungen generieren 
          ! dieser Algorithmus beruht auf folgenden Voraussetzungen:
          !
          ! - alle Randknoten des festen Randes werden in dem Feld IPOBO mit
          !   von Null abweichenden Werten gekennzeichnet
          !
          ! - der letzte von Null abweichende Randknoten wird mit LASTIPOBO
          !   gekennzeichnet
          !
          ! - der Rand verlaeuft in den entsprechenden Elementen nur entlang
          !   einer Elementkante,
          !   d.h. Elemente mit zwei Randkanten sind _UNZULAESSIG_.
          !
          ! - dieser Algorithmus erzeugt nur geschlossene Randpolygone,
          !   das SELAFIN-file enthaelt keine Informationen ueber offene Raender
          !
          !
          ele: DO iele = 1, get_ne_object (this)
             !
             l_vi = .false.
             l_vj = .false.
             l_vk = .false.
             !
             ! Schleife ueber den Bereich aller Randknoten
             DO iv = 1, last_ipobo
                !
                IF (p_ipobo(iv) /= 0) THEN
                   !
                   IF (p_nen(iele,1) == iv) THEN
                      l_vi = .TRUE.
                   ELSE IF (p_nen(iele,2) == iv) THEN
                      l_vj = .TRUE.
                   ELSE IF (p_nen(iele,3) == iv) THEN
                      l_vk = .TRUE.
                   END IF
                END IF
             END DO
             !
             ! ein Polygon weist drei Randknoten auf
             IF (l_vi .AND. l_vj .AND. l_vk) THEN
                !
                CALL setup_error_act &
                     ( all_errors(:), 26111, c_upname, c_modname, stat )
                WRITE( ctxt, '(I10)') SIZE(p_nen, DIM=1)
                WRITE( ctxt2,'(I10)') get_nv_object(this)
                CALL setup_error_act ( '<KnotenI>', ctxt )
                CALL setup_error_act ( '<KnotenJ>', ctxt2 )
                CALL setup_error_act ( '<KnotenK>', ctxt3 )
                ! 
                RETURN
             ENDIF
             !
             ! geschlossener Rand zwischen Knoten I und J
             IF (l_vi .AND. l_vj) THEN
                irand(iele) = 4
             !
             ! geschlossener Rand zwischen Knoten J und K                
             ELSEIF (l_vj .AND. l_vk) THEN
                irand(iele) = 1
             !
             ! geschlossener Rand zwischen Knoten K und I                
             ELSEIF (l_vk .AND. l_vi) THEN
                irand(iele) = 2
             ENDIF
             !
             IF (MOD(IELE,100000) == 0) &
                  WRITE(*,*) ' ... Randkennungen gesetzt fuer ',iele, &
                  ' Elemente von insgesamt ',get_ne_object(this)
             !
          END DO ele
          !
          !
          CALL setup_irand_object(this, irand)
          !
          ! [2.5] suche den westlichsten Punkt (wenn es mehrere gibt, 
          ! den letzten davon im Punktefeld)
          iv_first = 1
          !
          DO iv = 2, last_ipobo
             !
             IF ( p_xy(iv,1) <= p_xy(iv_first,1) ) iv_first = iv
             !
          ENDDO
          !
          ! [2.6] Polygonverzeichnis der Polygone berechnen
          CALL derive_ie (this)
          p_ie => get_ie_object ( this )
          !          
          !  
          ! [2.7] ausgehend vom westlichsten Punkt Knoten auf dem
          ! Rand entgegen dem Uhrzeigersinn finden
          !
          ! [2.7.1] Polygon zum Punkt iv_first finden
          iv_edge = iv_first
          l_first_again = .false.
          !
          vwest: DO iele = 1, get_ne_object (this)
             !
             DO i=1,ks
                IF (p_nen(iele,i) == iv_edge) EXIT vwest
                !
             ENDDO
             !
          ENDDO vwest
          !
          l = 0
          nv_edge = 1
          !
          ! [2.7.2] Anhand dem Nachbarpolygonverzeichnis p_ie
          ! herausfinden, ob im Polygon eine Randkante vorliegt und
          ! falls dies zutrifft den Randknoten auffinden
          lfirst: DO WHILE (.NOT. l_first_again)
             l = l+1
             !
             l_border_polygon = .false.
             !
             edge: DO iedge=1,ks
                !
                ! Kennung Nachbarpolygon kleiner 0:
                ! - Randkante liegt vor
                ! - naechstes Polygon iele suchen
                IF (p_ie(iele,iedge) <= 0) THEN
                   !
                   l_border_polygon = .true.
                   SELECT CASE ( iedge )
                      !
                   CASE(1) 
                      !
                      ! Kante 1 ist Randkante
                      IF(l==1) THEN 
                         iv_first = p_nen(iele,1)
                         iv_old(1) = p_nen(iele,1)
                      ENDIF
                      iv_edge = p_nen(iele,2)
                      iele =  p_ie(iele,2)
                      !
                   CASE(2)
                      !
                      ! Kante 2 ist Randkante
                      IF(l==1) THEN
                         iv_first = p_nen(iele,2)
                         iv_old(1) = p_nen(iele,2)
                      ENDIF
                      iv_edge = p_nen(iele,3)
                      iele =  p_ie(iele,3)
                      !
                   CASE(3)
                      !
                      ! Kante 3 ist Randkante
                      IF(l==1) THEN
                         iv_first = p_nen(iele,3)
                         iv_old(1) = p_nen(iele,3)
                      ENDIF
                      iv_edge = p_nen(iele,1)
                      iele =  p_ie(iele,1)
                      !
                   END SELECT
                   !
                   IF( .NOT.(iv_edge == iv_first .AND. l > 1) ) THEN
                      nv_edge = nv_edge + 1
                      iv_old(nv_edge) = iv_edge
                   ENDIF
                   !
                END IF
                !
             ENDDO edge
             !
             IF (.NOT. l_border_polygon) THEN
                ! 
                DO iv=1,ks
                   !
                   IF( p_nen(iele,iv) == iv_edge) EXIT
                END DO
                ! iele fuer den naechsten Durchlauf von lfirst setzen
                iele = p_ie(iele,iv)
                !
             ENDIF
             !
             ! [2.7.3] falls der erste Randknoten zum zweitenmal 
             ! getroffen wird, ist die Abbruchbedingung der 
             ! lfirst-Schleife erfuellt.
             IF(iv_edge == iv_first .AND. l > 1) l_first_again = .true.
             !
          ENDDO lfirst
          !
          ! [2.8] nrand setzen
          CALL setup_nrand_object (this, nv_edge)
          !
          !
          ! [2.9] sortieren oder nicht sortieren, das ist hier die Frage
          l_iv_old_sorted = .true.
          !
          IF(iv_old(1) /= 1) l_iv_old_sorted = .false.
          !
          DO iv = 2, nv_edge
             IF (iv_old(iv) /= iv_old(iv-1) + 1) l_iv_old_sorted = .false.
          ENDDO
          !
          ! [2.10.] Ggf. sind die Randknoten zu sortieren.
          ! Danach liegen die Randknoten vom westlichsten Knoten (1) 
          ! aufsteigend entgegen dem Uhrzeigersinn vor. Der letzte Randknoten
          ! hat die Kennung nv_edge. Daran schliessen sich die Innenknoten an.
          !
          IF(.NOT. l_iv_old_sorted) THEN
             !
             IF ( prn_op ) THEN
                WRITE( prn_lun, 7000, IOSTAT=iostat ) 
                !
                IF ( iostat /= 0 ) THEN
                   CALL setup_error_act &
                        ( all_errors(:), 26108, c_upname, c_modname, stat )
                   RETURN
                END IF
                !
             ENDIF
             !
             ! 
             ! [2.11] Schleife ueber alle Plaetze fuer Randknoten
             DO iv = 1, nv_edge
                !
                ! die Knoten iv und iv_old(iv) miteinander vertauschen
                ! und dabei die Felder p_xy, p_hv und p_nen aktualisieren     
                CALL node_swap &
                     (this, &
                     iv, iv_old(iv))
                !
                ! Schleife ueber alle nachfolgenden potentiellen Randknoten
                DO jv= iv+1, nv_edge
                   !
                   ! falls ein iv_old(jv) auf den getauschten Knoten iv zeigt,
                   ! muss auch iv_old(jv) getauscht werden
                   IF (iv_old(jv) == iv) iv_old(jv) = iv_old(iv)
                   !
                ENDDO
                !
             ENDDO
             !
          ELSE
             !
             IF ( prn_op ) THEN
                WRITE( prn_lun, 7010, IOSTAT=iostat ) 
                !
                IF ( iostat /= 0 ) THEN
                   CALL setup_error_act &
                        ( all_errors(:), 26108, c_upname, c_modname, stat )
                   RETURN
                END IF
                !
             ENDIF
          ENDIF
          !
          ! [2.12] h_grid-Komponente file
          !
          file = get_file_object(this)
          ! Dateiname 
          name = get_file_name (file)
          !
          ilen = LEN_TRIM(name)
          ! falls moeglich, den String um drei Zeichen nach hinten verschieben
          IF (ilen < MIN(LEN(get_file_name(file)),LEN(name)) - 2) THEN
             name(4:ilen+3) = name(1:ilen)
          ENDIF
          ! die ersten drei Zeichen mit der Kennung 'gi.' versehen
          name(1:3) = 'gi.'
          CALL set_file_name (file, name)
          CALL set_file_type  ( file, 'gitter05')
          ! 
          ! veraendertes file setzen
          CALL setup_file_object (this, file)
          !
          ! [2.13] h_grid-Komponente text veraendern
          !
          ALLOCATE (tmp_text(SIZE(get_text_object( this )) + 1), &
               STAT=stat )
          !
          alloct: IF ( stat /= 0 ) THEN
             CALL setup_error_act &
                  ( all_errors(:), 26109, c_upname, c_modname, stat )
             WRITE( ctxt, '(I10)') SIZE(get_text_object( this )) + 1
             CALL setup_error_act ( '<AktDim1>', ctxt )
          ELSE
             !
             DO i=1,SIZE(get_text_object( this ))
                tmp_text(i) = p_text(i)
             ENDDO
             !
             ! append_text an tmp_text anhaengen
             tmp_text(SIZE(get_text_object( this )) + 1) = &
                  append_text
             ! 
             ! veraenderten text setzen
             CALL setup_text_object (this, tmp_text)
             !
             !
             ! [2.14] lokales Deallokieren
             ! 
             DEALLOCATE ( irand, iv_old, tmp_text, STAT=stat )
             !
             IF ( stat /= 0 ) CALL setup_error_act &
                  ( all_errors(:), 26112, c_upname, c_modname, stat )
             !
          ENDIF alloct
       ENDIF alloci
    ENDIF ipo
    !
    CALL dealloc_h_grid_ncsize ( this )
    CALL dealloc_h_grid_nptir  ( this )
    CALL dealloc_h_grid_nptfr  ( this )
    CALL dealloc_h_grid_ipobo  ( this )
    CALL dealloc_h_grid_knolg  ( this )
    !
    ! [3] TELEMAC2D-Gitter liegt nicht vor
    ! 
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), 26110, c_upname, c_modname )
       IF ( ivar > 0 ) THEN
          CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       ELSE
          CALL setup_error_act ( '<DateiVarianteName>', 'undefined' )
       END IF
       WRITE(ctxt,'(I10)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
    ! [4] Formatanweisungen
    !
7000 FORMAT ( &
          '# ',/ &
          '# Bei der Konvertierung des TELEMAC-Gitters ins Format',/ &
          '# GITTER05 muessen die Knoten neu sortiert werden. ')
    !
7010 FORMAT ( &
          '# ',/ &
          '# Bei der Konvertierung des TELEMAC-Gitters ins Format',/ &
          '# GITTER05 muessen die Knoten nicht sortiert werden. ')
    !
  END SUBROUTINE convert_to_gitter05_d  
  !
  !! Gitterdaten in das Format UNTRIM konvertieren; <BR>
  !! z.Z. Konvertieren eines GITTER05-Gitters in ein UNTRIM_BAW-Gitter;
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE convert_to_untrim_d ( this )
    !! aktuelles Arbeitsobjekt
    TYPE (t_h_grid) , POINTER :: this 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER   :: c_upname='convert_to_untrim_d' ! 
    !! Default der mittleren geographische Breite
    REAL (KIND=Double), PARAMETER   :: def_angle = 53.0_Double        ! 
    !! Default der Tiefenlage zur Kennzeichnung von dauerhaften Landgebieten
    REAL (KIND=Double), PARAMETER   :: def_hland = -10000.0_Double    ! 
    !! Default f&uuml;r den minimalen Abstand zwischen Zentren
    REAL (KIND=Double), PARAMETER   :: def_dxmin = 1.00_Double        ! 
    !
    !! Datei-Objekt
    TYPE (t_file) :: file 
    !! Dateiname
    CHARACTER (LEN=80) :: name ! 
    !! h_grid-Komponente text 
    CHARACTER (LEN=80), ALLOCATABLE :: tmp_text(:) ! 
    !! Zeiger auf h_grid-Komponente Text
    CHARACTER (LEN=80), POINTER :: p_text(:) ! 
    !! Default-Text fuer UnTRIM-Datei
    CHARACTER (LEN=80) :: append_text ! 
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !! Hilfsvariable Dateivarianten-Nummer
    INTEGER :: ivar            ! 
    !! Statusvariable
    INTEGER :: stat, iostat    !  
    !! Anzahl der Polygone entlang des offenen Randes
    INTEGER , POINTER :: p_nbc        ! 
    !! Knotenverzeichnis der Polygone
    INTEGER , POINTER :: p_nen(:,:)   ! 
    !! Polygonverzeichnis der Polygone
    INTEGER , POINTER :: p_ie(:,:)    ! 
    !! Kantenverzeichnis der Polygone sowie Randkantentypverzeichnis der Polygone
    INTEGER , POINTER :: p_is(:,:), p_isbnd(:,:) ! 
    !! Randkennungen der Polygone
    INTEGER , POINTER :: p_irand(:)   ! 
    !! Anzahl der Knoten/Kanten in einem Polygon
    INTEGER , POINTER :: p_ks(:)      ! 
    !! Anzahl der Knoten auf dem Rand
    INTEGER , POINTER :: p_nrand      ! 
    !! Anzahl der Knoten
    INTEGER , POINTER :: p_nv         ! 
    !! Anzahl der Polygone
    INTEGER , POINTER :: p_ne         ! 
    !! Tiefe ueber den Kanten
    REAL (KIND=Double) , POINTER :: p_hu(:)              ! 
    !! nicht weiter erodierbare Tiefen &uuml;ber Kanten und Knoten
    REAL (KIND=Double) , POINTER :: p_huu(:), p_hvu(:)   ! 
    !! Koordinaten der Zentren der Polygone sowie der Knoten
    REAL (KIND=Double) , POINTER :: p_xc(:,:), p_xy(:,:) !            
    !! Tiefen an den Knoten
    REAL (KIND=Double) , POINTER :: p_hv(:)              ! 
    !! enthaelt das Polygon mindestens eine Kante am offenen Rand?
    LOGICAL, POINTER :: l_rand_offen(:) ! 
    !! Hilfsfelder
    INTEGER            , ALLOCATABLE :: i_data1(:)               ! 
    REAL (KIND=Double) , ALLOCATABLE :: d_data1(:), d_data2(:,:) ! 
    LOGICAL            , ALLOCATABLE :: l_data1(:)               ! 
    !! Z&auml;hlervariable
    INTEGER :: i, j, n, m, i_poly, i_poly2 ! 
    !! ganzzahlige Hilfsvariablen
    INTEGER :: nbc, ilen, ipos  ! 
    !! Variable zum Zwischenspeichern eines p_irand-Elementes
    INTEGER :: tmp_irand  ! 
    !! Variable zum Zwischenspeichern von p_nen-Elementen
    INTEGER :: tmp_nen(3) ! 
    !
    name        = REPEAT (' ', LEN(name))
    append_text = REPEAT (' ', LEN(append_text))
    ctxt        = REPEAT (' ', LEN(ctxt))
    ! [0] TELEMAC2D-Gitter liegt vor [ vorab nach GITTER05 konvertieren ]
    IF (get_h_grid_variant_no ( this ) == 5) CALL convert_to_gitter05 (this)
    IF (no_error() ) THEN
       ! [1] welche Gittervariante?
       ivar = get_h_grid_variant_no ( this )
       SELECT CASE ( ivar )
          ! ----------------------------------------------------------------
       CASE(1,2) ! 'GITTER05(formatted), GITTER05(unformatted)'
          ! ----------------------------------------------------------------
          append_text = '  aus GITTER05 konvertiertes UnTRIM-Gitter'
          NULLIFY (  p_nen, p_is, p_irand, p_nrand, p_text)
          ! Holen erforderlicher Daten
          p_nen   => get_nen_object ( this )
          p_irand => get_irand_object ( this )
          p_nrand => get_nrand_object ( this )
          p_text  => get_text_object ( this )
          ALLOCATE (l_rand_offen(SIZE(p_irand)), STAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 26105, c_upname, c_modname, stat )
             WRITE( ctxt, '(I10)') SIZE(p_irand)
             CALL setup_error_act ( '<AktDim1>', ctxt )
          ELSE
             !
             ! [3] Konvertierung
             ! [3.1] offene Randkanten in das logische Feld l_rand_offen eintragen
             l_rand_offen = .false.
             ! Kante 2 gegenueber Knoten 1
             DO i_poly = 1, SIZE(l_rand_offen)
                ! wenn Knoten 2 und 3 Randknoten sind
                kante2: IF ( p_nen(i_poly,2) <= p_nrand .AND. &
                     p_nen(i_poly,3) <= p_nrand ) THEN
                   ! Kante 2 ist offen
                   IF (edge_open(2, p_irand(i_poly))) THEN
                      ! liegt Kante 2 auf dem Rand?
                      l_rand_offen(i_poly) = neighboured_boundary_nodes ( &
                           p_nen(i_poly,2), &
                           p_nen(i_poly,3), &
                           p_nrand)
                   ENDIF
                ENDIF kante2
                ! Kante 3 gegenueber Knoten 2
                IF (.NOT. l_rand_offen(i_poly)) THEN 
                   ! wenn Knoten 1 und 3 Randknoten sind
                   kante3: IF ( p_nen(i_poly,1) <= p_nrand .AND. &
                        p_nen(i_poly,3) <= p_nrand ) THEN
                      ! Kante 3 ist offen
                      IF (edge_open(3, p_irand(i_poly))) THEN
                         ! liegt Kante 3 auf dem Rand?
                         l_rand_offen(i_poly) = neighboured_boundary_nodes ( &
                              p_nen(i_poly,1), &
                              p_nen(i_poly,3), &
                              p_nrand)
                      ENDIF
                   ENDIF kante3
                ENDIF
                ! Kante 1 gegenueber Knoten 3
                IF (.NOT. l_rand_offen(i_poly)) THEN 
                   ! wenn Knoten 1 und 2 Randknoten sind
                   kante1: IF ( p_nen(i_poly,1) <= p_nrand .AND. &
                        p_nen(i_poly,2) <= p_nrand ) THEN
                      ! Kante 1 ist offen
                      IF (edge_open(1, p_irand(i_poly))) THEN
                         ! liegt Kante 1 auf dem Rand?
                         l_rand_offen(i_poly) = neighboured_boundary_nodes ( &
                              p_nen(i_poly,1), &
                              p_nen(i_poly,2), &
                              p_nrand)
                      ENDIF
                   ENDIF kante1
                ENDIF
             ENDDO
             nbc     = COUNT (l_rand_offen)
             i_poly2 = nbc + 1
             ! [3.2] p_nen und p_irand umsortieren, so dass die ersten Elemente
             ! Kanten des offenen Randes repraesentieren
             DO i_poly = 1, nbc
                IF (.NOT. l_rand_offen(i_poly)) THEN
                   look: DO
                      IF (l_rand_offen(i_poly2)) THEN
                         tmp_nen(:) = p_nen(i_poly,:)
                         tmp_irand = p_irand(i_poly)
                         ! Feldelement i_poly erhaelt Dreieck mit mindestens einem offenen Rand
                         p_nen(i_poly,:) = p_nen(i_poly2,:)
                         p_irand(i_poly) = p_irand(i_poly2)
                         ! Feldelement i_poly2 erhaelt Dreieck ohne offenen Rand
                         p_nen(i_poly2,:) = tmp_nen(:)
                         p_irand(i_poly2) = tmp_irand
                         i_poly2 = i_poly2 + 1
                         EXIT look
                      ELSE
                         i_poly2 = i_poly2 + 1
                      ENDIF
                   ENDDO look
                ENDIF
             ENDDO
             ! [3.3] weitere Berechnungen
             IF ( no_error( ) ) CALL setup_nbc_object   ( this, nbc       )
             IF ( no_error( ) ) CALL setup_angle_object ( this, def_angle )
             IF ( no_error( ) ) CALL setup_hland_object ( this, def_hland )
             IF ( no_error( ) ) CALL setup_dxmin_object ( this, def_dxmin )
             ! Polygonzentren berechnen
             IF(no_error()) CALL derive_xc (this)
             ! Kantenverzeichnis der Polygone berechnen
             IF(no_error()) CALL derive_is (this)
             p_is => get_is_object(this)
             ! [3.4] Tiefe auf den Kanten berechnen ...
             ! [3.4.1] ... allgemein
             IF(no_error()) CALL derive_hu (this)
             p_hu => get_hu_object(this)
             IF(no_error()) THEN
                ! [3.4.2] ... Kanten des geschlossenen Randes
                DO i_poly = 1, SIZE(l_rand_offen)
                   ! Kante 2 gegenueber Knoten 1
                   ! wenn Knoten 2 und 3 Randknoten sind
                   edge2: IF ( p_nen(i_poly,2) <= p_nrand .AND. &
                        p_nen(i_poly,3) <= p_nrand ) THEN
                      ! Kante 2 ist geschlossen
                      IF ( .NOT. edge_open(2, p_irand(i_poly))) THEN
                         ! liegt Kante 2 auf dem Rand?
                         IF ( neighboured_boundary_nodes ( &
                              p_nen(i_poly,2), &
                              p_nen(i_poly,3), &
                              p_nrand) ) p_hu(p_is(i_poly,2)) = def_hland -1.0_Double
                      ENDIF
                   ENDIF edge2
                   ! wenn Knoten 1 und 3 Randknoten sind
                   edge3: IF ( p_nen(i_poly,1) <= p_nrand .AND. &
                        p_nen(i_poly,3) <= p_nrand ) THEN
                      ! Kante 3 ist geschlossen
                      IF ( .NOT. edge_open(3, p_irand(i_poly))) THEN
                         ! liegt Kante 3 auf dem Rand?
                         IF ( neighboured_boundary_nodes ( &
                              p_nen(i_poly,1), &
                              p_nen(i_poly,3), &
                              p_nrand) ) p_hu(p_is(i_poly,3)) = def_hland -1.0_Double
                      ENDIF
                   ENDIF edge3
                   ! Kante 1 gegenueber Knoten 3
                   ! wenn Knoten 1 und 2 Randknoten sind
                   edge1: IF ( p_nen(i_poly,1) <= p_nrand .AND. &
                        p_nen(i_poly,2) <= p_nrand ) THEN
                      ! Kante 1 ist geschlossen
                      IF ( .NOT. edge_open(1, p_irand(i_poly))) THEN
                         IF ( neighboured_boundary_nodes ( &
                              p_nen(i_poly,1), &
                              p_nen(i_poly,2), &
                              p_nrand) ) p_hu(p_is(i_poly,1)) = def_hland -1.0_Double
                      ENDIF
                   ENDIF edge1
                ENDDO
             ENDIF
             ! [3.5] h_grid-Komponente file
             file = get_file_object(this)
             ! Dateinamen holen
             name = get_file_name (file)
             ilen = LEN_TRIM(name)
             ! falls moeglich, den String um vier Zeichen nach hinten verschieben
             IF (ilen < MIN(LEN(get_file_name(file)),LEN(name)) - 3) THEN
                name(5:ilen+4) = name(1:ilen)
             ENDIF
             ! die ersten vier Zeichen mit der Kennung 'utr.' versehen
             name(1:4) = 'utr.'
             ! Komponenten von file setzen
             CALL set_file_name (file, name)
             CALL set_file_form  ( file, 'FORMATTED  ')
             CALL set_file_type  ( file, 'UNTRIM_BAW')
             CALL set_file_delim ( file, 'QUOTE' )
             ! veraenderte h_grid-Komponente file setzen
             CALL setup_file_object (this, file)
             ! [3.6] h_grid-Komponente text veraendern
             ! falls text bereits vorhanden -> erweitern
             IF(ASSOCIATED( get_text_object( this ) ) ) THEN
                ALLOCATE (tmp_text(SIZE(get_text_object( this )) + 1), &
                     STAT=stat )
                IF ( stat /= 0 ) THEN
                   CALL setup_error_act &
                        ( all_errors(:), 26109, c_upname, c_modname, stat )
                   WRITE( ctxt, '(I10)') SIZE(get_text_object( this )) + 1
                   CALL setup_error_act ( '<AktDim1>', ctxt )
                ELSE
                   DO i=1,SIZE(get_text_object( this ))
                      tmp_text(i) = p_text(i)
                   ENDDO
                   tmp_text(SIZE(get_text_object( this )) + 1) = &
                        append_text
                   CALL setup_text_object (this, tmp_text)
                   ! De-Allokieren 
                   DEALLOCATE ( tmp_text, STAT=stat )
                   IF ( stat /= 0 ) THEN
                      CALL setup_error_act &
                           ( all_errors(:), 26113, c_upname, c_modname, stat )
                   END IF
                ENDIF
             ELSE
                CALL setup_text_object (this, append_text)
             ENDIF
             WRITE(*,*) get_text_object (this)
             ! [3.7] Setzen von NR
             IF ( no_error( ) ) CALL setup_nr_object ( this, SIZE(this%nen,1) )
             ! [3.8] Sortierung der Innenkanten fuer UnTRIM-Gitter
             IF ( no_error( ) ) CALL convert_untrim_nsi       ( this )
             ! [3.9] Optimierung der Tiefen (Entfernen toter Volumina)
             IF ( no_error( ) ) CALL convert_untrim_terrace   ( this )
             IF(no_error()) THEN
                ! De-Allokieren 
                DEALLOCATE ( l_rand_offen, STAT=stat )
                IF ( stat /= 0 ) THEN
                   CALL setup_error_act ( all_errors(:), 26106, c_upname, c_modname, stat )
                END IF
             ENDIF
          ENDIF
          NULLIFY ( p_nbc, p_nen, p_is, p_irand, p_nrand)
          ! 3.10 de-allokieren nicht mehr benoetigter Felder
          IF ( no_error( ) ) THEN
             CALL dealloc_h_grid_irand ( this ) 
          END IF
          ! ----------------------------------------------------------------
       CASE ( 6 ) ! Delft3D-Gitternetz
          ! ----------------------------------------------------------------
          append_text = '* aus DELFT3D konvertiertes UnTRIM-Gitter '
          ! ----------------------------------------------------------------
          ! [4.1] eliminiere die nicht weiter benoetigten Knoten aus NEN
          !       und setze die Koordinaten XY der Knoten neu
          ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          IF ( no_error( ) ) THEN
             p_ne  => get_ne_object  ( this )
             p_nv  => get_nv_object  ( this )
             p_xy  => get_xy_object  ( this )
             p_hv  => get_hv_object  ( this )
             p_nen => get_nen_object ( this ) 
             IF ( .NOT. ASSOCIATED( p_nen ) ) THEN
                CALL derive_nen ( this )
                p_nen => get_nen_object ( this )
             END IF
             p_ks  => get_ks_object  ( this )
             IF ( .NOT. ASSOCIATED( p_ks ) ) THEN
                CALL derive_ks ( this )
                p_ks => get_ks_object ( this )
             END IF
             ALLOCATE ( l_data1(p_nv), i_data1(p_nv) )
             l_data1(:) = .false.
             i_data1(:) = -1
             DO i=1,SIZE(p_nen,1)
                DO j=1,p_ks(i)
                   l_data1(p_nen(i,j)) = .true.
                END DO
             END DO
             ALLOCATE ( d_data1(COUNT(l_data1)), d_data2(COUNT(l_data1),2) )
             n = 0
             DO i=1,p_nv
                IF ( .NOT. l_data1(i) ) CYCLE
                n            = n + 1
                i_data1(i)   = n
                d_data2(n,:) = p_xy(i,:) ! neue Koordinaten der Knoten
                d_data1(n  ) = p_hv(i)   ! neue Tiefen der Knoten
             END DO
             DO i=1,SIZE(p_nen,1)
                DO j=1,p_ks(i)
                   p_nen(i,j) = i_data1(p_nen(i,j))
                END DO
             END DO
             p_nv = -1
             CALL setup_xy_object  ( this, d_data2 )
             CALL setup_hv_object  ( this, d_data1 )
             CALL setup_nr_object  ( this, p_ne    )
             DEALLOCATE ( l_data1, i_data1, d_data1, d_data2 )
             NULLIFY    ( p_nv, p_xy, p_nen, p_ks  )
          END IF
          ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          ! [ 4.2 ] Sortieren der Polygone 1 ... NBC
          ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
          IF ( no_error( ) ) THEN
             p_nen   => get_nen_object   ( this ) 
             p_isbnd => get_isbnd_object ( this )
             p_ks    => get_ks_object    ( this ) 
             ALLOCATE(l_data1(SIZE(p_nen,1)),i_data1(SIZE(p_nen,2)))
             DO i=1,SIZE(p_nen,1)
                l_data1(i) = .false.
                DO j=1,p_ks(i)
                   IF ( l_data1(i) ) EXIT
                   l_data1(i) = ( is_polyedge_water_level(this,i,j) .OR. is_polyedge_riemann(this,i,j) )
                END DO
             END DO
             nbc = COUNT(l_data1)
             n   = 0
             DO i=1,SIZE(l_data1)
                IF ( n == nbc  ) EXIT
                IF ( l_data1(i) ) THEN
                   n = n + 1
                   IF ( i > n ) THEN ! ggf. tauschen
                      i_data1(:)   = p_nen(i,:)
                      p_nen(i,:)   = p_nen(n,:)
                      p_nen(n,:)   = i_data1(:)
                      i_data1(:)   = p_isbnd(i,:)
                      p_isbnd(i,:) = p_isbnd(n,:)
                      p_isbnd(n,:) = i_data1(:)
                      m            = p_ks(i)
                      p_ks(i)      = p_ks(n)
                      p_ks(n)      = m
                   END IF
                END IF
             END DO
             CALL setup_nbc_object ( this, nbc )
             DEALLOCATE ( l_data1, i_data1 )
             NULLIFY ( p_nen, p_ks, p_isbnd )
          END IF
          ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          ! [ 4.3 ] HLAND, ANGLE, DXMIN, HU(neu), FILE, TEXT
          ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
          IF ( no_error( ) ) THEN
             CALL setup_hland_object ( this, def_hland )
             CALL setup_angle_object ( this, def_angle )
             CALL setup_dxmin_object ( this, def_dxmin )
             CALL derive_hu          ( this )
             p_hu  => get_hu_object  ( this )
             NULLIFY ( p_hu )
             CALL derive_huu         ( this )
             p_huu => get_huu_object ( this ) 
             NULLIFY( p_huu )
             CALL derive_hvu         ( this )
             p_hvu => get_hvu_object ( this ) 
             NULLIFY( p_hvu )
             !
             file = get_file_object ( this )
             name = get_file_name   ( file )
             ilen = LEN_TRIM(name)
             IF (ilen < LEN(name) - 3) name(5:ilen+4) = name(1:ilen)
             name(1:4) = 'utr.'
             ipos      = INDEX( name, '.', BACK=.true. )
             name(ipos+1: )       = REPEAT( ' ', LEN(name)-ipos )
             name(ipos+1:ipos+3 ) = 'dat'
             CALL set_file_name     ( file, name )
             CALL set_file_status   ( file, 'UNKNOWN'    )
             CALL set_file_form     ( file, 'FORMATTED'  )
             CALL set_file_type     ( file, 'UNTRIM_BAW' )
             CALL set_file_delim    ( file, 'QUOTE' )
             CALL setup_file_object ( this, file )
             !
             p_text => get_text_object ( this )
             IF ( ASSOCIATED( p_text ) ) THEN
                ALLOCATE   ( tmp_text(SIZE(p_text)+1) )
                tmp_text(:) = REPEAT( ' ', LEN(tmp_text) )
                tmp_text( 1:SIZE(p_text) ) = p_text(:) 
                tmp_text( SIZE(p_text)+1 ) = append_text
                NULLIFY( p_text )
                CALL setup_text_object ( this, tmp_text )
                DEALLOCATE ( tmp_text )
             END IF
             CALL derive_hv          ( this ) ! neu eingefuegt (GL)
          END IF
          ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
          ! [ 4.4 ] div. nicht mehr benoetigte Felder de-allokieren
          ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
          IF ( no_error( ) ) THEN
             CALL dealloc_h_grid_thd   ( this )
             CALL dealloc_h_grid_lwl   ( this )
             CALL dealloc_h_grid_ext   ( this )
             CALL dealloc_h_grid_isdam ( this )
          END IF
          ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          ! [ 4.5 ] NSI-Sortierung, NSF ermitteln / KEINE Terrassierung
          ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
          p_hu => get_hu_object   ( this )
          NULLIFY ( p_hu )
          IF ( no_error( ) ) CALL convert_untrim_nsi     ( this )
          p_hu => get_hu_object   ( this )
          NULLIFY ( p_hu )
          ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
          ! [ 4.6 ] div. nicht mehr benoetigte Felder de-allokieren
          ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
          IF ( no_error( ) ) THEN
             CALL dealloc_h_grid_irand ( this ) 
             CALL dealloc_h_grid_m     ( this )
             CALL dealloc_h_grid_n     ( this )
             CALL dealloc_h_grid_enc   ( this )
             CALL dealloc_h_grid_bnd   ( this )
             CALL dealloc_h_grid_isbnd ( this )
             CALL dealloc_h_grid_dry   ( this )
          END IF
          ! ----------------------------------------------------------------
       CASE DEFAULT ! unbekannter Gittertyp fuer die Konversion
          ! ----------------------------------------------------------------
          CALL setup_error_act ( all_errors(:), 26004, c_upname, c_modname )
          IF ( ivar > 0 ) THEN
             CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
          ELSE
             CALL setup_error_act ( '<DateiVarianteName>', 'undefined' )
          END IF
          WRITE(ctxt,'(I10)') ivar
          CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
       END SELECT
    END IF
    !
  END SUBROUTINE convert_to_untrim_d
  !
  !! Gitterdaten in das Format SELAFIN konvertieren <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE convert_to_selafin_d ( )
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER   :: c_upname='convert_to_selafin_d' ! 
    !
    write(*,*) ' *** code missing '//TRIM(c_upname)
    !
  END SUBROUTINE convert_to_selafin_d
  !
  !! Red-Black-Sortierung f&uuml;r UNTRIM-Gitter erzeugen <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE convert_untrim_red_black_d ( this )
    !! aktuelles Arbeitsobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER   :: c_upname='convert_untrim_red_black_d' ! 
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !! Hilfsvariable Dateivarianten-Nummer
    INTEGER :: ivar ! 
    !! Statusvariable
    INTEGER :: stat, iostat ! 
    !! Anzahl der Polygone im Gitter
    INTEGER , POINTER :: p_ne       ! 
    !! Anzahl der Polygone entlang des offenen Randes
    INTEGER , POINTER :: p_nbc      ! 
    !! Anzahl der Kanten/Knoten je Polygon
    INTEGER , POINTER :: p_ks(:)    ! 
    !! Knotenverzeichnis der Polygone
    INTEGER , POINTER :: p_nen(:,:) ! 
    !! Polygonverzeichnis der Polygone
    INTEGER , POINTER :: p_ie(:,:)  ! 
    !! Kantenverzeichnis der Polygone
    INTEGER , POINTER :: p_is(:,:)  ! 
    !! Polygonverzeichnis der Kanten
    INTEGER , POINTER :: p_je(:,:)  ! 
    !! Randkennungen der Polygone
    INTEGER , POINTER :: p_irand(:) ! 
    !! Fl&auml;che der Polygone
    REAL (KIND=Double) , POINTER :: p_aa(:) ! 
    !! Tiefen der Polygone
    REAL (KIND=Double) , POINTER :: p_hw(:) ! 
    !! Koordinaten der Zentren der Polygone
    REAL (KIND=Double) , POINTER :: p_xc(:,:) ! 
    !! Indikatorfeld rote/schwarze Polygone <BR>
    !! nnrr(i) = 0 : <EM>rotes</EM> Polygon <BR>
    !! nnrr(i) = 1 : <EM>schwarzes</EM> Polygon
    INTEGER , ALLOCATABLE :: nnrr(:) ! 
    !! Z&auml;hlervariable
    INTEGER :: i, j ! 
    !! ganzzahlige Hilfsvariablen
    INTEGER :: i2, nr, nb, ntmp ! 
    !! reelwertige Hilfsvariablen
    REAL (KIND=Double) :: tmp ! 
    !
    ! [1.1] Red-Black-Sortierung fuer das Gitter durchfuehren
    !
    ivar = get_h_grid_variant_no ( this )
    !
    SELECT CASE ( ivar )
    CASE(3,4) ! 'UNTRIM_VC,UNTRIM_BAW'
       NULLIFY ( p_ne, p_nbc, p_ks, p_nen, p_ie, p_is, &
            p_je, p_irand, p_aa, p_hw, p_xc )
       ! Holen erforderlicher Daten
       p_ne  => get_ne_object  ( this )
       p_nbc => get_nbc_object ( this )
       p_ie  => get_ie_object  ( this )
       p_is  => get_is_object  ( this )
       p_ks  => get_ks_object  ( this )
       p_nen => get_nen_object ( this )
       IF ( .NOT. ASSOCIATED( p_is ) ) THEN
          CALL derive_is ( this )
          p_is => get_is_object ( this )
       END IF
       IF ( .NOT. ASSOCIATED( p_ie ) ) THEN
          CALL derive_ie ( this )
          p_ie => get_ie_object ( this )
       END IF
       p_xc    => get_xc_object    ( this )
       p_je    => get_je_object    ( this )
       p_irand => get_irand_object ( this )
       p_aa    => get_aa_object    ( this )
       p_hw    => get_hw_object    ( this )
       IF ( no_error ( ) ) THEN
          ! Allokieren des Rot-Schwarz-Feldes
          ALLOCATE ( nnrr(0:p_ne), STAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 26001, c_upname, c_modname, stat )
             WRITE( ctxt, '(I10)') p_ne
             CALL setup_error_act ( '<AktDim1>', ctxt )
          ELSE
             nnrr(:)              = -1
             nnrr(1:MAX(1,p_nbc)) = 0 ! rote Polygone entlang des offenen Randes
             ! bestimme fuer jedes Polygon die Farbe Rot oder Schwarz
             DO WHILE ( ANY( nnrr(1:) < 0 ) )
                DO i=1,p_ne
                   IF ( nnrr(i) == -1  .AND. ANY( nnrr(p_ie(i,1:p_ks(i))) == 0 ) ) THEN
                      nnrr(i) = 1 ! aktuelles Polygon ist schwarz
                      WHERE ( p_ie(i,1:p_ks(i)) > 0 )
                         nnrr(p_ie(i,1:p_ks(i))) = 0 ! Nachbarpolygone werden rot
                      END WHERE
                   END IF
                END DO
             END DO
             IF ( prn_op ) THEN
                nr = COUNT( nnrr(:) == 0 )
                nb = COUNT( nnrr(:) == 1 )
                WRITE( prn_lun, 8000, IOSTAT=iostat ) &
                     p_ne, nr, REAL(100*nr)/REAL(p_ne), nb, REAL(100*nb)/REAL(p_ne)
                IF ( iostat /= 0 ) THEN
                   CALL setup_error_act ( all_errors(:), 26003, c_upname, c_modname, iostat )
                END IF
             END IF
             IF ( no_error( ) ) THEN
                ! bringe die "roten" Polygone in die "Pole-Position"
                i2 = p_ne
                DO i = p_nbc+1,p_ne-1
                   IF ( nnrr(i) == 0 ) CYCLE ! 
                   DO j = i2, i, -1
                      IF (nnrr(j) == 1 ) CYCLE
                      i2 = j
                      EXIT
                   END DO
                   IF ( i >= i2 ) EXIT
                   ! ggf. Polygonverzeichnis der Kanten
                   IF ( ASSOCIATED( p_je ) ) THEN
                      DO j=1,p_ks( i)
                         WHERE ( p_je(p_is( i,j),:) ==   i ) p_je(p_is( i,j),:) = -i2
                      END DO
                      DO j=1,p_ks(i2)
                         WHERE ( p_je(p_is(i2,j),:) ==  i2 ) p_je(p_is(i2,j),:) =   i
                      END DO
                      DO j=1,p_ks( i)
                         WHERE ( p_je(p_is( i,j),:) == -i2 ) p_je(p_is( i,j),:) =  i2
                      END DO
                   END IF
                   ! Anzahl der Knoten / Kanten im Polygon abwandeln
                   ntmp     = p_ks(i)
                   p_ks(i)  = p_ks(i2)
                   p_ks(i2) = ntmp
                   ! Knotenverzeichnis der Polygone abwandeln und ...
                   ! ... Kantenverzeichnis der Polygone abwandeln
                   DO j=1,SIZE(p_nen(:,:),DIM=2)
                      ntmp        = p_nen( i,j)
                      p_nen( i,j) = p_nen(i2,j)
                      p_nen(i2,j) = ntmp
                      ntmp        = p_is( i,j)
                      p_is( i,j)  = p_is(i2,j)
                      p_is(i2,j)  = ntmp
                   END DO
                   ! ggf. Zentrumskoordinaten der Polygone
                   IF ( ASSOCIATED( p_xc ) ) THEN
                      DO j=1,SIZE(p_xc(:,:),DIM=2)
                         tmp        = p_xc( i,j)
                         p_xc( i,j) = p_xc(i2,j)
                         p_xc(i2,j) = tmp
                      END DO
                   END IF
                   ! ggf. Randkennungen der Polygone
                   IF ( ASSOCIATED( p_irand ) ) THEN
                      ntmp        = p_irand( i)
                      p_irand( i) = p_irand(i2)
                      p_irand(i2) = ntmp
                   END IF
                   ! ggf. Flaechen der Polygone
                   IF ( ASSOCIATED( p_aa ) ) THEN
                      tmp      = p_aa( i)
                      p_aa( i) = p_aa(i2)
                      p_aa(i2) = tmp
                   END IF
                   ! ggf. Tiefen der Polygone
                   IF ( ASSOCIATED( p_hw ) ) THEN
                      tmp      = p_hw( i)
                      p_hw( i) = p_hw(i2)
                      p_hw(i2) = tmp
                   END IF
                   i2 = i2 - 1
                END DO
             END IF
             IF ( ASSOCIATED( p_ie ) ) CALL derive_ie ( this )
             CALL setup_nr_object ( this, nr )
             ! De-Allokieren des Rot-Schwarz-Feldes
             DEALLOCATE ( nnrr, STAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 26002, c_upname, c_modname, stat )
             END IF
          END IF
       END IF
       NULLIFY ( p_ne, p_nbc, p_ks, p_nen, p_ie, p_is, &
            p_je, p_irand, p_aa, p_hw, p_xc )
    CASE DEFAULT
       WRITE(*,*) ivar
       CALL setup_error_act ( all_errors(:), 26000, c_upname, c_modname )
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I10)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
    ! [2.1] Formatanweisungen
    !
8000 FORMAT ( &
          '# Informationen zur Red-Black-Sortierung der Polygone -------------',/ &
          '# Anzahl der Polygone = ',I10,/ &
          '# ... davon ROT       = ',I10,', entspricht ',F7.4,' Prozent',/ &
          '# ... davon SCHWARZ   = ',I10,', entspricht ',F7.4,' Prozent',/ &
          '# -----------------------------------------------------------------' )
    ! 
  END SUBROUTINE convert_untrim_red_black_d
  !
  !! Sortierung nach Innenkanten f&uuml;r UNTRIM-Gitter erzeugen <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE convert_untrim_nsi_d ( this )
    !! aktuelles Arbeitsobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER   :: c_upname='convert_untrim_nsi_d' ! 
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !! Hilfsvariable Dateivarianten-Nummer
    INTEGER :: ivar ! 
    !! Statusvariable
    INTEGER :: stat, iostat ! 
    !! Anzahl der Innenkanten
    INTEGER            , POINTER :: p_nsi     ! 
    !! Polygonverzeichnis der Kanten
    INTEGER            , POINTER :: p_je(:,:) ! 
    !! Kantenverzeichnis der Polygone
    INTEGER            , POINTER :: p_is(:,:) ! 
    !! erster Knoten der Kanten
    INTEGER            , POINTER :: p_jb(:)   ! 
    !! zweiter Knoten der Kanten
    INTEGER            , POINTER :: p_jt(:)   ! 
    !! Tiefe an den Kanten
    REAL (KIND=Double) , POINTER :: p_hu(:)   ! 
    !! Abstand zwischen Zentren
    REAL (KIND=Double) , POINTER :: p_dx(:)   ! 
    !! L&auml;nge der Kanten
    REAL (KIND=Double) , POINTER :: p_dy(:)   ! 
    !! Zentren der Kanten
    REAL (KIND=Double) , POINTER :: p_xs(:,:) ! 
    !! Tiefe f&uuml;r Landpunkte
    REAL (KIND=Double) , POINTER :: p_hl      ! 
    !! nicht weiter erodierbare Tiefe an den Kanten
    REAL (KIND=Double) , POINTER :: p_huu(:)  ! 
    !
    !! Hilfsvariable "Anzahl der Innenkanten"
    INTEGER                      :: l_nsi     ! 
    !! Hilfsvariable "erster Knoten einer Kante"
    INTEGER                      :: l_jb      ! 
    !! Hilfsvariable "zweiter Knoten einer Kante"
    INTEGER                      :: l_jt      ! 
    !! Hilfsvariable "Nachbarpolygon einer Kante"
    INTEGER                      :: l_je(2)   ! 
    !! Hilfsvariable "nicht weiter erodierbare Kantentiefe"
    REAL (KIND=Double)           :: l_huu     ! 
    !! Hilfsvariable "Kantentiefe"
    REAL (KIND=Double)           :: l_hu      ! 
    !! Hilfsvariable "Zentrumsabstand"
    REAL (KIND=Double)           :: l_dx      ! 
    !! Hilfsvariable "Kantenl&auml;nge"
    REAL (KIND=Double)           :: l_dy      ! 
    !! Hilfsvariable "Kantenzentrum"
    REAL (KIND=Double)           :: l_xs(2)   ! 
    !
    !! weitere Hilfsvariablen
    INTEGER                      :: i, j, k   ! 
    !
    ! [1.1] Sortierung der internen Kanten fuer das Gitter durchfuehren
    !
    ivar = get_h_grid_variant_no ( this )
    !
    SELECT CASE ( ivar )
    CASE(3,4) ! 'UNTRIM_VC,UNTRIM_BAW'
       NULLIFY ( p_nsi, p_je, p_is, p_jb, p_jt, p_hu, p_dx, p_dy, p_xs, p_hl, p_huu )
       ! Holen erforderlicher Daten
       p_is  => get_is_object    ( this ) ! 
       p_hu  => get_hu_object    ( this ) ! 
       p_huu => get_huu_object   ( this ) ! 
       p_hl  => get_hland_object ( this )
       p_dx  => get_dx_object    ( this )
       p_dy  => get_dy_object    ( this )
       p_xs  => get_xs_object    ( this )
       IF ( .NOT. ASSOCIATED( p_is ) ) THEN ! is, je, jb, jt
          CALL derive_is ( this )
          p_is => get_is_object ( this )
       END IF
       IF ( .NOT. ASSOCIATED( p_hu ) ) THEN
          CALL derive_hu ( this )
          p_hu => get_hu_object ( this )
       END IF
       IF ( .NOT. ASSOCIATED( p_dx ) ) THEN
          CALL derive_dx ( this )
          p_dx => get_dx_object ( this )
       END IF
       IF ( .NOT. ASSOCIATED( p_dy ) ) THEN
          CALL derive_dy ( this )
          p_dy => get_dy_object ( this )
       END IF
       IF ( .NOT. ASSOCIATED( p_xs ) ) THEN
          CALL derive_xs ( this )
          p_xs => get_xs_object ( this )
       END IF
       p_je => get_je_object ( this ) ! 
       p_jb => get_jb_object ( this ) ! 
       p_jt => get_jt_object ( this ) ! 
       !
       IF ( no_error ( ) ) THEN
          l_nsi = this%ns
          DO j=1,this%ns
             DO WHILE( ANY( p_je(l_nsi,:) < 1 ) .OR. p_hu(l_nsi) <= p_hl ) ! Randkante
                l_nsi = l_nsi - 1
             END DO
             IF ( j >= l_nsi                                ) EXIT  ! alle gefunden
             IF ( ALL( p_je(j,:) > 0 ) .AND. p_hu(j) > p_hl ) CYCLE ! Innenkante
             ! ... Kantenverzeichnis der Polygone anpassen (Kanten j <-> l_nsi tauschen )
             DO i=1,SIZE(p_is,2)
                DO k=1,SIZE(p_je,2)
                   IF ( p_je(j,k) > 0 ) THEN
                      IF ( p_is(p_je(j,k),i)     ==      j ) p_is(p_je(j,k),i) = -l_nsi
                   END IF
                END DO
             END DO
             DO i=1,SIZE(p_is,2)
                DO k=1,SIZE(p_je,2)
                   IF ( p_je(l_nsi,k) > 0 ) THEN
                      IF ( p_is(p_je(l_nsi,k),i) ==  l_nsi ) p_is(p_je(l_nsi,k),i) = j
                   END IF
                END DO
             END DO
             DO i=1,SIZE(p_is,2)
                DO k=1,SIZE(p_je,2)
                   IF ( p_je(j,k) > 0 ) THEN
                      IF ( p_is(p_je(j,k),i)     == -l_nsi ) p_is(p_je(j,k),i) = l_nsi
                   END IF
                END DO
             END DO
             ! WHERE( p_is(p_je(    j,:),:) ==      j ) p_is(p_je(    j,:),:) = -l_nsi
             ! WHERE( p_is(p_je(l_nsi,:),:) ==  l_nsi ) p_is(p_je(l_nsi,:),:) = j
             ! WHERE( p_is(p_je(    j,:),:) == -l_nsi ) p_is(p_je(    j,:),:) = l_nsi
             ! ... Kantentiefen anpassen
             l_hu = p_hu(j)      ; p_hu(j)   = p_hu(l_nsi)   ; p_hu(l_nsi)   = l_hu
             ! ... ersten Knoten einer Kante anpassen
             l_jb = p_jb(j)      ; p_jb(j)   = p_jb(l_nsi)   ; p_jb(l_nsi)   = l_jb
             ! ... zweiten Knoten einer Kante anpassen
             l_jt = p_jt(j)      ; p_jt(j)   = p_jt(l_nsi)   ; p_jt(l_nsi)   = l_jt
             ! ... Polygonverzeichnis der Kante anpassen
             l_je(:) = p_je(j,:) ; p_je(j,:) = p_je(l_nsi,:) ; p_je(l_nsi,:) = l_je(:)
             ! ... Abstand der Zentren anpassen
             l_dx = p_dx(j)      ; p_dx(j)   = p_dx(l_nsi)   ; p_dx(l_nsi)   = l_dx
             ! ... Laenge der Kanten anpassen
             l_dy = p_dy(j)      ; p_dy(j)   = p_dy(l_nsi)   ; p_dy(l_nsi)   = l_dy
             ! ... Zentrumskoordinaten der Kanten anpassen
             l_xs(:) = p_xs(j,:) ; p_xs(j,:) = p_xs(l_nsi,:) ; p_xs(l_nsi,:) = l_xs(:)
             ! ... (optional) nicht weiter erodierbare Kantentiefen anpassen
             IF ( ASSOCIATED(p_huu) ) THEN
                l_huu = p_huu(j)    ; p_huu(j)  = p_huu(l_nsi)  ; p_huu(l_nsi)  = l_huu
             END IF
          END DO
          ! ... NSI ermitteln
          CALL derive_nsi( this ) ; p_nsi => get_nsi_object ( this )
          ! ... NSF ermitteln
          CALL derive_nsf( this )
          ! ... Test auf Gleichheit
          IF ( l_nsi /= p_nsi ) THEN
             CALL setup_error_act ( all_errors(:), 26006, c_upname, c_modname, iostat )
             WRITE(ctxt,'(I10)') p_nsi ; CALL setup_error_act ( '<ActDeriveNsi>', ctxt )
             WRITE(ctxt,'(I10)') l_nsi ; CALL setup_error_act ( '<ActLocalNsi>' , ctxt )
          END IF
          IF ( no_error() .AND. prn_op ) THEN
             ! ... Schreiben der Informationen
             WRITE( prn_lun, 8000, IOSTAT=iostat )                                  &
                  this%ns,                                                          &
                  this%nsi            , REAL(100*this%nsi          )/REAL(this%ns), &
                  (this%ns - this%nsi), REAL(100*(this%ns-this%nsi))/REAL(this%ns)
             IF ( iostat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 26005, c_upname, c_modname, iostat )
             END IF
          END IF
          NULLIFY ( p_nsi, p_je, p_is, p_jb, p_jt, p_hu, p_dx, p_dy, p_xs, p_hl, p_huu )
       END IF
       !
    CASE DEFAULT
       WRITE(*,*) ivar
       CALL setup_error_act ( all_errors(:), 26200, c_upname, c_modname )
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I10)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
    ! [2.1] Formatanweisungen
    !
8000 FORMAT ( &
          '# Informationen zur Sortierung des Gitters nach internen Kanten ---',/ &
          '# Anzahl der Kanten      = ',I10,/ &
          '# ... davon Innenkanten  = ',I10,', entspricht ',F7.4,' Prozent',/ &
          '# ... davon Aussenkanten = ',I10,', entspricht ',F7.4,' Prozent',/ &
          '# -----------------------------------------------------------------' )
    ! 
  END SUBROUTINE convert_untrim_nsi_d
  !
  !! passe die Tiefen des Gitters so an, dass tote Volumina in 3D-Anwendungen
  !! nicht mehr auftreten koennen <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE convert_untrim_terrace_d ( this )
    !! aktuelles Arbeitsobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=24), PARAMETER   :: c_upname='convert_untrim_terrace_d' ! 
    !
    !! Hilfsfeld f&uuml;r Fehlertexte
    CHARACTER (LEN=10) :: ctxt ! 
    !! Hilfsvariable Dateivarianten-Nummer
    INTEGER :: ivar ! 
    !! Anzahl der Innenkanten
    INTEGER            , POINTER :: p_nsi       ! 
    !! Nummer der letzten Kante mit Fluss-Randbedingung
    INTEGER            , POINTER :: p_nsf       ! 
    !! Anzahl der Knoten des Gitters
    INTEGER            , POINTER :: p_nv        ! 
    !! Tiefen auf den Kanten
    REAL (KIND=Double) , POINTER :: p_hu(:)     ! 
    !! erster Knoten einer Kante
    INTEGER            , POINTER :: p_jb(:)     ! 
    !! zweiter Knoten einer Kante
    INTEGER            , POINTER :: p_jt(:)     ! 
    !! Tiefen an den Knoten (lokal)
    REAL (KIND=Double) , ALLOCATABLE :: l_hv(:) ! 
    !! Z&auml;hlervariable
    INTEGER                      :: j ! 
    !
    ! [1.1] Sortierung der internen Kanten fuer das Gitter durchfuehren
    !
    ivar = get_h_grid_variant_no ( this )
    !
    SELECT CASE ( ivar )
    CASE(3,4) ! 'UNTRIM_VC,UNTRIM_BAW'
       NULLIFY ( p_nsi, p_nsf, p_nv, p_hu, p_jb, p_jt )
       ! Holen erforderlicher Daten
       p_nsi => get_nsi_object ( this )
       p_nsf => get_nsf_object ( this )
       p_nv  => get_nv_object  ( this )
       p_hu  => get_hu_object  ( this )
       p_jb  => get_jb_object  ( this )
       p_jt  => get_jt_object  ( this )
       IF ( .NOT. ASSOCIATED( p_nsi ) ) THEN
          CALL convert_untrim_nsi( this )
          CALL derive_nsi( this )
          p_nsi => get_nsi_object ( this )
       END IF
       IF ( p_nsi == 0 ) THEN
          CALL convert_untrim_nsi( this )
          CALL derive_nsi( this )
          p_nsi => get_nsi_object ( this )
       END IF
       IF ( .NOT. ASSOCIATED( p_nsf ) ) THEN
          CALL derive_nsf( this )
          p_nsf => get_nsf_object ( this )
       END IF
       p_nsf = MAX( p_nsi, p_nsf )
       ALLOCATE( l_hv(p_nv) )
       l_hv(:) = MAXVAL( p_hu )
       DO j=1,p_nsf
          l_hv(p_jb(j)) = MIN( l_hv(p_jb(j)), p_hu(j) )
          l_hv(p_jt(j)) = MIN( l_hv(p_jt(j)), p_hu(j) )
       END DO
       DO j=1,p_nsf
          p_hu(j) = MAX( l_hv(p_jb(j)), l_hv(p_jt(j)) )
       END DO
       !
       DEALLOCATE( l_hv )
       NULLIFY ( p_nsi, p_nsf, p_nv, p_hu, p_jb, p_jt )
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), 26300, c_upname, c_modname )
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I10)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
  END SUBROUTINE convert_untrim_terrace_d
  !
  !! Verfeinere ein UnTRIM-Gitternetz durch Vierteilung <BR>
  !! Unterprogramm erzeugt Fehlermeldungen
  SUBROUTINE refine_d ( this )
    !! aktuelles Arbeitsobjekt
    TYPE (t_h_grid)    , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=8) , PARAMETER   :: c_upname='refine_d' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ctxt  ! 
    INTEGER :: ivar             ! 
    CHARACTER (LEN=80) , POINTER     :: p_text(:)                                                   ! 
    INTEGER            , POINTER     :: p_nv, p_ne, p_ns, p_nsi, p_nsf, p_nbc                       ! 
    INTEGER            , POINTER     :: p_jb(:), p_jt(:), p_ks(:), p_is(:,:), p_ie(:,:), p_nen(:,:) ! 
    REAL (KIND=Double) , POINTER     :: p_hland, p_xy(:,:), p_hu(:), p_hw(:), p_huu(:)              ! 
    INTEGER                          :: l_nv, l_ne, l_ns, l_nsi, l_nsf, l_nbc                       ! 
    CHARACTER (LEN=80) , ALLOCATABLE :: l_text(:)                                                   ! 
    INTEGER            , ALLOCATABLE :: l_ks(:), l_is(:,:), l_nen(:,:), m_is(:), m_nen(:), m_ie(:)  ! 
    REAL (KIND=Double) , ALLOCATABLE :: l_xy(:,:), l_hu(:), l_huu(:)                                ! 
    INTEGER :: i, ii, iadd, j, jj, jm, jn, kn, kk, mm, ne, ne3, ne4, nn, n3, n4, m_ks, dwlp_code    ! 
    REAL (KIND=Double) :: xym(2), hu, huu, mean_hu, max_hu, min_hu, max_huu ! 
    !
    NULLIFY( p_nv, p_ne , p_ns, p_nsi, p_nsf, p_nbc, p_jb, p_jt, p_ks, p_is, &
             p_ie, p_nen, p_xy, p_hu , p_hw, p_hland, p_text, p_huu )
    ivar = get_h_grid_variant_no ( this )
    SELECT CASE ( ivar )
    CASE(3,4) ! 'UNTRIM_VC,UNTRIM_BAW'
       ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
       ! Informationen aus dem bestehenden Objekt extrahieren
       ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
       dwlp_code = get_dwlp_code_object ( this )
       p_nv  => get_nv_object  ( this )
       p_ne  => get_ne_object  ( this )
       p_ns  => get_ns_object  ( this )
       p_nsi => get_nsi_object ( this )
       IF ( .NOT. ASSOCIATED( p_nsi ) ) THEN
          CALL derive_nsi ( this )
          p_nsi =>  get_nsi_object ( this )
       END IF
       p_nsf => get_nsf_object ( this )
       IF ( .NOT. ASSOCIATED( p_nsf ) ) THEN
          CALL derive_nsf ( this )
          p_nsf => get_nsf_object ( this )
       END IF
       p_nbc => get_nbc_object ( this )
       p_ks  => get_ks_object  ( this )
       p_is  => get_is_object  ( this )
       IF ( .NOT. ASSOCIATED( p_is ) ) THEN
          CALL derive_is ( this )
          p_is  => get_is_object  ( this )
       END IF
       p_nen => get_nen_object ( this )
       p_ie  => get_ie_object  ( this )
       IF ( .NOT. ASSOCIATED( p_ie ) ) THEN
          CALL derive_ie ( this )
          p_ie  => get_ie_object  ( this )
       END IF
       p_jb    => get_jb_object    ( this )
       p_jt    => get_jt_object    ( this )
       p_hland => get_hland_object ( this )
       p_xy    => get_xy_object    ( this )
       p_hu    => get_hu_object    ( this )
       p_hw    => get_hw_object    ( this )
       IF ( .NOT. ASSOCIATED( p_hw ) ) THEN
          CALL derive_hw ( this )
          p_hw    => get_hw_object    ( this )
       END IF
       p_text  => get_text_object  ( this )
       p_huu   => get_huu_object   ( this )
       ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
       ! Neuberechnung verschiedener Daten (Anzahl der Knoten, ... )
       ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
       ne3     = COUNT( p_ks(:) == 3 )
       ne4     = COUNT( p_ks(:) == 4 )
       l_nv  =   p_nv  + p_ns  + ne4
       l_ne  = 4*p_ne
       l_ns  = 2*p_ns  + 3*ne3 + 4*ne4 
       l_nsi = 2*p_nsi + 3*ne3 + 4*ne4
       l_nsf = l_nsi   + 2*( p_nsf - p_nsi )
       l_nbc = 0
       DO i=1,p_nbc
          nn    = COUNT( p_hu(p_is(i,1:p_ks(i))) > p_hland .AND. p_ie(i,1:p_ks(i)) <= 0 ) ! Anzahl der Randkanten
          l_nbc = MERGE( l_nbc + nn + 1 , l_nbc, nn > 0 )
       END DO
       ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
       ! Allokieren der Felder fuer das verfeinerte Gitternetz
       ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
       ALLOCATE( l_ks(l_ne), l_is(l_ne,SIZE(p_is,2)), l_nen(l_ne,SIZE(p_nen,2)))
       ALLOCATE( l_xy(l_nv,SIZE(p_xy,2)), l_hu(l_ns) )
       l_ks(:)   = 0          ; l_is(:,:)  = 0          ; l_nen(:,:) = 0
       l_xy(:,:) = 0.0_Double ; l_nen(:,:) = 0.0_Double
       IF ( ASSOCIATED(p_huu) ) THEN
          ALLOCATE( l_huu(l_ns) )
          l_huu(:) = 11022.0_Double
       END IF
       ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       ! Zerlege zunaechst die Elemente ohne Beachtung von NBC
       ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       ne = 0 ; n3 = 0 ; n4 = 0
       DO i=1,p_ne
          n3      = MERGE( n3+1, n3, p_ks(i) == 3 )
          n4      = MERGE( n4+1, n4, p_ks(i) == 4 )
          nn      = 0
          mean_hu = 0.0_Double
          max_hu  = -10001.0_Double
          min_hu  =  11022.0_Double
          max_huu = -10001.0_Double
          DO j=1,p_ks(i)
             IF ( p_hu(p_is(i,j)) > p_hland ) THEN
                nn      = nn + 1
                mean_hu = mean_hu + p_hu(p_is(i,j))
                max_hu  = MAX(max_hu,p_hu(p_is(i,j)))
                min_hu  = MIN(min_hu,p_hu(p_is(i,j)))
                IF ( ALLOCATED(l_huu) ) max_huu = MAX(max_huu,p_huu(p_is(i,j)))
             END IF
          END DO
          IF ( nn > 0 ) THEN
             mean_hu = mean_hu / nn
          ELSE
             mean_hu = p_hland - 1.0_Double
             max_hu  = mean_hu
             min_hu  = mean_hu
          END IF
          DO j=1,p_ks(i)
             jm = MERGE( p_ks(i), j-1, j == 1       )
             ne = ne + 1
             l_ks(ne) = p_ks(i)
             DO jj=1,l_ks(ne)
                ! ... neue Knotennummer "kn" berechnen
                SELECT CASE ( jj )
                CASE ( 1 )
                   kn = p_nen(i,j)
                CASE ( 2 )
                   kn = p_nv + p_is(i,j)
                CASE ( 3 )
                   IF ( p_ks(i) == 4 ) THEN
                      kn = p_nv + p_ns + n4
                   ELSE
                      kn = p_nv + p_is(i,jm)
                   END IF
                CASE ( 4 )
                   kn = p_nv + p_is(i,jm)
                END SELECT
                ! ... neue Kantennummer "jn" berechnen
                SELECT CASE ( jj )
                CASE ( 1 )
                   iadd = MERGE( 1, 2, p_nen(i,j) == p_jb(p_is(i,j)) )
                   IF ( p_is(i,j) > p_nsi ) THEN
                      jn = 2*p_nsi + 3*ne3 + 4*ne4 + 2*(p_is(i,j)-p_nsi-1) + iadd
                   ELSE
                      jn = 2*(p_is(i,j)-1) + iadd
                   END IF
                   hu = p_hu(p_is(i,j))
                   IF ( ALLOCATED(l_huu) ) huu = p_huu(p_is(i,j))
                CASE ( 2 )
                   IF ( p_ks(i) == 4 ) THEN
                      jn = 2*p_nsi + 3*ne3 + 4*(n4-1) + j
                   ELSE
                      jn = 2*p_nsi + 3*(n3-1) + j
                   END IF
                   SELECT CASE ( dwlp_code )
                   CASE ( 1 ) ; hu = mean_hu
                   CASE ( 2 ) ; hu = max_hu
                   CASE ( 3 ) ; hu = min_hu
                   END SELECT
                   huu = max_huu
                CASE ( 3 )
                   IF ( p_ks(i) == 4 ) THEN
                      jn = 2*p_nsi + 3*ne3 + 4*(n4-1) + jm
                      SELECT CASE ( dwlp_code )
                      CASE ( 1 ) ; hu = mean_hu
                      CASE ( 2 ) ; hu = max_hu
                      CASE ( 3 ) ; hu = min_hu
                      END SELECT
                      huu = max_huu
                   ELSE
                      iadd = MERGE( 1, 2, p_nen(i,j) == p_jb(p_is(i,jm)) )
                      IF ( p_is(i,jm) > p_nsi ) THEN
                         jn = 2*p_nsi + 3*ne3 + 4*ne4 + 2*(p_is(i,jm)-p_nsi-1) + iadd
                      ELSE
                         jn = 2*(p_is(i,jm)-1) + iadd
                      END IF
                      hu = p_hu(p_is(i,jm))
                      IF ( ALLOCATED(l_huu) ) huu = p_huu(p_is(i,jm))
                   END IF
                CASE ( 4 )
                   iadd = MERGE( 1, 2, p_nen(i,j) == p_jb(p_is(i,jm)) )
                   IF ( p_is(i,jm) > p_nsi ) THEN
                      jn = 2*p_nsi + 3*ne3 + 4*ne4 + 2*(p_is(i,jm)-p_nsi-1) + iadd
                   ELSE
                      jn = 2*(p_is(i,jm)-1) + iadd
                   END IF
                   hu = p_hu(p_is(i,jm))
                   IF ( ALLOCATED(l_huu) ) huu = p_huu(p_is(i,jm))
                END SELECT
                l_nen(ne,jj) = kn
                l_is(ne,jj)  = jn 
                l_hu(jn)     = hu
                IF ( ALLOCATED(l_huu) ) l_huu(jn) = huu
             END DO
          END DO
          IF ( p_ks(i) == 3 ) THEN
             ne       = ne + 1
             l_ks(ne) = p_ks(i)
             DO j=1,3
                l_nen(ne,j) = l_nen(ne-4+j,3)
                l_is(ne,j)  = l_is(ne-4+j,2)
             END DO
          END IF
       END DO
       ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       ! Berechne jetzt die x- und y-Koordinaten der neuen Punkte
       ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       n3 = 0 ; n4 = 0
       DO i=1,p_ne
          xym(:) = 0.0_Double
          n3     = MERGE( n3+1, n3, p_ks(i) == 3 )
          n4     = MERGE( n4+1, n4, p_ks(i) == 4 )
          DO j=1,p_ks(i)
             kn         = p_nen(i,j)       ! Punkte die identisch mit alten Punkten sind
             l_xy(kn,:) = p_xy(kn,:)
             xym(:)     = xym(:) + p_xy(kn,:)
             kn         = p_nv + p_is(i,j) ! Punkte in den alten Kantenmitten
             l_xy(kn,:) = 0.5_Double*(p_xy(p_jb(p_is(i,j)),:)+p_xy(p_jt(p_is(i,j)),:))
             IF ( j == 4 ) THEN
                xym(:) = xym(:) / REAL(p_ks(i))
                kn     = p_nv + p_ns + n4 ! Punkte in der Mitte von Vierecken
                l_xy(kn,:) = xym(:)
             END IF
          END DO
       END DO
       ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       ! Setze die Groessen in dem Arbeitsobjekt um bzw. definiere sie neu
       ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       IF ( ASSOCIATED( p_text ) ) THEN
          ALLOCATE ( l_text(SIZE(p_text)+1) )
          l_text(:) = REPEAT( ' ', LEN(l_text) )
          l_text(1:SIZE(p_text)) = p_text(:)
       ELSE
          ALLOCATE ( l_text(1) )
          l_text(:) = REPEAT( ' ', LEN(l_text) )
       END IF
       l_text(SIZE(l_text)) = ' Gitternetz wurde mit "refine" automatisch verfeinert '
       ! ... fuer die Re-Initialisierung vorbereiten
       p_nv = -1
       p_ne = -1
       p_ns = -1
       ! ... Aufraeumen
       NULLIFY( p_nv, p_ne , p_ns, p_nsi, p_nsf, p_nbc, p_jb, p_jt, p_ks, p_is, &
            p_ie, p_nen, p_xy, p_hu , p_hw, p_hland, p_text )
       ! ... zentrale Werte neu setzen
       CALL setup_ks_object    ( this, l_ks  )
       CALL setup_is_object    ( this, l_is  )
       CALL setup_nen_object   ( this, l_nen )
       CALL setup_xy_object    ( this, l_xy  )
       CALL setup_hu_object    ( this, l_hu  )
       CALL setup_text_object  ( this, l_text )
       CALL setup_nbc_object   ( this, l_nbc  )
       CALL setup_nsi_object   ( this, l_nsi  )
       CALL setup_nsf_object   ( this, l_nsf  )
       CALL setup_nr_object    ( this, l_ne   )
       IF ( ALLOCATED(l_huu) ) CALL setup_huu_object( this, l_huu )
       ! ... obsolete Werte entfernen
       CALL dealloc_h_grid_hv ( this )
       CALL dealloc_h_grid_jb ( this )
       CALL dealloc_h_grid_jt ( this )
       CALL dealloc_h_grid_je ( this )
       CALL dealloc_h_grid_ie ( this )
       CALL dealloc_h_grid_xs ( this )
       CALL dealloc_h_grid_xc ( this )
       CALL dealloc_h_grid_xg ( this )
       CALL dealloc_h_grid_dx ( this )
       CALL dealloc_h_grid_dy ( this )
       CALL dealloc_h_grid_aa ( this )
       CALL dealloc_h_grid_hw ( this )
       ! ... fehlende Werte nachberechnen
       CALL derive_ie ( this )
       ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       ! Sortiere jetzt die Randelemente nach vorne 1 ... l_nbc
       ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       p_ne    => get_ne_object    ( this )
       p_ks    => get_ks_object    ( this )
       p_nen   => get_nen_object   ( this )
       p_is    => get_is_object    ( this )
       p_ie    => get_ie_object    ( this )
       p_hu    => get_hu_object    ( this )
       p_nbc   => get_nbc_object   ( this )
       p_hland => get_hland_object ( this )
       ALLOCATE ( m_is(SIZE(p_is,2)), m_nen(SIZE(p_nen,2)), m_ie(SIZE(p_ie,2)) )
       DO i=1,p_nbc
          nn = COUNT( p_hu(p_is(i,1:p_ks(i))) > p_hland .AND. p_ie(i,1:p_ks(i)) <= 0 ) !
          IF ( nn == 0 ) THEN
             DO ii=p_nbc+1,p_ne
                mm = COUNT( p_hu(p_is(ii,1:p_ks(ii))) > p_hland .AND. p_ie(ii,1:p_ks(ii)) <= 0 ) !
                IF ( mm == 0 ) CYCLE
                m_ks        = p_ks(ii)
                m_nen(:)    = p_nen(ii,:)
                m_is(:)     = p_is(ii,:)
                m_ie(:)     = p_ie(ii,:)
                p_ks(ii)    = p_ks(i) 
                p_nen(ii,:) = p_nen(i,:)
                p_is(ii,:)  = p_is(i,:)
                p_ie(ii,:)  = p_ie(i,:)
                EXIT
             END DO
             p_ks(i)    = m_ks
             p_nen(i,:) = m_nen(:)
             p_is(i,:)  = m_is(:)
             p_ie(i,:)  = m_ie(:)
          END IF
       END DO
       ! ... obsolete Werte entfernen
       CALL dealloc_h_grid_jb ( this )
       CALL dealloc_h_grid_jt ( this )
       CALL dealloc_h_grid_je ( this )
       CALL dealloc_h_grid_ie ( this )
       ! ...
       CALL derive_jb ( this )
       CALL derive_jt ( this )
       CALL derive_je ( this )
       CALL derive_ie ( this )
       CALL derive_hv ( this )
       CALL derive_hw ( this )
       CALL derive_xc ( this )
       CALL derive_dx ( this )
       CALL derive_dy ( this )
       IF ( ALLOCATED(l_huu) ) CALL derive_hvu( this )
       ! ... Aufraeumen
       NULLIFY( p_nv, p_ne , p_ns, p_nsi, p_nsf, p_nbc, p_jb, p_jt, p_ks, p_is, &
            p_ie, p_nen, p_xy, p_hu , p_hw, p_hland, p_text, p_huu )
       DEALLOCATE( l_ks, l_is, l_nen, l_xy, l_hu )
       IF ( ALLOCATED( l_huu ) ) DEALLOCATE( l_huu )
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), 26400, c_upname, c_modname )
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I10)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
  END SUBROUTINE refine_d
  !
  !! Vertiefen der Bathymetrie eines Gitters <BR>
  !! Unterprogramm erzeugt Fehlermeldungen
  SUBROUTINE deepen_d ( this, dz )
    !! aktuelles Arbeitsobjekt
    TYPE (t_h_grid)    , POINTER :: this ! 
    !! Vertiefungsbetrag (positiv nach Unten)
    REAL (KIND=Double) , INTENT(IN) :: dz ! 
    !! Name der Subroutine
    CHARACTER (LEN=8) , PARAMETER   :: c_upname='deepen_d' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ctxt  ! 
    INTEGER :: ivar             ! 
    REAL (KIND=Double) , POINTER :: p_hland                      ! 
    REAL (KIND=Double) , POINTER :: p_hu(:) , p_hv(:) , p_hw(:)  ! 
    REAL (KIND=Double) , POINTER :: p_huu(:), p_hvu(:), p_hwu(:) ! 
    !
    NULLIFY(p_hu,p_hv,p_hw,p_huu,p_hvu,p_hwu)
    !
    ivar = get_h_grid_variant_no ( this )
    SELECT CASE ( ivar )
    CASE(1,2,5,6) ! 'GITTER05[DAT|BIN],SELAFIN,DELFT3D'
       p_hv  => get_hv_object ( this )
       p_hvu => get_hvu_object( this )
       ! ... Tiefen an den Knoten
       IF ( ASSOCIATED(p_hv) ) THEN
          IF ( ASSOCIATED(p_hvu) ) THEN
             p_hv = MIN( p_hv+dz, p_hvu )
          ELSE
             p_hv = p_hv+dz
          END IF
       END IF
    CASE(3,4) ! 'UNTRIM_VC,UNTRIM_BAW'
       p_hland => get_hland_object( this )
       p_hu    => get_hu_object   ( this )
       p_huu   => get_huu_object  ( this )
       ! ... Tiefen an den Kanten
       IF ( ASSOCIATED(p_hu) .AND. ASSOCIATED(p_hland) ) THEN
          IF ( ASSOCIATED(p_huu) ) THEN
             WHERE (p_hu > p_hland) p_hu = MIN( p_hu+dz, p_huu )
             write(*,*) ' ... deepen HU using HUU'
          ELSE
             WHERE (p_hu > p_hland) p_hu = p_hu+dz
             write(*,*) ' ... deepen HU'
          END IF
       END IF
       ! ... Tiefen in den Polygonen aus Kantentiefen ableiten
       CALL derive_hw ( this )
       p_hw  => get_hw_object ( this )
       p_hwu => get_hwu_object( this )
       IF ( ASSOCIATED(p_hw) .AND. ASSOCIATED(p_hwu) ) THEN
          WHERE (p_hw > p_hland) p_hw = MIN( p_hw, p_hwu )
          write(*,*) ' ... deepen HW using HWU'
       END IF
       ! ... Tiefen an den Knoten
       CALL derive_hv ( this )
       p_hv  => get_hv_object ( this )
       p_hvu => get_hvu_object( this )
       IF ( ASSOCIATED(p_hv) .AND. ASSOCIATED(p_hvu) ) THEN
          WHERE (p_hv > p_hland) p_hv = MIN( p_hv, p_hvu )
          write(*,*) ' ... deepen HV using HVU'
       END IF
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), 26500, c_upname, c_modname )
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I10)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
    NULLIFY(p_hu,p_hv,p_hw,p_huu,p_hvu,p_hwu)
    !
  END SUBROUTINE deepen_d
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
  !! Pr&uuml;fe, ob zwei Knoten des Randes benachbart sind. <BR>
  !! Function erzeugt keine Fehlermeldungen
  FUNCTION neighboured_boundary_nodes &
       ( node_a, node_b, p_nrand ) &
       RESULT( l_neighboured )
    !
    ! Formalparameter
    !! Eckknoten der Kante
    INTEGER :: node_a, node_b
    !! letzter Knoten auf dem Rand, benachbart von p_nrand-1 und 1
    INTEGER :: p_nrand
    ! Rueckgabewert
    !! Testergebnis
    LOGICAL :: l_neighboured 
    !
    l_neighboured = .false.
    !
    ! wenn die Knoten A und B im Knotenverzeichnis benachbart sind: 
    IF ( ABS(node_a-node_b) == 1) THEN
       ! -> Kante zwischen den Knoten liegt auf dem Rand
       l_neighboured = .true.
       ! erster und letzter Randknoten
    ELSEIF ( ( node_a==1 .AND. node_b==p_nrand ) .OR. &
             ( node_b==1 .AND. node_a==p_nrand )   ) THEN
       ! -> Kante zwischen den Knoten liegt auf dem Rand
       l_neighboured = .true.
    ENDIF
    !
  END FUNCTION neighboured_boundary_nodes
  !! zwei Knoten miteinander vertauschen <BR>
  !! und die Felder p_xy, p_hv und p_nen aktualisieren
  !! Subroutine erzeugt keine Fehlermeldungen
  SUBROUTINE node_swap &
       ( this, &
       i1kno, i2kno)
    !
    !! aktuelles Arbeitsobjekt
    TYPE (t_h_grid) , POINTER :: this 
    !! Nummer des ersten zu tauschenden Knotens
    INTEGER, INTENT(IN) :: i1kno
    !! Nummer des zweiten zu tauschenden Knotens
    INTEGER, INTENT(IN) :: i2kno
    !
    !! Name der Subroutine
    CHARACTER (LEN=9), PARAMETER   :: c_upname='node_swap' ! 
    INTEGER :: iele, i
    !! Knotenverzeichnis der Polygone
    INTEGER , POINTER :: p_nen(:,:)
    !! Anzahl der Knoten im Polygon
    INTEGER, PARAMETER :: ks = 3
    !! Knotenkoordinaten
    REAL (KIND=Double), POINTER :: p_xy(:,:)
    !! Knotentiefen
    REAL (KIND=Double), POINTER :: p_hv(:)
    !! 
    REAL (KIND=Double) :: x_tmp, y_tmp, hv_tmp
    !
    ! [1.1] nichts tun
    !
    IF( i1kno == i2kno ) THEN

       RETURN

    ELSE
       !
       ! [1.2] Knotenkoordinaten und -tiefen tauschen
       p_xy => get_xy_object ( this ) 
       p_hv => get_hv_object ( this ) 
       p_nen => get_nen_object ( this ) 
       !
       x_tmp  = p_xy(i1kno,1)
       y_tmp  = p_xy(i1kno,2)
       hv_tmp = p_hv(i1kno)  
       !
       p_xy(i1kno,1) = p_xy(i2kno,1)
       p_xy(i1kno,2) = p_xy(i2kno,2)
       p_hv(i1kno)   = p_hv(i2kno)  
       !
       p_xy(i2kno,1) = x_tmp
       p_xy(i2kno,2) = y_tmp
       p_hv(i2kno)   = hv_tmp
       !
       ! [1.3] Knotennummern des Polygonverzeichnisses tauschen
       DO iele=1,get_ne_object(this)
          !
          DO i=1,ks
             !
             IF( p_nen(iele,i) == i1kno ) THEN
                
                p_nen(iele,i) = i2kno
                
             ELSEIF( p_nen(iele,i) == i2kno ) THEN
                
                p_nen(iele,i) = i1kno

             ENDIF

          ENDDO

       ENDDO

    ENDIF

  END SUBROUTINE node_swap
  !
  !! Pr&uuml;fe ob eine Dreieckskante offen ist <BR>
  !! Function erzeugt keine Fehlermeldungen
  FUNCTION edge_open &
       ( edge, irand ) &
       RESULT( l_open )
    !
    ! Formalparameter
    !! Kennung der Kante innerhalb des Dreiecks
    !! edge = 2 : Kante liegt gegenueber Knoten 1
    !! edge = 3 : Kante liegt gegenueber Knoten 2
    !! edge = 1 : Kante liegt gegenueber Knoten 3
    INTEGER :: edge
    !! Randkennungskode f&uuml;r Polygone (Ticad)
    INTEGER :: irand
    ! Rueckgabewert
    !! .true.  : Kante ist offen
    !! .false. : Kante ist geschlossen
    LOGICAL :: l_open 
    !
    l_open = .false.
    !
    ! alle drei Kanten sind offen
    IF (irand == 0 ) THEN
       l_open = .true.
       ! 
    ELSE
       !
       SELECT CASE (edge)
          ! einige Kanten sind offen
       CASE (2)
          IF ( irand == 2 .OR. &  
               irand == 4 .OR. &  
               irand == 6 )     l_open = .true.
       CASE (3)
          IF ( irand == 1 .OR. &  
               irand == 4 .OR. &  
               irand == 5 )     l_open = .true.
       CASE (1)
          IF ( irand == 1 .OR. &  
               irand == 2 .OR. &  
               irand == 3 )     l_open = .true.
       END SELECT
       !
    ENDIF
    !
  END FUNCTION edge_open
  !
END MODULE m_h_grid_convert
! TailOfPackageUserInterface -----------------------------------------------
