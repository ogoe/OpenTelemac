! -------------------------------------------------------------------------
! HeadOfPackageModule -----------------------------------------------------
!
!! <H2>Berechnen fehlender Gitterdaten aus bekannten Werten</h2>
!! @author <A HREF="mailto:lang@hamburg.baw.de">G. Lang</A> und 
!! <A HREF="mailto:schade@hamburg.baw.de">P.Schade</A>
!! @version 4.10 vom 03/01/07, Quellcode: mod_m_h_grid_derive.f90
!! <HR>
!! compute unknown from known grid data                              <BR>
!!                                                                  <BR>
!! conversion of grid data between different formats <BR>
!
!  Copyright-Hinweis
!
!  Copyright (C) 2001 Bundesanstalt fuer Wasserbau
!
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2002-07-18 : G. Lang    : Startversion 
!  01.02 : 2002/07/24 : G. Lang    : verschiedene Methoden ergaenzt (GL, 2002-Juli-24)
!  01.03 : 2002/07/25 : P. Schade  : derive_aa und derive_xc
!  01.04 : 2002/07/25 : P. Schade  : Fehlermeldung 25004 und max_dist
!  01.05 : 2002/07/26 : G. Lang    : Berechnung "dx" wird "dxmin" beruecksichtigt
!  01.06 : 2002/09/05 : G. Lang    : Felder in Pointer in Functions ohne (:) oder (:,:) angegeben
!  01.07 : 2002/09/06 : G. Lang    : derive_hv (Tiefe an den Knoten)
!  01.08 : 2002/10/02 : J. Juerges : Komponente xs (Mitten-Koordinaten der Kanten) hinzugefuegt
!  01.09 : 2003-03-20 : P. Schade  : Aenderungen am Header, z.B. Copyright
!  01.10 : 2004-02-25 : G. Lang    : Berechnen der Anzahl der internen Kanten NSI
!  01.11 : 2004-03-17 : G. Lang    : HV optional aus HU ableiten
!  01.12 : 2004/03/17 : G. Lang    : derive_hv wurde erweitert, falls hu(:) bekannt ist
!  01.13 : 2004/04/26 : G. Lang    : Methode zum Ermitteln der Knotentiefe an UTR angepasst
!  01.14 : 2004-05-27 : J. Juerges : XG (Polygon-Schwerpunktkoordinaten) ableiten
!  01.15 : 2004-06-25 : G. Lang    : Berechnung von NSF
!  01.16 : 2005/01/11 : S. Spohr   : IF derive_ns, IF derive_nrand und SUB derive_nrand_d
!  02.01 : 2005-03-07 : G. Lang    : Erweiterungen OpenMI (unvollstaendig)
!  02.02 : 2005-03-10 : G. Lang    : div. Erweiterungen fuer Konfigurationsphase OpenMI
!  02.03 : 2005-03-16 : G. Lang    : Erweiterungen fuer ALLBoundaries,AllOpenBoundaries und AllClosedBoundaries 
!  03.01 : 2005-07-21 : G. Lang    : Anpassungen/Erweiterungen fuer mod. OpenMI-ElementSet-Verstaendnis
!  04.01 : 2005-08-22 : G. Lang    : Lesen/Schreiben Delft3D-Gitternetz
!  04.02 : 2005-11-13 : G. Lang    : isbnd(:,:) fuer Delft3D-Gitternetz ableiten
!  04.03 : 2005-11-23 : G. Lang    : Erweiterungen *.thd, *.lwl, *.ext
!  04.04 : 2005-11-23 : G. Lang    : in derive_nsf wurde das Umsortieren der U-Tiefen nachtraeglich integriert
!  04.05 : 2005-12-02 : G. Lang    : derive_hv, Kantentiefen nur falls nicht Land beruecksichtigen
!  04.06 : 2005-12-28 : G. Lang    : derive_nsf ueberarbeitet; derive_jb|jt|je|ie optional ohne Rueckgriff auf derive_is
!  04.07 : 2006-03-03 : G. Lang    : Warnungen bei Kontrollberechnung in derive_xc_d nur in trc_lun ausgeben
!  04.08 : 2006-04-13 : G. Lang    : optionale unerodierbare Tiefen huu(:), hvu(:) und hwu(:)
!  04.09 : 2007-02-05 : G. Lang    : dg(:) berechnen; Mod. fuer je(j,1) > 0 (derive_jb,derive_jt,derive_is); derive_nrand modifizieren
!  04.10 : 2007-03-01 : G. Lang    : Korrekturen in "derive_b_s_d" wegen Schleifen in inneren Raendern
!
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! Das Modul stellt verschiedene Methoden zum Berechnen noch nicht
!! bekannter Gittergr&ouml;&szlig;en aus schon vorhandenen zur 
!! Verf&uuml;gung. Im Einzelnen z&auml;hlen hierzu Berechnungsmethoden
!! f&uuml;r
!! <OL>
!!   <LI> Polygonfl&auml;chen, 
!!   <LI> Mittenkoordinaten der Kanten,
!!   <LI> Zentrumskoordinaten der Polygone,
!!   <LI> Schwerpunktkoordinaten der Polygone,
!!   <LI> Kantenverzeichnis der Polygone,
!!   <LI> Polygonverzeichnis der Kanten,
!!   <LI> Startknoten der Kanten,
!!   <LI> Endknoten der Kanten,
!!   <LI> Tiefe auf den Kanten,
!!   <LI> Tiefe in den Polygonen,
!!   <LI> Polygonverzeichnis der Polygone,
!!   <LI> Abstand zwischen benachbarten Zentren,
!!   <LI> L&auml;nge der Kanten.
!!   <LI> Anzahl der internen Kanten
!!   <LI> ...
!! </OL>
!! <HR>
!! <H3>Gebrauch der Methoden dieses Moduls</H3>
!! Die in diesem Modul bereitgestellten Methoden d&uuml;rfen nur
!! von zu dem Paket "h_grid" geh&ouml;renden Modulen direkt 
!! verwendet werden. Insbesondere ist das direkte Verwenden von
!! Methoden aus anderen Paketen unzul&auml;ssig. 
!
MODULE m_h_grid_derive
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] Basis-Modul mit globalen Konstantwerten
  !
  USE b_constants, ONLY : &
       ! Parameter
       Double, &
       Byte
  !
  ! [A.2] Basis-Modul "Fehler"
  !
  USE b_error, ONLY :       &
       ! Routinen
       no_error,            &
       any_error,           &
       setup_error_act
  ! 
  ! [A.3] Basis-Modul "2D-Punkte"
  USE b_point_2d, ONLY : &
       ! Datentyp
       t_point_2d,       &
       ! Routinen / Interfaces
       new_point_2d,     &
       kill_point_2d,    &
       set_point_2d_x,   &
       set_point_2d_y,   &
       inside_point_2d
  !
  ! ----------------------------------------------------------------------
  ! [B] gemeinsam genutzte Daten des Paketes "h_grid"
  ! ----------------------------------------------------------------------
  !
  ! [B.1] Daten-Modul des Paketes "h_grid"
  !
  USE m_h_grid_data, ONLY :                                                               &
       ! Typdefinition
       t_h_grid, t_d3d_openbc, t_d3d_thd, t_d3d_weir,                                     &
       ! Parameter und Konstantwerte
       c_variants_type, all_errors, prn_op, prn_lun, trc_op, trc_lun,                     &
       ! Routinen / Interfaces
       get_h_grid_variant_no,                                                             &
       get_nv_object, get_ne_object, get_nen_object, get_nsi_object, get_nsf_object,      &
       get_nbc_object, get_is_object, get_je_object, get_xy_object, get_xc_object,        &
       get_ks_object, get_aa_object, get_hv_object, get_hw_object, get_jb_object,         &
       get_jt_object, get_hu_object, get_dxmin_object, get_irand_object,                  &
       get_hland_object, get_ipobo_object, get_ns_object, get_ie_object, get_b_s_object,  &
       get_b_t_object, get_b_ms_object, get_bnd_object, get_enc_object, get_dry_object,   &
       get_bnd_object, get_m_object, get_n_object, get_xs_object, get_dy_object,          &
       get_dx_object, get_isbnd_object, get_isdam_object, get_thd_object, get_lwl_object, &
       get_ext_object, get_huu_object, get_xg_object,                                     &
       !
       setup_aa_object, setup_xs_object, setup_xc_object, setup_xg_object,                &
       setup_is_object, setup_jb_object, setup_jt_object, setup_je_object,                &
       setup_ie_object, setup_hu_object, setup_hv_object, setup_hw_object,                &
       setup_dx_object, setup_dy_object, setup_dxmin_object, setup_nsi_object,            &
       setup_nsf_object, setup_nrand_object, setup_b_ms_object, setup_b_ss_object,        &
       setup_b_s_object, setup_b_v_object, setup_b_t_object, setup_nen_object,            &
       setup_ks_object, setup_irand_object, setup_isbnd_object, setup_isdam_object,       &
       setup_huu_object, setup_hvu_object, setup_hwu_object, setup_dg_object,             &
       !
       is_polyedge_current , is_polyedge_discharge , is_polyedge_total_discharge,         &
       is_polyedge_thin_dam, is_polyedge_local_weir, is_polyedge_2d_weir
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
  !! Fl&auml;che der Polygone aus bekannten Daten berechnen
  INTERFACE derive_aa
     MODULE PROCEDURE derive_aa_d
  END INTERFACE
  !! Mittenkoordinaten der Kanten aus bekannten Daten berechnen
  INTERFACE derive_xs
     MODULE PROCEDURE derive_xs_d
  END INTERFACE
  !! Zentrumskoordinaten der Polygone aus bekannten Daten berechnen
  INTERFACE derive_xc
     MODULE PROCEDURE derive_xc_d
  END INTERFACE
  !! Schwerpunktkoordinaten der Polygone aus bekannten Daten berechnen
  INTERFACE derive_xg
     MODULE PROCEDURE derive_xg_d
  END INTERFACE
  !! Kantenverzeichnis der Polygone neu berechnen
  INTERFACE derive_is
     MODULE PROCEDURE derive_is_d
  END INTERFACE
  !! Polygonverzeichnis der Kanten neu berechnen 
  INTERFACE derive_je
     MODULE PROCEDURE derive_je_d
  END INTERFACE
  !! Startknoten der Kanten neu berechnen 
  INTERFACE derive_jb
     MODULE PROCEDURE derive_jb_d ! 
  END INTERFACE
  !! Endknoten der Kanten neu berechnen 
  INTERFACE derive_jt
     MODULE PROCEDURE derive_jt_d ! 
 END INTERFACE
  !! Tiefe auf den Knoten neu berechnen 
  INTERFACE derive_hv
     MODULE PROCEDURE derive_hv_d 
  END INTERFACE
  !! Tiefe auf den Kanten neu berechnen 
  INTERFACE derive_hu
     MODULE PROCEDURE derive_hu_d 
  END INTERFACE
  !! Tiefe &uuml;ber den Polygonen neu berechnen
  INTERFACE derive_hw
     MODULE PROCEDURE derive_hw_d 
  END INTERFACE
  !! Polygonverzeichnis der Polygone neu berechnen
  INTERFACE derive_ie
     MODULE PROCEDURE derive_ie_d 
  END INTERFACE
  !! Abstand zwischen den Zentren benachbarter Polygone 
  INTERFACE derive_dx
     MODULE PROCEDURE derive_dx_d 
  END INTERFACE
  !! L&auml;nge der Kanten 
  INTERFACE derive_dy
     MODULE PROCEDURE derive_dy_d 
  END INTERFACE
  !! Abstand zwischen den Schwerpunkten benachbarter Polygone 
  INTERFACE derive_dg
     MODULE PROCEDURE derive_dg_d 
  END INTERFACE
  !! Anzahl der internen Kanten ermitteln 
  INTERFACE derive_nsi
     MODULE PROCEDURE derive_nsi_d 
  END INTERFACE
  !! Position der letzten Kante mit Fluss-Randbedingung
  INTERFACE derive_nsf
     MODULE PROCEDURE derive_nsf_d 
  END INTERFACE
  !! Anzahl der Gitterkanten ermitteln
  INTERFACE derive_ns
     MODULE PROCEDURE derive_is_d ! ist so gewollt
  END INTERFACE
  !! Anzahl der Knoten auf dem &auml;&szlig;eren Rand ermitteln
  INTERFACE derive_nrand
     MODULE PROCEDURE derive_nrand_d 
  END INTERFACE
  !! Hauptsektionen auf R&auml;ndern ableiten
  INTERFACE derive_b_ms
     MODULE PROCEDURE derive_b_s_d
  END INTERFACE
  !! Kanten auf R&auml;ndern ableiten
  INTERFACE derive_b_s
     MODULE PROCEDURE derive_b_s_d
  END INTERFACE
  !! Knoten auf R&auml;ndern ableiten
  INTERFACE derive_b_v
     MODULE PROCEDURE derive_b_s_d
  END INTERFACE
  !! Typ der Randkanten ableiten
  INTERFACE derive_b_t
     MODULE PROCEDURE derive_b_t_d
  END INTERFACE
  !! Unterabschnittsnummern der Randkanten ermitteln
  INTERFACE derive_b_ss
     MODULE PROCEDURE derive_b_ss_d
  END INTERFACE
  !! Knotenverzeichnis der Elemente [ nur Delft3D ]
  INTERFACE derive_nen
     MODULE PROCEDURE derive_nen_d
  END INTERFACE
  !! Randkennungen der Elemente [ nur Delft3D ]
  INTERFACE derive_irand
     MODULE PROCEDURE derive_irand_d
  END INTERFACE
  !! Anzahl der Knoten/Kanten der Elemente [ nur Delft3D ]
  INTERFACE derive_ks
     MODULE PROCEDURE derive_ks_d
  END INTERFACE
  !! unerodierbare Tiefe auf den Kanten neu berechnen 
  INTERFACE derive_huu
     MODULE PROCEDURE derive_huu_d 
  END INTERFACE
  !! unerodierbare Tiefe an den Knoten neu berechnen 
  INTERFACE derive_hvu
     MODULE PROCEDURE derive_hvu_d 
  END INTERFACE
  !! unerodierbare Tiefe an den Polygonen neu berechnen 
  INTERFACE derive_hwu
     MODULE PROCEDURE derive_hwu_d 
  END INTERFACE
  !
  !
  ! [C.5] Zuweisungen
  ! [C.6] Operatoren (optional, falls sinnvoll)
  ! [C.7] Liste der oeffentlichen Methoden
  !
  PUBLIC :: derive_aa    ! Flaechen
  PUBLIC :: derive_xs    ! Koordinaten der Kantenmitten
  PUBLIC :: derive_xc    ! Koordinaten der Polygonzentren
  PUBLIC :: derive_xg    ! Koordinaten der Polygonschwerpunkte
  PUBLIC :: derive_is    ! Kantenverzeichnis
  PUBLIC :: derive_je    ! Polygonverzeichnis der Kanten
  PUBLIC :: derive_jb    ! Startknoten der Kanten
  PUBLIC :: derive_jt    ! Endknoten der Kanten
  PUBLIC :: derive_hv    ! Tiefe an den Knoten
  PUBLIC :: derive_hu    ! Tiefe auf den Kanten
  PUBLIC :: derive_hw    ! Tiefe &uuml;ber den Polygonen neu berechnen
  PUBLIC :: derive_ie    ! Polygonverzeichnis der Polygone neu berechnen
  PUBLIC :: derive_dx    ! Abstand zwischen den Zentren benachbarter Polygone neu berechnen
  PUBLIC :: derive_dy    ! Laenge der Kanten der Polygone
  PUBLIC :: derive_dg    ! Abstand zwischen den Schwerpunkten benachbarter Polygone neu berechnen
  PUBLIC :: derive_nsi   ! Anzahl der internen Kanten ermitteln
  PUBLIC :: derive_nsf   ! Position der letzten Kante mit Fluss-Randbedingung
  PUBLIC :: derive_ns    ! Anzahl der Gitterkanten ermitteln
  PUBLIC :: derive_nrand ! Anzahl der Knoten auf dem aeusseren Rand ermitteln
  PUBLIC :: derive_b_ms  ! Hauptrandabschnitte ableiten
  PUBLIC :: derive_b_s   ! Kanten auf dem Rand ermitteln
  PUBLIC :: derive_b_v   ! Knoten auf dem Rand ermitteln
  PUBLIC :: derive_b_t   ! Typ der Randkanten ermitteln
  PUBLIC :: derive_b_ss  ! Unterabschnittsnummern der Randkanten ermitteln
  PUBLIC :: derive_nen   ! Knotenverzeichnis der Elemente [ nur Delft3D ]
  PUBLIC :: derive_irand ! Randkennungen der Elemente [ nur Delft3D ]
  PUBLIC :: derive_ks    ! Anzahl der Knoten/Kanten der Elemente [ nur Delft3D ]
  PUBLIC :: derive_huu   ! unerodierbare Kantentiefe neu berechnen
  PUBLIC :: derive_hvu   ! unerodierbare Knotentiefe neu berechnen
  PUBLIC :: derive_hwu   ! unerodierbare Polygontiefe neu berechnen
  !
  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Datentypen, Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] lokale Typdefinitionen
  !
  !! Typ zur Aufnahme von Kantendaten             <BR>
  !! a) Id der Kante                              <BR>
  !! b) Endeknoten der Kante                      <BR>
  !! c) linkes/rechtes Polygon der Kante          <BR>
  !! d) Index der Kante im linken/rechten Polygon
  TYPE , PRIVATE :: l_edg
     INTEGER             :: j    ! edge index "j"
     INTEGER             :: m    ! second node of edge
     INTEGER             :: p(2) ! polygons
     INTEGER (KIND=Byte) :: l(2) ! local edge numbers
  END TYPE l_edg
  !
  ! [D.2] Konstantwerte (Parameter)
  !
  !! Name des Moduls
  CHARACTER (LEN=15), PARAMETER :: c_modname = 'm_h_grid_derive' ! 
  !
  ! [D.3] Variablen (statische Daten des Moduls)
  !
  !! Hilfsfeld f&uuml;r Fehlermeldungen
  CHARACTER (LEN=12)           :: ctxt       ! 
  !! Dateivariante
  INTEGER                      :: ivar       ! 
  !
  ! [D.4] Schnittstellen
  !
  !! Berechnen der lokalen Knoten-/Kantenummer in Abh&auml;ngigkeit von ks
  INTERFACE get_l_no
     MODULE PROCEDURE get_l_no_d
  END INTERFACE
  !
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
  !! Fl&auml;che der Polygone aus dem Knotenverzeichnis der Polygone
  !! sowie den Koordinaten der Knoten berechnen; <BR>
  !! bei mehr als drei Knoten wird das Polygon in Teildreiecke zerlegt; <BR>
  !! die Fl&auml;che wird in dem Objekt "this" abgelegt; <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE derive_aa_d &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER       :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER   :: c_upname='derive_aa_d' ! 
    !! Name der angeforderten Gr&ouml;&szlig;e
    CHARACTER (LEN=10), PARAMETER   :: c_pgname='Flaeche AA' ! 
    !! String fuer Fehlermeldung
    CHARACTER (LEN=24), PARAMETER   :: c_node_tria_txt='lok. Dreiecksverzeichnis' 
    !! Statusvariable STAT
    INTEGER                         :: stat       ! 
    !! Knotenverzeichnis der Polygone
    INTEGER            , POINTER    :: p_nen(:,:) ! 
    !! Knotenverzeichnis der aus den Polygonen erzeugten Teildreiecke
    INTEGER            , POINTER    :: p_node_tria(:,:)  
    !! Anzahl der Knoten/Kanten in den Polygonen
    INTEGER            , POINTER    :: p_ks(:)    ! 
    !! Koordinaten der Knoten des Gitters
    REAL (KIND=Double) , POINTER    :: p_xy(:,:)  ! 
    !! Fl&auml;chen der Polygone
    REAL (KIND=Double) , POINTER    :: p_aa(:)    ! 
    !! Fl&auml;che eines Polygons
    REAL (KIND=Double) :: p_aa_tmp
    !! Koordinaten der Knoten eines Teildreieckes
    REAL (KIND=Double) :: xy(3,2)   
    !! Z&auml;hler
    INTEGER                         :: ipoly, inode, itria, i
    !
    ivar = get_h_grid_variant_no(this)
    !
    SELECT CASE ( ivar )
    CASE ( 1,2,3,4,5,6 ) ! "gitter05", "untrim", "selafin", "delft3d"
       NULLIFY ( p_nen, p_xy, p_ks, p_aa )
       ! Holen der zur Berechnung erforderlichen Daten
       p_nen => get_nen_object ( this )
       IF ( .NOT. ASSOCIATED(p_nen) ) THEN
          CALL derive_nen ( this )
          p_nen => get_nen_object ( this )
       END IF
       p_xy  => get_xy_object  ( this )
       p_ks  => get_ks_object  ( this )
       IF ( .NOT. ASSOCIATED(p_ks) ) THEN
          CALL derive_ks ( this )
          p_ks  => get_ks_object ( this )
       END IF
       ! Allokieren von Memory fuer das Berechnungsergebnis
       ALLOCATE ( p_aa(SIZE(p_nen,DIM=1)), STAT=stat )
       ! Fehlermeldung / Berechnung
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -25001, c_upname, c_modname, stat )
          CALL setup_error_act ( '<AllocGroessen>', c_pgname )
       ELSE
          !
          p_aa(:) = 0.
          ! Schleife ueber alle Polygone
          polyg: DO ipoly = 1, SIZE(p_aa)
             IF ( any_error() ) EXIT
             ! Polygon in Dreiecke aufteilen
             CALL poly_to_triangles &
                  ( ipoly, &
                  c_node_tria_txt, &
                  p_nen, &
                  p_ks, &
                  p_node_tria ) 
             !
             IF (no_error()) THEN
                !
                ! Schleife ueber alle Teildreiecke eines Polygons
                trias: DO itria = 1, p_ks(ipoly)-2
                   DO inode = 1, 3
                      DO i = 1, 2
                         xy(inode,i) = p_xy(p_node_tria(itria,inode),i)
                      ENDDO
                   ENDDO
                   ! Flaeche eines Teildreieckes
                   p_aa_tmp    = get_triangle_area_2d ( xy )
                   ! Flaeche eines Polygones
                   p_aa(ipoly) = p_aa(ipoly) + p_aa_tmp
                   !
                   IF ( itria == 2 .AND. p_aa_tmp > 0.0 ) THEN
                      DO i=1,2
                         xy(1,i) = p_xy(p_nen(ipoly,1),i)
                         xy(2,i) = p_xy(p_nen(ipoly,2),i)
                         xy(3,i) = p_xy(p_nen(ipoly,4),i)
                      END DO
                      p_aa_tmp = p_aa(ipoly) - get_triangle_area_2d(xy)
                   END IF
                   IF (p_aa_tmp < 0.) THEN
                      CALL setup_error_act ( all_errors(:), -25004, c_upname, c_modname )
                      WRITE(ctxt,'(I12)') ipoly          ; CALL setup_error_act ( '<NummerPolygon>', ctxt )
                      WRITE(ctxt,'(F12.3)') p_aa(ipoly)  ; CALL setup_error_act ( '<Flaeche>', ctxt )
                      WRITE(ctxt,'(F12.3)') p_aa_tmp     ; CALL setup_error_act ( '<TeilFlaeche>', ctxt )
                      WRITE(ctxt,'(I12)') p_nen(ipoly,1) ; CALL setup_error_act ( '<nummer-1>', ctxt )
                      WRITE(ctxt,'(I12)') p_nen(ipoly,2) ; CALL setup_error_act ( '<nummer-2>', ctxt )
                      WRITE(ctxt,'(I12)') p_nen(ipoly,3) ; CALL setup_error_act ( '<nummer-3>', ctxt )
                      WRITE(ctxt,'(I12)') p_nen(ipoly,4) ; CALL setup_error_act ( '<nummer-4>', ctxt )
                      WRITE(ctxt,'(F12.3)') p_xy(p_nen(ipoly,1),1) ; CALL setup_error_act ( '<x-1>', ctxt )
                      WRITE(ctxt,'(F12.3)') p_xy(p_nen(ipoly,2),1) ; CALL setup_error_act ( '<x-2>', ctxt )
                      WRITE(ctxt,'(F12.3)') p_xy(p_nen(ipoly,3),1) ; CALL setup_error_act ( '<x-3>', ctxt )
                      WRITE(ctxt,'(F12.3)') p_xy(p_nen(ipoly,4),1) ; CALL setup_error_act ( '<x-4>', ctxt )
                      WRITE(ctxt,'(F12.3)') p_xy(p_nen(ipoly,1),2) ; CALL setup_error_act ( '<y-1>', ctxt )
                      WRITE(ctxt,'(F12.3)') p_xy(p_nen(ipoly,2),2) ; CALL setup_error_act ( '<y-2>', ctxt )
                      WRITE(ctxt,'(F12.3)') p_xy(p_nen(ipoly,3),2) ; CALL setup_error_act ( '<y-3>', ctxt )
                      WRITE(ctxt,'(F12.3)') p_xy(p_nen(ipoly,4),2) ; CALL setup_error_act ( '<y-4>', ctxt )
                      EXIT trias
                   ENDIF
                ENDDO trias
                ! De-Allokieren
                DEALLOCATE ( p_node_tria, STAT=stat )
                IF ( stat /= 0 ) THEN
                   CALL setup_error_act ( all_errors(:), -25002, c_upname, c_modname, stat )
                   CALL setup_error_act ( '<DeAllocGroessen>', c_node_tria_txt)
                END IF
             ELSE
                EXIT polyg
             ENDIF
             !
          ENDDO polyg
          IF (no_error()) THEN
             ! Transfer der Daten
             CALL setup_aa_object ( this, p_aa )
             ! De-Allokieren
             DEALLOCATE ( p_aa, STAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), -25002, c_upname, c_modname, stat )
                CALL setup_error_act ( '<DeAllocGroessen>', c_pgname )
             END IF
          END IF
       END IF
       NULLIFY ( p_nen, p_xy, p_ks, p_aa )
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
       CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I12)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
  END SUBROUTINE derive_aa_d
  !
  !! Koordinaten der Mitten der Kanten aus dem Knotenverzeichnis der 
  !! Kanten sowie den Koordinaten der Knoten berechnen; <BR>
  !! die Koordinaten werden in dem Objekt "this" abgelegt; <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE derive_xs_d &
       ( this )
    !
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER       :: this
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER   :: c_upname='derive_xs_d'
    !! Name der angeforderten Gr&ouml;&szlig;e
    CHARACTER (LEN=20), PARAMETER   :: c_pgname='Mittenkoordinaten XS'
    !
    !! Statusvariable STAT
    INTEGER                         :: stat
    !! Knotenverzeichnisse der Kanten
    INTEGER            , POINTER    :: p_jb(:), p_jt(:)
    !! Koordinaten der Knoten des Gitters
    REAL (KIND=Double) , POINTER    :: p_xy(:,:)
    !! Mittenkoordinaten der Kanten
    REAL (KIND=Double) , POINTER    :: p_xs(:,:)
    !! Z&auml;hler
    INTEGER :: i_edge
    !! Anfangs- und Endknotennummer einer Kante
    INTEGER :: node1_nr, node2_nr
    !
    ivar = get_h_grid_variant_no(this)
    !
    SELECT CASE ( ivar )
    CASE ( 1,2,3,4,5,6 ) ! "gitter05", "untrim", "selafin", "delft3d"
       !
       NULLIFY ( p_xy, p_jb, p_jt, p_xs )
       ! Holen der zur Berechnung erforderlichen Daten
       p_xy  => get_xy_object ( this )
       p_jb  => get_jb_object ( this )
       p_jt  => get_jt_object ( this )
       ! ggf. Neuberechnen von jb und jt erforderlich
       IF ( .NOT. ASSOCIATED( p_jb ) .OR. &
            .NOT. ASSOCIATED( p_jt )       ) THEN
          CALL derive_is ( this )
          p_jb => get_jb_object ( this )
          p_jt => get_jt_object ( this )
       END IF
       IF ( no_error( ) ) THEN
          ! Allokieren von Memory fuer das Berechnungsergebnis
          ALLOCATE ( p_xs(SIZE(p_jb),2), STAT=stat )
          ! Fehlermeldung / Berechnung
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), -25001, c_upname, c_modname, stat )
             CALL setup_error_act ( '<AllocGroessen>', c_pgname )
          ELSE
             ! Schleife ueber alle Kanten
             edgego: DO i_edge = 1, SIZE(p_xs,DIM=1)
                !
                node1_nr = p_jb( i_edge )
                node2_nr = p_jt( i_edge )
                !
                IF ( node1_nr > 0 .AND. node1_nr <= SIZE(p_xy,DIM=1) .AND. &
                     node2_nr > 0 .AND. node2_nr <= SIZE(p_xy,DIM=1) ) THEN
                   !
                   p_xs( i_edge, : ) = .5 * ( p_xy( node1_nr, : ) + p_xy( node2_nr, : ) )
                   !
                ELSE
                   !
                   CALL setup_error_act ( all_errors(:), -25005, c_upname, c_modname )
                   WRITE(ctxt,'(I12)') i_edge
                   CALL setup_error_act ( '<i_edge>', ADJUSTL(TRIM(ctxt)) )
                   WRITE(ctxt,'(I12)') node1_nr
                   CALL setup_error_act ( '<no1_nr>', ADJUSTL(TRIM(ctxt)) )
                   WRITE(ctxt,'(I12)') node2_nr
                   CALL setup_error_act ( '<no2_nr>', ADJUSTL(TRIM(ctxt)) )
                   WRITE(ctxt,'(I12)') SIZE(p_xy,DIM=1)
                   CALL setup_error_act ( '<nof_nodes>', ADJUSTL(TRIM(ctxt)) )
                   !
                END IF
                !
             ENDDO edgego
             !
             IF (no_error()) THEN
                ! Transfer der Daten
                CALL setup_xs_object ( this, p_xs )
                ! De-Allokieren
                DEALLOCATE ( p_xs, STAT=stat )
                IF ( stat /= 0 ) THEN
                   CALL setup_error_act ( all_errors(:), -25002, c_upname, c_modname, stat )
                   CALL setup_error_act ( '<DeAllocGroessen>', c_pgname )
                END IF
             END IF
             !
          END IF
          !
          NULLIFY ( p_jb, p_jt, p_xy, p_xs )
          !
       END IF
       !
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
       CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I12)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
  END SUBROUTINE derive_xs_d
  !
  !! Koordinaten der Zentren der Polygone aus dem Knotenverzeichnis der 
  !! Polygone sowie den Koordinaten der Knoten berechnen; <BR>
  !! bei mehr als drei Knoten wird das Polygon in Teildreiecke zerlegt; <BR>
  !! die Koordinaten werden in dem Objekt "this" abgelegt; <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE derive_xc_d &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER       :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER   :: c_upname='derive_xc_d' ! 
    !! Name der angeforderten Gr&ouml;&szlig;e
    CHARACTER (LEN=22), PARAMETER   :: c_pgname='Zentrumskoordinaten XC' ! 
    !! String fuer Fehlermeldung
    CHARACTER (LEN=24), PARAMETER   :: c_node_tria_txt='lok. Dreiecksverzeichnis'
    !! Knotenverzeichnis der aus den Polygonen erzeugten Teildreiecke
    INTEGER            , POINTER    :: p_node_tria(:,:)  
    !! Statusvariable STAT
    INTEGER                         :: stat       ! 
    !! Knotenverzeichnis der Polygone
    INTEGER            , POINTER    :: p_nen(:,:) ! 
    !! Anzahl der Knoten/Kanten in den Polygonen
    INTEGER            , POINTER    :: p_ks(:)    ! 
    !! Koordinaten der Knoten des Gitters
    REAL (KIND=Double) , POINTER    :: p_xy(:,:)  ! 
    !! Zentrumskoordinaten der Polygone
    REAL (KIND=Double) , POINTER    :: p_xc(:,:)  ! 
    !! Zentrumskoordinaten eines Teildreieckes
    REAL (KIND=Double) :: p_center_old(2)
    !! Koordinaten der Knoten eines Teildreieckes
    REAL (KIND=Double) :: xy(3,2)   
    !! Vielfaches von EPSILON; bestimmt den Maximalabstand zwischen Zentren von Teildreiecken
    INTEGER, PARAMETER :: i_eps = 5
    !! Maximalabstand zwischen Zentren von Teildreiecken
    REAL (KIND=Double) :: max_dist   
    !! Z&auml;hler
    INTEGER                         :: ipoly, inode, itria, i
    !
    max_dist = MAX(i_eps * EPSILON(p_center_old), 0.1_Double)
    !
    ivar = get_h_grid_variant_no(this)
    !
    SELECT CASE ( ivar )
    CASE ( 1,2,3,4,5,6 ) ! "gitter05", "untrim", "selafin", "delft3d"
       NULLIFY ( p_nen, p_xy, p_ks, p_xc )
       ! Holen der zur Berechnung erforderlichen Daten
       p_nen => get_nen_object ( this )
       IF ( .NOT. ASSOCIATED(p_nen) ) THEN
          CALL derive_nen ( this )
          p_nen => get_nen_object ( this )
       END IF
       p_xy  => get_xy_object  ( this )
       p_ks  => get_ks_object  ( this )
       IF ( .NOT. ASSOCIATED(p_ks) ) THEN
          CALL derive_ks ( this )
          p_ks  => get_ks_object ( this )
       END IF
       ! Allokieren von Memory fuer das Berechnungsergebnis
       ALLOCATE ( p_xc(SIZE(p_nen,DIM=1),2), STAT=stat )
       ! Fehlermeldung / Berechnung
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -25001, c_upname, c_modname, stat )
          CALL setup_error_act ( '<AllocGroessen>', c_pgname )
       ELSE
          ! Schleife ueber alle Polygone
          polygo: DO ipoly = 1, SIZE(p_xc,DIM=1)
             !
             ! Polygon in Dreiecke aufteilen
             CALL poly_to_triangles &
                  ( ipoly, &
                  c_node_tria_txt, &
                  p_nen, &
                  p_ks, &
                  p_node_tria ) 
             !
             IF (no_error()) THEN
                !
                ! Schleife ueber alle Teildreiecke eines Polygons
                trias: DO itria = 1, p_ks(ipoly)-2
                   DO inode = 1, 3
                      DO i = 1, 2
                         xy(inode,i) = p_xy(p_node_tria(itria,inode),i)
                      ENDDO
                   ENDDO
                   !
                   ! Zentrumskoordinaten eines (Teil-) Dreieckes
                   p_xc(ipoly,:) = get_triangle_center_2d ( xy )
                   !
                   ! ggf. Kontrollrechnung mit Teildreiecken
                   IF (p_ks(ipoly) > 3) THEN
                      !
                      IF(itria > 1) THEN
                         ! Abstand der x-Komponenten und Abstand der y-Komponenten pruefen
                         ! falls zu grosser Abstand, werden Bildschirmwarnungen ausgegeben
                         ! -> ggf. liegt ist das Gitter nicht UNTRIM-konform
                         IF (ABS(p_xc(ipoly,1) - p_center_old(1)) > max_dist .OR. &
                             ABS(p_xc(ipoly,2) - p_center_old(2)) > max_dist ) THEN 
                            IF ( trc_op ) THEN
                               WRITE(trc_lun,*) &
                                    ' *** Warning *** Abweichungen bei der Kontrollberechnung der Zentrumskoordinaten'
                               WRITE(trc_lun,*) '     Nummer des Polygons = ', ipoly
                               WRITE(trc_lun,*) ' Nummer des Teildreiecks = ', itria
                               WRITE(trc_lun,*) ' erlaubter Abstand       = ', max_dist
                               WRITE(trc_lun,*) '                  deltax = ', &
                                    ABS(p_xc(ipoly,1) - p_center_old(1))
                               WRITE(trc_lun,*) '                  deltay = ', &
                                    ABS(p_xc(ipoly,2) - p_center_old(2))
                            END IF
                         ENDIF
                         p_xc(ipoly,:) = 0.5_Double*(p_xc(ipoly,:)+p_center_old(:))
                      ENDIF
                      p_center_old(:) = p_xc(ipoly,:)
                      !
                   ENDIF
                   !
                ENDDO trias
                !
                ! De-Allokieren
                DEALLOCATE ( p_node_tria, STAT=stat )
                IF ( stat /= 0 ) THEN
                   CALL setup_error_act ( all_errors(:), -25002, c_upname, c_modname, stat )
                   CALL setup_error_act ( '<DeAllocGroessen>', c_node_tria_txt)
                END IF
             ELSE
                EXIT polygo
             ENDIF
             !
          ENDDO polygo
          !
          IF (no_error()) THEN
             ! Transfer der Daten
             CALL setup_xc_object ( this, p_xc )
             ! De-Allokieren
             p_xc = 0.
             !
             DEALLOCATE ( p_xc, STAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), -25002, c_upname, c_modname, stat )
                CALL setup_error_act ( '<DeAllocGroessen>', c_pgname )
             END IF
          END IF
          !
       END IF
       !
       NULLIFY ( p_nen, p_xy, p_ks, p_xc )
       !
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
       CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I12)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
  END SUBROUTINE derive_xc_d
  !
  !! Koordinaten der Polygon-Schwerpunkte aus dem Knotenverzeichnis der 
  !! Polygone sowie den Koordinaten der Knoten berechnen; <BR>
  !! die Koordinaten werden in dem Objekt "this" abgelegt; <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE derive_xg_d &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER       :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER   :: c_upname='derive_xg_d' ! 
    !! Name der angeforderten Gr&ouml;&szlig;e
    CHARACTER (LEN=25), PARAMETER   :: c_pgname='Schwerpunktkoordinaten xg' ! 
    !! Statusvariable STAT
    INTEGER                         :: stat       ! 
    !! Knotenverzeichnis der Polygone
    INTEGER            , POINTER    :: p_nen(:,:) ! 
    !! Anzahl der Knoten/Kanten in den Polygonen
    INTEGER            , POINTER    :: p_ks(:)    ! 
    !! Koordinaten der Knoten des Gitters
    REAL (KIND=Double) , POINTER    :: p_xy(:,:)  ! 
    !! Schwerpunktkoordinaten der Polygone
    REAL (KIND=Double) , POINTER    :: p_xg(:,:)  ! 
    !! Indizes
    INTEGER                         :: ipoly, inode1, inode2
    !! Knotennumern
    INTEGER                         :: node_nr1, node_nr2
    !! Hilfsgroessen
    REAL (KIND=Double)              :: asum, xsum, ysum, area
    !
    ivar = get_h_grid_variant_no(this)
    !
    SELECT CASE ( ivar )
    CASE ( 1,2,3,4,5,6 ) ! "gitter05", "untrim", "selafin", "delft3d"
       !
       NULLIFY ( p_nen, p_xy, p_ks, p_xg )
       !
       ! Holen der zur Berechnung erforderlichen Daten
       p_nen => get_nen_object ( this )
       IF ( .NOT. ASSOCIATED(p_nen) ) THEN
          CALL derive_nen ( this )
          p_nen => get_nen_object ( this )
       END IF
       p_xy  => get_xy_object  ( this )
       p_ks  => get_ks_object  ( this )
       IF ( .NOT. ASSOCIATED(p_ks) ) THEN
          CALL derive_ks ( this )
          p_ks  => get_ks_object ( this )
       END IF
       !
       ! Allokieren von Memory fuer das Berechnungsergebnis
       ALLOCATE ( p_xg(SIZE(p_nen,DIM=1),2), STAT=stat )
       !
       ! Berechnung / Fehlermeldung
       IF ( stat == 0 ) THEN
          !
          ! Schleife ueber alle Polygone
          polygo: DO ipoly = 1, SIZE(p_xg,DIM=1)
             !
             ! Schwerpunktberechnung fuer Dreiecke einfacher,
             ! als fuer Polygone mit Knotenanzahl > 3
             IF ( p_ks( ipoly ) == 3 ) THEN
                !
                ! Schwerpunktberechnung fuer Dreiecke
                p_xg( ipoly, 1 ) = &
                     ( p_xy(p_nen(ipoly,1),1) &
                     + p_xy(p_nen(ipoly,2),1) &
                     + p_xy(p_nen(ipoly,3),1) ) / 3.D0
                p_xg( ipoly, 2 ) = &
                     ( p_xy(p_nen(ipoly,1),2) &
                     + p_xy(p_nen(ipoly,2),2) &
                     + p_xy(p_nen(ipoly,3),2) ) / 3.D0
                !
             ELSE
                !
                ! Schwerpunktberechnung fuer Polygone mit Knotenanzahl > 3
                !
                ! Summation ueber alle Knoten des Polygons
                asum = 0.D0
                xsum = 0.D0
                ysum = 0.D0
                !
                DO inode1 = 1, p_ks( ipoly )
                   !
                   ! Nachfolger-Knotenindex
                   inode2 = inode1 + 1
                   IF ( inode2 > p_ks( ipoly ) ) inode2 = 1
                   !
                   ! Knotennummern
                   node_nr1 = p_nen ( ipoly, inode1 )
                   node_nr2 = p_nen ( ipoly, inode2 )
                   !
                   ! Summation fortfuehren
                   asum = asum &
                        + (p_xy(node_nr1,1)*p_xy(node_nr2,2) - p_xy(node_nr2,1)*p_xy(node_nr1,2))
                   xsum = xsum &
                        + ((p_xy(node_nr1,1)+p_xy(node_nr2,1)) &
                        * (p_xy(node_nr1,1)*p_xy(node_nr2,2)-p_xy(node_nr2,1)*p_xy(node_nr1,2)))
                   ysum = ysum &
                        + ((p_xy(node_nr1,2)+p_xy(node_nr2,2)) &
                        * (p_xy(node_nr1,1)*p_xy(node_nr2,2)-p_xy(node_nr2,1)*p_xy(node_nr1,2)))
                   !
                END DO
                !
                area = 0.5D0 * asum
                !
                p_xg( ipoly, 1 ) = xsum / ( 6.D0*area )
                p_xg( ipoly, 2 ) = ysum / ( 6.D0*area )
                !
             END IF
             !
          ENDDO polygo
          !
          IF (no_error()) THEN
             !
             ! Transfer der Daten
             CALL setup_xg_object ( this, p_xg )
             !
             ! De-Allokieren
             p_xg = 0.
             DEALLOCATE ( p_xg, STAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), -25002, c_upname, c_modname, stat )
                CALL setup_error_act ( '<DeAllocGroessen>', c_pgname )
             END IF
             !
          END IF
          !
       ELSE
          !
          CALL setup_error_act ( all_errors(:), -25001, c_upname, c_modname, stat )
          CALL setup_error_act ( '<AllocGroessen>', c_pgname )
          !
       END IF
       !
       NULLIFY ( p_nen, p_xy, p_ks, p_xg )
       !
    CASE DEFAULT
       !
       CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
       CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I12)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
       !
    END SELECT
    !
  END SUBROUTINE derive_xg_d
  !
  !! Kantenverzeichnis der Polygone aus dem Knotenverzeichnis der 
  !! Polygone berechnen <BR>
  !! das Kantenverzeichnis wird in dem Objekt "this" abgelegt <BR>
  !! des weiteren werden auch noch das Polygonverzeichnis der Kanten,
  !! sowie die Verzeichnisse mit den Start- und Endknoten der Kanten
  !! mit erzeugt und ebenfalls in dem Arbeitsobjekt "this" abgelegt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE derive_is_d &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER       :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER   :: c_upname='derive_is_d' ! 
    !! Name der angeforderten Gr&ouml;&szlig;e
    CHARACTER (LEN=17), PARAMETER   :: c_pgname ='Kantenverzeichnis'         ! 
    CHARACTER (LEN=18), PARAMETER   :: c_pgname1='Hilfsfelder SN, NN'        ! 
    CHARACTER (LEN=25), PARAMETER   :: c_pgname2='Ergebnisse IS, JE, JB, JT' ! 
    !! Statusvariable STAT
    INTEGER                         :: stat       ! 
    !! Anzahl der Knoten
    INTEGER            , POINTER    :: p_nv       ! 
    !! Knotenverzeichnis der Polygone
    INTEGER            , POINTER    :: p_nen(:,:) ! 
    !! Anzahl der Knoten/Kanten in den Polygonen
    INTEGER            , POINTER    :: p_ks(:)    ! 
    !! Kantenverzeichnis der Polygone
    INTEGER           , ALLOCATABLE :: is(:,:)    ! 
    !! Polygonverzeichnis der Kanten 
    INTEGER           , ALLOCATABLE :: je(:,:)    ! 
    !! Anfangsknoten auf Kante
    INTEGER           , ALLOCATABLE :: jb(:)      ! 
    !! Endeknoten auf Kante
    INTEGER           , ALLOCATABLE :: jt(:)      ! 
    !! Feld zum Zwischenspeichern der Daten
    TYPE (l_edg)      , ALLOCATABLE :: sn(:,:)    !
    !! Feld zum memorieren der Anzahl von Kanten je Knoten
    INTEGER           , ALLOCATABLE :: nn(:)      ! 
    !! maximale Anzahl der an einen Knoten grenzenden Kanten
    INTEGER                         :: nsx        ! 
    !! aktuelle Anzahl der Kanten 
    INTEGER                         :: ns         ! 
    !! Z&auml;hlervariable
    INTEGER                         :: i, j, k, l, m, n ! 
    !
    ivar = get_h_grid_variant_no(this)
    !
    SELECT CASE ( ivar )
    CASE ( 1,2,3,4,5,6 ) ! "gitter05", "untrim", "selafin", "delft3d"
       NULLIFY ( p_nv, p_nen, p_ks )
       ! Holen der zur Berechnung erforderlichen Daten
       p_nv  => get_nv_object  ( this )
       p_nen => get_nen_object ( this )
       IF ( .NOT. ASSOCIATED(p_nen) ) THEN
          CALL derive_nen ( this )
          p_nen => get_nen_object ( this )
       END IF
       p_ks  => get_ks_object  ( this )
       IF ( .NOT. ASSOCIATED(p_ks) ) THEN
          CALL derive_ks ( this )
          p_ks  => get_ks_object ( this )
       END IF
       nsx = get_max_edges_per_vertex ( p_nen, p_ks, p_nv )
       ! Allokieren von Memory fuer das Zwischenergebnis
       ALLOCATE ( sn(p_nv,nsx), nn(p_nv), STAT=stat )
       ! Fehlermeldung / Berechnung
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), -25001, c_upname, c_modname, stat )
          CALL setup_error_act ( '<AllocGroessen>', c_pgname1 )
       ELSE
          !
          CALL build_nn_sn_from_nen ( p_nen, p_ks, nn, sn )
          ns = MAXVAL( sn(:,:)%j )
          ! Allokieren der Ergebnisfelder
          ALLOCATE ( is(SIZE(p_nen,DIM=1),SIZE(p_nen,DIM=2)), &
               je(ns,2), jb(ns), jt(ns), STAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), -25001, c_upname, c_modname, stat )
             CALL setup_error_act ( '<AllocGroessen>', c_pgname2 )
          ELSE
             ! Transfer der Daten
             DO n=1,SIZE(nn(:))
                DO k=1,nn(n)
                   j       = sn(n,k)%j
                   jb(j)   = n            ! Startknoten
                   jt(j)   = sn(n,k)%m    ! Endknoten 
                   je(j,:) = sn(n,k)%p(:) ! Polygonverzeichnis Kanten
                   DO m=1,2
                      l = sn(n,k)%l(m)
                      i = sn(n,k)%p(m)
                      IF ( i > 0 .AND. l > 0 ) is(i,l) = j
                   END DO
                END DO
             END DO
             ! Fuer Randkanten sicherstellen dass je(j,1) > 0 ist. Hierfuer
             ! muessen ggf. je, jb und jt getauscht werden
             DO j=1,SIZE(je,1)
                IF ( je(j,1) > 0 ) CYCLE
                m       = je(j,1)
                je(j,1) = je(j,2)
                je(j,2) = m
                m       = jb(j)
                jb(j)   = jt(j)
                jt(j)   = m
             END DO
             CALL setup_is_object ( this, is )
             CALL setup_jb_object ( this, jb )
             CALL setup_jt_object ( this, jt )
             CALL setup_je_object ( this, je )
             ! De-Allokieren
             DEALLOCATE ( is, je, jb, jt, STAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), -25002, c_upname, c_modname, stat )
                CALL setup_error_act ( '<DeAllocGroessen>', c_pgname2 )
             END IF
          END IF
          DEALLOCATE ( sn, nn, STAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), -25002, c_upname, c_modname, stat )
             CALL setup_error_act ( '<DeAllocGroessen>', c_pgname1 )
          END IF
       END IF
       NULLIFY ( p_nv, p_nen, p_ks )
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
       CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I12)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
  END SUBROUTINE derive_is_d
  !
  !! Verzeichnis mit den Anfangsknoten der Kanten <BR> 
  !! das Anfangsknotenverzeichnis wird in dem Objekt "this" abgelegt <BR>
  !! es wird zun&auml;chst versucht, das Verzeichnis aus dem Seitenverzeichnis
  !! der Polygone zu ermitteln, insofern dieses vorhanden ist; ansonsten
  !! wird die Funktion <EM>derive_is</EM> gerufen <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE derive_jb_d &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER       :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER   :: c_upname='derive_jb_d' ! 
    !! Name der angeforderten Gr&ouml;&szlig;e
    CHARACTER (LEN=24), PARAMETER   :: c_pgname ='Anfangsknoten der Kanten'  ! 
    CHARACTER (LEN=13), PARAMETER   :: c_pgname1='Ergebnisse JB' ! 
    !! Statusvariable STAT
    INTEGER                         :: stat         ! 
    !! Anzahl der Kanten im Gitternetz
    INTEGER            , POINTER    :: p_ns         ! 
    !! Knotenverzeichnis der Polygone
    INTEGER            , POINTER    :: p_nen(:,:)   ! 
    !! Anzahl der Knoten/Kanten in den Polygonen
    INTEGER            , POINTER    :: p_ks(:)      ! 
    !! Kantenverzeichnis der Polygone
    INTEGER            , POINTER    :: p_is(:,:)    ! 
    !! Anfangsknoten auf Kante
    INTEGER           , ALLOCATABLE :: jb(:), jt(:) ! 
    !! lokales Nachbarverzeichnis der der Kanten
    INTEGER           , ALLOCATABLE :: je(:,:)      ! 
    !! Z&auml;hlervariable
    INTEGER                         :: i, j, j1, j2, k, lr, m ! 
    !
    ivar = get_h_grid_variant_no(this)
    !
    SELECT CASE ( ivar )
    CASE ( 1,2,3,4,5,6 ) ! "gitter05", "untrim", "selafin", "delft3d"
       NULLIFY ( p_nen, p_ks, p_is )
       ! Holen der zur Berechnung erforderlichen Daten
       p_ns  => get_ns_object  ( this )
       p_nen => get_nen_object ( this )
       IF ( .NOT. ASSOCIATED(p_nen) ) THEN
          CALL derive_nen ( this )
          p_nen => get_nen_object ( this )
       END IF
       p_ks  => get_ks_object  ( this )
       IF ( .NOT. ASSOCIATED(p_ks) ) THEN
          CALL derive_ks ( this )
          p_ks  => get_ks_object ( this )
       END IF
       p_is  => get_is_object  ( this )
       IF ( .NOT. ASSOCIATED(p_is) ) THEN
          CALL derive_is ( this ) ! erzeugt automatisch auch jb(:)
          p_is  => get_is_object ( this )
       ELSE
          ! Allokieren von Memory fuer das Zwischenergebnis
          ALLOCATE ( jb(p_ns), jt(p_ns), je(p_ns,2), STAT=stat )
          ! Fehlermeldung / Berechnung
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), -25001, c_upname, c_modname, stat )
             CALL setup_error_act ( '<AllocGroessen>', c_pgname1 )
          ELSE
             jb(:)   = 0
             jt(:)   = 0
             je(:,:) = 0
             DO i=1,SIZE(p_nen,1)
                DO j=1,p_ks(i)
                   k  = p_is(i,j)
                   j1 = j
                   j2 = MERGE( 1, j+1, j == p_ks(i) )
                   IF ( jb(k) > 0 ) THEN
                      lr    = MERGE( 1, 2, p_nen(i,j1) == jb(k) .AND. p_nen(i,j2) == jt(k) )
                   ELSE
                      jb(k) = MIN( p_nen(i,j1), p_nen(i,j2) )
                      jt(k) = MAX( p_nen(i,j1), p_nen(i,j2) )
                      lr    = MERGE( 1, 2, p_nen(i,j1) == jb(k) .AND. p_nen(i,j2) == jt(k) )
                   END IF
                   je(k,lr) = i
                END DO
             END DO
             ! Fuer Randkanten sicherstellen dass je(j,1) > 0 ist. Hierfuer
             ! muessen ggf. je, jb und jt getauscht werden
             DO j=1,SIZE(je,1)
                IF ( je(j,1) > 0 ) CYCLE
                m       = je(j,1)
                je(j,1) = je(j,2)
                je(j,2) = m
                m       = jb(j)
                jb(j)   = jt(j)
                jt(j)   = m
             END DO
             CALL setup_jb_object ( this, jb )
             ! De-Allokieren
             DEALLOCATE ( jb, jt, je, STAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), -25002, c_upname, c_modname, stat )
                CALL setup_error_act ( '<DeAllocGroessen>', c_pgname1 )
             END IF
          END IF
       END IF
       NULLIFY ( p_ns, p_nen, p_ks, p_is )
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
       CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I12)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
  END SUBROUTINE derive_jb_d
  !
  !! Verzeichnis mit den Endknoten der Kanten <BR> 
  !! das Endknotenverzeichnis wird in dem Objekt "this" abgelegt <BR>
  !! es wird zun&auml;chst versucht, das Verzeichnis aus dem Seitenverzeichnis
  !! der Polygone zu ermitteln, insofern dieses vorhanden ist; ansonsten
  !! wird die Funktion <EM>derive_is</EM> gerufen <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE derive_jt_d &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER       :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER   :: c_upname='derive_jt_d' ! 
    !! Name der angeforderten Gr&ouml;&szlig;e
    CHARACTER (LEN=20), PARAMETER   :: c_pgname ='Endknoten der Kanten'  ! 
    CHARACTER (LEN=13), PARAMETER   :: c_pgname1='Ergebnisse JT' ! 
    !! Statusvariable STAT
    INTEGER                         :: stat       ! 
    !! Anzahl der Kanten im Gitternetz
    INTEGER            , POINTER    :: p_ns       ! 
    !! Knotenverzeichnis der Polygone
    INTEGER            , POINTER    :: p_nen(:,:) ! 
    !! Anzahl der Knoten/Kanten in den Polygonen
    INTEGER            , POINTER    :: p_ks(:)    ! 
    !! Kantenverzeichnis der Polygone
    INTEGER            , POINTER    :: p_is(:,:)  ! 
    !! Endknoten auf Kante
    INTEGER           , ALLOCATABLE :: jt(:), jb(:) ! 
    !! lokales Nachbarverzeichnis der der Kanten
    INTEGER           , ALLOCATABLE :: je(:,:)      ! 
    !! Z&auml;hlervariable
    INTEGER                         :: i, j, j1, j2, k, lr, m ! 
    !
    ivar = get_h_grid_variant_no(this)
    !
    SELECT CASE ( ivar )
    CASE ( 1,2,3,4,5,6 ) ! "gitter05", "untrim", "selafin", "delft3d"
       NULLIFY ( p_nen, p_ks, p_is )
       ! Holen der zur Berechnung erforderlichen Daten
       p_ns  => get_ns_object  ( this )
       p_nen => get_nen_object ( this )
       IF ( .NOT. ASSOCIATED(p_nen) ) THEN
          CALL derive_nen ( this )
          p_nen => get_nen_object ( this )
       END IF
       p_ks  => get_ks_object  ( this )
       IF ( .NOT. ASSOCIATED(p_ks) ) THEN
          CALL derive_ks ( this )
          p_ks  => get_ks_object ( this )
       END IF
       p_is  => get_is_object  ( this )
       IF ( .NOT. ASSOCIATED(p_is) ) THEN
          CALL derive_is ( this ) ! erzeugt automatisch auch jt(:)
          p_is  => get_is_object ( this )
       ELSE
          ! Allokieren von Memory fuer das Zwischenergebnis
          ALLOCATE ( jt(p_ns), jb(p_ns), je(p_ns,2), STAT=stat )
          ! Fehlermeldung / Berechnung
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), -25001, c_upname, c_modname, stat )
             CALL setup_error_act ( '<AllocGroessen>', c_pgname1 )
          ELSE
             jt(:)   = 0
             jb(:)   = 0
             je(:,:) = 0
             DO i=1,SIZE(p_nen,1)
                DO j=1,p_ks(i)
                   k  = p_is(i,j)
                   j1 = j
                   j2 = MERGE( 1, j+1, j == p_ks(i) )
                   IF ( jt(k) > 0 ) THEN
                      lr    = MERGE( 1, 2, p_nen(i,j1) == jb(k) .AND. p_nen(i,j2) == jt(k) )
                   ELSE
                      jt(k) = MAX( p_nen(i,j1), p_nen(i,j2) )
                      jb(k) = MIN( p_nen(i,j1), p_nen(i,j2) )
                      lr    = MERGE( 1, 2, p_nen(i,j1) == jb(k) .AND. p_nen(i,j2) == jt(k) )
                   END IF
                   je(k,lr) = i
                END DO
             END DO
             ! Fuer Randkanten sicherstellen dass je(j,1) > 0 ist. Hierfuer
             ! muessen ggf. je, jb und jt getauscht werden
             DO j=1,SIZE(je,1)
                IF ( je(j,1) > 0 ) CYCLE
                m       = je(j,1)
                je(j,1) = je(j,2)
                je(j,2) = m
                m       = jb(j)
                jb(j)   = jt(j)
                jt(j)   = m
             END DO
             CALL setup_jt_object ( this, jt )
             ! De-Allokieren
             DEALLOCATE ( jt, jb, je, STAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), -25002, c_upname, c_modname, stat )
                CALL setup_error_act ( '<DeAllocGroessen>', c_pgname1 )
             END IF
          END IF
       END IF
       NULLIFY ( p_ns, p_nen, p_ks, p_is )
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
       CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I12)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
  END SUBROUTINE derive_jt_d
  !
  !! Nachbarelementverzeichnis der Kanten <BR> 
  !! das Nachbarelementverzeichnis der Kanten wird in dem Objekt "this" abgelegt <BR>
  !! es wird zun&auml;chst versucht, das Verzeichnis aus dem Seitenverzeichnis
  !! der Polygone zu ermitteln, insofern dieses vorhanden ist; ansonsten
  !! wird die Funktion <EM>derive_is</EM> gerufen <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE derive_je_d &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER       :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER   :: c_upname='derive_je_d' ! 
    !! Name der angeforderten Gr&ouml;&szlig;e
    CHARACTER (LEN=36), PARAMETER   :: c_pgname ='Nachbarelementverzeichnis der Kanten'  ! 
    CHARACTER (LEN=13), PARAMETER   :: c_pgname1='Ergebnisse JE' ! 
    !! Statusvariable STAT
    INTEGER                         :: stat       ! 
    !! Anzahl der Kanten im Gitternetz
    INTEGER            , POINTER    :: p_ns       ! 
    !! Knotenverzeichnis der Polygone
    INTEGER            , POINTER    :: p_nen(:,:) ! 
    !! Anzahl der Knoten/Kanten in den Polygonen
    INTEGER            , POINTER    :: p_ks(:)    ! 
    !! Kantenverzeichnis der Polygone
    INTEGER            , POINTER    :: p_is(:,:)  ! 
    !! Anfangsknoten der Kanten
    INTEGER            , POINTER    :: p_jb(:)    ! 
    !! Endknoten der Kanten
    INTEGER            , POINTER    :: p_jt(:)    ! 
    !! Endknoten auf Kante
    INTEGER           , ALLOCATABLE :: je(:,:)     ! 
    !! Z&auml;hlervariable
    INTEGER                         :: i, j, j1, j2, k, lr ! 
    !
    ivar = get_h_grid_variant_no(this)
    !
    SELECT CASE ( ivar )
    CASE ( 1,2,3,4,5,6 ) ! "gitter05", "untrim", "selafin", "delft3d"
       NULLIFY ( p_nen, p_ks, p_is, p_jb, p_jt )
       ! Holen der zur Berechnung erforderlichen Daten
       p_ns  => get_ns_object  ( this )
       p_nen => get_nen_object ( this )
       IF ( .NOT. ASSOCIATED(p_nen) ) THEN
          CALL derive_nen ( this )
          p_nen => get_nen_object ( this )
       END IF
       p_ks  => get_ks_object  ( this )
       IF ( .NOT. ASSOCIATED(p_ks) ) THEN
          CALL derive_ks ( this )
          p_ks  => get_ks_object ( this )
       END IF
       p_jb  => get_jb_object  ( this )
       IF ( .NOT. ASSOCIATED( p_jb ) ) THEN
          CALL derive_jb ( this ) ! beruecksichtigt je(j,1) > 0
          p_jb  => get_jb_object  ( this )
       END IF
       p_jt  => get_jt_object  ( this )
       IF ( .NOT. ASSOCIATED( p_jt ) ) THEN
          CALL derive_jt ( this ) ! beruecksichtigt je(j,1) > 0 
          p_jt  => get_jt_object  ( this )
       END IF
       p_is  => get_is_object  ( this )
       IF ( .NOT. ASSOCIATED(p_is) ) THEN
          CALL derive_is ( this ) ! erzeugt automatisch auch jt(:)
          p_is  => get_is_object ( this )
       ELSE
          ! Allokieren von Memory fuer das Zwischenergebnis
          ALLOCATE ( je(p_ns,2), STAT=stat )
          ! Fehlermeldung / Berechnung
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), -25001, c_upname, c_modname, stat )
             CALL setup_error_act ( '<AllocGroessen>', c_pgname1 )
          ELSE
             je(:,:) = 0
             DO i=1,SIZE(p_nen,1)
                DO j=1,p_ks(i)
                   k        = p_is(i,j)
                   j1       = j
                   j2       = MERGE( 1, j+1, j == p_ks(i) )
                   lr       = MERGE( 1, 2, p_nen(i,j1) == p_jb(k) .AND. p_nen(i,j2) == p_jt(k) )
                   je(k,lr) = i
                END DO
             END DO
             CALL setup_je_object ( this, je )
             ! De-Allokieren
             DEALLOCATE ( je, STAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), -25002, c_upname, c_modname, stat )
                CALL setup_error_act ( '<DeAllocGroessen>', c_pgname1 )
             END IF
          END IF
       END IF
       NULLIFY ( p_ns, p_nen, p_ks, p_is, p_jb, p_jt )
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
       CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I12)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
  END SUBROUTINE derive_je_d
  !
  !! Tiefenlage der Knoten aus der Tiefenlage der Elemente
  !! neu berechnen <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE derive_hv_d &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER        :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER    :: c_upname='derive_hv_d' ! 
    !! Name der angeforderten Gr&ouml;&szlig;e
    CHARACTER (LEN=21), PARAMETER    :: c_pgname='Tiefenlage der Knoten' ! 
    !! Name der fehlenden Gr&ouml;&szlig;en
    CHARACTER (LEN=30), PARAMETER    :: c_pgmiss='Tiefenlage der Polygone/Kanten' ! 
    !! Statusvariable
    INTEGER                          :: stat       !  
    !! Anzahl der Knoten
    INTEGER            , POINTER     :: p_nv       ! 
    !! Anzahl der Knoten je Polygon
    INTEGER            , POINTER     :: p_ks(:)    ! 
    !! erster Knoten einer Kante
    INTEGER            , POINTER     :: p_jb(:)    ! 
    !! zweiter Knoten einer Kante
    INTEGER            , POINTER     :: p_jt(:)    ! 
    !! Knotenverzeichnis der Polygone
    INTEGER            , POINTER     :: p_nen(:,:) ! 
    !! Tiefe &uuml;ber den Kanten
    REAL (KIND=Double) , POINTER     :: p_hu(:)    ! 
    !! Tiefe &uuml;ber den Polygnen
    REAL (KIND=Double) , POINTER     :: p_hw(:)    ! 
    !! Fl&auml;che der Polygone
    REAL (KIND=Double) , POINTER     :: p_aa(:)    ! 
    !! Grenztiefe
    REAL (KIND=Double) , POINTER     :: p_hl       ! 
    !! Tiefe &uuml;ber den Knoten / Gewichte
    REAL (KIND=Double) , ALLOCATABLE :: hv(:), wv(:) ! 
    !! Z&auml;hlervariablen
    INTEGER :: i, j, l, n ! 
    !
    ivar = get_h_grid_variant_no(this)
    SELECT CASE ( ivar )
    CASE ( 3,4 ) ! "untrim"
       NULLIFY ( p_nv, p_ks, p_nen, p_hw, p_aa, p_hu, p_jb, p_jt, p_hl )
       p_nv  => get_nv_object    ( this )
       p_ks  => get_ks_object    ( this )
       IF ( .NOT. ASSOCIATED(p_ks) ) THEN
          CALL derive_ks ( this )
          p_ks  => get_ks_object ( this )
       END IF
       p_nen => get_nen_object   ( this )
       IF ( .NOT. ASSOCIATED(p_nen) ) THEN
          CALL derive_nen ( this )
          p_nen => get_nen_object ( this )
       END IF
       p_hu  => get_hu_object    ( this )
       p_jb  => get_jb_object    ( this )
       p_hl  => get_hland_object ( this ) 
       IF ( .NOT. ASSOCIATED(p_hl) ) THEN
          ALLOCATE( this%hland )
          this%hland = -10000.0_Double
          p_hl => get_hland_object ( this ) 
       END IF
       IF ( .NOT. ASSOCIATED(p_jb) ) THEN
          CALL derive_jb ( this )
          p_jb => get_jb_object ( this )
       END IF
       p_jt  => get_jt_object  ( this )
       IF ( .NOT. ASSOCIATED(p_jt) ) THEN
          CALL derive_jt ( this )
          p_jt => get_jt_object ( this )
       END IF
       p_hw  => get_hw_object  ( this )
       IF ( .NOT. ASSOCIATED(p_hw) ) THEN
          CALL derive_hw ( this )
          p_hw  => get_hw_object( this )
       END IF
       p_aa => get_aa_object( this )
       IF ( .NOT. ASSOCIATED(p_aa) ) THEN
          CALL derive_aa ( this )
          p_aa => get_aa_object( this )
       END IF
       IF ( ASSOCIATED(p_aa) .AND. ( ASSOCIATED(p_hw) .OR.                             &
            ( ASSOCIATED(p_hu) .AND. ASSOCIATED(p_jb) .AND. ASSOCIATED(p_jt) ) ) .AND. &
            ASSOCIATED(p_nv) .AND.   ASSOCIATED(p_ks) .AND. ASSOCIATED(p_nen)         ) THEN
          ! Allokieren von Memory fuer das Berechnungsergebnis
          ALLOCATE ( hv(p_nv), wv(p_nv), STAT=stat )
          ! Fehlermeldung / Berechnung
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), -25001, c_upname, c_modname, stat )
             CALL setup_error_act ( '<AllocGroessen>', c_pgname )
          ELSE
             ! ... Knotentiefen zunaechst aus (maximalen) Polygontiefen ableiten
             hv(:) = p_hl
             DO i=1,SIZE(p_nen,DIM=1)
                DO l=1,p_ks(i)
                   n     = p_nen(i,l)
                   hv(n) = MAX( hv(n), p_hw(i) )
                END DO
             END DO
             ! ... optional Knotentiefen mit minimalen Kantentiefen korrigieren
             IF ( ASSOCIATED(p_hu) .AND. ASSOCIATED(p_jb) .AND. ASSOCIATED(p_jt) ) THEN 
                DO j=1,SIZE(p_hu)
                   IF ( p_hu(j) <= p_hl ) CYCLE
                   IF ( hv(p_jb(j)) > p_hl ) hv(p_jb(j)) = MIN( hv(p_jb(j)), p_hu(j) )
                   IF ( hv(p_jt(j)) > p_hl ) hv(p_jt(j)) = MIN( hv(p_jt(j)), p_hu(j) )
                END DO
             END IF
          END IF
          ! Transfer der Daten
          CALL setup_hv_object ( this, hv )
          DEALLOCATE( hv, wv )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), -25002, c_upname, c_modname, stat )
             CALL setup_error_act ( '<DeAllocGroessen>', c_pgname )
          END IF
       ELSE
          CALL setup_error_act ( all_errors(:), -25003, c_upname, c_modname )
          CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
          CALL setup_error_act ( '<FehlendeGroesse>', c_pgmiss ) 
       END IF
       NULLIFY ( p_nv, p_ks, p_nen, p_hw, p_aa, p_hu, p_jb, p_jt, p_hl )
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
       CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I12)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
  END SUBROUTINE derive_hv_d
  !
  !! Tiefenlage der Kanten aus der Tiefe der Knoten neu berechnen,
  !! falls deren Tiefenlage bekannt ist <BR> 
  !! die Tiefe der Kanten wird in dem Objekt "this" abgelegt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE derive_hu_d &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER       :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER   :: c_upname='derive_hu_d' ! 
    !! Name der angeforderten Gr&ouml;&szlig;e
    CHARACTER (LEN=21), PARAMETER   :: c_pgname='Tiefenlage der Kanten' ! 
    !! Name der fehlenden Gr&ouml;&szlig;en
    CHARACTER (LEN=21), PARAMETER   :: c_pgmiss='Tiefenlage der Knoten' ! 
    !! Statusvariable STAT
    INTEGER                         :: stat       ! 
    !! Startknoten der Kante
    INTEGER , POINTER :: p_jb(:) !  
    !! Endknoten der Kante
    INTEGER , POINTER :: p_jt(:) !  
    !! Kantenverzeichnis der Polygone
    INTEGER , POINTER :: p_is(:,:) ! 
    !! Anzahl der Kanten im Polygon
    INTEGER , POINTER :: p_ks(:)              ! 
    !! Verzeichnis f&uuml;r D&auml;mme und Wehre
    INTEGER , POINTER :: p_isdam(:,:,:)       ! 
    !! Tiefe an den Kanten
    REAL (KIND=Double) , POINTER :: p_hu(:)   ! 
    !! Tiefe an den Knoten
    REAL (KIND=Double) , POINTER :: p_hv(:)   ! 
    !! Tiefe &uuml;ber den Kanten
    REAL (KIND=Double) , ALLOCATABLE :: hu(:) ! 
    !! Z&auml;hlervariable
    INTEGER :: i, j ! 
    !
    ivar = get_h_grid_variant_no(this)
    !
    SELECT CASE ( ivar )
    CASE ( 1,2,3,4,5,6 ) ! "gitter05", "untrim", "selafin", "delft3d"
       NULLIFY ( p_hv, p_jb, p_jt )
       ! Holen der zur Berechnung erforderlichen Daten
       p_hv => get_hv_object ( this )
       IF ( ASSOCIATED( p_hv ) ) THEN
          p_jb => get_jb_object ( this )
          p_jt => get_jt_object ( this )
          p_is => get_is_object ( this )
          p_ks => get_ks_object ( this )
          ! ggf. Neuberechnen von jb und jt erforderlich
          IF ( .NOT. ASSOCIATED( p_jb ) .OR. &
               .NOT. ASSOCIATED( p_jt ) .OR. &
               .NOT. ASSOCIATED( p_is )      ) THEN
             CALL derive_is ( this )
             p_jb => get_jb_object ( this )
             p_jt => get_jt_object ( this )
             p_is => get_is_object ( this )
          END IF
          IF ( no_error( ) ) THEN
             ! Allokieren von Memory fuer das Berechnungsergebnis
             ALLOCATE ( hu(SIZE(p_jb)), STAT=stat )
             ! Fehlermeldung / Berechnung
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), -25001, c_upname, c_modname, stat )
                CALL setup_error_act ( '<AllocGroessen>', c_pgname )
             ELSE
                DO j=1,SIZE(hu(:))
                   hu(j) = 0.5_Double*(p_hv(p_jb(j))+p_hv(p_jt(j)))
                END DO
                ! Transfer der Daten
                CALL setup_hu_object ( this, hu )
                ! De-Allokieren
                DEALLOCATE ( hu, STAT=stat )
                IF ( stat /= 0 ) THEN
                   CALL setup_error_act ( all_errors(:), -25002, c_upname, c_modname, stat )
                   CALL setup_error_act ( '<DeAllocGroessen>', c_pgname )
                END IF
             END IF
          END IF
          ! ggf. Beruecksichtigen verschiedener duenner Waende / Daemme fuer Delft3D
          IF ( no_error( ) ) THEN
             SELECT CASE ( ivar )
             CASE ( 6 )
                p_hu    => get_hu_object( this )
                p_isdam => get_isdam_object ( this )
                IF (       ASSOCIATED( p_isdam ) .AND. ASSOCIATED( p_ks ) &
                     .AND. ASSOCIATED( p_is    ) .AND. ASSOCIATED( p_hu ) ) THEN
                   DO i=1,SIZE(p_is,1)
                      DO j=1,p_ks(i)
                         IF ( p_isdam(i,j,1) > 0 ) THEN
                            SELECT CASE ( p_isdam(i,j,2) )
                            CASE ( 1 )
                               IF ( ASSOCIATED( this%hland ) ) THEN
                                  p_hu(p_is(i,j)) = this%hland - 1.0_Double
                                  ! p_hu(p_is(i,j)) = this%hland + 1.0_Double
                               ELSE
                                  p_hu(p_is(i,j)) = -10001.0_Double
                                  ! p_hu(p_is(i,j)) = -9999.0_Double
                               END IF
                            CASE ( 2 )
                               IF ( ASSOCIATED( this%lwl ) ) THEN
                                  p_hu(p_is(i,j)) = this%lwl(p_isdam(i,j,1))%sill_depth
                               END IF
                            CASE ( 3 )
                               IF ( ASSOCIATED( this%ext ) ) THEN
                                  p_hu(p_is(i,j)) = this%ext(p_isdam(i,j,1))%sill_depth
                               END IF
                            END SELECT
                         END IF
                      END DO
                   END DO
                END IF
                NULLIFY ( p_hu, p_isdam )
             END SELECT
          END IF
       ELSE
          CALL setup_error_act ( all_errors(:), -25003, c_upname, c_modname )
          CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
          CALL setup_error_act ( '<FehlendeGroesse>', c_pgmiss ) 
       END IF
       NULLIFY ( p_hv, p_jb, p_jt, p_is, p_ks )
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
       CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I12)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
  END SUBROUTINE derive_hu_d
  !
  !! Tiefe &uuml;ber den Polygonen aus der Tiefe der Kanten neu berechnen <BR>
  !! die Tiefe der Polygone wird in dem Objekt "this" abgelegt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE derive_hw_d &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER       :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER   :: c_upname='derive_hw_d' ! 
    !! Name der angeforderten Gr&ouml;&szlig;e
    CHARACTER (LEN=23), PARAMETER   :: c_pgname='Tiefenlage der Polygone' ! 
    !! Name der fehlenden Gr&ouml;&szlig;en
    CHARACTER (LEN=21), PARAMETER   :: c_pgmiss='Tiefenlage der Kanten' ! 
    !! Statusvariable STAT
    INTEGER                         :: stat       ! 
    !! Anzahl der Knoten/Kanten in den Polygonen
    INTEGER            , POINTER :: p_ks(:)   ! 
    !! Kantenverzeichnis der Polygone
    INTEGER            , POINTER :: p_is(:,:) ! 
    !! Tiefe an den Knoten
    REAL (KIND=Double) , POINTER :: p_hu(:)   ! 
    !! Tiefe &uuml;ber den Kanten
    REAL (KIND=Double) , ALLOCATABLE :: hw(:) ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    ivar = get_h_grid_variant_no(this)
    !
    SELECT CASE ( ivar )
    CASE ( 1,2,3,4,5,6 ) ! "gitter05", "untrim", "selafin", "delft3d"
       NULLIFY ( p_hu, p_ks, p_is )
       ! Holen der zur Berechnung erforderlichen Daten
       p_ks => get_ks_object ( this )
       IF ( .NOT. ASSOCIATED(p_ks) ) THEN
          CALL derive_ks ( this )
          p_ks  => get_ks_object ( this )
       END IF
       p_is => get_is_object ( this )
       p_hu => get_hu_object ( this )
       ! ggf. p_is neu berechnen
       IF ( .NOT. ASSOCIATED( p_is ) ) THEN
          CALL derive_is ( this )
          IF ( no_error( ) ) p_is => get_is_object ( this )
       END IF
       ! ggf. p_hu neu berechnen
       IF ( no_error( ) .AND. .NOT. ASSOCIATED( p_hu ) ) THEN
          CALL derive_hu ( this )
          IF ( no_error( ) ) p_hu => get_hu_object ( this )
       END IF
       IF ( ASSOCIATED( p_hu ) ) THEN
          IF ( no_error( ) ) THEN
             ! Allokieren von Memory fuer das Berechnungsergebnis
             ALLOCATE ( hw(SIZE(p_ks(:))), STAT=stat )
             ! Fehlermeldung / Berechnung
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), -25001, c_upname, c_modname, stat )
                CALL setup_error_act ( '<AllocGroessen>', c_pgname )
             ELSE
                DO i=1,SIZE(hw(:))
                   hw(i) = MAXVAL(p_hu(p_is(i,1:p_ks(i))))
                END DO
                ! Transfer der Daten
                CALL setup_hw_object ( this, hw )
                ! De-Allokieren
                DEALLOCATE ( hw, STAT=stat )
                IF ( stat /= 0 ) THEN
                   CALL setup_error_act ( all_errors(:), -25002, c_upname, c_modname, stat )
                   CALL setup_error_act ( '<DeAllocGroessen>', c_pgname )
                END IF
             END IF
          END IF
       ELSE
          CALL setup_error_act ( all_errors(:), -25003, c_upname, c_modname )
          CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
          CALL setup_error_act ( '<FehlendeGroesse>', c_pgmiss ) 
       END IF
       NULLIFY ( p_hu, p_ks, p_is )
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
       CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I12)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
  END SUBROUTINE derive_hw_d
  !
  !! Polygonverzeichnis der Polygone neu berechnen <BR>
  !! das Verzeichnis wird in dem Objekt "this" abgelegt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE derive_ie_d &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER       :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER   :: c_upname='derive_ie_d' ! 
    !! Name der angeforderten Gr&ouml;&szlig;e
    CHARACTER (LEN=31), PARAMETER   :: c_pgname='Polygonverzeichnis der Polygone' ! 
    !! Statusvariable STAT
    INTEGER                         :: stat       ! 
    !! Anzahl der Knoten/Kanten in den Polygonen
    INTEGER            , POINTER :: p_ks(:)   ! 
    !! Kantenverzeichnis der Polygone
    INTEGER            , POINTER :: p_is(:,:) ! 
    !! Polygonverzeichnis der Kanten
    INTEGER            , POINTER :: p_je(:,:) ! 
    !! Polygonverzeichnis der Polygone
    INTEGER        , ALLOCATABLE :: ie(:,:) !  
    !! Z&auml;hlervariable
    INTEGER :: i, l ! 
    !
    ivar = get_h_grid_variant_no(this)
    !
    SELECT CASE ( ivar )
    CASE ( 1,2,3,4,5,6 ) ! "gitter05", "untrim", "selafin", "delft3d"
       NULLIFY ( p_ks, p_is, p_je )
       ! Holen der zur Berechnung erforderlichen Daten
       p_is => get_is_object ( this )
       ! ggf. p_is neu berechnen
       IF ( .NOT. ASSOCIATED( p_is ) ) THEN
          CALL derive_is ( this )
          IF ( no_error( ) ) p_is => get_is_object ( this )
       END IF
       p_ks => get_ks_object ( this )
       IF ( .NOT. ASSOCIATED(p_ks) ) THEN
          CALL derive_ks ( this )
          p_ks  => get_ks_object ( this )
       END IF
       p_je => get_je_object ( this )
       IF ( .NOT. ASSOCIATED ( p_je ) ) THEN
          CALL derive_je ( this )
          p_je => get_je_object ( this )
       END IF
       IF ( no_error( ) ) THEN
          ! Allokieren von Memory fuer das Berechnungsergebnis
          ALLOCATE ( ie(SIZE(p_is(:,:),DIM=1),SIZE(p_is(:,:),DIM=2)), STAT=stat )
          ! Fehlermeldung / Berechnung
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), -25001, c_upname, c_modname, stat )
             CALL setup_error_act ( '<AllocGroessen>', c_pgname )
          ELSE
             DO i=1,SIZE(p_ks(:))
                DO l=1,p_ks(i)
                   ie(i,l) = MERGE( p_je(p_is(i,l),1), &
                        p_je(p_is(i,l),2), p_je(p_is(i,l),1)/=i )
                END DO
             END DO
             ! Transfer der Daten
             CALL setup_ie_object ( this, ie )
             ! De-Allokieren
             DEALLOCATE ( ie, STAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), -25002, c_upname, c_modname, stat )
                CALL setup_error_act ( '<DeAllocGroessen>', c_pgname )
             END IF
          END IF
       END IF
       NULLIFY ( p_ks, p_is, p_je )
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
       CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I12)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
  END SUBROUTINE derive_ie_d
  !
  !! Abstand zwischen den Zentren benachbarter Polygone neu berechnen <BR>
  !! der ABstand wird in dem Objekt "this" abgelegt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE derive_dx_d &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER       :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER   :: c_upname='derive_dx_d' ! 
    !! Name der angeforderten Gr&ouml;&szlig;e
    CHARACTER (LEN=31), PARAMETER   :: c_pgname='Abstand zwischen Polygonzentren' ! 
    !! Statusvariable STAT
    INTEGER                         :: stat      ! 
    !! minimaler Abstand 
    REAL (KIND=Double)    , POINTER :: p_dxmin   ! 
    !! Polygonverzeichnis der Kanten
    INTEGER               , POINTER :: p_je(:,:) ! 
    !! Zentrumskoordinaten der Polygone
    REAL (KIND=Double)    , POINTER :: p_xc(:,:) ! 
    !! Tiefen &uuml;ber den Kanten
    REAL (KIND=Double)    , POINTER :: p_hu(:)   ! 
    !! Abstand zwischen den zentren der Polygone
    REAL (KIND=Double)    , POINTER :: dx(:)     ! 
    !! Z&auml;hlervariable
    INTEGER :: j ! 
    !
    ivar = get_h_grid_variant_no(this)
    !
    SELECT CASE ( ivar )
    CASE ( 1,2,3,4,5,6 ) ! "gitter05", "untrim", "selafin", "delft3d"
       NULLIFY ( p_je, p_xc, p_hu )
       ! Holen der zur Berechnung erforderlichen Daten
       p_je => get_je_object ( this )
       ! ggf. p_je neu berechnen
       IF ( .NOT. ASSOCIATED( p_je ) ) THEN
          CALL derive_je ( this )
          IF ( no_error( ) ) p_je => get_je_object ( this )
       END IF
       p_xc => get_xc_object ( this )
       ! ggf. p_xc neu berechnen
       IF ( no_error( ) .AND. .NOT. ASSOCIATED( p_xc ) ) THEN
          CALL derive_xc ( this )
          IF ( no_error( ) ) p_xc => get_xc_object ( this )
       END IF
       p_hu => get_hu_object ( this )
       ! ggf. p_hu neu berechnen
       IF ( no_error( ) .AND. .NOT. ASSOCIATED( p_hu ) ) THEN
          CALL derive_hu ( this )
          IF ( no_error( ) ) p_hu => get_hu_object ( this )
       END IF
       p_dxmin => get_dxmin_object ( this )
       ! ggf. auf 0.0 setzen
       IF ( no_error( ) .AND. .NOT. ASSOCIATED( p_dxmin ) ) THEN
          CALL setup_dxmin_object ( this, 0.0_Double )
          IF ( no_error( ) ) p_dxmin => get_dxmin_object ( this )
       END IF
       IF ( no_error( ) ) THEN
          ! Allokieren von Memory fuer das Berechnungsergebnis
          ALLOCATE ( dx(SIZE(p_hu(:))), STAT=stat )
          ! Fehlermeldung / Berechnung
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), -25001, c_upname, c_modname, stat )
             CALL setup_error_act ( '<AllocGroessen>', c_pgname )
          ELSE
             DO j=1,SIZE(dx(:))
                IF ( ALL( p_je(j,:) > 0 ) ) THEN
                   dx(j) = MAX( p_dxmin, &
                        SQRT(   (p_xc(p_je(j,2),1) - p_xc(p_je(j,1),1) )**2 &
                              + (p_xc(p_je(j,2),2) - p_xc(p_je(j,1),2) )**2 ) )
                ELSE
                   dx(j) = p_dxmin  
                END IF
             END DO
             ! Transfer der Daten
             CALL setup_dx_object ( this, dx )
             ! De-Allokieren
             DEALLOCATE ( dx, STAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), -25002, c_upname, c_modname, stat )
                CALL setup_error_act ( '<DeAllocGroessen>', c_pgname )
             END IF
          END IF
       END IF
       NULLIFY ( p_je, p_xc, p_hu )
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
       CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I12)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
  END SUBROUTINE derive_dx_d
  !
  !! L&auml;nge der Kanten der Polygone neu berechnen <BR>
  !! der Abstand wird in dem Objekt "this" abgelegt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE derive_dy_d &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER       :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER   :: c_upname='derive_dy_d' ! 
    !! Name der angeforderten Gr&ouml;&szlig;e
    CHARACTER (LEN=24), PARAMETER   :: c_pgname='Laenge der Polygonkanten' ! 
    !! Statusvariable STAT
    INTEGER                         :: stat      ! 
    !! Startknoten einer Kante
    INTEGER               , POINTER :: p_jb(:)   ! 
    !! Endknoten einer Kante
    INTEGER               , POINTER :: p_jt(:)   ! 
    !! Koordinaten der Knoten
    REAL (KIND=Double)    , POINTER :: p_xy(:,:) ! 
    !! L&auml;nge der Kanten der Polygone
    REAL (KIND=Double)    , POINTER :: dy(:)     ! 
    !! Z&auml;hlervariable
    INTEGER :: j ! 
    !
    ivar = get_h_grid_variant_no(this)
    !
    SELECT CASE ( ivar )
    CASE ( 1,2,3,4,5,6 ) ! "gitter05", "untrim", "selafin", "delft3d"
       NULLIFY ( p_jb, p_jt, p_xy )
       ! Holen der zur Berechnung erforderlichen Daten
       p_jb => get_jb_object ( this )
       p_jt => get_jt_object ( this )
       p_xy => get_xy_object ( this )
       ! ggf. p_jb und p_jt neu berechnen
       IF ( .NOT. ASSOCIATED( p_jb ) .OR. &
            .NOT. ASSOCIATED( p_jt )        ) THEN
          CALL derive_is ( this )
          IF ( no_error( ) ) p_jb => get_jb_object ( this )
          IF ( no_error( ) ) p_jt => get_jt_object ( this )
       END IF
       IF ( no_error( ) ) THEN
          ! Allokieren von Memory fuer das Berechnungsergebnis
          ALLOCATE ( dy(SIZE(p_jb(:))), STAT=stat )
          ! Fehlermeldung / Berechnung
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), -25001, c_upname, c_modname, stat )
             CALL setup_error_act ( '<AllocGroessen>', c_pgname )
          ELSE
             DO j=1,SIZE(dy(:))
                dy(j) = SQRT( (p_xy(p_jt(j),1)-p_xy(p_jb(j),1))**2 &
                     +  (p_xy(p_jt(j),2)-p_xy(p_jb(j),2))**2 )
             END DO
             ! Transfer der Daten
             CALL setup_dy_object ( this, dy )
             ! De-Allokieren
             DEALLOCATE ( dy, STAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), -25002, c_upname, c_modname, stat )
                CALL setup_error_act ( '<DeAllocGroessen>', c_pgname )
             END IF
          END IF
       END IF
       NULLIFY ( p_jb, p_jt, p_xy )
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
       CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I12)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
  END SUBROUTINE derive_dy_d
  !
  !! Abstand zwischen den Schwerpunkten benachbarter Polygone neu berechnen <BR>
  !! der Abstand wird in dem Objekt "this" abgelegt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE derive_dg_d ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER       :: this      ! 
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER   :: c_upname='derive_dg_d' ! 
    !! Name der angeforderten Gr&ouml;&szlig;e
    CHARACTER (LEN=30), PARAMETER   :: c_pgname='Abstand zwischen Schwerpunkten' ! 
    !! Statusvariable STAT
    INTEGER                         :: stat      ! 
    !! minimaler Abstand 
    REAL (KIND=Double)    , POINTER :: p_dxmin   ! 
    !! Polygonverzeichnis der Kanten
    INTEGER               , POINTER :: p_je(:,:) ! 
    !! Schwerpunktskoordinaten der Polygone
    REAL (KIND=Double)    , POINTER :: p_xg(:,:) ! 
    !! Tiefen &uuml;ber den Kanten
    REAL (KIND=Double)    , POINTER :: p_hu(:)   ! 
    !! Abstand zwischen den Schwerpunkten der Polygone
    REAL (KIND=Double)    , POINTER :: dg(:)     ! 
    !! Z&auml;hlervariable
    INTEGER :: j ! 
    !
    ivar = get_h_grid_variant_no(this)
    !
    SELECT CASE ( ivar )
    CASE ( 1,2,3,4,5,6 ) ! "gitter05", "untrim", "selafin", "delft3d"
       NULLIFY ( p_je, p_xg, p_hu, p_dxmin )
       ! Holen der zur Berechnung erforderlichen Daten
       p_je => get_je_object ( this )
       ! ggf. p_je neu berechnen
       IF ( .NOT. ASSOCIATED( p_je ) ) THEN
          CALL derive_je ( this )
          IF ( no_error( ) ) p_je => get_je_object ( this )
       END IF
       p_xg => get_xg_object ( this )
       ! ggf. p_xg neu berechnen
       IF ( no_error( ) .AND. .NOT. ASSOCIATED( p_xg ) ) THEN
          CALL derive_xg ( this )
          IF ( no_error( ) ) p_xg => get_xg_object ( this )
       END IF
       p_hu => get_hu_object ( this )
       ! ggf. p_hu neu berechnen
       IF ( no_error( ) .AND. .NOT. ASSOCIATED( p_hu ) ) THEN
          CALL derive_hu ( this )
          IF ( no_error( ) ) p_hu => get_hu_object ( this )
       END IF
       p_dxmin => get_dxmin_object ( this )
       ! ggf. auf 0.0 setzen
       IF ( no_error( ) .AND. .NOT. ASSOCIATED( p_dxmin ) ) THEN
          CALL setup_dxmin_object ( this, 0.0_Double )
          IF ( no_error( ) ) p_dxmin => get_dxmin_object ( this )
       END IF
       IF ( no_error( ) ) THEN
          ! Allokieren von Memory fuer das Berechnungsergebnis
          ALLOCATE ( dg(SIZE(p_hu)), STAT=stat )
          ! Fehlermeldung / Berechnung
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), -25001, c_upname, c_modname, stat )
             CALL setup_error_act ( '<AllocGroessen>', c_pgname )
          ELSE
             DO j=1,SIZE(dg)
                IF ( ALL( p_je(j,:) > 0 ) ) THEN
                   dg(j) = MAX( p_dxmin, &
                        SQRT(   (p_xg(p_je(j,2),1) - p_xg(p_je(j,1),1) )**2 &
                              + (p_xg(p_je(j,2),2) - p_xg(p_je(j,1),2) )**2 ) )
                ELSE
                   dg(j) = p_dxmin  
                END IF
             END DO
             ! Transfer der Daten
             CALL setup_dg_object ( this, dg )
             ! De-Allokieren
             DEALLOCATE ( dg, STAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), -25002, c_upname, c_modname, stat )
                CALL setup_error_act ( '<DeAllocGroessen>', c_pgname )
             END IF
          END IF
       END IF
       NULLIFY ( p_je, p_xg, p_hu, p_dxmin )
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
       CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I12)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
  END SUBROUTINE derive_dg_d
  !
  !! Anzahl der internen Kanten ermitteln <BR>
  !! die Anzahl der internen Kanten wird in dem Objekt "this" abgelegt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE derive_nsi_d &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER       :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER   :: c_upname='derive_nsi_d' ! 
    !! Name der angeforderten Gr&ouml;&szlig;e
    CHARACTER (LEN=26), PARAMETER   :: c_pgname='Anzahl der internen Kanten' ! 
    !! Polygonverzeichnis der Kanten
    INTEGER               , POINTER :: p_je(:,:) ! 
    !! Tiefen auf den Kanten
    REAL (KIND=Double)    , POINTER :: p_hu(:)   ! 
    !! Tiefe f&uuml;r Landkanten
    REAL (KIND=Double)    , POINTER :: p_hl      ! 
    !! Z&auml;hlervariable
    INTEGER :: nsi, j ! 
    !
    ivar = get_h_grid_variant_no(this)
    !
    SELECT CASE ( ivar )
    CASE ( 1,2,3,4,5,6 ) ! "gitter05", "untrim", "selafin", "delft3d"
       NULLIFY ( p_je, p_hu, p_hl )
       ! Holen der zur Berechnung erforderlichen Daten
       p_je => get_je_object    ( this )
       p_hu => get_hu_object    ( this )
       p_hl => get_hland_object ( this )
       ! ggf. p_je neu berechnen
       IF ( .NOT. ASSOCIATED( p_je ) ) THEN
          CALL derive_je ( this )
          IF ( no_error( ) ) p_je => get_je_object ( this )
       END IF
       ! ggf. p_hu neu berechnen
       IF ( .NOT. ASSOCIATED( p_hu ) ) THEN
          CALL derive_hu ( this )
          IF ( no_error( ) ) p_hu => get_hu_object ( this )
       END IF
       IF ( no_error( ) ) THEN
          nsi = 0
          IF ( ASSOCIATED( p_je ) .AND. ASSOCIATED( p_hu ) .AND. ASSOCIATED( p_hl ) ) THEN
             DO j=1,SIZE(p_hu)
                IF ( ALL( p_je(j,:) > 0 ) .AND. p_hu(j) > p_hl ) nsi = nsi + 1
             END DO
          END IF
          IF ( ASSOCIATED( p_je ) .AND. ASSOCIATED( p_hu ) .AND. .NOT. ASSOCIATED( p_hl ) ) THEN
             DO j=1,SIZE(p_hu)
                IF ( ALL( p_je(j,:) > 0 ) ) nsi = nsi + 1
             END DO
          END IF
       ELSE
          nsi = 0
       END IF
       NULLIFY ( p_je, p_hu, p_hl )
       CALL setup_nsi_object ( this, nsi )
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
       CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I12)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
  END SUBROUTINE derive_nsi_d
  !
  !! letzte Kante mit Fluss-Randbedingung setzen <BR>
  !! die Anzahl der internen Kanten wird in dem Objekt "this" abgelegt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE derive_nsf_d &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER       :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER   :: c_upname='derive_nsf_d' ! 
    !! Name der angeforderten Gr&ouml;&szlig;e
    CHARACTER (LEN=26), PARAMETER   :: c_pgname='letzte Kante mit Fluss-Rb.' ! 
    !! Polygonverzeichnis der Kanten
    INTEGER               , POINTER :: p_nsi      ! 
    !! Anzahl der Kanten
    INTEGER               , POINTER :: p_ns       ! 
    !! Knotenverzeichnis der Polygone
    INTEGER               , POINTER :: p_nen(:,:) ! 
    !! Anzahl der Knoten/Kanten in den Polygonen 
    INTEGER               , POINTER :: p_ks(:)    ! 
    !! Kantenverzeichnis der Polygone
    INTEGER               , POINTER :: p_is(:,:)  ! 
    !! Anfangs- und Endknoten einer Kante
    INTEGER               , POINTER :: p_jb(:), p_jt(:) ! 
    !! Nachbarverzeichnis der Kanten
    INTEGER               , POINTER :: p_je(:,:)        ! 
    !! Randkennungsverzeichnis der Kanten
    INTEGER               , POINTER :: p_isbnd(:,:)     ! 
    
    !! Koordinaten der Kantenmitten
    REAL (KIND=Double)    , POINTER :: p_xs(:,:)        ! 
    !! Abstand zwischen den Zentren benachbarter Polygone
    REAL (KIND=Double)    , POINTER :: p_dx(:)          ! 
    !! L&auml;nge der Kanten
    REAL (KIND=Double)    , POINTER :: p_dy(:)          ! 
    !! Tiefe an den Kanten
    REAL (KIND=Double)    , POINTER :: p_hu(:)          ! 
    !! diverse Hilfsfelder
    LOGICAL , ALLOCATABLE :: l_data1(:)                 ! 
    !! Hilfsvariable
    INTEGER :: i, j, k, l, n, nsf ! 
    REAL (KIND=Double) :: d                 ! 
    !
    ivar = get_h_grid_variant_no(this)
    !
    SELECT CASE ( ivar )
    CASE ( 1,2,3,4,5,6 ) ! "gitter05", "untrim", "selafin", "delft3d"
       NULLIFY ( p_nsi )
       ! Holen der zur Berechnung erforderlichen Daten
       p_nsi => get_nsi_object  ( this )
       ! ggf. p_nsi neu berechnen
       IF ( .NOT. ASSOCIATED( p_nsi ) ) THEN
          CALL derive_nsi ( this )
          IF ( no_error( ) ) p_nsi => get_nsi_object ( this )
       END IF
       IF ( no_error( ) ) THEN
          nsf = p_nsi
       ELSE
          nsf = 0
       END IF
       p_isbnd => get_isbnd_object ( this )
       IF ( ASSOCIATED(p_isbnd) ) THEN ! Sonderbehandlung "Delft3D"
          p_ks  => get_ks_object   ( this )
          p_nen => get_nen_object  ( this )
          IF ( .NOT. ASSOCIATED( p_nen ) ) THEN
             CALL derive_nen ( this )
             p_nen => get_nen_object ( this )
          END IF
          p_is  => get_is_object    ( this )
          IF ( .NOT. ASSOCIATED( p_is ) ) THEN
             CALL derive_is ( this )
             p_is  => get_is_object ( this )
          END IF
          p_jb  => get_jb_object ( this )
          IF ( .NOT. ASSOCIATED( p_jb ) ) THEN
             CALL derive_jb ( this )
             p_jb  => get_jb_object ( this )
          END IF
          p_jt  => get_jt_object ( this )
          IF ( .NOT. ASSOCIATED( p_jt ) ) THEN
             CALL derive_jt ( this )
             p_jt  => get_jt_object ( this )
          END IF
          p_je  => get_je_object ( this )
          IF ( .NOT. ASSOCIATED( p_je ) ) THEN
             CALL derive_je ( this )
             p_je  => get_je_object ( This )
          END IF
          p_xs  => get_xs_object ( this )
          IF ( .NOT. ASSOCIATED( p_xs ) ) THEN
             CALL derive_xs ( this )
             p_xs  => get_xs_object ( this )
          END IF
          p_dx  => get_dx_object ( this )
          IF ( .NOT. ASSOCIATED( p_dx ) ) THEN
             CALL derive_dx ( this )
             p_dx  => get_dx_object ( this )
          END IF
          p_dy  => get_dy_object ( this )
          IF ( .NOT. ASSOCIATED( p_dy ) ) THEN
             CALL derive_dy ( this )
             p_dy  => get_dy_object ( this )
          END IF
          p_hu  => get_hu_object ( this )
          IF ( .NOT. ASSOCIATED( p_hu ) ) THEN
             CALL derive_hu ( this )
             p_hu  => get_hu_object ( this )
          END IF
          p_ns  => get_ns_object ( this )
          ! Ermittle die Kanten mit "Fluss-Rand"
          ALLOCATE   ( l_data1(p_ns) )
          l_data1(:) = .false.
          DO i=1,SIZE(p_nen,1)
             DO j=1,p_ks(i)
                IF ( l_data1(p_is(i,j)) ) CYCLE
                IF ( is_polyedge_current(this,i,j) .OR. is_polyedge_discharge(this,i,j) .OR. &
                     is_polyedge_total_discharge(this,i,j) ) l_data1(p_is(i,j)) = .true. 
             END DO
          END DO
          nsf = p_nsi
          DO j=p_nsi+1,p_ns
             IF (.NOT. l_data1(j)) CYCLE
             nsf = nsf + 1
             IF ( j > nsf ) THEN
!!$                i1 = MAXVAL( p_je(j,:)   ) ! NSF-Kante
!!$                i2 = MAXVAL( p_je(nsf,:) ) ! nicht-NSF-Kante
!!$                j1 = MAXVAL(MINLOC( p_is(i1,:), p_is(i1,:) == j   )) ! Position NSF-Kante
!!$                j2 = MAXVAL(MINLOC( p_is(i2,:), p_is(i2,:) == nsf )) ! Position nicht-NSF-Kante
!!$                ! Kanten im Kantenverzeichnis tauschen
!!$                n           = p_is(i1,j1) 
!!$                p_is(i1,j1) = p_is(i2,j2)
!!$                p_is(i2,j2) = n
                DO l=1,SIZE(p_is,2)
                   DO k=1,SIZE(p_je,2)
                      IF ( p_je(j,k) > 0 ) THEN
                         IF ( p_is(p_je(j,k),l)   == j    ) p_is(p_je(j,k),l)   = -nsf
                      END IF
                   END DO
                END DO
                DO l=1,SIZE(p_is,2)
                   DO k=1,SIZE(p_je,2)
                      IF ( p_je(nsf,k) > 0 ) THEN
                         IF ( p_is(p_je(nsf,k),l) == nsf  ) p_is(p_je(nsf,k),l) = j
                      END IF
                   END DO
                END DO
                DO l=1,SIZE(p_is,2)
                   DO k=1,SIZE(p_je,2)
                      IF ( p_je(j,k) > 0 ) THEN
                         IF ( p_is(p_je(j,k),l)   == -nsf ) p_is(p_je(j,k),l)   = nsf
                      END IF
                   END DO
                END DO
                ! Startknoten tauschen
                n           = p_jb(j)
                p_jb(j)     = p_jb(nsf)
                p_jb(nsf)   = n
                ! Endknoten tauschen
                n           = p_jt(j)
                p_jt(j)     = p_jt(nsf)
                p_jt(nsf)   = n
                ! Elementverzeichnis der Kanten tauschen
                DO i=1,2
                   n           = p_je(j,i)
                   p_je(j,i)   = p_je(nsf,i)
                   p_je(nsf,i) = n
                END DO
                ! Mitten der Kanten tauschen
                DO i=1,2
                   d           = p_xs(j,i)
                   p_xs(j,i)   = p_xs(nsf,i)
                   p_xs(nsf,i) = d
                END DO
                ! Centerpunkt-Abstaende tauschen
                d         = p_dx(j)
                p_dx(j)   = p_dx(nsf)
                p_dx(nsf) = d
                ! Kantenlaengen tauschen
                d         = p_dy(j)
                p_dy(j)   = p_dy(nsf)
                p_dy(nsf) = d
                ! Kantentiefen tauschen
                d         = p_hu(j)
                p_hu(j)   = p_hu(nsf)
                p_hu(nsf) = d
             END IF
          END DO
          DEALLOCATE ( l_data1 )
       END IF
       NULLIFY ( p_nsi, p_ns, p_ks, p_nen, p_is, p_jb, p_jt, p_je, p_xs, p_dx, p_dy )
       CALL setup_nsf_object ( this, nsf )
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
       CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I12)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
  END SUBROUTINE derive_nsf_d
  !
  !! Anzahl der Knoten auf dem aeusseren Rand ermitteln<BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE derive_nrand_d &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER       :: this  ! 
    !
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER   :: c_upname='derive_nrand_d' ! 
    !! Name der angeforderten Gr&ouml;&szlig;e
    CHARACTER (LEN=35), PARAMETER   :: c_pgname='Anzahl Knoten auf aeusserem Rand ' ! 
    !! Knotenverzeichnis der Polygone
    INTEGER            , POINTER    :: p_nen(:,:) ! 
    !! Anzahl der Knoten/Kanten in den Polygonen
    INTEGER            , POINTER    :: p_ks(:)    ! 
    !! Anzahl der Knoten
    INTEGER            , POINTER    :: p_nv       ! 
    !! Anzahl der Gitterkanten
    INTEGER            , POINTER    :: p_ns       ! 
    !! Koordinaten der Knoten
    REAL (KIND=Double)    , POINTER :: p_xy(:,:)  ! 
    !! erster Knoten einer Kante
    INTEGER            , POINTER    :: p_jb(:)    ! 
    !! zweiter Knoten einer Kante
    INTEGER            , POINTER    :: p_jt(:)    ! 
    !! Nachbarverzeichnis der Kanten
    INTEGER            , POINTER    :: p_je(:,:)  ! 
    !! Nachbarverzeichnis der Polygone
    INTEGER            , POINTER    :: p_ie(:,:)  ! 
    !! Kantenverzeichnis der Polygone
    INTEGER            , POINTER    :: p_is(:,:)  ! 
    !
    !! Anzahl der Knoten auf dem aeusseren Rand
    INTEGER    :: nrand                    ! 
    !! Knotenbezeichner
    INTEGER    :: act_b_node, last_b_node  ! 
    !! Kantenbezeichner
    INTEGER    :: act_b_edge, act_l_edge   ! 
    !! Diverse Indizes von Polygonen
    INTEGER    :: act_b_poly               ! 
    !! Z&auml;hlervariable
    INTEGER    :: i, j, n ! 
    !
    ivar = get_h_grid_variant_no(this)
    !
    SELECT CASE ( ivar )
    CASE ( 1,2,3,4,5,6 ) ! "gitter05", "untrim", "selafin", "delft3d"
       !
       NULLIFY ( p_nen, p_nv, p_xy, p_ks, p_ns, p_jb, p_jt, p_je, p_ie, p_is )
       !
       ! Holen der zur Berechnung erforderlichen Daten
       p_nen => get_nen_object ( this )
       IF ( .NOT. ASSOCIATED(p_nen) ) THEN
          CALL derive_nen ( this )
          p_nen => get_nen_object ( this )
       END IF
       p_nv  => get_nv_object  ( this )
       p_xy  => get_xy_object  ( this )
       p_ks  => get_ks_object  ( this )
       IF ( .NOT. ASSOCIATED(p_ks) ) THEN
          CALL derive_ks ( this )
          p_ks  => get_ks_object ( this )
       END IF
       !
       p_ns  => get_ns_object  ( this )
       ! ggf. p_ns neu berechnen
       IF ( p_ns == 0 ) THEN           ! Komponente "ns" wird immer allokiert und mit 0 vorbelegt !
          WRITE(*,*) ' ... automatische Neuberechnung "derive_ns"'
          CALL derive_ns ( this )
          IF ( no_error( ) ) p_ns => get_ns_object ( this )
       END IF
       !
       p_is  => get_is_object  ( this )
       IF ( .NOT. ASSOCIATED( p_is ) ) THEN  ! ggf. p_is neu berechnen
          WRITE(*,*) ' ... automatische Neuberechnung "derive_is"'
          CALL derive_is ( this )
          IF ( no_error( ) ) p_is => get_is_object ( this )
       END IF
       !
       p_jb  => get_jb_object  ( this )
       IF ( .NOT. ASSOCIATED( p_jb ) ) THEN  ! ggf. p_jb neu berechnen
          WRITE(*,*) ' ... automatische Neuberechnung "derive_jb"'
          CALL derive_jb ( this )
          IF ( no_error( ) ) p_jb => get_jb_object ( this )
       END IF
       !
       p_jt  => get_jt_object  ( this )
       IF ( .NOT. ASSOCIATED( p_jt ) ) THEN  ! ggf. p_jt neu berechnen
          WRITE(*,*) ' ... automatische Neuberechnung "derive_jt"'
          CALL derive_jt ( this )
          IF ( no_error( ) ) p_jt => get_jt_object ( this )
       END IF
       !
       p_je  => get_je_object  ( this )
       IF ( .NOT. ASSOCIATED( p_je ) ) THEN  ! ggf. p_je neu berechnen
          WRITE(*,*) ' ... automatische Neuberechnung "derive_je"'
          CALL derive_je ( this )
          IF ( no_error( ) ) p_je => get_je_object ( this )
       END IF
       !
       p_ie  => get_ie_object  ( this )
       IF ( .NOT. ASSOCIATED( p_ie ) ) THEN  ! ggf. p_ie neu berechnen
          WRITE(*,*) ' ... automatische Neuberechnung "derive_ie"'
          CALL derive_ie ( this )
          IF ( no_error( ) ) p_ie => get_ie_object ( this )
       END IF
       !
       ! Initialisierungen -----------------------------------------------------------
       nrand       = 0
       act_b_node  = p_nen(1,1) ! wegen "delft3d"
       last_b_node = 0
       act_b_edge  = 0
       act_l_edge  = 0
       act_b_poly  = 0
       !
       ! ... finde einen beliebigen Knoten auf dem aeusseren Rand -------------------
       ! ... beruecksichtige nur die in p_nen vorliegenden Knoten -------------------
       DO i=1,SIZE(p_nen,1)
          DO j=1,p_ks(i)
             IF ( p_xy(p_nen(i,j),1) < p_xy(act_b_node,1) ) act_b_node = p_nen(i,j)
          END DO
       END DO
       !
       ! ... suche eine der beiden Randkanten dieses Knotens ------------------------
       DO j = 1, p_ns ! ueber alle Kanten
          IF ( act_b_edge /= 0 ) EXIT
          IF ( p_jb(j) == act_b_node  .OR.  p_jt(j) == act_b_node ) THEN
             IF ( ANY( p_je(j,:) <= 0 ) ) THEN
                act_b_edge = j                   
                act_b_poly = MAXVAL( p_je(j,:) ) 
             END IF
          END IF
       END DO
       DO j=1,p_ks(act_b_poly)
          IF ( act_l_edge /= 0 ) EXIT
          IF ( p_is(act_b_poly,j) == act_b_edge ) act_l_edge = j
       END DO
       ! ... den letzten zu suchenden Knoten
       last_b_node = p_nen(act_b_poly,act_l_edge)
       act_b_node  = p_nen(act_b_poly,get_l_no(p_ks(act_b_poly),act_l_edge+1) )
       nrand       = 2
       n           = 0
       !       
       ! ... Aufsammeln aller weiteren Knoten auf dem aeusseren Rand
       DO
          n = 0
          act_l_edge = get_l_no(p_ks(act_b_poly),act_l_edge+1)
          act_b_edge = p_is(act_b_poly,act_l_edge)
          IF ( p_ie(act_b_poly,act_l_edge) <= 0 ) THEN ! ist noch Randkante
             CONTINUE
          ELSE                                         ! ist keine Randkante mehr
             DO
                n          = n + 1
                act_b_poly = p_ie(act_b_poly,act_l_edge)
                act_l_edge = MAXVAL( MINLOC( p_is(act_b_poly,1:p_ks(act_b_poly)), &
                     p_is(act_b_poly,1:p_ks(act_b_poly)) == act_b_edge ))
                act_l_edge = get_l_no(p_ks(act_b_poly),act_l_edge+1)
                act_b_edge = p_is(act_b_poly,act_l_edge)
                IF ( ANY( p_je(act_b_edge,:) <= 0 )          .AND. & 
                        ( p_jb(act_b_edge) == act_b_node .OR. &
                          p_jt(act_b_edge) == act_b_node      ) .OR. n > 20 ) EXIT 
             END DO
          END IF
          act_b_node = p_nen(act_b_poly,get_l_no(p_ks(act_b_poly),act_l_edge+1))
          IF ( act_b_node == last_b_node .OR. n > 20 ) EXIT
          nrand      = nrand + 1
       END DO
       !
       IF ( n > 20 ) THEN
          CALL setup_error_act( all_errors(:), -25100, c_upname, c_modname )
       ELSE
          CALL setup_nrand_object ( this, nrand )
       END IF
       !
       NULLIFY ( p_nen, p_nv, p_xy, p_ks, p_ns, p_jb, p_jt, p_je, p_ie, p_is )
       !
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
       CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I12)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
  END SUBROUTINE derive_nrand_d
  !
  !! Kanten auf dem Rand ermitteln ------------------------------------------------
  SUBROUTINE derive_b_s_d ( this )
    !! Datenobjekt
    TYPE (t_h_grid)    , POINTER :: this                          ! 
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER   :: c_upname='derive_b_s_d' ! 
    !! Name der angeforderten Gr&ouml;&szlig;e
    CHARACTER (LEN=21), PARAMETER   :: c_pgname='sortierte Rand-Kanten' ! 
    !! Hilfsvariablen
    INTEGER            , POINTER :: p_je(:,:)                     ! 
    INTEGER            , POINTER :: p_jb(:), p_jt(:)              ! 
    REAL (KIND=Double) , POINTER :: p_xy(:,:)                     ! 
    INTEGER , ALLOCATABLE        :: l_b_s(:), l_b_ms(:), l_b_v(:) ! 
    INTEGER , POINTER            :: p_b_t(:)                      !  
    INTEGER                      :: i, j, k, n, ia, ie, ns, nf, nbs, mvs, mss, memo, l(1), m(1), ivar ! 
    INTEGER                      :: nj, mjs, n_nf, j_i(2), j_j(2), j_mvs(2) ! 
    REAL (KIND=Double)           :: x1, x2, y1, y2 ! 
    !
    ivar = get_h_grid_variant_no(this)
    !
    SELECT CASE ( ivar )
    CASE ( 1,2,3,4,5,6 ) ! "gitter05", "untrim", "selafin", "delft3d"
       !
       p_je => get_je_object ( this )
       IF ( .NOT. ASSOCIATED( p_je ) ) THEN
          CALL derive_je ( this )
          p_je => get_je_object ( this )
       END IF
       p_jb => get_jb_object ( this )
       IF ( .NOT. ASSOCIATED( p_jb ) ) THEN
          CALL derive_jb ( this )
          p_jb => get_jb_object ( this )
       END IF
       p_jt => get_jt_object ( this )
       IF ( .NOT. ASSOCIATED( p_jt ) ) THEN
          CALL derive_jt ( this )
          p_jt => get_jt_object ( this )
       END IF
       p_xy => get_xy_object ( this )
       !
       ! Randkanten sind alle Kanten, die nur ein Nachbarpolygon besitzen
       nbs = COUNT( p_je(:,:) <= 0  )
       IF ( nbs > 0 ) THEN
          ALLOCATE( l_b_s(nbs), l_b_ms(nbs), l_b_v(nbs) )
          l_b_s(:) = -1 ; l_b_ms(:) = -1 ; l_b_v(:) = -1
          n = 0
          DO i=1,SIZE(p_je,1)
             IF ( ANY( p_je(i,:) <= 0 ) ) THEN
                n        = n + 1
                l_b_s(n) = i
             END IF
          END DO
          ! finde den Randpunkt mit der kleinsten Koordinate
          l   = MINLOC( p_xy(p_jb,1) )
          m   = MINLOC( p_xy(p_jt,1) )
          mvs = MERGE( p_jb(l(1)), p_jt(m(1)), p_xy(p_jb(l(1)),1) <= p_xy(p_jt(m(1)),1) ) ! vertex
          mss = MERGE( l(1)      , m(1)      , p_xy(p_jb(l(1)),1) <= p_xy(p_jt(m(1)),1) ) ! edge
          ns        = 1 ! Hauptabschnittszaehler
          nf        = 1 ! Position des ersten Knotens des aktuellen Hauptabschnitts
          l_b_v(nf) = mvs
          n_nf      = 0 ! Zaehler fuer die Anzahl der Kanten in denen l_b_v(nf) vorkommt
          DO j=1,SIZE(l_b_s)
             IF ( p_jb(l_b_s(j)) == l_b_v(nf) .OR. p_jt(l_b_s(j)) == l_b_v(nf) ) n_nf = n_nf + 1
          END DO
          n_nf = n_nf/2
          DO i=1,SIZE(l_b_s)
             IF ( ns == 1 .OR. ivar /= 6 ) THEN 
                DO j=i,SIZE(l_b_s)
                   IF ( l_b_ms(i) > 0 ) EXIT
                   IF      ( mvs == p_jb(l_b_s(j)) .AND. p_je(l_b_s(j),2) <= 0 ) THEN
                      memo      = l_b_s(i)
                      l_b_s(i)  = l_b_s(j)
                      l_b_s(j)  = memo
                      mvs       = p_jt(l_b_s(i))
                      l_b_ms(i) = ns
                      IF ( l_b_v(nf) /= mvs                ) l_b_v(i+1) = mvs
                      IF ( l_b_v(nf) == mvs .AND. n_nf > 1 ) l_b_v(i+1) = mvs
                   ELSE IF ( mvs == p_jt(l_b_s(j)) .AND. p_je(l_b_s(j),1) <= 0 ) THEN
                      memo      = l_b_s(i)
                      l_b_s(i)  = l_b_s(j)
                      l_b_s(j)  = memo
                      mvs       = p_jb(l_b_s(i))
                      l_b_ms(i) = ns
                      IF ( l_b_v(nf) /= mvs                ) l_b_v(i+1) = mvs
                      IF ( l_b_v(nf) == mvs .AND. n_nf > 1 ) l_b_v(i+1) = mvs
                   END IF
                END DO
             ELSE ! Hauptabschnittsnummern > 1 fuer "DELFT3D"
                nj    = 0 ! Zaehler mit den moeglichen Kanten (maximal zwei)
                DO j=i,SIZE(l_b_s)
                   IF ( nj == 2 ) EXIT
                   IF      ( mvs == p_jb(l_b_s(j)) .AND. p_je(l_b_s(j),2) <= 0 ) THEN
                      nj        = nj + 1
                      j_i(nj)   = i
                      j_j(nj)   = j
                      j_mvs(nj) = p_jt(l_b_s(j))
                   ELSE IF ( mvs == p_jt(l_b_s(j)) .AND. p_je(l_b_s(j),1) <= 0 ) THEN
                      nj        = nj + 1
                      j_i(nj)   = i
                      j_j(nj)   = j
                      j_mvs(nj) = p_jb(l_b_s(j))
                   END IF
                END DO
                SELECT CASE ( nj )
                CASE ( 1 ) ! nur eine Kante gefunden
                   memo            = l_b_s(j_i(nj))
                   l_b_s(j_i(nj))  = l_b_s(j_j(nj))
                   l_b_s(j_j(nj))  = memo
                   mvs             = j_mvs(nj)
                   l_b_ms(j_i(nj)) = ns
                   IF ( l_b_v(nf) /= mvs .OR. n_nf == 2 ) l_b_v(i+1) = mvs ! erster Punkt wird nur ein Mal eingespeichert
                   mjs             = l_b_s(j_i(nj))
                CASE ( 2 ) ! falls zwei Kanten gefunden wurden, kann es nur eine sein
                   IF ( mvs == p_jt(mjs) ) THEN
                      x1 = p_xy(p_jt(mjs),1) - p_xy(p_jb(mjs),1)
                      y1 = p_xy(p_jt(mjs),2) - p_xy(p_jb(mjs),2)
                   ELSE
                      x1 = p_xy(p_jb(mjs),1) - p_xy(p_jt(mjs),1)
                      y1 = p_xy(p_jb(mjs),2) - p_xy(p_jt(mjs),2)
                   END IF
                   DO k=1,nj
                      IF ( l_b_ms(i) > 0 ) EXIT
                      IF ( mvs == p_jb(l_b_s(j_j(k))) ) THEN
                         x2 = p_xy(p_jt(l_b_s(j_j(k))),1) - p_xy(p_jb(l_b_s(j_j(k))),1)
                         y2 = p_xy(p_jt(l_b_s(j_j(k))),2) - p_xy(p_jb(l_b_s(j_j(k))),2)
                      ELSE
                         x2 = p_xy(p_jb(l_b_s(j_j(k))),1) - p_xy(p_jt(l_b_s(j_j(k))),1)
                         y2 = p_xy(p_jb(l_b_s(j_j(k))),2) - p_xy(p_jt(l_b_s(j_j(k))),2)
                      END IF
                      IF ( x1*y2 - x2*y1 >= 0.0_Double ) THEN
                         memo           = l_b_s(j_i(k))
                         l_b_s(j_i(k))  = l_b_s(j_j(k))
                         l_b_s(j_j(k))  = memo
                         mvs            = j_mvs(k)
                         l_b_ms(j_i(k)) = ns
                         IF ( l_b_v(nf) /= mvs .OR. n_nf == 2 ) l_b_v(i+1) = mvs ! erster Punkt wird nur ein Mal eingespeichert
                         mjs            = l_b_s(j_i(k))
                      END IF
                   END DO
                END SELECT
             END IF
             IF ( l_b_v(nf) == mvs .AND. i < SIZE(l_b_s) ) THEN
                n_nf = n_nf - 1 ! an "Kreuzungen" kann "mvs" zwei Mal auftreten
                IF ( n_nf <= 0 ) THEN
                   ns  = ns + 1 ! Hauptabschnitt
                   nf  = i  + 1 ! 
                   mvs = p_jb(l_b_s(i+1))
                   mjs = l_b_s(i+1) ! erste Kantennummer
                   l_b_v(nf) = mvs
                   n_nf = 0
                   DO j=1,SIZE(l_b_s)
                      IF ( l_b_ms(j) > 0 ) CYCLE
                      IF ( p_jb(l_b_s(j)) == l_b_v(nf) .OR. p_jt(l_b_s(j)) == l_b_v(nf) ) n_nf = n_nf + 1
                   END DO
                   n_nf = n_nf/2
                END IF
             END IF
          END DO
          ! Einstweiliges Setzen der Groessen ------------------------------------
          CALL setup_b_ms_object ( this, l_b_ms )
          CALL setup_b_s_object  ( this, l_b_s  )
          CALL setup_b_v_object  ( this, l_b_v  )
          ! Erneutes Sortieren des aeusseren Randes, so dass der erste Punkt auf -
          ! eine Grenze zwischen geschlossen | offen faellt (falls vorhanden) ----
          p_b_t => get_b_t_object ( this )
          IF ( .NOT. ASSOCIATED(p_b_t) ) THEN
             CALL derive_b_t ( this )
             p_b_t => get_b_t_object ( this )
          END IF
          IF ( ASSOCIATED(p_b_t) ) THEN
             ia = 1
             ie = COUNT( l_b_ms == 1 )
             IF ( .NOT. ( COUNT(p_b_t==0) == 0 .OR. COUNT(p_b_t==0) == ie ) ) THEN
                n = 0
                DO i=ie,ia,-1
                   IF ( p_b_t(i) /= p_b_t(1) ) EXIT
                   n = n + 1
                END DO
                IF ( n > 0 ) THEN
                   p_b_t(:) = l_b_s(:)
                   DO i=ia,ie
                      j = MERGE( i-n, ie-n+i, i > n )
                      l_b_s(i) = p_b_t(j)
                   END DO
                   p_b_t(:) = l_b_v(:)
                   DO i=ia,ie
                      j = MERGE( i-n, ie-n+i, i > n )
                      l_b_v(i) = p_b_t(j)
                   END DO
                END IF
             END IF
          END IF
          NULLIFY ( p_b_t )
          CALL setup_b_ms_object ( this, l_b_ms )
          CALL setup_b_s_object  ( this, l_b_s  )
          CALL setup_b_v_object  ( this, l_b_v  )
          CALL derive_b_t        ( this )
          DEALLOCATE( l_b_s, l_b_ms, l_b_v )
       END IF
       ! 
       NULLIFY ( p_je, p_jb, p_jt, p_xy )
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
       CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I12)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    ! 
  END SUBROUTINE derive_b_s_d
  !
  !! Typ (offen/geschlossen) der (Rand-) Kanten ermitteln ---------------------------------
  SUBROUTINE derive_b_t_d ( this )
    !! Datenobjekt
    TYPE (t_h_grid)    , POINTER :: this                          ! 
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER   :: c_upname='derive_b_t_d' ! 
    !! Name der angeforderten Gr&ouml;&szlig;e
    CHARACTER (LEN=19), PARAMETER   :: c_pgname='Typ der Rand-Kanten' ! 
    INTEGER :: i, j, ivar, n, memo ! 
    INTEGER , POINTER     :: p_je(:,:), p_b_s(:), p_nsi, p_nsf, p_nbc   ! 
    INTEGER , POINTER     :: p_irand(:), p_is(:,:), p_ks(:), p_ipobo(:), p_jb(:), p_jt(:) !  
    INTEGER , ALLOCATABLE :: l_b_t(:) ! 
    TYPE (t_d3d_openbc) , POINTER :: p_bnd(:) ! 
    !
    ivar = get_h_grid_variant_no(this)
    !
    p_b_s => get_b_s_object ( this )
    !
    SELECT CASE ( ivar )
    CASE ( 1,2 ) ! "gitter05"
       p_irand => get_irand_object ( this )
       IF ( .NOT. ASSOCIATED(p_irand) ) THEN
          CALL derive_irand ( this )
          p_irand => get_irand_object ( this )
       END IF
       p_ks    => get_ks_object    ( this )
       IF ( .NOT. ASSOCIATED(p_ks) ) THEN
          CALL derive_ks ( this )
          p_ks  => get_ks_object ( this )
       END IF
       p_is    => get_is_object    ( this )
       IF ( .NOT. ASSOCIATED(p_is) ) THEN
          CALL derive_is ( this )
          p_is => get_is_object ( this )
       END IF
       p_je    => get_je_object ( this )
       IF ( .NOT. ASSOCIATED( p_je ) ) THEN
          CALL derive_je ( this )
          p_je => get_je_object ( this )
       END IF
       IF ( ASSOCIATED(p_irand) .AND. ASSOCIATED(p_is) .AND. ASSOCIATED(p_je) .AND. &
            ASSOCIATED(p_b_s)   .AND. ASSOCIATED(p_ks) ) THEN
          ALLOCATE( l_b_t(SIZE(p_b_s)) )
          DO i=1,SIZE(l_b_t)
             n    = MAXVAL( p_je(p_b_s(i),:) )
             memo = -1
             DO j=1,p_ks(n)
                IF ( memo /= -1 ) EXIT
                IF ( p_is(n,j) == p_b_s(i) ) memo = j
             END DO
             SELECT CASE ( memo )
             CASE ( 1 ) ! 
                SELECT CASE ( p_irand(n) )
                CASE ( 4,5,6,7 )
                   l_b_t(i) = 0 ! closed
                CASE DEFAULT
                   l_b_t(i) = 1
                END SELECT
             CASE ( 2 ) ! 
                SELECT CASE ( p_irand(n) )
                CASE ( 1,3,5,7 )
                   l_b_t(i) = 0 ! closed
                CASE DEFAULT
                   l_b_t(i) = 1
                END SELECT
             CASE ( 3 ) ! 
                SELECT CASE ( p_irand(n) )
                CASE ( 2,3,6,7 )
                   l_b_t(i) = 0 ! closed
                CASE DEFAULT
                   l_b_t(i) = 1
                END SELECT
             END SELECT
          END DO
          CALL setup_b_t_object ( this, l_b_t )
          DEALLOCATE( l_b_t )
          NULLIFY( p_irand, p_is, p_je, p_ks )
       END IF
    CASE ( 3,4 ) ! "untrim"
       p_nsi => get_nsi_object ( this )
       IF ( .NOT. ASSOCIATED( p_nsi ) ) THEN
          CALL derive_nsi ( this )
          p_nsi => get_nsi_object ( this )
       END IF
       p_nsf => get_nsf_object ( this )
       IF ( .NOT. ASSOCIATED( p_nsf ) ) THEN
          CALL derive_nsf ( this )
          p_nsf => get_nsf_object ( this )
       END IF
       p_je => get_je_object ( this )
       IF ( .NOT. ASSOCIATED( p_je ) ) THEN
          CALL derive_je ( this )
          p_je => get_je_object ( this )
       END IF
       p_nbc => get_nbc_object ( this )
       IF ( ASSOCIATED(p_nsi) .AND. ASSOCIATED(p_nsf) .AND. ASSOCIATED(p_b_s) .AND. &
            ASSOCIATED(p_je)  .AND. ASSOCIATED(p_nbc) ) THEN
          ALLOCATE( l_b_t(SIZE(p_b_s)) )
          DO i=1,SIZE(l_b_t)
             l_b_t(i) = 0
             IF ( p_b_s(i) > p_nsi .AND. p_b_s(i) <= p_nsf ) THEN
                l_b_t(i) = 1
             ELSE
                n = MAXVAL( p_je(p_b_s(i),:) )
                IF ( n <= p_nbc ) l_b_t(i) = 1
             END IF
          END DO
          CALL setup_b_t_object ( this, l_b_t )
          DEALLOCATE( l_b_t )
          NULLIFY( p_nsi, p_nsf, p_je, p_nbc )
       END IF
    CASE ( 5 ) ! "selafin"
       p_ipobo => get_ipobo_object ( this )
       p_je    => get_je_object ( this )
       IF ( .NOT. ASSOCIATED( p_je ) ) THEN
          CALL derive_je ( this )
          p_je => get_je_object ( this )
       END IF
       p_jb    => get_jb_object ( this )
       IF ( .NOT. ASSOCIATED( p_jb ) ) THEN
          CALL derive_jb ( this )
          p_jb => get_jb_object ( this )
       END IF
       p_jt    => get_jt_object ( this )
       IF ( .NOT. ASSOCIATED( p_jt ) ) THEN
          CALL derive_jt ( this )
          p_jt => get_jt_object ( this )
       END IF
       IF ( ASSOCIATED(p_ipobo) .AND. ASSOCIATED(p_je) .AND. ASSOCIATED(p_b_s) .AND. &
            ASSOCIATED(p_jb)    .AND. ASSOCIATED(p_jt) ) THEN
          ALLOCATE(l_b_t(SIZE(p_b_s)))
          l_b_t(:) = 0
          CALL setup_b_t_object ( this, l_b_t )
          DEALLOCATE( l_b_t )
       END IF
       NULLIFY( p_ipobo, p_je, p_jb, p_jt )
    CASE ( 6 ) ! "delft3d"
       p_bnd   => get_bnd_object   ( this )
       p_irand => get_irand_object ( this )
       IF ( .NOT. ASSOCIATED(p_irand) ) THEN
          CALL derive_irand ( this )
          p_irand => get_irand_object ( this )
       END IF
       p_ks    => get_ks_object    ( this )
       IF ( .NOT. ASSOCIATED(p_ks) ) THEN
          CALL derive_ks ( this )
          p_ks  => get_ks_object ( this )
       END IF
       p_is    => get_is_object    ( this )
       IF ( .NOT. ASSOCIATED(p_is) ) THEN
          CALL derive_is ( this )
          p_is => get_is_object ( this )
       END IF
       p_je => get_je_object ( this )
       IF ( .NOT. ASSOCIATED( p_je ) ) THEN
          CALL derive_je ( this )
          p_je => get_je_object ( this )
       END IF
       IF ( ASSOCIATED(p_b_s) ) THEN
          ALLOCATE( l_b_t(SIZE(p_b_s)) )
          DO i=1,SIZE(l_b_t)
             n    = MAXVAL( p_je(p_b_s(i),:) )
             memo = -1
             DO j=1,p_ks(n)
                IF ( memo /= -1 ) EXIT
                IF ( p_is(n,j) == p_b_s(i) ) memo = j
             END DO
             SELECT CASE ( memo )
             CASE ( 1 ) ! erste Kante
                SELECT CASE ( p_irand(n) )
                CASE ( 1, 3, 5, 7, 9,11,13,15 )
                   l_b_t(i) = 0 ! closed
                CASE DEFAULT
                   l_b_t(i) = 1
                END SELECT
             CASE ( 2 ) ! zweite Kante
                SELECT CASE ( p_irand(n) )
                CASE ( 2, 3, 6, 7,10,11,14,15 )
                   l_b_t(i) = 0 ! closed
                CASE DEFAULT
                   l_b_t(i) = 1
                END SELECT
             CASE ( 3 ) ! dritte Kante
                SELECT CASE ( p_irand(n) )
                CASE ( 4, 5, 6, 7,12,13,14,15 )
                   l_b_t(i) = 0 ! closed
                CASE DEFAULT
                   l_b_t(i) = 1
                END SELECT
             CASE ( 4 ) ! vierte Kante
                SELECT CASE ( p_irand(n) )
                CASE ( 8, 9,10,11,12,13,14,15 )
                   l_b_t(i) = 0 ! closed
                CASE DEFAULT
                   l_b_t(i) = 1
                END SELECT
             END SELECT
          END DO
          CALL setup_b_t_object ( this, l_b_t )
          DEALLOCATE( l_b_t )
       END IF
       NULLIFY( p_bnd, p_irand, p_is, p_je, p_ks )
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
       CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I12)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    NULLIFY( p_b_s )
    !
  END SUBROUTINE derive_b_t_d
  !
  !! Unterabschnittsnummern der Rand-Kanten ermitteln
  SUBROUTINE derive_b_ss_d ( this )
    !! Datenobjekt
    TYPE (t_h_grid)    , POINTER :: this                          ! 
    !! Name der Subroutine
    CHARACTER (LEN=13), PARAMETER   :: c_upname='derive_b_ss_d' ! 
    !! Name der angeforderten Gr&ouml;&szlig;e
    CHARACTER (LEN=38), PARAMETER   :: c_pgname='Unterabschnittsnummern der Rand-Kanten' ! 
    INTEGER :: i, ms, ss, ivar ! 
    INTEGER , POINTER     :: p_b_ms(:), p_b_t(:)  ! 
    INTEGER , ALLOCATABLE :: l_b_ss(:) !  
    !
    ivar = get_h_grid_variant_no(this)
    !
    p_b_ms => get_b_ms_object ( this )
    IF ( .NOT. ASSOCIATED(p_b_ms) ) THEN
       CALL derive_b_ms ( this )
       p_b_ms => get_b_ms_object ( this )
    END IF
    p_b_t => get_b_t_object ( this )
    IF ( .NOT. ASSOCIATED(p_b_t) ) THEN
       CALL derive_b_t ( this )
       p_b_t => get_b_t_object ( this )
    END IF
    !
    SELECT CASE ( ivar )
    CASE ( 1,2,3,4,5,6 ) ! "gitter05", "untrim", "selafin", "delft3d"
       IF ( ASSOCIATED(p_b_t) .AND. ASSOCIATED(p_b_ms) ) THEN
          ALLOCATE(l_b_ss(SIZE(p_b_t)))
          ms        = p_b_ms(1)
          ss        = 1
          l_b_ss(1) = ss
          DO i=2,SIZE(l_b_ss)
             IF ( p_b_ms(i) /= ms ) THEN
                ms = p_b_ms(i)
                ss = ss + 1
             END IF
             IF ( p_b_t(i) /= p_b_t(i-1) ) ss = ss + 1 
             l_b_ss(i) = ss
          END DO
          CALL setup_b_ss_object ( this, l_b_ss )
          DEALLOCATE(l_b_ss)
       END IF
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
       CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I12)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    NULLIFY( p_b_ms, p_b_t )
    !
  END SUBROUTINE derive_b_ss_d
  !
  !! Knotenverzeichnis der Elemente ableiten [ nur f&uuml;r Delft3D ]
  SUBROUTINE derive_nen_d ( this )
    !! Datenobjekt
    TYPE (t_h_grid)    , POINTER :: this                          ! 
    !! Name der Subroutine
    CHARACTER (LEN=12) , PARAMETER   :: c_upname='derive_nen_d' ! 
    !! Name der angeforderten Gr&ouml;&szlig;e
    CHARACTER (LEN=30), PARAMETER   :: c_pgname='Knotenverzeichnis der Elemente' ! 
    !! Hilfsvariable
    INTEGER                         :: ivar                            ! 
    INTEGER                         :: i, j, n, np, ne                 ! 
    INTEGER , POINTER               :: p_m, p_n                        ! 
    INTEGER           , ALLOCATABLE :: l_nen(:,:), l_ks(:), l_irand(:), & ! 
                                       l_isbnd(:,:), l_isdam(:,:,:) ! 
    LOGICAL           , ALLOCATABLE :: l_inside(:), l_volume(:,:)      ! 
    TYPE (t_point_2d) , ALLOCATABLE :: l_enc_2d(:), l_points_2d(:)     ! 
    !
    ivar = get_h_grid_variant_no(this)
    !
    SELECT CASE ( ivar )
    CASE ( 6 ) ! "delft3d"
       p_m => get_m_object   ( this )
       p_n => get_n_object   ( this )
       IF ( .NOT. ASSOCIATED( p_m ) .OR. .NOT. ASSOCIATED( p_n ) ) THEN
          CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
          WRITE(ctxt(1:1),'(L1)') ASSOCIATED(p_m) ; CALL setup_error_act ( '<ok-m>', ctxt(1:1) )
          WRITE(ctxt(1:1),'(L1)') ASSOCIATED(p_n) ; CALL setup_error_act ( '<ok-n>', ctxt(1:1) )
          CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
          CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
          WRITE(ctxt,'(I12)') ivar
          CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
       ELSE
          ALLOCATE( l_inside(p_m*p_n), l_points_2d(p_m*p_n), l_volume(p_m-1,p_n-1) )
          l_inside(:)   = .true.
          l_volume(:,:) = .true. 
          n             = 0
          DO j=1,p_n      ! Normalkoordinaten aus den Gitterindices setzen
             DO i=1,p_m
                n = n + 1
                CALL new_point_2d   ( l_points_2d(n) )
                CALL set_point_2d_x ( l_points_2d(n), REAL(i,Double)+0.5_Double )
                CALL set_point_2d_y ( l_points_2d(n), REAL(j,Double)+0.5_Double )
             END DO
          END DO
          DO i=1,get_enc_count(this)      ! Anzahl der Enclosures
             np = get_enc_len ( this, i ) ! Laenge der aktuellen Enclosure
             IF ( np >= 3 ) THEN
                ALLOCATE( l_enc_2d(np) )
                CALL new_point_2d  ( l_enc_2d(:) )
                DO j=1,np ! Normalkoordinaten der aktuellen Enclosure 
                   l_enc_2d(j) = get_enc_point_2d ( this, i, j )
                END DO
                SELECT CASE ( i )
                CASE( 1 )    ! Randpolygon
                   DO j=1,SIZE(l_inside)
                      IF ( .NOT. l_inside(j) ) CYCLE
                      l_inside(j) = inside_point_2d ( l_points_2d(j), l_enc_2d(:) )
                   END DO
                CASE DEFAULT ! innere Polygone
                   DO j=1,SIZE(l_inside)
                      IF ( .NOT. l_inside(j) ) CYCLE
                      l_inside(j) = .NOT. inside_point_2d ( l_points_2d(j), l_enc_2d(:) )
                   END DO
                END SELECT
                CALL kill_point_2d ( l_enc_2d(:) )
                DEALLOCATE( l_enc_2d )
             END IF
          END DO
          DEALLOCATE( l_points_2d )
          CALL set_volume_from_inside ( l_volume, l_inside, p_m, p_n )
          CALL set_volume_from_dry    ( this, l_volume               )
          ne = get_ctrl_volume_count( l_volume )
          IF ( ne > 0 ) THEN
             ALLOCATE( l_nen(ne,4), l_ks(ne), l_irand(ne), l_isbnd(ne,4), l_isdam(ne,4,2) )
             l_ks(:)        = 4
             l_irand(:)     = 0
             l_isbnd(:,:)   = 0
             l_isdam(:,:,:) = 0
             CALL set_ctrl_volume_nen   ( l_nen, l_volume                           )
             CALL set_ctrl_volume_irand ( this, l_irand, l_isbnd, l_isdam, l_volume )
             !
             CALL setup_nen_object    ( this, l_nen     )
             CALL setup_ks_object     ( this, l_ks      )
             CALL setup_irand_object  ( this, l_irand   )
             CALL setup_isbnd_object  ( this, l_isbnd   )
             CALL setup_isdam_object  ( this, l_isdam   )
             DEALLOCATE( l_nen, l_ks, l_irand, l_isbnd, l_isdam )
          END IF
          DEALLOCATE( l_inside, l_volume )
       END IF
       NULLIFY( p_m, p_n )
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
       CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I12)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    NULLIFY( p_m, p_n )
    !
  END SUBROUTINE derive_nen_d
  !
  !! Anzahl der Kanten/Knoten der Elemente ableiten [ nur f&uuml;r Delft3D ]
  SUBROUTINE derive_ks_d ( this )
    !! Datenobjekt
    TYPE (t_h_grid)    , POINTER :: this                          ! 
    !! Name der Subroutine
    CHARACTER (LEN=11) , PARAMETER   :: c_upname='derive_ks_d' ! 
    !! Name der angeforderten Gr&ouml;&szlig;e
    CHARACTER (LEN=37), PARAMETER   :: c_pgname='Anzahl der Kanten/Knoten der Elemente' ! 
    !! Hilfsvariable
    INTEGER                         :: ivar                            ! 
    !
    ivar = get_h_grid_variant_no(this)
    !
    SELECT CASE ( ivar )
    CASE ( 6 ) ! "delft3d"
       CALL derive_nen_d ( this )
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
       CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I12)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
  END SUBROUTINE derive_ks_d
  !
  !! Randkennungen der Elemente ableiten [ nur f&uuml;r Delft3D ]
  SUBROUTINE derive_irand_d ( this )
    !! Datenobjekt
    TYPE (t_h_grid)    , POINTER :: this                          ! 
    !! Name der Subroutine
    CHARACTER (LEN=14) , PARAMETER  :: c_upname='derive_irand_d' ! 
    !! Name der angeforderten Gr&ouml;&szlig;e
    CHARACTER (LEN=26), PARAMETER   :: c_pgname='Randkennungen der Elemente' ! 
    !! Hilfsvariable
    INTEGER                         :: ivar                            ! 
    !
    ivar = get_h_grid_variant_no(this)
    !
    SELECT CASE ( ivar )
    CASE ( 6 ) ! "delft3d"
       CALL derive_nen_d ( this )
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
       CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I12)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
  END SUBROUTINE derive_irand_d
  !
  !! unerodierbare Tiefe der Kanten aus der Tiefe der Kanten neu berechnen,
  !! falls deren Tiefenlage bekannt ist <BR> 
  !! die unerodierbare Tiefe der Kanten wird in dem Objekt "this" abgelegt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE derive_huu_d &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER        :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER    :: c_upname='derive_huu_d' ! 
    !! Name der angeforderten Gr&ouml;&szlig;e
    CHARACTER (LEN=30), PARAMETER    :: c_pgname='unerodierbare Tiefe der Kanten' ! 
    !! Name der fehlenden Gr&ouml;&szlig;en
    CHARACTER (LEN=21), PARAMETER    :: c_pgmiss='Tiefenlage der Kanten' ! 
    !! Statusvariable STAT
    INTEGER                          :: stat      ! 
    !! Kantenverzeichnis der Polygone
    INTEGER            , POINTER     :: p_is(:,:)      ! 
    !! Anzahl der Kanten im Polygon
    INTEGER            , POINTER     :: p_ks(:)        ! 
    !! Verzeichnis f&uuml;r D&auml;mme und Wehre
    INTEGER            , POINTER     :: p_isdam(:,:,:) ! 
    !! Zeiger auf die Tiefe der Kanten
    REAL (KIND=Double) , POINTER     :: p_hu(:)        ! 
    !! Feld f&uuml;r unerodierbare Tiefen der Kanten
    REAL (KIND=Double) , ALLOCATABLE :: huu(:)         ! 
    !! Hilfsvariable
    INTEGER :: i, j ! 
    !
    ivar = get_h_grid_variant_no(this)
    !
    SELECT CASE ( ivar )
    CASE ( 1,2,3,4,5,6 ) ! "gitter05", "untrim", "selafin", "delft3d"
       NULLIFY ( p_is, p_ks, p_isdam, p_hu )
       p_ks => get_ks_object ( this )
       IF ( .NOT. ASSOCIATED( p_ks ) ) THEN
          CALL derive_ks ( this )
          p_ks => get_ks_object ( this )
       END IF
       p_hu => get_hu_object ( this )
       IF ( .NOT. ASSOCIATED( p_hu ) ) THEN
          CALL derive_hu ( this )
          p_hu => get_hu_object ( this )
       END IF
       p_isdam => get_isdam_object ( this )
       p_is    => get_is_object    ( this )
       IF ( ASSOCIATED( p_hu ) ) THEN
          ! Allokieren von Memory fuer das Berechnungsergebnis
          ALLOCATE ( huu(SIZE(p_hu)), STAT=stat )
          ! Fehlermeldung / Berechnung
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), -25001, c_upname, c_modname, stat )
             CALL setup_error_act ( '<AllocGroessen>', c_pgname )
          ELSE
             huu = 11022.0_Double
             SELECT CASE ( ivar ) 
                ! ggf. Beruecksichtigen verschiedener duenner Waende / Daemme fuer Delft3D
             CASE ( 6 )
                IF ( ASSOCIATED(p_isdam) .AND. ASSOCIATED(p_ks) .AND. ASSOCIATED(p_is) ) THEN
                   DO i=1,SIZE(p_is,1)
                      DO j=1,p_ks(i)
                         IF ( p_isdam(i,j,1) > 0 ) THEN
                            SELECT CASE ( p_isdam(i,j,2) )
                            CASE ( 1 )
                               IF ( ASSOCIATED( this%hland ) ) THEN
                                  huu(p_is(i,j)) = this%hland - 1.0_Double
                                  ! p_hu(p_is(i,j)) = this%hland + 1.0_Double
                               ELSE
                                  huu(p_is(i,j)) = -10001.0_Double
                                  ! p_hu(p_is(i,j)) = -9999.0_Double
                               END IF
                            CASE ( 2 )
                               IF ( ASSOCIATED( this%lwl ) ) THEN
                                  huu(p_is(i,j)) = this%lwl(p_isdam(i,j,1))%sill_depth
                               END IF
                            CASE ( 3 )
                               IF ( ASSOCIATED( this%ext ) ) THEN
                                  huu(p_is(i,j)) = this%ext(p_isdam(i,j,1))%sill_depth
                               END IF
                            END SELECT
                         END IF
                      END DO
                   END DO
                END IF
             END SELECT
             CALL setup_huu_object( this, huu )
             DEALLOCATE ( huu, STAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), -25002, c_upname, c_modname, stat )
                CALL setup_error_act ( '<DeAllocGroessen>', c_pgname )
             END IF
          END IF
       ELSE
          CALL setup_error_act ( all_errors(:), -25003, c_upname, c_modname )
          CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
          CALL setup_error_act ( '<FehlendeGroesse>', c_pgmiss ) 
       END IF
       NULLIFY ( p_is, p_ks, p_isdam, p_hu )
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
       CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I12)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
  END SUBROUTINE derive_huu_d
  !
  !! unerodierbare Tiefe der Knoten aus der Tiefe der Knoten neu berechnen,
  !! falls deren Tiefenlage bekannt ist <BR> 
  !! die unerodierbare Tiefe der Knoten wird in dem Objekt "this" abgelegt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE derive_hvu_d &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER        :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER    :: c_upname='derive_hvu_d' ! 
    !! Name der angeforderten Gr&ouml;&szlig;e
    CHARACTER (LEN=30), PARAMETER    :: c_pgname='unerodierbare Tiefe der Knoten' ! 
    !! Name der fehlenden Gr&ouml;&szlig;en
    CHARACTER (LEN=21), PARAMETER    :: c_pgmiss='Tiefenlage der Knoten' ! 
    !! Statusvariable STAT
    INTEGER                          :: stat      ! 
    !! Zeiger auf die Startknoten aller Kanten
    INTEGER            , POINTER     :: p_jb(:)   ! 
    !! Zeiger auf die Endeknoten aller Kanten
    INTEGER            , POINTER     :: p_jt(:)   ! 
    !! Zeiger auf die Landkennung
    REAL (KIND=Double) , POINTER     :: p_hl      ! 
    !! Zeiger auf die Tiefe der Knoten
    REAL (KIND=Double) , POINTER     :: p_hv(:)   ! 
    !! Zeiger auf die nicht weiter erodierbare Tiefe der Kanten
    REAL (KIND=Double) , POINTER     :: p_huu(:)  ! 
    !! Feld f&uuml;r unerodierbare Tiefen der Knoten
    REAL (KIND=Double) , ALLOCATABLE :: hvu(:)    ! 
    !! Hilfsvariable
    INTEGER                          :: j         ! 
    !
    ivar = get_h_grid_variant_no(this)
    !
    SELECT CASE ( ivar )
    CASE ( 1,2,3,4,5,6 ) ! "gitter05", "untrim", "selafin", "delft3d"
       NULLIFY ( p_jb, p_jt, p_hl, p_hv, p_huu )
       p_hv => get_hv_object ( this )
       IF ( .NOT. ASSOCIATED( p_hv ) ) THEN
          CALL derive_hv ( this )
          p_hv => get_hv_object ( this )
       END IF
       IF ( ASSOCIATED( p_hv ) ) THEN
          ! Allokieren von Memory fuer das Berechnungsergebnis
          ALLOCATE ( hvu(SIZE(p_hv)), STAT=stat )
          ! Fehlermeldung / Berechnung
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), -25001, c_upname, c_modname, stat )
             CALL setup_error_act ( '<AllocGroessen>', c_pgname )
          ELSE
             hvu = 11022.0_Double
             p_huu => get_huu_object ( this )
             IF ( .NOT. ASSOCIATED(p_huu) ) THEN
                CALL derive_huu( this )
                p_huu => get_huu_object ( this )
             END IF
             p_jb  => get_jb_object    ( this )
             IF ( .NOT. ASSOCIATED(p_jb) ) THEN
                CALL derive_jb ( this )
                p_jb => get_jb_object ( this )
             END IF
             p_jt  => get_jt_object  ( this )
             IF ( .NOT. ASSOCIATED(p_jt) ) THEN
                CALL derive_jt ( this )
                p_jt => get_jt_object ( this )
             END IF
             p_hl  => get_hland_object ( this ) 
             IF ( .NOT. ASSOCIATED(p_hl) ) THEN
                ALLOCATE( this%hland )
                this%hland = -10000.0_Double
                p_hl => get_hland_object ( this ) 
             END IF
             IF ( ASSOCIATED(p_huu) .AND. ASSOCIATED(p_hl) .AND. ASSOCIATED(p_jb) .AND. ASSOCIATED(p_jt) ) THEN 
                DO j=1,SIZE(p_huu)
                   IF ( p_huu(j) <= p_hl ) CYCLE
                   IF ( hvu(p_jb(j)) > p_hl ) hvu(p_jb(j)) = MIN( hvu(p_jb(j)), p_huu(j) )
                   IF ( hvu(p_jt(j)) > p_hl ) hvu(p_jt(j)) = MIN( hvu(p_jt(j)), p_huu(j) )
                END DO
             END IF
             CALL setup_hvu_object( this, hvu )
             DEALLOCATE ( hvu, STAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), -25002, c_upname, c_modname, stat )
                CALL setup_error_act ( '<DeAllocGroessen>', c_pgname )
             END IF
          END IF
       ELSE
          CALL setup_error_act ( all_errors(:), -25003, c_upname, c_modname )
          CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
          CALL setup_error_act ( '<FehlendeGroesse>', c_pgmiss ) 
       END IF
       NULLIFY ( p_jb, p_jt, p_hl, p_hv, p_huu )
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
       CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I12)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
  END SUBROUTINE derive_hvu_d
  !
  !! unerodierbare Tiefe der Polygone aus der Tiefe der Polygone neu berechnen,
  !! falls deren Tiefenlage bekannt ist <BR> 
  !! die unerodierbare Tiefe der Polygone wird in dem Objekt "this" abgelegt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE derive_hwu_d &
       ( this )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER        :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER    :: c_upname='derive_hwu_d' ! 
    !! Name der angeforderten Gr&ouml;&szlig;e
    CHARACTER (LEN=32), PARAMETER    :: c_pgname='unerodierbare Tiefe der Polygone' ! 
    !! Name der fehlenden Gr&ouml;&szlig;en
    CHARACTER (LEN=23), PARAMETER    :: c_pgmiss='Tiefenlage der Polygone' ! 
    !! Statusvariable STAT
    INTEGER                          :: stat      ! 
    !! Zeiger auf die Tiefe der Polygone
    REAL (KIND=Double) , POINTER     :: p_hw(:)   ! 
    !! Feld f&uuml;r unerodierbare Tiefen der Polygone
    REAL (KIND=Double) , ALLOCATABLE :: hwu(:)    ! 
    !
    ivar = get_h_grid_variant_no(this)
    !
    SELECT CASE ( ivar )
    CASE ( 1,2,3,4,5,6 ) ! "gitter05", "untrim", "selafin", "delft3d"
       NULLIFY ( p_hw )
       ! Holen der zur Berechnung erforderlichen Daten
       p_hw => get_hw_object ( this )
       IF ( .NOT. ASSOCIATED( p_hw ) ) THEN
          CALL derive_hw ( this )
          p_hw => get_hw_object ( this )
       END IF
       IF ( ASSOCIATED( p_hw ) ) THEN
          ! Allokieren von Memory fuer das Berechnungsergebnis
          ALLOCATE ( hwu(SIZE(p_hw)), STAT=stat )
          ! Fehlermeldung / Berechnung
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), -25001, c_upname, c_modname, stat )
             CALL setup_error_act ( '<AllocGroessen>', c_pgname )
          ELSE
             hwu = 11022.0_Double
             CALL setup_hwu_object( this, hwu )
             DEALLOCATE ( hwu, STAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), -25002, c_upname, c_modname, stat )
                CALL setup_error_act ( '<DeAllocGroessen>', c_pgname )
             END IF
          END IF
       ELSE
          CALL setup_error_act ( all_errors(:), -25003, c_upname, c_modname )
          CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
          CALL setup_error_act ( '<FehlendeGroesse>', c_pgmiss ) 
       END IF
       NULLIFY ( p_hw )
    CASE DEFAULT
       CALL setup_error_act ( all_errors(:), -25000, c_upname, c_modname )
       CALL setup_error_act ( '<AngeforderteGroesse>', c_pgname ) 
       CALL setup_error_act ( '<DateiVarianteName>', TRIM(c_variants_type(ivar)) )
       WRITE(ctxt,'(I12)') ivar
       CALL setup_error_act ( '<DateiVarianteNo>', ctxt )
    END SELECT
    !
  END SUBROUTINE derive_hwu_d
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
  !! Fl&auml;che eines Dreieckes im zweidimensionalen Raumes berechnen
  !! Subroutine erzeugt keine Fehlermeldungen
  FUNCTION get_triangle_area_2d &
       ( xy ) &
       RESULT( area )
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER   :: c_upname='get_triangle_area_2d' 
    !! Koordinaten der Knoten des Gitters
    REAL (KIND=Double) :: xy(3,2)   
    !! Fl&auml;che eines Dreieckes
    REAL (KIND=Double) :: area     
    !
    area = 0.5 * ( ( xy(2,1) - xy(1,1) ) * &
                   ( xy(3,2) - xy(1,2) ) - &
                   ( xy(3,1) - xy(1,1) ) * &
                   ( xy(2,2) - xy(1,2) ) )
    !
  END FUNCTION get_triangle_area_2d
  !! Zentrumskoordinaten eines Dreieckes im zweidimensionalen Raumes berechnen; <BR>
  !! das Zentrum ist der Umkreismittelpunkt; <BR>
  !! die Senkrechten auf den Seitenmitten treffen sich im Zentrum (kann vom Schwerpunkt verschieden sein)
  FUNCTION get_triangle_center_2d &
       ( xy ) &
       RESULT( center )
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER   :: c_upname='get_triangle_center_2d' 
    !! Koordinaten der Knoten des Gitters
    REAL (KIND=Double) :: xy(3,2)   
    !! Zentrumskoordinaten eines Polygons
    REAL (KIND=Double) :: center(2)    
    ! Hilfswerte 
    REAL (KIND=Double) :: nenner, help1, help2
    !
    nenner = 2.0 * ( &
         (   xy(1,1) - xy(2,1) )   & 
         *(   xy(1,2) - xy(3,2) )   &
         -(   xy(1,1) - xy(3,1) )   &
         *(   xy(1,2) - xy(2,2)  ) &
         )
    help1 =  &
           xy(1,1)**2  &
         + xy(1,2)**2  &
         - xy(2,1)**2  &
         - xy(2,2)**2
    !
    help2 = &
         xy(1,1)**2  &
         + xy(1,2)**2  &
         - xy(3,1)**2  &
         - xy(3,2)**2
    !
    center(1) = ( &
         help1   * (xy(1,2) - xy(3,2)) &
         - help2 * (xy(1,2) - xy(2,2)) &
         ) / nenner
    !
    center(2) = ( &
         help2   * (xy(1,1) - xy(2,1) ) &
         - help1 * (xy(1,1) - xy(3,1) ) &
         ) / nenner
    !
  END FUNCTION get_triangle_center_2d
  !
  !! ein geschlossenes, konvexes Polygon vollst&auml;ndig in Dreiecke aufteilen; <BR>
  !! konvex = kein Innenwinkel im Polygon darf groesser/gleich 180 Grad sein;
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE poly_to_triangles &
       ( ipoly, &
       c_node_tria_txt, &
       p_nen, &
       p_ks, &
       p_node_tria ) 
    !
    !! Index des aktuellen Polygons
    INTEGER, INTENT(IN)             :: ipoly
    !! String fuer Fehlermeldung
    CHARACTER (LEN=*), INTENT(IN)   :: c_node_tria_txt
    !! Knotenverzeichnis der Polygone
    INTEGER            , POINTER    :: p_nen(:,:)  
    !! Anzahl der Knoten/Kanten in den Polygonen
    INTEGER            , POINTER    :: p_ks(:)     
    !! Knotenverzeichnis der aus den Polygonen erzeugten Dreiecke <BR>
    !! - 1. Dimension = Index des Dreiecks im Polygon, Feldgrenze (p_ks(ipoly)-2 <BR>
    !! - 2. Dimension = Index des Knotens im Dreieck, Feldgrenze 3 <BR>
    INTEGER, POINTER  :: p_node_tria(:,:)  
    !
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER   :: c_upname='poly_to_triangles' 
    !! Status nach (De-) Allokieren
    INTEGER                         :: stat        
    !! Z&auml;hler
    INTEGER                         :: itria
    !
    NULLIFY ( p_node_tria )
    ! Allokieren von Memory fuer das Knotenverzeichnis der Teildreiecke
    ALLOCATE ( p_node_tria(p_ks(ipoly)-2,3), STAT=stat )
    ! Fehlermeldung / Berechnung
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -25001, c_upname, c_modname, stat )
       CALL setup_error_act ( '<AllocGroessen>', c_node_tria_txt )
    ELSE
       ! 
       IF(p_ks(ipoly) == 3)  THEN
          p_node_tria(1,1) = p_nen(ipoly,1)
          p_node_tria(1,2) = p_nen(ipoly,2)
          p_node_tria(1,3) = p_nen(ipoly,3)
       ELSE IF (p_ks(ipoly) > 3)  THEN
          !
          p_node_tria(:,1) = p_nen(ipoly,1)
          !
          ! Schleife ueber alle Dreiecke im Polygon
          DO itria = 1, SIZE(p_node_tria, DIM=1) 
             
             p_node_tria(itria,2) = p_nen(ipoly, itria + 1)
             p_node_tria(itria,3) = p_nen(ipoly, itria + 2)
             
          ENDDO

       ENDIF

    ENDIF
    !
  END SUBROUTINE poly_to_triangles
  !
  !! Ermittle die maximale Anzahl der an einen Knoten angrenzenden Kanten
  FUNCTION get_max_edges_per_vertex &
       ( p_nen, p_ks, p_nv ) &
       RESULT( nof )
    !! Knotenverzeichnis der Polygone
    INTEGER , POINTER :: p_nen(:,:) ! 
    !! Anzahl der Kanten/Knoten je Polygone
    INTEGER , POINTER :: p_ks(:)    ! 
    !! Anzahl der Vertices in dem Gitternetz
    INTEGER , POINTER :: p_nv       ! 
    !! R&uuml;ckgabewert: maximale Anzahl der Kanten an einem Knoten
    INTEGER :: nof ! 
    !! Hilfsfeld zum Memorieren der Anzahl f&uuml;r jeden Knoten
    INTEGER , ALLOCATABLE :: nn(:) ! 
    !! Z&auml;hlervariablen
    INTEGER :: i ! 
    !
    ALLOCATE( nn(p_nv) )
    nn(:) = 0
    DO i=1,SIZE(p_nen,DIM=1)
       nn(p_nen(i,1:p_ks(i))) = nn(p_nen(i,1:p_ks(i))) + 1
    END DO
    nof = MAXVAL( nn(:) )
    DEALLOCATE( nn )
    !
  END FUNCTION get_max_edges_per_vertex
  !
  SUBROUTINE build_nn_sn_from_nen &
       ( p_nen, p_ks, nn, sn )
    !! Knotenverzeichnis der Polygone
    INTEGER          , POINTER :: p_nen(:,:) ! 
    !! Anzahl der Kanten/Knoten je Polygon
    INTEGER          , POINTER :: p_ks(:) ! 
    !! akt. Zahl der am Knoten gespeicherten Kanten
    INTEGER      , INTENT(OUT) :: nn(:) ! 
    !! akt. Liste der den Knoten zugeordneten Kanten
    TYPE (l_edg) , INTENT(OUT) :: sn(:,:) ! 
    !! logischer Indikator
    LOGICAL :: l_new ! 
    !! Z&auml;hlervariablen
    INTEGER :: i, j, k, l ! 
    !! Variablen
    INTEGER :: n1, n2, n, m, lr ! 
    !
    ! [1.1] Initialisieren
    !
    DO k=1,SIZE(nn(:))
       nn(k) = 0
       DO l=1,SIZE(sn(:,:),DIM=2)
          sn(k,l)%j    = 0
          sn(k,l)%m    = 0
          sn(k,l)%p(:) = 0
          sn(k,l)%l(:) = 0
       END DO
    END DO
    !
    ! [1.2] alle Kanten fuer den Knoten mit der kleineren Nummer
    !       einspeichern
    !
    j = 0
    !
    DO i=1,SIZE(p_nen(:,:),DIM=1) ! Polygone
       DO l=1,p_ks(i)             ! Kanten eines Polygons
          n1 = l
          n2 = MERGE( 1, l+1, l == p_ks(i) )
          n  = MIN( p_nen(i,n1), p_nen(i,n2) ) ! Anfangsknoten
          m  = MAX( p_nen(i,n1), p_nen(i,n2) ) ! Endknoten
          lr = MERGE( 1, 2, n == p_nen(i,n1) ) ! 1 = links, 2 = rechts
          l_new = .true.
          DO k=1,nn(n)
             IF ( sn(n,k)%m == m ) THEN
                sn(n,k)%p(lr) = i
                sn(n,k)%l(lr) = l
                l_new = .false.
             END IF
             IF ( .NOT. l_new ) EXIT
          END DO
          IF ( l_new ) THEN
             nn(n)         = nn(n) + 1
             k             = nn(n)
             j             = j + 1
             sn(n,k)%j     = j 
             sn(n,k)%m     = m
             sn(n,k)%p(lr) = i
             sn(n,k)%l(lr) = l
          END IF
       END DO
    END DO
    !
  END SUBROUTINE build_nn_sn_from_nen 
  !
  ! -------------------------------------------------------------------------------
  ! kleine Hilfsfunktionen in Zusammenhang mit Ableitungen fuer Delft3D
  ! -------------------------------------------------------------------------------
  !
  !! Funktion zum Berechnen der Anzahl der <EM>Enclosure</EM>-Polygone <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_enc_count ( this ) &
       RESULT( res )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Anzahl der Umrandungspolygone
    INTEGER                   :: res        ! 
    !! Hilfsvariablen
    INTEGER , POINTER         :: l_enc(:,:) ! 
    !
    l_enc => get_enc_object ( this )
    IF ( ASSOCIATED( l_enc ) ) THEN
       res = MAXVAL( get_enc_number( l_enc ) )
    ELSE
       res = 0
    END IF
    NULLIFY ( l_enc )
    !
  END FUNCTION get_enc_count
  !
  !! Ermittle die Nummerierung der Enclosure-Punkte <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_enc_number ( enc ) &
       RESULT( res )
    !! Enclosure eines Gitters
    INTEGER , INTENT(IN) :: enc(:,:) ! 
    !! Nummerierung der Enclosure Punkte
    INTEGER :: res(SIZE(enc,1))      ! 
    !! Hilfsvariablen
    INTEGER                   :: n, m       ! 
    INTEGER                   :: m_enc(2)   ! 
    !
    n        = 1
    m        = 1
    res(1)   = 1
    m_enc(:) = enc(1,:)
    DO 
       n = n + 1
       IF ( n > SIZE(enc,1) ) EXIT
       res(n) = m
       IF ( ALL( m_enc(:) == enc(n,:) ) ) THEN
          m_enc(:) = -1
          m        = m + 1
       ELSE IF ( ALL( m_enc(:) == -1 ) ) THEN
          m_enc(:) = enc(n,:)
       END IF
    END DO
    !
  END FUNCTION get_enc_number
  !
  !! Funktion zum Berechnen der Anzahl der zu dem val-ten 
  !! <EM>Enclosure</EM>-Polygon geh&ouml;renden Polygonpunkte <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_enc_len ( this, val ) &
       RESULT( res )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Nummer des val-ten Umrandungspolygons
    INTEGER      , INTENT(IN) :: val  ! 
    !! Anzahl der zu dem val-ten Polygon geh&ouml;renden Punkte
    INTEGER                   :: res        ! 
    !! Hilfsvariablen
    INTEGER , POINTER         :: l_enc(:,:) ! 
    !
    l_enc => get_enc_object ( this )
    IF ( ASSOCIATED( l_enc ) ) THEN
       res = COUNT( get_enc_number( l_enc ) == val )
    ELSE
       res = 0
    END IF
    NULLIFY( l_enc )
    !
  END FUNCTION get_enc_len
  !
  !! Funktion ermittelt die (Index-) Koordinate des np-ten zu dem val-ten
  !! <EM>Enclosure</EM>-Polygon geh&ouml;renden Punktes  <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_enc_point_2d ( this, val, np ) &
       RESULT( res )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Nummer des val-ten Umrandungspolygons
    INTEGER      , INTENT(IN) :: val  ! 
    !! Nummer des np-ten Punktes auf dem val-ten Umrandungspolygon
    INTEGER      , INTENT(IN) :: np   ! 
    !! Ergebnis: Index-Koordinate des Punktes
    TYPE (t_point_2d)         :: res  ! 
    !! Hilfsvariablen
    INTEGER                   :: m           ! 
    INTEGER , POINTER         :: l_enc(:,:)  ! 
    INTEGER , ALLOCATABLE     :: l_number(:) ! 
    !
    CALL new_point_2d ( res )
    l_enc => get_enc_object ( this )
    IF ( ASSOCIATED( l_enc ) ) THEN
       ALLOCATE( l_number(SIZE(l_enc,1)) )
       l_number(:) = get_enc_number( l_enc )
       m           = MAXVAL(MINLOC( l_number, l_number == val ))
       IF ( m > 0 .AND. m+np-1 <= SIZE(l_number) ) THEN
          IF ( l_number(m+np-1) == val ) THEN
             CALL set_point_2d_x ( res, REAL( l_enc(m+np-1,1), Double ) )
             CALL set_point_2d_y ( res, REAL( l_enc(m+np-1,2), Double ) )
          END IF
       END IF
       DEALLOCATE( l_number )
    END IF
    NULLIFY( l_enc )
    !
  END FUNCTION get_enc_point_2d
  !
  !! Setze das Indikatorfeld (Kontroll-) Volumen aus den Inside-Informationen
  !! der Tiefenpunkte <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_volume_from_inside ( volume, inside, m, n )
    !! Indikatorfeld f&uuml;r im Modellgebiet liegende Kontrollvolumina
    LOGICAL , INTENT(INOUT) :: volume(:,:) ! 
    !! Indikatorfeld f&uuml;r innerhalb des Gebietes liegende Tiefenpunkte
    LOGICAL , INTENT(IN)    :: inside(:)   ! 
    !! Anzahl der Tiefenpunkte in M-Richtung
    INTEGER , INTENT(IN) :: m         ! 
    !! Anzahl der Tiefenpunkte in N-Richtung
    INTEGER , INTENT(IN) :: n         ! 
    !! Hilfsvariablen
    INTEGER :: i, j, l_idx(4)         !  
    !
    DO j=1,n-1
       DO i=1,m-1
          l_idx(1)    = (j-1)*m  + i
          l_idx(2)    = l_idx(1) + 1
          l_idx(3)    =     j*m  + i + 1
          l_idx(4)    = l_idx(3) - 1
          volume(i,j) = ALL( inside(l_idx) )
       END DO
    END DO
    !
  END SUBROUTINE set_volume_from_inside
  !
  !! Setze das Indikatorfeld (Kontroll-) Volumen aus den (eventuell)
  !! vorhandenen Informationen &uuml;ber trockene Punkte <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_volume_from_dry ( this, volume )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER       :: this        !
    !! Indikatorfeld f&uuml;r im Modellgebiet liegende Kontrollvolumina
    LOGICAL         , INTENT(INOUT) :: volume(:,:) ! 
    !! Hilfsvariablen
    INTEGER           :: i, j, k, sm, sn, dm, dn ! 
    INTEGER , POINTER :: l_dry(:,:) ! 
    !
    l_dry => get_dry_object ( this )
    IF ( ASSOCIATED( l_dry ) ) THEN
       DO k=1,SIZE(l_dry,1)
          dm = l_dry(k,3) - l_dry(k,1)
          sm = SIGN( 1, dm )
          dn = l_dry(k,4) - l_dry(k,2)
          sn = SIGN( 1, dn )
          IF      ( dm == 0 .AND. dn == 0 ) THEN
             volume(l_dry(k,1)-1,l_dry(k,2)-1) = .false.
          ELSE IF ( dm == 0 .AND. dn /= 0 ) THEN
             DO j=l_dry(k,2)-1,l_dry(k,4)-1,sn
                volume(l_dry(k,1)-1,j) = .false.
             END DO
          ELSE IF ( dm /= 0 .AND. dn == 0 ) THEN
             DO i=l_dry(k,1)-1,l_dry(k,3)-1,sm
                volume(i,l_dry(k,2)-1) = .false.
             END DO
          ELSE IF ( dm /= 0 .AND. dn /= 0 ) THEN
             j = l_dry(k,2)-1
             DO i=l_dry(k,1)-1,l_dry(k,3)-1,sm
                volume(i,j) = .false.
                j           = j + sn
             END DO
          END IF
       END DO
    END IF
    NULLIFY( l_dry )
    !
  END SUBROUTINE set_volume_from_dry
  !
  !! Ermittle die Anzahl der Kontrollvolumina in dem Moderllgebiet <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ctrl_volume_count ( volume ) &
       RESULT( res )
    !! Indikatorfeld f&uuml;r im Modellgebiet liegende Kontrollvolumina
    LOGICAL , INTENT(IN) :: volume(:,:) ! 
    !! Ergebnis: Anzahl der Kontrollvolumina im Modellgebiet
    INTEGER :: res                    ! 
    !
    res = COUNT( volume(:,:) )
    !
  END FUNCTION get_ctrl_volume_count
  ! 
  !! Unterprogramm zum Berechnen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_ctrl_volume_nen ( nen, volume )
    !! Knotenverzeichnis der Polygone
    INTEGER , INTENT(INOUT) :: nen(:,:) ! 
    !! Indikatorfeld f&uuml;r im Modellgebiet liegende Kontrollvolumina
    LOGICAL , INTENT(IN) :: volume(:,:) ! 
    !! Hilfsvariablen
    INTEGER :: i, j, l_idx(4), nn, m, n !  
    !
    nn = 0
    m  = SIZE(volume,1)+1
    n  = SIZE(volume,2)+1
    DO j=1,n-1
       DO i=1,m-1
          IF ( .NOT. volume(i,j) ) CYCLE
          l_idx(1)  = (j-1)*m  + i
          l_idx(2)  = l_idx(1) + 1
          l_idx(3)  =     j*m  + i + 1
          l_idx(4)  = l_idx(3) - 1
          nn        = nn + 1
          nen(nn,:) = l_idx(:)
       END DO
    END DO
    !
  END SUBROUTINE set_ctrl_volume_nen
  !
  !! Setze die Kennung f&uuml;r die Komponente "irand" und "isbnd" <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_ctrl_volume_irand ( this, irand, isbnd, isdam, volume )
    !! aktuelles (Arbeits-) Objekt
    TYPE (t_h_grid) , POINTER       :: this        ! 
    !! Kenn-Feld f&uuml;r die kompakte Kennzeichnung offener/geschlossener Kanten
    INTEGER         , INTENT(INOUT) :: irand(:)    ! 
    !! Zeiger f&uuml;r jede Kante eines Polygons auf die Komponente "bnd(:)"
    INTEGER         , INTENT(INOUT) :: isbnd(:,:)  ! 
    !! Zeiger f&uuml;r jede Kante eines Polygons auf die Komponenten "thd(:)|lwl(:)|ext(:)" <BR>
    !! Anmerkung: die letzte Spalte kennzeichnet den Typ der Kante <BR>
    !!            1 = d&uuml;nner Damm "thd",                      <BR>
    !!            2 = lokales Wehr "lwl",                          <BR>
    !!            3 = 2D-Wehr "ext"
    INTEGER         , INTENT(INOUT) :: isdam(:,:,:)  ! 
    !! Indikatorfeld f&uuml;r im Modellgebiet liegende Kontrollvolumina
    LOGICAL         , INTENT(IN)    :: volume(:,:) ! 
    !! Hilfsvariable
    INTEGER :: i, j, m, n, nn, il, ir, ju, jo           !  
    LOGICAL :: search                                   ! 
    TYPE (t_d3d_openbc) , POINTER :: l_bnd(:)           ! 
    TYPE (t_d3d_thd)    , POINTER :: l_thd(:)           ! 
    TYPE (t_d3d_weir)   , POINTER :: l_lwl(:), l_ext(:) ! 
    !
    m     =  SIZE(volume,1)
    n     =  SIZE(volume,2)
    l_bnd => get_bnd_object ( this )
    l_thd => get_thd_object ( this )
    l_lwl => get_lwl_object ( this )
    l_ext => get_ext_object ( this )
    nn    = 0
    DO j=1,n
       ju = j-1
       jo = j+1
       DO i=1,m
          IF ( .NOT. volume(i,j) ) CYCLE
          nn     = nn + 1
          il     =  i - 1
          ir     =  i + 1
          search = ( il < 1 .OR. ir > m .OR. ju < 1 .OR. jo > n )
          IF ( .NOT. search ) search = .NOT. volume(il,j)
          IF ( .NOT. search ) search = .NOT. volume(ir,j)
          IF ( .NOT. search ) search = .NOT. volume(i,ju)
          IF ( .NOT. search ) search = .NOT. volume(i,jo)
          IF ( search ) THEN ! +1 fuer verschiedene Zaehlungen
             CALL modify_irand ( 1,  i+1, ju+1, l_bnd, irand(nn), isbnd(nn,1) ) ! unten
             CALL modify_irand ( 2, ir+1,  j+1, l_bnd, irand(nn), isbnd(nn,2) ) ! rechts
             CALL modify_irand ( 4,  i+1, jo+1, l_bnd, irand(nn), isbnd(nn,3) ) ! oben
             CALL modify_irand ( 8, il+1,  j+1, l_bnd, irand(nn), isbnd(nn,4) ) ! links
          END IF
          IF ( ASSOCIATED( l_thd ) ) THEN
             IF ( i < m .AND. volume(ir, j) ) &
                  CALL modify_dam ( 'U', i+1, j+1, l_thd, isdam(nn,2,1:2) )     ! rechts
             IF ( j < n .AND. volume( i,jo) ) &
                  CALL modify_dam ( 'V', i+1, j+1, l_thd, isdam(nn,3,1:2) )     ! oben
          END IF
          IF ( ASSOCIATED( l_lwl ) ) THEN
             IF ( i < m .AND. volume(ir, j) ) &
                  CALL modify_weir ( 'U', 2, i+1, j+1, l_lwl, isdam(nn,2,1:2) ) ! rechts
             IF ( j < n .AND. volume( i,jo) ) &
                  CALL modify_weir ( 'V', 2, i+1, j+1, l_lwl, isdam(nn,3,1:2) ) ! oben
          END IF
          IF ( ASSOCIATED( l_ext ) ) THEN
             IF ( i < m .AND. volume(ir, j) ) &
                  CALL modify_weir ( 'U', 3, i+1, j+1, l_ext, isdam(nn,2,1:2) ) ! rechts
             IF ( j < n .AND. volume( i,jo) ) &
                  CALL modify_weir ( 'V', 3, i+1, j+1, l_ext, isdam(nn,3,1:2) ) ! oben
          END IF
       END DO
    END DO
    NULLIFY( l_bnd, l_thd, l_lwl, l_ext )
    !
  END SUBROUTINE set_ctrl_volume_irand
  !
  SUBROUTINE modify_irand ( iadd, ii, jj, bnd, irand, isbnd )
    !! Additionskonstante f&uuml;r IRAND falls geschlossen
    INTEGER , INTENT(IN) :: iadd ! 
    !! m-Index
    INTEGER , INTENT(IN) :: ii   ! 
    !! n-Index
    INTEGER , INTENT(IN) :: jj   ! 
    !! Beschreibung der offenen R&auml;nder
    TYPE (t_d3d_openbc) , POINTER :: bnd(:) ! 
    !! Randkennung des Kontrollvolumens
    INTEGER       , INTENT(INOUT) :: irand  ! 
    !! Zeiger auf bnd(:)
    INTEGER       , INTENT(INOUT) :: isbnd  ! 
    !! Hilfsvariable
    LOGICAL :: found   ! 
    INTEGER :: i, j, k, m1, m2, n1, n2, sm, sn, dm, dn ! 
    !
    found = .false.
    IF ( ASSOCIATED(bnd) ) THEN
       DO k=1,SIZE(bnd)
          IF ( found ) EXIT
          m1 = bnd(k)%grid_coor(1) ; m2 = bnd(k)%grid_coor(3)
          n1 = bnd(k)%grid_coor(2) ; n2 = bnd(k)%grid_coor(4)
          dm = m2 - m1             ; dn = n2 - n1
          sm = SIGN( 1, dm )       ; sn = SIGN( 1, dn )
          IF      ( dm == 0 .AND. dn == 0 ) THEN
             found = ( ii == m1 .AND. jj == n1 )
          ELSE IF ( dm == 0 .AND. dn /= 0 ) THEN
             DO j=n1,n2,sn
                IF ( found ) EXIT
                found = ( ii == m1 .AND. jj ==  j )
             END DO
          ELSE IF ( dm /= 0 .AND. dn == 0 ) THEN
             DO i=m1,m2,sm
                IF ( found ) EXIT
                found = ( ii ==  i .AND. jj == n1 )
             END DO
          ELSE IF ( dm /= 0 .AND. dn /= 0 ) THEN
             j = n1
             DO i=m1,m2,sm
                IF ( found ) EXIT
                found = ( ii ==  i .AND. jj ==  j )
                j     = j + sn
             END DO
          END IF
          IF ( found ) isbnd = k
       END DO
    END IF
    IF ( .NOT. found ) irand = irand + iadd
    !
  END SUBROUTINE modify_irand
  !
  SUBROUTINE modify_dam ( type, ii, jj, lth, isdam )
    !! Kennzeichnung des Kanten-Typs ["U" oder "V"]
    CHARACTER (LEN=*) , INTENT(IN) :: type    ! 
    !! m-Index
    INTEGER          , INTENT(IN) :: ii       ! 
    !! n-Index
    INTEGER          , INTENT(IN) :: jj       ! 
    !! Anfangs- und Endekoordinaten des Damm- oder Wehrabschnitts
    TYPE (t_d3d_thd) , INTENT(IN) :: lth(:)   ! 
    !! Zeiger auf die Struktur [thd(:)]
    INTEGER       , INTENT(INOUT) :: isdam(:) ! 
    !! Hilfsvariable
    LOGICAL :: found   ! 
    INTEGER :: i, j, k, m1, m2, n1, n2, sm, sn, dm, dn ! 
    !
    found = .false.
    DO k=1,SIZE(lth)
       IF ( found                         ) EXIT
       IF ( ALL( isdam /= 0 )             ) EXIT
       IF ( lth(k)%type(1:1) /= type(1:1) ) CYCLE
       m1 = lth(k)%grid_coor(1) ; m2 = lth(k)%grid_coor(3)
       n1 = lth(k)%grid_coor(2) ; n2 = lth(k)%grid_coor(4)
       dm = m2 - m1             ; dn = n2 - n1
       sm = SIGN( 1, dm )       ; sn = SIGN( 1, dn )
       IF      ( dm == 0 .AND. dn == 0 ) THEN
          found = ( ii == m1 .AND. jj == n1 )
       ELSE IF ( dm == 0 .AND. dn /= 0 ) THEN
          DO j=n1,n2,sn
             IF ( found ) EXIT
             found = ( ii == m1 .AND. jj ==  j )
          END DO
       ELSE IF ( dm /= 0 .AND. dn == 0 ) THEN
          DO i=m1,m2,sm
             IF ( found ) EXIT
             found = ( ii ==  i .AND. jj == n1 )
          END DO
       ELSE IF ( dm /= 0 .AND. dn /= 0 ) THEN
          j = n1
          DO i=m1,m2,sm
             IF ( found ) EXIT
             found = ( ii ==  i .AND. jj ==  j )
             j     = j + sn
          END DO
       END IF
       IF ( found ) THEN
          isdam(1) = k
          isdam(2) = 1
       END IF
    END DO
    !
  END SUBROUTINE modify_dam
  !
  SUBROUTINE modify_weir ( type, ityp, ii, jj, wei, isdam )
    !! Kennzeichnung des Kanten-Typs ["U" oder "V"]
    CHARACTER (LEN=*) , INTENT(IN) :: type     ! 
    !! Kennung [ 2 = 'lwl', 3 = 'ext' ]
    INTEGER           , INTENT(IN) :: ityp     ! 
    !! m-Index
    INTEGER           , INTENT(IN) :: ii       ! 
    !! n-Index
    INTEGER           , INTENT(IN) :: jj       ! 
    !! Anfangs- und Endekoordinaten des Damm- oder Wehrabschnitts
    TYPE (t_d3d_weir) , INTENT(IN) :: wei(:)   ! 
    !! Zeiger auf die Struktur [thd(:)]
    INTEGER        , INTENT(INOUT) :: isdam(:) ! 
    !! Hilfsvariable
    LOGICAL :: found   ! 
    INTEGER :: i, j, k, m1, m2, n1, n2, sm, sn, dm, dn ! 
    !
    IF ( ANY( (/2,3/) == ityp ) ) THEN
       found = .false.
       DO k=1,SIZE(wei)
          IF ( found                         ) EXIT
          IF ( ALL( isdam /= 0 )             ) EXIT
          IF ( wei(k)%type(1:1) /= type(1:1) ) CYCLE
          m1 = wei(k)%grid_coor(1) ; m2 = wei(k)%grid_coor(3)
          n1 = wei(k)%grid_coor(2) ; n2 = wei(k)%grid_coor(4)
          dm = m2 - m1             ; dn = n2 - n1
          sm = SIGN( 1, dm )       ; sn = SIGN( 1, dn )
          IF      ( dm == 0 .AND. dn == 0 ) THEN
             found = ( ii == m1 .AND. jj == n1 )
          ELSE IF ( dm == 0 .AND. dn /= 0 ) THEN
             DO j=n1,n2,sn
                IF ( found ) EXIT
                found = ( ii == m1 .AND. jj ==  j )
             END DO
          ELSE IF ( dm /= 0 .AND. dn == 0 ) THEN
             DO i=m1,m2,sm
                IF ( found ) EXIT
                found = ( ii ==  i .AND. jj == n1 )
             END DO
          ELSE IF ( dm /= 0 .AND. dn /= 0 ) THEN
             j = n1
             DO i=m1,m2,sm
                IF ( found ) EXIT
                found = ( ii ==  i .AND. jj ==  j )
                j     = j + sn
             END DO
          END IF
          IF ( found ) THEN
             isdam(1) = k
             isdam(2) = ityp
          END IF
       END DO
    END IF
    !
  END SUBROUTINE modify_weir
  !
  !
  FUNCTION get_l_no_d ( ks, var ) &
       RESULT( res )
    !! max. Anzahl der Knoten/Seiten im Polygon
    INTEGER , INTENT(IN) :: ks  !  
    !! lfd. Nummer einer Seite<
    INTEGER , INTENT(IN) :: var ! 
    !! Ergebnis: auf den Bereich [1:ks] reduizierte Nummer
    INTEGER :: res ! 
    !
    res = MOD( var, ks )
    IF ( res == 0 ) res = ks
    ! 
  END FUNCTION get_l_no_d
  !
END MODULE m_h_grid_derive
! TailOfPackageUserInterface -----------------------------------------------
