! -------------------------------------------------------------------------
! HeadOfPackageModule -----------------------------------------------------
!
!! <H2>Lesen und Schreiben von Gitterdaten f&uuml;r Dateityp "untrim"</h2>
!! @author <A HREF="mailto:lang@hamburg.baw.de">G. Lang</A>
!! @version 4.5 vom 02/05/07, Quellcode: mod_m_h_grid_untrim.f90
!! <HR>
!! read and write methods for "untrim" files                         <BR>
!! conversion of grid data between different formats <BR>
!
!  Copyright-Hinweis
!
!  Copyright (C) 2001 Bundesanstalt fuer Wasserbau
!
!  <CopyrightWildcard>
!
! 01.01 : 2002-07-19 : G. Lang    : Startversion
! 01.03 : 2003-03-20 : P. Schade  : Aenderungen am Header, z.B. Copyright
! 01.04 : 2003-12-02 : J. Juerges : Komponente nsi
! 01.05 : 2004-02-27 : G. Lang    : Verwenden von NSI
! 01.06 : 2004-06-25 : G. Lang    : Verwenden von NSF
! 02.01 : 2005-03-07 : G. Lang    : Erweiterungen OpenMI (unvollstaendig)
! 02.02 : 2005-03-10 : G. Lang    : div. Erweiterungen fuer Konfigurationsphase OpenMI
! 02.03 : 2005-03-16 : G. Lang    : Erweiterungen fuer ALLBoundaries,AllOpenBoundaries und AllClosedBoundaries 
! 03.01 : 2005-07-21 : G. Lang    : Anpassungen/Erweiterungen fuer mod. OpenMI-ElementSet-Verstaendnis
! 04.01 : 2005-08-22 : G. Lang    : Lesen/Schreiben Delft3D-Gitternetz
! 04.02 : 2005-12-12 : G. Lang    : zus. DERIVE-Funktionalitaet vor dem Schreiben von VC-Gitternetzen
! 04.03 : 2006-01-06 : G. Lang    : NAMELIST-IO abgewandelt (typisierte Namen jetzt stimmig mit "classic")
! 04.04 : 2006-09-07 : G. Lang    : formatierte Ausgabe fuer Dateityp "untrim_vc"
! 04.05 : 2007-02-05 : G. Lang    : nach Lesen von "untrim_vc", "untrim_baw" UP "swap_je_jb_jt" rufen
!
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! <OL>
!!   <LI> Lesen einer Datei im Format "untrim" (Versionen BAW und
!!        Vincenzo Casulli);
!!   <LI> Schreiben einer Datei im Format "untrim" (Versionen
!!        BAW und Vincenzo Casulli).
!! </OL>
!! Weitere Informationen zum Dateiformat unter
!! <A HREF="http://www.hamburg.baw.de/fkb/untrim/utrg-de.htm"><EM>untrim_grid.dat</EM></A>.
!! <HR>
!! <H3>Gebrauch der Methoden dieses Moduls</H3>
!! Die in diesem Modul bereitgestellten Methoden d&uuml;rfen nur
!! von zu dem Paket "h_grid" geh&ouml;renden Modulen direkt 
!! verwendet werden. Insbesondere ist das direkte Verwenden von
!! Methoden aus anderen Paketen unzul&auml;ssig. 
!
MODULE m_h_grid_untrim
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] Basis-Modul mit globalen Konstantwerten
  !
  USE b_constants, ONLY : &
       ! Parameter
       Double, Long, Byte
  !
  ! [A.2] Basis-Modul "Fehler"
  !
  USE b_error, ONLY :       &
       ! Routinen
       no_error,            &
       any_error,           &
       setup_error_act
  !
  ! [A.3] Basis-Modul "File"
  !
  USE b_file, ONLY : &
       !   Routinen / Interfaces
       get_file_unit, &
       get_file_name
  ! 
  ! [A.4] Basis-Modul "Datum+Uhrzeit"
  !
  USE b_datetime, ONLY : &
       ! Routinen / Interfaces
       get_datetime_from_system
  !
  ! ----------------------------------------------------------------------
  ! [B] gemeinsam genutzte Daten des Paketes "<PackageName>"
  ! ----------------------------------------------------------------------
  !
  ! [B.1] Daten-Modul des Paketes "<PackageName>"
  !
  USE m_h_grid_data, ONLY : &
       ! Typdefinition
       t_h_grid,   & ! 
       ! Parameter und Konstantwerte
       all_errors, & ! 
       ! Routinen / Interfaces
       get_nv_object, get_ns_object, get_nsi_object, get_nsf_object, get_ne_object,        &
       get_nr_object, get_nbc_object, get_angle_object, get_hland_object, get_text_object, &
       get_xy_object, get_ks_object, get_xc_object, get_nen_object, get_is_object,         &
       get_hu_object, get_hv_object, get_jb_object, get_jt_object, get_je_object,          &
       !
       setup_nbc_object, setup_nr_object, setup_nsi_object, setup_nsf_object,              &
       setup_angle_object, setup_hland_object, setup_text_object, setup_xy_object,         &
       setup_ks_object, setup_xc_object, setup_is_object, setup_nen_object,                &
       setup_hu_object, setup_hv_object, setup_jb_object, setup_jt_object,                 &
       setup_je_object, setup_time_object,                                                 &
       swap_je_jb_jt
  !
  USE m_h_grid_derive, ONLY : &
       derive_hv, derive_xc, derive_is, derive_hu, derive_jb, derive_jt, derive_je
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
  !! Lesen einer sequentiellen ASCII-Datei des Typs "untrim" (BAW)
  INTERFACE read_h_grid_untrim_baw_asc
     MODULE PROCEDURE read_h_grid_untrim_baw_asc_d
  END INTERFACE
  !! Lesen einer sequentiellen ASCII-Datei des Typs "untrim" (VC)
  INTERFACE read_h_grid_untrim_vc_asc
     MODULE PROCEDURE read_h_grid_untrim_vc_asc_d
  END INTERFACE
  !! Schreiben einer sequentiellen ASCII-Datei des Typs "untrim" (BAW)
  INTERFACE write_h_grid_untrim_baw_asc
     MODULE PROCEDURE write_h_grid_untrim_baw_asc_d
  END INTERFACE
  !! Schreiben einer sequentiellen ASCII-Datei des Typs "untrim" (VC)
  INTERFACE write_h_grid_untrim_vc_asc
     MODULE PROCEDURE write_h_grid_untrim_vc_asc_d
  END INTERFACE
  !
  ! [C.5] Zuweisungen
  ! [C.6] Operatoren (optional, falls sinnvoll)
  ! [C.7] Liste der oeffentlichen Methoden
  !
  PUBLIC :: read_h_grid_untrim_baw_asc  ! (BAW) / SEQUENTIAL, FORMATTED
  PUBLIC :: read_h_grid_untrim_vc_asc   ! (VC) / SEQUENTIAL, FORMATTED
  PUBLIC :: write_h_grid_untrim_baw_asc ! (BAW) / SEQUENTIAL, FORMATTED
  PUBLIC :: write_h_grid_untrim_vc_asc  ! (VC) / SEQUENTIAL, FORMATTED
  !
  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Datentypen, Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] lokale Typdefinitionen
  !
  !! maximale Anzahl der Kanten/Knoten im Element
  INTEGER , PARAMETER :: maxedges=4 ! 
  !! lokale "Emulation" von (classic) "t_gkk_point"
  TYPE , PRIVATE :: l_gkk
     REAL (KIND=Double) :: x_gkk ! x-Koordinate
     REAL (KIND=Double) :: y_gkk ! y-Koordinate
  END TYPE l_gkk
  !! lokale "Emulation" von (classic) "t_unigrid_node"
  TYPE , PRIVATE :: l_nod
     INTEGER      :: id   ! Knoten-Id
     TYPE (l_gkk) :: coor ! Koordinaten des Knotens
  END TYPE l_nod
  !! lokale "Emulation" von (classic) "t_unigrid_edge"
  TYPE , PRIVATE :: l_edg
     INTEGER      :: id   ! Kanten-Id
     INTEGER      :: node_anf_id ! Anfangsknoten
     INTEGER      :: node_end_id ! Endeknoten
  END TYPE l_edg
  !! lokale "Emulation" von (classic) "t_unigrid_cell"
  TYPE , PRIVATE :: l_cel
     INTEGER      :: id                 ! Element-Id
     INTEGER      :: type               ! Typ des Elements
     INTEGER      :: nedges             ! akt. Anzahl der Kanten
     INTEGER      :: edge_ids(maxedges) ! Kantenverzeichnis
  END TYPE l_cel
  !! lokale "Emulation" von (classic) "vertex_depth"
  TYPE , PRIVATE :: l_dep
     REAL (KIND=Double)  :: z            ! Tiefe
  END TYPE l_dep
  !! lokale "Emulation" von (classic) "edge_depth"
  TYPE , PRIVATE :: e_dep
     REAL (KIND=Double)  :: hu           ! Tiefe
  END TYPE e_dep
  !! lokale "Emulation" von (classic) "edge_material"
  TYPE , PRIVATE :: e_mat
     INTEGER (KIND=Byte) :: mid ! material identification number
  END TYPE e_mat
  !! lokale "Emulation" von (classic) "edge_neigh"
  TYPE , PRIVATE :: e_nei
     INTEGER (KIND=Long) :: je(2)       ! Nachbarelemente
  END TYPE e_nei
  !! lokale "Emulation" von (classic) "element_vertex"
  TYPE, PRIVATE :: c_ver
     INTEGER (KIND=Long) :: nen(maxedges) ! list of corners 
  END TYPE c_ver
  !
  ! [D.2] Konstantwerte (Parameter)
  !
  !! Shape-Parameter f&uuml;r Reshape-Befehl
  INTEGER , PARAMETER :: l_shape(1)=(/ maxedges /) ! 
  !! maximale Anzahl verschiedener Fehlertexte
  INTEGER , PARAMETER :: c_noflocalerrors=9 ! 
  !! lokale Fehlertexte
  CHARACTER (LEN=45) , PARAMETER :: c_localerrors(c_noflocalerrors) = & ! 
       (/ 'Namelist-Block LISTGRD (UNTRIM_VC-Datei)     ' , & ! 1
          'XY (UNTRIM_VC-Datei)                         ' , & ! 2
          'KS, XC, NEN und IS (UNTRIM_VC-Datei)         ' , & ! 3
          'HU, JB, JT und JE (UNTRIM_VC-Datei)          ' , & ! 4 
          'Namelist-Block STATIC (UNTRIM_BAW-Datei)     ' , & ! 5
          'Namelist-Block LOCATION (UNTRIM_BAW-Datei)   ' , & ! 6
          'Namelist-Block VERTEX (UNTRIM_BAW-Datei)     ' , & ! 7
          'Namelist-Block EDGE (UNTRIM_BAW-Datei)       ' , & ! 8
          'Namelist-Block ELEMENT (UNTRIM_BAW-Datei)    ' /)  ! 9
  !
  !! Name des Moduls
  CHARACTER (LEN=15), PARAMETER :: c_modname = 'm_h_grid_untrim' ! 
  !
  ! [D.3] Variablen (statische Daten des Moduls)
  !
  !! Hilfsfeld fuer Fehlertexte
  CHARACTER (LEN=15)  :: ctxt  ! 
  !! log. Kanalnummer f&uuml;r das Lesen der Daten
  INTEGER :: iunit             ! 
  !! Z&auml;hlervariable
  INTEGER :: i, j              ! 
  !! Statusvariable (READ und WRITE)
  INTEGER :: stat, iostat      ! 
  !! Zeiger auf Text in Fehlermeldung "c_localerrors"
  INTEGER :: isprun            ! 
  !
  ! Gitternetz ----------------------------------------------------------------
  !! Knotenverzeichnis
  INTEGER            , POINTER :: p_nen(:,:) ! 
  !! Anzahl der Knoten/Kanten im Polygon
  INTEGER            , POINTER :: p_ks(:)    ! 
  !! Anzahl der Knoten des Gitters
  INTEGER            , POINTER :: p_nv       ! 
  !! Anzahl der Kanten des Gitters
  INTEGER            , POINTER :: p_ns       ! 
  !! Anzahl der inneren Kanten des Gitters
  INTEGER            , POINTER :: p_nsi      ! 
  !! lezte Kante mit Fluss-Bedingung eines Gitters
  INTEGER            , POINTER :: p_nsf      ! 
  !! Anzahl der Polygone des Gitters
  INTEGER            , POINTER :: p_ne       ! 
  !! Anzahl der <EM>roten</EM> Polygone eines Gitters
  INTEGER            , POINTER :: p_nr       ! 
  !! Koordinaten der Knoten des Gitters
  REAL (KIND=Double) , POINTER :: p_xy(:,:)  ! 
  !! Anzahl der Polygone entlang des offenen Randes
  INTEGER            , POINTER :: p_nbc      ! 
  !! Tiefenlage zur Kennzeichnung von dauerhaften Landgebieten
  REAL (KIND=Double) , POINTER :: p_hland    ! 
  !! mittlere geographische Breite des Modellgebietes
  REAL (KIND=Double) , POINTER :: p_angle    ! 
  !! Klartextbeschreibung des Modellgebietes
  CHARACTER (LEN=80) , POINTER :: p_text(:)  ! 
  !! erster Knoten auf einer Kante des Gitters
  INTEGER            , POINTER :: p_jb(:)    ! 
  !! zweiter Knoten auf einer Kante des Gitters
  INTEGER            , POINTER :: p_jt(:)    ! 
  !! Kantenverzeichnis der Elemente des Gitters
  INTEGER            , POINTER :: p_is(:,:)  ! 
  !! Nachbarverzeichnis der Kante des Gitters
  INTEGER            , POINTER :: p_je(:,:)  ! 
  !! Koordinaten der Zentren des Gitters
  REAL (KIND=Double) , POINTER :: p_xc(:,:)  ! 
  !! Tiefe &uuml;ber den Kanten des Gitters
  REAL (KIND=Double) , POINTER :: p_hu(:)    ! 
  !! Tiefe &uuml;ber den Knoten des Gitters
  REAL (KIND=Double) , POINTER :: p_hv(:)    ! 
  !
  ! Daten in Namelist-Bloecken ------------------------------------------------
  !! Anzahl der Textzeilen
  INTEGER            :: nt       ! 
  !! maximale Anzahl der Knoten/Kanten je Polygon
  INTEGER            :: nc       ! 
  !! Anzahl der Knoten des Gitters
  INTEGER            :: nv       ! 
  !! Anzahl der Kanten des Gitters
  INTEGER            :: ns       ! 
  !! Anzahl der inneren Kanten des Gitters
  INTEGER            :: nsi      ! 
  !! letzte Kante mit Flussbedingung eines Gitters
  INTEGER            :: nsf      ! 
  !! Anzahl der Polygone des Gitters
  INTEGER            :: ne       ! 
  !! Anzahl der Polygone entlang des offenen Randes
  INTEGER            :: nbc      ! 
  !! Anzahl der <EM>roten</EM> Polygone
  INTEGER            :: nr       ! 
  !! Tiefenlage zur Kennzeichnung von dauerhaften Landgebieten
  REAL (KIND=Double) :: hland    ! 
  !! mittlere geographische Breite des Modellgebiets
  REAL (KIND=Double) :: angle    ! 
  !
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
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-READ-Methoden <<< [ERR_NO = 23000 bis 23999]
  ! ----------------------------------------------------------------------
  !
  !! &Uuml;bertragen der in einer Datei stehenden Daten in das (Arbeits-) Objekt <BR>
  !! f&uuml;r die Datei gilt FORM="FORMATTED", ACCESS="SEQUENTIAL"              <BR>
  !! das UnTRIM-Gitter liegt in der BAW-spezifischen Formatvariation (TYPE="untrim_baw") vor <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_h_grid_untrim_baw_asc_d &
       ( this )
    !
    !! Aktuelles Arbeitsobjekt zur Aufnahme der Gitterdaten
    TYPE (t_h_grid) , POINTER :: this ! 
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=28), PARAMETER  :: c_upname='read_h_grid_untrim_baw_asc_d' !
    !
    CALL init_before_readwrite ( this )
    !
    CALL read_baw_datablock_1 ( this )
    CALL read_baw_datablock_2 ( this )
    CALL read_baw_datablock_3 ( this )
    CALL read_baw_datablock_4 ( this )
    CALL read_baw_datablock_5 ( this )
    !
    CALL setup_error_after_read ( this )
    CALL init_after_readwrite   (      )
    !
    CALL swap_je_jb_jt          ( this )
    !
    CALL setup_time_object ( this, get_datetime_from_system ( ) )
    !
  END SUBROUTINE read_h_grid_untrim_baw_asc_d
  !
  !! &Uuml;bertragen der in einer Datei stehenden Daten in das (Arbeits-) Objekt <BR>
  !! f&uuml;r die Datei gilt FORM="FORMATTED", ACCESS="SEQUENTIAL"              <BR>
  !! das UnTRIM-Gitter liegt in der VC-spezifischen Formatvariation (TYPE="untrim_vc") vor <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_h_grid_untrim_vc_asc_d &
       ( this )
    !
    !! Aktuelles Arbeitsobjekt zur Aufnahme der Gitterdaten
    TYPE (t_h_grid)    , POINTER :: this ! 
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=27) , PARAMETER     :: c_upname='read_h_grid_untrim_vc_asc_d' !
    !
    CALL init_before_readwrite ( this )
    !
    CALL read_vc_datablock_1 ( this )
    CALL read_vc_datablock_2 ( this )
    CALL read_vc_datablock_3 ( this )
    CALL read_vc_datablock_4 ( this )
    !
    CALL setup_error_after_read ( this )
    CALL init_after_readwrite   (      )
    !
    CALL swap_je_jb_jt          ( this )
    !
    CALL setup_time_object ( this, get_datetime_from_system ( ) )
    !
  END SUBROUTINE read_h_grid_untrim_vc_asc_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-WRITE-Methoden <<< [ERR_NO = 24000 bis 24999]
  ! ----------------------------------------------------------------------
  !
  !! &Uuml;bertragen der in dem (Arbeits-) Objekt stehenden Daten in eine Datei  <BR>
  !! f&uuml;r die Datei gilt FORM="FORMATTED", ACCESS="SEQUENTIAL"              <BR>
  !! das UnTRIM-Gitter wird in der BAW-spezifischen Formatvariation (TYPE="untrim_baw") geschrieben <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE write_h_grid_untrim_baw_asc_d &
       ( this )
    !
    !! Aktuelles Arbeitsobjekt mit den Gitterdaten
    TYPE (t_h_grid) , POINTER :: this ! 
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=29), PARAMETER  :: c_upname='write_h_grid_untrim_baw_asc_d' !
    !
    CALL init_before_readwrite ( this )
    !
    CALL write_baw_datablock_1 ( this )
    CALL write_baw_datablock_2 ( this )
    CALL write_baw_datablock_3 ( this )
    CALL write_baw_datablock_4 ( this )
    CALL write_baw_datablock_5 ( this )
    !
    CALL setup_error_after_write ( this )
    CALL init_after_readwrite   (      )
    !
  END SUBROUTINE write_h_grid_untrim_baw_asc_d
  !
  !! &Uuml;bertragen der in dem (Arbeits-) Objekt stehenden Daten in eine Datei  <BR>
  !! f&uuml;r die Datei gilt FORM="FORMATTED", ACCESS="SEQUENTIAL"              <BR>
  !! das UnTRIM-Gitter wird in der VC-spezifischen Formatvariation (TYPE="untrim_vc") geschrieben <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE write_h_grid_untrim_vc_asc_d &
       ( this )
    !
    !! Aktuelles Arbeitsobjekt mit den Gitterdaten
    TYPE (t_h_grid) , POINTER :: this ! 
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=28), PARAMETER  :: c_upname='write_h_grid_untrim_vc_asc_d' !
    !
    CALL init_before_readwrite ( this )
    !
    CALL write_vc_datablock_1 ( this )
    CALL write_vc_datablock_2 ( this )
    CALL write_vc_datablock_3 ( this )
    CALL write_vc_datablock_4 ( this )
    !
    CALL setup_error_after_write ( this )
    CALL init_after_readwrite (      )
    !
  END SUBROUTINE write_h_grid_untrim_vc_asc_d
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
  ! ALLGEMEINE METHODEN --------------------------------------------------
  !
  !! Initialisieren einiger Variablen vor dem Lesen/Schreiben von Daten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_before_readwrite &
       ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !
    iunit  = get_file_unit ( this%file )
    stat   = 0
    iostat = 0
    isprun = 0
    !
    nt       = 0
    nc       = 0
    nv       = 0
    ns       = 0
    nsi      = 0
    nsf      = 0
    ne       = 0
    nr       = 0
    nbc      = 0
    hland    = -10000.0_Double
    angle    = 0.0_Double
    !
    NULLIFY ( p_nen, p_ks, p_nv, p_ns, p_nsi, p_nsf, p_ne, p_nr, p_xy, p_nbc, p_hv, &
         p_hland, p_angle, p_text, p_jb, p_jt, p_is, p_je, p_xc, p_hu )
    !
  END SUBROUTINE init_before_readwrite
  !
  !! Re-Initialisieren einiger Variablen vor nach dem Lesen von Daten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_after_readwrite &
       (  )
    !
    NULLIFY ( p_nen, p_ks, p_nv, p_ns, p_nsi, p_nsf, p_ne, p_nr, p_xy, p_nbc, p_hv, &
         p_hland, p_angle, p_text, p_jb, p_jt, p_is, p_je, p_xc, p_hu )
    !
  END SUBROUTINE init_after_readwrite
  !
  !! Ggf. einen Fehler nach dem Lesen von Daten erzeugen <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_error_after_read &
       ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=22) , PARAMETER :: c_upname='setup_error_after_read' ! 
    !
    IF ( isprun /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 23200, c_upname, c_modname, iostat )
       CALL setup_error_act ( '<DateiName>', TRIM( get_file_name( this%file ) ) )
       CALL setup_error_act ( '<LokalerFehlerText>', c_localerrors(isprun) )
    END IF
    !
  END SUBROUTINE setup_error_after_read
  !
  !! Ggf. einen Fehler nach dem Schreiben von Daten erzeugen <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_error_after_write &
       ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=23) , PARAMETER :: c_upname='setup_error_after_write' ! 
    !
    IF ( isprun /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 24200, c_upname, c_modname, iostat )
       CALL setup_error_act ( '<DateiName>', TRIM( get_file_name( this%file ) ) )
       CALL setup_error_act ( '<LokalerFehlerText>', c_localerrors(isprun) )
    END IF
    !
  END SUBROUTINE setup_error_after_write
  !
  ! UNTRIM_VC-spezifische METHODEN ------------------------------------------
  !
  !! Lesen des Datenblocks 1 (Namelist <EM>listgrd</EM>) <BR>
  !! VC-spezifisches UnTRIM-Format <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_vc_datablock_1 &
       ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Namensbezeichnung f&uuml;r das Modellgebiet
    CHARACTER (LEN=80) :: location ! 
    !! Namensliste f&uuml;r VC-spezifische UnTRIM-Dateien
    NAMELIST /listgrd/ ne, ns, nv, nbc, nr, nsi, nsf, angle, hland, location !
    !
    location = REPEAT( ' ', LEN(location) )
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       READ (iunit,NML=listgrd,IOSTAT=iostat )
       IF ( iostat == 0 ) THEN
          IF ( no_error( ) ) CALL setup_nbc_object   ( this, nbc      )
          IF ( no_error( ) ) CALL setup_nsi_object   ( this, nsi      )
          IF ( no_error( ) ) CALL setup_nsf_object   ( this, nsf      )
          IF ( no_error( ) ) THEN
             IF ( nr > 0 ) THEN
                CALL setup_nr_object    ( this, nr       )
             ELSE
                CALL setup_nr_object    ( this, ne       )
             END IF
          END IF
          IF ( no_error( ) ) CALL setup_angle_object ( this, angle    )
          IF ( no_error( ) ) CALL setup_hland_object ( this, hland    )
          IF ( no_error( ) ) CALL setup_text_object  ( this, location )
       ELSE
          isprun = 1 
       END IF
    END IF
    !
  END SUBROUTINE read_vc_datablock_1
  !
  !! Schreiben des Datenblocks 1 (Namelist <EM>listgrd</EM>) <BR>
  !! VC-spezifisches UnTRIM-Format <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE write_vc_datablock_1 &
       ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Namensbezeichnung f&uuml;r das Modellgebiet
    CHARACTER (LEN=80) :: location ! 
    !! Namensliste f&uuml;r VC-spezifische UnTRIM-Dateien
    NAMELIST /listgrd/ ne, ns, nv, nbc, nr, nsi, nsf, angle, hland, location !
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       p_ne    => get_ne_object    ( this )
       p_ns    => get_ns_object    ( this )
       p_nsi   => get_nsi_object   ( this )
       p_nsf   => get_nsf_object   ( this )
       p_nv    => get_nv_object    ( this )
       p_nbc   => get_nbc_object   ( this )
       p_nr    => get_nr_object    ( this )
       p_angle => get_angle_object ( this )
       p_hland => get_hland_object ( this )
       p_text  => get_text_object  ( this )
       IF ( no_error( ) ) THEN
          ne       = p_ne
          ns       = p_ns
          nsi      = p_nsi
          nsf      = p_nsf
          nv       = p_nv
          nbc      = p_nbc
          nr       = p_nr
          angle    = p_angle
          hland    = p_hland
          location = p_text(1) ! nur eine Zeile
          WRITE (iunit,NML=listgrd,IOSTAT=iostat )
          IF ( iostat /= 0 ) isprun = 1 
       END IF
    END IF
    !
  END SUBROUTINE write_vc_datablock_1
  !
  !! Lesen des Datenblocks 2 (Koordinaten <EM>xy</EM>) <BR>
  !! VC-spezifisches UnTRIM-Format <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_vc_datablock_2 &
       ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=19) , PARAMETER :: c_upname='read_vc_datablock_2' ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       ALLOCATE ( p_xy(nv,2), STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 23201, c_upname, c_modname, stat )
          WRITE(ctxt(1:10),'(I10)') nv
          CALL setup_error_act ( '<AktDim1>', ctxt )
          WRITE(ctxt(1:10),'(I10)') 2
          CALL setup_error_act ( '<AktDim2>', ctxt )
       ELSE
          READ(iunit,*,IOSTAT=iostat) &
               ( p_xy(i,1:2),i=1,nv )
          IF ( iostat == 0 ) THEN
             IF ( no_error( ) ) CALL setup_xy_object ( this, p_xy(:,:) )
          ELSE
             isprun = 2
          END IF
          DEALLOCATE ( p_xy, STAT=stat )
          IF ( stat /= 0 ) &
               CALL setup_error_act ( all_errors(:), 23202, c_upname, c_modname, stat )
       END IF
    END IF
    !
  END SUBROUTINE read_vc_datablock_2
  !
  !! Schreiben des Datenblocks 2 (Koordinaten <EM>xy</EM>) <BR>
  !! VC-spezifisches UnTRIM-Format <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE write_vc_datablock_2 &
       ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       p_xy => get_xy_object ( this )
       IF ( no_error( ) ) THEN
          DO i=1,SIZE( p_xy, DIM=1 )
             IF ( iostat /= 0 ) EXIT
             WRITE(iunit,'(2(G16.10,1X))',IOSTAT=iostat) ( p_xy(i,j),j=1,2 )
          END DO
          IF ( iostat /= 0 ) isprun = 2
       END IF
    END IF
    !
  END SUBROUTINE write_vc_datablock_2
  !
  !! Lesen des Datenblocks 3 (<EM>ks, xc, nen, is</EM>) <BR>
  !! VC-spezifisches UnTRIM-Format <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_vc_datablock_3 &
       ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=19) , PARAMETER :: c_upname='read_vc_datablock_3' ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       ALLOCATE ( p_ks(ne), p_xc(ne,2), p_nen(ne,maxedges), p_is(ne,maxedges), STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 23203, c_upname, c_modname, stat )
          WRITE(ctxt(1:10),'(I10)') ne
          CALL setup_error_act ( '<AktDim1>', ctxt )
          WRITE(ctxt(1:10),'(I10)') 4
          CALL setup_error_act ( '<AktDim2>', ctxt )
       ELSE
          READ(iunit,*,IOSTAT=iostat) &
               ( p_ks(i),p_xc(i,1:2),(p_nen(i,j),p_is(i,j),j=1,p_ks(i)),i=1,ne )
          IF ( iostat == 0 ) THEN
             IF ( no_error( ) ) CALL setup_ks_object  ( this, p_ks(:)    )
             IF ( no_error( ) ) CALL setup_xc_object  ( this, p_xc(:,:)  )
             IF ( no_error( ) ) CALL setup_nen_object ( this, p_nen(:,:) )
             IF ( no_error( ) ) CALL setup_is_object  ( this, p_is(:,:)  )
          ELSE
             isprun = 3
          END IF
          DEALLOCATE ( p_ks, p_xc, p_nen, p_is, STAT=stat )
          IF ( stat /= 0 ) &
               CALL setup_error_act ( all_errors(:), 23204, c_upname, c_modname, stat )
       END IF
    END IF
    !
  END SUBROUTINE read_vc_datablock_3
  !
  !! Schreiben des Datenblocks 3 (<EM>ks, xc, nen, is</EM>) <BR>
  !! VC-spezifisches UnTRIM-Format <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE write_vc_datablock_3 &
       ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       p_ks  => get_ks_object  ( this )
       p_xc  => get_xc_object  ( this )
       IF ( .NOT. ASSOCIATED(p_xc) ) THEN
          CALL derive_xc ( this )
          p_xc  => get_xc_object  ( this )
       END IF
       p_nen => get_nen_object ( this )
       p_is  => get_is_object  ( this )
       IF ( .NOT. ASSOCIATED(p_is) ) THEN
          CALL derive_is ( this )
          p_is  => get_is_object  ( this )
       END IF
       IF ( no_error( ) ) THEN
          DO i=1,SIZE( p_ks )
             IF ( iostat /= 0 ) EXIT
             WRITE(iunit,'(I2,1X,2(G16.10,1X),8(I7,1X))',IOSTAT=iostat) &
                  p_ks(i),p_xc(i,1:2),(p_nen(i,j),p_is(i,j),j=1,p_ks(i))
          END DO
          IF ( iostat /= 0 ) isprun = 3
       END IF
    END IF
    !
  END SUBROUTINE write_vc_datablock_3
  !
  !! Lesen des Datenblocks 4 (<EM>hu, jb, jt, je</EM>) <BR>
  !! VC-spezifisches UnTRIM-Format <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_vc_datablock_4 &
       ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=19) , PARAMETER :: c_upname='read_vc_datablock_4' ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       ALLOCATE ( p_hu(ns), p_jb(ns), p_jt(ns), p_je(ns,2), STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 23205, c_upname, c_modname, stat )
          WRITE(ctxt(1:10),'(I10)') ns
          CALL setup_error_act ( '<AktDim1>', ctxt )
          WRITE(ctxt(1:10),'(I10)') 2
          CALL setup_error_act ( '<AktDim2>', ctxt )
       ELSE
          READ(iunit,*,IOSTAT=iostat) &
               ( p_hu(j),p_jb(j),p_jt(j),p_je(j,1:2),j=1,ns )
          IF ( iostat == 0 ) THEN
             IF ( no_error( ) ) CALL setup_hu_object ( this, p_hu(:)   )
             IF ( no_error( ) ) CALL setup_jb_object ( this, p_jb(:)   )
             IF ( no_error( ) ) CALL setup_jt_object ( this, p_jt(:)   )
             IF ( no_error( ) ) CALL setup_je_object ( this, p_je(:,:) )
          ELSE
             isprun = 4
          END IF
          DEALLOCATE ( p_hu, p_jb, p_jt, p_je, STAT=stat )
          IF ( stat /= 0 ) &
               CALL setup_error_act ( all_errors(:), 23206, c_upname, c_modname, stat )
       END IF
    END IF
    !
  END SUBROUTINE read_vc_datablock_4
  !
  !! Schreiben des Datenblocks 4 (<EM>hu, jb, jt, je</EM>) <BR>
  !! VC-spezifisches UnTRIM-Format <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE write_vc_datablock_4 &
       ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       p_hu => get_hu_object ( this )
       IF ( .NOT. ASSOCIATED(p_hu) ) THEN
          CALL derive_hu ( this )
          p_hu  => get_hu_object  ( this )
       END IF
       p_jb => get_jb_object ( this )
       IF ( .NOT. ASSOCIATED(p_jb) ) THEN
          CALL derive_jb ( this )
          p_jb  => get_jb_object  ( this )
       END IF
       p_jt => get_jt_object ( this )
       IF ( .NOT. ASSOCIATED(p_jt) ) THEN
          CALL derive_jt ( this )
          p_jt  => get_jt_object  ( this )
       END IF
       p_je => get_je_object ( this )
       IF ( .NOT. ASSOCIATED(p_je) ) THEN
          CALL derive_je ( this )
          p_je  => get_je_object  ( this )
       END IF
       IF ( no_error( ) ) THEN
          DO j=1,SIZE( p_hu )
             IF ( iostat /= 0 ) EXIT
             WRITE(iunit,'(G16.10,1X,4(I7,1X))',IOSTAT=iostat) &
                  p_hu(j),p_jb(j),p_jt(j),p_je(j,1:2)
          END DO
          IF ( iostat /= 0 ) isprun = 4
       END IF
    END IF
    !
  END SUBROUTINE write_vc_datablock_4
  !
  ! UNTRIM_BAW-spezifische METHODEN ----------------------------------------
  !
  !! Lesen des Datenblocks 1 (Namelist <EM>static</EM>) <BR>
  !! BAW-spezifisches UnTRIM-Format <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_baw_datablock_1 &
       ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Namensliste der statischen Daten
    NAMELIST /static/  nt, nc, nv, ns, ne, nbc, nr, nsi, nsf, hland, angle   ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       READ (iunit,NML=static,IOSTAT=iostat )
       IF ( iostat == 0 ) THEN
          IF ( no_error( ) ) CALL setup_nbc_object   ( this, nbc      )
          IF ( no_error( ) ) CALL setup_nsi_object   ( this, nsi      )
          IF ( no_error( ) ) CALL setup_nsf_object   ( this, nsf      )
          IF ( no_error( ) ) THEN
             IF ( nr > 0 ) THEN
                CALL setup_nr_object    ( this, nr       )
             ELSE
                CALL setup_nr_object    ( this, ne       )
             END IF
          END IF
          IF ( no_error( ) ) CALL setup_angle_object ( this, angle    )
          IF ( no_error( ) ) CALL setup_hland_object ( this, hland    )
       ELSE
          isprun = 5
       END IF
    END IF
    !
  END SUBROUTINE read_baw_datablock_1
  !
  !! Schreiben des Datenblocks 1 (Namelist <EM>static</EM>) <BR>
  !! BAW-spezifisches UnTRIM-Format <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE write_baw_datablock_1 &
       ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Namensliste der statischen Daten
    NAMELIST /static/  nt, nc, nv, ns, ne, nbc, nr, nsi, nsf, hland, angle   ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       p_ne    => get_ne_object    ( this )
       p_ns    => get_ns_object    ( this )
       p_nv    => get_nv_object    ( this )
       p_nbc   => get_nbc_object   ( this )
       p_nr    => get_nr_object    ( this )
       p_nsi   => get_nsi_object   ( this )
       p_nsf   => get_nsf_object   ( this )
       p_angle => get_angle_object ( this )
       p_hland => get_hland_object ( this )
       p_text  => get_text_object  ( this )
       p_ks    => get_ks_object    ( this )
       IF ( no_error( ) ) THEN
          nt    = SIZE( p_text(:) )
          nc    = MAXVAL( p_ks(:) )
          nv    = p_nv
          ns    = p_ns
          ne    = p_ne
          nbc   = p_nbc
          nr    = p_nr
          nsi   = p_nsi
          nsf   = p_nsf
          hland = p_hland
          angle = p_angle
          WRITE (iunit,NML=static,IOSTAT=iostat )
          IF ( iostat /= 0 ) isprun = 5
       END IF
    END IF
    !
  END SUBROUTINE write_baw_datablock_1
  !
  !! Lesen des Datenblocks 2 (Namelist <EM>location</EM>) <BR>
  !! BAW-spezifisches UnTRIM-Format <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_baw_datablock_2 &
       ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='read_baw_datablock_2' ! 
    !! Name des Modellgebietes
    CHARACTER (LEN=80) :: loc               ! 
    !! Namelist f&uuml;r Stationsbeschreibung
    NAMELIST /location/ loc ! 
    !
    IF ( no_error( ) .AND. isprun == 0 .AND. nt > 0 ) THEN
       ALLOCATE ( p_text(nt), STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 23207, c_upname, c_modname, stat )
          WRITE(ctxt(1:10),'(I10)') nt
          CALL setup_error_act ( '<AktDim1>', ctxt )
       ELSE
          p_text(:) = REPEAT( ' ', LEN(p_text(:)) )
          DO i=1,nt
             READ (iunit,NML=location,IOSTAT=iostat) 
             IF ( iostat /= 0 ) EXIT
             p_text(i) = loc
          END DO
          IF ( iostat == 0 ) THEN
             CALL setup_text_object ( this, p_text(:) )
          ELSE
             isprun = 6
          END IF
          DEALLOCATE ( p_text, STAT=stat )
          IF ( stat /= 0 ) &
               CALL setup_error_act ( all_errors(:), 23208, c_upname, c_modname, stat )
       END IF
    END IF
    !
  END SUBROUTINE read_baw_datablock_2
  !  
  !! Schreiben des Datenblocks 2 (Namelist <EM>location</EM>) <BR>
  !! BAW-spezifisches UnTRIM-Format <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE write_baw_datablock_2 &
       ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='write_baw_datablock_2' ! 
    !! Name des Modellgebietes
    CHARACTER (LEN=80) :: loc               ! 
    !! Namelist f&uuml;r Stationsbeschreibung
    NAMELIST /location/ loc ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       p_text => get_text_object ( this )
       IF ( no_error( ) ) THEN
          DO i=1,SIZE(p_text(:))
             loc = p_text(i)
             WRITE (iunit,NML=location,IOSTAT=iostat) 
             IF ( iostat /= 0 ) isprun = 6
             IF ( isprun /= 0 ) EXIT
          END DO
       END IF
    END IF
    !
  END SUBROUTINE write_baw_datablock_2
  !
  !! Lesen des Datenblocks 3 (Namelist <EM>vertex</EM>) <BR>
  !! BAW-spezifisches UnTRIM-Format <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_baw_datablock_3 &
       ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='read_baw_datablock_3' ! 
    !! lokale Variable zur Aufnahme der Knotendaten
    TYPE (l_nod)       :: nod  ! 
    !! lokale Variable zur Aufnahme der Tiefeninformation
    TYPE (l_dep)       :: dep  ! 
    !! Namelist f&uuml;r die Knotendaten
    NAMELIST /vertex/ nod, dep ! 
    !
    IF ( no_error( ) .AND. isprun == 0 .AND. nv > 0 ) THEN
       ALLOCATE ( p_xy(nv,2), p_hv(nv), STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 23209, c_upname, c_modname, stat )
          WRITE(ctxt(1:10),'(I10)') nv
          CALL setup_error_act ( '<AktDim1>', ctxt )
          WRITE(ctxt(1:10),'(I10)') 2
          CALL setup_error_act ( '<AktDim2>', ctxt )
       ELSE
          DO i=1,nv
             READ (iunit,NML=vertex,IOSTAT=iostat)
             IF ( iostat /= 0 ) EXIT
             p_xy(i,1) = nod%coor%x_gkk
             p_xy(i,2) = nod%coor%y_gkk
             p_hv(i)   = dep%z
          END DO
          IF ( iostat == 0 ) THEN
             CALL setup_xy_object ( this, p_xy(:,:) )
             CALL setup_hv_object ( this, p_hv(:)   )
          ELSE
             isprun = 7
          END IF
          DEALLOCATE ( p_xy, p_hv, STAT=stat )
          IF ( stat /= 0 ) &
               CALL setup_error_act ( all_errors(:), 23210, c_upname, c_modname, stat )
       END IF
    END IF
    !
  END SUBROUTINE read_baw_datablock_3
  !
  !! Schreiben des Datenblocks 3 (Namelist <EM>vertex</EM>) <BR>
  !! BAW-spezifisches UnTRIM-Format <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE write_baw_datablock_3 &
       ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='write_baw_datablock_3' ! 
    !! lokale Variable zur Aufnahme der Knotendaten
    TYPE (l_nod)       :: nod  ! 
    !! lokale Variable zur Aufnahme der Tiefeninformation
    TYPE (l_dep)       :: dep  ! 
    !! Namelist f&uuml;r die Knotendaten
    NAMELIST /vertex/ nod, dep ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       p_xy => get_xy_object ( this )
       p_hv => get_hv_object ( this )
       IF ( .NOT. ASSOCIATED(p_hv) ) THEN
          CALL derive_hv ( this )
          p_hv => get_hv_object ( this )
       END IF
       IF ( no_error( ) ) THEN
          DO i=1,SIZE(p_xy(:,:),DIM=1)
             nod%id         = i
             nod%coor%x_gkk = p_xy(i,1)
             nod%coor%y_gkk = p_xy(i,2)
             IF ( ASSOCIATED( p_hv ) ) THEN
                dep%z = p_hv(i)
             ELSE
                dep%z = -10001.0_Double
             END IF
             WRITE (iunit,NML=vertex,IOSTAT=iostat)
             IF ( iostat /= 0 ) isprun = 7
             IF ( isprun /= 0 ) EXIT
          END DO
       END IF
    END IF
    !
  END SUBROUTINE write_baw_datablock_3
  !
  !! Lesen des Datenblocks 4 (Namelist <EM>edge</EM>) <BR>
  !! BAW-spezifisches UnTRIM-Format <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_baw_datablock_4 &
       ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='read_baw_datablock_4' ! 
    !! kantenbezogene Daten
    TYPE (l_edg)       :: edg    ! 
    !! Nachbarpolygone einer Kante
    TYPE (e_nei)       :: nei    ! 
    !! Tiefenlage der Kante
    TYPE (e_dep)       :: dep    ! 
    !! Namelist f&uuml;r die Kantendaten
    NAMELIST /edge/ edg, nei, dep ! 
    !
    IF ( no_error( ) .AND. isprun == 0 .AND. ns > 0 ) THEN
       ALLOCATE ( p_jb(ns), p_jt(ns), p_je(ns,2), p_hu(ns), STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 23211, c_upname, c_modname, stat )
          WRITE(ctxt(1:10),'(I10)') ns
          CALL setup_error_act ( '<AktDim1>', ctxt )
          WRITE(ctxt(1:10),'(I10)') 2
          CALL setup_error_act ( '<AktDim2>', ctxt )
       ELSE
          DO i=1,ns
             READ (iunit,NML=edge,IOSTAT=iostat)
             IF ( iostat /= 0 ) EXIT
             p_jb(i)   = edg%node_anf_id
             p_jt(i)   = edg%node_end_id
             p_je(i,:) = nei%je(:)
             p_hu(i)   = dep%hu
          END DO
          IF ( iostat == 0 ) THEN
             CALL setup_jb_object ( this, p_jb(:)   )
             CALL setup_jt_object ( this, p_jt(:)   )
             CALL setup_je_object ( this, p_je(:,:) )
             CALL setup_hu_object ( this, p_hu(:)   )
          ELSE
             isprun = 8
          END IF
          DEALLOCATE ( p_jb, p_jt, p_je, p_hu, STAT=stat )
          IF ( stat /= 0 ) &
               CALL setup_error_act ( all_errors(:), 23212, c_upname, c_modname, stat )
       END IF
    END IF
    !
  END SUBROUTINE read_baw_datablock_4
  !
  !! Schreiben des Datenblocks 4 (Namelist <EM>edge</EM>) <BR>
  !! BAW-spezifisches UnTRIM-Format <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE write_baw_datablock_4 &
       ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='write_baw_datablock_4' ! 
    !! kantenbezogene Daten
    TYPE (l_edg)       :: edg    ! 
    !! Materialkennung
    TYPE (e_mat)       :: mat    ! 
    !! Nachbarpolygone einer Kante
    TYPE (e_nei)       :: nei    ! 
    !! Tiefenlage der Kante
    TYPE (e_dep)       :: dep    ! 
    !! Namelist f&uuml;r die Kantendaten
    NAMELIST /edge/ edg, mat, nei, dep ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       p_jb => get_jb_object ( this )
       p_jt => get_jt_object ( this )
       p_je => get_je_object ( this )
       p_hu => get_hu_object ( this )
       IF ( no_error( ) ) THEN
          DO i=1,SIZE(p_jb(:))
             edg%id          = i
             edg%node_anf_id = p_jb(i)
             edg%node_end_id = p_jt(i)
             mat%mid         = 0
             nei%je(:)       = p_je(i,:)
             dep%hu          = p_hu(i)
             WRITE (iunit,NML=edge,IOSTAT=iostat)
             IF ( iostat /= 0 ) isprun = 8
             IF ( isprun /= 0 ) EXIT
          END DO
       END IF
    END IF
    !
  END SUBROUTINE write_baw_datablock_4
  !
  !! Lesen des Datenblocks 5 (Namelist <EM>element</EM>) <BR>
  !! BAW-spezifisches UnTRIM-Format <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_baw_datablock_5 &
       ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='read_baw_datablock_5' ! 
    !! elementbezogene Daten
    TYPE (l_cel) :: cel           !  
    !! Knotenverzeichnis des Elements
    TYPE (c_ver) :: ver ! 
    !! aktuelle Anzahl der maximalen Knoten/Kanten je Polygon
    INTEGER      :: mxn ! 
    !! Namelist f&uuml;r Elementdaten
    NAMELIST /element/ cel, ver ! 
    !
    IF ( no_error( ) .AND. isprun == 0 .AND. ne > 0 ) THEN
       ALLOCATE ( p_ks(ne), p_is(ne,maxedges), p_nen(ne,maxedges), STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 23213, c_upname, c_modname, stat )
          WRITE(ctxt(1:10),'(I10)') ne
          CALL setup_error_act ( '<AktDim1>', ctxt )
          WRITE(ctxt(1:10),'(I10)') maxedges
          CALL setup_error_act ( '<AktDim2>', ctxt )
       ELSE
          DO i=1,ne
             READ (iunit,NML=element,IOSTAT=iostat)
             IF ( iostat /= 0 ) EXIT
             p_ks(i)    = cel%nedges
             p_is(i,:)  = cel%edge_ids(:)
             p_nen(i,:) = ver%nen(:) 
          END DO
          IF ( iostat == 0 ) THEN
             mxn = MAXVAL( p_ks(:) )
             CALL setup_ks_object  ( this, p_ks(:)       )
             CALL setup_is_object  ( this, p_is(:,:mxn)  )
             CALL setup_nen_object ( this, p_nen(:,:mxn) )
          ELSE
             isprun = 9
          END IF
          DEALLOCATE ( p_ks, p_is, p_nen, STAT=stat )
          IF ( stat /= 0 ) &
               CALL setup_error_act ( all_errors(:), 23214, c_upname, c_modname, stat )
       END IF
    END IF
    !
  END SUBROUTINE read_baw_datablock_5
  !  
  !! Schreiben des Datenblocks 5 (Namelist <EM>element</EM>) <BR>
  !! BAW-spezifisches UnTRIM-Format <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE write_baw_datablock_5 &
       ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='write_baw_datablock_5' ! 
    !! elementbezogene Daten
    TYPE (l_cel) :: cel           !  
    !! Materialkennung
    TYPE (e_mat) :: mat           ! 
    !! Knotenverzeichnis des Elements
    TYPE (c_ver) :: ver           ! 
    !! Namelist f&uuml;r Elementdaten
    NAMELIST /element/ cel, mat, ver ! 
    !
    IF ( no_error( ) .AND. isprun == 0 ) THEN
       p_ks  => get_ks_object  ( this )
       p_is  => get_is_object  ( this )
       p_nen => get_nen_object ( this )
       p_xy  => get_xy_object  ( this )
       IF ( no_error( ) ) THEN
          DO i=1,SIZE(p_nen(:,:),DIM=1)
             cel%id          = i
             cel%type        = get_poly_type ( p_xy(p_nen(i,:p_ks(i)),:) )
             cel%nedges      = p_ks(i)
             cel%edge_ids(:) = RESHAPE( p_is(i,1:p_ks(i)), l_shape, (/ -1 /) )
             mat%mid         = 0
             ver%nen(:)      = RESHAPE( p_nen(i,1:p_ks(i)), l_shape, (/ -1 /) )
             WRITE (iunit,NML=element,IOSTAT=iostat)
             IF ( iostat /= 0 ) isprun = 9
             IF ( isprun /= 0 ) EXIT
          END DO
       END IF
    END IF
    !
  END SUBROUTINE write_baw_datablock_5
  !
  !! Bestimmen des Elementtyps in Abh&auml;ngigkeit von den Koordinaten
  !! und der Anzahl der Knoten im Polygon <BR>
  !! TYPE = 1 : Dreieck  <BR>
  !! TYPE = 2 : Quadrat  <BR>
  !! TYPE = 3 : Rechteck <BR>
  !! TYPE = 4 : Viereck  <BR>
  !! TYPE = 5 : beliebiges Polygon
  FUNCTION get_poly_type &
       ( xy )           &
       RESULT( type )
    !! Hilfsfeld
    INTEGER , PARAMETER :: idx(4)=(/2,3,4,1/) ! 
    !! Koordinaten des Polygons
    REAL (KIND=Double) , INTENT(IN) :: xy(:,:) ! 
    !! R&uuml;ckgabewert : Typ des Polygons
    INTEGER :: type ! 
    !! Hilfskoordinaten
    REAL (KIND=Double) :: lxy(SIZE(xy,DIM=1),SIZE(xy,DIM=2)) ! 
    !! Schwerpunkt
    REAL (KIND=Double) :: sp(SIZE(xy,DIM=2)) ! 
    !! Kantenl&auml;ngen
    REAL (KIND=Double) :: lk(SIZE(xy,DIM=1)) ! 
    !! Abst&auml;nde des Schwerpunkts von den Ecken
    REAL (KIND=Double) :: dn(SIZE(xy,DIM=1)) ! 
    !! Z&auml;hlervariable
    INTEGER :: l ! 
    !
    SELECT CASE ( SIZE(xy,DIM=1) )
    CASE (3)
       type = 1
    CASE (4)
       DO l=1,4
          lxy(l,:) = xy(l,:)-xy(1,:) ! Relativkoordinaten
       END DO
       DO l=1,2
          sp(l) = 0.25_Double*SUM( lxy(:,l) ) ! Schwerpunkt
       END DO
       DO l=1,4
          lk(l) = SQRT( SUM ( (lxy(idx(l),:)-lxy(l,:))**2 ) ) ! Kantenlaenge
          dn(l) = SQRT( SUM ( (lxy(l,:)-sp(:))**2 ) ) ! Schwerpunktsabstaende
       END DO
       DO l=4,1,-1
          dn(l) = ABS(dn(l)-dn(1)) ! Abstandsdifferenzen
          lk(l) = ABS(lk(l)-lk(1)) ! Laengendifferenzen
       END DO
       IF ( ALL( dn(:) <= EPSILON(dn) ) ) THEN
          type = MERGE( 2, 3, ALL( lk(:) <= EPSILON(lk) ) )
       ELSE
          type = 4
       END IF
    CASE DEFAULT
       type = 5
    END SELECT
    !
  END FUNCTION get_poly_type
  !  
END MODULE m_h_grid_untrim
! TailOfPackageUserInterface -----------------------------------------------
