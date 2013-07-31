! -------------------------------------------------------------------------
! HeadOfPackageModule -----------------------------------------------------
!
!! <H2>Datenmodul des Paketes "h_grid" und elementare Methoden auf Daten</h2>
!! @author <A HREF="mailto:lang@hamburg.baw.de">G. Lang</A>
!! @version 4.10 vom 02/19/07, Quellcode: mod_m_h_grid_data.f90
!! <HR>
!! global data for package "h_data" plus some elementary methods <BR>
!! <HR>
!!! <H3>Copyright-Hinweis</H3>
!!                                                                   <BR>
!! Copyright (C) 2002 <A HREF="http://www.hamburg.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!!
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Paketes
!  01.01 : 2002-07-15 : G. Lang     : Startversion
!  01.02 : 2002/07/10 : G. Lang     : + weitere Korrekturen+Ergaenzungen
!  01.03 : 2002/07/15 : G. Lang     : + Arbeitsversion 15. Juli 2002
!  01.04 : 2002/07/17 : G. Lang     : + Arbeitsversion 2002-Juli-17
!  01.05 : 2002/07/18 : G. Lang     : + Arbeitsversion vom 2002-Juli-18
!  01.06 : 2002/07/19 : G. Lang     : + Arbeitsversion 2002-Juli-19
!  01.07 : 2002/07/22 : G. Lang     : + Arbeitsversion 2002-Juli-21
!  01.08 : 2002/07/23 : G. Lang     : + Arbeitsversion 2002-Juli-23
!  01.09 : 2002/07/24 : G. Lang     : + Arbeitsversion 2002-Juli-24
!  01.10 : 2002/09/05 : G. Lang     : + Test dxmin gibt mindestens .true. zurueck
!  01.11 : 2002/09/10 : G. Lang     : + OK-Methoden erweitert (Orthogonalitaet,Orientierung,...)
!  01.12 : 2002/09/11 : G. Lang     : + Orthogonalitaetskriterium korrigiert
!  01.13 : 2002/10/02 : J. Juerges  : Komponente xs (Mitten-Koordinaten der Kanten) hinzugefuegt
!  01.14 : 2003/04/03 : P. Schade   : + Komponente ncsize
!  01.15 : 2003/04/09 : P. Schade   : + Komponente knolg
!  01.16 : 2003/04/16 : P. Schade   : + Komponente knolg und Korrekturen an ncsize
!  01.17 : 2003-06-04 : P. Schade   : + Komponenten nptfr und nptir
!  01.18 : 2003/06/16 : H. Weilbeer : ok-Methode fuer nptfr geaendert
!  01.19 : 2003/12/02 : J. Juerges  : Neue Komponente nsi (number of inside sides)
!  01.20 : 2004-02-25 : G. Lang     : Setzen der Komponente NSI
!  01.21 : 2004/03/17 : G. Lang     : + Beim Test fuer HW wird optional auch mit HLAND verglichen
!  01.22 : 2004-05-27 : J. Juerges  : + Komponente xg (Polygon-Schwerpunktkoordinaten)
!  01.23 : 2004-06-25 : G. Lang     : Neue Komponente NSF (letzte Kante mit Fluss-Rb.)
!  01.24 : 2005/01/11 : S. Spohr    : Modifikation "get_ns_object_0"; Warning, falls Komponente "ns" == 0
!  02.01 : 2005-03-07 : G. Lang     : + Erweiterungen OpenMI (unvollstaendig)
!  02.02 : 2005-03-10 : G. Lang     : div. Erweiterungen fuer Konfigurationsphase OpenMI
!  02.03 : 2005-03-16 : G. Lang     : Erweiterungen fuer ALLBoundaries,AllOpenBoundaries und AllClosedBoundaries !
!  03.01 : 2005-07-21 : G. Lang     : Anpassungen/Erweiterungen fuer mod. OpenMI-ElementSet-Verstaendnis
!  04.01 : 2005-08-22 : G. Lang     : Lesen/Schreiben Delft3D-Gitternetz
!  04.02 : 2005-11-14 : G. Lang     : Erweiterungen wegen ISBND(:,:)
!  04.03 : 2005-11-23 : G. Lang     : Erweiterungen *.thd, *.lwl, *.ext
!  04.03 : 2005-12-28 : G. Lang     : div. DEALLOC-Funktionen publiziert, Ortho-Check mit Vorabtest auf 1./(DX*DY)
!  04.04 : 2006-01-04 : G. Lang     : in kill_h_grid_object_0 wird work_object vor Zugriff mit associated geprueft
!  04.05 : 2006-04-13 : G. Lang     : optionale unerodierbare Tiefen huu(:), hvu(:) und hwu(:)
!  04.07 : 2006-06-07 : G. Lang     : ok_h_grid_huu|hvu|hwu geben OK zurueck falls die Komponenten nicht allokiert sind
!  04.08 : 2006-08-31 : G. Lang     : neue Komponente "dwlp" = "Depth_At_Water_Level_Points" (fuer Delft3D-Konversion)
!  04.09 : 2007-02-05 : G. Lang     : neue Komponente "dg" ; Test von je(:,:) modifiziert ; swap_je_jb_jt
!  04.10 : 2007-02-19 : G. Lang     : verbesserte informative Meldungen in ok_h_grid_xc
!
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! <OL>
!!   <LI> Definieren elementarer Types f&uuml;r das Paket "h_grid";
!!   <LI> Speicherung globaler Daten f&uuml;r das Paket "h_grid"
!!        um den Datenaustausch innerhalb des Paketes zu vereinfachen;
!!   <LI> Elementare Methoden auf Daten des Typs "t_h_grid" des Paketes;
!!   <LI> Ein direkter Zugriff auf diese Daten/Methoden von anderen Paketen 
!!        aus ist nicht zul&auml;ssig.
!! </OL>
!! <HR>
!!                                                                  <BR>
!
MODULE m_h_grid_data
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] Basis-Modul mit globalen Konstantwerten [ggf. entfernen]
  !
  USE b_constants, ONLY : &
       ! Konstantwerte
       Double
  !
  USE b_error, ONLY :       &
       ! Typdefinitionen
       t_error,             &
       ! Routinen / Interfaces
       no_error,            &
       any_error,           &
       setup_error_act
  !
  USE b_file, ONLY :       &
       ! Typdefinitionen
       t_file,             &
       ! Routinen
       new_file,           &
       ok_file,            &
       print_file,         &
       set_file_unit,      &
       get_file_unit,      &
       get_file_form,      &
       get_file_delim,     &
       get_file_type,      &
       get_file_access,    &
       file_is_formatted,  &
       file_is_sequential
  !
  USE b_datetime, ONLY :   &
       ! Typdefinition
       t_datetime,         &
       ! Routinen / Interfaces
       new_datetime,       &
       kill_datetime,      &
       ok_datetime,        &
       print_datetime,     &
       datetime_to_string
  !
  USE b_omi_quant, ONLY : &
       ! Typdefinition
       t_omi_quant,       &
       ! Routinen / Interfaces
       ok_omi_quant,      &
       print_omi_quant,   &
       copy_omi_quant,    &
       kill_omi_quant
  !
  USE b_omi_xyz, ONLY :   &
       ! Typdefinition
       t_omi_xyz,         &
       ! Routinen / Interfaces
       ok_omi_xyz,        &
       print_omi_xyz,     &
       copy_omi_xyz,      &
       kill_omi_xyz
  !
  USE b_omi_ind, ONLY : &
       ! Typdefinition
       t_omi_ind,       &
       ! Routinen / Interfaces
       ok_omi_ind,      &
       print_omi_ind,   &
       copy_omi_ind,    &
       kill_omi_ind
  !
  USE b_omi_ele, ONLY : &
       ! Typdefinition
       t_omi_ele,       &
       ! Routinen / Interfaces
       ok_omi_ele,      &
       print_omi_ele,   &
       copy_omi_ele,    &
       kill_omi_ele
  !
  USE b_omi_dope, ONLY : &
       ! Typdefinition
       t_omi_dope,       &
       ! Routinen / Interfaces
       ok_omi_dope,      &
       print_omi_dope,   &
       copy_omi_dope,    &
       kill_omi_dope
  !
  USE b_omi_space, ONLY : &
       ! Typdefinition
       t_omi_space,       &
       ! Routinen / Interfaces
       ok_omi_space,      &
       print_omi_space,   &
       kill_omi_space
  !
  USE b_omi_stamp, ONLY : &
       ! Typdefinition
       t_omi_stamp,       &
       ! Routinen / Interfaces
       ok_omi_stamp,      &
       print_omi_stamp,   &
       kill_omi_stamp
  !
  USE b_omi_span, ONLY : &
       ! Typdefinition
       t_omi_span,       &
       ! Routinen / Interfaces
       ok_omi_span,      &
       print_omi_span,   &
       kill_omi_span
  !
  USE b_omi_exch, ONLY : &
       ! Typdefinition
       t_omi_exch,       &
       ! Routinen / Interfaces
       ok_omi_exch,      &
       print_omi_exch,   &
       copy_omi_exch,    &
       kill_omi_exch
  !
  IMPLICIT NONE
  PRIVATE 
  !
  ! ---------------------------------------------------------------------
  ! [B] oeffentlich zugaengliche Deklarationen 
  ! ---------------------------------------------------------------------
  !
  !! max. L&auml;nge des Namens in einer Komponente des Typs "t_d3d_openbc"
  INTEGER , PUBLIC , PARAMETER :: c_len_d3d_openbc_name=20 ! 
  !! max. L&auml;nge der Typbezeichnungen 
  INTEGER , PUBLIC , PARAMETER :: c_len_d3d_openbc_type=1  !  
  !! Anzahl der Gitterkoordinaten f&uuml;r einen Randabschnitt
  INTEGER , PUBLIC , PARAMETER :: c_max_d3d_openbc_coor=4  ! 
  !! max. L&auml;nge der Typbezeichnung f&uuml;r das Vertikalprofil
  INTEGER , PUBLIC , PARAMETER :: c_len_d3d_openbc_prof=20 ! 
  !! max. L&auml;nge der Blockbezeichnungen mit Verweisen auf Amplituden- und Phasendateien
  INTEGER , PUBLIC , PARAMETER :: c_len_d3d_openbc_bloc=12 ! 
  !! max. Anzahl der Gitterkoordinaten zur Definition zusammenh&auml;ngender trockener Punkte
  INTEGER , PUBLIC , PARAMETER :: c_max_d3d_dry=4 ! 
  !! max. Anzahl der Zeichen in dem String zur Bezeichnung des Typs eines Wehrs
  !! oder eines Damms ("U" oder "V")
  INTEGER , PUBLIC , PARAMETER :: c_len_d3d_uv_type=1 ! 
  !! max. Anzahl der Gitterkoordinaten zur Definition von D&auml;mmen und Wehren
  INTEGER , PUBLIC , PARAMETER :: c_max_d3d_uv=4      ! 
  !! max. L&auml;nge der Zeichenkette <TT>dwlp</TT>
  INTEGER , PUBLIC , PARAMETER :: c_len_dwlp=4 ! 
  !! Liste mit den g&uuml;tigen Bezeichnungen f&uuml;r den Inhalt der Komponente "dwlp"
  CHARACTER (LEN=c_len_dwlp) :: c_dwlp_valid(3) = (/ 'MEAN', 'MAX ', 'MIN ' /) ! 
  !
  ! [B.0] Sub-Typdefinitionen fuer den lokalen Gebrauch, insbesondere in
  !       Zusammenhang mit Delft3D
  !
  ! [B.0.1] Offene Raender
  TYPE , PUBLIC :: t_d3d_openbc
     CHARACTER (LEN=c_len_d3d_openbc_name) :: name      ! 
     CHARACTER (LEN=c_len_d3d_openbc_type) :: bdry_type ! 
     CHARACTER (LEN=c_len_d3d_openbc_type) :: data_type ! 
     INTEGER                               :: grid_coor(c_max_d3d_openbc_coor) ! 
     REAL (KIND=Double)                    :: refl_coef ! 
     CHARACTER (LEN=c_len_d3d_openbc_prof) :: prof      ! 
     CHARACTER (LEN=c_len_d3d_openbc_bloc) :: bloc_ampl ! 
     CHARACTER (LEN=c_len_d3d_openbc_bloc) :: bloc_phas ! 
  END TYPE t_d3d_openbc
  !
  ! [B.0.2] Duenne Daemme
  TYPE , PUBLIC :: t_d3d_thd
     CHARACTER (LEN=c_len_d3d_uv_type) :: type                    ! 
     INTEGER                           :: grid_coor(c_max_d3d_uv) ! 
  END TYPE t_d3d_thd
  !
  ! [B.0.3] Duenne Wehre (lokale und 2D)
  TYPE , PUBLIC :: t_d3d_weir
     CHARACTER (LEN=c_len_d3d_uv_type) :: type                    ! 
     INTEGER                           :: grid_coor(c_max_d3d_uv) ! 
     REAL (KIND=Double)                :: frction                ! 
     REAL (KIND=Double)                :: sill_depth              ! 
  END TYPE t_d3d_weir
  !
  ! 
  ! [B.1] Typdefinition
  !
  !! Typ zur Aufnahme aller (konstanten) Daten eines (Package-) Objekts <BR>
  !! Komponente "id"    : eindeutige Identifikationsnummer des Datensatzes <BR>
  !! Komponente "name"  : beschreibender Name des Datensatzes <BR>
  !! Komponente "file"  : Datei-Objekt <BR>
  !! Komponente "nv"    : Anzahl Gitterknoten (vertices) <BR>
  !! Komponente "ns"    : Anzahl Gitterkanten (sides) <BR>
  !! Komponente "nsi"   : Anzahl innere Gitterkanten (sides) <BR>
  !! Komponente "nsf"   : Zeiger auf letzte Kante mit Fluss-Randbedingung (sides) <BR>
  !! Komponente "ne"    : Anzahl Polygone im Gitter (elements) <BR>
  !! Komponente "nr"    : Anzahl der roten Polygone im Gitter <BR>
  !! Komponente "ncsize": Anzahl der benutzten CPUs (TELEMAC parallel rechnen)
  !! Komponente "nrand" : Anzahl der Knoten auf dem &auml;&szlig;eren Rand <BR>
  !! Komponente "nptfr" : Anzahl der Teilgebiets-Randknoten (TELEMAC parallel rechnen); <BR>
  !!                      nicht nur aeusserer Rand, auch Knoten an Inseln zaehlen <BR>
  !! Komponente "nptir" : Anzahl der Interface-Knoten (TELEMAC parallel rechnen); <BR>
  !!                      im Gesamtgebiet Innenknoten und im Teilgebiet Randknoten <BR>
  !! Komponente "xy"    : Knoten-x- und y-koordinaten <BR>
  !!                      - d=1 x-Koordinaten. <BR>
  !!                      - d=2 y-Koordinaten. <BR>
  !! Komponente "nen"   : Knotennummern aller Polygone <BR>
  !!                      - Knotennummern werden entgegen dem Uhrzeigersinn notiert. <BR>
  !!                      - Derzeit k&ouml;ennen max. 4 Knoten pro Element notiert werden. <BR>
  !! Komponente "irand" : Randkennungskode f&uuml;r Polygone (Ticad) <BR>
  !! Komponente "ks"    : Anzahl der Knoten/Kanten im Polygon <BR>
  !! Komponente "hv"    : Tiefe an den Knoten der Polygone <BR>
  !! Komponente "time"  : Datum und Uhrzeit <BR>
  !! Komponente "nbc"   : Anzahl der Polygone entlang offener R&auml;nder <BR>
  !! Komponente "hland" : Tiefenlage zur Kennzeichnung von dauerhaften Landgebieten <BR>
  !! Komponente "angle" : mittlere geopgraphische Breite <BR>
  !! Komponente "text"  : Ortsbeschreibung(en) <BR>
  !! Komponente "jb"    : erster Knoten einer Kante <BR>
  !! Komponente "jt"    : zweiter Knoten einer Kante <BR>
  !! Komponente "is"    : Kantenverzeichnis der Polygone <BR>
  !! Komponente "je"    : Nachbarverzeichnis der Kanten <BR>
  !! Komponente "ie"    : Nachbarverzeichnis der Polygone <BR>
  !! Komponente "xs"    : x- und y-Koordinaten der Kantenmitten <BR>
  !! Komponente "xc"    : x- und y-Koordinaten der Polygonzentren <BR>
  !! Komponente "xg"    : x- und y-Koordinaten der Polygonschwerpunkte <BR>
  !! Komponente "dx"    : Abstand zwischen den Zentren benachbarter Polygone <BR>
  !! Komponente "dy"    : L&auml;nge der Kanten <BR>
  !! Komponente "dg"    : Abstand zwischen den Schwerpunkten benachbarter Polygone <BR>
  !! Komponente "aa"    : Fl&auml;che der Polygone <BR>
  !! Komponente "hu"    : Tiefe auf den Kanten <BR>
  !! Komponente "hw"    : Tiefe &uuml;ber den Polygonen <BR>
  !! Komponente "ipobo" : Randkennungen der Knoten (SELAFIN-Gitter) <BR>
  !! Komponente "knolg" : Zeiger auf die Knotennummern des Gesamtgitters
  !!                      fuer Knoten im lokalen Grid (TELEMAC parallel rechnen) <BR>
  !! Komponente "dxmin" : minimal zul&auml;ssiger Abstand zwischen Zentren <BR>
  !! Komponente "quant(:)" : Feld mit OpenMI-konformen <EM>Quantities</EM> <BR>
  !! Komponente "xyz(:)"   : Feld mit OpenMI-konformen Koordinaten-Objekten <BR>
  !! Komponente "ind(:)"   : Feld mit OpenMI-konformen Zeigern auf Koordinaten-Objekte <BR>
  !! Komponente "ele(:)"   : Feld mit OpenMI-konformen Zeigern auf <EM>ElementSets</EM> <BR>
  !! Komponente "dope(:)"  : Feld mit OpenMI-konformen Datenoperations-Komponenten <BR>
  !! Komponente "exch(:)"  : Feld mit OpenMI-konformen <EM>ExchangeItems</EM> <BR>
  !! Komponente "span"     : Skalar mit OpenMI-konformer Zeitraum-Angabe <BR>
  !! Komponente "stamp"    : Skalar mit OpenMI-konformer Zeit-Angabe <BR>
  !! Komponente "space"    : Skalar mit OpenMI-konformer Koordinatenbezugs-Angabe <BR>
  !! Komponente "b_ms(:)" : Feld mit Haupt-Nummern der Randabschnitte (&auml;&szlig;erer Rand == 1) <BR>
  !! Komponente "b_ss(:)" : Feld mit Unterabschnitts-Nummern der Randabschnitte  <BR>
  !! Komponente "b_s(:)"  : Feld mit Kanten-Nummern der Randabschnitte, sortiert <BR>
  !!                        als erstes kommen die Kanten auf dem &auml;&seren Rand (entgegen
  !!                        dem Uhrzeigersinn), danach folgen, soweit vorhanden, die Kanten 
  !!                        der inneren R&auml;nder (im Uhrzeigersinn)
  !! Komponente "b_v(:)"  : Feld mit Knoten-Nummern der Randabschnitte, sortiert <BR>
  !! Komponente "b_t(:)"  : Feld mit Typ-Nummern der Randabschnitte (0=geschlossen,1=offen) <BR>
  !! Komponente "m"       : Anzahl der Gitterpunkte in M-Richtung eines Delft3D-Gitters <BR>
  !! Komponente "n"       : Anzahl der Gitterpunkte in N-Richtung eines Delft3D-Gitters <BR>
  !! Komponente "enc(:,:)"   : <EM>Enclosure</EM> eines Delft3D-Gitters <BR>
  !! Komponente "bnd(:)"     : Informationen zu den Randbedingungen am offenen Rand eines Delft3D-Gitters <BR>
  !! Komponente "dry(:,:)"   : Definition trockener Punkte in einem Delft3D-Gitter <BR>
  !! Komponente "thd(:)"  : D&uuml;nne D&auml;mme <BR>
  !! Komponente "lwl(:)"  : lokale Wehre <BR>
  !! Komponente "ext(:)"  : 2d_Wehre <BR>
  !! Komponente "isbnd(:,:)" : Zeiger f&uuml;r jede Seite eines Polygons auf bnd(:) [ bnd(isbnd(i,1...ks(i))) ] 
  !!                           falls isbnd(:,:) gr&ouml;&szlig;er als Null ist <BR>
  !! Komponente "isdam(:,:,:)" : Zeiger f&uuml;r jede Seite eines Polygons auf entweder thd(:), lwl(:)
  !!                             oder ext(:): <BR>
  !!                             thd(isdam(i,1...ks(i),1) falls isdam(i,1...ks(i),2) = 1 <BR>
  !!                             lwl(isdam(i,1...ks(i),1) falls isdam(i,1...ks(i),2) = 2 <BR>
  !!                             ext(isdam(i,1...ks(i),1) falls isdam(i,1...ks(i),2) = 3 <BR>
  !! Komponente "huu(:)" : Tiefenlage des nicht weiter erodierbaren Horizonts an den Kanten <BR>
  !! Komponente "hvu(:)" : Tiefenlage des nicht weiter erodierbaren Horizonts an den Knoten <BR>
  !! Komponente "hwu(:)" : Tiefenlage des nicht weiter erodierbaren Horizonts an den Polygonen
 TYPE , PUBLIC :: t_h_grid ! 
     INTEGER                      :: id       ! 
     CHARACTER (LEN=80)           :: name     ! 
     TYPE (t_file)                :: file     ! 
     INTEGER            , POINTER :: nv       ! 
     INTEGER            , POINTER :: ns       ! 
     INTEGER            , POINTER :: nsi      ! 
     INTEGER            , POINTER :: nsf      ! 
     INTEGER            , POINTER :: ne       ! 
     INTEGER            , POINTER :: nr       ! 
     INTEGER            , POINTER :: ncsize   ! 
     INTEGER            , POINTER :: nrand    ! 
     INTEGER            , POINTER :: nptfr    ! 
     INTEGER            , POINTER :: nptir    ! 
     REAL (KIND=Double) , POINTER :: xy(:,:)  ! 
     INTEGER            , POINTER :: nen(:,:) ! 
     INTEGER            , POINTER :: irand(:) ! 
     INTEGER            , POINTER :: ks(:)    ! 
     REAL (KIND=Double) , POINTER :: hv(:)    ! 
     TYPE (t_datetime)  , POINTER :: time     ! 
     INTEGER            , POINTER :: nbc      ! 
     REAL (KIND=Double) , POINTER :: hland    ! 
     REAL (KIND=Double) , POINTER :: angle    ! 
     CHARACTER (LEN=80) , POINTER :: text(:)  ! 
     INTEGER            , POINTER :: jb(:)    ! 
     INTEGER            , POINTER :: jt(:)    ! 
     INTEGER            , POINTER :: is(:,:)  ! 
     INTEGER            , POINTER :: je(:,:)  ! 
     INTEGER            , POINTER :: ie(:,:)  ! 
     REAL (KIND=Double) , POINTER :: xs(:,:)  ! 
     REAL (KIND=Double) , POINTER :: xc(:,:)  ! 
     REAL (KIND=Double) , POINTER :: xg(:,:)  ! 
     REAL (KIND=Double) , POINTER :: dx(:)    ! 
     REAL (KIND=Double) , POINTER :: dy(:)    ! 
     REAL (KIND=Double) , POINTER :: dg(:)    ! 
     REAL (KIND=Double) , POINTER :: aa(:)    ! 
     REAL (KIND=Double) , POINTER :: hu(:)    ! 
     REAL (KIND=Double) , POINTER :: hw(:)    ! 
     INTEGER            , POINTER :: ipobo(:) ! 
     REAL (KIND=Double) , POINTER :: dxmin    ! 
     INTEGER            , POINTER :: knolg(:) ! 
     TYPE (t_omi_quant) , POINTER :: quant(:) ! 
     TYPE (t_omi_xyz)   , POINTER :: xyz(:)   ! 
     TYPE (t_omi_ind)   , POINTER :: ind(:)   ! 
     TYPE (t_omi_ele)   , POINTER :: ele(:)   ! 
     TYPE (t_omi_dope)  , POINTER :: dope(:)  ! 
     TYPE (t_omi_exch)  , POINTER :: exch(:)  ! 
     TYPE (t_omi_span)  , POINTER :: span     ! 
     TYPE (t_omi_stamp) , POINTER :: stamp    ! 
     TYPE (t_omi_space) , POINTER :: space    ! 
     INTEGER            , POINTER :: b_ms(:)  ! 
     INTEGER            , POINTER :: b_ss(:)  ! 
     INTEGER            , POINTER :: b_s(:)   ! 
     INTEGER            , POINTER :: b_v(:)   ! 
     INTEGER            , POINTER :: b_t(:)   ! 
     INTEGER            , POINTER :: m        ! 
     INTEGER            , POINTER :: n          ! 
     INTEGER            , POINTER :: enc(:,:)   ! 
     TYPE (t_d3d_openbc), POINTER :: bnd(:)     ! offener Rand
     TYPE (t_d3d_thd)   , POINTER :: thd(:)     ! duenne Daemme
     TYPE (t_d3d_weir)  , POINTER :: lwl(:)     ! lokale Wehre
     TYPE (t_d3d_weir)  , POINTER :: ext(:)     ! 2D-Wehre
     INTEGER            , POINTER :: dry(:,:)     !  
     INTEGER            , POINTER :: isbnd(:,:)   !  
     INTEGER            , POINTER :: isdam(:,:,:) !  
     REAL (KIND=Double) , POINTER :: huu(:)       ! 
     REAL (KIND=Double) , POINTER :: hvu(:)       ! 
     REAL (KIND=Double) , POINTER :: hwu(:)       ! 
     CHARACTER (LEN=c_len_dwlp)  , POINTER :: dwlp ! 
  END TYPE t_h_grid
  !
  !! Verkettete Liste zum Verwalten mehrerer Objekte des Typs "t_h_grid"  <BR>
  !! Komponente "object" : ein Objekt "t_h_grid" der verketteten Liste <BR>
  !! Komponente "prev"   : Zeiger auf das vorangehende Objekt der Liste <BR>
  !! Komponente "next"   : Zeiger auf das nachfolgende Objekt der Liste
  TYPE , PUBLIC :: t_h_grid_list
     TYPE (t_h_grid)      , POINTER :: object ! ein (Package-) Objekt mit (vielen) Daten
     TYPE (t_h_grid_list) , POINTER :: prev   ! Zeiger auf vorangehendes Objekt
     TYPE (t_h_grid_list) , POINTER :: next   ! Zeiger auf nachfolgendes Objekt
  END TYPE t_h_grid_list
  !
  ! [B.2] Konstantwerte (Parameter)
  !
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PUBLIC,  PARAMETER :: c_op           = .false.         ! 
  !! Kanalnummer f&uuml;r Sequential Ascii Input/Output   
  INTEGER           , PUBLIC,  PARAMETER :: c_asc_seq_lun  = 15              ! 
  !! Kanalnummer f&uuml;r Sequential Binary Input/Output
  INTEGER           , PUBLIC,  PARAMETER :: c_bin_seq_lun  = 16              ! 
  !! Kanalnummer f&uuml;r Direct Binary Input/Output
  INTEGER           , PUBLIC,  PARAMETER :: c_bin_dir_lun  = 17              ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PUBLIC,  PARAMETER :: c_lun          = -1              ! 
  !! Anzahl der anwenderspezifischen Datenkomponenten des Typs t_h_grid 
  INTEGER           , PUBLIC,  PARAMETER :: c_nofcomp      = 65              ! 
  !! Wert zum Kennzeichnen fehlender Gr&ouml;&szlig;en
  REAL (KIND=Double), PUBLIC,  PARAMETER :: c_missing_h_grid_double=1.1E+30_Double ! 
  !
  !! Definition der implementierten ein-eindeutigen Datei-Varianten <BR>
  !! Anzahl der implementierten Varianten f&uuml;r i/o
  INTEGER           , PUBLIC,  PARAMETER :: c_max_variants = 6              ! 
  !! --> die definierten Datei-Varianten muessen ein-eindeutig sein <---
  !! Bezeichnung der Datei-TYPE-Varianten
  CHARACTER (LEN=10), PUBLIC,  PARAMETER :: c_variants_type(c_max_variants) = (/           & ! 
       'GITTER05  ' , 'GITTER05  ' , 'UNTRIM_BAW' , 'UNTRIM_VC ' , 'SELAFIN   ' , 'DELFT3D   '  /) ! 
  !! Bezeichnung der Datei-CODE-Varianten (gemaess dirdef1)
  INTEGER           , PUBLIC,  PARAMETER :: c_variants_code(c_max_variants) = (/           & ! 
       111          , 111          , 114          , 116          , 112          , 49            /) ! 
  !! Bezeichnung der implementieren Fortran ACCESS-Varianten
  CHARACTER (LEN=10), PUBLIC,  PARAMETER :: c_variants_access(c_max_variants) = (/         & ! 
       'SEQUENTIAL' , 'SEQUENTIAL' , 'SEQUENTIAL' , 'SEQUENTIAL' , 'SEQUENTIAL' , 'SEQUENTIAL'  /) ! 
  !! Bezeichnung der implementieren Fortran FORM-Varianten
  CHARACTER (LEN=11), PUBLIC,  PARAMETER :: c_variants_form(c_max_variants) = (/           & ! 
       'FORMATTED  ', 'UNFORMATTED', 'FORMATTED  ', 'FORMATTED  ', 'UNFORMATTED', 'FORMATTED  ' /) ! 
  !! Bezeichnung der implementieren Fortran DELIM-Varianten
  CHARACTER (LEN=10), PUBLIC,  PARAMETER :: c_variants_delim(c_max_variants) = (/          & ! 
       'NONE      ' , 'NONE      ' , 'QUOTE     ' , 'QUOTE     ' , 'NONE      ' , 'NONE      '  /) ! 
  !
  ! [B.3] Variablen 
  !
  !! Feld zur Aufnahme aller Fehlermeldungen des Moduls
  TYPE (t_error) , ALLOCATABLE   , PUBLIC , SAVE :: all_errors(:)         !
  !! Indikator f&uuml;r eine erfolgreich durchgef&uuml;hrte Modulinitialisierung
  LOGICAL                        , PUBLIC , SAVE :: initialised = .false. ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung PRINT-Methoden
  LOGICAL                        , PUBLIC , SAVE :: prn_op      = c_op    ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE-Methoden
  LOGICAL                        , PUBLIC , SAVE :: trc_op      = c_op    ! 
  !! logische Kanalnummer f&uuml;r PRINT-Methoden
  INTEGER                        , PUBLIC , SAVE :: prn_lun     = c_lun   ! 
  !! logische Kanalnummer f&uuml;r TRACE-Methoden
  INTEGER                        , PUBLIC , SAVE :: trc_lun     = c_lun   ! 
  !! logische Kanalnummer f&uuml;r Sequential Ascii Input/Output
  INTEGER                        , PUBLIC , SAVE :: asc_seq_lun = c_asc_seq_lun ! 
  !! logische Kanalnummer f&uuml;r Sequential Binary Input/Output
  INTEGER                        , PUBLIC , SAVE :: bin_seq_lun = c_bin_seq_lun ! 
  !! logische Kanalnummer f&uuml;r Direct Binary Input/Output
  INTEGER                        , PUBLIC , SAVE :: bin_dir_lun = c_bin_dir_lun ! 
  !
  !! aktuelle Anzahl verwalteter (Package-) Objekte
  INTEGER                        , PUBLIC, SAVE :: nofobjects = 0        ! 
  !! Zeiger auf das erste verwaltete (Package-) Objekt (in Liste)
  TYPE (t_h_grid_list) , POINTER , PUBLIC, SAVE :: first_list_object     ! 
  !! Zeiger auf das aktuell zu bearbeitende (Package-) Objekt (in Liste)
  TYPE (t_h_grid_list) , POINTER , PUBLIC, SAVE :: work_list_object      ! 
  !! Zeiger auf das aktuell zu bearbeitende (Package-) Objekt
  TYPE (t_h_grid)      , POINTER , PUBLIC, SAVE :: work_object           ! 
  !
  ! [B.4] Schnittstellen
  !
  !! Erzeugen eines neuen Gitterobjektes
  INTERFACE new_h_grid_object
     MODULE PROCEDURE new_h_grid_object_0
  END INTERFACE
  !! Entfernen eines vorhandenen Gitterobjektes
  INTERFACE kill_h_grid_object
     MODULE PROCEDURE kill_h_grid_object_0
  END INTERFACE
  !! Ermittle den Zeiger auf ein Objekt mit "id" <BR>
  !! a) auf der Basis der Identifikationsnummer <BR>
  !! b) auf der Basis des Objektnamens
  INTERFACE get_h_grid_list_object
     MODULE PROCEDURE get_h_grid_list_object_0
     MODULE PROCEDURE get_h_grid_list_object_n0
  END INTERFACE
  !! Pr&uuml;fe ob ein Arbeitsobjekt vorhanden ist
  INTERFACE ok_work_object
     MODULE PROCEDURE ok_work_object_0
  END INTERFACE
  !! Holen einer Kopie f&uuml;r Komponente "name" aus Object "t_h_grid"
  INTERFACE get_name_object
     MODULE PROCEDURE get_name_object_0
  END INTERFACE
  !! Holen einer Kopie f&uuml;r Komponente "file" aus Object "t_h_grid"
  INTERFACE get_file_object
     MODULE PROCEDURE get_file_object_0
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "nv" aus Object "t_h_grid"
  INTERFACE get_nv_object
     MODULE PROCEDURE get_nv_object_0
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "ne" aus Object "t_h_grid"
  INTERFACE get_ne_object
     MODULE PROCEDURE get_ne_object_0
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "nr" aus Object "t_h_grid"
  INTERFACE get_nr_object
     MODULE PROCEDURE get_nr_object_0
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "ncsize" aus Object "t_h_grid"
  INTERFACE get_ncsize_object
     MODULE PROCEDURE get_ncsize_object_0
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "ns" aus Object "t_h_grid"
  INTERFACE get_ns_object
     MODULE PROCEDURE get_ns_object_0
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "nsi" aus Object "t_h_grid"
  INTERFACE get_nsi_object
     MODULE PROCEDURE get_nsi_object_0
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "nsf" aus Object "t_h_grid"
  INTERFACE get_nsf_object
     MODULE PROCEDURE get_nsf_object_0
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "xy" aus Object "t_h_grid"
  INTERFACE get_xy_object
     MODULE PROCEDURE get_xy_object_2
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "nen" aus Object "t_h_grid"
  INTERFACE get_nen_object
     MODULE PROCEDURE get_nen_object_2
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "irand" aus Object "t_h_grid"
  INTERFACE get_irand_object
     MODULE PROCEDURE get_irand_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "ks" aus Object "t_h_grid"
  INTERFACE get_ks_object
     MODULE PROCEDURE get_ks_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "hv" aus Object "t_h_grid"
  INTERFACE get_hv_object
     MODULE PROCEDURE get_hv_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "nrand" aus Object "t_h_grid"
  INTERFACE get_nrand_object
     MODULE PROCEDURE get_nrand_object_0
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "nptfr" aus Object "t_h_grid"
  INTERFACE get_nptfr_object
     MODULE PROCEDURE get_nptfr_object_0
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "nptir" aus Object "t_h_grid"
  INTERFACE get_nptir_object
     MODULE PROCEDURE get_nptir_object_0
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "time" aus Object "t_h_grid"
  INTERFACE get_time_object
     MODULE PROCEDURE get_time_object_0
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "nbc" aus Object "t_h_grid"
  INTERFACE get_nbc_object
     MODULE PROCEDURE get_nbc_object_0
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "hland" aus Object "t_h_grid"
  INTERFACE get_hland_object
     MODULE PROCEDURE get_hland_object_0
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "angle" aus Object "t_h_grid"
  INTERFACE get_angle_object
     MODULE PROCEDURE get_angle_object_0
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "text" aus Object "t_h_grid"
  INTERFACE get_text_object
     MODULE PROCEDURE get_text_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "jb" aus Object "t_h_grid"
  INTERFACE get_jb_object
     MODULE PROCEDURE get_jb_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "jt" aus Object "t_h_grid"
  INTERFACE get_jt_object
     MODULE PROCEDURE get_jt_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "is" aus Object "t_h_grid"
  INTERFACE get_is_object
     MODULE PROCEDURE get_is_object_2
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "je" aus Object "t_h_grid"
  INTERFACE get_je_object
     MODULE PROCEDURE get_je_object_2
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "ie" aus Object "t_h_grid"
  INTERFACE get_ie_object
     MODULE PROCEDURE get_ie_object_2
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "xs" aus Object "t_h_grid"
  INTERFACE get_xs_object
     MODULE PROCEDURE get_xs_object_2
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "xc" aus Object "t_h_grid"
  INTERFACE get_xc_object
     MODULE PROCEDURE get_xc_object_2
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "xg" aus Object "t_h_grid"
  INTERFACE get_xg_object
     MODULE PROCEDURE get_xg_object_2
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "dx" aus Object "t_h_grid"
  INTERFACE get_dx_object
     MODULE PROCEDURE get_dx_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "dy" aus Object "t_h_grid"
  INTERFACE get_dy_object
     MODULE PROCEDURE get_dy_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "dg" aus Object "t_h_grid"
  INTERFACE get_dg_object
     MODULE PROCEDURE get_dg_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "aa" aus Object "t_h_grid"
  INTERFACE get_aa_object
     MODULE PROCEDURE get_aa_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "hu" aus Object "t_h_grid"
  INTERFACE get_hu_object
     MODULE PROCEDURE get_hu_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "hw" aus Object "t_h_grid"
  INTERFACE get_hw_object
     MODULE PROCEDURE get_hw_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "ipobo" aus Object "t_h_grid"
  INTERFACE get_ipobo_object
     MODULE PROCEDURE get_ipobo_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "knolg" aus Object "t_h_grid"
  INTERFACE get_knolg_object
     MODULE PROCEDURE get_knolg_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "quant" aus Object "t_h_grid"
  INTERFACE get_quant_object
     MODULE PROCEDURE get_quant_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "xyz" aus Object "t_h_grid"
  INTERFACE get_xyz_object
     MODULE PROCEDURE get_xyz_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "ind" aus Object "t_h_grid"
  INTERFACE get_ind_object
     MODULE PROCEDURE get_ind_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "ele" aus Object "t_h_grid"
  INTERFACE get_ele_object
     MODULE PROCEDURE get_ele_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "dope" aus Object "t_h_grid"
  INTERFACE get_dope_object
     MODULE PROCEDURE get_dope_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "exch" aus Object "t_h_grid"
  INTERFACE get_exch_object
     MODULE PROCEDURE get_exch_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "span" aus Object "t_h_grid"
  INTERFACE get_span_object
     MODULE PROCEDURE get_span_object_0
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "stamp" aus Object "t_h_grid"
  INTERFACE get_stamp_object
     MODULE PROCEDURE get_stamp_object_0
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "space" aus Object "t_h_grid"
  INTERFACE get_space_object
     MODULE PROCEDURE get_space_object_0
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "dxmin" aus Object "t_h_grid"
  INTERFACE get_dxmin_object
     MODULE PROCEDURE get_dxmin_object_0
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "b_ms(:)" aus Object "t_h_grid"
  INTERFACE get_b_ms_object
     MODULE PROCEDURE get_b_ms_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "b_ss(:)" aus Object "t_h_grid"
  INTERFACE get_b_ss_object
     MODULE PROCEDURE get_b_ss_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "b_s(:)" aus Object "t_h_grid"
  INTERFACE get_b_s_object
     MODULE PROCEDURE get_b_s_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "b_v(:)" aus Object "t_h_grid"
  INTERFACE get_b_v_object
     MODULE PROCEDURE get_b_v_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "b_t(:)" aus Object "t_h_grid"
  INTERFACE get_b_t_object
     MODULE PROCEDURE get_b_t_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "m" aus Object "t_h_grid"
  INTERFACE get_m_object
     MODULE PROCEDURE get_m_object_0
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "n" aus Object "t_h_grid"
  INTERFACE get_n_object
     MODULE PROCEDURE get_n_object_0
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "enc(:,:)" aus Object "t_h_grid"
  INTERFACE get_enc_object
     MODULE PROCEDURE get_enc_object_2
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "bnd(:)" aus Object "t_h_grid"
  INTERFACE get_bnd_object
     MODULE PROCEDURE get_bnd_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "thd(:)" aus Object "t_h_grid"
  INTERFACE get_thd_object
     MODULE PROCEDURE get_thd_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "lwl(:)" aus Object "t_h_grid"
  INTERFACE get_lwl_object
     MODULE PROCEDURE get_lwl_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "ext(:)" aus Object "t_h_grid"
  INTERFACE get_ext_object
     MODULE PROCEDURE get_ext_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "dry(:,:)" aus Object "t_h_grid"
  INTERFACE get_dry_object
     MODULE PROCEDURE get_dry_object_2
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "isbnd(:,:)" aus Object "t_h_grid"
  INTERFACE get_isbnd_object
     MODULE PROCEDURE get_isbnd_object_2
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r Komponente "isdam(:,:,:)" aus Object "t_h_grid"
  INTERFACE get_isdam_object
     MODULE PROCEDURE get_isdam_object_3
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r die Komponente "huu(:)" aus Object "t_h_grid"
  INTERFACE get_huu_object
     MODULE PROCEDURE get_huu_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r die Komponente "hvu(:)" aus Object "t_h_grid"
  INTERFACE get_hvu_object
     MODULE PROCEDURE get_hvu_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r die Komponente "hwu(:)" aus Object "t_h_grid"
  INTERFACE get_hwu_object
     MODULE PROCEDURE get_hwu_object_1
  END INTERFACE
  !! Holen eines Zeigers f&uuml;r die Komponente "dwlp" aus Object "t_h_grid"
  INTERFACE get_dwlp_object
     MODULE PROCEDURE get_dwlp_object_0
  END INTERFACE
  !! Ermitteln des Codes f&uuml;r die Interpretation der Komponente "dwlp" aus Object "t_h_grid"
  INTERFACE get_dwlp_code_object
     MODULE PROCEDURE get_dwlp_code_object_0
  END INTERFACE
  !
  !! Setzen der Komponente "name" in einem Objekt "t_h_grid"
  INTERFACE setup_name_object
     MODULE PROCEDURE setup_name_object_0
  END INTERFACE
  !! Setzen der Komponente "file" in einem Objekt "t_h_grid"
  INTERFACE setup_file_object
     MODULE PROCEDURE setup_file_object_0
  END INTERFACE
  !! Setzen der Komponente "nr" in einem Objekt "t_h_grid"
  INTERFACE setup_nr_object
     MODULE PROCEDURE setup_nr_object_0
  END INTERFACE
  !! Setzen der Komponente "nsi" in einem Objekt "t_h_grid"
  INTERFACE setup_nsi_object
     MODULE PROCEDURE setup_nsi_object_0
  END INTERFACE
  !! Setzen der Komponente "nsf" in einem Objekt "t_h_grid"
  INTERFACE setup_nsf_object
     MODULE PROCEDURE setup_nsf_object_0
  END INTERFACE
  !! Setzen der Komponente "ncsize" in einem Objekt "t_h_grid"
  INTERFACE setup_ncsize_object
     MODULE PROCEDURE setup_ncsize_object_0
  END INTERFACE
  !! Setzen der Komponente "xy" in einem Objekt "t_h_grid"
  INTERFACE setup_xy_object
     MODULE PROCEDURE setup_xy_object_2
  END INTERFACE
  !! Setzen der Komponente "nen" in einem Objekt "t_h_grid"
  INTERFACE setup_nen_object
     MODULE PROCEDURE setup_nen_object_2
  END INTERFACE
  !! Setzen der Komponente "irand" in einem Objekt "t_h_grid"
  INTERFACE setup_irand_object
     MODULE PROCEDURE setup_irand_object_1
  END INTERFACE
  !! Setzen der Komponente "ks" in einem Objekt "t_h_grid"
  INTERFACE setup_ks_object
     MODULE PROCEDURE setup_ks_object_1
  END INTERFACE
  !! Setzen der Komponente "hv" in einem Objekt "t_h_grid"
  INTERFACE setup_hv_object
     MODULE PROCEDURE setup_hv_object_1
  END INTERFACE
  !! Setzen der Komponente "nrand" in einem Objekt "t_h_grid"
  INTERFACE setup_nrand_object
     MODULE PROCEDURE setup_nrand_object_0
  END INTERFACE
  !! Setzen der Komponente "nptfr" in einem Objekt "t_h_grid"
  INTERFACE setup_nptfr_object
     MODULE PROCEDURE setup_nptfr_object_0
  END INTERFACE
  !! Setzen der Komponente "nptir" in einem Objekt "t_h_grid"
  INTERFACE setup_nptir_object
     MODULE PROCEDURE setup_nptir_object_0
  END INTERFACE
  !! Setzen der Komponente "time" in einem Objekt "t_h_grid"
  INTERFACE setup_time_object
     MODULE PROCEDURE setup_time_object_0
  END INTERFACE
  !! Setzen der Komponente "nbc" in einem Objekt "t_h_grid"
  INTERFACE setup_nbc_object
     MODULE PROCEDURE setup_nbc_object_0
  END INTERFACE
  !! Setzen der Komponente "hland" in einem Objekt "t_h_grid"
  INTERFACE setup_hland_object
     MODULE PROCEDURE setup_hland_object_0
  END INTERFACE
  !! Setzen der Komponente "angle" in einem Objekt "t_h_grid"
  INTERFACE setup_angle_object
     MODULE PROCEDURE setup_angle_object_0
  END INTERFACE
  !! Setzen der Komponente "text" in einem Objekt "t_h_grid" <BR>
  !! a) einen skalaren String auf die Komponente "text" kopieren <BR>
  !! b) ein Feld auf die Komponente "text" kopieren
  INTERFACE setup_text_object
     MODULE PROCEDURE setup_text_object_0
     MODULE PROCEDURE setup_text_object_1
  END INTERFACE
  !! Setzen der Komponente "jb" in einem Objekt "t_h_grid"
  INTERFACE setup_jb_object
     MODULE PROCEDURE setup_jb_object_1
  END INTERFACE
  !! Setzen der Komponente "jt" in einem Objekt "t_h_grid"
  INTERFACE setup_jt_object
     MODULE PROCEDURE setup_jt_object_1
  END INTERFACE
  !! Setzen der Komponente "is" in einem Objekt "t_h_grid"
  INTERFACE setup_is_object
     MODULE PROCEDURE setup_is_object_2
  END INTERFACE
  !! Setzen der Komponente "je" in einem Objekt "t_h_grid"
  INTERFACE setup_je_object
     MODULE PROCEDURE setup_je_object_2
  END INTERFACE
  !! Setzen der Komponente "ie" in einem Objekt "t_h_grid"
  INTERFACE setup_ie_object
     MODULE PROCEDURE setup_ie_object_2
  END INTERFACE
  !! Setzen der Komponente "xs" in einem Objekt "t_h_grid"
  INTERFACE setup_xs_object
     MODULE PROCEDURE setup_xs_object_2
  END INTERFACE
  !! Setzen der Komponente "xc" in einem Objekt "t_h_grid"
  INTERFACE setup_xc_object
     MODULE PROCEDURE setup_xc_object_2
  END INTERFACE
  !! Setzen der Komponente "xg" in einem Objekt "t_h_grid"
  INTERFACE setup_xg_object
     MODULE PROCEDURE setup_xg_object_2
  END INTERFACE
  !! Setzen der Komponente "dx" in einem Objekt "t_h_grid"
  INTERFACE setup_dx_object
     MODULE PROCEDURE setup_dx_object_1
  END INTERFACE
  !! Setzen der Komponente "dy" in einem Objekt "t_h_grid"
  INTERFACE setup_dy_object
     MODULE PROCEDURE setup_dy_object_1
  END INTERFACE
  !! Setzen der Komponente "dg" in einem Objekt "t_h_grid"
  INTERFACE setup_dg_object
     MODULE PROCEDURE setup_dg_object_1
  END INTERFACE
  !! Setzen der Komponente "aa" in einem Objekt "t_h_grid"
  INTERFACE setup_aa_object
     MODULE PROCEDURE setup_aa_object_1
  END INTERFACE
  !! Setzen der Komponente "hu" in einem Objekt "t_h_grid"
  INTERFACE setup_hu_object
     MODULE PROCEDURE setup_hu_object_1
  END INTERFACE
  !! Setzen der Komponente "hw" in einem Objekt "t_h_grid"
  INTERFACE setup_hw_object
     MODULE PROCEDURE setup_hw_object_1
  END INTERFACE
  !! Setzen der Komponente "ipobo" in einem Objekt "t_h_grid"
  INTERFACE setup_ipobo_object
     MODULE PROCEDURE setup_ipobo_object_1
  END INTERFACE
  !! Setzen der Komponente "knolg" in einem Objekt "t_h_grid"
  INTERFACE setup_knolg_object
     MODULE PROCEDURE setup_knolg_object_1
  END INTERFACE
  !! Setzen der Komponente "quant" in einem Objekt "t_h_grid"
  INTERFACE setup_quant_object
     MODULE PROCEDURE setup_quant_object_1
  END INTERFACE
  !! Setzen der Komponente "xyz" in einem Objekt "t_h_grid"
  INTERFACE setup_xyz_object
     MODULE PROCEDURE setup_xyz_object_1
  END INTERFACE
  !! Setzen der Komponente "ind" in einem Objekt "t_h_grid"
  INTERFACE setup_ind_object
     MODULE PROCEDURE setup_ind_object_1
  END INTERFACE
  !! Setzen der Komponente "ele" in einem Objekt "t_h_grid"
  INTERFACE setup_ele_object
     MODULE PROCEDURE setup_ele_object_1
  END INTERFACE
  !! Setzen der Komponente "dope" in einem Objekt "t_h_grid"
  INTERFACE setup_dope_object
     MODULE PROCEDURE setup_dope_object_1
  END INTERFACE
  !! Setzen der Komponente "exch" in einem Objekt "t_h_grid"
  INTERFACE setup_exch_object
     MODULE PROCEDURE setup_exch_object_1
  END INTERFACE
  !! Setzen der Komponente "span" in einem Objekt "t_h_grid"
  INTERFACE setup_span_object
     MODULE PROCEDURE setup_span_object_0
  END INTERFACE
  !! Setzen der Komponente "stamp" in einem Objekt "t_h_grid"
  INTERFACE setup_stamp_object
     MODULE PROCEDURE setup_stamp_object_0
  END INTERFACE
  !! Setzen der Komponente "space" in einem Objekt "t_h_grid"
  INTERFACE setup_space_object
     MODULE PROCEDURE setup_space_object_0
  END INTERFACE
  !! Setzen der Komponente "dxmin" in einem Objekt "t_h_grid"
  INTERFACE setup_dxmin_object
     MODULE PROCEDURE setup_dxmin_object_0
  END INTERFACE
  !! Setzen der Komponente "b_ms(:)" in einem Objekt "t_h_grid"
  INTERFACE setup_b_ms_object
     MODULE PROCEDURE setup_b_ms_object_1
  END INTERFACE
  !! Setzen der Komponente "b_ss(:)" in einem Objekt "t_h_grid"
  INTERFACE setup_b_ss_object
     MODULE PROCEDURE setup_b_ss_object_1
  END INTERFACE
  !! Setzen der Komponente "b_s(:)" in einem Objekt "t_h_grid"
  INTERFACE setup_b_s_object
     MODULE PROCEDURE setup_b_s_object_1
  END INTERFACE
  !! Setzen der Komponente "b_v(:)" in einem Objekt "t_h_grid"
  INTERFACE setup_b_v_object
     MODULE PROCEDURE setup_b_v_object_1
  END INTERFACE
  !! Setzen der Komponente "b_t(:)" in einem Objekt "t_h_grid"
  INTERFACE setup_b_t_object
     MODULE PROCEDURE setup_b_t_object_1
  END INTERFACE
  !! Setzen der Komponente "m" in einem Objekt "t_h_grid"
  INTERFACE setup_m_object
     MODULE PROCEDURE setup_m_object_0
  END INTERFACE
  !! Setzen der Komponente "n" in einem Objekt "t_h_grid"
  INTERFACE setup_n_object
     MODULE PROCEDURE setup_n_object_0
  END INTERFACE
  !! Setzen der Komponente "enc(:,:)" in einem Objekt "t_h_grid"
  INTERFACE setup_enc_object
     MODULE PROCEDURE setup_enc_object_2
  END INTERFACE
  !! Setzen der Komponente "bnd(:)" in einem Objekt "t_h_grid"
  INTERFACE setup_bnd_object
     MODULE PROCEDURE setup_bnd_object_1
  END INTERFACE
  !! Setzen der Komponente "thd(:)" in einem Objekt "t_h_grid"
  INTERFACE setup_thd_object
     MODULE PROCEDURE setup_thd_object_1
  END INTERFACE
  !! Setzen der Komponente "lwl(:)" in einem Objekt "t_h_grid"
  INTERFACE setup_lwl_object
     MODULE PROCEDURE setup_lwl_object_1
  END INTERFACE
  !! Setzen der Komponente "ext(:)" in einem Objekt "t_h_grid"
  INTERFACE setup_ext_object
     MODULE PROCEDURE setup_ext_object_1
  END INTERFACE
  !! Setzen der Komponente "dry(:,:)" in einem Objekt "t_h_grid"
  INTERFACE setup_dry_object
     MODULE PROCEDURE setup_dry_object_2
  END INTERFACE
  !! Setzen der Komponente "isbnd(:,:)" in einem Objekt "t_h_grid"
  INTERFACE setup_isbnd_object
     MODULE PROCEDURE setup_isbnd_object_2
  END INTERFACE
  !! Setzen der Komponente "isdam(:,:,:)" in einem Objekt "t_h_grid"
  INTERFACE setup_isdam_object
     MODULE PROCEDURE setup_isdam_object_3
  END INTERFACE
  !! Setzen der Komponente "huu(:)" in einem Objekt "t_h_grid"
  INTERFACE setup_huu_object
     MODULE PROCEDURE setup_huu_object_1
  END INTERFACE
  !! Setzen der Komponente "hvu(:)" in einem Objekt "t_h_grid"
  INTERFACE setup_hvu_object
     MODULE PROCEDURE setup_hvu_object_1
  END INTERFACE
  !! Setzen der Komponente "hwu(:)" in einem Objekt "t_h_grid"
  INTERFACE setup_hwu_object
     MODULE PROCEDURE setup_hwu_object_1
  END INTERFACE
  !! Setzen der Komponente "dwlp" in einem Objekt "t_h_grid"
  INTERFACE setup_dwlp_object
     MODULE PROCEDURE setup_dwlp_object_0
  END INTERFACE
  !
  !! De-Allokieren aller Komponenten eines Objektes "t_h_grid"
  INTERFACE dealloc_h_grid_object
     MODULE PROCEDURE dealloc_h_grid_object_0
  END INTERFACE
  !
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "nv" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_nv_object
     MODULE PROCEDURE target_nv_object_0
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "ne" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_ne_object
     MODULE PROCEDURE target_ne_object_0
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "nr" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_nr_object
     MODULE PROCEDURE target_nr_object_0
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "ncsize" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_ncsize_object
     MODULE PROCEDURE target_ncsize_object_0
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "ns" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_ns_object
     MODULE PROCEDURE target_ns_object_0
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "nsi" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_nsi_object
     MODULE PROCEDURE target_nsi_object_0
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "nsf" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_nsf_object
     MODULE PROCEDURE target_nsf_object_0
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "xy" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_xy_object
     MODULE PROCEDURE target_xy_object_2
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "nen" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_nen_object
     MODULE PROCEDURE target_nen_object_2
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "irand" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_irand_object
     MODULE PROCEDURE target_irand_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "ks" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_ks_object
     MODULE PROCEDURE target_ks_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "hv" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_hv_object
     MODULE PROCEDURE target_hv_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "nrand" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_nrand_object
     MODULE PROCEDURE target_nrand_object_0
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "nptfr" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_nptfr_object
     MODULE PROCEDURE target_nptfr_object_0
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "nptir" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_nptir_object
     MODULE PROCEDURE target_nptir_object_0
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "time" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_time_object
     MODULE PROCEDURE target_time_object_0
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "nbc" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_nbc_object
     MODULE PROCEDURE target_nbc_object_0
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "hland" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_hland_object
     MODULE PROCEDURE target_hland_object_0
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "angle" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_angle_object
     MODULE PROCEDURE target_angle_object_0
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "text" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_text_object
     MODULE PROCEDURE target_text_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "jb" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_jb_object
     MODULE PROCEDURE target_jb_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "jt" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_jt_object
     MODULE PROCEDURE target_jt_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "is" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_is_object
     MODULE PROCEDURE target_is_object_2
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "je" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_je_object
     MODULE PROCEDURE target_je_object_2
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "ie" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_ie_object
     MODULE PROCEDURE target_ie_object_2
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "xs" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_xs_object
     MODULE PROCEDURE target_xs_object_2
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "xc" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_xc_object
     MODULE PROCEDURE target_xc_object_2
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "xg" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_xg_object
     MODULE PROCEDURE target_xg_object_2
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "dx" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_dx_object
     MODULE PROCEDURE target_dx_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "dy" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_dy_object
     MODULE PROCEDURE target_dy_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "dg" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_dg_object
     MODULE PROCEDURE target_dg_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "aa" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_aa_object
     MODULE PROCEDURE target_aa_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "hu" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_hu_object
     MODULE PROCEDURE target_hu_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "hw" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_hw_object
     MODULE PROCEDURE target_hw_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "ipobo" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_ipobo_object
     MODULE PROCEDURE target_ipobo_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "knolg" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_knolg_object
     MODULE PROCEDURE target_knolg_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "quant" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_quant_object
     MODULE PROCEDURE target_quant_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "xyz" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_xyz_object
     MODULE PROCEDURE target_xyz_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "ind" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_ind_object
     MODULE PROCEDURE target_ind_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "ele" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_ele_object
     MODULE PROCEDURE target_ele_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "dope" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_dope_object
     MODULE PROCEDURE target_dope_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "exch" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_exch_object
     MODULE PROCEDURE target_exch_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "span" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_span_object
     MODULE PROCEDURE target_span_object_0
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "stamp" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_stamp_object
     MODULE PROCEDURE target_stamp_object_0
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "space" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_space_object
     MODULE PROCEDURE target_space_object_0
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "dxmin" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_dxmin_object
     MODULE PROCEDURE target_dxmin_object_0
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "b_ms(:)" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_b_ms_object
     MODULE PROCEDURE target_b_ms_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "b_ss(:)" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_b_ss_object
     MODULE PROCEDURE target_b_ss_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "b_s(:)" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_b_s_object
     MODULE PROCEDURE target_b_s_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "b_v(:)" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_b_v_object
     MODULE PROCEDURE target_b_v_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "b_t(:)" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_b_t_object
     MODULE PROCEDURE target_b_t_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "m" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_m_object
     MODULE PROCEDURE target_m_object_0
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "n" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_n_object
     MODULE PROCEDURE target_n_object_0
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "enc(:,:)" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_enc_object
     MODULE PROCEDURE target_enc_object_2
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "bnd(:)" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_bnd_object
     MODULE PROCEDURE target_bnd_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "thd(:)" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_thd_object
     MODULE PROCEDURE target_thd_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "lwl(:)" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_lwl_object
     MODULE PROCEDURE target_lwl_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "ext(:)" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_ext_object
     MODULE PROCEDURE target_ext_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "dry(:,:)" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_dry_object
     MODULE PROCEDURE target_dry_object_2
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "isbnd(:,:)" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_isbnd_object
     MODULE PROCEDURE target_isbnd_object_2
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "isdam(:,:,:)" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_isdam_object
     MODULE PROCEDURE target_isdam_object_3
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "huu(:)" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_huu_object
     MODULE PROCEDURE target_huu_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "hvu(:)" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_hvu_object
     MODULE PROCEDURE target_hvu_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "hwu(:)" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_hwu_object
     MODULE PROCEDURE target_hwu_object_1
  END INTERFACE
  !! Pr&uuml;fen ob ein Zeiger mit Ziel-Komponente "dwlp" in "t_h_grid" &uuml;bereinstimmt
  INTERFACE target_dwlp_object
     MODULE PROCEDURE target_dwlp_object_0
  END INTERFACE
  !
  !! Pr&uuml;fen Korrektheit eines Objektes "t_h_grid"
  INTERFACE ok_h_grid_object
     MODULE PROCEDURE ok_h_grid_object_0
  END INTERFACE
  !
  !! Pr&uuml;fen Korrektheit einer Datei-Variante f&uuml;r Objekt "t_h_grid"
  INTERFACE ok_h_grid_variant_no
     MODULE PROCEDURE ok_h_grid_variant_no_0
  END INTERFACE
  !
  !! Hole Id-Nummer einer Datei-Variante f&uuml;r Objekt "t_h_grid"
  INTERFACE get_h_grid_variant_no
     MODULE PROCEDURE get_h_grid_variant_no_0
  END INTERFACE
  !
  !! Drucken des Inhalts eines Objektes "t_h_grid"
  INTERFACE print_h_grid_object
     MODULE PROCEDURE print_h_grid_object_0
  END INTERFACE
  !
  !! Initialisieren des Inhalts eines Objektes "t_h_grid_list"
  INTERFACE init_h_grid_list_object
     MODULE PROCEDURE init_h_grid_list_object_0
  END INTERFACE
  !
  !! handelt es sich bei einer Kombination (Polygon,Kante) um "Water Level Kante" (Delft3D)
  INTERFACE is_polyedge_water_level
     MODULE PROCEDURE is_polyedge_water_level_2
  END INTERFACE
  !! handelt es sich bei einer Kombination (Polygon,Kante) um "Riemann Kante" (Delft3D)
  INTERFACE is_polyedge_riemann
     MODULE PROCEDURE is_polyedge_riemann_2
  END INTERFACE
  !! handelt es sich bei einer Kombination (Polygon,Kante) um "Current Kante" (Delft3D)
  INTERFACE is_polyedge_current
     MODULE PROCEDURE is_polyedge_current_2
  END INTERFACE
  !! handelt es sich bei einer Kombination (Polygon,Kante) um "Discharge Kante" (Delft3D)
  INTERFACE is_polyedge_discharge
     MODULE PROCEDURE is_polyedge_discharge_2
  END INTERFACE
  !! handelt es sich bei einer Kombination (Polygon,Kante) um "Total Discharge Kante" (Delft3D)
  INTERFACE is_polyedge_total_discharge
     MODULE PROCEDURE is_polyedge_total_discharge_2
  END INTERFACE
  !! handelt es sich bei einer Kombination (Polygon, Kante) um die Kante eines d&uuml;nnen Damms (Delft3D)
  INTERFACE is_polyedge_thin_dam
     MODULE PROCEDURE is_polyedge_thin_dam_2
  END INTERFACE
  !! handelt es sich bei einer Kombination (Polygon, Kante) um die Kante eines lokalen Wehrs (Delft3D)
  INTERFACE is_polyedge_local_weir
     MODULE PROCEDURE is_polyedge_local_weir_2
  END INTERFACE
  !! handelt es sich bei einer Kombination (Polygon, Kante) um die Kante eines 2D-Wehrs (Delft3D)
  INTERFACE is_polyedge_2d_weir
     MODULE PROCEDURE is_polyedge_2d_weir_2
  END INTERFACE
  !
  !! Umnummerieren von je, jb und jt falls wenigstens ein je(:,1) <= 0 ist
  INTERFACE swap_je_jb_jt
     MODULE PROCEDURE swap_je_jb_jt_d
  END INTERFACE
  !
  PUBLIC :: new_h_grid_object      ! 
  PUBLIC :: kill_h_grid_object     ! 
  PUBLIC :: get_h_grid_list_object ! 
  PUBLIC :: ok_work_object         ! 
  !
  PUBLIC :: get_name_object      ! 
  PUBLIC :: get_file_object      ! 
  PUBLIC :: get_nv_object        ! 
  PUBLIC :: get_ne_object        ! 
  PUBLIC :: get_ns_object        ! 
  PUBLIC :: get_nsi_object       ! 
  PUBLIC :: get_nsf_object       ! 
  PUBLIC :: get_nr_object        ! 
  PUBLIC :: get_ncsize_object    ! 
  PUBLIC :: get_xy_object        ! 
  PUBLIC :: get_nen_object       ! 
  PUBLIC :: get_irand_object     ! 
  PUBLIC :: get_ks_object        ! 
  PUBLIC :: get_hv_object        ! 
  PUBLIC :: get_nrand_object     ! 
  PUBLIC :: get_nptfr_object     ! 
  PUBLIC :: get_nptir_object     ! 
  PUBLIC :: get_time_object      ! 
  PUBLIC :: get_nbc_object       ! 
  PUBLIC :: get_hland_object     ! 
  PUBLIC :: get_angle_object     ! 
  PUBLIC :: get_text_object      ! 
  PUBLIC :: get_jb_object        ! 
  PUBLIC :: get_jt_object        ! 
  PUBLIC :: get_is_object        ! 
  PUBLIC :: get_je_object        ! 
  PUBLIC :: get_ie_object        ! 
  PUBLIC :: get_xs_object        ! 
  PUBLIC :: get_xc_object        ! 
  PUBLIC :: get_xg_object        ! 
  PUBLIC :: get_dx_object        ! 
  PUBLIC :: get_dy_object        ! 
  PUBLIC :: get_dg_object        ! 
  PUBLIC :: get_aa_object        ! 
  PUBLIC :: get_hu_object        ! 
  PUBLIC :: get_hw_object        ! 
  PUBLIC :: get_ipobo_object     ! 
  PUBLIC :: get_knolg_object     ! 
  PUBLIC :: get_quant_object     ! 
  PUBLIC :: get_xyz_object       ! 
  PUBLIC :: get_ind_object       ! 
  PUBLIC :: get_ele_object       ! 
  PUBLIC :: get_dope_object      ! 
  PUBLIC :: get_exch_object      ! 
  PUBLIC :: get_span_object      ! 
  PUBLIC :: get_stamp_object     ! 
  PUBLIC :: get_space_object     ! 
  PUBLIC :: get_dxmin_object     ! 
  PUBLIC :: get_b_ms_object      ! 
  PUBLIC :: get_b_ss_object      ! 
  PUBLIC :: get_b_s_object       ! 
  PUBLIC :: get_b_v_object       ! 
  PUBLIC :: get_b_t_object       ! 
  PUBLIC :: get_m_object         ! 
  PUBLIC :: get_n_object         ! 
  PUBLIC :: get_enc_object       ! 
  PUBLIC :: get_bnd_object       ! 
  PUBLIC :: get_thd_object       ! 
  PUBLIC :: get_lwl_object       !
  PUBLIC :: get_ext_object       !
  PUBLIC :: get_dry_object       ! 
  PUBLIC :: get_isbnd_object     ! 
  PUBLIC :: get_isdam_object     ! 
  PUBLIC :: get_huu_object       ! 
  PUBLIC :: get_hvu_object       ! 
  PUBLIC :: get_hwu_object       ! 
  PUBLIC :: get_dwlp_object      ! 
  PUBLIC :: get_dwlp_code_object ! 
  !
  PUBLIC :: setup_name_object    ! 
  PUBLIC :: setup_file_object    ! 
  PUBLIC :: setup_nr_object      ! 
  PUBLIC :: setup_nsi_object     ! 
  PUBLIC :: setup_nsf_object     ! 
  PUBLIC :: setup_ncsize_object  ! 
  PUBLIC :: setup_xy_object      ! 
  PUBLIC :: setup_nen_object     ! 
  PUBLIC :: setup_irand_object   ! 
  PUBLIC :: setup_ks_object      ! 
  PUBLIC :: setup_hv_object      ! 
  PUBLIC :: setup_nrand_object   ! 
  PUBLIC :: setup_nptfr_object   ! 
  PUBLIC :: setup_nptir_object   ! 
  PUBLIC :: setup_time_object    ! 
  PUBLIC :: setup_nbc_object     ! 
  PUBLIC :: setup_hland_object   ! 
  PUBLIC :: setup_angle_object   ! 
  PUBLIC :: setup_text_object    ! 
  PUBLIC :: setup_jb_object      !  
  PUBLIC :: setup_jt_object      !  
  PUBLIC :: setup_is_object      !  
  PUBLIC :: setup_je_object      !  
  PUBLIC :: setup_ie_object      !  
  PUBLIC :: setup_xs_object      !  
  PUBLIC :: setup_xc_object      !  
  PUBLIC :: setup_xg_object      !  
  PUBLIC :: setup_dx_object      !  
  PUBLIC :: setup_dy_object      !  
  PUBLIC :: setup_dg_object      !  
  PUBLIC :: setup_aa_object      !  
  PUBLIC :: setup_hu_object      !  
  PUBLIC :: setup_hw_object      !  
  PUBLIC :: setup_ipobo_object   !  
  PUBLIC :: setup_knolg_object   !  
  PUBLIC :: setup_quant_object   !  
  PUBLIC :: setup_xyz_object     !  
  PUBLIC :: setup_ele_object     !  
  PUBLIC :: setup_ind_object     !  
  PUBLIC :: setup_dope_object    !  
  PUBLIC :: setup_exch_object    !  
  PUBLIC :: setup_span_object    !  
  PUBLIC :: setup_stamp_object   !  
  PUBLIC :: setup_space_object   !  
  PUBLIC :: setup_dxmin_object   !  
  PUBLIC :: setup_b_ms_object    !  
  PUBLIC :: setup_b_ss_object    !  
  PUBLIC :: setup_b_s_object     !  
  PUBLIC :: setup_b_v_object     !  
  PUBLIC :: setup_b_t_object     !  
  PUBLIC :: setup_m_object       !  
  PUBLIC :: setup_n_object       !  
  PUBLIC :: setup_enc_object     !  
  PUBLIC :: setup_bnd_object     !  
  PUBLIC :: setup_thd_object     !  
  PUBLIC :: setup_lwl_object     !  
  PUBLIC :: setup_ext_object     !  
  PUBLIC :: setup_dry_object     !  
  PUBLIC :: setup_isbnd_object   !  
  PUBLIC :: setup_isdam_object   !  
  PUBLIC :: setup_huu_object     !  
  PUBLIC :: setup_hvu_object     !  
  PUBLIC :: setup_hwu_object     !  
  PUBLIC :: setup_dwlp_object    !  
  !
  PUBLIC :: dealloc_h_grid_object ! 
  !
  PUBLIC :: target_nv_object     ! 
  PUBLIC :: target_ne_object     ! 
  PUBLIC :: target_ns_object     ! 
  PUBLIC :: target_nsi_object    ! 
  PUBLIC :: target_nsf_object    ! 
  PUBLIC :: target_nr_object     ! 
  PUBLIC :: target_ncsize_object ! 
  PUBLIC :: target_xy_object     ! 
  PUBLIC :: target_nen_object    ! 
  PUBLIC :: target_irand_object  ! 
  PUBLIC :: target_ks_object     ! 
  PUBLIC :: target_hv_object     ! 
  PUBLIC :: target_nrand_object  ! 
  PUBLIC :: target_nptfr_object  ! 
  PUBLIC :: target_nptir_object  ! 
  PUBLIC :: target_time_object   ! 
  PUBLIC :: target_nbc_object    ! 
  PUBLIC :: target_hland_object  ! 
  PUBLIC :: target_angle_object  ! 
  PUBLIC :: target_text_object   ! 
  PUBLIC :: target_jb_object     ! 
  PUBLIC :: target_jt_object     ! 
  PUBLIC :: target_is_object     ! 
  PUBLIC :: target_je_object     ! 
  PUBLIC :: target_ie_object     ! 
  PUBLIC :: target_xs_object     ! 
  PUBLIC :: target_xc_object     ! 
  PUBLIC :: target_xg_object     ! 
  PUBLIC :: target_dx_object     !
  PUBLIC :: target_dy_object     !
  PUBLIC :: target_dg_object     !
  PUBLIC :: target_aa_object     !
  PUBLIC :: target_hu_object     !
  PUBLIC :: target_hw_object     !
  PUBLIC :: target_ipobo_object  !
  PUBLIC :: target_knolg_object  !
  PUBLIC :: target_quant_object  !
  PUBLIC :: target_xyz_object    !
  PUBLIC :: target_ele_object    !
  PUBLIC :: target_ind_object    !
  PUBLIC :: target_dope_object   !
  PUBLIC :: target_exch_object   !
  PUBLIC :: target_span_object   !
  PUBLIC :: target_stamp_object  !
  PUBLIC :: target_space_object  !
  PUBLIC :: target_dxmin_object  !
  PUBLIC :: target_b_ms_object   !
  PUBLIC :: target_b_ss_object   !
  PUBLIC :: target_b_s_object    !
  PUBLIC :: target_b_v_object    !
  PUBLIC :: target_b_t_object    !
  PUBLIC :: target_m_object      !
  PUBLIC :: target_n_object      !
  PUBLIC :: target_enc_object    !
  PUBLIC :: target_bnd_object    !
  PUBLIC :: target_thd_object    !
  PUBLIC :: target_lwl_object    !
  PUBLIC :: target_ext_object    !
  PUBLIC :: target_dry_object    !
  PUBLIC :: target_isbnd_object  !
  PUBLIC :: target_isdam_object  !
  PUBLIC :: target_huu_object    !
  PUBLIC :: target_hvu_object    !
  PUBLIC :: target_hwu_object    !
  PUBLIC :: target_dwlp_object   !
  !
  PUBLIC :: ok_h_grid_object     ! 
  !
  PUBLIC :: ok_h_grid_variant_no  ! 
  PUBLIC :: get_h_grid_variant_no ! 
  !
  PUBLIC :: print_h_grid_object ! 
  !
  PUBLIC :: init_h_grid_list_object ! 
  !
  PUBLIC :: is_polyedge_water_level
  PUBLIC :: is_polyedge_riemann
  PUBLIC :: is_polyedge_current
  PUBLIC :: is_polyedge_discharge
  PUBLIC :: is_polyedge_total_discharge
  PUBLIC :: is_polyedge_thin_dam
  PUBLIC :: is_polyedge_local_weir
  PUBLIC :: is_polyedge_2d_weir
  !
  PUBLIC :: swap_je_jb_jt
  !
  ! ---------------------------------------------------------------------
  ! [C] modulintern zugaengliche Datentypen, Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [C.1] lokale Typdefinitionen
  !
  ! [C.2] Konstantwerte (Parameter)
  !! Name des Moduls
  CHARACTER (LEN=13), PRIVATE, PARAMETER :: c_modname      = 'm_h_grid_data' ! 
  !
  ! [C.3] Schnittstellen
  !
  !! Allokieren von Memory f&uuml;r die Komponente "nv"
  INTERFACE alloc_h_grid_nv 
     MODULE PROCEDURE alloc_h_grid_nv_0
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "ns"
  INTERFACE alloc_h_grid_ns 
     MODULE PROCEDURE alloc_h_grid_ns_0
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "nsi"
  INTERFACE alloc_h_grid_nsi 
     MODULE PROCEDURE alloc_h_grid_nsi_0
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "nsf"
  INTERFACE alloc_h_grid_nsf 
     MODULE PROCEDURE alloc_h_grid_nsf_0
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "ne"
  INTERFACE alloc_h_grid_ne 
     MODULE PROCEDURE alloc_h_grid_ne_0
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "nr"
  INTERFACE alloc_h_grid_nr 
     MODULE PROCEDURE alloc_h_grid_nr_0
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "ncsize"
  INTERFACE alloc_h_grid_ncsize 
     MODULE PROCEDURE alloc_h_grid_ncsize_0
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "xy"
  INTERFACE alloc_h_grid_xy 
     MODULE PROCEDURE alloc_h_grid_xy_2
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "nen"
  INTERFACE alloc_h_grid_nen
     MODULE PROCEDURE alloc_h_grid_nen_2
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "irand"
  INTERFACE alloc_h_grid_irand 
     MODULE PROCEDURE alloc_h_grid_irand_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "ks"
  INTERFACE alloc_h_grid_ks 
     MODULE PROCEDURE alloc_h_grid_ks_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "hv"
  INTERFACE alloc_h_grid_hv 
     MODULE PROCEDURE alloc_h_grid_hv_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "nrand"
  INTERFACE alloc_h_grid_nrand 
     MODULE PROCEDURE alloc_h_grid_nrand_0
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "nptfr"
  INTERFACE alloc_h_grid_nptfr 
     MODULE PROCEDURE alloc_h_grid_nptfr_0
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "nptir"
  INTERFACE alloc_h_grid_nptir 
     MODULE PROCEDURE alloc_h_grid_nptir_0
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "time"
  INTERFACE alloc_h_grid_time 
     MODULE PROCEDURE alloc_h_grid_time_0
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "nbc"
  INTERFACE alloc_h_grid_nbc 
     MODULE PROCEDURE alloc_h_grid_nbc_0
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "hland"
  INTERFACE alloc_h_grid_hland 
     MODULE PROCEDURE alloc_h_grid_hland_0
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "angle"
  INTERFACE alloc_h_grid_angle 
     MODULE PROCEDURE alloc_h_grid_angle_0
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "text"
  INTERFACE alloc_h_grid_text
     MODULE PROCEDURE alloc_h_grid_text_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "jb"
  INTERFACE alloc_h_grid_jb
     MODULE PROCEDURE alloc_h_grid_jb_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "jt"
  INTERFACE alloc_h_grid_jt
     MODULE PROCEDURE alloc_h_grid_jt_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "is"
  INTERFACE alloc_h_grid_is
     MODULE PROCEDURE alloc_h_grid_is_2
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "je"
  INTERFACE alloc_h_grid_je
     MODULE PROCEDURE alloc_h_grid_je_2
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "ie"
  INTERFACE alloc_h_grid_ie
     MODULE PROCEDURE alloc_h_grid_ie_2
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "xs"
  INTERFACE alloc_h_grid_xs
     MODULE PROCEDURE alloc_h_grid_xs_2
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "xc"
  INTERFACE alloc_h_grid_xc
     MODULE PROCEDURE alloc_h_grid_xc_2
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "xg"
  INTERFACE alloc_h_grid_xg
     MODULE PROCEDURE alloc_h_grid_xg_2
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "dx"
  INTERFACE alloc_h_grid_dx
     MODULE PROCEDURE alloc_h_grid_dx_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "dy"
  INTERFACE alloc_h_grid_dy
     MODULE PROCEDURE alloc_h_grid_dy_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "dg"
  INTERFACE alloc_h_grid_dg
     MODULE PROCEDURE alloc_h_grid_dg_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "aa"
  INTERFACE alloc_h_grid_aa
     MODULE PROCEDURE alloc_h_grid_aa_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "hu"
  INTERFACE alloc_h_grid_hu
     MODULE PROCEDURE alloc_h_grid_hu_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "hw"
  INTERFACE alloc_h_grid_hw
     MODULE PROCEDURE alloc_h_grid_hw_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "ipobo"
  INTERFACE alloc_h_grid_ipobo
     MODULE PROCEDURE alloc_h_grid_ipobo_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "knolg"
  INTERFACE alloc_h_grid_knolg
     MODULE PROCEDURE alloc_h_grid_knolg_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "quant"
  INTERFACE alloc_h_grid_quant
     MODULE PROCEDURE alloc_h_grid_quant_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "xyz"
  INTERFACE alloc_h_grid_xyz
     MODULE PROCEDURE alloc_h_grid_xyz_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "ind"
  INTERFACE alloc_h_grid_ind
     MODULE PROCEDURE alloc_h_grid_ind_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "ele"
  INTERFACE alloc_h_grid_ele
     MODULE PROCEDURE alloc_h_grid_ele_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "dope"
  INTERFACE alloc_h_grid_dope
     MODULE PROCEDURE alloc_h_grid_dope_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "exch"
  INTERFACE alloc_h_grid_exch
     MODULE PROCEDURE alloc_h_grid_exch_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "span"
  INTERFACE alloc_h_grid_span
     MODULE PROCEDURE alloc_h_grid_span_0
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "stamp"
  INTERFACE alloc_h_grid_stamp
     MODULE PROCEDURE alloc_h_grid_stamp_0
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "space"
  INTERFACE alloc_h_grid_space
     MODULE PROCEDURE alloc_h_grid_space_0
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "dxmin"
  INTERFACE alloc_h_grid_dxmin
     MODULE PROCEDURE alloc_h_grid_dxmin_0
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "b_ms(:)"
  INTERFACE alloc_h_grid_b_ms
     MODULE PROCEDURE alloc_h_grid_b_ms_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "b_ss(:)"
  INTERFACE alloc_h_grid_b_ss
     MODULE PROCEDURE alloc_h_grid_b_ss_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "b_s(:)"
  INTERFACE alloc_h_grid_b_s
     MODULE PROCEDURE alloc_h_grid_b_s_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "b_v(:)"
  INTERFACE alloc_h_grid_b_v
     MODULE PROCEDURE alloc_h_grid_b_v_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "b_t(:)"
  INTERFACE alloc_h_grid_b_t
     MODULE PROCEDURE alloc_h_grid_b_t_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "m"
  INTERFACE alloc_h_grid_m
     MODULE PROCEDURE alloc_h_grid_m_0
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "n"
  INTERFACE alloc_h_grid_n
     MODULE PROCEDURE alloc_h_grid_n_0
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "enc(:,:)"
  INTERFACE alloc_h_grid_enc
     MODULE PROCEDURE alloc_h_grid_enc_2
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "bnd(:)"
  INTERFACE alloc_h_grid_bnd
     MODULE PROCEDURE alloc_h_grid_bnd_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "thd(:)"
  INTERFACE alloc_h_grid_thd
     MODULE PROCEDURE alloc_h_grid_thd_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "lwl(:)"
  INTERFACE alloc_h_grid_lwl
     MODULE PROCEDURE alloc_h_grid_lwl_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "ext(:)"
  INTERFACE alloc_h_grid_ext
     MODULE PROCEDURE alloc_h_grid_ext_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "dry(:,:)"
  INTERFACE alloc_h_grid_dry
     MODULE PROCEDURE alloc_h_grid_dry_2
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "isbnd(:,:)"
  INTERFACE alloc_h_grid_isbnd
     MODULE PROCEDURE alloc_h_grid_isbnd_2
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "isdam(:,:,:)"
  INTERFACE alloc_h_grid_isdam
     MODULE PROCEDURE alloc_h_grid_isdam_3
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "huu(:)"
  INTERFACE alloc_h_grid_huu
     MODULE PROCEDURE alloc_h_grid_huu_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "hvu(:)"
  INTERFACE alloc_h_grid_hvu
     MODULE PROCEDURE alloc_h_grid_hvu_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "hwu(:)"
  INTERFACE alloc_h_grid_hwu
     MODULE PROCEDURE alloc_h_grid_hwu_1
  END INTERFACE
  !! Allokieren von Memory f&uuml;r die Komponente "dwlp"
  INTERFACE alloc_h_grid_dwlp
     MODULE PROCEDURE alloc_h_grid_dwlp_0
  END INTERFACE
  !
  !! De-Allokieren von Memory f&uuml;r die Komponente "nv"
  INTERFACE dealloc_h_grid_nv 
     MODULE PROCEDURE dealloc_h_grid_nv_0
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "ns"
  INTERFACE dealloc_h_grid_ns 
     MODULE PROCEDURE dealloc_h_grid_ns_0
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "nsi"
  INTERFACE dealloc_h_grid_nsi 
     MODULE PROCEDURE dealloc_h_grid_nsi_0
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "nsf"
  INTERFACE dealloc_h_grid_nsf 
     MODULE PROCEDURE dealloc_h_grid_nsf_0
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "ne"
  INTERFACE dealloc_h_grid_ne 
     MODULE PROCEDURE dealloc_h_grid_ne_0
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "nr"
  INTERFACE dealloc_h_grid_nr 
     MODULE PROCEDURE dealloc_h_grid_nr_0
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "ncsize"
  INTERFACE dealloc_h_grid_ncsize 
     MODULE PROCEDURE dealloc_h_grid_ncsize_0
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "xy"
  INTERFACE dealloc_h_grid_xy 
     MODULE PROCEDURE dealloc_h_grid_xy_2
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "nen"
  INTERFACE dealloc_h_grid_nen
     MODULE PROCEDURE dealloc_h_grid_nen_2
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "irand"
  INTERFACE dealloc_h_grid_irand 
     MODULE PROCEDURE dealloc_h_grid_irand_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "ks"
  INTERFACE dealloc_h_grid_ks 
     MODULE PROCEDURE dealloc_h_grid_ks_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "hv"
  INTERFACE dealloc_h_grid_hv 
     MODULE PROCEDURE dealloc_h_grid_hv_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "nrand"
  INTERFACE dealloc_h_grid_nrand 
     MODULE PROCEDURE dealloc_h_grid_nrand_0
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "nptfr"
  INTERFACE dealloc_h_grid_nptfr 
     MODULE PROCEDURE dealloc_h_grid_nptfr_0
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "nptir"
  INTERFACE dealloc_h_grid_nptir 
     MODULE PROCEDURE dealloc_h_grid_nptir_0
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "time"
  INTERFACE dealloc_h_grid_time 
     MODULE PROCEDURE dealloc_h_grid_time_0
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "nbc"
  INTERFACE dealloc_h_grid_nbc 
     MODULE PROCEDURE dealloc_h_grid_nbc_0
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "hland"
  INTERFACE dealloc_h_grid_hland 
     MODULE PROCEDURE dealloc_h_grid_hland_0
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "angle"
  INTERFACE dealloc_h_grid_angle 
     MODULE PROCEDURE dealloc_h_grid_angle_0
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "text"
  INTERFACE dealloc_h_grid_text 
     MODULE PROCEDURE dealloc_h_grid_text_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "jb"
  INTERFACE dealloc_h_grid_jb 
     MODULE PROCEDURE dealloc_h_grid_jb_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "jt"
  INTERFACE dealloc_h_grid_jt 
     MODULE PROCEDURE dealloc_h_grid_jt_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "is"
  INTERFACE dealloc_h_grid_is
     MODULE PROCEDURE dealloc_h_grid_is_2
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "je"
  INTERFACE dealloc_h_grid_je
     MODULE PROCEDURE dealloc_h_grid_je_2
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "ie"
  INTERFACE dealloc_h_grid_ie
     MODULE PROCEDURE dealloc_h_grid_ie_2
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "xs"
  INTERFACE dealloc_h_grid_xs
     MODULE PROCEDURE dealloc_h_grid_xs_2
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "xc"
  INTERFACE dealloc_h_grid_xc
     MODULE PROCEDURE dealloc_h_grid_xc_2
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "xg"
  INTERFACE dealloc_h_grid_xg
     MODULE PROCEDURE dealloc_h_grid_xg_2
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "dx"
  INTERFACE dealloc_h_grid_dx
     MODULE PROCEDURE dealloc_h_grid_dx_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "dy"
  INTERFACE dealloc_h_grid_dy
     MODULE PROCEDURE dealloc_h_grid_dy_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "dg"
  INTERFACE dealloc_h_grid_dg
     MODULE PROCEDURE dealloc_h_grid_dg_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "aa"
  INTERFACE dealloc_h_grid_aa
     MODULE PROCEDURE dealloc_h_grid_aa_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "hu"
  INTERFACE dealloc_h_grid_hu
     MODULE PROCEDURE dealloc_h_grid_hu_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "hw"
  INTERFACE dealloc_h_grid_hw
     MODULE PROCEDURE dealloc_h_grid_hw_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "ipobo"
  INTERFACE dealloc_h_grid_ipobo
     MODULE PROCEDURE dealloc_h_grid_ipobo_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "knolg"
  INTERFACE dealloc_h_grid_knolg
     MODULE PROCEDURE dealloc_h_grid_knolg_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "quant"
  INTERFACE dealloc_h_grid_quant
     MODULE PROCEDURE dealloc_h_grid_quant_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "xyz"
  INTERFACE dealloc_h_grid_xyz
     MODULE PROCEDURE dealloc_h_grid_xyz_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "ind"
  INTERFACE dealloc_h_grid_ind
     MODULE PROCEDURE dealloc_h_grid_ind_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "ele"
  INTERFACE dealloc_h_grid_ele
     MODULE PROCEDURE dealloc_h_grid_ele_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "dope"
  INTERFACE dealloc_h_grid_dope
     MODULE PROCEDURE dealloc_h_grid_dope_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "exch"
  INTERFACE dealloc_h_grid_exch
     MODULE PROCEDURE dealloc_h_grid_exch_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "span"
  INTERFACE dealloc_h_grid_span
     MODULE PROCEDURE dealloc_h_grid_span_0
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "stamp"
  INTERFACE dealloc_h_grid_stamp
     MODULE PROCEDURE dealloc_h_grid_stamp_0
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "space"
  INTERFACE dealloc_h_grid_space
     MODULE PROCEDURE dealloc_h_grid_space_0
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "dxmin"
  INTERFACE dealloc_h_grid_dxmin
     MODULE PROCEDURE dealloc_h_grid_dxmin_0
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "b_ms(:)"
  INTERFACE dealloc_h_grid_b_ms
     MODULE PROCEDURE dealloc_h_grid_b_ms_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "b_ss(:)"
  INTERFACE dealloc_h_grid_b_ss
     MODULE PROCEDURE dealloc_h_grid_b_ss_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "b_s(:)"
  INTERFACE dealloc_h_grid_b_s
     MODULE PROCEDURE dealloc_h_grid_b_s_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "b_v(:)"
  INTERFACE dealloc_h_grid_b_v
     MODULE PROCEDURE dealloc_h_grid_b_v_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "b_t(:)"
  INTERFACE dealloc_h_grid_b_t
     MODULE PROCEDURE dealloc_h_grid_b_t_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "m"
  INTERFACE dealloc_h_grid_m
     MODULE PROCEDURE dealloc_h_grid_m_0
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "n"
  INTERFACE dealloc_h_grid_n
     MODULE PROCEDURE dealloc_h_grid_n_0
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "enc(:,:)"
  INTERFACE dealloc_h_grid_enc
     MODULE PROCEDURE dealloc_h_grid_enc_2
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "bnd(:)"
  INTERFACE dealloc_h_grid_bnd
     MODULE PROCEDURE dealloc_h_grid_bnd_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "thd(:)"
  INTERFACE dealloc_h_grid_thd
     MODULE PROCEDURE dealloc_h_grid_thd_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "lwl(:)"
  INTERFACE dealloc_h_grid_lwl
     MODULE PROCEDURE dealloc_h_grid_lwl_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "ext(:)"
  INTERFACE dealloc_h_grid_ext
     MODULE PROCEDURE dealloc_h_grid_ext_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "dry(:,:)"
  INTERFACE dealloc_h_grid_dry
     MODULE PROCEDURE dealloc_h_grid_dry_2
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "isbnd(:,:)"
  INTERFACE dealloc_h_grid_isbnd
     MODULE PROCEDURE dealloc_h_grid_isbnd_2
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "isdam(:,:,:)"
  INTERFACE dealloc_h_grid_isdam
     MODULE PROCEDURE dealloc_h_grid_isdam_3
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "huu(:)"
  INTERFACE dealloc_h_grid_huu
     MODULE PROCEDURE dealloc_h_grid_huu_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "hvu(:)"
  INTERFACE dealloc_h_grid_hvu
     MODULE PROCEDURE dealloc_h_grid_hvu_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "hwu(:)"
  INTERFACE dealloc_h_grid_hwu
     MODULE PROCEDURE dealloc_h_grid_hwu_1
  END INTERFACE
  !! De-Allokieren von Memory f&uuml;r die Komponente "dwlp"
  INTERFACE dealloc_h_grid_dwlp
     MODULE PROCEDURE dealloc_h_grid_dwlp_0
  END INTERFACE
  !
  !! Pr&uuml;fe Komponente "id" in Object "t_h_grid"
  INTERFACE ok_h_grid_id
     MODULE PROCEDURE ok_h_grid_id_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "name" in Object "t_h_grid"
  INTERFACE ok_h_grid_name
     MODULE PROCEDURE ok_h_grid_name_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "file" in Object "t_h_grid"
  INTERFACE ok_h_grid_file
     MODULE PROCEDURE ok_h_grid_file_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "nv" in Object "t_h_grid"
  INTERFACE ok_h_grid_nv
     MODULE PROCEDURE ok_h_grid_nv_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "ns" in Object "t_h_grid"
  INTERFACE ok_h_grid_ns
     MODULE PROCEDURE ok_h_grid_ns_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "nsi" in Object "t_h_grid"
  INTERFACE ok_h_grid_nsi
     MODULE PROCEDURE ok_h_grid_nsi_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "nsf" in Object "t_h_grid"
  INTERFACE ok_h_grid_nsf
     MODULE PROCEDURE ok_h_grid_nsf_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "ne" in Object "t_h_grid"
  INTERFACE ok_h_grid_ne
     MODULE PROCEDURE ok_h_grid_ne_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "nr" in Object "t_h_grid"
  INTERFACE ok_h_grid_nr
     MODULE PROCEDURE ok_h_grid_nr_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "ncsize" in Object "t_h_grid"
  INTERFACE ok_h_grid_ncsize
     MODULE PROCEDURE ok_h_grid_ncsize_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "xy" in Object "t_h_grid"
  INTERFACE ok_h_grid_xy
     MODULE PROCEDURE ok_h_grid_xy_2
  END INTERFACE
  !! Pr&uuml;fe Komponente "nen" in Object "t_h_grid"
  INTERFACE ok_h_grid_nen
     MODULE PROCEDURE ok_h_grid_nen_2
  END INTERFACE
  !! Pr&uuml;fe Komponente "irand" in Object "t_h_grid"
  INTERFACE ok_h_grid_irand
     MODULE PROCEDURE ok_h_grid_irand_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "ks" in Object "t_h_grid"
  INTERFACE ok_h_grid_ks
     MODULE PROCEDURE ok_h_grid_ks_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "hv" in Object "t_h_grid"
  INTERFACE ok_h_grid_hv
     MODULE PROCEDURE ok_h_grid_hv_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "nrand" in Object "t_h_grid"
  INTERFACE ok_h_grid_nrand
     MODULE PROCEDURE ok_h_grid_nrand_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "nptfr" in Object "t_h_grid"
  INTERFACE ok_h_grid_nptfr
     MODULE PROCEDURE ok_h_grid_nptfr_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "nptir" in Object "t_h_grid"
  INTERFACE ok_h_grid_nptir
     MODULE PROCEDURE ok_h_grid_nptir_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "time" in Object "t_h_grid"
  INTERFACE ok_h_grid_time
     MODULE PROCEDURE ok_h_grid_time_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "nbc" in Object "t_h_grid"
  INTERFACE ok_h_grid_nbc
     MODULE PROCEDURE ok_h_grid_nbc_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "hland" in Object "t_h_grid"
  INTERFACE ok_h_grid_hland
     MODULE PROCEDURE ok_h_grid_hland_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "angle" in Object "t_h_grid"
  INTERFACE ok_h_grid_angle
     MODULE PROCEDURE ok_h_grid_angle_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "text" in Object "t_h_grid"
  INTERFACE ok_h_grid_text
     MODULE PROCEDURE ok_h_grid_text_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "jb" in Object "t_h_grid"
  INTERFACE ok_h_grid_jb
     MODULE PROCEDURE ok_h_grid_jb_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "jt" in Object "t_h_grid"
  INTERFACE ok_h_grid_jt
     MODULE PROCEDURE ok_h_grid_jt_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "is" in Object "t_h_grid"
  INTERFACE ok_h_grid_is
     MODULE PROCEDURE ok_h_grid_is_2
  END INTERFACE
  !! Pr&uuml;fe Komponente "je" in Object "t_h_grid"
  INTERFACE ok_h_grid_je
     MODULE PROCEDURE ok_h_grid_je_2
  END INTERFACE
  !! Pr&uuml;fe Komponente "ie" in Object "t_h_grid"
  INTERFACE ok_h_grid_ie
     MODULE PROCEDURE ok_h_grid_ie_2
  END INTERFACE
  !! Pr&uuml;fe Komponente "xs" in Object "t_h_grid"
  INTERFACE ok_h_grid_xs
     MODULE PROCEDURE ok_h_grid_xs_2
  END INTERFACE
  !! Pr&uuml;fe Komponente "xc" in Object "t_h_grid"
  INTERFACE ok_h_grid_xc
     MODULE PROCEDURE ok_h_grid_xc_2
  END INTERFACE
  !! Pr&uuml;fe Komponente "xg" in Object "t_h_grid"
  INTERFACE ok_h_grid_xg
     MODULE PROCEDURE ok_h_grid_xg_2
  END INTERFACE
  !! Pr&uuml;fe Komponente "dx" in Object "t_h_grid"
  INTERFACE ok_h_grid_dx
     MODULE PROCEDURE ok_h_grid_dx_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "dy" in Object "t_h_grid"
  INTERFACE ok_h_grid_dy
     MODULE PROCEDURE ok_h_grid_dy_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "dg" in Object "t_h_grid"
  INTERFACE ok_h_grid_dg
     MODULE PROCEDURE ok_h_grid_dg_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "aa" in Object "t_h_grid"
  INTERFACE ok_h_grid_aa
     MODULE PROCEDURE ok_h_grid_aa_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "hu" in Object "t_h_grid"
  INTERFACE ok_h_grid_hu
     MODULE PROCEDURE ok_h_grid_hu_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "hw" in Object "t_h_grid"
  INTERFACE ok_h_grid_hw
     MODULE PROCEDURE ok_h_grid_hw_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "ipobo" in Object "t_h_grid"
  INTERFACE ok_h_grid_ipobo
     MODULE PROCEDURE ok_h_grid_ipobo_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "knolg" in Object "t_h_grid"
  INTERFACE ok_h_grid_knolg
     MODULE PROCEDURE ok_h_grid_knolg_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "quant" in Object "t_h_grid"
  INTERFACE ok_h_grid_quant
     MODULE PROCEDURE ok_h_grid_quant_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "xyz" in Object "t_h_grid"
  INTERFACE ok_h_grid_xyz
     MODULE PROCEDURE ok_h_grid_xyz_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "ind" in Object "t_h_grid"
  INTERFACE ok_h_grid_ind
     MODULE PROCEDURE ok_h_grid_ind_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "ele" in Object "t_h_grid"
  INTERFACE ok_h_grid_ele
     MODULE PROCEDURE ok_h_grid_ele_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "dope" in Object "t_h_grid"
  INTERFACE ok_h_grid_dope
     MODULE PROCEDURE ok_h_grid_dope_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "exch" in Object "t_h_grid"
  INTERFACE ok_h_grid_exch
     MODULE PROCEDURE ok_h_grid_exch_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "span" in Object "t_h_grid"
  INTERFACE ok_h_grid_span
     MODULE PROCEDURE ok_h_grid_span_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "stamp" in Object "t_h_grid"
  INTERFACE ok_h_grid_stamp
     MODULE PROCEDURE ok_h_grid_stamp_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "space" in Object "t_h_grid"
  INTERFACE ok_h_grid_space
     MODULE PROCEDURE ok_h_grid_space_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "dxmin" in Object "t_h_grid"
  INTERFACE ok_h_grid_dxmin
     MODULE PROCEDURE ok_h_grid_dxmin_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "b_ms(:)" in Object "t_h_grid"
  INTERFACE ok_h_grid_b_ms
     MODULE PROCEDURE ok_h_grid_b_ms_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "b_ss(:)" in Object "t_h_grid"
  INTERFACE ok_h_grid_b_ss
     MODULE PROCEDURE ok_h_grid_b_ss_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "b_s(:)" in Object "t_h_grid"
  INTERFACE ok_h_grid_b_s
     MODULE PROCEDURE ok_h_grid_b_s_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "b_v(:)" in Object "t_h_grid"
  INTERFACE ok_h_grid_b_v
     MODULE PROCEDURE ok_h_grid_b_v_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "b_t(:)" in Object "t_h_grid"
  INTERFACE ok_h_grid_b_t
     MODULE PROCEDURE ok_h_grid_b_t_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "m" in Object "t_h_grid"
  INTERFACE ok_h_grid_m
     MODULE PROCEDURE ok_h_grid_m_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "n" in Object "t_h_grid"
  INTERFACE ok_h_grid_n
     MODULE PROCEDURE ok_h_grid_n_0
  END INTERFACE
  !! Pr&uuml;fe Komponente "enc(:,:)" in Object "t_h_grid"
  INTERFACE ok_h_grid_enc
     MODULE PROCEDURE ok_h_grid_enc_2
  END INTERFACE
  !! Pr&uuml;fe Komponente "bnd(:)" in Object "t_h_grid"
  INTERFACE ok_h_grid_bnd
     MODULE PROCEDURE ok_h_grid_bnd_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "thd(:)" in Object "t_h_grid"
  INTERFACE ok_h_grid_thd
     MODULE PROCEDURE ok_h_grid_thd_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "lwl(:)" in Object "t_h_grid"
  INTERFACE ok_h_grid_lwl
     MODULE PROCEDURE ok_h_grid_lwl_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "ext(:)" in Object "t_h_grid"
  INTERFACE ok_h_grid_ext
     MODULE PROCEDURE ok_h_grid_ext_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "dry(:,:)" in Object "t_h_grid"
  INTERFACE ok_h_grid_dry
     MODULE PROCEDURE ok_h_grid_dry_2
  END INTERFACE
  !! Pr&uuml;fe Komponente "isbnd(:,:)" in Object "t_h_grid"
  INTERFACE ok_h_grid_isbnd
     MODULE PROCEDURE ok_h_grid_isbnd_2
  END INTERFACE
  !! Pr&uuml;fe Komponente "isdam(:,:,:)" in Object "t_h_grid"
  INTERFACE ok_h_grid_isdam
     MODULE PROCEDURE ok_h_grid_isdam_3
  END INTERFACE
  !! Pr&uuml;fe Komponente "huu(:)" in Object "t_h_grid"
  INTERFACE ok_h_grid_huu
     MODULE PROCEDURE ok_h_grid_huu_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "hvu(:)" in Object "t_h_grid"
  INTERFACE ok_h_grid_hvu
     MODULE PROCEDURE ok_h_grid_hvu_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "hwu(:)" in Object "t_h_grid"
  INTERFACE ok_h_grid_hwu
     MODULE PROCEDURE ok_h_grid_hwu_1
  END INTERFACE
  !! Pr&uuml;fe Komponente "dwlp" in Object "t_h_grid"
  INTERFACE ok_h_grid_dwlp
     MODULE PROCEDURE ok_h_grid_dwlp_0
  END INTERFACE
  !
  !! Initialisieren eines Objektes "t_h_grid"
  INTERFACE init_h_grid_object
     MODULE PROCEDURE init_h_grid_object_0
  END INTERFACE 
  !
  !! Drucke Inhalt der Komponente "ne" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_ne
     MODULE PROCEDURE print_h_grid_ne_0
  END INTERFACE
  !! Drucke Inhalt der Komponente "nv" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_nv
     MODULE PROCEDURE print_h_grid_nv_0
  END INTERFACE
  !! Drucke Inhalt der Komponente "ns" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_ns
     MODULE PROCEDURE print_h_grid_ns_0
  END INTERFACE
  !! Drucke Inhalt der Komponente "nsi" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_nsi
     MODULE PROCEDURE print_h_grid_nsi_0
  END INTERFACE
  !! Drucke Inhalt der Komponente "nsf" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_nsf
     MODULE PROCEDURE print_h_grid_nsf_0
  END INTERFACE
  !! Drucke Inhalt der Komponente "nr" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_nr
     MODULE PROCEDURE print_h_grid_nr_0
  END INTERFACE
  !! Drucke Inhalt der Komponente "ncsize" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_ncsize
     MODULE PROCEDURE print_h_grid_ncsize_0
  END INTERFACE
  !! Drucke Inhalt der Komponente "xy" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_xy
     MODULE PROCEDURE print_h_grid_xy_2
  END INTERFACE
  !! Drucke Inhalt der Komponente "nen" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_nen
     MODULE PROCEDURE print_h_grid_nen_2
  END INTERFACE
  !! Drucke Inhalt der Komponente "irand" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_irand
     MODULE PROCEDURE print_h_grid_irand_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "ks" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_ks
     MODULE PROCEDURE print_h_grid_ks_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "hv" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_hv
     MODULE PROCEDURE print_h_grid_hv_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "nrand" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_nrand
     MODULE PROCEDURE print_h_grid_nrand_0
  END INTERFACE
  !! Drucke Inhalt der Komponente "nptfr" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_nptfr
     MODULE PROCEDURE print_h_grid_nptfr_0
  END INTERFACE
  !! Drucke Inhalt der Komponente "nptir" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_nptir
     MODULE PROCEDURE print_h_grid_nptir_0
  END INTERFACE
  !! Drucke Inhalt der Komponente "time" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_time
     MODULE PROCEDURE print_h_grid_time_0
  END INTERFACE
  !! Drucke Inhalt der Komponente "nbc" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_nbc
     MODULE PROCEDURE print_h_grid_nbc_0
  END INTERFACE
  !! Drucke Inhalt der Komponente "hland" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_hland
     MODULE PROCEDURE print_h_grid_hland_0
  END INTERFACE
  !! Drucke Inhalt der Komponente "angle" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_angle
     MODULE PROCEDURE print_h_grid_angle_0
  END INTERFACE
  !! Drucke Inhalt der Komponente "text" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_text
     MODULE PROCEDURE print_h_grid_text_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "jt" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_jt
     MODULE PROCEDURE print_h_grid_jt_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "jb" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_jb
     MODULE PROCEDURE print_h_grid_jb_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "is" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_is
     MODULE PROCEDURE print_h_grid_is_2
  END INTERFACE
  !! Drucke Inhalt der Komponente "je" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_je
     MODULE PROCEDURE print_h_grid_je_2
  END INTERFACE
  !! Drucke Inhalt der Komponente "ie" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_ie
     MODULE PROCEDURE print_h_grid_ie_2
  END INTERFACE
  !! Drucke Inhalt der Komponente "xs" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_xs
     MODULE PROCEDURE print_h_grid_xs_2
  END INTERFACE
  !! Drucke Inhalt der Komponente "xc" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_xc
     MODULE PROCEDURE print_h_grid_xc_2
  END INTERFACE
  !! Drucke Inhalt der Komponente "xg" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_xg
     MODULE PROCEDURE print_h_grid_xg_2
  END INTERFACE
  !! Drucke Inhalt der Komponente "dx" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_dx
     MODULE PROCEDURE print_h_grid_dx_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "dy" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_dy
     MODULE PROCEDURE print_h_grid_dy_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "dg" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_dg
     MODULE PROCEDURE print_h_grid_dg_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "aa" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_aa
     MODULE PROCEDURE print_h_grid_aa_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "hu" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_hu
     MODULE PROCEDURE print_h_grid_hu_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "hw" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_hw
     MODULE PROCEDURE print_h_grid_hw_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "ipobo" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_ipobo
     MODULE PROCEDURE print_h_grid_ipobo_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "knolg" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_knolg
     MODULE PROCEDURE print_h_grid_knolg_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "quant" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_quant
     MODULE PROCEDURE print_h_grid_quant_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "xyz" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_xyz
     MODULE PROCEDURE print_h_grid_xyz_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "ind" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_ind
     MODULE PROCEDURE print_h_grid_ind_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "ele" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_ele
     MODULE PROCEDURE print_h_grid_ele_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "dope" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_dope
     MODULE PROCEDURE print_h_grid_dope_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "exch" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_exch
     MODULE PROCEDURE print_h_grid_exch_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "span" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_span
     MODULE PROCEDURE print_h_grid_span_0
  END INTERFACE
  !! Drucke Inhalt der Komponente "stamp" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_stamp
     MODULE PROCEDURE print_h_grid_stamp_0
  END INTERFACE
  !! Drucke Inhalt der Komponente "space" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_space
     MODULE PROCEDURE print_h_grid_space_0
  END INTERFACE
  !! Drucke Inhalt der Komponente "dxmin" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_dxmin
     MODULE PROCEDURE print_h_grid_dxmin_0
  END INTERFACE
  !! Drucke Inhalt der Komponente "b_ms(:)" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_b_ms
     MODULE PROCEDURE print_h_grid_b_ms_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "b_ss(:)" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_b_ss
     MODULE PROCEDURE print_h_grid_b_ss_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "b_s(:)" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_b_s
     MODULE PROCEDURE print_h_grid_b_s_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "b_v(:)" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_b_v
     MODULE PROCEDURE print_h_grid_b_v_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "b_t(:)" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_b_t
     MODULE PROCEDURE print_h_grid_b_t_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "m" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_m
     MODULE PROCEDURE print_h_grid_m_0
  END INTERFACE
  !! Drucke Inhalt der Komponente "n" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_n
     MODULE PROCEDURE print_h_grid_n_0
  END INTERFACE
  !! Drucke Inhalt der Komponente "enc(:,:)" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_enc
     MODULE PROCEDURE print_h_grid_enc_2
  END INTERFACE
  !! Drucke Inhalt der Komponente "bnd(:)" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_bnd
     MODULE PROCEDURE print_h_grid_bnd_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "thd(:)" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_thd
     MODULE PROCEDURE print_h_grid_thd_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "lwl(:)" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_lwl
     MODULE PROCEDURE print_h_grid_lwl_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "ext(:)" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_ext
     MODULE PROCEDURE print_h_grid_ext_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "dry(:,:)" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_dry
     MODULE PROCEDURE print_h_grid_dry_2
  END INTERFACE
  !! Drucke Inhalt der Komponente "isbnd(:,:)" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_isbnd
     MODULE PROCEDURE print_h_grid_isbnd_2
  END INTERFACE
  !! Drucke Inhalt der Komponente "isdam(:,:,:)" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_isdam
     MODULE PROCEDURE print_h_grid_isdam_3
  END INTERFACE
  !! Drucke Inhalt der Komponente "huu(:)" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_huu
     MODULE PROCEDURE print_h_grid_huu_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "hvu(:)" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_hvu
     MODULE PROCEDURE print_h_grid_hvu_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "hwu(:)" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_hwu
     MODULE PROCEDURE print_h_grid_hwu_1
  END INTERFACE
  !! Drucke Inhalt der Komponente "dwlp" eines Objektes "t_h_grid"
  INTERFACE print_h_grid_dwlp
     MODULE PROCEDURE print_h_grid_dwlp_0
  END INTERFACE
  !
  !! Umsetzen eines Textes in Gro&szlig;buchstaben
  INTERFACE get_uppercase_char
     MODULE PROCEDURE get_uppercase_char_0
  END INTERFACE get_uppercase_char
  !! Ermitteln einer neuen Objekt-Id
  INTERFACE get_h_grid_new_id
     MODULE PROCEDURE get_h_grid_new_id_0
  END INTERFACE
  !! Gibt es &uuml;berhaupt Objekte
  INTERFACE any_objects
     MODULE PROCEDURE any_objects_0
  END INTERFACE
  !
  PUBLIC :: dealloc_h_grid_irand
  PUBLIC :: dealloc_h_grid_m
  PUBLIC :: dealloc_h_grid_n
  PUBLIC :: dealloc_h_grid_enc
  PUBLIC :: dealloc_h_grid_bnd
  PUBLIC :: dealloc_h_grid_thd
  PUBLIC :: dealloc_h_grid_lwl
  PUBLIC :: dealloc_h_grid_ext
  PUBLIC :: dealloc_h_grid_isbnd
  PUBLIC :: dealloc_h_grid_isdam
  PUBLIC :: dealloc_h_grid_dry
  PUBLIC :: dealloc_h_grid_ncsize
  PUBLIC :: dealloc_h_grid_nptir
  PUBLIC :: dealloc_h_grid_nptfr
  PUBLIC :: dealloc_h_grid_ipobo
  PUBLIC :: dealloc_h_grid_knolg
  PUBLIC :: dealloc_h_grid_is
  PUBLIC :: dealloc_h_grid_jt
  PUBLIC :: dealloc_h_grid_jb
  PUBLIC :: dealloc_h_grid_je
  PUBLIC :: dealloc_h_grid_ie
  PUBLIC :: dealloc_h_grid_aa
  PUBLIC :: dealloc_h_grid_hw
  PUBLIC :: dealloc_h_grid_hv
  PUBLIC :: dealloc_h_grid_xs
  PUBLIC :: dealloc_h_grid_xc
  PUBLIC :: dealloc_h_grid_xg
  PUBLIC :: dealloc_h_grid_dy
  PUBLIC :: dealloc_h_grid_dg
  PUBLIC :: dealloc_h_grid_dx
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
  !! Allokieren/Initialisieren eines neuen (Package-) Datenobjekts <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_h_grid_object_0 &
       ( id )
    !! Identifikationsnummer des Datenobjekts (Skalar) <BR>
    !! id = -1 : Allokieren/Initialisieren fehlgeschlagen
    INTEGER, INTENT(INOUT) :: id
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER :: c_upname='new_h_grid_object_0'
    !! Statusvariable
    INTEGER :: stat
    !! neues Package-Datenobjekt des Typs "t_h_grid_list"
    TYPE (t_h_grid_list) , POINTER :: this
    !! vorangehendes Package-Datenobjekt des Typs "t_h_grid_list"
    TYPE (t_h_grid_list) , POINTER :: prev
    !
    ALLOCATE ( this, STAT=stat )
    !
    IF ( stat /= 0 ) &
         CALL setup_error_act ( all_errors(:), 4001, c_upname, c_modname, stat )
    !
    IF ( no_error( ) ) THEN
       CALL init_h_grid_list_object( this )
       IF ( no_error( ) ) THEN
          NULLIFY ( prev )
          nofobjects     = nofobjects + 1
          id             = get_h_grid_new_id ( )
          this%object%id = id
          IF ( nofobjects == 1 ) THEN ! allererstes Objekt
             first_list_object => this
          ELSE                        ! Anhaengen an die verkettete Liste
             prev              => first_list_object
             DO
                IF ( .NOT. ASSOCIATED( prev%next ) ) EXIT
                prev => prev%next
             END DO
             this%prev => prev
             prev%next => this
          END IF
          NULLIFY ( prev )
       END IF
    END IF
    !
  END SUBROUTINE new_h_grid_object_0
  !
  !! De-Allokieren/De-Initialisieren eines (Package-) Datenobjekts <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_h_grid_object_0 ( id )
    !! Identifikationsnummer des Datenobjekts (Skalar)
    INTEGER , INTENT(IN) :: id
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='kill_h_grid_object_0'
    !! aktuelles (Package-) Datenobjekt vom Typ "t_h_grid_list"
    TYPE (t_h_grid_list) , POINTER :: this
    !! vorangehendes Objekt (previous)
    TYPE (t_h_grid_list) , POINTER :: prev
    !! nachfolgendes Objekt (next)
    TYPE (t_h_grid_list) , POINTER :: next
    !! Hilfsvariable
    LOGICAL :: l_next, l_prev ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ! das zu entfernende Objekt "this" ermitteln
    this => get_h_grid_list_object ( id )
    !
    IF ( no_error( ) ) THEN
       ! Vorgaenger von "this" ermitteln
       IF ( ASSOCIATED( this%prev ) ) THEN
          prev   => this%prev
       ELSE
          NULLIFY ( prev )
       END IF
       ! Nachfolger von "this" ermitteln
       IF ( ASSOCIATED( this%next ) ) THEN
          next => this%next
       ELSE
          NULLIFY ( next )
       END IF
       l_prev = ASSOCIATED( prev )
       l_next = ASSOCIATED( next )
       IF ( l_prev .AND. l_next ) THEN
          ! Vorgaenger und Nachfolger zu "this" vorhanden
          prev%next => next
          next%prev => prev
       ELSE IF ( .NOT. l_prev .AND. .NOT. l_next ) THEN
          ! "this" ist das letzte Objekt
          NULLIFY ( first_list_object )
       ELSE IF ( .NOT. l_prev .AND.       l_next ) THEN
          ! "this" hat keinen Vorgaenger mehr, aber noch einen Nachfolger
          NULLIFY( next%prev )
          first_list_object => next
       ELSE IF (       l_prev .AND. .NOT. l_next ) THEN
          ! "this" hat keinen Nachfolger mehr, aber noch einen Vorgaenger
          NULLIFY( prev%next )
       ENDIF
       ! falls (Arbeits-) Objekt
       IF ( ASSOCIATED( work_object ) ) THEN
          IF ( work_object%id == this%object%id ) NULLIFY( work_object )
       END IF
       ! dynamische Komponenten des Objektes "this" vernichten
       IF ( no_error( ) ) CALL dealloc_h_grid_object ( this%object )
       DEALLOCATE ( this%object, STAT=stat )
       IF ( stat /= 0 ) &
            CALL setup_error_act ( all_errors(:), 5002, c_upname, c_modname, stat )
       IF ( no_error( ) ) nofobjects = nofobjects - 1
       NULLIFY ( prev, next )
       DEALLOCATE ( this, STAT=stat )
       IF ( stat /= 0 ) &
            CALL setup_error_act ( all_errors(:), 5001, c_upname, c_modname, stat )
    END IF
    !
  END SUBROUTINE kill_h_grid_object_0
  !
  !! f&uuml;r eine Identifikationsnummer wird der Zeiger auf ein (Package-) Objekt zur&uuml;ckgegeben <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_list_object_0 ( id ) &
       RESULT( this )
    !! Identifikationsnummer des gew&uuml;nschten (Package-) Datenobjekts
    INTEGER , INTENT(IN) :: id
    !! Zeiger auf vorhandenes (Package-) Objekt mit Identifikationsnummer "id"
    TYPE (t_h_grid_list) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=24) , PARAMETER :: c_upname='get_h_grid_list_object_0'
    !! Hilfs-Zeiger auf (Package-) Objekte
    TYPE (t_h_grid_list) , POINTER :: l_this
    !
    NULLIFY ( this )
    !
    IF ( any_objects ( c_upname ) ) THEN
       NULLIFY ( this, l_this )
       l_this => first_list_object
       DO
          IF ( l_this%object%id == id ) this => l_this
          IF (       ASSOCIATED( this        ) ) EXIT
          IF ( .NOT. ASSOCIATED( l_this%next ) ) EXIT
          l_this => l_this%next
       END DO
       IF ( .NOT. ASSOCIATED( this ) ) THEN
          WRITE(*,*) ' *** keine (Package-) Datenobjekt "t_h_grid_list" fuer ID = ',id
          CALL setup_error_act ( all_errors(:), 4, c_upname, c_modname )
       END IF
    END IF
    !
  END FUNCTION get_h_grid_list_object_0
  !
  !! f&uuml;r einen Namen wird der Zeiger auf ein (Package-) Objekt zur&uuml;ckgegeben <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_list_object_n0 ( name ) &
       RESULT( this )
    !! Name des gew&uuml;nschten (Package-) Datenobjekts
    CHARACTER (LEN=*) , INTENT(IN) :: name ! 
    !! Zeiger auf vorhandenes (Package-) Objekt mit Name "name"
    TYPE (t_h_grid_list) , POINTER :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=25) , PARAMETER :: c_upname='get_h_grid_list_object_0n' ! 
    !! Hilfs-Zeiger auf (Package-) Objekte
    TYPE (t_h_grid_list) , POINTER :: l_this
    !
    NULLIFY ( this )
    !
    IF ( any_objects ( c_upname ) ) THEN
       NULLIFY ( this, l_this )
       l_this => first_list_object
       DO
          IF ( TRIM( l_this%object%name ) == TRIM( name ) ) this => l_this
          IF (       ASSOCIATED( this        ) ) EXIT
          IF ( .NOT. ASSOCIATED( l_this%next ) ) EXIT
          l_this => l_this%next
       END DO
       IF ( .NOT. ASSOCIATED( this ) ) THEN
          WRITE(*,*) ' *** keine (Package-) Datenobjekt "t_h_grid_list" fuer name = '//TRIM(name)
          CALL setup_error_act ( all_errors(:), 4, c_upname, c_modname )
       END IF
    END IF
    !
  END FUNCTION get_h_grid_list_object_n0
  !
  !! Pr&uuml;fe ob ein Arbeitsobjekt vorhanden ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_work_object_0 ( upname ) &
       RESULT( ok )
    !! Name der Subroutine die "ok_work_object" ruft
    CHARACTER (LEN=*) , INTENT(IN) :: upname
    !! Testergebnis
    LOGICAL :: ok
    !
    ok = ASSOCIATED( work_object )
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 5, upname, c_modname )
    !
  END FUNCTION ok_work_object_0
  !
  !! Setzen der Komponente "name" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE setup_name_object_0 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "name"
    CHARACTER (LEN=*) , INTENT(IN) :: val
    !
    this%name = REPEAT( ' ', LEN(this%name) )
    this%name = val
    !
  END SUBROUTINE setup_name_object_0
  !
  !! Setzen der Komponente "file" des (Arbeits-) Objektes <BR>
  !! falls "file%unit" nicht gesetzt (negativ), dann wird 
  !! diese Komponente auf "asc_seq_lun", "bin_seq_lun" oder "bin_dir_lun" gesetzt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_file_object_0 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "file"
    TYPE (t_file)  , INTENT(IN) :: val
    !
    this%file = val
    IF ( get_file_unit ( val ) < 0 ) THEN
       IF ( file_is_sequential( val ) ) THEN
          IF ( file_is_formatted( val ) ) THEN
             CALL set_file_unit ( this%file, asc_seq_lun )
          ELSE
             CALL set_file_unit ( this%file, bin_seq_lun )
          END IF
       ELSE
          CALL set_file_unit ( this%file, bin_dir_lun )
       END IF
    END IF
    !
  END SUBROUTINE setup_file_object_0
  !
  !! Setzen der Feld-Komponente "xy" des (Arbeits-) Objektes <BR>
  !! Zugleich wird die Anzahl Gitterknoten gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_xy_object_2 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "xy"
    REAL (KIND=Double) , INTENT(IN) :: val(:,:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='setup_xy_object_2'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( this%nv < 1 ) this%nv = SIZE ( val, DIM=1 )
    !
    IF ( this%nv /= SIZE ( val, DIM=1 ) ) THEN
       CALL setup_error_act ( all_errors(:), 3510, c_upname, c_modname )
       WRITE(ctmp,'(I10)') SIZE ( val, DIM=1 )
       CALL setup_error_act ( '<SIZE(..)>', ctmp )
       WRITE(ctmp,'(I10)') this%nv
       CALL setup_error_act ( '<...nv...>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_xy ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_xy   ( this, SIZE(val,DIM=1), SIZE(val,DIM=2) )
    IF ( no_error( ) ) this%xy(:,:) = val(:,:)
    !
  END SUBROUTINE setup_xy_object_2
  !
  !! Setzen der Feld-Komponente "nen" des (Arbeits-) Objektes <BR>
  !! Zugleich wird die Anzahl Gitterpolygone gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_nen_object_2 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "nen"
    INTEGER , INTENT(IN) :: val(:,:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_nen_object_2'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( this%ne < 1 ) this%ne = SIZE ( val, DIM=1 )
    !
    IF ( this%ne /= SIZE ( val, DIM=1 ) ) THEN
       CALL setup_error_act ( all_errors(:), 3520, c_upname, c_modname )
       WRITE(ctmp,'(I10)') SIZE ( val, DIM=1 )
       CALL setup_error_act ( '<SIZE(..)>', ctmp )
       WRITE(ctmp,'(I10)') this%ne
       CALL setup_error_act ( '<...ne...>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_nen ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_nen   ( this, SIZE(val,DIM=1), SIZE(val,DIM=2) )
    IF ( no_error( ) ) this%nen(:,:) = val(:,:)
    !
  END SUBROUTINE setup_nen_object_2
  !
  !! Setzen der Feld-Komponente "irand" des (Arbeits-) Objektes <BR>
  !! Zugleich wird die Anzahl Gitterpolygone gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_irand_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "irand"
    INTEGER , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='setup_irand_object_1'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( this%ne < 1 ) this%ne = SIZE ( val )
    !
    IF ( this%ne /= SIZE ( val ) ) THEN
       CALL setup_error_act ( all_errors(:), 3530, c_upname, c_modname )
       WRITE(ctmp,'(I10)') SIZE ( val )
       CALL setup_error_act ( '<SIZE(..)>', ctmp )
       WRITE(ctmp,'(I10)') this%ne
       CALL setup_error_act ( '<...ne...>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_irand ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_irand   ( this, SIZE(val) )
    IF ( no_error( ) ) this%irand(:) = val(:)
    !
  END SUBROUTINE setup_irand_object_1
  !
  !! Setzen der Feld-Komponente "ks" des (Arbeits-) Objektes <BR>
  !! Zugleich wird die Anzahl Gitterpolygone gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_ks_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "ks"
    INTEGER , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='setup_ks_object_1'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( this%ne < 1 ) this%ne = SIZE ( val )
    !
    IF ( this%ne /= SIZE ( val ) ) THEN
       CALL setup_error_act ( all_errors(:), 3540, c_upname, c_modname )
       WRITE(ctmp,'(I10)') SIZE ( val )
       CALL setup_error_act ( '<SIZE(..)>', ctmp )
       WRITE(ctmp,'(I10)') this%ne
       CALL setup_error_act ( '<...ne...>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_ks ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_ks   ( this, SIZE(val) )
    IF ( no_error( ) ) this%ks(:) = val(:)
    !
  END SUBROUTINE setup_ks_object_1
  !
  !! Setzen der Feld-Komponente "hv" des (Arbeits-) Objektes <BR>
  !! Zugleich wird die Anzahl Gitterknoten gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_hv_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "hv"
    REAL (KIND=Double) , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='setup_hv_object_1'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( this%nv < 1 ) this%nv = SIZE ( val )
    IF ( this%nv /= SIZE ( val ) ) THEN
       CALL setup_error_act ( all_errors(:), 3550, c_upname, c_modname )
       CALL setup_error_act ( '<component-name>', 'hv(:)' )
       WRITE(ctmp,'(I10)') SIZE ( val ) ; CALL setup_error_act ( '<SIZE(..)>', ctmp )
       WRITE(ctmp,'(I10)') this%nv      ; CALL setup_error_act ( '<...nv...>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_hv ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_hv   ( this, SIZE(val) )
    IF ( no_error( ) ) this%hv(:) = val(:)
    !
  END SUBROUTINE setup_hv_object_1
  !
  !! Setzen der Skalar-Komponente "nrand" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_nrand_object_0 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "nrand"
    INTEGER , INTENT(IN) :: val ! 
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_nrand ( this )
    IF ( no_error( ) ) CALL alloc_h_grid_nrand   ( this )
    IF ( no_error( ) ) this%nrand = val
    !
  END SUBROUTINE setup_nrand_object_0
  !
  !! Setzen der Skalar-Komponente "nptfr" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_nptfr_object_0 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "nptfr"
    INTEGER , INTENT(IN) :: val ! 
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_nptfr ( this )
    IF ( no_error( ) ) CALL alloc_h_grid_nptfr   ( this )
    IF ( no_error( ) ) this%nptfr = val
    !
  END SUBROUTINE setup_nptfr_object_0
  !
  !! Setzen der Skalar-Komponente "nptir" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_nptir_object_0 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "nptir"
    INTEGER , INTENT(IN) :: val ! 
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_nptir ( this )
    IF ( no_error( ) ) CALL alloc_h_grid_nptir   ( this )
    IF ( no_error( ) ) this%nptir = val
    !
  END SUBROUTINE setup_nptir_object_0
  !
  !! Setzen der Skalar-Komponente "time" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_time_object_0 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "time"
    TYPE (t_datetime)  , INTENT(IN)    :: val ! 
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_time ( this )
    IF ( no_error( ) ) CALL alloc_h_grid_time   ( this )
    IF ( no_error( ) ) this%time = val
    !
  END SUBROUTINE setup_time_object_0
  !
  !! Setzen der Skalar-Komponente "nbc" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_nbc_object_0 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "nbc"
    INTEGER            , INTENT(IN)    :: val ! 
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_nbc ( this )
    IF ( no_error( ) ) CALL alloc_h_grid_nbc   ( this )
    IF ( no_error( ) ) this%nbc = val
    !
  END SUBROUTINE setup_nbc_object_0
  !
  !! Setzen der Skalar-Komponente "nr" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_nr_object_0 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "nr"
    INTEGER            , INTENT(IN)    :: val ! 
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_nr ( this )
    IF ( no_error( ) ) CALL alloc_h_grid_nr   ( this )
    IF ( no_error( ) ) this%nr = val
    !
  END SUBROUTINE setup_nr_object_0
  !
  !! Setzen der Skalar-Komponente "nsi" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_nsi_object_0 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "nsi"
    INTEGER            , INTENT(IN)    :: val ! 
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_nsi ( this )
    IF ( no_error( ) ) CALL alloc_h_grid_nsi   ( this )
    IF ( no_error( ) ) this%nsi = val
    !
  END SUBROUTINE setup_nsi_object_0
  !
  !! Setzen der Skalar-Komponente "nsf" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_nsf_object_0 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "nsf"
    INTEGER            , INTENT(IN)    :: val ! 
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_nsf ( this )
    IF ( no_error( ) ) CALL alloc_h_grid_nsf   ( this )
    IF ( no_error( ) ) this%nsf = val
    !
  END SUBROUTINE setup_nsf_object_0
  !
  !! Setzen der Skalar-Komponente "ncsize" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_ncsize_object_0 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "ncsize"
    INTEGER            , INTENT(IN)    :: val ! 
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_ncsize ( this )
    IF ( no_error( ) ) CALL alloc_h_grid_ncsize   ( this )
    IF ( no_error( ) ) this%ncsize = val
    !
  END SUBROUTINE setup_ncsize_object_0
  !
  !! Setzen der Skalar-Komponente "hland" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_hland_object_0 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "hland"
    REAL (KIND=Double) , INTENT(IN)    :: val ! 
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_hland ( this )
    IF ( no_error( ) ) CALL alloc_h_grid_hland   ( this )
    IF ( no_error( ) ) this%hland = val
    !
  END SUBROUTINE setup_hland_object_0
  !
  !! Setzen der Skalar-Komponente "angle" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_angle_object_0 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "angle"
    REAL (KIND=Double) , INTENT(IN)    :: val ! 
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_angle ( this )
    IF ( no_error( ) ) CALL alloc_h_grid_angle   ( this )
    IF ( no_error( ) ) this%angle = val
    !
  END SUBROUTINE setup_angle_object_0
  !
  !! Setzen der Feld-Komponente "text" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_text_object_0 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "text"
    CHARACTER (LEN=80) , INTENT(IN)    :: val ! 
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_text ( this    )
    IF ( no_error( ) ) CALL alloc_h_grid_text   ( this, 1 )
    IF ( no_error( ) ) this%text(1) = val
    !
  END SUBROUTINE setup_text_object_0
  !
  !! Setzen der Feld-Komponente "text" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_text_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "text"
    CHARACTER (LEN=80) , INTENT(IN)    :: val(:) ! 
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_text ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_text   ( this, SIZE(val) )
    IF ( no_error( ) ) this%text(:) = val(:)
    !
  END SUBROUTINE setup_text_object_1
  !
  !! Setzen der Feld-Komponente "jb" des (Arbeits-) Objektes <BR>
  !! Zugleich wird die Anzahl Gitterkanten gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_jb_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "jb"
    INTEGER , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='setup_jb_object_1'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( this%ns < 1 ) this%ns = SIZE ( val )
    !
    IF ( this%ns /= SIZE ( val ) ) THEN
       CALL setup_error_act ( all_errors(:), 3650, c_upname, c_modname )
       WRITE(ctmp,'(I10)') SIZE ( val )
       CALL setup_error_act ( '<SIZE(..)>', ctmp )
       WRITE(ctmp,'(I10)') this%ns
       CALL setup_error_act ( '<...ns...>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_jb ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_jb   ( this, SIZE(val) )
    IF ( no_error( ) ) this%jb(:) = val(:)
    !
  END SUBROUTINE setup_jb_object_1
  !
  !! Setzen der Feld-Komponente "jt" des (Arbeits-) Objektes <BR>
  !! Zugleich wird die Anzahl Gitterkanten gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_jt_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "jt"
    INTEGER , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='setup_jt_object_1'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( this%ns < 1 ) this%ns = SIZE ( val )
    !
    IF ( this%ns /= SIZE ( val ) ) THEN
       CALL setup_error_act ( all_errors(:), 3660, c_upname, c_modname )
       WRITE(ctmp,'(I10)') SIZE ( val )
       CALL setup_error_act ( '<SIZE(..)>', ctmp )
       WRITE(ctmp,'(I10)') this%ns
       CALL setup_error_act ( '<...ns...>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_jt ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_jt   ( this, SIZE(val) )
    IF ( no_error( ) ) this%jt(:) = val(:)
    !
  END SUBROUTINE setup_jt_object_1
  !
  !! Setzen der Feld-Komponente "is" des (Arbeits-) Objektes <BR>
  !! Zugleich wird die Anzahl Gitterpolygone gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_is_object_2 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "is"
    INTEGER , INTENT(IN) :: val(:,:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='setup_is_object_2'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( this%ne < 1 ) this%ne = SIZE ( val, DIM=1 )
    !
    IF ( this%ne /= SIZE ( val, DIM=1 ) ) THEN
       CALL setup_error_act ( all_errors(:), 3670, c_upname, c_modname )
       WRITE(ctmp,'(I10)') SIZE ( val, DIM=1 )
       CALL setup_error_act ( '<SIZE(..)>', ctmp )
       WRITE(ctmp,'(I10)') this%ne
       CALL setup_error_act ( '<...ne...>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_is ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_is   ( this, SIZE(val,DIM=1), SIZE(val,DIM=2) )
    IF ( no_error( ) ) this%is(:,:) = val(:,:)
    !
  END SUBROUTINE setup_is_object_2
  !
  !! Setzen der Feld-Komponente "je" des (Arbeits-) Objektes <BR>
  !! Zugleich wird die Anzahl Gitterkanten gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_je_object_2 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "je"
    INTEGER , INTENT(IN) :: val(:,:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='setup_je_object_2'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( this%ns < 1 ) this%ns = SIZE ( val, DIM=1 )
    !
    IF ( this%ns /= SIZE ( val, DIM=1 ) ) THEN
       CALL setup_error_act ( all_errors(:), 3680, c_upname, c_modname )
       WRITE(ctmp,'(I10)') SIZE ( val, DIM=1 )
       CALL setup_error_act ( '<SIZE(..)>', ctmp )
       WRITE(ctmp,'(I10)') this%ns
       CALL setup_error_act ( '<...ns...>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_je ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_je   ( this, SIZE(val,DIM=1), SIZE(val,DIM=2) )
    IF ( no_error( ) ) this%je(:,:) = val(:,:)
    !
  END SUBROUTINE setup_je_object_2
  !
  !! Setzen der Feld-Komponente "ie" des (Arbeits-) Objektes <BR>
  !! Zugleich wird die Anzahl Gitterpolygone gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_ie_object_2 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "ie"
    INTEGER , INTENT(IN) :: val(:,:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='setup_ie_object_2'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( this%ne < 1 ) this%ne = SIZE ( val, DIM=1 )
    !
    IF ( this%ne /= SIZE ( val, DIM=1 ) ) THEN
       CALL setup_error_act ( all_errors(:), 3690, c_upname, c_modname )
       WRITE(ctmp,'(I10)') SIZE ( val, DIM=1 )
       CALL setup_error_act ( '<SIZE(..)>', ctmp )
       WRITE(ctmp,'(I10)') this%ne
       CALL setup_error_act ( '<...ne...>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_ie ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_ie   ( this, SIZE(val,DIM=1), SIZE(val,DIM=2) )
    IF ( no_error( ) ) this%ie(:,:) = val(:,:)
    !
  END SUBROUTINE setup_ie_object_2
  !
  !! Setzen der Feld-Komponente "xs" des (Arbeits-) Objektes <BR>
  !! Zugleich wird die Anzahl Gitterkanten gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_xs_object_2 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "xs"
    REAL (KIND=Double) , INTENT(IN)    :: val(:,:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='setup_xs_object_2'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( this%ns < 1 ) this%ns = SIZE ( val, DIM=1 )
    !
    IF ( this%ns /= SIZE ( val, DIM=1 ) ) THEN
       CALL setup_error_act ( all_errors(:), 3710, c_upname, c_modname )
       WRITE(ctmp,'(I10)') SIZE ( val, DIM=1 )
       CALL setup_error_act ( '<SIZE(..)>', ctmp )
       WRITE(ctmp,'(I10)') this%ns
       CALL setup_error_act ( '<...ns...>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_xs ( this                                   )
    IF ( no_error( ) ) CALL alloc_h_grid_xs   ( this, SIZE(val,DIM=1), SIZE(val,DIM=2) )
    IF ( no_error( ) ) this%xs(:,:) = val(:,:)
    !
  END SUBROUTINE setup_xs_object_2
  !
  !! Setzen der Feld-Komponente "xc" des (Arbeits-) Objektes <BR>
  !! Zugleich wird die Anzahl Gitterpolygone gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_xc_object_2 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "xc"
    REAL (KIND=Double) , INTENT(IN)    :: val(:,:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='setup_xc_object_2'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( this%ne < 1 ) this%ne = SIZE ( val, DIM=1 )
    !
    IF ( this%ne /= SIZE ( val, DIM=1 ) ) THEN
       CALL setup_error_act ( all_errors(:), 3700, c_upname, c_modname )
       WRITE(ctmp,'(I10)') SIZE ( val, DIM=1 )
       CALL setup_error_act ( '<SIZE(..)>', ctmp )
       WRITE(ctmp,'(I10)') this%ne
       CALL setup_error_act ( '<...ne...>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_xc ( this                                   )
    IF ( no_error( ) ) CALL alloc_h_grid_xc   ( this, SIZE(val,DIM=1), SIZE(val,DIM=2) )
    IF ( no_error( ) ) this%xc(:,:) = val(:,:)
    !
  END SUBROUTINE setup_xc_object_2
  !
  !! Setzen der Feld-Komponente "xg" des (Arbeits-) Objektes <BR>
  !! Zugleich wird die Anzahl Gitterpolygone gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_xg_object_2 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "xg"
    REAL (KIND=Double) , INTENT(IN)    :: val(:,:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='setup_xg_object_2'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( this%ne < 1 ) this%ne = SIZE ( val, DIM=1 )
    !
    IF ( this%ne /= SIZE ( val, DIM=1 ) ) THEN
       CALL setup_error_act ( all_errors(:), 3700, c_upname, c_modname )
       WRITE(ctmp,'(I10)') SIZE ( val, DIM=1 )
       CALL setup_error_act ( '<SIZE(..)>', ctmp )
       WRITE(ctmp,'(I10)') this%ne
       CALL setup_error_act ( '<...ne...>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_xg ( this                                   )
    IF ( no_error( ) ) CALL alloc_h_grid_xg   ( this, SIZE(val,DIM=1), SIZE(val,DIM=2) )
    IF ( no_error( ) ) this%xg(:,:) = val(:,:)
    !
  END SUBROUTINE setup_xg_object_2
  !
  !! Setzen der Feld-Komponente "dx" des (Arbeits-) Objektes <BR>
  !! Zugleich wird die Anzahl Gitterkanten gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_dx_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "dx"
    REAL (KIND=Double) , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='setup_dx_object_1'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( this%ns < 1 ) this%ns = SIZE ( val )
    !
    IF ( this%ns /= SIZE ( val ) ) THEN
       CALL setup_error_act ( all_errors(:), 3720, c_upname, c_modname )
       WRITE(ctmp,'(I10)') SIZE ( val )
       CALL setup_error_act ( '<SIZE(..)>', ctmp )
       WRITE(ctmp,'(I10)') this%ns
       CALL setup_error_act ( '<...ns...>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_dx ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_dx   ( this, SIZE(val) )
    IF ( no_error( ) ) this%dx(:) = val(:)
    !
  END SUBROUTINE setup_dx_object_1
  !
  !! Setzen der Feld-Komponente "dy" des (Arbeits-) Objektes <BR>
  !! Zugleich wird die Anzahl Gitterkanten gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_dy_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "dy"
    REAL (KIND=Double) , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='setup_dy_object_1'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( this%ns < 1 ) this%ns = SIZE ( val )
    !
    IF ( this%ns /= SIZE ( val ) ) THEN
       CALL setup_error_act ( all_errors(:), 3730, c_upname, c_modname )
       WRITE(ctmp,'(I10)') SIZE ( val )
       CALL setup_error_act ( '<SIZE(..)>', ctmp )
       WRITE(ctmp,'(I10)') this%ns
       CALL setup_error_act ( '<...ns...>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_dy ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_dy   ( this, SIZE(val) )
    IF ( no_error( ) ) this%dy(:) = val(:)
    !
  END SUBROUTINE setup_dy_object_1
  !
  !! Setzen der Feld-Komponente "dg" des (Arbeits-) Objektes <BR>
  !! Zugleich wird die Anzahl Gitterkanten gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_dg_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "dg"
    REAL (KIND=Double) , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='setup_dg_object_1'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( this%ns < 1 ) this%ns = SIZE ( val )
    !
    IF ( this%ns /= SIZE ( val ) ) THEN
       CALL setup_error_act ( all_errors(:), 3840, c_upname, c_modname )
       WRITE(ctmp,'(I10)') SIZE ( val )
       CALL setup_error_act ( '<SIZE(..)>', ctmp )
       WRITE(ctmp,'(I10)') this%ns
       CALL setup_error_act ( '<...ns...>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_dg ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_dg   ( this, SIZE(val) )
    IF ( no_error( ) ) this%dg(:) = val(:)
    !
  END SUBROUTINE setup_dg_object_1
  !
  !! Setzen der Feld-Komponente "aa" des (Arbeits-) Objektes <BR>
  !! Zugleich wird die Anzahl Gitterpolygone gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_aa_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "aa"
    REAL (KIND=Double) , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='setup_aa_object_1'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( this%ne < 1 ) this%ne = SIZE ( val )
    !
    IF ( this%ne /= SIZE ( val ) ) THEN
       CALL setup_error_act ( all_errors(:), 3740, c_upname, c_modname )
       WRITE(ctmp,'(I10)') SIZE ( val )
       CALL setup_error_act ( '<SIZE(..)>', ctmp )
       WRITE(ctmp,'(I10)') this%ne
       CALL setup_error_act ( '<...ne...>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_aa ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_aa   ( this, SIZE(val) )
    IF ( no_error( ) ) this%aa(:) = val(:)
    !
  END SUBROUTINE setup_aa_object_1
  !
  !! Setzen der Feld-Komponente "hu" des (Arbeits-) Objektes <BR>
  !! Zugleich wird die Anzahl Gitterkanten gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_hu_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "hu"
    REAL (KIND=Double) , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='setup_hu_object_1'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( this%ns < 1 ) this%ns = SIZE ( val )
    IF ( this%ns /= SIZE ( val ) ) THEN
       CALL setup_error_act ( all_errors(:), 3750, c_upname, c_modname )
       CALL setup_error_act ( '<component-name>', 'hu(:)' )
       WRITE(ctmp,'(I10)') SIZE ( val ) ; CALL setup_error_act ( '<SIZE(..)>', ctmp )
       WRITE(ctmp,'(I10)') this%ns      ; CALL setup_error_act ( '<...ns...>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_hu ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_hu   ( this, SIZE(val) )
    IF ( no_error( ) ) this%hu(:) = val(:)
    !
  END SUBROUTINE setup_hu_object_1
  !
  !! Setzen der Feld-Komponente "hw" des (Arbeits-) Objektes <BR>
  !! Zugleich wird die Anzahl Gitterpolygone gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_hw_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "hw"
    REAL (KIND=Double) , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='setup_hw_object_1'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( this%ne < 1 ) this%ne = SIZE ( val )
    IF ( this%ne /= SIZE ( val ) ) THEN
       CALL setup_error_act ( all_errors(:), 3760, c_upname, c_modname )
       CALL setup_error_act ( '<component-name>', 'hw(:)' )
       WRITE(ctmp,'(I10)') SIZE ( val ) ; CALL setup_error_act ( '<SIZE(..)>', ctmp )
       WRITE(ctmp,'(I10)') this%ne      ; CALL setup_error_act ( '<...ne...>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_hw ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_hw   ( this, SIZE(val) )
    IF ( no_error( ) ) this%hw(:) = val(:)
    !
  END SUBROUTINE setup_hw_object_1
  !
  !! Setzen der Feld-Komponente "ipobo" des (Arbeits-) Objektes <BR>
  !! Zugleich wird die Anzahl Gitterknoten gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_ipobo_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "ipobo"
    INTEGER            , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='setup_ipobo_object_1'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( this%nv < 1 ) this%nv = SIZE ( val )
    !
    IF ( this%nv /= SIZE ( val ) ) THEN
       CALL setup_error_act ( all_errors(:), 3780, c_upname, c_modname )
       WRITE(ctmp,'(I10)') SIZE ( val )
       CALL setup_error_act ( '<SIZE(..)>', ctmp )
       WRITE(ctmp,'(I10)') this%nv
       CALL setup_error_act ( '<...nv...>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_ipobo ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_ipobo   ( this, SIZE(val) )
    IF ( no_error( ) ) this%ipobo(:) = val(:)
    !
  END SUBROUTINE setup_ipobo_object_1
  !
  !! Setzen der Feld-Komponente "knolg" des (Arbeits-) Objektes <BR>
  !! Zugleich wird die Anzahl Gitterknoten gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_knolg_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "knolg"
    INTEGER            , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='setup_knolg_object_1'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( this%nv < 1 ) this%nv = SIZE ( val )
    !
    IF ( this%nv /= SIZE ( val ) ) THEN
       CALL setup_error_act ( all_errors(:), 3785, c_upname, c_modname )
       WRITE(ctmp,'(I10)') SIZE ( val )
       CALL setup_error_act ( '<SIZE(..)>', ctmp )
       WRITE(ctmp,'(I10)') this%nv
       CALL setup_error_act ( '<...nv...>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_knolg ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_knolg   ( this, SIZE(val) )
    IF ( no_error( ) ) this%knolg(:) = val(:)
    !
  END SUBROUTINE setup_knolg_object_1
  !
  !! Setzen der Feld-Komponente "quant" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_quant_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid)    , POINTER    :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "quant"
    TYPE (t_omi_quant) , INTENT(IN) :: val(:) ! 
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_quant ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_quant   ( this, SIZE(val) )
    IF ( no_error( ) ) CALL copy_omi_quant ( this%quant, val )
    !
  END SUBROUTINE setup_quant_object_1
  !
  !! Setzen der Feld-Komponente "xyz" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_xyz_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid)  , POINTER    :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "xyz"
    TYPE (t_omi_xyz) , INTENT(IN) :: val(:) ! 
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_xyz ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_xyz   ( this, SIZE(val) )
    IF ( no_error( ) ) CALL copy_omi_xyz ( this%xyz, val )
    !
  END SUBROUTINE setup_xyz_object_1
  !
  !! Setzen der Feld-Komponente "ele" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_ele_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid)  , POINTER    :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "ele"
    TYPE (t_omi_ele) , INTENT(IN) :: val(:) ! 
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_ele ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_ele   ( this, SIZE(val) )
    IF ( no_error( ) ) CALL copy_omi_ele ( this%ele, val )
    !
  END SUBROUTINE setup_ele_object_1
  !
  !! Setzen der Feld-Komponente "ind" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_ind_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid)  , POINTER    :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "ind"
    TYPE (t_omi_ind) , INTENT(IN) :: val(:) ! 
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_ind ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_ind   ( this, SIZE(val) )
    IF ( no_error( ) ) CALL copy_omi_ind ( this%ind, val )
    !
  END SUBROUTINE setup_ind_object_1
  !
  !! Setzen der Feld-Komponente "dope" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_dope_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid)   , POINTER    :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "dope"
    TYPE (t_omi_dope) , INTENT(IN) :: val(:) ! 
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_dope ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_dope   ( this, SIZE(val) )
    IF ( no_error( ) ) CALL copy_omi_dope ( this%dope, val )
    !
  END SUBROUTINE setup_dope_object_1
  !
  !! Setzen der Feld-Komponente "exch" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_exch_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid)   , POINTER    :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "exch"
    TYPE (t_omi_exch) , INTENT(IN) :: val(:) ! 
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_exch ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_exch   ( this, SIZE(val) )
    IF ( no_error( ) ) CALL copy_omi_exch ( this%exch, val )
    !
  END SUBROUTINE setup_exch_object_1
  !
  !! Setzen der Komponente "span" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_span_object_0 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid)   , POINTER    :: this
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "span"
    TYPE (t_omi_span) , INTENT(IN) :: val ! 
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_span ( this )
    IF ( no_error( ) ) CALL alloc_h_grid_span   ( this )
    IF ( no_error( ) ) this%span = val
    !
  END SUBROUTINE setup_span_object_0
  !
  !! Setzen der Komponente "stamp" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_stamp_object_0 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid)    , POINTER    :: this
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "stamp"
    TYPE (t_omi_stamp) , INTENT(IN) :: val ! 
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_stamp ( this )
    IF ( no_error( ) ) CALL alloc_h_grid_stamp   ( this )
    IF ( no_error( ) ) this%stamp = val
    !
  END SUBROUTINE setup_stamp_object_0
  !
  !! Setzen der Komponente "space" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_space_object_0 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid)    , POINTER    :: this
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "space"
    TYPE (t_omi_space) , INTENT(IN) :: val ! 
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_space ( this )
    IF ( no_error( ) ) CALL alloc_h_grid_space   ( this )
    IF ( no_error( ) ) this%space = val
    !
  END SUBROUTINE setup_space_object_0
  !
  !! Setzen der Skalar-Komponente "dxmin" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_dxmin_object_0 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "dxmin"
    REAL (KIND=Double) , INTENT(IN) :: val ! 
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_dxmin ( this )
    IF ( no_error( ) ) CALL alloc_h_grid_dxmin   ( this )
    IF ( no_error( ) ) this%dxmin = MAX( 0.0_Double, val )
    !
  END SUBROUTINE setup_dxmin_object_0
  !
  !! Setzen der Feld-Komponente "b_ms(:)" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_b_ms_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "b_ms(:)"
    INTEGER , INTENT(IN) :: val(:) ! 
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_b_ms ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_b_ms   ( this, SIZE(val) )
    IF ( no_error( ) ) this%b_ms(:) = val(:)
    !
  END SUBROUTINE setup_b_ms_object_1
  !
  !! Setzen der Feld-Komponente "b_ss(:)" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_b_ss_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "b_ms(:)"
    INTEGER , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER :: c_upname='setup_b_ss_object_1'
    !! Hilfsvariable
    CHARACTER (LEN=10) :: ch ! 
    !
    IF ( ASSOCIATED(this%b_ms) ) THEN
       IF ( SIZE(this%b_ms) /= SIZE(val) ) THEN
          CALL setup_error_act ( all_errors(:), 3790, c_upname, c_modname )
          CALL setup_error_act ( '<array>', 'b_ss(:)' )
          WRITE(ch,'(I10)') SIZE(val)       ; CALL setup_error_act ( '<actual>', ch )
          WRITE(ch,'(I10)') SIZE(this%b_ms) ; CALL setup_error_act ( '<required>', ch )
       END IF
    END IF
    IF ( no_error( ) ) CALL dealloc_h_grid_b_ss ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_b_ss   ( this, SIZE(val) )
    IF ( no_error( ) ) this%b_ss(:) = val(:)
    !
  END SUBROUTINE setup_b_ss_object_1
  !
  !! Setzen der Feld-Komponente "b_s(:)" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_b_s_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "b_ms(:)"
    INTEGER , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_b_s_object_1'
    !! Hilfsvariable
    CHARACTER (LEN=10) :: ch ! 
    !
    IF ( ASSOCIATED(this%b_ms) ) THEN
       IF ( SIZE(this%b_ms) /= SIZE(val) ) THEN
          CALL setup_error_act ( all_errors(:), 3790, c_upname, c_modname )
          CALL setup_error_act ( '<array>', 'b_s(:)' )
          WRITE(ch,'(I10)') SIZE(val)       ; CALL setup_error_act ( '<actual>', ch )
          WRITE(ch,'(I10)') SIZE(this%b_ms) ; CALL setup_error_act ( '<required>', ch )
       END IF
    END IF
    IF ( no_error( ) ) CALL dealloc_h_grid_b_s ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_b_s   ( this, SIZE(val) )
    IF ( no_error( ) ) this%b_s(:) = val(:)
    !
  END SUBROUTINE setup_b_s_object_1
  !
  !! Setzen der Feld-Komponente "b_v(:)" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_b_v_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "b_ms(:)"
    INTEGER , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_b_v_object_1'
    !! Hilfsvariable
    CHARACTER (LEN=10) :: ch ! 
    !
    IF ( ASSOCIATED(this%b_ms) ) THEN
       IF ( SIZE(this%b_ms) /= SIZE(val) ) THEN
          CALL setup_error_act ( all_errors(:), 3790, c_upname, c_modname )
          CALL setup_error_act ( '<array>', 'b_v(:)' )
          WRITE(ch,'(I10)') SIZE(val)       ; CALL setup_error_act ( '<actual>', ch )
          WRITE(ch,'(I10)') SIZE(this%b_ms) ; CALL setup_error_act ( '<required>', ch )
       END IF
    END IF
    IF ( no_error( ) ) CALL dealloc_h_grid_b_v ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_b_v   ( this, SIZE(val) )
    IF ( no_error( ) ) this%b_v(:) = val(:)
    !
  END SUBROUTINE setup_b_v_object_1
  !
  !! Setzen der Feld-Komponente "b_t(:)" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_b_t_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "b_ms(:)"
    INTEGER , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_b_t_object_1'
    !! Hilfsvariable
    CHARACTER (LEN=10) :: ch ! 
    !
    IF ( ASSOCIATED(this%b_ms) ) THEN
       IF ( SIZE(this%b_ms) /= SIZE(val) ) THEN
          CALL setup_error_act ( all_errors(:), 3790, c_upname, c_modname )
          CALL setup_error_act ( '<array>', 'b_t(:)' )
          WRITE(ch,'(I10)') SIZE(val)       ; CALL setup_error_act ( '<actual>', ch )
          WRITE(ch,'(I10)') SIZE(this%b_ms) ; CALL setup_error_act ( '<required>', ch )
       END IF
    END IF
    IF ( no_error( ) ) CALL dealloc_h_grid_b_t ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_b_t   ( this, SIZE(val) )
    IF ( no_error( ) ) this%b_t(:) = val(:)
    !
  END SUBROUTINE setup_b_t_object_1
  !
  !! Setzen der Skalar-Komponente "m" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_m_object_0 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER    :: this ! 
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "m"
    INTEGER         , INTENT(IN) :: val  ! 
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_m ( this )
    IF ( no_error( ) ) CALL alloc_h_grid_m   ( this )
    IF ( no_error( ) ) this%m = MAX( 0, val )
    !
  END SUBROUTINE setup_m_object_0
  !
  !! Setzen der Skalar-Komponente "n" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_n_object_0 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER    :: this ! 
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "n"
    INTEGER         , INTENT(IN) :: val  ! 
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_n ( this )
    IF ( no_error( ) ) CALL alloc_h_grid_n   ( this )
    IF ( no_error( ) ) this%n = MAX( 0, val )
    !
  END SUBROUTINE setup_n_object_0
  !
  !! Setzen der Feld-Komponente "enc(:,:)" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_enc_object_2 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "enc(:,:)"
    INTEGER , INTENT(IN)      :: val(:,:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_enc_object_2'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( SIZE(val,2) /= 2 ) THEN
       CALL setup_error_act ( all_errors(:), 3800, c_upname, c_modname )
       WRITE(ctmp,'(I10)') SIZE(val,2)
       CALL setup_error_act ( '<dimension>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_enc ( this                           )
    IF ( no_error( ) ) CALL alloc_h_grid_enc   ( this, SIZE(val,1), SIZE(val,2) )
    IF ( no_error( ) ) this%enc(:,:) = val(:,:)
    !
  END SUBROUTINE setup_enc_object_2
  !
  !! Setzen der Feld-Komponente "dry(:,:)" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_dry_object_2 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "dry(:,:)"
    INTEGER , INTENT(IN)      :: val(:,:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_dry_object_2'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( SIZE(val,2) /= c_max_d3d_dry ) THEN
       CALL setup_error_act ( all_errors(:), 3810, c_upname, c_modname )
       WRITE(ctmp,'(I10)') SIZE(val,2)
       CALL setup_error_act ( '<dimension>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_dry ( this                           )
    IF ( no_error( ) ) CALL alloc_h_grid_dry   ( this, SIZE(val,1), SIZE(val,2) )
    IF ( no_error( ) ) this%dry(:,:) = val(:,:)
    !
  END SUBROUTINE setup_dry_object_2
  !
  !! Setzen der Feld-Komponente "isbnd(:,:)" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_isbnd_object_2 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "isbnd(:,:)"
    INTEGER , INTENT(IN)      :: val(:,:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='setup_isbnd_object_2'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( SIZE(val,2) /= 4 ) THEN
       CALL setup_error_act ( all_errors(:), 3820, c_upname, c_modname )
       WRITE(ctmp,'(I10)') SIZE(val,2)
       CALL setup_error_act ( '<dimension>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_isbnd ( this                           )
    IF ( no_error( ) ) CALL alloc_h_grid_isbnd   ( this, SIZE(val,1), SIZE(val,2) )
    IF ( no_error( ) ) this%isbnd(:,:) = val(:,:)
    !
  END SUBROUTINE setup_isbnd_object_2
  !
  !! Setzen der Feld-Komponente "isdam(:,:,:)" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_isdam_object_3 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "isdam(:,:,:)"
    INTEGER , INTENT(IN)      :: val(:,:,:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='setup_isdam_object_3'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( SIZE(val,2) /= 4 .OR. SIZE(val,3) /= 2 ) THEN
       CALL setup_error_act ( all_errors(:), 3830, c_upname, c_modname )
       WRITE(ctmp,'(I10)') SIZE(val,2) ; CALL setup_error_act ( '<dimension2>', ctmp )
       WRITE(ctmp,'(I10)') SIZE(val,3) ; CALL setup_error_act ( '<dimension3>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_isdam ( this                                        )
    IF ( no_error( ) ) CALL alloc_h_grid_isdam   ( this, SIZE(val,1), SIZE(val,2), SIZE(val,3) )
    IF ( no_error( ) ) this%isdam(:,:,:) = val(:,:,:)
    !
  END SUBROUTINE setup_isdam_object_3
  !
  !! Setzen der Feld-Komponente "bnd(:)" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_bnd_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid)     , POINTER    :: this   ! 
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "bnd(:)"
    TYPE (t_d3d_openbc) , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_bnd_object_1'
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_bnd ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_bnd   ( this, SIZE(val) )
    IF ( no_error( ) ) this%bnd(:) = val(:)
    !
  END SUBROUTINE setup_bnd_object_1
  !
  !! Setzen der Feld-Komponente "thd(:)" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_thd_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid)  , POINTER    :: this   ! 
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "thd(:)"
    TYPE (t_d3d_thd) , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_thd_object_1'
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_thd ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_thd   ( this, SIZE(val) )
    IF ( no_error( ) ) this%thd(:) = val(:)
    !
  END SUBROUTINE setup_thd_object_1
  !
  !! Setzen der Feld-Komponente "lwl(:)" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_lwl_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid)   , POINTER    :: this   ! 
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "lwl(:)"
    TYPE (t_d3d_weir) , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_lwl_object_1'
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_lwl ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_lwl   ( this, SIZE(val) )
    IF ( no_error( ) ) this%lwl(:) = val(:)
    !
  END SUBROUTINE setup_lwl_object_1
  !
  !! Setzen der Feld-Komponente "ext(:)" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_ext_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid)   , POINTER    :: this   ! 
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "ext(:)"
    TYPE (t_d3d_weir) , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_ext_object_1'
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_ext ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_ext   ( this, SIZE(val) )
    IF ( no_error( ) ) this%ext(:) = val(:)
    !
  END SUBROUTINE setup_ext_object_1
  !
  !! Setzen der Feld-Komponente "huu(:)" des (Arbeits-) Objektes <BR>
  !! Zugleich wird die Anzahl Gitterkanten gesetzt oder, sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_huu_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid)    , POINTER    :: this   ! 
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "huu(:)"
    REAL (KIND=Double) , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_huu_object_1'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( this%ns < 1 ) this%ns = SIZE ( val )
    IF ( this%ns /= SIZE ( val ) ) THEN
       CALL setup_error_act ( all_errors(:), 3750, c_upname, c_modname )
       CALL setup_error_act ( '<component-name>', 'huu(:)' )
       WRITE(ctmp,'(I10)') SIZE ( val ) ; CALL setup_error_act ( '<SIZE(..)>', ctmp )
       WRITE(ctmp,'(I10)') this%ns      ; CALL setup_error_act ( '<...ns...>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_huu ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_huu   ( this, SIZE(val) )
    IF ( no_error( ) ) this%huu(:) = val(:)
    !
  END SUBROUTINE setup_huu_object_1
  !
  !! Setzen der Feld-Komponente "hvu(:)" des (Arbeits-) Objektes <BR>
  !! Zugleich wird die Anzahl Gitterknoten gesetzt oder sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_hvu_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid)    , POINTER    :: this   ! 
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "hvu(:)"
    REAL (KIND=Double) , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_hvu_object_1'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( this%nv < 1 ) this%nv = SIZE ( val )
    IF ( this%nv /= SIZE ( val ) ) THEN
       CALL setup_error_act ( all_errors(:), 3550, c_upname, c_modname )
       CALL setup_error_act ( '<component-name>', 'hvu(:)' )
       WRITE(ctmp,'(I10)') SIZE ( val ) ; CALL setup_error_act ( '<SIZE(..)>', ctmp )
       WRITE(ctmp,'(I10)') this%nv      ; CALL setup_error_act ( '<...nv...>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_hvu ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_hvu   ( this, SIZE(val) )
    IF ( no_error( ) ) this%hvu(:) = val(:)
    !
  END SUBROUTINE setup_hvu_object_1
  !
  !! Setzen der Feld-Komponente "hwu(:)" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_hwu_object_1 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid)    , POINTER    :: this   ! 
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "hwu(:)"
    REAL (KIND=Double) , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_hwu_object_1'
    !! Hilfstext
    CHARACTER (LEN=10) :: ctmp ! 
    !
    IF ( this%ne < 1 ) this%ne = SIZE ( val )
    IF ( this%ne /= SIZE ( val ) ) THEN
       CALL setup_error_act ( all_errors(:), 3760, c_upname, c_modname )
       CALL setup_error_act ( '<component-name>', 'hw(:)' )
       WRITE(ctmp,'(I10)') SIZE ( val ) ; CALL setup_error_act ( '<SIZE(..)>', ctmp )
       WRITE(ctmp,'(I10)') this%ne      ; CALL setup_error_act ( '<...ne...>', ctmp )
    ENDIF
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_hwu ( this            )
    IF ( no_error( ) ) CALL alloc_h_grid_hwu   ( this, SIZE(val) )
    IF ( no_error( ) ) this%hwu(:) = val(:)
    !
  END SUBROUTINE setup_hwu_object_1
  !
  !! Setzen der Feld-Komponente "dwlp" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_dwlp_object_0 ( this, val )
    !! Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid)    , POINTER    :: this   ! 
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "dwlp"
    CHARACTER (LEN=*)  , INTENT(IN) :: val    ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER :: c_upname='setup_dwlp_object_0'
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_dwlp ( this )
    IF ( no_error( ) ) CALL alloc_h_grid_dwlp   ( this )
    IF ( no_error( ) ) this%dwlp = val(1:MIN(LEN_TRIM(val),c_len_dwlp))
    !
  END SUBROUTINE setup_dwlp_object_0
  !
  !! De-Allokieren aller Komponenten eines Objektes "t_h_grid" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_object_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !
    IF ( no_error( ) ) CALL dealloc_h_grid_nv    ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_ns    ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_nsi   ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_nsf   ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_ne    ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_nrand ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_nptfr ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_nptir ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_xy    ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_nen   ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_irand ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_ks    ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_hv    ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_time  ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_nbc   ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_hland ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_angle ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_text  ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_jb    ( this )    
    IF ( no_error( ) ) CALL dealloc_h_grid_jt    ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_is    ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_je    ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_ie    ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_xs    ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_xc    ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_xg    ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_dx    ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_dy    ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_dg    ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_aa    ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_hu    ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_hw    ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_nr    ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_ncsize( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_ipobo ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_knolg ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_quant ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_xyz   ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_ind   ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_ele   ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_dope  ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_exch  ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_span  ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_stamp ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_space ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_dxmin ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_b_ms  ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_b_ss  ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_b_s   ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_b_v   ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_b_t   ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_m     ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_n     ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_enc   ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_bnd   ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_thd   ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_lwl   ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_ext   ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_dry   ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_isbnd ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_isdam ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_huu   ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_hvu   ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_hwu   ( this )
    IF ( no_error( ) ) CALL dealloc_h_grid_dwlp  ( this )
    !
  END SUBROUTINE dealloc_h_grid_object_0
  !
  !! Hole Kopie von "name" aus Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_name_object_0 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Komponente "name" aus Objekt
    CHARACTER (LEN=80) :: val
    !
    val = this%name
    !
  END FUNCTION get_name_object_0
  !
  !! Hole Kopie von "file" aus Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_file_object_0 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Komponente "file" aus Objekt
    TYPE (t_file) :: val
    !
    val = this%file
    !
  END FUNCTION get_file_object_0
  !
  !! Hole Pointer auf "nv" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_nv_object_0 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Komponente "nv" aus Objekt
    INTEGER , POINTER :: val
    !
    val => this%nv
    !
  END FUNCTION get_nv_object_0
  !
  !! Hole Pointer auf "ns" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_ns_object_0 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Komponente "ns" aus Objekt
    INTEGER , POINTER :: val ! 
    !
    IF ( this%ns == 0 ) THEN
       WRITE(*,*) ' *** Warning *** Wert fuer Komponente "ns" == 0 '
    END IF
    val => this%ns
    !
  END FUNCTION get_ns_object_0
  !
  !! Hole Pointer auf "nsi" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_nsi_object_0 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Komponente "nsi" aus Objekt
    INTEGER , POINTER :: val ! 
    !
    val => this%nsi
    !
  END FUNCTION get_nsi_object_0
  !
  !! Hole Pointer auf "nsf" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_nsf_object_0 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Komponente "nsf" aus Objekt
    INTEGER , POINTER :: val ! 
    !
    val => this%nsf
    !
  END FUNCTION get_nsf_object_0
  !
  !! Hole Pointer auf "ne" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_ne_object_0 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Komponente "ne" aus Objekt
    INTEGER , POINTER :: val ! 
    !
    val => this%ne
    !
  END FUNCTION get_ne_object_0
  !
  !! Hole Pointer auf "nr" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_nr_object_0 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Komponente "nr" aus Objekt
    INTEGER , POINTER :: val ! 
    !
    IF ( ASSOCIATED( this%nr ) ) THEN
       val => this%nr
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "nr"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_nr_object_0
  !
  !! Hole Pointer auf "ncsize" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_ncsize_object_0 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Komponente "ncsize" aus Objekt
    INTEGER , POINTER :: val ! 
    !
    IF ( ASSOCIATED( this%ncsize ) ) THEN
       val => this%ncsize
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "ncsize"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_ncsize_object_0
  !
  !! Hole Pointer auf "xy" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_xy_object_2 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Ergebniswert: Feld "xy" aus Objekt
    REAL (KIND=Double), POINTER :: val(:,:) ! 
    !
    IF ( ASSOCIATED( this%xy ) ) THEN
       val => this%xy
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "xy(:,:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_xy_object_2
  !
  !! Hole Pointer auf "nen" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_nen_object_2 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Feld "nen" aus Objekt
    INTEGER , POINTER :: val(:,:) ! 
    !
    IF ( ASSOCIATED( this%nen ) ) THEN
       val => this%nen
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "nen(:,:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_nen_object_2
  !
  !! Hole Pointer auf "irand" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_irand_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Feld "irand" aus Objekt
    INTEGER , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%irand ) ) THEN
       val => this%irand
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "irand(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_irand_object_1
  !
  !! Hole Pointer auf "ks" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ks_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Feld "ks" aus Objekt
    INTEGER , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%ks ) ) THEN
       val => this%ks
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "ks(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_ks_object_1
  !
  !! Hole Pointer auf "hv" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_hv_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Feld "hv" aus Objekt
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%hv ) ) THEN
       val => this%hv
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "hv(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_hv_object_1
  !
  !! Hole Pointer auf "nrand" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_nrand_object_0 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Skalar "nrand" aus Objekt
    INTEGER , POINTER :: val ! 
    !
    IF ( ASSOCIATED( this%nrand ) ) THEN
       val => this%nrand
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "nrand"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_nrand_object_0
  !
  !! Hole Pointer auf "nptfr" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_nptfr_object_0 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Skalar "nptfr" aus Objekt
    INTEGER , POINTER :: val ! 
    !
    IF ( ASSOCIATED( this%nptfr ) ) THEN
       val => this%nptfr
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "nptfr"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_nptfr_object_0
  !
  !! Hole Pointer auf "nptir" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_nptir_object_0 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Skalar "nptir" aus Objekt
    INTEGER , POINTER :: val ! 
    !
    IF ( ASSOCIATED( this%nptir ) ) THEN
       val => this%nptir
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "nptir"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_nptir_object_0
  !
  !! Hole Pointer auf "time" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_time_object_0 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Skalar "time" aus Objekt
    TYPE (t_datetime), POINTER :: val ! 
    !
    IF ( ASSOCIATED( this%time ) ) THEN
       val => this%time
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "time"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_time_object_0
  !
  !! Hole Pointer auf "nbc" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_nbc_object_0 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Skalar "nbc" aus Objekt
    INTEGER         , POINTER    :: val ! 
    !
    IF ( ASSOCIATED( this%nbc ) ) THEN
       val => this%nbc
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "nbc"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_nbc_object_0
  !
  !! Hole Pointer auf "hland" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_hland_object_0 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Skalar "hland" aus Objekt
    REAL (KIND=Double) , POINTER :: val ! 
    !
    IF ( ASSOCIATED( this%hland ) ) THEN
       val => this%hland
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "hland"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_hland_object_0
  !
  !! Hole Pointer auf "angle" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_angle_object_0 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Skalar "angle" aus Objekt
    REAL (KIND=Double) , POINTER :: val ! 
    !
    IF ( ASSOCIATED( this%angle ) ) THEN
       val => this%angle
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "angle"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_angle_object_0
  !
  !! Hole Pointer auf "text" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_text_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Feld "text" aus Objekt
    CHARACTER (LEN=80) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%text ) ) THEN
       val => this%text
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "text(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_text_object_1
  !
  !! Hole Pointer auf "jb" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_jb_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Feld "jb" aus Objekt
    INTEGER , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%jb ) ) THEN
       val => this%jb
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "jb(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_jb_object_1
  !
  !! Hole Pointer auf "jt" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_jt_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Feld "jt" aus Objekt
    INTEGER , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%jt ) ) THEN
       val => this%jt
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "jt(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_jt_object_1
  !
  !! Hole Pointer auf "is" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_is_object_2 ( this ) &
       RESULT ( val )
    !
    USE m_dredgesim_data, ONLY : DEBUG_ds
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Feld "is" aus Objekt
    INTEGER , POINTER :: val(:,:) ! 
    !
    IF ( ASSOCIATED( this%is ) ) THEN
       val => this%is
    ELSE
       IF (DEBUG_ds > 0) THEN
          WRITE(*,*) ' *** Warning *** keine Daten "is(:,:)"'
       END IF
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_is_object_2
  !
  !! Hole Pointer auf "je" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_je_object_2 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Feld "je" aus Objekt
    INTEGER , POINTER :: val(:,:) ! 
    !
    IF ( ASSOCIATED( this%je ) ) THEN
       val => this%je
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "je(:,:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_je_object_2
  !
  !! Hole Pointer auf "ie" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ie_object_2 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Feld "ie" aus Objekt
    INTEGER , POINTER :: val(:,:) ! 
    !
    IF ( ASSOCIATED( this%ie ) ) THEN
       val => this%ie
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "ie(:,:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_ie_object_2
  !
  !! Hole Pointer auf "xs" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_xs_object_2 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Feld "xs" aus Objekt
    REAL (KIND=Double) , POINTER :: val(:,:)
    !
    IF ( ASSOCIATED( this%xs ) ) THEN
       val => this%xs
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "xs(:,:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_xs_object_2
  !
  !! Hole Pointer auf "xc" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_xc_object_2 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Feld "xc" aus Objekt
    REAL (KIND=Double) , POINTER :: val(:,:) ! 
    !
    IF ( ASSOCIATED( this%xc ) ) THEN
       val => this%xc
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "xc(:,:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_xc_object_2
  !
  !! Hole Pointer auf "xg" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_xg_object_2 ( this ) &
       RESULT ( val )
    !
    USE m_dredgesim_data, ONLY : DEBUG_ds
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Feld "xg" aus Objekt
    REAL (KIND=Double) , POINTER :: val(:,:) ! 
    !
    IF ( ASSOCIATED( this%xg ) ) THEN
       val => this%xg
    ELSE
       IF (DEBUG_ds > 0) THEN
       	  WRITE(*,*) ' *** Warning *** keine Daten "xg(:,:)"'
       END IF
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_xg_object_2
  !
  !! Hole Pointer auf "dx" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_dx_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Feld "dx" aus Objekt
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%dx ) ) THEN
       val => this%dx
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "dx(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_dx_object_1
  !
  !! Hole Pointer auf "dy" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_dy_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Feld "dy" aus Objekt
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%dy ) ) THEN
       val => this%dy
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "dy(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_dy_object_1
  !
  !! Hole Pointer auf "dg" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_dg_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Feld "dg" aus Objekt
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%dg ) ) THEN
       val => this%dg
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "dg(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_dg_object_1
  !
  !! Hole Pointer auf "aa" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_aa_object_1 ( this ) &
       RESULT ( val )
    !
    USE m_dredgesim_data, ONLY : DEBUG_ds
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Feld "aa" aus Objekt
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%aa ) ) THEN
       val => this%aa
    ELSE
       IF (DEBUG_ds > 0) THEN
          WRITE(*,*) ' *** Warning *** keine Daten "aa(:)"'
       END IF
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_aa_object_1
  !
  !! Hole Pointer auf "hu" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_hu_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Feld "hu" aus Objekt
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%hu ) ) THEN
       val => this%hu
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "hu(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_hu_object_1
  !
  !! Hole Pointer auf "hw" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_hw_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Feld "hw" aus Objekt
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%hw ) ) THEN
       val => this%hw
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "hw(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_hw_object_1
  !
  !! Hole Pointer auf "ipobo" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ipobo_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Feld "ipobo" aus Objekt
    INTEGER , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%ipobo ) ) THEN
       val => this%ipobo
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "ipobo(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_ipobo_object_1
  !
  !! Hole Pointer auf "knolg" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_knolg_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Feld "knolg" aus Objekt
    INTEGER , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%knolg ) ) THEN
       val => this%knolg
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "knolg(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_knolg_object_1
  !
  !! Hole Pointer auf "quant" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_quant_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Feld "quant" aus Objekt
    TYPE (t_omi_quant) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%quant ) ) THEN
       val => this%quant
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "quant(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_quant_object_1
  !
  !! Hole Pointer auf "xyz" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_xyz_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid)  , POINTER :: this   ! 
    !! Ergebniswert: Feld "xyz" aus Objekt
    TYPE (t_omi_xyz) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%xyz ) ) THEN
       val => this%xyz
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "xyz(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_xyz_object_1
  !
  !! Hole Pointer auf "dope" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_dope_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid)   , POINTER :: this   ! 
    !! Ergebniswert: Feld "dope" aus Objekt
    TYPE (t_omi_dope) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%dope ) ) THEN
       val => this%dope
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "dope(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_dope_object_1
  !
  !! Hole Pointer auf "ele" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ele_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid)  , POINTER :: this   ! 
    !! Ergebniswert: Feld "ele" aus Objekt
    TYPE (t_omi_ele) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%ele ) ) THEN
       val => this%ele
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "ele(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_ele_object_1
  !
  !! Hole Pointer auf "ind" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ind_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid)  , POINTER :: this   ! 
    !! Ergebniswert: Feld "ind" aus Objekt
    TYPE (t_omi_ind) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%ind ) ) THEN
       val => this%ind
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "ind(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_ind_object_1
  !
  !! Hole Pointer auf "exch" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_exch_object_1 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid)   , POINTER :: this   ! 
    !! Ergebniswert: Feld "exch" aus Objekt
    TYPE (t_omi_exch) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%exch ) ) THEN
       val => this%exch
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "exch(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_exch_object_1
  !
  !! Hole Pointer auf "span" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_span_object_0 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid)   , POINTER :: this ! 
    !! Ergebniswert: Skalar "span" aus Objekt
    TYPE (t_omi_span) , POINTER :: val  ! 
    !
    IF ( ASSOCIATED( this%span ) ) THEN
       val => this%span
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "span"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_span_object_0
  !
  !! Hole Pointer auf "stamp" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_stamp_object_0 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid)   , POINTER :: this ! 
    !! Ergebniswert: Skalar "stamp" aus Objekt
    TYPE (t_omi_stamp) , POINTER :: val  ! 
    !
    IF ( ASSOCIATED( this%stamp ) ) THEN
       val => this%stamp
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "stamp"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_stamp_object_0
  !
  !! Hole Pointer auf "space" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_space_object_0 ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid)   , POINTER :: this ! 
    !! Ergebniswert: Skalar "space" aus Objekt
    TYPE (t_omi_space) , POINTER :: val  ! 
    !
    IF ( ASSOCIATED( this%space ) ) THEN
       val => this%space
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "space"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_space_object_0
  !
  !! Hole Pointer auf "dxmin" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_dxmin_object_0 &
       ( this ) &
       RESULT ( val )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Skalar "dxmin" aus Objekt
    REAL (KIND=Double) , POINTER :: val ! 
    !
    IF ( ASSOCIATED( this%dxmin ) ) THEN
       val => this%dxmin
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "dxmin"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_dxmin_object_0
  !
  !! Hole Pointer auf "b_ms(:)" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_b_ms_object_1 ( this ) &
       RESULT ( val )
    TYPE (t_h_grid) , POINTER :: this
    INTEGER , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%b_ms ) ) THEN
       val => this%b_ms
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "b_ms(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_b_ms_object_1
  !
  !! Hole Pointer auf "b_ss(:)" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_b_ss_object_1 ( this ) &
       RESULT ( val )
    TYPE (t_h_grid) , POINTER :: this
    INTEGER , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%b_ss ) ) THEN
       val => this%b_ss
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "b_ss(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_b_ss_object_1
  !
  !! Hole Pointer auf "b_s(:)" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_b_s_object_1 ( this ) &
       RESULT ( val )
    TYPE (t_h_grid) , POINTER :: this
    INTEGER , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%b_s ) ) THEN
       val => this%b_s
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "b_s(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_b_s_object_1
  !
  !! Hole Pointer auf "b_v(:)" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_b_v_object_1 ( this ) &
       RESULT ( val )
    TYPE (t_h_grid) , POINTER :: this
    INTEGER , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%b_v ) ) THEN
       val => this%b_v
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "b_v(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_b_v_object_1
  !
  !! Hole Pointer auf "b_t(:)" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_b_t_object_1 ( this ) &
       RESULT ( val )
    TYPE (t_h_grid) , POINTER :: this
    INTEGER , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%b_t ) ) THEN
       val => this%b_t
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "b_t(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_b_t_object_1
  !
  !! Hole Pointer auf "m" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_m_object_0 ( this ) &
       RESULT ( val )
    TYPE (t_h_grid) , POINTER :: this
    INTEGER , POINTER :: val ! 
    !
    IF ( ASSOCIATED( this%m ) ) THEN
       val => this%m
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "m"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_m_object_0
  !
  !! Hole Pointer auf "n" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_n_object_0 ( this ) &
       RESULT ( val )
    TYPE (t_h_grid) , POINTER :: this
    INTEGER , POINTER :: val ! 
    !
    IF ( ASSOCIATED( this%n ) ) THEN
       val => this%n
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "n"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_n_object_0
  !
  !! Hole Pointer auf "enc(:,:)" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_enc_object_2 ( this ) &
       RESULT ( val )
    TYPE (t_h_grid) , POINTER :: this
    INTEGER , POINTER :: val(:,:) ! 
    !
    IF ( ASSOCIATED( this%enc ) ) THEN
       val => this%enc
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "enc(:,:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_enc_object_2
  !
  !! Hole Pointer auf "dry(:,:)" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_dry_object_2 ( this ) &
       RESULT ( val )
    TYPE (t_h_grid) , POINTER :: this
    INTEGER , POINTER :: val(:,:) ! 
    !
    IF ( ASSOCIATED( this%dry ) ) THEN
       val => this%dry
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "dry(:,:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_dry_object_2
  !
  !! Hole Pointer auf "isbnd(:,:)" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_isbnd_object_2 ( this ) &
       RESULT ( val )
    TYPE (t_h_grid) , POINTER :: this
    INTEGER , POINTER :: val(:,:) ! 
    !
    IF ( ASSOCIATED( this%isbnd ) ) THEN
       val => this%isbnd
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "isbnd(:,:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_isbnd_object_2
  !
  !! Hole Pointer auf "isdam(:,:,:)" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_isdam_object_3 ( this ) &
       RESULT ( val )
    TYPE (t_h_grid) , POINTER :: this
    INTEGER , POINTER :: val(:,:,:) ! 
    !
    IF ( ASSOCIATED( this%isdam ) ) THEN
       val => this%isdam
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "isdam(:,:,:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_isdam_object_3
  !
  !! Hole Pointer auf "bnd(:)" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_bnd_object_1 ( this ) &
       RESULT ( val )
    TYPE (t_h_grid)     , POINTER :: this   ! 
    TYPE (t_d3d_openbc) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%bnd ) ) THEN
       val => this%bnd
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "bnd(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_bnd_object_1
  !
  !! Hole Pointer auf "thd(:)" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_thd_object_1 ( this ) &
       RESULT ( val )
    TYPE (t_h_grid)  , POINTER :: this   ! 
    TYPE (t_d3d_thd) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%thd ) ) THEN
       val => this%thd
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "thd(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_thd_object_1
  !
  !! Hole Pointer auf "lwl(:)" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_lwl_object_1 ( this ) &
       RESULT ( val )
    TYPE (t_h_grid)   , POINTER :: this   ! 
    TYPE (t_d3d_weir) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%lwl ) ) THEN
       val => this%lwl
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "lwl(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_lwl_object_1
  !
  !! Hole Pointer auf "ext(:)" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_ext_object_1 ( this ) &
       RESULT ( val )
    TYPE (t_h_grid)   , POINTER :: this   ! 
    TYPE (t_d3d_weir) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%ext ) ) THEN
       val => this%ext
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "ext(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_ext_object_1
  !
  !! Hole Pointer auf "huu(:)" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_huu_object_1 ( this ) &
       RESULT ( val )
    TYPE (t_h_grid)    , POINTER :: this   ! 
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%huu ) ) THEN
       val => this%huu
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "huu(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_huu_object_1
  !
  !! Hole Pointer auf "hvu(:)" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_hvu_object_1 ( this ) &
       RESULT ( val )
    TYPE (t_h_grid)    , POINTER :: this   ! 
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%hvu ) ) THEN
       val => this%hvu
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "hvu(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_hvu_object_1
  !
  !! Hole Pointer auf "hwu(:)" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_hwu_object_1 ( this ) &
       RESULT ( val )
    TYPE (t_h_grid)    , POINTER :: this   ! 
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !
    IF ( ASSOCIATED( this%hwu ) ) THEN
       val => this%hwu
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "hwu(:)"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_hwu_object_1
  !
  !! Hole Pointer auf "dwlp" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_dwlp_object_0 ( this ) &
       RESULT ( val )
    TYPE (t_h_grid)            , POINTER :: this ! 
    CHARACTER (LEN=c_len_dwlp) , POINTER :: val  ! 
    !
    IF ( ASSOCIATED( this%dwlp ) ) THEN
       val => this%dwlp
    ELSE
       WRITE(*,*) ' *** Warning *** keine Daten "dwlp"'
       NULLIFY ( val )
    END IF
    !
  END FUNCTION get_dwlp_object_0
  !
  !! Hole Code f&uuml;r Komponente "dwlp" in Objekt "this" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_dwlp_code_object_0 ( this ) &
       RESULT ( val )
    TYPE (t_h_grid) , POINTER :: this ! 
    !! R&uuml;ckgabewert: Code f&uuml;r Komponente "dwlp"
    !! 1 == 'MEAN'
    !! 2 == 'MAX '
    !! 3 == 'MIN '
    INTEGER :: val  ! 
    !! Hilfsvariable
    INTEGER :: i    ! 
    !
    val = -1
    IF ( ASSOCIATED( this%dwlp ) ) THEN
       DO i=1,SIZE(c_dwlp_valid)
          IF ( val > 0 ) EXIT
          IF ( c_dwlp_valid(i) == this%dwlp ) val = i
       END DO
    END IF
    val = MAX(1,val)
    !
  END FUNCTION get_dwlp_code_object_0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "nv" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION target_nv_object_0 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    INTEGER , POINTER :: val
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok
    !
    ok = ASSOCIATED( val, this%nv )
    !
  END FUNCTION target_nv_object_0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "ns" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION target_ns_object_0 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    INTEGER , POINTER :: val
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok
    !
    ok = ASSOCIATED( val, this%ns )
    !
  END FUNCTION target_ns_object_0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "nsi" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION target_nsi_object_0 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    INTEGER , POINTER :: val
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok
    !
    ok = ASSOCIATED( val, this%nsi )
    !
  END FUNCTION target_nsi_object_0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "nsf" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION target_nsf_object_0 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    INTEGER , POINTER :: val
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok
    !
    ok = ASSOCIATED( val, this%nsf )
    !
  END FUNCTION target_nsf_object_0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "ne" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION target_ne_object_0 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    INTEGER , POINTER :: val
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok
    !
    ok = ASSOCIATED( val, this%ne )
    !
  END FUNCTION target_ne_object_0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "nr" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION target_nr_object_0 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    INTEGER , POINTER :: val
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok
    !
    ok = ASSOCIATED( val, this%nr )
    !
  END FUNCTION target_nr_object_0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "ncsize" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION target_ncsize_object_0 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    INTEGER , POINTER :: val
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok
    !
    ok = ASSOCIATED( val, this%ncsize )
    !
  END FUNCTION target_ncsize_object_0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "xy" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_xy_object_2 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    REAL (KIND=Double) , POINTER :: val(:,:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok
    !
    IF ( ASSOCIATED( this%xy ) ) THEN
       ok = ASSOCIATED( val, this%xy )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_xy_object_2
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "nen" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_nen_object_2 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    INTEGER , POINTER :: val(:,:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%nen ) ) THEN
       ok = ASSOCIATED( val, this%nen )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_nen_object_2
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "irand" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_irand_object_1 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    INTEGER , POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%irand ) ) THEN
       ok = ASSOCIATED( val, this%irand )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_irand_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "ks" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_ks_object_1 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    INTEGER , POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%ks ) ) THEN
       ok = ASSOCIATED( val, this%ks )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_ks_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "hv" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_hv_object_1 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%hv ) ) THEN
       ok = ASSOCIATED( val, this%hv )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_hv_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "nrand" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_nrand_object_0 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    INTEGER, POINTER :: val ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%nrand ) ) THEN
       ok = ASSOCIATED( val, this%nrand )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_nrand_object_0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "nrand" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_nptfr_object_0 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    INTEGER, POINTER :: val ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%nptfr ) ) THEN
       ok = ASSOCIATED( val, this%nptfr )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_nptfr_object_0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "nrand" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_nptir_object_0 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    INTEGER, POINTER :: val ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%nptir ) ) THEN
       ok = ASSOCIATED( val, this%nptir )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_nptir_object_0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "time" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_time_object_0 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    TYPE (t_datetime) , POINTER   :: val ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%time ) ) THEN
       ok = ASSOCIATED( val, this%time )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_time_object_0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "nbc" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_nbc_object_0 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    INTEGER          , POINTER    :: val ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%nbc ) ) THEN
       ok = ASSOCIATED( val, this%nbc )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_nbc_object_0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "hland" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_hland_object_0 ( this, val ) &
       RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    REAL (KIND=Double) , POINTER  :: val ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%hland ) ) THEN
       ok = ASSOCIATED( val, this%hland )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_hland_object_0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "angle" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_angle_object_0 ( this, val ) &
       RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    REAL (KIND=Double) , POINTER  :: val ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%angle ) ) THEN
       ok = ASSOCIATED( val, this%angle )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_angle_object_0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "text" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_text_object_1 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    CHARACTER(LEN=80) , POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%text ) ) THEN
       ok = ASSOCIATED( val, this%text )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_text_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "jb" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_jb_object_1 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    INTEGER , POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%jb ) ) THEN
       ok = ASSOCIATED( val, this%jb )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_jb_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "jt" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_jt_object_1 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    INTEGER , POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%jt ) ) THEN
       ok = ASSOCIATED( val, this%jt )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_jt_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "is" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_is_object_2 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    INTEGER , POINTER :: val(:,:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%is ) ) THEN
       ok = ASSOCIATED( val, this%is )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_is_object_2
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "je" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_je_object_2 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    INTEGER , POINTER :: val(:,:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%je ) ) THEN
       ok = ASSOCIATED( val, this%je )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_je_object_2
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "ie" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_ie_object_2 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    INTEGER , POINTER :: val(:,:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%ie ) ) THEN
       ok = ASSOCIATED( val, this%ie )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_ie_object_2
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "xs" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_xs_object_2 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    REAL (KIND=Double), POINTER :: val(:,:)
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok
    !
    IF ( ASSOCIATED( this%xs ) ) THEN
       ok = ASSOCIATED( val, this%xs )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_xs_object_2
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "xc" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_xc_object_2 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    REAL (KIND=Double), POINTER :: val(:,:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%xc ) ) THEN
       ok = ASSOCIATED( val, this%xc )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_xc_object_2
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "xg" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_xg_object_2 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    REAL (KIND=Double), POINTER :: val(:,:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%xg ) ) THEN
       ok = ASSOCIATED( val, this%xg )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_xg_object_2
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "dx" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_dx_object_1 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    REAL (KIND=Double), POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%dx ) ) THEN
       ok = ASSOCIATED( val, this%dx )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_dx_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "dy" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_dy_object_1 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    REAL (KIND=Double), POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%dy ) ) THEN
       ok = ASSOCIATED( val, this%dy )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_dy_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "dg" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_dg_object_1 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    REAL (KIND=Double), POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%dg ) ) THEN
       ok = ASSOCIATED( val, this%dg )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_dg_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "aa" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_aa_object_1 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    REAL (KIND=Double), POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%aa ) ) THEN
       ok = ASSOCIATED( val, this%aa )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_aa_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "hu" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_hu_object_1 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    REAL (KIND=Double), POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%hu ) ) THEN
       ok = ASSOCIATED( val, this%hu )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_hu_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "hw" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_hw_object_1 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    REAL (KIND=Double), POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%hw ) ) THEN
       ok = ASSOCIATED( val, this%hw )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_hw_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "ipobo" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_ipobo_object_1 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    INTEGER, POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%ipobo ) ) THEN
       ok = ASSOCIATED( val, this%ipobo )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_ipobo_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "knolg" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_knolg_object_1 ( this, val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this
    !! aktueller Pointer
    INTEGER, POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%knolg ) ) THEN
       ok = ASSOCIATED( val, this%knolg )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_knolg_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "quant" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_quant_object_1 ( this, val ) &
       RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid)    , POINTER :: this
    !! aktueller Pointer
    TYPE (t_omi_quant) , POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%quant ) ) THEN
       ok = ASSOCIATED( val, this%quant )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_quant_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "xyz" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_xyz_object_1 ( this, val ) &
       RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid)  , POINTER :: this
    !! aktueller Pointer
    TYPE (t_omi_xyz) , POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%xyz ) ) THEN
       ok = ASSOCIATED( val, this%xyz )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_xyz_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "ele" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_ele_object_1 ( this, val ) &
       RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid)  , POINTER :: this
    !! aktueller Pointer
    TYPE (t_omi_ele) , POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%ele ) ) THEN
       ok = ASSOCIATED( val, this%ele )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_ele_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "ind" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_ind_object_1 ( this, val ) &
       RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid)  , POINTER :: this
    !! aktueller Pointer
    TYPE (t_omi_ind) , POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%ind ) ) THEN
       ok = ASSOCIATED( val, this%ind )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_ind_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "dope" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_dope_object_1 ( this, val ) &
       RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid)   , POINTER :: this
    !! aktueller Pointer
    TYPE (t_omi_dope) , POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%dope ) ) THEN
       ok = ASSOCIATED( val, this%dope )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_dope_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "exch" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_exch_object_1 ( this, val ) &
       RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid)   , POINTER :: this
    !! aktueller Pointer
    TYPE (t_omi_exch) , POINTER :: val(:) ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%exch ) ) THEN
       ok = ASSOCIATED( val, this%exch )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_exch_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "span" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_span_object_0 ( this, val ) &
       RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid)   , POINTER :: this
    !! aktueller Pointer
    TYPE (t_omi_span) , POINTER :: val 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%span ) ) THEN
       ok = ASSOCIATED( val, this%span )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_span_object_0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "stamp" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_stamp_object_0 ( this, val ) &
       RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid)    , POINTER :: this
    !! aktueller Pointer
    TYPE (t_omi_stamp) , POINTER :: val 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%stamp ) ) THEN
       ok = ASSOCIATED( val, this%stamp )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_stamp_object_0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "space" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_space_object_0 ( this, val ) &
       RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid)    , POINTER :: this
    !! aktueller Pointer
    TYPE (t_omi_space) , POINTER :: val 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%space ) ) THEN
       ok = ASSOCIATED( val, this%space )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_space_object_0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "dxmin" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_dxmin_object_0 &
       ( this, &
         val ) &
         RESULT ( ok )
    !! aktuelles Objekt
    TYPE (t_h_grid)    , POINTER :: this
    !! aktueller Pointer
    REAL (KIND=Double) , POINTER :: val ! 
    !! Ergebniswert: Pruefergebnis true/false
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%dxmin ) ) THEN
       ok = ASSOCIATED( val, this%dxmin )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_dxmin_object_0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "b_ms(:)" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_b_ms_object_1 ( this, val ) &
       RESULT ( ok )
    TYPE (t_h_grid) , POINTER :: this
    INTEGER         , POINTER :: val(:) ! 
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%b_ms ) ) THEN
       ok = ASSOCIATED( val, this%b_ms )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_b_ms_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "b_ss(:)" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_b_ss_object_1 ( this, val ) &
       RESULT ( ok )
    TYPE (t_h_grid) , POINTER :: this
    INTEGER         , POINTER :: val(:) ! 
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%b_ss ) ) THEN
       ok = ASSOCIATED( val, this%b_ss )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_b_ss_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "b_s(:)" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_b_s_object_1 ( this, val ) &
       RESULT ( ok )
    TYPE (t_h_grid) , POINTER :: this
    INTEGER         , POINTER :: val(:) ! 
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%b_s ) ) THEN
       ok = ASSOCIATED( val, this%b_s )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_b_s_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "b_v(:)" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_b_v_object_1 ( this, val ) &
       RESULT ( ok )
    TYPE (t_h_grid) , POINTER :: this
    INTEGER         , POINTER :: val(:) ! 
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%b_v ) ) THEN
       ok = ASSOCIATED( val, this%b_v )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_b_v_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "b_t(:)" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_b_t_object_1 ( this, val ) &
       RESULT ( ok )
    TYPE (t_h_grid) , POINTER :: this
    INTEGER         , POINTER :: val(:) ! 
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%b_t ) ) THEN
       ok = ASSOCIATED( val, this%b_t )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_b_t_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "m" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_m_object_0 ( this, val ) &
       RESULT ( ok )
    TYPE (t_h_grid) , POINTER :: this
    INTEGER         , POINTER :: val  ! 
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%m ) ) THEN
       ok = ASSOCIATED( val, this%m )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_m_object_0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "n" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_n_object_0 ( this, val ) &
       RESULT ( ok )
    TYPE (t_h_grid) , POINTER :: this
    INTEGER         , POINTER :: val  ! 
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%n ) ) THEN
       ok = ASSOCIATED( val, this%n )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_n_object_0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "enc(:,:)" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_enc_object_2 ( this, val ) &
       RESULT ( ok )
    TYPE (t_h_grid) , POINTER :: this
    INTEGER         , POINTER :: val(:,:)  ! 
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%enc ) ) THEN
       ok = ASSOCIATED( val, this%enc )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_enc_object_2
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "dry(:,:)" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_dry_object_2 ( this, val ) &
       RESULT ( ok )
    TYPE (t_h_grid) , POINTER :: this
    INTEGER         , POINTER :: val(:,:)  ! 
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%dry ) ) THEN
       ok = ASSOCIATED( val, this%dry )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_dry_object_2
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "isbnd(:,:)" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_isbnd_object_2 ( this, val ) &
       RESULT ( ok )
    TYPE (t_h_grid) , POINTER :: this
    INTEGER         , POINTER :: val(:,:)  ! 
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%isbnd ) ) THEN
       ok = ASSOCIATED( val, this%isbnd )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_isbnd_object_2
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "isdam(:,:,:)" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_isdam_object_3 ( this, val ) &
       RESULT ( ok )
    TYPE (t_h_grid) , POINTER :: this
    INTEGER         , POINTER :: val(:,:,:)  ! 
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%isdam ) ) THEN
       ok = ASSOCIATED( val, this%isdam )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_isdam_object_3
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "bnd(:)" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_bnd_object_1 ( this, val ) &
       RESULT ( ok )
    TYPE (t_h_grid)     , POINTER :: this   ! 
    TYPE (t_d3d_openbc) , POINTER :: val(:) ! 
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%bnd ) ) THEN
       ok = ASSOCIATED( val, this%bnd )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_bnd_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "thd(:)" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_thd_object_1 ( this, val ) &
       RESULT ( ok )
    TYPE (t_h_grid)  , POINTER :: this   ! 
    TYPE (t_d3d_thd) , POINTER :: val(:) ! 
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%thd ) ) THEN
       ok = ASSOCIATED( val, this%thd )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_thd_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "lwl(:)" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_lwl_object_1 ( this, val ) &
       RESULT ( ok )
    TYPE (t_h_grid)   , POINTER :: this   ! 
    TYPE (t_d3d_weir) , POINTER :: val(:) ! 
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%lwl ) ) THEN
       ok = ASSOCIATED( val, this%lwl )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_lwl_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "ext(:)" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_ext_object_1 ( this, val ) &
       RESULT ( ok )
    TYPE (t_h_grid)   , POINTER :: this   ! 
    TYPE (t_d3d_weir) , POINTER :: val(:) ! 
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%ext ) ) THEN
       ok = ASSOCIATED( val, this%ext )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_ext_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "huu(:)" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_huu_object_1 ( this, val ) &
       RESULT ( ok )
    TYPE (t_h_grid)    , POINTER :: this   ! 
    REAL (KIND=Double) , POINTER :: val(:) ! 
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%huu ) ) THEN
       ok = ASSOCIATED( val, this%huu )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_huu_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "hvu(:)" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_hvu_object_1 ( this, val ) &
       RESULT ( ok )
    TYPE (t_h_grid)    , POINTER :: this   ! 
    REAL (KIND=Double) , POINTER :: val(:) ! 
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%hvu ) ) THEN
       ok = ASSOCIATED( val, this%hvu )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_hvu_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "hwu(:)" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_hwu_object_1 ( this, val ) &
       RESULT ( ok )
    TYPE (t_h_grid)    , POINTER :: this   ! 
    REAL (KIND=Double) , POINTER :: val(:) ! 
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%hwu ) ) THEN
       ok = ASSOCIATED( val, this%hwu )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_hwu_object_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "dwlp" mit "this" &uuml;bereinstimmt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION target_dwlp_object_0 ( this, val ) &
       RESULT ( ok )
    TYPE (t_h_grid)            , POINTER :: this ! 
    CHARACTER (LEN=c_len_dwlp) , POINTER :: val  ! 
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%dwlp ) ) THEN
       ok = ASSOCIATED( val, this%dwlp )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION target_dwlp_object_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Package-Objekt vorliegt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_object_0 ( this ) &
       RESULT( ok )
    !! Objekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=18), PARAMETER :: c_upname='ok_h_grid_object_0'
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp+3) ! 
    INTEGER :: nn                ! 
    !
    l_ok(:)  = .false.
    !
    l_ok( 1) = ok_h_grid_id    ( this ) !; write(*,*) ' >GL> l_ok( 1) = ',l_ok( 1)
    l_ok( 2) = ok_h_grid_name  ( this ) !; write(*,*) ' >GL> l_ok( 2) = ',l_ok( 2)
    l_ok( 3) = ok_h_grid_file  ( this ) !; write(*,*) ' >GL> l_ok( 3) = ',l_ok( 3)
    l_ok( 4) = ok_h_grid_nv    ( this ) !; write(*,*) ' >GL> l_ok( 4) = ',l_ok( 4)
    l_ok( 5) = ok_h_grid_ns    ( this ) !; write(*,*) ' >GL> l_ok( 5) = ',l_ok( 5)
    l_ok( 6) = ok_h_grid_nsi   ( this ) !; write(*,*) ' >GL> l_ok( 6) = ',l_ok( 6)
    l_ok( 7) = ok_h_grid_nsf   ( this ) !; write(*,*) ' >GL> l_ok( 7) = ',l_ok( 7)
    l_ok( 8) = ok_h_grid_ne    ( this ) !; write(*,*) ' >GL> l_ok( 8) = ',l_ok( 8)
    ! l_ok( 9) = ok_h_grid_xy    ( this ) !; write(*,*) ' >GL> l_ok( 9) = ',l_ok( 9)
    l_ok( 9) = .true.                   !; write(*,*) ' >GL> l_ok( 9) = ',l_ok( 9)
    l_ok(10) = ok_h_grid_nen   ( this ) !; write(*,*) ' >GL> l_ok(10) = ',l_ok(10)
    l_ok(11) = ok_h_grid_irand ( this ) !; write(*,*) ' >GL> l_ok(11) = ',l_ok(11)
    l_ok(12) = ok_h_grid_ks    ( this ) !; write(*,*) ' >GL> l_ok(12) = ',l_ok(12)
    l_ok(13) = ok_h_grid_hv    ( this ) !; write(*,*) ' >GL> l_ok(13) = ',l_ok(13)
    l_ok(14) = ok_h_grid_nrand ( this ) !; write(*,*) ' >GL> l_ok(14) = ',l_ok(14)
    l_ok(15) = ok_h_grid_time  ( this ) !; write(*,*) ' >GL> l_ok(15) = ',l_ok(15)
    l_ok(16) = ok_h_grid_nbc   ( this ) !; write(*,*) ' >GL> l_ok(16) = ',l_ok(16)
    l_ok(17) = ok_h_grid_hland ( this ) !; write(*,*) ' >GL> l_ok(17) = ',l_ok(17)
    l_ok(18) = ok_h_grid_angle ( this ) !; write(*,*) ' >GL> l_ok(18) = ',l_ok(18)
    l_ok(19) = ok_h_grid_text  ( this ) !; write(*,*) ' >GL> l_ok(19) = ',l_ok(19)
    l_ok(20) = ok_h_grid_jb    ( this ) !; write(*,*) ' >GL> l_ok(20) = ',l_ok(20)
    l_ok(21) = ok_h_grid_jt    ( this ) !; write(*,*) ' >GL> l_ok(21) = ',l_ok(21)
    l_ok(22) = ok_h_grid_is    ( this ) !; write(*,*) ' >GL> l_ok(22) = ',l_ok(22)
    l_ok(23) = ok_h_grid_je    ( this ) !; write(*,*) ' >GL> l_ok(23) = ',l_ok(23)
    l_ok(24) = ok_h_grid_ie    ( this ) !; write(*,*) ' >GL> l_ok(24) = ',l_ok(24)
    l_ok(25) = ok_h_grid_xs    ( this ) !; write(*,*) ' >GL> l_ok(25) = ',l_ok(25)
    l_ok(26) = ok_h_grid_xc    ( this ) !; write(*,*) ' >GL> l_ok(26) = ',l_ok(26)
    l_ok(27) = ok_h_grid_xg    ( this ) !; write(*,*) ' >GL> l_ok(27) = ',l_ok(27)
    l_ok(28) = ok_h_grid_dx    ( this ) !; write(*,*) ' >GL> l_ok(28) = ',l_ok(28)
    l_ok(29) = ok_h_grid_dy    ( this ) !; write(*,*) ' >GL> l_ok(29) = ',l_ok(29)
    l_ok(30) = ok_h_grid_dg    ( this ) !; write(*,*) ' >GL> l_ok(30) = ',l_ok(30)
    l_ok(31) = ok_h_grid_aa    ( this ) !; write(*,*) ' >GL> l_ok(31) = ',l_ok(31)
    l_ok(32) = ok_h_grid_hu    ( this ) !; write(*,*) ' >GL> l_ok(32) = ',l_ok(32)
    l_ok(33) = ok_h_grid_hw    ( this ) !; write(*,*) ' >GL> l_ok(33) = ',l_ok(33)
    l_ok(34) = ok_h_grid_nr    ( this ) !; write(*,*) ' >GL> l_ok(34) = ',l_ok(34)
    l_ok(35) = ok_h_grid_ipobo ( this ) !; write(*,*) ' >GL> l_ok(35) = ',l_ok(35)
    l_ok(36) = ok_h_grid_dxmin ( this ) !; write(*,*) ' >GL> l_ok(36) = ',l_ok(36)
    l_ok(37) = ok_h_grid_ncsize( this ) !; write(*,*) ' >GL> l_ok(37) = ',l_ok(37)
    l_ok(38) = ok_h_grid_knolg ( this ) !; write(*,*) ' >GL> l_ok(38) = ',l_ok(38)
    l_ok(39) = ok_h_grid_nptfr ( this ) !; write(*,*) ' >GL> l_ok(39) = ',l_ok(39)
    l_ok(40) = ok_h_grid_nptir ( this ) !; write(*,*) ' >GL> l_ok(40) = ',l_ok(40)
    l_ok(41) = ok_h_grid_quant ( this ) !; write(*,*) ' >GL> l_ok(41) = ',l_ok(41)
    l_ok(42) = ok_h_grid_xyz   ( this ) !; write(*,*) ' >GL> l_ok(42) = ',l_ok(42)
    l_ok(43) = ok_h_grid_ind   ( this ) !; write(*,*) ' >GL> l_ok(43) = ',l_ok(43)
    l_ok(44) = ok_h_grid_ele   ( this ) !; write(*,*) ' >GL> l_ok(44) = ',l_ok(44)
    l_ok(45) = ok_h_grid_dope  ( this ) !; write(*,*) ' >GL> l_ok(45) = ',l_ok(45)
    l_ok(46) = ok_h_grid_exch  ( this ) !; write(*,*) ' >GL> l_ok(46) = ',l_ok(46)
    l_ok(47) = ok_h_grid_span  ( this ) !; write(*,*) ' >GL> l_ok(47) = ',l_ok(47)
    l_ok(48) = ok_h_grid_stamp ( this ) !; write(*,*) ' >GL> l_ok(48) = ',l_ok(48)
    l_ok(49) = ok_h_grid_space ( this ) !; write(*,*) ' >GL> l_ok(49) = ',l_ok(49)
    l_ok(50) = ok_h_grid_b_ms  ( this ) !; write(*,*) ' >GL> l_ok(50) = ',l_ok(40)
    l_ok(51) = ok_h_grid_b_ss  ( this ) !; write(*,*) ' >GL> l_ok(51) = ',l_ok(51)
    l_ok(52) = ok_h_grid_b_s   ( this ) !; write(*,*) ' >GL> l_ok(52) = ',l_ok(52)
    l_ok(53) = ok_h_grid_b_v   ( this ) !; write(*,*) ' >GL> l_ok(53) = ',l_ok(53)
    l_ok(54) = ok_h_grid_b_t   ( this ) !; write(*,*) ' >GL> l_ok(54) = ',l_ok(54)
    l_ok(55) = ok_h_grid_m     ( this ) !; write(*,*) ' >GL> l_ok(55) = ',l_ok(55)
    l_ok(56) = ok_h_grid_n     ( this ) !; write(*,*) ' >GL> l_ok(56) = ',l_ok(56)
    l_ok(57) = ok_h_grid_enc   ( this ) !; write(*,*) ' >GL> l_ok(57) = ',l_ok(57)
    l_ok(58) = ok_h_grid_bnd   ( this ) !; write(*,*) ' >GL> l_ok(58) = ',l_ok(58)
    l_ok(59) = ok_h_grid_thd   ( this ) !; write(*,*) ' >GL> l_ok(59) = ',l_ok(59)
    l_ok(60) = ok_h_grid_lwl   ( this ) !; write(*,*) ' >GL> l_ok(60) = ',l_ok(50)
    l_ok(61) = ok_h_grid_ext   ( this ) !; write(*,*) ' >GL> l_ok(61) = ',l_ok(61)
    l_ok(62) = ok_h_grid_dry   ( this ) !; write(*,*) ' >GL> l_ok(62) = ',l_ok(62)
    l_ok(63) = ok_h_grid_isbnd ( this ) !; write(*,*) ' >GL> l_ok(63) = ',l_ok(63)
    l_ok(64) = ok_h_grid_isdam ( this ) !; write(*,*) ' >GL> l_ok(64) = ',l_ok(64)
    l_ok(65) = ok_h_grid_huu   ( this ) !; write(*,*) ' >GL> l_ok(65) = ',l_ok(65)
    l_ok(66) = ok_h_grid_hvu   ( this ) !; write(*,*) ' >GL> l_ok(66) = ',l_ok(66)
    l_ok(67) = ok_h_grid_hwu   ( this ) !; write(*,*) ' >GL> l_ok(67) = ',l_ok(67)
    l_ok(68) = ok_h_grid_dwlp  ( this ) !; write(*,*) ' >GL> l_ok(68) = ',l_ok(68)
    !
    ok  =   ALL( l_ok )
    nn  = COUNT( l_ok )
    !
  END FUNCTION ok_h_grid_object_0
  !
  !! Ermittle f&uuml;r das Objekt "this" die Datei-Varianten-Nummer <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_h_grid_variant_no_0 ( this ) &
       RESULT( ivar )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Nummer der aktuellen Variante <BR>
    !! -1 : nicht vorhanden
    INTEGER :: ivar
    !! Name der Function
    CHARACTER (LEN=23) , PARAMETER :: c_upname='get_h_grid_variant_no_0'
    !! logisches Feld
    LOGICAL :: l_ok(4)
    !! Z&auml;hler
    INTEGER :: i
    !
    ivar = -1
    i    = 0
    DO
       i = i + 1
       IF ( i > c_max_variants .OR. ivar /= -1 .OR. any_error ( ) ) EXIT
       l_ok(1) = ( TRIM( get_file_form  ( this%file ) ) == TRIM( c_variants_form(i)   ) )
       l_ok(2) = ( TRIM( get_file_access( this%file ) ) == TRIM( c_variants_access(i) ) )
       l_ok(3) = ( TRIM( get_file_delim ( this%file ) ) == TRIM( c_variants_delim(i)  ) )
       l_ok(4) = ( get_uppercase_char(TRIM(get_file_type(this%file))) == &
                   get_uppercase_char(TRIM(c_variants_type(i))) )
       ivar    = MERGE ( i, -1, ALL( l_ok(:) ) )
    END DO
    !
  END FUNCTION get_h_grid_variant_no_0
  !
  !! Pr&uuml;fe ob eine implementierte Datei-Variante vorliegt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_variant_no_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Ergebniswert: Erforderliche Datei-Variante ist implementiert <BR>
    LOGICAL :: ok
    !! Name der Function
    CHARACTER (LEN=22) , PARAMETER :: c_upname='ok_h_grid_variant_no_0'
    !
    ok = ( get_h_grid_variant_no ( this ) > 0 )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), -6001, c_upname, c_modname )
       CALL setup_error_act ( '<FortranFileType>'  , TRIM( get_file_type  ( this%file ) ) )
       CALL setup_error_act ( '<FortranFileForm>'  , TRIM( get_file_form  ( this%file ) ) )
       CALL setup_error_act ( '<FortranFileAccess>', TRIM( get_file_access( this%file ) ) )
       CALL setup_error_act ( '<FortranFileDelim>' , TRIM( get_file_delim ( this%file ) ) )
    END IF
    !
  END FUNCTION ok_h_grid_variant_no_0
  !
  !! Drucke den Inhalt eines Package-Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_object_0 ( this )
    !! Objekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Function
    CHARACTER (LEN=21), PARAMETER :: c_upname='print_h_grid_object_0'
    !! Statusvariable
    INTEGER :: stat
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) this%id, TRIM( this%name )
    !
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7001, c_upname, c_modname, stat )
    !
    IF ( no_error( ) ) CALL print_file ( this%file )
    !
    IF ( no_error( ) ) CALL print_h_grid_nv    ( this )
    IF ( no_error( ) ) CALL print_h_grid_ns    ( this )
    IF ( no_error( ) ) CALL print_h_grid_nsi   ( this )
    IF ( no_error( ) ) CALL print_h_grid_nsf   ( this )
    IF ( no_error( ) ) CALL print_h_grid_ne    ( this )
    IF ( no_error( ) ) CALL print_h_grid_nrand ( this )
    IF ( no_error( ) ) CALL print_h_grid_nptfr ( this )
    IF ( no_error( ) ) CALL print_h_grid_nptir ( this )
    IF ( no_error( ) ) CALL print_h_grid_nbc   ( this )
    IF ( no_error( ) ) CALL print_h_grid_nr    ( this )
    IF ( no_error( ) ) CALL print_h_grid_m     ( this )
    IF ( no_error( ) ) CALL print_h_grid_n     ( this )
    IF ( no_error( ) ) CALL print_h_grid_ncsize( this )
    IF ( no_error( ) ) CALL print_h_grid_time  ( this )
    IF ( no_error( ) ) CALL print_h_grid_hland ( this )
    IF ( no_error( ) ) CALL print_h_grid_angle ( this )
    IF ( no_error( ) ) CALL print_h_grid_text  ( this )
    IF ( no_error( ) ) CALL print_h_grid_xy    ( this )
    IF ( no_error( ) ) CALL print_h_grid_nen   ( this )
    IF ( no_error( ) ) CALL print_h_grid_irand ( this )
    IF ( no_error( ) ) CALL print_h_grid_ks    ( this )
    IF ( no_error( ) ) CALL print_h_grid_hv    ( this )
    IF ( no_error( ) ) CALL print_h_grid_jb    ( this )
    IF ( no_error( ) ) CALL print_h_grid_jt    ( this )
    IF ( no_error( ) ) CALL print_h_grid_is    ( this )
    IF ( no_error( ) ) CALL print_h_grid_je    ( this )
    IF ( no_error( ) ) CALL print_h_grid_ie    ( this )
    IF ( no_error( ) ) CALL print_h_grid_xs    ( this )
    IF ( no_error( ) ) CALL print_h_grid_xc    ( this )
    IF ( no_error( ) ) CALL print_h_grid_xg    ( this )
    IF ( no_error( ) ) CALL print_h_grid_dxmin ( this )
    IF ( no_error( ) ) CALL print_h_grid_dx    ( this )
    IF ( no_error( ) ) CALL print_h_grid_dy    ( this )
    IF ( no_error( ) ) CALL print_h_grid_dg    ( this )
    IF ( no_error( ) ) CALL print_h_grid_aa    ( this )
    IF ( no_error( ) ) CALL print_h_grid_hu    ( this )
    IF ( no_error( ) ) CALL print_h_grid_hw    ( this )
    IF ( no_error( ) ) CALL print_h_grid_ipobo ( this )
    IF ( no_error( ) ) CALL print_h_grid_b_ms  ( this )
    IF ( no_error( ) ) CALL print_h_grid_b_ss  ( this )
    IF ( no_error( ) ) CALL print_h_grid_b_s   ( this )
    IF ( no_error( ) ) CALL print_h_grid_b_v   ( this )
    IF ( no_error( ) ) CALL print_h_grid_b_t   ( this )
    IF ( no_error( ) ) CALL print_h_grid_enc   ( this )
    IF ( no_error( ) ) CALL print_h_grid_bnd   ( this )
    IF ( no_error( ) ) CALL print_h_grid_thd   ( this )
    IF ( no_error( ) ) CALL print_h_grid_lwl   ( this )
    IF ( no_error( ) ) CALL print_h_grid_ext   ( this )
    IF ( no_error( ) ) CALL print_h_grid_dry   ( this )
    IF ( no_error( ) ) CALL print_h_grid_isbnd ( this )
    IF ( no_error( ) ) CALL print_h_grid_isdam ( this )
    IF ( no_error( ) ) CALL print_h_grid_huu   ( this )
    IF ( no_error( ) ) CALL print_h_grid_hvu   ( this )
    IF ( no_error( ) ) CALL print_h_grid_hwu   ( this )
    IF ( no_error( ) ) CALL print_h_grid_dwlp  ( this )
    !
    IF ( no_error( ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7002, c_upname, c_modname, stat )
    END IF
    !
8000 FORMAT('# Beginn Objekt t_h_grid ---------------------------------------------',/&
            '# Objekt-Identifikationsnummer = ',I5,/&
            '# beschreibender Name          = ',A,/&
            '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT('# Ende   Objekt t_h_grid ---------------------------------------------')
    !
  END SUBROUTINE print_h_grid_object_0
  !
  !! Initialisieren eines Package-Objekts vom Typ "t_h_grid_list" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE init_h_grid_list_object_0 ( this )
    !! Package-Objekt des Typs "t_h_grid_list"
    TYPE (t_h_grid_list) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER  :: c_upname='init_h_grid_list_object_0'
    !! Statusvariable
    INTEGER :: stat
    !
    NULLIFY ( this%object )
    NULLIFY ( this%prev   )
    NULLIFY ( this%next   )
    !
    ALLOCATE ( this%object, STAT=stat )
    !
    IF ( stat /= 0 ) &
         CALL setup_error_act ( all_errors(:), 4002, c_upname, c_modname, stat )
    !
    IF ( no_error( ) ) CALL init_h_grid_object ( this%object )
    !
  END SUBROUTINE init_h_grid_list_object_0
  ! 
  !! logische Funktion um zu pr&uuml;fen, ob (Polygon,Kante) vom Typ "Water Level" ist [ Delft3D ]
  FUNCTION is_polyedge_water_level_2 ( this, i, j ) &
       RESULT( res )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Nummer des Polygons
    INTEGER         , INTENT(IN) :: i ! 
    !! Nummer der Kante in dem Polygon
    INTEGER         , INTENT(IN) :: j ! 
    !! R&uuml;ckgabewert
    LOGICAL :: res ! 
    !
    res = .false.
    IF ( ASSOCIATED( this%isbnd ) .AND. ASSOCIATED( this%bnd ) .AND. ASSOCIATED( this%ks ) ) THEN
       IF ( i >= 1 .AND. i <= SIZE( this%isbnd,1 ) .AND. j >= 1 .AND. j <= this%ks(i) ) THEN
          IF ( this%isbnd(i,j) > 0 ) THEN
             res = ( this%bnd(this%isbnd(i,j))%bdry_type == 'Z' )
          END IF
       END IF
    END IF
    !
  END FUNCTION is_polyedge_water_level_2
  !
  !! logische Funktion um zu pr&uuml;fen, ob (Polygon,Kante) vom Typ "Riemann" ist [ Delft3D ]
  FUNCTION is_polyedge_riemann_2 ( this, i, j ) &
       RESULT( res )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Nummer des Polygons
    INTEGER         , INTENT(IN) :: i ! 
    !! Nummer der Kante in dem Polygon
    INTEGER         , INTENT(IN) :: j ! 
    !! R&uuml;ckgabewert
    LOGICAL :: res ! 
    !
    res = .false.
    IF ( ASSOCIATED( this%isbnd ) .AND. ASSOCIATED( this%bnd ) .AND. ASSOCIATED( this%ks ) ) THEN
       IF ( i >= 1 .AND. i <= SIZE( this%isbnd,1 ) .AND. j >= 1 .AND. j <= this%ks(i) ) THEN
          IF ( this%isbnd(i,j) > 0 ) THEN
             res = ( this%bnd(this%isbnd(i,j))%bdry_type == 'R' )
          END IF
       END IF
    END IF
    !
  END FUNCTION is_polyedge_riemann_2
  !
  !! logische Funktion um zu pr&uuml;fen, ob (Polygon,Kante) vom Typ "Current" ist [ Delft3D ]
  FUNCTION is_polyedge_current_2 ( this, i, j ) &
       RESULT( res )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Nummer des Polygons
    INTEGER         , INTENT(IN) :: i ! 
    !! Nummer der Kante in dem Polygon
    INTEGER         , INTENT(IN) :: j ! 
    !! R&uuml;ckgabewert
    LOGICAL :: res ! 
    !
    res = .false.
    IF ( ASSOCIATED( this%isbnd ) .AND. ASSOCIATED( this%bnd ) .AND. ASSOCIATED( this%ks ) ) THEN
       IF ( i >= 1 .AND. i <= SIZE( this%isbnd,1 ) .AND. j >= 1 .AND. j <= this%ks(i) ) THEN
          IF ( this%isbnd(i,j) > 0 ) THEN
             res = ( this%bnd(this%isbnd(i,j))%bdry_type == 'C' )
          END IF
       END IF
    END IF
    !
  END FUNCTION is_polyedge_current_2
  !
  !! logische Funktion um zu pr&uuml;fen, ob (Polygon,Kante) vom Typ "Discharge" ist [ Delft3D ]
  FUNCTION is_polyedge_discharge_2 ( this, i, j ) &
       RESULT( res )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Nummer des Polygons
    INTEGER         , INTENT(IN) :: i ! 
    !! Nummer der Kante in dem Polygon
    INTEGER         , INTENT(IN) :: j ! 
    !! R&uuml;ckgabewert
    LOGICAL :: res ! 
    !
    res = .false.
    IF ( ASSOCIATED( this%isbnd ) .AND. ASSOCIATED( this%bnd ) .AND. ASSOCIATED( this%ks ) ) THEN
       IF ( i >= 1 .AND. i <= SIZE( this%isbnd,1 ) .AND. j >= 1 .AND. j <= this%ks(i) ) THEN
          IF ( this%isbnd(i,j) > 0 ) THEN
             res = ( this%bnd(this%isbnd(i,j))%bdry_type == 'Q' )
          END IF
       END IF
    END IF
    !
  END FUNCTION is_polyedge_discharge_2
  !
  !! logische Funktion um zu pr&uuml;fen, ob (Polygon,Kante) vom Typ "Total Discharge" ist [ Delft3D ]
  FUNCTION is_polyedge_total_discharge_2 ( this, i, j ) &
       RESULT( res )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Nummer des Polygons
    INTEGER         , INTENT(IN) :: i ! 
    !! Nummer der Kante in dem Polygon
    INTEGER         , INTENT(IN) :: j ! 
    !! R&uuml;ckgabewert
    LOGICAL :: res ! 
    !
    res = .false.
    IF ( ASSOCIATED( this%isbnd ) .AND. ASSOCIATED( this%bnd ) .AND. ASSOCIATED( this%ks ) ) THEN
       IF ( i >= 1 .AND. i <= SIZE( this%isbnd,1 ) .AND. j >= 1 .AND. j <= this%ks(i) ) THEN
          IF ( this%isbnd(i,j) > 0 ) THEN
             res = ( this%bnd(this%isbnd(i,j))%bdry_type == 'T' )
          END IF
       END IF
    END IF
    !
  END FUNCTION is_polyedge_total_discharge_2
  !
  !! logische Funktion um zu pr&uuml;fen, ob (Polygon,Kante) vom Typ "Thin Dam" ist [ Delft3D ]
  FUNCTION is_polyedge_thin_dam_2 ( this, i, j ) &
       RESULT( res )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Nummer des Polygons
    INTEGER         , INTENT(IN) :: i ! 
    !! Nummer der Kante in dem Polygon
    INTEGER         , INTENT(IN) :: j ! 
    !! R&uuml;ckgabewert
    LOGICAL :: res ! 
    !
    res = .false.
    IF ( ASSOCIATED( this%isdam ) .AND. ASSOCIATED( this%thd ) .AND. ASSOCIATED( this%ks ) ) THEN
       IF ( i >= 1 .AND. i <= SIZE( this%isdam,1 ) .AND. j >= 1 .AND. j <= this%ks(i) ) THEN
          IF ( this%isdam(i,j,1) > 0 .AND. this%isdam(i,j,2) == 1 ) THEN
             SELECT CASE ( j )
             CASE ( 1,3 )
                res = ( this%thd(this%isdam(i,j,1))%type == 'V' )
             CASE ( 2,4 )
                res = ( this%thd(this%isdam(i,j,1))%type == 'U' )
             END SELECT
          END IF
       END IF
    END IF
    !
  END FUNCTION is_polyedge_thin_dam_2
  !
  !! logische Funktion um zu pr&uuml;fen, ob (Polygon,Kante) vom Typ "Local Weir" ist [ Delft3D ]
  FUNCTION is_polyedge_local_weir_2 ( this, i, j ) &
       RESULT( res )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Nummer des Polygons
    INTEGER         , INTENT(IN) :: i ! 
    !! Nummer der Kante in dem Polygon
    INTEGER         , INTENT(IN) :: j ! 
    !! R&uuml;ckgabewert
    LOGICAL :: res ! 
    !
    res = .false.
    IF ( ASSOCIATED( this%isdam ) .AND. ASSOCIATED( this%lwl ) .AND. ASSOCIATED( this%ks ) ) THEN
       IF ( i >= 1 .AND. i <= SIZE( this%isdam,1 ) .AND. j >= 1 .AND. j <= this%ks(i) ) THEN
          IF ( this%isdam(i,j,1) > 0 .AND. this%isdam(i,j,2) == 2 ) THEN
             SELECT CASE ( j )
             CASE ( 1,3 )
                res = ( this%lwl(this%isdam(i,j,1))%type == 'V' )
             CASE ( 2,4 )
                res = ( this%lwl(this%isdam(i,j,1))%type == 'U' )
             END SELECT
          END IF
       END IF
    END IF
    !
  END FUNCTION is_polyedge_local_weir_2
  !
  !! logische Funktion um zu pr&uuml;fen, ob (Polygon,Kante) vom Typ "2D-Weir" ist [ Delft3D ]
  FUNCTION is_polyedge_2d_weir_2 ( this, i, j ) &
       RESULT( res )
    !! aktuelles Objekt
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Nummer des Polygons
    INTEGER         , INTENT(IN) :: i ! 
    !! Nummer der Kante in dem Polygon
    INTEGER         , INTENT(IN) :: j ! 
    !! R&uuml;ckgabewert
    LOGICAL :: res ! 
    !
    res = .false.
    IF ( ASSOCIATED( this%isdam ) .AND. ASSOCIATED( this%ext ) .AND. ASSOCIATED( this%ks ) ) THEN
       IF ( i >= 1 .AND. i <= SIZE( this%isdam,1 ) .AND. j >= 1 .AND. j <= this%ks(i) ) THEN
          IF ( this%isdam(i,j,1) > 0 .AND. this%isdam(i,j,2) == 3 ) THEN
             SELECT CASE ( j )
             CASE ( 1,3 )
                res = ( this%ext(this%isdam(i,j,1))%type == 'V' )
             CASE ( 2,4 )
                res = ( this%ext(this%isdam(i,j,1))%type == 'U' )
             END SELECT
          END IF
       END IF
    END IF
    !
  END FUNCTION is_polyedge_2d_weir_2
  ! 
  !! Umnummerieren von je, jb und jt falls wenigstens ein je(:,1) <= 0 ist <BR>
  !! Unterprogramm erzeugt keine Fehlermeldungen
  SUBROUTINE swap_je_jb_jt_d ( this )
    !! aktuelles Datenobjekt
    TYPE (t_h_grid) , POINTER :: this ! 
    ! lokale Variablen
    INTEGER :: j, m ! 
    !
    IF ( ASSOCIATED(this%jb) .AND. ASSOCIATED(this%jt) .AND. ASSOCIATED(this%je) ) THEN
       DO j=1,SIZE(this%je,1)
          IF ( this%je(j,1) > 0 ) CYCLE
          m            = this%je(j,1)
          this%je(j,1) = this%je(j,2)
          this%je(j,2) = m
          m            = this%jb(j)
          this%jb(j)   = this%jt(j)
          this%jt(j)   = m
       END DO
    END IF
    !
  END SUBROUTINE swap_je_jb_jt_d
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
  !! Allokieren der Komponente "nv" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_nv_0 ( this )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='alloc_h_grid_nv_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%nv, STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'nv' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_nv_0
  !
  !! Allokieren der Komponente "ns" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_ns_0 ( this )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='alloc_h_grid_ns_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%ns, STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'ns' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_ns_0
  !
  !! Allokieren der Komponente "nsi" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_nsi_0 ( this )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='alloc_h_grid_nsi_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%nsi, STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'nsi' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_nsi_0
  !
  !! Allokieren der Komponente "nsf" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_nsf_0 ( this )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='alloc_h_grid_nsf_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%nsf, STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'nsf' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_nsf_0
  !
  !! Allokieren der Komponente "ne" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_ne_0 ( this )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='alloc_h_grid_ne_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%ne, STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'ne' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_ne_0
  !
  !! Allokieren der Komponente "xy" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_xy_2 ( this, idim1, idim2 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Dimension f&uuml;r Komponente "xy"
    INTEGER         , INTENT(IN)    :: idim1, idim2 ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='alloc_h_grid_xy_2'
    !! Statusvariable
    INTEGER :: stat
    !
    ALLOCATE ( this%xy(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'xy(:,:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_xy_2
  !
  !! Allokieren der Komponente "nen" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_nen_2 ( this, idim1, idim2 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Dimension f&uuml;r Komponente "nen"
    INTEGER         , INTENT(IN)    :: idim1, idim2 ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='alloc_h_grid_nen_2'
    !! Statusvariable
    INTEGER :: stat
    !
    ALLOCATE ( this%nen(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'nen(:,:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_nen_2
  !
  !! Allokieren der Komponente "irand" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_irand_1 ( this, idim )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Dimension f&uuml;r Komponente "irand"
    INTEGER         , INTENT(IN)    :: idim
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='alloc_h_grid_irand_1'
    !! Statusvariable
    INTEGER :: stat
    !
    ALLOCATE ( this%irand(idim), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'irand(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_irand_1
  !
  !! Allokieren der Komponente "ks" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_ks_1 ( this, idim )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Dimension f&uuml;r Komponente "ks"
    INTEGER         , INTENT(IN)    :: idim
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='alloc_h_grid_ks_1'
    !! Statusvariable
    INTEGER :: stat
    !
    ALLOCATE ( this%ks(idim), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'ks(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_ks_1
  !
  !! Allokieren der Komponente "hv" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_hv_1 ( this, idim )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Dimension f&uuml;r Komponente "hv"
    INTEGER         , INTENT(IN) :: idim
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='alloc_h_grid_hv_1'
    !! Statusvariable
    INTEGER :: stat
    !
    ALLOCATE ( this%hv(idim), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'hv(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_hv_1
  !
  !! Allokieren der Komponente "nrand" (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_nrand_0 ( this )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='alloc_h_grid_nrand_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%nrand, STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'nrand' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_nrand_0
  !
  !! Allokieren der Komponente "nptfr" (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_nptfr_0 ( this )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='alloc_h_grid_nptfr_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%nptfr, STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'nptfr' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_nptfr_0
  !
  !! Allokieren der Komponente "nptir" (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_nptir_0 ( this )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='alloc_h_grid_nptir_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%nptir, STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'nptir' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_nptir_0
  !
  !! Allokieren der Komponente "time" (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_time_0 ( this )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='alloc_h_grid_time_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%time, STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'time' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_time_0
  !
  !! Allokieren der Komponente "nbc" (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_nbc_0 ( this )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='alloc_h_grid_nbc_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%nbc, STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'nbc' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_nbc_0
  !
  !! Allokieren der Komponente "hland" (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_hland_0 ( this )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='alloc_h_grid_hland_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%hland, STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'hland' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_hland_0
  !
  !! Allokieren der Komponente "angle" (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_angle_0 ( this )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='alloc_h_grid_angle_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%angle, STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'angle' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_angle_0
  !
  !! Allokieren der Komponente "text" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_text_1 ( this, idim )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Dimension f&uuml;r Komponente "text"
    INTEGER         , INTENT(IN)    :: idim ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='alloc_h_grid_text_1' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%text(idim), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'text(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_text_1
  !
  !! Allokieren der Komponente "jb" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_jb_1 ( this, idim )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Dimension f&uuml;r Komponente "jb"
    INTEGER         , INTENT(IN) :: idim
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='alloc_h_grid_jb_1'
    !! Statusvariable
    INTEGER :: stat
    !
    ALLOCATE ( this%jb(idim), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'jb(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_jb_1
  !
  !! Allokieren der Komponente "jt" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_jt_1 ( this, idim )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Dimension f&uuml;r Komponente "jt"
    INTEGER         , INTENT(IN) :: idim
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='alloc_h_grid_jt_1'
    !! Statusvariable
    INTEGER :: stat
    !
    ALLOCATE ( this%jt(idim), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'jt(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_jt_1
  !
  !! Allokieren der Komponente "is" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_is_2 ( this, idim1, idim2 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Dimension f&uuml;r Komponente "is"
    INTEGER         , INTENT(IN)    :: idim1, idim2 ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='alloc_h_grid_is_2'
    !! Statusvariable
    INTEGER :: stat
    !
    ALLOCATE ( this%is(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'is(:,:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_is_2
  !
  !! Allokieren der Komponente "je" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_je_2 ( this, idim1, idim2 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Dimension f&uuml;r Komponente "je"
    INTEGER         , INTENT(IN)    :: idim1, idim2 ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='alloc_h_grid_je_2'
    !! Statusvariable
    INTEGER :: stat
    !
    ALLOCATE ( this%je(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'je(:,:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_je_2
  !
  !! Allokieren der Komponente "ie" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_ie_2 ( this, idim1, idim2 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Dimension f&uuml;r Komponente "ie"
    INTEGER         , INTENT(IN)    :: idim1, idim2 ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='alloc_h_grid_ie_2'
    !! Statusvariable
    INTEGER :: stat
    !
    ALLOCATE ( this%ie(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'ie(:,:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_ie_2
  !
  !! Allokieren der Komponente "xs" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_xs_2 ( this, idim1, idim2 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Dimension 1 f&uuml;r Komponente "xs"
    INTEGER         , INTENT(IN)    :: idim1
    !! Dimension 2 f&uuml;r Komponente "xs"
    INTEGER         , INTENT(IN)    :: idim2
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='alloc_h_grid_xs_2'
    !! Statusvariable
    INTEGER :: stat
    !
    ALLOCATE ( this%xs(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'xs(:,:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_xs_2
  !
  !! Allokieren der Komponente "xc" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_xc_2 ( this, idim1, idim2 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Dimension 1 f&uuml;r Komponente "xc"
    INTEGER         , INTENT(IN)    :: idim1 ! 
    !! Dimension 2 f&uuml;r Komponente "xc"
    INTEGER         , INTENT(IN)    :: idim2 ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='alloc_h_grid_xc_2'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%xc(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'xc(:,:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_xc_2
  !
  !! Allokieren der Komponente "xg" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_xg_2 ( this, idim1, idim2 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Dimension 1 f&uuml;r Komponente "xg"
    INTEGER         , INTENT(IN)    :: idim1 ! 
    !! Dimension 2 f&uuml;r Komponente "xg"
    INTEGER         , INTENT(IN)    :: idim2 ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='alloc_h_grid_xg_2'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%xg(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'xg(:,:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_xg_2
  !
  !! Allokieren der Komponente "dx" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_dx_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Dimension f&uuml;r Komponente "dx"
    INTEGER         , INTENT(IN)    :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='alloc_h_grid_dx_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%dx(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'dx(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_dx_1
  !
  !! Allokieren der Komponente "dy" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_dy_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Dimension f&uuml;r Komponente "dy"
    INTEGER         , INTENT(IN)    :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='alloc_h_grid_dy_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%dy(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'dy(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_dy_1
  !
  !! Allokieren der Komponente "dg" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_dg_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Dimension f&uuml;r Komponente "dg"
    INTEGER         , INTENT(IN)    :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='alloc_h_grid_dg_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%dg(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'dg(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_dg_1
  !
  !! Allokieren der Komponente "aa" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_aa_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Dimension f&uuml;r Komponente "aa"
    INTEGER         , INTENT(IN)    :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='alloc_h_grid_aa_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%aa(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'aa(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_aa_1
  !
  !! Allokieren der Komponente "hu" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_hu_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Dimension f&uuml;r Komponente "hu"
    INTEGER         , INTENT(IN)    :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='alloc_h_grid_hu_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%hu(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'hu(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_hu_1
  !
  !! Allokieren der Komponente "hw" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_hw_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Dimension f&uuml;r Komponente "hw"
    INTEGER         , INTENT(IN)    :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='alloc_h_grid_hw_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%hw(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'hw(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_hw_1
  !
  !! Allokieren der Komponente "nr" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_nr_0 ( this )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='alloc_h_grid_nr_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%nr, STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'nr' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_nr_0
  !
  !! Allokieren der Komponente "ncsize" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_ncsize_0 ( this )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER  :: c_upname='alloc_h_grid_ncsize_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%ncsize, STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'nsize' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_ncsize_0
  !
  !! Allokieren der Komponente "ipobo" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_ipobo_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Dimension f&uuml;r Komponente "ipobo"
    INTEGER         , INTENT(IN)    :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='alloc_h_grid_ipobo_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%ipobo(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'ipobo(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_ipobo_1
  !
  !! Allokieren der Komponente "knolg" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_knolg_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Dimension f&uuml;r Komponente "knolg"
    INTEGER         , INTENT(IN)    :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='alloc_h_grid_knolg_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%knolg(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'knolg(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_knolg_1
  !
  !! Allokieren der Komponente "quant" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_quant_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER      :: this
    !! Dimension f&uuml;r Komponente "quant"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='alloc_h_grid_quant_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%quant(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'quant(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_quant_1
  !
  !! Allokieren der Komponente "xyz" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_xyz_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER      :: this
    !! Dimension f&uuml;r Komponente "xyz"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='alloc_h_grid_xyz_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%xyz(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'xyz(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_xyz_1
  !
  !! Allokieren der Komponente "ele" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_ele_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER      :: this
    !! Dimension f&uuml;r Komponente "ele"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='alloc_h_grid_ele_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%ele(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'ele(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_ele_1
  !
  !! Allokieren der Komponente "ind" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_ind_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER      :: this
    !! Dimension f&uuml;r Komponente "ind"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='alloc_h_grid_ind_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%ind(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'ind(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_ind_1
  !
  !! Allokieren der Komponente "dope" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_dope_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER      :: this
    !! Dimension f&uuml;r Komponente "dope"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='alloc_h_grid_dope_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%dope(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'dope(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_dope_1
  !
  !! Allokieren der Komponente "exch" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_exch_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER      :: this
    !! Dimension f&uuml;r Komponente "exch"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='alloc_h_grid_exch_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%exch(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'exch(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_exch_1
  !
  !! Allokieren der Komponente "span" (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_span_0 ( this )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER      :: this
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='alloc_h_grid_span_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%span, STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'span' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_span_0
  !
  !! Allokieren der Komponente "stamp" (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_stamp_0 ( this )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER      :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='alloc_h_grid_stamp_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%stamp, STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'stamp' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_stamp_0
  !
  !! Allokieren der Komponente "space" (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_space_0 ( this )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER      :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='alloc_h_grid_space_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%space, STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'space' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_space_0
  !
  !! Allokieren der Komponente "dxmin" (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_dxmin_0 ( this )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='alloc_h_grid_dxmin_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%dxmin, STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'dxmin' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_dxmin_0
  !
  !! Allokieren der Komponente "b_ms(:)" (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_b_ms_1 ( this, idim )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Dimension des Feldes
    INTEGER , INTENT(IN)      :: idim ! 
    !! Name des Unterprogramms
    CHARACTER (LEN=19), PARAMETER  :: c_upname='alloc_h_grid_b_ms_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%b_ms(idim), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'b_ms(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_b_ms_1
  !
  !! Allokieren der Komponente "b_ss(:)" (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_b_ss_1 ( this, idim )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Dimension des Feldes
    INTEGER , INTENT(IN)      :: idim ! 
    !! Name des Unterprogramms
    CHARACTER (LEN=19), PARAMETER  :: c_upname='alloc_h_grid_b_ss_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%b_ss(idim), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'b_ss(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_b_ss_1
  !
  !! Allokieren der Komponente "b_s(:)" (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_b_s_1 ( this, idim )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Dimension des Feldes
    INTEGER , INTENT(IN)      :: idim ! 
    !! Name des Unterprogramms
    CHARACTER (LEN=18), PARAMETER  :: c_upname='alloc_h_grid_b_s_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%b_s(idim), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'b_s(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_b_s_1
  !
  !! Allokieren der Komponente "b_v(:)" (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_b_v_1 ( this, idim )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Dimension des Feldes
    INTEGER , INTENT(IN)      :: idim ! 
    !! Name des Unterprogramms
    CHARACTER (LEN=18), PARAMETER  :: c_upname='alloc_h_grid_b_v_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%b_v(idim), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'b_v(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_b_v_1
  !
  !! Allokieren der Komponente "b_t(:)" (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_b_t_1 ( this, idim )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Dimension des Feldes
    INTEGER , INTENT(IN)      :: idim ! 
    !! Name des Unterprogramms
    CHARACTER (LEN=18), PARAMETER  :: c_upname='alloc_h_grid_b_t_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%b_t(idim), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'b_t(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_b_t_1
  !
  !! Allokieren der Komponente "m" (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_m_0 ( this )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name des Unterprogramms
    CHARACTER (LEN=16), PARAMETER  :: c_upname='alloc_h_grid_m_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%m, STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'm' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_m_0
  !
  !! Allokieren der Komponente "n" (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_n_0 ( this )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this ! 
    !! Name des Unterprogramms
    CHARACTER (LEN=16), PARAMETER  :: c_upname='alloc_h_grid_n_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%n, STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'n' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_n_0
  !
  !! Allokieren der Komponente "enc(:,:)" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_enc_2 ( this, idim1, idim2 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Dimension 1 f&uuml;r Komponente "enc"
    INTEGER         , INTENT(IN)    :: idim1 ! 
    !! Dimension 2 f&uuml;r Komponente "enc"
    INTEGER         , INTENT(IN)    :: idim2 ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='alloc_h_grid_enc_2'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%enc(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'enc(:,:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_enc_2
  !
  !! Allokieren der Komponente "dry(:,:)" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_dry_2 ( this, idim1, idim2 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Dimension 1 f&uuml;r Komponente "dry"
    INTEGER         , INTENT(IN)    :: idim1 ! 
    !! Dimension 2 f&uuml;r Komponente "dry"
    INTEGER         , INTENT(IN)    :: idim2 ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='alloc_h_grid_dry_2'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%dry(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'dry(:,:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_dry_2
  !
  !! Allokieren der Komponente "isbnd(:,:)" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_isbnd_2 ( this, idim1, idim2 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Dimension 1 f&uuml;r Komponente "isbnd"
    INTEGER         , INTENT(IN)    :: idim1 ! 
    !! Dimension 2 f&uuml;r Komponente "isbnd"
    INTEGER         , INTENT(IN)    :: idim2 ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='alloc_h_grid_isbnd_2'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%isbnd(idim1,idim2), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'isbnd(:,:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_isbnd_2
  !
  !! Allokieren der Komponente "isdam(:,:,:)" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_isdam_3 ( this, idim1, idim2, idim3 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER :: this
    !! Dimension 1 f&uuml;r Komponente "isdam"
    INTEGER         , INTENT(IN)    :: idim1 ! 
    !! Dimension 2 f&uuml;r Komponente "isdam"
    INTEGER         , INTENT(IN)    :: idim2 ! 
    !! Dimension 3 f&uuml;r Komponente "isdam"
    INTEGER         , INTENT(IN)    :: idim3 ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='alloc_h_grid_isdam_3'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%isdam(idim1,idim2,idim3), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'isdam(:,:,:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_isdam_3
  !
  !! Allokieren der Komponente "bnd(:)" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_bnd_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER     :: this
    !! Dimension 1 f&uuml;r Komponente "bnd"
    INTEGER         , INTENT(IN)  :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='alloc_h_grid_bnd_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%bnd(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'bnd(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_bnd_1
  !
  !! Allokieren der Komponente "thd(:)" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_thd_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER     :: this
    !! Dimension 1 f&uuml;r Komponente "thd"
    INTEGER         , INTENT(IN)  :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='alloc_h_grid_thd_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%thd(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'thd(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_thd_1
  !
  !! Allokieren der Komponente "lwl(:)" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_lwl_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER     :: this
    !! Dimension 1 f&uuml;r Komponente "lwl"
    INTEGER         , INTENT(IN)  :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='alloc_h_grid_lwl_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%lwl(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'lwl(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_lwl_1
  !
  !! Allokieren der Komponente "ext(:)" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_ext_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER     :: this
    !! Dimension 1 f&uuml;r Komponente "ext"
    INTEGER         , INTENT(IN)  :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='alloc_h_grid_ext_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%ext(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'ext(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_ext_1
  !
  !! Allokieren der Komponente "huu" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_huu_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER      :: this  ! 
    !! Dimension f&uuml;r Komponente "huu"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='alloc_h_grid_huu_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%huu(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'huu(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_huu_1
  !
  !! Allokieren der Komponente "hvu" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_hvu_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER      :: this  ! 
    !! Dimension f&uuml;r Komponente "hvu"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='alloc_h_grid_hvu_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%hvu(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'hvu(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_hvu_1
  !
  !! Allokieren der Komponente "hwu" (Feld) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_hwu_1 ( this, idim1 )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER      :: this  ! 
    !! Dimension f&uuml;r Komponente "hwu"
    INTEGER         , INTENT(IN)   :: idim1 ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='alloc_h_grid_hwu_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%hwu(idim1), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'hwu(:)' )
    END IF
    !
  END SUBROUTINE alloc_h_grid_hwu_1
  !
  !! Allokieren der Komponente "dwlp" (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_h_grid_dwlp_0 ( this )
    !! Package-Datenobjekt des Typs "t_h_grid"
    TYPE (t_h_grid) , POINTER      :: this  ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='alloc_h_grid_dwlp_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    ALLOCATE ( this%dwlp, STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 3010, c_upname, c_modname, stat )
       CALL setup_error_act ( '<component-name>', 'dwlp' )
    ELSE
       this%dwlp = REPEAT( ' ', c_len_dwlp )
    END IF
    !
  END SUBROUTINE alloc_h_grid_dwlp_0
  !
  !! De-Allokieren der Komponente "nv" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_nv_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='dealloc_h_grid_nv_0'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%nv ) ) THEN
       DEALLOCATE( this%nv, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'nv' )
       END IF
       NULLIFY( this%nv )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_nv_0
  !
  !! De-Allokieren der Komponente "ns" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_ns_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='dealloc_h_grid_ns_0'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%ns ) ) THEN
       DEALLOCATE( this%ns, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'ns' )
       END IF
       NULLIFY( this%ns )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_ns_0
  !
  !! De-Allokieren der Komponente "nsi" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_nsi_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='dealloc_h_grid_nsi_0'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%nsi ) ) THEN
       DEALLOCATE( this%nsi, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'nsi' )
       END IF
       NULLIFY( this%nsi )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_nsi_0
  !
  !! De-Allokieren der Komponente "nsf" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_nsf_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='dealloc_h_grid_nsf_0'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%nsf ) ) THEN
       DEALLOCATE( this%nsf, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'nsf' )
       END IF
       NULLIFY( this%nsf )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_nsf_0
  !
  !! De-Allokieren der Komponente "ne" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_ne_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='dealloc_h_grid_ne_0'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%ne ) ) THEN
       DEALLOCATE( this%ne, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'ne' )
       END IF
       NULLIFY( this%ne )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_ne_0
  !
  !! De-Allokieren der Komponente "xy" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_xy_2 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='dealloc_h_grid_xy_2'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%xy ) ) THEN
       DEALLOCATE( this%xy, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'xy(:,:)' )
       END IF
       NULLIFY( this%xy )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_xy_2
  !
  !! De-Allokieren der Komponente "nen" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_nen_2 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='dealloc_h_grid_nen_2'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%nen ) ) THEN
       DEALLOCATE( this%nen, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'nen(:,:)' )
       END IF
       NULLIFY( this%nen )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_nen_2
  !
  !! De-Allokieren der Komponente "irand" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_irand_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER  :: c_upname='dealloc_h_grid_irand_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%irand ) ) THEN
       DEALLOCATE( this%irand, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'irand(:)' )
       END IF
       NULLIFY( this%irand )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_irand_1
  !
  !! De-Allokieren der Komponente "ks" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_ks_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='dealloc_h_grid_ks_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%ks ) ) THEN
       DEALLOCATE( this%ks, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'ks(:)' )
       END IF
       NULLIFY( this%ks )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_ks_1
  !
  !! De-Allokieren der Komponente "hv" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_hv_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='dealloc_h_grid_hv_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%hv ) ) THEN
       DEALLOCATE( this%hv, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'hv(:)' )
       END IF
       NULLIFY( this%hv )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_hv_1
  !
  !! De-Allokieren der Komponente "nrand" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_nrand_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER  :: c_upname='dealloc_h_grid_nrand_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%nrand ) ) THEN
       DEALLOCATE( this%nrand, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'nrand' )
       END IF
       NULLIFY( this%nrand )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_nrand_0
  !
  !! De-Allokieren der Komponente "nptfr" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_nptfr_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER  :: c_upname='dealloc_h_grid_nptfr_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%nptfr ) ) THEN
       DEALLOCATE( this%nptfr, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'nptfr' )
       END IF
       NULLIFY( this%nptfr )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_nptfr_0
  !
  !! De-Allokieren der Komponente "nptir" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_nptir_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER  :: c_upname='dealloc_h_grid_nptir_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%nptir ) ) THEN
       DEALLOCATE( this%nptir, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'nptir' )
       END IF
       NULLIFY( this%nptir )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_nptir_0
  !
  !! De-Allokieren der Komponente "time" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_time_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER  :: c_upname='dealloc_h_grid_time_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%time ) ) THEN
       DEALLOCATE( this%time, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'time' )
       END IF
       NULLIFY( this%time )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_time_0
  !
  !! De-Allokieren der Komponente "nbc" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_nbc_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='dealloc_h_grid_nbc_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%nbc ) ) THEN
       DEALLOCATE( this%nbc, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'nbc' )
       END IF
       NULLIFY( this%nbc )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_nbc_0
  !
  !! De-Allokieren der Komponente "hland" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_hland_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER  :: c_upname='dealloc_h_grid_hland_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%hland ) ) THEN
       DEALLOCATE( this%hland, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'hland' )
       END IF
       NULLIFY( this%hland )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_hland_0
  !
  !! De-Allokieren der Komponente "angle" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_angle_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER  :: c_upname='dealloc_h_grid_angle_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%angle ) ) THEN
       DEALLOCATE( this%angle, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'angle' )
       END IF
       NULLIFY( this%angle )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_angle_0
  !
  !! De-Allokieren der Komponente "text" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_text_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER  :: c_upname='dealloc_h_grid_text_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%text ) ) THEN
       DEALLOCATE( this%text, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'text(:)' )
       END IF
       NULLIFY( this%text )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_text_1
  !
  !! De-Allokieren der Komponente "jb" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_jb_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='dealloc_h_grid_jb_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%jb ) ) THEN
       DEALLOCATE( this%jb, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'jb(:)' )
       END IF
       NULLIFY( this%jb )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_jb_1
  !
  !! De-Allokieren der Komponente "jt" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_jt_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='dealloc_h_grid_jt_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%jt ) ) THEN
       DEALLOCATE( this%jt, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'jt(:)' )
       END IF
       NULLIFY( this%jt )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_jt_1
  !
  !! De-Allokieren der Komponente "is" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_is_2 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='dealloc_h_grid_is_2'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%is ) ) THEN
       DEALLOCATE( this%is, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'is(:,:)' )
       END IF
       NULLIFY( this%is )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_is_2
  !
  !! De-Allokieren der Komponente "je" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_je_2 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='dealloc_h_grid_je_2'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%je ) ) THEN
       DEALLOCATE( this%je, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'je(:,:)' )
       END IF
       NULLIFY( this%je )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_je_2
  !
  !! De-Allokieren der Komponente "ie" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_ie_2 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='dealloc_h_grid_ie_2'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%ie ) ) THEN
       DEALLOCATE( this%ie, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'ie(:,:)' )
       END IF
       NULLIFY( this%ie )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_ie_2
  !
  !! De-Allokieren der Komponente "xs" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_xs_2 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='dealloc_h_grid_xs_2'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%xs ) ) THEN
       DEALLOCATE( this%xs, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'xs(:,:)' )
       END IF
       NULLIFY( this%xs )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_xs_2
  !
  !! De-Allokieren der Komponente "xc" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_xc_2 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='dealloc_h_grid_xc_2'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%xc ) ) THEN
       DEALLOCATE( this%xc, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'xc(:,:)' )
       END IF
       NULLIFY( this%xc )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_xc_2
  !
  !! De-Allokieren der Komponente "xg" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_xg_2 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='dealloc_h_grid_xg_2'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%xg ) ) THEN
       DEALLOCATE( this%xg, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'xg(:,:)' )
       END IF
       NULLIFY( this%xg )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_xg_2
  !
  !! De-Allokieren der Komponente "dx" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_dx_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='dealloc_h_grid_dx_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%dx ) ) THEN
       DEALLOCATE( this%dx, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'dx(:)' )
       END IF
       NULLIFY( this%dx )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_dx_1
  !
  !! De-Allokieren der Komponente "dy" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_dy_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='dealloc_h_grid_dy_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%dy ) ) THEN
       DEALLOCATE( this%dy, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'dy(:)' )
       END IF
       NULLIFY( this%dy )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_dy_1
  !
  !! De-Allokieren der Komponente "dg" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_dg_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='dealloc_h_grid_dg_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%dg ) ) THEN
       DEALLOCATE( this%dg, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'dg(:)' )
       END IF
       NULLIFY( this%dg )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_dg_1
  !
  !! De-Allokieren der Komponente "aa" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_aa_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='dealloc_h_grid_aa_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%aa ) ) THEN
       DEALLOCATE( this%aa, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'aa(:)' )
       END IF
       NULLIFY( this%aa )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_aa_1
  !
  !! De-Allokieren der Komponente "hu" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_hu_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='dealloc_h_grid_hu_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%hu ) ) THEN
       DEALLOCATE( this%hu, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'hu(:)' )
       END IF
       NULLIFY( this%hu )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_hu_1
  !
  !! De-Allokieren der Komponente "hw" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_hw_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='dealloc_h_grid_hw_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%hw ) ) THEN
       DEALLOCATE( this%hw, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'hw(:)' )
       END IF
       NULLIFY( this%hw )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_hw_1
  !
  !! De-Allokieren der Komponente "ipobo" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_ipobo_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER  :: c_upname='dealloc_h_grid_ipobo_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%ipobo ) ) THEN
       DEALLOCATE( this%ipobo, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'ipobo(:)' )
       END IF
       NULLIFY( this%ipobo )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_ipobo_1
  !
  !! De-Allokieren der Komponente "knolg" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_knolg_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER  :: c_upname='dealloc_h_grid_knolg_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%knolg ) ) THEN
       DEALLOCATE( this%knolg, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'knolg(:)' )
       END IF
       NULLIFY( this%knolg )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_knolg_1
  !
  !! De-Allokieren der Komponente "quant" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_quant_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER  :: c_upname='dealloc_h_grid_quant_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%quant ) ) THEN
       CALL kill_omi_quant ( this%quant )
       DEALLOCATE( this%quant, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'quant(:)' )
       END IF
       NULLIFY( this%quant )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_quant_1
  !
  !! De-Allokieren der Komponente "xyz" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_xyz_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='dealloc_h_grid_xyz_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%xyz ) ) THEN
       CALL kill_omi_xyz ( this%xyz )
       DEALLOCATE( this%xyz, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'xyz(:)' )
       END IF
       NULLIFY( this%xyz )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_xyz_1
  !
  !! De-Allokieren der Komponente "ele" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_ele_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='dealloc_h_grid_ele_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%ele ) ) THEN
       CALL kill_omi_ele ( this%ele )
       DEALLOCATE( this%ele, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'ele(:)' )
       END IF
       NULLIFY( this%ele )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_ele_1
  !
  !! De-Allokieren der Komponente "ind" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_ind_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='dealloc_h_grid_ind_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%ind ) ) THEN
       CALL kill_omi_ind ( this%ind )
       DEALLOCATE( this%ind, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'ind(:)' )
       END IF
       NULLIFY( this%ind )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_ind_1
  !
  !! De-Allokieren der Komponente "dope" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_dope_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER  :: c_upname='dealloc_h_grid_dope_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%dope ) ) THEN
       CALL kill_omi_dope ( this%dope )
       DEALLOCATE( this%dope, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'dope(:)' )
       END IF
       NULLIFY( this%dope )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_dope_1
  !
  !! De-Allokieren der Komponente "exch" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_exch_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER  :: c_upname='dealloc_h_grid_exch_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%exch ) ) THEN
       CALL kill_omi_exch ( this%exch )
       DEALLOCATE( this%exch, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'exch(:)' )
       END IF
       NULLIFY( this%exch )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_exch_1
  !
  !! De-Allokieren der Komponente "span" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_span_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER  :: c_upname='dealloc_h_grid_span_0'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%span ) ) THEN
       CALL kill_omi_span ( this%span )
       DEALLOCATE( this%span, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'span' )
       END IF
       NULLIFY( this%span )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_span_0
  !
  !! De-Allokieren der Komponente "stamp" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_stamp_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER  :: c_upname='dealloc_h_grid_stamp_0'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%stamp ) ) THEN
       CALL kill_omi_stamp ( this%stamp )
       DEALLOCATE( this%stamp, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'stamp' )
       END IF
       NULLIFY( this%stamp )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_stamp_0
  !
  !! De-Allokieren der Komponente "space" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_space_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER  :: c_upname='dealloc_h_grid_space_0'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%space ) ) THEN
       CALL kill_omi_space ( this%space )
       DEALLOCATE( this%space, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'space' )
       END IF
       NULLIFY( this%space )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_space_0
  !
  !! De-Allokieren der Komponente "dxmin" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_dxmin_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER  :: c_upname='dealloc_h_grid_dxmin_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%dxmin ) ) THEN
       DEALLOCATE( this%dxmin, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'dxmin' )
       END IF
       NULLIFY( this%dxmin )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_dxmin_0
  !
  !! De-Allokieren der Komponente "nr" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_nr_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='dealloc_h_grid_nr_0'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%nr ) ) THEN
       DEALLOCATE( this%nr, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'nr' )
       END IF
       NULLIFY( this%nr )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_nr_0
  !
  !! De-Allokieren der Komponente "ncsize" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_ncsize_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER  :: c_upname='dealloc_h_grid_ncsize_0'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%ncsize ) ) THEN
       DEALLOCATE( this%ncsize, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'ncsize' )
       END IF
       NULLIFY( this%ncsize )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_ncsize_0
  !
  !! De-Allokieren der Komponente "b_ms(:)" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_b_ms_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER  :: c_upname='dealloc_h_grid_b_ms_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%b_ms ) ) THEN
       DEALLOCATE( this%b_ms, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'b_ms(:)' )
       END IF
       NULLIFY( this%b_ms )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_b_ms_1
  !
  !! De-Allokieren der Komponente "b_ss(:)" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_b_ss_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER  :: c_upname='dealloc_h_grid_b_ss_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%b_ss ) ) THEN
       DEALLOCATE( this%b_ss, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'b_ss(:)' )
       END IF
       NULLIFY( this%b_ss )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_b_ss_1
  !
  !! De-Allokieren der Komponente "b_s(:)" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_b_s_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='dealloc_h_grid_b_s_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%b_s ) ) THEN
       DEALLOCATE( this%b_s, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'b_s(:)' )
       END IF
       NULLIFY( this%b_s )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_b_s_1
  !
  !! De-Allokieren der Komponente "b_v(:)" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_b_v_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='dealloc_h_grid_b_v_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%b_v ) ) THEN
       DEALLOCATE( this%b_v, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'b_v(:)' )
       END IF
       NULLIFY( this%b_v )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_b_v_1
  !
  !! De-Allokieren der Komponente "b_t(:)" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_b_t_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='dealloc_h_grid_b_t_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%b_t ) ) THEN
       DEALLOCATE( this%b_t, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'b_t(:)' )
       END IF
       NULLIFY( this%b_t )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_b_t_1
  !
  !! De-Allokieren der Komponente "m" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_m_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='dealloc_h_grid_m_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%m ) ) THEN
       DEALLOCATE( this%m, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'm' )
       END IF
       NULLIFY( this%m )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_m_0
  !
  !! De-Allokieren der Komponente "n" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_n_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER  :: c_upname='dealloc_h_grid_n_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%n ) ) THEN
       DEALLOCATE( this%n, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'n' )
       END IF
       NULLIFY( this%n )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_n_0
  !
  !! De-Allokieren der Komponente "enc" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_enc_2 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='dealloc_h_grid_enc_2'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%enc ) ) THEN
       DEALLOCATE( this%enc, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'enc(:,:)' )
       END IF
       NULLIFY( this%enc )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_enc_2
  !
  !! De-Allokieren der Komponente "dry" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_dry_2 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='dealloc_h_grid_dry_2'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%dry ) ) THEN
       DEALLOCATE( this%dry, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'dry(:,:)' )
       END IF
       NULLIFY( this%dry )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_dry_2
  !
  !! De-Allokieren der Komponente "isbnd" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_isbnd_2 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER  :: c_upname='dealloc_h_grid_isbnd_2'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%isbnd ) ) THEN
       DEALLOCATE( this%isbnd, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'isbnd(:,:)' )
       END IF
       NULLIFY( this%isbnd )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_isbnd_2
  !
  !! De-Allokieren der Komponente "isdam" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_isdam_3 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER  :: c_upname='dealloc_h_grid_isdam_3'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%isdam ) ) THEN
       DEALLOCATE( this%isdam, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'isdam(:,:)' )
       END IF
       NULLIFY( this%isdam )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_isdam_3
  !
  !! De-Allokieren der Komponente "bnd" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_bnd_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='dealloc_h_grid_bnd_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%bnd ) ) THEN
       DEALLOCATE( this%bnd, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'bnd(:)' )
       END IF
       NULLIFY( this%bnd )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_bnd_1
  !
  !! De-Allokieren der Komponente "thd" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_thd_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='dealloc_h_grid_thd_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%thd ) ) THEN
       DEALLOCATE( this%thd, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'thd(:)' )
       END IF
       NULLIFY( this%thd )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_thd_1
  !
  !! De-Allokieren der Komponente "lwl" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_lwl_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='dealloc_h_grid_lwl_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%lwl ) ) THEN
       DEALLOCATE( this%lwl, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'lwl(:)' )
       END IF
       NULLIFY( this%lwl )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_lwl_1
  !
  !! De-Allokieren der Komponente "ext" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_ext_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='dealloc_h_grid_ext_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%ext ) ) THEN
       DEALLOCATE( this%ext, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'ext(:)' )
       END IF
       NULLIFY( this%ext )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_ext_1
  !
  !! De-Allokieren der Komponente "huu" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_huu_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='dealloc_h_grid_huu_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%huu ) ) THEN
       DEALLOCATE( this%huu, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'huu(:)' )
       END IF
       NULLIFY( this%huu )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_huu_1
  !
  !! De-Allokieren der Komponente "hvu" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_hvu_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='dealloc_h_grid_hvu_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%hvu ) ) THEN
       DEALLOCATE( this%hvu, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'hvu(:)' )
       END IF
       NULLIFY( this%hvu )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_hvu_1
  !
  !! De-Allokieren der Komponente "hwu" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_hwu_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='dealloc_h_grid_hwu_1'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%hwu ) ) THEN
       DEALLOCATE( this%hwu, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'hwu(:)' )
       END IF
       NULLIFY( this%hwu )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_hwu_1
  !
  !! De-Allokieren der Komponente "dwlp" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_h_grid_dwlp_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER  :: c_upname='dealloc_h_grid_dwlp_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%dwlp ) ) THEN
       DEALLOCATE( this%dwlp, STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 5010, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'dwlp' )
       END IF
       NULLIFY( this%dwlp )
    END IF
    !
  END SUBROUTINE dealloc_h_grid_dwlp_0
  !
  !! Pr&uuml;fe, ob die Komponente "id" eines Package-Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_id_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='ok_h_grid_id_0'
    !
    ok = ( this%id > 0 )
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6001, c_upname, c_modname )
    !
  END FUNCTION ok_h_grid_id_0
  !
  !! Pr&uuml;fe, ob die Komponente "name" eines Package-Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_name_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='ok_h_grid_name_0'
    !
    ok = ( this%name(1:9) /= 'UNDEFINED' )
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6002, c_upname, c_modname )
    !
  END FUNCTION ok_h_grid_name_0
  !
  !! Pr&uuml;fe, ob die Komponente "file" eines Package-Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_file_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='ok_h_grid_file_0'
    !! logisches Testfeld
    LOGICAL :: l_ok(2)
    !
    l_ok(1) = ok_file              ( this%file )
    l_ok(2) = ok_h_grid_variant_no ( this      )
    ok      = ALL( l_ok )
    !
  END FUNCTION ok_h_grid_file_0
  !
  !! Pr&uuml;fe, ob die Komponente "nv" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_nv_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_h_grid_nv_0'
    !! Hilfsvariable
    CHARACTER (LEN=10) :: ctxt    ! 
    LOGICAL            :: l_ok(4) ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5)
       ok = ( this%nv >= 3 )
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6040, c_upname, c_modname )
          WRITE(ctxt,'(I10)') this%nv ; CALL setup_error_act ( '<ActNv>', ctxt )
       END IF
    CASE(6) ! 'DELFT3D"
       l_ok(:) = .false.
       l_ok(1) = ( this%nv >= 9 )
       l_ok(2) = ( ASSOCIATED( this%m ) )
       l_ok(3) = ( ASSOCIATED( this%n ) )
       IF ( ALL( l_ok(1:3) ) ) THEN
          l_ok(4) = ( this%nv == this%m*this%n )
       END IF
       ok = ALL( l_ok )
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6041, c_upname, c_modname )
          WRITE(ctxt(1:1),'(L1)') l_ok(1) ; CALL setup_error_act ( '<status-nv>', ctxt(1:1) )
          WRITE(ctxt(1:1),'(L1)') l_ok(2) ; CALL setup_error_act ( '<status-m>', ctxt(1:1) )
          WRITE(ctxt(1:1),'(L1)') l_ok(3) ; CALL setup_error_act ( '<status-n>', ctxt(1:1) )
          WRITE(ctxt(1:1),'(L1)') l_ok(4) ; CALL setup_error_act ( '<status-nv-m-n>', ctxt(1:1) )
          WRITE(ctxt,'(I10)') this%nv     ; CALL setup_error_act ( '<ActNv>', ctxt )
          IF ( l_ok(2) ) THEN
             WRITE(ctxt,'(I10)') this%m   ; CALL setup_error_act ( '<ActM>', ctxt )
          END IF
          IF ( l_ok(3) ) THEN
             WRITE(ctxt,'(I10)') this%n   ; CALL setup_error_act ( '<ActN>', ctxt )
          END IF
          IF ( ALL( l_ok(2:3) ) ) THEN
             WRITE(ctxt,'(I10)') this%m*this%n ; CALL setup_error_act ( '<ActMN>', ctxt )
          END IF
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_nv_0
  !
  !! Pr&uuml;fe, ob die Komponente "ns" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_ns_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_h_grid_ns_0'
    !! Hilfsvariable
    CHARACTER (LEN=10) :: ctxt ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(3,4) ! 'UNTRIM_BAW' und 'UNTRIM_VC'
       ok = ( this%ns >= 3 )
    CASE DEFAULT
       ok = ( this%ns >= 0 )
    END SELECT
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6050, c_upname, c_modname )
       WRITE(ctxt,'(I10)') this%ns ; CALL setup_error_act ( '<ActNs>', ctxt )
    END IF
    !
  END FUNCTION ok_h_grid_ns_0
  !
  !! Pr&uuml;fe, ob die Komponente "nsi" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_nsi_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_h_grid_nsi_0'
    !! Hilfsvariable
    CHARACTER (LEN=10) :: ctxt ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE DEFAULT
       ok = ( this%nsi >= 0 .AND. this%nsi <= this%ns )
    END SELECT
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6055, c_upname, c_modname )
       WRITE(ctxt,'(I10)') this%nsi ; CALL setup_error_act ( '<ActNsi>', ctxt )
       WRITE(ctxt,'(I10)') this%ns  ; CALL setup_error_act ( '<ActNs>', ctxt )
    END IF
    !
  END FUNCTION ok_h_grid_nsi_0
  !
  !! Pr&uuml;fe, ob die Komponente "nsf" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_nsf_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_h_grid_nsf_0'
    !! Hilfsvariable
    CHARACTER (LEN=10) :: ctxt ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE DEFAULT
       ok = ( this%nsf >= this%nsi .AND. this%nsf <= this%ns )
    END SELECT
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6056, c_upname, c_modname )
       WRITE(ctxt,'(I10)') this%nsf ; CALL setup_error_act ( '<ActNsf>', ctxt )
       WRITE(ctxt,'(I10)') this%nsi ; CALL setup_error_act ( '<ActNsi>', ctxt )
       WRITE(ctxt,'(I10)') this%ns  ; CALL setup_error_act ( '<ActNs>', ctxt )
    END IF
    !
  END FUNCTION ok_h_grid_nsf_0
  !
  !! Pr&uuml;fe, ob die Komponente "ne" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_ne_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_h_grid_ne_0'
    !! Hilfsvariable
    CHARACTER (LEN=10) :: ctxt ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       ok = ( this%ne >= 1 )
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6060, c_upname, c_modname )
          WRITE(ctxt,'(I10)') this%ne ; CALL setup_error_act ( '<ActNe>', ctxt )
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_ne_0
  !
  !! Pr&uuml;fe, ob die Komponente "xy" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_xy_2 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_h_grid_xy_2'
    !! Z&auml;hlervariablen
    INTEGER :: i, j, k ! 
    !! Hilfsfeld
    CHARACTER (LEN=15) :: ctxt ! 
    !
    ok = .true. 
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%xy ) ) THEN
          k = SIZE(this%xy,DIM=1)/40
          DO i=1,SIZE(this%xy,DIM=1)-1
             IF (MOD(i,2500)==0) WRITE(*,'(A,A,I8,A,I8,A,L1)') &
                  '  ... ',c_upname,i,' / ',SIZE(this%xy,DIM=1),' / ok = ',ok
             DO j=i+1,SIZE(this%xy,DIM=1)
                IF ( this%xy(i,1) == this%xy(j,1) ) THEN
                   IF ( this%xy(i,2) == this%xy(j,2) ) THEN
                      ok = .false. 
                      CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
                      WRITE(ctxt,'(G15.9)') this%xy(i,1)
                      CALL setup_error_act ( '<AktX>', ctxt )
                      WRITE(ctxt,'(G15.9)') this%xy(i,2)
                      CALL setup_error_act ( '<AktY>', ctxt )
                      WRITE(ctxt(1:10),'(I10)') i
                      CALL setup_error_act ( '<Knoten1>', ctxt(1:10) )
                      WRITE(ctxt(1:10),'(I10)') j
                      CALL setup_error_act ( '<Knoten2>', ctxt(1:10) )
                   END IF
                END IF
             END DO
          END DO
       ELSE
          ok = .false. 
          CALL setup_error_act ( all_errors(:), 6510, c_upname, c_modname )
          CALL setup_error_act ( '<component-name>', 'xy(:,:)' )
       END IF
    CASE DEFAULT
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_xy_2
  !
  !! Pr&uuml;fe, ob die Komponente "nen" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_nen_2 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_h_grid_nen_2'
    !! Z&auml;hlervariablen
    INTEGER :: i, j, k ! 
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !
    ok = .true.
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%nen ) ) THEN
          DO i=1,SIZE(this%nen,DIM=1)
             DO j=1,this%ks(i)-1
                DO k=j+1,this%ks(i)
                   IF ( this%nen(i,j) == this%nen(i,k) ) THEN
                      ok = .false.
                      CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
                      WRITE(ctxt,'(I10)') j
                      CALL setup_error_act ( '<LokalKnoten1>', ctxt )
                      WRITE(ctxt,'(I10)') k
                      CALL setup_error_act ( '<LokalKnoten2>', ctxt )
                      WRITE(ctxt,'(I10)') i
                      CALL setup_error_act ( '<AktPolyNo>', ctxt )
                   END IF
                END DO
             END DO
          END DO
       ELSE
          SELECT CASE ( get_h_grid_variant_no ( this ) )
          CASE ( 1,2,3,4,5 )
             ok = .false. 
             CALL setup_error_act ( all_errors(:), 6510, c_upname, c_modname )
             CALL setup_error_act ( '<component-name>', 'nen(:,:)' )
          CASE DEFAULT
             ok = .true.
          END SELECT
       END IF
    CASE DEFAULT
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_nen_2
  !
  !! Pr&uuml;fe, ob die Komponente "irand" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_irand_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='ok_h_grid_irand_1'
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    INTEGER            :: nx   ! 
    !
    IF ( ASSOCIATED( this%irand ) ) THEN
       SELECT CASE ( get_h_grid_variant_no ( this ) )
       CASE(1,2) ! 'GITTER05'
          ok = ( 0 <= MINVAL( this%irand(:) ) .AND. MAXVAL( this%irand(:) ) <= 7  )
          nx = 7
       CASE(6)   ! 'DELFT3D'
          ok = ( 0 <= MINVAL( this%irand(:) ) .AND. MAXVAL( this%irand(:) ) <= 15 )
          nx = 15
       END SELECT
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
          WRITE(ctxt,'(I10)') MINVAL( this%irand(:) ) ; CALL setup_error_act ( '<MinIrand>', ctxt )
          WRITE(ctxt,'(I10)') MAXVAL( this%irand(:) ) ; CALL setup_error_act ( '<MaxIrand>', ctxt )
          WRITE(ctxt,'(I10)') nx                      ; CALL setup_error_act ( '<nx>', ctxt )
       END IF
    ELSE
       SELECT CASE ( get_h_grid_variant_no ( this ) )
       CASE(1,2) ! 'GITTER05'
          ok = .false. 
          CALL setup_error_act ( all_errors(:), 6530, c_upname, c_modname )
       CASE DEFAULT
          ok = .true.
       END SELECT
    END IF
    !
  END FUNCTION ok_h_grid_irand_1
  !
  !! Pr&uuml;fe, ob die Komponente "ks" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_ks_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_h_grid_ks_1'
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%ks ) ) THEN
          SELECT CASE ( get_h_grid_variant_no ( this ) )
          CASE ( 6 )
             ok = ( 4 <= MINVAL( this%ks(:) ) .AND. MAXVAL( this%ks(:) ) <= 4 )
          CASE DEFAULT
             ok = ( 3 <= MINVAL( this%ks(:) ) .AND. MAXVAL( this%ks(:) ) <= 4 )
          END SELECT
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6070, c_upname, c_modname )
             WRITE(ctxt,'(I10)') MINVAL( this%ks(:) )
             CALL setup_error_act ( '<MinKs>', ctxt )
             WRITE(ctxt,'(I10)') MAXVAL( this%ks(:) )
             CALL setup_error_act ( '<MaxKs>', ctxt )
          END IF
       ELSE
          SELECT CASE ( get_h_grid_variant_no ( this ) )
          CASE ( 1,2,3,4,5 )
             ok = .false. 
             CALL setup_error_act ( all_errors(:), 6510, c_upname, c_modname )
             CALL setup_error_act ( '<component-name>', 'ks(:)' )
          CASE DEFAULT
             ok = .true.
          END SELECT
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_ks_1
  !
  !! Pr&uuml;fe, ob die Komponente "hv" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_hv_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_h_grid_hv_1'
    !! Hilfsfeld
    CHARACTER (LEN=15) :: ctxt ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%hv ) ) THEN
          ok = ( -10001.0_Double <= MINVAL( this%hv(:) ) .AND. MAXVAL( this%hv(:) ) <= 11022.0_Double )
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6080, c_upname, c_modname )
             WRITE(ctxt,'(G15.9)') MINVAL( this%hv(:) )
             CALL setup_error_act ( '<MinHv>', ctxt )
             WRITE(ctxt,'(G15.9)') MAXVAL( this%hv(:) )
             CALL setup_error_act ( '<MaxHv>', ctxt )
          END IF
       ELSE
          SELECT CASE ( get_h_grid_variant_no ( this ) )
          CASE(1,2,5,6) ! 'GITTER05', 'SELAFIN' und 'DELFT3D'
             ok = .false. 
             CALL setup_error_act ( all_errors(:), 6510, c_upname, c_modname )
             CALL setup_error_act ( '<component-name>', 'hv(:)' )
          CASE DEFAULT
             ok = .true.
          END SELECT
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_hv_1
  !
  !! Pr&uuml;fe, ob die Komponente "nrand" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_nrand_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='ok_h_grid_nrand_0'
    !! Hifsvariable
    CHARACTER (LEN=10) :: ctxt ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%nrand ) ) THEN
          ok = ( this%nrand >= 3 )
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6090, c_upname, c_modname )
             WRITE(ctxt,'(I10)') this%nrand
             CALL setup_error_act ( '<ActNrand>', ctxt )
          END IF
       ELSE
          SELECT CASE ( get_h_grid_variant_no ( this ) )
          CASE(1,2) ! 'GITTER05'
             ok = .false. 
             CALL setup_error_act ( all_errors(:), 6510, c_upname, c_modname )
             CALL setup_error_act ( '<component-name>', 'nrand' )
          CASE DEFAULT
             ok = .true.
          END SELECT
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_nrand_0
  !
  !! Pr&uuml;fe, ob die Komponente "nptfr" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_nptfr_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='ok_h_grid_nptfr_0'
    !! Hifsvariable
    CHARACTER (LEN=10) :: ctxt ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%nptfr ) .AND. ASSOCIATED( this%ncsize ) ) THEN
          IF ( this%ncsize > 0 ) THEN
             ok = ( 0 <= this%nptfr .AND. this%nptfr <= this%nv ) 
             IF ( .NOT. ok ) THEN
                CALL setup_error_act ( all_errors(:), 6094, c_upname, c_modname )
                WRITE(ctxt,'(I10)') this%nptfr
                CALL setup_error_act ( '<nptfr>', ctxt )
                WRITE(ctxt,'(I10)') this%nv
                CALL setup_error_act ( '<nv>', ctxt )
             END IF
          END IF
       ELSE
          SELECT CASE ( get_h_grid_variant_no ( this ) )
          CASE(5) ! 'SELAFIN'
             ok = .true.
             IF (ASSOCIATED(this%ncsize)) THEN
                IF (get_ncsize_object (this) > 0) THEN
                   ok = .false. 
                   CALL setup_error_act ( all_errors(:), 6520, c_upname, c_modname )
                ENDIF
             ENDIF
          CASE DEFAULT
             ok = .true.
          END SELECT
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_nptfr_0
  !
  !! Pr&uuml;fe, ob die Komponente "nptir" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_nptir_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='ok_h_grid_nptir_0'
    !! Hifsvariable
    CHARACTER (LEN=10) :: ctxt ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%nptir ) .AND. ASSOCIATED( this%ncsize ) ) THEN
          IF ( this%ncsize > 0 ) THEN
             ok = ( 1 <= this%nptir .AND. this%nptir <= this%nv ) 
             IF ( .NOT. ok ) THEN
                CALL setup_error_act ( all_errors(:), 6095, c_upname, c_modname )
                WRITE(ctxt,'(I10)') this%nptir
                CALL setup_error_act ( '<nptir>', ctxt )
                WRITE(ctxt,'(I10)') this%nv
                CALL setup_error_act ( '<nv>', ctxt )
             END IF
          END IF
       ELSE
          SELECT CASE ( get_h_grid_variant_no ( this ) )
          CASE(5) ! 'SELAFIN'
             ok = .true.
             IF (ASSOCIATED(this%ncsize)) THEN
                IF (get_ncsize_object (this) > 0) THEN
                   ok = .false. 
                   CALL setup_error_act ( all_errors(:), 6530, c_upname, c_modname )
                ENDIF
             ENDIF
          CASE DEFAULT
             ok = .true.
          END SELECT
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_nptir_0
  !
  !! Pr&uuml;fe, ob die Komponente "time" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_time_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='ok_h_grid_time_0'
    !
    IF ( ASSOCIATED( this%time ) ) THEN
       ok = ok_datetime ( this%time )
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6100, c_upname, c_modname )
          CALL setup_error_act ( '<ActTime>', TRIM( datetime_to_string( this%time ) ) )
       END IF
    ELSE
       CALL setup_error_act ( all_errors(:), 6510, c_upname, c_modname )
       CALL setup_error_act ( '<component-name>', 'time' )
       ok = .false. !
    END IF
    !
  END FUNCTION ok_h_grid_time_0
  !
  !! Pr&uuml;fe, ob die Komponente "nbc" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_nbc_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_h_grid_nbc_0'
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%nbc ) ) THEN
          ok = ( this%nbc >= 0 .AND. this%nbc <= this%ne )
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6110, c_upname, c_modname )
             WRITE(ctxt,'(I10)') this%nbc
             CALL setup_error_act ( '<ActNbc>', TRIM( ctxt ) )
             WRITE(ctxt,'(I10)') this%ne
             CALL setup_error_act ( '<MaxNbc>', TRIM( ctxt ) )
          END IF
       ELSE
          SELECT CASE ( get_h_grid_variant_no ( this ) )
          CASE(3,4) ! 'UNTRIM_BAW,UNTRIM_VC'
             ok = .false. 
             CALL setup_error_act ( all_errors(:), 6510, c_upname, c_modname )
             CALL setup_error_act ( '<component-name>', 'nbc' )
          CASE DEFAULT
             ok = .true.
          END SELECT
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_nbc_0
  !
  !! Pr&uuml;fe, ob die Komponente "hland" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_hland_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='ok_h_grid_hland_0'
    !! Hilfsfeld
    CHARACTER (LEN=15) :: ctxt ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%hland ) ) THEN
          ok = ( this%hland <= -10000.0_Double )
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6120, c_upname, c_modname )
             WRITE(ctxt,'(G15.9)') this%hland
             CALL setup_error_act ( '<ActHland>', TRIM( ctxt ) )
          END IF
       ELSE
          SELECT CASE ( get_h_grid_variant_no ( this ) )
          CASE(3,4) ! 'UNTRIM_BAW,UNTRIM_VC'
             ok = .false. 
             CALL setup_error_act ( all_errors(:), 6510, c_upname, c_modname )
             CALL setup_error_act ( '<component-name>', 'hland' )
          CASE DEFAULT
             ok = .true.
          END SELECT
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_hland_0
  !
  !! Pr&uuml;fe, ob die Komponente "angle" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_angle_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='ok_h_grid_angle_0'
    !! Hilfsfeld
    CHARACTER (LEN=15) :: ctxt ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%angle ) ) THEN
          ok = ( this%angle >= -90.0_Double .AND. this%angle <= +90.0_Double )
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6130, c_upname, c_modname )
             WRITE(ctxt,'(G15.9)') this%angle
             CALL setup_error_act ( '<ActAngle>', TRIM( ctxt ) )
             WRITE(ctxt,'(G15.9)')  90.0_Double
             CALL setup_error_act ( '<MaxAngle>', TRIM( ctxt ) )
             WRITE(ctxt,'(G15.9)') -90.0_Double
             CALL setup_error_act ( '<MinAngle>', TRIM( ctxt ) )
          END IF
       ELSE
          SELECT CASE ( get_h_grid_variant_no ( this ) )
          CASE(3,4) ! 'UNTRIM_BAW,UNTRIM_VC'
             ok = .false. 
             CALL setup_error_act ( all_errors(:), 6510, c_upname, c_modname )
             CALL setup_error_act ( '<component-name>', 'angle' )
          CASE DEFAULT
             ok = .true.
          END SELECT
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_angle_0
  !
  !! Pr&uuml;fe, ob die Komponente "text" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_text_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='ok_h_grid_text_1'
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !    
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%text ) ) THEN
          ok = ALL ( LEN_TRIM( this%text(:) ) > 0 )
          IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 6140, c_upname, c_modname )
       ELSE
          SELECT CASE ( get_h_grid_variant_no ( this ) )
          CASE(3,4,5,6) ! 'UNTRIM_BAW,UNTRIM_VC','SELAFIN' und 'DELFT3D'
             ok = .false. 
             CALL setup_error_act ( all_errors(:), 6510, c_upname, c_modname )
             CALL setup_error_act ( '<component-name>', 'text(:)' )
          CASE DEFAULT
             ok = .true.
          END SELECT
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_text_1
  !
  !! Pr&uuml;fe, ob die Komponente "jb" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_jb_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_h_grid_jb_1'
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%jb ) ) THEN
          ok = ALL ( 1 <= this%jb(:) .AND. this%jb(:) <= this%nv )
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6150, c_upname, c_modname )
             WRITE(ctxt,'(I10)') 1                    ! 
             CALL setup_error_act ( '<AllMinStart>', ctxt )
             WRITE(ctxt,'(I10)') this%nv              ! 
             CALL setup_error_act ( '<AllMaxStart>', ctxt )
             WRITE(ctxt,'(I10)') MINVAL( this%jb(:) ) ! 
             CALL setup_error_act ( '<ActMinStart>', ctxt )
             WRITE(ctxt,'(I10)') MAXVAL( this%jb(:) ) ! 
             CALL setup_error_act ( '<ActMaxStart>', ctxt )
          END IF
       ELSE
          SELECT CASE ( get_h_grid_variant_no ( this ) )
          CASE(3,4) ! 'UNTRIM_BAW,UNTRIM_VC'
             ok = .false. 
             CALL setup_error_act ( all_errors(:), 6510, c_upname, c_modname )
             CALL setup_error_act ( '<component-name>', 'jb(:)' )
          CASE DEFAULT
             ok = .true.
          END SELECT
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_jb_1
  !
  !! Pr&uuml;fe, ob die Komponente "jt" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_jt_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_h_grid_jt_1'
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%jt ) ) THEN
          ok = ALL ( 1 <= this%jt(:) .AND. this%jt(:) <= this%nv )
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6160, c_upname, c_modname )
             WRITE(ctxt,'(I10)') 1                    ! 
             CALL setup_error_act ( '<AllMinStart>', ctxt )
             WRITE(ctxt,'(I10)') this%nv              ! 
             CALL setup_error_act ( '<AllMaxStart>', ctxt )
             WRITE(ctxt,'(I10)') MINVAL( this%jt(:) ) ! 
             CALL setup_error_act ( '<ActMinStart>', ctxt )
             WRITE(ctxt,'(I10)') MAXVAL( this%jt(:) ) ! 
             CALL setup_error_act ( '<ActMaxStart>', ctxt )
          END IF
       ELSE
          SELECT CASE ( get_h_grid_variant_no ( this ) )
          CASE(3,4) ! 'UNTRIM_BAW,UNTRIM_VC'
             ok = .false. 
             CALL setup_error_act ( all_errors(:), 6510, c_upname, c_modname )
             CALL setup_error_act ( '<component-name>', 'jt(:)' )
          CASE DEFAULT
             ok = .true.
          END SELECT
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_jt_1
  !
  !! Pr&uuml;fe, ob die Komponente "is" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_is_2 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_h_grid_is_2'
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !! Z%auml;hlervariable
    INTEGER :: i ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%is ) ) THEN
          ok = .true.
          DO i=1,SIZE(this%is,DIM=1)
             IF ( .NOT. ok ) EXIT
             ok = ALL ( 1 <= this%is(i,1:this%ks(i)) .AND. this%is(i,1:this%ks(i)) <= this%ns )
             IF ( .NOT. ok ) THEN
                CALL setup_error_act ( all_errors(:), 6170, c_upname, c_modname )
                WRITE(ctxt,'(I10)') i
                CALL setup_error_act ( '<ActPolygon>', ctxt )
                WRITE(ctxt,'(I10)') 1                    ! 
                CALL setup_error_act ( '<AllMinStart>', ctxt )
                WRITE(ctxt,'(I10)') this%ns              ! 
                CALL setup_error_act ( '<AllMaxStart>', ctxt )
                WRITE(ctxt,'(I10)') MINVAL( this%is(i,1:this%ks(i)) ) ! 
                CALL setup_error_act ( '<ActMinStart>', ctxt )
                WRITE(ctxt,'(I10)') MAXVAL( this%is(i,1:this%ks(i)) ) ! 
                CALL setup_error_act ( '<ActMaxStart>', ctxt )
             END IF
          END DO
       ELSE
          SELECT CASE ( get_h_grid_variant_no ( this ) )
          CASE(3,4) ! 'UNTRIM_BAW,UNTRIM_VC'
             ok = .false. 
             CALL setup_error_act ( all_errors(:), 6510, c_upname, c_modname )
             CALL setup_error_act ( '<component-name>', 'is(:,:)' )
          CASE DEFAULT
             ok = .true.
          END SELECT
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_is_2
  !
  !! Pr&uuml;fe, ob die Komponente "je" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_je_2 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_h_grid_je_2'
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !! Z%auml;hlervariable
    INTEGER :: i ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%je ) ) THEN
          ok = .true.
          DO i=1,SIZE(this%je,DIM=1)
             IF ( .NOT. ok ) EXIT
             ok = ( ALL ( 0 <= this%je(i,:) .AND. this%je(i,:) <= this%ne ) .AND. this%je(i,1) > 0 )
             IF ( .NOT. ok ) THEN
                CALL setup_error_act ( all_errors(:), 6180, c_upname, c_modname )
                WRITE(ctxt,'(I10)') i
                CALL setup_error_act ( '<ActKante>', ctxt )
                WRITE(ctxt,'(I10)') 0                    ! 
                CALL setup_error_act ( '<AllMinStart>', ctxt )
                WRITE(ctxt,'(I10)') this%ne              !
                CALL setup_error_act ( '<AllMaxStart>', ctxt )
                WRITE(ctxt,'(I10)') MINVAL( this%je(i,:) ) ! 
                CALL setup_error_act ( '<ActMinStart>', ctxt )
                WRITE(ctxt,'(I10)') MAXVAL( this%je(i,:) ) ! 
                CALL setup_error_act ( '<ActMaxStart>', ctxt )
                WRITE(ctxt,'(I10)') this%je(i,1) ; CALL setup_error_act( '<ActJe1>', ctxt )
                WRITE(ctxt,'(I10)') this%je(i,2) ; CALL setup_error_act( '<ActJe2>', ctxt )
             END IF
          END DO
       ELSE
          SELECT CASE ( get_h_grid_variant_no ( this ) )
          CASE(3,4) ! 'UNTRIM_BAW,UNTRIM_VC'
             ok = .false. 
             CALL setup_error_act ( all_errors(:), 6510, c_upname, c_modname )
             CALL setup_error_act ( '<component-name>', 'je(:,:)' )
          CASE DEFAULT
             ok = .true.
          END SELECT
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_je_2
  !
  !! Pr&uuml;fe, ob die Komponente "ie" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_ie_2 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_h_grid_ie_2'
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !! Z%auml;hlervariable
    INTEGER :: i ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%ie ) ) THEN
          ok = .true.
          DO i=1,SIZE(this%ie,DIM=1)
             IF ( .NOT. ok ) EXIT
             ok = ALL ( 0 <= this%ie(i,1:this%ks(i)) .AND. this%ie(i,1:this%ks(i)) <= this%ne )
             IF ( .NOT. ok ) THEN
                CALL setup_error_act ( all_errors(:), 6190, c_upname, c_modname )
                WRITE(ctxt,'(I10)') i
                CALL setup_error_act ( '<ActPolygon>', ctxt )
                WRITE(ctxt,'(I10)') 0                    ! 
                CALL setup_error_act ( '<AllMinStart>', ctxt )
                WRITE(ctxt,'(I10)') this%ne              !
                CALL setup_error_act ( '<AllMaxStart>', ctxt )
                WRITE(ctxt,'(I10)') MINVAL( this%ie(i,1:this%ks(i)) ) ! 
                CALL setup_error_act ( '<ActMinStart>', ctxt )
                WRITE(ctxt,'(I10)') MAXVAL( this%ie(i,1:this%ks(i)) ) ! 
                CALL setup_error_act ( '<ActMaxStart>', ctxt )
             END IF
          END DO
       ELSE
          ok = .true.
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_ie_2
  !
  !! Pr&uuml;fe, ob die Komponente "xs" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_xs_2 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_h_grid_xs_2'
    !
    !! Z&auml;hlervariablen
    INTEGER :: i, j
    !! Hilfsfeld
    CHARACTER (LEN=15) :: ctxt
    !
    ok = .true. 
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%xs ) ) THEN
          DO i=1,SIZE(this%xs,DIM=1)-1
             IF (MOD(i,2500)==0) WRITE(*,'(A,A,I8,A,I8,A,L1)') &
                  '  ... ',c_upname,i,' / ',SIZE(this%xs,DIM=1),' / ok = ',ok
             DO j=i+1,SIZE(this%xs,DIM=1)
                IF ( this%xs(i,1) == this%xs(j,1) ) THEN
                   IF ( this%xs(i,2) == this%xs(j,2) ) THEN
                      ok = .false. 
                      CALL setup_error_act ( all_errors(:), 6015, c_upname, c_modname )
                      WRITE(ctxt,'(G15.9)') this%xs(i,1)
                      CALL setup_error_act ( '<AktX>', ctxt )
                      WRITE(ctxt,'(G15.9)') this%xs(i,2)
                      CALL setup_error_act ( '<AktY>', ctxt )
                      WRITE(ctxt(1:10),'(I10)') i
                      CALL setup_error_act ( '<Knoten1>', ctxt(1:10) )
                      WRITE(ctxt(1:10),'(I10)') j
                      CALL setup_error_act ( '<Knoten2>', ctxt(1:10) )
                   END IF
                END IF
             END DO
          END DO
       END IF
    CASE DEFAULT
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_xs_2
  !
  !! Pr&uuml;fe, ob die Komponente "xc" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_xc_2 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_h_grid_xc_2'
    !! Hilfsfelder / Variablen
    INTEGER , PARAMETER :: jtri(3)=(/2,3,1/)   ! Dreieck 
    INTEGER , PARAMETER :: jqua(4)=(/2,3,4,1/) ! Viereck
    REAL (KIND=Double)  :: xx(4,2) !  
    REAL (KIND=Double)  :: al(4)   ! 
    REAL (KIND=Double)  :: ortho, max_ortho, mit_ortho ! 
    !! Z&auml;hlervariablen
    INTEGER :: i, j, l, n, nn, m ! 
    !
    ok = .true.
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(3,4) ! UNTRIM_BAW und UNTRIM_VC
       IF ( ASSOCIATED(this%xc) .AND. ASSOCIATED(this%nen)   .AND. &
            ASSOCIATED(this%xy) .AND. ASSOCIATED(this%ks)    .AND. &
            ASSOCIATED(this%hu) .AND. ASSOCIATED(this%jb)    .AND. &
            ASSOCIATED(this%jt) .AND. ASSOCIATED(this%dx)    .AND. &
            ASSOCIATED(this%dy) .AND. ASSOCIATED(this%je)          ) THEN
          max_ortho = 0.0_Double
          mit_ortho = 0.0_Double
          n         = 0
          nn        = 0
          m         = 0
          DO j=1,SIZE(this%hu)
             IF ( ALL(this%je(j,:) > 0) ) THEN
                IF ( ABS(this%dx(j)*this%dy(j)) > 1.0E-6_Double ) THEN
                   ortho = ( this%xy(this%jt(j),2)   - this%xy(this%jb(j),2)   ) * &
                           ( this%xc(this%je(j,2),2) - this%xc(this%je(j,1),2) ) / &
                           ( this%dx(j) * this%dy(j) ) +                           &
                           ( this%xy(this%jt(j),1)   - this%xy(this%jb(j),1)   ) * &
                           ( this%xc(this%je(j,2),1) - this%xc(this%je(j,1),1) ) / &
                           ( this%dx(j) * this%dy(j) )
                   ortho = 90.0_Double*(1.0_Double-ACOS(ABS(ortho))/ACOS(0.0_Double))
                   IF ( ortho > 1.0_Double ) THEN
                      IF ( prn_op ) WRITE(prn_lun,9000) j, ortho, this%xy(this%jb(j),:), this%xy(this%jt(j),:)
                      n = n + 1
                   END IF
                   max_ortho = MAX(max_ortho,ortho)
                   mit_ortho = mit_ortho + ortho
                   m         = m + 1
                ELSE
                   IF ( prn_op ) WRITE(prn_lun,9010) j, this%xy(this%jb(j),:), this%xy(this%jt(j),:), &
                                                        this%xc(this%je(j,1),:), this%xc(this%je(j,2),:)
                   nn = nn + 1
                END IF
             END IF
          END DO
          IF ( m > 0 ) mit_ortho = mit_ortho/m
          IF ( n > 0 ) THEN
             WRITE(*,*) ' *** Warning *** Orthogonalitaet auf mehreren Verbindungslinien verletzt '
             WRITE(*,*) ' ... ',n,' von ',SIZE(this%hu),' Verbindungslinien zwischen Zentren      '
             WRITE(*,*) '     weichen um mehr als 1 Grad von der Orthogonalitaetsbedingung ab     '
             WRITE(*,*) ' --> fuer Details - siehe Druckerdatei (falls geoeffnet)'
          ELSE
             WRITE(*,*) ' inf ... alle Verbindungslinien zwischen Zentren sind (annaehernd) orthogonal '
          END IF
          IF ( nn > 0 ) THEN
             WRITE(*,*) ' *** Warning *** Produkte aus Zentrumsabstand und Kantenlaenge sind zu klein '
             WRITE(*,*) ' ... ',nn,' von ',SIZE(this%hu),' Kanten/Zentrumsverbindungen sind betroffen '
             WRITE(*,*) ' --> fuer Details - siehe Druckerdatei (falls geoeffnet)'
          ELSE
             WRITE(*,*) ' inf ... alle Kanten/Zentrumsverbindungen sind o.k.'
          END IF
          WRITE(*,'(A,F6.2,A)') '  inf ... max. Abweichnung von der Orthogonalitaet = ',max_ortho,' Grad'
          WRITE(*,'(A,F6.2,A)') '  inf ... mit. Abweichnung von der Orthogonalitaet = ',mit_ortho,' Grad'
          n = 0
          DO j=1,SIZE(this%hu)
             IF ( ALL(this%je(j,:) > 0) ) THEN
                ortho = ( this%xc(this%je(j,1),2) - this%xc(this%je(j,2),2) ) * &
                        ( this%xy(this%jt(j),1)   - this%xy(this%jb(j),1)   ) + &
                        ( this%xy(this%jt(j),2)   - this%xy(this%jb(j),2)   ) * &
                        ( this%xc(this%je(j,2),1) - this%xc(this%je(j,1),1) )
                IF ( ortho < 0.0_Double ) THEN
                   IF ( prn_op ) WRITE(prn_lun,9020) j, this%xy(this%jb(j),:), this%xy(this%jt(j),:), &
                                                        this%xc(this%je(j,1),:), this%xc(this%je(j,2),:)
                   n = n + 1
                END IF
             END IF
          END DO
          IF ( n > 0 ) THEN
             WRITE(*,*) ' *** Warning *** Kanten haben falsche Orientierung '
             WRITE(*,*) ' ... ',n,' von ',SIZE(this%hu),' Kanten sind fehlerhaft '
             WRITE(*,*) ' --> fuer Details - siehe Druckerdatei (falls geoeffnet)'
          ELSE
             WRITE(*,*) ' inf ... alle Kanten haben die richtige Orientierung '
          END IF
          n = 0
          DO i=1,SIZE(this%nen,DIM=1)
             DO l=1,this%ks(i)
                xx(l,:) = this%xy(this%nen(i,l),:)
             END DO
             SELECT CASE (this%ks(i))
             CASE(3)
                al(:this%ks(i)) = this%xc(i,1)*( xx(:this%ks(i),2) - xx(jtri(:),2)) + &
                     xx(:this%ks(i),1)*( xx(jtri(:)    ,2) - this%xc(i ,2)) + &
                     xx(jtri(:),1)    *( this%xc(i     ,2) - xx(:this%ks(i) ,2))
             CASE(4)
                al(:this%ks(i)) = this%xc(i,1)*( xx(:this%ks(i),2) - xx(jqua(:),2)) + &
                     xx(:this%ks(i),1)*( xx(jqua(:)    ,2) - this%xc(i ,2)) + &
                     xx(jqua(:),1)    *( this%xc(i     ,2) - xx(:this%ks(i) ,2))
             CASE DEFAULT
                WRITE(*,*) ' *** code missing *** '//TRIM(c_upname)
                WRITE(*,*) ' ... Polygon ist weder Dreieck noch Viereck '
             END SELECT
             IF ( ANY( al(:this%ks(i)) < 0.0_Double ) ) THEN
                IF ( prn_op ) WRITE(prn_lun,9030) i, this%xc(i,:)
                n = n + 1
             END IF
          END DO
          IF ( n > 0 ) THEN
             WRITE(*,*) ' *** Warning *** Zentren liegen teilweise nicht in Polygonen '
             WRITE(*,*) ' ... ',n,' von ',SIZE(this%xc,DIM=1),' Zentren liegen ausserhalb '
             WRITE(*,*) ' --> fuer Details - siehe Druckerdatei (falls geoeffnet)'
          ELSE
             WRITE(*,*) ' inf ... alle Zentren liegen innerhalb der Polygone '
          END IF
       ELSE
          SELECT CASE ( get_h_grid_variant_no ( this ) )
          CASE(4) ! UNTRIM_VC'
             ok = .false. 
             CALL setup_error_act ( all_errors(:), 6510, c_upname, c_modname )
             CALL setup_error_act ( '<component-name>', 'xc(:,:)' )
          CASE DEFAULT
          END SELECT
       END IF
    END SELECT
    !
9000 FORMAT('# Kante ',I10,' : Zentrumsverbindung weicht um ',F8.2,' Grad von Orhogonalitaet ab',/ &
            '# ... Anfangspunkt Kante = ',2F12.2,/ & 
            '# ... Endpunkt Kante     = ',2F12.2 )
9010 FORMAT('# Kante ',I10,' : das Produkt aus Zentrumsabstand und Kantenlaenge ist zu klein',/ &
            '# ... Anfangspunkt Kante = ',2F12.2,/ & 
            '# ... Endpunkt Kante     = ',2F12.2,/ &
            '# ... linkes Zentrum     = ',2F12.2,/ &
            '# ... rechtes Zentrum    = ',2F12.2)
9020 FORMAT('# Kante ',I10,' : Kanten und Zentrumsverbindung haben falsche Orientierung',/ &
            '# ... Anfangspunkt Kante = ',2F12.2,/ & 
            '# ... Endpunkt Kante     = ',2F12.2,/ &
            '# ... linkes Zentrum     = ',2F12.2,/ &
            '# ... rechtes Zentrum    = ',2F12.2)
9030 FORMAT('# Polygon ',I10,' : Sortierung der Knoten fehlerhaft',/ &
            '# ... Zentrum Polygon = ',2F12.2 )
    !
  END FUNCTION ok_h_grid_xc_2
  !
  !! Pr&uuml;fe, ob die Komponente "xg" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_h_grid_xg_2 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .true.
    !
  END FUNCTION ok_h_grid_xg_2
  !
  !! Pr&uuml;fe, ob die Komponente "dx" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_dx_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_h_grid_dx_1'
    !! Hilfsfeld
    CHARACTER (LEN=15) :: ctxt ! 
    REAL (KIND=Double) , POINTER :: p_dxmin ! 
    REAL (KIND=Double)           :: dxmin   ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%dx ) ) THEN
          p_dxmin => get_dxmin_object ( this )
          dxmin = MERGE( p_dxmin, 0.0_Double, ASSOCIATED( p_dxmin ) )
          ok = ALL( this%dx(:) >= dxmin )
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6220, c_upname, c_modname )
             WRITE(ctxt,'(G15.9)') dxmin 
             CALL setup_error_act ( '<AllMinAbstand>', ctxt )
             WRITE(ctxt,'(G15.9)') MINVAL( this%dx(:) ) 
             CALL setup_error_act ( '<ActMinAbstand>', ctxt )
          END IF
       ELSE
          ok = .true.
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_dx_1
  !
  !! Pr&uuml;fe, ob die Komponente "dy" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_dy_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_h_grid_dy_1'
    !! Hilfsfeld
    CHARACTER (LEN=15) :: ctxt ! 
    !! Z%auml;hlervariable
    INTEGER :: i ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%dy ) ) THEN
          ok = ALL( this%dy(:) > EPSILON( this%dy ) )
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6230, c_upname, c_modname )
             WRITE(ctxt,'(G15.9)') EPSILON( this%dy(:) ) 
             CALL setup_error_act ( '<AllMinLaenge>', ctxt )
             WRITE(ctxt,'(G15.9)') MINVAL( this%dy(:) ) 
             CALL setup_error_act ( '<ActMinLaenge>', ctxt )
          END IF
       ELSE
          ok = .true.
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_dy_1
  !
  !! Pr&uuml;fe, ob die Komponente "dg" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_dg_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_h_grid_dg_1'
    !! Hilfsfeld
    CHARACTER (LEN=15) :: ctxt ! 
    !! Z%auml;hlervariable
    INTEGER :: i ! 
    REAL (KIND=Double) , POINTER :: p_dxmin ! 
    REAL (KIND=Double)           :: dxmin   ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%dg ) ) THEN
          p_dxmin => get_dxmin_object ( this )
          dxmin = MERGE( p_dxmin, 0.0_Double, ASSOCIATED( p_dxmin ) )
          ok = ALL( this%dg(:) >= dxmin )
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6410, c_upname, c_modname )
             WRITE(ctxt,'(G15.9)') dxmin
             CALL setup_error_act ( '<AllMinLaenge>', ctxt )
             WRITE(ctxt,'(G15.9)') MINVAL( this%dg(:) ) 
             CALL setup_error_act ( '<ActMinLaenge>', ctxt )
          END IF
       ELSE
          ok = .true.
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_dg_1
  !
  !! Pr&uuml;fe, ob die Komponente "aa" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_aa_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_h_grid_aa_1'
    !! Hilfsfeld
    CHARACTER (LEN=15) :: ctxt ! 
    !! Z%auml;hlervariable
    INTEGER :: i ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%aa ) ) THEN
          ok = ALL( this%aa(:) > EPSILON( this%aa ) )
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6240, c_upname, c_modname )
             WRITE(ctxt,'(G15.9)') EPSILON( this%aa(:) ) 
             CALL setup_error_act ( '<AllMinFlaeche>', ctxt )
             WRITE(ctxt,'(G15.9)') MINVAL( this%aa(:) ) 
             CALL setup_error_act ( '<ActMinFlaeche>', ctxt )
          END IF
       ELSE
          ok = .true.
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_aa_1
  !
  !! Pr&uuml;fe, ob die Komponente "hu" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_hu_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_h_grid_hu_1'
    !! Hilfsfeld
    CHARACTER (LEN=15) :: ctxt ! 
    !! Z%auml;hlervariable
    INTEGER :: i ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%hu ) ) THEN
          ok = ( -10001.0_Double <= MINVAL( this%hu(:) ) .AND. MAXVAL( this%hu(:) ) <= 11022.0_Double )
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6250, c_upname, c_modname )
             CALL setup_error_act ( '<component-name>', 'hu(:)' )
             WRITE(ctxt,'(G15.9)') MINVAL( this%hu(:) ) ; CALL setup_error_act ( '<Min>', ctxt )
             WRITE(ctxt,'(G15.9)') MAXVAL( this%hu(:) ) ; CALL setup_error_act ( '<Max>', ctxt )
          END IF
       ELSE
          SELECT CASE ( get_h_grid_variant_no ( this ) )
          CASE(3,4) ! 'UNTRIM_BAW,UNTRIM_VC'
             ok = .false. 
             CALL setup_error_act ( all_errors(:), 6510, c_upname, c_modname )
             CALL setup_error_act ( '<component-name>', 'hu(:)' )
          CASE DEFAULT
             ok = .true.
          END SELECT
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_hu_1
  !
  !! Pr&uuml;fe, ob die Komponente "hw" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_hw_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_h_grid_hw_1'
    !! Hilfsfeld
    CHARACTER (LEN=15) :: ctxt ! 
    !! Z%auml;hlervariable
    INTEGER :: i ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%hw ) ) THEN
          IF ( ASSOCIATED( this%hland ) ) THEN
             ok = ( this%hland      < MINVAL( this%hw(:) ) .AND. MAXVAL( this%hw(:) ) <= 11022.0_Double )
          ELSE
             ok = ( -10000.0_Double < MINVAL( this%hw(:) ) .AND. MAXVAL( this%hw(:) ) <= 11022.0_Double )
          END IF
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6260, c_upname, c_modname )
             WRITE(ctxt,'(G15.9)') MINVAL( this%hw(:) )
             CALL setup_error_act ( '<MinHu>', ctxt )
             WRITE(ctxt,'(G15.9)') MAXVAL( this%hw(:) )
             CALL setup_error_act ( '<MaxHu>', ctxt )
             IF ( ASSOCIATED( this%hland ) ) THEN
                WRITE(ctxt,'(G15.9)') this%hland
             ELSE
                WRITE(ctxt,'(G15.9)') -10000.0_Double
             END IF
             CALL setup_error_act ( '<HLand>', ctxt )
          END IF
       ELSE
          ok = .true.
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_hw_1
  !
  !! Pr&uuml;fe, ob die Komponente "ipobo" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_ipobo_1 &
       ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='ok_h_grid_ipobo_1'
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !! Z%auml;hlervariable
    INTEGER :: i ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%ipobo ) ) THEN
          ok = ( 0 <= MINVAL( this%ipobo(:) ) .AND. MAXVAL( this%ipobo(:) ) <= this%nv )
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6280, c_upname, c_modname )
             WRITE(ctxt,'(I10)') MINVAL( this%ipobo(:) )
             CALL setup_error_act ( '<MinIpobo>', ctxt )
             WRITE(ctxt,'(I10)') MAXVAL( this%ipobo(:) )
             CALL setup_error_act ( '<MaxIpobo>', ctxt )
             WRITE(ctxt,'(I10)') this%nv
             CALL setup_error_act ( '<AllMaxIpobo>', ctxt )
          END IF
       ELSE
          SELECT CASE ( get_h_grid_variant_no ( this ) )
          CASE(5) ! 'SELAFIN'
             ok = .true.
             IF (ASSOCIATED(this%ncsize)) THEN
                IF (get_ncsize_object (this) <= 0) THEN
                   ok = .false. 
                   CALL setup_error_act ( all_errors(:), 6510, c_upname, c_modname )
                   CALL setup_error_act ( '<component-name>', 'ipobo(:)' )
                ENDIF
             ELSE
                ok = .false. 
                CALL setup_error_act ( all_errors(:), 6510, c_upname, c_modname )
                CALL setup_error_act ( '<component-name>', 'ipobo(:)' )
             ENDIF
          CASE DEFAULT
             ok = .true.
          END SELECT
          !
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_ipobo_1
  !
  !! Pr&uuml;fe, ob die Komponente "knolg" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_knolg_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='ok_h_grid_knolg_1'
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !! Z%auml;hlervariable
    INTEGER :: i ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%knolg ) ) THEN
          ok = ( 1 <= MINVAL( this%knolg(:) )) 
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6285, c_upname, c_modname )
             WRITE(ctxt,'(I10)') MINVAL( this%knolg(:) )
             CALL setup_error_act ( '<MinKnolg>', ctxt )
          END IF
       ELSE
          SELECT CASE ( get_h_grid_variant_no ( this ) )
          CASE(5) ! 'SELAFIN'
             ok = .true.
             IF (ASSOCIATED(this%ncsize)) THEN
                IF (get_ncsize_object (this) > 0) THEN
                   ok = .false. 
                   CALL setup_error_act ( all_errors(:), 6540, c_upname, c_modname )
                ENDIF
             ENDIF
          CASE DEFAULT
             ok = .true.
          END SELECT
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_knolg_1
  !
  !! Pr&uuml;fe, ob die Komponente "quant" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_quant_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%quant ) ) THEN
       ok = ALL( ok_omi_quant ( this%quant(:) ) )
    ELSE
       ok = .true.
    END IF
    !
  END FUNCTION ok_h_grid_quant_1
  !
  !! Pr&uuml;fe, ob die Komponente "xyz" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_xyz_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%xyz ) ) THEN
       ok = ALL( ok_omi_xyz ( this%xyz(:) ) )
    ELSE
       ok = .true.
    END IF
    !
  END FUNCTION ok_h_grid_xyz_1
  !
  !! Pr&uuml;fe, ob die Komponente "dope" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_dope_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%dope ) ) THEN
       ok = ALL( ok_omi_dope ( this%dope(:) ) )
    ELSE
       ok = .true.
    END IF
    !
  END FUNCTION ok_h_grid_dope_1
  !
  !! Pr&uuml;fe, ob die Komponente "exch" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_exch_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%exch ) ) THEN
       ok = ALL( ok_omi_exch ( this%exch(:) ) )
    ELSE
       ok = .true.
    END IF
    !
  END FUNCTION ok_h_grid_exch_1
  !
  !! Pr&uuml;fe, ob die Komponente "span" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_span_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%span ) ) THEN
       ok = ok_omi_span ( this%span )
    ELSE
       ok = .true.
    END IF
    !
  END FUNCTION ok_h_grid_span_0
  !
  !! Pr&uuml;fe, ob die Komponente "stamp" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_stamp_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%stamp ) ) THEN
       ok = ok_omi_stamp ( this%stamp )
    ELSE
       ok = .true.
    END IF
    !
  END FUNCTION ok_h_grid_stamp_0
  !
  !! Pr&uuml;fe, ob die Komponente "space" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_space_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%space ) ) THEN
       ok = ok_omi_space ( this%space )
    ELSE
       ok = .true.
    END IF
    !
  END FUNCTION ok_h_grid_space_0
  !
  !! Pr&uuml;fe, ob die Komponente "ind" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_ind_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%ind ) ) THEN
       ok = ALL( ok_omi_ind ( this%ind(:) ) )
    ELSE
       ok = .true.
    END IF
    !
  END FUNCTION ok_h_grid_ind_1
  !
  !! Pr&uuml;fe, ob die Komponente "ele" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_ele_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !
    IF ( ASSOCIATED( this%ele ) ) THEN
       ok = ALL( ok_omi_ele ( this%ele(:) ) )
    ELSE
       ok = .true.
    END IF
    !
  END FUNCTION ok_h_grid_ele_1
  !
  !! Pr&uuml;fe, ob die Komponente "dxmin" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_dxmin_0 &
       ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='ok_h_grid_dxmin_0'
    !! Hilfsfeld
    CHARACTER (LEN=15) :: ctxt ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%dxmin ) ) THEN
          ok = ( 0.0_Double <= this%dxmin )
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6290, c_upname, c_modname )
             WRITE(ctxt,'(G15.9)') this%dxmin
             CALL setup_error_act ( '<AktDxmin>', ctxt )
          END IF
       ELSE
          ok = .true. 
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_dxmin_0
  !
  !! Pr&uuml;fe, ob die Komponente "nr" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_nr_0 &
       ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_h_grid_nr_0'
    !! Hifsvariable
    CHARACTER (LEN=10) :: ctxt ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%nr ) ) THEN
          ok = ( 0 <= this%nr .AND. this%nr <= this%ne )
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6270, c_upname, c_modname )
             WRITE(ctxt,'(I10)') this%nr
             CALL setup_error_act ( "<AktNr>", ctxt )
             WRITE(ctxt,'(I10)') this%ne
             CALL setup_error_act ( "<MaxNr>", ctxt )
          END IF
       ELSE
          SELECT CASE ( get_h_grid_variant_no ( this ) )
          CASE(3,4) ! 'UNTRIM_BAW,UNTRIM_VC'
             ok = .false. 
             CALL setup_error_act ( all_errors(:), 6510, c_upname, c_modname )
             CALL setup_error_act ( '<component-name>', 'nr' )
          CASE DEFAULT
             ok = .true.
          END SELECT
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_nr_0
  !
  !! Pr&uuml;fe, ob die Komponente "ncsize" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_ncsize_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='ok_h_grid_ncsize_0'
    !! Hifsvariable
    CHARACTER (LEN=10) :: ctxt ! 
    !! Testergebnis
    INTEGER, PARAMETER :: max_cpu = 100000
    !
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%ncsize ) ) THEN
          ok = ( 0 <= this%ncsize .AND. this%ncsize <= max_cpu )
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6275, c_upname, c_modname )
             WRITE(ctxt,'(I10)') this%ncsize
             CALL setup_error_act ( "<AktNr>", ctxt )
             WRITE(ctxt,'(I10)') max_cpu
             CALL setup_error_act ( "<MaxCPU>", ctxt )
          END IF
       ELSE
          SELECT CASE ( get_h_grid_variant_no ( this ) )
          CASE(5) ! 'SELAFIN'
             ok = .false. 
             CALL setup_error_act ( all_errors(:), 6510, c_upname, c_modname )
             CALL setup_error_act ( '<component-name>', 'ncsize' )
          CASE DEFAULT
             ok = .true.
          END SELECT
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_ncsize_0
  !
  !! Pr&uuml;fe, ob die Komponente "b_ms(:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_b_ms_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='ok_h_grid_b_ms_1' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch      ! 
    LOGICAL            :: l_ok(2) ! 
    INTEGER            :: i       ! 
    !
    l_ok(:) = .true. 
    IF ( ASSOCIATED( this%b_ms ) ) THEN
       l_ok(1) = ALL( this%b_ms(:) > 0 )
       DO i=1,MAXVAL( this%b_ms )
          IF ( .NOT. l_ok(2) ) EXIT
          l_ok(2) = ( COUNT( this%b_ms(:) == i ) > 0 )
       END DO
    END IF
    ok = ALL( l_ok )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6550, c_upname, c_modname )
       WRITE(ch,'(I10)') MINVAL( this%b_ms ) ; CALL setup_error_act ( '<min>', ch )
       WRITE(ch,'(I10)') MAXVAL( this%b_ms ) ; CALL setup_error_act ( '<max>', ch )
       WRITE(ch,'(L1)') l_ok(2) ; CALL setup_error_act ( '<ok>', ch(1:1) )
    END IF
    !
  END FUNCTION ok_h_grid_b_ms_1
  !
  !! Pr&uuml;fe, ob die Komponente "b_ss(:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_b_ss_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='ok_h_grid_b_ss_1' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch      ! 
    LOGICAL            :: l_as(2) ! 
    INTEGER            :: l_ns(2) ! 
    INTEGER            :: i       ! 
    !
    l_ns(:) = -1
    l_as(1) = ASSOCIATED( this%b_ms )
    l_as(2) = ASSOCIATED( this%b_ss )
    IF ( l_as(1) ) l_ns(1) = SIZE( this%b_ms )
    IF ( l_as(2) ) l_ns(2) = SIZE( this%b_ss )
    ok = ( ALL(.NOT.l_as) .OR. ( ALL(l_as) .AND. ALL(l_ns==l_ns(1)) ) )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6560, c_upname, c_modname )
       WRITE(ch,'(L1)' ) l_as(1) ; CALL setup_error_act ( '<a-ms>', ch(1:1) )
       WRITE(ch,'(L1)' ) l_as(2) ; CALL setup_error_act ( '<a-ss>', ch(1:1) )
       WRITE(ch,'(I10)') l_ns(1) ; CALL setup_error_act ( '<s-ms>', ch )
       WRITE(ch,'(I10)') l_ns(2) ; CALL setup_error_act ( '<s-ss>', ch )
    END IF
    !
  END FUNCTION ok_h_grid_b_ss_1
  !
  !! Pr&uuml;fe, ob die Komponente "b_s(:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_b_s_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_h_grid_b_s_1' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch      ! 
    LOGICAL            :: l_as(2) ! 
    INTEGER            :: l_ns(2) ! 
    INTEGER            :: i       ! 
    !
    l_ns(:) = -1
    l_as(1) = ASSOCIATED( this%b_ms )
    l_as(2) = ASSOCIATED( this%b_s  )
    IF ( l_as(1) ) l_ns(1) = SIZE( this%b_ms )
    IF ( l_as(2) ) l_ns(2) = SIZE( this%b_s  )
    ok = ( ALL(.NOT.l_as) .OR. ( ALL(l_as) .AND. ALL(l_ns==l_ns(1)) ) )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6570, c_upname, c_modname )
       WRITE(ch,'(L1)' ) l_as(1) ; CALL setup_error_act ( '<a-ms>', ch(1:1) )
       WRITE(ch,'(L1)' ) l_as(2) ; CALL setup_error_act ( '<a-s>', ch(1:1) )
       WRITE(ch,'(I10)') l_ns(1) ; CALL setup_error_act ( '<s-ms>', ch )
       WRITE(ch,'(I10)') l_ns(2) ; CALL setup_error_act ( '<s-s>', ch )
    END IF
    !
  END FUNCTION ok_h_grid_b_s_1
  !
  !! Pr&uuml;fe, ob die Komponente "b_v(:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_b_v_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_h_grid_b_v_1' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch      ! 
    LOGICAL            :: l_as(2) ! 
    INTEGER            :: l_ns(2) ! 
    INTEGER            :: i       ! 
    !
    l_ns(:) = -1
    l_as(1) = ASSOCIATED( this%b_ms )
    l_as(2) = ASSOCIATED( this%b_v  )
    IF ( l_as(1) ) l_ns(1) = SIZE( this%b_ms )
    IF ( l_as(2) ) l_ns(2) = SIZE( this%b_v  )
    ok = ( ALL(.NOT.l_as) .OR. ( ALL(l_as) .AND. ALL(l_ns==l_ns(1)) ) )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6580, c_upname, c_modname )
       WRITE(ch,'(L1)' ) l_as(1) ; CALL setup_error_act ( '<a-ms>', ch(1:1) )
       WRITE(ch,'(L1)' ) l_as(2) ; CALL setup_error_act ( '<a-v>', ch(1:1) )
       WRITE(ch,'(I10)') l_ns(1) ; CALL setup_error_act ( '<s-ms>', ch )
       WRITE(ch,'(I10)') l_ns(2) ; CALL setup_error_act ( '<s-v>', ch )
    END IF
    !
  END FUNCTION ok_h_grid_b_v_1
  !
  !! Pr&uuml;fe, ob die Komponente "b_t(:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_b_t_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_h_grid_b_t_1' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ch      ! 
    LOGICAL            :: l_as(2) ! 
    INTEGER            :: l_ns(2) ! 
    INTEGER            :: i       ! 
    !
    l_ns(:) = -1
    l_as(1) = ASSOCIATED( this%b_ms )
    l_as(2) = ASSOCIATED( this%b_t  )
    IF ( l_as(1) ) l_ns(1) = SIZE( this%b_ms )
    IF ( l_as(2) ) l_ns(2) = SIZE( this%b_t  )
    ok = ( ALL(.NOT.l_as) .OR. ( ALL(l_as) .AND. ALL(l_ns==l_ns(1)) ) )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6590, c_upname, c_modname )
       WRITE(ch,'(L1)' ) l_as(1) ; CALL setup_error_act ( '<a-ms>', ch(1:1) )
       WRITE(ch,'(L1)' ) l_as(2) ; CALL setup_error_act ( '<a-t>', ch(1:1) )
       WRITE(ch,'(I10)') l_ns(1) ; CALL setup_error_act ( '<s-ms>', ch )
       WRITE(ch,'(I10)') l_ns(2) ; CALL setup_error_act ( '<s-t>', ch )
    END IF
    !
  END FUNCTION ok_h_grid_b_t_1
  !
  !! Pr&uuml;fe, ob die Komponente "m" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_m_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=13) , PARAMETER :: c_upname='ok_h_grid_m_0' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ctxt ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%m ) ) THEN
          ok = ( 3 <= this%m )
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6300, c_upname, c_modname )
             WRITE(ctxt,'(I10)') this%m ; CALL setup_error_act ( "<AktM>", ctxt )
          END IF
       ELSE
          SELECT CASE ( get_h_grid_variant_no ( this ) )
          CASE(6) ! 'DELFT3D'
             ok = .false. 
             CALL setup_error_act ( all_errors(:), 6510, c_upname, c_modname )
             CALL setup_error_act ( '<component-name>', 'm' )
          CASE DEFAULT
             ok = .true.
          END SELECT
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_m_0
  !
  !! Pr&uuml;fe, ob die Komponente "n" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_n_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=13) , PARAMETER :: c_upname='ok_h_grid_n_0' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ctxt ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%n ) ) THEN
          ok = ( 3 <= this%n )
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6300, c_upname, c_modname )
             WRITE(ctxt,'(I10)') this%n ; CALL setup_error_act ( "<AktN>", ctxt )
          END IF
       ELSE
          SELECT CASE ( get_h_grid_variant_no ( this ) )
          CASE(6) ! 'DELFT3D'
             ok = .false. 
             CALL setup_error_act ( all_errors(:), 6510, c_upname, c_modname )
             CALL setup_error_act ( '<component-name>', 'n' )
          CASE DEFAULT
             ok = .true.
          END SELECT
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_n_0
  !
  !! Pr&uuml;fe, ob die Komponente "enc(:,:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_enc_2 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_h_grid_enc_2' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ctxt    ! 
    LOGICAL            :: l_ok(4) ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%enc ) ) THEN
          IF ( ASSOCIATED( this%m ) .AND. ASSOCIATED( this%n ) ) THEN
             l_ok(1) = ( MINVAL( this%enc(:,1) ) >= 1        )
             l_ok(2) = ( MAXVAL( this%enc(:,1) ) <= this%m+1 )
             l_ok(3) = ( MINVAL( this%enc(:,2) ) >= 1        )
             l_ok(4) = ( MAXVAL( this%enc(:,2) ) <= this%n+1 )
             ok = ALL( l_ok )
             IF ( .NOT. ok ) THEN
                CALL setup_error_act ( all_errors(:), 6320, c_upname, c_modname )
                WRITE(ctxt(1:1),'(L1)') l_ok(1) ; CALL setup_error_act ( '<ok-mn>', ctxt(1:1) )
                WRITE(ctxt(1:1),'(L1)') l_ok(2) ; CALL setup_error_act ( '<ok-mx>', ctxt(1:1) )
                WRITE(ctxt(1:1),'(L1)') l_ok(3) ; CALL setup_error_act ( '<ok-nn>', ctxt(1:1) )
                WRITE(ctxt(1:1),'(L1)') l_ok(4) ; CALL setup_error_act ( '<ok-nx>', ctxt(1:1) )
                WRITE(ctxt,'(I10)') MINVAL(this%enc(:,1)) ; CALL setup_error_act ( '<mn>', ctxt )  
                WRITE(ctxt,'(I10)') MAXVAL(this%enc(:,1)) ; CALL setup_error_act ( '<mx>', ctxt )
                WRITE(ctxt,'(I10)') this%m+1 ; CALL setup_error_act ( '<m>', ctxt )
                WRITE(ctxt,'(I10)') MINVAL(this%enc(:,2)) ; CALL setup_error_act ( '<nn>', ctxt )  
                WRITE(ctxt,'(I10)') MAXVAL(this%enc(:,2)) ; CALL setup_error_act ( '<nx>', ctxt )
                WRITE(ctxt,'(I10)') this%n+1 ; CALL setup_error_act ( '<n>', ctxt )
             END IF
          END IF
       ELSE
          SELECT CASE ( get_h_grid_variant_no ( this ) )
          CASE(6) ! 'DELFT3D'
             ok = .false. 
             CALL setup_error_act ( all_errors(:), 6510, c_upname, c_modname )
             CALL setup_error_act ( '<component-name>', 'enc(:,:)' )
          CASE DEFAULT
             ok = .true.
          END SELECT
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_enc_2
  !
  !! Pr&uuml;fe, ob die Komponente "bnd(:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_bnd_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_h_grid_bnd_1' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ctxt    !  
    INTEGER            :: i, mi   ! 
    LOGICAL            :: l_ok(4) ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%bnd ) .AND. ASSOCIATED( this%m ) .AND. ASSOCIATED( this%n ) ) THEN
          ok = .true.
          DO i=1,SIZE(this%bnd)
             IF ( .NOT. ok ) EXIT
             l_ok(1) = ANY( (/ 'Z', 'C', 'Q', 'T', 'R' /) == this%bnd(i)%bdry_type )
             l_ok(2) = ANY( (/ 'A', 'H', 'Q', 'T'      /) == this%bnd(i)%data_type )
             l_ok(3) = ( ALL( this%bnd(i)%grid_coor(1:3:2) >= 1 ) .AND. &
                         ALL( this%bnd(i)%grid_coor(1:3:2) <= this%m+1 ) )
             l_ok(4) = ( ALL( this%bnd(i)%grid_coor(2:4:2) >= 1 ) .AND. &
                         ALL( this%bnd(i)%grid_coor(2:4:2) <= this%n+1 ) )
             mi      = i
             ok      = ALL( l_ok )
          END DO
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6350, c_upname, c_modname )
             WRITE(ctxt(1:1),'(L1)') l_ok(1) ; CALL setup_error_act ( '<ok-bd>', ctxt(1:1) )
             WRITE(ctxt(1:1),'(L1)') l_ok(2) ; CALL setup_error_act ( '<ok-da>', ctxt(1:1) )
             WRITE(ctxt(1:1),'(L1)') l_ok(3) ; CALL setup_error_act ( '<ok-m>', ctxt(1:1) )
             WRITE(ctxt(1:1),'(L1)') l_ok(4) ; CALL setup_error_act ( '<ok-n>', ctxt(1:1) )
             WRITE(ctxt(1:1),'(A1)') this%bnd(mi)%bdry_type    ; CALL setup_error_act ( '<bd>', ctxt(1:1) )
             WRITE(ctxt(1:1),'(A1)') this%bnd(mi)%data_type    ; CALL setup_error_act ( '<da>', ctxt(1:1) )
             WRITE(ctxt,'(I10)'    ) this%bnd(mi)%grid_coor(1) ; CALL setup_error_act ( '<m1>', ctxt )
             WRITE(ctxt,'(I10)'    ) this%bnd(mi)%grid_coor(2) ; CALL setup_error_act ( '<n1>', ctxt )
             WRITE(ctxt,'(I10)'    ) this%bnd(mi)%grid_coor(3) ; CALL setup_error_act ( '<m2>', ctxt )
             WRITE(ctxt,'(I10)'    ) this%bnd(mi)%grid_coor(4) ; CALL setup_error_act ( '<n2>', ctxt )
             WRITE(ctxt,'(I10)'    ) mi                        ; CALL setup_error_act ( '<nr>', ctxt )
          END IF
       ELSE
          ok = .true.
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_bnd_1
  !
  !! Pr&uuml;fe, ob die Komponente "thd(:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_thd_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_h_grid_thd_1' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ctxt    ! 
    INTEGER            :: i, mi   ! 
    LOGICAL            :: l_ok(3) ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%thd ) .AND. ASSOCIATED( this%m ) .AND. ASSOCIATED( this%n ) ) THEN
          ok = .true.
          DO i=1,SIZE(this%thd)
             IF ( .NOT. ok ) EXIT
             l_ok(1) = ANY( (/ 'U', 'V' /) == this%thd(i)%type )
             l_ok(2) = ( ALL( this%thd(i)%grid_coor(1:3:2) >= 2 ) .AND. &
                         ALL( this%thd(i)%grid_coor(1:3:2) <= this%m-1 ) )
             l_ok(3) = ( ALL( this%thd(i)%grid_coor(2:4:2) >= 2 ) .AND. &
                         ALL( this%thd(i)%grid_coor(2:4:2) <= this%n-1 ) )
             mi      = i
             ok      = ALL( l_ok )
          END DO
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6360, c_upname, c_modname )
             WRITE(ctxt(1:1),'(L1)') l_ok(1) ; CALL setup_error_act ( '<ok-ty>', ctxt(1:1) )
             WRITE(ctxt(1:1),'(L1)') l_ok(2) ; CALL setup_error_act ( '<ok-m>', ctxt(1:1) )
             WRITE(ctxt(1:1),'(L1)') l_ok(3) ; CALL setup_error_act ( '<ok-n>', ctxt(1:1) )
             WRITE(ctxt(1:1),'(A1)') this%thd(mi)%type         ; CALL setup_error_act ( '<ty>', ctxt(1:1) )
             WRITE(ctxt,'(I10)'    ) this%thd(mi)%grid_coor(1) ; CALL setup_error_act ( '<m1>', ctxt )
             WRITE(ctxt,'(I10)'    ) this%thd(mi)%grid_coor(2) ; CALL setup_error_act ( '<n1>', ctxt )
             WRITE(ctxt,'(I10)'    ) this%thd(mi)%grid_coor(3) ; CALL setup_error_act ( '<m2>', ctxt )
             WRITE(ctxt,'(I10)'    ) this%thd(mi)%grid_coor(4) ; CALL setup_error_act ( '<n2>', ctxt )
             WRITE(ctxt,'(I10)'    ) mi                        ; CALL setup_error_act ( '<nr>', ctxt )
          END IF
       ELSE
          ok = .true.
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_thd_1
  !
  !! Pr&uuml;fe, ob die Komponente "lwl(:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_lwl_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_h_grid_lwl_1' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ctxt    ! 
    INTEGER            :: i, mi   ! 
    LOGICAL            :: l_ok(4) ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%lwl ) .AND. ASSOCIATED( this%m ) .AND. ASSOCIATED( this%n ) ) THEN
          ok = .true.
          DO i=1,SIZE(this%lwl)
             IF ( .NOT. ok ) EXIT
             l_ok(1) = ANY( (/ 'U', 'V' /) == this%lwl(i)%type )
             l_ok(2) = ( ALL( this%lwl(i)%grid_coor(1:3:2) >= 2 ) .AND. &
                         ALL( this%lwl(i)%grid_coor(1:3:2) <= this%m-1 ) )
             l_ok(3) = ( ALL( this%lwl(i)%grid_coor(2:4:2) >= 2 ) .AND. &
                         ALL( this%lwl(i)%grid_coor(2:4:2) <= this%n-1 ) )
             l_ok(4) = ( this%lwl(i)%frction >= 0.0_Double )
             mi      = i
             ok      = ALL( l_ok )
          END DO
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6370, c_upname, c_modname )
             WRITE(ctxt(1:1),'(L1)') l_ok(1) ; CALL setup_error_act ( '<ok-ty>', ctxt(1:1) )
             WRITE(ctxt(1:1),'(L1)') l_ok(2) ; CALL setup_error_act ( '<ok-m>', ctxt(1:1) )
             WRITE(ctxt(1:1),'(L1)') l_ok(3) ; CALL setup_error_act ( '<ok-n>', ctxt(1:1) )
             WRITE(ctxt(1:1),'(L1)') l_ok(4) ; CALL setup_error_act ( '<ok-fr>', ctxt(1:1) )
             WRITE(ctxt(1:1),'(A1)') this%lwl(mi)%type         ; CALL setup_error_act ( '<ty>', ctxt(1:1) )
             WRITE(ctxt,'(I10)'    ) this%lwl(mi)%grid_coor(1) ; CALL setup_error_act ( '<m1>', ctxt )
             WRITE(ctxt,'(I10)'    ) this%lwl(mi)%grid_coor(2) ; CALL setup_error_act ( '<n1>', ctxt )
             WRITE(ctxt,'(I10)'    ) this%lwl(mi)%grid_coor(3) ; CALL setup_error_act ( '<m2>', ctxt )
             WRITE(ctxt,'(I10)'    ) this%lwl(mi)%grid_coor(4) ; CALL setup_error_act ( '<n2>', ctxt )
             WRITE(ctxt,'(F10.5)'  ) this%lwl(mi)%frction     ; CALL setup_error_act ( '<friction>', ctxt )
             WRITE(ctxt,'(I10)'    ) mi                        ; CALL setup_error_act ( '<nr>', ctxt )
          END IF
       ELSE
          ok = .true.
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_lwl_1
  !
  !! Pr&uuml;fe, ob die Komponente "ext(:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_ext_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_h_grid_ext_1' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ctxt    ! 
    INTEGER            :: i, mi   ! 
    LOGICAL            :: l_ok(4) ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%ext ) .AND. ASSOCIATED( this%m ) .AND. ASSOCIATED( this%n ) ) THEN
          ok = .true.
          DO i=1,SIZE(this%ext)
             IF ( .NOT. ok ) EXIT
             l_ok(1) = ANY( (/ 'U', 'V' /) == this%ext(i)%type )
             l_ok(2) = ( ALL( this%ext(i)%grid_coor(1:3:2) >= 2 ) .AND. &
                         ALL( this%ext(i)%grid_coor(1:3:2) <= this%m-1 ) )
             l_ok(3) = ( ALL( this%ext(i)%grid_coor(2:4:2) >= 2 ) .AND. &
                         ALL( this%ext(i)%grid_coor(2:4:2) <= this%n-1 ) )
             l_ok(4) = ( this%ext(i)%frction >= 0.0_Double )
             mi      = i
             ok      = ALL( l_ok )
          END DO
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6380, c_upname, c_modname )
             WRITE(ctxt(1:1),'(L1)') l_ok(1) ; CALL setup_error_act ( '<ok-ty>', ctxt(1:1) )
             WRITE(ctxt(1:1),'(L1)') l_ok(2) ; CALL setup_error_act ( '<ok-m>', ctxt(1:1) )
             WRITE(ctxt(1:1),'(L1)') l_ok(3) ; CALL setup_error_act ( '<ok-n>', ctxt(1:1) )
             WRITE(ctxt(1:1),'(L1)') l_ok(4) ; CALL setup_error_act ( '<ok-fr>', ctxt(1:1) )
             WRITE(ctxt(1:1),'(A1)') this%ext(mi)%type         ; CALL setup_error_act ( '<ty>', ctxt(1:1) )
             WRITE(ctxt,'(I10)'    ) this%ext(mi)%grid_coor(1) ; CALL setup_error_act ( '<m1>', ctxt )
             WRITE(ctxt,'(I10)'    ) this%ext(mi)%grid_coor(2) ; CALL setup_error_act ( '<n1>', ctxt )
             WRITE(ctxt,'(I10)'    ) this%ext(mi)%grid_coor(3) ; CALL setup_error_act ( '<m2>', ctxt )
             WRITE(ctxt,'(I10)'    ) this%ext(mi)%grid_coor(4) ; CALL setup_error_act ( '<n2>', ctxt )
             WRITE(ctxt,'(F10.5)'  ) this%ext(mi)%frction     ; CALL setup_error_act ( '<friction>', ctxt )
             WRITE(ctxt,'(I10)'    ) mi                        ; CALL setup_error_act ( '<nr>', ctxt )
          END IF
       ELSE
          ok = .true.
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_ext_1
  !
  !! Pr&uuml;fe, ob die Komponente "dry(:,:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_dry_2 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_h_grid_dry_2' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ctxt    ! 
    LOGICAL            :: l_ok(5) ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%dry ) ) THEN
          IF ( ASSOCIATED( this%m ) .AND. ASSOCIATED( this%n ) ) THEN
             l_ok(1) = ( MINVAL( this%dry(:,1:3:2) ) >= 2        )
             l_ok(2) = ( MAXVAL( this%dry(:,1:3:2) ) <= this%m+1 )
             l_ok(3) = ( MINVAL( this%dry(:,2:4:2) ) >= 2        )
             l_ok(4) = ( MAXVAL( this%dry(:,2:4:2) ) <= this%n+1 )
             l_ok(5) = ( SIZE(this%dry,2) == c_max_d3d_dry )
             ok = ALL( l_ok )
             IF ( .NOT. ok ) THEN
                CALL setup_error_act ( all_errors(:), 6330, c_upname, c_modname )
                WRITE(ctxt(1:1),'(L1)') l_ok(1) ; CALL setup_error_act ( '<ok-mn>', ctxt(1:1) )
                WRITE(ctxt(1:1),'(L1)') l_ok(2) ; CALL setup_error_act ( '<ok-mx>', ctxt(1:1) )
                WRITE(ctxt(1:1),'(L1)') l_ok(3) ; CALL setup_error_act ( '<ok-nn>', ctxt(1:1) )
                WRITE(ctxt(1:1),'(L1)') l_ok(4) ; CALL setup_error_act ( '<ok-nx>', ctxt(1:1) )
                WRITE(ctxt(1:1),'(L1)') l_ok(5) ; CALL setup_error_act ( '<ok-dry>', ctxt(1:1) )
                WRITE(ctxt,'(I10)') MINVAL(this%dry(:,1)) ; CALL setup_error_act ( '<mn>', ctxt )  
                WRITE(ctxt,'(I10)') MAXVAL(this%dry(:,1)) ; CALL setup_error_act ( '<mx>', ctxt )
                WRITE(ctxt,'(I10)') this%m+1 ; CALL setup_error_act ( '<m>', ctxt )
                WRITE(ctxt,'(I10)') MINVAL(this%dry(:,2)) ; CALL setup_error_act ( '<nn>', ctxt )  
                WRITE(ctxt,'(I10)') MAXVAL(this%dry(:,2)) ; CALL setup_error_act ( '<nx>', ctxt )
                WRITE(ctxt,'(I10)') this%n+1 ; CALL setup_error_act ( '<n>', ctxt )
                WRITE(ctxt,'(I10)') SIZE(this%dry,2) ; CALL setup_error_act ( '<size-dry>', ctxt )
             END IF
          END IF
       ELSE
          ok = .true.
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_dry_2
  !
  !! Pr&uuml;fe, ob die Komponente "isbnd(:,:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_isbnd_2 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='ok_h_grid_isbnd_2' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ctxt            ! 
    LOGICAL            :: l_ok(3)         ! 
    INTEGER            :: i, nbnd, mx, mn ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%isbnd ) ) THEN
          nbnd = 0
          IF ( ASSOCIATED( this%bnd ) ) nbnd = SIZE(this%bnd)
          mn      = MINVAL(this%isbnd)
          mn      = MIN( 0, mn )
          mx      = MAXVAL(this%isbnd)
          l_ok(1) = ( SIZE(this%isbnd,1) == this%ne )
          l_ok(2) = ( mn >= 0    )
          l_ok(3) = ( mx <= nbnd )
          ok      = ALL( l_ok )
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6340, c_upname, c_modname )
                WRITE(ctxt(1:1),'(L1)') l_ok(1) ; CALL setup_error_act ( '<ok-ne>', ctxt(1:1) )
                WRITE(ctxt(1:1),'(L1)') l_ok(2) ; CALL setup_error_act ( '<ok-mn>', ctxt(1:1) )
                WRITE(ctxt(1:1),'(L1)') l_ok(3) ; CALL setup_error_act ( '<ok-mx>', ctxt(1:1) )
                WRITE(ctxt,'(I10)') this%ne          ; CALL setup_error_act ( '<ne>', ctxt )
                WRITE(ctxt,'(I10)') mn               ; CALL setup_error_act ( '<mn>', ctxt )
                WRITE(ctxt,'(I10)') mx               ; CALL setup_error_act ( '<mx>', ctxt )
                WRITE(ctxt,'(I10)') SIZE(this%bnd,1) ; CALL setup_error_act ( '<size-isbnd>', ctxt )
                WRITE(ctxt,'(I10)') nbnd             ; CALL setup_error_act ( '<size-bnd>', ctxt )
          END IF
       ELSE
          ok = .true.
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_isbnd_2
  !
  !! Pr&uuml;fe, ob die Komponente "isdam(:,:,:)" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_isdam_3 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='ok_h_grid_isdam_3' ! 
    !! Parameter
    CHARACTER ( LEN=3) , PARAMETER :: c_dam(3) = (/ 'thd', 'lwl', 'ext' /) ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ctxt                       ! 
    LOGICAL            :: l_ok(5)                    ! 
    INTEGER            :: i, j, ndam, mx, mn, kx, kn ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       IF ( ASSOCIATED( this%isdam ) ) THEN
          DO j=1,3
             ndam = 0 
             SELECT CASE ( j )
             CASE ( 1 )
                IF ( ASSOCIATED( this%thd ) ) ndam = SIZE(this%thd)
             CASE ( 2 )
                IF ( ASSOCIATED( this%lwl ) ) ndam = SIZE(this%lwl)
             CASE ( 3 )
                IF ( ASSOCIATED( this%ext ) ) ndam = SIZE(this%ext)
             END SELECT
             IF ( ANY( this%isdam(:,:,2) == j ) ) THEN
                mn = MINVAL(this%isdam(:,:,1), this%isdam(:,:,2) == j )
                mx = MAXVAL(this%isdam(:,:,1), this%isdam(:,:,2) == j )
                kn = MINVAL(this%isdam(:,:,2), this%isdam(:,:,2) == j )
                kx = MAXVAL(this%isdam(:,:,2), this%isdam(:,:,2) == j )
             ELSE
                mn = 0
                mx = 0
                kn = 0
                kx = 0
             END IF
             l_ok(1) = ( SIZE(this%isdam,1) == this%ne )
             l_ok(2) = ( mn >= 0    )
             l_ok(3) = ( mx <= ndam )
             IF ( ndam > 0 ) THEN
                l_ok(4) = ( kn >= 1    )
                l_ok(5) = ( kx <= 3    )
             ELSE
                l_ok(4:5) = .true.
             END IF
             ok      = ALL( l_ok )
             IF ( .NOT. ok ) THEN
                CALL setup_error_act ( all_errors(:), 6390, c_upname, c_modname )
                CALL setup_error_act ( '<dam>', c_dam(j) )
                WRITE(ctxt(1:1),'(L1)') l_ok(1) ; CALL setup_error_act ( '<ok-ne>', ctxt(1:1) )
                WRITE(ctxt(1:1),'(L1)') l_ok(2) ; CALL setup_error_act ( '<ok-mn>', ctxt(1:1) )
                WRITE(ctxt(1:1),'(L1)') l_ok(3) ; CALL setup_error_act ( '<ok-mx>', ctxt(1:1) )
                WRITE(ctxt(1:1),'(L1)') l_ok(4) ; CALL setup_error_act ( '<ok-kn>', ctxt(1:1) )
                WRITE(ctxt(1:1),'(L1)') l_ok(5) ; CALL setup_error_act ( '<ok-kx>', ctxt(1:1) )
                WRITE(ctxt,'(I10)') this%ne     ; CALL setup_error_act ( '<ne>', ctxt )
                WRITE(ctxt,'(I10)') mn          ; CALL setup_error_act ( '<mn>', ctxt )
                WRITE(ctxt,'(I10)') mx          ; CALL setup_error_act ( '<mx>', ctxt )
                WRITE(ctxt,'(I10)') kn          ; CALL setup_error_act ( '<kn>', ctxt )
                WRITE(ctxt,'(I10)') kx          ; CALL setup_error_act ( '<kx>', ctxt )
                WRITE(ctxt,'(I10)') ndam        ; CALL setup_error_act ( '<size-ndam>', ctxt )
                WRITE(ctxt,'(I10)') SIZE(this%isdam,1) ; CALL setup_error_act ( '<size-isdam1>', ctxt )
                WRITE(ctxt,'(I10)') SIZE(this%isdam,2) ; CALL setup_error_act ( '<size-isdam2>', ctxt )
                WRITE(ctxt,'(I10)') SIZE(this%isdam,3) ; CALL setup_error_act ( '<size-isdam3>', ctxt )
             END IF
          END DO
       ELSE
          ok = .true.
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_isdam_3
  !
  !! Pr&uuml;fe, ob die Komponente "huu" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_huu_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_h_grid_huu_1'
    !! Hilfsfeld
    CHARACTER (LEN=15) :: ctxt ! 
    !! Z%auml;hlervariable
    INTEGER :: i ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       ok = .true.
       IF ( ASSOCIATED( this%huu ) ) THEN
          ok = ( -10001.0_Double <= MINVAL( this%huu(:) ) .AND. MAXVAL( this%huu(:) ) <= 11022.0_Double )
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6250, c_upname, c_modname )
             CALL setup_error_act ( '<component-name>', 'huu(:)' )
             WRITE(ctxt,'(G15.9)') MINVAL( this%huu(:) ) ; CALL setup_error_act ( '<Min>', ctxt )
             WRITE(ctxt,'(G15.9)') MAXVAL( this%huu(:) ) ; CALL setup_error_act ( '<Max>', ctxt )
          ELSE
             IF ( ASSOCIATED( this%hu ) ) THEN
                ok = ALL( this%huu(:) >= this%hu(:) ) 
                IF ( .NOT. ok ) THEN
                   CALL setup_error_act ( all_errors(:), 6400, c_upname, c_modname )
                   CALL setup_error_act ( '<component-name>', 'huu(:)' )
                   CALL setup_error_act ( '<comp-1>', 'huu(:)' )
                   CALL setup_error_act ( '<comp-2>', 'hu(:)' )
                END IF
             END IF
          END IF
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_huu_1
  !
  !! Pr&uuml;fe, ob die Komponente "hvu" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_hvu_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_h_grid_hvu_1'
    !! Hilfsfeld
    CHARACTER (LEN=15) :: ctxt ! 
    !! Z%auml;hlervariable
    INTEGER :: i ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       ok = .true.
       IF ( ASSOCIATED( this%hvu ) ) THEN
          ok = ( -10001.0_Double <= MINVAL( this%hvu(:) ) .AND. MAXVAL( this%hvu(:) ) <= 11022.0_Double )
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6250, c_upname, c_modname )
             CALL setup_error_act ( '<component-name>', 'hvu(:)' )
             WRITE(ctxt,'(G15.9)') MINVAL( this%hvu(:) ) ; CALL setup_error_act ( '<Min>', ctxt )
             WRITE(ctxt,'(G15.9)') MAXVAL( this%hvu(:) ) ; CALL setup_error_act ( '<Max>', ctxt )
          ELSE
             IF ( ASSOCIATED( this%hv ) ) THEN
                ok = ALL( this%hvu(:) >= this%hv(:) ) 
                IF ( .NOT. ok ) THEN
                   CALL setup_error_act ( all_errors(:), 6400, c_upname, c_modname )
                   CALL setup_error_act ( '<component-name>', 'hvu(:)' )
                   CALL setup_error_act ( '<comp-1>', 'hvu(:)' )
                   CALL setup_error_act ( '<comp-2>', 'hv(:)' )
                END IF
             END IF
          END IF
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_hvu_1
  !
  !! Pr&uuml;fe, ob die Komponente "hwu" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_hwu_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_h_grid_hwu_1'
    !! Hilfsfeld
    CHARACTER (LEN=15) :: ctxt ! 
    !! Z%auml;hlervariable
    INTEGER :: i ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       ok = .true.
       IF ( ASSOCIATED( this%hwu ) ) THEN
          ok = ( -10001.0_Double <= MINVAL( this%hwu(:) ) .AND. MAXVAL( this%hwu(:) ) <= 11022.0_Double )
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6250, c_upname, c_modname )
             CALL setup_error_act ( '<component-name>', 'hwu(:)' )
             WRITE(ctxt,'(G15.9)') MINVAL( this%hwu(:) ) ; CALL setup_error_act ( '<Min>', ctxt )
             WRITE(ctxt,'(G15.9)') MAXVAL( this%hwu(:) ) ; CALL setup_error_act ( '<Max>', ctxt )
          ELSE
             IF ( ASSOCIATED( this%hw ) ) THEN
                ok = ALL( this%hwu(:) >= this%hw(:) ) 
                IF ( .NOT. ok ) THEN
                   CALL setup_error_act ( all_errors(:), 6400, c_upname, c_modname )
                   CALL setup_error_act ( '<component-name>', 'hwu(:)' )
                   CALL setup_error_act ( '<comp-1>', 'hwu(:)' )
                   CALL setup_error_act ( '<comp-2>', 'hw(:)' )
                END IF
             END IF
          END IF
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_hwu_1
  !
  !! Pr&uuml;fe, ob die Komponente "dwlp" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_dwlp_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='ok_h_grid_dwlp_0'
    !! Hilfsfeld
    CHARACTER (LEN=15) :: ctxt ! 
    INTEGER :: i ! 
    !
    SELECT CASE ( get_h_grid_variant_no ( this ) )
    CASE(1,2,3,4,5,6)
       ok = .true.
       IF ( ASSOCIATED( this%dwlp ) ) THEN
          ok = ANY( this%dwlp == c_dwlp_valid )
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6600, c_upname, c_modname )
             CALL setup_error_act ( '<act>', this%dwlp )
             CALL setup_error_act ( '<req1>', c_dwlp_valid(1) )
             CALL setup_error_act ( '<req2>', c_dwlp_valid(2) )
             CALL setup_error_act ( '<req3>', c_dwlp_valid(3) )
          END IF
       END IF
    CASE DEFAULT
       ok = .true.
       WRITE(*,*) ' *** code missing '//TRIM(c_upname)
    END SELECT
    !
  END FUNCTION ok_h_grid_dwlp_0
  !
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE init_h_grid_object_0 ( this )
    !! Package-Objekt des Typs "t_h_grid_list"
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='init_h_grid_object_0'
    !
    ! initialisieren der statischen Komponenten
    this%id   = -1
    this%name = REPEAT( ' ', LEN( this%name ) )
    this%name = 'UNDEFINED'
    !
    CALL new_file ( this%file )
    !
    ! initialisieren der restlichen Komponenten (immer als Pointer)
    NULLIFY ( this%nv    )
    NULLIFY ( this%ns    )
    NULLIFY ( this%nsi   )
    NULLIFY ( this%nsf   )
    NULLIFY ( this%ne    )
    NULLIFY ( this%xy    )
    NULLIFY ( this%nen   )
    NULLIFY ( this%irand )
    NULLIFY ( this%ks    )
    NULLIFY ( this%hv    )
    NULLIFY ( this%nrand )
    NULLIFY ( this%nptfr )
    NULLIFY ( this%nptir )
    NULLIFY ( this%time  )
    NULLIFY ( this%nbc   )
    NULLIFY ( this%hland )
    NULLIFY ( this%angle )
    NULLIFY ( this%text  )
    NULLIFY ( this%jb    )
    NULLIFY ( this%jt    )
    NULLIFY ( this%is    )
    NULLIFY ( this%je    )
    NULLIFY ( this%ie    )
    NULLIFY ( this%xs    )
    NULLIFY ( this%xc    )
    NULLIFY ( this%xg    )
    NULLIFY ( this%dx    )
    NULLIFY ( this%dy    )
    NULLIFY ( this%dg    )
    NULLIFY ( this%aa    )
    NULLIFY ( this%hu    )
    NULLIFY ( this%hw    )
    NULLIFY ( this%nr    )
    NULLIFY ( this%ncsize)
    NULLIFY ( this%ipobo )
    NULLIFY ( this%knolg )
    NULLIFY ( this%quant )
    NULLIFY ( this%xyz   )
    NULLIFY ( this%ind   )
    NULLIFY ( this%ele   )
    NULLIFY ( this%dope  )
    NULLIFY ( this%exch  )
    NULLIFY ( this%span  )
    NULLIFY ( this%stamp )
    NULLIFY ( this%space )
    NULLIFY ( this%dxmin )
    NULLIFY ( this%b_ms  )
    NULLIFY ( this%b_ss  )
    NULLIFY ( this%b_s   )
    NULLIFY ( this%b_v   )
    NULLIFY ( this%b_t   )
    NULLIFY ( this%m     )
    NULLIFY ( this%n     )
    NULLIFY ( this%enc   )
    NULLIFY ( this%bnd   )
    NULLIFY ( this%dry   )
    NULLIFY ( this%thd   )
    NULLIFY ( this%lwl   )
    NULLIFY ( this%ext   )
    NULLIFY ( this%isbnd )
    NULLIFY ( this%isdam )
    NULLIFY ( this%huu   )
    NULLIFY ( this%hvu   )
    NULLIFY ( this%hwu   )
    NULLIFY ( this%dwlp  )
    !
    ! Speicherbelegung fuer Komponenten, die dauerhaft vorhanden sein sollen
    IF ( no_error( ) ) CALL alloc_h_grid_nv  ( this )
    IF ( no_error( ) ) CALL alloc_h_grid_ns  ( this )
    IF ( no_error( ) ) CALL alloc_h_grid_nsi ( this )
    IF ( no_error( ) ) CALL alloc_h_grid_nsf ( this )
    IF ( no_error( ) ) CALL alloc_h_grid_ne  ( this )
    !
    ! Wert-Initialisierung fuer Komponenten, die dauerhaft vorhanden sein sollen
    IF ( no_error( ) ) this%nv  = 0
    IF ( no_error( ) ) this%ns  = 0
    IF ( no_error( ) ) this%nsi = 0
    IF ( no_error( ) ) this%nsf = 0
    IF ( no_error( ) ) this%ne  = 0
    !
  END SUBROUTINE init_h_grid_object_0
  !
  !! Drucke den Inhalt der Komponente "nv" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_nv_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_h_grid_nv_0'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED(this%nv) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN 
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'nv' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) this%nv
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'nv' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          ELSE
             WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
                CALL setup_error_act ( '<component-name>', 'nv' )
                CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
             END IF
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente nv  - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# nv= ',I9.9,' (= Anzahl Gitterknoten)')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_nv_0
  !
  !! Drucke den Inhalt der Komponente "ns" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_ns_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_h_grid_ns_0'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED(this%ns) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN 
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'ns' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) this%ns
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'ns' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          ELSE
             WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
                CALL setup_error_act ( '<component-name>', 'ns' )
                CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
             END IF
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente ns  - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# ns= ',I9.9,' (= Anzahl Gitterkanten)')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_ns_0
  !
  !! Drucke den Inhalt der Komponente "nsi" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_nsi_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_h_grid_nsi_0'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED(this%nsi) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN 
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'nsi' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) this%nsi
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'nsi' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          ELSE
             WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
                CALL setup_error_act ( '<component-name>', 'nsi' )
                CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
             END IF
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente nsi - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# nsi= ',I9.9,' (= Anzahl innere Gitterkanten)')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_nsi_0
  !
  !! Drucke den Inhalt der Komponente "nsf" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_nsf_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_h_grid_nsf_0'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED(this%nsf) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN 
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'nsf' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) this%nsf
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'nsf' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          ELSE
             WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
                CALL setup_error_act ( '<component-name>', 'nsf' )
                CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
             END IF
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente nsf - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# nsf= ',I9.9,' (= Position der letzten Kante mit Flussbedingung)')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_nsf_0
  !
  !! Drucke den Inhalt der Komponente "ne" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_ne_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_h_grid_ne_0'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED(this%ne) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN 
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'ne' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) this%ne
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'ne' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          ELSE
             WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
                CALL setup_error_act ( '<component-name>', 'ne' )
                CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
             END IF
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente ne  - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# ne= ',I9.9,' (= Anzahl Gitterpolygone)')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_ne_0
  !
  !! Drucke den Inhalt der Komponente "xy" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_xy_2 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_h_grid_xy_2'
    !! Statusvariable
    INTEGER :: stat
    !! Z&auml;hler
    INTEGER :: i
    !
    IF ( ASSOCIATED( this%xy ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'xy(:,:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%xy,DIM=1)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i,this%xy(i,1),this%xy(i,2)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'xy(:,:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'xy(:,:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente xy  - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# v= ',I9.9,', x= ',F12.3,', y= ',F12.3)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_xy_2
  !
  !! Drucke den Inhalt der Komponente "nen" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_nen_2 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_h_grid_nen_2'
    !! Statusvariable
    INTEGER :: stat
    !! Z&auml;hler
    INTEGER :: i, j
    !
    IF ( ASSOCIATED( this%nen ) .AND. ASSOCIATED( this%ks ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'nen(:,:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%nen,DIM=1)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i,(this%nen(i,j),j=1,this%ks(i))
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'nen(:,:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'nen(:,:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente nen - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# e= ',I9.9,', v1= ',I9,', v2= ',I9,', v3= ',I9,', v4= ',I9)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_nen_2
  !
  !! Drucke den Inhalt der Komponente "irand" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_irand_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=20) , PARAMETER :: c_upname='print_h_grid_irand_1'
    !! Statusvariable
    INTEGER :: stat
    !! Z&auml;hler
    INTEGER :: i
    !
    IF ( ASSOCIATED( this%irand ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'irand(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%irand)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i,this%irand(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'irand(:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'irand(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente irand - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# e= ',I9.9,', irand= ',I1)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_irand_1
  !
  !! Drucke den Inhalt der Komponente "ks" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_ks_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_h_grid_ks_1'
    !! Statusvariable
    INTEGER :: stat
    !! Z&auml;hler
    INTEGER :: i
    !
    IF ( ASSOCIATED( this%ks ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'ks(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%ks)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i,this%ks(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'ks(:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'ks(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente ks  - - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# e= ',I9.9,', ks= ',I1)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_ks_1
  !
  !! Drucke den Inhalt der Komponente "hv" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_hv_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_h_grid_hv_1'
    !! Statusvariable
    INTEGER :: stat
    !! Z&auml;hler
    INTEGER :: i
    !
    IF ( ASSOCIATED( this%hv ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'hv(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%hv)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i,this%hv(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'hv(:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'hv(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente hv  - - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# n= ',I9.9,', hv= ',F15.4)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_hv_1
  !
  !! Drucke den Inhalt der Komponente "nrand" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_nrand_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=20) , PARAMETER :: c_upname='print_h_grid_nrand_0'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%nrand ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'nrand' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) this%nrand
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'nrand' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'nrand' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente nrand - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# nrand= ',I9.9,' (= Anzahl der Knoten auf dem aeusseren Rand)')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_nrand_0
  !
  !! Drucke den Inhalt der Komponente "nptfr" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_nptfr_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=20) , PARAMETER :: c_upname='print_h_grid_nptfr_0'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%nptfr ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'nptfr' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) this%nptfr
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'nptfr' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'nptfr' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente nptfr - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# nptfr= ',I9.9,' (= Anzahl der Teilgebiets-Randknoten (TELEMAC parallel)')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_nptfr_0
  !
  !! Drucke den Inhalt der Komponente "nptir" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_nptir_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=20) , PARAMETER :: c_upname='print_h_grid_nptir_0'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%nptir ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'nptir' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) this%nptir
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'nptir' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'nptir' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente nptir - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# nptir= ',I9.9,' (= Anzahl der Interface-Knoten (TELEMAC parallel)')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_nptir_0
  !
  !! Drucke den Inhalt der Komponente "time" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_time_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='print_h_grid_time_0'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%time ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'time' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          CALL print_datetime ( this%time )
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'time' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente time - - - - - - - - - - - - - - - - - - - - ')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_time_0
  !
  !! Drucke den Inhalt der Komponente "nbc" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_nbc_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_h_grid_nbc_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%nbc ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'nbc' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) this%nbc
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'nbc' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'nbc' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente nbc - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# nbc= ',I9.9,' (= Anzahl der Polygone entlang offener Raender)')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_nbc_0
  !
  !! Drucke den Inhalt der Komponente "hland" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_hland_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=20) , PARAMETER :: c_upname='print_h_grid_hland_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%hland ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'hland' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) this%hland
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'hland' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'hland' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente hland - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# hland= ',F14.3,' (= Grenzwasserbedeckung)')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_hland_0
  !
  !! Drucke den Inhalt der Komponente "angle" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_angle_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=20) , PARAMETER :: c_upname='print_h_grid_angle_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%angle ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'angle' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) this%angle
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'angle' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'angle' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente angle - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# angle= ',G14.9,' (= mittlere geographische Breite)')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_angle_0
  !
  !! Drucke den Inhalt der Komponente "text" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_text_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='print_h_grid_text_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i    ! 
    !
    IF ( ASSOCIATED( this%text ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'text(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%text)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i,this%text(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'text(:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'text(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente text  - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# n= ',I9.9,', text= ',A)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_text_1
  !
  !! Drucke den Inhalt der Komponente "jb" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_jb_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_h_grid_jb_1'
    !! Statusvariable
    INTEGER :: stat
    !! Z&auml;hler
    INTEGER :: i
    !
    IF ( ASSOCIATED( this%jb ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'jb(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%jb)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i,this%jb(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'jb(:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'jb(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente jb  - - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# s= ',I9.9,', jb= ',I10)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_jb_1
  !
  !! Drucke den Inhalt der Komponente "jt" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_jt_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_h_grid_jt_1'
    !! Statusvariable
    INTEGER :: stat
    !! Z&auml;hler
    INTEGER :: i
    !
    IF ( ASSOCIATED( this%jt ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'jt(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%jt)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i,this%jt(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'jt(:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'jt(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente jt  - - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# s= ',I9.9,', jt= ',I10)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_jt_1
  !
  !! Drucke den Inhalt der Komponente "is" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_is_2 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_h_grid_is_2'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i, j ! 
    !
    IF ( ASSOCIATED( this%is ) .AND. ASSOCIATED( this%ks ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'is(:,:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%is,DIM=1)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i,(this%is(i,j),j=1,this%ks(i))
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'is(:,:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'is(:,:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente is - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# e= ',I9.9,', s1= ',I9,', s2= ',I9,', s3= ',I9,', s4= ',I9)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_is_2
  !
  !! Drucke den Inhalt der Komponente "je" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_je_2 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_h_grid_je_2'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i, j ! 
    !
    IF ( ASSOCIATED( this%je ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'je(:,:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%je,DIM=1)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i,(this%je(i,j),j=1,2)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'je(:,:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'je(:,:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente je - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# s= ',I9.9,', e1= ',I9,', e2= ',I9)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_je_2
  !
  !! Drucke den Inhalt der Komponente "ie" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_ie_2 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_h_grid_ie_2'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i, j ! 
    !
    IF ( ASSOCIATED( this%ie ) .AND. ASSOCIATED( this%ks ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'ie(:,:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%ie,DIM=1)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i,(this%ie(i,j),j=1,this%ks(i))
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'ie(:,:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'ie(:,:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente ie - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# e= ',I9.9,', e1= ',I9,', e2= ',I9,', e3= ',I9,', e4= ',I9)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_ie_2
  !
  !! Drucke den Inhalt der Komponente "xs" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_xs_2 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_h_grid_xs_2'
    !! Statusvariable
    INTEGER :: stat
    !! Z&auml;hler
    INTEGER :: i, j
    !
    IF ( ASSOCIATED( this%xs ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'xs(:,:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%xs,DIM=1)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i,this%xs(i,1:2)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'xs(:,:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'xs(:,:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente xs - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# s= ',I9.9,', x= ',G16.10,', y= ',G16.10)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_xs_2
  !
  !! Drucke den Inhalt der Komponente "xc" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_xc_2 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_h_grid_xc_2'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i, j ! 
    !
    IF ( ASSOCIATED( this%xc ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'xc(:,:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%xc,DIM=1)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i,this%xc(i,1:2)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'xc(:,:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'xc(:,:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente xc - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# e= ',I9.9,', x= ',G16.10,', y= ',G16.10)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_xc_2
  !
  !! Drucke den Inhalt der Komponente "xg" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_xg_2 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_h_grid_xg_2'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%xg ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'xg(:,:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%xg,DIM=1)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i,this%xg(i,1:2)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'xg(:,:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'xg(:,:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente xg (center of gravity) - - - - - - - - - - - ')
8001 FORMAT ('# e= ',I9.9,', x= ',G16.10,', y= ',G16.10)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_xg_2
  !
  !! Drucke den Inhalt der Komponente "dx" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_dx_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_h_grid_dx_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%dx ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'dx(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%dx,DIM=1)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i,this%dx(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'dx(:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'dx(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente dx - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# s= ',I9.9,', dx= ',G16.10)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_dx_1
  !
  !! Drucke den Inhalt der Komponente "dy" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_dy_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    ! lokale Parameter / Variablen
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_h_grid_dy_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%dy ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'dy(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%dy,DIM=1)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i,this%dy(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'dy(:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'dy(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente dy - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# s= ',I9.9,', dy= ',G16.10)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_dy_1
  !
  !! Drucke den Inhalt der Komponente "dg" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_dg_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    ! lokale Parameter / Variablen
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_h_grid_dg_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%dg ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'dg(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%dg,DIM=1)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i,this%dg(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'dg(:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'dg(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente dg  - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# s= ',I9.9,', dg= ',G16.10)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_dg_1
  !
  !! Drucke den Inhalt der Komponente "aa" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_aa_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_h_grid_aa_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%aa ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'aa(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%aa,DIM=1)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i,this%aa(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'aa(:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'aa(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente aa - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# e= ',I9.9,', aa= ',G16.10)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_aa_1
  !
  !! Drucke den Inhalt der Komponente "hu" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_hu_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_h_grid_hu_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%hu ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'hu(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%hu,DIM=1)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i,this%hu(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'hu(:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'hu(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente hu - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# s= ',I9.9,', hu= ',G16.10)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_hu_1
  !
  !! Drucke den Inhalt der Komponente "hw" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_hw_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_h_grid_hw_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%hw ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'hw(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%hw,DIM=1)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i,this%hw(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'hw(:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'hw(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente hw - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# e= ',I9.9,', hw= ',G16.10)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_hw_1
  !
  !! Drucke den Inhalt der Komponente "nr" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_nr_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='print_h_grid_nr_0'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%nr ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'nr' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) this%nr
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'nr' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'nr' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente nr  - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# nr= ',I9.9,' (= Anzahl der roten Gitterpolygone)')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_nr_0
  !
  !! Drucke den Inhalt der Komponente "ncsize" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_ncsize_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=21) , PARAMETER :: c_upname='print_h_grid_ncsize_0'
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( ASSOCIATED( this%ncsize ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'ncsize' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) this%ncsize
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'ncsize' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'ncsize' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente ncsize  - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# ncsize= ',I9.9,' (= Anzahl der benutzten CPUs)')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_ncsize_0
  !
  !! Drucke den Inhalt der Komponente "ipobo" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_ipobo_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=20) , PARAMETER :: c_upname='print_h_grid_ipobo_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%ipobo ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'ipobo(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%ipobo,DIM=1)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i,this%ipobo(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'ipobo(:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'ipobo(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente ipobo - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# v= ',I9.9,', ipobo= ',I5)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_ipobo_1
  !
  !! Drucke den Inhalt der Komponente "knolg" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_knolg_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=20) , PARAMETER :: c_upname='print_h_grid_knolg_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%knolg ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'knolg(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%knolg,DIM=1)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i,this%knolg(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'knolg(:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'knolg(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente knolg - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# i= ',I10,', knolg= ',I10)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_knolg_1
  !
  !! Drucke den Inhalt der Komponente "quant" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_quant_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=20) , PARAMETER :: c_upname='print_h_grid_quant_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%quant ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'quant(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          CALL print_omi_quant ( this%quant(:) )
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'quant(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente quant - - - - - - - - - - - - - - - - - - - - ')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_quant_1
  !
  !! Drucke den Inhalt der Komponente "xyz" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_xyz_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_h_grid_xyz_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%xyz ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'xyz(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          CALL print_omi_xyz ( this%xyz(:) )
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'xyz(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente xyz - - - - - - - - - - - - - - - - - - - - - ')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_xyz_1
  !
  !! Drucke den Inhalt der Komponente "ele" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_ele_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_h_grid_ele_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%ele ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'ele(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          CALL print_omi_ele ( this%ele(:) )
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'ele(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente ele - - - - - - - - - - - - - - - - - - - - - ')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_ele_1
  !
  !! Drucke den Inhalt der Komponente "ind" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_ind_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_h_grid_ind_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%ind ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'ind(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          CALL print_omi_ind ( this%ind(:) )
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'ind(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente ind - - - - - - - - - - - - - - - - - - - - - ')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_ind_1
  !
  !! Drucke den Inhalt der Komponente "dope" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_dope_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='print_h_grid_dope_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%dope ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'dope(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          CALL print_omi_dope ( this%dope(:) )
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'dope(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente dope  - - - - - - - - - - - - - - - - - - - - ')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_dope_1
  !
  !! Drucke den Inhalt der Komponente "exch" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_exch_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='print_h_grid_exch_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%exch ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'exch(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          CALL print_omi_exch ( this%exch(:) )
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'exch(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente exch  - - - - - - - - - - - - - - - - - - - - ')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_exch_1
  !
  !! Drucke den Inhalt der Komponente "span" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_span_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='print_h_grid_span_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%span ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'span' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          CALL print_omi_span ( this%span )
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'span' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente span  - - - - - - - - - - - - - - - - - - - - ')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_span_0
  !
  !! Drucke den Inhalt der Komponente "stamp" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_stamp_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=20) , PARAMETER :: c_upname='print_h_grid_stamp_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%stamp ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'stamp' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          CALL print_omi_stamp ( this%stamp )
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'stamp' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente stamp - - - - - - - - - - - - - - - - - - - - ')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_stamp_0
  !
  !! Drucke den Inhalt der Komponente "space" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_space_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=20) , PARAMETER :: c_upname='print_h_grid_space_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%space ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'space' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          CALL print_omi_space ( this%space )
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'space' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8000)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente space - - - - - - - - - - - - - - - - - - - - ')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_space_0
  !
  !! Drucke den Inhalt der Komponente "dxmin" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_dxmin_0 &
       ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=20) , PARAMETER :: c_upname='print_h_grid_dxmin_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%dxmin ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'dxmin' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) this%dxmin
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'dxmin' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'dxmin' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente dxmin - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# dxmin= ',G15.9,', minimaler zulaessiger Abstand zwischen Zentren')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_dxmin_0
  !
  !! Drucke den Inhalt der Komponente "b_ms(:)" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_b_ms_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='print_h_grid_b_ms_1'
    !! Statusvariable
    INTEGER :: stat
    !! Z&auml;hler
    INTEGER :: i
    !
    IF ( ASSOCIATED( this%b_ms ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7310, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'b_ms(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%b_ms)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i, this%b_ms(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7310, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'b_ms(:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7310, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'stamp' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente b_ms(:) - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# b= ',I9.9,', b_ms= ',I10)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_b_ms_1
  !
  !! Drucke den Inhalt der Komponente "b_ss(:)" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_b_ss_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='print_h_grid_b_ss_1'
    !! Statusvariable
    INTEGER :: stat
    !! Z&auml;hler
    INTEGER :: i
    !
    IF ( ASSOCIATED( this%b_ss ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7310, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'b_ss(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%b_ss)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i, this%b_ss(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7310, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'b_ss(:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7310, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'stamp' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente b_ss(:) - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# b= ',I9.9,', b_ss= ',I10)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_b_ss_1
  !
  !! Drucke den Inhalt der Komponente "b_s(:)" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_b_s_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_h_grid_b_s_1'
    !! Statusvariable
    INTEGER :: stat
    !! Z&auml;hler
    INTEGER :: i
    !
    IF ( ASSOCIATED( this%b_s ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7310, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'b_s(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%b_s)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i, this%b_s(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7310, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'b_s(:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7310, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'stamp' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente b_s(:)  - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# b= ',I9.9,', b_s= ',I10)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_b_s_1
  !
  !! Drucke den Inhalt der Komponente "b_v(:)" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_b_v_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_h_grid_b_v_1'
    !! Statusvariable
    INTEGER :: stat
    !! Z&auml;hler
    INTEGER :: i
    !
    IF ( ASSOCIATED( this%b_v ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7310, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'b_v(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%b_v)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i, this%b_v(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7310, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'b_v(:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7310, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'stamp' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente b_v(:)  - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# b= ',I9.9,', b_v= ',I10)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_b_v_1
  !
  !! Drucke den Inhalt der Komponente "b_t(:)" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_b_t_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_h_grid_b_t_1'
    !! Statusvariable
    INTEGER :: stat
    !! Z&auml;hler
    INTEGER :: i
    !
    IF ( ASSOCIATED( this%b_t ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7310, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'b_t(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%b_t)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i, this%b_t(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7310, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'b_t(:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7310, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'stamp' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente b_t(:)  - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# b= ',I9.9,', b_t= ',I10)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_b_t_1
  !
  !! Drucke den Inhalt der Komponente "m" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_m_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='print_h_grid_m_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%m ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'm' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) this%m
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'm' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'm' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente m - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# m= ',I10,', Anzahl der Gitterpunkte in M-Richtung')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_m_0
  !
  !! Drucke den Inhalt der Komponente "n" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_n_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='print_h_grid_n_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%n ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'n' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) this%n
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'n' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'n' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente n - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# n= ',I10,', Anzahl der Gitterpunkte in N-Richtung')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_n_0
  !
  !! Drucke den Inhalt der Komponente "enc(:,:)" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_enc_2 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_h_grid_enc_2'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%enc ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'enc(:,:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%enc,1)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i, this%enc(i,1:2)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'enc(:,:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'enc(:,:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente enc(:,:)  - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# i= ',I9.9,', enc1= ',I9,', enc2= ',I9)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_enc_2
  !
  !! Drucke den Inhalt der Komponente "bnd(:)" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_bnd_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid)    , POINTER   :: this
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_h_grid_bnd_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%bnd ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'bnd(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%bnd,1)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i, this%bnd(i)%name, &
                  this%bnd(i)%bdry_type, this%bnd(i)%data_type,                 &
                  this%bnd(i)%grid_coor(:), this%bnd(i)%refl_coef,              &
                  this%bnd(i)%prof, this%bnd(i)%bloc_ampl, this%bnd(i)%bloc_phas
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'bnd(:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'bnd(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente bnd(:)  - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# i= ',I9.9,', name= ',A,', bdry_type= ',A,', data_type= ',A,', grid_coor= ',4I5, &
          ', refl_coef= ',G10.3,', prof= ',A,', bloc_ampl= ',A,', bloc_phas= ',A)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_bnd_1
  !
  !! Drucke den Inhalt der Komponente "thd(:)" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_thd_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid)    , POINTER   :: this
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_h_grid_thd_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%thd ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'thd(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%thd,1)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i, this%thd(i)%type, &
                  this%thd(i)%grid_coor(:)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'thd(:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'thd(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente thd(:)  - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# i= ',I9.9,', type= ',A,', grid_coor= ',4I5)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_thd_1
  !
  !! Drucke den Inhalt der Komponente "lwl(:)" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_lwl_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid)    , POINTER   :: this
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_h_grid_lwl_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%lwl ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'lwl(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%lwl,1)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i, this%lwl(i)%type, &
                  this%lwl(i)%grid_coor(:), this%lwl(i)%frction, this%lwl(i)%sill_depth
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'lwl(:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'lwl(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente lwl(:)  - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# i= ',I9.9,', type= ',A,', grid_coor= ',4I5,', friction = ',F10.5,', depth = ',F10.3)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_lwl_1
  !
  !! Drucke den Inhalt der Komponente "ext(:)" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_ext_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid)    , POINTER   :: this
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_h_grid_ext_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%ext ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'ext(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%ext,1)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i, this%ext(i)%type, &
                  this%ext(i)%grid_coor(:), this%ext(i)%frction, this%ext(i)%sill_depth
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'ext(:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'ext(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente ext(:)  - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# i= ',I9.9,', type= ',A,', grid_coor= ',4I5,', friction = ',F10.5,', depth = ',F10.3)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_ext_1
  !
  !! Drucke den Inhalt der Komponente "dry(:,:)" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_dry_2 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_h_grid_dry_2'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%dry ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'dry(:,:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%dry,1)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i, this%dry(i,1:c_max_d3d_dry)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'dry(:,:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'dry(:,:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente dry(:,:)  - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# i= ',I9.9,', dry11= ',I9,', dry12= ',I9,', dry21= ',I9,', dry22= ',I9)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_dry_2
  !
  !! Drucke den Inhalt der Komponente "isbnd(:,:)" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_isbnd_2 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=20) , PARAMETER :: c_upname='print_h_grid_isbnd_2'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%isbnd ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'isbnd(:,:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%isbnd,1)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i, this%isbnd(i,1:4)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'isbnd(:,:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'isbnd(:,:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente isbnd(:,:)  - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# i= ',I9.9,', isbnd1= ',I9,', isbnd2= ',I9,', isbnd3= ',I9,', isbnd4= ',I9)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_isbnd_2
  !
  !! Drucke den Inhalt der Komponente "isdam(:,:,:)" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_isdam_3 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=20) , PARAMETER :: c_upname='print_h_grid_isdam_3'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%isdam ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'isdam(:,:,:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%isdam,1)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i, this%isdam(i,1:4,1), &
                  this%isdam(i,1:4,2)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'isdam(:,:,:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'isdam(:,:,:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente isdam(:,:)  - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# i= ', I9.9,', isdam11= ',I9,', isdam21= ',I9,', isdam31= ',I9,', isdam41= ',I9,/, &
             '#               isdam12= ',I9,', isdam22= ',I9,', isdam32= ',I9,', isdam42= ',I9 )
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_isdam_3
  !
  !! Drucke den Inhalt der Komponente "huu" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_huu_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_h_grid_huu_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%huu ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'huu(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%huu,DIM=1)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i,this%huu(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'huu(:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'huu(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente huu - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# s= ',I9.9,', huu= ',G16.10)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_huu_1
  !
  !! Drucke den Inhalt der Komponente "hvu" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_hvu_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_h_grid_hvu_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%hvu ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'hvu(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%hvu,DIM=1)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i,this%hvu(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'hvu(:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'hvu(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente hvu - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# n= ',I9.9,', hvu= ',G16.10)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_hvu_1
  !
  !! Drucke den Inhalt der Komponente "hwu" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_hwu_1 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=18) , PARAMETER :: c_upname='print_h_grid_hwu_1'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%hwu ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'hwu(:)' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          DO i=1,SIZE(this%hwu,DIM=1)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i,this%hwu(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'hwu(:)' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          END IF
       END IF
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'hwu(:)' )
             CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente hwu - - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# n= ',I9.9,', hwu= ',G16.10)
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_hwu_1
  !
  !! Drucke den Inhalt der Komponente "dwlp" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_dwlp_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='print_h_grid_dwlp_0'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED(this%dwlp) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN 
          CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
          CALL setup_error_act ( '<component-name>', 'dwlp' )
          CALL setup_error_act ( '<action>', 'Header-Line (8000)' )
       ELSE
          WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) this%dwlp
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
             CALL setup_error_act ( '<component-name>', 'dwlp' )
             CALL setup_error_act ( '<action>', 'Data-Line (8001)' )
          ELSE
             WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 7300, c_upname, c_modname, stat )
                CALL setup_error_act ( '<component-name>', 'dwlp' )
                CALL setup_error_act ( '<action>', 'Footer-Line (8002)' )
             END IF
          END IF
       END IF
    END IF
    !
8000 FORMAT ('# Inhalt der Komponente dwlp  - - - - - - - - - - - - - - - - - - - - ')
8001 FORMAT ('# dwlp= ',A,' (= Depth_at_Water_Level_Points fuer Delft3D)')
8002 FORMAT ('# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_h_grid_dwlp_0
  !
  !! Umwandeln eines Textes in Gro&szlig;buchstaben <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_uppercase_char_0 ( in ) &
       RESULT( out )
    !! Zeichenfolge (Original)
    CHARACTER (LEN=*) , INTENT(IN) :: in  ! 
    !! Zeichenfolge (konvertiert in Gro&szlig;buchstaben)
    CHARACTER (LEN=LEN(in))        :: out ! 
    !! Z&auml;hler 
    INTEGER :: i, ic ! 
    !
    out = in
    DO i=1,LEN(out)
       ic = IACHAR(in(i:i))
       IF ( ic >= 97 .AND. ic <= 122 ) out(i:i) = ACHAR(ic-32)
    END DO
    !
  END FUNCTION get_uppercase_char_0
  !
  !! f&uuml;r ein neues (Package-) Objekt wird automatisch eine neue Identifikationsnummer erzeugt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_new_id_0 ( ) &
       RESULT( id )
    !! Identifikationsnummer des neuen (Package-) Objektes
    INTEGER :: id
    !! Hilfsvariable (Package-) Objekt
    TYPE (t_h_grid_list) , POINTER :: this
    !
    IF ( .NOT. ASSOCIATED( first_list_object ) ) THEN ! das allererste Mal
       id = 1
    ELSE ! falls schon Objekte vorhanden sind
       id   =  0
       this => first_list_object
       DO
          id = MAX (id, this%object%id) + 1
          IF ( .NOT. ASSOCIATED( this%next ) ) EXIT
          this => this%next
       END DO
       NULLIFY ( this )
    END IF
  END FUNCTION get_h_grid_new_id_0
  !
  !! Setzen der Fehlerbedingung 3 = keine Objekte "t_h_grid" <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION any_objects_0 ( upname ) &
       RESULT( ok )
    !! Name der Subroutine die "any_objects" ruft
    CHARACTER (LEN=*) , INTENT(IN) :: upname
    !! Testergebnis
    LOGICAL :: ok
    !
    ok = ( nofobjects > 0 )
    !
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 3, upname, c_modname )
    !
  END FUNCTION any_objects_0
  !
END MODULE m_h_grid_data
! TailOfPackageModule -----------------------------------------------------
