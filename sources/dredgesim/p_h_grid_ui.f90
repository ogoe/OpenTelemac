! -------------------------------------------------------------------------
! HeadOfPackageUserInterface ----------------------------------------------
!
!! <H2>Horizontal Grid</h2>
!! @author <A HREF="mailto:lang@hamburg.baw.de">G. Lang</A>, <A HREF="mailto:juerges@hamburg.baw.de">J. J&uuml;rges</A> und <A HREF="mailto:schade@hamburg.baw.de">P. Schade</A>
!! @version 4.9 vom 03/05/07, Quellcode: mod_p_h_grid_ui.f90
!! <HR>
!! Package contains routines dealing with horizontal grids           <BR>
!! <HR>
!  Copyright-Hinweis
!
!  Copyright (C) 2001 Bundesanstalt fuer Wasserbau
!
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Paketes
!  01.01 : 2002-07-09 : G. Lang, J. Juerges : Startversion; 
!  01.02 : 2002/07/10 : G. Lang    : + weitere Korrekturen+Ergaenzungen
!  01.03 : 2002/07/15 : G. Lang    : + Arbeitsversion 15. Juli 2002
!  01.04 : 2002/07/17 : G. Lang    : + Arbeitsversion 2002-Juli-17
!  01.05 : 2002/07/18 : G. Lang    : + Arbeitsversion vom 2002-Juli-18
!  01.06 : 2002/07/19 : G. Lang    : + Arbeitsversion 2002-Juli-19
!  01.07 : 2002/07/22 : G. Lang    : + Arbeitsversion 2002-Juli-21
!  01.08 : 2002/07/22 : G. Lang    : + ergaenzende Fehlermeldungen
!  01.09 : 2002/07/23 : G. Lang    : + Arbeitsversion 2002-Juli-23
!  01.10 : 2002/07/24 : G. Lang    : + Arbeitsversion 2002-Juli-24
!  01.11 : 2002/07/25 : P. Schade  : + Fehlermeldung 25004
!  01.12 : 2002/07/26 : P. Schade  : + convert_h_grid_to_untrim
!  01.13 : 2002/08/29 : P. Schade  : + convert_h_grid_to_gitter05
!  01.14 : 2002/09/05 : G. Lang    : + Felder in Pointer in Functions ohne (:) oder (:,:) angegeben
!  01.15 : 2002/09/06 : G. Lang    : + Hinweise auf autom. Berechnung, get_h_grid_act_variant_type
!  01.16 : 2002/09/10 : G. Lang    : + OK-Methoden erweitert (Orthogonalitaet,Orientierung,...)
!  01.17 : 2002/10/02 : J. Juerges : Komponente xs (Mitten-Koordinaten der Kanten) hinzugefuegt
!  01.18 : 2003-03-20 : P. Schade  : Aenderungen Header, z.B. Copyright;  Fehlermeldungen beschreiben
!  01.19 : 2003/03/26 : P. Schade  : + FM 16113
!  01.20 : 2003/04/03 : P. Schade  : Verschieben der Methode test_h_grid nach main_test_h_grid; + Komponente ncsize
!  01.21 : 2003-04-09 : P. Schade  : + Komponente knolg
!  01.22 : 2003-04-16 : P. Schade  : read_h_grid_selafin_0: + n_zero
!  01.23 : 2003-06-04 : P. Schade  : Komponenten nptfr und nptir: + Methoden und Aenderung read_h_grid_selafin_0
!  01.24 : 2003-06-04 : P. Schade  : Namensverkuerzungen von *section_bound* zu section und von *interface* zu *intface*
!  01.25 : 2003-12-02 : J. Juerges : + Komponente nsi
!  01.26 : 2004-02-26 : G. Lang    : neue Methoden
!  01.27 : 2004/04/26 : G. Lang    : + unnoetige Gitter-Checks beim Schreiben/Lesen entfernt
!  01.28 : 2004-05-27 : J. Juerges : + Komponente xg (Polygon-Schwerpunktkoordinaten)
!  01.29 : 2004-06-24 : G. Lang    : + Komponente nsf
!  01.30 : 2004-11-11 : G. Lang    : + Interface "convert_h_grid_terrace"
!  01.31 : 2005-01-11 : S. Spohr   : + CALL DERIVE-Fkt. in "get_h_grid_nof_edges" und "get_h_grid_nof_bound_nodes"
!  02.01 : 2005-03-07 : G. Lang    : + Erweiterungen OpenMI (unvollstaendig)
!  02.02 : 2005-03-10 : G. Lang    : + div. Erweiterungen fuer Konfigurationsphase OpenMI
!  02.03 : 2005-03-16 : G. Lang    : + Erweiterungen fuer ALLBoundaries,AllOpenBoundaries und AllClosedBoundaries 
!  03.01 : 2005-07-21 : G. Lang    : Anpassungen/Erweiterungen fuer mod. OpenMI-ElementSet-Verstaendnis
!  04.01 : 2005-08-22 : G. Lang    : Lesen/Schreiben Delft3D-Gitternetz
!  04.02 : 2005-11-23 : G. Lang    : Erweiterungen *.thd, *.lwl, *.ext
!  04.03 : 2005-12-28 : G. Lang    : Erweiterungen fuer "refine_h_grid"
!  04.04 : 2006-04-13 : G. Lang    : optionale unerodierbare Tiefen huu(:), hvu(:) und hwu(:)
!  04.05 : 2006-07-26 : G. Lang    : Vertiefen unter optionaler Beruecksichtigung unerodierbarer Tiefen
!  04.06 : 2006-08-31 : G. Lang    : Schnittstelle fuer neue Komponente "dwlp" = "Depth_At_Water_Level_Points" (fuer Delft3D-Konversion)
!  04.07 : 2007-01-12 : G. Lang    : + Schnittstelle get_max_h_grid_olddim( )
!  04.08 : 2007-02-05 : G. Lang    : neue Schnittstellen *_grav_center_*, aux_h_grid_get
!  04.08 : 2007-03-05 : Schade     : IF (no_error() vor close_file
!
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Packages</H3>
!! <UL>
!!    <LI> Speicherung der horizontalen Struktur eines oder mehrerer
!!         unstrukturierter Gitternetze
!!    <LI> Bereitstellen von Methoden zum Lesen und Schreiben eines
!!         Gitters in verschiedenen Dateiformaten, sowie zum Berechnen
!!         von gitternetzspezifischen Groessen (z.B. Kantenverzeichnis
!!         der Polygone/Elemente). Folgende Dateitypen werden derzeit
!!         unterst&uuml;tzt:
!!         <OL>
!!         <LI> <A HREF="http://www.hamburg.baw.de/fkb/gitter05/gi05-de.htm"><EM>gitter05.dat/bin</EM></A>,
!!         <LI> <A HREF="http://www.hamburg.baw.de/fkb/untrim/utrg-de.htm"><EM>untrim_grid.dat</EM></A>, und
!!         <LI> <A HREF="http://www.hamburg.baw.de/fkb/selafin/selaf-de.htm"><EM>selafin</EM></A>, und
!!         <LI> Delft3D-Gitternetz (noch keine Beschreibung vorhanden).
!!         </OL>
!! </UL>
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt keinen &ouml;ffentlich zug&auml;nglichen     <BR>
!! Datentyp zur Verf&uuml;gung.                                     <BR>
!!
!! <HR>
!! <H3>Verwenden des Moduls</H3>
!!                                                                  <BR>
!! Die Leistungen des Moduls k&ouml;nnen z.B. wie folgt in Anspruch genommen werden: <BR>
!! <OL>
!!    <LI> Einbinden des Moduls mittels USE-Anweisung in der rufenden Programmeinheit;
!!    <LI> Initialisieren des Paketes "h_grid" mit <TT>INIT_h_grid</TT>;
!!    <LI> Anwenden verschiedener SETUP-Methoden:
!!    <UL>
!!       <LI> <TT>SETUP_h_grid_prn_lun</TT> und
!!       <LI> <TT>SETUP_h_grid_trc_lun</TT>.
!!    </UL>
!!    <LI> Objekt bereitstellen
!!    <UL>
!!       <LI> <TT>NEW_h_grid</TT> und
!!       <LI> <TT>SETUP_h_grid_work_object</TT>
!!    </UL>
!!    <LI> Lesen eines Gitters
!!    <UL>
!!       <LI> <TT>SETUP_h_grid_file</TT>,
!!       <LI> <TT>SETUP_h_grid_name</TT> und
!!       <LI> <TT>READ_h_grid</TT>.
!!    </UL>
!!    <LI> Erzeugen OpenMI-konformer Daten
!!    <UL>
!!       <LI> <TT>GET_h_grid_exch_r</TT>
!!       <LI> <TT>GET_h_grid_time_horizon</TT>
!!       <LI> <TT>GET_h_grid_has_discr_time</TT>
!!       <LI> <TT>GET_h_grid_nof_discr_time</TT>
!!       <LI> <TT>GET_h_grid_discr_time</TT>
!!    </UL>
!!    <LI> De-Initialisieren des Paketes mit <TT>CLEAR_h_grid</TT>.
!!    <LI> Transfer von Paketdaten in alte Datenstrukturen. 
!!
!!    Zun&auml;chst muss ein neues Objekt angelegt werden
!!    <UL>
!!       <LI> <TT>NEW_h_grid</TT> und
!!       <LI> <TT>SETUP_h_grid_work_object</TT>
!!    </UL>
!!    Danach muss das Gitter in die paketinternen Datenstrukturen eingelesen werden:
!!    <UL>
!!       <LI> <TT>SETUP_h_grid_file</TT>,
!!       <LI> <TT>SETUP_h_grid_name</TT> und
!!       <LI> <TT>READ_h_grid</TT>.
!!    </UL>
!!    Schlie&szlig;lich k&ouml;nnen die Daten in die klassischen Daten-Strukturen 
!!    transferiert werden:
!!    <UL>
!!       <LI> <TT>AUX_h_grid_get</TT>  : Transfer von Paketdaten in klassische Felder
!!    </UL>
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
!!          die Methode PRINT_H_GRID_ALL_ERRORS.
!! 
module p_h_grid_ui
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1.1] BASIS-Modul mit globalen Konstantwerten
  !
  use b_constants, only : &
       ! Parameter
       double
  !
  ! [A.1.2] BASIS-Modul mit Fehler-Typ und -Routinen
  use b_error, only :       &
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
  ! [A.1.3] BASIS-Modul mit Datei-Typ und -Routinen
  use b_file, only :       &
       ! Typdefinitionen
       t_file,             &
       ! Routinen
       init_file,          &
       clear_file,         &
       new_file,           &
       ok_file,            &
       print_file,         &
       setup_file_prn_lun, &
       setup_file_trc_lun, &
       open_file,          &
       close_file,         &
       set_file_name,      &
       set_file_status,    &
       set_file_access,    &
       set_file_form,      &
       set_file_action,    &
       set_file_type
  !
  ! [A.1.4] BASIS-Modul fuer Zeitrechnung
  use b_datetime, only :       &
       ! Typdefinitionen
       t_datetime,             &
       ! Routinen
       init_datetime,          &
       clear_datetime,         &
       setup_datetime_prn_lun, &
       setup_datetime_trc_lun
  !
  ! [A.1.5] BASIS-Modul fuer 2D-Punkte
  use b_point_2d, only :       &
       ! Routinen
       init_point_2d,          &
       clear_point_2d,         &
       setup_point_2d_prn_lun, &
       setup_point_2d_trc_lun
  !
  ! [A.1.6] OpenMI-konformes Basis-Modul "b_omi_exch_r" [ "ragged array" Austauschgroesse ]
  !         Anmerkung: die folgenden Basis-Module werden von "exch" 
  !                    direkt bzw. indirekt mit-initialisiert
  !                    1.) mod_b_omi_exch.f90
  !                    2.) mod_b_omi_quant.f90
  !                        a) mod_b_dim.f90
  !                        b) mod_b_var.f90
  !                        c) mod_b_att.f90
  !                        d) mod_b_phy.f90
  !                        e) mod_b_omi_dim.f90
  !                        f) mod_b_omi_unit.f90
  !                    3.) mod_b_omi_ele.f90
  !                        a) mod_b_ind.f90
  !                        b) mod_b_xyz.f90
  !                           i) mod_b_omi_space.f90
  !                    4.) mod_b_omi_dope.f90
  !                        a) mod_b_omi_arg.f90
  use b_omi_exch_r, only :        &
       ! Typdefinition
       t_omi_exch_r,              &
       ! Routinen
       init_omi_exch_r,           &
       clear_omi_exch_r,          &
       setup_omi_exch_r_prn_lun,  &
       setup_omi_exch_r_trc_lun,  &
       setup_omi_exch_r_language, &
       new_omi_exch_r,            &
       set_omi_exch_r_odx,        &
       set_omi_exch_r_exch_ref,   &
       ok_omi_exch_r
  !
  ! [A.1.7] OpenMI-konformes Basis-Modul "span" [ Zeitspanne ]
  !         Anmerkung: die folgenden Basis-Module werden von "span" 
  !                    direkt bzw. indirekt mit-initialisiert
  !                    1.) mod_b_datetime.f90
  !                    2.) mod_b_omi_stamp.f90
  use b_omi_span, only :       &
       ! Typdefinition
       t_omi_span,             &
       ! Routinen
       init_omi_span,          &
       clear_omi_span,         &
       setup_omi_span_prn_lun, &
       setup_omi_span_trc_lun, &
       new_omi_span,           &
       set_omi_span_start,     &
       set_omi_span_end
  !
  ! [A.1.8] OpenMI-konformes Basis-Modul "exch" [ Austauschgroessen ]
  use b_omi_exch, only : &
       ! Typdefinition
       t_omi_exch
  !
  ! [A.1.9] OpenMI-konformes Basis-Modul "stamp" [ Zeitangabe ]
  use b_omi_stamp, only :      &
       ! Typdefinition
       t_omi_stamp,            &
       ! Routinen
       new_omi_stamp,          &
       get_omi_stamp_modjulianday
  !
  ! [A.2.1] Module die zu diesem Package "h_grid" gehoeren
  use m_h_grid_data ! nutze im wesentlichen alle Groessen
  !
  ! [A.2.2] Initialisieren und De-Initialisieren der Fehlermeldungen
  use m_h_grid_errors, only : &
       ! Routinen / Interfaces
       init_all_errors, &
       clear_all_errors
  !
  ! [A.2.3] Lesen und Schreiben von "gitter05.dat/bin"
  use m_h_grid_gitter05, only : &
       ! Routinen / Interfaces
       read_h_grid_gitter05_asc,  &
       read_h_grid_gitter05_bin,  &
       write_h_grid_gitter05_asc, &
       write_h_grid_gitter05_bin
  !
  ! [A.2.4] Lesen und Schreiben von "untrim_baw" und "untrim_vc"
  use m_h_grid_untrim, only : &
       ! Routinen / Interfaces
       read_h_grid_untrim_baw_asc,  &
       read_h_grid_untrim_vc_asc,   &
       write_h_grid_untrim_baw_asc, &
       write_h_grid_untrim_vc_asc
  !
  ! [A.2.5] Lesen und Schreiben von "selafin"
  use m_h_grid_selafin, only : &
       ! Routinen / Interfaces
       read_h_grid_selafin_seq_bin,  &
       write_h_grid_selafin_seq_bin
  !
  ! [A.2.6] Lesen und Schreiben von "delft3d"
  use m_h_grid_delft3d, only : &
       ! Routinen / Interfaces
       read_h_grid_delft3d_seq_asc,  &
       write_h_grid_delft3d_seq_asc
  !
  ! [A.2.7] Berechnen unbekannter aus bekannten Groessen
  use m_h_grid_derive, only : &
       ! Routinen / Interfaces
       derive_aa, derive_xs, derive_xc, derive_xg, derive_is, derive_je, derive_jb, &
       derive_jt, derive_hv, derive_hu, derive_hw, derive_ie, derive_dx, derive_dy, &
       derive_ns, derive_nrand, derive_nen, derive_ks, derive_irand, derive_huu,    &
       derive_hvu, derive_hwu, derive_dg
  !
  ! [A.2.8] Konversion von Gitternetzen zwischen verschiedenen Formaten
  use m_h_grid_convert, only :   &
       ! Routinen / Interfaces
       convert_to_gitter05,      &
       convert_to_untrim,        &
       convert_to_selafin,       &
       convert_untrim_red_black, &
       convert_untrim_nsi,       &
       convert_untrim_terrace,   &
       refine, deepen
  !
  ! [A.2.9] Erzeugen der OpenMI-konformen Datenkomponenten
  use m_h_grid_omi, only :       &
       create_omi
  !
  ! [A.2.10] Methoden zum Ermitteln von (klassischen) Dimensionen
  use m_h_grid_dimensions, only : &
       ! Routinen / Interfaces
       get_max_old_dimensions
  !
  ! ---------------------------------------------------------------------
  ! [B] alles muss explizit deklariert werden und ist default privat
  ! ---------------------------------------------------------------------
  !
  implicit none
  private
  !
  ! ---------------------------------------------------------------------
  ! [C] oeffentlich zugaengliche Deklarationen (mit PUBLIC-Attribut)
  ! ---------------------------------------------------------------------
  !
  ! [C.1] Es wird kein Datentyp oeffentlich zugaenglich gemacht
  ! [C.2] Konstantwerte (Parameter) [moeglichst nicht verwenden]
  ! [C.3] Variablen [moeglichst nicht verwenden]
  ! [C.4] Schnittstellen
  ! [C.4.1] erforderliche oeffentliche Schnittstellen
  !
  !! Allokieren/Initialisieren der statischen Datenobjekte des Packages
  interface init_h_grid
     module procedure init_h_grid_d
  end interface
  !! De-Allokieren/De-Initialisieren der statischen Datenobjekte des Packages
  interface clear_h_grid
     module procedure clear_h_grid_d
  end interface
  !! logische Kanalnummer f&uuml;r PRINT-Methoden auf Benutzerwert setzen
  interface setup_h_grid_prn_lun
     module procedure setup_h_grid_prn_lun_d
  end interface
  !! logische Kanalnummer f&uuml;r TRACE-Methoden auf Benutzerwert setzen
  interface setup_h_grid_trc_lun
     module procedure setup_h_grid_trc_lun_d
  end interface
  !! logische Kanalnummer f&uuml;r Input/Output (Sequential, Ascii)
  interface setup_h_grid_asc_seq_lun
     module procedure setup_h_grid_asc_seq_lun_d ! 
  end interface
  !! logische Kanalnummer f&uuml;r Input/Output (Sequential, Binary)
  interface setup_h_grid_bin_seq_lun
     module procedure setup_h_grid_bin_seq_lun_d ! 
  end interface
  !! logische Kanalnummer f&uuml;r Input/Output (Direct, Binary)
  interface setup_h_grid_bin_dir_lun
     module procedure setup_h_grid_bin_dir_lun_d ! 
  end interface
  !! Setzen des (Arbeits-) Objektes <BR>
  !! a) Arbeits-Objekt &uuml;ber die Identifikationsnummer des Objektes setzen <BR>
  !! b) Arbeits-Objekt &uuml;ber den Name des Objektes setzen
  interface setup_h_grid_work_object
     module procedure setup_h_grid_work_object_d
     module procedure setup_h_grid_work_object_n
  end interface
  !! Setzen der Komponente "name" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Skalar f&uuml;llen
  interface setup_h_grid_name
     module procedure setup_h_grid_name_w0 ! Arbeits-Objekt / Skalardaten
  end interface
  !! Setzen der Komponente "file" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Skalar f&uuml;llen
  interface setup_h_grid_file
     module procedure setup_h_grid_file_w0 ! Arbeits-Objekt / Skalardaten
  end interface
  !! Setzen der Feld-Komponente "xy" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Vektor f&uuml;llen 
  interface setup_h_grid_node_coord
     module procedure setup_h_grid_xy_w2 ! Arbeits-Objekt / Vektordaten
  end interface
  !! Setzen der Feld-Komponente "nen" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Vektor f&uuml;llen 
  interface setup_h_grid_nodelist_of_poly
     module procedure setup_h_grid_nen_w2 ! Arbeits-Objekt / Vektordaten
  end interface
  !! Setzen der Feld-Komponente "irand" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Vektor f&uuml;llen 
  interface setup_h_grid_poly_bound_code
     module procedure setup_h_grid_irand_w1 ! Arbeits-Objekt / Vektordaten
  end interface
  !! Setzen der Feld-Komponente "ks" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Vektor f&uuml;llen 
  interface setup_h_grid_nodes_of_poly
     module procedure setup_h_grid_ks_w1 ! Arbeits-Objekt / Vektordaten
  end interface
  !! Setzen der Feld-Komponente "hv" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Vektor f&uuml;llen 
  interface setup_h_grid_node_depth
     module procedure setup_h_grid_hv_w1 ! Arbeits-Objekt / Vektordaten
  end interface
  !! Setzen der Feld-Komponente "nrand" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Skalar f&uuml;llen
  interface setup_h_grid_nof_bound_nodes
     module procedure setup_h_grid_nrand_w0 ! Arbeits-Objekt / Skalardaten
  end interface
  !! Setzen der Feld-Komponente "nptfr" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Skalar f&uuml;llen
  interface setup_h_grid_nof_section_nodes
     module procedure setup_h_grid_nptfr_w0 ! Arbeits-Objekt / Skalardaten
  end interface
  !! Setzen der Feld-Komponente "nptir" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Skalar f&uuml;llen
  interface setup_h_grid_nof_intface_nodes
     module procedure setup_h_grid_nptir_w0 ! Arbeits-Objekt / Skalardaten
  end interface
  !! Setzen der Feld-Komponente "time" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Skalar f&uuml;llen 
  interface setup_h_grid_datetime
     module procedure setup_h_grid_time_w0 ! Arbeits-Objekt / Skalardaten
  end interface
  !! Setzen der Feld-Komponente "nbc" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Skalar f&uuml;llen 
  interface setup_h_grid_nof_bound_poly
     module procedure setup_h_grid_nbc_w0 ! Arbeits-Objekt / Skalardaten
  end interface
  !! Setzen der Feld-Komponente "hland" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Skalar f&uuml;llen 
  interface setup_h_grid_min_water_depth
     module procedure setup_h_grid_hland_w0 ! Arbeits-Objekt / Skalardaten
  end interface
  !! Setzen der Feld-Komponente "angle" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Skalar f&uuml;llen 
  interface setup_h_grid_mean_latitude
     module procedure setup_h_grid_angle_w0 ! Arbeits-Objekt / Skalardaten
  end interface
  !! Setzen der Feld-Komponente "text" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Vektor f&uuml;llen 
  interface setup_h_grid_text
     module procedure setup_h_grid_text_w1 ! Arbeits-Objekt / Vektordaten
  end interface
  !! Setzen der Feld-Komponente "jb" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Vektor f&uuml;llen 
  interface setup_h_grid_fst_node_of_edge
     module procedure setup_h_grid_jb_w1 ! Arbeits-Objekt / Vektordaten
  end interface
  !! Setzen der Feld-Komponente "jt" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Vektor f&uuml;llen 
  interface setup_h_grid_lst_node_of_edge
     module procedure setup_h_grid_jt_w1 ! Arbeits-Objekt / Vektordaten
  end interface
  !! Setzen der Feld-Komponente "is" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Vektor f&uuml;llen 
  interface setup_h_grid_edgelist_of_poly
     module procedure setup_h_grid_is_w2 ! Arbeits-Objekt / Vektordaten
  end interface
  !! Setzen der Feld-Komponente "je" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Vektor f&uuml;llen 
  interface setup_h_grid_polylist_of_edges
     module procedure setup_h_grid_je_w2 ! Arbeits-Objekt / Vektordaten
  end interface
  !! Setzen der Feld-Komponente "ie" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Vektor f&uuml;llen 
  interface setup_h_grid_polylist_of_poly
     module procedure setup_h_grid_ie_w2 ! Arbeits-Objekt / Vektordaten
  end interface
  !! Setzen der Feld-Komponente "xs" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Vektor f&uuml;llen 
  interface setup_h_grid_edge_center_coord
     module procedure setup_h_grid_xs_w2 ! Arbeits-Objekt / Vektordaten
  end interface
  !! Setzen der Feld-Komponente "xc" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Vektor f&uuml;llen 
  interface setup_h_grid_center_coord
     module procedure setup_h_grid_xc_w2 ! Arbeits-Objekt / Vektordaten
  end interface
  !! Setzen der Feld-Komponente "xg" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Vektor f&uuml;llen 
  interface setup_h_grid_grav_center_coord
     module procedure setup_h_grid_xg_w2 ! Arbeits-Objekt / Vektordaten
  end interface
  !! Setzen der Feld-Komponente "dg" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Vektor f&uuml;llen 
  interface setup_h_grid_grav_center_dist
     module procedure setup_h_grid_dg_w1 ! Arbeits-Objekt / Vektordaten
  end interface
  !! Setzen der Feld-Komponente "dx" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Vektor f&uuml;llen 
  interface setup_h_grid_center_dist
     module procedure setup_h_grid_dx_w1 ! Arbeits-Objekt / Vektordaten
  end interface
  !! Setzen der Feld-Komponente "dy" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Vektor f&uuml;llen 
  interface setup_h_grid_edge_length
     module procedure setup_h_grid_dy_w1 ! Arbeits-Objekt / Vektordaten
  end interface
  !! Setzen der Feld-Komponente "aa" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Vektor f&uuml;llen 
  interface setup_h_grid_poly_area
     module procedure setup_h_grid_aa_w1 ! Arbeits-Objekt / Vektordaten
  end interface
  !! Setzen der Feld-Komponente "hu" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Vektor f&uuml;llen 
  interface setup_h_grid_edge_depth
     module procedure setup_h_grid_hu_w1 ! Arbeits-Objekt / Vektordaten
  end interface
  !! Setzen der Feld-Komponente "hw" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Vektor f&uuml;llen 
  interface setup_h_grid_poly_depth
     module procedure setup_h_grid_hw_w1 ! Arbeits-Objekt / Vektordaten
  end interface
  !! Setzen der Skalar-Komponente "nr" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Skalar f&uuml;llen 
  interface setup_h_grid_nof_red_poly
     module procedure setup_h_grid_nr_w0 ! Arbeits-Objekt / Skalardaten
  end interface
  !! Setzen der Skalar-Komponente "ncsize" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Skalar f&uuml;llen 
  interface setup_h_grid_nof_cpus
     module procedure setup_h_grid_ncsize_w0 ! Arbeits-Objekt / Skalardaten
  end interface
  !! Setzen der Skalar-Komponente "ipobo" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Vektor f&uuml;llen 
  interface setup_h_grid_node_bound_code
     module procedure setup_h_grid_ipobo_w1 ! Arbeits-Objekt / Vektordaten
  end interface
  !! Setzen der Skalar-Komponente "dxmin" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Skalar f&uuml;llen 
  interface setup_h_grid_min_center_dist
     module procedure setup_h_grid_dxmin_w0 ! Arbeits-Objekt / Skalardaten
  end interface
  !! Setzen der Skalar-Komponente "m" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Skalar f&uuml;llen
  interface setup_h_grid_nof_m_points
     module procedure setup_h_grid_m_w0 ! Arbeits-Objekt / Skalardaten
  end interface
  !! Setzen der Skalar-Komponente "n" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Skalar f&uuml;llen
  interface setup_h_grid_nof_n_points
     module procedure setup_h_grid_n_w0 ! Arbeits-Objekt / Skalardaten
  end interface
  !! Setzen der Feld-Komponente "enc(:,:)" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Feld f&uuml;llen
  interface setup_h_grid_enclosure
     module procedure setup_h_grid_enc_w0
  end interface
  !! Setzen der Feld-Komponente "dry(:,:)" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Feld f&uuml;llen
  interface setup_h_grid_drypoints
     module procedure setup_h_grid_dry_w0
  end interface
  !! Setzen aller Subkomponenten der Komponente "bnd(:)" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Feld f&uuml;llen
  interface setup_h_grid_openbc
     module procedure setup_h_grid_bnd_w0
  end interface
  !! Setzen aller Subkomponenten der Komponente "thd(:)" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Feld f&uuml;llen
  interface setup_h_grid_thin_dams
     module procedure setup_h_grid_thd_w0
  end interface
  !! Setzen aller Subkomponenten der Komponente "lwl(:)" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Feld f&uuml;llen
  interface setup_h_grid_local_weirs
     module procedure setup_h_grid_lwl_w0
  end interface
  !! Setzen aller Subkomponenten der Komponente "ext(:)" des Objektes "t_h_grid" <BR>
  !! a) Arbeits-Objekt mit Feld f&uuml;llen
  interface setup_h_grid_2d_weirs
     module procedure setup_h_grid_ext_w0
  end interface
  !! Setzen der unerodierbaren Tiefen "huu(:)" an den Kanten des Objektes "t_h_grid" <BR>
  !! a) Arbeitsobjekt mit Feld f&uuml;llen
  interface setup_h_grid_edge_noero_depth
     module procedure setup_h_grid_huu_w0
  end interface
  !! Setzen der unerodierbaren Tiefen "hvu(:)" an den Knoten des Objektes "t_h_grid" <BR>
  !! a) Arbeitsobjekt mit Feld f&uuml;llen
  interface setup_h_grid_node_noero_depth
     module procedure setup_h_grid_hvu_w0
  end interface
  !! Setzen der unerodierbaren Tiefen "hwu(:)" an den Polygonen des Objektes "t_h_grid" <BR>
  !! a) Arbeitsobjekt mit Feld f&uuml;llen
  interface setup_h_grid_poly_noero_depth
     module procedure setup_h_grid_hwu_w0
  end interface
  !! Setzen der Steuervariable "dwlp" (<EM>Depth_at_Water_Level_Points</EM>) des Objektes "t_h_grid" <BR>
  !! a) Arbeitsobjekt mit Skalar f&uuml;llen
  interface setup_h_grid_depth_at_w_point
     module procedure setup_h_grid_dwlp_w0
  end interface
  !
  !! Drucken aller in diesem Modul abgelegten statischen Daten <BR>
  !! die nicht zu den (Paket-) Objekten geh&ouml;ren
  interface print_h_grid_static
     module procedure print_h_grid_static_d
  end interface
  !! Drucken aller (m&ouml;glichen) Fehlermeldungen dieses Packages
  interface print_h_grid_all_errors
     module procedure print_h_grid_all_errors_d
  end interface
  !! Drucken von Datenobjekten "t_h_grid" (work-object)       <BR>
  !! a) Ausgabe des Inhalts des aktuellen (Arbeits-) Objektes
  interface print_h_grid
     module procedure print_h_grid_w ! Version fuer Arbeits-Objekt
  end interface
  !! Erzeugen von (Package-) Datenobjekten "t_h_grid" (Skalar, 1D-Array) <BR>
  !! a) Erzeugen eines Objektes <BR>
  !! b) Erzeugen vieler Objekte
  interface new_h_grid
     module procedure new_h_grid_0  ! Version fuer Skalar
     module procedure new_h_grid_1  ! Version fuer 1D-Array
  end interface
  !! Vernichten von (Package-) Datenobjekten "t_h_grid" (Skalar, 1D-Array) <BR>
  !! a) Vernichten des aktuellen (Arbeits-) Objektes <BR>
  !! b) Vernichten eines Objektes mit Identifikationsnummer <BR>
  !! c) Vernichten mehrerer Objekte mit Identifikationsnummer
  interface kill_h_grid
     module procedure kill_h_grid_w  ! Version fuer Arbeits-Objekt
     module procedure kill_h_grid_0  ! Version fuer ein Objekt mit ID
     module procedure kill_h_grid_1  ! Version fuer mehrere Objekte mit ID
  end interface
  !! Pr&uuml;fen von (Package-) Datenobjekten "t_h_grid" <BR>
  !! a) Pr&uuml;fen des aktuellen (Arbeits-) Objektes
  interface ok_h_grid
     module procedure ok_h_grid_w ! Version fuer Arbeits-Objekt
  end interface
  !! Anzahl der vorhandenen (Package-) Objekte "t_h_grid"
  interface get_h_grid_nofobjects
     module procedure get_h_grid_nofobjects_d
  end interface
  !! Identifikationsnummern aller vorhandenen (Package-) Objekte "t_h_grid"
  interface get_h_grid_all_id
     module procedure get_h_grid_all_id_d
  end interface
  !! Identifikationsnummer des aktuellen Arbeitsobjektes ermitteln
  interface get_h_grid_work_object_id
     module procedure get_h_grid_work_object_id_d
  end interface
  !! Holen einer Kopie der Komponente "name" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_name
     module procedure get_h_grid_name_w0
  end interface
  !! Holen einer Kopie der Komponente "file" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_file
     module procedure get_h_grid_file_w0
  end interface
  !! Hole die Kenn-Nummer f&uuml;r den aktuellen Gittertyp
  interface get_h_grid_act_variant_type
     module procedure get_h_grid_act_variant_type_w0
  end interface
  !! Hole Pointer auf Feld-Komponente "nv" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_nof_nodes
     module procedure get_h_grid_nv_w0
  end interface
  !! Hole Pointer auf Feld-Komponente "ns" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_nof_edges
     module procedure get_h_grid_ns_w0
  end interface
  !! Hole Pointer auf Feld-Komponente "nsi" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_nof_inside_edges
     module procedure get_h_grid_nsi_w0
  end interface
  !! Hole Pointer auf Feld-Komponente "nsf" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_last_flux_bc_edge
     module procedure get_h_grid_nsf_w0
  end interface
  !! Hole Pointer auf Feld-Komponente "ne" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_nof_poly
     module procedure get_h_grid_ne_w0
  end interface
  !! Hole Pointer auf Feld-Komponente "xy" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_node_coord
     module procedure get_h_grid_xy_w2
  end interface
  !! Hole Pointer auf Feld-Komponente "nen" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_nodelist_of_poly
     module procedure get_h_grid_nen_w2
  end interface
  !! Hole Pointer auf Feld-Komponente "irand" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_poly_bound_code
     module procedure get_h_grid_irand_w1
  end interface
  !! Hole Pointer auf Feld-Komponente "ks" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_nodes_of_poly
     module procedure get_h_grid_ks_w1
  end interface
  !! Hole Pointer auf Feld-Komponsente "hv" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_node_depth
     module procedure get_h_grid_hv_w1
  end interface
  !! Hole Pointer auf Komponente "nrand" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_nof_bound_nodes
     module procedure get_h_grid_nrand_w0
  end interface
  !! Hole Pointer auf Komponente "nptfr" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_nof_section_nodes
     module procedure get_h_grid_nptfr_w0
  end interface
  !! Hole Pointer auf Komponente "nptir" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_nof_intface_nodes
     module procedure get_h_grid_nptir_w0
  end interface
  !! Hole Pointer auf Komponente "time" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_datetime
     module procedure get_h_grid_time_w0
  end interface
  !! Hole Pointer auf Komponente "nbc" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_nof_bound_poly
     module procedure get_h_grid_nbc_w0
  end interface
  !! Hole Pointer auf Komponente "hland" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_min_water_depth
     module procedure get_h_grid_hland_w0
  end interface
  !! Hole Pointer auf Komponente "angle" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_mean_latitude
     module procedure get_h_grid_angle_w0
  end interface
  !! Hole Pointer auf Komponente "text" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_text
     module procedure get_h_grid_text_w1
  end interface
  !! Hole Pointer auf Komponente "jb" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_fst_node_of_edge
     module procedure get_h_grid_jb_w1
  end interface
  !! Hole Pointer auf Komponente "jt" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_lst_node_of_edge
     module procedure get_h_grid_jt_w1
  end interface
  !! Hole Pointer auf Komponente "is" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_edgelist_of_poly
     module procedure get_h_grid_is_w2
  end interface
  !! Hole Pointer auf Komponente "je" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_polylist_of_edges
     module procedure get_h_grid_je_w2
  end interface
  !! Hole Pointer auf Komponente "ie" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_polylist_of_poly
     module procedure get_h_grid_ie_w2
  end interface
  !! Hole Pointer auf Komponente "xs" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_edge_center_coord
     module procedure get_h_grid_xs_w2
  end interface
  !! Hole Pointer auf Komponente "xc" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_center_coord
     module procedure get_h_grid_xc_w2
  end interface
  !! Hole Pointer auf Komponente "xg" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_grav_center_coord
     module procedure get_h_grid_xg_w2
  end interface
  !! Hole Pointer auf Komponente "dg" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_grav_center_dist
     module procedure get_h_grid_dg_w1
  end interface
  !! Hole Pointer auf Komponente "dx" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_center_dist
     module procedure get_h_grid_dx_w1
  end interface
  !! Hole Pointer auf Komponente "dy" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_edge_length
     module procedure get_h_grid_dy_w1
  end interface
  !! Hole Pointer auf Komponente "aa" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_poly_area
     module procedure get_h_grid_aa_w1
  end interface
  !! Hole Pointer auf Komponente "hu" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_edge_depth
     module procedure get_h_grid_hu_w1
  end interface
  !! Hole Pointer auf Komponente "hw" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_poly_depth
     module procedure get_h_grid_hw_w1
  end interface
  !! Hole Pointer auf Komponente "nr" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_nof_red_poly
     module procedure get_h_grid_nr_w0
  end interface
  !! Hole Pointer auf Komponente "ncsize" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_nof_cpus
     module procedure get_h_grid_ncsize_w0
  end interface
  !! Hole Pointer auf Komponente "ipobo" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_node_bound_code
     module procedure get_h_grid_ipobo_w1
  end interface
  !! Hole Pointer auf Komponente "dxmin" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_min_center_dist
     module procedure get_h_grid_dxmin_w0
  end interface
  !! Hole die Anzahl der Gitterpunkte in M-Richtung (soweit bekannt) <BR>
  !! a) f&uuml;r das aktuelle Arbietsobjekt
  interface get_h_grid_nof_m_points
     module procedure get_h_grid_m_w0
  end interface
  !! Hole die Anzahl der Gitterpunkte in N-Richtung (soweit bekannt) <BR>
  !! a) f&uuml;r das aktuelle Arbietsobjekt
  interface get_h_grid_nof_n_points
     module procedure get_h_grid_n_w0
  end interface
  !! Hole Pointer auf Komponente "enc(:,:)" des Objektes "t_h_grid" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_enclosure
     module procedure get_h_grid_enc_w0
  end interface
  !! Hole Pointer auf Komponente "dry(:,:)" des Objektes "t_h_grid" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_drypoints
     module procedure get_h_grid_dry_w0
  end interface
  !! Hole Pointer auf Komponente "bnd(:)%name" des Objektes "t_h_grid" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
!LEO  interface get_h_grid_openbc_name
!LEO     module procedure get_h_grid_bnd_name_w0
!LEO  end interface
  !! Hole Pointer auf Komponente "bnd(:)%bdry_type" des Objektes "t_h_grid" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
!LEO  interface get_h_grid_openbc_bdry_type
!LEO     module procedure get_h_grid_bnd_bdry_type_w0
!LEO  end interface
  !! Hole Pointer auf Komponente "bnd(:)%data_type" des Objektes "t_h_grid" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
!LEO  interface get_h_grid_openbc_data_type
!LEO     module procedure get_h_grid_bnd_data_type_w0
!LEO  end interface
  !! Hole Pointer auf Komponente "bnd(:)%refl_coef" des Objektes "t_h_grid" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
!LEO  interface get_h_grid_openbc_refl_coef
!LEO     module procedure get_h_grid_bnd_refl_coef_w0
!LEO  end interface
  !! Hole Pointer auf Komponente "bnd(:)%prof" des Objektes "t_h_grid" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
!LEO  interface get_h_grid_openbc_prof
!LEO     module procedure get_h_grid_bnd_prof_w0
!LEO  end interface
  !! Hole Pointer auf Komponente "bnd(:)%bloc_ampl" des Objektes "t_h_grid" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
!LEO  interface get_h_grid_openbc_bloc_ampl
!LEO     module procedure get_h_grid_bnd_bloc_ampl_w0
!LEO  end interface
  !! Hole Pointer auf Komponente "bnd(:)%bloc_phas" des Objektes "t_h_grid" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
!LEO  interface get_h_grid_openbc_bloc_phas
!LEO     module procedure get_h_grid_bnd_bloc_phas_w0
!LEO  end interface
  !! Ermittle die max. Anzahl der in der Komponente "bnd(:)%grid_coor(:)" 
  !! des Objektes "t_h_grid" abgelegten Informationen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_openbc_max_grid_coor
     module procedure get_h_grid_bnd_max_grid_coor_w0
  end interface
  !! Hole Pointer auf Komponente "bnd(:)%grid_coor(no)" des Objektes "t_h_grid" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
!LEO  interface get_h_grid_openbc_grid_coor
!LEO     module procedure get_h_grid_bnd_grid_coor_w0
!LEO  end interface
  !! Hole Pointer auf Komponente "thd(:)%type" des Objektes "t_h_grid" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
!LEO  interface get_h_grid_thdams_type
!LEO     module procedure get_h_grid_thd_type_w0
!LEO  end interface
  !! Ermittle die max. Anzahl der in der Komponente "thd(:)%grid_coor(:)" 
  !! des Objektes "t_h_grid" abgelegten Informationen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_thdams_max_grid_coor
     module procedure get_h_grid_thd_max_grid_coor_w0
  end interface
  !! Hole Pointer auf Komponente "thd(:)%grid_coor(no)" des Objektes "t_h_grid" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
!LEO  interface get_h_grid_thdams_grid_coor
!LEO     module procedure get_h_grid_thd_grid_coor_w0
!LEO  end interface
  !! Hole Pointer auf Komponente "lwl(:)%type" des Objektes "t_h_grid" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
!LEO  interface get_h_grid_lweirs_type
!LEO     module procedure get_h_grid_lwl_type_w0
!LEO  end interface
  !! Ermittle die max. Anzahl der in der Komponente "lwl(:)%grid_coor(:)" 
  !! des Objektes "t_h_grid" abgelegten Informationen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_lweirs_max_grid_coor
     module procedure get_h_grid_lwl_max_grid_coor_w0
  end interface
  !! Hole Pointer auf Komponente "lwl(:)%grid_coor(no)" des Objektes "t_h_grid" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
!LEO  interface get_h_grid_lweirs_grid_coor
!LEO     module procedure get_h_grid_lwl_grid_coor_w0
!LEO  end interface
  !! Hole Pointer auf Komponente "lwl(:)%friction" des Objektes "t_h_grid" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
!LEO  interface get_h_grid_lweirs_friction
!LEO     module procedure get_h_grid_lwl_friction_w0
!LEO  end interface
  !! Hole Pointer auf Komponente "lwl(:)%sill_depth" des Objektes "t_h_grid" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
!LEO  interface get_h_grid_lweirs_sill_depth
!LEO     module procedure get_h_grid_lwl_sill_depth_w0
!LEO  end interface
  !! Hole Pointer auf Komponente "ext(:)%type" des Objektes "t_h_grid" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
!LEO  interface get_h_grid_dweirs_type
!LEO     module procedure get_h_grid_ext_type_w0
!LEO  end interface
  !! Ermittle die max. Anzahl der in der Komponente "ext(:)%grid_coor(:)" 
  !! des Objektes "t_h_grid" abgelegten Informationen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_dweirs_max_grid_coor
     module procedure get_h_grid_ext_max_grid_coor_w0
  end interface
  !! Hole Pointer auf Komponente "ext(:)%grid_coor(no)" des Objektes "t_h_grid" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
!LEO  interface get_h_grid_dweirs_grid_coor
!LEO     module procedure get_h_grid_ext_grid_coor_w0
!LEO  end interface
  !! Hole Pointer auf Komponente "ext(:)%friction" des Objektes "t_h_grid" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
!LEO  interface get_h_grid_dweirs_friction
!LEO     module procedure get_h_grid_ext_friction_w0
!LEO  end interface
  !! Hole Pointer auf Komponente "ext(:)%sill_depth" des Objektes "t_h_grid" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
!LEO  interface get_h_grid_dweirs_sill_depth
!LEO     module procedure get_h_grid_ext_sill_depth_w0
!LEO  end interface
  !! Holen der unerodierbaren Tiefen "huu(:)" an den Kanten des Objektes "t_h_grid" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_edge_noero_depth
     module procedure get_h_grid_huu_w0
  end interface
  !! Holen der unerodierbaren Tiefen "hvu(:)" an den Knoten des Objektes "t_h_grid" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_node_noero_depth
     module procedure get_h_grid_hvu_w0
  end interface
  !! Holen der unerodierbaren Tiefen "hwu(:)" an den Polygonen des Objektes "t_h_grid" <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface get_h_grid_poly_noero_depth
     module procedure get_h_grid_hwu_w0
  end interface
  !! Ermittle den aktuellen maximalen Wert einer klassischen Fortran-Dimension
  !! in allen Objekten (alle Werte sind garantiert gr&ouml;&szlig;er als Null)<BR>
  !! a) f&uuml;r eine Dimension (gem&auml;&szlig; Name) <BR>
  !! b) f&uuml;r mehrere Dimensionen (gem&auml;&szlig; Namensliste)
  interface get_max_h_grid_olddim
     module procedure get_max_h_grid_olddim_0
     module procedure get_max_h_grid_olddim_1
  end interface
  !! Transfer des Inhalts verschiedener Komponenten in klassische Felder <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt <BR>
  !! b) f&uuml;r das aktuelle Arbeitsobjekt mit expliziter Angabe der Datei <BR>
  !! <EM>Anmerkung:</EM> die Daten muessen schon in dem Paket vorhanden sein
  interface aux_h_grid_get
     module procedure aux_h_grid_get_w
     module procedure aux_h_grid_get_wf
  end interface
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "nv" (Feld) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_nof_nodes
     module procedure target_h_grid_nv_w0
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "ns" (Feld) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_nof_edges
     module procedure target_h_grid_ns_w0
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "nsi" (Feld) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_nof_inside_edges
     module procedure target_h_grid_nsi_w0
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "nsf" (Feld) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_last_flux_bc_edge
     module procedure target_h_grid_nsf_w0
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "ne" (Feld) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_nof_poly
     module procedure target_h_grid_ne_w0
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "xy" (Feld) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_node_coord
     module procedure target_h_grid_xy_w2
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "nen" (Feld) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_nodelist_of_poly
     module procedure target_h_grid_nen_w2
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "irand" (Feld) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_poly_bound_code
     module procedure target_h_grid_irand_w1
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "ks" (Feld) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_nodes_of_poly
     module procedure target_h_grid_ks_w1
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "hv" (Feld) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_node_depth
     module procedure target_h_grid_hv_w1
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "nrand" (Skalar) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_nof_bound_nodes
     module procedure target_h_grid_nrand_w0
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "nptfr" (Skalar) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_nof_section_nodes
     module procedure target_h_grid_nptfr_w0
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "nptir" (Skalar) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_nof_intface_nodes
     module procedure target_h_grid_nptir_w0
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "time" (Skalar) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_datetime
     module procedure target_h_grid_time_w0
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "nbc" (Skalar) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_nof_bound_poly
     module procedure target_h_grid_nbc_w0
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "hland" (Skalar) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_min_water_depth
     module procedure target_h_grid_hland_w0
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "angle" (Skalar) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_mean_latitude
     module procedure target_h_grid_angle_w0
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "text" (Vektor) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_text
     module procedure target_h_grid_text_w1
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "jb" (Vektor) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_fst_node_of_edge
     module procedure target_h_grid_jb_w1
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "jt" (Vektor) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_lst_node_of_edge
     module procedure target_h_grid_jt_w1
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "is" (Vektor) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_edgelist_of_poly
     module procedure target_h_grid_is_w2
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "je" (Vektor) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_polylist_of_edges
     module procedure target_h_grid_je_w2
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "ie" (Vektor) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_polylist_of_poly
     module procedure target_h_grid_ie_w2
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "xs" (Vektor) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_edge_center_coord
     module procedure target_h_grid_xs_w2
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "xc" (Vektor) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_center_coord
     module procedure target_h_grid_xc_w2
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "xg" (Vektor) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_grav_center_coord
     module procedure target_h_grid_xg_w2
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "dg" (Vektor) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_grav_center_dist
     module procedure target_h_grid_dg_w1
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "dx" (Vektor) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_center_dist
     module procedure target_h_grid_dx_w1
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "dy" (Vektor) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_edge_length
     module procedure target_h_grid_dy_w1
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "aa" (Vektor) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_poly_area
     module procedure target_h_grid_aa_w1
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "hu" (Vektor) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_edge_depth
     module procedure target_h_grid_hu_w1
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "hw" (Vektor) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_poly_depth
     module procedure target_h_grid_hw_w1
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "ipobo" (Vektor) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_node_bound_code
     module procedure target_h_grid_ipobo_w1
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "dxmin" (Skalar) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_min_center_dist
     module procedure target_h_grid_dxmin_w0
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "nr" (Skalar) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_nof_red_poly
     module procedure target_h_grid_nr_w0
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "ncsize" (Skalar) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_nof_cpus
     module procedure target_h_grid_ncsize_w0
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "m" (Skalar) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_nof_m_points
     module procedure target_h_grid_m_w0
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "n" (Skalar) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_nof_n_points
     module procedure target_h_grid_n_w0
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "enc(:,:)" (Feld) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_enclosure
     module procedure target_h_grid_enc_w0
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "dry(:,:)" (Feld) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_drypoints
     module procedure target_h_grid_dry_w0
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "huu(:)" (Feld) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_edge_noero_depth
     module procedure target_h_grid_huu_w0
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "hvu(:)" (Feld) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_node_noero_depth
     module procedure target_h_grid_hvu_w0
  end interface
  !! Pr&uuml;fe ob Pointer f&uuml;r "hwu(:)" (Feld) &uuml;bereinstimmen <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt
  interface target_h_grid_poly_noero_depth
     module procedure target_h_grid_hwu_w0
  end interface
  !
  !! Lesen der Daten von Datei <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt <BR>
  !! b) f&uuml;r das aktuelle Arbeitsobjekt mit expliziter Angabe der Datei
  interface read_h_grid
     module procedure read_h_grid_w
     module procedure read_h_grid_wf
  end interface
  !! Schreiben der Daten in Datei <BR>
  !! a) f&uuml;r das aktuelle Arbeitsobjekt <BR>
  !! b) f&uuml;r das aktuelle Arbeitsobjekt mit expliziter Angabe der Datei
  interface write_h_grid
     module procedure write_h_grid_w
     module procedure write_h_grid_wf
  end interface
  !! Hole die Anzahl der implementierten Datei-Varianten
  interface get_h_grid_nof_variants
     module procedure get_h_grid_nof_variants_d
  end interface
  !! Hole den Datei-Type f&uuml;r das Paket
  interface get_h_grid_variants_type
     module procedure get_h_grid_variants_type_d
     module procedure get_h_grid_variants_type_0
  end interface
  !! Hole den Datei-Code f&uuml;r das Paket
  interface get_h_grid_variants_code
     module procedure get_h_grid_variants_code_d
     module procedure get_h_grid_variants_code_0
  end interface
  !! Hole FORTRAN-Form der implementierten Datei-Varianten
  interface get_h_grid_variants_form
     module procedure get_h_grid_variants_form_d
     module procedure get_h_grid_variants_form_0
  end interface
  !! Hole FORTRAN-Access der implementierten Datei-Varianten
  interface get_h_grid_variants_access
     module procedure get_h_grid_variants_access_d
     module procedure get_h_grid_variants_access_0
  end interface
  !! Hole FORTRAN-Delimiter der implementierten Datei-Varianten
  interface get_h_grid_variants_delim
     module procedure get_h_grid_variants_delim_d
     module procedure get_h_grid_variants_delim_0
  end interface
  !
  !! wandle ein Gitter in das UNTRIM-Format um
  interface convert_h_grid_to_gitter05
     module procedure convert_h_grid_to_gitter05_w0
  end interface
  !! wandle ein Gitter in das UNTRIM-Format um
  interface convert_h_grid_to_untrim
     module procedure convert_h_grid_to_untrim_w0
  end interface
  !! f&auml;rbe die Polygone mit den Farben Rot und Schwarz ein
  interface convert_h_grid_red_black_poly
     module procedure convert_h_grid_rb_w0
  end interface
  !! sortiere die Kanten nach Innen- und Au&szlig;enkanten
  interface convert_h_grid_nsi
     module procedure convert_h_grid_nsi_w0
  end interface
  !! eliminiere tote Volumina aus Gitternetz
  interface convert_h_grid_terrace
     module procedure convert_h_grid_terrace_w0
  end interface
  !! Verfeinere ein Gitternetz
  interface refine_h_grid
     module procedure refine_h_grid_w0
  end interface
  !! Vertiefe ein Gitter
  interface deepen_h_grid
     module procedure deepen_h_grid_w0
  END INTERFACE
  !
  !! Erzeuge alle Austauschgr&ouml;&szlig;en mit OpenMI-konformen Daten 
  !! f&uuml;r eine Liste von Arbeitsobjekten 
  INTERFACE get_h_grid_exch_r
     MODULE PROCEDURE get_h_grid_exch_r_1
  END INTERFACE
  !! Ermittle den Zeithorizont in OpenMI-konformer Weise f&uuml;r 
  !! f&uuml;r eine Liste von Arbeitsobjekten 
  INTERFACE get_h_grid_time_horizon
     MODULE PROCEDURE get_h_grid_time_horizon_1
  END INTERFACE
  !! Ermittle, ob diskrete Zeitangaben in OpenMI-konformer Weise f&uuml;r 
  !! ein Arbeitsobjekt vorhanden sind
  INTERFACE get_h_grid_has_discr_time
     MODULE PROCEDURE get_h_grid_has_discr_time_w
  END INTERFACE
  !! Ermittle, wie viele Zeitangaben in OpenMI-konformer Weise f&uuml;r 
  !! ein Arbeitsobjekt vorhanden sind
  INTERFACE get_h_grid_nof_discr_time
     MODULE PROCEDURE get_h_grid_nof_discr_time_w
  END INTERFACE
  !! Ermittle eine bestimmte Zeitangabe in OpenMI-konformer Weise f&uuml;r 
  !! ein Arbeitsobjekt anhand der lfd. Nummer der Zeitangabe
  INTERFACE get_h_grid_discr_time
     MODULE PROCEDURE get_h_grid_discr_time_w
  END INTERFACE
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  ! [C.5] Zuweisungen
  ! [C.6] Operatoren (optional, falls sinnvoll)
  ! [C.7] Liste der oeffentlichen Methoden
  PUBLIC :: init_h_grid                     
  PUBLIC :: clear_h_grid                    
  PUBLIC :: setup_h_grid_prn_lun            
  PUBLIC :: setup_h_grid_trc_lun            
  PUBLIC :: setup_h_grid_asc_seq_lun        
  PUBLIC :: setup_h_grid_bin_seq_lun        
  PUBLIC :: setup_h_grid_bin_dir_lun        
  PUBLIC :: setup_h_grid_work_object        
  PUBLIC :: setup_h_grid_name               
  PUBLIC :: setup_h_grid_file               
  PUBLIC :: setup_h_grid_node_coord         
  PUBLIC :: setup_h_grid_nodelist_of_poly   
  PUBLIC :: setup_h_grid_poly_bound_code    
  PUBLIC :: setup_h_grid_nodes_of_poly      
  PUBLIC :: setup_h_grid_node_depth         
  PUBLIC :: setup_h_grid_nof_bound_nodes    
  PUBLIC :: setup_h_grid_nof_section_nodes    
  PUBLIC :: setup_h_grid_nof_intface_nodes    
  PUBLIC :: setup_h_grid_datetime           
  PUBLIC :: setup_h_grid_nof_bound_poly     
  PUBLIC :: setup_h_grid_min_water_depth    
  PUBLIC :: setup_h_grid_mean_latitude      
  PUBLIC :: setup_h_grid_text               
  PUBLIC :: setup_h_grid_fst_node_of_edge   
  PUBLIC :: setup_h_grid_lst_node_of_edge   
  PUBLIC :: setup_h_grid_edgelist_of_poly   
  PUBLIC :: setup_h_grid_polylist_of_edges  
  PUBLIC :: setup_h_grid_polylist_of_poly   
  PUBLIC :: setup_h_grid_edge_center_coord  
  PUBLIC :: setup_h_grid_center_coord       
  PUBLIC :: setup_h_grid_grav_center_coord  
  PUBLIC :: setup_h_grid_grav_center_dist
  PUBLIC :: setup_h_grid_center_dist        
  PUBLIC :: setup_h_grid_edge_length        
  PUBLIC :: setup_h_grid_poly_area          
  PUBLIC :: setup_h_grid_edge_depth         
  PUBLIC :: setup_h_grid_poly_depth         
  PUBLIC :: setup_h_grid_nof_red_poly       
  PUBLIC :: setup_h_grid_nof_cpus           
  PUBLIC :: setup_h_grid_node_bound_code    
  PUBLIC :: setup_h_grid_min_center_dist    
  PUBLIC :: setup_h_grid_nof_m_points
  PUBLIC :: setup_h_grid_nof_n_points
  PUBLIC :: setup_h_grid_enclosure
  PUBLIC :: setup_h_grid_drypoints
  PUBLIC :: setup_h_grid_openbc
  PUBLIC :: setup_h_grid_thin_dams
  PUBLIC :: setup_h_grid_local_weirs
  PUBLIC :: setup_h_grid_2d_weirs
  PUBLIC :: setup_h_grid_edge_noero_depth
  PUBLIC :: setup_h_grid_node_noero_depth
  PUBLIC :: setup_h_grid_poly_noero_depth
  PUBLIC :: setup_h_grid_depth_at_w_point
  !
  PUBLIC :: new_h_grid                      
  PUBLIC :: kill_h_grid                     
  PUBLIC :: print_h_grid                    
  PUBLIC :: print_h_grid_static             
  PUBLIC :: print_h_grid_all_errors         
  PUBLIC :: ok_h_grid                       
  PUBLIC :: get_h_grid_nofobjects           
  PUBLIC :: get_h_grid_all_id               
  PUBLIC :: get_h_grid_work_object_id       
  PUBLIC :: get_h_grid_name                 
  PUBLIC :: get_h_grid_file                 
  PUBLIC :: get_h_grid_act_variant_type     
  PUBLIC :: get_h_grid_nof_nodes            
  PUBLIC :: get_h_grid_nof_edges            
  PUBLIC :: get_h_grid_nof_inside_edges
  PUBLIC :: get_h_grid_last_flux_bc_edge
  PUBLIC :: get_h_grid_nof_poly             
  PUBLIC :: get_h_grid_node_coord           
  PUBLIC :: get_h_grid_nodelist_of_poly     
  PUBLIC :: get_h_grid_poly_bound_code      
  PUBLIC :: get_h_grid_nodes_of_poly        
  PUBLIC :: get_h_grid_node_depth           
  PUBLIC :: get_h_grid_nof_bound_nodes      
  PUBLIC :: get_h_grid_nof_section_nodes      
  PUBLIC :: get_h_grid_nof_intface_nodes      
  PUBLIC :: get_h_grid_datetime             
  PUBLIC :: get_h_grid_nof_bound_poly       
  PUBLIC :: get_h_grid_min_water_depth      
  PUBLIC :: get_h_grid_mean_latitude        
  PUBLIC :: get_h_grid_text                 
  PUBLIC :: get_h_grid_fst_node_of_edge     
  PUBLIC :: get_h_grid_lst_node_of_edge     
  PUBLIC :: get_h_grid_edgelist_of_poly     
  PUBLIC :: get_h_grid_polylist_of_edges    
  PUBLIC :: get_h_grid_polylist_of_poly     
  PUBLIC :: get_h_grid_edge_center_coord    
  PUBLIC :: get_h_grid_center_coord         
  PUBLIC :: get_h_grid_grav_center_coord    
  PUBLIC :: get_h_grid_grav_center_dist
  PUBLIC :: get_h_grid_center_dist          
  PUBLIC :: get_h_grid_edge_length          
  PUBLIC :: get_h_grid_poly_area            
  PUBLIC :: get_h_grid_edge_depth           
  PUBLIC :: get_h_grid_poly_depth           
  PUBLIC :: get_h_grid_nof_red_poly         
  PUBLIC :: get_h_grid_nof_cpus           
  PUBLIC :: get_h_grid_node_bound_code      
  PUBLIC :: get_h_grid_min_center_dist      
  PUBLIC :: get_h_grid_nof_m_points
  PUBLIC :: get_h_grid_nof_n_points
  PUBLIC :: get_h_grid_enclosure
  PUBLIC :: get_h_grid_drypoints
!LEO  PUBLIC :: get_h_grid_openbc_name
!LEO  PUBLIC :: get_h_grid_openbc_bdry_type
!LEO  PUBLIC :: get_h_grid_openbc_data_type
!LEO  PUBLIC :: get_h_grid_openbc_refl_coef
!LEO  PUBLIC :: get_h_grid_openbc_prof
!LEO  PUBLIC :: get_h_grid_openbc_bloc_ampl
!LEO  PUBLIC :: get_h_grid_openbc_bloc_phas
  PUBLIC :: get_h_grid_openbc_max_grid_coor
!LEO  PUBLIC :: get_h_grid_openbc_grid_coor
!LEO  PUBLIC :: get_h_grid_thdams_type
  PUBLIC :: get_h_grid_thdams_max_grid_coor
!LEO  PUBLIC :: get_h_grid_thdams_grid_coor
!LEO  PUBLIC :: get_h_grid_lweirs_type
  PUBLIC :: get_h_grid_lweirs_max_grid_coor
!LEO  PUBLIC :: get_h_grid_lweirs_grid_coor
!LEO  PUBLIC :: get_h_grid_lweirs_friction
!LEO  PUBLIC :: get_h_grid_lweirs_sill_depth
!LEO  PUBLIC :: get_h_grid_dweirs_type
  PUBLIC :: get_h_grid_dweirs_max_grid_coor
!LEO  PUBLIC :: get_h_grid_dweirs_grid_coor
!LEO  PUBLIC :: get_h_grid_dweirs_friction
!LEO  PUBLIC :: get_h_grid_dweirs_sill_depth
  PUBLIC :: get_h_grid_edge_noero_depth
  PUBLIC :: get_h_grid_node_noero_depth
  PUBLIC :: get_h_grid_poly_noero_depth
  PUBLIC :: get_max_h_grid_olddim
  !
  PUBLIC :: target_h_grid_nof_nodes         
  PUBLIC :: target_h_grid_nof_edges         
  PUBLIC :: target_h_grid_nof_inside_edges
  PUBLIC :: target_h_grid_last_flux_bc_edge
  PUBLIC :: target_h_grid_nof_poly          
  PUBLIC :: target_h_grid_node_coord        
  PUBLIC :: target_h_grid_nodelist_of_poly  
  PUBLIC :: target_h_grid_poly_bound_code   
  PUBLIC :: target_h_grid_nodes_of_poly     
  PUBLIC :: target_h_grid_node_depth        
  PUBLIC :: target_h_grid_nof_bound_nodes   
  PUBLIC :: target_h_grid_nof_section_nodes   
  PUBLIC :: target_h_grid_nof_intface_nodes   
  PUBLIC :: target_h_grid_datetime          
  PUBLIC :: target_h_grid_nof_bound_poly    
  PUBLIC :: target_h_grid_min_water_depth   
  PUBLIC :: target_h_grid_mean_latitude     
  PUBLIC :: target_h_grid_text              
  PUBLIC :: target_h_grid_fst_node_of_edge  
  PUBLIC :: target_h_grid_lst_node_of_edge  
  PUBLIC :: target_h_grid_edgelist_of_poly  
  PUBLIC :: target_h_grid_polylist_of_edges 
  PUBLIC :: target_h_grid_polylist_of_poly  
  PUBLIC :: target_h_grid_edge_center_coord 
  PUBLIC :: target_h_grid_center_coord      
  PUBLIC :: target_h_grid_grav_center_coord 
  PUBLIC :: target_h_grid_grav_center_dist
  PUBLIC :: target_h_grid_center_dist       
  PUBLIC :: target_h_grid_edge_length       
  PUBLIC :: target_h_grid_poly_area         
  PUBLIC :: target_h_grid_edge_depth        
  PUBLIC :: target_h_grid_poly_depth        
  PUBLIC :: target_h_grid_nof_red_poly      
  PUBLIC :: target_h_grid_nof_cpus           
  PUBLIC :: target_h_grid_node_bound_code   
  PUBLIC :: target_h_grid_min_center_dist   
  PUBLIC :: target_h_grid_nof_m_points
  PUBLIC :: target_h_grid_nof_n_points
  PUBLIC :: target_h_grid_enclosure
  PUBLIC :: target_h_grid_drypoints
  PUBLIC :: target_h_grid_edge_noero_depth
  PUBLIC :: target_h_grid_node_noero_depth
  PUBLIC :: target_h_grid_poly_noero_depth
  !
  PUBLIC :: read_h_grid                     
  PUBLIC :: write_h_grid                    
  PUBLIC :: get_h_grid_nof_variants         
  PUBLIC :: get_h_grid_variants_type        
  PUBLIC :: get_h_grid_variants_code        
  PUBLIC :: get_h_grid_variants_form        
  PUBLIC :: get_h_grid_variants_access      
  PUBLIC :: get_h_grid_variants_delim       
  PUBLIC :: convert_h_grid_to_gitter05      
  PUBLIC :: convert_h_grid_to_untrim        
  PUBLIC :: convert_h_grid_red_black_poly   
  PUBLIC :: convert_h_grid_nsi
  PUBLIC :: convert_h_grid_terrace
  PUBLIC :: refine_h_grid
  PUBLIC :: deepen_h_grid
  PUBLIC :: get_h_grid_exch_r
  PUBLIC :: get_h_grid_time_horizon
  PUBLIC :: get_h_grid_has_discr_time
  PUBLIC :: get_h_grid_nof_discr_time
  PUBLIC :: get_h_grid_discr_time
  !
  PUBLIC :: aux_h_grid_get                  ! Transfer von Paketdaten in klassische Datenfelder
  !
  PUBLIC :: c_len_d3d_openbc_name, c_len_d3d_openbc_type, c_max_d3d_openbc_coor ! 
  PUBLIC :: c_len_d3d_openbc_prof, c_len_d3d_openbc_bloc, c_max_d3d_dry         ! 
  PUBLIC :: c_len_d3d_uv_type, c_max_d3d_uv                                     ! 
  !
  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Datentypen, Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] lokale Typdefinitionen
  ! [D.2] Konstantwerte (Parameter)
  !! Name des Moduls
  CHARACTER (LEN=11), PARAMETER :: c_modname      = 'p_h_grid_ui'
  !
  ! [D.3] Variablen / Felder
  !! Z&auml;hler f&uuml;r Initialisierungsaufrufe
  INTEGER                        , PUBLIC , SAVE :: n_init      = 0       ! 
  !
  ! [D.4] Schnittstellen
  !
  !! &Uuml;berpr&uuml;fen der Feldgrenzen verschiedene Felder <BR>
  !! Werfen der Fehlermeldung 50000 , falls die erforderliche Bedingung verletzt wird <BR>
  !! a) ein-dimensionale INTEGER-Felder <BR>
  !! c) ein-dimensionales INTEGER-Feld (extern) und konstante Feldgrenze (intern) <BR>
  !! b) ein-dimensionale REAL-Felder
  INTERFACE check_ext_arr_siz
     MODULE PROCEDURE check_ext_arr_siz_i_1
     MODULE PROCEDURE check_ext_arr_siz_i_0
     MODULE PROCEDURE check_ext_arr_siz_r_1
  END INTERFACE 
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
  !! Allokieren/Initialisieren der statischen Daten des Packages <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE init_h_grid_d ( )
    !
    USE b_error, ONLY : debug_b
    !

    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_b > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "p_h_grid_ui" version 4.9 of 03/05/07                '
          WRITE(*,*) ' Copyright (C) 2002 Bundesanstalt fuer Wasserbau       '
          WRITE(*,*)
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       IF ( no_error( ) ) CALL init_file ( )
       IF ( no_error( ) ) CALL init_point_2d ( )
       IF ( no_error( ) ) CALL init_datetime ( )
       IF ( no_error( ) ) CALL init_omi_exch_r ( )
       IF ( no_error( ) ) CALL init_omi_span ( )
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] alle weiteren Package-Module initialisieren (siehe [A.4])
       ! [1.5] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_h_grid_all_errors ( )
       ! [1.6] Initialisieren der logischen Kanalnummern
       prn_lun     = c_lun
       trc_lun     = c_lun
       asc_seq_lun = c_asc_seq_lun 
       bin_seq_lun = c_bin_seq_lun 
       bin_dir_lun = c_bin_dir_lun 
       ! [1.7] Initialisieren einiger statischer Variablen
       nofobjects = 0
       NULLIFY ( first_list_object )
       NULLIFY ( work_list_object )
       NULLIFY ( work_object  )
       ! [1.8] endgueltiges Setzen des Initialisierungs-Indikators
       initialised = MERGE( .true., .false., no_error( ) )
    END IF
    ! 2.0 Initialisierungszaehler heraufsetzen
    n_init = n_init + 1
    !
  END SUBROUTINE init_h_grid_d
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_h_grid_d ( )
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_h_grid_all_errors ( )
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       prn_lun     = c_lun
       trc_lun     = c_lun
       asc_seq_lun = c_asc_seq_lun 
       bin_seq_lun = c_bin_seq_lun 
       bin_dir_lun = c_bin_dir_lun 
       ! [1.3] alle weiteren Package-Module de-initialisieren (siehe [A.4])
       ! [1.4] ggf. weitere De-Initialsierungsmethoden rufen
       IF ( no_error( ) ) CALL clear_h_grid_all_objects ( )
       ! [1.5] Re-Initialisieren einiger statischer Variablen
       nofobjects = 0
       NULLIFY ( first_list_object )
       NULLIFY ( work_list_object )
       NULLIFY ( work_object  )
       ! [1.6] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.7] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.7.1] ggf. weitere Module de-initialisieren
       IF ( no_error( ) ) CALL clear_omi_exch_r ( )
       IF ( no_error( ) ) CALL clear_omi_span ( )
       IF ( no_error( ) ) CALL clear_datetime ( )
       IF ( no_error( ) ) CALL clear_point_2d ( )
       IF ( no_error( ) ) CALL clear_file ( )
       ! [1.7.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
    !
  END SUBROUTINE clear_h_grid_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_prn_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden
    INTEGER , INTENT(IN) :: lun
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER :: c_upname='setup_h_grid_prn_lun_d'
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       ! [1.1] PRN_LUNs der weiteren Package-Module setzen (siehe [A.4])
       ! [1.2] "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_file_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_point_2d_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_datetime_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_omi_exch_r_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_omi_span_prn_lun ( lun )
       ! [1.3] Setzen der lokalen statischen Daten
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
    END IF
    !
  END SUBROUTINE setup_h_grid_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_trc_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden
    INTEGER , INTENT(IN) :: lun
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER :: c_upname='setup_h_grid_trc_lun_d'
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       ! [1.1] TRC_LUNs der weiteren Package-Module setzen (siehe [A.4])
       ! [1.2] "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_file_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_point_2d_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_datetime_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_omi_exch_r_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_omi_span_trc_lun ( lun )
       ! [1.3] Setzen der lokalen statischen Daten
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
    END IF
    !
  END SUBROUTINE setup_h_grid_trc_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r Input/Output von Sequential, Ascii <BR>
  !! wird beim &Ouml;ffnen der Datei verwendet, falls "file%unit" negativ ist <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_asc_seq_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r Input/Output von Sequential, Ascii 
    INTEGER , INTENT(IN) :: lun
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER :: c_upname='setup_h_grid_asc_seq_lun_d'
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       asc_seq_lun = MERGE( lun, c_asc_seq_lun, lun > 0 )
    END IF
    !
  END SUBROUTINE setup_h_grid_asc_seq_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r Input/Output von Sequential, Binary <BR>
  !! wird beim &Ouml;ffnen der Datei verwendet, falls "file%unit" negativ ist <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_bin_seq_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r Input/Output von Sequential, Binary
    INTEGER , INTENT(IN) :: lun
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER :: c_upname='setup_h_grid_bin_seq_lun_d'
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       bin_seq_lun = MERGE( lun, c_bin_seq_lun, lun > 0 )
    END IF
    !
  END SUBROUTINE setup_h_grid_bin_seq_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r Input/Output von Direct, Binary <BR>
  !! wird beim &Ouml;ffnen der Datei verwendet, falls "file%unit" negativ ist <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_bin_dir_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r Input/Output von Direct, Binary
    INTEGER , INTENT(IN) :: lun
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER :: c_upname='setup_h_grid_bin_dir_lun_d'
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       bin_dir_lun = MERGE( lun, c_bin_dir_lun, lun > 0 )
    END IF
    !
  END SUBROUTINE setup_h_grid_bin_dir_lun_d
  !
  !! Setzen des (Arbeits-) Objekts mit dem gearbeitet werden soll <BR>
  !! Arbeitsobjekt &uuml;ber die Identifikationsnummer setzen
  SUBROUTINE setup_h_grid_work_object_d ( id )
    !! Identifikationsnummer des (Arbeits-) Objekts
    INTEGER , INTENT(IN) :: id
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER :: c_upname='setup_h_grid_work_object_d'
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       work_list_object => get_h_grid_list_object ( id )
       work_object      => work_list_object%object
    END IF
    !
  END SUBROUTINE setup_h_grid_work_object_d
  !
  !! Setzen des (Arbeits-) Objekts mit dem gearbeitet werden soll <BR>
  !! Arbeitsobjekt &uuml;ber den Namen setzen
  SUBROUTINE setup_h_grid_work_object_n ( name )
    !! Identifikationsnummer des (Arbeits-) Objekts
    CHARACTER (LEN=*) , INTENT(IN) :: name ! 
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER :: c_upname='setup_h_grid_work_object_n'
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       work_list_object => get_h_grid_list_object (name )
       work_object      => work_list_object%object
    END IF
    !
  END SUBROUTINE setup_h_grid_work_object_n
  !
  !! Setzen der Komponente "name" des (Arbeits-) Objektes mit Skalardaten <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_name_w0 ( val )
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "name"
    CHARACTER (LEN=*) , INTENT(IN) :: val
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='setup_h_grid_name_w0'
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_name_object ( work_object, val )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_name_w0
  !
  !! Setzen der Komponente "file" des (Arbeits-) Objektes mit Skalardaten <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_file_w0 ( val )
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "file"
    TYPE (t_file) , INTENT(IN) :: val
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='setup_h_grid_file_w0'
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_file_object ( work_object, val )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_file_w0
  !
  !! Setzen der Feld-Komponente "xy" des (Arbeits-) Objektes mit Vektordaten <BR>
  !! Zugleich wird die Anzahl Gitterknoten gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_xy_w2 ( val )
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "xy"
    REAL (KIND=Double) , INTENT(IN) :: val(:,:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_h_grid_xy_w2'
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_xy_object ( work_object, val(:,:) )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_xy_w2
  !
  !! Setzen der Feld-Komponente "nen" des (Arbeits-) Objektes mit Vektordaten <BR>
  !! Zugleich wird die Anzahl Gitterpolygone gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_nen_w2 ( val )
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "nen"
    INTEGER , INTENT(IN) :: val(:,:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER :: c_upname='setup_h_grid_nen_w2'
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_nen_object ( work_object, val(:,:) )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_nen_w2
  !
  !! Setzen der Feld-Komponente "irand" des (Arbeits-) Objektes mit Vektordaten <BR>
  !! Zugleich wird die Anzahl Gitterpolygone gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_irand_w1 ( val )
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "irand"
    INTEGER , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='setup_h_grid_irand_w1' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_irand_object ( work_object, val(:) )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_irand_w1
  !
  !! Setzen der Feld-Komponente "ks" des (Arbeits-) Objektes mit Vektordaten <BR>
  !! Zugleich wird die Anzahl Gitterpolygone gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_ks_w1 &
       ( val )
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "ks"
    INTEGER , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_h_grid_ks_w1' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_ks_object ( work_object, val(:) )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_ks_w1
  !
  !! Setzen der Feld-Komponente "hv" des (Arbeits-) Objektes mit Vektordaten <BR>
  !! Zugleich wird die Anzahl Gitterknoten gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_hv_w1 ( val )
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "hv"
    REAL (KIND=Double) , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_h_grid_hv_w1' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_hv_object ( work_object, val(:) )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_hv_w1
  !
  !! Setzen der Komponente "nrand" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_nrand_w0 ( val )
    !! zu setzender Wert f&uuml;r Komponente "nrand"
    INTEGER, INTENT(IN) :: val ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='setup_h_grid_nrand_w0' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_nrand_object ( work_object, val )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_nrand_w0
  !
  !! Setzen der Komponente "nptfr" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_nptfr_w0 ( val )
    !! zu setzender Wert f&uuml;r Komponente "nptfr"
    INTEGER, INTENT(IN) :: val ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='setup_h_grid_nptfr_w0' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_nptfr_object ( work_object, val )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_nptfr_w0
  !
  !! Setzen der Komponente "nptir" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_nptir_w0 ( val )
    !! zu setzender Wert f&uuml;r Komponente "nptir"
    INTEGER, INTENT(IN) :: val ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='setup_h_grid_nptir_w0' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_nptir_object ( work_object, val )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_nptir_w0
  !
  !! Setzen der Komponente "time" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_time_w0 ( val )
    !! zu setzender Wert f&uuml;r Komponente "time"
    TYPE (t_datetime), INTENT(IN) :: val ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='setup_h_grid_time_w0' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_time_object ( work_object, val )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_time_w0
  !
  !! Setzen der Komponente "nbc" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_nbc_w0 ( val )
    !! zu setzender Wert f&uuml;r Komponente "nbc"
    INTEGER , INTENT(IN) :: val ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER :: c_upname='setup_h_grid_nbc_w0' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_nbc_object ( work_object, val )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_nbc_w0
  !
  !! Setzen der Komponente "hland" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_hland_w0 ( val )
    !! zu setzender Wert f&uuml;r Komponente "hland"
    REAL (KIND=Double) , INTENT(IN) :: val ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='setup_h_grid_hland_w0' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_hland_object ( work_object, val )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_hland_w0
  !
  !! Setzen der Komponente "angle" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_angle_w0 ( val )
    !! zu setzender Wert f&uuml;r Komponente "angle"
    REAL (KIND=Double) , INTENT(IN) :: val ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='setup_h_grid_angle_w0' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_angle_object ( work_object, val )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_angle_w0
  !
  !! Setzen der Komponente "text" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_text_w1 ( val )
    !! zu setzender Wert f&uuml;r Komponente "text"
    CHARACTER (LEN=80) , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='setup_h_grid_text_w1' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_text_object ( work_object, val(:) )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_text_w1
  !
  !! Setzen der Feld-Komponente "jb" des (Arbeits-) Objektes mit Vektordaten <BR>
  !! Zugleich wird die Anzahl Gitterpolygone gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_jb_w1 ( val )
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "jb"
    INTEGER , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_h_grid_jb_w1' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_jb_object ( work_object, val(:) )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_jb_w1
  !
  !! Setzen der Feld-Komponente "jt" des (Arbeits-) Objektes mit Vektordaten <BR>
  !! Zugleich wird die Anzahl Gitterpolygone gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_jt_w1 ( val )
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "jt"
    INTEGER , INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_h_grid_jt_w1' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_jt_object ( work_object, val(:) )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_jt_w1
  !
  !! Setzen der Feld-Komponente "is" des (Arbeits-) Objektes mit Vektordaten <BR>
  !! Zugleich wird die Anzahl Gitterpolygone gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_is_w2 ( val )
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "is"
    INTEGER , INTENT(IN) :: val(:,:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_h_grid_is_w2' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_is_object ( work_object, val(:,:) )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_is_w2
  !
  !! Setzen der Feld-Komponente "je" des (Arbeits-) Objektes mit Vektordaten <BR>
  !! Zugleich wird die Anzahl Gitterkanten gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_je_w2 ( val )
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "je"
    INTEGER , INTENT(IN) :: val(:,:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_h_grid_je_w2' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_je_object ( work_object, val(:,:) )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_je_w2
  !
  !! Setzen der Feld-Komponente "ie" des (Arbeits-) Objektes mit Vektordaten <BR>
  !! Zugleich wird die Anzahl Gitterpolygone gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_ie_w2 ( val )
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "ie"
    INTEGER , INTENT(IN) :: val(:,:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_h_grid_ie_w2' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_ie_object ( work_object, val(:,:) )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_ie_w2
  !
  !! Setzen der Feld-Komponente "xs" des (Arbeits-) Objektes mit Vektordaten <BR>
  !! Zugleich wird die Anzahl Gitterkanten gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_xs_w2 ( val )
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "xs"
    REAL (KIND=Double), INTENT(IN) :: val(:,:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_h_grid_xs_w2'
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_xs_object ( work_object, val(:,:) )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_xs_w2
  !
  !! Setzen der Feld-Komponente "xc" des (Arbeits-) Objektes mit Vektordaten <BR>
  !! Zugleich wird die Anzahl Gitterpolygone gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_xc_w2 ( val )
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "xc"
    REAL (KIND=Double), INTENT(IN) :: val(:,:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_h_grid_xc_w2' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_xc_object ( work_object, val(:,:) )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_xc_w2
  !
  !! Setzen der Feld-Komponente "xg" des (Arbeits-) Objektes mit Vektordaten <BR>
  !! Zugleich wird die Anzahl Gitterpolygone gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_xg_w2 ( val )
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "xg"
    REAL (KIND=Double), INTENT(IN) :: val(:,:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_h_grid_xg_w2' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_xg_object ( work_object, val(:,:) )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_xg_w2
  !
  !! Setzen der Feld-Komponente "dg" des (Arbeits-) Objektes mit Vektordaten <BR>
  !! Zugleich wird die Anzahl Kanten gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_dg_w1 ( val )
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "dg"
    REAL (KIND=Double), INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_h_grid_dg_w1' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_dg_object ( work_object, val(:) )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_dg_w1
  !
  !! Setzen der Feld-Komponente "dx" des (Arbeits-) Objektes mit Vektordaten <BR>
  !! Zugleich wird die Anzahl Gitterkanten gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_dx_w1 ( val )
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "dx"
    REAL (KIND=Double), INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_h_grid_dx_w1' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_dx_object ( work_object, val(:) )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_dx_w1
  !
  !! Setzen der Feld-Komponente "dy" des (Arbeits-) Objektes mit Vektordaten <BR>
  !! Zugleich wird die Anzahl Gitterkanten gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_dy_w1 ( val )
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "dy"
    REAL (KIND=Double), INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_h_grid_dy_w1' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_dy_object ( work_object, val(:) )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_dy_w1
  !
  !! Setzen der Feld-Komponente "aa" des (Arbeits-) Objektes mit Vektordaten <BR>
  !! Zugleich wird die Anzahl Gitterpolygone gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_aa_w1 ( val )
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "aa"
    REAL (KIND=Double), INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_h_grid_aa_w1' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_aa_object ( work_object, val(:) )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_aa_w1
  !
  !! Setzen der Feld-Komponente "hu" des (Arbeits-) Objektes mit Vektordaten <BR>
  !! Zugleich wird die Anzahl Gitterkanten gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_hu_w1 ( val )
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "hu"
    REAL (KIND=Double), INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_h_grid_hu_w1' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_hu_object ( work_object, val(:) )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_hu_w1
  !
  !! Setzen der Feld-Komponente "hw" des (Arbeits-) Objektes mit Vektordaten <BR>
  !! Zugleich wird die Anzahl Gitterpolygone gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_hw_w1 ( val )
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "hw"
    REAL (KIND=Double), INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_h_grid_hw_w1' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_hw_object ( work_object, val(:) )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_hw_w1
  !
  !! Setzen der Feld-Komponente "ipobo" des (Arbeits-) Objektes mit Vektordaten <BR>
  !! Zugleich wird die Anzahl Gitterpolygone gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_ipobo_w1 ( val )
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "ipobo"
    INTEGER, INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='setup_h_grid_ipobo_w1' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_ipobo_object ( work_object, val(:) )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_ipobo_w1
  !
  !! Setzen der Feld-Komponente "dxmin" des (Arbeits-) Objektes mit Skalardaten <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_dxmin_w0 ( val )
    !! zu setzender Wert (Skalar) f&uuml;r Komponente "dxmin"
    REAL (KIND=Double), INTENT(IN) :: val ! 
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER :: c_upname='setup_h_grid_dxmin_w0' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_dxmin_object ( work_object, val )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_dxmin_w0
  !
  !! Setzen der Komponente "nr" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_nr_w0 ( val )
    !! zu setzender Wert f&uuml;r Komponente "nr"
    INTEGER, INTENT(IN) :: val ! 
    !! Name der Subroutine
    CHARACTER (LEN=18), PARAMETER :: c_upname='setup_h_grid_nr_w0' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_nr_object ( work_object, val )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_nr_w0
  !
  !! Setzen der Komponente "ncsize" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_ncsize_w0 ( val )
    !! zu setzender Wert f&uuml;r Komponente "ncsize"
    INTEGER, INTENT(IN) :: val ! 
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER :: c_upname='setup_h_grid_ncsize_w0' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_ncsize_object ( work_object, val )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_ncsize_w0
  !
  !! Setzen der Komponente "m" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_m_w0 ( val )
    !! zu setzender Wert f&uuml;r Komponente "m"
    INTEGER, INTENT(IN) :: val ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='setup_h_grid_m_w0' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_m_object ( work_object, val )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_m_w0
  !
  !! Setzen der Komponente "n" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_n_w0 ( val )
    !! zu setzender Wert f&uuml;r Komponente "n"
    INTEGER, INTENT(IN) :: val ! 
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER :: c_upname='setup_h_grid_n_w0' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_n_object ( work_object, val )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_n_w0
  !
  !! Setzen der Komponente "enc(:,:)" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_enc_w0 ( val )
    !! zu setzender Wert f&uuml;r Komponente "enc(:,:)"
    INTEGER, INTENT(IN) :: val(:,:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER :: c_upname='setup_h_grid_enc_w0' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_enc_object ( work_object, val )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_enc_w0
  !
  !! Setzen der Komponente "dry(:,:)" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_dry_w0 ( val )
    !! zu setzender Wert f&uuml;r Komponente "dry(:,:)"
    INTEGER, INTENT(IN) :: val(:,:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER :: c_upname='setup_h_grid_dry_w0' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_dry_object ( work_object, val )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_dry_w0
  !
  !! Setzen der Komponente "bnd(:)" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_bnd_w0 ( name, bdry_type, data_type, &
       m_coor1, n_coor1, m_coor2, n_coor2, refl_coef, prof, bloc_ampl, bloc_phas )
    !! Name des Randabschnitts
    CHARACTER (LEN=*) , INTENT(IN) :: name(:)           ! 
    !! Typ der Randbedingung auf dem Randabschnitt      <BR>
    !! Z : Wasserstand                                  <BR>
    !! C : Str&ouml;mung                                <BR>
    !! Q : Abflu&szlig; je Gitterzelle                  <BR>
    !! T : Abflu&szlig; auf dem gesamten Randabschnitt  <BR>
    !! R : Riemannsche Randbedingung
    CHARACTER (LEN=*) , INTENT(IN) :: bdry_type(:)      ! 
    !! Typ der Daten auf dem Randabschnitt              <BR>
    !! A : astronomisch                                 <BR>
    !! H : harmonisch                                   <BR>
    !! Q : QH-Tabelle                                   <BR>
    !! T : Zeitserie
    CHARACTER (LEN=*)  , INTENT(IN) :: data_type(:)     ! 
    !! Gitterkoordinaten (h-Punkte) der Randabschnitte  <BR>
    !! m-Koordinate des Anfangspunktes
    INTEGER            , INTENT(IN) :: m_coor1(:)       ! 
    !! Gitterkoordinaten (h-Punkte) der Randabschnitte  <BR>
    !! n-Koordinate des Anfangspunktes
    INTEGER            , INTENT(IN) :: n_coor1(:)       ! 
    !! Gitterkoordinaten (h-Punkte) der Randabschnitte  <BR>
    !! m-Koordinate des Endpunktes
    INTEGER            , INTENT(IN) :: m_coor2(:)       ! 
    !! Gitterkoordinaten (h-Punkte) der Randabschnitte  <BR>
    !! n-Koordinate des Endpunktes
    INTEGER            , INTENT(IN) :: n_coor2(:)       ! 
    !! Reflektionskoeffizient f&uuml;r den Randabschnitt
    REAL (KIND=Double) , INTENT(IN) :: refl_coef(:)     ! 
    !! Vertikalprofil auf R&auml;ndern vom Typ C, Q, T und R
    CHARACTER (LEN=*)  , INTENT(IN) :: prof(:)          ! 
    !! (opt) Dateiname mit Amplituden falls A
    CHARACTER (LEN=*)  , OPTIONAL , INTENT(IN) :: bloc_ampl(:) ! 
    !! (opt )Dateiname mit Phasen falls A
    CHARACTER (LEN=*)  , OPTIONAL , INTENT(IN) :: bloc_phas(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=19) , PARAMETER :: c_upname='setup_p_grid_bnd_w0' ! 
    !! Hilfsvariablen
    INTEGER :: i, nn ! 
    TYPE (t_d3d_openbc) , ALLOCATABLE :: l_bnd(:) ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          nn = MIN( SIZE(name   ), SIZE(bdry_type), SIZE(data_type), SIZE(m_coor1), &
                    SIZE(m_coor2), SIZE(n_coor1  ), SIZE(n_coor2  ) )
          IF ( PRESENT( bloc_ampl ) ) nn = MIN(nn,SIZE(bloc_ampl))
          IF ( PRESENT( bloc_phas ) ) nn = MIN(nn,SIZE(bloc_phas))
          IF ( nn > 0 ) THEN
             ALLOCATE( l_bnd(nn) )
             DO i=1,SIZE(l_bnd)
                l_bnd(i)%name         = REPEAT( ' ', LEN(l_bnd(i)%name)      )
                l_bnd(i)%bdry_type    = REPEAT( ' ', LEN(l_bnd(i)%bdry_type) )
                l_bnd(i)%data_type    = REPEAT( ' ', LEN(l_bnd(i)%data_type) )
                l_bnd(i)%prof         = REPEAT( ' ', LEN(l_bnd(i)%prof)      )
                l_bnd(i)%bloc_ampl    = REPEAT( ' ', LEN(l_bnd(i)%bloc_ampl) )
                l_bnd(i)%bloc_phas    = REPEAT( ' ', LEN(l_bnd(i)%bloc_phas) )
                !
                l_bnd(i)%name         = name(i)(1:MIN(LEN(l_bnd(i)%name),LEN_TRIM(name(i))))
                l_bnd(i)%bdry_type    = bdry_type(i)(1:MIN(LEN(l_bnd(i)%bdry_type),LEN_TRIM(bdry_type(i))))
                l_bnd(i)%data_type    = data_type(i)(1:MIN(LEN(l_bnd(i)%data_type),LEN_TRIM(data_type(i))))
                l_bnd(i)%refl_coef    = refl_coef(i)
                l_bnd(i)%grid_coor(1) = m_coor1(i)
                l_bnd(i)%grid_coor(2) = n_coor1(i)
                l_bnd(i)%grid_coor(3) = m_coor2(i)
                l_bnd(i)%grid_coor(4) = n_coor2(i)
                l_bnd(i)%prof         = prof(i)(1:MIN(LEN(l_bnd(i)%prof),LEN_TRIM(prof(i))))
                l_bnd(i)%bloc_ampl    = bloc_ampl(i)(1:MIN(LEN(l_bnd(i)%bloc_ampl),LEN_TRIM(bloc_ampl(i))))
                l_bnd(i)%bloc_phas    = bloc_phas(i)(1:MIN(LEN(l_bnd(i)%bloc_phas),LEN_TRIM(bloc_phas(i))))
             END DO
             CALL setup_bnd_object ( work_object, l_bnd(:) )
             DEALLOCATE ( l_bnd )
          END IF
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_bnd_w0
  !
  !! Setzen der Komponente "thd(:)" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_thd_w0 ( type, m_coor1, n_coor1, m_coor2, n_coor2 )
    !! Orientierung des d&uuml;nnen D&auml;mme: <BR>
    !! U : liegt auf U-Punkt <BR>
    !! V : liegt auf V-Punkt
    CHARACTER (LEN=*)  , INTENT(IN) :: type(:)          ! 
    !! Gitterkoordinaten (h-Punkte) der D&auml;mme  <BR>
    !! m-Koordinate des Anfangspunktes
    INTEGER            , INTENT(IN) :: m_coor1(:)       ! 
    !! Gitterkoordinaten (h-Punkte) der D&auml;mme  <BR>
    !! n-Koordinate des Anfangspunktes
    INTEGER            , INTENT(IN) :: n_coor1(:)       ! 
    !! Gitterkoordinaten (h-Punkte) der D&auml;mme  <BR>
    !! m-Koordinate des Endpunktes
    INTEGER            , INTENT(IN) :: m_coor2(:)       ! 
    !! Gitterkoordinaten (h-Punkte) der D&auml;mme  <BR>
    !! n-Koordinate des Endpunktes
    INTEGER            , INTENT(IN) :: n_coor2(:)       ! 
    !! Name der Subroutine
    CHARACTER (LEN=19) , PARAMETER :: c_upname='setup_p_grid_thd_w0' ! 
    !! Hilfsvariablen
    INTEGER :: i, nn ! 
    TYPE (t_d3d_thd) , ALLOCATABLE :: l_thd(:) ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          nn = MIN( SIZE(type   ), SIZE(m_coor1), SIZE(m_coor2), SIZE(n_coor1  ), SIZE(n_coor2  ) )
          IF ( nn > 0 ) THEN
             ALLOCATE( l_thd(nn) )
             DO i=1,SIZE(l_thd)
                l_thd(i)%type         = REPEAT( ' ', LEN(l_thd(i)%type)      )
                !
                l_thd(i)%type         = type(i)(1:MIN(LEN(l_thd(i)%type),LEN_TRIM(type(i))))
                l_thd(i)%grid_coor(1) = m_coor1(i)
                l_thd(i)%grid_coor(2) = n_coor1(i)
                l_thd(i)%grid_coor(3) = m_coor2(i)
                l_thd(i)%grid_coor(4) = n_coor2(i)
             END DO
             CALL setup_thd_object ( work_object, l_thd(:) )
             DEALLOCATE ( l_thd )
          END IF
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_thd_w0
  !
  !! Setzen der Komponente "lwl(:)" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_lwl_w0 ( type, m_coor1, n_coor1, m_coor2, n_coor2, &
       myfriction, sill_depth )
    !! Orientierung der lokalen Wehre: <BR>
    !! U : liegt auf U-Punkt <BR>
    !! V : liegt auf V-Punkt
    CHARACTER (LEN=*)  , INTENT(IN) :: type(:)          ! 
    !! Gitterkoordinaten (h-Punkte) der lokalen Wehre  <BR>
    !! m-Koordinate des Anfangspunktes
    INTEGER            , INTENT(IN) :: m_coor1(:)       ! 
    !! Gitterkoordinaten (h-Punkte) der lokalen Wehre  <BR>
    !! n-Koordinate des Anfangspunktes
    INTEGER            , INTENT(IN) :: n_coor1(:)       ! 
    !! Gitterkoordinaten (h-Punkte) der lokalen Wehre  <BR>
    !! m-Koordinate des Endpunktes
    INTEGER            , INTENT(IN) :: m_coor2(:)       ! 
    !! Gitterkoordinaten (h-Punkte) der lokalen Wehre  <BR>
    !! n-Koordinate des Endpunktes
    INTEGER            , INTENT(IN) :: n_coor2(:)       ! 
    !! Reibungsbeiwerte der lokalen Wehre
    REAL (KIND=Double) , INTENT(IN) :: myfriction(:)      ! 
    !! Tiefen der lokalen Wehre
    REAL (KIND=Double) , INTENT(IN) :: sill_depth(:)    ! 
    !! Name der Subroutine
    CHARACTER (LEN=19) , PARAMETER :: c_upname='setup_p_grid_lwl_w0' ! 
    !! Hilfsvariablen
    INTEGER :: i, nn ! 
    TYPE (t_d3d_weir) , ALLOCATABLE :: l_lwl(:) ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          nn = MIN( SIZE(type    ), &
                    SIZE(m_coor1 ), SIZE(m_coor2   ), SIZE(n_coor1  ), SIZE(n_coor2  ), &
                    SIZE(myfriction), SIZE(sill_depth) )
          IF ( nn > 0 ) THEN
             ALLOCATE( l_lwl(nn) )
             DO i=1,SIZE(l_lwl)
                l_lwl(i)%type         = REPEAT( ' ', LEN(l_lwl(i)%type)      )
                l_lwl(i)%frction     = 0.0_Double
                l_lwl(i)%sill_depth   = 0.0_Double
                !
                l_lwl(i)%type         = type(i)(1:MIN(LEN(l_lwl(i)%type),LEN_TRIM(type(i))))
                l_lwl(i)%grid_coor(1) = m_coor1(i)
                l_lwl(i)%grid_coor(2) = n_coor1(i)
                l_lwl(i)%grid_coor(3) = m_coor2(i)
                l_lwl(i)%grid_coor(4) = n_coor2(i)
                l_lwl(i)%frction     = myfriction(i)
                l_lwl(i)%sill_depth   = sill_depth(i)
             END DO
             CALL setup_lwl_object ( work_object, l_lwl(:) )
             DEALLOCATE ( l_lwl )
          END IF
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_lwl_w0
  !
  !! Setzen der Komponente "ext(:)" des (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_ext_w0 ( type, m_coor1, n_coor1, m_coor2, n_coor2, &
       myfriction, sill_depth )
    !! Orientierung der 2D-Wehre: <BR>
    !! U : liegt auf U-Punkt <BR>
    !! V : liegt auf V-Punkt
    CHARACTER (LEN=*)  , INTENT(IN) :: type(:)          ! 
    !! Gitterkoordinaten (h-Punkte) der 2D-Wehre  <BR>
    !! m-Koordinate des Anfangspunktes
    INTEGER            , INTENT(IN) :: m_coor1(:)       ! 
    !! Gitterkoordinaten (h-Punkte) der 2D-Wehre  <BR>
    !! n-Koordinate des Anfangspunktes
    INTEGER            , INTENT(IN) :: n_coor1(:)       ! 
    !! Gitterkoordinaten (h-Punkte) der 2D-Wehre  <BR>
    !! m-Koordinate des Endpunktes
    INTEGER            , INTENT(IN) :: m_coor2(:)       ! 
    !! Gitterkoordinaten (h-Punkte) der 2D-Wehre  <BR>
    !! n-Koordinate des Endpunktes
    INTEGER            , INTENT(IN) :: n_coor2(:)       ! 
    !! Reibungsbeiwerte der lokalen Wehre
    REAL (KIND=Double) , INTENT(IN) :: myfriction(:)      ! 
    !! Tiefen der lokalen Wehre
    REAL (KIND=Double) , INTENT(IN) :: sill_depth(:)    ! 
    !! Name der Subroutine
    CHARACTER (LEN=19) , PARAMETER :: c_upname='setup_p_grid_ext_w0' ! 
    !! Hilfsvariablen
    INTEGER :: i, nn ! 
    TYPE (t_d3d_weir) , ALLOCATABLE :: l_ext(:) ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          nn = MIN( SIZE(type    ), &
                    SIZE(m_coor1 ), SIZE(m_coor2   ), SIZE(n_coor1  ), SIZE(n_coor2  ), &
                    SIZE(myfriction), SIZE(sill_depth) )
          IF ( nn > 0 ) THEN
             ALLOCATE( l_ext(nn) )
             DO i=1,SIZE(l_ext)
                l_ext(i)%type         = REPEAT( ' ', LEN(l_ext(i)%type)      )
                l_ext(i)%frction     = 0.0_Double
                l_ext(i)%sill_depth   = 0.0_Double
                !
                l_ext(i)%type         = type(i)(1:MIN(LEN(l_ext(i)%type),LEN_TRIM(type(i))))
                l_ext(i)%grid_coor(1) = m_coor1(i)
                l_ext(i)%grid_coor(2) = n_coor1(i)
                l_ext(i)%grid_coor(3) = m_coor2(i)
                l_ext(i)%grid_coor(4) = n_coor2(i)
                l_ext(i)%frction     = myfriction(i)
                l_ext(i)%sill_depth   = sill_depth(i)
             END DO
             CALL setup_ext_object ( work_object, l_ext(:) )
             DEALLOCATE ( l_ext )
          END IF
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_ext_w0
  !
  !! Setzen der Feld-Komponente "huu" des (Arbeits-) Objektes mit Vektordaten <BR>
  !! Zugleich wird die Anzahl Gitterkanten gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_huu_w0 ( val )
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "huu"
    REAL (KIND=Double), INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER :: c_upname='setup_h_grid_huu_w0' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_huu_object ( work_object, val(:) )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_huu_w0
  !
  !! Setzen der Feld-Komponente "hvu" des (Arbeits-) Objektes mit Vektordaten <BR>
  !! Zugleich wird die Anzahl Gitterknoten gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_hvu_w0 ( val )
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "hvu"
    REAL (KIND=Double), INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER :: c_upname='setup_h_grid_hvu_w0' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_hvu_object ( work_object, val(:) )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_hvu_w0
  !
  !! Setzen der Feld-Komponente "hwu" des (Arbeits-) Objektes mit Vektordaten <BR>
  !! Zugleich wird die Anzahl Gitterpolygone gesetzt oder,
  !! sofern schon vorhanden, verglichen. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_hwu_w0 ( val )
    !! zu setzender Wert (Vektor) f&uuml;r Komponente "hwu"
    REAL (KIND=Double), INTENT(IN) :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER :: c_upname='setup_h_grid_hwu_w0' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_hwu_object ( work_object, val(:) )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_hwu_w0
  !
  !! Setzen der Feld-Komponente "dwlp" des (Arbeits-) Objektes mit eine Zeichenstring <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_h_grid_dwlp_w0 ( val )
    !! zu setzender Wert (String) f&uuml;r Komponente "dwlp"
    CHARACTER (LEN=*), INTENT(IN) :: val ! 
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER :: c_upname='setup_h_grid_dwlp_w0' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_dwlp_object ( work_object, val )
       END IF
    END IF
    !
  END SUBROUTINE setup_h_grid_dwlp_w0
  !
  !! Allokieren/Initialisieren eines neuen (Package-) Datenobjekts <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_h_grid_0 ( id )
    !! Identifikationsnummer des Datenobjekts (Skalar) <BR>
    !! id = -1 : Allokieren/Initialisieren fehlgeschlagen
    INTEGER, INTENT(INOUT) :: id
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER :: c_upname='new_h_grid_0'
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       CALL new_h_grid_object ( id )
    ELSE
       id = -1
    END IF
    !
  END SUBROUTINE new_h_grid_0
  !
  !! Allokieren/Initialisieren mehrerer neuer (Package-) Datenobjekte <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_h_grid_1 ( id )
    !! Identifikationsnummern des Datenobjekts (Vektor) <BR>
    !! id = -1 : Allokieren/Initialisieren fehlgeschlagen
    INTEGER, INTENT(INOUT) :: id(:)
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER :: c_upname='new_h_grid_1'
    !! Zaehler
    INTEGER :: i
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(id) .OR. any_error( ) ) EXIT
          CALL new_h_grid_0 ( id(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_h_grid_1
  !
  !! De-Allokieren/De-Initialisieren des aktuellen (Arbeits-) Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_h_grid_w ( )
    !! Name der Subroutine
    CHARACTER (LEN=13), PARAMETER :: c_upname='kill_h_grid_w'
    !
    IF ( ok_work_object ( c_upname ) ) THEN
       CALL kill_h_grid_0 ( work_object%id )
    END IF
    !
  END SUBROUTINE kill_h_grid_w
  !
  !! De-Allokieren/De-Initialisieren eines (Package-) Datenobjekts <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_h_grid_0 ( id )
    !! Identifikationsnummer des Datenobjekts (Skalar)
    INTEGER , INTENT(IN) :: id
    !! Name der Subroutine
    CHARACTER (LEN=13), PARAMETER :: c_upname='kill_h_grid_0'
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       CALL kill_h_grid_object ( id )
    END IF
    !
  END SUBROUTINE kill_h_grid_0
  !
  !! De-Allokieren/De-Initialisieren mehrerer (Package-) Datenobjekte <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_h_grid_1 ( id )
    !! Identifikationsnummern der Datenobjekte (Vektor)
    INTEGER , INTENT(IN) :: id(:)
    !! Name der Subroutine
    CHARACTER (LEN=13), PARAMETER :: c_upname='kill_h_grid_1'
    !! Z&auml;hler
    INTEGER                       :: i
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       i = 0
       DO
          i = i + 1
          IF ( i > SIZE(id) .OR. any_error( ) ) EXIT
          CALL kill_h_grid_0 ( id(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_h_grid_1
  !
  !! Pr&uuml;fe ob das aktuelle Arbeits-Objekt ein g&uuml;ltiges Datenobjekt ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_h_grid_w ( )  &
       RESULT( ok )
    !! Testergebnis (Skalar)
    LOGICAL :: ok
    !! Name der Function
    CHARACTER (LEN=11), PARAMETER :: c_upname='ok_h_grid_w'
    !! Hilfsfelder
    REAL (KIND=Double) , POINTER :: p_xc(:,:), p_dx(:), p_dy(:), p_aa(:) ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          NULLIFY(p_xc,p_dx,p_dy,p_aa)
          p_aa => get_h_grid_aa_w1()
          ! ... fuer Untrim-Gitter weitere Daten bereitstellen, damit verschiedene
          !     Tests durchgefuehrt werden koennen
          SELECT CASE(get_h_grid_variant_no(work_object))
          CASE(3,4) ! UNTRIM_VC und UNTRIM_BAW
             p_xc => get_h_grid_xc_w2()
             p_dx => get_h_grid_dx_w1()
             p_dy => get_h_grid_dy_w1()
          END SELECT
          ok = ok_h_grid_object ( work_object )
          ! ok = .true.
          NULLIFY(p_xc,p_dx,p_dy,p_aa)
       ELSE
          ok = .false.
       END IF
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION ok_h_grid_w
  !
  !! Drucke den Inhalt des aktuellen Arbeits-Objektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_w ( )
    !! Name der Function
    CHARACTER (LEN=14), PARAMETER :: c_upname='print_h_grid_w'
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL print_h_grid_object ( work_object )
       END IF
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_h_grid_w
  !
  !! Drucken aller statischen Daten des Packages (ohne Daten der Package-Objekte) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_static_d ( )
    !! Name der Function
    CHARACTER (LEN=21), PARAMETER :: c_upname='print_h_grid_static_d'
    !! Statusvariable
    INTEGER :: stat
    !! Z&auml;hlervariable
    INTEGER :: i
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       !
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) &
            initialised, prn_op, trc_op, prn_lun, trc_lun, n_init, nofobjects, &
            ASSOCIATED( first_list_object ), ASSOCIATED( work_object ), &
            asc_seq_lun, bin_seq_lun, bin_dir_lun
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       !
       DO i=1,c_max_variants
          !
          IF ( any_error( ) ) EXIT
          !
          WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i, &
               c_variants_access(i), c_variants_form(i), &
               c_variants_delim(i),  c_variants_code(i), &
               c_variants_type(i)
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
          !
       END DO
       !
       IF ( no_error( ) ) CALL print_h_grid_all_errors_d ( )
       !
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT( &
    '#-----------------------------------------------------------' ,/ &
    '# aktuelle statische Daten des Packages p_h_grid_ui  ',/ &
    '# ohne der in den Package-Objekten abgelegten Daten          ',/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
    '#       initialised = ',L1,/ &
    '#            prn_op = ',L1,/ &
    '#            trc_op = ',L1,/ &
    '#           prn_lun = ',I5,/ &
    '#           trc_lun = ',I5,/ &
    '#            n_init = ',I5,/ &
    '#        nofobjects = ',I5,' (Anzahl der Package-Objekte)',/ &
    '# first_list_object = ',L1,' (T = associated)',/ &
    '#       work_object = ',L1,' (T = associated)',/ &
    '#       asc_seq_lun = ',I5,/ &
    '#       bin_seq_lun = ',I5,/ &
    '#       bin_dir_lun = ',I5,/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
    '# Ueberblick ueber die implementierten Dateivarianten ',/ &
    '# -- Nr -- -- Access -- --  Form  -- -- Delim -- -- Code -- -- Type ----------')
8001 FORMAT( '#',I6,5X,A10,3X,A11,2X,A10,3X,I5,6X,A )
8002 FORMAT( &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
    '#------------------------------------------------------------')
    !
  END SUBROUTINE print_h_grid_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler des Packages <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_h_grid_all_errors_d ( )
    !! Name der Function
    CHARACTER (LEN=25), PARAMETER :: c_upname='print_h_grid_all_errors_d'
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       IF ( no_error( ) ) CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_h_grid_all_errors_d
  !
  !! ermittle die Anzahl der (Package-) Objekte "t_h_grid" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_h_grid_nofobjects_d ( ) &
       RESULT( val )
    !! Anzahl der vorhandenen Objekte "t_h_grid"
    INTEGER :: val
    !
    val = nofobjects
    !
  END FUNCTION get_h_grid_nofobjects_d
  !
  !! ermittle die Identifikationsnummern der aktuell vorhandenen (Package-) Objekte "t_h_grid" <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_all_id_d ( ) &
       RESULT( id )
    !! Identifikationsnummern der vorhandenen Objekte "t_h_grid" <BR>
    !! falls "not associated" : es sind keine Objekte vorhanden
    INTEGER , POINTER :: id(:)
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='get_h_grid_all_id_d'
    !! Zeiger auf aktuelles Datenobjekt "t_h_grid_list"
    TYPE (t_h_grid_list) , POINTER :: this
    !! Z&auml;hler
    INTEGER :: i
    !! Statusvariable
    INTEGER :: stat
    !
    IF ( nofobjects > 0 ) THEN
       ALLOCATE( id(nofobjects), STAT=stat )
       IF ( stat == 0 ) THEN
          id    = -1
          this => first_list_object
          i     = 0
          DO
             i = i + 1
             IF ( i > nofobjects .OR. .NOT. ASSOCIATED( this ) ) EXIT
             id(i) = this%object%id
             this => this%next
          END DO
          NULLIFY ( this )
       ELSE
          CALL setup_error_act ( all_errors(:), 9001, c_upname, c_modname, stat )
       END IF
    ELSE
       NULLIFY ( id )
    END IF
    !
  END FUNCTION get_h_grid_all_id_d
  !
  !! ermittle die Identifikationsnummer des aktuellen Arbeits-Objektes <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_work_object_id_d ( ) &
       RESULT( id )
    !! Identifikationsnummer des aktuellen Arbeits-Objektes <BR>
    !! id = -1 : es existiert kein "work object"
    INTEGER :: id
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=27), PARAMETER :: c_upname='get_h_grid_work_object_id_d'
    !
    id = -1
    !
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ASSOCIATED( work_object ) ) id = work_object%id
    END IF
    !
  END FUNCTION get_h_grid_work_object_id_d
  !
  !! Holen der Komponente "name" des (Arbeits-) Objektes <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_name_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : Kopie der Komponente "name"
    CHARACTER (LEN=80) :: val
    ! Lokale Parameter und Variablen
    !! Name der Function
    CHARACTER (LEN=18), PARAMETER :: c_upname='get_h_grid_name_w0'
    !
    val = REPEAT( ' ', LEN( val ) )
    val = 'UNDEFINED'
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val = get_name_object ( work_object )
       END IF
    END IF
    !
  END FUNCTION get_h_grid_name_w0
  !
  !! Holen der Komponente "file" des (Arbeits-) Objektes <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_file_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert: Kopie der Komponente "file"
    TYPE (t_file) :: val
    !! Name der Function
    CHARACTER (LEN=18), PARAMETER :: c_upname='get_h_grid_file_w0'
    !
    CALL new_file ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val = get_file_object ( work_object )
       END IF
    END IF
    !
  END FUNCTION get_h_grid_file_w0
  !
  !! Holen der Typ-Bezeichnung des Gitters im aktuellen Arbeitsobjekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_act_variant_type_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert: Typ des Gitters entsprechend "c_variants_type(:)"
    CHARACTER (LEN=LEN(c_variants_type)) :: val ! 
    !! Name der Function
    CHARACTER (LEN=30), PARAMETER :: c_upname='get_h_grid_act_variant_type_w0'
    !! Hilfsvariable
    INTEGER :: ivar ! 
    !
    val = REPEAT( ' ', LEN(val) )
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ivar = get_h_grid_variant_no(work_object)
          IF ( 1 <= ivar .AND. ivar <= SIZE(c_variants_type) ) THEN
             val = c_variants_type(ivar)
          ELSE
             val = 'undefined'
          END IF
       END IF
    END IF
    !
  END FUNCTION get_h_grid_act_variant_type_w0
  !
  !! Hole Pointer auf Komponente "nv" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_nv_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "nv"
    INTEGER , POINTER :: val
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='get_h_grid_nv_w0' 
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_nv_object ( work_object )
       END IF
    END IF
    !
  END FUNCTION get_h_grid_nv_w0
  !
  !! Hole Pointer auf Komponente "ns" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_ns_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "ns"
    INTEGER , POINTER :: val
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='get_h_grid_ns_w0' 
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_ns_object ( work_object )
          IF ( val == 0 ) THEN
             WRITE(*,*) ' ... automatische Neuberechnung "derive_ns"'
             CALL derive_ns ( work_object )
             IF ( no_error( ) ) val => get_ns_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_h_grid_ns_w0
  !
  !! Hole Pointer auf Komponente "nsi" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_nsi_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "nsi"
    INTEGER , POINTER :: val
    !! Name der Function
    CHARACTER (LEN=17), PARAMETER :: c_upname='get_h_grid_nsi_w0' 
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_nsi_object ( work_object )
       END IF
    END IF
    !
  END FUNCTION get_h_grid_nsi_w0
  !
  !! Hole Pointer auf Komponente "nsf" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_nsf_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "nsi"
    INTEGER , POINTER :: val
    !! Name der Function
    CHARACTER (LEN=17), PARAMETER :: c_upname='get_h_grid_nsf_w0' 
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_nsf_object ( work_object )
       END IF
    END IF
    !
  END FUNCTION get_h_grid_nsf_w0
  !
  !! Hole Pointer auf Komponente "ne" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_ne_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "ne"
    INTEGER , POINTER :: val
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='get_h_grid_ne_w0' 
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_ne_object ( work_object )
       END IF
    END IF
    !
  END FUNCTION get_h_grid_ne_w0
  !
  !! Hole Pointer auf Komponente "xy" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_xy_w2 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "xy"
    REAL (KIND=Double) , POINTER :: val(:,:) ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='get_h_grid_xy_w2'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_xy_object ( work_object )
       END IF
    END IF
    !
  END FUNCTION get_h_grid_xy_w2
  !
  !! Hole Pointer auf Komponente "nen" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_nen_w2 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "nen"
    INTEGER , POINTER :: val(:,:) ! 
    !! Name der Function
    CHARACTER (LEN=17), PARAMETER :: c_upname='get_h_grid_nen_w2'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          !LEODEBUG get_nen_object bedeutet Hole Pointer auf "xy" in Objekt "this"
          val => get_nen_object ( work_object )
          IF ( .NOT. ASSOCIATED(val) ) THEN
             CALL derive_nen ( work_object )
             val => get_nen_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_h_grid_nen_w2
  !
  !! Hole Pointer auf Komponente "irand" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_irand_w1 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "irand"
    INTEGER , POINTER :: val(:) ! 
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='get_h_grid_irand_w1'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_irand_object ( work_object )
          IF ( .NOT. ASSOCIATED(val) ) THEN
             CALL derive_irand ( work_object )
             val => get_irand_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_h_grid_irand_w1
  !
  !! Hole Pointer auf Komponente "ks" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_ks_w1 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "ks"
    INTEGER , POINTER :: val(:) ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='get_h_grid_ks_w1'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_ks_object ( work_object )
          IF ( .NOT. ASSOCIATED(val) ) THEN
             CALL derive_ks ( work_object )
             val => get_ks_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_h_grid_ks_w1
  !
  !! Hole Pointer auf Komponente "hv" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_hv_w1 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "hv"
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='get_h_grid_hv_w1'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_hv_object ( work_object )
          IF ( .NOT. ASSOCIATED(val) ) THEN
             WRITE(*,*) ' ... automatische Neuberechnung "derive_hv"'
             CALL derive_hv ( work_object )
             IF ( no_error( ) ) val => get_hv_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_h_grid_hv_w1
  !
  !! Hole Pointer auf Komponente "nrand" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_nrand_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "nrand"
    INTEGER , POINTER :: val ! 
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='get_h_grid_nrand_w0'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_nrand_object ( work_object )
          IF ( .NOT. ASSOCIATED(val) ) THEN
             WRITE(*,*) ' ... automatische Neuberechnung "derive_nrand"'
             CALL derive_nrand ( work_object )
             IF ( no_error( ) ) val => get_nrand_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_h_grid_nrand_w0
  !
  !! Hole Pointer auf Komponente "nptfr" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_nptfr_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "nptfr"
    INTEGER , POINTER :: val ! 
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='get_h_grid_nptfr_w0'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_nptfr_object ( work_object )
       END IF
    END IF
    !
  END FUNCTION get_h_grid_nptfr_w0
  !
  !! Hole Pointer auf Komponente "nptir" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_nptir_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "nptir"
    INTEGER , POINTER :: val ! 
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='get_h_grid_nptir_w0'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_nptir_object ( work_object )
       END IF
    END IF
    !
  END FUNCTION get_h_grid_nptir_w0
  !
  !! Hole Pointer auf Komponente "time" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_time_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "time"
    TYPE (t_datetime) , POINTER :: val ! 
    !! Name der Function
    CHARACTER (LEN=18), PARAMETER :: c_upname='get_h_grid_time_w0'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_time_object ( work_object )
       END IF
    END IF
    !
  END FUNCTION get_h_grid_time_w0
  !
  !! Hole Pointer auf Komponente "nbc" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_nbc_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "nbc"
    INTEGER , POINTER :: val ! 
    !! Name der Function
    CHARACTER (LEN=17), PARAMETER :: c_upname='get_h_grid_nbc_w0'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_nbc_object ( work_object )
       END IF
    END IF
    !
  END FUNCTION get_h_grid_nbc_w0
  !
  !! Hole Pointer auf Komponente "hland" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_hland_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "hland"
    REAL (KIND=Double) , POINTER :: val ! 
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='get_h_grid_hland_w0'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_hland_object ( work_object )
          IF ( .NOT. ASSOCIATED( val ) ) THEN
             CALL setup_h_grid_min_water_depth ( -10000.0_Double )
             IF ( no_error( ) ) val => get_hland_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_h_grid_hland_w0
  !
  !! Hole Pointer auf Komponente "angle" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_angle_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "angle"
    REAL (KIND=Double) , POINTER :: val ! 
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='get_h_grid_angle_w0'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_angle_object ( work_object )
       END IF
    END IF
    !
  END FUNCTION get_h_grid_angle_w0
  !
  !! Hole Pointer auf Komponente "text" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_text_w1 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "text"
    CHARACTER (LEN=80) , POINTER :: val(:) ! 
    !! Name der Function
    CHARACTER (LEN=18), PARAMETER :: c_upname='get_h_grid_text_w1'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_text_object ( work_object )
       END IF
    END IF
    !
  END FUNCTION get_h_grid_text_w1
  !
  !! Hole Pointer auf Komponente "jb" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_jb_w1 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "jb"
    INTEGER , POINTER :: val(:) ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='get_h_grid_jb_w1'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_jb_object ( work_object )
          IF ( .NOT. ASSOCIATED( val ) ) THEN
             WRITE(*,*) ' ... automatische Neuberechnung "derive_jb"'
             CALL derive_jb ( work_object )
             IF ( no_error( ) ) val => get_jb_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_h_grid_jb_w1
  !
  !! Hole Pointer auf Komponente "jt" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_jt_w1 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "jt"
    INTEGER , POINTER :: val(:) ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='get_h_grid_jt_w1'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_jt_object ( work_object )
          IF ( .NOT. ASSOCIATED( val ) ) THEN
             WRITE(*,*) ' ... automatische Neuberechnung "derive_jt"'
             CALL derive_jt ( work_object )
             IF ( no_error( ) ) val => get_jt_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_h_grid_jt_w1
  !
  !! Hole Pointer auf Komponente "is" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_is_w2 ( ) &
       RESULT ( val )
    !
    USE b_error, ONLY : DEBUG_b
    !! Ergebniswert : Pointer auf Komponente "is"
    INTEGER , POINTER :: val(:,:) ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='get_h_grid_is_w2'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_is_object ( work_object )
          IF ( .NOT. ASSOCIATED( val ) ) THEN
             IF (DEBUG_b > 0) THEN
                WRITE(*,*) ' ... automatische Neuberechnung "derive_is"'
             END IF
             CALL derive_is ( work_object )
             IF ( no_error( ) ) val => get_is_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_h_grid_is_w2
  !
  !! Hole Pointer auf Komponente "je" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_je_w2 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "je"
    INTEGER , POINTER :: val(:,:) ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='get_h_grid_je_w2'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_je_object ( work_object )
          IF ( .NOT. ASSOCIATED( val ) ) THEN
             WRITE(*,*) ' ... automatische Neuberechnung "derive_je"'
             CALL derive_je ( work_object )
             IF ( no_error( ) ) val => get_je_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_h_grid_je_w2
  !
  !! Hole Pointer auf Komponente "ie" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_ie_w2 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "ie"
    INTEGER , POINTER :: val(:,:) ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='get_h_grid_ie_w2'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_ie_object ( work_object )
          IF ( .NOT. ASSOCIATED( val ) ) THEN
             WRITE(*,*) ' ... automatische Neuberechnung "derive_ie"'
             CALL derive_ie ( work_object )
             IF ( no_error( ) ) val => get_ie_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_h_grid_ie_w2
  !
  !! Hole Pointer auf Komponente "xs" f&uuml;r (Arbeits-) Objekt <BR>
  !! Falls die Daten noch nicht vorhanden sind so werden sie automatisch
  !! aus anderen schon bekannten Gr&ouml;szlig;en abgeleitet <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_xs_w2 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "xs"
    REAL (KIND=Double) , POINTER :: val(:,:)
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='get_h_grid_xs_w2'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_xs_object ( work_object )
          IF ( .NOT. ASSOCIATED( val ) ) THEN
             WRITE(*,*) ' ... automatische Neuberechnung "derive_xs"'
             CALL derive_xs ( work_object )
             IF ( no_error( ) ) val => get_xs_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_h_grid_xs_w2
  !
  !! Hole Pointer auf Komponente "xc" f&uuml;r (Arbeits-) Objekt <BR>
  !! Falls die Daten noch nicht vorhanden sind so werden sie automatisch
  !! aus anderen schon bekannten Gr&ouml;szlig;en abgeleitet <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_xc_w2 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "xc"
    REAL (KIND=Double) , POINTER :: val(:,:) ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='get_h_grid_xc_w2'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_xc_object ( work_object )
          IF ( .NOT. ASSOCIATED( val ) ) THEN
             WRITE(*,*) ' ... automatische Neuberechnung "derive_xc"'
             CALL derive_xc ( work_object )
             IF ( no_error( ) ) val => get_xc_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_h_grid_xc_w2
  !
  !! Hole Pointer auf Komponente "xg" f&uuml;r (Arbeits-) Objekt <BR>
  !! Falls die Daten noch nicht vorhanden sind so werden sie automatisch
  !! aus anderen schon bekannten Gr&ouml;szlig;en abgeleitet <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_xg_w2 ( ) &
       RESULT ( val )
    !
    USE b_error, ONLY : DEBUG_b
    !! Ergebniswert : Pointer auf Komponente "xg"
    REAL (KIND=Double) , POINTER :: val(:,:) ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='get_h_grid_xg_w2'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_xg_object ( work_object )
          IF ( .NOT. ASSOCIATED( val ) ) THEN
             IF (DEBUG_b > 0) THEN
                WRITE(*,*) ' ... automatische Neuberechnung "derive_xg"'
             END IF
             CALL derive_xg ( work_object )
             IF ( no_error( ) ) val => get_xg_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_h_grid_xg_w2
  !
  !! Hole Pointer auf Komponente "dg" f&uuml;r (Arbeits-) Objekt <BR>
  !! Falls die Daten noch nicht vorhanden sind so werden sie automatisch
  !! aus anderen schon bekannten Gr&ouml;szlig;en abgeleitet <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_dg_w1 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "dg"
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='get_h_grid_dg_w1'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_dg_object ( work_object )
          IF ( .NOT. ASSOCIATED( val ) ) THEN
             WRITE(*,*) ' ... automatische Neuberechnung "derive_dg"'
             CALL derive_dg ( work_object )
             IF ( no_error( ) ) val => get_dg_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_h_grid_dg_w1
  !
  !! Hole Pointer auf Komponente "dx" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_dx_w1 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "dx"
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='get_h_grid_dx_w1'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_dx_object ( work_object )
          IF ( .NOT. ASSOCIATED( val ) ) THEN
             WRITE(*,*) ' ... automatische Neuberechnung "derive_dx"'
             CALL derive_dx ( work_object )
             IF ( no_error( ) ) val => get_dx_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_h_grid_dx_w1
  !
  !! Hole Pointer auf Komponente "dy" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_dy_w1 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "dy"
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='get_h_grid_dy_w1'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_dy_object ( work_object )
          IF ( .NOT. ASSOCIATED( val ) ) THEN
             WRITE(*,*) ' ... automatische Neuberechnung "derive_dy"'
             CALL derive_dy ( work_object )
             IF ( no_error( ) ) val => get_dy_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_h_grid_dy_w1
  !
  !! Hole Pointer auf Komponente "aa" f&uuml;r (Arbeits-) Objekt <BR>
  !! Falls die Daten noch nicht vorhanden sind so werden sie automatisch
  !! aus anderen schon bekannten Gr&ouml;szlig;en abgeleitet <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_aa_w1 ( ) &
       RESULT ( val )
    !
    USE b_error, ONLY : DEBUG_b
    !! Ergebniswert : Pointer auf Komponente "aa"
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='get_h_grid_aa_w1'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_aa_object ( work_object )
          IF ( .NOT. ASSOCIATED( val ) ) THEN
             IF (DEBUG_b > 0) THEN
                WRITE(*,*) ' ... automatische Neuberechnung "derive_aa"'
             END IF
             CALL derive_aa ( work_object )
             IF ( no_error( ) ) val => get_aa_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_h_grid_aa_w1
  !
  !! Hole Pointer auf Komponente "hu" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_hu_w1 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "hu"
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='get_h_grid_hu_w1'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_hu_object ( work_object )
          IF ( .NOT. ASSOCIATED( val ) ) THEN
             WRITE(*,*) ' ... automatische Neuberechnung "derive_hu"'
             CALL derive_hu ( work_object )
             IF ( no_error( ) ) val => get_hu_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_h_grid_hu_w1
  !
  !! Hole Pointer auf Komponente "hw" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_hw_w1 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "hw"
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='get_h_grid_hw_w1'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_hw_object ( work_object )
          IF ( .NOT. ASSOCIATED( val ) ) THEN
             WRITE(*,*) ' ... automatische Neuberechnung "derive_hw"'
             CALL derive_hw ( work_object )
             IF ( no_error( ) ) val => get_hw_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_h_grid_hw_w1
  !
  !! Hole Pointer auf Komponente "ipobo" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_ipobo_w1 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "ipobo"
    INTEGER , POINTER :: val(:) ! 
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='get_h_grid_ipobo_w1'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_ipobo_object ( work_object )
       END IF
    END IF
    !
  END FUNCTION get_h_grid_ipobo_w1
  !
  !! Hole Pointer auf Komponente "dxmin" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_dxmin_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "dxmin"
    REAL (KIND=Double) , POINTER :: val ! 
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='get_h_grid_dxmin_w0'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_dxmin_object ( work_object )
          IF ( .NOT. ASSOCIATED( val ) ) THEN
             CALL setup_h_grid_min_center_dist ( 0.0_Double )
             IF ( no_error( ) ) val => get_dxmin_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_h_grid_dxmin_w0
  !
  !! Hole Pointer auf Komponente "nr" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_nr_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "nr"
    INTEGER , POINTER :: val ! 
    !! Name der Function
    CHARACTER (LEN=16), PARAMETER :: c_upname='get_h_grid_nr_w0'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_nr_object ( work_object )
       END IF
    END IF
    !
  END FUNCTION get_h_grid_nr_w0
  !
  !! Hole Pointer auf Komponente "ncsize" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_ncsize_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "ncsize"
    INTEGER , POINTER :: val ! 
    !! Name der Function
    CHARACTER (LEN=20), PARAMETER :: c_upname='get_h_grid_ncsize_w0'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_ncsize_object ( work_object )
       END IF
    END IF
    !
  END FUNCTION get_h_grid_ncsize_w0
  !
  !! Hole Pointer auf Komponente "m" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_m_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "m"
    INTEGER , POINTER :: val
    !! Name der Function
    CHARACTER (LEN=15), PARAMETER :: c_upname='get_h_grid_m_w0' 
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_m_object ( work_object )
       END IF
    END IF
    !
  END FUNCTION get_h_grid_m_w0
  !
  !! Hole Pointer auf Komponente "n" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_n_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "n"
    INTEGER , POINTER :: val
    !! Name der Function
    CHARACTER (LEN=15), PARAMETER :: c_upname='get_h_grid_n_w0' 
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_n_object ( work_object )
       END IF
    END IF
    !
  END FUNCTION get_h_grid_n_w0
  !
  !! Hole Pointer auf Komponente "enc(:,:)" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_enc_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "enc(:,:)"
    INTEGER , POINTER :: val(:,:) ! 
    !! Name der Function
    CHARACTER (LEN=17), PARAMETER :: c_upname='get_h_grid_enc_w0' 
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_enc_object ( work_object )
       END IF
    END IF
    !
  END FUNCTION get_h_grid_enc_w0
  !
  !! Hole Pointer auf Komponente "dry(:,:)" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_dry_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "dry(:,:)"
    INTEGER , POINTER :: val(:,:) ! 
    !! Name der Function
    CHARACTER (LEN=17), PARAMETER :: c_upname='get_h_grid_dry_w0' 
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_dry_object ( work_object )
       END IF
    END IF
    !
  END FUNCTION get_h_grid_dry_w0
  !
  !! Hole Pointer auf Komponente "bnd(:)%name" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
!LEO  FUNCTION get_h_grid_bnd_name_w0 ( ) &
!LEO       RESULT ( val )
!LEO    !! Ergebniswert : Pointer auf Komponente "bnd(:)%name"
!LEO    CHARACTER (LEN=c_len_d3d_openbc_name) , POINTER :: val(:) ! 
!LEO    !! Name der Function
!LEO    CHARACTER (LEN=22), PARAMETER :: c_upname='get_h_grid_bnd_name_w0' 
!LEO    !! Hilfsvariable
!LEO    TYPE (t_d3d_openbc) , POINTER :: p_bnd(:) ! 
!LEO    !
!LEO    NULLIFY ( val )
!LEO    IF ( ok_initialised ( c_upname ) ) THEN
!LEO       IF ( ok_work_object ( c_upname ) ) THEN
!LEO          p_bnd => get_bnd_object ( work_object )
!LEO          IF ( ASSOCIATED( p_bnd ) ) THEN
!LEO             val => p_bnd%name
!LEO          END IF
!LEO       END IF
!LEO    END IF
!LEO    !
!LEO  END FUNCTION get_h_grid_bnd_name_w0
  !
  !! Hole Pointer auf Komponente "bnd(:)%bdry_type" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
!LEO  FUNCTION get_h_grid_bnd_bdry_type_w0 ( ) &
!LEO       RESULT ( val )
!LEO    !! Ergebniswert : Pointer auf Komponente "bnd(:)%bdry_type"
!LEO    CHARACTER (LEN=c_len_d3d_openbc_type) , POINTER :: val(:) ! 
!LEO    !! Name der Function
!LEO    CHARACTER (LEN=27), PARAMETER :: c_upname='get_h_grid_bnd_bdry_type_w0' 
!LEO    !! Hilfsvariable
!LEO    TYPE (t_d3d_openbc) , POINTER :: p_bnd(:) ! 
!LEO    !
!LEO    NULLIFY ( val )
!LEO    IF ( ok_initialised ( c_upname ) ) THEN
!LEO       IF ( ok_work_object ( c_upname ) ) THEN
!LEO          p_bnd => get_bnd_object ( work_object )
!LEO          IF ( ASSOCIATED( p_bnd ) ) THEN
!LEO             val => p_bnd%bdry_type
!LEO          END IF
!LEO       END IF
!LEO    END IF
!LEO    !
!LEO  END FUNCTION get_h_grid_bnd_bdry_type_w0
  !
  !! Hole Pointer auf Komponente "bnd(:)%data_type" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
!LEO  FUNCTION get_h_grid_bnd_data_type_w0 ( ) &
!LEO       RESULT ( val )
!LEO    !! Ergebniswert : Pointer auf Komponente "bnd(:)%data_type"
!LEO    CHARACTER (LEN=c_len_d3d_openbc_type) , POINTER :: val(:) ! 
!LEO    !! Name der Function
!LEO    CHARACTER (LEN=27), PARAMETER :: c_upname='get_h_grid_bnd_data_type_w0' 
!LEO    !! Hilfsvariable
!LEO    TYPE (t_d3d_openbc) , POINTER :: p_bnd(:) ! 
!LEO    !
!LEO    NULLIFY ( val )
!LEO    IF ( ok_initialised ( c_upname ) ) THEN
!LEO       IF ( ok_work_object ( c_upname ) ) THEN
!LEO          p_bnd => get_bnd_object ( work_object )
!LEO          IF ( ASSOCIATED( p_bnd ) ) THEN
!LEO             val => p_bnd%data_type
!LEO          END IF
!LEO       END IF
!LEO    END IF
!LEO    !
!LEO  END FUNCTION get_h_grid_bnd_data_type_w0
  !
  !! Hole Pointer auf Komponente "bnd(:)%refl_coef" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
!LEO  FUNCTION get_h_grid_bnd_refl_coef_w0 ( ) &
!LEO       RESULT ( val )
!LEO    !! Ergebniswert : Pointer auf Komponente "bnd(:)%refl_coef"
!LEO    REAL (KIND=Double)  , POINTER :: val(:) ! 
!LEO    !! Name der Function
!LEO    CHARACTER (LEN=27), PARAMETER :: c_upname='get_h_grid_bnd_refl_coef_w0' 
!LEO    !! Hilfsvariable
!LEO    TYPE (t_d3d_openbc) , POINTER :: p_bnd(:) ! 
!LEO    !
!LEO    NULLIFY ( val )
!LEO    IF ( ok_initialised ( c_upname ) ) THEN
!LEO       IF ( ok_work_object ( c_upname ) ) THEN
!LEO          p_bnd => get_bnd_object ( work_object )
!LEO          IF ( ASSOCIATED( p_bnd ) ) THEN
!LEO             val => p_bnd%refl_coef
!LEO          END IF
!LEO       END IF
!LEO    END IF
!LEO    !
!LEO  END FUNCTION get_h_grid_bnd_refl_coef_w0
  !
  !! Hole Pointer auf Komponente "bnd(:)%prof" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
!LEO   FUNCTION get_h_grid_bnd_prof_w0 ( ) &
!LEO        RESULT ( val )
!LEO     !! Ergebniswert : Pointer auf Komponente "bnd(:)%prof"
!LEO     CHARACTER (LEN=c_len_d3d_openbc_prof) , POINTER :: val(:) ! 
!LEO     !! Name der Function
!LEO     CHARACTER (LEN=22), PARAMETER :: c_upname='get_h_grid_bnd_prof_w0' 
!LEO    !! Hilfsvariable
!LEO    TYPE (t_d3d_openbc) , POINTER :: p_bnd(:) ! 
!LEO    !
!LEO    NULLIFY ( val )
!LEO    IF ( ok_initialised ( c_upname ) ) THEN
!LEO       IF ( ok_work_object ( c_upname ) ) THEN
!LEO          p_bnd => get_bnd_object ( work_object )
!LEO          IF ( ASSOCIATED( p_bnd ) ) THEN
!LEO             val => p_bnd%prof
!LEO          END IF
!LEO       END IF
!LEO    END IF
!LEO    !
!LEO  END FUNCTION get_h_grid_bnd_prof_w0
  !
  !! Hole Pointer auf Komponente "bnd(:)%bloc_ampl" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
!LEO  FUNCTION get_h_grid_bnd_bloc_ampl_w0 ( ) &
!LEO       RESULT ( val )
!LEO    !! Ergebniswert : Pointer auf Komponente "bnd(:)%bloc_ampl"
!LEO    CHARACTER (LEN=c_len_d3d_openbc_bloc) , POINTER :: val(:) ! 
!LEO    !! Name der Function
!LEO    CHARACTER (LEN=27), PARAMETER :: c_upname='get_h_grid_bnd_bloc_ampl_w0' 
!LEO    !! Hilfsvariable
!LEO    TYPE (t_d3d_openbc) , POINTER :: p_bnd(:) ! 
!LEO    !
!LEO    NULLIFY ( val )
!LEO    IF ( ok_initialised ( c_upname ) ) THEN
!LEO       IF ( ok_work_object ( c_upname ) ) THEN
!LEO          p_bnd => get_bnd_object ( work_object )
!LEO          IF ( ASSOCIATED( p_bnd ) ) THEN
!LEO             val => p_bnd%bloc_ampl
!LEO          END IF
!LEO       END IF
!LEO    END IF
!LEO    !
!LEO  END FUNCTION get_h_grid_bnd_bloc_ampl_w0
  !
  !! Hole Pointer auf Komponente "bnd(:)%bloc_phas" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
!LEO  FUNCTION get_h_grid_bnd_bloc_phas_w0 ( ) &
!LEO       RESULT ( val )
!LEO    !! Ergebniswert : Pointer auf Komponente "bnd(:)%bloc_phas"
!LEO    CHARACTER (LEN=c_len_d3d_openbc_bloc) , POINTER :: val(:) ! 
!LEO    !! Name der Function
!LEO    CHARACTER (LEN=27), PARAMETER :: c_upname='get_h_grid_bnd_bloc_phas_w0' 
!LEO    !! Hilfsvariable
!LEO    TYPE (t_d3d_openbc) , POINTER :: p_bnd(:) ! 
!LEO    !
!LEO    NULLIFY ( val )
!LEO    IF ( ok_initialised ( c_upname ) ) THEN
!LEO       IF ( ok_work_object ( c_upname ) ) THEN
!LEO          p_bnd => get_bnd_object ( work_object )
!LEO          IF ( ASSOCIATED( p_bnd ) ) THEN
!LEO             val => p_bnd%bloc_phas
!LEO          END IF
!LEO       END IF
!LEO    END IF
!LEO    !
!LEO  END FUNCTION get_h_grid_bnd_bloc_phas_w0
  !
  !! Ermittle die maximale Anzahl der in der Komponente "bnd(:)%grid_coor(:)" 
  !! des aktuellen (Arbeits-) Objektes abgelegten Informationen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_bnd_max_grid_coor_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : max. Anzahl der Eintr&auml;ge in "bnd(:)%grid_coor(:)"
    INTEGER :: val ! 
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_h_grid_bnd_max_grid_coor_w0' ! 
    !
    val = 0
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val = c_max_d3d_openbc_coor
       END IF
    END IF
    !
  END FUNCTION get_h_grid_bnd_max_grid_coor_w0
  !
  !! Hole Pointer auf Komponente "bnd(:)%grid_coor(1|2|3|4)" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
!LEO  FUNCTION get_h_grid_bnd_grid_coor_w0 ( no ) &
!LEO       RESULT ( val )
!LEO    !! Positionsnummer f&uuml;r "bnd(:)%grid_coor(no)"
!LEO    INTEGER , INTENT(IN) :: no     ! 
!LEO    !! Ergebniswert : Pointer auf Komponente "bnd(:)%grid_coor(ipos)"
!LEO    INTEGER , POINTER    :: val(:) ! 
!LEO    !! Name der Function
!LEO    CHARACTER (LEN=27), PARAMETER :: c_upname='get_h_grid_bnd_grid_coor_w0' 
!LEO    !! Hilfsvariable
!LEO    TYPE (t_d3d_openbc) , POINTER :: p_bnd(:) ! 
!LEO    !
!LEO    NULLIFY ( val )
!LEO    IF ( ok_initialised ( c_upname ) ) THEN
!LEO       IF ( ok_work_object ( c_upname ) ) THEN
!LEO          IF ( no >= 1 .AND. no <= c_max_d3d_openbc_coor ) THEN
!LEO             p_bnd => get_bnd_object ( work_object )
!LEO             IF ( ASSOCIATED( p_bnd ) ) THEN
!LEO                val => p_bnd%grid_coor(no)
!LEO             END IF
!LEO          END IF
!LEO       END IF
!LEO    END IF
!LEO    !
!LEO  END FUNCTION get_h_grid_bnd_grid_coor_w0
  !
  !! Hole Pointer auf Komponente "thd(:)%name" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
!LEO  FUNCTION get_h_grid_thd_type_w0 ( ) &
!LEO       RESULT ( val )
!LEO    !! Ergebniswert : Pointer auf Komponente "thd(:)%type"
!LEO    CHARACTER (LEN=c_len_d3d_uv_type) , POINTER :: val(:) ! 
!LEO    !! Name der Function
!LEO    CHARACTER (LEN=22), PARAMETER :: c_upname='get_h_grid_thd_type_w0' !  
!LEO    !! Hilfsvariable
!LEO    TYPE (t_d3d_thd) , POINTER :: p_thd(:) ! 
!LEO    !
!LEO    NULLIFY ( val )
!LEO    IF ( ok_initialised ( c_upname ) ) THEN
!LEO       IF ( ok_work_object ( c_upname ) ) THEN
!LEO          p_thd => get_thd_object ( work_object )
!LEO          IF ( ASSOCIATED( p_thd ) ) THEN
!LEO             val => p_thd%type
!LEO          END IF
!LEO       END IF
!LEO    END IF
!LEO    !
!LEO  END FUNCTION get_h_grid_thd_type_w0
  !
  !! Ermittle die maximale Anzahl der in der Komponente "thd(:)%grid_coor(:)" 
  !! des aktuellen (Arbeits-) Objektes abgelegten Informationen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_thd_max_grid_coor_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : max. Anzahl der Eintr&auml;ge in "thd(:)%grid_coor(:)"
    INTEGER :: val ! 
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_h_grid_thd_max_grid_coor_w0' ! 
    !
    val = 0
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val = c_max_d3d_uv
       END IF
    END IF
    !
  END FUNCTION get_h_grid_thd_max_grid_coor_w0
  !
  !! Hole Pointer auf Komponente "thd(:)%grid_coor(1|2|3|4)" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
!LEO  FUNCTION get_h_grid_thd_grid_coor_w0 ( no ) &
!LEO       RESULT ( val )
!LEO    !! Positionsnummer f&uuml;r "thd(:)%grid_coor(no)"
!LEO    INTEGER , INTENT(IN) :: no     ! 
!LEO    !! Ergebniswert : Pointer auf Komponente "thd(:)%grid_coor(ipos)"
!LEO    INTEGER , POINTER    :: val(:) ! 
!LEO    !! Name der Function
!LEO    CHARACTER (LEN=27), PARAMETER :: c_upname='get_h_grid_thd_grid_coor_w0' 
!LEO    !! Hilfsvariable
!LEO    TYPE (t_d3d_thd) , POINTER :: p_thd(:) ! 
!LEO    !
!LEO    NULLIFY ( val )
!LEO    IF ( ok_initialised ( c_upname ) ) THEN
!LEO       IF ( ok_work_object ( c_upname ) ) THEN
!LEO          IF ( no >= 1 .AND. no <= c_max_d3d_uv ) THEN
!LEO             p_thd => get_thd_object ( work_object )
!LEO             IF ( ASSOCIATED( p_thd ) ) THEN
!LEO                val => p_thd%grid_coor(no)
!LEO             END IF
!LEO          END IF
!LEO       END IF
!LEO    END IF
!LEO    !
!LEO  END FUNCTION get_h_grid_thd_grid_coor_w0
  !
  !! Hole Pointer auf Komponente "lwl(:)%name" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
!LEO  FUNCTION get_h_grid_lwl_type_w0 ( ) &
!LEO       RESULT ( val )
!LEO    !! Ergebniswert : Pointer auf Komponente "lwl(:)%type"
!LEO    CHARACTER (LEN=c_len_d3d_uv_type) , POINTER :: val(:) ! 
!LEO    !! Name der Function
!LEO    CHARACTER (LEN=22), PARAMETER :: c_upname='get_h_grid_lwl_type_w0' !  
!LEO    !! Hilfsvariable
!LEO    TYPE (t_d3d_weir) , POINTER :: p_lwl(:) ! 
!LEO    !
!LEO    NULLIFY ( val )
!LEO    IF ( ok_initialised ( c_upname ) ) THEN
!LEO       IF ( ok_work_object ( c_upname ) ) THEN
!LEO          p_lwl => get_lwl_object ( work_object )
!LEO          IF ( ASSOCIATED( p_lwl ) ) THEN
!LEO             val => p_lwl%type
!LEO          END IF
!LEO       END IF
!LEO    END IF
!LEO    !
!LEO  END FUNCTION get_h_grid_lwl_type_w0
  !
  !! Ermittle die maximale Anzahl der in der Komponente "lwl(:)%grid_coor(:)" 
  !! des aktuellen (Arbeits-) Objektes abgelegten Informationen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_lwl_max_grid_coor_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : max. Anzahl der Eintr&auml;ge in "lwl(:)%grid_coor(:)"
    INTEGER :: val ! 
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_h_grid_lwl_max_grid_coor_w0' ! 
    !
    val = 0
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val = c_max_d3d_uv
       END IF
    END IF
    !
  END FUNCTION get_h_grid_lwl_max_grid_coor_w0
  !
  !! Hole Pointer auf Komponente "lwl(:)%grid_coor(1|2|3|4)" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
!LEO  FUNCTION get_h_grid_lwl_grid_coor_w0 ( no ) &
!LEO       RESULT ( val )
!LEO    !! Positionsnummer f&uuml;r "lwl(:)%grid_coor(no)"
!LEO    INTEGER , INTENT(IN) :: no     ! 
!LEO    !! Ergebniswert : Pointer auf Komponente "lwl(:)%grid_coor(ipos)"
!LEO    INTEGER , POINTER    :: val(:) ! 
!LEO    !! Name der Function
!LEO    CHARACTER (LEN=27), PARAMETER :: c_upname='get_h_grid_lwl_grid_coor_w0' 
!LEO    !! Hilfsvariable
!LEO    TYPE (t_d3d_weir) , POINTER :: p_lwl(:) ! 
!LEO    !
!LEO    NULLIFY ( val )
!LEO    IF ( ok_initialised ( c_upname ) ) THEN
!LEO       IF ( ok_work_object ( c_upname ) ) THEN
!LEO          IF ( no >= 1 .AND. no <= c_max_d3d_uv ) THEN
!LEO             p_lwl => get_lwl_object ( work_object )
!LEO             IF ( ASSOCIATED( p_lwl ) ) THEN
!LEO                val => p_lwl%grid_coor(no)
!LEO             END IF
!LEO          END IF
!LEO       END IF
!LEO    END IF
!LEO    !
!LEO  END FUNCTION get_h_grid_lwl_grid_coor_w0
  !
  !! Hole Pointer auf Komponente "lwl(:)%friction" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
!LEO  FUNCTION get_h_grid_lwl_friction_w0 ( ) &
!LEO       RESULT ( val )
!LEO    !! Ergebniswert : Pointer auf Komponente "lwl(:)%friction"
!LEO    REAL (KIND=Double) , POINTER :: val(:) ! 
!LEO    !! Name der Function
!LEO    CHARACTER (LEN=26) , PARAMETER :: c_upname='get_h_grid_lwl_friction_w0' !  
!LEO    !! Hilfsvariable
!LEO    TYPE (t_d3d_weir)  , POINTER :: p_lwl(:) ! 
!LEO    !
!LEO    NULLIFY ( val )
!LEO    IF ( ok_initialised ( c_upname ) ) THEN
!LEO       IF ( ok_work_object ( c_upname ) ) THEN
!LEO          p_lwl => get_lwl_object ( work_object )
!LEO          IF ( ASSOCIATED( p_lwl ) ) THEN
!LEO             val => p_lwl%friction
!LEO          END IF
!LEO       END IF
!LEO    END IF
!LEO    !
!LEO  END FUNCTION get_h_grid_lwl_friction_w0
  !
  !! Hole Pointer auf Komponente "lwl(:)%sill_depth" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
!LEO  FUNCTION get_h_grid_lwl_sill_depth_w0 ( ) &
!LEO       RESULT ( val )
!LEO    !! Ergebniswert : Pointer auf Komponente "lwl(:)%sill_depth"
!LEO    REAL (KIND=Double) , POINTER :: val(:) ! 
!LEO    !! Name der Function
!LEO    CHARACTER (LEN=28) , PARAMETER :: c_upname='get_h_grid_lwl_sill_depth_w0' !  
!LEO    !! Hilfsvariable
!LEO    TYPE (t_d3d_weir)  , POINTER :: p_lwl(:) ! 
!LEO    !
!LEO    NULLIFY ( val )
!LEO    IF ( ok_initialised ( c_upname ) ) THEN
!LEO       IF ( ok_work_object ( c_upname ) ) THEN
!LEO          p_lwl => get_lwl_object ( work_object )
!LEO          IF ( ASSOCIATED( p_lwl ) ) THEN
!LEO             val => p_lwl%sill_depth
!LEO          END IF
!LEO       END IF
!LEO    END IF
!LEO    !
!LEO  END FUNCTION get_h_grid_lwl_sill_depth_w0
  !
  !! Hole Pointer auf Komponente "ext(:)%name" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
!LEO  FUNCTION get_h_grid_ext_type_w0 ( ) &
!LEO       RESULT ( val )
!LEO    !! Ergebniswert : Pointer auf Komponente "ext(:)%type"
!LEO    CHARACTER (LEN=c_len_d3d_uv_type) , POINTER :: val(:) ! 
!LEO    !! Name der Function
!LEO    CHARACTER (LEN=22), PARAMETER :: c_upname='get_h_grid_ext_type_w0' !  
!LEO    !! Hilfsvariable
!LEO    TYPE (t_d3d_weir) , POINTER :: p_ext(:) ! 
!LEO    !
!LEO    NULLIFY ( val )
!LEO    IF ( ok_initialised ( c_upname ) ) THEN
!LEO       IF ( ok_work_object ( c_upname ) ) THEN
!LEO          p_ext => get_ext_object ( work_object )
!LEO          IF ( ASSOCIATED( p_ext ) ) THEN
!LEO             val => p_ext%type
!LEO          END IF
!LEO       END IF
!LEO    END IF
!LEO    !
!LEO  END FUNCTION get_h_grid_ext_type_w0
  !
  !! Ermittle die maximale Anzahl der in der Komponente "ext(:)%grid_coor(:)" 
  !! des aktuellen (Arbeits-) Objektes abgelegten Informationen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_ext_max_grid_coor_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : max. Anzahl der Eintr&auml;ge in "ext(:)%grid_coor(:)"
    INTEGER :: val ! 
    !! Name der Function
    CHARACTER (LEN=31), PARAMETER :: c_upname='get_h_grid_ext_max_grid_coor_w0' ! 
    !
    val = 0
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val = c_max_d3d_uv
       END IF
    END IF
    !
  END FUNCTION get_h_grid_ext_max_grid_coor_w0
  !
  !! Hole Pointer auf Komponente "ext(:)%grid_coor(1|2|3|4)" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
!LEO  FUNCTION get_h_grid_ext_grid_coor_w0 ( no ) &
!LEO       RESULT ( val )
!LEO    !! Positionsnummer f&uuml;r "ext(:)%grid_coor(no)"
!LEO    INTEGER , INTENT(IN) :: no     ! 
!LEO    !! Ergebniswert : Pointer auf Komponente "ext(:)%grid_coor(ipos)"
!LEO    INTEGER , POINTER    :: val(:) ! 
!LEO    !! Name der Function
!LEO    CHARACTER (LEN=27), PARAMETER :: c_upname='get_h_grid_ext_grid_coor_w0' 
!LEO    !! Hilfsvariable
!LEO    TYPE (t_d3d_weir) , POINTER :: p_ext(:) ! 
!LEO    !
!LEO    NULLIFY ( val )
!LEO    IF ( ok_initialised ( c_upname ) ) THEN
!LEO       IF ( ok_work_object ( c_upname ) ) THEN
!LEO          IF ( no >= 1 .AND. no <= c_max_d3d_uv ) THEN
!LEO             p_ext => get_ext_object ( work_object )
!LEO             IF ( ASSOCIATED( p_ext ) ) THEN
!LEO                val => p_ext%grid_coor(no)
!LEO             END IF
!LEO          END IF
!LEO       END IF
!LEO    END IF
!LEO    !
!LEO  END FUNCTION get_h_grid_ext_grid_coor_w0
  !
  !! Hole Pointer auf Komponente "ext(:)%friction" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
!LEO  FUNCTION get_h_grid_ext_friction_w0 ( ) &
!LEO       RESULT ( val )
!LEO    !! Ergebniswert : Pointer auf Komponente "ext(:)%friction"
!LEO    REAL (KIND=Double) , POINTER :: val(:) ! 
!LEO    !! Name der Function
!LEO    CHARACTER (LEN=26) , PARAMETER :: c_upname='get_h_grid_ext_friction_w0' !  
!LEO    !! Hilfsvariable
!LEO    TYPE (t_d3d_weir)  , POINTER :: p_ext(:) ! 
!LEO    !
!LEO    NULLIFY ( val )
!LEO    IF ( ok_initialised ( c_upname ) ) THEN
!LEO       IF ( ok_work_object ( c_upname ) ) THEN
!LEO          p_ext => get_ext_object ( work_object )
!LEO          IF ( ASSOCIATED( p_ext ) ) THEN
!LEO             val => p_ext%friction
!LEO          END IF
!LEO       END IF
!LEO    END IF
!LEO    !
!LEO  END FUNCTION get_h_grid_ext_friction_w0
!LEO  !
  !! Hole Pointer auf Komponente "ext(:)%sill_depth" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
!LEO  FUNCTION get_h_grid_ext_sill_depth_w0 ( ) &
!LEO       RESULT ( val )
!LEO    !! Ergebniswert : Pointer auf Komponente "ext(:)%sill_depth"
!LEO    REAL (KIND=Double) , POINTER :: val(:) ! 
!LEO    !! Name der Function
!LEO    CHARACTER (LEN=28) , PARAMETER :: c_upname='get_h_grid_ext_sill_depth_w0' !  
!LEO    !! Hilfsvariable
!LEO    TYPE (t_d3d_weir)  , POINTER :: p_ext(:) ! 
!LEO    !
!LEO    NULLIFY ( val )
!LEO    IF ( ok_initialised ( c_upname ) ) THEN
!LEO       IF ( ok_work_object ( c_upname ) ) THEN
!LEO          p_ext => get_ext_object ( work_object )
!LEO          IF ( ASSOCIATED( p_ext ) ) THEN
!LEO             val => p_ext%sill_depth
!LEO          END IF
!LEO       END IF
!LEO    END IF
!LEO    !
!LEO  END FUNCTION get_h_grid_ext_sill_depth_w0
  !
  !! Hole Pointer auf Komponente "huu" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_huu_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "huu"
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !! Name der Function
    CHARACTER (LEN=17), PARAMETER :: c_upname='get_h_grid_huu_w0'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_huu_object ( work_object )
          IF ( .NOT. ASSOCIATED( val ) ) THEN
             WRITE(*,*) ' ... automatische Neuberechnung "derive_huu"'
             CALL derive_huu ( work_object )
             IF ( no_error( ) ) val => get_huu_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_h_grid_huu_w0
  !
  !! Hole Pointer auf Komponente "hvu" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_hvu_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "hvu"
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !! Name der Function
    CHARACTER (LEN=17), PARAMETER :: c_upname='get_h_grid_hvu_w0'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_hvu_object ( work_object )
          IF ( .NOT. ASSOCIATED( val ) ) THEN
             WRITE(*,*) ' ... automatische Neuberechnung "derive_hvu"'
             CALL derive_hvu ( work_object )
             IF ( no_error( ) ) val => get_hvu_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_h_grid_hvu_w0
  !
  !! Hole Pointer auf Komponente "hwu" f&uuml;r (Arbeits-) Objekt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_h_grid_hwu_w0 ( ) &
       RESULT ( val )
    !! Ergebniswert : Pointer auf Komponente "hwu"
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !! Name der Function
    CHARACTER (LEN=17), PARAMETER :: c_upname='get_h_grid_hwu_w0'
    !
    NULLIFY ( val )
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          val => get_hwu_object ( work_object )
          IF ( .NOT. ASSOCIATED( val ) ) THEN
             WRITE(*,*) ' ... automatische Neuberechnung "derive_hwu"'
             CALL derive_hwu ( work_object )
             IF ( no_error( ) ) val => get_hwu_object ( work_object )
          END IF
       END IF
    END IF
    !
  END FUNCTION get_h_grid_hwu_w0
  !
  !! Ermittle den aktuellen <EM>maximalen</EM> Wert f&uuml;r eine klassischen
  !! Fortran-Dimension (gem&auml;&szlig; Name) in allen definierten Datenobjekten 
  !! alle Werte sind garantiert gr&ouml;&szlig;er als Null) <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_max_h_grid_olddim_0 ( name ) &
       RESULT( res )
    !! klassische Fortran-Dimensionsbezeichnung                                    <BR>
    !! MAXELE  : max. Anzahl der Dreieckselemente                                  <BR>
    !!           (Vierecke werden als zwei Dreiecke gez&auml;hlt)                  <BR>
    !! MAXKNO  : max. Anzahl der Knoten des Gitters                                <BR>
    !! MAXINF  : max. Anzahl der Knoten/Kanten im Dreieck                          <BR>
    !! NINNEN  : max. Anzahl der Innenknoten eines Gitters (soweit verf&uuml;gbar) <BR>
    !! NRAND   : max. Anzahl der Randknoten eines Gitters (soweit verf&uuml;gbar)        
    CHARACTER (LEN=*) , INTENT(IN) :: name ! 
    !! Ergebniswert: maximaler Wert der Dimension in allen Objekten      <BR>
    !! i.d.R wird ein Wert gr&ouml;&szlig;er als Null zur&uuml;ckgegeben <BR>
    !! falls -9, so konnte der Wert nicht ermittelt werden
    INTEGER              :: res   ! 
    !! Name der Funktion
    CHARACTER (LEN=23) , PARAMETER :: c_upname='get_max_h_grid_olddim_0' ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       res = get_max_old_dimensions ( name )
    ELSE
       res = 1
    END IF
    !
  END FUNCTION get_max_h_grid_olddim_0
  !
  !! Ermittle die aktuellen <EM>maximalen</EM> Werte f&uuml;r mehrere klassische
  !! Fortran-Dimensionen (gem&auml;&szlig; Namensliste) in allen definierten Datenobjekten <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_max_h_grid_olddim_1 ( name ) &
       RESULT( res )
    !! klassische Fortran-Dimensionsbezeichnung                                    <BR>
    !! MAXELE  : max. Anzahl der Dreieckselemente                                  <BR>
    !!           (Vierecke werden als zwei Dreiecke gez&auml;hlt)                  <BR>
    !! MAXKNO  : max. Anzahl der Knoten des Gitters                                <BR>
    !! MAXINF  : max. Anzahl der Knoten/Kanten im Dreieck                          <BR>
    !! NINNEN  : max. Anzahl der Innenknoten eines Gitters (soweit verf&uuml;gbar) <BR>
    !! NRAND   : max. Anzahl der Randknoten eines Gitters (soweit verf&uuml;gbar)        
    CHARACTER (LEN=*) , INTENT(IN) :: name(:) ! 
    !! Ergebniswert: maximale Werte der Dimensionen in allen Objekten <BR>
    !! -9 == nicht definiert
    INTEGER              :: res(SIZE(name))   ! 
    !! Name der Funktion
    CHARACTER (LEN=23) , PARAMETER :: c_upname='get_max_h_grid_olddim_1' ! 
    !
    res(:) = -9
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       res(:) = get_max_old_dimensions ( name(:) )
    END IF
    !
  END FUNCTION get_max_h_grid_olddim_1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "nv" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_nv_w0 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "nv"
    INTEGER , POINTER :: val
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='target_h_grid_nv_w0'
    !
    ok = .false. 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_nv_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_nv_w0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "ns" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_ns_w0 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "ns"
    INTEGER , POINTER :: val
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='target_h_grid_ns_w0'
    !
    ok = .false. 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_ns_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_ns_w0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "nsi" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_nsi_w0 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "nsi"
    INTEGER , POINTER :: val
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok
    !! Name der Function
    CHARACTER (LEN=20), PARAMETER :: c_upname='target_h_grid_nsi_w0'
    !
    ok = .false. 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_nsi_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_nsi_w0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "nsf" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_nsf_w0 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "nsf"
    INTEGER , POINTER :: val
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok
    !! Name der Function
    CHARACTER (LEN=20), PARAMETER :: c_upname='target_h_grid_nsf_w0'
    !
    ok = .false. 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_nsf_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_nsf_w0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "ne" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_ne_w0 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "ne"
    INTEGER , POINTER :: val
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='target_h_grid_ne_w0'
    !
    ok = .false. 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_ne_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_ne_w0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "xy" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_xy_w2 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "xy"
    REAL (KIND=Double) , POINTER :: val(:,:) ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='target_h_grid_xy_w2'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_xy_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_xy_w2
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "nen" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_nen_w2 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "nen"
    INTEGER , POINTER :: val(:,:) ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok
    !! Name der Function
    CHARACTER (LEN=20), PARAMETER :: c_upname='target_h_grid_nen_w2'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_nen_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_nen_w2
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "irand" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_irand_w1 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "irand"
    INTEGER , POINTER :: val(:) ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok
    !! Name der Function
    CHARACTER (LEN=22), PARAMETER :: c_upname='target_h_grid_irand_w1'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_irand_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_irand_w1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "ks" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_ks_w1 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "ks"
    INTEGER , POINTER :: val(:) ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='target_h_grid_ks_w1'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_ks_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_ks_w1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "hv" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_hv_w1 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "hv"
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='target_h_grid_hv_w1'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_hv_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_hv_w1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "nrand" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_nrand_w0 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "nrand"
    INTEGER , POINTER :: val ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok
    !! Name der Function
    CHARACTER (LEN=22), PARAMETER :: c_upname='target_h_grid_nrand_w0'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_nrand_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_nrand_w0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "nptfr" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_nptfr_w0 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "nptfr"
    INTEGER , POINTER :: val ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok
    !! Name der Function
    CHARACTER (LEN=22), PARAMETER :: c_upname='target_h_grid_nptfr_w0'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_nptfr_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_nptfr_w0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "nptir" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_nptir_w0 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "nptir"
    INTEGER , POINTER :: val ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok
    !! Name der Function
    CHARACTER (LEN=22), PARAMETER :: c_upname='target_h_grid_nptir_w0'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_nptir_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_nptir_w0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "time" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_time_w0 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "time"
    TYPE (t_datetime), POINTER :: val ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok
    !! Name der Function
    CHARACTER (LEN=21), PARAMETER :: c_upname='target_h_grid_time_w0'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_time_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_time_w0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "nbc" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_nbc_w0 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "nbc"
    INTEGER , POINTER :: val ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok
    !! Name der Function
    CHARACTER (LEN=20), PARAMETER :: c_upname='target_h_grid_nbc_w0'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_nbc_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_nbc_w0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "hland" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_hland_w0 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "hland"
    REAL (KIND=Double) , POINTER :: val ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=22), PARAMETER :: c_upname='target_h_grid_hland_w0'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_hland_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_hland_w0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "angle" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_angle_w0 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "angle"
    REAL (KIND=Double) , POINTER :: val ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=22), PARAMETER :: c_upname='target_h_grid_angle_w0'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_angle_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_angle_w0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "text" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_text_w1 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "text"
    CHARACTER (LEN=80) , POINTER :: val(:) ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=21), PARAMETER :: c_upname='target_h_grid_text_w1'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_text_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_text_w1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "jb" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_jb_w1 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "jb"
    INTEGER , POINTER :: val(:) ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='target_h_grid_jb_w1'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_jb_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_jb_w1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "jt" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_jt_w1 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "jt"
    INTEGER , POINTER :: val(:) ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='target_h_grid_jt_w1'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_jt_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_jt_w1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "is" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_is_w2 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "is"
    INTEGER , POINTER :: val(:,:) ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='target_h_grid_is_w2'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_is_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_is_w2
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "je" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_je_w2 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "je"
    INTEGER , POINTER :: val(:,:) ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='target_h_grid_je_w2'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_je_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_je_w2
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "ie" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_ie_w2 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "ie"
    INTEGER , POINTER :: val(:,:) ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='target_h_grid_ie_w2'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_ie_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_ie_w2
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "xs" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_xs_w2 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "xs"
    REAL (KIND=Double) , POINTER :: val(:,:)
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='target_h_grid_xs_w2'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_xs_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_xs_w2
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "xc" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_xc_w2 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "xc"
    REAL (KIND=Double) , POINTER :: val(:,:) ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='target_h_grid_xc_w2'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_xc_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_xc_w2
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "xg" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_xg_w2 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "xg"
    REAL (KIND=Double) , POINTER :: val(:,:) ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='target_h_grid_xg_w2'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_xg_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_xg_w2
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "dg" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_dg_w1 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "dg"
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='target_h_grid_dg_w1'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_dg_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_dg_w1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "dx" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_dx_w1 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "dx"
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='target_h_grid_dx_w1'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_dx_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_dx_w1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "dy" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_dy_w1 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "dy"
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='target_h_grid_dy_w1'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_dy_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_dy_w1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "aa" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_aa_w1 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "aa"
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='target_h_grid_aa_w1'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_aa_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_aa_w1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "hu" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_hu_w1 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "hu"
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='target_h_grid_hu_w1'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_hu_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_hu_w1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "hw" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_hw_w1 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "hw"
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='target_h_grid_hw_w1'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_hw_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_hw_w1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "ipobo" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_ipobo_w1 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "ipobo"
    INTEGER , POINTER :: val(:) ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=22), PARAMETER :: c_upname='target_h_grid_ipobo_w1'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_ipobo_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_ipobo_w1
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "dxmin" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_dxmin_w0 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "dxmin"
    REAL (KIND=Double), POINTER :: val ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=22), PARAMETER :: c_upname='target_h_grid_dxmin_w0'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_dxmin_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_dxmin_w0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "nr" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_nr_w0 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "nr"
    INTEGER , POINTER :: val ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok
    !! Name der Function
    CHARACTER (LEN=19), PARAMETER :: c_upname='target_h_grid_nr_w0'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_nr_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_nr_w0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "ncsize" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_ncsize_w0 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "ncsize"
    INTEGER , POINTER :: val ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok
    !! Name der Function
    CHARACTER (LEN=23), PARAMETER :: c_upname='target_h_grid_ncsize_w0'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_ncsize_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_ncsize_w0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "m" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_m_w0 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "m"
    INTEGER, POINTER :: val ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=18), PARAMETER :: c_upname='target_h_grid_m_w0'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_m_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_m_w0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "n" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_n_w0 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "n"
    INTEGER, POINTER :: val ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=18), PARAMETER :: c_upname='target_h_grid_n_w0'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_n_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_n_w0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "enc(:,:)" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_enc_w0 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "enc(:,:)"
    INTEGER, POINTER :: val(:,:) ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=20), PARAMETER :: c_upname='target_h_grid_enc_w0'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_enc_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_enc_w0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "dry(:,:)" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_dry_w0 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "dry(:,:)"
    INTEGER, POINTER :: val(:,:) ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=20), PARAMETER :: c_upname='target_h_grid_dry_w0'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_dry_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_dry_w0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "huu" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_huu_w0 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "huu"
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=20), PARAMETER :: c_upname='target_h_grid_huu_w0'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_huu_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_huu_w0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "hvu" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_hvu_w0 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "hvu"
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=20), PARAMETER :: c_upname='target_h_grid_hvu_w0'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_hvu_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_hvu_w0
  !
  !! Pr&uuml;fe ob Pointer f&uuml;r "hwu" mit (Arbeits-) Object &uuml;bereinstimmen <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION target_h_grid_hwu_w0 ( val ) &
       RESULT ( ok )
    !! Zeiger auf Datenobjekt vom Typ der Komponente "hwu"
    REAL (KIND=Double) , POINTER :: val(:) ! 
    !! Ergebniswert : Pr&uuml;fergebnis true/false
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=20), PARAMETER :: c_upname='target_h_grid_hwu_w0'
    !
    ok = .false.
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          ok = target_hwu_object ( work_object, val )
       END IF
    END IF
    !
  END FUNCTION target_h_grid_hwu_w0
  !
  !! &Uuml;betragen der in einer Datei stehenden Daten in das (Arbeits-) Objekt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_h_grid_w ( )
    !! Name der Subroutine
    CHARACTER (LEN=13), PARAMETER  :: c_upname='read_h_grid_w'
    !
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL read_h_grid_0 ( work_object )
       END IF
    END IF
    !
  END SUBROUTINE read_h_grid_w
  !
  !! &Uuml;betragen der in einer Datei stehenden Daten in das (Arbeits-) Objekt <BR>
  !! Es wird die in der Parameterliste stehende Datei verwendet <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_h_grid_wf ( file, ncsize )
    !! Bezeichnung der Datei aus der Daten gelesen werden sollen
    TYPE (t_file) , INTENT(IN) :: file
    !! optional: Anzahl an CPUs (beim parallelen Rechnen mit TELEMAC)
    INTEGER, INTENT(IN), OPTIONAL :: ncsize
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='read_h_grid_wf'
    !
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_file_object ( work_object, file )
          !
          IF (PRESENT(ncsize)) THEN
             CALL read_h_grid_0 ( work_object, ncsize )
          ELSE
             CALL read_h_grid_0 ( work_object )
          ENDIF
       END IF
    END IF
    !
  END SUBROUTINE read_h_grid_wf
  !
  !! &Uuml;betragen der in dem (Arbeits-) Objekt stehenden Daten in Datei <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE write_h_grid_w ( )
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER  :: c_upname='write_h_grid_w'
    !
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL write_h_grid_0 ( work_object )
       END IF
    END IF
    !
  END SUBROUTINE write_h_grid_w
  !
  !! &Uuml;betragen der Daten des (Arbeits-) Objekts in eine Datei <BR>
  !! Es wird die in der Parameterliste stehende Datei verwendet <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE write_h_grid_wf ( file )
    !! Bezeichnung der Datei aus der Daten gelesen werden sollen
    TYPE (t_file) , INTENT(IN) :: file
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER  :: c_upname='write_h_grid_wf'
    !
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_file_object ( work_object, file )
          CALL write_h_grid_0 ( work_object )
       END IF
    END IF
    !
  END SUBROUTINE write_h_grid_wf
  !
  !! Holen der Anzahl der implementierten Datei-Varianten <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldung
  FUNCTION get_h_grid_nof_variants_d ( ) &
       RESULT( var )
    !! Ergebniswert: Anzahl der implementierten Datei-Varianten
    INTEGER :: var
    !
    var = c_max_variants
    !
  END FUNCTION get_h_grid_nof_variants_d
  !
  !! Holen des Datei-Types aller implementierten Datei-Varianten <BR>
  !! Function erzeugt Fehlermeldung
  FUNCTION get_h_grid_variants_type_d ( ) &
       RESULT( var )
    !! Ergebniswert: Datei-Types aller implementierten Datei-Varianten
    CHARACTER (LEN=LEN(c_variants_type)) , POINTER :: var(:) ! 
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER  :: c_upname='get_h_grid_variants_type_d'
    !! Statusvariable
    INTEGER :: stat
    !
    ALLOCATE ( var(c_max_variants), STAT=stat )
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 9002, c_upname, c_modname, stat )
       NULLIFY ( var )
    ELSE
       var(:) = c_variants_type(:)
    END IF
    !
  END FUNCTION get_h_grid_variants_type_d
  !
  !! Holen des Datei-Types einer implementierten Datei-Variante <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldung
  FUNCTION get_h_grid_variants_type_0 ( ivar ) &
       RESULT( var )
    ! Formalparameter
    !! Nummer einer implementierten Variante
    INTEGER , INTENT(IN)                 :: ivar
    !! Ergebniswert: Datei-Type der Implementation "ivar"
    CHARACTER (LEN=LEN(c_variants_type)) :: var
    !
    IF ( ivar > 0 .AND. ivar <= c_max_variants ) THEN
       var = c_variants_type(ivar)
    ELSE
       var = REPEAT( '-', LEN(var) )
    END IF
    !
  END FUNCTION get_h_grid_variants_type_0
  !
  !! Holen des Datei-Codes aller implementierten Datei-Varianten <BR>
  !! Function erzeugt Fehlermeldung
  FUNCTION get_h_grid_variants_code_d ( ) &
       RESULT( var )
    !! Ergebniswert: Datei-Codes aller implementierten Datei-Varianten
    INTEGER , POINTER :: var(:)
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER  :: c_upname='get_h_grid_variants_code_d'
    !! Statusvariable
    INTEGER :: stat
    !
    ALLOCATE ( var(c_max_variants), STAT=stat )
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 9003, c_upname, c_modname, stat )
       NULLIFY ( var )
    ELSE
       var(:) = c_variants_code(:)
    END IF
    !
  END FUNCTION get_h_grid_variants_code_d
  !
  !! Holen des Datei-Codes einer implementierten Datei-Variante <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldung
  FUNCTION get_h_grid_variants_code_0 ( ivar ) &
       RESULT( var )
    ! Formalparameter
    !! Nummer einer implementierten Variante
    INTEGER , INTENT(IN)                 :: ivar
    !! Ergebniswert: Datei-Code der Implementation "ivar"
    CHARACTER (LEN=LEN(c_variants_form)) :: var
    !
    IF ( ivar > 0 .AND. ivar <= c_max_variants ) THEN
       var = c_variants_form(ivar)
    ELSE
       var = REPEAT( '-', LEN(var) )
    END IF
    !
  END FUNCTION get_h_grid_variants_code_0
  !
  !! Holen der Fortran-Form aller implementierten Datei-Varianten <BR>
  !! Function erzeugt Fehlermeldung
  FUNCTION get_h_grid_variants_form_d ( ) &
       RESULT( var )
    !! Ergebniswert: Fortran-Formen aller implementierten Datei-Varianten
    CHARACTER (LEN=LEN(c_variants_form)) , POINTER :: var(:)
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER  :: c_upname='get_h_grid_variants_form_d'
    !! Statusvariable
    INTEGER :: stat
    !
    ALLOCATE ( var(c_max_variants), STAT=stat )
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 9004, c_upname, c_modname, stat )
       NULLIFY ( var )
    ELSE
       var(:) = c_variants_form(:)
    END IF
    !
  END FUNCTION get_h_grid_variants_form_d
  !
  !! Holen der Fortran-Form einer implementierten Datei-Variante <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldung
  FUNCTION get_h_grid_variants_form_0 ( ivar ) &
       RESULT( var )
    ! Formalparameter
    !! Nummer einer implementierten Variante
    INTEGER , INTENT(IN)                 :: ivar
    !! Ergebniswert: Fortran-Form der Implementation "ivar"
    CHARACTER (LEN=LEN(c_variants_form)) :: var
    !
    IF ( ivar > 0 .AND. ivar <= c_max_variants ) THEN
       var = c_variants_form(ivar)
    ELSE
       var = REPEAT( '-', LEN(var) )
    END IF
    !
  END FUNCTION get_h_grid_variants_form_0
  !
  !! Holen des Fortran-Access aller implementierten Datei-Varianten <BR>
  !! Function erzeugt Fehlermeldung
  FUNCTION get_h_grid_variants_access_d ( ) &
       RESULT( var )
    !! Ergebniswert: Fortran-Access aller implementierten Datei-Varianten
    CHARACTER (LEN=LEN(c_variants_access)) , POINTER :: var(:)
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=28), PARAMETER  :: c_upname='get_h_grid_variants_access_d'
    !! Statusvariable
    INTEGER :: stat
    !
    ALLOCATE ( var(c_max_variants), STAT=stat )
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 9005, c_upname, c_modname, stat )
       NULLIFY ( var )
    ELSE
       var(:) = c_variants_access(:)
    END IF
    !
  END FUNCTION get_h_grid_variants_access_d
  !
  !! Holen des Fortran-Access einer implementierten Datei-Variante <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldung
  FUNCTION get_h_grid_variants_access_0 ( ivar ) &
       RESULT( var )
    ! Formalparameter
    !! Nummer einer implementierten Variante
    INTEGER , INTENT(IN)                   :: ivar
    !! Ergebniswert: Fortran-Access der Implementation "ivar"
    CHARACTER (LEN=LEN(c_variants_access)) :: var
    !
    IF ( ivar > 0 .AND. ivar <= c_max_variants ) THEN
       var = c_variants_access(ivar)
    ELSE
       var = REPEAT( '-', LEN(var) )
    END IF
    !
  END FUNCTION get_h_grid_variants_access_0
  !
  !! Holen des Fortran-Delimiters aller implementierten Datei-Varianten <BR>
  !! Function erzeugt Fehlermeldung
  FUNCTION get_h_grid_variants_delim_d ( ) &
       RESULT( var )
    !! Ergebniswert: Fortran-Delimiter aller implementierten Datei-Varianten
    CHARACTER (LEN=LEN(c_variants_delim)) , POINTER :: var(:)
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=27), PARAMETER  :: c_upname='get_h_grid_variants_delim_d'
    !! Statusvariable
    INTEGER :: stat
    !
    ALLOCATE ( var(c_max_variants), STAT=stat )
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 9006, c_upname, c_modname, stat )
       NULLIFY ( var )
    ELSE
       var(:) = c_variants_delim(:)
    END IF
    !
  END FUNCTION get_h_grid_variants_delim_d
  !
  !! Holen des Fortran-Delimiters einer implementierten Datei-Variante <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldung
  FUNCTION get_h_grid_variants_delim_0 ( ivar ) &
       RESULT( var )
    ! Formalparameter
    !! Nummer einer implementierten Variante
    INTEGER , INTENT(IN)                  :: ivar
    !! Ergebniswert: Fortran-Delimiter der Implementation "ivar"
    CHARACTER (LEN=LEN(c_variants_delim)) :: var
    !
    IF ( ivar > 0 .AND. ivar <= c_max_variants ) THEN
       var = c_variants_delim(ivar)
    ELSE
       var = REPEAT( '-', LEN(var) )
    END IF
    !
  END FUNCTION get_h_grid_variants_delim_0
  !
  ! ----------------------------------------------------------------------
  ! PUBLIC-CONVERT-Methoden [Fehlermeldungen 26000 - 26999 ]
  ! ----------------------------------------------------------------------
  !
  !! wandle das Arbeitsobjekt in ein Gitter im GITTER05-Format um
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE convert_h_grid_to_gitter05_w0 ( )
    !! Name der Subroutine
    CHARACTER (LEN=29), PARAMETER  :: c_upname='convert_h_grid_to_gitter05_w0'
    !
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL convert_to_gitter05 ( work_object )
       END IF
    END IF
    !
  END SUBROUTINE convert_h_grid_to_gitter05_w0
  !
  !! wandle das Arbeitsobjekt in ein Gitter im UNTRIM-Format um
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE convert_h_grid_to_untrim_w0 ( )
    !! Name der Subroutine
    CHARACTER (LEN=28), PARAMETER  :: c_upname='convert_h_grid_to_untrim_w0'
    !
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL convert_to_untrim ( work_object )
       END IF
    END IF
    !
  END SUBROUTINE convert_h_grid_to_untrim_w0
  !
  !! f&uuml;hre eine Red-Black-Sortierung f&uuml;r das (Arbeits-) Objekt aus <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE convert_h_grid_rb_w0 ( )
    !! Name der Subroutine
    CHARACTER (LEN=20), PARAMETER  :: c_upname='convert_h_grid_rb_w0'
    !
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL convert_untrim_red_black ( work_object )
       END IF
    END IF
    !
  END SUBROUTINE convert_h_grid_rb_w0
  !
  !! sortiere Innen- und Ausssenkanten f&uuml;r das (Arbeits-) Objekt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE convert_h_grid_nsi_w0 ( )
    !! Name der Subroutine
    CHARACTER (LEN=21), PARAMETER  :: c_upname='convert_h_grid_nsi_w0'
    !
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL convert_untrim_nsi ( work_object )
       END IF
    END IF
    !
  END SUBROUTINE convert_h_grid_nsi_w0
  !
  !! eliminiere tote Volumina f&uuml;r das (Arbeits-) Objekt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE convert_h_grid_terrace_w0 ( )
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER  :: c_upname='convert_h_grid_terrace_w0'
    !
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL convert_untrim_terrace ( work_object )
       END IF
    END IF
    !
  END SUBROUTINE convert_h_grid_terrace_w0
  !
  !! Gitternetzverfeinerung f&uuml;r das (Arbeits-) Objekt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE refine_h_grid_w0 ( )
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER  :: c_upname='refine_h_grid_w0'
    !
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL refine ( work_object )
       END IF
    END IF
    !
  END SUBROUTINE refine_h_grid_w0
  !
  !! Vertiefen der Bathymetrie f&uuml;r das (Arbeits-) Objekt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE deepen_h_grid_w0 ( dz )
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER  :: c_upname='deepen_h_grid_w0'
    !! Vertiefungsbetrag (positiv nach Unten)
    REAL (KIND=Double) , INTENT(IN) :: dz ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL deepen ( work_object, dz )
       END IF
    END IF
    !
  END SUBROUTINE deepen_h_grid_w0
  !
  !! Erzeuge alle Austauschgr&ouml;&szlig;en mit OpenMI-konformen Daten 
  !! f&uuml;r eine Liste von Arbeitsobjekten <BR>
  !! Funktion erzeugt Fehlermeldungen <BR>
  !! es wird ein Zeiger auf die internen Daten zur&uuml;ckgegeben
  FUNCTION get_h_grid_exch_r_1 ( id ) &
       RESULT( res )
    !! Liste mit Id's der Datenobjekte des Typs "t_h_grid" f&uuml;r die
    !! alle OpenMI-konformen Austauschgr&ouml;&szlig;en erzeugt werden
    !! sollen
    INTEGER , INTENT(IN)          :: id(:)  ! 
    !! Ergebnis: Liste mit allen OpenMI-konformen Austauschgr&ouml;&szlig;en
    TYPE (t_omi_exch_r) , POINTER :: res(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER  :: c_upname='get_h_grid_exch_r_1' ! 
    !! Hilfsvariablen
    INTEGER :: i ! 
    TYPE (t_omi_exch) , POINTER   :: p_exch(:) ! 
    !
    NULLIFY ( p_exch )
    ALLOCATE( res(SIZE(id)) )
    CALL new_omi_exch_r ( res(:) )
    IF ( ok_initialised( c_upname ) ) THEN
       ! bedarfsweises Erzeugen der OpenMI-konformen Informationen
       ! ... und Transfer in das "ragged array" "res(:)"
       DO i=1,SIZE(id)
          CALL setup_h_grid_work_object_d ( id(i) )
          IF ( any_error( ) ) EXIT
          IF ( ok_work_object ( c_upname ) ) THEN
             p_exch => get_exch_object ( work_object )
             IF ( .NOT. ASSOCIATED( p_exch ) ) THEN
                CALL create_omi ( work_object )
             END IF
             p_exch => get_exch_object ( work_object )
             IF ( ASSOCIATED( p_exch ) ) THEN
                CALL set_omi_exch_r_odx      ( res(i), id(i) )
                CALL set_omi_exch_r_exch_ref ( res(i), p_exch )
             END IF
          END IF
       END DO
       ! [2.1] Aufsammeln der OpenMI-konformen Informationen in dem Feld "res(:)"
       NULLIFY ( p_exch )
    END IF
    !
  END FUNCTION get_h_grid_exch_r_1
  !
  !! Erzeuge die Informationen zum Zeithorizont in OpenMI-konformer Weise
  !! f&uuml;r eine Liste von Arbeitsobjekten <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION get_h_grid_time_horizon_1 ( id ) &
       RESULT( res )
    !! Liste mit Id's der Datenobjekte des Typs "t_h_grid" f&uuml;r die
    !! alle OpenMI-konformen Austauschgr&ouml;&szlig;en erzeugt werden
    !! sollen
    INTEGER , INTENT(IN)  :: id(:) ! 
    !! Ergebnis: Zeithorizont der Gitterobjekte
    TYPE (t_omi_span) , POINTER :: res   ! 
    !! Name der Subroutine
    CHARACTER (LEN=25), PARAMETER  :: c_upname='get_h_grid_time_horizon_1' ! 
    !! Hilfsvariablen
    INTEGER :: i  ! 
    REAL (KIND=Double) :: l_start, l_end ! 
    LOGICAL :: ok ! 
    TYPE (t_omi_stamp) , POINTER :: p_stamp
    !
    ok = .false. 
    l_start = 1.0E+30_Double ; l_end = -1.0E+30_Double
    IF ( ok_initialised( c_upname ) ) THEN
       DO i=1,SIZE(id)
          CALL setup_h_grid_work_object_d ( id(i) )
          IF ( any_error( ) ) EXIT
          IF ( ok_work_object ( c_upname ) ) THEN
             p_stamp => get_stamp_object ( work_object )
             IF ( ASSOCIATED( p_stamp ) ) THEN
                ok = .true.
                l_start = MIN( l_start, get_omi_stamp_modjulianday( p_stamp ) )
                l_end   = MAX( l_end  , get_omi_stamp_modjulianday( p_stamp ) )
             END IF
          END IF
       END DO
       !
       IF ( ok ) THEN
          ALLOCATE ( res )
          CALL new_omi_span ( res )
          CALL set_omi_span_start ( res, l_start )
          CALL set_omi_span_end   ( res, l_end   )
       ELSE
          NULLIFY( res )
       END IF
       NULLIFY ( p_stamp )
    END IF
    !
  END FUNCTION get_h_grid_time_horizon_1
  !
  !! Ermittle, ob diskrete Zeitangaben f&uuml;r das aktuelle Arbeitsobjekt und 
  !! ein bestimmtes Austauschobjekt in OpenMI-konformer Weise vorhanden sind <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION get_h_grid_has_discr_time_w ( exch_idx ) &
       RESULT( res )
    !! Zeiger auf die Position der Austauschgr&ouml;&szlig;e in dem
    !! aktuellen Arbeitsobjekt
    INTEGER , INTENT(IN)  :: exch_idx ! 
    !! Ergebnis: diskrete Zeitangaben sind vorhanden / nicht vorhanden
    LOGICAL :: res   ! 
    !! Name der Subroutine
    CHARACTER (LEN=27), PARAMETER :: c_upname='get_h_grid_has_discr_time_w' ! 
    !! Hilfsvariablen
    TYPE (t_omi_stamp) , POINTER :: p_stamp
    !
    NULLIFY ( p_stamp )
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          p_stamp => get_stamp_object ( work_object )
       END IF
    END IF
    res = ( ASSOCIATED( p_stamp ) )
    NULLIFY( p_stamp )
    !
  END FUNCTION get_h_grid_has_discr_time_w
  !
  !! Ermittle, wie viele diskrete Zeitangaben f&uuml;r das aktuelle Arbeitsobjekt 
  !! und ein bestimmtes Austauschobjekt in OpenMI-konformer Weise vorhanden sind <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION get_h_grid_nof_discr_time_w ( exch_idx ) &
       RESULT( res )
    !! Zeiger auf die Position der Austauschgr&ouml;&szlig;e in dem
    !! aktuellen Arbeitsobjekt
    INTEGER , INTENT(IN)  :: exch_idx ! 
    !! Ergebnis: Anzahl diskreter Zeitangaben 
    INTEGER :: res   ! 
    !! Name der Subroutine
    CHARACTER (LEN=27), PARAMETER :: c_upname='get_h_grid_nof_discr_time_w' ! 
    !
    res = 0
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          res = MERGE( 1, 0, get_h_grid_has_discr_time_w( exch_idx ) )
       END IF
    END IF
    !
  END FUNCTION get_h_grid_nof_discr_time_w
  !
  !! Ermittle eine bestimmte diskrete Zeitangaben f&uuml;r das aktuelle Arbeitsobjekt und 
  !! ein bestimmtes Austauschobjekt in OpenMI-konformer Weise anhand der lfd. Nummer
  !! des Zeitpunktes <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION get_h_grid_discr_time_w ( exch_idx, stamp_idx ) &
       RESULT( res )
    !! Zeiger auf die Position der Austauschgr&ouml;&szlig;e in dem
    !! aktuellen Arbeitsobjekt
    INTEGER , INTENT(IN)  :: exch_idx  ! 
    !! Zeiger auf die aktuelle Nummer der Zeitangabe
    INTEGER , INTENT(IN)  :: stamp_idx ! 
    !! Ergebnis: diskrete Zeitangaben sind vorhanden / nicht vorhanden
    TYPE (t_omi_stamp) :: res   ! 
    !! Name der Subroutine
    CHARACTER (LEN=23), PARAMETER :: c_upname='get_h_grid_discr_time_w' ! 
    !! Hilfsvariablen
    TYPE (t_omi_stamp) , POINTER :: p_stamp
    !
    CALL new_omi_stamp( res )
    NULLIFY ( p_stamp )
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          p_stamp => get_stamp_object ( work_object )
          IF ( ASSOCIATED( p_stamp ) ) THEN
             IF ( stamp_idx >= 1 .AND. stamp_idx <= get_h_grid_nof_discr_time_w ( exch_idx ) ) THEN
                res = p_stamp
             END IF
          END IF
       END IF
    END IF
    NULLIFY( p_stamp )
    !
  END FUNCTION get_h_grid_discr_time_w
  !
  !! Transfer des Inhalts verschiedener Komponenten in klassische Felder <BR>
  !! f&uuml;r das aktuelle Arbeitsobjekt                                 <BR>
  !! man beachte, dass z. B. Vierecke in zwei Dreicke zerlegt und so     <BR>
  !! zur&uuml;ckgegeben werden                                           <BR>
  !! <EM>Anmerkung:</EM> die Daten muessen schon in dem Paket vorhanden sein
  SUBROUTINE aux_h_grid_get_w ( addxy, nrand, ninnen, iaktkno,  &
       iaktele, ieleinf, irand, coord, depth, xmin, xmax, ymin, &
       ymax, zmin, zmax, rkmin3, rkmax3, armin, armax )
    !
    !! Additionskonstante fuer die Koordinaten
    REAL    , INTENT(IN)    :: addxy(:)     ! 
    !! Anzahl der Randknoten (nur gitter05)
    INTEGER , INTENT(INOUT) :: nrand        ! 
    !! Anzahl der Innenknoten (nur gitter05)
    INTEGER , INTENT(INOUT) :: ninnen       ! 
    !! akt. Anzahl der Knotenpunkte
    INTEGER , INTENT(INOUT) :: iaktkno      ! 
    !! akt. Anzahl der Elemente (als Dreiecke gez&auml;hlt)
    INTEGER , INTENT(INOUT) :: iaktele      ! 
    !! Knotenverzeichnis der Elemente (Dreiecke)
    INTEGER , INTENT(INOUT) :: ieleinf(:,:) ! 
    !! Randkennungen der Elemente (Dreiecke)
    INTEGER , INTENT(INOUT) :: irand(:)     ! 
    !! Koordinaten der Knotenpunkte
    REAL    , INTENT(INOUT) :: coord(:,:)   ! 
    !! ungestoerte Wassertiefe an den Knoten
    REAL    , INTENT(INOUT) :: depth(:)     ! 
    !! Koordinate des linken Systemrandes
    REAL    , INTENT(INOUT) :: xmin         ! 
    !! Koordinate des rechten Systemrandes
    REAL    , INTENT(INOUT) :: xmax         ! 
    !! Koordinate des unteren Systemrandes
    REAL    , INTENT(INOUT) :: ymin         ! 
    !! Koordinate des oberen Systemrandes
    REAL    , INTENT(INOUT) :: ymax         ! 
    !! minimale ungestoerte Wassertiefe
    REAL    , INTENT(INOUT) :: zmin         ! 
    !! maximale ungestoerte Wassertiefe
    REAL    , INTENT(INOUT) :: zmax         ! 
    !! min. Kantenlaenge (3D)
    REAL    , INTENT(INOUT) :: rkmin3       ! 
    !! max. Kantenlaenge (3D)
    REAL    , INTENT(INOUT) :: rkmax3       ! 
    !! min. Elementflaeche
    REAL    , INTENT(INOUT) :: armin        ! 
    !! max. Elementflaeche
    REAL    , INTENT(INOUT) :: armax        ! 
    !
    !! Name der Subroutine
    CHARACTER (LEN=16), PARAMETER  :: c_upname='aux_h_grid_get_w' !
    ! lokale Variablen
    INTEGER            , POINTER :: p_nrand, p_nv, p_ne, p_nen(:,:), p_ks(:), p_ie(:,:) ! 
    REAL (KIND=Double) , POINTER :: p_xy(:,:), p_hv(:), p_aa(:), p_dy(:) !  
    INTEGER                      :: i, k ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          !
          NULLIFY( p_nv, p_ne, p_nrand, p_nen, p_ks, p_ie, p_xy, p_hv, p_aa, p_dy  )
          !
          nrand        = 0    ! * 
          ninnen       = 0    ! * 
          iaktkno      = 0    ! * 
          iaktele      = 0    ! * 
          ieleinf(:,:) = 0    ! * 
          irand(:)     = 0    ! *
          ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          coord(:,:)   = 0.0  ! *
          depth(:)     = 0.0  ! *
          ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          xmin         = 0.0  ! *
          xmax         = 0.0  ! *
          ymin         = 0.0  ! *
          ymax         = 0.0  ! *
          zmin         = 0.0  ! *
          zmax         = 0.0  ! *
          rkmin3       = 0.0
          rkmax3       = 0.0
          armin        = 0.0
          armax        = 0.0
          ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          p_nv    => get_h_grid_nof_nodes( )
          p_ne    => get_h_grid_nof_poly( )
          p_nrand => get_h_grid_nof_bound_nodes( )
          p_nen   => get_h_grid_nodelist_of_poly( )
          p_ks    => get_h_grid_nodes_of_poly( )
          p_ie    => get_h_grid_polylist_of_poly( )
          p_xy    => get_h_grid_node_coord( )
          p_hv    => get_h_grid_node_depth( )
          p_aa    => get_h_grid_poly_area( )
          p_dy    => get_h_grid_edge_length( )
          !
          ! --- Anzahl der Knoten sowie der Rand- und Innenknoten -----------
          IF ( ASSOCIATED(p_nv) ) THEN
             iaktkno = p_nv
             IF ( ASSOCIATED(p_nrand) ) THEN
                nrand  = p_nrand
                ninnen = iaktkno - nrand
             ELSE
                nrand  = -1
                ninnen = -1
             END IF
          END IF
          ! --- Anzahl der Elemente (als Dreiecke) --------------------------
          IF ( ASSOCIATED(p_ne) ) THEN
             IF ( ASSOCIATED(p_ks) ) THEN
                iaktele = COUNT( p_ks == 3 ) + 2*COUNT( p_ks == 4 )
             ELSE
                iaktele = p_ne
             END IF
          END IF
          ! Knotenverzeichnis der Elemente (als Dreiecke) -------------------
          IF ( ASSOCIATED(p_nen) .AND. ASSOCIATED(p_ne) .AND. ASSOCIATED(p_ks) ) THEN
             CALL check_ext_arr_siz ( 'IELEINF(:,1)', '3', ieleinf(:,1), 3, .TRUE. )
             CALL check_ext_arr_siz ( 'IELEINF(1,:)', 'iaktele', ieleinf(1,:), iaktele, .FALSE. )
             k = 0
             DO i=1,p_ne
                k            = k + 1
                ieleinf(1,k) = p_nen(i,1) - 1 ! TICAD-Nummerierung beginnt bei 0
                ieleinf(2,k) = p_nen(i,2) - 1 ! TICAD-Nummerierung beginnt bei 0
                ieleinf(3,k) = p_nen(i,3) - 1 ! TICAD-Nummerierung beginnt bei 0
                IF ( p_ks(i) == 4 ) THEN
                   k            = k + 1
                   ieleinf(1,k) = p_nen(i,3) - 1 ! TICAD-Nummerierung beginnt bei 0
                   ieleinf(2,k) = p_nen(i,4) - 1 ! TICAD-Nummerierung beginnt bei 0
                   ieleinf(3,k) = p_nen(i,1) - 1 ! TICAD-Nummerierung beginnt bei 0
                END IF
             END DO
          END IF
          ! Randkennungsverzeichnis der Elemente (als Dreiecke) -------------
          IF (  ASSOCIATED(p_ne) .AND. ASSOCIATED(p_ks) .AND. ASSOCIATED(p_ie) ) THEN
             CALL check_ext_arr_siz ( 'IRAND(:)', 'iaktele', irand(:), iaktele, .FALSE. )
             k = 0
             DO i=1,p_ne
                IF ( p_ks(i) == 3 ) THEN
                   k = k + 1
                   IF ( p_ie(i,1) <= 0 ) irand(k) = irand(k) + 4
                   IF ( p_ie(i,2) <= 0 ) irand(k) = irand(k) + 1
                   IF ( p_ie(i,3) <= 0 ) irand(k) = irand(k) + 2
                ELSE IF ( p_ks(i) == 4 ) THEN
                   k = k + 1
                   IF ( p_ie(i,1) <= 0 ) irand(k) = irand(k) + 4
                   IF ( p_ie(i,2) <= 0 ) irand(k) = irand(k) + 1
                   k = k + 1
                   IF ( p_ie(i,3) <= 0 ) irand(k) = irand(k) + 4
                   IF ( p_ie(i,4) <= 0 ) irand(k) = irand(k) + 1
                END IF
             END DO
          END IF
          ! Koordinaten der Knoten ------------------------------------------
          IF ( ASSOCIATED(p_xy) .AND. ASSOCIATED(p_nv) ) THEN
             CALL check_ext_arr_siz ( 'COORD(:,1)', 'p_xy(1,:)', coord(:,1), p_xy(1,:), .TRUE. )
             CALL check_ext_arr_siz ( 'COORD(1,:)', 'p_xy(:,1)', coord(1,:), p_xy(:,1), .FALSE. )
             DO i=1,p_nv
                coord(:,i) = REAL( p_xy(i,:) ) + addxy(:)
             END DO
          END IF
          ! Koordinaten der Tiefen ------------------------------------------
          IF ( ASSOCIATED(p_hv) .AND. ASSOCIATED(p_nv) ) THEN
             CALL check_ext_arr_siz ( 'DEPTH(:)', 'p_hv(:)', depth(:), p_hv(:), .FALSE. )
             DO i=1,p_nv
                depth(i) = REAL( p_hv(i) )
             END DO
          END IF
          ! diverse Maximalwerte --------------------------------------------
          xmin   = MINVAL( coord(1,1:iaktkno) )
          xmax   = MAXVAL( coord(1,1:iaktkno) )
          ymin   = MINVAL( coord(2,1:iaktkno) )
          ymax   = MAXVAL( coord(2,1:iaktkno) )
          zmin   = MINVAL( depth(1:iaktkno)   )
          zmax   = MAXVAL( depth(1:iaktkno)   )
          IF ( ASSOCIATED(p_dy) ) THEN
             rkmin3 = MINVAL( p_dy(:) )
             rkmax3 = MAXVAL( p_dy(:) )
          END IF
          IF ( ASSOCIATED(p_aa) .AND. ASSOCIATED(p_ks) ) THEN
             armin  = 1.0E+31
             armin  = REAL( MINVAL( p_aa(:), p_ks(:) == 3 ) )
             armin  = MIN( armin, REAL( MINVAL( 0.5_Double*p_aa(:), p_ks(:) == 4 ) ) )
             armax = 0.0
             armax  = REAL( MAXVAL( p_aa(:), p_ks(:) == 3 ) )
             armax  = MAX( armax, REAL( MAXVAL( 0.5_Double*p_aa(:), p_ks(:) == 4 ) ) )
          END IF
          !
          NULLIFY( p_nv, p_ne, p_nrand, p_nen, p_ks, p_ie, p_xy, p_hv, p_aa, p_dy  )
          !
       END IF
    END IF
    !
  END SUBROUTINE aux_h_grid_get_w
  !
  !! Transfer des Inhalts verschiedener Komponenten in klassische Felder <BR>
  !! f&uuml;r das aktuelle Arbeitsobjekt                         <BR>
  !! <EM>Anmerkung:</EM> die Daten muessen schon in dem Paket vorhanden sein
  SUBROUTINE aux_h_grid_get_wf ( file, addxy, nrand, ninnen, iaktkno,  &
       iaktele, ieleinf, irand, coord, depth, xmin, xmax, ymin,        &
       ymax, zmin, zmax, rkmin3, rkmax3, armin, armax )
    !
    !! Bezeichnung der Datei aus der die Daten transferiert werden sollen
    TYPE (t_file) , INTENT(IN)    :: file         ! 
    !! Additionskonstante fuer die Koordinaten
    REAL          , INTENT(IN)    :: addxy(:)     ! 
    !! Anzahl der Randknoten (nur gitter05)
    INTEGER       , INTENT(INOUT) :: nrand        ! 
    !! Anzahl der Innenknoten (nur gitter05)
    INTEGER       , INTENT(INOUT) :: ninnen       ! 
    !! akt. Anzahl der Knotenpunkte
    INTEGER       , INTENT(INOUT) :: iaktkno      ! 
    !! akt. Anzahl der Elemente (als Dreiecke gez&auml;hlt)
    INTEGER       , INTENT(INOUT) :: iaktele      ! 
    !! Knotenverzeichnis der Elemente (Dreiecke)
    INTEGER       , INTENT(INOUT) :: ieleinf(:,:) ! 
    !! Randkennungen der Elemente (Dreiecke)
    INTEGER       , INTENT(INOUT) :: irand(:)     ! 
    !! Koordinaten der Knotenpunkte
    REAL          , INTENT(INOUT) :: coord(:,:)   ! 
    !! ungestoerte Wassertiefe an den Knoten
    REAL          , INTENT(INOUT) :: depth(:)     ! 
    !! Koordinate des linken Systemrandes
    REAL          , INTENT(INOUT) :: xmin         ! 
    !! Koordinate des rechten Systemrandes
    REAL          , INTENT(INOUT) :: xmax         ! 
    !! Koordinate des unteren Systemrandes
    REAL          , INTENT(INOUT) :: ymin         ! 
    !! Koordinate des oberen Systemrandes
    REAL          , INTENT(INOUT) :: ymax         ! 
    !! minimale ungestoerte Wassertiefe
    REAL          , INTENT(INOUT) :: zmin         ! 
    !! maximale ungestoerte Wassertiefe
    REAL          , INTENT(INOUT) :: zmax         ! 
    !! min. Kantenlaenge (3D)
    REAL          , INTENT(INOUT) :: rkmin3       ! 
    !! max. Kantenlaenge (3D)
    REAL          , INTENT(INOUT) :: rkmax3       ! 
    !! min. Elementflaeche
    REAL          , INTENT(INOUT) :: armin        ! 
    !! max. Elementflaeche
    REAL          , INTENT(INOUT) :: armax        ! 
    !
    !! Name der Subroutine
    CHARACTER (LEN=17), PARAMETER  :: c_upname='aux_h_grid_get_wf' !
    !
    IF ( ok_initialised( c_upname ) ) THEN
       IF ( ok_work_object ( c_upname ) ) THEN
          CALL setup_file_object ( work_object, file )
          CALL aux_h_grid_get_w ( addxy, nrand, ninnen, iaktkno,        &
               iaktele, ieleinf, irand, coord, depth, xmin, xmax, ymin, &
               ymax, zmin, zmax, rkmin3, rkmax3, armin, armax )
       END IF
    END IF
    !
  END SUBROUTINE aux_h_grid_get_wf
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
  !! Setzen der Fehlerbedingung 1 = Modul nicht initialisiert <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_initialised ( upname ) &
       RESULT( ok )
    !! Name der Subroutine die "ok_initialised" ruft
    CHARACTER (LEN=*) , INTENT(IN) :: upname
    !! Testergebnis
    LOGICAL :: ok
    !! Fehlernummer
    INTEGER            :: ierr
    !! Fehlertext
    CHARACTER (LEN=80) :: cerr(3)
    !
    ok = initialised
    !
    IF ( .NOT. ok ) THEN
       WRITE(*,*) ' *** Warnung *** Modul "p_h_grid" nicht initialisiert'
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Package "p_h_grid" ist nicht initialisiert'
       cerr(3) = '--> INIT_h_grid ausfuehren'
       CALL setup_error_act ( ierr, cerr(:), upname, c_modname )
    END IF
    !
  END FUNCTION ok_initialised
  !
  !! Setzen der Fehlerbedingung 2 = Modul schon initialisiert <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION not_initialised ( upname ) &
       RESULT( ok )
    !! Name der Subroutine die "not_initialised" ruft
    CHARACTER (LEN=*) , INTENT(IN) :: upname
    !! Testergebnis
    LOGICAL :: ok
    !
    ok = .NOT. initialised
    !
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 2, upname, c_modname )
    !
  END FUNCTION not_initialised
  !
  !! Allokieren/Initialisieren aller Fehlermeldungen des Packages <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE init_h_grid_all_errors ( )
    !
    CALL init_all_errors ( )
    !
  END SUBROUTINE init_h_grid_all_errors
  !
  !! De-Allokieren/Re-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_h_grid_all_errors ( )
    !
    CALL clear_all_errors ( )
    !
  END SUBROUTINE clear_h_grid_all_errors
  !
  !! De-Allokieren/Re-Initialisieren aller Objekte "t_h_grid" des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_h_grid_all_objects ( )
    !! Liste der aktuell vorhandenen Objekte
    INTEGER , POINTER :: id(:)
    !
    id => get_h_grid_all_id ( )
    !
    IF ( ASSOCIATED( id ) ) THEN
       CALL kill_h_grid ( id )
       DEALLOCATE ( id )
    END IF
    !
  END SUBROUTINE clear_h_grid_all_objects
  !
  !! Lesen von Daten aus Datei in das Objekt "this" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_h_grid_0 ( this, ncsize )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! optional: Anzahl an CPUs (beim parallelen Rechnen mit TELEMAC)
    INTEGER, INTENT(IN), OPTIONAL :: ncsize
    !! Name der Routine
    CHARACTER (LEN=13) , PARAMETER :: c_upname='read_h_grid_0'
    !! Hilfsvariable
    CHARACTER (LEN=10) :: string ! 
    !
    IF ( no_error( ) ) CALL set_file_action ( this%file, 'READ' )
    IF ( no_error( ) ) CALL set_file_status ( this%file, 'OLD' )
    !
    IF ( ok_file ( this%file ) ) THEN
       !
       IF ( ok_h_grid_variant_no ( this ) ) THEN
          !
          CALL open_file ( this%file )
          !
          IF ( no_error ( ) ) THEN
             !
             SELECT CASE ( get_h_grid_variant_no ( this ) )
             CASE ( 1 )
                CALL read_h_grid_gitter05_asc_0 ( this )
             CASE ( 2 )
                CALL read_h_grid_gitter05_bin_0 ( this )
             CASE ( 3 )
                CALL read_h_grid_untrim_baw_0   ( this )
             CASE ( 4 )
                CALL read_h_grid_untrim_vc_0    ( this )
             CASE ( 5 )
                IF (PRESENT(ncsize)) THEN
                   CALL read_h_grid_selafin_0   ( this, ncsize )
                ELSE
                   CALL read_h_grid_selafin_0   ( this )
                ENDIF
             CASE ( 6 )
                CALL read_h_grid_delft3d_0      ( this )
             CASE ( 8 )
                CALL read_h_grid_topo_0         ( this )
             CASE ( 9 )
                CALL read_h_grid_tr2topo_0      ( this )
             CASE ( 10 )
                CALL read_h_grid_tr2topoind_0   ( this )
             CASE DEFAULT
                CALL setup_error_act ( all_errors(:), -23001, c_upname, c_modname )
                WRITE ( string, '(I10)') get_h_grid_variant_no ( this )
                CALL setup_error_act ( '<DateiVarianteNo>' , string )
                WRITE ( string, '(I10)') c_max_variants
                CALL setup_error_act ( '<MaxDateiVarianteNo>' , string )
             END SELECT ! Hinweis: Anzahl der CASEs muss mit "c_max_variants" uebereinstimmen
             !
          END IF
          IF(no_error( )) CALL close_file ( this%file )
       END IF
    END IF
    !
  END SUBROUTINE read_h_grid_0
  !
  !! Lesen von Daten aus Datei in das Objekt "this" (Dateityp = gitter05.dat)
  SUBROUTINE read_h_grid_gitter05_asc_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !
    CALL read_h_grid_gitter05_asc ( this )
    !
  END SUBROUTINE read_h_grid_gitter05_asc_0
  !
  !! Lesen von Daten aus Datei in das Objekt "this" (Dateityp = gitter05.bin)
  SUBROUTINE read_h_grid_gitter05_bin_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !
    CALL read_h_grid_gitter05_bin ( this )
    !
  END SUBROUTINE read_h_grid_gitter05_bin_0
  !
  !! Lesen von Daten aus Datei in das Objekt "this" (Dateityp = selafin)
  SUBROUTINE read_h_grid_selafin_0 ( this, ncsize )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! optional: Anzahl an CPUs (beim parallelen Rechnen mit TELEMAC)
    INTEGER, INTENT(IN), OPTIONAL :: ncsize
    ! Nullwert
    INTEGER, PARAMETER :: n_zero = 0 ! 
    !
    IF (PRESENT(ncsize)) THEN
       CALL setup_h_grid_nof_cpus (ncsize)
       IF (no_error()) CALL read_h_grid_selafin_seq_bin ( this )
    ELSE
       CALL setup_h_grid_nof_cpus (n_zero)
       CALL setup_h_grid_nof_section_nodes (n_zero)
       CALL setup_h_grid_nof_intface_nodes (n_zero)
       CALL read_h_grid_selafin_seq_bin ( this )
    ENDIF
    !
  END SUBROUTINE read_h_grid_selafin_0
  !
  !! Lesen von Daten aus Datei in das Objekt "this" (Dateityp = untrim_grid.dat [BAW])
  SUBROUTINE read_h_grid_untrim_baw_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !
    CALL read_h_grid_untrim_baw_asc ( this )
    !
  END SUBROUTINE read_h_grid_untrim_baw_0
  !
  !! Lesen von Daten aus Datei in das Objekt "this" (Dateityp = untrim_grid.dat [VC])
  SUBROUTINE read_h_grid_untrim_vc_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !
    CALL read_h_grid_untrim_vc_asc ( this )
    !
  END SUBROUTINE read_h_grid_untrim_vc_0
  !
  !! Lesen von Daten aus Datei in das Objekt "this" (Dateityp = delft3d.grd)
  SUBROUTINE read_h_grid_delft3d_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !
    CALL read_h_grid_delft3d_seq_asc ( this )
    !
  END SUBROUTINE read_h_grid_delft3d_0
  !
  !! Lesen von Daten aus Datei in das Objekt "this" (Dateityp = topo.bin)
  SUBROUTINE read_h_grid_topo_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Routine
    CHARACTER (LEN=18) , PARAMETER :: c_upname='read_h_grid_topo_0'
    ! Hinweis: die Datei wurde schon geoeffnet
    WRITE(*,*) ' *** missing code *** '//TRIM(c_upname)
    !
  END SUBROUTINE read_h_grid_topo_0
  !
  !! Lesen von Daten aus Datei in das Objekt "this" (Dateityp = tr2.topo.bin)
  SUBROUTINE read_h_grid_tr2topo_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Routine
    CHARACTER (LEN=21) , PARAMETER :: c_upname='read_h_grid_tr2topo_0'
    ! Hinweis: die Datei wurde schon geoeffnet
    WRITE(*,*) ' *** missing code *** '//TRIM(c_upname)
    !
  END SUBROUTINE read_h_grid_tr2topo_0
  !
  !! Lesen von Daten aus Datei in das Objekt "this" (Dateityp = tr2.topo.bin.ind)
  SUBROUTINE read_h_grid_tr2topoind_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Routine
    CHARACTER (LEN=24) , PARAMETER :: c_upname='read_h_grid_tr2topoind_0'
    ! Hinweis: die Datei wurde schon geoeffnet
    WRITE(*,*) ' *** missing code *** '//TRIM(c_upname)
    !
  END SUBROUTINE read_h_grid_tr2topoind_0
  !
  !! Schreiben von Daten aus Objekt "this" in Datei <BR>
  !! vor dem Schreiben wird das Gitter auf Korrektheit gepr&uuml;ft <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE write_h_grid_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Routine
    CHARACTER (LEN=14) , PARAMETER :: c_upname='write_h_grid_0'
    !! Hilfsvariable
    CHARACTER (LEN=10) :: string
    !
    IF ( no_error( ) ) CALL set_file_action ( this%file, 'WRITE' )
    !
    IF ( ok_file ( this%file ) ) THEN
       IF ( ok_h_grid_variant_no ( this ) ) THEN
          CALL open_file ( this%file )
          IF ( no_error ( ) ) THEN
             SELECT CASE ( get_h_grid_variant_no ( this ) )
             CASE ( 1 )
                CALL write_h_grid_gitter05_asc_0 ( this )
             CASE ( 2 )
                CALL write_h_grid_gitter05_bin_0 ( this )
             CASE ( 3 )
                CALL write_h_grid_untrim_baw_0   ( this )
             CASE ( 4 )
                CALL write_h_grid_untrim_vc_0    ( this )
             CASE ( 5 )
                CALL write_h_grid_selafin_0      ( this )
             CASE ( 6 )
                CALL write_h_grid_delft3d_0      ( this )
             CASE ( 8 )
                CALL write_h_grid_topo_0         ( this )
             CASE ( 9 )
                CALL write_h_grid_tr2topo_0      ( this )
             CASE ( 10 )
                CALL write_h_grid_tr2topoind_0   ( this )
             CASE DEFAULT
                CALL setup_error_act ( all_errors(:), -24001, c_upname, c_modname )
                WRITE ( string, '(I10)') get_h_grid_variant_no ( this )
                CALL setup_error_act ( '<DateiVarianteNo>' , string )
                WRITE ( string, '(I10)') c_max_variants
                CALL setup_error_act ( '<MaxDateiVarianteNo>' , string )
             END SELECT ! Hinweis: Anzahl der CASEs muss mit "c_max_variants" uebereinstimmen
          END IF
          IF(no_error( )) CALL close_file ( this%file )
       END IF
    END IF
    !
  END SUBROUTINE write_h_grid_0
  !
  !! Schreiben von Daten aus Objekt "this" in Datei (Dateityp = gitter05.dat)
  SUBROUTINE write_h_grid_gitter05_asc_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !
    CALL write_h_grid_gitter05_asc ( this )
    !
  END SUBROUTINE write_h_grid_gitter05_asc_0
  !
  !! Schreiben von Daten aus Objekt "this" in Datei (Dateityp = gitter05.bin)
  SUBROUTINE write_h_grid_gitter05_bin_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !
    CALL write_h_grid_gitter05_bin ( this )
    !
  END SUBROUTINE write_h_grid_gitter05_bin_0
  !
  !! Schreiben von Daten aus Objekt "this" in Datei (Dateityp = selafin)
  SUBROUTINE write_h_grid_selafin_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !
    CALL write_h_grid_selafin_seq_bin ( this )
    !
  END SUBROUTINE write_h_grid_selafin_0
  !
  !! Schreiben von Daten aus Objekt "this" in Datei (Dateityp = untrim_grid.dat [BAW])
  SUBROUTINE write_h_grid_untrim_baw_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !
    CALL write_h_grid_untrim_baw_asc ( this )
    !
  END SUBROUTINE write_h_grid_untrim_baw_0
  !
  !! Schreiben von Daten aus Objekt "this" in Datei (Dateityp = untrim_grid.dat [VC])
  SUBROUTINE write_h_grid_untrim_vc_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !
    CALL write_h_grid_untrim_vc_asc ( this )
    !
  END SUBROUTINE write_h_grid_untrim_vc_0
  !
  !! Schreiben von Daten aus Objekt "this" in Datei (Dateityp = delft3d.grd)
  SUBROUTINE write_h_grid_delft3d_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !
    CALL write_h_grid_delft3d_seq_asc ( this )
    !
  END SUBROUTINE write_h_grid_delft3d_0
  !
  !! Schreiben von Daten aus Objekt "this" in Datei (Dateityp = topo.bin)
  SUBROUTINE write_h_grid_topo_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Routine
    CHARACTER (LEN=19) , PARAMETER :: c_upname='write_h_grid_topo_0'
    ! Hinweis: die Datei wurde schon geoeffnet
    WRITE(*,*) ' *** missing code *** '//TRIM(c_upname)
    !
  END SUBROUTINE write_h_grid_topo_0
  !
  !! Schreiben von Daten aus Objekt "this" in Datei (Dateityp = tr2.topo.bin)
  SUBROUTINE write_h_grid_tr2topo_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Routine
    CHARACTER (LEN=22) , PARAMETER :: c_upname='write_h_grid_tr2topo_0'
    ! Hinweis: die Datei wurde schon geoeffnet
    WRITE(*,*) ' *** missing code *** '//TRIM(c_upname)
    !
  END SUBROUTINE write_h_grid_tr2topo_0
  !
  !! Schreiben von Daten aus Objekt "this" in Datei (Dateityp = tr2.topo.bin.ind)
  SUBROUTINE write_h_grid_tr2topoind_0 ( this )
    !! Datenobjekt
    TYPE (t_h_grid) , POINTER :: this
    !! Name der Routine
    CHARACTER (LEN=25) , PARAMETER :: c_upname='write_h_grid_tr2topoind_0'
    ! Hinweis: die Datei wurde schon geoeffnet
    WRITE(*,*) ' *** missing code *** '//TRIM(c_upname)
    !
  END SUBROUTINE write_h_grid_tr2topoind_0
  !
  !! &Uuml;berpr&uuml;fen der Feldgrenzen f&uuml;r eindimensionale INTEGER-Felder <BR>
  !! Werfen der Fehlermeldung 50000 , falls die erforderliche Bedingung verletzt wird
  SUBROUTINE check_ext_arr_siz_i_0 ( name_ext, name_int, f_ext, c_int, eq )
    !! Name des externen Feldes
    CHARACTER (LEN=*)  , INTENT(IN) :: name_ext ! 
    !! Name des internen Feldes
    CHARACTER (LEN=*)  , INTENT(IN) :: name_int ! 
    !! externes ein-dimensionales Feld
    INTEGER            , INTENT(IN) :: f_ext(:) ! 
    !! konstante Feldgrenze
    INTEGER            , INTENT(IN) :: c_int    ! 
    !! Vergleichsoperator <BR>
    !! .true.  : externes und internes Feld muessen exakt gleich sein <BR>
    !! .false. : das externe Feld kann groesser als das interne Feld sein
    LOGICAL            , INTENT(IN) :: eq       ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=21) , PARAMETER :: c_upname='check_ext_arr_siz_i_1' ! 
    ! lokale Variablen
    CHARACTER (LEN=10) :: l_char       ! 
    INTEGER            :: n_ext, n_int ! 
    !
    n_int = c_int
    n_ext = SIZE(f_ext)
    !
    IF ( eq .AND. ( n_int /= n_ext ) .OR. .NOT. eq .AND. ( n_int > n_ext ) ) THEN
       CALL setup_error_act ( all_errors, 50000, c_upname, c_modname )
       CALL setup_error_act ( '<ExtFieldName>', name_ext             )
       CALL setup_error_act ( '<IntComponentName>', name_int         )
       WRITE(l_char,'(I10)') SHAPE(f_ext) ; CALL setup_error_act ( '<AktDim>', l_char  )
       WRITE(l_char,'(I10)') n_int        ; CALL setup_error_act ( '<ReqDim>', l_char  )
       WRITE(l_CHAR(1:1),'(L1)') eq       ; CALL setup_error_act ( '<Op>', l_CHAR(1:1) )
    END IF
    !
  END SUBROUTINE check_ext_arr_siz_i_0
  !
  !! &Uuml;berpr&uuml;fen der Feldgrenzen f&uuml;r eindimensionale INTEGER-Felder <BR>
  !! Werfen der Fehlermeldung 50000 , falls die erforderliche Bedingung verletzt wird
  SUBROUTINE check_ext_arr_siz_i_1 ( name_ext, name_int, f_ext, f_int, eq )
    !! Name des externen Feldes
    CHARACTER (LEN=*)  , INTENT(IN) :: name_ext ! 
    !! Name des internen Feldes
    CHARACTER (LEN=*)  , INTENT(IN) :: name_int ! 
    !! externes ein-dimensionales Feld
    INTEGER            , INTENT(IN) :: f_ext(:) ! 
    !! interne ein-dimensionale Komponente
    INTEGER            , INTENT(IN) :: f_INT(:) ! 
    !! Vergleichsoperator <BR>
    !! .true.  : externes und internes Feld muessen exakt gleich sein <BR>
    !! .false. : das externe Feld kann groesser als das interne Feld sein
    LOGICAL            , INTENT(IN) :: eq       ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=21) , PARAMETER :: c_upname='check_ext_arr_siz_i_1' ! 
    ! lokale Variablen
    CHARACTER (LEN=10) :: l_char       ! 
    INTEGER            :: n_ext, n_int ! 
    !
    n_int = SIZE(f_int)
    n_ext = SIZE(f_ext)
    !
    IF ( eq .AND. ( n_int /= n_ext ) .OR. .NOT. eq .AND. ( n_int > n_ext ) ) THEN
       CALL setup_error_act ( all_errors, 50000, c_upname, c_modname )
       CALL setup_error_act ( '<ExtFieldName>', name_ext             )
       CALL setup_error_act ( '<IntComponentName>', name_int         )
       WRITE(l_char,'(I10)') SHAPE(f_ext) ; CALL setup_error_act ( '<AktDim>', l_char  )
       WRITE(l_char,'(I10)') SHAPE(f_int) ; CALL setup_error_act ( '<ReqDim>', l_char  )
       WRITE(l_CHAR(1:1),'(L1)') eq       ; CALL setup_error_act ( '<Op>', l_CHAR(1:1) )
    END IF
    !
  END SUBROUTINE check_ext_arr_siz_i_1
  !
  !! &Uuml;berpr&uuml;fen der Feldgrenzen f&uuml;r eindimensionale REAL-Felder <BR>
  !! Werfen der Fehlermeldung 50000 , falls die erforderliche Bedingung verletzt wird
  SUBROUTINE check_ext_arr_siz_r_1 ( name_ext, name_int, f_ext, f_int, eq )
    !! Name des externen Feldes
    CHARACTER (LEN=*)  , INTENT(IN) :: name_ext ! 
    !! Name des internen Feldes
    CHARACTER (LEN=*)  , INTENT(IN) :: name_int ! 
    !! externes ein-dimensionales Feld
    REAL               , INTENT(IN) :: f_ext(:) ! 
    !! interne ein-dimensionale Komponente
    REAL (KIND=Double) , INTENT(IN) :: f_INT(:) ! 
    !! Vergleichsoperator <BR>
    !! .true.  : externes und internes Feld muessen exakt gleich sein <BR>
    !! .false. : das externe Feld kann groesser als das interne Feld sein
    LOGICAL            , INTENT(IN) :: eq       ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=21) , PARAMETER :: c_upname='check_ext_arr_siz_i_1' ! 
    ! lokale Variablen
    CHARACTER (LEN=10) :: l_char       ! 
    INTEGER            :: n_ext, n_int ! 
    !
    n_int = SIZE(f_int)
    n_ext = SIZE(f_ext)
    !
    IF ( eq .AND. ( n_int /= n_ext ) .OR. .NOT. eq .AND. ( n_int > n_ext ) ) THEN
       CALL setup_error_act ( all_errors, 50000, c_upname, c_modname )
       CALL setup_error_act ( '<ExtFieldName>', name_ext             )
       CALL setup_error_act ( '<IntComponentName>', name_int         )
       WRITE(l_char,'(I10)') SHAPE(f_ext) ; CALL setup_error_act ( '<AktDim>', l_char  )
       WRITE(l_char,'(I10)') SHAPE(f_int) ; CALL setup_error_act ( '<ReqDim>', l_char  )
       WRITE(l_CHAR(1:1),'(L1)') eq       ; CALL setup_error_act ( '<Op>', l_CHAR(1:1) )
    END IF
    !
  END SUBROUTINE check_ext_arr_siz_r_1
  !
END MODULE p_h_grid_ui
! TailOfPackageUserInterface -----------------------------------------------
