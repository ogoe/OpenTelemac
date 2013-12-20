! -------------------------------------------------------------------------
! HeadOfPackageModule -----------------------------------------------------
!
!! <H2>Modul zum Bereitstellen OpenMI-konformer Informationen f&uuml;r IPDS</h2>
!! @author <A HREF="mailto:lang@hamburg.baw.de">G. Lang</A>
!! @version 3.1 vom 13.03 07, Quellcode: mod_m_ipds_omi.f90
!! <HR>
!! generation of OpenMI-compliant informations related to the IPDS software package <BR>
!! <HR>
!  Copyright-Hinweis
!                                                                    <BR>
!  Copyright (C) 2005 <A HREF="http://www.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!                                                                    <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2005-08-10 : G. Lang : Erstversion
!  02.01 : 2005-08-10 : G. Lang : Erweiterung fuer Export OpenMI-konformer Daten
!  02.02 : 2007-03-02 : G. Lang : Korrektur beu Namensuebertragung nach "l_stru_id" in "create_omi_l_ind_i_d"
!  03.01 : 2007-03-13 : G. Lang : neue Hauptversionsnummer 3
!
!!                                            
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! <OL>
!!   <LI> Bereitstellen verschiedener Leistungen zum Erzeugen OpenMI-konformer Daten f&uuml;r "ipds".
!! </OL>
!! <HR>
!
MODULE m_ipds_omi
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1.1] Basis-Modul mit globalen Konstantwerten [ggf. entfernen]
  USE b_constants, ONLY :   &
       Double
  !
  ! [A.1.2] Basis-Modul mit Typ+Methoden "Fehlerbehandlung"
  USE b_error, ONLY :       &
       ! Routinen / Interfaces
       no_error,            &
       any_error,           &
       setup_error_act
  !
  ! [A.1.3] Basis-Modul mit Typ+Methoden "Zeitangaben"
  USE b_datetime, ONLY :   &
       ! Typdefinition
       t_datetime
  !
  ! [A.1.4] Basis-Modul mit Typ+Methoden "Dateibehandlung"
  USE b_file, ONLY :          &
       ! Typdefinition
       t_file,                &
       ! Routinen / Interfaces
       ne_file,               &
       new_file,              &
       ok_file,               &
       print_file,            &
       kill_file,             &
       get_file_type,         &
       get_file_path,         &
       get_file_name,         &
       auto_file_access_form, &
       set_file_unit        
  !
  ! [A.1.5] Basis-Modul mit Typ+Methoden "physikalische Groessen"
  USE b_phy, ONLY :              &
       ! Routinen / Interfaces
       get_phy_quant_ref_code,   &
       get_phy_quant_descr,      &
       get_phy_quant_ref_descr,  &
       get_phy_quant_omiele_nof, &
       get_phy_quant_omigen_nof, &
       get_phy_quant_omiele,     &
       get_phy_quant_omigen,     &
       is_phy_quant_synoptic,    &
       is_phy_quant_period,      &
       is_phy_quant_omiele_3d
  !
  ! [A.1.6] Basis-Modul mit Typ+Methoden "Dimensionsbezeichner"
  USE b_dim, ONLY :              &
       ! Typ
       t_dim, &
       ! Routinen / Interfaces
       ok_dim
  !
  ! [A.1.7] Basis-Modul mit Typ+Methoden "Variablenbezeichner"
  USE b_var, ONLY :         &
       ! Datentyp
       t_var,               &
       ! Routinen / Interfaces
       ok_var,              &
       get_var_name
  !
  ! [A.1.8] Basis-Modul mit Typ+Methoden "Attributbezeichner"
  USE b_att, ONLY :         &
       ! Datentyp
       t_att, &
       ! Daten
       c_att_name,               &
       ! Routinen / Interfaces
       ok_att,                   &
       get_att_var_name_id,      &
       get_att_interfaces_count, &
       get_att_ch_as_datetime,   &
       get_att_nof_values,       &
       get_att_idx
  !
  ! [A.1.9] Basis-Modul mit Typ+Methoden "2D-Koordinaten"
  USE b_point_2d, ONLY :      &
       ! Datentyp
       t_point_2d,            &
       ! Routinen / Interfaces
       get_point_2d_x,        &
       get_point_2d_y,        &
       get_point_2d_diff_ind, &
       get_point_2d_diff_idx, &
       get_point_2d_diff_count
  !
  ! [A.2.1] Basis-Modul mit Typ+Methoden fuer OpenMI-konforme <EM>Quantit&auml;ten</EM>
  USE b_omi_quant, ONLY :         &
       ! Typdefinition
       t_omi_quant,               &
       ! Konstante
       c_len_omi_quant_id,        &
       c_undef_omi_quant_int,     &
       ! Routinen / Interfaces
       new_omi_quant,             &
       kill_omi_quant,            &
       get_code_from_omi_quant,   &
       get_cla_no_from_omi_quant, &
       get_var_no_from_omi_quant, &
       get_max_omi_quant,         &
       get_omi_quant_id,          &
       get_omi_quant_idx,         &
       set_omi_quant,             &
       is_omi_quant_magnitude
  !
  ! [A.2.2] Basis-Modul mit Typ+Methoden fuer OpenMI-konforme Koordinatensystem-Beschreibungen
  USE b_omi_space, ONLY :      &
       ! Typdefinition
       t_omi_space,            &
       ! Routinen / Interfaces
       eq_omi_space,           &
       new_omi_space,          &
       kill_omi_space,         &
       get_omi_space_id,       &
       set_omi_space_id
  !
  ! [A.2.3] Basis-Modul mit Typ+Methoden fuer OpenMI-konforme Koordinaten-Objekte
  USE b_omi_xyz, ONLY :               &
       ! Typdefinition
       t_omi_xyz,                     &
       ! Konstante
       c_len_omi_xyz_id,              &
       c_len_omi_xyz_description,     &
       ! Routinen / Interfaces
       new_omi_xyz,                   &
       kill_omi_xyz,                  &
       get_omi_xyz_idx,               &
       get_omi_xyz_id,                &
       get_omi_xyz_description,       &
       get_omi_xyz_refsystem,         &
       get_omi_xyz_x_ref,             &
       get_omi_xyz_y_ref,             &
       get_omi_xyz_z_ref,             &
       get_omi_xyz_bini_ref,          &
       get_omi_xyz_ctype,             &
       get_omi_xyz_point_count,       &
       get_omi_xyz_layer_count,       &
       get_omi_xyz_layer_name,        &
       get_omi_xyz_layer_point_count, &
       get_omi_xyz_layer_point_idx,   &
       set_omi_xyz_id,                &
       set_omi_xyz_description,       &
       set_omi_xyz_refsystem,         &
       set_omi_xyz_ztype,             &
       set_omi_xyz_ctype,             &
       set_omi_xyz_hlimit,            &
       set_omi_xyz_layers,            &
       is_omi_xyz_three_dimensional,  &
       create_omi_xyz_no_layers,      &
       create_omi_xyz_with_layers,    &
       update_omi_xyz
  !
  ! [A.2.4] Basis-Modul mit Typ+Methoden fuer OpenMI-konforme Verweislisten-Objekte
  USE b_omi_ind, ONLY :           &
       ! Typdefinition
       t_omi_ind,                 &
       ! Konstante
       c_len_omi_ind_id,          &
       c_len_omi_ind_description, &
       c_len_omi_ind_stru_id,     &
       ! Routinen / Interfaces
       new_omi_ind,               &
       kill_omi_ind,              &
       get_omi_ind_id,            &
       get_omi_ind_description,   &
       get_omi_ind_idx,           &
       set_omi_ind_id,            &
       set_omi_ind_description,   &
       set_omi_ind_stru_start,    &
       set_omi_ind_stru_len,      &
       set_omi_ind_stru_list,     &
       set_omi_ind_stru_id,       &
       copy_omi_ind,              &
       create_omi_ind_pt,         &
       create_omi_ind_lay_pt
  !
  ! [A.2.5] Basis-Modul mit Typ+Methoden fuer OpenMI-konforme ElementSets-Objekte
  USE b_omi_ele, ONLY :         &
       ! Typdefinition
       t_omi_ele,               &
       c_len_omi_ele_id,        &
       c_undef_omi_ele_char,    &
       ! Routinen / Interfaces
       new_omi_ele,             &
       kill_omi_ele,            &
       set_omi_ele_auto,        &
       get_omi_ele_id,          &
       get_omi_ele_xyz_ref,     &
       get_omi_ele_ind_xyz_ref, &
       get_omi_ele_ind_fac_ref
  !
  ! [A.2.6] Basis-Modul mit Typ+Methoden fuer OpenMI-konforme Arguments-Objekte
  USE b_omi_arg, ONLY :         &
       ! Typdefinition
       t_omi_arg,               &
       ! Routinen / Interfaces
       new_omi_arg,             &
       kill_omi_arg,            &
       set_omi_arg_key,         &
       set_omi_arg_value,       &
       set_omi_arg_readonly,    &
       set_omi_arg_description
  !
  ! [A.2.7] Basis-Modul mit Typ+Methoden fuer OpenMI-konforme DataOperations-Objekte
  USE b_omi_dope, ONLY :        &
       ! Typdefinition
       t_omi_dope,              &
       ! Routinen / Interfaces
       new_omi_dope,            &
       kill_omi_dope,           &
       set_omi_dope_auto
  !
  ! [A.2.8] Basis-Modul mit Typ+Methoden fuer OpenMI-konforme ExchangeItems-Objekte
  USE b_omi_exch, ONLY :        &
       ! Typdefinition
       t_omi_exch,              &
       ! Routinen / Interfaces
       new_omi_exch,            &
       kill_omi_exch,           &
       set_omi_exch_quant_ref,  &
       set_omi_exch_ele_ref,    &
       set_omi_exch_dope_ref,   &
       set_omi_exch_role,       &
       get_omi_exch_ele_ref,    &
       get_omi_exch_quant_ref
  !
  ! [A.2.9] Basis-Modul mit Typ+Methoden fuer OpenMI-konforme Zeitspannen-Objekte
  USE b_omi_span, ONLY :       &
       ! Typdefinition
       t_omi_span,             &
       ! Routinen / Interfaces
       new_omi_span,           &
       kill_omi_span,          &
       set_omi_span_start,     &
       set_omi_span_end
  !
  ! [A.2.10] Basis-Modul mit Typ+Methoden fuer OpenMI-konforme Terminangaben
  USE b_omi_stamp, ONLY :          &
       ! Typdefinition
       t_omi_stamp,                &
       ! Routinen / Interfaces
       new_omi_stamp,              &
       kill_omi_stamp,             &
       set_omi_stamp_modjulianday, &
       get_omi_stamp_closest_idx
  !
  ! ---------------------------------------------------------------------
  ! [B]  Module des Paketes "SediMorph"
  ! ---------------------------------------------------------------------
  !
  ! [B.1] Paket-Modul mit globalen Daten
  USE m_ipds_data, ONLY :        &
       ! Typdefinition
       t_ipds,                   &
       ! Daten
       all_errors,               &
       c_missing_ipds_double,    &
       ! Routinen / Interfaces
       get_dim_object,           &
       get_var_object,           &
       get_att_object,           &
       get_xyz_object,           &
       get_ind_object,           &
       get_ele_object,           &
       get_quant_object,         &
       get_dope_object,          &
       get_stamp_object,         &
       get_exch_object,          &
       get_mespos_object,        &
       setup_quant_object,       &
       setup_xyz_object_ref,     &
       setup_ind_object_ref,     &
       setup_ele_object,         &
       setup_xyz_object,         &
       setup_ind_object,         &
       setup_dope_object,        &
       setup_exch_object,        &
       setup_stamp_object,       &
       setup_span_object
  !
  ! [B.2] Paket-Modul mit Derive-Methoden
  USE m_ipds_derive, ONLY :      &
       ! Routinen / Interfaces
       derive_dim,               &
       derive_var,               &
       derive_att
  !
  ! [B.3] Paket-Modul Sampling Points
  USE m_ipds_mespos, ONLY :      &
       ! Typdefinition
       ! Daten
       c_len_mespos_name,        &
       ! Routinen / Interfaces
       get_mespos_coor,          &
       get_mespos_name
  !
  ! [B.4] Paket-Modul Regional Values
  USE m_ipds_region, ONLY :      &
       ! Typdefinition
       ! Routinen / Interfaces
       get_region_nof_border,    &
       get_region_border,        &
       get_region_name
  !
  ! [B.5] Paket-Modul physikalische Daten
  USE m_ipds_phyval, ONLY :      &
       ! Daten
       c_len_phyval_name
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
  !! Erzeuge alle OpenMI-konformen Daten f&uuml;r ein Objekt des Typs "t_ipds"
  INTERFACE create_omi
     MODULE PROCEDURE create_omi_w0
  END INTERFACE
  !! pr&uuml;fe, ob eine Gr&ouml;&szlig;e &uuml;ber diskrete Zeitangaben verf&uuml;gt
  INTERFACE has_omi_discr_time
     MODULE PROCEDURE has_omi_discr_time_w0
  END INTERFACE
  !! pr&uuml;fe, ob eine Gr&ouml;&szlig;e &uuml;ber diskrete Zeitangaben verf&uuml;gt
  INTERFACE has_omi_time_span
     MODULE PROCEDURE has_omi_time_span_w0
  END INTERFACE
  !! ermittle die Anzahl der diskreten Zeitangaben
  INTERFACE nof_omi_discr_time
     MODULE PROCEDURE nof_omi_discr_time_w0
  END INTERFACE
  !! ermittle eine diskrete Zeitangabe
  INTERFACE get_omi_discr_time
     MODULE PROCEDURE get_omi_discr_time_w0
  END INTERFACE
  !! ermittle eine Zeitraumangabe
  INTERFACE get_omi_time_span
     MODULE PROCEDURE get_omi_time_span_w0
  END INTERFACE
  !
  !! ermittle die x-Koordinaten des Koordinatenobjektes einer Austauschgr&ouml;&szlig;e 
  INTERFACE get_omi_xyz_x_coord
     MODULE PROCEDURE get_omi_xyz_x_coord_w0
  END INTERFACE
  !! ermittle die y-Koordinaten des Koordinatenobjektes einer Austauschgr&ouml;&szlig;e 
  INTERFACE get_omi_xyz_y_coord
     MODULE PROCEDURE get_omi_xyz_y_coord_w0
  END INTERFACE
  !! ermittle die z-Koordinaten des Koordinatenobjektes einer Austauschgr&ouml;&szlig;e 
  INTERFACE get_omi_xyz_z_coord
     MODULE PROCEDURE get_omi_xyz_z_coord_w0
  END INTERFACE
  !! ermittle den Namen der physikalischen Gr&ouml;&szlig;e einer Austauschgr&ouml;&szlig;e 
  INTERFACE get_omi_phyval_name
     MODULE PROCEDURE get_omi_phyval_name_w0
  END INTERFACE
  !! Ermittle die Varianten-Nummer einer Austauschgr&ouml;&szlig;e
  INTERFACE get_omi_variant_no
     MODULE PROCEDURE get_omi_variant_no_w0
  END INTERFACE
  !! Ermittle, ob es sich bei einer physikalischen Gr&ouml;&szlig;e um eine
  !! <EM>Magnitude</EM>, also den Betrag eines Vektors handelt
  INTERFACE is_omi_magnitude
     MODULE PROCEDURE is_omi_magnitude_d
  END INTERFACE
  !
  PUBLIC :: create_omi
  PUBLIC :: has_omi_discr_time
  PUBLIC :: has_omi_time_span
  PUBLIC :: nof_omi_discr_time
  PUBLIC :: get_omi_discr_time
  PUBLIC :: get_omi_time_span
  PUBLIC :: get_omi_xyz_x_coord
  PUBLIC :: get_omi_xyz_y_coord
  PUBLIC :: get_omi_xyz_z_coord
  PUBLIC :: get_omi_phyval_name
  PUBLIC :: get_omi_variant_no
  PUBLIC :: is_omi_magnitude
  !
  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Datentypen, Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] lokale Typdefinitionen
  !
  ! [D.2] Konstantwerte (Parameter)
  !
  !! maximale Anzahl der Objekte vom Typ "t_omi_dope" und "t_omi_arg" --
  INTEGER            , PARAMETER :: c_max_omi_dope_idx=1 ! 
  !! Zeiger auf c_dope_id(:) ind mod_b_omi_dope
  INTEGER            , PARAMETER :: c_omi_dope_idx(c_max_omi_dope_idx)=(/ 9 /) ! 
  !
  ! --------------------------------------------------------------------
  !! Name des Moduls
  CHARACTER (LEN=10) , PRIVATE, PARAMETER :: c_modname='m_ipds_omi' ! 
  !! max. Anzahl m&ouml;glicher unterschiedlicher Koordinaten-Objekt-Typen
  INTEGER            , PARAMETER :: c_max_xyz=3 ! 
  !! Id's der Koordinatenobjekte
  CHARACTER (LEN=14) , PARAMETER :: c_xyz_id(c_max_xyz) = (/ & ! 
       '2d-xy-nolay   ', & ! [1] ! fuer ein von aussen vorgegebenes Gitter
       '2d-xy-nolay-mp', & ! [2] ! fuer evtl. vorhandene Messpositionen
       '2d-xy-nolay-re' /) ! [3] ! fuer evtl. vorhandene regionale Werte
  !! Id's der zugeordneten Gitter-Koordinaten-Objekte
  CHARACTER (LEN=03) , PARAMETER :: c_g_exch_id(c_max_xyz) = (/ & ! 
       'xy ', 'xy ', 'xy ' /) ! 
  !! Beschreibungen der Koordinatenobjekte
  CHARACTER (LEN=44) , PARAMETER :: c_xyz_descr(c_max_xyz) = (/ & ! 
       !1234567890 234567890 234567890 234567890 234567890
       'xy-coordinates, no depth, external grid     ', & ! [1]
       'xy-coordinates, no depth, sampling points   ', & ! [2]
       'xy-coordinates, z-layers, regional values   ' /) ! [3]
  !! maximale Anzahl der zu erzeugenden Indexlisten-Objekte f&uuml;r Sampling Points
  INTEGER            , PARAMETER :: c_max_mespos_ind_id=1 ! 
  !! Namen der zu erzeugenden Indexliste-Objekte f&uuml;r Sampling Points
  CHARACTER (LEN=17) , PARAMETER :: c_mespos_ind_id(c_max_mespos_ind_id) = (/ & ! 
       'AllLocations/xyPt' /)
  !! Beschreibungen der zu erzeugenden Indexlisten-Objekte f&uuml;r Sampling Points
  CHARACTER (LEN=19) , PARAMETER :: c_mespos_ind_descr(c_max_mespos_ind_id) = (/ & ! 
       'all sampling points' /)
  !! maximale Anzahl der zu erzeugenden Indexlisten-Objekte f&uuml;r Regional Values
  INTEGER            , PARAMETER :: c_max_region_ind_id=1 ! 
  !! Namen der zu erzeugenden Indexliste-Objekte f&uuml;r Regional Values
  CHARACTER (LEN=19) , PARAMETER :: c_region_ind_id(c_max_region_ind_id) = (/ & ! 
       'HorizontalGrid/xyPg' /)
  !! Beschreibungen der zu erzeugenden Indexlisten-Objekte f&uuml;r Regional Values
  CHARACTER (LEN=11) , PARAMETER :: c_region_ind_descr(c_max_region_ind_id) = (/ & ! 
       'all regions' /)
  !
  ! [D.3] lokale Schnittstellen
  !
  !! Erzeuge die <EM>Quantities</EM> f&uuml;r ein Datenobjekt
  INTERFACE create_omi_quant
     MODULE PROCEDURE create_omi_quant_w0
  END INTERFACE
  !! Erzeuge <EM>Koordinatenobjekte</EM> f&uuml;r ein Datenobjekt
  INTERFACE create_omi_xyz
     MODULE PROCEDURE create_omi_xyz_w0
  END INTERFACE
  !! Erzeuge <EM>Verweislisten</EM> f&uuml;r ein Datenobjekt
  INTERFACE create_omi_ind
     MODULE PROCEDURE create_omi_ind_w0
  END INTERFACE
  !! Erzeuge <EM>ElementSets</EM> f&uuml;r ein Datenobjekt
  INTERFACE create_omi_ele
     MODULE PROCEDURE create_omi_ele_w0
  END INTERFACE
  !! Erzeuge <EM>DataOperations</EM> f&uuml;r ein Datenobjekt
  INTERFACE create_omi_dope
     MODULE PROCEDURE create_omi_dope_w0
  END INTERFACE
  !! Erzeuge <EM>ExchangeItems</EM> f&uuml;r ein Datenobjekt
  INTERFACE create_omi_exch
     MODULE PROCEDURE create_omi_exch_w0
  END INTERFACE
  !! Erzeuge <EM>Span</EM> f&uuml;r ein Datenobjekt
  INTERFACE create_omi_span
     MODULE PROCEDURE create_omi_span_w0
  END INTERFACE
  !! Erzeuge <EM>Stamp</EM> f&uuml;r ein Datenobjekt
  INTERFACE create_omi_stamp
     MODULE PROCEDURE create_omi_stamp_w0
  END INTERFACE
  !
  !! Pr&uuml;fe, ob die Koordinatensystem &uuml;bereinstimmen
  INTERFACE ok_grid_space_identity
     MODULE PROCEDURE ok_grid_space_identity_d
  END INTERFACE
  !! Ermittle die Indikator-Liste f&uuml;r die zu erstellenden Koordinatenobjekte "c_xyz_id(:)"
  INTERFACE get_xyz_id_ind
     MODULE PROCEDURE get_xyz_id_ind_d
  END INTERFACE
  !! Ermittle die Anzahl der zu erstellenden Koordinatenobjekte aus "c_xyz_id(:)"
  INTERFACE get_xyz_id_count
     MODULE PROCEDURE get_xyz_id_count_d
  END INTERFACE
  !! Ermittle den Listenindex eines lfd. zu erzeugenden Koordinatenobjekts auf "c_xyz_id(:)"
  INTERFACE get_xyz_id_idx
     MODULE PROCEDURE get_xyz_id_idx_d
  END INTERFACE
  !! ermittle den Zeiger auf das Gitter-Koordinaten-Objekt
  INTERFACE get_grid_xyz_ref
     MODULE PROCEDURE get_grid_xyz_ref_d
  END INTERFACE
  !! ermittle die Anzahl der zu erzeugenden ElementSetIds <BR>
  !! a) f&uuml;r viele Variablen
  INTERFACE get_exch_ele_id_count
     MODULE PROCEDURE get_exch_ele_id_count_1
  END INTERFACE
  !! erzeuge die Liste mit allen m&ouml;glichen zu erzeugenden ElementSet-Ids <BR>
  !! a) f&uuml;r viele Variablen
  INTERFACE get_exch_all_ele_id
     MODULE PROCEDURE get_exch_all_ele_id_1
  END INTERFACE
  !! Ermittle die Indkatorliste f&uuml;r die tats&auml;chlich zu erzeugenden ElementSet-Ids
  !! a) f&uuml;r eine bestimmte Variable <BR>
  !! b) f&uuml;r viele Variablen
  INTERFACE get_exch_ele_id_ind
     MODULE PROCEDURE get_exch_ele_id_ind_d
     MODULE PROCEDURE get_exch_ele_id_ind_1
  END INTERFACE
  !! Ermittle den Namen der zu erzeugenden ElementSet-Id anhand der laufenden Nummer <BR>
  !! a) f&uuml;r viele Variablen
  INTERFACE get_exch_ele_id
     MODULE PROCEDURE get_exch_ele_id_1
  END INTERFACE
  !! Ermittle den Zeiger auf das Daten-Koordinaten-Objekt das zum Erstellen
  !! eines bestimmten Daten-Index-Listen-Objekts ben&ouml;tigt wird
  INTERFACE get_idx_d
     MODULE PROCEDURE get_idx_d_d 
  END INTERFACE
  !! Ermittle den Zeiger auf das Gitter-Index-Listen-Objekt das zum Erstellen
  !! eines bestimmten Daten-Index-Listen-Objekts ben&ouml;tigt wird
  INTERFACE get_idx_g
     MODULE PROCEDURE get_idx_g_d
  END INTERFACE
  !! Ermittle die Anzahl der zu erzeugenden Austauschgr&ouml;&szlig;en <BR>
  !! a) f&uuml;r viele Variablen
  INTERFACE get_exch_count
     MODULE PROCEDURE get_exch_count_1
  END INTERFACE
  !! Ermittle die Indikatorliste f&uuml;r die zum Erzeugen von Austauschgr&ouml;&szlig;en
  !! zu verwendenden ElementSets <BR>
  !! a) f&uuml;r eine Variable
  INTERFACE get_exch_ele_ind
     MODULE PROCEDURE get_exch_ele_ind_0
  END INTERFACE
  !! Ermittle die Indikatorliste f&uuml;r die zum Erzeugen von Austauschgr&ouml;&szlig;en
  !! zu verwendenden Quantities <BR>
  !! a) f&uuml;r eine Variable
  INTERFACE get_exch_quant_ind
     MODULE PROCEDURE get_exch_quant_ind_0
  END INTERFACE
  !! Ermittle den Code einer Austauschgr&ouml;&szlig;e
  INTERFACE get_code_from_exch_idx
     MODULE PROCEDURE get_code_from_exch_idx_0
  END INTERFACE
  !! Ermittle das Koordinatenobjekt einer Austauschgr&ouml;&szlig;e
  INTERFACE get_exch_xyz
     MODULE PROCEDURE get_exch_xyz_d
  END INTERFACE
  !! Ermittle das Quantity-Objekt einer Austauschgr&ouml;&szlig;e
  INTERFACE get_exch_quant
     MODULE PROCEDURE get_exch_quant_d
  END INTERFACE
  !
  !! Erzeuge Koordinatenobjekt f&uuml;r externes Gitter ohne Schichtstruktur
  INTERFACE create_omi_xyz_2d
     MODULE PROCEDURE create_omi_xyz_2d_d
  END INTERFACE
  !! Erzeuge Koordinatenobjekt f&uuml;r Sampling Points ohne Schichtstruktur
  INTERFACE create_omi_xyz_2d_mp
     MODULE PROCEDURE create_omi_xyz_2d_mp_d
  END INTERFACE
  !! Erzeuge Koordinatenobjekt f&uuml;r Regional Values ohne Schichtstruktur
  INTERFACE create_omi_xyz_2d_re
     MODULE PROCEDURE create_omi_xyz_2d_re_d
  END INTERFACE
  !! Erzeuge eine Komponente der Daten-Index-Listen-Objekte aus dem externen Gitter
  INTERFACE create_omi_l_ind_g
     MODULE PROCEDURE create_omi_l_ind_g_d
  END INTERFACE
  !! Erzeuge eine Komponente der Daten-Index-Listen-Objekte f&uuml;r die ipds-Geometrien
  INTERFACE create_omi_l_ind_i
     MODULE PROCEDURE create_omi_l_ind_i_d
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
  ! Oeffentliche Methoden oder Methoden mit Zugriff ueber PUBLIC Interfaces
  !
  !! Erzeuge alle OpenMI-konformen Komponenten des aktuellen Datenobjektes "this" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE create_omi_w0 ( this, g_exch, id, n )
    !! aktuelles Datenobjekt
    TYPE (t_ipds)  , POINTER    :: this      !
    !! Austauschgr&ouml;&szlig;en des Gitters
    TYPE (t_omi_exch) , INTENT(IN) :: g_exch(:) ! 
    !! Liste mit den Id's der Datenobjekte die zu der aktuellen OpenMI-Komponente geh&ouml;ren
    INTEGER           , INTENT(IN) :: id(:)     ! 
    !! aktuelle lfd. Nummer [1,...] des aktuell zu erzeugenden Objekts
    INTEGER           , INTENT(IN) :: n         ! 
    !! Name des Unterprogramms
    CHARACTER (LEN=13) , PARAMETER :: c_upname='create_omi_w0' ! 
    !
    ! [1.1] Erzeugen der Beschreibungen der <EM>Quantities</EM>
    IF ( no_error( ) ) CALL create_omi_quant ( this, id, n )
    ! [1.2] Erzeugen der Koordinaten-Objekte
    IF ( no_error( ) ) CALL create_omi_xyz   ( this, g_exch, id )
    ! [1.3] Erzeugen der Verweislisten-Objekte
    IF ( no_error( ) ) CALL create_omi_ind   ( this, g_exch, id )
    ! [1.4] Erzeugen der ElementSets-Objekte
    IF ( no_error( ) ) CALL create_omi_ele   ( this, g_exch, id )
    ! [1.5] Erzeugen der DataOperations-Objekte
    IF ( no_error( ) ) CALL create_omi_dope  ( this )
    ! [1.6] Erzeugen der ExchangeItems-Objekte
    IF ( no_error( ) ) CALL create_omi_exch  ( this, g_exch )
    ! [1.7] Erzeugen der Zeitangabe-Objekte
    IF ( no_error( ) ) CALL create_omi_span  ( this )
    !
  END SUBROUTINE create_omi_w0
  !
  !! Ermittle, ob f&uuml;r eine Austauschgr&ouml;&szlig;e diskrete Zeitangaben
  !! vorhanden sind oder nicht <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION has_omi_discr_time_w0 ( this, exch_idx ) &
       RESULT( res )
    !! aktuelles Datenobjekt
    TYPE (t_ipds) , POINTER   :: this     ! 
    !! Zeiger auf die Position der Austauschgr&ouml;&szlig;e in dem
    !! aktuellen Arbeitsobjekt
    INTEGER      , INTENT(IN) :: exch_idx ! 
    !! Ergebnis : true/false
    LOGICAL                   :: res      ! 
    !! Hilfsvariable
    TYPE (t_att) , POINTER    :: p_att(:) ! 
    !
    p_att => get_att_object ( this )
    IF ( ASSOCIATED( p_att ) ) THEN
       res = ( is_phy_quant_synoptic ( get_code_from_exch_idx( this, exch_idx ) ) &
               .AND. ANY( get_att_idx( p_att, c_att_name((/6,7/)) ) > 0 ) )
    ELSE
       res = .false.
    END IF
    NULLIFY( p_att )
    !
  END  FUNCTION has_omi_discr_time_w0 
  !
  !! Ermittle, ob f&uuml;r eine Austauschgr&ouml;&szlig;e eine Zeitspannenangabe
  !! vorhanden ist oder nicht <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION has_omi_time_span_w0 ( this, exch_idx ) &
       RESULT( res )
    !! aktuelles Datenobjekt
    TYPE (t_ipds) , POINTER    :: this     ! 
    !! Zeiger auf die Position der Austauschgr&ouml;&szlig;e in dem
    !! aktuellen Arbeitsobjekt
    INTEGER       , INTENT(IN) :: exch_idx ! 
    !! Ergebnis : true/false
    LOGICAL                    :: res      ! 
    !! Hilfsvariable
    TYPE (t_att) , POINTER     :: p_att(:) ! 
    !
    p_att => get_att_object ( this )
    IF ( ASSOCIATED( p_att ) ) THEN
       res = ( ( is_phy_quant_period   ( get_code_from_exch_idx( this, exch_idx ) ) .OR. &
                 is_phy_quant_synoptic ( get_code_from_exch_idx( this, exch_idx ) ) )    &
               .AND. ALL( get_att_idx( p_att, c_att_name((/3,4/)) ) > 0 ) )
    ELSE
       res = .false.
    END IF
    NULLIFY( p_att )
    !
  END  FUNCTION has_omi_time_span_w0
  !
  !! Ermittle, f&uuml;r eine Austauschgr&ouml;&szlig;e die Anzahl der
  !! diskreten Zeitangaben <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION nof_omi_discr_time_w0 ( this, exch_idx ) &
       RESULT( res )
    !! aktuelles Datenobjekt
    TYPE (t_ipds) , POINTER :: this     ! 
    !! Zeiger auf die Position der Austauschgr&ouml;&szlig;e in dem
    !! aktuellen Arbeitsobjekt
    INTEGER      , INTENT(IN)  :: exch_idx ! 
    !! Ergebnis : Anzahl diskreter Zeitangaben
    INTEGER                    :: res      ! 
    !! Hilfsvariable
    TYPE (t_att) , POINTER     :: p_att(:) ! 
    INTEGER      , PARAMETER   :: c_idx(2)=(/6,7/) ! 
    INTEGER                    :: i, idx(2)        ! 
    !
    res = 0
    IF ( has_omi_discr_time_w0 ( this, exch_idx ) ) THEN
       p_att => get_att_object ( this )
       IF ( ASSOCIATED( p_att ) ) THEN
          idx(:) = get_att_idx( p_att, c_att_name(c_idx(:)) )
          DO i=1,SIZE(idx)
             IF ( idx(i) <= 0 ) CYCLE
             res = get_att_nof_values( p_att(idx(i)) )
          END DO
       END IF
    END IF
    NULLIFY( p_att )
    !
  END  FUNCTION nof_omi_discr_time_w0
  !
  !! Ermittle, f&uuml;r eine Austauschgr&ouml;&szlig;e eine
  !! bestimmte diskreten Zeitangaben <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION get_omi_discr_time_w0 ( this, exch_idx, time_idx ) &
       RESULT( res )
    !! aktuelles Datenobjekt
    TYPE (t_ipds) , POINTER    :: this     ! 
    !! Zeiger auf die Position der Austauschgr&ouml;&szlig;e in dem
    !! aktuellen Arbeitsobjekt
    INTEGER      , INTENT(IN)  :: exch_idx ! 
    !! Zeiger auf die Position des gew&uuml;nschten Termins
    INTEGER      , INTENT(IN)  :: time_idx ! 
    !! Ergebnis : Zeitangabe
    TYPE (t_omi_stamp)         :: res      ! 
    !! Hilfsvariable
    TYPE (t_omi_stamp) , POINTER     :: p_stamp(:)       ! 
    !
    CALL new_omi_stamp ( res )
    IF ( has_omi_discr_time_w0 ( this, exch_idx ) ) THEN
       p_stamp => get_stamp_object ( this ) 
       IF ( .NOT. ASSOCIATED( p_stamp ) ) THEN
          CALL create_omi_stamp ( this )
          p_stamp => get_stamp_object ( this )
       END IF
       IF ( ASSOCIATED( p_stamp ) ) THEN
          IF ( time_idx >= 1 .AND. time_idx <= SIZE(p_stamp) ) THEN
             res = p_stamp(time_idx) 
          END IF
       END IF
    END IF
    NULLIFY( p_stamp )
    !
  END  FUNCTION get_omi_discr_time_w0
  !
  !! Ermittle, f&uuml;r eine Austauschgr&ouml;&szlig;e die
  !! Angabe eines Zeitraums <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION get_omi_time_span_w0 ( this, exch_idx ) &
       RESULT( res )
    !! aktuelles Datenobjekt
    TYPE (t_ipds) , POINTER :: this     ! 
    !! Zeiger auf die Position der Austauschgr&ouml;&szlig;e in dem
    !! aktuellen Arbeitsobjekt
    INTEGER      , INTENT(IN)  :: exch_idx ! 
    !! Ergebnis : Zeitspanne
    TYPE (t_omi_span)          :: res      ! 
    !! Hilfsvariable
    TYPE (t_att)      , POINTER     :: p_att(:)         ! 
    TYPE (t_datetime)               :: l_datetime(2)    ! 
    INTEGER           , PARAMETER   :: c_idx(2)=(/3,4/) ! 
    INTEGER                         :: i, idx(2)        ! 
    !
    CALL new_omi_span ( res )
    IF ( has_omi_time_span_w0 ( this, exch_idx ) ) THEN
       p_att => get_att_object ( this )
       IF ( ASSOCIATED( p_att ) ) THEN
          idx(:) = get_att_idx( p_att, c_att_name(c_idx(:)) )
          DO i=1,SIZE(idx)
             IF ( idx(i) <= 0 ) CYCLE
             l_datetime(i:i) = get_att_ch_as_datetime( p_att(idx(i)) )
          END DO
       END IF
       CALL set_omi_span_start ( res, l_datetime(1) )
       CALL set_omi_span_end   ( res, l_datetime(2) )
    END IF
    NULLIFY( p_att )
    !
  END FUNCTION get_omi_time_span_w0
  !
  !! Ermittle die x-Koordinaten f&uuml;r eine Austauschgr&ouml;&szlig;e des 
  !! Datenobjekts <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_x_coord_w0 ( this, exch_idx ) & 
       RESULT( res )
    !! aktuelles Datenobjekt
    TYPE (t_ipds)   , POINTER    :: this ! 
    !! Zeiger auf die Austauschgr&ouml;&szlig;e des aktuellen Arbeitsobjekts
    INTEGER         , INTENT(IN) :: exch_idx ! 
    !! Ergebnis: Zeiger auf die x-Koordinaten des Koordinatenobjektes
    REAL (KIND=Double) , POINTER :: res(:) ! 
    !! Hilfsvariablen
    TYPE (t_omi_xyz)   , POINTER :: p_xyz ! 
    !
    NULLIFY(res)
    p_xyz => get_exch_xyz ( this, exch_idx )
    IF ( ASSOCIATED(p_xyz) ) res => get_omi_xyz_x_ref ( p_xyz )
    NULLIFY( p_xyz )
    !
  END FUNCTION get_omi_xyz_x_coord_w0
  !
  !! Ermittle die y-Koordinaten f&uuml;r eine Austauschgr&ouml;&szlig;e des 
  !! Datenobjekts <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_y_coord_w0 ( this, exch_idx ) & 
       RESULT( res )
    !! aktuelles Datenobjekt
    TYPE (t_ipds)   , POINTER    :: this ! 
    !! Zeiger auf die Austauschgr&ouml;&szlig;e des aktuellen Arbeitsobjekts
    INTEGER         , INTENT(IN) :: exch_idx ! 
    !! Ergebnis: Zeiger auf die y-Koordinaten des Koordinatenobjektes
    REAL (KIND=Double) , POINTER :: res(:) ! 
    !! Hilfsvariablen
    TYPE (t_omi_xyz)   , POINTER :: p_xyz ! 
    !
    NULLIFY(res)
    p_xyz => get_exch_xyz ( this, exch_idx )
    IF ( ASSOCIATED(p_xyz) ) res => get_omi_xyz_y_ref ( p_xyz )
    NULLIFY( p_xyz )
    !
  END FUNCTION get_omi_xyz_y_coord_w0
  !
  !! Ermittle die z-Koordinaten f&uuml;r eine Austauschgr&ouml;&szlig;e des 
  !! Datenobjekts <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_omi_xyz_z_coord_w0 ( this, exch_idx ) & 
       RESULT( res )
    !! aktuelles Datenobjekt
    TYPE (t_ipds)   , POINTER    :: this ! 
    !! Zeiger auf die Austauschgr&ouml;&szlig;e des aktuellen Arbeitsobjekts
    INTEGER         , INTENT(IN) :: exch_idx ! 
    !! Ergebnis: Zeiger auf die z-Koordinaten des Koordinatenobjektes
    REAL (KIND=Double) , POINTER :: res(:) ! 
    !! Hilfsvariablen
    TYPE (t_omi_xyz)   , POINTER :: p_xyz ! 
    !
    NULLIFY(res)
    p_xyz => get_exch_xyz ( this, exch_idx )
    IF ( ASSOCIATED(p_xyz) ) THEN
       IF ( is_omi_xyz_three_dimensional( p_xyz ) ) res => get_omi_xyz_z_ref ( p_xyz )
    END IF
    NULLIFY( p_xyz )
    !
  END FUNCTION get_omi_xyz_z_coord_w0
  !
  !! ermittle den Namen der physikalischen Gr&ouml;&szlig;e einer Austauschgr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen
  FUNCTION get_omi_phyval_name_w0 ( this, exch_idx ) &
       RESULT( res )
    !! aktuelles Datenobjekt
    TYPE (t_ipds)        , POINTER    :: this     ! 
    !! Zeiger auf die Austauschgr&ouml;&szlig;e des aktuellen Arbeitsobjekts
    INTEGER              , INTENT(IN) :: exch_idx ! 
    !! Ergebnis: Name der physikalischen Gr&ouml;&szlig;e
    CHARACTER (LEN=c_len_phyval_name) :: res      ! 
    !! Hilfsvariable
    TYPE (t_omi_quant)      , POINTER :: p_quant ! 
    !
    res = REPEAT( ' ', LEN(res) )
    res = 'undefined'
    p_quant => get_exch_quant ( this, exch_idx )
    IF ( ASSOCIATED( p_quant ) ) THEN
       res = get_phy_quant_descr( get_code_from_omi_quant( p_quant ) )
    END IF
    NULLIFY(p_quant)
    !
  END FUNCTION get_omi_phyval_name_w0
  !
  !! ermittle den Nummer der Datenvariante einer Austauschgr&ouml;&szlig;e <BR>
  !! Funktion erzeugt keine Fehlermeldungen
  FUNCTION get_omi_variant_no_w0 ( this, exch_idx ) &
       RESULT( res )
    !! aktuelles Datenobjekt
    TYPE (t_ipds)        , POINTER    :: this     ! 
    !! Zeiger auf die Austauschgr&ouml;&szlig;e des aktuellen Arbeitsobjekts
    INTEGER              , INTENT(IN) :: exch_idx ! 
    !! Ergebnis: Nummer der Datenvariante
    INTEGER                           :: res      ! 
    !! Hilfsvariable
    TYPE (t_omi_quant)      , POINTER :: p_quant ! 
    !
    res = c_undef_omi_quant_int
    p_quant => get_exch_quant ( this, exch_idx )
    IF ( ASSOCIATED( p_quant ) ) THEN
       res = get_var_no_from_omi_quant( p_quant )
    END IF
    NULLIFY(p_quant)
    !
  END FUNCTION get_omi_variant_no_w0
  !
  !! Ermittle, ob es sich bei einer Austauschg&ouml;&szlig;e um eine <EM>Magnitude</EM>,
  !! also den Betrag eines Vektors handelt <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_omi_magnitude_d ( this, exch_idx ) &
       RESULT( res )
    !! aktuelles Datenobjekt
    TYPE (t_ipds)        , POINTER    :: this     ! 
    !! Zeiger auf die Austauschgr&ouml;&szlig;e des aktuellen Arbeitsobjekts
    INTEGER              , INTENT(IN) :: exch_idx ! 
    !! Ergebnis: Austauschgr&ouml;szlig;e ist eine <EM>Magnitude</EM>
    LOGICAL :: res ! 
    !! Hilfsvariable
    TYPE (t_omi_quant)      , POINTER :: p_quant ! 
    !
    res = .false.
    p_quant => get_exch_quant ( this, exch_idx )
    IF ( ASSOCIATED( p_quant ) ) THEN
       res = is_omi_quant_magnitude ( p_quant )
    END IF
    NULLIFY(p_quant)
    !
  END FUNCTION is_omi_magnitude_d
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
  ! ----------------------------------------------------------------------
  ! >>> Quantities <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ! ----------------------------------------------------------------------
  !! Erzeuge die Beschreibung der OpenMI-konformen <EM>Quantities</EM> 
  !! f&uuml;r das aktuelle Datenobjekt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE create_omi_quant_w0 ( this, id, n )
    !! aktuelles Datenobjekt
    TYPE (t_ipds)   , POINTER   :: this !
    !! Liste mit den Id's der Datenobjekte die zu der aktuellen OpenMI-Komponente geh&ouml;ren
    INTEGER           , INTENT(IN) :: id(:)     ! 
    !! aktuelle lfd. Nummer [1,...] des aktuell zu erzeugenden Objekts
    INTEGER           , INTENT(IN) :: n         ! 
    !! Name des Unterprogramms
    CHARACTER (LEN=19) , PARAMETER :: c_upname='create_omi_quant_w0' ! 
    !! Hilfsvariablen
    TYPE (t_dim)       , POINTER     :: p_dim(:)   !
    TYPE (t_var)       , POINTER     :: p_var(:)   !
    TYPE (t_att)       , POINTER     :: p_att(:)   !
    TYPE (t_omi_quant) , ALLOCATABLE :: l_quant(:) ! 
    INTEGER                          :: nq         ! 
    !
    p_dim => get_dim_object ( this )
    IF ( .NOT. ASSOCIATED(p_dim) ) THEN
       CALL derive_dim ( this )
       p_dim => get_dim_object ( this )
    END IF
    p_var => get_var_object ( this )
    IF ( .NOT. ASSOCIATED(p_var) ) THEN
       CALL derive_var ( this )
       p_var => get_var_object ( this )
    END IF
    p_att => get_att_object ( this )
    IF ( .NOT. ASSOCIATED(p_att) ) THEN
       CALL derive_att ( this )
       p_att => get_att_object ( this )
    END IF
    !
    IF ( ALL(ok_att(p_att,p_var)) .AND. ALL(ok_var(p_var,p_dim)) .AND. ALL(ok_dim(p_dim)) ) THEN
       nq = SUM( get_max_omi_quant(p_var(:),p_dim(:)) )
       IF ( nq > 0 ) THEN
          ALLOCATE( l_quant(nq) )
          IF ( no_error( ) ) CALL new_omi_quant      ( l_quant(:) )
          IF ( no_error( ) ) CALL set_omi_quant      ( l_quant(:), p_var(:), p_dim(:), p_att(:) )
          IF ( no_error( ) ) CALL setup_quant_object ( this, l_quant(:) )
          IF ( no_error( ) ) CALL kill_omi_quant     ( l_quant(:) )
          DEALLOCATE( l_quant )
       END IF
    END IF
    !
    NULLIFY( p_dim, p_var, p_att )
    !
  END SUBROUTINE create_omi_quant_w0
  !
  ! ----------------------------------------------------------------------
  ! >>> Koordinaten-Objekte <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ! ----------------------------------------------------------------------
  !
  !! Erzeuge die OpenMI-konformen Koordinaten-Objekte f&uuml;r das 
  !! aktuelle Datenobjekt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE create_omi_xyz_w0 ( this, g_exch, id )
    !! aktuelles Datenobjekt
    TYPE (t_ipds)      , POINTER     :: this      !
    !! Austauschgr&ouml;&szlig;en des Gitters
    TYPE (t_omi_exch)  , INTENT(IN)  :: g_exch(:) ! 
    !! Liste mit den Id's der Datenobjekte die zu der aktuellen OpenMI-Komponente geh&ouml;ren
    INTEGER            , INTENT(IN)  :: id(:)     ! 
    !! Name des Unterprogramms
    CHARACTER (LEN=17) , PARAMETER   :: c_upname='create_omi_xyz_w0' ! 
    !! Hilfsvariablen
    REAL (KIND=Double) , PARAMETER   :: c_hlimit=0.01_Double         ! 
    TYPE (t_var)       , POINTER     :: p_var(:)  ! 
    TYPE (t_att)       , POINTER     :: p_att(:)  ! 
    TYPE (t_omi_xyz)   , POINTER     :: g_xyz     ! 
    TYPE (t_omi_xyz)   , ALLOCATABLE :: l_xyz(:)  ! 
    TYPE (t_omi_space)               :: space     ! 
    INTEGER                          :: i, idx    ! 
    !
    IF ( get_xyz_id_count( this, g_exch ) > 0 ) THEN
       p_var => get_var_object( this )
       p_att => get_att_object( this )
       ! [1.1] Koordinatenobjekte anlegen
       ALLOCATE( l_xyz(get_xyz_id_count(this,g_exch)) )
       CALL new_omi_xyz( l_xyz )
       ! [1.2] Koordinatensystemdefinition
       CALL new_omi_space    ( space ) 
       CALL set_omi_space_id ( space, 'Cartesian_Projected[Easting,Northing]Vertical[Depth]' )
       ! [1.3] Koordinatenobjekte mit Daten belegen
       DO i=1,SIZE(l_xyz)
          IF ( any_error( ) ) EXIT
          idx   =  get_xyz_id_idx     ( this, g_exch, i )
          g_xyz => get_grid_xyz_ref   ( this, g_exch, c_g_exch_id(idx) )
          IF ( ok_grid_space_identity ( this, g_xyz, space ) ) THEN
             SELECT CASE ( idx )
             CASE ( 1 ) ! externes Gitter
                CALL create_omi_xyz_2d    ( idx, g_xyz, space, c_hlimit, l_xyz(i) )
             CASE ( 2 ) ! Sampling Points
                CALL create_omi_xyz_2d_mp ( idx, this , space, c_hlimit, l_xyz(i) )
             CASE ( 3 ) ! Regional Values
                CALL create_omi_xyz_2d_re ( idx, this , space, c_hlimit, l_xyz(i) )
             END SELECT
          END IF
       END DO
       IF ( no_error( ) ) THEN
          CALL setup_xyz_object ( this, l_xyz )
          CALL kill_omi_xyz ( l_xyz )
          DEALLOCATE ( l_xyz )
       END IF
    ELSE
       CALL setup_error_act ( all_errors(:), -30000, c_upname, c_modname )
       CALL setup_error_act ( '<objectname1>', TRIM(this%name) )
    END IF
    NULLIFY( p_var, p_att, g_xyz )
    !
  END SUBROUTINE create_omi_xyz_w0
  !
  ! ----------------------------------------------------------------------
  ! >>> Verweisliste-Objekte <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ! ----------------------------------------------------------------------
  !
  !! Erzeuge die OpenMI-konformen Verweislisten-Objekte f&uuml;r das 
  !! aktuelle Datenobjekt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE create_omi_ind_w0 ( this, g_exch, id )
    !! aktuelles Datenobjekt
    TYPE (t_ipds)   , POINTER   :: this !
    !! Austauschgr&ouml;&szlig;en des Gitters
    TYPE (t_omi_exch) , INTENT(IN) :: g_exch(:) ! 
    !! Liste mit den Id's der Datenobjekte die zu der aktuellen OpenMI-Komponente geh&ouml;ren
    INTEGER           , INTENT(IN) :: id(:)    ! 
    !! Name des Unterprogramms
    CHARACTER (LEN=17) , PARAMETER :: c_upname='create_omi_ind_w0' !  
    !! Hilfsvariablen
    INTEGER                              :: i, ni, idx_d, idx_g    ! 
    CHARACTER (LEN=c_len_omi_ele_id)     :: ele_id                 ! 
    TYPE (t_var)           , POINTER     :: p_var(:)               ! 
    TYPE (t_att)           , POINTER     :: p_att(:)               ! 
    TYPE (t_omi_xyz)       , POINTER     :: p_xyz(:)               ! 
    TYPE (t_omi_ind)       , ALLOCATABLE :: l_ind(:)               !        
    !
    p_var => get_var_object( this )
    p_att => get_att_object( this )
    p_xyz => get_xyz_object( this )
    ni    = get_exch_ele_id_count( this, p_xyz, p_var, p_att, g_exch )
    IF ( ni > 0 ) THEN
       ALLOCATE( l_ind(ni) )
       CALL new_omi_ind( l_ind )
       DO i=1,SIZE(l_ind)
          IF ( any_error( ) ) EXIT
          ele_id = get_exch_ele_id( this, p_xyz, p_var, p_att, g_exch, i )
          idx_d  = get_idx_d ( this, ele_id, p_xyz  ) 
          idx_g  = get_idx_g ( this, ele_id, g_exch )
          IF ( no_error( ) ) THEN
             IF ( idx_g > 0 ) THEN
                CALL create_omi_l_ind_g ( this, ele_id, p_xyz(idx_d), g_exch(idx_g), l_ind(i) )
             ELSE
                CALL create_omi_l_ind_i ( this, ele_id, p_xyz(idx_d), l_ind(i) )
             END IF
          END IF
       END DO
       CALL setup_ind_object ( this, l_ind )
       CALL kill_omi_ind     ( l_ind )
       DEALLOCATE            ( l_ind )
    ELSE
       CALL setup_error_act ( all_errors(:), -30020, c_upname, c_modname )
       CALL setup_error_act ( '<objectname1>', TRIM(this%name) )
    END IF
    NULLIFY( p_var, p_att, p_xyz )
    !
  END SUBROUTINE create_omi_ind_w0
  !
  ! ----------------------------------------------------------------------
  ! >>> ElementSets-Objekte <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ! ----------------------------------------------------------------------
  !
  !! Erzeuge die OpenMI-konformen Koordinaten-Objekte f&uuml;r das 
  !! aktuelle Datenobjekt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE create_omi_ele_w0 ( this, g_exch, id )
    !! aktuelles Datenobjekt
    TYPE (t_ipds)   , POINTER   :: this                            !
    !! Austauschgr&ouml;&szlig;en des Gitters
    TYPE (t_omi_exch) , INTENT(IN) :: g_exch(:)                       ! 
    !! Liste mit den Id's der Datenobjekte die zu der aktuellen OpenMI-Komponente geh&ouml;ren
    INTEGER           , INTENT(IN) :: id(:)    ! 
    !! Hilfsvariablen
    INTEGER                          :: i, ni, idx_d, idx_i ! 
    CHARACTER (LEN=c_len_omi_ele_id) :: ele_id   ! 
    TYPE (t_var)           , POINTER :: p_var(:) ! 
    TYPE (t_att)           , POINTER :: p_att(:) ! 
    TYPE (t_omi_ind)       , POINTER :: p_ind(:), s_ind ! 
    TYPE (t_omi_xyz)       , POINTER :: p_xyz(:), s_xyz ! 
    TYPE (t_omi_ele)   , ALLOCATABLE :: l_ele(:) ! 
    !
    p_var => get_var_object ( this )
    p_att => get_att_object ( this )
    p_xyz => get_xyz_object ( this )
    p_ind => get_ind_object ( this )
    ni    = get_exch_ele_id_count( this, p_xyz, p_var, p_att, g_exch )
    IF ( ni > 0 ) THEN
       ALLOCATE( l_ele(ni) )
       CALL new_omi_ele ( l_ele )
       DO i=1,ni
          IF ( any_error ( ) ) EXIT
          ele_id = get_exch_ele_id ( this, p_xyz, p_var, p_att, g_exch, i )
          idx_d  = get_idx_d ( this, ele_id, p_xyz  ) 
          idx_i  = get_omi_ind_idx ( p_ind, ele_id )
          IF ( no_error( ) .AND. idx_d > 0 .AND. idx_i > 0 ) THEN
             s_xyz => p_xyz(idx_d)
             s_ind => p_ind(idx_i)
             CALL set_omi_ele_auto ( l_ele(i), s_xyz, s_ind, ' ' )
          END IF
       END DO
       CALL setup_ele_object ( this, l_ele )
       CALL kill_omi_ele ( l_ele )
       DEALLOCATE( l_ele )
    END IF
    NULLIFY ( p_xyz, p_var, p_att, p_ind, s_xyz, s_ind )
    !
  END SUBROUTINE create_omi_ele_w0
  !
  ! ----------------------------------------------------------------------
  ! >>> DataOperations-Objekte <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ! ----------------------------------------------------------------------
  !
  !! Erzeuge die OpenMI-konformen DataOperations-Objekte f&uuml;r das 
  !! aktuelle Datenobjekt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE create_omi_dope_w0 ( this )
    !! aktuelles Datenobjekt
    TYPE (t_ipds)    , POINTER   :: this !
    !! Hilfsvariablen
    TYPE (t_omi_dope)  :: dope(c_max_omi_dope_idx) ! 
    REAL (KIND=Double) :: m_d(1)                   ! 
    INTEGER            :: i                        ! 
    !
    CALL new_omi_dope ( dope(:) )
    DO i=1,SIZE(dope)
       IF ( i == 1 ) THEN
          m_d(:) = c_missing_ipds_double
          CALL set_omi_dope_auto( dope(i), c_omi_dope_idx(i), m_d(:) )
       ELSE
          CALL set_omi_dope_auto( dope(i), c_omi_dope_idx(i) )
       END IF
    END DO
    CALL setup_dope_object ( this, dope(:) )
    CALL kill_omi_dope ( dope(:) )
    !
  END SUBROUTINE create_omi_dope_w0
  !
  ! ----------------------------------------------------------------------
  ! >>> ExchangeItems-Objekte <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ! ----------------------------------------------------------------------
  !
  !! Erzeuge die OpenMI-konformen ExchangeItems-Objekte f&uuml;r das 
  !! aktuelle Datenobjekt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE create_omi_exch_w0 ( this, g_exch )
    !! aktuelles Datenobjekt
    TYPE (t_ipds)    , POINTER   :: this !
    !! Austauschgr&ouml;&szlig;en des Gitters
    TYPE (t_omi_exch)  , INTENT(IN) :: g_exch(:)                       ! 
    !! Name des Unterprogramms
    CHARACTER (LEN=18) , PARAMETER :: c_upname='create_omi_exch_w0' ! 
    !! Hilfsvariablen
    INTEGER                          :: i, j, k, nn, nx ! 
    TYPE (t_omi_exch)  , ALLOCATABLE :: l_exch(:)                      ! 
    TYPE (t_omi_quant) , POINTER     :: p_quant(:), s_quant            ! 
    TYPE (t_omi_ele)   , POINTER     :: p_ele(:), s_ele                ! 
    TYPE (t_omi_dope)  , POINTER     :: p_dope(:)                      ! 
    TYPE (t_omi_xyz)   , POINTER     :: p_xyz(:)                       ! 
    TYPE (t_var)       , POINTER     :: p_var(:)                       ! 
    TYPE (t_att)       , POINTER     :: p_att(:)                       ! 
    TYPE (t_dim)       , POINTER     :: p_dim(:)                       ! 
    CHARACTER (LEN=c_len_omi_ele_id) , POINTER :: p_ele_id(:)          ! 
    LOGICAL , ALLOCATABLE            :: l_ele(:), l_quant(:)           ! 
    !
    p_quant   => get_quant_object ( this )
    p_ele     => get_ele_object   ( this )
    p_dope    => get_dope_object  ( this )
    p_xyz     => get_xyz_object   ( this )
    p_var     => get_var_object   ( this )
    p_att     => get_att_object   ( this )
    p_dim     => get_dim_object   ( this )
    p_ele_id  => get_exch_all_ele_id ( this, g_exch ) 
    !
    nx = get_exch_count ( p_xyz, p_var, p_att, p_dim, p_ele_id )
    IF ( nx > 0 ) THEN
       ALLOCATE ( l_exch(nx), l_ele(SIZE(p_ele)), l_quant(SIZE(p_quant)) )
       CALL new_omi_exch  ( l_exch(:) )
       nn = 0
       DO i=1,SIZE(p_var)
          IF ( any_error( ) ) EXIT
          l_ele   = get_exch_ele_ind   ( p_xyz, p_var(i), p_att, p_ele, p_ele_id )
          l_quant = get_exch_quant_ind ( p_var(i), p_att, p_dim, p_quant         )
          IF ( ANY( l_ele ) .AND. ANY( l_quant ) ) THEN
             DO j=1,SIZE(l_ele)
                IF ( .NOT. l_ele(j) ) CYCLE
                DO k=1,SIZE(l_quant)
                   IF ( .NOT. l_quant(k) ) CYCLE
                   nn = nn + 1
                   s_ele   => p_ele(j)
                   s_quant => p_quant(k)
                   CALL set_omi_exch_quant_ref ( l_exch(nn), s_quant )
                   CALL set_omi_exch_ele_ref   ( l_exch(nn), s_ele   )
                   CALL set_omi_exch_dope_ref  ( l_exch(nn), p_dope  )
                   CALL set_omi_exch_role      ( l_exch(nn), 1       ) ! Export
                END DO
             END DO
          END IF
       END DO
       CALL setup_exch_object ( this, l_exch(:) )
       CALL kill_omi_exch ( l_exch(:) )
       DEALLOCATE( l_exch, l_ele, l_quant )
    END IF
    IF ( ASSOCIATED( p_ele_id ) ) DEALLOCATE( p_ele_id )
    NULLIFY( p_quant, p_ele, p_dope, p_xyz, p_var, p_att, p_dim, p_ele_id )
    !
  END SUBROUTINE create_omi_exch_w0
  !
  ! ----------------------------------------------------------------------
  ! >>> Zeitangaben-Objekte <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ! ----------------------------------------------------------------------
  !
  !! Erzeuge die OpenMI-konformen Zeitangaben-Objekte f&uuml;r das 
  !! aktuelle Datenobjekt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE create_omi_span_w0 ( this )
    !! aktuelles Datenobjekt
    TYPE (t_ipds)    , POINTER   :: this    !
    !! Name des Unterprogramms
    CHARACTER (LEN=18) , PARAMETER :: c_upname='create_omi_span_w0' ! 
    !! Hilfsvariable
    INTEGER , PARAMETER :: c_max_idx=4, c_max_dt=2             ! 
    INTEGER , PARAMETER :: c_idx(c_max_idx) = (/ 3, 4, 6, 7 /) ! 
    TYPE (t_dim)       , POINTER     :: p_dim(:)       ! 
    TYPE (t_var)       , POINTER     :: p_var(:)       ! 
    TYPE (t_att)       , POINTER     :: p_att(:)       ! 
    TYPE (t_datetime)                :: span(c_max_dt) ! 
    TYPE (t_datetime)  , ALLOCATABLE :: time(:)        ! 
    TYPE (t_omi_span)                :: l_span         ! 
    LOGICAL                          :: found          ! 
    INTEGER                          :: idx(c_max_idx) ! 
    INTEGER                          :: nt             ! 
    !
    p_var => get_var_object   ( this )
    p_att => get_att_object   ( this )
    p_dim => get_dim_object   ( this )
    idx   = get_att_idx ( p_att, c_att_name(c_idx) )
    found = .false.
    IF      ( ALL( idx(1:2) > 0 ) ) THEN
       span(1:1) = get_att_ch_as_datetime ( p_att(idx(1)) )
       span(2:2) = get_att_ch_as_datetime ( p_att(idx(2)) )
       found     = .true.
    ELSE IF ( idx(4) > 0          ) THEN
       nt      = get_att_nof_values       ( p_att(idx(4)) )
       ALLOCATE( time(nt) )
       time    = get_att_ch_as_datetime   ( p_att(idx(4)) ) 
       span(1) = time(1)
       span(2) = time(nt)
       DEALLOCATE( time )
       found     = .true.
    ELSE IF ( idx(3) > 0          ) THEN
       span(1:1) = get_att_ch_as_datetime ( p_att(idx(3)) )
       span(2:2) = get_att_ch_as_datetime ( p_att(idx(3)) )
       found     = .true.
    END IF
    IF ( found ) THEN
       CALL new_omi_span       ( l_span          )
       CALL set_omi_span_start ( l_span, span(1) )
       CALL set_omi_span_end   ( l_span, span(2) )
       CALL setup_span_object  ( this, l_span    )
       CALL kill_omi_span      ( l_span          )
    END IF
    !
  END SUBROUTINE create_omi_span_w0
  !
  !! Erzeuge die OpenMI-konformen Zeitstempel-Objekte f&uuml;r das 
  !! aktuelle Datenobjekt <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE create_omi_stamp_w0 ( this )
    !! aktuelles Datenobjekt
    TYPE (t_ipds)    , POINTER   :: this    !
    !! Name des Unterprogramms
    CHARACTER (LEN=19) , PARAMETER :: c_upname='create_omi_stamp_w0' ! 
    !! Hilfsvariable
    TYPE (t_att)       , POINTER     :: p_att(:)         ! 
    TYPE (t_datetime)  , ALLOCATABLE :: l_datetime(:)    ! 
    TYPE (t_omi_stamp) , ALLOCATABLE :: l_stamp(:)       ! 
    TYPE (t_omi_stamp) , POINTER     :: p_stamp(:)       ! 
    INTEGER            , PARAMETER   :: c_idx(2)=(/6,7/) ! 
    INTEGER                          :: i, j, idx(2)     ! 
    !
    p_att   => get_att_object   ( this )
    ! erzeugen des Hilfsfeldes "stamp" fuer "this"
    IF ( ASSOCIATED( p_att ) ) THEN
       idx(:) = get_att_idx( p_att, c_att_name(c_idx(:)) )
       DO i=1,SIZE(idx)
          IF ( idx(i) <= 0 ) CYCLE
          ALLOCATE ( l_datetime(get_att_nof_values(p_att(idx(i)))) )
          ALLOCATE ( l_stamp(get_att_nof_values(p_att(idx(i))))    )
          l_datetime(:) = get_att_ch_as_datetime( p_att(idx(i)) )
          DO j=1,SIZE(l_datetime)
             CALL set_omi_stamp_modjulianday ( l_stamp(j), l_datetime(j) )
          END DO
          CALL setup_stamp_object ( this, l_stamp )
          DEALLOCATE ( l_datetime, l_stamp )
       END DO
    END IF
    NULLIFY( p_att )
    !
  END SUBROUTINE create_omi_stamp_w0
  !
  ! -----------------------------------------------------------------------
  ! >>> PRIVATE-OK-Methoden <<<
  ! -----------------------------------------------------------------------
  !
  !! Pr&uuml;fe die Identitit&auml;t der Koordinatensysteme <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION ok_grid_space_identity_d ( this, xyz, space ) &
       RESULT( res )
    !! aktuelles Arbeitsobjekt
    TYPE (t_ipds)      , POINTER    :: this  ! 
    !! Koordinatenobjekt
    TYPE (t_omi_xyz)   , INTENT(IN) :: xyz   ! 
    !! Koordinatensystembeschreibung
    TYPE (t_omi_space) , INTENT(IN) :: space !
    !! Testergebnis
    LOGICAL                         :: res   ! 
    !! Name des Unterprogramms
    CHARACTER (LEN=24) , PARAMETER :: c_upname='ok_grid_space_identity_d' ! 
    !
    IF ( eq_omi_space( space, get_omi_xyz_refsystem(xyz) ) ) THEN
       res = .true.
    ELSE
       res = .false.
       CALL setup_error_act ( all_errors(:), -30010, c_upname, c_modname )
       CALL setup_error_act ( '<objectname1>', TRIM(this%name) )
       CALL setup_error_act ( '<gid>', TRIM(get_omi_space_id(get_omi_xyz_refsystem(xyz))) )
       CALL setup_error_act ( '<did>', TRIM(get_omi_space_id(space)) )
    END IF
    !
  END FUNCTION ok_grid_space_identity_d
  !
  ! -----------------------------------------------------------------------
  ! >>> PRIVATE-GET-Methoden <<<
  ! -----------------------------------------------------------------------
  !
  !! Ermittle die Indikator-Liste f&uuml;r die zu erstellenden Koordinatenobjekte "c_xyz_id(:)" <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_xyz_id_ind_d ( this, g_exch ) &
       RESULT( res )
    !! aktuelles Datenobjekt
    TYPE (t_ipds)     , POINTER    :: this      !
    !! Austauschgr&ouml;&szlig;en des Gitters
    TYPE (t_omi_exch) , INTENT(IN) :: g_exch(:) !
    !! Ergebnis: Indikatorliste
    LOGICAL :: res(c_max_xyz) ! 
    !! Hilfsvariablen
    CHARACTER (LEN=c_len_omi_xyz_id) :: id_ch ! 
    TYPE (t_omi_ele)       , POINTER :: p_ele ! 
    TYPE (t_omi_xyz)       , POINTER :: p_xyz ! 
    INTEGER                          :: i     ! 
    !
    res(:) = .false.
    DO i=1,SIZE(g_exch)
       IF ( res(1) ) EXIT
       p_ele => get_omi_exch_ele_ref ( g_exch(i) )
       IF ( ASSOCIATED(p_ele) ) THEN
          p_xyz => get_omi_ele_xyz_ref ( p_ele )
          IF ( ASSOCIATED(p_xyz) ) THEN
             id_ch = get_omi_xyz_id ( p_xyz )
             SELECT CASE ( id_ch )
             CASE ( c_g_exch_id(1) )
                res(1) = .true.
             END SELECT
          END IF
       END IF
    END DO
    IF ( ASSOCIATED(this%mespos) ) res(2) = ( SIZE(this%mespos) > 0 )
    IF ( ASSOCIATED(this%region) ) res(3) = ( SIZE(this%region) > 0 )
    !
    NULLIFY( p_ele, p_xyz )
    !
  END FUNCTION get_xyz_id_ind_d
  !
  !! Ermittle die Anzahl der zu erstellenden Koordinatenobjekte aus "c_xyz_id(:)" <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_xyz_id_count_d ( this, g_exch ) &
       RESULT( res )
    !! aktuelles Datenobjekt
    TYPE (t_ipds)     , POINTER    :: this      !
    !! Austauschgr&ouml;&szlig;en des Gitters
    TYPE (t_omi_exch) , INTENT(IN) :: g_exch(:) !
    !! Ergebnis: Anzahl der zu erzeugenden Koordinaten-Objekte
    INTEGER :: res ! 
    !
    res = COUNT( get_xyz_id_ind( this, g_exch ) )
    !
  END FUNCTION get_xyz_id_count_d
  !
  !! Ermittle den Listenindex auf "c_xyz_id(:)" des val-ten Koordinatenobjekts <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_xyz_id_idx_d ( this, g_exch, val ) &
       RESULT( res )
    !! aktuelles Datenobjekt
    TYPE (t_ipds)     , POINTER    :: this      !
    !! Austauschgr&ouml;&szlig;en des Gitters
    TYPE (t_omi_exch) , INTENT(IN) :: g_exch(:) !
    !! lfd. Nummer des zu erzeugenden Koordinatenobjektes
    INTEGER           , INTENT(IN) :: val       ! 
    !! Ergebnis: Listenindex
    INTEGER :: res              ! 
    !! Hilfsvariablen
    LOGICAL :: l_ind(c_max_xyz) ! 
    INTEGER :: i, n             ! 
    !
    res      = -1
    n        =  0
    l_ind(:) = get_xyz_id_ind ( this, g_exch )
    IF ( val >= 1 .AND. val <= get_xyz_id_count( this, g_exch ) ) THEN
       DO i=1,SIZE(l_ind)
          IF ( res > 0  ) EXIT
          IF ( l_ind(i) ) n   = n + 1
          IF ( n == val ) res = i
       END DO
    END IF
    !
  END FUNCTION get_xyz_id_idx_d
  !
  !! ermittle den Zeiger auf das Gitter-Koordinaten-Objekt <BR>
  !! Funktion erzeugt <EM>keine</EM>Fehlermeldungen
  FUNCTION get_grid_xyz_ref_d ( this, g_exch, val ) & 
       RESULT(res)
    !! aktuelles Arbeitsobjekt
    TYPE (t_ipds)     , POINTER    :: this      ! 
    !! Austauschgroessen Gitter
    TYPE (t_omi_exch) , INTENT(IN) :: g_exch(:) ! 
    !! Id des zu suchenden Koordinaten-Objektes einer Austauschgroesse
    CHARACTER (LEN=*) , INTENT(IN) :: val       ! 
    !! Ergebnis: Zeiger auf Koordinatenobjekt
    TYPE (t_omi_xyz) , POINTER     :: res       ! 
    !! Hilfsvariablen
    TYPE (t_omi_ele)       , POINTER :: p_ele ! 
    TYPE (t_omi_xyz)       , POINTER :: p_xyz ! 
    CHARACTER (LEN=c_len_omi_xyz_id) :: id_ch ! 
    INTEGER :: i ! 
    !
    NULLIFY(res)
    DO i=1,SIZE(g_exch)
       IF ( ASSOCIATED(res) ) EXIT
       p_ele => get_omi_exch_ele_ref ( g_exch(i) )
       IF ( ASSOCIATED(p_ele) ) THEN
          p_xyz => get_omi_ele_xyz_ref ( p_ele )
          IF ( ASSOCIATED(p_xyz) ) THEN
             id_ch = get_omi_xyz_id ( p_xyz )
             IF ( LEN_TRIM(id_ch) == LEN_TRIM(val) ) THEN
                IF ( id_ch(1:LEN_TRIM(id_ch)) == val(1:LEN_TRIM(val)) ) THEN
                   res => get_omi_ele_xyz_ref ( p_ele )
                END IF
             END IF
          END IF
       END IF
    END DO
    NULLIFY( p_ele, p_xyz )
    !
  END FUNCTION get_grid_xyz_ref_d
  !
  !! ermittle die Anzahl der zu erzeugenden ElementSetIds f&uuml;r Austauschgr&ouml;&szlig;en <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_exch_ele_id_count_1 ( this, xyz, var, att, g_exch ) &
       RESULT( res )
    !! aktuelles Datenobjekt
    TYPE (t_ipds)     , INTENT(IN) :: this      ! 
    !! verf&uuml;gbare Koordinatenobjekte                    [IPDS  ]
    TYPE (t_omi_xyz)  , INTENT(IN) :: xyz(:)    ! 
    !! Variablenbezeichnungen                                [IPDS  ]
    TYPE (t_var)      , INTENT(IN) :: var(:)    ! 
    !! Attribute                                             [IPDS  ]
    TYPE (t_att)      , INTENT(IN) :: att(:)    ! 
    !! Feld mit allen Austauschgr&ouml;&szlig;en des Gitters [H_GRID]
    TYPE (t_omi_exch) , INTENT(IN) :: g_exch(:) ! 
    !! Ergebnis : Anzahl der zu erzeugenden Indexliste-Objekte
    INTEGER                        :: res       ! 
    !! Hilfsvariablen
    LOGICAL                      , ALLOCATABLE :: l_ind(:) ! 
    CHARACTER (LEN=c_len_omi_ele_id) , POINTER :: ele_id(:) ! 
    !
    ele_id => get_exch_all_ele_id ( this, g_exch ) ! Liste aller moeglichen Ids
    ALLOCATE(l_ind(SIZE(ele_id)))
    l_ind =  get_exch_ele_id_ind ( ele_id, xyz, var, att )
    res   = COUNT(l_ind)
    IF ( ALLOCATED (l_ind ) ) DEALLOCATE( l_ind  )
    IF ( ASSOCIATED(ele_id) ) DEALLOCATE( ele_id )
    NULLIFY( ele_id )
    !
  END FUNCTION get_exch_ele_id_count_1
  !
  !! erzeuge die Liste mit allen m&ouml;glichen zu erzeugenden ElementSet-Ids <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_exch_all_ele_id_1 ( this, g_exch ) &
       RESULT( res )
    !! aktuelles Datenobjekt
    TYPE (t_ipds)     , INTENT(IN) :: this      ! 
    !! Feld mit allen Austauschgr&ouml;&szlig;en des Gitters [H_GRID]
    TYPE (t_omi_exch) , INTENT(IN) :: g_exch(:) ! 
    !! Ergebnis: Feld mit allen m&ouml;glichen ElementSet-Ids
    CHARACTER (LEN=c_len_omi_ele_id) , POINTER :: res(:) ! 
    !! Hilfsvariablen
    INTEGER                    :: i, n, m ! 
    TYPE (t_omi_ele) , POINTER :: p_ele ! 
    !
    NULLIFY(res)
    n = SIZE(g_exch)
    IF ( ASSOCIATED(this%mespos) ) n = n + c_max_mespos_ind_id
    IF ( ASSOCIATED(this%region) ) n = n + c_max_region_ind_id
    IF ( n > 0 ) THEN
       ALLOCATE( res(n) )
       res(:) = REPEAT( ' ', LEN(res(i)) )
       DO i=1,SIZE(g_exch)
          res(i) = c_undef_omi_ele_char
          p_ele => get_omi_exch_ele_ref ( g_exch(i) )
          IF ( ASSOCIATED(p_ele) ) res(i) = get_omi_ele_id( p_ele )
          NULLIFY( p_ele )
       END DO
       m = SIZE(g_exch)
       IF ( ASSOCIATED(this%mespos) ) THEN
          DO i=1,c_max_mespos_ind_id
             res(m+i) = TRIM(c_mespos_ind_id(i))//'['//TRIM(this%name)//']'
          END DO
          m = m + c_max_mespos_ind_id
       END IF
       IF ( ASSOCIATED(this%region) ) THEN
          DO i=1,c_max_region_ind_id
             res(m+i) = TRIM(c_region_ind_id(i))//'['//TRIM(this%name)//']'
          END DO
          m = m + c_max_region_ind_id
       END IF
    END IF
    !
  END FUNCTION get_exch_all_ele_id_1
  !
  !! Ermittle die Indkatorliste f&uuml;r die tats&auml;chlich zu erzeugenden ElementSet-Ids 
  !! f&uuml;r eine bestimmte Variable <BR> 
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_exch_ele_id_ind_d ( ele_id, xyz, var, att ) &
       RESULT( res )
    !! Liste alle m&ouml;glichen ElementSetIds
    CHARACTER (LEN=*) , INTENT(IN) :: ele_id(:) ! 
    !! verf&uuml;gbare Koordinatenobjekte                    [IPDS  ]
    TYPE (t_omi_xyz)  , INTENT(IN) :: xyz(:)    ! 
    !! Variablenbezeichnung                                  [IPDS  ]
    TYPE (t_var)      , INTENT(IN) :: var       ! 
    !! Attribute                                             [IPDS  ]
    TYPE (t_att)      , INTENT(IN) :: att(:)    ! 
    !! Ergebnis : Indikatorliste f&uuml;r die tats&auml;chlich zu erzeugenden ElementSetIds
    LOGICAL :: res(SIZE(ele_id))  ! 
    !! Hilfsvariablen
    CHARACTER (LEN=c_len_omi_ele_id) :: ch_omiele, ch_omigen    ! 
    LOGICAL :: is_3d(3) ! 
    INTEGER :: i, j, k, l, code, l_omiele, l_omigen ! 
    !
    code     = get_att_var_name_id ( att, var )
    res(:)   = .false.
    is_3d(1) = ( get_att_interfaces_count ( att ) > 2 )
    DO l=1,SIZE(xyz)
       is_3d(2) = is_omi_xyz_three_dimensional ( xyz(l) )
       DO k=1,get_phy_quant_omiele_nof ( code )
          ch_omiele = REPEAT( ' ', LEN(ch_omiele) )
          ch_omiele = get_phy_quant_omiele(code,k)
          l_omiele  = LEN_TRIM(ch_omiele)
          is_3d(3)  = is_phy_quant_omiele_3d(code,k)
          IF ( ANY( (/0,3/) == COUNT(is_3d) ) ) THEN
             DO j=1,get_phy_quant_omigen_nof ( code )
                ch_omigen = REPEAT( ' ', LEN(ch_omigen) )
                ch_omigen = get_phy_quant_omigen(code,j)
                l_omigen  = LEN_TRIM(ch_omigen)
                DO i=1,SIZE(ele_id)
                   IF ( res(i) ) CYCLE
                   res(i) = ( ele_id(i)(1:l_omigen) == ch_omigen(1:l_omigen) .AND. &
                              INDEX( ele_id(i)(l_omigen+1:), ch_omiele(1:l_omiele) ) > 0 ) 
                END DO
             END DO
          END IF
       END DO
    END DO
    !
  END FUNCTION get_exch_ele_id_ind_d
  !
  !! Ermittle die Indkatorliste f&uuml;r die tats&auml;chlich zu erzeugenden ElementSet-Ids 
  !! f&uuml;r viele Variablen <BR> 
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_exch_ele_id_ind_1 ( ele_id, xyz, var, att ) &
       RESULT( res )
    !! Liste alle m&ouml;glichen ElementSetIds
    CHARACTER (LEN=*) , INTENT(IN) :: ele_id(:) ! 
    !! verf&uuml;gbare Koordinatenobjekte                    [IPDS  ]
    TYPE (t_omi_xyz)  , INTENT(IN) :: xyz(:)    ! 
    !! Variablenbezeichnungen                                [IPDS  ]
    TYPE (t_var)      , INTENT(IN) :: var(:)    ! 
    !! Attribute                                             [IPDS  ]
    TYPE (t_att)      , INTENT(IN) :: att(:)    ! 
    !! Ergebnis : Indikatorliste f&uuml;r die tats&auml;chlich zu erzeugenden ElementSetIds
    LOGICAL :: res(SIZE(ele_id))  ! 
    !! Hilfsvariablen
    INTEGER :: i ! 
    LOGICAL , ALLOCATABLE :: l_ind(:) ! 
    !
    ALLOCATE( l_ind(SIZE(ele_id)) )
    res(:) = .false.
    DO i=1,SIZE(var)
       l_ind(:) = get_exch_ele_id_ind ( ele_id, xyz, var(i), att )
       WHERE ( l_ind .AND. .NOT. res ) res = l_ind
    END DO
    DEALLOCATE( l_ind )
    !
  END FUNCTION get_exch_ele_id_ind_1
  !
  !! Ermittle den Namen der val-ten zu erzeugenden ElementSetId f&uuml;r viele Variablen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_exch_ele_id_1 ( this, xyz, var, att, g_exch, val ) &
       RESULT( res )
    !! aktuelles Datenobjekt
    TYPE (t_ipds)     , INTENT(IN) :: this      ! 
    !! verf&uuml;gbare Koordinatenobjekte                    [IPDS  ]
    TYPE (t_omi_xyz)  , INTENT(IN) :: xyz(:)    ! 
    !! Variablenbezeichnungen                                [IPDS  ]
    TYPE (t_var)      , INTENT(IN) :: var(:)    ! 
    !! Attribute                                             [IPDS  ]
    TYPE (t_att)      , INTENT(IN) :: att(:)    ! 
    !! Feld mit allen Austauschgr&ouml;&szlig;en des Gitters [H_GRID]
    TYPE (t_omi_exch) , INTENT(IN) :: g_exch(:) ! 
    !! lfd. Nummer des ElementSets deren Id bestimmt werden soll
    INTEGER           , INTENT(IN) :: val       ! 
    !! Ergebnis : Namen der val-ten zu erzeugenden ElementSetId
    CHARACTER (LEN=c_len_omi_ele_id) :: res ! 
    !! Hilfsvariablen 
    INTEGER                                    :: i, n        ! 
    CHARACTER (LEN=c_len_omi_ele_id) , POINTER :: p_ele_id(:) ! 
    LOGICAL , ALLOCATABLE                      :: l_ind(:)    ! 
    !
    res = REPEAT( ' ', LEN(res) )
    res = c_undef_omi_ele_char
    p_ele_id => get_exch_all_ele_id ( this, g_exch )
    ALLOCATE(l_ind(SIZE(p_ele_id)))
    l_ind = get_exch_ele_id_ind ( p_ele_id, xyz, var, att )
    IF ( val >= 1 .AND. val <= COUNT(l_ind) ) THEN
       n = 0
       DO i=1,SIZE(l_ind)
          IF ( res(1:LEN(c_undef_omi_ele_char)) /= c_undef_omi_ele_char ) EXIT
          IF ( l_ind(i) ) n = n + 1
          IF ( n == val ) res = p_ele_id(i)
       END DO
    END IF
    IF (  ALLOCATED(l_ind)    ) DEALLOCATE( l_ind )
    IF ( ASSOCIATED(p_ele_id) ) DEALLOCATE( p_ele_id )
    NULLIFY( p_ele_id )
    !
  END FUNCTION get_exch_ele_id_1
  !
  !! Ermittle den Zeiger auf das Daten-Koordinaten-Objekt das zum Erstellen
  !! eines bestimmten Daten-Index-Listen-Objekts ben&ouml;tigt wird <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION get_idx_d_d ( this, ele_id, var ) &
       RESULT( res )
    !! aktuelles Objekt
    TYPE (t_ipds)     , POINTER    :: this   ! 
    !! Id des ElementSet fuer das zu erstellende Index-Listen-Objekt
    CHARACTER (LEN=*) , INTENT(IN) :: ele_id ! 
    !! Liste der verf&uuml;gbaren Koordinaten-Objekte
    TYPE (t_omi_xyz)  , INTENT(IN) :: var(:) ! 
    !! Zeiger (Indexposition) des erforderlichen Koordinatenobjektes in var(:)
    INTEGER :: res  ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=11) , PARAMETER :: c_upname='get_idx_d_d' ! 
    !! Hilfsvariable
    CHARACTER (LEN=LEN(c_xyz_id))  :: l_xyz_id ! 
    LOGICAL :: is_ipds   ! 
    INTEGER :: i         ! 
    !
    res      = -1
    is_ipds  = ( INDEX( ele_id, TRIM(this%name) ) > 0 )
    l_xyz_id = REPEAT( ' ', LEN(l_xyz_id) )
    l_xyz_id = c_undef_omi_ele_char
    IF ( is_ipds ) THEN
       DO i=1,c_max_mespos_ind_id
          IF ( l_xyz_id(1:LEN(c_undef_omi_ele_char)) /= c_undef_omi_ele_char ) EXIT
          IF ( INDEX( ele_id, TRIM(c_mespos_ind_id(i)) ) > 0 ) l_xyz_id = c_xyz_id(2)
       END DO
       DO i=1,c_max_region_ind_id
          IF ( l_xyz_id(1:LEN(c_undef_omi_ele_char)) /= c_undef_omi_ele_char ) EXIT
          IF ( INDEX( ele_id, TRIM(c_region_ind_id(i)) ) > 0 ) l_xyz_id = c_xyz_id(3)
       END DO
    ELSE
       l_xyz_id = c_xyz_id(1)
    END IF
    res = get_omi_xyz_idx ( var, l_xyz_id )
    IF ( res <= 0 ) THEN
       CALL setup_error_act ( all_errors(:), -30030, c_upname, c_modname )
       CALL setup_error_act ( '<objectname1>', TRIM(this%name) )
       CALL setup_error_act ( '<indexname>', TRIM(ele_id) )
    END IF
    !
  END FUNCTION get_idx_d_d
  !
  !! Ermittle den Zeiger auf das Gitter-Indexlisten-Objekt das zum Erstellen
  !! eines bestimmten Daten-Index-Listen-Objekts ben&ouml;tigt wird <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_idx_g_d ( this, ele_id, var ) &
       RESULT( res )
    !! aktuelles Objekt
    TYPE (t_ipds)     , POINTER    :: this   ! 
    !! ElementSet Id des zu erstellenden Index-Listen-Objekts
    CHARACTER (LEN=*) , INTENT(IN) :: ele_id ! 
    !! Liste der verf&uuml;gbaren Gitter-Austausch-Objekte
    TYPE (t_omi_exch) , INTENT(IN) :: var(:) ! 
    !! Zeiger (Indexposition) des erforderlichen Gitterindexlistenobjektes in var(:)
    INTEGER :: res  ! 
    !! Hilfsvariable
    TYPE (t_omi_ele)     , POINTER   :: p_ele     ! 
    CHARACTER (LEN=c_len_omi_ele_id) :: ch        ! 
    INTEGER                          :: i, l1, l2 ! 
    !
    res = -1
    DO i=1,SIZE(var)
       IF ( res /= -1 ) EXIT
       p_ele => get_omi_exch_ele_ref    ( var(i) )
       ch    =  get_omi_ele_id          ( p_ele  )
       l1    = INDEX( ele_id, '/' )
       l2    = INDEX( ele_id, '/', BACK=.true. )
       IF ( ch(1:LEN(ch)-l2+l1) == ele_id(1:l1)//ele_id(l2+1:) ) res = i
    END DO
    !
  END FUNCTION get_idx_g_d
  !
  !! Ermittle die Anzahl der zu erzeugenden Austauschgr&ouml;&szlig;en 
  !! f&uuml;r viele Variablen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_exch_count_1 ( xyz, var, att, dim, ele_id) &
       RESULT( res )
    !! Liste der Koordinatenobjekte
    TYPE (t_omi_xyz)  , INTENT(IN) :: xyz(:)    ! 
    !! Variable
    TYPE (t_var)      , INTENT(IN) :: var(:)    ! 
    !! Liste der Attribute
    TYPE (t_att)      , INTENT(IN) :: att(:)    ! 
    !! Liste der Dimensionen
    TYPE (t_dim)      , INTENT(IN) :: dim(:)    ! 
    !! Liste aller tats&auml;chlich zu erzeugenden ElementSet-Ids
    CHARACTER (LEN=*) , INTENT(IN) :: ele_id(:) ! 
    !! Ergebnis : Anzahl der zu erzeugenden Austauschgr&ouml;&szlig;en
    INTEGER :: res ! 
    !! Hilfsvariablen
    INTEGER :: i   ! 
    !
    res = 0
    DO i=1,SIZE(var)
       res = res + get_max_omi_quant(var(i),dim)*COUNT(get_exch_ele_id_ind(ele_id,xyz,var(i),att))
    END DO
    !
  END FUNCTION get_exch_count_1
  !
  !! Ermittle die Indikatorliste f&uuml;r die zum Erzeugen von Austauschgr&ouml;&szlig;en
  !! zu verwendenden ElementSets f&uuml;r eine Variable <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_exch_ele_ind_0 ( xyz, var, att, ele, ele_id ) &
       RESULT(res)
    !! Liste der Koordinatenobjekte
    TYPE (t_omi_xyz)  , INTENT(IN) :: xyz(:)    ! 
    !! Variable
    TYPE (t_var)      , INTENT(IN) :: var       ! 
    !! Liste der Attribute
    TYPE (t_att)      , INTENT(IN) :: att(:)    ! 
    !! Liste der ElementSets
    TYPE (t_omi_ele)  , INTENT(IN) :: ele(:)    ! 
    !! Liste aller tats&auml;chlich zu erzeugenden ElementSet-Ids
    CHARACTER (LEN=*) , INTENT(IN) :: ele_id(:) ! 
    !! Ergebnis: Indikatorliste f&uuml;r die Elementsets ele(:)
    LOGICAL :: res(SIZE(ele))                   ! 
    !! Hilfsvariablen
    CHARACTER (LEN=c_len_omi_ele_id) :: ch      ! 
    INTEGER :: i, j, l1, l2 ! 
    LOGICAL , ALLOCATABLE :: l_ind(:)           ! 
    !
    ALLOCATE( l_ind(SIZE(ele_id)) )
    l_ind  = get_exch_ele_id_ind ( ele_id, xyz, var, att )
    res(:) = .false.
    DO i=1,SIZE(res)
       ch = get_omi_ele_id ( ele(i) )
       l1 = LEN_TRIM(ch)
       DO j=1,SIZE(ele_id)
          IF ( .NOT. l_ind(j) ) CYCLE
          IF ( res(i)   ) EXIT
          l2 = LEN_TRIM(ele_id(j))
          IF ( l1 == l2 ) THEN
             IF ( ch(1:l1) == ele_id(j)(1:l2) ) res(i) = .true.
          END IF
       END DO
    END DO
    DEALLOCATE( l_ind )
    !
  END FUNCTION get_exch_ele_ind_0
  !
  !! Ermittle die Indikatorliste f&uuml;r die zum Erzeugen von Austauschgr&ouml;&szlig;en
  !! zu verwendenden Quantities f&uuml;r eine Variable <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_exch_quant_ind_0 ( var, att, dim, quant ) &
       RESULT(res)
    !! Variable
    TYPE (t_var)       , INTENT(IN) :: var      ! 
    !! Liste der Attribute
    TYPE (t_att)       , INTENT(IN) :: att(:)   ! 
    !! Liste der Dimensionen
    TYPE (t_dim)       , INTENT(IN) :: dim(:)   ! 
    !! Liste aller Quantities
    TYPE (t_omi_quant) , INTENT(IN) :: quant(:) ! 
    !! Ergebnis : Indiaktorliste f&uuml;r zu erzeugende ElementSets
    LOGICAL :: res(SIZE(quant)) ! 
    !! Hilfsvariablen
    INTEGER :: i, idx, nq       ! 
    TYPE (t_omi_quant) , ALLOCATABLE :: l_quant(:) ! 
    !
    res(:) = .false.
    nq     = get_max_omi_quant ( var, dim )
    IF ( nq > 0 ) THEN
       ALLOCATE( l_quant(nq) )
       CALL new_omi_quant ( l_quant )
       CALL set_omi_quant ( l_quant, var, dim, att )
       DO i=1,nq
          idx = get_omi_quant_idx ( quant, get_omi_quant_id(l_quant(i)) )
          IF ( idx > 0 ) res(idx) = .true. 
       END DO
       CALL kill_omi_quant ( l_quant )
       DEALLOCATE ( l_quant )
    END IF
    !
  END FUNCTION get_exch_quant_ind_0
  !
  !! Ermittle den Code f&uuml;r eine Austauschgr&ouml;&szlig;e anhand der 
  !! Positions-Nummer der Austauschgr&ouml;&szlig;e <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_code_from_exch_idx_0 ( this, exch_idx ) &
       RESULT( res )
    !! aktuelles Datenobjekt
    TYPE (t_ipds) , POINTER    :: this     ! 
    !! Zeiger auf die Position der Austauschgr&ouml;&szlig;e in dem
    !! aktuellen Arbeitsobjekt
    INTEGER      , INTENT(IN)  :: exch_idx ! 
    !! Ergebnis : Code oder -1 falls undefiniert
    INTEGER                    :: res      ! 
    !! Hilfsvariablen
    TYPE (t_omi_exch)  , POINTER :: p_exch(:) ! 
    TYPE (t_omi_quant) , POINTER :: p_quant   ! 
    !
    res = -1
    p_exch => get_exch_object( this )
    IF ( ASSOCIATED( p_exch ) ) THEN
       IF ( exch_idx >= 1 .AND. exch_idx <= SIZE(p_exch) ) THEN
          p_quant => get_omi_exch_quant_ref ( p_exch(exch_idx) )
          IF ( ASSOCIATED( p_quant ) ) THEN
             res = get_code_from_omi_quant ( p_quant )
             res = MERGE( -1, res , res == c_undef_omi_quant_int )
          END IF
       END IF
    END IF
    NULLIFY( p_exch, p_quant )
    !
  END FUNCTION get_code_from_exch_idx_0
  !
  !! Ermittle den Zeiger auf das Koordinatenobjekt einer Austauschr&ouml;&szlig;e <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_exch_xyz_d ( this, exch_idx ) & 
       RESULT( res )
    !! aktuelles Datenobjekt
    TYPE (t_ipds) , POINTER    :: this     ! 
    !! Zeiger auf die aktuelle Austauschgr&ouml;&szlig;e, deren Koordinatenobjekt
    !! ermittelt werden soll
    INTEGER       , INTENT(IN) :: exch_idx ! 
    !! Ergebnis : Zeiger auf das gesuchte Koordinatenobjekt
    TYPE (t_omi_xyz) , POINTER :: res      ! 
    !! Hilfsvariablen
    TYPE (t_omi_ele) , POINTER :: p_ele    ! 
    !
    NULLIFY( res )
    IF ( ASSOCIATED(this%exch) ) THEN
       IF ( exch_idx >= 1 .AND. exch_idx <= SIZE(this%exch) ) THEN
          p_ele => get_omi_exch_ele_ref ( this%exch(exch_idx) )
          IF ( ASSOCIATED( p_ele ) ) THEN
             res => get_omi_ele_xyz_ref ( p_ele )
          END IF
       END IF
    END IF
    NULLIFY(p_ele)
    !
  END FUNCTION get_exch_xyz_d
  !
  !! Ermittle den Zeiger auf das Quantity-Objekt einer Austauschr&ouml;&szlig;e <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_exch_quant_d ( this, exch_idx ) & 
       RESULT( res )
    !! aktuelles Datenobjekt
    TYPE (t_ipds) , POINTER    :: this     ! 
    !! Zeiger auf die aktuelle Austauschgr&ouml;&szlig;e, deren Koordinatenobjekt
    !! ermittelt werden soll
    INTEGER       , INTENT(IN) :: exch_idx ! 
    !! Ergebnis : Zeiger auf das gesuchte Quantity-Objekt
    TYPE (t_omi_quant) , POINTER :: res      ! 
    !
    NULLIFY( res )
    IF ( ASSOCIATED(this%exch) ) THEN
       IF ( exch_idx >= 1 .AND. exch_idx <= SIZE(this%exch) ) THEN
          res => get_omi_exch_quant_ref ( this%exch(exch_idx) )
       END IF
    END IF
    !
  END FUNCTION get_exch_quant_d
  !
  ! -----------------------------------------------------------------------
  ! >>> PRIVATE-CREATE-Methoden <<< 
  ! -----------------------------------------------------------------------
  !
  !! Erzeuge Koordinatenobjekt f&uuml;r externes Gitter ohne Schichtstruktur <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE create_omi_xyz_2d_d ( no, g_xyz, space, hlimit, xyz )
    !! Nummer des zu erzeugenden Koordinaten-Objekts gemaess "c_xyz_id(no)"
    INTEGER            , INTENT(IN)  :: no     ! 
    !! Gitterkoordinaten
    TYPE (t_omi_xyz)   , INTENT(IN)  :: g_xyz  ! 
    !! Koordinatensystembeschreibung
    TYPE (t_omi_space) , INTENT(IN)  :: space  ! 
    !! minimale Wasserbedeckung
    REAL (KIND=Double) , INTENT(IN)  :: hlimit ! 
    !! Koordinaten-Objekt
    TYPE (t_omi_xyz)   , INTENT(OUT) :: xyz    ! 
    !! Hilfsvariablen
    REAL (KIND=Double) , POINTER     :: p_x(:), p_y(:), p_b(:) ! 
    !
    ! [1] Rueckgabeobjekt initialisieren
    CALL new_omi_xyz ( xyz )
    ! [2] Koordinaten des Gitters extrahieren
    p_x => get_omi_xyz_x_ref    ( g_xyz )
    p_y => get_omi_xyz_y_ref    ( g_xyz )
    p_b => get_omi_xyz_bini_ref ( g_xyz )
    ! [3] Koordinaten-Objekt erzeugen
    CALL set_omi_xyz_id           ( xyz, c_xyz_id(no) )
    CALL set_omi_xyz_description  ( xyz, c_xyz_descr(no) )
    CALL set_omi_xyz_refsystem    ( xyz, space )
    CALL set_omi_xyz_ztype        ( xyz, 0 )
    CALL set_omi_xyz_ctype        ( xyz, 0 )
    CALL set_omi_xyz_hlimit       ( xyz, hlimit )
    IF ( ASSOCIATED( p_b ) ) THEN
       CALL create_omi_xyz_no_layers ( xyz, p_x, p_y, bottom=p_b )
    ELSE
       CALL create_omi_xyz_no_layers ( xyz, p_x, p_y )
    END IF
    NULLIFY( p_x, p_y, p_b )
    !
  END SUBROUTINE create_omi_xyz_2d_d
  !
  !! Erzeuge Koordinatenobjekt f&uuml;r Sampling Points ohne Schichtstruktur <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE create_omi_xyz_2d_mp_d ( no, this, space, hlimit, xyz )
    !! Nummer des zu erzeugenden Koordinaten-Objekts gemaess "c_xyz_id(no)"
    INTEGER            , INTENT(IN)  :: no     ! 
    !! aktuelles Datenobjekt
    TYPE (t_ipds)      , POINTER     :: this   ! 
    !! Koordinatensystembeschreibung
    TYPE (t_omi_space) , INTENT(IN)  :: space  ! 
    !! minimale Wasserbedeckung
    REAL (KIND=Double) , INTENT(IN)  :: hlimit ! 
    !! Koordinaten-Objekt
    TYPE (t_omi_xyz)   , INTENT(OUT) :: xyz    ! 
    !! Hilfsvariablen
    INTEGER :: i, n, nd ! 
    LOGICAL            , ALLOCATABLE :: l_ind(:)       ! 
    REAL (KIND=Double) , ALLOCATABLE :: l_x(:), l_y(:) ! 
    TYPE (t_point_2d)  , ALLOCATABLE :: l_point_2d(:)  ! 
    !
    CALL new_omi_xyz ( xyz )
    IF ( ASSOCIATED(this%mespos) ) THEN
       ALLOCATE( l_ind(SIZE(this%mespos)), l_point_2d(SIZE(this%mespos)) )
       DO i=1,SIZE(l_point_2d)
          l_point_2d(i) = get_mespos_coor ( this%mespos(i) )
       END DO
       l_ind = get_point_2d_diff_ind   ( l_point_2d )
       nd    = get_point_2d_diff_count ( l_point_2d )
       n     = 0
       ALLOCATE( l_x(nd), l_y(nd) )
       DO i=1,SIZE(l_point_2d)
          IF ( .NOT. l_ind(i) ) CYCLE
          n      = n + 1
          l_x(n) = get_point_2d_x ( l_point_2d(i) )
          l_y(n) = get_point_2d_y ( l_point_2d(i) )
       END DO
       CALL set_omi_xyz_id           ( xyz, c_xyz_id(no) )
       CALL set_omi_xyz_description  ( xyz, c_xyz_descr(no) )
       CALL set_omi_xyz_refsystem    ( xyz, space )
       CALL set_omi_xyz_ztype        ( xyz, 0 )
       CALL set_omi_xyz_ctype        ( xyz, 0 )
       CALL set_omi_xyz_hlimit       ( xyz, hlimit )
       CALL create_omi_xyz_no_layers ( xyz, l_x, l_y )
    END IF
    IF ( ALLOCATED(l_point_2d) ) DEALLOCATE( l_point_2d )
    IF ( ALLOCATED(l_ind     ) ) DEALLOCATE( l_ind      )
    IF ( ALLOCATED(l_x       ) ) DEALLOCATE( l_x        )
    IF ( ALLOCATED(l_y       ) ) DEALLOCATE( l_y        )
    !
  END SUBROUTINE create_omi_xyz_2d_mp_d
  !
  !! Erzeuge Koordinatenobjekt f&uuml;r Regional Values ohne Schichtstruktur <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE create_omi_xyz_2d_re_d ( no, this, space, hlimit, xyz )
    !! Nummer des zu erzeugenden Koordinaten-Objekts gemaess "c_xyz_id(no)"
    INTEGER            , INTENT(IN)  :: no     ! 
    !! aktuelles Datenobjekt
    TYPE (t_ipds)      , POINTER     :: this   ! 
    !! Koordinatensystembeschreibung
    TYPE (t_omi_space) , INTENT(IN)  :: space  ! 
    !! minimale Wasserbedeckung
    REAL (KIND=Double) , INTENT(IN)  :: hlimit ! 
    !! Koordinaten-Objekt
    TYPE (t_omi_xyz)   , INTENT(OUT) :: xyz    ! 
    !! Hilfsvariablen
    INTEGER :: i, n, nd ! 
    LOGICAL            , ALLOCATABLE :: l_ind(:)       ! 
    REAL (KIND=Double) , ALLOCATABLE :: l_x(:), l_y(:) ! 
    TYPE (t_point_2d)  , ALLOCATABLE :: l_point_2d(:)  ! 
    !
    CALL new_omi_xyz ( xyz )
    IF ( ASSOCIATED(this%region) ) THEN
       nd = 0
       DO i=1,SIZE(this%region)
          nd = nd + get_region_nof_border ( this%region(i) )
       END DO
       ALLOCATE( l_ind(nd), l_point_2d(nd) )
       n  = 0
       DO i=1,SIZE(this%region)
          nd                   = get_region_nof_border ( this%region(i) )
          l_point_2d(n+1:n+nd) = get_region_border     ( this%region(i) )
          n                    = n + nd
       END DO
       l_ind = get_point_2d_diff_ind   ( l_point_2d )
       nd    = get_point_2d_diff_count ( l_point_2d )
       n     = 0
       ALLOCATE( l_x(nd), l_y(nd) )
       DO i=1,SIZE(l_point_2d)
          IF ( .NOT. l_ind(i) ) CYCLE
          n      = n + 1
          l_x(n) = get_point_2d_x ( l_point_2d(i) )
          l_y(n) = get_point_2d_y ( l_point_2d(i) )
       END DO
       ! [2] Koordinatenobjekt erzeugen
       CALL set_omi_xyz_id           ( xyz, c_xyz_id(no) )
       CALL set_omi_xyz_description  ( xyz, c_xyz_descr(no) )
       CALL set_omi_xyz_refsystem    ( xyz, space )
       CALL set_omi_xyz_ztype        ( xyz, 0 )
       CALL set_omi_xyz_ctype        ( xyz, 0 )
       CALL set_omi_xyz_hlimit       ( xyz, hlimit )
       CALL create_omi_xyz_no_layers ( xyz, l_x, l_y )
    END IF
    IF ( ALLOCATED(l_point_2d) ) DEALLOCATE( l_point_2d )
    IF ( ALLOCATED(l_ind     ) ) DEALLOCATE( l_ind      )
    IF ( ALLOCATED(l_x       ) ) DEALLOCATE( l_x        )
    IF ( ALLOCATED(l_y       ) ) DEALLOCATE( l_y        )
    !
  END SUBROUTINE create_omi_xyz_2d_re_d
  !
  !! Erzeuge eine Komponente der Daten-Index-Listen-Objekte aus dem externen Gitter <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE create_omi_l_ind_g_d ( this, ind_id, xyz, g_exch, ind )
    !! Datenobjekt
    TYPE (t_ipds)      , POINTER       :: this   ! 
    !! Id f&uuml;r das zu erzeugenden Index-Listen-Objekt
    CHARACTER (LEN=*)  , INTENT(IN)    :: ind_id ! 
    !! Daten-Koordinaten-Objekt 
    TYPE (t_omi_xyz)   , INTENT(IN)    :: xyz    ! 
    !! Gitter-Austausch-Objekt
    TYPE (t_omi_exch)  , INTENT(IN)    :: g_exch ! 
    !! Daten-Listen-Index-Objekt
    TYPE (t_omi_ind)   , INTENT(INOUT) :: ind    ! 
    !! Hilfsvariablen
    TYPE (t_omi_ind) , POINTER :: g_ind_xyz ! 
    TYPE (t_omi_ele) , POINTER :: p_ele     ! 
    !
    p_ele => get_omi_exch_ele_ref ( g_exch )
    IF ( ASSOCIATED( p_ele ) ) THEN
       g_ind_xyz => get_omi_ele_ind_xyz_ref ( p_ele  ) 
       IF ( ASSOCIATED( g_ind_xyz ) ) THEN
          CALL copy_omi_ind    ( ind, g_ind_xyz )
          CALL set_omi_ind_id  ( ind, ind_id    )
       END IF
    END IF
    NULLIFY ( g_ind_xyz, p_ele )
    !
  END SUBROUTINE create_omi_l_ind_g_d
  !
  !! Erzeuge eine Komponente der Daten-Index-Listen-Objekte f&uuml;r die IPDS-Geometrien <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE create_omi_l_ind_i_d ( this, ind_id, xyz, ind )
    !! Datenobjekt
    TYPE (t_ipds)      , POINTER       :: this   ! 
    !! Id f&uuml;r das zu erzeugenden Index-Listen-Objekt
    CHARACTER (LEN=*)  , INTENT(IN)    :: ind_id ! 
    !! Daten-Koordinaten-Objekt 
    TYPE (t_omi_xyz)   , INTENT(IN)    :: xyz    ! 
    !! Daten-Listen-Index-Objekt
    TYPE (t_omi_ind)   , INTENT(INOUT) :: ind    ! 
    !! Hilfsvariablen
    INTEGER :: i, n, nd, nr, ia, ie ! 
    CHARACTER (LEN=c_len_omi_ind_stru_id) , ALLOCATABLE :: l_stru_id(:) ! 
    INTEGER            , ALLOCATABLE :: l_stru_start(:), l_stru_len(:), l_stru_list(:), l_idx(:) ! 
    TYPE (t_point_2d)  , ALLOCATABLE :: l_point_2d(:)  ! 
    CHARACTER (LEN=c_len_mespos_name) , ALLOCATABLE :: l_name(:)      ! 
    !
    SELECT CASE ( get_omi_xyz_id( xyz ) )
    CASE ( c_xyz_id(2) ) ! Sampling Points
       !
       CALL set_omi_ind_id          ( ind, ind_id )
       CALL set_omi_ind_description ( ind, c_mespos_ind_descr(1)//', '//TRIM(this%name) )
       nd = SIZE(this%mespos)
       ALLOCATE( l_name(nd), l_point_2d(nd), l_idx(nd) )
       ALLOCATE( l_stru_id(nd), l_stru_start(nd), l_stru_len(nd), l_stru_list(nd) )
       DO i=1,SIZE(l_point_2d)
          l_point_2d(i) = get_mespos_coor ( this%mespos(i) )
          l_name(i)     = get_mespos_name ( this%mespos(i) )
       END DO
       l_idx = get_point_2d_diff_idx ( l_point_2d )
       DO i=1,nd
          l_stru_start(i) = i
          l_stru_len(i)   = 1
          l_stru_list(i)  = l_idx(i)
          l_stru_id(i)    = l_name(i)(1:MIN(LEN(l_stru_id),LEN_TRIM(l_name(i))))
       END DO
       !
    CASE ( c_xyz_id(3) ) ! Regional Values
       !
       CALL set_omi_ind_id          ( ind, ind_id )
       CALL set_omi_ind_description ( ind, c_region_ind_descr(1)//', '//TRIM(this%name) )
       nd = 0
       nr = SIZE(this%region)
       DO i=1,nr
          nd = nd + get_region_nof_border ( this%region(i) )
       END DO
       ALLOCATE( l_name(nr), l_point_2d(nd), l_idx(nd) )
       ALLOCATE( l_stru_id(nr), l_stru_start(nr), l_stru_len(nr), l_stru_list(nd) )
       n = 0
       DO i=1,nr
          nd                   = get_region_nof_border ( this%region(i) )
          l_point_2d(n+1:n+nd) = get_region_border     ( this%region(i) )
          l_name(i)            = REPEAT( ' ', LEN(l_name(i)) )
          l_name(i)            = get_region_name       ( this%region(i) )
          n                    = n + nd
       END DO
       l_idx = get_point_2d_diff_idx ( l_point_2d )
       n  = 1
       DO i=1,nr
          l_stru_start(i)    = n
          l_stru_len(i)      = get_region_nof_border ( this%region(i) )
          ia                 = l_stru_start(i)
          ie                 = ia + l_stru_len(i) - 1
          l_stru_list(ia:ie) = l_idx(ia:ie)
          l_stru_id(i)       = l_name(i)(1:MIN(LEN(l_stru_id),LEN_TRIM(l_name(i))))
          n                  = ie + 1
       END DO
    END SELECT
    !
    IF ( ALLOCATED( l_stru_start ) ) THEN
       CALL set_omi_ind_stru_start ( ind, l_stru_start )
       DEALLOCATE( l_stru_start )
    END IF
    IF ( ALLOCATED( l_stru_len ) ) THEN
       CALL set_omi_ind_stru_len   ( ind, l_stru_len )
       DEALLOCATE( l_stru_len )
    END IF
    IF ( ALLOCATED( l_stru_list ) ) THEN
       CALL set_omi_ind_stru_list  ( ind, l_stru_list )
       DEALLOCATE( l_stru_list )
    END IF
    IF ( ALLOCATED( l_stru_id ) ) THEN
       CALL set_omi_ind_stru_id    ( ind, l_stru_id )
       DEALLOCATE( l_stru_id )
    END IF
    IF ( ALLOCATED( l_point_2d ) ) DEALLOCATE( l_point_2d )
    IF ( ALLOCATED( l_name     ) ) DEALLOCATE( l_name     )
    IF ( ALLOCATED( l_idx      ) ) DEALLOCATE( l_idx      )
    !
  END SUBROUTINE create_omi_l_ind_i_d
  !
END MODULE m_ipds_omi
  !
! TailOfPackageModule -----------------------------------------------------
