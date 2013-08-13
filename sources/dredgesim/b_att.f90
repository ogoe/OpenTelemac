! -------------------------------------------------------------------------
! HeadOfBaseModule --------------------------------------------------------
!
!! <H2>Typ+Methoden f&uuml;r das Ablegen von Attributen zu Daten, Dateien</h2>
!! @author <A HREF="mailto:lang@hamburg.baw.de">G. Lang</A>, <A HREF="mailto:i.uliczka@hamburg.baw.de">I. Uliczka</A>
!! @version 2.5 vom 03/06/07, Quellcode: mod_b_att.f90
!! <HR>
!! type and methods to deal with attributes related to data, files <BR>
!! <HR>
!  Copyright-Hinweis
!                                                  <BR>
!  Copyright (C) 2005 Bundesanstalt fuer Wasserbau <BR>
!                                                  <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2002-10-14 : G. Lang, I. Uliczka : Startversion
!  01.02 : 2002-11-26 : G. Lang     : neues Attribut "dynamic_bathymetry"
!  01.03 : 2002-12-06 : S. Spohr    : fuer scale_factor sind jetzt auch negative Werte zulaessig
!  01.04 : 2002-08-02 : G. Lang     : ok_dynamic_bathymetry korrigiert
!  01.05 : 2002-08-02 : G. Lang     : Anpassungen TV12 vom Dez. 2002
!  01.06 : 2003-08-29 : P. Schade   : Functions get_att_field_idx* 
!  01.07 : 2003-12-17 : G. Lang     : neue Funktion "get_att_related_file"
!  01.08 : 2003-12-17 : G. Lang     : Korrektur in "get_att_related_file_0"
!  01.09 : 2003-12-17 : G. Lang     : neue Funktion is_datetime_in_att_period
!  01.10 : 2004-01-19 : H. Weilbeer : INTENT(OUT) teilweise INTENT(INOUT)
!  01.11 : 2004-01-26 : G. Lang     : neu: is_datetime_in_att
!  01.12 : 2004-01-28 : G. Seiss    : !>WIN-NT: Kommentare eingefuegt
!  01.13 : 2004-06-28 : S. Spohr    : + Attribute : "settling_velocity", "settling_velocity_formula" 
!  01.14 : 2004-06-28 : G. Lang     : Schreibfehler eliminiert
!  01.15 : 2004-07-01 : G. Lang     : weitere Attribute eingefuegt
!  01.16 : 2004-05-09 : G. Lang     : Erweiterungen fuer io_dataset-Einbindung
!  01.17 : 2005-06-02 : G. Lang     : undefinierte Variable in is_datetime_in_att_period_0 definiert
!  01.18 : 2005-07-06 : G. Lang     : ok_file_type-Aufrufe anpassen    
!  01.19 : 2005-08-10 : G. Lang     : neue Funktion has_att_cross_section_average
!  02.01 : 2006-03-29 : G. Lang     : Erweiterungen CF-Metadaten/Delft3D-NetCDF-Schnittstelle
!  02.02 : 2006-05-03 : G. Lang     : ok_att_consistency_d, keine FM 6618 wegen evtl. Konflikt mit BDF-!  02.03 : 2006-10-04 : P. Schade   : Sicherheitscheck ok_layer_interfaces_0 erst ab 2. Element
!  02.03 : 2007-03-05 : G. Lang     : has_att_related_file: Pruefen, ob eine bestimmte Datei enthalten ist
!  02.04 : 2007-03-06 : G. Lang     : get_att_systemfile: Ermittle die Systemdatei
!
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! <UL>
!!    <LI> Typ und Methoden f&uuml;r Attribute;
!!    <LI> neben einer Kennung <EM>id</EM> und einem Namen <EM>name</EM>
!!         wird eine Variablen-Id <EM>var_id</EM> zugewiesen:
!!         <UL>
!!            <LI> falls var_id gr&ouml;&szlig;er als Null ist, so
!!                 ist das Attribut der Variableben mit der entsprechenden
!!                 Id zugeordnet;
!!            <LI> falls var_id Null ist, so handelt es sich bei dem
!!                 betreffenden Attribut um ein <EM>globales</EM> Attribut,
!!                 welches zum Beispiel einer Datei zugeordnet ist.
!!         </UL>
!!    <LI> ein Attribut_Objekt kann in Zusammenhang mit der Definition
!!         von Variablen oder Dateiinhalten wieder verwendet werden;
!!    <LI> einem Attribut kann als Information entweder
!!         <UL>
!!            <LI> Texte (speziell Datum+Uhrzeit+Zeitzone), 
!!            <LI> ganze Zahlen oder
!!            <LI> reellwertige Gr&ouml;&szlig;en enthalten.
!!         </UL>
!! </UL>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt den selbst definierten Datentyp <EM>t_att</EM> 
!! zur Verf&uuml;gung. Dieser besteht aus den folgenden Komponenten:      <BR>
!! <OL>
!!     <LI> <TT>id</TT>     : kennzeichnende Identifikationsnummer (Kennung) 
!!     <LI> <TT>name</TT>   : Klartext-Bezeichnung des Attributs (z.B. <EM>title</EM>)
!!     <LI> <TT>var_id</TT> : kennzeichnende Identifikationsnummer der Variablen
!!                            der dieses Attribut zugeordnet ist; falls
!!                            var_id=0 ist, dann handelt es sich um ein globales
!!                            Attribut
!!     <LI> <TT>ch</TT>     : Pointer-Feld zur Aufnahme von Text(en)
!!     <LI> <TT>in</TT>     : Pointer-Feld zur Aufnahme von ganzzahligen Werten
!!     <LI> <TT>dp</TT>     : Pointer-Feld zur Aufnahme von reellen Werten
!! </OL>
!!                                                                  <BR>
!! <HR>
!! <H3>Verwenden des Moduls</H3>
!!                                                                  <BR>
!! Die Leistungen des Moduls k&ouml;nnen wie folgt in Anspruch genommen werden: <BR>
!! <OL>
!!    <LI> Einbinden des Moduls mittels USE-Anweisung in der rufenden Programmeinheit;
!!    <LI> Initialisieren des Moduls b_att mit INIT-Methode;
!!    <LI> ggf. Verwenden verschiedener SETUP-Methoden;
!!    <LI> Verwenden beliebiger Modul-Methoden (nicht INIT und CLEAR);
!!    <LI> De-Initialisieren des Moduls b_att mit CLEAR-Methode.
!! </OL>
!! <EM>Anmerkung</EM>: Ein Attribut kann entweder Text oder ganze Zahlen
!!                     oder reelle Zahlen aufnehmen, aber nicht eine
!!                     Mischung davon.
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
!!          die Methode PRINT_ATT_ALL_ERRORS.
!!                                                                    <BR>
!
MODULE b_att
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] BASIS-Modul mit globalen Konstantwerten
  !
  USE b_constants, ONLY : &
       ! Parameter
       Double
  !
  ! [A.2] BASIS-Modul mit Fehler-Typ und -Routinen 
  !
  USE b_error, ONLY :       &
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
  ! [A.3] BASIS-Modul mit Type/Methoden f&uuml;r Datum und Uhrzeit
  !
  USE b_datetime, ONLY :        &
       ! Typdefinition
       t_datetime,              &
       ! Routinen / Interfaces
       init_datetime,           &
       clear_datetime,          &
       setup_datetime_prn_lun,  &
       setup_datetime_trc_lun,  &
       setup_datetime_language, &
       new_datetime,            &
       ok_datetime,             &
       get_datetime_language,   &
       set_datetime,            &
       OPERATOR(<=),            &
       OPERATOR(==),            &
       OPERATOR(>=),            &
!>WIN-NT:     eq_datetime_0_0,        &
!>WIN-NT:     eq_datetime_0_1,        &
!>WIN-NT:     eq_datetime_1_0,        &
!>WIN-NT:     eq_datetime_1_1,        &
!>WIN-NT:     ne_datetime_0_0,        &
!>WIN-NT:     ne_datetime_0_1,        &
!>WIN-NT:     ne_datetime_1_0,        &
!>WIN-NT:     ne_datetime_1_1,        &
!>WIN-NT:     le_datetime_0_0,        &
!>WIN-NT:     le_datetime_0_1,        &
!>WIN-NT:     le_datetime_1_0,        &
!>WIN-NT:     le_datetime_1_1,        &
!>WIN-NT:     ge_datetime_0_0,        &
!>WIN-NT:     ge_datetime_0_1,        &
!>WIN-NT:     ge_datetime_1_0,        &
!>WIN-NT:     ge_datetime_1_1,        &
       datetime_to_string,      &
       string_to_datetime,      &
       get_datetime_from_system
  !
  ! [A.4] BASIS-Modul mit Type/Methoden f&uuml;r Variablen
  !
  USE b_var, ONLY :        &
       ! Typdefinition
       t_var,              &
       ! Routinen / Interfaces
       init_var,           &
       clear_var,          &
       setup_var_prn_lun,  &
       setup_var_trc_lun,  &  
       get_var_id,         &
       get_var_name,       &
       get_var_type,       &
       set_var_id,         &
       var_exists,         &
       get_var_idx
  !
  ! [A.5] BASIS-Modul mit Type/Methoden f&uuml;r Dateien
  !
  USE b_file, ONLY :       &
       ! Datentyp
       t_file,             &
       ! Konstante
       c_file_type_len,    & 
       ! Routinen / Interfaces
       init_file, clear_file, &
       setup_file_prn_lun, setup_file_trc_lun, &
       ok_file_type,       &
       file_is_formatted, file_is_unformatted, file_is_direct, file_is_sequential, &
       new_file,           &
       set_file_name, set_file_form, set_file_type, set_file_access, &
       get_file_type, get_file_systemfile, &
       eq_file_path_and_name
  !
  ! ---------------------------------------------------------------------
  ! [B] alles muss explizit deklariert werden und ist default privat
  ! ---------------------------------------------------------------------
  !
  IMPLICIT NONE
  PRIVATE
  !
  ! ---------------------------------------------------------------------
  ! [C] oeffentlich zugaengliche Deklarationen (mit PUBLIC-Attribut)
  ! ---------------------------------------------------------------------
  !
  ! [C.0] max. Anzahl zulaessiger Zeichen in verschiedenen Komponenten von (t_att) <BR>
  !! maximale Anzahl der Zeichen in der Komponenten <EM>name</EM> von (t_att)
  INTEGER , PARAMETER , PUBLIC :: c_len_att_name=40 ! 
  !! maximale Anzahl der Zeichen in der Komponenten <EM>name</EM> von (t_att)
  INTEGER , PARAMETER , PUBLIC :: c_len_att_ch=80   ! 

  ! [C.1] (maximal) einen oeffentlich zugaenglichen Typ deklarieren
  !
  !! Typ zur Aufnahme der zu einem Attribut geh&ouml;renden Daten <BR>
  !! von den Pointer-Feldern darf immer nur eines allokiert sein <BR>
  !! <TT>id</TT>     : kennzeichnende Identifikationsnummer (Kennung) <BR>
  !! <TT>name</TT>   : Klartext-Bezeichnung des Attributs (z.B. <EM>title</EM>) <BR>
  !! <TT>var_id</TT> : kennzeichnende Identifikationsnummer der Variablen
  !!                   der dieses Attribut zugeordnet ist; falls
  !!                   var_id=0 ist, dann handelt es sich um ein globales
  !!                   Attribut <BR>
  !! <TT>ch</TT>     : Pointer-Feld zur Aufnahme von Text <BR>
  !! <TT>in</TT>     : Pointer-Feld zur Aufnahme von ganzzahligen Werten <BR>
  !! <TT>dp</TT>     : Pointer-Feld zur Aufnahme von reellen Werten
  TYPE , PUBLIC :: t_att
     PRIVATE
     INTEGER                                :: id     !  
     CHARACTER (LEN=c_len_att_name)         :: name   ! 
     INTEGER                                :: var_id ! 
     CHARACTER (LEN=c_len_att_ch) , POINTER :: ch(:)  ! 
     INTEGER                      , POINTER :: in(:)  ! 
     REAL (KIND=Double)           , POINTER :: dp(:)  ! 
  END TYPE t_att
  !
  ! [C.2] Konstantwerte (Parameter)
  !
  !! maximale Anzahl g&uuml;ltiger Attributnamen (bei Bedarf erweitern)
  INTEGER, PUBLIC, PARAMETER :: c_max_att_name=105 ! 
  !! maximale Anzahl g&uuml;ltiger Angaben f&uuml;r <<horizontal_coordinate_system_definition>>
  INTEGER, PUBLIC, PARAMETER :: c_max_hcs=9 !
  !! maximale Anzahl g&uuml;ltiger Angaben f&uuml;r <<map_projection>>
  INTEGER, PUBLIC, PARAMETER :: c_max_map_proj=2 !
  !! maximale Anzahl g&uuml;ltiger Angaben f&uuml;r <<horizontal_datum_name>>
  INTEGER, PUBLIC, PARAMETER :: c_max_hdn=2 !
  !! maximale Anzahl g&uuml;ltiger Angaben f&uuml;r <<ellisoid_name>>
  INTEGER, PUBLIC, PARAMETER :: c_max_elli_name=2 !
  !! maximale Anzahl g&uuml;ltiger Angaben f&uuml;r <<vertical_coordinate_system_definition>>
  INTEGER, PUBLIC, PARAMETER :: c_max_vcs=6 !
  !! maximale Anzahl g&uuml;ltiger Angaben f&uuml;r <<horizontal_discretisation>>
  INTEGER, PUBLIC, PARAMETER :: c_max_hdisc=15 !
  !! maximale Anzahl g&uuml;ltiger Angaben f&uuml;r <<vertical_discretisation>>
  INTEGER, PUBLIC, PARAMETER :: c_max_vdisc=9 !
  !! maximale Anzahl g&uuml;ltiger Angaben f&uuml;r <<storage_type>>
  INTEGER, PUBLIC, PARAMETER :: c_max_st_type=18 !
  !! maximale Anzahl der Zeichen zum Kennzeichnen der zul&auml;ssigen Attribut-Verwendung
  INTEGER, PUBLIC, PARAMETER :: c_len_att_cdg=3      ! 
  !! maximale Anzahl der Zeichen zum Kennzeichnen des Attribut-Typs
  INTEGER, PUBLIC, PARAMETER :: c_len_att_typ=3      ! 
  !! maximale Anzahl der Zeichen in den zul&auml;ssigen <<grid_mapping_name>>
  INTEGER, PUBLIC, PARAMETER :: c_len_grid_mapping_name=28 !  
  !! maximale Anzahl der Zeichen in den zul&auml;ssigen Bezeichnungen f&uuml;r <<axis>>
  INTEGER, PUBLIC, PARAMETER :: c_len_axis=1               ! 
  !! maximale Anzahl der Zeichen in den zul&auml;ssigen Bezeichnungen f&uuml;r <<calendar>>
  INTEGER, PUBLIC, PARAMETER :: c_len_calendar=19          ! 
  !! maximale Anzahl der Zeichen in den zul&auml;ssigen Bezeichnungen f&uuml;r <<cell_measures>>
  INTEGER, PUBLIC, PARAMETER :: c_len_cell_measures=6      ! 
  !! maximale Anzahl der Zeichen in den zul&auml;ssigen Bezeichnungen f&uuml;r <<cell_methods>>
  INTEGER, PUBLIC, PARAMETER :: c_len_cell_methods=18      ! 
  !! maximale Anzahl der Zeichen in den zul&auml;ssigen Bezeichnungen f&uuml;r <<positive>>
  INTEGER, PUBLIC, PARAMETER :: c_len_positive=4           ! 
  !! maximale Anzahl der Zeichen in den zul&auml;ssigen Bezeichnungen f&uuml;r <<standard_name modifier>>
  INTEGER, PUBLIC, PARAMETER :: c_len_standard_name_modifier=22 ! 
  !
  !! maximale Anzahl der zul&auml;ssigen Angaben f&uuml;r <<grid_mapping_name>>
  INTEGER, PUBLIC, PARAMETER :: c_max_grid_mapping_name=7  ! 
  !! maximale Anzahl der Attribute f&uuml;r "grid_mapping_parameter"
  INTEGER, PUBLIC, PARAMETER :: c_max_grid_mapping_att=5   ! 
  !! maximale Anzahl der alternativen Angaben f&uuml;r
  INTEGER, PUBLIC, PARAMETER :: c_max_grid_mapping_alt=2   !  
  !! maximale Anzahl der zul&auml;ssigen Bezeichnungen f&uuml;r <<axis>>
  INTEGER, PUBLIC, PARAMETER :: c_max_axis=4               ! 
  !! maximale Anzahl der zul&auml;ssigen Bezeichnungen f&uuml;r <<calendar>>
  INTEGER, PUBLIC, PARAMETER :: c_max_calendar=10          ! 
  !! maximale Anzahl der zul&auml;ssigen Bezeichnungen f&uuml;r <<cell_measures>>
  INTEGER, PUBLIC, PARAMETER :: c_max_cell_measures=2      ! 
  !! maximale Anzahl der zul&auml;ssigen Bezeichnungen f&uuml;r <<cell_methods>>
  INTEGER, PUBLIC, PARAMETER :: c_max_cell_methods=10      ! 
  !! maximale Anzahl der zul&auml;ssigen Bezeichnungen f&uuml;r <<positive
  INTEGER, PUBLIC, PARAMETER :: c_max_positive=2           ! 
  !! maximale Anzahl der Zeichen in den zul&auml;ssigen Bezeichnungen f&uuml;r <<standard_name modifier>>
  INTEGER, PUBLIC, PARAMETER :: c_max_standard_name_modifier=4 ! 
  !
  ! ----------------------------------------------------------------------------------------
  ! >>> neue Attribute bitte hinten anfuegen <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ! ----------------------------------------------------------------------------------------
  !
  !! <BR>
  !! <CENTER>
  !! <STRONG>Vorbemerkungen</STRONG> 
  !! </CENTER> <BR>
  !! Bei der Namensgebung f&uuml;r die Attribute (Schl&uuml;sselworte) wurden der
  !! <OL>
  !! <LI> Content Standard for Digital Geospatial Metadata</EM>, der
  !! <LI> <EM>NetCDF User s Guide for Fortran90</EM>, sowie ab Version 2.1 der
  !! <LI> <A HREF="http://www.cgd.ucar.edu/cms/eaton/cf-metadata/CF-1.0.html"><EM>CF-Metadaten Standard</EM></A>
  !! </OL>
  !! mit einigen BAW-spezifischen Erweiterungen ber&uuml;cksichtigt. Nur die
  !! Informationen in den mit <EM><STRONG>Fettschrift</STRONG></EM> gekennzeichneten
  !! Attributen k&ouml;nnen derzeit in BDF-Dateien &uuml;bernommen werden, die anderen
  !! werden nicht in diese &uuml;bertragen. Die anderen Datenformate sind dagegen
  !! in der Lage, alle in Attributen gespeicherten Informationen aufzunehmen.
  !
  !! <BR>
  !! <BR>
  !! <CENTER><STRONG>globale Attribute</STRONG></CENTER>
  !! <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! <P>
  !! (86) = <EM>Conventions</EM>: [CF] <BR>
  !!        Bezeichnung die standardisierten NetCDF-Konventionen, denen der Datensatz gen&uuml;gt. <BR>
  !!        Der Wert wird als relativer Verzeichnispfad interpretiert, in welchem die 
  !!        disziplinen-spezifischen Konventionen abgelegt sind. Hierdurch wird eine hierarchische
  !!        Struktur unterst&uuml;tzt. <BR>
  !!        Augenblicklich wird der Name des die Konventionen enthaltenden Verzeichnis relativ zu 
  !!        <TT>pub/netcdf/Conventions/</TT> auf <TT>ftp.unidata.ucar.edu</TT> angegeben. <BR>
  !!        Alternativ darf auch eine vollst&auml;ndige URL angeben werden. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (01) = <EM>title</EM>: [CF] <BR>
  !!        Titel der Simulation, Studie, Me&szlig;kampagne, also eine kurze Beschreibung des 
  !!        Inhalts der Datei (soll immer vorhanden sein) <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (02) = <EM><STRONG>history</STRONG></EM>: [CF] <BR>
  !!        Filter-Informationen; jedes Programm, dass auf die Daten zugreift, erg&auml;nzt eine 
  !!        weitere Textzeile mit Angaben zu Datum und Uhrzeit, User, Programm und Optionen, 
  !!        so dass der Verarbeitungsweg der Daten verfolgt werden kann (muss immer vorhanden sein) 
  !!        <EM>Hinweis</EM>: Routine <TT>add_att_history</TT> <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (84) = <EM>comment</EM>: [CF] <BR>
  !!        Verschiedene Informationen zu den Daten und Methoden die bei Ihrer Erstellung 
  !!        verwendet wurden. <BR>
  !!        Hinweis: kann auch variablenbezogen verwendet werden. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (92) = <EM>institution</EM>: [CF] <BR>
  !!        Angaben zur Organisation, welche die originalen Daten produziert hat. 
  !!        Bestenfalls sollten die Angaben im Internet zur Verf&uuml;gung stehen. <BR>
  !!        Hinweis: kann auch variablenbezogen verwendet werden. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (97) = <EM>references</EM>: [CF] <BR>
  !!        Weitere erg&auml;nzende Hinweise zu den Daten oder zu den f&uuml;r ihre 
  !!        Erzeugung eingesetzten Methoden. <BR>
  !!        Bestenfalls sollten die Angaben im Internet zur Verf&uuml;gung stehen. <BR>
  !!        Hinweis: kann auch variablenbezogen verwendet werden. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (98) = <EM>source</EM>: [CF] <BR>
  !!        Hinweise zur Methode der Datenerzeugung, z.B. Name des eingesetzten
  !!        Modells und Modellversion. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (103) = <EM>astro</EM>: [WLD] <BR>
  !!        Hinweis auf astronomische Einfl&uuml;sse. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (104) = <EM>grid_type</EM>: [WLD] <BR>
  !!        Hinweis auf den Typ des Gitters. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P>
  !! <BR>
  !
  !! <CENTER><EM>Informationen zu Uhrzeit und Datum</EM></CENTER>
  !! <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! <P>
  !! (03) = <EM><STRONG>beginning_date</STRONG></EM>: [BAW] <BR>
  !!        Anfangsdatum und -zeit (soll immer vorhanden sein, insofern die Daten einen Zeitbezug haben) <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (04) = <EM><STRONG>ending_date</STRONG></EM>: [BAW] <BR>
  !!        Endedatum und -zeit (soll immer vorhanden sein insofern die Daten einen Zeitbezug haben) <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (05) = <EM><STRONG>reference_date</STRONG></EM>: [BAW] <BR>
  !!        Referenzdatum- und zeit f&uuml;r Zeitursprung (muss immer dann vorhanden sein, wenn in 
  !!        der Datei auch relative Zeitangaben vorhanden sind, wie dies z.B. bei einer BDF-Datei 
  !!        die &auml;quidistante Zeitserien enth&auml;lt der Fall ist) <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (06) = <EM>single_date</EM>: [BAW] ] <BR>
  !!        eine Datums- und Zeitangabe <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (07) = <EM><STRONG>multiple_dates</STRONG></EM>: [BAW] <BR>
  !!        mehrere Datums- und Zeitangaben (muss dann vorhanden sein, wenn die Datei eine 
  !!        gr&ouml;&szlig;ere Anzahl von Datens&auml;tzen f&uuml;r verschiedene Termine 
  !!        enth&auml;lt) <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P>
  !! <BR>
  !
  !! <CENTER><EM>Informationen zur geographischen Lage</EM></CENTER>
  !! <BR>
  !! F&uuml;r eine eventuelle sp&auml;tere automatische Erzeugung von Metadaten sollte 
  !! man sich allm&auml;hlich angew&ouml;hnen, diese Informationen bereitzustellen.
  !! <BR>
  !! <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! <P>
  !! (13) = <EM>false_easting</EM>: [CF] <BR>
  !!        Wert der zu allen Abszissenwerten in einem rechtwinkligen Koordinatensystem
  !!        hinzuaddiert wird. Dieser Wert wird h&auml;fig dazu verwendet, um negative
  !!        Koordinatenwerte zu eliminieren. Ist in den Einheiten der Koordinate mit dem
  !!        Namen <TT>projection_x_coordinate</TT> anzugeben. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (14) = <EM>false_northing</EM>: [CF] <BR>
  !!        Wert der zu allen Ordinatenwerte in einem rechtwinkligen Koordinatensystem
  !!        hinzuaddiert wird. Dieser Wert wird h&auml;fig dazu verwendet, um negative
  !!        Koordinatenwerte zu eliminieren. Ist in den Einheiten der Koordinate mit dem
  !!        Namen <TT>projection_y_coordinate</TT> anzugeben. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (67) = <EM>grid_mapping_name</EM>: [CF] <BR>
  !!        Name zur n&auml;heren Identifikation der Kartenprojektion.
  !!        <EM>Hinweis:</EM> Liste der zul&auml;ssigen Namen beachten (<TT>c_grid_mapping_name</TT>). <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (68) = <EM>grid_north_pole_latitude</EM>: [CF] <BR>
  !!        Wahre Breite (in Grad Nord) des Nordpols eines rotierten Gitters (-90 .LE. <EM>wert</EM> .LE. 90). <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (69) = <EM>grid_north_pole_longitude</EM>: [CF] <BR>
  !!        Wahre L&auml;nge (in Grad Ost) des <EM>Nordpols eines rotierten Gitters</EM> (-180 .LE. <EM>wert</EM> .LT. 180). <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (70) = <EM>latitude_of_projection_origin</EM>: [CF] <BR>
  !!        Breitengrad der als Ursprung eines rechtwinkligen Koordinatensystems f&uuml;r eine
  !!        Kartenprojektion gew&auml;hlt wurde (-90 .LE. <EM>wert</EM> .LE. 90). <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (12) = <EM>longitude_of_central_meridian</EM>: [CF] <BR> 
  !!        zentraler L&auml;ngengrad einer Kartenprojektion ( -180 .LE. <EM>wert</EM> .LT. 180. ) <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (71) = <EM>longitude_of_projection_origin</EM>: [CF] <BR>
  !!        L&auml;ngengrad der als Ursprung eines rechtwinkligen Koordinatensystems f&uuml;r eine
  !!        Kartenprojektion gew&auml;hlt wurde (-180 .LE. <EM>wert</EM> .LT. 180). <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (72) = <EM>north_pole_grid_longitude</EM>: [CF] <BR>
  !!        L&auml;ngengrad des <EM>wahren Nordpols in einem rotierten Gitter</EM> (-180 .LE. <EM>wert</EM> .LT. 180). <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (73) = <EM>scale_factor_at_central_meridian</EM>: [CF] <BR>
  !!        Ma&szlig;stabsfaktor zur Umrechnung einer Entfernung aus den von einer Karte
  !!        abgegriffenen Werten entlang des zentralen Meridians (<EM>wert</EM> .GT. 0). <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (74) = <EM>scale_factor_at_projection_origin</EM>: [CF] <BR>
  !!        Ma&szlig;stabsfaktor zur Umrechnung einer Entfernung aus den von einer Karte
  !!        abgegriffenen Werten im Zentrum der Kartenprojektion (<EM>wert</EM> .GT. 0). <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (75) = <EM>standard_parallel</EM>: [CF] <BR>
  !!        Linie konstanter Breite bei der sich die Erdoberfl&auml;che mit der Ebene oder
  !!        Abwicklungsfl&auml;che miteinander schneiden. Ggf. k&ouml;nnen zwei Werte
  !!        angegeben werden, falls zwei Parallelen existieren (-90 .LE. <EM>wert</EM> .LE. 90). <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (76) = <EM>straight_vertical_longitude_from_pole</EM>: [CF] <BR>
  !!        Breitengrad der in einer Kartendarstellung gerade und senkrecht nach oben vom
  !!        Nord- zum S&uuml;dpol dargestellt werden soll (-180 .LE. <EM>wert</EM> .LT. 180). <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (08) = <EM>horizontal_coordinate_system_definition</EM>: [BAW] <BR> 
  !!        geographic|planar|local].
  !!        <EM>Hinweis</EM>: m&ouml;glichst <TT>grid_mapping_name</TT> verwenden. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (09) = <EM>map_projection</EM>: [BAW]  <BR>
  !!        Projektionsmethode (z.B. Mercator).
  !!        <EM>Hinweis</EM>: m&ouml;glichst <TT>grid_mapping_name</TT> verwenden. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (10) = <EM>horizontal_datum_name</EM>: [BAW]  <BR>
  !!        Lagebezugspunkt (z.B. Potsdam) <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (11) = <EM>ellipsoid_name</EM>: [BAW]  <BR>
  !!        Ellipsoid (z.B. Bessel)        <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (15) = <EM>west_bounding_coordinate</EM>: [BAW] <BR>
  !!        westlicher Rand <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (16) = <EM>east_bounding_coordinate</EM>: [BAW]  <BR>
  !!        &ouml;stlicher Rand                       <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (17) = <EM>north_bounding_coordinate</EM>: [BAW]  <BR>
  !!        n&ouml;rdlicher Rand                     <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (18) = <EM>south_bounding_coordinate</EM>: [BAW]  <BR>
  !!        s&uuml;dlicher Rand                      <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (105) = <EM>coordinate_system</EM>: [WLD] <BR> 
  !!        <EM>Hinweis</EM>: m&ouml;glichst <TT>grid_mapping_name</TT> verwenden. <BR>
  !! </P>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! <BR>
  !
  !! <CENTER><EM>Informationen zum H&ouml;hen-/Tiefenbezug</EM></CENTER>
  !! <BR>
  !! F&uuml;r eine eventuelle sp&auml;tere automatische Erzeugung von Metadaten sollte 
  !! man sich allm&auml;hlich angew&ouml;hnen, diese Informationen bereitzustellen.
  !! <BR>
  !! <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! <P>
  !! (19) = <EM>vertical_coordinate_system_definition</EM>: [BAW]  <BR>
  !!        H&ouml;hen- oder Tiefenbezug der vertikalen Koordinaten [altitude|depth].
  !!        <EM>Hinweis</EM>: m&ouml;glichst Attribut <TT>positive</TT> bei Achsendefinition verwenden. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (20) = <EM>altitude_datum_name</EM>: [BAW]  <BR>
  !!        H&ouml;henbezug. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (21) = <EM>depth_datum_name</EM>: [BAW]  <BR>
  !!        Tiefenbezug. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P>
  !! <BR>
  !
  !! <CENTER><EM>Informationen zum Format der Datei</EM></CENTER>
  !! <BR>
  !! F&uuml;r eine eventuelle sp&auml;tere automatische Erzeugung von Metadaten sollte 
  !! man sich allm&auml;hlich angew&ouml;hnen, diese Informationen bereitzustellen.
  !! <BR>
  !! <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! <P>
  !! (22) = <EM>format_name</EM>: [BAW]  <BR>
  !!        Name des Formats (z.B. BDF) <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (23) = <EM><STRONG>format_version_no</STRONG></EM>: [BAW]  <BR>
  !!        Versionsnummer des Formats   <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P>
  !! <BR>
  !
  !! <CENTER><EM>Informationen zum thematischen Inhalt der Datei</EM></CENTER><BR>
  !! Zu geeigneten Schl&uuml;sselw&ouml;rtern existieren viele Quellen. Es wird
  !! empfohlen, aus den unter 
  !! <A HREF="http://nokis.baw.de/Links.12.0.html">NOKIS-Metadata</A>
  !! genannten auszuw&auml;hlen.
  !! <BR>
  !! <BR>
  !! F&uuml;r eine eventuelle sp&auml;tere automatische Erzeugung von Metadaten sollte 
  !! man sich allm&auml;hlich angew&ouml;hnen, diese Informationen bereitzustellen.
  !! <BR>
  !! <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! <P>
  !! (24) = <EM>theme_keywords</EM>: [BAW] <BR>
  !!        Stichworte zum thematischen Inhalt      <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (25) = <EM>place_keywords</EM>: [BAW] <BR>
  !!        geographische Bezeichnungen             <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (26) = <EM>temporal_keywords</EM>: [BAW] <BR>
  !!        zeitliche Bezeichnungen                 <BR>
  !! </P>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! <BR>
  !! es wird empfohlen, englische Begriffe zur Beschreibung zu verwenden                 <BR>
  !! <BR>
  !
  !! <CENTER><EM>Informationen zu eng mit den Daten verkn&uuml;pften Dateien (BAW)</EM></CENTER>
  !! <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! <P>
  !! (27) = <EM><STRONG>related_file_name</STRONG></EM>: [BAW]  <BR>
  !!        Dateiname                          <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (28) = <EM><STRONG>related_file_form</STRONG></EM>: [BAW]  <BR>
  !!        ... Format [FORMATTED,UNFORMATTED] <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (29) = <EM><STRONG>related_file_access</STRONG></EM>: [BAW] <BR>
  !!        ... Zugriff [DIRECT,SEQUENTIAL]  <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (30) = <EM><STRONG>related_file_type</STRONG></EM>: [BAW] <BR>
  !!        ... TYPE [GITTER05,BDF,...]        <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P>
  !! <BR>
  !
  !! <CENTER><EM>Informationen zu besonderen Positionen (BAW)</EM></CENTER>
  !! <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! <P>
  !! (31) = <EM><STRONG>reference_location_index</STRONG></EM>: [BAW] <BR>
  !!        Ref. Position (Knotennummer)  <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (32) = <EM><STRONG>reference_location_coordinates</STRONG></EM>: [BAW] <BR>
  !!        ... Koordinaten         <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (33) = <EM><STRONG>zero_phase_location_index</STRONG></EM>: [BAW] <BR>
  !!        Bez. Position (Knotennummer) <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (34) = <EM><STRONG>zero_phase_location_coordinates</STRONG></EM>: [BAW] <BR>
  !!        ... Koordinaten        <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P>
  !! <BR>
  !
  !! <CENTER><EM>Informationen zur vertikalen Gitterstruktur (BAW)</EM></CENTER>
  !! <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! <P>
  !! (35) = <EM><STRONG>sigma_interfaces</STRONG></EM>: [BAW] <BR>
  !!        Lage der Schichtgrenzen (sigma-Koordinaten) <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (36) = <EM><STRONG>layer_interfaces</STRONG></EM>: [BAW] <BR>
  !!        Lage der Schichtgrenzen (z-Koordinaten)     <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P>
  !! <BR>
  !
  !! <CENTER><EM>sonstige Informationen (BAW)</EM></CENTER>
  !! <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! <P>
  !! (37) = <EM><STRONG>minimum_water_depth</STRONG></EM>: [BAW] <BR>
  !!        minimale Wasserbedeckung          <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (57) = <EM><STRONG>dynamic_bathymetry</STRONG></EM>: [BAW] <BR>
  !!        (morpho-) dynamische Bathymetrie  <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (60) = <EM><STRONG>directional_sector</STRONG></EM>: [BAW] <BR>
  !!        Richtungssektorbezeichnung        <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (61) = <EM><STRONG>cross_sectional_average</STRONG></EM>: [BAW] <BR>
  !!        querschnittsintegriert            <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P>
  !! <BR>
  !!
  !! <CENTER><STRONG>variablenbezogene Attribute</STRONG></CENTER>
  !! <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! <P>
  !! (38) = <EM><STRONG>long_name</STRONG></EM>: [CF]  <BR>
  !!        Ein beschreibender Name aus welchem auf den Inhalt der Variablen 
  !!        geschlossen werden kann. Hierf&uuml;r existieren keine international 
  !!        standardisierten Bezeichnungen. Innerhalb der BAW sollte ein Name 
  !!        m&ouml;glichst gem&auml;&szlig; "phydef" verwendet werden (muss 
  !!        immer angegeben werden).   <BR>
  !!        Kann zur Bezeichnung in Plottdarstellungen verwendet werden. Falls
  !!        das Attribut nicht vorhanden ist, kann alternativ auch auf den
  !!        zumeist kryptischen Standardnamen <TT>standard_name</TT>
  !!        zur&uuml;ckgegriffen werden. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (39) = <EM>short_name</EM>: [BAW]  <BR>
  !!        Ein kurzer beschreibender, eindeutiger Name. Hierf&uuml;r existieren 
  !!        keine international standardisierten Bezeichnungen. Innerhalb der BAW 
  !!        sollte ein Name m&ouml;glichst gem&auml;&szlig; "phydef" verwendet 
  !!        werden. <BR>
  !!        Kann zur (kurzen) Bezeichnung physikalischer Gr&ouml;&szlig;en
  !!        in Auswahlmenues verwendet werden. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (40) = <EM><STRONG>units</STRONG></EM>: [CF] <BR>
  !!        Einheit der phys. Gr&ouml;&szlig;e, m&ouml;glichst gem&auml;&szlig; 
  !!        <A HREF="http://www.unidata.ucar.edu/software/udunits/">UDUNITS</A>, so dass
  !!        automatische Konversionsroutinen darauf <EM>losgelassen</EM> werden
  !!        k&ouml;nnen; oder aus "phydef". F&uuml;r <EM>Randvariablen</EM>
  !!        oder <EM>klimatologische Variablen</EM> sowie ggf. dimensionslose
  !!        Variablen kann die Angabe entfallen. <BR>
  !!        F&uuml;r die Zeitvariable mu&szlig; daraus das Bezugsdatum sowie die
  !!        Einheiten der Koordinate Zeit ersichtlich werden. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (46) = <EM>scale_factor</EM>: [CF]  <BR>
  !!        Skalierfaktor mit dem die auf Datei stehenden Daten nach dem Lesen 
  !!        multipliziert werden sollen, bevor ein eventueller Offset ber&uuml;cksichtigt
  !!        wird (sollte immer angegeben werden) <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (50) = <EM>add_offset</EM>: [CF] <BR>
  !!        Offset (kann angegeben werden). <BR>
  !!        Dieser Wert wird nach dem Lesen zur Variablen hinzuaddiert, nachdem
  !!        diese ggf. zuvor skaliert wurden. Beim Schreiben ist die Vorgehensweise
  !!        umgekehrt. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (47) = <EM>valid_range</EM>: [CF] <BR>
  !!        G&uuml;ltigkeitsbereich der Variablen (sollte immer angegeben werden). <BR>
  !!        Es sind zwei Werte mit den minimalen und maximalen Werten f&uuml;r diese Variable,
  !!        gleichwertig zur Vorgabe von <TT>valid_min</TT> bzw. <TT>valid_max</TT>
  !!        anzugeben. <TT>valid_range</TT> darf nicht vorhanden sein wenn zumindest
  !!        eines der beiden zuvor genannten alternativen Attribute vorhanden ist. <BR>
  !!        Generische Anwendungen behandeln aller au&szlig;erhalb des vorgegebenen
  !!        Wertebereichs liegende Daten als <EM>missing</EM>. Falls keines der drei
  !!        genannten Attribute vereinbart wurde, dann wird <TT>_FillValue</TT>
  !!        zur Definition des G&uuml;tigkeitsbereichs herangezogen. Wurde ein
  !!        G&uuml;ltigkeitsbereich vereinbart, so sollte <TT>_FillValue</TT> 
  !!        au&szlig;erhalb davon liegen. <BR>
  !!        <EM>Hinweis</EM>: es wird empfohlen, <TT>valid_min</TT> und <TT>valid_max</TT>
  !!        zu verwenden. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (49) = <EM>missing_value</EM>: [CF] <BR>
  !!        Wert zum Kennzeichnen ung&uuml;tiger bzw. undefinierter Daten (kann angegeben werden). <BR>
  !!        Der Wert sollte au&szlig;erhalb eines eventuell angegebenen G&uuml;ltigkeitsbereichs
  !!        der Daten liegen. Es wird allerdings empfohlen alternativ einen G&uuml;ltigkeitsbereich
  !!        vorzugeben. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (51) = <EM>_FillValue</EM>: [CF]  <BR>
  !!        Wert zum Kennzeichnen ung&uuml;tiger bzw. undefinierter Daten (kann angegeben werden). <BR>
  !!        Dieses Attribut soll an Stelle von <TT>missing_value</TT> verwendet werden. Wird kein
  !!        G&uuml;ltigkeitsbereich vereinbart, so sind bei positivem <TT>_FillValue</TT> alle
  !!        Daten die gr&ouml;&szlig;er oder gleich dem Attributwert sind ung&uuml;ltig bzw. 
  !!        fehlend. Analoges gilt bei negativem <TT>_FillValue</TT> (im negativen Bereich).
  !!        Wird ein G&uuml;ltigkeitsbereich vereinbart, so mu&szlig; <TT>_FillValue</TT>
  !!        au&szlig;erhalb liegen. <BR>
  !!        Mit <TT>_FillValue</TT> werden auf Datei automatisch diejenigen Daten 
  !!        gekennzeichnet, die niemals wirklich beschrieben wurden. <BR>
  !!        Der <TT>_FillValue</TT> gilt f&uuml;r die nicht transformierten Daten (<TT>add_offset</TT>
  !!        und oder <TT>scale_factor</TT>). <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (77) = <EM>ancillary_variables</EM>: [CF] <BR>
  !!        Identifiziert eine Variable, deren Inhalt in einem engen Zusammenhang 
  !!        zu den Daten der aktuellen steht (beispielsweise Ungenauigkeit gemessener
  !!        Daten). Damit lassen sich Zusammenh&auml;nge zwischen verschiedenen
  !!        Variablen herstellen. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (81) = <EM>cell_measures</EM>: [CF] <BR>
  !!        Identifiziert eine Variable, die Fl&auml;chen oder Volumen von Zellen enth&auml;lt. <BR>
  !!        Der Wert der Variablen setzt sich aus <EM>ma&szlig;zahl: variablenname</EM>
  !!        zusammen. F&uuml;r die Ma&szlig;zahl sind derzeit nur <TT>area</TT> und <TT>volume</TT>
  !!        zugelassen. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (82) = <EM>cell_methods</EM>: [CF] <BR>
  !!        Beschreibt die Methode, die beim Erzeugen zellenbezogener Daten eingesetzt wurde. <BR>
  !!        Der Wert der Variablen setzt sich aus <EM>name: methode</EM> zusammen. F&uuml;r 
  !!        <EM>name</EM> kann entweder die Dimension der Variablen, eine skalare Koordinatenvariable,
  !!        oder ein g&uuml;ltiger Standardname <TT>standard_name</TT> eingesetzt werden. 
  !!        Die Liste der zul&auml;ssigen Methodenbezeichnungen ist auf die folgenden Werte begrenzt: 
  !!        <OL>
  !!        <LI> <TT>point</TT>, die Datenwerte sind repres&auml;sentativ f&uuml;r Punkte in Raum
  !!                             und Zeit (Voreinstellung f&uuml;r <EM>intensive</EM>, also von der
  !!                             Gr&ouml;&szlig;e der Zelle unabh&auml;ngige Gr&ouml;&szlig;en);
  !!        <LI> <TT>sum</TT>, die Datenwerte sind repres&auml;entativ f&uuml;r &uuml;ber die
  !!                           Zelle akkumlierte bzw. summierte Gr&ouml;&szlig;en (Voreinstellung
  !!                           f&uuml;r <EM>extensive</EM>, also von der Gr&ouml;&szlig;e der
  !!                           Zelle abh&auml;ngige Gr&ouml;&szlig;en);      
  !!        <LI> <TT>maximum</TT>, Maximum;
  !!        <LI> <TT>mean</TT>, Mittelwert;
  !!        <LI> <TT>median</TT>, Median;
  !!        <LI> <TT>minimum</TT>, Minimum;
  !!        <LI> <TT>mid_range</TT>, aus Maximum und Minimum abgeleiteter Durchschnittswert;
  !!        <LI> <TT>mode</TT>, Mode;
  !!        <LI> <TT>standard_deviation</TT>, Standardabweichung; sowie
  !!        <LI> <TT>variance</TT>, Varianz.
  !!        </OL>
  !!        <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !!  (87) = <EM>coordinates</EM>: [CF] <BR>
  !!        Identifiziert die Hilfskoordinatenvariablen, Stationsbezeichner oder 
  !!        alternativen Koordinaten einer Variablen. <BR>
  !!        Alle Dimensionen einer Variablen die nicht Breite, L&auml;nge, Vertkale oder
  !!        Zeit sind, m&uuml;ssen mit den vorgenannten Koordinaten in Bezug gesetzt
  !!        gesetzt werden. <BR>
  !!        Der Wert dieses Attributs setzt sich daher aus eine durch Leerzeichen
  !!        getrennten Liste der benutzten Hilfskoordinatenvariablen zusammen. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !!  (88) = <EM>flag_meanings</EM>: [CF] <BR>
  !!        Wird in Zusammenhang mit <TT>flag_values</TT> verwendet, um deren Bedeutung
  !!        n&auml;her zu erl&auml;tern. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !!  (89) = <EM>flag_values</EM>: [CF] <BR>
  !!        Liste durch Leerzeichen getrennter Werte mit besonderer Bedeutung, die 
  !!        in dem erg&auml;nzenden Attribut <TT>flag_meanings</TT> n&auml;her
  !!        erl&auml;tert sind. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (91) = <EM>grid_mapping</EM>: [CF] <BR>
  !!        Vezeichnet eine Variable, in welcher die Informationen &uuml;ber das f&uuml;r die
  !!        aktuelle Variable benutze Koordinatensystem abgelegt sind. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (99) = <EM>standard_error_multiplier</EM>: [CF] <BR>
  !!        Falls eine Variable mit einem <TT>standard_name</TT> &uuml;ber den Modifier
  !!        <TT>standard_error</TT> verf&uuml;gt und dieses Attribut tr&auml;gt, so 
  !!        wird der Fehler mit dem angegebenen Vielfachen des Standardfehlers 
  !!        multipliziert. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (100) = <EM>standard_name</EM>: [CF] <BR>
  !!         Ein Standardname, der auf einen Eintrag und die n&auml;here Beschreibung in
  !!         der g&uuml;ltigen Tabelle der Standardnamen hinweist. <BR>
  !!         Diese Standardbezeichnung darf, durch ein Leerzeichen abgetrennt, noch 
  !!         einen zus&auml;tzlichen <EM>Modifier</EM> (z.B. <TT>standard_error</TT>)
  !!         enthalten. Ist letzteres der Fall, so handelt es sich um eine 
  !!         <EM>standard_name</EM> eng zugeordnete Variable (<TT>ancillary_variables</TT>
  !!         verwenden). Folgende <EM>Modifier</EM> sind zul&auml;ssig:
  !!         <OL>
  !!         <LI> <TT>detection_minimum</TT>: Kleinster detektierbarer Datenwert;
  !!         <LI> <TT>number_of_observations</TT>: Anzahl der Einzelmessungen, aus denen
  !!              der Datenwert ermittelt wurde;
  !!         <LI> <TT>standard_error</TT>: Unsicherheit des Datenwertes, wobei sowohl 
  !!              systematische als auch zuf&auml;llige Fehler erfasst sind; es wird
  !!              &uuml;blicherweise davon ausgegangen, dass die Fehler f&uuml;r einen
  !!              Standard-Fehler und nicht ein Vielfaches davon angegeben werden. Sollte
  !!              dies nicht der Fall sein, so muss die entsprechende Hilfsvariable
  !!              das Attribut <TT>standard_error_multiplier</TT>;
  !!         <LI> <TT>status_flag</TT>: Hinweis auf Datenwerte mit besonderer Bedeutung
  !!              (die entsprechende Variable muss dann die Attribute <TT>flag_values</TT>
  !!              sowie <TT>flag_meanings</TT> enthalten).
  !!         </OL>
  !!         <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (101) = <EM>valid_max</EM>: [CF] <BR>
  !!         Gr&ouml;&szlig;ter g&uuml;tiger Wert einer Variablen. <BR>
  !!         Hinweis: <TT>valid_range</TT> m&ouml;glichst nicht verwenden. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (102) = <EM>valid_min</EM>: [CF] <BR>
  !!         Kleinster g&uuml;tiger Wert einer Variablen. <BR>
  !!         Hinweis: <TT>valid_range</TT> m&ouml;glichst nicht verwenden. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  !  die folgenden Attribute duerfen ausschliesslich in Zusammenhang mit Koordinatenvariablen 
  !  benutzt werden
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  !! </P><P>
  !! (78) = <EM>axis</EM>: [CF] <BR>
  !!        Zeigt an, um welche Koordinatenachse es sich dabei handelt. Typischer Weise sind
  !!        dies L&auml;nge, Breite, Vertikale und Zeit. Es sollen ausschlie&szlig;lich die
  !!        Bezeichnungen <TT>X</TT>, <TT>Y</TT>, <TT>Z</TT> und <TT>T</TT> verwendet werden,
  !!        so dass eine automatische Zuordnung, z.B. in Plottprogrammen, der verschiedenen
  !!        Raumachsen sowie der Zeitachse m&ouml;glich ist. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  !! </P><P>
  !! (79) = <EM>bounds</EM>: [CF] <BR>
  !!        Identifiziert eine sogenannte <EM>Randvariable</EM>. Deren Wert bezeichnet
  !!        eine Variable, welche die Knoten der Zellen enth&auml;lt, zu denen die
  !!        aktuellen Koordinaten geh&ouml;ren. <BR>
  !!        Die Dimension einer Randvariablen ist um genau 1 gr&ouml;&szlig;er als die
  !!        zu ihr geh&ouml;renden Koordinaten oder Hilfskoordinaten. Die zus&auml;tzliche
  !!        Dimension soll dabei die am schnellsten variierende sein. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (80) = <EM>calendar</EM>: [CF] <BR>
  !!        Art des benutzten Kalenders, um Zeitachsen zu dekodieren. Dieses Attribut sollte der
  !!        Variablen Zeit zugeordnet werden. Die folgenden Werte sind zul&auml;ssig:
  !!        <OL>
  !!        <LI> <TT>gregorian</TT> or <TT>standard</TT>, gemischter gregorianisch-julianischer
  !!             Kalender (dies ist die Voreisntellung);
  !!        <LI> <TT>proleptic_gregorian</TT>, der gregorianische Kalender wird vor die
  !!             Kalenderumstellung am 15. Okt. 1582 ausgedehnt;
  !!        <LI> <TT>no_leap</TT> oder <TT>365_day</TT>, ein gregorianischer Kalender ohne
  !!             Schaltjahre, dasss hei&szlig;t, alle Jahre sind 365 Tage lang;
  !!        <LI> <TT>all_leap</TT> oder <TT>366_day</TT>, ein gregorianischer Kalender in
  !!             dem alle Jahre Schaltjahre sind;
  !!        <LI> <TT>360_day</TT>, alle Jahre werden zu 360 Tagen und jeder Monat zu 30 Tagen
  !!             gerechnet;
  !!        <LI> <TT>julian</TT>, Julianischer Kalender;
  !!        <LI> <TT>none</TT>, f&uuml;r ein festes Datum, wobei das Datum durch das 
  !!             <TT>units</TT>-Attribut festgelegt wird.
  !!        </OL>
  !!        <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (83) = <EM>climatology</EM>: [CF] <BR>
  !!        Identifiziert eine klimatologische Zeit-Koordinaten-Variable. Wird ben&ouml;tigt, wenn
  !!        z.B. saisonale Gr&ouml;&szlig;en aus verschiedenen Jahren verarbeitet werden
  !!        sollen. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (85) = <EM>compress</EM>: [CF] <BR>
  !!        Bezeichnet f&uuml;r eine Koordinatenvariable (zumeist Index-Punktliste) diejenigen 
  !!        Dimensionen, die durch Aufsammeln (Entfernen dauerhaft fehlender Daten) komprimiert 
  !!        werden. Der Wert ist eine Liste durch Leerzeichen getrennter Dimensionen, die 
  !!        aufgesammelt werden sollen. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (90) = <EM>formula_terms</EM>: [CF] <BR>
  !!         Identifiziert Variablen die bestimmten Termen in einer Formel zugeordnet werden. <BR>
  !!         Wird insbesondere in Zusammenhang mit dimensionslosen vertikalen Koordinaten genutzt,
  !!         um die aktuelle Lage (z.B. einer Schicht) aus verf&uuml;gbaren Variablen berechenbar
  !!         zu machen. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (93) = <EM>leap_month</EM>: [CF] <BR>
  !!        Bezeichnet denjenigen Monat, der in einem Schaltjahr um einen Tag verl&auml;ngert
  !!        wird, insofern ein benutzerdefinierter Kalender benutzt wird. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (94) = <EM>leap_year</EM>: [CF] <BR>
  !!        Bezeichnet ein Schaltjahr. Es wird davon ausgegangen, dass alle Jahre, die sich
  !!        von diesem Jahr um ein ganzzahliges Vielfaches von 4 unterscheiden, ebenfalls
  !!        Schaltjahre sind, insofern ein benutzerdefinierter Kalender benutzt wird. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (95) = <EM>month_lengths</EM>: [CF] <BR>
  !!        Gibt f&uuml;r alle normalen Jahre die Anzahl der Tage in den jeweiligen Monaten
  !!        vor, insofern ein benutzerdefinierter Kalender benutzt wird. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (96) = <EM>positive</EM>: [CF] <BR>
  !!        Richtung der zunehmenden Vertikalkoordinate [ "up" oder "down" ]. <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (41) = <EM><STRONG>name_id</STRONG></EM>: [BAW]  <BR>
  !!        phys. Code-Kennung, immer gem&auml;&szlig; phydef (muss immer angegeben werden) <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (42) = <EM><STRONG>time_id</STRONG></EM>: [BAW]  <BR>
  !!        Zeitabh&auml;ngigkeit, immer gem&auml;&szlig; "phydef" (sollte immer angegeben werden):
  !!        <UL>
  !!        <LI> 0 = keine Zeitabh&auml;ngigkeit, 
  !!        <LI> 1 = synoptische Daten, Zeitserie oder Daten f&uuml;r ein Ereignis, und
  !!        <LI> 2 = Daten f&uuml;r einen Zeitraum.
  !!        </UL>   
  !!        <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (43) = <EM>class_id</EM>: [BAW]  <BR>
  !!        Klassenzugeh&ouml;rigkeit, immer gem&auml;&szlig; "phydef" (kann angegeben werden)
  !!        <UL>
  !!        <LI> 01 = h_Flut,
  !!        <LI> 02 = h_Ebbe,
  !!        <LI> 03 = h_Tide,
  !!        <LI> 04 = v_Flut,
  !!        <LI> 05 = v_Ebbe,
  !!        <LI> 06 = v_Tide, 
  !!        <LI> 07 = Amplit, 
  !!        <LI> 08 = Phase,  
  !!        <LI> 09 = AmpVst, 
  !!        <LI> 10 = PhasDif,
  !!        <LI> 11 = dAmplit,
  !!        <LI> 12 = dPhase, 
  !!        <LI> 13 = dAmpVst,
  !!        <LI> 14 = dPhaDif,
  !!        <LI> 15 = others, und 
  !!        <LI> 16 = meteorologische Groessen.
  !!        </UL>
  !!        <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (44) = <EM>plot_id</EM>: [BAW] <BR>
  !!        Plot-Id, immer gem&auml;&szlig; "phydef" (kann angegeben werden) 
  !!        <UL>
  !!        <LI> 0 = keine Zeit-Beschriftung anbringen, 
  !!        <LI> 1 = Zeitangabe oder Tideuhr fuer einen synopt. Zeitpunkt,
  !!        <LI> 2 = Zeitraum von ... bis ... , 
  !!        <LI> 3 = Tideanfang ... ,
  !!        <LI> 4 = Tidehochwasser ... ,
  !!        <LI> 5 = Halbtidebeginn ... ,
  !!        <LI> 6 = Kenterungszeit ... , und
  !!        <LI> 7 = Tideniedrigwasser.
  !!        </UL>
  !!        <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (45) = <EM>ref_name_id</EM>: [BAW] <BR>
  !!        phys. Code-Kennung der Referenzgr&ouml;&szlig;e, immer gem&auml;&szlig; 
  !!        "phydef" (sollte immer angegeben werden) <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (48) = <EM>actual_range</EM>: [BAW] <BR>
  !!        aktueller Wertebereich (kann angegeben werden) <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (52) = <EM>FORTRAN_format</EM>: [CF] <BR>
  !!        Fortran-Format zum Ausdrucken des Wertes der Variablen (kann angegeben werden) <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (53) = <EM>horizontal_discretisation</EM>: [BAW] <BR>
  !!        [node,edge,polygon,cell,face,...]          <BR>
  !!        besser CF-Attribut <EM>cell_measures</EM> verwenden
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (54) = <EM>vertical_discretisation</EM>: [BAW]  <BR>
  !!        [top,mid,bot]                              <BR>
  !!        sollte besser direkt aus den Koordinaten ablesbar sein
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (55) = <EM><STRONG>storage_type</STRONG></EM>: [BAW]  <BR>
  !!        [IN,RE,DP,CX,RI2,RR2] (muss immer angeben werden)   <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (56) = <EM>class_names</EM>: [BAW] <BR>
  !!        sollte immer dann angegeben werden, wenn eine physikalische
  !!        Gr&ouml;&szlig;e in mehreren Varianten (z.B. Sedimentklassen)
  !!        vorhanden ist, und diese in einer Variablen angelegt sind <BR>
  !!        f&uuml;r CF-konforme Speicherung m&uuml;ssen die verschiedene
  !!        Klassen auf verschiedene Variablen verteilt werden <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (58) = <EM><STRONG>settling_velocity</STRONG></EM>: [BAW] <BR>
  !!        Sinkgeschwindigkeit <BR>
  !!        ggf. zuk&uuml;nftig besser mit CF-Attribut <EM>ancillary_variables</EM> arbeiten <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (59) = <EM><STRONG>settling_velocity_formula</STRONG></EM>: [BAW] <BR>
  !!        Art der Sinkgeschwindigkeitsberechnung <BR>
  !!        ggf. zuk&uuml;nftig besser mit CF-Attribut <EM>ancillary_variables</EM> arbeiten <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (62) = <EM><STRONG>data_interval</STRONG></EM>: [BAW]  <BR>
  !!        Datenintervall <BR>
  !!        ggf. zuk&uuml;nftig besser mit CF-Attribut <EM>ancillary_variables</EM> arbeiten <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (63) = <EM><STRONG>high_water_mark</STRONG></EM>: [BAW] <BR>
  !!        oberer Grenzwert Wasserstand <BR>
  !!        ggf. zuk&uuml;nftig besser mit CF-Attribut <EM>ancillary_variables</EM> arbeiten <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (64) = <EM><STRONG>low_water_mark</STRONG></EM>: [BAW]  <BR>
  !!        unterer Grenzwert Wasserstand <BR>
  !!        ggf. zuk&uuml;nftig besser mit CF-Attribut <EM>ancillary_variables</EM> arbeiten <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (65) = <EM><STRONG>high_tracer_mark</STRONG></EM>: [BAW] <BR>
  !!        oberer Grenzwert Tracer <BR>
  !!        ggf. zuk&uuml;nftig besser mit CF-Attribut <EM>ancillary_variables</EM> arbeiten <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P><P>
  !! (66) = <EM><STRONG>low_tracer_mark</STRONG></EM>: [BAW] <BR>
  !!        unterer Grenzwert Tracer 
  !!        ggf. zuk&uuml;nftig besser mit CF-Attribut <EM>ancillary_variables</EM> arbeiten <BR>
  !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !! </P>
  !! <BR>
  !
  CHARACTER (LEN=c_len_att_name), PUBLIC, PARAMETER :: c_att_name(c_max_att_name)= & ! 
       !   1234567890 234567890 234567890 234567890    1234567890 234567890 234567890 234567890
       (/ 'title                                   ', 'history                                 ', & ! 001/002
          'beginning_date                          ', 'ending_date                             ', & ! 003/004
          'reference_date                          ', 'single_date                             ', & ! 005/006
          'multiple_dates                          ', 'horizontal_coordinate_system_definition ', & ! 007/008
          'map_projection                          ', 'horizontal_datum_name                   ', & ! 009/010
          'ellipsoid_name                          ', 'longitude_of_central_meridian           ', & ! 010/011
          'false_easting                           ', 'false_northing                          ', & ! 013/014
          'west_bounding_coordinate                ', 'east_bounding_coordinate                ', & ! 015/016
          'north_bounding_coordinate               ', 'south_bounding_coordinate               ', & ! 017/018
          'vertical_coordinate_system_definition   ', 'altitude_datum_name                     ', & ! 019/020
          'depth_datum_name                        ', 'format_name                             ', & ! 021/022
          'format_version_no                       ', 'theme_keywords                          ', & ! 023/024
          'place_keywords                          ', 'temporal_keywords                       ', & ! 025/026
          'related_file_name                       ', 'related_file_form                       ', & ! 027/028
          'related_file_access                     ', 'related_file_type                       ', & ! 029/030
          'reference_location_index                ', 'reference_location_coordinates          ', & ! 031/032
          'zero_phase_location_index               ', 'zero_phase_location_coordinates         ', & ! 033/034
          'sigma_interfaces                        ', 'layer_interfaces                        ', & ! 035/036
          'minimum_water_depth                     ', 'long_name                               ', & ! 037/038
          'short_name                              ', 'units                                   ', & ! 039/040
          'name_id                                 ', 'time_id                                 ', & ! 041/042
          'class_id                                ', 'plot_id                                 ', & ! 043/044
          'ref_name_id                             ', 'scale_factor                            ', & ! 045/046
          'valid_range                             ', 'actual_range                            ', & ! 047/048
          'missing_value                           ', 'add_offset                              ', & ! 049/050
          '_FillValue                              ', 'FORTRAN_format                          ', & ! 051/052
          'horizontal_discretisation               ', 'vertical_discretisation                 ', & ! 053/054
          'storage_type                            ', 'class_names                             ', & ! 055/056
          'dynamic_bathymetry                      ', 'settling_velocity                       ', & ! 057/058
          'settling_velocity_formula               ', 'directional_sector                      ', & ! 059/060
          'cross_sectional_average                 ', 'data_interval                           ', & ! 061/062
          'high_water_mark                         ', 'low_water_mark                          ', & ! 063/064
          'high_tracer_mark                        ', 'low_tracer_mark                         ', & ! 065/066
          'grid_mapping_name                       ', 'grid_north_pole_latitude                ', & ! 067/068
          'grid_north_pole_longitude               ', 'latitude_of_projection_origin           ', & ! 069/070
          'longitude_of_projection_origin          ', 'north_pole_grid_longitude               ', & ! 071/072
          'scale_factor_at_central_meridian        ', 'scale_factor_at_projection_origin       ', & ! 073/074
          'standard_parallel                       ', 'straight_vertical_longitude_from_pole   ', & ! 075/076
          'ancillary_variables                     ', 'axis                                    ', & ! 077/078
          'bounds                                  ', 'calendar                                ', & ! 079/080
          'cell_measures                           ', 'cell_methods                            ', & ! 081/082
          'climatology                             ', 'comment                                 ', & ! 083/084
          'compress                                ', 'Conventions                             ', & ! 085/086
          'coordinates                             ', 'flag_meanings                           ', & ! 087/088
          'flag_values                             ', 'formula_terms                           ', & ! 089/090
          'grid_mapping                            ', 'institution                             ', & ! 091/092
          'leap_month                              ', 'leap_year                               ', & ! 093/094
          'month_lengths                           ', 'positive                                ', & ! 095/096
          'references                              ', 'source                                  ', & ! 097/098
          'standard_error_multiplier               ', 'standard_name                           ', & ! 099/100
          'valid_max                               ', 'valid_min                               ', & ! 101/102
          'astro                                   ', 'grid_type                               ', & ! 103/104
          'coordinate_system                       '                                             /) ! 105
  !
  !! Bezeichnung des Soll-Datentyps der Attribute : <BR>
  !! ch(?) : Typ CHARACTER zwingend erforderlich    <BR>
  !! in(?) : Typ INTEGER   zwingend erforderlich    <BR>
  !! dp(?) : Typ DOUBLE    zwingend erforderlich    <BR>
  !! va(?) : der Typ muss mit dem Typ der zugeordneten Variablen identisch sein 
  CHARACTER (LEN=05), PUBLIC, PARAMETER :: c_att_type_shape(c_max_att_name)= & ! 
       !   12345    12345    12345    12345    12345
       (/ 'ch(:)', 'ch(:)', 'ch(1)', 'ch(1)', 'ch(1)', & ! 001/005
          'ch(1)', 'ch(:)', 'ch(1)', 'ch(1)', 'ch(1)', & ! 006/010 
          'ch(1)', 'dp(1)', 'dp(1)', 'dp(1)', 'dp(1)', & ! 011/015
          'dp(1)', 'dp(1)', 'dp(1)', 'ch(1)', 'ch(1)', & ! 016/020 
          'ch(1)', 'ch(1)', 'ch(1)', 'ch(:)', 'ch(:)', & ! 021/025
          'ch(:)', 'ch(:)', 'ch(:)', 'ch(:)', 'ch(:)', & ! 026/030
          'in(1)', 'dp(3)', 'in(1)', 'dp(3)', 'dp(:)', & ! 031/035
          'dp(:)', 'dp(1)', 'ch(1)', 'ch(1)', 'ch(1)', & ! 036/040
          'in(1)', 'in(1)', 'in(1)', 'in(1)', 'in(1)', & ! 041/045
          'dp(1)', 'va(2)', 'va(2)', 'va(1)', 'dp(1)', & ! 046/050
          'va(1)', 'ch(1)', 'ch(1)', 'ch(1)', 'ch(1)', & ! 051/055
          'ch(:)', 'in(1)', 'ch(:)', 'ch(:)', 'ch(1)', & ! 056/060
          'in(1)', 'ch(:)', 'dp(:)', 'dp(:)', 'dp(:)', & ! 061/065
          'dp(:)', 'ch(1)', 'dp(1)', 'dp(1)', 'dp(1)', & ! 066/070
          'dp(1)', 'dp(1)', 'dp(1)', 'dp(1)', 'dp(:)', & ! 071/075
          'dp(1)', 'ch(1)', 'ch(1)', 'ch(1)', 'ch(1)', & ! 076/080
          'ch(1)', 'ch(1)', 'ch(1)', 'ch(:)', 'ch(1)', & ! 081/085
          'ch(:)', 'ch(1)', 'ch(1)', 'ch(1)', 'ch(1)', & ! 086/090
          'ch(1)', 'ch(1)', 'in(1)', 'in(1)', 'in(:)', & ! 091/095
          'ch(1)', 'ch(1)', 'ch(1)', 'dp(1)', 'ch(1)', & ! 096/100
          'va(1)', 'va(1)', 'ch(1)', 'ch(1)', 'ch(1)' /) ! 101/105
  !
  !! Feld zum Kenzeichnen des zul&auml;ssigen Verwendung des Attributs, wie es verwendet werden darf   <BR>
  !! die nachfolgend aufgez&auml;hlten Werte, auch in Kombination, sind zul&auml;ssig:                 <BR>
  !! <STRONG>C</STRONG> : Verwenden als <EM>koordinatenvariablenbezogenes</EM> Attribut zul&auml;ssig, <BR>
  !! <STRONG>D</STRONG> : Verwenden als <EM>variablenbezogenes</EM> Attribut zul&auml;ssig, und        <BR>
  !! <STRONG>G</STRONG> : Verwenden als <EM>globales</EM> Attribut zul&auml;ssig
  CHARACTER (LEN=c_len_att_cdg), PUBLIC, PARAMETER :: c_att_cdg(c_max_att_name) = & ! 
       (/ 'G  ', 'G  ', 'G  ', 'G  ', 'G  ', 'G  ', 'G  ', 'G  ', 'G  ', 'G  ',   & ! 001/10
          'G  ', 'D  ', 'D  ', 'D  ', 'G  ', 'G  ', 'G  ', 'G  ', 'G  ', 'G  ',   & ! 011/20
          'G  ', 'G  ', 'G  ', 'G  ', 'G  ', 'G  ', 'G  ', 'G  ', 'G  ', 'G  ',   & ! 021/30
          'G  ', 'G  ', 'G  ', 'G  ', 'G  ', 'G  ', 'G  ', 'CD ', 'CD ', 'CD ',   & ! 031/40
          'CD ', 'CD ', 'CD ', 'CD ', 'CD ', 'D  ', 'CD ', 'CD ', 'D  ', 'D  ',   & ! 041/50
          'D  ', 'CD ', 'D  ', 'D  ', 'D  ', 'D  ', 'G  ', 'D  ', 'D  ', 'D  ',   & ! 051/60
          'G  ', 'D  ', 'D  ', 'D  ', 'D  ', 'D  ', 'CD ', 'CD ', 'CD ', 'CD ',   & ! 061/70
          'CD ', 'CD ', 'CD ', 'CD ', 'CD ', 'CD ', 'D  ', 'C  ', 'C  ', 'C  ',   & ! 071/80
          'D  ', 'D  ', 'C  ', 'DG ', 'C  ', 'G  ', 'D  ', 'D  ', 'D  ', 'C  ',   & ! 081/90
          'D  ', 'DG ', 'C  ', 'C  ', 'C  ', 'C  ', 'DG ', 'DG ', 'D  ', 'CD ',   & ! 091/100
          'CD ', 'CD ', 'G  ', 'G  ', 'G  '                                      /) ! 101/105
  !
  !! Feld zum Kennzeichnen des Typs (Standard oder intern(BAW) des Attributs <BR>
  !! die nachfolgend aufgez&auml;hlten Werte sind zul&auml;ssig:             <BR>
  !! <STRONG>CF</STRONG>  : konform mit CF-Metadaten-Standard                <BR>
  !! <STRONG>BAW</STRONG> : baw-spezifische Erweiterung          
  CHARACTER (LEN=c_len_att_typ), PUBLIC, PARAMETER :: c_att_typ(c_max_att_name) = & ! 
       (/ 'CF ', 'CF ', 'BAW', 'BAW', 'BAW', 'BAW', 'BAW', 'BAW', 'BAW', 'BAW',   & ! 001/010
          'BAW', 'CF ', 'CF ', 'CF ', 'BAW', 'BAW', 'BAW', 'BAW', 'BAW', 'BAW',   & ! 011/020
          'BAW', 'BAW', 'BAW', 'BAW', 'BAW', 'BAW', 'BAW', 'BAW', 'BAW', 'BAW',   & ! 021/030
          'BAW', 'BAW', 'BAW', 'BAW', 'BAW', 'BAW', 'BAW', 'CF ', 'BAW', 'CF ',   & ! 031/040
          'BAW', 'BAW', 'BAW', 'BAW', 'BAW', 'CF ', 'CF ', 'BAW', 'CF ', 'CF ',   & ! 041/050
          'CF ', 'CF ', 'BAW', 'BAW', 'BAW', 'BAW', 'BAW', 'BAW', 'BAW', 'BAW',   & ! 051/060
          'BAW', 'BAW', 'BAW', 'BAW', 'BAW', 'BAW', 'CF ', 'CF ', 'CF ', 'CF ',   & ! 061/070
          'CF ', 'CF ', 'CF ', 'CF ', 'CF ', 'CF ', 'CF ', 'CF ', 'CF ', 'CF ',   & ! 071/080
          'CF ', 'CF ', 'CF ', 'CF ', 'CF ', 'CF ', 'CF ', 'CF ', 'CF ', 'CF ',   & ! 081/090
          'CF ', 'CF ', 'CF ', 'CF ', 'CF ', 'CF ', 'CF ', 'CF ', 'CF ', 'CF ',   & ! 091/100
          'CF ', 'CF ', 'WLD', 'WLD', 'WLD'                                      /) ! 101/105
  !
  !! Definition der zul&auml;ssigen Werte f&uuml;r das Attribut <TT>grid_mapping_name</TT>
  CHARACTER (LEN=c_len_grid_mapping_name), PUBLIC, PARAMETER :: c_grid_mapping_name(c_max_grid_mapping_name)= & ! 
       (/ 'albers_conical_equal_area   ', 'lambert_azimuthal_equal_area' , & ! 01/02
          'lambert_conformal_conic     ', 'polar_stereographic         ' , & ! 03/04
          'rotated_latitude_longitude  ', 'stereographic               ' , & ! 05/06
          'transverse_mercator         '                                   /)! 07    
  !
  !! Definition der erforderlichen zus&auml;tzlichen Attribute (zu "c_grid_mapping_name")
  INTEGER  , PUBLIC, PARAMETER ::                                                                  & ! 
       c_grid_mapping_att(c_max_grid_mapping_att,c_max_grid_mapping_alt,c_max_grid_mapping_name) = & ! 
       ( RESHAPE( (/ 75, 12, 70, 13, 14,    0,  0,  0,  0,  0,    & ! "albers_conical_equal_area" 
                     71, 70, 13, 14,  0,    0,  0,  0,  0,  0,    & ! "lambert_azimuthal_equal_area"
                     75, 12, 70, 13, 14,    0,  0,  0,  0,  0,    & ! "lambert_conformal_conic"
                     76, 70, 75, 13, 14,    0,  0, 73,  0,  0,    & ! "polar_stereographic"
                     68, 69, 72,  0,  0,    0,  0,  0,  0,  0,    & ! "rotated_latitude_longitude"
                     71, 70, 74, 13, 14,    0,  0,  0,  0,  0,    & ! "stereographic"
                     73, 12, 70, 13, 14,    0,  0,  0,  0,  0 /), & ! "transverse_mercator"
                     SHAPE=(/c_max_grid_mapping_att,c_max_grid_mapping_alt,c_max_grid_mapping_name/)) )!
  !
  !! Definition der zul&auml;ssigen Werte f&uuml;r das Attribut <TT>axis</TT>
  CHARACTER (LEN=c_len_axis), PUBLIC, PARAMETER :: c_axis(c_max_axis) = (/ 'X', 'Y', 'Z', 'T' /) ! 
  !
  !! Definition der zul&auml;ssigen Werte f&uuml;r das Attribut <TT>calendar</TT>
  CHARACTER (LEN=c_len_calendar), PUBLIC, PARAMETER :: c_calendar(c_max_calendar) = & ! 
       (/ 'gregorian          ', 'standard           ', 'proleptic_gregorian', & ! 
          'no_leap            ', '365_day            ', 'all_leap           ', & !
          '366_day            ', '360_day            ', 'julian             ', & ! 
          'none               ' /) 
  !
  !! Definition der zul&auml;ssigen Werte f&uuml;r das Attribut <TT>cell_measures</TT>
  CHARACTER (LEN=c_len_cell_measures), PUBLIC, PARAMETER :: c_cell_measures(c_max_cell_measures) = & ! 
       (/ 'area  ', 'volume' /) ! 
  !
  !! Definition der zul&auml;ssigen Werte f&uuml;r das Attribut <TT>cell_methods</TT>
  CHARACTER (LEN=c_len_cell_methods), PUBLIC, PARAMETER :: c_cell_methods(c_max_cell_methods) = & ! 
       (/ 'point             ', 'sum               ', 'maximum           ', & ! 
          'median            ', 'mid_range         ', 'minimum           ', & ! 
          'mean              ', 'mode              ', 'standard_deviation', & ! 
          'variance          '                                              /)
  !
  !! Definition der zul&auml;ssigen Werte f&uuml;r das Attribut <TT>positive</TT>
  CHARACTER (LEN=c_len_positive), PUBLIC, PARAMETER :: c_positive(c_max_positive) = & ! 
       (/ 'up  ', 'down' /) ! 
  !
  !! Definition der zul&auml;ssigen Werte f&uuml;r <TT>standard_name modifier</TT>
  CHARACTER (LEN=c_len_standard_name_modifier), PUBLIC, PARAMETER :: & ! 
       c_standard_name_modifier(c_max_standard_name_modifier) = & ! 
       (/ 'detection_minimum     ', 'number_of_observations',   & !  
          'standard_error        ', 'status_flag           ' /)   !  
  !
  CHARACTER (LEN=10), PUBLIC, PARAMETER :: c_hor_coord_system(c_max_hcs)= & ! 
       !   1234567890    1234567890    1234567890
       (/ 'geographic', 'Geographic', 'GEOGRAPHIC', & 
          'planar    ', 'Planar    ', 'PLANAR    ', &
          'local     ', 'Local     ', 'LOCAL     ' /)
  !
  CHARACTER (LEN=08), PUBLIC, PARAMETER :: c_map_proj(c_max_map_proj)= &
       !   12345678    12345678  
       (/ 'Mercator', 'MERCATOR'  /)
  !
  CHARACTER (LEN=07), PUBLIC, PARAMETER :: c_hor_datum_name(c_max_hdn)= & !
       !   1234567    1234567
       (/ 'Potsdam', 'POTSDAM'  /)
  !
  CHARACTER (LEN=06), PUBLIC, PARAMETER :: c_elli_name(c_max_elli_name)= &
       !   123456    123456
       (/ 'Bessel', 'BESSEL'  /)
  !
  CHARACTER (LEN=08), PUBLIC, PARAMETER :: c_ver_coord_system(c_max_vcs)= & ! 
       !   12345678    12345678    12345678
       (/ 'altitude', 'Altitude', 'ALTITUDE', & 
          'depth   ', 'Depth   ', 'DEPTH   '  /)
  !
  CHARACTER (LEN=04), PUBLIC, PARAMETER :: c_hor_disc(c_max_hdisc)= & !
       !   1234    1234    1234
       (/ 'node', 'Node', 'NODE', &
          'edge', 'Edge', 'EDGE', &
          'poly', 'Poly', 'POLY', &
          'cell', 'Cell', 'CELL', &
          'face', 'Face', 'FACE' /)
  !
  CHARACTER (LEN=03), PUBLIC, PARAMETER :: c_ver_disc(c_max_vdisc)= & !
       !   123    123    123
       (/ 'top', 'Top', 'TOP', &
          'mid', 'Mid', 'MID', &
          'bot', 'Bot', 'BOT' /)
  !
  CHARACTER (LEN=03), PUBLIC, PARAMETER :: c_storage_type(c_max_st_type)= &
       !   123
       (/ 'in ', 'In ', 'IN ', &
          're ', 'Re ', 'RE ', &
          'dp ', 'Dp ', 'DP ', &
          'cx ', 'Cx ', 'CX ', &
          'ri2', 'Ri2', 'RI2', &
          'rr2', 'Rr2', 'RR2' /)
  !
  ! [C.3] Variablen [moeglichst nicht verwenden]
  ! [C.4] Schnittstellen
  ! [C.4.1] erforderliche oeffentliche Schnittstellen
  !
  !! ggf. Allokieren der statischen Daten des Moduls; <BR>
  !! Initialisieren der statischen Modul-Daten mit Default-Werten.
  INTERFACE init_att
     MODULE PROCEDURE init_att_d ! 
  END INTERFACE
  !! ggf. De-Allokieren der statischen Daten des Moduls; <BR>
  !! Re-Initialisieren einiger statischer Daten mit Default-Werten.
  INTERFACE clear_att
     MODULE PROCEDURE clear_att_d ! 
  END INTERFACE
  !! Setzen der logischen Kanalnummer <EM>PRN_LUN</EM> f&uuml;r PRINT-Methoden; <BR>
  !! keine Ausgabe: <EM>PRN_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>PRN_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>PRN_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_att_prn_lun
     MODULE PROCEDURE setup_att_prn_lun_d ! 
  END INTERFACE
  !! Setzen der logischen Kanalnummer <EM>TRC_LUN</EM> f&uuml;r TRACE-Methoden; <BR>
  !! keine Ausgabe: <EM>TRC_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>TRC_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>TRC_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_att_trc_lun
     MODULE PROCEDURE setup_att_trc_lun_d ! 
  END INTERFACE
  !! Neues Objekt vom Typ "t_att" erzeugen; <BR>
  !! NULLIFY f&uuml;r dynamische Komponenten-Felder; <BR>
  !! Initialisieren mit Default-Werten: <BR>
  !! a) ein Objekt (Skalar) erzeugen; <BR>
  !! b) viele Objekte (Vektor) erzeugen.
  INTERFACE new_att
     MODULE PROCEDURE new_att_0  ! Version fuer Skalar
     MODULE PROCEDURE new_att_1  ! Version fuer 1D-Array
  END INTERFACE
  !! Objekt vom Typ "t_att" vernichten; <BR>
  !! ggf. De-Allokieren von Memory; <BR>
  !! teilweise Re-Initialisieren mit Default-Werten: <BR>
  !! a) ein Objekt (Skalar) vernichten; <BR>
  !! b) viele Objekte (Vektor) vernichten.
  INTERFACE kill_att
     MODULE PROCEDURE kill_att_0 ! Version fuer Skalar
     MODULE PROCEDURE kill_att_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Objekt vom Typ "t_att" auf G&uuml;ltigkeit pr&uuml;fen; <BR>
  !! werden mehrere Objekte in der Parameterliste &uuml;bergeben,
  !! so wird auch deren Eindeutigkeit hinsichtlich
  !! der Kennung <EM>id</EM> und des Namens <EM>name</EM> 
  !! gepr&uuml;ft; <BR>
  !! ferner wird ermittelt, ob die Namen der Attribute den in
  !! diesem Modul g&uuml;tigen Namen "c_att_name(:)" entsprechen; <BR>
  !! wird zus&auml;tzlich auch noch eine Liste von Variablen (t_var)
  !! &uuml;bergeben, so wird au&szlig;erdem noch ermittelt, ob
  !! die in den Attributen vorhandenen Variablen Id's in der Liste
  !! der Variablen vorhanden sind oder nicht: <BR>
  !! a) ein Objekt (Skalar) pr&uuml;fen; <BR>
  !! b) viele Objekte (Vektor) pr&uuml;fen; <BR>
  !! c) wie a), jedoch wird zus&auml;tztlich ermittelt ob die
  !!    "var_id" des Objekts in einer Liste von Variablen (t_var)
  !!    vorhanden ist oder nicht; <BR>
  !! d) wie b), jedoch wird zus&auml;tztlich f&uuml;r alle Objekte
  !!    ermittelt, ob die "var_id" der Objekte in einer Liste von 
  !!    Variablen vorhanden sind oder nicht.
  INTERFACE ok_att
     MODULE PROCEDURE ok_att_0   ! Version fuer Skalar
     MODULE PROCEDURE ok_att_1   ! Version fuer 1D-Array
     MODULE PROCEDURE ok_att_0_v ! Skalar mit Check der "var_id"
     MODULE PROCEDURE ok_att_1_v ! Vektor mit Check der "var_id"
  END INTERFACE
  !! Alle Komponenten des Typs "t_att" auf <EM>PRN_LUN</EM> ausgeben: <BR>
  !! a) ein Objekt (Skalar) drucken; <BR>
  !! b) viele Objekte (Vektor) drucken. <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_att
     MODULE PROCEDURE print_att_0 ! Version fuer Skalar
     MODULE PROCEDURE print_att_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Alle statischen Daten des Moduls auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_att_static
     MODULE PROCEDURE print_att_static_d ! 
  END INTERFACE
  !! Alle Fehlermeldungen des Moduls auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_att_all_errors
     MODULE PROCEDURE print_att_all_errors_d ! 
  END INTERFACE
  !! Setze Komponente "id" in "t_att" auf Benutzerwert(e): <BR>
  !! a) einem Objekt einen Benutzerwert zuweisen;       <BR>
  !! b) vielen Objekten verschiedene Benutzerwerte zuweisen. 
  INTERFACE set_att_id
     MODULE PROCEDURE set_att_id_0_0 ! 
     MODULE PROCEDURE set_att_id_1_1 ! 
  END INTERFACE
  !! Setze Komponente "name" in "t_att" auf Benutzerwert(e): <BR>
  !! a) einem Objekt einen Benutzerwert zuweisen;         <BR>
  !! b) vielen Objekten verschiedene Benutzerwerte zuweisen; <BR>
  !! c) einem Objekt den Wert c_att_name(i) zuweisen; <BR>
  !! d) vielen Objekten die Werte c_att_name(i(:)) zuweisen.
  INTERFACE set_att_name
     MODULE PROCEDURE set_att_name_0_0   ! 
     MODULE PROCEDURE set_att_name_1_1   ! 
     MODULE PROCEDURE set_att_name_0_0_i ! 
  END INTERFACE
  !! Setze Komponente "var_id" in "t_att" auf Benutzerwert(e): <BR>
  !! a) einem Objekt einen Benutzerwert zuweisen;         <BR>
  !! b) vielen Objekten einen Benutzerwerte zuweisen;     <BR>
  !! c) vielen Objekten verschiedene Benutzerwerte zuweisen. 
  INTERFACE set_att_var_id
     MODULE PROCEDURE set_att_var_id_0_0 ! 
     MODULE PROCEDURE set_att_var_id_1_0 ! 
     MODULE PROCEDURE set_att_var_id_1_1 ! 
  END INTERFACE
  !! Setze Feld-Komponente "ch" in "t_att" auf Benutzerwert(e): <BR>
  !! a) einem Objekt einen (skalaren) Wert zuweisen; <BR>
  !! b) einem Objekt viele Werte (Vektor) zuweisen; <BR>
  !! c) einem Objekt eine (skalare) Datums-/Zeitangabe (t_datetime) zuweisen; <BR>
  !! d) einem Objekt viele Datums-/Zeitangaben (t_datetime) zuweisen. <BR>
  !! werden Datumsangaben (t_datetime) &uuml;bergeben, so werden 
  !! diese automatisch in einen Character-String umgewandelt
  !! (englische Konvention). <BR>
  !! <EM>Hinweis:</EM> von den Komponenten "ch", "in" und 
  !! "dp" darf f&uuml;r ein Attribut nur eine allokiert werden.
  INTERFACE set_att_ch
     MODULE PROCEDURE set_att_ch_0_0 ! 
     MODULE PROCEDURE set_att_ch_0_1 ! 
     MODULE PROCEDURE set_att_ch_t_0 !
     MODULE PROCEDURE set_att_ch_t_1 !
  END INTERFACE
  !! Setze Feld-Komponente "in" in "t_att" auf Benutzerwert(e): <BR>
  !! a) einem Objekt einen (skalaren) Wert zuweisen; <BR>
  !! b) einem Objekt viele Werte (Vektor) zuweisen. <BR>
  !! <EM>Hinweis:</EM> von den Komponenten "ch", "in" und 
  !! "dp" darf f&uuml;r ein Attribut nur eine allokiert werden.
  INTERFACE set_att_in
     MODULE PROCEDURE set_att_in_0_0 ! 
     MODULE PROCEDURE set_att_in_0_1 ! 
  END INTERFACE
  !! Setze Feld-Komponente "dp" in "t_att" auf Benutzerwert(e): <BR>
  !! a) einem Objekt einen (skalaren) Wert zuweisen; <BR>
  !! b) einem Objekt viele Werte (Vektor) zuweisen. <BR>
  !! <EM>Hinweis:</EM> von den Komponenten "ch", "in" und 
  !! "dp" darf f&uuml;r ein Attribut nur eine allokiert werden.
  INTERFACE set_att_dp
     MODULE PROCEDURE set_att_dp_0_0 ! 
     MODULE PROCEDURE set_att_dp_0_1 ! 
  END INTERFACE
  !! F&uuml;ge einen oder mehrere Texteintr&auml;ge zu einem
  !! schon exitierenden Attribut (mit belegter dynamischer
  !! Feld-Komponente "ch(:)") hinzu: <BR>
  !! a) einem Objekt einen (skalaren) Texteintrag hinzuf&uuml;gen; <BR>
  !! b) einem Objekt mehrere Texteintr&auml;ge (Vektor) hinzuf&uuml;gen; <BR> 
  !! c) einem Objekt eine (skalare) Datums-/Zeitangabe (t_datetime) hinzuf&uuml;gen; <BR>
  !! d) einem Objekt viele Datums-/Zeitangaben (t_datetime) hinzuf&uuml;gen. <BR>
  !! werden Datumsangaben (t_datetime) &uuml;bergeben, so werden 
  !! diese automatisch in einen Character-String umgewandelt
  !! (englische Konvention).
  INTERFACE add_att_ch
     MODULE PROCEDURE add_att_ch_0_0
     MODULE PROCEDURE add_att_ch_0_1
     MODULE PROCEDURE add_att_ch_t_0
     MODULE PROCEDURE add_att_ch_t_1
  END INTERFACE
  !! F&uuml;ge eine oder mehrere ganze Zahlen zu einem
  !! schon exitierenden Attribut (mit belegter dynamischer
  !! Feld-Komponente "in(:)") hinzu: <BR>
  !! a) einem Objekt eine ganze Zahl hinzuf&uuml;gen; <BR>
  !! b) einem Objekt mehrere ganze Zahlen (Vektor) hinzuf&uuml;gen.
  INTERFACE add_att_in
     MODULE PROCEDURE add_att_in_0_0
     MODULE PROCEDURE add_att_in_0_1
  END INTERFACE
  !! F&uuml;ge eine oder mehrere reelle (Double) Zahlen zu einem
  !! schon exitierenden Attribut (mit belegter dynamischer
  !! Feld-Komponente "dp(:)") hinzu: <BR>
  !! a) einem Objekt eine ganze Zahl hinzuf&uuml;gen; <BR>
  !! b) einem Objekt mehrere ganze Zahlen (Vektor) hinzuf&uuml;gen. 
  INTERFACE add_att_dp
     MODULE PROCEDURE add_att_dp_0_0
     MODULE PROCEDURE add_att_dp_0_1
  END INTERFACE
  !! h&auml;nge an das Attribut "history" noch einen Eintrag,
  !! bestehend aus Datum und Zeit, Programm und Dateiname hinten
  !! dran: <BR>
  !! a) Eintrag in ein bestimmtes Attribut einf&uuml;gen; <BR>
  !! b) Eintrag in eine Liste von Attributen einf&uuml;gen. 
  INTERFACE add_att_history
     MODULE PROCEDURE add_att_history_0_0
     MODULE PROCEDURE add_att_history_1_0
  END INTERFACE
  !! H&auml;nge eine oder mehrere Attribute an ein Pointer-Feld
  !! des Typs "t_att" an: <BR>
  !! a) Anf&uuml;gen einer Gr&ouml;&szlig;e; <BR>
  !! b) Anf&uuml;gen mehrerer Gr&ouml;&szlig;en.
  INTERFACE add_att
     MODULE PROCEDURE add_att_d_0
     MODULE PROCEDURE add_att_d_1
  END INTERFACE
  !! Hole Komponente "id" aus "t_att": <BR>
  !! a) eine "id" zur&uuml;ckgeben; <BR>
  !! b) mehrere "id's" zur&uuml;ckgeben. <BR>
  !! <EM>Hinweis:</EM> es wird eine Kopie der Daten zur&uuml;ckgegeben.
  INTERFACE get_att_id
     MODULE PROCEDURE get_att_id_0_0 ! Skalar
     MODULE PROCEDURE get_att_id_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "name" aus "t_att": <BR>
  !! a) ein "name" zur&uuml;ckgeben; <BR>
  !! b) mehrere "name's" zur&uuml;ckgeben. <BR>
  !! <EM>Hinweis:</EM> es wird eine Kopie der Daten zur&uuml;ckgegeben.
  INTERFACE get_att_name
     MODULE PROCEDURE get_att_name_0_0 ! Skalar
     MODULE PROCEDURE get_att_name_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "var_id" aus "t_att": <BR>
  !! a) eine "var_id" zur&uuml;ckgeben; <BR>
  !! b) mehrere "var_id's" zur&uuml;ckgeben. <BR>
  !! <EM>Hinweis:</EM> es wird eine Kopie der Daten zur&uuml;ckgegeben.
  INTERFACE get_att_var_id
     MODULE PROCEDURE get_att_var_id_0_0 ! Skalar
     MODULE PROCEDURE get_att_var_id_1_0 ! Vektor
  END INTERFACE
  !! Hole Feld-Komponente "ch" aus "t_att". <BR>
  !! <EM>Hinweis:</EM> es wird eine Kopie der Daten zur&uuml;ckgegeben.
  INTERFACE get_att_ch
     MODULE PROCEDURE get_att_ch_0_1 ! 
  END INTERFACE
  !! Hole Feld-Komponente "ch" als Datums-/Zeitangabe(n) (t_datetime) aus "t_att". <BR>
  !! <EM>Hinweis:</EM> es wird eine Kopie der Daten zur&uuml;ckgegeben.
  INTERFACE get_att_ch_as_datetime
     MODULE PROCEDURE get_att_ch_as_datetime_0_1 ! 
  END INTERFACE
  !! Hole Feld-Komponente "in" aus "t_att". <BR>
  !! <EM>Hinweis:</EM> es wird eine Kopie der Daten zur&uuml;ckgegeben.
  INTERFACE get_att_in
     MODULE PROCEDURE get_att_in_0_1 ! Skalar
  END INTERFACE
  !! Hole Feld-Komponente "dp" aus "t_att". <BR>
  !! <EM>Hinweis:</EM> es wird eine Kopie der Daten zur&uuml;ckgegeben.
  INTERFACE get_att_dp
     MODULE PROCEDURE get_att_dp_0_1 ! Skalar
  END INTERFACE
  !! Ermittle den Typ der zu dem Attribut geh&ouml;renden Daten: <BR>
  !! R&uuml;ckgabewert <TT>CH</TT> f&uuml;r Texte; <BR>
  !! R&uuml;ckgabewert <TT>IN</TT> f&uuml;r ganze Zahlen; <BR>
  !! R&uuml;ckgabewert <TT>DP</TT> f&uuml;r reelle (Double) Zahlen: <BR>
  !! a) es wird eine Typbeschreibung zur&uuml;ckgegeben; <BR>
  !! b) es werden mehrere Typbeschreibungen zur&uuml;ckgegeben.
  INTERFACE get_att_type
     MODULE PROCEDURE get_att_type_0_0 ! Skalar
     MODULE PROCEDURE get_att_type_1_0 ! Vektor
  END INTERFACE
  !! Ermittle die Anzahl der in einem Attribut abgelegten Werte;
  !! hierbei wird die Gr&ouml;&szlig;e der einzigen allokierten 
  !! Feldkomponente (ch(:) oder in(:) oder dp(:)) ermittelt: <BR>
  !! a) es wird ein Wert zur&uuml;ckgegeben; <BR>
  !! a) es werden mehrere Werte zur&uuml;ckgegeben. <BR>
  !! <EM>Hinweis:</EM> falls keine Feld-Komponente allokiert 
  !! sein sollte, dann wird der Wert 0 zur&uuml;ckgegeben.
  INTERFACE get_att_nof_values
     MODULE PROCEDURE get_att_nof_values_0_0 ! Skalar
     MODULE PROCEDURE get_att_nof_values_1_0 ! Vektor
  END INTERFACE
  !! Ermittle die Position "idx" in einem Feld des Typs "t_att"
  !! f&uuml;r eine vorgegebene Id, einen vorgegebenen Namen 
  !! oder ein vorgegebenes Attribut; <BR>
  !! bei "name" kann optional auch noch die "var_id" mit
  !! &uuml;bergeben werden, insofern es sich um ein
  !! variablenbezogenes Attribut handelt:
  !! a) f&uuml;r eine Identifikationsnummer "id";             <BR>
  !! b) f&uuml;r viele Identifikationsnummern "id(:)";        <BR>
  !! c) f&uuml;r einen Namen "name";                          <BR>
  !! d) f&uuml;r viele Namen "name(:)";                       <BR>
  !! e) f&uuml;r ein Attribut vom Typ "t_att";                <BR>
  !! f) f&uuml;r viele Attribute vom Typ "t_att";             <BR>
  !! g) f&uuml;r einen Namen "name" und eine "var_id";        <BR>
  !! h) f&uuml;r viele Namen "name(:)" und viele "var_id(:)". <BR>
  !! <EM>Hinweis:</EM> falls keine Position ermittelt werden 
  !! kann, so wird der Wert 0 zur&uuml;ckgegeben.
  INTERFACE get_att_idx
     MODULE PROCEDURE get_att_idx_i_0
     MODULE PROCEDURE get_att_idx_i_1
     MODULE PROCEDURE get_att_idx_n_0
     MODULE PROCEDURE get_att_idx_n_1
     MODULE PROCEDURE get_att_idx_d_0
     MODULE PROCEDURE get_att_idx_d_1
     MODULE PROCEDURE get_att_idx_n_v_0
     MODULE PROCEDURE get_att_idx_n_v_1
  END INTERFACE
  !! Ermittle in den Komponentenfeldern "ch", "in" und "dp" eines
  !! skalaren Objektes vom Typ "t_att" das Element "idx", das <BR>
  !! als erstes den gesuchten Wert aufweist. <BR>
  !! a) im Komponentenfeld "ch(:)";        <BR>
  !! b) im Komponentenfeld "ch(:)", anstelle von "ch" wird beim Aufruf ein Objekt <BR>
  !!    vom Typ "t_datetime" uebergeben, das intern in einen String gewandelt wird; <BR>
  !! c) im Komponentenfeld "in(:)";        <BR>     
  !! d) im Komponentenfeld "dp(:)";        <BR>
  !! <EM>Hinweis:</EM> falls keine Position ermittelt werden 
  !! kann, so wird der Wert 0 zur&uuml;ckgegeben.
  INTERFACE get_att_field_idx
     MODULE PROCEDURE get_att_field_idx_ch_0
     MODULE PROCEDURE get_att_field_idx_dt_0
     MODULE PROCEDURE get_att_field_idx_in_0
     MODULE PROCEDURE get_att_field_idx_dp_0
  END INTERFACE
  !! Transferiere die Informationen aus den Related-File Attributen
  !! in eine oder mehrere Variablen des Typs "t_file <BR>
  !! a) Inhalt einer bestimmten Feldposition transferieren <BR>
  !! b) Inhalte aller Feldpositionen transferieren
  INTERFACE get_att_related_file
     MODULE PROCEDURE get_att_related_file_0
     MODULE PROCEDURE get_att_related_file_1
  END INTERFACE
  !! Ermittle die Anzahl der f&uuml;r eine bestimmte Variable "var_id"
  !! in einem Feld von Attributen "att(:)" (t_att) vorhandenen 
  !! Attribute: <BR>
  !! a) f&uuml;r eine Variable; <BR>
  !! b) f&uuml;r viele Variablen. 
  INTERFACE get_nof_att_for_var_id
     MODULE PROCEDURE get_nof_att_for_var_id_0
     MODULE PROCEDURE get_nof_att_for_var_id_1
  END INTERFACE
  !! Ermittle die Anzahl der f&uuml;r einen bestimmten Attribut-Namen "name"
  !! in einem Feld von Attributen "att(:)" (t_att) vorhandenen Attribute: <BR>
  !! a) f&uuml;r einen Namen; <BR>
  !! b) f&uuml;r viele Namen. 
  INTERFACE get_nof_att_for_name
     MODULE PROCEDURE get_nof_att_for_name_0
     MODULE PROCEDURE get_nof_att_for_name_1
  END INTERFACE
  !! Ermittle die Anzahl der f&uuml;r einen bestimmten Attribut-Namen "name"
  !! mit Inhalt "contents" vorhandenen Attribute "att(:)" (t_att) <BR>
  !! a) f&uuml;r einen (Attribut-Namen) mit Zeichen-Inhalt
  INTERFACE get_nof_att_for_contents
     MODULE PROCEDURE get_nof_att_for_contents_ch_0
  END INTERFACE
  !! Ermittle die Indices idx(:) unter denen Attribute f&uuml;r eine 
  !! bestimmte Variable "var_id" in einem Feld von Attributen 
  !! "att(:)" (t_att) abgelegt sind.
  INTERFACE get_att_idx_for_var_id
     MODULE PROCEDURE get_att_idx_for_var_id_0
  END INTERFACE
  !! Ermittle die Indices idx(:) unter denen Attribute mit einem
  !! bestimmten Namen "name" in einem Feld von Attributen 
  !! "att(:)" (t_att) abgelegt sind.
  INTERFACE get_att_idx_for_name
     MODULE PROCEDURE get_att_idx_for_name_0
  END INTERFACE
  !! Ermittle die Indices idx(:) unter denen Attribute mit einem
  !! Namen "name" und Inhalt "contents" in einem Feld von Attributen 
  !! "att(:)" (t_att) abgelegt sind.
  INTERFACE get_att_idx_for_contents
     MODULE PROCEDURE get_att_idx_for_contents_ch_0
  END INTERFACE
  !! Ermittle die Ordnung des Auftretens einer Liste von Daten
  !! (Skalar/Vektor von Strings, Skalar/Vektor von Integer-Zahlen, Skalar/Vektor 
  !! von Double-Zahlen, Skalar/Vektor von Datums- und Zeitangaben) in den 
  !! aktuellen Werten eines Atributs mit Name "name" und 
  !! (optionaler) Variablen-Id "id"; <BR>
  !! es wird ein Skalar/Vektor mit Zeiger/Zeigern (Indices) auf die Position 
  !! in dem Datenfeld des aktuellen Attributs zur&uuml;ckgegeben: <BR>
  !! a) f&uuml;r einen Skalar (Character-String) "ch"; <BR>
  !! b) f&uuml;r einen Vektor von Character-Strings "ch(:)"; <BR>
  !! c) f&uuml;r einen Skalar (Integer-Zahl) "in"; <BR>
  !! d) f&uuml;r einen Vektor von Integer-Zahlen "in(:)"; <BR>
  !! e) f&uuml;r einen Skalar (Double-Zahl) "dp"; <BR>
  !! f) f&uuml;r einen Vektor von Double-Zahlen "dp(:)"; <BR>
  !! g) f&uuml;r einen Skalar (Datums- und Zeitangabe) "ti"; <BR>
  !! h) f&uuml;r einen Vektor von Datums- und Zeitangaben "ti(:)".
  INTERFACE get_att_data_order
     MODULE PROCEDURE get_att_data_order_ch_1_0
     MODULE PROCEDURE get_att_data_order_ch_1_1
     MODULE PROCEDURE get_att_data_order_in_1_0
     MODULE PROCEDURE get_att_data_order_in_1_1
     MODULE PROCEDURE get_att_data_order_dp_1_0
     MODULE PROCEDURE get_att_data_order_dp_1_1
     MODULE PROCEDURE get_att_data_order_ti_1_0
     MODULE PROCEDURE get_att_data_order_ti_1_1
  END INTERFACE
  !! Ermittle den Dateibezeichner f&uuml;r die Systemdatei (falls eine solche vorhanden ist)
  INTERFACE get_att_systemfile
     MODULE PROCEDURE get_att_systemfile_1
  END INTERFACE
  !! Ersetzen eines alten Wertes f&uuml;r eine 
  !! Variablen-Id durch einen neuen Wert; <BR>
  !! die Ersetzungen werden in einem Feld von Variablen var(:) (t_var)
  !! und in einem Feld von Attributen att(:) (t_att) in konsistenter Weise
  !! vorgenommen.
  INTERFACE replace_att_var_id
     MODULE PROCEDURE replace_att_var_id_1_1
  END INTERFACE
  !! Ermittle, ob f&uuml;r einen Namen, eine Id oder
  !! ein Attribut ein entsprechender Attribut-Eintrag in 
  !! einem Feld von Attributen "att(:)" (t_att) vorhanden ist; <BR>
  !! bei "name" kann optional auch noch die "var_id" mit
  !! &uuml;bergeben werden, insofern es sich um ein
  !! variablenbezogenes Attribut handelt: <BR>
  !! a) f&uuml;r eine Identifikationsnummer "id";        <BR>
  !! b) f&uuml;r viele Identifikationsnummern "id(:)";   <BR>
  !! c) f&uuml;r einen Namen "name";                     <BR>
  !! d) f&uuml;r viele Namen "name(:)";                  <BR>
  !! e) f&uuml;r ein Attribut vom Typ "t_att";           <BR>
  !! f) f&uuml;r viele Attribute vom Typ "t_att";        <BR>
  !! g) f&uuml;r einen Namen "name" und eine Variablen-Id "var_id"; <BR>
  !! h) f&uuml;r viele Namen "name(:)" und viele Variablen-Id's "var_id(:)".
  INTERFACE att_exists
     MODULE PROCEDURE att_exists_i_0
     MODULE PROCEDURE att_exists_i_1
     MODULE PROCEDURE att_exists_n_0
     MODULE PROCEDURE att_exists_n_1
     MODULE PROCEDURE att_exists_d_0
     MODULE PROCEDURE att_exists_d_1
     MODULE PROCEDURE att_exists_n_v_0
     MODULE PROCEDURE att_exists_n_v_1
  END INTERFACE
  !! Ermittle, ob es sich um ein Attribut mit Text als 
  !! Inhalt handelt: <BR>
  !! a) f&uuml;r ein Attribut (Skalar); <BR>
  !! b) f&uuml;r viele Attribute (Vektor).
  INTERFACE is_att_ch
     MODULE PROCEDURE is_att_ch_0_0
     MODULE PROCEDURE is_att_ch_1_0
  END INTERFACE
  !! Ermittle, ob es sich um ein Attribut mit ganzen Zahlen als Inhalt handelt: <BR>
  !! a) f&uuml;r ein Attribut (Skalar); <BR>
  !! b) f&uuml;r viele Attribute (Vektor).
  INTERFACE is_att_in
     MODULE PROCEDURE is_att_in_0_0
     MODULE PROCEDURE is_att_in_1_0
  END INTERFACE
  !! Ermittle, ob es sich um ein Attribut mit reellen (Double) Zahlen als Inhalt handelt: <BR>
  !! a) f&uuml;r ein Attribut (Skalar); <BR>
  !! b) f&uuml;r viele Attribute (Vektor).
  INTERFACE is_att_dp
     MODULE PROCEDURE is_att_dp_0_0
     MODULE PROCEDURE is_att_dp_1_0
  END INTERFACE
  !! Ermittle, ob eine Zeitangabe innerhalb des Zeitraums der abgelegten
  !! Daten liegt <BR>
  !! a) f&uuml;r eine Zeitangabe <BR>
  !! b) f&uuml;r viele Zeitangaben
  INTERFACE is_datetime_in_att_period
     MODULE PROCEDURE is_datetime_in_att_period_0
     MODULE PROCEDURE is_datetime_in_att_period_1
  END INTERFACE
  !! Ermittle, ob eine Zeitangabe innerhalb exakt in den abgelegten
  !! Daten vorhanden ist <BR>
  !! a) f&uuml;r eine Zeitangabe <BR>
  !! b) f&uuml;r viele Zeitangaben
  INTERFACE is_datetime_in_att
     MODULE PROCEDURE is_datetime_in_att_0
     MODULE PROCEDURE is_datetime_in_att_1
  END INTERFACE
  !! Erzeuge eine Fehlermeldung falls das entsprechende Attribut
  !! in einer Liste von Attributen nicht vorhanden ist; <BR>
  !! die Suche kann auf eine Variable mit einer bestimmten Id
  !! "var_id" eingeschr&auml;nkt werden: <BR>
  !! a) falls ein Attribut "name" nicht vorhanden ist; <BR>
  !! b) wie a) jedoch f&uuml;r mehrere Namen "name(:)"; <BR>
  !! c) wie a) jedoch beschr&auml;nkt auf die Variable "var_id"; <BR>
  !! d) wie c) jedoch f&uuml;r mehrere Namen "name(:)"; <BR>
  !! e) wie d) jedoch f&uuml;r viele Variablen "var_id(:)".         
  INTERFACE gen_error_if_att_not_exists
     MODULE PROCEDURE gen_error_if_att_not_exists_0
     MODULE PROCEDURE gen_error_if_att_not_exists_1
     MODULE PROCEDURE gen_error_if_att_not_exists_0_v
     MODULE PROCEDURE gen_error_if_att_not_exists_1_v
     MODULE PROCEDURE gen_error_if_att_not_exists_1_1
  END INTERFACE
  !! Pr&uuml;fe, ob das gew&uuml;nschte Attribut in einer Liste
  !! von Attributen vorhanden ist; <BR>
  !! falls dies der Fall ist, dann ermittle, ob einer oder
  !! mehrere vorgegebene Werte (Skalar/Vektor von Character-Strings,
  !! Skalar/Vektor von Integer-Zahlen, Skalar/Vektor von Double-Zahlen,
  !! Skalar/Vektor von Datums- und Zeitangaben) den aktuellen Werten des
  !! Attributs entsprechen oder nicht; <BR>
  !! falls eine der vorgenannten Bedingungen nicht erf&uuml;llt
  !! ist, dann wird eine <EM>Fehlermeldung</EM> ausgegeben: <BR>
  !! a) Skalar (Character-String);       <BR>
  !! b) Vektor (Character-Strings);      <BR>
  !! c) Skalar (Integer-Zahl);           <BR>
  !! d) Vektor (Integer-Zahlen);         <BR>
  !! e) Skalar (Double-Zahl);            <BR>
  !! f) Vektor (Double-Zahlen);          <BR>
  !! g) Skalar (Datums- und Zeitangabe); <BR>
  !! h) Vektor (Datums- und Zeitangaben).
  INTERFACE gen_error_if_data_unavailable
     MODULE PROCEDURE gen_error_if_data_un_ch_1_0
     MODULE PROCEDURE gen_error_if_data_un_ch_1_1
     MODULE PROCEDURE gen_error_if_data_un_in_1_0
     MODULE PROCEDURE gen_error_if_data_un_in_1_1
     MODULE PROCEDURE gen_error_if_data_un_dp_1_0
     MODULE PROCEDURE gen_error_if_data_un_dp_1_1
     MODULE PROCEDURE gen_error_if_data_un_ti_1_0
     MODULE PROCEDURE gen_error_if_data_un_ti_1_1
  END INTERFACE
  !! Pr&uuml;fe, ob das gew&uuml;nschte Attribut in einer Liste
  !! von Attributen vorhanden ist; <BR>
  !! falls dies der Fall ist, dann ermittle, ob einer oder
  !! mehrere vorgegebene Werte (Skalar/Vektor von Character-Strings,
  !! Skalar/Vektor von Integer-Zahlen, Skalar/Vektor von Double-Zahlen,
  !! Skalar/Vektor von Datums- und Zeitangaben) den aktuellen Werten des
  !! Attributs entsprechen oder nicht; <BR>
  !! falls eine der vorgenannten Bedingungen nicht erf&uuml;llt
  !! ist, dann wird eine <EM>Warnung</EM> ausgegeben: <BR>
  !! a) Skalar (Character-String);       <BR>
  !! b) Vektor (Character-Strings);      <BR>
  !! c) Skalar (Integer-Zahl);           <BR>
  !! d) Vektor (Integer-Zahlen);         <BR>
  !! e) Skalar (Double-Zahl);            <BR>
  !! f) Vektor (Double-Zahlen);          <BR>
  !! g) Skalar (Datums- und Zeitangabe); <BR>
  !! h) Vektor (Datums- und Zeitangaben).
  INTERFACE gen_warn_if_data_unavailable
     MODULE PROCEDURE gen_warn_if_data_un_ch_1_0
     MODULE PROCEDURE gen_warn_if_data_un_ch_1_1
     MODULE PROCEDURE gen_warn_if_data_un_in_1_0
     MODULE PROCEDURE gen_warn_if_data_un_in_1_1
     MODULE PROCEDURE gen_warn_if_data_un_dp_1_0
     MODULE PROCEDURE gen_warn_if_data_un_dp_1_1
     MODULE PROCEDURE gen_warn_if_data_un_ti_1_0
     MODULE PROCEDURE gen_warn_if_data_un_ti_1_1
  END INTERFACE
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  !! Pr&uuml;fe, ob der Inhalt eines oder mehrerer Attribute
  !! in Ordnung ist oder nicht: <BR>
  !! a) Skalar;                          <BR>
  !! b) Vektor (inkl. Konsistenz-Check); <BR>
  !! c) Skalar mit passender "var_id";   <BR>
  !! d) Vektor mit passender var_id.
  INTERFACE ok_att_contents
     MODULE PROCEDURE ok_att_contents_0
     MODULE PROCEDURE ok_att_contents_1
     MODULE PROCEDURE ok_att_contents_0_v
     MODULE PROCEDURE ok_att_contents_1_v
  END INTERFACE
  !! Pr&uuml;fe den Inhalt mehrerer Attribute auf Konsistenz,
  !! z.B. ob bei zusammen geh&ouml;renden Attributen alle
  !! vorhanden sind oder wenigstens eines fehlt, bzw. verschiedene
  !! Attribute vorhanden sind die nicht gleichzeitig auftreten
  !! d&uuml;rfen.
  INTERFACE ok_att_consistency
     MODULE PROCEDURE ok_att_consistency_d !
  END INTERFACE
  ! [C.5] Zuweisungen
  ! [C.6] Operatoren
  ! [C.6.1] unbedingt erforderliche oeffentliche Operatoren
  !
  !! Zuweisung zwischen zwei Attributen: <BR>
  !! a) Skalar: att1    = att2; <BR>
  !! b) Vektor: att1(:) = att2(:). <BR>
  INTERFACE ASSIGNMENT(=)
     MODULE PROCEDURE as_att_0_0
     MODULE PROCEDURE as_att_1_1
  END INTERFACE
  !! Pr&uuml;fe, ob zwei Objekte vom Typ "t_att" gleich sind; <BR>
  !! zwei Attribute sind dann gleich, wenn sie in allen Komponenten
  !! &uuml;bereinstimmen: <BR>
  !! a) att1    == att2    ; <BR>
  !! b) att1    == att2(:) ; <BR>
  !! c) att1(:) == att2    ; <BR>
  !! d) att1(:) == att2(:) .
  INTERFACE OPERATOR(==)
     MODULE PROCEDURE eq_att_0_0  ! Skalar / Skalar
     MODULE PROCEDURE eq_att_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE eq_att_1_0  ! Vektor / Skalar
     MODULE PROCEDURE eq_att_1_1  ! Vektor / Vektor
!>WIN-NT:     MODULE PROCEDURE eq_datetime_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE eq_datetime_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE eq_datetime_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE eq_datetime_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  ! [C.6.2] optional vorhandene oeffentliche Operatoren
  !! Pr&uuml;fe, ob zwei Objekte vom Typ "t_att" ungleich sind; <BR>
  !! zwei Attribute sind dann ungleich, wenn sie sich in wenigstens 
  !! einer Komponente voneinander unterscheiden: <BR>
  !! a) att1    /= att2    ; <BR>
  !! b) att1    /= att2(:) ; <BR>
  !! c) att1(:) /= att2    ; <BR>
  !! d) att1(:) /= att2(:) .
  INTERFACE OPERATOR(/=)
     MODULE PROCEDURE ne_att_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ne_att_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE ne_att_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ne_att_1_1  ! Vektor / Vektor
!>WIN-NT:     MODULE PROCEDURE ne_datetime_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE ne_datetime_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE ne_datetime_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE ne_datetime_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  ! Fuer Windows Digital Fortran-Compiler leider notwendig!
!>WIN-NT:  INTERFACE OPERATOR(<=)
!>WIN-NT:     MODULE PROCEDURE le_datetime_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE le_datetime_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE le_datetime_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE le_datetime_1_1  ! Vektor / Vektor
!>WIN-NT:  END INTERFACE
!>WIN-NT:  INTERFACE OPERATOR(>=)
!>WIN-NT:     MODULE PROCEDURE ge_datetime_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE ge_datetime_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE ge_datetime_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE ge_datetime_1_1  ! Vektor / Vektor
!>WIN-NT:  END INTERFACE
  !
  !! sind in den Attributen Informationen zu Sigma-Schichten vorhanden
  INTERFACE has_att_sigma_interfaces
     MODULE PROCEDURE has_att_sigma_interfaces_1
  END INTERFACE
  !! sind in den Attributen Informationen zu Sigma-Schichten vorhanden
  INTERFACE has_att_layer_interfaces
     MODULE PROCEDURE has_att_layer_interfaces_1
  END INTERFACE
  !! ist eine zeitvariable Bathymetrie vorhanden
  INTERFACE has_att_dynamic_bathymetry
     MODULE PROCEDURE has_att_dynamic_bathymetry_1
  END INTERFACE
  !! sind Angaben zur minimalen Wasserbedeckung vorhanden
  INTERFACE has_att_minimum_water_depth
     MODULE PROCEDURE has_att_minimum_water_depth_1
  END INTERFACE
  !! tritt eine bestimmte Code-Kennung "name_id" in einer Liste von Attributen auf
  INTERFACE has_att_name_id
     MODULE PROCEDURE has_att_name_id_1_0
  END INTERFACE
  !! ist das Attribut mit querschnittsintegrierten Gr&ouml;&szlig;en vorhanden
  INTERFACE has_att_cross_sectional_average
     MODULE PROCEDURE has_att_cross_section_average_1
  END INTERFACE
  !! gibt es zu einer bestimmten Variablen einen Standardnamen
  INTERFACE has_att_standard_name
     MODULE PROCEDURE has_att_standard_name_1_0
  END INTERFACE
  !! ermittle, ob eine bestimmte Datei als <EM>related file</EM> vorhanden ist
  INTERFACE has_att_related_file
     MODULE PROCEDURE has_att_related_file_1_0
  END INTERFACE
  !
  !! ermittle die Anzahl der Sigma- oder Z-Schichtgrenzen
  INTERFACE get_att_interfaces_count
     MODULE PROCEDURE get_att_interfaces_count_1
  END INTERFACE
  !! ermittle den Wert einer Interface anhand der laufenden Nummer
  INTERFACE get_att_interface
     MODULE PROCEDURE get_att_interface_1_0
  END INTERFACE
  !! ermittle den Wert zur minimalen Wasserbedeckung, falls vorhanden
  INTERFACE get_att_minimum_water_depth
     MODULE PROCEDURE get_att_minimum_water_depth_1
  END INTERFACE
  !! ermittle den Code "name_id" einer bestimmten Variablen
  INTERFACE get_att_var_name_id
     MODULE PROCEDURE get_att_var_name_id_1_0
  END INTERFACE
  !! ermittle die Anzahl der Worte in einer Zeichenkette <BR>
  !! a) f&uuml;r einen Character-String
  INTERFACE get_att_word_in_ch_count
     MODULE PROCEDURE get_word_in_ch_count
  END INTERFACE
  !! ermittle das n-te laufende Wort in einer Zeichenkette <BR>
  !! a) f&uuml;r einen Character-String
  INTERFACE get_att_word_in_ch
     MODULE PROCEDURE get_word_in_ch
  END INTERFACE
  !! ermittle den Standard-Namen <BR>
  !! a) f&uuml;r eine Variable
  INTERFACE get_att_standard_name
     MODULE PROCEDURE get_att_standard_name_1_0
  END INTERFACE
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  ! [C.7.1] unbedingt erforderliche oeffentliche Methoden
  !
  PUBLIC :: init_att                    ! Initialisieren (Modul)
  PUBLIC :: clear_att                   ! De-Initialisieren (Modul)
  PUBLIC :: setup_att_prn_lun           ! Setzen prn_lun 
  PUBLIC :: setup_att_trc_lun           ! Setzen trc_lun 
  PUBLIC :: new_att                     ! Erzeugen 
  PUBLIC :: kill_att                    ! Vernichten
  PUBLIC :: ok_att                      ! Pruefen
  PUBLIC :: print_att                   ! Drucken
  PUBLIC :: print_att_static            ! Drucken aller statischen Daten
  PUBLIC :: print_att_all_errors        ! Drucken aller (moeglichen) Fehlermeldungen
  PUBLIC :: set_att_id                  ! Setzen der Komponente id
  PUBLIC :: set_att_name                ! Setzen der Komponente name
  PUBLIC :: set_att_var_id              ! Setzen der Komponente var_id
  PUBLIC :: set_att_ch                  ! Setzen der Komponente ch
  PUBLIC :: set_att_in                  ! Setzen der Komponente in
  PUBLIC :: set_att_dp                  ! Setzen der Komponente dp
  PUBLIC :: get_att_id                  ! Holen der Komponente id
  PUBLIC :: get_att_name                ! Holen der Komponente name
  PUBLIC :: get_att_var_id              ! Holen der Komponente var_id
  PUBLIC :: get_att_ch                  ! Holen der Komponente ch
  PUBLIC :: get_att_in                  ! Holen der Komponente in
  PUBLIC :: get_att_dp                  ! Holen der Komponente dp
  PUBLIC :: OPERATOR(==)                ! Operator "==" 
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: add_att_ch                      ! Texteintrag hinzufuegen
  PUBLIC :: add_att_in                      ! ganze Zahlen hinzufuegen
  PUBLIC :: add_att_dp                      ! reelle (Double) Zahlen hinzufuegen
  PUBLIC :: add_att_history                 ! Eintrag in History hinzufuegen
  PUBLIC :: add_att                         ! Attribute an Liste anhaengen
  PUBLIC :: ASSIGNMENT(=)                   ! Zuweisung zwischen zwei Attributen
  PUBLIC :: OPERATOR(/=)                    ! Operator "/="
  PUBLIC :: get_att_type                    ! Typ "CH ", "IN ", "RE "
  PUBLIC :: get_att_nof_values              ! Anzahl der abgelegten Werte
  PUBLIC :: get_att_idx                     ! Position eines Attributs
  PUBLIC :: get_att_field_idx               ! Element (eines Komponentenfeldes), das den 
                                            ! gesuchten Wert als erstes aufweist
  PUBLIC :: get_att_related_file            ! Informationen nach Variable des Typs "t_file" uebertragen
  PUBLIC :: replace_att_var_id              ! Konsistentes Ersetzen von Variablen-Id's
  PUBLIC :: att_exists                      ! Attributs-Eintrag vorhanden
  PUBLIC :: is_att_ch                       ! Inhalt ist "CH "
  PUBLIC :: is_att_in                       ! Inhalt ist "IN "
  PUBLIC :: is_att_dp                       ! Inhalt ist "DP "
  PUBLIC :: is_datetime_in_att              ! ist eine/mehrere Zeitangaben in den Daten exakt enthalten
  PUBLIC :: is_datetime_in_att_period       ! ist eine/mehrere Zeitangaben in den Daten enthalten
  PUBLIC :: get_nof_att_for_var_id          ! Anzahl der Attribute f&uuml;r "var_id" ermitteln
  PUBLIC :: get_nof_att_for_name            ! Anzahl der Attribute f&uuml;r "name" ermitteln 
  PUBLIC :: get_nof_att_for_contents        ! Anzahl der Attribute mit einem bestimmten Inhalt  ermitteln 
  PUBLIC :: get_att_idx_for_var_id          ! Indices der Attribute f&uuml;r "var_id" ermitteln
  PUBLIC :: get_att_idx_for_name            ! Indices der Attribute f&uuml;r "name" ermitteln
  PUBLIC :: get_att_idx_for_contents        ! Indices der Attribute mit einem bestimmten Inhalt ermitteln
  PUBLIC :: get_att_ch_as_datetime          ! Holen der Komponente ch als Datums-/Zeitangabe (t_datetime)
  PUBLIC :: get_att_data_order              ! Ermittle die Ordnung des Auftretens fuer eine Liste von Daten
  PUBLIC :: get_att_systemfile              ! Ermittle die Systemdatei (falls vorhanden)
  PUBLIC :: gen_error_if_att_not_exists     ! erzeuge eine Fehlermeldung falls ein Attribut nicht vorhanden ist 
  PUBLIC :: gen_warn_if_data_unavailable    ! erzeuge eine Warnung falls Daten nicht in Attribut vorhanden sind
  PUBLIC :: gen_error_if_data_unavailable   ! erzeuge eine Fehlermeldung falls Daten nicht in Attribut vorhanden sind
  PUBLIC :: ok_att_contents                 ! pruefe den Inhalt eines Attributes
  PUBLIC :: ok_att_consistency              ! pruefe den Inhalt mehrerer Attribute auf Konsistenz 
  PUBLIC :: has_att_sigma_interfaces        ! liegen Informationen ueber sigma-Schichten vor
  PUBLIC :: has_att_layer_interfaces        ! liegen Informationen ueber z-Schichten vor
  PUBLIC :: has_att_dynamic_bathymetry      ! liegt ein Hinweis auf dynamische Bathymetrie vor
  PUBLIC :: has_att_minimum_water_depth     ! liegen Informationen zur benutzten minimalen Wassertiefe vor
  PUBLIC :: has_att_name_id                 ! liegen Informationen zu einer bestimmten phys. Groesse vor 
  PUBLIC :: has_att_cross_sectional_average ! liegt ein Hinweis auf querschnittsintegrierte Groessen vor
  PUBLIC :: has_att_standard_name           ! weist eine Variable das Attribut Standardname auf
  PUBLIC :: has_att_related_file            ! ermittle ob eine bestimmte Datei als <EM>related file</EM> vorhanden ist
  PUBLIC :: get_att_interfaces_count        ! gib die Anzahl der Schichtgrenzen zurueck
  PUBLIC :: get_att_interface               ! gib die Schichtgrenzen zurueck
  PUBLIC :: get_att_minimum_water_depth     ! gib die minimale Wassertiefe zurueck
  PUBLIC :: get_att_var_name_id             ! gib die passende Code-Kennung (phydef) zu einer Variablen zurueck
  PUBLIC :: get_att_word_in_ch_count        ! Anzahl der Worte in einer Zeichenkette
  PUBLIC :: get_att_word_in_ch              ! n-tes laufendes Wort in einer Zeichenkette
  PUBLIC :: get_att_standard_name           ! ermittle den Standardnamen 
  !
  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] lokale Typdefinitionen
  ! [D.2] Konstantwerte (Parameter)
  !! Name des Moduls
  CHARACTER (LEN=05), PARAMETER :: c_modname      = 'b_att' ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false. ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1      ! 
  !! Anzahl der Datenkomponenten des Typs t_att
  INTEGER           , PARAMETER :: c_nofcomp      = 6       ! ggf. modifizieren
  !
  ! [D.3] Variablen (statische Daten des Moduls)
  !! Feld zur Aufnahme aller Fehlermeldungen des Moduls
  TYPE (t_error) , ALLOCATABLE, SAVE :: all_errors(:)       !  
  !! Indikator f&uuml;r eine erfolgreich durchgef&uuml;hrte Modulinitialisierung
  LOGICAL                , SAVE :: initialised = .false.    !  
  !! Indikator f&uuml;r Durchf&uuml;hrung PRINT-Methoden
  LOGICAL                , SAVE :: prn_op      = c_op       !  
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE-Methoden
  LOGICAL                , SAVE :: trc_op      = c_op       !  
  !! logische Kanalnummer f&uuml;r PRINT-Methoden
  INTEGER                , SAVE :: prn_lun     = c_lun      !  
  !! logische Kanalnummer f&uuml;r TRACE-Methoden
  INTEGER                , SAVE :: trc_lun     = c_lun      !  
  !! Z&auml;hler f&uuml;r Initialisierungsaufrufe
  INTEGER                , SAVE :: n_init      = 0          !  
  !
  ! [D.4] Schnittstellen
  !
  INTERFACE ok_title
     MODULE PROCEDURE ok_title_0  ! Skalar
  END INTERFACE
  INTERFACE ok_history
     MODULE PROCEDURE ok_history_0  ! Skalar
  END INTERFACE
  INTERFACE ok_beginning_date
     MODULE PROCEDURE ok_beginning_date_0  ! Skalar
  END INTERFACE
  INTERFACE ok_ending_date
     MODULE PROCEDURE ok_ending_date_0  ! Skalar
  END INTERFACE
  INTERFACE ok_reference_date
     MODULE PROCEDURE ok_reference_date_0  ! Skalar
  END INTERFACE
  INTERFACE ok_single_date
     MODULE PROCEDURE ok_single_date_0  ! Skalar
  END INTERFACE
  INTERFACE ok_multiple_dates
     MODULE PROCEDURE ok_multiple_dates_0  ! Skalar
  END INTERFACE
  INTERFACE ok_hor_coord_system_def
     MODULE PROCEDURE ok_hor_coord_system_def_0  ! Skalar
  END INTERFACE
  INTERFACE ok_map_projection
     MODULE PROCEDURE ok_map_projection_0  ! Skalar
  END INTERFACE
  INTERFACE ok_horizontal_datum_name
     MODULE PROCEDURE ok_horizontal_datum_name_0  ! Skalar
  END INTERFACE
  INTERFACE ok_ellipsoid_name
     MODULE PROCEDURE ok_ellipsoid_name_0  ! Skalar
  END INTERFACE
  INTERFACE ok_longitude_of_cen_meri
     MODULE PROCEDURE ok_longitude_of_cen_meri_0  ! Skalar
  END INTERFACE
  INTERFACE ok_false_easting
     MODULE PROCEDURE ok_false_easting_0  ! Skalar
  END INTERFACE
  INTERFACE ok_false_northing
     MODULE PROCEDURE ok_false_northing_0  ! Skalar
  END INTERFACE
  INTERFACE ok_west_bounding_coord
     MODULE PROCEDURE ok_west_bounding_coord_0  ! Skalar
  END INTERFACE
  INTERFACE ok_east_bounding_coord
     MODULE PROCEDURE ok_east_bounding_coord_0  ! Skalar
  END INTERFACE
  INTERFACE ok_north_bounding_coord
     MODULE PROCEDURE ok_north_bounding_coord_0  ! Skalar
  END INTERFACE
  INTERFACE ok_south_bounding_coord
     MODULE PROCEDURE ok_south_bounding_coord_0  ! Skalar
  END INTERFACE
  INTERFACE ok_vert_coord_system_def
     MODULE PROCEDURE ok_vert_coord_system_def_0  ! Skalar
  END INTERFACE
  INTERFACE ok_altitude_datum_name
     MODULE PROCEDURE ok_altitude_datum_name_0  ! Skalar
  END INTERFACE
  INTERFACE ok_depth_datum_name
     MODULE PROCEDURE ok_depth_datum_name_0  ! Skalar
  END INTERFACE
  INTERFACE ok_format_name
     MODULE PROCEDURE ok_format_name_0  ! Skalar
  END INTERFACE
  INTERFACE ok_format_version_no
     MODULE PROCEDURE ok_format_version_no_0  ! Skalar
  END INTERFACE
  INTERFACE ok_theme_keywords
     MODULE PROCEDURE ok_theme_keywords_0  ! Skalar
  END INTERFACE
  INTERFACE ok_place_keywords
     MODULE PROCEDURE ok_place_keywords_0  ! Skalar
  END INTERFACE
  INTERFACE ok_temporal_keywords
     MODULE PROCEDURE ok_temporal_keywords_0  ! Skalar
  END INTERFACE
  INTERFACE ok_related_file_name
     MODULE PROCEDURE ok_related_file_name_0  ! Skalar
  END INTERFACE
  INTERFACE ok_related_file_form
     MODULE PROCEDURE ok_related_file_form_0  ! Skalar
  END INTERFACE
  INTERFACE ok_related_file_access
     MODULE PROCEDURE ok_related_file_access_0  ! Skalar
  END INTERFACE
  INTERFACE ok_related_file_type
     MODULE PROCEDURE ok_related_file_type_0  ! Skalar
  END INTERFACE
  INTERFACE ok_reference_location_index
     MODULE PROCEDURE ok_reference_location_index_0  ! Skalar
  END INTERFACE
  INTERFACE ok_reference_location_coord
     MODULE PROCEDURE ok_reference_location_coord_0  ! Skalar
  END INTERFACE
  INTERFACE ok_zero_phase_location_index
     MODULE PROCEDURE ok_zero_phase_location_index_0  ! Skalar
  END INTERFACE
  INTERFACE ok_zero_phase_location_coord
     MODULE PROCEDURE ok_zero_phase_location_coord_0  ! Skalar
  END INTERFACE
  INTERFACE ok_sigma_interfaces
     MODULE PROCEDURE ok_sigma_interfaces_0  ! Skalar
  END INTERFACE
  INTERFACE ok_layer_interfaces
     MODULE PROCEDURE ok_layer_interfaces_0  ! Skalar
  END INTERFACE
  INTERFACE ok_minimum_water_depth
     MODULE PROCEDURE ok_minimum_water_depth_0  ! Skalar
  END INTERFACE
  INTERFACE ok_dynamic_bathymetry
     MODULE PROCEDURE ok_dynamic_bathymetry_0  ! Skalar
  END INTERFACE
  INTERFACE ok_long_name
     MODULE PROCEDURE ok_long_name_0  ! Skalar
  END INTERFACE
  INTERFACE ok_short_name
     MODULE PROCEDURE ok_short_name_0  ! Skalar
  END INTERFACE
  INTERFACE ok_units
     MODULE PROCEDURE ok_units_0  ! Skalar
  END INTERFACE
  INTERFACE ok_name_id
     MODULE PROCEDURE ok_name_id_0  ! Skalar
  END INTERFACE
  INTERFACE ok_time_id
     MODULE PROCEDURE ok_time_id_0  ! Skalar
  END INTERFACE
  INTERFACE ok_class_id
     MODULE PROCEDURE ok_class_id_0  ! Skalar
  END INTERFACE
  INTERFACE ok_plot_id
     MODULE PROCEDURE ok_plot_id_0  ! Skalar
  END INTERFACE
  INTERFACE ok_ref_name_id
     MODULE PROCEDURE ok_ref_name_id_0  ! Skalar
  END INTERFACE
  INTERFACE ok_scale_factor
     MODULE PROCEDURE ok_scale_factor_0  ! Skalar
  END INTERFACE
  INTERFACE ok_valid_range
     MODULE PROCEDURE ok_valid_range_0  ! Skalar
  END INTERFACE
  INTERFACE ok_actual_range
     MODULE PROCEDURE ok_actual_range_0  ! Skalar
  END INTERFACE
  INTERFACE ok_missing_value
     MODULE PROCEDURE ok_missing_value_0  ! Skalar
  END INTERFACE
  INTERFACE ok_add_offset
     MODULE PROCEDURE ok_add_offset_0  ! Skalar
  END INTERFACE
  INTERFACE ok__FillValue
     MODULE PROCEDURE ok__FillValue_0  ! Skalar
  END INTERFACE
  INTERFACE ok_FORTRAN_format
     MODULE PROCEDURE ok_FORTRAN_format_0  ! Skalar
  END INTERFACE
  INTERFACE ok_horizontal_discret
     MODULE PROCEDURE ok_horizontal_discret_0  ! Skalar
  END INTERFACE
  INTERFACE ok_vertical_discret
     MODULE PROCEDURE ok_vertical_discret_0  ! Skalar
  END INTERFACE
  INTERFACE ok_storage_type
     MODULE PROCEDURE ok_storage_type_0  ! Skalar
  END INTERFACE
  INTERFACE ok_class_names
     MODULE PROCEDURE ok_class_names_0  ! Skalar
  END INTERFACE
  INTERFACE ok_settling_velocity
     MODULE PROCEDURE ok_settling_velocity_0   ! Skalar
  END INTERFACE
  INTERFACE ok_settling_velo_form
     MODULE PROCEDURE ok_settling_velo_form_0  ! Skalar
  END INTERFACE
  INTERFACE ok_directional_sector
     MODULE PROCEDURE ok_directional_sector_d
  END INTERFACE
  INTERFACE ok_cross_sectional_average
     MODULE PROCEDURE ok_cross_sectional_average_d
  END INTERFACE
  INTERFACE ok_data_interval
     MODULE PROCEDURE ok_data_interval_d
  END INTERFACE
  INTERFACE ok_high_water_mark
     MODULE PROCEDURE ok_high_water_mark_d
  END INTERFACE
  INTERFACE ok_low_water_mark
     MODULE PROCEDURE ok_low_water_mark_d
  END INTERFACE
  INTERFACE ok_high_tracer_mark
     MODULE PROCEDURE ok_high_tracer_mark_d
  END INTERFACE
  INTERFACE ok_low_tracer_mark
     MODULE PROCEDURE ok_low_tracer_mark_d
  END INTERFACE
  INTERFACE ok_grid_mapping_name
     MODULE PROCEDURE ok_grid_mapping_name_d
  END INTERFACE
  INTERFACE ok_grid_north_pole_latitude
     MODULE PROCEDURE ok_grid_north_pole_latitude_d
  END INTERFACE
  INTERFACE ok_grid_north_pole_longitude
     MODULE PROCEDURE ok_grid_north_pole_longitude_d
  END INTERFACE
  INTERFACE ok_latitude_of_proj_origin
     MODULE PROCEDURE ok_latitude_of_proj_origin_d
  END INTERFACE
  INTERFACE ok_longitude_of_proj_origin
     MODULE PROCEDURE ok_longitude_of_proj_origin_d
  END INTERFACE
  INTERFACE ok_north_pole_grid_longitude
     MODULE PROCEDURE ok_north_pole_grid_longitude_d
  END INTERFACE
  INTERFACE ok_scale_factor_at_c_meridian
     MODULE PROCEDURE ok_scale_factor_at_c_meridian_d
  END INTERFACE
  INTERFACE ok_scale_factor_at_p_origin
     MODULE PROCEDURE ok_scale_factor_at_p_origin_d
  END INTERFACE
  INTERFACE ok_standard_parallel
     MODULE PROCEDURE ok_standard_parallel_d
  END INTERFACE
  INTERFACE ok_straight_v_l_from_pole
     MODULE PROCEDURE ok_straight_v_l_from_pole_d
  END INTERFACE
  INTERFACE ok_ancillary_variables
     MODULE PROCEDURE ok_ancillary_variables_d
  END INTERFACE
  INTERFACE ok_axis
     MODULE PROCEDURE ok_axis_d
  END INTERFACE
  INTERFACE ok_bounds
     MODULE PROCEDURE ok_bounds_d
  END INTERFACE
  INTERFACE ok_calendar
     MODULE PROCEDURE ok_calendar_d
  END INTERFACE
  INTERFACE ok_cell_measures
     MODULE PROCEDURE ok_cell_measures_d
  END INTERFACE
  INTERFACE ok_cell_methods
     MODULE PROCEDURE ok_cell_methods_d
  END INTERFACE
  INTERFACE ok_climatology
     MODULE PROCEDURE ok_climatology_d
  END INTERFACE
  INTERFACE ok_comment
     MODULE PROCEDURE ok_comment_d
  END INTERFACE
  INTERFACE ok_compress
     MODULE PROCEDURE ok_compress_d
  END INTERFACE
  INTERFACE ok_Conventions
     MODULE PROCEDURE ok_Conventions_d
  END INTERFACE
  INTERFACE ok_coordinates
     MODULE PROCEDURE ok_coordinates_d
  END INTERFACE
  INTERFACE ok_flag_meanings
     MODULE PROCEDURE ok_flag_meanings_d
  END INTERFACE
  INTERFACE ok_flag_values
     MODULE PROCEDURE ok_flag_values_d
  END INTERFACE
  INTERFACE ok_formula_terms
     MODULE PROCEDURE ok_formula_terms_d
  END INTERFACE
  INTERFACE ok_grid_mapping
     MODULE PROCEDURE ok_grid_mapping_d
  END INTERFACE
  INTERFACE ok_institution
     MODULE PROCEDURE ok_institution_d
  END INTERFACE
  INTERFACE ok_leap_month
     MODULE PROCEDURE ok_leap_month_d
  END INTERFACE
  INTERFACE ok_leap_year
     MODULE PROCEDURE ok_leap_year_d
  END INTERFACE
  INTERFACE ok_month_lengths
     MODULE PROCEDURE ok_month_lengths_d
  END INTERFACE
  INTERFACE ok_positive
     MODULE PROCEDURE ok_positive_d
  END INTERFACE
  INTERFACE ok_references
     MODULE PROCEDURE ok_references_d
  END INTERFACE
  INTERFACE ok_source
     MODULE PROCEDURE ok_source_d
  END INTERFACE
  INTERFACE ok_standard_error_multiplier
     MODULE PROCEDURE ok_standard_error_multiplier_d
  END INTERFACE
  INTERFACE ok_standard_name
     MODULE PROCEDURE ok_standard_name_d
  END INTERFACE
  INTERFACE ok_valid_max
     MODULE PROCEDURE ok_valid_max_d
  END INTERFACE
  INTERFACE ok_valid_min
     MODULE PROCEDURE ok_valid_min_d
  END INTERFACE
  INTERFACE ok_astro
     MODULE PROCEDURE ok_astro_d
  END INTERFACE
  INTERFACE ok_grid_type
     MODULE PROCEDURE ok_grid_type_d
  END INTERFACE
  INTERFACE ok_coordinate_system
     MODULE PROCEDURE ok_coordinate_system_d
  END INTERFACE
  !
  !! Umsetzen eines Textes in Gro&szlig;buchstaben
  INTERFACE get_uppercase_char
     MODULE PROCEDURE get_uppercase_char_0
  END INTERFACE
  !! Umsetzen eines Textes in Kleinbuchstaben
  INTERFACE get_lowercase_char
     MODULE PROCEDURE get_lowercase_char_0
     MODULE PROCEDURE get_lowercase_char_1
  END INTERFACE
  !
  ! [D.5] Assignments
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
  ! Globale Methoden mit Zugriff ueber PUBLIC Interfaces
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-INIT-Methoden <<< [ERR_NO =  1000 bis  1999]
  ! ----------------------------------------------------------------------
  !
  !! Allokieren/Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE init_att_d ( )
    !
    USE b_error, ONLY : DEBUG_b
    !! Name der Subroutine
    CHARACTER (LEN=10), PARAMETER :: c_upname='init_att_d' 
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_b > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "b_att" version 2.5 of 03/06/07                 '
          WRITE(*,*) ' Copyright (C) 2002 Bundesanstalt fuer Wasserbau'
          WRITE(*,*)
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       IF ( no_error( ) ) CALL init_var      ( ) 
       IF ( no_error( ) ) CALL init_datetime ( ) 
       IF ( no_error( ) ) CALL init_file     ( ) 
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_att_all_errors ( ) 
       ! [1.5] Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.6] endgueltiges Setzen des Initialisierungs-Indikators
       initialised = MERGE( .true., .false., no_error( ) )
    END IF
    ! 2.0 Initialisierungszaehler heraufsetzen
    n_init = n_init + 1
    !
  END SUBROUTINE init_att_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_att_d ( )
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER :: c_upname='clear_att_d' ! 
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_att_all_errors ( ) 
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.3] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.4] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.4.1] ggf. weitere Module de-initialisieren
       IF ( no_error( ) ) CALL clear_datetime ( ) 
       IF ( no_error( ) ) CALL clear_var      ( ) 
       IF ( no_error( ) ) CALL clear_file     ( ) 
       ! [1.4.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
  END SUBROUTINE clear_att_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_att_prn_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER :: c_upname='setup_att_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_datetime_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_var_prn_lun ( lun )
       IF ( no_error( ) ) CALL setup_file_prn_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_att_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_att_trc_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER :: c_upname='setup_att_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_datetime_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_var_trc_lun ( lun )
       IF ( no_error( ) ) CALL setup_file_trc_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_att_trc_lun_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_att_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_att) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=09), PARAMETER :: c_upname='new_att_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       this%id      = 0 
       this%name    = REPEAT( ' ', LEN(this%name) )
       this%name    = 'undefined' 
       this%var_id  = -1 
       NULLIFY ( this%ch )
       NULLIFY ( this%in )
       NULLIFY ( this%dp )
    END IF
    !
  END SUBROUTINE new_att_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_att_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=09), PARAMETER :: c_upname='new_att_1' ! 
    !! Z&auml;hler      
    INTEGER :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       DO i=1,SIZE(this)
          IF ( any_error( ) ) EXIT
          CALL new_att_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_att_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_att_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_att) , INTENT(INOUT) :: this ! 
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=10), PARAMETER :: c_upname='kill_att_0' ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       IF ( no_error( ) ) CALL dealloc_att_ch ( this )
       IF ( no_error( ) ) CALL dealloc_att_in ( this )
       IF ( no_error( ) ) CALL dealloc_att_dp ( this )
       IF ( no_error( ) ) CALL new_att_0      ( this )
    END IF
    !
  END SUBROUTINE kill_att_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_att_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=10), PARAMETER :: c_upname='kill_att_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       DO i=1,SIZE(this)
          IF ( any_error( ) ) EXIT
          CALL kill_att_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_att_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_att_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=08), PARAMETER :: c_upname='ok_att_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp+3) ! 
    !
    l_ok = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok(1)  = ok_att_id      ( this )
       l_ok(2)  = ok_att_name    ( this )
       l_ok(3)  = ok_att_var_id  ( this )
       l_ok(4)  = ok_att_ch      ( this )
       l_ok(5)  = ok_att_in      ( this )
       l_ok(6)  = ok_att_dp      ( this )
       l_ok(7)  = ok_att_alloc   ( this )
       l_ok(8)  = ok_att_type    ( this )
       l_ok(9)  = ok_att_contents( this )
    END IF
    !
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_att_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_att_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! R&uuml;ckgabewert: Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=08), PARAMETER :: c_upname='ok_att_1' 
    !! Z&auml;hler      
    INTEGER :: i ! 
    !
    ok(:) = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       DO i=1,SIZE(this)
          ok(i) = ok_att_0 ( this(i) )
       END DO
       ! alle Id's muessen verschieden sein
       WHERE ( .NOT. ok_att_different_id ( this(:) ) )
          ok(:) = ok_att_different_id ( this(:) )
       END WHERE
       ! alle Namen muessen verschieden sein (fuer identische Var-Id's)
       WHERE ( .NOT. ok_att_different_name ( this(:) ) )
          ok(:) = ok_att_different_name ( this(:) )
       END WHERE
       ! manche Attribute muessen untereinander konsistent sein
       WHERE ( .NOT. ok_att_consistency ( this(:) ) )
          ok(:) =  ok_att_consistency ( this(:) ) 
       END WHERE
    END IF
    !
  END FUNCTION ok_att_1
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! pr&uuml;fe au&szlig;erdem, ob die "var_id" des Objektes in einer Liste
  !! von Variablen des Typs "t_var" vorhanden ist oder nicht <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_att_0_v ( this , var ) &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_att) , INTENT(IN) :: this   ! 
    !! Liste von Variablen (Vektor)
    TYPE (t_var) , INTENT(IN) :: var(:) ! 
    !! R&uuml;ckgabewert : Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=10), PARAMETER :: c_upname='ok_att_0_v' 
    !! Lokales Feld mit Testergebnissen 
    LOGICAL :: l_ok(4) ! 
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !
    l_ok(:) = .false.
    !
    l_ok(1) = ok_att_0 ( this )
    IF ( l_ok(1) ) THEN
       IF ( this%var_id > 0 ) THEN
          l_ok(2) = ( COUNT( get_var_id(var(:)) == this%var_id ) == 1 )
       ELSE ! globales Attribut 
          l_ok(2) = .true.
       END IF
       IF ( .NOT. l_ok(2) ) THEN
          CALL setup_error_act ( all_errors(:), 6600, c_upname, c_modname )
          CALL setup_error_act ( '<AktAttName>', TRIM(get_att_name(this)) )
          WRITE(ctxt,'(I10)') this%var_id
          CALL setup_error_act ( '<AktVarId>', ctxt )
       END IF
    END IF
    ! Pruefe zus&auml;tzlich den Inhalt des Attributes, wenn die "var_id" 
    ! in der uebergebenen Liste vorhanden ist
    IF ( ALL( l_ok(1:2) ) ) l_ok(3) =  ok_att_contents_0_v ( this, this%var_id )
    IF ( ALL( l_ok(1:3) ) ) l_ok(4) =  ok_att_var_type_0_v ( this, var(:)      )
    !
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_att_0_v
  !
  !! Pr&uuml;fe ob g&uuml;ltige Datenobjekte vorliegen (Skalar) <BR>
  !! pr&uuml;fe au&szlig;erdem, ob die "var_id"'s der Objekte in einer Liste
  !! von Variablen des Typs "t_var" vorhanden sind oder nicht <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_att_1_v ( this , var ) &
       RESULT( ok )
    !! Datenobjekte (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! Liste von Variablen (Vektor)
    TYPE (t_var) , INTENT(IN) :: var(:)  ! 
    !! R&uuml;ckgabewert : Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=10), PARAMETER :: c_upname='ok_att_1_v' 
    !! Z&auml;hlervariable
    INTEGER           :: i, j, k, l, m, n, l_idx, n_idx, m_idx, k_idx ! 
    INTEGER , POINTER :: var_id(:), att_idx(:), att_jdx(:)            ! 
    !! Konstante
    LOGICAL             :: l_ok(c_max_grid_mapping_alt)               ! 
    CHARACTER (LEN=10)  :: ctxt                                       ! 
    INTEGER , PARAMETER :: c_a_max=4                                  ! 
    INTEGER , PARAMETER :: c_a_id(c_a_max) = (/ 77, 83, 87, 91 /)     ! 
    !
    ok(:) = ok_att_1 ( this(:) )
    DO i=1,SIZE(ok)
       IF ( .NOT. ok(i) ) CYCLE
       ok(i) = ok_att_0_v ( this(i), var(:) )
    END DO
    var_id => get_var_id_list_for_att ( this(:) )
    IF ( ASSOCIATED( var_id ) ) THEN
       DO j=1,SIZE(var_id)
          att_idx => get_att_idx_for_var_id ( this(:), var_id(j) )
          IF ( ASSOCIATED(att_idx) ) THEN
             ! ---------------------------------------------------------------------------------------------
             ! Nach bestimmten Variablen-Namen suchen die vorhanden sein muessen 
             ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
             ! i=1 : "ancillary_variables" muessen als Variablen vorhanden sein
             ! i=2 : "climatology" muessen als Variablen vorhanden sein
             ! i=3 : "coordinates" muessen als Variablen vorhanden sein
             ! i=4 : "grid_mapping" muessen als Variablen vorhanden sein
             ! ---------------------------------------------------------------------------------------------
             ! ... Attribute zu allen Variablen-Id's ermitteln
             DO i=1,SIZE(c_a_id)
                n_idx = get_att_idx( this(att_idx), c_att_name(c_a_id(i)) )
                IF ( n_idx > 0 ) THEN
                   DO k=1,SIZE(this(att_idx(n_idx))%ch)
                      DO l=1,get_word_in_ch_count(this(att_idx(n_idx))%ch(k))
                         IF ( .NOT. var_exists( var(:), get_word_in_ch(this(att_idx(n_idx))%ch(k),l) ) ) THEN
                            CALL setup_error_act ( all_errors(:), 6622, c_upname, c_modname )
                            WRITE(ctxt,'(I10)') var_id ; CALL setup_error_act ( '<VarId>', ctxt )
                            CALL setup_error_act ( '<AktAttName>', TRIM(c_att_name(c_a_id(i))) )
                            CALL setup_error_act ( '<AktVarName>', TRIM(get_word_in_ch(this(att_idx(n_idx))%ch(k),l)) )
                            ok(att_idx(n_idx)) = .false.
                         END IF
                      END DO
                   END DO
                END IF
             END DO
             ! ---------------------------------------------------------------------------------------------
             ! Tests mit das Koordinatensystem beschreibenden Variablen und Attributen
             ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
             ! pruefe, ob zu der Beschreibung einer Koordinate alle erforderlichen Attribute vorhanden sind
             ! ---------------------------------------------------------------------------------------------
             n_idx = get_att_idx( this(att_idx), c_att_name(91) ) ! "grid_mapping
             IF ( n_idx > 0 ) THEN
                m_idx = get_var_idx( var(:), TRIM(this(att_idx(n_idx))%ch(1)) )
                IF ( m_idx > 0 ) THEN
                   att_jdx => get_att_idx_for_var_id ( this(:), get_var_id(var(m_idx)) )
                   IF ( ASSOCIATED(att_jdx) ) THEN
                      k_idx = get_att_idx( this(att_jdx), c_att_name(67) )
                      IF ( k_idx > 0 ) THEN
                         l_idx = get_grid_mapping_name_idx ( this(att_jdx(k_idx))%ch(1) )
                         IF ( l_idx > 0 ) THEN
                            DO n=1,c_max_grid_mapping_att
                               l_ok(:) = .true.
                               DO m=1,COUNT( c_grid_mapping_att(n,:,l_idx) > 0 )
                                  l_ok(m) = ( get_att_idx(this(att_jdx),c_att_name(c_grid_mapping_att(n,m,l_idx))) > 0 )
                               END DO
                               IF ( .NOT. ALL(l_ok(:)) ) THEN
                                  CALL setup_error_act ( all_errors(:), 6623, c_upname, c_modname )
                                  CALL setup_error_act ( '<GridMappingName>', TRIM(this(att_idx(n_idx))%ch(1)) )
                                  IF ( c_grid_mapping_att(n,1,l_idx) > 0 ) THEN
                                     CALL setup_error_act ( '<AktAttName1>', TRIM(c_att_name(c_grid_mapping_att(n,1,l_idx))) )
                                     WRITE(ctxt,'(L1)') l_ok(1) ; CALL setup_error_act ( '<status1>', ctxt(1:1) )
                                  ELSE
                                     CALL setup_error_act ( '<AktAttName1>', 'nicht erforderlich' )
                                     CALL setup_error_act ( '<status1>', 'T' )
                                  END IF
                                  IF ( c_grid_mapping_att(n,2,l_idx) > 0 ) THEN
                                     CALL setup_error_act ( '<AktAttName2>', TRIM(c_att_name(c_grid_mapping_att(n,2,l_idx))) )
                                     WRITE(ctxt,'(L1)') l_ok(2) ; CALL setup_error_act ( '<status2>', ctxt(1:1) )
                                  ELSE
                                     CALL setup_error_act ( '<AktAttName2>', 'nicht erforderlich' )
                                     CALL setup_error_act ( '<status2>', 'T' )
                                  END IF
                                  ok(att_idx(n_idx)) = .false.
                               END IF
                            END DO
                         END IF
                      END IF
                      DEALLOCATE(att_jdx)
                      NULLIFY(att_jdx)
                   END IF
                END IF
             END IF
             DEALLOCATE(att_idx)
             NULLIFY(att_idx)
          END IF
       END DO
       DEALLOCATE(var_id)
       NULLIFY(var_id)
    END IF
    !
  END FUNCTION ok_att_1_v
  !
  !! Test auf Gleichheit des Typs des Attributs mit dem Typ der Variablen, f&uuml;r
  !! diejenigen Attribute, deren "c_att_type_shape" auf "va???" lautet <BR>
  !! Funktion erzeugt Fehlermeldungen
  FUNCTION ok_att_var_type_0_v ( this, var ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_att) , INTENT(IN) :: this   ! 
    !! Liste der Variablen
    TYPE (t_var) , INTENT(IN) :: var(:) !
    !! Ergebnis : Testergebnis
    LOGICAL :: res                      ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=19) , PARAMETER :: c_upname='ok_att_var_type_0_v' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=2) :: atyp     ! 
    INTEGER           :: idx, jdx ! 
    !
    res = .true. 
    IF ( this%var_id > 0 ) THEN
       idx = get_att_name_idx ( this )
       IF ( idx > 0 ) THEN
          IF ( c_att_type_shape(idx)(1:2) == 'va' ) THEN
             jdx = get_var_idx ( var(:), this%var_id )
             IF ( jdx > 0 ) THEN
                SELECT CASE ( TRIM(get_var_type(var(jdx))) )
                CASE ( 'IN', 'SH' )
                   res  = ( is_att_in ( this ) .OR. is_att_dp ( this ) )
                   atyp = 'IN'
                CASE ( 'RE', 'DP', 'TI' )
                   res  = is_att_dp ( this )
                   atyp = 'DP'
                CASE ( 'CH' )
                   res  = is_att_ch ( this )
                   atyp = 'CH'
                END SELECT
                IF ( .NOT. res ) THEN
                   CALL setup_error_act ( all_errors(:), 6624, c_upname, c_modname )
                   CALL setup_error_act ( '<AttributName>', TRIM(this%name) )
                   CALL setup_error_act ( '<AttributTyp>', atyp )
                   CALL setup_error_act ( '<VariableName>', TRIM(get_var_name(var(jdx))) )
                   CALL setup_error_act ( '<VariableTyp>', TRIM(get_var_type(var(jdx))) )
                END IF
             END IF
          END IF
       END IF
    END IF
    !
  END FUNCTION ok_att_var_type_0_v
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_att_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! Name der Function
    CHARACTER (LEN=11), PARAMETER :: c_upname='print_att_0' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7001, c_upname, c_modname, stat )
       IF ( no_error( ) ) CALL print_att_id     ( this )
       IF ( no_error( ) ) CALL print_att_name   ( this )
       IF ( no_error( ) ) CALL print_att_var_id ( this )
       IF ( no_error( ) ) CALL print_att_ch     ( this )
       IF ( no_error( ) ) CALL print_att_in     ( this )
       IF ( no_error( ) ) CALL print_att_dp     ( this )
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7002, c_upname, c_modname, stat )
       END IF
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT('# Beginn Objekt t_att ------------------------------')
8001 FORMAT('# Ende   Objekt t_att ------------------------------')
    !
  END SUBROUTINE print_att_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_att_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! Name der Function
    CHARACTER (LEN=11), PARAMETER :: c_upname='print_att_1' 
    !! Z&auml;hler
    INTEGER :: i ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       DO i=1,SIZE(this)
          IF ( any_error( ) ) EXIT
          WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) i
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7003, c_upname, c_modname, stat )
          IF ( no_error( ) ) CALL print_att_0 ( this(i) )
       END DO
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT ('# Datenobjekt-Index i = ',I10.10,' ---------------------------')
    !
  END SUBROUTINE print_att_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_att_static_d ( )
    !! Name der Function
    CHARACTER (LEN=18), PARAMETER :: c_upname='print_att_static_d' 
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) &
            initialised, prn_op, trc_op, prn_lun, trc_lun, n_init, c_max_att_name,      &
            c_max_hcs, c_max_map_proj, c_max_hdn, c_max_elli_name, c_max_vcs,           &
            c_max_hdisc, c_max_vdisc, c_max_st_type, c_max_grid_mapping_name,           &
            c_max_grid_mapping_att, c_max_grid_mapping_alt, c_max_axis, c_max_calendar, &
            c_max_cell_measures, c_max_cell_methods, c_max_positive,                    &
            c_max_standard_name_modifier
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       DO i=1,c_max_att_name
          IF ( any_error( ) ) EXIT
          WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i, c_att_type_shape(i), &
               c_att_name(i), c_att_cdg(i), c_att_typ(i)
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       END DO
       DO i=1,c_max_hcs
          IF ( any_error( ) ) EXIT
          WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat ) i, c_hor_coord_system(i)
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       END DO
       DO i=1,c_max_map_proj
          IF ( any_error( ) ) EXIT
          WRITE ( UNIT=prn_lun, FMT=8003, IOSTAT=stat ) i, c_map_proj(i) 
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       END DO
       DO i=1,c_max_hdn
          IF ( any_error( ) ) EXIT
          WRITE ( UNIT=prn_lun, FMT=8004, IOSTAT=stat ) i, c_hor_datum_name(i)
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       END DO
       DO i=1,c_max_elli_name
          IF ( any_error( ) ) EXIT
          WRITE ( UNIT=prn_lun, FMT=8005, IOSTAT=stat ) i, c_elli_name(i) 
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       END DO
       DO i=1,c_max_vcs
          IF ( any_error( ) ) EXIT
          WRITE ( UNIT=prn_lun, FMT=8006, IOSTAT=stat ) i, c_ver_coord_system(i)
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       END DO
       DO i=1,c_max_hdisc
          IF ( any_error( ) ) EXIT
          WRITE ( UNIT=prn_lun, FMT=8007, IOSTAT=stat ) i, c_hor_disc(i) 
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       END DO
       DO i=1,c_max_vdisc
          IF ( any_error( ) ) EXIT
          WRITE ( UNIT=prn_lun, FMT=8008, IOSTAT=stat ) i, c_ver_disc(i) 
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       END DO
       DO i=1,c_max_st_type
          IF ( any_error( ) ) EXIT
          WRITE ( UNIT=prn_lun, FMT=8009, IOSTAT=stat ) i, c_storage_type(i) 
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       END DO
       DO i=1,c_max_grid_mapping_name
          IF ( any_error( ) ) EXIT
          WRITE ( UNIT=prn_lun, FMT=8010, IOSTAT=stat ) i, c_grid_mapping_name(i) 
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       END DO
       DO i=1,c_max_grid_mapping_name
          IF ( any_error( ) ) EXIT
          WRITE ( UNIT=prn_lun, FMT=8011, IOSTAT=stat ) i, c_grid_mapping_att(:,:,i)
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       END DO
       DO i=1,c_max_axis
          IF ( any_error( ) ) EXIT
          WRITE ( UNIT=prn_lun, FMT=8012, IOSTAT=stat ) i, c_axis(i)
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       END DO
       DO i=1,c_max_calendar
          IF ( any_error( ) ) EXIT
          WRITE ( UNIT=prn_lun, FMT=8013, IOSTAT=stat ) i, c_calendar(i)
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       END DO
       DO i=1,c_max_cell_measures
          IF ( any_error( ) ) EXIT
          WRITE ( UNIT=prn_lun, FMT=8014, IOSTAT=stat ) i, c_cell_measures(i)
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       END DO
       DO i=1,c_max_cell_methods
          IF ( any_error( ) ) EXIT
          WRITE ( UNIT=prn_lun, FMT=8015, IOSTAT=stat ) i, c_cell_methods(i)
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       END DO
       DO i=1,c_max_positive
          IF ( any_error( ) ) EXIT
          WRITE ( UNIT=prn_lun, FMT=8016, IOSTAT=stat ) i, c_positive(i)
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       END DO
       DO i=1,c_max_standard_name_modifier
          IF ( any_error( ) ) EXIT
          WRITE ( UNIT=prn_lun, FMT=8017, IOSTAT=stat ) i, c_standard_name_modifier(i)
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       END DO
       IF ( no_error( ) ) WRITE ( UNIT=prn_lun, FMT=8100, IOSTAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       IF ( no_error( ) ) CALL print_att_all_errors_d ( )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls b_att         ',/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
    '#                  initialised = ',L1,/ &
    '#                       prn_op = ',L1,/ &
    '#                       trc_op = ',L1,/ &
    '#                      prn_lun = ',I5,/ &
    '#                      trc_lun = ',I5,/ &
    '#                       n_init = ',I5,/ &
    '#               c_max_att_name = ',I5,/ &
    '#               c_max_hcs      = ',I5,/ &
    '#               c_max_map_proj = ',I5,/ &
    '#               c_max_hdn      = ',I5,/ &
    '#              c_max_elli_name = ',I5,/ &
    '#              c_max_vcs       = ',I5,/ &
    '#                  c_max_hdisc = ',I5,/ &
    '#                  c_max_vdisc = ',I5,/ &
    '#                c_max_st_type = ',I5,/ &
    '#      c_max_grid_mapping_name = ',I5,/ &
    '#       c_max_grid_mapping_att = ',I5,/ &
    '#       c_max_grid_mapping_alt = ',I5,/ &
    '#                   c_max_axis = ',I5,/ &
    '#               c_max_calendar = ',I5,/ &
    '#          c_max_cell_measures = ',I5,/ &
    '#           c_max_cell_methods = ',I5,/ &
    '#               c_max_positive = ',I5,/ &
    '# c_max_standard_name_modifier = ',I5,/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -' )
8001 FORMAT( '#  i, c_att_*                  : idx = ',I5,', type = ',A,&
          ', name = ',A,', CDG = ',A,', STD = ',A )
8002 FORMAT( '#  i, c_hor_coord_system       : idx = ',I5,', wert = ',A )
8003 FORMAT( '#  i, c_map_proj               : idx = ',I5,', wert = ',A )
8004 FORMAT( '#  i, c_hor_datum_name         : idx = ',I5,', wert = ',A )
8005 FORMAT( '#  i, c_elli_name              : idx = ',I5,', wert = ',A )
8006 FORMAT( '#  i, c_ver_coord_system       : idx = ',I5,', wert = ',A )
8007 FORMAT( '#  i, c_hor_disc               : idx = ',I5,', wert = ',A )
8008 FORMAT( '#  i, c_ver_disc               : idx = ',I5,', wert = ',A )
8009 FORMAT( '#  i, c_storage_type           : idx = ',I5,', wert = ',A )
8010 FORMAT( '#  i, c_grid_mapping_name      : idx = ',I5,', wert = ',A )
8011 FORMAT( '#  i, c_grid_mapping_att       : idx = ',I5,', wert = ',12I3 )
8012 FORMAT( '#  i, c_axis                   : idx = ',I5,', wert = ',A )
8013 FORMAT( '#  i, c_calendar               : idx = ',I5,', wert = ',A )
8014 FORMAT( '#  i, c_cell_measures          : idx = ',I5,', wert = ',A )
8015 FORMAT( '#  i, c_cell_methods           : idx = ',I5,', wert = ',A )
8016 FORMAT( '#  i, c_positive               : idx = ',I5,', wert = ',A )
8017 FORMAT( '#  i, c_standard_name_modifier : idx = ',I5,', wert = ',A )
8100 FORMAT( &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
    '#------------------------------------------------------------') 
    !
  END SUBROUTINE print_att_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_att_all_errors_d ( )
    !! Name der Function
    CHARACTER (LEN=22), PARAMETER :: c_upname='print_att_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_att_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! weise der Komponente "id" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_att_id_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_att) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "id"
    INTEGER        , INTENT(IN)  :: val  ! 
    !
    this%id = val
    !
  END SUBROUTINE set_att_id_0_0
  !
  !! weise der Komponente "id" vieler Objekte verschiedene Werte zu (Vektor) <BR>
  !! die Zuweisung wird nur dann ausgef&uuml;hrt, falls die Anzahl der Objekte
  !! mit der Anzahl der Werte &uuml;bereinstimmt                             <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_att_id_1_1 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "id"
    INTEGER      , INTENT(IN)    :: val(:)  ! 
    !
    IF ( SIZE(this)==SIZE(val) ) this(:)%id = val(:)
    !
  END SUBROUTINE set_att_id_1_1
  !
  !! weise der Komponente "name" einen skalaren Wert zu (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_att_name_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_att)      , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "name"
    CHARACTER (LEN=*) , INTENT(IN)    :: val  ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    this%name = REPEAT( ' ', LEN(this%name) )
    this%name = val
    DO i=1,LEN_TRIM(this%name) ! Blanks durch "underscore" ersetzen
       IF ( this%name(i:i) == ' ' ) this%name(i:i) = '_'
    END DO
    !
  END SUBROUTINE set_att_name_0_0
  !
  !! weise der Komponente "name" vieler Objekte verschiedene Werte zu (Vektor) <BR>
  !! die Zuweisung wird nur dann ausgef&uuml;hrt, falls die Anzahl der Objekte
  !! mit der Anzahl der Werte &uuml;bereinstimmt                               <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_att_name_1_1 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_att)      , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "name"
    CHARACTER (LEN=*) , INTENT(IN)    :: val(:)  ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,MIN(SIZE(this),SIZE(val))
       CALL set_att_name_0_0(this(i),val(i))
    END DO
    !
  END SUBROUTINE set_att_name_1_1
  !
  !! weise der Komponente "name" einen skalaren Wert zu (Skalar) <BR>
  !! "name" wird auf den Wert "c_att_name(idx)" gesetzt          <BR>
  !! es wird nur dann ein Wert zugewiesen, falls idx kleiner oder gleich c_max_att_name ist <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_att_name_0_0_i ( this, idx )
    !! Datenobjekt (Skalar)
    TYPE (t_att) , INTENT(INOUT) :: this ! 
    !! Index f&uuml;r Attribut in "c_att_name(idx)"
    INTEGER      , INTENT(IN)    :: idx  ! 
    !
    IF ( 0 < idx .AND. idx <= c_max_att_name ) THEN
       this%name = REPEAT( ' ', LEN(this%name) )
       this%name = c_att_name(idx)
    END IF
    !
  END SUBROUTINE set_att_name_0_0_i
  !
  !! weise der Komponente "name" vieler Objekte verschiedene skalare Werte zu (Vektor) <BR>
  !! "name" wird auf die Werte "c_att_name(idx(:))" gesetzt                            <BR>
  !! es wird nur dann ein Wert zugewiesen, falls idx kleiner oder gleich c_max_att_name ist 
  !! und die Anzahl der Objekte mit der Anzahl der Werte &uuml;bereinstimmt <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_att_name_1_1_i ( this, idx )
    !! Datenobjekt (Skalar)
    TYPE (t_att) , INTENT(INOUT) :: this(:) ! 
    !! Index f&uuml;r Attribut in "c_att_name(idx)"
    INTEGER      , INTENT(IN)    :: idx(:)  ! 
    !! Z&auml;hlervariable
    INTEGER                      :: i       ! 
    !
    IF ( SIZE(idx) == SIZE(this) ) THEN
       DO i=1,SIZE(idx)
          CALL set_att_name_0_0_i ( this(i), idx(i) )
       END DO
    END IF
    !
  END SUBROUTINE set_att_name_1_1_i
  !
  !! weise der Komponente "var_id" einen skalaren Wert zu (Skalar) <BR>
  !! falls var_id gr&ouml;&szlig;er als Null angegeben wird,
  !! so wird das Attribut einer Variablen mit derselben 
  !! Id-Nummer zugewiesen <BR>
  !! falls var_id=0 ist so handelt es sich um ein globales Attribut <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_att_var_id_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_att) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "var_id"
    INTEGER      , INTENT(IN)    :: val  ! 
    !
    this%var_id = val
    !
  END SUBROUTINE set_att_var_id_0_0
  ! 
  !! weise der Komponente "var_id" vieler Objekte denselben Wert zu <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_att_var_id_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "var_id"
    INTEGER      , INTENT(IN)    :: val     ! 
    !
    this(:)%var_id = val
    !
  END SUBROUTINE set_att_var_id_1_0
  !
  !! weise der Komponente "var_id" vieler Objekte verschiedene Werte zu (Vektor) <BR>
  !! die Zuweisung wird nur dann ausgef&uuml;hrt, falls die Anzahl der Objekte
  !! mit der Anzahl der Werte &uuml;bereinstimmt                               <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_att_var_id_1_1 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "var_id"
    INTEGER      , INTENT(IN)    :: val(:)  ! 
    !
    IF ( SIZE(this)==SIZE(val) ) this(:)%var_id = val(:)
    !
  END SUBROUTINE set_att_var_id_1_1
  !
  !! weise der dynamischen Komponente "ch" einen Skalar zu <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_att_ch_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_att)      , INTENT(INOUT) :: this   ! 
    !! Wert f&uuml;r Komponente "ch" (Skalar)
    CHARACTER (LEN=*) , INTENT(IN)    :: val    ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='set_att_ch_0_0' 
    !! Hilfsfeld
    CHARACTER (LEN=LEN(val)) :: ctxt(1) ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       ctxt(1) = val
       CALL set_att_ch_0_1 ( this, ctxt(:) )
    END IF
    !
  END SUBROUTINE set_att_ch_0_0
  !
  !! weise der dynamischen Komponente "ch" einen Vektor zu <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_att_ch_0_1 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_att)      , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "ch" (Vektor)
    CHARACTER (LEN=*) , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='set_att_ch_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_att_ch ( this            )
       IF ( no_error( ) ) CALL alloc_att_ch   ( this, SIZE(val) )
       IF ( no_error( ) ) CALL init_att_ch    ( this            )
       IF ( no_error( ) ) this%ch(:) = val(:)
    END IF
    !
  END SUBROUTINE set_att_ch_0_1
  !
  !! weise der dynamischen Komponente "ch" einen Skalar zu <BR>
  !! eine Datums-/Zeitangabe vom Typ "t_datetime" wird hierzu 
  !! automatisch in einen String konvertiert, wobei die englische
  !! Spracheinstellung benutzt wird <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_att_ch_t_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_att)      , INTENT(INOUT) :: this   ! 
    !! Datums- und Zeitangabe (Skalar)
    TYPE (t_datetime) , INTENT(IN)    :: val    ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='set_att_ch_t_0' 
    !! Spracheinstellung (Default)
    INTEGER , PARAMETER :: c_l=2 ! 
    !! Hilfsfeld
    CHARACTER (LEN=34)  :: ctxt(1) ! 
    !! Memo f&uuml;r Spracheinstellung
    INTEGER :: m_l ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       m_l = get_datetime_language( )
       CALL setup_datetime_language ( c_l )
       ctxt(1) = datetime_to_string ( val )
       CALL set_att_ch_0_1 ( this, ctxt(:) )
       CALL setup_datetime_language ( m_l )
    END IF
    !
  END SUBROUTINE set_att_ch_t_0
  !
  !! weise der dynamischen Komponente "ch" einen Vektor zu <BR>
  !! mehrere Datums-/Zeitangabe vom Typ "t_datetime" werden hierzu 
  !! automatisch in ein String-Feld konvertiert, wobei die englische
  !! Spracheinstellung benutzt wird <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_att_ch_t_1 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_att)      , INTENT(INOUT) :: this   ! 
    !! Datums- und Zeitangaben (Vektor)
    TYPE (t_datetime) , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='set_att_ch_t_1' 
    !! Spracheinstellung (Default)
    INTEGER , PARAMETER :: c_l=2 ! 
    !! Memo f&uuml;r Spracheinstellung
    INTEGER :: m_l ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       m_l = get_datetime_language( )
       CALL setup_datetime_language ( c_l )
       IF ( no_error( ) ) CALL dealloc_att_ch ( this            )
       IF ( no_error( ) ) CALL alloc_att_ch   ( this, SIZE(val) )
       IF ( no_error( ) ) CALL init_att_ch    ( this            )
       IF ( no_error( ) ) this%ch(:) = datetime_to_string( val(:) )
       CALL setup_datetime_language ( m_l )
    END IF
    !
  END SUBROUTINE set_att_ch_t_1
  !
  !! weise der dynamischen Komponente "in" einen Skalar zu <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_att_in_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_att)      , INTENT(INOUT) :: this   ! 
    !! Wert f&uuml;r Komponente "in" (Skalar)
    INTEGER           , INTENT(IN)    :: val    ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='set_att_in_0_0' 
    !! Hilfsfeld
    INTEGER :: arr(1) ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       arr(1) = val
       CALL set_att_in_0_1 ( this, arr(:) )
    END IF
    !
  END SUBROUTINE set_att_in_0_0
  !
  !! weise der dynamischen Komponente "in" einen Vektor zu <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_att_in_0_1 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_att) , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "in"
    INTEGER      , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='set_att_in_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_att_in ( this            )
       IF ( no_error( ) ) CALL alloc_att_in   ( this, SIZE(val) )
       IF ( no_error( ) ) CALL init_att_in    ( this            )
       IF ( no_error( ) ) this%in(:) = val(:)
    END IF
    !
 END SUBROUTINE set_att_in_0_1
  !
  !! weise der dynamischen Komponente "dp" einen Skalar zu <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_att_dp_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_att)       , INTENT(INOUT) :: this   ! 
    !! Wert f&uuml;r Komponente "dp" (Skalar)
    REAL (KIND=Double) , INTENT(IN)    :: val    ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='set_att_dp_0_0' 
    !! Hilfsfeld
    REAL (KIND=Double) :: arr(1) ! 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       arr(1) = val
       CALL set_att_dp_0_1 ( this, arr(:) )
    END IF
    !
  END SUBROUTINE set_att_dp_0_0
  !
  !! weise der dynamischen Komponente "dp" einen Vektor zu <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE set_att_dp_0_1 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_att)       , INTENT(INOUT) :: this   ! 
    !! Werte f&uuml;r Komponente "dp"
    REAL (KIND=Double) , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='set_att_dp_0_1' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       IF ( no_error( ) ) CALL dealloc_att_dp ( this            )
       IF ( no_error( ) ) CALL alloc_att_dp   ( this, SIZE(val) )
       IF ( no_error( ) ) CALL init_att_dp    ( this            )
       IF ( no_error( ) ) this%dp(:) = val(:)
    END IF
    !
  END SUBROUTINE set_att_dp_0_1
  !
  !! f&uuml;ge einen Texteintrag zu einem schon existierenden 
  !! Attribut mit Text hinzu <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE add_att_ch_0_0 ( this, ch )
    !! Datenobjekt (Skalar)
    TYPE (t_att)      , INTENT(INOUT) :: this ! 
    !! hinzuzuf&uuml;gender Text (Skalar)
    CHARACTER (LEN=*) , INTENT(IN)    :: ch   ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='add_att_ch_0_0' 
    !! Hilfsvariable
    CHARACTER (LEN=10) :: ctxt ! 
    !! Hilfsfeld
    CHARACTER (LEN=LEN(this%ch)), ALLOCATABLE :: l_ch(:) !     
    !! Z&auml;hlervariablen
    INTEGER :: i,n ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    SELECT CASE ( get_att_type(this) )
    CASE ( 'IN ', 'DP ' ) ! falscher Typ
       CALL setup_error_act ( all_errors(:), 8540, c_upname, c_modname )
       CALL setup_error_act ( '<AktTyp>', get_att_type(this) )
       CALL setup_error_act ( '<AktAttName>', get_att_name(this) )
    CASE DEFAULT
       IF ( get_att_nof_values(this) > 0 ) THEN
          n = get_att_nof_values(this) + 1
       ELSE
          n = 1
       END IF
       ALLOCATE( l_ch(n), STAT=stat )
       IF ( stat /= 0   ) THEN
          CALL setup_error_act ( all_errors(:), 8541, c_upname, c_modname, stat )
          WRITE(ctxt,'(I10)') n
          CALL setup_error_act ( '<AktDim1>', ctxt )
          CALL setup_error_act ( '<AktAttName>', get_att_name(this) )
       ELSE
          l_ch(:) = REPEAT( ' ', LEN(l_ch(:)) )
          IF ( n > 1 ) l_ch(1:n-1) = get_att_ch( this )
          l_ch(n) = ch
          CALL set_att_ch ( this, l_ch(:) )
          DEALLOCATE ( l_ch , STAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 8542, c_upname, c_modname, stat )
             CALL setup_error_act ( '<AktAttName>', get_att_name(this) )
          END IF
       END IF
    END SELECT
    !
  END SUBROUTINE add_att_ch_0_0 
  !
  !! f&uuml;ge mehrere Texteintr&auml;ge zu einem schon existierenden 
  !! Attribut mit Text hinzu <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE add_att_ch_0_1 ( this, ch )
    !! Datenobjekt (Skalar)
    TYPE (t_att)      , INTENT(INOUT) :: this  ! 
    !! hinzuzuf&uuml;gender Text (Vektor)
    CHARACTER (LEN=*) , INTENT(IN)    :: ch(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='add_att_ch_0_1' 
    !! Z&auml;hlervariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ch)
       IF ( any_error( ) ) EXIT
       CALL add_att_ch_0_0 ( this, ch(i) )
    END DO
    !
  END SUBROUTINE add_att_ch_0_1 
  !
  !! f&uuml;ge einen Zeitangabe zu einem schon existierenden 
  !! Attribut mit Text hinzu <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE add_att_ch_t_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_att)      , INTENT(INOUT) :: this ! 
    !! hinzuzuf&uuml;gende Zeitangabe (Skalar)
    TYPE (t_datetime) , INTENT(IN)    :: val   ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='add_att_ch_t_0' 
    !! Spracheinstellung (Englisch)
    INTEGER , PARAMETER :: c_l=2 ! 
    !! Memo Spracheinstellung
    INTEGER :: m_l ! 
    !
    m_l = get_datetime_language ( )
    CALL setup_datetime_language ( c_l )
    IF ( no_error( ) ) CALL add_att_ch_0_0 ( this, datetime_to_string(val) )
    CALL setup_datetime_language ( m_l )
    !
  END SUBROUTINE add_att_ch_t_0 
  !
  !! f&uuml;ge mehrere Zeitangaben zu einem schon existierenden 
  !! Attribut mit Text hinzu <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE add_att_ch_t_1 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_att)      , INTENT(INOUT) :: this   ! 
    !! hinzuzuf&uuml;gende Zeitangaben (Vektor)
    TYPE (t_datetime) , INTENT(IN)    :: val(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='add_att_ch_t_1' 
    !! Spracheinstellung (Englisch)
    INTEGER , PARAMETER :: c_l=2 ! 
    !! Memo Spracheinstellung
    INTEGER :: m_l ! 
    !
    m_l = get_datetime_language ( )
    CALL setup_datetime_language ( c_l )
    IF ( no_error( ) ) CALL add_att_ch_0_1 ( this, datetime_to_string(val(:)) )
    CALL setup_datetime_language ( m_l )
    !
  END SUBROUTINE add_att_ch_t_1
  !
  !! f&uuml;ge eine ganze Zahl zu einem schon existierenden 
  !! Attribut mit ganzen Zahlen hinzu <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE add_att_in_0_0 ( this, in )
    !! Datenobjekt (Skalar)
    TYPE (t_att)      , INTENT(INOUT) :: this ! 
    !! hinzuzuf&uuml;gende ganze Zahl (Skalar)
    INTEGER           , INTENT(IN)    :: in   ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='add_att_in_0_0' 
    !! Hilfsvariable
    CHARACTER (LEN=10)   :: ctxt    ! 
    !! Hilfsfeld
    INTEGER, ALLOCATABLE :: l_in(:) !     
    !! Z&auml;hlervariablen
    INTEGER :: i,n ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    SELECT CASE ( get_att_type(this) )
    CASE ( 'CH ', 'DP ' ) ! falscher Typ
       CALL setup_error_act ( all_errors(:), 8550, c_upname, c_modname )
       CALL setup_error_act ( '<AktTyp>', get_att_type(this) )
       CALL setup_error_act ( '<AktAttName>', get_att_name(this) )
    CASE DEFAULT
       IF ( get_att_nof_values(this) > 0 ) THEN
          n = get_att_nof_values(this) + 1
       ELSE
          n = 1
       END IF
       ALLOCATE( l_in(n), STAT=stat )
       IF ( stat /= 0   ) THEN
          CALL setup_error_act ( all_errors(:), 8551, c_upname, c_modname, stat )
          WRITE(ctxt,'(I10)') n
          CALL setup_error_act ( '<AktDim1>', ctxt )
          CALL setup_error_act ( '<AktAttName>', get_att_name(this) )
       ELSE
          l_in(:) = HUGE( l_in )
          IF ( n > 1 ) l_in(1:n-1) = get_att_in(this)
          l_in(n) = in
          CALL set_att_in ( this, l_in(:) )
          DEALLOCATE ( l_in , STAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 8552, c_upname, c_modname, stat )
             CALL setup_error_act ( '<AktAttName>', get_att_name(this) )
          END IF
       END IF
    END SELECT
    !
  END SUBROUTINE add_att_in_0_0 
  !
  !! f&uuml;ge mehrere ganze Zahlen zu einem schon existierenden 
  !! Attribut mit ganzen Zahlen hinzu <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE add_att_in_0_1 ( this, in )
    !! Datenobjekt (Skalar)
    TYPE (t_att)      , INTENT(INOUT) :: this  ! 
    !! hinzuzuf&uuml;gende ganze Zahl (Vektor)
    INTEGER           , INTENT(IN)    :: in(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='add_att_in_0_1' 
    !! Z&auml;hlervariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(in)
       IF ( any_error( ) ) EXIT
       CALL add_att_in_0_0 ( this, in(i) )
    END DO
    !
  END SUBROUTINE add_att_in_0_1 
  !
  !! f&uuml;ge eine reelle (Double) Zahl zu einem schon existierenden 
  !! Attribut mit rellen (Double) Zahlen hinzu <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE add_att_dp_0_0 ( this, dp )
    !! Datenobjekt (Skalar)
    TYPE (t_att)      , INTENT(INOUT) :: this ! 
    !! hinzuzuf&uuml;gende relle (Double) Zahl (Skalar)
    REAL (KIND=Double), INTENT(IN)    :: dp   ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='add_att_dp_0_0' 
    !! Hilfsvariable
    CHARACTER (LEN=10)   :: ctxt    ! 
    !! Hilfsfeld
    REAL (KIND=Double) , ALLOCATABLE :: l_dp(:) !     
    !! Z&auml;hlervariablen
    INTEGER :: i,n ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    SELECT CASE ( get_att_type(this) )
    CASE ( 'CH ', 'IN ' ) ! falscher Typ
       CALL setup_error_act ( all_errors(:), 8560, c_upname, c_modname )
       CALL setup_error_act ( '<AktTyp>', get_att_type(this) )
       CALL setup_error_act ( '<AktAttName>', get_att_name(this) )
    CASE DEFAULT
       IF ( get_att_nof_values(this) > 0 ) THEN
          n = get_att_nof_values(this) + 1
       ELSE
          n = 1
       END IF
       ALLOCATE( l_dp(n), STAT=stat )
       IF ( stat /= 0   ) THEN
          CALL setup_error_act ( all_errors(:), 8561, c_upname, c_modname, stat )
          WRITE(ctxt,'(I10)') n
          CALL setup_error_act ( '<AktDim1>', ctxt )
          CALL setup_error_act ( '<AktAttName>', get_att_name(this) )
       ELSE
          l_dp(:) = HUGE( l_dp )
          IF ( n > 1 ) l_dp(1:n-1) = get_att_dp(this)
          l_dp(n) = dp
          CALL set_att_dp ( this, l_dp(:) )
          DEALLOCATE ( l_dp , STAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 8562, c_upname, c_modname, stat )
             CALL setup_error_act ( '<AktAttName>', get_att_name(this) )
          END IF
       END IF
    END SELECT
    !
  END SUBROUTINE add_att_dp_0_0 
  !
  !! f&uuml;ge mehrere relle (Double) Zahlen zu einem schon existierenden 
  !! Attribut mit rellen (Double) Zahlen hinzu <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE add_att_dp_0_1 ( this, dp )
    !! Datenobjekt (Skalar)
    TYPE (t_att)      , INTENT(INOUT) :: this  ! 
    !! hinzuzuf&uuml;gende relle (Double) Zahlen (Vektor)
    REAL (KIND=Double), INTENT(IN)    :: dp(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER :: c_upname='add_att_dp_0_1' 
    !! Z&auml;hlervariablen
    INTEGER :: i ! 
    !
    DO i=1,SIZE(dp)
       IF ( any_error( ) ) EXIT
       CALL add_att_dp_0_0 ( this, dp(i) )
    END DO
    !
  END SUBROUTINE add_att_dp_0_1 
  !
  !! einen Eintrag in dem Attribut "history" erg&auml;nzen <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE add_att_history_0_0 ( this, progname, filename )
    !! Datenobjekt (Skalar) <BR>
    !! der Eintrag wird nur dann vorgenommen falls der Name des Attributes
    !! entweder <EM>history</EM> ist
    TYPE (t_att)      , INTENT(INOUT) :: this     ! 
    !! Name des rufenden Programmes / der rufenden Programmeinheit
    CHARACTER (LEN=*) , INTENT(IN)    :: progname ! 
    !! Name der Steuerdatei des rufenden Programmes oder andere
    !! kennzeichnende Parameter des Programmaufrufs
    CHARACTER (LEN=*) , INTENT(IN)    :: filename ! 
    !! Datum und Zeit (aus System)
    TYPE (t_datetime) :: time ! 
    !! Hilfsvariable
    INTEGER           :: l, m ! 
    !
    IF ( get_lowercase_char(this%name) == get_lowercase_char(c_att_name(2)) ) THEN
       time = get_datetime_from_system( )
       CALL add_att_ch(this,time)
       l = SIZE(this%ch)
       m = LEN_TRIM(this%ch(l)) + 1
       IF ( LEN_TRIM(progname) > 0 ) this%ch(l)(m:) = ', '//TRIM(progname)
       m = LEN_TRIM(this%ch(l)) + 1
       IF ( LEN_TRIM(filename) > 0 ) this%ch(l)(m:) = ', '//TRIM(filename)
    END IF
    !
  END SUBROUTINE add_att_history_0_0
  !
  !! einen Eintrag in dem Attribut "history" in einer Liste von Attributen erg&auml;nzen
  !! oder, falls noch nicht vorhanden, ein globales Attribut mit dem Namen "history"
  !! in die Liste einf&uuml;gen <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE add_att_history_1_0 ( this, progname, filename )
    !! Datenobjekt (Vektor) <BR>
    !! der Eintrag wird entweder in einem schon vorhandenen Attribut "history"
    !! vorgenommen, oder es wird ein neues Attribut mit dem Namen "history" an
    !! die Attributliste angeh&auml;ngt
    TYPE (t_att)      , POINTER    :: this(:)   ! 
    !! Name des rufenden Programmes / der rufenden Programmeinheit
    CHARACTER (LEN=*) , INTENT(IN) :: progname ! 
    !! Name der Steuerdatei des rufenden Programmes oder andere
    !! kennzeichnende Parameter des Programmaufrufs
    CHARACTER (LEN=*) , INTENT(IN) :: filename ! 
    !! Zeiger auf Attribut "history" in der Liste von Attributen
    INTEGER      :: idx ! 
    !! Hilfsobjekt (Attribut)
    TYPE (t_att) :: att ! 
    !
    idx = get_att_idx(this(:),c_att_name(2))
    IF ( idx > 0 ) THEN ! das Attribut war schon vorhanden
       CALL add_att_history_0_0 ( this(idx), progname, filename )
    ELSE
       CALL new_att(att)
       CALL set_att_id(att,MAXVAL(this(:)%id)+1)
       CALL set_att_name(att,2)
       CALL set_att_var_id(att,0) ! globales Attribut
       CALL add_att_history(att, progname, filename)
       CALL add_att ( this, att )
    END IF
    !
  END SUBROUTINE add_att_history_1_0
  !
  !! Anh&auml;ngen eines Attributs an ein Pointer-Feld mit Attributen
  SUBROUTINE add_att_d_0 ( this1, this2 )
    !! Pointer-Feld an das ein weiteree Attribut angeh&auml;ngt werden soll
    TYPE (t_att) , POINTER     :: this1(:) !
    !! anzuh&auml;ngendes Attribut
    TYPE (t_att) , INTENT(IN)  :: this2    !
    !! Name der Subroutine
    CHARACTER (LEN=11) , PARAMETER :: c_upname='add_att_d_0' ! 
    !! lokales Hilfsfeld
    TYPE (t_att) , ALLOCATABLE :: this3(:) ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsvariable
    INTEGER :: n    ! 
    !
    IF ( ASSOCIATED( this1 ) ) THEN
       n = SIZE(this1)
       ALLOCATE( this3(n), STAT=stat )
       IF ( stat /= 0   ) THEN
          CALL setup_error_act ( all_errors(:), 8500, c_upname, c_modname, stat )
       ELSE
          CALL new_att( this3(:) )
          this3(:) = this1(:)
          DEALLOCATE ( this1, STAT=stat )
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 8501, c_upname, c_modname, stat )
          ELSE
             NULLIFY ( this1 )
          END IF
       END IF
    ELSE
       n = 0
    END IF
    IF ( no_error( ) ) THEN
       ALLOCATE ( this1(n+1), STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 8502, c_upname, c_modname, stat )
       ELSE
          IF ( ALLOCATED( this3 ) ) THEN
             this1(1:n) = this3(1:n)
             this1(n+1) = this2  
             DEALLOCATE ( this3, STAT=stat )
             IF ( stat /= 0 ) THEN
                CALL setup_error_act ( all_errors(:), 8503, c_upname, c_modname, stat )
             END IF
          ELSE
             this1(n+1) = this2
          END IF
       END IF
    END IF
    !
  END SUBROUTINE add_att_d_0
  !
  !! Anh&auml;ngen mehrerer Attribute an ein Pointer-Feld mit Dimensionsangaben
  SUBROUTINE add_att_d_1 ( this1, this2 )
    !! Pointer-Feld an das ein weiteres Attribut angeh&auml;ngt werden soll
    TYPE (t_att) , POINTER    :: this1(:) !
    !! anzuh&auml;ngende Dimensionen
    TYPE (t_att) , INTENT(IN) :: this2(:) !
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this2)
       IF ( any_error( ) ) EXIT
       CALL add_att_d_0 ( this1, this2(i) )
    END DO
    !
  END SUBROUTINE add_att_d_1
  !  
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! hole die Komponente "id" aus einem skalaren Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben         <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_att_id_0_0 ( this ) &
         RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_att) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "id" (Skalar)
    INTEGER :: val  ! 
    !
    val = this%id
    !
  END FUNCTION get_att_id_0_0
  !
  !! hole die Komponente "id" aus einem vektoriellen Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben            <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_att_id_1_0 ( this ) &
         RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "id"
    INTEGER :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%id
    !
  END FUNCTION get_att_id_1_0
  !
  !! hole die Komponente "name" aus einem skalaren Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben           <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_att_name_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_att) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "name" (Skalar)
    CHARACTER (LEN=LEN(this%name)) :: val  ! 
    !
    val = this%name
    !
  END FUNCTION get_att_name_0_0
  !
  !! hole die Komponente "name" aus einem vektoriellen Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben               <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_att_name_1_0 ( this ) &
         RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_att)     , INTENT(IN)  :: this(:)          ! 
    !! R&uuml;ckgabewert "name"
    CHARACTER (LEN=LEN(this%name)) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%name
    !
  END FUNCTION get_att_name_1_0
  !
  !! hole die Komponente "var_id" aus einem skalaren Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben             <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_att_var_id_0_0 ( this ) &
         RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_att) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "var_id" (Skalar)
    INTEGER :: val  ! 
    !
    val = this%var_id
    !
  END FUNCTION get_att_var_id_0_0
  !
  !! hole die Komponente "var_id" aus einem vektoriellen Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben                 <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_att_var_id_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "var_id"
    INTEGER :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%var_id
    !
  END FUNCTION get_att_var_id_1_0
  !
  !! hole die dynamische Feld-Komponente "ch" aus einem skalaren Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben                         <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_att_ch_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_att) , INTENT(IN)  :: this     ! 
    !! R&uuml;ckgabewert "ch" (Vektor)
    CHARACTER (LEN=LEN(this%ch)) :: val(SIZE(this%ch)) ! 
    !
    val(:) = this%ch(:)
    !
  END FUNCTION get_att_ch_0_1
  !
  !! hole die dynamische Feld-Komponente "ch" aus einem skalaren Datenobjekt <BR>
  !! die String-Werte werden in den Typ "t_datetime" konvertiert             <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben                         <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_att_ch_as_datetime_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_att) , INTENT(IN)  :: this     ! 
    !! R&uuml;ckgabewert "ch" als Datums-/Zeitangabe (Vektor)
    TYPE (t_datetime) :: val(SIZE(this%ch)) ! 
    !! Spracheinstellung f&uuml;r Datumskonversion (Englisch)
    INTEGER , PARAMETER :: c_l=2 ! 
    !! Memo f&uuml;r Spracheinstellung
    INTEGER :: m_l ! 
    !! Hilfsvariablen
    INTEGER :: idx ! 
    !
    m_l = get_datetime_language  (     )
    CALL setup_datetime_language ( c_l )
    idx = INDEX( get_lowercase_char(this%ch(1)), 'since' )
    IF ( idx > 0 ) THEN
       val(1) = get_datetime_from_unit ( ADJUSTL(this%ch(1)(idx+5:)) )
    ELSE
       val(:) = string_to_datetime ( this%ch(:)(1:34) )
    END IF
    CALL setup_datetime_language ( m_l )
    !
  END FUNCTION get_att_ch_as_datetime_0_1
  !
  !! Ermittle die Position des Auftretens f&uuml;r den Skalar
  !! (Character-String) "ch" in den aktuellen Daten des Attributs "name"
  !! mit der optionalen Variablen-Id "varid" <BR>
  !! Gro&szlig;- und Kleinschreibung werden beim Ermitteln der Ordnung nicht
  !! unterschieden <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_att_data_order_ch_1_0 ( this, ch, name, varid )     &
       RESULT(ipos)
    !! Objektliste in der nach dem Attribut "name" gesucht werden soll
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Text der in dem Attribut vorhanden sein soll
    CHARACTER (LEN=*) , INTENT(IN) :: ch      ! 
    !! Name des gesuchten Attributs
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Id der gesuchten Variablen 
    INTEGER , OPTIONAL, INTENT(IN) :: varid   ! 
    !! R&uuml;ckgabewert : Position des Auftretens des durch
    !! "ch" festgelegten Werts in den aktuellen Daten des Attributs
    !! "name" f&uuml;r die optionale Variablen-Id "varid" <BR>
    !! falls ein Wert nicht auftritt, wird "0" zur&uuml;ckgegeben <BR>
    !! Gro&szlig;- und Kleinschreibung wird nicht unterschieden
    INTEGER :: ipos ! 
    !! Hilfsvariablen
    CHARACTER (LEN=LEN(ch)) :: l_ch(1)   ! 
    INTEGER                 :: l_ipos(1) ! 
    !
    l_ch(1) = ch
    IF ( PRESENT(varid) ) THEN
       l_ipos(:) = get_att_data_order_ch_1_1( this, l_ch, name, varid )
    ELSE
       l_ipos(:) = get_att_data_order_ch_1_1( this, l_ch, name        )
    END IF
    ipos = l_ipos(1)
    !
  END FUNCTION get_att_data_order_ch_1_0
  !
  !! Ermittle die Ordnung (Reihenfolge, Position des Auftretens) der Werte in
  !! einem Vektor von Strings "ch(:)" in den aktuellen Daten des Attributs "name"
  !! mit der optionalen Variablen-Id "varid" <BR>
  !! Gro&szlig;- und Kleinschreibung werden beim Ermitteln der Ordnung nicht
  !! unterschieden <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_att_data_order_ch_1_1 ( this, ch, name, varid )     &
       RESULT(ipos)
    !! Objektliste in der nach dem Attribut "name" gesucht werden soll
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Text(e) der(die) in dem Attribut vorhanden sein soll(en)
    CHARACTER (LEN=*) , INTENT(IN) :: ch(:)   ! 
    !! Name des gesuchten Attributs
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Id der gesuchten Variablen 
    INTEGER , OPTIONAL, INTENT(IN) :: varid   ! 
    !! R&uuml;ckgabewert : Ordnung , Position des Auftretens der durch
    !! "ch(:)" festgelegten Werte in den aktuellen Daten des Attributs
    !! "name" f&uuml;r die optionale Variablen-Id "varid" <BR>
    !! falls ein Wert nicht auftritt, wird "0" zur&uuml;ckgegeben <BR>
    !! Gro&szlig;- und Kleinschreibung wird nicht unterschieden
    INTEGER :: ipos(SIZE(ch)) ! 
    !! Hilfsvariablen
    INTEGER :: i, j, idx ! 
    !
    ipos(:) = 0
    IF ( PRESENT(varid) ) THEN
       CALL gen_error_if_att_not_exists( this(:), name, varid )
       idx = get_att_idx( this(:), name, varid )
    ELSE
       CALL gen_error_if_att_not_exists( this(:), name )
       idx = get_att_idx( this(:), name )
    END IF
    IF ( no_error( ) ) THEN
       IF ( is_att_ch(this(idx)) ) THEN
          DO i=1,SIZE(ch)
             DO j=1,get_att_nof_values(this(idx))
                IF ( ipos(i) > 0 ) EXIT
                IF ( LEN_TRIM(ch(i)) == LEN_TRIM(this(idx)%ch(j) ) ) THEN
                   IF ( TRIM(get_uppercase_char(ch(i))) == TRIM(get_uppercase_char(this(idx)%ch(j))) ) ipos(i) = j
                END IF
             END DO
          END DO
       END IF
    END IF
    !
  END FUNCTION get_att_data_order_ch_1_1
  !
  !! Ermittle die Position des Auftretens f&uuml;r den Skalar
  !! (Integer-Zahl) "in" in den aktuellen Daten des Attributs "name"
  !! mit der optionalen Variablen-Id "varid" <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_att_data_order_in_1_0 ( this, in, name, varid )     &
       RESULT(ipos)
    !! Objektliste in der nach dem Attribut "name" gesucht werden soll
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Integer-Zahl die in dem Attribut vorhanden sein soll
    INTEGER           , INTENT(IN) :: in      ! 
    !! Name des gesuchten Attributs
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Id der gesuchten Variablen 
    INTEGER , OPTIONAL, INTENT(IN) :: varid   ! 
    !! R&uuml;ckgabewert : Position des Auftretens des durch
    !! "in" festgelegten Werts in den aktuellen Daten des Attributs
    !! "name" f&uuml;r die optionale Variablen-Id "varid" <BR>
    !! falls ein Wert nicht auftritt, wird "0" zur&uuml;ckgegeben
    INTEGER :: ipos ! 
    !! Hilfsvariablen
    INTEGER :: l_in(1)   ! 
    INTEGER :: l_ipos(1) ! 
    !
    l_in(1) = in
    IF ( PRESENT(varid) ) THEN
       l_ipos(:) = get_att_data_order_in_1_1( this, l_in, name, varid )
    ELSE
       l_ipos(:) = get_att_data_order_in_1_1( this, l_in, name        )
    END IF
    ipos = l_ipos(1)
    !
  END FUNCTION get_att_data_order_in_1_0
  !
  !! Ermittle die Ordnung (Reihenfolge, Position des Auftretens) der Werte in
  !! einem Vektor von Integer-Zahlen "in(:)" in den aktuellen Daten des Attributs "name"
  !! mit der optionalen Variablen-Id "varid" <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_att_data_order_in_1_1 ( this, in, name, varid )     &
       RESULT(ipos)
    !! Objektliste in der nach dem Attribut "name" gesucht werden soll
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Integer-Zahlen die in dem Attribut vorhanden sein soll
    INTEGER           , INTENT(IN) :: in(:)   ! 
    !! Name des gesuchten Attributs
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Id der gesuchten Variablen 
    INTEGER , OPTIONAL, INTENT(IN) :: varid   ! 
    !! R&uuml;ckgabewert : Ordnung , Position des Auftretens der durch
    !! "in(:)" festgelegten Werte in den aktuellen Daten des Attributs
    !! "name" f&uuml;r die optionale Variablen-Id "varid" <BR>
    !! falls ein Wert nicht auftritt, wird "0" zur&uuml;ckgegeben
    INTEGER :: ipos(SIZE(in)) ! 
    !! Hilfsvariablen
    INTEGER :: i, j, idx ! 
    !
    ipos(:) = 0
    IF ( PRESENT(varid) ) THEN
       CALL gen_error_if_att_not_exists( this(:), name, varid )
       idx = get_att_idx( this(:), name, varid )
    ELSE
       CALL gen_error_if_att_not_exists( this(:), name )
       idx = get_att_idx( this(:), name )
    END IF
    IF ( no_error( ) ) THEN
       IF ( is_att_in(this(idx)) ) THEN
          DO i=1,SIZE(in)
             DO j=1,get_att_nof_values(this(idx))
                IF ( ipos(i) > 0 ) EXIT
                IF ( in(i) == this(idx)%in(j)) ipos(i) = j
             END DO
          END DO
       END IF
    END IF
    !
  END FUNCTION get_att_data_order_in_1_1
  !
  !! Ermittle die Position des Auftretens f&uuml;r den Skalar
  !! (Double-Zahl) "dp" in den aktuellen Daten des Attributs "name"
  !! mit der optionalen Variablen-Id "varid" <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_att_data_order_dp_1_0 ( this, dp, name, varid )     &
       RESULT(ipos)
    !! Objektliste in der nach dem Attribut "name" gesucht werden soll
    TYPE (t_att)       , INTENT(IN) :: this(:) ! 
    !! Double-Zahl die in dem Attribut vorhanden sein soll
    REAL (KIND=Double) , INTENT(IN) :: dp      ! 
    !! Name des gesuchten Attributs
    CHARACTER (LEN=*)  , INTENT(IN) :: name    ! 
    !! Id der gesuchten Variablen 
    INTEGER , OPTIONAL , INTENT(IN) :: varid   ! 
    !! R&uuml;ckgabewert : Position des Auftretens des durch
    !! "dp" festgelegten Werts in den aktuellen Daten des Attributs
    !! "name" f&uuml;r die optionale Variablen-Id "varid" <BR>
    !! falls ein Wert nicht auftritt, wird "0" zur&uuml;ckgegeben
    INTEGER :: ipos ! 
    !! Hilfsvariablen
    REAL (KIND=Double) :: l_dp(1)   ! 
    INTEGER            :: l_ipos(1) ! 
    !
    l_dp(1) = dp
    IF ( PRESENT(varid) ) THEN
       l_ipos(:) = get_att_data_order_dp_1_1( this, l_dp, name, varid )
    ELSE
       l_ipos(:) = get_att_data_order_dp_1_1( this, l_dp, name        )
    END IF
    ipos = l_ipos(1)
    !
  END FUNCTION get_att_data_order_dp_1_0
  !
  !! Ermittle die Ordnung (Reihenfolge, Position des Auftretens) der Werte in
  !! einem Vektor von Double-Zahlen "dp(:)" in den aktuellen Daten des Attributs "name"
  !! mit der optionalen Variablen-Id "varid" <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_att_data_order_dp_1_1 ( this, dp, name, varid )     &
       RESULT(ipos)
    !! Objektliste in der nach dem Attribut "name" gesucht werden soll
    TYPE (t_att)       , INTENT(IN) :: this(:) ! 
    !! Double-Zahlen die in dem Attribut vorhanden sein soll
    REAL (KIND=Double) , INTENT(IN) :: dp(:)   ! 
    !! Name des gesuchten Attributs
    CHARACTER (LEN=*)  , INTENT(IN) :: name    ! 
    !! Id der gesuchten Variablen 
    INTEGER , OPTIONAL , INTENT(IN) :: varid   ! 
    !! R&uuml;ckgabewert : Ordnung , Position des Auftretens der durch
    !! "dp(:)" festgelegten Werte in den aktuellen Daten des Attributs
    !! "name" f&uuml;r die optionale Variablen-Id "varid" <BR>
    !! falls ein Wert nicht auftritt, wird "0" zur&uuml;ckgegeben
    INTEGER :: ipos(SIZE(dp)) ! 
    !! Hilfsvariablen
    INTEGER :: i, j, idx ! 
    !
    ipos(:) = 0
    IF ( PRESENT(varid) ) THEN
       CALL gen_error_if_att_not_exists( this(:), name, varid )
       idx = get_att_idx( this(:), name, varid )
    ELSE
       CALL gen_error_if_att_not_exists( this(:), name )
       idx = get_att_idx( this(:), name )
    END IF
    IF ( no_error( ) ) THEN
       IF ( is_att_dp(this(idx)) ) THEN
          DO i=1,SIZE(dp)
             DO j=1,get_att_nof_values(this(idx))
                IF ( ipos(i) > 0 ) EXIT
                IF ( dp(i) == this(idx)%dp(j)) ipos(i) = j
             END DO
          END DO
       END IF
    END IF
    !
  END FUNCTION get_att_data_order_dp_1_1
  !
  !! Ermittle die Position des Auftretens f&uuml;r den Skalar
  !! (Datums- und Zeitangabe) "ti" in den aktuellen Daten des Attributs "name"
  !! mit der optionalen Variablen-Id "varid" <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_att_data_order_ti_1_0 ( this, ti, name, varid )     &
       RESULT(ipos)
    !! Objektliste in der nach dem Attribut "name" gesucht werden soll
    TYPE (t_att)       , INTENT(IN) :: this(:) ! 
    !! Datums- und Zeitangabe die in dem Attribut vorhanden sein soll
    TYPE (t_datetime)  , INTENT(IN) :: ti      ! 
    !! Name des gesuchten Attributs
    CHARACTER (LEN=*)  , INTENT(IN) :: name    ! 
    !! Id der gesuchten Variablen 
    INTEGER , OPTIONAL , INTENT(IN) :: varid   ! 
    !! R&uuml;ckgabewert : Position des Auftretens des durch
    !! "ti" festgelegten Werts in den aktuellen Daten des Attributs
    !! "name" f&uuml;r die optionale Variablen-Id "varid" <BR>
    !! falls ein Wert nicht auftritt, wird "0" zur&uuml;ckgegeben
    INTEGER :: ipos ! 
    !! Hilfsvariablen
    TYPE (t_datetime) :: l_ti(1)   ! 
    INTEGER           :: l_ipos(1) ! 
    !
    l_ti(1) = ti
    IF ( PRESENT(varid) ) THEN
       l_ipos(:) = get_att_data_order_ti_1_1( this, l_ti, name, varid )
    ELSE
       l_ipos(:) = get_att_data_order_ti_1_1( this, l_ti, name        )
    END IF
    ipos = l_ipos(1)
    !
  END FUNCTION get_att_data_order_ti_1_0
  !
  !! Ermittle die Ordnung (Reihenfolge, Position des Auftretens) der Werte in
  !! einem Vektor von Datums- und Zeitangaben "ti(:)" in den aktuellen Daten des 
  !! Attributs "name" mit der optionalen Variablen-Id "varid" <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_att_data_order_ti_1_1 ( this, ti, name, varid )     &
       RESULT(ipos)
    !! Objektliste in der nach dem Attribut "name" gesucht werden soll
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Datums- und Zeitangaben die in dem Attribut vorhanden sein sollen
    TYPE (t_datetime) , INTENT(IN) :: ti(:)   ! 
    !! Name des gesuchten Attributs
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Id der gesuchten Variablen 
    INTEGER , OPTIONAL, INTENT(IN) :: varid   ! 
    !! R&uuml;ckgabewert : Ordnung , Position des Auftretens der durch
    !! "ti(:)" festgelegten Werte in den aktuellen Daten des Attributs
    !! "name" f&uuml;r die optionale Variablen-Id "varid" <BR>
    !! falls ein Wert nicht auftritt, wird "0" zur&uuml;ckgegeben
    INTEGER :: ipos(SIZE(ti)) ! 
    !! Hilfsvariablen
    INTEGER :: i, j, idx ! 
    TYPE (t_datetime) , ALLOCATABLE :: l_ti(:) ! 
    !
    ipos(:) = 0
    IF ( PRESENT(varid) ) THEN
       CALL gen_error_if_att_not_exists( this(:), name, varid )
       idx = get_att_idx( this(:), name, varid )
    ELSE
       CALL gen_error_if_att_not_exists( this(:), name )
       idx = get_att_idx( this(:), name )
    END IF
    IF ( no_error( ) ) THEN
       IF ( is_att_ch(this(idx)) ) THEN
          ALLOCATE( l_ti(get_att_nof_values(this(idx))) )
          l_ti = get_att_ch_as_datetime(this(idx))
          DO i=1,SIZE(ti)
             DO j=1,SIZE(l_ti)
                IF ( ipos(i) > 0 ) EXIT
                IF ( l_ti(j) == ti(i) ) ipos(i) = j
             END DO
          END DO
          DEALLOCATE( l_ti )
       END IF
    END IF
    !
  END FUNCTION get_att_data_order_ti_1_1
  !
  !! Ermittle das Dateiobjekt f&uuml;r die Systemdatei, falls eine solche vorhanden ist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_att_systemfile_1 ( this ) &
       RESULT(res)
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Dateiobjekt "Systemdatei"
    TYPE (t_file) :: res ! 
    ! Hilfsvariablen
    TYPE (t_file) , POINTER :: l_file(:) ! 
    !
    CALL new_file(res)
    l_file => get_att_related_file_1 ( this )
    IF ( ASSOCIATED(l_file) ) THEN
       res = get_file_systemfile( l_file )
       DEALLOCATE(l_file) ; NULLIFY(l_file)
    END IF
    !
  END FUNCTION get_att_systemfile_1
  !
  !! hole die dynamische Feld-Komponente "in" aus einem skalaren Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben                         <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_att_in_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_att) , INTENT(IN)  :: this     ! 
    !! R&uuml;ckgabewert "in" (Vektor)
    INTEGER :: val(SIZE(this%in)) ! 
    !
    val(:) = this%in(:)
    !
  END FUNCTION get_att_in_0_1
  !
  !! hole die dynamische Feld-Komponente "dp" aus einem skalaren Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben                         <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_att_dp_0_1 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_att) , INTENT(IN)  :: this     ! 
    !! R&uuml;ckgabewert "dp" (Vektor)
    REAL (KIND=Double) :: val(SIZE(this%dp)) ! 
    !
    val(:) = this%dp(:)
    !
  END FUNCTION get_att_dp_0_1
  !
  !! Ermittle den Typ der in einem Attribut abgelegten Daten (Skalar) <BR>
  !! "CH " == Text <BR>
  !! "IN " == ganze Zahlen <BR>
  !! "DP " == reelle Zahlen <BR>
  !! "UND" == nicht definiert <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_att_type_0_0 ( this ) &
       RESULT( val )
    !! Datenobjekt (Skalar)
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert (Skalar)
    CHARACTER (LEN=3)         :: val  ! 
    !
    val = MERGE( 'CH ', 'UND' , ASSOCIATED( this%ch ) )
    val = MERGE( 'IN ', val   , ASSOCIATED( this%in ) )
    val = MERGE( 'DP ', val   , ASSOCIATED( this%dp ) )
    !
  END FUNCTION get_att_type_0_0
  !
  !! Ermittle den Typ der in einem Attribut abgelegten Daten (Vektor)<BR>
  !! "CH " == Text <BR>
  !! "IN " == ganze Zahlen <BR>
  !! "DP " == reelle Zahlen <BR>
  !! "UND" == nicht definiert <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_att_type_1_0 ( this ) &
       RESULT( val )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:)         ! 
    !! R&uuml;ckgabewert (Vektor)
    CHARACTER (LEN=3)         :: val(SIZE(this)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i !
    !
    DO i=1,SIZE(val)
       val(i) = get_att_type_0_0 ( this(i) )
    END DO
    !
  END FUNCTION get_att_type_1_0
  !
  !! Ermittle die Anzahl der einem Attribut zugeordneten Werte (Skalar) <BR>
  !! falls keine Feld-Komponente allokiert wurde wird der Wert 0
  !! zur&uuml;ckgegeben <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_att_nof_values_0_0 ( this )               &
       RESULT( nof )
    !! Datenobjekt (Skalar)
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert (Skalar)
    INTEGER                   :: nof  ! 
    !
    SELECT CASE ( get_att_type( this ) )
    CASE ( 'CH ' )
       nof = SIZE(this%ch)
    CASE ( 'IN ' )
       nof = SIZE(this%in)
    CASE ( 'DP ' )
       nof = SIZE(this%dp)
    CASE DEFAULT
       nof = 0
    END SELECT
    !
  END FUNCTION get_att_nof_values_0_0
  !
  !! Ermittle die Anzahl der verschiedenen Attribute zugeordneten Werte (Vektor) <BR>
  !! falls keine Feld-Komponente allokiert wurde wird jeweils der Wert 0
  !! zur&uuml;ckgegeben <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION get_att_nof_values_1_0 ( this )               &
       RESULT( nof )
    !! Datenobjekt (Skalar)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! R&uuml;ckgabewert (Vektor)
    INTEGER           :: nof(SIZE(this)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(nof)
       nof(i) = get_att_nof_values_0_0 ( this(i) )
    END DO
    !
  END FUNCTION get_att_nof_values_1_0
  !
  !! Ermittle die Position eines gesuchten Attributs in einem Feld
  !! f&uuml;r eine Identifikationsnummer "id" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_att_idx_i_0 ( this, id ) &
       RESULT( idx )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! Identifikationsnummer des gesuchten Objekts
    INTEGER      , INTENT(IN) :: id      ! 
    !! R&uuml;ckgabewert : Feldindex (idx = 0 falls nicht gefunden)
    INTEGER :: idx ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    IF ( ANY( this(:)%id == id ) ) THEN
       idx = MINVAL( MINLOC( this(:)%id, this(:)%id == id ) )
    ELSE
       idx = 0
    END IF
    ! 
  END FUNCTION get_att_idx_i_0
  !
  !! Ermittle die Position eines gesuchten Attributs in einem Feld
  !! f&uuml;r mehrere vorgegebene Identifikationsnummern "id" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_att_idx_i_1 ( this, id ) &
       RESULT( idx )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! Identifikationsnummern der gesuchten Objekte
    INTEGER      , INTENT(IN) :: id(:)   ! 
    !! R&uuml;ckgabewert : Feldindices (idx = 0 falls nicht gefunden)
    INTEGER :: idx(SIZE(id)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(id)
       idx(i) = get_att_idx_i_0 ( this(:), id(i) )
    END DO
    ! 
  END FUNCTION get_att_idx_i_1
  !
  !! Ermittle die Position eines gesuchten Attributs in einem Feld
  !! f&uuml;r einen vorgegebenen Namen "name" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_att_idx_n_0 ( this, name ) &
       RESULT( idx )
    !! Datenobjekt (Vektor)
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Name des gesuchten Objekts
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! R&uuml;ckgabewert : Feldindex (idx = 0 falls nicht gefunden)
    INTEGER :: idx ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    idx = 0
    DO i=1,SIZE(this)
       IF ( idx /= 0 ) EXIT
       IF ( LEN_TRIM(this(i)%name) == LEN_TRIM(name) ) THEN
          IF ( TRIM(get_lowercase_char(this(i)%name)) == TRIM(get_lowercase_char(name)) ) idx = i
       END IF
    END DO
    ! 
  END FUNCTION get_att_idx_n_0
  !
  !! Ermittle die Position eines gesuchten Attributs in einem Feld
  !! f&uuml;r einen vorgegebenen Namen "name" und eine vorgegebene
  !! Variablen-Id "var_id" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_att_idx_n_v_0  ( this, name, var_id ) &
       RESULT( idx )
    !! Datenobjekt (Vektor)
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Name des gesuchten Objekts
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Id der Variablen des gesuchten Objekts
    INTEGER           , INTENT(IN) :: var_id  ! 
    !! R&uuml;ckgabewert : Feldindex (idx = 0 falls nicht gefunden)
    INTEGER :: idx ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    idx = 0
    DO i=1,SIZE(this)
       IF ( idx /= 0 ) EXIT
       IF ( LEN_TRIM(this(i)%name) == LEN_TRIM(name) ) THEN
          IF ( TRIM(get_lowercase_char(this(i)%name))  == TRIM(get_lowercase_char(name)) .AND. &
                    this(i)%var_id == var_id   ) idx = i
       END IF
    END DO
    ! 
  END FUNCTION get_att_idx_n_v_0
  !
  !! Ermittle die Position eines gesuchten Attributs in einem Feld
  !! f&uuml;r mehrere vorgegebene Namen "name" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_att_idx_n_1 ( this, name ) &
       RESULT( idx )
    !! Datenobjekt (Vektor)
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Namen der gesuchten Objekte
    CHARACTER (LEN=*) , INTENT(IN) :: name(:)   ! 
    !! R&uuml;ckgabewert : Feldindices (idx = 0 falls nicht gefunden)
    INTEGER :: idx(SIZE(name)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(name)
       idx(i) = get_att_idx_n_0 ( this(:), name(i) )
    END DO
    ! 
  END FUNCTION get_att_idx_n_1
  !
  !! Ermittle die Position eines gesuchten Attributs in einem Feld
  !! f&uuml;r mehrere vorgegebene Namen "name" und mehrere vorgegebene
  !! Variablen Id's "var_id" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_att_idx_n_v_1 ( this, name, var_id ) &
       RESULT( idx )
    !! Datenobjekt (Vektor)
    TYPE (t_att)      , INTENT(IN) :: this(:)   ! 
    !! Namen der gesuchten Objekte
    CHARACTER (LEN=*) , INTENT(IN) :: name(:)   ! 
    !! Variablen Id's der gesuchten Objekte
    INTEGER           , INTENT(IN) :: var_id(:) ! 
    !! R&uuml;ckgabewert : Feldindices (idx = 0 falls nicht gefunden)
    INTEGER :: idx(SIZE(name)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    idx(:) = 0
    DO i=1,MIN(SIZE(name),SIZE(var_id))
       idx(i) = get_att_idx_n_v_0 ( this(:), name(i), var_id(i) )
    END DO
    ! 
  END FUNCTION get_att_idx_n_v_1
  !
  !! Ermittle die Position eines gesuchten Attributs in einem Feld
  !! f&uuml;r eine vorgegebene Attributsangabe vom Typ "t_att" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_att_idx_d_0 ( this, att ) &
       RESULT( idx )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! Identifikationsnummer des gesuchten Objekts
    TYPE (t_att) , INTENT(IN) :: att     ! 
    !! R&uuml;ckgabewert : Feldindex (idx = 0 falls nicht gefunden)
    INTEGER :: idx ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    idx = 0
    DO i=1,SIZE(this)
       IF ( idx /= 0       ) EXIT
       IF ( this(i) == att ) idx = i
    END DO
    ! 
  END FUNCTION get_att_idx_d_0
  !
  !! Ermittle die Position einer gesuchten Attributs in einem Feld
  !! f&uuml;r mehrere vorgegebene Attribute vom Typ "t_att" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_att_idx_d_1 ( this, att ) &
       RESULT( idx )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! Attensionen der gesuchten Objekte
    TYPE (t_att) , INTENT(IN) :: att(:)  ! 
    !! R&uuml;ckgabewert : Feldindices (idx = 0 falls nicht gefunden)
    INTEGER :: idx(SIZE(att)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(att)
       idx(i) = get_att_idx_d_0 ( this(:), att(i) )
    END DO
    ! 
  END FUNCTION get_att_idx_d_1
  !
  !! Ermittle im Komponentenfeld "ch" eines
  !! skalaren Objektes vom Typ "t_att" <BR>
  !! das Element "idx", das als erstes den 
  !! gesuchten Wert aufweist. <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_att_field_idx_ch_0 ( this, ch_in )&
       RESULT( idx )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this 
    !! Identifikationsnummer des gesuchten Objekts
    CHARACTER (LEN=*), INTENT(IN) :: ch_in
    !! R&uuml;ckgabewert : Feldindex <BR>
    !! - idx = 0 falls nicht gefunden <BR>
    INTEGER :: idx ! 
    ! Z&auml;hler
    INTEGER :: i
    !
    idx = 0
    IF ( ASSOCIATED( this%ch ) .AND. SIZE(this%ch) > 0 ) THEN
       look: DO i=1,SIZE(this%ch)
          IF (ADJUSTL(TRIM( this%ch(i))) == ADJUSTL(TRIM( ch_in ))) THEN
             idx = i
             EXIT look
          ENDIF
       ENDDO look
    END IF
    ! 
  END FUNCTION get_att_field_idx_ch_0
  !
  !! Konvertiere das zu suchende Objekt vom
  !! Typ "t_datetime" in einen String und suche <BR>
  !! diesen in dem Komponentenfeld "ch" mittels
  !! get_att_field_idx_ch_0 . <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_att_field_idx_dt_0 ( this, dt_in )&
       RESULT( idx )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this 
    !! Identifikationsnummer des gesuchten Objekts
    TYPE (t_datetime) , INTENT(IN) :: dt_in 
    !! R&uuml;ckgabewert : Feldindex <BR>
    !! - idx = 0 falls nicht gefunden <BR>
    INTEGER :: idx ! 
    !
    !! Name der Subroutine
    CHARACTER (LEN=22), PARAMETER :: c_upname='get_att_field_idx_dt_0'
    !! Spracheinstellung (Default)
    INTEGER , PARAMETER :: c_l=2 ! 
    !! Hilfsfeld
    CHARACTER (LEN=34)  :: ctxt ! 
    !! Memo f&uuml;r Spracheinstellung
    INTEGER :: m_l ! 
    !
    idx = 0
    IF ( ok_initialised ( c_upname ) ) THEN
       m_l = get_datetime_language( )
       CALL setup_datetime_language ( c_l )
       ctxt = datetime_to_string ( dt_in )
       idx = get_att_field_idx_ch_0 &
            ( this, ctxt )
       CALL setup_datetime_language ( m_l )
    END IF
    ! 
  END FUNCTION get_att_field_idx_dt_0
  !
  !! Ermittle im Komponentenfeld "in" eines
  !! skalaren Objektes vom Typ "t_att" <BR>
  !! das Element "idx", das als erstes den 
  !! gesuchten Wert aufweist. <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_att_field_idx_in_0 ( this, in_in ) &
       RESULT( idx )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this 
    !! Identifikationsnummer des gesuchten Objekts
    INTEGER, INTENT(IN) :: in_in
    !! R&uuml;ckgabewert : Feldindex <BR>
    !! - idx = 0 falls nicht gefunden oder this%in(:) nicht assoziiert <BR>
    INTEGER :: idx ! 
    !
    idx = 0
    IF ( ASSOCIATED( this%in ) ) THEN
       IF ( ANY( this%in(:) ==  in_in ) ) THEN
          idx = MINVAL( MINLOC( this%in(:), this%in(:) == in_in ) )
       END IF
    END IF
    ! 
  END FUNCTION get_att_field_idx_in_0
  !
  !! Ermittle im Komponentenfeld "dp" eines
  !! skalaren Objektes vom Typ "t_att" <BR>
  !! das Element "idx", das als erstes den 
  !! gesuchten Wert aufweist. <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_att_field_idx_dp_0 ( this, dp_in ) &
       RESULT( idx )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this 
    !! Identifikationsnummer des gesuchten Objekts
    REAL (KIND=Double), INTENT(IN) :: dp_in
    !! R&uuml;ckgabewert : Feldindex <BR>
    !! - idx = 0 falls nicht gefunden oder this%dp(:) nicht assoziiert <BR>
    INTEGER :: idx ! 
    !
    idx = 0
    IF ( ASSOCIATED( this%dp ) ) THEN
       IF ( ANY( this%dp(:) ==  dp_in ) ) THEN
          idx = MINVAL( MINLOC( this%dp(:), this%dp(:) == dp_in ) )
       END IF
    END IF
    ! 
  END FUNCTION get_att_field_idx_dp_0
  !
  !! Transferiere den Inhalt einer Feldposition der related-file
  !! Attribute in eine Variable des Typs t_file <BR>
  !! Function erzeugt <EM>keine eigenen</EM> Fehlermeldungen
  FUNCTION get_att_related_file_0 ( this, idx )              &
       RESULT(file)
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! Feld-Position der zu transferierenden Daten
    INTEGER      , INTENT(IN) :: idx     ! 
    !! Variable des Typs "t_file"
    TYPE (t_file) :: file ! 
    !
    !! Liste der zu transferierenden Attribute <BR>
    !! (27) = related_file_name                <BR>
    !! (28) = related_file_form                <BR>
    !! (29) = related_file_access              <BR>
    !! (30) = related_file_type
    INTEGER , PARAMETER :: id(4) = (/27,28,29,30/) ! 
    !! Z&auml;hler
    INTEGER :: i, jdx ! 
    !
    CALL new_file ( file )
    !
    DO i=1,SIZE(id)
       IF ( any_error() ) EXIT
       jdx = get_att_idx(this(:),c_att_name(id(i)))
       IF ( jdx > 0 ) THEN
          IF ( ASSOCIATED(this(jdx)%ch) ) THEN
             IF ( idx <= SIZE(this(jdx)%ch) .AND. idx > 0 ) THEN
                SELECT CASE ( id(i) )
                CASE ( 27 )
                   CALL set_file_name   ( file, TRIM(this(jdx)%ch(idx)) )
                CASE ( 28 )
                   CALL set_file_form   ( file, TRIM(this(jdx)%ch(idx)) )
                CASE ( 29 )
                   CALL set_file_access ( file, TRIM(this(jdx)%ch(idx)) )
                CASE ( 30 )
                   CALL set_file_type   ( file, TRIM(this(jdx)%ch(idx)) )
                END SELECT
             END IF
          END IF
       END IF
    END DO
    !
  END FUNCTION get_att_related_file_0
  !
  !! Transferiere den Inhalt allere Feldpositionen der related-file
  !! Attribute in ein Feld des Typs t"file <BR>
  !! Falls keine Informationen vorhanden sind, so ist der 
  !! zur&uuml;ckgegebene Pointer nicht assoziert <BR>
  !! Function erzeugt <EM>keine eigenen</EM> Fehlermeldungen
  FUNCTION get_att_related_file_1 ( this )                   &
       RESULT( file )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! Feld des Typs "t_file"
    TYPE (t_file)   , POINTER :: file(:) ! 
    !
    !! Liste der zu transferierenden Attribute <BR>
    !! (27) = related_file_name                <BR>
    !! (28) = related_file_form                <BR>
    !! (29) = related_file_access              <BR>
    !! (30) = related_file_type
    INTEGER , PARAMETER :: id(4) = (/27,28,29,30/) ! 
    !! Z&auml;hler
    INTEGER :: i, n, jdx ! 
    !
    NULLIFY ( file ) ; n = 0
    !
    DO i=1,SIZE(id)
       IF ( any_error() ) EXIT
       jdx = get_att_idx(this(:),c_att_name(id(i)))
       IF ( jdx > 0 ) THEN
          IF ( ASSOCIATED(this(jdx)%ch) ) n = MAX(n,SIZE(this(jdx)%ch))
       END IF
    END DO
    !
    IF ( n > 0 ) THEN
       ALLOCATE ( file(n) ) ; CALL new_file( file(:) )
       DO i=1,n
          file(i) = get_att_related_file_0 ( this(:), i )
       END DO
    END IF
    !
  END FUNCTION get_att_related_file_1
  !
  !! Ermittle die Anzahl der Attribute, die in einem Feld von Attributen
  !! f&uuml;r eine bestimmte Variable mit Id "var_id" vorhanden sind <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_nof_att_for_var_id_0 ( this, var_id )              &
       RESULT( nof )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! Id der Variablen (Skalar)
    INTEGER      , INTENT(IN) :: var_id  ! 
    !! R&uuml;ckgabewert : Anzahl der Attribute f&uuml;r "var_id"
    INTEGER :: nof ! 
    !
    nof = COUNT( this(:)%var_id == var_id )
    !
  END FUNCTION get_nof_att_for_var_id_0
  !
  !! Ermittle die Anzahl der Attribute, die in einem Feld von Attributen
  !! f&uuml;r verschiedene Variablen mit Id "var_id(:)" vorhanden sind <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_nof_att_for_var_id_1 ( this, var_id )              &
       RESULT( nof )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:)   ! 
    !! Id's der Variablen (Vektor)
    INTEGER      , INTENT(IN) :: var_id(:) ! 
    !! R&uuml;ckgabewert : Anzahl der Attribute f&uuml;r "var_id(:)" (Vektor)
    INTEGER :: nof(SIZE(var_id)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(nof)
       nof(i) = get_nof_att_for_var_id_0 ( this(:), var_id(i) )
    END DO
    !
  END FUNCTION get_nof_att_for_var_id_1
  !
  !! Ermittle die Anzahl der Attribute, die in einem Feld von Attributen
  !! f&uuml;r eine bestimmten Namen "name" vorhanden sind <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_nof_att_for_name_0 ( this, name ) &
       RESULT( nof )
    !! Datenobjekt (Vektor)
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Name des Attributs (Skalar)
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! R&uuml;ckgabewert : Anzahl der Attribute f&uuml;r "name"
    INTEGER :: nof ! 
    !! Hilfsvariable
    INTEGER :: i, l1, l2 ! 
    !
    nof = 0
    l1  = LEN_TRIM(name)
    DO i=1,SIZE(this)
       l2 = LEN_TRIM(this(i)%name)
       IF ( l1 == l2 ) THEN
          IF ( this(i)%name(1:l2) == name(1:l1) ) nof = nof + 1
       END IF
    END DO
    !
  END FUNCTION get_nof_att_for_name_0
  !
  !! Ermittle die Anzahl der Attribute, die in einem Feld von Attributen
  !! f&uuml;r verschiedene Namen "name(:)" vorhanden sind <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_nof_att_for_name_1 ( this, name ) &
       RESULT( nof )
    !! Datenobjekt (Vektor)
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Namen der Attribute (Vektor)
    CHARACTER (LEN=*) , INTENT(IN) :: name(:) ! 
    !! R&uuml;ckgabewert : Anzahl der Attribute f&uuml;r "name(:)" (Vektor)
    INTEGER :: nof(SIZE(name)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(nof)
       nof(i) = get_nof_att_for_name_0 ( this(:), name(i) )
    END DO
    !
  END FUNCTION get_nof_att_for_name_1
  !
  !! Ermittle die Anzahl der Attribute, die in einem Feld von Attributen
  !! f&uuml;r eine bestimmten Attribut-Namen "name" sowie einem bestimmten
  !! Inhakt (Zeichen) vorhanden sind <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_nof_att_for_contents_ch_0 ( this, name, ch ) &
       RESULT( nof )
    !! Datenobjekt (Vektor)
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Name des Attributs (Skalar)
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Inhelt des Attributs
    CHARACTER (LEN=*) , INTENT(IN) :: ch      ! 
    !! R&uuml;ckgabewert : Anzahl der Attribute f&uuml;r Kombination "name" und "ch"
    INTEGER :: nof ! 
    !! Hilfsvariable
    CHARACTER (LEN=c_len_att_ch) :: l1_string, l2_string ! 
    INTEGER            :: i, l1, l2            ! 
    INTEGER  , POINTER :: p_idx(:)             ! 
    !
    nof   = 0
    p_idx => get_att_idx_for_name ( this(:), name )
    IF ( ASSOCIATED(p_idx) ) THEN
       DO i=1,SIZE(p_idx)
          IF ( .NOT. is_att_ch(this(p_idx(i))) ) CYCLE
          l1_string = REPEAT( ' ', LEN(l1_string) )
          l2_string = REPEAT( ' ', LEN(l2_string) )
          l1_string = get_word_in_ch(ch,1)
          l2_string = get_word_in_ch(this(p_idx(i))%ch(1),1)
          l1 = LEN_TRIM(l1_string)
          l2 = LEN_TRIM(l2_string)
          IF ( l1 == l2 ) THEN
             IF ( l1_string(1:l1) == l2_string(1:l2) ) nof = nof + 1
          END IF
       END DO
       DEALLOCATE(p_idx) ; NULLIFY(p_idx)
    END IF
    !
  END FUNCTION get_nof_att_for_contents_ch_0
  !
  !! ermittle eine Liste der Indices "idx(:)" von Attributen die einer 
  !! Variablen "var_id" zugeordnet sind "att(idx(:))" (t_att) <BR>
  !! ist kein Attribut vorhanden so wird ein NULL-Pointer zur&uuml;ckgegeben <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_att_idx_for_var_id_0 ( this, var_id ) &
       RESULT( idx )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! Id der Variablen (Skalar)
    INTEGER      , INTENT(IN) :: var_id  ! 
    !! R&uuml;ckgabewert : Indices der Attribute "idx(:)" (Pointer)
    INTEGER , POINTER :: idx(:) !  
    !! Name der Funktion
    CHARACTER (LEN=24) , PARAMETER :: c_upname='get_att_idx_for_var_id_0' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hlervariable
    INTEGER :: i, n ! 
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !
    NULLIFY ( idx )
    IF ( get_nof_att_for_var_id ( this(:), var_id ) > 0 ) THEN
       ALLOCATE ( idx(get_nof_att_for_var_id(this(:),var_id)), STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 9500, c_upname, c_modname, stat )
          WRITE(ctxt,'(I10)') get_nof_att_for_var_id (this(:),var_id)
          CALL setup_error_act ( '<AktDim1>', ctxt )
          WRITE(ctxt,'(I10)') var_id
          CALL setup_error_act ( '<AktVarId>', ctxt )
       ELSE
          n = 0
          DO i=1,SIZE(this)
             IF ( get_att_var_id ( this(i) ) == var_id ) THEN
                n      = n + 1
                idx(n) = i
             END IF
          END DO
       END IF
    END IF
    !
  END FUNCTION get_att_idx_for_var_id_0
  !
  !! ermittle eine Liste der Indices "idx(:)" von Attributen die einem
  !! Namen "name" zugeordnet sind "att(idx(:))" (t_att) <BR>
  !! ist kein Attribut vorhanden so wird ein NULL-Pointer zur&uuml;ckgegeben <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_att_idx_for_name_0 ( this, name ) &
       RESULT( idx )
    !! Datenobjekt (Vektor)
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Name des Attributs (Skalar)
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! R&uuml;ckgabewert : Indices der Attribute "idx(:)" (Pointer)
    INTEGER , POINTER :: idx(:) !  
    !! Name der Funktion
    CHARACTER (LEN=22) , PARAMETER :: c_upname='get_att_idx_for_name_0' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsvariable
    INTEGER            :: i, n, l1, l2 ! 
    CHARACTER (LEN=10) :: ctxt         ! 
    !
    NULLIFY ( idx )
    n = get_nof_att_for_name(this(:),name)
    IF ( n > 0 ) THEN
       ALLOCATE ( idx(n), STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 9501, c_upname, c_modname, stat )
          WRITE(ctxt,'(I10)') n
          CALL setup_error_act ( '<AktDim1>', ctxt )
          CALL setup_error_act ( '<AktName>', TRIM(name) )
       ELSE
          n  = 0
          l1 = LEN_TRIM(name)
          DO i=1,SIZE(this)
             l2 = LEN_TRIM(this(i)%name)
             IF ( l1 == l2 ) THEN
                IF ( name(1:l1) == this(i)%name(1:l2) ) THEN
                   n      = n + 1
                   idx(n) = i
                END IF
             END IF
          END DO
       END IF
    END IF
    !
  END FUNCTION get_att_idx_for_name_0
  !
  !! ermittle eine Liste der Indices "idx(:)" von Attributen die einem
  !! Namen "name" zugeordnet sind sowie einen bestimmten Inhalt aufweisen <BR>
  !! ist kein Attribut vorhanden so wird ein NULL-Pointer zur&uuml;ckgegeben <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION get_att_idx_for_contents_ch_0 ( this, name, ch ) &
       RESULT( idx )
    !! Datenobjekt (Vektor)
    TYPE (t_att)      , INTENT(IN) :: this(:)  ! 
    !! Name des Attributs (Skalar)
    CHARACTER (LEN=*) , INTENT(IN) :: name     ! 
    !! Inhalt des Attributs
    CHARACTER (LEN=*) , INTENT(IN) :: ch       ! 
    !! R&uuml;ckgabewert : Indices der Attribute "idx(:)" (Pointer)
    INTEGER , POINTER :: idx(:)                !  
    !! Name der Funktion
    CHARACTER (LEN=29) , PARAMETER :: c_upname='get_att_idx_for_contents_ch_0' ! 
    !! Statusvariable
    INTEGER            :: stat                 ! 
    !! Hilfsvariable
    CHARACTER (LEN=10) :: ctxt                 ! 
    CHARACTER (LEN=c_len_att_ch) :: l1_string, l2_string ! 
    INTEGER            :: i, n, l1, l2         ! 
    INTEGER , POINTER  :: p_idx(:)             ! 
    !
    NULLIFY ( idx )
    n = get_nof_att_for_contents(this(:),name,ch)
    IF ( n > 0 ) THEN
       ALLOCATE ( idx(n), STAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 9502, c_upname, c_modname, stat )
          WRITE(ctxt,'(I10)') n
          CALL setup_error_act ( '<AktDim1>', ctxt )
          CALL setup_error_act ( '<AktName>', TRIM(name) )
          CALL setup_error_act ( '<AktContents>', TRIM(ch) )
       ELSE
          p_idx => get_att_idx_for_name ( this(:), name )
          IF ( ASSOCIATED(p_idx) ) THEN
             n  = 0
             DO i=1,SIZE(p_idx)
                IF ( .NOT. is_att_ch(this(p_idx(i))) ) CYCLE
                l1_string = REPEAT( ' ', LEN(l1_string) )
                l2_string = REPEAT( ' ', LEN(l2_string) )
                l1_string = get_word_in_ch(ch,1)
                l2_string = get_word_in_ch(this(p_idx(i))%ch(1),1)
                l1 = LEN_TRIM(l1_string)
                l2 = LEN_TRIM(l2_string)
                IF ( l1 == l2 ) THEN
                   IF ( l1_string(1:l1) == l2_string(1:l2) ) THEN
                      n      = n + 1
                      idx(n) = p_idx(i)
                   END IF
                END IF
             END DO
             DEALLOCATE(p_idx) ; NULLIFY(p_idx)
          END IF
       END IF
    END IF
    !
  END FUNCTION get_att_idx_for_contents_ch_0
  !
  !! Konsistentes Ersetzen einer Variablen-Id durch einen neuen
  !! Wert in einem Feld von Variablen und Attributen <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE replace_att_var_id_1_1 ( o_var_id, n_var_id, var, att )
    !! alte Variablen-Id, die ersetzt werden soll
    INTEGER , INTENT(IN) :: o_var_id ! 
    !! neue Variablen-Id
    INTEGER , INTENT(IN) :: n_var_id ! 
    !! Liste der Variablen
    TYPE (t_var) , INTENT(INOUT) :: var(:) ! 
    !! Liste der Attribute
    TYPE (t_att) , INTENT(INOUT) :: att(:) ! 
    !! lokale Hilfsfelder f&uuml;r Variablen-Id's
    INTEGER :: l_n_var_id(SIZE(var)) ! 
    !! maximale Variablen Id
    INTEGER :: max_var_id ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    IF ( o_var_id /= n_var_id ) THEN
       l_n_var_id(:) = get_var_id( var(:) )
       WHERE ( l_n_var_id(:) ==  n_var_id ) l_n_var_id(:) = -n_var_id
       WHERE ( l_n_var_id(:) ==  o_var_id ) l_n_var_id(:) =  n_var_id
       max_var_id = MAXVAL(l_n_var_id(:))+1
       WHERE ( l_n_var_id(:) == -n_var_id ) l_n_var_id(:) = max_var_id
       CALL set_var_id ( var(:), l_n_var_id(:) )
       !
       WHERE ( att(:)%var_id ==  n_var_id ) att(:)%var_id =  -n_var_id
       WHERE ( att(:)%var_id ==  o_var_id ) att(:)%var_id =   n_var_id
       WHERE ( att(:)%var_id == -n_var_id ) att(:)%var_id = max_var_id
    END IF
    !
  END SUBROUTINE replace_att_var_id_1_1
  !
  !! Ermittle ob in einem Feld ein Attribut mit vorgegebener
  !! Identifikationsnummer "id" existiert <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION att_exists_i_0 ( this, id ) &
       RESULT( ex )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! Identifikationsnummer des gesuchten Objekts
    INTEGER      , INTENT(IN) :: id      ! 
    !! R&uuml;ckgabewert : .true./.false.
    LOGICAL :: ex ! 
    !
    ex = ( get_att_idx_i_0 ( this(:), id ) > 0 )
    ! 
  END FUNCTION att_exists_i_0
  !
  !! Ermittle ob in einem Feld Attribute mit vorgegebenen
  !! Identifikationsnummern "id" vorhanden sind <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION att_exists_i_1 ( this, id ) &
       RESULT( ex )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! Identifikationsnummern der gesuchten Objekte
    INTEGER      , INTENT(IN) :: id(:)   ! 
    !! R&uuml;ckgabewert : .true./.false. f&uuml;r alle Id's
    LOGICAL :: ex(SIZE(id)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ex)
       ex(i) = att_exists_i_0 ( this(:), id(i) )
    END DO
    ! 
  END FUNCTION att_exists_i_1
  !
  !! Ermittle ob in einem Feld ein Attribut mit vorgegebenem
  !! Namen "name" existiert <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION att_exists_n_0 ( this, name ) &
       RESULT( ex )
    !! Datenobjekt (Vektor)
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Name des gesuchten Objekts
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! R&uuml;ckgabewert : .true./.false.
    LOGICAL :: ex ! 
    !
    ex = ( get_att_idx_n_0 ( this(:), name ) > 0 )
    ! 
  END FUNCTION att_exists_n_0
  !
  !! Ermittle ob in einem Feld ein Attribut mit vorgegebenem
  !! Namen "name" und vorgegebener Variablen-Id existiert <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION att_exists_n_v_0 ( this, name, var_id ) &
       RESULT( ex )
    !! Datenobjekt (Vektor)
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Name des gesuchten Objekts
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Id der gesuchten Variablen
    INTEGER           , INTENT(IN) :: var_id  ! 
    !! R&uuml;ckgabewert : .true./.false.
    LOGICAL :: ex ! 
    !
    ex = ( get_att_idx_n_v_0 ( this(:), name, var_id ) > 0 )
    ! 
  END FUNCTION att_exists_n_v_0
  !
  !! Ermittle ob in einem Feld Attribute mit vorgegebenen
  !! Namen "name" vorhanden sind <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION att_exists_n_1 ( this, name ) &
       RESULT( ex )
    !! Datenobjekt (Vektor)
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Namen der gesuchten Objekte
    CHARACTER (LEN=*) , INTENT(IN) :: name(:) ! 
    !! R&uuml;ckgabewert : .true./.false. f&uuml;r alle Namen
    LOGICAL :: ex(SIZE(name)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ex)
       ex(i) = att_exists_n_0 ( this(:), name(i) )
    END DO
    ! 
  END FUNCTION att_exists_n_1
  !
  !! Ermittle ob in einem Feld Attribute mit vorgegebenen
  !! Namen "name" und Variablen Id's "var_id" vorhanden sind <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION att_exists_n_v_1 ( this, name, var_id ) &
       RESULT( ex )
    !! Datenobjekt (Vektor)
    TYPE (t_att)      , INTENT(IN) :: this(:)   ! 
    !! Namen der gesuchten Objekte
    CHARACTER (LEN=*) , INTENT(IN) :: name(:)   ! 
    !! Id's der gesuchten Variablen
    INTEGER           , INTENT(IN) :: var_id(:) ! 
    !! R&uuml;ckgabewert : .true./.false. f&uuml;r alle Namen
    LOGICAL :: ex(MIN(SIZE(name),SIZE(var_id))) ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ex)
       ex(i) = att_exists_n_v_0 ( this(:), name(i), var_id(i) )
    END DO
    ! 
  END FUNCTION att_exists_n_v_1
  !
  !! Ermittle ob in einem Feld ein Attribut mit vorgegebenen
  !! Attributen "att" existiert <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION att_exists_d_0 ( this, att ) &
       RESULT( ex )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! Attension des gesuchten Objekts
    TYPE (t_att) , INTENT(IN) :: att     ! 
    !! R&uuml;ckgabewert : .true./.false.
    LOGICAL :: ex ! 
    !
    ex = ( get_att_idx_d_0 ( this(:), att ) > 0 )
    ! 
  END FUNCTION att_exists_d_0
  !
  !! Ermittle ob in einem Feld Attribute mit vorgegebenen
  !! Attributen "att" vorhanden sind <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION att_exists_d_1 ( this, att ) &
       RESULT( ex )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! Attensionen der gesuchten Objekte
    TYPE (t_att) , INTENT(IN) :: att(:)  ! 
    !! R&uuml;ckgabewert : .true./.false. f&uuml;r alle Id's
    LOGICAL :: ex(SIZE(att)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ex)
       ex(i) = att_exists_d_0 ( this(:), att(i) )
    END DO
    ! 
  END FUNCTION att_exists_d_1
  !
  !! Ermittle, ob ein Attribut Text als Inhalt aufweist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_att_ch_0_0 ( this )          &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = ( get_att_type( this ) == 'CH ' )
    !
  END FUNCTION is_att_ch_0_0
  !
  !! Ermittle, ob viele Attribute Text als Inhalt aufweisen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_att_ch_1_0 ( this )          &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! R&uuml;ckgabewert : Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Z&auml;ervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = is_att_ch_0_0 ( this(i) )
    END DO
    !
  END FUNCTION is_att_ch_1_0
  !
  !! Ermittle, ob ein Attribut ganze Zahlen als Inhalt aufweist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_att_in_0_0 ( this )          &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = ( get_att_type( this ) == 'IN ' )
    !
  END FUNCTION is_att_in_0_0
  !
  !! Ermittle, ob viele Attribute ganze Zahlen als Inhalt aufweisen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_att_in_1_0 ( this )          &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! R&uuml;ckgabewert : Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Z&auml;ervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = is_att_in_0_0 ( this(i) )
    END DO
    !
  END FUNCTION is_att_in_1_0
  !
  !! Ermittle, ob ein Attribut reelle (Double) Zahlen als Inhalt aufweist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_att_dp_0_0 ( this )          &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = ( get_att_type( this ) == 'DP ' )
    !
  END FUNCTION is_att_dp_0_0
  !
  !! Ermittle, ob viele Attribute reelle (Double) Zahlen als Inhalt aufweisen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_att_dp_1_0 ( this )          &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! R&uuml;ckgabewert : Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Z&auml;ervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = is_att_dp_0_0 ( this(i) )
    END DO
    !
  END FUNCTION is_att_dp_1_0
  !
  !! Ermittle, ob eine Zeitangabe in den Daten exakt enthalten ist oder nicht
  !! Function erzeugt <EM>keine eigenen</EM> Fehlermeldungen
  FUNCTION is_datetime_in_att_0 ( this, var )            &
       RESULT(ok)
    !! Liste der aktuellen Attribute
    TYPE (t_att)      , INTENT(IN) :: this(:) !
    !! aktueller Termin
    TYPE (t_datetime) , INTENT(IN) :: var     !
    !! Testergebnis (Skalar)
    LOGICAL :: ok                             ! 
    !
    !! Liste der Attribut-Typen <BR>
    !! 6 = single_date    <BR>
    !! 7 = multiple_dates
    INTEGER , PARAMETER :: id(2) = (/6,7/) ! 
    !! Hilfsfeld f&uuml;r Datumsangaben
    TYPE (t_datetime) , ALLOCATABLE :: time(:) ! 
    !! Hilfsvariablen
    INTEGER :: idx(2)                          ! 
    !! Z&auml;hler
    INTEGER :: i, n                            ! 
    !
    ok     = .false.
    idx(:) = get_att_idx( this(:), c_att_name(id(:)) )
    !
    IF ( .NOT. ok .AND. idx(1) > 0 ) THEN
       IF ( is_att_ch( this(idx(1)) ) ) THEN
          ok = ALL( var == get_att_ch_as_datetime(this(idx(1))) )
       END IF
    END IF
    !
    IF ( .NOT. ok .AND. idx(2) > 0 ) THEN
       IF ( is_att_ch( this(idx(2)) ) ) THEN
          ALLOCATE( time(SIZE(this(idx(2))%ch)) )
          time = get_att_ch_as_datetime( this(idx(2)) )
          ok   = ( COUNT( time(:) == var ) == 1 )
          DEALLOCATE( time )
       END IF
    END IF
    !
  END FUNCTION is_datetime_in_att_0
  !
  !! Ermittle, ob mehrere Zeitangaben (exakt) in den Daten enthalten sind 
  !! oder nicht
  !! Function erzeugt <EM>keine eigenen</EM> Fehlermeldungen
  FUNCTION is_datetime_in_att_1 ( this, var )                   &
       RESULT(ok)
    !! Liste der aktuellen Attribute
    TYPE (t_att)      , INTENT(IN) :: this(:) !
    !! aktuelle Termine (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: var(:)  !
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(var))                  ! 
    !
    INTEGER :: i                              ! 
    !
    DO i=1,SIZE(var)
       ok(i) = is_datetime_in_att_0 ( this(:), var(i) )
    END DO
    !
  END FUNCTION is_datetime_in_att_1
  !
  !! Ermittle, ob eine Zeitangabe in den Daten enthalten ist oder nicht
  !! Function erzeugt <EM>keine eigenen</EM> Fehlermeldungen
  FUNCTION is_datetime_in_att_period_0 ( this, var )                   &
       RESULT(ok)
    !! Liste der aktuellen Attribute
    TYPE (t_att)      , INTENT(IN) :: this(:) !
    !! aktueller Termin
    TYPE (t_datetime) , INTENT(IN) :: var     !
    !! Testergebnis (Skalar)
    LOGICAL :: ok                             ! 
    !
    !! Liste der Attribut-Typen <BR>
    !! 3 = beginning_date <BR>
    !! 4 = ending_date    <BR>
    !! 6 = single_date    <BR>
    !! 7 = multiple_dates
    INTEGER , PARAMETER :: id(4) = (/3,4,6,7/) ! 
    !! Hilfsfeld f&uuml;r Datumsangaben
    TYPE (t_datetime) , ALLOCATABLE :: time(:) ! 
    !! Hilfsvariablen
    INTEGER :: idx(4)                          ! 
    !! Z&auml;hler
    INTEGER :: i, n                            ! 
    !
    ok     = .false.
    idx(:) = get_att_idx( this(:), c_att_name(id(:)) )
    !
    IF ( ALL(idx(1:2) > 0 ) ) THEN
       IF ( ALL( is_att_ch( this(idx(1:2)) ) ) ) THEN
          ok = ( ALL( var >= get_att_ch_as_datetime(this(idx(1))) ) .AND. &
                 ALL( var <= get_att_ch_as_datetime(this(idx(2))) ) )
       END IF
    END IF
    !
    IF ( .NOT. ok .AND. idx(3) > 0 ) THEN
       IF ( is_att_ch( this(idx(3)) ) ) THEN
          ok = ALL( var == get_att_ch_as_datetime(this(idx(3))) )
       END IF
    END IF
    !
    IF ( .NOT. ok .AND. idx(4) > 0 ) THEN
       IF ( is_att_ch( this(idx(4)) ) ) THEN
          ALLOCATE( time(SIZE(this(idx(4))%ch)) )
          n    = SIZE(time)
          time = get_att_ch_as_datetime( this(idx(4)) )
          ok   = ( var >= time(1) .AND. var <= time(n) )
          DEALLOCATE( time )
       END IF
    END IF
    !
  END FUNCTION is_datetime_in_att_period_0
  !
  !! Ermittle, ob mehrere Zeitangaben in den Daten enthalten sind 
  !! oder nicht
  !! Function erzeugt <EM>keine eigenen</EM> Fehlermeldungen
  FUNCTION is_datetime_in_att_period_1 ( this, var )                   &
       RESULT(ok)
    !! Liste der aktuellen Attribute
    TYPE (t_att)      , INTENT(IN) :: this(:) !
    !! aktuelle Termine (Vektor)
    TYPE (t_datetime) , INTENT(IN) :: var(:)  !
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(var))                  ! 
    !
    INTEGER :: i                              ! 
    !
    DO i=1,SIZE(var)
       ok(i) = is_datetime_in_att_period_0 ( this(:), var(i) )
    END DO
    !
  END FUNCTION is_datetime_in_att_period_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_att_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_att) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_att) , INTENT(IN) :: this2 ! 
    !! R&uuml;ckgabewert : Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Testergebnisse f&uuml;r alle Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1)  = eq_att_id     ( this1, this2 )
    l_ok(2)  = eq_att_name   ( this1, this2 )
    l_ok(3)  = eq_att_var_id ( this1, this2 )
    l_ok(4)  = eq_att_ch     ( this1, this2 )
    l_ok(5)  = eq_att_in     ( this1, this2 )
    l_ok(6)  = eq_att_dp     ( this1, this2 )
    !
    ok = ALL( l_ok )
    !
  END FUNCTION eq_att_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_att_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_att) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_att) , INTENT(IN) :: this2    ! 
    !! R&uuml;ckgabewert : Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_att_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_att_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_att_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_att) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_att) , INTENT(IN) :: this2(:) ! 
    !! R&uuml;ckgabewert : Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_att_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_att_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_att_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_att) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_att) , INTENT(IN) :: this2(:) ! 
    !! R&uuml;ckgabewert : Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_att_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_att_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(+)-Methoden <<< [ERR_NO = 11000 bis 11999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(-)-Methoden <<< [ERR_NO = 12000 bis 12999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(*)-Methoden <<< [ERR_NO = 13000 bis 13999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(/)-Methoden <<< [ERR_NO = 14000 bis 14999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(>)-Methoden <<< [ERR_NO = 15000 bis 15999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(>=)-Methoden <<< [ERR_NO = 16000 bis 16999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(<)-Methoden <<< [ERR_NO = 17000 bis 17999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(<=)-Methoden <<< [ERR_NO = 18000 bis 18999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(/=)-Methoden <<< [ERR_NO = 19000 bis 19999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_att_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_att) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_att) , INTENT(IN) :: this2 ! 
    !! R&uuml;ckgabewert : Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_att_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Skalar ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_att_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_att) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_att) , INTENT(IN) :: this2    ! 
    !! R&uuml;ckgabewert : Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ok = .NOT. ( this1(:) == this2 )
    !
  END FUNCTION ne_att_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Vektor )<BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
 FUNCTION ne_att_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_att) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_att) , INTENT(IN) :: this2(:) ! 
    !! R&uuml;ckgabewert : Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ok = .NOT. ( this1 == this2(:) )
    !
  END FUNCTION ne_att_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Vektor ) <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_att_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_att) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_att) , INTENT(IN) :: this2(:) ! 
    !! R&uuml;ckgabewert : Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    ok = .NOT. ( this1(:) == this2(:) )
    !
  END FUNCTION ne_att_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-ASSIGNMENT(=)-Methoden <<< [ERR_NO = 21000 bis 21999]
  ! ----------------------------------------------------------------------
  !
  !! Zuweisung zwischen zwei skalaren Objekte des Typs "t_att" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE as_att_0_0 ( this1, this2 )
    !! Datenobjekt Ergebniswert
    TYPE (t_att) , INTENT(INOUT) :: this1 ! 
    !! Datenobjekt Zuweisungswert
    TYPE (t_att) , INTENT(IN)  :: this2 !
    !! Name der Subroutine
    CHARACTER (LEN=10) , PARAMETER:: c_upname='as_att_0_0' ! 
    !
    CALL new_att ( this1 )
    IF ( no_error( ) ) THEN
       this1%id     = this2%id
       this1%name   = this2%name
       this1%var_id = this2%var_id
       IF ( get_att_nof_values(this2) > 0 ) THEN
          SELECT CASE ( get_att_type(this2) )
          CASE ( 'CH ' )
             CALL set_att_ch ( this1, this2%ch(:) )
          CASE ( 'IN ' )
             CALL set_att_in ( this1, this2%in(:) )
          CASE ( 'DP ' )
             CALL set_att_dp ( this1, this2%dp(:) )
          CASE DEFAULT
             CALL setup_error_act ( all_errors(:), 21000, c_upname, c_modname )
             CALL setup_error_act ( '<AktAttName>', get_att_name(this2) )
             CALL setup_error_act ( '<AktType>', get_att_type(this2) )
          END SELECT
       END IF
    END IF
    !
  END SUBROUTINE as_att_0_0
  !
  !! Zuweisung zwischen zwei vektoriellen Objekten des Typs "t_att" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE as_att_1_1 ( this1, this2 )
    !! Datenobjekt Ergebniswerte (Vektor)
    TYPE (t_att) , INTENT(INOUT) :: this1(:) ! 
    !! Datenobjekt Zuweisungswerte (Vektor)
    TYPE (t_att) , INTENT(IN)  :: this2(:) !
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,MIN(SIZE(this1),SIZE(this2))
       IF ( any_error( ) ) EXIT
       CALL as_att_0_0 ( this1(i), this2(i) )
    END DO
    !
  END SUBROUTINE as_att_1_1
  !
  ! ----------------------------------------------------------------------
  ! >>> sonst. PUBLIC-Methoden <<< [ERR_NO = -30000 bis -39999]
  ! ----------------------------------------------------------------------
  !
  !! Setze eine Warnung ab, falls ein Skalar (Character-String) "ch" in den 
  !! aktuellen Daten des Attributs "name" mit der (optionalen) Variablen-Id 
  !! "varid" nicht vorhanden ist <BR>
  !! Gro&szlig;- und Kleinschreibung werden nicht unterschieden <BR>
  !! Function erzeugt Fehlermeldungen, falls das Attribut nicht in der
  !! Objektliste vorhanden ist
  SUBROUTINE gen_warn_if_data_un_ch_1_0 ( this, ch, name, varid )
    !! Objektliste in der nach dem Attribut "name" gesucht werden soll
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Text der in dem Attribut vorhanden sein soll
    CHARACTER (LEN=*) , INTENT(IN) :: ch      ! 
    !! Name des gesuchten Attributs
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Variablen-Id des gesuchten Attributs
    INTEGER , OPTIONAL, INTENT(IN) :: varid   ! 
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER  :: c_upname='gen_warn_if_data_un_ch_1_0' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=LEN(ch))        :: l_ch(1)  ! 
    !
    l_ch(1) = ch
    IF ( PRESENT(varid) ) THEN
       CALL gen_warn_if_data_un_ch_1_1( this, l_ch, name, varid )
    ELSE
       CALL gen_warn_if_data_un_ch_1_1( this, l_ch, name        )
    END IF
    !
  END SUBROUTINE gen_warn_if_data_un_ch_1_0
  !
  !! Setze eine Warnung ab, falls nicht alle Daten eines Vektors (von Character-
  !! Strings) "ch(:)" in den aktuellen Daten des Attributs "name" mit der 
  !! (optionalen) Variablen-Id "varid" vorhanden sind <BR>
  !! Gro&szlig;- und Kleinschreibung werden nicht unterschieden <BR>
  !! Function erzeugt Fehlermeldungen, falls das Attribut nicht in der
  !! Objektliste vorhanden ist
  SUBROUTINE gen_warn_if_data_un_ch_1_1 ( this, ch, name, varid )
    !! Objektliste in der nach dem Attribut "name" gesucht werden soll
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Daten (Text) der in dem Attribut vorhanden sein soll
    CHARACTER (LEN=*) , INTENT(IN) :: ch(:)   ! 
    !! Name des gesuchten Attributs
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Variablen-Id des gesuchten Attributs
    INTEGER , OPTIONAL, INTENT(IN) :: varid   ! 
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER :: c_upname='gen_warn_if_data_un_ch_1_1' ! 
    !! Hilfsvariablen
    INTEGER :: ipos(SIZE(ch)) ! 
    INTEGER :: i             ! 
    !
    IF ( PRESENT(varid) ) THEN
       ipos(:) = get_att_data_order_ch_1_1( this, ch, name, varid )
    ELSE
       ipos(:) = get_att_data_order_ch_1_1( this, ch, name )
    END IF
    IF ( no_error( ) ) THEN
       IF ( .NOT. ALL( ipos(:) > 0 ) ) THEN
          WRITE(*,*) ' *** Warning *** '//TRIM(c_upname)
          WRITE(*,*) ' Werte (Character-Daten) nicht in Attribut-Daten vorhanden'
          WRITE(*,*) ' Name des Attributs = '//TRIM(name)
          WRITE(*,*) ' Liste der gesuchten Werte (i, ipos(i), ch(i)): '
          DO i=1,SIZE(ch)
             WRITE(*,'(A,2I6,A)') ' -> ',i,ipos(i),TRIM(ch(i))
          END DO
       END IF
    END IF
    !
  END SUBROUTINE gen_warn_if_data_un_ch_1_1
  !
  !! Setze eine Warnung ab, falls ein Skalar (Integer-Zahl) "in" in den 
  !! aktuellen Daten des Attributs "name" mit der (optionalen) Variablen-Id 
  !! "varid" nicht vorhanden ist <BR>
  !! Function erzeugt Fehlermeldungen, falls das Attribut nicht in der
  !! Objektliste vorhanden ist
  SUBROUTINE gen_warn_if_data_un_in_1_0 ( this, in, name, varid )
    !! Objektliste in der nach dem Attribut "name" gesucht werden soll
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Integer-Zahl die in dem Attribut vorhanden sein soll
    INTEGER           , INTENT(IN) :: in      ! 
    !! Name des gesuchten Attributs
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Variablen-Id des gesuchten Attributs
    INTEGER , OPTIONAL, INTENT(IN) :: varid   ! 
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER  :: c_upname='gen_warn_if_data_un_in_1_0' ! 
    !! Hilfsvariablen
    INTEGER :: l_in(1)  ! 
    !
    l_in(1) = in
    IF ( PRESENT(varid) ) THEN
       CALL gen_warn_if_data_un_in_1_1( this, l_in, name, varid )
    ELSE
       CALL gen_warn_if_data_un_in_1_1( this, l_in, name        )
    END IF
    !
  END SUBROUTINE gen_warn_if_data_un_in_1_0
  !
  !! Setze eine Warnung ab, falls nicht alle Daten eines Vektors (von 
  !! Integer-Zahlen) "in(:)" in den aktuellen Daten des Attributs "name" 
  !! mit der (optionalen) Variablen-Id "varid" vorhanden sind <BR>
  !! Function erzeugt Fehlermeldungen, falls das Attribut nicht in der
  !! Objektliste vorhanden ist
  SUBROUTINE gen_warn_if_data_un_in_1_1 ( this, in, name, varid )
    !! Objektliste in der nach dem Attribut "name" gesucht werden soll
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Integer-Zahlen die in dem Attribut vorhanden sein sollen
    INTEGER           , INTENT(IN) :: in(:)   ! 
    !! Name des gesuchten Attributs
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Variablen-Id des gesuchten Attributs
    INTEGER , OPTIONAL, INTENT(IN) :: varid   ! 
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER :: c_upname='gen_warn_if_data_un_in_1_1' ! 
    !! Hilfsvariablen
    INTEGER :: ipos(SIZE(in)) ! 
    INTEGER :: i             ! 
    !
    IF ( PRESENT(varid) ) THEN
       ipos(:) = get_att_data_order_in_1_1( this, in, name, varid )
    ELSE
       ipos(:) = get_att_data_order_in_1_1( this, in, name )
    END IF
    IF ( no_error( ) ) THEN
       IF ( .NOT. ALL( ipos(:) > 0 ) ) THEN
          WRITE(*,*) ' *** Warning *** '//TRIM(c_upname)
          WRITE(*,*) ' Werte (Integer-Zahlen) nicht in Attribut-Daten vorhanden'
          WRITE(*,*) ' Name des Attributs = '//TRIM(name)
          WRITE(*,*) ' Liste der gesuchten Werte (i, ipos(i), in(i)): '
          DO i=1,SIZE(in)
             WRITE(*,'(A,2I6,I10)') ' -> ',i,ipos(i),in(i)
          END DO
       END IF
    END IF
    !
  END SUBROUTINE gen_warn_if_data_un_in_1_1
  !
  !! Setze eine Warnung ab, falls ein Skalar (Double-Zahl) "dp" in den 
  !! aktuellen Daten des Attributs "name" mit der (optionalen) Variablen-Id 
  !! "varid" nicht vorhanden ist <BR>
  !! Function erzeugt Fehlermeldungen, falls das Attribut nicht in der
  !! Objektliste vorhanden ist
  SUBROUTINE gen_warn_if_data_un_dp_1_0 ( this, dp, name, varid )
    !! Objektliste in der nach dem Attribut "name" gesucht werden soll
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Double-Zahl die in dem Attribut vorhanden sein soll
    REAL (KIND=Double), INTENT(IN) :: dp      ! 
    !! Name des gesuchten Attributs
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Variablen-Id des gesuchten Attributs
    INTEGER , OPTIONAL, INTENT(IN) :: varid   ! 
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER  :: c_upname='gen_warn_if_data_un_dp_1_0' ! 
    !! Hilfsvariablen
    REAL (KIND=Double) :: l_dp(1)  ! 
    !
    l_dp(1) = dp
    IF ( PRESENT(varid) ) THEN
       CALL gen_warn_if_data_un_dp_1_1( this, l_dp, name, varid )
    ELSE
       CALL gen_warn_if_data_un_dp_1_1( this, l_dp, name        )
    END IF
    !
  END SUBROUTINE gen_warn_if_data_un_dp_1_0
  !
  !! Setze eine Warnung ab, falls nicht alle Daten eines Vektors (von 
  !! Double-Zahlen) "dp(:)" in den aktuellen Daten des Attributs "name" 
  !! mit der (optionalen) Variablen-Id "varid" vorhanden sind <BR>
  !! Function erzeugt Fehlermeldungen, falls das Attribut nicht in der
  !! Objektliste vorhanden ist
  SUBROUTINE gen_warn_if_data_un_dp_1_1 ( this, dp, name, varid )
    !! Objektliste in der nach dem Attribut "name" gesucht werden soll
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Double-Zahlen die in dem Attribut vorhanden sein sollen
    REAL (KIND=Double), INTENT(IN) :: dp(:)   ! 
    !! Name des gesuchten Attributs
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Variablen-Id des gesuchten Attributs
    INTEGER , OPTIONAL, INTENT(IN) :: varid   ! 
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER :: c_upname='gen_warn_if_data_un_dp_1_1' ! 
    !! Hilfsvariablen
    INTEGER :: ipos(SIZE(dp)) ! 
    INTEGER :: i             ! 
    !
    IF ( PRESENT(varid) ) THEN
       ipos(:) = get_att_data_order_dp_1_1( this, dp, name, varid )
    ELSE
       ipos(:) = get_att_data_order_dp_1_1( this, dp, name )
    END IF
    IF ( no_error( ) ) THEN
       IF ( .NOT. ALL( ipos(:) > 0 ) ) THEN
          WRITE(*,*) ' *** Warning *** '//TRIM(c_upname)
          WRITE(*,*) ' Werte (Double-Zahlen) nicht in Attribut-Daten vorhanden'
          WRITE(*,*) ' Name des Attributs = '//TRIM(name)
          WRITE(*,*) ' Liste der gesuchten Werte (i, ipos(i), dp(i)): '
          DO i=1,SIZE(dp)
             WRITE(*,'(A,2I6,G15.9)') ' -> ',i,ipos(i),dp(i)
          END DO
       END IF
    END IF
    !
  END SUBROUTINE gen_warn_if_data_un_dp_1_1
  !
  !! Setze eine Warnung ab, falls ein Skalar (Datum+Zeit) "ti" in den 
  !! aktuellen Daten des Attributs "name" mit der (optionalen) Variablen-Id 
  !! "varid" nicht vorhanden ist <BR>
  !! Function erzeugt Fehlermeldungen, falls das Attribut nicht in der
  !! Objektliste vorhanden ist
  SUBROUTINE gen_warn_if_data_un_ti_1_0 ( this, ti, name, varid )
    !! Objektliste in der nach dem Attribut "name" gesucht werden soll
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Datum+Zeit die in dem Attribut vorhanden sein soll
    TYPE (t_datetime) , INTENT(IN) :: ti      ! 
    !! Name des gesuchten Attributs
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Variablen-Id des gesuchten Attributs
    INTEGER , OPTIONAL, INTENT(IN) :: varid   ! 
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER  :: c_upname='gen_warn_if_data_un_ti_1_0' ! 
    !! Hilfsvariablen
    TYPE (t_datetime) :: l_ti(1)  ! 
    !
    l_ti(1) = ti
    IF ( PRESENT(varid) ) THEN
       CALL gen_warn_if_data_un_ti_1_1( this, l_ti, name, varid )
    ELSE
       CALL gen_warn_if_data_un_ti_1_1( this, l_ti, name        )
    END IF
    !
  END SUBROUTINE gen_warn_if_data_un_ti_1_0
  !
  !! Setze eine Warnung ab, falls nicht alle Daten eines Vektors (von 
  !! Datums- und Zeitangaben) "ti(:)" in den aktuellen Daten des Attributs "name" 
  !! mit der (optionalen) Variablen-Id "varid" vorhanden sind <BR>
  !! Function erzeugt Fehlermeldungen, falls das Attribut nicht in der
  !! Objektliste vorhanden ist
  SUBROUTINE gen_warn_if_data_un_ti_1_1 ( this, ti, name, varid )
    !! Objektliste in der nach dem Attribut "name" gesucht werden soll
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Datums- und Zeitangaben die in dem Attribut vorhanden sein sollen
    TYPE (t_datetime) , INTENT(IN) :: ti(:)   ! 
    !! Name des gesuchten Attributs
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Variablen-Id des gesuchten Attributs
    INTEGER , OPTIONAL, INTENT(IN) :: varid   ! 
    !! Name der Subroutine
    CHARACTER (LEN=26), PARAMETER :: c_upname='gen_warn_if_data_un_ti_1_1' ! 
    !! Hilfsvariablen
    INTEGER :: ipos(SIZE(ti)) ! 
    INTEGER :: i             ! 
    !
    IF ( PRESENT(varid) ) THEN
       ipos(:) = get_att_data_order_ti_1_1( this, ti, name, varid )
    ELSE
       ipos(:) = get_att_data_order_ti_1_1( this, ti, name )
    END IF
    IF ( no_error( ) ) THEN
       IF ( .NOT. ALL( ipos(:) > 0 ) ) THEN
          WRITE(*,*) ' *** Warning *** '//TRIM(c_upname)
          WRITE(*,*) ' Werte (Datum+Zeit) nicht in Attribut-Daten vorhanden'
          WRITE(*,*) ' Name des Attributs = '//TRIM(name)
          WRITE(*,*) ' Liste der gesuchten Werte (i, ipos(i), ti(i)): '
          DO i=1,SIZE(ti)
             WRITE(*,'(A,2I6,A)') ' -> ',i,ipos(i),datetime_to_string(ti(i))
          END DO
       END IF
    END IF
    !
  END SUBROUTINE gen_warn_if_data_un_ti_1_1
  !
  ! ------------------------------------------------------------------------------
  ! ------------------------------------------------------------------------------
  !
  !! Setze eine Fehlermeldung ab, falls ein Skalar (Character-String) "ch" in den 
  !! aktuellen Daten des Attributs "name" mit der (optionalen) Variablen-Id 
  !! "varid" nicht vorhanden ist <BR>
  !! Gro&szlig;- und Kleinschreibung werden nicht unterschieden <BR>
  !! Function erzeugt Fehlermeldungen
  SUBROUTINE gen_error_if_data_un_ch_1_0 ( this, ch, name, varid )
    !! Objektliste in der nach dem Attribut "name" gesucht werden soll
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Text der in dem Attribut vorhanden sein soll
    CHARACTER (LEN=*) , INTENT(IN) :: ch      ! 
    !! Name des gesuchten Attributs
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Variablen-Id des gesuchten Attributs
    INTEGER , OPTIONAL, INTENT(IN) :: varid   ! 
    !! Name der Subroutine
    CHARACTER (LEN=27), PARAMETER  :: c_upname='gen_error_if_data_un_ch_1_0' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=LEN(ch))        :: l_ch(1)  ! 
    !
    l_ch(1) = ch
    IF ( PRESENT(varid) ) THEN
       CALL gen_error_if_data_un_ch_1_1( this, l_ch, name, varid )
    ELSE
       CALL gen_error_if_data_un_ch_1_1( this, l_ch, name        )
    END IF
    !
  END SUBROUTINE gen_error_if_data_un_ch_1_0
  !
  !! Erzeuge eine Fehlermeldung, falls nicht alle Daten eines Vektors (von Character-
  !! Strings) "ch(:)" in den aktuellen Daten des Attributs "name" mit der 
  !! (optionalen) Variablen-Id "varid" vorhanden sind <BR>
  !! Gro&szlig;- und Kleinschreibung werden nicht unterschieden <BR>
  !! Function erzeugt Fehlermeldungen
  SUBROUTINE gen_error_if_data_un_ch_1_1 ( this, ch, name, varid )
    !! Objektliste in der nach dem Attribut "name" gesucht werden soll
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Daten (Text) der in dem Attribut vorhanden sein soll
    CHARACTER (LEN=*) , INTENT(IN) :: ch(:)   ! 
    !! Name des gesuchten Attributs
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Variablen-Id des gesuchten Attributs
    INTEGER , OPTIONAL, INTENT(IN) :: varid   ! 
    !! Name der Subroutine
    CHARACTER (LEN=27), PARAMETER :: c_upname='gen_error_if_data_un_ch_1_1' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ctxt           ! 
    INTEGER            :: ipos(SIZE(ch)) ! 
    INTEGER            :: idx            ! 
    INTEGER            :: mipos          ! 
    !
    IF ( PRESENT(varid) ) THEN
       ipos(:) = get_att_data_order_ch_1_1( this, ch, name, varid )
       idx     = get_att_idx( this, name, varid )
    ELSE
       ipos(:) = get_att_data_order_ch_1_1( this, ch, name )
       idx     = get_att_idx( this, name )
    END IF
    IF ( no_error( ) ) THEN
       IF ( .NOT. ALL( ipos(:) > 0 ) ) THEN
          CALL setup_error_act ( all_errors(:), -30002, c_upname, c_modname )
          CALL setup_error_act ( '<AktAttName>', TRIM(name) )
          WRITE(ctxt,'(I10)') varid 
          CALL setup_error_act ( '<AktAttId>', ctxt )
          WRITE(ctxt,'(I10)') get_att_nof_values(this(idx))
          CALL setup_error_act ( '<NofAttData>', ctxt )
          WRITE(ctxt,'(I10)') SIZE(ch)
          CALL setup_error_act ( '<NofUserData>', ctxt )
          WRITE(ctxt,'(I10)') COUNT( ipos(:) > 0 )
          CALL setup_error_act ( '<ActFoundData>', ctxt )
          WRITE(ctxt,'(I10)') COUNT( ipos(:) <= 0 )
          CALL setup_error_act ( '<ActNotFoundData>', ctxt )
          mipos = MINVAL(MINLOC( ipos(:), ipos(:) <= 0 ))
          WRITE(ctxt,'(I10)') mipos
          CALL setup_error_act ( '<FirstNotFoundUserDataPos>', ctxt )
          CALL setup_error_act ( '<FirstNotFoundUserData>', TRIM(ch(mipos)) )
       END IF
    END IF
    !
  END SUBROUTINE gen_error_if_data_un_ch_1_1
  !
  !! Setze eine Fehlermeldung ab, falls ein Skalar (Integer-Zahl) "in" in den 
  !! aktuellen Daten des Attributs "name" mit der (optionalen) Variablen-Id 
  !! "varid" nicht vorhanden ist <BR>
  !! Function erzeugt Fehlermeldungen
  SUBROUTINE gen_error_if_data_un_in_1_0 ( this, in, name, varid )
    !! Objektliste in der nach dem Attribut "name" gesucht werden soll
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Integer-Zahl die in den Daten des Attributs vorhanden sein soll
    INTEGER           , INTENT(IN) :: in      ! 
    !! Name des gesuchten Attributs
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Variablen-Id des gesuchten Attributs
    INTEGER , OPTIONAL, INTENT(IN) :: varid   ! 
    !! Name der Subroutine
    CHARACTER (LEN=27), PARAMETER  :: c_upname='gen_error_if_data_un_in_1_0' ! 
    !! Hilfsvariablen
    INTEGER                        :: l_in(1)  ! 
    !
    l_in(1) = in
    IF ( PRESENT(varid) ) THEN
       CALL gen_error_if_data_un_in_1_1( this, l_in, name, varid )
    ELSE
       CALL gen_error_if_data_un_in_1_1( this, l_in, name        )
    END IF
    !
  END SUBROUTINE gen_error_if_data_un_in_1_0
  !
  !! Erzeuge eine Fehlermeldung, falls nicht alle Daten eines Vektors 
  !! (von Integer-Zahlen) "in(:)" in den aktuellen Daten des Attributs 
  !! "name" mit der (optionalen) Variablen-Id "varid" vorhanden sind <BR>
  !! Function erzeugt Fehlermeldungen
  SUBROUTINE gen_error_if_data_un_in_1_1 ( this, in, name, varid )
    !! Objektliste in der nach dem Attribut "name" gesucht werden soll
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Integer-Zahlen die in dem Attribut vorhanden sein sollen
    INTEGER           , INTENT(IN) :: in(:)   ! 
    !! Name des gesuchten Attributs
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Variablen-Id des gesuchten Attributs
    INTEGER , OPTIONAL, INTENT(IN) :: varid   ! 
    !! Name der Subroutine
    CHARACTER (LEN=27), PARAMETER :: c_upname='gen_error_if_data_un_in_1_1' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ctxt           ! 
    INTEGER            :: ipos(SIZE(in)) ! 
    INTEGER            :: mipos          ! 
    INTEGER            :: idx            ! 
    !
    IF ( PRESENT(varid) ) THEN
       ipos(:) = get_att_data_order_in_1_1( this, in, name, varid )
       idx     = get_att_idx( this, name, varid )
    ELSE
       ipos(:) = get_att_data_order_in_1_1( this, in, name )
       idx     = get_att_idx( this, name        )
    END IF
    IF ( no_error( ) ) THEN
       IF ( .NOT. ALL( ipos(:) > 0 ) ) THEN
          CALL setup_error_act ( all_errors(:), -30002, c_upname, c_modname )
          CALL setup_error_act ( '<AktAttName>', TRIM(name) )
          WRITE(ctxt,'(I10)') varid 
          CALL setup_error_act ( '<AktAttId>', ctxt )
          WRITE(ctxt,'(I10)') get_att_nof_values(this(idx))
          CALL setup_error_act ( '<NofAttData>', ctxt )
          WRITE(ctxt,'(I10)') SIZE(in)
          CALL setup_error_act ( '<NofUserData>', ctxt )
          WRITE(ctxt,'(I10)') COUNT( ipos(:) > 0 )
          CALL setup_error_act ( '<ActFoundData>', ctxt )
          WRITE(ctxt,'(I10)') COUNT( ipos(:) <= 0 )
          CALL setup_error_act ( '<ActNotFoundData>', ctxt )
          mipos = MINVAL(MINLOC( ipos(:), ipos(:) <= 0 ))
          WRITE(ctxt,'(I10)') mipos
          CALL setup_error_act ( '<FirstNotFoundUserDataPos>', ctxt )
          WRITE(ctxt,'(I10)') in(mipos)
          CALL setup_error_act ( '<FirstNotFoundUserData>', ctxt )
       END IF
    END IF
    !
  END SUBROUTINE gen_error_if_data_un_in_1_1
  !
  !! Setze eine Fehlermeldung ab, falls ein Skalar (Double-Zahl) "in" in den 
  !! aktuellen Daten des Attributs "name" mit der (optionalen) Variablen-Id 
  !! "varid" nicht vorhanden ist <BR>
  !! Function erzeugt Fehlermeldungen
  SUBROUTINE gen_error_if_data_un_dp_1_0 ( this, dp, name, varid )
    !! Objektliste in der nach dem Attribut "name" gesucht werden soll
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Double-Zahl die in den Daten des Attributs vorhanden sein soll
    REAL (KIND=Double), INTENT(IN) :: dp      ! 
    !! Name des gesuchten Attributs
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Variablen-Id des gesuchten Attributs
    INTEGER , OPTIONAL, INTENT(IN) :: varid   ! 
    !! Name der Subroutine
    CHARACTER (LEN=27), PARAMETER  :: c_upname='gen_error_if_data_un_dp_1_0' ! 
    !! Hilfsvariablen
    REAL (KIND=Double)             :: l_dp(1)  ! 
    !
    l_dp(1) = dp
    IF ( PRESENT(varid) ) THEN
       CALL gen_error_if_data_un_dp_1_1( this, l_dp, name, varid )
    ELSE
       CALL gen_error_if_data_un_dp_1_1( this, l_dp, name        )
    END IF
    !
  END SUBROUTINE gen_error_if_data_un_dp_1_0
  !
  !! Erzeuge eine Fehlermeldung, falls nicht alle Daten eines Vektors 
  !! (von Double-Zahlen) "dp(:)" in den aktuellen Daten des Attributs 
  !! "name" mit der (optionalen) Variablen-Id "varid" vorhanden sind <BR>
  !! Function erzeugt Fehlermeldungen
  SUBROUTINE gen_error_if_data_un_dp_1_1 ( this, dp, name, varid )
    !! Objektliste in der nach dem Attribut "name" gesucht werden soll
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Double-Zahlen die in dem Attribut vorhanden sein sollen
    REAL (KIND=Double), INTENT(IN) :: dp(:)   ! 
    !! Name des gesuchten Attributs
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Variablen-Id des gesuchten Attributs
    INTEGER , OPTIONAL, INTENT(IN) :: varid   ! 
    !! Name der Subroutine
    CHARACTER (LEN=27), PARAMETER :: c_upname='gen_error_if_data_un_dp_1_1' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ctxt           ! 
    CHARACTER (LEN=15) :: rtxt           ! 
    INTEGER            :: ipos(SIZE(dp)) ! 
    INTEGER            :: mipos          ! 
    INTEGER            :: idx            ! 
    !
    IF ( PRESENT(varid) ) THEN
       ipos(:) = get_att_data_order_dp_1_1( this, dp, name, varid )
       idx     = get_att_idx( this, name, varid )
    ELSE
       ipos(:) = get_att_data_order_dp_1_1( this, dp, name )
       idx     = get_att_idx( this, name        )
    END IF
    IF ( no_error( ) ) THEN
       IF ( .NOT. ALL( ipos(:) > 0 ) ) THEN
          CALL setup_error_act ( all_errors(:), -30002, c_upname, c_modname )
          CALL setup_error_act ( '<AktAttName>', TRIM(name) )
          WRITE(ctxt,'(I10)') varid 
          CALL setup_error_act ( '<AktAttId>', ctxt )
          WRITE(ctxt,'(I10)') get_att_nof_values(this(idx))
          CALL setup_error_act ( '<NofAttData>', ctxt )
          WRITE(ctxt,'(I10)') SIZE(dp)
          CALL setup_error_act ( '<NofUserData>', ctxt )
          WRITE(ctxt,'(I10)') COUNT( ipos(:) > 0 )
          CALL setup_error_act ( '<ActFoundData>', ctxt )
          WRITE(ctxt,'(I10)') COUNT( ipos(:) <= 0 )
          CALL setup_error_act ( '<ActNotFoundData>', ctxt )
          mipos = MINVAL(MINLOC( ipos(:), ipos(:) <= 0 ))
          WRITE(ctxt,'(I10)') mipos
          CALL setup_error_act ( '<FirstNotFoundUserDataPos>', ctxt )
          WRITE(rtxt,'(G15.9)') dp(mipos)
          CALL setup_error_act ( '<FirstNotFoundUserData>', rtxt )
       END IF
    END IF
    !
  END SUBROUTINE gen_error_if_data_un_dp_1_1
  !
  !! Setze eine Fehlermeldung ab, falls ein Skalar (Datum+Zeit) "ti" in den 
  !! aktuellen Daten des Attributs "name" mit der (optionalen) Variablen-Id 
  !! "varid" nicht vorhanden ist <BR>
  !! Function erzeugt Fehlermeldungen
  SUBROUTINE gen_error_if_data_un_ti_1_0 ( this, ti, name, varid )
    !! Objektliste in der nach dem Attribut "name" gesucht werden soll
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Datum+Zeit die in den Daten des Attributs vorhanden sein soll
    TYPE (t_datetime) , INTENT(IN) :: ti      ! 
    !! Name des gesuchten Attributs
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Variablen-Id des gesuchten Attributs
    INTEGER , OPTIONAL, INTENT(IN) :: varid   ! 
    !! Name der Subroutine
    CHARACTER (LEN=27), PARAMETER  :: c_upname='gen_error_if_data_un_ti_1_0' ! 
    !! Hilfsvariablen
    TYPE (t_datetime)              :: l_ti(1)  ! 
    !
    l_ti(1) = ti
    IF ( PRESENT(varid) ) THEN
       CALL gen_error_if_data_un_ti_1_1( this, l_ti, name, varid )
    ELSE
       CALL gen_error_if_data_un_ti_1_1( this, l_ti, name        )
    END IF
    !
  END SUBROUTINE gen_error_if_data_un_ti_1_0
  !
  !! Erzeuge eine Fehlermeldung, falls nicht alle Daten eines Vektors 
  !! (mit Datums- und Zeitangaben) "ti(:)" in den aktuellen Daten des Attributs 
  !! "name" mit der (optionalen) Variablen-Id "varid" vorhanden sind <BR>
  !! Function erzeugt Fehlermeldungen
  SUBROUTINE gen_error_if_data_un_ti_1_1 ( this, ti, name, varid )
    !! Objektliste in der nach dem Attribut "name" gesucht werden soll
    TYPE (t_att)      , INTENT(IN) :: this(:) ! 
    !! Datums- und Zeitangaben die in dem Attribut vorhanden sein sollen
    TYPE (t_datetime) , INTENT(IN) :: ti(:)   ! 
    !! Name des gesuchten Attributs
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Variablen-Id des gesuchten Attributs
    INTEGER , OPTIONAL, INTENT(IN) :: varid   ! 
    !! Name der Subroutine
    CHARACTER (LEN=27), PARAMETER :: c_upname='gen_error_if_data_un_ti_1_1' ! 
    !! Hilfsvariablen
    CHARACTER (LEN=10) :: ctxt           ! 
    INTEGER            :: ipos(SIZE(ti)) ! 
    INTEGER            :: mipos          ! 
    INTEGER            :: idx            ! 
    !
    IF ( PRESENT(varid) ) THEN
       ipos(:) = get_att_data_order_ti_1_1( this, ti, name, varid )
       idx     = get_att_idx( this, name, varid )
    ELSE
       ipos(:) = get_att_data_order_ti_1_1( this, ti, name )
       idx     = get_att_idx( this, name        )
    END IF
    IF ( no_error( ) ) THEN
       IF ( .NOT. ALL( ipos(:) > 0 ) ) THEN
          CALL setup_error_act ( all_errors(:), -30002, c_upname, c_modname )
          CALL setup_error_act ( '<AktAttName>', TRIM(name) )
          WRITE(ctxt,'(I10)') varid 
          CALL setup_error_act ( '<AktAttId>', ctxt )
          WRITE(ctxt,'(I10)') get_att_nof_values(this(idx))
          CALL setup_error_act ( '<NofAttData>', ctxt )
          WRITE(ctxt,'(I10)') SIZE(ti)
          CALL setup_error_act ( '<NofUserData>', ctxt )
          WRITE(ctxt,'(I10)') COUNT( ipos(:) > 0 )
          CALL setup_error_act ( '<ActFoundData>', ctxt )
          WRITE(ctxt,'(I10)') COUNT( ipos(:) <= 0 )
          CALL setup_error_act ( '<ActNotFoundData>', ctxt )
          mipos = MINVAL(MINLOC( ipos(:), ipos(:) <= 0 ))
          WRITE(ctxt,'(I10)') mipos
          CALL setup_error_act ( '<FirstNotFoundUserDataPos>', ctxt )
          CALL setup_error_act ( '<FirstNotFoundUserData>', datetime_to_string(ti(mipos)) )
       END IF
    END IF
    !
  END SUBROUTINE gen_error_if_data_un_ti_1_1
  !
  ! ----------------------------------------------------------------------------
  ! ----------------------------------------------------------------------------
  !
  !! erzeuge eine Fehlermeldung falls ein Attribut mit dem Namen
  !! "name" nicht in einer Liste von Attributen vorhanden ist <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE gen_error_if_att_not_exists_0 ( this, name )
    !! Datenobjekt (Liste aller Attribute)
    TYPE (t_att)      , INTENT(IN) :: this(:) !
    !! Name des gesuchten Attributs
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Name der Subroutine
    CHARACTER (LEN=29) , PARAMETER :: c_upname='gen_error_if_att_not_exists_0' ! 
    !
    IF ( .NOT. att_exists( this(:), name ) ) THEN
       CALL setup_error_act ( all_errors(:), -30000, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(name) )
    END IF
    !
  END SUBROUTINE gen_error_if_att_not_exists_0
  !
  !! erzeuge eine Fehlermeldung falls mehrere Attribute mit den Namen
  !! "name(:)" nicht in einer Liste von Attributen vorhanden sind <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE gen_error_if_att_not_exists_1 ( this, name )
    !! Datenobjekt (Liste aller Attribute)
    TYPE (t_att)      , INTENT(IN) :: this(:) !
    !! Namen der gesuchten Attribute
    CHARACTER (LEN=*) , INTENT(IN) :: name(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=29) , PARAMETER :: c_upname='gen_error_if_att_not_exists_1' ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(name)
       CALL gen_error_if_att_not_exists(this(:),name(i))
    END DO
    !
  END SUBROUTINE gen_error_if_att_not_exists_1
  !
  !! erzeuge eine Fehlermeldung falls ein Attribut mit dem Namen
  !! "name" f&uuml;r eine Variable "var_id" nicht in einer Liste 
  !! von Attributen vorhanden ist <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE gen_error_if_att_not_exists_0_v ( this, name, var_id )
    !! Datenobjekt (Liste aller Attribute)
    TYPE (t_att)      , INTENT(IN) :: this(:) !
    !! Name des gesuchten Attributs
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! Id der Variablen f&uuml;r die das Attribut vorhanden sein soll
    INTEGER           , INTENT(IN) :: var_id  ! 
    !! Name der Subroutine
    CHARACTER (LEN=31) , PARAMETER :: c_upname='gen_error_if_att_not_exists_0_v' ! 
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !
    IF ( .NOT. att_exists( this(:), name, var_id ) ) THEN
       CALL setup_error_act ( all_errors(:), -30001, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(name) )
       WRITE(ctxt,'(I10)') var_id
       CALL setup_error_act ( '<AktAttId>', ctxt )
    END IF
    !
  END SUBROUTINE gen_error_if_att_not_exists_0_v
  !
  !! erzeuge eine Fehlermeldung falls mehrere Attribute mit den Namen
  !! "name(:)" f&uuml;r eine Variable "var_id" nicht in einer Liste 
  !! von Attributen vorhanden sind <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE gen_error_if_att_not_exists_1_v ( this, name, var_id )
    !! Datenobjekt (Liste aller Attribute)
    TYPE (t_att)      , INTENT(IN) :: this(:) !
    !! Namen der gesuchten Attribute
    CHARACTER (LEN=*) , INTENT(IN) :: name(:) ! 
    !! Id der Variablen f&uuml;r die die Attribute vorhanden sein sollen
    INTEGER           , INTENT(IN) :: var_id  ! 
    !! Name der Subroutine
    CHARACTER (LEN=31) , PARAMETER :: c_upname='gen_error_if_att_not_exists_1_v' ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(name)
       CALL gen_error_if_att_not_exists_0_v(this(:),name(i),var_id)
    END DO
    !
  END SUBROUTINE gen_error_if_att_not_exists_1_v
  !
  !! erzeuge eine Fehlermeldung falls mehrere Attribute mit den Namen
  !! "name(:)" f&uuml;r mehrere Variablen "var_id(:)" nicht in einer 
  !! Liste von Attributen vorhanden sind <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE gen_error_if_att_not_exists_1_1 ( this, name, var_id )
    !! Datenobjekt (Liste aller Attribute)
    TYPE (t_att)      , INTENT(IN) :: this(:)   !
    !! Namen der gesuchten Attribute
    CHARACTER (LEN=*) , INTENT(IN) :: name(:)   ! 
    !! Id's der Variablen f&uuml;r die die Attribute vorhanden sein sollen
    INTEGER           , INTENT(IN) :: var_id(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=31) , PARAMETER :: c_upname='gen_error_if_att_not_exists_1_1' ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(var_id)
       CALL gen_error_if_att_not_exists_1_v(this(:),name(:),var_id(i))
    END DO
    !
  END SUBROUTINE gen_error_if_att_not_exists_1_1
  !
  !! pr&uuml;fe den Inhalt eines Attributes <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_att_contents_0 ( this ) &
         RESULT( ok )
    !! Objekt  (Skalar)
    TYPE (t_att)      , INTENT(IN) :: this           ! 
    !! R&uuml;ckgabewert : Testergebnis (Skalar)
    LOGICAL :: ok  ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    ok = .false.
    idx = get_att_name_idx ( this )
    SELECT CASE ( idx )
    CASE (   1 )
       ok = ok_title(this)
    CASE (   2 )
       ok = ok_history(this)
    CASE (   3 )
       ok = ok_beginning_date(this)
    CASE (   4 )
       ok = ok_ending_date(this)
    CASE (   5 )
       ok = ok_reference_date(this)
    CASE (   6 )
       ok = ok_single_date(this)
    CASE (   7 )
       ok = ok_multiple_dates(this)
    CASE (   8 )
       ok = ok_hor_coord_system_def(this)
    CASE (   9 )
       ok = ok_map_projection(this)
    CASE (  10 )
       ok = ok_horizontal_datum_name(this)
    CASE (  11 )
       ok = ok_ellipsoid_name(this)
    CASE (  12 )
       ok = ok_longitude_of_cen_meri(this)
    CASE (  13 )
       ok = ok_false_easting(this)
    CASE (  14 )
       ok = ok_false_northing(this)
    CASE (  15 )
       ok = ok_west_bounding_coord(this)
    CASE (  16 )
       ok = ok_east_bounding_coord(this)
    CASE (  17 )
       ok = ok_north_bounding_coord(this)
    CASE (  18 )
       ok = ok_south_bounding_coord(this)
    CASE (  19 )
       ok = ok_vert_coord_system_def(this)
    CASE (  20 )
       ok = ok_altitude_datum_name(this)
    CASE (  21 )
       ok = ok_depth_datum_name(this)
    CASE (  22 )
       ok = ok_format_name(this)
    CASE (  23 )
       ok = ok_format_version_no(this)
    CASE (  24 )
       ok = ok_theme_keywords(this)
    CASE (  25 )
       ok = ok_place_keywords(this)
    CASE (  26 )
       ok = ok_temporal_keywords(this)
    CASE (  27 )
       ok = ok_related_file_name(this)
    CASE (  28 )
       ok = ok_related_file_form(this)
    CASE (  29 )
       ok = ok_related_file_access(this)
    CASE (  30 )
       ok = ok_related_file_type(this)
    CASE (  31 )
       ok = ok_reference_location_index(this)
    CASE (  32 )
       ok = ok_reference_location_coord(this)
    CASE (  33 )
       ok = ok_zero_phase_location_index(this)
    CASE (  34 )
       ok = ok_zero_phase_location_coord(this)
    CASE (  35 )
       ok = ok_sigma_interfaces(this)
    CASE (  36 )
       ok = ok_layer_interfaces(this)
    CASE (  37 )
       ok = ok_minimum_water_depth(this)
    CASE (  57 )
       ok = ok_dynamic_bathymetry(this)
    CASE (  38 )
       ok = ok_long_name(this)
    CASE (  39 )
       ok = ok_short_name(this)
    CASE (  40 )
       ok = ok_units(this)
    CASE (  41 )
       ok = ok_name_id(this)
    CASE (  42 )
       ok = ok_time_id(this)
    CASE (  43 )
       ok = ok_class_id(this)
    CASE (  44 )
       ok = ok_plot_id(this)
    CASE (  45 )
       ok = ok_ref_name_id(this)
    CASE (  46 )
       ok = ok_scale_factor(this)
    CASE (  47 )
       ok = ok_valid_range(this)
    CASE (  48 )
       ok = ok_actual_range(this)
    CASE (  49 )
       ok = ok_missing_value(this)
    CASE (  50 )
       ok = ok_add_offset(this)
    CASE (  51 )
       ok = ok__FillValue(this)
    CASE (  52 )
       ok = ok_FORTRAN_format(this)
    CASE (  53 )
       ok = ok_horizontal_discret(this)
    CASE (  54 )
       ok = ok_vertical_discret(this)
    CASE (  55 )
       ok = ok_storage_type(this)
    CASE (  56 )
       ok = ok_class_names(this)
    CASE (  58 )
       ok = ok_settling_velocity(this)
    CASE (  59 )
       ok = ok_settling_velo_form(this)
    CASE (  60 )
       ok = ok_directional_sector(this)
    CASE (  61 )
       ok = ok_cross_sectional_average(this)
    CASE (  62 )
       ok = ok_data_interval(this)
    CASE (  63 )
       ok = ok_high_water_mark(this)
    CASE (  64 )
       ok = ok_low_water_mark(this)
    CASE (  65 )
       ok = ok_high_tracer_mark(this)
    CASE (  66 )
       ok = ok_low_tracer_mark(this)
    CASE (  67 )
       ok = ok_grid_mapping_name(this)
    CASE (  68 )
       ok = ok_grid_north_pole_latitude(this)
    CASE (  69 )
       ok = ok_grid_north_pole_longitude(this)
    CASE (  70 )
       ok = ok_latitude_of_proj_origin(this)
    CASE (  71 )
       ok = ok_longitude_of_proj_origin(this)
    CASE (  72 )
       ok = ok_north_pole_grid_longitude(this)
    CASE (  73 )
       ok = ok_scale_factor_at_c_meridian(this)
    CASE (  74 )
       ok = ok_scale_factor_at_p_origin(this)
    CASE (  75 )
       ok = ok_standard_parallel(this)
    CASE (  76 )
       ok = ok_straight_v_l_from_pole(this)
    CASE (  77 )
       ok = ok_ancillary_variables(this)
    CASE (  78 )
       ok = ok_axis(this)
    CASE (  79 )
       ok = ok_bounds(this)
    CASE (  80 )
       ok = ok_calendar(this)
    CASE (  81 )
       ok = ok_cell_measures(this)
    CASE (  82 )
       ok = ok_cell_methods(this)
    CASE (  83 )
       ok = ok_climatology(this)
    CASE (  84 )
       ok = ok_comment(this)
    CASE (  85 )
       ok = ok_compress(this)
    CASE (  86 )
       ok = ok_Conventions(this)
    CASE (  87 )
       ok = ok_coordinates(this)
    CASE (  88 )
       ok = ok_flag_meanings(this)
    CASE (  89 )
       ok = ok_flag_values(this)
    CASE (  90 )
       ok = ok_formula_terms(this)
    CASE (  91 )
       ok = ok_grid_mapping(this)
    CASE (  92 )
       ok = ok_institution(this)
    CASE (  93 )
       ok = ok_leap_month(this)
    CASE (  94 )
       ok = ok_leap_year(this)
    CASE (  95 )
       ok = ok_month_lengths(this)
    CASE (  96 )
       ok = ok_positive(this)
    CASE (  97 )
       ok = ok_references(this)
    CASE (  98 )
       ok = ok_source(this)
    CASE (  99 )
       ok = ok_standard_error_multiplier(this)
    CASE ( 100 )
       ok = ok_standard_name(this)
    CASE ( 101 )
       ok = ok_valid_max(this)
    CASE ( 102 )
       ok = ok_valid_min(this)
    CASE ( 103 )
       ok = ok_astro(this)
    CASE ( 104 )
       ok = ok_grid_type(this)
    CASE ( 105 )
       ok = ok_coordinate_system(this)
       ! der folgende Fall wird von ok_att abgefangen
    CASE DEFAULT
       WRITE(*,*) '>>> '//TRIM(this%name)//' nicht bekannt, kein Test !!'
    END SELECT
    !
  END FUNCTION ok_att_contents_0
  !
  !! pr&uuml;fe den Inhalt mehrerer Attribute <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_att_contents_1 ( this ) &
         RESULT( ok )
    !! Objekt  (Vektor)
    TYPE (t_att)      , INTENT(IN) :: this(:)           ! 
    !! R&uuml;ckgabewert : Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Z&auml;hler
    INTEGER :: i
    !
    DO i = 1, SIZE(this)
       ok(i) = ok_att_contents ( this(i) )
    END DO
    !
  END FUNCTION ok_att_contents_1
  !
  !! pr&uuml;fe den Inhalt eines Attributes, falls seine Id 
  !! dem angeforderten Wert entspricht <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_att_contents_0_v ( this, var_id ) &
         RESULT( ok )
    !! Objekt1  (Skalar)
    TYPE (t_att)      , INTENT(IN) :: this          ! 
    !! Objekt2  (angeforderte Id)
    INTEGER           , INTENT(IN) :: var_id 
    !! R&uuml;ckgabewert : Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = .false.
    !
    IF ( get_att_var_id(this) == var_id ) ok = ok_att_contents_0(this)
    !
  END FUNCTION ok_att_contents_0_v
  !
  !! pr&uuml;fe den Inhalt mehrerer Attribute, falls deren Id's 
  !! dem (gleichen) angeforderten Wert entsprechen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_att_contents_1_v ( this, var_id ) &
         RESULT( ok )
    !! Objekt1  (Vektor)
    TYPE (t_att)      , INTENT(IN) :: this(:)          ! 
    !! Objekt2  (angeforderte Id)
    INTEGER           , INTENT(IN) :: var_id 
    !! R&uuml;ckgabewert : Testergebnis (Skalar)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Z&auml;hler
    INTEGER :: i
    !
    ok(:) = .false.
    !
    DO i = 1, SIZE(this)
       IF ( get_att_var_id(this(i)) == var_id ) ok(i) = ok_att_contents_0(this(i))
    END DO
    !
  END FUNCTION ok_att_contents_1_v
  !
  !! pr&uuml;fe den Inhalt der Attribute auf Konsistenz untereinander <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_att_consistency_d ( this ) &
         RESULT( ok )
    !! Objekt  (Vektor) : Liste aller vorhandenen Attribute
    TYPE (t_att), INTENT(IN) :: this(:)
    !! R&uuml;ckgabewert : Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) !  wird fuer die beiden Attribute, bei denen ein Test fehlgeschlagen ist, auf .false. gesetzt
    !! Name der Subroutine
    CHARACTER (LEN=20) , PARAMETER :: c_upname='ok_att_consistency_d' ! 
    !! Hilfsvariable Datentyp datetime
    TYPE (t_datetime) , ALLOCATABLE :: ch_datetime1(:), ch_datetime2(:)
    !! Anzahl der durchzuf&uuml;hrenden Querchecks
    INTEGER, PARAMETER :: c_max_check=18 ! 
    !! Id's der Indices f&uuml;r das erste Attribut
    INTEGER, PARAMETER :: c_frst_id(c_max_check) = &
         (/ 3,  5,  5, &     !  1 -  3  beginning_date,    reference_date,           reference_date,      
            3,  6,  5, &     !  4 -  6  beginning_date,    single_date,              reference_date,      
            3,  7,  5, &     !  7 -  9  beginning_date,    multiple_dates,           reference_date,  
            6, 27, 27, &     ! 10 - 12  single_date,       related_file_name,        related_file_name,
           27, 31, 33, &     ! 13 - 15  related_file_name, reference_location_index, zero_location_index,
           35, 15, 18 /)     ! 16 - 18  sigma_interfaces,  west_bounding_coordinate, south_bounding_coordinate
    !! Id's der Indices f&uuml;r das zweite Attribut
    INTEGER, PARAMETER :: c_scnd_id(c_max_check) = &
         (/ 4,  3,  4, &     !  1 -  3  ending_date,       beginning_date,           ending_date,    
            6,  4,  6, &     !  4 -  6  single_date,       ending_date,              single_date,      
            7,  4,  7, &     !  7 -  9  multiple_dates,    ending_date,              multiple_dates,  
            7, 28, 29, &     ! 10 - 12  multiple_dates,    related_file_form,        related_file_access,
           30, 32, 34, &     ! 13 - 15  related_file_type, reference_location_coordi, zero_location_coordinates,
           36, 16, 17 /)     ! 16 - 18  layer_interfaces,  east_bounding_coordinate, north_bounding_coordinate
    !! zwei Felder, die zeigen, wo die o.g. Attribute in this(:) stehen (idx = 0 falls nicht gefunden)
    INTEGER :: frst_idx(c_max_check), scnd_idx(c_max_check)
    !! Z&auml;hlervariable Nr. des Tests
    INTEGER :: i, j, k, m_f, m_s, n, n_f, n_s, n_t ! 
    !! Hilfstext
    CHARACTER(LEN=15)  :: ctxt ! 
    !! verschiedene Parameter-Felder f&uuml;r die Tests mit den variablenbezogenen Attributen
    ! --------------------------------------------------------------------
    INTEGER , PARAMETER :: c_max_a=3                             ! 
    INTEGER , PARAMETER :: c_f_a_id(c_max_a) = (/ 64, 66, 102 /) ! 
    INTEGER , PARAMETER :: c_s_a_id(c_max_a) = (/ 63, 65, 101 /) ! 
    ! --------------------------------------------------------------------
    INTEGER , PARAMETER :: c_max_b=1                             ! 
    INTEGER , PARAMETER :: c_f_b_id(c_max_b) = (/ 49 /)          ! 
    INTEGER , PARAMETER :: c_s_b_id(c_max_b) = (/ 51 /)          ! 
    ! --------------------------------------------------------------------
    INTEGER , PARAMETER :: c_max_c=2                             ! 
    INTEGER , PARAMETER :: c_f_c_id(c_max_c) = (/  47,  47 /)    ! 
    INTEGER , PARAMETER :: c_s_c_id(c_max_c) = (/ 102, 101 /)    ! 
    ! --------------------------------------------------------------------
    INTEGER , PARAMETER :: c_max_d=2                             ! 
    INTEGER , PARAMETER :: c_f_d_id(c_max_d) = (/  47,  47 /)    ! 
    INTEGER , PARAMETER :: c_s_d_id(c_max_d) = (/  49,  51 /)    ! 
    ! --------------------------------------------------------------------
    INTEGER , PARAMETER :: c_max_e=2                             ! 
    INTEGER , PARAMETER :: c_f_e_id(c_max_e) = (/ 102, 102 /)    ! 
    INTEGER , PARAMETER :: c_s_e_id(c_max_e) = (/ 101, 101 /)    ! 
    INTEGER , PARAMETER :: c_t_e_id(c_max_e) = (/  49,  51 /)    ! 
    ! --------------------------------------------------------------------
    INTEGER , PARAMETER :: c_max_f=1                             ! 
    INTEGER , PARAMETER :: c_f_f_id(c_max_f) = (/ 88 /)          ! 
    INTEGER , PARAMETER :: c_s_f_id(c_max_f) = (/ 89 /)          ! 
    ! --------------------------------------------------------------------
    !! Hilfsfeld zur Aufnahme der Liste mit den unterschiedlichen var_id's
    INTEGER  , POINTER :: var_id(:), att_idx(:)              ! 
    REAL (KIND=Double) :: dp_f(1), dp_s(1), dp_t(1), dp_r(2) ! 
    !
    ok(:) = .true.
    !
    ! -------------------------------------------------------------------------------------------
    ! Konsistenztests fuer verschiedene globale Attribute
    ! -------------------------------------------------------------------------------------------
    !
    ! ermittle fuer alle geplanten Quer-Checks die idx-e
    DO i = 1, c_max_check
       frst_idx(i) = get_att_idx ( this(:), c_att_name(c_frst_id(i)) )
       scnd_idx(i) = get_att_idx ( this(:), c_att_name(c_scnd_id(i)) )
    END DO
    ! ... Schleife ueber alle durchzufuehrenden Querchecks
    DO i = 1, c_max_check
       ! ... sind beide Attribute c_frst_id und c_scnd_id vorhanden ?
       IF ( frst_idx(i) > 0 .AND. scnd_idx(i) > 0 ) THEN
          ! wenn ja, dann teste den Inhalt
          IF ( ok_att_contents(this(frst_idx(i))) .AND. ok_att_contents(this(scnd_idx(i))) ) THEN
             ! ... und fuehre die Querchecks durch
             SELECT CASE (i)
             CASE (1:9) 
                ! ... 1 - 9 : Datum(frst_isx(i)) <= Datum(scnd_idx(i))
                ALLOCATE( ch_datetime1(get_att_nof_values(this(frst_idx(i)))) )
                ALLOCATE( ch_datetime2(get_att_nof_values(this(scnd_idx(i)))) )
                ch_datetime1(:) = get_att_ch_as_datetime( this(frst_idx(i)) )
                ch_datetime2(:) = get_att_ch_as_datetime( this(scnd_idx(i)) )
                IF ( ALL( ch_datetime1(:) <= ch_datetime2(:) ) ) THEN
                   ok(frst_idx(i)) = .true.
                   ok(scnd_idx(i)) = .true.
                ELSE
                   ok(frst_idx(i)) = .false.
                   ok(scnd_idx(i)) = .false.
                   CALL setup_error_act ( all_errors(:), 6801, c_upname, c_modname )
                   CALL setup_error_act ( '<..AttName1..>', TRIM(this(frst_idx(i))%name) ) 
                   CALL setup_error_act ( '<..Date1..>', datetime_to_string (ch_datetime1(1)) )
                   CALL setup_error_act ( '<..AttName2..>', TRIM(this(scnd_idx(i))%name) )
                   CALL setup_error_act ( '<..Date2..>', datetime_to_string (ch_datetime2(1)) )
                END IF
                DEALLOCATE( ch_datetime1, ch_datetime2 )
             CASE (10, 16)
                ! ... 10, 16   : frst_idx(i) und scnd_idx(i) duerfen nicht gleichzeitig vorhanden sein
                ok(frst_idx(i)) = .false.
                ok(scnd_idx(i)) = .false.
                CALL setup_error_act ( all_errors(:), 6802, c_upname, c_modname )
                CALL setup_error_act ( '<..AttName1..>', TRIM(c_att_name(c_frst_id(i))) )
                WRITE(ctxt(1:10), '(I10)') frst_idx(i)
                CALL setup_error_act ( '<..idx1..>', ctxt(1:10) )

                CALL setup_error_act ( '<..AttName2..>', TRIM(c_att_name(c_scnd_id(i))) )
                WRITE(ctxt(1:10), '(I10)') scnd_idx(i)
                CALL setup_error_act ( '<..idx2..>', ctxt(1:10) )
             CASE (11:15)
                ! ... 11 - 15  : wenn frst_idx(i) vorhanden ist, muss auch scnd_idx(i) existieren
                ok(frst_idx(i)) = .true.
                ok(scnd_idx(i)) = .true.
             END SELECT
          END IF
       END IF
       SELECT CASE ( i )
       CASE ( 17,18 )
          ! 17 : west_bounding_coordinate  <= east_bounding_coordinate
          ! 18 : south_bounding_coordinate <= north_bounding_coordinate
          IF ( frst_idx(i) > 0 .AND. scnd_idx(i) > 0 ) THEN
             dp_f(:) = get_att_dp( this(frst_idx(i)) )
             dp_s(:) = get_att_dp( this(scnd_idx(i)) )
             IF ( dp_f(1) > dp_s(1) ) THEN
                CALL setup_error_act ( all_errors(:), 6617, c_upname, c_modname )
                CALL setup_error_act ( '<AktAttName1>', TRIM(c_att_name(frst_idx(i))) )
                CALL setup_error_act ( '<AktAttName2>', TRIM(c_att_name(scnd_idx(i))) )
                CALL setup_error_act ( '<op>', '<=' )
                WRITE(ctxt(1:10),'(I10)') 0 ; CALL setup_error_act ( '<VarId>', ctxt(1:10) )
                WRITE(ctxt,'(G15.6)') dp_f(1) ; CALL setup_error_act ( '<wert-1>', ctxt )
                WRITE(ctxt,'(G15.6)') dp_s(1) ; CALL setup_error_act ( '<wert-2>', ctxt )
                WRITE(ctxt,'(G15.6)') dp_f(1) ; CALL setup_error_act ( '<zul-wert-1>', ctxt )
                WRITE(ctxt,'(G15.6)') dp_s(1) ; CALL setup_error_act ( '<zul-wert-2>', ctxt )
                IF ( frst_idx(i) > 0 ) ok(frst_idx(i)) = .false.
                IF ( scnd_idx(i) > 0 ) ok(scnd_idx(i)) = .false.
             END IF
          ELSE IF ( frst_idx(i) == 0 .AND. scnd_idx(i) == 0 ) THEN ! beide nicht vorhanden
             CONTINUE ! beide nicht vorhanden
          ELSE                                     ! nur eines der Attribute vorhanden
             IF ( i /= 3 ) THEN
                CALL setup_error_act ( all_errors(:), 6618, c_upname, c_modname )
                CALL setup_error_act ( '<AktAttName1>', TRIM(c_att_name(frst_idx(i))) )
                CALL setup_error_act ( '<AktAttName2>', TRIM(c_att_name(scnd_idx(i))) )
                WRITE(ctxt(1:10),'(I10)') 0 ; CALL setup_error_act ( '<VarId>', ctxt(1:10) )
                WRITE(ctxt(1:10),'(I10)') frst_idx(i) ; CALL setup_error_act ( '<idx1>', ctxt(1:10) )
                WRITE(ctxt(1:10),'(I10)') scnd_idx(i) ; CALL setup_error_act ( '<idx2>', ctxt(1:10) )
                IF ( frst_idx(i) > 0 ) ok(frst_idx(i)) = .false.
                IF ( scnd_idx(i) > 0 ) ok(scnd_idx(i)) = .false.
             END IF
          END IF
       END SELECT
    END DO
    !
    ! -------------------------------------------------------------------------------------------
    ! Konsistenztests fuer verschiedene variablenbezogenen Attribute
    ! -------------------------------------------------------------------------------------------
    !
    ! ... ggf. weitere Querchecks einfuegen
    var_id => get_var_id_list_for_att ( this(:) )
    IF ( ASSOCIATED( var_id ) ) THEN
       ! ... Attribute zu allen Variablen-Id's ermitteln
       DO j=1,SIZE(var_id)
          att_idx => get_att_idx_for_var_id ( this(:), var_id(j) )
          IF ( ASSOCIATED(att_idx) ) THEN
             ! (A) Attribute, die paarweise vorkommen muessen, und die geordnet sein uessen
             !     A.1 low_water_mark            <= high_water_mark
             !     A.2 low_tracer_mark           <= high_tracer_mark
             !     A.3 valid_min                 <= valid_max [ nur falls beide vorhanden ]
             DO i=1,c_max_a
                n_f = get_att_idx( this(att_idx), c_att_name(c_f_a_id(i)) )
                n_s = get_att_idx( this(att_idx), c_att_name(c_s_a_id(i)) )
                IF      ( n_f  > 0 .AND. n_s  > 0 ) THEN ! beide vorhanden
                   dp_f(:) = 1.0_Double ; dp_s(:) = -1.0_Double
                   IF ( is_att_dp(this(att_idx(n_f))) ) dp_f(:) = get_att_dp(this(att_idx(n_f))) 
                   IF ( is_att_dp(this(att_idx(n_s))) ) dp_s(:) = get_att_dp(this(att_idx(n_s))) 
                   IF ( is_att_in(this(att_idx(n_f))) ) dp_f(:) = REAL(get_att_in(this(att_idx(n_f))),Double)
                   IF ( is_att_in(this(att_idx(n_s))) ) dp_s(:) = REAL(get_att_in(this(att_idx(n_s))),Double)
                   IF ( dp_f(1) > dp_s(1) ) THEN
                      CALL setup_error_act ( all_errors(:), 6617, c_upname, c_modname )
                      CALL setup_error_act ( '<AktAttName1>', TRIM(c_att_name(c_f_a_id(i))) )
                      CALL setup_error_act ( '<AktAttName2>', TRIM(c_att_name(c_s_a_id(i))) )
                      CALL setup_error_act ( '<op>', '<=' )
                      WRITE(ctxt(1:10),'(I10)') var_id(j) ; CALL setup_error_act ( '<VarId>', ctxt(1:10) )
                      WRITE(ctxt,'(G15.6)') dp_f(1) ; CALL setup_error_act ( '<wert-1>', ctxt )
                      WRITE(ctxt,'(G15.6)') dp_s(1) ; CALL setup_error_act ( '<wert-2>', ctxt )
                      WRITE(ctxt,'(G15.6)') dp_f(1) ; CALL setup_error_act ( '<zul-wert-1>', ctxt )
                      WRITE(ctxt,'(G15.6)') dp_s(1) ; CALL setup_error_act ( '<zul-wert-2>', ctxt )
                      IF ( n_f > 0 ) ok(att_idx(n_f)) = .false.
                      IF ( n_s > 0 ) ok(att_idx(n_s)) = .false.
                   END IF
                ELSE IF ( n_f == 0 .AND. n_s == 0 ) THEN ! beide nicht vorhanden
                   CONTINUE ! beide nicht vorhanden
                ELSE                                     ! nur eines der Attribute vorhanden
                   ! einstweilen keine Fehlermeldung, da Konflikt mit BDF
!!$                   IF ( i /= 3 ) THEN
!!$                      CALL setup_error_act ( all_errors(:), 6618, c_upname, c_modname )
!!$                      CALL setup_error_act ( '<AktAttName1>', TRIM(c_att_name(c_f_a_id(i))) )
!!$                      CALL setup_error_act ( '<AktAttName2>', TRIM(c_att_name(c_s_a_id(i))) )
!!$                      WRITE(ctxt(1:10),'(I10)') var_id(j) ; CALL setup_error_act ( '<VarId>', ctxt(1:10) )
!!$                      IF ( n_f > 0 ) ok(att_idx(n_f)) = .false.
!!$                      IF ( n_s > 0 ) ok(att_idx(n_s)) = .false.
!!$                   END IF
                END IF
             END DO
             ! (B) Attribute, die paarweise vorkommen muessen, und identisch sein muessen
             !     B.1 _FillValue == missing_value
             DO i=1,c_max_b
                n_f = get_att_idx( this(att_idx), c_att_name(c_f_b_id(i)) )
                n_s = get_att_idx( this(att_idx), c_att_name(c_s_b_id(i)) )
                IF      ( n_f  > 0 .AND. n_s  > 0 ) THEN ! beide vorhanden
                   dp_f(:) = 1.0_Double ; dp_s(:) = -1.0_Double
                   IF ( is_att_dp(this(att_idx(n_f))) ) dp_f(:) = get_att_dp(this(att_idx(n_f))) 
                   IF ( is_att_dp(this(att_idx(n_s))) ) dp_s(:) = get_att_dp(this(att_idx(n_s))) 
                   IF ( is_att_in(this(att_idx(n_f))) ) dp_f(:) = REAL(get_att_in(this(att_idx(n_f))),Double)
                   IF ( is_att_in(this(att_idx(n_s))) ) dp_s(:) = REAL(get_att_in(this(att_idx(n_s))),Double)
                   IF ( dp_f(1) /= dp_s(1) ) THEN
                      CALL setup_error_act ( all_errors(:), 6617, c_upname, c_modname )
                      CALL setup_error_act ( '<AktAttName1>', TRIM(c_att_name(c_f_b_id(i))) )
                      CALL setup_error_act ( '<AktAttName2>', TRIM(c_att_name(c_s_b_id(i))) )
                      CALL setup_error_act ( '<op>', '==' )
                      WRITE(ctxt(1:10),'(I10)') var_id(j) ; CALL setup_error_act ( '<VarId>', ctxt(1:10) )
                      WRITE(ctxt,'(G15.6)') dp_f(1) ; CALL setup_error_act ( '<wert-1>', ctxt )
                      WRITE(ctxt,'(G15.6)') dp_s(1) ; CALL setup_error_act ( '<wert-2>', ctxt )
                      WRITE(ctxt,'(G15.6)') dp_f(1) ; CALL setup_error_act ( '<zul-wert-1>', ctxt )
                      WRITE(ctxt,'(G15.6)') dp_s(1) ; CALL setup_error_act ( '<zul-wert-2>', ctxt )
                      IF ( n_f > 0 ) ok(att_idx(n_f)) = .false.
                      IF ( n_s > 0 ) ok(att_idx(n_s)) = .false.
                   END IF
                END IF
             END DO
             ! (C) Attribute die niemals gleichzeitig vorkommen duerfen
             !     C.1 valid_range und valid_min
             !     C.2 valid_range und valid_max
             DO i=1,c_max_c
                n_f = get_att_idx( this(att_idx), c_att_name(c_f_c_id(i)) )
                n_s = get_att_idx( this(att_idx), c_att_name(c_s_c_id(i)) )
                IF ( n_f  > 0 .AND. n_s  > 0 ) THEN ! beide vorhanden
                   CALL setup_error_act ( all_errors(:), 6619, c_upname, c_modname )
                   CALL setup_error_act ( '<AktAttName1>', TRIM(c_att_name(c_f_c_id(i))) )
                   CALL setup_error_act ( '<AktAttName2>', TRIM(c_att_name(c_s_c_id(i))) )
                   WRITE(ctxt(1:10),'(I10)') var_id(j) ; CALL setup_error_act ( '<VarId>', ctxt(1:10) )
                   IF ( n_f > 0 ) ok(att_idx(n_f)) = .false.
                   IF ( n_s > 0 ) ok(att_idx(n_s)) = .false.
                END IF
             END DO
             !
             ! (D) Ungueltige oder Fillwerte muessen ausserhalb des Wertebereichs liegen (Teil 1)
             !     D.1 valid_range und missing_value
             !     D.2 valid_range und _FillValue
             DO i=1,c_max_d
                n_f = get_att_idx( this(att_idx), c_att_name(c_f_d_id(i)) )
                n_s = get_att_idx( this(att_idx), c_att_name(c_s_d_id(i)) )
                IF ( n_f  > 0 .AND. n_s  > 0 ) THEN ! beide vorhanden
                   dp_f(:) = 1.0_Double ; dp_s(:) = -1.0_Double
                   IF ( is_att_dp(this(att_idx(n_f))) ) dp_f(:) = get_att_dp(this(att_idx(n_f))) 
                   IF ( is_att_dp(this(att_idx(n_s))) ) dp_s(:) = get_att_dp(this(att_idx(n_s))) 
                   IF ( is_att_in(this(att_idx(n_f))) ) dp_f(:) = REAL(get_att_in(this(att_idx(n_f))),Double)
                   IF ( is_att_in(this(att_idx(n_s))) ) dp_s(:) = REAL(get_att_in(this(att_idx(n_s))),Double)
                   IF ( dp_r(1) <= dp_s(1) .AND. dp_r(2) >= dp_s(1) ) THEN
                      CALL setup_error_act ( all_errors(:), 6620, c_upname, c_modname )
                      CALL setup_error_act ( '<AktAttName>', TRIM(c_att_name(c_s_d_id(i))) )
                      CALL setup_error_act ( '<op1>', '<'    )
                      CALL setup_error_act ( '<op2>', '.OR.' )
                      CALL setup_error_act ( '<op3>', '>'    )
                      WRITE(ctxt(1:10),'(I10)') var_id ; CALL setup_error_act ( '<var_id>', ctxt(1:10) )
                      WRITE(ctxt,'(G15.9)') dp_r(1)    ; CALL setup_error_act ( '<zul-wert-1>', ctxt )
                      WRITE(ctxt,'(G15.9)') dp_r(2)    ; CALL setup_error_act ( '<zul-wert-2>', ctxt )
                      WRITE(ctxt,'(G15.9)') dp_s(1)    ; CALL setup_error_act ( '<wert>', ctxt       )
                      IF ( n_s > 0 ) ok(att_idx(n_s)) = .false.
                   END IF
                END IF
             END DO
             !
             ! (E) Ungueltige oder Fillwerte muessen ausserhalb des Wertebereichs liegen (Teil 2)
             !     E.1 valid_min, valid_max und missing_value
             !     E.2 valid_min, valid_max und _FillValue
             DO i=1,c_max_e
                n_f = get_att_idx( this(att_idx), c_att_name(c_f_e_id(i)) )
                n_s = get_att_idx( this(att_idx), c_att_name(c_s_e_id(i)) )
                n_t = get_att_idx( this(att_idx), c_att_name(c_t_e_id(i)) )
                IF ( n_f  > 0 .AND. n_s  > 0 .AND. n_t > 0 ) THEN ! beide vorhanden
                   dp_f(:) = 0.0_Double ; dp_s(:) = 0.0_Double ; dp_t(:) = 0.0_Double
                   IF ( is_att_dp(this(att_idx(n_f))) ) dp_f(:) = get_att_dp(this(att_idx(n_f))) ! valid_min
                   IF ( is_att_dp(this(att_idx(n_s))) ) dp_s(:) = get_att_dp(this(att_idx(n_s))) ! valid_max
                   IF ( is_att_dp(this(att_idx(n_t))) ) dp_t(:) = get_att_dp(this(att_idx(n_t))) ! Fuellwert, fehlender Wert
                   IF ( is_att_in(this(att_idx(n_f))) ) dp_f(:) = REAL(get_att_in(this(att_idx(n_f))),Double) ! valid_min
                   IF ( is_att_in(this(att_idx(n_s))) ) dp_s(:) = REAL(get_att_in(this(att_idx(n_s))),Double) ! valid_max
                   IF ( is_att_in(this(att_idx(n_t))) ) dp_t(:) = REAL(get_att_in(this(att_idx(n_t))),Double) ! Fuellwert, fehlender Wert
                   IF ( dp_f(1) <= dp_t(1) .AND. dp_s(1) >= dp_t(1) ) THEN
                      CALL setup_error_act ( all_errors(:), 6620, c_upname, c_modname )
                      CALL setup_error_act ( '<AktAttName>', TRIM(c_att_name(c_s_e_id(i))) )
                      CALL setup_error_act ( '<op1>', '<'    )
                      CALL setup_error_act ( '<op2>', '.OR.' )
                      CALL setup_error_act ( '<op3>', '>'    )
                      WRITE(ctxt(1:10),'(I10)') var_id ; CALL setup_error_act ( '<var_id>', ctxt(1:10) )
                      WRITE(ctxt,'(G15.9)') dp_f(1)    ; CALL setup_error_act ( '<zul-wert-1>', ctxt )
                      WRITE(ctxt,'(G15.9)') dp_s(1)    ; CALL setup_error_act ( '<zul-wert-2>', ctxt )
                      WRITE(ctxt,'(G15.9)') dp_t(1)    ; CALL setup_error_act ( '<wert>', ctxt       )
                      IF ( n_s > 0 ) ok(att_idx(n_t)) = .false.
                   END IF
                END IF
             END DO
             !
             ! (F) Spezial
             !     E.1 Anzahl der "flag_meanings" muss der Anzahl der "flag_values" entsprechen
             DO i=1,c_max_f
                n_f = get_att_idx( this(att_idx), c_att_name(c_f_f_id(i)) )
                n_s = get_att_idx( this(att_idx), c_att_name(c_s_f_id(i)) )
                IF      ( n_f  > 0 .AND. n_s  > 0 ) THEN ! beide vorhanden
                   DO k=1,MIN(SIZE(this(att_idx(n_f))%ch),SIZE(this(att_idx(n_s))%ch))
                      m_f = get_word_in_ch_count ( this(att_idx(n_f))%ch(k) )
                      m_s = get_word_in_ch_count ( this(att_idx(n_s))%ch(k) )
                      IF ( m_f /= m_s ) THEN
                         CALL setup_error_act ( all_errors(:), 6621, c_upname, c_modname )
                         CALL setup_error_act ( '<AktAttName1>', TRIM(c_att_name(c_f_f_id(i))) )
                         CALL setup_error_act ( '<AktAttName2>', TRIM(c_att_name(c_s_f_id(i))) )
                         WRITE(ctxt(1:10),'(I10)') var_id ; CALL setup_error_act ( '<var_id>', ctxt(1:10) )
                         CALL setup_error_act ( '<wert-1>', TRIM(this(att_idx(n_f))%ch(k)) )
                         CALL setup_error_act ( '<wert-2>', TRIM(this(att_idx(n_s))%ch(k)) )
                         WRITE(ctxt(1:10),'(I10)') m_f ; CALL setup_error_act ( '<anzahl-1>', ctxt(1:10) )
                         WRITE(ctxt(1:10),'(I10)') m_s ; CALL setup_error_act ( '<anzahl-2>', ctxt(1:10) )
                         IF ( n_f > 0 ) ok(att_idx(n_f)) = .false.
                         IF ( n_s > 0 ) ok(att_idx(n_s)) = .false.
                      END IF
                   END DO
                ELSE IF ( n_f == 0 .AND. n_s == 0 ) THEN ! beide nicht vorhanden
                   CONTINUE ! beide nicht vorhanden
                ELSE                                     ! nur eines der Attribute vorhanden
                   CALL setup_error_act ( all_errors(:), 6618, c_upname, c_modname )
                   CALL setup_error_act ( '<AktAttName1>', TRIM(c_att_name(c_f_f_id(i))) )
                   CALL setup_error_act ( '<AktAttName2>', TRIM(c_att_name(c_f_f_id(i))) )
                   WRITE(ctxt(1:10),'(I10)') var_id(j) ; CALL setup_error_act ( '<VarId>', ctxt(1:10) )
                   IF ( n_f > 0 ) ok(att_idx(n_f)) = .false.
                   IF ( n_s > 0 ) ok(att_idx(n_s)) = .false.
                END IF
             END DO
             !
             DEALLOCATE(att_idx)
             NULLIFY(att_idx)
          END IF
       END DO
       ! ... Aufraeumen
       DEALLOCATE( var_id ) 
       NULLIFY(var_id)
    END IF
    !
  END FUNCTION ok_att_consistency_d
  !
  !! Pr&uuml;fe, ob in einer Liste von Attributen Informationen zu 
  !! Sigma-Schichten abgelegt sind <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_att_sigma_interfaces_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !
    res = MERGE( .true., .false., get_att_idx_n_0 ( this(:), c_att_name(35) ) > 0 )
    !
  END FUNCTION has_att_sigma_interfaces_1
  !
  !! Pr&uuml;fe, ob in einer Liste von Attributen Informationen zu 
  !! Z-Schichten abgelegt sind <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_att_layer_interfaces_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !
    res = MERGE( .true., .false., get_att_idx_n_0 ( this(:), c_att_name(36) ) > 0 )
    !
  END FUNCTION has_att_layer_interfaces_1
  !
  !! Pr&uuml;fe, ob in der Attributliste Angaben zur dynamischen Bathymetrie vorhanden
  !! sind und diese auf eine dynamische Ver&auml;nderung eingestellt sind <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_att_dynamic_bathymetry_1 ( this ) &
       RESULT ( res )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !
    IF ( get_att_idx_n_0(this(:),c_att_name(57)) > 0 ) THEN
       res = MERGE( .true., .false., this(get_att_idx_n_0(this(:),c_att_name(57)))%in(1) == 1 )
    ELSE
       res = .false.
    END IF
    !
  END FUNCTION has_att_dynamic_bathymetry_1
  !
  !! Pr&uuml;fe, ob in der Attributliste Angaben zur minimalen Wasserbedeckung vorhanden sind <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_att_minimum_water_depth_1 ( this ) &
       RESULT ( res )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !
    res = MERGE( .true., .false., get_att_idx_n_0(this(:),c_att_name(37)) > 0 )
    !
  END FUNCTION has_att_minimum_water_depth_1
  !
  !! Pr&uuml;fe, ob in der Attributliste Angaben zu einer bestimmten physikalischen 
  !! Gr&ouml;&szlig;e "name_id" vorhanden sind  <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_att_name_id_1_0 ( this, val ) &
       RESULT ( res )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! Code der physikalischen Gr&ouml;$szlig;e
    INTEGER      , INTENT(IN) :: val     ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !! Hilfsvariable
    INTEGER :: i   ! 
    ! 
    res = .false.
    DO i=1,SIZE(this)
       IF ( res ) EXIT
       IF ( get_lowercase_char(this(i)%name) == get_lowercase_char(c_att_name(41)) ) THEN
          IF ( this(i)%in(1) == val ) res = .true.
       END IF
    END DO
    !
  END FUNCTION has_att_name_id_1_0
  !
  !! Pr&uuml;fe, ob in der Attributliste Angaben zur dynamischen Bathymetrie vorhanden
  !! sind und diese auf eine dynamische Ver&auml;nderung eingestellt sind <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_att_cross_section_average_1 ( this ) &
       RESULT ( res )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! Testergebnis
    LOGICAL :: res ! 
    !
    IF ( get_att_idx_n_0(this(:),c_att_name(61)) > 0 ) THEN
       res = MERGE( .true., .false., this(get_att_idx_n_0(this(:),c_att_name(61)))%in(1) == 1 )
    ELSE
       res = .false.
    END IF
    !
  END FUNCTION has_att_cross_section_average_1
  !! 
  !! Pr&uuml;fe, ob in der Attributliste f&uuml;r eine Variable das Attribut
  !! Standardname vorhanden ist oder nicht <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION has_att_standard_name_1_0 ( this, var ) &
       RESULT(res)
    !! Datenobjekt (Vektor)
    TYPE (t_att)    , INTENT(IN) :: this(:) ! 
    !! Variable f&uuml;r die das Vorhandensein des Attributs gepr&uuml;ft werden soll
    TYPE (t_var)    , INTENT(IN) :: var      ! 
    !! Ergebnis: Attribut vorhanden / nicht vorhanden
    LOGICAL                      :: res      !    
    !! Hilfsvariable
    CHARACTER (LEN=c_len_att_ch) :: ch       ! 
    !
    ch  = REPEAT( ' ', LEN(ch) )
    ch  = get_att_standard_name ( this, var )
    res = .NOT. ( LEN_TRIM(ch) == 9 .AND. ch(1:9) == 'undefined' ) 
    !
  END FUNCTION has_att_standard_name_1_0
  !
  !! Pr&uuml;fe, ob eine bestimmte Datei als <EM>related file</EM> vorhanden ist <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldung
  FUNCTION has_att_related_file_1_0 ( this, var ) &
       RESULT(res)
    !! Datenobjekt (Vektor)
    TYPE (t_att)  , INTENT(IN) :: this(:) ! 
    !! Dateiobjekt
    TYPE (t_file) , INTENT(IN) :: var     !
    !! Ergebnis: typgleiche Datei ist als related file vorhanden
    LOGICAL                    :: res     ! 
    ! Hilfsvariablen
    CHARACTER (LEN=c_file_type_len) :: l_type(2) ! 
    TYPE (t_file)         , POINTER :: l_file(:) ! 
    INTEGER                         :: i         ! 
    !
    res = .false.
    l_file => get_att_related_file_1 ( this )
    IF ( ASSOCIATED(l_file) ) THEN
       l_type(1) = get_lowercase_char( get_file_type(var) )
       DO i=1,SIZE(l_file)
          IF ( res ) EXIT
          l_type(2) = get_lowercase_char( get_file_type(l_file(i)) )
          IF ( eq_file_path_and_name( l_file(i), var ) ) THEN
             IF ( l_type(1) == l_type(2) ) res = .true.
          END IF
       END DO
       DEALLOCATE(l_file)
    END IF
    ! 
  END FUNCTION has_att_related_file_1_0
  !
  !! Ermittle die Anzahl der Schichtgrenzen (Sigma- oder Z-Schichten) <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_att_interfaces_count_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! Anzahl der Schichten
    INTEGER :: res ! 
    !
    IF      ( has_att_sigma_interfaces ( this ) ) THEN
       res = get_att_nof_values_0_0 ( this(get_att_idx_n_0(this(:),c_att_name(35))) )
    ELSE IF ( has_att_layer_interfaces ( this ) ) THEN
       res = get_att_nof_values_0_0 ( this(get_att_idx_n_0(this(:),c_att_name(36))) )
    ELSE
       res = 0
    END IF
    !
  END FUNCTION get_att_interfaces_count_1
  !
  !! ermittle den Wert f&uuml;r eine Schichtgrenze anhand der lfd. Nummer <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_att_interface_1_0 ( this, val ) & 
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! lfd. Nummer der Schichtgrenze
    INTEGER      , INTENT(IN) :: val     ! 
    !! Ergebnis : Schichtgrenze
    REAL (KIND=Double) :: res  ! 
    !
    res = -12345.0_Double
    IF ( val >= 1 .AND. val <= get_att_interfaces_count_1 ( this ) ) THEN
       IF      ( has_att_sigma_interfaces ( this ) ) THEN
          res = this(get_att_idx_n_0(this(:),c_att_name(35)))%dp(val)
       ELSE IF ( has_att_layer_interfaces ( this ) ) THEN
          res = this(get_att_idx_n_0(this(:),c_att_name(36)))%dp(val)
       END IF
    END IF
    !
  END FUNCTION get_att_interface_1_0
  !
  !! ermittle den Wert der minimalen Wasserbedeckung, falls vorhanden, 
  !! anderenfalls wird 0.0 zur&uuml;ckgegeben <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_att_minimum_water_depth_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: minimal zul&auml;ssige Wasserbedeckung
    REAL (KIND=Double)        :: res     ! 
    !
    IF ( has_att_minimum_water_depth( this ) ) THEN
       res = this(get_att_idx_n_0(this,c_att_name(37)))%dp(1)
    ELSE
       res = 0.0_Double
    END IF
    ! 
  END FUNCTION get_att_minimum_water_depth_1
  !
  !! ermittle den Code "name_id" einer Variablen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_att_var_name_id_1_0 ( this, var ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! Variable, deren "name_id" ermittelt werden soll
    TYPE (t_var) , INTENT(IN) :: var     ! 
    !! Ergebnis: Code "name_id" der Variablen, falls nicht vorhanden
    !! wird -1 zur&uuml;ckgegeben
    INTEGER :: res ! 
    !! Hilfsvariable
    INTEGER , POINTER :: idx(:) ! 
    INTEGER           :: jdx    ! 
    !
    res = -1
    idx => get_att_idx_for_var_id ( this, get_var_id( var ) )
    IF ( ASSOCIATED(idx) ) THEN
       jdx = get_att_idx ( this(idx), c_att_name(41) )
       IF ( jdx > 0 ) res = this(idx(jdx))%in(1)
       DEALLOCATE(idx) ; NULLIFY(idx)
    END IF
    !
  END FUNCTION get_att_var_name_id_1_0
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
  ! Lokale Methoden:
  !
  ! ----------------------------------------------------------------------
  ! >>> ALLGEMEIN-Methoden <<< [ERR_NO =  0001 bis  0999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der Fehlerbedingung 1 = Modul nicht initialisiert <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_initialised ( upname )         &
       RESULT( ok )
    !! Name der Subroutine die "ok_initialised" ruft
    CHARACTER (LEN=*) , INTENT(IN) :: upname ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Fehlernummer
    INTEGER            :: ierr    ! 
    !! Fehlertext
    CHARACTER (LEN=80) :: cerr(3) ! 
    !
    ok = initialised
    !
    IF ( .NOT. ok ) THEN
       WRITE(*,*) ' *** Warnung *** Modul "b_att" nicht initialisiert'
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_att ausfuehren'
       CALL setup_error_act ( ierr, cerr(:), upname, c_modname )
    END IF
    !
  END FUNCTION ok_initialised
  !
  !! Setzen der Fehlerbedingung 2 = Modul schon initialisiert <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION not_initialised ( upname )          &
       RESULT( ok )
    !! Name der Subroutine die "not_initialised" ruft
    CHARACTER (LEN=*) , INTENT(IN) :: upname ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .NOT. initialised
    IF ( .NOT. ok ) CALL setup_error_act ( all_errors(:), 2, upname, c_modname )
    !
  END FUNCTION not_initialised
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-INIT-Methoden <<< [ERR_NO =  1000 bis  1999]
  ! ----------------------------------------------------------------------
  !
  !! Allokieren/Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE init_att_all_errors ( )
    !! Z&auml;hlervariable
    INTEGER :: i, ic ! 
    !
    DO i=1,2
       !
       ic = 0
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 1 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist nicht initialisiert\n'//&
               '--> INIT_att ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_att ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_att"\n'//&
               'Typ-Komponente = "ch"\n'//&
               '--> Code in Modul "b_att" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_att"\n'//&
               'Typ-Komponente = "in"\n'//&
               '--> Code in Modul "b_att" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_att"\n'//&
               'Typ-Komponente = "dp"\n'//&
               '--> Code in Modul "b_att" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_att"\n'//&
               'Typ-Komponente = "id"\n'//&
               'aktuell        = <AktId>\n'//&
               'erforderlich   = id > 0\n'//&
               'Attribut       = <AktAttName>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6011 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_att"\n'//&
               'Typ-Komponente = "id"\n'//&
               'aktuell        = <AktId>\n'//&
               'dieser Wert kommt mehrfach in einem Vektor vor\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_att"\n'//&
               'Typ-Komponente = "name"\n'//&
               'aktuell        = "<AktName>"\n'//&
               'erforderlich   = siehe Werte des Feldes "c_att_name(:)"\n'//& 
               'Attribut       = <AktAttName>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6021 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_att"\n'//&
               'Typ-Komponente = "name"\n'//&
               'aktuell        = <AktName>\n'//&
               'dieser Wert kommt mehrfach in einem Vektor vor\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_att"\n'//&
               'Typ-Komponente = "var_id"\n'//&
               'aktuell        = <AktVarId>\n'//&
               'erforderlich   = var_id >= 0\n'//&
               'Attribut       = <AktAttName>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6031 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_att"\n'//&
               'Typ-Komponente = "var_id"\n'//&
               'Typ des Attributs passt nicht zur "var_id"\n'//&
               'aktuell        = <AktVarId>\n'//&
               'Attribut       = <AktAttName>\n'//&
               'Typ CDG        = <cdg>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_att"\n'//&
               'Typ-Komponente = "ch"\n'//&
               'keine Komponente darf den Wert "undefined" aufweisen\n'//&
               'Attribut       = <AktAttName>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_att"\n'//&
               'Typ-Komponente = "in"\n'//&
               'keine Komponente darf den Wert "HUGE(in)" aufweisen\n'//&
               'Attribut       = <AktAttName>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_att"\n'//&
               'Typ-Komponente = "dp"\n'//&
               'keine Komponente darf den Wert "HUGE(dp)" aufweisen\n'//&
               'Attribut       = <AktAttName>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Feld-Komponenten von "t_att"\n'//&
               'Typ-Komponenten = "ch", "in" und "dp"\n'//&
               'Attribut        = <AktAttName>\n'//&
               'es muss genau eine Komponente allokiert sein\n'//&
               'aktuell allokierte Komponenten = <AktAlloc>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6501 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Feld-Komponenten von "t_att"\n'//&
               'der Typ der erforderlichen/allokierten Feldkomponenten ist inkonsistent\n'//&
               'aktuell      = <AktType>\n'//&
               'erforderlich = <ReqType>\n'//&
               'Attribut     = <AktAttName>\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6502 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Feld-Komponenten von "t_att"\n'//&
               'die Anzahl der erforderlichen/allokierten Feldkomponenten ist inkonsistent\n'//&
               'aktuell      = <AktValues>\n'//&
               'erforderlich = <ReqValues>\n'//&
               'Attribut     = <AktAttName>\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6503 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Feld-Komponenten von "t_att"\n'//&
               'dynamische Feldkomponenten wurden nicht allokiert\n'//&
               'Attribut     = <AktAttName>\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6600 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in der Komponente "var_id" eines Objektes "t_att"\n'//&
               'fuer die aktuelle "var_id" ist kein Eintrag in einer Liste\n'//&
               'von Variablen des Typs "t_var" vorhanden\n'//&
               'akt. Attribut = <AktAttName>\n'//&
               'akt. "var_id" = <AktVarId>\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6601 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler im Attribut "<..name..>" eines Objektes "t_att"\n'//&
               'erwartet wird eine Datumsangabe im englischen BAW-Format wie folgt:\n'//&
               'mm/dd/yyyy-hh:mm:ss:nnnnnnnnn ZONE\n'//&
               'aktueller (fehlerhafter) Wert:\n'//&
               '<..date..>\n'//&
               '--> Code/Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6602 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler im Attribut "multiple_dates" eines Objektes "t_att"\n'//&
               'erwartet werden Datumsangaben im englischen BAW-Format wie folgt:\n'//&
               'mm/dd/yyyy-hh:mm:ss:nnnnnnnnn ZONE\n'//&
               'es sind nicht alle Werte sinnvoll\n'//&
               'aktuelle Werte:\n'//&
               '<..multidates..>\n'//&
               '--> Code/Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6603 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler im Attribut "multiple_dates" eines Objektes "t_att"\n'//&
               'erwartet werden chronologische Datumsangaben:\n'//&
               'aktuelle Werte (lfd. Nummern <..lfdNrn..>):\n'//&
               '<..multidate1..>\n'//&
               '<..multidate2..>\n'//&
               '--> Code/Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6604 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler im Attribut "<..name..>" eines Objektes "t_att"\n'//&
               'die aktuelle Angabe entspricht keinem der erlaubten Werte\n'//&
               'akt. Wert : <..aktuell..>\n'//&
               'erlaubte Angaben :\n'//&
               '     <..erlaubt..>\n'//&
               '--> Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6605 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler im Attribut "<AktAttName>" eines Objektes "t_att"\n'//&
               'der zulaessige Wertebereich wird nicht eingehalten\n'//&
               'zulaessig : <zul-wert-1> <op1> WERT <op2> <zul-wert-2>\n'//&
               'aktuell   : WERT = <wert>\n'//&
               '--> Code/Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6606 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler im Attribut "<..name..>" eines Objektes "t_att"\n'//&
               'die aktuelle Angabe entspricht keinem der erlaubten Werte\n'//&
               'akt. Wert : <..aktuell..>\n'//&
               '--> Code/Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6607 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler im Attribut "<..name..>" eines Objektes "t_att"\n'//&
               'erlaubt sind Werte groesser 0 ... \n'//&
               'akt. Wert : <..aktuell..>\n'//&
               '--> Code/Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6608 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler im Attribut "<..name..>" eines Objektes "t_att"\n'//&
               'erlaubt sind Werte groesser/gleich 0 ... \n'//&
               'akt. Wert : <..aktuell..>\n'//&
               '--> Code/Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6609 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler im Attribut "<..name..>" eines Objektes "t_att"\n'//&
               'erlaubt sind die Werte <..erlaubt..> \n'//&
               'akt. Wert : <..aktuell..>\n'//&
               '--> Code/Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6610 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler im Attribut "ref_name_id" eines Objektes "t_att"\n'//&
               'erlaubt sind ganze Zahlen groesser oder gleich 0 ... \n'//&
               'akt. Wert : <..refid..>\n'//&
               '--> Code/Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6611 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler im Attribut "<AktAttName>" eines Objektes "t_att"\n'//&
               'das zulaessige Verhaeltnis der Werte wird nicht eingehalten\n'//&
               'zulaessig : <wert-1> <op1> <wert-2>\n'//&
               '--> Code/Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6612 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler im Attribut "<..name..>" eines Objektes "t_att"\n'//&
               'erlaubt sind die Werte 0 und 1 \n'//&
               'akt. Wert imorpho = <..aktuell..>\n'//&
               '--> Code/Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6613 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler im Attribut "<name>" eines Objektes "t_att"\n'//&
               'erlaubt sind die Werte aus Feld "<feldname>"\n'//&
               'aktuell = <aktuell>\n'//&
               '--> Code/Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6614 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler im Attribut "<AktAttName>" eines Objektes "t_att"\n'//&
               'Schichtgrenzen des Typs "<LayerTyp>" sind nicht korrekt sortiert\n'//&
               '--> Code/Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6615 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_att"\n'//&
               'Typ-Komponente = "ch"\n'//&
               'alle Feldelemente sind leer\n'//&
               'Attribut       = <AktAttName>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6616 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_att"\n'//&
               'Typ-Komponente = "ch"\n'//&
               'mindestens ein Feldelement ist leer\n'//&
               'Attribut       = <AktAttName>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6617 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler bei Vergleich der Werte zweier Attribute in Objekt "t_att"\n'//&
               'var_id     = "<VarId>"\n'//&
               'Attribut 1 = "<AktAttName1>"\n'//&
               'Attribut 2 = "<AktAttName2>"\n'//&
               'Wert 1     = <wert-1>\n'//&
               'Wert 2     = <wert-2>\n'//&
               'die nachfolgende Vergleichsoperation liefert ein falsches Ergebnis\n'//&
               'zulaessig : <zul-wert-1> <op> <zul-wert-2>\n'//&
               '--> Code/Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6618 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'eines von zwei erforderlichen Attributen fehlt in Objekt "t_att"\n'//&
               'var_id     = "<VarId>"\n'//&
               'Attribut 1 = "<AktAttName1>", idx = <idx1>\n'//&
               'Attribut 2 = "<AktAttName2>", idx = <idx2>\n'//&
               '--> Code/Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6619 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'inkompatible Attribute kommen gleichzeitig in Objekt "t_att" vor\n'//&
               'var_id     = "<VarId>"\n'//&
               'Attribut 1 = "<AktAttName1>"\n'//&
               'Attribut 2 = "<AktAttName2>"\n'//&
               '--> Code/Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6620 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler im Attribut "<AktAttName>" eines Objektes "t_att"\n'//&
               'der zulaessige Wertebereich wird nicht eingehalten\n'//&
               'VarID     : <var_id>\n'//&
               'zulaessig : WERT <op1> <zul-wert-1> <op2> <zul-wert-2> <op3> WERT\n'//&
               'aktuell   : WERT = <wert>\n'//&
               '--> Code/Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6621 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler bei Vergleich der Werte zweier Attribute in Objekt "t_att"\n'//&
               'var_id     = "<VarId>"\n'//&
               'Attribut 1 = "<AktAttName1>"\n'//&
               'Attribut 2 = "<AktAttName2>"\n'//&
               'Wert 1     = <wert-1>\n'//&
               'Wert 2     = <wert-2>\n'//&
               'beide Attribute muessen dieselbe Anzahl von Werten aufweisen\n'//&
               'Anzahl 1   = <anzahl-1>\n'//&
               'Anzahl 2   = <anzahl-2>\n'//&
               '--> Code/Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6622 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Name einer zugeordneten Variablen ist in "t_var(:)" unbekannt\n'//&
               'var_id   = "<VarId>"\n'//&
               'Attribut = "<AktAttName>"\n'//&
               'Variable = "<AktVarName>"\n'//&
               '--> Code/Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6623 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'wenigstes ein erforderliches Attribut ist fuer eine Beschreibung der\n'//&
               'der Kartenprojektion nicht vorhanden\n'//&
               'Mapping    = "<GridMappingName>"\n'//&
               'Attribut 1 = "<AktAttName1>", Status = <status1>\n'//&
               'Variable 2 = "<AktAttName2>", Status = <status2>\n'//&
               '--> Code/Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6624 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'der Typ des Attributs passt nicht zum Typ der Variablen\n'//&
               'Attribut Name  = "<AttributName>"\n'//&
               '...      Typ   = "<AttributTyp>"\n'//&
               'Variablen Name = "<VariableName>"\n'//&
               '...       Typ  = "<VariableTyp>"\n'//&
               '--> Code/Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6801 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler beim Konsistenzcheck zweier Attribute\n'//&
               'es muss gelten: Datum1 < oder = Datum2 ... \n'//&
               'Attribut1 : <..AttName1..>\n'//&
               '   Inhalt : <..Date1..>\n'//&
               'Attribut2 : <..AttName2..>\n'//&
               '   Inhalt : <..Date2..>\n'//&
               '--> Code/Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6802 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler beim Konsistenzcheck zweier Attribute\n'//&
               'es muss genau eines der beiden Attribute vorhanden sein ... \n'//&
               'Attribut1 : <..AttName1..>\n'//&
               'Status (>0 bedeutet _vorhanden_) : <..idx1..>\n'//&
               'Attribut2 : <..AttName2..>\n'//&
               'Status (>0 bedeutet _vorhanden_) : <..idx2..>\n'//&
               '--> Code/Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code in Modul "b_att" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Fusszeilen\n'//&
               '--> Code in Modul "b_att" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Index des Datenobjektes (1D-Array)\n'//&
               '--> Code in Modul "b_att" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_att"\n'//&
               'Typ-Komponente = "id"\n'//&
               '--> Code in Modul "b_att" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_att"\n'//&
               'Typ-Komponente = "name"\n'//&
               '--> Code in Modul "b_att" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_att"\n'//&
               'Typ-Komponente = "var_id"\n'//&
               '--> Code in Modul "b_att" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_att"\n'//&
               'Typ-Komponente = "ch"\n'//&
               '--> Code in Modul "b_att" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_att"\n'//&
               'Typ-Komponente = "in"\n'//&
               '--> Code in Modul "b_att" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_att"\n'//&
               'Typ-Komponente = "dp"\n'//&
               '--> Code in Modul "b_att" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "b_att"\n'//&
               '--> Code in Modul "b_att" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_att"\n'//&
               'Typ-Komponente     = "ch"\n'//&
               'aktuelle Dimension = "<AktDim1>"\n'//&
               '--> Code in Modul "b_att" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8050 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_att"\n'//&
               'Typ-Komponente     = "in"\n'//&
               'aktuelle Dimension = "<AktDim1>"\n'//&
               '--> Code in Modul "b_att" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8060 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_att"\n'//&
               'Typ-Komponente     = "dp"\n'//&
               'aktuelle Dimension = "<AktDim1>"\n'//&
               '--> Code in Modul "b_att" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ADD-Methoden\n'//&
               'Fehler beim Allokieren des Hilfsfeldes "this3(:)"\n'//&
               '--> Code in Modul "b_att" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8501 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ADD-Methoden\n'//&
               'Fehler beim De-Allokieren des Pointerfeldes "this1(:)"\n'//&
               '--> Code in Modul "b_att" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8502 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ADD-Methoden\n'//&
               'Fehler beim Allokieren des Pointerfeldes "this1(:)"\n'//&
               '--> Code in Modul "b_att" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8503 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ADD-Methoden\n'//&
               'Fehler beim De-Allokieren des Hilfsfeldes "this3(:)"\n'//&
               '--> Code in Modul "b_att" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8540 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ADD-Methoden\n'//&
               'Typ fehlerhaft fuer Hinzufuegen von Text in das Attribut\n'//&
               'Typ-Komponente     = "ch"\n'//&
               'aktueller Typ      = "<AktTyp>"\n'//&
               'aktuelles Attribut = "<AktAttName>"\n'//&
               '--> Daten in Parameterlister pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8541 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ADD-Methoden\n'//&
               'Fehler beim Allokieren des Hilfsfeldes l_ch(:)\n'//&
               'Typ-Komponente     = "ch"\n'//&
               'aktuelle Dimension = "<AktDim1>"\n'//&
               'aktuelles Attribut = "<AktAttName>"\n'//&
               '--> Daten in Parameterlister pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8542 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ADD-Methoden\n'//&
               'Fehler beim De-Allokieren des Hilfsfeldes l_ch(:)\n'//&
               'Typ-Komponente     = "ch"\n'//&
               'aktuelles Attribut = "<AktAttName>"\n'//&
               '--> Daten in Parameterlister pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8550 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ADD-Methoden\n'//&
               'Typ fehlerhaft fuer Hinzufuegen von ganzen Zahlen in das Attribut\n'//&
               'Typ-Komponente     = "in"\n'//&
               'aktueller Typ      = "<AktTyp>"\n'//&
               'aktuelles Attribut = "<AktAttName>"\n'//&
               '--> Daten in Parameterlister pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8551 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ADD-Methoden\n'//&
               'Fehler beim Allokieren des Hilfsfeldes l_in(:)\n'//&
               'Typ-Komponente     = "in"\n'//&
               'aktuelle Dimension = "<AktDim1>"\n'//&
               'aktuelles Attribut = "<AktAttName>"\n'//&
               '--> Daten in Parameterlister pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8552 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ADD-Methoden\n'//&
               'Fehler beim De-Allokieren des Hilfsfeldes l_in(:)\n'//&
               'Typ-Komponente     = "in"\n'//&
               'aktuelles Attribut = "<AktAttName>"\n'//&
               '--> Daten in Parameterlister pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8560 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ADD-Methoden\n'//&
               'Typ fehlerhaft fuer Hinzufuegen von reellen (Double) Zahlen in das Attribut\n'//&
               'Typ-Komponente     = "dp"\n'//&
               'aktueller Typ      = "<AktTyp>"\n'//&
               'aktuelles Attribut = "<AktAttName>"\n'//&
               '--> Daten in Parameterlister pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8561 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ADD-Methoden\n'//&
               'Fehler beim Allokieren des Hilfsfeldes l_dp(:)\n'//&
               'Typ-Komponente     = "dp"\n'//&
               'aktuelle Dimension = "<AktDim1>"\n'//&
               'aktuelles Attribut = "<AktAttName>"\n'//&
               '--> Daten in Parameterlister pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8562 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ADD-Methoden\n'//&
               'Fehler beim De-Allokieren des Hilfsfeldes l_dp(:)\n'//&
               'Typ-Komponente     = "dp"\n'//&
               'aktuelles Attribut = "<AktAttName>"\n'//&
               '--> Daten in Parameterlister pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Allocate-Fehler Indexfeld "idx(:)" zur Rueckgabe der Indices\n'//&
               'fuer alle zu einer Variablen "var_id" gehoerenden Attribute\n'//&
               'aktuelle Dimension = "<AktDim1>"\n'//&
               'aktuelle Variable  = "<AktVarId>"\n'//&
               '--> Code in Modul "b_att" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9501 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Allocate-Fehler Indexfeld "idx(:)" zur Rueckgabe der Indices\n'//&
               'fuer alle zu einem Attributnamen "name" gehoerenden Attribute\n'//&
               'aktuelle Dimension = "<AktDim1>"\n'//&
               'aktueller Name     = "<AktName>"\n'//&
               '--> Code in Modul "b_att" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9502 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Allocate-Fehler Indexfeld "idx(:)" zur Rueckgabe der Indices\n'//&
               'fuer alle zu einem Attributnamen "name" mit einem bestimmten\n'//&
               'Inhalt "contents" gehoerenden Attribute\n'//&
               'aktuelle Dimension = "<AktDim1>"\n'//&
               'aktueller Name     = "<AktName>"\n'//&
               'aktueller Inhalt   = "<AktContents>"\n'//&
               '--> Code in Modul "b_att" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 9510 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GET-Methoden\n'//&
               'Fehler beim Lesen der Referenzzeit aus "units"-Attribut\n'//&
               'Wert = "<wert>"\n'//&
               '--> Daten/Code pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 21000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ASSIGNMENT(=)-Methoden\n'//&
               'Assignment ist fuer den aktuellen Typ nicht realisiert\n'//&
               'aktuelles Attribut = "<AktAttName>"\n'//&
               'aktueller Typ      = "<AktType>"\n'//&
               'erforderlicher Typ = "[CH,IN,DP]"\n'//&
               '--> Code in Modul "b_att" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -30000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GEN_ERROR-Methoden\n'//&
               'ein Attribut mit Namen "name" kommt nicht in einer Attributliste vor\n'//&
               'Name des gesuchten Attributs = "<AktAttName>"\n'//&
               '--> bereitgestellte Daten pruefen, ggf. PRINT_att verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -30001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GEN_ERROR-Methoden\n'//&
               'ein Attribut mit Namen "name" ist fuer eine Variable "var_id"\n'//&
               'nicht in einer Attributliste vorhanden\n'//&
               'Name des gesuchten Attributs = "<AktAttName>"\n'//&
               'Id der Variablen             = "<AktAttId>"\n'//&
               '--> bereitgestellte Daten pruefen, ggf. PRINT_att verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -30002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: GEN_ERROR-Methoden\n'//&
               'ein oder mehrere Werte sind nicht in den Attribut-Daten enthalten\n'//&
               'Name des gesuchten Attributs   = "<AktAttName>"\n'//&
               'Id der Variablen               = "<AktAttId>"\n'//&
               'Anzahl der Werte im Attribut   = "<NofAttData>"\n'//&
               'Anzahl der Werte in User-Liste = "<NofUserData>"\n'//&
               'Anzahl der gefundenen Werte    = "<ActFoundData>"\n'//&
               '... nicht gefundenen Werte     = "<ActNotFoundData>"\n'//&
               'erste nicht gefundene Position = "<FirstNotFoundUserDataPos>"\n'//&
               '... User-Wertvorgabe           = "<FirstNotFoundUserData>"\n'//&
               '--> bereitgestellte Daten pruefen, ggf. PRINT_att verwenden' )
       END IF
       !
       ! Allokieren der Felder beim ersten Durchlauf (i==1)
       !
       IF ( i == 1 ) THEN
          ALLOCATE ( all_errors( ic ) )
          CALL new_error( all_errors(:) )
       END IF
       !
    END DO
    !
  END SUBROUTINE init_att_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_att_all_errors ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_att_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Allokieren der dynamischen Feld-Komponente "ch" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_att_ch ( this, idim )
    !! Datenobjekt
    TYPE (t_att) , INTENT(INOUT) :: this   ! 
    !! Dimension der dynamischen Feld-Komponente "ch"
    INTEGER      , INTENT(IN)  :: idim   ! 
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER  :: c_upname='alloc_att_ch' !
    !! Statusvariable
    INTEGER :: stat !  
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !
    ALLOCATE ( this%ch(idim), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 8040, c_upname, c_modname, stat )
       WRITE(ctxt,'(I10)') 
       CALL setup_error_act ( '<AktDim1>', ctxt )
    END IF
    !
  END SUBROUTINE alloc_att_ch
  !
  !! Allokieren der dynamischen Feld-Komponente "in" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_att_in ( this, idim )
    !! Datenobjekt
    TYPE (t_att) , INTENT(INOUT) :: this   ! 
    !! Dimension der dynamischen Feld-Komponente "in"
    INTEGER      , INTENT(IN)  :: idim   ! 
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER  :: c_upname='alloc_att_in' !
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !
    ALLOCATE ( this%in(idim), STAT=stat )
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 8050, c_upname, c_modname, stat )
       WRITE(ctxt,'(I10)') 
       CALL setup_error_act ( '<AktDim1>', ctxt )
    END IF
    !
  END SUBROUTINE alloc_att_in
  !
  !! Allokieren der dynamischen Feld-Komponente "dp" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE alloc_att_dp ( this, idim )
    !! Datenobjekt
    TYPE (t_att) , INTENT(INOUT) :: this   ! 
    !! Dimension der dynamischen Feld-Komponente "dp"
    INTEGER      , INTENT(IN)  :: idim   ! 
    !! Name der Subroutine
    CHARACTER (LEN=12), PARAMETER  :: c_upname='alloc_att_dp' !
    !! Statusvariable 
    INTEGER :: stat ! 
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !
    ALLOCATE ( this%dp(idim), STAT=stat )
    !
    IF ( stat /= 0 ) THEN
       CALL setup_error_act ( all_errors(:), 8060, c_upname, c_modname, stat )
       WRITE(ctxt,'(I10)') 
       CALL setup_error_act ( '<AktDim1>', ctxt )
    END IF
    !
  END SUBROUTINE alloc_att_dp
  !
  ! ----------------------------------------------------------------------
  ! --> nicht benoetigte INIT-Routinen bitte unbedingt loeschen <---------
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren der Feld-Komponente "ch" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_att_ch ( this )
    !! Datenobjekt
    TYPE (t_att) , INTENT(INOUT) :: this   ! 
    !
    this%ch(:) = REPEAT( ' ',LEN(this%ch) )
    this%ch(:) = 'undefined'
    !
  END SUBROUTINE init_att_ch
  !
  !! Initialisieren der Feld-Komponente "in" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_att_in ( this )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_att) , INTENT(INOUT) :: this   ! 
    !
    this%in(:) = HUGE( this%in )
    !
  END SUBROUTINE init_att_in
  !
  !! Initialisieren der Feld-Komponente "dp" mit Default-Werten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE init_att_dp ( this )
    !! Datenobjekt
    TYPE (t_att) , INTENT(INOUT) :: this   ! 
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER  :: c_upname='init_att_dp' !
    !
    this%dp(:) = HUGE( this%dp )
    !
  END SUBROUTINE init_att_dp
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren der dynamischen Feld-Komponente "ch" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_att_ch ( this )
    !! Datenobjekt
    TYPE (t_att) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER  :: c_upname='dealloc_att_ch' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%ch ) ) THEN
       DEALLOCATE ( this%ch, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5040, c_upname, c_modname, stat )
       NULLIFY ( this%ch ) 
    END IF
    !
  END SUBROUTINE dealloc_att_ch
  !
  !! De-Allokieren der dynamischen Feld-Komponente "in" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_att_in ( this )
    !! Datenobjekt
    TYPE (t_att) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER  :: c_upname='dealloc_att_in' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%in ) ) THEN
       DEALLOCATE ( this%in, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5050, c_upname, c_modname, stat )
       NULLIFY ( this%in ) 
    END IF
    !
  END SUBROUTINE dealloc_att_in
  !
  !! De-Allokieren der dynamischen Feld-Komponente "dp" <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_att_dp ( this )
    !! Datenobjekt
    TYPE (t_att) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=14), PARAMETER  :: c_upname='dealloc_att_dp' !
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ASSOCIATED( this%dp ) ) THEN
       DEALLOCATE ( this%dp, STAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 5060, c_upname, c_modname, stat )
       NULLIFY ( this%dp ) 
    END IF
    !
  END SUBROUTINE dealloc_att_dp
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe, ob die Komponente "id" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_att_id ( this ) &
       RESULT( ok )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=09) , PARAMETER :: c_upname='ok_att_id' ! 
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !
    ok = ( this%id > 0 )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
       WRITE(ctxt,'(I10)') this%id
       CALL setup_error_act ( '<AktId>', ctxt )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_att_id
  !
  !! Pr&uuml;fe, ob die Komponente "name" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_att_name ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=11) , PARAMETER :: c_upname='ok_att_name' ! 
    !
    ok = ( COUNT( get_lowercase_char(c_att_name(:)) == get_lowercase_char(this%name) ) == 1 )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
       CALL setup_error_act ( '<AktName>', TRIM( this%name ) )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_att_name
  !
  !! Pr&uuml;fe, ob die Komponente "var_id" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_att_var_id ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=13) , PARAMETER :: c_upname='ok_att_var_id' ! 
    !! Hilfsvariable
    CHARACTER (LEN=10) :: ctxt   ! 
    INTEGER            :: i, idx ! 
    !
    ok = ( this%var_id >= 0 )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
       WRITE(ctxt,'(I10)') this%var_id ; CALL setup_error_act ( '<AktVarId>', ctxt )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    ELSE
       idx = -1
       DO i=1,SIZE(c_att_name)
          IF ( idx > 0 ) EXIT
          IF ( get_lowercase_char(c_att_name(i)) == get_lowercase_char(this%name) ) idx = i
       END DO
       IF ( idx > 0 ) THEN
          IF ( this%var_id > 0 ) THEN
             ok = ( MAX(INDEX(c_att_cdg(idx),'C'),INDEX(c_att_cdg(idx),'D')) > 0 )
          ELSE
             ok = ( INDEX(c_att_cdg(idx),'G') > 0 )
          END IF
          IF ( .NOT. ok ) THEN
             CALL setup_error_act ( all_errors(:), 6031, c_upname, c_modname )
             WRITE(ctxt,'(I10)') this%var_id ; CALL setup_error_act ( '<AktVarId>', ctxt )
             CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
             CALL setup_error_act ( '<cdg>', TRIM(c_att_cdg(idx)) )
          END IF
       END IF
    END IF
    !
  END FUNCTION ok_att_var_id
  !
  !! Pr&uuml;fe, ob die Komponente "ch" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_att_ch ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=09) , PARAMETER :: c_upname='ok_att_ch' ! 
    !
    ok = .true. 
    IF ( ASSOCIATED( this%ch ) ) THEN
       ok = ALL( this%ch(:)(1:9) /= 'undefined' )
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6040, c_upname, c_modname )
          CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
       END IF
    END IF
    !
  END FUNCTION ok_att_ch
  !
  !! Pr&uuml;fe, ob die Komponente "in" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_att_in ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=09) , PARAMETER :: c_upname='ok_att_in' ! 
    !
    ok = .true. 
    IF ( ASSOCIATED( this%in ) ) THEN
       ok = ALL( this%in(:) < HUGE( this%in ) )
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6050, c_upname, c_modname )
          CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
       END IF
    END IF
    !
  END FUNCTION ok_att_in
  !
  !! Pr&uuml;fe, ob die Komponente "dp" eines Datenobjektes o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_att_dp ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=09) , PARAMETER :: c_upname='ok_att_dp' ! 
    !
    ok = .true. 
    IF ( ASSOCIATED( this%dp ) ) THEN
       ok = ALL( this%dp(:) < HUGE( this%dp ) )
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6060, c_upname, c_modname )
          CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
       END IF
    END IF
    !
  END FUNCTION ok_att_dp
  !
  !! Pr&uuml;fe, ob genau eine Feldkomponente allokiert wurde <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_att_alloc ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok                     ! 
    !! Name der Funktion
    CHARACTER (LEN=12) , PARAMETER :: c_upname='ok_att_alloc' ! 
    !! Hilfsfeld 
    LOGICAL :: l_ok(3)                ! 
    !! Hilfstext
    CHARACTER (LEN=10) :: ctxt        ! 
    !
    l_ok(1) = ASSOCIATED( this%ch )
    l_ok(2) = ASSOCIATED( this%in )
    l_ok(3) = ASSOCIATED( this%dp )
    ok      = ( COUNT( l_ok(:) ) == 1 )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6500, c_upname, c_modname )
       WRITE(ctxt,'(I10)') COUNT( l_ok(:) )
       CALL setup_error_act ( '<AktAlloc>', ctxt )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_att_alloc
  !
  !! Pr&uuml;fe, ob der Typ und Shape konsistent mit den Anforderungen sind <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_att_type ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok                     ! 
    !! Name der Funktion
    CHARACTER (LEN=11) , PARAMETER :: c_upname='ok_att_type' ! 
    !! Typ der Variablen
    CHARACTER (LEN=3) :: val1, val2 ! 
    !! Anzahl der in dem Attribut abgelegten Werte
    INTEGER           :: nof1, nof2 ! 
    !! Hilfsstring
    CHARACTER (LEN=10) :: ctxt ! 
    !
    ok  = .false. 
    val1 = get_att_type           ( this )
    nof1 = get_att_nof_values     ( this )
    val2 = get_req_att_type       ( this )
    nof2 = get_req_nof_att_values ( this )
    !
    IF ( val1 == val2 .OR. val2 == 'VA ' ) THEN
       IF ( nof2 >= 0 ) THEN
          IF ( nof1 == nof2 ) THEN
             ok = .true.
          ELSE
             CALL setup_error_act ( all_errors(:), 6502, c_upname, c_modname )
             WRITE(ctxt,'(I10)') nof1
             CALL setup_error_act ( '<AktValues>', ctxt )
             WRITE(ctxt,'(I10)') nof2
             CALL setup_error_act ( '<ReqValues>', ctxt )
             CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
          END IF
       ELSE
          IF ( nof1 > 0 ) THEN
             ok = .true. 
          ELSE
             CALL setup_error_act ( all_errors(:), 6503, c_upname, c_modname )
             CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
          END IF
       END IF
    ELSE
       CALL setup_error_act ( all_errors(:), 6501, c_upname, c_modname )
       CALL setup_error_act ( '<AktType>', val1 )
       CALL setup_error_act ( '<ReqType>', val2 )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_att_type
  !
  !! Pr&uuml;fe, ob alle Id's verschieden sind
  FUNCTION ok_att_different_id ( this ) &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! R&uuml;ckgabewert: Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='ok_att_different_id' ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = ( COUNT( this(:)%id == this(i)%id ) == 1 )
       IF ( .NOT. ok(i) ) THEN
          CALL setup_error_act ( all_errors(:), 6011, c_upname, c_modname )
          WRITE(ctxt,'(I10)') this(i)%id
          CALL setup_error_act ( '<AktId>', ctxt )
       END IF
    END DO
    !
  END FUNCTION ok_att_different_id
  !
  !! Pr&uuml;fe, ob alle Namen verschieden sind
  FUNCTION ok_att_different_name ( this ) &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! R&uuml;ckgabewert: Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Funktion
    CHARACTER (LEN=21) , PARAMETER :: c_upname='ok_att_different_name' ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = ( COUNT( this(:)%name   == this(i)%name .AND.      &
                        this(:)%var_id == this(i)%var_id ) == 1 )
       IF ( .NOT. ok(i) ) THEN
          CALL setup_error_act ( all_errors(:), 6021, c_upname, c_modname )
          CALL setup_error_act ( '<AktName>', TRIM( this(i)%name ) )
       END IF
    END DO
    !
  END FUNCTION ok_att_different_name
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OK-Methoden <<< Inhalt der Attribute wird geprueft 
  !                             [ERR_NO =  6600 bis  6699]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe, ob das Attribut (01) "title" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_title_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok !
    !! Lokale Parameter und Variablen
    CHARACTER (LEN=10) , PARAMETER :: c_upname='ok_title_0' ! 
    INTEGER                        :: i                     ! 
    !
    ok = .false.
    DO i=1,SIZE(this%ch)
       IF ( ok ) EXIT
       ok = ( LEN_TRIM(this%ch(i)) > 0 )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6615, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_title_0
  !
  !! Pr&uuml;fe, ob das Attribut (02) "history" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_history_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Lokale Parameter und Variablen
    CHARACTER (LEN=12) , PARAMETER :: c_upname='ok_history_0' ! 
    INTEGER                        :: i                       ! 
    !
    ok = .false.
    DO i=1,SIZE(this%ch)
       IF ( ok ) EXIT
       ok = ( LEN_TRIM(this%ch(i)) > 0 )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6615, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_history_0
  !
  !! Pr&uuml;fe, ob das Attribut (03) "beginning_date" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_beginning_date_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='ok_beginning_date_0' ! 
    !! Zeitangabe als Datentyp datetime
    TYPE (t_datetime)  :: ch_datetime(1)
    !
    ch_datetime(:) = get_att_ch_as_datetime( this )
    ok             = ok_datetime( ch_datetime(1) )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6601, c_upname, c_modname )
       CALL setup_error_act ( '<..name..>', this%name )
       CALL setup_error_act ( '<..date..>', datetime_to_string ( ch_datetime(1) ) )
    END IF
    !
  END FUNCTION ok_beginning_date_0
  !
  !! Pr&uuml;fe, ob das Attribut (04) "ending_date" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_ending_date_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='ok_ending_date_0' ! 
    !! Zeitangabe als Datentyp datetime
    TYPE (t_datetime)  :: ch_datetime(1)
    !
    ch_datetime(:) = get_att_ch_as_datetime(this)
    ok             = ok_datetime( ch_datetime(1) )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6601, c_upname, c_modname )
       CALL setup_error_act ( '<..name..>', this%name )
       CALL setup_error_act ( '<..date..>', datetime_to_string ( ch_datetime(1) ) )
    END IF
    !
  END FUNCTION ok_ending_date_0
  !
  !! Pr&uuml;fe, ob das Attribut (05) "reference_date" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_reference_date_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='ok_reference_date_0' ! 
    !! Zeitangabe als Datentyp datetime
    TYPE (t_datetime)  :: ch_datetime(1)
    !
    ch_datetime(:) = get_att_ch_as_datetime( this )
    ok             = ok_datetime( ch_datetime(1) )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6601, c_upname, c_modname )
       CALL setup_error_act ( '<..name..>', this%name )
       CALL setup_error_act ( '<..date..>', datetime_to_string ( ch_datetime(1) ) )
    END IF
    !
  END FUNCTION ok_reference_date_0
  !
  !! Pr&uuml;fe, ob das Attribut (06) "single_date" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_single_date_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='ok_single_date_0' ! 
    !! Zeitangabe als Datentyp datetime
    TYPE (t_datetime)  :: ch_datetime(1) 
    !
    ch_datetime(:) = get_att_ch_as_datetime( this )
    ok             = ok_datetime( ch_datetime(1) )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6601, c_upname, c_modname )
       CALL setup_error_act ( '<..name..>', this%name )
       CALL setup_error_act ( '<..date..>', datetime_to_string ( ch_datetime(1) ) )
    END IF
    !
  END FUNCTION ok_single_date_0
  !
  !! Pr&uuml;fe, ob das Attribut (07) "multiple_dates" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_multiple_dates_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='ok_multiple_dates_0' ! 
    !! Zeitangabe als Datentyp datetime
    TYPE (t_datetime)  :: ch_datetime(SIZE(this%ch)) 
    !! Zeitangabe als String
    CHARACTER (LEN=34) :: ctxt
    !! Z&auml;hler als String
    CHARACTER (LEN=15)  :: itxt
    !! Z&auml;hler
    INTEGER :: i
    !! Einzel-Testergebnisse
    LOGICAL :: ok_single(SIZE(this%ch))
    !
    ch_datetime(:) = get_att_ch_as_datetime( this )
    ok_single(:)   = ok_datetime( ch_datetime(:) )
    ok             = ALL( ok_single(:) )
    !
    IF ( .NOT. ok) THEN
       CALL setup_error_act ( all_errors(:), 6602, c_upname, c_modname )
       DO i = 1, SIZE(this%ch)
          IF ( .NOT. ok_single(i) ) CALL setup_error_act ( '<..multidates..>', datetime_to_string ( ch_datetime(i) ) )
       END DO
    ELSE
       ! wenn alle Angaben korrekt sind, dann jeweils 2 auf Chronologie pr&uuml;fen
       IF ( SIZE(this%ch) > 1 ) THEN
          DO i = 2, SIZE(this%ch)
             IF ( ch_datetime(i) <= ch_datetime(i-1) ) THEN
                ok = .false.
                WRITE(itxt,'(I5,A,I5)') i-1, ' und ', i
                CALL setup_error_act ( all_errors(:), 6603, c_upname, c_modname )
                CALL setup_error_act ( '<..lfdNrn..>', itxt )
                CALL setup_error_act ( '<..multidate1..>', datetime_to_string ( ch_datetime(i-1) ) )
                CALL setup_error_act ( '<..multidate2..>', datetime_to_string ( ch_datetime(i) ) )
             END IF
          END DO
       END IF
       !
    END IF
    !
  END FUNCTION ok_multiple_dates_0
  !
  !! Pr&uuml;fe, ob das Attribut (08) "horizontal_coordinate_system_definition" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_hor_coord_system_def_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=26) , PARAMETER :: c_upname='ok_hor_coord_system_name_0' ! 
    !! Hor.Koord.System als String
    CHARACTER (LEN=LEN(this%ch)) :: ctxt(1)
    !! alle erlaubten Hor.Koord.Systeme als ein langer String
    CHARACTER (LEN=3*LEN(this%ch)+2) :: cmaxtxt
    !! Z&auml;hler
    INTEGER :: i, i1
    !! Stringl&auml;nge
    INTEGER :: l
    !
    ok      = .false.
    ctxt(:) = get_att_ch( this )
    !
    DO i = 1, c_max_hcs
       IF ( ok ) EXIT
       IF ( LEN_TRIM(ctxt(1)) == LEN_TRIM(c_hor_coord_system(i)) ) &
          ok = ( TRIM(ctxt(1)) == TRIM(c_hor_coord_system(i)) )
    END DO
    !
    IF ( .NOT. ok ) THEN
       i1 = 1
       cmaxtxt = REPEAT(' ', LEN(cmaxtxt)) ! Initialisieren
       DO i = 1, c_max_hcs, 3
          l = LEN_TRIM(c_hor_coord_system(i))
          WRITE( cmaxtxt(i1:i1+l), '(A,A)') TRIM(c_hor_coord_system(i)), '|'
          i1 = i1 + l + 1
       END DO
       CALL setup_error_act ( all_errors(:), 6604, c_upname, c_modname )
       CALL setup_error_act ( '<..name..>', this%name )
       CALL setup_error_act ( '<..aktuell..>', TRIM(ctxt(1)) )
       CALL setup_error_act ( '<..erlaubt..>', TRIM(cmaxtxt) ) 
    END IF
    !
  END FUNCTION ok_hor_coord_system_def_0
  !
  !! Pr&uuml;fe, ob das Attribut (09) "map_projection" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_map_projection_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='ok_map_projection_0' ! 
    !! akt.MapProjection als String
    CHARACTER (LEN=LEN(this%ch)) :: ctxt(1)
    !! alle erlaubten MapProjectionen als ein langer String
    CHARACTER (LEN=2*LEN(this%ch)+1) :: cmaxtxt
    !! Z&auml;hler
    INTEGER :: i, i1
    !! Stringl&auml;nge
    INTEGER :: l
    !
    ok      = .false.
    ctxt(:) = get_att_ch( this )
    !
    DO i = 1, c_max_map_proj
       IF ( ok ) EXIT
       IF ( LEN_TRIM(ctxt(1)) == LEN_TRIM(c_map_proj(i)) ) &
            ok = ( TRIM(ctxt(1)) == TRIM(c_map_proj(i)) ) 
    END DO
    !
    IF ( .NOT. ok ) THEN
       i1 = 1
       cmaxtxt = REPEAT(' ', LEN(cmaxtxt)) ! Initialisieren
       DO i = 1, c_max_map_proj
          l = LEN_TRIM(c_map_proj(i))
          WRITE( cmaxtxt(i1:i1+l), '(A,A)') TRIM(c_map_proj(i)), '|'
          i1 = i1 + l + 1
       END DO
       CALL setup_error_act ( all_errors(:), 6604, c_upname, c_modname )
       CALL setup_error_act ( '<..name..>', this%name )
       CALL setup_error_act ( '<..aktuell..>', TRIM(ctxt(1)) )
       CALL setup_error_act ( '<..erlaubt..>', TRIM(cmaxtxt) ) 
    END IF
    !
  END FUNCTION ok_map_projection_0
  !
  !! Pr&uuml;fe, ob das Attribut (10) "horizontal_datum_name" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_horizontal_datum_name_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=26) , PARAMETER :: c_upname='ok_horizontal_datum_name_0' ! 
    !! akt.HorDatumName als String
    CHARACTER (LEN=c_len_att_ch) :: ctxt(1) ! 
    !! alle erlaubten Angaben als ein langer String
    CHARACTER (LEN=15) :: cmaxtxt
    !! Z&auml;hler
    INTEGER :: i, i1
    !! Stringl&auml;nge
    INTEGER :: l
    !
    ok      = .false.
    ctxt(:) = get_att_ch( this )
    !
    DO i = 1, c_max_hdn
       IF ( ok ) EXIT
       IF ( LEN_TRIM(ctxt(1)) == LEN_TRIM(c_hor_datum_name(i)) ) &
            ok = ( TRIM(ctxt(1)) == TRIM(c_hor_datum_name(i)) )
    END DO
    !
    IF ( .NOT. ok ) THEN
       i1 = 1
       cmaxtxt = REPEAT(' ', LEN(cmaxtxt)) ! Initialisieren
       DO i = 1, c_max_hdn
          l = LEN_TRIM(c_hor_datum_name(i))
          WRITE( cmaxtxt(i1:i1+l), '(A,A)') TRIM(c_hor_datum_name(i)), '|'
          i1 = i1 + l + 1
       END DO
       CALL setup_error_act ( all_errors(:), 6604, c_upname, c_modname )
       CALL setup_error_act ( '<..name..>', this%name )
       CALL setup_error_act ( '<..hordatname..>', TRIM(ctxt(1)) )
       CALL setup_error_act ( '<..erlaubt..>', TRIM(cmaxtxt) ) 
    END IF
    !
  END FUNCTION ok_horizontal_datum_name_0
  !
  !! Pr&uuml;fe, ob das Attribut (11) "ellipsoid_name" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_ellipsoid_name_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='ok_ellipsoid_name_0' ! 
    !! akt.Ellipsoid als String
    CHARACTER (LEN=c_len_att_ch) :: ctxt(1) ! 
    !! alle erlaubten Ellipsoide als ein langer String
    CHARACTER (LEN=13) :: cmaxtxt
    !! Z&auml;hler
    INTEGER :: i, i1
    !! Stringl&auml;nge
    INTEGER :: l
    !
    ok      = .false.
    ctxt(:) = get_att_ch( this )
    !
    DO i = 1, c_max_elli_name
       IF ( ok ) EXIT
       IF ( LEN_TRIM(ctxt(1)) == LEN_TRIM(c_elli_name(i)) ) &
            ok = ( TRIM(ctxt(1)) == TRIM(c_elli_name(i)) )
    END DO
    !
    IF ( .NOT. ok ) THEN
       i1 = 1
       cmaxtxt = REPEAT(' ', LEN(cmaxtxt)) ! Initialisieren
       DO i = 1, c_max_elli_name
          l = LEN_TRIM(c_elli_name(i))
          WRITE( cmaxtxt(i1:i1+l), '(A,A)') TRIM(c_elli_name(i)), '|'
          i1 = i1 + l + 1
       END DO
       CALL setup_error_act ( all_errors(:), 6604, c_upname, c_modname )
       CALL setup_error_act ( '<..name..>', this%name )
       CALL setup_error_act ( '<..aktuell..>', TRIM(ctxt(1)) )
       CALL setup_error_act ( '<..erlaubt..>', TRIM(cmaxtxt) ) 
    END IF
    !
  END FUNCTION ok_ellipsoid_name_0
  !
  !! Pr&uuml;fe, ob das Attribut (12) "longitude_of_central_meridian" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_longitude_of_cen_meri_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=26) , PARAMETER :: c_upname='ok_longitude_of_cen_meri_0'    !  
    !! untere und obere Grenzwerte f&uuml;r die aktuelle Komponente
    REAL (KIND=Double) , PARAMETER :: c_low=-180.0_Double, c_high=180.0_Double ! 
    !! akt.Wert als Real
    REAL (KIND=Double) :: longi(1)
    !! akt.Wert als String
    CHARACTER (LEN=15) :: ctxt
    !
    longi = get_att_dp( this )
    ok    = ( (c_low .LE. longi(1)) .AND. (longi(1) .LT. c_high) ) ! Vgl-Operatoren beachten
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6605, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
       CALL setup_error_act ( '<op1>', '<=' )
       CALL setup_error_act ( '<op2>', '< ' )
       WRITE(ctxt,'(G15.9)') c_low    ; CALL setup_error_act ( '<zul-wert-1>', ctxt )
       WRITE(ctxt,'(G15.9)') c_high   ; CALL setup_error_act ( '<zul-wert-2>', ctxt )
       WRITE(ctxt,'(G15.9)') longi(1) ; CALL setup_error_act ( '<wert>', ctxt       )
    END IF
    !
  END FUNCTION ok_longitude_of_cen_meri_0
  !
  !! Pr&uuml;fe, ob das Attribut (13) "false_easting" o.k. ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_false_easting_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .true. ! derzeit kein Test
    !
  END FUNCTION ok_false_easting_0
  !
  !! Pr&uuml;fe, ob das Attribut (14) "false_northing" o.k. ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_false_northing_0 ( this ) &
       RESULT( ok )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .true. ! derzeit kein Test
    !
  END FUNCTION ok_false_northing_0
  !
  !! Pr&uuml;fe, ob das Attribut (15) "west_bounding_coordinate" o.k. ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_west_bounding_coord_0 ( this ) &
       RESULT( ok )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .true. ! derzeit kein Test
    !
  END FUNCTION ok_west_bounding_coord_0
  !
  !! Pr&uuml;fe, ob das Attribut (16) "east_bounding_coordinate" o.k. ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_east_bounding_coord_0 ( this ) &
       RESULT( ok )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .true. ! derzeit kein Test
    !
  END FUNCTION ok_east_bounding_coord_0
  !
  !! Pr&uuml;fe, ob das Attribut (17) "north_bounding_coordinate" o.k. ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_north_bounding_coord_0 ( this ) &
       RESULT( ok )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .true. ! derzeit kein Test
    !
  END FUNCTION ok_north_bounding_coord_0
  !
  !! Pr&uuml;fe, ob das Attribut (18) "south_bounding_coordinate" o.k. ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_south_bounding_coord_0 ( this ) &
       RESULT( ok )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .true. ! derzeit kein Test
    !
  END FUNCTION ok_south_bounding_coord_0
  !
  !! Pr&uuml;fe, ob das Attribut (19) "vertical_coordinate_system_definition" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_vert_coord_system_def_0 ( this ) &
       RESULT( ok )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=26) , PARAMETER :: c_upname='ok_vert_coord_system_def_0' ! 
    !! akt.VertKoordSystem als String
    CHARACTER (LEN=LEN(this%ch)) :: ctxt(1)
    !! alle erlaubten Angaben als ein langer String
    CHARACTER (LEN=2*LEN(this%ch)+1) :: cmaxtxt
    !! Z&auml;hler
    INTEGER :: i, i1
    !! Stringl&auml;nge
    INTEGER :: l
    !
    ok      = .false.
    ctxt(:) = get_att_ch( this )
    !
    DO i = 1, c_max_vcs
       IF ( ok ) EXIT
       IF ( LEN_TRIM(ctxt(1)) == LEN_TRIM(c_ver_coord_system(i)) ) &
            ok = ( TRIM(ctxt(1)) == TRIM(c_ver_coord_system(i)) )
    END DO
    !
    IF ( .NOT. ok ) THEN
       i1 = 1
       cmaxtxt = REPEAT(' ', LEN(cmaxtxt)) ! Initialisieren
       DO i = 1, c_max_vcs
          l = LEN_TRIM(c_ver_coord_system(i))
          WRITE( cmaxtxt(i1:i1+l), '(A,A)') TRIM(c_ver_coord_system(i)), '|'
          i1 = i1 + l + 1
       END DO
       CALL setup_error_act ( all_errors(:), 6604, c_upname, c_modname )
       CALL setup_error_act ( '<..name..>', this%name )
       CALL setup_error_act ( '<..aktuell..>', TRIM(ctxt(1)) )
       CALL setup_error_act ( '<..erlaubt..>', TRIM(cmaxtxt) ) 
    END IF
    !
  END FUNCTION ok_vert_coord_system_def_0
  !
  !! Pr&uuml;fe, ob das Attribut (20) "altitude_datum_name" o.k. ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_altitude_datum_name_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .true. ! derzeit kein Test
    !
  END FUNCTION ok_altitude_datum_name_0
  !
  !! Pr&uuml;fe, ob das Attribut (21) "depth_datum_name" o.k. ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_depth_datum_name_0 ( this ) &
       RESULT( ok )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .true. ! derzeit kein Test
    !
  END FUNCTION ok_depth_datum_name_0
  !
  !! Pr&uuml;fe, ob das Attribut (22) "format_name" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_format_name_0 ( this ) &
       RESULT( ok )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='ok_format_name_0' ! 
    !! aktueller FormatName als String
    CHARACTER (LEN=LEN(this%ch)) :: ctxt(1)
    !
    ctxt = get_att_ch( this )
    ok   = ok_file_type( ctxt(1) )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6606, c_upname, c_modname )
       CALL setup_error_act ( '<..name..>', this%name )
       CALL setup_error_act ( '<..aktuell..>', ctxt(1) ) 
    END IF
    !
  END FUNCTION ok_format_name_0
  !
  !! Pr&uuml;fe, ob das Attribut (23) "format_version_no" o.k. ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_format_version_no_0 ( this ) &
       RESULT( ok )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .true. ! derzeit kein Test
    !
  END FUNCTION ok_format_version_no_0
  !
  !! Pr&uuml;fe, ob das Attribut (24) "theme_keywords" o.k. ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_theme_keywords_0 ( this ) &
       RESULT( ok )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .true. ! derzeit kein Test
    !
  END FUNCTION ok_theme_keywords_0
  !
  !! Pr&uuml;fe, ob das Attribut (25) "place_keywords" o.k. ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_place_keywords_0 ( this ) &
       RESULT( ok )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .true. ! derzeit kein Test
    !
  END FUNCTION ok_place_keywords_0
  !
  !! Pr&uuml;fe, ob das Attribut (26) "temporal_keywords" o.k. ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_temporal_keywords_0 ( this ) &
       RESULT( ok )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .true. ! derzeit kein Test
    !
  END FUNCTION ok_temporal_keywords_0
  !
  !! Pr&uuml;fe, ob das Attribut (27) "related_file_name" o.k. ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_related_file_name_0 ( this ) &
       RESULT( ok )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .true. ! derzeit kein Test
    !
  END FUNCTION ok_related_file_name_0
  !
  !! Pr&uuml;fe, ob das Attribut (28) "related_file_form" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_related_file_form_0 ( this ) &
       RESULT( ok )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=22) , PARAMETER :: c_upname='ok_related_file_form_0' ! 
    !! aktuelle FileForm als String
    CHARACTER (LEN=LEN(this%ch)) :: ctxt(1)
    !! Einzel-Testergebnisse f&uuml;r FORMATTED und UNFORMATTED
    LOGICAL :: ok_1, ok_2
    !
    ctxt = get_att_ch( this )
    ok_2 = file_is_unformatted (ctxt(1))
    ok_1 = file_is_formatted   (ctxt(1))
    ok   = ( ok_1 .OR. ok_2 )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6604, c_upname, c_modname )
       CALL setup_error_act ( '<..name..>', this%name )
       CALL setup_error_act ( '<..aktuell..>', ctxt(1) ) 
       CALL setup_error_act ( '<..erlaubt..>', 'FORMATTED oder UNFORMATTED' ) 
    END IF
    !
  END FUNCTION ok_related_file_form_0
  !
  !! Pr&uuml;fe, ob das Attribut (29) "related_file_access" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_related_file_access_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=24) , PARAMETER :: c_upname='ok_related_file_access_0' ! 
    !! aktueller FileAccess als String
    CHARACTER (LEN=LEN(this%ch)) :: ctxt(1)
    !! Einzel-Testergebnisse f&uuml;r DIRECT und SEQUENTIAL
    LOGICAL :: ok_1, ok_2
    !
    ctxt = get_att_ch( this )
    ok_1 = file_is_direct (ctxt(1))
    ok_2 = file_is_sequential (ctxt(1))
    ok   = ( ok_1 .OR. ok_2 )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6607, c_upname, c_modname )
       CALL setup_error_act ( '<..name..>', this%name )
       CALL setup_error_act ( '<..aktuell..>', ctxt(1)) 
       CALL setup_error_act ( '<..erlaubt..>', 'DIRECT oder SEQUENTIAL' ) 
    END IF
    !
  END FUNCTION ok_related_file_access_0
  !
  !! Pr&uuml;fe, ob das Attribut (30) "related_file_type" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_related_file_type_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=22) , PARAMETER :: c_upname='ok_related_file_type_0' ! 
    !! aktueller FileType als String
    CHARACTER (LEN=LEN(this%ch)) :: ctxt(1)
    !
    ctxt = get_att_ch   ( this    )
    ok   = ok_file_type ( ctxt(1) )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6606, c_upname, c_modname )
       CALL setup_error_act ( '<..name..>', this%name ) 
       CALL setup_error_act ( '<..aktuell..>', ctxt(1) ) 
    END IF
    !
  END FUNCTION ok_related_file_type_0
  !
  !! Pr&uuml;fe, ob das Attribut (31) "reference_location_index" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_reference_location_index_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=29) , PARAMETER :: c_upname='ok_reference_location_index_0' ! 
    !! akt.Wert als Real
    INTEGER :: locindex(1)
    !! akt.Wert als String
    CHARACTER (LEN=10) :: ctxt
    !
    locindex(:) = get_att_in( this )
    ok          = ( locindex(1) .GT. 0 )
    !
    IF ( .NOT. ok ) THEN
       WRITE( ctxt, '(I10)') locindex(1)
       CALL setup_error_act ( all_errors(:), 6607, c_upname, c_modname )
       CALL setup_error_act ( '<..name..>', this%name )
       CALL setup_error_act ( '<..aktuell..>', ctxt )
    END IF
    !
  END FUNCTION ok_reference_location_index_0
  !
  !! Pr&uuml;fe, ob das Attribut (32) "reference_location_coordinates" o.k. ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_reference_location_coord_0 ( this ) &
       RESULT( ok )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .true. ! derzeit kein Test
    !
  END FUNCTION ok_reference_location_coord_0
  !
  !! Pr&uuml;fe, ob das Attribut (33) "zero_phase_location_index" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_zero_phase_location_index_0 ( this ) &
       RESULT( ok )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=30) , PARAMETER :: c_upname='ok_zero_phase_location_index_0' ! 
    !! akt.Wert als Real
    INTEGER :: locindex(1)
    !! akt.Wert als String
    CHARACTER (LEN=10) :: ctxt
    !
    locindex(:) = get_att_in( this )
    ok = ( locindex(1) .GT. 0 )
    !
    IF ( .NOT. ok ) THEN
       WRITE( ctxt, '(I10)') locindex(1)
       CALL setup_error_act ( all_errors(:), 6607, c_upname, c_modname )
       CALL setup_error_act ( '<..name..>', this%name )
       CALL setup_error_act ( '<..aktuell..>', ctxt )
    END IF
    !
  END FUNCTION ok_zero_phase_location_index_0
  !
  !! Pr&uuml;fe, ob das Attribut (34) "zero_phase_location_coordinates" o.k. ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_zero_phase_location_coord_0 ( this ) &
       RESULT( ok )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .true. ! derzeit kein Test
    !
  END FUNCTION ok_zero_phase_location_coord_0
  !
  !! Pr&uuml;fe, ob das Attribut (35) "sigma_interfaces" o.k. ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_sigma_interfaces_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=21) , PARAMETER :: c_upname='ok_sigma_interfaces_0' ! 
    !! lokale Variable
    INTEGER :: n  ! 
    !
    ok = .true.
    n  = get_att_nof_values(this)
    IF ( n > 1 ) THEN
       ok = ( ALL( this%dp(1:n-1)-this%dp(2:n) >= 0.0_Double ) .OR. &
              ALL( this%dp(1:n-1)-this%dp(2:n) <= 0.0_Double ) )
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6614, c_upname, c_modname )
          CALL setup_error_act ( '<AktAttName>', TRIM(this%name)          )
          CALL setup_error_act ( '<LayerTyp>', 'sigma_interfaces' )
       END IF
    END IF
    !
  END FUNCTION ok_sigma_interfaces_0
  !
  !! Pr&uuml;fe, ob das Attribut (36) "layer_interfaces" o.k. ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_layer_interfaces_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=21) , PARAMETER :: c_upname='ok_layer_interfaces_0' ! 
    !! lokale Variable
    INTEGER :: n  ! 
    !
    ok = .true.
    n  = get_att_nof_values(this)
    ! Das erste Element wird ausgespart, weil dort die tiefste Modelltiefe steht
    IF ( n > 2 ) THEN
       ok = ( ALL( this%dp(2:n-1)-this%dp(3:n) >= 0.0_Double ) .OR. &
              ALL( this%dp(2:n-1)-this%dp(3:n) <= 0.0_Double ) )
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6614, c_upname, c_modname )
          CALL setup_error_act ( '<AktAttName>', TRIM(this%name)          )
          CALL setup_error_act ( '<LayerTyp>', 'z-layer_interfaces' )
       END IF
    END IF
    !
  END FUNCTION ok_layer_interfaces_0
  !
  !! Pr&uuml;fe, ob das Attribut (37) "minimum_water_depth" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_minimum_water_depth_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=24) , PARAMETER :: c_upname='ok_minimum_water_depth_0' ! 
    !! akt.Wert als Real
    REAL (KIND=Double) :: mini(1)
    !! akt.Wert als String
    CHARACTER (LEN=15) :: ctxt
    !
    mini(:) = get_att_dp( this )
    ok      = ( mini(1) .GE. 0.0_Double )
    !
    IF ( .NOT. ok ) THEN
       WRITE( ctxt, '(G15.9)') mini(1)
       CALL setup_error_act ( all_errors(:), 6608, c_upname, c_modname )
       CALL setup_error_act ( '<..name..>', this%name )
       CALL setup_error_act ( '<..aktuell..>', ctxt )
    END IF
    !
  END FUNCTION ok_minimum_water_depth_0
  !
  !! Pr&uuml;fe, ob das Attribut (57) "dynamic_bathymetry" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_dynamic_bathymetry_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this       ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL                   :: ok         ! 
    !! Name der Funktion
    CHARACTER (LEN=23) , PARAMETER :: c_upname='ok_dynamic_bathymetry_0' ! 
    !! akt.Wert als Integer
    INTEGER                   :: imorpho(1) ! 
    !! akt.Wert als String
    CHARACTER (LEN=10)        :: ctxt       ! 
    !
    imorpho(:) = get_att_in( this )
    ok         = ( 0 <= imorpho(1) .AND. imorpho(1) <= 1 )
    !
    IF ( .NOT. ok ) THEN
       WRITE( ctxt, '(I10)') imorpho(1)
       CALL setup_error_act ( all_errors(:), 6612, c_upname, c_modname )
       CALL setup_error_act ( '<..name..>', this%name )
       CALL setup_error_act ( '<..aktuell..>', ctxt )
    END IF
    !
  END FUNCTION ok_dynamic_bathymetry_0
  !
  !! Pr&uuml;fe, ob das Attribut (38) "long_name" o.k. ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_long_name_0 ( this ) &
       RESULT( ok )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this !  
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL                   :: ok   !  
    !! Name der Programmeinheit
    CHARACTER (LEN=14), PARAMETER :: c_upname='ok_long_name_0' ! 
    INTEGER                       :: i ! 
    !
    ok = .false.
    DO i=1,SIZE(this%ch)
       IF ( ok ) EXIT
       ok = ( LEN_TRIM(this%ch(i)) > 0 )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6615, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_long_name_0
  !
  !! Pr&uuml;fe, ob das Attribut (39) "short_name" o.k. ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_short_name_0 ( this ) &
       RESULT( ok )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=15), PARAMETER :: c_upname='ok_short_name_0' ! 
    INTEGER                       :: i ! 
    !
    ok = .false.
    DO i=1,SIZE(this%ch)
       IF ( ok ) EXIT
       ok = ( LEN_TRIM(this%ch(i)) > 0 )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6615, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_short_name_0
  !
  !! Pr&uuml;fe, ob das Attribut (40) "units" o.k. ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_units_0 ( this ) &
       RESULT( ok )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=10), PARAMETER :: c_upname='ok_units_0' ! 
    INTEGER                       :: i ! 
    !
    ok = .false.
    DO i=1,SIZE(this%ch)
       IF ( ok ) EXIT
       ok = ( LEN_TRIM(this%ch(i)) > 0 )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6615, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_units_0
  !
  !! Pr&uuml;fe, ob das Attribut (41) "name_id" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_name_id_0 ( this ) &
       RESULT( ok )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=12) , PARAMETER :: c_upname='ok_name_id_0' ! 
    !! akt.Wert als Integer
    INTEGER :: nameid(1)
    !! akt.Wert als String
    CHARACTER (LEN=10) :: ctxt
    !
    nameid(:) = get_att_in( this )
    ok        = ( nameid(1) .GT. 0 )
    !
    IF ( .NOT. ok ) THEN
       WRITE( ctxt, '(I10)') nameid(1)
       CALL setup_error_act ( all_errors(:), 6607, c_upname, c_modname )
       CALL setup_error_act ( '<..name..>', this%name )
       CALL setup_error_act ( '<..aktuell..>', ctxt )
    END IF
    !
  END FUNCTION ok_name_id_0
  !
  !! Pr&uuml;fe, ob das Attribut (42) "time_id" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_time_id_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=12) , PARAMETER :: c_upname='ok_time_id_0' ! 
    !! akt.Wert als Integer
    INTEGER :: timeid(1)
    !! akt.Wert als String
    CHARACTER (LEN=10) :: ctxt
    !
    timeid(:) = get_att_in( this )
    ok        = ( (0 .LE. timeid(1)) .AND. (timeid(1) .LE. 3) )
    !
    IF ( .NOT. ok ) THEN
       WRITE( ctxt, '(I10)') timeid(1)
       CALL setup_error_act ( all_errors(:), 6609, c_upname, c_modname )
       CALL setup_error_act ( '<..name..>', this%name )
       CALL setup_error_act ( '<..erlaubt..>', '0, 1 und 2' )
       CALL setup_error_act ( '<..aktuell..>', ctxt )
    END IF
    !
  END FUNCTION ok_time_id_0
  !
  !! Pr&uuml;fe, ob das Attribut (43) "class_id" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_class_id_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=13) , PARAMETER :: c_upname='ok_class_id_0' ! 
    !! akt.Wert als Integer
    INTEGER :: classid(1)
    !! akt.Wert als String
    CHARACTER (LEN=10) :: ctxt
    !
    classid(:) = get_att_in( this )
    ok         = ( (1 .LE. classid(1)) .AND. (classid(1) .LE. 16) )
    !
    IF ( .NOT. ok ) THEN
       WRITE( ctxt, '(I10)') classid(1)
       CALL setup_error_act ( all_errors(:), 6609, c_upname, c_modname )
       CALL setup_error_act ( '<..name..>', this%name )
       CALL setup_error_act ( '<..erlaubt..>', '1, 2, ... 16' )
       CALL setup_error_act ( '<..aktuell..>', ctxt )
    END IF
    !
  END FUNCTION ok_class_id_0
  !
  !! Pr&uuml;fe, ob das Attribut (44) "plot_id" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_plot_id_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=12) , PARAMETER :: c_upname='ok_plot_id_0' ! 
    !! akt.Wert als Integer
    INTEGER :: plotid(1)
    !! akt.Wert als String
    CHARACTER (LEN=10) :: ctxt
    !
    plotid(:) = get_att_in( this )
    ok        = ( (0 .LE. plotid(1)) .AND. (plotid(1) .LE. 7) )
    !
    IF ( .NOT. ok ) THEN
       WRITE( ctxt, '(I10)') plotid(1)
       CALL setup_error_act ( all_errors(:), 6609, c_upname, c_modname )
       CALL setup_error_act ( '<..name..>', this%name )
       CALL setup_error_act ( '<..erlaubt..>', '0, 1, ... 7' )
       CALL setup_error_act ( '<..aktuell..>', ctxt )
    END IF
    !
  END FUNCTION ok_plot_id_0
  !
  !! Pr&uuml;fe, ob das Attribut (45) "ref_name_id" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_ref_name_id_0 ( this ) &
       RESULT( ok )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='ok_ref_name_id_0' ! 
    !! akt.Wert als Integer
    INTEGER :: refid(1)
    !! akt.Wert als String
    CHARACTER (LEN=10) :: ctxt
    !
    refid = get_att_in( this )
    ok    = ( refid(1) .GE. 0 )
    !
    IF ( .NOT. ok ) THEN
       WRITE( ctxt, '(I10)') refid(1)
       CALL setup_error_act ( all_errors(:), 6610, c_upname, c_modname )
       CALL setup_error_act ( '<..refid..>', ctxt )
    END IF
    !
  END FUNCTION ok_ref_name_id_0
  !
  !! Pr&uuml;fe, ob das Attribut (46) "scale_factor" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_scale_factor_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='ok_scale_factor_0' ! 
    !! untere und obere Grenzwerte f&uuml;r die aktuelle Komponente
    REAL (KIND=Double) , PARAMETER :: c_low=-1.0E+9_Double, c_high=1.0E+9_Double ! 
    !! akt.Wert als Real
    REAL (KIND=Double) :: wert(1) ! 
    !! akt.Wert als String
    CHARACTER (LEN=15) :: ctxt     ! 
    !
    wert(:) = 0.0_Double
    IF ( is_att_dp(this) ) wert(:) = get_att_dp(this)
    IF ( is_att_in(this) ) wert(:) = REAL(get_att_in(this),Double)
    ok      = .false.
    ok      = MERGE( .true., ok, c_low      <= wert(1) .AND. wert(1) < 0.0_Double )
    ok      = MERGE( .true., ok, 0.0_Double <  wert(1) .AND. wert(1) <= c_high    )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6605, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name)//' [ /= 0 ]' )
       CALL setup_error_act ( '<op1>', '<=' )
       CALL setup_error_act ( '<op2>', '<=' )
       WRITE(ctxt,'(G15.9)') c_low   ; CALL setup_error_act ( '<zul-wert-1>', ctxt )
       WRITE(ctxt,'(G15.9)') c_high  ; CALL setup_error_act ( '<zul-wert-2>', ctxt )
       WRITE(ctxt,'(G15.9)') wert(1) ; CALL setup_error_act ( '<wert>', ctxt       )
    END IF
    !
  END FUNCTION ok_scale_factor_0
  !
  !! Pr&uuml;fe, ob das Attribut (47) "valid_range" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_valid_range_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='ok_valid_range_0' ! 
    !! akt.Wert als Real
    REAL (KIND=Double) :: valid(2)
    !! akt.Wert als String
    CHARACTER (LEN=15) :: ctxt
    !
    valid(:) = 0.0_Double
    IF ( is_att_dp(this) ) valid(:) = get_att_dp(this)
    IF ( is_att_in(this) ) valid(:) = REAL(get_att_in(this),Double)
    ok       = ( valid(1) < valid(2) )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6611, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
       CALL setup_error_act ( '<op1>', '<' )
       WRITE(ctxt,'(G15.9)') valid(1) ; CALL setup_error_act ( '<wert-1>', ctxt )
       WRITE(ctxt,'(G15.9)') valid(2) ; CALL setup_error_act ( '<wert-2>', ctxt )
    END IF
    !
  END FUNCTION ok_valid_range_0
  !
  !! Pr&uuml;fe, ob das Attribut (48) "actual_range" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_actual_range_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='ok_actual_range_0' ! 
    !! akt.Wert als Real
    REAL (KIND=Double) :: actual(2)
    !! akt.Wert als String
    CHARACTER (LEN=15) :: ctxt
    !
    actual(:) = 0.0_Double
    IF ( is_att_dp(this) ) actual(:) = get_att_dp(this)
    IF ( is_att_in(this) ) actual(:) = REAL(get_att_in(this),Double)
    ok        = ( actual(1) < actual(2) )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6611, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
       CALL setup_error_act ( '<op1>', '<' )
       WRITE(ctxt,'(G15.9)') actual(1) ; CALL setup_error_act ( '<wert-1>', ctxt )
       WRITE(ctxt,'(G15.9)') actual(2) ; CALL setup_error_act ( '<wert-2>', ctxt )
    END IF
    !
  END FUNCTION ok_actual_range_0
  !
  !! Pr&uuml;fe, ob das Attribut (49) "missing_value" o.k. ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_missing_value_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .true. ! derzeit kein Test
    !
  END FUNCTION ok_missing_value_0
  !
  !! Pr&uuml;fe, ob das Attribut (50) "add_offset" o.k. ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_add_offset_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .true. ! derzeit kein Test
    !
  END FUNCTION ok_add_offset_0
  !
  !! Pr&uuml;fe, ob das Attribut (51) "_FillValue" o.k. ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok__FillValue_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .true. ! derzeit kein Test
    !
  END FUNCTION ok__FillValue_0
  !
  !! Pr&uuml;fe, ob das Attribut (52) "FORTRAN_format" o.k. ist <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_FORTRAN_format_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=19), PARAMETER :: c_upname='ok_FORTRAN_format_0' ! 
    INTEGER                       :: i ! 
    !
    ok = .false.
    DO i=1,SIZE(this%ch)
       IF ( ok ) EXIT
       ok = ( LEN_TRIM(this%ch(i)) > 0 )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6615, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_FORTRAN_format_0
  !
  !! Pr&uuml;fe, ob das Attribut (53) "horizontal_discretisation" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_horizontal_discret_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=23) , PARAMETER :: c_upname='ok_horizontal_discret_0' ! 
    !! akt.HorDiscret als String
    CHARACTER (LEN=LEN(this%ch)) :: ctxt(1)
    !! alle erlaubten Angaben als ein langer String
    CHARACTER (LEN=4*(LEN(this%ch)+1)) :: cmaxtxt
    !! Z&auml;hler
    INTEGER :: i, i1
    !! Stringl&auml;nge
    INTEGER :: l
    !
    ok      = .false.
    ctxt(:) = get_att_ch( this )
    !
    DO i = 1, c_max_hdisc
       IF ( ok ) EXIT
       IF ( LEN_TRIM(ctxt(1)) == LEN_TRIM(c_hor_disc(i)) ) &
            ok = ( TRIM(ctxt(1)) == TRIM(c_hor_disc(i)) )
    END DO
    !
    IF ( .NOT. ok ) THEN
       i1 = 1
       cmaxtxt = REPEAT(' ', LEN(cmaxtxt)) ! Initialisieren
       DO i = 1, c_max_hdisc
          l = LEN_TRIM(c_hor_disc(i))
          WRITE( cmaxtxt(i1:i1+l), '(A,A)') TRIM(c_hor_disc(i)), '|'
          i1 = i1 + l + 1
       END DO
       CALL setup_error_act ( all_errors(:), 6604, c_upname, c_modname )
       CALL setup_error_act ( '<..name..>', this%name )
       CALL setup_error_act ( '<..aktuell..>', TRIM(ctxt(1)) )
       CALL setup_error_act ( '<..erlaubt..>', TRIM(cmaxtxt)) 
    END IF
    !
  END FUNCTION ok_horizontal_discret_0
  !
  !! Pr&uuml;fe, ob das Attribut (54) "vertical_discretisation" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_vertical_discret_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=23) , PARAMETER :: c_upname='ok_vertical_discret_0' ! 
    !! akt.VertDiscret als String
    CHARACTER (LEN=LEN(this%ch)) :: ctxt(1)
    !! alle erlaubten Angaben als ein langer String
    CHARACTER (LEN=3*(LEN(this%ch)+1)) :: cmaxtxt
    !! Z&auml;hler
    INTEGER :: i, i1
    !! Stringl&auml;nge
    INTEGER :: l
    !
    ok      = .false.
    ctxt(:) = get_att_ch( this )
    !
    DO i = 1, c_max_vdisc
       IF ( ok ) EXIT
       IF ( LEN_TRIM(ctxt(1)) == LEN_TRIM(c_ver_disc(i)) ) &
            ok = ( TRIM(ctxt(1)) == TRIM(c_ver_disc(i)) )
    END DO
    !
    IF ( .NOT. ok ) THEN
       i1 = 1
       cmaxtxt = REPEAT(' ', LEN(cmaxtxt)) ! Initialisieren
       DO i = 1, c_max_vdisc
          l = LEN_TRIM(c_ver_disc(i))
          WRITE( cmaxtxt(i1:i1+l), '(A,A)') TRIM(c_ver_disc(i)), '|'
          i1 = i1 + l + 1
       END DO
       CALL setup_error_act ( all_errors(:), 6604, c_upname, c_modname )
       CALL setup_error_act ( '<..name..>', this%name )
       CALL setup_error_act ( '<..aktuell..>', TRIM(ctxt(1)) )
       CALL setup_error_act ( '<..erlaubt..>', TRIM(cmaxtxt)) 
    END IF
    !
  END FUNCTION ok_vertical_discret_0
  !
  !! Pr&uuml;fe, ob das Attribut (55) "storage_type" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_storage_type_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=17) , PARAMETER :: c_upname='ok_storage_type_0' ! 
    !! akt.StorageType als String
    CHARACTER (LEN=LEN(this%ch)) :: ctxt(1)
    !! alle erlaubten Angaben als ein langer String
    CHARACTER (LEN=6*(LEN(this%ch)+1)) :: cmaxtxt
    !! Z&auml;hler
    INTEGER :: i, i1
    !! Stringl&auml;nge
    INTEGER :: l
    !
    ok      = .false.
    ctxt(:) = get_att_ch( this )
    !
    DO i = 1, c_max_st_type
       IF ( ok ) EXIT
       IF ( LEN_TRIM(ctxt(1)) == LEN_TRIM(c_storage_type(i)) ) &
            ok = ( TRIM(ctxt(1)) == TRIM(c_storage_type(i)) ) 
    END DO
    !
    IF ( .NOT. ok ) THEN
       i1 = 1
       cmaxtxt = REPEAT(' ', LEN(cmaxtxt)) ! Initialisieren
       DO i = 1, c_max_st_type
          l = LEN_TRIM(c_storage_type(i))
          WRITE( cmaxtxt(i1:i1+l), '(A,A)') TRIM(c_storage_type(i)), '|'
          i1 = i1 + l + 1
       END DO
       CALL setup_error_act ( all_errors(:), 6604, c_upname, c_modname )
       CALL setup_error_act ( '<..name..>', this%name )
       CALL setup_error_act ( '<..aktuell..>', TRIM(ctxt(1)) )
       CALL setup_error_act ( '<..erlaubt..>', TRIM(cmaxtxt)) 
    END IF
    !
  END FUNCTION ok_storage_type_0
  !
  !! Pr&uuml;fe, ob das Attribut (56) "class_names" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_class_names_0 ( this ) &
       RESULT( ok )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=16), PARAMETER :: c_upname='ok_class_names_0' ! 
    INTEGER                       :: i ! 
    !
    ok = .true.
    DO i=1,SIZE(this%ch)
       IF ( .NOT. ok ) EXIT
       ok = ( LEN_TRIM(this%ch(i)) > 0 )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6616, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_class_names_0
  !
  !! Pr&uuml;fe, ob das Attribut (58) "settling_velocity" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_settling_velocity_0 ( this ) &
       RESULT( ok )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=22), PARAMETER :: c_upname='ok_settling_velocity_0' ! 
    INTEGER                       :: i ! 
    !
    ok = .true.
    DO i=1,SIZE(this%ch)
       IF ( .NOT. ok ) EXIT
       ok = ( LEN_TRIM(this%ch(i)) > 0 )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6616, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_settling_velocity_0
  !
  !! Pr&uuml;fe, ob das Attribut (59) "settling_velocity_formula" o.k. ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_settling_velo_form_0 ( this ) &
       RESULT( ok )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=23), PARAMETER :: c_upname='ok_settling_velo_form_0' ! 
    INTEGER                       :: i ! 
    !
    ok = .true.
    DO i=1,SIZE(this%ch)
       IF ( .NOT. ok ) EXIT
       ok = ( LEN_TRIM(this%ch(i)) > 0 )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6616, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_settling_velo_form_0
  !
  !! Pr&uuml;fen der Angaben des Attributes (60) --> directional_sector <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_directional_sector_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=23), PARAMETER :: c_upname='ok_directional_sector_d' ! 
    INTEGER                       :: i ! 
    !
    ok = .true.
    DO i=1,SIZE(this%ch)
       IF ( .NOT. ok ) EXIT
       ok = ( LEN_TRIM(this%ch(i)) > 0 )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6616, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_directional_sector_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (61) --> cross_sectional_average <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_cross_sectional_average_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=28), PARAMETER :: c_upname='ok_cross_sectional_average_d' !  
    !! unterer und oberer Grenzwert
    INTEGER           , PARAMETER :: c_low=0, c_high=1                       ! 
    INTEGER                       :: wert(1)                                 ! 
    CHARACTER (LEN=10)            :: ctxt                                    ! 
    !
    wert(:) = get_att_in(this)
    ok      = ( c_low <= wert(1) .AND. wert(1) <= c_high )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6605, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
       CALL setup_error_act ( '<op1>', '<=' )
       CALL setup_error_act ( '<op2>', '<=' )
       WRITE(ctxt,'(I10)') c_low   ; CALL setup_error_act ( '<zul-wert-1>', ctxt )
       WRITE(ctxt,'(I10)') c_high  ; CALL setup_error_act ( '<zul-wert-2>', ctxt )
       WRITE(ctxt,'(I10)') wert(1) ; CALL setup_error_act ( '<wert>', ctxt       )
    END IF
    !
  END FUNCTION ok_cross_sectional_average_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (62) --> data_interval <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_data_interval_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=18), PARAMETER :: c_upname='ok_data_interval_d' ! 
    INTEGER                       :: i ! 
    !
    ok = .true.
    DO i=1,SIZE(this%ch)
       IF ( .NOT. ok ) EXIT
       ok = ( LEN_TRIM(this%ch(i)) > 0 )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6616, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_data_interval_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (63) --> high_water_mark <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_high_water_mark_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .true. ! derzeit kein Test
    !
  END FUNCTION ok_high_water_mark_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (64) --> low_water_mark <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_low_water_mark_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .true. ! derzeit kein Test
    !
  END FUNCTION ok_low_water_mark_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (65) --> high_tracer_mark <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_high_tracer_mark_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .true. ! derzeit kein Test
    !
  END FUNCTION ok_high_tracer_mark_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (66) --> low_tracer_mark <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ok_low_tracer_mark_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !
    ok = .true. ! derzeit kein Test
    !
  END FUNCTION ok_low_tracer_mark_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (67) --> grid_mapping_name <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_grid_mapping_name_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name des Unterprogramms
    CHARACTER (LEN=22), PARAMETER :: c_upname='ok_grid_mapping_name_d' ! 
    !! Hilfsvariable
    INTEGER :: i, l1, l2  ! 
    !
    ok = .false.
    l1 = LEN_TRIM(this%ch(1))
    DO i=1,SIZE(c_grid_mapping_name)
       IF ( ok ) EXIT
       l2 = LEN_TRIM(c_grid_mapping_name(i))
       IF ( l1 == l2 ) ok = ( this%ch(1)(1:l1) == c_grid_mapping_name(i)(1:l2) )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6613, c_upname, c_modname )
       CALL setup_error_act ( '<name>', TRIM(this%name) )
       CALL setup_error_act ( '<feldname>', 'c_grid_mapping_name' )
       CALL setup_error_act ( '<aktuell>', TRIM(this%ch(1)) )
    END IF
    !
  END FUNCTION ok_grid_mapping_name_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (68) --> grid_north_pole_latitude <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_grid_north_pole_latitude_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=29) , PARAMETER :: c_upname='ok_grid_north_pole_latitude_d' ! 
    !! unterer und oberer Grenzwert
    REAL (KIND=Double) , PARAMETER :: c_low=-90.0_Double, c_high=90.0_Double   ! 
    REAL (KIND=Double)             :: wert(1)                                  ! 
    CHARACTER (LEN=15)             :: ctxt                                     ! 
    !
    wert(:) = get_att_dp(this)
    ok      = ( c_low <= wert(1) .AND. wert(1) <= c_high )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6605, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
       CALL setup_error_act ( '<op1>', '<=' )
       CALL setup_error_act ( '<op2>', '<=' )
       WRITE(ctxt,'(G15.9)') c_low   ; CALL setup_error_act ( '<zul-wert-1>', ctxt )
       WRITE(ctxt,'(G15.9)') c_high  ; CALL setup_error_act ( '<zul-wert-2>', ctxt )
       WRITE(ctxt,'(G15.9)') wert(1) ; CALL setup_error_act ( '<wert>', ctxt       )
    END IF
    !
  END FUNCTION ok_grid_north_pole_latitude_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (69) --> grid_north_pole_longitude <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_grid_north_pole_longitude_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=30) , PARAMETER :: c_upname='ok_grid_north_pole_longitude_d' ! 
    !! unterer und oberer Grenzwert
    REAL (KIND=Double) , PARAMETER :: c_low=-180.0_Double, c_high=180.0_Double ! 
    REAL (KIND=Double)             :: wert(1)                                  ! 
    CHARACTER (LEN=15)             :: ctxt                                     ! 
    !
    wert(:) = get_att_dp(this)
    ok      = ( c_low <= wert(1) .AND. wert(1) < c_high )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6605, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
       CALL setup_error_act ( '<op1>', '<=' )
       CALL setup_error_act ( '<op2>', '< ' )
       WRITE(ctxt,'(G15.9)') c_low   ; CALL setup_error_act ( '<zul-wert-1>', ctxt )
       WRITE(ctxt,'(G15.9)') c_high  ; CALL setup_error_act ( '<zul-wert-2>', ctxt )
       WRITE(ctxt,'(G15.9)') wert(1) ; CALL setup_error_act ( '<wert>', ctxt       )
    END IF
    !
  END FUNCTION ok_grid_north_pole_longitude_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (70) --> latitude_of_projection_origin <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_latitude_of_proj_origin_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=28) , PARAMETER :: c_upname='ok_latitude_of_proj_origin_d' ! 
    !! unterer und oberer Grenzwert
    REAL (KIND=Double) , PARAMETER :: c_low=-90.0_Double, c_high=90.0_Double   ! 
    REAL (KIND=Double)             :: wert(1)                                  ! 
    CHARACTER (LEN=15)             :: ctxt                                     ! 
    !
    wert(:) = get_att_dp(this)
    ok      = ( c_low <= wert(1) .AND. wert(1) <= c_high )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6605, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
       CALL setup_error_act ( '<op1>', '<=' )
       CALL setup_error_act ( '<op2>', '<=' )
       WRITE(ctxt,'(G15.9)') c_low   ; CALL setup_error_act ( '<zul-wert-1>', ctxt )
       WRITE(ctxt,'(G15.9)') c_high  ; CALL setup_error_act ( '<zul-wert-2>', ctxt )
       WRITE(ctxt,'(G15.9)') wert(1) ; CALL setup_error_act ( '<wert>', ctxt       )
    END IF
    !
  END FUNCTION ok_latitude_of_proj_origin_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (71) --> longitude_of_projection_origin <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_longitude_of_proj_origin_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=29) , PARAMETER :: c_upname='ok_longitude_of_proj_origin_d' ! 
    !! unterer und oberer Grenzwert
    REAL (KIND=Double) , PARAMETER :: c_low=-180.0_Double, c_high=180.0_Double ! 
    REAL (KIND=Double)             :: wert(1)                                  ! 
    CHARACTER (LEN=15)             :: ctxt                                     ! 
    !
    wert(:) = get_att_dp(this)
    ok      = ( c_low <= wert(1) .AND. wert(1) < c_high )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6605, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
       CALL setup_error_act ( '<op1>', '<=' )
       CALL setup_error_act ( '<op2>', '< ' )
       WRITE(ctxt,'(G15.9)') c_low   ; CALL setup_error_act ( '<zul-wert-1>', ctxt )
       WRITE(ctxt,'(G15.9)') c_high  ; CALL setup_error_act ( '<zul-wert-2>', ctxt )
       WRITE(ctxt,'(G15.9)') wert(1) ; CALL setup_error_act ( '<wert>', ctxt       )
    END IF
    !
  END FUNCTION ok_longitude_of_proj_origin_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (72) --> north_pole_grid_longitude <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_north_pole_grid_longitude_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=30) , PARAMETER :: c_upname='ok_north_pole_grid_longitude_d' ! 
    !! unterer und oberer Grenzwert
    REAL (KIND=Double) , PARAMETER :: c_low=-180.0_Double, c_high=180.0_Double ! 
    REAL (KIND=Double)             :: wert(1)                                  ! 
    CHARACTER (LEN=15)             :: ctxt                                     ! 
    !
    wert(:) = get_att_dp(this)
    ok      = ( c_low <= wert(1) .AND. wert(1) < c_high )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6605, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
       CALL setup_error_act ( '<op1>', '<=' )
       CALL setup_error_act ( '<op2>', '< ' )
       WRITE(ctxt,'(G15.9)') c_low   ; CALL setup_error_act ( '<zul-wert-1>', ctxt )
       WRITE(ctxt,'(G15.9)') c_high  ; CALL setup_error_act ( '<zul-wert-2>', ctxt )
       WRITE(ctxt,'(G15.9)') wert(1) ; CALL setup_error_act ( '<wert>', ctxt       )
    END IF
    !
  END FUNCTION ok_north_pole_grid_longitude_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (73) --> scale_factor_at_central_meridian <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_scale_factor_at_c_meridian_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=31), PARAMETER :: c_upname='ok_scale_factor_at_c_meridian_d' ! 
    !! Hilfsvariablen
    REAL (KIND=Double) :: wert(2) ! 
    CHARACTER (LEN=15) :: ctxt    ! 
    !
    wert(:)   = 0.0_Double
    wert(2:2) = get_att_dp( this )
    ok        = ( wert(1) < wert(2) )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6611, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
       CALL setup_error_act ( '<op1>', '<' )
       WRITE(ctxt,'(G15.9)') wert(1) ; CALL setup_error_act ( '<wert-1>', ctxt )
       WRITE(ctxt,'(G15.9)') wert(2) ; CALL setup_error_act ( '<wert-2>', ctxt )
    END IF
    !
  END FUNCTION ok_scale_factor_at_c_meridian_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (74) --> scale_factor_at_projection_origin <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_scale_factor_at_p_origin_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=29), PARAMETER :: c_upname='ok_scale_factor_at_p_origin_d' ! 
    !! Hilfsvariablen
    REAL (KIND=Double) :: wert(2) ! 
    CHARACTER (LEN=15) :: ctxt    ! 
    !
    wert(:)   = 0.0_Double
    wert(2:2) = get_att_dp( this )
    ok        = ( wert(1) < wert(2) )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6611, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
       CALL setup_error_act ( '<op1>', '<' )
       WRITE(ctxt,'(G15.9)') wert(1) ; CALL setup_error_act ( '<wert-1>', ctxt )
       WRITE(ctxt,'(G15.9)') wert(2) ; CALL setup_error_act ( '<wert-2>', ctxt )
    END IF
    !
  END FUNCTION ok_scale_factor_at_p_origin_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (75) --> standard_parallel <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_standard_parallel_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=22) , PARAMETER :: c_upname='ok_standard_parallel_d' ! 
    !! unterer und oberer Grenzwert
    REAL (KIND=Double) , PARAMETER :: c_low=-90.0_Double, c_high=90.0_Double ! 
    INTEGER                        :: i                                      ! 
    CHARACTER (LEN=15)             :: ctxt                                   ! 
    !
    DO i=1,SIZE(this%dp)
       ok = ( c_low <= this%dp(i) .AND. this%dp(i) <= c_high )
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6605, c_upname, c_modname )
          CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
          CALL setup_error_act ( '<op1>', '<=' )
          CALL setup_error_act ( '<op2>', '<=' )
          WRITE(ctxt,'(G15.9)') c_low      ; CALL setup_error_act ( '<zul-wert-1>', ctxt )
          WRITE(ctxt,'(G15.9)') c_high     ; CALL setup_error_act ( '<zul-wert-2>', ctxt )
          WRITE(ctxt,'(G15.9)') this%dp(i) ; CALL setup_error_act ( '<wert>', ctxt       )
       END IF
    END DO
    !
  END FUNCTION ok_standard_parallel_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (76) --> straight_vertical_longitude_from_pole <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_straight_v_l_from_pole_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=27) , PARAMETER :: c_upname='ok_straight_v_l_from_pole_d' ! 
    !! unterer und oberer Grenzwert
    REAL (KIND=Double) , PARAMETER :: c_low=-180.0_Double, c_high=180.0_Double ! 
    REAL (KIND=Double)             :: wert(1)                                  ! 
    CHARACTER (LEN=15)             :: ctxt                                     ! 
    !
    wert(:) = get_att_dp(this)
    ok      = ( c_low <= wert(1) .AND. wert(1) < c_high )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6605, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
       CALL setup_error_act ( '<op1>', '<=' )
       CALL setup_error_act ( '<op2>', '< ' )
       WRITE(ctxt,'(G15.9)') c_low   ; CALL setup_error_act ( '<zul-wert-1>', ctxt )
       WRITE(ctxt,'(G15.9)') c_high  ; CALL setup_error_act ( '<zul-wert-2>', ctxt )
       WRITE(ctxt,'(G15.9)') wert(1) ; CALL setup_error_act ( '<wert>', ctxt       )
    END IF
    !
  END FUNCTION ok_straight_v_l_from_pole_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (77) --> ancillary_variables <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_ancillary_variables_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=24), PARAMETER :: c_upname='ok_ancillary_variables_d' ! 
    INTEGER                       :: i ! 
    !
    ok = .true.
    DO i=1,SIZE(this%ch)
       IF ( .NOT. ok ) EXIT
       ok = ( LEN_TRIM(this%ch(i)) > 0 )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6616, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_ancillary_variables_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (78) --> axis <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_axis_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=9), PARAMETER :: c_upname='ok_axis_d' ! 
    !! Hilfsvariable
    INTEGER :: i, l1, l2  ! 
    !
    ok = .false.
    l1 = LEN_TRIM(this%ch(1))
    DO i=1,SIZE(c_axis)
       IF ( ok ) EXIT
       l2 = LEN_TRIM(c_axis(i))
       IF ( l1 == l2 ) ok = ( this%ch(1)(1:l1) == c_axis(i)(1:l2) )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6613, c_upname, c_modname )
       CALL setup_error_act ( '<name>', TRIM(this%name) )
       CALL setup_error_act ( '<feldname>', 'c_axis' )
       CALL setup_error_act ( '<aktuell>', TRIM(this%ch(1)) )
    END IF
    !
  END FUNCTION ok_axis_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (79) --> bounds <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_bounds_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=11), PARAMETER :: c_upname='ok_bounds_d' ! 
    INTEGER                       :: i ! 
    !
    ok = .true.
    DO i=1,SIZE(this%ch)
       IF ( .NOT. ok ) EXIT
       ok = ( LEN_TRIM(this%ch(i)) > 0 )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6616, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_bounds_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (80) --> calendar <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_calendar_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=13), PARAMETER :: c_upname='ok_calendar_d' ! 
    !! Hilfsvariable
    INTEGER :: i, l1, l2  ! 
    !
    ok = .false.
    l1 = LEN_TRIM(this%ch(1))
    DO i=1,SIZE(c_calendar)
       IF ( ok ) EXIT
       l2 = LEN_TRIM(c_calendar(i))
       IF ( l1 == l2 ) ok = ( this%ch(1)(1:l1) == c_calendar(i)(1:l2) )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6613, c_upname, c_modname )
       CALL setup_error_act ( '<name>', TRIM(this%name) )
       CALL setup_error_act ( '<feldname>', 'c_calendar' )
       CALL setup_error_act ( '<aktuell>', TRIM(this%ch(1)) )
    END IF
    !
  END FUNCTION ok_calendar_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (81) --> cell_measures <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_cell_measures_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=18), PARAMETER :: c_upname='ok_cell_measures_d' ! 
    !! Hilfsvariable
    INTEGER :: i, l1, l2  ! 
    !
    ok = .false.
    l1 = LEN_TRIM(this%ch(1))
    DO i=1,SIZE(c_cell_measures)
       IF ( ok ) EXIT
       l2 = LEN_TRIM(c_cell_measures(i))
       IF ( l1 == l2 ) ok = ( this%ch(1)(1:l1) == c_cell_measures(i)(1:l2) )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6613, c_upname, c_modname )
       CALL setup_error_act ( '<name>', TRIM(this%name) )
       CALL setup_error_act ( '<feldname>', 'c_cell_measures' )
       CALL setup_error_act ( '<aktuell>', TRIM(this%ch(1)) )
    END IF
    !
  END FUNCTION ok_cell_measures_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (82) --> cell_methods <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_cell_methods_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=17), PARAMETER :: c_upname='ok_cell_methods_d' ! 
    !! Hilfsvariable
    INTEGER :: i, l1, l2  ! 
    !
    ok = .false.
    l1 = LEN_TRIM(this%ch(1))
    DO i=1,SIZE(c_cell_methods)
       IF ( ok ) EXIT
       l2 = LEN_TRIM(c_cell_methods(i))
       IF ( l1 == l2 ) ok = ( this%ch(1)(1:l1) == c_cell_methods(i)(1:l2) )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6613, c_upname, c_modname )
       CALL setup_error_act ( '<name>', TRIM(this%name) )
       CALL setup_error_act ( '<feldname>', 'c_cell_methods' )
       CALL setup_error_act ( '<aktuell>', TRIM(this%ch(1)) )
    END IF
    !
  END FUNCTION ok_cell_methods_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (83) --> climatology <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_climatology_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=16), PARAMETER :: c_upname='ok_climatology_d' ! 
    INTEGER                       :: i ! 
    !
    ok = .true.
    DO i=1,SIZE(this%ch)
       IF ( .NOT. ok ) EXIT
       ok = ( LEN_TRIM(this%ch(i)) > 0 )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6616, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_climatology_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (84) --> comment <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_comment_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=12), PARAMETER :: c_upname='ok_comment_d' ! 
    INTEGER                       :: i ! 
    !
    ok = .true.
    DO i=1,SIZE(this%ch)
       IF ( .NOT. ok ) EXIT
       ok = ( LEN_TRIM(this%ch(i)) > 0 )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6616, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_comment_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (85) --> compress <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_compress_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=13), PARAMETER :: c_upname='ok_compress_d' ! 
    INTEGER                       :: i ! 
    !
    ok = .true.
    DO i=1,SIZE(this%ch)
       IF ( .NOT. ok ) EXIT
       ok = ( LEN_TRIM(this%ch(i)) > 0 )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6616, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_compress_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (86) --> Conventions <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_Conventions_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=16), PARAMETER :: c_upname='ok_Conventions_d' ! 
    INTEGER                       :: i ! 
    !
    ok = .true.
    DO i=1,SIZE(this%ch)
       IF ( .NOT. ok ) EXIT
       ok = ( LEN_TRIM(this%ch(i)) > 0 )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6616, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_Conventions_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (87) --> coordinates <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_coordinates_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=16), PARAMETER :: c_upname='ok_coordinates_d' ! 
    INTEGER                       :: i ! 
    !
    ok = .true.
    DO i=1,SIZE(this%ch)
       IF ( .NOT. ok ) EXIT
       ok = ( LEN_TRIM(this%ch(i)) > 0 )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6616, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_coordinates_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (88) --> flag_meanings <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_flag_meanings_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=18), PARAMETER :: c_upname='ok_flag_meanings_d' ! 
    INTEGER                       :: i ! 
    !
    ok = .true.
    DO i=1,SIZE(this%ch)
       IF ( .NOT. ok ) EXIT
       ok = ( LEN_TRIM(this%ch(i)) > 0 )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6616, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_flag_meanings_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (89) --> flag_values <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_flag_values_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=16), PARAMETER :: c_upname='ok_flag_values_d' ! 
    INTEGER                       :: i ! 
    !
    ok = .true.
    DO i=1,SIZE(this%ch)
       IF ( .NOT. ok ) EXIT
       ok = ( LEN_TRIM(this%ch(i)) > 0 )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6616, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_flag_values_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (90) --> formula_terms <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_formula_terms_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=18), PARAMETER :: c_upname='ok_formula_terms_d' ! 
    INTEGER                       :: i ! 
    !
    ok = .true.
    DO i=1,SIZE(this%ch)
       IF ( .NOT. ok ) EXIT
       ok = ( LEN_TRIM(this%ch(i)) > 0 )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6616, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_formula_terms_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (91) --> grid_mapping <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_grid_mapping_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=17), PARAMETER :: c_upname='ok_grid_mapping_d' ! 
    INTEGER                       :: i ! 
    !
    ok = .true.
    DO i=1,SIZE(this%ch)
       IF ( .NOT. ok ) EXIT
       ok = ( LEN_TRIM(this%ch(i)) > 0 )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6616, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_grid_mapping_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (92) --> institution <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_institution_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=16), PARAMETER :: c_upname='ok_institution_d' ! 
    INTEGER                       :: i ! 
    !
    ok = .true.
    DO i=1,SIZE(this%ch)
       IF ( .NOT. ok ) EXIT
       ok = ( LEN_TRIM(this%ch(i)) > 0 )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6616, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_institution_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (93) --> leap_month <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_leap_month_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=15) , PARAMETER :: c_upname='ok_leap_month_d' ! 
    !! unterer und oberer Grenzwert
    INTEGER , PARAMETER :: c_low=1, c_high=12 ! 
    INTEGER             :: wert(1)            ! 
    CHARACTER (LEN=10)  :: ctxt               ! 
    !
    wert(:) = get_att_in(this)
    ok      = ( c_low <= wert(1) .AND. wert(1) <= c_high )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6605, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
       CALL setup_error_act ( '<op1>', '<=' )
       CALL setup_error_act ( '<op2>', '<=' )
       WRITE(ctxt,'(I10)') c_low   ; CALL setup_error_act ( '<zul-wert-1>', ctxt )
       WRITE(ctxt,'(I10)') c_high  ; CALL setup_error_act ( '<zul-wert-2>', ctxt )
       WRITE(ctxt,'(I10)') wert(1) ; CALL setup_error_act ( '<wert>', ctxt       )
    END IF
    !
  END FUNCTION ok_leap_month_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (94) --> leap_year <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_leap_year_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_leap_year_d' ! 
    !! unterer und oberer Grenzwert
    INTEGER , PARAMETER :: c_low=1, c_high=2005 ! 
    INTEGER             :: wert(1)              ! 
    CHARACTER (LEN=10)  :: ctxt                 ! 
    !
    wert(:) = get_att_in(this)
    ok      = ( c_low <= wert(1) .AND. wert(1) <= c_high )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6605, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
       CALL setup_error_act ( '<op1>', '<=' )
       CALL setup_error_act ( '<op2>', '<=' )
       WRITE(ctxt,'(I10)') c_low   ; CALL setup_error_act ( '<zul-wert-1>', ctxt )
       WRITE(ctxt,'(I10)') c_high  ; CALL setup_error_act ( '<zul-wert-2>', ctxt )
       WRITE(ctxt,'(I10)') wert(1) ; CALL setup_error_act ( '<wert>', ctxt       )
    END IF
    !
  END FUNCTION ok_leap_year_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (95) --> month_lengths <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_month_lengths_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=14) , PARAMETER :: c_upname='ok_leap_year_d' ! 
    !! unterer und oberer Grenzwert
    INTEGER , PARAMETER :: c_low=28, c_high=31 ! 
    INTEGER             :: i                   ! 
    CHARACTER (LEN=10)  :: ctxt                ! 
    !
    DO i=1,SIZE(this%in)
       ok = ( c_low <= this%in(i) .AND. this%in(i) <= c_high )
       IF ( .NOT. ok ) THEN
          CALL setup_error_act ( all_errors(:), 6605, c_upname, c_modname )
          CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
          CALL setup_error_act ( '<op1>', '<=' )
          CALL setup_error_act ( '<op2>', '<=' )
          WRITE(ctxt,'(I10)') c_low      ; CALL setup_error_act ( '<zul-wert-1>', ctxt )
          WRITE(ctxt,'(I10)') c_high     ; CALL setup_error_act ( '<zul-wert-2>', ctxt )
          WRITE(ctxt,'(I10)') this%in(i) ; CALL setup_error_act ( '<wert>', ctxt       )
       END IF
    END DO
    !
  END FUNCTION ok_month_lengths_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (96) --> positive <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_positive_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=13), PARAMETER :: c_upname='ok_positive_d' ! 
    !! Hilfsvariable
    INTEGER :: i, l1, l2  ! 
    !
    ok = .false.
    l1 = LEN_TRIM(this%ch(1))
    DO i=1,SIZE(c_positive)
       IF ( ok ) EXIT
       l2 = LEN_TRIM(c_positive(i))
       IF ( l1 == l2 ) ok = ( this%ch(1)(1:l1) == c_positive(i)(1:l2) )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6613, c_upname, c_modname )
       CALL setup_error_act ( '<name>', TRIM(this%name) )
       CALL setup_error_act ( '<feldname>', 'c_positive' )
       CALL setup_error_act ( '<aktuell>', TRIM(this%ch(1)) )
    END IF
    !
  END FUNCTION ok_positive_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (97) --> references <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_references_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=15), PARAMETER :: c_upname='ok_references_d' ! 
    INTEGER                       :: i ! 
    !
    ok = .true.
    DO i=1,SIZE(this%ch)
       IF ( .NOT. ok ) EXIT
       ok = ( LEN_TRIM(this%ch(i)) > 0 )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6616, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_references_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (98) --> source <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_source_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=11), PARAMETER :: c_upname='ok_source_d' ! 
    INTEGER                       :: i ! 
    !
    ok = .true.
    DO i=1,SIZE(this%ch)
       IF ( .NOT. ok ) EXIT
       ok = ( LEN_TRIM(this%ch(i)) > 0 )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6616, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_source_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (99) --> standard_error_multiplier <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_standard_error_multiplier_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name des Unterprogramms
    CHARACTER (LEN=30), PARAMETER :: c_upname='ok_standard_error_multiplier_d' ! 
    !! untere und obere Grenzwerte f&uuml;r die aktuelle Komponente
    REAL (KIND=Double) , PARAMETER :: c_low=0.5_Double, c_high=10.0_Double ! 
    !! akt.Wert als Real
    REAL (KIND=Double) :: wert(1) ! 
    !! akt.Wert als String
    CHARACTER (LEN=15) :: ctxt     ! 
    !
    wert(:) = 0.0_Double
    IF ( is_att_dp(this) ) wert(:) = get_att_dp(this)
    IF ( is_att_in(this) ) wert(:) = REAL(get_att_in(this),Double)
    ok      = ( c_low <= wert(1) .AND. wert(1) <= c_high )
    !
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6605, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
       CALL setup_error_act ( '<op1>', '<=' )
       CALL setup_error_act ( '<op2>', '<=' )
       WRITE(ctxt,'(G15.9)') c_low   ; CALL setup_error_act ( '<zul-wert-1>', ctxt )
       WRITE(ctxt,'(G15.9)') c_high  ; CALL setup_error_act ( '<zul-wert-2>', ctxt )
       WRITE(ctxt,'(G15.9)') wert(1) ; CALL setup_error_act ( '<wert>', ctxt       )
    END IF
    !
  END FUNCTION ok_standard_error_multiplier_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (100) --> standard_name <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_standard_name_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=18), PARAMETER :: c_upname='ok_standard_name_d' ! 
    INTEGER                       :: i ! 
    !
    ok = .true.
    DO i=1,SIZE(this%ch)
       IF ( .NOT. ok ) EXIT
       ok = ( LEN_TRIM(this%ch(i)) > 0 )
    END DO
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6616, c_upname, c_modname )
       CALL setup_error_act ( '<AktAttName>', TRIM(this%name) )
    END IF
    !
  END FUNCTION ok_standard_name_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (101) --> valid_max <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_valid_max_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name des Unterprogramms
    CHARACTER (LEN=14), PARAMETER :: c_upname='ok_valid_max_d' ! 
    !
    ok = .true. ! derzeit kein Test
    !
  END FUNCTION ok_valid_max_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (102) --> valid_min <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_valid_min_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name des Unterprogramms
    CHARACTER (LEN=14), PARAMETER :: c_upname='ok_valid_min_d' ! 
    !
    ok = .true. ! derzeit kein Test
    !
  END FUNCTION ok_valid_min_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (103) --> astro <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_astro_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name des Unterprogramms
    CHARACTER (LEN=10), PARAMETER :: c_upname='ok_astro_d' ! 
    !
    ok = .true. ! derzeit kein Test
    !
  END FUNCTION ok_astro_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (104) --> grid_type <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_grid_type_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name des Unterprogramms
    CHARACTER (LEN=14), PARAMETER :: c_upname='ok_grid_type_d' ! 
    !
    ok = .true. ! derzeit kein Test
    !
  END FUNCTION ok_grid_type_d
  !
  !! Pr&uuml;fen der Angaben des Attributes (105) --> coordinate_system <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_coordinate_system_d ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Testergebnis
    LOGICAL :: ok ! 
    !! Name des Unterprogramms
    CHARACTER (LEN=22), PARAMETER :: c_upname='ok_coordinate_system_d' ! 
    !
    ok = .true. ! derzeit kein Test
    !
  END FUNCTION ok_coordinate_system_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "id" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_att_id ( this )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=12) , PARAMETER :: c_upname='print_att_id' ! 
    !! Statusvariable
    INTEGER :: stat !  
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) this%id
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente id  - - - - - - - - - - - - - - - - - ',/&
           '# id    = ',I5,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_att_id
  !
  !! Drucke den Inhalt der Komponente "name" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_att_name ( this )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='print_att_name' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) TRIM( this%name )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente name  - - - - - - - - - - - - - - - - ',/&
           '# name  = ',A,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_att_name
  !
  !! Drucke den Inhalt der Komponente "var_id" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_att_var_id ( this )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=16) , PARAMETER :: c_upname='print_att_var_id' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) this%var_id
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente var_id  - - - - - - - - - - - - - - - ',/&
           '# var_id = ',I5,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_att_var_id
  !
  !! Drucke den Inhalt der Komponente "ch" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_att_ch ( this )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=12) , PARAMETER :: c_upname='print_att_ch' ! 
    !! Statusvariable
    INTEGER :: stat !  
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%ch ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7040, c_upname, c_modname, stat )
       ELSE
          DO i=1,SIZE(this%ch)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i, TRIM( this%ch(i) )
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7040, c_upname, c_modname, stat )
          ELSE
             WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
             IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7040, c_upname, c_modname, stat )
          END IF
       END IF
    END IF
    !
8000 FORMAT ( '# Inhalt der Komponente ch  - - - - - - - - - - - - - - - - - ') 
8001 FORMAT ( '# nr = ',I5,', ch(nr) = ',A )
8002 FORMAT ( '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_att_ch
  !
  !! Drucke den Inhalt der Komponente "in" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_att_in ( this )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=12) , PARAMETER :: c_upname='print_att_in' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%in ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname, stat )
       ELSE
          DO i=1,SIZE(this%in)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i, this%in(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname, stat )
          ELSE
             WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
             IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7050, c_upname, c_modname, stat )
          END IF
       END IF
    END IF
    !
8000 FORMAT ( '# Inhalt der Komponente in  - - - - - - - - - - - - - - - - - ') 
8001 FORMAT ( '# nr = ',I5,', in(nr) = ',I15 )
8002 FORMAT ( '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_att_in
  !
  !! Drucke den Inhalt der Komponente "dp" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_att_dp ( this )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! Name der Funktion
    CHARACTER (LEN=12) , PARAMETER :: c_upname='print_att_dp' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    IF ( ASSOCIATED( this%dp ) ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0 ) THEN
          CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
       ELSE
          DO i=1,SIZE(this%dp)
             IF ( stat /= 0 ) EXIT
             WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i, this%dp(i)
          END DO
          IF ( stat /= 0 ) THEN
             CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
          ELSE
             WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
             IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7060, c_upname, c_modname, stat )
          END IF
       END IF
    END IF
    !
8000 FORMAT ( '# Inhalt der Komponente dp  - - - - - - - - - - - - - - - - - ') 
8001 FORMAT ( '# nr = ',I5,', dp(nr) = ',G15.9 )
8002 FORMAT ( '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_att_dp
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! ermittle den nach "c_att_type_shape" erforderlichen "type" f&uuml;r das 
  !! aktuelle Objekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_req_att_type ( this )             &
       RESULT( val )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : erforderlicher Typ des Objekts
    CHARACTER (LEN=3) :: val ! 
    !! Z&auml;hlervariable
    INTEGER :: i   ! 
    !! Indices
    INTEGER :: idx ! 
    !
    idx = 0
    DO i=1,c_max_att_name
       IF ( idx /= 0 ) EXIT
       idx = MERGE ( i, 0, get_lowercase_char(c_att_name(i)) == get_lowercase_char(this%name) )
    END DO
    !
    IF ( 0 < idx .AND. idx <= c_max_att_name ) THEN
       val  = MERGE( 'CH ', 'UND', c_att_type_shape(idx)(1:2) == 'ch' )
       val  = MERGE( 'IN ', val  , c_att_type_shape(idx)(1:2) == 'in' )
       val  = MERGE( 'DP ', val  , c_att_type_shape(idx)(1:2) == 'dp' )
       val  = MERGE( 'VA ', val  , c_att_type_shape(idx)(1:2) == 'va' )
    ELSE
       val = 'UND' ! 
    END IF
    !
  END FUNCTION get_req_att_type
  !
  !! ermittle die Anzahl der zu dem Attribut gehoerenden Werte f&uuml;r das 
  !! aktuelle Objekt <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_req_nof_att_values ( this )                   &
       RESULT( nof )
    !! Datenobjekt
    TYPE (t_att) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : Anzahl der erforderlichen Werte (-1 == unbegrenzt )
    INTEGER :: nof ! 
    !! Z&auml;hlervariable
    INTEGER :: i   ! 
    !! Indices
    INTEGER :: idx, ia, ie ! 
    !! Hilfsfeld
    CHARACTER (LEN=LEN(c_att_type_shape)) :: ctxt ! 
    idx = 0
    DO i=1,c_max_att_name
       IF ( idx /= 0 ) EXIT
       idx = MERGE ( i, 0, get_lowercase_char(c_att_name(i)) == get_lowercase_char(this%name) )
    END DO
    !
    IF ( 0 < idx .AND. idx <= c_max_att_name ) THEN
       ia  = INDEX( c_att_type_shape(idx), '(' ) + 1 
       ie  = INDEX( c_att_type_shape(idx), ')' ) - 1
       IF ( c_att_type_shape(idx)(ia:ia) == ":" ) THEN
          nof = -1
       ELSE
          ctxt = REPEAT( ' ', LEN(ctxt) )
          ctxt = c_att_type_shape(idx)(ia:ie)
          READ ( ctxt, FMT=* ) nof
       END IF
    ELSE
       nof = 0
    END IF
    !
  END FUNCTION get_req_nof_att_values
  !
  !! Ermittle die Liste der unterschiedlichen var_id's f&uuml;r alle Attribute <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_var_id_list_for_att ( this ) &
       RESULT( res )
    !! aktuelles Objekt (Vektor)
    TYPE (t_att) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Liste mit allen unterschiedlichen Variablen-Ids
    INTEGER         , POINTER :: res(:)  ! 
    !! Hilfsvariablen
    LOGICAL , ALLOCATABLE :: l_l(:)  ! 
    INTEGER               :: i, n, m ! 
    ! Initialisieren
    NULLIFY(res)
    ALLOCATE ( l_l(SIZE(this)) )
    WHERE ( this(:)%var_id == 0 ) 
       l_l(:) = .false.
    ELSEWHERE
       l_l(:) = .true.
    END WHERE
    ! die Positionen mit dem ersten Auftreten neuer var_id's ermitteln
    DO i=1,SIZE(this)-1
       IF ( .NOT. l_l(i) ) CYCLE
       WHERE ( this(i+1:)%var_id == this(i)%var_id ) l_l(i+1:) = .false.
    END DO
    ! Anzahl ermitteln, und eindeutige var_id's umspeichern
    n = COUNT( l_l )
    IF ( n > 0 ) THEN
       ALLOCATE( res(n) )
       res(:) = -1
       m      = 0
       DO i=1,SIZE(l_l)
          IF ( .NOT. l_l(i) ) CYCLE
          m      = m + 1
          res(m) = this(i)%var_id
       END DO
    END IF
    ! Aufraeumen
    DEALLOCATE ( l_l )
    !
  END FUNCTION get_var_id_list_for_att
  !
  !! Ermittle die Anfangs- und Ende-Indizes zusammenh&auml;ngender Worte in einem String <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_word_idx_list_in_ch ( var ) &
       RESULT( res )
    !! Stringvariable
    CHARACTER (LEN=*) , INTENT(IN) :: var      ! 
    !! Ergebnis: res(:,1) = Anfangsindices <BR>
    !!           res(:,2) = Endeindices
    INTEGER , POINTER              :: res(:,:) ! 
    !! Hilfsvariable
    LOGICAL , ALLOCATABLE :: l_l(:)    ! 
    LOGICAL               :: lastblank ! 
    INTEGER               :: i, n, m   !  
    !
    NULLIFY(res)
    IF ( LEN_TRIM(var) > 0 ) THEN
       ALLOCATE(l_l(LEN(var)))
       lastblank = .true.
       n         = 0
       ! ... Positionen mit Zeichen und Anzahl der Worte ermitteln
       DO i=1,SIZE(l_l)
          l_l(i)    = MERGE( .false., .true., var(i:i) == ' ' )
          n         = MERGE( n+1, n, lastblank .AND. l_l(i) )
          lastblank = .NOT. l_l(i) 
       END DO
       IF ( n > 0 ) THEN
          ALLOCATE(res(n,2))
          res(:,:)  = 0
          m         = 0
          lastblank = .true.
          DO i=1,SIZE(l_l)
             IF      ( lastblank .AND. l_l(i) ) THEN
                m        = m + 1
                res(m,1) = i
                res(m,2) = LEN_TRIM(var) ! vorlaeufiger Wert
             ELSE IF ( .NOT. lastblank .AND. .NOT. l_l(i) ) THEN
                res(m,2) = i-1
             END IF
             lastblank = .NOT. l_l(i)
          END DO
       END IF
       DEALLOCATE(l_l)
    END IF
    !
  END FUNCTION get_word_idx_list_in_ch
  !
  !! Ermittle die Anzahl der unterscheidbaren Worte in einem String <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_word_in_ch_count ( var ) &
       RESULT( res )
    !! Stringvariable
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Ergebnis: Anzahl der unterscheidbaren Worte in "var"
    INTEGER                        :: res ! 
    INTEGER , POINTER :: iaie(:,:) ! 
    !
    iaie => get_word_idx_list_in_ch ( var )
    IF ( ASSOCIATED(iaie) ) THEN
       res = SIZE(iaie,1)
    ELSE
       res = 0 ! 
    END IF
    !
  END FUNCTION get_word_in_ch_count
  !
  !! Ermittle ein bestimmtes Wort in einem String <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_word_in_ch ( var, n ) &
       RESULT( res )
    !! Stringvariable
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Nummer des gew&uuml;nschten Wortes
    INTEGER           , INTENT(IN) :: n   ! 
    !! Ergebnis: n-tes Wort in "var"
    CHARACTER(LEN=LEN(var))        :: res ! 
    !! Hilfsvariable
    INTEGER , POINTER :: iaie(:,:) ! 
    !
    iaie => get_word_idx_list_in_ch ( var )
    res  = REPEAT( ' ', LEN(res) )
    res  = 'undefined'
    IF ( ASSOCIATED(iaie) ) THEN
       res  = REPEAT( ' ', LEN(res) )
       IF ( n >= 1 .AND. n <= SIZE(iaie,1) ) THEN
          res(1:iaie(n,2)-iaie(n,1)+1) = var(iaie(n,1):iaie(n,2))
       END IF
    END IF
    !
  END FUNCTION get_word_in_ch
  !       
  !! Ermittle die Indexposition f&uuml;r ein aktuelles Grid-Mapping in "c_grid_mapping_name" <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_grid_mapping_name_idx ( var ) &
       RESULT( res )
    !! String mit dem Namen einer Koordinatentransformation
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! Ergebnis: Zeiger auf Eintrag in "c_grid_mapping_name", oder 0 falls nicht vorhanden
    INTEGER                        :: res ! 
    !! Hilfsvariable
    INTEGER :: i, l1, l2  ! 
    !
    res = 0
    l1  = LEN_TRIM(var)
    DO i=1,SIZE(c_grid_mapping_name)
       IF ( res > 0 ) EXIT
       l2 = LEN_TRIM(c_grid_mapping_name(i))
       IF ( l1 == l2 ) THEN
          IF ( var(1:l1) == c_grid_mapping_name(i)(1:l2) ) res = i
       END IF
    END DO
    !
  END FUNCTION get_grid_mapping_name_idx
  !
  !! Ermittle die Indexposition f&uuml;r den Namen des Attributs in dem Feld "c_att_name(:)" <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_att_name_idx ( this ) &
       RESULT( res )
    !! Datenobjeklt (Skalar)
    TYPE (t_att) , INTENT(IN) :: this      ! 
    !! Ergebnis: Zeiger auf Position in "c_att_name(:)" oder 0 falls nicht vorhanden
    INTEGER                   :: res       ! 
    !! Hilfsvariable
    INTEGER                   :: i, l1, l2 ! 
    !
    res = 0
    l1  = LEN_TRIM(this%name)
    DO i=1,SIZE(c_att_name)
       IF ( res /= 0 ) EXIT
       l2 = LEN_TRIM(c_att_name(i))
       IF ( l1 == l2 ) THEN
          IF ( get_lowercase_char(this%name(1:l1)) == get_lowercase_char(c_att_name(i)(1:l2)) ) res = i
       END IF
    END DO
    !
  END FUNCTION get_att_name_idx
  !
  FUNCTION get_datetime_from_unit ( var ) &
       RESULT( res )
    !! String mit Datumsangabe aus "units"-Attribut
    CHARACTER (LEN=*) , INTENT(IN) :: var ! 
    !! R&uuml;ckgabewert "var" als Datums-/Zeitangabe
    TYPE (t_datetime) :: res              ! 
    !! Name der Programmeinheit
    CHARACTER (LEN=22) , PARAMETER :: c_upname='get_datetime_from_unit' ! 
    !! diverse Hilfsvariablen
    INTEGER :: year, month, day, hour, min, sec, nan, zon, iostat, idx, jdx ! 
    !
    CALL new_datetime( res )
    year = 0 ; month = 0 ; day = 0 ; hour = 0 ; min = 0 ; sec = 0 ; nan = 0 ; zon = 0 
                       READ(var( 1: 4),'(I4)',IOSTAT=iostat) year
    IF ( iostat == 0 ) READ(var( 6: 7),'(I2)',IOSTAT=iostat) month
    IF ( iostat == 0 ) READ(var( 9:10),'(I2)',IOSTAT=iostat) day
    idx = INDEX( var(11:), ':' )
    IF ( idx > 0 ) THEN
       idx = 8 + idx
       IF ( iostat == 0 ) READ(var(idx+0:idx+1),'(I2)',IOSTAT=iostat) hour
       IF ( iostat == 0 ) READ(var(idx+3:idx+4),'(I2)',IOSTAT=iostat) min
       IF ( iostat == 0 ) READ(var(idx+6:idx+7),'(I2)',IOSTAT=iostat) sec
    END IF
    jdx = idx + 8
    idx = INDEX( var(jdx:), ':')
    IF ( idx > 0 ) THEN
       idx = jdx + idx - 4
       IF ( iostat == 0 ) READ(var(idx+0:idx+2),'(I3)',IOSTAT=iostat) zon
    END IF
    IF ( iostat == 0 ) THEN
       CALL set_datetime ( res, day, month, year, hour, min, sec, nan, zon )
    ELSE
       CALL setup_error_act ( all_errors(:), 9510, c_upname, c_modname, iostat )
       CALL setup_error_act ( '<wert>', TRIM(var) )
    END IF
    !
  END FUNCTION get_datetime_from_unit
  !
  !! Ermittle den Standardnamen einer Variablen, falls dieser vorhanden ist <BR>
  !! falls kein Standardname vorhanden ist, wird "undefined" zur&uuml;ckgegeben <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_att_standard_name_1_0 ( this, var ) &
       RESULT(res)
    !! aktuelle Datenobjekte
    TYPE (t_att)    , INTENT(IN) :: this(:) ! 
    !! Variable, deren Standardname ermittelt werden soll
    TYPE (t_var)    , INTENT(IN) :: var     ! 
    !! Ergebnis: Standardname der Variablen oder "undefined" falls nicht vorhanden
    CHARACTER (LEN=c_len_att_ch) :: res     ! 
    !! Hilfsvariable
    INTEGER :: idx ! 
    !
    res = REPEAT( ' ', LEN(res) ) ! 
    res = 'undefined'             ! 
    idx = get_att_idx ( this, c_att_name(100), get_var_id(var) )
    IF ( idx > 0 ) THEN
       IF ( is_att_ch( this(idx) ) ) THEN
          res = get_att_word_in_ch ( this(idx)%ch(1), 1 )
       END IF
    END IF
    !
  END FUNCTION get_att_standard_name_1_0
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe Komponente "id" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_att_id ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_att) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_att) , INTENT(IN) :: this2 ! 
    !! R&uuml;ckgabewert : Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = ( this1%id == this2%id )
    !
  END FUNCTION eq_att_id
  !
  !! pr&uuml;fe Komponente "name" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_att_name ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_att) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_att) , INTENT(IN) :: this2 ! 
    !! R&uuml;ckgabewert : Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    IF ( LEN_TRIM( this1%name ) == LEN_TRIM( this2%name ) ) THEN
       ok = ( get_lowercase_char(this1%name) == get_lowercase_char(this2%name) )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION eq_att_name 
  !
  !! pr&uuml;fe Komponente "var_id" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_att_var_id ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_att) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_att) , INTENT(IN) :: this2 ! 
    !! R&uuml;ckgabewert : Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = ( this1%var_id == this2%var_id )
    !
  END FUNCTION eq_att_var_id 
  !
  !! pr&uuml;fe Komponente "ch" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_att_ch ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_att) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_att) , INTENT(IN) :: this2 ! 
    !! R&uuml;ckgabewert : Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    IF      ( .NOT. ASSOCIATED( this1%ch ) .AND. .NOT. ASSOCIATED( this2%ch ) ) THEN
       ok = .true.
    ELSE IF ( ASSOCIATED( this1%ch )       .AND. ASSOCIATED( this2%ch )       ) THEN
       IF ( SIZE(this1%ch) == SIZE(this2%ch) ) THEN
          ok = ALL( this1%ch(:) == this2%ch(:) )
       ELSE
          ok = .false.
       END IF
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION eq_att_ch 
  !
  !! pr&uuml;fe Komponente "in" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_att_in ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_att) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_att) , INTENT(IN) :: this2 ! 
    !! R&uuml;ckgabewert : Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    IF      ( .NOT. ASSOCIATED( this1%in ) .AND. .NOT. ASSOCIATED( this2%in ) ) THEN
       ok = .true.
    ELSE IF ( ASSOCIATED( this1%in )       .AND. ASSOCIATED( this2%in )       ) THEN
       IF ( SIZE(this1%in) == SIZE(this2%in) ) THEN
          ok = ALL( this1%in(:) == this2%in(:) )
       ELSE
          ok = .false.
       END IF
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION eq_att_in 
  !
  !! pr&uuml;fe Komponente "dp" zweier Datenobjekte auf Gleichheit <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_att_dp ( this1, this2 ) &
         RESULT( ok )
    !! Referenzobjekt (Skalar)
    TYPE (t_att) , INTENT(IN) :: this1 ! 
    !! Vergleichsobjekt (Skalar)
    TYPE (t_att) , INTENT(IN) :: this2 ! 
    !! R&uuml;ckgabewert : Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    IF      ( .NOT. ASSOCIATED( this1%dp ) .AND. .NOT. ASSOCIATED( this2%dp ) ) THEN
       ok = .true.
    ELSE IF ( ASSOCIATED( this1%dp )       .AND. ASSOCIATED( this2%dp )       ) THEN
       IF ( SIZE(this1%dp) == SIZE(this2%dp) ) THEN
          ok = ALL( this1%dp(:) == this2%dp(:) )
       ELSE
          ok = .false.
       END IF
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION eq_att_dp 
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(+)-Methoden <<< [ERR_NO = 11000 bis 11999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(-)-Methoden <<< [ERR_NO = 12000 bis 12999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(*)-Methoden <<< [ERR_NO = 13000 bis 13999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(/)-Methoden <<< [ERR_NO = 14000 bis 14999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(>)-Methoden <<< [ERR_NO = 15000 bis 15999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(>=)-Methoden <<< [ERR_NO = 16000 bis 16999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(<)-Methoden <<< [ERR_NO = 17000 bis 17999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(<=)-Methoden <<< [ERR_NO = 18000 bis 18999]
  ! ----------------------------------------------------------------------
  !
  ! ------------------------------------------------------------------------
  ! Sonstiges
  ! ------------------------------------------------------------------------
  !
  !! Umwandeln eines Textes in Gro&szlig;buchstaben <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_uppercase_char_0 ( in ) &
       RESULT( out )
    !! Zeichenfolge (Original)
    CHARACTER (LEN=*) , INTENT(IN) :: in  ! 
    !! R&uuml;ckgabewert : Zeichenfolge (konvertiert in Gro&szlig;buchstaben)
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
  !! Gro&szlig;buchstaben in Kleinbuchstaben umwandeln (Skalar) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_lowercase_char_0 ( in ) &
       RESULT( out )
    !! Zeichenfolge (Original)
    CHARACTER (LEN=*) , INTENT(IN) :: in  ! 
    !! R&uuml;ckgabewert : Zeichenfolge (konvertiert in Kleinbuchstaben)
    CHARACTER (LEN=LEN(in))        :: out ! 
    !! Z&auml;hler 
    INTEGER :: i, ic ! 
    !
    out = in
    DO i=1,LEN(out)
       ic = IACHAR(in(i:i))
       IF ( ic >= 65 .AND. ic <= 90 ) out(i:i) = ACHAR(ic+32)
    END DO
    !
  END FUNCTION get_lowercase_char_0
  !
  !! Gro&szlig;buchstaben in Kleinbuchstaben umwandeln (Vektor) <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_lowercase_char_1 ( in ) &
       RESULT( out )
    !! Zeichenfolge (Original)
    CHARACTER (LEN=*) , INTENT(IN) :: in(:)         ! 
    !! R&uuml;ckgabewert : Zeichenfolge (konvertiert in Kleinbuchstaben)
    CHARACTER (LEN=LEN(in))        :: out(SIZE(in)) ! 
    !! Z&auml;hler 
    INTEGER :: i ! 
    !
    DO i=1,SIZE(out)
       out(i) = get_lowercase_char_0 ( in(i) )
    END DO
    !
  END FUNCTION get_lowercase_char_1
  !
END MODULE b_att
! TailOfBaseModule --------------------------------------------------------
