! -------------------------------------------------------------------------
! HeadOfBaseModule --------------------------------------------------------
!
!! <H2>Datentyp+Methoden zum Vorhalten von Dimensionsangaben zu Variablen</h2>
!! @author <A HREF="mailto:lang@hamburg.baw.de">G. Lang</A>
!! @version 2.3 vom 08/16/06, Quellcode: mod_b_dim.f90
!! <HR>
!! type+methods to deal with dimensions for variables <BR>
!! <HR>
!! <H3>Copyright-Hinweis</H3>
!!                                                                   <BR>
!! Copyright (C) <Year> <A HREF="http://www.hamburg.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!!                                                                   <BR>
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2002-08-02 : G. Lang     : Startversion
!  01.02 : 2002-08-05 : G. Lang     : FM Nr 7010 fehlte
!  01.03 : 2002-08-07 : G. Lang     : Korr/00000/02936
!  01.04 : 2002-08-07 : G. Lang     : SIZE(a(:)) -> SIZE(a)
!  01.05 : 2002-08-12 : G. Lang     : neue Dimensionsbezeichner
!  01.06 : 2002-08-14 : G. Lang     : Dimension "vedge" hinzugef&uuml;gt
!  01.07 : 2002-08-14 : G. Lang     : zulaessige Dimensionsnamen ergaenzt
!  01.08 : 2002-08-23 : G. Lang     : neue Klassen fuer Dimensionen
!  01.09 : 2002-08-30 : G. Lang     : in Komponente Name werden Blanks durch "_" ersetzt
!  01.10 : 2002-09-03 : G. Lang     : valid_unlimited_dim_name
!  01.11 : 2002-09-11 : G. Lang     : Korrektur in ok-Funktion
!  01.12 : 2002-09-12 : G. Lang     : gen_error_if_dim_not_equal
!  01.13 : 2002-08-14 : G. Lang     : Anpassungen TV12 vom Dez. 2002
!  01.14 : 2004-01-19 : H. Weilbeer : INTENT(OUT) nach INTENT(INOUT)
!  01.15 : 2004-01-19 : G. Lang     : neue Dimensionen eingefuegt
!  01.16 : 2004-04-23 : J. Juerges  : get_dim_name um get_dim_name_1_i_0 (Auswahl ueber ID) erweitert
!  01.17 : 2004-05-09 : G. Lang     : Erweiterungen fuer io_dataset-Einbindung
!  02.01 : 2006-03-29 : G. Lang     : Erweiterungen CF-Metadaten/Delft3D-NetCDF-Schnittstelle
!  02.02 : 2006-05-04 : G. Lang     : is_3d_class_dim, get_3d_class_dim_count
!  02.03 : 2006-08-16 : G. Lang     : neue Dimensionsbezeichnung "CHAR20"
!
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! <UL>
!!    <LI> Typ und Methoden f&uuml;r Dimensionsbezeichner;
!!    <LI> neben einer Kennung <EM>id</EM> und einem Namen <EM>name</EM>
!!         wird eine L&auml;nge <EM>len</EM> zugewiesen;
!!    <LI> die Dimensionsangabe kann in Zusammenhang mit der Definition
!!         von Variablen wieder verwendet werden;
!!    <LI> dieser Typ und seine Methoden wird insbesondere zur
!!         Speicherung von Informationen &uuml;ber Daten ben&ouml;tigt.
!! </UL>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Typ-Definition</H3>
!! Dieses Modul stellt den selbst definierten Datentyp <EM>t_dim</EM>
!! zur Verf&uuml;gung. Dieser besteht aus den folgenden Komponenten:      <BR>
!! <OL>
!!     <LI> <TT>id</TT>   : kennzeichnende Identifikationsnummer (Kennung);
!!     <LI> <TT>name</TT> : Klartext-Bezeichnung der Dimension (z.B. <EM>time</EM>);
!!     <LI> <TT>len</TT>  : Gr&ouml;&szlig;e der Dimension.
!! </OL>
!!                                                                  <BR>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Verwenden des Moduls</H3>
!!                                                                  <BR>
!! Die Leistungen des Moduls k&ouml;nnen wie folgt in Anspruch genommen werden: <BR>
!! <OL>
!!    <LI> Einbinden des Moduls mittels USE-Anweisung in der rufenden Programmeinheit;
!!    <LI> Initialisieren des Moduls b_dim mit INIT-Methode;
!!    <LI> ggf. Verwenden verschiedener SETUP-Methoden;
!!    <LI> Verwenden beliebiger Modul-Methoden (nicht INIT und CLEAR);
!!    <LI> De-Initialisieren des Moduls b_dim mit CLEAR-Methode.
!! </OL>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Fehlersituationen des Moduls</H3>
!!                                                                    <BR>
!! Hinweis: einige Methoden dieses Moduls erzeugen Fehlermeldungen,
!!          andere nicht. 
!!          Routinen die Fehlermeldungen generieren m&uuml;ssen pr&uuml;fen,
!!          ob das Modul korrekt initialisert wurde (ok_initialised). <BR>
!!          CALL PRINT_DIM_ALL_ERRORS ( ) verwenden.
!
MODULE b_dim
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] BASIS-Modul mit globalen Konstantwerten
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
  ! [C.0] Konstantwerte ------------------------------------------------- 
  !
  !! maximale zul&auml;ssige L&auml;nge des Namens <EM>name</EM> in <EM>t_dim</EM>
  INTEGER , PUBLIC , PARAMETER :: c_len_dim_name=20 ! 
  !
  ! [C.1] (maximal) einen oeffentlich zugaenglichen Typ deklarieren
  !
  !! Speichern von Informationen f&uuml;r eine Dimensionsangabe     <BR>
  !! Identifikationsnummer <EM>id</EM>                              <BR>
  !! Name der Dimension <EM>name</EM> (z.B. <EM>time</EM>)        <BR>
  !! Gr&ouml;&szlig;e der Dimension <EM>len</EM>, wird hierf&uuml;r 
  !! len = -1 angegeben, so wird die entsprechende Dimension als
  !! nicht begrenzt (<EM>unlimited</EM>) betrachtet; entspricht
  !! i.d.R. den geschriebenen Records, z.B. den Zeitpunkten bei 
  !! synoptischen Datens&auml;tzen
  TYPE , PUBLIC :: t_dim
     PRIVATE
     INTEGER                        :: id   ! 
     CHARACTER (LEN=c_len_dim_name) :: name ! 
     INTEGER                        :: len  ! 
  END TYPE t_dim
  !
  ! [C.2] Konstantwerte (Parameter)
  !
  !! maximale Anzahl der g&uuml;tigen namen f&uuml;r Dimensionen
  INTEGER, PUBLIC, PARAMETER :: c_max_dim_name=35 ! 
  !! Definition der g&uuml;ltigen Dimensionsbezeichner <BR>
  !! Position(01) : <EM>time</EM>, Anzahl der Zeitschritte; <BR>
  !! Position(02) : <EM>node</EM>, Anzahl der Knotenpunkte; <BR>
  !! Position(03) : <EM>edge</EM>, Anzahl der Kanten; <BR>
  !! Position(04) : <EM>node_in_polygon</EM>, Anzahl der Knoten/Kanten in einem Polygon; <BR>
  !! Position(05) : <EM>polygon</EM>, Anzahl der Polygone; <BR>
  !! Position(06) : <EM>cell</EM>, Anzahl der 3D-Zellen; <BR>
  !! Position(07) : <EM>face</EM>, Anzahl der Seitenfl&auml;chen von 3D-Zellen; <BR>
  !! Position(08) : <EM>layer</EM>, Anzahl der Schichten; <BR>
  !! Position(09) : <EM>layer_interfaces</EM>, Anzahl der Grenzen zwischen Schichten, 
  !!                                            inklusive <EM>Boden</EM> und <EM>Deckel</EM>; <BR>
  !! Position(10) : <EM>variant</EM>, Anzahl der Varianten einer physikalischen Gr&ouml;&szlig;e <BR>
  !! Position(11) : <EM>sediment_classes</EM>, Anzahl der Sedimentklassen <BR>
  !! Position(12) : <EM>dune_systems</EM>, Anzahl der D&uuml;nensysteme <BR>
  !! Position(13) : <EM>ripple_systems</EM>, Anzahl der Riffelsysteme <BR>
  !! Position(14) : <EM>vedge</EM>, Anzahl der vertikalen Kanten unter Knoten (f&uuml;r 
  !!                drei-dimensionale Daten <BR>
  !! Position(15) : <EM>vecdim1</EM> Dimensionalit&auml;t (1) einer vektoriellen Gr&ouml;&szlig;e,
  !!                falls der Betrag <EM>nicht</EM> enthalten ist <BR>
  !! Position(16) : <EM>vecdim_plus_amount1</EM> Dimensionalit&auml;t (1) einer vektoriellen 
  !!                Gr&ouml;&szlig;e, falls als letzte Komponente auch noch der Betrag mit 
  !!                abgespeichert wird; <BR>
  !! Position(17) : <EM>vecdim2</EM> Dimensionalit&auml;t (2) einer weiteren vektoriellen 
  !!                Gr&ouml;&szlig;e, falls der Betrag <EM>nicht</EM> enthalten ist <BR>
  !! Position(18) : <EM>vecdim_plus_amount2</EM> Dimensionalit&auml;t (2) einer vektoriellen 
  !!                Gr&ouml;&szlig;e, falls als letzte Komponente auch noch der Betrag 
  !!                mit abgespeichert wird; <BR>
  !! Position(19) : <EM>vecdim3</EM> Dimensionalit&auml;t (3) noch einer weiteren vektoriellen 
  !!                Gr&ouml;&szlig;e, falls der Betrag <EM>nicht</EM> enthalten ist <BR>
  !! Position(20) : <EM>vecdim_plus_amount3</EM> Dimensionalit&auml;t (3) noch einer vektoriellen 
  !!                Gr&ouml;&szlig;e, falls als letzte Komponente auch noch der Betrag mit 
  !!                abgespeichert wird; <BR>
  !! Position(21) : <EM>lat</EM> geographische Breite <BR>
  !! Position(22) : <EM>lat</EM> geographische L&auml;nge <BR>
  !! Position(23) : <EM>x</EM> x-Koordinate eines kartesischen Koordinatensystems <BR> 
  !! Position(24) : <EM>y</EM> y-Koordinate eines kartesischen Koordinatensystems <BR> 
  !! Position(25) : <EM>z</EM> z-Koordinate eines kartesischen Koordinatensystems  <BR>
  !! Position(26) : <EM>bedload_classes</EM>, Anzahl der Bedload-Klassen <BR>
  !! Position(27) : <EM>suspension_classes</EM>, Anzahl der Suspensions-Klassen <BR>
  !! Position(28) : <EM>frequency</EM>, Anzahl der Frequenzen <BR>
  !! Position(29) : <EM>direction</EM>, Anzahl der Richtungen <BR>
  !! Position(30) : <EM>strlen1</EM>, Stringl&auml;nge Typ 1 <BR>
  !! Position(31) : <EM>strlen2</EM>, Stringl&auml;nge Typ 2 <BR>
  !! Position(32) : <EM>strlen3</EM>, Stringl&auml;nge Typ 3 <BR>
  !! Position(33) : <EM>M</EM>, Anzahl der Zellen in M-Richtung <BR>
  !! Position(34) : <EM>N</EM>, Anzahl der Zellen in N-Richtung <BR>
  !! Position(35) : <EM>char20</EM>, String mit der L&auml;nge 20
  CHARACTER (LEN=c_len_dim_name) , PUBLIC, PARAMETER :: c_dim_name(c_max_dim_name)= & ! 
       (/ 'time                ', 'node                ', & ! 01-02
          'edge                ', 'node_in_polygon     ', & ! 03-04
          'polygon             ', 'cell                ', & ! 05-06
          'face                ', 'layer               ', & ! 07-08
          'layer_interfaces    ', 'variant             ', & ! 09-10
          'sediment_classes    ', 'dune_systems        ', & ! 11-12
          'ripple_systems      ', 'vedge               ', & ! 13-14
          'vecdim1             ', 'vecdim_plus_amount1 ', & ! 15-16
          'vecdim2             ', 'vecdim_plus_amount2 ', & ! 17-18
          'vecdim3             ', 'vecdim_plus_amount3 ', & ! 19-20
          'lat                 ', 'lon                 ', & ! 21-22
          'x                   ', 'y                   ', & ! 23-24 
          'z                   ', 'bedload_classes     ', & ! 25-26
          'suspension_classes  ', 'frequency           ', & ! 27-28
          'direction           ', 'strlen1             ', & ! 29-30
          'strlen2             ', 'strlen3             ', & ! 31-32
          'm                   ', 'n                   ', & ! 33-34
          'char20              '                         /) ! 35   
  ! *** neue Bezeichner hinten anfuegen ***
  !
  !! Indikatorvariable, ob eine der vorgenannten Dimensionen zu den Vektordimensionen z&auml;hlt
  LOGICAL , PARAMETER :: c_vector_class_dim(c_max_dim_name) = & ! 
       (/.false., .false., .false., .false., .false., .false., .false., .false., .false., .false., & ! 01-10
         .false., .false., .false., .false., .true. , .true. , .true. , .true. , .true. , .true. , & ! 11-20
         .false., .false., .false., .false., .false., .false., .false., .false., .false. ,.false., & ! 21-30
         .false., .false., .false., .false., .false. /)                                              ! 31-35
  !! Indikatorvariable, ob eine der vorgenannten Dimensionen zu den Variantenimensionen z&auml;hlt
  LOGICAL , PARAMETER :: c_variant_class_dim(c_max_dim_name) = & ! 
       (/.false., .false., .false., .false., .false., .false., .false., .false., .false., .true. , & ! 01-10
         .true. , .true. , .true. , .false., .false., .false., .false., .false., .false., .false., & ! 11-20
         .false., .false., .false., .false., .false., .true. , .true. , .true. , .true. , .false., & ! 21-30
         .false., .false., .false., .false., .false. /)                                              ! 31-35
  !! Indikatorvariable, ob eine der vorgenannten Dimensionen zu den 3D-Dimensionen z&auml;hlt
  LOGICAL , PARAMETER :: c_3d_class_dim(c_max_dim_name) = & ! 
       (/.false., .false., .false., .false., .false., .false., .false., .true. , .true. , .false., & ! 01-10
         .false., .false., .false., .true. , .false., .false., .false., .false., .true. , .true. , & ! 11-20
         .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., & ! 21-30
         .false., .false., .false., .false., .false. /)                                              ! 31-35
  !
  ! [C.3] Variablen [moeglichst nicht verwenden]
  !
  ! [C.4] Schnittstellen
  !
  ! [C.4.1] erforderliche oeffentliche Schnittstellen
  !
  !! Allokieren/Initialisieren der statischen Datenobjekte des Moduls
  INTERFACE init_dim
     MODULE PROCEDURE init_dim_d ! 
  END INTERFACE
  !! De-Allokieren/De-Initialisieren der statischen Datenobjekte des Moduls
  INTERFACE clear_dim
     MODULE PROCEDURE clear_dim_d ! 
  END INTERFACE
  !! Setzen der logischen Kanalnummer <EM>PRN_LUN</EM> f&uuml;r PRINT-Methoden; <BR>
  !! keine Ausgabe: <EM>PRN_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>PRN_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>PRN_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_dim_prn_lun
     MODULE PROCEDURE setup_dim_prn_lun_d ! 
  END INTERFACE
  !! Setzen der logischen Kanalnummer <EM>TRC_LUN</EM> f&uuml;r TRACE-Methoden; <BR>
  !! keine Ausgabe: <EM>TRC_LUN</EM> = -1; <BR>
  !! Ausgabe: <EM>TRC_LUN</EM> = >0; <BR>
  !! <EM>Hinweis</EM>: <EM>TRC_LUN</EM> in rufender Programmeinheit &ouml;ffnen.
  INTERFACE setup_dim_trc_lun
     MODULE PROCEDURE setup_dim_trc_lun_d ! 
  END INTERFACE
  !! Neues Objekt vom Typ "t_dim" erzeugen; <BR>
  !! Initialisieren mit Default-Werten: <BR>
  !! a) ein Objekt (Skalar) erzeugen; <BR>
  !! b) viele Objekte (Vektor) erzeugen.
  INTERFACE new_dim
     MODULE PROCEDURE new_dim_0  ! Version fuer Skalar
     MODULE PROCEDURE new_dim_1  ! Version fuer 1D-Array
  END INTERFACE
  !! Objekt vom Typ "t_dim" vernichten; <BR>
  !! teilweise Re-Initialisieren mit Default-Werten: <BR>
  !! a) ein Objekt (Skalar) vernichten; <BR>
  !! b) viele Objekte (Vektor) vernichten.
  INTERFACE kill_dim
     MODULE PROCEDURE kill_dim_0 ! Version fuer Skalar
     MODULE PROCEDURE kill_dim_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Objekt vom Typ "t_dim" auf G&uuml;ltigkeit pr&uuml;fen; <BR>
  !! werden mehrere Objekte in der Parameterliste &uuml;bergeben,
  !! so wird auch deren Eindeutigkeit hinsichtlich
  !! der Kennung <EM>id</EM> und des Namens <EM>name</EM> 
  !! gepr&uuml;ft; <BR>
  !! des weiteren wird bei Vorhandensein mehrerer Objekte
  !! ermittelt, ob nur eine "unlimited" Dimension vorhanden
  !! ist: <BR>
  !! a) ein Objekt (Skalar) pr&uuml;fen; <BR>
  !! b) mehrerer Objekte (Vektor) pr&uuml;fen.
  INTERFACE ok_dim
     MODULE PROCEDURE ok_dim_0 ! Version fuer Skalar
     MODULE PROCEDURE ok_dim_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Alle Komponenten des Typs "t_dim" auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0: <BR>
  !! a) ein Objekt (Skalar) ausgeben; <BR>
  !! b) viele Objekte (Vektor) ausgeben.
  INTERFACE print_dim
     MODULE PROCEDURE print_dim_0 ! Version fuer Skalar
     MODULE PROCEDURE print_dim_1 ! Version fuer 1D-Array
  END INTERFACE
  !! Alle statischen Daten des Moduls auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_dim_static
     MODULE PROCEDURE print_dim_static_d ! 
  END INTERFACE
  !! Alle Fehlermeldungen des Moduls auf <EM>PRN_LUN</EM> ausgeben; <BR>
  !! <EM>Hinweis:</EM> Ausgabe nur f&uuml;r <EM>PRN_LUN</EM> > 0.
  INTERFACE print_dim_all_errors
     MODULE PROCEDURE print_dim_all_errors_d ! 
  END INTERFACE
  !! Setze Komponente "id" in "t_dim" auf Benutzerwert <BR>
  !! <EM>id</EM> &#062; 0 muss erf&uuml;llt sein       <BR>
  !! in einem Feld von Dimensionsangaben darf jeder Wert nur ein Mal vorkommen <BR>
  !! a) eine Id auf Benutzerwert setzen <BR>
  !! b) viele Id's auf Benutzerwerte setzen
  INTERFACE set_dim_id
     MODULE PROCEDURE set_dim_id_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_dim_id_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !! Setze Komponente "name" in "t_dim" auf Benutzerwert <BR>
  !! <EM>name</EM> darf nur die in dem Feld "c_dim_name" definierten Werte annehmen <BR>
  !! in einem Feld von Dimensionsangaben darf ein Name nur ein Mal auftreten <BR>
  !! a) einen Namen auf Benutzerwert setzen <BR>
  !! b) viele Namen auf Benutzerwerte setzen
  INTERFACE set_dim_name
     MODULE PROCEDURE set_dim_name_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_dim_name_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !! Setze Komponente "len" in "t_dim" auf Benutzerwert <BR>
  !! <EM>len</EM> &#062; 0 muss f&uuml;r fest stehende Dimensionen erf&uuml;llt sein <BR>
  !! handelt es sich bei einer Dimension um eine unbegrenzte (unlimited- oder 
  !! Record-Dimension), so ist <EM>len</EM> = -1 anzugeben <BR>
  !! a) einen Namen auf Benutzerwert setzen <BR>
  !! b) viele Namen auf einen Benutzerwert setzen <BR>
  !! c) viele Namen auf viele Benutzerwerte setzen
  INTERFACE set_dim_len
     MODULE PROCEDURE set_dim_len_0_0 ! Objekt (Skalar) / Daten (Skalar)
     MODULE PROCEDURE set_dim_len_1_0 ! Objekt (Vektor) / Daten (Skalar) 
     MODULE PROCEDURE set_dim_len_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !! Setze alle Komponenten "id", "name" und "len" auf Benutzerwerte <BR>
  !! a) eine Variable auf Benutzerwerte setzen <BR>
  !! b) viele Variablen auf Benutzerwerte setzen
  INTERFACE set_dim
     MODULE PROCEDURE set_dim_0_0 ! Objekt (Skalar) / Daten (Skalar) 
     MODULE PROCEDURE set_dim_1_1 ! Objekt (Vektor) / Daten (Vektor) 
  END INTERFACE
  !! H&auml;nge eine oder mehrere Dimensionen an ein Pointer-Feld
  !! des Typs "t_dim" an <BR>
  !! a) Anf&uuml;gen einer Gr&ouml;&szlig;e <BR>
  !! b) Anf&uuml;gen mehrerer Gr&ouml;&szlig;en
  INTERFACE add_dim
     MODULE PROCEDURE add_dim_d_0
     MODULE PROCEDURE add_dim_d_1
  END INTERFACE
  !! Hole Komponente "id" aus "t_dim" <BR>
  !! a) eine Id holen <BR>
  !! b) viele Id's holen 
  INTERFACE get_dim_id
     MODULE PROCEDURE get_dim_id_0_0 ! Skalar
     MODULE PROCEDURE get_dim_id_1_0 ! Vektor
  END INTERFACE
  !! Hole Komponente "name" aus "t_dim" <BR>
  !! a) einen Namen holen <BR>
  !! b) viele Namen holen 
  INTERFACE get_dim_name
     MODULE PROCEDURE get_dim_name_0_0   ! Skalar
     MODULE PROCEDURE get_dim_name_1_0   ! Vektor
     MODULE PROCEDURE get_dim_name_1_i_0 ! Skalar (Auswahl ueber ID)
  END INTERFACE
  !! Hole Komponente "len" aus "t_dim" <BR>
  !! a) eine L&auml;nge holen <BR>
  !! b) viele L&auml;ngen holen 
  INTERFACE get_dim_len
     MODULE PROCEDURE get_dim_len_0_0 ! Skalar
     MODULE PROCEDURE get_dim_len_1_0 ! Vektor
  END INTERFACE
  !! Ermittle die Id f&uuml;r die "unlimited" Dimension;
  !! falls keine entsprechende Dimension vorhanden ist
  !! wird als Id 0 zur&uuml;ckgegeben.
  INTERFACE get_dim_unlimited_id
     MODULE PROCEDURE get_dim_unlimited_id_1 ! 
  END INTERFACE
  !! Ermittle den Namen f&uuml;r die "unlimited" Dimension;
  !! falls keine entsprechende Dimension vorhanden ist
  !! wird als Name "undefined" zur&uuml;ckgegeben
  INTERFACE get_dim_unlimited_name
     MODULE PROCEDURE get_dim_unlimited_name_1 ! 
  END INTERFACE
  !! Ermittle die Anzahl der zu den Vektorklassen geh&ouml;renden Dimensionen <BR>
  !! a) f&uuml;r eine Dimension <BR>
  !! b) f&uuml;r viele Dimensionen 
  INTERFACE get_vector_class_dim_count
     MODULE PROCEDURE get_vector_class_dim_count_0
     MODULE PROCEDURE get_vector_class_dim_count_1
  END INTERFACE
  !! Ermittle die Anzahl der zu den Skalarklassen geh&ouml;renden Dimensionen <BR>
  !! a) f&uuml;r eine Dimension <BR>
  !! b) f&uuml;r viele Dimensionen 
  INTERFACE get_variant_class_dim_count
     MODULE PROCEDURE get_variant_class_dim_count_0
     MODULE PROCEDURE get_variant_class_dim_count_1
  END INTERFACE
  !! Ermittle die Anzahl der zur drei-dimensionalen Klasse geh&ouml;renden Dimensionen <BR>
  !! a) f&uuml;r eine Dimension <BR>
  !! b) f&uuml;r mehrere Dimensionen
  INTERFACE get_3d_class_dim_count
     MODULE PROCEDURE get_3d_class_dim_count_0
     MODULE PROCEDURE get_3d_class_dim_count_1
  END INTERFACE
  !! Ermittle die Position in einer Liste der zu den Vektorklassen geh&ouml;rende 
  !! Dimension anhand der lfd. Nummer <BR>
  !! a) f&uuml;r viele Dimensionen
  INTERFACE get_vector_class_dim_idx
     MODULE PROCEDURE get_vector_class_dim_idx_1_0
  END INTERFACE
  !! Ermittle eine zu den Variantenklassen geh&ouml;rende Dimension anhand der lfd. Nummer <BR>
  !! a) f&uuml;r viele Dimensionen
  INTERFACE get_variant_class_dim_idx
     MODULE PROCEDURE get_variant_class_dim_idx_1_0
  END INTERFACE
  !
  !! Ermittle die Position einer gesuchten Dimension in einem Feld
  !! f&uuml;r verschiedene Suchkriterien (Id, Name, Dimension); 
  !! falls keine Position ermittelt werden kann, so wird
  !! der Wert 0 zur&uuml;ckgegeben:
  !! a) f&uuml;r eine Identifikationsnummer "id" <BR>
  !! b) f&uuml;r viele Identifikationsnummern "id(:)" <BR>
  !! c) f&uuml;r einen Namen "name" <BR>
  !! d) f&uuml;r viele Namen "name(:)" <BR>
  !! e) f&uuml;r eine Dimensionsangabe vom Typ "t_dim" <BR>
  !! f) f&uuml;r viele Dimensionsangaben vom Typ "t_dim"
  INTERFACE get_dim_idx
     MODULE PROCEDURE get_dim_idx_i_0
     MODULE PROCEDURE get_dim_idx_i_1
     MODULE PROCEDURE get_dim_idx_n_0
     MODULE PROCEDURE get_dim_idx_n_1
     MODULE PROCEDURE get_dim_idx_d_0
     MODULE PROCEDURE get_dim_idx_d_1
  END INTERFACE
  !! Pr&uuml;fe ob eine Dimensionsangabe in einem Feld
  !! f&uuml;r verschiedene Suchkriterien (Id, Name, Dimension) 
  !! vorhanden ist:                                     <BR>
  !! a) f&uuml;r eine Identifikationsnummer "id"        <BR>
  !! b) f&uuml;r viele Identifikationsnummern "id(:)"   <BR>
  !! c) f&uuml;r einen Namen "name"                     <BR>
  !! d) f&uuml;r viele Namen "name(:)"                  <BR>
  !! e) f&uuml;r eine Dimensionsangabe vom Typ "t_dim"  <BR>
  !! f) f&uuml;r viele Dimensionsangaben vom Typ "t_dim"
  INTERFACE dim_exists
     MODULE PROCEDURE dim_exists_i_0
     MODULE PROCEDURE dim_exists_i_1
     MODULE PROCEDURE dim_exists_n_0
     MODULE PROCEDURE dim_exists_n_1
     MODULE PROCEDURE dim_exists_d_0
     MODULE PROCEDURE dim_exists_d_1
  END INTERFACE
  !! Pr&uuml;fe, ob eine Dimensionsangabe "unlimited" ist
  !! f&uuml;r verschiedene Suchkriterien (Id, Name, Dimension) <BR>
  !! a) f&uuml;r eine Identifikationsnummer "id" <BR>
  !! b) f&uuml;r einen Namen "name" <BR>
  !! c) f&uuml;r eine Dimension des Typs "t_dim"
  INTERFACE is_unlimited_dim
     MODULE PROCEDURE is_unlimited_dim_1_i ! 
     MODULE PROCEDURE is_unlimited_dim_1_n ! 
     MODULE PROCEDURE is_unlimited_dim_0   ! 
  END INTERFACE
  !! Pr&uuml;fe, ob eine Dimensionsangabe zu den sogenannten Vektorklassen z&auml;hlt <BR>
  !! a) f&uuml;r eine Dimensionsangabe <BR>
  !! b) f&uuml;r viele Dimensionsangaben
  INTERFACE is_vector_class_dim
     MODULE PROCEDURE is_vector_class_dim_0
     MODULE PROCEDURE is_vector_class_dim_1
  END INTERFACE
  !! Pr&uuml;fe, ob eine Dimensionsangabe zu den sogenannten Variantenklassen z&auml;hlt <BR>
  !! a) f&uuml;r eine Dimensionsangabe <BR>
  !! b) f&uuml;r viele Dimensionsangaben
  INTERFACE is_variant_class_dim
     MODULE PROCEDURE is_variant_class_dim_0
     MODULE PROCEDURE is_variant_class_dim_1
  END INTERFACE
  !! Pr&uuml;fe, ob eine Dimensionsangabe zu den drei-dimensionalen Klassen z&auml;hlt <BR>
  !! a) f&uuml;r eine Dimensionsangabe <BR>
  !! b) f&uuml;r viele Dimensionsangaben
  INTERFACE is_3d_class_dim
     MODULE PROCEDURE is_3d_class_dim_0
     MODULE PROCEDURE is_3d_class_dim_1
  END INTERFACE
  !! Pr&uuml;fe, ob die "unlimited" Dimension einen bestimmten Namen
  !! tr&auml;gt oder in einer Liste zul&auml;ssiger Namen vorhanden ist:<BR>
  !! a) &Uuml;bereinstimmung mit einem bestimmten Namen <BR>
  !! b) &Uuml;bereinstimmung mit wenigstens einem in einer Liste
  !!    vorkommendem Namen <BR>
  !! c) &Uuml;bereinstimmung mit "c_dim_name(i)" <BR>
  !! d) &Uuml;bereinstimmung mit wenigstens einem in einer Liste i(:)
  !!    vorkommendem Namen "c_dim_name(i(:))" <BR>
  INTERFACE valid_unlimited_dim_name
     MODULE PROCEDURE valid_unlimited_dim_name_1_0
     MODULE PROCEDURE valid_unlimited_dim_name_1_1
     MODULE PROCEDURE valid_unlimited_dim_name_1_0_i
     MODULE PROCEDURE valid_unlimited_dim_name_1_1_i
  END INTERFACE
  !! Pr&uuml;fe, ob der Name einer vorgegebenen Dimension in
  !! einer Liste von Dimensionen vorkommt; <BR>
  !! falls dies der Fall ist, dann pr&uuml;fe zus&auml;tzlich,
  !! ob die L&auml;nge der Dimension mit der gefundenen Dimension
  !! &uuml;bereinstimmt; <BR>
  !! erzeuge eine Fehlermeldung f&uuml;r den Fall dass eine der
  !! vorgenannten Bedingungen nicht erf&uuml;llt sind: <BR>
  !! a) falls Name und L&auml;nge bekannt sind <BR>
  !! b) falls ein Objekt (t_dim) bekannt ist <BR>
  !! Anmerkung: die Id der Dimension spielt bei diesem Test keine Rolle.
  INTERFACE gen_error_if_dim_not_equal
     MODULE PROCEDURE gen_error_if_dim_not_equal_1_nl
     MODULE PROCEDURE gen_error_if_dim_not_equal_1_0
  END INTERFACE
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  ! ... ggf. ergaenzen
  !
  ! [C.5] Zuweisungen
  !
  ! ... ggf. ergaenzen
  !
  ! [C.6] Operatoren
  !
  ! [C.6.1] unbedingt erforderliche oeffentliche Operatoren
  !
  !! Pr&uuml;fung zweier Datenobjekte "t_dim" auf Gleichheit <BR>
  !! zwei Objekte sind gleich wenn sie in allen Komponenten
  !! &uuml;bereinstimmen: <BR>
  !! a) dim1    == dim2 ; <BR>
  !! a) dim1    == dim2(:) ; <BR>
  !! a) dim1(:) == dim2 ; <BR>
  !! a) dim1(:) == dim2(:) . <BR>
  INTERFACE OPERATOR(==)
     MODULE PROCEDURE eq_dim_0_0  ! Skalar / Skalar
     MODULE PROCEDURE eq_dim_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE eq_dim_1_0  ! Vektor / Skalar
     MODULE PROCEDURE eq_dim_1_1  ! Vektor / Vektor
  END INTERFACE
  INTERFACE eq_dim
     MODULE PROCEDURE eq_dim_0_0  ! Skalar / Skalar
     MODULE PROCEDURE eq_dim_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE eq_dim_1_0  ! Vektor / Skalar
     MODULE PROCEDURE eq_dim_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  ! [C.6.2] optional vorhandene oeffentliche Operatoren
  !
  !! Pr&uuml;fung zweier Datenobjekte "t_dim" auf Ungleichheit <BR>
  !! zwei Objekte sind ungleich, wenn sie in wenigstens einer
  !! Komponente verschieden sind: <BR>
  !! a) dim1    /= dim2 ; <BR>
  !! a) dim1    /= dim2(:) ; <BR>
  !! a) dim1(:) /= dim2 ; <BR>
  !! a) dim1(:) /= dim2(:) . <BR>
  INTERFACE OPERATOR(/=)
     MODULE PROCEDURE ne_dim_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ne_dim_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE ne_dim_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ne_dim_1_1  ! Vektor / Vektor
  END INTERFACE
  INTERFACE ne_dim
     MODULE PROCEDURE ne_dim_0_0  ! Skalar / Skalar
     MODULE PROCEDURE ne_dim_0_1  ! Skalar / Vektor 
     MODULE PROCEDURE ne_dim_1_0  ! Vektor / Skalar
     MODULE PROCEDURE ne_dim_1_1  ! Vektor / Vektor
  END INTERFACE
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  ! [C.7.1] unbedingt erforderliche oeffentliche Methoden
  !
  PUBLIC :: init_dim                 ! Initialisieren (Modul)
  PUBLIC :: clear_dim                ! De-Initialisieren (Modul)
  PUBLIC :: setup_dim_prn_lun        ! Setzen prn_lun 
  PUBLIC :: setup_dim_trc_lun        ! Setzen trc_lun 
  PUBLIC :: new_dim                  ! Erzeugen 
  PUBLIC :: kill_dim                 ! Vernichten
  PUBLIC :: ok_dim                   ! Pruefen
  PUBLIC :: print_dim                ! Drucken
  PUBLIC :: print_dim_static         ! Drucken aller statischen Daten
  PUBLIC :: print_dim_all_errors     ! Drucken aller (moeglichen) Fehlermeldungen
  PUBLIC :: set_dim_id               ! Setzen der Komponente id
  PUBLIC :: set_dim_name             ! Setzen der Komponente name
  PUBLIC :: set_dim_len              ! Setzen der Komponente len
  PUBLIC :: get_dim_id               ! Holen der Komponente id
  PUBLIC :: get_dim_name             ! Holen der Komponente name
  PUBLIC :: get_dim_len              ! Holen der Komponente len
  PUBLIC :: OPERATOR(==)             ! Operator "=="
  PUBLIC :: eq_dim
  !
  ! [C.7.2] optional vorhandene oeffentliche Methoden
  !
  PUBLIC :: ne_dim
  PUBLIC :: OPERATOR(/=)                ! 
  PUBLIC :: set_dim                     ! 
  PUBLIC :: add_dim                     ! 
  PUBLIC :: get_dim_unlimited_id        ! 
  PUBLIC :: get_dim_unlimited_name      ! 
  PUBLIC :: get_dim_idx                 ! 
  PUBLIC :: get_vector_class_dim_count  ! 
  PUBLIC :: get_variant_class_dim_count ! 
  PUBLIC :: get_3d_class_dim_count      ! 
  PUBLIC :: get_vector_class_dim_idx    ! 
  PUBLIC :: get_variant_class_dim_idx   ! 
  PUBLIC :: dim_exists                  ! 
  PUBLIC :: is_unlimited_dim            ! 
  PUBLIC :: is_vector_class_dim         ! 
  PUBLIC :: is_variant_class_dim        ! 
  PUBLIC :: is_3d_class_dim             ! 
  PUBLIC :: valid_unlimited_dim_name    ! 
  PUBLIC :: gen_error_if_dim_not_equal  ! 
  !
  ! ---------------------------------------------------------------------
  ! [D] modulintern zugaengliche Daten und Methoden (PRIVATE)
  ! ---------------------------------------------------------------------
  !
  ! [D.1] lokale Typdefinitionen
  !
  ! [D.2] Konstantwerte (Parameter)
  !
  !! Name des Moduls
  CHARACTER (LEN=05), PARAMETER :: c_modname      = 'b_dim' ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE/PRINT-Methoden (Default)
  LOGICAL           , PARAMETER :: c_op           = .false.          ! 
  !! Kanalnummer f&uuml;r TRACE/PRINT-Methoden (Default)
  INTEGER           , PARAMETER :: c_lun          = -1               ! 
  !! Anzahl der Datenkomponenten des Typs t_dim
  INTEGER           , PARAMETER :: c_nofcomp      = 3                ! ggf. modifizieren
  !
  ! [D.3] Variablen (statische Daten des Moduls)
  !
  !! Feld zur Aufnahme aller Fehlermeldungen des Moduls
  TYPE (t_error) , ALLOCATABLE, SAVE :: all_errors(:)! 
  !! Indikator f&uuml;r eine erfolgreich durchgef&uuml;hrte Modulinitialisierung
  LOGICAL                     , SAVE :: initialised = .false.  ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung PRINT-Methoden
  LOGICAL                     , SAVE :: prn_op      = c_op     ! 
  !! Indikator f&uuml;r Durchf&uuml;hrung TRACE-Methoden
  LOGICAL                     , SAVE :: trc_op      = c_op     ! 
  !! logische Kanalnummer f&uuml;r PRINT-Methoden
  INTEGER                     , SAVE :: prn_lun     = c_lun    ! 
  !! logische Kanalnummer f&uuml;r TRACE-Methoden
  INTEGER                     , SAVE :: trc_lun     = c_lun    ! 
  !! Z&auml;hler f&uuml;r Initialisierungsaufrufe
  INTEGER                     , SAVE :: n_init      = 0        ! 
  !
  ! [D.4] Schnittstellen
  !
  !! Umsetzen eines Textes in Kleinbuchstaben
  INTERFACE get_lowercase_char
     MODULE PROCEDURE get_lowercase_char_0
     MODULE PROCEDURE get_lowercase_char_1
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
  ! Globale Methoden mit Zugriff ueber PUBLIC Interfaces
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-INIT-Methoden <<< [ERR_NO =  1000 bis  1999]
  ! ----------------------------------------------------------------------
  !
  !! Allokieren/Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE init_dim_d ( )
    !
    USE b_error, ONLY : DEBUG_b
    !! Name der Subroutine
    CHARACTER (LEN=10), PARAMETER :: c_upname='init_dim_d' 
    !
    IF ( .NOT. initialised ) THEN
       ! [1.1] Drucken des Copyright-Hinweises
       IF (DEBUG_b > 0) THEN
          WRITE(*,*) ' '
          WRITE(*,*) ' "b_dim" version 2.3 of 08/16/06'
          WRITE(*,*) ' Copyright (C) 2002 Bundesanstalt fuer Wasserbau'
          WRITE(*,*)
       END IF
       ! [1.2] alle mit USE eingebundenen Basis-Module initialisieren
       ! [1.2.1] Error-Modul zuerst initialisieren
       CALL init_error ( )
       ! [1.2.2] ggf. weitere Module initialisieren
       ! [1.3] vorlaeufiges Setzen von "initialised"
       initialised = .true.
       ! [1.4] Allokieren/Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL init_dim_all_errors ( ) 
       ! [1.5] Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.6] endgueltiges Setzen des Initialisierungs-Indikators
       initialised = MERGE( .true., .false., no_error( ) )
    END IF
    ! 2.0 Initialisierungszaehler heraufsetzen
    n_init = n_init + 1
    !
  END SUBROUTINE init_dim_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren der statischen Daten des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_dim_d ( )
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=11), PARAMETER :: c_upname='clear_dim_d' ! 
    !
    IF ( initialised .AND. n_init == 1 ) THEN
       ! [1.1] De-Allokieren/De-Initialisieren der Fehlermeldungen
       IF ( no_error( ) ) CALL clear_dim_all_errors ( ) 
       ! [1.2] De-Initialisieren der logischen Kanalnummern
       prn_lun = c_lun
       trc_lun = c_lun
       ! [1.3] Rueck-Setzen des Initialisierungs-Indikators
       initialised = MERGE( .false., .true., no_error( ) )
       ! [1.4] alle mit USE eingebundenen Basis-Module de-initialisieren
       ! [1.4.1] ggf. weitere Module de-initialisieren
       ! [1.4.2] Error-Modul zuletzt de-initialisieren
       IF ( no_error( ) ) CALL clear_error ( )
    END IF
    ! 2.0 Initialisierungszaehler heruntersetzen
    n_init = n_init - 1
    !
  END SUBROUTINE clear_dim_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  !! Setzen der logischen Kanalnummer f&uuml;r PRINT-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_dim_prn_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r PRINT-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER :: c_upname='setup_dim_prn_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       prn_lun = MERGE(    lun,   c_lun,     lun > 0 )
       prn_op  = MERGE( .true., .false., prn_lun > 0 )
       ! "prn_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_prn_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_dim_prn_lun_d
  !
  !! Setzen der logischen Kanalnummer f&uuml;r TRACE-Methoden <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE setup_dim_trc_lun_d ( lun )
    !! aktuelle Kanalnummer f&uuml;r TRACE-Methoden 
    INTEGER , INTENT(IN) :: lun ! 
    !! Name der Subroutine
    CHARACTER (LEN=19), PARAMETER :: c_upname='setup_dim_trc_lun_d' 
    !
    IF ( ok_initialised ( c_upname ) ) THEN
       trc_lun = MERGE(    lun,   c_lun,     lun > 0 )
       trc_op  = MERGE( .true., .false., trc_lun > 0 )
       ! "trc_lun" in eingebundenen Basis-Modulen ebenfalls setzen
       IF ( no_error( ) ) CALL setup_error_trc_lun ( lun )
    END IF
    !
  END SUBROUTINE setup_dim_trc_lun_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  !! Initialisieren eines neuen Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_dim_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_dim) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=09), PARAMETER :: c_upname='new_dim_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       this%id   = 0
       this%name = REPEAT( ' ', LEN( this%name ) )
       this%name = 'undefined' 
       this%len  = 0
    END IF
    !
  END SUBROUTINE new_dim_0
  !
  !! Initialisieren eines neuen Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE new_dim_1 ( this )
    ! Formalparameter
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(INOUT) :: this(:) ! 
    ! Lokale Parameter und Variablen
    !! Name der Subroutine
    CHARACTER (LEN=09), PARAMETER :: c_upname='new_dim_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       DO i=1,SIZE(this)
          IF ( any_error( ) ) EXIT
          CALL new_dim_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE new_dim_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_dim_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_dim) , INTENT(INOUT) :: this ! 
    !! Name der Subroutine
    CHARACTER (LEN=10), PARAMETER :: c_upname='kill_dim_0' 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       IF ( no_error( ) ) CALL new_dim_0 ( this )
    END IF
    !
  END SUBROUTINE kill_dim_0
  !
  !! De-Allokieren/De-Initialisieren eines Datenobjekts (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE kill_dim_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(INOUT) :: this(:) ! 
    !! Name der Subroutine
    CHARACTER (LEN=10), PARAMETER :: c_upname='kill_dim_1' ! 
    !! Z&auml;hler      
    INTEGER                       :: i ! 
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       DO i=1,SIZE(this)
          IF ( any_error( ) ) EXIT
          CALL kill_dim_0 ( this(i) )
       END DO
    END IF
    !
  END SUBROUTINE kill_dim_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_dim_0 ( this ) &
       RESULT( ok )
    !! Datenobjekt (Skalar)
    TYPE (t_dim) , INTENT(IN) :: this ! 
    !! Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Name der Function
    CHARACTER (LEN=08), PARAMETER :: c_upname='ok_dim_0' 
    !! Lokales Feld mit Testergebnissen f&uuml;r die Komponenten
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok = .false.
    !
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       l_ok(1) = ok_dim_id    ( this )
       l_ok(2) = ok_dim_name  ( this )
       l_ok(3) = ok_dim_len   ( this )
    END IF
    !
    ok  = ALL( l_ok )
    !
  END FUNCTION ok_dim_0
  !
  !! Pr&uuml;fe ob ein g&uuml;ltiges Datenobjekt vorliegt (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  FUNCTION ok_dim_1 ( this ) &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this(:) ! 
    !! Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Function
    CHARACTER (LEN=08), PARAMETER :: c_upname='ok_dim_1' 
    !! Z&auml;hler      
    INTEGER :: i ! 
    !
    ok = .false.
    IF ( ok_initialised( c_upname ) ) THEN ! Modul muss initialisiert sein
       DO i=1,SIZE(this)
          ok(i) = ok_dim_0 ( this(i) )
       END DO
       ! alle Id's muessen verschieden sein
       WHERE ( .NOT. ok_dim_different_id ( this(:) ) )
          ok(:) = ok_dim_different_id ( this(:) )
       END WHERE
       ! alle Namen muessen verschieden sein
       WHERE ( .NOT. ok_dim_different_name ( this(:) ) )
          ok(:) = ok_dim_different_name ( this(:) )
       END WHERE
       ! die Dimension "-1" darf allenfalls ein Mal vorkommen
       WHERE ( .NOT. ok_dim_unique_unlimited ( this(:) ) )
          ok(:) = ok_dim_unique_unlimited ( this(:) )
       END WHERE
    END IF
    !
  END FUNCTION ok_dim_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt eines Datenobjektes (Skalar) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_dim_0 ( this )
    !! Datenobjekt (Skalar)
    TYPE (t_dim) , INTENT(IN) :: this ! 
    !! Name der Function
    CHARACTER (LEN=11), PARAMETER :: c_upname='print_dim_0' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat )
       IF ( stat /= 0   ) CALL setup_error_act ( all_errors(:), 7001, c_upname, c_modname, stat )
       IF ( no_error( ) ) CALL print_dim_id   ( this )
       IF ( no_error( ) ) CALL print_dim_name ( this )
       IF ( no_error( ) ) CALL print_dim_len  ( this )
       IF ( no_error( ) ) THEN
          WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7002, c_upname, c_modname, stat )
       END IF
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT('# Beginn Objekt t_dim ------------------------------')
8001 FORMAT('# Ende   Objekt t_dim ------------------------------')
    !
  END SUBROUTINE print_dim_0
  !
  !! Drucke den Inhalt eines Datenobjektes (Vektor) <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_dim_1 ( this )
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this(:) ! 
    !! Name der Function
    CHARACTER (LEN=11), PARAMETER :: c_upname='print_dim_1' 
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
          IF ( no_error( ) ) CALL print_dim_0 ( this(i) )
       END DO
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT ('# Datenobjekt-Index i = ',I10.10,' ---------------------------')
    !
  END SUBROUTINE print_dim_1
  !
  !! Drucken aller statischen Daten eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_dim_static_d ( )
    !! Name der Function
    CHARACTER (LEN=18), PARAMETER :: c_upname='print_dim_static_d' 
    !! Statusvariable
    INTEGER :: stat ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) &
            initialised, prn_op, trc_op, prn_lun, trc_lun, n_init, c_max_dim_name
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       DO i=1,c_max_dim_name
          IF ( any_error( ) ) EXIT
          WRITE ( UNIT=prn_lun, FMT=8001, IOSTAT=stat ) i, TRIM( c_dim_name(i) )
          IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       END DO
       IF ( no_error( ) ) WRITE ( UNIT=prn_lun, FMT=8002, IOSTAT=stat )
       IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7500, c_upname, c_modname, stat )
       IF ( no_error( ) ) CALL print_dim_all_errors_d ( )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
8000 FORMAT( &
    '#------------------------------------------------------------',/ &
    '# aktuelle statische Daten des Moduls b_dim         ',/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &  
    '#    initialised = ',L1,/ &
    '#         prn_op = ',L1,/ &
    '#         trc_op = ',L1,/ &
    '#        prn_lun = ',I5,/ &
    '#        trc_lun = ',I5,/ &
    '#         n_init = ',I5,/ &
    '# c_max_dim_name = ',I5,/ &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -')
8001 FORMAT( '#  i, c_dim_name : idx = ',I5,', name = ',A )
8002 FORMAT( &
    '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',/ &
    '#------------------------------------------------------------') 
    !
  END SUBROUTINE print_dim_static_d
  !
  !! Drucken aller (m&ouml;glichen) Fehler eines Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_dim_all_errors_d ( )
    !! Name der Function
    CHARACTER (LEN=22), PARAMETER :: c_upname='print_dim_all_errors_d' 
    !
    IF ( ok_initialised( c_upname ) .AND. prn_op ) THEN
       CALL print_error( all_errors(:) )
    ELSE
       WRITE(*,*) ' >>> Subroutine '//TRIM(c_upname)//' - keine Druckausgabe '
    END IF
    !
  END SUBROUTINE print_dim_all_errors_d
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-SET-Methoden <<< [ERR_NO =  8000 bis  8999]
  ! ----------------------------------------------------------------------
  !
  !! weise der Komponente "id" einen skalaren Wert zu (Skalar)                 <BR>
  !! <EM>id</EM> &#062; 0 muss erf&uuml;llt sein                               <BR>
  !! in einem Feld von Dimensionsangaben darf jeder Wert nur ein Mal vorkommen <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_dim_id_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_dim) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "id"
    INTEGER      , INTENT(IN)    :: val  ! 
    !
    this%id = val
    !
  END SUBROUTINE set_dim_id_0_0
  !
  !! weise der Komponente "id" eines Feldes Werte aus einem Vektor zu <BR>
  !! die Zuweisung erfolgt nur f&uuml;r den Fall dass beide Felder gleich gro&szlig; sind <BR>
  !! <EM>id</EM> &#062; 0 muss erf&uuml;llt sein                               <BR>
  !! in einem Feld von Dimensionsangaben darf jeder Wert nur ein Mal vorkommen <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_dim_id_1_1 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "id" (Vektor)
    INTEGER      , INTENT(IN)    :: val(:)  ! 
    !
    IF ( SIZE(this) == SIZE(val) ) this(:)%id = val(:)
    !
  END SUBROUTINE set_dim_id_1_1
  !
  !! weise der Komponente "name" einen skalaren Wert zu (Skalar) <BR>
  !! <EM>name</EM> darf nur die in dem Feld "c_dim_name" definierten Werte annehmen <BR>
  !! in einem Feld von Dimensionsangaben darf ein Name nur ein Mal auftreten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_dim_name_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_dim)      , INTENT(INOUT) :: this ! 
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
  END SUBROUTINE set_dim_name_0_0
  !
  !! weise der Komponente "name" eines Feldes Werte aus einem Vektor zu <BR>
  !! die Zuweisung erfolgt nur f&uuml;r den Fall dass beide Felder gleich gro&szlig; sind <BR>
  !! <EM>name</EM> darf nur die in dem Feld "c_dim_name" definierten Werte annehmen <BR>
  !! in einem Feld von Dimensionsangaben darf ein Name nur ein Mal auftreten <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_dim_name_1_1 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_dim)      , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "name" (Vektor)
    CHARACTER (LEN=*) , INTENT(IN)    :: val(:)  ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i = 1, MIN(SIZE(this),SIZE(val))
       CALL set_dim_name_0_0 ( this(i), val(i) )
    END DO
    !
  END SUBROUTINE set_dim_name_1_1
  !
  !! weise der Komponente "len" einen skalaren Wert zu (Skalar) <BR>
  !! <EM>len</EM> &#062; 0 muss f&uuml;r fest stehende Dimensionen erf&uuml;llt sein <BR>
  !! handelt es sich bei einer Dimension um eine unbegrenzte 
  !! (unlimited- oder Record-Dimension), so ist <EM>len</EM> = -1 anzugeben <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_dim_len_0_0 ( this, val )
    !! Datenobjekt (Skalar)
    TYPE (t_dim) , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "len"
    INTEGER      , INTENT(IN)    :: val  ! 
    !
    this%len = val
    !
  END SUBROUTINE set_dim_len_0_0
  !
  !! weise der Komponente "len" eines Feldes Werte aus einem Skalar zu <BR>
  !! <EM>len</EM> &#062; 0 muss f&uuml;r fest stehende Dimensionen erf&uuml;llt sein <BR>
  !! handelt es sich bei einer Dimension um eine unbegrenzte 
  !! (unlimited- oder Record-Dimension), so ist <EM>len</EM> = -1 anzugeben <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_dim_len_1_0 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponenten "len"
    INTEGER      , INTENT(IN)    :: val     ! 
    !
    this(:)%len = val
    !
  END SUBROUTINE set_dim_len_1_0
  !
  !! weise der Komponente "len" eines Feldes Werte aus einem Vektor zu <BR>
  !! die Zuweisung erfolgt nur f&uuml;r den Fall dass beide Felder gleich gro&szlig; sind <BR>
  !! <EM>len</EM> &#062; 0 muss f&uuml;r fest stehende Dimensionen erf&uuml;llt sein <BR>
  !! handelt es sich bei einer Dimension um eine unbegrenzte 
  !! (unlimited- oder Record-Dimension), so ist <EM>len</EM> = -1 anzugeben <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_dim_len_1_1 ( this, val )
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(INOUT) :: this(:) ! 
    !! Werte f&uuml;r Komponente "len" (Vektor)
    INTEGER      , INTENT(IN)    :: val(:)  ! 
    !
    IF ( SIZE(this) == SIZE(val) ) this(:)%len = val(:)
    !
  END SUBROUTINE set_dim_len_1_1
  !
  !! weise den Komponenten "id", "name" und "len" skalare Werte zu <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_dim_0_0 ( this, val1, val2, val3 )
    !! Datenobjekt (Skalar)
    TYPE (t_dim)      , INTENT(INOUT) :: this ! 
    !! Wert f&uuml;r Komponente "id"
    INTEGER           , INTENT(IN)    :: val1 ! 
    !! Wert f&uuml;r Komponente "name"
    CHARACTER (LEN=*) , INTENT(IN)    :: val2 ! 
    !! Wert f&uuml;r Komponente "len"
    INTEGER           , INTENT(IN)    :: val3 ! 
    !
    this%id   = val1
    this%name = val2
    this%len  = val3
    !
  END SUBROUTINE set_dim_0_0
  !
  !! weise f&uuml;r ein Feld den Komponenten "id", "name" und "len" 
  !! aus einem Vektor die entsprechenden Werte zu <BR>
  !! die Zuweisung erfolgt nur f&uuml;r den Fall dass die Felder gleich gro&szlig; sind <BR>
  !! Subroutine erzeugt <EM>keine</EM> Fehlermeldungen
  SUBROUTINE set_dim_1_1 ( this, val1, val2, val3 )
    !! Datenobjekt (Vektor)
    TYPE (t_dim)      , INTENT(INOUT) :: this(:) ! 
    !! Wert f&uuml;r Komponente "id" (Vektor)
    INTEGER           , INTENT(IN)    :: val1(:) ! 
    !! Wert f&uuml;r Komponente "name" (Vektor)
    CHARACTER (LEN=*) , INTENT(IN)    :: val2(:) ! 
    !! Wert f&uuml;r Komponente "len" (Vektor)
    INTEGER           , INTENT(IN)    :: val3(:)  ! 
    !
    IF ( SIZE(this) == SIZE(val1) ) this(:)%id   = val1(:)
    IF ( SIZE(this) == SIZE(val2) ) this(:)%name = val2(:)
    IF ( SIZE(this) == SIZE(val3) ) this(:)%len  = val3(:)
    !
  END SUBROUTINE set_dim_1_1
  !
  !! Anh&auml;ngen einer Dimension an ein Pointer-Feld mit Dimensionsangaben
  SUBROUTINE add_dim_d_0 ( this1, this2 )
    !! Pointer-Feld an das eine weitere Dimensionsangabe angeh&auml;ngt werden soll
    TYPE (t_dim) , POINTER     :: this1(:) !
    !! anzuh&auml;ngende Dimension
    TYPE (t_dim) , INTENT(IN)  :: this2    !
    !! Name der Subroutine
    CHARACTER (LEN=11) , PARAMETER :: c_upname='add_dim_d_0' ! 
    !! lokales Hilfsfeld
    TYPE (t_dim) , ALLOCATABLE :: this3(:) ! 
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
  END SUBROUTINE add_dim_d_0
  !
  !! Anh&auml;ngen mehrere Dimensionen an ein Pointer-Feld mit Dimensionsangaben
  SUBROUTINE add_dim_d_1 ( this1, this2 )
    !! Pointer-Feld an das eine weitere Dimensionsangabe angeh&auml;ngt werden soll
    TYPE (t_dim) , POINTER    :: this1(:) !
    !! anzuh&auml;ngende Dimensionen
    TYPE (t_dim) , INTENT(IN) :: this2(:) !
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this2)
       IF ( any_error( ) ) EXIT
       CALL add_dim_d_0 ( this1, this2(i) )
    END DO
    !
  END SUBROUTINE add_dim_d_1
  !  
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-GET-Methoden <<< [ERR_NO =  9000 bis  9999]
  ! ----------------------------------------------------------------------
  !
  !! hole die Komponente "id" aus einem skalaren Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_dim_id_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_dim) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "id" (Skalar)
    INTEGER :: val  ! 
    !
    val = this%id
    !
  END FUNCTION get_dim_id_0_0
  !
  !! hole die Komponente "id" aus einem vektoriellen Datenobjekt <BR>
  !! es wird eine Kopie der Daten als Vektor zur&uuml;ckgegeben <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_dim_id_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "id" (Vektor)
    INTEGER :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%id
    !
  END FUNCTION get_dim_id_1_0
  !
  !! hole die Komponente "name" aus einem skalaren Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_dim_name_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_dim)     , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "name" (Skalar)
    CHARACTER (LEN=LEN(this%name)) :: val  ! 
    !
    val = this%name
    !
  END FUNCTION get_dim_name_0_0
  !
  !! hole die Komponente "name" aus einem vektoriellen Datenobjekt <BR>
  !! es wird eine Kopie der Daten als Vektor zur&uuml;ckgegeben <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_dim_name_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_dim)     , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "name"
    CHARACTER (LEN=LEN(this%name)) :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%name
    !
  END FUNCTION get_dim_name_1_0
  !
  !! hole die Komponente "name" aus einem vektoriellen Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben <BR>
  !! die Auswahl des Objektes aus der Liste erfolgt ueber die ID <BR>
  !! "name"='undefined', falls gesuchtes Objekt nicht existiert  <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_dim_name_1_i_0 ( this, id ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN)  :: this(:) ! 
    !! Identifikationsnummer des gesuchten Objekts
    INTEGER      , INTENT(IN)  :: id      ! 
    !! R&uuml;ckgabewert "name"
    CHARACTER (LEN=LEN(this%name)) :: val ! 
    !! Index des Objekts : idx=0, falls nicht gefunden
    INTEGER :: idx ! 
    !
    val = REPEAT( ' ', LEN( val ) )
    idx = get_dim_idx_i_0  ( this, id )
    IF ( idx > 0 ) THEN
       val = this(idx)%name
    ELSE
       val = 'undefined'
    END IF
    !
  END FUNCTION get_dim_name_1_i_0
  !
  !! hole die Komponente "len" aus einem skalaren Datenobjekt <BR>
  !! es wird eine Kopie der Daten zur&uuml;ckgegeben <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_dim_len_0_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Skalar)
    TYPE (t_dim) , INTENT(IN)  :: this ! 
    !! R&uuml;ckgabewert "len" (Skalar)
    INTEGER :: val  ! 
    !
    val = this%len
    !
  END FUNCTION get_dim_len_0_0
  !
  !! hole die Komponente "len" aus einem vektoriellen Datenobjekt <BR>
  !! es wird eine Kopie der Daten als Vektor zur&uuml;ckgegeben <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_dim_len_1_0 ( this ) &
       RESULT( val ) 
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN)  :: this(:) ! 
    !! R&uuml;ckgabewert "len"
    INTEGER :: val(SIZE(this))  ! 
    !
    val(:) = this(:)%len
    !
  END FUNCTION get_dim_len_1_0
  !
  !! Ermittle die Id der "unlimited" Dimension <BR>
  !! falls keine entsprechende Dimension vorhanden ist
  !! wird als Id 0 zur&uuml;ckgegeben <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_dim_unlimited_id_1 ( this ) &
       RESULT( id )
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this(:) ! 
    !! R&uuml;ckgabewert : Id der "unlimited" Dimension (id = 0 falls nicht gefunden)
    INTEGER :: id ! 
    !! Z&auml;hlervariable
    INTEGER :: i  ! 
    !
    id = 0
    DO i=1,SIZE(this)
       IF ( id /= 0 ) EXIT
       IF ( this(i)%len == -1 ) id = this(i)%id
    END DO
    ! 
  END FUNCTION get_dim_unlimited_id_1
  !
  !! Ermittle den Namen der "unlimited" Dimension <BR>
  !! falls keine entsprechende Dimension vorhanden ist
  !! wird als Name "undefined" zur&uuml;ckgegeben <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_dim_unlimited_name_1 ( this ) &
         RESULT( name )
    !! Datenobjekt (Vektor)
    TYPE (t_dim)      , INTENT(IN) :: this(:) ! 
    !! R&uuml;ckgabewert : Name der "unlimited" Dimension (Name = 'undefined' falls nicht gefunden)
    CHARACTER (LEN=LEN(this%name)) :: name    ! 
    !! Hilfsvariable
    INTEGER :: id, idx ! 
    !
    id = get_dim_unlimited_id ( this(:) )
    IF ( id > 0 ) THEN
       idx  = get_dim_idx ( this(:), id )
       name = this(idx)%name
    ELSE
       name = REPEAT( ' ', LEN(name) )
       name = 'undefined'
    END IF
    ! 
  END FUNCTION get_dim_unlimited_name_1
  !
  !! Ermittle die Anzahl der zu den Vektorklassen geh&ouml;renden Dimensionen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_vector_class_dim_count_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_dim) , INTENT(IN) :: this ! 
    !! Ergebnis: Anzahl der zu Vektorklassen geh&ouml;renden Dimensionen
    INTEGER                   :: res  ! 
    !
    res = MERGE( 1, 0, is_vector_class_dim( this ) )
    ! 
  END FUNCTION get_vector_class_dim_count_0
  !
  !! Ermittle die Anzahl der zu den Vektorklassen geh&ouml;renden Dimensionen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_vector_class_dim_count_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Anzahl der zu Vektorklassen geh&ouml;renden Dimensionen
    INTEGER                   :: res     ! 
    !! Hilfsvariable
    INTEGER                   :: i  !     
    !
    res = 0
    DO i=1,SIZE(this)
       res = res + get_vector_class_dim_count_0 ( this(i) )
    END DO
    ! 
  END FUNCTION get_vector_class_dim_count_1
  !
  !! Ermittle die Anzahl der zu den Variantenklassen geh&ouml;renden Dimensionen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_variant_class_dim_count_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_dim) , INTENT(IN) :: this ! 
    !! Ergebnis: Anzahl der zu Variantenklassen geh&ouml;renden Dimensionen
    INTEGER                   :: res  ! 
    !
    res = MERGE( 1, 0, is_variant_class_dim( this ) )
    ! 
  END FUNCTION get_variant_class_dim_count_0
  !
  !! Ermittle die Anzahl der zu den Variantenklassen geh&ouml;renden Dimensionen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_variant_class_dim_count_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Anzahl der zu Variantenklassen geh&ouml;renden Dimensionen
    INTEGER                   :: res     ! 
    !! Hilfsvariable
    INTEGER                   :: i  !     
    !
    res = 0
    DO i=1,SIZE(this)
       res = res + get_variant_class_dim_count_0 ( this(i) )
    END DO
    ! 
  END FUNCTION get_variant_class_dim_count_1
  !
  !! Ermittle die Anzahl der zu den 3D-Klassen geh&ouml;renden Dimensionen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_3d_class_dim_count_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_dim) , INTENT(IN) :: this ! 
    !! Ergebnis: Anzahl der zu den 3D-Klassen geh&ouml;renden Dimensionen
    INTEGER                   :: res  ! 
    !
    res = MERGE( 1, 0, is_3d_class_dim( this ) )
    ! 
  END FUNCTION get_3d_class_dim_count_0
  !
  !! Ermittle die Anzahl der zu den 3D-Klassen geh&ouml;renden Dimensionen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_3d_class_dim_count_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Anzahl der zu den 3D-Klassen geh&ouml;renden Dimensionen
    INTEGER                   :: res     ! 
    !! Hilfsvariable
    INTEGER                   :: i  !     
    !
    res = 0
    DO i=1,SIZE(this)
       res = res + get_3d_class_dim_count_0 ( this(i) )
    END DO
    ! 
  END FUNCTION get_3d_class_dim_count_1
  !
  !! Ermittle den Positionsindex der n-ten zu den Vektorklassen geh&ouml;renden Dimension
  !! in einer Liste von Dimensionen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_vector_class_dim_idx_1_0 ( this, n ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this(:) ! 
    !! lfd. Nummer der Vektordimension
    INTEGER      , INTENT(IN) :: n       ! 
    !! Ergebnis : Index der n-ten Vektordimension in this(:) oder 0 falls nicht vorhanden
    INTEGER :: res ! 
    !! Hilfsvariable
    LOGICAL :: ll(SIZE(this)) ! 
    INTEGER :: i, m ! 
    !
    res   = 0 
    m     = 0
    ll(:) = is_vector_class_dim( this(:) )
    DO i=1,SIZE(ll)
       IF ( res /= 0 ) EXIT
       IF ( ll(i)    ) m = m + 1
       IF ( m == n   ) res = i
    END DO
    ! 
  END FUNCTION get_vector_class_dim_idx_1_0
  !
  !! Ermittle den Positionsindex der n-ten zu den Variantenklassen geh&ouml;renden Dimension
  !! in einer Liste von Dimensionen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_variant_class_dim_idx_1_0 ( this, n ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this(:) ! 
    !! lfd. Nummer der Vektordimension
    INTEGER      , INTENT(IN) :: n       ! 
    !! Ergebnis : Index der n-ten Variantendimension in this(:) oder 0 falls nicht vorhanden
    INTEGER :: res ! 
    !! Hilfsvariable
    LOGICAL :: ll(SIZE(this)) ! 
    INTEGER :: i, m ! 
    !
    res   = 0 
    m     = 0
    ll(:) = is_variant_class_dim( this(:) )
    DO i=1,SIZE(ll)
       IF ( res /= 0 ) EXIT
       IF ( ll(i)    ) m = m + 1
       IF ( m == n   ) res = i
    END DO
    ! 
  END FUNCTION get_variant_class_dim_idx_1_0
  !
  !! Ermittle ob eine gesuchte Dimension "unlimited" ist
  !! f&uuml;r eine Dimension des Typs "t_dim" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_unlimited_dim_0 &
       ( this ) &
         RESULT( ok )
    !! Datenobjekt 
    TYPE (t_dim) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert : .true./.false.
    LOGICAL :: ok ! 
    !
    ok = ( this%len == -1 )
    !
  END FUNCTION is_unlimited_dim_0
  !
  !! Ermittle, ob eine Dimension zu den sogenannten Vektorklassen z&auml;hlt <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_vector_class_dim_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_dim) , INTENT(IN) :: this ! 
    !! Ergebnis: Testergebnis
    LOGICAL :: res ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    res = .false.
    DO i=1,SIZE(c_dim_name)
       IF ( res ) EXIT
       IF ( c_vector_class_dim(i) .AND. &
            get_lowercase_char(c_dim_name(i)) == get_lowercase_char(this%name) ) res = .true.
    END DO
    !
  END FUNCTION is_vector_class_dim_0
  !
  !! Ermittle, welche Dimensionen zu den sogenannten Vektorklassen z&auml;hlen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_vector_class_dim_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Testergebnis
    LOGICAL :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       res(i) = is_vector_class_dim_0 ( this(i) )
    END DO
    !
  END FUNCTION is_vector_class_dim_1
  !
  !! Ermittle, ob eine Dimension zu den sogenannten Variantenklassen z&auml;hlt <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_variant_class_dim_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_dim) , INTENT(IN) :: this ! 
    !! Ergebnis: Testergebnis
    LOGICAL :: res ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    res = .false.
    DO i=1,SIZE(c_dim_name)
       IF ( res ) EXIT
       IF ( c_variant_class_dim(i) .AND. &
            get_lowercase_char(c_dim_name(i)) == get_lowercase_char(this%name) ) res = .true.
    END DO
    !
  END FUNCTION is_variant_class_dim_0
  !
  !! Ermittle, welche Dimensionen zu den sogenannten Variantenklassen z&auml;hlen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_variant_class_dim_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Testergebnis
    LOGICAL :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       res(i) = is_variant_class_dim_0 ( this(i) )
    END DO
    !
  END FUNCTION is_variant_class_dim_1
  !
  !! Ermittle, ob eine Dimension zu den sogenannten 3D-Klassen z&auml;hlt <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_3d_class_dim_0 ( this ) &
       RESULT( res )
    !! Datenobjekt (Skalar)
    TYPE (t_dim) , INTENT(IN) :: this ! 
    !! Ergebnis: Testergebnis
    LOGICAL :: res ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    res = .false.
    DO i=1,SIZE(c_dim_name)
       IF ( res ) EXIT
       IF ( c_3d_class_dim(i) .AND. &
            get_lowercase_char(c_dim_name(i)) == get_lowercase_char(this%name) ) res = .true.
    END DO
    !
  END FUNCTION is_3d_class_dim_0
  !
  !! Ermittle, welche Dimensionen zu den sogenannten den 3D-Klassen z&auml;hlen <BR>
  !! Funktion erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_3d_class_dim_1 ( this ) &
       RESULT( res )
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this(:) ! 
    !! Ergebnis: Testergebnis
    LOGICAL :: res(SIZE(this)) ! 
    !! Hilfsvariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(this)
       res(i) = is_3d_class_dim_0 ( this(i) )
    END DO
    !
  END FUNCTION is_3d_class_dim_1
  !
  !! Ermittle ob eine gesuchte Dimension "unlimited" ist
  !! f&uuml;r eine Identifikationsnummer "id" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_unlimited_dim_1_i ( this, id ) &
         RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this(:) ! 
    !! Identifikationsnummer des gesuchten Objekts
    INTEGER      , INTENT(IN) :: id      ! 
    !! R&uuml;ckgabewert : .true./.false.
    LOGICAL :: ok ! 
    !
    ok = ( get_dim_unlimited_id( this(:) ) == id )
    !
  END FUNCTION is_unlimited_dim_1_i
  !
  !! Ermittle ob der Name der "unlimited" Dimension mit einem bestimmten
  !! vorgegebenem Namen &uuml;bereinstimmt oder nicht <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION valid_unlimited_dim_name_1_0 ( this, name ) &
       RESULT(ok)
    !! Datenobjekt (Vektor)
    TYPE (t_dim)      , INTENT(IN) :: this(:) ! 
    !! vorgegebener Name der "unlimited" Dimension
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! R&uuml;ckgabewert : Testergebnis <BR>
    !! falls keine "unlimited" Dimension vorhanden ist, wird der Wert
    !! .false. zur&uuml;ckgegeben
    LOGICAL :: ok ! 
    !
    IF ( get_dim_unlimited_id(this(:)) > 0 ) THEN
       ok = ( TRIM(get_lowercase_char(get_dim_unlimited_name(this(:)))) == TRIM(get_lowercase_char(name)) )
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION valid_unlimited_dim_name_1_0
  !
  !! Ermittle ob der Name der "unlimited" Dimension mit wenigstens 
  !! einem Namen in einer Liste von Namen &uuml;bereinstimmt oder nicht <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION valid_unlimited_dim_name_1_1 ( this, name ) &
       RESULT(ok)
    !! Datenobjekt (Vektor)
    TYPE (t_dim)      , INTENT(IN) :: this(:) ! 
    !! Liste zul&auml;ssiger Namen f&uuml;r die "unlimited" Dimension
    CHARACTER (LEN=*) , INTENT(IN) :: name(:) ! 
    !! R&uuml;ckgabewert : Testergebnis <BR>
    !! falls keine "unlimited" Dimension vorhanden ist, wird der Wert
    !! .false. zur&uuml;ckgegeben
    LOGICAL :: ok               ! 
    !! Hilfsfeld
    LOGICAL :: l_ok(SIZE(name)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i                ! 
    !
    DO i=1,SIZE(l_ok)
       l_ok(i) = valid_unlimited_dim_name_1_0 ( this(:), name(i) )
    END DO
    ok = ANY( l_ok(:) )
    !
  END FUNCTION valid_unlimited_dim_name_1_1
  !
  !! Ermittle ob der Name der "unlimited" Dimension mit dem 
  !! Namen "c_dim_name(i)" &uuml;bereinstimmt oder nicht <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION valid_unlimited_dim_name_1_0_i ( this, idx ) &
       RESULT(ok)
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this(:) ! 
    !! Zeiger auf Position in "c_dim_name(:)"
    INTEGER      , INTENT(IN) :: idx     ! 
    !! R&uuml;ckgabewert : Testergebnis <BR>
    !! falls keine "unlimited" Dimension vorhanden ist, wird der Wert
    !! .false. zur&uuml;ckgegeben, ebenso dann wenn "idx" nicht auf
    !! einen Wert aus "c_dim_name(:)" weist
    LOGICAL :: ok ! 
    !
    IF ( 1 <= idx .AND. idx <= SIZE(c_dim_name) ) THEN
       ok = valid_unlimited_dim_name(this(:),c_dim_name(idx))
    ELSE
       ok = .false.
    END IF
    !
  END FUNCTION valid_unlimited_dim_name_1_0_i
  !
  !! Ermittle ob der Name der "unlimited" Dimension mit wenigstens 
  !! einem Namen in einer Liste von Namen &uuml;bereinstimmt oder nicht <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION valid_unlimited_dim_name_1_1_i ( this, idx ) &
       RESULT(ok)
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this(:) ! 
    !! Zeiger "idx(:)" auf Positionen in "c_dim_name(:)"
    INTEGER      , INTENT(IN) :: idx(:)  ! 
    !! R&uuml;ckgabewert : Testergebnis <BR>
    !! falls keine "unlimited" Dimension vorhanden ist, wird der Wert
    !! .false. zur&uuml;ckgegeben
    LOGICAL :: ok              ! 
    !! Hilfsfeld
    LOGICAL :: l_ok(SIZE(idx)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i               ! 
    !
    DO i=1,SIZE(l_ok)
       l_ok(i) = valid_unlimited_dim_name_1_0_i ( this(:), idx(i) )
    END DO
    ok = ANY( l_ok(:) )
    !
  END FUNCTION valid_unlimited_dim_name_1_1_i
  !
  !! Ermittle ob eine gesuchte Dimension "unlimited" ist
  !! f&uuml;r einen Namen "name" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION is_unlimited_dim_1_n ( this, name ) &
         RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_dim)      , INTENT(IN) :: this(:) ! 
    !! Identifikationsnummer des gesuchten Objekts
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! R&uuml;ckgabewert : .true./.false.
    LOGICAL :: ok ! 
    !
    ok = ( TRIM(get_lowercase_char(get_dim_unlimited_name(this(:)))) == TRIM(get_lowercase_char(name)))
    !
  END FUNCTION is_unlimited_dim_1_n
  !
  !! Ermittle die Position einer gesuchten Dimension in einem Feld
  !! f&uuml;r eine Identifikationsnummer "id" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_dim_idx_i_0 ( this, id ) &
       RESULT( idx )
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this(:) ! 
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
  END FUNCTION get_dim_idx_i_0
  !
  !! Ermittle die Position einer gesuchten Dimension in einem Feld
  !! f&uuml;r mehrere vorgegebene Identifikationsnummern "id" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_dim_idx_i_1 ( this, id ) &
       RESULT( idx )
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this(:) ! 
    !! Identifikationsnummern der gesuchten Objekte
    INTEGER      , INTENT(IN) :: id(:)   ! 
    !! R&uuml;ckgabewert : Feldindices (idx = 0 falls nicht gefunden)
    INTEGER :: idx(SIZE(id)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(id)
       idx(i) = get_dim_idx_i_0 ( this(:), id(i) )
    END DO
    ! 
  END FUNCTION get_dim_idx_i_1
  !
  !! Ermittle die Position einer gesuchten Dimension in einem Feld
  !! f&uuml;r einen vorgegebenen Namen "name" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_dim_idx_n_0 ( this, name ) &
       RESULT( idx )
    !! Datenobjekt (Vektor)
    TYPE (t_dim)      , INTENT(IN) :: this(:) ! 
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
  END FUNCTION get_dim_idx_n_0
  !
  !! Ermittle die Position einer gesuchten Dimension in einem Feld
  !! f&uuml;r mehrere vorgegebene Namen "name" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_dim_idx_n_1 ( this, name ) &
       RESULT( idx )
    !! Datenobjekt (Vektor)
    TYPE (t_dim)      , INTENT(IN) :: this(:) ! 
    !! Namen der gesuchten Objekte
    CHARACTER (LEN=*) , INTENT(IN) :: name(:)   ! 
    !! R&uuml;ckgabewert : Feldindices (idx = 0 falls nicht gefunden)
    INTEGER :: idx(SIZE(name)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(name)
       idx(i) = get_dim_idx_n_0 ( this(:), name(i) )
    END DO
    ! 
  END FUNCTION get_dim_idx_n_1
  !
  !! Ermittle die Position einer gesuchten Dimension in einem Feld
  !! f&uuml;r eine vorgegebene Dimensionsangabe vom Typ "t_dim" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_dim_idx_d_0 ( this, dim ) &
       RESULT( idx )
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this(:) ! 
    !! Identifikationsnummer des gesuchten Objekts
    TYPE (t_dim) , INTENT(IN) :: dim     ! 
    !! R&uuml;ckgabewert : Feldindex (idx = 0 falls nicht gefunden)
    INTEGER :: idx ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    idx = 0
    DO i=1,SIZE(this)
       IF ( idx /= 0       ) EXIT
       IF ( this(i) == dim ) idx = i
    END DO
    ! 
  END FUNCTION get_dim_idx_d_0
  !
  !! Ermittle die Position einer gesuchten Dimension in einem Feld
  !! f&uuml;r mehrere vorgegebene Dimensionen vom Typ "t_dim" <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION get_dim_idx_d_1 ( this, dim ) &
       RESULT( idx )
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this(:) ! 
    !! Dimensionen der gesuchten Objekte
    TYPE (t_dim) , INTENT(IN) :: dim(:)  ! 
    !! R&uuml;ckgabewert : Feldindices (idx = 0 falls nicht gefunden)
    INTEGER :: idx(SIZE(dim)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(dim)
       idx(i) = get_dim_idx_d_0 ( this(:), dim(i) )
    END DO
    ! 
  END FUNCTION get_dim_idx_d_1
  !
  !! Ermittle ob in einem Feld eine Dimension mit vorgegebener
  !! Identifikationsnummer "id" existiert <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION dim_exists_i_0 ( this, id ) &
       RESULT( ex )
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this(:) ! 
    !! Identifikationsnummer des gesuchten Objekts
    INTEGER      , INTENT(IN) :: id      ! 
    !! R&uuml;ckgabewert : .true./.false.
    LOGICAL :: ex ! 
    !
    ex = ( get_dim_idx_i_0 ( this(:), id ) > 0 )
    ! 
  END FUNCTION dim_exists_i_0
  !
  !! Ermittle ob in einem Feld Dimensionen mit vorgegebenen
  !! Identifikationsnummern "id" vorhanden sind <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION dim_exists_i_1 ( this, id ) &
       RESULT( ex )
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this(:) ! 
    !! Identifikationsnummern der gesuchten Objekte
    INTEGER      , INTENT(IN) :: id(:)   ! 
    !! R&uuml;ckgabewert : .true./.false. f&uuml;r alle Id's
    LOGICAL :: ex(SIZE(id)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ex)
       ex(i) = dim_exists_i_0 ( this(:), id(i) )
    END DO
    ! 
  END FUNCTION dim_exists_i_1
  !
  !! Ermittle ob in einem Feld eine Dimension mit vorgegebenem
  !! Namen "name" existiert <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION dim_exists_n_0 ( this, name ) &
       RESULT( ex )
    !! Datenobjekt (Vektor)
    TYPE (t_dim)      , INTENT(IN) :: this(:) ! 
    !! Name des gesuchten Objekts
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! R&uuml;ckgabewert : .true./.false.
    LOGICAL :: ex ! 
    !
    ex = ( get_dim_idx_n_0 ( this(:), name ) > 0 )
    ! 
  END FUNCTION dim_exists_n_0
  !
  !! Ermittle ob in einem Feld Dimensionen mit vorgegebenen
  !! Namen "name" vorhanden sind <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION dim_exists_n_1 ( this, name ) &
       RESULT( ex )
    !! Datenobjekt (Vektor)
    TYPE (t_dim)      , INTENT(IN) :: this(:) ! 
    !! Namen der gesuchten Objekte
    CHARACTER (LEN=*) , INTENT(IN) :: name(:) ! 
    !! R&uuml;ckgabewert : .true./.false. f&uuml;r alle Namen
    LOGICAL :: ex(SIZE(name)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ex)
       ex(i) = dim_exists_n_0 ( this(:), name(i) )
    END DO
    ! 
  END FUNCTION dim_exists_n_1
  !
  !! Ermittle ob in einem Feld eine Dimension mit vorgegebener
  !! Dimension "dim" existiert <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION dim_exists_d_0 ( this, dim ) &
       RESULT( ex )
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this(:) ! 
    !! Dimension des gesuchten Objekts
    TYPE (t_dim) , INTENT(IN) :: dim     ! 
    !! R&uuml;ckgabewert : .true./.false.
    LOGICAL :: ex ! 
    !
    ex = ( get_dim_idx_d_0 ( this(:), dim ) > 0 )
    ! 
  END FUNCTION dim_exists_d_0
  !
  !! Ermittle ob in einem Feld Dimensionen mit vorgegebenen
  !! Dimensionen "dim" vorhanden sind <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION dim_exists_d_1 ( this, dim ) &
       RESULT( ex )
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this(:) ! 
    !! Dimensionen der gesuchten Objekte
    TYPE (t_dim) , INTENT(IN) :: dim(:)  ! 
    !! R&uuml;ckgabewert : .true./.false. f&uuml;r alle Id's
    LOGICAL :: ex(SIZE(dim)) ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ex)
       ex(i) = dim_exists_d_0 ( this(:), dim(i) )
    END DO
    ! 
  END FUNCTION dim_exists_d_1
  !
  ! ----------------------------------------------------------------------
  ! >>> PUBLIC-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Skalar ) <BR>
  !! zwei Objekte werden dann als gleich betrachtet, wenn sie in
  !! allen ihren Komponenten &uuml;bereinstimmen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_dim_0_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_dim) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_dim) , INTENT(IN) :: this2 ! 
    !! R&uuml;ckgabewert: Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !! Hilfsfeld
    LOGICAL :: l_ok(c_nofcomp) ! 
    !
    l_ok(1) = ( this1%id   == this2%id   )
    l_ok(2) = ( get_lowercase_char(this1%name) == get_lowercase_char(this2%name) )
    l_ok(3) = ( this1%len  == this2%len  )
    ok      = ALL( l_ok )
    !
  END FUNCTION eq_dim_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Skalar ) <BR>
  !! zwei Objekte werden dann als gleich betrachtet, wenn sie in
  !! allen ihren Komponenten &uuml;bereinstimmen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_dim_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_dim) , INTENT(IN) :: this2    ! 
    !! R&uuml;ckgabewert: Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_dim_0_0( this1(i), this2 )
    END DO
    !
  END FUNCTION eq_dim_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Skalar / Vektor ) <BR>
  !! zwei Objekte werden dann als gleich betrachtet, wenn sie in
  !! allen ihren Komponenten &uuml;bereinstimmen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_dim_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_dim) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this2(:) ! 
    !! R&uuml;ckgabewert: Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    ! 
    DO i=1,SIZE(ok)
       ok(i) = eq_dim_0_0( this1, this2(i) )
    END DO
    !
  END FUNCTION eq_dim_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Gleichheit ( Vektor / Vektor ) <BR>
  !! zwei Objekte werden dann als gleich betrachtet, wenn sie in
  !! allen ihren Komponenten &uuml;bereinstimmen <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION eq_dim_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this2(:) ! 
    !! R&uuml;ckgabewert: Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !! Z&auml;hler
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = eq_dim_0_0( this1(i), this2(i) )
    END DO
    !
  END FUNCTION eq_dim_1_1
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
  !! zwei Objekte werden als ungleich betrachtet insofern sie sich in
  !! mindestens einer Komponente unterscheiden <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_dim_0_0 ( this1, this2 ) &
         RESULT( ok )
    ! Formalparameter
    !! Objekt 1 (Skalar)
    TYPE (t_dim) , INTENT(IN) :: this1 ! 
    !! Objekt 2 (Skalar)
    TYPE (t_dim) , INTENT(IN) :: this2 ! 
    !! R&uuml;ckgabewert: Testergebnis (Skalar)
    LOGICAL :: ok ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_dim_0_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Skalar ) <BR>
  !! zwei Objekte werden als ungleich betrachtet insofern sie sich in
  !! mindestens einer Komponente unterscheiden <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_dim_1_0 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Skalar)
    TYPE (t_dim) , INTENT(IN) :: this2    ! 
    !! R&uuml;ckgabewert: Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this1)) ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_dim_1_0
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Skalar / Vektor )<BR>
  !! zwei Objekte werden als ungleich betrachtet insofern sie sich in
  !! mindestens einer Komponente unterscheiden <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen 
  FUNCTION ne_dim_0_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Skalar)
    TYPE (t_dim) , INTENT(IN) :: this1    ! 
    !! Objekt 2 (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this2(:) ! 
    !! R&uuml;ckgabewert: Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this2)) ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_dim_0_1
  !
  !! pr&uuml;fe zwei Datenobjekte auf Ungleichheit ( Vektor / Vektor ) <BR>
  !! zwei Objekte werden als ungleich betrachtet insofern sie sich in
  !! mindestens einer Komponente unterscheiden <BR>
  !! Function erzeugt <EM>keine</EM> Fehlermeldungen
  FUNCTION ne_dim_1_1 ( this1, this2 ) &
         RESULT( ok )
    !! Objekt 1 (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this1(:) ! 
    !! Objekt 2 (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this2(:) ! 
    !! R&uuml;ckgabewert: Testergebnis (Vektor)
    LOGICAL :: ok(MIN(SIZE(this1),SIZE(this2))) ! 
    !
    ok = .NOT. ( this1 == this2 )
    !
  END FUNCTION ne_dim_1_1
  !
  ! ----------------------------------------------------------------------
  ! PUBLIC-GEN-ERROR-Methoden [ ]
  ! ----------------------------------------------------------------------
  !
  !! Erzeuge ggf. eine Fehlermeldung, falls eine Dimension mit
  !! vorgegebenem Namen und vorgegebener Gr&ouml;&szlig;e nicht
  !! in einer Liste von Dimensionen vorhanden ist <BR>
  !! Anmerkung: die Id der Dimension spielt hierbei keine Rolle <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE gen_error_if_dim_not_equal_1_nl ( this, name, nlen )
    !! Objektliste (t_dim) in der gesucht werden soll
    TYPE (t_dim)      , INTENT(IN) :: this(:) ! 
    !! gesuchter Name der Dimension
    CHARACTER (LEN=*) , INTENT(IN) :: name    ! 
    !! erforderliche L&auml;nge der gesuchten Dimension
    INTEGER           , INTENT(IN) :: nlen    ! 
    !! Name der Subroutine
    CHARACTER (LEN=31) , PARAMETER :: c_upname='gen_error_if_dim_not_equal_1_nl' ! 
    !! Hilfsvariablen
    LOGICAL            :: l_ok(2) ! 
    CHARACTER (LEN=10) :: ctxt    ! 
    !
    l_ok(:) = .false. 
    l_ok(1) = ( get_dim_idx(this(:),name) > 0 )
    IF ( l_ok(1) ) l_ok(2) = ( this(get_dim_idx(this(:),name))%len == nlen )
    IF ( .NOT. ALL( l_ok(:) ) ) THEN
       CALL setup_error_act ( all_errors(:), -6040, c_upname, c_modname )
       CALL setup_error_act ( '<ReqDimName>', TRIM(name) )
       WRITE(ctxt,'(I10)') nlen
       CALL setup_error_act ( '<ReqDimLen>', ctxt )
       ctxt = REPEAT( ' ', LEN(ctxt) )
       WRITE(ctxt(1:1),'(L1)') l_ok(1)
       CALL setup_error_act ( '<OkName>', ctxt )
       WRITE(ctxt(1:1),'(L1)') l_ok(2)
       CALL setup_error_act ( '<OkLen>', ctxt )
    END IF
    !
  END SUBROUTINE gen_error_if_dim_not_equal_1_nl
  !
  !! Erzeuge ggf. eine Fehlermeldung, falls eine Dimension 
  !! hinsichtlich Name und L&auml;nge nicht mit einer vorgegebenen
  !! Dimension (t_dim) &uuml;bereinstimmt <BR>
  !! Anmerkung: die Id der Dimension spielt hierbei keine Rolle <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE gen_error_if_dim_not_equal_1_0 ( this1, this2 )
    !! Objektliste in der gesucht werden soll
    TYPE (t_dim)      , INTENT(IN) :: this1(:) ! 
    !! Dimension, die hinsichtlich Name und L&auml;nge mit einem Eintrag
    !! in der Objektliste &uuml;bereinstimmen soll
    TYPE (t_dim)      , INTENT(IN) :: this2    ! 
    !
    CALL gen_error_if_dim_not_equal_1_nl ( this1(:), this2%name, this2%len )
    !
  END SUBROUTINE gen_error_if_dim_not_equal_1_0
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
  FUNCTION ok_initialised ( upname ) &
       RESULT( ok )
    !! Name der Subroutine die "ok_initialised" ruft
    CHARACTER (LEN=*) , INTENT(IN) :: upname ! 
    !! R&uuml;ckgabewert: Testergebnis
    LOGICAL :: ok ! 
    !! Fehlernummer
    INTEGER            :: ierr    ! 
    !! Fehlertext
    CHARACTER (LEN=80) :: cerr(3) ! 
    !
    ok = initialised
    !
    IF ( .NOT. ok ) THEN
       WRITE(*,*) ' *** Warnung *** Modul "b_dim" nicht initialisiert'
       ierr    = 1
       cerr(:) = REPEAT( ' ', LEN( cerr ) )
       cerr(1) = 'Fehlerkategorie: ALLGEMEIN'
       cerr(2) = 'Modul ist nicht initialisiert'
       cerr(3) = '--> INIT_dim ausfuehren'
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
    CHARACTER (LEN=*) , INTENT(IN) :: upname ! 
    !! R&uuml;ckgabewert: Testergebnis
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
  SUBROUTINE init_dim_all_errors ( )
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
               '--> INIT_dim ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ALLGEMEIN\n'//&
               'Modul ist schon initialisiert\n'//&
               '--> CLEAR_dim ausfuehren' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_dim"\n'//&
               'Typ-Komponente = "id"\n'//&
               '--> Code in Modul "b_dim" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_dim"\n'//&
               'Typ-Komponente = "name"\n'//&
               '--> Code in Modul "b_dim" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 5030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: KILL-Methoden\n'//&
               'De-Allocate-Fehler fuer Komponente von "t_dim"\n'//&
               'Typ-Komponente = "len"\n'//&
               '--> Code in Modul "b_dim" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_dim"\n'//&
               'Typ-Komponente = "id"\n'//&
               'aktuell        = <AktId>\n'//&
               'erforderlich   = id > 0\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6011 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_dim"\n'//&
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
               'Fehler in Komponente von "t_dim"\n'//&
               'Typ-Komponente = "name"\n'//&
               'aktuell        = "<AktName>"\n'//&
               'erforderlich   = siehe Werte des Feldes "c_dim_name(:)"\n'//&
               '--> Daten pruefen, ggf. PRINT-/PRINT-STATIC-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6021 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_dim"\n'//&
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
               'Fehler in Komponente von "t_dim"\n'//&
               'Typ-Komponente = "len"\n'//&
               'aktuell        = <AktLen>\n'//&
               'erforderlich   = len > 0 ODER len = -1 (unlimited)\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 6031 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: OK-Methoden\n'//&
               'Fehler in Komponente von "t_dim"\n'//&
               'Typ-Komponente    = "len"\n'//&
               'eine Dimension "unlimited" kommt mehrfach in einem Vektor vor\n'//&
               'aktuelle Position = <AktPos>\n'//&
               '--> Daten pruefen, ggf. PRINT-Methode verwenden' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Kopfzeilen\n'//&
               '--> Code in Modul "b_dim" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken der Fusszeilen\n'//&
               '--> Code in Modul "b_dim" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Drucken des Index des Datenobjektes (1D-Array)\n'//&
               '--> Code in Modul "b_dim" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_dim"\n'//&
               'Typ-Komponente = "id"\n'//&
               '--> Code in Modul "b_dim" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_dim"\n'//&
               'Typ-Komponente = "name"\n'//&
               '--> Code in Modul "b_dim" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken von Objekt "t_dim"\n'//&
               'Typ-Komponente = "len"\n'//&
               '--> Code in Modul "b_dim" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: PRINT-Methoden\n'//&
               'Fehler beim Drucken statischer Daten aus "b_dim"\n'//&
               '--> Code in Modul "b_dim" / Daten pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_dim"\n'//&
               'Typ-Komponente = "id"\n'//&
               '--> Code in Modul "b_dim" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_dim"\n'//&
               'Typ-Komponente = "name"\n'//&
               '--> Code in Modul "b_dim" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: NEW-Methoden\n'//&
               'Allocate-Fehler fuer Komponente von "t_dim"\n'//&
               'Typ-Komponente = "len"\n'//&
               '--> Code in Modul "b_dim" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ADD-Methoden\n'//&
               'Fehler beim Allokieren des Hilfsfeldes "this3(:)"\n'//&
               '--> Code in Modul "b_dim" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8501 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ADD-Methoden\n'//&
               'Fehler beim De-Allokieren des Pointerfeldes "this1(:)"\n'//&
               '--> Code in Modul "b_dim" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8502 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ADD-Methoden\n'//&
               'Fehler beim Allokieren des Pointerfeldes "this1(:)"\n'//&
               '--> Code in Modul "b_dim" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 8503 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: ADD-Methoden\n'//&
               'Fehler beim De-Allokieren des Hilfsfeldes "this3(:)"\n'//&
               '--> Code in Modul "b_dim" pruefen' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -6040 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'Fehlerkategorie: modulspezifische GEN-ERROR-Methoden\n'//&
               'gewuenschte Dimension (Name+Laenge) nicht in Objektliste enthalten"\n'//&
               'gesuchter Name       = <ReqDimName>"\n'//&
               'erforderliche Laenge = <ReqDimLen>"\n'//&
               'ok-Status (Name)     = <OkName>"\n'//&
               'ok-Status (Laenge)   = <OkLen>"\n'//&
               '--> Daten (Name,Laenge) in rufender Programmeinheit pruefen' )
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
  END SUBROUTINE init_dim_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-CLEAR-Methoden <<< [ERR_NO =  2000 bis  2999]
  ! ----------------------------------------------------------------------
  !
  !! De-Allokieren/De-Initialisieren aller Fehlermeldungen des Moduls <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE clear_dim_all_errors &
       ( )
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER  :: c_upname='clear_dim_all_errors' !
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_dim_all_errors
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-SETUP-Methoden <<< [ERR_NO =  3000 bis  3999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-NEW-Methoden <<< [ERR_NO =  4000 bis  4999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-KILL-Methoden <<< [ERR_NO =  5000 bis  5999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OK-Methoden <<< [ERR_NO =  6000 bis  6999]
  ! ----------------------------------------------------------------------
  !
  !! Pr&uuml;fe, ob die Komponente "id" eines Datenobjektes o.k. ist <BR>
  !! es wird gepr&uuml;ft, ob "id" gr&ouml;&szlig;er als 0 ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_dim_id ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_dim) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert: Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=09) , PARAMETER :: c_upname='ok_dim_id' ! 
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !
    ok = ( this%id > 0 )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6010, c_upname, c_modname )
       WRITE(ctxt,'(I10)') this%id
       CALL setup_error_act ( '<AktId>', ctxt )
    END IF
    !
  END FUNCTION ok_dim_id
  !
  !! Pr&uuml;fe, ob die Komponente "name" eines Datenobjektes o.k. ist <BR>
  !! es wird gepr&uuml;ft, ob "name" in dem Feld "c_dim_name" vorkommt <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_dim_name ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_dim) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert: Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=11) , PARAMETER :: c_upname='ok_dim_name' ! 
    !
    ok = ( COUNT( get_lowercase_char(c_dim_name(:)) == get_lowercase_char(this%name) ) == 1 )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6020, c_upname, c_modname )
       CALL setup_error_act ( '<AktName>', TRIM( this%name ) )
    END IF
    !
  END FUNCTION ok_dim_name
  !
  !! Pr&uuml;fe, ob die Komponente "len" eines Datenobjektes o.k. ist <BR>
  !! es wird gepr&uuml;ft, ob "len" = -1 (unlimited Dimension) oder 
  !! gr&ouml;&szlig;er als 0 ist <BR>
  !! Function erzeugt Fehlermeldungen
  FUNCTION ok_dim_len ( this ) &
       RESULT( ok )
    !! Datenobjekt
    TYPE (t_dim) , INTENT(IN) :: this ! 
    !! R&uuml;ckgabewert: Testergebnis
    LOGICAL :: ok ! 
    !! Name der Funktion
    CHARACTER (LEN=10) , PARAMETER :: c_upname='ok_dim_len' ! 
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !
    ok = ( this%len == -1 .OR. this%len > 0 )
    IF ( .NOT. ok ) THEN
       CALL setup_error_act ( all_errors(:), 6030, c_upname, c_modname )
       WRITE(ctxt,'(I10)') this%len
       CALL setup_error_act ( '<AktLen>', ctxt )
    END IF
    !
  END FUNCTION ok_dim_len
  !
  !! Pr&uuml;fe, ob alle Id's verschieden sind
  FUNCTION ok_dim_different_id ( this ) &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this(:) ! 
    !! R&uuml;ckgabewert: Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Funktion
    CHARACTER (LEN=19) , PARAMETER :: c_upname='ok_dim_different_id' ! 
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
  END FUNCTION ok_dim_different_id
  !
  !! Pr&uuml;fe, ob alle Namen verschieden sind
  FUNCTION ok_dim_different_name ( this ) &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this(:) ! 
    !! R&uuml;ckgabewert: Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Funktion
    CHARACTER (LEN=21) , PARAMETER :: c_upname='ok_dim_different_name' ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !
    DO i=1,SIZE(ok)
       ok(i) = ( COUNT( get_lowercase_char(this(:)%name) == get_lowercase_char(this(i)%name) ) == 1 )
       IF ( .NOT. ok(i) ) THEN
          CALL setup_error_act ( all_errors(:), 6021, c_upname, c_modname )
          CALL setup_error_act ( '<AktName>', TRIM( this(i)%name ) )
       END IF
    END DO
    !
  END FUNCTION ok_dim_different_name
  !
  !! Pr&uuml;fe, ob nur eine Dimension "unlimited" ist
  FUNCTION ok_dim_unique_unlimited ( this ) &
       RESULT( ok )
    !! Datenobjekt (Vektor)
    TYPE (t_dim) , INTENT(IN) :: this(:) ! 
    !! R&uuml;ckgabewert: Testergebnis (Vektor)
    LOGICAL :: ok(SIZE(this)) ! 
    !! Name der Funktion
    CHARACTER (LEN=23) , PARAMETER :: c_upname='ok_dim_unique_unlimited' ! 
    !! Z&auml;hlervariable
    INTEGER :: i ! 
    !! Hilfsfeld
    CHARACTER (LEN=10) :: ctxt ! 
    !
    ok(:) = .true.
    DO i=1,SIZE(ok)
       IF ( this(i)%len == -1 ) THEN
          ok(i) = ( COUNT( this(:)%len == this(i)%len ) == 1 )
          IF ( .NOT. ok(i) ) THEN
             CALL setup_error_act ( all_errors(:), 6031, c_upname, c_modname )
             WRITE(ctxt,'(I10)') i 
             CALL setup_error_act ( '<AktPos>', ctxt )
          END IF
       END IF
    END DO
    !
  END FUNCTION ok_dim_unique_unlimited
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-PRINT-Methoden <<< [ERR_NO =  7000 bis  7999]
  ! ----------------------------------------------------------------------
  !
  !! Drucke den Inhalt der Komponente "id" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_dim_id ( this )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_dim) , INTENT(IN) :: this ! 
    ! lokale Parameter / Variablen
    !! Name der Funktion
    CHARACTER (LEN=12) , PARAMETER :: c_upname='print_dim_id' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) this%id
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7010, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente id  - - - - - - - - - - - - - - - - - ',/&
           '# id   = ',I5,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_dim_id
  !
  !! Drucke den Inhalt der Komponente "name" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_dim_name ( this )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_dim) , INTENT(IN) :: this ! 
    ! lokale Parameter / Variablen
    !! Name der Funktion
    CHARACTER (LEN=14) , PARAMETER :: c_upname='print_dim_name' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) TRIM( this%name )
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7020, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente name  - - - - - - - - - - - - - - - - ',/&
           '# name = ',A,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_dim_name
  !
  !! Drucke den Inhalt der Komponente "len" eines Datenobjektes <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE print_dim_len ( this )
    ! Formalparameter
    !! Datenobjekt
    TYPE (t_dim) , INTENT(IN) :: this ! 
    ! lokale Parameter / Variablen
    !! Name der Funktion
    CHARACTER (LEN=13) , PARAMETER :: c_upname='print_dim_len' ! 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    WRITE ( UNIT=prn_lun, FMT=8000, IOSTAT=stat ) this%len
    IF ( stat /= 0 ) CALL setup_error_act ( all_errors(:), 7030, c_upname, c_modname, stat )
    !
8000 FORMAT &
          ('# Inhalt der Komponente len - - - - - - - - - - - - - - - - - ',/&
           '# len  = ',I10,/&
           '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ')
    !
  END SUBROUTINE print_dim_len
  !
  ! ----------------------------------------------------------------------
  ! >>> PRIVATE-OPERATOR(==)-Methoden <<< [ERR_NO = 10000 bis 10999]
  ! ----------------------------------------------------------------------
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
  ! >>> PRIVATE-OPERATOR(>=)-Methoden <<< [ERR_NO = 18000 bis 18999]
  ! ----------------------------------------------------------------------
  !
  ! ----------------------------------------------------------------------
  ! >>> SONSTIGES <<<
  ! ----------------------------------------------------------------------
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
END MODULE b_dim
! TailOfBaseModule --------------------------------------------------------
