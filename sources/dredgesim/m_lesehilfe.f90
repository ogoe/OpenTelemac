!! <H2>M_LESEHILFE</h2>
!! @author Susanne Spohr
!! @version 1.3 vom 07/11/02, Quellcode: mod_m_lesehilfe.f90
!! <HR>
!! <one line to give the program's name and an idea of what it does> <BR>
!! <HR>
!! <H3>Copyright-Hinweis</H3>
!!                                                                   <BR>
!! Copyright (C) 2002 <A HREF="http://www.hamburg.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!!                                                                   <BR>
!! <HR>
!! <H3>Entwicklungsgeschichte des Modules</H3>
!! 01.01 : 2002-05-30 : Susanne Spohr : Re-Engineering des Moduls mod_lesehilfe.f90 der Library dictionary <BR>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Modulss</H3>
!! Verschiedene Lesehilfen
!! - <readblockname> Namen einer Block-Struktur lesen
!! - <readkarte> Lesen einer ASCII-Zeile
!! - <read_logical> Lesen einer logischen Groesse
!! - <Time_Probelesen> Interpretierbarkeit einer Zeitangabe
!!   ueberpruefen
!! - <read_keyzeile> Lesen einer keyzeile und typische
!!   Checks
!! ...                                                              <BR>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>&Ouml;ffentliche Methoden</H3>
!!                                                                  <BR>
!! <H4>Basis-Methoden</H4>
!!                                                                  <BR>
!! <UL>
!!    <LI> <EM>...</EM> 
!!    <OL>
!!       <LI> ...
!!       <LI> ...
!!    </OL>
!!    <LI> <EM>...</EM>
!!    <OL>
!!       <LI> ...
!!       <LI> ...
!!    </OL>
!! </UL>
!! <HR>
!!                                                                  <BR>
!! <H3>Verwenden des Moduls</H3>
!!                                                                  <BR>
!! Die Leistungen dieses Moduls werden von "p_dictionary_ui" aus
!! in Anspruch genommen. Ein Verwenden der Methoden dieses Moduls
!! von anderen Paketen aus ist nicht zul&auml;ssig. F&uuml;r die
!! korrekte Verwendung der in diesem Modul befindlichen Methoden 
!! ist eine korrekte Initialisierung von "p_dictionary_ui"
!! erforderlich.
!!
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Fehlersituationen des Moduls</H3>
!!                                                                    <BR>
!! siehe hierzu die Fehlermeldungen in User Interface "p_dictionary_ui" 
!!
MODULE m_lesehilfe
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] BASIS-Modul mit Fehler-Typ und -Routinen
  !
  USE b_error, ONLY :   &
       ! Routinen
       no_error,        &
       any_error,       &
       setup_error_act
  !
  ! [A.2] BASIS-Modul fuer das File-Handling
  !
  USE b_file, ONLY :  &
       ! Typdefinition
       t_file,        &
       ! Routinen
       get_file_unit, &
       get_file_name
  !
  ! ----------------------------------------------------------------------
  ! [B] Module des Paketes "dictionary"
  ! ----------------------------------------------------------------------
  !
  ! [B.1] Daten-Modul des Paketes "dictionary"
  !
  USE m_dictionary_data, ONLY : &
       ! globale Paket-Variablen
       prn_op,      & ! Indikator Druckerausgabe
       trc_op,      & ! Indikator Drucken in Trace-Datei
       prn_lun,     & ! log. Fortran-Unit fuer Druckerausgabe
       trc_lun,     & ! log. Fortran-Unit fuer Traceausgabe
       all_errors     ! Fehlermeldunegn
  !
  ! Hinweis: Module anderer Packages duerfen nicht verwendet werden.
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
  ! [C.2] Konstantwerte (Parameter) [moeglichst nicht verwenden]
  !
  ! [C.3] Variablen [moeglichst nicht verwenden]
  !
  ! [C.4] Schnittstellen
  !
  ! [C.4.1] erforderliche oeffentliche Schnittstellen
  !
  !! ...
  INTERFACE readblockname
     MODULE PROCEDURE readblockname_d ! 
  END INTERFACE
  !
  !! ...
  INTERFACE readkarte
     MODULE PROCEDURE readkarte_d ! 
  END INTERFACE
  !
  !! ...
  INTERFACE read_logical
     MODULE PROCEDURE read_logical_d ! 
  END INTERFACE
  !
  !! ...
  INTERFACE Time_Probelesen
     MODULE PROCEDURE Time_Probelesen_d ! 
  END INTERFACE
  !
  !! ...
  INTERFACE read_keyzeile
     MODULE PROCEDURE read_keyzeile_d ! 
  END INTERFACE
  !
  ! [C.5] Zuweisungen
  !
  ! ... ggf. ergaenzen
  !
  ! [C.6] Operatoren (optional, falls sinnvoll)
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  PUBLIC :: readblockname                    ! ...
  PUBLIC :: readkarte                        ! ...
  PUBLIC :: read_logical                     ! ...
  PUBLIC :: Time_Probelesen                  ! ...
  PUBLIC :: read_keyzeile                    ! ...
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
  CHARACTER (LEN=31), PARAMETER :: c_modname      = 'm_lesehilfe' ! 
  !!
  LOGICAL           , PARAMETER :: l_wri = .False.
  !
  ! [D.3] Variablen (statische Daten des Moduls)
  !
  ! [D.4] Schnittstellen
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
  ! ----------------------------------------------------------------------
  ! >>> modulspezifische PUBLIC-Methoden <<< [ERR_NO < 0]
  ! ----------------------------------------------------------------------
  !
  !! Interpretation des Blocknamens oder ENDFILE. <BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE readblockname_d &
       ( karte, doit, blockname )
    !
    ! Formalparameter
    !! ASCI-String mit Eingabedaten
    CHARACTER (LEN=*), INTENT(IN)          :: karte
    !! Steuerparameter
    !! .true.  = es wurde ein gueltiger Blockname ge-
    !!           funden. Lesen mit den Daten des genannten
    !!           Blocks fortfahren <BR>
    !! .false. = Ende der Datei erreicht. Es sind keine
    !!           weiteren Bloecke mehr vorhanden.
    LOGICAL          , INTENT(OUT)         :: doit
    !! String mit dem Namen des Blocks
    CHARACTER (LEN=*), INTENT(OUT)         :: blockname
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='readblockname_d'
    INTEGER                                :: jl
    !
       !
       jl = INDEX(karte, ' ')
       !
       SELECT CASE (karte(1:jl))
       !
       CASE ('ENDFILE')
          !
          ! ENDFILE-Marke
          !
          doit = .false.
          !
       CASE ('BEGINDATA')
          !
          ! Datenblock
          !
          CALL Get_Blockname &
               ( karte, blockname )
          !
          IF ( any_error( ) ) RETURN
          !
          doit = .true.
          !
       CASE DEFAULT
          !       
          ! Fehler -1810 : fehlerhaftes Steuerwort in Eingabedatei !
          !              erlaubt = [BEGINDATA,ENDFILE]
          !
          CALL setup_error_act ( all_errors(:), -1810, c_upname, c_modname )
          CALL setup_error_act ( '<aktuell>', TRIM(karte) )
          !
          doit = .false.
          !
          RETURN
          !
       END SELECT
       !
    !
  END SUBROUTINE readblockname_d
  !
  !! Lesen einer Eingabezeile mit Hilfe des
  !! Programmes (UNI_KARTLE, wird hier durch das lokale 
  !! Programm Karte_lesen ersetzt) <BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE readkarte_d &
       ( fil2read, ftext, karte )
    !
    ! Formalparameter
    !!  Datei aus der gelesen werden soll
    TYPE (t_file)    ,     INTENT(IN   )         :: fil2read
    !!  Fehlertext fuer da ! 
    CHARACTER (LEN=*),     INTENT(IN   )         :: ftext
    !!  String mit Eingabedaten
    CHARACTER (LEN=*),     INTENT(INOUT)         :: karte
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='readkarte_d'
    !! Name der zu lesenden Datei
 !   CHARACTER (LEN=*)             :: fil2read_name
    !
    LOGICAL                              :: lende
    CHARACTER (LEN=LEN(karte)+10)        :: ck_karte
    CHARACTER (LEN=4)                    :: c_zeillen
    !
       !
       ! Initialisierungen
       !
       karte = REPEAT(' ',LEN(karte))
       !
       ! Lesen einer Eingabezeile  
       !
       CALL Karte_lesen &
          ( fil2read, ftext, ck_karte, lende)
       !
!       fil2read_name = get_file_name(fil2read)
       !
       IF (lende) THEN
          !       
          ! Fehler -1820 :  Vorzeitiges Ende der Datei erreicht !
          !
          CALL setup_error_act ( all_errors(:), -1820, c_upname, c_modname )
          CALL setup_error_act ( '<datei>', TRIM(get_file_name(fil2read)) )
          !
          RETURN
          !
       END IF
       !
       IF ( LEN_TRIM(ck_karte) .GT. LEN(karte) ) THEN
          !       
          ! Fehler -1830 : Datei enthaelt eine unzulaessig lange Zeile !
          !
          c_zeillen  = REPEAT(' ',LEN(c_zeillen))
          WRITE(c_zeillen,'(I4)') LEN(karte)
          c_zeillen  = ADJUSTL(c_zeillen)
          !
          CALL setup_error_act ( all_errors(:), -1830, c_upname, c_modname )
          CALL setup_error_act ( '<datei>', TRIM(get_file_name(fil2read)) )
          CALL setup_error_act ( '<zeil_len>', TRIM(c_zeillen) )
          CALL setup_error_act ( '<zeil_anfang>', TRIM(ck_karte) )
          !
          RETURN
          !
       ELSE
          !
          karte = ck_karte(1:LEN(karte))
          !
       END IF
       !
    !
  END SUBROUTINE readkarte_d
  !
  !! Liest eine Zeitangabe - Datumsangabe oder Inkrement -
  !! zur Probe<BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE Time_Probelesen_d &
       ( c_kenn, c_feld )
    !
    ! USE-Statements :
    !
    ! BASIS-Modul fuer den Umgang mit Zeitinkrementen
    !
    USE b_time, ONLY : &
         ! Typdefinitionen
         t_time,         &
         ! Routinen
         string_to_time
    !
    ! BASIS-Modul fuer den Umgang mit Zeitangaben
    !
    USE b_datetime, ONLY : &
         ! Typdefinitionen
         t_datetime,         &
         ! Routinen
         string_to_datetime
    !
    ! Package-Module
    !    
    USE m_stdat_types, ONLY : &
         ! Parameter
         key_len
    !
    ! Formalparameter
    !! Kennung, ob Zeitpunkt oder Inkrement [DATE,INCR]
    CHARACTER (LEN=*)                   , INTENT(IN)     :: c_kenn
    !! Feld mit Zeitpunkts-Angaben
    CHARACTER (LEN=key_len)   , DIMENSION(:) , POINTER   :: c_feld
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='Time_Probelesen_d'
    !
    TYPE (t_datetime)    :: dt_string
    TYPE (t_time)        :: ti_string
    !
    INTEGER              :: i
    !
       !
       SELECT CASE (c_kenn)
       CASE('DATE')
          !
          DO i = 1, SIZE(c_feld)
             !
             dt_string = string_to_datetime ( c_feld(i) )
             !
          END DO
          !
       CASE('INCR')
          !
          DO i = 1, SIZE(c_feld)
             !
             ti_string = string_to_time ( c_feld(i) )
             !
          END DO
          !
       CASE DEFAULT
          !       
          ! Fehler -1840 : Parameter <c_kenn> hat unzulaessigen Wert !
          !
          CALL setup_error_act ( all_errors(:), -1840, c_upname, c_modname )
          !
          RETURN
          !
       END SELECT
       !
    !
  END SUBROUTINE Time_Probelesen_d
  !
  !! Liest eine Schluesselwort-Zeile deren Wert eine
  !! logische Groesse ist.<BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE read_logical_d &
       ( key, wert, l_back )
    !
    ! Formalparameter
    !! Schluesselwort einer Key-Zeile
    CHARACTER (LEN=*)  , INTENT(IN)      :: key
    !! Wert einer Key-Zeile (d.h. hinter Gleichheitszeichen)
    CHARACTER (LEN=*)  , INTENT(IN)      :: wert
    !! Logischer Rueckgabewert
    LOGICAL            , INTENT(OUT)     :: l_back
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='read_logical_d'
    !
       !
       SELECT CASE (TRIM(wert))
       CASE('1','T','True','.True.','true','.true.','TRUE','.TRUE.')

          l_back = .True.

       CASE('0','F','False','.False.','false','.false.','FALSE','.FALSE.')

          l_back = .False.

       CASE DEFAULT
          !       
          ! Fehler -1850 : Die Angabe zu einer logischen Groesse ist nicht interpretierbar !
          !
          CALL setup_error_act ( all_errors(:), -1850, c_upname, c_modname )
          CALL setup_error_act ( '<key>', TRIM(key) )
          CALL setup_error_act ( '<wert>', TRIM(wert) )
          !
          RETURN
          !
       END SELECT
       !
    !
  END SUBROUTINE read_logical_d
  !
  !! Liest eine SchluesselwortZeile.<BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE read_keyzeile_d &
       ( file, blockname, block, karte, key, wert, doit, egb_nr )
    !
    ! USE-Statements
    !
    USE m_stdat_types, ONLY : &
         ! Typdefinition
         t_block
    !
    USE m_stringliste, ONLY : &
         !Routine
         make_liste
    !
    ! Formalparameter
    !! Dateitypkennzeichnung z.B. 'Eingabe','Dictionary'
    CHARACTER (LEN=*)               , INTENT(IN   )   :: file
    !! Name der aktuellen Blockdata-Struktur
    CHARACTER (LEN=*)               , INTENT(IN   )   :: blockname
    !! Variable mit SchluesselwortBeschreibungen
    TYPE(t_block)                   , INTENT(INOUT)   :: block
    !! ASCCI-String zum Lesen einer Zeile
    CHARACTER (LEN=*)               , INTENT(INOUT)   :: karte
    !! keyword der Zeile
    CHARACTER (LEN=*)               , INTENT(  OUT)   :: key
    !! String mit Datensatz zur physik.Groesse
    CHARACTER (LEN=*)               , INTENT(  OUT)   :: wert
    !! Steuerparameter<BR>
    !! .true.  = Lesen im Block fortsetzen<BR>
    !! .false. = Block verlassen
    LOGICAL                         , INTENT(  OUT)   :: doit
    !! enthaelt Nummer des aktuellen Eingabeblockes; dieser 
    !! Parameter sollte nicht vorhanden sein, wenn Keyzeile
    !! einer Dictionary-Datei gelesen wird
    INTEGER           , OPTIONAL    , INTENT(IN   )   :: egb_nr
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='read_keyzeile_d'
    !
    INTEGER                       :: i
    LOGICAL                       :: l_set
    !
    !! String mit erlaubten Schluesselwoertern
    CHARACTER (LEN=60)            :: erlaubt
    !! True, <erlaubt>-String ist zu lang
    LOGICAL                       :: l_tolong
    !
    !! ggf. String mit Nummer des Eingabeblockes
    CHARACTER (LEN=6)             :: c_egb
    !! ggf. String mit Zeilnummer
    CHARACTER (LEN=6)             :: c_zeil
    !! Blockbezeichnung fuer Fehlermeldung
    CHARACTER (LEN=120)           :: c_block
    !
       !
       doit = .true.
       !
       ! Ggf.: Character mit Nummer des Eingabeblockes generieren
       !
       c_block = REPEAT(' ',LEN(c_block))
       !
       IF ( PRESENT(egb_nr) ) THEN
          !
          c_egb   = REPEAT(' ',LEN(c_egb))
          !
          IF ( .NOT. block%L_Single ) THEN
             !
             ! ... wenn Eingabeblock mehrfach vorkommen kann
             !
             WRITE(c_egb,'(I4)') egb_nr
             c_egb  = ADJUSTL(c_egb)
             c_egb  = ' '//TRIM(c_egb)//'.'
             !
          END IF
          !
          c_block = TRIM(c_egb)//' '//TRIM(blockname)//'-Block'
          !
       ELSE
          !
          c_block = ' Block '//TRIM(blockname)
          !
       END IF
       !
       ! ... Ende des BLOCK-Blockes
       !
       karte = ADJUSTL(karte)
       !
       IF ( karte(1:7) .EQ. 'ENDDATA' ) THEN       
          !
          doit = .false.
          !
          ! ... Leerzeile in Eingabe-Block
          !
       ELSE IF ( LEN_TRIM(karte) .EQ. 0 ) THEN
          !       
          ! Fehler -1860 : Ein Block enthaelt eine Leerzeile !
          !
          CALL setup_error_act ( all_errors(:), -1860, c_upname, c_modname )
          CALL setup_error_act ( '<file_typ>', TRIM(file) )
          CALL setup_error_act ( '<block>', TRIM(c_block) )
          !
          RETURN
          !
       ELSE 
          !
          ! ... oder Schluesselwort-Zeile
          !
          ! Ermitteln von Key und zugehoerigem Wert der EingabeZeile
          !
          CALL Zerlege_Keyzeile &
               ( TRIM(karte), key, wert )
          !
          IF ( any_error( ) ) RETURN
          !
          IF ( l_wri ) WRITE(*,*) '       --> Schluesselwort ',TRIM(key)
          !
          IF ( LEN(TRIM(key))  .EQ. 0 ) THEN  
             !       
             ! Fehler -1870 : Ein Block enthaelt eine Eingabezeile ohne Schluesselwort !
             !
             CALL setup_error_act ( all_errors(:), -1870, c_upname, c_modname )
             CALL setup_error_act ( '<filetyp>', TRIM(file) )
             CALL setup_error_act ( '<block>', TRIM(c_block) )
             !
             RETURN
             !
          END IF
          !
          ! Checken, ob gelesenes Keywort :
          !  - zu den gueltigen zaehlt
          !  - nicht trotz SingleStatus mehrfach im Block auftaucht
          !
          l_set = .false.
          !
          vgl : DO i = 1, SIZE(block%Key)
             !
             IF ( l_set ) EXIT vgl
             !
             IF ( block%Key(i)%Name .EQ. TRIM(key) ) THEN
                !
                IF ( block%Key(i)%L_Single .AND. &
                     block%Key(i)%ZeilAnz(1) .GT. 0     ) THEN
                   !       
                   ! Fehler -1880 : In einem Block wurde das Schluesselwort mehrfach
                   !              spezifiziert !
                   !
                   CALL setup_error_act ( all_errors(:), -1880, c_upname, c_modname )
                   CALL setup_error_act ( '<file_typ>', TRIM(file) )
                   CALL setup_error_act ( '<block>', TRIM(c_block) )
                   CALL setup_error_act ( '<key>', TRIM(key) )
                   !
                   RETURN
                   !
                END IF
                !
                l_set = .true.
                block%Key(i)%ZeilAnz(1) = block%Key(i)%ZeilAnz(1) +  1
                !
                c_zeil = REPEAT(' ',LEN(c_zeil))
                IF ( .NOT. block%Key(i)%L_Single ) THEN
                   !
                   ! ... wenn KeyZeile mehrfach vorkommen kann
                   !
                   WRITE(c_zeil,'(I4)') block%Key(i)%ZeilAnz(1)
                   c_zeil  = ADJUSTL(c_zeil)
                   c_zeil  = ' '//TRIM(c_zeil)//'.'
                   !
                END IF
                !
             END IF
             !
          END DO vgl
          !
          ! Check : Ist key ein gueltiges Schluesselwort ?
          !
          IF ( .NOT. l_set ) THEN
             !
             ! Fehlerfall : key ist kein gueltiges Schluesselwort !
             !
             ! ... erlaubt-String basteln !!   z.B. [L_Opt,ReqIf,NotIf]
             !
             erlaubt = REPEAT(' ',LEN(erlaubt))
             l_tolong = .False.
             !
             DO i = 1, SIZE(block%Key)
                !
                IF ( l_tolong ) EXIT
                !
                CALL make_liste &
                  ( TRIM(block%Key(i)%Name),',', erlaubt, l_tolong )
                !
                IF ( any_error( ) ) RETURN
                !
             END DO
             !
             IF ( l_tolong  .OR.  &
                  LEN_TRIM(erlaubt) .GT. LEN(erlaubt) - 2 ) THEN
                !
                erlaubt = REPEAT(' ', LEN(erlaubt))
                erlaubt(1:18) = 'siehe Muster-Datei'
             ELSE
                erlaubt = '['//TRIM(erlaubt)//']'
             END IF
             !       
             ! Fehler -1890 : Ein Block enthaelt ein ungueltiges Schluesselwort !
             !
             CALL setup_error_act ( all_errors(:), -1890, c_upname, c_modname )
             CALL setup_error_act ( '<block>', TRIM(c_block) )
             CALL setup_error_act ( '<aktuell>', TRIM(key) )
             CALL setup_error_act ( '<erlaubt>', TRIM(erlaubt) )
             CALL setup_error_act ( '<file_typ>', TRIM(file) )
             !
             doit = .false.
             !
             RETURN
             !
          END IF
          !
          IF ( LEN(TRIM(wert)) .EQ. 0 ) THEN 
             !       
             ! Fehler -1900 : Keywort-Zeile ohne Eintragung hinter dem Gleichheitszeichen !
             !
             CALL setup_error_act ( all_errors(:), -1900, c_upname, c_modname )
             CALL setup_error_act ( '<file_typ>', TRIM(file) )
             CALL setup_error_act ( '<block>', TRIM(c_block) )
             CALL setup_error_act ( '<zeil_nr>', TRIM(c_zeil) )
             CALL setup_error_act ( '<key>', TRIM(key) )
             !
             RETURN
             !
          END IF
          !
       END IF
       !
    !
  END SUBROUTINE read_keyzeile_d
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
  ! -----------------------------------------------------------------------------
  ! >>> modulspezifische PRIVATE-Methoden <<< [ERR_NO < 0]
  ! -----------------------------------------------------------------------------
  !
  !! (lokales UP analog zu f77-Prog kartle2.f) <BR>
  !!
  !! Es wird eine Zeile (?? Zeichen) von einer ASCII-Datei
  !! eingelesen. Stehen in der ersten Spalte die Zeichen C oder c, so
  !! wird diese Karte ueberlesen. Steht in der ersten Spalte ein *, so
  !! wird diese Karte ebenfalls ueberlesen, allerdings zusaetzlich auf
  !! die Standardausgabe herausgeschrieben. Steht dagagen in den ersten
  !! vier Spalten das Wort STOP, so wird die Eingabe an dieser Stelle
  !! beendet.<BR>
  !! Version 2: Wird ein vorzeitiges Dateiende entdeckt, dann wird die
  !! Variable LENDE=TRUE gesetzt und zum aufrufenden Programm zurueckgesprun-
  !! gen.<BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE Karte_lesen &
       ( fil2read, ftext, karte, lende)
    !
    ! Formalparameter
    !! Eingabedatei
    TYPE (t_file)     , INTENT(IN   )  :: fil2read
    !! Fehlertext 
    CHARACTER (LEN=*) , INTENT(IN)     :: ftext
    !! eingelesen Zeile der Eingabedatei
    CHARACTER (LEN=*) , INTENT(  OUT)  :: karte
    !! Dateiendekennung:<BR>
    !! .TRUE.  = vorzeitiges Dateiende entdeckt<BR>
    !! .FALSE. = alles OK
    LOGICAL           , INTENT(  OUT)  :: lende
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='Karte_lesen'
    !! Kanalnummerstring fuer Fehlermeldung
    CHARACTER (LEN=6)                  :: c_lkl
    !! Kommentarzeile Ja/Nein
    LOGICAL                            :: l_com
    !! Kanalnummer zum Lesen des Files
    INTEGER                            :: fil2read_unit
    !
       !
       karte = REPEAT(' ', LEN(karte))
       !
       lende = .False.
       l_com = .True.
       !
       fil2read_unit = get_file_unit(fil2read)
       !
       read : DO
          !
          IF ( .NOT. l_com ) EXIT read
          !
          READ (fil2read_unit,'(A)',END=8000,ERR=9000) karte
          !
          IF (karte(1:1) .EQ. '*') THEN
             !
             ! ... Kommentarzeile mit Echo auf Bildschirm und in Printerdatei
             !
             WRITE (prn_lun,'(A)') karte
             WRITE (*,'(A)') karte
             !
          ELSE IF (karte(1:2) .NE. 'C ' .AND.  karte(1:2) .NE. 'c '  .AND. &
               karte(1:1) .NE. '&'  .AND. &
               karte(1:1) .NE. '#'  .AND.  karte(1:1) .NE. '@') THEN
             !
             ! ... keine Kommentarzeile
             !
             l_com = .False.
             !
          END IF
          !
       END DO read
       !
       IF (karte(1:4) .EQ. 'STOP') THEN
          ! 
          ! ... STOP - Eingabetest
          !     ( Meldung auf Bildschirm und in Printerdatei )
          !
          WRITE (prn_lun,*) ' '
          WRITE (prn_lun,*) ' *** Karte_Lesen ***'
          WRITE (prn_lun,*) ' '
          WRITE (prn_lun,*) ' STOP - Eingabetest bei Lesen von Datei'
          WRITE (prn_lun,*) '        ',TRIM(get_file_name(fil2read))
          !
          WRITE (*,*) ' '
          WRITE (*,*) ' *** Karte_Lesen ***'
          WRITE (*,*) ' '
          WRITE (*,*) ' STOP - Eingabetest bei Lesen von Datei'
          WRITE (*,*) '        ',TRIM(get_file_name(fil2read))
          !
          STOP
          !
       END IF
       !
       RETURN
       !
       !
       ! Fehlerbehandlung --------------------------------------------------
       !
       ! Dateiende erreicht
       ! --> lende = .true.  setzen !
       ! Fehlermeldung uebernimmt die rufende Routine
       !
 8000  CONTINUE
       !
       lende = .True.
       !
       RETURN
       !
       ! Fehler -1910 : Datei-Lesefehler
       !
 9000  c_lkl = REPEAT(' ',LEN(c_lkl))
       WRITE (c_lkl, '(I4)') get_file_unit( fil2read )
       !
       CALL setup_error_act ( all_errors(:), -1910, c_upname, c_modname, card=karte )
       CALL setup_error_act ( '<lkl_nr>', TRIM(c_lkl) )
       CALL setup_error_act ( '<datei_name>', TRIM(get_file_name(fil2read)) )
       CALL setup_error_act ( '<xyz>', TRIM(ftext) )
       CALL setup_error_act ( '<letzte_zeile>', TRIM(karte) )
       !
       RETURN
       !
       !
    !
  END SUBROUTINE Karte_lesen
  !
  !! Analysiere den in einer mit BEGINDATA beginnenden
  !! Zeile stehenden Namen BLOCKNAME.<BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE Get_Blockname &
       ( karte, blockname )
    !
    ! Formalparameter
    !! Datenzeile mit BEGINDATA-Anweisung
    CHARACTER (LEN=*)                 , INTENT(IN   )  :: karte
    !! aktueller Blockname
    CHARACTER (LEN=*)                 , INTENT(  OUT)  :: blockname
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='Get_Blockname'
    !! Hilfsstrings fuer Fehlermeldung
    CHARACTER (LEN=6)             :: c_actlen, c_maxlen
    !
    CHARACTER (LEN=LEN(karte))    :: c_block
    !
       !
       ! Pruefe BEGINDATA-String
       !
       IF (karte(1:9) .NE. 'BEGINDATA') THEN
          !       
          ! Fehler -1920 : BEGINDATA nicht in karte vorhanden !
          !
          CALL setup_error_act ( all_errors(:), -1920, c_upname, c_modname )
          CALL setup_error_act ( '<karte1_9>', karte(1:9) )
          !
          RETURN
          !
       END IF
       !
       ! Ermittle die aktuelle Laenge des Blocknamens
       !
       c_block  = REPEAT(' ',LEN(c_block))
       c_block  = ADJUSTL(karte(10:))
       !
       IF (LEN_TRIM(c_block) .LT. 1) THEN
          !       
          ! Fehler -1930 : Datei enthaelt einen namenlosen DATA-Block !
          !
          CALL setup_error_act ( all_errors(:), -1930, c_upname, c_modname )
          !
          RETURN
          !
       END IF
       !
       ! Lesen des Blocknamens
       !
       blockname = REPEAT(' ',LEN(blockname))
       !
       IF (LEN_TRIM(c_block) .GT. LEN(blockname)) THEN
          !       
          ! Fehler -1940 : Stringlaenge von <blockname> zu kurz !
          !
          c_actlen = REPEAT(' ',LEN(c_actlen))
          c_maxlen = REPEAT(' ',LEN(c_maxlen))
          !
          WRITE(c_actlen,'(I4)') LEN_TRIM(c_block)
          WRITE(c_maxlen,'(I4)') LEN(blockname)
          !
          CALL setup_error_act ( all_errors(:), -1940, c_upname, c_modname )
          CALL setup_error_act ( '<akt_blockname>', TRIM(c_block) )
          CALL setup_error_act ( '<act_len>', TRIM(c_actlen) )
          CALL setup_error_act ( '<max_len>', TRIM(c_maxlen) )
          !
          RETURN
          !
       ELSE
          !
          blockname = c_block
          !
       END IF
       !
    !
  END SUBROUTINE Get_Blockname
  !
  !! Trennt Schluesselwortzeilen in ihre Bestandteile
  !! KEY und WERT auf !<BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE Zerlege_Keyzeile &
       ( zeile, key, wert )
    !
    ! Formalparameter
    !! Datenzeile der Form : key = wert
    CHARACTER (LEN=*)                 , INTENT(IN   )  :: zeile
    !! String mit akt. Schluesselwort
    CHARACTER (LEN=*)                 , INTENT(  OUT)  :: key
    !! String mit akt. Wert
    CHARACTER (LEN=*)                 , INTENT(  OUT)  :: wert
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='Zerlege_Keyzeile'
    !! Hilfsstrings fuer Fehlermeldung
    CHARACTER (LEN=6)             :: c_actlen, c_maxlen
    !
    INTEGER                              :: glz_pos
    CHARACTER (LEN=LEN(zeile))           :: c_key, c_wert
    !
       !
       ! Lokalisiere das Gleichheitszeichen "="
       !
       glz_pos = INDEX(zeile, '=')
       !
       IF (glz_pos .EQ. 0) THEN
          !       
          ! Fehler -1950 : Schluesselwortzeile enthaelt kein Gleichheitszeichen !
          !
          CALL setup_error_act ( all_errors(:), -1950, c_upname, c_modname )
          CALL setup_error_act ( '<zeile>', TRIM(zeile) )
          !
          RETURN
          !
       END IF
       !
       ! Schluesselwort(KEY) lesen
       !
       key    = REPEAT(' ', LEN(key))
       c_key  = REPEAT(' ', LEN(c_key ))
       !
       c_key  = zeile(1:glz_pos-1)
       c_key  = ADJUSTL(c_key)
       !
       IF (LEN_TRIM(c_key) .GT. LEN(key)) THEN
          !       
          ! Fehler -1960 : Schluesselwort der Eingabezeile ist zu lang !
          !
          c_actlen = REPEAT(' ',LEN(c_actlen))
          c_maxlen = REPEAT(' ',LEN(c_maxlen))
          !
          WRITE(c_actlen,'(I4)') LEN_TRIM(c_key)
          WRITE(c_maxlen,'(I4)') LEN(key)
          !
          CALL setup_error_act ( all_errors(:), -1960, c_upname, c_modname )
          CALL setup_error_act ( '<zeile>', TRIM(zeile) )
          CALL setup_error_act ( '<act_len>', TRIM(c_actlen) )
          CALL setup_error_act ( '<max_len>', TRIM(c_maxlen) )
          !
          RETURN
          !
       ELSE
          !
          key = c_key
          !
       END IF
       !
       ! WERT der Keyzeile lesen
       !
       wert   = REPEAT(' ', LEN(wert))
       c_wert = REPEAT(' ', LEN(c_wert))
       !
       c_wert = zeile(glz_pos+1:)
       !
       c_wert = ADJUSTL(c_wert)
       !
       IF (LEN_TRIM(c_wert) .GT. LEN(wert)) THEN
          !       
          ! Fehler -1970 : Wert der Schluesselwortzeile ist zu lang !
          !
          c_actlen = REPEAT(' ',LEN(c_actlen))
          c_maxlen = REPEAT(' ',LEN(c_maxlen))
          !
          WRITE(c_actlen,'(I4)') LEN_TRIM(c_wert)
          WRITE(c_maxlen,'(I4)') LEN(wert)
          !
          CALL setup_error_act ( all_errors(:), -1970, c_upname, c_modname )
          CALL setup_error_act ( '<zeile>', TRIM(zeile) )
          CALL setup_error_act ( '<act_len>', TRIM(c_actlen) )
          CALL setup_error_act ( '<max_len>', TRIM(c_maxlen) )
          !
          RETURN
          !
       ELSE
          !
          wert = c_wert(1:LEN_TRIM(c_wert))
          !
       END IF
       !
    !
  END SUBROUTINE Zerlege_Keyzeile
  !
END MODULE m_lesehilfe
!  ----------------------------------------------------------------------------
!  Ende des Source-Codes des Moduls
!  ----------------------------------------------------------------------------

