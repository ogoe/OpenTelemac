!! <H2>M_STDAT_TYPES</h2>
!! @author Susanne Spohr
!! @version 1.4 vom 07/07/05, Quellcode: mod_m_stdat_types.f90
!! <HR>
!! <one line to give the program's name and an idea of what it does> <BR>
!! <HR>
!! <H3>Copyright-Hinweis</H3>
!!                                                                   <BR>
!! Copyright (C) 2002 <A HREF="http://www.hamburg.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!!                                                                   <BR>
!! <HR>
!! <H3>Entwicklungsgeschichte des Modules</H3>
!! 01.01 : 2002-05-23 : Susanne Spohr : Re-Engineering des Moduls mod_stdat_types.f90 der Library basismodule <BR>
!! 01.02 : 2002-06-27 : Susanne Spohr : Package-Struktur ueberarbeitet (+ _data-Modul), init_.._all_errors nun nur in UI-Modul, ...
!! 01.03 : 2002-07-11 : Susanne Spohr : Verbesserung Fehlertexte
!! 01.04 : 2006-07-07 : G. Lang       : key_len von 80 auf 200 Zeichen heraufgesetzt
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Modulss</H3>
!! Datentypen zum Lesen einer Steuerdatei, die durch
!! eine Dictionary-Datei beschrieben wird.
!! Plus die zum Handling dieser teilweise komplexen
!! Datentypen benoetigten Subroutinen, z.B. :
!! - zur Initialisierung von Variablen
!! - zum Aufbau von verketteten Listen dieser Variablen
!! - Deallokieren der dynam. Komponentenfelder dieser
!!   Variablen
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
MODULE m_stdat_types
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] BASIS-Modul mit globalen Konstantwerten
  !
  USE b_constants, ONLY : &
       ! Parameter
       double
  !
  ! [A.2] BASIS-Modul mit Fehler-Typ und -Routinen
  !
  USE b_error, ONLY :   &
       ! Routinen
       no_error,        &
       any_error,       &
       setup_error_act
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
  ! [B.2] weitere Module die zum Package "dictionary" gehoeren
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
  ! [C.1] Konstantwerte (Parameter) [moeglichst nicht verwenden]
  !! line_len = Laenge von Zeichenketten fuer das Lesen einer Eingabezeile aus einer Datei
  INTEGER, PUBLIC, PARAMETER    :: line_len = 240 ! 
  !! key_len  = Zeichenketten-Laenge fuer Blocknamen & Schluesselwoerter
  INTEGER, PUBLIC, PARAMETER    :: key_len  = 200 ! 
  !
  ! [C.2] oeffentlich zugaengliche Typen
  !
  !! Typ zur Aufnahme eines Feldes, in Abhaengigkeit von seinem Datentyp <BR>
  !! Komponente "int"  : dynam. allokierbares Feld vom Typ : Integer     <BR>
  !! Komponente "real" : dynam. allokierbares Feld vom Typ : Real        <BR>
  !! Komponente "char" : dynam. allokierbares Feld vom Typ : Character   <BR>
  !! Komponente "log"  : dynam. allokierbares Feld vom Typ : Logical     <BR>
  !! Komponente "doub" : dynam. allokierbares Feld vom Typ : Real,hoehere Genauigkeit <BR>
  TYPE , PUBLIC :: t_feld
     INTEGER                  , DIMENSION(:), POINTER     :: int
     REAL                     , DIMENSION(:), POINTER     :: real
     CHARACTER (LEN=key_len)  , DIMENSION(:), POINTER     :: char
     LOGICAL                  , DIMENSION(:), POINTER     :: log
     REAL  (KIND=Double)      , DIMENSION(:), POINTER     :: doub
  END TYPE t_feld
  !
  !! Typ dient dem Abspeichern der fuer einen Parameter in einem Eingabeblock angegebenen Werte <BR>
  !! Komponente "Wert" : ... <BR>
  TYPE , PUBLIC :: t_wert
     TYPE (t_feld), DIMENSION(:) , POINTER       :: Wert
  END TYPE t_wert
  !
  !! Typ zur Aufnahme von Beschreibungen eines Dateielementes  [PARAMETER,KEY,BLOCK] <BR>
  !! Komponente "typ"    : Typ des Objektes  [PAR,KEY,BLOCK]                         <BR>
  !! Komponente "block"  : Name des Eingabeblockes                                   <BR>
  !! Komponente "key"    : Schluesselwort                                            <BR>
  !! Komponente "par_nr" : Parameter-Positionsnummer                                 <BR>
  !! Komponente "bl_nr"  : Adress-Nummer des Eingabeblockes                          <BR>
  !! Komponente "key_nr" : Adress-Nummer des Schluesselwortes                        <BR>
  TYPE , PUBLIC :: t_obj
     CHARACTER(LEN=5)                  :: typ
     CHARACTER(LEN=key_len)            :: block
     CHARACTER(LEN=key_len)            :: key
     INTEGER                           :: par_nr
     INTEGER                           :: bl_nr
     INTEGER                           :: key_nr
  END TYPE t_obj
  !
  !! Typ zur Aufnahme von Check-Anforderungen <BR>
  !! Komponente "objekt"  :  Objekt auf dessen Zustand gecheckt wird  [PAR,KEY,BLOCK] <BR>
  !! Komponente "oper"    :  Vergleichs-, bzw. Check-Operation [EXIST,NOTEXIST,EQ,NE,LT,LE,GT,GE] <BR>
  !! Komponente "wert"    :  Vergleichswert  aus Direktangabe <BR>
  TYPE , PUBLIC :: t_check
     TYPE (t_obj)                      :: objekt
     CHARACTER(LEN=10)                 :: oper
     TYPE (t_feld)                     :: wert
  END TYPE t_check
  !
  !! Typ zur Aufnahem der Daten zu einem Eingabe-Parameter <BR>
  !! Komponente "ParPos"   : Position des Parameters in der Eingabe-Zeile <BR>
  !! Komponente "Type"     : Gibt an von welchem Datentyp der Parameter ist <BR>
  !!                         ['INT','REAL','DOUBLE','CHAR','LOG','FILE','DATE','INCR'] <BR>
  !! Komponente "L_Opt"    : True, wenn Eingabeparameter optional <BR>
  !!                         ( ACHTUNG : nur moeglich wenn an letzter ParPos )<BR>
  !!                         Default-Einstellung = False<BR>
  !! Komponente "ReqIf"    : Bedingung bei der der Parameter in der KeyZeile spezifiziert werden muss <BR>
  !! Komponente "NotIf"    : Feld mit Bedingungen bei der der Parameter nicht in der KeyZeile angegeben <BR>
  !!                         werden darf ! <BR>
  !! Komponente "L_Array"  : True, wenn fuer den Parameter in der Eingabezeile mehrere Werte <BR>
  !!                         gleichzeitig angegeben werden duerfen<BR>
  !!                         Default-Einstellung = False<BR>
  !! Komponente "FixValue" : Feld mit erlaubten Eingabewerten <BR>
  !! Komponente "CheckIf"  : Feld mit Vergleichsoperationen die mit dem Eingabewert durchgefuehrt werden sollen <BR>
  !! Komponente "L_FilReq" : wenn Typ = FILE : True, wenn File bereits existieren muss <BR>
  !!                         Default-Einstellung = False<BR>
  !! Komponente "L_FilNew" : wenn typ = FILE : True, wenn File noch nicht existieren darf <BR>
  !!                         Default-Einstellung = False<BR>
  !! Komponente "EGB"      : Feld , welches fuer alle Eingabebloecke diesen Typs die<BR>
  !!                         geprueften Eingabewerte dieses Parameters enthaelt <BR>
  TYPE , PUBLIC :: t_par
     INTEGER                                     :: ParPos
     CHARACTER (LEN= 6)                          :: Type
     LOGICAL                                     :: L_Opt
     TYPE (t_check) , DIMENSION(:) , POINTER     :: ReqIf
     TYPE (t_check) , DIMENSION(:) , POINTER     :: NotIf
     LOGICAL                                     :: L_Array
     TYPE (t_feld)                               :: FixValue
     TYPE (t_check)  , DIMENSION(:) , POINTER    :: CheckIf
     LOGICAL                                     :: L_FilReq
     LOGICAL                                     :: L_FilNew
     TYPE (t_wert), DIMENSION(:) , POINTER       :: EGB
  END TYPE t_par
  !
  !! Typ zur Aufnahem der Daten zu einem Schluesselwort <BR>
  !! Komponente "Name"    : Schluesselwort-String <BR>
  !! Komponente "L_Single"   : True, wenn Schluesselwort im Eingabeblock nur einmal vorkommen darf <BR>
  !!                           Default-Einstellung = True <BR>
  !! Komponente "L_OneArray" : True, wenn Eingabewerte von Mehrfach-KeyZeilen die zu einem <BR>
  !!                           Parameter gehoeren in einem Rueckgabefeld ( also vertikal <BR>
  !!                           gebuendelt) zusammengefasst werden sollen. <BR>
  !!                           Macht nur Sinn, wenn L_Single = False ! <BR>
  !!                           Funktioniert nicht fuer : <BR>
  !!                                 - optional vereinbarte Parameter <BR>
  !!                                 - Arraygroessen-Parameter, der sich die KEY-Zeile mit <BR>
  !!                                   anderen Parametern teilt <BR>
  !!                                    ( Diese Eingabewerte muessen stets mittels Direkt- <BR>
  !!                                      Adressierung ueber Zeilennummer abgefragt werden ! ) <BR>
  !!                           Default-Einstellung = True <BR>
  !! Komponente "L_Opt"     : True, wenn Schluesselwort optional ist <BR>
  !!                          Default-Einstellung = False <BR>
  !! Komponente "ReqIf"     : Feld mit Bedingungen bei der eine solche Schluesselwortzeile <BR>
  !!                          im Eingabeblock spezifiziert werden muss <BR>
  !! Komponente "NotIf"     : Feld mit Bedingungen bei der eine solche Schluesselwortzeile <BR>
  !!                          im Eingabeblock nicht vorhanden sein darf !<BR>
  !! Komponente "L_Comment" : True, wenn Wert einer SchluesselwortZeile als Kommentar <BR>
  !!                          gelesen werden soll <BR>
  !!                          => Blanks gelten nicht als Trennzeichen zwischen Eingaben <BR>
  !!                          False, wenn dem nicht so ist <BR>
  !!                          Default-Einstellung = False <BR>
  !! Komponente "ParAnz"    : Anzahl an Parametern <BR>
  !! Komponente "Par"       : Feld mit Beschreibungen der Eingabeparameter <BR>
  !! Komponente "ZeilAnz"   : Anzahl an Zeilen mit diesem Schluesselwort <BR>
  !!                          im Eingabeblock der Steuerdatei <BR>
  TYPE , PUBLIC :: t_key
     CHARACTER (LEN=key_len)                     :: Name
     LOGICAL                                     :: L_Single
     LOGICAL                                     :: L_OneArray
     LOGICAL                                     :: L_Opt
     TYPE (t_check) , DIMENSION(:) , POINTER     :: ReqIf
     TYPE (t_check) , DIMENSION(:) , POINTER     :: NotIf
     LOGICAL                                     :: L_Comment
     INTEGER                                     :: ParAnz
     TYPE (t_par) , DIMENSION(:) , POINTER       :: Par
     INTEGER      , DIMENSION(:) , POINTER       :: ZeilAnz
  END TYPE t_key
  !
  !! Typ zur Aufnahem der Daten zu einem Eingabeblock <BR>
  !! Komponente "Name"     : Name des Eingabeblockes <BR>
  !! Komponente "L_Single" : True, wenn Eingabeblock (EGB) nur einmal in der Eingabedatei<BR>
  !!                         vorkommen darf <BR>
  !!                         Default-Einstellung = True <BR>
  !! Komponente "L_Opt"    : True, wenn Block optional <BR>
  !! Komponente "ReqIf"    : Bedingung bei der der Eingabeblock in der Steuerdatei vorhanden<BR> 
  !!                         sein muss<BR>
  !! Komponente "NotIf"    : Bedingung bei der der Eingabeblock nicht in der Steuerdatei<BR>
  !!                         vorhanden sein darf ! <BR>
  !! Komponente "EGBAnz"   : Anzahl an Eingabebloecken diesen Typs in Eingabedatei <BR>
  !! Komponente "KeyAnz"   : Anzahl der fuer den Block erlaubten Schluesselwoertern <BR>
  !! Komponente "Key"      : Feld mit erlaubten Schluesselwoertern <BR>
  TYPE , PUBLIC :: t_block
     CHARACTER (LEN=key_len)                     :: Name
     LOGICAL                                     :: L_Single
     LOGICAL                                     :: L_Opt
     TYPE (t_check) , DIMENSION(:) , POINTER     :: ReqIf
     TYPE (t_check) , DIMENSION(:) , POINTER     :: NotIf
     INTEGER                                     :: EGBAnz
     INTEGER                                     :: KeyAnz    ! ? noetig ?
     TYPE (t_key), DIMENSION(:) , POINTER        :: Key
  END TYPE t_block
  !
  !! Typ dient dem Einlesen der Schluesselwort-Zeilen mittels einer temporaeren verketteten Liste <BR>
  !! Komponente "key"   : Schluesselwort                          <BR>
  !! Komponente "wert"  : Wert einer SchluesselwortZeile          <BR>
  !! Komponente "prev"  : Pointer auf vorangehendes Listenelement <BR>
  !! Komponente "next"  : Pointer auf naechstes Listenelement     <BR>
  TYPE , PUBLIC :: t_vl_kz
     CHARACTER (LEN=key_len)      :: key
     CHARACTER (LEN=line_len)     :: wert
     TYPE (t_vl_kz), POINTER      :: prev
     TYPE (t_vl_kz), POINTER      :: next
  END TYPE t_vl_kz
  !
  !! Typ dient dem Einlesen von Feldern mittels einer temporaeren verketteten Liste <BR>
  !! Komponente "feld"  :  <BR>
  !! Komponente "prev"  : Pointer auf vorangehendes Listenelement <BR>
  !! Komponente "next"  : Pointer auf naechstes Listenelement     <BR>
  TYPE , PUBLIC :: t_vl_feld
     TYPE (t_feld)                 :: feld
     TYPE (t_vl_feld), POINTER     :: prev
     TYPE (t_vl_feld), POINTER     :: next
  END TYPE t_vl_feld
  !
  !! Typ dient dem Einlesen der ParameterInformationen mittels einer temporaeren verketteten Liste <BR>
  !! Komponente "par"   :  <BR>
  !! Komponente "prev"  : Pointer auf vorangehendes Listenelement <BR>
  !! Komponente "next"  : Pointer auf naechstes Listenelement     <BR>
  TYPE , PUBLIC :: t_vl_par
     TYPE (t_par)                 :: par
     TYPE (t_vl_par), POINTER     :: prev
     TYPE (t_vl_par), POINTER     :: next
  END TYPE t_vl_par
  !
  !! Typ dient dem Einlesen der KeyInformationen mittels einer temporaeren verketteten Liste <BR>
  !! Komponente "key"   :  <BR>
  !! Komponente "prev"  : Pointer auf vorangehendes Listenelement <BR>
  !! Komponente "next"  : Pointer auf naechstes Listenelement     <BR>
  TYPE , PUBLIC :: t_vl_key
     TYPE (t_key)                 :: key
     TYPE (t_vl_key), POINTER     :: prev
     TYPE (t_vl_key), POINTER     :: next
  END TYPE t_vl_key
  !
  !! Typ dient dem Einlesen der Block-Informationen mittels einer temporaeren verketteten Liste. <BR>
  !! Komponente "block"  :  <BR>
  !! Komponente "prev"   : Pointer auf vorangehendes Listenelement  <BR>
  !! Komponente "next"   : Pointer auf naechstes Listenelement      <BR>
  TYPE , PUBLIC :: t_vl_block
     TYPE (t_block)                 :: block
     TYPE (t_vl_block), POINTER     :: prev
     TYPE (t_vl_block), POINTER     :: next
  END TYPE t_vl_block
  !
  ! [C.3] Variablen [moeglichst nicht verwenden]
  !
  ! [C.4] Schnittstellen
  !
  ! [C.5] Zuweisungen
  !
  ! ... ggf. ergaenzen
  !
  ! [C.6] Operatoren (optional, falls sinnvoll)
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  PUBLIC :: birth_vl_block          !
  PUBLIC :: birth_vl_key            !
  PUBLIC :: birth_vl_par            !
  PUBLIC :: birth_vl_kz             !
  PUBLIC :: init_block              !
  PUBLIC :: init_key                !
  PUBLIC :: init_par                !
  PUBLIC :: init_feld               !
  PUBLIC :: init_objekt             !
  PUBLIC :: init_check              !
  PUBLIC :: dealloc_vl_kz           !
  PUBLIC :: dealloc_t_block         !
  PUBLIC :: dealloc_t_key           !
  PUBLIC :: dealloc_t_par           !
  PUBLIC :: dealloc_t_feld          !
  PUBLIC :: Adressnummern_suchen    !
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
  CHARACTER (LEN=31), PARAMETER :: c_modname      = 'm_stdat_types' ! 
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
  !! Gebaeren und Initialisieren eines neuen Listenelementes
  !! vom Typ <t_vl_block>.<BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE birth_vl_block &
       ( act_block, fst_block, lst_block )
    !
    ! Formalparameter
    !! aktuelles Element einer verketteten Liste
    TYPE (t_vl_block) , POINTER      :: act_block
    !! erstes Element einer verketteten Liste
    TYPE (t_vl_block) , POINTER      :: fst_block
    !! letztes Element einer verketteten Liste
    TYPE (t_vl_block) , POINTER      :: lst_block
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='birth_vl_block'
    !! Statusvariable
    INTEGER :: stat ! 
    !
       !
       ! Allokieren des neuen Listenelementes
       !
       ALLOCATE ( act_block, STAT=stat )
       !
       IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
          !
          CALL setup_error_act &
                ( all_errors(:), -10000, c_upname, c_modname, stat )
          CALL setup_error_act ( '<felder>', 'act_block' )
          !
       END IF
       !
       ! Initialisieren der <t_block>-Komponente
       !
       IF ( no_error( ) ) CALL init_block ( act_block%block )
       !
       IF ( no_error( ) ) THEN
          !
          ! alle Pointer richtig setzen 
          !
          NULLIFY(act_block%next)
          !
          IF (.NOT. ASSOCIATED(fst_block)) THEN
             fst_block   => act_block
             NULLIFY(act_block%prev)
          ELSE
             act_block%prev => lst_block
             lst_block%next => act_block
          END IF
          !
          lst_block      => act_block
          !
       END IF ! no_error( )
       !
    !
  END SUBROUTINE birth_vl_block
  !
  !! Gebaeren und Initialisieren eines neuen Listenelementes
  !! vom Typ <t_vl_key>.<BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE birth_vl_key &
       ( act_key, fst_key, lst_key )
    !
    ! Formalparameter
    !! aktuelles Element einer verketteten Liste
    TYPE (t_vl_key)  , POINTER      :: act_key
    !! erstes Element einer verketteten Liste
    TYPE (t_vl_key)  , POINTER      :: fst_key
    !! letztes Element einer verketteten Liste
    TYPE (t_vl_key)  , POINTER      :: lst_key
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='birth_vl_key'
    !! Statusvariable
    INTEGER :: stat ! 
    !
       !
       ! Allokieren des neuen Listenelementes
       !
       ALLOCATE ( act_key, STAT=stat )
       !
       IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
          !
          CALL setup_error_act &
                ( all_errors(:), -10000, c_upname, c_modname, stat )
          CALL setup_error_act ( '<felder>', 'act_key' )
          !
       END IF
       !
       ! Initialisieren der <t_key>-Komponente
       !
       IF ( no_error( ) ) CALL init_key ( act_key%key )
       !
       IF ( no_error( ) ) THEN
          !
          ! alle Pointer richtig setzen 
          !
          NULLIFY(act_key%next)
          !
          IF (.NOT. ASSOCIATED(fst_key)) THEN
             fst_key   => act_key
             NULLIFY(act_key%prev)
          ELSE
             act_key%prev => lst_key
             lst_key%next => act_key
          END IF
          !
          lst_key      => act_key
          !
       END IF ! no_error( )
       !
    !
  END SUBROUTINE birth_vl_key
  !
  !! Gebaeren und Initialisieren eines neuen Listenelementes
  !! vom Typ <t_vl_par>.<BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE birth_vl_par &
       ( act_par, fst_par, lst_par )
    !
    ! Formalparameter
    !! aktuelles Element einer verketteten Liste
    TYPE (t_vl_par)  , POINTER         :: act_par
    !! erstes Element einer verketteten Liste
    TYPE (t_vl_par)  , POINTER         :: fst_par
    !! letztes Element einer verketteten Liste
    TYPE (t_vl_par)  , POINTER         :: lst_par
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='birth_vl_par'
    !! Statusvariable
    INTEGER :: stat ! 
    !
       !
       ! Allokieren des neuen Listenelementes
       !
       ALLOCATE ( act_par, STAT=stat )
       !
       IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
          !
          CALL setup_error_act &
                ( all_errors(:), -10000, c_upname, c_modname, stat )
          CALL setup_error_act ( '<felder>', 'act_par' )
          !
       END IF
       !
       ! Initialisieren der <t_par>-Komponente
       !
       IF ( no_error( ) ) CALL init_par ( act_par%par)
       !
       IF ( no_error( ) ) THEN
          !
          ! alle Pointer richtig setzen 
          !
          NULLIFY(act_par%next)
          !
          IF (.NOT. ASSOCIATED(fst_par)) THEN
             fst_par   => act_par
             NULLIFY(act_par%prev)
          ELSE
             act_par%prev => lst_par
             lst_par%next => act_par
          END IF
          !
          lst_par      => act_par
          !
       END IF ! no_error( )
       !
    !
  END SUBROUTINE birth_vl_par
  !
  !! Gebaeren und Initialisieren eines neuen Listenelementes
  !! vom Typ t_vl_kz.<BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE birth_vl_kz &
       ( fst_kz, lst_kz, act_kz )
    !
    ! Formalparameter
    !! Erstes Element einer verketteten Liste
    TYPE (t_vl_kz) , POINTER          :: fst_kz
    !! Letztes Listenelement
    TYPE (t_vl_kz) , POINTER          :: lst_kz
    !! Neu generiertes aktuelles Listenelement
    TYPE (t_vl_kz) , POINTER          :: act_kz
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='birth_vl_kz'
    !! Statusvariable
    INTEGER :: stat ! 
    !
       !
       ! Allokieren des neuen Listenelementes
       !
       ALLOCATE ( act_kz, STAT=stat )
       !
       IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
          !
          CALL setup_error_act &
                ( all_errors(:), -10000, c_upname, c_modname, stat )
          CALL setup_error_act ( '<felder>', 'act_kz' )
          !
       END IF
       !
       ! Initialisieren der Komponenten
       !
       IF ( no_error( ) ) THEN
          !
          act_kz%key  = REPEAT(' ', LEN(act_kz%key ))
          act_kz%wert = REPEAT(' ', LEN(act_kz%wert))
          !
       END IF
       !
       IF ( no_error( ) ) THEN
          !
          ! alle Pointer richtig setzen 
          !
          NULLIFY(act_kz%next)
          !
          IF (.NOT. ASSOCIATED(fst_kz)) THEN
             fst_kz   => act_kz
             NULLIFY(act_kz%prev)
          ELSE
             act_kz%prev => lst_kz
             lst_kz%next => act_kz
          END IF
          !
          lst_kz      => act_kz
          !
       END IF ! no_error( )
       !
    !
  END SUBROUTINE birth_vl_kz
  !
  !! Initialisiert Variable vom Typ t_block <BR>
  SUBROUTINE init_block &
       ( block )
    !
    ! Formalparameter
    !! Variable fuer Blocktyp einer Eingabedatei
    TYPE (t_block)  , INTENT(  OUT)   :: block
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='init_block'
    !
    ! Initialisierungen durchfuehren
    ! 
    block%Name      =  REPEAT(' ', LEN(block%Name))
    block%L_Single  =  .True.
    block%L_Opt     =  .False.
    !
    block%EGBAnz   =  0
    !
    block%KeyAnz   =  0
    !
    NULLIFY (block%ReqIf)
    NULLIFY (block%NotIf)
    NULLIFY (block%Key)
    !
  END SUBROUTINE init_block
  !
  !! Initialisiert Variable vom Typ <t_key>
  SUBROUTINE init_key &
       ( key )
    !
    ! Formalparameter
    !! Variable fuer KeyZeile einer Eingabedatei
    TYPE (t_key  )  , INTENT(  OUT)   :: key
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='init_key'
    !
    ! Initialisierungen durchfuehren
    !
    key%Name        =  REPEAT(' ', LEN(key%Name))
    key%L_Single    =  .True.
    !
    key%L_OneArray  =  .True.
    !
    key%L_Opt       =  .False.
    NULLIFY (key%ReqIf)
    NULLIFY (key%NotIf)
    !
    key%L_Comment  =  .False.
    !
    key%ParAnz      =  0
    NULLIFY (key%Par)
    !
    NULLIFY (key%ZeilAnz)
    !
  END SUBROUTINE init_key
  !
  !! Initialisiert Variable vom Typ <t_par>.
  SUBROUTINE init_par &
       ( par )
    !
    ! Formalparameter
    !! Variable fuer Parameter einer Eingabedatei
    TYPE (t_par)    , INTENT(  OUT)   :: par
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='init_par'
    !
    ! Initialisierungen durchfuehren
    !
    par%ParPos   =  1
    par%Type     =  REPEAT(' ', LEN(par%Type))
    !
    par%L_Opt    =  .False.
    NULLIFY (par%ReqIf)
    NULLIFY (par%NotIf)
    NULLIFY (par%CheckIf)
    !
    par%L_Array  = .False.
    !
    CALL init_feld ( par%FixValue )
    !
    IF ( no_error( ) ) THEN
       !
       par%L_FilReq = .False.
       par%L_FilNew = .False.
       !
       NULLIFY (par%EGB)
       !
    END IF ! no_error( )
    !
  END SUBROUTINE init_par
  !
  !! Deallokiert alle Elemente einer verketteten Liste zur
  !! Aufnahme von Schluesselwort-Zeilen.<BR>
  SUBROUTINE dealloc_vl_kz &
       ( act_kz, fst_kz, lst_kz, vgl_kz )
    !
    ! Formalparameter
    !! Pointer auf aktuelles Listenelement
    TYPE (t_vl_kz)     , POINTER      :: act_kz
    !! Pointer auf erstes Listenelement
    TYPE (t_vl_kz)     , POINTER      :: fst_kz
    !! Pointer auf letztes Listenelement
    TYPE (t_vl_kz)     , POINTER      :: lst_kz
    !! Pointer auf Vergleichs- Listenelement
    TYPE (t_vl_kz)     , POINTER      :: vgl_kz
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='dealloc_vl_kz'
    !
    ! Elemente der verketteten Liste nacheinander deallokieren
    !
    ! ... erstes Listenelement
    !
    act_kz => fst_kz
    !
    ! ... Schleife ueber alle Listenelemente
    !
    vlist : DO
       !
       IF ( .NOT. ASSOCIATED(act_kz) ) EXIT vlist
       !
       ! ... naechsten Element
       !
       lst_kz  => act_kz
       act_kz  => act_kz%next
       !
       ! ... Deallokieren des wegsortierten Listenelementes
       DEALLOCATE (lst_kz)
       !      
    END DO vlist
    !
    !   Pointerverbindungen loesen
    !
    NULLIFY (fst_kz)
    NULLIFY (act_kz)
    NULLIFY (lst_kz)
    NULLIFY (vgl_kz)
    !
  END SUBROUTINE dealloc_vl_kz
  !
  !! De-Allokieren der dynamischen KomponentenFelder des
  !! Variablentyps <t_block>.<BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_t_block &
       ( block )
    !
    ! Formalparameter
    !! Variable zur Beschreibung einer Blockstruktur einer Eingabedatei
    TYPE (t_block)  , INTENT(INOUT)              :: block
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='dealloc_t_block'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Zaehler
    INTEGER :: i
    !
       !
       ! De-Allokieren des Speicherplatzes
       !
       IF ( ASSOCIATED(block%ReqIf) ) THEN
          !
          ! block%ReqIf
          !
          DO i = 1, size(block%ReqIf)
             !
             IF ( any_error( ) ) EXIT
             !
             CALL dealloc_t_check ( block%ReqIf(i) )
             !
          END DO
          !
          IF ( no_error( ) ) THEN
             !
             DEALLOCATE ( block%ReqIf, STAT=stat )
             !
             IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Deallocate
                !
                CALL setup_error_act &
                      ( all_errors(:), -20000, c_upname, c_modname, stat )
                CALL setup_error_act ( '<felder>', 'block%ReqIf' )
                !
             END IF
             !
             IF ( no_error( ) ) NULLIFY (block%ReqIf)
             !
          END IF ! no_error( )
          !
       END IF ! ASSOCIATED(block%ReqIf)
       !
       IF ( no_error( ) ) THEN
          !
          IF ( ASSOCIATED(block%NotIf) ) THEN
             !
             ! block%NotIf
             !
             DO i = 1, size(block%NotIf)
                !
                IF ( any_error( ) ) EXIT
                !
                CALL dealloc_t_check ( block%NotIf(i) )
                !
             END DO
             !
             IF ( no_error( ) ) THEN
                !
                DEALLOCATE ( block%NotIf, STAT=stat )
                !
                IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Deallocate
                   !
                   CALL setup_error_act &
                         ( all_errors(:), -20000, c_upname, c_modname, stat )
                   CALL setup_error_act ( '<felder>', 'block%NotIf' )
                   !
                END IF
                !
                IF ( no_error( ) ) NULLIFY (block%NotIf)
                !
             END IF ! no_error( )
             !
          END IF ! ASSOCIATED(block%NotIf)
          !
       END IF ! no_error( )
       !
       IF ( no_error( ) ) THEN
          !
          IF ( ASSOCIATED(block%Key) ) THEN
             !
             ! block%Key
             !
             DO i = 1, size(block%Key)
                !
                IF ( any_error( ) ) EXIT
                !
                CALL dealloc_t_key ( block%Key(i) )
                !
             END DO
             !
             IF ( no_error( ) ) THEN
                !
                DEALLOCATE ( block%Key, STAT=stat )
                !
                IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Deallocate
                   !
                   CALL setup_error_act &
                         ( all_errors(:), -20000, c_upname, c_modname, stat )
                   CALL setup_error_act ( '<felder>', 'block%Key' )
                   !
                END IF
                !
                IF ( no_error( ) ) NULLIFY (block%Key)
                !
             END IF ! no_error( )
             !
          END IF ! ASSOCIATED(block%Key)
          !
       END IF ! no_error( )
       !
    !
  END SUBROUTINE dealloc_t_block
  !
  !! De-Allokieren der dynamischen KomponentenFelder des
  !! Variablentyps <t_key>. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_t_key &
       ( key )
    !
    ! Formalparameter
    !! Variable zur Beschreibung eines Schluesselwortes eines Eingabeblockes
    TYPE (t_key)    , INTENT(INOUT)              :: key
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='dealloc_t_key'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Zaehler
    INTEGER :: i
    !
       !
       ! De-Allokieren des Speicherplatzes
       !
       IF ( ASSOCIATED(key%ReqIf) ) THEN
          !
          ! key%ReqIf
          !
          DO i = 1, size(key%ReqIf)
             !
             IF ( any_error( ) ) EXIT
             !
             CALL dealloc_t_check ( key%ReqIf(i) )
             !
          END DO
          !
          IF ( no_error( ) ) THEN
             !
             DEALLOCATE ( key%ReqIf, STAT=stat )
             !
             IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Deallocate
                !
                CALL setup_error_act &
                      ( all_errors(:), -20000, c_upname, c_modname, stat )
                CALL setup_error_act ( '<felder>', 'key%ReqIf' )
                !
             END IF
             !
             IF ( no_error( ) ) NULLIFY (key%ReqIf)
             !
          END IF ! no_error( )
          !
       END IF ! ASSOCIATED(key%ReqIf)
       !
       IF ( no_error( ) ) THEN
          !
          IF ( ASSOCIATED(key%NotIf) ) THEN
             !
             ! key%NotIf
             !
             DO i = 1, size(key%NotIf)
                !
                IF ( any_error( ) ) EXIT
                !
                CALL dealloc_t_check ( key%NotIf(i) )
                !
             END DO
             !
             IF ( no_error( ) ) THEN
                !
                DEALLOCATE ( key%NotIf, STAT=stat )
                !
                IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Deallocate
                   !
                   CALL setup_error_act &
                         ( all_errors(:), -20000, c_upname, c_modname, stat )
                   CALL setup_error_act ( '<felder>', 'key%NotIf' )
                   !
                END IF
                !
                IF ( no_error( ) ) NULLIFY (key%NotIf)
                !
             END IF ! no_error( )
             !
          END IF ! ASSOCIATED(key%NotIf)
          !
       END IF ! no_error( )
       !
       IF ( no_error( ) ) THEN
          !
          IF ( ASSOCIATED(key%Par) ) THEN
             !
             ! key%Par
             !
             DO i = 1, size(key%Par)
                !
                IF ( any_error( ) ) EXIT
                !
                CALL dealloc_t_par ( key%Par(i) )
                !
             END DO
             !
             IF ( no_error( ) ) THEN
                !
                DEALLOCATE ( key%Par, STAT=stat )
                !
                IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Deallocate
                   !
                   CALL setup_error_act &
                         ( all_errors(:), -20000, c_upname, c_modname, stat )
                   CALL setup_error_act ( '<felder>', 'key%Par' )
                   !
                END IF
                !
                IF ( no_error( ) ) NULLIFY (key%Par)
                !
             END IF ! no_error( )
             !
          END IF ! ASSOCIATED(key%Par)
          !
       END IF ! no_error( )
       !
       IF ( no_error( ) ) THEN
          !
          IF ( ASSOCIATED(key%ZeilAnz) ) THEN
             !
             ! key%ZeilAnz
             !
             DEALLOCATE ( key%ZeilAnz, STAT=stat )
             !
             IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Deallocate
                !
                CALL setup_error_act &
                      ( all_errors(:), -20000, c_upname, c_modname, stat )
                CALL setup_error_act ( '<felder>', 'key%ZeilAnz' )
                !
             END IF
             !
             IF ( no_error( ) ) NULLIFY (key%ZeilAnz)
             !
          END IF ! ASSOCIATED(key%ZeilAnz)
          !
       END IF ! no_error( )
       !
    !
  END SUBROUTINE dealloc_t_key
  !
  !! De-Allokieren der dynamischen KomponentenFelder des
  !! Variablentyps <t_par>.<BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_t_par &
       ( par )
    !
    ! Formalparameter
    !! Variable zur Beschreibung eines Parameters einer Eingabezeile
    TYPE (t_par)    , INTENT(INOUT)              :: par
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='dealloc_t_par'
    !! Statusvariable
    INTEGER  :: stat ! 
    ! 
    INTEGER  :: i, j
    !
       !
       ! De-Allokieren des Speicherplatzes
       !
       IF ( ASSOCIATED(par%ReqIf) ) THEN
          !
          ! par%ReqIf
          !
          DO i = 1, size(par%ReqIf)
             !
             IF ( any_error( ) ) EXIT
             !
             CALL dealloc_t_check ( par%ReqIf(i) )
             !
          END DO
          !
          IF ( no_error( ) ) THEN
             !
             DEALLOCATE ( par%ReqIf, STAT=stat )
             !
             IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Deallocate
                !
                CALL setup_error_act &
                      ( all_errors(:), -20000, c_upname, c_modname, stat )
                CALL setup_error_act ( '<felder>', 'par%ReqIf' )
                !
             END IF
             !
             IF ( no_error( ) ) NULLIFY (par%ReqIf)
             !
          END IF ! no_error( )
          !
       END IF ! ASSOCIATED(par%ReqIf)
       !
       !
       IF ( no_error( ) ) THEN
          !
          IF ( ASSOCIATED(par%NotIf) ) THEN
             !
             ! par%NotIf
             !
             DO i = 1, size(par%NotIf)
                !
                IF ( any_error( ) ) EXIT
                !
                CALL dealloc_t_check ( par%NotIf(i) )
                !
             END DO
             !
             IF ( no_error( ) ) THEN
                !
                DEALLOCATE ( par%NotIf, STAT=stat )
                !
                IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Deallocate
                   !
                   CALL setup_error_act &
                         ( all_errors(:), -20000, c_upname, c_modname, stat )
                   CALL setup_error_act ( '<felder>', 'par%NotIf' )
                   !
                END IF
                !
                IF ( no_error( ) ) NULLIFY (par%NotIf)
                !
             END IF ! no_error( )
             !
          END IF ! ASSOCIATED(par%NotIf)
          !
       END IF ! no_error( )
       !
       IF ( no_error( ) ) THEN
          !
          IF ( ASSOCIATED(par%CheckIf) ) THEN
             !
             ! par%CheckIf
             !
             DO i = 1, size(par%CheckIf)
                !
                IF ( any_error( ) ) EXIT
                !
                CALL dealloc_t_check ( par%CheckIf(i) )
                !
             END DO
             !
             IF ( no_error( ) ) THEN
                !
                DEALLOCATE ( par%CheckIf, STAT=stat )
                !
                IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Deallocate
                   !
                   CALL setup_error_act &
                         ( all_errors(:), -20000, c_upname, c_modname, stat )
                   CALL setup_error_act ( '<felder>', 'par%CheckIf' )
                   !
                END IF
                !
                IF ( no_error( ) ) NULLIFY (par%CheckIf)
                !
             END IF ! no_error( )
             !
          END IF ! ASSOCIATED(par%CheckIf)
          !
       END IF ! no_error( )
       !
       !
       ! par%FixValue
       !
       IF ( no_error( ) ) CALL dealloc_t_feld ( par%FixValue )
       !
       IF ( no_error( ) ) THEN
          !
          ! par%EGB
          !
          IF ( ASSOCIATED(par%EGB) ) THEN
             !
             DO i = 1, SIZE(par%EGB)
                !
                IF ( any_error( ) ) EXIT
                !
                IF ( ASSOCIATED(par%EGB(i)%Wert) ) THEN
                   !
                   DO j = 1, SIZE(par%EGB(i)%Wert)
                      !
                      IF ( any_error( ) ) EXIT
                      !
                      CALL dealloc_t_feld ( par%EGB(i)%Wert(j) )
                      !
                   END DO
                   !
                   IF ( no_error( ) ) NULLIFY (par%EGB(i)%Wert)
                   !
                END IF
                !
             END DO
             !
             IF ( no_error( ) ) NULLIFY (par%EGB)
             !
          END IF ! ASSOCIATED(par%EGB)
          !
       END IF ! no_error( )
       !
    !
  END SUBROUTINE dealloc_t_par
  !
  !! De-Allokieren der dynamischen KomponentenFelder des
  !! Variablentyps <t_check>.<BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_t_check &
       ( mycheck )
    !
    ! Formalparameter
    !! Variable zur Beschreibung einer Check-Anforderung
    TYPE (t_check)    , INTENT(INOUT)            :: mycheck
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='dealloc_t_check'
    !
    INTEGER                        :: i
    !
    ! De-Allokieren des Speicherplatzes
    ! 
    !   check%wert
    !
    CALL dealloc_t_feld ( mycheck%wert )
    !
  END SUBROUTINE dealloc_t_check
  !
  !! De-Allokieren der dynamischen KomponentenFelder des
  !! Variablentyps <t_feld>.<BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE dealloc_t_feld &
       ( feld )
    !
    ! Formalparameter
    !! Variable zur Aufnahme eines Feldes in Abhaengigkeit von seinem Datentyp
    TYPE (t_feld)    , INTENT(INOUT)             :: feld
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='dealloc_t_feld'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    INTEGER                        :: i
    !
       !
       ! feld%int
       !
       IF ( ASSOCIATED(feld%int) ) THEN
          !
          DEALLOCATE ( feld%int, STAT=stat )
          !
          IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Deallocate
             !
             CALL setup_error_act &
                   ( all_errors(:), -20000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<felder>', 'feld%int' )
             !
          END IF
          !
          IF ( no_error( ) ) NULLIFY (feld%int)
          !
       END IF
       !
       ! feld%real
       !
       IF ( no_error( ) ) THEN
          !
          IF ( ASSOCIATED(feld%real) ) THEN
             !
             DEALLOCATE ( feld%real, STAT=stat )
             !
             IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Deallocate
                !
                CALL setup_error_act &
                      ( all_errors(:), -20000, c_upname, c_modname, stat )
                CALL setup_error_act ( '<felder>', 'feld%real' )
                !
             END IF
             !
             IF ( no_error( ) ) NULLIFY (feld%real)
             !
          END IF
          !
       END IF ! no_error( )
       !
       ! feld%char
       !
       IF ( no_error( ) ) THEN
          !
          IF ( ASSOCIATED(feld%char) ) THEN
             !
             DEALLOCATE ( feld%char, STAT=stat )
             !
             IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Deallocate
                !
                CALL setup_error_act &
                      ( all_errors(:), -20000, c_upname, c_modname, stat )
                CALL setup_error_act ( '<felder>', 'feld%char' )
                !
             END IF
             !
             IF ( no_error( ) ) NULLIFY (feld%char)
             !
          END IF
          !
       END IF ! no_error( )
       !
       ! feld%log
       !
       IF ( no_error( ) ) THEN
          !
          IF ( ASSOCIATED(feld%log) ) THEN
             !
             DEALLOCATE ( feld%log, STAT=stat )
             !
             IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Deallocate
                !
                CALL setup_error_act &
                      ( all_errors(:), -20000, c_upname, c_modname, stat )
                CALL setup_error_act ( '<felder>', 'feld%log' )
                !
             END IF
             !
             IF ( no_error( ) ) NULLIFY (feld%log)
             !
          END IF
          !
       END IF ! no_error( )
       !
       ! feld%doub
       !
       IF ( no_error( ) ) THEN
          !
          IF ( ASSOCIATED(feld%doub) ) THEN
             !
             DEALLOCATE ( feld%doub, STAT=stat )
             !
             IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Deallocate
                !
                CALL setup_error_act &
                      ( all_errors(:), -20000, c_upname, c_modname, stat )
                CALL setup_error_act ( '<felder>', 'feld%doub' )
                !
             END IF
             !
             IF ( no_error( ) ) NULLIFY (feld%doub)
             !
          END IF
          !
       END IF ! no_error( )
       !
    !
  END SUBROUTINE dealloc_t_feld
  !
  !! Initialisiert Variable vom Typ <t_feld>.<BR>
  SUBROUTINE init_feld &
       ( feld )
    !
    ! Formalparameter
    !! Variable zur Aufnahme eines Feldes in Abhaengigkeit
    !! von seinem Datentyp
    TYPE (t_feld)    , INTENT(  OUT)   :: feld
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='init_feld'
    !
    ! Initialisierungen durchfuehren
    !
    NULLIFY (feld%int)
    NULLIFY (feld%real)
    NULLIFY (feld%char)
    NULLIFY (feld%log)
    NULLIFY (feld%doub)
    !
  END SUBROUTINE init_feld
  !
  !! Initialisiert Variable vom Typ <t_obj>.<BR>
  SUBROUTINE init_objekt &
       ( objekt )
    !
    ! Formalparameter
    !! Variable zur Beschreibung eines Dateielementes
    !! [PARAMETER,KEY,BLOCK]
    TYPE (t_obj)     , INTENT(  OUT)   :: objekt
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='init_objekt'
    !
    ! Komponenten des Datentyps <t_obj> initialisieren
    !
    objekt%typ    =  REPEAT(' ', LEN(objekt%typ  ))
    objekt%block  =  REPEAT(' ', LEN(objekt%block))
    objekt%key    =  REPEAT(' ', LEN(objekt%key  ))
    objekt%par_nr =  0
    objekt%bl_nr  =  0
    objekt%key_nr =  0
    !
  END SUBROUTINE init_objekt
  !
  !! Initialisiert Variable vom Typ <t_check>. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE init_check &
       ( mycheck )
    !
    ! Formalparameter
    !! Variable zur Aufnahme von Check-Anforderungen
    TYPE (t_check)   , INTENT(  OUT)   :: mycheck
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='init_check'
    !
    ! check%objekt
    !
    CALL init_objekt ( mycheck%objekt )
    !
    ! check%oper
    !
    IF ( no_error( ) ) mycheck%oper = REPEAT(' ', LEN(mycheck%oper ))
    !
    ! check%wert
    !
    IF ( no_error( ) ) CALL init_feld ( mycheck%wert )
    !
  END SUBROUTINE init_check
  !
  !! UP sucht die Adressnummern eines Datei-Elementes.<BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE  Adressnummern_suchen &
       ( such_obj, bloecke, bl_nr, key_nr, par_nr )
    !
    ! Formalparameter
    !!
    !! Variable mit Objekt aus der Check-Anforderung
    TYPE (t_obj)                     , INTENT(IN   )  :: such_obj
    !! Feld mit gesammter Dictionary-Information
    TYPE (t_block) , DIMENSION(:)    , POINTER        :: bloecke
    !! Block-Nummer des SuchObjektes
    INTEGER                          , INTENT(  OUT)  :: bl_nr
    !! Key-Nummer   des SuchObjektes
    INTEGER                          , INTENT(  OUT)  :: key_nr
    !! Parameter-Nummer des SuchObjektes
    INTEGER                          , INTENT(  OUT)  :: par_nr
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='Adressnummern_suchen'
    INTEGER                             :: i
    LOGICAL                             :: l_def
    !
       !
       ! Initialisierungen
       !
       bl_nr   = 0
       key_nr  = 0
       par_nr  = 0
       !
       IF ( .NOT. ASSOCIATED(bloecke)) THEN
          !
          ! Fehler -2010 : Der Pointerfeld <bloecke> .not. associated
          !
          CALL setup_error_act ( all_errors(:), -2010, c_upname, c_modname )
          !
          RETURN
          !
       END IF
       !
       ! SUCH_OBJ in <bloecke> aufspueren
       !
       IF ( LEN_TRIM(such_obj%block) .EQ. 0 ) THEN
          !
          ! Fehler -2020 : kein Blockname spezifiziert
          !
          CALL setup_error_act ( all_errors(:), -2020, c_upname, c_modname )
          !
          RETURN
          !
       END IF
       !
       ! ... in jedem Fall Blocknummer suchen
       !
       DO i = 1, SIZE(bloecke)
          !
          IF ( bloecke(i)%Name .EQ. such_obj%block ) THEN
             !   
             bl_nr = i
             !
          END IF
          !
       END DO
       !
       ! ... wenn Blocknummer gefunden wurde, ggf. Key suchen
       !
       IF ( bl_nr .GT. 0 ) THEN
       
          SELECT CASE (such_obj%typ)
          CASE('PAR','KEY')
             !
             IF ( LEN_TRIM(such_obj%key) .EQ. 0 ) THEN
                !
                ! Fehler -2030 : kein Key-Name spezifiziert
                !
                CALL setup_error_act ( all_errors(:), -2030, c_upname, c_modname )
                !
                RETURN
                !
             END IF
             !
             DO i = 1, SIZE(bloecke(bl_nr)%Key)
                !
                IF ( bloecke(bl_nr)%Key(i)%Name .EQ. such_obj%key ) THEN
                   !
                   key_nr = i
                   !
                END IF
                !
             END DO   ! i
             !
          END SELECT
          !
       END IF
       !
       ! ... wenn Blocknummer & Keynummer bekannt, ggf. par_nr suchen
       !
       IF ( bl_nr .GT. 0  .AND.  key_nr .GT. 0 ) THEN
          !    
          SELECT CASE (such_obj%typ)
          CASE('PAR')
             !
             IF ( such_obj%par_nr .EQ. 0 ) THEN
                !
                ! Fehler -2040 : keine Parameter-Nummer spezifiziert
                !
                CALL setup_error_act ( all_errors(:), -2040, c_upname, c_modname )
                !
                RETURN
                !
             END IF
             !
             DO i = 1, SIZE(bloecke(bl_nr)%Key(key_nr)%Par)
                !
                IF ( bloecke(bl_nr)%Key(key_nr)%Par(i)%ParPos .EQ. &
                     such_obj%par_nr ) THEN
                   !
                   par_nr   = i
                   !
                END IF
                !
             END DO   ! i
             !
          END SELECT
          !
       END IF
       !
    !
  END SUBROUTINE Adressnummern_suchen
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
END MODULE m_stdat_types
!------------------------------------------------------------------------------
! Ende des Source-Codes des Moduls
!------------------------------------------------------------------------------
