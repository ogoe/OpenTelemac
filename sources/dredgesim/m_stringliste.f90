!! <H2>M_STRINGLISTE</h2>
!! @author Susanne Spohr
!! @version 1.4 vom 15.03 07, Quellcode: mod_m_stringliste.f90
!! <HR>
!! Handling stringlists read / write <BR>
!! <HR>
!! <H3>Copyright-Hinweis</H3>
!!                                                                   <BR>
!! Copyright (C) 2002 <A HREF="http://www.hamburg.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!!                                                                   <BR>
!! <HR>
!! <H3>Entwicklungsgeschichte des Modules</H3>
!! 01.01 : 2002-05-22 : Susanne Spohr : Re-Engineering des Moduls mod_stringliste.f90 der Library basismodule <BR>
!! 01.12 : 2007-03-15 : G. Lang       : Sonderbehandlung fuer zwischen Anfuehrungszeichen liegende Leerzeichen,
!!                                      z.B. "gitter  05.dat" (fuer Leerzeichen in Pfad- und Dateinamen).
!!                                      Diese Leerzeichen werden temporaer durch ? ersetzt
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Modulss</H3>
!! Umgang mit Stringlisten schreiben /lesen                         <BR>
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
MODULE m_stringliste
  !
  ! ----------------------------------------------------------------------
  ! [A] Basis-Module mit haeufig benutzten Methoden
  ! ----------------------------------------------------------------------
  !
  ! [A.1] BASIS-Modul mit globalen Konstantwerten
  !
  USE b_constants, ONLY : &
       ! Parameter
       single,         &
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
  USE m_stdat_types, ONLY : &
       !   Parameter 
       key_len
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
  !! char_len = enthaelt die zulaessige Characterlaenge
  INTEGER, PUBLIC, PARAMETER :: char_len = key_len
  !
  ! [C.3] Variablen [moeglichst nicht verwenden]
  !
  ! [C.4] Schnittstellen
  !
  !! Lesen einer Stringliste
  INTERFACE read_liste
     MODULE PROCEDURE read_liste_char
     MODULE PROCEDURE read_liste_int
     MODULE PROCEDURE read_liste_real
     MODULE PROCEDURE read_liste_doub
     MODULE PROCEDURE read_liste_log
  END INTERFACE
  !
  !! Erstellen einer Stringliste
  INTERFACE make_liste
     MODULE PROCEDURE make_liste_char !
     MODULE PROCEDURE make_liste_int
     MODULE PROCEDURE make_liste_real
     MODULE PROCEDURE make_liste_doub
     MODULE PROCEDURE make_liste_log
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
  PUBLIC :: read_liste                         ! Lesen einer Stringliste
  PUBLIC :: make_liste                         ! Erstellen einer Stringliste
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
  CHARACTER (LEN=31), PARAMETER :: c_modname      = 'm_stringliste' ! 
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
  ! >>> MODULSPEZIFISCHE-PUBLIC-Methoden <<< [ERR_NO < 0]
  ! ----------------------------------------------------------------------
  !
  !! Liest einen String der eine Liste enthaelt, deren
  !! Elemente durch ein anzugebendes Trennzeichen getrennt sind. <BR>
  !! Des weiteren werden in "" eingeschlossene Blanks maskiert und
  !! daher nicht als Trennzeichen mitgez&auml;hlt                <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_liste_char &
       ( list_string, sign, liste, delim, repla )
    !
    ! Formalparameter
    !! String mit Liste der Elemente
    CHARACTER (LEN=*)            , INTENT(IN   )        :: list_string ! 
    !! Trennzeichen
    CHARACTER (LEN=*)            , INTENT(IN   )        :: sign        ! 
    !! Feld mit Character-Listenelementen
    CHARACTER (LEN=key_len) , DIMENSION(:) , POINTER    :: liste       ! 
    !! (optional) Delimiter-Zeichen
    CHARACTER (LEN=1) , OPTIONAL , INTENT(IN)           :: delim       !  
    !! (optional) vorlaeufiges Ersatzzeichen fuer "sign" zwischen zwei "delim"
    CHARACTER (LEN=1) , OPTIONAL , INTENT(IN)           :: repla       ! 
   !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=15), PARAMETER :: c_upname='read_liste_char' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    CHARACTER (LEN=LEN(list_string))             :: string              ! 
    CHARACTER (LEN=4)                            :: c_listlen           ! 
    INTEGER                                      :: tz_anz, dl_anz, i, i_pos, & ! 
                                                    i_anf, i_end, nn ! 
    INTEGER  , DIMENSION(:,:) , ALLOCATABLE      :: tz_pos, dl_pos      ! 
    !
    !
    IF ( LEN(sign) .GT. 1 ) THEN
       !
       ! Fehler -1530 : SIGN laenger als ein Zeichen !
       !
       CALL setup_error_act ( all_errors(:), -1530, c_upname, c_modname )
       !
       RETURN
       !
    END IF
    !
    IF ( LEN_TRIM(list_string) .EQ. 0 ) THEN
       !
       ! Fehler -1540 : LIST_STRING ist ein Blankstring !
       !
       CALL setup_error_act ( all_errors(:), -1540, c_upname, c_modname )
       !
       RETURN
       !
    END IF
    !
    IF ( ASSOCIATED(liste) ) THEN
       !
       ! Fehler -1550 : Pointerfeld liste bereits associated !
       !
       CALL setup_error_act ( all_errors(:), -1550, c_upname, c_modname )
       CALL setup_error_act ( '<liste>', 'LISTE' )
       !
       RETURN
       !
    END IF
    !
    ! Blanks vor List_String cutten
    !
    string = REPEAT(' ', LEN(string))
    string = ADJUSTL(list_string)
    !
    ! Allokieren und Initialisierung des lokalen Feldes mit den Trennzeichen-
    ! Positionen
    !
    ALLOCATE ( tz_pos(LEN_TRIM(string),2),dl_pos(LEN_TRIM(string),2),STAT =stat )
    !
    IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
       !
       CALL setup_error_act &
                ( all_errors(:), -10000, c_upname, c_modname, stat )
       CALL setup_error_act ( '<felder>', 'tz_pos, dl_pos' )
       !
       RETURN
       !
    END IF
    ! 
    tz_pos = 0  ! Feldoperation
    dl_pos = 0  ! Feldoperation
    tz_anz = 0
    dl_anz = 0
    nn     = 0  
    !
    ! Ermittle das Auftreten von Delimiter-Zeichen im String ------------
    !
    IF ( PRESENT(delim) ) THEN
       DO i_pos=1,LEN_TRIM(string)
          IF ( string(i_pos:i_pos) == delim ) THEN
             nn = nn + 1
             SELECT CASE ( nn )
             CASE ( 1 )
                dl_anz           = dl_anz + 1
                dl_pos(dl_anz,1) = i_pos
             CASE ( 2 )
                dl_pos(dl_anz,2) = i_pos
                nn = 0
             END SELECT
          END IF
       END DO
    END IF
    ! 
    ! Ersetze eventuell auftretende Trennzeichen zwischen paarweise -----
    ! auftretenden Delimitern -------------------------------------------
    !
    IF ( PRESENT(delim) .AND. PRESENT(repla) ) THEN
       IF ( dl_anz > 0 ) THEN
          DO i=1,dl_anz
             IF ( ALL(dl_pos(i,:) > 0) ) THEN
                string(dl_pos(i,1):dl_pos(i,1)) = sign ! durch Trennzeichen ersetzen
                string(dl_pos(i,2):dl_pos(i,2)) = sign ! durch Trennzeichen ersetzen
                DO i_pos=dl_pos(i,1)+1,dl_pos(i,2)-1
                   IF ( string(i_pos:i_pos) == sign ) string(i_pos:i_pos) = repla
                END DO
             END IF
          END DO
       END IF
    END IF
    !
    ! Anzahl an wirksamen Trennungen ermitteln
    !
    i_pos = 1
    !
    loop : DO
       !
       IF ( i_pos .GT. LEN_TRIM(string) ) EXIT loop
       !
       IF ( string(i_pos:i_pos) .EQ. sign ) THEN
          !
          ! Trennzeichen in String gefunden
          !
          IF (sign .EQ. ' ') THEN
             ! 
             ! Trennzeichen ist ein Blank ' '
             !
             IF ( tz_anz .GT. 0 ) THEN
                !
                IF ( tz_pos(tz_anz,2) .EQ.  i_pos - 1 ) THEN
                   !
                   ! Trennzeichen schliesst an vorheriges direkt an
                   !
                   tz_pos(tz_anz,2)  = i_pos
                   !
                ELSE
                   !
                   ! vorheriges Zeichen ist kein Trennzeichen
                   !
                   tz_anz            = tz_anz + 1
                   tz_pos(tz_anz,1)  = i_pos
                   tz_pos(tz_anz,2)  = i_pos
                   !
                END IF
                !
             ELSE
                !
                tz_anz            = tz_anz + 1
                tz_pos(tz_anz,1)  = i_pos
                tz_pos(tz_anz,2)  = i_pos
                !
             END IF
             !
          ELSE
             !
             ! Trennzeichen ist ein Zeichen ungleich dem Blank
             !
             tz_anz            = tz_anz + 1
             tz_pos(tz_anz,1)  = i_pos
             tz_pos(tz_anz,2)  = i_pos
             !
          END IF
          !
       END IF
       !
       i_pos = i_pos + 1
       !
    ENDDO loop
    !
    ! Allokieren des Feldes char%feld
    ! ( Anzahl Trennzeichen + 1 = Anzahl Listenelemente )
    !
    ALLOCATE ( liste(tz_anz + 1), STAT=stat )
    !
    IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
       !
       CALL setup_error_act &
            ( all_errors(:), -10000, c_upname, c_modname, stat )
       CALL setup_error_act ( '<felder>', 'liste' )
       !
       RETURN
       !
    END IF
    ! 
    ! Initialisieren des Feldes Liste
    !
    DO i = 1, SIZE(liste)
       liste(i)    = REPEAT(' ',LEN(liste(i)))
    END DO
    !
    ! Listenelemente aus String nach Feld liste uebertragen
    !
    ! String mit Zeichenkettenlaenge basteln (fuer evtl. Fehlermeldung)
    c_listlen  = REPEAT(' ',LEN(c_listlen))
    WRITE(c_listlen,'(I4)') LEN(liste(1))
    c_listlen  = ADJUSTL(c_listlen)
    !
    DO i = 1, tz_anz          ! ueber alle Trennzeichen
       !       
       IF ( i .EQ. 1 ) THEN
          !
          IF ( tz_pos(i,1) .EQ. 1 ) THEN
             ! Fall kann fuer SIGN = ' ' nicht eintreten
             i_anf = 0
             i_end = 0
          ELSE
             i_anf = 1
             i_end = tz_pos(i,1) -1
          END IF
          !
       ELSE
          !
          IF ( tz_pos(i,1)-1 .EQ. tz_pos(i-1,2) ) THEN
             ! Fall kann fuer SIGN = ' ' nicht eintreten
             i_anf = 0
             i_end = 0
          ELSE
             i_anf = tz_pos(i-1,2) + 1
             i_end = tz_pos(i  ,1) - 1
          END IF
          !
       END IF
       !
       IF ( i_end - i_anf + 1 .GT. LEN(liste(i)) ) THEN
          !
          ! Fehler -1560 : Liste enthaelt ein Element mit unzulaessig langer Zeichenkette !
          !
          CALL setup_error_act ( all_errors(:), -1560, c_upname, c_modname )
          CALL setup_error_act ( '<c_listlen>', TRIM(c_listlen) )
          CALL setup_error_act ( '<string>', TRIM(string) )
          !
          RETURN
          !
       ELSE
          !  
          IF ( i_anf .NE. 0 .AND. i_end .NE. 0 ) THEN
             liste(i)(1:i_end-i_anf+1) = string(i_anf:i_end)
          END IF
          !
       END IF
       !
    END DO ! i = 1, tz_anz
    !
    ! letztes Listenelement
    !
    IF ( tz_anz .EQ. 0 ) THEN
       !
       ! kein Trennzeichen gefunden
       ! Fall kann fuer sign = ' ' nicht eintreten
       !
       i_anf = 1
       i_end = LEN_TRIM(string)
       !
    ELSE IF ( tz_pos(tz_anz,2) .EQ. LEN_TRIM(string) ) THEN
       !
       ! Fall kann fuer sign = ' ' nicht eintreten
       !
       i_anf = 0
       i_end = 0
       !        
    ELSE
       !
       i_anf = tz_pos(tz_anz,2) + 1
       i_end = LEN_TRIM(string)
       !
    END IF
    !
    IF ( i_end - i_anf + 1 .GT. LEN(liste(tz_anz + 1)) ) THEN
       !
       ! Fehler -1560 : Liste enthaelt ein Element mit unzulaessig langer Zeichenkette !
       !
       CALL setup_error_act ( all_errors(:), -1560, c_upname, c_modname )
       CALL setup_error_act ( '<c_listlen>', TRIM(c_listlen) )
       CALL setup_error_act ( '<string>', TRIM(string) )
       !
       RETURN
       !   
    ELSE
       !
       IF ( i_anf .NE. 0 .AND. i_end .NE. 0 ) THEN
          liste(tz_anz + 1)(1:i_end-i_anf+1) = string(i_anf:i_end)
       END IF
       !
    END IF
    !
    IF ( ALLOCATED(tz_pos) ) DEALLOCATE(tz_pos)
    IF ( ALLOCATED(dl_pos) ) DEALLOCATE(dl_pos)
    !
  END SUBROUTINE read_liste_char
  !
  !! Liest ein Feld von Integer-Zahlen aus einer
  !! Stringliste.
  !! Elemente sind durch ein anzugebendes Trennzeichen
  !! getrennt.<BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_liste_int &
       ( list_string, sign, int_liste )
    !
    ! Formalparameter
    !! String mit Liste der Elemente
    CHARACTER (LEN=*)                , INTENT(IN   )   :: list_string
    !! Trennzeichen
    CHARACTER (LEN=*)                , INTENT(IN   )   :: sign
    !! Feld mit Integer-Listenelementen
    INTEGER , DIMENSION(:)           , POINTER         :: int_liste
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='read_liste_int' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    CHARACTER (LEN=key_len), DIMENSION(:) , POINTER      :: char_liste
    INTEGER                                              :: i, j, ios
    !
       !
       ! Initialisierungen
       !
       NULLIFY(char_liste)
       ! 
       IF ( ASSOCIATED(int_liste) ) THEN
          !
          ! Fehler -1550 : Pointerfeld int_liste bereits associated !
          !
          CALL setup_error_act ( all_errors(:), -1550, c_upname, c_modname )
          CALL setup_error_act ( '<liste>', 'int_liste' )
          !
          RETURN
          !
       END IF
       !
       ! String in CharFeld aufsplitten
       !
       CALL read_liste_char ( list_string, sign, char_liste )
       !
       IF ( no_error( ) ) THEN
          !
          ! Allokieren des Feldes int_liste
          !
          ALLOCATE ( int_liste(SIZE(char_liste)),STAT =stat )
          !
          IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
             !
             CALL setup_error_act &
                   ( all_errors(:), -10000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<felder>', 'int_liste' )
             !
          END IF
          !
       END IF
       !
       IF ( no_error( ) ) THEN
          !
          ! Integer-Elemente auslesen
          !
          DO i = 1, SIZE(char_liste)
             !
             char_liste(i) = ADJUSTL(char_liste(i))
             !
             DO j = 1, LEN_TRIM(char_liste(i))
                !
                IF ( char_liste(i)(j:j) .EQ. ' ' ) THEN
                   !
                   ! Fehler -1570 : Ein Listenelement schliesst mind.  ein Leerzeichen ein !
                   !
                   CALL setup_error_act ( all_errors(:), -1570, c_upname, c_modname )
                   CALL setup_error_act ( '<dat_type>', 'INTEGER' )
                   CALL setup_error_act ( '<list_string>', TRIM(list_string) )
                   !
                   RETURN
                   !  
                END IF
                !
             END DO
             !
             READ( char_liste(i),*,IOSTAT=ios) int_liste(i)
             !
             IF ( ios .NE. 0 ) THEN
                !
                ! Fehler -1510 : Lesefehler INTEGER-Zahl
                !
                CALL setup_error_act ( all_errors(:), -1510, c_upname, c_modname, ios )
                CALL setup_error_act ( '<Datentyp>', 'INTEGER' )
                CALL setup_error_act ( '<list_string>', TRIM(list_string ) )
                !
                RETURN
                !
             END IF
             !
          END DO ! i = 1, SIZE(char_liste)
          !
          ! Deallocate des lokalen Feldes char_liste
          !
          DEALLOCATE ( char_liste, STAT=stat )
          !
          IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Deallocate
             !
             CALL setup_error_act &
                   ( all_errors(:), -20000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<felder>', 'char_liste' )
             !
          END IF
          !
          IF ( no_error( ) ) NULLIFY (char_liste)
          !
       END IF ! no_error( )
       !
    !
  END SUBROUTINE read_liste_int
  !
  !! Liest ein Feld von Real-Zahlen aus einer Stringliste.
  !! Elemente sind durch ein anzugebendes Trennzeichen
  !! getrennt.
  SUBROUTINE read_liste_real &
       ( list_string, sign, real_liste )
    !
    ! Formalparameter
    !! String mit Liste der Elemente
    CHARACTER (LEN=*)                 , INTENT(IN   )   :: list_string
    !! Trennzeichen
    CHARACTER (LEN=*)                 , INTENT(IN   )   :: sign
    !! Feld mit Real-Listenelementen
    REAL (KIND=Single) , DIMENSION(:) , POINTER         :: real_liste
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='read_liste_real' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    CHARACTER (LEN=key_len), DIMENSION(:) , POINTER      :: char_liste
    INTEGER                                              :: i, j, ios
    !
       !
       ! Initialisierungen
       !
       NULLIFY(char_liste)
       !
       IF ( ASSOCIATED(real_liste) ) THEN
          !
          ! Fehler -1550 : Pointerfeld real_liste bereits associated !
          !
          CALL setup_error_act ( all_errors(:), -1550, c_upname, c_modname )
          CALL setup_error_act ( '<liste>', 'real_liste' )
          !
          RETURN
          !
       END IF
       !
       ! String in CharFeld aufsplitten
       !
       CALL read_liste_char ( list_string, sign, char_liste )
       !
       IF ( no_error( ) ) THEN
          !
          ! Allokieren des Feldes real_liste
          !
          ALLOCATE ( real_liste(SIZE(char_liste)),STAT =stat )
          !
          IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
             !
             CALL setup_error_act &
                   ( all_errors(:), -10000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<felder>', 'real_liste' )
             !
          END IF
          !
       END IF
       !
       IF ( no_error( ) ) THEN
          !
          ! Real-Elemente auslesen
          !
          DO i = 1, SIZE(char_liste)
             !
             char_liste(i) = ADJUSTL(char_liste(i))
             !
             DO j = 1, LEN_TRIM(char_liste(i))
                !
                IF ( char_liste(i)(j:j) .EQ. ' ' ) THEN
                   !
                   ! Fehler -1570 : Ein Listenelement schliesst mind.  ein Leerzeichen ein !
                   !
                   CALL setup_error_act ( all_errors(:), -1570, c_upname, c_modname )
                   CALL setup_error_act ( '<dat_type>', 'REAL' )
                   CALL setup_error_act ( '<list_string>', TRIM(list_string) )
                   !
                   RETURN
                   !  
                END IF
                !
             END DO
             !
             READ( char_liste(i),*,IOSTAT=ios) real_liste(i)
             !
             IF ( ios .NE. 0 ) THEN
                !
                ! Fehler -1510 : Lesefehler REAL-Zahl
                !
                CALL setup_error_act ( all_errors(:), -1510, c_upname, c_modname, ios )
                CALL setup_error_act ( '<Datentyp>', 'REAL' )
                CALL setup_error_act ( '<list_string>', TRIM(list_string ) )
                !
                RETURN
                !
             END IF
             !
          END DO ! i = 1, SIZE(char_liste)
          !
          ! Deallocate des lokalen Feldes char_liste
          !
          DEALLOCATE ( char_liste, STAT=stat )
          !
          IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Deallocate
             !
             CALL setup_error_act &
                   ( all_errors(:), -20000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<felder>', 'char_liste' )
             !
          END IF
          !
          IF ( no_error( ) ) NULLIFY (char_liste)
          !
       END IF ! no_error( )
       !
    !
  END SUBROUTINE read_liste_real
  !
  !! Liest ein Feld von Real-Zahlen hoeherer Genauigkeit
  !! (KIND=Double) aus einer Stringliste.
  !! Elemente sind durch ein anzugebendes Trennzeichen
  !! getrennt.<BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_liste_doub &
       ( list_string, sign, doub_liste )
    !
    ! Formalparameter
    !! String mit Liste der Elemente
    CHARACTER (LEN=*)                 , INTENT(IN   )   :: list_string
    !! Trennzeichen
    CHARACTER (LEN=*)                 , INTENT(IN   )   :: sign
    !! Feld mit Real(Double)-Listenelementen
    REAL (KIND=Double) , DIMENSION(:) , POINTER         :: doub_liste
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='read_liste_doub' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    CHARACTER (LEN=key_len), DIMENSION(:) , POINTER      :: char_liste
    INTEGER                                              :: i, j, ios
    !
       !
       ! Initialisierungen
       !
       NULLIFY(char_liste)
       !
       IF ( ASSOCIATED(doub_liste) ) THEN
          !
          ! Fehler -1550 : Pointerfeld doub_liste bereits associated !
          !
          CALL setup_error_act ( all_errors(:), -1550, c_upname, c_modname )
          CALL setup_error_act ( '<liste>', 'doub_liste' )
          !
          RETURN
          !
       END IF
       !
       ! String in CharFeld aufsplitten
       !
       CALL read_liste_char ( list_string, sign, char_liste )
       !
       IF ( no_error( ) ) THEN
          !
          ! Allokieren des Feldes doub_liste
          !
          ALLOCATE ( doub_liste(SIZE(char_liste)),STAT=stat )
          !
          IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
             !
             CALL setup_error_act &
                   ( all_errors(:), -10000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<felder>', 'doub_liste' )
             !
          END IF
          !
       END IF
       !
       IF ( no_error( ) ) THEN
          !
          ! Real(Double)-Elemente auslesen
          !
          DO i = 1, SIZE(char_liste)
             !
             char_liste(i) = ADJUSTL(char_liste(i))
             !
             DO j = 1, LEN_TRIM(char_liste(i))
                !
                IF ( char_liste(i)(j:j) .EQ. ' ' ) THEN
                   !
                   ! Fehler -1570 : Ein Listenelement schliesst mind.  ein Leerzeichen ein !
                   !
                   CALL setup_error_act ( all_errors(:), -1570, c_upname, c_modname )
                   CALL setup_error_act ( '<dat_type>', 'REAL(Double)' )
                   CALL setup_error_act ( '<list_string>', TRIM(list_string) )
                   !
                   RETURN
                   !  
                END IF
                !
             END DO
             !
             READ( char_liste(i),*,IOSTAT=ios) doub_liste(i)
             !
             IF ( ios .NE. 0 ) THEN
                !
                ! Fehler -1510 : Lesefehler REAL(Double)-Zahl
                !
                CALL setup_error_act ( all_errors(:), -1510, c_upname, c_modname, ios )
                CALL setup_error_act ( '<Datentyp>', 'REAL(Double)' )
                CALL setup_error_act ( '<list_string>', TRIM(list_string) )
                !
                RETURN
                !
             END IF
             !
          END DO ! i = 1, SIZE(char_liste)
          !
          ! Deallocate des lokalen Feldes char_liste
          !
          DEALLOCATE ( char_liste, STAT=stat )
          !
          IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Deallocate
             !
             CALL setup_error_act &
                   ( all_errors(:), -20000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<felder>', 'char_liste' )
             !
          END IF
          !
          IF ( no_error( ) ) NULLIFY (char_liste)
          !
       END IF ! no_error( )
       !
    !
  END SUBROUTINE read_liste_doub
  !
  !! Liest ein Feld von logischen Groessen aus einer
  !! Stringliste.
  !! Elemente sind durch ein anzugebendes Trennzeichen
  !! getrennt.<BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE read_liste_log &
       ( list_string, sign, log_liste )
    !
    ! Formalparameter
    !! String mit Liste der Elemente
    CHARACTER (LEN=*)                , INTENT(IN   )   :: list_string
    !! Trennzeichen
    CHARACTER (LEN=*)                , INTENT(IN   )   :: sign
    !! Feld mit Logik-Listenelementen
    LOGICAL , DIMENSION(:)           , POINTER         :: log_liste
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='read_liste_log' 
    !! Statusvariable
    INTEGER :: stat ! 
    !
    CHARACTER (LEN=key_len), DIMENSION(:) , POINTER      :: char_liste
    INTEGER                                              :: i, j
    !
       !
       ! Initialisierungen
       !
       NULLIFY (char_liste)
       !
       IF ( ASSOCIATED(log_liste) ) THEN
          !
          ! Fehler -1550 : Pointerfeld log_liste bereits associated !
          !
          CALL setup_error_act ( all_errors(:), -1550, c_upname, c_modname )
          CALL setup_error_act ( '<liste>', 'log_liste' )
          !
          RETURN
          !
       END IF
       !
       ! String in CharFeld aufsplitten
       !
       CALL read_liste_char ( list_string, sign, char_liste )
       !
       IF ( no_error( ) ) THEN
          !
          ! Allokieren des Feldes log_liste
          !
          ALLOCATE ( log_liste(SIZE(char_liste)),STAT=stat )
          !
          IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
             !
             CALL setup_error_act &
                   ( all_errors(:), -10000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<felder>', 'log_liste' )
             !
          END IF
          !
       END IF
       !
       IF ( no_error( ) ) THEN
          !
          ! Logik Elemente auslesen
          !
          DO i = 1, SIZE(char_liste)

             char_liste(i) = ADJUSTL(char_liste(i))

             DO j = 1, LEN_TRIM(char_liste(i))

                IF ( char_liste(i)(j:j) .EQ. ' ' ) THEN
                   !
                   ! Fehler -1571 : Ein Listenelement schliesst mind.  ein Leerzeichen ein !
                   !
                   CALL setup_error_act ( all_errors(:), -1571, c_upname, c_modname )
                   CALL setup_error_act ( '<list_string>', TRIM(list_string) )
                   !
                   RETURN
                   !  
                END IF
                !
             END DO
             !
             SELECT CASE ( TRIM(char_liste(i)) )
             CASE('1','T','t','True','.True.','true','.true.','TRUE','.TRUE.')
                !
                log_liste(i) = .True.
                !
             CASE('0','F','f','False','.False.','false','.false.','FALSE','.FALSE.')
                !
                log_liste(i) = .False.
                !
             CASE DEFAULT
                !
                ! Fehler -1520 : Lesefehler LOGICAL-Groesse
                !
                CALL setup_error_act ( all_errors(:), -1520, c_upname, c_modname )
                CALL setup_error_act ( '<list_string>', TRIM(list_string) )
                !
                RETURN
                !
             END SELECT
             !
          END DO ! i = 1, SIZE(char_liste)
          !
          ! Deallocate des lokalen Feldes char_liste
          !
          DEALLOCATE ( char_liste, STAT=stat )
          !
          IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Deallocate
             !
             CALL setup_error_act &
                   ( all_errors(:), -20000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<felder>', 'char_liste' )
             !
          END IF
          !
          IF ( no_error( ) ) NULLIFY (char_liste)
          !
       END IF ! no_error( )
       !
    !
  END SUBROUTINE read_liste_log
  !
  !! Haengt an eine Stringliste ein weiteres Listenelement
  !! vom Typ Character dran. 
  !! Das Element wird zum vorangehenden durch das
  !! anzugebende Trennzeichen SIGN getrennt.<BR>
  !! Ausnahme : die Liste enthaelt noch kein Element <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE make_liste_char &
       ( char, sign, list_string, l_tolong )
    !
    ! Formalparameter
    !! Character-Element
    CHARACTER (LEN=*)                , INTENT(IN   )    :: char
    !! Trennzeichen, welches die Listenelemente trennt
    CHARACTER (LEN=*)                , INTENT(IN   )    :: sign

    !! Stringliste
    CHARACTER (LEN=*)                , INTENT(INOUT)    :: list_string
    !! True, wenn Liste zu lang fuer die Character-Variable LIST_STRING geworden ist<BR>
    !! False, wenn dem nicht so ist
    LOGICAL                          , INTENT(  OUT)    :: l_tolong
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='make_liste_char' 

    INTEGER                            :: i_add, i_anf, i_ende
    CHARACTER (LEN=LEN(char)+1)        :: char_loc
    !
       !
       IF ( LEN(sign) .GT. 1 ) THEN
          !
          ! Fehler -1530 : SIGN laenger als ein Zeichen !
          !
          CALL setup_error_act ( all_errors(:), -1530, c_upname, c_modname )
          !
          RETURN
          !
       END IF
       !
       IF ( no_error( ) ) THEN
          !
          l_tolong = .False.

          IF ( LEN_TRIM(list_string) .NE. 0 ) THEN
             i_add    = LEN_TRIM(char) + 1        ! Fuers Trennzeichen <sign>
             char_loc = sign//TRIM(char)
          ELSE
             i_add    = LEN_TRIM(char)
             char_loc = TRIM(char) 
          END IF

          i_anf   = LEN_TRIM(list_string) + 1

          IF ( i_anf+i_add-1 .GT. LEN(list_string) ) THEN
             l_tolong = .True.
          END IF

          IF ( i_anf  .LE.  LEN(list_string) ) THEN

             i_ende = MIN( i_anf+i_add-1 , LEN(list_string) )

             list_string(i_anf:i_ende) = char_loc(1:i_ende-i_anf+1)

          END IF
          !
       END IF ! no_error( )
       !
    !
  END SUBROUTINE make_liste_char
  !
  !! Haengt an eine Stringliste ein weiteres Listenelement
  !! vom Typ Integer dran. 
  !! Das Element wird zum vorangehenden durch das
  !! anzugebende Trennzeichen SIGN getrennt.<BR>
  !! Ausnahme : die Liste enthaelt noch kein Element <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE make_liste_int &
       ( int, sign, list_string, l_tolong )
    !
    ! Formalparameter
    !! Integer-Element
    INTEGER                      , INTENT(IN   )        :: int
    !! Trennzeichen welches die Listenelemente trennt
    CHARACTER (LEN=*)            , INTENT(IN   )        :: sign

    !! String mit Liste der Elemente
    CHARACTER (LEN=*)            , INTENT(  OUT)        :: list_string
    !! True, wenn Liste zu lang fuer die Character-Variable LIST_STRING geworden ist<BR>
    !! False, wenn dem nicht so ist
    LOGICAL                      , INTENT(  OUT)        :: l_tolong
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='make_liste_int' 

    CHARACTER(LEN=key_len)                    :: c_temp
    !
       !
       IF ( LEN(sign) .GT. 1 ) THEN
          !
          ! Fehler -1530 : SIGN laenger als ein Zeichen !
          !
          CALL setup_error_act ( all_errors(:), -1530, c_upname, c_modname )
          !
          RETURN
          !
       END IF
       !
       IF ( no_error( ) ) THEN
          !
          ! Integer-Groessen in Character-Variable schreiben
          !
          c_temp = REPEAT(' ', LEN(c_temp))
          WRITE(c_temp,*) int
          c_temp = ADJUSTL(c_temp)
          !
          ! Aus Char-Feld Stringliste bauen
          !
          CALl make_liste_char &
                ( TRIM(c_temp), sign, list_string, l_tolong )
          !
       END IF ! no_error( )
       !
    !
  END SUBROUTINE make_liste_int
  !
  !! Haengt an eine Stringliste ein weiteres Listenelement
  !! vom Typ REAL dran. 
  !! Das Element wird zum vorangehenden durch das
  !! anzugebende Trennzeichen SIGN getrennt.<BR>
  !! Ausnahme : die Liste enthaelt noch kein Element <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE make_liste_real &
       ( real, sign, list_string, l_tolong )
    !
    ! Formalparameter
    !! Real-Element
    REAL  (KIND=Single)          , INTENT(IN   )        :: real
    !! Trennzeichen welches die Listenelemente trennt
    CHARACTER (LEN=*)            , INTENT(IN   )        :: sign
    !! String mit Liste der Elemente
    CHARACTER (LEN=*)            , INTENT(  OUT)        :: list_string
    !! True, wenn Liste zu lang fuer die Character-Variable LIST_STRING geworden ist
    !! False, wenn dem nicht so ist
    LOGICAL                      , INTENT(  OUT)        :: l_tolong
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='make_liste_real' 

    CHARACTER(LEN=key_len)                    :: c_temp
    !
       !
       IF ( LEN(sign) .GT. 1 ) THEN
          !
          ! Fehler -1530 : SIGN laenger als ein Zeichen !
          !
          CALL setup_error_act ( all_errors(:), -1530, c_upname, c_modname )
          !
          RETURN
          !
       END IF
       !
       IF ( no_error( ) ) THEN
          !
          ! Real-Groessen in Character-Variable schreiben
          !
          c_temp = REPEAT(' ', LEN(c_temp))
          WRITE(c_temp,*) real
          c_temp = ADJUSTL(c_temp)
          !
          ! Aus Char-Feld Stringliste bauen
          !
          CALL make_liste_char &
                ( TRIM(c_temp), sign, list_string, l_tolong )
          !
       END IF ! no_error( )
       !
    !
  END SUBROUTINE make_liste_real
  !
  !! Haengt an eine Stringliste ein weiteres Listenelement
  !! vom Typ REAL(Double) dran. 
  !! Das Element wird zum vorangehenden durch das
  !! anzugebende Trennzeichen SIGN getrennt.<BR>
  !! Ausnahme : die Liste enthaelt noch kein Element <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE make_liste_doub &
       ( doub, sign, list_string, l_tolong )
    !
    ! Formalparameter
    !! Real(Double)-Element
    REAL  (KIND=Double)          , INTENT(IN   )        :: doub
    !! Trennzeichen welches die Listenelemente trennt
    CHARACTER (LEN=*)            , INTENT(IN   )        :: sign
    !! String mit Liste der Elemente
    CHARACTER (LEN=*)            , INTENT(  OUT)        :: list_string
    !! True, wenn Liste zu lang fuer die Character-Variable LIST_STRING geworden ist<BR>
    !! False, wenn dem nicht so ist
    LOGICAL                      , INTENT(  OUT)        :: l_tolong
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='make_liste_doub' 

    CHARACTER(LEN=key_len)                    :: c_temp
    !
       !
       IF ( LEN(sign) .GT. 1 ) THEN
          !
          ! Fehler -1530 : SIGN laenger als ein Zeichen !
          !
          CALL setup_error_act ( all_errors(:), -1530, c_upname, c_modname )
          !
          RETURN
          !
       END IF
       !
       IF ( no_error( ) ) THEN
          !
          ! Real(Double)-Groessen in Character-Variable schreiben
          !
          c_temp = REPEAT(' ', LEN(c_temp))
          WRITE(c_temp,*) doub
          c_temp = ADJUSTL(c_temp)
          !
          ! Aus Char-Feld Stringliste bauen
          !
          CALl make_liste_char &
                ( TRIM(c_temp), sign, list_string, l_tolong )
          !
       END IF ! no_error( )
       !
    !
  END SUBROUTINE make_liste_doub
  !
  !! Haengt an eine Stringliste ein weiteres Listenelement
  !! vom Typ LOGICAL dran. 
  !! Das Element wird zum vorangehenden durch das
  !! anzugebende Trennzeichen SIGN getrennt.<BR>
  !! Ausnahme : die Liste enthaelt noch kein Element <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE make_liste_log &
       ( log, sign, list_string, l_tolong )
    !
    ! Formalparameter
    !! Logical-Element
    LOGICAL                      , INTENT(IN   )        :: log
    !! Trennzeichen welches die Listenelemente trennt
    CHARACTER (LEN=*)            , INTENT(IN   )        :: sign
    !! String mit Liste der Elemente
    CHARACTER (LEN=*)            , INTENT(  OUT)        :: list_string
    !! True, wenn Liste zu lang fuer die Character-Variable LIST_STRING geworden ist<BR>
    !! False, wenn dem nicht so ist
    LOGICAL                      , INTENT(  OUT)        :: l_tolong
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='make_liste_log' 

    CHARACTER(LEN=key_len)                    :: c_temp
    !
       !
       IF ( LEN(sign) .GT. 1 ) THEN
          !
          ! Fehler -1530 : SIGN laenger als ein Zeichen !
          !
          CALL setup_error_act ( all_errors(:), -1530, c_upname, c_modname )
          !
          RETURN
          !
       END IF
       !
       IF ( no_error( ) ) THEN
          !
          !! Logical-Groessen in Character-Variable schreiben
          !
          c_temp = REPEAT(' ', LEN(c_temp))
          IF ( log ) THEN
             c_temp(1:4) = 'TRUE' 
          ELSE
             c_temp(1:5) = 'FALSE'
          END IF
          !
          ! Aus Char-Feld Stringliste bauen
          !
          CALl make_liste_char &
                ( TRIM(c_temp), sign, list_string, l_tolong )
          !
       END IF ! no_error( )
       !
    !
  END SUBROUTINE make_liste_log
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
  !
END MODULE m_stringliste
!  ----------------------------------------------------------------------------
!  Ende des Source-Codes des Moduls
!  ----------------------------------------------------------------------------

