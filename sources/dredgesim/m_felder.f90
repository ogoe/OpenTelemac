!! <H2>M_FELDER</h2>
!! @author Susanne Spohr
!! @version 1.3 vom 07/11/02, Quellcode: mod_m_felder.f90
!! <HR>
!! <one line to give the program's name and an idea of what it does> <BR>
!! <HR>
!! <H3>Copyright-Hinweis</H3>
!!                                                                   <BR>
!! Copyright (C) 2002 <A HREF="http://www.hamburg.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!!                                                                   <BR>
!! <HR>
!! <H3>Entwicklungsgeschichte des Modules</H3>
!! 01.01 : 2002-05-30 : Susanne Spohr : Re-Engineering des Moduls mod_felder.f90 der Library dictionary <BR>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Modulss</H3>
!! Uebertragen der FixValue-Feldangaben in die
!! vorgesehene <t_feld>-Komponente der Variablen vom 
!! Typ <t_par>.
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
!!                                                                  <BR>
!! <HR>
!! <H3>Fehlersituationen des Moduls</H3>
!!                                                                    <BR>
!! siehe hierzu die Fehlermeldungen in User Interface "p_dictionary_ui" 
!!
MODULE m_felder
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
       ! Typdefinitionen
       t_feld, &
       t_vl_kz , &
       ! Routinen
       dealloc_vl_kz
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
  !! ...
  INTERFACE transfer_felder
     MODULE PROCEDURE transfer_felder_d ! 
  END INTERFACE
  !
  ! [C.5] Zuweisungen
  !
  ! [C.6] Operatoren (optional, falls sinnvoll)
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  PUBLIC :: transfer_felder               ! ...
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
  CHARACTER (LEN=31), PARAMETER :: c_modname      = 'm_felder' ! 
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
  !! Uebertragen der FixValue-Feldangaben aus der
  !! uebergebenen verketteten Liste in die
  !! vorgesehene <t_feld>-Komponente der Variablen vom 
  !! Typ <t_par>. <BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE transfer_felder_d &
       ( act_kz, fst_kz, lst_kz, vgl_kz, block, blockname, &
        DatType, FixValue )
    !
    ! USE-Statements
    !
    USE m_stdat_types, ONLY : &
         ! Typdefinitionen
         t_block
    !
    ! Formalparameter
    !! aktuelles ELement der verketteten Liste aus Keyzeilen
    !! mit Fixvalue-Angaben
    TYPE (t_vl_kz)                   , POINTER        :: act_kz
    !! 1. ELement der v.L. aus Keyzeilen mit Fixvalues
    TYPE (t_vl_kz)                   , POINTER        :: fst_kz
    !! letztes ELement der v.L. aus Keyzeilen mit Fixvalues
    TYPE (t_vl_kz)                   , POINTER        :: lst_kz
    !! Vgl.-ELement der v.L. aus Keyzeilen mit Fixvalues
    TYPE (t_vl_kz)                   , POINTER        :: vgl_kz
    !! Informationen zum aktuellen Parameterblock
    TYPE (t_block  )                 , INTENT(IN   )  :: block
    !! Name des Parameterblockes
    CHARACTER (LEN=*)                , INTENT(IN   )  :: blockname
    !! Datentyp des Parameters
    CHARACTER (LEN=*)                , INTENT(IN   )  :: DatType
    !! Variable zum Abspeichern der FixValue-Werte
    TYPE (t_feld)                                     :: FixValue
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='transfer_felder_d'
    INTEGER                            :: i
    !
       !
       ! Check : Enthaelt der Block eine Zeile mit FixValue-Angaben doppelt ?
       !
       act_kz => fst_kz
       !
       outer: DO
          !
          IF ( .NOT. ASSOCIATED(act_kz) ) EXIT outer
          !
          vgl_kz => act_kz%next
          !
          inner: DO 
             !
             IF ( .NOT. ASSOCIATED(vgl_kz) ) EXIT inner
             !
             IF ( TRIM(act_kz%key ) .EQ. TRIM(vgl_kz%key ) .AND. &
                  TRIM(act_kz%wert) .EQ. TRIM(vgl_kz%wert)        ) THEN
                !
                ! Fehler -1700 : Der Block eine Zeile mit FixValue-Angaben doppelt !
                !
                CALL setup_error_act ( all_errors(:), -1700, c_upname, c_modname )
                CALL setup_error_act ( '<blockname>', TRIM(blockname) )
                CALL setup_error_act ( '<key>', TRIM(act_kz%key) )
                CALL setup_error_act ( '<wert>', TRIM(act_kz%wert) )
                !
                RETURN
                ! 
             END IF
             !  
             vgl_kz => vgl_kz%next
             !
          END DO inner
          !
          act_kz => act_kz%next
          !
       END DO outer
       !
       !   Datentypabhaengiges dynamisches KomponentenFeld der Variablen
       !   vom Typ <t_feld> allokieren und Daten aus der Einleseliste uebertragen
       !
       DO i = 1, SIZE(block%Key)
          !
          SELECT CASE (TRIM(block%Key(i)%Name))
          CASE('FixValue')
             !
             IF ( block%Key(i)%ZeilAnz(1) .GT. 0  ) THEN
                !
                CALL alloc_t_feld &
                     ( act_kz, fst_kz, lst_kz, DatType, &
                     FixValue )
                !
                IF ( any_error( ) ) RETURN
                !
             END IF
             !
          END SELECT
          !
       END DO
       !
       ! Verkettete Liste mit Feldangaben loeschen
       !
       CALL dealloc_vl_kz ( act_kz, fst_kz, lst_kz, vgl_kz )
       !
    !
  END SUBROUTINE transfer_felder_d
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
  ! >>> modulspezifische PRIVATE-Methoden <<< [ERR_NO < 0]
  ! ----------------------------------------------------------------------
  !
  !! Allokiert ein Feld vom Typ t_feld und traegt die in der 
  !! verketteten Liste uebergebenen Werte in
  !! das dem Datentyp entsprechende Komponentenfeld ein.<BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE alloc_t_feld &
       ( act_kz, fst_kz, lst_kz, dat_typ, feld )
    !
    ! USE-Statements
    !
    USE m_stdat_types, ONLY : &
         ! Typdefinitionen
         t_vl_feld,&
         ! Routinen
         init_feld,&
         dealloc_t_feld
    !
    USE m_stringliste, ONLY : &
         ! Routinen
         read_liste
    !
    USE m_lesehilfe, ONLY : &
         ! Routinen
         Time_Probelesen
    !
    ! Formalparameter
    !! aktuelles ELement der verketteten Liste aus Keyzeilen
    !! mit Fixvalue-Angaben
    TYPE (t_vl_kz)     , POINTER          :: act_kz
    !! 1. ELement der v.L. aus Keyzeilen mit Fixvalues
    TYPE (t_vl_kz)     , POINTER          :: fst_kz
    !! letztes ELement der v.L. aus Keyzeilen mit Fixvalues
    TYPE (t_vl_kz)     , POINTER          :: lst_kz
    !! Datentyp des Parameters
    CHARACTER (LEN=*)                     :: dat_typ
    !! Variable zum datentypabhaengigen Abspeichern eines
    !! Feldes
    TYPE (t_feld)                         :: feld
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='alloc_t_feld'
    !! Statusvariable
    INTEGER :: stat ! 
    INTEGER                                    :: i, anz
    !
    TYPE (t_vl_feld)        , POINTER          :: fst_feld, &
                                                  lst_feld, &
                                                  act_feld, &
                                                  vgl_feld
    !
       !
       NULLIFY(fst_feld)
       NULLIFY(lst_feld)
       NULLIFY(act_feld)
       NULLIFY(vgl_feld)
       !
       ! Aus der verketteten Liste mit den Charlisten werden
       ! nun die Feldelemente und deren Anzahl ermittelt
       !
       ! ... erstes Listenelement
       !
       anz = 0   
       act_kz => fst_kz
       !
       ! ... Schleife ueber alle Listenelemente
       !
       vlist_kz : DO
          !
          IF ( .NOT. ASSOCIATED(act_kz) ) EXIT vlist_kz
          !
          ! Allokieren und initialisieren des neuen Listenelementes
          !
          ALLOCATE ( act_feld, STAT=stat )
          !
          IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
             !
             CALL setup_error_act &
                   ( all_errors(:), -10000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<felder>', 'act_feld' )
             !
             RETURN
             !
          END IF
          !
          CALL init_feld ( act_feld%feld )
          !
          IF ( any_error( ) ) RETURN
          !
          ! die Listen-Pointer setzen 
          !
          NULLIFY(act_feld%next)
          !
          IF (.NOT. ASSOCIATED(fst_feld)) THEN
             fst_feld   => act_feld
             NULLIFY(act_feld%prev)
          ELSE
             act_feld%prev => lst_feld
             lst_feld%next => act_feld
          END IF
          !
          lst_feld      => act_feld
          !
          ! die Listenelemente der Keyzeile lesen und die Werte in das zum
          ! Datentyp gehoerige KomponentenFeld der <t_feld>-Variablen speichern
          !
          SELECT CASE (dat_typ)
          CASE ('INT')
             !
             CALL read_liste  &
                  ( TRIM(act_kz%wert), ' ', act_feld%feld%int )
             !
             IF ( any_error( ) ) RETURN
             !
             anz = anz + SIZE(act_feld%feld%int)
             !
          CASE ('REAL')
             !
             CALL read_liste  &
                  ( TRIM(act_kz%wert), ' ', act_feld%feld%real )
             !
             IF ( any_error( ) ) RETURN
             !
             anz = anz + SIZE(act_feld%feld%real)
             !
          CASE ('LOG')
             !
             CALL read_liste  &
                  ( TRIM(act_kz%wert), ' ', act_feld%feld%log )
             !
             IF ( any_error( ) ) RETURN
             !
             anz = anz + SIZE(act_feld%feld%log )
             !
          CASE ('CHAR','FILE','DATE','INCR')
             !
             CALL read_liste  &
                  ( TRIM(act_kz%wert), ' ', act_feld%feld%char )
             !
             IF ( any_error( ) ) RETURN
             !
             anz = anz + SIZE(act_feld%feld%char)
             !
             SELECT CASE (dat_typ)
             CASE ('DATE')
                !
                CALL Time_Probelesen &
                     ( 'DATE', act_feld%feld%char )
                !
                IF ( any_error( ) ) RETURN
                !
             CASE ('INCR')
                !
                CALL Time_Probelesen &
                     ( 'INCR', act_feld%feld%char )
                !
                IF ( any_error( ) ) RETURN
                !
             END SELECT
             !
          CASE ('DOUBLE')
             !
             CALL read_liste  &
                     ( TRIM(act_kz%wert), ' ', act_feld%feld%doub )
             !
             IF ( any_error( ) ) RETURN
             !
             anz = anz + SIZE(act_feld%feld%doub)
             !
          END SELECT
          !
          ! ... naechsten Listen-String initialisieren
          !
          lst_kz  => act_kz
          act_kz  => act_kz%next
          !
       END DO vlist_kz
       !
       ! Speicherfeld der Variablen vom Typ <t_feld> in Abhaengigkeit vom
       ! Datentyp allokieren
       !
       SELECT CASE (dat_typ)
       CASE ('INT')
          !
          ALLOCATE ( feld%int(anz), STAT=stat )
          !
          IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
             !
             CALL setup_error_act &
                   ( all_errors(:), -10000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<felder>', 'feld%int' )
             !
             RETURN
             !
          END IF
          !
       CASE ('REAL')
          !
          ALLOCATE ( feld%real(anz), STAT=stat )
          !
          IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
             !
             CALL setup_error_act &
                   ( all_errors(:), -10000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<felder>', 'feld%real' )
             !
             RETURN
             !
          END IF
          !
       CASE ('LOG')
          !
          ALLOCATE ( feld%log(anz), STAT=stat)
          !
          IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
             !
             CALL setup_error_act &
                   ( all_errors(:), -10000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<felder>', 'feld%log' )
             !
             RETURN
             !
          END IF
          !
       CASE ('CHAR','DATE','INCR','FILE')
          !
          ALLOCATE ( feld%char(anz), STAT=stat )
          !
          IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
             !
             CALL setup_error_act &
                   ( all_errors(:), -10000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<felder>', 'feld%char' )
             !
             RETURN
             !
          END IF
          !
          DO i = 1, SIZE(feld%char)
             feld%char(i) = REPEAT(' ',LEN(feld%char(i)))
          END DO
          !
       CASE ('DOUBLE')
          !
          ALLOCATE ( feld%doub(anz), STAT=stat )
          !
          IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
             !
             CALL setup_error_act &
                   ( all_errors(:), -10000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<felder>', 'feld%doub' )
             !
             RETURN
             !
          END IF
          !
       END SELECT
       !
       ! Elemente der verketteten Liste mit den Feldelementen werden nun in die
       ! dynamischen Feld feld uebertragen
       !
       !      ... erstes Listenelement
       !
       act_feld => fst_feld
       anz = 0   
       !
       ! ... Schleife ueber alle Listenelemente
       !
       vlist_feld : DO
          !
          IF ( .NOT. ASSOCIATED(act_feld) ) EXIT vlist_feld
          !
          ! ... nacheinander in das Feld <feld> einsortieren
          !
          SELECT CASE (dat_typ)
          CASE('INT')
             !
             feld%int(anz+1 : anz + SIZE(act_feld%feld%int)) = &
                  act_feld%feld%int
             !
             anz = anz + SIZE(act_feld%feld%int)
             !
          CASE('REAL')
             !
             feld%real(anz+1 : anz + SIZE(act_feld%feld%real)) = &
                  act_feld%feld%real
             !
             anz = anz + SIZE(act_feld%feld%real)
             !
          CASE('LOG')
             !
             feld%log(anz+1 : anz + SIZE(act_feld%feld%log)) = &
                  act_feld%feld%log
             !
             anz = anz + SIZE(act_feld%feld%log)
             !
          CASE('CHAR','DATE','INCR','FILE')
             !
             feld%char(anz+1 : anz + SIZE(act_feld%feld%char)) = &
                  act_feld%feld%char
             anz = anz + SIZE(act_feld%feld%char)
             !
          CASE('DOUBLE')
             !
             feld%doub(anz+1 : anz + SIZE(act_feld%feld%doub)) = &
                  act_feld%feld%doub
             !
             anz = anz + SIZE(act_feld%feld%doub)
             !
          END SELECT
          !
          ! ... naechstes Teilfeld initialisieren
          !
          lst_feld  => act_feld
          act_feld  => act_feld%next
          !
          ! ... Deallokieren des wegsortierten Listenelementes
          !
          CALL dealloc_t_feld &
               ( lst_feld%feld )
          !
          IF ( any_error( ) ) RETURN
          !
          NULLIFY (lst_feld%prev)
          NULLIFY (lst_feld%next)
          !
          DEALLOCATE (lst_feld)
          !  
       END DO vlist_feld
       !
    !
  END SUBROUTINE alloc_t_feld
  !
END MODULE m_felder
!  ----------------------------------------------------------------------------
!  Ende des Source-Codes des Moduls
!  ----------------------------------------------------------------------------

