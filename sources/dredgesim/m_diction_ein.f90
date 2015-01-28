!! <H2>M_DICTION_EIN</h2>
!! @author Susanne Spohr
!! @version 1.4 vom 07/11/02, Quellcode: mod_m_diction_ein.f90
!! <HR>
!! read dictionary file <BR>
!! <HR>
!! <H3>Copyright-Hinweis</H3>
!!                                                                   <BR>
!! Copyright (C) 2002 <A HREF="http://www.hamburg.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!!                                                                   <BR>
!! <HR>
!! <H3>Entwicklungsgeschichte des Modules</H3>
!! 01.01 : 2002-05-27 : Susanne Spohr : Re-Engineering des Moduls mod_diction_ein.f90 der Library dictionary <BR>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Modulss</H3>
!! Lesen der Dictionary-Datei einer Steuerdatei                     <BR>
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
MODULE m_diction_ein
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
  ! [A.2] BASIS-Modul Datei-Handling
  !
  USE b_file, ONLY :  &
       ! Typdefinition
       t_file
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
       all_errors,  & ! Fehlermeldungen
       dicfile_name   ! Name der Dictionary-Datei
  !
  ! [B.2] weitere Module die zum Package "dictionary" gehoeren
  !
  USE m_lesehilfe, ONLY : &
       ! Routinen
       readkarte, &
       readblockname, &
       read_logical, &
       read_keyzeile
  !
  USE m_stdat_types, ONLY : &
       !   Parameter 
       key_len, & 
       line_len, &
       ! Typdefinitionen
       t_block, &
       t_key   , &
       t_par   , &
       t_vl_kz , &
       t_vl_block , &
       t_vl_key   , &
       t_vl_par   , &
       ! Routinen
       birth_vl_block , &
       birth_vl_key   , &
       birth_vl_par   , &
       birth_vl_kz    , &
       init_block     , &    
       init_key       , &
       dealloc_t_block
  !
  USE m_dic_checks, ONLY : &
       ! Routinen
       transfer_CKs
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
  !! Lesen der Dictionary-Datei
  INTERFACE read_dictionary
     MODULE PROCEDURE read_dictionary_d ! 
  END INTERFACE
  !
  !! ...
  INTERFACE write_dictionary
     MODULE PROCEDURE write_dictionary_d ! 
  END INTERFACE
  !
  ! [C.4.1] erforderliche oeffentliche Schnittstellen
  !
  ! [C.4.2] optionale oeffentliche Schnittstellen
  !
  ! [C.5] Zuweisungen
  !
  ! ... ggf. ergaenzen
  !
  ! [C.6] Operatoren (optional, falls sinnvoll)
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  PUBLIC :: read_dictionary       ! Lesen der Dictionary-Datei
  PUBLIC :: write_dictionary      ! Zu Testzwecken : schreibe Inhalt Dico-Dat
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
  CHARACTER (LEN=31), PARAMETER :: c_modname      = 'm_diction_ein' ! 
  !! ....
  LOGICAL           , PARAMETER :: l_wri = .False.
  !
  ! [D.3] Variablen (statische Daten des Moduls)
  !
  ! Verschiedene verkettete Listen zum dynamischen Einlesen der Dictionary-Datei
  !
  !! Liste mit PARAMETER-Block-Beschreibungen
  TYPE (t_vl_par)   , POINTER   :: fst_par, lst_par, act_par, vgl_par
  !! Liste mit KEY-Block-Beschreibungen
  TYPE (t_vl_key)   , POINTER   :: fst_key, lst_key, act_key, vgl_key
  !! Liste mit BLOCK-Block-Beschreibungen
  TYPE (t_vl_block) , POINTER   :: fst_block, lst_block, act_block, vgl_block
  INTEGER                       :: block_anz
  !! Listen mit Keyzeilen zu Check-Anforderungen und Feldangaben
  TYPE (t_vl_kz)   , POINTER    :: fst_ckB, lst_ckB, act_ckB, vgl_ckB, &
                                   fst_ckK, lst_ckK, act_ckK, vgl_ckK, &
                                   fst_ckP, lst_ckP, act_ckP, vgl_ckP, &
                                   fst_fd, lst_fd, act_fd, vgl_fd
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
  !! Liest Dictionary-Datei eines Programmes<BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE read_dictionary_d &
       ( dicdat, bloecke )
    !
    ! USE-Statements
    !
    ! BASIS-Modul fuer das File-Handling
    !
    USE b_file, ONLY :  &
       ! Routinen
       open_file, &
       close_file
    !
    ! Package-Modul
    !
    USE m_dic_checks, ONLY : &
         ! Routinen
         Checks_Pruefen
    !
    ! Formalparameter
    !! Dictionary-Datei
    TYPE (t_file )                  , INTENT(INOUT)   :: dicdat
    !! Feld mit Beschreibungen der einzelnen Eingabebloecke
    TYPE (t_block  ) , DIMENSION(:) , POINTER         :: bloecke
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='read_dictionary_d'
    !! Hilfsstring fuer Fehlertext
    CHARACTER (LEN=6)                :: c_actlen
    !
    CHARACTER (LEN=line_len)         :: blockname
    CHARACTER (LEN=line_len)         :: karte

    LOGICAL                          :: doit_file
    LOGICAL                          :: doit_block
    !
    !
    ! Initialisierungen
    !
    block_anz = 0
    !
    NULLIFY (fst_block)
    NULLIFY (act_block)
    NULLIFY (lst_block)
    NULLIFY (vgl_block)
    !
    ! Oeffnen der programmspezifischen Dictionary-Datei
    CALL open_file( dicdat )
    !
    !
    ! Lesen der Dictionary-Datei
    !
    doit_file  = .True.
    !
    file: DO
       !
       IF ( any_error( )  .OR.  .NOT. doit_file ) EXIT file
       !
       doit_block = .true.
       !
       ! Lesen von ENDFILE oder BEGINDATA <blockname>
       !
       karte = REPEAT(' ',LEN(karte))
       !
       CALL readkarte &
            ( dicdat, 'Lese Dictionary BEGINDATA/ENDFILE', karte )
       !
       IF ( no_error( ) ) karte = ADJUSTL(karte)
       !
       ! ... wenn keine Leerzeile
       !
       IF ( no_error( )  .AND.  LEN_TRIM(karte) .GT. 0 ) THEN
          !
          CALL readblockname ( karte, doit_file, blockname )
          !
          ! Lesen der blockspezifischen Daten
          !
          IF ( no_error( )  .AND.  doit_file ) THEN
             !
             SELECT CASE ( blockname(1:5) )
                !
             CASE ('BLOCK')
                !
                IF ( LEN_TRIM(blockname) .GT. 5 ) THEN
                   !
                   ! Gebaeren eines Listenelementes vom Typ <t_vl_block>
                   ! inkl. Initialisierung
                   !
                   CALL birth_vl_block &
                        ( act_block, fst_block, lst_block )
                   !
                   IF ( no_error( ) ) THEN
                      !
                      ! Zaehlen der Dictionary-Blockbeschreibung
                      !
                      block_anz = block_anz + 1
                      !
                      ! Blocknamen eintragen
                      !
                      blockname = ADJUSTL(blockname(6:))
                      !
                      IF (LEN_TRIM(blockname) .GT. LEN(act_block%block%Name)) THEN
                         !
                         ! Fehler -530 : Stringlaenge des Namens einer BLOCK-Blockstruktur ist
                         !             zu lang !
                         !
                         c_actlen = REPEAT(' ',LEN(c_actlen))
                         WRITE(c_actlen,'(I4)') LEN(act_block%block%Name)
                         !
                         CALL setup_error_act ( all_errors(:), -530, c_upname, c_modname )
                         CALL setup_error_act ( '<file_name>', TRIM( dicfile_name ) )
                         CALL setup_error_act ( '<len_block_name>', TRIM(c_actlen) )
                         CALL setup_error_act ( '<blockname>', TRIM(blockname) )
                         !
                         RETURN
                         !
                      ELSE
                         !
                         act_block%block%Name = blockname(1:LEN_TRIM(blockname))
                         !
                      END IF
                      !
                   END IF ! no_error( )
                   !
                   ! Lesen eines Dictionary-Blockes 
                   !
                   IF ( no_error( ) ) CALL read_dicblock ( dicdat )
                   !
                ELSE
                   !
                   ! Fehler -520 : Die Dictionary-Datei enthaelt eine Eingabeblock-Beschreibung
                   !              ohne Blocknamen !
                   !
                   CALL setup_error_act ( all_errors(:), -520, c_upname, c_modname )
                   CALL setup_error_act ( '<file_name>', TRIM( dicfile_name ) )
                   !
                   RETURN
                   !
                END IF
                !
             CASE DEFAULT
                !
                ! Fehler -510 : Ungueltige Blockdata-Struktur in Dictionary-Datei !
                !
                CALL setup_error_act ( all_errors(:), -510, c_upname, c_modname )
                CALL setup_error_act ( '<file_name>', TRIM( dicfile_name ) )
                CALL setup_error_act ( '<akt_blockname>', TRIM(blockname) )
                !
                RETURN
                !
             END SELECT
             !
          END IF ! ( no_error( ) )  .AND.  doit_file = true )
          !
       END IF ! ( no_error( ) )  .AND.  LEN(TRIM(karte)) .GT. 0 )
       !
    END DO file
    !
    ! Elemente der verketteten Liste vom Typ t_vl_block werden nun in das
    ! Feld vom Typ <t_block> uebertragen
    ! Das UP transfer_BLOCKs :
    !     - checkt ob mind. 1 Block spezifiziert wurde
    !     - kein Block doppelt, d.h. gleicher Name spezifiziert wurde
    !     - allokiert den Speicherplatz (Feld:bloecke)
    !     - transferiert die Daten und gibt den Speicherplatz
    !       fuer die Elemente der verketteten Liste <t_vl_block> wieder frei
    !
    IF ( no_error( ) ) CALL transfer_BLOCKs ( bloecke )
    !
    ! Saemtliche Check-Anforderungen (ReqIf,NotIf,CheckIfPar) auf 
    ! Plausibilitaet pruefen :
    !    - Vergleichsobjekte existent und zulaessig ?
    !    - Vergleichswerte mit gefordertem Datentyp uebereinstimmend ?
    !    - Vergleichs-Operation fuer Datentyp machbar ?
    !    - etc.
    !
    IF ( no_error( ) ) CALL Checks_Pruefen ( bloecke )
    !
    ! Schliessen der Datei mit den Eingabesteuerdaten
    IF ( no_error( ) ) CALL close_file (dicdat)
    !
  END SUBROUTINE read_dictionary_d
  !
  !! Gibt die aus der Dictionary-Datei eines Programmes
  !! gelesenen Angaben auf den Bildschirm aus.
  !! Nuetzlich zu Testzwecken !
  SUBROUTINE write_dictionary_d &
       ( bloecke )
    !
    ! Formalparameter
    !! Feld mit Beschreibungen der einzelnen Eingabebloecke
    TYPE (t_block  ) , DIMENSION(:) , POINTER         :: bloecke
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='write_dictionary_d'
    INTEGER                          :: i, j, k, l
    !
    ! Schreiben der Dictionary-Datei
    !
    DO i = 1,size(bloecke)
       write(*,*) ' *** BLOCK ***'
       write(*,*) ' BLOCKNAME = ', bloecke(i)%Name
       write(*,*) ' L_Opt     = ', bloecke(i)%L_Opt
       !
       IF ( ASSOCIATED(bloecke(i)%ReqIf) ) THEN
          do j = 1, SIZE (bloecke(i)%ReqIf)
             write(*,*) ' ReqIf%oper          = ', bloecke(i)%ReqIf(j)%oper
             write(*,*) ' ReqIf%objekt%typ    = ', &
                  bloecke(i)%ReqIf(j)%objekt%typ
             write(*,*) ' ReqIf%objekt%block  = ', &
                  bloecke(i)%ReqIf(j)%objekt%block
             write(*,*) ' ReqIf%objekt%key    = ', &
                  bloecke(i)%ReqIf(j)%objekt%key
             write(*,*) ' ReqIf%objekt%par_nr = ', &
                  bloecke(i)%ReqIf(j)%objekt%par_nr
          end do
       END IF
       !
       IF ( ASSOCIATED(bloecke(i)%NotIf) ) THEN
          do j = 1, SIZE (bloecke(i)%NotIf)
             write(*,*) ' NotIf%oper          = ', bloecke(i)%NotIf(j)%oper
             write(*,*) ' NotIf%objekt%typ    = ', &
                  bloecke(i)%NotIf(j)%objekt%typ
             write(*,*) ' NotIf%objekt%block  = ', &
                  bloecke(i)%NotIf(j)%objekt%block
             write(*,*) ' NotIf%objekt%key    = ', &
                  bloecke(i)%NotIf(j)%objekt%key
             write(*,*) ' NotIf%objekt%par_nr = ', &
                  bloecke(i)%NotIf(j)%objekt%par_nr
          end do
       END IF
       !
       do j = 1, Size (bloecke(i)%Key)
          write(*,*) '     *** KEY ***'
          write(*,*) '     KEYNAME   = ', bloecke(i)%Key(j)%Name
          write(*,*) '     L_Opt     = ', bloecke(i)%Key(j)%L_Opt
          write(*,*) '     L_Single  = ', bloecke(i)%Key(j)%L_Single
          IF ( ASSOCIATED(bloecke(i)%Key(j)%ReqIf) ) THEN
             do k = 1, SIZE (bloecke(i)%Key(j)%ReqIf)
             write(*,*) '      ReqIf%oper          = ', &
                  bloecke(i)%Key(j)%ReqIf(k)%oper
             write(*,*) '      ReqIf%objekt%typ    = ', &
                  bloecke(i)%Key(j)%ReqIf(k)%objekt%typ
             write(*,*) '      ReqIf%objekt%block  = ', &
                  bloecke(i)%Key(j)%ReqIf(k)%objekt%block
             write(*,*) '      ReqIf%objekt%key    = ', &
                  bloecke(i)%Key(j)%ReqIf(k)%objekt%key
             write(*,*) '      ReqIf%objekt%par_nr = ', &
                  bloecke(i)%Key(j)%ReqIf(k)%objekt%par_nr
             end do
          END IF
          !
          IF ( ASSOCIATED(bloecke(i)%Key(j)%NotIf) ) THEN
             do k = 1, SIZE (bloecke(i)%Key(j)%NotIf)
             write(*,*) '      NotIf%oper          = ', &
                  bloecke(i)%Key(j)%NotIf(k)%oper
             write(*,*) '      NotIf%objekt%typ    = ', &
                  bloecke(i)%Key(j)%NotIf(k)%objekt%typ
             write(*,*) '      NotIf%objekt%block  = ', &
                  bloecke(i)%Key(j)%NotIf(k)%objekt%block
             write(*,*) '      NotIf%objekt%key    = ', &
                  bloecke(i)%Key(j)%NotIf(k)%objekt%key
             write(*,*) '      NotIf%objekt%par_nr = ', &
                  bloecke(i)%Key(j)%NotIf(k)%objekt%par_nr
             end do
          END IF
          !
          do k = 1, SIZE (bloecke(i)%Key(j)%Par)
             write(*,*) '          *** PAR ***'
             write(*,*) '          ParPos      = ', &
                        bloecke(i)%Key(j)%Par(k)%ParPos
             write(*,*) '          Type        = ', &
                        bloecke(i)%Key(j)%Par(k)%Type
             write(*,*) '          L_Opt       = ', &
                  bloecke(i)%Key(j)%Par(k)%L_Opt
             IF ( ASSOCIATED(bloecke(i)%Key(j)%Par(k)%ReqIf) ) THEN
                do l = 1, SIZE (bloecke(i)%Key(j)%Par(k)%ReqIf)
             write(*,*) '           ReqIf%oper          = ', &
                  bloecke(i)%Key(j)%Par(k)%ReqIf(l)%oper
             write(*,*) '           ReqIf%objekt%typ    = ', &
                  bloecke(i)%Key(j)%Par(k)%ReqIf(l)%objekt%typ
             write(*,*) '           ReqIf%objekt%block  = ', &
                  bloecke(i)%Key(j)%Par(k)%ReqIf(l)%objekt%block
             write(*,*) '           ReqIf%objekt%key    = ', &
                  bloecke(i)%Key(j)%Par(k)%ReqIf(l)%objekt%key
             write(*,*) '           ReqIf%objekt%par_nr = ', &
                  bloecke(i)%Key(j)%Par(k)%ReqIf(l)%objekt%par_nr
                end do
             END IF
             !
             IF ( ASSOCIATED(bloecke(i)%Key(j)%Par(k)%NotIf) ) THEN
                do l = 1, SIZE (bloecke(i)%Key(j)%Par(k)%NotIf)
             write(*,*) '           NotIf%oper          = ', &
                  bloecke(i)%Key(j)%Par(k)%NotIf(l)%oper
             write(*,*) '           NotIf%objekt%typ    = ', &
                  bloecke(i)%Key(j)%Par(k)%NotIf(l)%objekt%typ
             write(*,*) '           NotIf%objekt%block  = ', &
                  bloecke(i)%Key(j)%Par(k)%NotIf(l)%objekt%block
             write(*,*) '           NotIf%objekt%key    = ', &
                  bloecke(i)%Key(j)%Par(k)%NotIf(l)%objekt%key
             write(*,*) '           NotIf%objekt%par_nr = ', &
                  bloecke(i)%Key(j)%Par(k)%NotIf(l)%objekt%par_nr
                end do
             END IF
             !
             IF ( ASSOCIATED (bloecke(i)%Key(j)%Par(k)%CheckIf) ) THEN
                do l = 1, SIZE (bloecke(i)%Key(j)%Par(k)%CheckIf)
             write(*,*) '           CheckIf%oper          = ', &
                  bloecke(i)%Key(j)%Par(k)%CheckIf(l)%oper
             write(*,*) '           CheckIf%objekt%typ    = ', &
                  bloecke(i)%Key(j)%Par(k)%CheckIf(l)%objekt%typ
             write(*,*) '           CheckIf%objekt%block  = ', &
                  bloecke(i)%Key(j)%Par(k)%CheckIf(l)%objekt%block
             write(*,*) '           CheckIf%objekt%key    = ', &
                  bloecke(i)%Key(j)%Par(k)%CheckIf(l)%objekt%key
             write(*,*) '           CheckIf%objekt%par_nr = ', &
                  bloecke(i)%Key(j)%Par(k)%CheckIf(l)%objekt%par_nr
                end do
             END IF
             !
             write(*,*) '          L_Array     = ', &
                        bloecke(i)%Key(j)%Par(k)%L_Array
             write(*,*) '          L_FilReq    = ', &
                        bloecke(i)%Key(j)%Par(k)%L_FilReq
             write(*,*) '          L_FilNew    = ', &
                        bloecke(i)%Key(j)%Par(k)%L_FilNew
             !
             IF ( ASSOCIATED(bloecke(i)%Key(j)%Par(k)%FixValue%int) ) THEN
                write(*,*) '          FixValue INT = '
                write(*,*) bloecke(i)%Key(j)%Par(k)%FixValue%int
             end if
             !
             IF ( ASSOCIATED(bloecke(i)%Key(j)%Par(k)%FixValue%real) ) THEN
                write(*,*) '          FixValue REAL= '
                write(*,*) bloecke(i)%Key(j)%Par(k)%FixValue%real
             end if
             !
             IF ( ASSOCIATED(bloecke(i)%Key(j)%Par(k)%FixValue%doub) ) THEN
                write(*,*) '          FixValue DOUBLE= '
                write(*,*) bloecke(i)%Key(j)%Par(k)%FixValue%doub
             end if
             !
             IF ( ASSOCIATED(bloecke(i)%Key(j)%Par(k)%FixValue%char) ) THEN
                write(*,*) '          FixValue CHAR= '
                DO l = 1, size(bloecke(i)%Key(j)%Par(k)%FixValue%char)
                   write(*,*) &
                       TRIM(ADJUSTL(bloecke(i)%Key(j)%Par(k)%FixValue%char(l)))
                end do
             end if
             !
             IF ( ASSOCIATED(bloecke(i)%Key(j)%Par(k)%FixValue%log) ) THEN
                write(*,*) '          FixValue LOG = '
                write(*,*) bloecke(i)%Key(j)%Par(k)%FixValue%log
             end if
             !
          end do
          !
       end do
       !
    end do
    !
  END SUBROUTINE write_dictionary_d
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
  !! ... <BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE transfer_BLOCKs &
       ( bloecke )
    !
    ! Formalparameter
    !! Feld mit Beschreibungen der einzelnen Eingabebloecke
    TYPE (t_block)    , DIMENSION(:) , POINTER        :: bloecke 
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='transfer_BLOCKs'
    !! Statusvariable
    INTEGER :: stat ! 
    !
    INTEGER                            :: nr
    !
    !
    IF ( block_anz .EQ. 0 ) THEN
       !
       ! Fehler -540 : In der Dictionary-Datei ist kein Eingabeblock spezifiziert !
       !
       CALL setup_error_act ( all_errors(:), -540, c_upname, c_modname )
       CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
       !
       RETURN
       !
    END IF
    !
    !
    ! Check : Wurde ein Eingabeblock mehrfach beschrieben ?
    !
    act_block => fst_block
    !
    outer: DO
       !
       IF ( .NOT. ASSOCIATED(act_block) ) EXIT outer
       !
       vgl_block => act_block%next
       !
       inner: DO 
          !
          IF ( .NOT. ASSOCIATED(vgl_block) ) EXIT inner
          !
          IF ( TRIM(act_block%block%Name) .EQ. &
               TRIM(vgl_block%block%Name)       ) THEN
             !
             ! Fehler -550 : In der Dictionary-Datei wurde ein Eingabeblock
             !               mehrfach beschrieben !
             !
             CALL setup_error_act ( all_errors(:), -550, c_upname, c_modname )
             CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
             CALL setup_error_act ( '<blockname>', TRIM(act_block%block%Name) )
             !
             RETURN
             !   
          END IF
          !
          vgl_block => vgl_block%next
          !
       END DO inner
       !
       act_block => act_block%next
       !
    END DO outer
    !
    ! ... Feld bloecke allokieren
    !
    ALLOCATE ( bloecke(block_anz), STAT=stat )
    !
    IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
       !
       CALL setup_error_act &
            ( all_errors(:), -10000, c_upname, c_modname, stat )
       CALL setup_error_act ( '<felder>', 'bloecke' )
       !
       RETURN
       !
    END IF
    ! 
    ! Elemente der verketteten Liste vom Typ t_vl_block werden nun in das
    ! Feld bloecke vom Typ <t_block> uebertragen
    !
    !   ... erstes Listenelement
    !
    act_block => fst_block
    !
    !   ... Schleife ueber alle Listenelemente
    !
    nr = 0
    vlist : DO
       !
       IF ( .NOT. ASSOCIATED(act_block) ) EXIT vlist
       !
       nr = nr + 1
       !
       bloecke(nr) = act_block%block
       !
       ! ... naechste Datei initialisieren
       !
       lst_block => act_block
       act_block => act_block%next
       !
       ! ... Deallokieren des wegsortierten Listenelementes
       !
       DEALLOCATE (lst_block)
       !
    END DO vlist
    !
    ! Pointerverbindungen loesen
    !
    NULLIFY (fst_block)
    NULLIFY (act_block)
    NULLIFY (lst_block)
    NULLIFY (vgl_block)
    !
  END SUBROUTINE transfer_BLOCKs
  !
  !! Liest BLOCK-Block der Dictionary-Datei. <BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE read_dicblock &
       ( dicdat )
    !
    ! Formalparameter
    !! Dictionary-Datei
    TYPE (t_file)    , INTENT(IN   )   :: dicdat
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='read_dicblock'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Hilfsstring fuer Fehlertext
    CHARACTER (LEN=6)                  :: c_actlen
    !
    CHARACTER (LEN=key_len)            :: key
    CHARACTER (LEN=line_len)           :: karte, wert, blockname
    !
    LOGICAL                            :: doit_block
    !
    TYPE(t_block)                      :: dic_bl
    INTEGER                            :: i
    !
    !
    ! Initialisierungen
    !
    NULLIFY (fst_key)
    NULLIFY (act_key)
    NULLIFY (lst_key)
    NULLIFY (vgl_key)
    !
    NULLIFY (fst_ckB)
    NULLIFY (act_ckB)
    NULLIFY (lst_ckB)
    NULLIFY (vgl_ckB)
    !
    ! Beschreibung der moeglichen Schluesselwoerter des BLOCK-Blockes
    !
    ! ... Initialisieren von lokaler t_block Variablen
    !
    CALL init_block ( dic_bl )
    !
    IF ( any_error( ) ) RETURN
    !
    ! ... Allokieren
    !
    ALLOCATE ( dic_bl%Key(4), STAT=stat )
    !
    IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
       !
       CALL setup_error_act &
            ( all_errors(:), -10000, c_upname, c_modname, stat )
       CALL setup_error_act ( '<felder>', 'dic_bl%Key' )
       !
       RETURN
       !
    END IF
    ! 
    ! ... Initialisieren von t_key-Komponentenfeld der lokalen t_block Variablen
    !
    DO i = 1, SIZE(dic_bl%Key)
       !
       CALL init_key ( dic_bl%Key(i) )
       !
       IF ( any_error( ) ) RETURN
       !
       ! ... Feld fuer ZeilAnz allokieren
       !
       ALLOCATE ( dic_bl%Key(i)%ZeilAnz(1), STAT=stat )
       !
       IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
          !
          CALL setup_error_act &
               ( all_errors(:), -10000, c_upname, c_modname, stat )
          CALL setup_error_act ( '<felder>', 'dic_bl%Key(i)%ZeilAnz' )
          !
          RETURN
          !
       END IF
       !
    END DO
    !
    IF ( no_error( ) ) THEN
       !
       dic_bl%Key(1)%Name        = 'L_Single'
       dic_bl%Key(1)%L_Opt       = .True.
       dic_bl%Key(1)%L_Single    = .True.
       dic_bl%Key(1)%ZeilAnz(1)  = 0
       !
       dic_bl%Key(2)%Name        = 'L_Opt'
       dic_bl%Key(2)%L_Opt       = .True.
       dic_bl%Key(2)%L_Single    = .True.
       dic_bl%Key(2)%ZeilAnz(1)  = 0
       !
       dic_bl%Key(3)%Name        = 'ReqIf'
       dic_bl%Key(3)%L_Opt       = .True.
       dic_bl%Key(3)%L_Single    = .False.
       dic_bl%Key(3)%ZeilAnz(1)  = 0
       !
       dic_bl%Key(4)%Name        = 'NotIf'
       dic_bl%Key(4)%L_Opt       = .True.
       dic_bl%Key(4)%L_Single    = .False.
       dic_bl%Key(4)%ZeilAnz(1)  = 0
       !
       ! Lesen des BLOCK-Blockes der Dictionary-Datei
       !
       IF ( l_wri ) WRITE(*,*) ' read Dictionary-BLOCK '//TRIM(act_block%block%Name)
       !
       doit_block = .true.
       !
       loop_block : DO
          !
          IF ( any_error( )  .OR. .NOT. doit_block) EXIT loop_block
          !
          CALL readkarte &
               ( dicdat, &
               'Lese Zeile in Block '//TRIM(act_block%block%name), karte)
          !
          IF ( no_error( )  .AND.  LEN_TRIM(karte) .GT. 0 ) THEN
             !
             ! ... wenn keine Leerzeile
             !
             karte = ADJUSTL(karte)
             !
             SELECT CASE (karte(1:9))
             CASE ('BEGINDATA')
                !
                blockname = REPEAT(' ',LEN(blockname))
                blockname = ADJUSTL(karte(10:))
                !
                SELECT CASE ( blockname(1:3) )
                   !
                CASE ('KEY')
                   !
                   IF ( LEN_TRIM(blockname) .GT. 3 ) THEN
                      !
                      ! Gebaeren eines Listenelementes vom Typ <t_vl_key>
                      ! inkl. Initialisierung
                      !
                      CALL birth_vl_key &
                           ( act_key, fst_key, lst_key )
                      !
                      IF ( no_error( ) ) THEN
                         !
                         ! KeyZaehler des aktuellen Block-Elementes erhoehen
                         !
                         act_block%block%KeyAnz = act_block%block%KeyAnz + 1
                         !
                         ! Namensgebung
                         !
                         blockname = ADJUSTL(blockname(4:))
                         !
                         IF (LEN_TRIM(blockname) .GT. LEN(act_key%key%Name)) THEN
                            !
                            ! Fehler -580 : Fehler in der Dictionary-Datei.
                            !              Die Stringlaenge eines Schluesselwortes ist zu lang !
                            !
                            c_actlen = REPEAT(' ',LEN(c_actlen))
                            WRITE(c_actlen,'(I4)') LEN(act_key%key%Name)
                            !
                            CALL setup_error_act ( all_errors(:), -580, c_upname, c_modname )
                            CALL setup_error_act ( '<file_name>', TRIM( dicfile_name ) )
                            CALL setup_error_act ( '<blockname>', TRIM(act_block%block%name) )
                            CALL setup_error_act ( '<len_key_name>', TRIM(c_actlen) )
                            CALL setup_error_act ( '<keyblock_name>', TRIM(blockname) )
                            !
                            RETURN
                            !
                         ELSE
                            !
                            act_key%key%Name = blockname(1:MIN(LEN(act_key%key%Name),LEN_TRIM(blockname)))
                            !
                         END IF
                         !
                      END IF ! no_error( )
                      !
                      ! Lesen eines Dictionary-Blockkeys
                      !
                      IF ( no_error( ) ) CALL read_dicblockkey ( dicdat )
                      !
                   ELSE
                      !
                      ! Fehler -570 : KEY-Block ohne Schluesselwortangabe !
                      !
                      CALL setup_error_act ( all_errors(:), -570, c_upname, c_modname )
                      CALL setup_error_act ( '<file_name>', TRIM( dicfile_name ) )
                      CALL setup_error_act ( '<blockname>', TRIM(act_block%block%Name) )
                      !
                      RETURN
                      !
                   END IF
                   !
                CASE DEFAULT
                   !
                   ! Fehler -560 : BEGINDATA-Kennwort ungleich KEY !
                   ! 
                   CALL setup_error_act ( all_errors(:), -560, c_upname, c_modname )
                   CALL setup_error_act ( '<file_name>', TRIM( dicfile_name ) )
                   CALL setup_error_act ( '<blockname>', TRIM(act_block%block%name) )
                   CALL setup_error_act ( '<akt_name>', TRIM(blockname) )
                   !
                   RETURN
                   !
                END SELECT
                !
             CASE DEFAULT
                !
                CALL read_keyzeile &
                     ( 'Dictionary', TRIM(act_block%block%name), &
                     dic_bl, karte, key, wert, doit_block )
                !
                IF ( no_error( )  .AND.  doit_block ) THEN
                   !
                   CALL read_dicblockzeile &
                        ( key, wert )
                   !
                END IF
                !
             END SELECT
             !
          END IF       ! ( no_error( )  .AND.  LEN(TRIM(karte)) .GT. 0 )
          !
       END DO loop_block
       !
    END IF ! no_error( )
    !
    ! Check : Sind alle required Schluesselwort-Zeilen der Blockdata-Struktur
    !         vorhanden ?
    !
    IF ( no_error( ) ) CALL required_there &
         ( dic_bl, 'BLOCK', &
         TRIM(act_block%block%Name), 'dummy', 'dummy' )
    !
    ! Check : Wurde ReqIf oder NotIf-Bedingung gesetzt, obwohl Block nicht
    !         optional ist
    !
    IF ( no_error( )  .AND.  .NOT. act_block%block%L_Opt ) THEN
       !
       CALL reqif_notif &
            ( dic_bl, 'BLOCK', TRIM(act_block%block%Name), &
            'dummy', 'dummy' )
       !
    END IF
    !
    ! Dynamische KomponentenFelder ReqIf, NotIf des Typs t_block allokieren
    ! --> Check : Enthaelt der Block eine Check-Zeile doppelt ?
    ! --> Allokieren der benoetigten KomponentenFelder
    ! --> Daten uebertragen
    ! --> Elemente der Einleselisten loeschen
    !
    IF ( no_error( ) ) CALL transfer_CKs &
         ( act_ckB, fst_ckB, lst_ckB, vgl_ckB, dic_bl, &
         TRIM(act_block%block%Name), &
         act_block%block%ReqIf, act_block%block%NotIf )
    !     
    ! Elemente der verketteten Liste vom Typ t_vl_key werden nun in das
    ! dynamische Komponenten-Feld vom Typ <t_key> von act_block%block uebertragen
    ! Das UP transfer_KEYs :
    !     - checkt ob mind. 1 Key spezifiziert wurde
    !     - kein Key doppelt, d.h. gleicher Name spezifiziert wurde
    !     - allokiert den Speicherplatz
    !     - transferiert die Daten und gibt den Speicherplatz
    !       fuer die Elemente der verketteten Liste <t_vl_key> wieder frei
    !
    IF ( no_error( ) ) CALL transfer_KEYs ( )
    !
    ! Deallokieren der lokalen Variablen vom Typ t_block
    !
    IF ( no_error( ) ) CALL dealloc_t_block ( dic_bl )
    !
    !   -------------------
    !
    IF ( no_error( )  .AND.  l_wri ) THEN
       !
       WRITE(*,*) ' done'
       !
    END IF
    !
  END SUBROUTINE read_dicblock
  !
  !! Checkt, ob alle required Schluesselwort-Zeilen einer
  !! Blockdata-Struktur vorhanden sind.<BR>
  !! Setzt Fehler falls dies nicht so sein sollte !<BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE required_there &
       ( block, element, c_block, c_key, c_par )
    !
    ! Formalparameter
    !! Key-Beschreibungsfeld der Blockdata-Struktur
    TYPE(t_block)                   , INTENT(IN   )    :: block
    !! Art des Dateielementes, erlaubt :  BLOCK,KEY,PAR
    CHARACTER (LEN=*)               , INTENT(IN   )    :: element
    !! Blockname
    CHARACTER (LEN=*)               , INTENT(IN   )    :: c_block
    !! Keyname
    CHARACTER (LEN=*)               , INTENT(IN   )    :: c_key
    !! Parameter-Nummer
    CHARACTER (LEN=*)               , INTENT(IN   )    :: c_par
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='required_there'
    INTEGER      :: i
    !
    !
    ! Check : alle required Angaben vorhanden ?
    ! 
    DO i = 1, SIZE(block%Key)
       !
       IF ( .NOT. block%Key(i)%L_Opt  .AND. &
            block%Key(i)%ZeilAnz(1) .EQ. 0   ) THEN
          !
          SELECT CASE ( TRIM(element) )
          CASE ('BLOCK')
             !
             ! Fehler -590 : Datei-Element ist unvollstaendig !
             !
             CALL setup_error_act ( all_errors(:), -590, c_upname, c_modname )
             CALL setup_error_act ( '<datei-element>', 'Der BLOCK '//TRIM(c_block) )
             !
          CASE ('KEY')
             !
             ! Fehler -590 : Datei-Element ist unvollstaendig !
             !
             CALL setup_error_act ( all_errors(:), -590, c_upname, c_modname )
             CALL setup_error_act ( '<datei-element>', 'Die KEY-Beschreibung '//TRIM(c_key)//&
                  ' in BLOCK '//TRIM(c_block) )
             !
          CASE ('PAR')
             !
             ! Fehler -590 : Datei-Element ist unvollstaendig !
             !
             CALL setup_error_act ( all_errors(:), -590, c_upname, c_modname )
             CALL setup_error_act ( '<datei-element>', &
                  'Der PARAMETER-Block '//TRIM(c_par)//' des KEYs '&
                  //TRIM(c_key)//' in BLOCK '//TRIM(c_block) )
             !
          END SELECT
          !
          ! Fortsetzung Fehler -590 : Datei-Element ist unvollstaendig !
          !
          CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
          CALL setup_error_act ( '<keyname>', TRIM(block%Key(i)%Name) )
          !
          RETURN
          !
       END IF
       !
    END DO
    !
  END SUBROUTINE required_there
  !
  !! Das Unterprogramm checkt, ob ReqIf oder NotIf-Bedingung
  !! gesetzt wurde, obwohl Block als optional vereinbart ist
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE reqif_notif &
       ( block, element, c_block, c_key, c_par )
    !
    ! Formalparameter
    !! Key-Beschreibungsfeld der Blockdata-Struktur
    TYPE (t_block)                  , INTENT(IN   )    :: block
    !! Elementart, erlaubt : BLOCK,KEY,PAR
    CHARACTER (LEN=*)               , INTENT(IN   )    :: element
    !! Blockname
    CHARACTER (LEN=*)               , INTENT(IN   )    :: c_block
    !! Schluesselwort
    CHARACTER (LEN=*)               , INTENT(IN   )    :: c_key
    !! Parameter-Nummer
    CHARACTER (LEN=*)               , INTENT(IN   )    :: c_par
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='reqif_notif'
    INTEGER                       :: i
    !
    !
    ! Check : ReqIf, NotIf nur gesetzt, wenn Block optional ist
    !
    DO i = 1, SIZE(block%Key)
       !
       IF ( block%Key(i)%Name .EQ. 'ReqIf' .OR. &
            block%Key(i)%Name .EQ. 'NotIf'       ) THEN
          !
          IF ( block%Key(i)%ZeilAnz(1) .GT. 0 ) THEN
             !
             SELECT CASE ( TRIM(element) )
             CASE ('BLOCK')
                !
                ! Fehler -610 : ReqIf, NotIf fuer Block gesetzt, der nicht als optional
                !               vereinbart wurde !
                !
                CALL setup_error_act ( all_errors(:), -610, c_upname, c_modname )
                CALL setup_error_act ( '<dictionary-element>', 'Der BLOCK '//TRIM(c_block) )
                !
             CASE ('KEY')
                !
                ! Fehler -610 : ReqIf, NotIf fuer Key gesetzt, der nicht als optional
                !               vereinbart wurde !
                !
                CALL setup_error_act ( all_errors(:), -610, c_upname, c_modname )
                CALL setup_error_act ( '<dictionary-element>', &
                     'Die KEY-Beschreibung '//c_key//' in BLOCK '//TRIM(c_block) )
                !
             CASE ('PAR')
                !
                ! Fehler -610 : ReqIf, NotIf fuer Parameter gesetzt, der nicht als optional
                !               vereinbart wurde !
                !
                CALL setup_error_act ( all_errors(:), -610, c_upname, c_modname )
                CALL setup_error_act ( '<dictionary-element>', &
                     'Der Parameter '//TRIM(c_par)//' des KEYs '//&
                     TRIM(c_key)//' in BLOCK '//TRIM(c_block))
                !
             END SELECT
             !
             ! Fortsetzung Fehler -610 : ReqIf, NotIf fuer Dateielement (BLOCK,KEY,PAR) gesetzt,
             !                           das nicht als optional vereinbart wurde !
             !
             CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
             CALL setup_error_act ( '<reqif_notif>', TRIM(block%Key(i)%Name) )
             CALL setup_error_act ( '<block_key_par>', element )
             !
             RETURN
             !
          END IF
          !
       END IF
       !
    END DO
    !
  END SUBROUTINE reqif_notif
  !
  !! Elemente der verketteten Liste vom Typ t_vl_key werden
  !! in das dynamische Komponenten-Feld vom Typ <t_key> von
  !! act_block%block uebertragen
  !! Das UP transfer_KEYs :
  !!  - checkt ob mind. 1 Key spezifiziert wurde
  !!  - kein Key doppelt, d.h. gleicher Name spezifiziert
  !!    wurde
  !!  - allokiert den Speicherplatz
  !!  - transferiert die Daten und gibt den Speicherplatz
  !!    fuer die Elemente der verketteten Liste <t_vl_key>
  !!    wieder frei
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE transfer_KEYs &
       ( )
    !    
    ! USE-Statements
    !
    USE m_stdat_types, ONLY : &
         ! Routinen
         dealloc_t_key
    !
    ! Formalparameter
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='transfer_KEYs'
    !! Statusvariable
    INTEGER   :: stat ! 
    !
    INTEGER   :: nr
    !
    !
    IF ( act_block%block%KeyAnz .EQ. 0 ) THEN
       !
       ! Fehler -620 : Es wurde kein Schluesselwort spezifiziert ?
       !
       CALL setup_error_act ( all_errors(:), -620, c_upname, c_modname )
       CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
       CALL setup_error_act ( '<blockname>', TRIM(act_block%block%Name) )
       !
       RETURN
       !
    END IF
    !
    ! Check : Wurde ein Schluesselwort versehentlich mehrfach beschrieben ?
    !
    act_key => fst_key
    !
    outer: DO
       !
       IF ( .NOT. ASSOCIATED(act_key) ) EXIT outer
       !
       vgl_key => act_key%next
       !
       inner: DO 
          !
          IF ( .NOT. ASSOCIATED(vgl_key) ) EXIT inner
          !
          IF ( TRIM(act_key%key%Name) .EQ. TRIM(vgl_key%key%Name) ) THEN
             !
             ! Fehler -630 : Ein Schluesselwort wurde mehrfach beschrieben !
             !
             CALL setup_error_act ( all_errors(:), -630, c_upname, c_modname )
             CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
             CALL setup_error_act ( '<blockname>', TRIM(act_block%block%Name) )
             CALL setup_error_act ( '<keyname>', TRIM(act_key%key%Name) )
             !
             RETURN
             !
          END IF
          !  
          vgl_key => vgl_key%next
          !
       END DO inner
       !
       act_key => act_key%next
       !
    END DO outer
    !
    ! ... KompomnentenFeld Key allokieren
    !
    ALLOCATE (act_block%block%Key(act_block%block%KeyAnz), &
         STAT=stat)
    !
    IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
       !
       CALL setup_error_act &
            ( all_errors(:), -10000, c_upname, c_modname, stat )
       CALL setup_error_act ( '<felder>', 't_block%Key' )
       !
       RETURN
       !
    END IF
    !
    !   Elemente der verketteten Liste vom Typ t_vl_key werden nun in das
    !   dynamische Komponenten-Feld vom Typ <t_key> der Variablen 
    !   vom Typ <t_block> uebertragen
    !
    ! ... erstes Listenelement
    !
    act_key => fst_key
    ! 
    ! ... Schleife ueber alle Listenelemente
    !
    nr = 0
    vlist : DO
       !
       IF ( .NOT. ASSOCIATED(act_key) ) EXIT vlist
       !
       nr = nr + 1
       !
       act_block%block%Key(nr) = act_key%key
       !
       ! ... naechstes Listenelement ansprechen
       !
       lst_key => act_key
       act_key => act_key%next
       !
       ! ... Deallokieren des wegsortierten Listenelementes
       !
       DEALLOCATE (lst_key)
       !
    END DO vlist
    !
    !   Pointerverbindungen loesen
    !
    NULLIFY (fst_key)
    NULLIFY (act_key)
    NULLIFY (lst_key)
    NULLIFY (vgl_key)
    !
  END SUBROUTINE transfer_KEYs
  !
  !! Liest KEY-Block der Dictionary-Datei. <BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE read_dicblockkey &
       ( dicdat )
    !
    ! Formalparameter
    !! Dictionary-Datei
    TYPE (t_file)    , INTENT(IN   )   :: dicdat
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='read_dicblockkey'
    !! Statusvariable
    INTEGER   :: stat ! 
    !
    CHARACTER (LEN=key_len)              :: blockname, key
    CHARACTER (LEN=line_len)             :: karte, wert
    !
    LOGICAL                              :: doit_key
    !
    TYPE(t_block)                        :: key_bl
    CHARACTER (LEN=3)                    :: c_parpos
    INTEGER                              :: i, ios
    !
    !
    ! Initialisierungen
    !
    NULLIFY (fst_par)
    NULLIFY (act_par)
    NULLIFY (lst_par)
    NULLIFY (vgl_par)
    !
    NULLIFY (fst_ckK)
    NULLIFY (act_ckK)
    NULLIFY (lst_ckK)
    NULLIFY (vgl_ckK)
    !
    ! Beschreibung der moeglichen Schluesselwoerter des KEY-Blockes
    !
    ! ... Initialisieren der lokaler <t_block>-Variablen
    !
    CALL init_block ( key_bl )
    !
    IF ( any_error( ) ) RETURN
    !
    ! ... Allokieren der lokaler <t_block>-Variablen
    !
    ALLOCATE (key_bl%Key(6), STAT=stat)
    !
    IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
       !
       CALL setup_error_act &
            ( all_errors(:), -10000, c_upname, c_modname, stat )
       CALL setup_error_act ( '<felder>', 'key_bl' )
       !
       RETURN
       !
    END IF
    !
    ! ... Initialisieren von <t_key>-Komponentenfeld der lokalen
    !     <t_block>-Variablen
    !
    DO i = 1, SIZE(key_bl%Key)
       !
       IF ( any_error( ) ) EXIT
       !
       CALL init_key ( key_bl%Key(i) )
       !
       IF ( no_error( ) ) THEN
          !
          ! ... Feld fuer ZeilAnz allokieren
          !
          ALLOCATE ( key_bl%Key(i)%ZeilAnz(1), STAT=stat )
          !
          IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
             !
             CALL setup_error_act &
                  ( all_errors(:), -10000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<felder>', 'key_bl%Key%ZeilAnz' )
             !
          END IF
          !
       END IF ! no_error( )
       !
    END DO ! i = 1, SIZE(key_bl%Key)
    !
    IF ( any_error( ) ) RETURN
    !
    ! Setzen der fuer KEY-Bloecke erlaubten Schluesselwoerter und ihrer 
    ! Attribute
    !
    key_bl%Key(1)%Name      = 'L_Single'
    key_bl%Key(1)%L_Opt     = .True.
    key_bl%Key(1)%L_Single  = .True.
    key_bl%Key(1)%ZeilAnz(1)   = 0
    !
    key_bl%Key(2)%Name        = 'L_Opt'
    key_bl%Key(2)%L_Opt       = .True.
    key_bl%Key(2)%L_Single    = .True.
    key_bl%Key(2)%ZeilAnz(1)  = 0
    !
    key_bl%Key(3)%Name        = 'ReqIf'
    key_bl%Key(3)%L_Opt       = .True.
    key_bl%Key(3)%L_Single    = .False.
    key_bl%Key(3)%ZeilAnz(1)  = 0
    !
    key_bl%Key(4)%Name        = 'NotIf'
    key_bl%Key(4)%L_Opt       = .True.
    key_bl%Key(4)%L_Single    = .False.
    key_bl%Key(4)%ZeilAnz(1)  = 0
    !
    key_bl%Key(5)%Name        = 'L_OneArray'
    key_bl%Key(5)%L_Opt       = .True.
    key_bl%Key(5)%L_Single    = .True.
    key_bl%Key(5)%ZeilAnz(1)  = 0
    !
    key_bl%Key(6)%Name        = 'L_Comment'
    key_bl%Key(6)%L_Opt       = .True.
    key_bl%Key(6)%L_Single    = .True.
    key_bl%Key(6)%ZeilAnz(1)  = 0
    !
    ! Lesen des KEY-Blockes der Dictionary-Datei
    ! 
    IF ( l_wri ) THEN
       !
       WRITE(*,*) &
            '   read Dictionary-KEY '//TRIM(act_key%key%name)
    END IF
    !
    doit_key = .true.
    !
    loop_key : DO
       !
       IF ( .NOT. doit_key ) EXIT loop_key
       !
       CALL readkarte &
            ( dicdat, 'Lese Zeile in KEY-Block '//TRIM(act_key%key%name), karte )
       !
       IF ( any_error( ) ) RETURN
       !
       ! ... wenn keine Leerzeile
       !
       IF ( LEN_TRIM(karte) .GT. 0 ) THEN
          !
          karte = ADJUSTL(karte)
          !
          SELECT CASE ( karte(1:9) )
          CASE ('BEGINDATA')
             !
             blockname = REPEAT(' ',LEN(blockname))
             blockname = ADJUSTL(karte(10:)) 
             !
             SELECT CASE ( blockname(1:3) )
                !
             CASE ('PAR')
                !
                IF ( LEN_TRIM(blockname) .GT. 3 ) THEN
                   !
                   ! Gebaeren eines Listenelementes vom Typ <t_vl_par>
                   ! inkl. Initialisierung
                   !
                   CALL birth_vl_par ( act_par, fst_par, lst_par )
                   !
                   IF ( any_error( ) ) RETURN
                   !
                   ! ParZaehler des aktuellen Key-Elementes erhoehen
                   !
                   act_key%key%ParAnz = act_key%key%ParAnz + 1
                   !
                   ! Positionsnummer eintragen
                   !
                   c_parpos = ADJUSTL(blockname(4:))
                   !
                   READ(c_parpos(1:3),'(I3)',IOSTAT=ios) act_par%par%ParPos
                   !
                   IF ( ios .NE. 0 ) THEN
                      !
                      ! Fehler -660 : beim Lesen der ParameterPosition
                      !
                      CALL setup_error_act ( all_errors(:), -660, c_upname, c_modname )
                      CALL setup_error_act ( '<file_name>' , TRIM( dicfile_name ) )
                      CALL setup_error_act ( '<keyname>', TRIM(act_key%key%Name) )
                      CALL setup_error_act ( '<blockname>', TRIM(act_block%block%Name) )
                      !
                      RETURN
                      !
                   END IF
                   !
                   ! Lesen eines Dictionary-KEY-Block-Parameters
                   !
                   CALL read_dickeyPar ( dicdat )
                   !
                   IF ( any_error( ) ) RETURN
                   !
                ELSE
                   !
                   ! Fehler -650 : PAR-Block ohne Parameternummer
                   !
                   CALL setup_error_act ( all_errors(:), -650, c_upname, c_modname )
                   CALL setup_error_act ( '<file_name>' , TRIM( dicfile_name ) )
                   CALL setup_error_act ( '<keyname>', TRIM(act_key%key%Name) )
                   CALL setup_error_act ( '<blockname>', TRIM(act_block%block%Name) )
                   !
                   RETURN
                   !
                END IF
                !
             CASE DEFAULT
                !
                ! Fehler -640 : BEGINDATA-Kennwort ungleich PAR !
                !
                CALL setup_error_act ( all_errors(:), -640, c_upname, c_modname )
                CALL setup_error_act ( '<file_name>' , TRIM( dicfile_name ) )
                CALL setup_error_act ( '<keyname>', TRIM(act_key%key%Name) )
                CALL setup_error_act ( '<blockname>', TRIM(act_block%block%Name) )
                CALL setup_error_act ( '<akt_name>', TRIM(blockname) )
                !
                RETURN
                !
             END SELECT
             !
          CASE DEFAULT
             !
             IF ( LEN(TRIM(karte)) .GT. 0 ) THEN
                !
                CALL read_keyzeile &
                     ( 'Dictionary', &
                     'KEY '//TRIM(act_key%key%Name)//&
                     ' in BLOCK '//TRIM(act_block%block%Name), &
                     key_bl, karte, key, wert, doit_key )
                !
                IF ( any_error( ) ) RETURN
                !
                IF ( doit_key ) THEN
                   !
                   CALL read_dickeyzeile &
                        ( key, wert )
                   !
                   IF ( any_error( ) ) RETURN
                   !
                END IF
                !
             END IF
             !
          END SELECT
          !
       END IF ! ( LEN_TRIM(karte) .GT. 0 )
       !
    END DO loop_key
    !
    ! Check : Sind alle required Schluesselwort-Zeilen der Blockdata-Struktur
    !         vorhanden ?
    !
    CALL required_there &
         ( key_bl, 'KEY', &
         TRIM(act_block%block%Name), TRIM(act_key%key%Name), 'dummy' )
    !
    IF ( any_error( ) ) RETURN
    !
    ! Check : Wurde ReqIf oder NotIf-Bedingung gesetzt, obwohl Key nicht
    !         optional ist
    !
    IF ( .NOT. act_key%key%L_Opt ) THEN
       !
       CALL reqif_notif &
            ( key_bl, 'KEY', TRIM(act_block%block%Name), &
            TRIM(act_key%key%Name), 'dummy' )
       !
       IF ( any_error( ) ) RETURN
       !
    END IF
    !
    ! Dynamische KomponentenFelder ReqIf, NotIf der Variablen vom Typ <t_key>
    ! belegen
    ! --> Check : Enthaelt der KEY-Block eine Check-Zeile doppelt ?
    ! --> Allokieren der benoetigten KomponentenFelder
    ! --> Daten uebertragen
    ! --> Elemente der Einleselisten loeschen
    !
    CALL transfer_CKs &
         ( act_ckK, fst_ckK, lst_ckK, vgl_ckK, key_bl, &
         'KEY '//TRIM(act_key%key%Name)//' in Block '//&
         TRIM(act_block%block%Name), &
         act_key%key%ReqIf, act_key%key%NotIf )
    !
    IF ( any_error( ) ) RETURN
    !
    ! Elemente der verketteten Liste vom Typ t_vl_par werden nun in das
    ! dynamische Komponenten-Feld vom Typ <t_par> von act_key%key uebertragen
    ! Das UP transfer_PARs :
    !          - checkt ob mind. 1 Parameter spezifiziert wurde
    !          - kein Parameter doppelt, d.h. gleiche Positionsnummer
    !            spezifiziert wurde
    !          - allokiert den Speicherplatz des Komponentenfeldes
    !          - transferiert die Daten und gibt den Speicherplatz
    !            fuer die Elemente der verketteten Liste <t_vl_par> wieder frei
    !
    CALL transfer_PARs &
         ( )
    !
    IF ( any_error( ) ) RETURN
    !
    ! Pruefen, wenn Keyzeile = Kommentarzeile ob folgende Bedingungen erfuellt 
    ! sind :   - nur ein PAR fuer KEY vereinbart
    !          - Par%Type = 'CHAR'
    !          - Par%LArray = False
    ! ( Kommentar-Zeile bedeutet : Blanks sind keine Eingabe-Trennzeichen )
    !
    IF ( act_key%key%L_Comment ) THEN
       !
       IF ( SIZE(act_key%key%Par) .GT. 1 ) THEN
          !
          ! Fehler -680 : fuer Kommentarzeile darf nicht mehr als ein Parameter
          !               vereinbart werden.
          !
          CALL setup_error_act ( all_errors(:), -680, c_upname, c_modname )
          CALL setup_error_act ( '<file_name>' , TRIM( dicfile_name ) )
          CALL setup_error_act ( '<keyname>', TRIM(act_key%key%Name) )
          CALL setup_error_act ( '<blockname>', TRIM(act_block%block%Name) )
          !
          RETURN
          !
       END IF
       !
       IF ( act_key%key%Par(1)%Type .NE. 'CHAR' ) THEN
          !
          ! Fehler -690 : wenn Key%Par(1)%Type .NE. 'CHAR'
          !               fuer Kommentarzeile ist nur CHARACTER als Parametertyp zulaessig
          CALL setup_error_act ( all_errors(:), -690, c_upname, c_modname )
          CALL setup_error_act ( '<file_name>' , TRIM( dicfile_name ) )
          CALL setup_error_act ( '<keyname>', TRIM(act_key%key%Name) )
          CALL setup_error_act ( '<blockname>', TRIM(act_block%block%Name) )
          !
          RETURN
          !
       END IF
       !
       IF ( act_key%key%Par(1)%L_Array ) THEN
          !
          ! Fehler -700 : wenn Key%Par(1)%L_Array = T
          !               bei Kommentarzeilen darf L_Array nicht TRUE gesetzt werden
          !
          CALL setup_error_act ( all_errors(:), -700, c_upname, c_modname )
          CALL setup_error_act ( '<file_name>' , TRIM( dicfile_name ) )
          CALL setup_error_act ( '<keyname>', TRIM(act_key%key%Name) )
          CALL setup_error_act ( '<blockname>', TRIM(act_block%block%Name) )
          !
          RETURN
          !
       END IF
       !
    END IF   ! act_key%key%L_Comment 
    !
    ! Wenn fuer einen KEY nur ein einziger Parameter vereinbart wurde, so ist 
    ! es unsinnig ihn mit dem Attribut L_Opt = True zu versehen, da das ueber
    ! die KEY-Optionalitaet steuerbar ist !
    !
    IF ( SIZE(act_key%key%Par) .EQ. 1 ) THEN
       !
       IF ( act_key%key%Par(1)%L_Opt ) THEN
          !
          ! Fehler -670 : ueberfluessiges L_Opt-Attribut
          !
          CALL setup_error_act ( all_errors(:), -670, c_upname, c_modname )
          CALL setup_error_act ( '<file_name>' , TRIM( dicfile_name ) )
          CALL setup_error_act ( '<blockname>', TRIM(act_block%block%Name) )
          CALL setup_error_act ( '<keyname>', TRIM(act_key%key%Name) )
          !
          RETURN
          ! 
       END IF
       !
    END IF
    !
    ! Deallokieren der lokalen Variablen vom Typ t_block
    !
    CALL dealloc_t_block ( key_bl )
    !
    IF ( any_error( ) ) RETURN
    !
    !
    IF ( l_wri ) THEN
       !
       WRITE(*,*) '   done'
       !
    END IF
    !
  END SUBROUTINE read_dicblockkey
  !
  !! Elemente der verketteten Liste vom Typ <t_vl_par> werden
  !! nun in das dynamische Komponenten-Feld vom Typ <t_par>
  !! von <act_key%key> uebertragen
  !! Das UP transfer_PARs :
  !!    - checkt, ob mind. 1 Parameter spezifiziert wurde
  !!    - checkt, ob kein Parameter doppelt, d.h. gleiche
  !!      Positionsnummer spezifiziert wurde
  !!    - allokiert den Speicherplatz des Komponentenfeldes
  !!    - transferiert die Daten und gibt den Speicherplatz
  !!      fuer die Elemente der verketteten Liste <t_vl_par>
  !!      wieder frei
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE transfer_PARs &
       ( )
    !
    ! USE-Statements
    !
    USE m_stdat_types, ONLY : &
         ! Routinen
         dealloc_t_par
    !
    ! Formalparameter
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='transfer_PARs'
    !! Statusvariable
    INTEGER   :: stat ! 
    !
    CHARACTER (LEN=3)                  :: c_parpos
    INTEGER                            :: i
    !
    !
    IF ( act_key%key%ParAnz .EQ. 0 ) THEN
       !
       ! Fehler -710 : Es wurde kein Parameter spezifiziert !
       !
       CALL setup_error_act ( all_errors(:), -710, c_upname, c_modname )
       CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
       CALL setup_error_act ( '<keyname>', TRIM(act_key%key%Name) )
       CALL setup_error_act ( '<blockname>', TRIM(act_block%block%Name) )
       !
       RETURN
       !
    END IF
    !
    ! Check : Wurde ein Parameter versehentlich mehrfach beschrieben ?
    !
    act_par => fst_par
    !
    outer: DO
       !
       IF ( .NOT. ASSOCIATED(act_par) ) EXIT outer
       !
       vgl_par => act_par%next
       !
       inner: DO 
          !
          IF ( .NOT. ASSOCIATED(vgl_par) ) EXIT inner
          !
          IF ( act_par%par%ParPos .EQ. vgl_par%par%ParPos ) THEN
             !
             ! Fehler -720 : Ein Parameter mit einer bestimmten Positions-Nummer
             !             wurde mehrfach spezifiziert !
             !
             WRITE(c_parpos(1:3),'(I3)') act_par%par%ParPos
             c_parpos = ADJUSTL(c_parpos)
             !
             CALL setup_error_act ( all_errors(:), -720, c_upname, c_modname )
             CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
             CALL setup_error_act ( '<keyname>', TRIM(act_key%key%Name) )
             CALL setup_error_act ( '<blockname>', TRIM(act_block%block%Name) )
             CALL setup_error_act ( '<pos_nr>', TRIM(c_parpos) )
             !
             RETURN
             !   
          END IF
          ! 
          vgl_par => vgl_par%next
          !
       END DO inner
       !
       act_par => act_par%next
       !
    END DO outer
    !
    ! KompomnentenFeld Par allokieren
    !
    ALLOCATE ( act_key%key%Par(act_key%key%ParAnz), STAT=stat )
    !
    IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
       !
       CALL setup_error_act &
            ( all_errors(:), -10000, c_upname, c_modname, stat )
       CALL setup_error_act ( '<felder>', 't_key%Par' )
       !
       RETURN
       !
    END IF
    !
    ! Elemente der verketteten Liste vom Typ <t_vl_par> werden nun in das
    ! dynamische Komponenten-Feld vom Typ <t_par> der Variablen 
    ! vom Typ <t_key> uebertragen
    !
    ! ... erstes Listenelement
    !
    act_par => fst_par
    !
    ! ... Schleife ueber alle Listenelemente
    !
    vlist : DO
       !
       IF ( .NOT. ASSOCIATED(act_par) ) EXIT vlist
       !
       IF ( act_par%par%ParPos .GT. SIZE(act_key%key%Par) ) THEN
          !
          ! Fehler -730 : Parameter-Positions-Nummer liegt ausserhalb des zulaessigen
          !               Wertebereiches !
          !
          WRITE(c_parpos(1:3),'(I3)') act_par%par%ParPos
          c_parpos = ADJUSTL(c_parpos)
          !
          CALL setup_error_act ( all_errors(:), -730, c_upname, c_modname )
          CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
          CALL setup_error_act ( '<keyname>', TRIM(act_key%key%Name) )
          CALL setup_error_act ( '<blockname>', TRIM(act_block%block%Name) )
          CALL setup_error_act ( '<pos_nr>', TRIM(c_parpos) )
          !
          RETURN
          !
       ELSE
          !
          act_key%key%Par(act_par%par%ParPos) = act_par%par
          !
       END IF
       !
       ! ... naechste Datei initialisieren
       !
       lst_par => act_par
       act_par => act_par%next
       !
       ! ... Deallokieren des wegsortierten Listenelementes
       !
       DEALLOCATE (lst_par)
       !
    END DO vlist
    !
    ! Pointerverbindungen loesen
    !
    NULLIFY (fst_par)
    NULLIFY (act_par)
    NULLIFY (lst_par)
    NULLIFY (vgl_par)
    !
    !
    !   Check : Fuer Keyzeilen mit mehreren Parametern duerfen die
    !           Parameter-Attribute L_Array und L_Opt nur fuer den Parameter an 
    !           letzter Position auf True gesetzt sein
    !
    IF ( SIZE(act_key%key%Par) .GT. 1 ) THEN
       !
       DO i = 1, SIZE(act_key%key%Par) - 1
          !
          IF ( act_key%key%Par(i)%L_Array ) THEN
             !
             ! Fehler -740 : Ein Parameter wurde als Array-Groesse vereinbart (L_Array=True),
             !               obwohl er nicht an letzter Position der Key-Zeile steht !
             !
             WRITE(c_parpos(1:3),'(I3)') act_key%key%Par(i)%ParPos
             c_parpos = ADJUSTL(c_parpos)
             !
             CALL setup_error_act ( all_errors(:), -740, c_upname, c_modname )
             CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
             CALL setup_error_act ( '<pos_nr>', TRIM(c_parpos) )
             CALL setup_error_act ( '<keyname>', TRIM(act_key%key%Name) )
             CALL setup_error_act ( '<blockname>', TRIM(act_block%block%Name) )
             !
             RETURN
             !
          END IF
          !
          IF ( act_key%key%Par(i)%L_Opt ) THEN
             !
             ! Fehler -750 : Ein Parameter wurde als optional (L_Opt=True) vereinbart,
             !               obwohl er nicht an letzter Position der Key-Zeile steht !
             !
             WRITE(c_parpos(1:3),'(I3)') act_key%key%Par(i)%ParPos
             c_parpos = ADJUSTL(c_parpos)
             !
             CALL setup_error_act ( all_errors(:), -750, c_upname, c_modname )
             CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
             CALL setup_error_act ( '<pos_nr>', TRIM(c_parpos) )
             CALL setup_error_act ( '<keyname>', TRIM(act_key%key%Name) )
             CALL setup_error_act ( '<blockname>', TRIM(act_block%block%Name) )
             !
             RETURN
             !
          END IF
          !
       END DO
       !
    END IF
    !
  END SUBROUTINE transfer_PARs
  !
  !! Liest PAR-Block der Dictionary-Datei
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE read_dickeyPar &
       ( dicdat )
    !
    ! USE-Statements
    !
    USE m_felder, ONLY : &
         ! Routinen
         transfer_felder
    !
    ! Formalparameter
    !! Dictionary-Datei
    TYPE (t_file)    , INTENT(IN   )  :: dicdat
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='read_dickeyPar'
    !! Statusvariable
    INTEGER   :: stat ! 
    !
    CHARACTER (LEN=key_len)             :: key
    CHARACTER (LEN=line_len)            :: karte, wert
    !
    LOGICAL                             :: doit_par
    !
    TYPE(t_block)                       :: par_bl
    CHARACTER (LEN=3)                   :: c_parpos
    INTEGER                             :: i
    !
    !
    ! Initialisierungen
    !
    NULLIFY (fst_ckP)
    NULLIFY (act_ckP)
    NULLIFY (lst_ckP)
    NULLIFY (vgl_ckP)
    !
    NULLIFY (fst_fd)
    NULLIFY (act_fd)
    NULLIFY (lst_fd)
    NULLIFY (vgl_fd)
    !
    ! Beschreibung der moeglichen Schluesselwoerter des PAR-Blockes
    !
    ! ... Initialisieren von lokaler <t_block> Variablen
    CALL init_block ( par_bl )
    !
    IF ( any_error( ) ) RETURN
    !
    ! ... Allokieren von lokaler <t_block> Variablen
    !
    ALLOCATE ( par_bl%Key(9), STAT=stat )
    !
    IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
       !
       CALL setup_error_act &
            ( all_errors(:), -10000, c_upname, c_modname, stat )
       CALL setup_error_act ( '<felder>', 'par_bl' )
       !
       RETURN
       !
    END IF
    !
    ! ... Initialisieren von <t_key>-Komponentenfeld der lokalen
    !     <t_block>-Variablen
    !
    DO i = 1, SIZE(par_bl%Key)
       !
       CALL init_key ( par_bl%Key(i) )
       !
       IF ( any_error( ) ) RETURN
       !
       ! ... Feld fuer ZeilAnz allokieren
       !
       ALLOCATE ( par_bl%Key(i)%ZeilAnz(1), STAT=stat )
       !
       IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
          !
          CALL setup_error_act &
               ( all_errors(:), -10000, c_upname, c_modname, stat )
          CALL setup_error_act ( '<felder>', 'par_bl%Key%ZeilAnz' )
          !
          RETURN
          !
       END IF
       !
    END DO
    !
    ! Setzen der fuer PARAMETER-Bloecke erlaubten Schluesselwoerter und ihrer 
    ! Attribute
    !
    par_bl%Key( 1)%Name        = 'Type'
    par_bl%Key( 1)%L_Opt       = .False.
    par_bl%Key( 1)%L_Single    = .True.
    par_bl%Key( 1)%ZeilAnz(1)  = 0
    !
    par_bl%Key( 2)%Name        = 'L_Opt'
    par_bl%Key( 2)%L_Opt       = .True.
    par_bl%Key( 2)%L_Single    = .True.
    par_bl%Key( 2)%ZeilAnz(1)  = 0
    !
    par_bl%Key( 3)%Name        = 'ReqIf'
    par_bl%Key( 3)%L_Opt       = .True.
    par_bl%Key( 3)%L_Single    = .False.
    par_bl%Key( 3)%ZeilAnz(1)  = 0
    !
    par_bl%Key( 4)%Name        = 'NotIf'
    par_bl%Key( 4)%L_Opt       = .True.
    par_bl%Key( 4)%L_Single    = .False.
    par_bl%Key( 4)%ZeilAnz(1)  = 0
    !
    par_bl%Key( 5)%Name        = 'L_Array'
    par_bl%Key( 5)%L_Opt       = .True.
    par_bl%Key( 5)%L_Single    = .True.
    par_bl%Key( 5)%ZeilAnz(1)  = 0
    !
    par_bl%Key( 6)%Name        = 'FixValue'
    par_bl%Key( 6)%L_Opt       = .True.
    par_bl%Key( 6)%L_Single    = .False.
    par_bl%Key( 6)%ZeilAnz(1)  = 0
    !
    par_bl%Key( 7)%Name        = 'CheckIfPar'
    par_bl%Key( 7)%L_Opt       = .True.
    par_bl%Key( 7)%L_Single    = .False.
    par_bl%Key( 7)%ZeilAnz(1)  = 0
    !
    par_bl%Key( 8)%Name        = 'L_FileReq'
    par_bl%Key( 8)%L_Opt       = .True.
    par_bl%Key( 8)%L_Single    = .True.
    par_bl%Key( 8)%ZeilAnz(1)  = 0
    !
    par_bl%Key( 9)%Name        = 'L_FileNew'
    par_bl%Key( 9)%L_Opt       = .True.
    par_bl%Key( 9)%L_Single    = .True.
    par_bl%Key( 9)%ZeilAnz(1)  = 0
    !
    ! Lesen des KEY-Blockes der Dictionary-Datei
    !
    IF ( l_wri ) THEN
       !
       WRITE(*,*) &
            '     read Dictionary-PAR ',act_par%par%ParPos
    END IF
    !
    !
    WRITE(c_parpos(1:3),'(I3)') act_par%par%ParPos
    c_parpos = ADJUSTL(c_parpos)
    !
    doit_par = .true.
    !
    loop_par : DO
       !
       IF (.NOT. doit_par) EXIT loop_par
       !
       CALL readkarte &
            ( dicdat, 'Lese Zeile in PAR-Block '//TRIM(c_parpos) , karte )
       !
       IF ( any_error( ) ) RETURN
       !
       ! ... wenn keine Leerzeile
       IF ( LEN_TRIM(karte) .GT. 0 ) THEN
          !
          karte = ADJUSTL(karte)
          !
          CALL read_keyzeile &
               ( 'Dictionary', &
               'PAR'//TRIM(c_parpos)//' des KEYs '//TRIM(act_key%key%Name)// &
               ' in BLOCK '//TRIM(act_block%block%Name) , &
               par_bl, karte, key, wert, doit_par )
          !
          IF ( any_error( ) ) RETURN
          !
          IF ( doit_par ) THEN
             ! 
             CALL read_dicparzeile ( key, wert )
             !
             IF ( any_error( ) ) RETURN
             !
          END IF
          !
       END IF
       !
    END DO loop_par
    !
    ! Check : Sind alle required Schluesselwort-Zeilen der Blockdata-Struktur
    !         vorhanden ?
    !
    CALL required_there &
         ( par_bl, 'PAR', &
         TRIM(act_block%block%Name), TRIM(act_key%key%Name), TRIM(c_parpos) )
    !
    IF ( any_error( ) ) RETURN
    !
    ! Check : Wurde ReqIf oder NotIf-Bedingung gesetzt, obwohl Par nicht
    !         optional ist
    !
    IF ( .NOT. act_par%par%L_Opt ) THEN
       !
       CALL reqif_notif &
            ( par_bl, 'PAR', TRIM(act_block%block%Name), &
            TRIM(act_key%key%Name), TRIM(c_parpos) )
       !
       IF ( any_error( ) ) RETURN
       !
    END IF
    !
    ! Fuer ParType = 'FILE' : 
    !
    IF ( act_par%par%Type .EQ. 'FILE' ) THEN
       !
       IF ( act_par%par%L_FilReq  .AND. &
            act_par%par%L_FilNew         ) THEN
          !
          ! Fehler -770 : L_FilReq und L_FilNew sind gleichzeitig auf True gesetzt !
          !
          CALL setup_error_act ( all_errors(:), -770, c_upname, c_modname )
          CALL setup_error_act ( '<file_name>' , TRIM( dicfile_name ) )
          CALL setup_error_act ( '<pos_nr>', TRIM(c_parpos) )
          CALL setup_error_act ( '<keyname>', TRIM(act_key%key%Name) )
          CALL setup_error_act ( '<blockname>', TRIM(act_block%block%Name) )
          !
          RETURN
          !
       END IF
       !
    END IF
    !
    !   L_FilReq, L_FilNew duerfen nur gesetzt werden, wenn ParType = 'FILE'
    !
    IF ( act_par%par%Type .NE. 'FILE' ) THEN
       !
       IF ( par_bl%Key( 8)%ZeilAnz(1) .GT. 0  .OR. &
            par_bl%Key( 9)%ZeilAnz(1) .GT. 0       ) THEN
          !
          !
          ! Fehler -760 : Eine L_File... -Bedingung wurde gesetzt, obwohl
          !               der Eingabeparameter nicht vom Typ FILE ist !
          !
          CALL setup_error_act ( all_errors(:), -760, c_upname, c_modname )
          CALL setup_error_act ( '<file_name>' , TRIM( dicfile_name ) )
          CALL setup_error_act ( '<pos_nr>', TRIM(c_parpos) )
          CALL setup_error_act ( '<keyname>', TRIM(act_key%key%Name) )
          CALL setup_error_act ( '<blockname>', TRIM(act_block%block%Name) )
          !
          RETURN
          !
       END IF
       !
    END IF
    !
    ! Dynamische KomponentenFelder ReqIf, NotIf der Variablen vom Typ <t_par>
    ! belegen
    ! --> Check : Enthaelt der PAR-Block eine Check-Zeile doppelt ?
    ! --> Allokieren der benoetigten KomponentenFelder
    ! --> Daten uebertragen
    ! --> Elemente der Einleselisten loeschen
    !
    CALL transfer_CKs &
         ( act_ckP, fst_ckP, lst_ckP, vgl_ckP, par_bl, &
         'PAR '//TRIM(c_parpos)//', KEY '//TRIM(act_key%key%Name)//&
         ' in Block '//TRIM(act_block%block%Name), &
         act_par%par%ReqIf, act_par%par%NotIf, act_par%par%CheckIf )
    !
    IF ( any_error( ) ) RETURN
    !
    !
    ! Dynamisches KomponentenFeld FixValue der Variablen vom
    ! Typ <t_par> belegen
    ! --> Check : Enthaelt der PAR-Block eine FixValue-Zeile doppelt ?
    ! --> Allokieren der benoetigten KomponentenFelder
    ! --> Daten uebertragen
    ! --> Elemente der Einleselisten loeschen
    !
    CALL transfer_felder &
         ( act_fd, fst_fd, lst_fd, vgl_fd, par_bl, &
         'PAR '//TRIM(c_parpos)//', KEY '//TRIM(act_key%key%Name)//&
         ' in Block '//TRIM(act_block%block%Name), &
         act_par%par%Type, act_par%par%FixValue )
    !
    IF ( any_error( ) ) RETURN
    !
    !
    ! Deallokieren der lokalen Variablen vom Typ t_block
    !
    CALL dealloc_t_block ( par_bl )
    !
    IF ( any_error( ) ) RETURN
    !
    !
    IF ( l_wri ) WRITE(*,*) '     done'
    !
  END SUBROUTINE read_dickeyPar
  !
  !! Liest die Zeilen eines Dictionary-BLOCK-Blockes.<BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE read_dicblockzeile &
       ( key, wert )
    !
    ! Formalparameter
    !! Schluesselwort
    CHARACTER (LEN=*)  , INTENT(IN   )   :: key
    !! Wert einer SchluesselwortZeile
    CHARACTER (LEN=*)  , INTENT(IN   )   :: wert
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='read_dicblockzeile'
    !
    !
    SELECT CASE (TRIM(key))
       !
    CASE ('L_Single')
       !
       CALL read_logical &
            ( TRIM(key), TRIM(wert), act_block%block%L_Single)
       !
       IF ( any_error( ) ) RETURN
       !
    CASE ('L_Opt')
       !
       CALL read_logical &
            ( TRIM(key), TRIM(wert), act_block%block%L_Opt)
       !
       IF ( any_error( ) ) RETURN
       !
    CASE ('ReqIf', 'NotIf')
       !
       CALL birth_vl_kz &
            ( fst_ckB, lst_ckB, act_ckB)
       !
       IF ( any_error( ) ) RETURN
       !
       act_ckB%key  = TRIM(key)
       act_ckB%wert = TRIM(wert)
       !
    END SELECT
    !
  END SUBROUTINE read_dicblockzeile
  !
  !! Liest die Zeilen eines Dictionary-KEY-Blockes.<BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE read_dickeyzeile &
       ( key, wert )
    !
    ! Formalparameter
    !! Schluesselwort
    CHARACTER (LEN=*)  , INTENT(IN   )   :: key
    !! Wert einer SchluesselwortZeile
    CHARACTER (LEN=*)  , INTENT(IN   )   :: wert
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='read_dickeyzeile'
    !
    !
    SELECT CASE (TRIM(key))
       !
    CASE ('L_Single')
       !
       CALL read_logical &
            ( TRIM(key), TRIM(wert), act_key%key%L_Single)
       !
       IF ( any_error( ) ) RETURN
       !
    CASE ('L_Opt')
       !
       CALL read_logical &
            ( TRIM(key), TRIM(wert), act_key%key%L_Opt)
       !
       IF ( any_error( ) ) RETURN
       !
    CASE ('ReqIf', 'NotIf')
       !
       CALL birth_vl_kz &
            ( fst_ckK, lst_ckK, act_ckK)
       !
       IF ( any_error( ) ) RETURN
       !
       act_ckK%key  = TRIM(key)
       act_ckK%wert = TRIM(wert)
       !
    CASE ('L_OneArray')
       !
       CALL read_logical &
            ( TRIM(key), TRIM(wert), act_key%key%L_OneArray)
       !
       IF ( any_error( ) ) RETURN
       !
    CASE ('L_Comment')
       !
       CALL read_logical &
            ( TRIM(key), TRIM(wert), act_key%key%L_Comment)
       !
       IF ( any_error( ) ) RETURN
       !
    END SELECT
    !
  END SUBROUTINE read_dickeyzeile
  !
  !! Liest die Zeilen eines Dictionary-PARAMETER-Blockes. <BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE read_dicparzeile &
       ( key, wert )
    !
    ! Formalparameter
    !! Schluesselwort
    CHARACTER (LEN=*)  , INTENT(IN   )   :: key
    !! Wert einer SchluesselwortZeile
    CHARACTER (LEN=*)  , INTENT(IN   )   :: wert
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='read_dicparzeile'
    !
    !
    SELECT CASE (TRIM(key))
       !
    CASE ('Type')
       !
       CALL read_partype &
            ( TRIM(key), TRIM(wert), act_par%par%Type )
       !
       IF ( any_error( ) ) RETURN
       !
    CASE ('L_Opt')
       !
       CALL read_logical &
            ( TRIM(key), TRIM(wert), act_par%par%L_Opt )
       !
       IF ( any_error( ) ) RETURN
       !
    CASE ('ReqIf', 'NotIf', 'CheckIfPar')
       !
       CALL birth_vl_kz &
            ( fst_ckP, lst_ckP, act_ckP )
       !
       IF ( any_error( ) ) RETURN
       !
       act_ckP%key  = TRIM(key)
       act_ckP%wert = TRIM(wert)
       !
    CASE ('L_Array')
       !
       CALL read_logical &
            ( TRIM(key), TRIM(wert), act_par%par%L_Array )
       !
       IF ( any_error( ) ) RETURN
       !
    CASE ('FixValue')
       !
       CALL birth_vl_kz &
            ( fst_fd, lst_fd, act_fd)
       !
       IF ( any_error( ) ) RETURN
       !
       act_fd%key  = TRIM(key)
       act_fd%wert = TRIM(wert)
       !
    CASE ('L_FileReq')
       !
       CALL read_logical &
            ( TRIM(key), TRIM(wert), act_par%par%L_FilReq)
       !
       IF ( any_error( ) ) RETURN
       !
    CASE ('L_FileNew')
       !
       CALL read_logical &
            ( TRIM(key), TRIM(wert), act_par%par%L_FilNew)
       !
       IF ( any_error( ) ) RETURN
       !
    END SELECT
    !
  END SUBROUTINE read_dicparzeile
  !
  !! Liest Datentyp eines Parameters.<BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE read_partype &
       ( key, wert, type )
    !
    ! Formalparameter
    !! Schluesselwort einer Key-Zeile
    CHARACTER (LEN=*)  , INTENT(IN)      :: key
    !! Wert einer Key-Zeile (d.h. hinter Gleichheitszeichen)
    CHARACTER (LEN=*)  , INTENT(IN)      :: wert
    !! DatenTyp des Parameters<BR>
    !! [INT,REAL,DOUBLE,CHAR,LOG,FILE,DATE,INCR]
    CHARACTER (LEN=*)  , INTENT(OUT)     :: type
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='read_partype'
    CHARACTER (LEN=3)             :: c_parpos
    !
    !
    type = REPEAT(' ', LEN(type))
    !
    ! Fallunterscheidung Datentyp
    !
    SELECT CASE (TRIM(wert))
    CASE('INT')
       !
       type = 'INT  '
       !
    CASE('REAL')
       !
       type = 'REAL '
       !
    CASE('DOUBLE')
       !
       type = 'DOUBLE'
       !
    CASE('CHAR')
       !
       type = 'CHAR '
       !
    CASE('LOG')
       !
       type = 'LOG  '
       !
    CASE('FILE')
       !
       type = 'FILE '
       !
    CASE('DATE')
       !
       type = 'DATE '
       !
    CASE('INCR')
       !
       type = 'INCR '
       !
    CASE DEFAULT
       !
       WRITE(c_parpos(1:3),'(I3)') act_par%par%ParPos
       c_parpos = ADJUSTL(c_parpos)
       !
       ! Fehler -780 : ungueltiger Wert fuer Parameter-Datentyp
       !             erlaubt = [INT,REAL,DOUBLE,CHAR,LOG,FILE,DATE,INCR]
       !
       CALL setup_error_act ( all_errors(:), -780, c_upname, c_modname )
       CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
       CALL setup_error_act ( '<pos_nr>', TRIM(c_parpos) )
       CALL setup_error_act ( '<keyname>', TRIM(act_key%key%Name) )
       CALL setup_error_act ( '<blockname>', TRIM(act_block%block%Name) )
       CALL setup_error_act ( '<akt_wert>', TRIM(wert) )
       !
       RETURN
       !
    END SELECT
    !
  END SUBROUTINE read_partype
  !
END MODULE m_diction_ein
!  ----------------------------------------------------------------------------
!  Ende des Source-Codes des Moduls
!  ----------------------------------------------------------------------------

