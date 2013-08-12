!!m <H2>M_DIC_CHECKS</h2>
!! @author Susanne Spohr
!! @version 1.4 vom 02/03/04, Quellcode: mod_m_dic_checks.f90
!! <HR>
!! <one line to give the program's name and an idea of what it does> <BR>
!! <HR>
!! <H3>Copyright-Hinweis</H3>
!!                                                                   <BR>
!! Copyright (C) 2002 <A HREF="http://www.hamburg.baw.de/">Bundesanstalt f&uuml;r Wasserbau</A> <BR>
!!                                                                   <BR>
!! <HR>
!! <H3>Entwicklungsgeschichte des Modules</H3>
!! 01.01 : 2002-05-29 : Susanne Spohr : Re-Engineering des Moduls mod_dic_checks.f90 der Library dictionary <BR>
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Moduls</H3>
!! Krams der im Zusammenhang mit den in der Dictionary
!! Datei zu vereinbarenden Checks : ReqIf,NotIf,
!! CheckIfPar steht.
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
MODULE m_dic_checks
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
       setup_error_act, &
       specific_error
  !
  ! Basismodul zum Rechnen mit Zeitinkrementen
  USE b_time
  ! Basismodul zum Rechnen mit absoluten Zeitangaben
  USE b_datetime
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
       all_errors,  & ! Fehlermeldunegn
       dicfile_name   ! Name der Dictionary-Datei
  !
  ! [B.2] weitere Module die zum Package "dictionary" gehoeren
  !
  USE m_stdat_types, ONLY : &
       ! Parameter
       key_len, &
       ! Typdefinitionen
       t_check, &
       t_obj, &
       t_block, &
       t_vl_kz, &
       ! Routinen
       dealloc_vl_kz, &
       init_objekt
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
  INTERFACE transfer_CKs
     MODULE PROCEDURE transfer_CKs_d ! 
  END INTERFACE
  !
  !! ...
  INTERFACE Checks_Pruefen
     MODULE PROCEDURE Checks_Pruefen_d ! 
  END INTERFACE
  !
!>WIN-NT:  INTERFACE OPERATOR(==)
!>WIN-NT:     MODULE PROCEDURE eq_datetime_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE eq_datetime_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE eq_datetime_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE eq_datetime_1_1  ! Vektor / Vektor
!>WIN-NT:     MODULE PROCEDURE eq_time_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE eq_time_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE eq_time_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE eq_time_1_1  ! Vektor / Vektor
!>WIN-NT:  END INTERFACE
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
  PUBLIC :: transfer_CKs        ! ...
  PUBLIC :: Checks_Pruefen      ! ...
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
  CHARACTER (LEN=31), PARAMETER :: c_modname      = 'm_dic_checks' ! 
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
  !! Die Checkanforderungen aus der uebergebenen verketteten
  !! Liste werden in die entsprechenden KomponentenFelder
  !! des jeweiligen Dateielementes sortiert. <BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE transfer_CKs_d &
       ( act_ck, fst_ck, lst_ck, vgl_ck, block, blockname, &
        ReqIf, NotIf, CheckIf)
    !
    ! Formalparameter
    !! akt. Element verkett. Liste mit Check-Anforderungen
    TYPE (t_vl_kz)     , POINTER        :: act_ck
    !! erstes Element verkett. Liste mit Check-Anforderungen
    TYPE (t_vl_kz)     , POINTER        :: fst_ck
    !! letztes Element verkett. Liste mit Check-Anforderungen
    TYPE (t_vl_kz)     , POINTER        :: lst_ck
    !! Vgl.-Element verkett. Liste mit Check-Anforderungen
    TYPE (t_vl_kz)     , POINTER        :: vgl_ck
    !! Variable mit Beschreibung des Dictionary-Blockes
    TYPE (t_block)     , INTENT(IN   )  :: block
    !! Name des Dictionary-Blockes
    CHARACTER (LEN=*)  , INTENT(IN   )  :: blockname
    !! Feld mit ReqIf-Anforderungen des Dateielementes
    TYPE (t_check) , DIMENSION(:) , POINTER           :: ReqIf
    !! Feld mit NotIf-Anforderungen des Dateielementes
    TYPE (t_check) , DIMENSION(:) , POINTER           :: NotIf
    !! Feld mit CheckIfPar-Anforderungen des Dateielementes
    TYPE (t_check) , DIMENSION(:) , POINTER, OPTIONAL :: CheckIf
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='transfer_CKs_d'
    !
    INTEGER                            :: i
    !
    !
    ! Check : Enthaelt der Block eine Check-Zeile doppelt ?
    !
    act_ck => fst_ck
    !
    outer: DO
       !
       IF ( .NOT. ASSOCIATED(act_ck) ) EXIT outer
       !
       vgl_ck => act_ck%next
       !
       inner: DO 
          !
          IF ( .NOT. ASSOCIATED(vgl_ck) ) EXIT inner
          !
          IF ( TRIM(act_ck%key ) .EQ. TRIM(vgl_ck%key ) .AND. &
               TRIM(act_ck%wert) .EQ. TRIM(vgl_ck%wert)        ) THEN
             !
             ! Fehler -1010 : Ein Block der Dictionary-Datei enthaelt eine Check-Zeile doppelt !
             !
             CALL setup_error_act ( all_errors(:), -1010, c_upname, c_modname )
             CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
             CALL setup_error_act ( '<blockname>', TRIM(blockname) )
             CALL setup_error_act ( '<akt_key>', TRIM(act_ck%key) )
             CALL setup_error_act ( '<akt_wert>', TRIM(act_ck%wert) )
             !
             RETURN
             !
          END IF
          !
          vgl_ck => vgl_ck%next
          !
       END DO inner
       !
       act_ck => act_ck%next
       !
    END DO outer
    !
    ! Dynamische KomponentenFelder vom Typ <t_check> allokieren und Daten aus 
    ! der Einleseliste uebertragen
    !
    DO i = 1, SIZE(block%Key)
       !
       IF ( block%Key(i)%ZeilAnz(1) .GT. 0  ) THEN
          !
          SELECT CASE (TRIM(block%Key(i)%Name))
             !
          CASE('ReqIf')
             !
             CALL alloc_check_feld &
                  ( act_ck, fst_ck, lst_ck, TRIM(blockname), &
                  'ReqIf', block%Key(i)%ZeilAnz(1), ReqIf )
             !
             IF ( any_error( ) ) RETURN
             !
          CASE('NotIf')
             !
             CALL alloc_check_feld &
                  ( act_ck, fst_ck, lst_ck, TRIM(blockname), &
                  'NotIf', block%Key(i)%ZeilAnz(1), NotIf )
             !
             IF ( any_error( ) ) RETURN
             !
          CASE ('CheckIfPar')
             !
             CALL alloc_check_feld &
                  ( act_ck, fst_ck, lst_ck, TRIM(blockname), &
                  'CheckIfPar', block%Key(i)%ZeilAnz(1), CheckIf )
             !
             IF ( any_error( ) ) RETURN
             !
          END SELECT
          !
       END IF
       !
    END DO
    !
    !   Verkettete Liste mit Checks loeschen
    !
    CALL dealloc_vl_kz ( act_ck, fst_ck, lst_ck, vgl_ck )
    !
  END SUBROUTINE transfer_CKs_d
  !
  !! Saemtliche in einer Dictionary-Datei aufgefuehrten
  !! Check-Anforderungen werden hinsichtlich ihres Aufbaus 
  !! und ihrer Interpretierbarkeit ueberprueft.
  !! Ausserdem wird gecheckt, ob sich die angeforderten
  !! ReqIf-/NotIf-Bedingungen nicht gegenseitig ausschliessen.<BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE Checks_Pruefen_d &
       ( bloecke )
    !
    ! Formalparameter
    !! Feld mit Informationen der Gesamt-Dictionary-Datei
    TYPE (t_block  ) , DIMENSION(:)  , POINTER         :: bloecke
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='Checks_Pruefen_d'
    !
    INTEGER                               :: i, j, k
    CHARACTER (LEN=3)                     :: c_parpos
    CHARACTER (LEN=key_len)               :: dicbl_name
    !
    TYPE(t_obj)                           :: objekt
    !
    !
    ! Schleife zum Auffinden saemtlicher Check-Anforederungen
    ! { ReqIf, NotIf, CheckIfPar }
    !
    DO i = 1, SIZE(bloecke)
       !
       ! Namen des Dictionary-BLOCK-Blockes generieren
       !
       dicbl_name = REPEAT(' ', LEN(dicbl_name))
       dicbl_name = TRIM(bloecke(i)%Name)
       !
       ! Objekt-Beschreibung generieren
       !
       CALL init_objekt &
            ( objekt )
       !
       IF ( any_error( ) ) RETURN
       !
       objekt%typ(1:5) = 'BLOCK'
       objekt%block    = TRIM(bloecke(i)%Name)
       objekt%bl_nr    = i
       !
       ! Ggf.: ReqIf-Anforderungen checken
       !
       IF ( ASSOCIATED(bloecke(i)%ReqIf) ) THEN
          !
          CALL Check_ReqIfNotIf &
               ( dicbl_name, objekt, bloecke(i)%ReqIf, &
               'ReqIf', bloecke )
          !
          IF ( any_error( ) ) RETURN
          !
       END IF
       !
       ! Ggf.: NotIf-Anforderungen checken 
       !
       IF ( ASSOCIATED(bloecke(i)%NotIf) ) THEN
          !
          CALL Check_ReqIfNotIf &
               ( dicbl_name, objekt, bloecke(i)%NotIf, &
               'NotIf', bloecke )
          !
          IF ( any_error( ) ) RETURN
          !
       END IF
       !
       ! Ggf.: checken, ob die zu einem Objekt gehoerenden
       !       ReqIf|NotIf-Anforderungen sich nicht gegenseitig ausschliessen
       !
       IF ( ASSOCIATED(bloecke(i)%ReqIf)  .OR. &
            ASSOCIATED(bloecke(i)%NotIf)       ) THEN
          !
          CALL ReqIfNotIf_Widerspruch &
               ( dicbl_name, bloecke(i)%ReqIf, &
               bloecke(i)%NotIf, bloecke )
          !
          IF ( any_error( ) ) RETURN
          !
       END IF
       !
       DO j = 1, SIZE(bloecke(i)%Key)
          !
          ! Namen des Dictionary-KEY-Blockes generieren
          !
          dicbl_name = REPEAT(' ', LEN(dicbl_name))
          dicbl_name = 'KEY '//TRIM(bloecke(i)%Key(j)%Name)// &
               ' in Block '//TRIM(bloecke(i)%Name)
          !
          ! Objekt-Beschreibung generieren
          !
          CALL init_objekt &
               ( objekt)
          !
          IF ( any_error( ) ) RETURN
          !
          objekt%typ(1:3) = 'KEY'
          objekt%block    = TRIM(bloecke(i)%Name)
          objekt%key      = TRIM(bloecke(i)%Key(j)%Name)
          objekt%bl_nr    = i
          objekt%key_nr   = j
          !
          ! Ggf.: ReqIf-Anforderungen checken
          !
          IF ( ASSOCIATED(bloecke(i)%Key(j)%ReqIf) ) THEN
             !
             CALL Check_ReqIfNotIf &
                  ( dicbl_name, objekt, bloecke(i)%Key(j)%ReqIf, &
                  'ReqIf', bloecke )
             !
             IF ( any_error( ) ) RETURN
             !
          END IF
          !
          ! Ggf.: NotIf-Anforderungen checken 
          !
          IF ( ASSOCIATED(bloecke(i)%Key(j)%NotIf) ) THEN
             !
             CALL Check_ReqIfNotIf &
                  ( dicbl_name, objekt, bloecke(i)%Key(j)%NotIf, &
                  'NotIf', bloecke )
             !
             IF ( any_error( ) ) RETURN
             !
          END IF
          !
          ! Ggf.: checken, ob die zu einem Objekt gehoerenden
          !       ReqIf|NotIf-Anforderungen sich nicht gegenseitig ausschliessen
          !
          IF ( ASSOCIATED(bloecke(i)%Key(j)%ReqIf)  .OR. &
               ASSOCIATED(bloecke(i)%Key(j)%NotIf)       ) THEN
             !
             CALL ReqIfNotIf_Widerspruch &
                  ( dicbl_name, bloecke(i)%Key(j)%ReqIf, &
                  bloecke(i)%Key(j)%NotIf, bloecke )
             !
             IF ( any_error( ) ) RETURN
             !
          END IF
          !
          DO k = 1, SIZE(bloecke(i)%Key(j)%Par)
             !
             ! Namen des Dictionary-PARAMETER-Blockes generieren
             !
             c_parpos = REPEAT(' ', LEN(c_parpos))
             WRITE(c_parpos(1:3),'(I3)') bloecke(i)%Key(j)%Par(k)%ParPos
             c_parpos = ADJUSTL(c_parpos)
             !
             dicbl_name = REPEAT(' ', LEN(dicbl_name))
             dicbl_name = 'PAR '//TRIM(c_parpos)//', KEY '//&
                  TRIM(bloecke(i)%Key(j)%Name)//&
                  ' in Block '//TRIM(bloecke(i)%Name)
             !
             ! Objekt-Beschreibung generieren
             !
             CALL init_objekt &
                  ( objekt )
             !
             IF ( any_error( ) ) RETURN
             !
             objekt%typ(1:3) = 'PAR'
             objekt%block    = TRIM(bloecke(i)%Name)
             objekt%key      = TRIM(bloecke(i)%Key(j)%Name)
             objekt%bl_nr    = i
             objekt%key_nr   = j
             objekt%par_nr   = k
             !
             ! Ggf.: ReqIf-Anforderungen checken
             !
             IF ( ASSOCIATED(bloecke(i)%Key(j)%Par(k)%ReqIf) ) THEN
                !
                CALL Check_ReqIfNotIf &
                     ( dicbl_name, objekt, &
                     bloecke(i)%Key(j)%Par(k)%ReqIf, &
                     'ReqIf', bloecke )
                !
                IF ( any_error( ) ) RETURN
                !
             END IF
             !
             ! Ggf.: NotIf-Anforderungen checken 
             !
             IF ( ASSOCIATED(bloecke(i)%Key(j)%Par(k)%NotIf) ) THEN
                !
                CALL Check_ReqIfNotIf &
                     ( dicbl_name, objekt, &
                     bloecke(i)%Key(j)%Par(k)%NotIf, &
                     'NotIf', bloecke )
                !
                IF ( any_error( ) ) RETURN
                !
             END IF
             !
             ! Ggf.: checken, ob die zu einem Objekt gehoerenden
             !       ReqIf|NotIf-Anforderungen sich nicht gegenseitig
             !       ausschliessen
             !
             IF ( ASSOCIATED(bloecke(i)%Key(j)%Par(k)%ReqIf)  .OR. &
                  ASSOCIATED(bloecke(i)%Key(j)%Par(k)%NotIf)       ) THEN
                !
                CALL ReqIfNotIf_Widerspruch &
                     ( dicbl_name, bloecke(i)%Key(j)%Par(k)%ReqIf, &
                     bloecke(i)%Key(j)%Par(k)%NotIf, bloecke )
                !
                IF ( any_error( ) ) RETURN
                !
             END IF
             !
             ! CheckIfPar-Anforderungen ueberpruefen
             !
             IF ( ASSOCIATED(bloecke(i)%Key(j)%Par(k)%CheckIf) ) THEN
                !
                CALL Check_CheckIfPar &
                     ( TRIM(dicbl_name), TRIM(bloecke(i)%Name), &
                     TRIM(bloecke(i)%Key(j)%Name), &
                     bloecke(i)%Key(j)%Par(k), bloecke )
                !
                IF ( any_error( ) ) RETURN
                !
             END IF
             !
          END DO   ! k = 1, SIZE(bloecke(i)%Key(j)%Par)
          !
       END DO   ! j = 1, SIZE(bloecke(i)%Key)
       !
    END DO   ! i = 1, SIZE(bloecke)
    !
  END SUBROUTINE Checks_Pruefen_d
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
  ! >>> modulspezifische PRIVATE-Methoden <<< [ERR_NO << 0]
  ! ----------------------------------------------------------------------
  !
  !! Ueberprueft den korrekten Aufbau von Check-Anforderungen
  !! vom Typ ReqIf/NotIf !<BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE Check_ReqIfNotIf &
       ( dicbl_name, objekt, mycheck, ck_art, bloecke )
    !
    ! Formalparameter
    !! Name des Dictionary-Blockes (fuer Fehlermeldung)
    CHARACTER (LEN=*)                , INTENT(IN   )   :: dicbl_name
    !! Objekt dessen ReqIf|NotIf-Anforderung geprueft wird
    TYPE (t_obj)                     , INTENT(IN   )   :: objekt
    !! Feld mit ReqIf's oder NotIf's
    TYPE (t_check) , DIMENSION(:)    , POINTER         :: mycheck
    !! Art des Checks { ReqIf oder NotIf }
    CHARACTER (LEN=*)                , INTENT(IN   )   :: ck_art
    !! Feld mit Informationen der Gesamt-Dictionary-Datei
    TYPE (t_block  ) , DIMENSION(:)  , POINTER         :: bloecke
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='Check_ReqIfNotIf'
    !! Fehlerkennung
    INTEGER        :: ierr=0
    !
    INTEGER                           :: i
    INTEGER                           :: i_bl, i_key, i_par
    !
    !
    ! einige Fehlermeldungen :
    !
    IF ( .NOT. ASSOCIATED(mycheck) ) THEN
       !
       ! Fehler -1020 : Fuer das Objekt existiert keine Check-Anforderung !
       !
       CALL setup_error_act ( all_errors(:), -1020, c_upname, c_modname )
       CALL setup_error_act ( '<objekt_typ>', TRIM(objekt%typ) )
       CALL setup_error_act ( '<check_art>', TRIM(ck_art) )
       !
       RETURN
       !
    END IF
    !
    IF ( .NOT. ASSOCIATED(bloecke) ) THEN
       !
       ! Fehler -1030 : Das Parameter-Pointerfeld <bloecke> ist .not. associated !
       !
       CALL setup_error_act ( all_errors(:), -1030, c_upname, c_modname )
       !
       RETURN
       !
    END IF
    !
    IF ( ck_art .NE. 'ReqIf'  .AND.  ck_art .NE. 'NotIf' ) THEN
       !
       ! Fehler -1040 : Parameter <ck_art> hat unerlaubten Wert  !
       !
       CALL setup_error_act ( all_errors(:), -1040, c_upname, c_modname )
       !
       RETURN
       !
    END IF
    !
    ! Vergleichs-Objekte der ReqIf|NotIf-Anforderungen pruefen
    !
    DO i = 1, SIZE(mycheck)
       !
       ! Check : - ist Vergleichs-Objekt ungleich dem Objekt zu welchem 
       !           die Check-Anforderung gehoert
       !         - ist Vergleichs-Objekt in Dictionary-Datei definiert
       !   und : - Ausfuellen der Blanko-Adressen
       !
       CALL Objekt_pruefen &
            ( mycheck(i)%objekt, objekt, dicbl_name, &
            TRIM(ck_art), bloecke ) 
       !
       IF ( any_error( ) ) RETURN
       !
       ! Adress-Nummern des Vergleichsobjektes
       !
       i_bl   = mycheck(i)%objekt%bl_nr
       i_key  = mycheck(i)%objekt%key_nr
       i_par  = mycheck(i)%objekt%par_nr
       !
       ! Pruefe : Ist Vergleichs-Objekt eindeutig zu identifizieren ?
       !
       SELECT CASE (mycheck(i)%objekt%typ)
       CASE ('PAR','KEY')
          !
          ! Vergleichsobjekt (Typ : Key, Par) darf nicht in einem Eingabeblock
          ! liegen, fuer den gilt : L_Single = False ; da es dann nicht mehr
          ! eindeutig zu identifizieren ist.
          ! Ausnahme : das VGL-Objekt befindet sich im selben Eingabeblock, wie
          !            der durchzufuehrenden Check !
          !
          IF ( .NOT. bloecke(i_bl)%L_Single ) THEN
             !
             IF ( objekt%bl_nr .NE. i_bl ) THEN
                !
                ! Fehler -1090 : Ein Block der Dictionary-Datei enthaelt eine fehlerhafte Check-Zeile !
                !              Das Vergleichs-Objekt kann nicht eindeutig identifiziert werden,
                !              da fuer seinen Eingabeblock-Typ gilt : L_Single = False !
                !
                CALL setup_error_act ( all_errors(:), -1090, c_upname, c_modname )
                CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
                CALL setup_error_act ( '<blockname>', TRIM(dicbl_name) )
                CALL setup_error_act ( '<check_art>', TRIM(ck_art) )
                !
                RETURN
                !
             END IF
             !
          END IF
          !
          SELECT CASE (mycheck(i)%objekt%typ)
          CASE ('PAR')
             !
             ! Vergleichsobjekt (Typ: Par) darf nicht in einer Keyzeile
             ! liegen, fuer die gilt : L_Single = False ; da es dann nicht
             ! mehr eindeutig zu identifizieren ist.
             ! Ausnahme : das VGL-Objekt befindet sich in derselben
             !            KeyZeile, wie der durchzufuehrenden Check !
             !
             IF ( .NOT. bloecke(i_bl)%Key(i_key)%L_Single ) THEN
                !
                IF ( objekt%bl_nr  .NE. i_bl  .OR. &
                     objekt%key_nr .NE. i_key         ) THEN
                   !
                   ! Fehler -1080 : Ein Block der Dictionary-Datei enthaelt eine fehlerhafte Check-Zeile !
                   !              Der Vergleichs-Parameter kann nicht eindeutig identifiziert werden,
                   !              da fuer seine Keyzeile gilt : L_Single = False !
                   !
                   CALL setup_error_act ( all_errors(:), -1080, c_upname, c_modname )
                   CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
                   CALL setup_error_act ( '<blockname>', TRIM(dicbl_name) )
                   CALL setup_error_act ( '<check_art>', TRIM(ck_art) )
                   !
                   RETURN
                   !
                END IF
                !
             END IF
             !
          END SELECT
          ! 
       END SELECT
       !
       ! Falls in Check-Anforderung direkt angegebener Vergleichswert vorhanden
       ! ist, pruefen ob : 
       !   - Datentyp mit dem des Vergleichsobjektes uebereinstimmt ?
       !   - Nur ein einzelner Wert spezifiziert wurde ?    
       !
       IF ( mycheck(i)%oper .EQ. 'EQ'  .OR. &         
            mycheck(i)%oper .EQ. 'NE'        ) THEN   
          !
          CALL Direktwerte_pruefen &
               ( bloecke(i_bl)%Key(i_key)%Par(i_par)%Type, &
               dicbl_name, mycheck(i) )
          !
          IF ( any_error( ) ) RETURN
          !
          ! Das Vergleichs-Objekt muss als Einzelwert eindeutig identifizierbar
          ! sein !
          !
          ! A : Vergleichs-Objekt darf keine Feldgroesse sein
          !     ( Parameter%L_Array = True )
          !
          IF ( bloecke(i_bl)%Key(i_key)%Par(i_par)%L_Array ) THEN
             !
             ! Fehler -1070 : Ein Block der Dictionary-Datei enthaelt eine fehlerhafte Check-Zeile !
             !              Der Vergleichs-Parameter ist unzulaessigerweise eine Feldgroesse !
             CALL setup_error_act ( all_errors(:), -1070, c_upname, c_modname )
             CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
             CALL setup_error_act ( '<blockname>', TRIM(dicbl_name) )
             CALL setup_error_act ( '<check_art>', TRIM(ck_art) )
             !
             RETURN
             !        
          END IF
          !
       END IF   ! check(i)%oper = .EQ. oder .NE.
       !
       ! Check : Wurde VergleichsObjekt fuer 'EXIST','NOTEXIST' -Operation
       !         ueberhaupt optional vereinbart
       !
       IF ( mycheck(i)%oper .EQ. 'EXIST'    .OR. &         
            mycheck(i)%oper .EQ. 'NOTEXIST'        ) THEN   
          !          
          SELECT CASE (mycheck(i)%objekt%typ)
          CASE('BLOCK')
             IF ( .NOT. bloecke(i_bl)%L_Opt ) ierr = 5
          CASE('KEY')
             IF ( .NOT. bloecke(i_bl)%Key(i_key)%L_Opt ) ierr = 5
          CASE('PAR')
             IF ( .NOT. bloecke(i_bl)%Key(i_key)%Par(i_par)%L_Opt ) &
                  ierr = 5
          END SELECT
          !
          IF( ierr .EQ. 5 ) THEN
             !
             ! Fehler -1060 : Ein Block der Dictionary-Datei enthaelt eine fehlerhafte Check-Zeile !
             !              Das Vergleichs-Objekt wurde nicht als optional vereinbart !
             !
             CALL setup_error_act ( all_errors(:), -1060, c_upname, c_modname )
             CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
             CALL setup_error_act ( '<blockname>', TRIM(dicbl_name) )
             CALL setup_error_act ( '<check_art>', TRIM(ck_art) )
             !
             RETURN
             !
          END IF
          !
       END IF
       !
       ! Check : Die Existenz eines Objektes darf nicht vom Zustand eines 
       !         Objektes abhaengen, welches in derselben Adresslinie steht
       !
       IF ( mycheck(i)%objekt%typ .EQ. 'BLOCK'  .OR. &
            objekt%typ          .EQ. 'BLOCK'       ) THEN
          !
          IF ( objekt%bl_nr .EQ. mycheck(i)%objekt%bl_nr ) THEN
             !
             ierr = 4
             !
          END IF
          !
       ELSE IF ((mycheck(i)%objekt%typ .EQ. 'KEY' .AND. &
            objekt%typ .EQ. 'PAR')  .OR. &
            (mycheck(i)%objekt%typ .EQ. 'PAR' .AND. &
            objekt%typ .EQ. 'KEY')                 )  THEN
          !
          ! ... Objekte gleicher Art haben nur dieselbe Adresslinie, wenn es
          !     sich um dasselbe Objekt handelt; dieser Fall wurde bereits
          !     ausgeschlossen
          !
          IF ( objekt%bl_nr  .EQ. mycheck(i)%objekt%bl_nr  .AND. &
               objekt%key_nr .EQ. mycheck(i)%objekt%key_nr       ) THEN
             !
             ierr = 4
             !
          END IF
          !
       END IF
       !
       IF ( ierr .EQ. 4 ) THEN
          !
          ! Fehler -1050 : Ein Block der Dictionary-Datei enthaelt eine fehlerhafte Check-Zeile !
          !              Existenz eines Objektes haengt vom Zustand eines anderen Objektes
          !              derselben "Adresslinie" ab !
          !
          CALL setup_error_act ( all_errors(:), -1050, c_upname, c_modname )
          CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
          CALL setup_error_act ( '<blockname>', TRIM(dicbl_name) )
          CALL setup_error_act ( '<check_art>', TRIM(ck_art) )
          !
          RETURN
          !
       END IF
       !
    END DO   ! i = 1, SIZE(check)
    !
  END SUBROUTINE Check_ReqIfNotIf
  !
  !! Prueft, ob die zu einem Dateielement [BLOCK,KEY,PAR]
  !! gehoerigen ReqIf- und NotIf-Bedingungen sich nicht 
  !! gegenseitig ausschliessen.<BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE ReqIfNotIf_Widerspruch &
       ( dicbl_name, ReqIf, NotIf, bloecke )
    !
    ! USE-Statements :
    !
    ! Formalparameter
    !! Name des Dictionary-Blockes (fuer Fehlermeldung)
    CHARACTER (LEN=*)                , INTENT(IN   )   :: dicbl_name
    !! Feld mit ReqIf-Anforderungen eines Objektes
    TYPE (t_check) , DIMENSION(:)    , POINTER         :: ReqIf
    !! Feld mit NotIf-Anforderungen eines Objektes
    TYPE (t_check) , DIMENSION(:)    , POINTER         :: NotIf
    !! Feld mit Informationen der Gesamt-Dictionary-Datei
    TYPE (t_block  ) , DIMENSION(:)  , POINTER         :: bloecke
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='ReqIfNotIf_Widerspruch'
    !
    INTEGER                           :: i, j, i_bl, i_key, i_par
    !
    TYPE(t_check) , POINTER     :: check_A
    TYPE(t_check) , POINTER     :: check_B
    !
    LOGICAL                     :: l_req, l_not
    !
    INTEGER                     :: anz
    !
    LOGICAL                     :: l_wider
    LOGICAL                     :: l_gleich
    LOGICAL                     :: l_kontra
    LOGICAL                     :: l_ck_wert
    !
    !! Hilfsvariable A fuer den Vergleich von Zeitangaben
    TYPE (t_datetime)           :: Date_A
    !! Hilfsvariable B fuer den Vergleich von Zeitangaben
    TYPE (t_datetime)           :: Date_B
    !! Hilfsvariable A fuer den Vergleich von Zeitinkrementen
    TYPE (t_time)               :: Incr_A
    !! Hilfsvariable B fuer den Vergleich von Zeitinkrementen
    TYPE (t_time)               :: Incr_B
    !
    !
    ! einige Fehlermeldungen :
    !
    IF ( .NOT. ASSOCIATED(bloecke) ) THEN
       !
       ! Fehler -1100 : Das Parameter-Pointerfeld <bloecke> ist .not. associated !
       !
       CALL setup_error_act ( all_errors(:), -1100, c_upname, c_modname )
       !
       RETURN
       !
    END IF
    !
    ! Welche Check-Arten sind vorhanden
    !
    l_req = .False.
    l_not = .False.
    !
    IF ( ASSOCIATED(ReqIf)  .AND.  ASSOCIATED(NotIf) ) THEN
       !
       anz = SIZE(ReqIf) + SIZE(NotIf) 
       !
       l_req = .True.
       l_not = .True.
       !
    ELSE IF ( ASSOCIATED(ReqIf) ) THEN
       !
       anz = SIZE(ReqIf)
       !
       l_req = .True.
       !
    ELSE IF ( ASSOCIATED(NotIf) ) THEN
       !
       anz = SIZE(NotIf) 
       !
       l_not = .True.
       !
    END IF
    !
    ! Widersprechen sich zwei Check-Anforderungen
    !
    l_wider = .False.
    !
    IF ( anz .GT. 1 ) THEN
       !
       DO i = 1, anz -1
          !
          IF ( l_wider ) EXIT
          !
          NULLIFY (check_A)
          !
          IF ( l_req .AND. l_not ) THEN
             !
             IF ( i .LE. SIZE(ReqIf) ) THEN
                !
                check_A => ReqIf(i)
                !
             ELSE
                !
                check_A => NotIf(i-SIZE(ReqIf))
                !
             END IF
             !
          ELSE IF (l_req) THEN
             !
             check_A => ReqIf(i)
             !
          ELSE IF (l_not) THEN
             !
             check_A => NotIf(i)
             !
          END IF
          !
          DO j = i + 1, anz
             !
             IF ( l_wider ) EXIT
             !
             NULLIFY (check_B)
             !
             IF ( l_req .AND. l_not ) THEN
                !
                IF ( j .LE. SIZE(ReqIf) ) THEN
                   !
                   check_B => ReqIf(j)
                   !
                ELSE
                   !
                   check_B => NotIf(j-SIZE(ReqIf))
                   !
                END IF
                !
             ELSE IF (l_req) THEN
                !
                check_B => ReqIf(j)
                !
             ELSE IF (l_not) THEN
                !
                check_B => NotIf(j)
                !
             END IF
             !
             ! Check : Sind die Vergleichsobjekte der Checks identisch ?
             !
             CALL Objekt_Vergleich &
                  ( check_A%objekt, check_B%objekt, l_gleich )
             !
             IF ( any_error( ) ) RETURN
             !
             ! Wenn identisches Vergleichsobjekt :
             !
             IF ( l_gleich ) THEN
                !
                ! Operation-Vergleich
                !
                l_kontra = .False.
                !
                IF ( l_req .AND. l_not ) THEN
                   !
                   IF ( i .LE. SIZE(ReqIf)  .AND.  j .GT. SIZE(ReqIf)) THEN
                      !
                      l_kontra = .True.
                      !
                   END IF
                   !
                END IF
                !
                l_ck_wert = .False.
                !
                IF ( l_kontra ) THEN
                   !
                   IF ( check_A%oper .EQ. check_B%oper ) THEN
                      !
                      SELECT CASE (check_A%oper)
                      CASE('EXIST','NOTEXIST')
                         !
                         l_wider   = .True.
                         !
                      CASE('EQ','NE')
                         !
                         l_ck_wert = .True.
                         !
                      END SELECT
                      !
                   END IF
                   !
                ELSE
                   !
                   IF ( (check_A%oper .EQ. 'EXIST' .AND. &
                        check_B%oper .EQ. 'NOTEXIST')    .OR. &
                        (check_A%oper .EQ. 'NOTEXIST' .AND. &
                        check_B%oper .EQ. 'EXIST')           ) THEN
                      !
                      l_wider   = .True.
                      !
                   ELSE IF ( (check_A%oper .EQ. 'EQ' .AND. &
                        check_B%oper .EQ. 'NE')    .OR. &
                        (check_A%oper .EQ. 'NE' .AND. &
                        check_B%oper .EQ. 'EQ')         ) THEN
                      !
                      l_ck_wert = .True.
                      !
                   END IF
                   !
                END IF   ! l_kontra
                !
                ! Direktwerte vergleichen, falls noetig
                !
                IF ( l_ck_wert ) THEN
                   !
                   i_bl   = check_A%objekt%bl_nr
                   i_key  = check_A%objekt%key_nr
                   i_par  = check_A%objekt%par_nr
                   !
                   SELECT CASE (bloecke(i_bl)%Key(i_key)%Par(i_par)%Type)
                   CASE('INT')
                      IF (check_A%wert%int(1) .EQ. check_B%wert%int(1)) &
                           l_wider = .True.
                      !
                   CASE('REAL')
                      IF (check_A%wert%real(1) .EQ. check_B%wert%real(1)) &
                           l_wider = .True.
                      !
                   CASE('LOG')
                      !
                      IF ( check_A%wert%log(1) .AND. &
                           check_B%wert%log(1)) l_wider = .True.
                      !
                      IF ( .NOT. check_A%wert%log(1) .AND. &
                           .NOT. check_B%wert%log(1)) l_wider = .True.
                      !
                   CASE('CHAR','FILE')
                      IF (check_A%wert%char(1) .EQ. check_B%wert%char(1)) &
                           l_wider = .True.
                      !
                   CASE('DATE')
                      !
                      Date_A = string_to_datetime ( check_A%wert%char(1) )
                      Date_B = string_to_datetime ( check_B%wert%char(1) )
                      !
                      IF ( Date_A == Date_B ) l_wider = .True.
                      !
                   CASE('INCR')
                      !
                      Incr_A = string_to_time ( check_A%wert%char(1) )
                      Incr_B = string_to_time ( check_B%wert%char(1) )
                      !
                      IF ( Incr_A == Incr_B ) l_wider = .True.
                      !
                   CASE('DOUBLE')
                      !
                      IF ( check_A%wert%doub(1) .EQ. &
                           check_B%wert%doub(1) ) l_wider = .True.
                      !
                   END SELECT
                   !
                END IF
                !
             END IF   ! l_gleich
             !
             IF ( l_wider ) THEN
                !
                ! Fehler -1110 : ReqIf|NotIf-Zeilen schliessen sich gegenseitig aus !
                !
                CALL setup_error_act ( all_errors(:), -1110, c_upname, c_modname )
                CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
                CALL setup_error_act ( '<blockname>', TRIM(dicbl_name) )
                !
                RETURN
                !
             END IF
             !
          END DO   ! j = i + 1, anz
          !
       END DO   ! i = 1, anz -1
       !
    END IF   ! ( anz .GT. 1 )
    !
  END SUBROUTINE ReqIfNotIf_Widerspruch
  !
  !! Prueft, ob Aufbau von CheckIfPar-Anforderungen
  !! korrekt ist.<BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE Check_CheckIfPar &
       ( dicbl_name, block, key, par, bloecke )
    !
    ! USE-Statements
    !
    USE m_stdat_types, ONLY : &
         ! Typdefinitionen
         t_par
    !
    ! Formalparameter
    !! Name des Dictionary-Blockes (fuer Fehlermeldung)
    CHARACTER (LEN=*)                , INTENT(IN   )   :: dicbl_name
    !! Name des Eingabeblockes des Parameters
    CHARACTER (LEN=*)                , INTENT(IN   )   :: block
    !! Name des Schluesselwortes des Parameters
    CHARACTER (LEN=*)                , INTENT(IN   )   :: key
    !! Variable fuer Parameter-Informationen
    TYPE (t_par)                     , INTENT(INOUT)   :: par
    !! Feld mit Informationen der Gesamt-Dictionary-Datei
    TYPE (t_block  ) , DIMENSION(:) , POINTER          :: bloecke
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='Check_CheckIfPar'
    !! Fehlerkennung
    INTEGER              :: ierr=0
    !!
    CHARACTER (LEN=16)   :: c_object
    !
    INTEGER              :: i
    !
    INTEGER              :: i_bl, i_key,i_par
    !
    TYPE (t_obj)         :: ck_obj
    !
    !
    IF ( .NOT. ASSOCIATED(par%CheckIf) ) THEN
       !
       ! Fehler -1120 : Fuer Parameter existiert keine CheckIfPar-Anforderung !
       !
       CALL setup_error_act ( all_errors(:), -1120, c_upname, c_modname )
       !
       RETURN
       !
    END IF
    !
    ! Vergleichsoperation auf ParameterTyp checken
    !            - fuer CHAR, FILE und LOG  nur .EQ. und .NE. erlaubt
    !
    IF ( par%Type .EQ. 'CHAR'  .OR.  par%Type .EQ. 'FILE'  .OR. &
         par%Type .EQ. 'LOG'                                     ) THEN
       !
       DO i = 1, SIZE(par%CheckIf)
          !
          IF ( par%CheckIf(i)%oper .NE. 'EQ' .AND. &
               par%CheckIf(i)%oper .NE. 'NE'        ) THEN
             !
             ! Fehler -1130 : Vergleichsoperation ist weder .EQ. noch .NE. !
             !
             CALL setup_error_act ( all_errors(:), -1130, c_upname, c_modname )
             CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
             CALL setup_error_act ( '<blockname>', TRIM(dicbl_name) )
             CALL setup_error_act ( '<par_typ>', TRIM(par%Type) )
             !
             RETURN
             !
          END IF
          !
       END DO
       !
    END IF
    !
    !
    DO i = 1, SIZE(par%CheckIf)
       !
       ! Falls in Check-Anforderung direkt angegebener Vergleichswert vorhanden
       ! ist, pruefen ob : 
       !    - Datentyp mit dem des Parameters uebereinstimmt ?
       !    - Nur ein einzelner Wert spezifiziert wurde ?    
       !
       IF ( ASSOCIATED(par%CheckIf(i)%wert%char) ) THEN   
          !
          CALL Direktwerte_pruefen &
               ( par%Type, dicbl_name, par%CheckIf(i) )
          !
          IF ( any_error( ) ) RETURN
          !
       END IF
       !
       ! Vergleichsobjekt der CheckIfPar-Anforderung pruefen
       !
       IF ( LEN_TRIM(par%CheckIf(i)%objekt%typ) .NE. 0 ) THEN
          !
          CALL init_objekt &
               ( ck_obj )
          !
          IF ( any_error( ) ) RETURN
          !
          ck_obj%typ(1:3) = 'PAR'
          ck_obj%block    = block
          ck_obj%key      = key
          ck_obj%par_nr   = par%ParPos
          !
          ! Check : - ist Vergleichs-Objekt ungleich dem Objekt zu welchem 
          !           die Check-Anforderung gehoert
          !         - ist Vergleichs-Objekt in Dictionary-Datei definiert
          !   und : - Ausfuellen der Blanko-Adressen
          !
          CALL Objekt_pruefen &
               ( par%CheckIf(i)%objekt, ck_obj, dicbl_name, &
               'CheckIfPar', bloecke )
          !
          IF ( any_error( ) ) RETURN
          !
          ! Stimmt Datentyp des Vergleichs-Objektes mit Datentyp des Parameters
          ! ueberein
          !
          i_bl    = par%CheckIf(i)%objekt%bl_nr
          i_key   = par%CheckIf(i)%objekt%key_nr
          i_par   = par%CheckIf(i)%objekt%par_nr
          !
          IF ( bloecke(i_bl)%Key(i_key)%Par(i_par)%Type .NE. par%Type ) THEN
             !
             ! Fehler -1140 : Datentyp des Vergleichs-Objektes stimmt nicht mit
             !               dem Datentyp des Parameters ueberein !
             !
             CALL setup_error_act ( all_errors(:), -1140, c_upname, c_modname )
             CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
             CALL setup_error_act ( '<blockname>', TRIM(dicbl_name) )
             !
             RETURN
             !
          END IF
          !
          ! Das Vergleichs-Objekt muss als Einzelwert eindeutig identifizierbar
          ! sein !
          !
          ! A : Vergleichs-Objekt darf keine Feldgroesse sein
          !     ( Parameter%L_Array = True )
          !
          IF ( bloecke(i_bl)%Key(i_key)%Par(i_par)%L_Array ) THEN
             !
             ! Fehler -1150 : Vergleichs-Parameter ist unzulaessigerweise eine Feldgroesse !
             !
             CALL setup_error_act ( all_errors(:), -1150, c_upname, c_modname )
             CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
             CALL setup_error_act ( '<blockname>', TRIM(dicbl_name) )
             !
             RETURN
             !
          END IF
          !
          ! B : Vergleichsparameter darf nicht in einer Keyzeile liegen, fuer
          !     die gilt : L_Single = False ; es sei denn er steht in derselben
          !     Zeile wie der Parameter fuer den der Check durchgefuehrt wird
          !
          IF ( .NOT. bloecke(i_bl)%Key(i_key)%L_Single ) THEN
             !
             IF ( ck_obj%block .NE. bloecke(i_bl)%Name  .OR. &
                  ck_obj%key   .NE. bloecke(i_bl)%Key(i_key)%Name ) THEN
                !
                ! Fehler -1160 : Fuer den Vergleichs-Parameter kommen unzulaessig viele Werte in
                !               Frage, da fuer seine Keyzeile gilt : L_Single = False !
                !
                CALL setup_error_act ( all_errors(:), -1160, c_upname, c_modname )
                CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
                CALL setup_error_act ( '<blockname>', TRIM(dicbl_name) )
                !
                RETURN
                !
             END IF
             !
          END IF
          !
          ! C : Vergleichsparameter darf nicht in einem Eingabeblock liegen, fuer
          !     den gilt : L_Single = False ; es sei denn er steht im selben
          !     EGB wie der Parameter fuer den der Check durchgefuehrt wird
          !
          IF ( .NOT. bloecke(i_bl)%L_Single ) THEN
             ! 
             IF ( ck_obj%block .NE. bloecke(i_bl)%Name ) THEN
                !
                ! Fehler -1170 : Fuer den Vergleichs-Parameter kommen unzulaessig viele Werte in
                !               Frage, da fuer seinen Eingabeblock gilt : L_Single = False !
                !
                CALL setup_error_act ( all_errors(:), -1170, c_upname, c_modname )
                CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
                CALL setup_error_act ( '<blockname>', TRIM(dicbl_name) )
                !
                RETURN
                !
             END IF
             !
          END IF
          !
          ! Der Vergleichs-Parameter muss vorhanden sein, wenn der zu checkende
          ! Parameter mit dem CheckIfPar-Check vorhanden ist ; d.h. er darf 
          ! nur unter bestimmten Bedingungen einer Optionalitaet unterliegen
          !
          c_object='                '
          !        '1234567890123456' 
          IF ( ck_obj%block .EQ. bloecke(i_bl)%Name ) THEN
             !
             IF ( ck_obj%key .NE. bloecke(i_bl)%Key(i_key)%Name ) THEN
                !
                IF ( bloecke(i_bl)%Key(i_key)%L_Opt ) THEN
                   ierr = 7
                   c_object = 'seinen KEY'
                ELSE IF ( bloecke(i_bl)%Key(i_key)%Par(i_par)%L_Opt ) THEN
                   ierr = 7
                   c_object = 'ihn'
                END IF
                !
             END IF
             !
          ELSE
             !
             IF ( bloecke(i_bl)%L_Opt ) THEN
                ierr = 7
                c_object = 'seinen Block-Typ'
                !          '1234567890123456'                       
             ELSE IF ( bloecke(i_bl)%Key(i_key)%L_Opt ) THEN
                ierr = 7
                c_object = 'seinen KEY'
             ELSE IF ( bloecke(i_bl)%Key(i_key)%Par(i_par)%L_Opt ) THEN
                ierr = 7
                c_object = 'ihn'
             END IF
             !
          END IF
          !
          IF ( ierr .EQ. 7 ) THEN
             !
             ! Fehler -1180 : Vergleichs-Parameter unterliegt einer unzulaessigen Optionalitaet !
             !
             CALL setup_error_act ( all_errors(:), -1180, c_upname, c_modname )
             CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
             CALL setup_error_act ( '<blockname>', TRIM(dicbl_name) )
             CALL setup_error_act ( '<wen_objekt>', TRIM(c_object) )
             !
             RETURN
             !
          END IF
          !
       END IF   ! Vergleichsobjekt vorhanden
       !
    END DO   ! Schleife ueber alle CheckIfPar-Anforderungen des Parameters
    !
  END SUBROUTINE Check_CheckIfPar
  !
  !! Ueberprueft die in Check-Anforderung direkt 
  !! angegebenen Werte hinsichtlich Datentyp und Anzahl !
  !! Es darf nur einen Vergleichswert geben !<BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE Direktwerte_pruefen &
       ( DataType, dicbl_name, mycheck )
    !
    ! USE-Statements
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
    !! Datentyp auf den der Wert geprueft werden soll
    CHARACTER (LEN=*)                , INTENT(IN   )  :: DataType
    !! Name des Dictionary-Blockes
    CHARACTER (LEN=*)                , INTENT(IN   )  :: dicbl_name
    !! Variable fuer Check-Anforderung
    TYPE (t_check)                                    :: mycheck
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='Direktwerte_pruefen'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Fehlerkennung
    INTEGER              :: ierr=0
    !
    CHARACTER (LEN=key_len) , DIMENSION(:), POINTER    :: c_hilf
    !
    !
    NULLIFY(c_hilf)
    !
    ! Probelesen
    !
    SELECT CASE (DataType)
    CASE ('INT')
       !
       CALL read_liste  &
            ( TRIM(mycheck%wert%char(1)), ' ', mycheck%wert%int )
       !
       IF ( any_error( ) ) THEN
          !
          IF ( specific_error(-1510) ) THEN    ! -1510 = Lesefehler trat auf !
             !
             ierr = 1
             !
          ELSE
             !
             RETURN
             !
          END IF
          !
       END IF
       !
       IF ( SIZE(mycheck%wert%int) .GT. 1 ) THEN
          !
          ierr   = 2
          !      
       END IF
       !
    CASE ('REAL')
       !
       CALL read_liste  &
            ( TRIM(mycheck%wert%char(1)), ' ', mycheck%wert%real )
       !
       IF ( any_error( ) ) THEN
          !
          IF ( specific_error(-1510) ) THEN    ! -1510 = Lesefehler trat auf !
             !
             ierr = 1
             !
          ELSE
             !
             RETURN
             !
          END IF
          !
       END IF
       !
       IF ( SIZE(mycheck%wert%real) .GT. 1 ) THEN
          !      
          ierr   = 2
          !
       END IF
       !
    CASE ('LOG')
       !
       CALL read_liste  &
            ( TRIM(mycheck%wert%char(1)), ' ', mycheck%wert%log )
       !
       IF ( any_error( ) ) THEN
          !
          IF ( specific_error(-1520) ) THEN   ! -1520 auch Lesefehler, aber fuer LOGICAL !
             !
             ierr = 1
             !
          ELSE
             !
             RETURN
             !
          END IF
          !
       END IF
       !
       IF ( SIZE(mycheck%wert%log) .GT. 1 ) THEN
          !
          ierr   = 2
          !
       END IF
       !
    CASE ('DOUBLE')
       !
       CALL read_liste  &
            ( TRIM(mycheck%wert%char(1)), ' ', mycheck%wert%doub )
       !
       IF ( any_error( ) ) THEN
          !
          IF ( specific_error(-1510) ) THEN    ! -1510 = Lesefehler trat auf !
             !
             ierr = 1
             !
          ELSE
             !
             RETURN
             !
          END IF
          !
       END IF
       !
       IF ( SIZE(mycheck%wert%doub) .GT. 1 ) THEN
          !
          ierr   = 2
          !
       END IF
       !
    CASE ('CHAR', 'FILE','DATE','INCR')
       !
       CALL read_liste  &
            ( TRIM(mycheck%wert%char(1)), ' ', c_hilf )
       !
       IF ( any_error( ) ) RETURN
       !
       IF ( SIZE(c_hilf) .GT. 1 ) THEN
          !   
          ierr   = 2
          !
       END IF
       !
       IF ( ierr .NE. 2 ) THEN
          !
          SELECT CASE (DataType)
          CASE ('DATE')
             !
             CALL Time_Probelesen &
                  ( 'DATE', c_hilf )
             !
             IF ( any_error( ) ) RETURN
             !
          CASE ('INCR')
             !
             CALL Time_Probelesen &
                  ( 'INCR', c_hilf )
             !
             IF ( any_error( ) ) RETURN
             !
          END SELECT
          !
       END IF
       !
       ! Deallocate des lokalen Feldes c_hilf
       !
       DEALLOCATE ( c_hilf, STAT=stat )
       !
       IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Deallocate
          !
          CALL setup_error_act &
               ( all_errors(:), -20000, c_upname, c_modname, stat )
          CALL setup_error_act ( '<felder>', 'c_hilf' )
          !
          RETURN
          !
       END IF
       !
       NULLIFY (c_hilf)
       !
    END SELECT
    !
    IF ( ierr .EQ. 1 ) THEN
       !
       ! Fehler -1190 : fuer den Datentyp trat ein Lesefehler auf !
       !
       CALL setup_error_act ( all_errors(:), -1190, c_upname, c_modname )
       CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
       CALL setup_error_act ( '<blockname>', TRIM(dicbl_name) )
       !
       RETURN
       !
    END IF
    !
    IF ( ierr .EQ. 2 ) THEN
       !
       ! Fehler -1200 : Fuer den Vergleichswert wurde mehr als ein Wert angegeben !
       !
       CALL setup_error_act ( all_errors(:), -1200, c_upname, c_modname )
       CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
       CALL setup_error_act ( '<blockname>', TRIM(dicbl_name) )
       !  
       RETURN
       !
    END IF
    !
    SELECT CASE (DataType)
    CASE ('INT','REAL','LOG','DOUBLE')
       !
       ! Deallokieren von wert%char(1)
       !
       DEALLOCATE ( mycheck%wert%char, STAT=stat )
       !
       IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Deallocate
          !
          CALL setup_error_act &
               ( all_errors(:), -20000, c_upname, c_modname, stat )
          CALL setup_error_act ( '<felder>', 'mycheck%wert%char' )
          !
          RETURN
          !
       END IF
       !
       NULLIFY (mycheck%wert%char)
       !
    END SELECT
    !
  END SUBROUTINE Direktwerte_pruefen
  !
  !! Ueberprueft das in Check-Anforderungen  
  !! angegebene Objekt !
  !! - ist es in Dictionary-Datei existent
  !! - ist es ungleich dem Objekt zu dem die
  !!   Check-Anforderung gehoert
  !! 
  !! Ausserdem werden Blank-Adressierungen mit den 
  !! Objekt-Adressen des ck_obj aufgefuellt !<BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE Objekt_pruefen &
       ( vgl_obj, ck_obj, dicbl_name, ck_art, bloecke )
    !
    ! USE-Statements
    !
    USE m_stdat_types, ONLY : &
         ! Routinen
         Adressnummern_suchen
    !
    ! Formalparameter
    !! Variable mit Objekt aus der Check-Anforderung
    TYPE (t_obj)                     , INTENT(INOUT)  :: vgl_obj
    !! Variable mit Beschreibung des Objektes, zu dem die
    !! zu pruefende Check-Anforderung gehoert
    TYPE (t_obj)                     , INTENT(IN   )  :: ck_obj
    !! Name des Dictionary-Blockes
    CHARACTER (LEN=*)                , INTENT(IN   )  :: dicbl_name
    !! Art der Check-Anforderung [CheckIfPar,ReqIf,NotIf]
    CHARACTER (LEN=*)                , INTENT(IN   )  :: ck_art
    !! Feld mit gesammter Dictionary-Information
    TYPE (t_block) , DIMENSION(:)    , POINTER        :: bloecke
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='Objekt_pruefen'
    !
    LOGICAL                             :: l_gleich, &
                                           l_def
    !
    INTEGER                             :: par_nr
    INTEGER                             :: i
    !
    !
    ! Initialisierungen
    !
    par_nr  = 0
    !
    IF ( .NOT. ASSOCIATED(bloecke)) THEN
       !
       ! Fehler -1210 : Der Pointerfeld <bloecke> .not. associated !
       !
       CALL setup_error_act ( all_errors(:), -1210, c_upname, c_modname )
       !
       RETURN
       !
    END IF
    !
    ! Blanko-Adressierungen ausfuellen
    !
    IF ( LEN_TRIM(vgl_obj%block) .EQ. 0 ) THEN
       !
       vgl_obj%block = ck_obj%block
       !
    END IF
    !
    SELECT CASE (vgl_obj%typ)
    CASE('PAR','KEY')
       !
       IF ( LEN_TRIM(vgl_obj%key) .EQ. 0 ) THEN
          !
          vgl_obj%key = ck_obj%key
          !
       END IF
       !
    END SELECT
    !
    ! Adressnummern des Vergleichsobjektes feststellen
    ! ( Nummer = 0 bedeutet Objekt-Adressteil wurde nicht gefunden )
    !
    CALL Adressnummern_suchen &
         ( vgl_obj, bloecke, vgl_obj%bl_nr, vgl_obj%key_nr, par_nr )
    !
    IF ( any_error( ) ) RETURN
    !
    ! Check : VGL_OBJ und CK_OBJ duerfen nicht identisch sein
    !
    CALL Objekt_Vergleich &
         ( vgl_obj, ck_obj, l_gleich )
    !
    IF ( any_error( ) ) RETURN
    !
    IF ( l_gleich ) THEN
       !
       ! Fehler -1220 : Das Vergleichs-Objekt ist identisch mit dem Objekt, welches die
       !               Check-Anforderung stellt !
       !
       CALL setup_error_act ( all_errors(:), -1220, c_upname, c_modname )
       CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
       CALL setup_error_act ( '<blockname>', TRIM(dicbl_name) )
       CALL setup_error_act ( '<check_art>', TRIM(ck_art) )
       !
       RETURN
       !
    END IF
    !
    ! Check : Ist VGL_OBJ in <bloecke> definiert
    !
    l_def = .True.
    !
    ! ... in jedem Fall : Blocknummer checken
    !
    IF ( vgl_obj%bl_nr .EQ. 0 ) THEN
       !   
       l_def = .False.
       !
    END IF
    !
    ! ... wenn Block OK : ggf. Keynummer checken
    !
    IF ( l_def ) THEN
       !
       SELECT CASE (vgl_obj%typ)
       CASE('PAR','KEY')
          !
          IF ( vgl_obj%key_nr .EQ. 0 ) THEN
             !
             l_def = .False.
             !
          END IF
          !
       END SELECT
       !
    END IF
    !
    ! ... wenn Block & Key OK : ggf. par_nr checken
    !
    IF ( l_def ) THEN
       !
       SELECT CASE (vgl_obj%typ)
       CASE('PAR')
          !
          IF ( par_nr .EQ. 0 ) THEN
             !
             l_def = .False.
             !
          END IF
          !
       END SELECT
       !
    END IF
    !
    IF ( .NOT. l_def ) THEN
       !
       ! Fehler -1230 : Vergleichsobjekt in Dictionary-Datei nicht definiert !
       !
       CALL setup_error_act ( all_errors(:), -1230, c_upname, c_modname )
       CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
       CALL setup_error_act ( '<blockname>', TRIM(dicbl_name) )
       CALL setup_error_act ( '<check_art>', TRIM(ck_art) )
       !
       RETURN
       !
    END IF
    !
  END SUBROUTINE Objekt_pruefen
  !
  !! UP prueft ob zwei Objekte <t_obj> identisch sind.<BR>
  !! Subroutine erzeugt keine Fehlermeldung.
  SUBROUTINE Objekt_Vergleich &
       ( objekt_A, objekt_B, l_gleich )
    !
    ! Formalparameter
    !! Variable mit Beschreibung des Objektes A
    TYPE (t_obj)                     , INTENT(IN   )  :: objekt_A
    !! Variable mit Beschreibung des Objektes B
    TYPE (t_obj)                     , INTENT(IN   )  :: objekt_B
    !! True , wenn Objekte identisch<BR>
    !! False, wenn Objekte nicht identisch
    LOGICAL                          , INTENT(  OUT)  :: l_gleich
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='Objekt_Vergleich'
    !
    ! Check : Handelt es sich bei objekt_A und objekt_B um dasselbe Objekt ?
    !
    l_gleich = .True.
    !
    IF ( objekt_A%typ .EQ. objekt_B%typ ) THEN
       !  
       IF ( objekt_A%block .EQ. objekt_B%block ) THEN
          !  
          SELECT CASE (objekt_A%typ)
          CASE('PAR','KEY')
             !
             IF ( objekt_A%key .EQ. objekt_B%key ) THEN
                !
                SELECT CASE (objekt_A%typ)
                CASE('PAR')
                   !
                   IF ( objekt_A%par_nr .NE. objekt_B%par_nr ) THEN
                      !  
                      l_gleich = .False.
                      !
                   END IF
                   !
                END SELECT
                !
             ELSE
                !
                l_gleich = .False.
                !
             END IF
             !
          END SELECT
          !
       ELSE
          !
          l_gleich = .False.
          !
       END IF
       !
    ELSE   
       !
       l_gleich = .False.
       !
    END IF    ! ( objekt_A%typ .EQ. objekt_B%typ )
    !
  END SUBROUTINE Objekt_Vergleich
  !
  !! Allokiert ein Feld vom Typ t_check und traegt die zum
  !! CheckTyp gehoerigen Werte 
  !! aus der uebergebenen verketteten Liste in die 
  !! Feldelemente ein.<BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE alloc_check_feld &
       ( act_ck, fst_ck, lst_ck, blockname, &
         ck_typ, anz, mycheck )
    !
    ! USE-Statements
    !
    USE m_stdat_types, ONLY : &
         ! Routine
         init_check
    !
    ! Formalparameter
    TYPE (t_vl_kz)     , POINTER                :: act_ck
    TYPE (t_vl_kz)     , POINTER                :: fst_ck
    TYPE (t_vl_kz)     , POINTER                :: lst_ck
    CHARACTER (LEN=*)                           :: blockname
    CHARACTER (LEN=*)                           :: ck_typ
    INTEGER                                     :: anz
    TYPE (t_check) , DIMENSION(:) , POINTER     :: mycheck
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='alloc_check_feld'
    !! Statusvariable
    INTEGER :: stat ! 
    INTEGER                       :: i, nr
    !
    !
    IF ( TRIM(ck_typ) .NE. 'ReqIf' .AND. &
         TRIM(ck_typ) .NE. 'NotIf' .AND. &
         TRIM(ck_typ) .NE. 'CheckIfPar'      ) THEN
       !
       ! Fehler -1240 : falscher CheckTyp !
       !
       CALL setup_error_act ( all_errors(:), -1240, c_upname, c_modname )
       CALL setup_error_act ( '<check_typ>', TRIM(ck_typ) )
       !
       RETURN
       !
    END IF
    !
    ! Speicherfeld ck allokieren
    !
    ALLOCATE (mycheck(anz), STAT=stat)
    !
    IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
       !
       CALL setup_error_act &
            ( all_errors(:), -10000, c_upname, c_modname, stat )
       CALL setup_error_act ( '<felder>', 'mycheck' )
       !
       RETURN
       !
    END IF
    !
    ! Speicherfeld initialisieren
    !
    DO i = 1, SIZE(mycheck)
       !
       CALL init_check ( mycheck(i) )
       !
       IF ( any_error( ) ) RETURN
       !
    END DO
    !
    ! Elemente der verketteten Liste mit den CheckStrings werden nun in die
    ! dynamischen Feld ck uebertragen
    !
    ! ... erstes Listenelement
    !
    act_ck => fst_ck
    nr = 0   
    !
    ! ... Schleife ueber alle Listenelemente
    !
    vlist : DO
       !
       IF ( .NOT. ASSOCIATED(act_ck) ) EXIT vlist
       !
       IF ( TRIM(act_ck%key) .EQ. ck_typ ) THEN
          !
          ! ... nacheinander in das Feld ck einsortieren
          !
          nr = nr + 1
          !
          IF (nr .GT. anz) THEN
             !
             ! Fehler -1250 : ANZ zu klein in UP-Aufruf  !
             !
             CALL setup_error_act ( all_errors(:), -1250, c_upname, c_modname )
             !
             RETURN
             !
          ELSE
             !
             CALL read_check &
                  ( TRIM(blockname), ck_typ, &
                  TRIM(act_ck%wert), mycheck(nr) )
             !
             IF ( any_error( ) ) RETURN
             !
          END IF
          !
       END IF
       !
       ! ... naechsten CheckString initialisieren
       !
       lst_ck  => act_ck
       act_ck  => act_ck%next
       !
    END DO vlist
    !
  END SUBROUTINE alloc_check_feld
  !
  !! Liest aus einem String die Informationen zu einem 
  !! durchzufuehrenden Check in den dafuer vorgesehenen
  !! Datentypen ein.
  !! ReqIf, NotIf, CheckIfPar sind die mit diesem UP
  !! auswertbaren Check-Vorschriften <BR>
  !! Subroutine erzeugt Fehlermeldungen.
  SUBROUTINE read_check &
       ( blockname, check_typ, zeil_wert, mycheck )
    !
    ! USE-Anweisung(en)
    !
    USE m_stringliste, ONLY : &
         ! Routine
         read_liste
    !
    ! Formalparameter
    !! Name des Dictionary-Blockes
    CHARACTER (LEN=*)  , INTENT(IN   )         :: blockname
    !! Gibt Check-Typ an  [ReqIf,NotIf,CheckIf]
    CHARACTER (LEN=*)  , INTENT(IN   )         :: check_typ
    !! Wert einer Schluesselwort-Zeile
    CHARACTER (LEN=*)  , INTENT(IN   )         :: zeil_wert
    !! Variable zum Ablegen der Check-Vorschrift
    TYPE (t_check)     , INTENT(INOUT)         :: mycheck
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='read_check'
    !! Statusvariable
    INTEGER :: stat ! 
    !! Fehlerkennung
    INTEGER    :: ierr=0
    !
    INTEGER    :: klam_auf , &     ! /
                  klam_zu  , &     !   Positionsnummern der einzelnen 
                  punkt_1  , &     !   signifikanten Zeichen im Check-String
                  punkt_2          ! /
    !
    CHARACTER (LEN=LEN(check_typ))                      :: ck_typ
    CHARACTER (LEN=LEN(zeil_wert))                      :: z_wert   , &
                                                           c_hilf , &
                                                           list_string
    !
    CHARACTER (LEN=key_len) , DIMENSION(:) , POINTER    :: c_feld
    INTEGER                                             :: i, j, ios
    LOGICAL                                             :: l_readwert
    !
    !
    ! Initialisieren der Fehlertexte
    !
    klam_auf = 0
    klam_zu  = 0
    punkt_1  = 0
    punkt_2  = 0
    !
    ck_typ       = ADJUSTL(check_typ)
    z_wert       = ADJUSTL(zeil_wert)
    !
    list_string  = REPEAT(' ',LEN(list_string))
    NULLIFY(c_feld)
    !
    l_readwert   = .False.
    !
    ! Positionen der einzelnen signifikanten Zeichen des Strings mit der
    ! Check-Vorschrift ermitteln
    !
    DO i = 1, LEN_TRIM(z_wert)
       !
       SELECT CASE ( z_wert(i:i) )
          !
       CASE('(')
          !
          IF (klam_auf .EQ. 0) THEN
             IF (klam_zu .EQ. 0) THEN         ! ) darf nicht vor ( stehen
                klam_auf = i
             ELSE
                ierr = 2
             END IF
          ELSE
             ierr = 1                         ! ( darf nur einmal vorkommen
          END IF
          !
       CASE(')')
          !
          IF (klam_zu .EQ. 0) THEN            ! ) darf nicht vor ( stehen
             IF (klam_auf .GT. 0) THEN
                klam_zu = i
             ELSE
                ierr = 2
             END IF
          ELSE
             ierr = 1                         ! ) darf nur einmal vorkommen
          END IF
          !
       CASE('.')
          !
          ! --> Punkte nach Klammerausdruck
          !
          IF (klam_auf .GT. 0 .AND. klam_zu .GT. 0) THEN
             !
             IF (punkt_1 .EQ. 0) THEN
                !
                punkt_1 = i
                !
             ELSE IF ( punkt_2 .EQ. 0 ) THEN
                !
                IF (punkt_1 .LT. klam_auf) THEN
                   !
                   ! Fehler : 2.Punkt liegt hinter Klammerbereich, obwohl
                   !          1.Punkt davor liegt
                   !
                   ierr = 3
                   !
                ELSE    ! Pkt.1-Position liegt fest & ist OK
                   !
                   punkt_2 = i
                   !
                END IF
                !
             END IF
             !
             ! --> Punkte vor Klammerausdruck
             !
          ELSE IF (klam_auf .EQ. 0 .AND. klam_zu .EQ. 0) THEN
             !
             IF (punkt_1 .EQ. 0) THEN
                !
                punkt_1 = i
                !
             ELSE IF (punkt_2 .EQ. 0) THEN
                !
                punkt_2 = i
                !
             END IF
             !
          END IF
          !
          ! Bemerkung : Punkte innerhalb des Klammerausdruckes interessieren
          !             an dieser Stelle nicht !
          !
       END SELECT
       !
       ! --> Fehler : eines der signifikanten Zeichen zu haeufig
       !
       IF ( ierr .EQ. 1 ) THEN
          !
          ! Fehler -1260 : Der Ausdruck enthaelt eine Klammer zuviel !
          !
          CALL setup_error_act ( all_errors(:), -1260, c_upname, c_modname )
          CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
          CALL setup_error_act ( '<blockname>', TRIM(blockname) )
          CALL setup_error_act ( '<check_typ>', TRIM(ck_typ) )
          CALL setup_error_act ( '<ausdruck>', TRIM(z_wert) )
          !
          RETURN
          !
       END IF
       !
       IF ( ierr .EQ. 2 ) THEN
          !
          ! Fehler -1270 :   ) vor ( !
          !
          CALL setup_error_act ( all_errors(:), -1270, c_upname, c_modname )
          CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
          CALL setup_error_act ( '<blockname>', TRIM(blockname) )
          CALL setup_error_act ( '<check_typ>', TRIM(ck_typ) )
          CALL setup_error_act ( '<ausdruck>', TRIM(z_wert) )
          !
          RETURN
          !
       END IF
       !
       IF ( ierr .EQ. 3 ) THEN
          !
          ! Fehler -1280 : Punktposition falsch !
          !
          CALL setup_error_act ( all_errors(:), -1280, c_upname, c_modname )
          CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
          CALL setup_error_act ( '<blockname>', TRIM(blockname) )
          CALL setup_error_act ( '<check_typ>', TRIM(ck_typ) )
          CALL setup_error_act ( '<ausdruck>', TRIM(z_wert) )
          !
          RETURN
          !
       END IF
       !
    END DO
    !
    !
    IF ( klam_auf .NE. 0  .AND.  klam_zu .EQ. 0 ) THEN
       !
       ! Fehler -1290 : dem Klammerausdruck fehlt die schliessende Klammer !
       !
       CALL setup_error_act ( all_errors(:), -1290, c_upname, c_modname )
       CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
       CALL setup_error_act ( '<blockname>', TRIM(blockname) )
       CALL setup_error_act ( '<check_typ>', TRIM(ck_typ) )
       CALL setup_error_act ( '<ausdruck>', TRIM(z_wert) )
       !
       RETURN
       !
    END IF
    !
    IF ( punkt_1 .EQ. 0  .OR.  punkt_2 .EQ. 0 ) THEN
       !
       ! Fehler -1300 : einer oder beide Punkte fehlen !
       !
       CALL setup_error_act ( all_errors(:), -1300, c_upname, c_modname )
       CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
       CALL setup_error_act ( '<blockname>', TRIM(blockname) )
       CALL setup_error_act ( '<check_typ>', TRIM(ck_typ) )
       CALL setup_error_act ( '<ausdruck>', TRIM(z_wert) )
       !
       RETURN
       !
    END IF
    !
    ! Bedingung
    !
    SELECT CASE (ck_typ)
       !
    CASE('ReqIf', 'NotIf')
       ! 
       IF ( klam_auf .EQ. 0  .AND.  klam_zu .EQ. 0 ) THEN
          !
          ! Fehler -1310 : der Klammer-Ausdruck mit der Objekt-Adresse fehlt !
          !
          CALL setup_error_act ( all_errors(:), -1310, c_upname, c_modname )
          CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
          CALL setup_error_act ( '<blockname>', TRIM(blockname) )
          CALL setup_error_act ( '<check_typ>', TRIM(ck_typ) )
          CALL setup_error_act ( '<ausdruck>', TRIM(z_wert) )
          !
          RETURN
          !
       END IF
       !
       IF ( punkt_2 .LT. klam_auf ) THEN
          !
          ! Fehler -1320 : Vergleichsoperation steht vor Klammer-Ausdruck !
          !
          CALL setup_error_act ( all_errors(:), -1320, c_upname, c_modname )
          CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
          CALL setup_error_act ( '<blockname>', TRIM(blockname) )
          CALL setup_error_act ( '<check_typ>', TRIM(ck_typ) )
          CALL setup_error_act ( '<ausdruck>', TRIM(z_wert) )
          !
          RETURN
          !
       END IF
       !
       ! check%objekt%typ eintragen
       !
       SELECT CASE ( z_wert(1 : klam_auf-1 ) )
       CASE ('PAR')
          
          mycheck%objekt%typ(1:3)   = 'PAR'
          
       CASE ('KEY')
          
          mycheck%objekt%typ(1:3)   = 'KEY'
          
       CASE ('BLOCK')
          
          mycheck%objekt%typ(1:5)   = 'BLOCK'
          
       CASE DEFAULT
          !
          ! Fehler -1330 : Unzulaessige CheckObjektTyp angefordert !
          !               Erlaubt : [PAR,KEY,BLOCK]
          !
          CALL setup_error_act ( all_errors(:), -1330, c_upname, c_modname )
          CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
          CALL setup_error_act ( '<blockname>', TRIM(blockname) )
          CALL setup_error_act ( '<check_typ>', TRIM(ck_typ) )
          CALL setup_error_act ( '<ausdruck>', TRIM(z_wert) )
          CALL setup_error_act ( '<erlaubt>', '[PAR,KEY,BLOCK]' )
          !
          RETURN
          !
       END SELECT
       !
       ! Check-Operation eintragen
       !
       SELECT CASE (mycheck%objekt%typ) 
       CASE('PAR')
          !
          SELECT CASE ( z_wert( punkt_1+1 : punkt_2-1 ) )
          CASE ('EQ')
             !
             mycheck%oper(1:2)   = 'EQ'
             l_readwert        = .True.
             !
          CASE ('NE')
             !
             mycheck%oper(1:2)   = 'NE'
             l_readwert        = .True.
             !
          CASE ('EXIST')
             !
             mycheck%oper(1:5)   = 'EXIST'
             !
          CASE ('NOTEXIST')
             !
             mycheck%oper(1:8)   = 'NOTEXIST'
             !
          CASE DEFAULT
             !
             ! Fehler -1340 : unzulaessige CheckOperation angefordert !
             !               Erlaubt : [EQ,NE,EXIST,NOTEXIST]
             !
             CALL setup_error_act ( all_errors(:), -1340, c_upname, c_modname )
             CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
             CALL setup_error_act ( '<blockname>', TRIM(blockname) )
             CALL setup_error_act ( '<check_typ>', TRIM(ck_typ) )
             CALL setup_error_act ( '<ausdruck>', TRIM(z_wert) )
             CALL setup_error_act ( '<erlaubt>', '[EQ,NE,EXIST,NOTEXIST]' )
             !
             RETURN
             !
          END SELECT
          !
       CASE ('BLOCK','KEY')
          !
          SELECT CASE ( z_wert( punkt_1+1 : punkt_2-1 ) )
          CASE ('EXIST')
             mycheck%oper(1:5)   = 'EXIST'
          CASE ('NOTEXIST')
             mycheck%oper(1:8)   = 'NOTEXIST'
          CASE DEFAULT
             !
             ! Fehler -1340 : unzulaessige CheckOperation angefordert !
             !               Erlaubt : [EXIST,NOTEXIST]
             !
             CALL setup_error_act ( all_errors(:), -1340, c_upname, c_modname )
             CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
             CALL setup_error_act ( '<blockname>', TRIM(blockname) )
             CALL setup_error_act ( '<check_typ>', TRIM(ck_typ) )
             CALL setup_error_act ( '<ausdruck>', TRIM(z_wert) )
             CALL setup_error_act ( '<erlaubt>', '[EXIST,NOTEXIST]' )
             !
             RETURN
             !
          END SELECT
          !
       END SELECT
       !
    CASE('CheckIfPar')
       !
       SELECT CASE ( z_wert(punkt_1+1:punkt_2-1) )
       CASE ('EQ')
          mycheck%oper(1:2)   = 'EQ'
       CASE ('NE')
          mycheck%oper(1:2)   = 'NE'
       CASE ('LE')
          mycheck%oper(1:2)   = 'LE'
       CASE ('LT')
          mycheck%oper(1:2)   = 'LT'
       CASE ('GE')
          mycheck%oper(1:2)   = 'GE'
       CASE ('GT')
          mycheck%oper(1:2)   = 'GT'
       CASE DEFAULT
          !
          ! Fehler -1340 : unzulaessige Check-Operation angefordert !
          !
          CALL setup_error_act ( all_errors(:), -1340, c_upname, c_modname )
          CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
          CALL setup_error_act ( '<blockname>', TRIM(blockname) )
          CALL setup_error_act ( '<check_typ>', TRIM(ck_typ) )
          CALL setup_error_act ( '<ausdruck>', TRIM(z_wert) )
          CALL setup_error_act ( '<erlaubt>', '[EQ,NE,LE,LT,GE,GT]' )
          !
          RETURN
          !
       END SELECT
       !
       IF ( klam_auf .GT. 0  .AND.  punkt_2 .GT. klam_auf ) THEN
          !
          ! Fehler -1350 : Vergleichsoperation steht hinter Klammer-Ausdruck !
          !
          CALL setup_error_act ( all_errors(:), -1350, c_upname, c_modname )
          CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
          CALL setup_error_act ( '<blockname>', TRIM(blockname) )
          CALL setup_error_act ( '<check_typ>', TRIM(ck_typ) )
          CALL setup_error_act ( '<ausdruck>', TRIM(z_wert) )
          !
          RETURN
          !
       END IF
       !
       ! check%objekt%typ eintragen
       !
       IF ( klam_auf .GT. 0 ) THEN
          !
          c_hilf  = REPEAT(' ',LEN(c_hilf))
          c_hilf  = z_wert(punkt_2 + 1 : klam_auf - 1)
          c_hilf  = ADJUSTL(c_hilf)
          !
          SELECT CASE ( c_hilf )
          CASE ('PAR')
             !
             mycheck%objekt%typ(1:3)   = 'PAR'
             !
          CASE DEFAULT
             !
             ! Fehler -1330 : Unzulaessige CheckObjektTyp angefordert !
             !
             CALL setup_error_act ( all_errors(:), -1330, c_upname, c_modname )
             CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
             CALL setup_error_act ( '<blockname>', TRIM(blockname) )
             CALL setup_error_act ( '<check_typ>', TRIM(ck_typ) )
             CALL setup_error_act ( '<ausdruck>', TRIM(z_wert) )
             CALL setup_error_act ( '<erlaubt>', '[PAR]' )
             !
             RETURN
             !
          END SELECT
          !
       ELSE
          !
          l_readwert = .True.
          !
       END IF
       !
    CASE DEFAULT
       !
       ! Fehler -1360 : Das Unterprogramm wird mit einem unzulaessigen Wert des
       !               Parameters <ck_typ> aufgerufen !
       !               erlaubt = [ReqIf,NotIf,CheckIfPar]
       !
       CALL setup_error_act ( all_errors(:), -1360, c_upname, c_modname )
       CALL setup_error_act ( '<check_typ>', TRIM(ck_typ) )
       !
       RETURN
       !
    END SELECT
    !
    ! Ggf.: check%objekt-Adresse eintragen
    !
    IF ( klam_auf .GT. 0  .AND.  klam_zu .GT. 0 ) THEN
       !
       list_string(1:klam_zu-klam_auf-1)  = z_wert(klam_auf+1 : klam_zu-1 )
       !
       IF ( LEN_TRIM(list_string) .GT. 0 ) THEN
          !
          CALL read_liste ( list_string, ',', c_feld )
          !
          IF ( any_error( ) ) RETURN
          !
          DO i = 1, SIZE(c_feld)
             c_feld(i) = ADJUSTL(c_feld(i))
          END DO
          !
          SELECT CASE (mycheck%objekt%typ) 
          CASE('PAR')
             !
             IF ( SIZE(c_feld) .LE. 3 ) THEN
                !
                j = 0
                !
                DO i = SIZE(c_feld),1,-1
                   !
                   j = j + 1
                   !
                   IF ( j .EQ. 1 ) THEN
                      !
                      READ( c_feld(i),'(I3)',IOSTAT=ios) mycheck%objekt%par_nr
                      !
                      IF ( ios .NE. 0 ) THEN
                         !
                         ! Fehler -1370 : Fehler beim Lesen der ParameterPosition !
                         !
                         CALL setup_error_act ( all_errors(:), -1370, c_upname, c_modname, ios )
                         CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
                         CALL setup_error_act ( '<blockname>', TRIM(blockname) )
                         CALL setup_error_act ( '<check_typ>', TRIM(ck_typ) )
                         CALL setup_error_act ( '<ausdruck>', TRIM(z_wert) )
                         !
                         RETURN
                         !
                      END IF
                      !
                   ELSE IF ( j .EQ. 2 ) THEN
                      ! --> Objekt-Key
                      mycheck%objekt%key    = c_feld(i)
                      !
                   ELSE IF ( j .EQ. 3 ) THEN
                      ! --> Objekt-Block
                      mycheck%objekt%block  = c_feld(i)
                      !
                   END IF
                   !
                END DO
                !
             ELSE
                ! --> Fehlerfall : zuviele Angaben fuer Objekt-Addresse
                ierr   = 13
                !
             END IF
             !
          CASE('KEY')
             !
             IF ( SIZE(c_feld) .LE. 2 ) THEN
                !
                j = 0
                !
                DO i = SIZE(c_feld),1,-1
                   !
                   j = j + 1
                   !
                   IF ( j .EQ. 1 ) THEN
                      ! read key
                      mycheck%objekt%key    = c_feld(i)
                      !
                   ELSE IF ( j .EQ. 2 ) THEN
                      ! read block
                      mycheck%objekt%block  = c_feld(i)
                      !
                   END IF
                   !
                END DO
                !
             ELSE
                ! --> Fehlerfall : zuviele Angaben fuer Objekt-Addresse
                ierr   = 13
                !
             END IF
             !
          CASE('BLOCK')
             !
             IF ( SIZE(c_feld) .EQ. 1 ) THEN
                ! read block
                mycheck%objekt%block  = c_feld(1)
                !
             ELSE
                ! --> Fehlerfall : zuviele Angaben fuer Objekt-Addresse
                ierr   = 13
                !
             END IF
             !
          END SELECT
          !
          IF ( ierr .EQ. 13 ) THEN
             !
             ! Fehler -1380 : zuviele Angaben fuer Objekt-Addresse !
             !
             CALL setup_error_act ( all_errors(:), -1380, c_upname, c_modname )
             CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
             CALL setup_error_act ( '<blockname>', TRIM(blockname) )
             CALL setup_error_act ( '<check_typ>', TRIM(ck_typ) )
             CALL setup_error_act ( '<ausdruck>', TRIM(z_wert) )
             !
             RETURN
             !
          END IF
          !
       ELSE
          !
          ! Fehler -1390 : nur BlankString zwischen Klammern !
          !
          CALL setup_error_act ( all_errors(:), -1390, c_upname, c_modname )
          CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
          CALL setup_error_act ( '<blockname>', TRIM(blockname) )
          CALL setup_error_act ( '<check_typ>', TRIM(ck_typ) )
          CALL setup_error_act ( '<ausdruck>', TRIM(z_wert) )
          !
          RETURN
          !
       END IF     !  ( LEN_TRIM(list_string) .GT. 0 )
       !
    END IF     !  ( klam_auf .GT. 0  .AND.  klam_zu .GT. 0 )
    !
    ! Ggf.: check%wert eintragen
    !
    IF ( l_readwert ) THEN
       !
       c_hilf  = REPEAT(' ',LEN(c_hilf))
       c_hilf  = z_wert(punkt_2 + 1 : )
       c_hilf  = ADJUSTL(c_hilf)
       !
       IF ( LEN_TRIM(c_hilf) .NE.0 ) THEN
          !
          ALLOCATE (mycheck%wert%char(1), STAT=stat)
          !
          IF ( stat /= 0 ) THEN ! Fehler bei Speicher-Allocate
             !
             CALL setup_error_act &
                  ( all_errors(:), -10000, c_upname, c_modname, stat )
             CALL setup_error_act ( '<felder>', 'mycheck%wert%char' )
             !
             RETURN
             !
          END IF
          !
          mycheck%wert%char(1) = REPEAT(' ', LEN(mycheck%wert%char(1)) )
          mycheck%wert%char(1) = c_hilf
          !
       ELSE
          !
          ! Fehler -1400 : Angabe des Vergleichswertes fehlt !
          !
          CALL setup_error_act ( all_errors(:), -1400, c_upname, c_modname )
          CALL setup_error_act ( '<file_name>', TRIM(dicfile_name) )
          CALL setup_error_act ( '<blockname>', TRIM(blockname) )
          CALL setup_error_act ( '<check_typ>', TRIM(ck_typ) )
          CALL setup_error_act ( '<ausdruck>', TRIM(z_wert) )
          !
          RETURN
          !
       END IF
       !
    END IF
    !
  END SUBROUTINE read_check
  !
END MODULE m_dic_checks
!  ----------------------------------------------------------------------------
!  Ende des Source-Codes des Moduls
!  ----------------------------------------------------------------------------

