!! <H2>M_STDAT_CHECKS</h2>
!! @author Susanne Spohr
!! @version 1.5 vom 07/07/05, Quellcode: mod_m_stdat_checks.f90
!! <HR>
!! <one line to give the program's name and an idea of what it does> <BR>
!! <HR>
!  Copyright-Hinweis
!
!  Copyright (C) 2002 Bundesanstalt fuer Wasserbau
!
!  <CopyrightWildcard>
!
!  Entwicklungsgeschichte des Moduls
!  01.01 : 2002-05-24 : Susanne Spohr : Re-Engineering des Moduls mod_stdat_checks.f90 der Library dictionary
!  01.02 : 2002-06-27 : Susanne Spohr : Package-Struktur ueberarbeitet (+ _data-Modul), init_.._all_errors nun nur in UI-Modul, ...
!  01.03 : 2002-07-11 : Susanne Spohr : Verbesserung Fehlertexte
!  01.04 : 2004-02-03 : Susanne Spohr : Anpassung NT
!  01.05 : 2005-07-07 : G. Lang       : set_file_path_and_name verwenden
!! <HR>
!!                                                                  <BR>
!! <HR>
!! <H3>Kurze Leistungsbeschreibung des Modulss</H3>
!! Verschiedene Checks auf den Eingabedaten der Steuerdatei         <BR>
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
MODULE m_stdat_checks
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
  ! [A.2] BASIS-Modul fuer den Umgang mit Zeitinkrementen
  !
  USE b_time
  !
  ! [A.3] BASIS-Modul fuer den Umgang mit Zeitinkrementen
  !
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
       all_errors     ! Fehlermeldunegn
  !
  ! [B.2] weitere Module die zum Package "dictionary" gehoeren
  !
  USE m_stdat_types, ONLY : &
       ! Parameter
       line_len  , &
       key_len   , &
       ! Typdefinitionen
       t_block   , &
       t_check
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
  ! [C.1] oeffentlich zugaengliche Typen deklarieren
  !
  ! [C.2] Konstantwerte (Parameter) [moeglichst nicht verwenden]
  !
  ! [C.3] Variablen [moeglichst nicht verwenden]
  !
  ! [C.4] Schnittstellen
  !
  !! Checkt, ob Parameter zu der Menge der erlaubten Werte gehoert
  INTERFACE FixValue_Check
     MODULE PROCEDURE FixValue_Check_d ! 
  END INTERFACE
  !
  !! An-, bzw.Abwesenheitscheck von Eingabeelementen der Steuerdatei
  INTERFACE ReqIfNotIf_Auswertung
     MODULE PROCEDURE ReqIfNotIf_Auswertung_d ! 
  END INTERFACE
  !
  !! Checken bestimmter Parameter-Bedingungen
  INTERFACE CheckIfPar_Auswertung
     MODULE PROCEDURE CheckIfPar_Auswertung_d ! 
  END INTERFACE
  !
  !! Checken von Dateienexistenzen
  INTERFACE File_Checks
     MODULE PROCEDURE File_Checks_d ! 
  END INTERFACE
  !
  ! [C.5] Zuweisungen
  !
  ! [C.6] Operatoren (optional, falls sinnvoll)
  !
!>WIN-NT:  INTERFACE OPERATOR(<)
!>WIN-NT:     MODULE PROCEDURE lt_datetime_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE lt_datetime_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE lt_datetime_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE lt_datetime_1_1  ! Vektor / Vektor
!>WIN-NT:     MODULE PROCEDURE lt_time_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE lt_time_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE lt_time_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE lt_time_1_1  ! Vektor / Vektor
!>WIN-NT:  END INTERFACE
!>WIN-NT:  INTERFACE OPERATOR(<=)
!>WIN-NT:     MODULE PROCEDURE le_datetime_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE le_datetime_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE le_datetime_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE le_datetime_1_1  ! Vektor / Vektor
!>WIN-NT:     MODULE PROCEDURE le_time_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE le_time_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE le_time_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE le_time_1_1  ! Vektor / Vektor
!>WIN-NT:  END INTERFACE
!>WIN-NT:  INTERFACE OPERATOR(>)
!>WIN-NT:     MODULE PROCEDURE gt_datetime_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE gt_datetime_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE gt_datetime_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE gt_datetime_1_1  ! Vektor / Vektor
!>WIN-NT:     MODULE PROCEDURE gt_time_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE gt_time_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE gt_time_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE gt_time_1_1  ! Vektor / Vektor
!>WIN-NT:  END INTERFACE
!>WIN-NT:  INTERFACE OPERATOR(>=)
!>WIN-NT:     MODULE PROCEDURE ge_datetime_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE ge_datetime_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE ge_datetime_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE ge_datetime_1_1  ! Vektor / Vektor
!>WIN-NT:     MODULE PROCEDURE ge_time_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE ge_time_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE ge_time_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE ge_time_1_1  ! Vektor / Vektor
!>WIN-NT:  END INTERFACE
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
!>WIN-NT:  INTERFACE OPERATOR(/=)
!>WIN-NT:     MODULE PROCEDURE ne_datetime_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE ne_datetime_0_1  ! Skalar / Vektor 
!>WIN-NT:    MODULE PROCEDURE ne_datetime_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE ne_datetime_1_1  ! Vektor / Vektor
!>WIN-NT:     MODULE PROCEDURE ne_time_0_0  ! Skalar / Skalar
!>WIN-NT:     MODULE PROCEDURE ne_time_0_1  ! Skalar / Vektor 
!>WIN-NT:     MODULE PROCEDURE ne_time_1_0  ! Vektor / Skalar
!>WIN-NT:     MODULE PROCEDURE ne_time_1_1  ! Vektor / Vektor
!>WIN-NT:  END INTERFACE
  !
  ! [C.7] Liste der oeffentlichen Methoden
  !
  PUBLIC :: FixValue_Check                      ! Checkt, ob Parameter zu der Menge der erlaubten Werte gehoert
  PUBLIC :: ReqIfNotIf_Auswertung               ! An-, bzw.Abwesenheitscheck von Eingabeelementen der Steuerdatei
  PUBLIC :: CheckIfPar_Auswertung               ! Checken bestimmter Parameter-Bedingungen
  PUBLIC :: File_Checks                         ! Checken von Dateienexistenzen
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
  CHARACTER (LEN=31), PARAMETER :: c_modname      = 'm_stdat_checks' ! 
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
  !! UP checkt, ob - falls FixValues vorhanden - die
  !! Eingabewerte zu den erlaubten Werten gehoeren. <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE FixValue_Check_d &
       ( block )
    !
    ! USE-Statements
    !
    ! Package-Modul
    !
    USE m_stringliste, ONLY : &
         ! Routinen
         make_liste
    !
    ! Formalparameter
    !! Variable ..
    TYPE (t_block  )                , INTENT(INOUT)     :: block
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='FixValue_Check_d'

    INTEGER                                  :: i_egb, i, j, k, l, m
    LOGICAL                                  :: l_fehler, l_found
    !
    !! Hilfsvariable A fuer den Vergleich von Zeitangaben
    TYPE (t_datetime)          :: Date_A
    !! Hilfsvariable B fuer den Vergleich von Zeitangaben
    TYPE (t_datetime)          :: Date_B
    !! Hilfsvariable A fuer den Vergleich von Zeitinkrementen
    TYPE (t_time)              :: Incr_A
    !! Hilfsvariable B fuer den Vergleich von Zeitinkrementen
    TYPE (t_time)              :: Incr_B
    !
    INTEGER                                  :: i_zeil
    CHARACTER (LEN=6)                        :: c_zeil, c_egb

    CHARACTER (LEN=25)                       :: c_par
    CHARACTER (LEN = 60)                     :: c_erlaubt

    LOGICAL                                  :: l_tolong
    !
    !
    ! Initialisierungen
    !
    c_erlaubt =  REPEAT(' ', LEN(c_erlaubt))
    l_tolong  =  .False.
    !
    i_zeil  = 0
    !
    l_fehler = .False.
    !
    !   Schleifen zur Adressierung saemtlicher Parameter
    !
    IF ( block%EGBAnz .GT. 0 ) THEN
       !
       DO i_egb = 1, block%EGBAnz
          !
          IF ( any_error( ) ) EXIT
          !
          DO i = 1, SIZE(block%Key)
             !
             IF ( any_error( ) ) EXIT
             !
             IF ( block%Key(i)%ZeilAnz(i_egb) .GT. 0 ) THEN
                !
                DO j = 1, SIZE(block%Key(i)%Par)
                   !
                   IF ( any_error( ) ) EXIT
                   !
                   ! Fallunterscheidung : Parameter-Datentyp
                   !
                   SELECT CASE (block%Key(i)%Par(j)%Type)
                   CASE ('INT')
                      !
                      ! INTEGER
                      !
                      IF ( ASSOCIATED(block%Key(i)%Par(j)%FixValue%int) ) THEN
                         
                         DO k = 1, block%Key(i)%ZeilAnz(i_egb)
                            !
                            IF ( any_error( )  .OR.  l_fehler ) EXIT
                            !
                            IF ( ASSOCIATED(block%Key(i)%Par(j)%EGB(i_egb)%Wert(k)%int) ) THEN
                               
                               DO l = 1, SIZE(block%Key(i)%Par(j)%EGB(i_egb)%Wert(k)%int)
                                  !
                                  IF ( any_error( )  .OR.  l_fehler ) EXIT
                                  !
                                  l_found = .False.
                                  
                                  DO m = 1, SIZE(block%Key(i)%Par(j)%FixValue%int)
                                     
                                     IF ( block%Key(i)%Par(j)%EGB(i_egb)%Wert(k)%int(l) .EQ. &
                                          block%Key(i)%Par(j)%FixValue%int(m) ) THEN
                                        
                                        l_found = .True.

                                     END IF
                                     
                                  END DO  ! m
                                  
                                  IF ( .NOT. l_found ) THEN
                                     
                                     l_fehler = .True.
                                     i_zeil = k
                                     
                                     DO m = 1, SIZE(block%Key(i)%Par(j)%FixValue%int)
                                        !
                                        IF ( any_error( )  .OR.  l_tolong ) EXIT
                                        !
                                        CALL make_liste &
                                             ( block%Key(i)%Par(j)%FixValue%int(m),',', &
                                             c_erlaubt, l_tolong )
                                           !
                                     END DO  ! m
                                     
                                  END IF
                            
                               END DO  ! l

                            END IF   ! ASSO %int

                         END DO  ! k

                      END IF
                      !
                   CASE ('REAL')
                      !
                      ! REAL
                      !
                      IF ( ASSOCIATED(block%Key(i)%Par(j)%FixValue%real) ) THEN
                         
                         DO k = 1, block%Key(i)%ZeilAnz(i_egb)
                            !
                            IF ( any_error( )  .OR.  l_fehler ) EXIT
                            !
                            IF ( ASSOCIATED(block%Key(i)%Par(j)%EGB(i_egb)%Wert(k)%real) ) THEN
                               
                               DO l = 1, SIZE(block%Key(i)%Par(j)%EGB(i_egb)%Wert(k)%real)
                                  !
                                  IF ( any_error( )  .OR.  l_fehler ) EXIT
                                  !
                                  l_found = .False.
                                  
                                  DO m = 1, SIZE(block%Key(i)%Par(j)%FixValue%real)
                                     
                                     IF ( block%Key(i)%Par(j)%EGB(i_egb)%Wert(k)%real(l) .EQ. &
                                          block%Key(i)%Par(j)%FixValue%real(m) ) THEN
                                        
                                        l_found = .True.
                                        
                                     END IF
                                     
                                  END DO  ! m
                                  
                                  IF ( .NOT. l_found ) THEN
                                     
                                     l_fehler = .True.
                                     i_zeil = k
                                     
                                     DO m = 1, SIZE(block%Key(i)%Par(j)%FixValue%real)
                                        !
                                        IF ( any_error( )  .OR.  l_tolong ) EXIT
                                        !
                                        CALL make_liste &
                                             ( block%Key(i)%Par(j)%FixValue%real(m),',', &
                                             c_erlaubt, l_tolong )
                                        !
                                     END DO  ! m
                                     
                                  END IF
                                  
                               END DO  ! l
                               
                            END IF
                               
                         END DO  ! k

                      END IF
                      !
                   CASE ('LOG')
                      !
                      ! LOGICAL
                      !
                      IF ( ASSOCIATED(block%Key(i)%Par(j)%FixValue%log) ) THEN
                         
                         DO k = 1, block%Key(i)%ZeilAnz(i_egb)
                            !
                            IF ( any_error( )  .OR.  l_fehler ) EXIT
                            !
                            IF ( ASSOCIATED(block%Key(i)%Par(j)%EGB(i_egb)%Wert(k)%log) ) THEN
                               
                               DO l = 1, SIZE(block%Key(i)%Par(j)%EGB(i_egb)%Wert(k)%log)
                                  !
                                  IF ( any_error( )  .OR.  l_fehler ) EXIT
                                  !
                                  l_found = .False.
                                  
                                  DO m = 1, SIZE(block%Key(i)%Par(j)%FixValue%log)

                                     IF ( block%Key(i)%Par(j)%EGB(i_egb)%Wert(k)%log(l) .AND. &
                                          block%Key(i)%Par(j)%FixValue%log(m) ) THEN
                                        
                                        l_found = .True.
                                        
                                     ELSE IF (.NOT. block%Key(i)%Par(j)%EGB(i_egb)%Wert(k)%log(l) .AND. &
                                          .NOT. block%Key(i)%Par(j)%FixValue%log(m) ) THEN
                                        
                                        l_found = .True.
                                        
                                     END IF
                                     
                                  END DO  ! m
                                  
                                  IF ( .NOT. l_found ) THEN
                                     
                                     l_fehler = .True.
                                     i_zeil = k
                                     
                                     DO m = 1, SIZE(block%Key(i)%Par(j)%FixValue%log)
                                        !
                                        IF ( any_error( )  .OR.  l_tolong ) EXIT
                                        !
                                        CALL make_liste &
                                             ( block%Key(i)%Par(j)%FixValue%log(m),',', &
                                             c_erlaubt, l_tolong )
                                        !
                                     END DO  ! m
                                     
                                  END IF
                            
                               END DO  ! l

                            END IF

                         END DO  ! k

                      END IF
                      !
                   CASE ('CHAR','FILE','DATE','INCR')
                      !
                      ! CHARACTER, FILE, DATE, INCR
                      !
                      IF ( ASSOCIATED(block%Key(i)%Par(j)%FixValue%char) ) THEN
                         
                         DO k = 1, block%Key(i)%ZeilAnz(i_egb)
                            !
                            IF ( any_error( )  .OR.  l_fehler ) EXIT
                            !
                            IF ( ASSOCIATED(block%Key(i)%Par(j)%EGB(i_egb)%Wert(k)%char) ) THEN
                               
                               DO l = 1, SIZE(block%Key(i)%Par(j)%EGB(i_egb)%Wert(k)%char)
                                  !
                                  IF ( any_error( )  .OR.  l_fehler ) EXIT
                                  !
                                  l_found = .False.
                                  
                                  DO m = 1, SIZE(block%Key(i)%Par(j)%FixValue%char)
                                     
                                     SELECT CASE (block%Key(i)%Par(j)%Type)
                                     CASE ('CHAR','FILE')
                                        
                                        IF ( block%Key(i)%Par(j)%EGB(i_egb)%Wert(k)%char(l) .EQ. &
                                             block%Key(i)%Par(j)%FixValue%char(m) ) THEN
                                           
                                           l_found = .True.
                                           
                                        END IF
                                        
                                     CASE ('DATE')
                                        !
                                        Date_A = string_to_datetime ( &
                                             block%Key(i)%Par(j)%EGB(i_egb)%Wert(k)%char(l) )
                                        !
                                        Date_B = string_to_datetime ( block%Key(i)%Par(j)%FixValue%char(m) )
                                        !
                                        IF ( Date_A == Date_B ) l_found = .True.
                                        !
                                     CASE ('INCR')
                                        !
                                        Incr_A = string_to_time ( &
                                             block%Key(i)%Par(j)%EGB(i_egb)%Wert(k)%char(l) )
                                        !
                                        Incr_B = string_to_time ( block%Key(i)%Par(j)%FixValue%char(m) )
                                        !
                                        IF ( Incr_A == Incr_B ) l_found = .True.
                                        !
                                     END SELECT
                                     
                                  END DO  ! m
                            
                                  IF ( .NOT. l_found ) THEN
                                     
                                     l_fehler = .True.
                                     i_zeil = k

                                     DO m = 1, SIZE(block%Key(i)%Par(j)%FixValue%char)
                                        !
                                        IF ( any_error( )  .OR.  l_tolong ) EXIT
                                        !
                                        CALL make_liste &
                                             ( TRIM(block%Key(i)%Par(j)%FixValue%char(m)),',', &
                                             c_erlaubt, l_tolong )
                                        !
                                     END DO  ! m
                                     
                                  END IF
                                  
                               END DO  ! l

                            END IF

                         END DO  ! k

                      END IF
                      !
                   CASE ('DOUBLE')
                      !
                      ! DOUBLE-REAL
                      !
                      IF ( ASSOCIATED(block%Key(i)%Par(j)%FixValue%doub) ) THEN
                         
                         DO k = 1, block%Key(i)%ZeilAnz(i_egb)
                            !
                            IF ( any_error( )  .OR.  l_fehler ) EXIT
                            !
                            IF ( ASSOCIATED(block%Key(i)%Par(j)%EGB(i_egb)%Wert(k)%doub) ) THEN
                               
                               DO l = 1, SIZE(block%Key(i)%Par(j)%EGB(i_egb)%Wert(k)%doub)
                                  !
                                  IF ( any_error( )  .OR.  l_fehler ) EXIT
                                  !
                                  l_found = .False.
                                  
                                  DO m = 1, SIZE(block%Key(i)%Par(j)%FixValue%doub)
                                     
                                     IF ( block%Key(i)%Par(j)%EGB(i_egb)%Wert(k)%doub(l) .EQ. &
                                          block%Key(i)%Par(j)%FixValue%doub(m) ) THEN
                                        
                                        l_found = .True.
                                        
                                     END IF
                                     
                                  END DO  ! m
                                  
                                  IF ( .NOT. l_found ) THEN
                                     
                                     l_fehler = .True.
                                     i_zeil = k
                                     
                                     DO m = 1, SIZE(block%Key(i)%Par(j)%FixValue%doub)
                                        !
                                        IF ( any_error( )  .OR.  l_tolong ) EXIT
                                        !
                                        CALL make_liste &
                                             ( block%Key(i)%Par(j)%FixValue%doub(m),',', &
                                             c_erlaubt, l_tolong )
                                        !
                                     END DO  ! m
                                     
                                  END IF
                                  
                               END DO  ! l
                                  
                            END IF

                         END DO  ! k

                      END IF   ! ASSOCIATED(block%Key(i)%Par(j)%FixValue%doub)
                      !
                      !
                   END SELECT ! Type
                   !
                   ! Ggf.: Fehlermeldung generieren und ausgeben             
                   !
                   IF ( no_error( )  .AND.  l_fehler ) THEN
                      !       
                      c_egb = REPEAT(' ',LEN(c_egb))
                      ! ... wenn Eingabeblock mehrfach vorkam
                      IF ( block%EGBAnz .GT. 1 ) THEN
                         !
                         WRITE(c_egb,'(I4)') i_egb
                         c_egb  = ADJUSTL(c_egb)
                         c_egb  = ' '//TRIM(c_egb)//'.'
                         !
                      END IF
                      !
                      c_zeil = REPEAT(' ',LEN(c_zeil))
                      ! ... wenn KeyZeile mehrfach vorkam
                      IF ( block%Key(i)%ZeilAnz(i_egb) .GT. 1 ) THEN
                         !
                         WRITE(c_zeil,'(I4)') i_zeil
                         c_zeil  = ADJUSTL(c_zeil)
                         c_zeil  = ' '//TRIM(c_zeil)//'.'
                         !
                      END IF
                      !
                      c_par = REPEAT(' ',LEN(c_par))
                      ! ... Parameternummer, wenns mehrere gibt
                      IF ( SIZE(block%Key(i)%Par) .GT. 1 ) THEN
                         !
                         WRITE(c_par,'(I3)') j
                         c_par = ADJUSTL(c_par)
                         !
                         c_par   = 'fuer den '//TRIM(c_par)//'. Parameter'
                         !
                      END IF
                      !
                      ! ... wenn 'erlaubt'-String zu lang ist
                      IF ( l_tolong  .OR.  &
                           LEN_TRIM(c_erlaubt) .GT. LEN(c_erlaubt) - 2 ) THEN
                         !
                         c_erlaubt = REPEAT(' ', LEN(c_erlaubt))
                         c_erlaubt(1:18) = 'siehe Muster-Datei'
                      ELSE
                         c_erlaubt = '['//TRIM(c_erlaubt)//']'
                      END IF
                      !
                      ! Fehler -2510 : unzulaessiger Parameter-Wert.
                      !              Der Parameter-Wert entspricht keinem der vereinbarten,
                      !              zulaessigen Parameterwerte [FixValues]
                      !
                      CALL setup_error_act ( all_errors(:), -2510, c_upname, c_modname )
                      CALL setup_error_act ( '<block_nr>', TRIM(c_egb) )
                      CALL setup_error_act ( '<blockname>', TRIM(block%Name) )
                      CALL setup_error_act ( '<key_nr>', TRIM(c_zeil) )
                      CALL setup_error_act ( '<key_name>', TRIM(block%Key(i)%Name) )
                      CALL setup_error_act ( '<par_nr>', TRIM(c_par) )
                      CALL setup_error_act ( '<erlaubt_string>', TRIM(c_erlaubt) )
                      !
                      RETURN
                      !
                   END IF
                   !   
                END DO  ! j
                !
             END IF  ! block%Key(i)%ZeilAnz .GT. 0
             !
          END DO  ! i
          !
       END DO ! i_egb  Schleife ueber alle Eingabebloecke dieser Definition
       !
    END IF   ! ( block%EGBAnz .GT. 0 )
    !
    !
  END SUBROUTINE FixValue_Check_d
  !
  !! UP wertet ReqIf-,NotIf-Bedingungen aus <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE  ReqIfNotIf_Auswertung_d &
       ( bloecke )
    !
    ! Formalparameter
    !! Feld mit Daten der einzelnen Eingabebloecke
    TYPE (t_block) , DIMENSION(:)    , POINTER           :: bloecke
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='ReqIfNotIf_Auswertung_d'
    !! Fehlerkennung
    INTEGER                 ::  ierr=0
    !
    INTEGER                 :: i, j, k, l, m, n
    INTEGER                 :: i_egb, i_bl, i_key, m_end, i_zeil
    LOGICAL                 :: l_erfuellt
    !! l_zw : True, wenn zeilenweise Auswertung der Anwesenheitsbedingung
    LOGICAL                 :: l_zw
    !! l_ck : False, wenn kein Check der Anwesenheitsbedingung durchgefuehrt
    !!       werden soll
    LOGICAL                 :: l_ck
    CHARACTER (LEN=3)       :: c_parpos
    CHARACTER (LEN=6)       :: c_egb, c_zeil
    !
    !
    !   Die Existenzen der einzelnen Datei-Objekte [Bloecke, Schluesselwortzeilen
    !   und Eingabewerte] werden uberprueft ! 
    !   Die Dictionary-Datei wird dabei von hinten aufgerollt ( der zuletzt
    !   vereinbarte Block zuerst gecheckt). ParameterExistenzen werden vor
    !   Schluesselwortzeilen und diese vor BlockAnwesenheiten geprueft !
    !   
    DO i = SIZE(bloecke), 1, -1     !  Schleife ueber alle Eingabebloecke
       !
       IF ( any_error( ) ) EXIT
       !
       IF ( bloecke(i)%EGBAnz .GT. 0 ) THEN
          
          DO i_egb = 1, bloecke(i)%EGBAnz      ! ueber alle Bloecke desselben Typs
             !
             IF ( any_error( ) ) EXIT
             !
             c_egb = REPEAT(' ',LEN(c_egb))
             
             IF ( bloecke(i)%EGBAnz .GT. 1 ) THEN
                WRITE(c_egb,'(I4)') i_egb
                c_egb = ADJUSTL(c_egb)
                c_egb = ' '//TRIM(c_egb)//'.'
             END IF
             
             
             DO j = SIZE(bloecke(i)%Key), 1, -1
                !
                IF ( any_error( ) ) EXIT
                !
                IF ( bloecke(i)%Key(j)%ZeilAnz(i_egb) .GT. 0 ) THEN
                   
                   DO k = SIZE(bloecke(i)%Key(j)%Par), 1, -1
                      !
                      IF ( any_error( ) ) EXIT
                      !
                      c_parpos = REPEAT(' ', LEN(c_parpos))
                      WRITE(c_parpos,'(I3)') k      
                      c_parpos = ADJUSTL(c_parpos)
                      !
                      !
                      IF ( ASSOCIATED(bloecke(i)%Key(j)%Par(k)%ReqIf) ) THEN
                         !
                         ! ... Parameter ReqIf-Anforderung :
                         !
                         DO l = 1, SIZE(bloecke(i)%Key(j)%Par(k)%ReqIf)  ! ueber alle ReqIf-Bedingungen des Parameters
                            !
                            IF ( any_error( ) ) EXIT
                            !
                            l_zw = .False.
                            l_ck = .True.
                            !
                            ! Wenn Vergleichsparameter in Mehrfachkeyzeile, dann muss zeilenweise (l_zw = T)
                            ! ausgewertet werden
                            ! [ Objekt & VGL-Objekt befinden sich dann in derselben Zeile, da nur dann fuer
                            !   den Key des Vergleichsobjektes  L_Single = False  gelten darf
                            !   (wird in Check_ReqIfNotIf  mod_dic_checks  geprueft) ]
                            !
                            IF ( bloecke(i)%Key(j)%Par(k)%ReqIf(l)%objekt%typ(1:3) .EQ. 'PAR' ) THEN
                               
                               ! ... Adressnummern des Vergleichsobjektes
                               i_bl  = bloecke(i)%Key(j)%Par(k)%ReqIf(l)%objekt%bl_nr
                               i_key = bloecke(i)%Key(j)%Par(k)%ReqIf(l)%objekt%key_nr
                               
                               IF ( .NOT. bloecke(i_bl)%Key(i_key)%L_Single ) THEN
                                  
                                  IF (ASSOCIATED(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert)) THEN
                                     
                                     l_zw = .True.
                                     m_end = SIZE(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert)
                                     
                                  ELSE
                                        
                                     l_ck = .False.

                                  END IF

                               ELSE
                               
                                  m_end = 1

                               END IF

                            ELSE
                               
                               m_end = 1

                            END IF
                            !
                            ! -> Check wird durchgefuehrt !
                            !
                            IF ( l_ck ) THEN
                               
                               DO m = 1, m_end
                                  !
                                  IF ( any_error( ) ) EXIT
                                  !
                                  CALL ReqIfNotIf_Erfuellt &
                                       ( bloecke(i)%Key(j)%Par(k)%ReqIf(l), i, &
                                       bloecke, l_erfuellt, i_egb, m )
                                  !
                                  IF ( no_error( )  .AND.  l_erfuellt ) THEN
                                     
                                     IF ( .NOT. ASSOCIATED(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert) ) THEN
                                        
                                        ierr = 5
                                        
                                     ELSE
                                        
                                        IF ( l_zw ) THEN
                                           
                                           SELECT CASE (bloecke(i)%Key(j)%Par(k)%Type )
                                           CASE('INT')
                                              IF ( .NOT. ASSOCIATED(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert(m)%int) ) &
                                                   ierr = 5
                                           CASE('REAL')
                                              IF ( .NOT. ASSOCIATED(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert(m)%real) ) &
                                                   ierr = 5
                                           CASE('LOG')
                                              IF ( .NOT. ASSOCIATED(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert(m)%log) ) &
                                                   ierr = 5
                                           CASE('CHAR','FILE','DATE','INCR')
                                              IF ( .NOT. ASSOCIATED(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert(m)%char) ) &
                                                   ierr = 5
                                           CASE('DOUBLE')
                                              
                                              IF ( .NOT. ASSOCIATED(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert(m)%doub)) &
                                                   ierr = 5
                                              
                                           END SELECT

                                           IF ( ierr .EQ. 5 ) i_zeil = m
                                           

                                        ELSE

                                           SELECT CASE (bloecke(i)%Key(j)%Par(k)%Type )
                                           CASE('INT')
                                              
                                              DO n = 1, SIZE(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert)
                                                 
                                                 IF ( ierr .EQ. 5 ) EXIT
                                                 
                                                 IF (.NOT. ASSOCIATED(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert(n)%int)) THEN
                                                    ierr = 5
                                                    i_zeil = n
                                                 END IF
                                                 
                                              END DO
                                              
                                           CASE('REAL')
                                              
                                              DO n = 1, SIZE(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert)
                                                 
                                                 IF ( ierr .EQ. 5 ) EXIT
                                                 
                                                 IF (.NOT. ASSOCIATED(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%wert(n)%real)) THEN
                                                    ierr = 5
                                                    i_zeil = n
                                                 END IF
                                              
                                              END DO

                                           CASE('LOG')

                                              DO n = 1, SIZE(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert)
                                                 
                                                 IF ( ierr .EQ. 5 ) EXIT
                                                 
                                                 IF (.NOT. ASSOCIATED(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert(n)%log)) THEN
                                                    ierr = 5
                                                    i_zeil = n
                                                 END IF
                                                 
                                              END DO

                                           CASE('CHAR','FILE','DATE','INCR')

                                              DO n = 1, SIZE(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert)

                                                 IF ( ierr .EQ. 5 ) EXIT
                                                 
                                                 IF (.NOT. ASSOCIATED(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert(n)%char)) THEN
                                                    ierr = 5
                                                    i_zeil = n
                                                 END IF
                                                 
                                              END DO

                                           CASE('DOUBLE')
               
                                              DO n = 1, SIZE(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert)
                                                 
                                                 IF ( ierr .EQ. 5 ) EXIT
                                                 
                                                 IF (.NOT. ASSOCIATED(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert(n)%doub)) THEN
                                                    ierr = 5
                                                    i_zeil = n
                                                 END IF

                                              END DO

                                           END SELECT   ! (bloecke(i)%Key(j)%Par(k)%Type )
                                        
                                        END IF   ! Fallunterscheidung : l_zw

                                     END IF   ! FU : ASSOCIA..(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert)

                                  END IF   ! no_error( )  .AND.  l_erfuellt

                                  IF ( no_error( )  .AND.  ierr .EQ. 5 ) THEN
                                     !
                                     ! Fehler -2560 : erforderlicher Parameterwert fehlt
                                     !
                                     c_zeil = REPEAT(' ',LEN(c_zeil))
                                     IF ( bloecke(i)%Key(j)%ZeilAnz(i_egb) .GT. 1 ) THEN
                                        WRITE(c_zeil,'(I4)') i_zeil
                                        c_zeil = ADJUSTL(c_zeil)
                                        c_zeil = ' '//TRIM(c_zeil)//'.'
                                     END IF
                                     !
                                     CALL setup_error_act ( all_errors(:), -2560, c_upname, c_modname )
                                     CALL setup_error_act ( '<block_nr>', TRIM(c_egb) )
                                     CALL setup_error_act ( '<blockname>', TRIM(bloecke(i)%Name) )
                                     CALL setup_error_act ( '<zeilen_nr>', TRIM(c_zeil) )
                                     CALL setup_error_act ( '<schluessel>', TRIM(bloecke(i)%Key(j)%Name) )
                                     CALL setup_error_act ( '<par_nr>', TRIM(c_parpos) )
                                     !
                                     RETURN
                                     !
                                  END IF   ! ( no_error( )  .AND.  ierr .EQ. 5 )

                               END DO   ! m = 1, m_end

                            END IF   ! ( l_ck )

                         END DO   ! l

                      END IF   ! asso Req
                      !
                      !
                      IF ( no_error( )  .AND.  ASSOCIATED(bloecke(i)%Key(j)%Par(k)%NotIf) ) THEN
                         !
                         ! ... Parameter NotIf-Anforderung :
                         !
                         DO l = 1, SIZE(bloecke(i)%Key(j)%Par(k)%NotIf)
                            !
                            IF ( any_error( ) ) EXIT
                            !
                            l_zw = .False.
                            l_ck = .True.
                            !
                            ! Wenn Vergleichsparameter in Mehrfachkeyzeile, dann muss zeilenweise (l_zw = T)
                            ! ausgewertet werden
                            ! [ Objekt & VGL-Objekt befinden sich dann in derselben Zeile, da nur dann fuer
                            !   den Key des Vergleichsobjektes  L_Single = False  gelten darf
                            !   (wird in Check_ReqIfNotIf  mod_dic_checks  geprueft) ]
                            ! 
                            IF ( bloecke(i)%Key(j)%Par(k)%NotIf(l)%objekt%typ(1:3) .EQ. 'PAR' ) THEN
                               
                               ! ... Adressnummern des Vergleichsobjektes
                               i_bl  = bloecke(i)%Key(j)%Par(k)%NotIf(l)%objekt%bl_nr
                               i_key = bloecke(i)%Key(j)%Par(k)%NotIf(l)%objekt%key_nr
                               
                               IF ( .NOT. bloecke(i_bl)%Key(i_key)%L_Single ) THEN
                                  
                                  IF (ASSOCIATED(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert)) THEN
                                     
                                     l_zw = .True.
                                     m_end = SIZE(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert)
                                     
                                  ELSE

                                     l_ck = .False.

                                  END IF

                               ELSE

                                  m_end = 1

                               END IF

                            ELSE

                               m_end = 1

                            END IF
                            !
                            ! -> Check wird durchgefuehrt !
                            !
                            IF ( l_ck ) THEN
                               
                               DO m = 1, m_end
                                  !
                                  IF ( any_error( ) ) EXIT
                                  !
                                  CALL ReqIfNotIf_Erfuellt &
                                       ( bloecke(i)%Key(j)%Par(k)%NotIf(l), i, &
                                       bloecke, l_erfuellt, i_egb, m )
                                  !
                                  IF ( no_error( )  .AND.  l_erfuellt ) THEN
                                     
                                     IF ( ASSOCIATED(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert) ) THEN
                                        
                                        IF ( l_zw ) THEN
                                           
                                           SELECT CASE (bloecke(i)%Key(j)%Par(k)%Type )
                                           CASE('INT')
                                              IF ( ASSOCIATED(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert(m)%int) ) &
                                                   ierr = 6
                                           CASE('REAL')
                                              IF ( ASSOCIATED(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert(m)%real) ) &
                                                   ierr = 6
                                           CASE('LOG')
                                              IF ( ASSOCIATED(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert(m)%log) ) &
                                                   ierr = 6
                                           CASE('CHAR','FILE','DATE','INCR')
                                              IF ( ASSOCIATED(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert(m)%char) ) &
                                                   ierr = 6
                                           CASE('DOUBLE')
                                              IF ( ASSOCIATED(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert(m)%doub) ) &
                                                   ierr = 6
                                              
                                           END SELECT

                                           IF ( ierr .EQ. 6 ) i_zeil = m


                                        ELSE

                                           SELECT CASE (bloecke(i)%Key(j)%Par(k)%Type )
                                           CASE('INT')
                                              
                                              DO n = 1, SIZE(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert)
                                                 
                                                 IF ( ierr .EQ. 6 ) EXIT
                                                 
                                                 IF ( ASSOCIATED(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert(n)%int) ) THEN
                                                    ierr = 6
                                                    i_zeil = n
                                                 END IF
                                                 
                                              END DO

                                           CASE('REAL')

                                              DO n = 1, SIZE(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert)

                                                 IF ( ierr .EQ. 6 ) EXIT

                                                 IF ( ASSOCIATED(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert(n)%real) ) THEN
                                                    ierr = 6
                                                    i_zeil = n
                                                 END IF

                                              END DO

                                           CASE('LOG')

                                              DO n = 1, SIZE(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert)

                                                 IF ( ierr .EQ. 6 ) EXIT

                                                 IF ( ASSOCIATED(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert(n)%log) ) THEN
                                                    ierr = 6
                                                    i_zeil = n
                                                 END IF
         
                                              END DO

                                           CASE('CHAR','FILE','DATE','INCR')

                                              DO n = 1, SIZE(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert)

                                                 IF ( ierr .EQ. 6 ) EXIT

                                                 IF ( ASSOCIATED(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert(n)%char) ) THEN
                                                    ierr = 6
                                                    i_zeil = n
                                                 END IF

                                              END DO

                                           CASE('DOUBLE')

                                              DO n = 1, SIZE(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert)

                                                 IF ( ierr .EQ. 6 ) EXIT

                                                 IF ( ASSOCIATED(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert(n)%doub) ) THEN
                                                    ierr = 6
                                                    i_zeil = n
                                                 END IF

                                              END DO

                                           END SELECT   ! (bloecke(i)%Key(j)%Par(k)%Type )

                                        END IF   ! Fallunterscheidung : l_zw

                                     END IF   ! ASSOCIA..(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert)

                                  END IF   ! no_error( )  .AND.  l_erfuellt
                                  
                                  IF ( no_error( )  .AND.  ierr .EQ. 6 ) THEN
                                     !
                                     ! Fehler -2570 : unzulaessige Parameterwert-Angabe in Schluesselwortzeile
                                     !
                                     c_zeil = REPEAT(' ',LEN(c_zeil))
                                     IF ( bloecke(i)%Key(j)%ZeilAnz(i_egb) .GT. 1 ) THEN
                                        WRITE(c_zeil,'(I4)') i_zeil
                                        c_zeil = ADJUSTL(c_zeil)
                                        c_zeil = ' '//TRIM(c_zeil)//'.'
                                     END IF
                                     !
                                     CALL setup_error_act ( all_errors(:), -2570, c_upname, c_modname )
                                     CALL setup_error_act ( '<block_nr>', TRIM(c_egb) )
                                     CALL setup_error_act ( '<blockname>', TRIM(bloecke(i)%Name) )
                                     CALL setup_error_act ( '<zeilen_nr>', TRIM(c_zeil) )
                                     CALL setup_error_act ( '<schluessel>', TRIM(bloecke(i)%Key(j)%Name) )
                                     CALL setup_error_act ( '<par_nr>', TRIM(c_parpos) )
                                     !
                                     RETURN
                                     !
                                  END IF  ! no_error( )  .AND.  ierr .EQ. 6

                               END DO   ! m

                            END IF   ! ( l_ck )

                         END DO   ! l

                      END IF   ! asso Not

                   END DO   ! k = SIZE(bloecke(i)%Key(j)%Par), 1, -1

                END IF   ! ( bloecke(i)%Key(j)%ZeilAnz .GT. 0 )
                !
                !
                ! KEY-Existenzen pruefen
                !
                IF ( no_error( )  .AND.  ASSOCIATED(bloecke(i)%Key(j)%ReqIf) ) THEN 
                   !
                   ! ... wenn KEY einer ReqIf-Anforderung unterliegt
                   !
                   DO k = 1, SIZE(bloecke(i)%Key(j)%ReqIf)
                      !
                      IF ( any_error( ) ) EXIT
                      !
                      CALL ReqIfNotIf_Erfuellt &
                           ( bloecke(i)%Key(j)%ReqIf(k), i, &
                           bloecke, l_erfuellt, i_egb )
                      !
                      IF ( no_error( ) ) THEN
                         !
                         IF ( l_erfuellt .AND. &
                              bloecke(i)%Key(j)%ZeilAnz(i_egb) .EQ. 0 ) THEN
                            !
                            ! Fehler -2540 : Eine erforderliche Schluesselwortzeile fehlt
                            !
                            CALL setup_error_act ( all_errors(:), -2540, c_upname, c_modname )
                            CALL setup_error_act ( '<block_nr>', TRIM(c_egb) )
                            CALL setup_error_act ( '<blockname>', TRIM(bloecke(i)%Name) )
                            CALL setup_error_act ( '<schluessel>', TRIM(bloecke(i)%Key(j)%Name) )
                            !
                            RETURN
                            !
                         END IF
                         !
                      END IF ! no_error( )
                      !
                   END DO  ! k
                   !
                END IF   ! no_error( )  .AND.  ASSO.. (bloecke(i)%Key(j)%ReqIf)
                !
                !
                IF ( no_error( )  .AND.  ASSOCIATED(bloecke(i)%Key(j)%NotIf) ) THEN
                   !
                   ! ... wenn KEY einer NotIf-Anforderung unterliegt
                   !
                   DO k = 1, SIZE(bloecke(i)%Key(j)%NotIf)
                      !
                      IF ( any_error( ) ) EXIT
                      !
                      CALL ReqIfNotIf_Erfuellt &
                           ( bloecke(i)%Key(j)%NotIf(k), i, &
                           bloecke, l_erfuellt, i_egb )
                      !
                      IF ( no_error( ) ) THEN
                         !
                         IF ( l_erfuellt .AND. &
                              bloecke(i)%Key(j)%ZeilAnz(i_egb) .GT. 0 ) THEN
                            !
                            ! Fehler -2550 : Block enthaelt eine unzulaessige Schluesselwort-Zeile
                            !
                            CALL setup_error_act ( all_errors(:), -2550, c_upname, c_modname )
                            CALL setup_error_act ( '<block_nr>', TRIM(c_egb) )
                            CALL setup_error_act ( '<blockname>', TRIM(bloecke(i)%Name) )
                            CALL setup_error_act ( '<schluessel>', TRIM(bloecke(i)%Key(j)%Name) )
                            !
                            RETURN
                            !
                         END IF
                         !
                      END IF ! no_error( )
                      !
                   END DO   ! k = 1, SIZE(bloecke(i)%Key(j)%NotIf)
                   !
                END IF   ! no_error( )  .AND.  ASSO.. (bloecke(i)%Key(j)%NotIf)
                !
                !
             END DO   ! j = SIZE(bloecke(i)%Key), 1, -1
             !
          END DO   ! i_egb = 1, bloecke(i)%EGBAnz
          !
       END IF   ! ( bloecke(i)%EGBAnz .GT. 0 )
       !
       !
       ! BLOCK-Existenzen pruefen
       !
       IF ( no_error( )  .AND.  ASSOCIATED(bloecke(i)%ReqIf) ) THEN
          !
          ! ... wenn BLOCK einer ReqIf-Anforderung unterliegt
          !
          DO j = 1, SIZE(bloecke(i)%ReqIf)
             !
             IF ( any_error( ) ) EXIT
             !
             CALL ReqIfNotIf_Erfuellt &
                  ( bloecke(i)%ReqIf(j), i, bloecke, l_erfuellt )
             !
             IF ( no_error( ) ) THEN
                !
                IF ( l_erfuellt .AND. bloecke(i)%EGBAnz .EQ. 0 ) THEN
                   !
                   ! Fehler -2520 : In der Eingabesteuer-Datei fehlt ein
                   !              bestimmter Eingabeblock [ReqIf]
                   !
                   CALL setup_error_act ( all_errors(:), -2520, c_upname, c_modname )
                   CALL setup_error_act ( '<blockname>', TRIM(bloecke(i)%Name) )
                   !
                   RETURN
                   !
                END IF
                !
             END IF ! no_error( )
             
          END DO   ! j = 1, SIZE(bloecke(i)%ReqIf)
          
       END IF   ! ( no_error( )  .AND.  ASSO...(bloecke(i)%ReqIf) )
       !
       !
       IF ( no_error( )  .AND.  ASSOCIATED(bloecke(i)%NotIf) ) THEN
          !
          ! ... wenn BLOCK einer NotIf-Anforderung unterliegt
          !
          DO j = 1, SIZE(bloecke(i)%NotIf)
             !
             IF ( any_error( ) ) EXIT
             !
             CALL ReqIfNotIf_Erfuellt &
                  ( bloecke(i)%NotIf(j), i, bloecke, l_erfuellt )
             !
             IF ( no_error( ) ) THEN
                !
                IF ( l_erfuellt .AND. bloecke(i)%EGBAnz .GT. 0 ) THEN
                   !
                   ! Fehler -2530 : Die Eingabesteuer-Datei enthaelt einen
                   !              unerwuenschten Eingabeblock [NotIf]
                   !
                   CALL setup_error_act ( all_errors(:), -2530, c_upname, c_modname )
                   CALL setup_error_act ( '<blockname>', TRIM(bloecke(i)%Name) )
                   !
                   RETURN
                   !
                END IF
                !
             END IF ! no_error( )

          END DO   ! j = 1, SIZE(bloecke(i)%NotIf)

       END IF   ! ( no_error( )  .AND.  ASSO...(bloecke(i)%NotIf) )
       !
       !
    END DO   ! i = SIZE(bloecke), 1, -1
    !
    !
  END SUBROUTINE ReqIfNotIf_Auswertung_d
  !
  !! UP wertet CheckIfPar-Anforderung aus <BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE  CheckIfPar_Auswertung_d &
       ( bloecke )
    !
    ! Formalparameter
    !! Feld mit Daten der einzelnen Eingabebloecke
    TYPE (t_block) , DIMENSION(:)    , POINTER           :: bloecke
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='CheckIfPar_Auswertung_d'
    !
    INTEGER                              :: i, j, k, l, i_bl, i_key, i_par
    !
    CHARACTER (LEN=20)                   :: c_oper
    CHARACTER (LEN=line_len)             :: c_wert
    CHARACTER (LEN=5)                    :: c_einer
    CHARACTER (LEN=1)                    :: c_n
    CHARACTER (LEN=3)                    :: c_parpos
    CHARACTER (LEN=25)                   :: c_par1, c_par2
 
    INTEGER                              :: i_egb
    CHARACTER (LEN=6)                    :: c_egb

    LOGICAL                              :: l_erfuellt
    !
    !
    ! weitere Initialisierungen
    !
    c_oper    =  REPEAT(' ', LEN(c_oper))
    c_wert    =  REPEAT(' ', LEN(c_wert))
    !
    c_einer   =  REPEAT(' ', LEN(c_einer))
    c_n       =  REPEAT(' ', LEN(c_n))
    c_parpos  =  REPEAT(' ', LEN(c_parpos)) 
    c_par1    =  REPEAT(' ', LEN(c_par1)) 
    c_par2    =  REPEAT(' ', LEN(c_par2))
    !
    i_bl  = 0
    i_key = 0
    i_par = 0
    !
    !
    DO i = 1, SIZE(bloecke)           ! ueber alle Block-Typen
       !
       IF ( any_error( ) ) EXIT
       !
       IF ( bloecke(i)%EGBAnz .GT. 0 ) THEN
          !
          DO i_egb = 1, bloecke(i)%EGBAnz    ! ueber alle Eingabebloecke diesen Block-Typs 
             !
             IF ( any_error( ) ) EXIT
             !
             c_egb = REPEAT(' ',LEN(c_egb))
             !
             IF ( bloecke(i)%EGBAnz .GT. 1 ) THEN
                WRITE(c_egb,'(I4)') i_egb
                c_egb = ADJUSTL(c_egb)
                c_egb = ' '//TRIM(c_egb)//'.'
             END IF
             !
             DO j = 1, SIZE(bloecke(i)%Key) 
                !
                IF ( any_error( ) ) EXIT
                !
                IF ( bloecke(i)%Key(j)%ZeilAnz(i_egb) .GT. 0 ) THEN
                   !
                   DO k = 1, SIZE(bloecke(i)%Key(j)%Par)
                      !
                      IF ( any_error( ) ) EXIT
                      !
                      IF ( ASSOCIATED(bloecke(i)%Key(j)%Par(k)%CheckIf) ) THEN
                         !
                         DO l = 1, SIZE(bloecke(i)%Key(j)%Par(k)%CheckIf)
                            !
                            IF ( any_error( ) ) EXIT
                            !
                            CALL CheckIfPar_Erfuellt &
                                 ( i, i_egb, bloecke(i)%Key(j)%Par(k), &
                                 bloecke(i)%Key(j)%Par(k)%CheckIf(l), &
                                 bloecke, l_erfuellt )
                            !
                            IF ( no_error( )  .AND.  .NOT. l_erfuellt ) THEN
                               !
                               ! Fehlermeldung  generieren :
                               !
                               ! ... wenn KeyZeile mehrfach vorkommen kann
                               !
                               IF ( bloecke(i)%Key(j)%ZeilAnz(i_egb) .GT. 1 ) THEN
                                  !
                                  c_einer = 'einer'
                                  c_n     = 'n'
                                  !
                               END IF
                               !
                               !... Parameternummer, wenns mehrere gibt
                               !
                               IF ( SIZE(bloecke(i)%Key(j)%Par) .GT. 1 ) THEN
                                  !
                                  WRITE(c_parpos,'(I3)') k
                                  c_parpos = ADJUSTL(c_parpos)
                                  !
                                  c_par1   = 'fuer den '//TRIM(c_parpos)//'. Parameter'
                                  !
                               END IF
                               !
                               IF ( LEN_TRIM(bloecke(i)%Key(j)%Par(k)%CheckIf(l)%objekt%typ) .EQ. 0 ) THEN
                                  !
                                  ! Parameterwert mit direkt in Check angegebenen Wert vergleichen
                                  !
                                  ! ... c_oper bauen
                                  !
                                  SELECT CASE(bloecke(i)%Key(j)%Par(k)%CheckIf(l)%oper)
                                  CASE('EQ')
                                     c_oper(1:8)  = 'ungleich'
                                  CASE('NE')
                                     c_oper(1:6)  = 'gleich'
                                  CASE('LE')
                                     c_oper(1:12) = 'groesser als'
                                  CASE('LT')
                                     c_oper(1:15) = 'groesser-gleich'
                                  CASE('GE')
                                     c_oper(1:11) = 'kleiner als'
                                  CASE('GT')
                                     c_oper(1:14) = 'kleiner-gleich'
                                  END SELECT
                                  !
                                  ! ... c_wert bauen :
                                  !
                                  SELECT CASE(bloecke(i)%Key(j)%Par(k)%Type)
                                  CASE('INT')
                                     WRITE(c_wert,*) (bloecke(i)%Key(j)%Par(k)%CheckIf(l)%wert%int(1))
                                  CASE('REAL')
                                     WRITE(c_wert,*) (bloecke(i)%Key(j)%Par(k)%CheckIf(l)%wert%real(1))
                                  CASE('LOG')
                                     IF ( bloecke(i)%Key(j)%Par(k)%CheckIf(l)%wert%log(1) ) THEN
                                        c_wert(1:4) = 'TRUE'
                                     ELSE
                                        c_wert(1:5) = 'FALSE'
                                     END IF
                                  CASE('CHAR','FILE','DATE','INCR')
                                     WRITE(c_wert,*) (bloecke(i)%Key(j)%Par(k)%CheckIf(l)%wert%char(1))
                                  CASE('DOUBLE')
                                     WRITE(c_wert,*) (bloecke(i)%Key(j)%Par(k)%CheckIf(l)%wert%doub(1))
                                  END SELECT
                                  !
                                  c_wert = ADJUSTL(c_wert)
                                  !
                                  ! Fehler -2580 : Eingegebener Parameterwert haelt geforderter
                                  !              Vergleichsoperation mit einem bestimmten Wert nicht stand
                                  !
                                  CALL setup_error_act ( all_errors(:), -2580, c_upname, c_modname )
                                  CALL setup_error_act ( '<block_nr>', TRIM(c_egb) )
                                  CALL setup_error_act ( '<blockname>', TRIM(bloecke(i)%Name) )
                                  CALL setup_error_act ( '<einer>', TRIM(c_einer) )
                                  CALL setup_error_act ( '<n>', TRIM(c_n) )
                                  CALL setup_error_act ( '<keyname>', TRIM(bloecke(i)%Key(j)%Name) )
                                  CALL setup_error_act ( '<fuer_par_x>', TRIM(c_par1) )
                                  CALL setup_error_act ( '<vgl_op>', TRIM(c_oper) )
                                  CALL setup_error_act ( '<als_wert_y>', TRIM(c_wert) )
                                  !
                               ELSE
                                  !
                                  ! Parameterwert mit Wert eines im Check adressierten
                                  ! Vergleichsparameters vergleichen
                                  !
                                  ! ... c_oper bauen :
                                  !
                                  SELECT CASE(bloecke(i)%Key(j)%Par(k)%CheckIf(l)%oper)
                                  CASE('EQ')
                                     c_oper(1:12)  = 'ungleich dem'
                                  CASE('NE')
                                     c_oper(1:10)  = 'gleich dem'
                                  CASE('LE')
                                     c_oper(1:16)  = 'groesser als der'
                                  CASE('LT')
                                     c_oper(1:19)  = 'groesser-gleich dem'
                                  CASE('GE')
                                     c_oper(1:15)  = 'kleiner als der'
                                  CASE('GT')
                                     c_oper(1:18)  = 'kleiner-gleich dem'
                                  END SELECT
                                  !
                                  i_bl   = bloecke(i)%Key(j)%Par(k)%CheckIf(l)%objekt%bl_nr
                                  i_key  = bloecke(i)%Key(j)%Par(k)%CheckIf(l)%objekt%key_nr
                                  i_par  = bloecke(i)%Key(j)%Par(k)%CheckIf(l)%objekt%par_nr
                                  !
                                  ! ... Parameternummer des Vergleichsobjektes, wenns mehrere gibt
                                  !
                                  IF ( SIZE(bloecke(i_bl)%Key(i_key)%Par) .GT. 1 ) THEN
                                     !
                                     WRITE(c_parpos,'(I3)') i_par
                                     c_parpos = ADJUSTL(c_parpos)
                                     !
                                     c_par2   = 'des '//TRIM(c_parpos)//'. Parameters'
                                     !
                                  END IF
                                  !
                                  ! Fehler -2590 : Eingegebener Parameterwert haelt geforderter Vergleichsoperation
                                  !              mit bestimmten anderen Parameter nicht stand
                                  !
                                  CALL setup_error_act ( all_errors(:), -2590, c_upname, c_modname )
                                  CALL setup_error_act ( '<block_nr>', TRIM(c_egb) )
                                  CALL setup_error_act ( '<blockname>', TRIM(bloecke(i)%Name) )
                                  CALL setup_error_act ( '<einer>', TRIM(c_einer) )
                                  CALL setup_error_act ( '<n>', TRIM(c_n) )
                                  CALL setup_error_act ( '<keyname>', TRIM(bloecke(i)%Key(j)%Name) )
                                  CALL setup_error_act ( '<fuer_par_x>', TRIM(c_par1) )
                                  CALL setup_error_act ( '<vgl_op>', TRIM(c_oper) )
                                  CALL setup_error_act ( '<als_wert_y>', TRIM(c_par2) )
                                  CALL setup_error_act ( '<vgl_key>', TRIM(bloecke(i_bl)%Key(i_key)%Name) )
                                  CALL setup_error_act ( '<vgl_block>', TRIM(bloecke(i_bl)%Name) )
                                  !
                               END IF
                               !
                               RETURN
                               !
                            END IF   ! ( no_error( )  .AND.  .NOT. l_erfuellt )
                            !
                         END DO   ! l = 1, SIZE(bloecke(i)%Key(j)%Par(k)%CheckIf)
                         !
                      END IF   ! ASSOCIATED(bloecke(i)%Key(j)%Par(k)%CheckIf
                      !
                   END DO   ! k = 1, SIZE(bloecke(i)%Key(j)%Par)
                   !
                END IF   ! ( bloecke(i)%Key(j)%ZeilAnz(i_egb) .GT. 0 )
                !
             END DO   ! j = 1, SIZE(bloecke(i)%Key)
             !
          END DO   ! i_egb = 1, bloecke(i)%EGBAnz
          !
       END IF   ! ( bloecke(i)%EGBAnz .GT. 0 )
       !
    END DO   ! i = 1, SIZE(bloecke)
    !
    !
  END SUBROUTINE CheckIfPar_Auswertung_d
  !
  !! UP fuehrt File-Checks durch<BR>
  !! Subroutine erzeugt Fehlermeldungen
  SUBROUTINE  File_Checks_d &
       ( bloecke )
    !
    ! USE-Statements :
    !
    ! BASIS-Modul fuer das File-Handling
    !
    USE b_file, ONLY : &
       ! Typdefinition
       t_file,         &
       ! Routinen
       new_file,                &
       kill_file,               &
       set_file_path_and_name,  &
       get_file_name,  &
       get_file_path,  &
       file_exists,    &
       file_exists_not
    !
    ! Formalparameter
    !! Feld mit Daten der einzelnen Eingabebloecke
    TYPE (t_block) , DIMENSION(:)    , POINTER           :: bloecke
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='File_Checks_d'
    !! Hilfsvariable fuer Datei-Existenztest
    TYPE (t_file)                 :: test_dat
    !
    INTEGER                              :: i, j, k, l, m

    LOGICAL                              :: lreq, lnew

    INTEGER                              :: i_egb
    !
    !
    !
    !
    DO i = 1, SIZE(bloecke)
       !
       IF ( any_error( ) ) EXIT
       !
       IF ( bloecke(i)%EGBAnz .GT. 0 ) THEN
          !
          DO i_egb = 1, bloecke(i)%EGBAnz
             !
             IF ( any_error( ) ) EXIT
             !
             DO j = 1, SIZE(bloecke(i)%Key)
                !
                IF ( any_error( ) ) EXIT
                !
                DO k = 1, SIZE(bloecke(i)%Key(j)%Par)
                   !
                   IF ( any_error( ) ) EXIT
                   !
                   IF ( bloecke(i)%Key(j)%Par(k)%Type .EQ. 'FILE' ) THEN
                      !
                      IF ( bloecke(i)%Key(j)%Par(k)%L_FilReq  .OR. &
                           bloecke(i)%Key(j)%Par(k)%L_FilNew        ) THEN
                         !  
                         IF ( ASSOCIATED(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert) ) THEN
                            !
                            lreq = .false.
                            lnew = .false.
                            !
                            IF ( bloecke(i)%Key(j)%Par(k)%L_FilReq ) THEN
                               !
                               ! ... Dateien muessen vorhanden sein
                               !
                               lreq = .true.
                               !
                            ELSE
                               !
                               ! ... Dateien duerfen nicht vorhanden sein
                               !
                               lnew = .true.
                               !
                            END IF
                            !
                            DO l = 1, SIZE(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert)
                               !
                               IF ( any_error( ) ) EXIT
                               !
                               IF ( ASSOCIATED(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert(l)%char) ) THEN
                                  !
                                  DO m = 1, SIZE(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert(l)%char)
                                     !
                                     IF ( any_error( ) ) EXIT
                                     !
                                     IF ( bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert(l)%char(m)(1:4) /= 'NONE' ) THEN
                                        !
                                        CALL new_file ( test_dat )
                                        CALL set_file_path_and_name ( test_dat, &
                                             TRIM(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert(l)%char(m)) )
                                        !
                                        IF ( lreq  .AND.  file_exists_not(test_dat) ) THEN
                                           !
                                           ! Fehler -2610 : Benoetigte Datei fehlt !
                                           !
                                           CALL setup_error_act ( all_errors(:), -2610, c_upname, c_modname )
                                           CALL setup_error_act ( '<path>', TRIM(get_file_path(test_dat)) )
                                           CALL setup_error_act ( '<file>', TRIM(get_file_name(test_dat)) )
                                           !
                                        ELSE IF ( lnew  .AND.  file_exists(test_dat) ) THEN
                                           !
                                           ! Fehler -2620 : Neu anzulegende Datei existiert bereits !
                                           !
                                           CALL setup_error_act ( all_errors(:), -2620, c_upname, c_modname )
                                           CALL setup_error_act ( '<path>', TRIM(get_file_path(test_dat)) )
                                           CALL setup_error_act ( '<file>', TRIM(get_file_name(test_dat)) )
                                           !
                                        END IF
                                        !
                                        CALL kill_file ( test_dat )
                                        !
                                     END IF
                                     !
                                  END DO  ! m
                                  !
                               END IF
                               !
                            END DO   ! l
                            !
                         END IF   ! ASSO...(bloecke(i)%Key(j)%Par(k)%EGB(i_egb)%Wert)
                         !
                      END IF   ! ...%L_FilReq .OR. ...%L_FilNew
                      !
                   END IF   ! ...%Type .EQ. 'FILE'
                   !
                END DO   ! k = 1, SIZE(bloecke(i)%Key(j)%Par)
                !
             END DO   ! j = 1, SIZE(bloecke(i)%Key)
             !
          END DO   ! i_egb = 1, bloecke(i)%EGBAnz
          !
       END IF   ! ( bloecke(i)%EGBAnz .GT. 0 )
       !
    END DO   ! i = 1, SIZE(bloecke)
    !
  END SUBROUTINE File_Checks_d
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
  !! UP checkt ob eine ReqIf oder NotIf -Bedingung erfuellt ist.<BR>
  !! Subroutine erzeugt keine Fehlermeldung
  SUBROUTINE ReqIfNotIf_Erfuellt &
       ( beding, block_nr, bloecke, l_erfuellt, egb_nr, zeil_nr )
    !
    ! Formalparameter
    !! enthaelt die zu pruefende ReqIf|NotIf-Anforderung
    TYPE (t_check)                   , INTENT(IN   )     :: beding
    !! Nummer des Block-Types  in der die zu pruefende
    !! ReqIf|NotIf-Anforderung zu finden ist
    INTEGER                          , INTENT(IN   )     :: block_nr
    !! Feld mit Daten der einzelnen Eingabebloecke
    TYPE (t_block) , DIMENSION(:)    , POINTER           :: bloecke
    !! True , wenn Bedingung erfuellt<BR>
    !! False, wenn Bedingung nicht erfuellt
    LOGICAL                          , INTENT(  OUT)     :: l_erfuellt    
    !! Nummer des aktuellen Eingabeblockes dieses Block-Types
    INTEGER        , OPTIONAL        , INTENT(IN   )     :: egb_nr
    !! Zeilennummer bei Parameter-Anwesenheitschecks 
    INTEGER        , OPTIONAL        , INTENT(IN   )     :: zeil_nr
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER :: c_upname='ReqIfNotIf_Erfuellt'

    INTEGER                   :: i_bl, i_key, i_par, i_egb, z_nr
    !
    !! Hilfsvariable A fuer den Vergleich von Zeitangaben
    TYPE (t_datetime)          :: Date_A
    !! Hilfsvariable B fuer den Vergleich von Zeitangaben
    TYPE (t_datetime)          :: Date_B
    !! Hilfsvariable A fuer den Vergleich von Zeitinkrementen
    TYPE (t_time)              :: Incr_A
    !! Hilfsvariable B fuer den Vergleich von Zeitinkrementen
    TYPE (t_time)              :: Incr_B
    !
    ! Initialisierung der optionalen Parameter
    !
    ! ... EGB_NR
    !
    IF ( PRESENT(egb_nr) ) THEN
       i_egb = egb_nr
    ELSE
       i_egb = 1
    END IF
    !
    ! ... ZEIL_NR
    !
    IF ( PRESENT(zeil_nr) ) THEN
       z_nr = zeil_nr
    ELSE
       z_nr = 1
    END IF
    !
    ! weitere Initialisierungen
    !
    l_erfuellt = .False.
    !
    i_bl   = 0
    i_key  = 0
    i_par  = 0
    !
    ! Pruefen ob Anwesenheitsbedingung erfuellt ist 
    !
    SELECT CASE (beding%objekt%typ)
    CASE('BLOCK')

       i_bl  = beding%objekt%bl_nr

       SELECT CASE (beding%oper)
       CASE('EXIST')
          
          IF ( bloecke(i_bl)%EGBAnz .GT. 0 ) l_erfuellt = .True.

       CASE('NOTEXIST')
          
          IF ( bloecke(i_bl)%EGBAnz .EQ. 0 ) l_erfuellt = .True.

       END SELECT
       
    CASE('KEY')

       i_bl  = beding%objekt%bl_nr
       i_key = beding%objekt%key_nr

       ! Wenn Vergleichsobjekt im selben Block liegt wie der Check, so wird eingabeblockweise gecheckt, da
       ! es sich um einen Eingabeblock handeln kann der mehrfach vorkommen darf (d.h. i_egb = egb_nr s.o. ) !
       ! Aber, wenn : 

       IF ( i_bl .NE. block_nr ) THEN

          ! Vergleichsobjekt in einem anderen Block liegt, so kann dieser kein Mehrfachblock sein
          ! [ das wird bereits an frueherer Stelle (mod_dic_checks) gewaehrleistet ] => i_egb = 1 !

          i_egb = 1

       END IF

       IF ( bloecke(i_bl)%EGBAnz .GT. 0 ) THEN

          SELECT CASE (beding%oper)
          CASE('EXIST')
          
             IF ( bloecke(i_bl)%Key(i_key)%ZeilAnz(i_egb) .GT. 0 ) l_erfuellt = .True.

          CASE('NOTEXIST')
          
             IF ( bloecke(i_bl)%Key(i_key)%ZeilAnz(i_egb) .EQ. 0 ) l_erfuellt = .True.

          END SELECT

       ELSE

          SELECT CASE (beding%oper)
          CASE('NOTEXIST')
          
             l_erfuellt = .True.

          END SELECT


       END IF
       
    CASE('PAR')

       i_bl  = beding%objekt%bl_nr
       i_key = beding%objekt%key_nr
       i_par = beding%objekt%par_nr

       ! Wenn Vergleichsobjekt im selben Block liegt wie der Check, so wird eingabeblockweise gecheckt, da
       ! es sich um einen Eingabeblock handeln kann der mehrfach vorkommen darf (d.h. i_egb = egb_nr s.o. ) !
       ! Aber, wenn : 

       IF ( i_bl .NE. block_nr ) THEN

       ! Liegt Vergleichsobjekt in einem anderen Block, dieser kann dann kein Mehrfachblock sein
       ! [ das wird bereits an frueherer Stelle (mod_dic_checks) gewaehrleistet ] => i_egb = 1 !

          i_egb = 1

       END IF

       IF ( bloecke(i_bl)%EGBAnz .GT. 0 ) THEN

          IF ( ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert)) THEN


!            ------------------------------------------------------------------------
             SELECT CASE (bloecke(i_bl)%Key(i_key)%Par(i_par)%Type)
             CASE('INT')

                SELECT CASE (beding%oper)
                CASE('EXIST')
                   
                   IF (ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%int)) &
                        l_erfuellt = .True.
             
                CASE('NOTEXIST')
          
                   IF (.NOT. ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%int)) &
                        l_erfuellt = .True.

                CASE('EQ')
          
                   IF ( ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%int) ) THEN

                      IF ( (bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%int(1))  .EQ. &
                           beding%wert%int(1) )    l_erfuellt = .True.
                   END IF

                CASE('NE')
          
                   IF ( ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%int) ) THEN

                      IF ( (bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%int(1))  .NE. &
                           beding%wert%int(1) )    l_erfuellt = .True.
                   END IF

                END SELECT


!            ------------------------------------------------------------------------
             CASE('REAL')

                SELECT CASE (beding%oper)
                CASE('EXIST')
          
                   IF (ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%real)) &
                        l_erfuellt = .True.
                   
                CASE('NOTEXIST')
          
                   IF (.NOT. ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%real)) &
                        l_erfuellt = .True.

                CASE('EQ')
          
                   IF ( ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%real) ) THEN

                      IF ( (bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%real(1))  .EQ. &
                           beding%wert%real(1) )    l_erfuellt = .True.
                   END IF

                CASE('NE')
          
                   IF ( ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%real) ) THEN

                      IF ( (bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%real(1))  .NE. &
                           beding%wert%real(1) )    l_erfuellt = .True.
                   END IF

                END SELECT


!            ------------------------------------------------------------------------
             CASE('LOG')

                SELECT CASE (beding%oper)
                CASE('EXIST')
          
                   IF (ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%log)) &
                        l_erfuellt = .True.
             
                CASE('NOTEXIST')
          
                   IF (.NOT. ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%log)) &
                        l_erfuellt = .True.

                CASE('EQ')
          
                   IF ( ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%log) ) THEN

                      IF ( bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%log(1) &
                           .AND.  beding%wert%log(1) )    l_erfuellt = .True.

                      IF ( .NOT. bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%log(1) &
                           .AND.  .NOT. beding%wert%log(1) )    l_erfuellt = .True.

                   END IF

                CASE('NE')
          
                   IF ( ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%log) ) THEN

                      IF ( bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%log(1) &
                           .AND.  .NOT. beding%wert%log(1) )    l_erfuellt = .True.
                      
                      IF ( .NOT. bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%log(1) &
                           .AND.  beding%wert%log(1) )    l_erfuellt = .True.

                   END IF

                END SELECT


!            ------------------------------------------------------------------------
             CASE('CHAR','FILE')

                SELECT CASE (beding%oper)
                CASE('EXIST')
          
                   IF (ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%char)) &
                        l_erfuellt = .True.
             
                CASE('NOTEXIST')

                   IF (.NOT. ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%char)) &
                        l_erfuellt = .True.

                CASE('EQ')
          
                   IF ( ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%char) ) THEN

                      IF ( (bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%char(1))  .EQ. &
                           beding%wert%char(1) )    l_erfuellt = .True.
                   END IF
   
                CASE('NE')
          
                   IF ( ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%char) ) THEN

                      IF ( (bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%char(1))  .NE. &
                           beding%wert%char(1) )    l_erfuellt = .True.
                   END IF

                END SELECT

       
!            ------------------------------------------------------------------------
             CASE('DATE','INCR')

                SELECT CASE (beding%oper)
                CASE('EXIST')
          
                   IF (ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%char)) &
                        l_erfuellt = .True.
             
                CASE('NOTEXIST')

                   IF (.NOT. ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%char)) &
                        l_erfuellt = .True.

                CASE('EQ')
          
                   IF ( ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%char) ) THEN

                      SELECT CASE (bloecke(i_bl)%Key(i_key)%Par(i_par)%Type)
                      CASE('DATE')      
                         !
                         Date_A = string_to_datetime ( &
                                    bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%char(1) )
                         !
                         Date_B = string_to_datetime ( beding%wert%char(1) )
                         !
                         IF ( Date_A == Date_B ) l_erfuellt = .True.
                         !
                      CASE('INCR')
                         !
                         Incr_A = string_to_time ( &
                                    bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%char(1) )
                         !
                         Incr_B = string_to_time ( beding%wert%char(1) )
                         !
                         IF ( Incr_A == Incr_B ) l_erfuellt = .True.
                         !
                      END SELECT

                   END IF

                CASE('NE')
          
                   IF ( ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%char) ) THEN

                      SELECT CASE (bloecke(i_bl)%Key(i_key)%Par(i_par)%Type)
                      CASE('DATE')      
                         !
                         Date_A = string_to_datetime ( &
                                    bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%char(1) )
                         !
                         Date_B = string_to_datetime ( beding%wert%char(1) )
                         !
                         IF ( Date_A /= Date_B ) l_erfuellt = .True.
                         !
                      CASE('INCR')
                         !
                         Incr_A = string_to_time ( &
                                    bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%char(1) )
                         !
                         Incr_B = string_to_time ( beding%wert%char(1) )
                         !
                         IF ( Incr_A /= Incr_B ) l_erfuellt = .True.
                         !
                      END SELECT

                   END IF

                END SELECT
       

!            ------------------------------------------------------------------------
             CASE('DOUBLE')
                
                SELECT CASE (beding%oper)
                CASE('EXIST')
          
                   IF (ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%doub)) &
                        l_erfuellt = .True.
             
                CASE('NOTEXIST')
          
                   IF (.NOT. ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%doub)) &
                        l_erfuellt = .True.

                CASE('EQ')
          
                   IF ( ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%doub) ) THEN

                      IF ( (bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%doub(1))  .EQ. &
                           beding%wert%doub(1) )    l_erfuellt = .True.
                   END IF
   
                CASE('NE')
          
                   IF ( ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%doub) ) THEN

                      IF ( (bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(z_nr)%doub(1))  .NE. &
                           beding%wert%doub(1) )    l_erfuellt = .True.
                   END IF

                END SELECT


             END SELECT   ! (bloecke(i_bl)%Key(i_key)%Par(i_par)%Type)

!            --------------------------------------------------------------------


          ELSE

             SELECT CASE (beding%oper)
             CASE('NOTEXIST','NE')
          
                l_erfuellt = .True.

             END SELECT

          END IF   ! (ASSOCIATED(bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert))
          
       ELSE

          SELECT CASE (beding%oper)
          CASE('NOTEXIST','NE')
          
             l_erfuellt = .True.

          END SELECT

       END IF   ! ( bloecke(i_bl)%EGBAnz .GT. 0 )
          
    END SELECT   ! (beding%objekt%typ)
    !
  END SUBROUTINE ReqIfNotIf_Erfuellt
  !
  !! UP checkt ob eine CheckIfPar-Bedingung erfuellt ist.<BR>
  SUBROUTINE CheckIfPar_Erfuellt &
       ( block_nr, egb_nr, par, CheckIf, bloecke, l_erfuellt )
    !
    ! USE-Statements :
    !
    ! BASIS-Modul mit globalen Konstantwerten
    !
    USE b_constants, ONLY : &
         ! Parameter
         double
    !
    ! Package-Modul
    !
    USE m_stdat_types, ONLY : &
         ! Typdefinitionen
         t_par
    !
    ! Formalparameter
    !! Nummer des Block-Types
    INTEGER                          , INTENT(IN   )     :: block_nr
    !! Nummer des Eingabeblockes
    INTEGER                          , INTENT(IN   )     :: egb_nr
    !! Parameter dessen CheckIfPar-Anforderung geprueft wird
    TYPE (t_par)                     , INTENT(IN   )     :: par
    !! zu pruefende CheckIfPar-Bedingung
    TYPE (t_check)                   , INTENT(IN   )     :: CheckIf
    !! Feld mit Daten der einzelnen Eingabebloecke
    TYPE (t_block) , DIMENSION(:)    , POINTER           :: bloecke
    !! True , wenn Bedingung erfuellt
    !! False, wenn Bedingung nicht erfuellt
    LOGICAL                          , INTENT(  OUT)     :: l_erfuellt    
    !
    ! Lokale Parameter / Variablen
    !! Name der Subroutine
    CHARACTER (LEN=31), PARAMETER          :: c_upname='CheckIfPar_Erfuellt'
    !
    INTEGER                                :: i, j
    INTEGER                                :: i_bl, i_key, i_par, i_z
    !! i_egb : egb-Nummer des Parameterwertes des Vergleichsobjektes
    INTEGER                                :: i_egb

    INTEGER                                :: int_par , int_vgl
    REAL                                   :: r_par , r_vgl
    REAL (KIND=Double)                     :: db_par, db_vgl
    LOGICAL                                :: l_par , l_vgl
    !
    !! Hilfsvariable 1 fuer den Vergleich von Zeitangaben
    TYPE (t_datetime)             :: date_par
    !! Hilfsvariable 2 fuer den Vergleich von Zeitangaben
    TYPE (t_datetime)             :: date_vgl
    !! Hilfsvariable 1 fuer den Vergleich von Zeitinkrementen
    TYPE (t_time)                 :: incr_par
    !! Hilfsvariable 2 fuer den Vergleich von Zeitinkrementen
    TYPE (t_time)                 :: incr_vgl
    !
    !! l_dw  : True , wenn auf Direktwert gecheckt werden soll
    !!         False, wenn auf Vergleichsparameter gecheckt werden soll
    LOGICAL                                :: l_dw
    !! l_zw  : True , wenn Vergleichsparameter mit dem gecheckt wird in derselben
    !!                Eingabezeile liegt
    LOGICAL                                :: l_zw
    !
    !
    IF ( .NOT. ASSOCIATED(par%EGB(egb_nr)%Wert) ) THEN
       !
       ! Fehler -2600 : Das Pointerfeld <par%EGB(i_egb)%Wert> .not. associated
       !
       CALL setup_error_act ( all_errors(:), -2600, c_upname, c_modname )
       !
       RETURN
       !
    END IF
    !
    ! ------------------------------
    !
    l_erfuellt = .True.
    !
    IF ( LEN_TRIM(CheckIf%objekt%typ) .GT. 0 ) THEN
       !
       !  Fall : Vergleich mit anderem Parameterwert
       !
       l_dw = .False.
       ! ... Adressnummern des Vergleichsobjektes
       !
       i_bl  = CheckIf%objekt%bl_nr
       i_key = CheckIf%objekt%key_nr
       i_par = CheckIf%objekt%par_nr
       !
       !
       ! Wenn Vergleichsparameter im selben Block liegt wie der Check, so wird
       ! eingabeblockweise gecheckt, da es sich um einen Eingabeblock handeln
       ! kann der mehrfach vorkommen darf  => i_egb = egb_nr !
       ! Aber, wenn : 
       ! Vergleichsparameter in einem anderen Block liegt, so kann dieser kein
       ! Mehrfachblock sein !
       ! Das wird bereits an frueherer Stelle (mod_dic_checks) gewaehrleistet !
       !  => i_egb = 1 !
       !
       IF ( i_bl .EQ. block_nr ) THEN
          
          i_egb = egb_nr
          
       ELSE
          
          i_egb = 1

       END IF
       !
       ! ---------------------------------------------------------------------
       !
       IF ( .NOT. bloecke(i_bl)%Key(i_key)%L_Single ) THEN
          !
          ! Fall : Verleichsparameter liegt in einer KeyZeile, welche
          !        mehrfach vorhanden sein darf
          !        => zwangslaeufig : er liegt in derselben Eingabezeile
          !           wie die zu checkende Parametergroesse
          !        Check muss zeilenweise ( l_zw = True ) durchgefuehrt werden !!
          !
          l_zw = .True.
          !
       ELSE
          !
          l_zw = .False.
          !
       END IF
       !
    ELSE 
       !
       ! Fall : Vergleich mit Direktwert
       !
       l_dw = .True.
       !
    END IF
    !
    ! ---------------------------------------------------------------------------
    !
    SELECT CASE (par%Type)      ! kein case fuer FILE,CHAR,LOG
    CASE('INT')
       !        
       DO i = 1, SIZE(par%EGB(egb_nr)%Wert)
          !
          IF ( .NOT. l_dw ) THEN
             !
             IF ( l_zw ) THEN
                i_z = i
             ELSE
                i_z = 1
             END IF
             !
          END IF
          !
          IF ( .NOT. l_erfuellt ) EXIT
          !
          IF ( ASSOCIATED(par%EGB(egb_nr)%Wert(i)%int) ) THEN
             !
             DO j = 1, SIZE(par%EGB(egb_nr)%Wert(i)%int)
                !  
                IF ( .NOT. l_erfuellt ) EXIT
                ! 
                int_par = par%EGB(egb_nr)%Wert(i)%int(j)
                !
                IF ( l_dw ) THEN
                   int_vgl = CheckIf%wert%int(1)
                ELSE
                   int_vgl = bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(i_z)%int(1)
                END IF
                !
                !
                SELECT CASE (CheckIf%oper)
                CASE ('LE')
                   IF ( int_par .GT. int_vgl ) l_erfuellt = .False.
                CASE ('LT')
                   IF ( int_par .GE. int_vgl ) l_erfuellt = .False.
                CASE ('GT')
                   IF ( int_par .LE. int_vgl ) l_erfuellt = .False.
                CASE ('GE')
                   IF ( int_par .LT. int_vgl ) l_erfuellt = .False.
                CASE ('EQ')
                   IF ( int_par .NE. int_vgl ) l_erfuellt = .False.
                CASE ('NE')
                   IF ( int_par .EQ. int_vgl ) l_erfuellt = .False.
                END SELECT
                !
                !
             END DO   ! j = 1, SIZE(par%EGB(egb_nr)%Wert(i)%int)
             !    
          END IF   ! ASSOCIATED(par%EGB(egb_nr)%Wert(i)%int)
          !
       END DO   ! i = 1, SIZE(par%EGB(egb_nr)%Wert)
       !
       ! ---------------------------------------------------------------------------
       !
    CASE('REAL')
       !
       DO i = 1, SIZE(par%EGB(egb_nr)%Wert)
          !
          IF ( .NOT. l_dw ) THEN
             !
             IF ( l_zw ) THEN
                i_z = i
             ELSE
                i_z = 1
             END IF
             !
          END IF
          !
          IF ( .NOT. l_erfuellt ) EXIT
          !
          IF ( ASSOCIATED(par%EGB(egb_nr)%Wert(i)%real) ) THEN
             !
             DO j = 1, SIZE(par%EGB(egb_nr)%Wert(i)%real)
                !
                IF ( .NOT. l_erfuellt ) EXIT
                !
                r_par = par%EGB(egb_nr)%Wert(i)%real(j)
                !
                IF ( l_dw ) THEN
                   r_vgl = CheckIf%wert%real(1)
                ELSE
                   r_vgl = bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(i_z)%real(1)
                END IF
                !
                !
                SELECT CASE (CheckIf%oper)
                CASE ('LE')
                   IF ( r_par .GT. r_vgl ) l_erfuellt = .False.
                CASE ('LT')
                   IF ( r_par .GE. r_vgl ) l_erfuellt = .False.
                CASE ('GT')
                   IF ( r_par .LE. r_vgl ) l_erfuellt = .False.
                CASE ('GE')
                   IF ( r_par .LT. r_vgl ) l_erfuellt = .False.
                CASE ('EQ')
                   IF ( r_par .NE. r_vgl ) l_erfuellt = .False.
                CASE ('NE')
                   IF ( r_par .EQ. r_vgl ) l_erfuellt = .False.
                END SELECT
                !
                !
             END DO   ! j = 1, SIZE(par%EGB(egb_nr)%Wert(i)%real)
             !       
          END IF   ! ASSOCIATED(par%EGB(egb_nr)%Wert(i)%real)
          !
       END DO   ! i = 1, SIZE(par%EGB(egb_nr)%Wert)
       !
       ! ---------------------------------------------------------------------------
       ! 
    CASE('DATE','INCR')
       ! 
       DO i = 1, SIZE(par%EGB(egb_nr)%Wert)
          !
          IF ( .NOT. l_dw ) THEN
             !
             IF ( l_zw ) THEN
                i_z = i
             ELSE
                i_z = 1
             END IF
             !
          END IF
          !
          IF ( .NOT. l_erfuellt ) EXIT
          !
          IF ( ASSOCIATED(par%EGB(egb_nr)%Wert(i)%char) ) THEN
             !
             DO j = 1, SIZE(par%EGB(egb_nr)%Wert(i)%char)
                !
                IF ( .NOT. l_erfuellt ) EXIT
                !
                SELECT CASE (par%Type)
                CASE('DATE')      
                   !
                   date_par = string_to_datetime ( par%EGB(egb_nr)%Wert(i)%char(j) )
                   !
                   IF ( l_dw ) THEN
                      date_vgl = string_to_datetime ( CheckIf%wert%char(1) )
                   ELSE
                      date_vgl = string_to_datetime ( &
                           bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(i_z)%char(1) )
                   END IF
                   !
                   !
                   SELECT CASE (CheckIf%oper)
                   CASE ('LE')
                      IF ( date_par > date_vgl ) l_erfuellt = .False.
                   CASE ('LT')
                      IF ( date_par >= date_vgl ) l_erfuellt = .False.
                   CASE ('GT')
                      IF ( date_par <= date_vgl ) l_erfuellt = .False.
                   CASE ('GE')
                      IF ( date_par < date_vgl ) l_erfuellt = .False.
                   CASE ('EQ')
                      IF ( date_par /= date_vgl ) l_erfuellt = .False.
                   CASE ('NE')
                      IF ( date_par == date_vgl ) l_erfuellt = .False.
                   END SELECT
                   !
                CASE('INCR')
                   !    
                   incr_par = string_to_time ( par%EGB(egb_nr)%Wert(i)%char(j) )
                   !
                   IF ( l_dw ) THEN
                      incr_vgl = string_to_time ( CheckIf%wert%char(1) )
                   ELSE
                      incr_vgl = string_to_time ( &
                           bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(i_z)%char(1) )
                   END IF
                   !
                   !
                   SELECT CASE (CheckIf%oper)
                   CASE ('LE')
                      IF ( incr_par > incr_vgl ) l_erfuellt = .False.
                   CASE ('LT')
                      IF ( incr_par >= incr_vgl ) l_erfuellt = .False.
                   CASE ('GT')
                      IF ( incr_par <= incr_vgl ) l_erfuellt = .False.
                   CASE ('GE')
                      IF ( incr_par < incr_vgl ) l_erfuellt = .False.
                   CASE ('EQ')
                      IF ( incr_par /= incr_vgl ) l_erfuellt = .False.
                   CASE ('NE')
                      IF ( incr_par == incr_vgl ) l_erfuellt = .False.
                   END SELECT
                   !
                END SELECT
                !
             END DO   ! j = 1, SIZE(par%EGB(egb_nr)%Wert(i)%char)
             !     
          END IF   ! ASSOCIATED(par%EGB(egb_nr)%Wert(i)%char)
          !
       END DO   ! i = 1, SIZE(par%EGB(egb_nr)%Wert)
       !
       ! ---------------------------------------------------------------------------
       !
    CASE('DOUBLE')
       !
       DO i = 1, SIZE(par%EGB(egb_nr)%Wert)
          !
          IF ( .NOT. l_dw ) THEN
             !
             IF ( l_zw ) THEN
                i_z = i
             ELSE
                i_z = 1
             END IF
             !
          END IF
          !
          IF ( .NOT. l_erfuellt ) EXIT
          !
          IF ( ASSOCIATED(par%EGB(egb_nr)%Wert(i)%doub) ) THEN
             !
             DO j = 1, SIZE(par%EGB(egb_nr)%Wert(i)%doub)
                !
                IF ( .NOT. l_erfuellt ) EXIT
                !
                db_par = par%EGB(egb_nr)%Wert(i)%doub(j)
                !
                IF ( l_dw ) THEN
                   db_vgl = CheckIf%wert%doub(1)
                ELSE
                   db_vgl = bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(i_z)%doub(1)
                END IF
                !
                !
                SELECT CASE (CheckIf%oper)
                CASE ('LE')
                   IF ( db_par .GT. db_vgl ) l_erfuellt = .False.
                CASE ('LT')
                   IF ( db_par .GE. db_vgl ) l_erfuellt = .False.
                CASE ('GT')
                   IF ( db_par .LE. db_vgl ) l_erfuellt = .False.
                CASE ('GE')
                   IF ( db_par .LT. db_vgl ) l_erfuellt = .False.
                CASE ('EQ')
                   IF ( db_par .NE. db_vgl ) l_erfuellt = .False.
                CASE ('NE')
                   IF ( db_par .EQ. db_vgl ) l_erfuellt = .False.
                END SELECT
                !
                !
             END DO   ! j = 1, SIZE(par%EGB(egb_nr)%Wert(i)%doub)
             !        
          END IF   ! ASSOCIATED(par%EGB(egb_nr)%Wert(i)%doub)
          !
       END DO   ! i = 1, SIZE(par%EGB(egb_nr)%Wert)
       !
       ! ---------------------------------------------------------------------------
       !
    CASE('LOG')
       !
       DO i = 1, SIZE(par%EGB(egb_nr)%Wert)
          !
          IF ( .NOT. l_dw ) THEN
             !
             IF ( l_zw ) THEN
                i_z = i
             ELSE
                i_z = 1
             END IF
             !
          END IF
          !
          IF ( .NOT. l_erfuellt ) EXIT
          !
          IF ( ASSOCIATED(par%EGB(egb_nr)%Wert(i)%log) ) THEN
             !
             DO j = 1, SIZE(par%EGB(egb_nr)%Wert(i)%log)
                !
                IF ( .NOT. l_erfuellt ) EXIT
                !
                l_par = par%EGB(egb_nr)%Wert(i)%log(j)
                !
                IF ( l_dw ) THEN
                   l_vgl = CheckIf%wert%log(1)
                ELSE
                   l_vgl = bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(i_z)%log(1)
                END IF
                !
                !
                SELECT CASE (CheckIf%oper)
                CASE ('EQ')
                   IF ( l_par  .AND.  .NOT. l_vgl ) l_erfuellt = .False.
                   IF ( .NOT. l_par  .AND.  l_vgl ) l_erfuellt = .False.
                CASE ('NE')
                   IF ( l_par  .AND.  l_vgl ) l_erfuellt = .False.
                   IF ( .NOT. l_par  .AND.  .NOT. l_vgl ) l_erfuellt = .False.
                END SELECT
                !
                !
             END DO   ! j = 1, SIZE(par%EGB(egb_nr)%Wert(i)%log)
             !        
          END IF   ! ASSOCIATED(par%EGB(egb_nr)%Wert(i)%log)
          !
       END DO   ! i = 1, SIZE(par%EGB(egb_nr)%Wert)
       !
       ! ---------------------------------------------------------------------------
       !
    CASE('CHAR','FILE')
       !
       DO i = 1, SIZE(par%EGB(egb_nr)%Wert)
          !
          IF ( .NOT. l_dw ) THEN
             !
             IF ( l_zw ) THEN
                i_z = i
             ELSE
                i_z = 1
             END IF
             !
          END IF
          !
          IF ( .NOT. l_erfuellt ) EXIT
          !
          IF ( ASSOCIATED(par%EGB(egb_nr)%Wert(i)%char) ) THEN
             !
             DO j = 1, SIZE(par%EGB(egb_nr)%Wert(i)%char)
                !
                IF ( .NOT. l_erfuellt ) EXIT
                !
                IF ( l_dw ) THEN
                   !
                   !
                   SELECT CASE (CheckIf%oper)
                   CASE ('EQ')
                      !
                      IF ( par%EGB(egb_nr)%Wert(i)%char(j) .NE. &
                           CheckIf%wert%char(1) ) l_erfuellt = .False.
                      !
                   CASE ('NE')
                      !
                      IF ( par%EGB(egb_nr)%Wert(i)%char(j) .EQ. &
                           CheckIf%wert%char(1) ) l_erfuellt = .False.
                      !
                   END SELECT
                   !
                   !
                ELSE   ! IF .NOT. l_dw
                   !
                   !
                   SELECT CASE (CheckIf%oper)
                   CASE ('EQ')
                      !
                      IF ( par%EGB(egb_nr)%Wert(i)%char(j) .NE. &
                           bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(i_z)%char(1) ) &
                           l_erfuellt = .False.
                      !
                   CASE ('NE')
                      !
                      IF ( par%EGB(egb_nr)%Wert(i)%char(j) .EQ. &
                           bloecke(i_bl)%Key(i_key)%Par(i_par)%EGB(i_egb)%Wert(i_z)%char(1) ) &
                           l_erfuellt = .False.
                      !
                   END SELECT
                   !
                END IF
                !
             END DO   ! j = 1, SIZE(par%EGB(egb_nr)%Wert(i)%char)
             !           
          END IF   ! ASSOCIATED(par%EGB(egb_nr)%Wert(i)%char)
          !
       END DO   ! i = 1, SIZE(par%EGB(egb_nr)%Wert)
       !
    END SELECT
    !
    !
  END SUBROUTINE CheckIfPar_Erfuellt
  !
END MODULE m_stdat_checks
!  ----------------------------------------------------------------------------
!  Ende des Source-Codes des Moduls
!  ----------------------------------------------------------------------------

