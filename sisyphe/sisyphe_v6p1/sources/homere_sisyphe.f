!                    **********************
                     PROGRAM HOMERE_SISYPHE
!                    **********************
!
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    1) ACQUIRES THE DATA REQUIRED TO DEFINE THE POINTERS:
!+                   STEERING FILE + GEOMETRY FILE (PARTIALLY ONLY).
!+
!+            2) CALLS THE SUBROUTINE SISYPHE.
!
!history  C LE NORMANT (LNH)
!+        09/04/2009
!+        V6P0
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
      USE INTERFACE_SISYPHE
!
      IMPLICIT NONE
      INTEGER     LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER TDEB,TFIN,IFLOT,NCAR,DUMINT
      LOGICAL DUMLOG
!
      CHARACTER(LEN=24), PARAMETER :: CODE='SISYPHE                 '
!
      CHARACTER(LEN=250) PATH
      CHARACTER(LEN=144) MOTCAR(300),FILE_DESC(4,300)
!
      INTEGER  TIME_IN_SECONDS
      EXTERNAL TIME_IN_SECONDS
!
!======================================================================
!
      CALL BIEF_INIT(CODE,PATH,NCAR,.TRUE.)
!
      TDEB = TIME_IN_SECONDS()
!
!  HEADING TO THE LISTING
!
      IF(LNG.EQ.1) WRITE(LU,100)
      IF(LNG.EQ.2) WRITE(LU,101)
      WRITE(LU,102)
100   FORMAT(/////,1X,'LISTING DE SISYPHE ',78('-'))
101   FORMAT(/////,1X,'LISTING OF SISYPHE ',78('-'))
102   FORMAT(/////,
     &14X,'    SSSS I   SSSS Y   Y PPPP  H   H EEEEE',/,
     &14X,'   S     I  S      Y Y  P   P H   H E    ',/,
     &14X,'    SSS  I   SSS    Y   PPPP  HHHHH EEEE  ',/,
     &14X,'       S I      S   Y   P     H   H E     ',/,
     &14X,'   SSSS  I  SSSS    Y   P     H   H EEEEE',/,
     &14X,'                                          ',/,
     &14X,'                 VERSION 6.0              ',/,
     &14X,/////)
!
!-----------------------------------------------------------------------
!
! READS THE STEERING FILE
!
      CALL LECDON_SISYPHE(MOTCAR,FILE_DESC,PATH,NCAR,CODE)
!
!-----------------------------------------------------------------------
!
      IFLOT = 0
      CALL BIEF_OPEN_FILES(CODE,SIS_FILES,MAXLU_SIS,
     &                     PATH,NCAR,.FALSE.,IFLOT,2)
!
!-----------------------------------------------------------------------
!
! ALLOCATES VECTORS, MATRICES AND BLOCKS
!
      CALL POINT_SISYPHE
!
!-----------------------------------------------------------------------
!
!  CALLS THE SUBROUTINE SISYPHE
!  -1 GOES THROUGH THE WHOLE SUBROUTINE BECAUSE THERE IS NO COUPLING
!  THE OTHER VARIABLES ARE ONLY USED WHEN COUPLING
!
!     INOUT VARIABLES IN SISYPHE CANNOT BE HARD-CODED
      DUMINT=1
      DUMLOG=.FALSE.
!
      CALL SISYPHE(-1,0,0,0,0,T1,T1,T1,T1,T1,T1,T1,T1,
     &             DUMLOG,DUMINT,DUMLOG,CODE,1,
     &             T1,T1,0.D0,T1,0.D0,DUMLOG,DUMLOG,
     &             T1,1,T1,T1,T1,T1)
!
!-----------------------------------------------------------------------
!
      CALL BIEF_CLOSE_FILES(CODE,SIS_FILES,MAXLU_SIS,.TRUE.)
!
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.1) WRITE(LU,10)
      IF(LNG.EQ.2) WRITE(LU,11)
10    FORMAT(1X,///,1X,'FIN NORMALE DU PROGRAMME',///)
11    FORMAT(1X,///,1X,'CORRECT END OF RUN',///)
!
!-----------------------------------------------------------------------
!
      TFIN = TIME_IN_SECONDS()
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'DUREE DU CALCUL : ',TFIN-TDEB,' SECONDES'
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'COMPUTER TIME: ',TFIN-TDEB,' SECONDS'
      ENDIF
!
!-----------------------------------------------------------------------
!
      STOP
      END PROGRAM HOMERE_SISYPHE
