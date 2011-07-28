!                    **************************
                     SUBROUTINE BIEF_OPEN_FILES
!                    **************************
!
     &(CODE,FILES,NFILES,PATH,NCAR,FLOT,IFLOT,ICODE)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    OPENS FILES DECLARED IN THE STEERING FILE.
!
!note     STEERING AND DICTIONARY FILES ARE OPENED AND CLOSED
!+         IN LECDON
!
!history  J-M HERVOUET (LNHE)
!+        12/10/2009
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
!| CODE           |-->| NAME OF CALLING PROGRAMME
!| FILES          |-->| STRUCTURES OF CODE FILES
!| FLOT           |-->| LOGICAL, IF YES LOGICAL UNITS DECIDED BY
!|                |   | THIS SUBROUTINE, IF NO, TAKEN IN SUBMIT
!| ICODE          |---| NUMERO DU CODE EN CAS DE COUPLAGE
!| IFLOT          |-->| IF FLOT=YES, START NEW LOGICAL UNIT NUMBERS
!|                |   | AT IFLOT+1
!| NCAR           |-->| NUMBER OF CHARACTERS IN THE PATH
!| NFILES         |-->| NUMBER OF FILES
!| PATH           |-->| FULL NAME OF THE PATH WHERE THE CASE IS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_BIEF_OPEN_FILES => BIEF_OPEN_FILES
      USE DECLARATIONS_TELEMAC
      USE M_MED
!
      IMPLICIT NONE
      INTEGER     LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER           , INTENT(IN)    :: NFILES
      CHARACTER(LEN=24) , INTENT(IN)    :: CODE
      TYPE(BIEF_FILE)   , INTENT(INOUT) :: FILES(NFILES)
      CHARACTER(LEN=250), INTENT(IN)    :: PATH
      INTEGER           , INTENT(IN)    :: NCAR,ICODE
      INTEGER           , INTENT(INOUT) :: IFLOT
      LOGICAL           , INTENT(IN)    :: FLOT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
      CHARACTER(LEN=11) :: FORME,EXTENS
      EXTERNAL EXTENS
!
!-----------------------------------------------------------------------
!
!     MESSAGE
!
      IF(LNG.EQ.1) WRITE(LU,*) 'OUVERTURE DES FICHIERS POUR ',CODE
      IF(LNG.EQ.2) WRITE(LU,*) 'OPENING FILES FOR ',CODE
!
!
!     DECODES THE SUBMIT STRING FOR THE FILES IN THE STEERING FILE
!
      DO I=1,NFILES
!
        IF(FILES(I)%NAME(1:1).NE.' ') THEN
!
!         LOGICAL UNIT MODIFIED WHEN COUPLING
!
          IF(FLOT) THEN
            IFLOT=IFLOT+1
!           2 AND 3 SKIPPED (DICTIONARY AND STEERING FILES)
            IF(IFLOT.EQ.2) IFLOT=4
!           5 AND 6 SKIPPED (STANDARD INPUT AND OUTPUT)
            IF(IFLOT.EQ.5) IFLOT=7
            FILES(I)%LU=IFLOT
          ENDIF
!
          IF(FILES(I)%BINASC.EQ.'ASC') THEN
            FORME='FORMATTED  '
          ELSE
            FORME='UNFORMATTED'
          ENDIF
!
!         OPENS THE FILE
!
          IF(FILES(I)%FMT.EQ.'MED     ') THEN
!
            IF(NCSIZE.LE.1) THEN
              CALL OPEN_FILE_MED(FILES(I)%TELNAME,FILES(I)%LU,
     &                           FILES(I)%ACTION)
            ELSE
!             PARALLEL MODE, FILE TYPE: SCAL
              IF(FILES(I)%TYPE(1:4).EQ.'SCAL') THEN
              CALL OPEN_FILE_MED(PATH(1:NCAR)//TRIM(FILES(I)%TELNAME)
     &               ,FILES(I)%LU,FILES(I)%ACTION)
!             PARALLEL MODE, OTHER FILE TYPE
              ELSE
              CALL OPEN_FILE_MED(PATH(1:NCAR)//TRIM(FILES(I)%TELNAME)
     &               //EXTENS(NCSIZE-1,IPID)//'.med',FILES(I)%LU,
     &               FILES(I)%ACTION)
              ENDIF
            ENDIF
!
          ELSE
!
            IF(NCSIZE.LE.1) THEN
!             SCALAR
              OPEN(FILES(I)%LU,FILE=FILES(I)%TELNAME,
     &             FORM=FORME,ACTION=FILES(I)%ACTION)
            ELSE
!             PARALLEL, FILE TYPE: SCAL
              IF(FILES(I)%TYPE(1:4).EQ.'SCAL') THEN
                OPEN(FILES(I)%LU,
     &               FILE=PATH(1:NCAR)//TRIM(FILES(I)%TELNAME),
     &               FORM=FORME,ACTION=FILES(I)%ACTION)
!             PARALLEL, OTHER FILE TYPE
              ELSE
                OPEN(FILES(I)%LU,
     &               FILE=PATH(1:NCAR)//TRIM(FILES(I)%TELNAME)
     &               //EXTENS(NCSIZE-1,IPID),
     &               FORM=FORME,ACTION=FILES(I)%ACTION)
              ENDIF
            ENDIF
!
          ENDIF
!
        ENDIF
!
      ENDDO
!
!     SETS AND STORES THE CODE NAME
!
      NAMECODE = CODE
      NNAMECODE(ICODE) = CODE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
