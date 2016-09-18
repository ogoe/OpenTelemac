!                    **************************
                     SUBROUTINE BIEF_OPEN_FILES
!                    **************************
!
     &(CODE,FILES,NFILES,PATH,NCAR,ICODE,FULLNAME)
!
!***********************************************************************
! BIEF   V7P1
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
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        27/11/2013
!+        V6P3
!+   Opening by all processors of ACTION='WRITE' file precluded. In this
!+   case only the processor 0 will do it.
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        26/12/2013
!+        V7P0
!+   Checking the declared format when possible : case of a SERAFIN file
!+   in READ mode.
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CODE           |-->| NAME OF CALLING PROGRAMME
!| FILES          |-->| STRUCTURES OF CODE FILES
!| ICODE          |---| NUMERO DU CODE EN CAS DE COUPLAGE
!| NCAR           |-->| NUMBER OF CHARACTERS IN THE PATH
!| NFILES         |-->| NUMBER OF FILES
!| PATH           |-->| FULL NAME OF THE PATH WHERE THE CASE IS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_BIEF_OPEN_FILES => BIEF_OPEN_FILES
      USE DECLARATIONS_TELEMAC
      USE INTERFACE_HERMES
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER           , INTENT(IN)    :: NFILES
      CHARACTER(LEN=24) , INTENT(IN)    :: CODE
      TYPE(BIEF_FILE)   , INTENT(INOUT) :: FILES(NFILES)
      CHARACTER(LEN=250), INTENT(IN)    :: PATH
      INTEGER           , INTENT(IN)    :: NCAR,ICODE
      LOGICAL           , INTENT(IN)    :: FULLNAME
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I, II, ICLI, IERR
!
      CHARACTER(LEN=11) :: FORME,EXTENS
      CHARACTER(LEN=300) :: FILE_NAME
      CHARACTER(LEN=300) :: CLI_NAME
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
!         GET LOGICAL UNIT
          CALL GET_FREE_ID(FILES(I)%LU)
!
          IF(FILES(I)%BINASC.EQ.'ASC') THEN
            FORME='FORMATTED  '
          ELSE
            FORME='UNFORMATTED'
          ENDIF
!
!         OPENS THE FILE
          ! If fullname name is true we use the real name of the file not the temporary foldare name
          IF(FULLNAME) THEN
            FILE_NAME = FILES(I)%NAME
          ELSE
            FILE_NAME = PATH(1:NCAR)//TRIM(FILES(I)%TELNAME)
          ENDIF
!
          ! Boundary file will be opened by the hermes module
          IF(FILES(I)%TYPE.EQ.'CONLIM') CYCLE
!
          IF((FILES(I)%FMT(1:7).EQ.'SERAFIN')
     &       .OR.(FILES(I)%FMT.EQ.'MED     ')) THEN
            ! Get the boundary confitions file
            ICLI = 0
            IF(FILES(I)%ACTION.EQ.'READ     ') THEN
              DO II=1,NFILES
                IF(FILES(II)%NAME(1:1).NE.' ') THEN
                  IF(FILES(II)%TYPE(1:6).EQ.'CONLIM') THEN
                    ICLI = II
                    EXIT
                  ENDIF
                ENDIF
              ENDDO
              IF(ICLI.EQ.0) THEN
                IF(LNG.EQ.1) THEN
                  WRITE(LU,*) 'PAS DE FICHIER DE CONDITIONS LIMITES'
                ENDIF
                IF(LNG.EQ.1) THEN
                  WRITE(LU,*) 'NO BOUNDARY CONDITIONS FILE'
                ENDIF
              ENDIF
            ENDIF
            ! Rename file_name and cli_name
            IF(FULLNAME) THEN
              IF((NCSIZE.LE.1).OR.(FILES(I)%TYPE(1:4).EQ.'SCAL')) THEN
                FILE_NAME = PATH(1:NCAR)//TRIM(FILES(I)%NAME)
                IF(ICLI.NE.0) THEN
                  CLI_NAME = PATH(1:NCAR)//TRIM(FILES(ICLI)%NAME)
                ENDIF
              ELSE
                FILE_NAME = PATH(1:NCAR)//TRIM(FILES(I)%NAME)
     &                      //EXTENS(NCSIZE-1,IPID)
                IF(ICLI.NE.0) THEN
                  CLI_NAME = PATH(1:NCAR)//TRIM(FILES(ICLI)%NAME)
     &                        //EXTENS(NCSIZE-1,IPID)
                ENDIF
              ENDIF
            ELSE
              IF((NCSIZE.LE.1).OR.(FILES(I)%TYPE(1:4).EQ.'SCAL')) THEN
                FILE_NAME = PATH(1:NCAR)//TRIM(FILES(I)%TELNAME)
                IF(ICLI.NE.0) THEN
                  CLI_NAME = PATH(1:NCAR)//TRIM(FILES(ICLI)%TELNAME)
                ENDIF
              ELSE
                FILE_NAME = PATH(1:NCAR)//TRIM(FILES(I)%TELNAME)
     &                      //EXTENS(NCSIZE-1,IPID)
                IF(ICLI.NE.0) THEN
                  CLI_NAME = PATH(1:NCAR)//TRIM(FILES(ICLI)%TELNAME)
     &                        //EXTENS(NCSIZE-1,IPID)
                ENDIF
              ENDIF
            ENDIF

            IF(FILES(I)%TYPE(1:4).EQ.'SCAL') THEN
              ! If the file is a scalar type on the processor zero is opning it
              ! And it a read or readwrite file
              IF(IPID.EQ.0.OR.FILES(I)%ACTION(5:9).NE.'WRITE') THEN
                CALL OPEN_MESH(FILES(I)%FMT, TRIM(FILE_NAME),
     &                         FILES(I)%LU, FILES(I)%ACTION, IERR)
                CALL CHECK_CALL(IERR,'BIEF_OPEN_FILES:OPEN_MESH')
                IF(ICLI.NE.0) THEN
                  CALL OPEN_BND(FILES(I)%FMT, TRIM(CLI_NAME),
     &                          FILES(I)%LU, FILES(ICLI)%ACTION, IERR)
                  CALL CHECK_CALL(IERR,'BIEF_OPEN_FILES:OPEN_BND')
                ENDIF
              ENDIF
            ELSE
              CALL OPEN_MESH(FILES(I)%FMT, TRIM(FILE_NAME), FILES(I)%LU,
     &                       FILES(I)%ACTION, IERR)
              CALL CHECK_CALL(IERR,'BIEF_OPEN_FILES:OPEN_MESH')
              IF(ICLI.NE.0) THEN
                CALL OPEN_BND(FILES(I)%FMT, TRIM(CLI_NAME),
     &                        FILES(I)%LU, FILES(ICLI)%ACTION, IERR)
                CALL CHECK_CALL(IERR,'BIEF_OPEN_FILES:OPEN_BND')
              ENDIF
            ENDIF
            IF(IERR.NE.0) THEN
              IF(LNG.EQ.1) THEN
                WRITE(LU,*) 'ERREUR LORS DE L OUVERTURE DE : ',
     &                      TRIM(FILE_NAME)
                WRITE(LU,*) 'ET DE : ',
     &                      TRIM(CLI_NAME)
                WRITE(LU,*) 'AU FORMAT ',FILES(I)%FMT
                WRITE(LU,*) 'EN MODE ',FILES(I)%ACTION
                WRITE(LU,*) 'ERREUR ',IERR
              ENDIF
              IF(LNG.EQ.2) THEN
                WRITE(LU,*) 'ERROR WHILE OPENING: ', TRIM(FILE_NAME)
                WRITE(LU,*) 'AND: ', TRIM(CLI_NAME)
                WRITE(LU,*) 'IN ',FILES(I)%FMT,' FORMAT '
                WRITE(LU,*) 'IN ',FILES(I)%ACTION,' MODE'
                WRITE(LU,*) 'ERROR ',IERR
              ENDIF
              CALL PLANTE(1)
              STOP
            ENDIF
!
          ELSE
!
            IF(NCSIZE.LE.1) THEN
!             SCALAR
              OPEN(FILES(I)%LU,FILE=FILE_NAME,
     &             FORM=FORME,ACTION=FILES(I)%ACTION)
            ELSE
!             PARALLEL, FILE TYPE: SCAL
!             ALL PROCESSORS CANNOT OPEN THE SAME FILE FOR WRITING
!             IN THIS CASE, ONLY PROCESSOR 0 MAY OPEN AND WRITE
              IF(FILES(I)%TYPE(1:4).EQ.'SCAL') THEN
                IF(IPID.EQ.0.OR.FILES(I)%ACTION(1:5).NE.'WRITE') THEN
                  OPEN(FILES(I)%LU,
     &                 FILE=FILE_NAME,
     &                 FORM=FORME,ACTION=FILES(I)%ACTION)
                ENDIF
!             PARALLEL, OTHER FILE TYPE
              ELSE
                OPEN(FILES(I)%LU,
     &               FILE=TRIM(FILE_NAME)
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

