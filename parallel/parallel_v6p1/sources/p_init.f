!                    *****************
                     SUBROUTINE P_INIT
!                    *****************
!
     &(CHAINE,NCAR,IPID,NCSIZE)
!
!***********************************************************************
! PARALLEL   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES.
!+                REGISTERS PROGRAM WITH PARASTATION.
!
!history  HANS HERRMANN (HANOVER)
!+        **/06/1996
!+
!+
!
!history  REINHARD HINKELMANN (HANOVER)
!+        08/06/1996
!+
!+   MODIFIED
!
!history  J-M HERVOUET (LNH)
!+        17/12/1996
!+
!+   MODIFIED
!
!history  RAINER JOHANNI (SGI MUNICH)
!+        **/10/1999
!+
!+   ADAPTED FOR MPI
!
!history  J.A. JANKOWSKI (BAW KARLSRUHE)
!+        28/12/1999
!+
!+   RELEASE 5.0 MODIFIED
!
!history  P. VEZOLLE (IBM)
!+        16/05/2008
!+        V5P9
!+   MODIFIED (SIZE OF EXTENSION)
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
!| CHAINE         |---| WORKING DIRECTORY
!| IPID           |---| PROCESSUS ID
!| NCAR           |---| SIZE OF THE CHARACTER STRING
!| NCSIZE         |---| NUMBER OF MPI PROCESSUS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON /INFO/ LNG,LU
!
      INCLUDE 'mpif.h'
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(OUT)            :: NCAR,IPID,NCSIZE
      CHARACTER(LEN=250), INTENT(OUT) :: CHAINE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER MYTID,IER,I,NPREAD
!
      CHARACTER*255 EXE
!
      LOGICAL YAPARA
      CHARACTER*11 PNUMBER
      CHARACTER*13 MYNAM
!
!-----------------------------------------------------------------------
! MPI IS SILENT WHEN EVERYTHING IS GOING ON PROPERLY
! DEFAULT LANGUAGE 2 AND STANDARD OUTPUT 6
! IN ORDER TO SEE ERROR MESSAGES (TO THE MASTER!)
! THE SUBROUTINE CALLED NEXT IS READ_CONFIG !
! THIS IS NOT PRETTY...
!
      LNG=2
      LU=6
!
!     ALL WRITE STATEMENTS BEFORE OPENING A FILE ON LU=6 SEEM TO RAISE
!     A PROBLEM ON WINDOWS COMPAQ COMPILER
!
!     FROM SOGREAH (BUT DENYNONE IS NOT STANDARD)
!     OPEN(UNIT=LU,FILE="PARALLEL.LOG",FORM='FORMATTED',ACTION='WRITE',
!    *     SHARE='DENYNONE')
!
!$$$      IF(LNG.EQ.1) WRITE(LU,*) 'ENTREE DANS P_INIT'
!$$$      IF(LNG.EQ.2) WRITE(LU,*) 'ENTERING P_INIT'
!$$$      IF(LNG.EQ.1) WRITE(LU,*)
!$$$     *    'LOGGING OUTPUT DIRECTED INTO FILES FOR EACH PROCESSOR'
!$$$      IF(LNG.EQ.2) WRITE(LU,*)
!$$$     *    'LOGGING OUTPUT DIRECTED INTO FILES FOR EACH PROCESSOR'
!
! INITIALISES MPI
!
      CALL MPI_INIT(IER)
      IF(IER.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'P_INIT: ERREUR DANS MPI_INIT'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_INIT: ERROR IN MPI_INIT'
        WRITE(LU,*) 'MPI ERROR ',IER
        STOP
      ENDIF
!
! OBTAINS MYTID, IT IS VIRTUALLY THE PROCESSOR NUMBER (RANK)
!
      CALL MPI_COMM_RANK(MPI_COMM_WORLD,IPID,IER)
      IF(IER.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'P_INIT: ERREUR DANS MPI_COMM_RANK'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_INIT: ERROR IN MPI_COMM_RANK'
        WRITE(LU,*) 'MPI ERROR ',IER
        STOP
      ENDIF
!
! OBTAINS NCSIZE
!
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NCSIZE,IER)
      IF(IER.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'P_INIT: ERREUR DANS MPI_COMM_SIZE'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_INIT: ERROR IN MPI_COMM_SIZE'
        WRITE(LU,*) 'MPI ERROR ',IER
        STOP
      ENDIF
!
!$$$      IF(LNG.EQ.1) WRITE(LU,*)
!$$$     &    'MPI: CALCUL AVEC ',NCSIZE,' PROCESSEURS'
!$$$      IF(LNG.EQ.2) WRITE(LU,*)
!$$$     &    'MPI: COMPUTATION WITH ',NCSIZE,' PROCESSORS'
!
! MANIPULATES MASTER'S AND SLAVES' STANDART OUTPUT TO FILES PE#LOG.TXT
! SLAVES WRITE TO CHANNEL 95 (?)
! WORKS FOR PE# 0-999 (WE HAVE A DECENT NUMBER OF PROCESSORS!)
!
      IF(NCSIZE.GT.1) THEN
        PNUMBER = '00000-00000'
!
        IF((NCSIZE-1).LT.10) THEN
          WRITE(PNUMBER(05:05),'(I1)') NCSIZE-1
        ELSEIF((NCSIZE-1).LT.100) THEN
          WRITE(PNUMBER(04:05),'(I2)') NCSIZE-1
        ELSEIF((NCSIZE-1).LT.1000) THEN
          WRITE(PNUMBER(03:05),'(I3)') NCSIZE-1
        ELSEIF((NCSIZE-1).LT.10000) THEN
          WRITE(PNUMBER(02:05),'(I4)') NCSIZE-1
        ELSE
          WRITE(PNUMBER(01:05),'(I5)') NCSIZE-1
        ENDIF
!
        IF(IPID.LT.10) THEN
          WRITE(PNUMBER(11:11),'(I1)') IPID
        ELSEIF(IPID.LT.100) THEN
          WRITE(PNUMBER(10:11),'(I2)') IPID
        ELSEIF(IPID.LT.1000) THEN
          WRITE(PNUMBER(9:11),'(I3)') IPID
        ELSEIF(IPID.LT.10000) THEN
          WRITE(PNUMBER(8:11),'(I4)') IPID
        ELSE
          WRITE(PNUMBER(7:11),'(I5)') IPID
        ENDIF
        WRITE(MYNAM,'("PE", A11)') PNUMBER
!
      ENDIF
!
      IF(IPID.EQ.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'MAITRE PROCESSEUR NUMERO ',
     &                  IPID,' OF THE GROUP OF ',NCSIZE
        IF(LNG.EQ.2) WRITE(LU,*) 'MASTER PROCESSOR NUMBER ',
     &                  IPID,' OF THE GROUP OF ',NCSIZE
      ELSE
        OPEN(UNIT=LU,FILE=MYNAM//'.LOG', FORM='FORMATTED',
     &         STATUS='UNKNOWN')
        IF(LNG.EQ.1) WRITE(LU,*) 'ESCLAVE PROCESSEUR NUMERO ',
     &                           IPID,' IN THE GROUP OF ',NCSIZE
        IF(LNG.EQ.2) WRITE(LU,*) 'SLAVE  PROCESSOR NUMBER ',
     &                           IPID,' IN THE GROUP OF ',NCSIZE
      ENDIF
!
! READS THE NUMBER OF PROCESSORS AND NAME OF THE EXECUTABLE
!
      NCAR=0
      NPREAD=1
      YAPARA=.FALSE.
      INQUIRE(FILE='./PARAL',EXIST=YAPARA)
      IF(YAPARA) THEN
        OPEN(40,FILE='PARAL',FORM='FORMATTED',ACTION='READ')
        READ(40,*) NPREAD
        IF(NPREAD.NE.NCSIZE) THEN
          WRITE (LU,*)
     &      'P_INIT: FILE PARAL IS INCONSISTENT WITH MPI PARAMETERS'
          WRITE (LU,*) 'MPI NCSIZE   = ',NCSIZE
          WRITE (LU,*) 'PARAL NCSIZE = ',NPREAD
        ENDIF
!C        IF(LNG.EQ.1) WRITE(LU,*)'CALCUL AVEC ',NPREAD,' PROCESSEURS'
!C        IF(LNG.EQ.2) WRITE(LU,*)'COMPUTATION WITH ',NPREAD,' PROCESSORS'
        READ(40,*) NCAR
        READ(40,100) CHAINE
100     FORMAT(A250)
        EXE(1:NCAR+5)=CHAINE(1:NCAR) // 'A.EXE'
        IF(LNG.EQ.1) WRITE(LU,*) 'FICHIER EXECUTABLE : ',EXE(1:NCAR+5)
        IF(LNG.EQ.2) WRITE(LU,*) 'EXECUTABLE FILE: ',EXE(1:NCAR+5)
        CLOSE(40)
      ELSEIF(IPID.EQ.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'P_INIT: FICHIER PARAL NON TROUVE'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_INIT: FILE PARAL NOT FOUND'
        STOP
      ENDIF
!
! THE BARRIER USUALLY COMES UNEXPECTED
!
      CALL MPI_BARRIER(MPI_COMM_WORLD,IER)
      IF (IER.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'P_INIT: ERREUR DANS MPI_BARRIER'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_INIT: ERROR MPI_BARRIER'
        WRITE(LU,*) 'MPI ERROR ',IER
        STOP
      ELSE
        IF(LNG.EQ.1) WRITE(LU,*) 'BARRIERE PASSEE'
        IF(LNG.EQ.2) WRITE(LU,*) 'BARRIER PASSED'
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
