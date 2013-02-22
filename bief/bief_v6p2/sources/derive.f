!                    *****************
                     SUBROUTINE DERIVE
!                    *****************
!
     &(U,V,W,DT,AT,X,Y,Z,IKLE,IFABOR,LT,IELM,IELMU,NDP,NPOIN,NPOIN2,
     & NELEM,NELMAX,SURDET,XFLOT,YFLOT,ZFLOT,
     & SHPFLO,SHZFLO,TAGFLO,ELTFLO,ETAFLO,
     & NFLOT,NFLOT_MAX,FLOPRD,MESH,UL,
     & ISUB,DX,DY,DZ,ELTBUF,SHPBUF,SHZBUF,SIZEBUF)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    - COMPUTES THE BARYCENTRIC COORDINATES OF A FLOAT
!+                  IN THE MESH AT THE TIME OF RELEASE.
!+
!+            - COMPUTES THE SUCCESSIVE POSITIONS OF THIS FLOAT
!+                  WHICH IS CARRIED WITHOUT FRICTION BY THE CURRENT
!+                 (SUBSEQUENT TIMESTEPS).
!
!history  J-M JANIN (LNH)
!+        18/08/94
!+        V5P1
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
!history  J-M HERVOUET (LNHE)
!+        19/06/2012
!+        V6P2
!+   Adapted for calling SCARACT instead of CHAR11. However parallelism
!+   will require further modifications.
!
!history  J-M HERVOUET (LNHE)
!+        22/02/2013
!+        V6P3
!+   New file format for Tecplot. Works in parallel. Prepared for 3D.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME STEP (I.E. TIME INTERVAL).
!| DX             |<->| WORK ARRAY (DISPLACEMENTS ALONG X)
!| DY             |<->| WORK ARRAY (DISPLACEMENTS ALONG Y)
!| DZ             |<->| WORK ARRAY (DISPLACEMENTS ALONG Z)
!| ELTBUF         |<->| WORK ARRAY
!| ELTFLO         |<->| NUMBERS OF ELEMENTS WHERE ARE THE FLOATS
!| ETAFLO         |<->| LEVELS WHERE ARE THE FLOATS
!| FLOPRD         |-->| NUMBER OF TIME STEPS BETWEEB TWO RECORDS
!|                |   | FOR FLOATS POSITIONS.
!| IELM           |-->| TYPE OF ELEMENT.
!| IELMU          |-->| TYPE OF ELEMENT FOR VELOCITIES.
!| IFABOR         |-->| ELEMENTS BEHIND THE EDGES OF ANOTHER ELEMENT
!|                |   | IF IFABOR NEGATIVE OR 0, THE EDGE IS A
!|                |   | LIQUID OR PERIODIC BOUNDARY
!| IKLE           |-->| CONNECTIVITY TABLE.
!| ISUB           |<->| ARRIVAL SUB-DOMAIN OF PARTICLES.
!| LT             |-->| TIME STEP NUMBER.
!| MESH           |<->| MESH STRUCTURE.
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN 2D
!| NFLOT          |<->| NUMBER OF FLOATS.
!| NFLOT_MAX      |<->| MAXIMUM NUMBER OF FLOATS.
!| NPOIN          |-->| NUMBER OF POINTS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| SHPBUF         |<->| WORK ARRAY 
!| SHPFLO         |<->| BARYCENTRIC COORDINATES OF FLOATS IN THEIR 
!|                |   | ELEMENTS.
!| SHZBUF         |<->| WORK ARRAY 
!| SHZFLO         |<->| BARYCENTRIC COORDINATE ON VERTICAL
!| SIZEBUF        |-->| DILMENSION OF SOME WORK ARRAYS
!| SURDET         |-->| 1/DETERMINANT, USED IN ISOPARAMETRIC
!|                |   | TRANSFORMATION.
!| TAGFLO         |-->| TAGS OF FLOATS  
!| U              |-->| X-COMPONENT OF VELOCITY
!| UL             |-->| LOGICAL UNIT OF OUTPUT FILE
!| V              |-->| Y-COMPONENT OF VELOCITY
!| W              |-->| Z-COMPONENT OF VELOCITY
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| Z              |-->| ELEVATIONS OF POINTS IN THE MESH
!| XFLOT          |<->| ABSCISSAE OF FLOATS
!| YFLOT          |<->| ORDINATES OF FLOATS
!| ZFLOT          |<->| ELEVATIONS OF FLOATS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_DERIVE => DERIVE
      USE STREAMLINE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NPOIN,LT,IELM,IELMU,NDP,NELEM
      INTEGER         , INTENT(IN)    :: FLOPRD,NELMAX,UL,SIZEBUF,NPOIN2
      INTEGER         , INTENT(IN)    :: NFLOT_MAX
      INTEGER         , INTENT(INOUT) :: NFLOT
      DOUBLE PRECISION, INTENT(IN)    :: DT,AT
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN),V(NPOIN),W(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN),Z(NPOIN)
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,NDP)
      INTEGER         , INTENT(IN)    :: IFABOR(NELMAX,NDP)
      DOUBLE PRECISION, INTENT(IN)    :: SURDET(NELEM)
      DOUBLE PRECISION, INTENT(INOUT) :: XFLOT(NFLOT_MAX),DX(NFLOT_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: YFLOT(NFLOT_MAX),DY(NFLOT_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: ZFLOT(NFLOT_MAX),DZ(NFLOT_MAX)
      INTEGER         , INTENT(INOUT) :: TAGFLO(NFLOT_MAX)
      INTEGER         , INTENT(INOUT) :: ELTFLO(NFLOT_MAX)
      INTEGER         , INTENT(INOUT) :: ETAFLO(NFLOT_MAX)
      INTEGER         , INTENT(INOUT) :: ELTBUF(SIZEBUF)
      INTEGER         , INTENT(INOUT) :: ISUB(NFLOT_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: SHPFLO(NDP,NFLOT_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: SHZFLO(NFLOT_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: SHPBUF(NDP,SIZEBUF)
      DOUBLE PRECISION, INTENT(INOUT) :: SHZBUF(SIZEBUF)
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IFLOT,FRE(1),FREBUF(1),IPROC,NFLOTG,NPLAN,ELT
      INTEGER N1,N2,N3,N4,N5,N6,NOMB,SENS
!
      DOUBLE PRECISION ZSTAR(1)
!
      CHARACTER(LEN=32) TEXTE(2)
      CHARACTER(LEN=72) LIGNE
!
      LOGICAL YESITIS
!
      TYPE(BIEF_OBJ) :: SVOID
!
      INTEGER  P_ISUM
      EXTERNAL P_ISUM
!
      CHARACTER(LEN=11) EXTENS
      EXTERNAL          EXTENS
!
      LOGICAL DEJA
      DATA    DEJA/.FALSE./
!
      SAVE
!
!-----------------------------------------------------------------------
!
!     PARAMETERISING THE CALL TO SCARACT
!
!     NUMBER OF PLANES
      NPLAN=1
!     NO VARIABLE TO INTERPOLATE AT THE FOOT OF CHARACTERISTICS
      NOMB=0
!     FORWARD TRACKING
      SENS=1
!
      IF(IELM.NE.11) THEN
        IF(LNG.EQ.1) WRITE(LU,123) IELM
        IF(LNG.EQ.2) WRITE(LU,124) IELM
123     FORMAT(1X,'DERIVE : TYPE D''ELEMENT NON PREVU : ',1I6)
124     FORMAT(1X,'DERIVE : UNEXPECTED TYPE OF ELEMENT: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     INITIALISING SVOID AND HEADER OF A TECPLOT FILE
!
      IF(.NOT.DEJA) THEN
!
!       THOUGH NOMB = 0, THESE COMPONENTS WILL BE USED IN SCARACT
!
        SVOID%TYPE=2
        SVOID%DIM1=1
        ALLOCATE(SVOID%R(1))
!
!       HEADER OF TECPLOT FILE
!
        IF(IPID.EQ.0) THEN
          TEXTE(1)='X                               '
          TEXTE(2)='Y                               '
          IF(LNG.EQ.1) THEN
            WRITE(UL,100) 'TITLE = "FICHIER DES FLOTTEURS"'
          ELSE
            WRITE(UL,100) 'TITLE = "DROGUES FILE"'
          ENDIF
          WRITE(UL,100) 'VARIABLES = "LABELS","'//
     &                   TEXTE(1)//'","'//TEXTE(2)//'","COLOUR"'
        ENDIF
        DEJA=.TRUE.
100     FORMAT(A)
      ENDIF
!
      SVOID%ELM=IELM
!
!-----------------------------------------------------------------------
!
!     TRAJECTORIES COMPUTED FOR ALL POINTS
!
      CALL SCARACT(SVOID,SVOID,U,V,W,W,X,Y,ZSTAR,ZSTAR,
     &             XFLOT,YFLOT,ZFLOT,ZFLOT,
     &             DX,DY,DZ,DZ,Z,SHPFLO,SHZFLO,SHZFLO,SURDET,DT,
     &             IKLE,IFABOR,ELTFLO,ETAFLO,
     &             FRE,ELTBUF,ISUB,IELM,IELMU,NELEM,NELMAX,            
     &             NOMB,NPOIN,NPOIN2,NDP,NPLAN,1,
     &             MESH,NFLOT,BIEF_NBPTS(IELMU,MESH),SENS,        
     &             SHPBUF,SHZBUF,SHZBUF,FREBUF,SIZEBUF,
     &              .TRUE.,.FALSE.,.FALSE.)
!    &               POST,  PERIO,   YA4D)
!                    POST=.TRUE. OTHERWISE ISUB IS NOT FILLED
!
!-----------------------------------------------------------------------
!
      IF(NCSIZE.GT.1.AND.NFLOT.GT.0) THEN
!
!       IN // XFLOT AND YFLOT MAY HAVE BEEN DESTROYED BY SCARACT
!       BECAUSE RE-USED FOR GENERATIONS OF LOST PARTICLES
!       THEY ARE REDONE HERE FOR PARTICLES WHICH ARE STILL IN THE
!       GLOBAL DOMAIN (TEST ISUB(IFLOT)=IPID NOT DONE, BUT COULD BE...)
!
        IF(IELM.EQ.11) THEN
          DO IFLOT=1,NFLOT
            ELT=ELTFLO(IFLOT)
            IF(ELT.GT.0) THEN
              N1=IKLE(ELT,1)
              N2=IKLE(ELT,2)
              N3=IKLE(ELT,3)
              XFLOT(IFLOT)=SHPFLO(1,IFLOT)*X(N1)
     &                    +SHPFLO(2,IFLOT)*X(N2)
     &                    +SHPFLO(3,IFLOT)*X(N3)     
              YFLOT(IFLOT)=SHPFLO(1,IFLOT)*Y(N1)
     &                    +SHPFLO(2,IFLOT)*Y(N2)
     &                    +SHPFLO(3,IFLOT)*Y(N3)
            ENDIF   
          ENDDO 
        ELSE
          DO IFLOT=1,NFLOT
            ELT=ELTFLO(IFLOT)
            IF(ELT.GT.0) THEN       
              N1=IKLE(ELT,1)+NPOIN2*(ETAFLO(IFLOT)-1) 
              N2=IKLE(ELT,2)+NPOIN2*(ETAFLO(IFLOT)-1)
              N3=IKLE(ELT,3)+NPOIN2*(ETAFLO(IFLOT)-1)
              N4=IKLE(ELT,1)+NPOIN2* ETAFLO(IFLOT) 
              N5=IKLE(ELT,2)+NPOIN2* ETAFLO(IFLOT)
              N6=IKLE(ELT,3)+NPOIN2* ETAFLO(IFLOT)
              XFLOT(NFLOT)=SHPFLO(1,IFLOT)*X(N1)
     &                    +SHPFLO(2,IFLOT)*X(N2)
     &                    +SHPFLO(3,IFLOT)*X(N3)     
              YFLOT(NFLOT)=SHPFLO(1,IFLOT)*Y(N1)
     &                    +SHPFLO(2,IFLOT)*Y(N2)
     &                    +SHPFLO(3,IFLOT)*Y(N3)
              ZFLOT(NFLOT)=(Z(N1)*SHPFLO(1,IFLOT) 
     &                     +Z(N2)*SHPFLO(2,IFLOT)
     &                     +Z(N3)*SHPFLO(3,IFLOT))*(1.D0-SHZFLO(IFLOT))
     &                    +(Z(N4)*SHPFLO(1,IFLOT) 
     &                     +Z(N5)*SHPFLO(2,IFLOT) 
     &                     +Z(N6)*SHPFLO(3,IFLOT))*SHZFLO(IFLOT)
            ENDIF   
          ENDDO 
        ENDIF        
!
      ENDIF
!
!     SENDING THE PARTICLES THAT MIGRATED TO ANOTHER SUB-DOMAIN
!
      IF(NCSIZE.GT.1) THEN
        CALL SEND_PARTICLES(XFLOT,YFLOT,ZFLOT,SHPFLO,SHZFLO,ELTFLO,
     &                      ETAFLO,ISUB,TAGFLO,NDP,NFLOT,NFLOT_MAX,
     &                      MESH,NPLAN)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     CASE OF LOST FLOATS (EXITED OR NOW REMOVED AFTER BEING SENT TO
!                          ANOTHER SUB-DOMAIN) 
!
      IFLOT=1
      IF(NCSIZE.GT.1) THEN
!
!       IN // MODE
!
11      CONTINUE
!       LOST OR MIGRATED FLOATS
        IF(NFLOT.GT.0.AND.NCSIZE.GT.1) THEN
          IF(ELTFLO(IFLOT).LE.0.OR.ISUB(IFLOT).NE.IPID) THEN 
            CALL DEL_PARTICLE(TAGFLO(IFLOT),NFLOT,NFLOT_MAX,
     &                        XFLOT,YFLOT,ZFLOT,TAGFLO,SHPFLO,SHZFLO,
     &                        ELTFLO,ETAFLO,MESH%TYPELM,ISUB)
!           THE SAME IFLOT IS NOW A NEW PARTICLE AND MUST BE CHECKED AGAIN!
            IF(IFLOT.LE.NFLOT) GO TO 11
          ENDIF
          IFLOT=IFLOT+1
          IF(IFLOT.LE.NFLOT) GO TO 11
        ENDIF
!
      ELSE
!
!       IN SCALAR MODE
!
10      CONTINUE
!       LOST FLOATS ONLY
        IF(NFLOT.GT.0) THEN
          IF(ELTFLO(IFLOT).LE.0) THEN 
            CALL DEL_PARTICLE(TAGFLO(IFLOT),NFLOT,NFLOT_MAX,
     &                        XFLOT,YFLOT,ZFLOT,TAGFLO,SHPFLO,SHZFLO,
     &                        ELTFLO,ETAFLO,MESH%TYPELM)
!           THE SAME IFLOT IS NOW A NEW PARTICLE AND MUST BE CHECKED AGAIN!
            IF(IFLOT.LE.NFLOT) GO TO 10
          ENDIF
          IFLOT=IFLOT+1
          IF(IFLOT.LE.NFLOT) GO TO 10
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     TECPLOT FILE
!
      IF(NCSIZE.GT.1) THEN
!
!       WAITING ALL PROCESSORS (SO THAT NFLOT IS UPDATED FOR ALL
!                               BEFORE CALLING P_ISUM)
! 
        CALL P_SYNC
!
!       PARALLEL VERSION
!
        NFLOTG=P_ISUM(NFLOT)
        IF(NFLOTG.GT.0.AND.(LT.EQ.1.OR.(LT/FLOPRD)*FLOPRD.EQ.LT)) THEN
!
!         1) EVERY PROCESSOR WRITES ITS OWN DATA IN A FILE WITH EXTENSION
!
          IF(NFLOT.GT.0) THEN
            OPEN(99,FILE=EXTENS(NCSIZE,IPID+1),
     &           FORM='FORMATTED',STATUS='NEW')
            DO IFLOT=1,NFLOT
              WRITE(99,300) TAGFLO(IFLOT),XFLOT(IFLOT),YFLOT(IFLOT),1
            ENDDO
            CLOSE(99)
          ENDIF
!
!         2) WAITING ALL PROCESSORS
!
          CALL P_SYNC
!
!         3) PROCESSOR 0 READS ALL EXISTING FILES AND MERGES 
!            THEM IN THE FINAL FILE
!
          IF(IPID.EQ.0) THEN      
            WRITE(UL,200) 'ZONE DATAPACKING=POINT, T="G_',AT,
     &      ' seconds"',', I=',NFLOTG,', SOLUTIONTIME=',AT
            DO IPROC=1,NCSIZE
              INQUIRE(FILE=EXTENS(NCSIZE,IPROC),EXIST=YESITIS)
              IF(YESITIS) THEN
                OPEN(99,FILE=EXTENS(NCSIZE,IPROC),
     &               FORM='FORMATTED',STATUS='OLD')
20              CONTINUE
                READ(99,100,ERR=21,END=21) LIGNE
                WRITE(UL,*) LIGNE
                GO TO 20
21              CONTINUE
                CLOSE(99,STATUS='DELETE')
              ENDIF
            ENDDO
          ENDIF
!
        ENDIF
!
      ELSE
!
!       SCALAR VERSION
!
        IF(NFLOT.GT.0.AND.(LT.EQ.1.OR.(LT/FLOPRD)*FLOPRD.EQ.LT)) THEN
            WRITE(UL,200) 'ZONE DATAPACKING=POINT, T="G_',AT,
     &                    ' seconds"',', I=',NFLOT,', SOLUTIONTIME=',AT
            DO IFLOT=1,NFLOT
              WRITE(UL,300) TAGFLO(IFLOT),XFLOT(IFLOT),YFLOT(IFLOT),1
            ENDDO
200       FORMAT(A,F12.4,A,A,I4,A,F12.4)
300       FORMAT(I6,',',F16.8,',',F16.8,',',I2)
        ENDIF
!
      ENDIF      
!
!-----------------------------------------------------------------------
!
      RETURN
      END
