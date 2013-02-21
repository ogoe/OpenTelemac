!                    *****************
                     SUBROUTINE DERIVE
!                    *****************
!
     &(U,V,DT,AT,X,Y,IKLE,IFABOR,LT,IELM,IELMU,NDP,NPOIN,
     & NELEM,NELMAX,SURDET,XFLOT,YFLOT,
     & SHPFLO,DEBFLO,FINFLO,TAGFLO,ELTFLO,NFLOT,FLOPRD,MESH,UL,
     & ISUB,DX,DY,ELTBUF,SHPBUF,SIZEBUF)
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
!+        12/02/2013
!+        V6P3
!+   New file format for Tecplot.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DEBFLO         |-->| TIME STEP FOR THE RELEASE OF FLOATS
!| DT             |-->| TIME STEP (I.E. TIME INTERVAL).
!| DX             |<->| WORK ARRAY (DISPLACEMENTS ALONG X)
!| DY             |<->| WORK ARRAY (DISPLACEMENTS ALONG Y)
!| ELTBUF         |<->| WORK ARRAY
!| ELTFLO         |<->| NUMBERS OF ELEMENTS WHERE ARE THE FLOATS
!| FINFLO         |<->| TIME STEP FOR ENDING THE TREATMENT OF A FLOAT
!|                |   | CAN BE CHANGED IF A FLOAT EXITS BY A FREE EXIT
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
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN 2D
!| NFLOT          |-->| NOMBER OF FLOATS.
!| NPOIN          |-->| NUMBER OF POINTS
!| SHPBUF         |<->| WORK ARRAY 
!| SHPFLO         |<->| BARYCENTRIC COORDINATES OF FLOATS IN THEIR 
!|                |   | ELEMENTS.
!| SIZEBUF        |-->| DILMENSION OF SOME WORK ARRAYS
!| SURDET         |-->| 1/DETERMINANT, USED IN ISOPARAMETRIC
!|                |   | TRANSFORMATION.
!| TAGFLO         |-->| TAGS OF FLOATS  
!| U              |-->| X-COMPONENT OF VELOCITY
!| UL             |-->| LOGICAL UNIT OF OUTPUT FILE
!| V              |-->| Y-COMPONENT OF VELOCITY
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| XFLOT          |<->| ABSCISSAE OF FLOATS
!| YFLOT          |<->| ORDINATES OF FLOATS
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
      INTEGER         , INTENT(IN)    :: FLOPRD,NELMAX,NFLOT,UL,SIZEBUF
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN),V(NPOIN),DT,AT
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,NDP)
      INTEGER         , INTENT(IN)    :: IFABOR(NELMAX,NDP)
      DOUBLE PRECISION, INTENT(IN)    :: SURDET(NELEM)
      DOUBLE PRECISION, INTENT(INOUT) :: XFLOT(NFLOT),DX(NFLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: YFLOT(NFLOT),DY(NFLOT)
      INTEGER         , INTENT(INOUT) :: DEBFLO(NFLOT),FINFLO(NFLOT)
      INTEGER         , INTENT(IN)    :: TAGFLO(NFLOT)
      INTEGER         , INTENT(INOUT) :: ELTFLO(NFLOT),ELTBUF(SIZEBUF)
      INTEGER         , INTENT(INOUT) :: ISUB(NFLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: SHPFLO(NDP,NFLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: SHPBUF(NDP,SIZEBUF)
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IFLOT,N1,N2,N3,IELEM,ETA(1),SENS,FRE(1)
      INTEGER NPLAN,NPT,FREBUF(1)
!
      DOUBLE PRECISION DET1,DET2,DET3,ZSTAR(1),ZCONV(1)
      DOUBLE PRECISION SHZBUF(1),SHZ(1),Z(1),SHF(1)
!
      CHARACTER(LEN=32) TEXTE(2)
!
      TYPE(BIEF_OBJ) :: SVOID
!
!-----------------------------------------------------------------------
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
!     2D HERE
!
      NPLAN=1
!
!     SCARACT WILL TREAT ALL POINTS TOGETHER
!
      NPT=NFLOT
!
!     FORWARD CHARACTERISTICS
!
      SENS=1
!
!     HEADER OF A TECPLOT FILE
!
      IF(LT.EQ.1) THEN
        TEXTE(1)='X                               '
        TEXTE(2)='Y                               '
        IF(LNG.EQ.1) THEN
          WRITE(UL,100) 'TITLE = "FICHIER DES FLOTTEURS"'
        ELSE
          WRITE(UL,100) 'TITLE = "DROGUES FILE"'
        ENDIF
        WRITE(UL,100) 'VARIABLES = "LABELS","'//
     &                 TEXTE(1)//'","'//TEXTE(2)//'","COLOUR"'
100     FORMAT(A)
      ENDIF
!
      DO IFLOT=1,NFLOT
!
!       POINT ALREADY EXITED OR NOT YET RELEASED OR REMOVED
        IF(DEBFLO(IFLOT).LT.0            .OR.
     &                LT.LT.DEBFLO(IFLOT).OR.
     &                LT.GT.FINFLO(IFLOT)     ) THEN
!
!         THE POINT WILL NOT BE TREATED IF GIVEN
!         A STARTING ELEMENT EQUAL TO 0
!        
          ELTFLO(IFLOT) = 0       
!        
        ELSEIF(LT.EQ.DEBFLO(IFLOT)) THEN
!
!-----------------------------------------------------------------------
!
!   - COMPUTES THE BARYCENTRIC COORDINATES OF A FLOAT IN THE MESH
!     AT THE TIME OF RELEASE
!
!-----------------------------------------------------------------------
!
!         P1 TRIANGLES MESH
!
          DO IELEM=1,NELEM
            N1=IKLE(IELEM,1)
            N2=IKLE(IELEM,2)
            N3=IKLE(IELEM,3)
!
! DET1 = (N2N3,N2FLOT)  DET2 = (N3N1,N3FLOT)  DET3 = (N1N2,N1FLOT)
! ----------------------------------------------------------------
!
            DET1=(X(N3)-X(N2))*(YFLOT(IFLOT)-Y(N2))
     &          -(Y(N3)-Y(N2))*(XFLOT(IFLOT)-X(N2))
            DET2=(X(N1)-X(N3))*(YFLOT(IFLOT)-Y(N3))
     &          -(Y(N1)-Y(N3))*(XFLOT(IFLOT)-X(N3))
            DET3=(X(N2)-X(N1))*(YFLOT(IFLOT)-Y(N1))
     &          -(Y(N2)-Y(N1))*(XFLOT(IFLOT)-X(N1))
            IF(DET1.GE.0.D0.AND.DET2.GE.0.D0.AND.DET3.GE.0.D0) GOTO 30
!
          ENDDO
!
          IF(LNG.EQ.1) WRITE(LU,33) IFLOT
          IF(LNG.EQ.2) WRITE(LU,34) IFLOT
33        FORMAT(1X,'ERREUR D''INTERPOLATION DANS DERIVE :',/,
     &           1X,'LARGAGE DU FLOTTEUR',I6,/,
     &           1X,'EN DEHORS DU DOMAINE DE CALCUL')
34        FORMAT(1X,'INTERPOLATION ERROR IN DERIVE :',/,
     &           1X,'DROP POINT OF FLOAT',I6,/,
     &           1X,'OUT OF THE DOMAIN')
          CALL PLANTE(1)
          STOP
!
! ELEMENT CONTAINING THE POINT OF RELEASE, COMPUTES THE SHPFLO
! ------------------------------------------------------------
!
30        CONTINUE
          SHPFLO(1,IFLOT) = DET1*SURDET(IELEM)
          SHPFLO(2,IFLOT) = DET2*SURDET(IELEM)
          SHPFLO(3,IFLOT) = DET3*SURDET(IELEM)
          ELTFLO (IFLOT)  = IELEM
!
        ELSEIF(LT.GT.DEBFLO(IFLOT).AND.LT.LE.FINFLO(IFLOT)) THEN
!
!-----------------------------------------------------------------------
!
!   - COMPUTES THE SUCCESSIVE POSITIONS OF THIS FLOAT WHICH IS
!     CARRIED BY THE CURRENT 
!
!-----------------------------------------------------------------------
!
        ENDIF
!
      ENDDO
!
!     TRAJECTORIES COMPUTED FOR ALL POINTS
!
      CALL SCARACT(SVOID,SVOID,U,V,V,V,X,Y,
     &             ZSTAR,ZSTAR,
     &             XFLOT,YFLOT,ZCONV,ZCONV,
     &             DX,DY,DY,DY,Z,SHPFLO,
     &             SHZ,SHF,SURDET,DT,
     &             IKLE,IFABOR,ELTFLO,
     &             ETA,FRE,ELTBUF,ISUB,IELM,IELMU,NELEM,NELMAX,
     &             0,NPOIN,NPOIN,NDP,NPLAN,1,
     &             MESH,NPT,BIEF_NBPTS(IELMU,MESH),SENS,        
     &             SHPBUF,SHZBUF,SHZBUF,FREBUF,SIZEBUF,
     &             .FALSE.,.FALSE.,.FALSE.)
!
!-----------------------------------------------------------------------
!
!     CASE OF LOST FLOATS OR FLOATS OUT OF TIME RANGE
!     THEY WILL NOT BE PUT IN THE FILE
!
      DO IFLOT=1,NFLOT
!       FRESHLY LOST FLOATS, CHARACTERISTICS RETURN : - ELEMENT NUMBER
        IF(ELTFLO(IFLOT).LT.0) THEN 
!         DEBFLO STORES THE EXIT ELEMENT NUMBER
!         DEBFLO<0 WILL NOT CHANGE TESTS ON DEBFLO AT THIS LEVEL
          DEBFLO(IFLOT)=ELTFLO(IFLOT)
          ELTFLO(IFLOT)=0
        ENDIF
!       OLD LOST FLOATS: THEIR DEBFLO IS NEGATIVE BUT ELTFLO HAS BEEN
!       RESET TO 1
        IF(DEBFLO(IFLOT).LT.0) THEN 
          ELTFLO(IFLOT)=0
        ENDIF
!       FLOATS OUT OF TIME RANGE
!       SCARACT WILL RETURN ELTFLO=1 IF GIVEN ELTFLO=0
        IF(LT.LT.DEBFLO(IFLOT).OR.LT.GT.FINFLO(IFLOT)) THEN
          ELTFLO(IFLOT) = 0
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
!     TECPLOT FILE
!
      IF(LT.EQ.1.OR.(LT/FLOPRD)*FLOPRD.EQ.LT) THEN
        NPT=0
        DO IFLOT=1,NFLOT
          IF(ELTFLO(IFLOT).GT.0) NPT=NPT+1
        ENDDO
        WRITE(UL,200) 'ZONE DATAPACKING=POINT, T="G_',AT,
     *                ' seconds"',', I=',NPT,', SOLUTIONTIME=',AT
        DO IFLOT=1,NFLOT
          IF(ELTFLO(IFLOT).GT.0) THEN
            WRITE(UL,300) TAGFLO(IFLOT),XFLOT(IFLOT),YFLOT(IFLOT),1
          ENDIF
        ENDDO
200     FORMAT(A,F12.4,A,A,I4,A,F12.4)
300     FORMAT(I6,',',F16.8,',',F16.8,',',I2)
      ENDIF      
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
