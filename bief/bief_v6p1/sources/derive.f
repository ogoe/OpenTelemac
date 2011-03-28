!                    *****************
                     SUBROUTINE DERIVE
!                    *****************
!
     &( U , V , DT , X , Y , IKLE , IFABOR , LT , IELM , NDP , NPOIN ,
     &  NELEM , NELMAX , SURDET , XFLOT , YFLOT ,
     &  SHPFLO , DEBFLO , FINFLO , ELTFLO , NFLOT , NITFLO,FLOPRD,T8)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DEBFLO         |-->| TIME STEP FOR THE RELEASE OF FLOATS
!| DT             |-->| TIME STEP (I.E. TIME INTERVAL).
!| ELTFLO         |<->| NUMBERS OF ELEMENTS WHERE ARE THE FLOATS
!| FINFLO         |<->| TIME STEP FOR ENDING THE TREATMENT OF A FLOAT
!|                |   | CAN BE CHANGED IF A FLOAT EXITS BY A FREE EXIT
!| FLOPRD         |-->| NUMBER OF TIME STEPS BETWEEB TWO RECORDS
!|                |   | FOR FLOATS POSITIONS.
!| IELM           |-->| TYPE OF ELEMENT.
!| IFABOR         |-->| ELEMENTS BEHIND THE EDGES OF ANOTHER ELEMENT
!|                |   | IF IFABOR NEGATIVE OR 0, THE EDGE IS A
!|                |   | LIQUID OR PERIODIC BOUNDARY
!| IKLE           |-->| CONNECTIVITY TABLE.
!| LT             |-->| TIME STEP NUMBER.
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN 2D
!| NFLOT          |-->| NOMBER OF FLOATS.
!| NITFLO         |-->| MAXIMUM NUMBER OF RECORDS FOR POSITIONS
!|                |   | OF FLOATS.
!| NPOIN          |-->| NUMBER OF POINTS
!| SHPFLO         |<->| BARYCENTRIC COORDINATES OF FLOATS IN THEIR 
!|                |   | ELEMENTS.
!| SURDET         |-->| 1/DETERMINANT, USED IN ISOPARAMETRIC
!|                |   | TRANSFORMATION.
!| T8             |<->| WORK ARRAY
!| U              |-->| X-COMPONENT OF VELOCITY
!| V              |-->| Y-COMPONENT OF VELOCITY
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| XFLOT          |<->| ABSCISSAE OF FLOATS
!| YFLOT          |<->| ORDINATES OF FLOATS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_DERIVE => DERIVE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NPOIN,LT,IELM,NDP,NELEM
      INTEGER         , INTENT(IN)    :: NITFLO,FLOPRD,NELMAX,NFLOT
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN),V(NPOIN),DT
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,NDP)
      INTEGER         , INTENT(IN)    :: IFABOR(NELMAX,NDP)
      DOUBLE PRECISION, INTENT(IN)    :: SURDET(NELEM)
      DOUBLE PRECISION, INTENT(INOUT) :: XFLOT(NITFLO,NFLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: YFLOT(NITFLO,NFLOT)
      INTEGER         , INTENT(INOUT) :: DEBFLO(NFLOT),FINFLO(NFLOT)
      INTEGER         , INTENT(INOUT) :: ELTFLO(NFLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: SHPFLO(NDP,NFLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: T8(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER LTP,IFLOT,N1,N2,N3,NRK,IELEM,LTT,NSP(1)
!
      DOUBLE PRECISION DET1,DET2,DET3,DX(1),DY(1)
!
!-----------------------------------------------------------------------
!
      LTT=(LT-1)/FLOPRD + 1
      LTP=(LT-2)/FLOPRD + 1
!
      DO 10 IFLOT=1,NFLOT
!
        IF(LT.EQ.DEBFLO(IFLOT)) THEN
!
!-----------------------------------------------------------------------
!
!   - COMPUTES THE BARYCENTRIC COORDINATES OF A FLOAT IN THE MESH
!     AT THE TIME OF RELEASE
!
!-----------------------------------------------------------------------
!
          XFLOT(LTT,IFLOT) = XFLOT(1,IFLOT)
          YFLOT(LTT,IFLOT) = YFLOT(1,IFLOT)
!
          IF(IELM.EQ.11) THEN
!
! P1 TRIANGLES MESH
! ========================
!
            DO 20 IELEM=1,NELEM
              N1=IKLE(IELEM,1)
              N2=IKLE(IELEM,2)
              N3=IKLE(IELEM,3)
!
! DET1 = (N2N3,N2FLOT)  DET2 = (N3N1,N3FLOT)  DET3 = (N1N2,N1FLOT)
! ----------------------------------------------------------------
!
              DET1=(X(N3)-X(N2))*(YFLOT(LTT,IFLOT)-Y(N2))
     &            -(Y(N3)-Y(N2))*(XFLOT(LTT,IFLOT)-X(N2))
              DET2=(X(N1)-X(N3))*(YFLOT(LTT,IFLOT)-Y(N3))
     &            -(Y(N1)-Y(N3))*(XFLOT(LTT,IFLOT)-X(N3))
              DET3=(X(N2)-X(N1))*(YFLOT(LTT,IFLOT)-Y(N1))
     &            -(Y(N2)-Y(N1))*(XFLOT(LTT,IFLOT)-X(N1))
              IF(DET1.GE.0.D0.AND.DET2.GE.0.D0.AND.DET3.GE.0.D0) GOTO 30
!
20          CONTINUE
!
            IF(LNG.EQ.1) WRITE(LU,33) IFLOT
            IF(LNG.EQ.2) WRITE(LU,34) IFLOT
33          FORMAT(1X,'ERREUR D''INTERPOLATION DANS DERIVE :',/,
     &             1X,'LARGAGE DU FLOTTEUR',I6,/,
     &             1X,'EN DEHORS DU DOMAINE DE CALCUL')
34          FORMAT(1X,'INTERPOLATION ERROR IN DERIVE :',/,
     &             1X,'DROP POINT OF FLOAT',I6,/,
     &             1X,'OUT OF THE DOMAIN')
            STOP
!
! ELEMENT CONTAINING THE POINT OF RELEASE, COMPUTES THE SHPFLO
! ---------------------------------------------------------------
!
30          CONTINUE
            SHPFLO(1,IFLOT) = DET1*SURDET(IELEM)
            SHPFLO(2,IFLOT) = DET2*SURDET(IELEM)
            SHPFLO(3,IFLOT) = DET3*SURDET(IELEM)
            ELTFLO (IFLOT)  = IELEM
!
          ELSE
            IF(LNG.EQ.1) WRITE(LU,123) IELM
            IF(LNG.EQ.2) WRITE(LU,124) IELM
123         FORMAT(1X,'DERIVE : TYPE D''ELEMENT NON PREVU : ',1I6)
124         FORMAT(1X,'DERIVE : UNEXPECTED TYPE OF ELEMENT: ',1I6)
            CALL PLANTE(1)
            STOP
          ENDIF
!
        ELSEIF(LT.GT.DEBFLO(IFLOT).AND.LT.LE.FINFLO(IFLOT)) THEN
!
!-----------------------------------------------------------------------
!
!   - COMPUTES THE SUCCESSIVE POSITIONS OF THIS FLOAT WHICH IS
!     CARRIED WITHOUT FRICTION BY THE CURRENT (SUBSEQUENT TIMESTEPS)
!
!-----------------------------------------------------------------------
!
! NUMBER OF RUNGE-KUTTA SUB-STEPS, BY CROSSED ELEMENT
! ======================================================
!
          NRK = 3
!
          XFLOT(LTT,IFLOT) = XFLOT(LTP,IFLOT)
          YFLOT(LTT,IFLOT) = YFLOT(LTP,IFLOT)
!
          IF(IELM.EQ.11) THEN
!
!  P1 TRIANGLES
!  ============
!
            CALL CHAR11( U , V , DT , NRK , X , Y , IKLE , IFABOR ,
     &                   XFLOT(LTT,IFLOT) , YFLOT(LTT,IFLOT) , DX , DY ,
     &                   SHPFLO(1,IFLOT) , ELTFLO(IFLOT) , NSP ,
     &                   1 , NPOIN , NELEM , NELMAX , SURDET ,  1,T8 )
!
          ELSE
!
            IF(LNG.EQ.1) WRITE(LU,123) IELM
            IF(LNG.EQ.2) WRITE(LU,124) IELM
            STOP
!
          ENDIF
!
!  CASE OF LOST FLOATS
!  ========================
!
          IF(ELTFLO(IFLOT).LE.0) FINFLO(IFLOT) = LT
!
        ENDIF
!
10    CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
