!                    *****************
                     SUBROUTINE PROXIM
!                    *****************
!
     &(IP,XP,YP,X,Y,NP,NPOIN,IKLE,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    IDENTIFIES THE POINTS OF THE MESH CLOSEST TO A SET
!+                OF GIVEN POINTS.
!
!history  J-M HERVOUET (LNHE)
!+        03/07/2009
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
!| IKLE           |-->| CONNECTIVITY TABLE.
!| IP             |<--| ADDRESSES OF NEAREST POINTS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NP             |-->| NUMBER OF POINTS IN THE SET
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XP             |-->| ABSCISSAE OF POINTS IN THE SET
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| YP             |-->| ORDINATES OF POINTS IN THE SET
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_PROXIM => PROXIM
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NP,NPOIN,NELEM,NELMAX
      INTEGER, INTENT(INOUT) :: IP(NP)
      INTEGER, INTENT(IN)    :: IKLE(NELMAX,3)
!
      DOUBLE PRECISION, INTENT(IN) :: XP(NP),YP(NP),X(NPOIN),Y(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,K,IELEM
      DOUBLE PRECISION X1,Y1,X2,Y2,X3,Y3,A31,A12,A23,DIST2,D2,ALERT
      DOUBLE PRECISION XX,YY
!
      INTRINSIC SQRT
!
      DOUBLE PRECISION P_DSUM,P_DMAX
      EXTERNAL         P_DSUM,P_DMAX
!
!-----------------------------------------------------------------------
!
      DO 10 K=1,NP
        IP(K)=0
        DIST2=1.D10
        ALERT=0.D0
        XX=-1.D10
        YY=-1.D10
!
!       LOOP ON THE TRIANGLES:
!
        DO 20 IELEM=1,NELEM
          X1=X(IKLE(IELEM,1))
          X2=X(IKLE(IELEM,2))
          X3=X(IKLE(IELEM,3))
          Y1=Y(IKLE(IELEM,1))
          Y2=Y(IKLE(IELEM,2))
          Y3=Y(IKLE(IELEM,3))
          A31=XP(K)*Y3-YP(K)*X3+X3*Y1-X1*Y3+X1*YP(K)-XP(K)*Y1
          A12=XP(K)*Y1-YP(K)*X1+X1*Y2-X2*Y1+X2*YP(K)-XP(K)*Y2
          A23=XP(K)*Y2-YP(K)*X2+X2*Y3-X3*Y2+X3*YP(K)-XP(K)*Y3
          IF(A31.GT.-1.D-6.AND.A12.GT.-1.D-6.AND.A23.GT.-1.D-6) THEN
!           TAKES THE NEAREST NODE
            DO I=1,3
              D2=(XP(K)-X(IKLE(IELEM,I)))**2+(YP(K)-Y(IKLE(IELEM,I)))**2
              IF(D2.LT.DIST2) THEN
                IP(K)=IKLE(IELEM,I)
                DIST2=D2
              ENDIF
            ENDDO
          ENDIF
20      CONTINUE
        IF(IP(K).EQ.0) THEN
          IF(LNG.EQ.1) WRITE(LU,*) 'POINT SOURCE ',K,' HORS DOMAINE'
          IF(LNG.EQ.2) WRITE(LU,*) 'SOURCE POINT ',K,' OUTSIDE DOMAIN'
          IF(NCSIZE.LE.1) THEN
            WRITE(LU,*) ' '
            IF(LNG.EQ.1) WRITE(LU,*) 'INTERDIT EN MODE SCALAIRE'
            IF(LNG.EQ.2) WRITE(LU,*) 'NOT ALLOWED IN SCALAR MODE'
            CALL PLANTE(1)
            STOP
          ENDIF
        ELSE
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'POINT SOURCE ',K,' ASSIMILE AU POINT ',IP(K)
            WRITE(LU,*) 'SITUE A ',SQRT(DIST2),' METRES'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'SOURCE POINT ',K,' PUT ON POINT ',IP(K)
            WRITE(LU,*) 'LOCATED AT ',SQRT(DIST2),' METRES'
          ENDIF
          IF(SQRT(DIST2).GT.1.D-6.AND.NCSIZE.GT.1) THEN
            XX=X(IP(K))
            YY=Y(IP(K))
            ALERT=1.D0
          ENDIF
        ENDIF
        IF(NCSIZE.GT.1) THEN
          XX=P_DMAX(XX)
          YY=P_DMAX(YY)
          ALERT=P_DSUM(ALERT)
        ENDIF
        IF(ALERT.GT.0.5D0) THEN
          WRITE(LU,*) ' '
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'EN MODE PARALLELE LES SOURCES PONCTUELLES'
            WRITE(LU,*) 'DOIVENT COINCIDER EXACTEMENT AVEC DES POINTS'
            WRITE(LU,*) 'DU MAILLAGE, POUR LA SOURCE ',K,' CHOISIR :'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'IN PARALLEL SOURCES MUST COINCIDE WITH'
            WRITE(LU,*) 'NODES IN THE MESH, FOR SOURCE',K,' CHOOSE:'
          ENDIF
          WRITE(LU,*) 'X=',XX,' Y=',YY
          CALL PLANTE(1)
          STOP
        ENDIF
10    CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
