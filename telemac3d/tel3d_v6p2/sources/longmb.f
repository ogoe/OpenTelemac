!                    *****************
                     SUBROUTINE LONGMB
!                    *****************
!
     &(LM,Z,HN,NPOIN3,NPOIN2,NPLAN,U,V,X,Y,P1,P2,P3,NTRAC,TA,KARMAN,ZF)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE SQUARE OF THE MIXING LENGTH
!+                FOR A BUOYANT JET.
!
!history  F MARCOS    (LNH)
!+        25/11/97
!+        V5P4
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
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
!| HN             |-->| WATER DEPTH AT TIME N
!| KARMAN         |-->| KARMAN CONSTANT
!| LM             |<->| MIXING LENGTH
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| NTRAC          |-->| NUMBER OF ACTIVE TRACERS
!| P1             |<->| WORK ARRAY
!| P2             |<->| WORK ARRAY
!| P3             |<->| WORK ARRAY
!| TA             |-->| ACTIVE TRACERS CONCENTRATIONS
!| U              |-->| HORIZONTAL COMPONENT OF VELOCITY
!| V              |-->| HORIZONTAL COMPONENT OF VELOCITY
!| X              |-->| HORIZONTAL COORDINATE
!| Y              |-->| HORIZONTAL COORDINATE
!| Z              |-->| ELEVATION OF REAL 3D MESH POINTS
!| ZF             |-->| ELEVATION OF BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN3, NPOIN2, NPLAN, NTRAC
!
      DOUBLE PRECISION, INTENT(INOUT) :: LM(NPOIN3) ! POINTER
      DOUBLE PRECISION, INTENT(IN)    :: Z(NPOIN3), HN(NPOIN2),KARMAN
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN3), V(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN3), Y(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: P1(NPOIN3), P2(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: P3(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: TA(NPOIN3),ZF(NPOIN2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I, IPLAN, I3D, I3DI, I3DS, I3DM
      INTEGER IMAX, ICHSUP, ICHINF, I3DMIN, IMIN
      DOUBLE PRECISION  ZCHSUP, ZCHINF,COSA, SINA, EPS
      DOUBLE PRECISION VMAX, VMIN, VIT, ZMAX, DELT, ZINT, LMN, TAMAX
!
!***********************************************************************
!
! SQUARE OF THE MIXING LENGTH ALONG Z
! (SEE : RODI , TURBULENCE MODELS AND THEIR APPLICATIONS IN HYDRAULICS
!        IAHR)
!
!
      IF(NTRAC.EQ.0) THEN
        IF (LNG.EQ.1) WRITE(LU,101)
        IF (LNG.EQ.2) WRITE(LU,102)
        CALL PLANTE(1)
        STOP
      ENDIF
!
      DO I=1,NPOIN2
!
        VMAX=0.D0
        VMIN=5.D10
        IMAX=1
        ZMAX=0.D0
        DO IPLAN=1,NPLAN
          I3D = I + (IPLAN-1)*NPOIN2
          IF((Z(I3D)-Z(I)).LE.0.2D0*HN(I)) THEN
            LM(I3D)=(KARMAN*(Z(I3D)-ZF(I)))**2
          ELSE
            LM(I3D)=(0.2D0*KARMAN*HN(I))**2
          ENDIF
        ENDDO
!
        I3DMIN=0
        TAMAX=0.D0
        DO IPLAN=1,NPLAN
          I3D = I + (IPLAN-1)*NPOIN2
          VIT=SQRT(U(I3D)**2+V(I3D)**2)
          IF(TA(I3D).GT.TAMAX) THEN
            TAMAX=TA(I3D)
          ENDIF
          IF(VIT.LT.VMIN) THEN
            VMIN=VIT
            I3DMIN=I3D
            IMIN=IPLAN
          ENDIF
        ENDDO
!
        IF(TAMAX.LT.1.D-2) GOTO 95
        DO IPLAN=1,NPLAN
          I3D = I + (IPLAN-1)*NPOIN2
          P2(I3D)=U(I3D)-U(I3DMIN)
          P3(I3D)=V(I3D)-V(I3DMIN)
        ENDDO
!
        DO IPLAN=1,NPLAN
          I3D = I + (IPLAN-1)*NPOIN2
          VIT=SQRT(P2(I3D)**2+P3(I3D)**2)
          IF(VIT.GT.VMAX) THEN
            VMAX=VIT
            IMAX=IPLAN
            ZMAX=Z(I3D)
          ENDIF
          IF(ABS(VIT-VMAX).LT.1D-6) THEN
            IF(ABS(IPLAN-IMAX).LT.2) ZMAX=(Z(I3D)+ZMAX)*0.5D0
          ENDIF
        ENDDO
        IF((ABS(VMAX).LT.1D-6).OR.(TAMAX.LT.1.D-2)) GOTO 95
        I3DM=I+(IMAX-1)*NPOIN2
        COSA=P2(I3DM)/VMAX
        SINA=P3(I3DM)/VMAX
        DO IPLAN=1,NPLAN
          I3D=I+(IPLAN-1)*NPOIN2
          P1(I3D)=P2(I3D)*COSA+P3(I3D)*SINA
        ENDDO
!
        ICHSUP=1
        ICHINF=NPLAN
        ZCHSUP=Z(I)
        ZCHINF=Z(I+(NPLAN-1)*NPOIN2)
!
        IF(ABS(P1(I+(NPLAN-1)*NPOIN2)).GT.VMAX/100.D0) THEN
          ICHSUP=NPLAN
          ZCHSUP=Z(I+(NPLAN-1)*NPOIN2)
        ELSE
          DO IPLAN=NPLAN-1,IMAX,-1
            I3D = I + (IPLAN-1)*NPOIN2
            I3DS= I + (IPLAN  )*NPOIN2
             IF ((ABS(P1(I3D)).GT.VMAX/100.D0).AND.
     &           (ABS(P1(I3DS)).LT.VMAX/100.D0).AND.
     &           (ABS(P1(I3D)-P1(I3DS)).GT.1.D-6))   THEN
                 IF(P1(I3D).LT.0.D0) THEN
                   EPS=-1.D0
                 ELSE
                   EPS=1.D0
                 ENDIF
                 ICHSUP=MAX(ICHSUP,IPLAN)
              ZINT=Z(I3DS)+(Z(I3D)-Z(I3DS))*(EPS*VMAX/100.D0-P1(I3DS))
     &                 /(P1(I3D)-P1(I3DS))
                 ZCHSUP=MAX(ZCHSUP,ZINT)
               ENDIF
          ENDDO
        ENDIF
!
        IF(ABS(P1(I)).GT.VMAX/100.D0) THEN
          ICHINF=1
          ZCHINF=Z(I)
        ELSE
          DO IPLAN=2,IMAX
            I3D = I + (IPLAN-1)*NPOIN2
            I3DI= I + (IPLAN-2)*NPOIN2
            IF((ABS(P1(I3D)).GT.VMAX/100.D0).AND.
     &         (ABS(P1(I3DI)).LT.VMAX/100.D0).AND.
     &          (ABS(P1(I3D)-P1(I3DI)).GT.1.D-6))   THEN
                IF(P1(I3D).LT.0.D0) THEN
                  EPS=-1.D0
                ELSE
                  EPS=1.D0
                ENDIF
                ICHSUP=MAX(ICHSUP,IPLAN)
                ICHINF=MIN(ICHINF,IPLAN)
             ZINT=Z(I3DI)+(Z(I3D)-Z(I3DI))*(EPS*VMAX/100.D0-P1(I3DI))
     &                 /(P1(I3D)-P1(I3DI))
                ZCHINF=MIN(ZCHINF,ZINT)
            ENDIF
          ENDDO
        ENDIF
!
        IF(IMAX.NE.NPLAN) THEN
          DO IPLAN=IMAX,NPLAN
            I3D = I + (IPLAN-1)*NPOIN2
            DELT=ABS(ZMAX-ZCHSUP)
            LMN =(0.2*KARMAN*DELT)**2
            LM(I3D)=MIN(LM(I3D),LMN)
          ENDDO
        ENDIF
!
        IF(IMAX.NE.1) THEN
          DO IPLAN=1,IMAX
            I3D = I + (IPLAN-1)*NPOIN2
            DELT=ABS(ZMAX-ZCHINF)
            LMN =(0.2D0*KARMAN*DELT)**2
            LM(I3D)=MIN(LM(I3D),LMN)
          ENDDO
        ENDIF
!
        IF(IMAX.EQ.1.OR.IMAX.EQ.NPLAN) GOTO 95
!
        I3D=I+(IMAX-1)*NPOIN2
        DELT=ABS(ZCHINF-ZCHSUP)/2.D0
        LMN =(0.2D0*KARMAN*DELT)**2
        LM(I3D)=MIN(LM(I3D),LMN)
90      CONTINUE
95      CONTINUE
!
      ENDDO
!
!-----------------------------------------------------------------------
!
101   FORMAT(' LONGMB : CE MODELE DE LONGUEUR DE MELANGE EST RESERVE',/,
     &       '          AUX JETS FLOTTANTS, MAIS ICI NTRAC = 0')
102   FORMAT(' LONGMB : THIS MIXING LENGTH MODEL IS DEDICATED TO',/,
     &       '          BUOYANT JETS, BUT HERE NTRAC = 0')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
