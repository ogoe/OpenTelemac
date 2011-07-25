!                    ****************
                     SUBROUTINE COUPE
!                    ****************
!
     & (F,FINT,SCURV,NPOIN,IKLE3,IFABOR,X,Y,Z,SURFAC,NELEM2,NPOIN3,
     &  NETAGE,X1,Y1,Z1,X2,Y2,Z2)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    EXTRACTS A SLICE FROM THE DOMAIN FOLLOWING THE SEGMENT
!+                DEFINED BY (X1,Y1,Z1) AND (X2,Y2,Z2).
!
!history  F LEPEINTRE (LNH)
!+        25/11/97
!+        V5P1
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
!| F              |-->| FUNCTION TO INTERPOLATE
!| FINT           |<->| INTERPOLATED FUNCTION ALONG THE PROFILE
!| IFABOR         |-->| CORRESPONDENCE BETWEEN BOUNDARY EDGE AND 2D ELEMENT
!| IKLE3          |-->| GLOBAL 3D CONNECTIVITY
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D
!| NETAGE         |-->| NUMBER OF PLANES - 1
!| NPOIN          |-->| NUMBER OF POINTS ALONG THE PROFILE
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| SCURV          |<->| ABCISSAE OF INTERPOLATING POINTS ON THE PROFILE
!| SURFAC         |-->| AREA OF TRIANGLES IN 2D
!| X              |-->| MESH COORDINATE
!| X1             |-->| COORDINATE OF THE VERTICES OF THE SEGMENT
!| X2             |-->| COORDINATE OF THE VERTICES OF THE SEGMENT
!| Y              |-->| MESH COORDINATE
!| Y1             |-->| COORDINATE OF THE VERTICES OF THE SEGMENT
!| Y2             |-->| COORDINATE OF THE VERTICES OF THE SEGMENT
!| Z              |-->| MESH COORDINATE
!| Z1             |-->| COORDINATE OF THE VERTICES OF THE SEGMENT
!| Z2             |-->| COORDINATE OF THE VERTICES OF THE SEGMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) ::  NPOIN, NELEM2, NPOIN3, NETAGE
!
      INTEGER, INTENT(IN) ::  IKLE3(NELEM2,NETAGE,6)
      INTEGER, INTENT(IN) ::  IFABOR(NELEM2,3)
!
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: FINT(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: SCURV(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN3), Y(NPOIN3), Z(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELEM2)
      DOUBLE PRECISION, INTENT(IN)    :: X1,Y1,Z1,X2,Y2,Z2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! LOGICAL VALUE WHICH INDICATES WHETHER THE SUPPORT LINE IS IN THE DOMAIN
!
      LOGICAL ENTREE
!
      INTEGER IETAGE,IELAR,IPOIN,IFAC,IELARN, NOEUD
!
      DOUBLE PRECISION XET,YET,ZET,EPSILO
      DOUBLE PRECISION XC,YC,ZC,XS1,YS1,XS2,YS2,A1,A2,DX2
      DOUBLE PRECISION XAR1,XAR2,XAR3,YAR1,YAR2,YAR3
      DOUBLE PRECISION ZAR1,ZAR2,ZAR3,ZAR4,ZAR5,ZAR6
      DOUBLE PRECISION ZS1H,ZS2H,ZS3H,ZS1B,ZS2B,ZS3B,ZHAU,ZBAS
      DOUBLE PRECISION ALPHA,BETA,GAMMA
      DOUBLE PRECISION SHP1,SHP2,SHP3,SHP4,SHP5,SHP6
      DOUBLE PRECISION F1,F2,F3,F4,F5,F6
!
      INTRINSIC MIN,ABS,SQRT
!
      DATA EPSILO /1.D-6/
!
!***********************************************************************
!
! COORDINATES OF THE ENTRY POINT OF THE SUPPORT LINE IN THE DOMAIN
!
      XET = X1
      YET = Y1
      ZET = Z1
!
      ENTREE = .FALSE.
!
      IELAR = 1
!
      CALL OV( 'X=C     ' , SCURV, Y , Z , 1.D+60 , NPOIN )
      CALL OV( 'X=C     ' , FINT , Y , Z , 1.D+60 , NPOIN )
!
      POINTBOUCLE: DO IPOIN = 1,NPOIN
!
!        COORDINATES OF THE CURRENT POINT
!
100      CONTINUE   ! A GOTO TARGET...
!
         XC = XET + (X2-X1)*(IPOIN-1)/(NPOIN-1)
         YC = YET + (Y2-Y1)*(IPOIN-1)/(NPOIN-1)
         ZC = ZET + (Z2-Z1)*(IPOIN-1)/(NPOIN-1)
!
!        MESH ELEMENT THE CURRENT POINT BELONGS TO
!
!        IDENTIFIES THE 2D ELEMENT FIRST, THEN THE LAYER
!
!        2D LOCATION
!
!        CARTESIAN EQUATION OF A FACE: A1*(X-XS1) + A2*(Y-YS1) = 0
!        WHERE XS1,YS1 ARE THE COORDINATES OF A POINT OF THE FACE
!
!        BELONGS TO ELEMENT IELAR: THE ARRIVAL POINT IS ON THE CORRECT
!        SIDE OF THE THREE FACES OF THE ELEMENT
!
30       CONTINUE   ! A GOTO TARGET...
!
!        COORDINATES OF THE FIRST VERTEX
         XS1 = X    (IKLE3(IELAR,1     ,1))
         YS1 = Y    (IKLE3(IELAR,1     ,1))
!
         FACBOUCLE: DO IFAC = 3 , 1 , -1
!
!           ASSUMES VERTICES ARE GIVEN ANTI-CLOCKWISE :
!           USES THE FACT THAT POINT 1 OF FACE N IS
!           POINT 2 OF FACE N-1
!
!           FACE THROUGH VERTICES S1 AND S2
            XS2 = XS1
            YS2 = YS1
            XS1 = X    (IKLE3(IELAR,1     ,IFAC  ))
            YS1 = Y    (IKLE3(IELAR,1     ,IFAC  ))
!
            A1 = YS1 - YS2
            A2 = XS2 - XS1
            DX2 = A1*(XC-XS1) + A2*(YC-YS1)
!
!           TEST: IS THE ARRIVAL POINT ON THE CORRECT SIDE OF THE FACE?
            IF (DX2.LT.-SURFAC(IELAR)*EPSILO) THEN
!              ON THE WRONG SIDE OF THE FACE
!              GO TO NEXT ELEMENT
               IELARN = IFABOR(IELAR,IFAC)
               IF (IELARN.LE.0) THEN
!
!                 OUTSIDE OF THE DOMAIN
                  IF (.NOT.ENTREE) THEN
!                    IF HAS NOT ALREADY ENTERED, STARTS AGAIN
!                    PROGRESSING ALONG THE SUPPORT LINE
                     XET = XET + (X2-X1)/(NPOIN-1)
                     YET = YET + (Y2-Y1)/(NPOIN-1)
                     ZET = ZET + (Z2-Z1)/(NPOIN-1)
                     IF ((XET-X2)*(XET-X1).GT.EPSILO*SURFAC(IELAR) .OR.
     &                   (YET-Y2)*(YET-Y1).GT.EPSILO*SURFAC(IELAR) .OR.
     &                   (ZET-Z2)*(ZET-Z1).GT.EPSILO*SURFAC(IELAR)) THEN
!                       THE SUPPORT LINE DOES NOT CROSS THE DOMAIN
                        IF (LNG.EQ.1) WRITE(LU,21) X1,Y1,Z1,X2,Y2,Z2
                        IF (LNG.EQ.2) WRITE(LU,22) X1,Y1,Z1,X2,Y2,Z2
                        RETURN
                     ENDIF
                     GOTO 100
                  ELSE
!                    HAS JUST LEFT THE DOMAIN
                     DO NOEUD=IPOIN,NPOIN
                        SCURV(NOEUD) = SCURV(IPOIN-1)
                        FINT (NOEUD) = FINT (IPOIN-1)
                     END DO
                     RETURN
                  ENDIF
!
               ELSE
                  IELAR = IELARN
               ENDIF
               GOTO 30
            ENDIF
!
         END DO FACBOUCLE
!
!     IDENTIFIES THE LAYER
!
!     COORDINATES IN THE 2D REFERENCE ELEMENT
      XAR1 = X    (IKLE3(IELAR,1     ,1))
      XAR2 = X    (IKLE3(IELAR,1     ,2))
      XAR3 = X    (IKLE3(IELAR,1     ,3))
      YAR1 = Y    (IKLE3(IELAR,1     ,1))
      YAR2 = Y    (IKLE3(IELAR,1     ,2))
      YAR3 = Y    (IKLE3(IELAR,1     ,3))
      ALPHA = ((XC  -XAR1)*(YAR3-YAR1) - (XAR3-XAR1)*(YC  -YAR1))/
     &        ((XAR2-XAR1)*(YAR3-YAR1) - (XAR3-XAR1)*(YAR2-YAR1))
      BETA  = ((XAR2-XAR1)*(YC  -YAR1) - (XC  -XAR1)*(YAR2-YAR1))/
     &        ((XAR2-XAR1)*(YAR3-YAR1) - (XAR3-XAR1)*(YAR2-YAR1))
!
!     COMPUTES THE LAYER
      ZS1H = Z(IKLE3(IELAR,NETAGE,4))
      ZS2H = Z(IKLE3(IELAR,NETAGE,5))
      ZS3H = Z(IKLE3(IELAR,NETAGE,6))
      ZS1B = Z(IKLE3(IELAR,1     ,1))
      ZS2B = Z(IKLE3(IELAR,1     ,2))
      ZS3B = Z(IKLE3(IELAR,1     ,3))
!
      ZHAU = (1.D0-ALPHA-BETA)*ZS1H + ALPHA*ZS2H + BETA*ZS3H
      ZBAS = (1.D0-ALPHA-BETA)*ZS1B + ALPHA*ZS2B + BETA*ZS3B
!
!     ARE WE IN THE DOMAIN ?
!
      IF ( (ZC-ZHAU)/(ZHAU-ZBAS).GT.EPSILO .OR.
     &     (ZBAS-ZC)/(ZHAU-ZBAS).GT.EPSILO      ) THEN
         IF (.NOT.ENTREE) THEN
!          IF HAS NOT ALREADY ENTERED, STARTS AGAIN
!          PROGRESSING ALONG THE SUPPORT LINE
           XET = XET + (X2-X1)/(NPOIN-1)
           YET = YET + (Y2-Y1)/(NPOIN-1)
           ZET = ZET + (Z2-Z1)/(NPOIN-1)
           IF ((XET-X2)*(XET-X1).GT.EPSILO*SURFAC(IELAR) .OR.
     &         (YET-Y2)*(YET-Y1).GT.EPSILO*SURFAC(IELAR) .OR.
     &         (ZET-Z2)*(ZET-Z1).GT.EPSILO*SURFAC(IELAR)     ) THEN
!             THE SUPPORT LINE DOES NOT CROSS THE DOMAIN
              IF (LNG.EQ.1) WRITE(LU,21) X1,Y1,Z1,X2,Y2,Z2
              IF (LNG.EQ.2) WRITE(LU,22) X1,Y1,Z1,X2,Y2,Z2
              RETURN
           ENDIF
           GOTO 100
         ELSE
!          HAS JUST LEFT THE DOMAIN
           DO NOEUD=IPOIN,NPOIN
              SCURV(NOEUD) = SCURV(IPOIN-1)
              FINT (NOEUD) = FINT (IPOIN-1)
           END DO
           RETURN
         ENDIF
      ENDIF
!
!     NUMBER OF THE LAYER
      IETAGE = MIN(1+INT(NETAGE*ABS((ZC-ZBAS)/(ZHAU-ZBAS))),NETAGE)
!
!     WE ARE IN THE DOMAIN
      IF (.NOT.ENTREE) ENTREE = .TRUE.
!
      ZAR1 = Z    (IKLE3(IELAR,IETAGE,1))
      ZAR2 = Z    (IKLE3(IELAR,IETAGE,2))
      ZAR3 = Z    (IKLE3(IELAR,IETAGE,3))
      ZAR4 = Z    (IKLE3(IELAR,IETAGE,4))
      ZAR5 = Z    (IKLE3(IELAR,IETAGE,5))
      ZAR6 = Z    (IKLE3(IELAR,IETAGE,6))
!
!     COORDINATES IN THE 3D REFERENCE ELEMENT
      A1 = ((1.D0-ALPHA-BETA)*ZAR1 + ALPHA*ZAR2 + BETA*ZAR3)/2.D0
      A2 = ((1.D0-ALPHA-BETA)*ZAR4 + ALPHA*ZAR5 + BETA*ZAR6)/2.D0
      GAMMA = ( ZC-A1-A2)/(A2-A1)
!
!     BASE FUNCTIONS IN THE REFERENCE ELEMENT
      SHP1 = (1.D0-ALPHA-BETA)*(1.D0-GAMMA)/2.D0
      SHP2 =       ALPHA      *(1.D0-GAMMA)/2.D0
      SHP3 =             BETA *(1.D0-GAMMA)/2.D0
      SHP4 = (1.D0-ALPHA-BETA)*(1.D0+GAMMA)/2.D0
      SHP5 =       ALPHA      *(1.D0+GAMMA)/2.D0
      SHP6 =             BETA *(1.D0+GAMMA)/2.D0
!
!     INTERPOLATES
      F1 = F    (IKLE3(IELAR,IETAGE,1))
      F2 = F    (IKLE3(IELAR,IETAGE,2))
      F3 = F    (IKLE3(IELAR,IETAGE,3))
      F4 = F    (IKLE3(IELAR,IETAGE,4))
      F5 = F    (IKLE3(IELAR,IETAGE,5))
      F6 = F    (IKLE3(IELAR,IETAGE,6))
!
      FINT(IPOIN) = SHP1*F1 + SHP2*F2 + SHP3*F3 + SHP4*F4 + SHP5*F5
     &            + SHP6*F6
!
!     COMPUTES THE CURVILINEAR X-COORDINATE
      SCURV(IPOIN) = SQRT( (XC-X1)**2 + (YC-Y1)**2 + (ZC-Z1)**2 )
!
      END DO POINTBOUCLE
!
!-----------------------------------------------------------------------
!
21    FORMAT('COUPE : LA COUPE NE TRAVERSE PAS LE DOMAINE',/,
     &       'X1,Y1,Z1 :',3G16.7,/,'X2,Y2,Z2 :',3G16.7)
22    FORMAT('COUPE : THE CROSSING LINE DO NOT CROSS THE DOMAIN',/,
     &       'X1,Y1,Z1 :',3G16.7,/,'X2,Y2,Z2 :',3G16.7)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE COUPE
