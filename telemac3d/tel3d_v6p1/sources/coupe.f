C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       EXTRACTS A SLICE FROM THE DOMAIN FOLLOWING THE SEGMENT
!>                DEFINED BY (X1,Y1,Z1) AND (X2,Y2,Z2).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> F, FINT, IFABOR, IKLE3, NELEM2, NETAGE, NPOIN, NPOIN3, SCURV, SURFAC, X, X1, X2, Y, Y1, Y2, Z, Z1, Z2
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A1, A2, ALPHA, BETA, DX2, ENTREE, EPSILO, F1, F2, F3, F4, F5, F6, GAMMA, IELAR, IELARN, IETAGE, IFAC, IPOIN, NOEUD, SHP1, SHP2, SHP3, SHP4, SHP5, SHP6, XAR1, XAR2, XAR3, XC, XET, XS1, XS2, YAR1, YAR2, YAR3, YC, YET, YS1, YS2, ZAR1, ZAR2, ZAR3, ZAR4, ZAR5, ZAR6, ZBAS, ZC, ZET, ZHAU, ZS1B, ZS1H, ZS2B, ZS2H, ZS3B, ZS3H
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OV()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SCOPE()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>      <tr>
!>      <td><center> 5.1                                       </center>
!> </td><td> 25/11/97
!> </td><td> F LEPEINTRE (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>F
!></td><td>--></td><td>FONCTION A INTERPOLER
!>    </td></tr>
!>          <tr><td>FINT
!></td><td><--</td><td>FONCTION INTERPOLEE LE LONG DU PROFIL
!>    </td></tr>
!>          <tr><td>IFABOR
!></td><td>--></td><td>CORRESPONDANCE FACE DE BORD - ELEMENT 2D
!>    </td></tr>
!>          <tr><td>IKLE3
!></td><td>--></td><td>TABLE DE CONNECTIVITE 3D
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NOMBRE D'ELEMENTS 2D
!>    </td></tr>
!>          <tr><td>NETAGE
!></td><td>--></td><td>NOMBRE D'ETAGES
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS LE LONG DU PROFIL
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NOMBRE DE POINTS 3D
!>    </td></tr>
!>          <tr><td>SCURV
!></td><td><--</td><td>ABCISSE DES POINTS D'INTERPOLATION DU PROFIL
!>    </td></tr>
!>          <tr><td>SURFAC
!></td><td>--></td><td>SURFACE DES ELEMENTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>X,Y,Z
!></td><td>--></td><td>COORDONNEES DU MAILLAGE
!>    </td></tr>
!>          <tr><td>X1,Y1,Z1
!></td><td>--></td><td>COORDONNEES DES EXTREMITES DU SEGMENT
!>    </td></tr>
!>          <tr><td>X2,Y2,Z2
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE COUPE
     & (F,FINT,SCURV,NPOIN,IKLE3,IFABOR,X,Y,Z,SURFAC,NELEM2,NPOIN3,
     &  NETAGE,X1,Y1,Z1,X2,Y2,Z2)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F             |-->| FONCTION A INTERPOLER
C| FINT           |<--| FONCTION INTERPOLEE LE LONG DU PROFIL
C| IFABOR         |-->| CORRESPONDANCE FACE DE BORD - ELEMENT 2D
C| IKLE3          |-->| TABLE DE CONNECTIVITE 3D
C| NELEM2         |-->| NOMBRE D'ELEMENTS 2D
C| NETAGE         |-->| NOMBRE D'ETAGES
C| NPOIN          |-->| NOMBRE DE POINTS LE LONG DU PROFIL
C| NPOIN3         |-->| NOMBRE DE POINTS 3D
C| SCURV          |<--| ABCISSE DES POINTS D'INTERPOLATION DU PROFIL
C| SURFAC         |-->| SURFACE DES ELEMENTS DU MAILLAGE 2D
C| X,Y,Z          |-->| COORDONNEES DU MAILLAGE
C| X1,Y1,Z1       |-->| COORDONNEES DES EXTREMITES DU SEGMENT
C| X2,Y2,Z2       |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
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
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C LOGICAL VALUE WHICH INDICATES WHETHER THE SUPPORT LINE IS IN THE DOMAIN
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
C COORDINATES OF THE ENTRY POINT OF THE SUPPORT LINE IN THE DOMAIN
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
C        COORDINATES OF THE CURRENT POINT
!
100      CONTINUE   ! A GOTO TARGET...
!
         XC = XET + (X2-X1)*(IPOIN-1)/(NPOIN-1)
         YC = YET + (Y2-Y1)*(IPOIN-1)/(NPOIN-1)
         ZC = ZET + (Z2-Z1)*(IPOIN-1)/(NPOIN-1)
!
C        MESH ELEMENT THE CURRENT POINT BELONGS TO
!
C        IDENTIFIES THE 2D ELEMENT FIRST, THEN THE LAYER
!
C        2D LOCATION
!
C        CARTESIAN EQUATION OF A FACE: A1*(X-XS1) + A2*(Y-YS1) = 0
C        WHERE XS1,YS1 ARE THE COORDINATES OF A POINT OF THE FACE
!
C        BELONGS TO ELEMENT IELAR: THE ARRIVAL POINT IS ON THE CORRECT
C        SIDE OF THE THREE FACES OF THE ELEMENT
!
30       CONTINUE   ! A GOTO TARGET...
!
C        COORDINATES OF THE FIRST VERTEX
         XS1 = X    (IKLE3(IELAR,1     ,1))
         YS1 = Y    (IKLE3(IELAR,1     ,1))
!
         FACBOUCLE: DO IFAC = 3 , 1 , -1
!
C           ASSUMES VERTICES ARE GIVEN ANTI-CLOCKWISE :
C           USES THE FACT THAT POINT 1 OF FACE N IS
C           POINT 2 OF FACE N-1
!
C           FACE THROUGH VERTICES S1 AND S2
            XS2 = XS1
            YS2 = YS1
            XS1 = X    (IKLE3(IELAR,1     ,IFAC  ))
            YS1 = Y    (IKLE3(IELAR,1     ,IFAC  ))
!
            A1 = YS1 - YS2
            A2 = XS2 - XS1
            DX2 = A1*(XC-XS1) + A2*(YC-YS1)
!
C           TEST: IS THE ARRIVAL POINT ON THE CORRECT SIDE OF THE FACE?
            IF (DX2.LT.-SURFAC(IELAR)*EPSILO) THEN
C              ON THE WRONG SIDE OF THE FACE
C              GO TO NEXT ELEMENT
               IELARN = IFABOR(IELAR,IFAC)
               IF (IELARN.LE.0) THEN
!
C                 OUTSIDE OF THE DOMAIN
                  IF (.NOT.ENTREE) THEN
C                    IF HAS NOT ALREADY ENTERED, STARTS AGAIN
C                    PROGRESSING ALONG THE SUPPORT LINE
                     XET = XET + (X2-X1)/(NPOIN-1)
                     YET = YET + (Y2-Y1)/(NPOIN-1)
                     ZET = ZET + (Z2-Z1)/(NPOIN-1)
                     IF ((XET-X2)*(XET-X1).GT.EPSILO*SURFAC(IELAR) .OR.
     &                   (YET-Y2)*(YET-Y1).GT.EPSILO*SURFAC(IELAR) .OR.
     &                   (ZET-Z2)*(ZET-Z1).GT.EPSILO*SURFAC(IELAR)) THEN
C                       THE SUPPORT LINE DOES NOT CROSS THE DOMAIN
                        IF (LNG.EQ.1) WRITE(LU,21) X1,Y1,Z1,X2,Y2,Z2
                        IF (LNG.EQ.2) WRITE(LU,22) X1,Y1,Z1,X2,Y2,Z2
                        RETURN
                     ENDIF
                     GOTO 100
                  ELSE
C                    HAS JUST LEFT THE DOMAIN
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
C     IDENTIFIES THE LAYER
!
C     COORDINATES IN THE 2D REFERENCE ELEMENT
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
C     COMPUTES THE LAYER
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
C     ARE WE IN THE DOMAIN ?
!
      IF ( (ZC-ZHAU)/(ZHAU-ZBAS).GT.EPSILO .OR.
     &     (ZBAS-ZC)/(ZHAU-ZBAS).GT.EPSILO      ) THEN
         IF (.NOT.ENTREE) THEN
C          IF HAS NOT ALREADY ENTERED, STARTS AGAIN
C          PROGRESSING ALONG THE SUPPORT LINE
           XET = XET + (X2-X1)/(NPOIN-1)
           YET = YET + (Y2-Y1)/(NPOIN-1)
           ZET = ZET + (Z2-Z1)/(NPOIN-1)
           IF ((XET-X2)*(XET-X1).GT.EPSILO*SURFAC(IELAR) .OR.
     &         (YET-Y2)*(YET-Y1).GT.EPSILO*SURFAC(IELAR) .OR.
     &         (ZET-Z2)*(ZET-Z1).GT.EPSILO*SURFAC(IELAR)     ) THEN
C             THE SUPPORT LINE DOES NOT CROSS THE DOMAIN
              IF (LNG.EQ.1) WRITE(LU,21) X1,Y1,Z1,X2,Y2,Z2
              IF (LNG.EQ.2) WRITE(LU,22) X1,Y1,Z1,X2,Y2,Z2
              RETURN
           ENDIF
           GOTO 100
         ELSE
C          HAS JUST LEFT THE DOMAIN
           DO NOEUD=IPOIN,NPOIN
              SCURV(NOEUD) = SCURV(IPOIN-1)
              FINT (NOEUD) = FINT (IPOIN-1)
           END DO
           RETURN
         ENDIF
      ENDIF
!
C     NUMBER OF THE LAYER
      IETAGE = MIN(1+INT(NETAGE*ABS((ZC-ZBAS)/(ZHAU-ZBAS))),NETAGE)
!
C     WE ARE IN THE DOMAIN
      IF (.NOT.ENTREE) ENTREE = .TRUE.
!
      ZAR1 = Z    (IKLE3(IELAR,IETAGE,1))
      ZAR2 = Z    (IKLE3(IELAR,IETAGE,2))
      ZAR3 = Z    (IKLE3(IELAR,IETAGE,3))
      ZAR4 = Z    (IKLE3(IELAR,IETAGE,4))
      ZAR5 = Z    (IKLE3(IELAR,IETAGE,5))
      ZAR6 = Z    (IKLE3(IELAR,IETAGE,6))
!
C     COORDINATES IN THE 3D REFERENCE ELEMENT
      A1 = ((1.D0-ALPHA-BETA)*ZAR1 + ALPHA*ZAR2 + BETA*ZAR3)/2.D0
      A2 = ((1.D0-ALPHA-BETA)*ZAR4 + ALPHA*ZAR5 + BETA*ZAR6)/2.D0
      GAMMA = ( ZC-A1-A2)/(A2-A1)
!
C     BASE FUNCTIONS IN THE REFERENCE ELEMENT
      SHP1 = (1.D0-ALPHA-BETA)*(1.D0-GAMMA)/2.D0
      SHP2 =       ALPHA      *(1.D0-GAMMA)/2.D0
      SHP3 =             BETA *(1.D0-GAMMA)/2.D0
      SHP4 = (1.D0-ALPHA-BETA)*(1.D0+GAMMA)/2.D0
      SHP5 =       ALPHA      *(1.D0+GAMMA)/2.D0
      SHP6 =             BETA *(1.D0+GAMMA)/2.D0
!
C     INTERPOLATES
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
C     COMPUTES THE CURVILINEAR X-COORDINATE
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
C
C#######################################################################
C