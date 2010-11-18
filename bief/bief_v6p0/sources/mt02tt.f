C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE DIFFUSION MATRIX FOR TETRAHEDRONS.
!><br>            THE FUNCTION DIFFUSION COEFFICIENT IS HERE A P1
!>                DIAGONAL TENSOR.
!>  @code
!>     STORAGE CONVENTION FOR EXTRA-DIAGONAL TERMS:
!>
!>     XM(IELEM, 1)  ---->  M(1,2) = M(2,1)
!>     XM(IELEM, 2)  ---->  M(1,3) = M(3,1)
!>     XM(IELEM, 3)  ---->  M(1,4) = M(4,1)
!>     XM(IELEM, 4)  ---->  M(2,3) = M(3,2)
!>     XM(IELEM, 5)  ---->  M(2,4) = M(4,2)
!>     XM(IELEM, 6)  ---->  M(3,4) = M(4,3)
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> F, G, H, IKLE, INCHYD, NELEM, NELMAX, SF, SG, SH, T, X, XM, XMUL, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUX, AUX1, AUX2, AUX3, AUX4, AUX5, AUX6, AUX7, AUX8, AUX9, AUXX, AUXXX, COEF, FGTOT, FHTOT, FTOT, GHTOT, GTOT, HTOT, I1, I2, I3, I4, IELEM, ISO, SUR24, T1, T11, T13, T15, T17, T19, T21, T23, T28, T3, T35, T42, T49, T5, T51, T54, T7, T9, VTOT, WTOT, X2, X3, X4, Y2, Y3, Y4, Z2, Z3, Z4
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MT02TT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>MATRIY()

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
!>      <td><center> 5.3                                       </center>
!> </td><td> 28/11/94
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18; F  LEPEINTRE (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>A11,A12
!></td><td><--</td><td>ELEMENTS DE LA MATRICE
!>    </td></tr>
!>          <tr><td>F,G,H
!></td><td>--></td><td>FONCTIONS INTERVENANT DANS LE CALCUL DE LA
!>                  MATRICE.
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE1
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
!>    </td></tr>
!>          <tr><td>INCHYD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>SF,SG,SH
!></td><td>--></td><td>STRUCTURES DE F,G ET H.
!>    </td></tr>
!>          <tr><td>SW
!></td><td>--></td><td>SWITCH 1:CARRE MAGIQUE, 2:CLASSIC COEFF
!>    </td></tr>
!>          <tr><td>T
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X,Y,Z
!></td><td>--></td><td>COORDONNEES DES POINTS DANS L'ELEMENT
!>    </td></tr>
!>          <tr><td>XM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>FACTEUR MULTIPLICATIF
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MT02TT
     &( T,XM,XMUL,SF,SG,SH,F,G,H,
     &  X,Y,Z,IKLE,NELEM,NELMAX,INCHYD)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A11,A12        |<--| ELEMENTS DE LA MATRICE
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LE CALCUL DE LA
C|                |   | MATRICE.
C| IKLE           |---| 
C| IKLE1          |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| INCHYD         |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SF,SG,SH       |-->| STRUCTURES DE F,G ET H.
C| SW             |-->| SWITCH 1:CARRE MAGIQUE, 2:CLASSIC COEFF
C| T             |---| 
C| X,Y,Z          |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XM             |---| 
C| XMUL           |-->| FACTEUR MULTIPLICATIF
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_MT02TT => MT02TT
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE(NELMAX,4)
C
      DOUBLE PRECISION, INTENT(INOUT) :: T(NELMAX,4),XM(NELMAX,6)
C
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: F(*),G(*),H(*)
C
C     STRUCTURES OF F, G, H
C
      TYPE(BIEF_OBJ), INTENT(IN) :: SF,SG,SH
C
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),Z(*)
C
      LOGICAL, INTENT(IN) :: INCHYD
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     SPECIFIC DECLARATIONS
C
      DOUBLE PRECISION X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4
      INTEGER I1,I2,I3,I4,IELEM,ISO
C
      DOUBLE PRECISION COEF,SUR24
      DOUBLE PRECISION FTOT,GTOT,HTOT,VTOT,WTOT,FGTOT,GHTOT,FHTOT
      DOUBLE PRECISION T1,T3,T5,T7,T9,T11,T13,T15,T17,T19,T21,T23
      DOUBLE PRECISION T35,T49,T28,T42,T51,T54
      DOUBLE PRECISION AUX,AUXX,AUXXX
      DOUBLE PRECISION AUX1,AUX2,AUX3,AUX4,AUX5,AUX6,AUX7,AUX8,AUX9
C
C***********************************************************************
C
      SUR24=1.D0/24.D0
      ISO = SF%DIM2
C
      IF((SF%ELM.EQ.31.AND.SG%ELM.EQ.31.AND.SH%ELM.EQ.31).OR.
     &   (SF%ELM.EQ.51.AND.SG%ELM.EQ.51.AND.SH%ELM.EQ.51).AND.
     &    ISO.EQ.1 ) THEN
C
C-----------------------------------------------------------------------
C
C   LINEAR DISCRETISATION OF DIFFUSION COEFFICIENTS
C
C   LOOP ON THE TETRAHEDRONS
C
      DO 20 IELEM=1,NELEM
C
      I1=IKLE(IELEM,1)
      I2=IKLE(IELEM,2)
      I3=IKLE(IELEM,3)
      I4=IKLE(IELEM,4)
C
C-----------------------------------------------------------------------
C
C VISCOSITY ALONG X Y AND Z
C
      HTOT=F(I1)+F(I2)+F(I3)+F(I4)
      VTOT=G(I1)+G(I2)+G(I3)+G(I4)
      WTOT=H(I1)+H(I2)+H(I3)+H(I4)
C
      X2=X(I2)-X(I1)
      Y2=Y(I2)-Y(I1)
      Z2=Z(I2)-Z(I1)
      X3=X(I3)-X(I1)
      Y3=Y(I3)-Y(I1)
      Z3=Z(I3)-Z(I1)
      X4=X(I4)-X(I1)
      Y4=Y(I4)-Y(I1)
      Z4=Z(I4)-Z(I1)
C
C-----------------------------------------------------------------------
C    COEF:  THANKS MAPLE...
C-----------------------------------------------------------------------
C
      T1  = X2*Y3
      T3  = X2*Y4
      T5  = X3*Y2
      T7  = X4*Y2
      T9  = X3*Z2
      T11 = X4*Z2
C     T13 = 4 TIMES THE TETRAHEDRON VOLUME ?
      T13 = T1*Z4-T3*Z3-T5*Z4+T7*Z3+T9*Y4-T11*Y3
C
      T15 = -Y2*Z3+Y3*Z2
      T17 =  X2*Z4-T11
      T19 = -Y3*Z4+Y4*Z3
      T21 =  X2*Z3-T9
      T23 = -Y2*Z4+Y4*Z2
      T35 =  X3*Z4-X4*Z3
      T49 =  X3*Y4-X4*Y3
C
C     BEWARE, PROBLEM WITH TIDAL FLATS HERE
C
C      COEF=XMUL*SUR24/MAX(T13,0.01D0*SURF)
C      COEF=XMUL*SUR24/MAX(T13,1.D-3)
      COEF=XMUL*SUR24/MAX(T13,1.D-10)
C     COEF=XMUL*SUR24/T13
C
      T28 = -T19+T23-T15
      T42 = -T35+T17-T21
      T51 = T3-T7
      T54 = T49-T3+T7+T1-T5
C
      T(IELEM,1) =COEF*(HTOT*T28**2+VTOT*T42**2+WTOT*T54**2)
      T(IELEM,2) =COEF*(HTOT*T19**2+VTOT*T35**2+WTOT*T49**2)
      T(IELEM,3) =COEF*(HTOT*T23**2+VTOT*T17**2+WTOT*T51**2)
      XM(IELEM,1)=COEF*(HTOT*T28*T19+VTOT*T42*T35-WTOT*T54*T49)
      XM(IELEM,2)=COEF*(-HTOT*T28*T23-VTOT*T42*T17+WTOT*T54*T51)
      XM(IELEM,3)=-(XM(IELEM,2)+XM(IELEM,1)+T(IELEM,1))
      XM(IELEM,4)=COEF*(-HTOT*T19*T23-VTOT*T35*T17-WTOT*T49*T51)
      XM(IELEM,5)= -(XM(IELEM,4)+T(IELEM,2)+XM(IELEM,1))
      XM(IELEM,6)= -(T(IELEM,3)+XM(IELEM,4)+XM(IELEM,2))
      T(IELEM,4) = -(XM(IELEM,3)+XM(IELEM,5)+XM(IELEM,6))
C
C-----------------------------------------------------------------------
C
20    CONTINUE
C
      ELSEIF(SF%ELM.EQ.30.AND.SG%ELM.EQ.30.AND.SH%ELM.EQ.30.AND.
     &       ISO.EQ.1) THEN
C
C
C-----------------------------------------------------------------------
C
C   P0 DISCRETISATION OF DIFFUSION COEFFICIENTS (CONSTANT ON AN ELEMENT)
C
C   LOOP ON THE PRISMS
C
      DO 21 IELEM=1,NELEM
C
      I1=IKLE(IELEM,1)
      I2=IKLE(IELEM,2)
      I3=IKLE(IELEM,3)
      I4=IKLE(IELEM,4)
C
C     ONLY DIFFERENCE WITH LOOP 20
      HTOT=4*F(IELEM)
      VTOT=4*G(IELEM)
      WTOT=4*H(IELEM)
C     END OF ONLY DIFFERENCE WITH LOOP 20
C
C-----------------------------------------------------------------------
C
      X2=X(I2)-X(I1)
      Y2=Y(I2)-Y(I1)
      Z2=Z(I2)-Z(I1)
      X3=X(I3)-X(I1)
      Y3=Y(I3)-Y(I1)
      Z3=Z(I3)-Z(I1)
      X4=X(I4)-X(I1)
      Y4=Y(I4)-Y(I1)
      Z4=Z(I4)-Z(I1)
C
C-----------------------------------------------------------------------
C    COEF:  THANKS MAPLE...
C-----------------------------------------------------------------------
C
      T1  = X2*Y3
      T3  = X2*Y4
      T5  = X3*Y2
      T7  = X4*Y2
      T9  = X3*Z2
      T11 = X4*Z2
C     T13 = 4 TIMES THE TETRAHEDRON VOLUME ?
      T13 = T1*Z4-T3*Z3-T5*Z4+T7*Z3+T9*Y4-T11*Y3
C
      T15 = -Y2*Z3+Y3*Z2
      T17 =  X2*Z4-T11
      T19 = -Y3*Z4+Y4*Z3
      T21 =  X2*Z3-T9
      T23 = -Y2*Z4+Y4*Z2
      T35 =  X3*Z4-X4*Z3
      T49 =  X3*Y4-X4*Y3
C
C     IF WIDTH MORE THAN 0.01 M
C
C     BEWARE, PROBLEM WITH TIDAL FLAT HERE
C
C      COEF=XMUL*SUR24/MAX(T13,0.01D0*SURF)
C      COEF=XMUL*SUR24/MAX(T13,1.D-3)
      COEF=XMUL*SUR24/MAX(T13,1.D-10)
C     COEF=XMUL*SUR24/T13
C
      T28 = -T19+T23-T15
      T42 = -T35+T17-T21
      T51 = T3-T7
      T54 = T49-T3+T7+T1-T5
C
      T(IELEM,1) =COEF*(HTOT*T28**2+VTOT*T42**2+WTOT*T54**2)
      T(IELEM,2) =COEF*(HTOT*T19**2+VTOT*T35**2+WTOT*T49**2)
      T(IELEM,3) =COEF*(HTOT*T23**2+VTOT*T17**2+WTOT*T51**2)
      XM(IELEM,1)=COEF*(HTOT*T28*T19+VTOT*T42*T35-WTOT*T54*T49)
      XM(IELEM,2)=COEF*(-HTOT*T28*T23-VTOT*T42*T17+WTOT*T54*T51)
      XM(IELEM,3)=-(XM(IELEM,2)+XM(IELEM,1)+T(IELEM,1))
      XM(IELEM,4)=COEF*(-HTOT*T19*T23-VTOT*T35*T17-WTOT*T49*T51)
      XM(IELEM,5)= -(XM(IELEM,4)+T(IELEM,2)+XM(IELEM,1))
      XM(IELEM,6)= -(T(IELEM,3)+XM(IELEM,4)+XM(IELEM,2))
      T(IELEM,4) = -(XM(IELEM,3)+XM(IELEM,5)+XM(IELEM,6))
C
C---------------------------------------------------------------
C
21    CONTINUE
C
      ELSEIF(SF%ELM.EQ.30.AND.ISO.EQ.6) THEN

C   P0 DISCRETISATION OF DIFFUSION COEFFICIENTS (CONSTANT ON AN ELEMENT)
C   NONISOTROPIC VISCOSITY ==> 6 COMPONENTS
C   THE FUNCTION DIFFUSION COEFFICIENT IS HERE A P0 NON DIAGONAL
C   SYMMETRICAL TENSOR
C
C                / DXX DXY DXZ \   / F FG FH \
C            D = | DYX DYY DYZ | = | X  G GH |
C                \ DZX DZY DZZ /   \ X XX  H /
C-----------------------------------------------------------------------
C
C   LOOP ON THE TETRAHEDRONS
C
      DO 22 IELEM=1,NELEM
C
      I1=IKLE(IELEM,1)
      I2=IKLE(IELEM,2)
      I3=IKLE(IELEM,3)
      I4=IKLE(IELEM,4)
C
      FTOT = 4.D0*F(IELEM          )
      GTOT = 4.D0*F(IELEM +   NELEM)
      HTOT = 4.D0*F(IELEM + 2*NELEM)
      GHTOT= 4.D0*F(IELEM + 3*NELEM)
      FHTOT= 4.D0*F(IELEM + 4*NELEM)
      FGTOT= 4.D0*F(IELEM + 5*NELEM)
C
C-----------------------------------------------------------------------
C
      X2=X(I2)-X(I1)
      Y2=Y(I2)-Y(I1)
      Z2=Z(I2)-Z(I1)
      X3=X(I3)-X(I1)
      Y3=Y(I3)-Y(I1)
      Z3=Z(I3)-Z(I1)
      X4=X(I4)-X(I1)
      Y4=Y(I4)-Y(I1)
      Z4=Z(I4)-Z(I1)
C
C-----------------------------------------------------------------------
C    COEF:  THANKS MAPLE...
C-----------------------------------------------------------------------
C
      T13 = X2*Y3*Z4-X2*Y4*Z3-Y2*X3*Z4+Y2*X4*Z3+Z2*X3*Y4-Z2*X4*Y3
C
      AUX   = Y3*Z4-Y4*Z3-Y2*Z4+Z2*Y4+Y2*Z3-Z2*Y3
      AUXX  = X3*Z4-X4*Z3-X2*Z4+Z2*X4+X2*Z3-Z2*X3
      AUXXX = X3*Y4-X4*Y3-X2*Y4+Y2*X4+X2*Y3-Y2*X3
      AUX1 = FTOT*(Y3*Z4-Y4*Z3-Y2*Z4+Z2*Y4+Y2*Z3-Z2*Y3)
      AUX2 = FGTOT*(-X3*Z4+X4*Z3+X2*Z4-Z2*X4-X2*Z3+Z2*X3)
      AUX3 = FHTOT*(X3*Y4-X4*Y3-X2*Y4+Y2*X4+X2*Y3-Y2*X3)
      AUX4 = GTOT*(-X3*Z4+X4*Z3+X2*Z4-Z2*X4-X2*Z3+Z2*X3)
      AUX5 = GHTOT*(X3*Y4-X4*Y3-X2*Y4+Y2*X4+X2*Y3-Y2*X3)
      AUX6 = FGTOT*(Y3*Z4-Y4*Z3-Y2*Z4+Z2*Y4+Y2*Z3-Z2*Y3)
      AUX7 = HTOT*(X3*Y4-X4*Y3-X2*Y4+Y2*X4+X2*Y3-Y2*X3)
      AUX8 = FHTOT*(Y3*Z4-Y4*Z3-Y2*Z4+Z2*Y4+Y2*Z3-Z2*Y3)
      AUX9 = GHTOT*(-X3*Z4+X4*Z3+X2*Z4-Z2*X4-X2*Z3+Z2*X3)
C
      COEF=XMUL*SUR24/MAX(T13,1.D-14)
C
      T(IELEM,1) =COEF*(AUX*(AUX1+AUX2+AUX3)
     & -AUXX*(AUX4+AUX5+AUX6)+AUXXX*(AUX7+AUX8+AUX9))
      T(IELEM,2) =COEF*((Y3*Z4-Y4*Z3)*
     & (FTOT*(Y3*Z4-Y4*Z3)+FGTOT*(-X3*Z4+X4*Z3)+FHTOT*(X3*Y4-X4*Y3))
     & -(X3*Z4-X4*Z3)*(GTOT*(-X3*Z4+X4*Z3)+FGTOT*(Y3*Z4-Y4*Z3)
     & +GHTOT*(X3*Y4-X4*Y3))+(X3*Y4-X4*Y3)*(HTOT*(X3*Y4-X4*Y3)
     & +FHTOT*(Y3*Z4-Y4*Z3)+GHTOT*(-X3*Z4+X4*Z3)))
      T(IELEM,3) =COEF*((Y2*Z4-Z2*Y4)*(FTOT*(Y2*Z4-Z2*Y4)+
     & FGTOT*(-X2*Z4+Z2*X4)+FHTOT*(X2*Y4-Y2*X4))-
     & (X2*Z4-Z2*X4)*(GTOT*(-X2*Z4+Z2*X4)+FGTOT*(Y2*Z4-Z2*Y4)+
     & GHTOT*(X2*Y4-Y2*X4))+(X2*Y4-Y2*X4)*(HTOT*(X2*Y4-Y2*X4)+
     & FHTOT*(Y2*Z4-Z2*Y4)+GHTOT*(-X2*Z4+Z2*X4)))
      T(IELEM,4) =COEF*((Y2*Z3-Z2*Y3)*(FTOT*(Y2*Z3-Z2*Y3)+
     & FGTOT*(-X2*Z3+Z2*X3)+FHTOT*(X2*Y3-Y2*X3))-
     & (X2*Z3-Z2*X3)*(GTOT*(-X2*Z3+Z2*X3)+FGTOT*(Y2*Z3-Z2*Y3)+
     & GHTOT*(X2*Y3-Y2*X3))+(X2*Y3-Y2*X3)*(HTOT*(X2*Y3-Y2*X3)+
     & FHTOT*(Y2*Z3-Z2*Y3)+GHTOT*(-X2*Z3+Z2*X3)))
      XM(IELEM,1)=COEF*((X3*Z4-X4*Z3)*(AUX4+AUX5+AUX6)-
     & (Y3*Z4-Y4*Z3)*(AUX1+AUX2+AUX3)-(X3*Y4-X4*Y3)*(AUX7+AUX8+AUX9))
      XM(IELEM,2)=COEF*((Y2*Z4-Z2*Y4)*(AUX1+AUX2+AUX3)-
     & (X2*Z4-Z2*X4)*(AUX4+AUX5+AUX6)+(X2*Y4-Y2*X4)*(AUX7+AUX8+AUX9))
      XM(IELEM,3)=COEF*((X2*Z3-Z2*X3)*(AUX4+AUX5+AUX6)-
     & (Y2*Z3-Z2*Y3)*(AUX1+AUX2+AUX3)-(X2*Y3-Y2*X3)*(AUX7+AUX8+AUX9))
      XM(IELEM,4)=COEF*((X2*Z4-Z2*X4)*(GTOT*(-X3*Z4+X4*Z3)+
     & FGTOT*(Y3*Z4-Y4*Z3)+GHTOT*(X3*Y4-X4*Y3))-
     & (Y2*Z4-Z2*Y4)*(FTOT*(Y3*Z4-Y4*Z3)+FGTOT*(-X3*Z4+X4*Z3)+
     & FHTOT*(X3*Y4-X4*Y3))-(X2*Y4-Y2*X4)*(HTOT*(X3*Y4-X4*Y3)+
     & FHTOT*(Y3*Z4-Y4*Z3)+GHTOT*(-X3*Z4+X4*Z3)))
      XM(IELEM,5)=COEF*((Y2*Z3-Z2*Y3)*(FTOT*(Y3*Z4-Y4*Z3)+
     & FGTOT*(-X3*Z4+X4*Z3)+FHTOT*(X3*Y4-X4*Y3))-
     & (X2*Z3-Z2*X3)*(GTOT*(-X3*Z4+X4*Z3)+FGTOT*(Y3*Z4-Y4*Z3)
     & +GHTOT*(X3*Y4-X4*Y3))+(X2*Y3-Y2*X3)*(HTOT*(X3*Y4-X4*Y3)
     & +FHTOT*(Y3*Z4-Y4*Z3)+GHTOT*(-X3*Z4+X4*Z3)))
      XM(IELEM,6)=COEF*((X2*Z3-Z2*X3)*(GTOT*(-X2*Z4+Z2*X4)+
     & FGTOT*(Y2*Z4-Z2*Y4)+GHTOT*(X2*Y4-Y2*X4))-
     & (Y2*Z3-Z2*Y3)*(FTOT*(Y2*Z4-Z2*Y4)+FGTOT*(-X2*Z4+Z2*X4)+
     & FHTOT*(X2*Y4-Y2*X4))-(X2*Y3-Y2*X3)*(HTOT*(X2*Y4-Y2*X4)+
     & FHTOT*(Y2*Z4-Z2*Y4)+GHTOT*(-X2*Z4+Z2*X4)))

22    CONTINUE

      ELSE
C
        IF (LNG.EQ.1) WRITE(LU,1000) SF%ELM,SG%ELM,SH%ELM
        IF (LNG.EQ.2) WRITE(LU,1001) SF%ELM,SG%ELM,SH%ELM
1000    FORMAT(1X,'MT02TT (BIEF) : MAUVAIS TYPE DE F,G OU H : ',
     &  I6,1X,I6,1X,I6)
1001    FORMAT(1X,'MT02TT (BIEF) : WRONG TYPE OF F,G OR H: ',
     &  I6,1X,I6,1X,I6)
        CALL PLANTE(1)
        STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C  TREATMENT OF HYDROSTATIC INCONSISTENCIES
C
C     IF(INCHYD) THEN
C
C     DO 22 IELEM=1,NELEM
C
C        I1=IKLE(IELEM,1)
C        I2=IKLE(IELEM,2)
C        I3=IKLE(IELEM,3)
C        I4=IKLE(IELEM,4)
C
C        IF(MAX(Z(I1),Z(I2),Z(I3)).GT.MIN(Z(I4),Z(I5),Z(I6))) THEN
C
C          T(IELEM,1)  =0.D0
C          T(IELEM,2)  =0.D0
C          T(IELEM,3)  =0.D0
C          T(IELEM,4)  =0.D0
C          XM(IELEM, 1)=0.D0
C          XM(IELEM, 2)=0.D0
C          XM(IELEM, 3)=0.D0
C          XM(IELEM, 4)=0.D0
C          XM(IELEM, 5)=0.D0
C          XM(IELEM, 6)=0.D0
C
C        ENDIF
C
C22    CONTINUE
C
C     ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C