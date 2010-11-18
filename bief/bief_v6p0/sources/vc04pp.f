C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!>  @code
!>     IF FORMUL ENDS WITH 'HOR'
!>     IN THE TRANSFORMED MESH<br>
!>                 /            D(PSII*)           D(PSII*)
!>     V  = XMUL  /    H * U * -------- +  H * V * --------   D(OMEGA*)
!>      I        /OMEGA*          DX                 DY<br>
!>     BEWARE : TRANSFORMED MESH HERE !!!!<br><br>
!>     IF FORMUL ENDS WITH 'TOT'
!>     IN THE REAL MESH<br>
!>                 /     ->  --->
!>     V  = XMUL  /      U * GRAD(PSI)   D(OMEGA*)
!>      I        /OMEGA<br>
!>     BEWARE : REAL MESH HERE !!!!<br>
!>     PSII IS OF TYPE P1 PRISM
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE JACOBIAN MUST BE POSITIVE

!>  @warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM

!>  @warning  IF SPECAD=.TRUE., THE ADVECTING FIELD IS NOT ONLY
!>                 U AND V BUT U+DM1*GRAD(ZCONV)

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> F, FORMUL, G, H, IKLE1, IKLE2, IKLE3, IKLE4, IKLE5, IKLE6, NELEM, NELMAX, SF, SG, SH, SPECAD, SU, SV, SW, U, V, W, W1, W2, W3, W4, W5, W6, X, XMUL, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DET, G2, G3, GRADGX, GRADGY, H1, H2, H3, HU1, HU2, HUINF, HUSUP, HV1, HV2, HVINF, HVSUP, I1, I2, I3, I4, I5, I6, IELEM, IELEM2, IELMU, IELMV, IELMW, NELEM2, Q1, Q2, Q3, Q4, Q5, Q6, SHT, SUR144, U1, U2, U3, U4, U5, U6, V1, V2, V3, V4, V5, V6, X1, X2, X3, Y1, Y2, Y3, Z2, Z3, Z4, Z5, Z6
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_VC04PP
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> NBPTS(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>MT14PP(), VECTOS()

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
!>      <td><center> 6.0                                       </center>
!> </td><td> 21/06/06
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>F,G,H
!></td><td>--></td><td>FONCTIONS INTERVENANT DANS LA FORMULE.
!>    </td></tr>
!>          <tr><td>FORMUL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE1,
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE5
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE6
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE.
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>SF,SG,SH
!></td><td>--></td><td>STRUCTURES DES FONCTIONS F,G ET H
!>    </td></tr>
!>          <tr><td>SPECAD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SU,SV,SW
!></td><td>--></td><td>STRUCTURES DES FONCTIONS U,V ET W
!>    </td></tr>
!>          <tr><td>SURFAC
!></td><td>--></td><td>SURFACE DES ELEMENTS.
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>COMPOSANTES D'UN VECTEUR
!>                  INTERVENANT DANS LA FORMULE.
!>    </td></tr>
!>          <tr><td>W1,2,3
!></td><td><--</td><td>VECTEUR RESULTAT SOUS FORME NON ASSEMBLEE.
!>    </td></tr>
!>          <tr><td>W2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W5
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W6
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XEL,YEL,
!></td><td>--></td><td>COORDONNEES DES POINTS DANS L'ELEMENT
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>COEFFICIENT MULTIPLICATEUR.
!>    </td></tr>
!>          <tr><td>Y
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Z
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VC04PP
     &( XMUL,SU,SV,SW,U,V,W,SF,SG,SH,F,G,H,X,Y,Z,
     &  IKLE1,IKLE2,IKLE3,IKLE4,IKLE5,IKLE6,NELEM,NELMAX,
     &  W1,W2,W3,W4,W5,W6,SPECAD,FORMUL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LA FORMULE.
C| FORMUL         |---| 
C| IKLE1,         |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
C| IKLE2          |---| 
C| IKLE3          |---| 
C| IKLE4          |---| 
C| IKLE5          |---| 
C| IKLE6          |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE.
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE.
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SF,SG,SH       |-->| STRUCTURES DES FONCTIONS F,G ET H
C| SPECAD         |---| 
C| SU,SV,SW       |-->| STRUCTURES DES FONCTIONS U,V ET W
C| SURFAC         |-->| SURFACE DES ELEMENTS.
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR
C|                |   | INTERVENANT DANS LA FORMULE.
C| W1,2,3         |<--| VECTEUR RESULTAT SOUS FORME NON ASSEMBLEE.
C| W2             |---| 
C| W3             |---| 
C| W4             |---| 
C| W5             |---| 
C| W6             |---| 
C| X             |---| 
C| XEL,YEL,       |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| COEFFICIENT MULTIPLICATEUR.
C| Y             |---| 
C| Z             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_VC04PP => VC04PP
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
      INTEGER, INTENT(IN) :: IKLE4(NELMAX),IKLE5(NELMAX),IKLE6(NELMAX)
C
      DOUBLE PRECISION, INTENT(IN)   ::X(*),Y(*),Z(*)
      DOUBLE PRECISION, INTENT(INOUT)::W1(NELMAX),W2(NELMAX),W3(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT)::W4(NELMAX),W5(NELMAX),W6(NELMAX)
      DOUBLE PRECISION, INTENT(IN)   ::XMUL
C
      LOGICAL, INTENT(IN) :: SPECAD
      CHARACTER(LEN=16), INTENT(IN) :: FORMUL
C
C     STRUCTURES OF U, V AND REAL DATA
C
      TYPE(BIEF_OBJ),   INTENT(IN) :: SU,SV,SW,SF,SG,SH
      DOUBLE PRECISION, INTENT(IN) :: U(*),V(*),W(*),F(*),G(*),H(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION SUR144,X1,X2,X3,Y1,Y2,Y3,H1,H2,H3,SHT
      DOUBLE PRECISION HU1,HU2,HUINF,HUSUP,HV1,HV2,HVINF,HVSUP
      DOUBLE PRECISION U1,U2,U3,U4,U5,U6,V1,V2,V3,V4,V5,V6
      DOUBLE PRECISION Q1,Q2,Q3,Q4,Q5,Q6,Z2,Z3,Z4,Z5,Z6,G2,G3
      DOUBLE PRECISION GRADGX,GRADGY,DET
      INTEGER I1,I2,I3,I4,I5,I6,IELEM2,NELEM2
      INTEGER IELEM,IELMU,IELMV,IELMW
C
C**********************************************************************
C
      SUR144 = XMUL / 144.D0
C
C-----------------------------------------------------------------------
C
      IELMU=SU%ELM
      IELMV=SV%ELM
      IELMW=SW%ELM
      NELEM2 = NBPTS(10)
C
C-----------------------------------------------------------------------
C
C   HORIZONTAL TERMS
C
      IF(FORMUL(14:16).EQ.'HOR') THEN
C
C   LOOP ON THE ELEMENTS
C
      IF(IELMU.EQ.41.AND.IELMV.EQ.41) THEN
C
C-----------------------------------------------------------------------
C
C  U AND V DISCRETISED IN P1 PRISM:
C
      IF(.NOT.SPECAD) THEN
C
C     STANDARD CASE
C
      DO IELEM = 1 , NELEM
C
         I1 = IKLE1(IELEM)
         I2 = IKLE2(IELEM)
         I3 = IKLE3(IELEM)
         I4 = IKLE4(IELEM)
         I5 = IKLE5(IELEM)
         I6 = IKLE6(IELEM)
C
         H1 = Z(I4) - Z(I1)
         H2 = Z(I5) - Z(I2)
         H3 = Z(I6) - Z(I3)
         SHT = H1 + H2 + H3
C
         HU1 = (U(I1)+U(I2)+U(I3))*SHT + H1*U(I1) + H2*U(I2) + H3*U(I3)
         HU2 = (U(I4)+U(I5)+U(I6))*SHT + H1*U(I4) + H2*U(I5) + H3*U(I6)
         HUINF = (HU1+HU1+HU2) * SUR144
         HUSUP = (HU1+HU2+HU2) * SUR144
C
         HV1 = (V(I1)+V(I2)+V(I3))*SHT + H1*V(I1) + H2*V(I2) + H3*V(I3)
         HV2 = (V(I4)+V(I5)+V(I6))*SHT + H1*V(I4) + H2*V(I5) + H3*V(I6)
         HVINF = (HV1+HV1+HV2) * SUR144
         HVSUP = (HV1+HV2+HV2) * SUR144
C
         Y1 = Y(I2) - Y(I3)
         Y2 = Y(I3) - Y(I1)
         Y3 = Y(I1) - Y(I2)
C
         X1 = X(I3) - X(I2)
         X2 = X(I1) - X(I3)
         X3 = X(I2) - X(I1)
C
         W1(IELEM) = Y1*HUINF + X1*HVINF
         W2(IELEM) = Y2*HUINF + X2*HVINF
         W3(IELEM) = Y3*HUINF + X3*HVINF
         W4(IELEM) = Y1*HUSUP + X1*HVSUP
         W5(IELEM) = Y2*HUSUP + X2*HVSUP
         W6(IELEM) = Y3*HUSUP + X3*HVSUP
C
      ENDDO
C
      ELSE
C
C     CASE WITH SPECIFIC ADVECTING FIELD
C
      DO IELEM = 1 , NELEM
C
         I1 = IKLE1(IELEM)
         I2 = IKLE2(IELEM)
         I3 = IKLE3(IELEM)
         I4 = IKLE4(IELEM)
         I5 = IKLE5(IELEM)
         I6 = IKLE6(IELEM)
C
         H1 = Z(I4) - Z(I1)
         H2 = Z(I5) - Z(I2)
         H3 = Z(I6) - Z(I3)
         SHT = H1 + H2 + H3
C
         Y2 = Y(I2) - Y(I1)
         Y3 = Y(I3) - Y(I1)
         X2 = X(I2) - X(I1)
         X3 = X(I3) - X(I1)
         DET= X2*Y3-X3*Y2
         IELEM2 = MOD(IELEM-1,NELEM2) + 1
C        G IS PIECE-WISE LINEAR
C        IT IS ZCONV IN TELEMAC-3D
         G2 = G(IELEM2+NELEM2)-G(IELEM2)
         G3 = G(IELEM2+2*NELEM2)-G(IELEM2)
         GRADGX=(G2*Y3-G3*Y2)/DET
         GRADGY=(X2*G3-X3*G2)/DET
C
         U1=U(I1)+F(I1)*GRADGX
         U2=U(I2)+F(I2)*GRADGX
         U3=U(I3)+F(I3)*GRADGX
         U4=U(I4)+F(I4)*GRADGX
         U5=U(I5)+F(I5)*GRADGX
         U6=U(I6)+F(I6)*GRADGX
         V1=V(I1)+F(I1)*GRADGY
         V2=V(I2)+F(I2)*GRADGY
         V3=V(I3)+F(I3)*GRADGY
         V4=V(I4)+F(I4)*GRADGY
         V5=V(I5)+F(I5)*GRADGY
         V6=V(I6)+F(I6)*GRADGY
C
         HU1 = (U1+U2+U3)*SHT + H1*U1 + H2*U2 + H3*U3
         HU2 = (U4+U5+U6)*SHT + H1*U4 + H2*U5 + H3*U6
         HUINF = (HU1+HU1+HU2) * SUR144
         HUSUP = (HU1+HU2+HU2) * SUR144
C
         HV1 = (V1+V2+V3)*SHT + H1*V1 + H2*V2 + H3*V3
         HV2 = (V4+V5+V6)*SHT + H1*V4 + H2*V5 + H3*V6
         HVINF = (HV1+HV1+HV2) * SUR144
         HVSUP = (HV1+HV2+HV2) * SUR144
C
         Y1 = Y(I2) - Y(I3)
         Y2 = Y(I3) - Y(I1)
         Y3 = Y(I1) - Y(I2)
C
         X1 = X(I3) - X(I2)
         X2 = X(I1) - X(I3)
         X3 = X(I2) - X(I1)
C
         W1(IELEM) = Y1*HUINF + X1*HVINF
         W2(IELEM) = Y2*HUINF + X2*HVINF
         W3(IELEM) = Y3*HUINF + X3*HVINF
         W4(IELEM) = Y1*HUSUP + X1*HVSUP
         W5(IELEM) = Y2*HUSUP + X2*HVSUP
         W6(IELEM) = Y3*HUSUP + X3*HVSUP
C
      ENDDO
C
      ENDIF
C
C     ELSEIF(IELMU.EQ.  ) THEN
C
C-----------------------------------------------------------------------
C
      ELSE
C
C-----------------------------------------------------------------------
C
       IF (LNG.EQ.1) WRITE(LU,101) IELMU,SU%NAME
       IF (LNG.EQ.2) WRITE(LU,102) IELMU,SU%NAME
101    FORMAT(1X,'VC04PP (BIEF) :',/,
     &        1X,'DISCRETISATION DE U ET V : ',1I6,' CAS NON PREVU',/,
     &        1X,'NOM REEL DE U : ',A6)
102    FORMAT(1X,'VC04PP (BIEF) :',/,
     &        1X,'DISCRETISATION OF U ET V : ',1I6,' NOT IMPLEMENTED',/,
     &        1X,'REAL NAME OF U : ',A6)
       CALL PLANTE(1)
       STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(FORMUL(14:16).EQ.'TOT') THEN
C
C     HORIZONTAL TERMS
C
C     LOOP ON THE ELEMENTS
C
C     HERE SIGMAG NOT TAKEN INTO ACCOUNT (IT WOULD BE TO IMPLEMENT IF A
C     COMPATIBILITY WITH 2D CONTINUITY EQUATION REQUIRED)
C
      IF(IELMU.EQ.41.AND.
     &   IELMV.EQ.41.AND.
     &   IELMW.EQ.41) THEN
C
C-----------------------------------------------------------------------
C
C     VELOCITY DISCRETISED IN P1 PRISM:
C
      DO IELEM = 1 , NELEM
C
         I1 = IKLE1(IELEM)
         I2 = IKLE2(IELEM)
         I3 = IKLE3(IELEM)
         I4 = IKLE4(IELEM)
         I5 = IKLE5(IELEM)
         I6 = IKLE6(IELEM)
C
         X2=X(I2)-X(I1)
         X3=X(I3)-X(I1)
         Y2=Y(I2)-Y(I1)
         Y3=Y(I3)-Y(I1)
C
         Z2=Z(I2)-Z(I1)
         Z3=Z(I3)-Z(I1)
         Z4=Z(I4)-Z(I1)
         Z5=Z(I5)-Z(I1)
         Z6=Z(I6)-Z(I1)
C
         U1=U(I1)
         U2=U(I2)
         U3=U(I3)
         U4=U(I4)
         U5=U(I5)
         U6=U(I6)
C
         V1=V(I1)
         V2=V(I2)
         V3=V(I3)
         V4=V(I4)
         V5=V(I5)
         V6=V(I6)
C
         Q1=W(I1)
         Q2=W(I2)
         Q3=W(I3)
         Q4=W(I4)
         Q5=W(I5)
         Q6=W(I6)
!
C        DERIVATIVE IN Z
!
         W1(IELEM) = (-3*Q6-3*Q5-3*Q2-3*Q3-6*Q4-6*Q1)*(Y3*X2-X3*Y2)
         W2(IELEM) = (-3*Q6-6*Q5-6*Q2-3*Q3-3*Q4-3*Q1)*(Y3*X2-X3*Y2)
         W3(IELEM) = (-6*Q6-3*Q1-3*Q4-6*Q3-3*Q2-3*Q5)*(Y3*X2-X3*Y2)
         W4(IELEM) = - W1(IELEM)
         W5(IELEM) = - W2(IELEM)
         W6(IELEM) = - W3(IELEM)
!
C        DERIVATIVE IN X
!
         W1(IELEM) = W1(IELEM) +
     &         (-2*Z2*U1+3*Z6*U3+Z6*U2-4*Z2*U2-Z6*U5+Z5*U4+3*Z4*U5+4*Z5*
     &U2-2*Z2*U5-2*Z3*U5+6*Z4*U4-3*Z6*U4-3*Z3*U6-6*Z3*U1-3*Z3*U4+2*Z5*U3
     &+2*Z5*U5+6*Z4*U1+3*Z4*U2+Z5*U6-2*Z2*U3-4*Z3*U2-Z2*U4-Z2*U6+3*Z4*U3
     &+3*Z4*U6+2*Z5*U1-6*Z3*U3)*Y2+(6*Z2*U1-4*Z6*U3-2*Z6*U6-2*Z6*U2+6*Z2
     &*U2-Z6*U5+3*Z5*U4-3*Z4*U5-3*Z5*U2+3*Z2*U5+Z3*U5-6*Z4*U4-Z6*U4+2*Z3
     &*U6+2*Z3*U1+Z3*U4-Z5*U3-6*Z4*U1-3*Z4*U2+Z5*U6+4*Z2*U3+2*Z3*U2+3*Z2
     &*U4+2*Z2*U6-3*Z4*U3-3*Z4*U6+4*Z3*U3-2*Z6*U1)*Y3
         W2(IELEM) = W2(IELEM) +
     &         (Z4*U3-4*Z3*U2+2*Z4*U2-2*Z6*U4+2*Z4*U6-2*Z3*U3-Z6*U3-2*Z6
     &*U6-2*Z3*U5+2*Z4*U4-2*Z3*U1+Z4*U1-Z3*U6-Z6*U1-2*Z6*U2-4*Z6*U5+4*Z4
     &*U5-Z3*U4)*Y2+(Z4*U3-Z4*U6-2*Z3*U2+6*Z5*U5+4*Z6*U3+2*Z6*U2-2*Z3*U6
     &+3*Z5*U3+3*Z5*U4-Z3*U4+6*Z5*U2+3*Z4*U1+Z6*U4-Z3*U5+2*Z6*U6-4*Z3*U3
     &+2*Z6*U1+3*Z5*U1+3*Z5*U6+Z6*U5-2*Z3*U1-3*Z4*U5)*Y3
         W3(IELEM) = W3(IELEM) +
     &         (-6*Z6*U6-2*Z5*U1-3*Z6*U1-Z5*U6+2*Z2*U5+4*Z2*U2+Z2*U4-2*Z
     &5*U5+2*Z2*U1-2*Z5*U3-3*Z6*U2+3*Z4*U6+Z2*U6-6*Z6*U3-4*Z5*U2+2*Z2*U3
     &-Z5*U4+Z4*U5-Z4*U2-3*Z4*U1-3*Z6*U5-3*Z6*U4)*Y2+(-2*Z4*U3-Z4*U2-4*Z
     &4*U6+2*Z2*U2+2*Z5*U3+4*Z2*U3-2*Z4*U4+2*Z5*U4+2*Z2*U6+Z5*U1+2*Z2*U1
     &-Z4*U1+Z5*U2+Z2*U4+Z2*U5+2*Z5*U5+4*Z5*U6-2*Z4*U5)*Y3
         W4(IELEM) = W4(IELEM) +
     &         (Z3*U2-2*Z2*U6+4*Z5*U5-2*Z2*U2+6*Z6*U6+3*Z6*U1+2*Z6*U2-Z3
     &*U5+2*Z5*U6-2*Z2*U4+2*Z5*U2-3*Z3*U6-Z2*U3+3*Z6*U3-Z2*U1+2*Z5*U4+6*
     &Z6*U4+3*Z3*U1+Z5*U3+4*Z6*U5-4*Z2*U5+Z5*U1)*Y2+(Z3*U2-4*Z5*U6-2*Z6*
     &U3-2*Z5*U3-6*Z5*U5-6*Z5*U4-Z6*U1+4*Z3*U6-Z2*U3+2*Z3*U3-2*Z6*U4+2*Z
     &3*U4-4*Z6*U6+2*Z3*U5+3*Z2*U5-2*Z6*U5+Z2*U6-3*Z5*U2-Z6*U2-3*Z2*U1+Z
     &3*U1-3*Z5*U1)*Y3
         W5(IELEM) = W5(IELEM) +
     &         (2*Z6*U6+Z6*U1-Z4*U1-2*Z4*U6-2*Z4*U4+Z3*U4+2*Z3*U3+2*Z3*U
     &5+4*Z3*U2+Z3*U6-2*Z4*U2+Z6*U3+2*Z6*U4-4*Z4*U5+2*Z6*U2+2*Z3*U1+4*Z6
     &*U5-Z4*U3)*Y2+(2*Z4*U3-Z3*U1-Z3*U2-6*Z2*U2+2*Z6*U3+4*Z4*U6+Z6*U1+3
     &*Z4*U2+6*Z4*U4-4*Z3*U6-3*Z2*U3+3*Z4*U1+2*Z6*U4-2*Z3*U4+6*Z4*U5-2*Z
     &3*U3+Z6*U2-3*Z2*U4-6*Z2*U5+2*Z6*U5-2*Z3*U5+4*Z6*U6-3*Z2*U6-3*Z2*U1
     &)*Y3
         W6(IELEM) = W6(IELEM) +
     &         (3*Z3*U5+2*Z2*U4-2*Z5*U2+3*Z3*U2-Z5*U1-2*Z5*U6+Z2*U1-2*Z4
     &*U2+2*Z2*U2+4*Z2*U5-Z5*U3-6*Z4*U6+2*Z2*U6+3*Z3*U4+6*Z3*U3+6*Z3*U6+
     &Z2*U3-6*Z4*U4+3*Z3*U1-2*Z5*U4-4*Z4*U5-3*Z4*U1-4*Z5*U5-3*Z4*U3)*Y2+
     &(2*Z4*U3-Z5*U1+4*Z4*U6-2*Z2*U2-2*Z5*U3+Z4*U2+Z4*U1-2*Z5*U4-4*Z2*U3
     &-Z5*U2-4*Z5*U6-2*Z5*U5-2*Z2*U6-2*Z2*U1-Z2*U5-Z2*U4+2*Z4*U5+2*Z4*U4
     &)*Y3
!
C        DERIVATIVE IN Y
!
         W1(IELEM) = W1(IELEM) +
     &         (-4*Z5*V2-3*Z4*V5-Z5*V6+Z6*V5+2*Z3*V5+4*Z2*V2+3*Z3*V6+Z2*
     &V4+4*Z3*V2-2*Z5*V3-Z6*V2+Z2*V6+6*Z3*V1-6*Z4*V1-2*Z5*V5+6*Z3*V3+2*Z
     &2*V3-3*Z4*V3-2*Z5*V1+3*Z3*V4-6*Z4*V4-3*Z4*V6+3*Z6*V4-3*Z6*V3-Z5*V4
     &+2*Z2*V1+2*Z2*V5-3*Z4*V2)*X2+(3*Z5*V2+3*Z4*V5-Z5*V6+Z6*V5-Z3*V5-6*
     &Z2*V2-2*Z3*V6-3*Z2*V4-2*Z3*V2+Z5*V3+2*Z6*V2-2*Z2*V6-2*Z3*V1+6*Z4*V
     &1-4*Z3*V3-4*Z2*V3+3*Z4*V3+2*Z6*V6-Z3*V4+6*Z4*V4+3*Z4*V6+Z6*V4+4*Z6
     &*V3-3*Z5*V4-6*Z2*V1+2*Z6*V1-3*Z2*V5+3*Z4*V2)*X3
         W2(IELEM) = W2(IELEM) +
     &         (-2*Z4*V6+2*Z6*V6+Z6*V3+2*Z6*V4-Z4*V3-Z4*V1+2*Z3*V1+Z3*V4
     &-4*Z4*V5+4*Z6*V5+2*Z6*V2+2*Z3*V3+Z3*V6+4*Z3*V2-2*Z4*V4+2*Z3*V5-2*Z
     &4*V2+Z6*V1)*X2+(-3*Z5*V1-6*Z5*V2+2*Z3*V1-3*Z5*V4+2*Z3*V6+4*Z3*V3+Z
     &3*V4+Z3*V5-6*Z5*V5-3*Z4*V1+3*Z4*V5-2*Z6*V2-Z6*V5-Z4*V3-2*Z6*V1-4*Z
     &6*V3-3*Z5*V6-Z6*V4+Z4*V6+2*Z3*V2-3*Z5*V3-2*Z6*V6)*X3
         W3(IELEM) = W3(IELEM) +
     &         (-2*Z2*V5+Z5*V4+6*Z6*V3+6*Z6*V6+3*Z6*V5-2*Z2*V1+3*Z6*V4+2
     &*Z5*V5+Z5*V6+3*Z4*V1-2*Z2*V3-Z4*V5+3*Z6*V1-Z2*V6-3*Z4*V6+2*Z5*V3-4
     &*Z2*V2-Z2*V4+3*Z6*V2+4*Z5*V2+Z4*V2+2*Z5*V1)*X2+(-4*Z5*V6-2*Z2*V2-2
     &*Z2*V1-Z2*V4-Z5*V1+Z4*V1+Z4*V2+2*Z4*V4-2*Z5*V5+4*Z4*V6-Z2*V5-2*Z2*
     &V6+2*Z4*V5-2*Z5*V4-4*Z2*V3-Z5*V2+2*Z4*V3-2*Z5*V3)*X3
         W4(IELEM) = W4(IELEM) +
     &         (-2*Z5*V4-6*Z6*V6-2*Z5*V6+4*Z2*V5-4*Z5*V5-4*Z6*V5+2*Z2*V2
     &+Z2*V3-6*Z6*V4+2*Z2*V6+3*Z3*V6-Z5*V1-2*Z6*V2-Z5*V3-3*Z6*V3+Z2*V1+2
     &*Z2*V4-3*Z6*V1-Z3*V2-2*Z5*V2-3*Z3*V1+Z3*V5)*X2+(3*Z5*V1+2*Z6*V5+3*
     &Z2*V1-Z3*V2-Z3*V1+2*Z6*V4-4*Z3*V6-2*Z3*V3+Z6*V2-2*Z3*V4+2*Z6*V3+6*
     &Z5*V5+Z2*V3-Z2*V6+2*Z5*V3+Z6*V1+4*Z5*V6+6*Z5*V4-2*Z3*V5+3*Z5*V2+4*
     &Z6*V6-3*Z2*V5)*X3
         W5(IELEM) = W5(IELEM) +
     &         (Z4*V3-Z6*V1-2*Z6*V6+4*Z4*V5-2*Z3*V3-Z6*V3+2*Z4*V4-Z3*V4+
     &2*Z4*V6-2*Z6*V4-4*Z6*V5-2*Z3*V1-Z3*V6-4*Z3*V2-2*Z6*V2-2*Z3*V5+2*Z4
     &*V2+Z4*V1)*X2+(-2*Z6*V5+3*Z2*V1-4*Z4*V6+Z3*V1-2*Z4*V3+4*Z3*V6+2*Z3
     &*V3+2*Z3*V4-6*Z4*V5-3*Z4*V1-3*Z4*V2-6*Z4*V4+3*Z2*V4-2*Z6*V3+6*Z2*V
     &2+3*Z2*V3-4*Z6*V6+3*Z2*V6-Z6*V1-Z6*V2+6*Z2*V5-2*Z6*V4+2*Z3*V5+Z3*V
     &2)*X3
         W6(IELEM) = W6(IELEM) +
     &         (6*Z4*V6-2*Z2*V2+2*Z5*V4-4*Z2*V5-6*Z3*V3+2*Z5*V6-Z2*V1+6*
     &Z4*V4+3*Z4*V1+3*Z4*V3+Z5*V1-3*Z3*V1-3*Z3*V4-2*Z2*V6-6*Z3*V6+Z5*V3-
     &Z2*V3-2*Z2*V4+4*Z5*V5-3*Z3*V2+2*Z5*V2-3*Z3*V5+2*Z4*V2+4*Z4*V5)*X2+
     &(2*Z5*V4+2*Z2*V1+Z5*V1-Z4*V2-2*Z4*V4+2*Z5*V5-Z4*V1+4*Z5*V6+2*Z2*V2
     &-2*Z4*V5+2*Z5*V3+Z5*V2+4*Z2*V3+Z2*V5+Z2*V4+2*Z2*V6-4*Z4*V6-2*Z4*V3
     &)*X3
!
         W1(IELEM) = W1(IELEM)*SUR144
         W2(IELEM) = W2(IELEM)*SUR144
         W3(IELEM) = W3(IELEM)*SUR144
         W4(IELEM) = W4(IELEM)*SUR144
         W5(IELEM) = W5(IELEM)*SUR144
         W6(IELEM) = W6(IELEM)*SUR144
!
      ENDDO
C
C-----------------------------------------------------------------------
C
C     ELSEIF(IELMU.EQ.  ) THEN
C
C-----------------------------------------------------------------------
C
      ELSE
C
C-----------------------------------------------------------------------
C
       IF (LNG.EQ.1) WRITE(LU,301) IELMU,SU%NAME
       IF (LNG.EQ.2) WRITE(LU,302) IELMU,SU%NAME
301    FORMAT(1X,'VC04PP (BIEF) :',/,
     &        1X,'DISCRETISATION DE U,V,W : ',1I6,' CAS NON PREVU',/,
     &        1X,'NOM REEL DE U : ',A6)
302    FORMAT(1X,'VC04PP (BIEF) :',/,
     &        1X,'DISCRETISATION OF U,V,W : ',1I6,' NOT IMPLEMENTED',/,
     &        1X,'REAL NAME OF U : ',A6)
       CALL PLANTE(1)
       STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      ELSE
C
C-----------------------------------------------------------------------
C
       IF (LNG.EQ.1) WRITE(LU,201) FORMUL
       IF (LNG.EQ.2) WRITE(LU,202) FORMUL
201    FORMAT(1X,'VC04PP (BIEF) :',/,
     &        1X,'IL MANQUE HOR OU TOT EN FIN DE FORMULE : ',A16)
202    FORMAT(1X,'VC04PP (BIEF) :',/,
     &        1X,'HOR OR TOT LACKING AT THE END OF THE FORMULA : ',A16)
       CALL PLANTE(1)
       STOP
C
C-----------------------------------------------------------------------
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C