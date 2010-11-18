C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!>  @code
!>                    /                  DF      DF
!>      V  =  XMUL   /       PSII  * ( U --  + V -- )   D(OMEGA)
!>       I          /OMEGA               DX      DY<br>
!>    PSI(I) IS A BASE OF QUASI-BUBBLE TYPE
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE JACOBIAN MUST BE POSITIVE

!>  @warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> F, FORMUL, IKLE1, IKLE2, IKLE3, IKLE4, NELEM, NELMAX, SF, SU, SV, U, V, W1, W2, W3, W4, XEL, XMUL, YEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> BETAN1, BETAN2, BETAN3, F1, F2, F3, F4, IAD1, IAD2, IAD3, IELEM, IELMF, IELMU, IELMV, IG1, IG2, IG3, IL, IT, K1, K2, K3, L12, L13, L21, L23, L31, L32, PHIT, SIG, SUR6, TIERS, U1, U2, U3, U4, USUR2, V1, V2, V3, V4, VSUR2, X1, X2, X3, XSU216, XSUR72, Y1, Y2, Y3
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_VC08BB
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>VECTOS()

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
!>      <td><center> 5.6                                       </center>
!> </td><td> 29/12/05
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18; F LEPEINTRE (LNH) 30 87 78 54
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
!></td><td>--></td><td>VECTEUR RESULTAT SOUS FORME NON ASSEMBLEE.
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
!>          <tr><td>XEL,YEL,
!></td><td>--></td><td>COORDONNEES DES POINTS DANS L'ELEMENT
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>COEFFICIENT MULTIPLICATEUR.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VC08BB
     &( XMUL,SF,SU,SV,F,U,V,XEL,YEL,
     &  IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NELMAX,
     &  W1,W2,W3,W4 , FORMUL )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LA FORMULE.
C| FORMUL         |---| 
C| IKLE1,         |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
C| IKLE2          |---| 
C| IKLE3          |---| 
C| IKLE4          |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE.
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE.
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SF,SG,SH       |-->| STRUCTURES DES FONCTIONS F,G ET H
C| SU,SV,SW       |-->| STRUCTURES DES FONCTIONS U,V ET W
C| SURFAC         |-->| SURFACE DES ELEMENTS.
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR
C|                |   | INTERVENANT DANS LA FORMULE.
C| W1,2,3         |-->| VECTEUR RESULTAT SOUS FORME NON ASSEMBLEE.
C| W2             |---| 
C| W3             |---| 
C| W4             |---| 
C| XEL,YEL,       |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| COEFFICIENT MULTIPLICATEUR.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_VC08BB => VC08BB
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX*3),YEL(NELMAX*3),XMUL
C     W1 IS ALSO USED AS 1-DIMENSIONAL FOR ALL W
      DOUBLE PRECISION, INTENT(INOUT) :: W1(4*NELMAX),W2(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELMAX),W4(NELMAX)
C     IKLE1 IS ALSO USED AS A 1-DIMENSIONAL IKLE
      INTEGER, INTENT(IN) :: IKLE1(4*NELMAX)
      INTEGER, INTENT(IN) :: IKLE2(NELMAX),IKLE3(NELMAX),IKLE4(NELMAX)
      CHARACTER(LEN=16), INTENT(IN) :: FORMUL
C
C     STRUCTURES OF F, G, H, U, V, W AND REAL DATA
C
      TYPE(BIEF_OBJ), INTENT(IN) :: SF,SU,SV
      DOUBLE PRECISION, INTENT(IN) :: F(*),U(*),V(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,IELMF,IELMU,IELMV,IL(3,3)
      INTEGER IG1,IG2,IG3,IT,IAD1,IAD2,IAD3
C
      DOUBLE PRECISION K1,K2,K3
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION X2,Y2,X3,Y3,F1,F2,F3,F4,U1,U2,U3,U4,V1,V2,V3,V4
      DOUBLE PRECISION XSUR72,XSU216,X1,Y1,TIERS
      DOUBLE PRECISION PHIT,SUR6,USUR2,VSUR2
      DOUBLE PRECISION L12,L13,L21,L23,L31,L32,SIG,BETAN1,BETAN2,BETAN3
C
      INTRINSIC MAX,MIN,SIGN
C
C-----------------------------------------------------------------------
C
C     FOR A QUASI-BUBBLE TRIANGLE : NODE NUMBERS OF THE SUB-TRIANGLES
C     IN THE INITIAL TRIANGLE
C     IL(NUMBER OF THE SUB-TRIANGLE,LOCAL NUMBER IN THE SUB-TRIANGLE)
C
      DATA IL /1,2,3,2,3,1,4,4,4/
C
C-----------------------------------------------------------------------
C
      XSUR72 = XMUL/72.D0
      XSU216 = XMUL/216.D0
      TIERS  = 1.D0 / 3.D0
      SUR6   = 1.D0 / 6.D0
C
      IELMF=SF%ELM
      IELMU=SU%ELM
      IELMV=SV%ELM
C
C-----------------------------------------------------------------------
C
C     FUNCTION F AND VECTOR U ARE QUASI-BUBBLE
C
      IF(IELMF.EQ.12.AND.IELMU.EQ.12.AND.IELMV.EQ.12) THEN
C
      IF(FORMUL(14:16).EQ.'PSI') THEN
C
C     INITIALISES W
C
      DO 32 IELEM = 1 , NELEM
        W1(IELEM) = 0.D0
        W2(IELEM) = 0.D0
        W3(IELEM) = 0.D0
        W4(IELEM) = 0.D0
32    CONTINUE
C
C     PSI SCHEME, LOOP ON THE 3 SUB-TRIANGLES AND
C     PREASSEMBLY
C
      DO 10 IT=1,3
CDIR$ IVDEP
      DO 33 IELEM = 1 , NELEM
C
C     ADDRESSES IN ARRAY (NELMAX,*)
      IAD1= IELEM + (IL(IT,1)-1)*NELMAX
      IAD2= IELEM + (IL(IT,2)-1)*NELMAX
      IAD3= IELEM + (IL(IT,3)-1)*NELMAX
C     GLOBAL NUMBERS IN THE INITIAL TRIANGLE
      IG1 = IKLE1(IAD1)
      IG2 = IKLE1(IAD2)
      IG3 = IKLE1(IAD3)
C     COORDINATES OF THE SUB-TRIANGLE NODES
      X1 = XEL(IAD1)
      X2 = XEL(IAD2) - X1
C     POINT 3 IS ALWAYS THE CENTRE OF THE INITIAL TRIANGLE
      X3=TIERS*(XEL(IELEM)+XEL(IELEM+NELMAX)+XEL(IELEM+2*NELMAX))-X1
      Y1 = YEL(IAD1)
      Y2 = YEL(IAD2) - Y1
C     POINT 3 IS ALWAYS THE CENTRE OF THE INITIAL TRIANGLE
      Y3=TIERS*(YEL(IELEM)+YEL(IELEM+NELMAX)+YEL(IELEM+2*NELMAX))-Y1
C     F VALUES IN THE SUB-TRIANGLE
      F1 = F(IG1)
      F2 = F(IG2)
      F3 = F(IG3)
C
      USUR2 = (U(IG1)+U(IG2)+U(IG3))*SUR6
      VSUR2 = (V(IG1)+V(IG2)+V(IG3))*SUR6
C
      K1 = USUR2 * (Y2-Y3) - VSUR2 * (X2-X3)
      K2 = USUR2 * (Y3   ) - VSUR2 * (X3   )
      K3 = USUR2 * (  -Y2) - VSUR2 * (  -X2)
C
      L12 = MAX(  MIN(K1,-K2) , 0.D0 )
      L13 = MAX(  MIN(K1,-K3) , 0.D0 )
      L21 = MAX(  MIN(K2,-K1) , 0.D0 )
      L23 = MAX(  MIN(K2,-K3) , 0.D0 )
      L31 = MAX(  MIN(K3,-K1) , 0.D0 )
      L32 = MAX(  MIN(K3,-K2) , 0.D0 )
C
      BETAN1 = L12*(F1-F2) + L13*(F1-F3)
      BETAN2 = L21*(F2-F1) + L23*(F2-F3)
      BETAN3 = L31*(F3-F1) + L32*(F3-F2)
C
      PHIT = BETAN1 + BETAN2 + BETAN3
      SIG  = SIGN(1.D0,PHIT)
C
      W1(IAD1)=W1(IAD1)+ XMUL * SIG * MAX(MIN(SIG*BETAN1,SIG*PHIT),0.D0)
      W1(IAD2)=W1(IAD2)+ XMUL * SIG * MAX(MIN(SIG*BETAN2,SIG*PHIT),0.D0)
      W1(IAD3)=W1(IAD3)+ XMUL * SIG * MAX(MIN(SIG*BETAN3,SIG*PHIT),0.D0)
C
33    CONTINUE
10    CONTINUE
C
      ELSE
C
C     CLASSICAL COMPUTATION
C
      DO 3 IELEM = 1 , NELEM
C
         X2 = XEL(IELEM+NELMAX)
         X3 = XEL(IELEM+2*NELMAX)
         Y2 = YEL(IELEM+NELMAX)
         Y3 = YEL(IELEM+2*NELMAX)
C
         U1 = U(IKLE1(IELEM))
         U2 = U(IKLE2(IELEM))
         U3 = U(IKLE3(IELEM))
         U4 = U(IKLE4(IELEM))
         V1 = V(IKLE1(IELEM))
         V2 = V(IKLE2(IELEM))
         V3 = V(IKLE3(IELEM))
         V4 = V(IKLE4(IELEM))
C
         F1 = F(IKLE1(IELEM))
         F2 = F(IKLE2(IELEM)) - F1
         F3 = F(IKLE3(IELEM)) - F1
         F4 = F(IKLE4(IELEM)) - F1
C
         W1(IELEM)= (((V3+V4+2*V1)*X2+(V3+V4+2*V1)*X3-(Y3+Y2)*U3-(Y3+
     &     Y2)*U4-2*(Y3+Y2)*U1)*F3-3*((V3+V4+2*V1)*X3-(V4+V2+2*
     &     V1)*X2-(Y3-Y2)*U4-2*(Y3-Y2)*U1-U3*Y3+U2*Y2)*F4-((V4+V2+
     &     2*V1)*X2+(V4+V2+2*V1)*X3-(Y3+Y2)*U4-(Y3+Y2)*U2-2*(Y3+Y2
     &     )*U1)*F2)*XSUR72
C
         W2(IELEM)= (-(((2*V3+3*V4+6*V2+V1)*X3-(V3-V1)*X2-(2*Y3-Y2)
     &     *U3-(Y3+Y2)*U1-3*U4*Y3-6*U2*Y3)*F2-(2*(V3+V4+2*V2)*X2
     &     -(V3+V4+2*V2)*X3+(Y3-2*Y2)*U3+(Y3-2*Y2)*U4+2*(Y3-2*
     &     Y2)*U2)*F3-3*((V3+V4+2*V2)*X3-(V3-V1)*X2-(Y3-Y2)*U3-U4*
     &     Y3-2*U2*Y3-U1*Y2)*F4))*XSUR72
C
         W3(IELEM)= (((6*V3+3*V4+2*V2+V1)*X2-(V2-V1)*X3-(Y3+Y2)*U1+(
     &     Y3-2*Y2)*U2-6*U3*Y2-3*U4*Y2)*F3+((2*V3+V4+V2)*X2-2*(
     &     2*V3+V4+V2)*X3+2*(2*Y3-Y2)*U3+(2*Y3-Y2)*U4+(2*Y3-Y2)*
     &     U2)*F2-3*((2*V3+V4+V2)*X2-(V2-V1)*X3+(Y3-Y2)*U2-2*U3*
     &     Y2-U4*Y2-U1*Y3)*F4)*XSUR72
C
         W4(IELEM)= (((3*V3+6*V4+2*V2+V1)*X2-(V2-V1)*X3-(Y3+Y2)*U1+(
     &     Y3-2*Y2)*U2-3*U3*Y2-6*U4*Y2)*F3-((2*V3+6*V4+3*V2+V1
     &     )*X3-(V3-V1)*X2-(2*Y3-Y2)*U3-(Y3+Y2)*U1-6*U4*Y3-3*U2*
     &     Y3)*F2-3*((V3-V1)*X2-(V2-V1)*X3-(Y3-Y2)*U1-U3*Y2+U2*Y3)*
     &     F4)*XSUR72

C
3     CONTINUE
C
      ENDIF
C
C     FUNCTION F IS QUASI-BUBBLE AND VECTOR U LINEAR
C
      ELSEIF(IELMF.EQ.12.AND.IELMU.EQ.11.AND.IELMV.EQ.11) THEN
C
      IF(FORMUL(14:16).EQ.'PSI') THEN
C
       IF (LNG.EQ.1) WRITE(LU,400)
       IF (LNG.EQ.2) WRITE(LU,401)
400    FORMAT(1X,'VC08BB (BIEF) : PSI NON PROGRAMME EN QUASI-BULLE')
401    FORMAT(1X,'VC08BB (BIEF) : PSI NOT IMPLEMENTED FOR QUASI-BUBBLE')
       CALL PLANTE(1)
       STOP
C
      ELSE
C
C     CLASSICAL COMPUTATION
C
      DO 4 IELEM = 1 , NELEM
C
         X2 = XEL(IELEM+NELMAX)
         X3 = XEL(IELEM+2*NELMAX)
         Y2 = YEL(IELEM+NELMAX)
         Y3 = YEL(IELEM+2*NELMAX)
C
         U1 = U(IKLE1(IELEM))
         U2 = U(IKLE2(IELEM))
         U3 = U(IKLE3(IELEM))
         V1 = V(IKLE1(IELEM))
         V2 = V(IKLE2(IELEM))
         V3 = V(IKLE3(IELEM))
C
         F1 = F(IKLE1(IELEM))
         F2 = F(IKLE2(IELEM)) - F1
         F3 = F(IKLE3(IELEM)) - F1
         F4 = F(IKLE4(IELEM)) - F1
C
         W1(IELEM)=(((4*V3+V2+7*V1)*X2+(4*V3+V2+7*V1)*X3-4*(Y3+Y2
     &    )*U3-(Y3+Y2)*U2-7*(Y3+Y2)*U1)*F3-3*((4*V3+V2+7*V1)*X3
     &    -(V3+4*V2+7*V1)*X2-(4*Y3-Y2)*U3-7*(Y3-Y2)*U1-(Y3-4*
     &    Y2)*U2)*F4-((V3+4*V2+7*V1)*X2+(V3+4*V2+7*V1)*X3-(Y3+
     &    Y2)*U3-4*(Y3+Y2)*U2-7*(Y3+Y2)*U1)*F2)*XSU216
         W2(IELEM)=((2*(4*V3+7*V2+V1)*X2-(4*V3+7*V2+V1)*X3+4*(Y3
     &    -2*Y2)*U3+7*(Y3-2*Y2)*U2+(Y3-2*Y2)*U1)*F3+3*((4*V3+
     &    7*V2+V1)*X3-3*(V3-V1)*X2-(4*Y3-3*Y2)*U3-(Y3+3*Y2)*U1-
     &    7*U2*Y3)*F4-3*((3*V3+7*V2+2*V1)*X3-(V3-V1)*X2-(3*Y3-
     &    Y2)*U3-(2*Y3+Y2)*U1-7*U2*Y3)*F2)*XSU216
         W3(IELEM)=(((7*V3+4*V2+V1)*X2-2*(7*V3+4*V2+V1)*X3+7*(2
     &    *Y3-Y2)*U3+4*(2*Y3-Y2)*U2+(2*Y3-Y2)*U1)*F2-3*((7*V3+
     &    4*V2+V1)*X2-3*(V2-V1)*X3-(3*Y3+Y2)*U1+(3*Y3-4*Y2)*U2-
     &    7*U3*Y2)*F4+3*((7*V3+3*V2+2*V1)*X2-(V2-V1)*X3-(Y3+2*
     &    Y2)*U1+(Y3-3*Y2)*U2-7*U3*Y2)*F3)*XSU216
         W4(IELEM)=(((5*V3+4*V2+3*V1)*X2-(V2-V1)*X3-(Y3+3*Y2)*U1+(
     &    Y3-4*Y2)*U2-5*U3*Y2)*F3-((4*V3+5*V2+3*V1)*X3-(V3-V1)
     &    *X2-(4*Y3-Y2)*U3-(3*Y3+Y2)*U1-5*U2*Y3)*F2-3*((V3-V1)*
     &    X2-(V2-V1)*X3-(Y3-Y2)*U1-U3*Y2+U2*Y3)*F4)*XSUR72
C
4     CONTINUE
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      ELSE
C
C-----------------------------------------------------------------------
C
       IF (LNG.EQ.1) WRITE(LU,100) IELMF,SF%NAME
       IF (LNG.EQ.1) WRITE(LU,200) IELMU,SU%NAME
       IF (LNG.EQ.1) WRITE(LU,300)
       IF (LNG.EQ.2) WRITE(LU,101) IELMF,SF%NAME
       IF (LNG.EQ.1) WRITE(LU,201) IELMU,SU%NAME
       IF (LNG.EQ.1) WRITE(LU,301)
100    FORMAT(1X,'VC08BB (BIEF) :',/,
     &        1X,'DISCRETISATION DE F : ',1I6,
     &        1X,'NOM REEL : ',A6)
200    FORMAT(1X,'DISCRETISATION DE U : ',1I6,
     &        1X,'NOM REEL : ',A6)
300    FORMAT(1X,'CAS NON PREVU')
101    FORMAT(1X,'VC08BB (BIEF) :',/,
     &        1X,'DISCRETIZATION OF F:',1I6,
     &        1X,'REAL NAME: ',A6)
201    FORMAT(1X,'DISCRETIZATION OF U:',1I6,
     &        1X,'REAL NAME: ',A6)
301    FORMAT(1X,'CASE NOT IMPLEMENTED')
       CALL PLANTE(1)
       STOP
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