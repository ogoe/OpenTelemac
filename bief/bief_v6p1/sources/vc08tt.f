C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!>  @code
!>                    /                  DF      DF
!>      V  =  XMUL   /       PSII  * ( U --  + V -- )   D(OMEGA)
!>       I          /OMEGA               DX      DY<br>
!>    PSI(I) IS A BASE OF TYPE P1 TETRAHEDRON
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE JACOBIAN MUST BE POSITIVE

!>  @warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM - REAL MESH

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> F, IKLE1, IKLE2, IKLE3, IKLE4, NELEM, NELMAX, SF, SU, SV, SW, U, V, W, W1, W2, W3, W4, X, XMUL, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> F1MF2, F1MF3, F1MF4, HELP1, HELP2, HELP3, I1, I2, I3, I4, IELEM, IELMF, IELMU, IELMV, IELMW, Q1, Q2, Q3, Q4, U1, U1234, U2, U3, U4, V1, V1234, V2, V3, V4, W1234, X2, X3, X4, XSUR120, Y2, Y3, Y4, Z2, Z3, Z4
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_VC08TT
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
!>      <td><center> 5.3                                       </center>
!> </td><td> 22/03/02
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
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
!>          <tr><td>X,Y,Z
!></td><td>--></td><td>COORDONNEES DES POINTS
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>COEFFICIENT MULTIPLICATEUR.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VC08TT
     &( XMUL,SF,SU,SV,SW,F,U,V,W,X,Y,Z,
     &  IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NELMAX,
     &  W1,W2,W3,W4)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LA FORMULE.
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
C| X,Y,Z          |-->| COORDONNEES DES POINTS
C| XMUL           |-->| COEFFICIENT MULTIPLICATEUR.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF  !, EX_VC08TT => VC08TT
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX)
      INTEGER, INTENT(IN) :: IKLE3(NELMAX),IKLE4(NELMAX)
C
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),Z(*),XMUL
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX),W2(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELMAX),W4(NELMAX)
C
C     STRUCTURES OF F, U, V AND REAL DATA
C
      TYPE(BIEF_OBJ),   INTENT(IN) :: SF,SU,SV,SW
      DOUBLE PRECISION, INTENT(IN) :: F(*),U(*),V(*),W(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION X2,X3,X4,Y2,Y3,Y4,Z2,Z3,Z4
      DOUBLE PRECISION U1,U2,U3,U4,V1,V2,V3,V4,Q1,Q2,Q3,Q4
      DOUBLE PRECISION F1MF2,F1MF3,F1MF4,HELP1,HELP2,HELP3
      DOUBLE PRECISION U1234,V1234,W1234,XSUR120
C
      INTEGER I1,I2,I3,I4,IELEM,IELMF,IELMU,IELMV,IELMW
C
C**********************************************************************
C
      XSUR120 = XMUL / 120.D0
C
      IELMF=SF%ELM
      IELMU=SU%ELM
      IELMV=SV%ELM
      IELMW=SW%ELM
C
C-----------------------------------------------------------------------
C
C     FUNCTION F AND VECTOR U ARE LINEAR
C
      IF( (IELMU.EQ.31.AND.IELMV.EQ.31.AND.IELMW.EQ.31.AND.IELMF.EQ.31)
     &    .OR.
     &    (IELMU.EQ.51.AND.IELMV.EQ.51.AND.IELMW.EQ.51.AND.IELMF.EQ.51)
     &  ) THEN
C
C        LOOP ON THE ELEMENTS
C
         DO 2 IELEM = 1,NELEM
C
            I1 = IKLE1(IELEM)
            I2 = IKLE2(IELEM)
            I3 = IKLE3(IELEM)
            I4 = IKLE4(IELEM)
C
            X2  =   X(I2) - X(I1)
            X3  =   X(I3) - X(I1)
            X4  =   X(I4) - X(I1)
            Y2  =   Y(I2) - Y(I1)
            Y3  =   Y(I3) - Y(I1)
            Y4  =   Y(I4) - Y(I1)
            Z2  =   Z(I2) - Z(I1)
            Z3  =   Z(I3) - Z(I1)
            Z4  =   Z(I4) - Z(I1)
C
            U1  =  U(I1)
            U2  =  U(I2)
            U3  =  U(I3)
            U4  =  U(I4)
C
            V1  =  V(I1)
            V2  =  V(I2)
            V3  =  V(I3)
            V4  =  V(I4)
C
            Q1  =  W(I1)
            Q2  =  W(I2)
            Q3  =  W(I3)
            Q4  =  W(I4)
C
            U1234 = U1 + U2 + U3 + U4
            V1234 = V1 + V2 + V3 + V4
            W1234 = Q1 + Q2 + Q3 + Q4
C
            F1MF2  =  F(I1) - F(I2)
            F1MF3  =  F(I1) - F(I3)
            F1MF4  =  F(I1) - F(I4)
C
            HELP1 = (  (Y4*Z3-Y3*Z4)*F1MF2
     &                +(Y2*Z4-Y4*Z2)*F1MF3
     &                +(Y3*Z2-Y2*Z3)*F1MF4  ) * XSUR120
C
            HELP2 = (  (X3*Z4-X4*Z3)*F1MF2
     &                +(X4*Z2-X2*Z4)*F1MF3
     &                +(X2*Z3-X3*Z2)*F1MF4  ) * XSUR120
C
            HELP3 = (  (X4*Y3-X3*Y4)*F1MF2
     &                +(X2*Y4-X4*Y2)*F1MF3
     &                +(X3*Y2-X2*Y3)*F1MF4  ) * XSUR120
C
            W1(IELEM) = ( U1234 + U1 ) * HELP1
     &                + ( V1234 + V1 ) * HELP2
     &                + ( W1234 + Q1 ) * HELP3
            W2(IELEM) = ( U1234 + U2 ) * HELP1
     &                + ( V1234 + V2 ) * HELP2
     &                + ( W1234 + Q2 ) * HELP3
            W3(IELEM) = ( U1234 + U3 ) * HELP1
     &                + ( V1234 + V3 ) * HELP2
     &                + ( W1234 + Q3 ) * HELP3
            W4(IELEM) = ( U1234 + U4 ) * HELP1
     &                + ( V1234 + V4 ) * HELP2
     &                + ( W1234 + Q4 ) * HELP3
C
2        CONTINUE
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
         IF (LNG.EQ.2) WRITE(LU,201) IELMU,SU%NAME
         IF (LNG.EQ.2) WRITE(LU,301)
100      FORMAT(1X,'VC08TT (BIEF) :',/,
     &          1X,'DISCRETISATION DE F : ',1I6,
     &          1X,'NOM REEL : ',A6)
200      FORMAT(1X,'DISCRETISATION DE U : ',1I6,
     &          1X,'NOM REEL : ',A6)
300      FORMAT(1X,'CAS NON PREVU')
101      FORMAT(1X,'VC08TT (BIEF) :',/,
     &          1X,'DISCRETIZATION OF F:',1I6,
     &          1X,'REAL NAME: ',A6)
201      FORMAT(1X,'DISCRETIZATION OF U:',1I6,
     &          1X,'REAL NAME: ',A6)
301      FORMAT(1X,'CASE NOT IMPLEMENTED')
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
