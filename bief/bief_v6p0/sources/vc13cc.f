C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!>  @code
!>                    /            --->
!>    VEC(I) = XMUL  /    PSI(I) * GRAD(F)  D(OMEGA)
!>                  /OMEGA<br>
!>    PSI(I) IS A BASE OF P2 TYPE
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  IMPORTANT : IF F IS OF TYPE P0, THE RESULT IS 0.
!>                     HERE, IF F IS P0, IT REALLY MEANS THAT F IS
!>                     P1, BUT GIVEN BY ELEMENTS.
!>                     THE SIZE OF F SHOULD THEN BE : F(NELMAX,6).

!>  @warning  THE JACOBIAN MUST BE POSITIVE

!>  @warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> F, ICOORD, IKLE1, IKLE2, IKLE3, IKLE4, IKLE5, IKLE6, NELEM, NELMAX, SF, W1, W2, W3, W4, W5, W6, XEL, XMUL, YEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DISCF, F1, F2, F3, F4, F5, F6, IELEM, IELMF, X2, X3, XSUR30, XSUR6, Y2, Y3
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
!>      <td><center> 5.9                                       </center>
!> </td><td> 01/07/08
!> </td><td> A FROEHLY (MATMECA) 01 30 87 80 18
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
!>          <tr><td>ICOORD
!></td><td>--></td><td>COORDONNEE SUIVANT LAQUELLE ON DERIVE.
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
                        SUBROUTINE VC13CC
     &( XMUL,SF,F,XEL,YEL,
     &  IKLE1,IKLE2,IKLE3,IKLE4,IKLE5,IKLE6,
     &  NELEM,NELMAX,
     &  W1,W2,W3,W4,W5,W6,ICOORD )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LA FORMULE.
C| ICOORD         |-->| COORDONNEE SUIVANT LAQUELLE ON DERIVE.
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
C| XEL,YEL,       |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| COEFFICIENT MULTIPLICATEUR.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF !, EX_VC13CC => VC13CC
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX,ICOORD
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
      INTEGER, INTENT(IN) :: IKLE4(NELMAX),IKLE5(NELMAX),IKLE6(NELMAX)
C
      DOUBLE PRECISION, INTENT(IN)    :: XEL(NELMAX,*),YEL(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX),W2(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELMAX),W4(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W5(NELMAX),W6(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
C
C     STRUCTURE OF F AND REAL DATA
C
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
      DOUBLE PRECISION, INTENT(IN) :: F(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,IELMF,DISCF
      DOUBLE PRECISION F1,F2,F3,F4,F5,F6
      DOUBLE PRECISION X2,X3,Y2,Y3
      DOUBLE PRECISION XSUR6,XSUR30
C
C-----------------------------------------------------------------------
C
      IELMF=SF%ELM
      DISCF = SF%DIMDISC
      XSUR6 = XMUL / 6.D0
      XSUR30= XMUL / 30.D0
C
C-----------------------------------------------------------------------
C     F OF TYPE P1
C-----------------------------------------------------------------------
C
      IF(IELMF.EQ.11) THEN
C
C================================
C  DERIVATIVE WRT X  =
C================================
C
        IF(ICOORD.EQ.1) THEN
C
        DO 1 IELEM = 1 , NELEM
C
        F1  = F(IKLE1(IELEM))
        F2  = F(IKLE2(IELEM))
        F3  = F(IKLE3(IELEM))
C
        Y2  =  YEL(IELEM,2)
        Y3  =  YEL(IELEM,3)
C
        W1(IELEM) = 0.D0
        W2(IELEM) = 0.D0
        W3(IELEM) = 0.D0
        W4(IELEM) = (Y2*(F1-F3)+Y3*(F2-F1))*XSUR6
        W5(IELEM) = W4(IELEM)
        W6(IELEM) = W4(IELEM)
C
1       CONTINUE
C
C================================
C  DERIVATIVE WRT Y  =
C================================
C
      ELSEIF(ICOORD.EQ.2) THEN
C
        DO 2 IELEM = 1 , NELEM
C
        F1  = F(IKLE1(IELEM))
        F2  = F(IKLE2(IELEM))
        F3  = F(IKLE3(IELEM))
C
        X2  =  XEL(IELEM,2)
        X3  =  XEL(IELEM,3)
C
        W1(IELEM) = 0.D0
        W2(IELEM) = 0.D0
        W3(IELEM) = 0.D0
        W4(IELEM) = (X2*(F3-F1)+X3*(F1-F2))*XSUR6
        W5(IELEM) = W4(IELEM)
        W6(IELEM) = W4(IELEM)
C
2     CONTINUE
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,200) ICOORD
          IF (LNG.EQ.2) WRITE(LU,201) ICOORD
          CALL PLANTE(1)
          STOP
        ENDIF
C
C-----------------------------------------------------------------------
C
C     BEWARE: HERE F IS LINEAR BUT DISCONTINUOUS BETWEEN THE ELEMENTS
C
      ELSEIF(IELMF.EQ.10.AND.DISCF.EQ.11) THEN
C
C  X COORDINATE
C
      IF(ICOORD.EQ.1) THEN
C
      DO 3 IELEM = 1 , NELEM
C
        F1  = F(IELEM)
        F2  = F(IELEM+NELMAX)
        F3  = F(IELEM+2*NELMAX)
C
        Y2  =  YEL(IELEM,2)
        Y3  =  YEL(IELEM,3)
C
        W1(IELEM) = 0.D0
        W2(IELEM) = 0.D0
        W3(IELEM) = 0.D0
        W4(IELEM) = (Y2*(F1-F3)+Y3*(F2-F1))*XSUR6
        W5(IELEM) = W4(IELEM)
        W6(IELEM) = W4(IELEM)
C
3     CONTINUE
C
      ELSEIF(ICOORD.EQ.2) THEN
C
C  Y COORDINATE
C
      DO 4 IELEM = 1 , NELEM
C
        F1  = F(IELEM)
        F2  = F(IELEM+NELMAX)
        F3  = F(IELEM+2*NELMAX)
C
        X2  =  XEL(IELEM,2)
        X3  =  XEL(IELEM,3)
C
        W1(IELEM) = 0.D0
        W2(IELEM) = 0.D0
        W3(IELEM) = 0.D0
        W4(IELEM) = (X2*(F3-F1)+X3*(F1-F2))*XSUR6
        W5(IELEM) = W4(IELEM)
        W6(IELEM) = W4(IELEM)
C
4     CONTINUE
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,200) ICOORD
          IF (LNG.EQ.2) WRITE(LU,201) ICOORD
          CALL PLANTE(1)
          STOP
        ENDIF
C
C-----------------------------------------------------------------------
C     F OF TYPE P2
C-----------------------------------------------------------------------
C
      ELSEIF(IELMF.EQ.13) THEN
C
C================================
C  DERIVATIVE WRT X  =
C================================
C
      IF(ICOORD.EQ.1) THEN
C
        DO 5 IELEM = 1 , NELEM
C
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
C
        F1 = F(IKLE1(IELEM))
        F2 = F(IKLE2(IELEM))
        F3 = F(IKLE3(IELEM))
        F4 = F(IKLE4(IELEM))
        F5 = F(IKLE5(IELEM))
        F6 = F(IKLE6(IELEM))
C
        W1(IELEM) = ((2.D0*F1+F5-F4+F3-3.D0*F6)*Y2
     &            +  (F6-F5+3.D0*F4-F2-2.D0*F1)*Y3) * XSUR30
        W2(IELEM) = ((2.D0*F2-3.D0*F4+F1+F6-F5)*Y3
     &            +  (F3-F1-2.D0*F5+2.D0*F4   )*Y2) * XSUR30
        W3(IELEM) = ((3.D0*F6+F5-F4-F1-2.D0*F3)*Y2
     &            +  (F1-F2+2.D0*F5-2.D0*F6   )*Y3) * XSUR30
        W4(IELEM) = ((-4.D0*F6-8.D0*(F5-F4)+3.D0*F1+F3)*Y2
     &            +  (3.D0*(F2-F1)+4.D0*(F5-F6)       )*Y3) * XSUR30
        W5(IELEM) = ((4.D0*F6-8.D0*(F5-F4)-F1-3.D0*F3 )*Y2
     &            +  (3.D0*F2-4.D0*F4+F1+8.D0*(F5-F6) )*Y3) * XSUR30
        W6(IELEM) = ((4.D0*(F4-F5)+3.D0*(F1-F3)       )*Y2
     &            +  (4.D0*F4-3.D0*F1+8.D0*(F5-F6)-F2 )*Y3) * XSUR30
C
5     CONTINUE
C
C================================
C  DERIVATIVE WRT Y  =
C================================
C
      ELSEIF(ICOORD.EQ.2) THEN
C
        DO 6 IELEM = 1 , NELEM
C
        X2 = XEL(IELEM,2)
        X3 = XEL(IELEM,3)
C
        F1  = F(IKLE1(IELEM))
        F2  = F(IKLE2(IELEM))
        F3  = F(IKLE3(IELEM))
        F4  = F(IKLE4(IELEM))
        F5  = F(IKLE5(IELEM))
        F6  = F(IKLE6(IELEM))
C
        W1(IELEM) = ((F4-F5-2.D0*F1+3.D0*F6-F3 )*X2
     &            +  (-3.D0*F4-F6+2.D0*F1+F2+F5)*X3) * XSUR30
        W2(IELEM) = ((F5-F1-2.D0*F2+3.D0*F4-F6)*X3
     &            +  (-F3+F1-2.D0*(F4-F5)     )*X2 ) * XSUR30
        W3(IELEM) = ((-3.D0*F6+2.D0*F3+F1+F4-F5)*X2
     &            +  (2.D0*(F6-F5)-F1+F2       )*X3) * XSUR30
        W4(IELEM) = ((4.D0*F6-F3-3.D0*F1-8.D0*(F4-F5))*X2
     &            +  (4.D0*(F6-F5)+3.D0*(F1-F2)      )*X3) * XSUR30
        W5(IELEM) = ((3.D0*F3+F1-8.D0*(F4-F5)-4.D0*F6)*X2
     &            +  (8.D0*(F6-F5)+4.D0*F4-F1-3.D0*F2)*X3) * XSUR30
        W6(IELEM) = ((3.D0*(F3-F1)-4.D0*(F4-F5)      )*X2
     &            +  (8.D0*(F6-F5)+3.D0*F1+F2-4.D0*F4)*X3) * XSUR30
C
6     CONTINUE
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,200) ICOORD
          IF (LNG.EQ.2) WRITE(LU,201) ICOORD
          CALL PLANTE(1)
          STOP
        ENDIF
C
C-----------------------------------------------------------------------
C     F IS P2 BUT DISCONTINUOUS BETWEEN THE ELEMENTS
C-----------------------------------------------------------------------
C
      ELSEIF(IELMF.EQ.10.AND.DISCF.EQ.13) THEN
C
C================================
C  DERIVATIVE WRT X  =
C================================
C
      IF(ICOORD.EQ.1) THEN
C
        DO 7 IELEM = 1 , NELEM
C
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
C
        F1  = F(IELEM         )
        F2  = F(IELEM+  NELMAX)
        F3  = F(IELEM+2*NELMAX)
        F4  = F(IELEM+3*NELMAX)
        F5  = F(IELEM+4*NELMAX)
        F6  = F(IELEM+5*NELMAX)
C
        W1(IELEM) = ((2.D0*F1+F5-F4+F3-3.D0*F6)*Y2
     &            +  (F6-F5+3.D0*F4-F2-2.D0*F1)*Y3) * XSUR30
        W2(IELEM) = ((2.D0*F2-3.D0*F4+F1+F6-F5)*Y3
     &            +  (F3-F1-2.D0*F5+2.D0*F4   )*Y2) * XSUR30
        W3(IELEM) = ((3.D0*F6+F5-F4-F1-2.D0*F3)*Y2
     &            +  (F1-F2+2.D0*F5-2.D0*F6   )*Y3) * XSUR30
        W4(IELEM) = ((-4.D0*F6-8.D0*(F5-F4)+3.D0*F1+F3)*Y2
     &            +  (3.D0*(F2-F1)+4.D0*(F5-F6)       )*Y3) * XSUR30
        W5(IELEM) = ((4.D0*F6-8.D0*(F5-F4)-F1-3.D0*F3 )*Y2
     &            +  (3.D0*F2-4.D0*F4+F1+8.D0*(F5-F6) )*Y3) * XSUR30
        W6(IELEM) = ((4.D0*(F4-F5)+3.D0*(F1-F3)       )*Y2
     &            +  (4.D0*F4-3.D0*F1+8.D0*(F5-F6)-F2 )*Y3) * XSUR30
C
7     CONTINUE
C
C================================
C  DERIVATIVE WRT Y  =
C================================
C
      ELSEIF(ICOORD.EQ.2) THEN
C
      DO 8 IELEM = 1 , NELEM
C
        X2 = XEL(IELEM,2)
        X3 = XEL(IELEM,3)
C
        F1  = F(IELEM         )
        F2  = F(IELEM+  NELMAX)
        F3  = F(IELEM+2*NELMAX)
        F4  = F(IELEM+3*NELMAX)
        F5  = F(IELEM+4*NELMAX)
        F6  = F(IELEM+5*NELMAX)
C
        W1(IELEM) = ((F4-F5-2.D0*F1+3.D0*F6-F3 )*X2
     &            +  (-3.D0*F4-F6+2.D0*F1+F2+F5)*X3) * XSUR30
        W2(IELEM) = ((F5-F1-2.D0*F2+3.D0*F4-F6)*X3
     &            +  (-F3+F1-2.D0*(F4-F5)     )*X2 ) * XSUR30
        W3(IELEM) = ((-3.D0*F6+2.D0*F3+F1+F4-F5)*X2
     &            +  (2.D0*(F6-F5)-F1+F2       )*X3) * XSUR30
        W4(IELEM) = ((4.D0*F6-F3-3.D0*F1-8.D0*(F4-F5))*X2
     &            +  (4.D0*(F6-F5)+3.D0*(F1-F2)      )*X3) * XSUR30
        W5(IELEM) = ((3.D0*F3+F1-8.D0*(F4-F5)-4.D0*F6)*X2
     &            +  (8.D0*(F6-F5)+4.D0*F4-F1-3.D0*F2)*X3) * XSUR30
        W6(IELEM) = ((3.D0*(F3-F1)-4.D0*(F4-F5)      )*X2
     &            +  (8.D0*(F6-F5)+3.D0*F1+F2-4.D0*F4)*X3) * XSUR30
C
8     CONTINUE
C
        ELSE
          IF (LNG.EQ.1) WRITE(LU,200) ICOORD
          IF (LNG.EQ.2) WRITE(LU,201) ICOORD
          CALL PLANTE(1)
          STOP
        ENDIF
C
C-----------------------------------------------------------------------
C      OTHERS
C      ELSEIF
C-----------------------------------------------------------------------
C
      ELSE
C
       IF(LNG.EQ.1) WRITE(LU,100) IELMF,SF%NAME
       IF(LNG.EQ.2) WRITE(LU,101) IELMF,SF%NAME
100    FORMAT(1X,'VC13CC (BIEF) :',/,
     &        1X,'DISCRETISATION DE F NON PREVUE : ',1I6,
     &        1X,'NOM REEL : ',A6)
101    FORMAT(1X,'VC13CC (BIEF) :',/,
     &        1X,'DISCRETIZATION OF F NOT AVAILABLE:',1I6,
     &        1X,'REAL NAME: ',A6)
       CALL PLANTE(1)
       STOP
C
      ENDIF
C
200       FORMAT(1X,'VC13CC (BIEF) : COMPOSANTE IMPOSSIBLE ',
     &              1I6,' VERIFIER ICOORD')
201       FORMAT(1X,'VC13CC (BIEF) : IMPOSSIBLE COMPONENT ',
     &              1I6,' CHECK ICOORD')
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C