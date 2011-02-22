C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!>  @code
!>   (EXAMPLE OF THE X COMPONENT, WHICH CORRESPONDS TO ICOORD=1)<br>
!>                       /            DF
!>    VEC(I)  =  XMUL   /     ( P  *( --  )) D(OMEGA)
!>                     /OMEGA    I    DX<br>
!>    P   IS A LINEAR BASE
!>     I<br>
!>    F IS A VECTOR OF TYPE P1 OR OTHER
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  IMPORTANT : IF F IS OF TYPE P0, THE RESULT IS 0.
!>                     HERE, IF F IS P0, IT REALLY MEANS THAT F IS
!>                     P1, BUT GIVEN BY ELEMENTS.
!>                     THE SIZE OF F SHOULD THEN BE : F(NELMAX,3).

!>  @warning  THE JACOBIAN MUST BE POSITIVE

!>  @warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> F, ICOORD, IKLE1, IKLE2, IKLE3, IKLE4, NELEM, NELMAX, SF, W1, W2, W3, XEL, XMUL, YEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DISCF, F1, F2, F3, F4, IELEM, IELMF, X2, X3, XSUR18, XSUR6, Y2, Y3
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
!>      <td><center> 5.1                                       </center>
!> </td><td> 09/12/94
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
                        SUBROUTINE VC13AA
     &( XMUL,SF,F,XEL,YEL,
     &  IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NELMAX,W1,W2,W3 , ICOORD )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LA FORMULE.
C| ICOORD         |-->| COORDONNEE SUIVANT LAQUELLE ON DERIVE.
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
C| W1,2,3         |<--| VECTEUR RESULTAT SOUS FORME NON ASSEMBLEE.
C| W2             |---| 
C| W3             |---| 
C| XEL,YEL,       |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| COEFFICIENT MULTIPLICATEUR.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF  !, EX_VC13AA => VC13AA
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX,ICOORD
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX)
      INTEGER, INTENT(IN) :: IKLE3(NELMAX),IKLE4(NELMAX)
C
      DOUBLE PRECISION, INTENT(IN)    :: XEL(NELMAX,*),YEL(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) ::W1(NELMAX),W2(NELMAX),W3(NELMAX)
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
      DOUBLE PRECISION XSUR6,XSUR18,F1,F2,F3,F4,X2,X3,Y2,Y3
C
C-----------------------------------------------------------------------
C
      XSUR6 = XMUL / 6.D0
      XSUR18= XMUL /18.D0
C
C-----------------------------------------------------------------------
C
      IELMF=SF%ELM
      DISCF = SF%DIMDISC
C
C-----------------------------------------------------------------------
C
C     F IS LINEAR
C
      IF(IELMF.EQ.11) THEN
C
C  X COORDINATE
C
      IF(ICOORD.EQ.1) THEN
C
      DO 1 IELEM = 1 , NELEM
C
        W1(IELEM) = ( YEL(IELEM,2) *
     &                (F(IKLE1(IELEM))-F(IKLE3(IELEM)))
     &              + YEL(IELEM,3) *
     &                (F(IKLE2(IELEM))-F(IKLE1(IELEM))) ) * XSUR6
        W2(IELEM) = W1(IELEM)
        W3(IELEM) = W1(IELEM)
C
1     CONTINUE
C
      ELSEIF(ICOORD.EQ.2) THEN
C
C  Y COORDINATE
C
      DO 2 IELEM = 1 , NELEM
C
         W1(IELEM) = ( XEL(IELEM,2) *
     &                 (F(IKLE3(IELEM))-F(IKLE1(IELEM)))
     &               + XEL(IELEM,3) *
     &                 (F(IKLE1(IELEM))-F(IKLE2(IELEM))) ) * XSUR6
         W2(IELEM)  =  W1(IELEM)
         W3(IELEM)  =  W1(IELEM)
C
2     CONTINUE
C
      ELSE
C
          IF (LNG.EQ.1) WRITE(LU,200) ICOORD
          IF (LNG.EQ.2) WRITE(LU,201) ICOORD
200       FORMAT(1X,'VC13AA (BIEF) : COMPOSANTE IMPOSSIBLE ',
     &              1I6,' VERIFIER ICOORD')
201       FORMAT(1X,'VC13AA (BIEF) : IMPOSSIBLE COMPONENT ',
     &              1I6,' CHECK ICOORD')
          CALL PLANTE(0)
          STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C     F IS QUASI-BUBBLE
C
      ELSEIF(IELMF.EQ.12) THEN
C
C  X COORDINATE
C
      IF(ICOORD.EQ.1) THEN
C
      DO 5 IELEM = 1 , NELEM
C
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
C
        F1 = F(IKLE1(IELEM))
        F2 = F(IKLE2(IELEM)) - F1
        F3 = F(IKLE3(IELEM)) - F1
        F4 = F(IKLE4(IELEM)) - F1
C
        W1(IELEM)=(Y2*(-3*F4-2*F3+F2)+Y3*(3*F4-F3+2*F2)) * XSUR18
        W2(IELEM)=(-3*Y2*F3+Y3*(-3*F4+F3+4*F2)) * XSUR18
        W3(IELEM)=(Y2*(3*F4-4*F3-F2)+3*Y3*F2) * XSUR18
C
5     CONTINUE
C
      ELSEIF(ICOORD.EQ.2) THEN
C
C  Y COORDINATE
C
      DO 6 IELEM = 1 , NELEM
C
        X2 = XEL(IELEM,2)
        X3 = XEL(IELEM,3)
C
        F1 = F(IKLE1(IELEM))
        F2 = F(IKLE2(IELEM)) - F1
        F3 = F(IKLE3(IELEM)) - F1
        F4 = F(IKLE4(IELEM)) - F1
C
        W1(IELEM)=(X2*(3*F4+2*F3-F2)+X3*(-3*F4+F3-2*F2)) * XSUR18
        W2(IELEM)=(3*X2*F3+X3*(3*F4-F3-4*F2)) * XSUR18
        W3(IELEM)=(X2*(-3*F4+4*F3+F2)-3*X3*F2) * XSUR18
C
6     CONTINUE
C
      ELSE
C
          IF (LNG.EQ.1) WRITE(LU,200) ICOORD
          IF (LNG.EQ.2) WRITE(LU,201) ICOORD
          CALL PLANTE(0)
          STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C     BEWARE: HERE F IS LINEAR BUT DISCONTINUOUS BETWEEN THE ELEMENTS
C
      ELSEIF(IELMF.EQ.10.AND.DISCF.EQ.12) THEN
C
C  X COORDINATE
C
      IF(ICOORD.EQ.1) THEN
C
      DO 7 IELEM = 1 , NELEM
C
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
C
        F1 = F(IELEM)
        F2 = F(IELEM+  NELMAX)-F1
        F3 = F(IELEM+2*NELMAX)-F1
        F4 = F(IELEM+3*NELMAX)-F1
C
        W1(IELEM)=(Y2*(-3*F4-2*F3+F2)+Y3*(3*F4-F3+2*F2)) * XSUR18
        W2(IELEM)=(-3*Y2*F3+Y3*(-3*F4+F3+4*F2)) * XSUR18
        W3(IELEM)=(Y2*(3*F4-4*F3-F2)+3*Y3*F2) * XSUR18
C
7     CONTINUE
C
      ELSEIF(ICOORD.EQ.2) THEN
C
C  Y COORDINATE
C
      DO 8 IELEM = 1 , NELEM
C
        X2 = XEL(IELEM,2)
        X3 = XEL(IELEM,3)
C
        F1 = F(IELEM)
        F2 = F(IELEM+  NELMAX)-F1
        F3 = F(IELEM+2*NELMAX)-F1
        F4 = F(IELEM+3*NELMAX)-F1
C
        W1(IELEM)=(X2*(3*F4+2*F3-F2)+X3*(-3*F4+F3-2*F2)) * XSUR18
        W2(IELEM)=(3*X2*F3+X3*(3*F4-F3-4*F2)) * XSUR18
        W3(IELEM)=(X2*(-3*F4+4*F3+F2)-3*X3*F2) * XSUR18
C
8     CONTINUE
C
      ELSE
C
          IF (LNG.EQ.1) WRITE(LU,200) ICOORD
          IF (LNG.EQ.2) WRITE(LU,200) ICOORD
          CALL PLANTE(0)
          STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(IELMF.EQ.10.AND.DISCF.EQ.11) THEN
C
C  X COORDINATE
C
      IF(ICOORD.EQ.1) THEN
C
      DO 3 IELEM = 1 , NELEM
C
        W1(IELEM) = ( YEL(IELEM,2) * (F(IELEM)-F(IELEM+2*NELMAX))
     &              + YEL(IELEM,3) * (F(IELEM+NELMAX)-F(IELEM)))* XSUR6
        W2(IELEM) = W1(IELEM)
        W3(IELEM) = W1(IELEM)
C
3     CONTINUE
C
      ELSEIF(ICOORD.EQ.2) THEN
C
C  Y COORDINATE
C
      DO 4 IELEM = 1 , NELEM
C
         W1(IELEM) = ( XEL(IELEM,2) * (F(IELEM+2*NELMAX)-F(IELEM))
     &               + XEL(IELEM,3) * (F(IELEM)-F(IELEM+NELMAX)))*XSUR6
         W2(IELEM)  =  W1(IELEM)
         W3(IELEM)  =  W1(IELEM)
C
4     CONTINUE
C
      ELSE
C
          IF (LNG.EQ.1) WRITE(LU,200) ICOORD
          IF (LNG.EQ.2) WRITE(LU,200) ICOORD
          CALL PLANTE(0)
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
       IF (LNG.EQ.1) WRITE(LU,101) IELMF,SF%NAME
       IF (LNG.EQ.2) WRITE(LU,102) IELMF,SF%NAME
101    FORMAT(1X,'VC13AA (BIEF) :',/,
     &        1X,'DISCRETISATION DE F : ',1I6,' CAS NON PREVU',/,
     &        1X,'NOM REEL DE F : ',A6)
102    FORMAT(1X,'VC13AA (BIEF) :',/,
     &        1X,'DISCRETISATION OF F : ',1I6,' NOT IMPLEMENTED',/,
     &        1X,'REAL NAME OF F: ',A6)
       CALL PLANTE(0)
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