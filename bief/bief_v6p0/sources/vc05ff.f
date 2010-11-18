C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!>  @code
!>                    /         ->
!>    VEC(I) = XMUL  /    (U,V).N  PSI(I)  D(GAMMA)
!>                  /GAMMA<br>
!>    PSI(I) IS A BASE OF TYPE P1 QUADRILATERAL
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
!>    </th><td> IKLE1, IKLE2, IKLE3, IKLE4, NBOR, NELEM, NELMAX, SU, SV, U, V, W1, W2, W3, W4, X, XMUL, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AX, AY, H1, H2, HT, I1, I2, I3, I4, IELEM, IELMU, IELMV, N1, N2, N3, N4, U1, U2, U3, U4, V1, V2, V3, V4, XSUR72
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_VC05FF
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
!>      <td><center> 6.0                                       </center>
!> </td><td> 24/07/2009
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
!>          <tr><td>NBOR
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
!>          <tr><td>W1,2,3,4
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
                        SUBROUTINE VC05FF
     &( XMUL,SU,SV,U,V,X,Y,Z,
     &  IKLE1,IKLE2,IKLE3,IKLE4,NBOR,NELEM,NELMAX,W1,W2,W3,W4)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LA FORMULE.
C| IKLE1,         |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
C| IKLE2          |---| 
C| IKLE3          |---| 
C| IKLE4          |---| 
C| NBOR           |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE.
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE.
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SF,SG,SH       |-->| STRUCTURES DES FONCTIONS F,G ET H
C| SU,SV,SW       |-->| STRUCTURES DES FONCTIONS U,V ET W
C| SURFAC         |-->| SURFACE DES ELEMENTS.
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR
C|                |   | INTERVENANT DANS LA FORMULE.
C| W1,2,3,4       |<--| VECTEUR RESULTAT SOUS FORME NON ASSEMBLEE.
C| W2             |---| 
C| W3             |---| 
C| W4             |---| 
C| X,Y,Z          |-->| COORDONNEES DES POINTS
C| XMUL           |-->| COEFFICIENT MULTIPLICATEUR.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_VC05FF => VC05FF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: NBOR(*)
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX)
      INTEGER, INTENT(IN) :: IKLE3(NELMAX),IKLE4(NELMAX)
C
      DOUBLE PRECISION, INTENT(IN)    :: X(*),Y(*),Z(*)
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX),W2(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELMAX),W4(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
C
C     STRUCTURES OF U, V AND REAL DATA
C
      TYPE(BIEF_OBJ), INTENT(IN)   :: SU,SV
      DOUBLE PRECISION, INTENT(IN) :: U(*),V(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELMU,IELMV,IELEM,N1,N2,N3,N4,I1,I2,I3,I4
C
      DOUBLE PRECISION XSUR72,H1,H2,HT,AX,AY
      DOUBLE PRECISION U1,U2,U3,U4,V1,V2,V3,V4
C
C-----------------------------------------------------------------------
C
      IELMU=SU%ELM
      IELMV=SV%ELM
C
C-----------------------------------------------------------------------
C
      XSUR72 = XMUL/72.D0
C
C     U LINEAR BY PRISMS
C
C-----------------------------------------------------------------------
C
      IF(IELMU.EQ.71.AND.IELMV.EQ.71) THEN
C
C-----------------------------------------------------------------------
C
C        LOOP ON THE BOUNDARY SIDES
C
         DO IELEM = 1,NELEM
C
C           LOCAL NUMBERING OF THE SIDE NODES
C
            I1 = IKLE1(IELEM)
            I2 = IKLE2(IELEM)
            I3 = IKLE3(IELEM)
            I4 = IKLE4(IELEM)
C
C           GLOBAL NUMBERING OF THE SIDE NODES
C
            N1 = NBOR(I1)
            N2 = NBOR(I2)
            N3 = NBOR(I3)
            N4 = NBOR(I4)
C
            H1 = Z(N4) - Z(N1)
            H2 = Z(N3) - Z(N2)
            HT = H1 + H2
            H1 = H1 + H1 + HT
            H2 = H2 + H2 + HT
C
            U1 = U(I1) + U(I1) + U(I4)
            U2 = U(I2) + U(I2) + U(I3)
            U3 = U(I2) + U(I3) + U(I3)
            U4 = U(I1) + U(I4) + U(I4)
C
            AX = (Y(N2)-Y(N1)) * XSUR72
C
            V1 = V(I1) + V(I1) + V(I4)
            V2 = V(I2) + V(I2) + V(I3)
            V3 = V(I2) + V(I3) + V(I3)
            V4 = V(I1) + V(I4) + V(I4)
C
            AY = (X(N1)-X(N2)) * XSUR72
C
            W1(IELEM) = (U1*H1+U2*HT)*AX + (V1*H1+V2*HT)*AY
            W2(IELEM) = (U1*HT+U2*H2)*AX + (V1*HT+V2*H2)*AY
            W3(IELEM) = (U4*HT+U3*H2)*AX + (V4*HT+V3*H2)*AY
            W4(IELEM) = (U4*H1+U3*HT)*AX + (V4*H1+V3*HT)*AY
C
         ENDDO
C
C-----------------------------------------------------------------------
C

C
C-----------------------------------------------------------------------
C
      ELSEIF(IELMU.EQ.41.AND.IELMV.EQ.41) THEN
C
C-----------------------------------------------------------------------
C
C        LOOP ON THE BOUNDARY SIDES
C
         DO IELEM = 1,NELEM
C
C  GLOBAL NUMBERING OF THE SIDE NODES
C
            N1 = NBOR(IKLE1(IELEM))
            N2 = NBOR(IKLE2(IELEM))
            N3 = NBOR(IKLE3(IELEM))
            N4 = NBOR(IKLE4(IELEM))
C
            H1 = Z(N4) - Z(N1)
            H2 = Z(N3) - Z(N2)
            HT = H1 + H2
            H1 = H1 + H1 + HT
            H2 = H2 + H2 + HT
C
            U1 = U(N1) + U(N1) + U(N4)
            U2 = U(N2) + U(N2) + U(N3)
            U3 = U(N2) + U(N3) + U(N3)
            U4 = U(N1) + U(N4) + U(N4)
            AX = (Y(N2)-Y(N1)) * XSUR72
C
            V1 = V(N1) + V(N1) + V(N4)
            V2 = V(N2) + V(N2) + V(N3)
            V3 = V(N2) + V(N3) + V(N3)
            V4 = V(N1) + V(N4) + V(N4)
            AY = (X(N1)-X(N2)) * XSUR72
C
            W1(IELEM) = (U1*H1+U2*HT)*AX + (V1*H1+V2*HT)*AY
            W2(IELEM) = (U1*HT+U2*H2)*AX + (V1*HT+V2*H2)*AY
            W3(IELEM) = (U4*HT+U3*H2)*AX + (V4*HT+V3*H2)*AY
            W4(IELEM) = (U4*H1+U3*HT)*AX + (V4*H1+V3*HT)*AY
C
         ENDDO
C
C-----------------------------------------------------------------------
C
      ELSE
C
C-----------------------------------------------------------------------
C
         IF (LNG.EQ.1) WRITE(LU,100) IELMU,SU%NAME
         IF (LNG.EQ.2) WRITE(LU,101) IELMU,SU%NAME
100      FORMAT(1X,'VC05FF (BIEF) :',/,
     &          1X,'DISCRETISATION DE U NON PREVUE : ',1I6,
     &          1X,'NOM REEL : ',A6)
101      FORMAT(1X,'VC05FF (BIEF) :',/,
     &          1X,'DISCRETIZATION OF U NOT AVAILABLE:',1I6,
     &          1X,'REAL NAME: ',A6)
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