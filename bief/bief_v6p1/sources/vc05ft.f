C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!>  @code
!>                    /         ->
!>    VEC(I) = XMUL  /    (U,V).N  PSI(I) D(GAMMA)
!>                  /GAMMA<br>
!>    PSI(I) IS A BASE OF TYPE P1 TRIANGLE ON A LATERAL VERTICAL
!>    MESH (PRISMS MESH SPLIT IN TETRAHEDRONS)
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE JACOBIAN MUST BE POSITIVE

!>  @warning  THE RESULT IS IN W1,2,3 IN NOT ASSEMBLED FORM

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IKLE1, IKLE2, IKLE3, NBOR, NELEM, NELMAX, SU, SV, U, V, W1, W2, W3, X, XMUL, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I1, I2, I3, IELEM, IELMU, IELMV, N1, N2, N3, U1, U2, U3, V1, V2, V3, X1, X2, X3, XSUR24, Y1, Y2, Y3, Z1, Z2, Z3
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_VC05FT
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
!> </td><td> 05/02/91
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
!>          <tr><td>IKLE1,
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE3
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
                        SUBROUTINE VC05FT
     &( XMUL,SU,SV,U,V,X,Y,Z,
     &  IKLE1,IKLE2,IKLE3,NBOR,NELEM,NELMAX,W1,W2,W3)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LA FORMULE.
C| IKLE1,         |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
C| IKLE2          |---| 
C| IKLE3          |---| 
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
C| X,Y,Z          |-->| COORDONNEES DES POINTS
C| XMUL           |-->| COEFFICIENT MULTIPLICATEUR.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_VC05FT => VC05FT
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NBOR(*),NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
C
      DOUBLE PRECISION, INTENT(IN)   :: X(*),Y(*),Z(*)
      DOUBLE PRECISION, INTENT(INOUT):: W1(NELMAX),W2(NELMAX),W3(NELMAX)
      DOUBLE PRECISION, INTENT(IN)   :: XMUL
C
C-----------------------------------------------------------------------
C
C     STRUCTURES OF U, V AND REAL DATA
C
      TYPE(BIEF_OBJ), INTENT(IN)   :: SU,SV
      DOUBLE PRECISION, INTENT(IN) :: U(*),V(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION U1,U2,U3,V1,V2,V3,X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3
      DOUBLE PRECISION XSUR24
      INTEGER IELMU,IELMV,IELEM,N1,N2,N3,I1,I2,I3
C
C-----------------------------------------------------------------------
C
      IELMU=SU%ELM
      IELMV=SV%ELM
C
C-----------------------------------------------------------------------
C
      XSUR24 = XMUL/24.D0
C
C     U LINEAR BY TETRAHEDRONS
C
C-----------------------------------------------------------------------
C
      IF(IELMU.EQ.61.AND.IELMV.EQ.61) THEN
C
C-----------------------------------------------------------------------
C
C   LOOP ON THE BOUNDARY SIDES
C
         DO IELEM = 1,NELEM
C
C           LOCAL NUMBERING OF THE SIDE NODES
C
            I1 = IKLE1(IELEM)
            I2 = IKLE2(IELEM)
            I3 = IKLE3(IELEM)
C
C           GLOBAL NUMBERING OF THE SIDE NODES
C
            N1 = NBOR(I1)
            N2 = NBOR(I2)
            N3 = NBOR(I3)
C
            U1 = U(I1)
            U2 = U(I2)
            U3 = U(I3)
            V1 = V(I1)
            V2 = V(I2)
            V3 = V(I3)
C
            X1 = X(N1)
            X2 = X(N2)-X1
            X3 = X(N3)-X1
            Y1 = Y(N1)
            Y2 = Y(N2)-Y1
            Y3 = Y(N3)-Y1
            Z1 = Z(N1)
            Z2 = Z(N2)-Z1
            Z3 = Z(N3)-Z1
C
C           COMPUTES THE AREA OF THE TRIANGLE (BY VECTOR PRODUCT)
C
C           NOTE: VECTOR NORMAL TO THE TRIANGLE,
C                 WHICH NORM IS THE SURFACE:
C
C                      0.5  (Y2*Z3-Y3*Z2)
C                      0.5  (X3*Z2-X2*Z3)
C                      0.5  (X2*Y3-X3*Y2)  : THIS TERM =0
C
C           INSPIRED FROM MASVEC ON TRIANGLES :
C
C
            W1(IELEM) = XSUR24* (  (Y2*Z3-Y3*Z2)*(2*U1+  U2+  U3)
     &                            +(X3*Z2-X2*Z3)*(2*V1+  V2+  V3) )
            W2(IELEM) = XSUR24* (  (Y2*Z3-Y3*Z2)*(  U1+2*U2+  U3)
     &                            +(X3*Z2-X2*Z3)*(  V1+2*V2+  V3) )
            W3(IELEM) = XSUR24* (  (Y2*Z3-Y3*Z2)*(  U1+  U2+2*U3)
     &                            +(X3*Z2-X2*Z3)*(  V1+  V2+2*V3) )
C
         ENDDO
C
C-----------------------------------------------------------------------
C
      ELSEIF(IELMU.EQ.51.AND.IELMV.EQ.51) THEN
C
C-----------------------------------------------------------------------
C
C   LOOP ON THE BOUNDARY SIDES
C
         DO IELEM = 1,NELEM
C
C           GLOBAL NUMBERING OF THE SIDE NODES
C
            N1 = NBOR(IKLE1(IELEM))
            N2 = NBOR(IKLE2(IELEM))
            N3 = NBOR(IKLE3(IELEM))
C
            U1 = U(N1)
            U2 = U(N2)
            U3 = U(N3)
            V1 = V(N1)
            V2 = V(N2)
            V3 = V(N3)
            X1 = X(N1)
            X2 = X(N2)-X1
            X3 = X(N3)-X1
            Y1 = Y(N1)
            Y2 = Y(N2)-Y1
            Y3 = Y(N3)-Y1
            Z1 = Z(N1)
            Z2 = Z(N2)-Z1
            Z3 = Z(N3)-Z1
C
C           COMPUTES THE AREA OF THE TRIANGLE (BY VECTOR PRODUCT)
C
C           NOTE: VECTOR NORMAL TO THE TRIANGLE,
C                 WHICH NORM IS THE SURFACE:
C
C                      0.5  (Y2*Z3-Y3*Z2)
C                      0.5  (X3*Z2-X2*Z3)
C                      0.5  (X2*Y3-X3*Y2)  : THIS TERM =0
C
C           INSPIRED FROM MASVEC ON TRIANGLES :
C
C
            W1(IELEM) = XSUR24* (  (Y2*Z3-Y3*Z2)*(2*U1+  U2+  U3)
     &                            +(X3*Z2-X2*Z3)*(2*V1+  V2+  V3) )
            W2(IELEM) = XSUR24* (  (Y2*Z3-Y3*Z2)*(  U1+2*U2+  U3)
     &                            +(X3*Z2-X2*Z3)*(  V1+2*V2+  V3) )
            W3(IELEM) = XSUR24* (  (Y2*Z3-Y3*Z2)*(  U1+  U2+2*U3)
     &                            +(X3*Z2-X2*Z3)*(  V1+  V2+2*V3) )
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
100      FORMAT(1X,'VC05FT (BIEF) :',/,
     &          1X,'DISCRETISATION DE U NON PREVUE : ',1I6,
     &          1X,'NOM REEL : ',A6)
101      FORMAT(1X,'VC05FT (BIEF) :',/,
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