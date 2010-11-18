C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS.
!>  @code
!>                    /
!>    VEC(I) = XMUL  /    PSI(I) * F -  VOLU2D(I)*F(I)  D(OMEGA)
!>                  /OMEGA<br>
!>    PSI (I) IS A BASE OF TYPE TRIANGLE P1<br>
!>    F IS A DISCRETISATION VECTOR P0, P1 OR DISCONTINUOUS P1
!>    VOLU2D IS THE INTEGRAL OF THE BASES
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM

!>  @warning  THE JACOBIAN MUST BE POSITIVE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> F, IKLE1, IKLE2, IKLE3, NELEM, NELMAX, SF, SURFAC, W, XMUL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> COEF, F1, F2, F3, IELEM, IELMF, XSUR12
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>FILTER_H()

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
!> </td><td> 20/05/2008
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
!>          <tr><td>IKLE1,
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE3
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
!>          <tr><td>W
!></td><td><-></td><td>VECTEUR RESULTAT SOUS FORME NON ASSEMBLEE.
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
                        SUBROUTINE SMOOTHING_FLUX
     &(XMUL,SF,F,SURFAC,IKLE1,IKLE2,IKLE3,NELEM,NELMAX,W)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LA FORMULE.
C| IKLE1,         |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
C| IKLE2          |---| 
C| IKLE3          |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE.
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE.
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SF,SG,SH       |-->| STRUCTURES DES FONCTIONS F,G ET H
C| SU,SV,SW       |-->| STRUCTURES DES FONCTIONS U,V ET W
C| SURFAC         |-->| SURFACE DES ELEMENTS.
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR
C|                |   | INTERVENANT DANS LA FORMULE.
C| W             |<->| VECTEUR RESULTAT SOUS FORME NON ASSEMBLEE.
C| XEL,YEL,       |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| COEFFICIENT MULTIPLICATEUR.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
C
      DOUBLE PRECISION, INTENT(INOUT) :: W(NELMAX,3)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
C
C     STRUCTURE OF F AND REAL DATA
C
      TYPE(BIEF_OBJ), INTENT(IN)   :: SF
      DOUBLE PRECISION, INTENT(IN) :: F(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,IELMF
      DOUBLE PRECISION XSUR12,F1,F2,F3,COEF
C
C-----------------------------------------------------------------------
C
      IELMF=SF%ELM
C
C-----------------------------------------------------------------------
C
C     F IS LINEAR
C
      IF(IELMF.EQ.11) THEN
C
      XSUR12 = XMUL / 12.D0
C
      DO IELEM = 1 , NELEM
C
        F1  = F(IKLE1(IELEM))
        F2  = F(IKLE2(IELEM))
        F3  = F(IKLE3(IELEM))
C
        COEF = XSUR12 * SURFAC(IELEM)
C
        W(IELEM,1) = W(IELEM,1) + COEF * ( F2+F3-2*F1 )
        W(IELEM,2) = W(IELEM,2) + COEF * ( F1+F3-2*F2 )
        W(IELEM,3) = W(IELEM,3) + COEF * ( F1+F2-2*F3 )
C
      ENDDO
C
C-----------------------------------------------------------------------
C
      ELSE
C
C-----------------------------------------------------------------------
C
       IF (LNG.EQ.1) WRITE(LU,100) IELMF,SF%NAME
       IF (LNG.EQ.2) WRITE(LU,101) IELMF,SF%NAME
100    FORMAT(1X,'ESSAI (BIEF) :',/,
     &        1X,'DISCRETISATION DE F NON PREVUE : ',1I6,
     &        1X,'NOM REEL : ',A6)
101    FORMAT(1X,'ESSAI (BIEF) :',/,
     &        1X,'DISCRETIZATION OF F NOT AVAILABLE:',1I6,
     &        1X,'REAL NAME: ',A6)
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