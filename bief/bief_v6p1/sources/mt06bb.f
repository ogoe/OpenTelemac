C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS THE FOLLOWING MATRIX:
!>  @code
!>     SUM(F*PSII*PSIJ)<br>
!>            WITH:  P1 QUASI-BUBBLE
!>                   P2 QUASI-BUBBLE
!>                   F P1 OR QUASI-BUBBLE
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE JACOBIAN MUST BE POSITIVE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A11, A12, A13, A14, A22, A23, A24, A33, A34, A44, F, IKLE1, IKLE2, IKLE3, IKLE4, NELEM, NELMAX, SF, SURFAC, XMUL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> F1, F2, F3, F4, IELEM, IELMF, XMS006, XMS009, XMS018, XMS036, XMS054, XMS090, XMS180, XMS540
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MT06BB
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
!>      <td><center> 5.1                                       </center>
!> </td><td> 10/01/95
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18; C MOULIN (LNH) 30 87 83 81
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
!>          <tr><td>A13
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A14
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A22
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A23
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A24
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A33
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A34
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A44
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F,G,H
!></td><td>--></td><td>FONCTIONS INTERVENANT DANS LE CALCUL DE LA
!>                  MATRICE.
!>    </td></tr>
!>          <tr><td>IKLE1
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
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
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>SF,SG,SH
!></td><td>--></td><td>STRUCTURES DE F,G ET H.
!>    </td></tr>
!>          <tr><td>SU,SV,SW
!></td><td>--></td><td>STRUCTURES DE U,V ET W.
!>    </td></tr>
!>          <tr><td>SURFAC
!></td><td>--></td><td>SURFACE DES TRIANGLES.
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>COMPOSANTES D'UN VECTEUR INTERVENANT DANS LE
!>                  CALCUL DE LA MATRICE.
!>    </td></tr>
!>          <tr><td>XEL,YEL,ZEL
!></td><td>--></td><td>COORDONNEES DES POINTS DANS L'ELEMENT
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>FACTEUR MULTIPLICATIF
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MT06BB
     &( A11 , A12 , A13 , A14 ,
     &        A22 , A23 , A24 ,
     &              A33 , A34 ,
     &                    A44 ,
     &  XMUL,SF,F,SURFAC,IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NELMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A11,A12        |<--| ELEMENTS DE LA MATRICE
C| A13            |---| 
C| A14            |---| 
C| A22            |---| 
C| A23            |---| 
C| A24            |---| 
C| A33            |---| 
C| A34            |---| 
C| A44            |---| 
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LE CALCUL DE LA
C|                |   | MATRICE.
C| IKLE1          |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| IKLE2          |---| 
C| IKLE3          |---| 
C| IKLE4          |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SF,SG,SH       |-->| STRUCTURES DE F,G ET H.
C| SU,SV,SW       |-->| STRUCTURES DE U,V ET W.
C| SURFAC         |-->| SURFACE DES TRIANGLES.
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR INTERVENANT DANS LE
C|                |   | CALCUL DE LA MATRICE.
C| XEL,YEL,ZEL    |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| FACTEUR MULTIPLICATIF
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_MT06BB => MT06BB
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
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*),A14(*)
      DOUBLE PRECISION, INTENT(INOUT) ::        A22(*),A23(*),A24(*)
      DOUBLE PRECISION, INTENT(INOUT) ::               A33(*),A34(*)
      DOUBLE PRECISION, INTENT(INOUT) ::                      A44(*)
C
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: F(*)
C
C     STRUCTURE OF F
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
C
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
C
      DOUBLE PRECISION F1,F2,F3,F4,XMS090,XMS180,XMS540
      DOUBLE PRECISION XMS018,XMS054,XMS006,XMS009,XMS036
      INTEGER IELMF,IELEM
C
C=======================================================================
C
C     EXTRACTS THE TYPE OF ELEMENT FOR F
C
      IELMF = SF%ELM
C
C  CASE WHERE F IS P0
C
      IF(IELMF.EQ.10) THEN
C
      XMS009 = XMUL /  9.D0
      XMS006 = XMUL /  6.D0
      XMS018 = XMUL / 18.D0
      XMS036 = XMUL / 36.D0
C
      DO 1 IELEM = 1 , NELEM
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
         F1  =  F(IELEM) * SURFAC(IELEM)
C
C  DIAGONAL TERMS
C
         A11(IELEM) = F1*XMS009
         A22(IELEM) = F1*XMS009
         A33(IELEM) = F1*XMS009
         A44(IELEM) = F1*XMS006
C
C  EXTRADIAGONAL TERMS
C
         A12(IELEM) = F1*XMS036
         A13(IELEM) = F1*XMS036
         A14(IELEM) = F1*XMS018
         A23(IELEM) = F1*XMS036
         A24(IELEM) = F1*XMS018
         A34(IELEM) = F1*XMS018
C
1     CONTINUE
C
C
C-----------------------------------------------------------------------
C
C  CASE WHERE F IS LINEAR
C
      ELSEIF(IELMF.EQ.11) THEN
C
      XMS054 = XMUL /  54.D0
      XMS018 = XMUL /  18.D0
      XMS540 = XMUL / 540.D0
C
      DO 4 IELEM = 1 , NELEM
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
         F1  =  F(IKLE1(IELEM))
         F2  =  F(IKLE2(IELEM))
         F3  =  F(IKLE3(IELEM))
C
C   INITIALISES THE INTERMEDIATE VARIABLES
C
C
C  DIAGONAL TERMS
C
         A11(IELEM) = (SURFAC(IELEM)*(F3+F2+4*F1))*XMS054
         A22(IELEM) = (SURFAC(IELEM)*(F3+4*F2+F1))*XMS054
         A33(IELEM) = (SURFAC(IELEM)*(4*F3+F2+F1))*XMS054
         A44(IELEM) = (SURFAC(IELEM)*(F3+F2+F1))  *XMS018
C
C  EXTRADIAGONAL TERMS
C
         A12(IELEM) = (SURFAC(IELEM)*(   F3+ 7*F2+ 7*F1))*XMS540
         A13(IELEM) = (SURFAC(IELEM)*( 7*F3+   F2+ 7*F1))*XMS540
         A14(IELEM) = (SURFAC(IELEM)*( 7*F3+ 7*F2+16*F1))*XMS540
         A23(IELEM) = (SURFAC(IELEM)*( 7*F3+ 7*F2+   F1))*XMS540
         A24(IELEM) = (SURFAC(IELEM)*( 7*F3+16*F2+ 7*F1))*XMS540
         A34(IELEM) = (SURFAC(IELEM)*(16*F3+ 7*F2+ 7*F1))*XMS540
C
4     CONTINUE
C
C-----------------------------------------------------------------------
C
      ELSEIF(IELMF.EQ.12) THEN
C
C-----------------------------------------------------------------------
C
C   QUASI-BUBBLE DISCRETISATION OF F:
C
C
      XMS090 = XMUL / 90.D0
      XMS180 = XMUL / 180.D0
C
      DO 5 IELEM = 1 , NELEM
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
         F1  =  F(IKLE1(IELEM))
         F2  =  F(IKLE2(IELEM))
         F3  =  F(IKLE3(IELEM))
         F4  =  F(IKLE4(IELEM))
C
C  DIAGONAL TERMS
C
         A11(IELEM) = (SURFAC(IELEM)*(  F3+2*F4+  F2+6*F1))*XMS090
         A22(IELEM) = (SURFAC(IELEM)*(  F3+2*F4+6*F2+  F1))*XMS090
         A33(IELEM) = (SURFAC(IELEM)*(6*F3+2*F4+  F2+  F1))*XMS090
         A44(IELEM) = (SURFAC(IELEM)*(2*F3+9*F4+2*F2+2*F1))*XMS090
C
C  EXTRADIAGONAL TERMS
C
         A12(IELEM) = (SURFAC(IELEM)*(F4+2*F2+2*F1))*XMS180
         A13(IELEM) = (SURFAC(IELEM)*(2*F3+F4+2*F1))*XMS180
         A14(IELEM) = (SURFAC(IELEM)*(F3+4*F4+F2+4*F1))*XMS180
         A23(IELEM) = (SURFAC(IELEM)*(2*F3+F4+2*F2))*XMS180
         A24(IELEM) = (SURFAC(IELEM)*(F3+4*F4+4*F2+F1))*XMS180
         A34(IELEM) = (SURFAC(IELEM)*(4*F3+4*F4+F2+F1))*XMS180
C
5     CONTINUE
C
C-----------------------------------------------------------------------
C
C   ANOTHER DISCRETISATION
C      ELSEIF(IELMF.EQ.XX) THEN
C
C-----------------------------------------------------------------------
C
      ELSE
C
       IF (LNG.EQ.1) WRITE(LU,100) IELMF,SF%NAME
       IF (LNG.EQ.2) WRITE(LU,101) IELMF,SF%NAME
100    FORMAT(1X,'MT06BB (BIEF) :',/,
     &        1X,'DISCRETISATION DE F NON PREVUE : ',1I6,
     &        1X,'NOM REEL : ',A6)
101    FORMAT(1X,'MT06BB (BIEF) :',/,
     &        1X,'DISCRETIZATION OF F NOT AVAILABLE:',1I6,
     &        1X,'REAL NAME: ',A6)
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