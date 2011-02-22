C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS THE DIFFUSION TERM FOR ESTEL2D.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE JACOBIAN MUST BE POSITIVE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A11, A12, A13, A22, A23, A33, NELEM, NELMAX, SU, SURFAC, SV, U, V, XEL, XMUL, YEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELEM, IELMNU, IELMNV, ISOU, ISOV, KSAT1, KSAT2, KSAT3, SOM, X2, X3, XSUR12, Y2, Y3
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MT02AA_2
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
!>      <td><center> 5.8                                       </center>
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
!>          <tr><td>A13
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A22
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A23
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A33
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F,G,H
!></td><td>--></td><td>FONCTIONS INTERVENANT DANS LE CALCUL DE LA
!>                  MATRICE.
!>    </td></tr>
!>          <tr><td>IKLE1,2,3
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>SF,SG,SH
!></td><td>--></td><td>STRUCTURES DE F,G,H
!>    </td></tr>
!>          <tr><td>SU,SV,SW
!></td><td>--></td><td>STRUCTURES DE U,V,W
!>    </td></tr>
!>          <tr><td>SURFAC
!></td><td>--></td><td>SURFACE DES TRIANGLES.
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>COMPOSANTES D'UN VECTEUR INTERVENANT DANS LE
!>                  CALCUL DE LA MATRICE.
!>    </td></tr>
!>          <tr><td>XEL,YEL
!></td><td>--></td><td>COORDONNEES DES POINTS DANS L'ELEMENT
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>FACTEUR MULTIPLICATIF
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MT02AA_2
     &( A11 , A12 , A13 ,
     &        A22 , A23 ,
     &              A33 ,
     &  XMUL,SU,SV,U,V,
     &  XEL,YEL,SURFAC,NELEM,NELMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A11,A12        |<--| ELEMENTS DE LA MATRICE
C| A13            |---| 
C| A22            |---| 
C| A23            |---| 
C| A33            |---| 
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LE CALCUL DE LA
C|                |   | MATRICE.
C| IKLE1,2,3      |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SF,SG,SH       |-->| STRUCTURES DE F,G,H
C| SU,SV,SW       |-->| STRUCTURES DE U,V,W
C| SURFAC         |-->| SURFACE DES TRIANGLES.
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR INTERVENANT DANS LE
C|                |   | CALCUL DE LA MATRICE.
C| XEL,YEL        |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| FACTEUR MULTIPLICATIF
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_MT02AA_2 => MT02AA_2
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) ::        A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) ::               A33(*)
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: U(*),V(*)
C     STRUCTURE OF U AND V
      TYPE(BIEF_OBJ)  , INTENT(IN) :: SU,SV
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
C
      INTEGER IELEM,IELMNU,IELMNV,ISOU,ISOV
C
      DOUBLE PRECISION X2,X3,Y2,Y3
      DOUBLE PRECISION KSAT1,KSAT2,KSAT3
      DOUBLE PRECISION SOM,XSUR12
C
C=======================================================================
C
C     EXTRACTS THE TYPE OF ELEMENT FOR VISCOSITY
C
      IELMNU = SU%ELM
      ISOU   = SU%DIM2
C
      IELMNV = SV%ELM
      ISOV   = SV%DIM2
C
      XSUR12 = XMUL / 12.D0
C
C-----------------------------------------------------------------------
C TESTS THE TYPES OF U AND V
C U (KR) : P0 AND DIM 3 (BECAUSE DISCONTINUOUS P1) - V (KS) : P0 AND DIM 3
C-----------------------------------------------------------------------
C
      IF(IELMNU.EQ.10.AND.ISOU.EQ.3.AND.SU%DIMDISC.EQ.11
     &   .AND.
     &   IELMNV.EQ.10.AND.ISOV.EQ.3) THEN
C
      DO 4 IELEM = 1 , NELEM
C
C THE 3 TERMS OF MATRIX V (KS IS SYMMETRICAL)
C
        KSAT1=SV%R(IELEM)
        KSAT2=SV%R(IELEM+NELEM)
        KSAT3=SV%R(IELEM+2*NELEM)
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
         X2  =  XEL(IELEM,2)
         X3  =  XEL(IELEM,3)
C
         Y2  =  YEL(IELEM,2)
         Y3  =  YEL(IELEM,3)
C
C   INITIALISES THE INTERMEDIATE VARIABLES
C
         SOM = ( SU%R(IELEM+2*NELEM)
     &      +   SU%R(IELEM+NELEM)
     &      +   SU%R(IELEM) ) * XSUR12 / SURFAC(IELEM)
C
C  DIAGONAL TERMS
C
      A11(IELEM) = (KSAT1*Y2**2-2*KSAT1*Y2*Y3+KSAT1*Y3**2+KSAT2*X2**2-
     &  2*KSAT2*X2*X3+KSAT2*X3**2-2*KSAT3*Y2*X2+2*KSAT3*Y2*X3+
     &  2*KSAT3*X2*Y3-2*KSAT3*Y3*X3)*SOM
C
      A22(IELEM) = (KSAT1*Y3**2+KSAT2*X3**2-2*KSAT3*Y3*X3)*SOM
C
      A33(IELEM) = (KSAT1*Y2**2+KSAT2*X2**2-2*KSAT3*Y2*X2)*SOM

C
C  EXTRADIAGONAL TERMS
C
      A12(IELEM) = -(-KSAT1*Y2*Y3+KSAT1*Y3**2-KSAT2*X2*X3+KSAT2*X3**2+
     &          KSAT3*X2*Y3-2*KSAT3*Y3*X3+KSAT3*Y2*X3)*SOM
C
      A13(IELEM) = -(KSAT1*Y2**2-KSAT1*Y2*Y3+KSAT2*X2**2-KSAT2*X2*X3-
     &          2*KSAT3*Y2*X2+KSAT3*Y2*X3+KSAT3*X2*Y3)*SOM
C
      A23(IELEM) = (-KSAT1*Y2*Y3-KSAT2*X2*X3+KSAT3*Y2*X3+KSAT3*X2*Y3)*
     &          SOM
C
C   END OF THE LOOP ON THE ELEMENTS
C
4     CONTINUE
C-----------------------------------------------------------------------
C
      ELSE
C
        IF (LNG.EQ.1) WRITE(LU,10)
        IF (LNG.EQ.2) WRITE(LU,11)
10      FORMAT(1X,'MT02AA_2 (BIEF) : TYPES NON PREVUS')
11      FORMAT(1X,
     &  'MT02AA_2 (BIEF) : TYPES NOT AVAILABLE')
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