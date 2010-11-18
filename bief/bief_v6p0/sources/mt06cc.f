C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS THE FOLLOWING MATRIX:
!>  @code
!>     SUM(F*PSII*PSIJ)<br>
!>            WITH:  QUADRATIC P1
!>                   QUADRATIC P2
!>                   F P1 OR QUADRATIC
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
!>    </th><td> A11, A12, A13, A14, A15, A16, A22, A23, A24, A25, A26, A33, A34, A35, A36, A44, A45, A46, A55, A56, A66, F, IKLE1, IKLE2, IKLE3, IKLE4, IKLE5, IKLE6, NELEM, NELMAX, SF, SURFAC, XMUL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUX1260, AUX210, AUX315, AUX630, F1, F2, F3, F4, F5, F6, IELEM, IELMF, XSU1260, XSUR030, XSUR045, XSUR180, XSUR210, XSUR315, XSUR630
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
!>      <td><center> 5.9                                       </center>
!> </td><td> 19/06/08
!> </td><td> ALGIANE FROEHLY (MATMECA) 01 30 87 80 18
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
!>          <tr><td>A15
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A16
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
!>          <tr><td>A25
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A26
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A33
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A34
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A35
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A36
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A44
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A45
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A46
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A55
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A56
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A66
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
!>          <tr><td>IKLE5
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE6
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
                        SUBROUTINE MT06CC
     &( A11 , A12 , A13 , A14 , A15 , A16 ,
     &        A22 , A23 , A24 , A25 , A26 ,
     &              A33 , A34 , A35 , A36 ,
     &                    A44 , A45 , A46 ,
     &                          A55 , A56 ,
     &                                A66 ,
     &  XMUL,SF,F,SURFAC,
     &  IKLE1,IKLE2,IKLE3,IKLE4,IKLE5,IKLE6,NELEM,NELMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A11,A12        |<--| ELEMENTS DE LA MATRICE
C| A13            |---| 
C| A14            |---| 
C| A15            |---| 
C| A16            |---| 
C| A22            |---| 
C| A23            |---| 
C| A24            |---| 
C| A25            |---| 
C| A26            |---| 
C| A33            |---| 
C| A34            |---| 
C| A35            |---| 
C| A36            |---| 
C| A44            |---| 
C| A45            |---| 
C| A46            |---| 
C| A55            |---| 
C| A56            |---| 
C| A66            |---| 
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LE CALCUL DE LA
C|                |   | MATRICE.
C| IKLE1          |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| IKLE2          |---| 
C| IKLE3          |---| 
C| IKLE4          |---| 
C| IKLE5          |---| 
C| IKLE6          |---| 
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
      USE BIEF!, EX_MT06CC => MT06CC
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
      INTEGER, INTENT(IN) :: IKLE5(NELMAX),IKLE6(NELMAX)
C
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A14(*),A15(*),A16(*)
      DOUBLE PRECISION, INTENT(INOUT) ::        A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A24(*),A25(*),A26(*)
      DOUBLE PRECISION, INTENT(INOUT) ::               A33(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A34(*),A35(*),A36(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A44(*),A45(*),A46(*)
      DOUBLE PRECISION, INTENT(INOUT) ::        A55(*),A56(*)
      DOUBLE PRECISION, INTENT(INOUT) ::               A66(*)
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
      DOUBLE PRECISION F1,F2,F3,F4,F5,F6,XSUR030,XSUR045,XSUR180
      DOUBLE PRECISION XSUR315,XSUR210,XSUR630,XSU1260
      DOUBLE PRECISION AUX315,AUX210,AUX630,AUX1260
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
      XSUR030 = XMUL / 30.D0
      XSUR045 = XMUL / 45.D0
      XSUR180 = XMUL /180.D0
C
      DO 1 IELEM = 1 , NELEM
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
         F1  =  F(IELEM) * SURFAC(IELEM)
C
C  DIAGONAL TERMS
C
         A11(IELEM) =   XSUR030 * F1
         A22(IELEM) =   A11(IELEM)
         A33(IELEM) =   A11(IELEM)
         A44(IELEM) =   8.D0 * XSUR045 * F1
         A55(IELEM) =   A44(IELEM)
         A66(IELEM) =   A44(IELEM)
C
C  EXTRADIAGONAL TERMS
C
         A12(IELEM) = - XSUR180 * F1
         A13(IELEM) =   A12(IELEM)
         A14(IELEM) =   0.D0
         A15(IELEM) = - XSUR045*F1
         A16(IELEM) =   0.D0
C
         A23(IELEM) =   A12(IELEM)
         A24(IELEM) =   0.D0
         A25(IELEM) =   0.D0
         A26(IELEM) =   A15(IELEM)
C
         A34(IELEM) =   A15(IELEM)
         A35(IELEM) =   0.D0
         A36(IELEM) =   0.D0
C
         A45(IELEM) =   4.D0*XSUR045*F1
         A46(IELEM) =   A45(IELEM)
C
         A56(IELEM) =   A45(IELEM)
C
1     CONTINUE
C
C-----------------------------------------------------------------------
C
C  CASE WHERE F IS LINEAR
C
      ELSEIF(IELMF.EQ.11) THEN
C
      XSUR210 = XMUL /  210.D0
      XSUR315 = XMUL /  315.D0
      XSU1260 = XMUL / 1260.D0
C
      DO 4 IELEM = 1 , NELEM

      AUX210 = SURFAC(IELEM) * XSUR210
      AUX315 = SURFAC(IELEM) * XSUR315
      AUX1260= SURFAC(IELEM) * XSU1260
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
         A11(IELEM) =      (5.D0*F1+     F2+     F3)*AUX210
         A22(IELEM) =      (     F1+5.D0*F2+     F3)*AUX210
         A33(IELEM) =      (     F1+     F2+5.D0*F3)*AUX210
         A44(IELEM) = 8.D0*(3.D0*F1+3.D0*F2+     F3)*AUX315
         A55(IELEM) = 8.D0*(     F1+3.D0*F2+3.D0*F3)*AUX315
         A66(IELEM) = 8.D0*(3.D0*F1+     F2+3.D0*F3)*AUX315
C
C  EXTRADIAGONAL TERMS
C
         A12(IELEM) = -(4.D0*F1+4.D0*F2-     F3)*AUX1260
         A13(IELEM) = -(4.D0*F1-     F2+4.D0*F3)*AUX1260
         A14(IELEM) =  (3.D0*F1-2.D0*F2-     F3)*AUX315
         A15(IELEM) = -(     F1+3.D0*F2+3.D0*F3)*AUX315
         A16(IELEM) =  (3.D0*F1-     F2-2.D0*F3)*AUX315
C
         A23(IELEM) =  (     F1-4.D0*F2-4.D0*F3)*AUX1260
         A24(IELEM) = -(2.D0*F1-3.D0*F2+     F3)*AUX315
         A25(IELEM) = -(     F1-3.D0*F2+2.D0*F3)*AUX315
         A26(IELEM) = -(3.D0*F1+     F2+3.D0*F3)*AUX315
C
         A34(IELEM) = -(3.D0*F1+3.D0*F2+     F3)*AUX315
         A35(IELEM) = -(     F1+2.D0*F2-3.D0*F3)*AUX315
         A36(IELEM) = -(2.D0*F1+     F2-3.D0*F3)*AUX315
C
         A45(IELEM) =  (2.D0*F1+3.D0*F2+2.D0*F3)*AUX315*4.D0
         A46(IELEM) =  (3.D0*F1+2.D0*F2+2.D0*F3)*AUX315*4.D0
C
         A56(IELEM) =  (2.D0*F1+3.D0*F3+2.D0*F2)*AUX315*4.D0
C
4     CONTINUE
C
C-----------------------------------------------------------------------
C
      ELSEIF(IELMF.EQ.13) THEN
C
C-----------------------------------------------------------------------
C
C   QUADRATIC DISCRETISATION OF F:
C
      XSUR315 = XMUL /  315.D0
      XSUR630 = XMUL /  630.D0
      XSU1260 = XMUL / 1260.D0
C
      DO 5 IELEM = 1 , NELEM

      AUX315 = SURFAC(IELEM) * XSUR315
      AUX630 = SURFAC(IELEM) * XSUR630
      AUX1260= SURFAC(IELEM) * XSU1260
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
      F1  =  F(IKLE1(IELEM))
      F2  =  F(IKLE2(IELEM))
      F3  =  F(IKLE3(IELEM))
      F4  =  F(IKLE4(IELEM))
      F5  =  F(IKLE5(IELEM))
      F6  =  F(IKLE6(IELEM))
C
C  DIAGONAL TERMS
C
       A11(IELEM) = (6.D0*(F4+F6)+9.D0*F1+2.D0*F5-F2-F3) * AUX630
       A22(IELEM) = (6.D0*(F4+F5)+2.D0*F6+9.D0*F2-F1-F3) * AUX630
       A33(IELEM) = (6.D0*(F6+F5)+9.D0*F3+2.D0*F4-F1-F2) * AUX630
       A44(IELEM) =  4.D0*(3.D0*(F6+F5)+9.D0*F4-F3) * AUX315
       A55(IELEM) =  4.D0*(3.D0*(F4+F6)+9.D0*F5-F1) * AUX315
       A66(IELEM) =  4.D0*(3.D0*(F4+F5)+9.D0*F6-F2) * AUX315
C
C  EXTRADIAGONAL TERMS
C
      A12(IELEM) = -(2.D0*(F1+F2)+4.D0*F4-F3) * AUX1260
      A13(IELEM) = -(2.D0*(F1+F3)+4.D0*F6-F2) * AUX1260
      A14(IELEM) =  (3.D0* F1    -2.D0*F5-F2) * AUX315
      A15(IELEM) = -(2.D0*(F4+F6)+4.D0*F5-F1) * AUX315
      A16(IELEM) =  (3.D0*F1     -2.D0*F5-F3) * AUX315
C
      A23(IELEM) =  (-2.D0*(F2+F3)-4.D0*F5+F1) * AUX1260
      A24(IELEM) =  (-2.D0*F6     +3.D0*F2-F1) * AUX315
      A25(IELEM) =  (-2.D0*F6     +3.D0*F2-F3) * AUX315
      A26(IELEM) =  (-2.D0*(F4+F5)-4.D0*F6+F2) * AUX315
C
      A34(IELEM) =  (-2.D0*(F6+F5)-4.D0*F4+F3) * AUX315
      A35(IELEM) =  (-2.D0*F4     +3.D0*F3-F2) * AUX315
      A36(IELEM) =  (-2.D0*F4     +3.D0*F3-F1) * AUX315
C
      A45(IELEM) =  2.D0*(6.D0*(F4+F5)+4.D0*F6-F1-F3) * AUX315
      A46(IELEM) =  2.D0*(6.D0*(F4+F6)+4.D0*F5-F2-F3) * AUX315
C
      A56(IELEM) =  2.D0*(6.D0*(F6+F5)+4.D0*F4-F2-F1) * AUX315
C
5     CONTINUE
C
      ELSE
C
       IF (LNG.EQ.1) WRITE(LU,100) IELMF,SF%NAME
       IF (LNG.EQ.2) WRITE(LU,101) IELMF,SF%NAME
100    FORMAT(1X,'MT06CC (BIEF) :',/,
     &        1X,'DISCRETISATION DE F NON PREVUE : ',1I6,
     &        1X,'NOM REEL : ',A6)
101    FORMAT(1X,'MT06CC (BIEF) :',/,
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