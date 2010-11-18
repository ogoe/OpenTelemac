C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS THE FOLLOWING MATRIX
!>                FOR P2 TRIANGLES:
!>  @code
!>            T*M + (1-T) *DM
!>
!>            M  IS THE MASS MATRIX P2*P2
!>            DM IS MASS-LUMPED M
!>            T IS A VECTOR CONSTANT BY ELEMENT
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
!>    </th><td> A11, A12, A13, A14, A15, A16, A22, A23, A24, A25, A26, A33, A34, A35, A36, A44, A45, A46, A55, A56, A66, F, NELEM, NELMAX, SF, SURFAC, XMUL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELEM, T, XSUR180, XSUR30, XSUR45
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
!> </td><td> 01/08/08
!> </td><td> A FROEHLY (MATMECA) 01 30 87 80 18
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
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>COMPOSANTES D'UN VECTEUR INTERVENANT DANS LE
!>                  CALCUL DE LA MATRICE.
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>FACTEUR MULTIPLICATIF
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MT07CC
     &( A11 , A12 , A13 , A14 , A15 , A16 ,
     &        A22 , A23 , A24 , A25 , A26 ,
     &              A33 , A34 , A35 , A36 ,
     &                    A44 , A45 , A46 ,
     &                          A55 , A56 ,
     &                                A66 ,
     &  XMUL,SF,F,SURFAC,NELEM,NELMAX)
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
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SF,SG,SH       |-->| STRUCTURES DE F,G ET H.
C| SU,SV,SW       |-->| STRUCTURES DE U,V ET W.
C| SURFAC         |---| 
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR INTERVENANT DANS LE
C|                |   | CALCUL DE LA MATRICE.
C| XMUL           |-->| FACTEUR MULTIPLICATIF
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF!, EX_MT07CC => MT07CC
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX
C
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A14(*),A15(*),A16(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A22(*),A23(*),A24(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A25(*),A26(*),A33(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A34(*),A35(*),A36(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A44(*),A45(*),A46(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A55(*),A56(*),A66(*)

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
      DOUBLE PRECISION T,XSUR30,XSUR45,XSUR180
      INTEGER IELEM
C
C=======================================================================
C
      XSUR30 = XMUL/30.D0
      XSUR45 = XMUL/45.D0
      XSUR180 = XMUL/180.D0
C
C-----------------------------------------------------------------------
C
      IF(SF%ELM.EQ.10) THEN
C
C-----------------------------------------------------------------------
C
C   P0 DISCRETISATION OF F:
C
      DO 5 IELEM = 1 , NELEM
C
C   INITIALISES THE INTERMEDIATE VARIABLES
C
         T = F(IELEM)
C
C  DIAGONAL TERMS
C
C  FOR P2 ELEMENTS, DM(1) = DM(2) = DM(3) = 0
C               AND DM(4) = DM(5) = DM(6) = S/3

         A11(IELEM) = T*SURFAC(IELEM)*XSUR30
         A22(IELEM) = A11(IELEM)
         A33(IELEM) = A11(IELEM)
         A44(IELEM) = (15.D0-7.D0*T)*SURFAC(IELEM)*XSUR45
         A55(IELEM) = A44(IELEM)
         A66(IELEM) = A44(IELEM)
C
C  EXTRADIAGONAL TERMS
C
         A12(IELEM) = -SURFAC(IELEM)*T*XSUR180
         A13(IELEM) = A12(IELEM)
         A14(IELEM) = 0.D0
         A15(IELEM) = -SURFAC(IELEM)*T*XSUR45
         A16(IELEM) = 0.D0
         A23(IELEM) = A12(IELEM)
         A24(IELEM) = 0.D0
         A25(IELEM) = 0.D0
         A26(IELEM) = A15(IELEM)
         A34(IELEM) = A15(IELEM)
         A35(IELEM) = 0.D0
         A36(IELEM) = 0.D0
         A45(IELEM) = (15.D0-11.D0*T)*SURFAC(IELEM)*XSUR45
         A46(IELEM) = A45(IELEM)
         A56(IELEM) = A45(IELEM)
C
5     CONTINUE
C
C-----------------------------------------------------------------------
C
C  ANOTHER DISCRETISATION OF F
C      ELSEIF(SF%ELM.EQ.XX) THEN
C
C-----------------------------------------------------------------------
C
      ELSE
C
       IF (LNG.EQ.1) WRITE(LU,100) SF%ELM,SF%NAME
       IF (LNG.EQ.2) WRITE(LU,101) SF%ELM,SF%NAME
100    FORMAT(1X,'MT07CC (BIEF) :',/,
     &        1X,'DISCRETISATION DE F NON PREVUE : ',1I6,
     &        1X,'NOM REEL : ',A6)
101    FORMAT(1X,'MT07CC (BIEF) :',/,
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