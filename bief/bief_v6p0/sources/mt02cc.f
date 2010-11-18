C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS THE DIFFUSION MATRIX FOR P2 TRIANGLES.
!><br>            VISCOSITY CAN BE ISOTROPIC, OR NOT ISOTROPIC. IN THIS
!>                CASE U IS AN ARRAY WITH SECOND DIMENSION EQUAL TO 3.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE JACOBIAN MUST BE POSITIVE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A11, A12, A13, A14, A15, A16, A22, A23, A24, A25, A26, A33, A34, A35, A36, A44, A45, A46, A55, A56, A66, IKLE1, IKLE2, IKLE3, NELEM, NELMAX, SU, SURFAC, U, XEL, XMUL, YEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUX1, AUX2, AUX3, AUX4, IAD2, IAD3, IELEM, IELMNU, ISO, NUX1, NUX2, NUX3, NUY1, NUY2, NUY3, NUZ1, NUZ2, NUZ3, X2, X3, Y2, Y3
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
!> </td><td> ALGIANE FROEHLY (MATMECA)  01 30 87 80 18
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
                        SUBROUTINE MT02CC
     &( A11 , A12 , A13 , A14 , A15, A16,
     &        A22 , A23 , A24 , A25, A26,
     &              A33 , A34 , A35, A36,
     &                    A44 , A45, A46,
     &                          A55, A56,
     &                               A66,
     &  XMUL,SU,U,XEL,YEL,SURFAC,IKLE1,
     &  IKLE2,IKLE3,NELEM,NELMAX)
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
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SF,SG,SH       |-->| STRUCTURES DE F,G,H
C| SU,SV,SW       |-->| STRUCTURES DE U,V,W
C| SURFAC         |-->| SURFACE DES TRIANGLES.
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR INTERVENANT DANS LE
C|                |   | CALCUL DE LA MATRICE.
C| XEL,YEL,ZEL    |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| FACTEUR MULTIPLICATIF
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF!, EX_MT02CC => MT02CC
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A14(*),A15(*),A16(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A24(*),A25(*),A26(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A33(*),A34(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A35(*),A36(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A44(*),A45(*),A46(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A55(*),A56(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A66(*)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL,U(*)
C     STRUCTURE OF U
      TYPE(BIEF_OBJ), INTENT(IN)      :: SU
C
      DOUBLE PRECISION, INTENT(IN)    :: XEL(NELMAX,3),YEL(NELMAX,3)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELMAX)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
C
      INTEGER IELMNU,IELEM,ISO,IAD2,IAD3
C
      DOUBLE PRECISION X2,X3,Y2,Y3,AUX1,AUX2,AUX3,AUX4
      DOUBLE PRECISION NUX1,NUX2,NUX3
      DOUBLE PRECISION NUY1,NUY2,NUY3
      DOUBLE PRECISION NUZ1,NUZ2,NUZ3
C
C=======================================================================
C
C     EXTRACTS THE TYPE OF ELEMENT FOR VISCOSITY
C
      IELMNU = SU%ELM
      ISO = SU%DIM2
C
C     IF(IELMNU.EQ.10.AND.ISO.EQ.1) THEN
C
C-----------------------------------------------------------------------
C
C  P0 DISCRETISATION FOR VISCOSITY:
C
C-----------------------------------------------------------------------
C
      IF(IELMNU.EQ.11.AND.ISO.EQ.1) THEN
C
C-----------------------------------------------------------------------
C
C  P1 DISCRETISATION FOR ISOTROPIC VISCOSITY:
C
      DO 5 IELEM = 1 , NELEM
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
         X2  =  XEL(IELEM,2)
         X3  =  XEL(IELEM,3)
C
         Y2  =  YEL(IELEM,2)
         Y3  =  YEL(IELEM,3)
C
         NUX1 = U(IKLE1(IELEM))
         NUX2 = U(IKLE2(IELEM))
         NUX3 = U(IKLE3(IELEM))
C
         AUX1 = XMUL/(60.D0*SURFAC(IELEM))
         AUX2 = 3.D0 * AUX1
         AUX3 = 4.D0 * AUX1
         AUX4 = 8.D0 * AUX1
C
C  EXTRADIAGONAL TERMS
C
         A12(IELEM)= -(-Y3**2+Y3*Y2-X3**2+X3*X2) *
     &                (2.D0*NUX1+2.D0*NUX2+NUX3) * AUX1
C
         A13(IELEM)=  (-Y3*Y2+Y2**2-X3*X2+X2**2) *
     &                (2.D0*NUX1+NUX2+2.D0*NUX3) * AUX1
C
         A14(IELEM)= ((-11.D0*NUX1-4.D0*NUX3-5.D0*NUX2) * (Y3**2+X3**2)
     &             +  (3.D0*NUX1-NUX3-2.D0*NUX2       ) * (Y2**2+X2**2)
     &             +  (8.D0*NUX1+7.D0*NUX2+5.D0*NUX3  ) * (X3*X2+Y3*Y2))
     &             *  AUX1
C
         A15(IELEM)=-((3.D0*NUX1-2.D0*NUX3-NUX2       ) * (Y3**2+X3**2)
     &             +  (3.D0*NUX1-NUX3-2.D0*NUX2       ) * (Y2**2+X2**2)
     &             +  (-6.D0*NUX1+3.D0*NUX2+3.D0*NUX3 ) * (X3*X2+Y3*Y2))
     &             *  AUX1
C
         A16(IELEM)=-((-3.D0*NUX1+2.D0*NUX3+NUX2      ) * (Y3**2+X3**2)
     &             +  (11.D0*NUX1+5.D0*NUX3+4.D0*NUX2 ) * (Y2**2+X2**2)
     &             +  (-8.D0*NUX1-5.D0*NUX2-7.D0*NUX3 ) * (X3*X2+Y3*Y2))
     &             *  AUX1
C
         A23(IELEM)=  (Y3*Y2+X3*X2) * (NUX1+2.D0*NUX3+2.D0*NUX2) * AUX1
C
         A24(IELEM)= ((-5.D0*NUX1-4.D0*NUX3-11.D0*NUX2) * (Y3**2+X3**2)
     &             +  (3.D0*NUX1+14.D0*NUX2+3.D0*NUX3 ) * (X3*X2+Y3*Y2))
     &             *  AUX1
C
         A25(IELEM)=-((NUX1+2.D0*NUX3-3.D0*NUX2       ) * (Y3**2+X3**2)
     &             +  (3.D0*NUX1+3.D0*NUX3+14.D0*NUX2 ) * (Y3*Y2+X3*X2))
     &             *   AUX1
C
         A26(IELEM)= ((NUX1+2.D0*NUX3-3.D0*NUX2       ) * (Y3**2+X3**2)
     &             +  (NUX1-NUX3)                       * (X3*X2+Y3*Y2))
     &             *  AUX1
C
         A34(IELEM)= ((NUX1-3.D0*NUX3+2.D0*NUX2      ) * (Y2**2+X2**2)
     &             +  (NUX1-NUX2)                      * (X3*X2+Y3*Y2))
     &             *  AUX1
C
         A35(IELEM)=-((NUX1-3.D0*NUX3+2.D0*NUX2      ) * (Y2**2+X2**2)
     &             +  (3.D0*NUX1+3.D0*NUX2+14.D0*NUX3) * (X3*X2+Y3*Y2))
     &             *  AUX1
C
         A36(IELEM)=-((5.D0*NUX1+11.D0*NUX3+4.D0*NUX2) * (Y2**2+X2**2)
     &             +  (-3.D0*(NUX1+NUX2)-14.D0*NUX3  ) * (X3*X2+Y3*Y2))
     &             *  AUX1
C
         A45(IELEM)=-((-NUX1+NUX2                    ) * (Y3**2+X3**2)
     &             +  (-NUX1-6.D0*NUX2-3.D0*NUX3     ) * (X3*X2+Y3*Y2)
     &             +  (6.D0*NUX2+2.D0*NUX1+2.D0*NUX3 ) * (Y2**2+X2**2))
     &             *  AUX3
C
         A46(IELEM)=-((NUX1-NUX2                     ) * (Y3**2+X3**2)
     &             +  (NUX1-NUX3                     ) * (Y2**2+X2**2)
     &             +  (4.D0*NUX1+3.D0*NUX2+3.D0*NUX3 ) * (X3*X2+Y3*Y2))
     &             *  AUX3
C
         A56(IELEM)= ((-2.D0*NUX1-2.D0*NUX2-6.D0*NUX3) * (Y3**2+X3**2)
     &             +  (NUX1-NUX3                     ) * (Y2**2+X2**2)
     &             +  (NUX1+3.D0*NUX2+6.D0*NUX3      ) * (X3*X2+Y3*Y2))
     &             *  AUX3
C
C    DIAGONAL TERMS
C
         A11(IELEM)= ((Y3-Y2)**2+(X3-X2)**2)
     &             * (3.D0*NUX1+NUX2+NUX3) * AUX2
C
         A22(IELEM)=  (Y3**2+X3**2) * (NUX1+3.D0*NUX2+NUX3) * AUX2
C
         A33(IELEM)=  (Y2**2+X2**2) * (NUX1+NUX2+3.D0*NUX3) * AUX2
C
         A44(IELEM)=  ((2.D0*NUX1+NUX3+2.D0*NUX2) * (Y3**2+X3**2)
     &             +   (NUX1+NUX3+3.D0*NUX2     ) * (Y2**2+X2**2)
     &             +   (-4.D0*NUX2-NUX3         ) * (X3*X2+Y3*Y2))
     &             *   AUX4
C
         A55(IELEM)=  ((NUX1+NUX2+3.D0*NUX3     ) * (Y3**2+X3**2)
     &             +   (NUX1+NUX3+3.D0*NUX2     ) * (Y2**2+X2**2)
     &             +  (-NUX1-2.D0*NUX2-2.D0*NUX3) * (X3*X2+Y3*Y2))
     &             *   AUX4
C
        A66(IELEM) = ((NUX1+3.D0*NUX3+NUX2      ) * (Y3**2+X3**2)
     &             +  (2.D0*NUX1+2.D0*NUX3+NUX2 ) * (Y2**2+X2**2)
     &             +  (-NUX2-4.D0*NUX3          ) * (X3*X2+Y3*Y2))
     &             *  AUX4
C
5     CONTINUE
C
C-----------------------------------------------------------------------
C
      ELSEIF(IELMNU.EQ.11.AND.ISO.EQ.3) THEN
C
C-----------------------------------------------------------------------
C
C  P1 DISCRETISATION FOR NONISOTROPIC VISCOSITY:
C
      IAD2 = SU%MAXDIM1
      IAD3 = 2*IAD2
      DO 6 IELEM = 1 , NELEM
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
         X2  =  XEL(IELEM,2)
         X3  =  XEL(IELEM,3)
C
         Y2  =  YEL(IELEM,2)
         Y3  =  YEL(IELEM,3)
C
         NUX1 = U(IKLE1(IELEM))
         NUX2 = U(IKLE2(IELEM))
         NUX3 = U(IKLE3(IELEM))
         NUY1 = U(IKLE1(IELEM) + IAD2)
         NUY2 = U(IKLE2(IELEM) + IAD2)
         NUY3 = U(IKLE3(IELEM) + IAD2)
         NUZ1 = U(IKLE1(IELEM) + IAD3)
         NUZ2 = U(IKLE2(IELEM) + IAD3)
         NUZ3 = U(IKLE3(IELEM) + IAD3)
C
         AUX1 = XMUL/(60.D0 * SURFAC(IELEM))
         AUX2 = 4.D0 * AUX1
         AUX3 = 3.D0 * AUX1
C
C  EXTRADIAGONAL TERMS
C
      A12(IELEM) =
     &   ((NUY3+2.D0*NUY1+2.D0*NUY2) * (X3**2-X3*X2           ) +
     &    (NUZ3+2.D0*NUZ1+2.D0*NUZ2) * (Y2*X3+Y3*X2-2.D0*Y3*X3) +
     &    (NUX3+2.D0*NUX2+2.D0*NUX1) * (Y3**2-Y3*Y2           ) ) * AUX1
C
      A13(IELEM) =
     &   ((2.D0*NUY1+2.D0*NUY3+NUY2) * (X2**2-X3*X2           ) +
     &    (2.D0*NUZ1+2.D0*NUZ3+NUZ2) * (Y3*X2+Y2*X3-2.D0*Y2*X2) +
     &    (2.D0*NUX1+NUX2+2.D0*NUX3) * (Y2**2-Y3*Y2           ) ) * AUX1
C
      A14(IELEM) =
     &  -((- 3.D0*NUX1+ 2.D0*NUX2+      NUX3) * Y2**2         +
     &    (- 8.D0*NUX1- 7.D0*NUX2- 5.D0*NUX3) * Y3*Y2         +
     &    ( 11.D0*NUX1+ 5.D0*NUX2+ 4.D0*NUX3) * Y3**2         +
     &    (- 3.D0*NUY1+ 2.D0*NUY2+      NUY3) * X2**2         +
     &    (- 8.D0*NUY1- 7.D0*NUY2- 5.D0*NUY3) * X3*X2         +
     &    ( 11.D0*NUY1+ 5.D0*NUY2+ 4.D0*NUY3) * X3**2         +
     &    (  6.D0*NUZ1- 4.D0*NUZ2- 2.D0*NUZ3) * Y2*X2         +
     &    (  8.D0*NUZ1+ 7.D0*NUZ2+ 5.D0*NUZ3) * (Y3*X2+Y2*X3) +
     &    (-22.D0*NUZ1-10.D0*NUZ2- 8.D0*NUZ3) * Y3*X3         ) * AUX1
C
      A15(IELEM) =
     &  -((  3.D0*NUX1- 2.D0*NUX2-      NUX3) * Y2**2         +
     &    (- 6.D0*NUX1+ 3.D0*NUX2+ 3.D0*NUX3) * Y3*Y2         +
     &    (  3.D0*NUX1-      NUX2- 2.D0*NUX3) * Y3**2         +
     &    (  3.D0*NUY1- 2.D0*NUY2-      NUY3) * X2**2         +
     &    (- 6.D0*NUY1+ 3.D0*NUY2+ 3.D0*NUY3) * X3*X2         +
     &    (  3.D0*NUY1-      NUY2- 2.D0*NUY3) * X3**2         +
     &    (- 6.D0*NUZ1+ 4.D0*NUZ2+ 2.D0*NUZ3) * Y2*X2         +
     &    (  6.D0*NUZ1- 3.D0*NUZ2- 3.D0*NUZ3) * (Y3*X2+Y2*X3) +
     &    (- 6.D0*NUZ1+ 2.D0*NUZ2+ 4.D0*NUZ3) * Y3*X3         ) * AUX1
C
      A16(IELEM) =
     &   ((-11.D0*NUX1- 4.D0*NUX2- 5.D0*NUX3) * Y2**2         +
     &    (  8.D0*NUX1+ 5.D0*NUX2+ 7.D0*NUX3) * Y3*Y2         +
     &    (  3.D0*NUX1-      NUX2- 2.D0*NUX3) * Y3**2         +
     &    (-11.D0*NUY1- 4.D0*NUY2- 5.D0*NUY3) * X2**2         +
     &    (+ 8.D0*NUY1+ 5.D0*NUY2+ 7.D0*NUY3) * X3*X2         +
     &    (  3.D0*NUY1-      NUY2- 2.D0*NUY3) * X3**2         +
     &    ( 22.D0*NUZ1+ 8.D0*NUZ2+10.D0*NUZ3) * Y2*X2         +
     &    (- 8.D0*NUZ1- 5.D0*NUZ2- 7.D0*NUZ3) * (Y3*X2+Y2*X3) +
     &    (- 6.D0*NUZ1+ 2.D0*NUZ2+ 4.D0*NUZ3) * Y3*X3         ) * AUX1
C
      A23(IELEM) =
     &   (( NUY1+2.D0*NUY2+2.D0*NUY3) * X3*X2                 +
     &    (-NUZ1-2.D0*NUZ2-2.D0*NUZ3) * (Y3*X2+Y2*X3)         +
     &    ( NUX1+2.D0*NUX2+2.D0*NUX3) * Y3*Y2                 ) * AUX1
C
      A24(IELEM) =
     &  -((- 3.D0*NUX1-14.D0*NUX2- 3.D0*NUX3) * Y3*Y2         +
     &    (  5.D0*NUX1+11.D0*NUX2+ 4.D0*NUX3) * Y3**2         +
     &    (- 3.D0*NUY1-14.D0*NUY2- 3.D0*NUY3) * X3*X2         +
     &    (  5.D0*NUY1+11.D0*NUY2+ 4.D0*NUY3) * X3**2         +
     &    (  3.D0*NUZ1+14.D0*NUZ2+ 3.D0*NUZ3) * (Y3*X2+Y2*X3) +
     &    (-10.D0*NUZ1-22.D0*NUZ2- 8.D0*NUZ3) * Y3*X3         ) * AUX1
C
      A25(IELEM) =
     &  -((  3.D0*NUX1+14.D0*NUX2+ 3.D0*NUX3) * Y3*Y2         +
     &    (       NUX1- 3.D0*NUX2+ 2.D0*NUX3) * Y3**2         +
     &    (  3.D0*NUY1+14.D0*NUY2+ 3.D0*NUY3) * X3*X2         +
     &    (       NUY1- 3.D0*NUY2+ 2.D0*NUY3) * X3**2         +
     &    (- 3.D0*NUZ1-14.D0*NUZ2- 3.D0*NUZ3) * (Y3*X2+Y2*X3) +
     &    (- 2.D0*NUZ1+ 6.D0*NUZ2- 4.D0*NUZ3) * Y3*X3         ) * AUX1
C
      A26(IELEM) =
     &   (( NUX1-NUX3) * Y3*Y2 + (-NUY3+NUY1) * X3*X2 +
     &    ( NUZ3-NUZ1) * (Y3*X2+X3*Y2)                +
     &    (2.D0*NUY3+NUY1-3.D0*NUY2) * X3**2          +
     &    (6.D0*NUZ2-4.D0*NUZ3-2.D0*NUZ1) * Y3*X3     +
     &    (2.D0*NUX3-3.D0*NUX2+     NUX1) * Y3**2     ) * AUX1
C
      A34(IELEM) =
     &   ((      NUY1-3.D0*NUY3+2.D0*NUY2)  * X2**2 +
     &    (-2.D0*NUZ1+6.D0*NUZ3-4.D0*NUZ2)  * Y2*X2 +
     &    (      NUX1+2.D0*NUX2-3.D0*NUX3)  * Y2**2 +
     &    (NUY1-NUY2) * X3*X2 + (NUX1-NUX2) * Y3*Y2 +
     &    (NUZ2-NUZ1) * (Y3*X2+Y2*X3)               )*AUX1
C
      A35(IELEM) =
     &  -((       NUX1+ 2.D0*NUX2- 3.D0*NUX3) * Y2**2         +
     &    (  3.D0*NUX1+ 3.D0*NUX2+14.D0*NUX3) * Y3*Y2         +
     &    (       NUY1+ 2.D0*NUY2- 3.D0*NUY3) * X2**2         +
     &    (  3.D0*NUY1+ 3.D0*NUY2+14.D0*NUY3) * X3*X2         +
     &    (- 3.D0*NUZ1- 3.D0*NUZ2-14.D0*NUZ3) * (Y3*X2+Y2*X3) +
     &    (- 2.D0*NUZ1- 4.D0*NUZ2+ 6.D0*NUZ3) * Y2*X2         ) * AUX1
C
      A36(IELEM) =
     &   ((- 5.D0*NUX1- 4.D0*NUX2-11.D0*NUX3) * Y2**2         +
     &    (  3.D0*NUX1+3.D0*NUX2+ 14.D0*NUX3) * Y3*Y2         +
     &    (- 5.D0*NUY1- 4.D0*NUY2-11.D0*NUY3) * X2**2         +
     &    (  3.D0*NUY1+ 3.D0*NUY2+14.D0*NUY3) * X3*X2         +
     &    ( 10.D0*NUZ1+ 8.D0*NUZ2+22.D0*NUZ3) * Y2*X2         +
     &    (- 3.D0*NUZ1- 3.D0*NUZ2-14.D0*NUZ3) * (Y3*X2+Y2*X3) ) * AUX1
C
      A45(IELEM) =
     &   ((- 2.D0*NUY1- 6.D0*NUY2- 2.D0*NUY3) * X2**2         +
     &    (       NUY1+ 6.D0*NUY2+ 3.D0*NUY3) * X3*X2         +
     &    (  4.D0*NUZ1+12.D0*NUZ2+ 4.D0*NUZ3) * Y2*X2         +
     &    (-      NUZ1- 6.D0*NUZ2- 3.D0*NUZ3) * (Y3*X2+Y2*X3) +
     &    (NUY1-NUY2)*X3**2 + (NUX1-NUX2)*Y3**2               +
     &    (- 2.D0*NUZ1+ 2.D0*NUZ2           ) * Y3*X3         +
     &    (- 2.D0*NUX1- 2.D0*NUX3- 6.D0*NUX2) * Y2**2         +
     &    (  6.D0*NUX2+ 3.D0*NUX3+      NUX1) * Y3*Y2         ) * AUX2
C
      A46(IELEM) =
     &  -((-NUY3+NUY1)*X2**2 + (NUX1-NUX2) * Y3**2            +
     &    ( NUX1-NUX3)*Y2**2 + (NUY1-NUY2) * X3**2            +
     &    (  4.D0*NUY1+ 3.D0*NUY3+ 3.D0*NUY2) * X3*X2         +
     &    (  2.D0*NUZ3- 2.D0*NUZ1           ) * Y2*X2         +
     &    ( -3.D0*NUZ3- 4.D0*NUZ1- 3.D0*NUZ2) * (Y3*X2+Y2*X3) +
     &    (- 2.D0*NUZ1+ 2.D0*NUZ2           ) * Y3*X3         +
     &    (  3.D0*NUX2+ 4.D0*NUX1+ 3.D0*NUX3) * Y3*Y2         ) * AUX2
C
      A56(IELEM) =
     &  -(( NUY3-NUY1)*X2**2 + (NUX3-NUX1) * Y2**2            +
     &    (- 3.D0*NUY2- 6.D0*NUY3-      NUY1) * X3*X2         +
     &    (  2.D0*NUZ1- 2.D0*NUZ3           ) * Y2*X2         +
     &    (  3.D0*NUZ2+      NUZ1+ 6.D0*NUZ3) * (Y3*X2+Y2*X3) +
     &    (  6.D0*NUY3+ 2.D0*NUY1+ 2.D0*NUY2) * X3**2         +
     &    (-12.D0*NUZ3- 4.D0*NUZ2- 4.D0*NUZ1) * Y3*X3         +
     &    (- 6.D0*NUX3- 3.D0*NUX2-      NUX1) * Y3*Y2         +
     &    (  2.D0*NUX2+ 2.D0*NUX1+ 6.D0*NUX3) * Y3**2         ) * AUX2
C
C   THE DIAGONAL TERMS ARE OBTAINED BY MEANS OF THE
C   MAGIC SQUARE OR BY DIRECT COMPUTATION:
C
      A11(IELEM) = - A12(IELEM) - A13(IELEM) - A14(IELEM)
     &             - A15(IELEM) - A16(IELEM)
      A22(IELEM) = ((NUY1+NUY3+3.D0*NUY2) * X3**2            +
     &              (-6.D0*NUZ2-2.D0*NUZ1-2.D0*NUZ3) * Y3*X3 +
     &              (NUX3+NUX1+3.D0*NUX2) * Y3**2            ) * AUX3
      A33(IELEM) = ((NUY1+3.D0*NUY3+NUY2) * X2**2            +
     &              (-6.D0*NUZ3-2.D0*NUZ1-2.D0*NUZ2) * Y2*X2 +
     &              ( 3.D0*NUX3+NUX1+NUX2          ) * Y2**2 ) * AUX3
      A44(IELEM) = - A14(IELEM) - A24(IELEM) - A34(IELEM)
     &             - A45(IELEM) - A46(IELEM)
      A55(IELEM) = - A15(IELEM) - A25(IELEM) - A35(IELEM)
     &             - A45(IELEM) - A56(IELEM)
      A66(IELEM) = - A16(IELEM) - A26(IELEM) - A36(IELEM)
     &             - A46(IELEM) - A56(IELEM)
C
6     CONTINUE
C
C-----------------------------------------------------------------------
C
      ELSE
C
        IF (LNG.EQ.1) WRITE(LU,10) IELMNU,ISO
        IF (LNG.EQ.2) WRITE(LU,11) IELMNU,ISO
10    FORMAT(1X,'MT02CC (BIEF) :TYPE DE VISCOSITE NON PREVU: ',2I6)
11    FORMAT(1X,'MT02CC (BIEF) :TYPE OF VISCOSITY NOT AVAILABLE: ',2I6)
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