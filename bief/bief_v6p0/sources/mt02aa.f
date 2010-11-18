C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS THE DIFFUSION MATRIX FOR P1 TRIANGLES.
!><br>            VISCOSITY CAN BE ISOTROPIC IF U IS A VECTOR WITH 1
!>                DIMENSION. IT CAN BE ALSO TENSORIAL IF U IS A VECTOR
!>                WITH 3 DIMENSIONS, THEN REPRESENTING NUXX, NUYY, NUXY.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE JACOBIAN MUST BE POSITIVE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A11, A12, A13, A22, A23, A33, FORMUL, IKLE1, IKLE2, IKLE3, NELEM, NELMAX, SU, SURFAC, SV, U, V, XEL, XMUL, YEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> APD, AUX, BPE, COEF1, COEF2, CPF, G1, G123, G2, G3, I1, I2, I3, IAD2, IAD3, IELEM, IELMNU, IELMNV, ISO, S2D, SOMVX, SOMVY, SOMVZ, VISC1, VISC2, VISC3, X2, X2AUX, X2MX3, X2X3, X3, X3AUX, XSUR12, XSUR48, Y2, Y2AUX, Y2MY3, Y2Y3, Y3, Y3AUX
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MT02AA
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
!> </td><td> 16/07/07
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
!>          <tr><td>FORMUL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE1,2,3
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
                        SUBROUTINE MT02AA
     &( A11 , A12 , A13 ,
     &        A22 , A23 ,
     &              A33 ,
     &  XMUL,SU,U,SV,V,
     &  XEL,YEL,SURFAC,IKLE1,IKLE2,IKLE3,NELEM,NELMAX,FORMUL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A11,A12        |<--| ELEMENTS DE LA MATRICE
C| A13            |---| 
C| A22            |---| 
C| A23            |---| 
C| A33            |---| 
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LE CALCUL DE LA
C|                |   | MATRICE.
C| FORMUL         |---| 
C| IKLE1,2,3      |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
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
C| XEL,YEL        |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| FACTEUR MULTIPLICATIF
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_MT02AA => MT02AA
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
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) ::        A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) ::               A33(*)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
      DOUBLE PRECISION, INTENT(IN)    :: U(*),V(*)
C     STRUCTURE OF U
      TYPE(BIEF_OBJ), INTENT(IN)      :: SU,SV
      DOUBLE PRECISION, INTENT(IN)    :: XEL(NELMAX,3),YEL(NELMAX,3)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELMAX)
      CHARACTER(LEN=16), INTENT(IN)   :: FORMUL
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
C
      INTEGER IELMNU,IELMNV,IELEM,ISO,IAD2,IAD3,I1,I2,I3
C
      DOUBLE PRECISION X2,X3,Y2,Y3,S2D,VISC1,VISC2,VISC3,BPE,CPF,APD
      DOUBLE PRECISION AUX,X2X3,Y2Y3,X2AUX,Y2AUX,X3AUX,Y3AUX,X2MX3,Y2MY3
      DOUBLE PRECISION SOMVX,SOMVY,SOMVZ,XSUR12,XSUR48
      DOUBLE PRECISION G1,G2,G3,COEF1,COEF2,G123
C
C=======================================================================
C
      XSUR12 = XMUL / 12.D0
      XSUR48 = XMUL / 48.D0
C
C     EXTRACTS THE TYPE OF ELEMENT FOR VISCOSITY
C
      IELMNU=SU%ELM
      IELMNV=SV%ELM
      ISO = SU%DIM2
C
      IF(IELMNU.EQ.10.AND.ISO.EQ.1) THEN
C
C-----------------------------------------------------------------------
C
C  P0 DISCRETISATION FOR ISOTROPIC VISCOSITY:
C
      DO 4 IELEM = 1 , NELEM
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
         S2D   = XMUL*0.25D0/SURFAC(IELEM)
         VISC1 = U(IELEM)*S2D
         BPE   = VISC1*(Y3**2 + X3**2)
         CPF   = VISC1*(Y2*Y3 + X2*X3)
         APD   = VISC1*(Y2**2 + X2**2)
C
C  DIAGONAL TERMS
C
         A11(IELEM)= APD+BPE-2*CPF
         A22(IELEM)= BPE
         A33(IELEM)= APD
C
C  EXTRADIAGONAL TERMS
C
         A12(IELEM)= CPF-BPE
         A13(IELEM)= CPF-APD
         A23(IELEM)=-CPF
C
C   END OF THE LOOP ON THE ELEMENTS
C
4     CONTINUE
C
C-----------------------------------------------------------------------
C
C  P0 DISCRETISATIO FOR NONISOTROPIC VISCOSITY
C
      ELSEIF(IELMNU.EQ.10.AND.ISO.EQ.3) THEN
C
C-----------------------------------------------------------------------
C
C  P0 DISCRETISATION FOR ISOTROPIC VISCOSITY:
C
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
C  INITIALISES THE INTERMEDIATE VARIABLES
C
         S2D   = XMUL*0.25D0/SURFAC(IELEM)
         VISC1 = U(         IELEM)*S2D
         VISC2 = U(  NELMAX+IELEM)*S2D
         VISC3 = U(2*NELMAX+IELEM)*S2D
         BPE   = VISC1*Y3**2 + VISC2*X3**2
         CPF   = VISC1*Y2*Y3 + VISC2*X2*X3
         APD   = VISC1*Y2**2 + VISC2*X2**2
         X2MX3  = X2-X3
         Y2MY3  = Y2-Y3
C
C  EXTRADIAGONAL TERMS
C
         A12(IELEM)= CPF-BPE   - ( Y2MY3*X3 + X2MX3*Y3 ) * VISC3
         A13(IELEM)= CPF-APD   + ( Y2MY3*X2 + X2MX3*Y2 ) * VISC3
         A23(IELEM)= -CPF      + ( X2*Y3    + X3*Y2    ) * VISC3
C
C  DIAGONAL TERMS
C
         A11(IELEM) = - A12(IELEM) - A13(IELEM)
         A22(IELEM) = - A12(IELEM) - A23(IELEM)
         A33(IELEM) = - A13(IELEM) - A23(IELEM)
C
C   END OF THE LOOP ON THE ELEMENTS
C
6     CONTINUE
C
C-----------------------------------------------------------------------
C
      ELSEIF(IELMNU.EQ.11.AND.ISO.EQ.1) THEN
C
C-----------------------------------------------------------------------
C
      IF(FORMUL(7:8).EQ.'UV'.AND.
     &   IELMNU.EQ.11.AND.IELMNV.EQ.11) THEN
C
C-----------------------------------------------------------------------
C
      DO IELEM = 1 , NELEM
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
        I1=IKLE1(IELEM)
        I2=IKLE2(IELEM)
        I3=IKLE3(IELEM)
C
        G1=V(I1)
        G2=V(I2)
        G3=V(I3)
C
        COEF1=XSUR48/SURFAC(IELEM)
        G123=G1+G2+G3
        COEF2=U(I1)*(G1+G123)+U(I2)*(G2+G123)+U(I3)*(G3+G123)
C
C  EXTRADIAGONAL TERMS
C
        A12(IELEM)= (Y3*(Y2-Y3)+X3*(X2-X3))*COEF2*COEF1
        A13(IELEM)=-(Y2*(Y2-Y3)+X2*(X2-X3))*COEF2*COEF1
        A23(IELEM)=-(Y2*    Y3 +X2*    X3 )*COEF2*COEF1
C
C  DIAGONAL TERMS
C
        A11(IELEM) = - A12(IELEM) - A13(IELEM)
        A22(IELEM) = - A12(IELEM) - A23(IELEM)
        A33(IELEM) = - A13(IELEM) - A23(IELEM)
C
      ENDDO
C
C   END OF THE LOOP ON THE ELEMENTS
C
      ELSE
C
C  P1 DISCRETISATION FOR VISCOSITY:
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
C   INITIALISES THE INTERMEDIATE VARIABLES
C
         SOMVX = ( U(IKLE1(IELEM))
     &            +U(IKLE2(IELEM))
     &            +U(IKLE3(IELEM)) ) * XSUR12 / SURFAC(IELEM)
         X2X3  = X2 * X3  + Y2 * Y3
         X2AUX = X2*(-X2+X3)+Y2*(-Y2+Y3)
         X3AUX = X3*(-X2+X3)+Y3*(-Y2+Y3)
C
C  EXTRADIAGONAL TERMS
C
         A12(IELEM) = - SOMVX * X3AUX
         A13(IELEM) =   SOMVX * X2AUX
         A23(IELEM) = - SOMVX * X2X3
C
C  DIAGONAL TERMS
C
         A11(IELEM) = - A12(IELEM) - A13(IELEM)
         A22(IELEM) = - A12(IELEM) - A23(IELEM)
         A33(IELEM) = - A13(IELEM) - A23(IELEM)
C
5     CONTINUE
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C  LINEAR DISCRETISATION FOR NONISOTROPIC VISCOSITY
C
      ELSEIF(IELMNU.EQ.11.AND.ISO.EQ.3) THEN
C
C-----------------------------------------------------------------------
C
C  P1 DISCRETISATION FOR VISCOSITY:
C
      IAD2 = SU%MAXDIM1
      IAD3 = 2*IAD2
C
      DO 7 IELEM = 1 , NELEM
C
C  INITIALISES THE GEOMETRICAL VARIABLES
C
         X2  =  XEL(IELEM,2)
         X3  =  XEL(IELEM,3)
C
         Y2  =  YEL(IELEM,2)
         Y3  =  YEL(IELEM,3)
C
C  INITIALISES THE INTERMEDIATE VARIABLES
C
         SOMVX = U(IKLE1(IELEM)     )
     &         + U(IKLE2(IELEM)     )
     &         + U(IKLE3(IELEM)     )
         SOMVY = U(IKLE1(IELEM)+IAD2)
     &         + U(IKLE2(IELEM)+IAD2)
     &         + U(IKLE3(IELEM)+IAD2)
         SOMVZ = U(IKLE1(IELEM)+IAD3)
     &         + U(IKLE2(IELEM)+IAD3)
     &         + U(IKLE3(IELEM)+IAD3)
C
C   INITIALISES THE INTERMEDIATE VARIABLES
C
         AUX = XSUR12 / SURFAC(IELEM)
         X2X3 = X2 * X3
         Y2Y3 = Y2 * Y3
         X2AUX = X2*(-X2+X3)
         Y2AUX = Y2*(-Y2+Y3)
         X3AUX = X3*(-X2+X3)
         Y3AUX = Y3*(-Y2+Y3)
         X2MX3 = X2-X3
         Y2MY3 = Y2-Y3
C
C  EXTRADIAGONAL TERMS
C
         A12(IELEM) = (   - SOMVX * Y3AUX
     &                    - SOMVY * X3AUX
     &                    - Y2MY3*X3*SOMVZ
     &                    - X2MX3*Y3*SOMVZ ) * AUX
C
         A13(IELEM) = (     SOMVX * Y2AUX
     &                    + SOMVY * X2AUX
     &                    + Y2MY3*X2*SOMVZ
     &                    + X2MX3*Y2*SOMVZ ) * AUX
C
         A23(IELEM) = (   - SOMVX * Y2Y3
     &                    - SOMVY * X2X3
     &                    + SOMVZ*X3*Y2+SOMVZ*X2*Y3 ) * AUX
C
C  DIAGONAL TERMS
C
         A11(IELEM) = - A12(IELEM) - A13(IELEM)
         A22(IELEM) = - A12(IELEM) - A23(IELEM)
         A33(IELEM) = - A13(IELEM) - A23(IELEM)
C
7     CONTINUE
C
C-----------------------------------------------------------------------
C
      ELSE
C
        IF (LNG.EQ.1) WRITE(LU,10) IELMNU,ISO
        IF (LNG.EQ.2) WRITE(LU,11) IELMNU,ISO
10      FORMAT(1X,'MT02AA (BIEF) : TYPE DE VISCOSITE NON PREVU : ',2I6)
11      FORMAT(1X,
     &  'MT02AA (BIEF) : TYPE OF VISCOSITY NOT AVAILABLE : ',2I6)
        CALL PLANTE(1)
        STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(FORMUL(14:16).EQ.'MON') THEN
        IF(XMUL.GT.0.D0) THEN
          DO IELEM=1,NELEM
            A12(IELEM)=MIN(A12(IELEM),0.D0)
            A13(IELEM)=MIN(A13(IELEM),0.D0)
            A23(IELEM)=MIN(A23(IELEM),0.D0)
C           DIAGONAL TERMS REDONE
            A11(IELEM) = - A12(IELEM) - A13(IELEM)
            A22(IELEM) = - A12(IELEM) - A23(IELEM)
            A33(IELEM) = - A13(IELEM) - A23(IELEM)
          ENDDO
        ELSE
          DO IELEM=1,NELEM
            A12(IELEM)=MAX(A12(IELEM),0.D0)
            A13(IELEM)=MAX(A13(IELEM),0.D0)
            A23(IELEM)=MAX(A23(IELEM),0.D0)
C           DIAGONAL TERMS REDONE
            A11(IELEM) = - A12(IELEM) - A13(IELEM)
            A22(IELEM) = - A12(IELEM) - A23(IELEM)
            A33(IELEM) = - A13(IELEM) - A23(IELEM)
          ENDDO
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C