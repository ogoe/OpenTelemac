C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS COEFFICIENTS LAMBDA(I,J) FOR N-TYPE MURD SCHEME.
!><br>            THE ELEMENT IS THE P1 PRISM.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE JACOBIAN MUST BE POSITIVE

!>  @reference J-M HERVOUET THESIS OR BOOK: MURD SCHEME IN 3 DIMENSIONS
!>             CHAPTER 6.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> F, G, H, IKLE, LEGO, NELEM, NELMAX, PPQ, SF, SG, SH, SIGMAG, SPECAD, SU, SURFAC, SV, SW, T, U, V, W, X, XM, XMUL, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ALFA, ALFA1, ALFA2, ALFAI0, ALFAI3, ALFAII, ALFAIJ, ALFAIK, ALFAJ, ALFAJ0, ALFAJ3, ALFAJI, ALFAJJ, ALFAJK, ALFAK, ALFAK0, ALFAK3, ALFAKI, ALFAKJ, ALFAKK, BETA, DELTA, EPS, FORMUL, I0, I3, IELEM, IELMW, INDIC, IPLUS1, IPLUS3, IXM, J0, J3, K0, K3, LI0I3, LI0J0, LI0K0, LI3I0, LI3J0, LI3J3, LI3K0, LI3K3, LJ0I0, LJ0K0, LJ3I0, LJ3I3, LJ3J0, LJ3K0, LJ3K3, LK0I0, LK0J0, LK3I0, LK3I3, LK3J0, LK3J3, LK3K0, SOM0, SOM3, XSUR3
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MT14PP
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> VC04PP()
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
!>      <td><center> 6.0                                       </center>
!> </td><td> 27/04/2010
!> </td><td> J-M HERVOUET / A. DECOENE (LNHE)
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 04/08/2008
!> </td><td> JMH
!> </td><td> INVERTED DIMENSIONS OF XM (SEE ALSO MURD3D)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 21/10/2004
!> </td><td> JMH
!> </td><td> MASS-LUMPING FOR COMPATIBILITY WITH NEW VERSION OF TRIDW2,
!> <br>      ALL OCCURRENCES OF F123+W(...) ARE REPLACED BY 4*W(...)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 16/08/1999
!> </td><td> JMH
!> </td><td> EPS ADDED FOR THE DIVISION BY 0 TESTS
!> <br>     (WAS IF(ALFA.GT.0.D0), ETC BEFORE)
!> <br>      EPS=1.D-10 FOR THE TIME BEING, TO BE DISCUSSED
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 28/11/1994
!> </td><td> J-M JANIN (LNH)
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
!>          <tr><td>F,G,H
!></td><td>--></td><td>FONCTIONS INTERVENANT DANS LE CALCUL DE LA
!>                  MATRICE.
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE1
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
!>    </td></tr>
!>          <tr><td>LEGO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>PPQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SF,SG,SH
!></td><td>--></td><td>STRUCTURES DE F,G ET H.
!>    </td></tr>
!>          <tr><td>SIGMAG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SPECAD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SU,SV,SW
!></td><td>--></td><td>STRUCTURES DE U,V ET W.
!>    </td></tr>
!>          <tr><td>SURFAC
!></td><td>--></td><td>SURFACE DES TRIANGLES.
!>    </td></tr>
!>          <tr><td>T
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>COMPOSANTES D'UN VECTEUR INTERVENANT DANS LE
!>                  CALCUL DE LA MATRICE.
!>    </td></tr>
!>          <tr><td>X,Y,Z
!></td><td>--></td><td>COORDONNEES DES POINTS DANS L'ELEMENT
!>    </td></tr>
!>          <tr><td>XM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>FACTEUR MULTIPLICATIF
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MT14PP
     &( T,XM,PPQ,LEGO,XMUL,SU,SV,SW,U,V,W,SF,SG,SH,F,G,H,
     &  X,Y,Z,SURFAC,IKLE,NELEM,NELMAX,SIGMAG,SPECAD)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A11,A12        |<--| ELEMENTS DE LA MATRICE
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LE CALCUL DE LA
C|                |   | MATRICE.
C| IKLE           |---| 
C| IKLE1          |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| LEGO           |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| PPQ            |---| 
C| SF,SG,SH       |-->| STRUCTURES DE F,G ET H.
C| SIGMAG         |---| 
C| SPECAD         |---| 
C| SU,SV,SW       |-->| STRUCTURES DE U,V ET W.
C| SURFAC         |-->| SURFACE DES TRIANGLES.
C| T             |---| 
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR INTERVENANT DANS LE
C|                |   | CALCUL DE LA MATRICE.
C| X,Y,Z          |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XM             |---| 
C| XMUL           |-->| FACTEUR MULTIPLICATIF
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_MT14PP => MT14PP
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE(NELMAX,6),PPQ(6,6)
C
      DOUBLE PRECISION, INTENT(INOUT) :: T(NELMAX,6),XM(30,NELMAX)
C
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: U(*),V(*),W(*),F(*),G(*),H(*)
C
      LOGICAL, INTENT(IN) :: LEGO,SIGMAG,SPECAD
C
C     STRUCTURES OF U, V, W
C
      TYPE(BIEF_OBJ), INTENT(IN) :: SU,SV,SW,SF,SG,SH
C
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),Z(*)
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
C
      INTEGER IELEM,IELMW
      DOUBLE PRECISION XSUR3
      DOUBLE PRECISION LI0J0,LI0K0,LJ0I0,LJ0K0,LK0I0,LK0J0
      DOUBLE PRECISION LI3J3,LI3K3,LJ3I3,LJ3K3,LK3I3,LK3J3
      DOUBLE PRECISION LI3J0,LI3K0,LJ3I0,LJ3K0,LK3I0,LK3J0
      DOUBLE PRECISION LI3I0,LJ3J0,LK3K0,LI0I3
      DOUBLE PRECISION ALFA,ALFAJ,ALFAK,ALFA1,ALFA2,BETA,SOM0,SOM3
      DOUBLE PRECISION ALFAI0,ALFAJ0,ALFAK0,ALFAI3,ALFAJ3,ALFAK3
      DOUBLE PRECISION ALFAII,ALFAIJ,ALFAIK,ALFAJI,ALFAJJ,ALFAJK
      DOUBLE PRECISION ALFAKI,ALFAKJ,ALFAKK,EPS,DELTA
      DATA EPS /1.D-10/
C
      INTEGER IPLUS1(6),IPLUS3(6),INDIC(0:7)
      INTEGER IXM,I0,J0,K0,I3,J3,K3
C
      CHARACTER(LEN=16) :: FORMUL
C
C-----------------------------------------------------------------------
C
C     DATA
C
      DATA IPLUS1 / 2 , 3 , 1 , 5 , 6 , 4 /
      DATA IPLUS3 / 4 , 5 , 6 , 1 , 2 , 3 /
      DATA INDIC  / 4 , 4 , 5 , 3 , 6 , 2 , 1 , 1 /
C
C=======================================================================
C
      IELMW = SW%ELM
C                                                         *   *
C     COMPUTES THE COEFFICIENTS A(I) (INTEGRAL OF H U.GRAD(PSI), ONLY
C     CONSIDERING THE HORIZONTAL PART OF U). SEE JMH THESIS
C
C     NOTE: THIS VC04PP CALL IS ALREADY DONE BY FLUX3D IN TELEMAC-3D
C           (THROUGH A CALL TO VECTOR, BUT T IS AT THAT TIME MESH3D%W
C            AND IS NOT KEPT). OPTIMISATION MAY BE POSSIBLE.
C
      FORMUL='             HOR'
      CALL VC04PP(XMUL,SU,SV,SW,U,V,W,SF,SG,SH,F,G,H,X,Y,Z,
     & IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),IKLE(1,5),IKLE(1,6),
     & NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4),T(1,5),T(1,6),
     & SPECAD,FORMUL)
C     FORMUL NORMALLY STARTS WITH VGRADP OR VGRADP2, OR VGRADP22 TO KNOW
C     SIGMAG AND SPECAD, USELESS HERE.
      XSUR3 = XMUL/3.D0
C
C-----------------------------------------------------------------------
C
      IF(IELMW.NE.41) THEN
        STOP 'MT14PP DISCRETISATION NON PREVUE'
      ENDIF
C
C     COMPUTES THE COEFFICIENTS B(I) BY ELEMENTS
C     AND COMPUTES LAMBDA(I,J)
C
      DO 1 IELEM = 1 , NELEM
C
         IXM = 0
         IF (W(IKLE(IELEM,1)).GT.0.D0) IXM = 1
         IF (W(IKLE(IELEM,2)).GT.0.D0) IXM = IXM + 2
         IF (W(IKLE(IELEM,3)).GT.0.D0) IXM = IXM + 4
C
C        NOTE JMH:
C        IT SEEMS THAT INDIC IS THE POINT THAT RECEIVES THE VERTICAL
C        VELOCITY WHICH IS THE ONLY ONE OF ITS SIGN
C
C        ALL W
C        ONLY W1 > 0       INDIC = 4
C        ONLY W2 > 0       INCIC = 5
C        ONLY W3 > 0       INDIC = 6
C        ALL W > 0         INDIC = 1 (RANDOM, WHY NOT 4 ?)
C        ONLY W1
C        ONLY W2
C        ONLY W3
C
         I0 = INDIC(IXM)
         J0 = IPLUS1(I0)
         K0 = IPLUS1(J0)
         I3 = IPLUS3(I0)
         J3 = IPLUS1(I3)
         K3 = IPLUS1(J3)
C
         ALFA = XSUR3 * SURFAC(IELEM)
C
         LI3I0 = ALFA * ABS(W(IKLE(IELEM,MIN(I0,I3))))
         LI0I3 = 0.D0
         IF (IXM.GE.1.AND.IXM.LE.6) THEN
            LI0I3 = LI3I0
            LI3I0 = 0.D0
         ENDIF
         LJ3J0 = ALFA * ABS(W(IKLE(IELEM,MIN(J0,J3))))
         XM(PPQ(J0,J3),IELEM) = 0.D0
         LK3K0 = ALFA * ABS(W(IKLE(IELEM,MIN(K0,K3))))
         XM(PPQ(K0,K3),IELEM) = 0.D0
C
         ALFAI0 = -T(IELEM,I0)
         ALFAJ0 = -T(IELEM,J0)
         ALFAK0 = -T(IELEM,K0)
         ALFAI3 = -T(IELEM,I3)
         ALFAJ3 = -T(IELEM,J3)
         ALFAK3 = -T(IELEM,K3)
C
         LJ0I0 = MAX(0.D0,MIN(ALFAI0,-ALFAJ0))
         LK0I0 = MAX(0.D0,MIN(ALFAI0,-ALFAK0))
         LI3J3 = MAX(0.D0,MIN(ALFAJ3,-ALFAI3))
         LI3K3 = MAX(0.D0,MIN(ALFAK3,-ALFAI3))
C
         ALFAJ = MIN(LJ3J0,MAX(LI3J3,LJ0I0))
         ALFAK = MIN(LK3K0,MAX(LI3K3,LK0I0))
         ALFA  = MIN(LI0I3,ALFAJ+ALFAK)
         IF (ALFA.GT.EPS) THEN
!
C           JMH CODE
!
C           IF(ALFA.LT.ALFAJ+ALFAK) THEN
C             LIMITATION OF ALFAJ AND ALFAK WITH THE IDEA THAT THE
C             TWO CORRECTED FLUXES WILL BE AS EQUAL AS POSSIBLE
C             MOREOVER WE MUST HAVE ALFAJ+ALFAK=ALFA
C             IF(ALFAK.GT.ALFAJ) THEN
C               DELTA=MIN(ALFA,ALFAK-ALFAJ)
C               ALFAK=0.5D0*(ALFA+DELTA)
C               ALFAJ=0.5D0*(ALFA-DELTA)
C             ELSE
C               DELTA=MIN(ALFA,ALFAJ-ALFAK)
C               ALFAK=0.5D0*(ALFA-DELTA)
C               ALFAJ=0.5D0*(ALFA+DELTA)
C             ENDIF
C           ENDIF
!
C           JMJ CODE
!
            BETA = ALFA / (ALFAJ+ALFAK)
            ALFAJ = ALFAJ * BETA
            ALFAK = ALFAK * BETA
C
            LI0I3 = LI0I3-ALFA
            LJ3J0 = LJ3J0-ALFAJ
            LK3K0 = LK3K0-ALFAK
C
            ALFAI3 = ALFAI3+ALFA
            ALFAI0 = ALFAI0-ALFA
            ALFAJ0 = ALFAJ0+ALFAJ
            ALFAJ3 = ALFAJ3-ALFAJ
            ALFAK0 = ALFAK0+ALFAK
            ALFAK3 = ALFAK3-ALFAK
C
            LJ0I0 = MAX(0.D0,MIN(ALFAI0,-ALFAJ0))
            LK0I0 = MAX(0.D0,MIN(ALFAI0,-ALFAK0))
            LI3J3 = MAX(0.D0,MIN(ALFAJ3,-ALFAI3))
            LI3K3 = MAX(0.D0,MIN(ALFAK3,-ALFAI3))
C
         ENDIF
C
         LI0J0 = MAX(0.D0,MIN(ALFAJ0,-ALFAI0))
         LK0J0 = MAX(0.D0,MIN(ALFAJ0,-ALFAK0))
         LI0K0 = MAX(0.D0,MIN(ALFAK0,-ALFAI0))
         LJ0K0 = MAX(0.D0,MIN(ALFAK0,-ALFAJ0))
         LJ3I3 = MAX(0.D0,MIN(ALFAI3,-ALFAJ3))
         LJ3K3 = MAX(0.D0,MIN(ALFAK3,-ALFAJ3))
         LK3I3 = MAX(0.D0,MIN(ALFAI3,-ALFAK3))
         LK3J3 = MAX(0.D0,MIN(ALFAJ3,-ALFAK3))
C
         XM(PPQ(J0,I3),IELEM) = 0.D0
         XM(PPQ(K0,I3),IELEM) = 0.D0
         XM(PPQ(I0,J3),IELEM) = 0.D0
         XM(PPQ(K0,J3),IELEM) = 0.D0
         XM(PPQ(I0,K3),IELEM) = 0.D0
         XM(PPQ(J0,K3),IELEM) = 0.D0
C
         ALFAI0 = 0.D0
         ALFAJ0 = 0.D0
         ALFAK0 = 0.D0
         ALFAI3 = 0.D0
         ALFAJ3 = 0.D0
         ALFAK3 = 0.D0
C
         SOM0 = LJ0I0+LK0I0
         SOM3 = LI3J3+LI3K3
C
         ALFA1 = MIN(LI0I3,SOM0)
         IF (ALFA1.GT.EPS) THEN
C
            BETA = 1.D0 / SOM0
            ALFAJ0 = LJ0I0 * BETA
            ALFAK0 = LK0I0 * BETA
            ALFA = MAX(0.D0,ALFA1-SOM3)
C
            LJ0I0 = LJ0I0 - ALFAJ0*ALFA1
            LK0I0 = LK0I0 - ALFAK0*ALFA1
            XM(PPQ(J0,I3),IELEM) = ALFAJ0*ALFA
            XM(PPQ(K0,I3),IELEM) = ALFAK0*ALFA
C
         ENDIF
C
         ALFA2 = MIN(LI0I3,SOM3)
         IF (ALFA2.GT.EPS) THEN
C
            BETA = 1.D0 / SOM3
            ALFAJ3 = LI3J3 * BETA
            ALFAK3 = LI3K3 * BETA
            ALFA = MAX(0.D0,ALFA2-SOM0)
C
            LI3J3 = LI3J3 - ALFAJ3*ALFA2
            LI3K3 = LI3K3 - ALFAK3*ALFA2
            XM(PPQ(I0,J3),IELEM) = ALFAJ3*ALFA
            XM(PPQ(I0,K3),IELEM) = ALFAK3*ALFA
C
         ENDIF
C
         ALFA = MIN(ALFA1,ALFA2)
         IF (ALFA.GT.0.D0) THEN
C
            ALFAJ3 = ALFAJ3 * ALFA
            ALFAK3 = ALFAK3 * ALFA
            ALFAJJ = ALFAJ3 * ALFAJ0
            ALFAKK = ALFAK3 * ALFAK0
            ALFAJK = ALFAJ3 * ALFAK0
            ALFAKJ = ALFAK3 * ALFAJ0
            ALFA = MIN(ALFAJK,ALFAKJ)
C
            XM(PPQ(J0,J3),IELEM) = ALFAJJ + ALFA
            XM(PPQ(K0,K3),IELEM) = ALFAKK + ALFA
            XM(PPQ(K0,J3),IELEM) = ALFAJK - ALFA
            XM(PPQ(J0,K3),IELEM) = ALFAKJ - ALFA
C
         ENDIF
C
         XM(PPQ(I0,I3),IELEM) = MAX(0.D0,LI0I3-MAX(SOM3,SOM0))
C
         LJ3I0 = 0.D0
         LK3I0 = 0.D0
         LI3J0 = 0.D0
         LK3J0 = 0.D0
         LI3K0 = 0.D0
         LJ3K0 = 0.D0
C
         SOM0 = LI0J0+LI0K0
         SOM3 = LJ3I3+LK3I3
C
         ALFA1 = MIN(LI3I0,SOM0)
         IF (ALFA1.GT.EPS) THEN
C
            BETA = 1.D0 / SOM0
            ALFAJ0 = LI0J0 * BETA
            ALFAK0 = LI0K0 * BETA
            ALFA = MAX(0.D0,ALFA1-SOM3)
C
            LI0J0 = LI0J0 - ALFAJ0*ALFA1
            LI0K0 = LI0K0 - ALFAK0*ALFA1
            LI3J0 = LI3J0 + ALFAJ0*ALFA
            LI3K0 = LI3K0 + ALFAK0*ALFA
C
         ENDIF
C
         XM(PPQ(I0,J0),IELEM) = LI0J0
         XM(PPQ(I0,K0),IELEM) = LI0K0
C
         ALFA2 = MIN(LI3I0,SOM3)
         IF (ALFA2.GT.EPS) THEN
C
            BETA = 1.D0 / SOM3
            ALFAJ3 = LJ3I3 * BETA
            ALFAK3 = LK3I3 * BETA
            ALFA = MAX(0.D0,ALFA2-SOM0)
C
            LJ3I3 = LJ3I3 - ALFAJ3*ALFA2
            LK3I3 = LK3I3 - ALFAK3*ALFA2
            LJ3I0 = LJ3I0 + ALFAJ3*ALFA
            LK3I0 = LK3I0 + ALFAK3*ALFA
C
         ENDIF
C
         XM(PPQ(J3,I3),IELEM) = LJ3I3
         XM(PPQ(K3,I3),IELEM) = LK3I3
C
         ALFA = MIN(ALFA1,ALFA2)
         IF (ALFA.GT.0.D0) THEN
C
            ALFAJ0 = ALFAJ0 * ALFA
            ALFAK0 = ALFAK0 * ALFA
            ALFAJJ = ALFAJ0 * ALFAJ3
            ALFAKK = ALFAK0 * ALFAK3
            ALFAJK = ALFAJ0 * ALFAK3
            ALFAKJ = ALFAK0 * ALFAJ3
            ALFA = MIN(ALFAJK,ALFAKJ)
C
            LJ3J0 = LJ3J0 + ALFAJJ + ALFA
            LK3K0 = LK3K0 + ALFAKK + ALFA
            LK3J0 = LK3J0 + ALFAJK - ALFA
            LJ3K0 = LJ3K0 + ALFAKJ - ALFA
C
         ENDIF
C
         LI3I0 = MAX(0.D0,LI3I0-MAX(SOM0,SOM3))
C
         SOM0 = LJ0I0+LJ0K0
         SOM3 = LI3J3+LK3J3
C
         ALFA1 = MIN(LJ3J0,SOM0)
         IF (ALFA1.GT.EPS) THEN
C
            BETA = 1.D0 / SOM0
            ALFAI0 = LJ0I0 * BETA
            ALFAK0 = LJ0K0 * BETA
            ALFA = MAX(0.D0,ALFA1-SOM3)
C
            LJ0I0 = LJ0I0 - ALFAI0*ALFA1
            LJ0K0 = LJ0K0 - ALFAK0*ALFA1
            LJ3I0 = LJ3I0 + ALFAI0*ALFA
            LJ3K0 = LJ3K0 + ALFAK0*ALFA
C
         ENDIF
C
         XM(PPQ(J0,I0),IELEM) = LJ0I0
         XM(PPQ(J0,K0),IELEM) = LJ0K0
C
         ALFA2 = MIN(LJ3J0,SOM3)
         IF (ALFA2.GT.EPS) THEN
C
            BETA = 1.D0 / SOM3
            ALFAI3 = LI3J3 * BETA
            ALFAK3 = LK3J3 * BETA
            ALFA = MAX(0.D0,ALFA2-SOM0)
C
            LI3J3 = LI3J3 - ALFAI3*ALFA2
            LK3J3 = LK3J3 - ALFAK3*ALFA2
            LI3J0 = LI3J0 + ALFAI3*ALFA
            LK3J0 = LK3J0 + ALFAK3*ALFA
C
         ENDIF
C
         XM(PPQ(I3,J3),IELEM) = LI3J3
         XM(PPQ(K3,J3),IELEM) = LK3J3
C
         ALFA = MIN(ALFA1,ALFA2)
         IF (ALFA.GT.0.D0) THEN
C
            ALFAI0 = ALFAI0 * ALFA
            ALFAK0 = ALFAK0 * ALFA
            ALFAII = ALFAI0 * ALFAI3
            ALFAKK = ALFAK0 * ALFAK3
            ALFAIK = ALFAI0 * ALFAK3
            ALFAKI = ALFAK0 * ALFAI3
            ALFA = MIN(ALFAIK,ALFAKI)
C
            LI3I0 = LI3I0 + ALFAII + ALFA
            LK3K0 = LK3K0 + ALFAKK + ALFA
            LK3I0 = LK3I0 + ALFAIK - ALFA
            LI3K0 = LI3K0 + ALFAKI - ALFA
C
         ENDIF
C
         LJ3J0 = MAX(0.D0,LJ3J0-MAX(SOM0,SOM3))
C
         SOM0 = LK0I0+LK0J0
         SOM3 = LI3K3+LJ3K3
C
         ALFA1 = MIN(LK3K0,SOM0)
         IF (ALFA1.GT.EPS) THEN
C
            BETA = 1.D0 / SOM0
            ALFAI0 = LK0I0 * BETA
            ALFAJ0 = LK0J0 * BETA
            ALFA = MAX(0.D0,ALFA1-SOM3)
C
            LK0I0 = LK0I0 - ALFAI0*ALFA1
            LK0J0 = LK0J0 - ALFAJ0*ALFA1
            LK3I0 = LK3I0 + ALFAI0*ALFA
            LK3J0 = LK3J0 + ALFAJ0*ALFA
C
         ENDIF
C
         XM(PPQ(K0,I0),IELEM) = LK0I0
         XM(PPQ(K0,J0),IELEM) = LK0J0
C
         ALFA2 = MIN(LK3K0,SOM3)
         IF (ALFA2.GT.EPS) THEN
C
            BETA = 1.D0 / SOM3
            ALFAI3 = LI3K3 * BETA
            ALFAJ3 = LJ3K3 * BETA
            ALFA = MAX(0.D0,ALFA2-SOM0)
C
            LI3K3 = LI3K3 - ALFAI3*ALFA2
            LJ3K3 = LJ3K3 - ALFAJ3*ALFA2
            LI3K0 = LI3K0 + ALFAI3*ALFA
            LJ3K0 = LJ3K0 + ALFAJ3*ALFA
C
         ENDIF
C
         XM(PPQ(I3,K3),IELEM) = LI3K3
         XM(PPQ(J3,K3),IELEM) = LJ3K3
C
         ALFA = MIN(ALFA1,ALFA2)
         IF (ALFA.GT.0.D0) THEN
C
            ALFAI0 = ALFAI0 * ALFA
            ALFAJ0 = ALFAJ0 * ALFA
            ALFAII = ALFAI0 * ALFAI3
            ALFAJJ = ALFAJ0 * ALFAJ3
            ALFAIJ = ALFAI0 * ALFAJ3
            ALFAJI = ALFAJ0 * ALFAI3
            ALFA = MIN(ALFAIJ,ALFAJI)
C
            LI3I0 = LI3I0 + ALFAII + ALFA
            LJ3J0 = LJ3J0 + ALFAJJ + ALFA
            LJ3I0 = LJ3I0 + ALFAIJ - ALFA
            LI3J0 = LI3J0 + ALFAJI - ALFA
C
         ENDIF
C
         LK3K0 = MAX(0.D0,LK3K0-MAX(SOM0,SOM3))
C
         XM(PPQ(I3,I0),IELEM) = LI3I0
         XM(PPQ(J3,I0),IELEM) = LJ3I0
         XM(PPQ(K3,I0),IELEM) = LK3I0
         XM(PPQ(I3,J0),IELEM) = LI3J0
         XM(PPQ(J3,J0),IELEM) = LJ3J0
         XM(PPQ(K3,J0),IELEM) = LK3J0
         XM(PPQ(I3,K0),IELEM) = LI3K0
         XM(PPQ(J3,K0),IELEM) = LJ3K0
         XM(PPQ(K3,K0),IELEM) = LK3K0
C
1     CONTINUE
C
C-----------------------------------------------------------------------
C
C     COMPUTES THE SUM OF EACH ROW (WITH A - SIGN)
C     THE DIAGONAL TERMS ARE 0
C
      IF(LEGO) THEN
C
        DO IELEM = 1,NELEM
C
            T(IELEM,1) = -XM(01,IELEM)-XM(02,IELEM)
     &                   -XM(03,IELEM)-XM(04,IELEM)-XM(05,IELEM)
            T(IELEM,2) = -XM(16,IELEM)-XM(06,IELEM)
     &                   -XM(07,IELEM)-XM(08,IELEM)-XM(09,IELEM)
            T(IELEM,3) = -XM(17,IELEM)-XM(21,IELEM)
     &                   -XM(10,IELEM)-XM(11,IELEM)-XM(12,IELEM)
            T(IELEM,4) = -XM(18,IELEM)-XM(22,IELEM)
     &                   -XM(25,IELEM)-XM(13,IELEM)-XM(14,IELEM)
            T(IELEM,5) = -XM(19,IELEM)-XM(23,IELEM)
     &                   -XM(26,IELEM)-XM(28,IELEM)-XM(15,IELEM)
            T(IELEM,6) = -XM(20,IELEM)-XM(24,IELEM)
     &                   -XM(27,IELEM)-XM(29,IELEM)-XM(30,IELEM)
C
        ENDDO
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