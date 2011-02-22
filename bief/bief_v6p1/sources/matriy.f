C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       OPERATIONS BETWEEN MATRICES.
!><br>            THE MATRIX IS IDENTIFIED BY THE FORMULATION IN
!>                CHARACTER STRING FORMUL.
!>  @code
!>-----------------------------------------------------------------------
!>
!>  MEANING OF IELM AND IELM2
!>
!>  TYPE OF ELEMENT      NUMBER OF POINTS
!>
!>  A OR 11 : P1 TRIANGLE            3
!>  B OR 12 : QUASI-BUBBLE TRIANGLE  4
!>  C OR 13 : P1-ISO P1 TRIANGLE     6
!>  D OR 14 : P2 TRIANGLE            7
!>  E       : NOTHING FOR NOW
!>
!>  F OR 21 : Q1 QUADRILATERAL       4
!>  G OR 22 : Q2 QUADRILATERAL       8
!>  H OR 24 : Q2 QUADRILATERAL       9
!>
!>  T OR 31 : P1 TETRAHEDRON         4
!>
!>  P OR 41 : TELEMAC-3D PRISMS      6
!>
!>-----------------------------------------------------------------------
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> F, FORMUL, G, H, IELM1, IELM2, IKLE, LEGO, NBOR, NELEM, NELMAX, S, SF, SG, SH, SU, SURFAC, SV, SW, T, TYPDIA, TYPEXT, U, V, W, XEL, XM, XMUL, YEL, ZEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AAQ, AAS, ABQ, ACQ, BAQ, BBQ, BBS, CAQ, FFS, ICOORD, INCHYD, OOS, PPQ, PPS, SIGMAG, SPECAD, TDIA, TEXT
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MATRIY
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> MT01AA(), MT01BB(), MT01CC(), MT01OO(), MT01PP(), MT01TT(), MT02AA(), MT02AA_2(), MT02BB(), MT02CC(), MT02PP(), MT02TT(), MT03AA(), MT03BB(), MT03CC(), MT04AA(), MT04BB(), MT04CC(), MT04PP(), MT04TT(), MT05AA(), MT05BB(), MT05CC(), MT05PP(), MT05TT(), MT06AA(), MT06BB(), MT06CC(), MT06FF(), MT06FT(), MT06FT2(), MT06OC(), MT06OO(), MT06PP(), MT06TT(), MT07AA(), MT07BB(), MT07CC(), MT08AA(), MT08AB(), MT08AC(), MT08BA(), MT08BB(), MT08PP(), MT08TT(), MT11AA(), MT11AB(), MT11AC(), MT11BA(), MT11BB(), MT12AA(), MT12AB(), MT12AC(), MT12BA(), MT12BB(), MT13AA(), MT13AB(), MT13BA(), MT13BB(), MT13CA(), MT13CC(), MT14PP(), MT99AA(), MT99BB(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>MATRIX()

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
!> </td><td> 21/07/2008
!> </td><td> JM HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 16/09/2005
!> </td><td>
!> </td><td> MT04PP NOW IN 2 OPTIONS, AND WITHOUT VERTICAL UPWIND
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>F,G,H
!></td><td>--></td><td>FONCTIONS INTERVENANT DANS LA FORMULE
!>    </td></tr>
!>          <tr><td>FORMUL
!></td><td>--></td><td>FORMULE DECRIVANT LA MATRICE
!>    </td></tr>
!>          <tr><td>IELM1
!></td><td>--></td><td>TYPE D'ELEMENT POUR LES LIGNES
!>    </td></tr>
!>          <tr><td>IELM2
!></td><td>--></td><td>TYPE D'ELEMENT POUR LES COLONNES
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
!>    </td></tr>
!>          <tr><td>LEGO
!></td><td>--></td><td>LOGIQUE : POUR ASSEMBLER LA DIAGONALE
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!>                  (CAS DU MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>S
!></td><td>--></td><td>TYPE DE STOCKAGE DE LA MATRICE
!>    </td></tr>
!>          <tr><td>SF,SG,SH
!></td><td>--></td><td>STRUCTURES DES FONCTIONS F,G,H ET
!>    </td></tr>
!>          <tr><td>SU,SV,SW
!></td><td>--></td><td>U,V,H CI-DESSOUS.
!>    </td></tr>
!>          <tr><td>SURFAC
!></td><td>--></td><td>SURFACE DES ELEMENTS.
!>    </td></tr>
!>          <tr><td>T
!></td><td>--></td><td>TABLEAU DE TRAVAIL DE DIMENSION QUI
!>                  CONTIENDRA LA DIAGONALE NON ASSEMBLEE
!>    </td></tr>
!>          <tr><td>TYPDIA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TYPEXT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>COMPOSANTES D'UN VECTEUR U DANS LA FORMULE
!>    </td></tr>
!>          <tr><td>XEL,YEL,ZEL
!></td><td>--></td><td>COORDONNEES DES POINTS DANS L'ELEMENT
!>    </td></tr>
!>          <tr><td>XM
!></td><td><-></td><td>TERMES EXTRA-DIAGONAUX
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>COEFFICIENT MULTIPLICATEUR DU RESULTAT
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MATRIY
     &(FORMUL,XM,TYPDIA,TYPEXT,
     & XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,T,LEGO,
     & XEL,YEL,ZEL,SURFAC,IKLE,NBOR,
     & NELEM,NELMAX,IELM1,IELM2,S,NPLAN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LA FORMULE
C| FORMUL         |-->| FORMULE DECRIVANT LA MATRICE
C| IELM1          |-->| TYPE D'ELEMENT POUR LES LIGNES
C| IELM2          |-->| TYPE D'ELEMENT POUR LES COLONNES
C| IKLE           |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| LEGO           |-->| LOGIQUE : POUR ASSEMBLER LA DIAGONALE
C| NBOR           |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS DU MAILLAGE ADAPTATIF)
C| S             |-->| TYPE DE STOCKAGE DE LA MATRICE
C| SF,SG,SH       |-->| STRUCTURES DES FONCTIONS F,G,H ET
C| SU,SV,SW       |-->| U,V,H CI-DESSOUS.
C| SURFAC         |-->| SURFACE DES ELEMENTS.
C| T             |-->| TABLEAU DE TRAVAIL DE DIMENSION QUI
C|                |   | CONTIENDRA LA DIAGONALE NON ASSEMBLEE
C| TYPDIA         |---| 
C| TYPEXT         |---| 
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR U DANS LA FORMULE
C| XEL,YEL,ZEL    |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XM             |<->| TERMES EXTRA-DIAGONAUX
C| XMUL           |-->| COEFFICIENT MULTIPLICATEUR DU RESULTAT
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_MATRIY => MATRIY
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NELMAX,NELEM,IELM1,IELM2,S
      INTEGER, INTENT(IN)             :: NPLAN
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,*),NBOR(*)
      LOGICAL, INTENT(INOUT)          :: LEGO
      TYPE(BIEF_OBJ), INTENT(IN)      :: SF,SG,SH,SU,SV,SW
      DOUBLE PRECISION, INTENT(IN)    :: F(*),G(*),H(*),U(*),V(*),W(*)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XEL(*),YEL(*),ZEL(*)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
      DOUBLE PRECISION, INTENT(INOUT) :: XM(NELMAX,*),T(NELMAX,*)
      CHARACTER(LEN=16), INTENT(IN)   :: FORMUL
      CHARACTER(LEN=1), INTENT(INOUT) :: TYPDIA,TYPEXT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL SIGMAG,INCHYD,SPECAD
C
      CHARACTER(LEN=1) TDIA,TEXT
C
C-----------------------------------------------------------------------
C
      INTEGER ICOORD
C
      INTEGER AAQ(3,3,2),BBQ(4,4,2),ABQ(3,4,2),BAQ(4,3,2),PPQ(6,6,2)
      INTEGER AAS(3,3,2),BBS(4,4,2),OOS(2,2,2),FFS(4,4,2),PPS(6,6,2)
      INTEGER ACQ(3,6,2),CAQ(6,3,2)
C
C  BEWARE: SHOULD TRANSPOSE THE FOLLOWING MATRICES IN NON-SPECIAL
C  CASES, BECAUSE OF THE FORTRAN NOTATION OF DATA
C
C  BEWARE: OM WAS NOT PARAMETERISED WITH THESE ARRAYS
C          THESE DATA ALSO APPEAR IN MATVCT
C
      DATA OOS/  0 ,  1 ,
     &           1 ,  0 ,
C S=2 NOT IMPLEMENTED
     &           0 ,  0 ,
     &           0 ,  0 /
C
C     SYMMETRICAL P1-P1 EBE (S=1)
      DATA AAS/  0 ,  1 ,  2 ,
     &           1 ,  0 ,  3 ,
     &           2 ,  3 ,  0 ,
C     SYMMETRICAL P1-P1 PRE-ASSEMBLED EBE (S=2)
     &           0 ,  1 ,  3 ,
     &           1 ,  0 ,  2 ,
     &           3 ,  2 ,  0 /
C
C     NONSYMMETRICAL P1-P1 EBE (S=1)
      DATA AAQ/  0 ,  4 ,  5 ,
     &           1 ,  0 ,  6 ,
     &           2 ,  3 ,  0 ,
C     NONSYMMETRICAL P1-P1 PRE-ASSEMBLED EBE (S=2)
     &           0 ,  4 ,  3 ,
     &           1 ,  0 ,  5 ,
     &           6 ,  2 ,  0 /
C
C     SYMMETRICAL QUASI-BUBBLE QUASI-BUBBLE EBE (S=1)
      DATA BBS/  0 ,  1 ,  2 ,  3 ,
     &           1 ,  0 ,  4 ,  5 ,
     &           2 ,  4 ,  0 ,  6 ,
     &           3 ,  5 ,  6 ,  0 ,
C     SYMMETRICAL QUASI-BUBBLE QUASI-BUBBLE PRE-ASSEMBLED EBE (S=2)
     &           0 ,  4 ,  6 ,  1 ,
     &           4 ,  0 ,  5 ,  2 ,
     &           6 ,  5 ,  0 ,  3 ,
     &           1 ,  2 ,  3 ,  0 /
C
C     NONSYMMETRICAL QUASI-BUBBLE QUASI-BUBBLE EBE (S=1)
      DATA BBQ/  0 ,  7 ,  8 ,  9 ,
     &           1 ,  0 , 10 , 11 ,
     &           2 ,  4 ,  0 , 12 ,
     &           3 ,  5 ,  6 ,  0 ,
C     NONSYMMETRICAL QUASI-BUBBLE QUASI-BUBBLE PRE-ASSEMBLED EBE (S=2)
     &           0 , 10 ,  6 ,  7 ,
     &           4 ,  0 , 11 ,  8 ,
     &          12 ,  5 ,  0 ,  9 ,
     &           1 ,  2 ,  3 ,  0 /
C
C     NONSYMMETRICAL P1 QUASI-BUBBLE EBE (S=1)
      DATA ABQ/  0 ,  4 ,  7 ,
     &           1 ,  0 ,  8 ,
     &           2 ,  5 ,  0 ,
     &           3 ,  6 ,  9 ,
C     NONSYMMETRICAL P1 QUASI-BUBBLE PRE-ASSEMBLED EBE (S=2)
     &           0 ,  7 ,  3 ,
     &           1 ,  0 ,  8 ,
     &           9 ,  2 ,  0 ,
     &           4 ,  5 ,  6 /
C
C     NONSYMMETRICAL QUASI-BUBBLE P1 EBE (S=1)
      DATA BAQ/  0 ,  3 ,  5 ,  7 ,
     &           1 ,  0 ,  6 ,  8 ,
     &           2 ,  4 ,  0 ,  9 ,
C     NONSYMMETRICAL QUASI-BUBBLE P1 PRE-ASSEMBLED EBE (S=2)
     &           0 ,  7 ,  3 ,  4 ,
     &           1 ,  0 ,  8 ,  5 ,
     &           9 ,  2 ,  0 ,  6 /
C     NONSYMMETRICAL P1 P2 EBE (S=1)
      DATA ACQ/   0 ,  6  , 11 ,
     &            1 ,  0  , 12 ,
     &            2 ,  7  ,  0 ,
     &            3 ,  8  , 13 ,
     &            4 ,  9  , 14 ,
     &            5 ,  10 , 15 ,
C S=2 NOT IMPLEMENTED
     &            0 ,  0 ,  0  ,
     &            0 ,  0 ,  0  ,
     &            0 ,  0 ,  0  ,
     &            0 ,  0 ,  0  ,
     &            0 ,  0 ,  0  ,
     &            0 ,  0 ,  0  /
C     NONSYMMETRICAL P2 P1 EBE (S=1)
      DATA CAQ/  0 ,  3 ,  5 ,  7 , 10 , 13 ,
     &           1 ,  0 ,  6 ,  8 , 11 , 14 ,
     &           2 ,  4 ,  0 ,  9 , 12 , 15 ,
C     NONSYMMETRICAL P2 P1 PRE-ASSEMBLED EBE (S=2)
C     - NOT IMPLEMENTED
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 /
C
C     ADDED BY JMJ BUT NOT USED
C     SYMMETRICAL P1-P1 PRISMS AND P2 TRIANGLES EBE (S=1)
      DATA PPS/  0 ,  1 ,  2 ,  3 ,  4 ,  5 ,
     &           1 ,  0 ,  6 ,  7 ,  8 ,  9 ,
     &           2 ,  6 ,  0 , 10 , 11 , 12 ,
     &           3 ,  7 , 10 ,  0 , 13 , 14 ,
     &           4 ,  8 , 11 , 13 ,  0 , 15 ,
     &           5 ,  9 , 12 , 14 , 15 ,  0 ,
C     SYMMETRICAL P1-P1 PRISMS PRE-ASSEMBLED EBE (S=2) - NOT IMPLEMENTED
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 /
C
C     NONSYMMETRICAL P1-P1 PRISMS EBE (S=1)
      DATA PPQ/  0 , 16 , 17 , 18 , 19 , 20 ,
     &           1 ,  0 , 21 , 22 , 23 , 24 ,
     &           2 ,  6 ,  0 , 25 , 26 , 27 ,
     &           3 ,  7 , 10 ,  0 , 28 , 29 ,
     &           4 ,  8 , 11 , 13 ,  0 , 30 ,
     &           5 ,  9 , 12 , 14 , 15 ,  0 ,
C     NONSYMMETRICAL P1-P1 PRISMS PRE-ASSEMBLED EBE (S=2) - NOT IMPLEMENTED
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 /
C
C     SYMMETRICAL Q1-Q1 QUADRANGLES EBE (S=1)
      DATA FFS/  0 ,  1 ,  2 ,  3 ,
     &           1 ,  0 ,  4 ,  5 ,
     &           2 ,  4 ,  0 ,  6 ,
     &           3 ,  5 ,  6 ,  0 ,
C     SYMMETRICAL Q1-Q1 QUADRANGLES PRE-ASSEMBLED EBE (S=2) - NOT IMPLEMENTED
     &           0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 /
C
C-----------------------------------------------------------------------
C
C  TESTS THE TYPE OF MATRIX
C
C=======================================================================
C     MASS MATRIX
C=======================================================================
C
      IF(FORMUL(1:16).EQ.'MATMAS          ') THEN
C
C-----------------------------------------------------------------------
C       P1 TRIANGLE ROW ELEMENT
        IF(IELM1.EQ.11) THEN
C
C.......................................................................
C         P1 TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.11) THEN
             CALL MT01AA(   T(1,1)   ,XM(1,AAS(1,2,S)),XM(1,AAS(1,3,S)),
     &                                       T(1,2)   ,XM(1,AAS(2,3,S)),
     &                                                        T(1,3)   ,
     &                   XMUL,SURFAC,NELEM,NELMAX)
C
            TYPDIA='Q'
            TYPEXT='S'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C.......................................................................
C         ERROR ON THE COLUMN ELEMENT
C.......................................................................
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       QUASI-BUBBLE TRIANGLE ROW ELEMENT
        ELSEIF(IELM1.EQ.12) THEN
C
C.......................................................................
C         QUASI-BUBBLE TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.12) THEN
          CALL MT01BB
     & (   T(1,1)   ,XM(1,BBS(1,2,S)),XM(1,BBS(1,3,S)),XM(1,BBS(1,4,S)),
     &                      T(1,2)   ,XM(1,BBS(2,3,S)),XM(1,BBS(2,4,S)),
     &                                       T(1,3)   ,XM(1,BBS(3,4,S)),
     &                                                        T(1,4)   ,
     &      XMUL,SURFAC,NELEM,NELMAX)
C
            TYPDIA='Q'
            TYPEXT='S'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C.......................................................................
C         ERROR ON THE COLUMN ELEMENT
C.......................................................................
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       P2 TRIANGLE
C-----------------------------------------------------------------------
C
        ELSEIF(IELM1.EQ.13) THEN
C
C       TESTS THE COLUMN ELEMENT
C
C.......................................................................
C         P2 TRIANGLE
C.......................................................................
C
          IF(IELM2.EQ.13) THEN
             CALL MT01CC
     & (   T(1,1)   ,XM(1,PPS(1,2,S)),XM(1,PPS(1,3,S)),
     &   XM(1,PPS(1,4,S)),XM(1,PPS(1,5,S)),XM(1,PPS(1,6,S)),
     &     T(1,2)   ,XM(1,PPS(2,3,S)),XM(1,PPS(2,4,S)),
     &   XM(1,PPS(2,5,S)),XM(1,PPS(2,6,S)),
     &     T(1,3)   ,XM(1,PPS(3,4,S)),XM(1,PPS(3,5,S)),
     &   XM(1,PPS(3,6,S)),
     &     T(1,4)   ,XM(1,PPS(4,5,S)),XM(1,PPS(4,6,S)),
     &     T(1,5)   ,XM(1,PPS(5,6,S)),T(1,6),
     &     XMUL,SURFAC,NELEM,NELMAX)
C
             TYPDIA='Q'
             TYPEXT='S'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       P1 SEGMENT ROW ELEMENT
        ELSEIF(IELM1.EQ.1) THEN
C.......................................................................
C         P1 SEGMENT COLUMN ELEMENT
          IF(IELM2.EQ.1.AND.S.EQ.1) THEN
             CALL MT01OO(   T(1,1)   ,XM(1,OOS(1,2,S)),
     &                                       T(1,2)   ,
     &                   XMUL,SURFAC,NELEM,NELMAX)
C
             TYPDIA='Q'
             TYPEXT='S'
C
C.......................................................................
C         OTHER COLUMN ELEMENT
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C>>>>
C-----------------------------------------------------------------------
C       P1 PRISM ROW ELEMENT
        ELSEIF(IELM1.EQ.41) THEN
C
C.......................................................................
C         P1 PRISM COLUMN ELEMENT
          IF(IELM2.EQ.41) THEN
             CALL MT01PP(T,XM,XMUL,ZEL,SURFAC,IKLE,NELEM,NELMAX)
C
             TYPDIA='Q'
             TYPEXT='S'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       T1 TETRAHEDRON ROW ELEMENT
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
C
C.......................................................................
C         P1 PRISM COLUMN ELEMENT
          IF(IELM2.EQ.31.OR.IELM2.EQ.51) THEN
             CALL MT01TT(T,XM,XMUL,XEL,YEL,ZEL,IKLE,NELEM,NELMAX)
C
             TYPDIA='Q'
             TYPEXT='S'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       OTHER ROW ELEMENT
C-----------------------------------------------------------------------
C
C       ELSEIF
C
C-----------------------------------------------------------------------
C       ERROR ON THE ROW ELEMENT
C-----------------------------------------------------------------------
C
        ELSE
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
        ENDIF
C
C=======================================================================
C     DIFFUSION MATRIX
C=======================================================================
C
      ELSEIF(FORMUL(1:6).EQ.'MATDIF') THEN
C
C     CHARACTER 7 INFORMS WHETHER INCHYD OR NOT
C
      INCHYD = .FALSE.
      IF(FORMUL(7:7).EQ.'2') INCHYD = .TRUE.
C
C     TESTS THE ROW ELEMENT
C
C-----------------------------------------------------------------------
C       P1 TRIANGLE
        IF(IELM1.EQ.11) THEN
C
C.......................................................................
C         P1 TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.11) THEN
C
C     CHARACTER 7 ALSO INFORMS WHETHER THE DIFFUSION TERM IS REQUIRED
C     FOR ESTEL
C
      IF(FORMUL(7:7).NE.'3') THEN
C
             CALL MT02AA(  T(1,1),XM(1,AAS(1,2,S)),XM(1,AAS(1,3,S)),
     &                                   T(1,2)   ,XM(1,AAS(2,3,S)),
     &                                                      T(1,3) ,
     &                         XMUL,SU,U,SV,V,XEL,YEL,SURFAC,
     &             IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,FORMUL)
C
      ELSE
C
             CALL MT02AA_2( T(1,1)   ,XM(1,AAS(1,2,S)),XM(1,AAS(1,3,S)),
     &                                       T(1,2)   ,XM(1,AAS(2,3,S)),
     &                                                        T(1,3)   ,
     &                   XMUL,SU,SV,U,V,XEL,YEL,SURFAC,NELEM,NELMAX)

C
      ENDIF
C
      TYPDIA='Q'
      TYPEXT='S'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       QUASI-BUBBLE TRIANGLE ROW ELEMENT
        ELSEIF(IELM1.EQ.12) THEN
C
C.......................................................................
C         QUASI-BUBBLE TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.12) THEN
             CALL MT02BB
     & (   T(1,1)   ,XM(1,BBS(1,2,S)),XM(1,BBS(1,3,S)),XM(1,BBS(1,4,S)),
     &                      T(1,2)   ,XM(1,BBS(2,3,S)),XM(1,BBS(2,4,S)),
     &                                       T(1,3)   ,XM(1,BBS(3,4,S)),
     &                                                        T(1,4)   ,
     &          XMUL,SU,U,XEL,YEL,SURFAC,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX)
C
             TYPDIA='Q'
             TYPEXT='S'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       P2 TRIANGLE ROW ELEMENT
C
        ELSEIF(IELM1.EQ.13) THEN
C
C.......................................................................
C         P2 TRIANGLE COLUMN ELEMENT
C         USES MATRIX PPS BECAUSE WILL BE A 6X6 SYMMETRICAL MATRIX
C

          IF(IELM2.EQ.13) THEN
             CALL MT02CC
     & (   T(1,1)   ,XM(1,PPS(1,2,S)),XM(1,PPS(1,3,S)),
     &   XM(1,PPS(1,4,S)),XM(1,PPS(1,5,S)),XM(1,PPS(1,6,S)),
     &     T(1,2)   ,XM(1,PPS(2,3,S)),XM(1,PPS(2,4,S)),
     &   XM(1,PPS(2,5,S)),XM(1,PPS(2,6,S)),
     &     T(1,3)   ,XM(1,PPS(3,4,S)),XM(1,PPS(3,5,S)),
     &   XM(1,PPS(3,6,S)),
     &     T(1,4)   ,XM(1,PPS(4,5,S)),XM(1,PPS(4,6,S)),
     &     T(1,5)   ,XM(1,PPS(5,6,S)),T(1,6),
     &          XMUL,SU,U,XEL,YEL,SURFAC,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &          NELEM,NELMAX)
C
             TYPDIA='Q'
             TYPEXT='S'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       P1 PRISM ROW ELEMENT
        ELSEIF(IELM1.EQ.41) THEN
C
C.......................................................................
C         P1 PRISM COLUMN ELEMENT
          IF(IELM2.EQ.41) THEN
C
          IF(FORMUL(7:7).EQ.'*') THEN
C           COMPUTATION BASED ON THE TRANSFORMED MESH
            CALL MT02PP_STAR(T,XM,XMUL,SF,SG,SH,F,G,H,
     *                      XEL,YEL,ZEL,SURFAC,IKLE,NELEM,NELMAX,INCHYD,
     *                      FORMUL,NPLAN)
            IF(FORMUL(10:13).EQ.'1234') THEN
              TYPEXT='S'
            ELSEIF(FORMUL(10:13).EQ.'1 3 ') THEN
              TYPEXT='Q'
            ELSEIF(FORMUL(10:13).EQ.' 2 4') THEN
              TYPEXT='Q'
            ELSE
              WRITE(LU,*) 'ERROR ON FORMULA=',FORMUL
              CALL PLANTE(1)
              STOP
            ENDIF
          ELSE
C           COMPUTATION IN REAL MESH
C           CALL MT02PT(T,XM,XMUL,SF,SG,SH,F,G,H,
C    *                  XEL,YEL,ZEL,IKLE,NELEM,NELMAX,INCHYD)
            CALL MT02PP(T,XM,XMUL,SF,SG,SH,F,G,H,
     *                  XEL,YEL,ZEL,SURFAC,IKLE,NELEM,NELMAX,INCHYD,
     *                  FORMUL,NPLAN)
            TYPEXT='S'
          ENDIF
          TYPDIA='Q'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       TETRAHEDRON ROW ELEMENT
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
C
C.......................................................................
C         P1 PRISM COLUMN ELEMENT
          IF(IELM2.EQ.31.OR.IELM2.EQ.51) THEN
C
          CALL MT02TT(T,XM,XMUL,SF,SG,SH,F,G,H,
     &                XEL,YEL,ZEL,IKLE,NELEM,NELMAX,INCHYD)
C
          TYPDIA='Q'
          TYPEXT='S'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       OTHER ROW ELEMENT
C-----------------------------------------------------------------------
C
C       ELSEIF
C
C-----------------------------------------------------------------------
C       ERROR ON THE ROW ELEMENT
C-----------------------------------------------------------------------
C
        ELSE
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
        ENDIF
C
C=======================================================================
C     CONTRIBUTION OF SUPG TO THE MASS MATRIX
C=======================================================================
C
      ELSEIF(FORMUL(1:16).EQ.'MASUPG          ') THEN
C
C-----------------------------------------------------------------------
C       P1 TRIANGLE ROW ELEMENT
        IF(IELM1.EQ.11) THEN
C
C.......................................................................
C         P1 TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.11) THEN
             CALL MT03AA(   T(1,1)   ,XM(1,AAQ(1,2,S)),XM(1,AAQ(1,3,S)),
     &                   XM(1,AAQ(2,1,S)),   T(1,2)   ,XM(1,AAQ(2,3,S)),
     &                   XM(1,AAQ(3,1,S)),XM(1,AAQ(3,2,S)),   T(1,3)   ,
     &                   XMUL,SF,SG,SU,SV,F,G,U,V,XEL,YEL,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   NELEM,NELMAX)
C
             TYPDIA='Q'
             TYPEXT='Q'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       QUASI-BUBBLE TRIANGLE ROW ELEMENT
        ELSEIF(IELM1.EQ.12) THEN
C
C.......................................................................
C         QUASI-BUBBLE TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.12) THEN
             CALL MT03BB
     & (   T(1,1)   ,XM(1,BBQ(1,2,S)),XM(1,BBQ(1,3,S)),XM(1,BBQ(1,4,S)),
     &  XM(1,BBQ(2,1,S)),   T(1,2)   ,XM(1,BBQ(2,3,S)),XM(1,BBQ(2,4,S)),
     &  XM(1,BBQ(3,1,S)),XM(1,BBQ(3,2,S)),   T(1,3)   ,XM(1,BBQ(3,4,S)),
     &  XM(1,BBQ(4,1,S)),XM(1,BBQ(4,2,S)),XM(1,BBQ(4,3,S)),   T(1,4)   ,
     &          XMUL,SF,SG,SU,SV,F,G,U,V,
     &          XEL,YEL,IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX)
C
             TYPDIA='Q'
             TYPEXT='Q'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       P2 TRIANGLE ROW ELEMENT
        ELSEIF(IELM1.EQ.13) THEN
C
C.......................................................................
C         P2 TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.13) THEN
             CALL MT03CC
     & (   T(1,1)   ,XM(1,PPQ(1,2,S)),XM(1,PPQ(1,3,S)),
     &   XM(1,PPQ(1,4,S)),XM(1,PPQ(1,5,S)),XM(1,PPQ(1,6,S)),
     &   XM(1,PPQ(2,1,S)),T(1,2),XM(1,PPQ(2,3,S)),XM(1,PPQ(2,4,S)),
     &   XM(1,PPQ(2,5,S)),XM(1,PPQ(2,6,S)),XM(1,PPQ(3,1,S)),
     &   XM(1,PPQ(3,2,S)),T(1,3),XM(1,PPQ(3,4,S)),XM(1,PPQ(3,5,S)),
     &   XM(1,PPQ(3,6,S)),XM(1,PPQ(4,1,S)),XM(1,PPQ(4,2,S)),
     &   XM(1,PPQ(4,3,S)),T(1,4),XM(1,PPQ(4,5,S)),XM(1,PPQ(4,6,S)),
     &   XM(1,PPQ(5,1,S)),XM(1,PPQ(5,2,S)),XM(1,PPQ(5,3,S)),
     &   XM(1,PPQ(5,4,S)),  T(1,5)   ,XM(1,PPQ(5,6,S)),
     &   XM(1,PPQ(6,1,S)),XM(1,PPQ(6,2,S)) ,XM(1,PPQ(6,3,S)),
     &   XM(1,PPQ(6,4,S)),XM(1,PPQ(6,5,S)),T(1,6),
     &          XMUL,SF,SG,SU,SV,F,G,U,V,XEL,YEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &          IKLE(1,4),IKLE(1,5),IKLE(1,6),
     &          NELEM,NELMAX)
C
             TYPDIA='Q'
             TYPEXT='Q'
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       OTHER
C-----------------------------------------------------------------------
C
C       ELSEIF
C
C-----------------------------------------------------------------------
C       ERROR ON THE ROW ELEMENT
C-----------------------------------------------------------------------
C
        ELSE
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
        ENDIF
C
C=======================================================================
C     U.GRAD U.GRAD MATRIX
C=======================================================================
C
      ELSEIF(FORMUL(1:6).EQ.'MAUGUG') THEN
C
C-----------------------------------------------------------------------
C       P1 TRIANGLE ROW ELEMENT
        IF(IELM1.EQ.11) THEN
C
C.......................................................................
C         P1 TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.11) THEN
             CALL MT04AA(   T(1,1)   ,XM(1,AAS(1,2,S)),XM(1,AAS(1,3,S)),
     &                                       T(1,2)   ,XM(1,AAS(2,3,S)),
     &                                                        T(1,3)   ,
     &                   XMUL,SU,SV,U,V,XEL,YEL,SURFAC,IKLE,
     &                   NELEM,NELMAX)
C
             TYPDIA='Q'
             TYPEXT='S'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       QUASI-BUBBLE TRIANGLE ROW ELEMENT
        ELSEIF(IELM1.EQ.12) THEN
C
C.......................................................................
C         QUASI-BUBBLE TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.12) THEN
             CALL MT04BB
     & (   T(1,1)   ,XM(1,BBS(1,2,S)),XM(1,BBS(1,3,S)),XM(1,BBS(1,4,S)),
     &                      T(1,2)   ,XM(1,BBS(2,3,S)),XM(1,BBS(2,4,S)),
     &                                       T(1,3)   ,XM(1,BBS(3,4,S)),
     &                                                        T(1,4)   ,
     &          XMUL,SU,SV,U,V,XEL,YEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),NELEM,NELMAX)
C
             TYPDIA='Q'
             TYPEXT='S'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       P2 TRIANGLE ROW ELEMENT
        ELSEIF(IELM1.EQ.13) THEN
C
C.......................................................................
C         P2 TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.13) THEN
             CALL MT04CC
     & (   T(1,1)   ,XM(1,PPS(1,2,S)),XM(1,PPS(1,3,S)),
     &   XM(1,PPS(1,4,S)),XM(1,PPS(1,5,S)),XM(1,PPS(1,6,S)),
     &     T(1,2)   ,XM(1,PPS(2,3,S)),XM(1,PPS(2,4,S)),
     &   XM(1,PPS(2,5,S)),XM(1,PPS(2,6,S)),
     &     T(1,3)   ,XM(1,PPS(3,4,S)),XM(1,PPS(3,5,S)),
     &   XM(1,PPS(3,6,S)),
     &     T(1,4)   ,XM(1,PPS(4,5,S)),XM(1,PPS(4,6,S)),
     &     T(1,5)   ,XM(1,PPS(5,6,S)),T(1,6),
     &          XMUL,SU,SV,U,V,XEL,YEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &          IKLE(1,4),IKLE(1,5),IKLE(1,6),NELEM,NELMAX)
C
             TYPDIA='Q'
             TYPEXT='S'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       P1 PRISM ROW ELEMENT
        ELSEIF(IELM1.EQ.41) THEN
C
C.......................................................................
C         P1 PRISM COLUMN ELEMENT
          IF(IELM2.EQ.41) THEN
             CALL MT04PP(T,XM,XMUL,SU,SV,SW,U,V,W,
     &                   XEL,YEL,ZEL,SURFAC,IKLE,NELEM,NELMAX,FORMUL)
C
             TYPDIA='Q'
             IF(FORMUL(7:7).EQ.'2') THEN
               TYPEXT='S'
             ELSE
               TYPEXT='Q'
             ENDIF
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       TETRAHEDRON ROW ELEMENT
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
C
C.......................................................................
C         P1 PRISM COLUMN ELEMENT
          IF(IELM2.EQ.31.OR.IELM2.EQ.51) THEN
             CALL MT04TT(T,XM,XMUL,SU,SV,SW,U,V,W,
     &                   XEL,YEL,ZEL,IKLE,NELEM,NELMAX)
C
             TYPDIA='Q'
             TYPEXT='S'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       OTHER ROW ELEMENT
C-----------------------------------------------------------------------
C
C       ELSEIF
C
C-----------------------------------------------------------------------
C       ERROR ON THE ROW ELEMENT
C-----------------------------------------------------------------------
C
        ELSE
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
        ENDIF
C
C=======================================================================
C     U.GRAD MATRIX
C=======================================================================
C
      ELSEIF(FORMUL(1:6).EQ.'MATVGR') THEN
C
      SIGMAG = .FALSE.
      IF(FORMUL(7:7).EQ.'2') SIGMAG = .TRUE.
      SPECAD = .FALSE.
      IF(FORMUL(8:8).EQ.'2') SPECAD = .TRUE.
C
C-----------------------------------------------------------------------
C       P1 TRIANGLE ROW ELEMENT
        IF(IELM1.EQ.11) THEN
C
C.......................................................................
C         P1 TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.11) THEN
             CALL MT05AA(   T(1,1)   ,XM(1,AAQ(1,2,S)),XM(1,AAQ(1,3,S)),
     &                   XM(1,AAQ(2,1,S)),   T(1,2)   ,XM(1,AAQ(2,3,S)),
     &                   XM(1,AAQ(3,1,S)),XM(1,AAQ(3,2,S)),   T(1,3)   ,
     &                   XMUL,SU,SV,U,V,XEL,YEL,IKLE,
     &                   NELEM,NELMAX,FORMUL)
C
             TYPDIA='Q'
             TYPEXT='Q'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       QUASI-BUBBLE TRIANGLE ROW ELEMENT
        ELSEIF(IELM1.EQ.12) THEN
C
C.......................................................................
C         QUASI-BUBBLE TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.12) THEN
             CALL MT05BB
     & (   T(1,1)   ,XM(1,BBQ(1,2,S)),XM(1,BBQ(1,3,S)),XM(1,BBQ(1,4,S)),
     &  XM(1,BBQ(2,1,S)),   T(1,2)   ,XM(1,BBQ(2,3,S)),XM(1,BBQ(2,4,S)),
     &  XM(1,BBQ(3,1,S)),XM(1,BBQ(3,2,S)),   T(1,3)   ,XM(1,BBQ(3,4,S)),
     &  XM(1,BBQ(4,1,S)),XM(1,BBQ(4,2,S)),XM(1,BBQ(4,3,S)),   T(1,4)   ,
     &          XMUL,SU,SV,U,V,XEL,YEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &          NELEM,NELMAX,FORMUL)
C
             TYPDIA='Q'
             TYPEXT='Q'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       P2 TRIANGLE ROW ELEMENT
        ELSEIF(IELM1.EQ.13) THEN
C
C.......................................................................
C         P2 TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.13) THEN
             CALL MT05CC
     & (   T(1,1)   ,XM(1,PPQ(1,2,S)),XM(1,PPQ(1,3,S)),
     &   XM(1,PPQ(1,4,S)),XM(1,PPQ(1,5,S)),XM(1,PPQ(1,6,S)),
     &   XM(1,PPQ(2,1,S)),T(1,2),XM(1,PPQ(2,3,S)),XM(1,PPQ(2,4,S)),
     &   XM(1,PPQ(2,5,S)),XM(1,PPQ(2,6,S)),XM(1,PPQ(3,1,S)),
     &   XM(1,PPQ(3,2,S)),T(1,3),XM(1,PPQ(3,4,S)),XM(1,PPQ(3,5,S)),
     &   XM(1,PPQ(3,6,S)),XM(1,PPQ(4,1,S)),XM(1,PPQ(4,2,S)),
     &   XM(1,PPQ(4,3,S)),T(1,4),XM(1,PPQ(4,5,S)),XM(1,PPQ(4,6,S)),
     &   XM(1,PPQ(5,1,S)),XM(1,PPQ(5,2,S)),XM(1,PPQ(5,3,S)),
     &   XM(1,PPQ(5,4,S)),  T(1,5)   ,XM(1,PPQ(5,6,S)),
     &   XM(1,PPQ(6,1,S)),XM(1,PPQ(6,2,S)) ,XM(1,PPQ(6,3,S)),
     &   XM(1,PPQ(6,4,S)),XM(1,PPQ(6,5,S)),T(1,6),
     &          XMUL,SU,SV,U,V,XEL,YEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &          IKLE(1,4),IKLE(1,5),IKLE(1,6),
     &          NELEM,NELMAX,FORMUL)
C
             TYPDIA='Q'
             TYPEXT='Q'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       P1 PRISM ROW ELEMENT
        ELSEIF(IELM1.EQ.41) THEN
C
C.......................................................................
C         P1 PRISM COLUMN ELEMENT
          IF(IELM2.EQ.41) THEN
             CALL MT05PP(T,XM,XMUL,SU,SV,SW,U,V,W,SF,SG,SH,F,G,H,
     &                   XEL,YEL,ZEL,IKLE,NELEM,NELMAX,SURFAC,SIGMAG,
     &                   SPECAD,NPLAN)
C
             TYPDIA='Q'
             TYPEXT='Q'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       TETRAHEDRON ROW ELEMENT
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
C
C.......................................................................
C         P1 PRISM COLUMN ELEMENT
          IF(IELM2.EQ.31.OR.IELM2.EQ.51) THEN
             CALL MT05TT(T,XM,XMUL,SU,SV,SW,U,V,W,
     &                   XEL,YEL,ZEL,IKLE,NELEM,NELMAX)
C
             TYPDIA='Q'
             TYPEXT='Q'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       ERROR ON THE ROW ELEMENT
C-----------------------------------------------------------------------
C
        ELSE
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
        ENDIF
C
C=======================================================================
C     F PSI PSJ MATRIX
C=======================================================================
C
      ELSEIF(FORMUL(1:6).EQ.'FMATMA') THEN
C
C-----------------------------------------------------------------------
C       P1 TRIANGLE ROW ELEMENT
        IF(IELM1.EQ.11) THEN
C
C.......................................................................
C         P1 TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.11) THEN
C
             CALL MT06AA(   T(1,1)   ,XM(1,AAS(1,2,S)),XM(1,AAS(1,3,S)),
     &                                       T(1,2)   ,XM(1,AAS(2,3,S)),
     &                                                        T(1,3)   ,
     &                   XMUL,SF,F,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX)
C
            TYPDIA='Q'
            TYPEXT='S'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       P1 TRIANGLE ROW ELEMENT
C       (LATERAL SIDE OF PRISM SPLIT IN TETRAHEDRONS)
C
        ELSEIF(IELM1.EQ.61.OR.IELM1.EQ.81) THEN
C
C.......................................................................
C         P1 TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.61.OR.IELM1.EQ.81) THEN
C
C     CHARACTER 7 ALSO INFORMS WHETHER THE ADDITIONAL TERM IS REQUIRED
C     FOR ESTEL-3D
C
      IF(FORMUL(7:7).NE.'2') THEN
C
                CALL MT06FT
     & (   T(1,1)   ,XM(1,AAS(1,2,S)),XM(1,AAS(1,3,S)),
     &                      T(1,2)   ,XM(1,AAS(2,3,S)),
     &                                       T(1,3)   ,
     &          XMUL,SF,F,XEL,YEL,ZEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &          NBOR,NELEM,NELMAX)
C
      ELSE
                CALL MT06FT2
     & (   T(1,1)   ,XM(1,AAS(1,2,S)),XM(1,AAS(1,3,S)),
     &                      T(1,2)   ,XM(1,AAS(2,3,S)),
     &                                       T(1,3)   ,
     &          XMUL,SF,F,SU,U,XEL,YEL,ZEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &          NBOR,NELEM,NELMAX)
      ENDIF
            TYPDIA='Q'
            TYPEXT='S'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       QUASI-BUBBLE TRIANGLE ROW ELEMENT
C
        ELSEIF(IELM1.EQ.12) THEN
C
C.......................................................................
C         QUASI-BUBBLE TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.12) THEN
             CALL MT06BB
     & (   T(1,1)   ,XM(1,BBS(1,2,S)),XM(1,BBS(1,3,S)),XM(1,BBS(1,4,S)),
     &                      T(1,2)   ,XM(1,BBS(2,3,S)),XM(1,BBS(2,4,S)),
     &                                       T(1,3)   ,XM(1,BBS(3,4,S)),
     &                                                        T(1,4)   ,
     &          XMUL,SF,F,SURFAC,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),NELEM,NELMAX)
C
             TYPDIA='Q'
             TYPEXT='S'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       QUADRATIC TRIANGLE ROW ELEMENT
C
        ELSEIF(IELM1.EQ.13) THEN
C
C.......................................................................
C         QUADRATIC TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.13) THEN
             CALL MT06CC
     & ( T(1,1)   ,XM(1,PPS(1,2,S)),XM(1,PPS(1,3,S)),
     &   XM(1,PPS(1,4,S)),XM(1,PPS(1,5,S)),XM(1,PPS(1,6,S)),
     &   T(1,2)   ,XM(1,PPS(2,3,S)),XM(1,PPS(2,4,S)),
     &   XM(1,PPS(2,5,S)),XM(1,PPS(2,6,S)),
     &   T(1,3)   ,XM(1,PPS(3,4,S)),XM(1,PPS(3,5,S)),XM(1,PPS(3,6,S)),
     &   T(1,4)   ,XM(1,PPS(4,5,S)),XM(1,PPS(4,6,S)),
     &   T(1,5)   ,XM(1,PPS(5,6,S)),T(1,6)          ,
     &          XMUL,SF,F,SURFAC,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &          IKLE(1,4),IKLE(1,5),IKLE(1,6),NELEM,NELMAX)
C
             TYPDIA='Q'
             TYPEXT='S'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       QUADRILATERAL ROW ELEMENT (LATERAL SIDE OF PRISM)
        ELSEIF(IELM1.EQ.71) THEN
C
C.......................................................................
C
C         BEWARE !!!!!!!!!!
C         QUADRANGLE COLUMN ELEMENT FOR TELEMAC-3D PRISMS
          IF(IELM2.EQ.71) THEN
C
               CALL MT06FF
     & (   T(1,1)   ,XM(1,FFS(1,2,S)),XM(1,FFS(1,3,S)),XM(1,FFS(1,4,S)),
     &                      T(1,2)   ,XM(1,FFS(2,3,S)),XM(1,FFS(2,4,S)),
     &                                       T(1,3)   ,XM(1,FFS(3,4,S)),
     &                                                        T(1,4)   ,
     &         XMUL,SF,F,XEL,YEL,ZEL,
     &         IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &         NBOR,NELEM,NELMAX)
C
               TYPDIA='Q'
               TYPEXT='S'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C<<<<
C-----------------------------------------------------------------------
C       P1 SEGMENT ROW ELEMENT
        ELSEIF(IELM1.EQ.1) THEN
C.......................................................................
C         P1 SEGMENT COLUMN ELEMENT
          IF(IELM2.EQ.1.AND.S.EQ.1) THEN
             CALL MT06OO(   T(1,1)   ,XM(1,OOS(1,2,S)),
     &                                       T(1,2)   ,
     &                   XMUL,SF,F,SURFAC,IKLE(1,1),IKLE(1,2),
     &                   NBOR,NELEM,NELMAX)
C
             TYPDIA='Q'
             TYPEXT='S'
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C<<<<
C-----------------------------------------------------------------------
C       P2 SEGMENT ROW ELEMENT
        ELSEIF(IELM1.EQ.2) THEN
C.......................................................................
C         P2 SEGMENT COLUMN ELEMENT
          IF(IELM2.EQ.2.AND.S.EQ.1) THEN
               CALL MT06OC
     & (   T(1,1)   ,XM(1,AAS(1,2,S)),XM(1,AAS(1,3,S)),
     &                      T(1,2)   ,XM(1,AAS(2,3,S)),
     &                                       T(1,3)   ,
     &        XMUL,SF,F,SURFAC,IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                   NBOR,NELEM,NELMAX)
C
             TYPDIA='Q'
             TYPEXT='S'
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       P1 PRISM ROW ELEMENT
C
        ELSE IF (IELM1.EQ.41) THEN
C.......................................................................
C         P1 PRISM COLUMN ELEMENT
          IF (IELM2.EQ.41) THEN
             CALL MT06PP(T,XM,
     &                   XMUL,SF,F,ZEL,SURFAC,IKLE,NELEM,NELMAX)
C
             TYPDIA='Q'
             TYPEXT='S'
C             CALL SETDIA(M,'Q')
C             CALL SETEXT(M,'S')
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       OTHER
C-----------------------------------------------------------------------
C
C       ELSEIF
C
C
C-----------------------------------------------------------------------
C       TETRAHEDRON ROW ELEMENT
C
        ELSE IF (IELM1.EQ.31.OR.IELM1.EQ.51) THEN
C.......................................................................
C         P1 PRISM COLUMN ELEMENT
          IF (IELM2.EQ.31.OR.IELM2.EQ.51) THEN
             CALL MT06TT(T,XM,
     &                   XMUL,SF,F,
     &                   XEL,YEL,ZEL,IKLE,NELEM,NELMAX)
C
             TYPDIA='Q'
             TYPEXT='S'
C             CALL SETDIA(M,'Q')
C             CALL SETEXT(M,'S')
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       OTHER
C-----------------------------------------------------------------------
C
C       ELSEIF
C-----------------------------------------------------------------------
C       ERROR ON THE ROW ELEMENT
C-----------------------------------------------------------------------
C
        ELSE
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
        ENDIF
C
C=======================================================================
C     MASS-LUMPED MASS MATRIX
C=======================================================================
C
      ELSEIF(FORMUL(1:16).EQ.'MSLUMP          ') THEN
C
C     TESTS THE ROW ELEMENT
C
C-----------------------------------------------------------------------
C       P1 TRIANGLE
C-----------------------------------------------------------------------
C
        IF(IELM1.EQ.11) THEN
C
C       TESTS THE COLUMN ELEMENT
C
C.......................................................................
C         P1 TRIANGLE
C.......................................................................
C
          IF(IELM2.EQ.11) THEN
             CALL MT07AA(   T(1,1)   ,XM(1,AAS(1,2,S)),XM(1,AAS(1,3,S)),
     &                                       T(1,2)   ,XM(1,AAS(2,3,S)),
     &                                                        T(1,3)   ,
     &                   XMUL,SF,F,SURFAC,NELEM,NELMAX)
C
             TYPDIA='Q'
             TYPEXT='S'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       QUASI-BUBBLE TRIANGLE
C-----------------------------------------------------------------------
C
        ELSEIF(IELM1.EQ.12) THEN
C
C       TESTS THE COLUMN ELEMENT
C
C.......................................................................
C         QUASI-BUBBLE TRIANGLE
C.......................................................................
C
          IF(IELM2.EQ.12) THEN
             CALL MT07BB
     & (   T(1,1)   ,XM(1,BBS(1,2,S)),XM(1,BBS(1,3,S)),XM(1,BBS(1,4,S)),
     &                      T(1,2)   ,XM(1,BBS(2,3,S)),XM(1,BBS(2,4,S)),
     &                                       T(1,3)   ,XM(1,BBS(3,4,S)),
     &                                                        T(1,4)   ,
     &          XMUL,SF,F,SURFAC,NELEM,NELMAX)
C
             TYPDIA='Q'
             TYPEXT='S'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       P2 TRIANGLE
C-----------------------------------------------------------------------
C
        ELSEIF(IELM1.EQ.13) THEN
C
C       TESTS THE COLUMN ELEMENT
C
C.......................................................................
C         P2 TRIANGLE
C.......................................................................
C
          IF(IELM2.EQ.13) THEN
             CALL MT07CC
     & (   T(1,1)   ,XM(1,PPS(1,2,S)),XM(1,PPS(1,3,S)),
     &   XM(1,PPS(1,4,S)),XM(1,PPS(1,5,S)),XM(1,PPS(1,6,S)),
     &     T(1,2)   ,XM(1,PPS(2,3,S)),XM(1,PPS(2,4,S)),
     &   XM(1,PPS(2,5,S)),XM(1,PPS(2,6,S)),
     &     T(1,3)   ,XM(1,PPS(3,4,S)),XM(1,PPS(3,5,S)),
     &   XM(1,PPS(3,6,S)),
     &     T(1,4)   ,XM(1,PPS(4,5,S)),XM(1,PPS(4,6,S)),
     &     T(1,5)   ,XM(1,PPS(5,6,S)),T(1,6),
     &          XMUL,SF,F,SURFAC,NELEM,NELMAX)
C
             TYPDIA='Q'
             TYPEXT='S'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       OTHER
C-----------------------------------------------------------------------
C
C       ELSEIF
C
C-----------------------------------------------------------------------
C       ERROR ON THE ROW ELEMENT
C-----------------------------------------------------------------------
C
        ELSE
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
        ENDIF
C
C=======================================================================
C     U GRADIENT MATRIX
C=======================================================================
C
      ELSEIF(FORMUL(1:15).EQ.'MATFGR         ') THEN
C
C     CHARACTER 16 IS THE SELECTED COORDINATE
C
      IF(FORMUL(16:16).EQ.'X') THEN
        ICOORD=1
      ELSEIF(FORMUL(16:16).EQ.'Y') THEN
        ICOORD=2
      ELSEIF(FORMUL(16:16).EQ.'Z') THEN
        ICOORD=3
      ENDIF
C
C-----------------------------------------------------------------------
C       P1 TRIANGLE ROW ELEMENT
C-----------------------------------------------------------------------
C
        IF(IELM1.EQ.11) THEN
C
C.......................................................................
C         P1 TRIANGLE COLUMN ELEMENT
C.......................................................................
C
          IF(IELM2.EQ.11) THEN
             CALL MT08AA(   T(1,1)   ,XM(1,AAQ(1,2,S)),XM(1,AAQ(1,3,S)),
     &                   XM(1,AAQ(2,1,S)),   T(1,2)   ,XM(1,AAQ(2,3,S)),
     &                   XM(1,AAQ(3,1,S)),XM(1,AAQ(3,2,S)),   T(1,3)   ,
     &                   XMUL,SF,F,XEL,YEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                   NELEM,NELMAX,ICOORD)
C
             TYPDIA='Q'
             TYPEXT='Q'
C
C.......................................................................
C
          ELSEIF(IELM2.EQ.12) THEN
             CALL MT08AB
     & (   T(1,1)   ,XM(1,ABQ(1,2,S)),XM(1,ABQ(1,3,S)),XM(1,ABQ(1,4,S)),
     &  XM(1,ABQ(2,1,S)),   T(1,2)   ,XM(1,ABQ(2,3,S)),XM(1,ABQ(2,4,S)),
     &  XM(1,ABQ(3,1,S)),XM(1,ABQ(3,2,S)),   T(1,3)   ,XM(1,ABQ(3,4,S)),
     &          XMUL,SF,F,XEL,YEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &          NELEM,NELMAX,ICOORD)
C
             TYPDIA='Q'
             TYPEXT='Q'
C
           ELSEIF(IELM2.EQ.13) THEN
             CALL MT08AC(   T(1,1)   ,XM(1,ACQ(1,2,S)),XM(1,ACQ(1,3,S)),
     &                   XM(1,ACQ(1,4,S)),XM(1,ACQ(1,5,S)),
     &                   XM(1,ACQ(1,6,S)),XM(1,ACQ(2,1,S)),
     &                   T(1,2)  ,XM(1,ACQ(2,3,S)),
     &                   XM(1,ACQ(2,4,S)),XM(1,ACQ(2,5,S)),
     &                   XM(1,ACQ(2,6,S)),XM(1,ACQ(3,1,S)),
     &                   XM(1,ACQ(3,2,S)),T(1,3)  ,XM(1,ACQ(3,4,S)),
     &                   XM(1,ACQ(3,5,S)),XM(1,ACQ(3,6,S)),
     &                   XMUL,SF,F,XEL,YEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                   IKLE(1,4),IKLE(1,5),IKLE(1,6),
     &                   NELEM,NELMAX,ICOORD)
C
             TYPDIA='Q'
             TYPEXT='Q'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       QUASI-BUBBLE TRIANGLE ROW ELEMENT
        ELSEIF(IELM1.EQ.12) THEN
C
C.......................................................................
C         QUASI-BUBBLE TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.12) THEN
             CALL MT08BB
     & (   T(1,1)   ,XM(1,BBQ(1,2,S)),XM(1,BBQ(1,3,S)),XM(1,BBQ(1,4,S)),
     &  XM(1,BBQ(2,1,S)),   T(1,2)   ,XM(1,BBQ(2,3,S)),XM(1,BBQ(2,4,S)),
     &  XM(1,BBQ(3,1,S)),XM(1,BBQ(3,2,S)),   T(1,3)   ,XM(1,BBQ(3,4,S)),
     &  XM(1,BBQ(4,1,S)),XM(1,BBQ(4,2,S)),XM(1,BBQ(4,3,S)),   T(1,4)   ,
     &          XMUL,SF,F,XEL,YEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &          NELEM,NELMAX,ICOORD)
C
             TYPDIA='Q'
             TYPEXT='Q'
C
C         LINEAR TRIANGLE COLUMN ELEMENT
          ELSEIF(IELM2.EQ.11) THEN
             CALL MT08BA
     &         (     T(1,1)     ,XM(1,BAQ(1,2,S)),XM(1,BAQ(1,3,S)),
     &          XM(1,BAQ(2,1,S)),     T(1,2)     ,XM(1,BAQ(2,3,S)),
     &          XM(1,BAQ(3,1,S)),XM(1,BAQ(3,2,S)),     T(1,3)     ,
     &          XM(1,BAQ(4,1,S)),XM(1,BAQ(4,2,S)),XM(1,BAQ(4,3,S)),
     &          XMUL,SF,F,XEL,YEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &          NELEM,NELMAX,ICOORD)
C
             TYPDIA='Q'
             TYPEXT='Q'
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C
C       P1 PRISM ROW ELEMENT
        ELSEIF(IELM1.EQ.41) THEN
C
C.......................................................................
C         P1 PRISM COLUMN ELEMENT
          IF(IELM2.EQ.41.AND.ICOORD.EQ.3) THEN
             CALL MT08PP(T,XM,XMUL,SF,F,SURFAC,IKLE,NELEM,NELMAX)
             TYPDIA='Q'
             TYPEXT='Q'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       P1 PRISM ROW ELEMENT
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
C
C.......................................................................
C         P1 PRISM COLUMN ELEMENT
          IF(IELM2.EQ.31.OR.IELM2.EQ.51) THEN
             CALL MT08TT(T,XM,XMUL,XEL,YEL,ZEL,SF,F,IKLE,NELEM,NELMAX)
             TYPDIA='Q'
             TYPEXT='Q'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C-----------------------------------------------------------------------
C       OTHER
C-----------------------------------------------------------------------
C
C       ELSEIF
C-----------------------------------------------------------------------
C       ERROR ON THE ROW ELEMENT
C-----------------------------------------------------------------------
C
        ELSE
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
        ENDIF
C
C=======================================================================
C     F U GRADIENT MATRIX (NOT IMPLEMENTED)
C=======================================================================
C
C     ELSEIF(FORMUL(1:15).EQ.'MATQGR         ') THEN
C
C     CHARACTER 16 IS THE SELECTED COORDINATE
C
C     IF(FORMUL(16:16).EQ.'X') THEN
C       ICOORD=1
C     ELSEIF(FORMUL(16:16).EQ.'Y') THEN
C       ICOORD=2
C     ELSEIF(FORMUL(16:16).EQ.'Z') THEN
C       ICOORD=3
C     ENDIF
C
C-----------------------------------------------------------------------
C       P1 TRIANGLE ROW ELEMENT
C-----------------------------------------------------------------------
C
C       IF(IELM1.EQ.11) THEN
C
C.......................................................................
C         P1 TRIANGLE COLUMN ELEMENT
C.......................................................................
C
C         IF(IELM1.EQ.11) THEN
C            CALL MT09AA(   T(1,1)   ,XM(1,AAQ(1,2,S)),XM(1,AAQ(1,3,S)),
C    *                   XM(1,AAQ(2,1,S)),   T(1,2)   ,XM(1,AAQ(2,3,S)),
C    *                   XM(1,AAQ(3,1,S)),XM(1,AAQ(3,2,S)),   T(1,3)   ,
C    *                   XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
C    *                   XEL,YEL,ZEL,SURFAC,
C    *                   IKLE(1,1),IKLE(1,2),IKLE(1,3),
C    *                   NELEM,NELMAX,ICOORD)
C
C            TYPDIA='Q'
C            TYPEXT='Q'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
C         ELSE
C           IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
C           IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
C           IF (LNG.EQ.1) WRITE(LU,2000) IELM1
C           IF (LNG.EQ.2) WRITE(LU,2001) IELM1
C           IF (LNG.EQ.1) WRITE(LU,3000) IELM2
C           IF (LNG.EQ.2) WRITE(LU,3001) IELM2
C           CALL PLANTE(1)
C           STOP
C         ENDIF
C
C-----------------------------------------------------------------------
C       OTHER
C-----------------------------------------------------------------------
C
C       ELSEIF
C
C-----------------------------------------------------------------------
C       ERROR ON THE ROW ELEMENT
C-----------------------------------------------------------------------
C
C       ELSE
C         IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
C         IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
C         IF (LNG.EQ.1) WRITE(LU,2000) IELM1
C         IF (LNG.EQ.2) WRITE(LU,2001) IELM1
C         CALL PLANTE(1)
C         STOP
C       ENDIF
C
C=======================================================================
C     U.N MATRIX  (NOT IMPLEMENTED)
C=======================================================================
C
C     ELSEIF(FORMUL(1:15).EQ.'??????         ') THEN
C
C     CHARACTER 16 IS THE SELECTED COORDINATE
C
C     IF(FORMUL(16:16).EQ.'X') THEN
C       ICOORD=1
C     ELSEIF(FORMUL(16:16).EQ.'Y') THEN
C       ICOORD=2
C     ELSEIF(FORMUL(16:16).EQ.'Z') THEN
C       ICOORD=3
C     ENDIF
C
C-----------------------------------------------------------------------
C       P1 TRIANGLE ROW ELEMENT
C-----------------------------------------------------------------------
C
C       IF(IELM1.EQ.11) THEN
C
C.......................................................................
C         P1 TRIANGLE COLUMN ELEMENT
C.......................................................................
C
C         IF(IELM1.EQ.11) THEN
C            CALL MT10AA(   T(1,1)   ,XM(1,AAQ(1,2,S)),XM(1,AAQ(1,3,S)),
C    *                   XM(1,AAQ(2,1,S)),   T(1,2)   ,XM(1,AAQ(2,3,S)),
C    *                   XM(1,AAQ(3,1,S)),XM(1,AAQ(3,2,S)),   T(1,3)   ,
C    *                   XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
C    *                   XEL,YEL,ZEL,SURFAC,
C    *                   IKLE(1,1),IKLE(1,2),IKLE(1,3),
C    *                   NELEM,NELMAX,ICOORD)
C
C            TYPDIA='Q'
C            TYPEXT='Q'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
C         ELSE
C           IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
C           IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
C           IF (LNG.EQ.1) WRITE(LU,2000) IELM1
C           IF (LNG.EQ.2) WRITE(LU,2001) IELM1
C           IF (LNG.EQ.1) WRITE(LU,3000) IELM2
C           IF (LNG.EQ.2) WRITE(LU,3001) IELM2
C           CALL PLANTE(1)
C           STOP
C         ENDIF
C
C-----------------------------------------------------------------------
C       OTHER
C-----------------------------------------------------------------------
C
C       ELSEIF
C
C-----------------------------------------------------------------------
C       ERROR ON THE ROW ELEMENT
C-----------------------------------------------------------------------
C
C       ELSE
C         IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
C         IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
C         IF (LNG.EQ.1) WRITE(LU,2000) IELM1
C         IF (LNG.EQ.2) WRITE(LU,2001) IELM1
C         CALL PLANTE(1)
C         STOP
C       ENDIF
C
C=======================================================================
C     - PSIJ GRAD(F PSII) MATRIX
C=======================================================================
C
      ELSEIF(FORMUL(1:15).EQ.'MATGRF         ') THEN
C
C     CHARACTER 16 IS THE SELECTED COORDINATE
C
      IF(FORMUL(16:16).EQ.'X') THEN
        ICOORD=1
      ELSEIF(FORMUL(16:16).EQ.'Y') THEN
        ICOORD=2
      ELSEIF(FORMUL(16:16).EQ.'Z') THEN
        ICOORD=3
      ENDIF
C
C-----------------------------------------------------------------------
C       P1 TRIANGLE ROW ELEMENT
C-----------------------------------------------------------------------
C
        IF(IELM1.EQ.11) THEN
C
C.......................................................................
C         P1 TRIANGLE COLUMN ELEMENT
C.......................................................................
C
          IF(IELM2.EQ.11) THEN
             CALL MT11AA(   T(1,1)   ,XM(1,AAQ(1,2,S)),XM(1,AAQ(1,3,S)),
     &                   XM(1,AAQ(2,1,S)),   T(1,2)   ,XM(1,AAQ(2,3,S)),
     &                   XM(1,AAQ(3,1,S)),XM(1,AAQ(3,2,S)),   T(1,3)   ,
     &                   XMUL,SF,F,XEL,YEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                   NELEM,NELMAX,ICOORD)
C
             TYPDIA='Q'
             TYPEXT='Q'
C
C.......................................................................
C
          ELSEIF(IELM2.EQ.12) THEN
             CALL MT11AB
     & (   T(1,1)   ,XM(1,ABQ(1,2,S)),XM(1,ABQ(1,3,S)),XM(1,ABQ(1,4,S)),
     &  XM(1,ABQ(2,1,S)),   T(1,2)   ,XM(1,ABQ(2,3,S)),XM(1,ABQ(2,4,S)),
     &  XM(1,ABQ(3,1,S)),XM(1,ABQ(3,2,S)),   T(1,3)   ,XM(1,ABQ(3,4,S)),
     &          XMUL,SF,F,XEL,YEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &          NELEM,NELMAX,ICOORD)
C
             TYPDIA='Q'
             TYPEXT='Q'
C
C.......................................................................
C
          ELSEIF(IELM2.EQ.13) THEN
             CALL MT11AC(
     &       T(1,1)   ,XM(1,ACQ(1,2,S)),XM(1,ACQ(1,3,S)),
     &       XM(1,ACQ(1,4,S)),XM(1,ACQ(1,5,S)),
     &       XM(1,ACQ(1,6,S)),XM(1,ACQ(2,1,S)),
     &       T(1,2)  ,XM(1,ACQ(2,3,S)),
     &       XM(1,ACQ(2,4,S)),XM(1,ACQ(2,5,S)),
     &       XM(1,ACQ(2,6,S)),XM(1,ACQ(3,1,S)),
     &       XM(1,ACQ(3,2,S)),T(1,3)  ,XM(1,ACQ(3,4,S)),
     &       XM(1,ACQ(3,5,S)),XM(1,ACQ(3,6,S)),
     &          XMUL,SF,F,XEL,YEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &          IKLE(1,4),IKLE(1,5),IKLE(1,6),
     &          NELEM,NELMAX,ICOORD)
C
             TYPDIA='Q'
             TYPEXT='Q'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       QUASI-BUBBLE TRIANGLE ROW ELEMENT
        ELSEIF(IELM1.EQ.12) THEN
C
C.......................................................................
C         QUASI-BUBBLE TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.12) THEN
             CALL MT11BB
     & (   T(1,1)   ,XM(1,BBQ(1,2,S)),XM(1,BBQ(1,3,S)),XM(1,BBQ(1,4,S)),
     &  XM(1,BBQ(2,1,S)),   T(1,2)   ,XM(1,BBQ(2,3,S)),XM(1,BBQ(2,4,S)),
     &  XM(1,BBQ(3,1,S)),XM(1,BBQ(3,2,S)),   T(1,3)   ,XM(1,BBQ(3,4,S)),
     &  XM(1,BBQ(4,1,S)),XM(1,BBQ(4,2,S)),XM(1,BBQ(4,3,S)),   T(1,4)   ,
     &          XMUL,SF,F,XEL,YEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &          NELEM,NELMAX,ICOORD)
C
             TYPDIA='Q'
             TYPEXT='Q'
C
C         LINEAR TRIANGLE COLUMN ELEMENT
          ELSEIF(IELM2.EQ.11) THEN
             CALL MT11BA
     &         (     T(1,1)     ,XM(1,BAQ(1,2,S)),XM(1,BAQ(1,3,S)),
     &          XM(1,BAQ(2,1,S)),     T(1,2)     ,XM(1,BAQ(2,3,S)),
     &          XM(1,BAQ(3,1,S)),XM(1,BAQ(3,2,S)),     T(1,3)     ,
     &          XM(1,BAQ(4,1,S)),XM(1,BAQ(4,2,S)),XM(1,BAQ(4,3,S)),
     &          XMUL,SF,F,XEL,YEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &          NELEM,NELMAX,ICOORD)
C
             TYPDIA='Q'
             TYPEXT='Q'
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       OTHER
C-----------------------------------------------------------------------
C
C       ELSEIF
C
C-----------------------------------------------------------------------
C       ERROR ON THE ROW ELEMENT
C-----------------------------------------------------------------------
C
        ELSE
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
        ENDIF
C
C=======================================================================
C   PSIJ GRAD(F)   U .GRAD(PSII) MATRIX
C=======================================================================
C
      ELSEIF(FORMUL(1:15).EQ.'MATUGH         ') THEN
C
C     CHARACTER 16 IS THE SELECTED COORDINATE
C
      IF(FORMUL(16:16).EQ.'X') THEN
        ICOORD=1
      ELSEIF(FORMUL(16:16).EQ.'Y') THEN
        ICOORD=2
      ELSEIF(FORMUL(16:16).EQ.'Z') THEN
        ICOORD=3
      ENDIF
C
C-----------------------------------------------------------------------
C       P1 TRIANGLE ROW ELEMENT
C-----------------------------------------------------------------------
C
        IF(IELM1.EQ.11) THEN
C
C.......................................................................
C         P1 TRIANGLE COLUMN ELEMENT
C.......................................................................
C
          IF(IELM2.EQ.11) THEN
             CALL MT12AA(   T(1,1)   ,XM(1,AAQ(1,2,S)),XM(1,AAQ(1,3,S)),
     &                   XM(1,AAQ(2,1,S)),   T(1,2)   ,XM(1,AAQ(2,3,S)),
     &                   XM(1,AAQ(3,1,S)),XM(1,AAQ(3,2,S)),   T(1,3)   ,
     &                   XMUL,SF,SU,SV,F,U,V,XEL,YEL,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                   NELEM,NELMAX,ICOORD)
C
             TYPDIA='Q'
             TYPEXT='Q'
C
C.......................................................................
C
          ELSEIF(IELM2.EQ.12) THEN
             CALL MT12AB
     & (   T(1,1)   ,XM(1,ABQ(1,2,S)),XM(1,ABQ(1,3,S)),XM(1,ABQ(1,4,S)),
     &  XM(1,ABQ(2,1,S)),   T(1,2)   ,XM(1,ABQ(2,3,S)),XM(1,ABQ(2,4,S)),
     &  XM(1,ABQ(3,1,S)),XM(1,ABQ(3,2,S)),   T(1,3)   ,XM(1,ABQ(3,4,S)),
     &          XMUL,SF,SU,SV,F,U,V,
     &          XEL,YEL,SURFAC,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &          NELEM,NELMAX,ICOORD)
C
             TYPDIA='Q'
             TYPEXT='Q'
C
C.......................................................................
C         P2 TRIANGLE COLUMN ELEMENT
C.......................................................................
C
          ELSEIF(IELM2.EQ.13) THEN
             CALL MT12AC(
     &       T(1,1)   ,XM(1,ACQ(1,2,S)),XM(1,ACQ(1,3,S)),
     &       XM(1,ACQ(1,4,S)),XM(1,ACQ(1,5,S)),
     &       XM(1,ACQ(1,6,S)),XM(1,ACQ(2,1,S)),
     &       T(1,2)  ,XM(1,ACQ(2,3,S)),
     &       XM(1,ACQ(2,4,S)),XM(1,ACQ(2,5,S)),
     &       XM(1,ACQ(2,6,S)),XM(1,ACQ(3,1,S)),
     &       XM(1,ACQ(3,2,S)),T(1,3)  ,XM(1,ACQ(3,4,S)),
     &       XM(1,ACQ(3,5,S)),XM(1,ACQ(3,6,S)),
     &          XMUL,SF,SU,SV,F,U,V,
     &          XEL,YEL,SURFAC,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &          IKLE(1,5),IKLE(1,6),
     &          NELEM,NELMAX,ICOORD)
C
             TYPDIA='Q'
             TYPEXT='Q'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       QUASI-BUBBLE TRIANGLE ROW ELEMENT
        ELSEIF(IELM1.EQ.12) THEN
C
C.......................................................................
C         QUASI-BUBBLE TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.12) THEN
             CALL MT12BB
     & (   T(1,1)   ,XM(1,BBQ(1,2,S)),XM(1,BBQ(1,3,S)),XM(1,BBQ(1,4,S)),
     &  XM(1,BBQ(2,1,S)),   T(1,2)   ,XM(1,BBQ(2,3,S)),XM(1,BBQ(2,4,S)),
     &  XM(1,BBQ(3,1,S)),XM(1,BBQ(3,2,S)),   T(1,3)   ,XM(1,BBQ(3,4,S)),
     &  XM(1,BBQ(4,1,S)),XM(1,BBQ(4,2,S)),XM(1,BBQ(4,3,S)),   T(1,4)   ,
     &          XMUL,SF,SU,SV,F,U,V,XEL,YEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &          NELEM,NELMAX,ICOORD)
C
             TYPDIA='Q'
             TYPEXT='Q'
C
          ELSEIF(IELM2.EQ.11) THEN
             CALL MT12BA
     &         (     T(1,1)     ,XM(1,BAQ(1,2,S)),XM(1,BAQ(1,3,S)),
     &          XM(1,BAQ(2,1,S)),     T(1,2)     ,XM(1,BAQ(2,3,S)),
     &          XM(1,BAQ(3,1,S)),XM(1,BAQ(3,2,S)),     T(1,3)     ,
     &          XM(1,BAQ(4,1,S)),XM(1,BAQ(4,2,S)),XM(1,BAQ(4,3,S)),
     &          XMUL,SF,SU,SV,F,U,V,
     &          XEL,YEL,SURFAC,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &          NELEM,NELMAX,ICOORD)
C
             TYPDIA='Q'
             TYPEXT='Q'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       OTHER
C-----------------------------------------------------------------------
C
C       ELSEIF
C
C-----------------------------------------------------------------------
C       ERROR ON THE ROW ELEMENT
C-----------------------------------------------------------------------
C
        ELSE
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
        ENDIF
C
C=======================================================================
C   PSIJ GRAD(PSII) MATRIX  (SIGN HAS CHANGED COMPARED TO 3.0)
C   (PSIJ GRAD(PSII) IN THE CASE OF B/A)
C=======================================================================
C
      ELSEIF(FORMUL(1:15).EQ.'MATGRA         ') THEN
C
C     CHARACTER 16 IS THE SELECTED COORDINATE
C
      IF(FORMUL(16:16).EQ.'X') THEN
        ICOORD=1
      ELSEIF(FORMUL(16:16).EQ.'Y') THEN
        ICOORD=2
      ELSEIF(FORMUL(16:16).EQ.'Z') THEN
        ICOORD=3
      ENDIF
C
C-----------------------------------------------------------------------
C       P1 TRIANGLE ROW ELEMENT
C-----------------------------------------------------------------------
C
        IF(IELM1.EQ.11) THEN
C
C.......................................................................
C         P1 TRIANGLE COLUMN ELEMENT
C.......................................................................
C
          IF(IELM2.EQ.11) THEN
             CALL MT13AA(   T(1,1)   ,XM(1,AAQ(1,2,S)),XM(1,AAQ(1,3,S)),
     &                   XM(1,AAQ(2,1,S)),   T(1,2)   ,XM(1,AAQ(2,3,S)),
     &                   XM(1,AAQ(3,1,S)),XM(1,AAQ(3,2,S)),   T(1,3)   ,
     &                   XMUL,XEL,YEL,NELEM,NELMAX,ICOORD)
C
             TYPDIA='Q'
             TYPEXT='Q'
C
          ELSEIF(IELM2.EQ.12) THEN
             CALL MT13AB(   T(1,1)   ,XM(1,ABQ(1,2,S)),XM(1,ABQ(1,3,S)),
     &                   XM(1,ABQ(1,4,S)),
     &                   XM(1,ABQ(2,1,S)),   T(1,2)   ,XM(1,ABQ(2,3,S)),
     &                   XM(1,ABQ(2,4,S)),
     &                   XM(1,ABQ(3,1,S)),XM(1,ABQ(3,2,S)),   T(1,3)   ,
     &                   XM(1,ABQ(3,4,S)),
     &                   XMUL,XEL,YEL,NELEM,NELMAX,ICOORD)
C
             TYPDIA='Q'
             TYPEXT='Q'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       QUASI-BUBBLE TRIANGLE ROW ELEMENT
        ELSEIF(IELM1.EQ.12) THEN
C
C.......................................................................
C         P1 TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.11) THEN
             CALL MT13BA
     &         (     T(1,1)     ,XM(1,BAQ(1,2,S)),XM(1,BAQ(1,3,S)),
     &          XM(1,BAQ(2,1,S)),     T(1,2)     ,XM(1,BAQ(2,3,S)),
     &          XM(1,BAQ(3,1,S)),XM(1,BAQ(3,2,S)),     T(1,3)     ,
     &          XM(1,BAQ(4,1,S)),XM(1,BAQ(4,2,S)),XM(1,BAQ(4,3,S)),
     &          XMUL,XEL,YEL,NELEM,NELMAX,ICOORD)
C
             TYPDIA='Q'
             TYPEXT='Q'
C
C.......................................................................
C         QUASI-BUBBLE TRIANGLE COLUMN ELEMENT
          ELSEIF(IELM2.EQ.12) THEN
             CALL MT13BB
     & (   T(1,1)   ,XM(1,BBQ(1,2,S)),XM(1,BBQ(1,3,S)),XM(1,BBQ(1,4,S)),
     &  XM(1,BBQ(2,1,S)),   T(1,2)   ,XM(1,BBQ(2,3,S)),XM(1,BBQ(2,4,S)),
     &  XM(1,BBQ(3,1,S)),XM(1,BBQ(3,2,S)),   T(1,3)   ,XM(1,BBQ(3,4,S)),
     &  XM(1,BBQ(4,1,S)),XM(1,BBQ(4,2,S)),XM(1,BBQ(4,3,S)),   T(1,4)   ,
     &          XMUL,XEL,YEL,NELEM,NELMAX,ICOORD)
C
             TYPDIA='Q'
             TYPEXT='Q'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       P2 TRIANGLE ROW ELEMENT
        ELSEIF(IELM1.EQ.13) THEN
C
C.......................................................................
C         P1 TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.11) THEN
             CALL MT13CA
     &         (     T(1,1)     ,XM(1,CAQ(1,2,S)),XM(1,CAQ(1,3,S)),
     &          XM(1,CAQ(2,1,S)),     T(1,2)     ,XM(1,CAQ(2,3,S)),
     &          XM(1,CAQ(3,1,S)),XM(1,CAQ(3,2,S)),     T(1,3)     ,
     &          XM(1,CAQ(4,1,S)),XM(1,CAQ(4,2,S)),XM(1,CAQ(4,3,S)),
     &          XM(1,CAQ(5,1,S)),XM(1,CAQ(5,2,S)),XM(1,CAQ(5,3,S)),
     &          XM(1,CAQ(6,1,S)),XM(1,CAQ(6,2,S)),XM(1,CAQ(6,3,S)),
     &          XMUL,XEL,YEL,NELEM,NELMAX,ICOORD)
C
             TYPDIA='Q'
             TYPEXT='Q'
C
C.......................................................................
C         P2 TRIANGLE COLUMN ELEMENT
          ELSEIF(IELM2.EQ.13) THEN
             CALL MT13CC
     & (   T(1,1)   ,XM(1,PPQ(1,2,S)),XM(1,PPQ(1,3,S)),
     &   XM(1,PPQ(1,4,S)),XM(1,PPQ(1,5,S)),XM(1,PPQ(1,6,S)),
     &   XM(1,PPQ(2,1,S)),T(1,2),XM(1,PPQ(2,3,S)),XM(1,PPQ(2,4,S)),
     &   XM(1,PPQ(2,5,S)),XM(1,PPQ(2,6,S)),XM(1,PPQ(3,1,S)),
     &   XM(1,PPQ(3,2,S)),T(1,3),XM(1,PPQ(3,4,S)),XM(1,PPQ(3,5,S)),
     &   XM(1,PPQ(3,6,S)),XM(1,PPQ(4,1,S)),XM(1,PPQ(4,2,S)),
     &   XM(1,PPQ(4,3,S)),T(1,4),XM(1,PPQ(4,5,S)),XM(1,PPQ(4,6,S)),
     &   XM(1,PPQ(5,1,S)),XM(1,PPQ(5,2,S)),XM(1,PPQ(5,3,S)),
     &   XM(1,PPQ(5,4,S)),  T(1,5)   ,XM(1,PPQ(5,6,S)),
     &   XM(1,PPQ(6,1,S)),XM(1,PPQ(6,2,S)) ,XM(1,PPQ(6,3,S)),
     &   XM(1,PPQ(6,4,S)),XM(1,PPQ(6,5,S)),T(1,6),
     &          XMUL,XEL,YEL,NELEM,NELMAX,ICOORD)
C
             TYPDIA='Q'
             TYPEXT='Q'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C-----------------------------------------------------------------------
C         ERROR ON THE COLUMN ELEMENT
C-----------------------------------------------------------------------
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       OTHER ROW ELEMENT
C-----------------------------------------------------------------------
C
C       ELSEIF
C
C-----------------------------------------------------------------------
C       ERROR ON THE ROW ELEMENT
C-----------------------------------------------------------------------
C
        ELSE
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
        ENDIF
C
C>>>>
C=======================================================================
C     MURD MATRIX
C=======================================================================
C
      ELSEIF(FORMUL(1:6).EQ.'MAMURD') THEN
C
C     CHARACTER 7 INFORMS WHETHER SIGMAG OR NOT
C
      SIGMAG = .FALSE.
      IF(FORMUL(7:7).EQ.'2') SIGMAG = .TRUE.
C
C     CHARACTER 8 GIVES THE DETAILS FOR CALL TO VC04PP
C
      SPECAD = .FALSE.
      IF(FORMUL(8:8).EQ.'2') SPECAD = .TRUE.
C
C     CHARACTERS 14 TO 16 GIVE THE SCHEME OPTION
C
      IF(FORMUL(14:16).EQ.'PSI') LEGO = .FALSE.
C
C-----------------------------------------------------------------------
C       P1 PRISM ROW ELEMENT
        IF(IELM1.EQ.41) THEN
C
C.......................................................................
C         P1 PRISM COLUMN ELEMENT
          IF(IELM2.EQ.41) THEN
             CALL MT14PP(T,XM,PPQ(1,1,S),LEGO,
     &                   XMUL,SU,SV,SW,U,V,W,SF,SG,SH,F,G,H,
     &                   XEL,YEL,ZEL,SURFAC,IKLE,NELEM,NELMAX,SIGMAG,
     &                   SPECAD)
C
            TYPDIA='Q'
            TYPEXT='Q'
C
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C.......................................................................
C         ERROR ON THE COLUMN ELEMENT
C.......................................................................
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       OTHER ROW ELEMENT
C-----------------------------------------------------------------------
C
C       ELSEIF
C
C-----------------------------------------------------------------------
C       ERROR ON THE ROW ELEMENT
C-----------------------------------------------------------------------
C
        ELSE
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
        ENDIF
C<<<<
C
C=======================================================================
C     BOUSSINESQ MATRIX
C=======================================================================
C
      ELSEIF(FORMUL(1:7).EQ.'FFBT   ') THEN
C
C-----------------------------------------------------------------------
C       P1 TRIANGLE ROW ELEMENT
        IF(IELM1.EQ.11) THEN
C
C.......................................................................
C         P1 TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.11) THEN
            CALL MT99AA
     &                  (   T(1,1)   ,XM(1,AAQ(1,2,S)),XM(1,AAQ(1,3,S)),
     &                   XM(1,AAQ(2,1,S)),   T(1,2)   ,XM(1,AAQ(2,3,S)),
     &                   XM(1,AAQ(3,1,S)),XM(1,AAQ(3,2,S)),   T(1,3)   ,
     &                   XMUL,SF,F,XEL,YEL,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                   NELEM,NELMAX,FORMUL,TDIA,TEXT)
C
            TYPDIA = TDIA
            TYPEXT = TEXT
C.......................................................................
C         OTHER
C.......................................................................
C
C         ELSEIF
C
C.......................................................................
C         ERROR ON THE COLUMN ELEMENT
C.......................................................................
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C-----------------------------------------------------------------------
C       OTHER ROW ELEMENT
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
C       QUASI-BUBBLE TRIANGLE ROW ELEMENT
C
        ELSEIF(IELM1.EQ.12) THEN
C
C.......................................................................
C         QUASI-BUBBLE TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.12) THEN
            CALL MT99BB
     & (   T(1,1)   ,XM(1,BBQ(1,2,S)),XM(1,BBQ(1,3,S)),XM(1,BBQ(1,4,S)),
     &  XM(1,BBQ(2,1,S)),   T(1,2)   ,XM(1,BBQ(2,3,S)),XM(1,BBQ(2,4,S)),
     &  XM(1,BBQ(3,1,S)),XM(1,BBQ(3,2,S)),   T(1,3)   ,XM(1,BBQ(3,4,S)),
     &  XM(1,BBQ(4,1,S)),XM(1,BBQ(4,2,S)),XM(1,BBQ(4,3,S)),   T(1,4)   ,
     &         XMUL,SF,F,XEL,YEL,SURFAC,
     &         IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &         NELEM,NELMAX,FORMUL,TDIA,TEXT)
C
C
            TYPDIA = TDIA
            TYPEXT = TEXT
C
C.......................................................................
C         OTHER
C.......................................................................
C
C
C.......................................................................
C         ERROR ON THE COLUMN ELEMENT
C.......................................................................
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
C
C-----------------------------------------------------------------------
C       ERROR ON THE ROW ELEMENT
C-----------------------------------------------------------------------
C
C
        ELSE
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
        ENDIF
C
      ELSE
C
C=======================================================================
C     ERROR: TYPE OF MATRIX NOT IMPLEMENTED
C=======================================================================
C
        IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
        IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
1000  FORMAT(1X,'MATRIY (BIEF) : MATRICE NON PREVUE : ',A16)
1001  FORMAT(1X,'MATRIY (BIEF) : MATRIX NOT IMPLEMENTED:',A16)
2000  FORMAT(1X,'                POUR IELM1 = ',1I6)
2001  FORMAT(1X,'                FOR IELM1 = ',1I6)
3000  FORMAT(1X,'                ET IELM2 = ',1I6)
3001  FORMAT(1X,'                AND IELM2 = ',1I6)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
