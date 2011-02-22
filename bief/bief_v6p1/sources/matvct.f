C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MATRIX VECTOR OPERATIONS.
!>  @code
!>   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!>   PERFORMED ON VECTORS X,Y AND MATRIX M.<br>
!>   THE RESULT IS VECTOR X (A NON-ASSEMBLED PART OF WHICH CAN BE IN
!>   ARRAY W IF LEGO = .FALSE.)<br>
!>   THESE OPERATIONS ARE DIFFERENTS DEPENDING ON THE DIAGONAL TYPE
!>   AND THE OFF-DIAGONAL TERMS TYPE.<br>
!>   IMPLEMENTED OPERATIONS :<br>
!>      OP = 'X=AY    '  : X = AY
!>      OP = 'X=X+AY  '  : X = X + AY
!>      OP = 'X=X-AY  '  : X = X - AY
!>      OP = 'X=X+CAY '  : X = X + C AY
!>      OP = 'X=TAY   '  : X = TA Y (TA: TRANSPOSE OF A)
!>      OP = 'X=X+TAY '  : X = X + TA Y
!>      OP = 'X=X-TAY '  : X = X - TA Y
!>      OP = 'X=X+CTAY'  : X = X + C TA Y
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> C, DA, DIMIKM, GLOSEG, IELM1, IELM2, IELMX, IKLE, IKLEM1, LEGO, LIMVOI, LV, MXPTVS, NELEM, NELMAX, NPMAX, NPOIN, NPT, NPTFR, OP, P, S, SIZGLO, SIZXA, TYPDIA, TYPEXT, W, X, XA, Y
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AAQ, AAS, ABQ, ACQ, BAQ, BBQ, BBS, CAQ, NPT2, NSEG1, NSEG2, OOS, PPQ, PPS, SYM, Z
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MATVCT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ASSVEC(), MV0202(), MV0303(), MV0304(), MV0306(), MV0403(), MV0404(), MV0603(), MV0606(), MVSEG(), MW0303(), NBPTS(), NBSEG(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>MATVEC()

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
!> </td><td> 05/02/2010
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td> NSEG1 AND NSEG2 MODIFIED BEFORE CALLING MVSEG
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 31/03/2008
!> </td><td> ALGIANE FROEHLY (MATMECA)
!> </td><td> QUADRATIC TRIANGLE
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>C
!></td><td>--></td><td>CONSTANTE DONNEE
!>    </td></tr>
!>          <tr><td>DA
!></td><td>--></td><td>DIAGONALE DE LA MATRICE.
!>    </td></tr>
!>          <tr><td>DIMIKM
!></td><td>--></td><td>PREMIERE DIMENSION DE IKLEM1.
!>    </td></tr>
!>          <tr><td>GLOSEG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELM1
!></td><td>--></td><td>TYPE D'ELEMENT (LIGNES DE LA MATRICE).
!>    </td></tr>
!>          <tr><td>IELM2
!></td><td>--></td><td>TYPE D'ELEMENT (COLONNES DE LA MATRICE).
!>    </td></tr>
!>          <tr><td>IELMX
!></td><td>--></td><td>TYPE D'ELEMENT DU RESULTAT
!>                  EGAL A IELM1 OU IELM2 SUIVANT L'OPERATION.
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>CORRESPONDANCE NUMEROTATIONS LOCALE ET GLOBALE
!>    </td></tr>
!>          <tr><td>IKLEM1
!></td><td>--></td><td>TABLEAU DE CONNECTIVITE UTILISE POUR LE
!>                  STOCKAGE DE TYPE 2.
!>    </td></tr>
!>          <tr><td>LEGO
!></td><td>--></td><td>= .TRUE. W1,2,... SONT ASSEMBLES SUR X
!>                  =.FALSE. W1,2,... SONT LAISSES TELS QUELS.
!>    </td></tr>
!>          <tr><td>LIMVOI
!></td><td>--></td><td>TABLEAU UTILISE POUR LE STOCKAGE 2.
!>    </td></tr>
!>          <tr><td>LV
!></td><td>--></td><td>LONGUEUR DU VECTEUR POUR LA VECTORISATION.
!>    </td></tr>
!>          <tr><td>MXPTVS
!></td><td>--></td><td>PREMIERE DIMENSION DE LIMVOI.
!>                  NOMBRE MAXIMUM DE VOISINS D'UN POINT
!>    </td></tr>
!>          <tr><td>NELBOR
!></td><td>--></td><td>NUMEROS DES ELEMENTS DE BORD.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NPMAX
!></td><td>--></td><td>NOMBRE MAXIMUM DE POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NPT
!></td><td>--></td><td>DIMENSION DE LA DIAGONALE
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS DE BORD.
!>    </td></tr>
!>          <tr><td>NULONE
!></td><td>--></td><td>NUMEROS LOCAUX DANS L'ELEMENT DES POINTS DE
!>                  BORD.
!>    </td></tr>
!>          <tr><td>OP
!></td><td>--></td><td>OPERATION A EFFECTUER
!>    </td></tr>
!>          <tr><td>P
!></td><td>--></td><td>TYPE DE PRODUIT MATRICE X VECTEUR.
!>    </td></tr>
!>          <tr><td>S
!></td><td>--></td><td>TYPE DE STOCKAGE.
!>    </td></tr>
!>          <tr><td>SIZGLO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SIZXA
!></td><td>--></td><td>FIRST DIMENSION OF ARRAY XA
!>    </td></tr>
!>          <tr><td>TYPDIA
!></td><td>--></td><td>TYPE DE LA DIAGONALE (CHAINE DE CARACTERES)
!>                  TYPDIA = 'Q' : DIAGONALE QUELCONQUE
!>                  TYPDIA = 'I' : DIAGONALE IDENTITE.
!>                  TYPDIA = '0' : DIAGONALE NULLE.
!>    </td></tr>
!>          <tr><td>TYPEXT
!></td><td>--></td><td>TYPE DES TERMES EXTRADIAGONAUX
!>                  TYPEXT = 'Q' : QUELCONQUES.
!>                  TYPEXT = 'S' : SYMETRIQUES.
!>                  TYPEXT = '0' : NULS.
!>    </td></tr>
!>          <tr><td>W
!></td><td><--</td><td>TABLEAUX DE TRAVAIL QUI CONTIENNENT UNE PARTIE
!>                  DU RESULTAT SI L'ON CHOISIT LE MODE NON
!>                  ASSEMBLE ( LEGO = .FALSE. )
!>    </td></tr>
!>          <tr><td>X
!></td><td><--</td><td>VECTEUR IMAGE
!>    </td></tr>
!>          <tr><td>XA
!></td><td>--></td><td>TERMES EXTRA-DIAGONAUX DE LA MATRICE
!>    </td></tr>
!>          <tr><td>Y
!></td><td>--></td><td>VECTEUR OPERANDE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MATVCT
     &(OP, X , DA,TYPDIA,XA,TYPEXT, Y ,
     & C,IKLE,NPT,NELEM,NELMAX,W,LEGO,IELM1,IELM2,IELMX,LV,
     & S,P,IKLEM1,DIMIKM,LIMVOI,MXPTVS,NPMAX,NPOIN,NPTFR,
     & GLOSEG,SIZGLO,SIZXA,NDP,MESH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C              |-->| CONSTANTE DONNEE
C| DA             |-->| DIAGONALE DE LA MATRICE.
C| DIMIKM         |-->| PREMIERE DIMENSION DE IKLEM1.
C| GLOSEG         |---| 
C| IELM1          |-->| TYPE D'ELEMENT (LIGNES DE LA MATRICE).
C| IELM2          |-->| TYPE D'ELEMENT (COLONNES DE LA MATRICE).
C| IELMX          |-->| TYPE D'ELEMENT DU RESULTAT
C|                |   | EGAL A IELM1 OU IELM2 SUIVANT L'OPERATION.
C| IKLE           |-->| CORRESPONDANCE NUMEROTATIONS LOCALE ET GLOBALE
C| IKLEM1         |-->| TABLEAU DE CONNECTIVITE UTILISE POUR LE
C|                |   | STOCKAGE DE TYPE 2.
C| LEGO           |-->| = .TRUE. W1,2,... SONT ASSEMBLES SUR X
C|                |   | =.FALSE. W1,2,... SONT LAISSES TELS QUELS.
C| LIMVOI         |-->| TABLEAU UTILISE POUR LE STOCKAGE 2.
C| LV             |-->| LONGUEUR DU VECTEUR POUR LA VECTORISATION.
C| MXPTVS         |-->| PREMIERE DIMENSION DE LIMVOI.
C|                |   | NOMBRE MAXIMUM DE VOISINS D'UN POINT
C| NELBOR         |-->| NUMEROS DES ELEMENTS DE BORD.
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NPMAX          |-->| NOMBRE MAXIMUM DE POINTS DU MAILLAGE.
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE.
C| NPT            |-->| DIMENSION DE LA DIAGONALE
C| NPTFR          |-->| NOMBRE DE POINTS DE BORD.
C| NULONE         |-->| NUMEROS LOCAUX DANS L'ELEMENT DES POINTS DE
C|                |   | BORD.
C| OP             |-->| OPERATION A EFFECTUER
C| P              |-->| TYPE DE PRODUIT MATRICE X VECTEUR.
C| S              |-->| TYPE DE STOCKAGE.
C| SIZGLO         |---| 
C| SIZXA          |-->| FIRST DIMENSION OF ARRAY XA
C| TYPDIA         |-->| TYPE DE LA DIAGONALE (CHAINE DE CARACTERES)
C|                |   | TYPDIA = 'Q' : DIAGONALE QUELCONQUE
C|                |   | TYPDIA = 'I' : DIAGONALE IDENTITE.
C|                |   | TYPDIA = '0' : DIAGONALE NULLE.
C| TYPEXT         |-->| TYPE DES TERMES EXTRADIAGONAUX
C|                |   | TYPEXT = 'Q' : QUELCONQUES.
C|                |   | TYPEXT = 'S' : SYMETRIQUES.
C|                |   | TYPEXT = '0' : NULS.
C| W              |<--| TABLEAUX DE TRAVAIL QUI CONTIENNENT UNE PARTIE
C|                |   | DU RESULTAT SI L'ON CHOISIT LE MODE NON
C|                |   | ASSEMBLE ( LEGO = .FALSE. )
C| X              |<--| VECTEUR IMAGE
C| XA             |-->| TERMES EXTRA-DIAGONAUX DE LA MATRICE
C| Y              |-->| VECTEUR OPERANDE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_MATVCT => MATVCT
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: IELM1,IELM2,IELMX,NPOIN,NPMAX,S,P,SIZXA
      INTEGER, INTENT(IN)    :: NDP
      INTEGER, INTENT(INOUT) :: NPT
      INTEGER, INTENT(IN) :: NELEM,NELMAX,LV,DIMIKM,MXPTVS,NPTFR,SIZGLO
      INTEGER, INTENT(IN) :: IKLE(NELMAX,*),IKLEM1(*),LIMVOI(*)
      INTEGER, INTENT(IN) :: GLOSEG(SIZGLO,2)
      CHARACTER(LEN=8), INTENT(IN)    :: OP
      CHARACTER(LEN=1),INTENT(IN)     :: TYPDIA,TYPEXT
      DOUBLE PRECISION, INTENT(INOUT) :: X(*)
      DOUBLE PRECISION, INTENT(IN)    :: Y(*),DA(*),XA(SIZXA,*),C
      DOUBLE PRECISION, INTENT(INOUT) :: W(NELMAX,*)
      LOGICAL, INTENT(IN)             :: LEGO
      TYPE(BIEF_MESH), INTENT(IN)     :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER NSEG1,NSEG2,SYM,NPT2
C
      INTEGER AAQ(3,3,2),ABQ(3,4,2),BAQ(4,3,2)
      INTEGER ACQ(3,6,2),BBQ(4,4,2),CAQ(6,3,2),PPQ(6,6,2)
      INTEGER AAS(3,3,2),BBS(4,4,2),PPS(6,6,2)
      INTEGER OOS(2,2,2)
C
      DOUBLE PRECISION Z(1)
C
      INTRINSIC MIN
C
C     THESE DATA ALSO APPEAR IN MATVCT
C
C     DATA OOS/  0 ,  1 ,
C    *           1 ,  0 ,
C S=2 NOT IMPLEMENTED
C    *           0 ,  0 ,
C    *           0 ,  0 /
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
C     NONSYMMETRICAL P1 P2 EBE (S=1)
      DATA ACQ/   0 ,  6 ,  11,
     &            1 ,  0 ,  12,
     &            2 ,  7 ,  0 ,
     &            3 ,  8 ,  13,
     &         	  4 ,  9 ,  14,
     &		  5 ,  10,  15,
C S=2 NOT IMPLEMENTED
     &            0 ,  0 ,  0 ,
     &            0 ,  0 ,  0 ,
     &            0 ,  0 ,  0 ,
     &            0 ,  0 ,  0 ,
     &         	  0 ,  0 ,  0 ,
     &		  0 ,  0 ,  0 /
C     NONSYMMETRICAL QUASI-BUBBLE P1 EBE (S=1)
      DATA BAQ/  0 ,  3 ,  5 ,  7 ,
     &           1 ,  0 ,  6 ,  8 ,
     &           2 ,  4 ,  0 ,  9 ,
C     NONSYMMETRICAL QUASI-BUBBLE P1 PRE-ASSEMBLED EBE (S=2)
     &           0 ,  7 ,  3 ,  4 ,
     &           1 ,  0 ,  8 ,  5 ,
     &           9 ,  2 ,  0 ,  6 /

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
C     SYMMETRICAL P1-P1 PRISMS AND P2 TRIANGLES EBE (S=1)
      DATA PPS/  0 ,  1 ,  2 ,  3 ,  4 ,  5 ,
     &           1 ,  0 ,  6 ,  7 ,  8 ,  9 ,
     &           2 ,  6 ,  0 , 10 , 11 , 12 ,
     &           3 ,  7 , 10 ,  0 , 13 , 14 ,
     &           4 ,  8 , 11 , 13 ,  0 , 15 ,
     &           5 ,  9 , 12 , 14 , 15 ,  0 ,
C     SYMMETRICAL P1-P1 PRISMS AND P2 TRIANGLES PRE-ASSEMBLED EBE (S=2)
C     - NOT IMPLEMENTED
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 /
C
C     NONSYMMETRICAL P1-P1 PRISMS AND P2 TRIANGLES EBE (S=1)
      DATA PPQ/  0 , 16 , 17 , 18 , 19 , 20 ,
     &           1 ,  0 , 21 , 22 , 23 , 24 ,
     &           2 ,  6 ,  0 , 25 , 26 , 27 ,
     &           3 ,  7 , 10 ,  0 , 28 , 29 ,
     &           4 ,  8 , 11 , 13 ,  0 , 30 ,
     &           5 ,  9 , 12 , 14 , 15 ,  0 ,
C    NONSYMMETRICAL P1-P1 PRISMS AND P2 TRIANGLES PRE-ASSEMBLED EBE (S=2)
C     - NOT IMPLEMENTED
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 /
C
C-----------------------------------------------------------------------
C
      IF(S.EQ.1) THEN
C
C-----------------------------------------------------------------------
C
C     TRADITIONAL EBE STORAGE AND TRADITIONAL EBE MATRIX X VECTOR PRODUCT
C
C-----------------------------------------------------------------------
C
      IF(IELM1.EQ.1) THEN
C
        IF(IELM2.EQ.1) THEN
          CALL MV0202(OP, X , DA,TYPDIA,
     &                XA(1,1),XA(1,2),TYPEXT, Y,C,
     &                IKLE(1,1),IKLE(1,2),
     &                NPT,NELEM,W(1,1),W(1,2))
        ELSE
          IF (LNG.EQ.1) WRITE(LU,100) IELM1,IELM2,S
          IF (LNG.EQ.2) WRITE(LU,101) IELM1,IELM2,S
          CALL PLANTE(1)
          STOP
        ENDIF
C
      ELSEIF(IELM1.EQ.11) THEN
C
        IF(IELM2.EQ.11) THEN
          IF(TYPEXT(1:1).EQ.'S') THEN
            CALL MV0303(OP, X , DA,TYPDIA,
     &                  XA(1,AAS(1,2,S)),
     &                  XA(1,AAS(1,3,S)),
     &                  XA(1,AAS(2,1,S)),
     &                  XA(1,AAS(2,3,S)),
     &                  XA(1,AAS(3,1,S)),
     &                  XA(1,AAS(3,2,S)),
     &                  TYPEXT,Y,C,
     &                  IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                  NPT,NELEM,
     &                  W(1,1),W(1,2),W(1,3))
          ELSE
            CALL MV0303(OP, X , DA,TYPDIA,
     &                  XA(1,AAQ(1,2,S)),
     &                  XA(1,AAQ(1,3,S)),
     &                  XA(1,AAQ(2,1,S)),
     &                  XA(1,AAQ(2,3,S)),
     &                  XA(1,AAQ(3,1,S)),
     &                  XA(1,AAQ(3,2,S)),
     &                  TYPEXT,Y,C,
     &                  IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                  NPT,NELEM,
     &                  W(1,1),W(1,2),W(1,3))
          ENDIF
C
        ELSEIF(IELM2.EQ.12) THEN
C
          CALL MV0304(OP, X , DA,TYPDIA,
     &                XA(1,ABQ(1,2,S)),
     &                XA(1,ABQ(1,3,S)),
     &                XA(1,ABQ(1,4,S)),
     &                XA(1,ABQ(2,1,S)),
     &                XA(1,ABQ(2,3,S)),
     &                XA(1,ABQ(2,4,S)),
     &                XA(1,ABQ(3,1,S)),
     &                XA(1,ABQ(3,2,S)),
     &                XA(1,ABQ(3,4,S)),
     &                TYPEXT, Y,C,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                NPT,NELEM,
     &                W(1,1),W(1,2),W(1,3),W(1,4))
C
         ELSEIF(IELM2.EQ.13) THEN
C
          NPT2=BIEF_NBPTS(IELM2,MESH)
          CALL MV0306(OP, X , DA,TYPDIA,
     &                XA(1,ACQ(1,2,S)), XA(1,ACQ(1,3,S)),
     &                XA(1,ACQ(1,4,S)), XA(1,ACQ(1,5,S)),
     &                XA(1,ACQ(1,6,S)), XA(1,ACQ(2,1,S)),
     &                XA(1,ACQ(2,3,S)), XA(1,ACQ(2,4,S)),
     &                XA(1,ACQ(2,5,S)), XA(1,ACQ(2,6,S)),
     &                XA(1,ACQ(3,1,S)), XA(1,ACQ(3,2,S)),
     &                XA(1,ACQ(3,4,S)), XA(1,ACQ(3,5,S)),
     &                XA(1,ACQ(3,6,S)),
     &                TYPEXT, Y,C,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                IKLE(1,4),IKLE(1,5),IKLE(1,6),
     &                NPT,NPT2,NELEM,
     &                W(1,1),W(1,2),W(1,3),
     &                W(1,4),W(1,5),W(1,6))
C
        ELSE
          IF (LNG.EQ.1) WRITE(LU,100) IELM1,IELM2,S
          IF (LNG.EQ.2) WRITE(LU,101) IELM1,IELM2,S
          CALL PLANTE(1)
          STOP
        ENDIF
C
      ELSEIF(IELM1.EQ.12.OR.IELM2.EQ.31.OR.IELM2.EQ.51) THEN
C
        IF(IELM2.EQ.12.OR.IELM2.EQ.31.OR.IELM2.EQ.51) THEN
          IF(TYPEXT(1:1).EQ.'S') THEN
            CALL MV0404(OP, X , DA,TYPDIA,
     &                  XA(1,BBS(1,2,S)),
     &                  XA(1,BBS(1,3,S)),
     &                  XA(1,BBS(1,4,S)),
     &                  XA(1,BBS(2,1,S)),
     &                  XA(1,BBS(2,3,S)),
     &                  XA(1,BBS(2,4,S)),
     &                  XA(1,BBS(3,1,S)),
     &                  XA(1,BBS(3,2,S)),
     &                  XA(1,BBS(3,4,S)),
     &                  XA(1,BBS(4,1,S)),
     &                  XA(1,BBS(4,2,S)),
     &                  XA(1,BBS(4,3,S)),
     &                  TYPEXT, Y,C,
     &                  IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                  NPT,NELEM,
     &                  W(1,1),W(1,2),W(1,3),W(1,4))
          ELSE
            CALL MV0404(OP, X , DA,TYPDIA,
     &                  XA(1,BBQ(1,2,S)),
     &                  XA(1,BBQ(1,3,S)),
     &                  XA(1,BBQ(1,4,S)),
     &                  XA(1,BBQ(2,1,S)),
     &                  XA(1,BBQ(2,3,S)),
     &                  XA(1,BBQ(2,4,S)),
     &                  XA(1,BBQ(3,1,S)),
     &                  XA(1,BBQ(3,2,S)),
     &                  XA(1,BBQ(3,4,S)),
     &                  XA(1,BBQ(4,1,S)),
     &                  XA(1,BBQ(4,2,S)),
     &                  XA(1,BBQ(4,3,S)),
     &                  TYPEXT, Y,C,
     &                  IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                  NPT,NELEM,
     &                  W(1,1),W(1,2),W(1,3),W(1,4))
          ENDIF
        ELSEIF(IELM2.EQ.11) THEN
          CALL MV0403(OP, X , DA,TYPDIA,
     &                XA(1,BAQ(1,2,S)),
     &                XA(1,BAQ(1,3,S)),
     &                XA(1,BAQ(2,1,S)),
     &                XA(1,BAQ(2,3,S)),
     &                XA(1,BAQ(3,1,S)),
     &                XA(1,BAQ(3,2,S)),
     &                XA(1,BAQ(4,1,S)),
     &                XA(1,BAQ(4,2,S)),
     &                XA(1,BAQ(4,3,S)),
     &                TYPEXT, Y,C,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                NPT,NELEM,
     &                W(1,1),W(1,2),W(1,3),W(1,4))
        ELSE
          IF (LNG.EQ.1) WRITE(LU,100) IELM1,IELM2,S
          IF (LNG.EQ.2) WRITE(LU,101) IELM1,IELM2,S
          CALL PLANTE(1)
          STOP
        ENDIF
C
      ELSEIF(IELM1.EQ.41.OR.IELM1.EQ.13) THEN
C
        IF(IELM2.EQ.41.OR.IELM2.EQ.13) THEN
C
          CALL MV0606(OP, X , DA,TYPDIA,XA,TYPEXT, Y,C,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                IKLE(1,4),IKLE(1,5),IKLE(1,6),
     &                NPT,NELEM,NELMAX,
     &                W(1,1),W(1,2),W(1,3),W(1,4),W(1,5),W(1,6))
C
        ELSEIF(IELM2.EQ.11) THEN
C
C         HERE IELM1=13
          NPT2=BIEF_NBPTS(IELM1,MESH)
          CALL MV0603(OP, X , DA,TYPDIA,
     &                XA(1,CAQ(1,2,S)),XA(1,CAQ(1,3,S)),
     &                XA(1,CAQ(2,1,S)),XA(1,CAQ(2,3,S)),
     &                XA(1,CAQ(3,1,S)),XA(1,CAQ(3,2,S)),
     &                XA(1,CAQ(4,1,S)),XA(1,CAQ(4,2,S)),
     &                XA(1,CAQ(4,3,S)),XA(1,CAQ(5,1,S)),
     &                XA(1,CAQ(5,2,S)),XA(1,CAQ(5,3,S)),
     &                XA(1,CAQ(6,1,S)),XA(1,CAQ(6,2,S)),
     &                XA(1,CAQ(6,3,S)),
     &                TYPEXT, Y,C,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                IKLE(1,4),IKLE(1,5),IKLE(1,6),
     &                NPT,NPT2,NELEM,
     &                W(1,1),W(1,2),W(1,3),
     &                W(1,4),W(1,5),W(1,6))
C
        ELSE
          IF (LNG.EQ.1) WRITE(LU,100) IELM1,IELM2,S
          IF (LNG.EQ.2) WRITE(LU,101) IELM1,IELM2,S
          CALL PLANTE(1)
          STOP
        ENDIF
C
C  IELM1 NOT IMPLEMENTED : ERROR
C
      ELSE
C
        IF (LNG.EQ.1) WRITE(LU,100) IELM1,IELM2,S
        IF (LNG.EQ.2) WRITE(LU,101) IELM1,IELM2,S
100     FORMAT(1X,'MATVCT (BIEF) : ELEMENTS ',1I2,' ET ',1I2,/,1X,
     &            'ET STOCKAGE ',1I2,'   CAS NON PREVU')
101     FORMAT(1X,'MATVCT (BIEF) : ELEMENTS ',1I2,' AND ',1I2,/,1X,
     &            'AND STORAGE ',1I2,'   CASE NOT IMPLEMENTED')
        CALL PLANTE(1)
        STOP
C
      ENDIF
C
C     POSSIBLE FINAL ASSEMBLY OF X
C
C     SINCE INIT = FALSE HERE, MAY NOT NEED NPT
      NPT = BIEF_NBPTS(IELMX,MESH)
      IF(LEGO) CALL ASSVEC(X,IKLE,NPT,NELEM,NELMAX,IELMX,W,
     &                     .FALSE.,LV,.FALSE.,Z,NDP)
C
      ELSEIF(S.EQ.3.AND.P.EQ.2) THEN
C
C-----------------------------------------------------------------------
C
C  SEGMENT STORAGE AND FRONTAL MATRIX X VECTOR PRODUCT
C
C-----------------------------------------------------------------------
C
      IF(IELM1.EQ.1) THEN
C
        IF(IELM2.EQ.1) THEN
C         CALL MW0202(OP, X , DA,TYPDIA,
C    *                XA(1,1),XA(1,2),TYPEXT, Y,C,
C    *                IKLE(1,1),IKLE(1,2),
C    *                NPT,NELEM,
C    *                W(1,1),W(1,2))
C       ELSE
          IF (LNG.EQ.1) WRITE(LU,100) IELM1,IELM2,S
          IF (LNG.EQ.2) WRITE(LU,101) IELM1,IELM2,S
          CALL PLANTE(1)
          STOP
        ENDIF
C
      ELSEIF(IELM1.EQ.11) THEN
C
        IF(IELM2.EQ.11) THEN
C
          CALL MW0303(OP, X , DA,TYPDIA,XA,TYPEXT, Y,C,
     &                IKLEM1,DIMIKM,LIMVOI,MXPTVS,NPMAX,NPOIN,W)
C
        ELSEIF(IELM2.EQ.12) THEN
C
C         CALL MW0304(OP, X , DA,TYPDIA,
C    *                XA(1,1),XA(1,2),XA(1,3),
C    *                XA(1,4),XA(1,5),XA(1,6),
C    *                XA(1,7),XA(1,8),XA(1,9),
C    *                TYPEXT, Y,C,
C    *                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
C    *                NPT,NELEM,
C    *                W(1,1),W(1,2),W(1,3),W(1,4))
C       ELSE
          IF (LNG.EQ.1) WRITE(LU,100) IELM1,IELM2,S
          IF (LNG.EQ.2) WRITE(LU,101) IELM1,IELM2,S
          CALL PLANTE(1)
          STOP
        ENDIF
C
      ELSEIF(IELM1.EQ.12) THEN
C
        IF(IELM2.EQ.12) THEN
C           CALL MW0404(OP, X , DA,TYPDIA,
C    *                  XA(1,1),XA(1,2),XA(1,3),XA(1,1),
C    *                  XA(1,4),XA(1,5),XA(1,2),XA(1,4),
C    *                  XA(1,6),XA(1,3),XA(1,5),XA(1,6),
C    *                  TYPEXT, Y,C,
C    *                  IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
C    *                  NPT,NELEM,
C    *                  W(1,1),W(1,2),W(1,3),W(1,4))
C       ELSEIF(IELM2.EQ.11) THEN
C         CALL MW0403(OP, X , DA,TYPDIA,
C    *                XA(1,1),XA(1,2),XA(1,3),
C    *                XA(1,4),XA(1,5),XA(1,6),
C    *                XA(1,7),XA(1,8),XA(1,9),
C    *                TYPEXT, Y,C,
C    *                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
C    *                NPT,NELEM,
C    *                W(1,1),W(1,2),W(1,3),W(1,4))
C       ELSE
          IF (LNG.EQ.1) WRITE(LU,100) IELM1,IELM2,S
          IF (LNG.EQ.2) WRITE(LU,101) IELM1,IELM2,S
          CALL PLANTE(1)
          STOP
        ENDIF
C
      ELSEIF(IELM1.EQ.41) THEN
C
        IF(IELM2.EQ.41) THEN
C
C         CALL MW0606(OP, X , DA,TYPDIA,XA,TYPEXT, Y,C,
C    *                IKLE(1,1),IKLE(1,2),IKLE(1,3),
C    *                IKLE(1,4),IKLE(1,5),IKLE(1,6),
C    *                NPT,NELEM,NELMAX,
C    *                W(1,1),W(1,2),W(1,3),W(1,4),W(1,5),W(1,6))
C       ELSE
          IF (LNG.EQ.1) WRITE(LU,100) IELM1,IELM2,S
          IF (LNG.EQ.2) WRITE(LU,101) IELM1,IELM2,S
          CALL PLANTE(1)
          STOP
        ENDIF
C
C  IELM1 NOT IMPLEMENTED : ERROR
C
      ELSE
C
        IF (LNG.EQ.1) WRITE(LU,100) IELM1,IELM2,S
        IF (LNG.EQ.2) WRITE(LU,101) IELM1,IELM2,S
        CALL PLANTE(1)
        STOP
C
      ENDIF
C
C  STORAGE BY SEGMENTS
C
      ELSEIF(S.EQ.3.AND.P.EQ.1) THEN
C
C-----------------------------------------------------------------------
C
C  SEGMENT STORAGE AND TRADITIONAL MATRIX X VECTOR PRODUCT
C
C-----------------------------------------------------------------------
C
      NSEG1 = BIEF_NBSEG(IELM1,MESH)
      NSEG2 = BIEF_NBSEG(IELM2,MESH)
C
C     IN LINEAR-QUADRATIC RECTANGULAR MATRICES, PURELY QUADRATIC
C     SEGMENTS ARE NOT CONSIDERED (NUMBER 13,14 AND 15, SO 3 PER ELEMENT)
C
      IF(IELM1.EQ.11.AND.IELM2.EQ.13) THEN
        NSEG2=NSEG2-3*NELEM
      ELSEIF(IELM1.EQ.13.AND.IELM2.EQ.11) THEN
        NSEG1=NSEG1-3*NELEM
      ENDIF
C
      IF(TYPEXT(1:1).EQ.'Q') THEN
        SYM = MIN(NSEG1,NSEG2)
      ELSE
        SYM = 0
      ENDIF
      CALL MVSEG (OP, X , DA,TYPDIA,XA(1,1),XA(SYM+1,1),
     &            TYPEXT,Y,C,NPT,NELEM,NSEG1,NSEG2,
     &            GLOSEG(1,1),GLOSEG(1,2),IELM1,IELM2)
C
C-----------------------------------------------------------------------
C
C  STORAGE NOT IMPLEMENTED
C
C-----------------------------------------------------------------------
C
      ELSE
        IF (LNG.EQ.1) WRITE(LU,102) S,P
        IF (LNG.EQ.2) WRITE(LU,103) S,P
102     FORMAT(1X,'MATVCT (BIEF) : ',1I2,' ET ',1I2,/,1X,
     &            'STOCKAGE ET PRODUIT MATRICE-VECTEUR INCOMPATIBLES')
103     FORMAT(1X,'MATVCT (BIEF) : ',1I2,' AND ',1I2,/,1X,
     &            'STORAGE AND MATRIX-VECTOR PRODUCT INCOMPATIBLE')
        CALL PLANTE(1)
        STOP
      ENDIF
C
C=======================================================================
C
      RETURN
      END
C
C#######################################################################
C
