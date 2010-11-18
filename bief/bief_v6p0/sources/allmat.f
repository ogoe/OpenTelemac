C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ALLOCATES MEMORY FOR A REAL MATRIX STRUCTURE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CFG, IELM1, IELM2, MAT, NOM, TYPDIA, TYPEXT
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELMD, NAME
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_ALLMAT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ALLVEC(), DIM1_EXT(), DIM2_EXT(), NBPTS()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ALMESH(), POINT_ADJ_T2D(), POINT_ARTEMIS(), POINT_SISYPHE(), POINT_TELEMAC2D(), POINT_TELEMAC3D(), POINT_TOMAWAC()

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
!> </td><td> 01/03/95
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CFG(1)
!></td><td>--></td><td>TYPE DE STOCKAGE.
!>    </td></tr>
!>          <tr><td>CFG(2)
!></td><td>--></td><td>CHOIX DU PRODUIT MATRICE X VECTEUR
!>    </td></tr>
!>          <tr><td>IELM1
!></td><td>--></td><td>TYPE D'ELEMENT PAR LIGNES
!>    </td></tr>
!>          <tr><td>IELM2
!></td><td>--></td><td>TYPE D'ELEMENT PAR COLONNES
!>    </td></tr>
!>          <tr><td>MAT
!></td><td>--></td><td>MATRICE A ALLOUER
!>    </td></tr>
!>          <tr><td>NOM
!></td><td>--></td><td>NOM FORTRAN DU TABLEAU
!>    </td></tr>
!>          <tr><td>TYPDIA
!></td><td>--></td><td>TYPE DE DIAGONALE ('Q', 'I' OU '0')
!>    </td></tr>
!>          <tr><td>TYPEXT
!></td><td>--></td><td>TYPE DE TERMES EXTRA-DIAGONAUX:'Q','S' OU '0'
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ALLMAT
     &( MAT , NOM , IELM1 , IELM2 , CFG , TYPDIA , TYPEXT )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CFG(1)         |-->| TYPE DE STOCKAGE.
C| CFG(2)         |-->| CHOIX DU PRODUIT MATRICE X VECTEUR
C| IELM1          |-->| TYPE D'ELEMENT PAR LIGNES
C| IELM2          |-->| TYPE D'ELEMENT PAR COLONNES
C| MAT            |-->| MATRICE A ALLOUER
C| NOM            |-->| NOM FORTRAN DU TABLEAU
C| TYPDIA         |-->| TYPE DE DIAGONALE ('Q', 'I' OU '0')
C| TYPEXT         |-->| TYPE DE TERMES EXTRA-DIAGONAUX:'Q','S' OU '0'
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_ALLMAT => ALLMAT
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: MAT
      CHARACTER(LEN=6), INTENT(IN)    :: NOM
      INTEGER         , INTENT(IN)    :: IELM1,IELM2,CFG(2)
      CHARACTER(LEN=1), INTENT(IN)    :: TYPDIA,TYPEXT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELMD
C
      CHARACTER(LEN=6) :: NAME
C
C-----------------------------------------------------------------------
C  HEADER COMMON TO ALL OBJECTS
C-----------------------------------------------------------------------
C
C     KEY OF THE OBJECT - TO CHECK MEMORY CRASHES
C
      MAT%KEY = 123456
C
C     TYPE OF THE OBJECT (HERE MATRIX)
C
      MAT%TYPE = 3
C
C     NAME OF THE OBJECT
C
      MAT%NAME = NOM
C
C-----------------------------------------------------------------------
C  PART SPECIFIC TO MATRICES
C-----------------------------------------------------------------------
C
C     ELEMENT OF DIAGONAL (SMALLEST ELEMENT IF MATRIX IS RECTANGULAR)
      IELMD = IELM1
      IF(NBPTS(IELM2).LT.NBPTS(IELM1)) IELMD = IELM2
C
C  TYPE OF STORAGE
C
      MAT%STO = CFG(1)
C
C  TYPES OF ELEMENTS FOR LINE AND COLUMN
C
      MAT%ELMLIN = IELM1
      MAT%ELMCOL = IELM2
C
C  ALLOCATES THE DIAGONAL (UNTIL THIS PONIT MAT%D IS ONLY A POINTER)
C
C     MAT%D WILL POINT TO AN EXISTING BIEF_OBJ
      ALLOCATE(MAT%D)
C
      NAME = 'D' // NOM(1:5)
      IF(TYPDIA(1:1).EQ.'Q') THEN
C        ONLY CASE WHERE THE DIAGONAL DOES EXIST
         CALL ALLVEC(1,MAT%D,NAME,IELMD,1,2)
      ELSE
         CALL ALLVEC(1,MAT%D,NAME,0    ,1,0)
      ENDIF
C     TYPE IS FORGOTTEN UNTIL INITIALISATION OF MATRIX
C     MAT%TYPDIA = TYPDIA(1:1)
      MAT%TYPDIA = '?'
C
C     ALLOCATES OFF-DIAGONAL TERMS (AS FOR DIAGONAL)
C
      ALLOCATE(MAT%X)
C
      NAME = 'X' // NOM(1:5)
C
         CALL ALLVEC(1,MAT%X,NAME,
     &               DIM1_EXT(IELM1,IELM2,CFG(1),TYPEXT),
     &               DIM2_EXT(IELM1,IELM2,CFG(1),TYPEXT),0)
C
C     TYPE IS FORGOTTEN UNTIL INITIALISATION OF MATRIX
C     MAT%TYPEXT = TYPEXT(1:1)
      MAT%TYPEXT = '?'
C
C     MATRIX X VECTOR PRODUCT
      MAT%PRO = CFG(2)
C
C-----------------------------------------------------------------------
C
C     IF(LNG.EQ.1) WRITE(LU,*) 'MATRICE : ',NOM,' ALLOUEE'
C     IF(LNG.EQ.2) WRITE(LU,*) 'MATRIX: ',NOM,' ALLOCATED'
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C