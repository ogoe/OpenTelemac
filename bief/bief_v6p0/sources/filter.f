C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       FILTERS A VECTOR USING A MATRIX.
!><br>            FOR EXAMPLE, THE USE OF A MASS MATRIX YIELDS
!>                SMOOTHING.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  IF BLDMAT=.FALSE. MATRIX A IS GIVEN, IT IS NOT RE-BUILT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A, BLDMAT, F, FORMUL, G, H, MASKEL, MESH, MSK, N, T1, T2, U, V, VEC, W, XMUL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, I
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_FILTER
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> LUMP(), MATRIX(), MATVEC(), OS(), PARCOM()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CORFON(), CORFON(), CORFON(), CORFON(), CORRECTION_DEPTH_2D(), CORRECTION_DEPTH_3D(), RADIA1(), RADIA2()

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
!> </td><td> 24/04/97
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>A
!></td><td><-></td><td>MATRICE (DONNEE OU CONSTRUITE SUIVANT BLDMAT)
!>    </td></tr>
!>          <tr><td>BLDMAT
!></td><td>--></td><td>LOGIQUE : ON CONSTRUIT LA MATRICE OU PAS.
!>    </td></tr>
!>          <tr><td>F,G,H,U,V,W
!></td><td>--></td><td>FONCTIONS INTERVENANT DANS LA MATRICE
!>    </td></tr>
!>          <tr><td>FORMUL
!></td><td>--></td><td>FORMULE DECRIVANT LA MATRICE
!>                  (MEMES CONVENTIONS QUE DANS MATRIX)
!>    </td></tr>
!>          <tr><td>MESH,
!></td><td>--></td><td>BLOCS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>MSK,MASKEL
!></td><td>--></td><td>LOGIQUE ET TABLEAU POUR LE MASQUAGE
!>    </td></tr>
!>          <tr><td>N
!></td><td>--></td><td>NOMBRE DE FOIS OU ON FAIT L'OPERATION.
!>    </td></tr>
!>          <tr><td>T1
!></td><td>--></td><td>TABLEAU DE TRAVAIL.
!>    </td></tr>
!>          <tr><td>T2
!></td><td><-></td><td>TABLEAU DE TRAVAIL. MATRICE A MASS-LUMPEE
!>                  EN SORTIE (VOIR AUSSI XMUL)
!>    </td></tr>
!>          <tr><td>VEC
!></td><td><-></td><td>VECTEUR A FILTRER
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>FACTEUR MULTIPLICATIF NON NUL
!>                  N'A AUCUNE INFLUENCE SAUF SUR T2.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FILTER
     &(VEC,BLDMAT,T1,T2,
     & A,FORMUL,
     & XMUL,F,G,H,U,V,W,
     & MESH,MSK,MASKEL,N)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |<->| MATRICE (DONNEE OU CONSTRUITE SUIVANT BLDMAT)
C| BLDMAT         |-->| LOGIQUE : ON CONSTRUIT LA MATRICE OU PAS.
C| F,G,H,U,V,W    |-->| FONCTIONS INTERVENANT DANS LA MATRICE
C| FORMUL         |-->| FORMULE DECRIVANT LA MATRICE
C|                |   | (MEMES CONVENTIONS QUE DANS MATRIX)
C| MESH,          |-->| BLOCS DU MAILLAGE.
C| MSK,MASKEL     |-->| LOGIQUE ET TABLEAU POUR LE MASQUAGE
C| N             |-->| NOMBRE DE FOIS OU ON FAIT L'OPERATION.
C| T1             |-->| TABLEAU DE TRAVAIL.
C| T2             |<->| TABLEAU DE TRAVAIL. MATRICE A MASS-LUMPEE
C|                |   | EN SORTIE (VOIR AUSSI XMUL)
C| VEC            |<->| VECTEUR A FILTRER
C| XMUL           |-->| FACTEUR MULTIPLICATIF NON NUL
C|                |   | N'A AUCUNE INFLUENCE SAUF SUR T2.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_FILTER => FILTER
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)           :: N
      DOUBLE PRECISION, INTENT(IN)  :: XMUL
      LOGICAL, INTENT(IN)           :: BLDMAT,MSK
      CHARACTER(LEN=16), INTENT(IN) :: FORMUL
      TYPE(BIEF_MESH), INTENT(INOUT):: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT) :: VEC,A,T1,T2
      TYPE(BIEF_OBJ), INTENT(IN)    :: F,G,H,U,V,W,MASKEL
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I
C
      DOUBLE PRECISION C
C
C-----------------------------------------------------------------------
C
      DO 10 I=1,N
C
C  COMPUTES THE MATRIX ACCORDING TO THE GIVEN FORMULATION (OPTIONAL)
C
      IF(BLDMAT.AND.I.EQ.1) THEN
C
          CALL MATRIX(A,'M=N     ',FORMUL,VEC%ELM,VEC%ELM,
     &                XMUL,F,G,H,U,V,W,MESH,MSK,MASKEL)
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C  COMPUTES THE PRODUCT A * VEC (WITH ASSEMBLY)
C
      CALL MATVEC( 'X=AY    ',T1,A,VEC,C,MESH)
      IF(NCSIZE.GT.1) CALL PARCOM(T1,2,MESH)
C
C-----------------------------------------------------------------------
C
C  COMPRESSES A ON ITS DIAGONAL
C
      IF(I.EQ.1) THEN
        CALL LUMP(T2,A,MESH,XMUL)
        IF(NCSIZE.GT.1) CALL PARCOM(T2,2,MESH)
        CALL OS('X=1/Y   ',T2,T2,T2,C,IOPT=2,INFINI=0.D0,ZERO=1.D-20)
      ENDIF
C
C-----------------------------------------------------------------------
C
C  COMPUTES F = A * F / (ASSEMBLED A)
C
C  CHECKS DIVISIONS BY 0 CAUSED BY EXTERNAL POINTS IN
C  THE LEONARD FORMAT, WHICH CAN HAVE 0 VALUES
C
      CALL OS('X=YZ    ',X=VEC,Y=T1,Z=T2)
C
C-----------------------------------------------------------------------
C
10    CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C