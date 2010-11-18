C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SUMS UP THE TERMS OF MATRIX A, BY LINE.
!><br>            MULTIPLIES THE RESULT BY XMUL.
!><br>            SIMPLY DOES DIAG = A X (VECTOR EQUAL TO XMUL).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A, DIAG, MESH, XMUL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_LUMP
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CPSTVC(), MATVEC(), OS(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BERKHO(), CVDFTR(), DIFF3D(), FILTER(), KEPSIL(), MESH_PROP(), PROPAG(), WAVE_EQUATION()

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
!>      <td><center> 5.5                                       </center>
!> </td><td> 08/12/94
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
!></td><td>--></td><td>MATRICE
!>    </td></tr>
!>          <tr><td>DIAG
!></td><td><--</td><td>VECTEUR RESULTAT.
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES ELEMENTS
!>                  =1. : NORMAL   =0. : ELEMENT MASQUE
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>BLOC DES TABLEAUX ENTIERS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES.
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>COEFFICIENT MULTIPLICATEUR
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE LUMP
     &(DIAG,A,MESH,XMUL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |-->| MATRICE
C| DIAG           |<--| VECTEUR RESULTAT.
C| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
C|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE
C| MESH           |-->| BLOC DES TABLEAUX ENTIERS DU MAILLAGE.
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
C| XMUL           |-->| COEFFICIENT MULTIPLICATEUR
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_LUMP => LUMP
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      DOUBLE PRECISION, INTENT(IN)   :: XMUL
      TYPE(BIEF_OBJ), INTENT(IN)     :: A
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: DIAG
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      DOUBLE PRECISION C
C
C-----------------------------------------------------------------------
C
      IF(A%ELMLIN.NE.A%ELMCOL) THEN
        IF (LNG.EQ.1) WRITE(LU,50)
        IF (LNG.EQ.2) WRITE(LU,51)
50      FORMAT(1X,'LUMP (BIEF) : A N''EST PAS UNE MATRICE CARREE')
51      FORMAT(1X,'LUMP (BIEF) : A IS NOT A SQUARE MATRIX')
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      CALL CPSTVC(A%D,DIAG)
C
C-----------------------------------------------------------------------
C
C  BUILDS A VECTOR THAT IS XMUL EVERYWHERE
C
      CALL OS( 'X=C     ', X=DIAG , C=XMUL )
C
C  DIAG IS THE PRODUCT OF A BY THIS VECTOR
C  DIAG HERE PLAYS THE ROLE OF X AND Y (CAN BE DONE)
C
      CALL MATVEC('X=AY    ',DIAG,A,DIAG,C,MESH,.TRUE.)
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C