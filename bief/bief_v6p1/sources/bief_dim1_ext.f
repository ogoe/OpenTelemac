C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       GIVES THE FIRST DIMENSION OF A MATRICE'S EXTRA-DIAGONAL
!>                TERMS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IELM1, IELM2, STO, TYPEXT
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELMX, N
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_DIM1_EXT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> NBMPTS(), NBPEL(), NBSEG(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ALLMAT(), MATRIX(), OM()

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
!> </td><td> J-M HERVOUET (LNH) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IELM1
!></td><td>--></td><td>TYPE DE L'ELEMENT DE LIGNE
!>    </td></tr>
!>          <tr><td>IELM2
!></td><td>--></td><td>TYPE DE L'ELEMENT DE COLONNE
!>    </td></tr>
!>          <tr><td>STO
!></td><td>--></td><td>TYPE DE STOCKAGE
!>    </td></tr>
!>          <tr><td>TYPEXT
!></td><td>--></td><td>TYPE DES TERMES EXTRA-DIAGONAUX
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        INTEGER FUNCTION BIEF_DIM1_EXT
     &(IELM1,IELM2,STO,TYPEXT,MESH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IELM1          |-->| TYPE DE L'ELEMENT DE LIGNE
C| IELM2          |-->| TYPE DE L'ELEMENT DE COLONNE
C| STO            |-->| TYPE DE STOCKAGE
C| TYPEXT         |-->| TYPE DES TERMES EXTRA-DIAGONAUX
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_BIEF_DIM1_EXT => BIEF_DIM1_EXT
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER         , INTENT(IN) :: IELM1,IELM2,STO
      CHARACTER(LEN=1), INTENT(IN) :: TYPEXT
      TYPE(BIEF_MESH) , INTENT(IN) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELMX,N
C
C-----------------------------------------------------------------------
C
      IELMX = 10*(IELM1/10)
C
      IF(TYPEXT.EQ.'0') THEN
C
C        NOT 0 TO ENABLE BOUND CHECKING
         BIEF_DIM1_EXT = 1
C
      ELSEIF(STO.EQ.1) THEN
C
C        CLASSICAL EBE STORAGE
C
         BIEF_DIM1_EXT =BIEF_NBMPTS(IELMX,MESH)
C
      ELSEIF(STO.EQ.3) THEN
C
C        EDGE-BASED STORAGE
C
         IF(TYPEXT.EQ.'S') THEN
           BIEF_DIM1_EXT=BIEF_NBSEG(IELM1,MESH)
         ELSE
           BIEF_DIM1_EXT=BIEF_NBSEG(IELM1,MESH)+BIEF_NBSEG(IELM2,MESH)
           N=MAX(BIEF_NBPEL(IELM1,MESH),BIEF_NBPEL(IELM2,MESH))
     &      -MIN(BIEF_NBPEL(IELM1,MESH),BIEF_NBPEL(IELM2,MESH))
           IF(N.GE.2) THEN
C            SOME SEGMENTS LINK ONLY E.G. QUADRATIC POINTS AND
C            WILL NOT BE CONSIDERED IN A RECTANGULAR MATRIX
C            THIS IS THE CASE WITH 3 SEGMENTS IN QUADRATIC TRIANGLE
             BIEF_DIM1_EXT=BIEF_DIM1_EXT
     &                    -N*(N-1)*BIEF_NBMPTS(IELMX,MESH)/2
           ENDIF
         ENDIF
C
      ELSE
C
        IF(LNG.EQ.1) WRITE(LU,100) STO
        IF(LNG.EQ.2) WRITE(LU,101) STO
100     FORMAT(1X,'BIEF_DIM1_EXT : STOCKAGE NON PREVU : ',1I6)
101     FORMAT(1X,'BIEF_DIM1_EXT : UNKNOWN TYPE OF STORAGE: ',1I6)
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
