C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SUMS UP THE COMPONENTS OF A VECTOR.
!><br>            X CAN BE A VECTOR, OR
!><br>            A BLOCK STRUCTURE OF VECTORS IN IDENTICAL NUMBER
!>                AND CHARACTERISTICS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  IF THE VECTORS HAVE A SECOND DIMENSION,
!>            IT IS PRESENTLY IGNORED

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> X
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BIEF_SUM
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE(), SOMME()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BILAN(), BILAN_SISYPHE(), DEBIMP(), DEBIMP3D(), MASS3D(), PROPAG(), SUSPENSION_BILAN()

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
!> </td><td> 11/05/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>X
!></td><td>--></td><td>LA STRUCTURE DONT ON VEUT LA SOMME
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        DOUBLE PRECISION FUNCTION BIEF_SUM
     &( X )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| X             |-->| LA STRUCTURE DONT ON VEUT LA SOMME
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_BIEF_SUM => BIEF_SUM
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C-----------------------------------------------------------------------
C
C     STRUCTURES: VECTORS OR BLOCKS
C
      TYPE(BIEF_OBJ), INTENT(IN) :: X
C
C-----------------------------------------------------------------------
C
C  CASE OF A VECTOR
C
      IF(X%TYPE.EQ.2) THEN
C
        BIEF_SUM = SOMME(X%R,X%DIM1*X%DIM2)
C
C-----------------------------------------------------------------------
C
C  CASE WHERE THE STRUCTURES ARE BLOCKS (TO BE CODED UP)
C
C     ELSEIF(X%TYPE.EQ.4) THEN
C
C-----------------------------------------------------------------------
C
C  ERROR
C
      ELSE
C
         IF (LNG.EQ.1) WRITE(LU,50) X%NAME,X%TYPE
         IF (LNG.EQ.1) WRITE(LU,53)
         IF (LNG.EQ.2) WRITE(LU,60) X%NAME,X%TYPE
         IF (LNG.EQ.2) WRITE(LU,63)
50       FORMAT(1X,'BIEF_SUM (BIEF) : NOM DE X : ',A6,'  TYPE : ',1I6)
53       FORMAT(1X,'                  CAS NON PREVU')
60       FORMAT(1X,'BIEF_SUM (BIEF): NAME OF X : ',A6,'  TYPE : ',1I6)
63       FORMAT(1X,'                 CASE NOT IMPLEMENTED')
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