C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       EXTENDS THE CONNECTIVITY TABLE.
!>                CASE OF AN EXTENSION TO QUADRATIC ELEMENTS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ELTSEG, IKLBOR, IKLE, NBOR, NELEM, NELMAX, NPOIN, NPTFR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELEM, K
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>COMP_IKLE()

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
!> </td><td> 20/03/08
!> </td><td> J-M HERVOUET (LNH) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ELTSEG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td><-></td><td>TABLEAU DES CONNECTIVITES
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE SOMMETS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CPIK13
     &(IKLE,IKLBOR,ELTSEG,NBOR,NELEM,NELMAX,NPOIN,NPTFR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ELTSEG         |---| 
C| IKLBOR         |---| 
C| IKLE           |<->| TABLEAU DES CONNECTIVITES
C| NBOR           |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS
C| NPOIN          |-->| NOMBRE DE SOMMETS DU MAILLAGE
C| NPTFR          |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NELEM,NELMAX,NPOIN,NPTFR
      INTEGER, INTENT(IN)    :: ELTSEG(NELMAX,3)
      INTEGER, INTENT(INOUT) :: IKLE(NELMAX,6),IKLBOR(NPTFR,*),NBOR(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,K
C
C-----------------------------------------------------------------------
C
C     CONNECTIVITY TABLE OF QUADRATIC GLOBAL POINTS
C
      DO IELEM = 1 , NELEM
C
C       NUMBER=NPOIN+NUMBER OF THE SEGMENT CONTAINING THE POINT
C
        IKLE(IELEM,4) = NPOIN + ELTSEG(IELEM,1)
        IKLE(IELEM,5) = NPOIN + ELTSEG(IELEM,2)
        IKLE(IELEM,6) = NPOIN + ELTSEG(IELEM,3)
C
      ENDDO
C
C-----------------------------------------------------------------------
C
C     CONNECTIVITY TABLE OF QUADRATIC BOUNDARY POINTS
C     GLOBAL NUMBERS OF BOUNDARY QUADRATIC POINTS
C
      DO K=1,NPTFR
        IKLBOR(K,3)=K+NPTFR
C       SEGMENTS 1 TO NPTFR ARE THE BOUNDARY SEGMENTS
        NBOR(IKLBOR(K,3))=NPOIN+K
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C