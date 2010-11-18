C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       EXTENDS THE CONNECTIVITY TABLES AND ARRAY NBOR.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ELTSEG, IELM, IKLBOR, IKLE, NBOR, NELEM, NELMAX, NPOIN, NPTFR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CPIK12(), CPIK13(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>INBIEF()

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
!> </td><td> 20/03/2008
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ELTSEG
!></td><td>--></td><td>SEGMENT NUMBERS OF AN ELEMENT
!>    </td></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>TYPE OF ELEMENT
!>    </td></tr>
!>          <tr><td>IKLBOR
!></td><td><-></td><td>CONNECTIVITY TABLE FOR BOUNDARY POINTS
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td><-></td><td>CONNECTIVITY TABLE FOR ALL POINTS
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td><-></td><td>GLOBAL NUMBERS OF BOUNDARY POINTS
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
!></td><td>--></td><td>NUMBER OF (LINEAR) BOUNDARY POINTS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE COMP_IKLE
     &(IKLE,IKLBOR,ELTSEG,NBOR,IELM,NELEM,NELMAX,NPOIN,NPTFR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ELTSEG         |-->| SEGMENT NUMBERS OF AN ELEMENT
C| IELM           |-->| TYPE OF ELEMENT
C| IKLBOR         |<->| CONNECTIVITY TABLE FOR BOUNDARY POINTS
C| IKLE           |<->| CONNECTIVITY TABLE FOR ALL POINTS
C| NBOR           |<->| GLOBAL NUMBERS OF BOUNDARY POINTS
C| NELEM          |-->| NOMBRE D'ELEMENTS
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS
C| NPOIN          |-->| NOMBRE DE SOMMETS DU MAILLAGE
C| NPTFR          |-->| NUMBER OF (LINEAR) BOUNDARY POINTS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF   !, EX_COMP_IKLE => COMP_IKLE
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NELEM,NELMAX,IELM,NPOIN,NPTFR
      INTEGER, INTENT(IN)    :: ELTSEG(NELMAX,3)
      INTEGER, INTENT(INOUT) :: IKLE(NELMAX,*),IKLBOR(NPTFR,*),NBOR(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      IF(IELM.EQ.12) THEN
C
        CALL CPIK12(IKLE,NELEM,NELMAX,NPOIN)
C
      ELSEIF(IELM.EQ.13.OR.IELM.EQ.14) THEN
C
        CALL CPIK13(IKLE,IKLBOR,ELTSEG,NBOR,NELEM,NELMAX,NPOIN,NPTFR)
C
      ELSE
C
        IF(LNG.EQ.1) WRITE(LU,10) IELM
        IF(LNG.EQ.2) WRITE(LU,11) IELM
10      FORMAT(1X,'CPIKLE : DISCRETISATION NON PREVUE :'    ,I6)
11      FORMAT(1X,'CPIKLE: DISCRETIZATION NOT IMPLEMENTED:',I6)
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