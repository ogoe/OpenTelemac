C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       DETERMINES THE LENGTH OF A VECTOR WITHOUT BACK
!>                DEPENDENCIES FOR LOOPS ON THE ELEMENTS.
!><br>            ONLY LOOKS FOR VALUES :
!>                1, 64, 128, 256, 512, OR 1024.
!><br>            THE PRINCIPLE IS TO PERFORM, IN SCALAR AND VECTOR
!>                MODE, AN ALGORITHM WHICH COMPUTES THE NUMBER OF
!>                ADJACENT ELEMENTS AT EACH POINT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  IN VECTOR MODE WITH DEPENDENCIES, THE RESULT IS WRONG

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IKLE, LV, NDP, NELEM, NELMAX, NPOIN, V
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE(), VECLE3(), VECLE4(), VECLE6()
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
!>      <td><center> 5.1                                       </center>
!> </td><td> 11/03/94
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td> ORIGINAL IDEA FROM J.-P. GREGOIRE
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>TABLE DE CONNECTIVITE
!>    </td></tr>
!>          <tr><td>LV
!></td><td><--</td><td>LONGUEUR DE VECTEUR ADMISSIBLE
!>    </td></tr>
!>          <tr><td>NDP
!></td><td>--></td><td>NOMBRE DE SOMMETS PAR ELEMENT
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE TOTAL D'ELEMENTS
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>V
!></td><td>--></td><td>TABLEAU DE TRAVAIL REEL, DE DIMENSION NPOIN
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VECLEN
     & (LV,NDP,IKLE,NELEM,NELMAX,NPOIN,V)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IKLE           |-->| TABLE DE CONNECTIVITE
C| LV             |<--| LONGUEUR DE VECTEUR ADMISSIBLE
C| NDP            |-->| NOMBRE DE SOMMETS PAR ELEMENT
C| NELEM          |-->| NOMBRE D'ELEMENTS
C| NELMAX         |-->| NOMBRE TOTAL D'ELEMENTS
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
C| V             |-->| TABLEAU DE TRAVAIL REEL, DE DIMENSION NPOIN
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF !, EX_VECLEN => VECLEN
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(INOUT) :: LV
      INTEGER, INTENT(IN)    :: NELEM,NELMAX,NDP,NPOIN
      INTEGER, INTENT(IN)    :: IKLE(NELMAX,NDP)
C
      DOUBLE PRECISION, INTENT(INOUT) :: V(NPOIN)
C
C-----------------------------------------------------------------------
C
      IF(NDP.EQ.3) THEN
        CALL VECLE3(LV,IKLE,NELEM,NELMAX,NPOIN,V)
      ELSEIF(NDP.EQ.4) THEN
        CALL VECLE4(LV,IKLE,NELEM,NELMAX,NPOIN,V)
      ELSEIF(NDP.EQ.6) THEN
        CALL VECLE6(LV,IKLE,NELEM,NELMAX,NPOIN,V)
      ELSE
        IF(LNG.EQ.1) WRITE(LU,50) NDP
        IF(LNG.EQ.2) WRITE(LU,60) NDP
50      FORMAT(1X,'VECLEN : VALEUR DE NDP NON PREVUE : ',1I6)
60      FORMAT(1X,'VECLEN : UNEXPECTED VALUE OF NDP: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C