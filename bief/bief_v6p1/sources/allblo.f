C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ALLOCATES MEMORY FOR A BLOCK STRUCTURE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BLO, NOM
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ERR
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_ALLBLO
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ALLBLO_IN_BLOCK(), POINT_ADJ_T2D(), POINT_ARTEMIS(), POINT_SISYPHE(), POINT_TELEMAC2D(), POINT_TELEMAC3D(), POINT_TOMAWAC(), SOLVE()

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
!> </td><td> 10/01/95
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>BLO
!></td><td>--></td><td>BLOC RESULTAT
!>    </td></tr>
!>          <tr><td>NOM
!></td><td>--></td><td>NOM FORTRAN DU TABLEAU
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ALLBLO
     &( BLO , NOM )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BLO            |-->| BLOC RESULTAT
C| NOM            |-->| NOM FORTRAN DU TABLEAU
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_ALLBLO => ALLBLO
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: BLO
      CHARACTER(LEN=6), INTENT(IN)    :: NOM
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ERR
C
C-----------------------------------------------------------------------
C  COMMON PART FOR ALL OBJECTS
C-----------------------------------------------------------------------
C
C     KEY OF THE OBJECT
C
      BLO%KEY = 123456
C
C     TYPE OF THE OBJECT
C
      BLO%TYPE = 4
C
C     NAME OF THE OBJECT
C
      BLO%NAME = NOM
C
C-----------------------------------------------------------------------
C  PART SPECIFIC TO BLOCKS
C-----------------------------------------------------------------------
C
C     NUMBER OF OBJECTS IN THE BLOCK
C
      BLO%N = 0
C
C     ALLOCATES THE POINTERS ARRAY ADR
C
      BLO%MAXBLOCK = 128
      ALLOCATE(BLO%ADR(BLO%MAXBLOCK),STAT=ERR)
C
C-----------------------------------------------------------------------
C
      IF(ERR.EQ.0) THEN
C       IF(LNG.EQ.1) WRITE(LU,*) 'BLOC : ',NOM,' ALLOUE'
C       IF(LNG.EQ.2) WRITE(LU,*) 'BLOCK: ',NOM,' ALLOCATED'
      ELSE
        IF(LNG.EQ.1) WRITE(LU,10) NOM,ERR
        IF(LNG.EQ.2) WRITE(LU,20) NOM,ERR
10      FORMAT(1X,'ERREUR A L''ALLOCATION DU BLOC : ',A6,/,1X,
     &            'CODE D''ERREUR : ',1I6)
20      FORMAT(1X,'ERROR DURING ALLOCATION OF BLOCK: ',A6,/,1X,
     &            'ERROR CODE: ',1I6)
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