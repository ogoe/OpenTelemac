C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ADDS AN OBJECT TO A BLOCK STRUCTURE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BLOC, OBJ
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_ADDBLO
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>POINT_ADJ_T2D(), POINT_ARTEMIS(), POINT_SISYPHE(), POINT_TELEMAC2D(), POINT_TELEMAC3D(), POINT_TOMAWAC(), SOLAUX(), SOLVE()

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
!>          <tr><td>BLOC
!></td><td><-></td><td>NOM FORTRAN DU BLOC
!>    </td></tr>
!>          <tr><td>OBJ
!></td><td>--></td><td>NOUVEL OBJET.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ADDBLO
     &( BLOC , OBJ )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BLOC           |<->| NOM FORTRAN DU BLOC
C| OBJ            |-->| NOUVEL OBJET.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_ADDBLO => ADDBLO
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(INOUT)      :: BLOC
      TYPE(BIEF_OBJ), INTENT(IN), TARGET :: OBJ
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     INCREASES THE NUMBER OF OBJECTS IN THE BLOCK
C
      BLOC%N = BLOC%N + 1
      IF(BLOC%N.GT.BLOC%MAXBLOCK) THEN
       IF(LNG.EQ.1) THEN
         WRITE(LU,*) 'ADDBLO : ',OBJ%NAME,' TROP PETIT'
         WRITE(LU,*) '         AUGMENTER MAXBLOCK DANS ALLBLO'
         WRITE(LU,*) '         (ACTUELLEMENT : ',BLOC%MAXBLOCK,')'
       ENDIF
       IF(LNG.EQ.2) THEN
         WRITE(LU,*) 'ADDBLO : ',OBJ%NAME,' TOO SMALL'
         WRITE(LU,*) '         INCREASE MAXBLOCK IN ALLBLO'
         WRITE(LU,*) '         (CURRENTLY : ',BLOC%MAXBLOCK,')'
       ENDIF
       STOP
      ENDIF
C
C     ASSIGNS THE TARGET OBJ TO THE POINTER OF RANK BLOC%N
C
      BLOC%ADR(BLOC%N)%P => OBJ
C
C-----------------------------------------------------------------------
C
C      IF(LNG.EQ.1) THEN
C        WRITE(LU,*) 'ADDBLO : ',OBJ%NAME,' AJOUTE A ',BLOC%NAME
C      ENDIF
C      IF(LNG.EQ.2) THEN
C        WRITE(LU,*) 'ADDBLO : ',OBJ%NAME,'  ADDED TO ',BLOC%NAME
C      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C