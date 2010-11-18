C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PREPARES SOURCE TERMS FOR DIFFUSION OF TRACERS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC3D :<br>
!> @link DECLARATIONS_TELEMAC3D::NTRAC NTRAC@endlink, 
!> @link DECLARATIONS_TELEMAC3D::S0TA S0TA@endlink, 
!> @link DECLARATIONS_TELEMAC3D::S1TA S1TA@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ITRAC
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC3D()

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
!> </td><td> 21/10/2004
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/06/2001
!> </td><td> CDG/SOGREAH
!> </td><td> TRACER SOURCES
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>S0TA,
!></td><td><--</td><td>TERMES SOURCES EXPLICITES SUR LES TRACEURS
!>    </td></tr>
!>          <tr><td>S1TA
!></td><td><--</td><td>TERMES SOURCES IMPLICITES SUR LES TRACEURS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE SOURCE_TRAC
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| S0TA,          |<--| TERMES SOURCES EXPLICITES SUR LES TRACEURS
C| S1TA           |<--| TERMES SOURCES IMPLICITES SUR LES TRACEURS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!----------------------------------------------------------------------
!
      INTEGER ITRAC
!
!----------------------------------------------------------------------
!
C     SETS SOURCE TERMS TO ZERO
!
      IF(NTRAC.GE.1) THEN
!
C        CALL OS ( 'X=C     ' , X=S0TA , C=0.D0 )
C        CALL OS ( 'X=C     ' , X=S1TA , C=0.D0 )
!
C        SOURCE TERMS SIMPLY MARKED
!
C        BEWARE, PUT Q INSTEAD OF 0 IN TYPR IF NOT NIL
!
         DO ITRAC=1,NTRAC
           S0TA%ADR(ITRAC)%P%TYPR='0'
           S1TA%ADR(ITRAC)%P%TYPR='0'
         ENDDO
!
C        EXAMPLE OF RADIOACTIVE DECAY E**(-KT) ON FIRST TRACER, HERE C=K
!
C        S1TA%ADR(1)%P%TYPR='Q'
C        CALL OS('X=C     ',S1TA%ADR(1)%P,C=1.D0)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C