C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE INCIDENT WAVE IMPOSED AT THE BOUNDARY.
!>  @code
!>     IN EACH NODE OF INCIDENT WAVE, IT IS NECESSARY TO GIVE:<br>
!>     ( 1 - NINC . NBOR ) A COS ( PHI - OMEGA T)<br>
!>     WHERE :<br>
!>     NINC  : DIRECTION OF THE INCIDENT WAVE
!>     NBOR  : NORMAL TO THE WALL (XSGBOR AND YSGBOR)
!>     A     : WAVE AMPLITUDE
!>     OMEGA : WAVE ANGULAR FREQUENCY
!>     PHI   : WAVE PHASE
!>     T     : TIME
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, ATMOS, C0, COTOND, GRAV, H, LT, MESH, PATMOS, PRIVE, ROEAU, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> PI
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>PROPAG()

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
!>      <td><center> 5.2                                       </center>
!> </td><td> 17/08/1994
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ATMOS
!></td><td>--></td><td>LOGIQUE INDIQUANT SI PATMOS EST REMPLI.
!>    </td></tr>
!>          <tr><td>C0
!></td><td>--></td><td>CELERITE DE REFERENCE
!>    </td></tr>
!>          <tr><td>COTOND
!></td><td><--</td><td>ONDE RESULTAT.
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>PESANTEUR
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEUR D'EAU.
!>    </td></tr>
!>          <tr><td>LT,AT
!></td><td>--></td><td>NUMERO DE L'ITERATION,TEMPS
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>STRUCTURE DU MAILLAGE
!>    </td></tr>
!>          <tr><td>PATMOS
!></td><td>--></td><td>PRESSION ATMOSPHERIQUE
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td>--></td><td>TABLEAU DE TRAVAIL DEFINI DANS PRINCI
!>    </td></tr>
!>          <tr><td>ROEAU
!></td><td>--></td><td>MASSE VOLUMIQUE DE L'EAU
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>FOND
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE INCIDE
     &(COTOND,H,C0,PATMOS,ATMOS,ZF,MESH,LT,AT,GRAV,ROEAU,PRIVE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ATMOS          |-->| LOGIQUE INDIQUANT SI PATMOS EST REMPLI.
C| C0             |-->| CELERITE DE REFERENCE
C| COTOND         |<--| ONDE RESULTAT.
C| GRAV           |-->| PESANTEUR
C| H             |-->| HAUTEUR D'EAU.
C| LT,AT          |-->| NUMERO DE L'ITERATION,TEMPS
C| MESH           |-->| STRUCTURE DU MAILLAGE
C| PATMOS         |-->| PRESSION ATMOSPHERIQUE
C| PRIVE          |-->| TABLEAU DE TRAVAIL DEFINI DANS PRINCI
C| ROEAU          |-->| MASSE VOLUMIQUE DE L'EAU
C| ZF             |-->| FOND
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)            :: LT
      LOGICAL, INTENT(IN)            :: ATMOS
      DOUBLE PRECISION, INTENT(IN)   :: AT,GRAV,ROEAU
      TYPE(BIEF_OBJ), INTENT(IN)     :: PATMOS,H,C0,ZF,PRIVE
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: COTOND
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION PI
C
C-----------------------------------------------------------------------
C
      CALL OS( 'X=C     ' , X=COTOND , C=0.D0 )
C
C     PI = 3.141592653589D0
C     T=200.D0
C     W=2.*PI/T
C     A=0.25
C
C      DO 10 K=261,271
C       COTOND%R(K) = 2.*A*SIN(W*AT)
C10    CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C