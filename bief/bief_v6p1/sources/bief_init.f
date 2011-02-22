C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief  

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CHAINE, CODE, NCAR, PINIT
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::IPID IPID@endlink, 
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> INDAUT, VERAUT
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BIEF_INIT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> AUTORI(), P_INIT(), READ_CONFIG()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>HOMERE_ARTEMIS(), HOMERE_SISYPHE(), HOMERE_TELEMAC2D(), HOMERE_TELEMAC3D(), HOMERE_TOMAWAC()

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
!> </td><td>
!> </td><td> J-M HERVOUET (LNH) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CHAINE
!></td><td><-></td><td>NAME OF CURRENT DIRECTORY
!>    </td></tr>
!>          <tr><td>CODE
!></td><td>--></td><td>NAME OF CALLING PROGRAMME
!>    </td></tr>
!>          <tr><td>NCAR
!></td><td><-></td><td>LENGTH OF CHAIN
!>    </td></tr>
!>          <tr><td>PINIT
!></td><td>--></td><td>LOGICAL, IF YES, INITIALIZE PARALLELISM
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE BIEF_INIT
     &(CODE,CHAINE,NCAR,PINIT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CHAINE         |<->| NAME OF CURRENT DIRECTORY
C| CODE           |-->| NAME OF CALLING PROGRAMME
C| NCAR           |<->| LENGTH OF CHAIN
C| PINIT          |-->| LOGICAL, IF YES, INITIALIZE PARALLELISM
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_BIEF_INIT => BIEF_INIT
      USE DECLARATIONS_TELEMAC
C
      IMPLICIT NONE
      INTEGER     LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CHARACTER(LEN=24), INTENT(IN)     :: CODE
      LOGICAL, INTENT(IN)               :: PINIT
      CHARACTER(LEN=250), INTENT(INOUT) :: CHAINE
      INTEGER, INTENT(INOUT)            :: NCAR
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER INDAUT,VERAUT
C
C-----------------------------------------------------------------------
C
C     TO RUN IN PARALLEL MODE
C
C     P_INIT (PARALLEL) RETURNS THE WORKING DIRECTORY AND ITS LENGTH
C     P_INIT (PARAVOID) RETURNS NCAR = 0 AND NCSIZE = 0
C
      IF(PINIT) CALL P_INIT(CHAINE,NCAR,IPID,NCSIZE)
C
C-----------------------------------------------------------------------
C
C     LANGUAGE AND LOGICAL UNIT FOR OUTPUTS
C
      CALL READ_CONFIG(LNG,LU,CHAINE,NCAR)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
