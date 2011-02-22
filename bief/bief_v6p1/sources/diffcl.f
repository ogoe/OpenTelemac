C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES THE VALUE OF THE TRACER FOR BOUNDARY
!>                CONDITIONS OF TYPE DIRICHLET, IN THE DIFFUSION STEP.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ICONV, LITBOR, NBOR, NPOIN, NPTFR, TBOR, TTILD
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC :<br>
!> @link DECLARATIONS_TELEMAC::ADV_CAR ADV_CAR@endlink, 
!> @link DECLARATIONS_TELEMAC::KSORT KSORT@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> K
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D()

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
!> </td><td> 09/10/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td> MOVED FROM TELEMAC-2D TO ALLOW CALL BY SISYPHE
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ICONV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KSORT
!></td><td>--></td><td>INDICATEUR DE POINT DE SORTIE FLUIDE .
!>    </td></tr>
!>          <tr><td>LITBOR
!></td><td>--></td><td>TYPES DE CONDITIONS AUX LIMITES DU TRACEUR.
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>ADRESSES DES POINTS FRONTIERES.
!>                  CONDITIONS AUX LIMITES (PHYSIQUE) .
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS TOTAL .
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERES .
!>    </td></tr>
!>          <tr><td>TBOR
!></td><td><-></td><td>CONDITIONS AUX LIMITES SUR T.
!>    </td></tr>
!>          <tr><td>TTILD
!></td><td>--></td><td>TRACEUR.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DIFFCL
     &(LITBOR,TTILD,TBOR,NBOR,ICONV,NPOIN,NPTFR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ICONV          |---| 
C| KSORT          |-->| INDICATEUR DE POINT DE SORTIE FLUIDE .
C| LITBOR         |-->| TYPES DE CONDITIONS AUX LIMITES DU TRACEUR.
C| NBOR           |-->| ADRESSES DES POINTS FRONTIERES.
C|                |   | CONDITIONS AUX LIMITES (PHYSIQUE) .
C| NPOIN          |-->| NOMBRE DE POINTS TOTAL .
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES .
C| TBOR           |<->| CONDITIONS AUX LIMITES SUR T.
C| TTILD          |-->| TRACEUR.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE DECLARATIONS_TELEMAC
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NPOIN,NPTFR,ICONV
      INTEGER, INTENT(IN)             :: NBOR(NPTFR)
      INTEGER, INTENT(IN)             :: LITBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: TTILD(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: TBOR(NPTFR)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K
C
C----------------------------------------------------------------------
C
C  INITIALISES TBOR (SEE CONSTRUCTION OF LIMTRA IN DIFFIN)
C
      IF(ICONV.EQ.ADV_CAR) THEN
C
      DO 1 K=1,NPTFR
C
C  IMPOSES THE RESULT OF ADVECTION AT FREE EXITS
C                      WITH THE METHOD OF CHARACTERISTICS
C
        IF(LITBOR(K).EQ.KSORT) TBOR(K) = TTILD(NBOR(K))
C
1     CONTINUE
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