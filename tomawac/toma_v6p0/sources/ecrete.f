C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES THE VARIANCE SPECTRUM (SETS IT TO 0) AT
!>                ALL THE NODES WHERE THE DEPTH OF WATER IS LESS
!>                THAN PROMIN.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DEPTH, F, NF, NPLAN, NPOIN2, PROMIN
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IP, JF, JP
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>WAC()

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
!>      <td><center> 5.4                                       </center>
!> </td><td> 19/01/2004
!> </td><td> M. BENOIT (EDF LNHE) 01 30 87 83 51
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DEPTH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DEPTH(
!></td><td>--></td><td>VECTEUR DES PROFONDEURS D'EAU
!>    </td></tr>
!>          <tr><td>F
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F(
!></td><td>---</td><td>DENSITE SPECTRALE
!>    </td></tr>
!>          <tr><td>NF
!></td><td>--></td><td>NOMBRE DE FREQUENCES
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE DIRECTIONS
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS 2D
!>    </td></tr>
!>          <tr><td>PROMIN
!></td><td>--></td><td>PROFONDEUR MINIMALE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ECRETE
     &( F     , DEPTH , NPOIN2, NPLAN , NF    , PROMIN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DEPTH          |---| 
C| DEPTH(         |-->| VECTEUR DES PROFONDEURS D'EAU
C| F             |---| 
C| F(             |---| DENSITE SPECTRALE
C| NF             |-->| NOMBRE DE FREQUENCES
C| NPLAN          |-->| NOMBRE DE DIRECTIONS
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| PROMIN         |-->| PROFONDEUR MINIMALE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
      INTEGER          NPOIN2 , NPLAN, NF
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF), DEPTH(NPOIN2)
      DOUBLE PRECISION PROMIN
      INTEGER          IP    , JP    , JF
C
      DO IP=1,NPOIN2
        IF (DEPTH(IP).LT.PROMIN) THEN
          DO JF=1,NF
            DO JP=1,NPLAN
              F(IP,JP,JF)=0.0D0
            ENDDO
          ENDDO
        ENDIF
      ENDDO
C
      RETURN
      END
C
C#######################################################################
C