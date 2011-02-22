C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FRICTION VELOCITY U*
!>                FOR ALL THE NODES IN THE 2D MESH.
!><br>            USES A DRAG COEFFICIENT WHICH VARIES LINEARLY WITH
!>                WIND SPEED. THE FORMULATION IS IDENTICAL TO THAT
!>                USED IN WAM-CYCLE 3 (WAMDI GROUP, 1988).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @reference WAMDI GROUP (1988) :
!>                     "A THIRD GENERATION OCEAN WAVE PREDICTION MODEL".
!>                      JPO, VOL 18, PP 1775-1810.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> NPOIN2, USTAR, UV, VV
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CDRAG, IP, UVENT
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SEMIMP(), WAC()

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
!>      <td><center> 1.1                                       </center>
!> </td><td> 26/03/96
!> </td><td> M. BENOIT (EDF/DER/LNH)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE SPATIAL
!>    </td></tr>
!>          <tr><td>USTAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>USTAR(
!></td><td><--</td><td>TABLEAU DES VITESSES DE FROTTEMENT
!>    </td></tr>
!>          <tr><td>UV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UV(
!></td><td>--></td><td>TABLEAU DES COMPOSANTES OUEST-EST DU VENT
!>    </td></tr>
!>          <tr><td>VV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VV(
!></td><td>--></td><td>TABLEAU DES COMPOSANTES SUD-NORD  DU VENT
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE USTAR2
     &( USTAR , UV    , VV    , NPOIN2)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE SPATIAL
C| USTAR          |---| 
C| USTAR(         |<--| TABLEAU DES VITESSES DE FROTTEMENT
C| UV             |---| 
C| UV(            |-->| TABLEAU DES COMPOSANTES OUEST-EST DU VENT
C| VV             |---| 
C| VV(            |-->| TABLEAU DES COMPOSANTES SUD-NORD  DU VENT
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      INTEGER  NPOIN2
      DOUBLE PRECISION USTAR(NPOIN2) , UV(NPOIN2) , VV(NPOIN2)
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER  IP
      DOUBLE PRECISION UVENT , CDRAG
C
C
C.....MAIN LOOP ON THE NODES OF THE 2D MESH
C     """""""""""""""""""""""""""""""""""""""""""""""""""""
      DO 100 IP=1,NPOIN2
C
C.......COMPUTES THE WIND SPEED 10 METERS ABOVE WATER
C       """""""""""""""""""""""""""""""""""""""""
        UVENT=DSQRT(UV(IP)**2+VV(IP)**2)
C
C.......COMPUTES THE DRAG COEFFICIENT
C       """""""""""""""""""""""""""""""""
        CDRAG = 6.5D-5*UVENT + 8.D-4
        IF (UVENT.LT.7.5D0) CDRAG = 1.2875D-3
C
C.......COMPUTES THE FRICTION VELOCITY
C       """""""""""""""""""""""""""""""""""
        USTAR(IP)=DSQRT(CDRAG)*UVENT
C
  100 CONTINUE
C
      RETURN
      END
C
C#######################################################################
C