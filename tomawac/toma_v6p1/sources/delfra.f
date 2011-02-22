C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE COEFFICIENT THAT NORMALISES THE DIRECTIONAL
!>                SPREADING FUNCTION IN COS **2.S (TETA-TETA0).
!>  @code
!>                               GAMMA( SS + 0.5)
!>        DELFRA(SS) = SQRT(PI)  ----------------
!>                               GAMMA( SS + 1. )
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DEUPI, SS
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DELFRA
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> GAMMLN()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>FSPRD1(), FSPRD3()

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
!>      <td><center> 1.2                                       </center>
!> </td><td> 07/11/96
!> </td><td> M. BENOIT
!> </td><td> MODIFIED
!> </td></tr>
!>      <tr>
!>      <td><center> 1.0                                       </center>
!> </td><td> 15/11/95
!> </td><td> M. BENOIT
!> </td><td> CREATED
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DELFRA
!></td><td><--</td><td>COEFFICIENT DE NORMALISATION DE LA FRA
!>    </td></tr>
!>          <tr><td>DEUPI
!></td><td>--></td><td>2.PI
!>    </td></tr>
!>          <tr><td>SS
!></td><td>--></td><td>EXPOSANT DE LA FONCTION DE REPARTITION ANG.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        FUNCTION DELFRA
     &( SS    , DEUPI )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DELFRA         |<--| COEFFICIENT DE NORMALISATION DE LA FRA
C| DEUPI          |-->| 2.PI
C| SS             |-->| EXPOSANT DE LA FONCTION DE REPARTITION ANG.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      DOUBLE PRECISION DELFRA, SS    , DEUPI
C
C.....EXTERNAL FUNCTIONS
C     """"""""""""""""""
      DOUBLE PRECISION GAMMLN
      EXTERNAL         GAMMLN
C
C
      DELFRA=SQRT(DEUPI/2.D0)
     &      *EXP(GAMMLN(SS+0.5D0,DEUPI)-GAMMLN(SS+1.D0,DEUPI))
C
      RETURN
      END
C
C#######################################################################
C