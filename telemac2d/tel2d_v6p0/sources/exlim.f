C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief      EXTRAPOLATES THE GRADIENT AND USES OF A SLOPE LIMITER.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BETA, GRI, GRIJ, ILIM
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUX1, E2, GRI1, GRI2, GRIJ2
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Unknown(s)
!>    </th><td> DSIGN
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>FLUCIN(), GRADZ(), MAJTRAC()

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
!> </td><td>
!> </td><td> INRIA
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>BETA
!></td><td>--></td><td>COEFFICIENT EXTRAPOLATION POUR ORDRE 2
!>    </td></tr>
!>          <tr><td>GRI,GRIJ
!></td><td>--></td><td>GRADIENTS
!>    </td></tr>
!>          <tr><td>ILIM
!></td><td>--></td><td>OPTION POUR LIMITEUR
!>                  1 : MINMOD
!>                  2 : VAN ALBADA
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        DOUBLE PRECISION FUNCTION EXLIM
     &(ILIM,BETA,GRI,GRIJ)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BETA           |-->| COEFFICIENT EXTRAPOLATION POUR ORDRE 2
C| GRI,GRIJ       |-->| GRADIENTS
C| ILIM           |-->| OPTION POUR LIMITEUR
C|                |   | 1 : MINMOD
C|                |   | 2 : VAN ALBADA
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: ILIM
      DOUBLE PRECISION, INTENT(IN) :: GRI,GRIJ,BETA
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION GRI1,GRI2,GRIJ2,AUX1,E2
C
C-----------------------------------------------------------------------
C
C    EXTRAPOLATES THE GRADIENT AND USES SLOPE LIMITER
C
      GRI1 = (1.D0+BETA)*GRI - BETA*GRIJ
C
      IF(ILIM.EQ.1) THEN
C
C    MINMOD
C
       EXLIM=0.5D0*(DSIGN(1.D0,GRI1)+DSIGN(1.D0,GRIJ))
     &   *MIN(ABS(GRI1),ABS(GRIJ))
C
C
      ELSEIF (ILIM.EQ.2) THEN
C
C    VAN ALBADA
C
      E2 = 1.D-12
C
         AUX1 = 0.5D0*(1.D0+DSIGN(1.D0,GRI1*GRIJ))
         GRI2  = GRI1*GRI1  + E2
         GRIJ2 = GRIJ*GRIJ  + E2
C
         EXLIM  = AUX1*(GRI2*GRIJ+GRIJ2*GRI)/(GRI2+GRIJ2)
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