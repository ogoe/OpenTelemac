C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE WAVE ORBITAL VELOCITY.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> GRAV, H, HW, NPOIN, TW, UW
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DPI2, I, PI, POL, X, Y
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SISYPHE()

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
!>      <td><center> 5.1                                       </center>
!> </td><td> 20/05/96
!> </td><td>
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>GRAVITE
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEUR D'EAU
!>    </td></tr>
!>          <tr><td>HW
!></td><td>--></td><td>HAUTEUR DE HOULE
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS
!>    </td></tr>
!>          <tr><td>TW
!></td><td>--></td><td>PERIODE DE LA HOULE
!>    </td></tr>
!>          <tr><td>UW
!></td><td><--</td><td>VITESSE ORBITALE DE LA HOULE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CALCUW
     & ( UW, H, HW, TW, GRAV ,NPOIN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| GRAV           |-->| GRAVITE
C| H             |-->| HAUTEUR D'EAU
C| HW             |-->| HAUTEUR DE HOULE
C| NPOIN          |-->| NOMBRE DE POINTS
C| TW             |-->| PERIODE DE LA HOULE
C| UW             |<--| VITESSE ORBITALE DE LA HOULE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: UW(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: TW(NPOIN),H(NPOIN), HW(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: GRAV
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION   PI,DPI2
      PARAMETER ( PI = 3.141592653589793D0 , DPI2 = (4.D0*PI*PI) )
      DOUBLE PRECISION   POL, Y ,X
      INTEGER I
      INTRINSIC SQRT, SINH
C
C  SOLVES Y=X*TH(X) WITH Y=(2*PI/TW)**2*H/G AND X=(2*PI/L)*H
C  USING A POLYNOMIAL FUNCTION (HUNT METHOD - 9TH ORDER)
C
      DO 10 I=1,NPOIN
       IF ( (TW(I) .GT. 0.D0).AND.(HW(I).GT.0.D0) ) THEN
         Y = DPI2 / GRAV * H(I) / (TW(I) * TW(I))
         POL = 1.D0 + Y * ( 0.66667D0 +
     &                Y * ( 0.35550D0 +
     &                Y * ( 0.16084D0 +
     &                Y * ( 0.06320D0 +
     &                Y * ( 0.02174D0 +
     &                Y * ( 0.00654D0 +
     &                Y * ( 0.00171D0 +
     &                Y * ( 0.00039D0 +
     &                Y * ( 0.00011D0 ) ))))))))
         X = SQRT( Y*Y + Y / POL )
C
         IF ( X .GT. 10.D0) THEN
            UW(I) = 0.D0
         ELSE
            UW(I) = PI / TW(I) * HW(I) / (SINH(X))
         ENDIF
       ELSE
         UW(I) = 0.D0
       ENDIF
 10   CONTINUE

      RETURN
      END SUBROUTINE CALCUW
C
C#######################################################################
C