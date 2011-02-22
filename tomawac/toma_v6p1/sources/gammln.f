C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE NATURAL LOGARITHM FOR THE GAMMA FUNCTION
!>                (EULER FUNCTION OF SECOND-KIND).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note   IF XX IS AN INTEGER NOTED N, GAMMA(N) = (N-1)!

!>  @reference "NUMERICAL RECIPES. THE ART OF SCIENTIFIC COMPUTING",
!>                       PRESS ET AL. (1989). (CF. PP 156-157)

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DEUPI, XX
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUX, COF, GAMMLN, J, SER, STP, TMP, X, XC
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DELFRA(), QBREK3()

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
!>          <tr><td>DEUPI
!></td><td>--></td><td>2.PI
!>    </td></tr>
!>          <tr><td>XX
!></td><td>--></td><td>VALEUR EN LAQUELLE LOG(GAMMA) EST CALCULE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        FUNCTION GAMMLN
     &( XX    , DEUPI )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DEUPI          |-->| 2.PI
C| XX             |-->| VALEUR EN LAQUELLE LOG(GAMMA) EST CALCULE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      DOUBLE PRECISION GAMMLN, XX    , DEUPI
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER  J
      DOUBLE PRECISION STP   , X     , XC    , TMP   , SER   , AUX
      DOUBLE PRECISION COF(6)
C
C
      COF(1)= 76.180091730D0
      COF(2)=-86.505320330D0
      COF(3)= 24.014098220D0
      COF(4)= -1.231739516D0
      COF(5)=  0.001208580D0
      COF(6)= -0.000005364D0
      STP   =  2.506628275D0
C
      IF (XX.LT.1.D0) THEN
        XC=2.D0-XX
      ELSE
        XC=XX
      ENDIF
      X=XC-1.D0
      TMP=X+5.5D0
      TMP=(X+0.5D0)*DLOG(TMP)-TMP
      SER=1.D0
      DO 11 J=1,6
        X=X+1.D0
        SER=SER+COF(J)/X
   11 CONTINUE
      GAMMLN=TMP+DLOG(STP*SER)
      IF (XX.LT.1D0) THEN
        AUX=0.5D0*DEUPI*(1.D0-XX)
        GAMMLN=DLOG(AUX/SIN(AUX))-GAMMLN
      ENDIF
C
      RETURN
      END
C
C#######################################################################
C