C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FRACTION OF BREAKING WAVES: QB.
!>                QB IS USED IN BATTJES AND JANSSEN (1978).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @reference BATTJES AND JANSSEN (1978) :
!>                     "ENERGY LOSS AND SET-UP DUE TO BREAKING
!>                      OF RANDOM WAVES". ICCE'78.

!>  @reference DINGEMANS (1983) :
!>                     "VERIFICATION OF NUMERICAL WAVE PROPAGATION
!>                      MODELS WITH FIELD MEASUREMENTS. CREDIZ
!>                      VERIFICATION HARINGVLIET".

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> B, IQBBJ
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> B2, CB, EPS, EXPO, F, Q0, QBBJ78, QMAX, QMIN
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>QBREK1()

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
!> </td><td> 14/02/96
!> </td><td> F.BECQ; M. BENOIT (EDF/DER/LNH)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>B
!></td><td>--></td><td>(HE/HM)
!>    </td></tr>
!>          <tr><td>IQBBJ
!></td><td>--></td><td>INDICE DE LA METHODE UTILISEE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        FUNCTION QBBJ78
     &( B     , IQBBJ )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| B             |-->| (HE/HM)
C| IQBBJ          |-->| INDICE DE LA METHODE UTILISEE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      DOUBLE PRECISION QBBJ78, B
      INTEGER  IQBBJ
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      DOUBLE PRECISION F     , CB    , EPS   , QMAX  , QMIN  , Q0
      DOUBLE PRECISION B2    , EXPO
C
C
      EPS = 1.D-7
C
      IF (B.GE.1.D0) THEN
        QBBJ78 = 1.D0
        RETURN
      ENDIF
C
      IF(IQBBJ.EQ.0) THEN
C       =========================
C       SOLVES BY DICHOTOMY
C       =========================
        QMIN  = 0.D0
        QMAX  = B
   10   CONTINUE
        QBBJ78 = (QMIN+QMAX)/2.D0
        F      = 1.D0 - QBBJ78 + B*B*DLOG(QBBJ78)
        IF (ABS(F).LT.EPS) RETURN
        IF (F.GT.0.D0) THEN
           QMAX = QBBJ78
        ELSE
           QMIN = QBBJ78
        ENDIF
        GOTO 10
C
      ELSEIF(IQBBJ.EQ.1) THEN
C       ======================================================
C       EXPLICIT FORMULATION 1 (INSPIRED FROM CREDIZ VERSION-1)
C       ======================================================
      CB = 0.5D0
        IF (B.GE.CB) THEN
          QBBJ78 = ((B-CB)/(1.D0-CB))**2
        ELSE
          QBBJ78 = 0.D0
        ENDIF
C
      ELSEIF(IQBBJ.EQ.2) THEN
C       ======================================================
C       EXPLICIT FORMULATION 2 (INSPIRED FROM CREDIZ VERSION-2)
C       ======================================================
        CB = 0.3D0
        IF (B.LT.CB) THEN
          QBBJ78 = 0.D0
        ELSEIF (B.LT.0.5D0) THEN
          B2     = B**2
          EXPO   = DEXP(-1.D0/B2)
          QBBJ78 = B2*EXPO/(B2-EXPO)
        ELSEIF (B.LT.0.9D0) THEN
          Q0     = (2.D0*B-1.D0)**2
          B2     = B**2
          EXPO   = DEXP((Q0-1.D0)/B2)
          QBBJ78 = Q0 - B2*(Q0-EXPO)/(B2-EXPO)
        ELSE
          QBBJ78 = (2.D0*B-1.D0)**2
        ENDIF
C
      ELSEIF(IQBBJ.EQ.3) THEN
C       ======================================================
C       EXPLICIT FORMULATION 3 (INSPIRED FROM CREDIZ VERSION-3)
C       ======================================================
        QBBJ78 = 2.4D0*B**7
C
      ENDIF
C
      RETURN
      END
C
C#######################################################################
C