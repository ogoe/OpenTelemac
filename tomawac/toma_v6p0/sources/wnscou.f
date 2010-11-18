C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE WAVE NUMBER.
!>                SOLVES THE DISPERSION EQUATION WITHOUT CURRENT.
!>  @code
!>     3 METHODS ARE USED DEPENDING ON THE VALUE OF K0*D
!>     (K0: WAVE NUMBER IN INFINITE DEPTH)
!>
!>                      3.2                  5.6
!>      -----------------!--------------------!-----------------> K0*D
!>                       !                    !
!>   EXPLICIT METHODE    ! ITERATIVE METHODE  !     K=K0
!>   (HUNT 9TH ORDER)    !                    ! (INFINITE DEPTH)
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CK2, DEPTH, FREQ
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A, AUX, DEUPI, EPS, I, OM, P, XG, XK0, XK0D, Y, YI
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>INIPHY(), QBREK1(), QBREK4(), QTRIA1()

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
!>      <td><center> 1.0                                       </center>
!> </td><td> 07/02/95
!> </td><td> M. BENOIT (EDF/DER/LNH)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CK2
!></td><td><--</td><td>NOMBRE D'ONDE
!>    </td></tr>
!>          <tr><td>DEPTH
!></td><td>--></td><td>PROFONDEUR
!>    </td></tr>
!>          <tr><td>FREQ
!></td><td>--></td><td>FREQUENCE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE WNSCOU
     &( CK2   , FREQ  , DEPTH )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CK2            |<--| NOMBRE D'ONDE
C| DEPTH          |-->| PROFONDEUR
C| FREQ           |-->| FREQUENCE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      DOUBLE PRECISION CK2   , FREQ  , DEPTH
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER  I
      DOUBLE PRECISION P(9)  , EPS   , XG    , DEUPI , XK0   , XK0D
      DOUBLE PRECISION AUX   , A     , Y     , YI    , OM
C
      DATA DEUPI/6.28318531D0/
      DATA EPS/0.0001D0/
      DATA XG/9.806D0/
      DATA P/0.66667D0,0.35550D0,0.16084D0,0.06320D0,0.02174D0,
     &       0.00654D0,0.00171D0,0.00039D0,0.00011D0/
C
C
C
C.....COMPUTES THE ANGULAR FREQUENCY (OM), K0 AND K0D
      OM=FREQ*DEUPI
      XK0=OM*OM/XG
      XK0D=XK0*DEPTH
C
C.....DETERMINES THE METHOD OF RESOLUTION DEPENDING ON THE VALUE OF XK0D
C     ================================================================
C
      IF (XK0D.LE.3.2) THEN
C.......EXPLICIT METHOD (HUNT 9TH ORDER)
        Y=XK0*DEPTH
        AUX=1.D0
        YI=1.D0
        DO 12 I=1,9
          YI=YI*Y
          AUX=AUX+P(I)*YI
   12   CONTINUE
        AUX=Y+1.D0/AUX
        CK2=OM/SQRT(XG*DEPTH/AUX)
C
      ELSEIF (XK0D.LE.5.6) THEN
C.......ITERATIVE METHOD (FROM HUNT 9TH ORDER)
        Y=XK0*DEPTH
        AUX=1.D0
        YI=1.D0
        DO 11 I=1,9
          YI=YI*Y
          AUX=AUX+P(I)*YI
   11   CONTINUE
        AUX=Y+1.D0/AUX
        CK2=OM/SQRT(XG*DEPTH/AUX)
    2   CONTINUE
        A=CK2
        CK2=XK0/TANH(A*DEPTH)
        IF (ABS(CK2-A)/CK2.LT.EPS) GOTO 3
        GOTO 2
    3   CONTINUE
C
      ELSE
C.......APPROXIMATION OF INFINITE DEPTH
        CK2=XK0
      ENDIF
C
      RETURN
      END
C
C#######################################################################
C