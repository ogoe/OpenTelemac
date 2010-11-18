C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE MAXIMUM ORBITAL VELOCITY NEAR THE BOTTOM
!>               (AVERAGE VELOCITY ON THE SPECTRUM).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BETA, DEPTH, DFREQ, F, GRAVIT, NF, NPLAN, NPOIN2, UWBM, XK
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> COEF, DEUKD, DEUPI, DTETAR, IP, JF, JP
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DUMP2D()

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
!> </td><td> 05/07/96
!> </td><td> M. BENOIT
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>BETA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DEPTH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DFREQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRAVIT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UWBM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XK
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VITFON
     &( UWBM  , F     , XK    , DEPTH , DFREQ , NF    , NPOIN2, NPLAN ,
     &      GRAVIT, BETA  )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BETA           |---| 
C| DEPTH          |---| 
C| DFREQ          |---| 
C| F             |---| 
C| GRAVIT         |---| 
C| NF             |---| 
C| NPLAN          |---| 
C| NPOIN2         |---| 
C| UWBM           |---| 
C| XK             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      INTEGER  NF    , NPLAN , NPOIN2
      DOUBLE PRECISION GRAVIT
      DOUBLE PRECISION UWBM(NPOIN2), DEPTH(NPOIN2), BETA(NPOIN2)
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF), XK(NPOIN2,NF), DFREQ(NF)
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER  IP    , JP    , JF
      DOUBLE PRECISION DEUPI , DTETAR, DEUKD , COEF
C
C
      DEUPI=2.D0*3.14159265D0
      DTETAR=DEUPI/FLOAT(NPLAN)
C
      DO 30 IP = 1,NPOIN2
        UWBM(IP) = 0.D0
   30 CONTINUE
C
C.....SUMS UP THE DISCRETISED PART OF THE SPECTRUM
C     """"""""""""""""""""""""""""""""""""""""""""""""
      DO 20 JF = 1,NF
        COEF=2.D0*GRAVIT*DFREQ(JF)*DTETAR
        DO 25 IP = 1,NPOIN2
          DEUKD = MIN(2.D0*DEPTH(IP)*XK(IP,JF),7.D2)
          BETA(IP) = COEF*XK(IP,JF)/SINH(DEUKD)
   25   CONTINUE
        DO 10 JP = 1,NPLAN
          DO 5 IP=1,NPOIN2
            UWBM(IP) = UWBM(IP) + F(IP,JP,JF)*BETA(IP)
    5     CONTINUE
   10   CONTINUE
   20 CONTINUE
C
      DO 35 IP=1,NPOIN2
        UWBM(IP) = DSQRT(UWBM(IP))
   35 CONTINUE
C
      RETURN
      END
C
C#######################################################################
C