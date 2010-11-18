C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BAILARD FORMULATION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ALPHAW, CF, DENS, FCW, FW, GRAV, HOULE, NPOIN, PI, QSC, QSCX, QSCY, QSS, QSSX, QSSY, THETAC, THETAW, TOB, TOBW, U2D, UC3X, UC3Y, UCMOY, US4X, US4Y, UW, V2D, XMVE, XWC
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C3, C4, EPSC, EPSS, I, NUM, PHI, U3X, U3Y
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BEDLOAD_BAILARD
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> BEDLOAD_DIRECTION(), BEDLOAD_INTERACT(), CPSTVC(), OS(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BEDLOAD_FORMULA()

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
!> </td><td> 21/12/2006
!> </td><td> JMH
!> </td><td> BEDLOAD_CALCBAIL DELETED; 'HOULE' ARGUMENT ADDED
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 01/10/2003
!> </td><td> C. VILLARET (LNHE)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ALPHAW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DENS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FCW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HOULE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSCX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSCY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSSX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSSY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>THETAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>THETAW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TOB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TOBW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UC3X
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UC3Y
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UCMOY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>US4X
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>US4Y
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XMVE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XWC
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
        SUBROUTINE BEDLOAD_BAILARD !
     &(U2D,V2D,UCMOY,TOB,TOBW,THETAW,UW,FW,CF,NPOIN,PI,
     & XMVE,GRAV,DENS,XWC,ALPHAW,QSCX,QSCY,QSSX,QSSY,
     & UC3X,UC3Y,US4X,US4Y,THETAC,FCW,QSC,QSS,HOULE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ALPHAW         |---| 
C| CF             |---| 
C| DENS           |---| 
C| FCW            |---| 
C| FW             |---| 
C| GRAV           |---| 
C| HOULE          |---| 
C| NPOIN          |---| 
C| PI             |---| 
C| QSC            |---| 
C| QSCX           |---| 
C| QSCY           |---| 
C| QSS            |---| 
C| QSSX           |---| 
C| QSSY           |---| 
C| THETAC         |---| 
C| THETAW         |---| 
C| TOB            |---| 
C| TOBW           |---| 
C| U2D            |---| 
C| UC3X           |---| 
C| UC3Y           |---| 
C| UCMOY          |---| 
C| US4X           |---| 
C| US4Y           |---| 
C| UW             |---| 
C| V2D            |---| 
C| XMVE           |---| 
C| XWC            |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE,EX_BEDLOAD_BAILARD => BEDLOAD_BAILARD
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
C 2/ GLOBAL VARIABLES
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: U2D,V2D,UCMOY, TOB
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOBW, THETAW, UW, FW, CF
      INTEGER,          INTENT(IN)    :: NPOIN
      LOGICAL,          INTENT(IN)    :: HOULE
      DOUBLE PRECISION, INTENT(IN)    :: PI, XMVE, GRAV, DENS, XWC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ALPHAW        ! WORK ARRAY T1
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSCX, QSCY    ! WORK ARRAY T2 AND T3
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSSX, QSSY    ! WORK ARRAY T4 AND T5
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: UC3X, UC3Y    ! WORK ARRAY T6 AND T7
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: US4X, US4Y    ! WORK ARRAY T8 AND T9
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: THETAC, FCW   ! WORK ARRAY T10 AND T11
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC, QSS
!
C 3/ LOCAL VARIABLES
!
      INTEGER                     :: I
      DOUBLE PRECISION            :: C3, C4, PHI
      DOUBLE PRECISION, PARAMETER :: EPSC = 0.21D0   ! BEDLOAD
      DOUBLE PRECISION, PARAMETER :: EPSS = 0.025D0  ! SUSPENSION
      DOUBLE PRECISION            :: U3X, U3Y, NUM
!
!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
C     CASE WITH WAVES
!
      IF(HOULE) THEN
!
C     ANGLE OF VELOCITY WITH OX (IN RADIANS)
!
      CALL BEDLOAD_DIRECTION(U2D,V2D,NPOIN,PI,THETAC)
!
C     ANGLE OF WAVES WITH OX (IN RADIANS)
!
      CALL OS('X=CY    ', X=ALPHAW, Y=THETAW, C=-PI/180.D0)
      CALL OS('X=X+C   ', X=ALPHAW, C=0.5D0*PI)
      CALL OS('X=Y-Z   ', X=ALPHAW, Y=ALPHAW, Z=THETAC)
!
C     PARAMETERS  ,
!
!
C     US4X AND US4Y ARE WORK ARRAYS, THEIR STRUCTURE IS GIVEN HERE
C     THE STRUCTURE OF THETAC (CATHERINE DON'T REMOVE THIS PLEASE)
      CALL CPSTVC(THETAC,US4X)
      CALL CPSTVC(THETAC,US4Y)
!
      DO I = 1, NPOIN

         ! ********************* !
         ! I - CURRENT REFERENCE SYSTEM !
         ! ********************* !

         U3X = UCMOY%R(I)**3
     &       + UCMOY%R(I)*UW%R(I)**2 * (1 + COS(2.D0*ALPHAW%R(I))/ 2.D0)
         U3Y = UCMOY%R(I)*UW%R(I)**2 * SIN(2.D0*ALPHAW%R(I)) / 2.D0

         ! ********************************************** !
         ! II - 3RD ORDER MOMENTUM (LINEAR WAVE THEORY)   !
         ! ********************************************** !

         UC3X%R(I) = U3X * COS(THETAC%R(I)) - U3Y * SIN(THETAC%R(I))
         UC3Y%R(I) = U3X * SIN(THETAC%R(I)) + U3Y * COS(THETAC%R(I))

         ! ************************************************************ !
         ! III -  4TH ORDER MOMENTUM (COLINEAR WAVES AND CURRENTS)      !
         ! ************************************************************ !

         NUM = ( 8.D0*UCMOY%R(I)**4 + 3.D0*UW%R(I)**4
     &           + 24.D0*(UCMOY%R(I)**2)*(UW%R(I)**2) )*0.125D0
         US4X%R(I) = NUM * COS(THETAC%R(I))
         US4Y%R(I) = NUM * SIN(THETAC%R(I))

       ENDDO

      ! *********************************************** !
      ! IV -  FRICTION COEFFICIENT WAVE + CURRENT       !
      ! *********************************************** !

      CALL BEDLOAD_INTERACT
     &     (UCMOY,TOBW,TOB,ALPHAW,FW,CF,UW,NPOIN,XMVE,FCW)

      ! ******************************** !
      ! V - TRANSPORT RATES              !
      ! ******************************** !

      PHI = PI   / 6.D0  ! FRICTION ANGLE
      C3  = EPSC / (GRAV*DENS*TAN(PHI))
      C4  = EPSS / (GRAV*DENS*XWC)
      CALL OS('X=CYZ   ', X=QSCX, Y=FCW,  Z=UC3X, C=C3)
      CALL OS('X=CYZ   ', X=QSCY, Y=FCW,  Z=UC3Y, C=C3)
      CALL OS('X=CYZ   ', X=QSSX, Y=FCW,  Z=US4X, C=C4)
      CALL OS('X=CYZ   ', X=QSSY, Y=FCW,  Z=US4Y, C=C4)
!
C     CASE WITHOUT WAVES
!
      ELSE
!
        CALL PLANTE(1)
        STOP 'BAILARD WITHOUT WAVES NOT PROGRAMMED'
!
      ENDIF
!
C     NORMS
!
      CALL OS('X=N(Y,Z)', X=QSC,  Y=QSCX, Z=QSCY)
      CALL OS('X=N(Y,Z)', X=QSS,  Y=QSSX, Z=QSSY)

!======================================================================!
!======================================================================!

      RETURN
      END
C
C#######################################################################
C