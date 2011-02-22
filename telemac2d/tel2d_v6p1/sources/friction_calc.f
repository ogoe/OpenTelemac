C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SETS THE FRICTION COEFFICIENT.
!>  @code
!>     FRICTION LAWS PROGRAMMED :<br>
!>     KFROT = 0 :  NO FRICTION
!>     KFROT = 1 :  LAW OF HAALAND
!>     KFROT = 2 :  LAW OF CHEZY
!>     KFROT = 3 :  LAW OF STRICKLER
!>     KFROT = 4 :  LAW OF MANNING
!>     KFROT = 5 :  LAW OF NIKURADSE
!>     KFROT = 6 :  LOG LAW OF WALL
!>     KFROT = 7 :  LAW OF COLEBROOK-WHITE
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note         LAWS CODED UP : NO FRICTION; HAALAND; CHEZY;
!>                STRICKLER; MANNING; NIKURADSE; WALL; COLEBROOK-WHITE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CF, CHESTR, DW_MESH, GRAV, HC, KARMAN, KFROT, NDEF, N_END, N_START, VK, VRES
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUX, DW, DWPLUS, I, INLOG, ITER, OLDCF, OLDUST, RE, TERM1, TERM2, TIERS, UNORM, UST
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>FRICTION_UNIF(), FRICTION_ZONES()

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
!>      <td><center> 5.5                                       </center>
!> </td><td>
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 20/04/2004
!> </td><td> F. HUVELIN
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CHESTR
!></td><td>--></td><td>FRICTION PARAMETER
!>    </td></tr>
!>          <tr><td>DW_MESH
!></td><td>--></td><td>DISTANCE TO THE BOUNDARY
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>GRAVITY ACCELERATION
!>    </td></tr>
!>          <tr><td>HC
!></td><td>--></td><td>WATER DEPTH : MAX(H,HMIN)
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td>--></td><td>VON KARMAN'S CONSTANT
!>    </td></tr>
!>          <tr><td>KFROT
!></td><td>--></td><td>LAW USED FOR THE CALCULATION
!>    </td></tr>
!>          <tr><td>NDEF
!></td><td>--></td><td>DEFAULT'S MANNING
!>    </td></tr>
!>          <tr><td>N_START,N_END
!></td><td>--></td><td>STARTING AND ENDING POINT
!>    </td></tr>
!>          <tr><td>VK
!></td><td>--></td><td>KINEMATIC VISCOSITY
!>    </td></tr>
!>          <tr><td>VRES
!></td><td>--></td><td>RESULTANT VELOCITY
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FRICTION_CALC
     &(N_START, N_END, KFROT, NDEF, VK, GRAV,
     & KARMAN, CHESTR, DW_MESH, HC, VRES, CF)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CF             |---| 
C| CHESTR         |-->| FRICTION PARAMETER
C| DW_MESH        |-->| DISTANCE TO THE BOUNDARY
C| GRAV           |-->| GRAVITY ACCELERATION
C| HC             |-->| WATER DEPTH : MAX(H,HMIN)
C| KARMAN         |-->| VON KARMAN'S CONSTANT
C| KFROT          |-->| LAW USED FOR THE CALCULATION
C| NDEF           |-->| DEFAULT'S MANNING
C| N_START,N_END  |-->| STARTING AND ENDING POINT
C| VK             |-->| KINEMATIC VISCOSITY
C| VRES           |-->| RESULTANT VELOCITY
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER,          INTENT(IN)    :: N_START, N_END, KFROT
      DOUBLE PRECISION, INTENT(IN)    :: NDEF, VK, GRAV, KARMAN
      TYPE(BIEF_OBJ),   INTENT(IN)    :: CHESTR,DW_MESH,HC,VRES
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CF
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER                       :: I, ITER
      DOUBLE PRECISION, PARAMETER   :: TIERS = 1.D0/3.D0
      DOUBLE PRECISION, PARAMETER   :: SUR30 = 1.D0/30.D0
      DOUBLE PRECISION              :: UNORM, INLOG, AUX
      DOUBLE PRECISION              :: OLDUST, OLDCF
      DOUBLE PRECISION              :: RE, UST, DW, DWPLUS
      DOUBLE PRECISION              :: TERM1,TERM2
!
!=======================================================================!
!=======================================================================!
!                               PROGRAMME                               !
!=======================================================================!
!=======================================================================!
!
      SELECT CASE (KFROT)
!
! NO FRICTION
! -----------
!
      CASE(0)
!
         DO I = N_START, N_END
            CF%R(I) = 0.D0
         ENDDO
!
! LAW OF HAALAND
! --------------
!
      CASE(1)
!
         DO I = N_START, N_END
            UNORM = MAX(VRES%R(I),1.D-6)
!                         1.D-6: LAMINAR VISCOSITY OF WATER
            INLOG = (6.9D0*1.D-6/4.D0  /HC%R(I)/UNORM)**3
     &            + (CHESTR%R(I)/14.8D0/HC%R(I))**3.33
            INLOG = MIN(1.D0-1.D-6,INLOG)
            AUX   = -0.6D0*LOG(INLOG)/LOG(10.D0)
            CF%R(I) = 0.25D0 / AUX**2
         ENDDO
!
! LAW OF CHEZY
! ------------
!
      CASE(2)
!
         DO I = N_START, N_END
            CF%R(I) = 2.D0*GRAV/(CHESTR%R(I)**2)
         ENDDO
!
! LAW OF STRICKLER
! ----------------
!
      CASE(3)
!
         DO I = N_START, N_END
            CF%R(I) = 2.D0*GRAV/CHESTR%R(I)**2/HC%R(I)**TIERS
         ENDDO
!
! LAW OF MANNING
! --------------
!
      CASE(4)
!
         DO I = N_START, N_END
            CF%R(I) = 2.D0*GRAV*(CHESTR%R(I)**2)/HC%R(I)**TIERS
         ENDDO
!
! LAW OF NIKURADSE
! ----------------
!
      CASE(5)
! 
!        NOTE: 11.036 IS 30.D0/EXP(1.D0)
         DO I = N_START, N_END     
           AUX=MAX(1.001D0,HC%R(I)*11.036D0/CHESTR%R(I))  
           CF%R(I) = 2.D0 / (LOG(AUX)/KARMAN)**2                              
         ENDDO
!
! LOG LAW OF WALL FOR VISCOUS FRICTION
! ---------------
!
      CASE(6)
!
         DO I = N_START, N_END
!
            IF(VRES%R(I) < 1.0D-9) THEN
               CF%R(I) = 20.D0 ! RISMO2D = 10.D0 AND TELEMAC2D = 2*10.D0
            ELSE
!
               DW = 0.33D0*DW_MESH%R(I)
!
               IF (CHESTR%R(I) < 1.0D-9) THEN
!
! ITERATIVE COMPUTATION OF FRICTION VELOCITY UST
! ----------------------------------------------
!
                  UST    = 100.0*VK/DW
                  OLDUST = 0.D0
!
                  DO ITER = 1, 50
!
                     IF (ABS((UST-OLDUST)/UST)<=1.0D-6) EXIT
!
                     DWPLUS = DW*UST/VK
!
                     IF (DWPLUS < 11.D0) DWPLUS = 11.D0
!
                     OLDUST = UST
                     UST    = KARMAN*VRES%R(I) / LOG(9.D0*DWPLUS)
!
                  ENDDO
!
               ELSE
                  UST = KARMAN*VRES%R(I) / (LOG(DW/CHESTR%R(I))+8.5D0)
                  RE  = CHESTR%R(I)*UST  / VK
!
                  IF (RE < 70.D0) THEN
!
! ITERATIVE COMPUTATION OF FRICTION VELOCITY UST
! ----------------------------------------------
!
                     OLDUST = 0.D0
!
                     DO ITER = 1, 50
!
                        IF (ABS((UST-OLDUST)/UST)<=1.0D-6) EXIT
!
                        DWPLUS = DW*UST/VK
!
                        IF (DWPLUS < 11.D0) DWPLUS = 11.D0
!
                        RE     = CHESTR%R(I)*UST/VK
                        OLDUST = UST
!
                        IF (RE < 3.32D0) THEN
                           UST = KARMAN*VRES%R(I) / LOG(9.D0*DWPLUS)
                        ELSE
                           UST = KARMAN*VRES%R(I)
     &                         / (  LOG(DW/CHESTR%R(I))
     &                            + 3.32D0*LOG(RE)/RE
     &                            + KARMAN*(8.5D0-9.96D0/RE))
                        ENDIF
                     ENDDO
                  ENDIF
               ENDIF
!
               DWPLUS = DW*UST/VK
!
               IF (DWPLUS < 11.D0 ) THEN
                  UST = 11.0*VK / DW
                  UST = SQRT(VRES%R(I)*VK/DW)
               ENDIF
!
               CF%R(I) = 2.D0*(UST**2) / (VRES%R(I)**2)
            ENDIF
         ENDDO
!
! LAW OF COLEBROOK-WHITE
! ----------------------
!
      CASE(7)
!
         DO I = N_START, N_END
!
            RE = 4.D0*VRES%R(I)*HC%R(I)/VK
!
! THE ORIGINAL CONDITION FOR LAMINAR/TURBULENT FLOW
! COULD NOT BE HOLD (PROBLEMS DURING NR ITERATION):
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
            IF(RE.GT.500.D0) THEN
!
! TEST CONDITION: ROUGHNESS LESS THAN FLOW DEPTH
! ----------------------------------------------
!
               IF(CHESTR%R(I) < HC%R(I)) THEN
!                 NDEF : DEFAULT MANNING'S N
                  CF%R(I) = 2.D0*(NDEF**2)*GRAV/HC%R(I)**TIERS
               ELSE
                  TERM1   = 4.4D0 / RE
                  TERM2   = CHESTR%R(I) / 14.84D0 / HC%R(I)
                  CF%R(I) = 2.5D0 ! INITIALIZE CF=1/SQRT(CF) FOR ITERATION
                  OLDCF   = 0.D0
!
                  DO ITER = 1, 50
                     IF (ABS((CF%R(I)-OLDCF)/CF%R(I))<=1.0D-6) EXIT
                     OLDCF = CF%R(I)
                     CF%R(I) = -2.03D0*LOG10(OLDCF*TERM1 + TERM2)
                  ENDDO
!
                  IF (ITER.GE.50) THEN
                     CF%R(I) = -2.03D0*LOG10(TERM2)
                  ENDIF
!
                  CF%R(I) = 2.D0 / (CF%R(I)**2) / 8.D0
               ENDIF
!
            ELSEIF (RE.GT.100.D0 ) THEN
               CF%R(I) = 16.D0 / RE
            ELSE
               CF%R(I) = 0.16D0
            ENDIF
         ENDDO
!
! OTHER CASES
! -----------
!
      CASE DEFAULT
!
         IF (LNG.EQ.1) WRITE(LU,1) KFROT
1        FORMAT(I5,' : LOI DE FROTTEMENT INCONNUE')
         IF (LNG.EQ.2) WRITE(LU,2) KFROT
2        FORMAT(I5,' : UNKNOWN FRICTION LAW')
         CALL PLANTE(1)
         STOP
!
      END SELECT
!
!=======================================================================!
!=======================================================================!
!
      RETURN
      END
C
C#######################################################################
C
