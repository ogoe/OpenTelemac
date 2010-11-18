C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES FRICTION COEFFICIENT FOR NON-SUBMERGED
!>                VEGETATION FROM PARAMETERS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CF, CP, DP, G, HA, SP, VA, VK
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ACOF, ALFA, ANB, ANL, ANLAV, ANLCOUNT, ANLMAX, BCOF, CCOF, CW, CWR, CWR1, CWR2, CWRAV, CWRCOUNT, CWRMAX, DCOF, DCWR, DCWR1, DCWR2, FR, HRATIO, I, IANL, ICWR, ITERR, J, KMAXITER, KPRECISION, LAMBDA, LCWR, RANL, RCWR, REALROOTS, TMP1, TMP2, VRATIO, X
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CUBEEQUATION(), DRAGCOEFF()
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
!> </td><td> WRITTEN FROM THE C++ PROGRAM: RISMO2D OF THE BAW
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/11/1992
!> </td><td> MICHAEL SCHROEDER, BAW
!> </td><td> THE ALGORITHM WAS DEVELOPED BY LINDNER (1982) AND PASCHE
!>          (1986). I HAVE MADE SOME CHANGES TO THE COMPUTATION OF
!>           DEPTH RATIO (1990) AND TO THE WAKE LENGTH EQUATION (1992):
!>           THE SLOPE OF ENERGY LINE IS NO LONGER REQUESTED (THIS
!>           ESSENTIAL FOR USE IN 2D METHODS)
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CF
!></td><td>--></td><td>FRICTION COEFFICIENT FOR BOTTOM ROUGHNESS    C
!>    </td></tr>
!>          <tr><td>CP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DP
!></td><td>--></td><td>DIAMETER OF ROUGHNESS ELEMENT                C
!>    </td></tr>
!>          <tr><td>G
!></td><td>--></td><td>GRAVITY ACCELERATION                         C
!>    </td></tr>
!>          <tr><td>HA
!></td><td>--></td><td>FLOW DEPTH                                   C
!>    </td></tr>
!>          <tr><td>SP
!></td><td>--></td><td>SPACING OF ROUGHHNESS ELEMENT                C
!>    </td></tr>
!>          <tr><td>VA
!></td><td>--></td><td>VELOCITY                                     C
!>    </td></tr>
!>          <tr><td>VK
!></td><td>--></td><td>KINEMTIC VISCOSITY                           C
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FRICTION_LINDNER
     & (VA,HA,CF,VK,G,DP,SP,CP)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CF             |-->| FRICTION COEFFICIENT FOR BOTTOM ROUGHNESS    C
C| CP             |---| 
C| DP             |-->| DIAMETER OF ROUGHNESS ELEMENT                C
C| G             |-->| GRAVITY ACCELERATION                         C
C| HA             |-->| FLOW DEPTH                                   C
C| SP             |-->| SPACING OF ROUGHHNESS ELEMENT                C
C| VA             |-->| VELOCITY                                     C
C| VK             |-->| KINEMTIC VISCOSITY                           C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, INTENT(IN)  :: VA,HA,CF,VK,G,DP,SP
      DOUBLE PRECISION, INTENT(OUT) :: CP
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER,          PARAMETER :: KMAXITER = 200
      DOUBLE PRECISION, PARAMETER :: KPRECISION = 1.0D-3
      INTEGER                     :: CWRAV
      INTEGER                     :: CWRMAX
      INTEGER                     :: CWRCOUNT
      INTEGER                     :: ANLAV
      INTEGER                     :: ANLMAX
      INTEGER                     :: ANLCOUNT
      INTEGER                     :: ITERR
!
      INTEGER :: I, J
      INTEGER :: ICWR               ! ITERATION COUNTER: CWR
      INTEGER :: IANL               ! ITERATION COUNTER: ANL
      INTEGER :: REALROOTS
      LOGICAL :: LCWR
!
      DOUBLE PRECISION :: CW, CWR, RCWR, DCWR, ANL, ANB, RANL
      DOUBLE PRECISION :: CWR1, CWR2, DCWR1, DCWR2
      DOUBLE PRECISION :: LAMBDA, FR
      DOUBLE PRECISION :: X(3), VRATIO, HRATIO
      DOUBLE PRECISION :: ALFA, ACOF, BCOF, CCOF, DCOF
!
      DOUBLE PRECISION :: TMP1, TMP2
!
!=======================================================================!
!=======================================================================!
C                               PROGRAMME                               !
!=======================================================================!
!=======================================================================!
!
      LCWR     = .TRUE.
      CWRAV    = 0
      CWRMAX   = 0
      CWRCOUNT = 0
      ANLAV    = 0
      ANLMAX   = 0
      ANLCOUNT = 0
      ITERR    = 0

      IF ((DP < 1.0E-3)     .OR.(SP < 1.0E-2).OR.
     &    (ABS(VA) < 1.0E-3).OR.(HA < 1.0E-3)     ) THEN

         CP = 0.D0

      ELSE

         ! INITIALIZATION
         ! --------------
         CWR   = 1.0      ! DRAG COEFFICIENT
         ANL   = SP/2.0   ! WAKE LENGTH OF A CYLINDER
         CWR1  = 1.0
         CWR2  = 1.0
         DCWR1 = 0.0
         DCWR2 = 0.0

         ! START OF ITERATION FOR CWR
         ! --------------------------
         DO ICWR = 1, KMAXITER

            ! SUPERPOSED FRICTION COEFFICIENT
            ! -------------------------------
            LAMBDA = 8.0D0*CF  +  4.0D0*CWR*HA*DP/SP/SP

            ! DRAG COEFFICIENT CW FOR ONE CYLINDER
            ! ------------------------------------
            CALL DRAGCOEFF(VA, DP, VK, CW)

            ! WAKE LENGTH OF A CYLINDER (ITERATIVE COMPUTATION)
            ! -------------------------------------------------
            DO J=1, KMAXITER

               TMP1 = 1.0D0  +  ANL*LAMBDA/4.0D0/HA
               TMP2 = 30.0D0/ABS(TMP1)**(1.5)
               RANL = CW*DP*ABS(TMP2)**(1.429)

               ! TEST FOR CONVERGENCE
               ! --------------------
               IF (ABS((RANL-ANL)/RANL) < KPRECISION) THEN
                  ANL  = RANL
                  IANL = -1*J
                  EXIT
               ENDIF

               ANL = 0.5 * (RANL + ANL)
            ENDDO

            ! STATISTICS OF CWR ITERATION
            ! ---------------------------
            IF ( IANL > 0 ) THEN
               ANL = SP/2.0D0
            ELSE
               IANL = ABS(IANL)
               ANLCOUNT = ANLCOUNT + 1
               ANLAV = IANL + ANLAV
               IF (IANL > ANLMAX) ANLMAX = IANL
            ENDIF

            ! WAKE WIDTH
            ! ----------
            ANB = 0.24 * ABS(ANL)**(0.59) * ABS(CW*DP)**(0.41)

            ! RATIO OF VELOCITY IN FRONT OF AND BEHIND CYLINDER
            ! -------------------------------------------------
            VRATIO = 1.151 * ABS(ANL/SP)**(-0.483)
     &             +   0.5 * ABS(ANB/SP)**(1.1)

            ! RATIO OF FLOW DEPTH
            ! -------------------
            FR = VA / SQRT( G * HA ) ! FROUDE NUMBER

            ALFA = DP / SP
            ACOF =  FR * FR * (1.0D0 - ALFA * CWR/2.0D0)
            BCOF = -FR * FR - (1.0D0 - ALFA) / 2.0D0
            CCOF =  0.0D0
            DCOF = (1.0D0 - ALFA) / 2.0D0
            HRATIO = 1.0D0

            IF (ABS(ACOF) < 1.0E-10) THEN
               HRATIO = SQRT( -DCOF / BCOF)

            ELSE
               CALL CUBEEQUATION(ACOF, BCOF, CCOF, DCOF, REALROOTS, X)

               DO I = 1, REALROOTS
                  IF (X(I) > 0.0  .AND.  X(I) < 1.0)  THEN
                     HRATIO = X(I)
                     EXIT
                  ENDIF
               ENDDO
            ENDIF

            ! REVISE DRAG COEFFICIENT CWR
            ! ---------------------------
            RCWR = 1.3124D0*CW*VRATIO + 2.0D0*(1.0D0-HRATIO)/FR/FR

            ! TEST FOR CONVERGENCE
            ! --------------------
            IF ( ABS((RCWR-CWR)/RCWR) < KPRECISION ) THEN
               LCWR = .FALSE.
C               ICWR = -1/ICWR
               EXIT
            ENDIF

            ! USE PEGASUS ALGORITHM FOR CWR ITERATION
            ! ---------------------------------------
            DCWR = RCWR - CWR

            IF ((ICWR >= 3) .AND. (DCWR1*DCWR2 < 0.0D0)) THEN

               IF (DCWR2*DCWR < 0.0D0) THEN
                  DCWR1 = DCWR2/(DCWR2+DCWR)*DCWR1

               ELSE
                  CWR1  = CWR2
                  DCWR1 = DCWR2
               ENDIF
               CWR2  = CWR
               DCWR2 = DCWR
               CWR   = CWR2 - DCWR2*(CWR2-CWR1)/(DCWR2-DCWR1)

            ELSE
               CWR1 = CWR2
               DCWR1 = DCWR2
               CWR2 = CWR
               DCWR2 = DCWR

               IF ((ICWR >= 2) .AND. (DCWR1*DCWR2 < 0.0 )) THEN
                  CWR = CWR2 - DCWR2*(CWR2-CWR1)/(DCWR2-DCWR1)
               ELSE
                  CWR = RCWR
               ENDIF
            ENDIF

         ENDDO !ICWR = 1, KMAXITER


         IF (LCWR) THEN
            ITERR = ITERR + 1
            CP = -1.D0

         ELSE
            ! STATISTICS OF CWR ITERATION
            ! ---------------------------
            ICWR = -1/ICWR ! AS THE PROGRAM RISMO2D FROM THE BAW
            ICWR = -ICWR
            CWRCOUNT = CWRCOUNT + 1
            CWRAV = ICWR + CWRAV

            IF (ICWR > CWRMAX) CWRMAX = ICWR

            CP = LAMBDA/8.0D0 - CF
         ENDIF

      ENDIF
!
!=======================================================================!
!=======================================================================!
!
      RETURN
      END SUBROUTINE FRICTION_LINDNER
C
C#######################################################################
C