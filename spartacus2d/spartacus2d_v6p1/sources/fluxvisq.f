!                        *********************
                          SUBROUTINE FLUXVISQ
!                        *********************
!
     &(NLIEN , NLIENMAX , NPARF, NPMAX, ILIEN,
     & KFLUID, KTURB    , KVISQ, DIFF , ETA2 ,
     & FVISQ , GKERX    , GKERZ, NU0  , NUT  ,
     & RAB   , RHO      , VXAB , VZAB , XAB  ,
     & ZAB   , NFLUIDMAX                     )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! DIFF      ! -->  ! DIFFUSION TERM INTERMEDIATE                    !
! ! ETA2      ! -->  ! CORRECTION TERM FOR DIFFUSION                  !
! ! FVISQ     ! <--  ! MOMENTUM VISCOUS FLUX                          !
! ! GKERX,                                                            !
! ! GKERZ     ! -->  ! KERNEL DERIVATIVE COMPONENTS                   !
! ! ILIEN     ! -->  ! PARTICLE LINK LIST                             !
! ! KFLUID    ! -->  ! FLUID TYPE                                     !
! ! KTURB     ! -->  ! CHOICE INDEX FOR THE TURBULENCE MODEL          !
! ! KVISQ     ! -->  ! CHOICE INDEX FOR THE VISCOUS TERM MODEL        !
! ! NLIEN     ! -->  ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
! ! NLIENMAX  ! -->  ! MAXIMUM NUMBER OF LINKS RELATIVE TO A PARTICLE !
! ! NFLUIDMAX ! -->  ! MAXIMUM NUMBER OF FLUIDS                       !
! ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
! ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
! ! NU0       ! -->  ! MOLECULAR VISCOSITIES                          !
! ! NUT       ! -->  ! EDDY VISCOSITY                                 !
! ! RAB       ! -->  ! INTERPARTICLE DISTANCE                         !
! ! RHO       ! -->  ! DENSITY                                        !
! ! VXAB, VZAB! -->  ! VELOCITY DIFFERENCE BETWEEN PARTICLES          !
! ! XAB, ZAB  ! -->  ! COORDINATE DIFFERENCES BETWEEN PARTICLES       !
! !___________!______!________________________________________________!
!
! MODE : -->(NON MODIFIED DATA), <--(RESULT), <-->(MODIFIED DATA)
!----------------------------------------------------------------------
!
! SPARTACUS2D V5P9
! D. Violeau           & R. Issa
! +33(0)1-30-87-78-31 // +33(0)1-30-87-84-28
! LNHE - 2008
!
! FONCTION : calcule les flux visqueux
! FUNCTION : computes viscous flux
!
! PROGRAMMES APPELANT : IMPULSION
! CALLED BY
!
! PROGRAMMES APPELES  : -
! CALLED PROGRAMS
!
!----------------------------------------------------------------------
!
! Variables
!==========
!
      IMPLICIT NONE
!
      INTEGER NPMAX, NPARF, NLIENMAX, KTURB
      INTEGER I    , L    , J       , NFLUIDMAX
      INTEGER M    , N
!
      DOUBLE PRECISION ETA2
!
      INTEGER NLIEN (NPMAX)
      INTEGER KFLUID(NPMAX)
      INTEGER ILIEN(NPMAX,NLIENMAX)
!
      INTEGER KVISQ
!
      DOUBLE PRECISION RHO  (NPMAX)
      DOUBLE PRECISION NUT  (NPMAX)
!
      DOUBLE PRECISION NU0 (NFLUIDMAX)
!
      DOUBLE PRECISION RAB  (NPMAX,NLIENMAX), FVISQ(NPMAX,NLIENMAX)
      DOUBLE PRECISION XAB  (NPMAX,NLIENMAX), ZAB  (NPMAX,NLIENMAX)
      DOUBLE PRECISION GKERX(NPMAX,NLIENMAX), GKERZ(NPMAX,NLIENMAX)
      DOUBLE PRECISION VXAB (NPMAX,NLIENMAX), VZAB (NPMAX,NLIENMAX)
      DOUBLE PRECISION DIFF (NPMAX,NLIENMAX)
!
! Calcul des flux visqueux
!=========================
! Viscous flux computation
!=========================
!
      IF (KVISQ.EQ.1) THEN
!
! Modele de Monaghan
!-------------------
! Monaghan's model
!-----------------
!
        DO 103 I=1,NPARF
          M=KFLUID(I)
            DO 104 L=1,NLIEN(I)
              J=ILIEN(I,L)
                FVISQ(I,L)=8.D0*(NUT(I)+NUT(J)+2.D0*NU0(M))
     &                    *(VXAB(I,L)*XAB(I,L)
     &                     +VZAB(I,L)*ZAB(I,L))
     &                    /(RAB(I,L)**2+ETA2)
     &                    /(RHO(I)+RHO(J))
 104        CONTINUE
 103    CONTINUE
!
      ELSE
!
! Modele de Morris
!-----------------
! Morris' model
!--------------
!
        DO 203 I=1,NPARF
          M=KFLUID(I)
            DO 204 L=1,NLIEN(I)
              J=ILIEN(I,L)
              N=KFLUID(J)
                IF (KTURB.LE.1) THEN
                  DIFF (I,L)=(GKERX(I,L)*XAB(I,L)
     &                       +GKERZ(I,L)*ZAB(I,L))
     &                      /(RAB(I,L)**2+ETA2)
                ENDIF
                FVISQ(I,L)=(RHO(I)*(NUT(I)+NU0(M))
     & 	                  + RHO(J)*(NUT(J)+NU0(N)))
     &                    *DIFF(I,L)
     &                    /(RHO(I)*RHO(J))
 204        CONTINUE
 203    CONTINUE
!
      ENDIF
!
      RETURN
      END
!