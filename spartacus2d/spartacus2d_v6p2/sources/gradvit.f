!                        ********************
                          SUBROUTINE GRADVIT
!                        ********************
!
     &(NLIEN, NLIENMAX, NPARB, NPARF, NPART, NPMAX,
     & ILIEN, KPAR    , GKERX, GKERZ, GVXX , GVXZ ,
     & GVZX , GVZZ    , MASS , RHO  , VXAB , VZAB )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! GKERX,                                                            !
! ! GKERZ     ! -->  ! KERNEL DERIVATIVE COMPONENTS                   !
! ! GVXX, GVXZ                                                        !
! ! GVZX, GVZZ! <--  ! COMPONENTS OF THE VELOCITY GRADIENT TENSOR     !
! ! ILIEN     ! -->  ! PARTICLE LINK LIST                             !
! ! KPAR      ! -->  ! PARTICLE  TYPE                                 !
! ! MASS      ! -->  ! PARTICLE MASS                                  !
! ! NLIEN     ! -->  ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
! ! NLIENMAX  ! -->  ! MAXIMUM NUMBER OF LINKS RELATIVE TO A PARTICLE !
! ! NPARB     ! -->  ! NUMBER OF EDGE PARTICLES                       !
! ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
! ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
! ! RHO       ! -->  ! DENSITY                                        !
! ! VXAB, VZAB! MS-1 ! VELOCITY DIFFERENCE BETWEEN PARTICLES          !
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
! FONCTION : calcule le gradient de la vitesse
! FUNCTION : computes velocity gradients
!
! PROGRAMMES APPELANT : SPARTACUS2D
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
      INTEGER NPMAX, NPARF, NLIENMAX, NPARB
      INTEGER I    , J    , L       , NPART
!
      DOUBLE PRECISION TEMPO
!
      INTEGER NLIEN(NPMAX)
      INTEGER KPAR (NPMAX)
      INTEGER ILIEN(NPMAX,NLIENMAX)
!
      DOUBLE PRECISION RHO (NPMAX), MASS(NPMAX)
      DOUBLE PRECISION GVXX(NPMAX), GVXZ(NPMAX)
      DOUBLE PRECISION GVZX(NPMAX), GVZZ(NPMAX)
!
      DOUBLE PRECISION GKERX(NPMAX,NLIENMAX), GKERZ(NPMAX,NLIENMAX)
      DOUBLE PRECISION VXAB (NPMAX,NLIENMAX), VZAB (NPMAX,NLIENMAX)
!
!
! Calcul du gradient de vitesse
!==============================
! Velocity gradient computation
!==============================
!
        DO 310 I=1,NPART
          GVXX(I)=0.D0
          GVXZ(I)=0.D0
          GVZX(I)=0.D0
          GVZZ(I)=0.D0
 310    CONTINUE
!
        DO 492 I=1,NPARF+NPARB
          DO 491 L=1,NLIEN(I)
            J=ILIEN(I,L)
!
            IF(KPAR(J).LE.2) THEN
!
              TEMPO   = VXAB(I,L)
              GVXX(I) = GVXX(I)+MASS(J)*TEMPO*GKERX(I,L)
              GVXX(J) = GVXX(J)+MASS(I)*TEMPO*GKERX(I,L)
              GVXZ(I) = GVXZ(I)+MASS(J)*TEMPO*GKERZ(I,L)
              GVXZ(J) = GVXZ(J)+MASS(I)*TEMPO*GKERZ(I,L)
!
              TEMPO   = VZAB(I,L)
              GVZX(I) = GVZX(I)+MASS(J)*TEMPO*GKERX(I,L)
              GVZX(J) = GVZX(J)+MASS(I)*TEMPO*GKERX(I,L)
              GVZZ(I) = GVZZ(I)+MASS(J)*TEMPO*GKERZ(I,L)
              GVZZ(J) = GVZZ(J)+MASS(I)*TEMPO*GKERZ(I,L)
!
            ENDIF
!
 491      CONTINUE
 492    CONTINUE
!
        DO 503 I=1,NPARF+NPARB
          GVXX(I) = -GVXX(I)/RHO(I)
          GVXZ(I) = -GVXZ(I)/RHO(I)
          GVZZ(I) = -GVZZ(I)/RHO(I)
          GVZX(I) = -GVZX(I)/RHO(I)
 503    CONTINUE
!
      RETURN
      END