!                        ********************
                          SUBROUTINE TAUXDEF
!                        ********************
!
     &(NLIEN, NLIENMAX, NPARB, NPARF, NPMAX, ILIEN,
     & KDEF , KPAR    , ETA2 , GKERX, GKERZ, GVXX ,
     & GVXZ , GVZX    , GVZZ , MASS , RAB  , RHO  ,
     & S    , VXAB    , VZAB , XAB  , ZAB         )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! ETA2      ! -->  ! CORRECTION TERM FOR DIFFUSION                  !
! ! GKERX,                                                            !
! ! GKERZ     ! -->  ! KERNEL DERIVATIVE COMPONENTS                   !
! ! GVXX, GVXZ                                                        !
! ! GVZX, GVZZ! -->  ! COMPONENTS OF THE VELOCITY GRADIENT TENSOR     !
! ! ILIEN     ! -->  ! PARTICLE LINK LIST                             !
! ! KDEF      ! -->  ! CHOICE INDEX FOR THE STRAIN MODEL              !
! ! KPAR      ! -->  ! PARTICLE  TYPE                                 !
! ! MASS      ! -->  ! PARTICLE MASS                                  !
! ! NLIEN     ! -->  ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
! ! NLIENMAX  ! -->  ! MAXIMUM NUMBER OF LINKS RELATIVE TO A PARTICLE !
! ! NPARB     ! -->  ! NUMBER OF EDGE PARTICLES                       !
! ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
! ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
! ! RAB       ! -->  ! INTERPARTICLE DISTANCE                         !
! ! RHO       ! -->  ! DENSITY                                        !
! ! S         ! <--  ! RATE OF STRAIN                                 !
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
! FONCTION : calcule le taux de deformation
! FUNCTION : computes the rate of strain
!
! PROGRAMMES APPELANT : VISCTURB
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
      INTEGER NPMAX, NPARF, NLIENMAX
      INTEGER NPARB, KDEF , I
      INTEGER L    , J
!
      DOUBLE PRECISION ETA2, TEMPO
!
      INTEGER NLIEN(NPMAX)
      INTEGER KPAR (NPMAX)
      INTEGER ILIEN(NPMAX,NLIENMAX)
!
      DOUBLE PRECISION RHO (NPMAX), MASS(NPMAX), S(NPMAX)
      DOUBLE PRECISION GVXX(NPMAX), GVXZ(NPMAX)
      DOUBLE PRECISION GVZX(NPMAX), GVZZ(NPMAX)
!
      DOUBLE PRECISION RAB  (NPMAX,NLIENMAX)
      DOUBLE PRECISION XAB  (NPMAX,NLIENMAX), ZAB  (NPMAX,NLIENMAX)
      DOUBLE PRECISION VXAB (NPMAX,NLIENMAX), VZAB (NPMAX,NLIENMAX)
      DOUBLE PRECISION GKERX(NPMAX,NLIENMAX), GKERZ(NPMAX,NLIENMAX)
!
! Calcul du taux de deformation
!==============================
! Rate of strain computation
!===========================
!
      IF (KDEF.EQ.1) THEN
!
! modele fonde sur le gradient de vitesse
!----------------------------------------
! Model based on velocity gradients
!----------------------------------
!
 503    CONTINUE
!
        DO 114 I=1,NPARF+NPARB
          S(I) = GVXX(I)**2+0.5D0*(GVXZ(I)+GVZX(I))**2+GVZZ(I)**2
          S(I) = SQRT(2.D0*S(I))
 114    CONTINUE
!
      ELSE
!
! Modele de Violeau
!------------------
! Violeau's model
!----------------
!
        DO 809 I=1,NPARF+NPARB
          S(I) = 0.D0
 809    CONTINUE
!
        DO 810 I=1,NPARF
          DO 811 L=1,NLIEN(I)
            J=ILIEN(I,L)
!
            IF (KPAR(J).LE.2) THEN
!
              TEMPO=(GKERX(I,L)*XAB(I,L)
     &              +GKERZ(I,L)*ZAB(I,L))
     &              *(VXAB(I,L)**2
     &               +VZAB(I,L)**2)
     &              /(RAB(I,L)**2+ETA2)
     &              *(RHO(I)+RHO(J))
     &              /(RHO(I)*RHO(J))
              S(I)=S(I)+MASS(J)*TEMPO
              S(J)=S(J)+MASS(I)*TEMPO
!
            ENDIF
!
 811      CONTINUE
 810    CONTINUE
!
        DO 812 I=1,NPARF+NPARB
          S(I) = SQRT(ABS(S(I))/2.D0)
 812    CONTINUE
!
      ENDIF
!
      RETURN
      END