C                        ********************
                          SUBROUTINE TAUXDEF
C                        ********************
C     
     .(NLIEN, NLIENMAX, NPARB, NPARF, NPMAX, ILIEN,
     . KDEF , KPAR    , ETA2 , GKERX, GKERZ, GVXX ,
     . GVXZ , GVZX    , GVZZ , MASS , RAB  , RHO  ,
     . S    , VXAB    , VZAB , XAB  , ZAB         )
C
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! ETA2      ! -->  ! CORRECTION TERM FOR DIFFUSION                  !
C ! GKERX,                                                            !
C ! GKERZ     ! -->  ! KERNEL DERIVATIVE COMPONENTS                   !
C ! GVXX, GVXZ                                                        !
C ! GVZX, GVZZ! -->  ! COMPONENTS OF THE VELOCITY GRADIENT TENSOR     !
C ! ILIEN     ! -->  ! PARTICLE LINK LIST                             !
C ! KDEF      ! -->  ! CHOICE INDEX FOR THE STRAIN MODEL              !
C ! KPAR      ! -->  ! PARTICLE  TYPE                                 !
C ! MASS      ! -->  ! PARTICLE MASS                                  !
C ! NLIEN     ! -->  ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
C ! NLIENMAX  ! -->  ! MAXIMUM NUMBER OF LINKS RELATIVE TO A PARTICLE !
C ! NPARB     ! -->  ! NUMBER OF EDGE PARTICLES                       !
C ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
C ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
C ! RAB       ! -->  ! INTERPARTICLE DISTANCE                         !
C ! RHO       ! -->  ! DENSITY                                        !
C ! S         ! <--  ! RATE OF STRAIN                                 !
C ! VXAB, VZAB! -->  ! VELOCITY DIFFERENCE BETWEEN PARTICLES          !
C ! XAB, ZAB  ! -->  ! COORDINATE DIFFERENCES BETWEEN PARTICLES       !
C !___________!______!________________________________________________!
C
C MODE : -->(NON MODIFIED DATA), <--(RESULT), <-->(MODIFIED DATA)
C----------------------------------------------------------------------
C
C SPARTACUS2D V5P9
C D. Violeau           & R. Issa
C +33(0)1-30-87-78-31 // +33(0)1-30-87-84-28 
C LNHE - 2008
C     
C FONCTION : calcule le taux de deformation
C FUNCTION : computes the rate of strain
C
C PROGRAMMES APPELANT : VISCTURB
C CALLED BY
C
C PROGRAMMES APPELES  : -
C CALLED PROGRAMS
C     
C----------------------------------------------------------------------
C     
C Variables
C==========
C
      IMPLICIT NONE
C     
      INTEGER NPMAX, NPARF, NLIENMAX
      INTEGER NPARB, KDEF , I
      INTEGER L    , J
C     
      DOUBLE PRECISION ETA2, TEMPO
C      
      INTEGER NLIEN(NPMAX)
      INTEGER KPAR (NPMAX)
      INTEGER ILIEN(NPMAX,NLIENMAX)
C     
      DOUBLE PRECISION RHO (NPMAX), MASS(NPMAX), S(NPMAX)
      DOUBLE PRECISION GVXX(NPMAX), GVXZ(NPMAX)
      DOUBLE PRECISION GVZX(NPMAX), GVZZ(NPMAX)
C
      DOUBLE PRECISION RAB  (NPMAX,NLIENMAX)
      DOUBLE PRECISION XAB  (NPMAX,NLIENMAX), ZAB  (NPMAX,NLIENMAX)
      DOUBLE PRECISION VXAB (NPMAX,NLIENMAX), VZAB (NPMAX,NLIENMAX)
      DOUBLE PRECISION GKERX(NPMAX,NLIENMAX), GKERZ(NPMAX,NLIENMAX)
C
C Calcul du taux de deformation
C==============================
C Rate of strain computation
C===========================
C
      IF (KDEF.EQ.1) THEN
C
C modele fonde sur le gradient de vitesse
C----------------------------------------
C Model based on velocity gradients
C----------------------------------
C
 503    CONTINUE
C
        DO 114 I=1,NPARF+NPARB
          S(I) = GVXX(I)**2+0.5D0*(GVXZ(I)+GVZX(I))**2+GVZZ(I)**2
          S(I) = SQRT(2.D0*S(I))
 114    CONTINUE
C
      ELSE
C
C Modele de Violeau
C------------------
C Violeau's model
C----------------
C
        DO 809 I=1,NPARF+NPARB
          S(I) = 0.D0
 809    CONTINUE
C
        DO 810 I=1,NPARF
          DO 811 L=1,NLIEN(I)
            J=ILIEN(I,L)
C
            IF (KPAR(J).LE.2) THEN
C
              TEMPO=(GKERX(I,L)*XAB(I,L)
     .              +GKERZ(I,L)*ZAB(I,L))
     .              *(VXAB(I,L)**2
     .               +VZAB(I,L)**2)
     .              /(RAB(I,L)**2+ETA2)
     .              *(RHO(I)+RHO(J))
     .              /(RHO(I)*RHO(J))
              S(I)=S(I)+MASS(J)*TEMPO
              S(J)=S(J)+MASS(I)*TEMPO
C
            ENDIF
C
 811      CONTINUE
 810    CONTINUE
C
        DO 812 I=1,NPARF+NPARB
          S(I) = SQRT(ABS(S(I))/2.D0)
 812    CONTINUE
C
      ENDIF
C
      RETURN
      END
