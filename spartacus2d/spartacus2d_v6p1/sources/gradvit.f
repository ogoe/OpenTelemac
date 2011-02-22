C                        ********************
                          SUBROUTINE GRADVIT
C                        ********************
C
     .(NLIEN, NLIENMAX, NPARB, NPARF, NPART, NPMAX, 
     . ILIEN, KPAR    , GKERX, GKERZ, GVXX , GVXZ , 
     . GVZX , GVZZ    , MASS , RHO  , VXAB , VZAB )
C
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! GKERX,                                                            !
C ! GKERZ     ! -->  ! KERNEL DERIVATIVE COMPONENTS                   !
C ! GVXX, GVXZ                                                        !
C ! GVZX, GVZZ! <--  ! COMPONENTS OF THE VELOCITY GRADIENT TENSOR     !
C ! ILIEN     ! -->  ! PARTICLE LINK LIST                             !
C ! KPAR      ! -->  ! PARTICLE  TYPE                                 !
C ! MASS      ! -->  ! PARTICLE MASS                                  !
C ! NLIEN     ! -->  ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
C ! NLIENMAX  ! -->  ! MAXIMUM NUMBER OF LINKS RELATIVE TO A PARTICLE !
C ! NPARB     ! -->  ! NUMBER OF EDGE PARTICLES                       !
C ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
C ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
C ! RHO       ! -->  ! DENSITY                                        !
C ! VXAB, VZAB! MS-1 ! VELOCITY DIFFERENCE BETWEEN PARTICLES          !
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
C FONCTION : calcule le gradient de la vitesse
C FUNCTION : computes velocity gradients
C
C PROGRAMMES APPELANT : SPARTACUS2D
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
      INTEGER NPMAX, NPARF, NLIENMAX, NPARB
      INTEGER I    , J    , L       , NPART
C     
      DOUBLE PRECISION TEMPO
C
      INTEGER NLIEN(NPMAX)
      INTEGER KPAR (NPMAX)
      INTEGER ILIEN(NPMAX,NLIENMAX)
C     
      DOUBLE PRECISION RHO (NPMAX), MASS(NPMAX)
      DOUBLE PRECISION GVXX(NPMAX), GVXZ(NPMAX)
      DOUBLE PRECISION GVZX(NPMAX), GVZZ(NPMAX)
C
      DOUBLE PRECISION GKERX(NPMAX,NLIENMAX), GKERZ(NPMAX,NLIENMAX)
      DOUBLE PRECISION VXAB (NPMAX,NLIENMAX), VZAB (NPMAX,NLIENMAX)
C
C
C Calcul du gradient de vitesse
C==============================
C Velocity gradient computation
C==============================
C
        DO 310 I=1,NPART
          GVXX(I)=0.D0
          GVXZ(I)=0.D0
          GVZX(I)=0.D0
          GVZZ(I)=0.D0
 310    CONTINUE
C
        DO 492 I=1,NPARF+NPARB
          DO 491 L=1,NLIEN(I)
            J=ILIEN(I,L)
C
            IF(KPAR(J).LE.2) THEN
C
              TEMPO   = VXAB(I,L)
              GVXX(I) = GVXX(I)+MASS(J)*TEMPO*GKERX(I,L)
              GVXX(J) = GVXX(J)+MASS(I)*TEMPO*GKERX(I,L)
              GVXZ(I) = GVXZ(I)+MASS(J)*TEMPO*GKERZ(I,L)
              GVXZ(J) = GVXZ(J)+MASS(I)*TEMPO*GKERZ(I,L)
C
              TEMPO   = VZAB(I,L)
              GVZX(I) = GVZX(I)+MASS(J)*TEMPO*GKERX(I,L)
              GVZX(J) = GVZX(J)+MASS(I)*TEMPO*GKERX(I,L)
              GVZZ(I) = GVZZ(I)+MASS(J)*TEMPO*GKERZ(I,L)
              GVZZ(J) = GVZZ(J)+MASS(I)*TEMPO*GKERZ(I,L)
C
            ENDIF
C
 491      CONTINUE
 492    CONTINUE
C
        DO 503 I=1,NPARF+NPARB
          GVXX(I) = -GVXX(I)/RHO(I)
          GVXZ(I) = -GVXZ(I)/RHO(I)
          GVZZ(I) = -GVZZ(I)/RHO(I)
          GVZX(I) = -GVZX(I)/RHO(I)	
 503    CONTINUE
C
      RETURN
      END
