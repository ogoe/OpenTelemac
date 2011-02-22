C                        *********************
                          SUBROUTINE FLUXVISQ
C                        *********************
C     
     .(NLIEN , NLIENMAX , NPARF, NPMAX, ILIEN, 
     . KFLUID, KTURB    , KVISQ, DIFF , ETA2 , 
     . FVISQ , GKERX    , GKERZ, NU0  , NUT  ,
     . RAB   , RHO      , VXAB , VZAB , XAB  ,
     . ZAB   , NFLUIDMAX                     )
C
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! DIFF      ! -->  ! DIFFUSION TERM INTERMEDIATE                    !
C ! ETA2      ! -->  ! CORRECTION TERM FOR DIFFUSION                  !
C ! FVISQ     ! <--  ! MOMENTUM VISCOUS FLUX                          !
C ! GKERX,                                                            !
C ! GKERZ     ! -->  ! KERNEL DERIVATIVE COMPONENTS                   !
C ! ILIEN     ! -->  ! PARTICLE LINK LIST                             !
C ! KFLUID    ! -->  ! FLUID TYPE                                     !
C ! KTURB     ! -->  ! CHOICE INDEX FOR THE TURBULENCE MODEL          !
C ! KVISQ     ! -->  ! CHOICE INDEX FOR THE VISCOUS TERM MODEL        !
C ! NLIEN     ! -->  ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
C ! NLIENMAX  ! -->  ! MAXIMUM NUMBER OF LINKS RELATIVE TO A PARTICLE !
C ! NFLUIDMAX ! -->  ! MAXIMUM NUMBER OF FLUIDS                       !
C ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
C ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
C ! NU0       ! -->  ! MOLECULAR VISCOSITIES                          !
C ! NUT       ! -->  ! EDDY VISCOSITY                                 !
C ! RAB       ! -->  ! INTERPARTICLE DISTANCE                         !
C ! RHO       ! -->  ! DENSITY                                        !
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
C FONCTION : calcule les flux visqueux
C FUNCTION : computes viscous flux 
C
C PROGRAMMES APPELANT : IMPULSION
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
      INTEGER NPMAX, NPARF, NLIENMAX, KTURB
      INTEGER I    , L    , J       , NFLUIDMAX
      INTEGER M    , N
C     
      DOUBLE PRECISION ETA2
C      
      INTEGER NLIEN (NPMAX)
      INTEGER KFLUID(NPMAX)
      INTEGER ILIEN(NPMAX,NLIENMAX)
C
      INTEGER KVISQ
C     
      DOUBLE PRECISION RHO  (NPMAX)
      DOUBLE PRECISION NUT  (NPMAX)
C
      DOUBLE PRECISION NU0 (NFLUIDMAX)
C
      DOUBLE PRECISION RAB  (NPMAX,NLIENMAX), FVISQ(NPMAX,NLIENMAX)
      DOUBLE PRECISION XAB  (NPMAX,NLIENMAX), ZAB  (NPMAX,NLIENMAX)
      DOUBLE PRECISION GKERX(NPMAX,NLIENMAX), GKERZ(NPMAX,NLIENMAX)
      DOUBLE PRECISION VXAB (NPMAX,NLIENMAX), VZAB (NPMAX,NLIENMAX)
      DOUBLE PRECISION DIFF (NPMAX,NLIENMAX)
C
C Calcul des flux visqueux
C=========================
C Viscous flux computation
C=========================
C
      IF (KVISQ.EQ.1) THEN
C
C Modele de Monaghan
C-------------------
C Monaghan's model
C-----------------
C
        DO 103 I=1,NPARF
          M=KFLUID(I)
            DO 104 L=1,NLIEN(I)
              J=ILIEN(I,L)
                FVISQ(I,L)=8.D0*(NUT(I)+NUT(J)+2.D0*NU0(M))
     .                    *(VXAB(I,L)*XAB(I,L)
     .                     +VZAB(I,L)*ZAB(I,L))
     .                    /(RAB(I,L)**2+ETA2)
     .                    /(RHO(I)+RHO(J))
 104        CONTINUE
 103    CONTINUE
C       
      ELSE
C
C Modele de Morris
C-----------------
C Morris' model
C--------------
C
        DO 203 I=1,NPARF
          M=KFLUID(I)
            DO 204 L=1,NLIEN(I)
              J=ILIEN(I,L)
              N=KFLUID(J)
                IF (KTURB.LE.1) THEN
                  DIFF (I,L)=(GKERX(I,L)*XAB(I,L)
     .                       +GKERZ(I,L)*ZAB(I,L))
     .                      /(RAB(I,L)**2+ETA2)
                ENDIF
                FVISQ(I,L)=(RHO(I)*(NUT(I)+NU0(M))
     . 	                  + RHO(J)*(NUT(J)+NU0(N)))
     .                    *DIFF(I,L)
     .                    /(RHO(I)*RHO(J))
 204        CONTINUE
 203    CONTINUE
C
      ENDIF
C
      RETURN
      END

