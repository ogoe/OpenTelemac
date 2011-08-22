!                        ***********************
                          SUBROUTINE FROTTEMENT
!                        ***********************
!
     &  (NBMAX , NPARB    , NPARF , NPARTQ   , NPARTQMAX, NPMAX,
     &   NQUADX, NQUADXMAX, NQUADZ, NQUADZMAX, IPARTQ   , CR   ,
     &   KPER  , KUSTAR   , CTHETA, DELTA    , DESTDR   , DR   ,
     &   GVXX  , GVXZ     , GVZX  , GVZZ     , H        , KAPPA,
     &   MASS  , PI       , RHO   , RUG      , STHETA   , SUPP ,
     &   USTAR , VX       , VZ    , X        , XMIN     , XMAX ,
     &   Z     , ZMIN                                          )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! CR        ! -->  ! COEFFICIENT RELATIVE TO A ROUGH LOG LAW        !
! ! CTHETA,                                                           !
! ! STHETA    ! -->  ! COMPONENTS OF WALL NORMAL VECTORS              !
! ! DELTA     ! -->  ! EDGE PARTICLE DISTANCE TO REAL WALLS           !
! ! DESTDR    ! -->  ! RATIO DEST/DR                                  !
! ! DR        ! -->  ! INITIAL INTERPARTICLE SPACING                  !
! ! GVXX, GVXZ                                                        !
! ! GVZX, GVZZ! -->  ! COMPONENTS OF THE VELOCITY GRADIENT TENSOR     !
! ! H         ! -->  ! SMOOTHING LENGTH                               !
! ! IPARTQ    ! -->  ! INDEX OF PARTICLES LOCATED IN A SQUARE         !
! ! KPER      ! -->  ! LOGICAL INDEX FOR PERIODICITY                  !
! ! KUSTAR    ! -->  ! CHOICE INDEX FOR FRICTION VELOCITY COMPUTATION !
! ! MASS      ! -->  ! PARTICLE MASS                                  !
! ! NBMAX     ! -->  ! MAXIMUM NUMBER OF EDGE PARTICLES               !
! ! NPARB     ! -->  ! NUMBER OF EDGE PARTICLES                       !
! ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
! ! NPART     ! -->  ! TOTAL PARTICLE NUMBER                          !
! ! NPARTQ    ! -->  ! NUMBER OF PARTICLES IN A SQUARE                !
! ! NPARTQMAX ! -->  ! MAXIMUM NUMBER OF PARTICLES IN A SQUARE        !
! ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
! ! NQUADX,                                                           !
! ! NQUADZ    ! -->  ! NUMBER OF SQUARES ALONG EACH DIRECTION         !
! ! NQUADXMAX,                                                        !
! ! NQUADZMAX ! -->  ! MAXIMUM NUMBER OF SQUARES ALONG EACH DIRECTION !
! ! PI        ! -->  ! ARCHIMEDE'S NUMBER                             !
! ! RHO       ! -->  ! DENSITY                                        !
! ! RUG       ! -->  ! WALL ROUGHNESS                                 !
! ! SUPP      ! -->  ! KERNEL SUPPORT                                 !
! ! USTAR     ! <--  ! FRICTION VELOCITY                              !
! ! VX, VZ    ! -->  ! VELOCITY COMPONENTS                            !
! ! X, Z      ! -->  ! PARTICLE POSITION                              !
! ! XMIN, XMAX! -->  ! MINIMUM AND MAXIMUM X OF THE DOMAIN            !
! ! ZMIN, ZMAX! -->  ! MINIMUM AND MAXIMUM Z OF THE DOMAIN            !
! !___________!______!________________________________________________!
!
! MODE : -->(NON MODIFIED DATA), <--(RESULT), <-->(MODIFIED DATA)
!
!----------------------------------------------------------------------
!
! SPARTACUS2D V5P9
! D. Violeau           & R. Issa
! +33(0)1-30-87-78-31 // +33(0)1-30-87-84-28
! LNHE - 2008
!
! FONCTION : estime la vitesse de frottement
! FUNCTION : estimates the friction velocity
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
      INTEGER NPMAX    , NPARF
      INTEGER NBMAX    , NPARB    , NPARTQMAX
      INTEGER NQUADXMAX, NQUADZMAX, KUSTAR
      INTEGER I        , J        , L
      INTEGER M        , N        , K
      INTEGER MMIN     , MMAX     , NQUADX
      INTEGER NMIN     , NMAX     , NQUADZ
      INTEGER MQ       , NQ
!
      DOUBLE PRECISION KAPPA, DELTA, GVN  , CR   , DESTDR
      DOUBLE PRECISION SIGMA, SUPP , H    , RUG  , XMIN
      DOUBLE PRECISION Q    , TEMPO, DR   , VEST , ZMIN
      DOUBLE PRECISION XEST , ZEST , PI   , XMAX , DEST
!
      INTEGER IPARTQ(NQUADXMAX,NQUADZMAX,NPARTQMAX)
      INTEGER NPARTQ(NQUADXMAX,NQUADZMAX)
!
      DOUBLE PRECISION GVXX  (NPMAX), GVXZ  (NPMAX), USTAR(NBMAX)
      DOUBLE PRECISION GVZX  (NPMAX), GVZZ  (NPMAX), RHO  (NPMAX)
      DOUBLE PRECISION CTHETA(NBMAX), STHETA(NBMAX), X    (NPMAX)
      DOUBLE PRECISION VX    (NBMAX), VZ    (NBMAX), Z    (NPMAX)
      DOUBLE PRECISION MASS  (NBMAX)
!
      LOGICAL KPER
!
      SIGMA=15.D0/(7.D0*PI*H**2)
!
      DEST=DESTDR*DR
!
! Estimation de la vitesse de frottement
!=======================================
! Friction velocity estimation
!=============================
!
      IF (KUSTAR.EQ.1) THEN
!
! Modele fonde sur le gradient de vitesse
!----------------------------------------
! Model based on velocity gradient estimation
!--------------------------------------------
!
        DO 407 I=1,NPARB
          J=NPARF+I
          GVN      = (GVXX(J)-GVZZ(J))*STHETA(I)*CTHETA(I)
     &               +GVXZ(J)*STHETA(I)**2
     &               -GVZX(J)*CTHETA(I)**2
          USTAR(I) =  GVN*KAPPA*DELTA
          IF (USTAR(I).LT.0.D0) USTAR(I)= MIN(USTAR(I),-1.D-16)
          IF (USTAR(I).GE.0.D0) USTAR(I)= MAX(USTAR(I), 1.D-16)
 407    CONTINUE
!
      ELSE
!
! Modele fonde sur la loi log
!----------------------------
! Model based on the log-law
!---------------------------
!
        DO 507 I=1,NPARB
          J=NPARF+I
!
! Estimation de la vitesse au voisinage de la paroi
!..................................................
! Estimation of the velocity near the wall
!.........................................
!
          XEST=X(J)+DEST*CTHETA(I)
          ZEST=Z(J)+DEST*STHETA(I)
!
          VEST = 0.D0
!
          MQ=INT((XEST-XMIN)/(SUPP*H))+1
          NQ=INT((ZEST-ZMIN)/(SUPP*H))+1
          MMIN=MQ-1
          MMAX=MQ+1
          NMIN=NQ-1
          NMAX=NQ+1
          IF (MMIN.EQ.0       ) MMIN=1
          IF (MMAX.EQ.NQUADX+1) MMAX=NQUADX
          IF (NMIN.EQ.0)        NMIN=1
          IF (NMAX.EQ.NQUADZ+1) NMAX=NQUADZ
!
          DO 122 M=MMIN,MMAX
            DO 161 N=NMIN,NMAX
              DO 110 L=1,NPARTQ(M,N)
                K = IPARTQ(M,N,L)
!
                Q=SQRT((XEST-X(K))**2+(ZEST-Z(K))**2)/H
                IF (Q.LE.2.) THEN
                  TEMPO=2. D0     /3. D0-9.D0*Q**2/8. D0
     &                 +19.D0*Q**3/24.D0-5.D0*Q**4/32.D0
                ELSE
                  TEMPO=0.D0
                ENDIF
                TEMPO=TEMPO*SIGMA*MASS(K)/RHO(K)
                VEST = VEST+TEMPO*(VX(K)*STHETA(I)-VZ(K)*CTHETA(I))
!
 110          CONTINUE
 161        CONTINUE
 122      CONTINUE
!
          IF (KPER) THEN
!
! Periodicite a gauche
!.....................
! "Left" periodicity
!...................
!
            IF (MQ.EQ.1) THEN
!
              DO 222 M=NQUADX-1,NQUADX
                DO 261 N=NMIN,NMAX
                  DO 210 L=1,NPARTQ(M,N)
                    K = IPARTQ(M,N,L)
!
                    Q=SQRT((XEST-X(K)+XMAX-XMIN)**2+(ZEST-Z(K))**2)/H
                    IF (Q.LE.2.) THEN
                      TEMPO=2. D0     /3. D0-9.D0*Q**2/8. D0
     &                     +19.D0*Q**3/24.D0-5.D0*Q**4/32.D0
                    ELSE
                      TEMPO=0.D0
                    ENDIF
                    TEMPO=TEMPO*SIGMA*MASS(K)/RHO(K)
                    VEST = VEST+TEMPO*(VX(K)*STHETA(I)-VZ(K)*CTHETA(I))
!
 210              CONTINUE
 261            CONTINUE
 222          CONTINUE
!
            ENDIF
!
! Periodicite a droite
!.....................
! "Right" periodicity
!....................
!
            IF (MQ.GE.NQUADX-1) THEN
!
              DO 322 M=1,2
                DO 361 N=NMIN,NMAX
                  DO 310 L=1,NPARTQ(M,N)
                    K = IPARTQ(M,N,L)
!
                    Q=SQRT((XEST-X(K)-XMAX+XMIN)**2+(ZEST-Z(K))**2)/H
                    IF (Q.LE.2.) THEN
                      TEMPO=2. D0     /3. D0-9.D0*Q**2/8. D0
     &                     +19.D0*Q**3/24.D0-5.D0*Q**4/32.D0
                    ELSE
                      TEMPO=0.D0
                    ENDIF
                    TEMPO=TEMPO*SIGMA*MASS(K)/RHO(K)
                    VEST = VEST+TEMPO*(VX(K)*STHETA(I)-VZ(K)*CTHETA(I))
!
 310              CONTINUE
 361            CONTINUE
 322          CONTINUE
!
            ENDIF
!
          ENDIF
!
! Estimation de la vitesse de frottement
!.......................................
! Friction velocity estimation
!.............................
!
          USTAR(I) = VEST/(LOG((DEST+DELTA)/RUG)/KAPPA+CR)
          IF (USTAR(I).LT.0.D0) USTAR(I)= MIN(USTAR(I),-1.D-16)
          IF (USTAR(I).GE.0.D0) USTAR(I)= MAX(USTAR(I), 1.D-16)
!
 507    CONTINUE
!
      ENDIF
!
      RETURN
      END