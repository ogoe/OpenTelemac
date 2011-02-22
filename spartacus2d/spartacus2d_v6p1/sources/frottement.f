C                        ***********************
                          SUBROUTINE FROTTEMENT
C                        ***********************
C
     .  (NBMAX , NPARB    , NPARF , NPARTQ   , NPARTQMAX, NPMAX,
     .   NQUADX, NQUADXMAX, NQUADZ, NQUADZMAX, IPARTQ   , CR   ,
     .   KPER  , KUSTAR   , CTHETA, DELTA    , DESTDR   , DR   , 
     .   GVXX  , GVXZ     , GVZX  , GVZZ     , H        , KAPPA,
     .   MASS  , PI       , RHO   , RUG      , STHETA   , SUPP , 
     .   USTAR , VX       , VZ    , X        , XMIN     , XMAX ,
     .   Z     , ZMIN                                          )
C
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! CR        ! -->  ! COEFFICIENT RELATIVE TO A ROUGH LOG LAW        !
C ! CTHETA,                                                           !
C ! STHETA    ! -->  ! COMPONENTS OF WALL NORMAL VECTORS              !
C ! DELTA     ! -->  ! EDGE PARTICLE DISTANCE TO REAL WALLS           !
C ! DESTDR    ! -->  ! RATIO DEST/DR                                  !
C ! DR        ! -->  ! INITIAL INTERPARTICLE SPACING                  !
C ! GVXX, GVXZ                                                        !
C ! GVZX, GVZZ! -->  ! COMPONENTS OF THE VELOCITY GRADIENT TENSOR     !
C ! H         ! -->  ! SMOOTHING LENGTH                               !
C ! IPARTQ    ! -->  ! INDEX OF PARTICLES LOCATED IN A SQUARE         !
C ! KPER      ! -->  ! LOGICAL INDEX FOR PERIODICITY                  !
C ! KUSTAR    ! -->  ! CHOICE INDEX FOR FRICTION VELOCITY COMPUTATION !
C ! MASS      ! -->  ! PARTICLE MASS                                  !
C ! NBMAX     ! -->  ! MAXIMUM NUMBER OF EDGE PARTICLES               !
C ! NPARB     ! -->  ! NUMBER OF EDGE PARTICLES                       !
C ! NPARF     ! -->  ! NUMBER OF FLUID PARTICLES                      !
C ! NPART     ! -->  ! TOTAL PARTICLE NUMBER                          !
C ! NPARTQ    ! -->  ! NUMBER OF PARTICLES IN A SQUARE                !
C ! NPARTQMAX ! -->  ! MAXIMUM NUMBER OF PARTICLES IN A SQUARE        !
C ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
C ! NQUADX,                                                           !
C ! NQUADZ    ! -->  ! NUMBER OF SQUARES ALONG EACH DIRECTION         !
C ! NQUADXMAX,                                                        !
C ! NQUADZMAX ! -->  ! MAXIMUM NUMBER OF SQUARES ALONG EACH DIRECTION !
C ! PI        ! -->  ! ARCHIMEDE'S NUMBER                             !
C ! RHO       ! -->  ! DENSITY                                        !
C ! RUG       ! -->  ! WALL ROUGHNESS                                 !
C ! SUPP      ! -->  ! KERNEL SUPPORT                                 !
C ! USTAR     ! <--  ! FRICTION VELOCITY                              !
C ! VX, VZ    ! -->  ! VELOCITY COMPONENTS                            ! 
C ! X, Z      ! -->  ! PARTICLE POSITION                              !
C ! XMIN, XMAX! -->  ! MINIMUM AND MAXIMUM X OF THE DOMAIN            !
C ! ZMIN, ZMAX! -->  ! MINIMUM AND MAXIMUM Z OF THE DOMAIN            !
C !___________!______!________________________________________________!
C
C MODE : -->(NON MODIFIED DATA), <--(RESULT), <-->(MODIFIED DATA)
C     
C----------------------------------------------------------------------
C     
C SPARTACUS2D V5P9
C D. Violeau           & R. Issa
C +33(0)1-30-87-78-31 // +33(0)1-30-87-84-28 
C LNHE - 2008
C     
C FONCTION : estime la vitesse de frottement
C FUNCTION : estimates the friction velocity
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
      INTEGER NPMAX    , NPARF    
      INTEGER NBMAX    , NPARB    , NPARTQMAX
      INTEGER NQUADXMAX, NQUADZMAX, KUSTAR
      INTEGER I        , J        , L
      INTEGER M        , N        , K
      INTEGER MMIN     , MMAX     , NQUADX
      INTEGER NMIN     , NMAX     , NQUADZ
      INTEGER MQ       , NQ
C     
      DOUBLE PRECISION KAPPA, DELTA, GVN  , CR   , DESTDR
      DOUBLE PRECISION SIGMA, SUPP , H    , RUG  , XMIN
      DOUBLE PRECISION Q    , TEMPO, DR   , VEST , ZMIN
      DOUBLE PRECISION XEST , ZEST , PI   , XMAX , DEST
C
      INTEGER IPARTQ(NQUADXMAX,NQUADZMAX,NPARTQMAX)
      INTEGER NPARTQ(NQUADXMAX,NQUADZMAX)
C 
      DOUBLE PRECISION GVXX  (NPMAX), GVXZ  (NPMAX), USTAR(NBMAX)
      DOUBLE PRECISION GVZX  (NPMAX), GVZZ  (NPMAX), RHO  (NPMAX)
      DOUBLE PRECISION CTHETA(NBMAX), STHETA(NBMAX), X    (NPMAX)
      DOUBLE PRECISION VX    (NBMAX), VZ    (NBMAX), Z    (NPMAX)
      DOUBLE PRECISION MASS  (NBMAX)
C
      LOGICAL KPER
C
      SIGMA=15.D0/(7.D0*PI*H**2)
C 
      DEST=DESTDR*DR
C
C Estimation de la vitesse de frottement
C=======================================
C Friction velocity estimation
C=============================
C
      IF (KUSTAR.EQ.1) THEN
C
C Modele fonde sur le gradient de vitesse
C----------------------------------------
C Model based on velocity gradient estimation
C--------------------------------------------
C
        DO 407 I=1,NPARB
          J=NPARF+I
          GVN      = (GVXX(J)-GVZZ(J))*STHETA(I)*CTHETA(I)
     .               +GVXZ(J)*STHETA(I)**2
     .               -GVZX(J)*CTHETA(I)**2
          USTAR(I) =  GVN*KAPPA*DELTA
          IF (USTAR(I).LT.0.D0) USTAR(I)= MIN(USTAR(I),-1.D-16)
          IF (USTAR(I).GE.0.D0) USTAR(I)= MAX(USTAR(I), 1.D-16)
 407    CONTINUE
C
      ELSE
C
C Modele fonde sur la loi log
C----------------------------
C Model based on the log-law
C---------------------------
C
        DO 507 I=1,NPARB
          J=NPARF+I
C
C Estimation de la vitesse au voisinage de la paroi
C..................................................
C Estimation of the velocity near the wall
C.........................................
C 
          XEST=X(J)+DEST*CTHETA(I)
          ZEST=Z(J)+DEST*STHETA(I)
C
          VEST = 0.D0
C
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
C
          DO 122 M=MMIN,MMAX
            DO 161 N=NMIN,NMAX
              DO 110 L=1,NPARTQ(M,N)
                K = IPARTQ(M,N,L)
C
                Q=SQRT((XEST-X(K))**2+(ZEST-Z(K))**2)/H
                IF (Q.LE.2.) THEN
                  TEMPO=2. D0     /3. D0-9.D0*Q**2/8. D0
     .                 +19.D0*Q**3/24.D0-5.D0*Q**4/32.D0
                ELSE
                  TEMPO=0.D0
                ENDIF
                TEMPO=TEMPO*SIGMA*MASS(K)/RHO(K)
                VEST = VEST+TEMPO*(VX(K)*STHETA(I)-VZ(K)*CTHETA(I))
C
 110          CONTINUE
 161        CONTINUE
 122      CONTINUE
C
          IF (KPER) THEN
C
C Periodicite a gauche
C.....................
C "Left" periodicity
C...................
C
            IF (MQ.EQ.1) THEN
C
              DO 222 M=NQUADX-1,NQUADX
                DO 261 N=NMIN,NMAX
                  DO 210 L=1,NPARTQ(M,N)
                    K = IPARTQ(M,N,L)
C
                    Q=SQRT((XEST-X(K)+XMAX-XMIN)**2+(ZEST-Z(K))**2)/H
                    IF (Q.LE.2.) THEN
                      TEMPO=2. D0     /3. D0-9.D0*Q**2/8. D0
     .                     +19.D0*Q**3/24.D0-5.D0*Q**4/32.D0
                    ELSE
                      TEMPO=0.D0
                    ENDIF
                    TEMPO=TEMPO*SIGMA*MASS(K)/RHO(K)
                    VEST = VEST+TEMPO*(VX(K)*STHETA(I)-VZ(K)*CTHETA(I))
C
 210              CONTINUE
 261            CONTINUE
 222          CONTINUE
C
            ENDIF
C
C Periodicite a droite
C.....................
C "Right" periodicity
C....................
C
            IF (MQ.GE.NQUADX-1) THEN
C
              DO 322 M=1,2
                DO 361 N=NMIN,NMAX
                  DO 310 L=1,NPARTQ(M,N)
                    K = IPARTQ(M,N,L)
C
                    Q=SQRT((XEST-X(K)-XMAX+XMIN)**2+(ZEST-Z(K))**2)/H
                    IF (Q.LE.2.) THEN
                      TEMPO=2. D0     /3. D0-9.D0*Q**2/8. D0
     .                     +19.D0*Q**3/24.D0-5.D0*Q**4/32.D0
                    ELSE
                      TEMPO=0.D0
                    ENDIF
                    TEMPO=TEMPO*SIGMA*MASS(K)/RHO(K)
                    VEST = VEST+TEMPO*(VX(K)*STHETA(I)-VZ(K)*CTHETA(I))
C
 310              CONTINUE
 361            CONTINUE
 322          CONTINUE
C
            ENDIF
C
          ENDIF
C
C Estimation de la vitesse de frottement
C.......................................
C Friction velocity estimation
C.............................
C
          USTAR(I) = VEST/(LOG((DEST+DELTA)/RUG)/KAPPA+CR)
          IF (USTAR(I).LT.0.D0) USTAR(I)= MIN(USTAR(I),-1.D-16)
          IF (USTAR(I).GE.0.D0) USTAR(I)= MAX(USTAR(I), 1.D-16)
C
 507    CONTINUE
C
      ENDIF
C
      RETURN
      END
