C                        *********************
                          SUBROUTINE INTERPOL
C                        *********************
C
     .  (NPARTQ, NPARTQMAX, NPMAX , NPOIN, NQUADX, NQUADXMAX, 
     .   NQUADZ, NQUADZMAX, IPARTQ, KPER , EPS   , H        , 
     .   MASS  , NUT      , P     , PI   , PRIV  , RHO      , 
     .   S     , SUPP     , TEMPS , TKE  , VX    , VZ       , 
     .   X     , XINT     , XMIN  , XMAX , Z     , ZINT     , 
     .   ZMIN                                               )     
C
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! EPS       ! -->  ! DISSIPATION RATE                               !
C ! H         ! -->  ! SMOOTHING LENGTH                               !
C ! IPARTQ    ! -->  ! INDEX OF PARTICLES LOCATED IN A SQUARE         !
C ! KPER      ! -->  ! LOGICAL INDEX FOR PERIODICITY                  !
C ! LNG       ! -->  ! CHOICE INDEX FOR LANGUAGE                      !
C ! MASS      ! -->  ! PARTICLE MASS                                  !
C ! NPARTQ    ! -->  ! NUMBER OF PARTICLES IN A SQUARE                !
C ! NPARTQMAX ! -->  ! MAXIMAL NUMBER OF PARTICLES IN A SQUARE        !
C ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
C ! NPOIN     ! -->  ! NUMBER OF MESH POINTS FOR RUBENS               !
C ! NQUADX,                                                           !
C ! NQUADZ    ! -->  ! NUMBER OF SQUARES ALONG EACH DIRECTION         !
C ! NQUADXMAX,                                                        !
C ! NQUADZMAX ! -->  ! MAXIMUM NUMBER OF SQUARES ALONG EACH DIRECTION !
C ! NUT       ! -->  ! EDDY VISCOSITY                                 !
C ! P         ! -->  ! PRESSURE                                       !
C ! PI        ! -->  ! ARCHIMEDE'S NUMBER                             !
C ! PRIV      ! <--  ! PIVATE PRINTOUT VARIABLE FOR THE USER          !
C ! RHO       ! -->  ! DENSITY                                        !
C ! S         ! -->  ! RATE OF STRAIN                                 !
C ! SUPP      ! -->  ! KERNEL SUPPORT                                 !
C ! TEMPS     ! -->  ! PHYSICAL TIME                                  !
C ! TKE       ! -->  ! TURBULENT KINETIC ENERGY                       !
C ! VX, VZ    ! -->  ! VELOCITY COMPONENTS                            ! 
C ! X, Z      ! -->  ! PARTICLE POSITION                              !
C ! XINT, ZINT! -->  ! COORDINATES OF MESH POINTS                     !
C ! XMIN, XMAX! -->  ! MINIMUM AND MAXIMUM X OF THE DOMAIN            !
C ! ZMIN, ZMAX! -->  ! MINIMUM AND MAXIMUM Z OF THE DOMAIN            !
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
C FONCTION : interpole les donnees sur le maillage pour les sorties
C FUNCTION : interpolates the data on Rubens mesh for field printouts
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
      INTEGER NPMAX , NPOIN   
      INTEGER NQUADX, NQUADXMAX, NPARTQMAX
      INTEGER NQUADZ, NQUADZMAX
      INTEGER I     , J
      INTEGER M     , N        , L
      INTEGER MMIN  , MMAX     , MQ
      INTEGER NMIN  , NMAX     , NQ
      INTEGER LNG   , LU
      COMMON/INFO/LNG,LU      
C
      LOGICAL KPER
C
      DOUBLE PRECISION H, TEMPS, SIGMA, SUPP, PI
      DOUBLE PRECISION Q, TEMPO, XMIN , ZMIN, XMAX
C
      INTEGER IPARTQ(NQUADXMAX,NQUADZMAX,NPARTQMAX)
      INTEGER NPARTQ(NQUADXMAX,NQUADZMAX)
C
      REAL XINT   (NPOIN), ZINT  (NPOIN)
      REAL VXINT  (NPOIN), VZINT (NPOIN)
      REAL RHOINT (NPOIN), PINT  (NPOIN)
      REAL TKEINT (NPOIN), EPSINT(NPOIN)
      REAL NUTINT (NPOIN), SINT  (NPOIN)
      REAL PRIVINT(NPOIN)
C
      DOUBLE PRECISION X  (NPMAX), Z   (NPMAX), P   (NPMAX)
      DOUBLE PRECISION VX (NPMAX), VZ  (NPMAX), TKE (NPMAX)
      DOUBLE PRECISION RHO(NPMAX), MASS(NPMAX), EPS (NPMAX)
      DOUBLE PRECISION NUT(NPMAX), S   (NPMAX), PRIV(NPMAX)
C
      SIGMA=15.D0/(7.D0*PI*H**2)
C
C Interpolation
C==============
C
      PRINT*,''
      IF (LNG.EQ.1) THEN
        PRINT*,'===> Interpolation sur maillage'
      ELSEIF (LNG.EQ.2) THEN
        PRINT*,'===> Interpolation on Rubens mesh'
      ENDIF
C
      DO 100 I=1,NPOIN
C
        RHOINT (I)=0.
        VXINT  (I)=0.
        VZINT  (I)=0.
        PINT   (I)=0.
        TKEINT (I)=0.
        EPSINT (I)=0.
        NUTINT (I)=0.
        SINT   (I)=0.
        PRIVINT(I)=0.
C
        MQ=INT((XINT(I)-XMIN)/(SUPP*H))+1
        NQ=INT((ZINT(I)-ZMIN)/(SUPP*H))+1
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
           IF ((M.LE.NQUADX).AND.(N.LE.NQUADZ)) THEN
            DO 110 L=1,NPARTQ(M,N)
              J = IPARTQ(M,N,L)
C
              Q=SQRT((XINT(I)-X(J))**2
     .              +(ZINT(I)-Z(J))**2)/H
              IF (Q.LE.2.) THEN
                TEMPO=2. D0     /3. D0-9.D0*Q**2/8. D0
     .               +19.D0*Q**3/24.D0-5.D0*Q**4/32.D0
              ELSE
                TEMPO=0.D0
              ENDIF
              TEMPO=TEMPO*SIGMA*MASS(J)/RHO(J)
C
              RHOINT (I) = RHOINT (I) + TEMPO*RHO (J)
              VXINT  (I) = VXINT  (I) + TEMPO*VX  (J)
              VZINT  (I) = VZINT  (I) + TEMPO*VZ  (J)
              PINT   (I) = PINT   (I) + TEMPO*P   (J)
              TKEINT (I) = TKEINT (I) + TEMPO*TKE (J)
              EPSINT (I) = EPSINT (I) + TEMPO*EPS (J)
              NUTINT (I) = NUTINT (I) + TEMPO*NUT (J)
              SINT   (I) = SINT   (I) + TEMPO*S   (J)
              PRIVINT(I) = PRIVINT(I) + TEMPO*PRIV(J)
C
 110        CONTINUE
           ENDIF
 161      CONTINUE
 122    CONTINUE
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
               IF ((M.LE.NQUADX).AND.(N.LE.NQUADZ)) THEN
                DO 210 L=1,NPARTQ(M,N)
                  J = IPARTQ(M,N,L)
C
                  Q=SQRT((XINT(I)-X(J)+XMAX-XMIN)**2
     .                  +(ZINT(I)-Z(J))**2)/H
                  IF (Q.LE.2.) THEN
                    TEMPO=2. D0     /3. D0-9.D0*Q**2/8. D0
     .                   +19.D0*Q**3/24.D0-5.D0*Q**4/32.D0
                  ELSE
                    TEMPO=0.D0
                  ENDIF
                  TEMPO=TEMPO*SIGMA*MASS(J)/RHO(J)
C
                  RHOINT (I) = RHOINT (I) + TEMPO*RHO (J)
                  VXINT  (I) = VXINT  (I) + TEMPO*VX  (J)
                  VZINT  (I) = VZINT  (I) + TEMPO*VZ  (J)
                  PINT   (I) = PINT   (I) + TEMPO*P   (J)
                  TKEINT (I) = TKEINT (I) + TEMPO*TKE (J)
                  EPSINT (I) = EPSINT (I) + TEMPO*EPS (J)
                  NUTINT (I) = NUTINT (I) + TEMPO*NUT (J)
                  SINT   (I) = SINT   (I) + TEMPO*S   (J)
                  PRIVINT(I) = PRIVINT(I) + TEMPO*PRIV(J)
C
 210            CONTINUE
               ENDIF
 261          CONTINUE
 222        CONTINUE
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
               IF ((M.LE.NQUADX).AND.(N.LE.NQUADZ)) THEN
                DO 310 L=1,NPARTQ(M,N)
                  J = IPARTQ(M,N,L)
C
                  Q=SQRT((XINT(I)-X(J)-XMAX+XMIN)**2
     .                  +(ZINT(I)-Z(J))**2)/H
                  IF (Q.LE.2.) THEN
                    TEMPO=2. D0     /3. D0-9.D0*Q**2/8. D0
     .                   +19.D0*Q**3/24.D0-5.D0*Q**4/32.D0
                  ELSE
                    TEMPO=0.D0
                  ENDIF
                  TEMPO=TEMPO*SIGMA*MASS(J)/RHO(J)
C
                  RHOINT (I) = RHOINT (I) + TEMPO*RHO (J)
                  VXINT  (I) = VXINT  (I) + TEMPO*VX  (J)
                  VZINT  (I) = VZINT  (I) + TEMPO*VZ  (J)
                  PINT   (I) = PINT   (I) + TEMPO*P   (J)
                  TKEINT (I) = TKEINT (I) + TEMPO*TKE (J)
                  EPSINT (I) = EPSINT (I) + TEMPO*EPS (J)
                  NUTINT (I) = NUTINT (I) + TEMPO*NUT (J)
                  SINT   (I) = SINT   (I) + TEMPO*S   (J)
                  PRIVINT(I) = PRIVINT(I) + TEMPO*PRIV(J)
C
 310            CONTINUE
               ENDIF
 361          CONTINUE
 322        CONTINUE
C
          ENDIF
C
        ENDIF
C
 100  CONTINUE
C
C Ecriture fichier champs Rubens
C===============================
C Field file writing for Rubens
C==============================
C 
      WRITE (48)  REAL(TEMPS)
      WRITE (48) (REAL(VXINT  (I)),I=1,NPOIN)
      WRITE (48) (REAL(VZINT  (I)),I=1,NPOIN)
      WRITE (48) (REAL(RHOINT (I)),I=1,NPOIN)
      WRITE (48) (REAL(PINT   (I)),I=1,NPOIN)
      WRITE (48) (REAL(TKEINT (I)),I=1,NPOIN)
      WRITE (48) (REAL(EPSINT (I)),I=1,NPOIN)
      WRITE (48) (REAL(NUTINT (I)),I=1,NPOIN)
      WRITE (48) (REAL(SINT   (I)),I=1,NPOIN)
      WRITE (48) (REAL(PRIVINT(I)),I=1,NPOIN)
C
 903  FORMAT (A,D16.8)
C
      RETURN
      END
