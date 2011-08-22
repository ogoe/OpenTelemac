!                        *********************
                          SUBROUTINE INTERPOL
!                        *********************
!
     &  (NPARTQ, NPARTQMAX, NPMAX , NPOIN, NQUADX, NQUADXMAX,
     &   NQUADZ, NQUADZMAX, IPARTQ, KPER , EPS   , H        ,
     &   MASS  , NUT      , P     , PI   , PRIV  , RHO      ,
     &   S     , SUPP     , TEMPS , TKE  , VX    , VZ       ,
     &   X     , XINT     , XMIN  , XMAX , Z     , ZINT     ,
     &   ZMIN                                               )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! EPS       ! -->  ! DISSIPATION RATE                               !
! ! H         ! -->  ! SMOOTHING LENGTH                               !
! ! IPARTQ    ! -->  ! INDEX OF PARTICLES LOCATED IN A SQUARE         !
! ! KPER      ! -->  ! LOGICAL INDEX FOR PERIODICITY                  !
! ! LNG       ! -->  ! CHOICE INDEX FOR LANGUAGE                      !
! ! MASS      ! -->  ! PARTICLE MASS                                  !
! ! NPARTQ    ! -->  ! NUMBER OF PARTICLES IN A SQUARE                !
! ! NPARTQMAX ! -->  ! MAXIMAL NUMBER OF PARTICLES IN A SQUARE        !
! ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
! ! NPOIN     ! -->  ! NUMBER OF MESH POINTS FOR RUBENS               !
! ! NQUADX,                                                           !
! ! NQUADZ    ! -->  ! NUMBER OF SQUARES ALONG EACH DIRECTION         !
! ! NQUADXMAX,                                                        !
! ! NQUADZMAX ! -->  ! MAXIMUM NUMBER OF SQUARES ALONG EACH DIRECTION !
! ! NUT       ! -->  ! EDDY VISCOSITY                                 !
! ! P         ! -->  ! PRESSURE                                       !
! ! PI        ! -->  ! ARCHIMEDE'S NUMBER                             !
! ! PRIV      ! <--  ! PIVATE PRINTOUT VARIABLE FOR THE USER          !
! ! RHO       ! -->  ! DENSITY                                        !
! ! S         ! -->  ! RATE OF STRAIN                                 !
! ! SUPP      ! -->  ! KERNEL SUPPORT                                 !
! ! TEMPS     ! -->  ! PHYSICAL TIME                                  !
! ! TKE       ! -->  ! TURBULENT KINETIC ENERGY                       !
! ! VX, VZ    ! -->  ! VELOCITY COMPONENTS                            !
! ! X, Z      ! -->  ! PARTICLE POSITION                              !
! ! XINT, ZINT! -->  ! COORDINATES OF MESH POINTS                     !
! ! XMIN, XMAX! -->  ! MINIMUM AND MAXIMUM X OF THE DOMAIN            !
! ! ZMIN, ZMAX! -->  ! MINIMUM AND MAXIMUM Z OF THE DOMAIN            !
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
! FONCTION : interpole les donnees sur le maillage pour les sorties
! FUNCTION : interpolates the data on Rubens mesh for field printouts
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
      INTEGER NPMAX , NPOIN
      INTEGER NQUADX, NQUADXMAX, NPARTQMAX
      INTEGER NQUADZ, NQUADZMAX
      INTEGER I     , J
      INTEGER M     , N        , L
      INTEGER MMIN  , MMAX     , MQ
      INTEGER NMIN  , NMAX     , NQ
      INTEGER LNG   , LU
      COMMON/INFO/LNG,LU
!
      LOGICAL KPER
!
      DOUBLE PRECISION H, TEMPS, SIGMA, SUPP, PI
      DOUBLE PRECISION Q, TEMPO, XMIN , ZMIN, XMAX
!
      INTEGER IPARTQ(NQUADXMAX,NQUADZMAX,NPARTQMAX)
      INTEGER NPARTQ(NQUADXMAX,NQUADZMAX)
!
      REAL XINT   (NPOIN), ZINT  (NPOIN)
      REAL VXINT  (NPOIN), VZINT (NPOIN)
      REAL RHOINT (NPOIN), PINT  (NPOIN)
      REAL TKEINT (NPOIN), EPSINT(NPOIN)
      REAL NUTINT (NPOIN), SINT  (NPOIN)
      REAL PRIVINT(NPOIN)
!
      DOUBLE PRECISION X  (NPMAX), Z   (NPMAX), P   (NPMAX)
      DOUBLE PRECISION VX (NPMAX), VZ  (NPMAX), TKE (NPMAX)
      DOUBLE PRECISION RHO(NPMAX), MASS(NPMAX), EPS (NPMAX)
      DOUBLE PRECISION NUT(NPMAX), S   (NPMAX), PRIV(NPMAX)
!
      SIGMA=15.D0/(7.D0*PI*H**2)
!
! Interpolation
!==============
!
      PRINT*,''
      IF (LNG.EQ.1) THEN
        PRINT*,'===> Interpolation sur maillage'
      ELSEIF (LNG.EQ.2) THEN
        PRINT*,'===> Interpolation on Rubens mesh'
      ENDIF
!
      DO 100 I=1,NPOIN
!
        RHOINT (I)=0.
        VXINT  (I)=0.
        VZINT  (I)=0.
        PINT   (I)=0.
        TKEINT (I)=0.
        EPSINT (I)=0.
        NUTINT (I)=0.
        SINT   (I)=0.
        PRIVINT(I)=0.
!
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
!
        DO 122 M=MMIN,MMAX
          DO 161 N=NMIN,NMAX
           IF ((M.LE.NQUADX).AND.(N.LE.NQUADZ)) THEN
            DO 110 L=1,NPARTQ(M,N)
              J = IPARTQ(M,N,L)
!
              Q=SQRT((XINT(I)-X(J))**2
     &              +(ZINT(I)-Z(J))**2)/H
              IF (Q.LE.2.) THEN
                TEMPO=2. D0     /3. D0-9.D0*Q**2/8. D0
     &               +19.D0*Q**3/24.D0-5.D0*Q**4/32.D0
              ELSE
                TEMPO=0.D0
              ENDIF
              TEMPO=TEMPO*SIGMA*MASS(J)/RHO(J)
!
              RHOINT (I) = RHOINT (I) + TEMPO*RHO (J)
              VXINT  (I) = VXINT  (I) + TEMPO*VX  (J)
              VZINT  (I) = VZINT  (I) + TEMPO*VZ  (J)
              PINT   (I) = PINT   (I) + TEMPO*P   (J)
              TKEINT (I) = TKEINT (I) + TEMPO*TKE (J)
              EPSINT (I) = EPSINT (I) + TEMPO*EPS (J)
              NUTINT (I) = NUTINT (I) + TEMPO*NUT (J)
              SINT   (I) = SINT   (I) + TEMPO*S   (J)
              PRIVINT(I) = PRIVINT(I) + TEMPO*PRIV(J)
!
 110        CONTINUE
           ENDIF
 161      CONTINUE
 122    CONTINUE
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
               IF ((M.LE.NQUADX).AND.(N.LE.NQUADZ)) THEN
                DO 210 L=1,NPARTQ(M,N)
                  J = IPARTQ(M,N,L)
!
                  Q=SQRT((XINT(I)-X(J)+XMAX-XMIN)**2
     &                  +(ZINT(I)-Z(J))**2)/H
                  IF (Q.LE.2.) THEN
                    TEMPO=2. D0     /3. D0-9.D0*Q**2/8. D0
     &                   +19.D0*Q**3/24.D0-5.D0*Q**4/32.D0
                  ELSE
                    TEMPO=0.D0
                  ENDIF
                  TEMPO=TEMPO*SIGMA*MASS(J)/RHO(J)
!
                  RHOINT (I) = RHOINT (I) + TEMPO*RHO (J)
                  VXINT  (I) = VXINT  (I) + TEMPO*VX  (J)
                  VZINT  (I) = VZINT  (I) + TEMPO*VZ  (J)
                  PINT   (I) = PINT   (I) + TEMPO*P   (J)
                  TKEINT (I) = TKEINT (I) + TEMPO*TKE (J)
                  EPSINT (I) = EPSINT (I) + TEMPO*EPS (J)
                  NUTINT (I) = NUTINT (I) + TEMPO*NUT (J)
                  SINT   (I) = SINT   (I) + TEMPO*S   (J)
                  PRIVINT(I) = PRIVINT(I) + TEMPO*PRIV(J)
!
 210            CONTINUE
               ENDIF
 261          CONTINUE
 222        CONTINUE
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
               IF ((M.LE.NQUADX).AND.(N.LE.NQUADZ)) THEN
                DO 310 L=1,NPARTQ(M,N)
                  J = IPARTQ(M,N,L)
!
                  Q=SQRT((XINT(I)-X(J)-XMAX+XMIN)**2
     &                  +(ZINT(I)-Z(J))**2)/H
                  IF (Q.LE.2.) THEN
                    TEMPO=2. D0     /3. D0-9.D0*Q**2/8. D0
     &                   +19.D0*Q**3/24.D0-5.D0*Q**4/32.D0
                  ELSE
                    TEMPO=0.D0
                  ENDIF
                  TEMPO=TEMPO*SIGMA*MASS(J)/RHO(J)
!
                  RHOINT (I) = RHOINT (I) + TEMPO*RHO (J)
                  VXINT  (I) = VXINT  (I) + TEMPO*VX  (J)
                  VZINT  (I) = VZINT  (I) + TEMPO*VZ  (J)
                  PINT   (I) = PINT   (I) + TEMPO*P   (J)
                  TKEINT (I) = TKEINT (I) + TEMPO*TKE (J)
                  EPSINT (I) = EPSINT (I) + TEMPO*EPS (J)
                  NUTINT (I) = NUTINT (I) + TEMPO*NUT (J)
                  SINT   (I) = SINT   (I) + TEMPO*S   (J)
                  PRIVINT(I) = PRIVINT(I) + TEMPO*PRIV(J)
!
 310            CONTINUE
               ENDIF
 361          CONTINUE
 322        CONTINUE
!
          ENDIF
!
        ENDIF
!
 100  CONTINUE
!
! Ecriture fichier champs Rubens
!===============================
! Field file writing for Rubens
!==============================
!
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
!
 903  FORMAT (A,D16.8)
!
      RETURN
      END