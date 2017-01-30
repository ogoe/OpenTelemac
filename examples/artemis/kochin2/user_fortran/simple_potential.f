!     ******************************
      SUBROUTINE SIMPLE_POTENTIAL
!     ******************************

     &( X        , Y         , WNB       , PHI_RE   , PHI_IM,
     & DDXPHI_RE , DDYPHI_RE , DDXPHI_IM , DDYPHI_IM)
!
      IMPLICIT NONE
!
!.....Variables transmises
!     """"""""""""""""""""
      DOUBLE PRECISION X     , Y     , WNB   , PHI_RE, PHI_IM,H
!
!CP
      DOUBLE PRECISION DDXPHI_RE , DDYPHI_RE , DDXPHI_IM , DDYPHI_IM
!CP
!.....Variables locales
!     """""""""""""""""
      INTEGER          IX    , II
      DOUBLE PRECISION PI    , R     , TETA  , XX    , MODZ  , ARGZ  ,
     &                 AUX1  , AUX2  , CORREC
      DOUBLE PRECISION MODZLU(38), ARGZLU(38)
!
!     A VIRER  !!!!!!!!!    A VIRER  !!!!!!!!
      DOUBLE PRECISION OMEGA , GRAVIT

!CP
      DOUBLE PRECISION PHTETA,DPHTETA,MODTETA,DMODTETA
      DOUBLE PRECISION ANG1,ANG2,AR,BR,RP12,RP32
      DOUBLE PRECISION DDTPHR,DDTPHI,DDRPHR,DDRPHI
      DOUBLE PRECISION DDTMODZ(38),DDTARGZ(38)
!CP

      H=1.0D0
      GRAVIT=9.81D0
      OMEGA=DSQRT(GRAVIT*WNB)
!
      PI=4.0D0*DATAN(1.0D0)
      CORREC=-4.0D0*PI
!      correc=1.D0
!
!      WRITE(6,*) 'ON ENTRE CHEZ MICHEL'
!.....Calcul du rayon (R) et de l'angle (TETA, en degres sur [-180;180])
!.....à partir des coordonnees cartesiennes (X et Y) du point considere.
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      R=DSQRT(X*X+Y*Y)
      TETA=DATAN2(Y,X)*180.D0/PI
!      WRITE(6,*) 'TETA=',  X  ,  Y ,  TETA


  201 IF (TETA.LT.-180.D0) THEN
        TETA=TETA+360.D0
        GOTO 201
      ENDIF
  202 IF (TETA.GT.180.D0) THEN
        TETA=TETA-360.D0
        GOTO 202
      ENDIF


      AUX1=H*GRAVIT/(2.0D0*OMEGA)
      AUX2=WNB*R

      PHI_RE=  AUX1*SIN(AUX2)
      PHI_IM= -AUX1*COS(AUX2)

!CP
!.....GRADIENT
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
! -- DDR POTENTIEL

      AUX1  = AUX1*WNB

      DDRPHR=AUX1*COS(AUX2)
      DDRPHI=AUX1*SIN(AUX2)

! -- DDTETA POTENTIEL

      DDTPHR=0.0D0
      DDTPHI=0.0D0

! -- GRADIENT DANS LE REPERE ORTHO
      AUX2=TETA*PI/180.D0
      DDXPHI_RE=(DDRPHR*COS(AUX2)-(1.D0/R)*DDTPHR*SIN(AUX2))
      DDYPHI_RE=(DDRPHR*SIN(AUX2)+(1.D0/R)*DDTPHR*COS(AUX2))
      DDXPHI_IM=(DDRPHI*COS(AUX2)-(1.D0/R)*DDTPHI*SIN(AUX2))
      DDYPHI_IM=(DDRPHI*SIN(AUX2)+(1.D0/R)*DDTPHI*COS(AUX2))
!CP
!      WRITE(6,*) 'K=',WNB


      RETURN
      END

