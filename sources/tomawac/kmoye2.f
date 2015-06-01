!                       *****************
                        SUBROUTINE KMOYE2
!                       *****************
!
     &( XKMOY , XK    , F     , FREQ  , DFREQ , TAILF , NF    , NPLAN ,
     &  NPOIN2, AUX1  , AUX2  , AUX3  )
!
!**********************************************************************
!  TOMAWAC - V1.0    P. THELLIER & M. BENOIT (EDF/DER/LNH)  -  04/04/95
!**********************************************************************
!
!  FONCTION : CALCUL DU NOMBRE D'ONDE MOYEN EN TOUS LES POINTS DU
!  ********   MAILLAGE SPATIAL 2D.
!
!  ARGUMENTS :
!  ***********
!  +-------------+----+--------------------------------------------+
!  ! NOM         !MODE! SIGNIFICATION - OBSERVATIONS               !
!  +-------------+----+--------------------------------------------+
!  ! XKMOY(-)    !<-- ! TABLEAU DES NOMBRES D'ONDE MOYEN           !
!  ! XK(-,-)     ! -->! TABLEAU DES NOMBRES D'ONDE                 !
!  ! F(-,-,-)    ! -->! SPECTRE DIRECTIONNEL                       !
!  ! FREQ(-)     ! -->! TABLEAU DES FREQUENCES DE DISCRETISATION   !
!  ! DFREQ(-)    ! -->! TABLEAU DES PAS DE FREQUENCE               !
!  ! TAILF       ! -->! FACTEUR DE QUEUE (TAILF = 4 OU 5)          !
!  ! NF          ! -->! NOMBRE DE FREQUENCES DE DISCRETISATION     !
!  ! NPLAN       ! -->! NOMBRE DE DIRECTIONS DE DISCRETISATION     !
!  ! NPOIN2      ! -->! NOMBRE DE POINTS DU MAILLAGE SPATIAL       !
!  ! AUX1(-)     !<-->! TABLEAU DE TRAVAIL (DIMENSION NPOIN2)      !
!  ! AUX2(-)     !<-->! TABLEAU DE TRAVAIL (DIMENSION NPOIN2)      !
!  ! AUX3(-)     !<-->! TABLEAU DE TRAVAIL (DIMENSION NPOIN2)      !
!  +-------------+----+--------------------------------------------+
!  ! MODE   (-> : NON-MODIFIE)  (<-> : MODIFIE)  (<- : INITIALISE) !
!  +---------------------------------------------------------------+
!
!  APPELS :    - PROGRAMME(S) APPELANT  :  SEMIMP, PRE2D
!  ********    - PROGRAMME(S) APPELE(S) :    -
!
!  REMARQUES :
!  ***********
!  - LA PARTIE HAUTES-FREQUENCES DU SPECTRE N'EST PRISE EN COMPTE QUE
!    SI LE FACTEUR DE QUEUE (TAILF) EST STRICTEMENT SUPERIEUR A 1.
!
!**********************************************************************
!
      IMPLICIT NONE
!
!.....VARIABLES TRANSMISES
!     """"""""""""""""""""
      INTEGER  NF    , NPLAN , NPOIN2
      DOUBLE PRECISION TAILF
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF), XK(NPOIN2,NF)
      DOUBLE PRECISION FREQ(NF)  , DFREQ(NF) , XKMOY(NPOIN2)
      DOUBLE PRECISION AUX1(NPOIN2) , AUX2(NPOIN2) , AUX3(NPOIN2)
!
!.....VARIABLES LOCALES
!     """""""""""""""""
      INTEGER  IPLAN , JF    , IP
      DOUBLE PRECISION COEFF , PI    , SEUIL , CTE1  , CTE2  , AUX4
!
!
      PI = 3.141592654D0
      SEUIL = 1.D-20
      COEFF = SQRT(9.806D0)/(2.D0*PI)
      DO IP = 1,NPOIN2
        AUX1(IP) = 0.D0
        AUX2(IP) = 0.D0
      END DO
!
!.....SOMMATIONS SUR LA PARTIE DISCRETISEE DU SPECTRE.
!     """"""""""""""""""""""""""""""""""""""""""""""""
      DO 20 JF = 1,NF
        AUX4=DFREQ(JF)
!
        DO 15 IP=1,NPOIN2
          AUX3(IP) = 0.D0
   15   CONTINUE
        DO 10 IPLAN = 1,NPLAN
          DO 5 IP=1,NPOIN2
            AUX3(IP) = AUX3(IP) + F(IP,IPLAN,JF)
    5     CONTINUE
   10   CONTINUE
!
        DO 25 IP = 1,NPOIN2
          AUX1(IP)=AUX1(IP)+AUX3(IP)*SQRT(XK(IP,JF))*AUX4
          AUX2(IP)=AUX2(IP)+AUX3(IP)*AUX4
   25   CONTINUE
!
   20 CONTINUE
!
!.....PRISE EN COMPTE EVENTUELLE DE LA PARTIE HAUTES FREQUENCES.
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
      IF (TAILF.GT.1.D0) THEN
        CTE1=FREQ(NF)**2/(TAILF-2.D0)/COEFF
        CTE2=FREQ(NF)/(TAILF-1.D0)
      ELSE
        CTE1=0.D0
        CTE2=0.D0
      ENDIF
      DO 45 IP=1,NPOIN2
        AUX1(IP) = AUX1(IP) + AUX3(IP)*CTE1
        AUX2(IP) = AUX2(IP) + AUX3(IP)*CTE2
   45 CONTINUE
!
!.....CALCUL DU NOMBRE D'ONDE MOYEN.
!     """"""""""""""""""""""""""""""
      DO 50 IP=1,NPOIN2
        IF (AUX2(IP).LT.SEUIL) THEN
          XKMOY(IP) = 1.D0
        ELSE
          XKMOY(IP) = (AUX1(IP)/AUX2(IP))**2
        ENDIF
   50 CONTINUE
!
      RETURN
      END
