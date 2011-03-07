!                    *****************
                     SUBROUTINE INIPHY
!                    *****************
!
     &( XK    , CG    , B     , DEPTH , FREQ  , COSPHI, NPOIN2, NF    ,
     &  PROINF, SPHE  )
!
!***********************************************************************
! TOMAWAC   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE WAVE PARAMETERS THAT ARE TIME-INDEPENDENT
!+               (WAVE NUMBER, GROUP VELOCITY,...).
!
!note     ALL THE DIRECTIONS ARE IN RADIAN AND IN THE RANGE [0 ; 2PI].
!
!history  M. BENOIT (EDF/DER/LNH)
!+        07/02/95
!+        V1P0
!+   
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into 
!+   English comments 
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and 
!+   cross-referencing of the FORTRAN sources 
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| B              |---| 
!| CG             |---| 
!| COSPHI         |---| 
!| DEPTH          |---| 
!| FREQ           |---| 
!| NF             |-->| NOMBRE DE FREQUENCES
!| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE SPATIAL 2D
!| PROINF         |-->| INDICATEUR CALCUL EN PROFONDEUR INFINIE
!| SPHE           |-->| INDICATEUR CALCUL EN SPHERIQUE
!| XK             |---| 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER          NF    , NPOIN2
      DOUBLE PRECISION DEPTH(NPOIN2)    , COSPHI(NPOIN2), FREQ(NF)
      DOUBLE PRECISION B(NPOIN2,NF)  , XK(NPOIN2,NF) , CG(NPOIN2,NF)
      LOGICAL          PROINF, SPHE
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER          IP    , JF
      DOUBLE PRECISION DEUPI2, DEUPI , XG    , DPDSUG, AUX2
      DOUBLE PRECISION AUX1  , AUX3  , DEUKD , R2
!
!
      XG=9.81D0
      DEUPI=2.D0*3.14159265D0
      DEUPI2=DEUPI*DEUPI
      DPDSUG=DEUPI2/XG
      R2=(6400.D3)**2
!
      IF (PROINF) THEN
!                               +----------------------+
!.............................. ! INFINITE WATER DEPTH !
!                               +----------------------+
        DO 310 JF=1,NF
          AUX1=DPDSUG*(FREQ(JF))**2
          AUX3=0.5D0*XG/(DEUPI*FREQ(JF))
          DO 320 IP=1,NPOIN2
            XK(IP,JF)=AUX1
            CG(IP,JF)=AUX3
  320     CONTINUE
  310   CONTINUE
      ELSE
!                               +--------------------+
!.............................. ! FINITE WATER DEPTH !
!                               +--------------------+
        DO 410 JF=1,NF
          AUX2=DEUPI*FREQ(JF)
          DO 430 IP=1,NPOIN2
            CALL WNSCOU(AUX1,FREQ(JF),DEPTH(IP))
            DEUKD=2.D0*AUX1*DEPTH(IP)
            IF (DEUKD.GT.7.D2) THEN
              AUX3=0.5D0*AUX2/AUX1
            ELSE
              AUX3=0.5D0*(1.D0+DEUKD/SINH(DEUKD))*AUX2/AUX1
            ENDIF
            XK(IP,JF)=AUX1
            CG(IP,JF)=AUX3
  430     CONTINUE
  410   CONTINUE
      ENDIF
!
!
!.....COMPUTES B TO GO FROM (KX, KY) TO (FR, TETA)
!     ===================================================
      IF (.NOT.SPHE) THEN
!                               +-----------------------------+
!.............................. ! CARTESIAN COORDINATE SYSTEM !
!                               +-----------------------------+
        DO 710 JF=1,NF
          AUX1=DEUPI2*FREQ(JF)
          DO 720 IP=1,NPOIN2
            B(IP,JF)= CG(IP,JF)/(AUX1*XK(IP,JF))
  720     CONTINUE
  710   CONTINUE
!
      ELSE
!                               +-----------------------------+
!.............................. ! SPHERICAL COORDINATE SYSTEM !
!                               +-----------------------------+
        DO 810 JF=1,NF
          AUX1=DEUPI2*FREQ(JF)*R2
          DO 820 IP=1,NPOIN2
            B(IP,JF)= CG(IP,JF)/(AUX1*XK(IP,JF)*COSPHI(IP))
  820     CONTINUE
  810   CONTINUE
      ENDIF
!
      RETURN
      END