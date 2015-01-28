!                    *****************
                     SUBROUTINE BORD3D
!                    *****************
!
     &(TIME,LT,ENTET,NPTFR2_DIM,NFRLIQ)
!
!***********************************************************************
! TELEMAC3D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    SPECIFIC BOUNDARY CONDITIONS.
!
!note     1) FOR PRESCRIBED BOUNDARIES OF POINT BEING BOTH LATERAL
!+            AND BOTTOM : USE LATERAL ARRAYS.
!+
!+     2) FOR TYPES OF BOUNDARY CONDITIONS : USE SUBROUTINE LIMI3D.
!+
!+     3) SEDIMENT IS THE LAST TRACER.
!
!warning  MAY BE MODIFIED BY THE USER
!
!history
!+        11/02/2008
!+
!+   LOOP ON THE BOUNDARY POINTS SPLIT IN 3 LOOPS TO REVERSE
!
!history  J.-M. HERVOUET (LNHE)
!+        21/08/2009
!+        V6P0
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
!history  J.-M. HERVOUET (LNHE)
!+        19/09/2011
!+        V6P2
!+   Call to DEBIMP3D replaced by CALL DEBIMP_3D (new arguments)
!
!history  J.-M. HERVOUET (LNHE)
!+        11/03/2013
!+        V6P3
!+   Test IFRLIQ.NE.0 line 210.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ENTET          |-->| LOGICAL, IF YES INFORMATION IS GIVEN ON MASS
!|                |   | CONSERVATION.
!| LT             |-->| CURRENT TIME STEP NUMBER
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NPTFR2_DIM     |-->| NPTFR2? NOT USED
!| TIME           |-->| TIME OF TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D, EX_NFRLIQ=>NFRLIQ
      USE INTERFACE_TELEMAC3D, EX_BORD3D => BORD3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     TIME AND ENTET ARE AT AND INFOGR (NOW IN DECLARATIONS_TELEMAC3D)
      DOUBLE PRECISION, INTENT(IN)    :: TIME
      INTEGER         , INTENT(IN)    :: LT
      LOGICAL         , INTENT(IN)    :: ENTET
      INTEGER         , INTENT(IN)    :: NPTFR2_DIM
      INTEGER         , INTENT(IN)    :: NFRLIQ
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN2,NP,IBORD,IVIT,ICOT,IDEB,IFRLIQ,IPROF,K,N
      INTEGER IPTFR,ITRAC,IPLAN,I3D
      LOGICAL YAZMIN
      DOUBLE PRECISION ROEAU,ROAIR,VITV,PROFZ,WINDRELX,WINDRELY
!
      DOUBLE PRECISION P_DMIN
      INTEGER  P_IMAX
      EXTERNAL P_IMAX,P_DMIN
      DOUBLE PRECISION STA_DIS_CUR
      EXTERNAL STA_DIS_CUR
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION ZMIN(MAXFRO)
!
      INTEGER YADEB(MAXFRO),MSK1,IJK
!
!========================================================================
!
      DOUBLE PRECISION ALF ,PI,Y100,AJUL4,NLUN, HT
      DOUBLE PRECISION  DD,JD,TUNIV,HSUN,SLUN,PLUN,TLUN,PSUN
      DOUBLE PRECISION TONDES (120) 
      DOUBLE PRECISION UONDES(120),FONDES(120),VONDES(120)
      DOUBLE PRECISION AHN(50,120) , PHN(50,120)
      INTEGER NPTFRL,IPTFRL,NPTFRLM,IONDES,NONDES,NSPECTR
      INTEGER YY,MM,DAY,HOUR,MINU,SEC,AJUL,BJUL,NFO1
      DOUBLE PRECISION NIVM(50)        
      DOUBLE PRECISION PROF(NPOIN2)
      INTEGER NSURF(NPTFR2)
      DOUBLE PRECISION TEMPS
      INTEGER COMPTEUR
!
      INTRINSIC COS,SIN,INT,MOD,ACOS
!
      SAVE AHN, PHN 
!   
!=========================================================================
!     DOUBLE PRECISION XB,YB,ZB,WW,TREEL,A,B,CP,LAMB,RO,RO0,SAL
!     INTGER ITEMP
!
!     SIMPLE CASES FOR LATERAL BOUNDARIES ARE TREATED AUTOMATICALLY:
!
!     - PRESCRIBED DEPTH     (5 4 4)
!     - PRESCRIBED VELOCITY  (  6 6)
!     - PRESCRIBED DISCHARGE (  5 5)
!
!     CORRESPONDING KEYWORDS ARE:
!
!     'PRESCRIBED ELEVATIONS' OR 'COTES IMPOSEES'
!     'PRESCRIBED VELOCITIES' OR 'VITESSES IMPOSEES'
!     'PRESCRIBED FLOWRATES' OR 'DEBITS IMPOSES'
!
!     THE IMPLEMENTATION OF AUTOMATIC CASES MAY BE CANCELLED
!     PROVIDED THAT THE RELEVANT ARRAYS ARE FILLED
!
!
!***********************************************************************
!
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
!              AUTOMATIC TREATMENT OF LIQUID BOUNDARIES
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
!
!=======================================================================
!
!     SECURES NO SLIP BOUNDARY CONDITIONS
!
      IF(LT.EQ.1) THEN
!
!     VELOCITIES
!
      DO IPTFR = 1,NPTFR2
        IPOIN2 = NBOR2%I(IPTFR)
        DO IPLAN = 1,NPLAN
          IBORD = (IPLAN-1)*NPTFR2 + IPTFR
          IF(LIUBOL%I(IBORD).EQ.KADH) UBORL%R(IBORD) = 0.D0
          IF(LIVBOL%I(IBORD).EQ.KADH) VBORL%R(IBORD) = 0.D0
          IF(LIWBOL%I(IBORD).EQ.KADH) WBORL%R(IBORD) = 0.D0
        ENDDO
      ENDDO
!
      DO IPOIN2 = 1,NPOIN2
        IF(LIUBOF%I(IPOIN2).EQ.KADH) UBORF%R(IPOIN2) = 0.D0
        IF(LIVBOF%I(IPOIN2).EQ.KADH) VBORF%R(IPOIN2) = 0.D0
        IF(LIWBOF%I(IPOIN2).EQ.KADH) WBORF%R(IPOIN2) = 0.D0
        IF(LIUBOS%I(IPOIN2).EQ.KADH) UBORS%R(IPOIN2) = 0.D0
        IF(LIVBOS%I(IPOIN2).EQ.KADH) VBORS%R(IPOIN2) = 0.D0
        IF(LIWBOS%I(IPOIN2).EQ.KADH) WBORS%R(IPOIN2) = 0.D0
      ENDDO

      END IF
!
!===================================================================
!
      PI=ACOS(-1.D0)
      NPTFRLM = 28
      NFO1 = T3DFO1
      REWIND NFO1
      READ(NFO1,*) NSPECTR
        
      READ(NFO1,*) NONDES
      
      DO IONDES =1,NONDES
        READ (NFO1,*) TONDES(IONDES)
      ENDDO
      
      DO IONDES =NONDES+1,NSPECTR
        READ (NFO1,*) TONDES(IONDES)
      ENDDO
      DO IPTFRL = 1,NPTFRLM           
        IONDES=1
!       LECTURE DES HN ET GN DES NONDES POUR LES DIFFERENTS NOEUDS FRONTIÈRES
        READ(NFO1,*)
        READ(NFO1,*)   NIVM(IPTFRL), ALF
        DO IONDES =1,NONDES     
          READ(NFO1,*) AHN(IPTFRL,IONDES),PHN(IPTFRL,IONDES)
          AHN(IPTFRL,IONDES)=0.01D0*AHN(IPTFRL,IONDES)
        ENDDO
! 
        DO IONDES =NONDES+1,NSPECTR       
          READ(NFO1,*) AHN(IPTFRL,IONDES),PHN(IPTFRL,IONDES)
          AHN(IPTFRL,IONDES)=0.01D0*AHN(IPTFRL,IONDES)
        ENDDO
!
      ENDDO
!
      IONDES=1
!
! les phases sont calcul�es de mani�re � se recaler en temps par rapport
! au 06 avril 1999 22h (t=0 simu, TU)
    


!  Etape 2 calcul de un, fn, Vn
! ----------------------------------------------------------------
! ----------------------------------------------------------------      
!        Definition de la date
!-----------------------------------------------------------------
! -------------------------------------------------------------------
!     year
      YY=1999
!     month 
      MM=4
! day 
      DAY=8
      HOUR=22
      MINU=0 
      SEC=0
!---------------------------------------------------------------
!         passage en calendrier Julien JD et temps universel Tuniv
!         jour 6 à 22h donne DDdd=6.917
!------------------------------------------------------------------

      DD=DAY+(HOUR*3600.D0+MINU*60.D0+SEC)/(24.D0*3600.D0)
      HT=HOUR+(MINU*60.D0+SEC)/3600.D0


      IF ((MM.EQ.2) .OR.(MM.EQ.1)) THEN
        YY=YY-1
        MM=MM+12
      ENDIF
      Y100=YY/100.D0
      AJUL=INT(Y100)
      AJUL4=AJUL/4.D0
      BJUL=2.D0-AJUL+INT(AJUL4)
!     reutilsation des var y100, ajul4

      Y100=365.25D0*YY 
      AJUL4=30.6001D0*(MM+1)

      JD= INT(Y100)+INT(AJUL4)+DD +1720994.5D0+BJUL
      
      TUNIV=(JD-2415020.5D0)/36525.D0
!-------------------------------------------------------
!           calcul des variables fondamentales des astres
!            tlun, slun, hsun, plun, Nlun, psun
!----------------------------------------------------------
      SLUN=MOD(277.0248D0+481267.8906D0*TUNIV
     &+0.002D0*TUNIV**2.D0,360.D0)
      HSUN=MOD(280.1895D0+36000.7689D0*TUNIV
     &   +0.0003D0*TUNIV**2D0,360.D0)
      TLUN=MOD(15.D0*HT+HSUN-SLUN,360.D0)
      PLUN=MOD(334.3853D0+4069.034D0*TUNIV
     &   -0.0103D0*TUNIV**2.D0,360.D0)
  
      NLUN=MOD(100.8432D0+1934.142D0*TUNIV
     &   -0.0021D0*TUNIV**2.D0,360.D0) 
      PSUN=MOD(281.2209D0+1.7192D0*TUNIV
     &   +0.0005D0*TUNIV**2.D0,360.D0)    
! Calcul des un , facteurs nodaux de phases
!    Sa,Q1,O1,K1,N2,M2,S2,MN4,M4,MS4
!     Sa
      UONDES(1)=0.D0
!     Q1
      UONDES(2)=-10.8D0*SIN(PI*NLUN/180.D0)
!     O1
      UONDES(3)=-10.8D0*SIN(PI*NLUN/180.D0)
!     k1 à changer aussi
      UONDES(4)=11.36D0*SIN(PI*NLUN/180.D0)
!     N2             
      UONDES(5)=2.1D0*SIN(PI*NLUN/180.D0)
!     M2
      UONDES(6)=UONDES(5)
!     S2
      UONDES(7)=0.D0
!     MN4
      UONDES(8)=2.D0*UONDES(5)
!     M4
      UONDES(9)=2.D0*UONDES(6)
!     MS4
      UONDES(10)=2.1D0*SIN(PI*NLUN/180.D0)

!     old waves
!     M3
      UONDES(20)=3.15D0*SIN(PI*NLUN/180.D0)
!     K2 
      UONDES(11)=24.97D0*SIN(PI*NLUN/180.D0)

      UONDES(13)=2.1D0*SIN(PI*NLUN/180.D0)
!     MU2
      UONDES(12)=2.1D0*SIN(PI*NLUN/180.D0)
      UONDES(13)=UONDES(5)
      UONDES(14)=1.43D0*SIN(PI*NLUN/180.D0)
      UONDES(15)=2.1D0*SIN(PI*NLUN/180.D0)

      UONDES(16)=0.D0
      UONDES(17)=0.D0
      UONDES(18)=0.D0
      UONDES(19)=0.D0
      UONDES(21)=6.22D0*SIN(PI*NLUN/180.D0)

! Calcul de VN phase de l 'astre perturbateur
             
            

      IONDES=1
      VONDES (1)=MOD(2.D0*HSUN,360.D0)
      VONDES (2)=MOD(TLUN-2.D0*SLUN+PLUN,360.D0)
      VONDES (3)=MOD(TLUN-SLUN,360.D0)
      VONDES (4)=MOD(TLUN+SLUN,360.D0)
      VONDES (5)=MOD(2.D0*TLUN-SLUN+PLUN,360.D0)
      VONDES (6)=MOD(2.D0*TLUN,360.D0)
      VONDES (7)=MOD(2.D0*TLUN+2.D0*SLUN-2.D0*HSUN,360.D0)
      VONDES (8)=MOD(4.D0*TLUN-SLUN+PLUN,360.D0)
      VONDES (9)=MOD(4.D0*TLUN,360.D0)
      VONDES (10)=MOD(4.D0*TLUN+2.D0*SLUN-2.D0*HSUN,360.D0)
! old 14 waves
!     next line uncommented on 10/06/2014 to avoid a crash
      VONDES (11)=MOD(2.D0*TLUN+2.D0*SLUN,360.D0)
      VONDES (12)=MOD(2.D0*TLUN-4.D0*SLUN+4.D0*HSUN,360.D0)
      VONDES (13)=MOD(2.D0*TLUN-2.D0*SLUN+2.D0*PLUN,360.D0)
      VONDES (14)=MOD(2.D0*TLUN+SLUN-PLUN,360.D0)
      VONDES (15)=MOD(2.D0*TLUN-SLUN+2.D0*HSUN-PLUN,360.D0)
      VONDES (16)=MOD(2.D0*TLUN+2.D0*SLUN-3.D0*HSUN+PLUN,360.D0)
      VONDES (17)=MOD(2.D0*TLUN-SLUN-2.D0*HSUN+PLUN,360.D0)
      VONDES (18)=MOD(TLUN+SLUN-2.D0*HSUN,360.D0)
      VONDES (19)=MOD(2.D0*HSUN,360.D0)
      VONDES (20)=MOD(3.D0*TLUN,360.D0)
      VONDES (21)=MOD(6.D0*TLUN,360.D0)
!     Calcul des fn, facteurs nodaux en amplitudes
      DO IONDES =1,NONDES       
        FONDES (IONDES)=1.D0
      ENDDO
      
      IONDES=1
      
      FONDES(2)=1.009D0+0.187D0*COS(PI*NLUN/180.D0)  
      FONDES(3)=1.009D0+0.187D0*COS(PI*NLUN/180.D0) 
      FONDES(4)=1.006+0.198*COS(PI*NLUN/180.D0)         
      FONDES(5)=1.D0-0.037D0*COS(PI*NLUN/180.D0)
      FONDES(6)=FONDES(5)
!     uncommented on 10/06/2014 to avoid a crash
      FONDES(7)=1.D0
      FONDES(8)=FONDES(6)**2            
      FONDES(9)=FONDES(5)**2
             
      FONDES (9)=1.D0-0.056D0*COS(PI*NLUN/180.D0)
      FONDES(10)=1.0D0-0.037D0*COS(PI*NLUN/180.D0)
      FONDES(11)=1.0D0+0.436D0*COS(PI*NLUN/180.D0)
      FONDES(12)=FONDES(5)
      FONDES(13)=FONDES(5)
      FONDES(14)=1.0D0-0.037D0*COS(PI*NLUN/180.D0)
      FONDES(15)=1.0D0-0.024D0*COS(PI*NLUN/180.D0)
      FONDES(20)=1.D0-0.056D0*COS(PI*NLUN/180.D0)
      FONDES(21)=1.0D0-0.108D0*COS(PI*NLUN/180.D0)
! calcul des dephasages gn-Vn-un
      DO  IPTFRL = 1,NPTFRLM
        DO IONDES=1,NONDES
          PHN(IPTFRL,IONDES) = (PHN(IPTFRL,IONDES)-UONDES(IONDES)
     &      -VONDES(IONDES)) / 360.D0
!        print *,'avt',IONDES, AHN(IPTFRL,IONDES)
        ENDDO
      ENDDO
! ----------------------------------------------------------------------           


!
!            PREMIERE VALEUR SUSPECTE FIDE FL + PAS DE FACTEUR NODAL
!
! 777      FORMAT(16X,F4.2,5X,F5.1)
  777 FORMAT(F9.3,F9.3)
!XC       ENDIF
!
!======================================================
! CALCUL DE LA MAREE: amplitude
!======================================================
!
      IPTFRL = 1
!      PRINT*, NPTFR2
!      PRINT*, SIZE(LIHBOR%R)
      DO 51 K=1,NPTFR2
!       IF(compteur.EQ.NPLAN) THEN
!          DO IPLAN=1,NPLAN
!
!
!         NSURF(IPTFR)=(NPLAN-1)*NPTFR2+IPTFR
!      IF(LIHBOR%R(NSURF(IPTFR)).EQ.KENT) THEN
!
      IF (LIHBOR%I(K).EQ.KENT) THEN
        IONDES=1
        PROF(K)=0.D0
!        DO  IPTFRL = 1,NPTFRLM
        DO IONDES=1,NONDES

!    PROF (K)= amplitude de la marée A au noeud K
!    A=AM2+AS2+AN2+AM4
!    An=Hn*fn*cos(sn*temps-gn+un+Vn)

!        PROF(NSURF(IPTFR))=PROF(NSURF(IPTFR))+AHN(IPTFRL,IONDES)
!     *  *COS(2.D0*PI*(TEMPS/Tondes(IONDES)-PHN(IPTFRL,IONDES)))
!     *  *fondes(IONDES)

          PROF(K)=PROF(K)+AHN(IPTFRL,IONDES)*FONDES(IONDES)
     &    *COS(2.D0*PI*(AT/TONDES(IONDES)-PHN(IPTFRL,IONDES)))
        END DO
!        print *, K,IPTFRL,IONDES,AHN(IPTFRL,1)
!      END DO
!      IF (LIHBOR%I(K).EQ.KENT) THEN
!     ajout du niveau moyen et de la bathy     
!       HBOR(K) = -ZF(NBOR(K)) + NIVM(IPTFRL) + PROF(K)+0.312d0
!       HBOR%R(NSURF(IPTFR)) = -ZF%R(NBOR2%I(NSURF(IPTFR))) + 0.2d0    
!     *  + PROF(NSURF(IPTFR))

        HBOR%R(K) = -ZF%R(NBOR2%I(K)) + 0.2D0 + PROF(K)
!       if (K.eq.130) then       
!       print *,AT,PROF(130)
!       endif 
 
!
!
        IPTFRL=IPTFRL+1
      ENDIF         
51    CONTINUE   
!===================================================================
!     IMPORTANT OPTION:
!     VERTICAL VELOCITIES ARE SET AS HORIZONTAL VELOCITIES
!     THIS IS AN OPTION, OTHERWISE LIWBOL=KSORT (SEE LIMI3D)
!
!     DO IPTFR = 1,NPTFR2
!       IPOIN2 = NBOR2%I(IPTFR)
!       DO IPLAN = 1,NPLAN
!         IBORD = (IPLAN-1)*NPTFR2 + IPTFR
!         LIWBOL%I(IBORD)= LIUBOL%I(IBORD)
!         IF(LIWBOL%I(IBORD).EQ.KENT) WBORL%R(IBORD) = 0.D0
!       ENDDO
!     ENDDO
!
!     TRACERS
!
!     IF(NTRAC.NE.0) THEN
!
!       DO ITRAC = 1,NTRAC
!
!         DO IPTFR = 1,NPTFR2
!           IPOIN2 = NBOR2%I(IPTFR)
!           LITABF%ADR(ITRAC)%P%I(IPOIN2) = KSORT (DOES NOT WORK WITH SEDIMENT)
!           LITABS%ADR(ITRAC)%P%I(IPOIN2) = KSORT
!           DO IPLAN = 1,NPLAN
!             IBORD = (IPLAN-1)*NPTFR2 + IPTFR
!             IF(LITABL%ADR(ITRAC)%P%I(IBORD).EQ.KADH)
!    &           TABORL%ADR(ITRAC)%P%R(IBORD) = 0.D0
!           ENDDO
!         ENDDO
!
!         DO IPOIN2 = 1,NPOIN2
!           IF(LITABF%ADR(ITRAC)%P%I(IPOIN2).EQ.KADH)
!    &                       TABORF%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
!           IF(LITABS%ADR(ITRAC)%P%I(IPOIN2).EQ.KADH)
!    &                       TABORS%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
!         ENDDO
!
!       ENDDO
!
!     ENDIF
!
!
!=======================================================================
!  FOR ALL TIMESTEPS
!=======================================================================
!
!     IF VELOCITY PROFILE OPTION 5: MINIMUM ELEVATION OF EVERY BOUNDARY
!
      YAZMIN=.FALSE.
      DO IFRLIQ=1,NFRLIQ
        ZMIN(IFRLIQ)=1.D99
        IF(PROFVEL(IFRLIQ).EQ.5) YAZMIN=.TRUE.
      ENDDO
      IF(YAZMIN) THEN
        DO K=1,NPTFR2
          IFRLIQ=NUMLIQ%I(K)
          IPOIN2=NBOR2%I(K)
          IF(IFRLIQ.NE.0) THEN
            ZMIN(IFRLIQ)=MIN(ZMIN(IFRLIQ),ZF%R(IPOIN2)+H%R(IPOIN2))
          ENDIF
        ENDDO
        IF(NCSIZE.GT.1) THEN
          DO IFRLIQ=1,NFRLIQ
            ZMIN(IFRLIQ)=P_DMIN(ZMIN(IFRLIQ))
          ENDDO
        ENDIF
      ENDIF
!
!     INITIALISES YADEB
!
      IF(NFRLIQ.GE.1) THEN
        DO K=1,NFRLIQ
          YADEB(K)=0
        ENDDO
      ENDIF
!
      IDEB=0
      ICOT=0
      IVIT=0
!
!     LOOP ON ALL 2D BOUNDARY POINTS
!
      DO K=1,NPTFR2
!
!     PRESCRIBED ELEVATION GIVEN IN STEERING FILE (NCOTE<>0)
!     -------------------------------------------------------
!
      IF(LIHBOR%I(K).EQ.KENT.AND.NCOTE.NE.0) THEN
!
        IPOIN2 = NBOR2%I(K)
        ICOT=NUMLIQ%I(K)
        IF(STA_DIS_CURVES(ICOT).EQ.1) THEN
          HBOR%R(K) = STA_DIS_CUR(ICOT,FLUX_BOUNDARIES(ICOT),
     &                            PTS_CURVES(ICOT),QZ,NFRLIQ,
     &                            ZF%R(IPOIN2)+H%R(IPOIN2))
     &                - ZF%R(IPOIN2)
          HBOR%R(K) = MAX(0.D0,HBOR%R(K))
        ELSEIF(NCOTE.GE.NUMLIQ%I(K)) THEN
          N=IPOIN2
          IF(NCSIZE.GT.1) N=MESH2D%KNOLG%I(N)
          HBOR%R(K) = SL3(ICOT,AT,N,INFOGR)-ZF%R(IPOIN2)
          HBOR%R(K) = MAX(0.D0,HBOR%R(K))
        ELSE
          IF(LNG.EQ.1) WRITE(LU,100) NUMLIQ%I(K)
100       FORMAT(1X,'BORD3D : COTES IMPOSEES EN NOMBRE INSUFFISANT',/,
     &           1X,'         DANS LE FICHIER DES PARAMETRES',/,
     &           1X,'         IL EN FAUT AU MOINS : ',1I6,/,
     &           1X,'         AUTRE POSSIBILITE :',/,
     &           1X,'         FICHIER DES COURBES DE TARAGE MANQUANT')
          IF(LNG.EQ.2) WRITE(LU,101) NUMLIQ%I(K)
101       FORMAT(1X,'BORD3D: MORE PRESCRIBED ELEVATIONS ARE REQUIRED',/,
     &           1X,'        IN THE PARAMETER FILE',/,
     &           1X,'        AT LEAST ',1I6,' MUST BE GIVEN',/,
     &           1X,'        OTHER POSSIBILITY:',/,
     &           1X,'        STAGE-DISCHARGE CURVES FILE MISSING')
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ENDIF
!
      ENDDO
!
!     PRESCRIBED DISCHARGE GIVEN IN STEERING FILE (NDEBIT<>0)
!     --------------------------------------------------------
!
      DO K=1,NPTFR2
!
!     A VELOCITY PROFILE IS SET HERE AND WILL BE CORRECTED LATER
!     TO GET THE CORRECT DISCHARGE (CALL TO DEBIMP3D)
!
      IF(LIUBOL%I(K).EQ.KENT.AND.NDEBIT.NE.0) THEN
!
        IPOIN2 = NBOR2%I(K)
        DO NP=1,NPLAN
          IJK=(NP-1)*NPTFR2+K
          I3D=(NP-1)*NPOIN2+IPOIN2
          IFRLIQ=NUMLIQ%I(K)
          IF(PROFVEL(IFRLIQ).EQ.2) THEN
!           GIVEN BY USER IN BOUNDARY CONDITIONS FILE
            UBORL%R(IJK) = UBOR2D%R(K+NPTFR2)
            VBORL%R(IJK) = VBOR2D%R(K+NPTFR2)
          ELSEIF(PROFVEL(IFRLIQ).EQ.3) THEN
!           NORMAL AND NORM GIVEN BY UBOR IN BOUNDARY CONDITIONS FILE
            UBORL%R(IJK) = -XNEBOR2%R(K)*UBOR2D%R(K+NPTFR2)
            VBORL%R(IJK) = -YNEBOR2%R(K)*UBOR2D%R(K+NPTFR2)
          ELSEIF(PROFVEL(IFRLIQ).EQ.4) THEN
!           NORMAL AND PROPORTIONAL TO SQRT(H)
            UBORL%R(IJK)=-XNEBOR2%R(K) * SQRT(MAX(H%R(IPOIN2),0.D0))
            VBORL%R(IJK)=-YNEBOR2%R(K) * SQRT(MAX(H%R(IPOIN2),0.D0))
          ELSEIF(PROFVEL(IFRLIQ).EQ.5) THEN
!           NORMAL PROFILE IN SQUARE ROOT OF H, BUT VIRTUAL H
!           DEDUCED FROM LOWEST FREE SURFACE OF THE BOUNDARY
            UBORL%R(IJK)=-XNEBOR2%R(K) *
     &                   SQRT(MAX(ZMIN(IFRLIQ)-ZF%R(IPOIN2),0.D0))
            VBORL%R(IJK)=-YNEBOR2%R(K) *
     &                   SQRT(MAX(ZMIN(IFRLIQ)-ZF%R(IPOIN2),0.D0))
          ELSE
!           NORMAL AND NORM 1
            UBORL%R(IJK)=-XNEBOR2%R(K)
            VBORL%R(IJK)=-YNEBOR2%R(K)
          ENDIF
!         NO VELOCITY IF NO WATER
          IF(H%R(IPOIN2).LT.1.D-4) THEN
            UBORL%R(IJK) = 0.D0
            VBORL%R(IJK) = 0.D0
          ENDIF
!         CASE OF A VERTICAL PROFILE
          IF(VERPROVEL(IFRLIQ).NE.1) THEN
            PROFZ=VEL_PROF_Z(IFRLIQ,NBOR2%I(K),
     &                       AT,LT,NP,INFOGR,VERPROVEL(IFRLIQ))
            UBORL%R(IJK) = UBORL%R(IJK)*PROFZ
            VBORL%R(IJK) = VBORL%R(IJK)*PROFZ
          ENDIF
!         U AND V INITIALISED WITH PRESCRIBED VALUES (FOR DEBIMP3D)
!         WILL BE CHANGED AGAIN AFTER DEBIMP3D
          U%R(I3D)=UBORL%R(IJK)
          V%R(I3D)=VBORL%R(IJK)
        ENDDO
!
        YADEB(NUMLIQ%I(K))=1
!
      ENDIF
!
      ENDDO
!
!     PRESCRIBED VELOCITY GIVEN IN STEERING FILE (NVIT<>0)
!     -----------------------------------------------------
!
      DO K=1,NPTFR2
!
!     THIS VELOCITY IS CONSIDERED NORMAL TO THE BOUNDARY
!
      IF(LIUBOL%I(K).EQ.KENTU.AND.NVIT.NE.0) THEN
        IVIT=NUMLIQ%I(K)
        IF(NVIT.GE.IVIT) THEN
          DO NP=1,NPLAN
            IBORD = (NP-1)*NPTFR2+K
            IF(NCSIZE.GT.1) THEN
              N=MESH2D%KNOLG%I(NBOR2%I(K))+(NP-1)*NPOIN2
            ELSE
              N=NBOR3%I(IBORD)
            ENDIF
            UBORL%R(IBORD)=-MESH2D%XNEBOR%R(K)*VIT3(IVIT,AT,N,INFOGR)
            VBORL%R(IBORD)=-MESH2D%YNEBOR%R(K)*VIT3(IVIT,AT,N,INFOGR)
            WBORL%R(IBORD)=0.D0
          ENDDO
        ELSE
          IF(LNG.EQ.1) WRITE(LU,200) NUMLIQ%I(K)
200       FORMAT(1X,'BORD3D : VITESSES IMPOSEES EN NOMBRE INSUFFISANT',
     &           /,1X,'       DANS LE FICHIER DES PARAMETRES',/,
     &           1X,'       IL EN FAUT AU MOINS : ',1I6)
          IF(LNG.EQ.2) WRITE(LU,201) NUMLIQ%I(K)
201       FORMAT(1X,'BORD3D : MORE PRESCRIBED VELOCITIES ARE REQUIRED',
     &           /,1X,'       IN THE PARAMETER FILE',/,
     &           1X,'       AT LEAST ',1I6,' MUST BE GIVEN')
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
      ENDDO
!
!     PRESCRIBED TRACER GIVEN IN STEERING FILE,
!     BUT POSSIBLE OVERWRITING IF LIQUID BOUNDARY FILE IS GIVEN
!     SEE FUNCTION TR3
!     -------------------------------------------------------
!
      IF(NTRAC.GT.0.AND.NTRACER.GT.0) THEN
        DO ITRAC=1,NTRAC
        DO K=1,NPTFR2
        DO NP=1,NPLAN
          IBORD = (NP-1)*NPTFR2+K
          IF(LITABL%ADR(ITRAC)%P%I(IBORD).EQ.KENT) THEN
            IFRLIQ=NUMLIQ%I(K)
            IF(IFRLIQ.EQ.0) THEN
              IF(LNG.EQ.1) WRITE(LU,298) IBORD
298           FORMAT(1X,'BORD3D : VALEURS IMPOSEES DU TRACEUR',/,
     &               1X,'         SUR PAROI SOLIDE',/,
     &               1X,'         AU POINT DE BORD ',1I6)
              IF(LNG.EQ.2) WRITE(LU,299) IBORD
299           FORMAT(1X,'BORD3D: PRESCRIBED TRACER VALUE',/,
     &               1X,'        ON A SOLID BOUNDARY',/,
     &               1X,'        AT BOUNDARY POINT ',1I6)
              CALL PLANTE(1)
              STOP
            ENDIF
            IF(NTRACER.GE.IFRLIQ*NTRAC) THEN
              IF(NCSIZE.GT.1) THEN
                N=MESH2D%KNOLG%I(NBOR2%I(K))+(NP-1)*NPOIN2
              ELSE
                N=NBOR3%I(IBORD)
              ENDIF
              TABORL%ADR(ITRAC)%P%R(IBORD)=
     &                                   TR3(IFRLIQ,ITRAC,N,AT,INFOGR)
            ELSE
              IF(LNG.EQ.1) WRITE(LU,300) NUMLIQ%I(K)*NTRAC
300           FORMAT(1X,'BORD3D : VALEURS IMPOSEES DU TRACEUR',/,
     &               1X,'         EN NOMBRE INSUFFISANT',/,
     &               1X,'         DANS LE FICHIER DES PARAMETRES',/,
     &               1X,'         IL EN FAUT AU MOINS : ',1I6)
              IF(LNG.EQ.2) WRITE(LU,301) NUMLIQ%I(K)
301           FORMAT(1X,'BORD3D: MORE PRESCRIBED TRACER VALUES',/,
     &               1X,'        ARE REQUIRED IN THE PARAMETER FILE',/,
     &               1X,'        AT LEAST ',1I6,' MUST BE GIVEN')
              CALL PLANTE(1)
              STOP
            ENDIF
!           CASE OF A PROFILE ON THE VERTICAL
            IPROF=VERPROTRA(ITRAC+(IFRLIQ-1)*NTRAC)
            IF(IPROF.NE.1) THEN
              PROFZ=TRA_PROF_Z(IFRLIQ,NBOR2%I(K),
     &                         AT,LT,NP,INFOGR,IPROF,ITRAC)
              IF(IPROF.EQ.2.OR.IPROF.EQ.3) THEN
                TABORL%ADR(ITRAC)%P%R(IBORD)=PROFZ
              ELSE
                TABORL%ADR(ITRAC)%P%R(IBORD)=
     &          TABORL%ADR(ITRAC)%P%R(IBORD)*PROFZ
              ENDIF
            ENDIF
          ENDIF
!
        ENDDO
        ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     AUTOMATIC TIDAL BOUNDARY CONDITIONS
!
      IF(TIDALTYPE.GE.1) CALL TIDAL_MODEL_T3D()
!
!-----------------------------------------------------------------------
!
!     PRESCRIBED DISCHARGES: FINAL TREATMENT OF VELOCITIES
!     ----------------------------------------------------
!
!     LOOP ON LIQUID BOUNDARIES
!
      IF(NFRLIQ.NE.0) THEN
      DO 10 IFRLIQ = 1 , NFRLIQ
!
      IF(NDEBIT.NE.0) THEN
!
        MSK1=1
        IF(NDEBIT.GE.IFRLIQ) THEN
          IF(NCSIZE.GT.1) YADEB(IFRLIQ)=P_IMAX(YADEB(IFRLIQ))
           IF(YADEB(IFRLIQ).EQ.1) THEN
           CALL DEBIMP_3D(Q3(IFRLIQ,AT,INFOGR),
     &                    UBORL%R,VBORL%R,WBORL%R,
     &                    U,V,NUMLIQ%I,NUMLIQ_ELM%I,IFRLIQ,T3_02,
     &                    NPTFR2,NETAGE,MASK_3D%ADR(MSK1)%P,
     &                    MESH3D,EQUA,IELM2V,SVIDE,MASKTR,
     &                    MESH3D%NELEB)
           ENDIF
          ELSE
          IF(LNG.EQ.1) WRITE(LU,400) IFRLIQ
400       FORMAT(1X,'BORD3D : DEBITS IMPOSES',/,
     &           1X,'       EN NOMBRE INSUFFISANT',/,
     &           1X,'       DANS LE FICHIER DES PARAMETRES',/,
     &           1X,'       IL EN FAUT AU MOINS : ',1I6)
          IF(LNG.EQ.2) WRITE(LU,401) IFRLIQ
401       FORMAT(1X,'BORD3D : MORE PRESCRIBED FLOWRATES',/,
     &           1X,'       ARE REQUIRED IN THE PARAMETER FILE',/,
     &           1X,'       AT LEAST ',1I6,' MUST BE GIVEN')
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
10    CONTINUE
      ENDIF
!
!     RESETS BOUNDARY CONDITIONS ON U AND V (WILL BE USED BY TFOND
!     AND OTHER SUBROUTINES BEFORE THE NEXT BOUNDARY CONDITIONS TREATMENT)
!
      DO K=1,NPTFR2
        IF(LIUBOL%I(K).EQ.KENT) THEN
          DO NP=1,NPLAN
            IJK=(NP-1)*NPTFR2+K
            U%R((NP-1)*NPOIN2+NBOR2%I(K))=UBORL%R(IJK)
            V%R((NP-1)*NPOIN2+NBOR2%I(K))=VBORL%R(IJK)
          ENDDO
        ENDIF
      ENDDO
!
!     EXAMPLE OF PRESCRIBED VERTICAL VELOCITIES AT ENTRANCES
!     VELOCITIES TANGENT TO BOTTOM AND FREE SURFACE
!
!     DO K=1,NPTFR2
!       IF(LIWBOL%I(K).EQ.KENT.OR.LIWBOL%I(K).EQ.KENTU) THEN
!         DO NP=1,NPLAN
!             IJK=(NP-1)*NPTFR2+K
!             I2D=NBOR2%I(K)
!             I3D=(NP-1)*NPOIN2+I2D
!             WBORL DEDUCED FROM FREE SURFACE AND BOTTOM
!             TETA=(Z(I3D)-Z(I2D))/
!    *        MAX(1.D-3,Z((NPLAN-1)*NPOIN2+I2D)-Z(I2D))
!             GX=        TETA *GRADZN%ADR(1)%P%R(I2D)
!    *            +(1.D0-TETA)*GRADZF%ADR(1)%P%R(I2D)
!             GY=        TETA *GRADZN%ADR(2)%P%R(I2D)
!    *            +(1.D0-TETA)*GRADZF%ADR(2)%P%R(I2D)
!             WBORL%R(IJK)=UBORL%R(IJK)*GX+VBORL%R(IJK)*GY
!         ENDDO
!       ENDIF
!     ENDDO
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
!           END OF AUTOMATIC TREATMENT OF LIQUID BOUNDARIES
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
!
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
!                               WIND
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
      IF(VENT) THEN
        ROEAU = 1000.D0
        ROAIR = 1.3D0
        DO IPOIN2 = 1,NPOIN2
!         RELATIVE WIND
          WINDRELX=WIND%ADR(1)%P%R(IPOIN2)-U%R(NPOIN3-NPOIN2+IPOIN2)
          WINDRELY=WIND%ADR(2)%P%R(IPOIN2)-V%R(NPOIN3-NPOIN2+IPOIN2)
          VITV=SQRT(WINDRELX**2+WINDRELY**2)
!         A MORE ACCURATE TREATMENT
!         IF(VITV.LE.5.D0) THEN
!           FAIR = ROAIR/ROEAU*0.565D-3
!         ELSEIF (VITV.LE.19.22D0) THEN
!           FAIR = ROAIR/ROEAU*(-0.12D0+0.137D0*VITV)*1.D-3
!         ELSE
!           FAIR = ROAIR/ROEAU*2.513D-3
!         ENDIF
!         BEWARE : BUBORS IS VISCVI*DU/DN, NOT DU/DN
          IF(H%R(IPOIN2).GT.HWIND) THEN
!           EXPLICIT PART
            BUBORS%R(IPOIN2) =  FAIR*VITV*WIND%ADR(1)%P%R(IPOIN2)
            BVBORS%R(IPOIN2) =  FAIR*VITV*WIND%ADR(2)%P%R(IPOIN2)
!           IMPLICIT PART
            AUBORS%R(IPOIN2) = -FAIR*VITV
            AVBORS%R(IPOIN2) = -FAIR*VITV
          ELSE
            BUBORS%R(IPOIN2) = 0.D0
            BVBORS%R(IPOIN2) = 0.D0
            AUBORS%R(IPOIN2) = 0.D0
            AVBORS%R(IPOIN2) = 0.D0
          ENDIF
        ENDDO
      ENDIF
!
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
!                         END OF WIND TREATMENT
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
!
!
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
!                     HEAT EXCHANGE WITH ATMOSPHERE
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
!
!                 LINES BELOW WITH 'C!' ARE AN EXAMPLE
!                                              =======
!    TO BE GIVEN :
!
!    ITEMP = NUMBER OF TRACER WHICH IS THE HEAT
!    TAIR  = CONSTANT AIR TEMPERATURE
!    SAL   = CONSTANT WATER SALINITY
!
!!    ITEMP=1
!!    CP=4.18D3
!!    RO0=999.972D0
!!    B=0.0025D0
!!    TAIR=15.D0
!!    SAL=35.D-3
!!    WW=0.D0
!!    IF (VENT) WW=VITV
!!    DO IPOIN2=1,NPOIN2
!!       TREEL=TA%ADR(ITEMP)%P%R(NPOIN3-NPOIN2+IPOIN2)
!!       RO=RO0*(1.D0-(7.D0*(TREEL-4.D0)*(TREEL-4.D0)-750.D0*SAL)*1D-6)
!!       LAMB=RO*CP
!!       A=(4.48D0+0.049D0*TREEL)+2021.5D0*B*(1.D0+WW)*
!!   &     (1.12D0+0.018D0*TREEL+0.00158D0*TREEL*TREEL)
!!       ATABOS%ADR(ITEMP)%P%R(IPOIN2)=-A/LAMB
!!       BTABOS%ADR(ITEMP)%P%R(IPOIN2)= A*TAIR/LAMB
!!    ENDDO
!     IMPORTANT:
!     STATES THAT ATABOS AND BTABOS ARE NOT ZERO (SEE LIMI3D AND DIFF3D)
!     OTHERWISE THEY WILL NOT BE CONSIDERED
!!    ATABOS%ADR(ITEMP)%P%TYPR='Q'
!!    BTABOS%ADR(ITEMP)%P%TYPR='Q'
!
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
!                 END OF HEAT EXCHANGE WITH ATMOSPHERE
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
!
!
!-----------------------------------------------------------------------
!
!     OPTIMISATION:
!
!     EXPLICIT STRESSES WILL NOT BE TREATED IF SAID TO BE 0
!
!     EXPLICIT STRESSES SET TO 0 ON VELOCITIES (UNLESS PROGRAMMED
!                                               IN THIS SUBROUTINE):
!
      BUBORF%TYPR='0'
      BUBORL%TYPR='0'
      BVBORF%TYPR='0'
      BVBORL%TYPR='0'
      BWBORF%TYPR='0'
      BWBORL%TYPR='0'
      BWBORS%TYPR='0'
!
!     CASE OF WIND (SEE ABOVE)
!
      IF(VENT) THEN
        BUBORS%TYPR='Q'
        BVBORS%TYPR='Q'
        AUBORS%TYPR='Q'
        AVBORS%TYPR='Q'
      ELSE
        BUBORS%TYPR='0'
        BVBORS%TYPR='0'
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    *****************
                     SUBROUTINE STRCHE
!                    *****************
!
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE BOTTOM FRICTION COEFFICIENT
!+                IF VARIABLE IN SPACE.
!
!note     IN PARAMETER ESTIMATION WITH A LIST OF TESTS,
!+         THESE VALUES ARE DISCARDED.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER; THIS IS MERELY AN EXAMPLE
!code
!+  COMMENTS CEX MUST BE REMOVED TO IMPLEMENT THE EXAMPLE.
!+  HERE A CONSTANT FRICTION VALUE IS GIVEN:
!+
!+CEX   DO I=1,NPOIN
!+CEX     CHESTR%R(I) = 60.D0
!+CEX   ENDDO
!
!history  J-M HERVOUET (LNH)
!+        01/10/96
!+        V5P2
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
!     DECLARATIONS MUST BE ADAPTED TO EVERY CODE
!     THIS EXAMPLE APPLIES TO TELEMAC2D
!
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
!     HERE A CONSTANT FRICTION VALUE IS GIVEN
!
!EX   DO I=1,NPOIN
!EX     CHESTR%R(I) = 60.D0
!EX   ENDDO
!
      DO I=1,NPOIN2
        IF (Y(I)-X(I).GE.10000.D0) THEN
          CF%R(I) = 70.D0
        ELSEIF (X(I).LE.370000.D0) THEN
          CF%R(I) = 75.D0 
        ELSEIF (X(I).LE.374000.D0.AND.Y(I).GE.282000.D0) THEN
          CF%R(I) = 70.D0
        ELSE
          CF%R(I) = 60.D0
        ENDIF
      ENDDO
!-----------------------------------------------------------------------
!
!     COMMENTS HERE MAY BE CHANGED
!
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'STRCHE (BIEF) : PAS DE MODIFICATION DU FROTTEMENT'
        WRITE(LU,*)
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'STRCHE (BIEF): NO MODIFICATION OF FRICTION'
        WRITE(LU,*)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    *********************
                     SUBROUTINE T3D_CORFON
!                    *********************
!
     &(SZF, ST1, ST2, ZF, T1, T2, X, Y, PRIVE, NPOIN2,
     & LISFON, MSK, MASKEL, MATR2D, MESH2D, S)
!
!***********************************************************************
! TELEMAC3D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    MODIFIES THE BOTTOM TOPOGRAPHY.
!+
!+            STANDARD ACTION: SMOOTHES THE BOTTOM ELEVATION.
!+
!+           (KEYWORD:  'NUMBER OF BOTTOM SMOOTHINGS')
!
!note     EQUIVALENT TO CORFON (BIEF LIBRARY), EXCEPT THAT THIS
!+         SUBROUTINE DISTINGUISHES DATA FROM STRUCTURES.
!
!history  J.M. JANIN  (LNH)
!+        25/11/97
!+        V5P1
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
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
!history  J-M HERVOUET (LNHE)
!+        29/09/2011
!+        V6P2
!+   Name changed into T3D_CORFON to avoid duplication with Telemac-2D
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| LISFON         |-->| NUMBER OF SMOOTHINGS REQUIRED
!| MASKEL         |-->| MASK OF ELEMENTS
!| MATR2D         |<->| WORK MATRIX IN 2DH
!| MESH2D         |<->| 2D MESH
!| MSK            |-->| IF YES, THERE ARE MASKED ELEMENTS
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| PRIVE          |<->| BLOCK OF PRIVATE ARRAYS FOR USER
!| S              |-->| VOID STRUCTURE
!| ST1            |<->| STRUCTURE OF T1
!| ST2            |<->| STRUCTURE OF T2
!| SZF            |<->| STRUCTURE OF ZF
!| T1             |<->| WORK ARRAY
!| T2             |<->| WORK ARRAY
!| X              |-->| MESH COORDINATE
!| Y              |-->| MESH COORDINATE
!| ZF             |<->| ELEVATION OF BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN2, LISFON
      LOGICAL, INTENT(IN) :: MSK
      TYPE (BIEF_OBJ), INTENT(INOUT) :: SZF, ST1, ST2
      DOUBLE PRECISION, DIMENSION(NPOIN2), INTENT(INOUT) :: ZF, T1, T2
      DOUBLE PRECISION, DIMENSION(NPOIN2), INTENT(IN) :: X,Y
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: PRIVE
      TYPE (BIEF_OBJ),  INTENT(IN)    :: MASKEL
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: MATR2D
      TYPE (BIEF_MESH), INTENT(INOUT) :: MESH2D
      TYPE (BIEF_OBJ),  INTENT(IN)    :: S
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL MAS
      INTEGER I
!
!***********************************************************************
!
!     SMOOTHES THE BOTTOM ELEVATION
!
      IF(LISFON.GT.0) THEN
!
        MAS = .TRUE.
!
        CALL FILTER(SZF,MAS,ST1,ST2,MATR2D,'MATMAS          ',
     &              1.D0,S,S,S,S,S,S,MESH2D,MSK,MASKEL,LISFON)
      ENDIF
!
      DO I=1,NPOIN2
        IF (ZF(I).GE.7.D0) ZF(I)=7.D0
      ENDDO
      
      
      DO I=1,NPOIN2
        IF(X(I).GT.417939.D0.AND.X(I).LT.421750.D0.AND.
     &     Y(I).GT.283000.D0.AND.Y(I).LT.284451.D0      ) THEN
          ZF(I)=0.D0
        ENDIF
        IF(X(I).GT.410500.D0.AND.X(I).LT.411750.D0.AND.
     &     Y(I).GT.255750.D0.AND.Y(I).LT.256250.D0      ) THEN
          ZF(I)=0.D0
        ENDIF
      ENDDO     
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    *******************
                     SUBROUTINE OIL_FLOT
!                    *******************
!
     &(PARTICULES,NFLOT,NFLOT_MAX,MESH,LT,VOLDEV,RHO_OIL,NB_COMPO,
     &NB_HAP,FMCOMPO,TBCOMPO,FMHAP,TBHAP,SOLU,ETAL,AREA,NPLAN,GRAV)
!
!***********************************************************************
! TELEMAC2D & TELEMAC3D  V6P3                               21/08/2010
!***********************************************************************
!
!brief    THE USER MUST GIVE :
!+
!+
!+   1) THE TIMESTEP WHEN THE FLOATING BODY IS RELEASED.
!+
!+
!+   2) THE TIME WHEN THE COMPUTATION IS STOPPED FOR THIS FLOATING BODY.
!+
!+
!+   3) THE INITIAL POSITION OF THE FLOATING BODY AT THE TIME OF RELEASE.
!
!history  J-M JANIN (LNH)
!+        17/08/1994
!+        V5P2
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
!history  CEDRIC GOEURY (LHSV)
!+        28/06/2013
!+        V6P3
!+   First version
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ELTFLO         |<->| NUMBERS OF ELEMENTS WHERE ARE THE FLOATS
!| ETAFLO         |<->| LEVELS WHERE ARE THE FLOATS
!| LT             |-->| CURRENT TIME STEP
!| MESH           |<->| MESH STRUCTURE
!| NFLOT          |-->| NUMBER OF FLOATS
!| NFLOT_MAX      |-->| MAXIMUM NUMBER OF FLOATS
!| NIT            |-->| NUMBER OF TIME STEPS
!| NPLAN          |-->| NUMBER OF PLANES
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| SHPFLO         |<->| BARYCENTRIC COORDINATES OF FLOATS IN THEIR 
!|                |   | ELEMENTS.
!| SHZFLO         |<->| BARYCENTRIC COORDINATES OF FLOATS IN THEIR LEVEL
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| Z              |-->| ELEVATIONS OF POINTS IN THE MESH
!| XFLOT          |<->| ABSCISSAE OF FLOATING BODIES
!| YFLOT          |<->| ORDINATES OF FLOATING BODIES
!| ZFLOT          |<->| ELEVATIONS OF FLOATING BODIES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE STREAMLINE, ONLY : ADD_PARTICLE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(INOUT)          :: NFLOT
      INTEGER, INTENT(IN)             :: NFLOT_MAX,LT,NPLAN
      INTEGER, INTENT(IN)             :: NB_COMPO,NB_HAP
      INTEGER, INTENT(IN)             :: ETAL
      DOUBLE PRECISION, INTENT(IN)    :: GRAV
      DOUBLE PRECISION, INTENT(IN)    :: VOLDEV,RHO_OIL,AREA
      DOUBLE PRECISION, INTENT(IN)    :: FMCOMPO(NB_COMPO)
      DOUBLE PRECISION, INTENT(IN)    :: TBCOMPO(NB_COMPO)
      DOUBLE PRECISION, INTENT(IN)    :: FMHAP(NB_HAP)
      DOUBLE PRECISION, INTENT(IN)    :: TBHAP(NB_HAP)
      DOUBLE PRECISION, INTENT(IN)    :: SOLU(NB_HAP)
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(OIL_PART), INTENT(INOUT)   :: PARTICULES(NFLOT_MAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                         :: K,J,NUM_GLO,NUM_LOC,NUM_MAX,I
      INTEGER                         :: NFLOT_OIL
      DOUBLE PRECISION                :: RHO_EAU,PI,COEF1
      DOUBLE PRECISION                :: COEF2,DELTA,NU,NU2
      DOUBLE PRECISION                :: COORD_X, COORD_Y
      DOUBLE PRECISION                :: XFLOT(1), YFLOT(1),ZFLOT(1)
      DOUBLE PRECISION                :: SHPFLO(3,1)
      DOUBLE PRECISION                :: SHZFLO(1)
      INTEGER                         :: TAGFLO(1)
      INTEGER                         :: ELTFLO(1)
      INTEGER                         :: ETAFLO(1)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     THIS IS AN EXAMPLE !!!!!!!!!!!!!!!!!!!!
!
      RHO_EAU=1000.D0
      PI=ACOS(-1.D0)
!     HARDCODED WATER MOLECULAR VISCOSITY
      NU=1.D-6
      NU2=NU**2
!
      COEF1=1.21D0**4
      COEF2=COEF1/1.53**2
      DELTA=(RHO_EAU-RHO_OIL)/(RHO_EAU)
!
      IF(LT.EQ.250) THEN 
        NUM_GLO=0
        NUM_MAX=0
        NUM_LOC=0
        COORD_X=0.D0
        COORD_Y=0.D0 
        NUM_MAX=INT(SQRT(REAL(NFLOT_MAX)))
        DO K=0,NUM_MAX-1
          DO J=0,NUM_MAX-1
            COORD_X=340250.D0+REAL(J)
            COORD_Y=362750.D0+REAL(K)
            NUM_GLO=NUM_GLO+1
            NFLOT_OIL = 0
            IF(MESH%DIM.EQ.3)THEN
              CALL ADD_PARTICLE(COORD_X,COORD_Y,COORD_Y,NUM_GLO,
     &           NFLOT_OIL,1,XFLOT,YFLOT,ZFLOT,TAGFLO,SHPFLO,SHZFLO,
     &           ELTFLO,ETAFLO,MESH,NPLAN,0.D0,0.D0,0.D0,0.D0,0,0)
            ELSEIF(MESH%DIM.EQ.2)THEN
              CALL ADD_PARTICLE(COORD_X,COORD_Y,0.D0,NUM_GLO,
     &           NFLOT_OIL,1,XFLOT,YFLOT,YFLOT,TAGFLO,SHPFLO,SHPFLO,
     &           ELTFLO,ELTFLO,MESH,NPLAN,0.D0,0.D0,0.D0,0.D0,0,0)
            END IF
            IF(NFLOT_OIL.EQ.1)THEN
              NUM_LOC = NUM_LOC+1
!=========================================================================
!----INITIALIZATION PARAMETERS FOR THE CALCULATION OF PARTICULE MOTION----
!=========================================================================
              PARTICULES(NUM_LOC)%XOIL = XFLOT(1)
              PARTICULES(NUM_LOC)%YOIL = YFLOT(1)
              PARTICULES(NUM_LOC)%ID = TAGFLO(1)
              PARTICULES(NUM_LOC)%SHPOIL(1) = SHPFLO(1,1)
              PARTICULES(NUM_LOC)%SHPOIL(2) = SHPFLO(2,1)
              PARTICULES(NUM_LOC)%SHPOIL(3) = SHPFLO(3,1)
              PARTICULES(NUM_LOC)%ELTOIL = ELTFLO(1)
              IF(MESH%DIM.EQ.3)THEN
                PARTICULES(NUM_LOC)%ZOIL = ZFLOT(1)
                PARTICULES(NUM_LOC)%ETAOIL = ETAFLO(1)
                PARTICULES(NUM_LOC)%SHZOIL = SHZFLO(1)
              END IF
!=========================================================================
!-----------INITIALIZATION PARAMETERS FOR THE CALCULATION OF OIL----------
!---------------------------WEATHERING PROCESSES--------------------------
!=========================================================================
              PARTICULES(NUM_LOC)%STATE=1
              PARTICULES(NUM_LOC)%TPSECH=0
              IF(ETAL.EQ.1)THEN
                PARTICULES(NUM_LOC)%SURFACE=PI*COEF2*
     &               (DELTA*GRAV/(VOLDEV*NU2))**(1.D0/6.D0)
     &               *VOLDEV/NFLOT_MAX 
              ELSEIF(ETAL.EQ.3)THEN
                PARTICULES(NUM_LOC)%SURFACE = AREA
              ELSEIF(ETAL.EQ.2) THEN
                PARTICULES(NUM_LOC)%SURFACE = 0.D0
              ELSE
                IF(LNG.EQ.1) THEN
                  WRITE(LU,*) 'ETAL=',ETAL,' INCONNU DANS OIL_FLOT'
                ENDIF
                IF(LNG.EQ.1) THEN
                  WRITE(LU,*) 'ETAL=',ETAL,' UNKNOWN IN OIL_FLOT'
                ENDIF
                CALL PLANTE(1)
                STOP
              END IF
              PARTICULES(NUM_LOC)%MASS0 = (VOLDEV*RHO_OIL)/NFLOT_MAX
              PARTICULES(NUM_LOC)%MASS_EVAP=0.D0
              PARTICULES(NUM_LOC)%MASS_DISS=0.D0
              DO I=1,NB_COMPO
                PARTICULES(NUM_LOC)%COMPO(I)%MASS=
     &               PARTICULES(NUM_LOC)%MASS0*FMCOMPO(I)
                PARTICULES(NUM_LOC)%COMPO(I)%TB=TBCOMPO(I)
                PARTICULES(NUM_LOC)%COMPO(I)%SOL=0.D0
                PARTICULES(NUM_LOC)%MASS=PARTICULES(NUM_LOC)%MASS+
     &               PARTICULES(NUM_LOC)%COMPO(I)%MASS
              END DO
              DO I=1,NB_HAP
                PARTICULES(NUM_LOC)%HAP(I)%MASS=
     &               PARTICULES(NUM_LOC)%MASS0*FMHAP(I)
                PARTICULES(NUM_LOC)%HAP(I)%TB=TBHAP(I)
                 PARTICULES(NUM_LOC)%HAP(I)%SOL=SOLU(I)
                PARTICULES(NUM_LOC)%MASS=PARTICULES(NUM_LOC)%MASS+
     &               PARTICULES(NUM_LOC)%HAP(I)%MASS
              END DO
              NFLOT = NUM_LOC
            END IF
          END DO
        END DO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
