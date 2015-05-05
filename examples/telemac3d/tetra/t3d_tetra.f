!                       *****************
                        SUBROUTINE CONDIM
!                       *****************
!   
!
!***********************************************************************
! TELEMAC 3D VERSION 5.9    23/01/09   J-M HERVOUET(LNHE) 01 30 87 80 18
! FORTRAN95 VERSION         MARCH 1999        JACEK A. JANKOWSKI PINXIT
!
! 20/04/2007 : INITIALISATION OF DPWAVE ADDED
! 23/01/2009 : CHECKING OF ZSTAR ADDED
!
!***********************************************************************
!
!      FONCTION:
!      =========
!
!      INITIALISATION DES TABLEAUX DES GRANDEURS PHYSIQUES
!
!-----------------------------------------------------------------------
!
!      FUNCTION:
!      =========
!
!      INITIALISATION OF VELOCITY, DEPTH AND TRACERS
!
!-----------------------------------------------------------------------
!
! SOUS-PROGRAMME APPELE PAR : TELEMAC-3D
! SOUS-PROGRAMMES APPELES : OV , (CALCOT)
!
!***********************************************************************
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_CONDIM => CONDIM
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      INTEGER IPLAN, I,J
!
!***********************************************************************
!
!     TIME ORIGIN
!
      IF(.NOT.SUIT2) AT  = 0.D0
!
!     INITIALISATION OF H, THE WATER DEPTH
!
      IF(.NOT.SUIT2) THEN
!
      IF(CDTINI(1:10).EQ.'COTE NULLE'.OR.
     &   CDTINI(1:14).EQ.'ZERO ELEVATION') THEN
        CALL OS( 'X=C     ' ,X=H,C=0.D0)
        CALL OV( 'X=X-Y   ' , H%R , Z , Z , 0.D0 , NPOIN2 )
      ELSEIF(CDTINI(1:14).EQ.'COTE CONSTANTE'.OR.
     &       CDTINI(1:18).EQ.'CONSTANT ELEVATION') THEN
        CALL OS( 'X=C     ' ,X=H,C=COTINI)
        CALL OV( 'X=X-Y   ' , H%R , Z , Z , 0.D0 , NPOIN2 )
      ELSEIF(CDTINI(1:13).EQ.'HAUTEUR NULLE'.OR.
     &       CDTINI(1:10).EQ.'ZERO DEPTH') THEN
        CALL OS( 'X=C     ' ,X=H,C=0.D0)
      ELSEIF(CDTINI(1:17).EQ.'HAUTEUR CONSTANTE'.OR.
     &       CDTINI(1:14).EQ.'CONSTANT DEPTH') THEN
        CALL OS( 'X=C     ' ,X=H,C=HAUTIN)
      ELSEIF(CDTINI(1:13).EQ.'PARTICULIERES'.OR.
     &       CDTINI(1:10).EQ.'PARTICULAR'.OR.
     &       CDTINI(1:07).EQ.'SPECIAL') THEN
!     ZONE A MODIFIER
!     FOR SPECIAL INITIAL CONDITIONS ON DEPTH, PROGRAM HERE
        IF(LNG.EQ.1) WRITE(LU,10)                                       
        IF(LNG.EQ.2) WRITE(LU,11)                                       
10      FORMAT(1X,'CONDIM : AVEC DES CONDITIONS INITIALES PARTICULIERES'
     &      ,/,1X,'         VOUS DEVEZ MODIFIER CONDIM')                
11      FORMAT(1X,'CONDIM : WITH SPECIAL INITIAL CONDITIONS'            
     &      ,/,1X,'         YOU HAVE TO MODIFY CONDIM')                 
        CALL PLANTE(1)                                                  
        STOP
!     END OF SPECIAL INITIAL CONDITIONS
!     FIN DE LA ZONE A MODIFIER      
      ELSE
        IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'CONDIM : CONDITION INITIALE NON PREVUE : ',CDTINI
        ENDIF
        IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'CONDIM: INITIAL CONDITION UNKNOWN: ',CDTINI
        ENDIF
        STOP
      ENDIF 
      ELSE
        IF(LNG.EQ.1) WRITE(LU,*) 'HAUTEUR LUE DANS LE FICHIER BINAIRE 1'
        IF(LNG.EQ.2) WRITE(LU,*) 'DEPTH IS READ IN THE BINARY FILE 1'
      ENDIF
!
!     CLIPPING OF H
!
      DO I=1,NPOIN2
        H%R(I)=MAX(H%R(I),0.D0)
      ENDDO
!
      CALL OS ('X=Y     ',X=HN,Y=H)
!
!-----------------------------------------------------------------------
!
!     DATA FOR BUILDING VERTICAL COORDINATES IN CALCOT
!
!     TRANSF IS KEY-WORD "MESH TRANSFORMATION"
!     IF TRANSF = 0, SUBROUTINE CALCOT MUST BE IMPLEMENTED BY THE USER
!
!     POSSIBLE VALUES OF TRANSF_PLANE :
!
!     1 : SIGMA TRANSFORMATION WITH EVENLY SPACED PLANES
!     2 : SIGMA TRANSFORMATION WITH PROPORTIONS GIVEN IN ZSTAR
!     3 : PRESCRIBED ELEVATION GIVEN IN ZPLANE
!
!     STANDARD BELOW IS: EVENLY SPACED PLANES, NO OTHER DATA REQUIRED
!
      DO IPLAN = 1,NPLAN
        TRANSF_PLANE%I(IPLAN)=1
      ENDDO
!
!     EXAMPLE 1: ALL PLANES WITH PRESCRIBED ELEVATION
!
!     DO IPLAN = 1,NPLAN
!       TRANSF_PLANE%I(IPLAN)=3
!     ENDDO
!     ZPLANE%R(2)=-7.D0
!     ZPLANE%R(3)=-4.D0
!     ...
!     ZPLANE%R(NPLAN-1)=-0.05D0
! 
!
!     EXAMPLE 2: SIGMA TRANSFORMATION WITH GIVEN PROPORTIONS
!
!     DO IPLAN = 1,NPLAN
!       TRANSF_PLANE%I(IPLAN)=2
!     ENDDO
!     ZSTAR%R(2)=0.02D0
!     ZSTAR%R(3)=0.1D0
!     ...
!     ZSTAR%R(NPLAN-1)=0.95D0
! 
!
!     EXAMPLE 3: ONE PLANE WITH PRESCRIBED ELEVATION
!                AND 2 SIGMA TRANSFORMATIONS, WITH NPLAN=7
!                SIGMA TRANSFORMATIONS ARE MEANT BETWEEN
!                BOTTOM, FIXED ELEVATION PLANES AND FREE SURFACE
!                THE VALUES OF ZSTAR ARE LOCAL FOR EVERY
!                SIGMA TRANSFORMATION: 0. FOR LOWER FIXED PLANE
!                                      1. FOR UPPER FIXED PLANE
!
      DO IPLAN = 1,NPLAN
        TRANSF_PLANE%I(IPLAN)=1
      ENDDO
      TRANSF_PLANE%I(4)=3
      ZPLANE%R(4)=-0.2D0
!     ZSTAR%R(2)=0.2D0
!     ZSTAR%R(3)=0.8D0
!     ZSTAR%R(5)=0.1D0
!     ZSTAR%R(6)=0.9D0
! 
!
!***********************************************************************
!
!     COMPUTATION OF ELEVATIONS
!     IF IT IS A CONTINUATION, WILL BE DONE AFTER CALLING 'SUITE'
!
      IF(DEBU) CALL CALCOT(Z,H%R)
!
!***********************************************************************
!
!     INITIALISATION OF VELOCITIES
!
      IF(SUIT2) THEN       
        DO I=1,NPLAN
          DO J=1,NPOIN2
           U%R((I-1)*NPOIN2+J)=U2D%R(J)
           V%R((I-1)*NPOIN2+J)=V2D%R(J)
          ENDDO
        ENDDO
      ELSE
        CALL OS( 'X=0     ' , X=U )
        CALL OS( 'X=0     ' , X=V )
      ENDIF
!
      CALL OS( 'X=0     ' , X=W )
!
!-----------------------------------------------------------------------
!
!     TRACERS INITIALIZATION
!
      IF(NTRAC.GT.0) THEN
        DO I=1,NTRAC
          CALL OS( 'X=C     ', X=TA%ADR(I)%P, C=TRAC0(I))
        ENDDO
      ENDIF
!
!
!-----------------------------------------------------------------------
!   INITIALISATION DU MODELE K-EPSILON (FACULTATIF)
!   SI VOUS LE FAITES, INDIQUEZ AKEP = .FALSE.
!
      AKEP=.TRUE.
!
!     IF(ITURBV.EQ.3) THEN
!
!       HERE INITIALISE K AND EPSILON
!
!       AKEP = .FALSE.
!     ENDIF
!
!-----------------------------------------------------------------------
!
! INITIALIZE THE PRESSURE FIELDS TO 0.0
! 
      IF(NONHYD) THEN
        CALL OS('X=C     ',X=DP,C=0.D0)
        WRITE (LU,*) 'CONDIM: DYNAMIC PRESSURE INITIALISED TO ZERO'
        CALL OS('X=C     ',X=PH,C=0.D0)
        WRITE (LU,*) '        HYDROSTATIC PRESSURE INITIALISED TO ZERO.'
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       *****************
                        SUBROUTINE BORD3D
!                       ***************** 
!
     &(TIME,LT,ENTET,NPTFR2_DIM,NFRLIQ)
! 
!***********************************************************************
! TELEMAC 3D VERSION 6.0 20/08/2009 J.-M. HERVOUET (LNHE) 01 30 87 80 18
! 
! 11/02/2008 : BOUCLE SUR LES POINTS DE BORD CASSEE EN 3 BOUCLES POUR
!              INVERSER LA BOUCLE SUR LES TRACEURS ET LA BOUCLE SUR LES
!              POINTS (POUR EVITER DES IMPRESSIONS INTEMPESTIVES
!              DANS READ_FIC_LIQ).
!
! 20/08/2009 : TEST ON IPBOT FOR DIRICHLET VELOCITIES
!        
!***********************************************************************
! 
!      FONCTION: 
!      ========= 
! 
!      CONDITIONS AUX LIMITES SPECIFIQUES. PEUT ETRE MODIFIE PAR
!      L'UTILISATEUR 
!
!-----------------------------------------------------------------------
! 
!      FUNCTION: 
!      ========= 
! 
!      SPECIFIC BOUNDARY CONDITIONS, MAY BE MODIFIED BY THE USER. 
!
!-----------------------------------------------------------------------
!                          SOME USEFUL PARAMETERS
! .________________.____.______________________________________________.
! !  NOM           !MODE!                  ROLE                        !
! !________________!____!______________________________________________! 
! !  UBORF         !<-- ! PRESCRIBED VELOCITY ALONG X ON THE BOTTOM
! !  UBORL         !<-- ! PRESCRIBED VELOCITY ALONG X ON THE LATERAL
! !                !    ! BOUNDARY
! !  UBORS         !<-- ! PRESCRIBED VELOCITY ALONG X AT FREE SURFACE
! !  VBORF         !<-- ! PRESCRIBED VELOCITY ALONG Y ON THE BOTTOM
! !  VBORL         !<-- ! PRESCRIBED VELOCITY ALONG Y ON THE LATERAL
! !                !    ! BOUNDARY
! !  VBORS         !<-- ! PRESCRIBED VELOCITY ALONG Y AT FREE SURFACE
! !  WBORF         !<-- ! PRESCRIBED VELOCITY ALONG Z ON THE BOTTOM
! !  WBORL         !<-- ! PRESCRIBED VELOCITY ALONG Z ON THE LATERAL
! !                !    ! BOUNDARY
! !  WBORS         !<-- ! PRESCRIBED VELOCITY ALONG Z AT FREE SURFACE
! !  TABORF        !<-- ! PRESCRIBED TRACERS ON THE BOTTOM
! !  TABORL        !<-- ! PRESCRIBED TRACERS ON THE LATERAL BOUNDARY
! !  TABORS        !<-- ! PRESCRIBED TRACERS AT FREE SURFACE
! !                !    !
! !                !    ! LOGARITHMIC LAWS : AUBORF,L,S AND BUBORF,L,S
! !                !    !                    FOR VELOCITIES
! !                !    !                    ATABO,L,S AND BTABO,L,S
! !                !    !                    FOR TRACERS
! !                !    ! 
! ! AUBOR,BUBOR    !<-- ! LOG LAW: NU*DU/DN = AUBOR*U + BUBOR
! !                !    ! IF BUBOR(F,L,S) ADDED HERE, SPECIFY BUBOR%TYPR='Q'
! !                !    ! SEE END OF THE ROUTINE
! ! AUBOR,BVBOR    !<-- ! LOG LAW: NU*DV/DN = AVBOR*V + BVBOR
! !                !    ! IF BVBOR(F,L,S) ADDED HERE, SPECIFY BVBOR%TYPR='Q'
! !                !    ! SEE END OF THE ROUTINE
! ! AWBOR,BWBOR    !<-- ! LOG LAW: NU*DW/DN = AWBOR*W + BWBOR
! !                !    ! IF BWBOR(F,L,S) ADDED HERE, SPECIFY BWBOR%TYPR='Q'
! !                !    ! SEE END OF THE ROUTINE
! !                !    ! 
! ! ATABO,BTABO    !<-- ! LOG LAW: NU*DTA/DN = ATABO*TA + BTABO 
! !                !    !
! !                !    ! TYPES OF BOUNDARY CONDITIONS
! !                !    !                    
! !  LIU,V,WBOF    !<-->! ON BOTTOM FOR U,V,W
! !  LIU,V,WBOL    !<-->! ON LATERAL BOUNDARIES FOR U,V,W
! !  LIU,V,WBOS    !<-->! AT FREE SURFACE FOR U,V,W
! !  LITA,BF       !<-->! ON BOTTOM FOR TRACERS
! !  LITA,BL       !<-->! ON LATERAL BOUNDARIES FOR TRACERS
! !  LITA,BS       !<-->! AT FREE SURFACE FOR TRACERS
! !                !    !
! !  U,V,W         ! -->! COMPONENTS OF VELOCITY
! !  UMOY,VMOY     ! -->! DEPTH AVERAGED VELOCITY
! !  TA            ! -->! CONCENTRATION OF TRACERS
! !  ITURBV        ! -->! TURBULENCE MODEL (1:LAMINAR 2: MIXING LENGTH..)
! !  VENT          ! -->! WITH WIND (.TRUE.) OR WITHOUT (.FALSE.)
! !  FAIR          ! -->! DRAG COEFFICIENT OF WIND
! !  VENTX         ! -->! WIND VELOCITY ALONG X
! !  VENTY         ! -->! WIND VELOCITY ALONG Y
! !  AT            ! -->! TIME
! !  LT            ! -->! CURRENT NUMBER OF TIME-STEP
! !  DT            ! -->! TIME-STEP
! !  LIHBOR        ! -->! TYPE OF BOUNDARY CONDITIONS ON DEPTH
! !  HBOR          ! -->! PRESCRIBED DEPTH ON LATERAL BOUNDARIES
! !  HN            ! -->! DEPTH AT TIME TN
! !  X,Y,Z         ! -->! MESH COORDINATES
! !  ZF            ! -->! BOTTOM ELEVATION
! !  NBOR          ! -->! GLOBAL NUMBER OF 2D BOUNDARY POINTS
! !  NELEM3        ! -->! NUMBER OF 3D ELEMENTS
! !  IKLE3         ! -->! CONNECTIVITY TABLE IN 3D
! !  KP1BOR        ! -->! IN 2D : NEXT POINT ON THE BOUNDARY
! !                !    ! (BOUNDARY NUMBERING)
! ! XSGBOR,YSGBOR  ! -->! 2D NORMAL VECTORS TO THE SEGMENTS
! !  NPOIN3        ! -->! NUMBER OF POINTS IN 3D
! !  NPOIN2        ! -->! NUMBER OF POINTS IN 2D
! !  NETAGE        ! -->! NUMBER OF LAYERS OF 3D ELEMENTS
! !  NPTFR         ! -->! NUMBER OF BOUNDARY POINTS IN 2D
! !  NPTFR3        ! -->! NUMBER OF BOUNDARY POINTS IN 3D
! !  NPLAN         ! -->! NUMBER OF PLANES ON THE VERTICAL
! !  NELEM2        ! -->! NUMBER OF ELEMENTS IN 2D
! !                !    ! 
! !                !    ! POSSIBLE TYPES OF BOUNDARY CONDITIONS
! !                !    ! 
! !  KENT          ! -->! PRESCRIBED VALUE
! !  KENTU         ! -->! PRESCRIBED VELOCITY
! !  KSORT         ! -->! FREE (E.G. AT AN OUTPUT)
! !  KADH          ! -->! NO SLIP CONDITION
! !  KLOG          ! -->! SOLID BOUNDARY
! !  NTRAC         ! -->! NUMBER OF TRACERS
! !  SEDI          ! -->! IF YES, THERE IS SEDIMENT
! !  PRIVE         ! -->! BLOCK OF ARRAYS FOR THE USER 
! !  NPRIV         ! -->! NUMBER OF ARRAYS IN BLOCK PRIVE
! !  NDEBIT        ! -->! NUMBER OF BOUNDARIES WITH PRESCRIBED DISCHARGE
! !  NVIT          ! -->! NUMBER OF BOUNDARIES WITH PRESCRIBED VELOCITY
! !  NCOTE         ! -->! NUMBER OF BOUNDARIES WITH PRESCRIBED ELEVATION
! !  DEBIMP        ! -->! ARRAY OF PRESCRIBED DISCHARGES
! !  COTIMP        ! -->! ARRAY OF PRESCRIBED ELEVATIONS
! !  VITIMP        ! -->! ARRAY OF PRESCRIBED VELOCITIES
! !________________!____!______________________________________________!
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE) 
! 
!*********************************************************************** 
!                                                                       
! INFORMATION : FOR PRESCRIBED BOUNDARIES OF POINT BEING BOTH LATERAL
!               AND BOTTOM, USE LATERAL ARRAYS. 
!
!               FOR TYPES OF BOUNDARY CONDITIONS : USE SUBROUTINE LIMI3D
!
!               SEDIMENT IS THE LAST TRACER              
!                                                                      
!***********************************************************************
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
      INTEGER I,IPOIN2,NP,K1,IBORD,IVIT,ICOT,IDEB,IFRLIQ,IPROF,K,N,R
      INTEGER IPTFR,ITRAC,IPLAN,I3D
      DOUBLE PRECISION ROEAU,ROAIR,VITV,PROFZ,WINDRELX,WINDRELY     
!
      INTEGER  P_IMAX
      EXTERNAL P_IMAX
      DOUBLE PRECISION STA_DIS_CUR
      EXTERNAL STA_DIS_CUR
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION XB,YB,ZB,NORM,CP,RO0,A,B,SAL,WW,TREEL,RO,LAMB
!
      INTEGER YADEB(100),MSK1,IPTFR2,I2,IJK,ITEMP
!
!     SIMPLE CASES FOR LATERAL BOUNDARIES ARE TREATED AUTOMATICALLY:
!
!     - PRESCRIBED DEPTH     (5 4 4)
!     - PRESCRIBED VELOCITY  (  6 6)
!     - PRESCRIBED DISCHARGE (  5 5)
!
!     CORRESPONDING KEY-WORDS ARE:
!
!     'PRESCRIBED ELEVATIONS' OR 'COTES IMPOSEES'
!     'PRESCRIBED VELOCITIES' OR 'VITESSES IMPOSEES'
!     'PRESCRIBED FLOWRATES' OR 'DEBITS IMPOSES'
!
!     THE IMPLEMENTATION OF AUTOMATIC CASES MAY BE CANCELLED
!     PROVIDED THAT THE  RELEVANT ARRAYS ARE FILLED
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
!     SECURING NO SLIP BOUNDARY CONDITIONS
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
!
!     IMPORTANT OPTION:
!     VERTICAL VELOCITIES SET AS HORIZONTAL VELOCITIES 
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
      ENDIF
!
!=======================================================================
!
!
!  FOR ALL TIME STEPS
!
!     INITIALISING YADEB
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
!     LOOPS ON ALL 2D BOUNDARY POINTS
!
      DO K=1,NPTFR2
!
!     PRESCRIBED ELEVATION GIVEN IN PARAMETER FILE (NCOTE<>0)
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
          HBOR%R(K) = SL3(ICOT,AT,IPOIN2,INFOGR)-ZF%R(IPOIN2)
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
!     PRESCRIBED DISCHARGE GIVEN IN PARAMETER FILE (NDEBIT<>0)
!     --------------------------------------------------------
!
      DO K=1,NPTFR2
!
!     A VELOCITY PROFILE IS SET HERE AND WILL BE CORRECTED AFTER
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
!     PRESCRIBED VELOCITY GIVEN IN PARAMETER FILE (NVIT<>0)
!     -----------------------------------------------------
!
      DO K=1,NPTFR2
!
!     THIS VELOCITY IS CONSIDERED NORMAL TO THE BOUNDARY
!
      IF(LIUBOL%I(K).EQ.KENTU.AND.NVIT.NE.0) THEN
        IVIT=NUMLIQ%I(K)
        IF(NVIT.GE.IVIT) THEN
!
          DO NP=1,NPLAN
            IBORD = (NP-1)*NPTFR2+K
            UBORL%R(IBORD) =
     &      -MESH2D%XNEBOR%R(K)*VIT3(IVIT,AT,NBOR2%I(K),INFOGR)
            VBORL%R(IBORD) =
     &      -MESH2D%YNEBOR%R(K)*VIT3(IVIT,AT,NBOR2%I(K),INFOGR)
            WBORL%R(IBORD)=0.D0
          END DO
!
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
!     PRESCRIBED TRACER GIVEN IN PARAMETER FILE, BUT POSSIBLE
!     OVERWRITING IF LIQUID BOUNDARY FILE IS GIVEN
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
              TABORL%ADR(ITRAC)%P%R(IBORD) = 
     &        TR3(IFRLIQ,ITRAC,NBOR3%I(IBORD),AT,INFOGR)
     
              IF(NP.LE.4) THEN
                TABORL%ADR(ITRAC)%P%R(IBORD)=40.D0
              ELSE
                TABORL%ADR(ITRAC)%P%R(IBORD)=30.D0
              ENDIF 
     
     
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
!     PRESCRIBED DISCHARGES: FINAL TREATMENT OF VELOCITIES
!     ----------------------------------------------------
!
!     LOOP ON LIQUID BOUNDARIES
!
      IF(NFRLIQ.NE.0) THEN
      DO IFRLIQ = 1 , NFRLIQ
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
      ENDDO
      ENDIF
!
!     RESETTING BOUNDARY CONDITIONS ON U AND V (WILL BE USED BY TFOND
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
!                 LINES BELOW WITH '!C' ARE AN EXAMPLE
! 
!    TO BE GIVEN :
! 
!    ITEMP = NUMBER OF TRACER WHICH IS THE HEAT 
!    TAIR  = CONSTANT AIR TEMPERATURE
!    SAL   = CONSTANT WATER SALINITY 
! 
!C    ITEMP=1 
!C    CP=4.18D3 
!C    RO0=999.972D0 
!C    B=0.0025D0 
!C    TAIR=15.D0 
!C    SAL=35.D-3 
!C    WW=0.D0 
!C    IF (VENT) WW=VITV
!C    DO IPOIN2=1,NPOIN2 
!C       TREEL=TA%ADR(ITEMP)%P%R(NPOIN3-NPOIN2+IPOIN2) 
!C       RO=RO0*(1.D0-(7.D0*(TREEL-4.D0)*(TREEL-4.D0)-750.D0*SAL)*1D-6) 
!C       LAMB=RO*CP 
!C       A=(4.48D0+0.049D0*TREEL)+2021.5D0*B*(1.D0+WW)* 
!C   &     (1.12D0+0.018D0*TREEL+0.00158D0*TREEL*TREEL) 
!C       ATABOS%ADR(ITEMP)%P%R(IPOIN2)=-A/LAMB 
!C       BTABOS%ADR(ITEMP)%P%R(IPOIN2)= A*TAIR/LAMB
!C    ENDDO
!     IMPORTANT:
!     STATING THAT ATABOS AND BTABOS ARE NOT ZERO (SEE LIMI3D AND DIFF3D)
!     OTHERWISE THEY WILL NOT BE CONSIDERED
!C    ATABOS%ADR(ITEMP)%P%TYPR='Q'
!C    BTABOS%ADR(ITEMP)%P%TYPR='Q'
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
!     OPTIMIZATION:
!
!     EXPLICIT STRESSES WILL NOT BE TREATED IF SAID TO BE ZERO
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
!                       *****************
                        SUBROUTINE CALCOT
!                       *****************
!
     &(ZZ,HH)
!
!***********************************************************************
! TELEMAC 3D VERSION 6.0  11/03/2010  J-M HERVOUET (LNHE) 01 30 87 80 18
!                                       F LEPEINTRE (LNH) 30 87 78 54
!                                       J-M JANIN   (LNH) 30 87 72 84
! FORTRAN95 VERSION         MARCH 1999         JACEK A. JANKOWSKI PINXIT
!***********************************************************************
!
!      FONCTION:
!      =========
!
!    CONSTRUCTION DU TABLEAU DES COTES DU MAILLAGE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! !  NOM           !MODE!                  ROLE                        !
! !________________!____!______________________________________________!
! !  ZZ            !<-- ! COTES DES POINTS DU MAILLAGE
! !  HH            ! -->! HAUTEURS D'EAU
!
!    OTHER USEFUL VARIABLES
!
! !  ZSTAR%R       ! -->! POSITION RELATIVE DES PLANS QUASI HORIZONTAUX
! !  NPOIN2        ! -->! NOMBRE DE POINTS 2D
! !  NPLAN         ! -->! NOMBRE DE PLANS HORIZONTAUX
! !  HMIN          ! -->! 
! !  COTINT        ! -->! COTE DU PLAN INTERMEDIAIRE DE REFERENCE
! !  TRANSF        ! -->! CHOICE OF MESH TRANSFORMATION
! !________________!____!______________________________________________!
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
! SOUS-PROGRAMME APPELE PAR : MITRID
!
!***********************************************************************
!
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION, INTENT(IN)    :: HH(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: ZZ(NPOIN2,NPLAN)
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION RPLS,RPLI,ZFP,ZSP,DISBOT,DISSUR
      DOUBLE PRECISION DISMIN_BOT,DISMIN_SUR
      INTEGER IPOIN,IPLAN,I1,I2
!
!***********************************************************************
!
      DISMIN_SUR = 0.2D0
      DISMIN_BOT = 0.D0
!
!     1) IN ALL CASES: FREE SURFACE=BOTTOM+DEPTH
!
      IF(OPTBAN.EQ.1.AND.OPT_HNEG.NE.2) THEN
        DO IPOIN = 1,NPOIN2
          ZZ(IPOIN,NPLAN) = ZZ(IPOIN,1) + MAX(HH(IPOIN),0.D0)
        ENDDO
      ELSE
        DO IPOIN = 1,NPOIN2
          ZZ(IPOIN,NPLAN) = ZZ(IPOIN,1) + HH(IPOIN)
        ENDDO
      ENDIF
!
!     HERE IMPLEMENTATION BY USER 
!
      IF(TRANSF.EQ.0) THEN
!
        IF(LNG.EQ.1) WRITE(LU,81)
        IF(LNG.EQ.2) WRITE(LU,82)
81      FORMAT('CALCOT: TRANSFORMATION A PROGRAMMER PAR L''UTILISATEUR')
82      FORMAT('CALCOT: TRANSFORMATION TO BE PROGRAMMED BY USER')
        CALL PLANTE(1)
        STOP
!
!-----------------------------------------------------------------------
!
!     NOW ALL OTHER CASES: SEQUENCES OF SIGMA TRANSFORMATIONS
!                          AND PLANES WITH PRESCRIBED ELEVATION
!
      ELSEIF(NPLAN.GT.2) THEN
!
!-----------------------------------------------------------------------
!
!       2) SETTING THE PLANES WITH PRESCRIBED ELEVATION
!
        DO IPLAN=2,NPLAN-1
          IF(TRANSF_PLANE%I(IPLAN).EQ.3) THEN     
!           IF NOT POSSIBLE BECAUSE OF FREE SURFACE OR BOTTOM, A SECURITY
!           DISTANCE, DISMIN, IS USED. ALL PLANES THAT WOULD CROSS E.G.
!           THE BOTTOM AVOID IT AT A DISTANCE DISMIN*RPLI, SEE RPLI BELOW
            RPLS = DBLE(NPLAN-IPLAN) / DBLE(NPLAN)
            RPLI = DBLE(IPLAN-    1) / DBLE(NPLAN)
            DO IPOIN = 1,NPOIN2
              ZFP = ZZ(IPOIN,1)
              ZSP = ZZ(IPOIN,NPLAN)
              DISBOT = MIN(ZSP-ZFP,DISMIN_BOT)
              DISSUR = MIN(ZSP-ZFP,DISMIN_SUR)
              ZZ(IPOIN,IPLAN)=MIN(                    ZSP-DISSUR*RPLS,
     &                            MAX(ZPLANE%R(IPLAN),ZFP+DISBOT*RPLI))
            ENDDO
          ENDIF
        ENDDO
!
!       3) SETTING THE PLANES WITH SIGMA TRANSFORMATION
!
        I1=2
        DO WHILE(I1.NE.NPLAN)
          IF(TRANSF_PLANE%I(I1).EQ.3) THEN
            I1=I1+1
          ELSE
!           LOOKING FOR SEQUENCES OF SIGMA TRANSFORMATION PLANES
            I2=I1
            DO WHILE(TRANSF_PLANE%I(I2+1).NE.3.AND.I2+1.NE.NPLAN)
              I2=I2+1
            ENDDO
!           SIGMA TRANSFORMATION FOR PLANES I1 TO I2
!           BETWEEN ALREADY TREATED PLANES I1-1 AND I2+1
            DO IPLAN=I1,I2
              IF(TRANSF_PLANE%I(IPLAN).EQ.1) THEN
                ZSTAR%R(IPLAN)=FLOAT(IPLAN-I1+1)/FLOAT(I2-I1+2)
!             ELSE
!               ZSTAR%R(IPLAN) HAS BEEN GIVEN BY USER IN CONDIM
              ENDIF 
              DO IPOIN = 1,NPOIN2
                ZZ(IPOIN,IPLAN) = ZZ(IPOIN,I1-1)
     &                          + ZSTAR%R(IPLAN)*(  ZZ(IPOIN,I2+1)
     &                                             -ZZ(IPOIN,I1-1) )
              ENDDO
            ENDDO
            I1=I2+1    
          ENDIF
        ENDDO
!
!       4) CHECKING
!
        IF(NPLAN.GT.2) THEN
          DO IPLAN=2,NPLAN-1
            DO IPOIN = 1,NPOIN2
              IF(ZZ(IPOIN,IPLAN).LT.ZZ(IPOIN,IPLAN-1)) THEN
                IF(LNG.EQ.1) THEN
                  WRITE(LU,*) 'CALCOT : LES PLANS ',IPLAN-1,' ET ',IPLAN
                  WRITE(LU,*) '         SE CROISENT AU POINT ',IPOIN
                  WRITE(LU,*) '         COTE BASSE : ',ZZ(IPOIN,IPLAN-1)
                  WRITE(LU,*) '         COTE HAUTE : ',ZZ(IPOIN,IPLAN)
                  WRITE(LU,*) '         DIFFERENCE : ',ZZ(IPOIN,IPLAN)-
     &                                                 ZZ(IPOIN,IPLAN-1)
                  WRITE(LU,*) '         HAUTEUR    : ',HH(IPOIN)
                ENDIF
                 IF(LNG.EQ.2) THEN
                  WRITE(LU,*) 'CALCOT: PLANES ',IPLAN-1,' AND ',IPLAN
                  WRITE(LU,*) '        INTERCROSS AT POINT ',IPOIN
                  WRITE(LU,*) '        LOWER POINT : ',ZZ(IPOIN,IPLAN-1)
                  WRITE(LU,*) '        HIGHER POINT: ',ZZ(IPOIN,IPLAN)
                  WRITE(LU,*) '        DIFFERENCE  : ',ZZ(IPOIN,IPLAN)-
     &                                                 ZZ(IPOIN,IPLAN-1)
                  WRITE(LU,*) '        DEPTH       : ',HH(IPOIN)
                ENDIF
                CALL PLANTE(1)
                STOP
              ENDIF
            ENDDO
          ENDDO
        ENDIF
!
!-----------------------------------------------------------------------
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       *********************
                        SUBROUTINE T3D_CORFON
!                       *********************
!
     &(SZF, ST1, ST2, ZF, T1, T2, X, Y, PRIVE, NPOIN2,
     & LISFON, MSK, MASKEL, MATR2D, MESH2D, S)
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
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
      INTEGER K,I
      LOGICAL MAS
!
!-----------------------------------------------------------------------
!
      DO I=1,NPOIN2
        ZF(I) = MAX(-0.3D0,-0.0246875D0*(X(I)-10.D0)**2)
        IF(X(I).LT.2.5D0) THEN
          ZF(I)=MAX(-0.2D0-0.3D0*(X(I)/2.5D0)**2,-0.3D0)
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      IF(LISFON.GT.0) THEN
!
        MAS = .TRUE.
!
        CALL FILTER(SZF,MAS,ST1,ST2,MATR2D,'MATMAS          ',
     &              1.D0,S,S,S,S,S,S,MESH2D,MSK,MASKEL,LISFON)
!
      ENDIF
! 
      RETURN
      END  
!                    ************************
                     PROGRAM HOMERE_TELEMAC3D
!                    ************************
!
!
!***********************************************************************
! TELEMAC3D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    1) OPENS FILES, SETS POINTERS ACCORDING TO THE
!+                   PARAMETERS IMPOSED IN THE STEERING FILE AND
!+                   THE GIVEN GEOMETRY.
!+
!+            2) CALLS THE MAIN SUBROUTINE.
!+
!+            3) MEASURES CPU TIME.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/1999
!+
!+   FORTRAN95 VERSION
!
!history
!+        10/04/2009
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D
      USE DECLARATIONS_TELEMAC, ONLY : COUPLING
      USE DECLARATIONS_SISYPHE, ONLY : SIS_FILES,MAXLU_SIS
      USE DECLARATIONS_TOMAWAC, ONLY : WAC_FILES,MAXLU_WAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER     LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER TDEB(8),TFIN(8),NCAR,IFLOT
!
      CHARACTER(LEN=24), PARAMETER :: CODE1='TELEMAC3D               '
      CHARACTER(LEN=24), PARAMETER :: CODE2='SISYPHE                 '
      CHARACTER(LEN=24), PARAMETER :: CODE3='TOMAWAC                 '
!
      CHARACTER(LEN=250) PATH
      CHARACTER(LEN=144) MOTCAR(300),FILE_DESC(4,300)
!
!======================================================================
!
! STARTS COUNTING CPU TIME
!
      CALL DATE_AND_TIME(VALUES=TDEB)
!
! INITIALISES FILES (ESPECIALLY IMPORTANT FOR A PARALLEL MACHINE)
!
      CALL BIEF_INIT(CODE1,PATH,NCAR,.TRUE.)
!
! WRITES A BANNER TO THE LISTING
!
      IF(LNG.EQ.1) WRITE(LU,100)
      IF(LNG.EQ.2) WRITE(LU,101)
      WRITE(LU,102)
100   FORMAT(///,78('-'),/,1X,'LISTING DE TELEMAC-3D ',/)
101   FORMAT(///,78('-'),/,1X,'LISTING OF TELEMAC-3D ',/)
102   FORMAT(/////,
     &14X,'   TTTTT  EEEEE  L      EEEEE  M   M  AAAAA  CCCCC',/,
     &14X,'     T    E      L      E      MM MM  A   A  C    ',/,
     &14X,'     T    EEE    L      EEE    M M M  AAAAA  C    ',/,
     &14X,'     T    E      L      E      M   M  A   A  C    ',/,
     &14X,'     T    EEEEE  LLLLL  EEEEE  M   M  A   A  CCCCC',/,
     &14X,'                                                  ',/,
     &14X,'            3D   VERSION 7.0   FORTRAN 90    ',/,
     &14X,/////)
!
!-----------------------------------------------------------------------
! READS THE STEERING FILE
!
      CALL LECDON_TELEMAC3D(MOTCAR,FILE_DESC,PATH,NCAR)
!
!-----------------------------------------------------------------------
! OPENS THE FILES
!
      IFLOT = 0
      CALL BIEF_OPEN_FILES(CODE1,T3D_FILES,MAXLU_T3D,PATH,NCAR,
     &                     INCLUS(COUPLING,'SISYPHE').OR.
     &                     INCLUS(COUPLING,'TOMAWAC') ,IFLOT,1)
!
!-----------------------------------------------------------------------
!
! ALLOCATES VECTORS, MATRICES AND BLOCKS
!
      CALL POINT_TELEMAC3D
!
!-----------------------------------------------------------------------
!
! INITIALISES SISYPHE IF COUPLING THE 2 MODELS
!
      IF(INCLUS(COUPLING,'SISYPHE')) THEN
!
        IF(LNG.EQ.1) WRITE(LU,103)
        IF(LNG.EQ.2) WRITE(LU,104)
        WRITE(LU,105)
103     FORMAT(/////,1X,'LISTING DE SISYPHE AVEC COUPLAGE',78('-'))
104     FORMAT(/////,1X,'LISTING OF SISYPHE WITH COUPLING',78('-'))
105     FORMAT(/////,
     &  14X,'    SSSS I   SSSS Y   Y PPPP  H   H EEEEE' ,/,
     &  14X,'   S     I  S      Y Y  P   P H   H E    ' ,/,
     &  14X,'    SSS  I   SSS    Y   PPPP  HHHHH EEEE  ',/,
     &  14X,'       S I      S   Y   P     H   H E     ',/,
     &  14X,'   SSSS  I  SSSS    Y   P     H   H EEEEE' ,/,
     &  14X,'                                          ',/,
     &  14X,'                VERSION 7.0               ',/,
     &  14X,'      COUPLED WITH TELEMAC-3D INTERNALLY  ',/,
     &  14X,/////)
!
      CALL LECDON_SISYPHE(MOTCAR,FILE_DESC,PATH,NCAR,CODE1)
      CALL BIEF_OPEN_FILES(CODE2,SIS_FILES,MAXLU_SIS,PATH,NCAR,
     &                     INCLUS(COUPLING,'SISYPHE'),IFLOT,2)
      CALL CONFIG_CODE(1)
      CALL POINT_SISYPHE
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     INITIALISES TOMAWAC
!
      IF(INCLUS(COUPLING,'TOMAWAC')) THEN
!
        WRITE(LU,106)
        WRITE(LU,107)
106     FORMAT(100('-'),////////,
     &  16X,
     &  'TTTTT  OOOOO  M   M  AAAAA  W   W  AAAAA  CCCCC '
     &  ,/,16X,
     &  '  T    O   O  MM MM  A   A  W   W  A   A  C     '
     &  ,/,16X,
     &  '  T    O   O  M W M  AAAAA  W W W  AAAAA  C     '
     &  ,/,16X,
     &  '  T    O   O  M   M  A   A  WW WW  A   A  C     '
     &  ,/,16X,
     &  '  T    OOOOO  M   M  A   A  W   W  A   A  CCCCC '
     &  ,//)
107     FORMAT(15X,
     &  '               |    |    |                 '
     &  ,/,15X,
     &  '              )_)  )_)  )_) _              '
     &  ,/,15X,
     &  '             )___))___))___)\              '
     &  ,/,15X,
     &  '             )____)____)_____)\\           '
     &  ,/,15X,
     &  '           _____|____|____|____\\\__       '
     &  ,/,15X,
     &  '  ---------\               7.0  /---------  '
     &  ,/,15X,
     & '    ^^^^^^^^^^^^^^^^^^^^^^^^^^^             '
     &  ,/,15X,
     &  '         ^^^^      ^^^^     ^^^    ^^      '
     &  ,/,15X,
     &  '             ^^^^      ^^^                 '
     &,///)
!
      CALL LECDON_TOMAWAC(FILE_DESC,PATH,NCAR,CODE3)
      CALL BIEF_OPEN_FILES(CODE3,WAC_FILES,MAXLU_WAC,PATH,NCAR,
     &                     .TRUE.,IFLOT,3)
      CALL CONFIG_CODE(1)
      CALL POINT_TOMAWAC
!
      ENDIF
!
!=======================================================================
!
      CALL TELEMAC3D
!
!-----------------------------------------------------------------------
!
      CALL BIEF_CLOSE_FILES(CODE1,T3D_FILES,MAXLU_T3D,.TRUE.)
!
      IF(INCLUS(COUPLING,'SISYPHE')) THEN
        CALL CONFIG_CODE(2)
        CALL BIEF_CLOSE_FILES(CODE2,SIS_FILES,MAXLU_SIS,.FALSE.)
      ENDIF
!
      IF(INCLUS(COUPLING,'TOMAWAC')) THEN
        CALL CONFIG_CODE(3)
        CALL BIEF_CLOSE_FILES(CODE3,WAC_FILES,MAXLU_WAC,.FALSE.)
      ENDIF
!
!-----------------------------------------------------------------------
! HOPEFULLY GOOD NEWS
!
      IF(LNG.EQ.1) WRITE(LU,10)
      IF(LNG.EQ.2) WRITE(LU,11)
10    FORMAT(1X,///,1X,'FIN NORMALE DU PROGRAMME',///)
11    FORMAT(1X,///,1X,'CORRECT END OF RUN',///)
!
!-----------------------------------------------------------------------
! PRINTS THE CPU TIME CONSUMED
!
      CALL DATE_AND_TIME(VALUES=TFIN)
      CALL ELAPSE(TDEB,TFIN)
!
!-----------------------------------------------------------------------
!
      STOP
      END
