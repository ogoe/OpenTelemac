!                       *****************
                        SUBROUTINE QMOUT2
!                       *****************
!
     *( TSTOT , TSDER , F     , XK    , ENRJ  , FREQ  , FMOY  , XKMOY , 
     *  USOLD , USNEW , DEPTH , PROINF, CMOUT3, CMOUT4, CMOUT5, CMOUT6,
     *  GRAVIT, NF    , NPLAN , NPOIN2, CIMPLI, TAUX1 , BETA  , BETAO ,
     *  BETAN , BETOTO, BETOTN)
!
!**********************************************************************
! TOMAWAC   V6P1                                   23/06/2011
!**********************************************************************
!
!brief   COMPUTES THE CONTRIBUTION OF THE WHITECAPPING SINK TERM USING
!+          THE  PARAMETRISATION of VAN DER WESTHUYSEN (2007).
!
!reference    VAN DER WESTHUYSEN (2007): ADVANCES IN THE SPECTRAL 
!+              MODELLING OF WIND WAVES IN THE NEARSHORE, PHD THESID, 
!+              DELFT UNIVERSITY OF TECHNOLOGY
!
!history  E. GAGNAIRE-RENOU (EDF/LNHE)
!+        09/2010
!+        V6P0
!
!history  G.MATTAROLO (EDF - LNHE)
!+        23/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BETA           |<->| WORK TABLE
!| BETAN          |<->| WORK TABLE
!| BETAO          |<->| WORK TABLE
!| BETOTN         |<->| WORK TABLE
!| BETOTO         |<->| WORK TABLE
!| CIMPLI         |-->| IMPLICITATION COEFFICIENT FOR SOURCE TERM INTEG.
!| CMOUT3         |-->| WESTHUYSEN WHITE CAPPING DISSIPATION COEFFICIENT
!| CMOUT4         |-->| WESTHUYSEN SATURATION THRES. FOR THE DISSIPATION
!| CMOUT5         |-->| WESTHUYSEN WHITE CAPPING DISSIPATION COEFFICIENT
!| CMOUT6         |-->| WESTHUYSEN WHITE CAPPING WEIGHTING COEFFICIENT
!| DEPTH          |-->| WATER DEPTH
!| ENRJ           |-->| SPECTRUM VARIANCE
!| F              |-->| DIRECTIONAL SPECTRUM
!| FMOY           |-->| MEAN SPECTRAL FRQUENCY FMOY
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| GRAVIT         |-->| GRAVITY ACCELERATION
!| XK             |-->| DISCRETIZED WAVE NUMBER
!| XKMOY          |-->| AVERAGE WAVE NUMBER
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| PROINF         |-->| LOGICAL INDICATING INFINITE DEPTH ASSUMPTION
!| TAUX1          |<->| WORK TABLE
!| TSDER          |<->| DERIVED PART OF THE SOURCE TERM CONTRIBUTION
!| TSTOT          |-->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| USNEW          |-->| FRICTION VELOCITY AT TIME N+1
!| USOLD          |<->| FRICTION VELOCITY AT TIME N
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!  APPELS :    - PROGRAMME(S) APPELANT  : SEMIMP
!  ********    - PROGRAMME(S) APPELE(S) :    -
!
!  REMARCS :
!  ***********
!  - THE CONSTANT CMOUT3 (Cdis,break) UTILISED IN WESTHUYSEN (2007)
!                                    IS EQUAL TO 5.0*10^(-5)
!  - THE CONSTANT CMOUT4 (Br) UTILISED IN WESTHUYSEN (2007)
!                                    IS EQUAL TO 1.75*10^(-3)
!  - THE CONSTANT CMOUT5 (Cdis,non-break) UTILISED IN WESTHUYSEN 
!                                    (2007) IS EQUAL TO 3.29
!  - THE CONSTANT CMOUT6 (Delta) UTILISED IN WESTHUYSEN (2007)
!                                    IS EQUAL TO 0
!
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER   NF  , NPLAN , NPOIN2
      DOUBLE PRECISION CMOUT3, CMOUT4, GRAVIT
      DOUBLE PRECISION CMOUT5, CMOUT6, CIMPLI
      DOUBLE PRECISION USNEW (NPOIN2), USOLD (NPOIN2)
      DOUBLE PRECISION FREQ  (NF)    , DEPTH (NPOIN2), FMOY(NPOIN2) 
      DOUBLE PRECISION ENRJ  (NPOIN2), XKMOY (NPOIN2)
      DOUBLE PRECISION BETAN (NPOIN2), BETAO (NPOIN2)
      DOUBLE PRECISION BETA  (NPOIN2), TAUX1 (NPOIN2)
      DOUBLE PRECISION BETOTO(NPOIN2), BETOTN(NPOIN2)
      DOUBLE PRECISION TSTOT (NPOIN2,NPLAN,NF), TSDER(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION F     (NPOIN2,NPLAN,NF), XK   (NPOIN2,NF)
      LOGICAL PROINF
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER    JP  ,  IFF, IP
      DOUBLE PRECISION  PO , AUX, DIMPLI , C1 , C2 , P0O  , P0N
      DOUBLE PRECISION  B  , w  , DEUPI  , DTETAR  , CPHAS, F_INT, CG1
!      
      DEUPI  = 6.283185307D0
      DTETAR = DEUPI/DBLE(NPLAN)
      DIMPLI = 1.0D0-CIMPLI
      C1     = - CMOUT5*DEUPI**9.D0/GRAVIT**4.D0
      C2     = - CMOUT5*DEUPI
      w      = 25.D0
! 
      IF (PROINF) THEN   
!     ---------------- DEEP WATER CASE
!
!.......WORK ARRAY (TERM DEPENDING ONLY ON THE SPATIAL MESH NODE)
!       """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        DO IP = 1,NPOIN2
          TAUX1(IP) = C1 * ENRJ(IP)**2.D0 * FMOY(IP)**9.D0
        ENDDO
!
!.......LOOP ON THE DISCRETISED FREQUENCIES
!       """""""""""""""""""""""""""""""""""""""""""
        DO IFF = 1,NF
!
          DO IP = 1,NPOIN2
            F_INT=0.D0
            DO JP = 1,NPLAN
              F_INT=F_INT+F(IP,JP,IFF)*DTETAR
            ENDDO
!            
            CPHAS = DEUPI * FREQ(IFF) / XK(IP,IFF)
            P0O=3.D0+TANH(w*(USOLD(IP)/CPHAS-0.1D0))
            P0N=3.D0+TANH(w*(USNEW(IP)/CPHAS-0.1D0))            
!            
            CG1 = 0.5D0*GRAVIT/(DEUPI * FREQ(IFF))
            B   = CG1*(XK(IP,IFF)**3)*F_INT/DEUPI
!            
!..........COMPUTES THE BREAK/NON-BREAK TRANSITION
!          """""""""""""""""""""""""""""""""""""""
            PO  = 1/2.D0*(1+TANH(10.D0*((B/CMOUT4)**(0.5D0)-1.D0)))
!                         
!.........COMPUTES THE BREAK BETA
!         """"""""""""""""""""            
            BETAO(IP)=-CMOUT3*(B/CMOUT4)**(P0O/2.D0)
     *                              *GRAVIT**(0.5D0)*XK(IP,IFF)**(0.5)
            BETAN(IP)=-CMOUT3*(B/CMOUT4)**(P0N/2.D0)
     *                              *GRAVIT**(0.5D0)*XK(IP,IFF)**(0.5)
!
!.........COMPUTES THE NON-BREAK BETA
!         """"""""""""""""""""""""
            AUX = (FREQ(IFF)/FMOY(IP))**2.D0
            BETA(IP)=TAUX1(IP)*((1.D0-CMOUT6)*AUX+CMOUT6*AUX**2.D0)
!
!.........COMPUTES THE TOTAL BETA
!         """"""""""""""""""""            
            BETOTO(IP)=(1-PO)*BETA(IP)+PO*BETAO(IP)
            BETOTN(IP)=(1-PO)*BETA(IP)+PO*BETAN(IP)
          ENDDO                    
!
!.........TAKES THE SOURCE TERM INTO ACCOUNT
!         """"""""""""""""""""""""""""""""
          DO JP = 1,NPLAN
            DO IP = 1,NPOIN2
              TSTOT(IP,JP,IFF)=TSTOT(IP,JP,IFF)
     *              +(DIMPLI*BETOTO(IP)+CIMPLI*BETOTN(IP))*F(IP,JP,IFF)
              TSDER(IP,JP,IFF) = TSDER(IP,JP,IFF) + BETOTN(IP)
            ENDDO
          ENDDO
        ENDDO
!        
      ELSE
!     ---------------- FINITE DEPTH CASE
!
!.......WORK ARRAY (TERM DEPENDING ONLY ON THE SPATIAL MESH NODE)
!       """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        DO IP=1,NPOIN2
          TAUX1(IP) = C2 * ENRJ(IP)**2.D0 * FMOY(IP) * XKMOY(IP)**4.D0
        ENDDO 
!
!.......LOOP ON THE DISCRETISED FREQUENCIES
!       """""""""""""""""""""""""""""""""""""""""""
        DO IFF = 1,NF
!
          DO IP = 1,NPOIN2
            F_INT=0.D0
            DO JP = 1,NPLAN
              F_INT=F_INT+F(IP,JP,IFF)*DTETAR
            ENDDO
!            
            CPHAS = DEUPI * FREQ(IFF) / XK(IP,IFF)
            P0O=3.D0+TANH(w*(USOLD(IP)/CPHAS-0.1D0))
            P0N=3.D0+TANH(w*(USNEW(IP)/CPHAS-0.1D0))            
!                        
            CG1= DEUPI*FREQ(IFF)/XK(IP,IFF)*(0.5D0+XK(IP,IFF)*DEPTH(IP)
     *                               /DSINH(2.D0*XK(IP,IFF)*DEPTH(IP)))

            B   = CG1*(XK(IP,IFF)**3)*F_INT/DEUPI
!            
!..........COMPUTES THE BREAK/NON-BREAK TRANSITION
!          """""""""""""""""""""""""""""""""""""""
            PO  = 1/2.D0*(1+TANH(10.D0*((B/CMOUT4)**(0.5D0)-1.D0)))
!                         
!.........COMPUTES THE BREAK BETA
!         """"""""""""""""""""            
            BETAO(IP)=-CMOUT3*(B/CMOUT4)**(P0O/2.D0)
     *        *(TANH(XK(IP,IFF)*DEPTH(IP)))**((2.D0-P0O)/4.D0)
     *                            *GRAVIT**(0.5D0)*XK(IP,IFF)**(0.5D0)
            BETAN(IP)=-CMOUT3*(B/CMOUT4)**(P0N/2.D0)
     *        *(TANH(XK(IP,IFF)*DEPTH(IP)))**((2.D0-P0N)/4.D0)
     *                            *GRAVIT**(0.5D0)*XK(IP,IFF)**(0.5D0)
!
!.........COMPUTES THE NON-BREAK BETA
!         """"""""""""""""""""""""
            AUX = XK(IP,IFF) / XKMOY(IP)
            BETA(IP)=TAUX1(IP)*((1.D0-CMOUT6)*AUX+CMOUT6*AUX**2.D0)
!
!.........COMPUTES THE TOTAL BETA
!         """"""""""""""""""""            
            BETOTO(IP)=(1-PO)*BETA(IP)+PO*BETAO(IP)
            BETOTN(IP)=(1-PO)*BETA(IP)+PO*BETAN(IP)
          ENDDO                    
!
!.........TAKES THE SOURCE TERM INTO ACCOUNT
!         """"""""""""""""""""""""""""""""
          DO JP = 1,NPLAN
            DO IP = 1,NPOIN2
              TSTOT(IP,JP,IFF)=TSTOT(IP,JP,IFF)
     *              +(DIMPLI*BETOTO(IP)+CIMPLI*BETOTN(IP))*F(IP,JP,IFF)
              TSDER(IP,JP,IFF) = TSDER(IP,JP,IFF) + BETOTN(IP)
            ENDDO
          ENDDO
        ENDDO
!
      ENDIF
!
      RETURN
      END
