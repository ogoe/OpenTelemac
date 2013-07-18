!                    *****************
                     SUBROUTINE BERKHO
!                    *****************
!
     &(LT)
!
!***********************************************************************
! ARTEMIS   V6P1                                   31/05/2011
!***********************************************************************
!
!brief    SOLVES THE BERKHOFF EQUATION MODIFIED BY
!+                THE INTRODUCTION OF DISSIPATION TERMS.
!code
!+      DIV (C*CG*GRAD(PHI)) + C*CG*( K**2 + I*K*MU ) * PHI = 0
!+                                           ------
!+
!+ PHI IS A COMPLEX FUNCTION (REAL COMPONENT: PHIR AND IMAGINARY
!+ COMPONENT: PHII)
!+
!+ MU IS A DISSIPATION COEFFICIENT (A PRIORI UNKNOWN)
!+
!+ THE BOUNDARY CONDITIONS COUPLE THE EQUATIONS IN PHIR AND PHII
!+ THEY ARE:
!+
!+ D (PHI) /DN - I*K*PHI = D (F) /DN - I*K*F (N: EXTERNAL NORMAL)
!+ FOR A LIQUID BOUNDARY WITH INCIDENT WAVE CONDITION DEFINED
!+ BY THE POTENTIAL F (F=0 FOR A FREE EXIT)
!+
!+ D (PHI) /DN - I* (1-R*EXP (I*ALFA))/(1 + R*EXP (I*ALFA))*K*COS (TETA) *PHI = 0
!+ FOR A SOLID BOUNDARY, WITH WALL REFLEXION COEFFICIENT: R,
!+ ANGLE OF INCIDENCE OF THE WAVES ON THE WALL: TETA, AND DEPHASING
!+ CAUSED BY THE WALL: ALFA.
!+
!+ THUS GENERALLY :
!+ D(PHIR)/DN = APHIRB*PHII + BPHIRB*PHIR + CPHIRB
!+ D(PHII)/DN =-APHIRB*PHIR + BPHIRB*PHII + DPHIRB
!+
!+
!+ AFTER VARIATIONAL FORMULATION :
!+
!+         (  AM1          BM1     )  ( PHIR )   ( CV1 )
!+         (                       )  (      ) = (     )
!+         (                       )  (      )   (     )
!+         (  -BM1         AM1     )  ( PHII )   ( CV2 )
!+
!+           /
!+ AM1 =    / C*CG * GRAD(PSII)*GRAD(PSIJ) DS
!+         /S
!+
!+           /
!+       -  / OMEGA**2 * CG/C * PSII*PSIJ  DS
!+         /S
!+
!+           /
!+       -  /  BPHIRB * PSII*PSIJ  DB
!+         /B 
!+
!+           /                         /
!+ BM1 =  - /  APHIR * PSII*PSIJ DB + /  C*CG* K * MU * PSII * PSIJ DS
!+         /B                        /S
!+
!+          /
!+ CV1 =   /   CPHIR * PSII DB
!+        /B
!+
!+          /
!+ CV2 =   /   CPHII * PSII DB
!+        /B
!+
!+
!+ WHERE S IS THE COMPUTATIONAL DOMAIN AND B ITS BOUNDARY
!+       PSII AND PSIJ ARE THE BASIC FUNCTIONS AT NODES I AND J
!+
!+ GIVEN THAT APHII=-APHIR, BM1 IS ALSO IN THE EQUATION IN PHII.
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH)
!+        04/06/1999
!+        V5P1
!+
!
!history
!+        02/04/2007
!+
!+   INVERSION OF THE SECOND EQUATION BEFORE CALL TO SOLVE IF DIRECT
!+   SOLVEUR IS USED
!
!history  C. DENIS (SINETICS)
!+        18/03/2010
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
!| LT             |-->| INDICE OF THE CURRENT CALCULATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
      USE INTERFACE_ARTEMIS, EX_BERKHO => BERKHO
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER, INTENT(IN) :: LT
!
      INTEGER I,ITERMU

      DOUBLE PRECISION HM,HMUE,HEFF,ECRHMU,MODHMU
      DOUBLE PRECISION Q1,Q2,Q3
!
      DOUBLE PRECISION CBID,FFW
      DOUBLE PRECISION PI,DEGRAD,RADDEG
!
!--> VARIABLES FOR CURRENT IN BERKHO
      DOUBLE PRECISION ERREUR1,ERREUR2,ERRMAX1,KNORM
      DOUBLE PRECISION XMUL,XK,ZERO
      
      
      INTEGER J,ITERKN
!
!-----------------------------------------------------------------------
!
      PARAMETER( PI = 3.1415926535897932384626433D0 , DEGRAD=PI/180.D0 )
      PARAMETER( RADDEG = 180.D0 / PI )
!
!-----------------------------------------------------------------------
!
      INTRINSIC ABS,MIN,MAX,LOG
      DOUBLE PRECISION P_DMAX
      EXTERNAL P_DMAX
!
!----------------------------------------------------------------------
!
! INITIALISES MU AND FW: SET TO 0
!         FOR THE FIRST ITERATION
!
      ITERMU=0
      IF (LT.EQ.0) THEN
         CALL OS( 'X=C     ' , MU , SBID , SBID , 0.D0 )
         CALL OS( 'X=C     ' , FW , SBID , SBID , 0.D0 )
      ENDIF
!
!-----------------------------------------------------------------------
      ITERKN=0
      IF (COURANT) THEN
       ZERO=1E-10
!      INITIALISATION OF WAVE VECTOR COMPONENTS X&Y : T5 , T6 
       CALL OS('X=0     ',X=KANCX)
       CALL OS('X=0     ',X=KANCY)
      ENDIF

!
!-----------------------------------------------------------------------
!
!     =========================================
!98 : DISSIPATION : ITERATIVE LOOP : ON THE VARIABLE MU R ON THE WAVE VECTOR  
98    CONTINUE
!
      IF (COURANT) THEN
!     COMPUTE WAVE VECTOR       
!
!       --> CURRENT FIRST ITERATION : U=0  
        IF(ITERKN.GT.0) THEN
!
!       COMPUTE WR AND WAVE NUMBER AMPLITUDE
!       ------------------------------------
        DO I=1,NPOIN
         XK =K%R(I)
         CALL SOLVELAMBDA(XK,
     *                    UC%R(I),VC%R(I),KANCX%R(I),KANCY%R(I),H%R(I))
         K%R(I) =XK
         
         WR%R(I)=SQRT(GRAV*K%R(I)*TANH(K%R(I)*H%R(I)))
         C%R(I) =WR%R(I)/K%R(I)
         CG%R(I)=0.5D0*C%R(I)*
     &           (1.D0 + 2.D0*K%R(I)*H%R(I)/SINH(2.D0*K%R(I)*H%R(I)))
        ENDDO
!
!	ACTUALIZATION OF BOUNDARY CONDITIONS : K AS CHANGED !!
!       ------------------------------------------------------
!	 ----------
         CALL PHBOR       
!        ----------
       ENDIF
      
!     END OF WAVE-CURRENT FIRST STEP : NEXT STEP IS COMPUTING AM AND BM     
      ENDIF
!
!     =========================================
!                   MATRIX AM
!     =========================================
!     ---------------------------
!     DIFFUSION MATRIX FOR AM1
!     ---------------------------
! 
      CALL OS( 'X=YZ    ' , T1 , C , CG , CBID )
      CALL MATRIX(AM1,'M=N     ','MATDIF          ',IELM,IELM,
     &            1.D0,S,S,S,T1,T1,S,MESH,MSK,MASKEL)
!
!-----------------------------------------------------------------------
!
! PANCHANG, TO BE REVISITED: 7 IS GMRES
!
! THE DIFFUSION MATRIX USED FOR PRECONDITIONING IS STORED
! IF THE METHOD IS THAT OF PANCHANG ET AL. (ISOLVE(1) =7)
!
!     IF (ISOLVE(1).EQ.7) THEN
!
!        CALL OM('M=CN    ',AM3,AM1,Z,1.D0/(RELAX*(2.D0-RELAX)),MESH)
!
!     ENDIF
!
!-----------------------------------------------------------------------
!
!     -----------------------
!     MASS MATRIX FOR AM1
!     -----------------------
!     (WARNING : CAN'T USE CURRENT AND SECOND ORDER BOTTOM EFFECTS AT THE SAME TIME)
      IF (COURANT) THEN
         XMUL=1.D0
         CALL OS( 'X=YZ    ' , X=T1,Y=CG,Z=C)
         CALL OS( 'X=YZ    ' , X=T2 , Y=K , Z=K)
         CALL OS( 'X=XY    ' , X=T1 , Y=T2)      
         CALL OS( 'X=C     ' , X=T2 , C=OMEGA**2)
         IF(ITERKN.EQ.0)THEN
          CALL OS( 'X=C     ' , X=T3 , C=OMEGA**2)
         ELSE      
          CALL OS( 'X=YZ    ' , X=T3 , Y=WR , Z=WR )
         ENDIF
         CALL OS( 'X=Y+Z   ' , X=T1 , Y=T1 , Z=T2)
         CALL OS( 'X=Y-Z   ' , X=T1 , Y=T1 , Z=T3)  
         IF (IPENTCO.GT.(0.5)) THEN
           WRITE(LU,*) 'IT IS NOT POSSIBLE TO USE '
           WRITE(LU,*) 'CURRENT + BOTTOM EFFECTS AT THE SAME TIME'
           WRITE(LU,*) '- FOR CURRENT ALONE, FIX IPENTO=0'
           WRITE(LU,*) '- FOR BOTTOM EFFECTS ALONE, DON T USE CURRENT'
           WRITE(LU,*) '-------------------------'
           WRITE(LU,*) 'THE CODE IS GOING TO STOP'
           WRITE(LU,*) '-------------------------'
           STOP
         ENDIF
      ELSE
         CALL OS( 'X=Y/Z   ' , X=T1,Y=CG,Z=C)
         XMUL=OMEGA**2
!        SECOND ORDER BOTTOM EFFECTS ?    
!          (IPENTCO > 0 --> T1 = T1*(1+F) )
!           0 : NO EFFECT /  1 : GRADIENT / 2 : CURVATURE /  3 : GRADIENT+CURVATURE
          IF ( (IPENTCO.GT.(0.5)).AND.(IPENTCO.LT.(3.5)) ) THEN
              CALL PENTCO(IPENTCO)
!             <--T3 = 1+F  
!             -- T2 T4 T5 T6 T7 T9 T8 T11 T12
              CALL OS( 'X=YZ    ' , T1 , T1 , T3 , CBID )
          ENDIF
      ENDIF

      CALL MATRIX(AM2,'M=N     ','FMATMA          ', IELM , IELM ,
     &            XMUL , T1,S,S,S,S,S,MESH,MSK,MASKEL)
!

!     --------------------------------------------------
!     COMPUTES DIFFUSION MATRIX - MASS MATRIX 
!     --------------------------------------------------
      CALL OM( 'M=M+CN  ' , AM1 , AM2 , C , -1.D0 , MESH )
!

!
      IF (COURANT) THEN
!     --------------------------------------------------
!     ADDS CURRENT - CONVECTION MATRIX
!     --------------------------------------------------
!
       CALL MATRIX(AM2,'M=N     ','MAUGUG          ', IELM , IELM ,
     &            1.D0 , S,S,S,UC,VC,S,MESH,MSK,MASKEL)
!
       CALL OM( 'M=M+CN  ' , AM1 , AM2 , C , -1.D0 , MESH )
!
      ENDIF
!     --------------------------------
!     ADDS THE BOUNDARY TERM TO AM1
!     --------------------------------
!
!      AM1 et AM2 --> NON-SYMETRIQUES
       CALL OM( 'M=X(M)  ' , AM1 , AM1 , SBID , CBID , MESH )
       CALL OM( 'M=X(M)  ' , AM2 , AM2 , SBID , CBID , MESH )
!
!        ----------------------------
!        BOUNDARY TERM: INCIDENT WAVE
!        ----------------------------
!
       IF (NPTFR .GT. 0) THEN
         CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &        -1.D0,BPHI1B,S,S,S,S,S,MESH,.TRUE.,MASK1)
         CALL OM( 'M=M+N   ' , AM1 , MBOR , T1 , CBID , MESH )
!
!
         IF(COURANT) THEN
          CALL MATRIX(MBOR,'M=N     ','MATFGUG         ',IELMB,IELMB,
     &        1.D0,MESH%XSGBOR,MESH%YSGBOR,S,UC,VC,S,
     &        MESH,.TRUE.,MASK1)
          CALL OM( 'M=M+N   ' , AM1 , MBOR , SBID , CBID , MESH )
         ENDIF
!  
       ENDIF
!     ------------------------------
!     BOUNDARY TERM: FREE EXIT
!     ------------------------------
       IF (NPTFR .GT. 0) THEN
         CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &        -1.D0,BPHI2B,S,S,S,S,S,MESH,.TRUE.,MASK2)
         CALL OM( 'M=M+N   ' , AM1 , MBOR , T1 , CBID , MESH )
!
         IF(COURANT) THEN
          CALL MATRIX(MBOR,'M=N     ','MATFGUG         ',IELMB,IELMB,
     &        1.D0,MESH%XSGBOR,MESH%YSGBOR,S,UC,VC,S,
     &        MESH,.TRUE.,MASK2)
          CALL OM( 'M=M+N   ' , AM1 , MBOR , SBID , CBID , MESH )
         ENDIF
       ENDIF
!     ------------------------------
!     BOUNDARY TERM : SOLID BOUNDARY
!     ------------------------------
      IF (NPTFR .GT. 0) THEN
         CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &        -1.D0,BPHI3B,S,S,S,S,S,MESH,.TRUE.,MASK3)
         CALL OM( 'M=M+N   ' , AM1 , MBOR , T1 , CBID , MESH )
      END IF
!     --------------------- ---------
!     BOUNDARY TERM: IMPOSED WAVE
!     ------------------------------
      IF (NPTFR .GT. 0) THEN
         CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &        -1.D0,BPHI4B,S,S,S,S,S,MESH,.TRUE.,MASK4)
         CALL OM( 'M=M+N   ' , AM1 , MBOR , T1 , CBID , MESH )
      END IF
!
!     ------------------------------
!      BOUNDARY TERM: INCIDENT POTENTIAL
!     ------------------------------
!
       IF (NPTFR .GT. 0) THEN
         CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &        -1.D0,BPHI1B,S,S,S,S,S,MESH,.TRUE.,MASK5)
         CALL OM( 'M=M+N   ' , AM1 , MBOR , T1 , CBID , MESH )
!
         IF(COURANT) THEN
          CALL MATRIX(MBOR,'M=N     ','MATFGUG         ',IELMB,IELMB,
     &        1.D0,MESH%XSGBOR,MESH%YSGBOR,S,UC,VC,S,
     &        MESH,.TRUE.,MASK5)
          CALL OM( 'M=M+N   ' , AM1 , MBOR , SBID , CBID , MESH )
         ENDIF
!	 
       ENDIF
!
!     =========================================
!                   SECOND MEMBERS
!     =========================================
!     ---------------------
!     SECOND MEMBERS : CV1
!     ---------------------
!
         CALL OS( 'X=C     ' , CV1, SBID , SBID , 0.D0 )
!     ------------------------------
!     BOUNDARY TERM: INCIDENT WAVE
!     ------------------------------
!       --- CALCUL DE i COS(TETAP) GAMMA         
         CALL OS( 'X=CY    ' , T1,TETAP,SBID,DEGRAD)
         CALL OS( 'X=COS(Y)' , T2,T1,SBID,0.D0)
         CALL OS( 'X=YZ    ' , T3,CPHI1B,T2,0.D0)
         CALL OS( 'X=C     ' , T1, SBID , SBID , 0.D0 )
         
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           -1.D0,T3,S,S,S,S,S,MESH,.TRUE.,MASK1)
         END IF
         CALL OSDB( 'X=X+Y   ' , CV1 , T1 , SBID , CBID , MESH )

!        --- CALCUL DE GRAD(Gamma).n : REEL
          CALL OS( 'X=Y     ' , T2,CGRX1B,SBID,0.D0)
          CALL OS( 'X=Y     ' , T3,CGRY1B,SBID,0.D0)
          CALL OS( 'X=C     ' , T1, SBID , SBID , 0.D0 )
          CALL OS( 'X=C     ' , T4, SBID , SBID , 1.D0 )
          IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','FLUBDF          ',IELMB,
     &           1.D0,T4,S,S,T2,T3,S,MESH,.TRUE.,MASK1)
          END IF
          CALL OSDB( 'X=X+Y   ' , CV1 , T1 , SBID , CBID , MESH )
!     ---------------------------------
!     BOUNDARY TERM: INCIDENT POTENTIAL
!     ---------------------------------
!       --- CALCUL DE i COS(TETAP) GAMMA         
         CALL OS( 'X=CY    ' , T1,TETAP,SBID,DEGRAD)
         CALL OS( 'X=COS(Y)' , T2,T1,SBID,0.D0)
         CALL OS( 'X=YZ    ' , T3,CPHI1B,T2,0.D0)
         CALL OS( 'X=C     ' , T1, SBID , SBID , 0.D0 )
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           -1.D0,T3,S,S,S,S,S,MESH,.TRUE.,MASK5)
         END IF
         CALL OSDB( 'X=X+Y   ' , CV1 , T1 , SBID , CBID , MESH )

!       --- CALCUL DE GRAD(Gamma).n : REEL
          CALL OS( 'X=Y     ' , T2,CGRX1B,SBID,0.D0)
          CALL OS( 'X=Y     ' , T3,CGRY1B,SBID,0.D0)
          CALL OS( 'X=C     ' , T1, SBID , SBID , 0.D0 )
          CALL OS( 'X=C     ' , T4, SBID , SBID , 1.D0 )
          IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','FLUBDF          ',IELMB,
     &           1.D0,T4,S,S,T2,T3,S,MESH,.TRUE.,MASK5)
          END IF
          CALL OSDB( 'X=X+Y   ' , CV1 , T1 , SBID , CBID , MESH )


!     ------------------------------
!     BOUNDARY TERM: FREE EXIT
!     ------------------------------
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           1.D0,CPHI2B,S,S,S,S,S,MESH,.TRUE.,MASK2)
            CALL OSDB( 'X=X+Y   ' , CV1 , T1 , SBID , CBID , MESH )
         END IF
!     ------------------------------
!     BOUNDARY TERM: SOLID BOUNDARY
!     ------------------------------
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           1.D0,CPHI3B,S,S,S,S,S,MESH,.TRUE.,MASK3)
            CALL OSDB( 'X=X+Y   ' , CV1 , T1 , SBID , CBID , MESH )
         END IF
!     ------------------------------
!     BOUNDARY TERM: IMPOSED WAVE
!     ------------------------------
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           1.D0,CPHI4B,S,S,S,S,S,MESH,.TRUE.,MASK4)
            CALL OSDB( 'X=X+Y   ' , CV1 , T1 , SBID , CBID , MESH )
         END IF
!

!CP
!         IF (NCSIZE.GT.1) THEN
!           CALL PARCOM(CV1,2,MESH)
!	  ENDIF
!CP


!     ---------------------
!     SECOND MEMBERS : CV2
!     ---------------------
!
         CALL OS( 'X=C     ' , CV2, SBID , SBID , 0.D0 )
!     ------------------------------
!     BOUNDARY TERM: INCIDENT WAVE
!     ------------------------------
!     --- CALCUL DE i COS(TETAP) GAMMA : IMAGINAIRE
         CALL OS( 'X=CY    ' , T1,TETAP,SBID,DEGRAD)
         CALL OS( 'X=COS(Y)' , T2,T1,SBID,0.D0)
         CALL OS( 'X=YZ    ' , T3,DPHI1B,T2,0.D0)
         CALL OS( 'X=C     ' , T1, SBID , SBID , 0.D0 )
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           -1.D0,T3,S,S,S,S,S,MESH,.TRUE.,MASK1)
         END IF
         CALL OSDB( 'X=X+Y   ' , CV2 , T1 , SBID , CBID , MESH )
         
!        --- CALCUL DE GRAD(Gamma).n : IMAGINAIRE
          CALL OS( 'X=Y     ' , T2,DGRX1B,SBID,0.D0)
          CALL OS( 'X=Y     ' , T3,DGRY1B,SBID,0.D0)
          CALL OS( 'X=C     ' , T1, SBID , SBID , 0.D0 )
          CALL OS( 'X=C     ' , T4, SBID , SBID , 1.D0 )

          IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','FLUBDF          ',IELMB,
     &           1.D0,T4,S,S,T2,T3,S,MESH,.TRUE.,MASK1)
          END IF
          CALL OSDB( 'X=X+Y   ' , CV2 , T1 , SBID , CBID , MESH )
!     ---------------------------------
!     BOUNDARY TERM: INCIDENT POTENTIAL
!     ---------------------------------
!     --- CALCUL DE i COS(TETAP) GAMMA : IMAGINAIRE
         CALL OS( 'X=CY    ' , T1,TETAP,SBID,DEGRAD)
         CALL OS( 'X=COS(Y)' , T2,T1,SBID,0.D0)
         CALL OS( 'X=YZ    ' , T3,DPHI1B,T2,0.D0)
         CALL OS( 'X=C     ' , T1, SBID , SBID , 0.D0 )
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           -1.D0,T3,S,S,S,S,S,MESH,.TRUE.,MASK5)
         END IF
         CALL OSDB( 'X=X+Y   ' , CV2 , T1 , SBID , CBID , MESH )
         
!     --- CALCUL DE GRAD(Gamma).n : IMAGINAIRE
          CALL OS( 'X=Y     ' , T2,DGRX1B,SBID,0.D0)
          CALL OS( 'X=Y     ' , T3,DGRY1B,SBID,0.D0)
          CALL OS( 'X=C     ' , T1, SBID , SBID , 0.D0 )
          CALL OS( 'X=C     ' , T4, SBID , SBID , 1.D0 )

          IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','FLUBDF          ',IELMB,
     &           1.D0,T4,S,S,T2,T3,S,MESH,.TRUE.,MASK5)
          END IF
          CALL OSDB( 'X=X+Y   ' , CV2 , T1 , SBID , CBID , MESH )
!
!    ------------------------------
!    BOUNDARY TERM: FREE EXIT
!    ------------------------------
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           1.D0,DPHI2B,S,S,S,S,S,MESH,.TRUE.,MASK2)
         END IF
         CALL OSDB( 'X=X+Y   ' , CV2 , T1 , SBID , CBID , MESH )
!     ------------------------------
!     BOUNDARY TERM: SOLID BOUNDARY
!     ------------------------------
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           1.D0,DPHI3B,S,S,S,S,S,MESH,.TRUE.,MASK3)
            CALL OSDB( 'X=X+Y   ' , CV2 , T1 , SBID , CBID , MESH )
         END IF
!     ------------------------------
!     BOUNDARY TERM: IMPOSED WAVE
!     ------------------------------
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           1.D0,DPHI4B,S,S,S,S,S,MESH,.TRUE.,MASK4)
            CALL OSDB( 'X=X+Y   ' , CV2 , T1 , SBID , CBID , MESH )
         END IF
!CP
!          IF (NCSIZE.GT.1) THEN
!           CALL PARCOM(CV2,2,MESH)
!	  ENDIF
!CP
!     =========================================
!                   MATRIX BM
!     =========================================
!
!     ----------------------------------------------------------
!     COMPUTES THE MATRIX BM1 FOR THE MU VALUES SPECIFIED
!     FOR THE ITERATION 'ITERMU'
!     ----------------------------------------------------------
!
      CALL OS( 'X=YZ    ' , T1 , C  , CG , CBID )
      CALL OS( 'X=YZ    ' , T2 , K  , MU , CBID )
      CALL OS( 'X=YZ    ' , T1 , T1 , T2 , CBID )
      CALL MATRIX(BM1,'M=N     ','FMATMA          ', IELM , IELM ,
     &            1.D0 , T1,S,S,S,S,S,MESH,MSK,MASKEL)
!
      IF ((COURANT).AND.(ITERKN.GT.0)) THEN
!     ----------------------------------------------------------
!     ADD TERMS TO BM1 FOR THE CURRENT 2*OMEGA.U TERM
!     ----------------------------------------------------------
!          ON DESYMETRISE BM1
           CALL OM( 'M=X(M)  ' , BM1 , BM1 , SBID , CBID , MESH )
!
           CALL MATRIX(BM2,'M=N     ','MATVGR          ',IELM ,IELM ,
     *            2D0*OMEGA , S,S,S,UC,VC,S,
     *            MESH,MSK,MASKEL)  
!
           CALL OM( 'M=M+N   ' , BM1 , BM2 , T1 , CBID , MESH )
!
!     ----------------------------------------------------------
!     ADD TERMS TO THE MATRIX BM1 FOR THE CURRENT : DIV(U) TERM
!     ----------------------------------------------------------
!
           CALL VECTOR(T2 , '=' , 'MASBAS          ' , IELM ,
     *            1.D0 , S , S , S , S , S , S ,
     *            MESH , MSK  , MASKEL )
!
           CALL VECTOR(T13 , '=' , 'GRADF          X' , IELM ,
     *            1.D0 , UC , S , S , S , S , S ,
     *            MESH , MSK , MASKEL)
!
           CALL VECTOR(T14 , '=' , 'GRADF          Y' , IELM ,
     *            1.D0 , VC , S , S , S , S , S ,
     *            MESH , MSK , MASKEL)
!
           CALL OS( 'X=Y+Z   ' , X=T15 , Y=T14 , Z=T13 )
           CALL OS( 'X=Y/Z   ' , X=T16 , Y=T15 , Z=T2 )  
!
           CALL MATRIX(BM2,'M=N     ','FMATMA          ',IELM ,IELM ,
     *            OMEGA , T16,S,S,S,S,S,
     *            MESH,MSK,MASKEL)
!
           CALL OM( 'M=M+N   ' , BM1 , BM2 , SBID , CBID , MESH )
      ENDIF


!     -------------------------------------------
!     ADDS THE BOUNDARY TERM TO BM1
!     -------------------------------------------
!
      IF (NPTFR .GT. 0) THEN
!        ------------------------------
!        BOUNDARY TERM: INCIDENT WAVE
!        ------------------------------
         CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &        -1.D0,APHI1B,S,S,S,S,S,MESH,.TRUE.,MASK1)
!         WRITE (*,*)'MBOR = ', MBOR%TYPEXT
         CALL OM( 'M=M+N   ' , BM1 , MBOR , T1 , CBID , MESH )
      END IF
!        ------------------------------
!        BOUNDARY TERM: FREE EXIT
!        ------------------------------
      IF (NPTFR .GT. 0) THEN
            CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &           -1.D0,APHI2B,S,S,S,S,S,MESH,.TRUE.,MASK2)
         CALL OM( 'M=M+N   ' , BM1 , MBOR , T1 , CBID , MESH )
      END IF
!        ------------------------------
!        BOUNDARY TERM: SOLID BOUNDARY
!        ------------------------------
      IF (NPTFR .GT. 0) THEN
         CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &        -1.D0,APHI3B,S,S,S,S,S,MESH,.TRUE.,MASK3)
         CALL OM( 'M=M+N   ' , BM1 , MBOR , T1 , CBID , MESH )
      END IF
!        ------------------------------
!        BOUNDARY TERM: IMPOSED WAVE
!        ------------------------------
      IF (NPTFR .GT. 0) THEN
            CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &           -1.D0,APHI4B,S,S,S,S,S,MESH,.TRUE.,MASK4)
            CALL OM( 'M=M+N   ' , BM1 , MBOR , T1 , CBID , MESH )
      END IF
!        ------------------------------
!        BOUNDARY TERM: INCIDENT POTENTIAL
!        ------------------------------
      IF (NPTFR .GT. 0) THEN
         CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &        -1.D0,APHI1B,S,S,S,S,S,MESH,.TRUE.,MASK5)
         CALL OM( 'M=M+N   ' , BM1 , MBOR , T1 , CBID , MESH )
      END IF

!     ---------
!     AM2 = AM1
!     ---------
!
      CALL OM( 'M=N     ' , AM2 , AM1 , SBID , CBID , MESH )
!
!     --------------------------
!     BM1 BECOMES NONSYMMETRICAL
!     --------------------------
!
!     SI ON N'A PAS DE COURANT OU SI ON NE L'A PAS ENCORE PRIS EN COMPTE
!
      IF ((.NOT.COURANT).OR.ITERKN.EQ.0) THEN
       CALL OM( 'M=X(M)  ' , BM1 , BM1 , SBID , CBID , MESH )
      ENDIF

!
!     ----------------------------
!     TRIES MASS-LUMPING OF BM1
!     ----------------------------
!
!     MASLU = 1.D0
!     CALL LUMP(T1,BM1,MESH,XMESH,MASLU,MSK,MASKEL)
!     CALL OM( 'M=CN    ' , BM1 , BM1 , T1 , 1.D0-MASLU , MESH )
!     CALL OM( 'M=M+D   ' , BM1 , BM1 , T1 , C          , MESH )
!
!     ----------
!     BM2 = -BM1
!     ----------
!
      CALL OM( 'M=CN    ' , BM2 , BM1 , C , -1.D0 , MESH )
!
!     =======================================
!
!     TAKES INTO ACCOUNT DIRICHLET POINTS
!
!     =======================================
!
      IF (DEFERL .OR. FROTTE) THEN
         IF (LNG.EQ.1) WRITE(LU,220) ITERMU+1
         IF (LNG.EQ.2) WRITE(LU,221) ITERMU+1
 220     FORMAT(/,1X,'SOUS-ITERATION NUMERO :',1X,I3,/)
 221     FORMAT(/,1X,'SUB-ITERATION NUMBER :',1X,I3,/)
      ENDIF
      CALL DIRICH(UNK,MAT,RHS,PHIB,LIDIR%I,TB,MESH,KENT,MSK,MASKEL)
!
!     ===============================================================
!
!     INHIBITS POSSIBLE DIAGONAL PRECONDITIONING
!     IF AN ELEMENT OF DAM1 IS NEGATIVE OR NULL
!
!     ===============================================================
!
      CALL CNTPRE(AM1%D%R,NPOIN,SLVART%PRECON,SLVART%PRECON)
!      IF (LNG.EQ.1) WRITE(LU,230) SLVART%PRECON
!      IF (LNG.EQ.2) WRITE(LU,231) SLVART%PRECON
! 230  FORMAT(/,1X,'PRECONDITIONNEMENT APRES CONTROLE :',1X,I3)
! 231  FORMAT(/,1X,'PRECONDITIONNING AFTER CONTROL :',1X,I3)
!
!     ==========================================================
!
!     PRECONDITIONING BLOCK-DIAGONAL:
!                 THE MATRICES BECOME NONSYMMETRICAL.
!
!     ==========================================================
!
      IF (3*(SLVART%PRECON/3).EQ.SLVART%PRECON) THEN
        CALL OM( 'M=X(M)  ' , AM1 , AM1 , SBID , CBID , MESH )
        CALL OM( 'M=X(M)  ' , AM2 , AM2 , SBID , CBID , MESH )
      ENDIF
!
!     ==============================
!
!     SOLVES THE LINEAR SYSTEM
!
!     ==============================
!
!     ----------------------------
!     INITIALISES THE UNKNOWN
!     ----------------------------
!
      IF(ITERMU.EQ.0.AND.LT.EQ.0) THEN
        CALL LUMP(T1,AM1,MESH,1.D0)
        CALL OS( 'X=Y/Z   ' , PHIR , CV1 , T1 , CBID )
        CALL LUMP(T1,AM2,MESH,1.D0)
        CALL OS( 'X=Y/Z   ' , PHII , CV2 , T1 , CBID )
      ENDIF
!
      IF (LNG.EQ.1) WRITE(LU,240)
      IF (LNG.EQ.2) WRITE(LU,241)
 240  FORMAT(/,1X,'RESOLUTION DU SYSTEME LINEAIRE (SOLVE)',/)
 241  FORMAT(/,1X,'LINEAR SYSTEM SOLVING (SOLVE)',/)
!
      CALL SOLVE(UNK,MAT,RHS,TB,SLVART,INFOGR,MESH,AM3)
!
!     ============================================================
!     WAVE-CURRENT :  
!     CHECKS CONVERGENCE ON THE WAVE VECTOR                    (<-- to integrate with the dissipation loop?)
!     ============================================================
      IF (COURANT) THEN  
!      COMPUTE WAVE INCIDENCE USING SPEED AT THE FREE SURFACE
!      ------------------------------------------------------
!
!      -----------
       CALL CALDIR()
!      -----------
!          --> PHIR,PHII
!          --  T1,T2,T3,T4
!         <--  INCI

!
!       --- DIRECTION OF VECTOR K : INCI
       CALL OS( 'X=COS(Y)' , T5,INCI,SBID,0.D0)
       CALL OS( 'X=SIN(Y)' , T6,INCI,SBID,0.D0)
!      T5 = K  COS(INCIDENCE)
       CALL OS( 'X=XY    ' , X=T5 , Y=K)      
!      T6 = K  SIN(INCIDENCE)
       CALL OS( 'X=XY    ' , X=T6 , Y=K)      
!      ------------------------------------------------------
!
!     ----------------------
!     CONVERGENCE CRITERION
!     ---------------------
!      Error Initialisation 
       ERRMAX1=0.D0
       ERREUR1=0.D0
!      CONVERGENCE CRITERION ON WAVE NUMBER : 1%
       EPSDIS =1.D-02
!                                     ->    ->
!      MAX ERROR CALCULATION : NORM( Kn - Kn-1 )/NORM(Kn) < EPSDIS
!      ----------------------
       IF (ITERKN.GT.0) THEN
         DO I=1,NPOIN
          KNORM=SQRT(T5%R(I)**2+T6%R(I)**2)
          ERREUR1=(KANCX%R(I)-T5%R(I))**2
          ERREUR2=(KANCY%R(I)-T6%R(I))**2
          ERREUR1=ERREUR1+ERREUR2
          ERREUR1=SQRT(ERREUR1)/KNORM
          IF (ERREUR1.GT.ERRMAX1) THEN
           ERRMAX1=ERREUR1
          ENDIF
         ENDDO
         ERREUR1=ERRMAX1
!        MAX ERROR OUTPUTS
!        -----------------
         WRITE(LU,*) '-----------------------------------------------'
         WRITE(LU,*) 'WAVE-CURRENT : DIFF. BETWEEN 2 ITER. =',ERREUR1
         WRITE(LU,*) 'LOOP FOR WAVE-CURRENT : TOLERANCE    =',EPSDIS
        ELSE
         WRITE(LU,*) 'INITIAL LOOP FOR WAVE-CURRENT TERMINATED'
       ENDIF
       WRITE(LU,*) '-----------------------------------------------'
!
!      OLD WAVE VECTOR STORAGE
!      -----------------------
       CALL OS( 'X=Y     ' , X=KANCX , Y=T5 ) 
       CALL OS( 'X=Y     ' , X=KANCY , Y=T6 )      

       IF ( (ERREUR1.GT.EPSDIS).OR.(ITERKN.EQ.0) ) THEN
!         NEW ITERATION FOR WAVE INCIDENCE AND WAVE NUMBER
!         -----------------------
          ITERKN = ITERKN + 1
          GOTO 98
       ENDIF
       IF (LNG.EQ.1) WRITE(LU,202) ITERKN
       IF (LNG.EQ.2) WRITE(LU,203) ITERKN
202    FORMAT(/,1X,'NOMBRE DE SOUS-ITERATIONS POUR LA DIRECTION DE K:',
     *   1X,I3)  
203    FORMAT(/,1X,'NUMBER OF SUB-ITERATIONS FOR DIRECTION OF K:',
     *   1X,I3)
!      REMISE A 1 DU NOMBRE d'ITERATION SUR LE COURRANT
       ITERKN=1
!     =================================================
!     END OF THE LOOP ON WAVE DIRECTION AND WAVE NUMBER
!     =================================================
      ENDIF

!
!     ============================================================
!
!     COMPUTES THE TOTAL DISSIPATION COEFFICIENT MU_DEFERL + MU_FROTTE
!                                                  (MU2)       (T1)
!     IF BREAKING OR BOTTOM FRICTION TAKEN INTO ACCOUNT
!     ============================================================
!
!
      IF (DEFERL .OR. FROTTE) THEN
         ECRHMU = 0.D0
!
!     --------------------------------------------
!     INITIALISES MU2 AND T3: SET TO 0
!     MU2: NEW DISSIPATION COEFFICIENT
!     T3: QB FOR THE CURRENT PERIOD
!     --------------------------------------------
!
         CALL OS( 'X=C     ' , MU2 , SBID , SBID , 0.D0 )
         CALL OS( 'X=C     ' , T3  , SBID , SBID , 0.D0 )
!
!        ----------------------------------------------------
!        COMPUTES THE WAVE HEIGHT HMU CORRESPONDING TO
!        THE SOLUTION OF THE SYSTEM
!
         CALL OS( 'X=N(Y,Z)', T1  , PHIR , PHII , CBID )
         CALL OS( 'X=CY    ', HMU , T1   , SBID , 2.D0*OMEGA/GRAV )
!
!        --------------
!        IF BREAKING
!        --------------
!
         IF (DEFERL) THEN
!
!        ------------------------------------------------------
!        TESTS IF HMU > HM (THERE IS BREAKING) OR NOT,
!        AND CALCULATES MU2 ACCORDING TO DALLY OR BATTJES & JANSSEN
!        (IF REGULAR WAVES)
!        ------------------------------------------------------
!
            IF (.NOT. ALEMON .AND. .NOT. ALEMUL) THEN
               DO 20 I = 1,NPOIN
                  HM = 0.88D0/K%R(I)*TANH(GAMMAS*K%R(I)*H%R(I)/0.88D0)
!
!     HMUE = HMU/SQRT(2)
!
                  HMUE = HMU%R(I)/1.4142D0
                  HEFF=MIN(HMUE,HM)
                  HEFF=MAX(HEFF,1.D-5)
                  Q1 = 1.D-10
                  Q2 = (HEFF/HM)**2
!     ADDED BY JMH BECAUSE OF THE LOG FUNCTION, LATER ON
                  Q2 = MAX(Q2,1.D-9)
!
!     ------------
!     COMPUTES QB
!     ------------
!
                  CALL CALCQB(Q1,Q2,Q3)
!
!     ALGORITHM SPECIFIC TO REGULAR WAVES
!     FOR THE COMPUTATION OF THE RATE OF BREAKING
!
                  IF (ITERMU.EQ.0) THEN
                     IF (Q3.LT.0.19D0) THEN
                        T3%R(I) = 0.D0
                     ELSE
                        T3%R(I) = 1.D0
                     ENDIF
!
!                 T3 COMPUTED AT ITERMU = 0
!                 IS TEMPORARILY STORED IN QB
!
                     QB%R(I) = T3%R(I)
                  ELSE
                     IF (QB%R(I).EQ.1.D0) THEN
                        IF (Q3.LT.0.1D0) THEN
                           T3%R(I) = 0.D0
                        ELSE
                           T3%R(I) = 1.D0
                        ENDIF
                     ENDIF
                  ENDIF
 20            CONTINUE
!
!
!           --------------------------------
!           DALLY AND AL 1985
!           --------------------------------
!
               IF (IBREAK.EQ.2) THEN
                  DO 30 I = 1,NPOIN
                    HM = 0.88D0/K%R(I)*TANH(GAMMAS*K%R(I)*H%R(I)/0.88D0)
                     HEFF=MIN(HMU%R(I),HM)
                     HEFF=MAX(HEFF,1.D-5)
                     MU2%R(I)=T3%R(I)*KDALLY*
     &                    (1.D0-(GDALLY*H%R(I)/HEFF)**2)/H%R(I)
 30               CONTINUE
               ENDIF
!
!     -------------------------------------
!     BATTJES & JANSSEN 1978
!     -------------------------------------
!
               IF (IBREAK.EQ.1) THEN
                  DO 40 I = 1,NPOIN
                   HM = 0.88D0/K%R(I)*TANH(GAMMAS*K%R(I)*H%R(I)/0.88D0)
                     HEFF=MIN(HMU%R(I),HM)
                     HEFF=MAX(HEFF,1.D-5)
                     MU2%R(I) = T3%R(I)*2.D0*HEFF/(H%R(I)*CG%R(I)*PER)
 40               CONTINUE
               ENDIF
!
!     -------------------------------------------------------------
!     COMPUTES FIRST QB=T3, PROPORTION OF BREAKING OR BROKEN WAVES,
!     THEN MU2 ACCORDING TO B&J 78 (RANDOM SEAS)
!     -------------------------------------------------------------
!
            ELSE
               DO 50 I = 1,NPOIN
                  HM = 0.88D0/K%R(I)*TANH(GAMMAS*K%R(I)*H%R(I)/0.88D0)
!
!     HMUE = HMU/SQRT (2)
!
               HMUE = HMU%R(I)/1.4142D0
               HEFF=MIN(HMUE,HM)
               HEFF=MAX(HEFF,1.D-5)
               Q1 = 1.D-10
               Q2 = (HEFF/HM)**2
!     ADDED BY JMH BECAUSE OF THE LOG FUNCTION, LATER ON
               Q2 = MAX(Q2,1.D-9)
!
!              ------------
!              COMPUTES QB
!              ------------
!
               CALL CALCQB(Q1,Q2,Q3)
               T3%R(I) = Q3
!
!              -------------------------
!              COMPUTES MU2
!              -------------------------
!
               HEFF = MIN((HMU%R(I)/1.4142D0),HM)
               HEFF=MAX(HEFF,1.D-5)
               MU2%R(I)=ALFABJ*OMEGA*T3%R(I)*((HM/HEFF)**2)/
     &                (3.141592653589D0*CG%R(I))
 50         CONTINUE
!
         END IF
!
!        ------------------
!        END 'IF BREAKING'
!        ------------------
!
         ENDIF
!
!        --------------------------------
!        RE-INITIALISES T1 = 0 BECAUSE
!        T1 REPRESENTS MU_FROTTEMENT IN THE FOLLOWING
!        --------------------------------
!
         CALL OS( 'X=C     ' , T1 , C , CG , 0.D0 )
!
!        ---------------------
!        IF BOTTOM FRICTION
!        ---------------------
!
         IF (FROTTE) THEN
!
!           ------------------------------------------------
!           IF ENTFW=TRUE, THE FRICTION COEFFICIENT FW
!           IS THE SAME EVERYWHERE IN THE DOMAIN
!           ------------------------------------------------
!
            IF (ENTFW) THEN
               CALL FWSPEC(FW%R,FWCOEF,MESH%X%R,MESH%Y%R,
     &                     NPOIN,PRIVE,ZF%R)
            ELSE
               DO 70 I = 1,NPOIN
                  CALL CALCFW
     &                   (I,H%R,C%R,CG%R,K%R,HMU%R,
     &                    NPOIN,OMEGA,GRAV,
     &                    VISCO,DIAM90,DIAM50,MVSED,MVEAU,
     &                    FORMFR,REGIDO,RICOEF,
     &                    ENTREG,ENTRUG,FFW)
                  FW%R(I) = FFW
 70            CONTINUE
            ENDIF
!
!           -----------------------------------------
!           COMPUTES THE DISSIPATION COEFFICIENT FOR
!           BOTTOM FRICTION
!           -----------------------------------------
!
            IF (FORMFR .EQ. 1) THEN
!
!           ---------------------------------------------------
!           COMPUTES AN EFFECTIVE SPEED
!           UE = 1.2D0*(0.5*((DPHIR/DX)**2 + (DPHIR/DY)**2
!                         +(DPHII/DX)**2 + (DPHII/DY)**2))**0.5
!           UE IS STORED IN T4 HERE
!           ---------------------------------------------------
!
               CALL CALCUE
!
!              ----------------------------------------
!              THE DISSIPATION COEFFICIENT MU FOR
!              FRICTION IS STORED IN T1
!              ----------------------------------------
!
               CALL OS( 'X=C     ' , T1 , SBID , SBID , 0.D0 )
!
               DO 80 I = 1,NPOIN
                  T1%R(I) = (0.5D0*FW%R(I)*T4%R(I))/
     &                    (H%R(I)*((COSH(K%R(I)*H%R(I)))**2))
                  T1%R(I) = T1%R(I)/CG%R(I)
 80            CONTINUE
            ENDIF
!
            IF (FORMFR .EQ. 2) THEN
               CALL OS( 'X=C     ' , T1 , SBID , SBID , 0.D0 )
               DO 90 I = 1,NPOIN
                  T1%R(I) = (2*FW%R(I)*HMU%R(I)*
     &                    ((OMEGA/SINH(K%R(I)*H%R(I)))**3))
                  T1%R(I) = T1%R(I)/(3.D0*3.14159D0*GRAV)
                  T1%R(I) = T1%R(I)/CG%R(I)
 90            CONTINUE
            ENDIF
!
!        -------------------------
!        END 'IF BOTTOM FRICTION'
!        -------------------------
!
         END IF
!
!        -------------------------------------------------------
!        RELAXATION ON MU2 TO TRY AND AVOID OSCILLATIONS IN THE
!        CONVERGENCE OF THE SOLVEUR
!        -------------------------------------------------------
!
!
         MODHMU = 1.D-9
         DO I = 1,NPOIN
!
!           --------------------------
!           MU = MU_DEFERL + MU_FROTTE
!           --------------------------
!
            MU2%R(I) = MU2%R(I) + T1%R(I)
!
!           ----------
!           RELAXATION
!           ----------
!
            MU2%R(I) = MU%R(I) + RELDIS * (MU2%R(I) - MU%R(I))
            IF(ITERMU.EQ.0) THEN
               HMUANC%R(I) = HMU%R(I)
               ECRHMU = 1.D0
               MU%R(I) = MU2%R(I)
            ELSE
               ECRHMU = MAX(ECRHMU,ABS(HMU%R(I)-HMUANC%R(I)))
               MODHMU = MAX(MODHMU,ABS(HMU%R(I)))
               MU%R(I) = MU2%R(I)
               HMUANC%R(I) = HMU%R(I)
            ENDIF
         ENDDO
!
!
!        RELAXES THE RELAXATION AT EACH SUB-ITERATION
!        TO FACILITATE CONVERGENCE OF THE ALGORITHM USED TO
!        COMPUTE DISSIPATION (REGULAR WAVES)
!
         IF (.NOT. ALEMON .AND. .NOT. ALEMUL) THEN
            RELDIS = RELDIS * 0.85D0
         ENDIF
!
         IF (NCSIZE .NE. 0) THEN
            ECRHMU = P_DMAX(ECRHMU)
            MODHMU = P_DMAX(MODHMU)
         END IF
         IF (LNG.EQ.1) WRITE(LU,*) 'ECART ENTRE DEUX
     &        SOUS-ITERATIONS (%)',
     &        100*ECRHMU/MODHMU
         IF (LNG.EQ.2) WRITE(LU,*) 'DIFF. BETWEEN TWO
     &        SUB-ITERATIONS (%) ',
     &        100*ECRHMU/MODHMU
         ITERMU = ITERMU + 1
!
!
!        -----------------------------------------------------------
!        IF NUMBER OF SUB-ITERATIONS FOR MU >= MAX NUMBER OF SUB-ITERATIONS
!        EXITS THE LOOP OVER MU AND SETS THE RELATIVE DIFFERENCE
!        ECRHMU/MODHMU TO 10 % OF EPSDIS
!        -----------------------------------------------------------
!
         IF(ITERMU.GE.NITDIS) THEN
            IF (LNG.EQ.1) WRITE(LU,100) ITERMU
            IF (LNG.EQ.2) WRITE(LU,101) ITERMU
 100        FORMAT(/,1X,'BERKHO (ARTEMIS): NOMBRE DE SOUS-ITERATIONS',
     & 1X,'MAXIMUM ATTEINT :',1X,I3)
 101        FORMAT(/,1X,'BERKHO (ARTEMIS): YOU REACHED THE MAXIMUM',
     & 1X,'NUMBER OF SUB-ITERATIONS :)',1X,I3)
            ECRHMU = EPSDIS*MODHMU/10.D0
         ENDIF
!
!        ------------------------------------------------
!        CHECKS CONVERGENCE ON THE DISSIPATION LOOP
!        ------------------------------------------------
!
         WRITE(LU,*) ' '
         WRITE(LU,*) '----------------------------------------------- '
         IF (ECRHMU.GT.EPSDIS*MODHMU) GOTO 98
!
         IF (.NOT. ALEMON .AND. .NOT. ALEMUL) THEN
            CALL OS( 'X=Y     ', QB,T3,SBID,CBID)
         ELSE
            CALL OS( 'X=X+Y   ', QB,T3,SBID,CBID)
         ENDIF
!
         IF (LNG.EQ.1) WRITE(LU,200) ITERMU
         IF (LNG.EQ.2) WRITE(LU,201) ITERMU
 200     FORMAT(/,1X,'NOMBRE DE SOUS-ITERATIONS POUR LA DISSIPATION:',
     &   1X,I3)
 201     FORMAT(/,1X,'NUMBER OF SUB-ITERATIONS FOR DISSIPATION:',
     &   1X,I3)
!
!     ========================================
!
!     END 'IF BREAKING OR BOTTOM FRICTION'
!
!     ========================================
!
      ENDIF
!
! END OF THE ITERATIVE LOOP ON THE DISSIPATION TERM MU
!
!-----------------------------------------------------------------------
!
      RETURN
      END
