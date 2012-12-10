!                       ****************** 
                        SUBROUTINE DIFFRAC 
!                       ****************** 
! 
     &( CX    , CY    , CT    , XK    , CG    , DEPTH , DZX   , DZY   ,  
     &  FREQ  , COSTET, SINTET, NPOIN2, NPLAN , IFF   , NF    , PROINF,  
     &  SPHE  , A     , DFREQ , F     , CCG   , DIV   , DELTA , DDX   ,  
     &  DDY   , EPS   , NBOR  , NPTFR , XKONPT, RK    , RX    , RY    ,  
     &  RXX   , RYY   , NEIGB , NB_CLOSE, DIFFRA, MAXNSP, FLTDIF      )  
! 
!*********************************************************************** 
! TOMAWAC   V6P2                                   25/06/2012 
!*********************************************************************** 
! 
!brief    COMPUTES DIFFRACTION. 
!+ 
!+            COMPUTES THE DIFFRACTION TERM AND THE 
!+            DIFFRACTION-CORRECTED GROUP VELOCITY 
! 
!history  E. KRIEZI (LNH) 
!+        04/12/2006 
!+        V5P5 
!+ 
! 
! 
!history  G.MATTAROLO (EDF - LNHE) 
!+        23/06/2012 
!+        V6P2 
!+   Modification for V6P2 
! 
!history  J-M HERVOUET (EDF R&D, LNHE) 
!+        10/12/2012
!+        V6P3 
!+   4 subroutines GRAD-... inlined and removed from the tomawac library
!+   The inlined part has to be optimised... very strangely written... 
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
!| A              |<--| AMPLITUDE OF DIRECTIONAL SPECTRUM 
!| CCG            |<--| GROUP VELOCITY TIMES PHASE VELOCITY 
!| CG             |-->| DISCRETIZED GROUP VELOCITY 
!| COSTET         |-->| COSINE OF TETA ANGLE 
!| CX             |<->| ADVECTION FIELD ALONG X(OR PHI) 
!| CY             |<->| ADVECTION FIELD ALONG Y(OR LAMBDA) 
!| CT             |<->| ADVECTION FIELD ALONG TETA 
!| DDX            |<--| X-DERIVATIVE OF A VARIABLE 
!| DDY            |<--| Y-DERIVATIVE OF A VARIABLE  
!| DELTA          |<--| DIFFRACTION PARAMETER 
!| DEPTH          |-->| WATER DEPTH 
!| DFREQ          |-->| FREQUENCY STEPS BETWEEN DISCRETIZED FREQUENCIES 
!| DIFFRA         |-->| IF >0 DIFFRACTION IS CONSIDERED 
!|                      IF = 1 MSE FORMULATION 
!|                      IF = 2 RMSE FORMULATION 
!| DIV            |<--| DIVERGENCE OF FUNCTION USED FOR DELTA COMPUT. 
!| DZX            |-->| BOTTOM SLOPE ALONG X 
!| DZY            |-->| BOTTOM SLOPE ALONG Y 
!| EPS            |-->| VARIANCE THRESHOLD FOR DIFFRACTION 
!| F              |<->| VARIANCE DENSITY DIRECTIONAL SPECTRUM 
!| FLTDIF         |-->| IF TRUE, LOCAL AMPLITUDES ARE FILTERED 
!| FREQ           |-->| DISCRETIZED FREQUENCIES 
!| IFF            |-->| FREQUENCY INDEX 
!| MAXNSP         |-->| CONSTANT FOR MESHFREE TECHNIQUE 
!| NB_CLOSE       |-->| ARRAY USED IN THE MESHFREE TECHNIQUE 
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS 
!| NEIGB          |-->| NEIGHBOUR POINTS FOR MESHFREE METHOD 
!| NF             |-->| NUMBER OF FREQUENCIES 
!| NPLAN          |-->| NUMBER OF DIRECTIONS 
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH 
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS 
!| PROINF         |-->| LOGICAL INDICATING INFINITE DEPTH ASSUMPTION 
!| RK             |-->| ARRAY USED IN THE MESHFREE TECHNIQUE 
!| RX             |-->| ARRAY USED IN THE MESHFREE TECHNIQUE 
!| RXX            |-->| ARRAY USED IN THE MESHFREE TECHNIQUE 
!| RY             |-->| ARRAY USED IN THE MESHFREE TECHNIQUE 
!| RYY            |-->| ARRAY USED IN THE MESHFREE TECHNIQUE 
!| SINTET         |-->| SINE OF TETA ANGLE 
!| SPHE           |-->| LOGICAL INDICATING SPHERICAL COORD ASSUMPTION 
!| XK             |-->| DISCRETIZED WAVE NUMBER 
!| XKONPT         |<--| ARRAY USED FOR COMPUTING DIFFRACTION PARAMETER 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
! 
      USE BIEF
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI,ST1,ST0,ST4,IELM2,SA,MESH,
     &                                 SCCG,SDELTA,SXKONPT,SDDX,SDDY
!
      IMPLICIT NONE 
! 
      INTEGER LNG,LU       
      COMMON/INFO/ LNG,LU 
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
! 
      INTEGER NF,NPLAN,NPOIN2,NPTFR,IFF,MAXNSP
      INTEGER NB_CLOSE(NPOIN2),NEIGB(NPOIN2,MAXNSP) 
      INTEGER DIFFRA,NBOR(NPTFR) 
      DOUBLE PRECISION CX(NPOIN2,NPLAN), CY(NPOIN2,NPLAN) 
      DOUBLE PRECISION CT(NPOIN2,NPLAN), FREQ(NF) 
      DOUBLE PRECISION CG(NPOIN2,NF), XK(NPOIN2,NF) 
      DOUBLE PRECISION DEPTH(NPOIN2) 
      DOUBLE PRECISION DZX(NPOIN2), DZY(NPOIN2) 
      DOUBLE PRECISION COSTET(NPLAN), SINTET(NPLAN) 
      DOUBLE PRECISION A(NPOIN2) 
      DOUBLE PRECISION DFREQ(NF) 
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF) 
      DOUBLE PRECISION CCG(NPOIN2), DIV(NPOIN2) 
      DOUBLE PRECISION DELTA(NPOIN2) 
      DOUBLE PRECISION DDX(NPOIN2), DDY(NPOIN2) 
      DOUBLE PRECISION EPS 
      DOUBLE PRECISION RK(MAXNSP,NPOIN2) 
      DOUBLE PRECISION RX(MAXNSP,NPOIN2), RY(MAXNSP,NPOIN2)   
      DOUBLE PRECISION RXX(MAXNSP,NPOIN2), RYY(MAXNSP,NPOIN2) 
      DOUBLE PRECISION XKONPT(NPOIN2) 
      LOGICAL PROINF,SPHE, FLTDIF 
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!      
      INTEGER I, IP, IPOIN 
      DOUBLE PRECISION DDDN,DSDNSK,DEUKD
      DOUBLE PRECISION CDELTA,DELTAN
      DOUBLE PRECISION S_RMSE_IN, WJUNK, WJUNK2 
      DOUBLE PRECISION,ALLOCATABLE:: SQRDELTA(:) 
      DOUBLE PRECISION,ALLOCATABLE:: SQRCCG(:),A_RMSE(:) 
      DOUBLE PRECISION,ALLOCATABLE:: FRDK(:,:),FRDA(:,:),SCDA(:,:) 
      LOGICAL,ALLOCATABLE:: L_DELTA(:) 
!      
      LOGICAL DEJA
      DATA DEJA/.FALSE./
!      
      SAVE
! 
!*********************************************************************** 
! 
      IF(.NOT.DEJA)THEN
         ALLOCATE(SQRDELTA(NPOIN2))
         ALLOCATE(SQRCCG(NPOIN2))
         ALLOCATE(A_RMSE(NPOIN2))
         ALLOCATE(FRDK(NPOIN2,2))
         ALLOCATE(FRDA(NPOIN2,2))
         ALLOCATE(SCDA(NPOIN2,3))
         ALLOCATE(L_DELTA(NPOIN2))
         DEJA=.TRUE.
      ENDIF
! 
!----------------------------------------------------------------------- 
!     INFINITE WATER DEPTH ... 
!----------------------------------------------------------------------- 
! 
      IF(PROINF) THEN 
        IF(LNG.EQ.1) THEN 
          WRITE(LU,*) '' 
          WRITE(LU,*) '***************************************' 
          WRITE(LU,*) ' ATTENTION : LA DIFFRACTION N''EST PAS ' 
          WRITE(LU,*) ' PRISE EN COMPTE DANS LE CAS D''UNE    ' 
          WRITE(LU,*) ' PROFONDEUR INFINIE                    ' 
          WRITE(LU,*) '***************************************' 
        ELSE 
          WRITE(LU,*) '' 
          WRITE(LU,*) '***************************************' 
          WRITE(LU,*) ' ATTENTION : DIFFRACTION IS NOT TAKEN  ' 
          WRITE(LU,*) ' INTO ACCOUNT IN THE CASE OF INFINITE  ' 
          WRITE(LU,*) ' WATER DEPTH                           ' 
          WRITE(LU,*) '***************************************' 
        ENDIF 
        CALL PLANTE(1) 
        STOP 
! 
!----------------------------------------------------------------------- 
!     FINITE DEPTH 
!----------------------------------------------------------------------- 
! 
      ELSE 
! 
!     ------------------------------------------------------------------ 
!        ... CARTESIAN COORDINATES 
!     ------------------------------------------------------------------ 
! 
      IF (.NOT.SPHE) THEN 
! 
!     DIFFRACTION IS TAKEN INTO ACCOUNT 
!     
!     CCG VECTOR COMPUTATION 
!       
      DO IPOIN=1,NPOIN2  
        CCG(IPOIN) = CG(IPOIN,IFF)*DEUPI*FREQ(IFF)/XK(IPOIN,IFF) 
        XKONPT(IPOIN)=1.D0/(XK(IPOIN,IFF)**2)  
        SQRCCG(IPOIN)=SQRT(ABS(CCG(IPOIN))) 
      ENDDO 
! 
!     LOOP OVER THE DIRECTIONS 
!      
      DO 220 IP = 1,NPLAN 
!.....COMPUTATION OF LOCAL AMPLITUDES OF DIRECTIONAL SPECTRA 
!     """""""""""""""""""""""""""""""""""""""""""""""""""""" 
        DO IPOIN = 1,NPOIN2 
          A(IPOIN) = (2.D0*(F(IPOIN,IP,IFF) * DFREQ(IFF)* 
     &               DEUPI/NPLAN))**(0.5D0) 
          IF(DIFFRA.EQ.2)THEN 
            S_RMSE_IN=XK(IPOIN,IFF)*SQRCCG(IPOIN) 
            A_RMSE(IPOIN) =A(IPOIN)*S_RMSE_IN 
            A(IPOIN)=A_RMSE(IPOIN) 
          ENDIF 
        ENDDO 
! 
!Filtering the local amplitudes of directional spectra 
        IF(FLTDIF) CALL FILT_SA 
! 
!       CALL GRAD_ALFA
!       THIS WAS GRAD_ALFA
!       DERIVEES EN X
      CALL VECTOR(ST1,'=','GRADF          X',IELM2,1.D0,SA,
     * ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0)
      CALL VECTOR(ST4,'=','GRADF          X',IELM2,1.D0,MESH%X,
     * ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0)
      CALL OV('X=Y/Z   ',SDDX%R,ST1%R,ST4%R,0.D0,NPOIN2)
!      DERIVEES EN Y
      CALL VECTOR(ST1,'=','GRADF          Y',IELM2,1.D0,SA,
     * ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0)
      CALL VECTOR(ST4,'=','GRADF          Y',IELM2,1.D0,MESH%Y,
     * ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0)
      CALL OV('X=Y/Z   ',SDDY%R,ST1%R,ST4%R,0.D0,NPOIN2)
!       END OF GRAD_ALFA 
       	FRDA(:,1)=DDX 
       	FRDA(:,2)=DDY      	 
!DIFFRA=1 - Mean Slope Equation model 
!DIFFRA=2 - Revised Mean Slope Equation model 
        IF(DIFFRA.EQ.1)THEN 
!         CALL GRAD_CCG
!         THIS WAS GRAD_CCG
!         DERIVEES EN X
      CALL VECTOR(ST1,'=','GRADF          X',IELM2,1.D0,SCCG,
     * ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0)
      CALL VECTOR(ST4,'=','GRADF          X',IELM2,1.D0,MESH%X,
     * ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0)
      CALL OV('X=Y/Z   ',SDDX%R,ST1%R,ST4%R,0.D0,NPOIN2)
!        DERIVEES EN Y
      CALL VECTOR(ST1,'=','GRADF          Y',IELM2,1.D0,SCCG,
     * ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0)
      CALL VECTOR(ST4,'=','GRADF          Y',IELM2,1.D0,MESH%Y,
     * ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0)
      CALL OV('X=Y/Z   ',SDDY%R,ST1%R,ST4%R,0.D0,NPOIN2)
!         END OF GRAD_CCG 
        ELSE 
!         CALL GRAD_KON
!         THIS WAS GRAD_KON
!         DERIVEES EN X
      CALL VECTOR(ST1,'=','GRADF          X',IELM2,1.D0,SXKONPT,
     * ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0)
      CALL VECTOR(ST4,'=','GRADF          X',IELM2,1.D0,MESH%X,
     * ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0)
      CALL OV('X=Y/Z   ',SDDX%R,ST1%R,ST4%R,0.D0,NPOIN2)
!      DERIVEES EN Y
      CALL VECTOR(ST1,'=','GRADF          Y',IELM2,1.D0,SXKONPT,
     * ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0)
      CALL VECTOR(ST4,'=','GRADF          Y',IELM2,1.D0,MESH%Y,
     * ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0)
      CALL OV('X=Y/Z   ',SDDY%R,ST1%R,ST4%R,0.D0,NPOIN2)
!         END OF GRAD_KON 
        ENDIF  
       	FRDK(:,1)=DDX         
       	FRDK(:,2)=DDY     
! 
        DO IPOIN = 1,NPOIN2 
! 
!   calculate first and second derivative of A ( FFD=A)         
          CALL RPI_INTR (NEIGB,NB_CLOSE, 
     &      RK(1,IPOIN),RX(1,IPOIN),RY(1,IPOIN), 
     &      RXX(1,IPOIN),RYY(1,IPOIN), 
     &      NPOIN2,IPOIN,MAXNSP,A ,FRDA,SCDA,.FALSE.,.TRUE.) 
! 
!DIFFRA=1 - Mean Slope Equation model 
!DIFFRA=2 - Revised Mean Slope Equation model 
          IF(DIFFRA.EQ.1)THEN 
            DIV(IPOIN)=CCG(IPOIN)*SCDA(IPOIN,3) 
     &              + FRDK(IPOIN,1)*FRDA(IPOIN,1) 
     &              + FRDK(IPOIN,2)*FRDA(IPOIN,2) 
          ELSE 
            DIV(IPOIN)=XKONPT(IPOIN)*SCDA(IPOIN,3) 
     &              + FRDK(IPOIN,1)*FRDA(IPOIN,1) 
     &              + FRDK(IPOIN,2)*FRDA(IPOIN,2) 
          ENDIF 
! 
          IF (ABS(DIV(IPOIN)).LE.1.E-20) DIV(IPOIN)=0. 
        ENDDO 
! Calculating Delta=div/A       
        DO IPOIN = 1,NPOIN2 
            L_DELTA(IPOIN)=.TRUE. 
            IF(F(IPOIN,IP,IFF).LE.EPS) THEN 
              DELTA(IPOIN) = 0.D0 
              L_DELTA(IPOIN)=.FALSE.   
              SQRDELTA(IPOIN) =1.D0 
            ELSE 
!DIFFRA=1 - Mean Slope Equation model 
!DIFFRA=2 - Revised Mean Slope Equation model 
              IF(DIFFRA.EQ.1) THEN 
                DELTA(IPOIN)=DIV(IPOIN)*XKONPT(IPOIN)/ 
     &		     (CCG(IPOIN)*A(IPOIN)) 
              ELSE 
                DELTA(IPOIN)=(DIV(IPOIN)/A(IPOIN)) 
              ENDIF 
! 
              IF( DELTA(IPOIN).LE.-1.D0) THEN 
                SQRDELTA(IPOIN) =1.D0 
                L_DELTA(IPOIN)=.FALSE. 
                DELTA(IPOIN)= 0.D0  
              ELSE 
                SQRDELTA(IPOIN) = SQRT(1.D0+DELTA(IPOIN)) 
                L_DELTA(IPOIN)=.TRUE. 
              ENDIF 
! 
              IF ((SQRDELTA(IPOIN)).LE.EPS) THEN  
                SQRDELTA(IPOIN) =1.D0  
                L_DELTA(IPOIN)=.FALSE. 
                DELTA(IPOIN)= 0.D0  
              ENDIF 
           ENDIF 
        ENDDO 
! 
        DO I = 1,NPTFR 
          IPOIN = NBOR(I) 
          L_DELTA(IPOIN)=.FALSE. 
          SQRDELTA(IPOIN) =1.D0 
          DELTA(IPOIN)= 0.D0  
        ENDDO 
! 
!.....DELTA GRADIENT COMPUTATION 
!     """""""""""""""""""""""""""        
!     CALL GRAD_D
!     THIS WAS GRAD_D :  
!.....DERIVEES EN X
      CALL VECTOR(ST1,'=','GRADF          X',IELM2,1.D0,SDELTA,
     * ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0)
      CALL VECTOR(ST4,'=','GRADF          X',IELM2,1.D0,MESH%X,
     * ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0)
      CALL OV('X=Y/Z   ',SDDX%R,ST1%R,ST4%R,0.D0,NPOIN2)
!.....DERIVEES EN Y
      CALL VECTOR(ST1,'=','GRADF          Y',IELM2,1.D0,SDELTA,
     * ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0)
      CALL VECTOR(ST4,'=','GRADF          Y',IELM2,1.D0,MESH%Y,
     * ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0)
      CALL OV('X=Y/Z   ',SDDY%R,ST1%R,ST4%R,0.D0,NPOIN2) 
!     END OF GRAD_D 
!
!     calculation of CG_n =CG(1+delta)^0.5 
!     and of transfer rates Cx,Cy,Ctheta 
        DO IPOIN=1,NPOIN2 
          IF(L_DELTA(IPOIN)) THEN 
            DELTAN = -SINTET(IP)*DDY(IPOIN)+COSTET(IP)*DDX(IPOIN) 
            CDELTA = CG(IPOIN,IFF)/SQRDELTA(IPOIN)/2.D0     
            WJUNK=CDELTA*DELTAN 
          ELSE   
            SQRDELTA(IPOIN) =1.0            
            WJUNK=0.D0      
          ENDIF 
! 
          DDDN=-SINTET(IP)*DZY(IPOIN)+COSTET(IP)*DZX(IPOIN)           
          DEUKD=2.D0*XK(IPOIN,IFF)*DEPTH(IPOIN)	 
          IF (DEUKD.GT.7.D2) THEN 
            DSDNSK=0.D0 
          ELSE 
            DSDNSK=DEUPI*FREQ(IFF)*SQRDELTA(IPOIN)/SINH(DEUKD)          
          ENDIF           
!    
          WJUNK2=DSDNSK*DDDN           
          CT(IPOIN,IP)=-WJUNK2-WJUNK 
          CX(IPOIN,IP)=CG(IPOIN,IFF)*SQRDELTA(IPOIN)*SINTET(IP) 
          CY(IPOIN,IP)=CG(IPOIN,IFF)*SQRDELTA(IPOIN)*COSTET(IP)        
! 
        ENDDO   
! 
220   CONTINUE   
! 
!     ---------------------------------------------------------------- 
!       ... AND SPHERICAL COORDINATES 
!     ---------------------------------------------------------------- 
! 
      ELSE 
           IF(LNG.EQ.1) THEN 
             WRITE(LU,*) '' 
             WRITE(LU,*) '***************************************' 
             WRITE(LU,*) ' ATTENTION : LA VERSION ACTUELLE DE    ' 
             WRITE(LU,*) ' TOMAWAC NE PEUT PAS SIMULER LA        ' 
             WRITE(LU,*) ' DIFFRACTION AVEC LES COORDONNES       ' 
             WRITE(LU,*) ' SPHERIQUES                            ' 
             WRITE(LU,*) '***************************************' 
           ELSE 
             WRITE(LU,*) '' 
             WRITE(LU,*) '***************************************' 
             WRITE(LU,*) ' ATTENTION : THE PRESENT VERSION OF    ' 
             WRITE(LU,*) ' TOMAWAC CANNOT SIMULATE DIFFRACTION   ' 
             WRITE(LU,*) ' WHEN SPHERICAL COORDINATES ARE SET    ' 
             WRITE(LU,*) '***************************************' 
           ENDIF 
           CALL PLANTE(0) 
           STOP 
! 
! ENDIF (Finite depth) 
      ENDIF 
!ENDIF (Cartesian coordinates) 
      ENDIF 
!----------------------------------------------------------------------- 
! 
      RETURN 
      END 
