!                       ***************** 
                        SUBROUTINE PREDIF 
!                       ***************** 
! 
     &(CX    , CY    , CT    , DT    , X     , Y     , TETA  , COSTET, 
     & SINTET, FREQ  , IKLE2 , IFABOR, ETAP1 , TRA01 , SHP1  , SHP2  ,  
     & SHP3  , SHZ   , ELT   , ETA   , DEPTH , DZX   , DZY   , XK    ,  
     & CG    , ITR01 , NPOIN3, NPOIN2, NELEM2, NPLAN , NF    , SURDET,  
     & COURAN, SPHE  , PROINF, A     , DFREQ , F     , CCG   , DIV   ,  
     & DELTA , DDX   , DDY   , EPS   , NBOR  , NPTFR , XKONPT, RK    ,  
     & RX    , RY    , RXX   , RYY   , NEIGB , NB_CLOSE,DIFFRA,MAXNSP,  
     & FLTDIF )  
! 
!*********************************************************************** 
! TOMAWAC   V6P2                                   25/06/2012 
!*********************************************************************** 
! 
!brief    PREPARES DIFFRACTION. 
!+ 
!+            COMPUTES THE ADVECTION FIELD; TRACES BACK THE 
!+                CHARACTERISTICS. 
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
!+   Taking into account both Mean Sloe Equation model (Berkhoff,1972) 
!+      and Revised Mild Slope Equation model (Porter,2003) 
! 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
!| A              |<--| AMPLITUDE OF DIRECTIONAL SPECTRUM 
!| CCG            |<--| GROUP VELOCITY TIMES PHASE VELOCITY 
!| CG             |-->| DISCRETIZED GROUP VELOCITY 
!| COSTET         |-->| COSINE OF TETA ANGLE 
!| COURAN         |-->| LOGICAL INDICATING IF THERE IS A CURRENT 
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
!| DT             |-->| TIME STEP 
!| DZX            |-->| BOTTOM SLOPE ALONG X 
!| DZY            |-->| BOTTOM SLOPE ALONG Y 
!| ELT            |<->| NUMBERS OF THE ELEMENTS 2D OF THE 
!|                |   | POINTS TO BE ADVECTED 
!| EPS            |-->| VARIANCE THRESHOLD FOR DIFFRACTION 
!| ETA            |<->| NUMBERS OF THE LAYERS OF THE 
!|                |   | POINTS TO BE ADVECTED 
!| ETAP1          |<->| HIGHER LAYERS TABLE 
!| F              |<->| VARIANCE DENSITY DIRECTIONAL SPECTRUM 
!| FLTDIF         |-->| IF TRUE, LOCAL AMPLITUDES ARE FILTERED 
!| FREQ           |-->| DISCRETIZED FREQUENCIES 
!| IFABOR         |-->| ELEMENTS BEHIND THE EDGES OF A TRIANGLE 
!|                |   | IF NEGATIVE OR ZERO, THE EDGE IS A LIQUID, 
!|                |   | SOLID OR PERIODIC BOUNDARY 
!| IKLE2          |-->| TRANSITION BETWEEN LOCAL AND GLOBAL NUMBERING 
!|                |   | OF THE 2D MESH 
!| ITR01          |<->| WORK TABLE 
!| MAXNSP         |-->| CONSTANT FOR MESHFREE TECHNIQUE 
!| NB_CLOSE       |-->| ARRAY USED IN THE MESHFREE TECHNIQUE 
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS 
!| NEIGB          |-->| NEIGHBOUR POINTS FOR MESHFREE METHOD 
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D MESH 
!| NF             |-->| NUMBER OF FREQUENCIES 
!| NPLAN          |-->| NUMBER OF DIRECTIONS 
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH 
!| NPOIN3         |-->| NPOIN2*NPLAN 
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS 
!| PROINF         |-->| LOGICAL INDICATING INFINITE DEPTH ASSUMPTION 
!| RK             |-->| ARRAY USED IN THE MESHFREE TECHNIQUE 
!| RX             |-->| ARRAY USED IN THE MESHFREE TECHNIQUE 
!| RXX            |-->| ARRAY USED IN THE MESHFREE TECHNIQUE 
!| RY             |-->| ARRAY USED IN THE MESHFREE TECHNIQUE 
!| RYY            |-->| ARRAY USED IN THE MESHFREE TECHNIQUE 
!| SHP1,SHP2,SHP3 |<->| BARYCENTRIC COORDINATES OF THE NODES IN 
!|                |   | THEIR ASSOCIATED 2D ELEMENT "ELT" 
!| SHZ            |<->| BARYCENTRIC COORDINATES ALONG TETA OF THE  
!|                |   | NODES IN THEIR ASSOCIATED LAYER "ETA" 
!| SINTET         |-->| SINE OF TETA ANGLE 
!| SPHE           |-->| LOGICAL INDICATING SPHERICAL COORD ASSUMPTION 
!| SURDET         |-->| 1/DET. OF ELEMENTS 2D FOR ISOPARAM. TRANSF. 
!| TETA           |-->| DISCRETIZED DIRECTIONS 
!| TRA01          |<->| WORK TABLE 
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH 
!| XK             |-->| DISCRETIZED WAVE NUMBER 
!| XKONPT         |<--| ARRAY USED FOR COMPUTING DIFFRACTION PARAMETER 
!| Y              |-->| ORDINATES OF POINTS IN THE MESH 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
! 
      IMPLICIT NONE 
! 
      INTEGER LNG,LU 
      COMMON/INFO/ LNG,LU 
! 
!.....VARIABLES IN ARGUMENT 
!     """"""""""""""""""""" 
      INTEGER NPOIN3,NPOIN2,NELEM2,NPLAN,NF,NPTFR,MAXNSP 
      INTEGER NBOR(NPTFR) , DIFFRA 
      INTEGER ITR01(NPOIN3,3) 
      INTEGER ELT(NPOIN3,NF),ETA(NPOIN3,NF) 
      INTEGER IKLE2(NELEM2,3),IFABOR(NELEM2,7),ETAP1(NPLAN) 
      INTEGER NB_CLOSE(NPOIN2),NEIGB(NPOIN2,MAXNSP) 
      DOUBLE PRECISION CX(NPOIN3,NF),CY(NPOIN3,NF) 
      DOUBLE PRECISION CT(NPOIN3,NF) 
      DOUBLE PRECISION RK(MAXNSP,NPOIN2) 
      DOUBLE PRECISION RX(MAXNSP,NPOIN2), RY(MAXNSP,NPOIN2)   
      DOUBLE PRECISION RXX(MAXNSP,NPOIN2),RYY(MAXNSP,NPOIN2) 
      DOUBLE PRECISION SHP1(NPOIN3,NF) , SHP2(NPOIN3,NF) 
      DOUBLE PRECISION SHP3(NPOIN3,NF) , SHZ(NPOIN3,NF) 
      DOUBLE PRECISION X(NPOIN2),Y(NPOIN2) 
      DOUBLE PRECISION XK(NPOIN2,NF),CG(NPOIN2,NF) 
      DOUBLE PRECISION TETA(NPLAN),FREQ(NF) 
      DOUBLE PRECISION SINTET(NPLAN),COSTET(NPLAN) 
      DOUBLE PRECISION SURDET(NELEM2) 
      DOUBLE PRECISION DT, EPS 
      DOUBLE PRECISION TRA01(NPOIN3,8) 
      DOUBLE PRECISION DEPTH(NPOIN2),DZX(NPOIN2),DZY(NPOIN2) 
      DOUBLE PRECISION DDX(NPOIN2),DDY(NPOIN2), A(NPOIN2) 
      DOUBLE PRECISION DFREQ(NF)       
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF) 
      DOUBLE PRECISION CCG(NPOIN2),DIV(NPOIN2) 
      DOUBLE PRECISION DELTA(NPOIN2), XKONPT(NPOIN2) 
      LOGICAL COURAN,SPHE,PROINF, FLTDIF 
! 
!.....LOCAL VARIABLES 
!     """"""""""""""" 
      INTEGER IFF 
      INTEGER,DIMENSION(:,:,:), ALLOCATABLE :: GOODELT 
! 
!---------------------------------------------------------------------- 
! 
! 
      IF (.NOT.COURAN) THEN 
! 
!   ------------------------------------------------------------------- 
!    
!   RELATIVE = ABSOLUTE => ADVECTION IN 3D 
!   SEPARATES OUT THE FREQUENCIES 
! 
        ALLOCATE(GOODELT(NPOIN2,NPLAN,NF)) 
        DO 200 IFF=1,NF 
! 
! 
!      --------------------------------------------------------------- 
! 
!      COMPUTES THE ADVECTION FIELD 
! 
         CALL DIFFRAC 
     &  (CX,CY,CT,XK,CG,DEPTH,DZX,DZY,FREQ,COSTET,SINTET, 
     &   NPOIN2,NPLAN,IFF,NF,PROINF,SPHE,A,DFREQ, 
     &   F,CCG,DIV,DELTA,DDX,DDY,EPS,NBOR,NPTFR, XKONPT, 
     &   RK,RX,RY,RXX,RYY,NEIGB,NB_CLOSE, DIFFRA, MAXNSP, FLTDIF )  
! 
!      ---------------------------------------------------------------- 
! 
!      DETERMINES THE FOOT OF THE CHARACTERISTICS 
! 
         CALL INIPIE 
     &  (CX,CY,CT,X,Y,SHP1(1,IFF),SHP2(1,IFF),SHP3(1,IFF),SHZ(1,IFF), 
     &   ELT(1,IFF),ETA(1,IFF), 
     &   TRA01,TRA01(1,2),TRA01(1,3),TETA,IKLE2,NPOIN2,NELEM2,NPLAN, 
     &       ITR01,ITR01,ITR01,NELEM2,NPOIN2,IFABOR,GOODELT(1,1,IFF)) 
! 
         CALL MPOINT 
     &  (CX,CY,CT, 
     &   DT,X,Y,TETA,IKLE2,IFABOR,ETAP1,TRA01,TRA01(1,2), 
     &   TRA01(1,3),TRA01(1,4),TRA01(1,5),TRA01(1,6),SHP1(1,IFF), 
     &   SHP2(1,IFF),SHP3(1,IFF),SHZ(1,IFF),ELT(1,IFF),ETA(1,IFF), 
     &   ITR01(1,1),NPOIN3, 
     &   NPOIN2,NELEM2,NPLAN,IFF,SURDET,-1,ITR01(1,2)) 
! 
! 
200    CONTINUE 
       DEALLOCATE(GOODELT) 
! 
      ELSE 
! 
!   --------------------------------------------------------------- 
!     
!   IN A RELATIVE REFERENCE SYSTEM => ADVECTION IN 4D 
!   IT IS NO LONGER POSSIBLE TO SEPARATE THE FREQUENCIES OUT 
           IF(LNG.EQ.1) THEN 
             WRITE(LU,*) '' 
             WRITE(LU,*) '***************************************' 
             WRITE(LU,*) ' ATTENTION : LA DIFFRACTION N''EST PAS ' 
             WRITE(LU,*) ' PRISE EN COMPTE EN PRESENCE D''UN     ' 
             WRITE(LU,*) ' COURANT OU D''UNE HAUTEUR D''EAU      ' 
             WRITE(LU,*) ' VARIABLE                              ' 
             WRITE(LU,*) '***************************************' 
           ELSE 
             WRITE(LU,*) '' 
             WRITE(LU,*) '***************************************' 
             WRITE(LU,*) ' ATTENTION : DIFFRACTION IS NOT TAKEN  ' 
             WRITE(LU,*) ' INTO ACCOUNT IF CURRENTS OF VARYING   ' 
             WRITE(LU,*) ' WATER LEVELS ARE CONSIDERED           ' 
             WRITE(LU,*) '***************************************' 
           ENDIF 
           CALL PLANTE(0) 
           STOP 
! 
       ENDIF 
! 
!---------------------------------------------------------------------- 
! 
      RETURN 
      END 
