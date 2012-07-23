!                       ******************* 
                        SUBROUTINE RPI_INTR 
!             		******************* 
     &( NEIGB , NB_CLOSE, RK  , RX    , RY    , RXX   , RYY   , 
     &  NPOIN2, I     , MAXNSP, FFD   , FIRDIV, SECDIV, FRSTDIV, 
     &  SCNDDIV) 
! 
!*********************************************************************** 
! TOMAWAC   V6P2                                   25/06/2012 
!*********************************************************************** 
! 
!brief    FREE-MESH METHOD FOR DIFFRACTION COMPUTATION 
!+ 
!+            CALCULATES FIRST AND SECOND DERIVATIVE OF 
!+            VARIABLE FFD 
! 
!history  E. KRIEZI (LNH) 
!+        04/12/2006 
!+        V5P5 
!+ 
! 
!history  G.MATTAROLO (EDF - LNHE) 
!+        23/10/2011 
!+        V6P1 
!+   Translation of French names of the variables in argument 
! 
!history  G.MATTAROLO (EDF - LNHE) 
!+        23/06/2012 
!+        V6P2 
!+   Modification for V6P2 
! 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
!| FFD            |-->| INPUT FIELD FUNCTION 
!| FIRDIV         |<--| FIRST DERIVATIVE OF FFD 
!| FRSTDIV        |-->| IF TRUE COMPUTES 1ST DERIVATIVE 
!| I              |-->| POINT INDEX 
!| MAXNSP         |-->| CONSTANT FOR MESHFREE TECHNIQUE 
!| NB_CLOSE       |-->| ARRAY USED IN THE MESHFREE TECHNIQUE 
!| NEIGB          |-->| NEIGHBOUR POINTS FOR MESHFREE METHOD 
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH 
!| RK             |-->| ARRAY USED IN THE MESHFREE TECHNIQUE 
!| RX             |-->| ARRAY USED IN THE MESHFREE TECHNIQUE 
!| RXX            |-->| ARRAY USED IN THE MESHFREE TECHNIQUE 
!| RY             |-->| ARRAY USED IN THE MESHFREE TECHNIQUE 
!| RYY            |-->| ARRAY USED IN THE MESHFREE TECHNIQUE 
!| SECDIV         |<--| SECOND DERIVATIVE OF FFD 
!| SCNDDIV        |-->| IF TRUE COMPUTES 2ND DERIVATIVE 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
! 
      IMPLICIT NONE 
! 
!.....VARIABLES IN ARGUMENT 
!     """""""""""""""""""" 
      INTEGER NPOIN2, MAXNSP, I 
      INTEGER NEIGB(NPOIN2,MAXNSP), NB_CLOSE(NPOIN2) 
 
      DOUBLE PRECISION RK(MAXNSP) 
      DOUBLE PRECISION RX(MAXNSP), RY(MAXNSP)   
      DOUBLE PRECISION RXX(MAXNSP), RYY(MAXNSP) 
      DOUBLE PRECISION FIRDIV(NPOIN2,2), SECDIV(NPOIN2,3) 
      DOUBLE PRECISION FFD(NPOIN2) 
       
      LOGICAL FRSTDIV, SCNDDIV 
! 
!.....LOCAL VARIABLES  
!     """"""""""""""" 
      INTEGER IP, IPOIN, IP1, IPOIN1, NDER 
 
      DOUBLE PRECISION WU_OM(MAXNSP), ZETA(NPOIN2) 
      DOUBLE PRECISION WZ,WZX1,WZY1,WZX2,WZY2 
 
      LOGICAL FFUNC 
!       FFD the field function where data are coming from.  
!************************************************************************ 
! 
       DO IP1 =1,NB_CLOSE(I) 
          IPOIN=NEIGB(I,IP1) 
          WU_OM(IP1)=FFD(IPOIN) 
       ENDDO 
!   
!     Calculate derivatives in IPOIN         
! 
       IF(FRSTDIV) THEN       !C   first derivative 
         WZX1=0.D0 
         WZY1=0.D0 
         DO IP1 =1,NB_CLOSE(I) 
           WZX1=WZX1+RX(IP1)*WU_OM(IP1) 
           WZY1=WZY1+RY(IP1)*WU_OM(IP1)           
         ENDDO  
         FIRDIV(I,1)= WZX1 
         FIRDIV(I,2)= WZY1 
       ENDIF 
 
       IF(SCNDDIV) THEN       !C   second derivative 
         WZX2=0.D0 
         WZY2=0.D0 
         DO IP1 =1,NB_CLOSE(I)     
          WZX2=WZX2+RXX(IP1)*WU_OM(IP1) 
          WZY2=WZY2+RYY(IP1)*WU_OM(IP1)          
         ENDDO    
         SECDIV(I,1)= WZX2 
         SECDIV(I,2)= WZY2 
         SECDIV(I,3)= WZX2+WZY2     
       ENDIF 
! 
! in case we want the estimation of the FFunction : test version only 
!       IF(.FFUNC) THEN        
!         WZ=0.D0  
!         DO IPOIN1 =1,NB_CLOSE(I) 
!           WZ=WZ+RK(IPOIN1)*WU_OM(IPOIN1) 
!         ENDDO  
!         ZETA(I)= WZ   
!       ENDIF 
 
  
      RETURN 
      END                   
