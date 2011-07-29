!                       *****************
                        SUBROUTINE PENTCO
!                       *****************
     &(II)   
!
!***********************************************************************
!  ARTEMIS V6P1                                       31/05/11    
!***********************************************************************
!
!
!brief FUNCTION:   CALCULATE SECOND ORDER BOTTOM EFFECTS (GRADIENT&CURVATURE) 
!+                  FOR EXTENDED MILD-SLOPE EQUATION
!code
!+        OUTPUT :      
!+        1 + F = 1 + E1(KH)*grad(H)**2 + E2(KH)/K0*LAPLACIEN(H)
!+
!+      WE CAN CHOOSE TO ONTEGRATE ONLY GRADIANT EFFECTS	      : II=1
!+      WE CAN CHOOSE TO ONTEGRATE ONLY CURVATURE EFFECTS             : II=2
!+      WE CAN CHOOSE TO INTEGRATE BOTH GRADIANT & CURVATURE EFFECTS  : II=3
!+      DEFAULT VALUE  : IPENTCO=0, ARTEMIS SOLVE CLASSICAL MILD-SLOPE EQUATION
!+
!+
!+     EXPRESSIONS FOR E1 and  E2 USED HERE ARE (Chamberlain & Porter 1995) :
!+
!+     (given X = 2 KH)
!+     
!+              ( X**4 + 4 X**3 SH(X) - 9 SH(X)SH(2X) + 3 X (X+2SH(X))*(CH(X)**2-2CH(X)+3) )
!+     E1(KH) = -----------------------------------------------------------------------------
!+                                   3 ( X+SH(X) )**4  
!+
!+
!+
!+
!+                         (TH(X)-X)* CH(X)
!+    E2(KH)/K0 = 2 H * ------------------------
!+                         X ( SH(X) + X )**2
!+
!+      K0 IS THE WAVE NUMBER FOR INFINITE DEPTH : K0 = K TH(KH)
!+      H IS THE WATER DEPTH
!+                                           ------
!+
!+  USING SUBROUTINE BERKHO NOTATIONS, 
!+  AFTER VARIATIONAL FORMULATION :
!+           /
!+ AM1 =    / C*CG * GRAD(PSII)*GRAD(PSIJ) DS
!+         /S
!+
!+           /
!+       -  / OMEGA**2 * CG/C * (1+F) * PSII*PSIJ  DS
!+         /S
!+
!+           /
!+       -  /  BPHIRB * PSII*PSIJ  DB
!+         /B
!+
!+
!+ THE SECOND MEMEBER (DIFFUSION) IS MODIFIED
!+
!history C.PEYRARD & E.RAZAFINDRAKOTO
!+        31/05/11
!+        V6P1
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|    II          |-->|  OPTION FOR GRADIENT AND CURVATURE EFFECTS  
!|    T2          |---|  WORK TABLE
!|    T4          |---|  WORK TABLE 
!|    T5          |---|  WORK TABLE 
!|    T6          |---|  WORK TABLE 
!|    T7          |---|  WORK TABLE 
!|    T9          |---|  WORK TABLE 
!|    T8          |---|  WORK TABLE 
!|    T11         |---|  WORK TABLE 
!|    T12         |---|  WORK TABLE    
!|    T3          |<--|  OUTPUT WORK TABLE WITH CORRECTION TERMS FOR 
!|                |   |  GRADIENT AND CURVATURE EFFECTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! APPELE PAR                   :  BERKHO
! SOUS-PROGRAMMES APPELES      :  FCTE1 et FCTE2
! TABLEAUX DE TRAVAIL UTILISES :  T2 T3 T4 T5 T6 T7 T9 T8 T11 T12
!
!-----------------------------------------------------------------------
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: II      
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION CBID,FFW
      DOUBLE PRECISION PI,DEGRAD,RADDEG
!
!-----------------------------------------------------------------------
!
      PARAMETER( PI = 3.1415926535897932384626433D0 , DEGRAD=PI/180.D0 )
      PARAMETER( RADDEG = 180.D0 / PI )
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION FCTE1, FCTE2, XX
      EXTERNAL FCTE1, FCTE2
!
! MASS MATRIX
!
      CALL VECTOR(T2 , '=' , 'MASBAS          ' , IELM ,
     *            1.D0 , C , C , C , C , C , C ,
     *            MESH , MSK  , MASKEL )
!
      IF(II.EQ.1.OR.II.EQ.3) THEN
! --------------
! GRADIENT EFFECTS
! --------------
!
!--->  E1*GRAD(H)**2 ---> IN T4
!
!  DX
!
      CALL VECTOR(T7 , '=' , 'GRADF          X' , IELM ,
     *            1.D0 , H , T1 , T1 , T1 , T1 , T1 ,
     *            MESH , MSK , MASKEL)
      CALL OS( 'X=YZ    ' , T4,T7,T7,0.D0) 
!
!  DY 
!       
      CALL VECTOR(T7 , '=' , 'GRADF          Y' , IELM ,
     *            1.D0 , H , T1 , T1 , T1 , T1 , T1 ,
     *            MESH , MSK , MASKEL)
      CALL OS( 'X=YZ    ' , T5,T7,T7,0.D0)
!
! GRAD(H)**2  
!       
      CALL OS( 'X=X+Y   ', T4,T5,SBID,0.D0)
!
! SQUARE MASS
!
      CALL OS( 'X=YZ    ' , T8,T2,T2,0.D0)
!
! COEFF GRAD(H)**2 
!     
      CALL OS( 'X=Y/Z   ' , T4,T4,T8,0.D0)     
!           
!---> FUNCTION E1   
!                                                                         
      DO I=1,NPOIN
        T11%R(I)=FCTE1(  K%R(I)*H%R(I) )
      END DO
!    
!--->  E1*GRAD(H)**2 ---> IN T4
!
      CALL OS( 'X=YZ    ' , T4,T4,T11,0.D0)
! END OF GRADIENT EFFECTS
      ENDIF
!
      IF(II.EQ.2.OR.II.EQ.3) THEN
! ------------------
! CURVATURE EFFECTS
! ------------------
!---> E2/K0*LAPLACIAN(H)   ---> IN T9
!  DX
      CALL VECTOR(T7 , '=' , 'GRADF          X' , IELM ,
     *            1.D0 , H , T1 , T1 , T1 , T1 , T1 ,
     *            MESH , MSK , MASKEL)
      CALL OS( 'X=Y/Z    ' , T7,T7,T2,0.D0)   

      CALL VECTOR(T5 , '=' , 'GRADF          X' , IELM ,
     *            1.D0 , T7 , T1 , T1 , T1 , T1 , T1 ,
     *            MESH , MSK , MASKEL)
!
! COEFF DX
!
      CALL OS( 'X=Y/Z    ' , T5,T5,T2,0.D0)       
!      
!  DY
!
      CALL VECTOR(T7 , '=' , 'GRADF          Y' , IELM ,
     *            1.D0 , H , T1 , T1 , T1 , T1 , T1 ,
     *            MESH , MSK , MASKEL)
      CALL OS( 'X=Y/Z    ' , T7,T7,T2,0.D0) 
!       
      CALL VECTOR(T6 , '=' , 'GRADF          Y' , IELM ,
     *            1.D0 , T7 , T1 , T1 , T1 , T1 , T1 ,
     *            MESH , MSK , MASKEL)
!
! COEFF DY
!
      CALL OS( 'X=Y/Z    ' , T6,T6,T2,0.D0)   
!
      CALL OS( 'X=Y+Z   ', T9,T5,T6,0.D0)
!
!---> FUNCTION E2 * 2 H 
!
      DO I=1,NPOIN
       T12%R(I)=2.*H%R(I) * FCTE2(K%R(I)*H%R(I))
      END DO
!      
!---> E2/K0*LAPLACIAN(H)
!
      CALL OS( 'X=YZ    ' , T9,T9,T12,0.D0)
!        
! END OF CURVATURE EFFECTS
      ENDIF
!
! SUM OF GRADIENT AND CURVTURE EFFECTS, DEPENDING OF OPTION "IPENTCO"
!
      IF(II.EQ.1)  THEN
!      F= E1*GRAD(H)**2      
       CALL OS( 'X=Y      ' , T3,T4,SBID,0.D0)
      ENDIF
!
      IF(II.EQ.2)  THEN
!      F= E2/K0*LAPLACIAN(H)  
       CALL OS( 'X=Y      ' , T3,T9,SBID,0.D0)
      ENDIF
!      
      IF(II.EQ.3)  THEN
!      F =  E1*grad(H)**2 + E2/K0*LAPLACIEN(H)
       CALL OS( 'X=Y+Z    ' , T3,T4,T9,0.D0)
      ENDIF
!
! ADD 1.,  T3 = 1 + F
!
      CALL OS( 'X=X+C   ', T3,SBID,SBID,1.D0)
!
!-----------------------------------------------------------------------
!
      RETURN
      END

