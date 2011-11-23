!                    *****************************
                     SUBROUTINE BEDLOAD_SECCURRENT
!                    *****************************
!
     &(IELMU)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES THE NEW TAU FROM SECONDARY CURRENTS.
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
!history  R. KOPMANN, U.MERKEL
!         20/03/2011
!         V6P1 
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IELMU          |-->| TYPE OF ELEMENT FOR VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SISYPHE
      USE BIEF
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: IELMU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION C, ALPHAL
!
!     RK MODIFICATION FOR SECONDARY CURRENTS
!
!     COMPUTES THE GRADIENT OF THE FREE SURFACE IN X AND Y DIRECTION
!
      CALL VECTOR(T5,'=','GRADF          X',IELMU,
     &            1.D0,Z,S,S,S,S,S,MESH,MSK,MASKEL)
      CALL VECTOR(T6,'=','GRADF          Y',IELMU,
     &            1.D0,Z,S,S,S,S,S,MESH,MSK,MASKEL)
!
!     FOR PARALLEL COMPUTING
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM (T5, 2, MESH)
        CALL PARCOM (T6, 2, MESH)
      ENDIF
!
      CALL OS('X=XY    ',X=T5,Y=UNSV2D)
      CALL OS('X=XY    ',X=T6,Y=UNSV2D)
!
!     COMPUTES THE X- AND Y-COMPONENTS OF THE SECONDARY CURRENT 
!     ACCORDING TO ENGELUNG. TAU_X_SEC = C*QV, TAU_Y_SEC = C*QU
!
! AT THE MOMENT ALPHA MUST BE SET HERE  (0.75 FOR VERY ROUGH BOTTOMS, 1 FOR SMOOTH ONES)
! BEWARE: THE VARIABLE ALPHA IS MORE THAN THE ALPHA FROM THE THEORY
! Now included as Keyword in CAS - FILE
      ALPHAL = ALPHA
!RK
      IF(ALPHA.GT.0.D0) THEN
        ALPHAL = 7.D0 / ALPHAL * XMVE *GRAV
      ELSE
        ALPHAL = 0.D0
      ENDIF
!
      CALL OS( 'X=YZ    ' , T1 , T6      , U2D   , C   ) ! DZSDY*U2D
      CALL OS( 'X=YZ    ' , T2 , T5      , V2D   , C   ) ! DZSDX*V2D
      CALL OS( 'X=-Y    ' , T2 , T2      , T3   , C   )
      CALL OS( 'X=X+Y   ' , T1 , T2      , T3   , C   ) ! U2D*DZSDY - V2D*DZSDX
! NOTE JMH : WHY NOT THE FOLLOWING LINE INSTEAD OF THE 2 PREVIOUS ???
!     CALL OS( 'X=X-Y   ',X=T1,Y=T2) ! U2D*DZSDY - V2D*DZSDX
!
      CALL OS( 'X=YZ    ' , T2 , U2D      , U2D   , C   ) ! U2D**2
      CALL OS( 'X=YZ    ' , T3 , V2D      , V2D   , C   ) ! V2D**2
      CALL OS( 'X=X+Y   ' , T2 , T3      , T3   , C   ) ! U2D**2+V2D**2
!
      CALL OS('X=Y/Z   ' , T1 , T1 , T2, C ,2 , 0.D0,1.D-12) !(U2D*DZSDY - V2D*DZSDX)/(U2D**2+V2D**2)
!
      CALL OS( 'X=CX    ' , T1 , T2      , T3   , ALPHAL   ) ! T1 * 7/ALPHAL*XMVE*GRAV
      CALL OS( 'X=XY    ' , T1 , HN      , T3   , C   ) ! T1*HN
!
!     FOR ALL ROUGHNESS LAWS
!
      CALL OS( 'X=CXY   ' , X=T1 , Y=CF , C=0.5D0 )!T1*CF/2
!
!     TAU_X_SEK = -C*V2D : T5
!     TAU_Y_SEK =  C*U2D : T6
!
      CALL OS('X=YZ    ' , T5 , T1    , V2D,  C ) ! C*V2D
      CALL OS('X=YZ    ' , T6 , T1    , U2D,  C ) ! C*U2D
      CALL OS('X=-Y    ' , T6 , T6    , QV,   C ) ! -C*U2D
!
!     SQRT(TAU_X_SEK**2+TAU_Y_SEK**2) : T3
!
      CALL OS('X=YZ    ' , T2 , T5    , T5,  C ) ! T2 = (C*V2D)**2
      CALL OS('X=YZ    ' , T3 , T6    , T6,  C ) ! T3 = (C*U2D)**2
      CALL OS('X=X+Y   ' , T2 , T3    , T3,  C ) ! T2 = (C*V2D)**2+(C*U2D)**2
      CALL OS('X=SQR(Y)' , X=T3 , Y=T2 ) ! T3 = SQRT((C*U2D)**2+(C*V2D)**2
!
!     TAU_X_GES = TOB*EFFPNT*CALFA + TAU_X_SEK : T1
!     TAU_Y_GES = TOB*EFFPNT*SALFA + TAU_Y_SEK : T2
!
      CALL OS( 'X=YZ    ' , T1 , TOB      , COEFPN   , C   ) ! TOB*EFFPNT
      CALL OS( 'X=YZ    ' , T2 , T1      ,  SALFA   , C   ) ! TOB*EFFPNT*SALFA
      CALL OS( 'X=YZ    ' , T1 , T1      , CALFA   , C   ) ! TOB*EFFPNT*CALFA
      CALL OS('X=X+Y   ' , T1 , T5    , T3,  C ) ! TAU_X_GES = TOB*CALFA+TAU_X_SEK
      CALL OS('X=X+Y   ' , T2 , T6    , T3,  C ) ! TAU_Y_GES = TOB*SALFA+TAU_Y_SEK
!
!     TAU_GES=SQRT(TAU_X_GES**2+TAU_Y_GES**2)
!
      CALL OS( 'X=YZ    ' , T3 , T1 , T1 , C   ) ! TAU_X_GES**2
      CALL OS( 'X=YZ    ' , T4 , T2 , T2 , C   ) ! TAU_Y_GES**2
      CALL OS('X=X+Y   ' , T4 , T3    , T3,  C ) ! TAU_X_GES**2+TAU_Y_GES**2
      CALL OS('X=SQR(Y)' , T4 , T4    , T3,  C ) ! SQRT(TAU_X_GES**2+TAU_Y_GES**2)
!
!     NEW ANGLE
!     CALFA_NEW = COS(TAU_X_GES/TAU_GES)
!     SALFA_NEW = SIN(TAU_Y_GES/TAU_GES)
!
      CALL OS('X=Y/Z   ' , T1 , T1 , T4, C ,2 , 0.D0,1.D-12) !TAU_X_GES/TAU_GES
      CALL OS('X=Y/Z   ' , T2 , T2 , T4, C ,2 , 0.D0,1.D-12) !TAU_Y_GES/TAU_GES
!
!     TAKEN FROM EFFPNT UEBER
!     TO MAKE SURE THAT TAU_X_GES/TAU_GES IS IN RANGE [-1,1]
!
      DO I=1,NPOIN
         IF(T1%R(I).LT.-1.D0.OR.T1%R(I).GT.1.D0.OR.
     &      T2%R(I).LT.-1.D0.OR.T2%R(I).GT.1.D0) THEN
            WRITE(LU,*) 'NOT ACCEPTABLE BORDER CROSSING',I
         ENDIF
         T1%R(I) = MIN(T1%R(I),1.D0)
         T1%R(I) = MAX(T1%R(I),-1.D0)
         T2%R(I) = MIN(T2%R(I),1.D0)
         T2%R(I) = MAX(T2%R(I),-1.D0)
      ENDDO
!
!     COEFPN_NEW = TAU_GES / TOB
!
      CALL OS('X=Y/Z   ' , COEFPN , T4 , TOB, C ,2 , 0.D0,1.D-12) !COEFPN=TAU_GES/TOB
      CALL OS( 'X=Y     ' ,X=CALFA ,Y=T1 ) ! (TAU_X_GES/TAU_GES)
      CALL OS( 'X=Y     ' ,X=SALFA ,Y=T2 ) ! (TAU_Y_GES/TAU_GES)
!
!     FROM EFFPNT
!     FOR BOUNDARY NODES WITH IMPOSED FLOW :
!     QS IS NOT MODIFIED WHEN USER-DEFINED
!
      DO I = 1 , NPTFR
        IF(LIQBOR%I(I).EQ.5) THEN
          COEFPN%R(MESH%NBOR%I(I)) = 1.D0
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
