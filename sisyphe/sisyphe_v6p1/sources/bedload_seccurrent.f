C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE NEW TAU FROM SECONDARY CURRENTS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IELMU
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_SISYPHE :<br>
!> @link DECLARATIONS_SISYPHE::CALFA CALFA@endlink, 
!> @link DECLARATIONS_SISYPHE::CF CF@endlink, 
!> @link DECLARATIONS_SISYPHE::COEFPN COEFPN@endlink, 
!> @link DECLARATIONS_SISYPHE::GRAV GRAV@endlink, 
!> @link DECLARATIONS_SISYPHE::HN HN@endlink, 
!> @link DECLARATIONS_SISYPHE::LIQBOR LIQBOR@endlink, 
!> @link DECLARATIONS_SISYPHE::MASKEL MASKEL@endlink, 
!> @link DECLARATIONS_SISYPHE::MESH MESH@endlink, 
!> @link DECLARATIONS_SISYPHE::MSK MSK@endlink, 
!> @link DECLARATIONS_SISYPHE::NPOIN NPOIN@endlink, 
!> @link DECLARATIONS_SISYPHE::NPTFR NPTFR@endlink, 
!> @link DECLARATIONS_SISYPHE::QU QU@endlink, 
!> @link DECLARATIONS_SISYPHE::QV QV@endlink, 
!> @link DECLARATIONS_SISYPHE::S S@endlink, 
!> @link DECLARATIONS_SISYPHE::SALFA SALFA@endlink, 
!> @link DECLARATIONS_SISYPHE::T1 T1@endlink, 
!> @link DECLARATIONS_SISYPHE::T2 T2@endlink, 
!> @link DECLARATIONS_SISYPHE::T3 T3@endlink, 
!> @link DECLARATIONS_SISYPHE::T4 T4@endlink, 
!> @link DECLARATIONS_SISYPHE::T5 T5@endlink, 
!> @link DECLARATIONS_SISYPHE::T6 T6@endlink, 
!> @link DECLARATIONS_SISYPHE::TOB TOB@endlink, 
!> @link DECLARATIONS_SISYPHE::XMVE XMVE@endlink, 
!> @link DECLARATIONS_SISYPHE::Z Z@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ALPHA, C, I
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), PARCOM(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BEDLOAD_FORMULA()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IELMU
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
       SUBROUTINE BEDLOAD_SECCURRENT(IELMU)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IELMU          |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
       USE DECLARATIONS_SISYPHE
       USE BIEF
       IMPLICIT NONE
!
       INTEGER LNG,LU
       COMMON/INFO/LNG,LU
!
       INTEGER I, IELMU
       DOUBLE PRECISION C, ALPHA
!
C REMEMBER: QU = U_TEL*H_TEL, QV=V_TEL*H_TEL
!
!
!
C COMPUTES PI
C       PI = ACOS(-1.D0)
!
CRK MODIFICATION FOR SECONDARY CURRENTS
C COMPUTES THE GRADIENT OF THE FREE SURFACE IN X-DIRECTION
       CALL VECTOR(T5,'=','GRADF          X',IELMU,
     &      1.D0,Z,S,S,S,S,S,MESH,MSK,MASKEL)
C FOR PARALLEL COMPUTING
       IF (NCSIZE.GT.1) CALL PARCOM (T5, 2, MESH)
C COMPUTES THE GRADIENT OF THE FREE SURFACE IN Y-DIRECTION
       CALL VECTOR(T6,'=','GRADF          Y',IELMU,
     &      1.D0,Z,S,S,S,S,S,MESH,MSK,MASKEL)
C FOR PARALLEL COMPUTING
       IF (NCSIZE.GT.1) CALL PARCOM (T6, 2, MESH)
C COMPUTES THE MASS-MATRIX
      CALL VECTOR(T4,'=','MASBAS          ',IELMU,
     &      1.D0,S,S,S,S,S,S,MESH,MSK,MASKEL)
C FOR PARALLEL COMPUTING
       IF (NCSIZE.GT.1) CALL PARCOM (T4, 2, MESH)
C FOR THE WEAK FORMULATION IN FEM, MUST BE DIVIDED BY THE MASS-MATRIX
       CALL OS ('X=Y/Z   ', T5,T5,T4,C,2,0.D0,1.D-12)
       CALL OS ('X=Y/Z   ', T6,T6,T4,C,2,0.D0,1.D-12)
!
!
C COMPUTES THE X- AND Y-COMPONENTS OF THE SECONDARY CURRENT ACCORDING TO ENGELUNG
C TAU_X_SEC = C*QV, TAU_Y_SEC = C*QU
!
C AT THE MOMENT ALPHA MUST BE SET HERE  (0.75 FOR VERY ROUGH BOTTOMS, 1 FOR SMOOTH ONES)
C BEWARE: THE VARIABLE ALPHA IS MORE THAN THE ALPHA FROM THE THEORY
      ALPHA = 1.0D0
      ALPHA = 7.D0 / ALPHA * XMVE *GRAV
C     WRITE(LU,*)'ALPHA',1.D0/ALPHA*7.D0*GRAV*XMVE
!
!
      CALL OS( 'X=YZ    ' , T1 , T6      , QU   , C   ) ! DZSDY*QU
      CALL OS( 'X=Y/Z   ' , T1 , T1      , HN   , C   ) ! DZSDY*QU/HN
      CALL OS( 'X=YZ    ' , T2 , T5      , QV   , C   ) ! DZSDX*QV
      CALL OS( 'X=Y/Z   ' , T2 , T2      , HN   , C   ) ! DZSDX*QV/HN
      CALL OS( 'X=-Y    ' , T2 , T2      , T3   , C   )
      CALL OS( 'X=X+Y   ' , T1 , T2      , T3   , C   ) ! QU*DZSDY - QV*DZSDX
!
      CALL OS( 'X=YZ    ' , T2 , QU      , QU   , C   ) ! QU**2
      CALL OS( 'X=Y/Z   ' , T2 , T2      , HN   , C   ) ! QU**2/HN
      CALL OS( 'X=Y/Z   ' , T2 , T2      , HN   , C   ) ! QU**2/HN**2
      CALL OS( 'X=YZ    ' , T3 , QV      , QV   , C   ) ! QV**2
      CALL OS( 'X=Y/Z   ' , T3 , T3      , HN   , C   ) ! QV**2/HN
      CALL OS( 'X=Y/Z   ' , T3 , T3      , HN   , C   ) ! QV**2/HN**2
      CALL OS( 'X=X+Y   ' , T2 , T3      , T3   , C   ) ! QU**2+QV**2
!
      CALL OS('X=Y/Z   ' , T1 , T1 , T2, C ,2 , 0.D0,1.D-12) !(QU*DZSDY - QV*DZSDX)/(QU**2+QV**2)
!
      CALL OS( 'X=CX    ' , T1 , T2      , T3   , ALPHA   ) ! T1 * 7/ALPHA*XMVE*GRAV
      CALL OS( 'X=XY    ' , T1 , HN      , T3   , C   ) ! T1*HN
!
C ONLY FOR STRICKLER ROUGHNESS
C T4: CHESTR AS KSTR
C      CALL OS( 'X=C     ' , T4 , T2      , T3   ,71.2D0   ) ! SET OF KSTR

C      CALL OS( 'X=XC    ' , T1 , HN      , T3   , GRAV   ) ! T1*HN*GRAV
C      CALL OS( 'X=YZ    ' , T2 , T4  , T4  , C   ) ! CHESTR**2
C      CALL OS( 'X=Y/Z   ' , T1 , T1 , T2, C ,2 , 0.D0,1.D-12) ! T1 / CHESTR**2
C      C = 1.D0/3.D0
C      CALL OS( 'X=Y**C   ' , T2 , HN   , HN   , C   ) ! HN**1/3
C      CALL OS( 'X=Y/Z   ' , T1 , T1 , T2, C ,2 , 0.D0,1.D-12) ! T1 / HN**1/3
!
!
C FOR ALL ROUGHNESS LAWS
      CALL OS( 'X=CXY   ' , T1 , CF      , T3   , 0.5D0   )!T1*CF/2
!
C TAU_X_SEK = -C*QV : T5
C TAU_Y_SEK = C*QU : T6
      CALL OS('X=YZ    ' , T5 , T1    , QV,  C ) ! C*QV
      CALL OS('X=Y/Z   ' , T5 , T5    , HN,  C ) ! C*QV/HN
      CALL OS('X=YZ    ' , T6 , T1    , QU,  C ) ! C*QU
      CALL OS('X=Y/Z   ' , T6 , T6    , HN,  C ) ! C*QU/HN
      CALL OS('X=-Y    ' , T6 , T6    , QV,   C ) ! -C*QU
C SQRT(TAU_X_SEK**2+TAU_Y_SEK**2) : T3
      CALL OS('X=YZ    ' , T2 , T5    , T5,  C ) ! T2 = (C*QV)**2
      CALL OS('X=YZ    ' , T3 , T6    , T6,  C ) ! T3 = (C*QU)**2
      CALL OS('X=X+Y   ' , T2 , T3    , T3,  C ) ! T2 = (C*QV)**2+(C*QU)**2
      CALL OS('X=SQR(Y)' , T3 , T2    , T3,  C ) ! T3 = SQRT((C*QU)**2+(C*QV)**2
C      PRINT*,'TAUX',T5%R(1061),T6%R(1061)
!
C TAU_X_GES = TOB*EFFPNT*CALFA + TAU_X_SEK : T1
C TAU_Y_GES = TOB*EFFPNT*SALFA + TAU_Y_SEK : T2
      CALL OS( 'X=YZ    ' , T1 , TOB      , COEFPN   , C   ) ! TOB*EFFPNT
      CALL OS( 'X=YZ    ' , T2 , T1      ,  SALFA   , C   ) ! TOB*EFFPNT*SALFA
      CALL OS( 'X=YZ    ' , T1 , T1      , CALFA   , C   ) ! TOB*EFFPNT*CALFA
      CALL OS('X=X+Y   ' , T1 , T5    , T3,  C ) ! TAU_X_GES = TOB*CALFA+TAU_X_SEK
      CALL OS('X=X+Y   ' , T2 , T6    , T3,  C ) ! TAU_Y_GES = TOB*SALFA+TAU_Y_SEK
CTAU_GES=SQRT(TAU_X_GES**2+TAU_Y_GES**2)
      CALL OS( 'X=YZ    ' , T3 , T1      , T1   , C   ) ! TAU_X_GES**2
      CALL OS( 'X=YZ    ' , T4 , T2      , T2   , C   ) ! TAU_Y_GES**2
      CALL OS('X=X+Y   ' , T4 , T3    , T3,  C ) !TAU_X_GES**2+TAU_Y_GES**2
      CALL OS('X=SQR(Y)' , T4 , T4    , T3,  C ) ! SQRT(TAU_X_GES**2+TAU_Y_GES**2)
!
!
C NEW ANGLE
C CALFA_NEW = COS(TAU_X_GES/TAU_GES)
C SALFA_NEW = SIN(TAU_Y_GES/TAU_GES)
      CALL OS('X=Y/Z   ' , T1 , T1 , T4, C ,2 , 0.D0,1.D-12) !TAU_X_GES/TAU_GES
      CALL OS('X=Y/Z   ' , T2 , T2 , T4, C ,2 , 0.D0,1.D-12) !TAU_Y_GES/TAU_GES
!
C TAKEN FROM EFFPNT UEBER
C TO MAKE SURE THAT TAU_X_GES/TAU_GES IS IN RANGE [-1,1]
       DO I=1,NPOIN
         IF(T1%R(I).LT.-1.D0.OR.T1%R(I).GT.1.D0.OR.
     &      T2%R(I).LT.-1.D0.OR.T2%R(I).GT.1.D0) THEN
            PRINT*,'NOT ACCEPTABLE BORDER CROSSING',I
         ENDIF
         T1%R(I) = MIN(T1%R(I),1.D0)
         T1%R(I) = MAX(T1%R(I),-1.D0)
         T2%R(I) = MIN(T2%R(I),1.D0)
         T2%R(I) = MAX(T2%R(I),-1.D0)
       ENDDO
!
      CALL OS( 'X=Y     ' ,X=CALFA ,Y=T1 ) ! (TAU_X_GES/TAU_GES)
      CALL OS( 'X=Y     ' ,X=SALFA ,Y=T2 ) ! (TAU_Y_GES/TAU_GES)
!
C COEFPN_NEW = TAU_GES / TOB
      CALL OS('X=Y/Z   ' , COEFPN , T4 , TOB, C ,2 , 0.D0,1.D-12) !COEFPN=TAU_GES/TOB
!
C FROM EFFPNT
C FOR BOUNDARY NODES WITH IMPOSED FLOW :
C QS IS NOT MODIFIED WHEN USER-DEFINED
      DO 10 I = 1 , NPTFR
        IF (LIQBOR%I(I).EQ.5) THEN
          COEFPN%R(MESH%NBOR%I(I)) = 1.D0
        ENDIF
10    CONTINUE
!
      RETURN
      END


C
C#######################################################################
C