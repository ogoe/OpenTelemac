C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE WAVE HEIGHT AND PHASE, SPEEDS
!>                AND THE FREE SURFACE ELEVATION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_ARTEMIS, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_ARTEMIS :<br>
!> @link DECLARATIONS_ARTEMIS::GRAV GRAV@endlink, 
!> @link DECLARATIONS_ARTEMIS::H H@endlink, 
!> @link DECLARATIONS_ARTEMIS::HHO HHO@endlink, 
!> @link DECLARATIONS_ARTEMIS::IELM IELM@endlink, 
!> @link DECLARATIONS_ARTEMIS::INCI INCI@endlink, 
!> @link DECLARATIONS_ARTEMIS::MASKEL MASKEL@endlink, 
!> @link DECLARATIONS_ARTEMIS::MESH MESH@endlink, 
!> @link DECLARATIONS_ARTEMIS::MSK MSK@endlink, 
!> @link DECLARATIONS_ARTEMIS::NPOIN NPOIN@endlink, 
!> @link DECLARATIONS_ARTEMIS::OMEGA OMEGA@endlink, 
!> @link DECLARATIONS_ARTEMIS::PHAS PHAS@endlink, 
!> @link DECLARATIONS_ARTEMIS::PHII PHII@endlink, 
!> @link DECLARATIONS_ARTEMIS::PHIR PHIR@endlink, 
!> @link DECLARATIONS_ARTEMIS::S S@endlink, 
!> @link DECLARATIONS_ARTEMIS::SBID SBID@endlink, 
!> @link DECLARATIONS_ARTEMIS::T1 T1@endlink, 
!> @link DECLARATIONS_ARTEMIS::T2 T2@endlink, 
!> @link DECLARATIONS_ARTEMIS::T3 T3@endlink, 
!> @link DECLARATIONS_ARTEMIS::T4 T4@endlink, 
!> @link DECLARATIONS_ARTEMIS::U0 U0@endlink, 
!> @link DECLARATIONS_ARTEMIS::V0 V0@endlink, 
!> @link DECLARATIONS_ARTEMIS::ZF ZF@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A1, A2, ALPHA0, BID, D1, D2, I, MODPHI, PHI, PHI1, PHI2, PI, RADDEG, TETA01, WT0, XU1, XU2, XV1, XV2, ZERO
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ARTEMIS()

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
!>  <tr>
!>    <td><center> 5.1                                    </center></td>
!>    <td> 04/06/1999                                              </td>
!>    <td> D. AELBRECHT (LNH) 01.30.87.74.12                       </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td>                                                         </td>
!>    <td> J-M HERVOUET (LNH) 01.30.87.80.18                       </td>
!>    <td> LINKED TO BIEF 5.0                                      </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CALRES
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER I
C
      DOUBLE PRECISION PI,RADDEG
      DOUBLE PRECISION ZERO, BID
      DOUBLE PRECISION A1, A2 ,ALPHA0, D1, D2, PHI, PHI1, PHI2 ,MODPHI
      DOUBLE PRECISION TETA01, XU1 ,XU2, XV1, XV2, WT0
C
      INTRINSIC SQRT, ATAN2, DMOD, ABS, COS, SIN
C
C-----------------------------------------------------------------------
C
C
      PARAMETER (ZERO = 1.D-10)
      PARAMETER (PI = 3.1415926535897932384626433D0)
      PARAMETER (RADDEG = 57.29577951D0)
C
C=======================================================================
C WAVE HEIGHT
C=======================================================================
C
      CALL OS( 'X=N(Y,Z)', T1, PHIR, PHII , BID             )
      CALL OS( 'X=CY    ', HHO  ,T1, SBID ,(2.D0*OMEGA/GRAV))
C
C=======================================================================
C PHASE OF THE POTENTIAL (IN RADIAN)
C=======================================================================
C
      DO 10 I=1,NPOIN
         IF (T1%R(I).LT.ZERO) THEN
            PHAS%R(I) = 0.D0
         ELSE
            PHAS%R(I) = ATAN2( PHII%R(I),PHIR%R(I) )
         ENDIF
10    CONTINUE
C
C=======================================================================
C FREE SURFACE ELEVATION
C=======================================================================
C
      DO 20 I=1,NPOIN
         S%R(I) = -OMEGA/GRAV*PHII%R(I) + H%R(I) + ZF%R(I)
20    CONTINUE
C
C=======================================================================
C SPEEDS AT THE SURFACE (AT T=0 AND T=OMEGA/4)
C=======================================================================
C
C COMPUTES THE GRADIENTS (PHIR AND PHII)
C
C
      CALL VECTOR(U0 , '=' , 'GRADF          X' , IELM ,
     &            1.D0 , PHIR , SBID, SBID , SBID , SBID , SBID ,
     &            MESH , MSK , MASKEL )
C
      CALL VECTOR(V0 , '=' , 'GRADF          Y' , IELM ,
     &            1.D0 , PHIR , SBID , SBID , SBID , SBID , SBID ,
     &            MESH , MSK , MASKEL )
C
C     THE OLD VARIABLE U1 IS STORED IN T3
C     BECAUSE IT IS USED TO COMPUTE INCI
C
      CALL VECTOR(T3 , '=' , 'GRADF          X' , IELM ,
     &            1.D0 , PHII , SBID , SBID , SBID , SBID , SBID ,
     &            MESH , MSK , MASKEL )
C
C     THE OLD VARIABLE V1 IS STORED IN T4
C     BECAUSE IT IS USED TO COMPUTE INCI
C
      CALL VECTOR(T4 , '=' , 'GRADF          Y' , IELM ,
     &            1.D0 , PHII , SBID , SBID , SBID , SBID , SBID ,
     &            MESH , MSK , MASKEL )
C
      CALL VECTOR(T1 , '=' , 'MASBAS          ' , IELM ,
     &            1.D0 , SBID , SBID , SBID , SBID , SBID , SBID ,
     &            MESH , MSK , MASKEL )
C
      CALL OS( 'X=Y/Z   ' , U0    , U0    , T1 , BID )
      CALL OS( 'X=Y/Z   ' , V0    , V0    , T1 , BID )
      CALL OS( 'X=Y/Z   ' , T3    , T3    , T1 , BID )
      CALL OS( 'X=Y/Z   ' , T4    , T4    , T1 , BID )
C
C=======================================================================
C COMPUTES WAVE INCIDENCE
C=======================================================================
C
C        U0 (D(PHIR)/DX) : A      U1 (D(PHII)/DX): B
C        V0 (D(PHIR)/DY) : C      V1 (D(PHII)/DY): D
C FROM U= A COS WT + B SIN WT  TO : U = A1 COS ( WT - PHI1)
C      V= C COS WT + D SIN WT       V = A2 COS ( WT - PHI2)
C
      DO 30 I=1,NPOIN
        A1 = SQRT ( U0%R(I)*U0%R(I) + T3%R(I)*T3%R(I) )
        PHI1 = ATAN2( T3%R(I),U0%R(I) )
        A2 = SQRT ( V0%R(I)*V0%R(I) + T4%R(I)*T4%R(I) )
        PHI2 = ATAN2( T4%R(I),V0%R(I) )
C
C WRITTEN AS : U = A1 COS ( (WT - PHI1))
C              V = A2 COS ( (WT - PHI1) - PHI )
C WHERE PHI IS BETWEEN 0 AND 2*PI
C
        PHI = PHI2 - PHI1
        IF (PHI.LT.0.D0)   PHI = PHI+2.D0*PI
C
C ESTIMATES THE DIRECTION AND (WT0) WHEN THE ELLIPSE'S MAJOR AXIS
C IS REACHED.
C TREATS INDIVIDUAL CASES (LINEAR POLARISATION)
C
        MODPHI = DMOD( PHI, PI )
        IF ( (MODPHI.LT.ZERO).OR.((PI-MODPHI).LT.ZERO) ) THEN
          WT0 = PHI1
          IF ( (PHI.LT.2D0*ZERO).OR.((2.D0*PI-PHI).LT.2D0*ZERO) )THEN
            ALPHA0 = ATAN2( A2,A1 )
          ELSE
C                  (ABS(PHI-PI).LT.2D0*ZERO)
            ALPHA0 = 2.D0*PI - ATAN2( A2,A1 )
          ENDIF
        ELSE
C GENERAL CASE: ELLIPTIC POLARISATION
C        TAN(2*(WT0 - PHI1)) = A2**2*SIN(2*PHI)/(A1**2+A2**2*COS(2*PHI))
          TETA01 = ATAN2( (A2*A2*SIN(2*PHI)) ,
     &                    (A1*A1 + A2*A2*COS(2*PHI)) ) / 2.D0
          XU1 = A1 * COS ( TETA01)
          XV1 = A2 * COS ( TETA01 - PHI )
          XU2 = -A1 * SIN ( TETA01)
          XV2 = -A2 * SIN ( TETA01 - PHI )
          D1 = XU1*XU1 + XV1*XV1
          D2 = XU2*XU2 + XV2*XV2
          IF (D2.GT.D1) THEN
             TETA01 = TETA01 + PI/2.D0
             XU1    = XU2
             XV1    = XV2
          ENDIF
          WT0    = TETA01 + PHI1
          ALPHA0 = ATAN2( XV1,XU1 )
        ENDIF
        INCI%R(I)  = ALPHA0
        T2%R(I) = WT0
 30   CONTINUE
C
C FREE SURFACE IN PHASE WITH ALPHA0
C INCIDENCE IS CONSIDERED POSITIVE WHEN THE FREE SURFACE IS
C POSITIVE.
C
      DO 40 I=1,NPOIN
         A1 = -(PHII%R(I)*COS(T2%R(I))-PHIR%R(I)*SIN(T2%R(I)))
         IF (A1.LT.0.D0) THEN
           IF (INCI%R(I).GE.0.D0) THEN
             INCI%R(I) = INCI%R(I) - PI
           ELSE
             INCI%R(I) = INCI%R(I) + PI
           ENDIF
         ENDIF
 40   CONTINUE
C
C=======================================================================
C
      RETURN
      END
C
C#######################################################################
C