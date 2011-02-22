C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE RADIATION STRESSES AND DRIVING FORCES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @reference M.W. DINGEMANS, A.C. RADDER AND H.J. DE VRIEND
!>          COMPUTATION OF THE DRIVING FORCES OF WACE-INDUCED
!>          CURRENTS. COASTAL ENGINEERING, 11 (1987) PP 539-563.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_ARTEMIS, DECLARATIONS_TELEMAC, INTERFACE_ARTEMIS
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> LISHHO
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_ARTEMIS :<br>
!> @link DECLARATIONS_ARTEMIS::AM1 AM1@endlink, 
!> @link DECLARATIONS_ARTEMIS::C C@endlink, 
!> @link DECLARATIONS_ARTEMIS::CG CG@endlink, 
!> @link DECLARATIONS_ARTEMIS::FX FX@endlink, 
!> @link DECLARATIONS_ARTEMIS::FY FY@endlink, 
!> @link DECLARATIONS_ARTEMIS::GRAV GRAV@endlink, 
!> @link DECLARATIONS_ARTEMIS::H H@endlink, 
!> @link DECLARATIONS_ARTEMIS::HALE HALE@endlink, 
!> @link DECLARATIONS_ARTEMIS::HHO HHO@endlink, 
!> @link DECLARATIONS_ARTEMIS::IELM IELM@endlink, 
!> @link DECLARATIONS_ARTEMIS::INCI INCI@endlink, 
!> @link DECLARATIONS_ARTEMIS::MASKEL MASKEL@endlink, 
!> @link DECLARATIONS_ARTEMIS::MESH MESH@endlink, 
!> @link DECLARATIONS_ARTEMIS::MSK MSK@endlink, 
!> @link DECLARATIONS_ARTEMIS::NPOIN NPOIN@endlink, 
!> @link DECLARATIONS_ARTEMIS::SBID SBID@endlink, 
!> @link DECLARATIONS_ARTEMIS::SXX SXX@endlink, 
!> @link DECLARATIONS_ARTEMIS::SXY SXY@endlink, 
!> @link DECLARATIONS_ARTEMIS::SYY SYY@endlink, 
!> @link DECLARATIONS_ARTEMIS::T1 T1@endlink, 
!> @link DECLARATIONS_ARTEMIS::T2 T2@endlink, 
!> @link DECLARATIONS_ARTEMIS::T3 T3@endlink, 
!> @link DECLARATIONS_ARTEMIS::T4 T4@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> BID, COCO, COE, COSI, I, LISRAD, MAS, SISI, Z
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_RADIA2
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> FILTER(), OS(), VECTOR()
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
!>    <td> D. AELBRECHT (LNH) 01.30.87.74.12; F. BECQ (LNH)        </td>
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
!>          <tr><td>LISHHO
!></td><td>---</td><td>LISSAGE DE LA HAUTEUR DE HOULE
!>    </td></tr>
!>          <tr><td>OMEGA
!></td><td>--></td><td>PULSATION DE LA HOULE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE RADIA2
     &(LISHHO)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| LISHHO         |---| LISSAGE DE LA HAUTEUR DE HOULE
C| OMEGA          |-->| PULSATION DE LA HOULE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
      USE INTERFACE_ARTEMIS, EX_RADIA2=> RADIA2
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER I
      INTEGER LISHHO
C
      DOUBLE PRECISION BID
C
C INTERNAL VARIABLES FOR RADIA2
C
      DOUBLE PRECISION COE , COCO, COSI, SISI , Z(1)
      INTEGER          LISRAD
C
      LOGICAL MAS
C
      INTRINSIC SQRT, ATAN2, DMOD, ABS, COS, SIN
C
C
C=======================================================================
C     RADIATION STRESSES........METHOD 2 (IDENTICAL TO THAT USED IN TOMAWAC)
C=======================================================================
C
      CALL OS('X=Y     ',T3,HHO,SBID,BID)
C
C -------------------------------------------------------------
C SMOOTHES THE WAVE HEIGHT TO ELIMINATE PARASITIC
C OSCILLATIONS
C -------------------------------------------------------------
C
      IF(LISHHO.GT.0) THEN
         MAS = .TRUE.
         CALL FILTER(T3,MAS,T1,T2,AM1,'MATMAS          ',
     &               1.D0,SBID,SBID,SBID,SBID,SBID,SBID,
     &               MESH,MSK,MASKEL,LISHHO)
      ENDIF
C
      CALL OS('X=Y     ',HHO,T3,SBID,BID)
C
C -------------------------------------------------------------
C COMPUTES STRESSES SXX, SXY AND SYY
C -------------------------------------------------------------
C
      CALL OS('X=Y/Z   ' , T1 , CG , C  , BID )
      DO I=1,NPOIN
         COCO=COS(INCI%R(I))*COS(INCI%R(I))
         COSI=COS(INCI%R(I))*SIN(INCI%R(I))
         SISI=SIN(INCI%R(I))*SIN(INCI%R(I))
         COE=GRAV*HALE%R(I)*HALE%R(I)/16.D0
C
C THE COEFFICIENT 1/16 ABOVE STEMS FROM HHO REPRESENTING THE
C SIGNIFICANT WAVE HEIGHT (ENERGY) IN RANDOM SEAS
C
         SXX%R(I)= (T1%R(I)*(1.D0+COCO)-0.5D0)*COE
         SXY%R(I)= (T1%R(I)*COSI)*COE
         SYY%R(I)= (T1%R(I)*(1.D0+SISI)-0.5D0)*COE
      END DO
C
C
C=======================================================================
C SPACIAL GRADIENTS OF RADIATION STRESSES
C=======================================================================
C
C  -----------------------------------------------
C  OPTIONAL SMOOTHING(S) OF THE RADIATION STRESSES
C  -----------------------------------------------
C
      LISRAD = 3
C
      CALL OS('X=Y     ',T3,SXX,SBID,BID)
      IF(LISRAD.GT.0) THEN
         MAS = .TRUE.
         CALL FILTER(T3,MAS,T1,T2,AM1,'MATMAS          ',
     &               1.D0,SBID,SBID,SBID,SBID,SBID,SBID,
     &               MESH,MSK,MASKEL,LISRAD)
      ENDIF
      CALL OS('X=Y     ',SXX,T3,SBID,BID)
C
      CALL OS('X=Y     ',T3,SXY,SBID,BID)
      IF(LISRAD.GT.0) THEN
         MAS = .TRUE.
         CALL FILTER(T3,MAS,T1,T2,AM1,'MATMAS          ',
     &               1.D0,SBID,SBID,SBID,SBID,SBID,SBID,
     &               MESH,MSK,MASKEL,LISRAD)
      ENDIF
      CALL OS('X=Y     ',SXY,T3,SBID,BID)
C
      CALL OS('X=Y     ',T3,SYY,SBID,BID)
      IF(LISRAD.GT.0) THEN
         MAS = .TRUE.
         CALL FILTER(T3,MAS,T1,T2,AM1,'MATMAS          ',
     &               1.D0,SBID,SBID,SBID,SBID,SBID,SBID,
     &               MESH,MSK,MASKEL,LISRAD)
      ENDIF
      CALL OS('X=Y     ',SYY,T3,SBID,BID)
C
C END OF RADIATION STRESS SMOOTHING(S)
C -------------------------------------------------------
C
C=======================================================================
C DRIVING FORCES FX AND FY FOR WAVE-INDUCED CURRENTS
C=======================================================================
C
      CALL VECTOR(T1 , '=' , 'MASBAS          ' , IELM ,
     &            1.D0 , T3 , T3 , T3 , T3 , T3 , T3 ,
     &            MESH , MSK , MASKEL )
C
      CALL VECTOR
     & (T2,'=','GRADF          X',IELM,1.D0,SXX,T4,T4,T4,T4,T4,
     &  MESH , MSK , MASKEL )
      CALL OS('X=Y/Z   ',T2,T2,T1,BID)
C
      CALL VECTOR
     & (T3,'=','GRADF          Y',IELM,1.D0,SXY,T4,T4,T4,T4,T4,
     &  MESH , MSK , MASKEL )
      CALL OS('X=Y/Z   ',T3,T3,T1,BID)
C     ------------------------------------
C     FORCE FX = - (DSXX/DX + DSXY/DY) / H
C     ------------------------------------
      CALL OS('X=Y+Z   ',FX,T2,T3,BID)
      CALL OS('X=CY/Z  ',FX,FX,H,-1.D0)
C
C     ------------------------------------
C
      CALL VECTOR(T1 , '=' , 'MASBAS          ' , IELM ,
     &            1.D0 , T3 , T3 , T3 , T3 , T3 , T3 ,
     &            MESH , MSK , MASKEL )
C
      CALL VECTOR
     & (T2,'=','GRADF          X',IELM,1.D0,SXY,T4,T4,T4,T4,T4,
     &  MESH , MSK , MASKEL )
      CALL OS('X=Y/Z   ',T2,T2,T1,BID)
C
      CALL VECTOR
     & (T3,'=','GRADF          Y',IELM,1.D0,SYY,T4,T4,T4,T4,T4,
     &  MESH , MSK , MASKEL )
      CALL OS('X=Y/Z   ',T3,T3,T1,BID)
C
C     ------------------------------------
C     FORCE FY = - (DSXY/DX + DSYY/DY) / H
C     ------------------------------------
      CALL OS('X=Y+Z   ',FY,T2,T3,BID)
      CALL OS('X=CY/Z  ',FY,FY,H,-1.D0)
C
C=======================================================================
C
      RETURN
      END
C
C#######################################################################
C