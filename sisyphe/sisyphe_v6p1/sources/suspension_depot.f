C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FLUX OF DEPOSITION AND EROSION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  T1: TOB

!>  @note  TO DO:  REPLACE USTAR WITH TOB

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ACLADM, DEBUG, FLUDPT, HMIN, HN, KARMAN, NPOIN, SEDCO, T1, T2, TOB, VITCD, XMVE, XWC, ZERO, ZREF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUX, I
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SUSPENSION_DEPOT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), SUSPENSION_ROUSE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SUSPENSION_COMPUTATION()

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
!>      <tr>
!>      <td><center> 5.9                                       </center>
!> </td><td> 31/07/2008
!> </td><td> J-M HERVOUET + C VILLARET
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ACLADM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DEBUG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLUDPT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HMIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>HAUTEUR D'EAU
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SEDCO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TOB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VITCD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XMVE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XWC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZERO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZREF
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
        SUBROUTINE SUSPENSION_DEPOT  !
     &(TOB,HN, ACLADM,NPOIN, HMIN,XWC,VITCD,
     & ZERO,KARMAN,XMVE, T1,T2,ZREF,FLUDPT,DEBUG,SEDCO)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ACLADM         |---| 
C| DEBUG          |---| 
C| FLUDPT         |---| 
C| HMIN           |---| 
C| HN             |-->| HAUTEUR D'EAU
C| KARMAN         |---| 
C| NPOIN          |---| 
C| SEDCO          |---| 
C| T1             |---| 
C| T2             |---| 
C| TOB            |---| 
C| VITCD          |---| 
C| XMVE           |---| 
C| XWC            |---| 
C| ZERO           |---| 
C| ZREF           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE,EX_SUSPENSION_DEPOT => SUSPENSION_DEPOT
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE (BIEF_OBJ),  INTENT(IN)    ::  HN, ACLADM,TOB
      INTEGER,          INTENT(IN)    ::  NPOIN,DEBUG
      LOGICAL,          INTENT(IN)    :: SEDCO
      DOUBLE PRECISION, INTENT(IN)    ::  HMIN
      DOUBLE PRECISION, INTENT(IN)    :: XWC
      DOUBLE PRECISION, INTENT(IN)    :: VITCD
      DOUBLE PRECISION, INTENT(IN)    :: ZERO, KARMAN,XMVE
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: T1,T2
      TYPE (BIEF_OBJ),  INTENT(IN)    :: ZREF
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: FLUDPT
C
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER :: I
      DOUBLE PRECISION:: AUX
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!
!     ! ****************************************            !
      ! THE TOTAL FRICTION VELOCITY    --> USTAR (T1)       !
      ! HAS BEEN REPLACED BY USTARP (SKIN FRICTION VELOCITY)!
      ! FOR EROSION FLUX IN V6P0                            !
      ! ****************************************            !


      CALL OS('X=CY    ', X=T1, Y=TOB, C=1.D0/XMVE)
      CALL OS('X=+(Y,C)', X=T1, Y=T1, C=ZERO)
      CALL OS('X=SQR(Y)', X=T1, Y=T1)


      IF(SEDCO) THEN
!
      ! ************************************************ !
      ! IA - FORMULATION FOR COHESIVE SEDIMENTS          !
      !      (WITHOUT BEDLOAD)                           !
      ! ************************************************ !
!
C  COMPUTES THE PROBABILITY FOR DEPOSITION
!
         DO I = 1, NPOIN
           IF(VITCD.GT.1.D-08) THEN
             AUX = MAX(1.D0-(T1%R(I)/VITCD)**2,ZERO)
           ELSE
             AUX=0.D0
           ENDIF
C          COMPUTES THE IMPLICIT PART OF THE DEPOSITION FLUX
           FLUDPT%R(I)= XWC*AUX
         ENDDO
C UNIFORM SEDIMENT ALONG THE VERTICAL
         CALL OS('X=C     ', X=T2, C=1.D0)
!
      ! ******************************************* !
      ! IB - FORMULATION FOR NON-COHESIVE SEDIMENTS !
      !      (WITH BEDLOAD)                         !
      ! ******************************************* !
!
      ELSE
!
            ! ***************************************************** !
            !  COMPUTES THE RATIO BETWEEN NEAR BED CONC. AND MEAN CONC.  !
            !                                  -->  T2    (TO KEEP )     !
            ! ***************************************************** !
        IF (DEBUG > 0) WRITE(LU,*) 'SUSPENSION_ROUSE'
        CALL SUSPENSION_ROUSE(T1,HN,NPOIN,
     &                        KARMAN,HMIN,ZERO,XWC,ZREF,T2)
        IF (DEBUG > 0) WRITE(LU,*) 'END SUSPENSION_ROUSE'
!
            ! *****************************************************  !
            !  COMPUTES THE DEPOSITION FLUX --> FLUDPT = XWC * T2    !
            ! *****************************************************  !
!
         CALL OS('X=CY    ', X=FLUDPT, Y=T2, C=XWC)
!
      ENDIF
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
C
C#######################################################################
C