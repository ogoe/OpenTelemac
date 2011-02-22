C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BIJKER BEDLOAD TRANSPORT FORMULATION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BIJK, DENS, DM, GRAV, HN, HOULE, KARMAN, KSP, KSR, MU, NPOIN, QSC, QSS, T4, T7, T8, T9, TOB, TOBW, XMVE, XWC, ZERO
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C1, C2, I, UCF
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BEDLOAD_BIJKER
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> INTEG(), OS()
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
!>      <tr>
!>      <td><center> 5.6                                       </center>
!> </td><td> **/12/2004
!> </td><td> F. HUVELIN
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.4                                       </center>
!> </td><td> 10/03/2004
!> </td><td> C. VILLARET
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.1                                       </center>
!> </td><td> 26/11/2001
!> </td><td> C. MACHET; T. BOULET; E. BEN SLAMA
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>BIJK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DENS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HOULE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KSP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KSR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T7
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T8
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T9
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TOB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TOBW
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
!>     </table>
C
C#######################################################################
C
        SUBROUTINE BEDLOAD_BIJKER !
     &  (TOBW,TOB,MU,KSP,KSR,HN,NPOIN,DM,DENS,XMVE,GRAV,XWC,
     &   KARMAN,ZERO,T4,T7,T8,T9,QSC,QSS,BIJK,HOULE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BIJK           |---| 
C| DENS           |---| 
C| DM             |---| 
C| GRAV           |---| 
C| HN             |---| 
C| HOULE          |---| 
C| KARMAN         |---| 
C| KSP            |---| 
C| KSR            |---| 
C| MU             |---| 
C| NPOIN          |---| 
C| QSC            |---| 
C| QSS            |---| 
C| T4             |---| 
C| T7             |---| 
C| T8             |---| 
C| T9             |---| 
C| TOB            |---| 
C| TOBW           |---| 
C| XMVE           |---| 
C| XWC            |---| 
C| ZERO           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE,EX_BEDLOAD_BIJKER => BEDLOAD_BIJKER
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU

      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOBW, TOB, KSR,KSP, HN,MU
      INTEGER,          INTENT(IN)    :: NPOIN
      LOGICAL,          INTENT(IN)    :: HOULE
      DOUBLE PRECISION, INTENT(IN)    :: DM, DENS, XMVE, GRAV, XWC
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN, ZERO
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T4
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T7, T8, T9
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: QSC, QSS


      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER                      :: I
      DOUBLE PRECISION             :: C1, C2, UCF
      DOUBLE PRECISION, INTENT(IN) :: BIJK

!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!

      ! ***************************************************** !
      ! I - STRESS UNDER THE COMBINED ACTION OF WAVES AND CURRENTS !
      ! ***************************************************** !

      IF(HOULE) THEN
        CALL OS('X=CY    ', X=T4, Y=TOBW, C= 0.5D0)
        CALL OS('X=X+Y   ', X=T4, Y=TOB)
      ELSE
        CALL OS('X=Y     ', X=T4, Y=TOB)
      ENDIF

      ! ******************************************************* !
      ! II - CORRECTION TO TAKE BED FORMS INTO ACCOUNT          !
      ! ******************************************************* !

C      CALL OS('X=Y/Z   ', X=MU, Y=CFP, Z=CF)
C      CALL OS('X=Y**C  ', X=MU, Y=MU , C=0.75D0)

      ! ***************************** !
      ! III - BEDLOAD TRANSPORT       !
      ! ***************************** !
      C1 = BIJK*DM
      C2 = DENS*DM*XMVE*GRAV
      DO I = 1, NPOIN
         IF (T4%R(I)*MU%R(I)> ZERO) THEN
            QSC%R(I) = C1*SQRT(TOB%R(I)/XMVE )
     &               * EXP(-0.27D0*(C2/(T4%R(I)*MU%R(I))))
         ELSE
            QSC%R(I) = 0.D0
         ENDIF
      ENDDO

      ! *********************************************************** !
      ! IV- ROUSE NUMBER AND LOWER BOUND OF EINSTEIN INTEGRAL       ! (_IMP_)
      ! *********************************************************** !
      DO I = 1, NPOIN
         IF (T4%R(I) > 0.D0) THEN
            UCF     = SQRT( T4%R(I) / XMVE)
            T7%R(I) = XWC / ( KARMAN * UCF )
C            AUX     = 1.D0 + KARMAN*SQRT(2.D0/MAX(CF%R(I),ZERO))
C            T8%R(I) = 30.D0*EXP(-AUX)
             T8%R(I) = MAX(KSR%R(I),KSP%R(I))/MAX(HN%R(I),ZERO)
         ELSE
            T7%R(I)= 100001.D0
            T8%R(I)= 100001.D0
         ENDIF
      ENDDO

      ! ************************************ !
      ! V - EINSTEIN INTEGRAL                ! (_IMP_)
      ! ************************************ !
      CALL INTEG(T7%R, T8%R, T9%R, NPOIN)

      ! ************************************** !
      ! VI - TRANSPORT BY SUSPENSION           ! (_IMP_)
      ! ************************************** !
      CALL OS('X=YZ    ', X=QSS, Y=T9, Z=QSC)

!======================================================================!
!======================================================================!

      RETURN
      END SUBROUTINE BEDLOAD_BIJKER
C
C#######################################################################
C