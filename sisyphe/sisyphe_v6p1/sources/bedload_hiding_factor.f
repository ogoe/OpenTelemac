C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       HIDING FACTOR FOR EACH NODE, SEDIMENT CLASS
!>                AND TIME STEP.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ACLADM, DM, HIDFAC, HIDI, HIDING, KARIM_HOLLY_YANG, NPOIN
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BEDLOAD_HIDING_FACTOR
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), PLANTE()
!>   </td></tr>
!>     <tr><th> Unknown(s)
!>    </th><td> BIEF_OBJ, C1, C2, DOUBLE, GLOBAL, IN, INOUT, INTEGER, INTENT, J, LOCAL, TYPE, VARIABLES
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BEDLOAD_SOLIDISCHARGE()

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
!>      <td><center> 5.5                                       </center>
!> </td><td> 14/09/2004
!> </td><td> F. HUVELIN
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.3                                       </center>
!> </td><td> **/**/2002
!> </td><td> M. GONZALES DE LINARES
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.3                                       </center>
!> </td><td> **/11/2002
!> </td><td> B. MINH DUC
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
!>          <tr><td>DM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HIDFAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HIDI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HIDING
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KARIM_HOLLY_YANG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
        SUBROUTINE BEDLOAD_HIDING_FACTOR
     &(ACLADM, HIDFAC, NPOIN, HIDI, DM, KARIM_HOLLY_YANG, HIDING)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ACLADM         |---| 
C| DM             |---| 
C| HIDFAC         |---| 
C| HIDI           |---| 
C| HIDING         |---| 
C| KARIM_HOLLY_YAN|---| 
C| NPOIN          |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE,
     &    EX_BEDLOAD_HIDING_FACTOR => BEDLOAD_HIDING_FACTOR
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
      !
      !
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      !
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ACLADM
      INTEGER,          INTENT(IN)    :: HIDFAC, NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: HIDI, DM, KARIM_HOLLY_YANG
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HIDING
      !
      !
      ! 3/ LOCAL VARIABLES
      ! ------------------
      !
      INTEGER          :: J
      DOUBLE PRECISION :: C1, C2
!
!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
! *************************** !
C IA - CONSTANT HIDING FACTOR !
! *************************** !
!
      IF (HIDFAC == 0) THEN
!
         CALL OS('X=C     ', X=HIDING, C=HIDI)
!
! ************************** !
C IB - EGIAZAROFF FORMULATION !
! ************************** !
!
      ELSEIF (HIDFAC == 1) THEN
!
         C1 = LOG10(19.D0)
         C2 = 19.D0*DM
         DO J = 1, NPOIN
           HIDING%R(J) = (C1/LOG10(C2/ACLADM%R(J)))**2
         ENDDO
!
! ********************************** !
C IC - ASHIDA AND MICHIUE FORMULATION !
! ********************************** !
!
      ELSEIF (HIDFAC == 2) THEN
!
         C1 = LOG10(19.D0)
         C2 = 19.D0*DM
         DO J = 1, NPOIN
!
            IF(DM/ACLADM%R(J) >= 0.4D0) THEN
              HIDING%R(J) = (C1 / LOG10(C2/ACLADM%R(J)) )**2
            ELSE
              HIDING%R(J) = 0.85D0*(ACLADM%R(J)/DM)
            ENDIF
!
         ENDDO
!
! ************************************* !
C IE - KARIM, HOLLY AND YANG FORMULATION !
! ************************************* !
!
      ELSEIF (HIDFAC == 4) THEN
!
         CALL OS('X=1/Y   ', X=HIDING, Y=ACLADM)
         CALL OS('X=CX    ', X=HIDING, C=DM)
         CALL OS('X=Y**C  ', X=HIDING, Y=HIDING, C=KARIM_HOLLY_YANG)
!
      ELSE
!
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'FORMULE DE MASQUAGE INCONNUE : ',HIDFAC
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'UNKNOWN HIDING FACTOR FORMULA: ',HIDFAC
        ENDIF
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!======================================================================!
!======================================================================!
!
      RETURN
      END SUBROUTINE BEDLOAD_HIDING_FACTOR
C
C#######################################################################
C
