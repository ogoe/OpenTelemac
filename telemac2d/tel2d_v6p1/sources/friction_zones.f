
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES FRICTION FOR EACH NODE AND ZONE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CF, CFBOR, CHBORD, CHESTR, GRAV, H, ITURB, KARMAN, KFRO_B, LINDDP, LINDNER, LINDSP, LISRUG, MESH, NDEFMA, NDEF_B, NKFROT, S, T1, T2, U, V, VK
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CP, I, IELMC, IELMH, J
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_FRICTION_ZONES
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CHGDIS(), CPSTVC(), FRICTION_CALC(), FRICTION_LINDNER(), OS()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>FRICTION_CHOICE()

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
!> </td><td>
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 20/04/2004
!> </td><td> F. HUVELIN
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CFBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CHBORD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CHESTR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>H
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ITURB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KFRO_B
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LINDDP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LINDNER
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LINDSP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LISRUG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NDEFMA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NDEF_B
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NKFROT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>S
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VK
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FRICTION_ZONES
     & (MESH, H, U, V, S, CHESTR, CHBORD, NKFROT, NDEFMA, LINDDP,
     &  LINDSP, KFRO_B, NDEF_B, ITURB, LISRUG, LINDNER, VK,
     &  KARMAN, GRAV, T1, T2, CF, CFBOR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CF             |---| 
C| CFBOR          |---| 
C| CHBORD         |---| 
C| CHESTR         |---| 
C| GRAV           |---| 
C| H             |---| 
C| ITURB          |---| 
C| KARMAN         |---| 
C| KFRO_B         |---| 
C| LINDDP         |---| 
C| LINDNER        |---| 
C| LINDSP         |---| 
C| LISRUG         |---| 
C| MESH           |---| 
C| NDEFMA         |---| 
C| NDEF_B         |---| 
C| NKFROT         |---| 
C| S             |---| 
C| T1             |---| 
C| T2             |---| 
C| U             |---| 
C| V             |---| 
C| VK             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_FRICTION_ZONES => FRICTION_ZONES
C
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_MESH),    INTENT(IN)    :: MESH
      TYPE(BIEF_OBJ),     INTENT(IN)    :: H, U, V, S
      TYPE(BIEF_OBJ),     INTENT(IN)    :: CHESTR
      TYPE(BIEF_OBJ),     INTENT(IN)    :: CHBORD
      TYPE(BIEF_OBJ),     INTENT(IN)    :: NKFROT
      TYPE(BIEF_OBJ),     INTENT(IN)    :: NDEFMA, LINDDP, LINDSP
      TYPE(BIEF_OBJ),     INTENT(IN)    :: KFRO_B, NDEF_B
      INTEGER,            INTENT(IN)    :: ITURB, LISRUG
      LOGICAL,            INTENT(IN)    :: LINDNER
      DOUBLE PRECISION,   INTENT(IN)    :: VK, KARMAN, GRAV
      TYPE(BIEF_OBJ),     INTENT(INOUT) :: CF, CFBOR
      TYPE(BIEF_OBJ),     INTENT(INOUT) :: T1, T2
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER          :: I, J
      INTEGER          :: IELMC, IELMH
      DOUBLE PRECISION :: CP
!
!=======================================================================!
!=======================================================================!
C                               PROGRAMME                               !
!=======================================================================!
!=======================================================================!
!
      ! ======================================= !
      ! INITIALIZATION AND DISCRETIZATION CHECK !
      ! ======================================= !
!
      ! ELEMENT TYPE
      ! ------------
      IELMC = CF%ELM
      IELMH = H%ELM
!
      ! MAXIMUM BETWEEN WATER DEPTH AND 1.D-4
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      CALL CPSTVC(H,T1)
      CALL OS('X=Y     ', X=T1, Y=H)
      IF(IELMC.NE.IELMH) CALL CHGDIS(T1, IELMH, IELMC, MESH)
      CALL OS('X=+(Y,C)', X=T1, Y=T1, C=1.D-4)
!
      ! RESULTANT VELOCITY IN T2
      ! ------------------------
      CALL CPSTVC(CF,T2)
      CALL OS('X=N(Y,Z)', X=T2, Y=U, Z=V)
      CALL OS('X=+(Y,C)', X=T2, Y=T2, C=1.D-6)
!
!
      ! =============== !
      ! BOTTOM FRICTION !
      ! =============== !
!
      ! BOTTOM FRICTION CALCULATION
      ! ---------------------------
      DO I = 1, CF%DIM1
!
         ! FRICTION COEFFICIENT FOR THE BOTTOM
         ! -----------------------------------
         CALL FRICTION_CALC
     &        (I, I, NKFROT%I(I), NDEFMA%R(I), VK, GRAV,
     &         KARMAN, CHESTR, T1, T1, T2, CF)
!
         ! FRICTION COEFFICIENT FOR NON-SUBMERGED VEGETATION
         ! -------------------------------------------------
         IF (LINDNER) THEN
            CALL FRICTION_LINDNER
     &           (T2%R(I), T1%R(I), CF%R(I), VK, GRAV,
     &            LINDDP%R(I),LINDSP%R(I), CP)
!
            IF (CP < -0.9D0) THEN
               CP = 0.75D0*T1%R(I)*LINDDP%R(I) / (LINDSP%R(I))**2
            ENDIF
!
            CF%R(I) = (CF%R(I)+2.D0*CP)
         ENDIF
!
      END DO
!
!
      ! ============= !
      ! WALL FRICTION !
      ! ============= !
!
      IF(LISRUG.EQ.2) THEN
!
         DO J = 1, MESH%NPTFR
            I = MESH%NBOR%I(J)
            ! BOTTOM FRICTION CALCULATION
            ! ---------------------------
            CALL FRICTION_CALC
     &           (J,J,KFRO_B%I(J),NDEF_B%R(J),VK,GRAV,KARMAN,
     &            CHBORD,MESH%DISBOR,T1,T2,CFBOR)
         ENDDO
!
      ENDIF
!
!=======================================================================!
!=======================================================================!
      RETURN
      END SUBROUTINE FRICTION_ZONES
C
C#######################################################################
C
