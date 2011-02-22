
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES FRICTION FOR EACH NODE WHEN THERE IS ONLY
!>                ONE FRICTION LAW IN THE DOMAIN.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CF, CFBOR, CHBORD, CHESTR, DP, GRAV, H, ITURB, KARMAN, KFROT, LINDNER, LISRUG, MESH, NDEF, S, SB, SP, T1, T2, U, V, VK
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, CP, I, IELMC, IELMH
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_FRICTION_UNIF
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CHGDIS(), CPSTVC(), FRICTION_CALC(), FRICTION_LINDNER(), OS()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>FRICTION_CHOICE(), PROPAG_ADJ()

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
!> </td><td> WRITTEN FROM COEFRO.F
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
!>          <tr><td>DP
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
!>          <tr><td>KFROT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LINDNER
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LISRUG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NDEF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>S
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SP
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
                        SUBROUTINE FRICTION_UNIF
     &(MESH,H,U,V,CHESTR,S,KFROT,KFROTL,ITURB,LISRUG,LINDNER,
     & SB,NDEF,DP,SP,VK,KARMAN,GRAV,T1,T2,CHBORD,CF,CFBOR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CF             |---| 
C| CFBOR          |---| 
C| CHBORD         |---| 
C| CHESTR         |---| 
C| DP             |---| 
C| GRAV           |---| 
C| H             |---| 
C| ITURB          |---| 
C| KARMAN         |---| 
C| KFROT          |---| 
C| LINDNER        |---| 
C| LISRUG         |---| 
C| MESH           |---| 
C| NDEF           |---| 
C| S             |---| 
C| SB             |---| 
C| SP             |---| 
C| T1             |---| 
C| T2             |---| 
C| U             |---| 
C| V             |---| 
C| VK             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_TELEMAC2D, EX_FRICTION_UNIF => FRICTION_UNIF
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_MESH),  INTENT(IN)      :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)      :: H,U,V,CHESTR,CHBORD,S
      INTEGER,          INTENT(IN)      :: KFROT,KFROTL,ITURB,LISRUG
      LOGICAL,          INTENT(IN)      :: LINDNER
      DOUBLE PRECISION, INTENT(IN)      :: NDEF,DP,SP
      DOUBLE PRECISION, INTENT(IN)      :: VK,KARMAN,GRAV
!
      DOUBLE PRECISION, INTENT(INOUT)   :: SB
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: T1, T2
!
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: CF, CFBOR
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER          :: IELMC, IELMH, I
      DOUBLE PRECISION :: C, CP
!
!=======================================================================!
!=======================================================================!
C                               PROGRAMME                               !
!=======================================================================!
!=======================================================================!
!
! ======================================= !
C INITIALIZATION AND DISCRETIZATION CHECK !
! ======================================= !
!
      ! ELEMENT TYPE
      ! ------------
      IELMC = CF%ELM
      IELMH = H%ELM
!
      ! SAME DISCRETIZATION FOR WATER DEPTH AND FRICTION COEFFICIENT IF NEEDED
      ! ----------------------------------------------------------------------
      IF (KFROT.NE.0.AND.KFROT.NE.2) THEN
         !
         ! MAXIMUM BETWEEN WATER DEPTH AND 1.D-4
         ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         CALL CPSTVC(H,T1)
         CALL OS('X=Y     ', T1, H, S, C)
         IF(IELMC.NE.IELMH) CALL CHGDIS( T1 , IELMH , IELMC , MESH )
!        NIKURADSE LAW WILL DO ITS OWN CLIPPING
         IF(KFROT.NE.5) THEN
           CALL OS('X=+(Y,C)', T1, T1, S, 1.D-4)
         ENDIF
      ENDIF
!
      ! RESULTANT VELOCITY IN T2
      ! ------------------------
      IF ((KFROT==1).OR.(KFROT==6).OR.(KFROT==7)) THEN
         CALL CPSTVC(CF,T2)
         CALL OS('X=N(Y,Z)', T2,  U, V, C)
         CALL OS('X=+(Y,C)', T2, T2, S, 1.D-6)
      ENDIF
!
      ! =============== !
      ! BOTTOM FRICTION !
      ! =============== !
!
      ! FRICTION COEFFICIENT FOR THE BOTTOM
      ! -----------------------------------
      CALL FRICTION_CALC(1, CF%DIM1, KFROT, NDEF, VK, GRAV,
     &                   KARMAN, CHESTR, T1, T1, T2, CF)
!
      ! FRICTION COEFFICIENT FOR NON-SUBMERGED VEGETATION
      ! -------------------------------------------------
      IF(LINDNER) THEN
!
         DO I = 1, CF%DIM1
            CALL FRICTION_LINDNER
     &           (T2%R(I), T1%R(I), CF%R(I),
     &            VK, GRAV, DP, SP, CP)
!
            IF (CP< -0.9) THEN
               CP = 0.75*T1%R(I)*DP/(SP**2)
            ENDIF
!
            CF%R(I) = (CF%R(I)+2.D0*CP)
!
         ENDDO
      ENDIF
!
! ============= !
! WALL FRICTION !
! ============= !
!
      ! WALL FRICTION COMPUTATION
      ! -------------------------
!
      IF(LISRUG.EQ.2) THEN
        CALL FRICTION_CALC(1,MESH%NPTFR,KFROTL,NDEF,VK,GRAV,
     &                     KARMAN,CHBORD,MESH%DISBOR,T1,T2,CFBOR)
      ENDIF
!
!=======================================================================!
!=======================================================================!
!
      RETURN
      END
C
C#######################################################################
C
