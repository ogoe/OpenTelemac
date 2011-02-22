C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       FILLS THE ARRAYS MASK1, MASK2, MASK3, MASK4.<br>
!>      MASK1: CORRESPONDS TO INCIDENT WAVES (KINC)
!>      MASK2: CORRESPONDS TO FREE EXIT (KSORT)
!>      MASK3: CORRESPONDS TO SOLID BOUNDARY (KLOG)
!>      MASK4: CORRESPONDS TO IMPOSED WAVES (KENT)

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_ARTEMIS, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_ARTEMIS :<br>
!> @link DECLARATIONS_ARTEMIS::KP1BOR_TOT KP1BOR_TOT@endlink, 
!> @link DECLARATIONS_ARTEMIS::LIHBOR LIHBOR@endlink, 
!> @link DECLARATIONS_ARTEMIS::LIHBORT LIHBORT@endlink, 
!> @link DECLARATIONS_ARTEMIS::MASK1 MASK1@endlink, 
!> @link DECLARATIONS_ARTEMIS::MASK1T MASK1T@endlink, 
!> @link DECLARATIONS_ARTEMIS::MASK2 MASK2@endlink, 
!> @link DECLARATIONS_ARTEMIS::MASK2T MASK2T@endlink, 
!> @link DECLARATIONS_ARTEMIS::MASK3 MASK3@endlink, 
!> @link DECLARATIONS_ARTEMIS::MASK3T MASK3T@endlink, 
!> @link DECLARATIONS_ARTEMIS::MASK4 MASK4@endlink, 
!> @link DECLARATIONS_ARTEMIS::MASK4T MASK4T@endlink, 
!> @link DECLARATIONS_ARTEMIS::MESH MESH@endlink, 
!> @link DECLARATIONS_ARTEMIS::NBOR_TOT NBOR_TOT@endlink, 
!> @link DECLARATIONS_ARTEMIS::NPTFR NPTFR@endlink, 
!> @link DECLARATIONS_ARTEMIS::NPTFR_TOT NPTFR_TOT@endlink, 
!> @link DECLARATIONS_ARTEMIS::SBID SBID@endlink<hr>
!> DECLARATIONS_TELEMAC :<br>
!> @link DECLARATIONS_TELEMAC::KENT KENT@endlink, 
!> @link DECLARATIONS_TELEMAC::KINC KINC@endlink, 
!> @link DECLARATIONS_TELEMAC::KLOG KLOG@endlink, 
!> @link DECLARATIONS_TELEMAC::KSORT KSORT@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, IK, J
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> GLOBAL_TO_LOCAL_BOUND(), OS()
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
!>    <td><center> 6.0                                    </center></td>
!>    <td> 18/03/2010                                              </td>
!>    <td> C. DENIS (SINETICS)                                     </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center> 5.1                                    </center></td>
!>    <td> 06/07/1999                                              </td>
!>    <td> D. AELBRECHT (LNH) 01.30.87.74.12                       </td>
!>    <td>                                                         </td>
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
                        SUBROUTINE MASQUE_ARTEMIS
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
C
      INTEGER IK
C
C-----------------------------------------------------------------------
C
C MASKS
C
C     INITIALISES (SETS TO 0) ALL MASKING VECTORS
C

      IF (NCSIZE .GT. 1) THEN
         MASK1T=0.0
         MASK2T=0.0
         MASK3T=0.0
         MASK4T=0.0

         DO 5 IK=1,NPTFR_TOT

            IF (LIHBORT(IK).EQ.KLOG) THEN
               MASK3T(IK) = 1.D0
            ELSEIF (LIHBORT(KP1BOR_TOT(IK)).NE.KLOG) THEN
               IF (LIHBORT(IK).EQ.KINC) THEN
                  MASK1T(IK) = 1.D0
               ENDIF
               IF (LIHBORT(IK).EQ.KSORT) THEN
                  MASK2T(IK) = 1.D0
               ENDIF
               IF (LIHBORT(IK).EQ.KENT) THEN
                  MASK4T(IK) = 1.D0
               ENDIF
            ELSE
               MASK3T(IK) = 1.D0
            ENDIF
 5       CONTINUE
C
C-----------------------------------------------------------------------
C
         CALL GLOBAL_TO_LOCAL_BOUND(MASK1T,MASK1,MESH%NPTFR,NPTFR_TOT)
         CALL GLOBAL_TO_LOCAL_BOUND(MASK2T,MASK2,MESH%NPTFR,NPTFR_TOT)
         CALL GLOBAL_TO_LOCAL_BOUND(MASK3T,MASK3,MESH%NPTFR,NPTFR_TOT)
         CALL GLOBAL_TO_LOCAL_BOUND(MASK4T,MASK4,MESH%NPTFR,NPTFR_TOT)
C         DEALLOCATE(MASK1T)
C         DEALLOCATE(MASK2T)
C         DEALLOCATE(MASK3T)
C         DEALLOCATE(MASK4T)

      ELSE
C MASKS
C
C     INITIALISES (SETS TO 0) ALL MASKING VECTORS
C
      CALL OS( 'X=C     ' , MASK1 , SBID , SBID , 0.D0 )
      CALL OS( 'X=C     ' , MASK2 , SBID , SBID , 0.D0 )
      CALL OS( 'X=C     ' , MASK3 , SBID , SBID , 0.D0 )
      CALL OS( 'X=C     ' , MASK4 , SBID , SBID , 0.D0 )
C
      DO IK=1,NPTFR
         LIHBOR%I(IK)=LIHBORT(IK)
      END DO

      DO 6 IK=1,NPTFR
C
         IF (LIHBOR%I(IK).EQ.KLOG) THEN
            MASK3%R(IK) = 1.D0
         ELSEIF (LIHBOR%I(MESH%KP1BOR%I(IK)).NE.KLOG) THEN
            IF (LIHBOR%I(IK).EQ.KINC) THEN
               MASK1%R(IK) = 1.D0
            ENDIF
            IF (LIHBOR%I(IK).EQ.KSORT) THEN
               MASK2%R(IK) = 1.D0
            ENDIF
            IF (LIHBOR%I(IK).EQ.KENT) THEN
               MASK4%R(IK) = 1.D0
            ENDIF
         ELSE
            MASK3%R(IK) = 1.D0
         ENDIF

 6    CONTINUE
      ENDIF
        RETURN
        CONTAINS
        SUBROUTINE GLOBAL_TO_LOCAL_BOUND(TAB1,OBJ,NPTFR,NPTFR_TOT)
        IMPLICIT NONE
        DOUBLE PRECISION, INTENT(INOUT)  :: TAB1(:)
      TYPE (BIEF_OBJ), INTENT(INOUT) :: OBJ
      INTEGER, INTENT(IN) :: NPTFR
      INTEGER, INTENT(IN) :: NPTFR_TOT
      INTEGER :: I,J
      OBJ%R=0.0
      DO I=1,NPTFR_TOT
         DO J=1,MESH%NPTFR
            IF (NBOR_TOT(I) .EQ. MESH%KNOLG%I(MESH%NBOR%I(J))) THEN
               OBJ%R(J)=TAB1(I)
            END IF
         END DO
      END DO
      OBJ%DIM1=NPTFR
      RETURN

      END SUBROUTINE


      END
C
C#######################################################################
C