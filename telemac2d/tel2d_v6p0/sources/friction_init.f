C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES FRICTION BY ZONE INITIALISATION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC2D, FRICTION_DEF, INTERFACE_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_TELEMAC2D :<br>
!> @link DECLARATIONS_TELEMAC2D::CF CF@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CHBORD CHBORD@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CHESTR CHESTR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::FRTAB FRTAB@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ITURB ITURB@endlink, 
!> @link DECLARATIONS_TELEMAC2D::KFROPT KFROPT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::KFROT KFROT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::KFRO_B KFRO_B@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LINDDP LINDDP@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LINDNER LINDNER@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LINDSP LINDSP@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LISRUG LISRUG@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MESH MESH@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NDEFMA NDEFMA@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NDEF_B NDEF_B@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NKFROT NKFROT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NPOIN NPOIN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NZONES NZONES@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NZONMX NZONMX@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T2D_FILES T2D_FILES@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> FRICTION_ERR, I, J, K
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> FRICTION_READ(), FRICTION_USER(), PLANTE()
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
!>      <td><center> 6.0                                       </center>
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
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FRICTION_INIT
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE FRICTION_DEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      !2/ LOCAL VARIABLES
      !------------------
      INTEGER :: I, J, K
      LOGICAL :: FRICTION_ERR
!
!=======================================================================!
!=======================================================================!
C                               PROGRAMME                               !
!=======================================================================!
!=======================================================================!
!
      CALL FRICTION_READ(T2D_FILES(T2DCOF)%LU,
     &                   NZONMX,ITURB,LISRUG,LINDNER,
     &                   T2D_FILES(T2DCOF)%NAME,NZONES,FRTAB)
!
      ! INITIALIZATION (ALL ELEMENTS WITH -1
      ! IN ORDER TO CHECK AFTER USER INITIALIZATION)
      ! --------------------------------------------
      DO I = 1, CF%DIM1
        KFROPT%I(I) = -1
      ENDDO
!
      ! USER INITIALIZATION
      ! -------------------
      CALL FRICTION_USER
!
      FRICTION_ERR = .FALSE.
!
      ! CHECK VALUE
      ! -----------
C FH : FOR QUASI-BUBBLE
C FH : 2004/03/01
! =>
      DO I=1, NPOIN
! <=
C FH : 2004/03/01
C FH : FOR QUASI-BUBBLE

         ! NO FRICTION ZONE DEFINED
         ! ------------------------
         IF (KFROPT%I(I) == -1) THEN

            FRICTION_ERR = .TRUE.
            IF(NCSIZE>1) THEN
              K = MESH%KNOLG%I(I)
            ELSE
              K = I
            ENDIF
            IF(LNG == 1) WRITE(LU,10) K
            IF(LNG == 2) WRITE(LU,11) K
!
         ! LOCAL NUMBERING OF THE ZONE
         ! ---------------------------
         ELSE
            DO J = 1, NZONES
               IF(KFROPT%I(I) == FRTAB%ADR(J)%P%GNUMB(1)) THEN
                  KFROPT%I(I) = J
                  EXIT
               ENDIF
               IF(J==NZONES) THEN
                  FRICTION_ERR = .TRUE.
                  IF (NCSIZE>1) THEN
                      K = MESH%KNOLG%I(I)
                  ELSE
                      K=I
                  ENDIF
                  IF (LNG==1) WRITE(LU,20) K,KFROPT%I(I)
                  IF (LNG==2) WRITE(LU,21) K,KFROPT%I(I)
               ENDIF
            ENDDO
         ENDIF
!
      ENDDO
!
10    FORMAT('AUCUNE ZONE DE FROTTEMENT DEFINI POUR LE NOEUD : ',I5)
11    FORMAT('NO FRICTION ZONE DEFINED FOR THE NODE : ',I5)
!
20    FORMAT('MAUVAISE INITIALISATION DE LA ZONE DE FROTTEMENT DU ',
     &       'NOEUD :',I5
     &     ,/' ZONE : ',I9,' INCONNUE')
21    FORMAT('WRONG INITIALIZATION OF THE FRICTION ZONE FOR THE NODE :'
     &     ,  I5
     &     ,/' ZONE : ',I9,' UNKNOWN')
!
      IF(FRICTION_ERR) THEN
        CALL PLANTE(1)
        STOP
      ENDIF
!
C FH : FOR QUASI-BUBBLE
C FH : 2004/03/01
! =>
      ! VECTOR INITIALIZATION : WHOLE DOMAIN
      ! (FOR QUASI_BUBBLE, SEE FRICTION_CHOICE.F : CALL FRICTION_BUBBLE)
      ! ----------------------------------------------------------------
      IF (LINDNER) THEN
         DO I = 1, NPOIN
            CHESTR%R(I) = FRTAB%ADR(KFROPT%I(I))%P%RCOEF(1)
            NDEFMA%R(I) = FRTAB%ADR(KFROPT%I(I))%P%NDEF (1)
            NKFROT%I(I) = FRTAB%ADR(KFROPT%I(I))%P%RTYPE(1)
            LINDDP%R(I) = FRTAB%ADR(KFROPT%I(I))%P%DP
            LINDSP%R(I) = FRTAB%ADR(KFROPT%I(I))%P%SP
         ENDDO
      ELSE
         DO I = 1, NPOIN
            CHESTR%R(I) = FRTAB%ADR(KFROPT%I(I))%P%RCOEF(1)
            NDEFMA%R(I) = FRTAB%ADR(KFROPT%I(I))%P%NDEF (1)
            NKFROT%I(I) = FRTAB%ADR(KFROPT%I(I))%P%RTYPE(1)
         ENDDO
      ENDIF
!
      ! VECTOR INITIALIZATION : BOUNDARY CONDITIONS
      ! -------------------------------------------
      IF ((ITURB == 3).AND.(LISRUG == 2)) THEN
         DO J = 1, MESH%NPTFR
            I = MESH%NBOR%I(J)
            CHBORD%R(J) = FRTAB%ADR(KFROPT%I(I))%P%RCOEF(2)
            NDEF_B%R(J) = FRTAB%ADR(KFROPT%I(I))%P%NDEF (2)
            KFRO_B%I(J) = FRTAB%ADR(KFROPT%I(I))%P%RTYPE(2)
         ENDDO
      ENDIF
!
! <=
C FH : 2004/03/01
C FH : FOR QUASI-BUBBLE
      ! KFROT IS USED IN ORDER TO KNOW
      ! HOW MANY ZONE HAVES A FRICTION COEFFCIENT
      ! -----------------------------------------
      KFROT = 0
      DO I =1, NZONES
        IF(FRTAB%ADR(I)%P%RTYPE(1).NE.0) KFROT = KFROT + 1
      ENDDO
!
!=======================================================================!
!=======================================================================!
!
      RETURN
      END SUBROUTINE FRICTION_INIT
C
C#######################################################################
C