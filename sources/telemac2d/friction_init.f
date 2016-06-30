!                    ************************
                     SUBROUTINE FRICTION_INIT
!                    ************************
!
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES FRICTION BY ZONE INITIALISATION.
!
!history  F. HUVELIN
!+        20/04/2004
!+
!+
!
!history  J-M HERVOUET (LNHE)
!+
!+        V6P0
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE FRICTION_DEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      !2/ LOCAL VARIABLES
      !------------------
      INTEGER :: I, J, K
      LOGICAL :: FRICTION_ERR
!
!=======================================================================!
!=======================================================================!
!                               PROGRAMME                               !
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
! FH : FOR QUASI-BUBBLE
! FH : 2004/03/01
! =>
      DO I=1, NPOIN
! <=
! FH : 2004/03/01
! FH : FOR QUASI-BUBBLE
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
! FH : FOR QUASI-BUBBLE
! FH : 2004/03/01
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
      IF(LISRUG.EQ.2) THEN
        DO J = 1, MESH%NPTFR
          I = MESH%NBOR%I(J)
          CHBORD%R(J) = FRTAB%ADR(KFROPT%I(I))%P%RCOEF(2)
          NDEF_B%R(J) = FRTAB%ADR(KFROPT%I(I))%P%NDEF (2)
          KFRO_B%I(J) = FRTAB%ADR(KFROPT%I(I))%P%RTYPE(2)
        ENDDO
      ENDIF
!
! <=
! FH : 2004/03/01
! FH : FOR QUASI-BUBBLE
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
