!                    ****************************
                     SUBROUTINE AD_ALLVEC_ARTEMIS
!                    ****************************
!
     &(IVAR,ADOBJ)
!
!***********************************************************************
! ARTEMIS   V7P2
!***********************************************************************
!
!brief    ASSOCIATE THE PRINOUT ARRAY ADOBJ%R TO THE APPROPRIATE
!         DIFFERENTED VARIBALE.
!
!history  S.E.BOURBAN (HRW)
!+        20/06/2016
!+        V7P2
!+   First version for gradients
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IVAR       |-->| ONE SPECIFIC DIFERENTIABLE VARIABLE
!| ADOBJ      |<->| MOST LIKELY ONE OF THE VARSOR%ADR(I_ADVAR)%P
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_ARTEMIS
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: IVAR
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: ADOBJ
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SELECT CASE (IVAR)
!! COMPAD-DCO  BEGIN  JR2016
!! COMPAD-DCO  original code
!!#if defined COMPAD_DCO_T1S
!!      CASE(1)
!!!?        CALL DCO_T1S_GET( PHIR%R, ADOBJ%R, 1 )
!!        ADOBJ%R => PHIR%R
!!      CASE(2)
!!!?        CALL DCO_T1S_GET( PHII%R, ADOBJ%R, 1 )
!!        ADOBJ%R => PHII%R
!!      CASE(3)
!!!?        CALL DCO_T1S_GET( HHO%R, ADOBJ%R, 1 )
!!        ADOBJ%R => HHO%R
!!      CASE(4)
!!!?        CALL DCO_T1S_GET( PHAS%R, ADOBJ%R, 1 )
!!        ADOBJ%R => PHAS%R
!!      CASE(5)
!!!?        CALL DCO_T1S_GET( H%R, ADOBJ%R, 1 )
!!        ADOBJ%R => C%R
!!#endif
!!      CASE DEFAULT
!!        IF(LNG.EQ.1) THEN
!!          WRITE(LU,*) 'ERREUR : IDENTIFIANT DIFFERENTIE INCONNU ',IVAR
!!          WRITE(LU,*) '  LA ROUTINE AD_ALLVEC_ARTEMIS OU LE NOMBRE DE'
!!          WRITE(LU,*) '  VARIABLES DIFFERENCIEES SONT A METTRE A JOUR'
!!          WRITE(LU,*) '  -- OU BIEN LA CONFIGURATION NE S''APPLIQUE PAS'
!!          WRITE(LU,*) '  A LA DIFFERENTIATION ALGORITHMIQUE.'
!!        ELSEIF(LNG.EQ.2) THEN
!!          WRITE(LU,*) 'ERROR: UNKNOWN DIFFERENTIATED VARIABLE ',IVAR
!!          WRITE(LU,*) '  UPDATE EITHER SUBROUTINE AD_ALLVEC_ARTEMIS'
!!          WRITE(LU,*) '  OR THE NUMBER OF DIFERENTIATED  VARIABLES'
!!          WRITE(LU,*) '  -- OR ALGORITHMIC DIFFERENTIATION MIGHT NOT BE'
!!          WRITE(LU,*) '  CONFIGURED.'
!!        ENDIF
!!        CALL PLANTE(1)
!!      END SELECT
!! COMPAD-DCO  END    JR2016
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE AD_ALLVEC_ARTEMIS
