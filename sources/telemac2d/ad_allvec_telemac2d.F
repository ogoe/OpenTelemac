!                   ******************************
                    SUBROUTINE AD_ALLVEC_TELEMAC2D
!                   ******************************
!
     &(IVAR,ADOBJ)
!
!***********************************************************************
! TELEMAC2D   V7P2
!***********************************************************************
!
!brief    ASSOCIATE THE ARRAY ADOBJ%R TO THE APPROPRIATE
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
      USE DECLARATIONS_TELEMAC2D
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
#if defined(COMPAD)
      CASE(1)
!?        CALL DCO_T1S_GET( H%R, ADOBJ%R, 1 )
        ADOBJ%R => H%R
      CASE(2)
!?        CALL DCO_T1S_GET( H%R, ADOBJ%R, 1 )
        ADOBJ%R => ZF%R
      CASE(3)
!?        CALL DCO_T1S_GET( H%R, ADOBJ%R, 1 )
        ADOBJ%R => U%R
      CASE(4)
!?        CALL DCO_T1S_GET( H%R, ADOBJ%R, 1 )
        ADOBJ%R => V%R
      CASE(5)
!?        CALL DCO_T1S_GET( H%R, ADOBJ%R, 1 )
!        ADOBJ%R => H%R%val
#endif
      CASE DEFAULT
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'ERREUR : IDENTIFIANT DIFFERENTIE INCONNU ',IVAR
          WRITE(LU,*) '  LA ROUTINE AD_ALLVEC_TELEMAC2D OU LE NOMBRE DE'
          WRITE(LU,*) '  VARIABLES DIFFERENCIEES SONT A METTRE A JOUR'
          WRITE(LU,*) '  -- OU BIEN LA CONFIGURATION NE S''APPLIQUE PAS'
          WRITE(LU,*) '  A LA DIFFERENTIATION ALGORITHMIQUE.'
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*) 'ERROR: UNKNOWN DIFFERENTIATED VARIABLE ',IVAR
          WRITE(LU,*) '  UPDATE EITHER SUBROUTINE AD_ALLVEC_TELEMAC2D'
          WRITE(LU,*) '  OR THE NUMBER OF DIFERENTIATED  VARIABLES'
          WRITE(LU,*) '  -- OR ALGORITHMIC DIFFERENTIATION MIGHT NOT BE'
          WRITE(LU,*) '  CONFIGURED.'
        ENDIF
        CALL PLANTE(1)
        STOP
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE AD_ALLVEC_TELEMAC2D
