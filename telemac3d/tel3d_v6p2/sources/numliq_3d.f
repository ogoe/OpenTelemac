!                    ********************
                     SUBROUTINE NUMLIQ_3D
!                    ********************
!
     &(NUMLIQ,NUMLIQ_ELM,NPLAN,NPTFR2,KP1BOR)
!
!***********************************************************************
! TELEMAC3D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    EXTRUDES THE 2D ARRAY NUMLIQ TO 3D, FOR POINTS AND ELEMENTS
!
!history  J.M. HERVOUET  (LNH)
!+        19/09/2011
!+        V6P2
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| KP1BOR         |-->| GIVES THE NEXT BOUNDARY POINT IN A CONTOUR
!| MASK           |-->| 2D MASK
!| MASKBR         |<->| 3D MASK ON LATERAL BOUNDARIES
!| NETAGE         |-->| NUMBER OF PLANES - 1
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS IN 2D
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NPTFR2,NPLAN
      INTEGER, INTENT(IN)           :: KP1BOR(NPTFR2)
      INTEGER, INTENT(INOUT)        :: NUMLIQ(NPTFR2*NPLAN)
      TYPE(BIEF_OBJ), INTENT(INOUT) :: NUMLIQ_ELM
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,K,KP1,IETAGE
!
!-----------------------------------------------------------------------
!
!     EXTENDING NUMLIQ ON THE VERTICAL
!
      DO I=2,NPLAN
        DO K=1,NPTFR2
          NUMLIQ((I-1)*NPTFR2+K)=NUMLIQ(K)
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
!     BUILDING NUMLIQ_ELM
!
      DO I=1,NUMLIQ_ELM%DIM1
        NUMLIQ_ELM%I(I)=0
      ENDDO
!
!     PROVISIONAL (SHOULD BE DONE BY LOOKING AT IKLBOR)
!     AN ELEMENT IS IN BOUNDARY I IF ALL ITS POINTS ARE IN BOUNDARY I
!
      IF(NUMLIQ_ELM%ELM.EQ.70) THEN
!
!         QUADRILATERAL ON THE LATERAL BOUNDARIES
!
          DO K = 1,NPTFR2
            KP1=KP1BOR(K)
!           THIS IS FOR PARALLELISM (IF SEGMENT IN ANOTHER DOMAIN
!                                    KP1 IS SET TO K)
            IF(KP1.NE.K) THEN
!             ELEMENTS BETWEEN TWO POINTS OF THE SAME BOUNDARY
              IF(NUMLIQ(K).EQ.NUMLIQ(KP1)) THEN
                DO IETAGE = 1,NPLAN-1
                  NUMLIQ_ELM%I((IETAGE-1)*NPTFR2+K)=NUMLIQ(K)
                ENDDO
              ENDIF
            ENDIF
          ENDDO
!
      ELSEIF(NUMLIQ_ELM%ELM.EQ.60) THEN
!
!         TRIANGLES ON THE LATERAL BOUNDARIES
!
          DO K = 1,NPTFR2
            KP1=KP1BOR(K)
!           THIS IS FOR PARALLELISM (IF SEGMENT IN ANOTHER DOMAIN
!                                    KP1 IS SET TO K)
            IF(KP1.NE.K) THEN
!             ELEMENTS BETWEEN TWO POINTS OF THE SAME BOUNDARY
              IF(NUMLIQ(K).EQ.NUMLIQ(KP1)) THEN
                DO IETAGE = 1,NPLAN-1
                  NUMLIQ_ELM%I((IETAGE-1)*2*NPTFR2+K       )=NUMLIQ(K)
                  NUMLIQ_ELM%I((IETAGE-1)*2*NPTFR2+K+NPTFR2)=NUMLIQ(K)
                ENDDO
              ENDIF
            ENDIF
          ENDDO
!
      ELSE
        WRITE(LU,*) 'NUMLIQ_3D: UNKNOWN ELEMENT FOR NUMLIQ_ELM'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
