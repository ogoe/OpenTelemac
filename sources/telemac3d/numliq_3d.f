!                    ********************
                     SUBROUTINE NUMLIQ_3D
!                    ********************
!
     &(NUMLIQ,NUMLIQ_ELM,NPLAN,NPTFR2,IKLBOR,NELEB,NELEBX)
!
!***********************************************************************
! TELEMAC3D   V7P0                                   19/03/2014
!***********************************************************************
!
!brief    EXTRUDES THE 2D ARRAY NUMLIQ TO 3D, FOR POINTS AND ELEMENTS
!
!history  J.M. HERVOUET  (LNH)
!+        19/09/2011
!+        V6P2
!+
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        19/03/2014
!+        V7P0
!+   Boundary segments have now their own numbering, independent of
!+   boundary points numbering.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IKLBOR         |-->| CONNECTIVITY OF BOUNDARY SEGMENTS IN 2D
!| MASK           |-->| 2D MASK
!| MASKBR         |<->| 3D MASK ON LATERAL BOUNDARIES
!| NELEB          |-->| NUMBER OF BOUNDARY ELEMENTS
!| NELEBX         |-->| MAXIMUM NUMBER OF BOUNDARY ELEMENTS
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
      INTEGER, INTENT(IN)           :: NPTFR2,NPLAN,NELEB,NELEBX
      INTEGER, INTENT(IN)           :: IKLBOR(NELEBX,2)
      INTEGER, INTENT(INOUT)        :: NUMLIQ(NPTFR2*NPLAN)
      TYPE(BIEF_OBJ), INTENT(INOUT) :: NUMLIQ_ELM
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,K,KP1,IETAGE,IELEB
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
      IF(NUMLIQ_ELM%ELM.EQ.70) THEN
!
!       QUADRILATERAL ON THE LATERAL BOUNDARIES
!
        DO IELEB = 1,NELEB
          NUMLIQ_ELM%I(IELEB)=0
          K  =IKLBOR(IELEB,1)
          KP1=IKLBOR(IELEB,2)
!         ELEMENTS BETWEEN TWO POINTS OF THE SAME BOUNDARY
          IF(NUMLIQ(K).EQ.NUMLIQ(KP1)) THEN
            DO IETAGE = 1,NPLAN-1
              NUMLIQ_ELM%I((IETAGE-1)*NELEB+IELEB)=NUMLIQ(K)
            ENDDO
          ENDIF
        ENDDO
!
      ELSEIF(NUMLIQ_ELM%ELM.EQ.60) THEN
!
!       TRIANGLES ON THE LATERAL BOUNDARIES
!
        DO IELEB = 1,NELEB
          NUMLIQ_ELM%I(IELEB)=0
          K  =IKLBOR(IELEB,1)
          KP1=IKLBOR(IELEB,2)
!         ELEMENTS BETWEEN TWO POINTS OF THE SAME BOUNDARY
          IF(NUMLIQ(K).EQ.NUMLIQ(KP1)) THEN
            DO IETAGE = 1,NPLAN-1
              NUMLIQ_ELM%I((IETAGE-1)*2*NELEB+K      )=NUMLIQ(K)
              NUMLIQ_ELM%I((IETAGE-1)*2*NELEB+K+NELEB)=NUMLIQ(K)
            ENDDO
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
