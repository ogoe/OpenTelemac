!                    *****************
                     SUBROUTINE EXTMSK
!                    *****************
!
     &(MASKBR,MASK,NPTFR,NETAGE)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    EXTRUDES THE 2D MASK ON THE VERTICAL FOR LATERAL
!+                BOUNDARIES.
!
!history  J.M. HERVOUET  (LNH)
!+        23/12/2003
!+        V5P5
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
      INTEGER, INTENT(IN)           :: NPTFR,NETAGE
      DOUBLE PRECISION, INTENT(IN)  :: MASK(*)
      TYPE(BIEF_OBJ), INTENT(INOUT) :: MASKBR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,IETAGE
!
!=======================================================================
!
!=======================================================================
!
      IF(MASKBR%ELM.EQ.70) THEN
!
!         QUADRILATERAL ON THE LATERAL BOUNDARIES
!
          DO K = 1,NPTFR
            DO IETAGE = 1,NETAGE
              MASKBR%R((IETAGE-1)*NPTFR+K)=MASK(K)
            ENDDO
          ENDDO
!
      ELSEIF(MASKBR%ELM.EQ.60) THEN
!
!         TRIANGLES ON THE LATERAL BOUNDARIES
!
          DO K = 1,NPTFR
            DO IETAGE = 1,NETAGE
              MASKBR%R((IETAGE-1)*2*NPTFR+K      )=MASK(K)
              MASKBR%R((IETAGE-1)*2*NPTFR+K+NPTFR)=MASK(K)
            ENDDO
          ENDDO
!
      ELSE
!
        WRITE(LU,*) 'UNKNOWN ELEMENT FOR MASKBR IN EXTMSK'
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
