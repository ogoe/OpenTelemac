!                    ********************
                     SUBROUTINE SPALALLCL
!                    ********************
     &(NUBOR, LIMSA, LIUBOR, NPTFR,NUMIN,PROPNU,
     & KNEU, KDIR, KENT, KENTU, KADH, KLOG,KSORT)
!
!***********************************************************************
! TELEMAC2D   V7P0                                  31/08/2015
!***********************************************************************
!
!brief    BOUNDARY CONDITIONS FOR SPALART-ALLMARAS MODEL.
!
!history  A BOURGOIN (LNHE)
!+        31/08/2015
!+        V7p0
!+
! warning: to be improved: here we consider no friction on the walls
!          while the friction has non negligeable effects on the
!          production and destruction terms
! 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| KADH           |-->| CONVENTION FOR NO SLIP BOUNDARY CONDITION
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KENTU          |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VELOCITY
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| KNEU           |-->| CONVENTION FOR NEUMANN CONDITION
!| KENTU          |-->| CONVENTION FOR LIQUID INPUT WITH FREE OUTPUT
!| LIMSA          |-->| BOUNDARY CONDITIONS ON VISCSA
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON VELOCITY
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NUBOR          |<--| SPALART ALLAMRAS VISCOSITY ON BOUNDARIES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TELEMAC2D, EX_SPALALLCL => SPALALLCL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)      :: NPTFR
      INTEGER, INTENT(IN)      :: KNEU, KDIR,KENT,KADH, KLOG,KENTU,KSORT
      INTEGER, INTENT(IN)      :: LIMSA(NPTFR), LIUBOR(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: NUBOR(*)
      DOUBLE PRECISION, INTENT(IN) :: NUMIN,PROPNU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: K
!
!-----------------------------------------------------------------------
!
!=======================================================================
!
!  LOOP ON THE BOUNDARY NODES
!
!  COMPUTES NUBOR
!
!=======================================================================

      DO K=1, NPTFR
        NUBOR(K)=NUMIN
!
!  DIRICHLET ON VISCSA
!
        IF(LIMSA(K).EQ.KDIR) THEN
          IF(LIUBOR(K).EQ.KENT.OR.LIUBOR(K).EQ.KENTU) THEN
!
!           INPUT BOUNDARY: TURBULENCE DUE TO THE BOTTOM
!
              NUBOR(K)=NUMIN
          ELSEIF(LIUBOR(K).EQ.KLOG.OR.LIUBOR(K).EQ.KADH) THEN
!
!           WALL
!
             NUBOR(K)=NUMIN
          ELSE
            IF(LNG.EQ.1) WRITE(LU, 500) K, LIUBOR(K)
            IF(LNG.EQ.2) WRITE(LU, 501) K, LIUBOR(K)
500         FORMAT(1X,'SPALALLCL: POINT DE BORD ',1I6,
     &                'CAS NON PREVU POUR NUBOR',1X,'LIUBOR=',1I6)
501         FORMAT(1X,'SPALALLCL: BOUNDARY POINT ',1I6,
     &                'UNKNOWN CASE FOR NUBOR',1X,'LIUBOR=',1I6)
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF

      ENDDO

      RETURN
      END SUBROUTINE
