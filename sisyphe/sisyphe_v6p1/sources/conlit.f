!                    *****************
                     SUBROUTINE CONLIT
!                    *****************
!
     &(NBOR)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    ALLOWS TO IMPOSE TIME VARYING BOUNDARY CONDITIONS
!+               (CONSTANT VALUES CAN BE DIRECTLY IMPOSED IN CONDIM
!+                INPUT FILE).
!+
!+
!+            ALLOWS TO IMPOSE A SAND TRANSPORT RATE AT SOME
!+                BOUNDARY NODES (QBOR AND LIQBOR). IT IS THEN NECESSARY
!+                TO ALSO IMPOSE LIEBOR = KSORT AT THESE NODES !
!
!history  E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!+        11/09/1995
!+
!+
!
!history  C. MACHET
!+        07/06/2002
!+
!+
!
!history  CV
!+        19/06/2008
!+        V5P9
!+   TAKES INTO ACCOUNT CBOR_VASE AND CBOR_SABLE
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
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE
      USE DECLARATIONS_TELEMAC
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN):: NBOR(NPTFR)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,K,IFRLIQ,IRANK
!
!-----------------------------------------------------------------------
!
      DO  K=1,NPTFR
!
        I = NBOR(K)
!
!
!  DIRICHLET CONDITIONS
!  +++++++++++++++++++++
!
        IF(LIEBOR%I(K).EQ.KADH) THEN
          LIEBOR%I(K)= KLOG
        ENDIF
!
! IMPOSED SOLID DISCHARGE - FREE BED EVOLUTION
! ++++++++++++++++++++++++++++++++++++++++++++
! QBOR%ADR(J)%P%R(K) IS THE SOLID DISCHARGE IMPOSED AT THE BOUNDARY
!                   NODE K , CLASS OF SEDIMENT J
!
!               LIEBOR%I(K)=KSORT
!               LIQBOR%I(K)=KENT
!
!               QBOR%ADR(1)%P%R(K)=1.D-4
!               QBOR%ADR(2)%P%R(K)=1.D-4 .....
!
!  IMPOSED BED EVOLUTON
! +++++++++++++++++++++
!          IF (LIEBOR%I(K).EQ.KENT) THEN
!               EBOR%ADR(1)%P%R(K)=1.D-4
!               EBOR%ADR(2)%P%R(K)=1.D-4.....
!         ENDIF
!
       ENDDO
!
!-----------------------------------------------------------------------
!     LICBOR : BOUNDARY CONDITION FOR SEDIMENT CONCENTRATION
!-----------------------------------------------------------------------
      IF(SUSP) THEN
!
        DO K=1,NPTFR
!
!         SO FAR LICBOR=LIEBOR (WITH KADH CHANGED INTO KLOG, SEE ABOVE,
!                               BUT CAN BE CHANGED)
!
          LICBOR%I(K) = LIEBOR%I(K)
!
!         ENTRANCE : IMPOSED CONCENTRATION
!         -------------------------------
!
!         NOTE JMH: KSORT MUST BE TREATED ALSO BECAUSE SUBROUTINE DIFFIN
!                   MAY CHANGE A KSORT INTO KENT, DEPENDING OF FLOW
!
          IFRLIQ=NUMLIQ%I(K)
          IF(LIEBOR%I(K).EQ.KENT.OR.LIEBOR%I(K).EQ.KSORT) THEN
            DO I=1,NSICLA
               IRANK=I+(IFRLIQ-1)*NSICLA
               CBOR%ADR(I)%P%R(K) = CBOR_CLASSE(IRANK)
            ENDDO
          ENDIF
!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CONLIT
