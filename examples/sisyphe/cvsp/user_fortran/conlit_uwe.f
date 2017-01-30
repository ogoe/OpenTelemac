!                    *********************
                     SUBROUTINE CONLIT_UWE
!                    *********************
!
     &(NBOR,AT)
!
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
!| AT             |-->| TEMPS (s)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE
      USE DECLARATIONS_TELEMAC
      USE M1_RECIRCMODULE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN):: NBOR(NPTFR)
! CV: 12/06...
      DOUBLE PRECISION, INTENT(IN) :: AT
      DOUBLE PRECISION, EXTERNAL:: CGL
!...CV
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,K,KK,IFRLIQ,IRANK, NSIC
      DOUBLE PRECISION INFLOWWIDTH
!
!-----------------------------------------------------------------------
!


!     CALCULATING THE WITH OF THE INFLOW, OUR SEDIMENT INPUT HAS TO BE DISTRIBUTED OVER IT
        INFLOWWIDTH = 0.D0
      DO K=1,NPTFR
              KK = MESH%KP1BOR%I(K)
          IF (K.NE.KK) THEN
              IF((NUMLIQ%I(K).EQ.2).AND.(NUMLIQ%I(KK).EQ.2)) THEN ! FLUX FOR ONE CLASS
                IF(LIEBOR%I(K).EQ.4.AND.LIHBOR%I(K).EQ.4) THEN
                INFLOWWIDTH = INFLOWWIDTH + MESH%LGSEG%R(K)
!                PRINT*,'INFLOWWIDTH',K,MESH%LGSEG%R(K),INFLOWWIDTH
!     &           ,MESH%X%R(MESH%NBOR%I(K)),MESH%X%R(MESH%NBOR%I(KK))
                ENDIF
              ENDIF ! FLUX FOR ONE CLASS
          ENDIF
      ENDDO
!
      DO  K=1,NPTFR
!
        I = NBOR(K)
!
!       HERE KADH (WALL WITH NO SLIP CONDITION) IS CHANGED INTO KLOG (WALL)
!
        IF(LIEBOR%I(K).EQ.KADH) THEN
          LIEBOR%I(K)= KLOG
        ENDIF
!
!       DIRICHLET CONDITIONS
!       EITHER ON EVOLUTION OR ON SOLID DISCHARGE
!
!       EXAMPLE 1: IMPOSED SOLID DISCHARGE - FREE BED EVOLUTION
!
!       QBOR%ADR(J)%P%R(K) IS THE SOLID DISCHARGE IMPOSED AT THE BOUNDARY
!       NODE K , CLASS OF SEDIMENT J, EXCLUDING VOIDS
!
!
        ! RECIRC INPUT ....
        IF (NUMLIQ%I(K).EQ.2) THEN
          LIEBOR%I(K)=KSORT
          LIQBOR%I(K)=KENT
          PRINT *, 'QBOR%ADR(NSIC)%P%R(K),Q_OUTCLA(NSIC),INFLOWWIDTH,LT'
          DO NSIC = 1,NSICLA
            QBOR%ADR(NSIC)%P%R(K)= (Q_OUTCLA(NSIC) / INFLOWWIDTH)
            PRINT *, QBOR%ADR(NSIC)%P%R(K),Q_OUTCLA(NSIC),INFLOWWIDTH,LT
          ENDDO
        ENDIF
!
!       EXAMPLE 2: IMPOSED BED EVOLUTON
!
!       LIEBOR%I(K)=KENT
!       (LIQBOR%I(K)=KSORT IS DONE IN SISYPHE.F)
!       IF(LIEBOR%I(K).EQ.KENT) THEN
!         EBOR%ADR(1)%P%R(K)=1.D-4
!         EBOR%ADR(2)%P%R(K)=1.D-4.....
!       ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!     LICBOR : BOUNDARY CONDITION FOR SEDIMENT CONCENTRATION
!-----------------------------------------------------------------------
!
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
! CV 12/06 READING BOUNDARY CONDITION FILE
!
          IF(LICBOR%I(K).EQ.KENT.AND.
     &               SIS_FILES(SISLIQ)%NAME(1:1).NE.' ') THEN
!
            IF(IFRLIQ.GT.0) THEN
              DO I=1,NSICLA
                 CBOR%ADR(I)%P%R(K) = CGL(IFRLIQ,AT)/XMVS
              ENDDO
            ENDIF
          ENDIF
!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE

