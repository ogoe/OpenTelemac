!                       *************************
                        SUBROUTINE COLLECT_VALUES
!                       *************************
!
     &(X,Y,ZF,HN,NTRAC,T,IP,XP,YP,ZFP,HP,SLP,TRP)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   09/07/2013
!***********************************************************************
!
!brief    COLLECT DIFFERENT VALUES ON A NODE
!+
!
!+
!history  C.COULET (ARTELIA)
!+        09/07/2013
!+        V6P3
!+   Creation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| HN             |-->| DEPTH AT TIME T(N)
!| HP             |-->| DEPTH OF NODE IP
!| IP             |-->| INDEX OF THE INTERESTING NODE
!| NTRAC          |-->| NUMBER OF TRACERS
!| SLP            |<--| FREE SURFACE LEVEL OF NODE IP
!| T              |-->| BLOCK OF TRACERS
!| TRP            |<--| ARRAY OF TRACERS VALUES ON NODE IP
!| X              |-->| ABSCISSAE OF NODES
!| XP             |<--| ABSCISSAE OF NODE IP
!| Y              |-->| ORDINATES OF NODES
!| YP             |<--| ORDINATES OF NODE IP
!| ZF             |-->| BOTTOM
!| ZFP            |<--| BOTTOM LEVEL OF NODE IP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER, INTENT(IN)           :: NTRAC, IP
      DOUBLE PRECISION, INTENT(IN)  :: X(*),Y(*),ZF(*),HN(*)
      TYPE(BIEF_OBJ)  , INTENT(IN)  :: T
      DOUBLE PRECISION, INTENT(OUT) :: XP,YP,ZFP,HP,SLP
      DOUBLE PRECISION, INTENT(OUT) :: TRP(NTRAC)
!
      INTEGER ITRAC
!
      DOUBLE PRECISION P_DMAX,P_DMIN
      EXTERNAL         P_DMAX,P_DMIN
!
!-----------------------------------------------------------------------
!
      IF(IP.GT.0) THEN
        ZFP = ZF(IP)
        HP  = HN(IP)
        SLP = HP + ZFP
        DO ITRAC = 1, NTRAC
          TRP(ITRAC) = T%ADR(ITRAC)%P%R(IP)
        ENDDO
        XP = X(IP)
        YP = Y(IP)
      ELSE
        ZFP = 0.D0
        HP  = 0.D0
        SLP = 0.D0
        DO ITRAC = 1, NTRAC
          TRP(ITRAC) = 0.D0
        ENDDO
        XP = 0.D0
        YP = 0.D0
      ENDIF
!
      IF(NCSIZE.GT.1) THEN
        ZFP=P_DMAX(MAX(ZFP,0.D0))-P_DMIN(MAX(-ZFP,0.D0))
        HP =P_DMAX(MAX(HP ,0.D0))-P_DMIN(MAX(-HP ,0.D0))
        SLP=P_DMAX(MAX(SLP,0.D0))-P_DMIN(MAX(-SLP,0.D0))
        XP =P_DMAX(MAX(XP ,0.D0))-P_DMIN(MAX(-XP ,0.D0))
        YP =P_DMAX(MAX(YP ,0.D0))-P_DMIN(MAX(-YP ,0.D0))
        DO ITRAC = 1, NTRAC
           TRP(ITRAC) = P_DMAX(MAX( TRP(ITRAC),0.D0))
     &                - P_DMIN(MAX(-TRP(ITRAC),0.D0))
        ENDDO
      ENDIF
!
      RETURN
      END