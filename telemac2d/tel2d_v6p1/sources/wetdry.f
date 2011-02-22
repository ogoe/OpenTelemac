C                       ******************                              
                        SUBROUTINE WETDRY
C                       ******************                               
C                                                                       
     *(ETA1,Z1,H1,U1,V1,ETA2,Z2,H2,U2,V2,EPS)
C                                                                       
C***********************************************************************
C  TELEMAC 2D VERSION 5.9                                         R. ATA
C-----------------------------------------------------------------------
C                 WETTING DRYING TREATMENT FOR FINITE VOLUMES
C   
C                 CALLED ONLY FOR ZOKAGOA AND TCHAMEN
C     
C-----------------------------------------------------------------------
C                             ARGUMENTS                                 
C .________________.____.______________________________________________.
C !      NOM       !MODE!                   ROLE                       !
C !________________!____!______________________________________________!
C |  ETA           |<-->|  FREE SURFACE                                |
C |  Z             |<-->|  BATHYMETRY                                  |
C |  H             |<-->|  WATER DEPTH                                 |
C |  U             |<-->|  VELOCITY NORMAL COMPENENT                   |
C |  V             |<-->|  VELOCITY TANGENTIAL COMPENENT               |
C |  EPS           | -->|  TOLERANCE FOR WATER DEPTH                   |
C !________________!____!______________________________________________!
C  MODE: -->(NON MODIFIED DATA),<--(OUTPUT),<-->(MODIFIED DATA)             
C-----------------------------------------------------------------------
C     - CALLING SUBROUTINE: FLU_ZOKA.F                              
C 
C***********************************************************************
C 
      USE BIEF
C  
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, INTENT(IN)    :: EPS
      DOUBLE PRECISION, INTENT(INOUT)   :: ETA1,Z1,H1,U1,V1
      DOUBLE PRECISION, INTENT(INOUT)   :: ETA2,Z2,H2,U2,V2
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I
      DOUBLE PRECISION ENG
      DOUBLE PRECISION, PARAMETER :: DEUG=19.62D0
C
C***********************************************************************
     
      ENG = 0.0D0
C FIRST CASE: 
      IF(ETA1.LT.Z2)THEN
        IF((H1.GT.EPS).AND.(H2.LT.EPS))THEN
           IF(U1.NE.0.0D0)THEN 
C             COMPUTE THE ENERGY:U^2/2g + H + Z
              ENG = (U1*U1/DEUG) + ETA1
C             ENG MUST BE > Z2 
              IF(ENG.GT.Z2)THEN
                Z2   = ETA1
                ETA2 = ETA1
                H2 = 0.0D0
                U2 = 0.0D0
                V2 = 0.0D0
              ENDIF
           ENDIF
        ENDIF
      ENDIF

C SECOND CASE :
      IF(ETA2.LT.Z1)THEN
        IF((H1.LT.EPS).AND.(H2.GT.EPS))THEN
           IF(U2.NE.0.0D0)THEN 
C             COMPUTE THE ENERGY:U^2/2g + H + Z
              ENG = (U2*U2/DEUG) + ETA2
C             ENG MUST BE > Z1 
              IF(ENG.GT.Z1)THEN
                Z1   = ETA2
                ETA1 = ETA2
                H1 = 0.0D0
                U1 = 0.0D0
                V1 = 0.0D0
              ENDIF
           ENDIF
        ENDIF
      ENDIF

      END SUBROUTINE