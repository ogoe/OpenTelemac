          SUBROUTINE KS_SISYPHE(IKS,KS,KSP,KSR,KSPRATIO,HOULE,
     &           GRAV,XMVE,XMVS,VCE,
     &           HMIN,HN,ACLADM,UNORM,UW,TW,NPOIN)

C 
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C
      USE BIEF
C
      INTEGER,            INTENT(IN)  :: NPOIN,IKS
      LOGICAL,            INTENT(IN)  :: HOULE
      DOUBLE PRECISION,   INTENT(IN)  :: XMVE,XMVS, VCE,GRAV
      DOUBLE PRECISION,   INTENT(IN)  :: HMIN,KSPRATIO
      TYPE(BIEF_OBJ), INTENT(IN)      :: HN,UNORM
      TYPE(BIEF_OBJ), INTENT(IN)      :: TW,UW
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: KS,KSP,KSR
      TYPE(BIEF_OBJ), INTENT(IN)      :: ACLADM      

C
C
C +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  BED ROUGHNESS PREDICTOR
C                         SKIN   : KSP
C                         TOTAL  : KS
C                         RIPPLES : KSR
C                         KS PUT IN CHESTR IF NO COUPLING, RE-COMPUTED OTHERWISE
C  NOTE: IT IS RECOMMENDED TO USE FRICTION LAW NO 3 WHEN COUPLING TO
C        AVOID UNNECESSARY COMPUTATION
C +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C SKIN BED ROUGHNESS --> KSP
C
C RIPPLED BED ROUGHNESS --> KSR =KSP
C
C
C TOTAL BED ROUGHNESS --> KS
C       IKS= 0: FLAT BED     KS=KSP
C       IKS = 1: RIPPLED BED KS= KSP + KSR 
C       IKS=3 :  DUNED BED   KS= KSP +KSR +KSMR +KSD

         IF(IKS.EQ.0) THEN
           CALL OS('X=Y     ', X=KS, Y=KSP)
         ENDIF
C
         IF(IKS.EQ.1) THEN
C
           IF(HOULE) THEN
C WIBERG AND HARRIS: KSR (RIPPLES)
C                    KS (RIPPLES + SKIN)
              CALL RIDE(KSR%R, TW%R, UW%R, UNORM%R, GRAV, XMVE,
     &                XMVS, VCE, NPOIN, KSPRATIO, ACLADM%R)
              CALL OS('X=Y+Z   ', X=KS, Y=KSP, Z=KSR)
           ELSE
C VR PREDICTOR : KSR (RIPPLES)
              CALL RIDE_VR(KSR%R,KS%R,UNORM%R,HN%R,GRAV,XMVE,
     &                     XMVS,NPOIN,ACLADM%R)
              CALL OS('X=Y+Z   ', X=KS, Y=KSP, Z=KSR)
           ENDIF
C
         ENDIF  
C
          IF(IKS.EQ.3) THEN            
C VR PREDICTOR : KSR (RIPPLES)
              CALL RIDE_VR(KSR%R,KS%R,UNORM%R,HN%R,GRAV,XMVE,
     &                     XMVS,NPOIN,ACLADM%R)
              CALL OS('X=X+Y   ', X=KS, Y=KSP)
           ENDIF
C 

        END
C
