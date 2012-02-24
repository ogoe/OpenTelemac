!                    *********************
                     SUBROUTINE INIT_MIXTE
!                    *********************
!
     &(XMVS,NPOIN,AVAIL,NSICLA,ES,ELAY,NOMBLAY,CONC_VASE,
     & MS_SABLE,MS_VASE,ZF,ZR,AVA0,CONC,NLAYER,DEBU)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/07/2011
!***********************************************************************
!
!brief
!
!history
!+
!+        V6P0
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables   
!+   
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AVA0           |-->| VOLUME PERCENT 
!| AVAIL          |<->| VOLUME PERCENT OF EACH CLASS
!| CONC_VASE      |<->| MUD CONCENTRATION FOR EACH LAYER
!| ELAY           |<->| THICKNESS OF SURFACE LAYER
!| ES             |<->| LAYER THICKNESSES AS DOUBLE PRECISION
!| MS_SABLE       |<->| MASS OF SAND PER LAYER (KG/M2)
!| MS_VASE        |<->| MASS OF MUD PER LAYER (KG/M2)
!| NOMBLAY        |-->| NUMBER OF LAYERS FOR CONSOLIDATION
!| NPOIN          |-->| NUMBER OF POINTS
!| NSICLA         |-->| NUMBER OF SIZE CLASSES FOR BED MATERIALS
!| XMVS           |-->| WATER DENSITY 
!| ZF             |-->| ELEVATION OF BOTTOM
!| ZR             |-->| NON ERODABLE BED
!| CONC           |<->| CONC OF EACH BED LAYER
!| NLAYER         |<->| NUMBER OB LAYERS 
!| DEBU           |-->| FLAG, FOR PREVIOUS SEDIMENTOLOGICAL FILE 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE, ONLY : IT1
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)              :: NPOIN,NSICLA,NOMBLAY
      TYPE (BIEF_OBJ),  INTENT(INOUT)  :: NLAYER
      DOUBLE PRECISION, INTENT(IN)     :: XMVS
      DOUBLE PRECISION, INTENT(INOUT)  :: AVAIL(NPOIN,NOMBLAY,NSICLA)
      DOUBLE PRECISION, INTENT(INOUT)  :: ES(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT)  :: ELAY(NPOIN)
      DOUBLE PRECISION, INTENT(IN)     :: ZR(NPOIN),ZF(NPOIN)
      DOUBLE PRECISION,  INTENT(INOUT) :: MS_SABLE(NPOIN,NOMBLAY)
      DOUBLE PRECISION,  INTENT(INOUT) :: MS_VASE(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(IN)     :: CONC_VASE(NOMBLAY)
      DOUBLE PRECISION,  INTENT(INOUT) :: CONC(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(IN)     :: AVA0(NSICLA)
      LOGICAL, INTENT (IN)             :: DEBU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     LOCAL VARIABLES
!
      INTEGER I,J,K
      DOUBLE PRECISION HAUTSED,TEST1
!
!-----------------------------------------------------------------------
!
!*******INITIAL SEDIMENT COMPOSITION IS IDENTICAL AT EACH NODE
! DEFAULT INITIALISATION: ALL LAYERS ARE EMPTY EXCEPT BED LAYER
! OTHERWISE SET THICKNESS OF THE MUD LAYERS IN EPAI_VASE(I= 1, NCOUCH_TASS-1)
! 
!CV V6P2 ..
! 
!      IF(.NOT.DEBU)    
!          CALL INIT_COMPO_COH(NLAYER%R(I))
!      ELSE
!
!     COMPLETING THE LAST LAYER SO THAT SUM OF LAYERS = ZF-ZR
!
!
!       DO I=1,NPOIN
!        ELAY(I)=0.D0
!        IF(NOMBLAY.GT.1) THEN
!          DO J=1,NOMBLAY-1
!            ELAY(I)=ELAY(I)+ES(I,J)
!          ENDDO
!        ENDIF
!        DIFF= (ZF(I)-ZR(I)) - ELAY(I)
!        IF(DIFF.GE.0.D0) THEN
!          ES(I,NOMBLAY) = DIFF
!          ELAY(I) = ZF(I)-ZR(I)
!        ELSE
!          ES(I,NOMBLAY) = 0.D0
!          WRITE(LU,*) 'ERROR IN INIT-MIXTE: THE SUM OF THICKNESS'
!          WRITE(LU,*) 'OF BED LAYERS > ERODIBLE BED MATERIAL'
!          CALL PLANTE(1)
!          STOP
!        ENDIF
!      ENDDO
! CV V6P2: this part is identical than init_avai
! 
      IF(DEBU) THEN
!       TENTATIVE VALUE, THIS IS TO CHECK
!       ADDED BY JMH 30/06/2010
        DO J=1,NPOIN
          I=1
          DO K=2,NOMBLAY
            IF(ES(J,K).GT.0.D0) I = I + 1
          ENDDO
          NLAYER%I(J)=I
!         CHECKING ALL LAYERS AND CORRECTING AVAIL
!         DUE TO POSSIBLE SHIFT OF SINGLE PRECISION STORAGE
          DO K=1,NLAYER%I(J)
            TEST1=0.D0
            DO I=1,NSICLA
              TEST1=TEST1+AVAIL(J,K,I)
            ENDDO
            IF(TEST1.GT.1.D-10.AND.(TEST1-1.D0)**2.GT.1.D-10) THEN
              DO I=1,NSICLA
                AVAIL(J,K,I)=AVAIL(J,K,I)/TEST1
              ENDDO
            ENDIF
          ENDDO
        ENDDO
      ELSE
!
!  INITIALISATION OF ES : THICKNESS OF EACH LAYERS
!  INIT_COMPO_COH : composition of the sediment bed : thickness and concentrations
!        
        CALL INIT_COMPO_COH(IT1%I)
!
        DO J=1,NPOIN 
!
!         NOMBLAY IS THE MAXIMUM NUMBER OF LAYERS ALLOWED
!
          NLAYER%I(J) = IT1%I(J)
          IF(NLAYER%I(J).GT.NOMBLAY) THEN
            IF(LNG.EQ.1) WRITE(LU,1800) NOMBLAY
            IF(LNG.EQ.2) WRITE(LU,1815) NOMBLAY
            CALL PLANTE(1)
            STOP
          ENDIF
!
!       THE HEIGHT OF SEDIMENT (SUM OF ES) MUST NOT BE MORE THAN ZF-ZR
!       IF SO, THE HEIGHT OF THE LAST LAYER IS REDUCED
!       IF THERE ARE LAYERS UNDER ZR, THEY ARE NOT TAKEN INTO ACCOUNT
         HAUTSED = 0.D0
         DO K=1,IT1%I(J)
           IF(HAUTSED + ES(J,K) .GE. ZF(J) - ZR(J)) THEN
            ES(J,K) = ZF(J) - ZR(J) -  HAUTSED
            NLAYER%I(J) = K
            HAUTSED = HAUTSED + ES(J,K)
            GOTO 144
           ENDIF
           HAUTSED = HAUTSED + ES(J,K)
         ENDDO
144     CONTINUE
!
!       FOR CLEAN OUTPUTS
!
         IF(NLAYER%I(J).LT.NOMBLAY) THEN
          DO K=NLAYER%I(J)+1,NOMBLAY
            ES(J,K) = 0.D0
          ENDDO
         ENDIF
!
!        THE THICKNESS OF THE LAST LAYER IS ENLARGED SO THAT
!        THE HEIGHT OF SEDIMENT (SUM OF ES) IS EQUAL TO ZF-ZR
!
         IF(HAUTSED.LT.ZF(J)-ZR(J)) THEN
          ES(J,NLAYER%I(J))=ES(J,NLAYER%I(J))+ZF(J)-ZR(J)-HAUTSED
         ENDIF
!
       ENDDO
      ENDIF
!
! END LOOP  (initialization of layers)
!
!
!     COMPUTING THE INITIAL MASSES OF MUD AND SAND
!
      DO I=1,NPOIN
!
        DO J=1,NOMBLAY
!
!         FILLING VOIDS BETWEEN SAND GRAINS
!
          IF(NSICLA.EQ.1) THEN
!
!           PURE MUD
!
!            MS_VASE(I,J) = ES(I,J)*CONC_VASE(J)
            MS_VASE(I,J) = ES(I,J)*CONC(I,J)
            AVAIL(I,J,1) = 1.D0
!
          ELSE
!
!           IF MIXED
! V6P2      MS_VASE(I,J) = ES(I,J)*CONC_VASE(J)*AVA0(2) 
            MS_VASE(I,J) = ES(I,J)*CONC(I,J)*AVA0(2)
            MS_SABLE(I,J)= ES(I,J)*XMVS*AVA0(1)
! CV..
            IF(.NOT.DEBU) THEN
            IF(ES(I,J).GE.1.D-6) THEN
              AVAIL(I,J,1)= AVA0(1)
              AVAIL(I,J,2)= AVA0(2)
            ELSE
              AVAIL(I,J,1)= 0.D0
              AVAIL(I,J,2)= 0.D0
            ENDIF
!CV..
            ENDIF
!..CV
          ENDIF
        ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
!
1800  FORMAT(1X,'IL Y A PLUS DE ',1I6,' COUCHES DANS LA STRATIFICATION')
1815  FORMAT(1X,'THERE ARE MORE THAN ',1I6,' LAYERS IN STRATIFICATION')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
