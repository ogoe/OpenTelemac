!                    *****************
                     SUBROUTINE CONDIS
!                    *****************
!
     &(IVIDE   , EPAI  , TREST , CONC  , TEMP      , HDEP      ,
     & ZR      , ZF    , X     , Y     , NPOIN2    , NPOIN3    ,
     & NPF     , NCOUCH, TASSE , ITASS , RHOS      , XKV       ,
     & CFDEP   , ESOMT , TOCE  , SEDCO , CONC_LAYER, TOCE_LAYER,
     & ES_LAYER, SEDNCO, MIXTE , EPAICO, EPAINCO   , PVSCO     ,
     & PVSNCO  , PVSNCO0)
!
!***********************************************************************
! TELEMAC3D   V7P1
!***********************************************************************
!
!brief    INITIALISES THE SEDIMENT VARIABLES.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  NOEMIE DURAND (CHC-NRC); C LE NORMANT (LNH)
!+        18/07/06
!+        V5P7
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
!history  C. VILLARET & T. BENSON & D. KELLY (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments in sediment merged on 25/02/2014.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        02/05/2014
!+        V7P0
!+   Call to NOEROD of Sisyphe for non erodable bed in the case of non
!+   cohesive sediments.
!
!history  G. ANTOINE & M. JODEAU & J.M. HERVOUET (EDF - LNHE)
!+        13/10/2014
!+        V7P0
!+   New developments in sediment for mixed sediment transport
!
!history  C. VILLARET (EDF LAB, LNHE)
!+        02/09/2015
!+        V7P1
!+   Noerod from Sisyphe called to get the same non erodable bed.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CONC           |<--| CONCENTRATION OF MUD BED LAYER
!| CONC_LAYER     |-->| INPUT CONCENTRATION OF MUD BED LAYER
!|                |   | (MULTILAYER MODEL)
!| CFDEP          |<->| CONCENTRATION OF SAND LAYER
!| EPAI           |<--| THICKNESS OF SOLID FRACTION oF THE BED LAYER
!|                |   | (EPAI=DZ/(1+IVIDE), DZ BED LAYER THICKNESS)
!| EPAICO         |<->| THICKNESS OF COHESIVE SUB-LAYER
!| EPAINCO        |<->| THICKNESS OF NON-COHESIVE SUB-LAYER
!| ES_LAYER       |-->| INPUT BED LAYER THICKNESS
!| ESOMT          |<--| CUMULATED BED EVOLUTION
!| HDEP           |<--| THICKNESS OF FRESH DEPOSIT(FLUID MUD LAYER)
!| ITASS          |-->| INDEX OF MODEL CHOICE
!| IVIDE          |<--| VOID RATIO
!|                |   | (GIBSON MODEL ONLY)
!| MIXTE          |-->| LOGICAL, MIXED SEDIMENTS OR NOT
!| NCOUCH         |-->| NUMBER OF LAYERS WITHIN THE BED
!|                |   | (GIBSON MODEL)
!| NPF            |<--| NUMBER OF POINTS WITHIN THE BED ALONG THE VERTICAL
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF POINTS IN 3D
!| PVSCO          |<->| VOLUMIC PERCENTAGE OF COHESIVE SEDIMENT
!| PVSNCO         |<->| VOLUMIC PERCENTAGE OF NON-COHESIVE SEDIMENT
!| PVSNCO0        |-->| INITIAL VOLUMIC PERCENTAGE OF NON-COHESIVE SEDIMENT
!| RHOS           |-->| DENSITY OF SEDIMENT
!| SEDCO          |-->| COHESIVE SEDIMENT (LOGICAL)
!| SEDNCO         |-->| LOGICAL, SEDIMENT NON-COHESIVE OR NOT
!| TASSE          |-->| MULTILAYER SETTLING MODEL
!| TEMP           |<--| TIME COUNTER FOR CONSOLIDATION MODEL
!|                |   | (MULTILAYER MODEL)
!| TOCE           |<--| BED SHEAR STRESS OF MUD BED LAYER
!| TOCE_LAYER     |-->| INPUT BED SHEAR STRESS
!| TREST          |<->| CONSOLIDATION TIME SCALE
!|                |   | (ONLY FOR MULTILAYER MODEL)
!| X              |-->| FIRST COORDINATE OF 2D MESH
!| XKV            |-->| NON COHESIVE BED POROSITY
!| Y              |-->| SECOND COORDINATE OF 2D MESH
!| ZF             |-->| BOTTOM ELEVATION
!| ZR             |<--| ELEVATION OF RIDIG BED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC3D, ONLY : H,Z,NPLAN
      USE INTERFACE_TELEMAC3D, EX_CONDIS => CONDIS
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN2,NPOIN3,NCOUCH
      DOUBLE PRECISION, INTENT(OUT)   :: IVIDE(NPOIN2,NCOUCH+1), CFDEP
!
      DOUBLE PRECISION, INTENT(INOUT) :: EPAI(NPOIN2,NCOUCH)
      DOUBLE PRECISION, INTENT(INOUT) :: EPAICO(NPOIN2), EPAINCO(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: PVSCO(NPOIN2), PVSNCO(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: CONC(NPOIN2,NCOUCH)
      DOUBLE PRECISION, INTENT(INOUT) :: TEMP(NCOUCH,NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: HDEP(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: ZR(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TREST(NCOUCH)
      DOUBLE PRECISION, INTENT(INOUT) :: TOCE(NPOIN2,NCOUCH)
      DOUBLE PRECISION, INTENT(IN)    :: CONC_LAYER(NCOUCH)
      DOUBLE PRECISION, INTENT(IN)    :: ES_LAYER(NCOUCH)
      DOUBLE PRECISION, INTENT(IN)    :: TOCE_LAYER(NCOUCH)
      INTEGER, INTENT(INOUT)          :: NPF(NPOIN2)
      TYPE(BIEF_OBJ), INTENT (INOUT)  :: ESOMT
      LOGICAL, INTENT(IN)             :: TASSE
      LOGICAL, INTENT(IN)             :: SEDCO, SEDNCO, MIXTE
      INTEGER, INTENT(IN)             :: ITASS
      DOUBLE PRECISION, INTENT(IN)    :: RHOS,XKV, PVSNCO0
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION ECOUCH,PVSCO0,HAUTSED,ERROR
      INTEGER IPOIN,IC,IPF,CHOIX,NLISS,NLAYER,K,J
      DOUBLE PRECISION, POINTER :: ZS(:)
!
!     A POINTER TO THE FREE SURFACE IN Z
!
      ZS => Z(1+(NPLAN-1)*NPOIN2:NPLAN*NPOIN2)

!     INITIALISES BED EVOLUTION ESOMT -----
!
      CALL OS('X=0     ',X=ESOMT)
!
!     --------------------------------------------------------
!     INITIAL CONDITIONS FOR THE MULTILAYER COHESIVE BED MODEL
!     --------------------------------------------------------
!
!     CALLING NOEROD OF LIBRARY SISYPHE, TO GET ZR
!     NOTE: CHOIX AND NLISS NOT DEFINED, NOT USED
!
      CALL NOEROD(H%R,ZF,ZR,ZS,X,Y,NPOIN2,CHOIX,NLISS)
!
      IF(SEDCO) THEN
!
!       COHESIVE SEDIMENT OR NON COHESIVE
!
        DO IPOIN=1,NPOIN2
          HDEP(IPOIN) = 0.D0
          DO IC=1, NCOUCH
            CONC(IPOIN,IC) = CONC_LAYER(IC)
            TOCE(IPOIN,IC) = TOCE_LAYER(IC)
            EPAI(IPOIN,IC) = ES_LAYER(IC)
            HDEP(IPOIN)    = HDEP(IPOIN) + EPAI(IPOIN,IC)
          ENDDO
        ENDDO
!
      ENDIF
!
      IF(SEDNCO) THEN
!       LAST METHOD TO DEFINE HARD BOTTOM
!       NON ERODABLE BED: CALLING NOEROD OF SISYPHE
!       ZS, CHOIX AND NLISS ARE NOT USED IN DEFAULT NOEROD
!       NOEROD IS IN LIBRARY SISYPHE
!
!       ONLY ONE LAYER
        CFDEP = (1.D0-XKV)*RHOS
        DO IPOIN = 1,NPOIN2
          HDEP(IPOIN) = 0.D0
!         CV adding layers for non cohesive
          DO IC=1, NCOUCH
            CONC(IPOIN,IC) = CFDEP
            EPAI(IPOIN,IC) = ES_LAYER(IC)
            HDEP(IPOIN)   = HDEP(IPOIN)+EPAI(IPOIN,IC)
          ENDDO
        ENDDO
      ENDIF
!
      IF(MIXTE) THEN
        CFDEP= (1.D0-XKV)*RHOS
        PVSCO0 = 1.D0-PVSNCO0
        DO IPOIN=1,NPOIN2
          CONC(IPOIN,1)  = CONC_LAYER(1)
          TOCE(IPOIN,1)  = TOCE_LAYER(1)
          PVSCO(IPOIN)   = PVSCO0
          PVSNCO(IPOIN)  = PVSNCO0
          EPAICO(IPOIN)  = PVSCO0 * ES_LAYER(1)
          EPAINCO(IPOIN) = PVSNCO0 * ES_LAYER(1)
          EPAI(IPOIN,1)  = ES_LAYER(1)
          HDEP(IPOIN)    = EPAI(IPOIN,1)
        ENDDO
      ENDIF
!
!     Debut modif CV
!     CORRECTION OF LAYERS (see in INIT_AVAI)
!     THE HEIGHT OF SEDIMENT (SUM OF ES) MUST NOT BE MORE THAN ZF-ZR
!     IF SO, THE HEIGHT OF THE LAST LAYER IS REDUCED
!     IF THERE ARE LAYERS UNDER ZR, THEY ARE NOT TAKEN INTO ACCOUNT
!
      DO J=1,NPOIN2
        HAUTSED = 0.D0
        NLAYER  = NCOUCH
        DO K=1,NCOUCH
          IF(HAUTSED + EPAI(J,K) .GE. ZF(J) - ZR(J)) THEN
            EPAI(J,K) = ZF(J) - ZR(J) -  HAUTSED
            NLAYER = K
            HAUTSED = HAUTSED + EPAI(J,K)
            EXIT
          ENDIF
          HAUTSED = HAUTSED + EPAI(J,K)
        ENDDO
!       OTHER LAYERS SET TO 0.D0
        IF(NLAYER.LT.NCOUCH) THEN
          DO K=NLAYER+1,NCOUCH
            EPAI(J,K) = 0.D0
          ENDDO
        ENDIF
!       THE THICKNESS OF THE LAST LAYER IS ENLARGED SO THAT
!       THE HEIGHT OF SEDIMENT (SUM OF ES) IS EQUAL TO ZF-ZR
        IF(HAUTSED.LT.ZF(J)-ZR(J)) THEN
          EPAI(J,NCOUCH)=EPAI(J,NCOUCH)+ZF(J)-ZR(J)-HAUTSED
        ENDIF
      ENDDO
!
      DO J=1, NPOIN2
        HDEP(J) =0.D0
        DO K = 1, NCOUCH
          HDEP(J) = HDEP(J)+ EPAI(J,K)
        ENDDO
        ERROR= HDEP(J)-(ZF(J)-ZR(J))
        IF(ABS(ERROR).GT.1.D-6) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'CONDIS : ERREUR D''INITIALISATION : ',ITASS
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'CONDIS: INITIALISATION ERROR ',ITASS
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!
! END modif CV
!
!     ------------------------------------------
!     INITIAL CONDITIONS FOR CONSOLIDATION MODEL
!     ------------------------------------------
!
      IF(TASSE) THEN
!
        IF(ITASS.EQ.1) THEN
!
!         SIMPLE MULTI-LAYER MODEL
!
!         CHANGES HOURS INTO SECONDS  -----
          CALL OV( 'X=CX    ',TREST,TREST,TREST,3600.D0,NCOUCH)
!         INITIALISES TEMP
          CALL OV( 'X=C     ',TEMP,TEMP,TEMP,0.D0,NPOIN2*NCOUCH)
!
        ELSEIF(ITASS.EQ.2) THEN
!
!         GIBSON MODEL
!
          DO IPOIN=1,NPOIN2
            NPF(IPOIN) =NCOUCH
            DO IPF= 1, NCOUCH
              ECOUCH=(RHOS-CONC(IPOIN,IPF))/CONC(IPOIN,IPF)
              IF(IPF.EQ.1) THEN
                IVIDE(IPOIN,IPF)=ECOUCH
              ELSE
                IVIDE(IPOIN,IPF)= 2.D0*ECOUCH-IVIDE(IPOIN,IPF-1)
              ENDIF
            ENDDO
            IVIDE(IPOIN,NCOUCH+1)= 2.D0*ECOUCH-IVIDE(IPOIN,NCOUCH)
          ENDDO
!
        ELSE
!
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'CONDIS : MODELE DE TASSEMENT INCONNU : ',ITASS
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'CONDIS: UNKNOWN SETTLING MODEL: ',ITASS
          ENDIF
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

