!                    *****************
                     SUBROUTINE CONDIS
!                    *****************
!
     &(IVIDE, EPAI  , TREST , CONC      , TEMP   , HDEP   ,
     & ZR   , ZF    , X     , Y         , NPOIN2 , NPOIN3 ,
     & NPF  , NCOUCH, TASSE , ITASS     , RHOS   , XKV, CFDEP, 
     & ESOMT, TOCE  , SEDCO , CONC_LAYER, TOCE_LAYER, ES_LAYER)
!
!***********************************************************************
! TELEMAC3D   V7P0                                   21/08/2010
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
!+   sediments.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CONC           |<--| CONCENTRATION OF MUD BED LAYER
!| CONC_LAYER     |-->| INPUT CONCENTRATION OF MUD BED LAYER
!|                |   | (MULTILAYER MODEL)
!| CONSOL         |-->|
!| EPAI           |<--| THICKNESS OF SOLID FRACTION oF THE BED LAYER
!|                |   | (EPAI=DZ/(1+IVIDE), DZ BED LAYER THICKNESS)
!| ES_LAYER       |-->| INPUT BED LAYER THICKNESS
!| ESOMT          |<--| CUMULATED BED EVOLUTION
!| GIBSON         |-->| GIBSON SETTLING MODEL
!| HDEP           |<--| THICKNESS OF FRESH DEPOSIT(FLUID MUD LAYER)
!| IVIDE          |<--| VOID RATIO
!|                |   | (GIBSON MODEL ONLY)
!| NCOUCH         |-->| NUMBER OF LAYERS WITHIN THE BED
!|                |   | (GIBSON MODEL)
!| NPF            |<--| NUMBER OF POINTS WITHIN THE BED ALONG THE VERTICAL
!| NPFMAX         |-->| MAXIMUM NUMBER OF HORIZONTAL PLANES WITHIN THE BED
!|                |   | (GIBSON MODEL)
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF POINTS IN 3D
!| PRIVE          |-->| BLOCK OF PRIVATE ARRAYS FOR USER
!| SEDCO          |-->| COHESIVE SEDIMENT (LOGICAL)
!| TASSE          |-->| MULTILAYER SETTLING MODEL
!| TEMP           |<--| TIME COUNTER FOR CONSOLIDATION MODEL
!|                |   | (MULTILAYER MODEL)
!| TOCE           |<--| BED SHEAR STRESS OF MUD BED LAYER
!| TOCE_LAYER     |-->| INPUT BED SHEAR STRESS
!| TREST          |<->| CONSOLIDATION TIME SCALE
!|                |   | (ONLY FOR MULTILAYER MODEL)
!| X,Y            |-->| COORDINATES OF 2D MESH
!| ZF             |-->| BOTTOM ELEVATION
!| ZR             |<--| ELEVATION OF RIDIG BED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC3D, ONLY : H,Z,NPLAN
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN2,NPOIN3,NCOUCH
      DOUBLE PRECISION, INTENT(OUT)   :: IVIDE(NPOIN2,NCOUCH+1)
!
      DOUBLE PRECISION, INTENT(INOUT) :: EPAI(NPOIN2,NCOUCH)
      DOUBLE PRECISION, INTENT(INOUT) :: CONC(NPOIN2,NCOUCH),CFDEP
!      
      DOUBLE PRECISION, INTENT(OUT)   :: TEMP(NCOUCH,NPOIN2)
!      
      DOUBLE PRECISION, INTENT(OUT)   :: HDEP(NPOIN2)
      DOUBLE PRECISION, INTENT(OUT)   :: ZR(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TREST(NCOUCH)
      DOUBLE PRECISION, INTENT(INOUT) :: TOCE(NPOIN2,NCOUCH)
      DOUBLE PRECISION, INTENT(IN)    :: CONC_LAYER(NCOUCH)
      DOUBLE PRECISION, INTENT(IN)    :: ES_LAYER(NCOUCH)
      DOUBLE PRECISION, INTENT(IN)    :: TOCE_LAYER(NCOUCH)
      INTEGER, INTENT(OUT)            :: NPF(NPOIN2)
      TYPE(BIEF_OBJ), INTENT (INOUT)  :: ESOMT      
      LOGICAL, INTENT(IN)             :: TASSE
      LOGICAL, INTENT(IN)             :: SEDCO
      INTEGER, INTENT(IN)             :: ITASS
      DOUBLE PRECISION, INTENT(IN)    :: RHOS,XKV
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION ECOUCH,TCAR,MB
      INTEGER IPOIN,IC,IPF,CHOIX,NLISS
      DOUBLE PRECISION, POINTER :: ZS(:)
!
!     A POINTER TO THE FREE SURFACE IN Z.
!
      ZS => Z(1+(NPLAN-1)*NPOIN2:NPLAN*NPOIN2) 
!
!-----------------------------------------------------------------------
!
!     INITIALISES BED EVOLUTION ESOMT -----
!
      CALL OS('X=0     ',X=ESOMT)
!
!     --------------------------------------------------------
!     INITIAL CONDITIONS FOR THE MULTILAYER COHESIVE BED MODEL
!     --------------------------------------------------------
!
      IF(SEDCO) THEN
!
!       COHESIVE SEDIMENT
!
        DO IPOIN=1,NPOIN2
          DO IC=1, NCOUCH
            CONC(IPOIN,IC)= CONC_LAYER(IC)
            TOCE(IPOIN,IC)= TOCE_LAYER(IC)
          ENDDO
        ENDDO  
!       Bed layer thickness HDEP
        DO IPOIN=1,NPOIN2
          HDEP(IPOIN) = 0.D0 
          DO IC=1, NCOUCH
            EPAI(IPOIN,IC) = ES_LAYER(IC)
            HDEP(IPOIN) = HDEP(IPOIN) + EPAI(IPOIN,IC)  
          ENDDO        
        ENDDO
!
!       INITIALISES ZR TO ZF-HDEP
        CALL OV('X=Y-Z   ' ,ZR,ZF,HDEP,0.D0,NPOIN2)
!   
      ELSE
!
!       NON ERODABLE BED: CALLING NOEROD OF SISYPHE
!
!       ZS, CHOIX AND NLISS ARE NOT USED IN DEFAULT NOEROD
!       NOEROD IS IN LIBRARY SISYPHE
        CALL NOEROD(H%R,ZF,ZR,ZS,X,Y,NPOIN2,CHOIX,NLISS)
!
!       INITIALISES HDEP=ZF-ZR (NOT USED BY THE MULTILAYER MODEL)
!
        CALL OV('X=Y-Z   ',HDEP,ZF,ZR,0.D0,NPOIN2)
!
!       NON COHESIVE SEDIMENT (DIFFERENCE BETWEEN HDEP AND EPAI ?????)
!                              WHY KEEPING BOTH ??
!
!       ONLY ONE LAYER 
        CFDEP= (1.D0-XKV)*RHOS
        DO IPOIN=1,NPOIN2
          CONC(IPOIN,1)= CFDEP
          EPAI(IPOIN,1)=HDEP(IPOIN)
        ENDDO
!  
      ENDIF  
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
