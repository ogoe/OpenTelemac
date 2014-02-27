!                    *****************
                     SUBROUTINE CONDIS
!                    *****************
!
     &(IVIDE, EPAI  , TREST , CONC , TEMP   , HDEP   ,
     & ZR   , ZF    , X     , Y    , NPOIN2 , NPOIN3 ,
     & NPF  , NCOUCH, TASSE, ITASS  , RHOS, XKV, CFDEP, 
     & ESOMT, TOCE, SEDCO,CONC_LAYER, TOCE_LAYER, ES_LAYER)
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
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      INTEGER, INTENT(IN)             :: NPOIN2,NPOIN3,NCOUCH
      DOUBLE PRECISION, INTENT(OUT)   :: IVIDE(NPOIN2,NCOUCH+1)
!
      DOUBLE PRECISION, INTENT(INOUT)   :: EPAI(NPOIN2,NCOUCH)
      DOUBLE PRECISION, INTENT(INOUT)   :: CONC(NPOIN2,NCOUCH),CFDEP
!      
      DOUBLE PRECISION, INTENT(OUT)   :: TEMP(NCOUCH,NPOIN2)
!      
      DOUBLE PRECISION, INTENT(OUT)   :: HDEP(NPOIN2)
      DOUBLE PRECISION, INTENT(OUT)   :: ZR(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TREST(NCOUCH)
      DOUBLE PRECISION, INTENT(INOUT) ::  TOCE(NPOIN2,NCOUCH)
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
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION ECOUCH , TCAR
      INTEGER IPOIN, IC, IPF
!
      INTRINSIC LOG10,MAX
! CV
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
      DOUBLE PRECISION MB
!   
!#####> NOE-CHANGES
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!      LOGICAL CVGCSL
!      INTEGER ERR 
!     INTEGER NITCSL, ITCSL
!      DOUBLE PRECISION DTCSL,RESCSL
!      DOUBLE PRECISION, ALLOCATABLE :: TRA01(:,:)
!      DOUBLE PRECISION, ALLOCATABLE :: TRA02(:),TRA03(:),ZNOE(:)
!
! ALLOCATES MEMORY AND SETS INITIAL INTERFACE BETWEEN HDEP AND EPAI
!      ALLOCATE(TRA01(NPFMAX,6),STAT=ERR)
!      IF(ERR.NE.0) THEN
!        IF(LNG.EQ.1) WRITE(LU,*) 'CONDIS : MAUVAISE ALLOCATION DE TRA01'
!        IF(LNG.EQ.2) WRITE(LU,*) 'CONDIS: WRONG ALLOCATION OF TRA01'
!        STOP
!      ENDIF
!      ALLOCATE(TRA02(NPFMAX),STAT=ERR)
!      IF(ERR.NE.0) THEN
!        IF(LNG.EQ.1) WRITE(LU,*) 'CONDIS : MAUVAISE ALLOCATION DE TRA02'
!        IF(LNG.EQ.2) WRITE(LU,*) 'CONDIS: WRONG ALLOCATION OF TRA02'
!        STOP
!      ENDIF
!      ALLOCATE(TRA03(NPFMAX),STAT=ERR)
!      IF(ERR.NE.0) THEN
!        IF(LNG.EQ.1) WRITE(LU,*) 'CONDIS : MAUVAISE ALLOCATION DE TRA03'
!        IF(LNG.EQ.2) WRITE(LU,*) 'CONDIS: WRONG ALLOCATION OF TRA03'
!        STOP
!      ENDIF
!      ALLOCATE(ZNOE(NPOIN2),STAT=ERR)
!      IF(ERR.NE.0) THEN
!        IF(LNG.EQ.1) WRITE(LU,*) 'CONDIS : MAUVAISE ALLOCATION DE ZNOE'
!        IF(LNG.EQ.2) WRITE(LU,*) 'CONDIS: WRONG ALLOCATION OF ZNOE'
!        STOP
!      ENDIF
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!#####< NOE-CHANGES
!
!=======================================================================
!
!     -----  INITIALISES HDEP                  -----
!     -----  NOT USED BY THE MULTILAYER MODEL  -----
!
!     HERE, THE DEFAULT ACTION IS TO SET HDEP TO 100., HENCE ENABLE THE
!     EROSION PROCESS. THIS VALUE IS COMPATIBLE WITH SUBROUTINE NOEROD
!     IN SISYPHE.
!
      CALL OV('X=C     ',HDEP,HDEP,HDEP,100.D0,NPOIN2)
!
!  -----  INITIALISES BED EVOLUTION ESOMT -----
!
       CALL OS('X=0     ',X=ESOMT)

!     -------------------------------------------
!     INITIAL CONDITIONS FOR THE MULTILAYER COHESIVE BED MODEL
!     -------------------------------------------
!
      IF(SEDCO) THEN
!
! COHESIVE SEDIMENT
!
        DO IPOIN=1,NPOIN2
          DO IC=1, NCOUCH
            CONC(IPOIN,IC)= CONC_LAYER(IC)
            TOCE(IPOIN,IC)= TOCE_LAYER(IC)
          ENDDO
        ENDDO  
! Bed layer thickness
        DO IPOIN=1,NPOIN2
          HDEP(IPOIN) = 0.D0 
          DO IC=1, NCOUCH
             EPAI(IPOIN,IC) = ES_LAYER(IC)
             HDEP(IPOIN) = HDEP(IPOIN) + EPAI(IPOIN,IC)  
          ENDDO        
        ENDDO   
      ELSE
!-----------------------------------------------
! NON COHESIVE SEDIMENT
! EPAI NOT DEFINED replaced by HDEP
! -----------------------------------------------
! ONLY ONE LAYER 
        CFDEP= (1.D0-XKV)*RHOS
        DO IPOIN=1,NPOIN2
          CONC(IPOIN,1)= CFDEP
          EPAI(IPOIN,1)=HDEP(IPOIN)
        ENDDO  
      ENDIF  
!
!     -----  INITIALISES ZR  -----
!
      CALL OV('X=Y-Z   ' ,ZR,ZF,HDEP,0.D0,NPOIN2)
!
!  -----------------------------------------
!  INITIAL CONDITIONS FOR CONSOLIDATION MODEL
!  ------------------------------------------
      IF(TASSE) THEN
!      
! -------------------------
! SIMPLE MULTI-LAYER MODEL
!-------------------------
        IF (ITASS.EQ.1) THEN
!     
!       -----  CHANGES HOURS INTO SECONDS  -----
!
          CALL OV( 'X=CX    ',TREST,TREST,TREST,3600.D0,NCOUCH)
!
!       -----  INITIALISES TEMP  -----
!
          CALL OV( 'X=C     ',TEMP,TEMP,TEMP,0.D0,NPOIN2*NCOUCH)
!

!   
!      GIBSON MODEL
!     -------------

        ELSEIF (ITASS.EQ.2) THEN
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
        ENDIF
      ENDIF 
!
!#####> NOE-CHANGES
!
!       HERE IS OUR CHANCE TO SET
!       IVIDE AND (TRUE) EPAI REPRESENTATIVE OF CONSOLIDATED MATTER
!
!       CONSOL SHOULD BE .TRUE. IF THE LAYER DISTRIBUTION IS NOT KNOWN
!       AND THE USER WANTS TO LET THE MODEL CONSOLIDATE THE BED
!
! This part is cancelled 
!
!        IF(CONSOL) THEN
!
! TIME FOR CONSOLIDATION, WHICH COULD ALSO BE READ FROM PRIVE%ADR(2)
!          DTCSL = DT*10.D0
!          NITCSL = 100
!
! START PAST TIME LOOP
!
!          DO ITCSL = 1,NITCSL
!          CVGCSL = .FALSE.
!          DO WHILE(.NOT.CVGCSL)
!
!     -----  MANAGES THE DEPOSITED SEDIMENT:   -----
!     -----  ADDS NEW LAYERS TO THE MUDDY BED  -----
!
!            CALL GESTDP( IVIDE,EPAI,HDEP,
!     &        NPOIN2,NPFMAX,NPF, EPAI0,CONC(1),RHOS )
!
!     -----  CONSOLIDATES THE MUDDY BED  -----
!     -----  USING GIBSON EQUATION       -----
!
!            CALL TASSEM( IVIDE,EPAI, NPOIN2,NPFMAX,NPF, GRAV,RHOS,
!     &        DTCSL, CFMAX, TRA01,TRA02,TRA03 )
!
!     -----  UPDATES THE BOTTOM LAYER  -----
!     -----  RECOMPUTES THE INTERFACE  -----
!
!      RESCSL = 0.D0
!      DO IPOIN = 1,NPOIN2
!        ZNOE(IPOIN) = ZR(IPOIN)
!        DO IPF = 1,NPF(IPOIN)-1
!          ECOUCH=(IVIDE(IPF,IPOIN)+IVIDE(IPF+1,IPOIN))/2.D0
!          ZNOE(IPOIN) = ZNOE(IPOIN)+(1.D0+ECOUCH)*EPAI(IPF,IPOIN)
!        ENDDO
!        IF(ZF(IPOIN).LT.ZNOE(IPOIN)) THEN
!        RESCSL = RESCSL + ( MAX(
!     & ZF(IPOIN)-(ZNOE(IPOIN)-(1.D0+ECOUCH)*EPAI(NPF(IPOIN)-1,IPOIN)),
!     &  0.D0) - HDEP(IPOIN) )**2
!      ECOUCH =(IVIDE(NPF(IPOIN)-1,IPOIN)+IVIDE(NPF(IPOIN),IPOIN))/2.D0
!        HDEP(IPOIN) = MAX(
!     &  ZF(IPOIN)-(ZNOE(IPOIN)-(1.D0+ECOUCH)*EPAI(NPF(IPOIN)-1,IPOIN)),
!     &        0.D0)
!            EPAI(NPF(IPOIN)-1,IPOIN) = 0.D0
!            NPF(IPOIN) = NPF(IPOIN)-1
!          ELSE
!            RESCSL = RESCSL + (ZF(IPOIN)-ZNOE(IPOIN)-HDEP(IPOIN))**2
!            HDEP(IPOIN) = ZF(IPOIN) - ZNOE(IPOIN)
!          ENDIF
!        ENDDO
!        CVGCSL = RESCSL.LT.(1.D-8)
!      ENDDO
!
! END PAST TIME LOOP
!
!        ENDIF
!
! END IF CONSOL
!
!      ENDIF
!
! END IF TASSE/GIBSON
!
!      DEALLOCATE(TRA01)
!      DEALLOCATE(TRA02)
!      DEALLOCATE(TRA03)
!      DEALLOCATE(ZNOE)
!
!-----------------------------------------------------------------------      
!
      RETURN
      END
