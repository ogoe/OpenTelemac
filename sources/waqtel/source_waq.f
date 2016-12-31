!                    **********************
                     SUBROUTINE SOURCE_WAQ
!                    **********************
!
     &(NPOIN3,NPOIN2,TEXP,TIMP,TN,NTRAC,WAQPROCESS,RAYEFF,IND_T,IND_S,H,
     & HPROP,U,V,CF,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T2_1,T2_2,
     & T2_3,PATMOS,LISTIN,GRAV,ZF,DEBUG,MASSOU,DT,DIMM,VOLU2D,NPLAN,
     & LATIT,LONGIT,AT,MARDAT,MARTIM,ZPROP)
!
!***********************************************************************
! TELEMAC2D   V7P0                                   21/09/2014
!***********************************************************************
!
!brief    GIVES CONTRIBUTION OF WAQ PROCESSES TO SOURCE TERMS
!+                FOR THE TRACER.
!
!history  R. ATA
!+        21/09/2014
!+        V7P0
!+   CREATION
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME IN SECONDS
!| DT             |-->| TIME STEP
!| DIMM           |-->| 2D OR 3D
!| HPROP          |-->| PROPAGATION DEPTH (2D)
!| LONGIT         |-->| LONGITUTE OF ORIGIN POINT
!| LATIT          |-->| LATITUDE OF ORIGIN POINT
!| MASSOU         |<--| MASS OF TRACER ADDED BY SOURCE TERM
!| MAXSCE         |-->| MAXIMUM NUMBER OF SOURCES
!| MAXTRA         |-->| MAXIMUM NUMBER OF TRACERS
!| NTRAC          |-->| NUMBER OF TRACERS
!| NPLAN          |-->| NUMBER OF VERTICAL PLANES
!| T1,..,T12      |<->| WORKING STRUCTURES
!| T2_1,T2_2      |<->| 2D WORKING STRUCTURES 
!| TETAT          |-->| COEFFICIENT OF IMPLICITATION FOR TRACERS.
!| TEXP           |-->| EXPLICIT SOURCE TERM.
!| TIMP           |-->| IMPLICIT SOURCE TERM.
!| TN             |-->| TRACERS AT TIME N
!| TSCE           |-->| PRESCRIBED VALUES OF TRACERS AT POINT SOURCES
!| TSCEXP         |<--| EXPLICIT SOURCE TERM OF POINT SOURCES
!|                |   | IN TRACER EQUATION, EQUAL TO:
!|                |   | TSCE - ( 1 - TETAT ) TN
!| VOLU2D         |-->| BASES AREA (NON ASSEMBLED)
!| WAQPROCESS     |-->| WATER QUALITY PROCESS
!| YASMI          |<--| IF YES, THERE ARE IMPLICIT SOURCE TERMS
!| ZPROP          |-->| Z COORDINATES FOR 3D NODES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_WAQTEL,ONLY : DEMBEN,PHOTO,RESP,FORMK2,
     &                               O2SATU,K1,K22,K44,WATTEMP
      USE INTERFACE_WAQTEL, EX_SOURCE_WAQ => SOURCE_WAQ
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER       , INTENT(IN)    :: NPOIN3,WAQPROCESS,DEBUG,DIMM
      INTEGER       , INTENT(IN)    :: NTRAC,NPLAN,NPOIN2
      INTEGER       , INTENT(INOUT) :: IND_T,IND_S
      INTEGER       , INTENT(IN)    :: MARDAT(3),MARTIM(3)
      LOGICAL       , INTENT(IN)    :: LISTIN
      DOUBLE PRECISION,INTENT(IN  ) :: GRAV,DT,LATIT,LONGIT,AT
      DOUBLE PRECISION,INTENT(INOUT):: MASSOU(*)
      TYPE(BIEF_OBJ), INTENT(IN)    :: TN,H,HPROP,CF,U,V
      TYPE(BIEF_OBJ), INTENT(IN)    :: PATMOS,ZF,VOLU2D,ZPROP
      TYPE(BIEF_OBJ), INTENT(INOUT) :: TEXP,TIMP,RAYEFF
      TYPE(BIEF_OBJ), INTENT(INOUT) :: T1,T2,T3,T4,T5,T6
      TYPE(BIEF_OBJ), INTENT(INOUT) :: T7,T8,T9,T10,T11,T12
      TYPE(BIEF_OBJ), INTENT(INOUT) :: T2_1,T2_2,T2_3
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      SELECT CASE(WAQPROCESS)
!
!       O2 MODULE
!
        CASE(1)
          IF(DIMM.EQ.2)THEN
            IF(DEBUG.GT.0) WRITE(LU,*) 'CALL OF CALCS2D_O2'
            CALL CALCS2D_O2
     &           (NPOIN2,WATTEMP,O2SATU,DEMBEN,FORMK2,K1,K44,K22,PHOTO,
     &            RESP,TN,TEXP,TIMP,T1,T2,T3,T4,NTRAC,H,HPROP,U,V,
     &            MASSOU,DT,VOLU2D,IND_T.NE.0,IND_T,DEBUG)
            IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CALCS2D_O2'
          ELSEIF(DIMM.EQ.3)THEN
            IF(DEBUG.GT.0) WRITE(LU,*) 'CALL OF CALCS3D_O2'
            CALL CALCS3D_O2
     &           (NPOIN3,NPOIN2,NPLAN,WATTEMP,O2SATU,DEMBEN,
     &            FORMK2,K1,K44,K22,PHOTO,RESP,TN,TEXP,TIMP,
     &            T1,T2,T2_1,T2_2,NTRAC,H,HPROP,ZPROP,U,V,
     &            IND_T.NE.0,IND_T)
            IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROMCALCS3D_O2'
          ENDIF
!
!       BIOMASS MODULE
!
        CASE(2)
          IF(DIMM.EQ.2)THEN
            IF(DEBUG.GT.0) WRITE(LU,*) 'CALL OF CALCS2D_BIOMASS'
            CALL CALCS2D_BIOMASS(NPOIN2,WATTEMP,TN,TEXP,RAYEFF,NTRAC,H,
     &                           HPROP,T1,T2,T3,T4,T5,T6,DEBUG,MASSOU,
     &                           DT,VOLU2D,IND_T.NE.0,IND_T)
            IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CALCS2D_BIOMASS'
          ELSEIF(DIMM.EQ.3)THEN
            IF(DEBUG.GT.0) WRITE(LU,*) 'CALL OF CALCS3D_BIOMASS'
            CALL CALCS3D_BIOMASS(NPOIN3,NPOIN2,NPLAN,WATTEMP,TN,TEXP,
     &                           RAYEFF,NTRAC,ZPROP,T1,T2,T3,T4,
     &                           T5,T6,DEBUG,IND_T.NE.0,IND_T)
            IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CALCS3D_BIOMASS'
          ENDIF
!
!       EUTRO MODULE
!
        CASE(3)
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALL OF CALCSXD_EUTRO'
          IF(DIMM.EQ.2)THEN
            CALL CALCS2D_EUTRO(NPOIN2,WATTEMP,TN,TEXP,TIMP,RAYEFF,NTRAC,
     &                         H,HPROP,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,
     &                         T11,T12,DEBUG,MASSOU,DT,VOLU2D,
     &                         IND_T.NE.0,IND_T,U,V)
          ELSEIF(DIMM.EQ.3)THEN
            CALL CALCS3D_EUTRO(NPOIN3,NPOIN2,NPLAN,WATTEMP,TN,TEXP,
     &                         TIMP,RAYEFF,NTRAC,HPROP,ZPROP,T1,T2_1,
     &                         T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,DEBUG,
     &                         IND_T.NE.0,IND_T,U,V)
          ENDIF
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CALCSXD_EUTRO'
!
!       MICROPOL MODULE
!
        CASE(4)
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALL OF CALCSXD_MICROPOL'
          IF(DIMM.EQ.2)THEN
            CALL CALCS2D_MICROPOL(NPOIN2,NTRAC,TN,TEXP,TIMP,H,HPROP,
     &                            CF,U,V,T1,T2,T3,T4,DT,VOLU2D,MASSOU)
          ELSEIF(DIMM.EQ.3)THEN
            CALL CALCS3D_MICROPOL(NPOIN3,NPOIN2,NPLAN,NTRAC,TN,TEXP,
     &                            TIMP,ZPROP,CF,U,V,T2_1,T2_2,T2_3,T1,
     &                            DEBUG)
          ENDIF
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CALCSXD_MICROPOL'
!
!       THERMIC MODULE
!
        CASE(5)
          IF(DIMM.EQ.2)THEN
            IF(DEBUG.GT.0) WRITE(LU,*) 'CALL OF CALCS2D_THERMIC'
            CALL CALCS2D_THERMIC(NPOIN2,TN,TEXP,HPROP,PATMOS,IND_T,
     &                           LISTIN)
            IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CALCS2D_THERMIC'
          ELSEIF(DIMM.EQ.3)THEN
!          FOR SURFACE BOUNDARY CONTITIONS CALCS3D_THERMICS IS CALLED IN BOR3D  
!           CALL CALCS3D_THERMICS
!          SOURCE TERMS (VOLUME)   
            IF(DEBUG.GT.0) WRITE(LU,*) 'CALL OF CALCS3D_THERMICV'
            CALL CALCS3D_THERMICV(NPOIN3,NPOIN2,NPLAN,ZPROP%R,IND_T,
     &                            IND_S,TN,TEXP,TIMP,LONGIT,LATIT,
     &                            LISTIN,AT,MARDAT,MARTIM)
            IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CALCS3D_THERMICV'
          ENDIF
        CASE DEFAULT
          IF(LNG.EQ.1) THEN
            WRITE(LU,10)WAQPROCESS
          ELSE
            WRITE(LU,20)WAQPROCESS
          ENDIF
          CALL PLANTE(1)
          STOP
!
      END SELECT
!
10    FORMAT(1X,'SOURCE_WAQ: MODULE WAQ INCONNU : ',I4)
20    FORMAT(1X,'SOURCE_WAQ: UNKNOWN WAQ MODULE : ',I4)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
