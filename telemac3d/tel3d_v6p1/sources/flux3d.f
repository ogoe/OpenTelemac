!                    *****************
                     SUBROUTINE FLUX3D
!                    *****************
!
     &(FLUINT,FLUEXT,FLUEXTPAR,UCONV,VCONV,TRA01,TRA02,TRA03,
     & W1,NETAGE,NPLAN,NELEM3,IELM3,IELM2H,IELM2V,SVIDE,MESH3,
     & MASK8,MSK,MASKEL,MASKBR,LIMPRO,KDIR,NPTFR,DT,VOLU,VOLUN,
     & MESH2,GRAPRD,SIGMAG,TRAV2,NPOIN2,NPOIN3,DM1,ZCONV,
     & FLBOR,PLUIE,RAIN,FLODEL,FLOPAR,OPT_HNEG,FLULIM,YACVVF,LT,BYPASS,
     & N_ADV,MTRA1)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES RELATIVE WATER AND TRACER MASS BALANCES
!+                DURING A TIMESTEP, AS WELL AS ABSOLUTE CUMULATIVE
!+                BALANCES.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/1999
!+
!+   FORTRAN95 VERSION
!
!history  J-M HERVOUET(LNHE)
!+        26/04/2010
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BYPASS         |---| IF YES, BYPASS VOID VOLUMES
!| DM1            |-->| THE PIECE-WISE CONSTANT PART OF ADVECTION FIELD
!|                |   | IS DM1*GRAD(ZCONV), SEE SOLSYS.
!| DT             |-->| TIME STEP
!| FLBOR          |<->| FLUXES AT BOUNDARIES
!| FLODEL         |<->| FLUXES BY SEGMENT
!| FLOPAR         |<->| FLUXES BY SEGMENT, ASSEMBLED IN PARALLEL
!| FLUEXT         |<--| OUTPUT FLUX BY NODE
!| FLUEXTPAR      |<--| OUTPUT FLUX BY NODE, IN PARALLEL
!| FLUINT         |<--| INPUT FLUX BY NODE
!| FLULIM         |<->| LIMITATION OF FLUXES
!| GRAPRD         |-->| GRAPHIC PRINTOUT PERIOD
!| IELM2H         |-->| TYPE OF ELEMENT
!| IELM2V         |-->| TYPE DE DISCRETISATION 2DV
!| IELM3          |-->| TYPE DE DISCRETISATION 3D
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| LIMPRO         |---| PROPAGATION BC TYPES (TELEMAC2D'S PROPAG)
!| LT             |-->| CURRENT TIME STEP NUMBER
!| MASK8          |<->| 2D BOUNDARY MASK
!| MASKBR         |<->| 3D MASK ON LATERAL BOUNDARIES
!| MASKEL         |<->| MASKING OF 3D ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH2          |<->| 2D MESH
!| MESH3          |<->| 3D MESH
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| MTRA1          |<->| 3D WORK MATRIX
!| NELEM3         |-->| NUMBER OF ELEMENTS IN 3D
!| NETAGE         |-->| NUMBER OF LAYERS I.E. NUMBER OF PLANES - 1
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| OPT_HNEG       |---| OPTION FOR NEGATIVE DEPTHS
!| PLUIE          |-->| RAIN IN M/S MULTIPLIED BY VOLU2D
!| RAIN           |-->| IF YES, THERE IS RAIN OR EVAPORATION
!| SIGMAG         |-->| LOGICAL FOR GENERALISED SIGMA TRANSFORMATION
!| SVIDE          |<->| VOID STRUCTURE
!| TRA01          |<->| WORK ARRAY
!| TRA02          |<->| WORK ARRAY
!| TRA03          |<->| WORK ARRAY
!| TRAV2          |<->| WORK ARRAY
!| VCONV          |-->| ADVECTION VELOCITY FIELD
!| VOLU           |-->| VOLUME AROUND POINTS AT TIME N+1
!| VOLUN          |-->| VOLUME AROUND POINTS AT TIME N
!| W1             |<->| WORK ARRAY IN 3D
!| YACVVF         |-->| THERE IS AN ADVECTION WITH FINITE VOLUMES
!|                |   | (HENCE COMPUTATION OF FLUXES REQUIRED)
!| ZCONV          |-->| THE PIECE-WISE CONSTANT PART OF ADVECTION FIELD
!|                |   | IS DM1*GRAD(ZCONV), SEE SOLSYS.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC, ONLY : ADV_NSC,ADV_PSI,ADV_NSC_TF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NETAGE,NPLAN,NELEM3,NPOIN2,NPOIN3,LT
      INTEGER, INTENT(IN) :: IELM3,IELM2H,IELM2V,OPT_HNEG
      INTEGER, INTENT(IN) :: KDIR,NPTFR,GRAPRD
      INTEGER, INTENT(IN) :: LIMPRO(NPTFR,6),N_ADV(0:15)
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: FLUINT,FLUEXT,FLUEXTPAR
      TYPE(BIEF_OBJ), INTENT(IN)    :: VOLU,VOLUN,DM1,ZCONV
      TYPE(BIEF_OBJ), INTENT(IN)    :: UCONV,VCONV,PLUIE
      TYPE(BIEF_OBJ), INTENT(INOUT) :: MASKEL,MASKBR,MTRA1
      TYPE(BIEF_OBJ), INTENT(INOUT) :: FLOPAR,FLULIM
      TYPE(BIEF_OBJ), INTENT(INOUT), TARGET :: FLODEL
      TYPE(BIEF_OBJ), INTENT(INOUT) :: MASK8,FLBOR
      TYPE(BIEF_OBJ), INTENT(INOUT) :: SVIDE,TRA01,TRA02,TRA03,W1,TRAV2
      TYPE(BIEF_MESH), INTENT(INOUT):: MESH3, MESH2
!
      DOUBLE PRECISION, INTENT(IN)    :: DT
      LOGICAL, INTENT(IN)             :: MSK,SIGMAG,RAIN,YACVVF,BYPASS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPLAN,IPTFR,I,ISEG2D,ISEG3D,IOPT
      CHARACTER(LEN=16) FORMUL
      DOUBLE PRECISION SUM_FLUEXT,AUX
!
!***********************************************************************
!
      CALL OS('X=0     ',X=FLUEXT)
!
!=======================================================================
!
!   INTERNAL ADVECTION FLUXES
!
!=======================================================================
!
!        /            D(PSII*)           D(PSII*)
!       /     H * U * -------- + H * V * --------   D(OMEGA*)
!      /OMEGA*           DX                 DY
!
!
      FORMUL = 'VGRADP 2     HOR'
      CALL VECTOR(FLUINT,'=',FORMUL,IELM3,1.D0,DM1,ZCONV,SVIDE,
     &            UCONV,VCONV,SVIDE,MESH3,MSK,MASKEL)
!
!     STORING NON-ASSEMBLED FLUINT IN MATRIX MTRA1
!     WHICH IS NOT TO BE USED BEFORE BUILDING MATRICES MMURD OR
!     MURD_TF (SEE PRECON AND MT14PP)
!
      IF(N_ADV(ADV_NSC).GT.0.OR.N_ADV(ADV_PSI   ).GT.0
     &                      .OR.N_ADV(ADV_NSC_TF).GT.0) THEN
        DO I=1,6*MESH3%NELEM
          MTRA1%X%R(I)=MESH3%W%R(I)
        ENDDO
      ENDIF
!
!
!
! COMPUTING POINT TO POINT FLUXES (FOR TREATMENT OF TIDAL FLATS
!                                  OR FOR ADVECTION WITH FINITE VOLUMES)
! HERE THE CONVENTION FOR SEGMENTS, DUE TO THE CHOICE OF FLUINT, IS
! THAT A SEGMENT WITH POSITIVE FLUX IS MEANT WITH A FLOW FROM POINT 2
! TO POINT 1.
!
      IF(OPT_HNEG.EQ.2.OR.YACVVF) THEN
        IOPT=2
        CALL FLUX_EF_VF_3D(FLODEL%R,MESH2%W%R,MESH3%W%R,
     &                     MESH2%NSEG,MESH3%NSEG,MESH2%NELEM,
     &                     MESH3%NELEM,MESH2,MESH3,.TRUE.,IOPT,1)
      ENDIF
!
! LIMITING FLUXES ACCORDING TO WHAT IS DONE IN 2D CONTINUITY EQUATION
!
      IF(OPT_HNEG.EQ.2) THEN
!       LIMITATION OF 3D FLUXES WITH 2D LIMITATIONS
        CALL FLUX3DLIM(FLODEL%R,FLULIM%R,NPLAN,MESH2%NSEG)
!       NEW ASSEMBLY OF FLUINT (IN THIS CASE ASSEMBLING FLUINT
!                               IN VECTOR ABOVE IS USELESS)
        CALL ASSEG_3D(FLODEL%R,FLUINT%R,NPOIN3,NPLAN,MESH2%NSEG,
     &                MESH3%GLOSEG%I,MESH3%GLOSEG%DIM1,.TRUE.)
!
      ENDIF
!
!=======================================================================
!
!   COMPUTES THE ADVECTIVE FLUXES ON THE LATERAL LIQUID BOUNDARIES
!
!=======================================================================
!
!     /        ->  ->
!    /     H * U . N  PSII*  D(OMEGA*)
!   /
!  /LIQUID BOUNDARIES*
!
      FORMUL = 'FLUBOR          '
!
!     SETTING A MASK ON LIQUID LATERAL BOUNDARIES
!
      CALL EXTMSK(MASKBR,MASK8%R,NPTFR,NETAGE)
!
      CALL VECTOR
     & (TRA02, '=', FORMUL, IELM2V, 1.D0, SVIDE, SVIDE, SVIDE,
     &  UCONV, VCONV, SVIDE, MESH3, .TRUE., MASKBR)
!
      CALL OSDB( 'X=Y     ' , FLUEXT , TRA02 , TRA02 , 0.D0 , MESH3 )
!
!-----------------------------------------------------------------------
!
!     COMPUTATION OF FLUEXT ON POINTS WITH PRESCRIBED DEPTH
!     SO THAT CONTINUITY IS ENSURED.
!
!     EXCEPT AT THE FIRST CALL (BY THE FIRST CALL TO PRECON) FLBOR
!     HAS ALREADY BEEN COMPUTED (WAVE_EQUATION AND POSSIBLY
!     POSITIVE_DEPTHS). IT SHOULD GIVE HERE THE SAME VALUE
!
      IF(NPTFR.GT.0) THEN
        DO IPTFR = 1,NPTFR
          IF(LIMPRO(IPTFR,1).EQ.KDIR) THEN
            FLBOR%R(IPTFR)=0.D0
            DO IPLAN = 1,NPLAN
              I=(IPLAN-1)*NPOIN2+MESH2%NBOR%I(IPTFR)
!             FLUEXT COMPUTED TO SOLVE CONTINUITY IN 3D
!             WITH ASSUMPTION THAT W* IS ZERO.
              FLUEXT%R(I)=FLUINT%R(I)+(VOLUN%R(I)-VOLU%R(I))/DT
              FLBOR%R(IPTFR)=FLBOR%R(IPTFR)+FLUEXT%R(I)
            ENDDO
!
!           CHECKING THAT SUM OF FLUEXT IS STILL EQUAL TO FLBOR
!           IN THIS CASE DO NOT COMPUTE FLBOR ABOVE
!
!           SUM_FLUEXT=0.D0
!           DO IPLAN = 1,NPLAN
!             I=(IPLAN-1)*NPOIN2+MESH2%NBOR%I(IPTFR)
!             SUM_FLUEXT=SUM_FLUEXT+FLUEXT%R(I)
!           ENDDO
!           IF(ABS(SUM_FLUEXT-FLBOR%R(IPTFR)).GT.1.D-10) THEN
!             PRINT*,'PROBLEM AT POINT ',IPTFR
!             PRINT*,'FLBOR= ',FLBOR%R(IPTFR),' SUM_FLUEXT=',SUM_FLUEXT
!             POSSIBLE CORRECTION
!             DO IPLAN = 1,NPLAN
!               I=(IPLAN-1)*NPOIN2+MESH2%NBOR%I(IPTFR)
!               FLUEXT%R(I)=FLUEXT%R(I)*(FLBOR%R(IPTFR)/SUM_FLUEXT)
!             ENDDO
!             STOP
!           ENDIF
          ENDIF
        ENDDO
!
!       SPECIFIC TREATMENT OF POINTS THAT REMAIN WITHOUT VOLUME
!       THIS DOES NOT CHANGE FLBOR
!
        IF((OPT_HNEG.EQ.2.OR.SIGMAG).AND.BYPASS) THEN
          DO IPTFR = 1,NPTFR
            I=MESH2%NBOR%I(IPTFR)
            DO IPLAN = 1,NPLAN-1
              IF(VOLUN%R(I).LT.1.D-14.AND.VOLU%R(I).LT.1.D-14) THEN
!               FLUEXT GIVEN TO UPPER LAYER
                FLUEXT%R(I+NPOIN2)=FLUEXT%R(I+NPOIN2)+FLUEXT%R(I)
                FLUEXT%R(I)=0.D0
              ENDIF
              I=I+NPOIN2
            ENDDO
          ENDDO
        ENDIF
!
      ENDIF
!
!  ASSEMBLED VERSION OF FLUEXT
!
      IF(NCSIZE.GT.1) THEN
        CALL OS('X=Y     ',X=FLUEXTPAR,Y=FLUEXT)
        CALL PARCOM(FLUEXTPAR,2,MESH3)
!     ELSE
!       FLUEXTPAR%R=>FLUEXT%R   ! DONE ONCE FOR ALL IN POINT_TELEMAC3D
      ENDIF
!
!=======================================================================
!
!   COMPUTES THE ADVECTIVE FLUXES THROUGH THE BOTTOM AND FREE SURFACE
!
!=======================================================================
!
!  DIRICHLET TERMS AT THE BOTTOM AND FREE SURFACE
!
!     /
!    /     H * W*  PSII*  D(OMEGA*)
!   /FREE SURFACE AND BOTTOM (IN THE TRANSFPORMED MESH)
!
!   BOTTOM :
!
!     CALL VECTOR
!    &(TRAV2, '=', 'FLUBOR          ', IELM2H, -1.D0, SVIDE, SVIDE,
!    & SVIDE, SVIDE, SVIDE, WSBORF, MESH2,MSK,MASKEL)
!
!     CALL OV ( 'X=X+Y   ' ,FLUEXT%R(1:NPOIN2),
!    &                      TRAV2%R, TRAV2%R, 0.D0, NPOIN2)
!
!   SURFACE :
!
!     CALL VECTOR
!    &(TRAV2, '=', 'FLUBOR          ', IELM2H, 1.D0, SVIDE, SVIDE,
!    & SVIDE, SVIDE, SVIDE, WSBORS, MESH2,MSK,MASKEL)
!
!     CALL OV ( 'X=X+Y   ' ,FLUEXT%R((NPOIN3-NPOIN2+1):NPOIN3),
!    &                      TRAV2%R, TRAV2%R, 0.D0, NPOIN2)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
