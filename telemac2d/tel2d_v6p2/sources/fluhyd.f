C                       *****************
                        SUBROUTINE FLUHYD
C                       *****************
C
     *(NS,NT,NSEG,NPTFR,NUBO,G,DT,X,Y,AIRS,NU,AIRE,
     * UA,ZF,VNOIN,CE,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,
     * KDDL,HBOR,UBOR,VBOR,FLUENT,FLUSORT,NORDRE,CMI,JMI,
     * DJX,DJY,DX,DY,DTHAUT,CFLWTD,FLBOR,
     * DPX,DPY,IVIS,CVIS,FLUHBTEMP,BETA,DSZ,AIRST,HC,FLUXTEMP,NTRAC)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   03/15/2011
!***********************************************************************
!
!brief    COMPUTES FLUXES AT TIME N.
!
!history  INRIA
!+
!+        V5P8
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
!history  R. ATA (EDF-LNHE)
!+        03/15/2011
!+        V6P1
!+    INTRODUCTION OF FLBOR AND MASS BALANCE
!+    CHANGE CE(3,NS) TO CE(NS,3)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AIRE           |-->| ELEMENT AREA
!| AIRS           |-->| CELL AREA
!| AIRST          |-->| AREA OF SUB-TRIANGLES (SECOND ORDER)
!| BETA           |-->| EXTRAPOLATION COEFFICIENT FOR ORDRE 2
!| CE             |<--| FLUX   +  DIFFUSION TERMS
!| CFLWTD         |-->| CFL NUMBER
!| CMI            |-->| COORDINATES OF MIDDLE POINTS OF INTERFACES
!| CVIS           |-->| COEFFICIENT OF DIFFUSION FOR THE VELOCITIES
!| DJX,DJY        |-->| GRADIENTS PER TRIANGLE
!| DSZ            |-->| VARIATIONS Z (BATHY) FOR ORDRE 2
!| DT             |<->| TIME STEP
!| DTHAUT         |-->| USED FOR CFL CONDITION
!| DX,DY          |---| GRADIENTS PER NODES
!| FLUENT,FLUSORT |<--| INLET AND OUTLET MASS FLUXS BETWEEN TN AND TN+1
!| FLUHBTEMP      |<--| TRACER BORD FLUXES
!| FLUXTEMP       |<--| FLUX FOR TRACER
!| G              |-->| GRAVITY
!| HBOR           |-->| PRESCRIBED VALUES FOR H
!| HC             |<--| RECONSTRUCTED H FOR ORDRE 2 
!| IVIS           |-->| OPTION FOR THE DIFFUSION OF VELOCITIES
!| JMI            |-->| NUMBER OF THE TRIANGLE IN WHICH IS LOCATED
!|                |   | THE MIDDLE POINT OF THE INTERFACE
!| KDDL           |-->| CONVENTION FOR THE FREE POINTS
!| KDIR           |-->| CONVENTION DIRICHLET POINTS
!| KNEU           |-->| CONVENTION NEUMANN POINTS
!| LIMPRO         |-->| TYPES OF BOUNDARY CONDITION
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY NODES
!| NORDRE         |-->| ORDRE OF THE SCHEME
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NS             |-->| TOTAL NUMER OF POINTS IN THE MESH
!| NSEG           |-->| NUMBER OF EDGES IN THE MESH
!| NT             |-->| NUMBER OF ELEMENTS IN THE MESH
!| NTRAC          |---| NUMBER OF TRACER
!| NU             |-->| NUMEROS OF NODES PER TRIANGLE
!| NUBO           |-->| GLOBAL NUMBERS OF THE NODES FORMING THE EDGE
!| UA             |-->| UA(1,IS) = H,  UA(2,IS)=U  ,UA(3,IS)=V
!| UBOR           |-->| PERSCRIBED VALUES FOR U
!| VBOR           |-->| PRESCRIBED VALUES FOR V
!| VNOIN          |-->| NORMAL VECTOR TO THE INTERFACE
!|                |   | (2 FIRST COMPONENTS) AND
!|                |   | LENGTH OF THE SEGMENT (3RD COMPONENT)
!| X,Y            |-->| COORDINATES IF THE NODES
!| XNEBOR,YNEBOR  |-->| NORMAL VECTOR TO BOUNDARY NODES 
!| ZF             |-->| BATHYMETRY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_FLUHYD => FLUHYD
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NS,NT,NSEG,NPTFR,KDIR,KNEU,KDDL,NORDRE
      INTEGER, INTENT(IN) :: NBOR(NPTFR),LIMPRO(NPTFR,6),NU(NT,3)
      INTEGER, INTENT(IN) :: NUBO(2,NSEG),JMI(*),IVIS,NTRAC
      DOUBLE PRECISION, INTENT(IN) :: XNEBOR(2*NPTFR),YNEBOR(2*NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: HBOR(NPTFR),G,CFLWTD,DTHAUT(*)
      DOUBLE PRECISION, INTENT(IN) :: UBOR(NPTFR),VBOR(NPTFR),CMI(2,*)
      DOUBLE PRECISION, INTENT(IN) :: AIRST(2,*),CVIS
      DOUBLE PRECISION, INTENT(IN) :: X(NS),Y(NS),AIRS(NS),AIRE(NT)
      DOUBLE PRECISION, INTENT(INOUT) :: BETA,DT,HC(2,*)
      DOUBLE PRECISION, INTENT(INOUT) :: CE(NS,3),FLUENT,FLUSORT
      DOUBLE PRECISION, INTENT(IN) :: UA(3,NS),ZF(NS),VNOIN(3,NSEG)
      DOUBLE PRECISION, INTENT(IN) :: DSZ(2,*),DPX(3,NT),DPY(3,NT)
      DOUBLE PRECISION, INTENT(INOUT) :: DJX(3,*),DJY(3,*)
      DOUBLE PRECISION, INTENT(INOUT) :: DX(3,*),DY(3,*)
      TYPE(BIEF_OBJ), INTENT(INOUT) :: FLUXTEMP,FLUHBTEMP,FLBOR 
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!  
      INTEGER IS,IVAR
!
!-----------------------------------------------------------------------
!
!     INITIALIZATION OF CE
! 
      DO IS=1,NS
        DO IVAR=1,3
          CE(IS,IVAR) = 0.D0
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
!     COMPUTES GRADIENTS AT NODES AS WELL AS DIFFUSION TERMS
!
      IF(NORDRE.EQ.2.OR.IVIS.EQ.1) THEN
        CALL GRADNOD(NS,NT,NU,AIRE,AIRS,
     &               UA,DPX,DPY,DJX,DJY,DX,DY,IVIS,CVIS,CE,ZF)
      ENDIF
!
      CALL FLUCIN(NS,NSEG,NUBO,G,X,Y,CFLWTD,DT,UA,ZF,VNOIN,CE,NORDRE,
     &            CMI,JMI,DJX,DJY,DX,DY,BETA,DSZ,AIRS,
     &            AIRST,HC,FLUXTEMP,NPTFR,NBOR,XNEBOR,YNEBOR,NTRAC)
!
!     BOUNDARY CONDITIONS TREATMENT
!
      CALL CDL(NS,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,
     &         G,HBOR,UBOR,VBOR,UA,CE,FLUENT,FLUSORT,FLBOR,
     &         DTHAUT,DT,CFLWTD,FLUHBTEMP,NTRAC)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
