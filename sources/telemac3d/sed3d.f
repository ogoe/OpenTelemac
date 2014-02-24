!                    ****************
                     SUBROUTINE SED3D
!                    ****************
!
     &(MASBED, MASBED0, MASDEP,WC,TA,EPAI,HDEP,CONC,FLUER,FLUDP,TRA02,
     & NELEM2,NPOIN2,NPOIN3,NPFMAX,NCOUCH,
     & NPF,AT,TASSE,GIBSON,RHOS,VOLU2D)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE RELATIVE MASS BALANCE FOR THE
!+                SEDIMENT DURING A TIMESTEP.
!
!history  C.LE NORMANT(LNH)
!+        26/08/92
!+        V5P1
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
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
!history  J-M HERVOUET (LNHE)
!+        18/03/2011
!+        V6P1
!+   Call to massed replaces the old (and duplicated) formula.
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME
!| CFDEP          |-->| CONCENTRATION OF MUD DEPOSIT (G/L)
!| CONC           |-->| CONCENTRATION OF MUD BED LAYER
!|                |   | (MULTILAYER MODEL)
!| DT             |-->| TIME STEP
!| EPAI           |-->| THICKNESS OF SOLID BED LAYER
!|                |   | (EPAI=DZ/(1+IVIDE), DZ TOTAL BED THICKNESS)
!| FLUER          |-->| EROSION FLUX AT EACH 2D POINT
!| GIBSON         |-->| LOGICAL FOR GIBSON SETTLING MODEL
!| HDEP           |---| THICKNESS OF FRESH DEPOSIT (FLUID MUD LAYER)
!| IKLE2          |-->| GLOBAL NUMBERS OF POINTS IN 2D ELEMENTS
!| IVIDE          |-->| VOID INDEX OF MESH POINTS
!| LT             |-->| CURRENT TIME STEP NUMBER
!| MASSE1         |-->| MASS OF SUSPENDED SEDIMENT
!| NCOUCH         |-->| NUMBER OF LAYERS WITHIN THE BED
!|                |   | (MULTILAYER GIBSON SETTLING MODEL)
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D
!| NPF            |-->| NUMBER OF POINTS OF THE BOTTOM ON ONE VERTICAL
!| NPFMAX         |-->| MAXIMUM NUMBER OF HORIZONTAL PLANES THAT
!|                |   | DISCRETISE MUD BED
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| NTRAC          |-->| NUMBER OF ACTIVE TRACERS
!| PDEPOT         |-->| PROBABILITY OF DEPOSITION FOR EACH 2D POINT
!| RHOS           |-->| SEDIMENT DENSITY
!| SURFAC         |-->| AREA OF TRIANGLES
!| TA             |-->| TRACERS
!| TASSE          |-->| MULTILAYER SETTLING MODEL LOGICAL
!| TRA02          |<->| WORK ARRAY (RESULT)
!| U              |-->| VELOCITY COMPONENT AT CURRENT TIME STEP N
!| V              |-->| VELOCITY COMPONENT AT CURRENT TIME STEP N
!| VOLU2D         |-->| INTEGRAL OF TEST FUNCTIONS IN 2D
!| W              |-->| VELOCITY COMPONENT AT CURRENT TIME STEP N
!| WC             |-->| SETTLING VELOCITY
!| X              |-->| MESH COORDONINATES
!| Y              |-->| MESH COORDONINATES
!| Z              |-->| MESH COORDONINATES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPFMAX,NCOUCH,NELEM2,NPOIN2,NPOIN3
      INTEGER, INTENT(IN) :: NPF(NPOIN2)
!
      DOUBLE PRECISION, INTENT(INOUT) :: MASBED,MASDEP
      DOUBLE PRECISION, INTENT(IN) :: WC(NPOIN3)
      DOUBLE PRECISION, INTENT(IN) :: TA(NPOIN3),VOLU2D(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: EPAI(NCOUCH,NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: HDEP(NPOIN2),FLUER(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: FLUDP(NPOIN2),CONC(NCOUCH)
!
      DOUBLE PRECISION, INTENT(INOUT) :: TRA02(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN)    :: AT,RHOS
      DOUBLE PRECISION, INTENT(IN)    :: MASBED0
!
      LOGICAL, INTENT(IN)             :: TASSE , GIBSON
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION ERROR
!
      INTEGER I
!
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
!
!=======================================================================
!
! COMPUTES THE MASS OF ERODED SEDIMENTS (MASSE3)
! DURING THE TIMESTEP
!
!=======================================================================
!
!      FLUX=0.D0
!
!      DO I=1,NPOIN2
!        FLUX=FLUX+FLUER(I)*VOLU2D(I)
!      ENDDO
!
!      MASSE3=FLUX*DT
!      IF(NCSIZE.GT.1) MASSE3=P_DSUM(MASSE3)
!
!=======================================================================
!
! COMPUTES THE MASS OF DEPOSITED SEDIMENTS (MASSE4) DURING THE TIMESTEP
!
!=======================================================================
!
!      FLUX=0.D0
! FLUDP calculated in FONVAS (or MURD3D_POS) FLUDP>0
!      DO I=1,NPOIN2
!            FLUX=FLUX+FLUDP(I)*VOLU2D(I)
!      ENDDO
!
!      MASSE4=FLUX*DT
!      IF(NCSIZE.GT.1) MASSE4=P_DSUM(MASSE4)
!
!=======================================================================
!
! COMPUTES THE MASS OF SEDIMENT EXCHANGED (MASSE5)
! BETWEEN THE MUDDY BED AND THE FLUID DURING THE TIMESTEP
!
!=======================================================================
!
!      MASSE5=MASSE4-MASSE3
!      
!=======================================================================
!
! CUMULATED MASS OF SEDIMENT DEPOSITED (MASDEP =0 t=0)
!
!=======================================================================
! MASDEP calculated in  fonvas
!      MASDEP= MASDEP + MASSE5
!=======================================================================
!
! COMPUTES THE MASS OF SEDIMENT BED
!
!=======================================================================
!
      CALL MASSED(MASBED,EPAI,CONC,HDEP,TRA02,NPOIN2,NPFMAX,NCOUCH,
     &            NPF,TASSE,GIBSON,RHOS,VOLU2D)
!
!=======================================================================
! PRINTOUT
!=======================================================================
!
         ERROR = MASBED-(MASBED0 + MASDEP)
!
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'BILAN MASSE DE SEDIMENTS (SED3D)  TEMPS : ',AT
        WRITE(LU,*) 'MASSE DU LIT                         : ',MASBED
!
!        IF(MASSE5.GT.0) THEN
!          WRITE(LU,*) 'MASSE DEPOSEE AU COURS DU PAS DE TEMPS:',MASSE5
!        ELSE
!	  WRITE(LU,*) 'MASSE ERODEE AU COURS DU PAS DE TEMPS:',-MASSE5
!        ENDIF
        IF(MASDEP.GT.0) THEN
	   WRITE(LU,*) 'MASSE TOTALE  DEPOSEE               :', MASDEP
	ELSE
           WRITE(LU,*) 'MASSE TOTALE ERODEE                 : ',-MASDEP
        ENDIF 
         WRITE(LU,*) 'BILAN MASSE DU LIT (GAIN>0-  PERTE <0)  :', ERROR
      ENDIF
      
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'SEDIMENT BED MASS BALANCE AT TIME=',AT
        WRITE(LU,*) 'MASSE OF BED                        : ',MASBED
!
!        IF(MASSE5.LT.0) THEN
!          WRITE(LU,*) 'DEPOSITED MASS PER TIME STEP:', MASSE5
!        ELSE
!	  WRITE(LU,*) 'ERODED MASS PER TIME STEP:',MASSE5
!        ENDIF

        IF(MASDEP.GT.0) THEN
	   WRITE(LU,*) 'TOTAL DEPOSITED MASS               :', MASDEP
	ELSE
           WRITE(LU,*) 'TOTAL ERODED MASS                : ',-MASDEP
        ENDIF 
        WRITE(LU,*) 'SEDIMENT BED MASS BALANCE  (GAIN>0 LOSS<0):', ERROR

      ENDIF
!
!=======================================================================
!
      RETURN
      END
