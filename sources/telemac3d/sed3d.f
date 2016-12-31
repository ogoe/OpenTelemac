!                    ****************
                     SUBROUTINE SED3D
!                    ****************
!
     &(MASBED, MASBED0, MASDEP,WC,TA,EPAI,HDEP,CONC,FLUER,FLUDP,TRA02,
     & NELEM2,NPOIN2,NPOIN3,NPFMAX,NCOUCH,
     & NPF,AT,TASSE,GIBSON,RHOS,VOLU2D,
     & CFDEP,EPAICO,EPAINCO,MIXTE)
!
!***********************************************************************
! TELEMAC3D   V7P0                                   21/08/2010
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
!history  C. VILLARET & T. BENSON (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments for sediment, merged on 25/02/2014.
!
!history  G. ANTOINE & M. JODEAU & J.M. HERVOUET (EDF - LNHE)
!+        13/10/2014
!+        V7P0
!+   New developments in sediment for mixed sediment transport
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME
!| CFDEP          |-->| CONCENTRATION OF MUD DEPOSIT (G/L)
!| CONC           |-->| CONCENTRATION OF MUD BED LAYER
!|                |   | (MULTILAYER MODEL)
!| EPAI           |-->| THICKNESS OF SOLID BED LAYER
!|                |   | (EPAI=DZ/(1+IVIDE), DZ TOTAL BED THICKNESS)
!| EPAICO         |-->| THICKNESS OF COHESIVE SUB-LAYER
!| EPAINCO        |-->| THICKNESS OF NON-COHESIVE SUB-LAYER
!| FLUDP          |-->| DEPOSITION FLUX AT EACH 2D POINT
!| FLUER          |-->| EROSION FLUX AT EACH 2D POINT
!| GIBSON         |-->| LOGICAL FOR GIBSON SETTLING MODEL
!| HDEP           |---| THICKNESS OF FRESH DEPOSIT (FLUID MUD LAYER)
!| MIXTE          |-->| LOGICAL, MIXED SEDIMENTS OR NOT
!| MASBED         |<->| MASS OF BED
!| MASBED0        |-->| INITIAL MASS OF BED
!| MASDEP         |<->| TOTAL DEPOSITED MASS
!| NCOUCH         |-->| NUMBER OF LAYERS WITHIN THE BED
!|                |   | (MULTILAYER GIBSON SETTLING MODEL)
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D
!| NPF            |-->| NUMBER OF POINTS OF THE BOTTOM ON ONE VERTICAL
!| NPFMAX         |-->| MAXIMUM NUMBER OF HORIZONTAL PLANES THAT
!|                |   | DISCRETISE MUD BED
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| RHOS           |-->| SEDIMENT DENSITY
!| TA             |-->| TRACERS
!| TASSE          |-->| MULTILAYER SETTLING MODEL LOGICAL
!| TRA02          |<->| WORK ARRAY (RESULT)
!| VOLU2D         |-->| INTEGRAL OF TEST FUNCTIONS IN 2D
!| WC             |-->| SETTLING VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_SED3D => SED3D
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPFMAX,NCOUCH,NELEM2,NPOIN2,NPOIN3
      INTEGER, INTENT(IN) :: NPF(NPOIN2)
!
      DOUBLE PRECISION, INTENT(INOUT) :: MASBED,MASDEP
      DOUBLE PRECISION, INTENT(IN) :: WC(NPOIN3)
      DOUBLE PRECISION, INTENT(IN) :: TA(NPOIN3),VOLU2D(NPOIN2)
! BUG FIX TOM
      DOUBLE PRECISION, INTENT(IN) :: EPAI(NPOIN2,NCOUCH)
      DOUBLE PRECISION, INTENT(IN) :: EPAICO(NPOIN2), EPAINCO(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: HDEP(NPOIN2),FLUER(NPOIN2)
! BUG FIX T
      DOUBLE PRECISION, INTENT(IN) :: FLUDP(NPOIN2),CONC(NPOIN2,NCOUCH)
!
      DOUBLE PRECISION, INTENT(INOUT) :: TRA02(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN)    :: AT,RHOS,CFDEP
      DOUBLE PRECISION, INTENT(IN)    :: MASBED0
!
      LOGICAL, INTENT(IN)             :: TASSE,GIBSON,MIXTE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION ERROR
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
!      FLUDP calculated in FONVAS (or MURD3D_POS) FLUDP>0
!      DO I=1,NPOIN2
!        FLUX=FLUX+FLUDP(I)*VOLU2D(I)
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
!      MASDEP calculated in  fonvas
!      MASDEP= MASDEP + MASSE5
!=======================================================================
!
! COMPUTES THE MASS OF SEDIMENT BED
!
!=======================================================================
!
      CALL MASSED(MASBED,EPAI,CONC,HDEP,TRA02,NPOIN2,NPFMAX,NCOUCH,
     &            NPF,TASSE,GIBSON,RHOS,VOLU2D,CFDEP,EPAICO,EPAINCO,
     &            MIXTE)
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
        IF(MASDEP.GT.0) THEN
          WRITE(LU,*) 'MASSE TOTALE  DEPOSEE               :', MASDEP
        ELSE
          WRITE(LU,*) 'MASSE TOTALE ERODEE                 : ',-MASDEP
        ENDIF
        WRITE(LU,*) 'BILAN MASSE DU LIT (GAIN>0-  PERTE <0)  :', ERROR
      ENDIF
!
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'SEDIMENT BED MASS BALANCE AT TIME=',AT
        WRITE(LU,*) 'MASSE OF BED                        : ',MASBED
        IF(MASDEP.GT.0) THEN
          WRITE(LU,*) 'TOTAL DEPOSITED MASS               :', MASDEP
        ELSE
          WRITE(LU,*) 'TOTAL ERODED MASS                : ',-MASDEP
        ENDIF
        WRITE(LU,*) 'SEDIMENT BED MASS BALANCE  (GAIN>0 LOSS<0):', ERROR
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
