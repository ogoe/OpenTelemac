!
!  THIS TEST CASE WORKS WITH PROPAG AND DIFFIN MODIFIED
!
!  HYDRODYNAMICS IS GIVEN IN CONDIN, AND IS NOT SOLUTION OF
!  SHALLOW WATER EQUATIONS (IT IS JUST DIVERGENCE FREE)
!
!  UTIMP_TELEMAC2D: PRINTING THE CONE HEIGHT AFTER ONE ROTATION
!                   AND THE STANDARD DEVIATION
!  PROPAG: EMPTIED SUBROUTINE, EXCEPT A FEW LINES, SEE BELOW
!  DIFFIN: CHECKING OF ENTERING AND EXITING BOUNDARIES REMOVED
!
!
!                    **************************
                     SUBROUTINE UTIMP_TELEMAC2D
!                    **************************
!
     &(LTL,ATL,GRADEBL,GRAPRDL,LISDEBL,LISPRDL)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    WRITES OUT ADDITIONAL OUTPUT REQUIRED BY THE USER.
!
!note     THIS SUBROUTINE IS CALLED IN THE SAME PLACES AS THE
!+                MAIN TELEMAC2D OUTPUT SUBROUTINE (NAMED DESIMP),
!+                I.E. CALLED TWICE:
!+
!note   (1) ONCE PER RUN, WHEN LTL==0, INDEPENDENTLY OF WHETHER
!+             'OUTPUT OF INITIAL CONDITIONS : YES' IS SET OR NOT
!note   (2) EACH TIME STEP JUST AFTER DESIMP-OUTPUT
!
!warning  USER SUBROUTINE; DOES NOTHING BY DEFAULT
!
!history  JACEK A. JANKOWSKI PINXIT, BAW KARLSRUHE, JACEK.JANKOWSKI@BAW.DE
!+        **/08/2003
!+        V5P4
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
!| ATL            |-->| TIME OF TIME STEP, IN SECONDS
!| GRADEBL        |-->| FIRST TIME STEP FOR GRAPHIC OUTPUTS
!| GRAPRDL        |-->| PERIOD OF GRAPHIC OUTPUTS
!| LISDEBL        |-->| FIRST TIME STEP FOR LISTING OUTPUTS
!| LISPRDL        |-->| PERIOD OF LISTING OUTPUTS
!| LTL            |-->| CURRENT TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_PARALLEL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: ATL
      INTEGER, INTENT(IN) :: LTL,GRADEBL,GRAPRDL,LISDEBL,LISPRDL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,ITRAC,I1(20),I2(20),IPOIN
      LOGICAL DEJAUTIMP
      DOUBLE PRECISION MINIMORUM(20),MAXIMORUM(20),MAXIM(20),MINIM(20)
      DOUBLE PRECISION EIKON,C,AREA
      DATA DEJAUTIMP/.FALSE./
      SAVE MINIMORUM,MAXIMORUM,I1,I2
      CALL CPSTVC(T%ADR(1)%P,T1)
      IF(.NOT.DEJAUTIMP) THEN
        DO K=1,20
          MINIMORUM(K)=1.D99
          MAXIMORUM(K)=-MINIMORUM(K)
          I1(K)=0
          I2(K)=0
        ENDDO
        DEJAUTIMP=.TRUE.
      ENDIF
      DO K=1,20
        MINIM(K)=1.D99
        MAXIM(K)=-MINIM(K)
      ENDDO
      DO ITRAC=1,T%N
        DO K=1,T%ADR(ITRAC)%P%DIM1
          IF(T%ADR(ITRAC)%P%R(K).LT.MINIMORUM(ITRAC)) THEN
            I1(ITRAC)=K
            MINIMORUM(ITRAC)=T%ADR(ITRAC)%P%R(K)
          ENDIF
          MINIM(ITRAC)=MIN(MINIM(ITRAC),T%ADR(ITRAC)%P%R(K))
        ENDDO
        DO K=1,T%ADR(ITRAC)%P%DIM1
          IF(T%ADR(ITRAC)%P%R(K).GT.MAXIMORUM(ITRAC)) THEN
            I2(ITRAC)=K
            MAXIMORUM(ITRAC)=T%ADR(ITRAC)%P%R(K)
          ENDIF
          MAXIM(ITRAC)=MAX(MAXIM(ITRAC),T%ADR(ITRAC)%P%R(K))
        ENDDO
        DO IPOIN=1,T%ADR(ITRAC)%P%DIM1
          EIKON=( (X(IPOIN)-15.D0)**2 + (Y(IPOIN)-10.2D0)**2 ) / 2.D0
          T1%R(IPOIN)=T%ADR(ITRAC)%P%R(IPOIN)-EXP(-EIKON)
        ENDDO
      ENDDO
      DO ITRAC=1,NTRAC
        MINIMORUM(ITRAC)=P_DMIN(MINIMORUM(ITRAC))
        MINIM(ITRAC)=P_DMIN(MINIM(ITRAC))
        PRINT*,
     &  NAMETRAC(ITRAC),' MINIMORUM=',MINIMORUM(ITRAC),' EN ',I1(ITRAC)
        PRINT*,
     &  NAMETRAC(ITRAC),' MINIM=',MINIM(ITRAC),' EN ',I1(ITRAC)
      ENDDO
      DO ITRAC=1,NTRAC
        MAXIMORUM(ITRAC)=P_DMAX(MAXIMORUM(ITRAC))
        MAXIM(ITRAC)=P_DMAX(MAXIM(ITRAC))
        PRINT*,
     &  NAMETRAC(ITRAC),' MAXIMORUM=',MAXIMORUM(ITRAC),' EN ',I2(ITRAC)
        PRINT*,
     &  NAMETRAC(ITRAC),' MAXIM=',MAXIM(ITRAC),' EN ',I2(ITRAC)
      ENDDO
!     COMPUTING THE MEAN DEVIATION
      AREA=0.D0
      DO IPOIN=1,T%ADR(1)%P%DIM1
        AREA=AREA+VOLU2D%R(IPOIN)
      ENDDO
      IF(NCSIZE.GT.1) AREA=P_DSUM(AREA)
      DO ITRAC=1,T%N
        C=0.D0
        DO IPOIN=1,T%ADR(ITRAC)%P%DIM1
          EIKON=( (X(IPOIN)-15.D0)**2 + (Y(IPOIN)-10.2D0)**2 ) / 2.D0
          C=C+VOLU2D%R(IPOIN)*(T%ADR(ITRAC)%P%R(IPOIN)-EXP(-EIKON))**2
        ENDDO
        IF(NCSIZE.GT.1) THEN
          C=P_DSUM(C)
        ENDIF
        C=C/AREA
        PRINT*,NAMETRAC(ITRAC),' 10**3 STANDARD DEVIATION=',1.D3*SQRT(C)
      ENDDO
!
!***********************************************************************
! USER OUTPUT
!
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE UTIMP_TELEMAC2D
!                    *****************
                     SUBROUTINE PROPAG
!                    *****************
!
     &(U,V,H,UCONV,VCONV,CONVV,H0,PATMOS,ATMOS,
     & HPROP,UN,VN,HN,UTILD,VTILD,HTILD,DH,DU,DV,DHN,VISC,VISC_S,FU,FV,
     & SMH,MESH,ZF,AM1,AM2,AM3,BM1,BM2,CM1,CM2,TM1,A23,A32,MBOR,
     & CV1,CV2,CV3,W1,UBOR,VBOR,AUBOR,HBOR,DIRBOR,
     & TE1,TE2,TE3,TE4,TE5,T1,T2,T3,T4,T5,T6,T7,T8,
     & LIMPRO,MASK,GRAV,ROEAU,CF,DIFVIT,IORDRH,IORDRU,LT,AT,DT,
     & TETAH,TETAHC,TETAU,TETAD,
     & AGGLOH,AGGLOU,KDIR,INFOGR,KFROT,ICONVF,
     & PRIVE,ISOUSI,BILMAS,MASSES,MASS_RAIN,YASMH,OPTBAN,CORCON,
     & OPTSUP,MSK,MASKEL,MASKPT,RO,ROVAR,
     & MAT,RHS,UNK,TB,S,BD,PRECCU,SOLSYS,CFLMAX,OPDVIT,OPTSOU,
     & NFRLIQ,SLVPRO,EQUA,VERTIC,ADJO,ZFLATS,TETAZCOMP,UDEL,VDEL,DM1,
     & ZCONV,COUPLING,FLBOR,BM1S,BM2S,CV1S,VOLU2D,V2DPAR,UNSV2D,
     & NDGA1,NDGB1,NWEIRS,NPSING,HFROT,FLULIM,YAFLULIM,
     & FLULIMEBE,YAFLULIMEBE,RAIN,PLUIE,
     & MAXADV,OPTADV_VI)
!
!***********************************************************************
! TELEMAC2D   V7P2
!***********************************************************************
!
!brief    PROPAGATION - DIFFUSION - SOURCE TERMS STEP TO SOLVE
!+                THE SAINT-VENANT EQUATIONS.
!+
!+
!+      BOUNDARY CONDITIONS:
!+
!+
!+      ==>   NEUMANN CONDITION
!+
!+
!+            * DIFFUSION   : NU DU/DN = AUBOR . U;
!+                            TREATS THE DIFFUSION MATRIX DIRECTLY
!+
!+
!+            * PROPAGATION : THE BOUNDARY TERMS ARE TREATED IN
!+                            THE SECOND MEMBERS (IMPLICIT)
!+
!+
!+      ==>   DIRICHLET CONDITION
!+
!+
!+            * DIFFUSION, PROPAGATION :
!+                            TREATED USING MODIFIED EQUATIONS IN " PROCLI "
!code
!+      IN MATRIX FORM:
!+
!+                   N+1          N+1          N+1
!+             AM1  H     +  BM1 U     +  BM2 V     =  CV1
!+
!+            T     N+1           N+1
!+          -  CM1 H      +  AM2 U                  =  CV2
!+
!+            T     N+1                        N+1
!+          -  CM2 H                   +  AM3 V     =  CV3
!
!note     BM* REPRESENT DIVERGENCE MATRICES;
!+            BM1: DERIVATION RELATIVE TO X;
!+            BM2: DERIVATION RELATIVE TO Y.
!note
!+THE TRANSPOSE OF MATRICES BM* IS EQUAL TO THE OPPOSITE
!+            OF GRADIENT. SOME SIGNS ARE THEREFORE OPPOSITE IN
!+            THE EQUATIONS OF SPEED.
!note
!+THE LAPLACIAN MATRIX (TM1) HAS BEEN INTEGRATED IN PART.
!+            THE SIGN IS THEREFORE OPPOSITE IN THE EQUATIONS OF
!+            SPEED.
!
!history  JMH
!+        07/05/2007
!+
!+   MODIFICATION ON THE SOURCES IN CASE OF MASS-LUMPING
!
!history
!+        10/06/2008
!+
!+   FINITE VOLUME ADVECTION FOR SPEEDS
!
!history
!+        02/10/2008
!+
!+   CALL TO CVTRVF (ONE MORE WORKING ARRAY)
!
!history
!+        20/07/2009
!+
!+   ICONVF (2) = 5 MANDATORY, ALL OTHER CASES ERASED
!
!history
!+        22/07/2009
!+
!+   EQUALITY OF FLUXES IMPOSED ON EITHER SIDE OF A WEIR
!
!history  J-M HERVOUET (LNHE)
!+        09/10/2009
!+        V6P0
!+   PARAMETERISED ADVECTION OPTIONS
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
!+        09/08/2011
!+        V6P2
!+   Adaptation to parallelism
!
!history  J-M HERVOUET (LNHE)
!+        249/02/2012
!+        V6P2
!+   Rain and evaporation added
!
!history  J-M HERVOUET (LNHE)
!+        09/04/2013
!+        V6P3
!+   DIMGLO=MESH%GLOSEG%DIM1 used in call to CVTRVF_POS_2. Strangely
!+   avoids an "array temporary created" with Intel compiler.
!
!history  J-M HERVOUET (LNHE)
!+        12/04/2013
!+        V6P3
!+   Value of NELBOR controlled for allowing bound checking.
!
!history  C.COULET / A.REBAI / E.DAVID (ARTELIA)
!+        12/06/2013
!+        V6P3
!+   Adaptation to the dynamic allocation of weirs
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        13/03/2014
!+        V7P0
!+   Now written to enable different numbering of boundary points and
!+   boundary segments. Arguments C0 and COTOND removed. Incident wave
!+   removed.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        17/04/2014
!+        V7P0
!+   Correction for weirs. The function P_DSUM for weirs must be called
!+   in all processors, so it must be called outside the test:
!+   IF(MESH%NPTFR.GT0) where it was in previous versions.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        16/05/2014
!+        V7P0
!+   A copy of LIMPRO is done to be sent to cvtrvf (that may change it).
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        15/03/2016
!+        V7P2
!+   Enabling advection solver 15 (ERIA) for velocities with a double
!+   to cvtrvf_pos. Advection sschemes ADV_PSI_NC and ADV_NSC_NC removed.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        25/04/2016
!+        V7P2
!+   A23%STOX and A32%STOX set to 1 to enable a call by MATVEC because
!+   these matrices are not built by MATRIX.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        07/09/2016
!+        V7P2
!+   Adaptation to splitting of cvtrvf_pos into cvtrvf_nerd and
!+   cvtrvf_eria.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A23            |<->| MATRIX
!| A32            |<->| MATRIX
!| ADJO           |-->| IF YES : ADJOINT MODE
!| AGGLOH         |-->| KEYWORD: 'MASS-LUMPING ON H'
!| AGGLOU         |-->| KEYWORD: 'MASS-LUMPING ON VELOCITY'
!| AM1            |<->| MATRIX APPLYING TO H
!| AM2            |<->| MATRIX APPLYING TO U
!| AM3            |<->| MATRIX APPLYING TO V
!| AT             |-->| TIME IN SECONDS
!| ATMOS          |-->| IF YES, ATMOSPHERIC PRESSURE IN PATMOS
!| AUBOR          |<--| LAW OF FRICTION ON BOUNDARIES
!|                |   | NUT*DU/DN=AUBOR*U+BUBOR
!| BD             |---| ??????  NOT USED
!| BILMAS         |-->| LOGICAL TRIGGERING A MASS BALANCE INFORMATION
!| BM1S           |<->| MATRIX
!| BM2            |<->| MATRIX
!| BM2S           |<->| MATRIX
!| CF             |<--| ADIMENSIONAL FRICTION COEFFICIENT
!| CFLMAX         |<--| MAXIMUM CFL NUMBER (OBSERVED IN CURRENT TIME STEP)
!| CM1            |<->| MATRIX
!| CM2            |<->| MATRIX
!| CONVV          |-->| ARRAY OF LOGICAL GIVING THE VARIABLES TO BE
!|                |   | ADVECTED
!|                |   | CONVV(1):U,V CONVV(2):H
!| CORCON         |-->| CONTINUITY CORRECTION ON POINTS WITH
!|                |   | IMPOSED DEPTH (COMPATIBLE FLUX IS COMPUTED)
!| COUPLING       |-->| STRING WITH THE LIST OF COUPLED PROGRAMMES
!| CV1            |<->| RIGHT-HAND SIDE OF LINEAR SYSTEM
!| CV2            |<->| RIGHT-HAND SIDE OF LINEAR SYSTEM
!| CV3            |<->| RIGHT-HAND SIDE OF LINEAR SYSTEM
!| CV1S           |<->| RIGHT-HAND SIDE OF LINEAR SYSTEM
!| DH             |<--| H(N+1)-H(N)
!| DHN            |<--| H(N)-H(N-1)
!| DIFVIT         |-->| IF YES, DIFFUSION OF VELOCITY
!| DIRBOR         |<--| BLOCK WITH DIRICHLET BOUNDARY CONDITIONS
!| DM1            |-->| THE PIECE-WISE CONSTANT PART OF ADVECTION FIELD
!|                |   | IS DM1*GRAD(ZCONV), SEE SOLSYS.
!| DT             |-->| TIME STEP
!| DU             |<--| U(N+1)-U(N)
!| DV             |<--| V(N+1)-V(N)
!| EQUA           |-->| KEYWORD: 'EQUATIONS'
!| FLBOR          |<--| FLUXES AT BOUNDARY POINTS
!| FLULIM         |-->| FLUX LIMITATION
!| FU             |<->| SOURCE TERMS ON VELOCITY U
!| FV             |<->| SOURCE TERMS ON VELOCITY V
!| GRAV           |-->| GRAVITY
!| H0             |-->| REFERENCE DEPTH
!| HBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON DEPTH
!| HN             |-->| DEPTH AT TIME T(N)
!| HFROT          |-->| KEYWORD: 'DEPTH IN FRICTION TERMS'
!| HPROP          |-->| PROPAGATION DEPTH
!| HTILD          |-->| DEPTH AFTER ADVECTION
!| ICONVF         |-->| TYPE OF ADVECTION: 4 INTEGERS
!|                |   | ICONVF(1) : U AND V
!|                |   | ICONVF(2) : H (MANDATORY VALUE = 5)
!|                |   | ICONVF(3) : TRACERS
!|                |   | ICONVF(4) : K AND EPSILON
!| INFOGR         |-->| IF YES, INFORMATION ON GRADIENT
!| IORDRH         |-->| ORDER OF INITIAL GUESS OF H
!| IORDRU         |-->| ORDER OF INITIAL GUESS OF U
!| ISOUSI         |-->| NUMBER OF SUB-ITERATION IN THE TIME-STEP
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| KFROT          |-->| KEYWORD: 'LAW OF BOTTOM FRICTION'
!| LIMPRO         |<->| BOUNDARY CONDITIONS FOR H, U V PER POINTS
!|                |   | AND SEGMENTS
!| LT             |-->| ITERATION NUMBER
!| MASK           |-->| BLOCK OF MASKS FOR SEGMENTS :
!|                |   | MASK(MSK1): 1. IF KDIR ON U 0. ELSE
!|                |   | MASK(MSK2): 1. IF KDIR ON V 0. ELSE
!|                |   | MASK(MSK3): 1. IF KDDL ON U 0. ELSE
!|                |   | MASK(MSK4): 1. IF KDDL ON V 0. ELSE
!|                |   | MASK(MSK6): 1. IF KNEU ON V 0. ELSE
!|                |   | MASK(MSK7): 1. IF KOND 0. ELSE
!|                |   | MASK(MSK9): 1. IF KDIR ON H (POINT)
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MASKPT         |-->| MASKING PER POINT.
!|                |   | =1. : NORMAL   =0. : MASKED
!| MASSES         |-->| MASS OF WATER ADDED BY SOURCE TERM
!| MASS_RAIN      |-->| MASS ADDED BY RAIN OR EVAPORATION
!| MAXADV         |-->| MAXIMUM NUMBER OF ITERATIONS FOR ADVECTION SCHEMES
!| MAT            |<--| BLOCK OF MATRICES
!| MBOR           |<--| BOUNDARY MATRIX
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NPSING         |-->| NUMBER OF POINTS FOR EVERY SINGULARITY.
!| NDGA1          |-->| NDGA1%ADR(I)%I(NP) : BOUNDARY NUMBER OF POINT NP
!|                |   | OF WEIR I (side1)
!| NDGB1          |-->| NDGB1%ADR(I)%I(NP) : BOUNDARY NUMBER OF POINT NP
!|                |   | OF WEIR I (side2)
!| NWEIRS         |-->| NUMBER OF SINGULARITIES
!| OPDVIT         |-->| OPTION FOR DIFFUSION OF VELOCITIES
!| OPTADV_VI      |-->| OPTION FOR THE ADVECTION SCHEME OF VELOCITIES
!| OPTBAN         |-->| KEYWORD: 'OPTION FOR THE TREATMENT OF TIDAL FLATS'
!| OPTSOU         |-->| KEYWORD: 'TYPE OF SOURCES'
!| OPTSUP         |-->| KEYWORD: 'SUPG OPTION'
!| PATMOS         |-->| ATMOSPHERIC PRESSURE
!| PLUIE          |-->| RAIN OR EVAPORATION IN M/S IN A BIEF_OBJ
!| PRECCU         |-->| KEYWORD: 'C-U PRECONDITIONING'
!| PRIVE          |-->| BLOCK OF WORK BIEF_OBJ STRUCTURES
!| RAIN           |-->| IF YES, RAIN OR EVAPORATION
!| RHS            |<->| BLOCK OF PRIVATE BIEF_OBJ STRUCTURES
!| RO             |-->| WATER DENSITY IF VARIABLE
!| ROEAU          |-->| WATER DENSITY
!| ROVAR          |-->| IF YES, VARIABLE WATER DENSITY.
!| S              |-->| VOID STRUCTURE
!| SLVPRO         |-->| SOLVER STRUCTURE FOR PROPAGATION
!| SMH            |-->| SOURCE TERM IN CONTINUITY EQUATION
!| SOLSYS         |-->| KEYWORD: 'TREATMENT OF THE LINEAR SYSTEM'
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| T3             |<->| WORK BIEF_OBJ STRUCTURE
!| T4             |<->| WORK BIEF_OBJ STRUCTURE
!| T5             |<->| WORK BIEF_OBJ STRUCTURE
!| T6             |<->| WORK BIEF_OBJ STRUCTURE
!| T7             |<->| WORK BIEF_OBJ STRUCTURE
!| T8             |<->| WORK BIEF_OBJ STRUCTURE
!| TB             |<->| BLOCK WITH T1,T2,...
!| TE1            |<->| WORK BIEF_OBJ STRUCTURE FOR ELEMENTS
!| TE2            |<->| WORK BIEF_OBJ STRUCTURE FOR ELEMENTS
!| TE3            |<->| WORK BIEF_OBJ STRUCTURE FOR ELEMENTS
!| TE4            |<->| WORK BIEF_OBJ STRUCTURE FOR ELEMENTS
!| TE5            |<->| WORK BIEF_OBJ STRUCTURE FOR ELEMENTS
!| TETAD          |-->| IMPLICITATION ON DIFFUSION
!| TETAH          |-->| IMPLICITATION OF H IN U EQUATION
!| TETAHC         |-->| IMPLICITATION OF H IN CONTINUITY
!| TETAU          |-->| IMPLICITATION OF U AND
!| TM1            |<->| MATRIX
!| UBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY U
!| VBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY V
!| UCONV          |-->| X-COMPONENT OF ADVECTION VELOCITY FIELD
!| VCONV          |-->| Y-COMPONENT OF ADVECTION VELOCITY FIELD
!| UDEL           |<--| COMPATIBLE X-COMPONENT OF ADVECTION VELOCITY FIELD
!| UN             |<->| X-COMPONENT OF VELOCITY AT TIME T(N)
!| VN             |<->| Y-COMPONENT OF VELOCITY AT TIME T(N)
!| UNK            |<->| BLOCK OF UNKNOWNS
!| UNSV2D         |-->| INVERSE OF INTEGRALS OF TEST FUNCTIONS
!| UTILD          |-->| VELOCITY U IF ADVECTED BY CHARACTERISTICS
!| V2DPAR         |-->| INTEGRAL OF TEST FUNCTIONS, ASSEMBLED IN PARALLEL
!| VBOR           |-->| CONDITIONS AUX LIMITES SUR V.
!| VDEL           |<--| COMPATIBLE Y-COMPONENT OF ADVECTION VELOCITY FIELD
!| VERTIC         |-->| IF YES, THERE ARE VERTICAL STRUCTURES
!| VISC           |-->| VISCOSITY COEFFICIENTS ALONG X,Y AND Z .
!|                |   | IF P0 : PER ELEMENT
!|                |   | IF P1 : PERR POINT
!| VISC_S         |<->| WORK ARRAY FOR SAVING VISC
!| VOLU2D         |-->| INTEGRAL OF TEST FUNCTIONS, NOT ASSEMBLED IN PARALLEL
!| VTILD          |-->| VELOCITY V IF ADVECTED BY CHARACTERISTICS
!| W1             |<->| WORK ARRAY
!| YAFLULIM       |-->| IF, YES, FLULIM TAKEN INTO ACCOUNT
!| YASMH          |-->| IF YES, SMH TAKEN INTO ACCOUNT
!| ZCONV          |<--| THE PIECE-WISE CONSTANT PART OF ADVECTION FIELD
!|                |   | IS DM1*GRAD(ZCONV), SEE SOLSYS.
!| ZF             |-->| ELEVATION OF BOTTOM
!| ZFLATS         |<--| ELEVATION OF BOTTOM, MODIFIED FOR TIDAL FLATS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC, ONLY : ADV_CAR,ADV_SUP,ADV_NSC,ADV_PSI,
     &                                 ADV_LPO,
     &                                 ADV_NSC_TF,ADV_PSI_TF,ADV_LPO_TF,
     &                                 KDDL
      USE DECLARATIONS_TELEMAC2D, ONLY : TYPSEUIL,IT1,IT2,TB2,NCO_DIST,
     &                                   NSP_DIST,OPT_HNEG
!
      USE INTERFACE_TELEMAC2D, EX_PROPAG => PROPAG
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: LT,OPTSUP(4),KDIR,KFROT,ICONVF(4),NWEIRS
      INTEGER, INTENT(IN) :: IORDRH,IORDRU,ISOUSI,OPTBAN,OPTSOU,SOLSYS
      INTEGER, INTENT(IN) :: OPDVIT,NFRLIQ,HFROT,MAXADV,OPTADV_VI
      DOUBLE PRECISION, INTENT(IN)    :: TETAU,TETAD,TETAH,AGGLOH,AGGLOU
      DOUBLE PRECISION, INTENT(IN)    :: TETAHC,AT,DT,GRAV,ROEAU
      DOUBLE PRECISION, INTENT(IN)    :: TETAZCOMP
      DOUBLE PRECISION, INTENT(INOUT) :: CFLMAX,MASSES,MASS_RAIN
      LOGICAL, INTENT(IN) :: BILMAS,ATMOS,DIFVIT,INFOGR,CONVV(4),MSK
      LOGICAL, INTENT(IN) :: YASMH,ROVAR,PRECCU,VERTIC,ADJO,CORCON
      LOGICAL, INTENT(IN) :: YAFLULIM,RAIN,YAFLULIMEBE
      TYPE(SLVCFG), INTENT(INOUT)     :: SLVPRO
      CHARACTER(LEN=20),  INTENT(IN)  :: EQUA
      CHARACTER(LEN=*) ,  INTENT(IN)  :: COUPLING
!
!  STRUCTURES OF VECTORS
!
      TYPE(BIEF_OBJ), INTENT(IN)    :: NPSING,NDGA1,NDGB1,FLULIMEBE
      TYPE(BIEF_OBJ), INTENT(IN)    :: UCONV,VCONV,SMH,UN,VN,HN
      TYPE(BIEF_OBJ), INTENT(IN)    :: VOLU2D,V2DPAR,UNSV2D,FLULIM
      TYPE(BIEF_OBJ), INTENT(INOUT) :: RO,UDEL,VDEL,DM1,ZCONV,FLBOR
      TYPE(BIEF_OBJ), INTENT(IN)    :: UTILD,VTILD,PATMOS,CF
      TYPE(BIEF_OBJ), INTENT(INOUT) :: U,V,H,CV1,CV2,CV3,PRIVE,DH,DHN
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CV1S
      TYPE(BIEF_OBJ), INTENT(INOUT) :: DU,DV,FU,FV,VISC,VISC_S,HTILD
      TYPE(BIEF_OBJ), INTENT(INOUT) :: UBOR,VBOR,HBOR,AUBOR,LIMPRO
      TYPE(BIEF_OBJ), INTENT(IN)    :: MASKEL,MASKPT,ZF,PLUIE
      TYPE(BIEF_OBJ), INTENT(IN)    :: HPROP,H0
!
!     TE : BY ELEMENT               TE4,TE5 ONLY IF OPTBAN=3
      TYPE(BIEF_OBJ), INTENT(INOUT) :: TE1,TE2,TE3,TE4,TE5,ZFLATS
!     T  : BY POINT
      TYPE(BIEF_OBJ), INTENT(INOUT) :: T1,T2,T3,T4,T5,T6,T7,T8
      TYPE(BIEF_OBJ), INTENT(INOUT) :: W1
!     DUMMY STRUCTURE
      TYPE(BIEF_OBJ), INTENT(IN)    :: S
!
!-----------------------------------------------------------------------
!
!  STRUCTURES OF MATRICES
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: AM1,AM2,AM3,BM1,BM2,CM1,CM2,TM1
      TYPE(BIEF_OBJ), INTENT(INOUT) :: A23,A32,MBOR,BM1S,BM2S
!
!-----------------------------------------------------------------------
!
!  STRUCTURES OF BLOCKS
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: MASK,MAT,RHS,UNK,TB,BD,DIRBOR
!
!-----------------------------------------------------------------------
!
!  STRUCTURE OF MESH
!
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
      CALL OS('X=Y     ',X=UDEL,Y=U)
      CALL OS('X=Y     ',X=VDEL,Y=V)
      CALL OS('X=0     ',X=DM1)
      DO I=1,3*MESH%NELMAX
        ZCONV%R(I)=0.D0
      ENDDO
!
!     TO AVOID CALLING CORRECTION_DEPTH_2D
!
      OPT_HNEG=1
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       *****************
                        SUBROUTINE CONDIN
!                       *****************
!
!***********************************************************************
! TELEMAC-2D VERSION 5.0         19/08/98  J-M HERVOUET TEL: 30 87 80 18
!
!***********************************************************************
!
!     FONCTION  : INITIALISATION DES GRANDEURS PHYSIQUES H, U, V ETC
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |                | -- |
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN,ITRAC
!
      DOUBLE PRECISION EIKON
!
      INTRINSIC EXP
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DU TEMPS
!
      AT = 0.D0
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DES VITESSES : VITESSES NULLES
!
      CALL OS( 'X=C     ' , U , U , U , 0.D0 )
      CALL OS( 'X=C     ' , V , V , V , 0.D0 )
      DO IPOIN=1,NPOIN
        U%R(IPOIN) = -(Y(IPOIN)-10.05D0)
        V%R(IPOIN) =  (X(IPOIN)-10.05D0)
      ENDDO
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DE H , LA HAUTEUR D'EAU
!
      IF(CDTINI(1:10).EQ.'COTE NULLE') THEN
        CALL OS( 'X=C     ' , H , H  , H , 0.D0 )
        CALL OS( 'X=X-Y   ' , H , ZF , H , 0.D0 )
      ELSEIF(CDTINI(1:14).EQ.'COTE CONSTANTE') THEN
        CALL OS( 'X=C     ' , H , H  , H , COTINI )
        CALL OS( 'X=X-Y   ' , H , ZF , H , 0.D0   )
      ELSEIF(CDTINI(1:13).EQ.'HAUTEUR NULLE') THEN
        CALL OS( 'X=C     ' , H , H  , H , 0.D0  )
      ELSEIF(CDTINI(1:13).EQ.'PARTICULIERES') THEN
      DO IPOIN=1,NPOIN
!       EIKON=((X(IPOIN)-10.05D0)**2+(Y(IPOIN)-10.05D0)**2)/4.D0
!       H%R(IPOIN) = 2.4D0 * ( 1.D0 + EXP(-EIKON) )
        H%R(IPOIN) = 2.D0
      ENDDO
      ELSE
        WRITE(LU,*) 'CONDIN : CONDITION INITIALE NON PREVUE : ',CDTINI
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DES TRACEURS
!
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          DO IPOIN=1,NPOIN
            EIKON=( (X(IPOIN)-15.D0)**2 + (Y(IPOIN)-10.2D0)**2 ) / 2.D0
            T%ADR(ITRAC)%P%R(IPOIN) = EXP(-EIKON)
          ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
! INITIALISATION DE LA VISCOSITE
!
      CALL OS( 'X=C     ' , VISC , VISC , VISC , PROPNU )
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    *****************
                     SUBROUTINE DIFFIN
!                    *****************
!
     &(MASKTR,LIMTRA,LITBOR,CLT,U,V,XNEBOR,YNEBOR,NBOR,
     & NPTFR,KENT,KSORT,KLOG,KNEU,KDIR,KDDL,
     & ICONV,NELBOR,NPOIN,NELMAX,MSK,MASKEL,
     & NFRLIQ,THOMFR,FRTYPE,TN,TBOR,MESH,NUMLIQ,IKLBOR,NELEB,NELEBX)
!
!***********************************************************************
! BIEF   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES THE BOUNDARY CONDITIONS FOR TRACER DIFFUSION.
!
!history  J-M HERVOUET (LNH)
!+        25/06/2008
!+        V5P9
!+   MOVED FROM TELEMAC-2D TO ALLOW CALL BY SISYPHE
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
!history  J-M HERVOUET (LNH)
!+        28/046/2011
!+        V6P1
!+   LIQUID BOUNDARIES MASK ADDED
!+   CALL PARCOM_BORD DELETED (NOT USEFUL, WE DEAL HERE WITH SEGMENTS
!+   WHICH BELONG TO A SINGLE PROCESSOR)
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        13/03/2014
!+        V7P0
!+   Now written to enable different numbering of boundary points and
!+   boundary segments.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CLT            |<--| A MODIFIED COPY OF LITBOR.
!| FRTYPE         |-->| TYPE OF BOUNDARY CONDITIONS
!|                |   | 1: NORMAL   2: THOMPSON
!| ICONV          |-->| OPTION FOR ADVECTION : 1) CHARACTERISTICS
!|                |   |                        2) SUPG, ETC.
!| IKLBOR         |-->| CONNECTIVITY TABLE FOR BOUNDARY ELEMENTS
!| KDDL           |-->| CONVENTION FOR DEGREE OF FREEDOM
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| KNEU           |-->| CONVENTION FOR NEUMANN CONDITION
!| KSORT          |-->| CONVENTION FOR LIQUID OUTPUT WITH FREE VALUE
!| LIMTRA         |<--| TECHNICAL BOUNDARY CONDITIONS FOR TRACERS
!| LITBOR         |-->| PHYSICAL BOUNDARY CONDITIONS FOR TRACERS
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NELBOR         |-->| FOR THE KTH BOUNDARY EDGE, GIVES THE CORRESPONDING
!|                |   | ELEMENT.
!| NELEB          |-->| NUMBER OF BOUNDARY ELEMENTS
!| NELEBX         |-->| MAXIMUM NUMBER OF BOUNDARY ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| TBOR           |-->| DIRICHLET BOUNDARY CONDITIONS ON TRACERS
!| THOMFR         |-->| IF YES, THERE ARE THOMPSON BOUNDARY CONDITIONS
!| TN             |-->| TRACERS AT OLD TIME STEP
!| U              |-->| X-COMPONENT OF VELOCITY
!| V              |-->| Y-COMPONENT OF VELOCITY
!| XNEBOR         |-->| X-COMPONENT OF EXTERNAL NORMAL BOUNDARY VECTOR
!| YNEBOR         |-->| Y-COMPONENT OF EXTERNAL NORMAL BOUNDARY VECTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_DIFFIN => DIFFIN
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: MASKTR,TBOR
      TYPE(BIEF_OBJ), INTENT(IN)    :: TN
      INTEGER, INTENT(IN)    :: NELEB,NELEBX
      INTEGER, INTENT(IN)    :: NPOIN,NPTFR,NELMAX,ICONV,NFRLIQ
      INTEGER, INTENT(IN)    :: LITBOR(NPTFR),NBOR(NPTFR)
      INTEGER, INTENT(INOUT) :: LIMTRA(NPTFR),CLT(NPTFR)
      INTEGER, INTENT(IN)    :: IKLBOR(NELEBX,2)
      INTEGER, INTENT(IN)    :: KENT,KSORT,KLOG,KDIR,KDDL,KNEU
      INTEGER, INTENT(IN)    :: NELBOR(NELEBX),NUMLIQ(NPTFR)
      INTEGER, INTENT(IN)    :: FRTYPE(NFRLIQ)
!
      DOUBLE PRECISION, INTENT(IN) :: U(NPOIN), V(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: XNEBOR(NPTFR), YNEBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: MASKEL(NELMAX)
!
      LOGICAL, INTENT(IN) :: MSK,THOMFR
!
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,K1,K2,IELEM,DIR,DDL,NEU,OND,NONEU,IFRLIQ,IELEB
      DOUBLE PRECISION USCALN
!
!-----------------------------------------------------------------------
!
      DIR=1
      DDL=2
      NEU=3
      OND=4
      NONEU=5
!
!     CLT CONTAINS ARRAY LITBOR, POSSIBLY MODIFIED ACCORDING TO THE SIGN
!     OF U.N ON THE LIQUID BOUNDARIES, WHERE N IS THE OUTGOING NORMAL.
!
      DO K=1,NPTFR
        CLT(K) = LITBOR(K)
!       LOCATES THE LIQUID BOUNDARIES:
!!      IF(CLT(K).EQ.KENT) THEN
!!        USCALN = U(NBOR(K))*XNEBOR(K) + V(NBOR(K))*YNEBOR(K)
!         OUTGOING VELOCITY, FREE TRACER
!!        IF(USCALN.GT.0.D0) CLT(K) = KSORT
!!      ELSEIF(CLT(K).EQ.KSORT) THEN
!!        USCALN = U(NBOR(K))*XNEBOR(K) + V(NBOR(K))*YNEBOR(K)
!
!         INCOMING VELOCITY, TRACER IMPOSED AT THE LAST VALUE
!!        IF(USCALN.LT.0.D0) THEN
!!          TBOR%R(K)=TN%R(NBOR(K))
!!          CLT(K) = KENT
!!        ENDIF
!!      ENDIF
      ENDDO
!
!     BUILDS ARRAY MASKTR ACCORDING TO CLT
!
!     MASKTR EQUALS 1 FOR A SEGMENT OF TYPE NEUMANN, 0 OTHERWISE
!
!     A SEGMENT IS OF TYPE NEUMANN IF AT LEAST ONE OF ITS POINTS
!     IS SPECIFIED AS NEUMANN BY THE USER.
!
!
!     INITIALISES THE MASKS TO 0
!
      CALL OS('X=0     ',MASKTR)
!
      DO IELEB = 1 , NELEB
        K1=IKLBOR(IELEB,1)
        K2=IKLBOR(IELEB,2)
        IF(CLT(K1).EQ.KLOG.OR.CLT(K2).EQ.KLOG) THEN
!         SEGMENTS OF TYPE NEUMANN
          MASKTR%ADR(NEU)%P%R(IELEB)=1.D0
        ELSEIF(CLT(K1).EQ.KENT.AND.CLT(K2).EQ.KSORT) THEN
!         SEGMENTS OF TYPE EXIT
          MASKTR%ADR(DDL)%P%R(IELEB)=1.D0
        ELSEIF(CLT(K1).EQ.KSORT.OR.CLT(K2).EQ.KSORT) THEN
          MASKTR%ADR(DDL)%P%R(IELEB)=1.D0
        ELSEIF(CLT(K1).EQ.KSORT.AND.CLT(K2).EQ.KENT) THEN
!         SEGMENTS OF TYPE EXIT
          MASKTR%ADR(DDL)%P%R(IELEB)=1.D0
        ELSEIF(CLT(K1).EQ.KENT.OR.CLT(K2).EQ.KENT) THEN
          MASKTR%ADR(DIR)%P%R(IELEB)=1.D0
        ELSE
          IF(LNG.EQ.1) WRITE(LU,100)
          IF(LNG.EQ.2) WRITE(LU,101)
100       FORMAT(1X,'DIFFIN : CAS NON PREVU')
101       FORMAT(1X,'DIFFIN : UNEXPECTED CASE')
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!
!  POSSIBLE MASKING
!
      IF(MSK) THEN
        DO IELEB = 1 , NELEB
          K1=IKLBOR(IELEB,1)
          IELEM=NELBOR(IELEB)
          MASKTR%ADR(DIR)%P%R(IELEB) = MASKTR%ADR(DIR)%P%R(IELEB) *
     &                                                   MASKEL(IELEM)
          MASKTR%ADR(DDL)%P%R(IELEB) = MASKTR%ADR(DDL)%P%R(IELEB) *
     &                                                   MASKEL(IELEM)
          MASKTR%ADR(NEU)%P%R(IELEB) = MASKTR%ADR(NEU)%P%R(IELEB) *
     &                                                   MASKEL(IELEM)
          MASKTR%ADR(OND)%P%R(IELEB) = MASKTR%ADR(OND)%P%R(IELEB) *
     &                                                   MASKEL(IELEM)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     LIQUID BOUNDARIES MASK
!
      DO IELEB=1,NELEB
        MASKTR%ADR(NONEU)%P%R(IELEB)=1.D0-MASKTR%ADR(NEU)%P%R(IELEB)
      ENDDO
!
!-----------------------------------------------------------------------
!
! FROM PHYSICAL TO TECHNICAL CONDITIONS
!
      DO K=1,NPTFR
!
        IF(CLT(K).EQ.KENT ) THEN
!
!         ENTERING THE DOMAIN: IMPOSED TRACER
!
          LIMTRA(K) = KDIR
!
        ELSEIF(CLT(K).EQ.KSORT) THEN
!
!         LEAVING THE DOMAIN : FREE IF SUPG OR PSI SCHEME,
!                              RESULT OF IMPOSED ADVECTION OTHERWISE
!
          IF(ICONV.EQ.1) THEN
!           SEE DIFFCL : TTILD PUT IN TBOR
            LIMTRA(K) = KDIR
          ELSE
            LIMTRA(K) = KDDL
          ENDIF
!
        ELSEIF(CLT(K).EQ.KLOG ) THEN
!
!         WALL: NEUMANN CONDITIONS (IT'S NOT ACTUALLY USED)
!
          LIMTRA(K) = KNEU
!
        ELSE
!
!         ERROR, UNKNOWN VALUE OF LITBOR
!
          IF(LNG.EQ.1) WRITE(LU,10) K,LITBOR(K)
          IF(LNG.EQ.2) WRITE(LU,12) K,LITBOR(K)
10        FORMAT(1X,'DIFFIN: POINT ',1I6,' LITBOR= ',1I6,' ?????')
12        FORMAT(1X,'DIFFIN: POINT ',1I6,' LITBOR= ',1I6,' ?????')
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
      ENDDO
!
!----------------------------------------------------------------------
!
!     POST-TREATMENT FOR LIQUID BOUNDARY CONDITIONS (THOMPSON METHOD)
!     THE TRACER BOUNDARY CONDITION THEN IS OF TYPE DIRICHLET
!
      IF(NFRLIQ.GT.0.AND.THOMFR) THEN
!
        DO K= 1 , NPTFR
          IFRLIQ=NUMLIQ(K)
          IF(IFRLIQ.GT.0) THEN
            IF(FRTYPE(IFRLIQ).EQ.2) LIMTRA(K) = KDIR
          ENDIF
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
