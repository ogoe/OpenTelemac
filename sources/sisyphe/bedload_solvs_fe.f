!                    ***************************
                     SUBROUTINE BEDLOAD_SOLVS_FE
!                    ***************************
!
     &(MESH,S,EBOR,MASKEL,MASK,QSX,QSY,IELMT,NPOIN,NPTFR,KENT,KDIR,KDDL,
     & LIMTEC,DT,MSK,ENTET,T1,T2,T3,T4,T8,ZFCL,HZ,HZN,GLOSEG,DIMGLO,
     & FLODEL,FLULIM,NSEG,UNSV2D,CSF_SABLE,ICLA,FLBCLA,AVA,LIQBOR,QBOR,
     & MAXADV)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/07/2011
!***********************************************************************
!
!brief    SOLVES:
!code
!+     D(HZ)
!+     ---- + DIV(QS) = 0
!+      DT
!
!warning
!+     LIMTEC is used here instead of LIEBOR. The difference is that
!+     LIMTEC is LIEBOR corrected in view of sign of u.n at boundaries
!+     then KENT, KSORT apply to LIEBOR, while KDIR and KDDL apply on
!+     LIMTEC, see bedload_diffin.f
!
!history  E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!+        11/09/1995
!+        V5P1
!+
!
!history  B. MINH DUC
!+        **/**/2002
!+        V5P3
!+
!
!history  F. HUVELIN
!+        14/09/2004
!+        V5P5
!+
!
!history  J.-M. HERVOUET
!+        29/10/2007
!+        V5P8
!+
!
!history  J.-M. HERVOUET
!+        05/09/2009
!+        V6P0
!+   NEW METHOD
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
!history  J-M HERVOUET (EDF-LNHE)
!+        27/01/2012
!+        V6P2
!+  Argument ICLA added 
!
!history  J-M HERVOUET (EDF-LNHE)
!+        14/02/2012
!+        V6P2
!+  Optimisation, and FLBCLA built and kept for use in bilan_sisyphe
!+  Treatment of QBOR added     
!
!history  J-M HERVOUET (EDF-LNHE)
!+        25/12/2012
!+        V6P3
!+  3 arguments added to VECTOS.    
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DIMGLO         |-->| FIRST DIMENSION OF GLOSEG
!| DT             |-->| TIME STEP
!| EBOR           |<->| BOUNDARY CONDITION FOR BED EVOLUTION (DIRICHLET)
!| ENTET          |-->| LOGICAL, IF YES INFORMATION IS GIVEN ON MASS CONSERVATION
!| FLBCLA         |<->| FLUXES AT BOUNDARY FOR THE CLASS
!| FLODEL         |<--| FLUXES BETWEEN POINTS (PER SEGMENT)
!| FLULIM         |<--| LIMITATION OF FLUXES
!| GLOSEG         |-->| CONNECTIVITY TABLE FOR SEGMENTS
!| HZ             |<--| NEW AVAILABLE LAYER OF SEDIMENT
!| HZN            |-->| OLD AVAILABLE LAYER OF SEDIMENT
!| ICLA           |-->| CLASS NUMBER
!| IELMT          |-->| NUMBER OF ELEMENTS
!| KDDL           |-->| CONVENTION FOR DEGREE OF FREEDOM
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| LIMTEC         |-->| TYPE OF BOUNDARY CONDITION
!| LIQBOR         |-->| TYPE OF BOUNDARY CONDITION ON BEDLOAD DISCHARGE
!| MASK           |-->| BLOCK OF MASKS, EVERY ONE FOR A TYPE OF BOUNDARY
!|                |   | SEE DIFFIN.F IN LIBRARY BIEF.
!| MASKEL         |-->| MASKING OF ELEMENTS
!| MAXADV         |-->| MAXIMUM NUMBER OF ITERATIONS (IN POSITIVE_DEPTH)
!| MESH           |<->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NSEG           |-->| NUMBER OF SEGMENTS PER CONTROL SECTION
!| QBOR           |-->| PRESCRIBED BEDLOAD DISCHARGES 
!| QSX            |-->| SOLID DISCHARGE X
!| QSY            |-->| SOLID DISCHARGE Y
!| S              |-->| VOID STRUCTURE
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| T3             |<->| WORK BIEF_OBJ STRUCTURE
!| T4             |<->| WORK BIEF_OBJ STRUCTURE
!| T8             |<->| WORK BIEF_OBJ STRUCTURE
!| UNSV2D         |-->| INVERSE OF INTEGRALS OF TEST FUNCTIONS
!| ZFCL           |<--| ZFCL=HZ-HZN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_SISYPHE, EX_BEDLOAD_SOLVS_FE => BEDLOAD_SOLVS_FE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: S,MASKEL,MASK,QSX,QSY
      INTEGER,          INTENT(IN)    :: IELMT,NPOIN,NPTFR,KENT,KDIR
      INTEGER,          INTENT(IN)    :: DIMGLO,NSEG,ICLA,KDDL,MAXADV
      INTEGER,          INTENT(IN)    :: GLOSEG(DIMGLO,2)
      DOUBLE PRECISION, INTENT(IN)    :: DT,CSF_SABLE,AVA(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: FLULIM(NSEG)
      LOGICAL,          INTENT(IN)    :: MSK,ENTET
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: FLODEL,T1,T2,T3,T4,T8
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HZ,EBOR,LIMTEC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ZFCL,FLBCLA
      TYPE(BIEF_OBJ),   INTENT(IN)    :: HZN,UNSV2D,LIQBOR,QBOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,N
!
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
!
!-----------------------------------------------------------------------
!
!     BOUNDARY FLUXES
!
      CALL VECTOR(FLBCLA,'=','FLUBOR          ',IELBOR(IELMT,1),1.D0,
     &            S,S,S,QSX,QSY,S,MESH,.TRUE.,MASK)
!
!     BOUNDARY CONDITIONS: EITHER EBOR OR QBOR PRESCRIBED (NOT THE 2)
!
      DO K=1,NPTFR
        IF(LIQBOR%I(K).EQ.KENT) THEN
!         QBOR IS GIVEN BY USER, AND POSITIVE IF ENTERING
!         HERE WE PUT THE INTERNAL USAGE <0 = ENTERING
          FLBCLA%R(K)=-QBOR%R(K)
!         EVEN IF USER HAS SPECIFIED LIEBOR=KSORT, LIMTEC MAY HAVE BEEN
!         SET TO KDIR BY CHECKING IF VELOCITY IS ENTERING, THIS IS 
!         UNWANTED HERE AS QBOR ONLY IS TAKEN INTO ACCOUNT,
!         SO DDL IS PUT TO AVOID A DIRICHLET TREATMENT IN POSITIVE_DEPTHS.
          LIMTEC%I(K)=KDDL
        ELSEIF(LIMTEC%I(K).EQ.KDIR) THEN
!         HERE THE VARIABLE WILL BE THE LAYER DEPTH OF THE SEDIMENT CLASS,
!         PUT IN T8, NOT THE EVOLUTION
          N=MESH%NBOR%I(K)
          T8%R(K)=AVA(N)*EBOR%R(K)*CSF_SABLE+HZN%R(N)
        ENDIF
      ENDDO
!
!     HERE T1 MAY NOT BE ASSEMBLED, WE WORK DIRECTLY ON MESH%W%R AFTER
!     ADD LEGO=.FALSE. WHEN IT IS AN OPTIONAL ARGUMENT
!
      CALL VECTOR(T1,'=','VGRADP          ',QSX%ELM,-1.D0,
     *            S,S,S,QSX,QSY,S,MESH,MSK,MASKEL,LEGO=.FALSE.)
!
      CALL POSITIVE_DEPTHS(T1,T2,T3,T4,HZ,HZN,MESH,
     &                     FLODEL,.TRUE.,FLBCLA,DT,UNSV2D,NPOIN,
     &                     GLOSEG(1:DIMGLO,1),GLOSEG(1:DIMGLO,2),
     &                     MESH%NBOR%I,NPTFR,T8,.FALSE.,
!                                            VOID
     &                     1,FLULIM,
     &                     LIMTEC%I,T8%R  ,KDIR,ENTET,MESH%W%R,
!                                   EBOR%R
     &                     'SISYPHE                 ',2,MAXADV)
!                                                     2 : HARDCODED
!                             OPTION FOR POSITIVE DEPTHS ALGORITHMS
!                             HERE CHOICE OF OPTION INDEPENDENT OF
!                             SEGMENT NUMBERING
!
      CALL OS('X=Y-Z   ' ,X=ZFCL,Y=HZ,Z=HZN)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
