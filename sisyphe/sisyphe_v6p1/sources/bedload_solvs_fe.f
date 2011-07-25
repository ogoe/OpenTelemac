!                    *****************************
                     SUBROUTINE BEDLOAD_SOLVS_FE !
!                    *****************************
!
     &(MESH,S,EBOR,MASKEL,MASK,
     & QSX,QSY,IELMT,NPOIN,NPTFR,KENT,KDIR,LIMTEC,DT,
     & MSK, ENTET,T1,T2,T3,T4,T8,
     & ZFCL,HZ,HZN,GLOSEG,DIMGLO,FLODEL,FLULIM,NSEG,UNSV2D,CSF_SABLE)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    SOLVES:
!code
!+     D(HZ)
!+     ---- + DIV(T) = 0
!+      DT
!
!history  E. PELTIER;C. LENORMANT; J.-M. HERVOUET
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
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DIMGLO         |---|
!| DT             |-->| TIME STEP
!| EBOR           |<->| BOUNDARY CONDITION FOR BED EVOLUTION (DIRICHLET)
!| ENTET          |-->| LOGICAL, IF YES INFORMATION IS GIVEN ON MASS CONSERVATION
!| FLODEL         |---|
!| FLULIM         |---|
!| GLOSEG         |---|
!| HZ             |<--| NEW AVAILABLE LAYER OF SEDIMENT
!| HZN            |-->| OLD AVAILABLE LAYER OF SEDIMENT
!| IELMT          |-->| NUMBER OF ELEMENTS
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| LIMTEC         |<->| TYPE OF BOUNDARY CONDITION ***
!| MASK           |-->| BLOCK OF MASKS, EVERY ONE FOR A TYPE OF BOUNDARY
!|                |   | SEE DIFFIN.F IN LIBRARY BIEF.
!| MASKEL         |-->| MASKING OF ELEMENTS
!| MESH           |<->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NSEG           |-->| NUMBER OF SEGMENTS PER CONTROL SECTION 
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
      TYPE(BIEF_OBJ),   INTENT(IN)    :: S,LIMTEC,MASKEL,MASK,QSX,QSY
      INTEGER,          INTENT(IN)    :: IELMT,NPOIN,NPTFR,KENT,KDIR
      INTEGER,          INTENT(IN)    :: DIMGLO,NSEG
      INTEGER,          INTENT(IN)    :: GLOSEG(DIMGLO,2)
      DOUBLE PRECISION, INTENT(IN)    :: DT,CSF_SABLE
      DOUBLE PRECISION, INTENT(INOUT) :: FLULIM(NSEG)
      LOGICAL,          INTENT(IN)    :: MSK,ENTET
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: FLODEL,T1,T2,T3,T4,T8
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HZ,EBOR
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ZFCL
      TYPE(BIEF_OBJ),   INTENT(IN)    :: HZN,UNSV2D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K
!
!-----------------------------------------------------------------------
!
!     BOUNDARY FLUXES
!
      CALL CPSTVC(QSX,T4)
      CALL OS('X=C     ',X=T4,C=1.D0)
      CALL VECTOR(T8, '=', 'FLUBDF          ',IELBOR(IELMT,1),1.D0,
     &            T4,S,S,QSX,QSY,S,MESH,.TRUE.,MASK)
!
!     HERE THE VARIABLE WILL BE THE LAYER DEPTH OF THE SEDIMENT CLASS
!     NOT THE EVOLUTION
!
      DO K=1,NPTFR
        IF(LIMTEC%I(K).EQ.KDIR) THEN
!         JMH: FACTOR AVAIL MISSING ? (EBOR OBTAINED FOR EVERY CLASS ?)
          EBOR%R(K)=EBOR%R(K)*CSF_SABLE+HZN%R(MESH%NBOR%I(K))
        ENDIF
      ENDDO
!
!     CALL VECTOR(T1,'=','VGRADP          ',QSX%ELM,-1.D0,
!    *            S,S,S,QSX,QSY,S,MESH,MSK,MASKEL)
!                 T1 AS HUGRADP IS NOT USED AS AN ASSEMBLED VECTOR
!                 BUT TO GET THE NON ASSEMBLED FORM MESH%W
!     JUST LIKE CALL VECTOR BUT WITHOUT ASSEMBLING T1 BECAUSE LEGO IS SET
!     TO FALSE (ONLY NON ASSEMBLED MESH%W%R IS USED AFTER)
      CALL VECTOS(T1%R,'=','VGRADP          ',-1.D0,
     &            S%R,S%R,S%R,QSX%R,QSY%R,S%R,
     &            S,S,S,QSX,QSY,S,
!                           LEGO
     &            MESH%W%R,.FALSE.,
     &            MESH%XEL%R  , MESH%YEL%R  , MESH%ZEL%R  ,
     &            MESH%SURFAC%R,MESH%IKLE%I,MESH%NBOR%I,
     &            MESH%XSGBOR%R, MESH%YSGBOR%R, MESH%ZSGBOR%R,
     &            BIEF_NBPTS(QSX%ELM,MESH),MESH%NELEM,MESH%NELMAX,
     &            QSX%ELM,MESH%LV,MSK,MASKEL%R,MESH)
!
      CALL POSITIVE_DEPTHS(T1,T2,T3,T4,HZ,HZN,MESH,
     &                     FLODEL,.TRUE.,T8,DT,UNSV2D,NPOIN,
     &                     GLOSEG(1:DIMGLO,1),GLOSEG(1:DIMGLO,2),
     &                     MESH%NBOR%I,NPTFR,.FALSE.,T8,.FALSE.,
     &                     1,FLULIM,
     &                     LIMTEC%I,EBOR%R,KDIR,ENTET,MESH%W%R,
     &                     'SISYPHE                 ',2)
!                                                     2 : HARDCODED
!                             OPTION FOR POSITIVE DEPTHS ALGORITHMS
!                             HERE CHOICE OF OPTION INDEPENDENT OF
!                             SEGMENT NUMBERING
!
      CALL OS('X=Y-Z   ' ,X=ZFCL,Y=HZ,Z=HZN)
!
!     DIRICHLET CONDITIONS
!
      DO K=1,NPTFR
        IF(LIMTEC%I(K).EQ.KDIR) THEN
!         BOUNDARY CONDITIONS OF HZ MINUS HZN
!         HERE THIS IS MASS, DIVISION BY CSF_SABLE DONE LATER
          ZFCL%R(MESH%NBOR%I(K))=EBOR%R(K)-HZN%R(MESH%NBOR%I(K))
!         ORIGINAL EBOR RESTORED
          EBOR%R(K)=(EBOR%R(K)-HZN%R(MESH%NBOR%I(K)))/CSF_SABLE
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
