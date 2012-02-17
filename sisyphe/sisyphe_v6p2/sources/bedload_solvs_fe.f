!                    ***************************
                     SUBROUTINE BEDLOAD_SOLVS_FE
!                    ***************************
!
     &(MESH,S,EBOR,MASKEL,MASK,QSX,QSY,IELMT,NPOIN,NPTFR,KENT,KDIR,
     & LIMTEC,DT,MSK,ENTET,T1,T2,T3,T4,T8,ZFCL,HZ,HZN,GLOSEG,DIMGLO,
     & FLODEL,FLULIM,NSEG,UNSV2D,CSF_SABLE,ICLA,FLBCLA,AVA)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/07/2011
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
!history  J-M HERVOUET (EDF-LNHE)
!+        27/01/2012
!+        V6P2
!+  Argument ICLA added 
!
!history  J-M HERVOUET (EDF-LNHE)
!+        14/02/2012
!+        V6P2
!+  Optimisation, and FLBCLA built and kept for use in bilan_sisyphe     
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
      INTEGER,          INTENT(IN)    :: DIMGLO,NSEG,ICLA
      INTEGER,          INTENT(IN)    :: GLOSEG(DIMGLO,2)
      DOUBLE PRECISION, INTENT(IN)    :: DT,CSF_SABLE,AVA(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: FLULIM(NSEG)
      LOGICAL,          INTENT(IN)    :: MSK,ENTET
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: FLODEL,T1,T2,T3,T4,T8
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HZ,EBOR
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ZFCL,FLBCLA
      TYPE(BIEF_OBJ),   INTENT(IN)    :: HZN,UNSV2D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,I,N
      DOUBLE PRECISION MASINI,MASFIN,FLUXB
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
!     HERE THE VARIABLE WILL BE THE LAYER DEPTH OF THE SEDIMENT CLASS,
!     PUT IN T8, NOT THE EVOLUTION
!
      DO K=1,NPTFR
        IF(LIMTEC%I(K).EQ.KDIR) THEN
          N=MESH%NBOR%I(K)
          T8%R(K)=AVA(N)*EBOR%R(K)*CSF_SABLE+HZN%R(N)
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
     &                     FLODEL,.TRUE.,FLBCLA,DT,UNSV2D,NPOIN,
     &                     GLOSEG(1:DIMGLO,1),GLOSEG(1:DIMGLO,2),
     &                     MESH%NBOR%I,NPTFR,.FALSE.,T8,.FALSE.,
!                                                    VOID
     &                     1,FLULIM,
     &                     LIMTEC%I,T8%R  ,KDIR,ENTET,MESH%W%R,
!                                   EBOR%R
     &                     'SISYPHE                 ',2)
!                                                     2 : HARDCODED
!                             OPTION FOR POSITIVE DEPTHS ALGORITHMS
!                             HERE CHOICE OF OPTION INDEPENDENT OF
!                             SEGMENT NUMBERING
!
      CALL OS('X=Y-Z   ' ,X=ZFCL,Y=HZ,Z=HZN)
!
!     DIRICHLET CONDITIONS (DONE BY POSITIVE_DEPTHS, SO WHAT ???)
!
!     DO K=1,NPTFR
!       IF(LIMTEC%I(K).EQ.KDIR) THEN
!         BOUNDARY CONDITIONS OF HZ MINUS HZN
!         HERE THIS IS MASS, DIVISION BY CSF_SABLE DONE LATER
!         ZFCL%R(MESH%NBOR%I(K))=T8%R(K)-HZN%R(MESH%NBOR%I(K))
!       ENDIF
!     ENDDO
!
!-----------------------------------------------------------------------
!
!     BILAN
!
!     WRITE(LU,*) 'BILAN DE LA CLASSE ',ICLA
!     MASINI=0.D0
!     MASFIN=0.D0
!     DO I=1,NPOIN
!       MASINI=MASINI+HZN%R(I)*VOLU2D%R(I)
!       MASFIN=MASFIN+HZ%R(I)*VOLU2D%R(I)
!     ENDDO
!     IF(NCSIZE.GT.1) THEN
!       MASINI=P_DSUM(MASINI)
!       MASFIN=P_DSUM(MASFIN)  
!     ENDIF
!     WRITE(LU,*) 'MASSE INITIALE : ',MASINI,' MASSE FINALE : ',MASFIN 
!     WRITE(LU,*) 'DIFFERENCE : ',MASFIN-MASINI
!     WRITE(LU,*) 'DIFFERENCE EN VOLUME : ',(MASFIN-MASINI)/CSF_SABLE
!     FLUXB=0.D0
!     DO K=1,NPTFR
!       FLUXB=FLUXB+FLBCLA%R(K)
!     ENDDO
!     WRITE(LU,*) 'FLUX EN MASSE : ',FLUXB
!     WRITE(LU,*) 'ERREUR DE MASSE : ',MASFIN-MASINI+FLUXB*DT
!
!-----------------------------------------------------------------------
!
      RETURN
      END
