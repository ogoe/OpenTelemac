!                    *****************************
                     SUBROUTINE BEDLOAD_SOLVS_FE !
!                    *****************************
!
     &(MESH,S,EBOR,MASKEL,MASK,
     & QSX,QSY,IELMT,NPOIN,NPTFR,KENT,KDIR,LIMTEC,DT,
     & MSK, ENTET,T1,T2,T3,T4,T8,
     & ZFCL,HZ,HZN,GLOSEG,DIMGLO,FLODEL,FLULIM,NSEG,UNSV2D)
!
!***********************************************************************
! SISYPHE   V6P0                                   21/08/2010
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DIMGLO         |---| 
!| DT             |---| 
!| EBOR           |---| 
!| ENTET          |---| 
!| FLODEL         |---| 
!| FLULIM         |---| 
!| GLOSEG         |---| 
!| HZ             |<--| NEW AVAILABLE LAYER OF SEDIMENT
!| HZN            |-->| OLD AVAILABLE LAYER OF SEDIMENT
!| IELMT          |---| 
!| KDIR           |---| 
!| KENT           |---| 
!| LIMTEC         |---| 
!| MASK           |---| 
!| MASKEL         |---| 
!| MESH           |---| 
!| MSK            |---| 
!| NPOIN          |---| 
!| NPTFR          |---| 
!| NSEG           |---| 
!| QSX            |---| 
!| QSY            |---| 
!| S              |---| 
!| T1             |---| 
!| T2             |---| 
!| T3             |---| 
!| T4             |---| 
!| T8             |---| 
!| UNSV2D         |---| 
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
      DOUBLE PRECISION, INTENT(IN)    :: DT
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
          EBOR%R(K)=EBOR%R(K)+HZN%R(MESH%NBOR%I(K))
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
          EBOR%R(K)=EBOR%R(K)-HZN%R(MESH%NBOR%I(K))
          ZFCL%R(MESH%NBOR%I(K)) = EBOR%R(K)
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END