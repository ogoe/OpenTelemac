!                    *****************
                     SUBROUTINE CHARAC
!                    *****************
!
     &( FN  , FTILD  , NOMB   , UCONV  , VCONV , WCONV  , ZSTAR ,
     &  DT  , IFAMAS , IELM   , NPOIN2 , NPLAN , NPLINT ,
     &  MSK , MASKEL , SHP,SHZ , TB    , IT1,IT2,IT3,ISUB,MESH ,
     &  NELEM2,NELMAX2,IKLE2,SURDET2   , INILOC)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    CALLS THE METHOD OF CHARACTERISTICS
!+               (SUBROUTINE CARACT).
!
!history  J-M HERVOUET (LNHE)
!+        12/02/2010
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
!history  J-M HERVOUET (LNHE)
!+        03/01/2012
!+        V6P2
!+   NPOIN instead of NPOIN2 in the call to SCARACT at the position
!+   of argument NPLOT (goes with corrections in Streamline.f)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME STEP
!| FN             |-->| VARIABLES AT TIME N .
!| FTILD          |<--| VARIABLES AFTER ADVECTION .
!| IELM           |-->| TYPE OF ELEMENT : 11 : TRIANGLE P1
!|                |   |                   41 : PRISM IN TELEMAC3D
!| IFAMAS         |-->| A MODIFIED IFABOR WHEN ELEMENTS ARE MASKED
!| IKLE2          |-->| CONNECTIVITY TABLE FOR TRIANGLES
!| IT1            |<->| INTEGER WORK ARRAY
!| IT2            |<->| INTEGER WORK ARRAY
!| IT3            |<->| INTEGER WORK ARRAY: NO LONGER USED !!!!!!!!!!!!
!| ISUB           |<--| IN PARALLEL, WILL BE THE SUB-DOMAIN OF THE FOOT
!|                |   | OF THE CHARACTERISTIC.
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D
!| NELMAX2        |-->| MAXIMUM NUMBER OF ELEMENTS IN 2D
!| NOMB           |-->| NUMBER OF VARIABLES TO BE ADVECTED
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPLINT         |---| NOT USED
!| NPOIN2         |-->| NUMBER OF POINTS IN THE 2D MESH
!| SHP            |<->| BARYCENTRIC COORDINATES OF POINTS IN TRIANGLES
!| SHZ            |<->| BARYCENTRIC COORDINATES ON VERTICAL
!| SURDET2        |-->| GEOMETRIC COEFFICIENT USED IN PARAMETRIC TRANSFORMATION
!| TB             |<->| BLOCK CONTAINING THE BIEF_OBJ WORK ARRAYS
!| UCONV          |-->| X-COMPONENT OF ADVECTION FIELD
!| VCONV          |-->| Y-COMPONENT OF ADVECTION FIELD
!| WCONV          |-->| Z-COMPONENT OF ADVECTION FIELD IN THE TRANSFORMED MESH
!| ZSTAR          |-->| TRANSFORMED VERTICAL COORDINATES IN 3D 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CHARAC => CHARAC
      USE STREAMLINE, ONLY : SCARACT      
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)         :: NOMB
      INTEGER         , INTENT(IN)         :: NPLAN,NPLINT,NELEM2
      INTEGER         , INTENT(IN)         :: NPOIN2,NELMAX2
      INTEGER         , INTENT(INOUT)      :: IELM
      INTEGER         , INTENT(INOUT)      :: IT1(*),IT2(*)
      INTEGER         , INTENT(INOUT)      :: IT3(*),ISUB(*)
      TYPE(BIEF_OBJ)  , INTENT(IN)         :: FN,UCONV,VCONV,WCONV
      TYPE(BIEF_OBJ)  , INTENT(IN)         :: ZSTAR,MASKEL,IKLE2,SURDET2
      TYPE(BIEF_OBJ)  , INTENT(INOUT)      :: FTILD,TB,SHP,SHZ
      LOGICAL         , INTENT(IN)         :: MSK
      DOUBLE PRECISION, INTENT(IN)         :: DT
      TYPE(BIEF_MESH) , INTENT(INOUT)      :: MESH
      TYPE(BIEF_OBJ)  , INTENT(IN), TARGET :: IFAMAS
      LOGICAL, OPTIONAL, INTENT(IN)        :: INILOC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NPOIN,IELMU,IELEM
      DOUBLE PRECISION TIERS     
!
!-----------------------------------------------------------------------
!
      TYPE(BIEF_OBJ), POINTER :: T1,T2,T3,T4,T5,T6,T7
      INTEGER, DIMENSION(:), POINTER :: IFA
      INTEGER I,J,K,NPT,DIM1F
      LOGICAL QUAD,QUAB
!
!-----------------------------------------------------------------------
!
      TIERS=1.D0/3.D0
! 
!-----------------------------------------------------------------------
!  TABLEAUX DE TRAVAIL PRIS DANS LE BLOC TB
!-----------------------------------------------------------------------
!
      T1 =>TB%ADR( 1)%P
      T2 =>TB%ADR( 2)%P
      T3 =>TB%ADR( 3)%P
      T4 =>TB%ADR( 4)%P
      T5 =>TB%ADR( 5)%P
      T6 =>TB%ADR( 6)%P
      T7 =>TB%ADR( 7)%P
!
!-----------------------------------------------------------------------
!  DEPLOIEMENT DE LA STRUCTURE DE MAILLAGE
!-----------------------------------------------------------------------
!
      NPOIN = MESH%NPOIN
      IELMU = UCONV%ELM
!
!-----------------------------------------------------------------------
!  ARE THERE QUADRATIC OR QUASI-BUBBLE VARIABLES ?
!  AND COMPUTATION OF LARGEST NUMBER OF POINTS
!-----------------------------------------------------------------------
!
      QUAD=.FALSE.
      QUAB=.FALSE.
      NPT=0
      IF(FN%TYPE.EQ.4) THEN    
        DO I=1,FN%N
          IF(FN%ADR(I)%P%ELM.EQ.12) QUAB = .TRUE.
          IF(FN%ADR(I)%P%ELM.EQ.13) QUAD = .TRUE.
          NPT=MAX(NPT,FN%ADR(I)%P%DIM1)
        ENDDO
      ELSEIF(FN%TYPE.EQ.2) THEN
        IF(FN%ELM.EQ.12) QUAB = .TRUE.
        IF(FN%ELM.EQ.13) QUAD = .TRUE.
        NPT=MAX(NPT,FN%DIM1)
      ENDIF
      IF(QUAB.AND.QUAD) THEN
        WRITE(LU,*) 'CHARAC: QUADRATIC AND QUASI-BUBBLE CANNOT BE MIXED'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!     CHECKING SHP SIZE (ONCE A BUG...)
!-----------------------------------------------------------------------
!
      IF(3*NPT.GT.SHP%MAXDIM1*SHP%MAXDIM2) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'TAILLE DE SHP:',SHP%MAXDIM1*SHP%MAXDIM2
          WRITE(LU,*) 'TROP PETITE DANS CHARAC, ',3*NPT
          WRITE(LU,*) 'REQUISE'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'SIZE OF SHP:',SHP%MAXDIM1*SHP%MAXDIM2
          WRITE(LU,*) 'TOO SMALL IN CHARAC, ',3*NPT
          WRITE(LU,*) 'REQUESTED'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF   
!
!-----------------------------------------------------------------------
!  APPEL DE CARACT
!-----------------------------------------------------------------------
!
      IF(MSK) THEN
!       APPEL AVEC IFAMAS
        IFA=>IFAMAS%I
      ELSE
!       APPEL AVEC IFABOR
        IFA=>MESH%IFABOR%I
      ENDIF
!
      CALL OS('X=Y     ',X=T1,Y=MESH%X)
      CALL OS('X=Y     ',X=T2,Y=MESH%Y)
!
!     IELM MUST BE INTENT(INOUT) BECAUSE IT IS SUCH IN CHGDIS
      IF(QUAD) THEN    
        CALL CHGDIS(T1,IELM,13,MESH)
        CALL CHGDIS(T2,IELM,13,MESH)
      ELSEIF(QUAB) THEN
        CALL CHGDIS(T1,IELM,12,MESH)
        CALL CHGDIS(T2,IELM,12,MESH)
      ENDIF 
!           
      IF(IELM.EQ.11) THEN
        CALL GTSH11(SHP%R,IT1,IKLE2%I,MESH%ELTCAR%I,NPOIN2,
     &              NELEM2,NELMAX2,MESH%NSEG,QUAB,QUAD)
        DIM1F=NPT
      ELSEIF(IELM.EQ.41) THEN
        DO I=1,NPLAN
          CALL OV('X=C     ',T3%R((I-1)*NPOIN2+1:I*NPOIN2),
     &            T3%R,T3%R,ZSTAR%R(I),NPOIN2)
        ENDDO    
        CALL GTSH41(SHP%R,SHZ%R,WCONV%R,IT1,IT2,IKLE2%I,MESH%ELTCAR%I,
     &              NPOIN2,NELMAX2,NPLAN,QUAB,QUAD)
        DIM1F=NPOIN2
      ELSE
        WRITE(LU,*) 'ELEMENT NOT IMPLEMENTED IN CHARAC: ',IELM
        CALL PLANTE(1)
        STOP  
      ENDIF        
!        
      CALL SCARACT(FN,FTILD,UCONV%R,VCONV%R,WCONV%R,
     &             MESH%X%R,MESH%Y%R,ZSTAR%R,
     &             T1%R,T2%R,T3%R,T4%R,T5%R,T6%R,
     &             MESH%Z%R,SHP%R,SHZ%R,
     &             SURDET2%R,DT,IKLE2%I,IFA,IT1,IT2,IT3,ISUB,
     &             IELM,IELMU,NELEM2,NELMAX2,NOMB,NPOIN,NPOIN2,
     &             3,NPLAN,MESH,NPT,DIM1F,-1)
! 
!     PARALLEL COMMUNICATION: THE VALUE WITH HIGHEST ABSOLUTE VALUE
!                             IS KEPT. WE ASSUME THAT INTERFACE POINTS
!                             NOT TREATED BY A SUB-DOMAIN HAVE RECEIVED
!                             A FTILD=0.D0, THIS IS THE CASE BECAUSE
!                             POINTS IPLOT WITH ELT(IPLOT)=0
!                             ARE RETURNED WITH ALL SHP=0.D0
!    
      IF(NCSIZE.GT.1.AND.NOMB.GT.0) THEN
        IF(FTILD%TYPE.EQ.2) THEN 
          CALL PARCOM(FTILD,1,MESH)               
        ELSEIF(FTILD%TYPE.EQ.4) THEN 
          DO I=1,NOMB          
            CALL PARCOM(FTILD%ADR(I)%P,1,MESH) 
          ENDDO 
        ENDIF 
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
