!                    *****************
                     SUBROUTINE CHARAC
!                    *****************
!
     &( FN  , FTILD  , NOMB   , UCONV  , VCONV , WCONV  , ZSTAR ,
     &  DT  , IFAMAS , IELM   , NPOIN2 , NPLAN , NPLINT ,
     &  MSK , MASKEL , SHP,SHZ , TB    , IT1,IT2,IT3,IT4,MESH ,
     &  NELEM2,NELMAX2,IKLE2,SURDET2   , INILOC)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| PAS DE TEMPS
!| FN             |-->| VARIABLES A L'ETAPE N .
!| FTILD          |<--| VARIABLES APRES LA CONVECTION .
!| IELM           |-->| TYPE D'ELEMENT : 11 : TRIANGLE P1
!|                |   | 41 : PRISME DE TEL3D
!| IFAMAS         |-->| IFABOR MODIFIE QUAND DES ELEMENTS SONT MASQUES
!| IKLE2          |---|
!| INILOC         |---|
!| IT1            |---|
!| IT2            |---|
!| IT3            |---|
!| IT4            |---|
!| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
!|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE.
!| MESH           |-->| BLOC DES ENTIERS DU MAILLAGE
!| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
!| NELEM2         |---|
!| NELMAX2        |---|
!| NOMB           |-->| NOMBRE DE VARIABLES A CONVECTER.
!| NPLAN          |-->| NOMBRE DE PLAN SUIVANT Z (POUR TEL3D).
!| NPLINT         |-->| PLAN DE REFERENCE INTERMEDIAIRE (POUR TEL3D).
!| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D (POUR TEL3D).
!| SHP            |---|
!| SHZ            |---|
!| SURDET2        |---|
!| TB             |-->| BLOC DE TABLEAUX DE TRAVAIL (AU MOINS 8)
!| UCONV,VCONV    |-->| COMPOSANTES DES VITESSES DU CONVECTEUR.
!| WCONV          |---|
!| ZSTAR          |-->| COORDONNEES VERTICALES EN 3D.
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
      INTEGER         , INTENT(IN)         :: NPOIN2,IELM,NELMAX2
      INTEGER         , INTENT(INOUT)      :: IT1(*),IT2(*)
      INTEGER         , INTENT(INOUT)      :: IT3(*),IT4(*)
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
      INTEGER NPOIN,IELMU
      LOGICAL INITLOC
!
!-----------------------------------------------------------------------
!
      TYPE(BIEF_OBJ), POINTER :: T1,T2,T3,T4,T5,T6,T7
      INTEGER, DIMENSION(:), POINTER :: IFA
      INTEGER I,J,K
!
!-----------------------------------------------------------------------
!  WORKING ARRAYS FROM BLOCK TB
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
!  INITIALISES THE LOCATION OF POINTS, OR NOT
!-----------------------------------------------------------------------
!
      IF(PRESENT(INILOC)) THEN
        INITLOC=INILOC
      ELSE
        INITLOC=.TRUE.
      ENDIF
!
!-----------------------------------------------------------------------
!  DEPLOYS THE MESH STRUCTURE
!-----------------------------------------------------------------------
!
      NPOIN = MESH%NPOIN
      IELMU = UCONV%ELM
!
!-----------------------------------------------------------------------
!     CHECKS SHP SIZE (ONCE A BUG...)
!-----------------------------------------------------------------------
!
      IF(3*NPOIN.GT.SHP%MAXDIM1*SHP%MAXDIM2) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'TAILLE DE SHP:',SHP%MAXDIM1*SHP%MAXDIM2
          WRITE(LU,*) 'TROP PETITE DANS CHARAC, ',3*NPOIN
          WRITE(LU,*) 'REQUISE'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'SIZE OF SHP:',SHP%MAXDIM1*SHP%MAXDIM2
          WRITE(LU,*) 'TOO SMALL IN CHARAC, ',3*NPOIN
          WRITE(LU,*) 'REQUESTED'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!  CALLS CARACT
!-----------------------------------------------------------------------
!
      IF(MSK) THEN
!       CALL WITH IFAMAS
        IFA=>IFAMAS%I
      ELSE
!       CALL WITH IFABOR
        IFA=>MESH%IFABOR%I
      ENDIF
!
      CALL CPSTVC(MESH%X,T1)
      CALL CPSTVC(MESH%Y,T2)
!
      IF(NCSIZE.EQ.0) THEN
!
        CALL CARACT( FN , FTILD , UCONV%R , VCONV%R , WCONV%R ,
     &               MESH%X%R,MESH%Y%R,ZSTAR%R,
     &               T1,T2,T3%R,T4%R,T5%R,T6%R,
     &               MESH%Z%R,SHP%R,SHZ%R,
     &               SURDET2%R,DT,IKLE2%I,IFA,
     &               IT1,IT2,IT3,IT4,
     &               IELM,IELMU,NELEM2,NELMAX2,NOMB,NPOIN,NPOIN2,
     &               3,NPLAN,MESH%LV,
     &               MSK,MASKEL%R,MESH,MESH%FAC%R,T7%R,T7,INITLOC)
!
      ELSEIF(NCSIZE.GE.1) THEN
!
        CALL SCARACT( FN , FTILD , UCONV%R , VCONV%R , WCONV%R ,
     &                MESH%X%R,MESH%Y%R,ZSTAR%R,
     &                T1,T2,T3%R,T4%R,T5%R,T6%R,
     &                MESH%Z%R,SHP%R,SHZ%R,
     &                SURDET2%R,DT,IKLE2%I,IFA,
     &                IT1,IT2,IT3,IT4,
     &                IELM,IELMU,NELEM2,NELMAX2,NOMB,NPOIN,NPOIN2,
     &                3,NPLAN,MESH%LV,MSK,MASKEL%R,
     &                MESH,MESH%FAC%R,T7%R,T7,INITLOC)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END