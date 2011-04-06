C
C pas de princi pour le calcul telemac
C mais c'est nécessaire pour la compilation du
C princi du calcul sisyphe
C
C ici test avec initialisation à + infini des tableaux.
C
C                       *****************
                        SUBROUTINE ALLVEC
C                       *****************
C
     *( NAT , VEC , NOM , IELM , DIM2 , STATUT )
C
C***********************************************************************
C BIEF VERSION 5.6            11/07/95    J-M HERVOUET (LNH) 30 87 80 18
C***********************************************************************
C
C  FONCTION  : ALLOCATION EN MEMOIRE D'UNE STRUCTURE DE VECTEUR
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |   NAT          |<-- | 1: VECTEUR REEL   2:VECTEUR ENTIER
C |   VEC          |<-- | VECTEUR A ALLOUER
C |   NOM          | -->| NOM FORTRAN DU TABLEAU
C |   IELM         | -->| TYPE D'ELEMENT DU VECTEUR, OU DIMENSION
C |                |    | (SUIVANT LE STATUT, VOIR PLUS BAS)
C |   DIM2         | -->| DEUXIEMME DIMENSION DU VECTEUR
C |   STATUT       | -->| STATUT DU VECTEUR :
C |                |    | 0 : VECTEUR LIBRE, IELM EST ALORS SA DIMENSION
C |                |    | 1 : VECTEUR DEFINI SUR LE MAILLAGE
C |                |    |     IELM EST ALORS LE TYPE D'ELEMENT
C |                |    |     CHANGEMENT DE DISCRETISATION INTERDIT
C |                |    | 2 : COMME 1 MAIS CHANGEMENTS AUTORISES
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C
C PROGRAMMES APPELES : RIEN EN STANDARD
C
C***********************************************************************
C
      USE BIEF, EX_ALLVEC => ALLVEC
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: VEC
      INTEGER         , INTENT(IN)    :: NAT,IELM,DIM2,STATUT
      CHARACTER(LEN=6), INTENT(IN)    :: NOM
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ERR
      INTRINSIC MAX
C
      INTEGER IMAX,I
      DOUBLE PRECISION XMAX
C
C-----------------------------------------------------------------------
C  HEADER COMMON TO ALL OBJECTS
C-----------------------------------------------------------------------
C
C     TO CHECK MEMORY CRASHES
C
      VEC%KEY = 123456
C
C     TYPE OF OBJECT (HERE VECTOR)
C
      VEC%TYPE = 2
C
C     NAME OF OBJECT
C
      VEC%NAME = NOM
C
C-----------------------------------------------------------------------
C  PART SPECIFIC TO VECTORS
C-----------------------------------------------------------------------
C
C     NATURE
C
      VEC%NAT = NAT
C
C     MAXIMUM SIZE PER DIMENSION
C
      IF(STATUT.EQ.1.OR.STATUT.EQ.2) THEN
        VEC%MAXDIM1 = NBMPTS(IELM)
      ELSE
        VEC%MAXDIM1 = IELM
      ENDIF
C
C     VEC%MAXDIM1 MUST BE AT LEAST 1
C     TO AVOID BOUND CHECKING ERRORS ON SOME COMPILERS
C
      VEC%MAXDIM1=MAX(VEC%MAXDIM1,1)
C
C     DISCRETISATION
C
      IF(STATUT.EQ.1.OR.STATUT.EQ.2) THEN
        VEC%ELM = IELM
      ELSE
        VEC%ELM = -1000
      ENDIF
C
C     FIRST DIMENSION OF VECTOR
C
      IF(STATUT.EQ.1.OR.STATUT.EQ.2) THEN
        VEC%DIM1 = NBPTS(IELM)
      ELSE
        VEC%DIM1 = IELM
      ENDIF
C
C     SECOND DIMENSION OF VECTOR (VEC%DIM2 MAY BE CHANGED)
C
      VEC%DIM2    = DIM2
      VEC%MAXDIM2 = DIM2
C
C     CASE OF DISCONTINUITY BETWEEN ELEMENTS
C     (SEE CORRSL, VC13AA, VC13BB)
C
      VEC%DIMDISC = 0
C
C     STATUS
C
      VEC%STATUS = STATUT
C
C     INFORMATION ON CONTENT
C
      VEC%TYPR = '?'
      VEC%TYPI = '?'
C
C     DYNAMIC ALLOCATION OF MEMORY (REAL OR INTEGER, DEPENDING OF NAT)
C
      IF(NAT.EQ.1) THEN
C
        ALLOCATE(VEC%R(VEC%MAXDIM1*VEC%DIM2),STAT=ERR)
C       jaj nullify the integer part 
        NULLIFY(VEC%I)
C
C       FILLING ARRAY WITH BIG NUMBERS
C       TO RAISE QUESTIONS IF NOT INITIALISED
C
        XMAX = HUGE(100.D0)
        CALL OV('X=C     ',VEC%R,VEC%R,VEC%R,XMAX,
     *          VEC%MAXDIM1*VEC%DIM2)
C
      ELSEIF(NAT.EQ.2) THEN
C
        ALLOCATE(VEC%I(VEC%MAXDIM1*VEC%DIM2),STAT=ERR)  
C       jaj nullify the real part
        NULLIFY(VEC%R)
C
C       FILLING ARRAY WITH BIG NUMBERS
C       TO RAISE QUESTIONS IF NOT INITIALISED
C
        IMAX = HUGE(100)
          DO I=1,VEC%MAXDIM1*VEC%DIM2
            VEC%I(I) = IMAX
          END DO
C
      ELSE
        STOP 'UNKNOWN NAT IN ALLVEC'
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(ERR.EQ.0) THEN
C       IF(LNG.EQ.1) WRITE(LU,*) 'VECTEUR : ',NOM,' ALLOUE'
C       IF(LNG.EQ.2) WRITE(LU,*) 'VECTOR: ',NOM,' ALLOCATED'
      ELSE
        IF(LNG.EQ.1) WRITE(LU,10) NOM,ERR
        IF(LNG.EQ.2) WRITE(LU,20) NOM,ERR
10      FORMAT(1X,'ERREUR A L''ALLOCATION DU VECTEUR : ',A6,/,1X,
     *            'CODE D''ERREUR : ',1I6)
20      FORMAT(1X,'ERROR DURING ALLOCATION OF VECTOR: ',A6,/,1X,
     *            'ERROR CODE: ',1I6)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END

