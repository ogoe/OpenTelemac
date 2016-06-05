
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!Make sure your T2DCAS-File includes the following lines:
!!!!!!!!!!!!INITIAL GUESS FOR H =  0
!!!!!!!!!!!!INITIAL GUESS FOR U =  0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

C     ********************************************
      MODULE AD_TELEMAC2D_CHECKPOINT
C     ********************************************
C
C
      USE BIEF_DEF
      USE FRICTION_DEF

      IMPLICIT NONE

      PRIVATE

      PUBLIC :: CHECKPOINT_T2D_INIT
      PUBLIC :: CHECKPOINT_T2D_STORE,   CHECKPOINT_T2D_RESTORE
      PUBLIC :: CHECKPOINT_T2D_COPY_TO_TMP_CP
      PUBLIC :: CHECKPOINT_T2D_COPY_FROM_TMP_CP
      PUBLIC :: CHECKPOINT_T2D_COMPARE
      PUBLIC :: T2D_ADJ_BUFFER,         T2D_TMP_CP

      TYPE CHECKPOINT_T2D_TYPE
C
C     COMPONENTS OF VELOCITY
        TYPE(BIEF_OBJ)        :: U
        TYPE(BIEF_OBJ)        :: V
C
C     DEPTH AT NEW TIME-STEP
        TYPE(BIEF_OBJ)        :: H
C     SURFACE
!!        TYPE(BIEF_OBJ)        :: S
C
C     K AND EPSILON AT NEW TIME-STEP
!!        TYPE(BIEF_OBJ)        :: AK,EP
C

!     U AND V AT OLD TIME-STEP
!!        TYPE(BIEF_OBJ)        :: UN,VN
!     H AT OLD TIME-STEP
!!        TYPE(BIEF_OBJ)        :: HN
!     AK AT OLD TIME-STEP
!!        TYPE(BIEF_OBJ)        :: AKN
!     EP AT OLD TIME-STEP
!!        TYPE(BIEF_OBJ)        :: EPN

!     BOTTOM TOPOGRAPHY, PER POINT
        TYPE(BIEF_OBJ)        :: ZF
!     FRICTION COEFFICIENT, IN TERMS OF STRICKLER, CHEZY, ETC
        TYPE(BIEF_OBJ)        :: CHESTR
!!        TYPE(BIEF_OBJ)        :: CHBORD

!     VALUES ON BOUNDARIES: U
!!        TYPE(BIEF_OBJ)        :: UBOR
!     VALUES ON BOUNDARIES: V
!!        TYPE(BIEF_OBJ)        :: VBOR
!     VALUES ON BOUNDARIES: H
!!        TYPE(BIEF_OBJ)        :: HBOR

!       Simulation time
        DOUBLE PRECISION  :: AT_T2D

      END TYPE CHECKPOINT_T2D_TYPE

      TYPE(CHECKPOINT_T2D_TYPE), DIMENSION(:),
     $                 ALLOCATABLE,    TARGET        :: CP_T2D


      TYPE(CHECKPOINT_T2D_TYPE), POINTER             :: T2D_ADJ_BUFFER
      TYPE(CHECKPOINT_T2D_TYPE), POINTER             :: T2D_TMP_CP


      INTEGER                                        :: NumCP_T2D = -1


      SAVE
C

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


C     *****************
      SUBROUTINE CHECKPOINT_T2D_INIT
C     *****************
     &     ( NumCP )
C
C***********************************************************************
C     TELEMAC 2D VERSION 6.2          07/05/2013    J.Riehme STCE, RWTH Aachen
C     TELEMAC 2D VERSION 6.2          01/04/2012    U.Merkel
C***********************************************************************
C
C
C     FUNCTION  : Allocates / Inits all Checkpointing Parameters / Struktures
C     Use this only ones outside of the telemac2d.f time loop
C
C-----------------------------------------------------------------------
C     ARGUMENTS USED
C     .________________.____.______________________________________________
C     |      NOM       |MODE|                   ROLE
C     |________________|____|_______________________________________________
C     |   NumCP        | -->| NUMBER OF CHECKPOINTS TO ALLOCATE
C     |________________|____|______________________________________________
C     MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------

      USE BIEF
      USE DECLARATIONS_TELEMAC2D

      INTEGER, INTENT(IN)       :: NumCP

      INTEGER K, IELBU, IELBH, I

      INTEGER LNG,LU
      COMMON/INFO/LNG,LU

      print *,'CHECKPOINT_T2D_INIT : NUMCP ', NumCP

      IF ( NumCP_T2D .NE. -1 )  THEN
         IF(LNG.EQ.1) WRITE(LU,*)
     $        'CHECKPOINT_T2D_INIT :: CHECKPOINTS ALREADY INITIALISED'
         IF(LNG.EQ.2) WRITE(LU,*)
     $        'CHECKPOINT_T2D_INIT :: CHECKPOINTS ALREADY INITIALISED'
         CALL PLANTE(1)
         STOP
      ENDIF


      NumCP_T2D = NumCP

      ALLOCATE( CP_T2D(-2:NumCP_T2D) )

      Do K = -2, NumCP_T2D
C
C     COMPONENTS OF VELOCITY
         CALL BIEF_ALLVEC(1,CP_T2D(K)%U,'U_CP  ',IELMU,1,1,MESH)
         CALL BIEF_ALLVEC(1,CP_T2D(K)%V,'V_CP  ',IELMU,1,1,MESH)
C
C     DEPTH AT NEW TIME-STEP
         CALL BIEF_ALLVEC(1,CP_T2D(K)%H,'H_CP  ',IELMH,1,1,MESH)
!!         CALL BIEF_ALLVEC(1,CP_T2D(K)%S,'S_CP  ',0,1,1,MESH)
!
C     K AND EPSILON AT NEW TIME-STEP,
!     AK AT OLD TIME-STEP, EP AT OLD TIME-STEP
!!         if(ITURB.EQ.3) then
!!            CALL BIEF_ALLVEC(1,CP_T2D(K)%AK,'AK_CP ',IELMK,1,1,MESH)
!!            CALL BIEF_ALLVEC(1,CP_T2D(K)%EP,'EP_CP ',IELME,1,1,MESH)
!!            CALL BIEF_ALLVEC(1,CP_T2D(K)%AKN,'AKN_CP',IELMK,1,1,MESH)
!!            CALL BIEF_ALLVEC(1,CP_T2D(K)%EPN,'EPN_CP',IELME,1,1,MESH)
!!         else
!!            CALL BIEF_ALLVEC(1,CP_T2D(K)%AK,'AK_CP ',0,1,0,MESH)
!!            CALL BIEF_ALLVEC(1,CP_T2D(K)%EP,'EP_CP ',0,1,0,MESH)
!!            CALL BIEF_ALLVEC(1,CP_T2D(K)%AKN,'AKN_CP',0,1,0,MESH)
!!            CALL BIEF_ALLVEC(1,CP_T2D(K)%EPN,'EPN_CP',0,1,0,MESH)
!!         endif
!!

!     U AND V AT OLD TIME-STEP
!!         CALL BIEF_ALLVEC(1,CP_T2D(K)%UN,'UN_CP ',IELMU,1,1,MESH)
!!         CALL BIEF_ALLVEC(1,CP_T2D(K)%VN,'VN_CP ',IELMU,1,1,MESH)
!     H AT OLD TIME-STEP
!!         CALL BIEF_ALLVEC(1,CP_T2D(K)%HN,'HN_CP ',IELMH,1,1,MESH)
!CALL BIEF_ALLVEC(1,CP_T2D(K)%SN,'SN_CP ',IELMH,1,1,MESH)


!     BOTTOM TOPOGRAPHY, PER POINT
         CALL BIEF_ALLVEC(1,CP_T2D(K)%ZF,'ZF_CP ',IELMH,1,1,MESH)
!     FRICTION COEFFICIENT, IN TERMS OF STRICKLER, CHEZY, ETC
         CALL BIEF_ALLVEC(1,CP_T2D(K)%CHESTR,'CHE_CP',IELMU,1,1,MESH)
!!         CALL BIEF_ALLVEC(1,CP_T2D(K)%CHBORD,'CHE_CB',IELMU,1,1,MESH)


!     BOUNDARY CONDITIONS
!!         IELBU = IELBOR(IELMU,1)
!!         IELBH = IELBOR(IELMH,1)
!     VALUES ON BOUNDARIES: U
!!         CALL BIEF_ALLVEC(1,CP_T2D(K)%UBOR ,'UBORCB',IELBU,2,1,MESH)
!     VALUES ON BOUNDARIES: V
!!         CALL BIEF_ALLVEC(1,CP_T2D(K)%VBOR ,'VBORCB',IELBU,2,1,MESH)
!     VALUES ON BOUNDARIES: H
!!         CALL BIEF_ALLVEC(1,CP_T2D(K)%HBOR ,'HBORCB',IELBH,1,1,MESH)

C
      ENDDO

C     Set pointer for adjoint buffer
      T2D_ADJ_BUFFER => CP_T2D(-1)
      T2D_TMP_CP     => CP_T2D(-2)

      RETURN
      END SUBROUTINE CHECKPOINT_T2D_INIT



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



C     *****************
      SUBROUTINE CHECKPOINT_T2D_STORE
C     *****************
     &     (CP_ID)
C
C***********************************************************************
C     TELEMAC 2D VERSION 6.2          07/05/2013    J.Riehme STCE, RWTH Aachen
C     TELEMAC 2D VERSION 6.2          01/04/2012    U.Merkel
C***********************************************************************
C
C
C     FUNCTION  : Takes a CHECKPOINT of the main HYDRAULIC PARAMETERS
C
C
C-----------------------------------------------------------------------
C     ARGUMENTS USED
C     .________________.____.______________________________________________
C     |      NOM       |MODE|                   ROLE
C     |________________|____|_______________________________________________
C     |   CP_ID        | -->| CHECKPOINT SLOT TO OVERWRITE.
C     |________________|____|______________________________________________
C     MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------

      USE BIEF
      USE DECLARATIONS_TELEMAC2D

      INTEGER,          INTENT(IN) :: CP_ID

      INTEGER K, N
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU

      PRINT *,'CHECKPOINT_T2D_STORE: LT, CP_ID ',LT, CP_ID

      IF ( NumCP_T2D .EQ. -1 )  THEN
         IF(LNG.EQ.1) WRITE(LU,*)  'CHECKPOINT_T2D_STORE ::',
     $        ' CHECKPOINT SYSTEM NOT INTIALISED'
         IF(LNG.EQ.2) WRITE(LU,*)  'CHECKPOINT_T2D_STORE ::',
     $        ' CHECKPOINT SYSTEM NOT INTIALISED'
         CALL PLANTE(1)
         STOP
      ENDIF

      IF ( CP_ID < 0 .OR. CP_ID .GT. NumCP_T2D )  THEN
         IF(LNG.EQ.1) WRITE(LU,*)
     $        'CHECKPOINT_T2D_STORE :: WRONG CHECKPOINT ID ', CP_ID
         IF(LNG.EQ.2) WRITE(LU,*)
     $        'CHECKPOINT_T2D_STORE :: WRONG CHECKPOINT ID ', CP_ID
         CALL PLANTE(1)
         STOP
      ENDIF
C
C     COMPONENTS OF VELOCITY AT NEW TIME-STEP
      CALL OS('X=Y     ',X=CP_T2D(CP_ID)%U ,Y=U)
      CALL OS('X=Y     ',X=CP_T2D(CP_ID)%V ,Y=V)
C
C     DEPTH AT NEW TIME-STEP
      CALL OS('X=Y     ',X=CP_T2D(CP_ID)%H ,Y=H)
!!     CALL OS('X=Y     ',X=CP_T2D(CP_ID)%S ,Y=S)
C
C     K AND EPSILON AT NEW TIME-STEP
!!      CALL OS('X=Y     ',X=CP_T2D(CP_ID)%AK ,Y=AK)
!!      CALL OS('X=Y     ',X=CP_T2D(CP_ID)%EP ,Y=EP)

C     COMPONENTS OF VELOCITY OLD TIME-STEP
!!      CALL OS('X=Y     ',X=CP_T2D(CP_ID)%UN ,Y=UN)
!!      CALL OS('X=Y     ',X=CP_T2D(CP_ID)%VN ,Y=VN)
C
C     DEPTH AT OLD TIME-STEP
!!      CALL OS('X=Y     ',X=CP_T2D(CP_ID)%HN ,Y=HN)
!CALL OS('X=Y     ',X=CP_T2D(CP_ID)%SN ,Y=SN)
C
C     K AND EPSILON AT OLD TIME-STEP
!!      CALL OS('X=Y     ',X=CP_T2D(CP_ID)%AKN ,Y=AKN)
!!!      CALL OS('X=Y     ',X=CP_T2D(CP_ID)%EPN ,Y=EPN)

!     BOTTOM TOPOGRAPHY, PER POINT
      CALL OS('X=Y     ',X=CP_T2D(CP_ID)%ZF ,Y=ZF)
!     FRICTION COEFFICIENT, IN TERMS OF STRICKLER, CHEZY, ETC
      CALL OS('X=Y     ',X=CP_T2D(CP_ID)%CHESTR ,Y=CHESTR)
!!      CALL OS('X=Y     ',X=CP_T2D(CP_ID)%CHBORD ,Y=CHBORD)

C     BOUNDS
!!      CALL OS('X=Y     ',X=CP_T2D(CP_ID)%UBOR ,Y=UBOR)
!!      CALL OS('X=Y     ',X=CP_T2D(CP_ID)%VBOR ,Y=VBOR)
!!      CALL OS('X=Y     ',X=CP_T2D(CP_ID)%HBOR ,Y=HBOR)

      CP_T2D(CP_ID)%AT_T2D = AT

      PRINT *,'CHECKPOINT_T2D_STORE DONE: LT, CP_ID ',LT, CP_ID

      RETURN
      END SUBROUTINE CHECKPOINT_T2D_STORE



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

C     *****************
      SUBROUTINE CHECKPOINT_T2D_RESTORE
C     *****************
     &     (CP_ID)
C
C***********************************************************************
C     TELEMAC 2D VERSION 6.2          07/05/2013    J.Riehme STCE, RWTH Aachen
C     TELEMAC 2D VERSION 6.2          01/04/2012    U.Merkel
C***********************************************************************
C
C
C     FUNCTION  : Restores a CHECKPOINT of the main HYDRAULIC PARAMETERS
C
C
C-----------------------------------------------------------------------
C     ARGUMENTS USED
C     .________________.____.______________________________________________
C     |      NOM       |MODE|                   ROLE
C     |________________|____|_______________________________________________
C     |   CP_ID        | -->| CHECKPOINT SLOT TO READ
C     |________________|____|______________________________________________
C     MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C
C
C***********************************************************************
C
      USE BIEF
      USE DECLARATIONS_TELEMAC2D

      implicit none

      INTEGER,          INTENT(IN) :: CP_ID

      INTEGER LNG,LU
      COMMON/INFO/LNG,LU


      PRINT *,'CHECKPOINT_T2D_RESTORE: LT, CP_ID ',LT, CP_ID

      IF ( NumCP_T2D .EQ. -1 )  THEN
         IF(LNG.EQ.1) WRITE(LU,*)  'CHECKPOINT_T2D_RESTORE ::',
     $        ' CHECKPOINT SYSTEM NOT INTIALISED'
         IF(LNG.EQ.2) WRITE(LU,*)  'CHECKPOINT_T2D_RESTORE ::',
     $        ' CHECKPOINT SYSTEM NOT INTIALISED'
         CALL PLANTE(1)
         STOP
      ENDIF

      IF ( CP_ID < 0 .OR. CP_ID .GT. NumCP_T2D )  THEN
         IF(LNG.EQ.1) WRITE(LU,*)
     $        'CHECKPOINT_T2D_RESTORE :: WRONG CHECKPOINT ID ', CP_ID
         IF(LNG.EQ.2) WRITE(LU,*)
     $        'CHECKPOINT_T2D_RESTORE :: WRONG CHECKPOINT ID ', CP_ID
         CALL PLANTE(1)
         STOP
      ENDIF
C
C     COMPONENTS OF VELOCITY
      CALL OS('X=Y     ',X=U ,Y=CP_T2D(CP_ID)%U)
      CALL OS('X=Y     ',X=V ,Y=CP_T2D(CP_ID)%V)
C
C     DEPTH AT NEW TIME-STEP
      CALL OS('X=Y     ',X=H ,Y=CP_T2D(CP_ID)%H)
!CALL OS('X=Y     ',X=S ,Y=CP_T2D(CP_ID)%S)
C
C     K AND EPSILON AT NEW TIME-STEP
!CALL OS('X=Y     ',X=AK ,Y=CP_T2D(CP_ID)%AK)
!CALL OS('X=Y     ',X=EP ,Y=CP_T2D(CP_ID)%EP)

C     COMPONENTS OF VELOCITY OLD TIME-STEP
!CALL OS('X=Y     ',X=UN ,Y=CP_T2D(CP_ID)%UN)
!CALL OS('X=Y     ',X=VN ,Y=CP_T2D(CP_ID)%VN)
C
C     DEPTH AT OLD TIME-STEP
!CALL OS('X=Y     ',X=HN ,Y=CP_T2D(CP_ID)%HN)
C
C     K AND EPSILON AT OLD TIME-STEP
!CALL OS('X=Y     ',X=AKN ,Y=CP_T2D(CP_ID)%AKN)
!CALL OS('X=Y     ',X=EPN ,Y=CP_T2D(CP_ID)%EPN)

!     BOTTOM TOPOGRAPHY, PER POINT
      CALL OS('X=Y     ',X=ZF ,Y=CP_T2D(CP_ID)%ZF)
!     FRICTION COEFFICIENT, IN TERMS OF STRICKLER, CHEZY, ETC
      CALL OS('X=Y     ',X=CHESTR ,Y=CP_T2D(CP_ID)%CHESTR)
!!      CALL OS('X=Y     ',X=CHBORD ,Y=CP_T2D(CP_ID)%CHBORD)


!     BOUNDS
!CALL OS('X=Y     ' ,X=UBOR ,Y=CP_T2D(CP_ID)%UBOR)
!CALL OS('X=Y     ' ,X=VBOR ,Y=CP_T2D(CP_ID)%VBOR)
!CALL OS('X=Y     ' ,X=HBOR ,Y=CP_T2D(CP_ID)%HBOR)


      AT = CP_T2D(CP_ID)%AT_T2D

      PRINT *,'CHECKPOINT_T2D_RESTORE DONE: LT, CP_ID ',LT, CP_ID

      RETURN
      END  SUBROUTINE CHECKPOINT_T2D_RESTORE


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

C     *****************
      SUBROUTINE CHECKPOINT_T2D_COPY_TO_TMP_CP
C     *****************
     &     ( )
C
C***********************************************************************
C     TELEMAC 2D VERSION 6.2          07/05/2013    J.Riehme STCE, RWTH Aachen
C     TELEMAC 2D VERSION 6.2          01/04/2012    U.Merkel
C***********************************************************************
C
C
C     FUNCTION  : Copies variables to special checkpoint
C
C
C-----------------------------------------------------------------------
C     ARGUMENTS USED
C     .________________.____.______________________________________________
C     |      NOM       |MODE|                   ROLE
C     |________________|____|_______________________________________________
C     |________________|____|______________________________________________
C     MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------

      USE BIEF
      USE DECLARATIONS_TELEMAC2D

      INTEGER K, N
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU

      PRINT *,'CHECKPOINT_T2D_COPY_TO_TMP_CP ', LT

      IF ( NumCP_T2D .EQ. -1 )  THEN
         IF(LNG.EQ.1) WRITE(LU,*)  'CHECKPOINT_T2D_COPY_TO_TMP_CP ::',
     $        ' CHECKPOINT SYSTEM NOT INTIALISED'
         IF(LNG.EQ.2) WRITE(LU,*)  'CHECKPOINT_T2D_COPY_TO_TMP_CP ::',
     $        ' CHECKPOINT SYSTEM NOT INTIALISED'
         CALL PLANTE(1)
         STOP
      ENDIF
C
C     COMPONENTS OF VELOCITY AT NEW TIME-STEP
      CALL OS('X=Y     ',X=CP_T2D(-2)%U ,Y=U)
      CALL OS('X=Y     ',X=CP_T2D(-2)%V ,Y=V)
C
C     DEPTH AT NEW TIME-STEP
      CALL OS('X=Y     ',X=CP_T2D(-2)%H ,Y=H)
!!     CALL OS('X=Y     ',X=CP_T2D(-2)%S ,Y=S)
C
C     K AND EPSILON AT NEW TIME-STEP
!!      CALL OS('X=Y     ',X=CP_T2D(-2)%AK ,Y=AK)
!!      CALL OS('X=Y     ',X=CP_T2D(-2)%EP ,Y=EP)

C     COMPONENTS OF VELOCITY OLD TIME-STEP
!!      CALL OS('X=Y     ',X=CP_T2D(-2)%UN ,Y=UN)
!!      CALL OS('X=Y     ',X=CP_T2D(-2)%VN ,Y=VN)
C
C     DEPTH AT OLD TIME-STEP
!!      CALL OS('X=Y     ',X=CP_T2D(-2)%HN ,Y=HN)
!CALL OS('X=Y     ',X=CP_T2D(-2)%SN ,Y=SN)
C
C     K AND EPSILON AT OLD TIME-STEP
!!      CALL OS('X=Y     ',X=CP_T2D(-2)%AKN ,Y=AKN)
!!!      CALL OS('X=Y     ',X=CP_T2D(-2)%EPN ,Y=EPN)

!     BOTTOM TOPOGRAPHY, PER POINT
      CALL OS('X=Y     ',X=CP_T2D(-2)%ZF ,Y=ZF)
!     FRICTION COEFFICIENT, IN TERMS OF STRICKLER, CHEZY, ETC
      CALL OS('X=Y     ',X=CP_T2D(-2)%CHESTR ,Y=CHESTR)
!!      CALL OS('X=Y     ',X=CP_T2D(-2)%CHBORD ,Y=CHBORD)

C     BOUNDS
!!      CALL OS('X=Y     ',X=CP_T2D(-2)%UBOR ,Y=UBOR)
!!      CALL OS('X=Y     ',X=CP_T2D(-2)%VBOR ,Y=VBOR)
!!      CALL OS('X=Y     ',X=CP_T2D(-2)%HBOR ,Y=HBOR)

      CP_T2D(-2)%AT_T2D = AT

      PRINT *,'CHECKPOINT_T2D_COPY_TO_TMP_CP DONE: LT ',LT

      RETURN
      END SUBROUTINE CHECKPOINT_T2D_COPY_TO_TMP_CP


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


C     *****************
      SUBROUTINE CHECKPOINT_T2D_COPY_FROM_TMP_CP
C     *****************
     &     ( fts )
C
C***********************************************************************
C     TELEMAC 2D VERSION 6.2          01/04/2012    U.Merkel
C***********************************************************************
C
C
C     FUNCTION  : Copies variables to special checkpoint
C
C
C-----------------------------------------------------------------------
C     ARGUMENTS USED
C     .________________.____.______________________________________________
C     |      NOM       |MODE|                   ROLE
C     |________________|____|_______________________________________________
C     |________________|____|______________________________________________
C     MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------

      USE BIEF
      USE DECLARATIONS_TELEMAC2D

      LOGICAL, INTENT(IN)    :: fts  !  First Time Step
      INTEGER K, N
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU

      PRINT *,'CHECKPOINT_T2D_COPY_FROM_TMP_CP  ', LT

      IF ( NumCP_T2D .EQ. -1 )  THEN
         IF(LNG.EQ.1) WRITE(LU,*)  'CHECKPOINT_T2D_COPY_FROM_TMP_CP ::',
     $        ' CHECKPOINT SYSTEM NOT INTIALISED'
         IF(LNG.EQ.2) WRITE(LU,*)  'CHECKPOINT_T2D_COPY_FROM_TMP_CP ::',
     $        ' CHECKPOINT SYSTEM NOT INTIALISED'
         CALL PLANTE(1)
         STOP
      ENDIF
C
C
C     COMPONENTS OF VELOCITY
      CALL OS('X=Y     ',X=U ,Y=CP_T2D(-2)%U)
      CALL OS('X=Y     ',X=V ,Y=CP_T2D(-2)%V)
C
C     DEPTH AT NEW TIME-STEP
      CALL OS('X=Y     ',X=H ,Y=CP_T2D(-2)%H)
!CALL OS('X=Y     ',X=S ,Y=CP_T2D(-2)%S)
C
C     K AND EPSILON AT NEW TIME-STEP
!CALL OS('X=Y     ',X=AK ,Y=CP_T2D(-2)%AK)
!CALL OS('X=Y     ',X=EP ,Y=CP_T2D(-2)%EP)

C     COMPONENTS OF VELOCITY OLD TIME-STEP
!CALL OS('X=Y     ',X=UN ,Y=CP_T2D(-2)%UN)
!CALL OS('X=Y     ',X=VN ,Y=CP_T2D(-2)%VN)
C
C     DEPTH AT OLD TIME-STEP
!CALL OS('X=Y     ',X=HN ,Y=CP_T2D(-2)%HN)
C
C     K AND EPSILON AT OLD TIME-STEP
!CALL OS('X=Y     ',X=AKN ,Y=CP_T2D(-2)%AKN)
!CALL OS('X=Y     ',X=EPN ,Y=CP_T2D(-2)%EPN)

!     BOTTOM TOPOGRAPHY, PER POINT
      CALL OS('X=Y     ',X=ZF ,Y=CP_T2D(-2)%ZF)

!     FRICTION COEFFICIENT, IN TERMS OF STRICKLER, CHEZY, ETC
      IF ( .NOT. fts )  THEN
         CALL OS('X=Y     ',X=CHESTR ,Y=CP_T2D(-2)%CHESTR)
!!         CALL OS('X=Y     ',X=CHBORD ,Y=CP_T2D(-2)%CHBORD)
      ENDIF

!     BOUNDS
!CALL OS('X=Y     ' ,X=UBOR ,Y=CP_T2D(-2)%UBOR)
!CALL OS('X=Y     ' ,X=VBOR ,Y=CP_T2D(-2)%VBOR)
!CALL OS('X=Y     ' ,X=HBOR ,Y=CP_T2D(-2)%HBOR)

      AT = CP_T2D(-2)%AT_T2D

      PRINT *,'CHECKPOINT_T2D_COPY_FROM_TMP_CP DONE: LT ',LT

      RETURN
      END SUBROUTINE CHECKPOINT_T2D_COPY_FROM_TMP_CP


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


C                       *****************
      SUBROUTINE CHECKPOINT_T2D_COMPARE( cp1, cp2 )
C                       *****************
C
C***********************************************************************
C TELEMAC 2D VERSION 6.1          01/04/2012    U.Merkel
C***********************************************************************
C
C
C  FUNCTION  : CHECKPOINT TESTING / DEBUGGING ........
C
C
C-----------------------------------------------------------------------
C  ARGUMENTS USED
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|_______________________________________________
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------

      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D

      INTEGER, INTENT(IN)   :: cp1, cp2
C     UHM DEBUG / Delete later
      TYPE(BIEF_OBJ), SAVE, TARGET :: UHM_DU, UHM_DV, UHM_DH,
     $                                UHM_DZF, UHM_DCST, UHM_DCHB

      INTEGER MAXIN !UHM DEBUG
      DOUBLE PRECISION MAXVA !UHM DEBUG
      INTEGER J, K
      INTEGER, SAVE :: CALLED = 0

C     UHM DEBUG / Delete later
      IF ( CALLED .EQ. 0 ) THEN
         CALL BIEF_ALLVEC(1,UHM_DU,'UHM_DU',IELMU,1,1,MESH)
         CALL BIEF_ALLVEC(1,UHM_DV,'UHM_DV',IELMU,1,1,MESH)
         CALL BIEF_ALLVEC(1,UHM_DH,'UHM_DH',IELMH,1,1,MESH)
         CALL BIEF_ALLVEC(1,UHM_DZF,'UHM_DZF ',IELMH,1,1,MESH)
         CALL BIEF_ALLVEC(1,UHM_DCST,'UHM_DCST',IELMU,1,1,MESH)
         CALL BIEF_ALLVEC(1,UHM_DCHB,'UHM_DCHB',IELMU,1,1,MESH)

         CALLED = 1
      END IF

      CALL OS('X=Y-Z   ',X=UHM_DU  ,Y=CP_T2D(CP1)%U, Z=CP_T2D(CP2)%U )
      CALL OS('X=Y-Z   ',X=UHM_DH  ,Y=CP_T2D(CP1)%H, Z=CP_T2D(CP2)%H )
      CALL OS('X=Y-Z   ',X=UHM_DU  ,Y=CP_T2D(CP1)%V, Z=CP_T2D(CP2)%V )
      CALL OS('X=Y-Z   ',X=UHM_DZF ,Y=CP_T2D(CP1)%ZF, Z=CP_T2D(CP2)%ZF)
      CALL OS('X=Y-Z   ',X=UHM_DCST ,Y=CP_T2D(CP1)%CHESTR,
     $                               Z=CP_T2D(CP2)%CHESTR )
!!      CALL OS('X=Y-Z   ',X=UHM_DCHB ,Y=CP_T2D(CP1)%CHBORD,
!!     $                               Z=CP_T2D(CP2)%CHBORD )

      CALL MAXI(MAXVA,MAXIN,UHM_DU%R,NPOIN)
      PRINT *, 'CHECKPOINT_T2D_COMPARE(',CP1,',',CP2,')',
     $     '  MAX_DIFF_U: ', MAXVA
      CALL MAXI(MAXVA,MAXIN,UHM_DH%R,NPOIN)
      PRINT *, 'CHECKPOINT_T2D_COMPARE(',CP1,',',CP2,')',
     $     '  MAX_DIFF_H: ', MAXVA
      CALL MAXI(MAXVA,MAXIN,UHM_DU%R,NPOIN)
      PRINT *, 'CHECKPOINT_T2D_COMPARE(',CP1,',',CP2,')',
     $     '  MAX_DIFF_V: ', MAXVA

      CALL MAXI(MAXVA,MAXIN,UHM_DZF%R,NPOIN)
      PRINT *, 'CHECKPOINT_T2D_COMPARE(',CP1,',',CP2,')',
     $     '  MAX_DIFF_ZF: ', MAXVA
      CALL MAXI(MAXVA,MAXIN,UHM_DCST%R,NPOIN)
      PRINT *, 'CHECKPOINT_T2D_COMPARE(',CP1,',',CP2,')',
     $     '  MAX_DIFF_CHESTR: ', MAXVA
 !!     CALL MAXI(MAXVA,MAXIN,UHM_DCHB%R,NPOIN)
 !!     PRINT *, 'CHECKPOINT_T2D_COMPARE(',CP1,',',CP2,')',
 !!    $     '  MAX_DIFF_CHBORD: ', MAXVA

!      do J = 1,NPOIN
!     Print*, 'UHMDif',J, U_CP(4)%R(J) - U_CP(2)%R(J),
!    &                     H_CP(4)%R(J) - H_CP(2)%R(J)
!      enddo

      RETURN

      END SUBROUTINE CHECKPOINT_T2D_COMPARE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



      END MODULE AD_TELEMAC2D_CHECKPOINT

