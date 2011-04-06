C                       ********************************
                        SUBROUTINE BEDLOAD_HIDING_FACTOR
C                       ********************************

     & (ACLADM, HIDFAC, NPOIN, HIDI, DM, KARIM_HOLLY_YANG, HIDING)
C
C**********************************************************************C
C SISYPHE VERSION 5.5                 M. GONZALES DE LINARES     2002  C
C                                     B. MINH DUC            NOV. 2002 C
C                                     F. HUVELIN              14/09/04 C


C COPYRIGHT EDF-BAW-IFH   
C***********************************************************************
C
C  FONCTION  : Hiding factor for each node, sediment class and time step 
C
C     SUBROUTINE A REMPLIR PAR l'UTILISATEUR
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |                |    |  
C |    ICLASSE     | -->| SEDIMENT CLASS
C |    HIDING      |<-- | HIDING FACTOR FOR PARTICULAR CLASS FOR EACH POINT
C |    ACLADM      | -->| MEAN DIAMETER OF THE ACTIVE LAYER
C |    UNLADM      | -->| MEAN DIAMETER OF THE ACTIVE STRATUM  
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C PROGRAMME APPELANT : SISYPHE (CALLED AT EACH TIME STEP)
C PROGRAMMES APPELES : 
C***********************************************************************

!======================================================================!
!======================================================================!
!                    DECLARATION DES TYPES ET DIMENSIONS               !
!======================================================================!
!======================================================================!

      ! 1/ MODULES
      ! ----------
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU


      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ACLADM
      INTEGER,          INTENT(IN)    :: HIDFAC, NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: HIDI, DM,KARIM_HOLLY_YANG
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HIDING


      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER :: J
      DOUBLE PRECISION :: COEFA
C
C     COEFFICIENT POUR LA FORMULE EMPIRIQUE
C     MATHIEU...
C
      COEFA = 0.8D0
C
!======================================================================!
!======================================================================!
!                               PROGRAMME                              !
!======================================================================!
!======================================================================!


      ! CONSTANT HIDING FACTOR
      ! ----------------------
      IF (HIDFAC.EQ.0)THEN

         CALL OS('X=C     ', X=HIDING, C=HIDI)

      ! FORMULA OF EGIAZAROFF
      ! ---------------------
      ELSE IF (HIDFAC.EQ.1) THEN
         DO J = 1, NPOIN
            HIDING%R(J) = (DLOG10(19.D0)
     &                  /  DLOG10(19.D0*DM/ACLADM%R(J)))**2
         ENDDO
      ! FORMULA OF ASHIDA AND MICHIUE
      ! -----------------------------
      ELSE IF (HIDFAC.EQ.2) THEN
        DO J = 1, NPOIN
           IF (DM/ACLADM%R(J).GE.0.4D0) THEN
              HIDING%R(J) = (DLOG10(19.D0)
     &                    /  DLOG10(19.D0*DM/ACLADM%R(J)))**2
           ELSE
              HIDING%R(J) = 0.85D0*(ACLADM%R(J)/DM)
           ENDIF
        ENDDO
      ! ELSE IF (HIDFAC.EQ.3) => Hunziker cf subroutine hunz.f
      ! ------------------------------------------------------
      ! FORMULA OF KARIM, HOLLY AND YANG
      ! --------------------------------
      ELSE IF (HIDFAC.EQ.4) THEN
         DO J = 1, NPOIN
            HIDING%R(J) = (DM/ACLADM%R(J))**0.85D0
         ENDDO
      ENDIF
C
C..... MATTHIEU EMPIRICAL FORMULA
C
      IF(HIDFAC.EQ.5) THEN
        DO J=1,NPOIN
          HIDING%R(J)=       (DM/ACLADM%R(J))**(-COEFA)
        ENDDO
       ENDIF 	  
C
!======================================================================!
!======================================================================!
!
      RETURN
      END SUBROUTINE BEDLOAD_HIDING_FACTOR

