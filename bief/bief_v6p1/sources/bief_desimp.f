!                    **********************
                     SUBROUTINE BIEF_DESIMP
!                    **********************
!
     &(FORMAT_RES,VARSOR,HIST,NHIST,N,NRES,STD,AT,LT,LISPRD,LEOPRD,
     & SORLEO,SORIMP,MAXVAR,TEXTE,PTINIG,PTINIL)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    WRITES TO RESULT OR LISTING FILE.
!
!history  J-M HERVOUET (LNHE)
!+        01/04/2009
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
!| FORMAT_RES     |---| 
!| HIST           |---| 
!| LEOPRD         |-->| PERIODE DE SORTIE SUR LE FICHIER DE RESULTAT
!| LISPRD         |-->| PERIODE DE SORTIE SUR LISTING.
!| MAXVAR         |---| 
!| N              |-->| NOMBRE DE POINTS DU MAILLAGE.
!| NHIST          |-->| NOMBRE DE VALEURS DANS HIST
!| NRES           |-->| UNITE LOGIQUE DU FICHIER DE RESULTATS.
!| PTINIG         |-->| 1ER PAS DE TEMPS POUR LES SORTIES GRAPHIQUES
!| PTINIL         |-->| 1ER PAS DE TEMPS POUR LES SORTIES LISTING
!| SORIMP         |---| 
!| SORLEO         |---| 
!| STD            |-->| BINAIRE DU FICHIER DE RESULTATS (IBM,I3E,STD)
!| TEXTE          |-->| NOMS ET UNITES DES VARIABLES.
!| VARSOR         |-->| BLOC CONTENANT LES VARIABLES A METTRE DANS LES
!|                |   | RESULTATS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ)   , INTENT(IN) :: VARSOR
      CHARACTER(LEN=8) , INTENT(IN) :: FORMAT_RES
      INTEGER          , INTENT(IN) :: NRES,LT,LISPRD,LEOPRD
      INTEGER          , INTENT(IN) :: NHIST,PTINIG,PTINIL,N
      INTEGER          , INTENT(IN) :: MAXVAR
      DOUBLE PRECISION , INTENT(IN) :: AT,HIST(NHIST)
      CHARACTER(LEN=32), INTENT(IN) :: TEXTE(*)
      CHARACTER(LEN=3) , INTENT(IN) :: STD
      LOGICAL          , INTENT(IN) :: SORLEO(MAXVAR),SORIMP(MAXVAR)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER LTT,K
!
      LOGICAL LEO,IMP
!
!-----------------------------------------------------------------------
!
! LOGICAL THAT DEFINE THE OUTPUTS
!
      IMP=.FALSE.
      LEO=.FALSE.
      LTT=(LT/LISPRD)*LISPRD
      IF(LT.EQ.LTT.AND.LT.GE.PTINIL) IMP=.TRUE.
      LTT=(LT/LEOPRD)*LEOPRD
      IF(LT.EQ.LTT.AND.LT.GE.PTINIG) LEO=.TRUE.
!
!-----------------------------------------------------------------------
!
      IF(LEO) THEN
        CALL WRITE_DATA(FORMAT_RES,       ! RESULT FILE FORMAT ID
     &                  NRES,             ! LU RESULT FILE
     &                  MAXVAR,           ! MAX NB OF VARIABLES IN VARSOR
     &                  AT,               ! TIME
     &                  LT,               ! TIMESTEP
     &                  SORLEO(1:MAXVAR), ! OUTPUT OR NOT
     &                  TEXTE(1:MAXVAR),  ! TIMESTEP
     &                  VARSOR,           ! COLLECTION OF VECTORS
     &                  N)                ! NUMBER OF VALUES
      ENDIF
!
! TO LISTING FILE
!
      IF(IMP) THEN
        DO K=1,MAXVAR
          IF(SORIMP(K)) THEN
            IF(ASSOCIATED(VARSOR%ADR(K)%P%R)) THEN
              CALL IMPVEC(VARSOR%ADR(K)%P%R,TEXTE(K),N)
            ELSE
              IF(LNG.EQ.1) THEN
                WRITE(LU,*) 'DESIMP: VARIABLE NUMERO: ',K
                WRITE(LU,*) '        PAS OU MAL ALLOUEE'
                WRITE(LU,*) '        OU POINTEUR NON ASSOCIE'
              ENDIF
              IF(LNG.EQ.2) THEN
                WRITE(LU,*) 'DESIMP: VARIABLE NUMBER: ',K
                WRITE(LU,*) '        NOT OR NOT WELL ALLOCATED'
                WRITE(LU,*) '        OR POINTER NOT ASSOCIATED '
              ENDIF
!             CALL PLANTE(1)
!             STOP
            ENDIF
          ENDIF
        ENDDO
      ENDIF
!
!=======================================================================
!
      RETURN
      END