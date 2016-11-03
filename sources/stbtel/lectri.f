!                       *****************
                        SUBROUTINE LECTRI
!                       *****************
!
     & (X, Y, IKLE, NCOLOR,NGEO , NFO1)
!
!***********************************************************************
! PROGICIEL : STBTEL V5.2          18/08/93   P. LANG  (LHF)
!***********************************************************************
!
!     FONCTION  : LECTURE DES INFOS DE GEOMETRIE DANS LES FICHIERS TRIGR
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! !      NOM       !MODE!                   ROLE
! !________________!____!______________________________________________
! ! X,Y            !<-- ! COORDONNEES DES POINTS DU MAILLAGE
! ! IKLE           !<-- ! NUMEROS GLOBAUX DES NOEUDS DE CHAQUE ELEMENT
! ! NCOLOR         !<-- ! TABLEAU DES COULEURS DES NOEUDS(POUR LES CL)
! !________________!____!______________________________________________
! ! COMMON:        !    !
! !  GEO:          !    !
! !    MESH        ! -->! TYPE DES ELEMENTS DU MAILLAGE
! !    NDP         ! -->! NOMBRE DE NOEUDS PAR ELEMENTS
! !    NPOIN       ! -->! NOMBRE TOTAL DE NOEUDS DU MAILLAGE
! !    NELEM       ! -->! NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
! !    NPMAX       ! -->! DIMENSION EFFECTIVE DES TABLEAUX X ET Y
! !                !    ! (NPMAX = NPOIN + 100)
! !    NELMAX      ! -->! DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT
! !                !    ! LES ELEMENTS (NELMAX = NELEM + 200)
! !  FICH:         !    !
! !    NRES        !--> ! NUMERO DU CANAL DU FICHIER DE SERAFIN
! !    NGEO       !--> ! NUMERO DU CANAL DU FICHIER MAILLEUR
! !    NLIM      !--> ! NUMERO DU CANAL DU FICHIER DYNAM DE TELEMAC
! !    NFO1      !--> ! NUMERO DU CANAL DU FICHIER TRIANGLE TRIGRID
! !                !    !
! !  INFO:         !    !
! !    LNG         !--> ! LANGUE UTILISEE
! !    LU          !--> ! CANAL DE SORTIE DES MESSAGES
! !________________!____!______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
! APPELE PAR :
! APPEL DE :
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_STBTEL, ONLY: NELEM,NPOIN,NELMAX
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NGEO, NFO1
      INTEGER, INTENT(INOUT) :: IKLE(NELMAX,4)
      INTEGER, INTENT(INOUT) :: NCOLOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: X(*), Y(*)
!
      INTEGER ITYPND,NUMNOD,I,J
!
! COMMON
!
!
      REWIND (NGEO)
      REWIND (NFO1)
      READ (NGEO,'(//)')
      DO I=1,NPOIN
        READ (NGEO,*,ERR=8000,END=9000) NUMNOD,X(I),Y(I),ITYPND
        IF (ITYPND.EQ.1) NCOLOR(I) = 11
        IF (ITYPND.EQ.2) NCOLOR(I) = 4
        IF (ITYPND.EQ.3) NCOLOR(I) = 5
        IF (ITYPND.EQ.4) NCOLOR(I) = 7
        IF (ITYPND.EQ.5) NCOLOR(I) = 8
        IF (ITYPND.EQ.6) NCOLOR(I) = 9
        IF (ITYPND.EQ.7) NCOLOR(I) = 1
        IF (ITYPND.EQ.8) NCOLOR(I) = 12
        IF (ITYPND.EQ.9) NCOLOR(I) = 15
        IF (ITYPND.EQ.10) NCOLOR(I) = 2
        IF (ITYPND.EQ.11) NCOLOR(I) = 3
        IF (ITYPND.EQ.12) NCOLOR(I) = 14
        IF (ITYPND.EQ.13) NCOLOR(I) = 13
      ENDDO
!
      DO I=1,NELEM
        READ (NFO1, * , ERR=8010, END=9010) (IKLE(I,J),J=1,3)
      ENDDO
      RETURN
 8000 CONTINUE
      IF (LNG.EQ.1) WRITE (LU,4000)
      IF (LNG.EQ.2) WRITE (LU,4001)
 4000 FORMAT (//,1X,'***************************************'
     &        ,/,1X,'SOUS-PROGRAMME LECTRI : ERREUR DANS LA'
     &        ,/,1X,'LECTURE DU FICHIER DE MAILLAGE TRIGRID.'
     &        ,/,1X,'***************************************')
 4001 FORMAT (//,1X,'****************************'
     &        ,/,1X,'SUBROUTINE LECTRI :'
     &        ,/,1X,'ERROR READING TRIGRID FILE.'
     &        ,/,1X,'****************************')
      CALL PLANTE(1)
      STOP
 9000 CONTINUE
      IF (LNG.EQ.1) WRITE (LU,4010)
      IF (LNG.EQ.2) WRITE (LU,4011)
 4010 FORMAT (//,1X,'***************************************'
     &        ,/,1X,'SOUS-PROGRAMME LECTRI : FIN DU FICHIER'
     &        ,/,1X,'DE MAILLAGE RENCONTREE'
     &        ,/,1X,'***************************************')
 4011 FORMAT (//,1X,'***************************************'
     &        ,/,1X,'SUBROUTINE LECTRI : UNEXPECTED END OF'
     &        ,/,1X,'TRIGRID FILE ENCOUNTERED'
     &        ,/,1X,'***************************************')
      CALL PLANTE(1)
      STOP
 8010 CONTINUE
      IF (LNG.EQ.1) WRITE (LU,4020)
      IF (LNG.EQ.2) WRITE (LU,4021)
 4020 FORMAT (//,1X,'***************************************'
     &        ,/,1X,'SOUS-PROGRAMME LECTRI : ERREUR DANS LA'
     &        ,/,1X,'LECTURE DU FICHIER TRIANGLE DE TRIGRID'
     &        ,/,1X,'***************************************')
 4021 FORMAT (//,1X,'***************************************'
     &        ,/,1X,'SUBROUTINE LECTRI : ERROR READING'
     &        ,/,1X,'TRIGRID TRIANGLE FILE'
     &        ,/,1X,'***************************************')
      CALL PLANTE(1)
      STOP
 9010 CONTINUE
      IF (LNG.EQ.1) WRITE (LU,4030)
      IF (LNG.EQ.2) WRITE (LU,4031)
 4030 FORMAT (//,1X,'***************************************'
     &        ,/,1X,'SOUS-PROGRAMME LECTRI :'
     &        ,/,1X,'FIN DU FICHIER TRIANGLE RENCONTRE'
     &        ,/,1X,'***************************************')
 4031 FORMAT (//,1X,'***************************************'
     &        ,/,1X,'SUBROUTINE LECTRI : END OF'
     &        ,/,1X,'TRIGRID TRIANGLE FILE ENCOUNTERED'
     &        ,/,1X,'***************************************')
      CALL PLANTE(1)
      STOP
      END
