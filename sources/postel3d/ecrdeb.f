!                       *****************
                        SUBROUTINE ECRDEB
!                       *****************
!
     &(CANAL,FFORMAT,TITCAS,NBV,NTRAC,NTRPA,C2DH,TEXTLU,IC,N)
!
!***********************************************************************
! POSTEL3D VERSION 5.1   01/09/99   T. DENOT (LNH) 01 30 87 74 89
! FORTRAN90
!***********************************************************************
!
!     FONCTION  :  OUVERTURE D'UN FICHIER POUR UNE COUPE
!               + ECRITURE DU DEBUT DE l'ENTETE (TITRE,NBV,TEXTE).
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! !      NOM       !MODE!                   ROLE                       !
! !________________!____!______________________________________________!
! !   CANAL        ! -->! CANAL DE SORTIE                              !
! !   BINCOU       ! -->! STANDARD DE BINAIRE POUR LES COUPES          !
! !   TITCAS       ! -->! TITRE LU DANS LE FICHIER DE RESULTATS        !
! !   NBV          ! -->! NOMBRE DE VARIABLES EN SORTIE                !
! !   NTRAC        ! -->! NOMBRE DE TRACEURS ACTIFS                    !
! !   NTRPA        ! -->! NOMBRE DE TRACEURS PASSIFS                   !
! !   SORG3D       ! -->! INDICATEUR DES VARIABLES ENREGISTREES        !
! !   C2DH         ! -->! INDICATEUR DE LA NATURE DE LA COUPE (H OU V) !
! !________________!____!______________________________________________!
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! SOUS-PROGRAMME APPELE PAR : PRE2DH , COUPEV
! SOUS-PROGRAMME APPELES : ECRI2
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!***********************************************************************
!
      USE BIEF
      USE INTERFACE_HERMES
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER ,INTENT(IN) :: NBV(2),CANAL,NTRAC,NTRPA
      INTEGER I
      INTEGER IC,N
!
      LOGICAL C2DH
!
      CHARACTER*80 TITRE
      CHARACTER*72 TITCAS
      CHARACTER*32 TEXTLU(100)
      CHARACTER*15  NOMCOU
      CHARACTER*8 , INTENT(IN) ::  FFORMAT
!
      CHARACTER(LEN=3) :: EXTEN1
      CHARACTER(LEN=7) :: EXTEN2
      EXTERNAL EXTEN1,EXTEN2
      INTEGER :: NBVAR
      CHARACTER*32, ALLOCATABLE :: VAR_NAME(:)
      INTEGER :: IVAR, IERR
!
!-----------------------------------------------------------------------
!
!     OUVERTURE DU FICHIER D'UNE COUPE
!
      IF(C2DH) THEN
        NOMCOU = 'POSHOR_' // EXTEN1(IC) // '     '
      ELSE
        NOMCOU = 'POSVER_' // EXTEN2(IC,N)
      ENDIF
!
      CALL OPEN_MESH(FFORMAT,NOMCOU,CANAL,'READWRITE',IERR)
      CALL CHECK_CALL(IERR,'ECRDEB:OPEN_MESH')
!
!-----------------------------------------------------------------------
!
!  ECRITURE DU TITRE
!
      TITRE = TITCAS // FFORMAT
!
!-----------------------------------------------------------------------
!
!  ECRITURE DU NOMBRE DE VARIABLES EN SORTIE
!
!
      NBVAR = NBV(1) + NBV(2)
      IVAR = 1
!
!-----------------------------------------------------------------------
!
!  ECRITURE DES TEXTES
!
      IF (C2DH) THEN
        ALLOCATE(VAR_NAME(NBVAR),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'ECRDEB:VAR_NAME')
!
        IF (LNG.EQ.1) VAR_NAME(IVAR) = 
     &                        'INDICATEUR DOM.                 '
        IF (LNG.EQ.2) VAR_NAME(IVAR) = 
     &                        'DOMAIN INDICATOR                '
        IVAR = IVAR + 1
!
      ELSE
        ALLOCATE(VAR_NAME(NBVAR+2),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'ECRDEB:VAR_NAME')
!
        IF (LNG.EQ.1) VAR_NAME(IVAR) = 
     &                        'VITESSE UT      M/S             '
        IF (LNG.EQ.2) VAR_NAME(IVAR) = 
     &                        'VELOCITY UT     M/S             '
        IVAR = IVAR + 1
!
        IF (LNG.EQ.1) VAR_NAME(IVAR) = 
     &                        'VITESSE W       M/S             '
        IF (LNG.EQ.2) VAR_NAME(IVAR) = 
     &                        'VELOCITY W      M/S             '
        IVAR = IVAR + 1
!
        IF (LNG.EQ.1) VAR_NAME(IVAR) = 
     &                        'VITESSE UN      M/S             '
        IF (LNG.EQ.2) VAR_NAME(IVAR) = 
     &                        'VELOCITY UN     M/S             '
        IVAR = IVAR + 1
!
      ENDIF
!
!th pas besoin de 1 car on ne veut pas z
      IF (C2DH) THEN
        DO I=2,NBV(1)
          VAR_NAME(IVAR) = TEXTLU(I)
          IVAR = IVAR + 1
        ENDDO
      ELSE
        IF (NBV(1).GT.3) THEN
          DO I=4,NBV(1)
            VAR_NAME(IVAR) = TEXTLU(I)
            IVAR = IVAR + 1
          ENDDO
        ENDIF
      ENDIF
      CALL SET_HEADER(FFORMAT,CANAL,TITRE,IVAR-1,
     &                VAR_NAME(1:IVAR-1),IERR)
      CALL CHECK_CALL(IERR,'ECRDEB:SET_HEADER')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
