!                       *****************
                        SUBROUTINE ECRDEB
!                       *****************
!
     &(CANAL,BINCOU,TITCAS,NBV,NTRAC,NTRPA,C2DH,TEXTLU,IC,N)
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
!***********************************************************************
!
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER ,INTENT(IN) :: NBV(2),CANAL,NTRAC,NTRPA
      INTEGER ISTAT
      INTEGER I
      INTEGER IC,N
!
      LOGICAL C2DH
!
      CHARACTER*80 TITRE
      CHARACTER*72 TITCAS
      CHARACTER*32 TEXTE
      CHARACTER*32 TEXTLU(100)
      CHARACTER*15  NOMCOU
      CHARACTER*3 , INTENT(IN) ::  BINCOU
!
      CHARACTER(LEN=2) CB
      DOUBLE PRECISION XB(2)
      INTEGER IB(2)
      CHARACTER(LEN=3) :: EXTEN1
      CHARACTER(LEN=7) :: EXTEN2
      EXTERNAL EXTEN1,EXTEN2
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
      OPEN(CANAL, FILE=NOMCOU , FORM='UNFORMATTED',ACTION='READWRITE' )
!
!-----------------------------------------------------------------------
!
!  ECRITURE DU TITRE
!
      TITRE = TITCAS // '        '
      CALL ECRI2(XB,IB,TITRE,80,'CH',CANAL,BINCOU,ISTAT)
!
!-----------------------------------------------------------------------
!
!  ECRITURE DU NOMBRE DE VARIABLES EN SORTIE
!
!
      CALL ECRI2(XB,NBV,CB,2,'I',CANAL,BINCOU,ISTAT)
!
!-----------------------------------------------------------------------
!
!  ECRITURE DES TEXTES
!
      IF (C2DH) THEN
!
         IF (LNG.EQ.1) TEXTE = 'INDICATEUR DOM.                 '
         IF (LNG.EQ.2) TEXTE = 'DOMAIN INDICATOR                '
         CALL ECRI2(XB,IB,TEXTE,32,'CH',CANAL,BINCOU,ISTAT)
!
      ELSE
!
         IF (LNG.EQ.1) TEXTE = 'VITESSE UT      M/S             '
         IF (LNG.EQ.2) TEXTE = 'VELOCITY UT     M/S             '
         CALL ECRI2(XB,IB,TEXTE,32,'CH',CANAL,BINCOU,ISTAT)
!
         IF (LNG.EQ.1) TEXTE = 'VITESSE W       M/S             '
         IF (LNG.EQ.2) TEXTE = 'VELOCITY W      M/S             '
         CALL ECRI2(XB,IB,TEXTE,32,'CH',CANAL,BINCOU,ISTAT)
!
         IF (LNG.EQ.1) TEXTE = 'VITESSE UN      M/S             '
         IF (LNG.EQ.2) TEXTE = 'VELOCITY UN     M/S             '
         CALL ECRI2(XB,IB,TEXTE,32,'CH',CANAL,BINCOU,ISTAT)
!
      ENDIF
!
!th pas besoin de 1 car on ne veut pas z
       if (c2dh) then
         DO I=2,NBV(1)
            CALL ECRI2(XB,IB,TEXTLU(I),32,'CH',CANAL,BINCOU,ISTAT)
         ENDDO
       else
         if (nbv(1).gt.3) then
         DO I=5,NBV(1)+1
            CALL ECRI2(XB,IB,TEXTLU(I),32,'CH',CANAL,BINCOU,ISTAT)
         ENDDO
         endif
       endif
!
!-----------------------------------------------------------------------
!
      RETURN
      END