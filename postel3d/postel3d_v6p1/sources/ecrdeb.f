C                       *****************
                        SUBROUTINE ECRDEB
C                       *****************
C
     *(CANAL,BINCOU,TITCAS,NBV,NTRAC,NTRPA,C2DH,TEXTLU,IC,N)
C
C***********************************************************************
C POSTEL3D VERSION 5.1   01/09/99   T. DENOT (LNH) 01 30 87 74 89
C FORTRAN90
C***********************************************************************
C
C     FONCTION  :  OUVERTURE D'UN FICHIER POUR UNE COUPE
C               + ECRITURE DU DEBUT DE l'ENTETE (TITRE,NBV,TEXTE).
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C !      NOM       !MODE!                   ROLE                       !
C !________________!____!______________________________________________!
C !   CANAL        ! -->! CANAL DE SORTIE                              !
C !   BINCOU       ! -->! STANDARD DE BINAIRE POUR LES COUPES          !
C !   TITCAS       ! -->! TITRE LU DANS LE FICHIER DE RESULTATS        !
C !   NBV          ! -->! NOMBRE DE VARIABLES EN SORTIE                !
C !   NTRAC        ! -->! NOMBRE DE TRACEURS ACTIFS                    !
C !   NTRPA        ! -->! NOMBRE DE TRACEURS PASSIFS                   !
C !   SORG3D       ! -->! INDICATEUR DES VARIABLES ENREGISTREES        !
C !   C2DH         ! -->! INDICATEUR DE LA NATURE DE LA COUPE (H OU V) !
C !________________!____!______________________________________________!
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C
C SOUS-PROGRAMME APPELE PAR : PRE2DH , COUPEV
C SOUS-PROGRAMME APPELES : ECRI2
C
C***********************************************************************
C
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER ,INTENT(IN) :: NBV(2),CANAL,NTRAC,NTRPA
      INTEGER ISTAT
      INTEGER I
      INTEGER IC,N
C
      LOGICAL C2DH
C
      CHARACTER*80 TITRE
      CHARACTER*72 TITCAS
      CHARACTER*32 TEXTE
      CHARACTER*32 TEXTLU(100)
      CHARACTER*15  NOMCOU
      CHARACTER*3 , INTENT(IN) ::  BINCOU
C
      CHARACTER(LEN=2) CB
      DOUBLE PRECISION XB(2)
      INTEGER IB(2)
      CHARACTER(LEN=3) :: EXTEN1
      CHARACTER(LEN=7) :: EXTEN2
      EXTERNAL EXTEN1,EXTEN2
C
C-----------------------------------------------------------------------
C
C     OUVERTURE DU FICHIER D'UNE COUPE
C
      IF(C2DH) THEN
        NOMCOU = 'POSHOR_' // EXTEN1(IC) // '     '
      ELSE
        NOMCOU = 'POSVER_' // EXTEN2(IC,N)
      ENDIF
C
      OPEN(CANAL, FILE=NOMCOU , FORM='UNFORMATTED',ACTION='READWRITE' )
C
C-----------------------------------------------------------------------
C
C  ECRITURE DU TITRE
C
      TITRE = TITCAS // '        '
      CALL ECRI2(XB,IB,TITRE,80,'CH',CANAL,BINCOU,ISTAT)
C
C-----------------------------------------------------------------------
C
C  ECRITURE DU NOMBRE DE VARIABLES EN SORTIE
C
C
      CALL ECRI2(XB,NBV,CB,2,'I',CANAL,BINCOU,ISTAT)
C
C-----------------------------------------------------------------------
C
C  ECRITURE DES TEXTES
C
      IF (C2DH) THEN
C
         IF (LNG.EQ.1) TEXTE = 'INDICATEUR DOM.                 '
         IF (LNG.EQ.2) TEXTE = 'DOMAIN INDICATOR                '
         CALL ECRI2(XB,IB,TEXTE,32,'CH',CANAL,BINCOU,ISTAT)
C
      ELSE
C
         IF (LNG.EQ.1) TEXTE = 'VITESSE UT      M/S             '
         IF (LNG.EQ.2) TEXTE = 'VELOCITY UT     M/S             '
         CALL ECRI2(XB,IB,TEXTE,32,'CH',CANAL,BINCOU,ISTAT)
C
         IF (LNG.EQ.1) TEXTE = 'VITESSE W       M/S             '
         IF (LNG.EQ.2) TEXTE = 'VELOCITY W      M/S             '
         CALL ECRI2(XB,IB,TEXTE,32,'CH',CANAL,BINCOU,ISTAT)
C
         IF (LNG.EQ.1) TEXTE = 'VITESSE UN      M/S             '
         IF (LNG.EQ.2) TEXTE = 'VELOCITY UN     M/S             '
         CALL ECRI2(XB,IB,TEXTE,32,'CH',CANAL,BINCOU,ISTAT)
C
      ENDIF
C
cth pas besoin de 1 car on ne veut pas z
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
C
C-----------------------------------------------------------------------
C
      RETURN
      END
