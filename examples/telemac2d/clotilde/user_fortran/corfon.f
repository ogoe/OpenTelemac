!                       *****************
                        SUBROUTINE CORFON
!                       *****************
!
!***********************************************************************
! PROGICIEL : TELEMAC-2D 5.0          01/03/90    J-M HERVOUET
!***********************************************************************
!
!  USER SUBROUTINE CORFON
!
!  FUNCTION  : MODIFICATION OF THE BOTTOM TOPOGRAPHY
!
!
!-----------------------------------------------------------------------
!  ARGUMENTS USED IN THE EXAMPLE
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|_______________________________________________
! |      ZF        |<-->| FOND A MODIFIER.
! |      X,Y,(Z)   | -->| COORDONNEES DU MAILLAGE (Z N'EST PAS EMPLOYE).
! |      A         |<-- | MATRICE
! |      T1,2      | -->| TABLEAUX DE TRAVAIL (DIMENSION NPOIN)
! |      W1        | -->| TABLEAU DE TRAVAIL (DIMENSION 3 * NELEM)
! |      NPOIN     | -->| NOMBRE DE POINTS DU MAILLAGE.
! |      PRIVE     | -->| TABLEAU PRIVE POUR L'UTILISATEUR.
! |      LISFON    | -->| NOMBRE DE LISSAGES DU FOND.
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! PROGRAMME APPELANT :
! PROGRAMMES APPELES : RIEN EN STANDARD
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
! Debut CR
! seuil passe usiniere
!      INTEGER boucleCR5,nbathyCR5
!      PARAMETER (nbathyCR5=25)
!      INTEGER numeroCR5(nbathyCR5)
!      DOUBLE PRECISION fondCR5(nbathyCR5)
!      DATA numeroCR5 / 41161,44127,43449,44059,41060,44195,44126,44128,
!     &44221,44170,43752,43976,43448,44238,43447,44095,43975,43977,44006,
!     &44239,9866,44153,43918,44007,13267/
!      DATA fondCR5  / 52.50D0, 52.50D0,52.50D0,52.50D0,52.50D0,52.50D0,
!     &52.50D0,52.50D0,52.50D0,52.50D0,52.50D0,52.50D0,52.50D0,52.50D0,
!     &52.50D0,52.50D0,52.50D0,52.50D0,52.50D0,52.50D0,52.50D0,52.50D0,
!     &52.50D0,52.50D0,52.50D0 /
! seuil passe navigable
      INTEGER BOUCLECR6,NBATHYCR6
      PARAMETER (NBATHYCR6=55)
      INTEGER NUMEROCR6(NBATHYCR6)
      DOUBLE PRECISION FONDCR6(NBATHYCR6)
      DATA NUMEROCR6 / 44437, 44438, 44366, 44324, 44326, 43373, 44473,
     &43748, 44325, 43747, 44442, 44415, 44385, 44386, 44373, 39586,
     &44432,43713,44367,7277,44377,44393,44433,
     &44464,44302,43720,44388,43715,44459,43714,44322,44387,44389,
     &44458,44449,29578,44431,43721,44472,38636, 44358, 44384, 44289,
     &44290, 44423, 43485, 44372, 43840, 44291, 43792, 44402, 44371,
     &44330, 44331, 44346/
      DATA FONDCR6  / 66.40D0, 66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,
     &66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,
     &66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,
     &66.40D0,66.40D0,66.40D0, 66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,
     &66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,
     &66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,
     &66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,
     &66.40D0 /
! seuil passe est
      INTEGER BOUCLECR7,NBATHYCR7
      PARAMETER (NBATHYCR7=21)
      INTEGER NUMEROCR7(NBATHYCR7)
      DOUBLE PRECISION FONDCR7(NBATHYCR7)
      DATA NUMEROCR7 / 43910, 43890, 43504, 3801,43886,41708,43838,
     &43839,43887,10707, 43692,17328,43780,43691,43602,42060,43477,
     &13973, 43570, 43476, 43475 /
      DATA FONDCR7  / 66.40D0, 66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,
     &66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,
     &66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0/
! Fin CR
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL MAS
!
!-----------------------------------------------------------------------
!
!  MODIFICATION DU FOND DE CERTAINS POINTS DE LA FRONTIERE
!
!  LISSAGES EVENTUELS DU FOND
!
      IF(LISFON.GT.0) THEN
!
        MAS=.TRUE.
        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     &              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!      IF(LNG.EQ.1) THEN
!        IF(LISFON.EQ.0) THEN
!          WRITE(LU,*)
!          WRITE(LU,*) 'CORFON (TELEMAC2D) : PAS DE MODIFICATION DU FOND'
!          WRITE(LU,*)
!        ELSE
!          WRITE(LU,*)
!          WRITE(LU,*) 'CORFON (TELEMAC2D) : ',LISFON,' LISSAGES DU FOND'
!          WRITE(LU,*)
!        ENDIF
!      ENDIF
!      IF(LNG.EQ.2) THEN
!        IF(LISFON.EQ.0) THEN
!          WRITE(LU,*)
!          WRITE(LU,*) 'CORFON (TELEMAC2D): NO MODIFICATION OF BOTTOM'
!          WRITE(LU,*)
!        ELSE
!          WRITE(LU,*)
!          WRITE(LU,*) 'CORFON (TELEMAC2D): ',LISFON,' BOTTOM SMOOTHINGS'
!          WRITE(LU,*)
!        ENDIF
!      ENDIF
!
!-----------------------------------------------------------------------
!
! Debut CR
!
!
      IF ((AT.GE.30000.D0) .AND. (AT.LE.31315.D0)) THEN
        DO BOUCLECR6=1,NBATHYCR6
          ZF%R(GLOBAL_TO_LOCAL_POINT(NUMEROCR6(BOUCLECR6),MESH))=
     &                                               0.01D0*AT-246.75D0
        ENDDO
      ELSEIF (AT.GT.31315.D0) THEN
        DO BOUCLECR6=1,NBATHYCR6
          ZF%R(GLOBAL_TO_LOCAL_POINT(NUMEROCR6(BOUCLECR6),MESH))=
     &                                               FONDCR6(BOUCLECR6)
        ENDDO
      ENDIF
!
      IF ((AT.GE.30000.D0) .AND. (AT.LE.31190.D0)) THEN
        DO BOUCLECR7=1,NBATHYCR7
          ZF%R(GLOBAL_TO_LOCAL_POINT(NUMEROCR7(BOUCLECR7),MESH))=
     &                                                0.01D0*AT-245.5D0
        ENDDO
      ELSEIF (AT.GT.31190.D0) THEN
        DO BOUCLECR7=1,NBATHYCR7
          ZF%R(GLOBAL_TO_LOCAL_POINT(NUMEROCR7(BOUCLECR7),MESH))=
     &                                               FONDCR7(BOUCLECR7)
        ENDDO
      ENDIF
!
! Fin CR
      RETURN
      END

