!                       *****************
                        SUBROUTINE DYNAMI
!                       *****************
!
     &(NPTFR,NBOR,LIHBOR,LIUBOR,LIVBOR,LITBOR,NCOLFR,MAILLE,NLIM)
!
!***********************************************************************
! STBTEL VERSION        6.0                J-C GALLAND (LNH) 30 87 78 13
!                                          J-M JANIN   (LNH) 30 87 72 84
!                                          P LANG      (LHF)
! ORIGINE   : TELEMAC
!***********************************************************************
!
!  FONCTION : ECRITURE DU FICHIER DYNAM DE TELEMAC
!             POUR TOUTE MODIFICATION DES CL VOIR DANS LE SPGM STBTEL
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! I      NOM       IMODEI                   ROLE
! I________________I____I______________________________________________
! |   NPTFR        | -->| NOMBRE DE POINTS FRONTIERE
! |   NBOR         | -->| TABLEAU DES POINTS DE BORD
! |   NCOLFR       | -->| TABLEAU DES COULEURS DES POINTS FRONTIERES
! |   MAILLE       | -->| NOM DU MAILLEUR
! |________________|____|______________________________________________
! | COMMON:        |    |
! |  GEO:          |    |
! |    MESH        | -->| TYPE DES ELEMENTS DU MAILLAGE
! |    NDP         | -->| NOMBRE DE NOEUDS PAR ELEMENTS
! |    NELEM       | -->| NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!----------------------------------------------------------------------
! APPELE PAR : STBTEL
! APPEL DE : -
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NPTFR, NLIM
      INTEGER, INTENT(IN) :: NBOR(*) , NCOLFR(*)
      INTEGER, INTENT(INOUT) :: LIHBOR(*) , LIUBOR(*)
      INTEGER, INTENT(INOUT) :: LIVBOR(*) ,LITBOR(*)
      CHARACTER(LEN=9), INTENT(IN) :: MAILLE
!
      INTEGER ILOG , IADH , IENT , IENTU , IINC , ISORT
!
      INTEGER J
!
      DOUBLE PRECISION HBOR , UBOR , VBOR , AUBOR , TBOR , ATBOR ,BTBOR
!
!
!***********************************************************************
!
      ILOG = 2
      IADH = 0
      IENT = 5
      IENTU= 6
      ISORT= 4
      IINC = 1
!
      REWIND NLIM
!
      DO J =1,NPTFR
!
! PAR DEFAUT, ON SUPPOSE QUE LE POINT EST UN POINT FRONTIERE SOLIDE
! SANS FROTTEMENT. LA COULEUR 11, STANDARD POUR SUPERTAB, DONNE CE
! TYPE DE CARACTERISTIQUE.
!
        LIHBOR(J)=ILOG
        LIUBOR(J)=ILOG
        LIVBOR(J)=ILOG
        LITBOR(J)=ILOG
!
        IF(NCOLFR(J).EQ.1) THEN
!
! H IMPOSEE , U ET V LIBRES
!
          LIHBOR(J)=IENT
          LIUBOR(J)=ISORT
          LIVBOR(J)=ISORT
          LITBOR(J)=ISORT
!
        ELSE IF (NCOLFR(J).EQ.2) THEN
!
!  H  IMPOSEE , DEBIT IMPOSE
!
          LIHBOR(J)=IENT
          LIUBOR(J)=IENT
          LIVBOR(J)=IENT
          LITBOR(J)=IENT
!
        ELSE IF (NCOLFR(J).EQ.3) THEN
!
!  H , U ET V IMPOSEES
!
          LIHBOR(J)=IENT
          LIUBOR(J)=IENTU
          LIVBOR(J)=IENTU
          LITBOR(J)=IENT
!
        ELSE IF (NCOLFR(J).EQ.4) THEN
!
! H IMPOSEE , U LIBRE , V NULLE
!
          LIHBOR(J)=IENT
          LIUBOR(J)=ISORT
          LIVBOR(J)=IADH
          LITBOR(J)=ISORT
!
        ELSE IF (NCOLFR(J).EQ.5) THEN
!
!  CONDITION D'ONDE INCIDENTE
!
          LIHBOR(J)=IINC
          LIUBOR(J)=IINC
          LIVBOR(J)=IINC
          LITBOR(J)=ISORT
!
        ELSE IF (NCOLFR(J).EQ.7) THEN
!
! H IMPOSEE , U NULLE , V LIBRE
!
          LIHBOR(J)=IENT
          LIUBOR(J)=IADH
          LIVBOR(J)=ISORT
          LITBOR(J)=ISORT
!
        ELSE IF (NCOLFR(J).EQ.8) THEN
!
! H LIBRE , U ET V IMPOSEES
!
          LIHBOR(J)=ISORT
          LIUBOR(J)=IENT
          LIVBOR(J)=IENT
          LITBOR(J)=IENT
!
        ELSE IF (NCOLFR(J).EQ.9) THEN
!
!  H LIBRE , U ET V IMPOSEES
!
          LIHBOR(J)=ISORT
          LIUBOR(J)=IENTU
          LIVBOR(J)=IENTU
          LITBOR(J)=IENT
!
        ELSE IF (NCOLFR(J).EQ.12) THEN
!
! H LIBRE , U IMPOSEE , V NULLE
!
          LIHBOR(J)=ISORT
          LIUBOR(J)=IENT
          LIVBOR(J)=IADH
          LITBOR(J)=IENT
!
        ELSE IF (NCOLFR(J).EQ.13) THEN
!
! FRONTIERE SOLIDE AVEC V NULLE
!
          LIHBOR(J)=ILOG
          LIUBOR(J)=ILOG
          LIVBOR(J)=IADH
          LITBOR(J)=ILOG
!
        ELSE IF (NCOLFR(J).EQ.14) THEN
!
! FRONTIERE SOLIDE AVEC U NULLE
!
          LIHBOR(J)=ILOG
          LIUBOR(J)=IADH
          LIVBOR(J)=ILOG
          LITBOR(J)=ILOG
!
        ELSE IF (NCOLFR(J).EQ.15) THEN
!
! H LIBRE , U NULLE , V IMPOSEE
!
          LIHBOR(J)=ISORT
          LIUBOR(J)=IADH
          LIVBOR(J)=IENT
          LITBOR(J)=IENT
!
        ENDIF
!
      ENDDO
!
      DO J=1,NPTFR
!
        HBOR = 0.D0
        UBOR = 0.D0
        VBOR = 0.D0
        AUBOR = 0.D0
        TBOR = 0.D0
        ATBOR = 0.D0
        BTBOR = 0.D0
!
        WRITE(NLIM,30) LIHBOR(J),LIUBOR(J),LIVBOR(J),HBOR,UBOR,VBOR,
     &                   AUBOR,LITBOR(J),TBOR,ATBOR,BTBOR,NBOR(J),J
!
      ENDDO
!
      IF (LNG.EQ.1.AND.MAILLE(1:8).NE.'SUPERTAB'.AND.
     &                 MAILLE(1:7).NE.'TRIGRID') WRITE(LU,40) MAILLE
      IF (LNG.EQ.2.AND.MAILLE(1:8).NE.'SUPERTAB'.AND.
     &                 MAILLE(1:7).NE.'TRIGRID') WRITE(LU,3040) MAILLE
!
!-----------------------------------------------------------------------
!
 40   FORMAT(/,
     & ' *********************************************************',/,
     & ' ATTENTION : LE FICHIER UNIVERSEL EST AU FORMAT ',A8,/,
     & '             IL FAUDRA VERIFIER LES CONDITIONS AUX LIMITES',/,
     & '             DANS LE FICHIER DES CONDITIONS AUX LIMITES',/,
     & ' *********************************************************',/)
!
 3040 FORMAT(/,
     & ' **************************************************',/,
     & ' BEWARE: THE UNIVERSAL FILE FORMAT IS ',A8,/,
     & '         BOUNDARY CONDITIONS WILL HAVE TO BE',/,
     & '         CHECKED IN THE BOUNDARY CONDITIONS FILE',/,
     & ' **************************************************',/)
!
 30   FORMAT(1X,I2,1X,2(I1,1X),3(F6.3,1X),1X,
     &                    F3.1,3X,I1,1X,3(F6.3,1X),1I9,1X,1I9)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
