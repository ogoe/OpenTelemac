C                       *****************
                        SUBROUTINE DYNAMI
C                       *****************
C
     *(NPTFR,NBOR,LIHBOR,LIUBOR,LIVBOR,LITBOR,NCOLFR,MAILLE,NLIM)
C
C***********************************************************************
C STBTEL VERSION        6.0                J-C GALLAND (LNH) 30 87 78 13
C                                          J-M JANIN   (LNH) 30 87 72 84
C                                          P LANG      (LHF)
C ORIGINE   : TELEMAC
C***********************************************************************
C
C  FONCTION : ECRITURE DU FICHIER DYNAM DE TELEMAC
C             POUR TOUTE MODIFICATION DES CL VOIR DANS LE SPGM STBTEL
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C I      NOM       IMODEI                   ROLE
C I________________I____I______________________________________________
C |   NPTFR        | -->| NOMBRE DE POINTS FRONTIERE
C |   NBOR         | -->| TABLEAU DES POINTS DE BORD
C |   NCOLFR       | -->| TABLEAU DES COULEURS DES POINTS FRONTIERES
C |   MAILLE       | -->| NOM DU MAILLEUR
C |________________|____|______________________________________________
C | COMMON:        |    |
C |  GEO:          |    |
C |    MESH        | -->| TYPE DES ELEMENTS DU MAILLAGE
C |    NDP         | -->| NOMBRE DE NOEUDS PAR ELEMENTS
C |    NPOIN       | -->| NOMBRE TOTAL DE NOEUDS DU MAILLAGE
C |    NELEM       | -->| NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
C |    NPMAX       | -->| DIMENSION EFFECTIVE DES TABLEAUX X ET Y
C |                |    | (NPMAX = NPOIN + 0.1*NELEM)
C |    NELMAX      | -->| DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT
C |                |    | LES ELEMENTS (NELMAX = NELEM + 0.2*NELEM)
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C----------------------------------------------------------------------
C APPELE PAR : STBTEL
C APPEL DE : -
C***********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER ILOG , IADH , IENT , IENTU , IINC , ISORT
      INTEGER NLIM , NPTFR 
      INTEGER NELMAX , NELEM , NPOIN , MESH , NDP , NPMAX , J
      INTEGER NBOR(*) , NCOLFR(*)
      INTEGER LIHBOR(*) , LIUBOR(*) ,LIVBOR(*) ,LITBOR(*)
C
      DOUBLE PRECISION HBOR , UBOR , VBOR , AUBOR , TBOR , ATBOR ,BTBOR
C
      CHARACTER*9  MAILLE
C
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
C
C***********************************************************************
C
      ILOG = 2
      IADH = 0
      IENT = 5
      IENTU= 6
      ISORT= 4
      IINC = 1
C
      REWIND NLIM
C
      DO 10 J =1,NPTFR
C
C PAR DEFAUT, ON SUPPOSE QUE LE POINT EST UN POINT FRONTIERE SOLIDE
C SANS FROTTEMENT. LA COULEUR 11, STANDARD POUR SUPERTAB, DONNE CE
C TYPE DE CARACTERISTIQUE.
C
         LIHBOR(J)=ILOG
         LIUBOR(J)=ILOG
         LIVBOR(J)=ILOG
         LITBOR(J)=ILOG
C
         IF(NCOLFR(J).EQ.1) THEN
C
C H IMPOSEE , U ET V LIBRES
C
            LIHBOR(J)=IENT
            LIUBOR(J)=ISORT
            LIVBOR(J)=ISORT
            LITBOR(J)=ISORT
C
         ELSE IF (NCOLFR(J).EQ.2) THEN
C
C  H  IMPOSEE , DEBIT IMPOSE
C
            LIHBOR(J)=IENT
            LIUBOR(J)=IENT
            LIVBOR(J)=IENT
            LITBOR(J)=IENT
C
         ELSE IF (NCOLFR(J).EQ.3) THEN
C
C  H , U ET V IMPOSEES
C
            LIHBOR(J)=IENT
            LIUBOR(J)=IENTU
            LIVBOR(J)=IENTU
            LITBOR(J)=IENT
C
         ELSE IF (NCOLFR(J).EQ.4) THEN
C
C H IMPOSEE , U LIBRE , V NULLE
C
            LIHBOR(J)=IENT
            LIUBOR(J)=ISORT
            LIVBOR(J)=IADH
            LITBOR(J)=ISORT
C
         ELSE IF (NCOLFR(J).EQ.5) THEN
C
C  CONDITION D'ONDE INCIDENTE
C
            LIHBOR(J)=IINC
            LIUBOR(J)=IINC
            LIVBOR(J)=IINC
            LITBOR(J)=ISORT
C
         ELSE IF (NCOLFR(J).EQ.7) THEN
C
C H IMPOSEE , U NULLE , V LIBRE
C
            LIHBOR(J)=IENT
            LIUBOR(J)=IADH
            LIVBOR(J)=ISORT
            LITBOR(J)=ISORT
C
         ELSE IF (NCOLFR(J).EQ.8) THEN
C
C H LIBRE , U ET V IMPOSEES
C
            LIHBOR(J)=ISORT
            LIUBOR(J)=IENT
            LIVBOR(J)=IENT
            LITBOR(J)=IENT
C
         ELSE IF (NCOLFR(J).EQ.9) THEN
C
C  H LIBRE , U ET V IMPOSEES
C
            LIHBOR(J)=ISORT
            LIUBOR(J)=IENTU
            LIVBOR(J)=IENTU
            LITBOR(J)=IENT
C
         ELSE IF (NCOLFR(J).EQ.12) THEN
C
C H LIBRE , U IMPOSEE , V NULLE
C
            LIHBOR(J)=ISORT
            LIUBOR(J)=IENT
            LIVBOR(J)=IADH
            LITBOR(J)=IENT
C
         ELSE IF (NCOLFR(J).EQ.13) THEN
C
C FRONTIERE SOLIDE AVEC V NULLE
C
            LIHBOR(J)=ILOG
            LIUBOR(J)=ILOG
            LIVBOR(J)=IADH
            LITBOR(J)=ILOG
C
         ELSE IF (NCOLFR(J).EQ.14) THEN
C
C FRONTIERE SOLIDE AVEC U NULLE
C
            LIHBOR(J)=ILOG
            LIUBOR(J)=IADH
            LIVBOR(J)=ILOG
            LITBOR(J)=ILOG
C
         ELSE IF (NCOLFR(J).EQ.15) THEN
C
C H LIBRE , U NULLE , V IMPOSEE
C
            LIHBOR(J)=ISORT
            LIUBOR(J)=IADH
            LIVBOR(J)=IENT
            LITBOR(J)=IENT
C
         ENDIF
C
 10   CONTINUE
C
      DO 15 J=1,NPTFR
C
         HBOR = 0.D0
         UBOR = 0.D0
         VBOR = 0.D0
         AUBOR = 0.D0
         TBOR = 0.D0
         ATBOR = 0.D0
         BTBOR = 0.D0
C
         WRITE(NLIM,30) LIHBOR(J),LIUBOR(J),LIVBOR(J),HBOR,UBOR,VBOR,
     *                    AUBOR,LITBOR(J),TBOR,ATBOR,BTBOR,NBOR(J),J
C
15    CONTINUE
C
      IF (LNG.EQ.1.AND.MAILLE(1:8).NE.'SUPERTAB'.AND.
     +                 MAILLE(1:7).NE.'TRIGRID') WRITE(LU,40) MAILLE
      IF (LNG.EQ.2.AND.MAILLE(1:8).NE.'SUPERTAB'.AND.
     +                 MAILLE(1:7).NE.'TRIGRID') WRITE(LU,3040) MAILLE
C
C-----------------------------------------------------------------------
C
 40   FORMAT(/,
     * ' *********************************************************',/,
     * ' ATTENTION : LE FICHIER UNIVERSEL EST AU FORMAT ',A8,/,
     * '             IL FAUDRA VERIFIER LES CONDITIONS AUX LIMITES',/,
     * '             DANS LE FICHIER DES CONDITIONS AUX LIMITES',/,
     * ' *********************************************************',/)
C
 3040 FORMAT(/,
     * ' **************************************************',/,
     * ' BEWARE: THE UNIVERSAL FILE FORMAT IS ',A8,/,
     * '         BOUNDARY CONDITIONS WILL HAVE TO BE',/,
     * '         CHECKED IN THE BOUNDARY CONDITIONS FILE',/,
     * ' **************************************************',/)
C
 30   FORMAT(1X,I2,1X,2(I1,1X),3(F6.3,1X),1X,
     *                    F3.1,3X,I1,1X,3(F6.3,1X),1I8,1X,1I5)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
