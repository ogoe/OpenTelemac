C                       *****************
                        SUBROUTINE ELMSEC
C                       *****************
C
     *( ELPSEC, SEUSEC, TPSFIN,  X, Y, IKLE, NCOLOR, ISDRY, 
     *  IHAUT, NVAR, H, WORK, NEW, STD, NGEO )
C
C***********************************************************************
C PROGICIEL : STBTEL V5.2                     A. CABAL / P. LANG SOGREAH
C***********************************************************************
C
C     FONCTION  :  ELIMINATION DES ELEMENTS SECS DU MAILLAGE
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |   X,Y          |<-->| COORDONNEES DU MAILLAGE .
C |   IKLE         |<-->| NUMEROS GLOBAUX DES NOEUDS DE CHAQUE ELEMENT
C |   NCOLOR       |<-->| TABLEAU DES COULEURS DES POINTS DU MAILLAGE
C | ELPSEC         | -->| INDICATEUR ELIMIN. DES ELEMENTS PARTIELLEMENT SECS 
C | SEUSEC         | -->| VALEUR POUR LA DEFINITION SECHERESSE             
C | ISDRY(NELMAX)  |<-- | TAB INDICATEUR ELEMENTS SECS
C |                |    | = 1 POINT TOUJOURS SEC,
C |                |    | = 0 SOUS SEUSEC M D'EAU AU MOINS POUR 1 PAS DE TEMPS
C | IHAUT          | -->| NUM D'ORDRE DE LA VARIABLE HAUT D'EAU DANS FICH TEL2D
C | NVAR           | -->| NB DE VAR STOCKEES DANS LE FICHIER TEL2D
C | H              | -->| TABLEAU DES HAUTEURS D'EAU 
C | WORK           | -->| TABLEAU (REAL) DE TRAVAIL 
C |________________|____|______________________________________________
C | COMMON:        |    |
C |  GEO:          |   
C |    MESH        | -->| TYPE DES ELEMENTS DU MAILLAGE
C |    NDP         | -->| NOMBRE DE NOEUDS PAR ELEMENTS
C |    NPOIN       |<-->| NOMBRE TOTAL DE NOEUDS DU MAILLAGE
C |    NELEM       |<-->| NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
C |    NPMAX       | -->| DIMENSION EFFECTIVE DES TABLEAUX X ET Y
C |                |    | (NPMAX = NPOIN + 0.1*NELEM)
C |    NELMAX      | -->| DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT
C |                |    | LES ELEMENTS (NELMAX = NELEM + 0.2*NELEM)
C |  FICH:         |    |
C |    NRES        |--> | NUMERO DU CANAL DU FICHIER DE SERAFIN
C |    NGEO       |--> | NUMERO DU CANAL DU FICHIER MAILLEUR
C |    NLIM      |--> | NUMERO DU CANAL DU FICHIER DYNAM DE TELEMAC
C |    NFO1      |--> | NUMERO DU CANAL DU FICHIER TRIANGLE TRIGRID
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C----------------------------------------------------------------------
C APPELE PAR : STBTEL
C***********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      LOGICAL ELPSEC
      DOUBLE PRECISION SEUSEC
C
      INTEGER      MESH, NDP , NPOIN , NELEM , NPMAX , NELMAX
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
C
      INTEGER IKLE(NELMAX,4), ISDRY(NPMAX), NEW(NPMAX)
      INTEGER NCOLOR(NPMAX)
      INTEGER IHAUT, NVAR
C    
      DOUBLE PRECISION X(NPMAX),Y(NPMAX),H(NPMAX),TPSFIN(1)
C
      INTEGER NGEO 
C
      CHARACTER*3  STD
C
      REAL WORK(*)
C
C     VARIABLES LOCALES
C
      INTEGER I, IEL, NPDT, NPSEC, NSEC
      INTEGER J, NELI
      INTEGER IBID(1), ISTAT, NP1, NP2, NP3, ISECH
      CHARACTER*72 CBID
C
C     FONCTIONS
      LOGICAL EOF
C------------------------------------------------------------
      IF (NVAR.EQ.0) THEN
        IF (LNG.EQ.1) WRITE(LU,1012)  
        IF (LNG.EQ.2) WRITE(LU,2012)
        RETURN  
      ENDIF
      IF (IHAUT.EQ.0) THEN
        IF (LNG.EQ.1) WRITE(LU,1013)  
        IF (LNG.EQ.2) WRITE(LU,2013)
        RETURN  
      ENDIF
C     INITIALISATION DU TABLEAU ISDRY : PAS DEFAUT TOUS SECS 
      DO 5 I = 1, NPOIN
        ISDRY(I) = 1
 5    CONTINUE
C     LECTURE DES RESULTATS TELEMAC ET REMPLISSAGE DU TABLEAU ISDRY
C     -------------------------------------------------------------
      NPDT = 0
 10   CONTINUE
C     ENSEMBLE DES VARIABLES STOCKEES POUR UN PAS DE TEMPS
C     ----------------------------------------------------
      IF (EOF(NGEO)) GOTO 12
C     LECTURE DU TEMPS
C     ----------------
      CALL LIT(TPSFIN ,WORK,IBID,CBID,1,'R4',NGEO,STD,ISTAT)
      IF (EOF(NGEO)) GOTO 12
      NPDT = NPDT + 1
      NPSEC = 0
C
C     ON LIT LES VARIABLES STOCKEES AVANT LA HAUTEUR
C
      DO 15 I = 1, IHAUT -1
        CALL LIT(H ,WORK,IBID,CBID,NPOIN,'R4',NGEO,STD,ISTAT)
 15   CONTINUE
C
C     VARIABLE HAUTEUR D'EAU
C
      CALL LIT(H ,WORK,IBID,CBID,NPOIN,'R4',NGEO,STD,ISTAT)
C
C     MISE A JOUR DE ISDRY EN FONCTION DE LA HAUTEUR D'EAU DU PAS DE TEMPS
      DO 25 I = 1, NPOIN
        IF (H(I).GT.SEUSEC) THEN
          ISDRY(I) = 0
        ELSE
          NPSEC = NPSEC + 1
        ENDIF
 25   CONTINUE
      IF (LNG.EQ.1) WRITE(LU,1000) TPSFIN(1), NPSEC, SEUSEC
      IF (LNG.EQ.2) WRITE(LU,2000) TPSFIN(1), NPSEC, SEUSEC

C
C     LECTURE AUTRES VARIABLES RESTANTES
C     ----------------------------------
      DO 35 I = IHAUT +1, NVAR
        CALL LIT(H ,WORK,IBID,CBID,NPOIN,'R4',NGEO,STD,ISTAT)
 35   CONTINUE
C
      GOTO 10
 12   CONTINUE
C
C     FIN DE FICHIER ATTEINTE
C     -----------------------
C     ON RESSORT SI LE FICHIER NE CONTENAIT AUCUN PAS DE TEMPS
      IF (NPDT.EQ.0) THEN
        IF (LNG.EQ.1) WRITE(LU,1001)  
        IF (LNG.EQ.2) WRITE(LU,2001) 
        STOP  'ERREUR FATALE'
      ENDIF
C     TEST DES ELEMENTS SECS OU PARTIELLEMENTS SECS
C     ---------------------------------------------
      NPSEC = 0
      NSEC = 0

C     PARCOURS DES ELEMENTS
      DO 45 IEL = 1, NELEM
        NP1 = IKLE(IEL, 1)
        NP2 = IKLE(IEL, 2)
        NP3 = IKLE(IEL, 3)
        ISECH = ISDRY(NP1) * ISDRY(NP2) * ISDRY(NP3)
C       SI ISECH (PRODUIT) = 1 ELEMENT IEL TOUJOURS SEC
        IF (ISECH.EQ.1) THEN
C         POSITIONNE A 0 TOUS LES NUMEROS DES POINTS DE L'ELEMENT
          NSEC = NSEC + 1
          IKLE(IEL, 1) = 0
          IKLE(IEL, 2) = 0
          IKLE(IEL, 3) = 0
        ELSE
          IF (ELPSEC) THEN
C         TEST SI ELEMENT PARTIELLEMENT SEC
            ISECH =  ISDRY(NP1) + ISDRY(NP2) + ISDRY(NP3)
            IF (ISECH.GE.1) THEN
C             ELEMENT PARTIELLEMENT SEC A ELIMINER
C             POSITIONNE A 0 TOUS LES NUMEROS DES POINTS DE L'ELEMENT
              IKLE(IEL, 1) = 0
              IKLE(IEL, 2) = 0
              IKLE(IEL, 3) = 0
              NPSEC = NPSEC + 1
            ENDIF
C           FIN SI ELIMINATION PART. SECS
          ENDIF
        ENDIF
 45   CONTINUE 
C     FIN PARCOURS DE TOUS LES ELEMENTS
      IF (NSEC.EQ.0) THEN
       IF (LNG.EQ.1) WRITE(LU,1002) 
       IF (LNG.EQ.2) WRITE(LU,2002)  
      ELSE IF (NSEC.EQ.1) THEN
       IF (LNG.EQ.1) WRITE(LU,1003)  
       IF (LNG.EQ.2) WRITE(LU,2003) 
      ELSE
       IF (LNG.EQ.1) WRITE(LU,1004) NSEC
       IF (LNG.EQ.2) WRITE(LU,2004) NSEC
      ENDIF
      
      IF (ELPSEC) THEN 
        IF (NPSEC.EQ.0) THEN
         IF (LNG.EQ.1) WRITE(LU,1005) 
         IF (LNG.EQ.2) WRITE(LU,2005)  
        ELSE IF (NPSEC.EQ.1) THEN
         IF (LNG.EQ.1) WRITE(LU,1006)  
         IF (LNG.EQ.2) WRITE(LU,2006) 
        ELSE
         IF (LNG.EQ.1) WRITE(LU,1007) NPSEC
         IF (LNG.EQ.2) WRITE(LU,2007) NPSEC
        ENDIF
      ENDIF

C     S'IL N'Y A PAS D'ELEMENTS SECS OU P.SECS ON S'EN VA 
      IF ((NSEC.EQ.0) .AND. (NPSEC.EQ.0)) RETURN 
      
C     ELIMINATION DES ELEMENTS SECS ET PARTIELLLEMENT SECS     
C     ---------------------------------------------
      NELI = 0
      IEL = 1
C     POUR CHAQUE ELEMENT FAIRE       
 20   CONTINUE     
        IF ((IKLE(IEL, 1).EQ.0).AND.(IKLE(IEL, 2).EQ.0).AND.
     +     (IKLE(IEL, 3).EQ.0)) THEN
         NELI = NELI + 1              
         DO 48 I = IEL, NELEM - NELI
           IKLE(I,1) = IKLE(I+1, 1)
           IKLE(I,2) = IKLE(I+1, 2)
           IKLE(I,3) = IKLE(I+1, 3)
 48      CONTINUE
        ELSE
         IEL = IEL + 1
        ENDIF
      IF (IEL .LE. NELEM-NELI) GOTO 20
C     FIN POUR CHAQUE ELEMENT

      IF (NELI .LE. 0) THEN
         IF (LNG.EQ.1) WRITE(LU,1008)  
         IF (LNG.EQ.2) WRITE(LU,2008) 
      ELSE
         IF (LNG.EQ.1) WRITE(LU,1009) NELI 
         IF (LNG.EQ.2) WRITE(LU,2009) NELI
      ENDIF

      NELEM = NELEM - NELI

C      ELIMINATION DES POINTS NE FAISANT PLUS PARTIE DU MAILLAGE
C      REUTILISATION DE ISDRY POUR MARQUER LES POINTS NON UTILISEES
C      ---------------------------------------------
       DO 65 I = 1, NPOIN
         ISDRY(I) = 0
         NEW(I) = 0
 65    CONTINUE 
       
       DO 75 IEL = 1, NELEM
        ISDRY(IKLE(IEL,1)) = IKLE(IEL,1)
        ISDRY(IKLE(IEL,2)) = IKLE(IEL,2)
        ISDRY(IKLE(IEL,3)) = IKLE(IEL,3)
 75    CONTINUE
       
       NELI = 0
       I = 1
C      POUR CHAQUE POINT FAIRE 
       DO 85 I = 1, NPOIN
         IF (ISDRY(I) .EQ.0) THEN
           NELI = NELI + 1
           NEW(I) = 0
         ELSE
           NEW(I) = I - NELI
         ENDIF
 85    CONTINUE
C      FIN POUR CHAQUE POINT         
C
       NELI = 0
       I = 1
C      POUR CHAQUE POINT FAIRE 
 30    CONTINUE       
         IF (ISDRY(I).EQ.0) THEN
C          POINT I  A ELIMINER
C      WRITE(LU,*) 'POINT A ELIMINER',I,':',X(I),Y(I),NCOLOR(I)
           NELI = NELI + 1
C          DECALAGE DANS LE TABLEAU DES POINTS
           DO 95 J = I, NPOIN - NELI 
             X(J) = X(J+1)
             Y(J) = Y(J+1)
             NCOLOR(J) = NCOLOR(J+1)
             IF (ISDRY(J+1).GT.0) THEN
               ISDRY(J) = ISDRY(J+1) - 1
             ELSE
               ISDRY(J) = 0
             ENDIF
 95        CONTINUE
         ELSE
           I = I + 1
         ENDIF
       IF (I .LE. NPOIN - NELI) GOTO 30  
C      FIN POUR CHAQUE POINT         
       IF (NELI .LE. 0) THEN
         IF (LNG.EQ.1) WRITE(LU,1010)  
         IF (LNG.EQ.2) WRITE(LU,2010) 
       ELSE
         IF (LNG.EQ.1) WRITE(LU,1011) NELI 
         IF (LNG.EQ.2) WRITE(LU,2011) NELI
       ENDIF
       NPOIN = NPOIN - NELI

C      ON REPERCUTE LA RENUMEROTATION DANS IKLE
C      ----------------------------------------   
       DO 115 IEL = 1, NELEM
         J = IKLE(IEL,1)
         IKLE(IEL,1) = NEW(J)
         J = IKLE(IEL,2)
         IKLE(IEL,2) = NEW(J)
         J = IKLE(IEL,3)
         IKLE(IEL,3) = NEW(J)
 115   CONTINUE
      RETURN
C***********************************************************************
 1000 FORMAT(1X,'TEMPS ',G15.3,' : ',I8,
     +' POINT(S) AVEC HAUTEUR D''EAU EN DESSOUS DE',G15.3)
 2000 FORMAT(1X,'TIME ',G15.3,' : ',I8,
     +' POINT(S) WITH WATER DEPTH BELOW',G15.3)

 1001 FORMAT(/,1X,'DESOLE LE FICHIER UNIVERSEL NE CONTIENT PAS DE ',
     + /,1X,'RESULTATS DE SIMULATION.',
     + /,1X,'DETERMINATION DES ELEMENTS SECS IMPOSSIBLE !')
 2001 FORMAT(/,1X,'SORRY, THE UNIVERSAL FILE DOES NOT CONTAIN',
     + /,1X,'ANY COMPUTATION RESULTS.',
     + /,1X,'FINDING OUT DRY ELEMENTS IS IMPOSSIBLE !')

 1002 FORMAT(1X,'AUCUN ELEMENT COMPLETEMENT SEC TROUVE ',
     + /,1X,'DANS LE MAILLAGE.')
 2002 FORMAT(1X,'NO COMPLETELY DRY ELEMENT IN THE MESH.')

 1003 FORMAT(1X,'UN SEUL ELEMENT COMPLETEMENT SEC ',
     + /,1X,'TROUVE DANS LE MAILLAGE.')
 2003 FORMAT(1X,'ONLY ONE COMPLETELY DRY ELEMENT FOUND',
     + /,1X,'IN THE MESH.')

 1004 FORMAT(1X,'ELEMENTS COMPLETEMENT SECS TROUVES',
     + 1X,'DANS LE MAILLAGE : ',I8)
 2004 FORMAT(1X,'COMPLETELY DRY ELEMENTS IN THE MESH: ',I8)

 1005 FORMAT(1X,'AUCUN ELEMENT PARTIELLEMENT SEC DANS ',
     + /,1X,'LE MAILLAGE.')
 2005 FORMAT(1X,'NO PARTIALLY DRY ELEMENT IN THE MESH.')

 1006 FORMAT(1X,'UN SEUL ELEMENT PARTIELLEMENT SEC DANS ',
     + /,1X,'LE MAILLAGE.')
 2006 FORMAT(1X,'ONLY ONE PARTIALLY DRY ELEMENT IN THE MESH.')

 1007 FORMAT(1X,'ELEMENTS PARTIELLEMENT SECS TROUVES ',
     +'DANS LE MAILLAGE :',I8)
 2007 FORMAT(1X,'PARTIALLY DRY ELEMENTS IN THE MESH:',I8)

 1008 FORMAT(1X,'AUCUN ELEMENT N''A ETE SUPPRIME DU MAILLAGE.')
 2008 FORMAT(1X,'NO ELEMENT HAS BEEN CANCELLED IN THE MESH.')

 1009 FORMAT(1X,'ELEMENTS SUPPRIMES DU MAILLAGE :',I8)
 2009 FORMAT(1X,'ELEMENTS CANCELLED IN THE MESH:',I8)

 1010 FORMAT(1X,'AUCUN POINT N''A ETE SUPPRIME DU MAILLAGE.')
 2010 FORMAT(1X,'NO POINT HAS BEEN CANCELLED IN THE MESH.')

 1011 FORMAT(1X,'POINTS SUPPRIMES DU MAILLAGE :  ',I8)
 2011 FORMAT(1X,'POINTS CANCELLED IN THE MESH:  ',I8)

 1012 FORMAT(/,1X,'AUCUNE VARIABLE N''EST STOCKEE DANS LE FICHIER',
     +/,1X,'ELIMINATION DES ELEMENTS SECS IMPOSSIBLE.')
 2012 FORMAT(/,1X,'NO VARIABLE STORED ON THE FILE. ',
     + /,1X,'DRY ELEMENT SUPPRESSION IS IMPOSSIBLE.')

 1013 FORMAT(/,1X,'LA VARIABLE HAUTEUR D''EAU NE SEMBLE PAS ETRE',
     + /,1X,'STOCKEE DANS LE FICHIER.',
     + /,1X,'ELIMINATION DES ELEMENTS SECS IMPOSSIBLE.')
 2013 FORMAT(/,1X,'THE WATER DEPTH VARIABLE IS NOT STORED ON THE FILE',
     + /,1X,'DRY ELEMENT SUPPRESSION IS IMPOSSIBLE.')
      END
