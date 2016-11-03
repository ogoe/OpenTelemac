!                       *****************
                        SUBROUTINE FM3SEL
!                       *****************
!
     &(X,Y,NPOIN,NBOR,NFIC,STD,NVAR,TEXTE,TEXTLU,VARCLA,NVARCL,
     & TITRE,SORLEO,NSOR,W,IKLE,
     & IKLES,ITRAV,NELEM,NPTFR,NDP,MXPTVS,MXELVS,DATE,TIME,
     & DEBU,SUIT,ECRI,LISTIN,IPARAM,IPOBO)
!
!***********************************************************************
! PROGICIEL STBTEL V5.2       02/01/96    J-M HERVOUET (LNH) 30 71 80 18
!
!***********************************************************************
!
!     COMME FMTSEL, MAIS LA DIMENSION DE SORLEO  EST
!     PARAMETREE.
!
!     FONCTIONS :  LECTURE DU FICHIER GEOMETRIQUE AU STANDARD SELAFIN
!                  ECRITURE DU FICHIER GEOMETRIQUE AU STANDARD SELAFIN
!
!     LES FONCTIONS DE CE SOUS-PROGRAMME PEUVENT ETRE PILOTEES AVEC
!     LES ARGUMENTS DEBU, SUIT, ET ECRI
!
!     ATTENTION : 1) SI DEBU, SUIT ET ECRIT SONT A .FALSE.
!                    FM3SEL LIT LA GEOMETRIE.
!
!                 2) SI DEBU ITRAV DOIT ETRE LE TABLEAU IA DES ENTIERS
!                    ET ON NE DOIT PAS SE SERVIR DE IKLE ET IKLES
!                    CAR LE SOUS-PROGRAMME DE POINTEURS N'A PAS ENCORE
!                    ETE APPELE.
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   X,Y          |<-->| COORDONNEES DU MAILLAGE.
! |   NPOIN        |<-->| NOMBRE DE POINTS DU MAILLAGE.
! |   NBOR         | -->| NUMEROTAION GLOBALE DES POINTS DE BORD.
! |   NFIC         | -->| NUMERO DE CANAL DU FICHIER A LIRE OU ECRIRE.
! |   STAND        | -->| NON UTILISE
! |   STD          | -->| BINAIRE DU FICHIER (STD, IBM, I3E)
! |   NVAR         |<-->| NOMBRE DE VARIABLES DANS LE FICHIER
! |   TEXTE        |<-->| NOMS ET UNITES DES VARIABLES.
! |   TEXTLU       |<-->| NOMS ET UNITES DES VARIABLES QU'ON VA LIRE.
! |   VARCLA       | -->| TABLEAU CONTENANT LES VARIABLES CLANDESTI-NES.
! |   NVARCL       | -->| NOMBRE DE VARIABLES CLANDESTI-NES.
! |   TITRE        |<-->| TITRE DU FICHIER.
! |   SORLEO       | -->| VARIABLES QUE L'ON SOUHAITE ECRIRE DANS LE
! |                |    | FICHIER (TABLEAU DE 26 LOGIQUES)
! |   NSOR         | -->| DIMENSION DE SOLRLEO
! |   W            | -->| TABLEAU DE TRAVAIL CONSIDERE ICI COMME REEL
! |                |    | DE TAILLE NPOIN.
! |   IKLE         |<-->| TABLE DE CONNECTIVITE (I.E. PASSAGE DE LA
! |                |    | NUMEROTATION LOCALE DES POINTS D'UN ELEMENT
! |                |    | A LA NUMEROTATION GLOBALE
! |   IKLES        | -->| TABLEAU DE TRAVAIL SERVANT A MODIFIER IKLE
! |                |    | DIMENSION NELEM * NDP
! |   ITRAV        | -->| TABLEAU DE TRAVAIL ENTIER DE DIMENSION NPOIN
! |   NELEM        |<-->| NOMBRE D'ELEMENTS DU MAILLAGE.
! |   NPTFR        |<-->| NOMBRE DE POINTS FRONTIERE DU DOMAINE.
! |   NDP          |<-->| NOMBRE DE SOMMETS PAR ELEMENT.
! |   DEBU         | -->| ON LIT UNIQUEMENT LE DEBUT DU FICHIER POUR
! |                |    | CONNAITRE LES NOMBRES DE POINTS AVEC LESQUELS
! |                |    | ON POURRA CONSTRUIRE LES POINTEURS.
! |   SUIT         | -->| ON LIT TOUTE LA PARTIE GEOMETRIE DU FICHIER
! |                |    | POUR SE PLACER SUR LES ENREGISTREMENTS DES
! |                |    | RESULTATS.
! |   ECRI         | -->| ON ECRIT LA PARTIE GEOMETRIE DU FICHIER
! |   LISTIN       | -->| ECRITURE D'INFORMATIONS SUR LISTING (OU NON)
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! PROGRAMMES APPELES : LIT , ECRIT
!
!***********************************************************************
!
!    LISTE DES ENREGISTREMENTS DU FICHIER GEOMETRIQUE:
!
!      1    : TITRE DE L'ETUDE
!      2    : NOMBRE DE FONCTIONS LUES SUR LA GRILLE 1 ET LA GRILLE 2.
!      3    : NOM ET UNITE DES VARIABLES
!      4    : 1,0,0,0,0,0,0,0,0,0
!      5    : NELEM,NPOIN,NDP,1
!      6    : IKLE
!      7    : IPOBO TABLEAU DE DIMENSION NPOIN, 0 POUR LES POINTS
!             INTERIEURS, UN NUMERO SINON.
!      8    : X
!      9    : Y
!
!    CE QUI SUIT N'EST PAS FAIT DANS FM3SEL.
!
!     10    : TEMPS
!     11    : VARIABLES DECLAREES EN 3 (DANS L'ORDRE DES DECLARATIONS)
!
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_STBTEL, EX_FM3SEL => FM3SEL
      IMPLICIT NONE
!
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*)
      REAL, INTENT(INOUT) :: W(*)
!                     IKLE(NELEM,NDP) IKLES(NDP,NELEM)
      INTEGER, INTENT(IN) :: NBOR(*)
      INTEGER, INTENT(INOUT) :: IKLE(*),IKLES(*),ITRAV(*)
      INTEGER, INTENT(INOUT) :: NPOIN,NVAR,MXPTVS,MXELVS,TIME(3),DATE(3)
      INTEGER, INTENT(IN) :: NFIC,NVARCL,NSOR
      INTEGER, INTENT(INOUT) :: NELEM,NPTFR,NDP
      INTEGER, INTENT(IN) :: IPARAM(10),IPOBO(*)
      LOGICAL, INTENT(IN) :: DEBU,SUIT,ECRI,LISTIN,SORLEO(*)
      CHARACTER(LEN=3), INTENT(IN) :: STD
      CHARACTER(LEN=72), INTENT(IN) :: TITRE
!                        NSOR      NSOR+NVARCL
      CHARACTER(LEN=32), INTENT(IN) :: TEXTE(*),VARCLA(NVARCL)
      CHARACTER(LEN=32), INTENT(INOUT) :: TEXTLU(*)
!
      DOUBLE PRECISION XBID(2)
      CHARACTER(LEN=80) TITSEL
      CHARACTER(LEN=1) CBID
      INTEGER IELEM,I,IBID(1),IB(10),ISTAT
!
!-----------------------------------------------------------------------
!
!   ON SE PLACE AU DEBUT DU FICHIER
!
      REWIND NFIC
!
!   LEC/ECR 1   : NOM DU FICHIER GEOMETRIQUE.
!
      IF(ECRI) THEN
        TITSEL = TITRE // 'SERAPHIN'
        CALL ECRI2(XBID,IBID,TITSEL,80,'CH',NFIC,STD,ISTAT)
        IF(LNG.EQ.1) WRITE(LU,*) 'TITRE :',TITSEL
        IF(LNG.EQ.2) WRITE(LU,*) 'TITLE :',TITSEL
      ELSE
        CALL LIT(XBID,W,IBID,TITRE,72,'CH',NFIC,STD,ISTAT)
      ENDIF
!
!   LEC/ECR 2   : NOMBRE DE FONCTIONS DE DISCRETISATION 1 ET 2
!
      IF(ECRI) THEN
        IB(1)=0
        IB(2)=0
        DO I=1,NSOR
          IF(SORLEO(I)) IB(1) = IB(1) + 1
        ENDDO
        IB(1) = IB(1) + NVARCL
        CALL ECRI2(XBID,IB,CBID,2,'I ',NFIC,STD,ISTAT)
      ELSE
        CALL LIT(XBID,W,IB,CBID,2,'I ',NFIC,STD,ISTAT)
      ENDIF
      NVAR =  IB(1)  +  IB(2)  -  NVARCL
!
!   LEC/ECR 3 : NOMS ET UNITES DES VARIABLES
!
      IF(NVAR.GE.1) THEN
        IF(ECRI) THEN
          DO I=1,NSOR
            IF(SORLEO(I)) THEN
             CALL ECRI2(XBID,IBID,TEXTE(I)(1:32),32,'CH',NFIC,STD,ISTAT)
            ENDIF
          ENDDO
          IF(NVARCL.NE.0) THEN
            DO I=1,NVARCL
            CALL ECRI2(XBID,IBID,VARCLA(I)(1:32),32,'CH',NFIC,STD,ISTAT)
            ENDDO
          ENDIF
        ELSE
          DO I=1,NVAR+NVARCL
            CALL LIT(XBID,W,IBID,TEXTLU(I),32,'CH',NFIC,STD,ISTAT)
          ENDDO
        ENDIF
      ENDIF
!
!   LEC/ECR 4   : LISTE DE 10 PARAMETRES ENTIERS
!
      IF(ECRI) THEN
        IB(1) = 1
        DO I = 2,10
          IB(I) = 0
        ENDDO
!   Y-A-T-IL PASSAGE DE LA DATE ?
        IF(DATE(1)+DATE(2)+DATE(3)+TIME(1)+TIME(2)+TIME(3).NE.0) THEN
          IB(10) = 1
        ENDIF
!   ECRITURE DU TABLEAU DE 10 PARAMETRES
        IF(IPARAM(8).EQ.0.AND.IPARAM(9).EQ.0) THEN
          CALL ECRI2(XBID,IB,CBID,10,'I ',NFIC,STD,ISTAT)
        ELSE
!         ON RECRIT IPARAM QUI CONTIENT DES INFORMATIONS SUR LE PARALLELISME
          CALL ECRI2(XBID,IPARAM,CBID,10,'I ',NFIC,STD,ISTAT)
        ENDIF
!   PASSAGE DE LA DATE
        IF(IB(10).EQ.1) THEN
          IB(1)=DATE(1)
          IB(2)=DATE(2)
          IB(3)=DATE(3)
          IB(4)=TIME(1)
          IB(5)=TIME(2)
          IB(6)=TIME(3)
          CALL ECRI2(XBID,IB,CBID,6,'I ',NFIC,STD,ISTAT)
        ENDIF
      ELSE
        CALL LIT(XBID,W,IB,CBID,10,'I ',NFIC,STD,ISTAT)
        IF(IB(10).EQ.1) THEN
          CALL LIT(XBID,W,IB,CBID,6,'I ',NFIC,STD,ISTAT)
          DATE(1)=IB(1)
          DATE(2)=IB(2)
          DATE(3)=IB(3)
          TIME(1)=IB(4)
          TIME(2)=IB(5)
          TIME(3)=IB(6)
        ENDIF
      ENDIF
!
!   LEC/ECR 5 : 4 ENTIERS
!
      IF(ECRI) THEN
        IB(1) = NELEM
        IB(2) = NPOIN
        IB(3) = NDP
        IB(4) = 1
        CALL ECRI2(XBID,IB,CBID,4,'I ',NFIC,STD,ISTAT)
      ELSE
        CALL LIT(XBID,W,IB,CBID,4,'I ',NFIC,STD,ISTAT)
        NELEM = IB(1)
        NPOIN = IB(2)
        NDP   = IB(3)
      ENDIF
!
!   LEC/ECR 6 : IKLE
!
      IF(DEBU) THEN
!       MODIFICATION POUR LE CALCUL DE MXPTVS ET MXELVS
!       ON LIT MAINTENANT VRAIMENT IKLES ET ON LE RANGE DANS ITRAV
!       A L'ADRESSE 1+NPOIN
        CALL LIT(XBID,W,ITRAV(1+NPOIN),
     &           CBID,NELEM*NDP,'I ',NFIC,STD,ISTAT)
      ELSEIF(SUIT) THEN
        CALL LIT(XBID,W,IB   ,CBID,    2    ,'I ',NFIC,STD,ISTAT)
      ELSEIF(ECRI) THEN
!       INVERSION DE IKLE  EN IKLES POUR SELAFIN
        DO I      = 1,NDP
        DO IELEM  = 1,NELEM
          IKLES((IELEM-1)*NDP+I) = IKLE((I-1)*NELEM+IELEM)
        ENDDO
        ENDDO
        CALL ECRI2(XBID   ,IKLES,CBID,NELEM*NDP,'I ',NFIC,STD,ISTAT)
      ELSE
        CALL LIT(XBID,W,IKLES,CBID,NELEM*NDP,'I ',NFIC,STD,ISTAT)
!       INVERSION DE IKLES EN IKLE.
        DO I      = 1,NDP
        DO IELEM  = 1,NELEM
          IKLE((I-1)*NELEM+IELEM) = IKLES((IELEM-1)*NDP+I)
        ENDDO
        ENDDO
      ENDIF
!
!   LEC/ECR 7 : IPOBO
!
      IF(DEBU) THEN
        CALL LIT(XBID,W,ITRAV,CBID,NPOIN,'I ',NFIC,STD,ISTAT)
        NPTFR = 0
        IF(NPOIN.GE.1) THEN
          DO I = 1 , NPOIN
            IF(ITRAV(I).NE.0) NPTFR = NPTFR + 1
          ENDDO
        ENDIF
!       ITRAV(1) : IPOBO  ITRAV(1+NPOIN) : IKLES
!       ITRAV(1+NPOIN+NDP*NELEM) : TABLEAU DE TRAVAIL.
        CALL MXPTEL(MXPTVS,MXELVS,ITRAV(1+NPOIN),
     &              ITRAV(1+NPOIN+NDP*NELEM),
     &              NPOIN,NELEM,NDP,ITRAV,LISTIN)
!       IPOBO EST MODIFIE PAR MXPTEL
      ELSEIF(ECRI) THEN
        IF(IPARAM(8).EQ.0.AND.IPARAM(9).EQ.0) THEN
          DO I=1,NPOIN
           ITRAV(I) = 0
          ENDDO
          DO I =1,NPTFR
           ITRAV(NBOR(I)) = I
          ENDDO
          CALL ECRI2(XBID   ,ITRAV,CBID,NPOIN,'I ',NFIC,STD,ISTAT)
        ELSE
!       PARALLELISME
          CALL ECRI2(XBID   ,IPOBO,CBID,NPOIN,'I ',NFIC,STD,ISTAT)
        ENDIF
      ELSE
        CALL LIT(XBID,W,IB,CBID,2,'I ',NFIC,STD,ISTAT)
      ENDIF
!
!   LEC/ECR  8 ET 9 : X ET Y  COORDONNEES DES POINTS DU MAILLAGE
!
      IF(DEBU.OR.SUIT) THEN
        CALL LIT(XBID,W,IBID,CBID,2    ,'R4',NFIC,STD,ISTAT)
        CALL LIT(XBID,W,IBID,CBID,2    ,'R4',NFIC,STD,ISTAT)
      ELSEIF(ECRI) THEN
        CALL ECRI2(X   ,IBID,CBID,NPOIN,'R4',NFIC,STD,ISTAT)
        CALL ECRI2(Y   ,IBID,CBID,NPOIN,'R4',NFIC,STD,ISTAT)
        IF(LNG.EQ.1) WRITE(LU,*) 'ECRITURE DE X ET Y'
        IF(LNG.EQ.2) WRITE(LU,*) 'WRITING X AND Y'
      ELSE
        CALL LIT(X   ,W,IBID,CBID,NPOIN,'R4',NFIC,STD,ISTAT)
        CALL LIT(Y   ,W,IBID,CBID,NPOIN,'R4',NFIC,STD,ISTAT)
      ENDIF
!
      IF(DEBU.AND.LISTIN) THEN
        IF(LNG.EQ.1) WRITE(LU,300) TITRE
        IF(LNG.EQ.1) WRITE(LU,500) NPTFR,NELEM,NPOIN
        IF(LNG.EQ.2) WRITE(LU,301) TITRE
        IF(LNG.EQ.2) WRITE(LU,501) NPTFR,NELEM,NPOIN
        IF(NPOIN.LT.3.OR.NPTFR.LT.3.OR.NPTFR.GE.NPOIN) THEN
          IF(LNG.EQ.1) WRITE(LU,23) NPOIN,NPTFR
          IF(LNG.EQ.2) WRITE(LU,24) NPOIN,NPTFR
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!  FORMATS D'IMPRESSION :
!
23    FORMAT(1X,'FM3SEL : NOMBRE DE POINTS DU MAILLAGE : ',1I9,/,1X,
     &          '         NOMBRE DE POINTS DE FRONTIERE: ',1I9,/,1X,
     &          '         DONNEES ERRONEES, ARRET DU PROGRAMME')
24    FORMAT(1X,'FM3SEL : NUMBER OF POINTS IN THE MESH: ',1I9,/,1X,
     &          '         NUMBER OF BOUNDARY POINTS: ',1I9,/,1X,
     &          '         WRONG DATA, PROGRAMME STOPPED')
300   FORMAT(1X,//,1X,'TITRE: ',A72,/)
301   FORMAT(1X,//,1X,'TITLE: ',A72,/)
500   FORMAT(1X,'NOMBRE DE POINTS FRONTIERE: ',1I9,/,1X,
     &'NOMBRE D''ELEMENTS:',1I9,/,1X,'NOMBRE REEL DE POINTS:',1I9)
501   FORMAT(1X,'NUMBER OF BOUNDARY POINTS: ',1I9,/,1X,
     &'NUMBER OF ELEMENTS:',1I9,/,1X,'NUMBER OF POINTS:',1I9)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
