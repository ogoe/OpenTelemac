!                       *****************
                        SUBROUTINE CORFON
!                       *****************
!
     &(SZF, ST1, ST2, ZF, T1, T2, X, Y, PRIVE, NPOIN2,
     & LISFON, MSK, MASKEL, MATR2D, MESH2D, S)
!
!***********************************************************************
! TELEMAC-3D   V5P5         25/11/97      J.M. JANIN  (LNH) 30 87 72 84
! FORTRAN95 VERSION         MARCH 1999        JACEK A. JANKOWSKI PINXIT
!***********************************************************************
!
!  FONCTION  : CORRECTION DES FONDS RELEVES POUR TELEMAC-3D
!              (EQUIVALENT A CORFON DANS BIEF MAIS AVEC
!               DISTINCTION ENTRE DONNEES ET STRUCTURES)
!
!              EN STANDARD, CE SOUS-PROGRAMME UTILITAIRE NE FAIT
!              QUE DE LISSER LES FONDS AU PRORATA DU NOMBRE DE
!              LISSAGES FIXE DANS LE FICHIER DES PARAMETRES.
!
!              IL EST A LA DISPOSITION DES UTILISATEURS, POUR
!              LISSER SELECTIVEMENT OU CORRIGER DES FONDS SAISIS
!              PAR EXEMPLE.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! !  NOM           !MODE!                  ROLE                        !
! !________________!____!______________________________________________!
! !  (S)ZF         !<-->! FOND A MODIFIER.(SI S DEVANT : STRUCTURE)    !
! !  (S)T1,2       !<-->! TABLEAUX DE TRAVAIL (SI S DEVANT : STRUCTURE)!
! !  X,Y           ! -->! COORDONNEES DU MAILLAGE                      !
! !  PRIVE         ! -->! TABLEAU PRIVE POUR L'UTILISATEUR.            !
! !  NPOIN2        ! -->! NOMBRE DE POINTS DU MAILLAGE 2D.             !
! !  LISFON        ! -->! NOMBRE DE LISSAGES DU FOND.                  !
! !  MSK           ! -->! SI OUI, PRESENCE D'ELEMENTS MASQUES          !
! !  MASKEL        ! -->! MASQUAGE DES ELEMENTS                        !
! !  MATR          !<-->! MATRICE DE TRAVAIL                           !
! !  IMESH2        ! -->! BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE 2D   !
! !  AMESH2        ! -->! BLOC DES TABLEAUX DE REELS DU MAILLAGE 2D    !
! !________________!____!______________________________________________!
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! SOUS-PROGRAMME APPELE PAR : MITRID
! SOUS-PROGRAMMES APPELES : FILTER
!
!***********************************************************************
!
      USE BIEF

      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!------------------------------------------------------------------
!
      INTEGER, INTENT(IN) :: NPOIN2, LISFON
      LOGICAL, INTENT(IN) :: MSK
      TYPE (BIEF_OBJ), INTENT(INOUT) :: SZF, ST1, ST2
      DOUBLE PRECISION, DIMENSION(NPOIN2), INTENT(INOUT) :: ZF, T1, T2 
      DOUBLE PRECISION, DIMENSION(NPOIN2), INTENT(IN) :: X,Y 
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: PRIVE 
      TYPE (BIEF_OBJ),  INTENT(IN)    :: MASKEL
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: MATR2D
      TYPE (BIEF_MESH), INTENT(INOUT) :: MESH2D
      TYPE (BIEF_OBJ),  INTENT(IN)    :: S
!
!------------------------------------------------------------------
!
      INTEGER K,I
      LOGICAL MAS
!
!***********************************************************************
!
!  LISSAGES EVENTUELS DU FOND
!
      IF(LISFON.GT.0) THEN
!
         MAS = .TRUE.
!
         CALL FILTER(SZF,MAS,ST1,ST2,MATR2D,'MATMAS          ',
     &               1.D0,S,S,S,S,S,S,MESH2D,MSK,MASKEL,LISFON)

      ENDIF
C
C - D - CGD SOGREAH
C
C  MISE A JOUR DE LA BATHYMETRIE
C
      CALL OV( 'X=X+C     ' , ZF , ZF , ZF , -10.D0 , NPOIN2)
C
C - F - CGD SOGREAH
C
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       *****************
                        SUBROUTINE CONDIM
!                       *****************
!
!
!***********************************************************************
! TELEMAC-3D   V5P1         11/12/00      J-M HERVOUET(LNH) 30 87 80 18
!                                         F LEPEINTRE (LNH) 30 87 78 54
!                                         J-M JANIN   (LNH) 30 87 72 84
! FORTRAN95 VERSION         MARCH 1999        JACEK A. JANKOWSKI PINXIT
!***********************************************************************
!
!      FONCTION:
!      =========
!
!    INITIALISATION DES TABLEAUX DES GRANDEURS PHYSIQUES
!
!-----------------------------------------------------------------------
!
! SOUS-PROGRAMME APPELE PAR : TELEMAC-3D
! SOUS-PROGRAMMES APPELES : OV , (CALCOT)
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      INTEGER IPLAN, I,J
!
!***********************************************************************
! TIME ORIGIN
!
      AT  = 0.D0
!
! INITIALISATION DE H , LA HAUTEUR D'EAU.
!
      IF(.NOT.SUIT2) THEN
!
!     INITIALISATION OF H , THE DEPTH
!
      IF(CDTINI(1:10).EQ.'COTE NULLE'.OR.
     *   CDTINI(1:14).EQ.'ZERO ELEVATION') THEN
        CALL OS( 'X=C     ' , H   , H , H , 0.D0 )
        CALL OV( 'X=X-Y   ' , H%R , Z , Z , 0.D0 , NPOIN2 )
      ELSEIF(CDTINI(1:14).EQ.'COTE CONSTANTE'.OR.
     *       CDTINI(1:18).EQ.'CONSTANT ELEVATION') THEN
        CALL OS( 'X=C     ' , H , H , H , COTINI )
        CALL OV( 'X=X-Y   ' , H%R , Z , Z , 0.D0 , NPOIN2 )
      ELSEIF(CDTINI(1:13).EQ.'HAUTEUR NULLE'.OR.
     *       CDTINI(1:10).EQ.'ZERO DEPTH') THEN
        CALL OS( 'X=C     ' , H , H  , H , 0.D0  )
      ELSEIF(CDTINI(1:17).EQ.'HAUTEUR CONSTANTE'.OR.
     *       CDTINI(1:14).EQ.'CONSTANT DEPTH') THEN
        CALL OS( 'X=C     ' , H , H  , H , HAUTIN )
      ELSEIF(CDTINI(1:13).EQ.'PARTICULIERES'.OR.
     *       CDTINI(1:10).EQ.'PARTICULAR'.OR.
     *       CDTINI(1:07).EQ.'SPECIAL') THEN
!     ZONE A MODIFIER
!     FOR SPECIAL INITIAL CONDITIONS ON DEPTH, PROGRAM HERE                                                     
        IF(LNG.EQ.1) WRITE(LU,10)                                       
        IF(LNG.EQ.2) WRITE(LU,11)                                       
10      FORMAT(1X,'CONDIM : AVEC DES CONDITIONS INITIALES PARTICULIERES'
     *      ,/,1X,'         VOUS DEVEZ MODIFIER CONDIM')                
11      FORMAT(1X,'CONDIM : WITH SPECIAL INITIAL CONDITIONS'            
     *      ,/,1X,'         YOU HAVE TO MODIFY CONDIM')                 
        CALL PLANTE(1)                                                  
        STOP
!     END OF SPECIAL INITIAL CONDITIONS                                                            
!     FIN DE LA ZONE A MODIFIER      
      ELSE
        IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'CONDIM : CONDITION INITIALE NON PREVUE : ',CDTINI
        ENDIF
        IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'CONDIM: INITIAL CONDITION UNKNOWN: ',CDTINI
        ENDIF
        STOP
      ENDIF 
      ELSE
        IF(LNG.EQ.1) WRITE(LU,*) 'HAUTEUR LUE DANS LE FICHIER BINAIRE 1'
        IF(LNG.EQ.2) WRITE(LU,*) 'DEPTH IS READ IN THE BINARY FILE 1'
      ENDIF
!
!  CLIPPING OF H
!
      DO I=1,NPOIN2
        H%R(I)=MAX(H%R(I),HMIN)
      ENDDO
!
! INITIALISATION OF THE FREE SURFACE 
!
      CALL OS ('X=Y     ', X=HN, Y=H)

!-----------------------------------------------------------------------
!
!     INITIALISATION DE LA COTE DU PLAN INTERMEDIAIRE DE REFERENCE.
!     PAR DEFAUT, CE PLAN EST PLACE ENTRE FOND ET SURFACE AU PRORATA
!     DU PARAMETRE NPLINT.
!
      IF (NPLINT.GE.2) THEN
        CALL OV( 'X=C     ' , Z((NPLINT-1)*NPOIN2+1 : NPLINT*NPOIN2),
     *                Z, Z, COTINT , NPOIN2)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     INITIALISATION DE ZSTAR, LE RAPPORT ENTRE LA HAUTEUR D'EAU SOUS
!     UN PLAN QUASI HORIZONTAL ET LA HAUTEUR D'EAU TOTALE
!
! CAS SANS PLAN INTERMEDIAIRE DE REFERENCE
! ----------------------------------------
!
!         ON DOIT AVOIR :
!            * ZSTAR%R(1)     = 0.D0 ( PLAN DU FOND )
!            * ZSTAR%R(NPLAN) = 1.D0 ( PLAN DE LA SURFACE LIBRE )
!         ET POUR TOUT I COMPRIS ENTRE 1 ET NPLAN-1
!            * ZSTAR%R(I) < ZSTAR%R(I+1)
!
! CAS AVEC PLAN INTERMEDIAIRE DE REFERENCE
! ----------------------------------------
!
!         ON DOIT AVOIR :
!            * ZSTAR%R(1)      = -1.D0 ( PLAN DU FOND )
!            * ZSTAR%R(NPLINT) =  0.D0 ( PLAN INTERMEDIAIRE DE REFERENCE
!            * ZSTAR%R(NPLAN)  =  1.D0 ( PLAN DE LA SURFACE LIBRE )
!         ET POUR TOUT I COMPRIS ENTRE 1 ET NPLAN-1
!            * ZSTAR%R(I) < ZSTAR%R(I+1)
!
!     PAR DEFAUT, LES PLANS QUASI HORIZONTAUX SONT REGULIEREMENT ESPACES
!
!***********************************************************************
!     POUR DONNER VOTRE PROPRE REPARTITION DES PLANS, MODIFIEZ LES
!     DEUX BOUCLES SUIVANTES
!     REMARQUE : NPLINT=1 QUAND IL N'Y A PAS DE PLAN INTERMEDIAIRE
!     ATTENTION : EN CAS DE TRANSFORMATION SIGMA GENERALISEE,
!     ---------   ZSTAR(2) A ZSTAR(NPLAN-1) DOIVENT ETRE MODIFIEES
!                 ET CONTENIR LA COTE DE POSITIONNEMENT DES DIFFERENTS
!                 PLANS DU MAILLAGE (IL VA DE SOIT QUE CELLES-CI DOIVENT
!                 ETRE DONNEES DANS UN ORDRE STRICTEMENT CROISSANT).
!***********************************************************************
!
      IF (NPLINT.GE.2) THEN
        DO IPLAN = 1,NPLINT-1
          ZSTAR%R(IPLAN) = DBLE(IPLAN-NPLINT)/DBLE(NPLINT-1)
        END DO
      ENDIF
!
      DO IPLAN = NPLINT,NPLAN
        ZSTAR%R(IPLAN) = DBLE(IPLAN-NPLINT)/DBLE(NPLAN-NPLINT)
      END DO
!
C
C - D - CGD SOGREAH
C
      ZSTAR%R( 1) =   0.D0
      ZSTAR%R( 2) =   0.075D0
      ZSTAR%R( 3) =   0.15D0
      ZSTAR%R( 4) =   0.225D0
      ZSTAR%R( 5) =   0.325D0
      ZSTAR%R( 6) =   0.425D0
      ZSTAR%R( 7) =   0.525D0
      ZSTAR%R( 8) =   0.625D0
      ZSTAR%R( 9) =   0.7D0
      ZSTAR%R(10) =   0.775D0
      ZSTAR%R(11) =   0.85D0
      ZSTAR%R(12) =   0.925D0
      ZSTAR%R(13) =   0.97D0
      ZSTAR%R(14) =   0.99D0
      ZSTAR%R(15) =   1.D0
C
C - F - CGD SOGREAH
C
!
!***********************************************************************
!
! ON NE DISPOSE PAS AU DEBUT DE CE SOUS-PROG. DE Z EN TOUS LES POINTS.
! (CAR POUR CONNAITRE Z, IL FAUT CONNAITRE ZSTAR ET H).
! NEANMOINS, ON PEUT, A CETTE ETAPE DE LA ROUTINE, CALCULER Z.
! CELA PEUT SERVIR PAR EXEMPLE POUR INITIALISER VITESSES ET TRACEURS.
!
      CALL CALCOT(Z,H%R)
!
!***********************************************************************
!
!     INITIALISATION OF VELOCITIES
!
      IF(SUIT2) THEN       
        DO I=1,NPLAN
         DO J=1,NPOIN2
         U%R((I-1)*NPOIN2+J)=U2D%R(J)
         V%R((I-1)*NPOIN2+J)=V2D%R(J)
         ENDDO
        ENDDO
      ELSE
        CALL OS( 'X=0     ' , X=U )
        CALL OS( 'X=0     ' , X=V )
      ENDIF
!
      CALL OS( 'X=0     ' , X=W )
!
!-----------------------------------------------------------------------
!
!     INITIALISATION DES TRACEURS ACTIFS
!
      IF(NTRAC.NE.0) THEN
        CALL OS('X=0     ',X=TA)
      ENDIF
!
!-----------------------------------------------------------------------
!   INITIALISATION DU MODELE K-EPSILON (FACULTATIF)
!   SI VOUS LE FAITES, INDIQUEZ AKEP = .FALSE.
!
      AKEP=.TRUE.
!
!     IF(ITURBV.EQ.3) THEN
!
!       HERE INITIALISE K AND EPSILON
!
!       AKEP = .FALSE.
!     ENDIF
!
!-----------------------------------------------------------------------
!
! INITIALIZE THE PRESSURE FIELDS TO 0.0
! 
      IF(NONHYD) THEN
        CALL OS('X=C     ',X=DP,C=0.D0)
        WRITE (LU,*) 'CONDIM: DYNAMIC PRESSURE INITIALISED TO ZERO'
        CALL OS('X=C     ',X=PH,C=0.D0)
        WRITE (LU,*) '        HYDROSTATIC PRESSURE INITIALISED TO ZERO.'
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
