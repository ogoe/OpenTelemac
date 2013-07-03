!                       *****************
                        SUBROUTINE CONDIM
!                       *****************
! 
!
!***********************************************************************
! TELEMAC 3D VERSION 5.1    11/12/00      J-M HERVOUET(LNH) 30 87 80 18
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
      DOUBLE PRECISION AL
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
      CALL OS ('X=Y     ',X=HN,Y=H)
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
C
C     EXAMPLE 2: SIGMA TRANSFORMATION WITH GIVEN PROPORTIONS
C
      DO IPLAN = 1,NPLAN
        TRANSF_PLANE%I(IPLAN)=2
      ENDDO
C
! CV : 3/08/2009: maillage à pas variable
      AL=1.3D0
      DO IPLAN = 1,NPLAN
        ZSTAR%R(IPLAN) = (1.D0-AL**(IPLAN-1))/(1.D0-AL**(NPLAN-1))
      END DO
!
!***********************************************************************
!
!     COMPUTATION OF ELEVATIONS
!     IF IT IS A CONTINUATION, WILL BE DONE AFTER CALLING 'SUITE'
!
      IF(DEBU) CALL CALCOT(Z,H%R)
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
! CV: initialisation vitesses comme dans Tel2d      
        CALL OS( 'X=C     ' , U , U , U , 0.25D0/(0.6D0*1.1D0))
        CALL OS( 'X=C     ' , V , V , V , 0.D0 )
      ENDIF
!
      CALL OS( 'X=C     ' , W , W , W , 0.D0 )
!
!-----------------------------------------------------------------------
!
!     TRACERS INITIALIZATION : voir resultats du 2D
!
       IF(NTRAC.GT.0) THEN
         DO I=1,NTRAC
           CALL OS( 'X=C     ', X=TA%ADR(I)%P, C=TRAC0(I))
         ENDDO
       ENDIF
!!
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
! INITIALIZE THE HYDRODYNAMIC PRESSURE FIELD TO 0.0
! (PROJECTION2: IT MAY BE APPROPRIATE TO SOLVE A POISSON EQUATION FOR DP
!
      IF(NONHYD) THEN
        CALL OS('X=C     ', DP, DP, DP, 0.D0)
           WRITE (LU,*) 'CONDIM: DYNAMIC PRESSURE INITIALISED TO ZERO'
        CALL PHSTAT                       
     *       (PH%R,DELTAR%R,Z, T3_01%R, T3_02%R, RHO0, GRAV,          
     *                      NPOIN3, NPOIN2, NPLAN, PRIVE )
!!!        WRITE (LU,*) 'CONDIM: HYDROSTATIC PRESSURE INITIALISED.'
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       *****************
                        SUBROUTINE CORFON
!                       *****************
!
     &(SZF, ST1, ST2, ZF, T1, T2, X, Y, PRIVE, NPOIN2,
     & LISFON, MSK, MASKEL, MATR2D, MESH2D, S)
!
!***********************************************************************
! TELEMAC 3D VERSION 5.1    25/11/97      J.M. JANIN  (LNH) 30 87 72 84
! FORTRAN95 VERSION         MARCH 1999        JACEK A. JANKOWSKI PINXIT
!***********************************************************************
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
! cas test de la	 bosse
      DOUBLE PRECISION  PI
!------------------------------------------------------------------
      INTEGER K,I
      LOGICAL MAS
      DOUBLE PRECISION J !PENTE HYDRAULIQUE
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
!-----------------------------------------------------------------------
!
C  Bosse à t=0
C
      PI=3.141592653589793D0 
      DO 2 I=1,NPOIN2                                                            
        ZF(I) = 0.D0
        IF (MESH2D%X%R(I).GE.2.D0.AND.MESH2D%X%R(I).LE.10.D0) THEN                        
           ZF(I)=.1D0*DSIN(PI*(MESH2D%X%R(I)-2.D0)/8.D0)**2                                                                          
        END IF                                                                                                                        
2     CONTINUE
!
!	 
      RETURN
      END SUBROUTINE CORFON




