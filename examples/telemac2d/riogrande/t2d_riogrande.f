
!                       *****************
                        SUBROUTINE CONDIN
!                       *****************
!
!***********************************************************************
! TELEMAC 2D VERSION 5.8      30/08/07  J-M HERVOUET TEL: 01 30 87 80 18
!
!***********************************************************************
!
!     FONCTION  : INITIALISATION DES GRANDEURS PHYSIQUES H, U, V ETC
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |                | -- |
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ITRAC,I
      DOUBLE PRECISION WLEV(NPOIN)
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DU TEMPS
!
      AT = 0.D0
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DES VITESSES : VITESSES
!
      CALL OS( 'X=C     ' , U , U , U , 0.25D0/(0.6D0*1.1D0) )
      CALL OS( 'X=0     ' , X=V )
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DE H , LA HAUTEUR D'EAU
!
      IF(CDTINI(1:10).EQ.'COTE NULLE'.OR.
     &   CDTINI(1:14).EQ.'ZERO ELEVATION') THEN
        CALL OS( 'X=0     ' , X=H )
        CALL OS( 'X=X-Y   ' , X=H , Y=ZF )
      ELSEIF(CDTINI(1:14).EQ.'COTE CONSTANTE'.OR.
     &       CDTINI(1:18).EQ.'CONSTANT ELEVATION') THEN
        CALL OS( 'X=C     ' , H , H  , H , COTINI )
        CALL OS( 'X=X-Y   ' , H , ZF , H , 0.D0   )
      ELSEIF(CDTINI(1:13).EQ.'HAUTEUR NULLE'.OR.
     &       CDTINI(1:10).EQ.'ZERO DEPTH') THEN
        CALL OS( 'X=C     ' , H , H  , H , 0.D0  )
      ELSEIF(CDTINI(1:17).EQ.'HAUTEUR CONSTANTE'.OR.
     &       CDTINI(1:14).EQ.'CONSTANT DEPTH') THEN
        CALL OS( 'X=C     ' , H , H  , H , HAUTIN )
      ELSEIF(CDTINI(1:13).EQ.'PARTICULIERES'.OR.
     &       CDTINI(1:10).EQ.'PARTICULAR'.OR.
     &       CDTINI(1:07).EQ.'SPECIAL') THEN
!  ZONE A MODIFIER
        IF(LNG.EQ.1) WRITE(LU,10)
        IF(LNG.EQ.2) WRITE(LU,11)
10      FORMAT(1X,'CONDIN : AVEC DES CONDITIONS INITIALES PARTICULIERES'
     &         ,/,'         VOUS DEVEZ MODIFIER CONDIN')
11      FORMAT(1X,'CONDIN : WITH SPECIAL INITIAL CONDITIONS'
     &         ,/,'         YOU HAVE TO MODIFY CONDIN')
        CALL PLANTE(1)
        STOP
!  FIN DE LA ZONE A MODIFIER
      ELSE
        IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'CONDIN : CONDITION INITIALE NON PREVUE : ',CDTINI
        ENDIF
        IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'CONDIN: INITIAL CONDITION UNKNOWN: ',CDTINI
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DES TRACEURS
!
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          CALL OS( 'X=C     ' , X=T%ADR(ITRAC)%P , C=TRAC0(ITRAC) )
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
! INITIALISATION DE LA VISCOSITE
!
      CALL OS( 'X=C     ' , VISC , VISC , VISC , PROPNU )
!
!-----------------------------------------------------------------------
!
      RETURN

      DO I=1,NPOIN
        WLEV(I) = 0.D0

        IF (Y(I) .GE. 8138.D0) THEN
!       Station   <20
          WLEV(I)=0.000631*(Y(I)-8138)+10.97

        ELSEIF((Y(I).LE.8138.D0).AND.(Y(I).GE.7474.D0)) THEN
!        20-40
          WLEV(I)=0.000597*(Y(I)-7474)+10.58

        ELSEIF((Y(I).LE.7474).AND.(Y(I).GE.6922.D0)) THEN
!        40-60
          WLEV(I)=0.000883*(Y(I)-6922)+10.09

        ELSEIF((Y(I).LE.6922).AND.(Y(I).GE.6378.D0)) THEN
!        60-80
          WLEV(I)=0.000672*(Y(I)-6378)+9.72
        ELSEIF((Y(I).LE.6378).AND.(Y(I).GE.5766.D0)) THEN
!        80-100
          WLEV(I)=0.000697*(Y(I)-5766)+9.30
        ELSEIF((Y(I).LE.5766).AND.(Y(I).GE.5256.D0)) THEN
!        100-120
          WLEV(I)=0.000538*(Y(I)-5256)+9.02
        ELSEIF((Y(I).LE.5256).AND.(Y(I).GE.4708.D0)) THEN
!        120-140
          WLEV(I)=0.000779*(Y(I)-4708)+8.60
        ELSEIF((Y(I).LE.4708).AND.(Y(I).GE.4064.D0)) THEN
!        140-160
          WLEV(I)=0.000521*(Y(I)-4064)+8.26
        ELSEIF(Y(I).LE.4064) THEN
!        140-160
          WLEV(I)=0.000626*(Y(I)-3042)+7.92

        ENDIF
        H%R(I)=WLEV(I)-ZF%R(I)
      ENDDO

      END
!                       *****************
                        SUBROUTINE CORFON
!                       *****************
!
!***********************************************************************
! TELEMAC 2D VERSION 5.2          01/03/90    J-M HERVOUET
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
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      DOUBLE PRECISION PI,L0,LI,LO,HDUNE,LM
      INTEGER I
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL MAS
!
!-----------------------------------------------------------------------
!
!  Bosse � t=0
!

!      Lo=1.5D0
      DO I=1,NPOIN
        ZF%R(I) = 0.D0

        IF (Y(I) .GE. 8138.D0) THEN
!         <20
          ZF%R(I)=0.000904*(Y(I)-8138)+10.26
          PRINT *,Y(I),ZF%R(I)
        ELSEIF((Y(I).LE.8138.D0).AND.(Y(I).GE.7474.D0)) THEN
!        20-40
          ZF%R(I)=0.000491*(Y(I)-7474)+9.94

        ELSEIF((Y(I).LE.7474).AND.(Y(I).GE.6922.D0)) THEN
!        40-60
          ZF%R(I)=0.000392*(Y(I)-6922)+9.72

        ELSEIF((Y(I).LE.6922).AND.(Y(I).GE.6378.D0)) THEN
!        60-80
          ZF%R(I)=0.000807*(Y(I)-6378)+9.28
        ELSEIF((Y(I).LE.6378).AND.(Y(I).GE.5766.D0)) THEN
!        80-100
          ZF%R(I)=0.00119*(Y(I)-5766)+8.55
        ELSEIF((Y(I).LE.5766).AND.(Y(I).GE.5256.D0)) THEN
!        100-120
          ZF%R(I)=0.00061*(Y(I)-5256)+8.24
        ELSEIF((Y(I).LE.5256).AND.(Y(I).GE.4708.D0)) THEN
!        120-140
          ZF%R(I)=0.00059*(Y(I)-4708)+7.92
        ELSEIF((Y(I).LE.4708).AND.(Y(I).GE.4064.D0)) THEN
!        140-160
          ZF%R(I)=0.000364*(Y(I)-4064)+7.68
        ELSEIF(Y(I).LE.4064) THEN
!        140-160
          ZF%R(I)=0.000716*(Y(I)-3042)+6.95

        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
