!                    ****************
                     SUBROUTINE METEO
!                    ****************
!
     &(PATMOS,WINDX,WINDY,FUAIR,FVAIR,X,Y,AT,LT,NPOIN,VENT,ATMOS,
     & HN,TRA01,GRAV,ROEAU,NORD,PRIVE,FO1,FILES,LISTIN)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES ATMOSPHERIC PRESSURE AND WIND VELOCITY FIELDS
!+               (IN GENERAL FROM INPUT DATA FILES).
!
!warning  CAN BE ADAPTED BY USER
!
!history  J-M HERVOUET (LNHE)
!+        02/01/2004
!+        V5P4
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        30/01/2013
!+        V6P3
!+   Now 2 options with an example for reading a file. 
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT,LT          |-->| TIME, ITERATION NUMBER
!| ATMOS          |-->| YES IF PRESSURE TAKEN INTO ACCOUNT
!| FUAIR          |-->| VELOCITY OF WIND ALONG X, IF CONSTANT
!| FVAIR          |-->| VELOCITY OF WIND ALONG Y, IF CONSTANT
!| GRAV           |-->| GRAVITY ACCELERATION
!| HN             |-->| DEPTH
!| NORD           |-->| DIRECTION OF NORTH, COUNTER-CLOCK-WISE
!|                |   | STARTING FROM VERTICAL AXIS
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| PATMOS         |<--| ATMOSPHERIC PRESSURE
!| PRIVE          |-->| USER WORKING ARRAYS (BIEF_OBJ BLOCK)
!| ROEAU          |-->| WATER DENSITY
!| TRA01          |-->| WORKING ARRAY
!| VENT           |-->| YES IF WIND TAKEN INTO ACCOUNT
!| WINDX          |<--| FIRST COMPONENT OF WIND VELOCITY
!| WINDY          |<--| SECOND COMPONENT OF WIND VELOCITY
!| X              |-->| ABSCISSAE OF POINTS
!| Y              |-->| ORDINATES OF POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: LT,NPOIN,FO1
      LOGICAL, INTENT(IN)             :: ATMOS,VENT,LISTIN
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN),HN(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: WINDX(NPOIN),WINDY(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: PATMOS(NPOIN),TRA01(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FUAIR,FVAIR,AT,GRAV,ROEAU,NORD
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: PRIVE
      TYPE(BIEF_FILE), INTENT(IN)     :: FILES(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER MY_OPTION,UL
      DOUBLE PRECISION P0,Z(1),AT1,AT2,FUAIR1,FUAIR2,FVAIR1,FVAIR2,COEF
      DOUBLE PRECISION UAIR,VAIR
!
!-----------------------------------------------------------------------
!
!     DATA THAT YOU DECLARE AND READ HERE ONCE IN A FILE MAY HAVE TO BE
!     KEPT BECAUSE THIS SUBROUTINE IS CALLED AT EVERY TIME STEP.
!     WITHOUT THE SAVE COMMAND, ALL LOCAL DATA ARE FORGOTTEN IN THE NEXT
!     CALL.
!
      SAVE
!
!-----------------------------------------------------------------------
!
!     CHOOSE YOUR OPTION !!!
!
!     1: CONSTANTS GIVEN BY THE KEYWORDS:
!        AIR PRESSURE (GIVEN HERE AS P0, NO KEYWORD)
!        WIND VELOCITY ALONG X (HERE FUAIR)
!        WIND VELOCITY ALONG Y (HERE FVAIR)
!        THEY WILL BE SET ONCE FOR ALL BEFORE THE FIRST ITERATION (LT=0)
!
!     2: CONSTANT IN SPACE WIND COMPONENTS OF VELOCITY GIVEN IN THE FILE
!        FO1_WIND DECLARED AS FORMATTED DATA FILE 1 = FO1_WIND 
!
      MY_OPTION = 2
!
!-----------------------------------------------------------------------
!
!     BEWARE, HERE ONLY ONE COMPUTATION AT FIRST TIMESTEP
!
      IF(LT.EQ.0) THEN
!
        UL=FILES(FO1)%LU
!
!-----------------------------------------------------------------------
!
!       ATMOSPHERIC PRESSURE
!
        IF(ATMOS) THEN
          P0 = 100000.D0
          CALL OV( 'X=C     ' , PATMOS , Y , Z , P0 , NPOIN )
        ENDIF
!
!-----------------------------------------------------------------------
!
!       WIND : IN THIS CASE THE WIND IS CONSTANT,
!              VALUE GIVEN IN STEERING FILE.
!
!       MAY REQUIRE A ROTATION,
!       DEPENDING ON THE SYSTEM IN WHICH THE WIND VELOCITY WAS SUPPLIED
!
        IF(VENT) THEN
          CALL OV( 'X=C     ' , WINDX , Y , Z , FUAIR , NPOIN )
          CALL OV( 'X=C     ' , WINDY , Y , Z , FVAIR , NPOIN )
        ENDIF
!
        IF(MY_OPTION.EQ.2) THEN
!         JUMPING TWO LINES OF COMMENTS
          READ(UL,*,ERR=100,END=200)
          READ(UL,*,ERR=100,END=200)
!         READING THE FIRST TWO LINES OF DATA
          READ(UL,*,ERR=100,END=200) AT1,FUAIR1,FVAIR1
          READ(UL,*,ERR=100,END=200) AT2,FUAIR2,FVAIR2
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(MY_OPTION.EQ.2.AND.VENT) THEN
!
!       JUMPING TWO LINES OF COMMENTS
!
10      CONTINUE
        IF(AT.GE.AT1.AND.AT.LT.AT2) THEN
          IF(AT2-AT1.GT.1.D-6) THEN
            COEF=(AT-AT1)/(AT2-AT1)
          ELSE
            COEF=0.D0
          ENDIF
          UAIR=FUAIR1+COEF*(FUAIR2-FUAIR1)
          VAIR=FVAIR1+COEF*(FVAIR2-FVAIR1)
          IF(LISTIN) THEN
            IF(LNG.EQ.1) WRITE(LU,*) 'VENT A T=',AT,' UAIR=',UAIR,
     &                                              ' VAIR=',VAIR
            IF(LNG.EQ.2) WRITE(LU,*) 'WIND AT T=',AT,' UAIR=',UAIR,
     &                                               ' VAIR=',VAIR
          ENDIF
        ELSE
          AT1=AT2
          FUAIR1=FUAIR2
          FVAIR1=FVAIR2
          READ(UL,*,ERR=100,END=200) AT2,FUAIR2,FVAIR2
          GO TO 10
        ENDIF
!
        CALL OV('X=C     ',WINDX,Y,Z,UAIR,NPOIN)
        CALL OV('X=C     ',WINDY,Y,Z,VAIR,NPOIN)    
!
      ENDIF
!
      RETURN
!
!-----------------------------------------------------------------------
! 
100   CONTINUE
      WRITE(LU,*) ' '
      WRITE(LU,*) 'METEO'
      IF(LNG.EQ.1) WRITE(LU,*) 'ERREUR DANS LE FICHIER DE VENT'
      IF(LNG.EQ.2) WRITE(LU,*) 'ERROR IN THE WIND FILE'
      CALL PLANTE(1)
      STOP  
200   CONTINUE
      WRITE(LU,*) ' '
      WRITE(LU,*) 'METEO'
      IF(LNG.EQ.1) WRITE(LU,*) 'FIN PREMATUREE DU FICHIER DE VENT'
      IF(LNG.EQ.2) WRITE(LU,*) 'WIND FILE TOO SHORT'
      CALL PLANTE(1)
      STOP           
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       *****************
                        SUBROUTINE CONDIN
!                       *****************
!
!***********************************************************************
! TELEMAC-2D VERSION 5.0         19/08/98  J-M HERVOUET TEL: 30 87 80 18
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
      DOUBLE PRECISION FAIR1 , WIND , FVENT , HINI , LCANAL
      COMMON/FORFUN/FVENT,LCANAL,HINI
      INTEGER I,ITRAC                                     
!                                                                         
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!  
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DU TEMPS
!
      AT = 0.D0
!
!   INITIALISATION DU TEMPS                                               
!                                                                         
!                                                                         
      FAIR1   = 1.2615D-3
      WIND   = 5.D0
      FVENT  = FAIR1*WIND*WIND
      HINI   = -ZF%R(1)
      LCANAL = 500.D0
!-----------------------------------------------------------------------
!
!   INITIALISATION DES VITESSES : VITESSES NULLES
!
      CALL OS( 'X=C     ' , U , U , U , 0.D0 )
      CALL OS( 'X=C     ' , V , V , V , 0.D0 )
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DE H , LA HAUTEUR D'EAU
!
      IF(CDTINI(1:10).EQ.'COTE NULLE'.OR.
     &   CDTINI(1:14).EQ.'ZERO ELEVATION') THEN
        CALL OS( 'X=C     ' , H , H  , H , 0.D0 )
        CALL OS( 'X=X-Y   ' , H , ZF , H , 0.D0 )
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
      CALL EXACTE(H%R,X,Y,NPOIN,ZF%R)
        DO I = 1,NPOIN
          PRIVE%ADR(1)%P%R(I) = H%R(I)
        ENDDO                                         
!  FIN DE LA ZONE A MODIFIER      
      ELSE
        IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'CONDIN : CONDITION INITIALE NON PREVUE : ',CDTINI
        ENDIF
        IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'CONDIN: INITIAL CONDITION UNKNOWN: ',CDTINI
        ENDIF
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DU TRACEUR
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
      END           
!                       *****************                                 
                        SUBROUTINE EXACTE
!                       *****************                                 
!                                                                         
     &(FEXA,X,Y,NPOIN,ZF)
!                                                                         
!***********************************************************************  
! PROGICIEL : EX-PROGRAMME DE F. LEPEINTRE                                
!***********************************************************************  
!                                                                         
!     FONCTION:                                                           
!     =========                                                           
!                                                                         
!-----------------------------------------------------------------------  
!                             ARGUMENTS                                   
! .________________.____.______________________________________________.  
! |      NOM       |MODE|                   ROLE                       |  
! |________________|____|______________________________________________|  
! |                |    |                                              |  
! |________________|____|______________________________________________|  
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)   
!**********************************************************************   
!                                                                         
      IMPLICIT NONE
!                                                                         
      INTEGER NPOIN,I
!                                                    
      DOUBLE PRECISION FEXA(NPOIN),LCANAL,HINI,H0,GRAV,FVENT
      DOUBLE PRECISION X(NPOIN),Y(NPOIN),ZF(NPOIN)
!                                                                         
      EXTERNAL FUNC
      INTRINSIC SQRT
      DOUBLE PRECISION FUNC
!                                                                         
      COMMON/FORFUN/FVENT,LCANAL,HINI
!                                                                         
!-----------------------------------------------------------------------  
!                                                                         
!     HAUTEUR D'EAU EN X=0 A L'EQUILIBRE (POUR LA SOLUTION ANALYTIQUE)    
!                                                                         
      GRAV = 9.81D0
      H0 = HINI
      CALL ZBRENT(FUNC,1.D-6,0.D0,H0,100)
!                                                                         
!     CALCUL DE LA SOLUTION EXACTE                                        
!                                                                         
      DO I = 1 , NPOIN                                                 
!                                                                         
        FEXA(I) = SQRT( 2.D0*FVENT * X(I) / GRAV + H0*H0)
!                                                                         
      ENDDO                                                            
!                                                                         
!-----------------------------------------------------------------------  
!                                                                         
      RETURN
      END
!                       ******************************                    
                        DOUBLE PRECISION FUNCTION FUNC
!                       ******************************                    
!                                                                         
     &(X)
!                                                                         
!***********************************************************************  
! PROGICIEL : MITHRIDATE     01/06/90    PAINTER (LNH) 30 87 78 54        
!***********************************************************************  
!                                                                         
!     FONCTION:                                                           
!     =========                                                           
!                                                                         
!     FONCTION DONT LE ZERO CORRESPOND A LA HAUTEUR EN X=0                
!     QUAND LA SURFACE LIBRE EQUILIBRE LA FORCE DUE AU VENT               
!                                                                         
!-----------------------------------------------------------------------  
!                             ARGUMENTS                                   
! .________________.____.______________________________________________.  
! |      NOM       |MODE|                   ROLE                       |  
! |________________|____|______________________________________________|  
! |                |    |                                              |  
! |________________|____|______________________________________________|  
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)   
!----------------------------------------------------------------------   
!                                                                         
      DOUBLE PRECISION A1,A2,LCANAL,X,FVENT,HINI,GRAV
!                                                                         
      COMMON/FORFUN/FVENT,LCANAL,HINI
!                                                                         
!----------------------------------------------------------------------   
!                                                                         
      GRAV = 9.81D0
      A1 = 2.D0 * FVENT * LCANAL / GRAV
      A2 = 3.D0 * FVENT * HINI * LCANAL / GRAV
      FUNC =  (A1+X*X)**1.5D0 - X**3 - A2
!                                                                         
!-----------------------------------------------------------------------  
!                                                                         
      RETURN
      END
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
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
      INTEGER I
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL MAS
!
!-----------------------------------------------------------------------
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
      DO I=1,NPOIN
        ZF%R(I) = -2.D0
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END                  
       
!                       ***************************
                        SUBROUTINE PRERES_TELEMAC2D
!                       ***************************
!
!***********************************************************************
!  TELEMAC 2D VERSION 5.0    17/08/94    J-M HERVOUET (LNH) 30 87 80 18
!
!***********************************************************************
!
!     FONCTION  : PREPARATION DE VARIABLES QUI SERONT ECRITES SUR
!                 LE FICHIER DE RESULTATS OU SUR LE LISTING.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |      LT        | -->| NUMERO D'ITERATION
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
!  APPELE PAR : TELMAC
!
!  SOUS-PROGRAMME APPELE : OV
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
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     
      LOGICAL IMP,LEO
!
      INTEGER LTT,N,IMAX
!
      DOUBLE PRECISION HHPLG,XMAX
!
      INTRINSIC MAX,SQRT
!
!-----------------------------------------------------------------------
!
! LOGIQUES POUR DECIDER DES SORTIES
!
      IMP=.FALSE.
      LEO=.FALSE.
      LTT=(LT/LISPRD)*LISPRD
      IF((LT.EQ.LTT.OR.LT.EQ.NIT).AND.LT.GE.PTINIL) IMP=.TRUE.
      LTT=(LT/LEOPRD)*LEOPRD
      IF((LT.EQ.LTT.OR.LT.EQ.NIT).AND.LT.GE.PTINIG) LEO=.TRUE.
!
!     PAS D'IMPRESSION, PAS DE SORTIE SUR FICHIER, ON RESSORT
      IF(.NOT.(LEO.OR.IMP)) GO TO 1000
!
!
!=======================================================================
! CALCUL DE LA CELERITE (MISE DANS FU, VOIR LE BLOC VARSOR)
!=======================================================================
!
      IF((LEO.AND.SORLEO(3)).OR.(IMP.AND.SORIMP(3))) THEN
        DO N=1,NPOIN
          FU%R(N) = SQRT ( GRAV * MAX(H%R(N),0.D0) )
        ENDDO
      ENDIF
!
!=======================================================================
! CALCUL DE LA SURFACE LIBRE (= H + ZF, MISE DANS FV)
!=======================================================================
!
      IF((LEO.AND.SORLEO(5)).OR.(IMP.AND.SORIMP(5))) THEN
        CALL OS( 'X=Y+Z   ' , FV , H , ZF , 0.D0 )
      ENDIF
!
!=======================================================================
! CALCUL DU NOMBRE DE FROUDE
!=======================================================================
!
      IF((LEO.AND.SORLEO(7)).OR.(IMP.AND.SORIMP(7))) THEN
        DO N=1,NPOIN
          HHPLG = MAX( H%R(N) , 1.D-8 )
          T2%R(N) = SQRT (( U%R(N)**2 + V%R(N)**2 ) / ( HHPLG*GRAV ))
        ENDDO
      ENDIF
!
!=======================================================================
! CALCUL DU DEBIT SCALAIRE
!=======================================================================
!
      IF((LEO.AND.SORLEO(8)).OR.(IMP.AND.SORIMP(8))) THEN
        DO N=1,NPOIN
          T3%R(N) = SQRT (U%R(N)**2 + V%R(N)**2) * H%R(N)
        ENDDO
      ENDIF
!
!=======================================================================
! CALCUL DU DEBIT VECTORIEL , COMPOSANTE SUIVANT X
!=======================================================================
!
      IF((LEO.AND.SORLEO(13)).OR.(IMP.AND.SORIMP(13))) THEN
        CALL OS( 'X=YZ    ' , T4 , H , U , HHPLG )
      ENDIF
!
!=======================================================================
! CALCUL DU DEBIT VECTORIEL , COMPOSANTE SUIVANT Y
!=======================================================================
!
      IF((LEO.AND.SORLEO(14)).OR.(IMP.AND.SORIMP(14))) THEN
        CALL OS( 'X=YZ    ' , T5 , H , V , HHPLG )
      ENDIF
!
!=======================================================================
! CALCUL DE LA VITESSE SCALAIRE
!=======================================================================
!
      IF((LEO.AND.SORLEO(15)).OR.(IMP.AND.SORIMP(15))) THEN
        CALL OS( 'X=N(Y,Z)' , T6 , U , V , HHPLG )
      ENDIF
!
!=======================================================================
! CALCUL DU NOMBRE DE COURANT
!=======================================================================
!
      IF((LEO.AND.SORLEO(22)).OR.(IMP.AND.SORIMP(22))) THEN
!                             IELM
        CALL CFLPSI(T9,U,V,DT,11,MESH,MSK,MASKEL)
        CALL MAXI(XMAX,IMAX,T9%R,NPOIN)
        IF (LNG.EQ.1) WRITE(LU,78) XMAX
        IF (LNG.EQ.2) WRITE(LU,79) XMAX
78      FORMAT(1X,'PRERES : NOMBRE DE COURANT MAXIMUM :',G16.7)
79      FORMAT(1X,'PRERES: MAXIMUM COURANT NUMBER: ',G16.7)
      ENDIF
!
!=======================================================================
!
1000  CONTINUE
      RETURN
      END 
!                       *****************                               
                        SUBROUTINE ZBRENT                               
!                       *****************                               
!                                                                       
     &(FC1,EPS,X1,X2,ITMAX)                                             
!                                                                       
!***********************************************************************
! BIEF VERSION 3.0           18/08/94    J-M HERVOUET (LNH) 30 87 80 18 
!                                                                       
!***********************************************************************
!                                                                       
!  FONCTION  :  SOLUTION D'UNE EQUATION DONT UN ZERO UNIQUE EST ENTRE   
!               LES POINTS X1 ET X2.                                    
!                                                                       
!-----------------------------------------------------------------------
!                             ARGUMENTS                                 
! .________________.____.______________________________________________ 
! |      NOM       |MODE|                   ROLE                        
! |________________|____|______________________________________________ 
! |   FC1          | -->| FONCTION DONT ON CHERCHE LE ZERO              
! |                |    | DOIT ETRE DEFINIE EN DOUBLE PRECISION         
! |                |    | PAR AILLEURS.                                 
! |   EPS          | -->| PRECISION CHERCHEE.                           
! |   X1,X2        | -->| ENCADREMENT DE LA SOLUTION ENTREE             
! |                |<-->| X2 = SOLUTION EN SORTIE.                      
! |   ITMAX        | -->| NOMBRE MAXIMUM D'ITERATIONS.                  
! |________________|____|______________________________________________ 
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE) 
!-----------------------------------------------------------------------
!                                                                       
!  FONCTION APPELEE : FC1                                               
!                                                                       
!***********************************************************************
!                                                                       
      IMPLICIT NONE                                                     
      INTEGER LNG,LU                                                    
      COMMON/INFO/LNG,LU                                                
!                                                                       
      DOUBLE PRECISION AA,B,C,D,E,X1,X2,FA,FB,FC,EPS,EPS2,XM,S,P,Q,R
!                                                                       
      INTEGER ITMAX,ITER
!                                                                       
      DOUBLE PRECISION FC1
      EXTERNAL FC1                                                      
!                                                                       
      INTRINSIC ABS,SIGN,MIN                                            
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!  ON VERIFIE QU'ON ENCADRE BIEN LA SOLUTION :                          
!                                                                       
      AA=X1
      B=X2 
      FA=FC1(AA)
      FB=FC1(B)  
      IF(FB*FA.GT.0.D0) THEN                                            
        IF (LNG.EQ.1) WRITE(LU,*) 'ZBRENT : FC1(X1)*FC1(X2) EST POSITIF'
        IF (LNG.EQ.2) WRITE(LU,*) 'ZBRENT : ROOT MUST BE BRACKETED'
        CALL PLANTE(1)
        STOP
      ENDIF                                                             
!                                                                       
!  ITERATIONS :                                                         
!                                                                       
      FC=FB                                                             
      DO ITER=1,ITMAX                                                
        IF(FB*FC.GT.0.D0) THEN                                          
          C=AA
          FC=FA
          D=B-AA
          E=D
        ENDIF                                                           
        IF(ABS(FC).LT.ABS(FB)) THEN                                     
          AA=B
          B=C                                                           
          C=AA
          FA=FB                                                         
          FB=FC                                                         
          FC=FA                                                         
        ENDIF                                                           
        EPS2=0.5D0*EPS                                                  
        XM=0.5D0*(C-B)                                                  
        IF(ABS(XM).LE.EPS2.OR.FB.EQ.0.D0)THEN                           
          X2=B                                                          
          RETURN                                                        
        ENDIF                                                           
        IF(ABS(E).GE.EPS2.AND.ABS(FA).GT.ABS(FB)) THEN                  
          S=FB/FA                                                       
          IF(AA.EQ.C) THEN
            P=2.D0*XM*S                                                 
            Q=1.D0-S                                                    
          ELSE                                                          
            Q=FA/FC                                                     
            R=FB/FC                                                     
            P=S*(2.D0*XM*Q*(Q-R)-(B-AA)*(R-1.D0))
            Q=(Q-1.D0)*(R-1.D0)*(S-1.D0)                                
          ENDIF                                                         
          IF(P.GT.0.D0) Q=-Q                                            
          P=ABS(P)                                                      
          IF(2*P.LT.MIN(3.D0*XM*Q-ABS(EPS2*Q),ABS(E*Q))) THEN           
            E=D                                                         
            D=P/Q                                                       
          ELSE                                                          
            D=XM                                                        
            E=D                                                         
          ENDIF                                                         
        ELSE                                                            
          D=XM                                                          
          E=D                                                           
        ENDIF                                                           
        AA=B
        FA=FB                                                           
        IF(ABS(D).GT.EPS2) THEN                                         
          B=B+D                                                         
        ELSE                                                            
          B=B+SIGN(EPS2,XM)                                             
        ENDIF                                                           
        FB=FC1(B)                                                       
      ENDDO                                                          
!                                                                       
      IF (LNG.EQ.1) WRITE(LU,*) 'ZBRENT : MAXIMUM D''ITERATIONS ATTEINT'
      IF (LNG.EQ.2) WRITE(LU,*) 'ZBRENT : EXCEEDING MAXIMUM ITERATIONS' 
      X2=B                                                              
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
      RETURN                                                            
      END                                                               

