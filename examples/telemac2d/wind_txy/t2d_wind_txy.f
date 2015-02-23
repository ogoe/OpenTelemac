!                    ****************
                     SUBROUTINE METEO
!                    ****************
!
     &(PATMOS,WINDX,WINDY,FUAIR,FVAIR,X,Y,AT,LT,NPOIN,VENT,ATMOS,
     & HN,TRA01,GRAV,ROEAU,NORD,PRIVE,FO1,FILES,LISTIN)
!
!***********************************************************************
! TELEMAC2D   V7P0                                   
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
!history  P. PRODANOVIC (RIGGS ENGINEERING LTD.)
!+        22/04/2014
!+        V6P3
!+   Added an option for spacio-temporal wind interpolation of wind.
!+   
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        16/02/2015
!+        V7P0
!+   Shifting the stations coordinates removed in case of wind varying
!+   in time and space (option 99). Managing the divisions by 0 is now
!+   done by subroutine IDWM_T2D, and this does not spoil parallelism.
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
! ######################################################################
! IDWM WIND INTERPOLATION CUSTOM VARIABLES
! ######################################################################      
!      
      INTEGER I, NUMSTA, NUMPOINTS, A, B, J, K, JUNK
      DOUBLE PRECISION THETA_RAD
!      
!     CORDINATES OF THE STATIONS UTM
!
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: XX, YY, AT_WIND
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: OUT_WSPD, OUT_WDIR
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: WIND, POINTS
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: INPSTA_S
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: INPSTA_D
!
! ######################################################################  
      
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
!     99: VARIABLE IN TIME AND SPACE;  INVERSE DISTANCE WEIGHTING METHOD
!
      MY_OPTION = 99
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
! ######################################################################
! IDWM WIND INTERPOLATION; THIS IS EXECUTED ONLY ONCE AT THE START
! ######################################################################
!
        IF(MY_OPTION .EQ. 99) THEN
        ! READ BLANK LINE AT BEGINING OF FILE
          READ(UL,*)
        ! READ NUMSTA AND NUMPOINTS
          READ(UL,*) NUMSTA, NUMPOINTS
              
        !ALLOCATE THE ARRAYS
          ALLOCATE(XX(NUMSTA), YY(NUMSTA), AT_WIND(NUMPOINTS))
          ALLOCATE(WIND(NUMPOINTS,NUMSTA*2+5), POINTS(NPOIN,2))
          ALLOCATE(INPSTA_S(NUMSTA,3), INPSTA_D(NUMSTA,3))
          ALLOCATE(OUT_WSPD(NPOIN), OUT_WDIR(NPOIN))
!                  
          ! READ STATION CORDINATES
          DO B = 1,NUMSTA
            READ(UL,*) XX(B), YY(B)
           !WRITE(*,*) XX(B), YY(B)
          ENDDO
!
          ! READ THE WIND TIME SERIES FROM THE INPUT FILE 
          ! FIRST COLUMN IS TIME IN SECONDS, REST OF COLUMNS ARE WSPD 
          ! AND WDIR FOR EACH STATION READ
          DO A = 1, NUMPOINTS
            READ(UL,*) (WIND(A,B), B=1,NUMSTA*2+1)
          ENDDO
!              
          ! EXTRACT AT_WIND FROM WIND(A,B); FIRST COLUMN IS TIME IN SECONDS
          DO A = 1, NUMPOINTS
            AT_WIND(A) = WIND(A,1)
          ENDDO
!        
          ! ASSEMBLE THE POINTS ARRAY FOR IDWM FUNCTION
          DO I = 1, NPOIN
            POINTS(I,1) = X(I)
            POINTS(I,2) = Y(I)
          ENDDO
!              
        ENDIF !(MY_OPTION .EQ. 99)
!        
! #######################################################################        
!
      ENDIF !(LT.EQ.0)
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
! #######################################################################
!         IDWM WIND INTERPOLATION CODE
! #######################################################################
!
      IF(MY_OPTION.EQ.99.AND.VENT) THEN        
!       ASSEMBLE THE ARRAYS OF X,Y,WNDSPD AND X,Y,WNDDIR FOR EACH ITERATION
        DO A = 1,NUMPOINTS 
          IF(AT_WIND(A) .EQ. AT) THEN
            DO B = 1, NUMSTA
              ! ASSEMBLE THE ARRAYS FOR THIS TIME STEP
              INPSTA_D(B,1) = XX(B)
              INPSTA_D(B,2) = YY(B)
              INPSTA_D(B,3) = WIND(A,B*2+1)      
              INPSTA_S(B,1) = XX(B)
              INPSTA_S(B,2) = YY(B)
              INPSTA_S(B,3) = WIND(A,B*2)                          
            ENDDO    
          ENDIF
        ENDDO
!
        CALL IDWM_T2D(INPSTA_S,POINTS,OUT_WSPD,NPOIN,NUMSTA)
        CALL IDWM_T2D(INPSTA_D,POINTS,OUT_WDIR,NPOIN,NUMSTA)
!
!       CONVERT OUT_WSPD AND OUT_WDIR TO WINDX AND WINDY
!
        DO K = 1, NPOIN
        IF (OUT_WDIR(K) >= 0 .AND. OUT_WSPD(K) >= 0) THEN
          IF ((OUT_WDIR(K) >= 0) .AND. (OUT_WDIR(K) <= 90)) THEN
            THETA_RAD = OUT_WDIR(K) * 3.141592654 / 180.0
            WINDX(K) = -SIN(THETA_RAD) * OUT_WSPD(K)
            WINDY(K) = -COS(THETA_RAD) * OUT_WSPD(K)
          END IF
!                  
          IF ((OUT_WDIR(K) > 90) .AND. (OUT_WDIR(K) <= 180)) THEN
            THETA_RAD = (180 - OUT_WDIR(K)) * 3.141592654 / 180.0
            WINDX(K) = -SIN(THETA_RAD)* OUT_WSPD(K)
            WINDY(K) =  COS(THETA_RAD)* OUT_WSPD(K)
          END IF
!                  
          IF ((OUT_WDIR(K) > 180) .AND. (OUT_WDIR(K) <= 270)) THEN
            THETA_RAD = (OUT_WDIR(K)-180) * 3.141592654 / 180.0
            WINDX(K) = SIN(THETA_RAD)* OUT_WSPD(K)
            WINDY(K) = COS(THETA_RAD)* OUT_WSPD(K)
          END IF            
!                  
          IF ((OUT_WDIR(K) > 270) .AND. (OUT_WDIR(K) <= 360)) THEN
            THETA_RAD = (360-OUT_WDIR(K)) * 3.141592654 / 180.0
            WINDX(K) =  SIN(THETA_RAD)* OUT_WSPD(K)
            WINDY(K) = -COS(THETA_RAD)* OUT_WSPD(K)
          END IF      
        ELSE
          WINDX(K) = -999.D0
          WINDY(K) = -999.D0
          WRITE(*,*) 'NO WIND DATA FOR TIME ', AT
        END IF
        END DO !K
!       
      ENDIF ! MY_OPTION=99  
        
! #######################################################################
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
!                    *******************
                     SUBROUTINE IDWM_T2D
!                    *******************
!
     &(ELEV,POINTS,IDWM,NPOIN,NUMSTA)
!
!***********************************************************************
! TELEMAC2D   V7P0
!***********************************************************************
!
!brief    USES INVERSE DISTANCE WEIGHTING METHOD TO COMPUTE A WIND FIELD
!+               THAT VARIES IN TIME AND SPACE
!
!history  P. PRODANOVIC (RIGGS ENGINEERING LTD)
!+        23/04/2014
!+        V7P0
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        16/02/2015
!+        V7P0
!+   Managing the divisions by 0 + optimization.
!
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
!     VARIABLES THAT ARE USED BY THE SUBROUTINE TO PASS DATA IN AND OUT
      INTEGER,INTENT(IN) :: NPOIN, NUMSTA
      DOUBLE PRECISION, INTENT(IN) :: POINTS(NPOIN,2),ELEV(NUMSTA,3)
      DOUBLE PRECISION, INTENT(INOUT) :: IDWM(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N, M, I, J, K      
!     WEIGHTS, DENOMINATOR, DISTANCE
      DOUBLE PRECISION :: W1, W2, W3, W4, DEN, DIST
!     CURRENT MINS
      DOUBLE PRECISION :: MIN1CUR, MIN2CUR, MIN3CUR, MIN4CUR        
!     LOCATIONS OF THE MININIMS (USED FOR ARRAY REFERENCING)
      INTEGER :: MIN1LOC, MIN2LOC, MIN3LOC, MIN4LOC
!
!-----------------------------------------------------------------------
!
!     DEFINE N AND M, THIS IS DONE TO HARMONIZE PASTING OF CODE
      N = NUMSTA
      M = NPOIN    
!        
!     MAIN LOOP TO DO THE INTERPOLATION
!
      DO J = 1, M
!
!       SET MIN_PREV VARS TO LARGE NUMBERS
!
        MIN1CUR = 1.D30
        MIN2CUR = 1.D30
        MIN3CUR = 1.D30
        MIN4CUR = 1.D30  
!
!       INITIALIZE MIN_LOC
!
        MIN1LOC = -1
        MIN2LOC = -1
        MIN3LOC = -1
        MIN4LOC = -1
!
        DO I = 1, N
!
          IF(ELEV(I,3).GT.0.D0) THEN
!
            DIST=(ELEV(I,1)-POINTS(J,1))**2+(ELEV(I,2)-POINTS(J,2))**2  
!     
!           FIND MIN DIST IN EACH OF THE FOUR QUADRANTS
!           2 | 1
!           -----
!           3 | 4      
!            
!          QUADRANT 1
            IF(ELEV(I,1).GE.POINTS(J,1).AND.
     &         ELEV(I,2).GE.POINTS(J,2)      ) THEN    
              IF(DIST < MIN1CUR) THEN
                MIN1CUR = DIST      
                MIN1LOC = I
              ENDIF
            ENDIF                  
!           QUADRANT 2
            IF(ELEV(I,1).LT.POINTS(J,1).AND.
     &         ELEV(I,2).GE.POINTS(J,2)      ) THEN
              IF(DIST < MIN2CUR) THEN
                MIN2CUR = DIST
                MIN2LOC = I
              ENDIF
            ENDIF                  
!           QUADRANT 3
            IF(ELEV(I,1).LT.POINTS(J,1).AND.
     &         ELEV(I,2).LT.POINTS(J,2)      ) THEN 
              IF(DIST < MIN3CUR) THEN
                MIN3CUR = DIST
                MIN3LOC = I
              ENDIF
            ENDIF                  
!           QUADRANT 4
            IF(ELEV(I,1).GT.POINTS(J,1).AND.
     &         ELEV(I,2).LT.POINTS(J,2)      ) THEN
              IF(DIST < MIN4CUR) THEN
                MIN4CUR = DIST
                MIN4LOC = I
              ENDIF
            ENDIF
!
          ENDIF                        
!
        ENDDO
!                  
!       CALCULATE WEIGHTS
! 
!       AVOIDING DIVISIONS BY 0
!
        MIN1CUR=MAX(MIN1CUR,1.D-6)
        MIN2CUR=MAX(MIN2CUR,1.D-6)
        MIN3CUR=MAX(MIN3CUR,1.D-6)
        MIN4CUR=MAX(MIN4CUR,1.D-6)
!
        DEN = 1.D0/MIN1CUR+1.D0/MIN2CUR+1.D0/MIN3CUR+1.D0/MIN4CUR
!
!       IN CASE WHEN ALL INPUT DATA IS MISSING, ALSO OUTPUT MISSING
!
        IF(MIN1LOC.EQ.-1.AND.MIN2LOC.EQ.-1.AND.
     &     MIN3LOC.EQ.-1.AND.MIN4LOC.EQ.-1      ) THEN
          IDWM(J) = -999.D0
        ELSE
          IDWM(J) = 0.D0
          IF(MIN1LOC.GT.0) THEN
            IDWM(J)=IDWM(J)+ELEV(MIN1LOC,3)/MIN1CUR
          ENDIF
          IF(MIN2LOC.GT.0) THEN
            IDWM(J)=IDWM(J)+ELEV(MIN2LOC,3)/MIN2CUR
          ENDIF
          IF(MIN3LOC.GT.0) THEN
            IDWM(J)=IDWM(J)+ELEV(MIN3LOC,3)/MIN3CUR
          ENDIF
          IF(MIN4LOC.GT.0) THEN
            IDWM(J)=IDWM(J)+ELEV(MIN4LOC,3)/MIN4CUR
          ENDIF 
          IDWM(J) = IDWM(J)/DEN      
        ENDIF      
!
      ENDDO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
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
