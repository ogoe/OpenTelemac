C                        **********************
                          SUBROUTINE OUVERTURE
C                        **********************
C
     .   (NELEM, NPOIN, NVARSOR,NVARSORMAX, KVISU, 
     .    XINT , XMIN , XMAX   ,ZINT      , ZMIN , 
     .    ZMAX , DR                              )
C
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! DR        ! -->  ! INITIAL INTERPARTICLE SPACING                  !
C ! KVISU     ! -->  ! CHOICE INDEX FOR THE POSTPROCESSOR             !
C ! NELEM     ! -->  ! NUMBER OF MESH ELEMENTS FOR RUBENS             !
C ! NPOIN     ! -->  ! NUMBER OF MESH POINTS FOR RUBENS               !
C ! NVARSOR   ! -->  ! PRINTOUT VARIABLE NUMBER                       !
C ! NVARSORMAX! -->  ! MAXIMUM NUMBER OF PRINTOUT VARIABLES           !
C ! XINT, ZINT! -->  ! COORDINATES OF MESH POINTS                     !
C ! XMIN, XMAX! -->  ! MINIMUM AND MAXIMUM X OF THE DOMAIN            !
C ! ZMIN, ZMAX! -->  ! MINIMUM AND MAXIMUM Z OF THE DOMAIN            !
C !___________!______!________________________________________________!
C
C MODE : -->(NON MODIFIED DATA), <--(RESULT), <-->(MODIFIED DATA)
C----------------------------------------------------------------------
C
C SPARTACUS2D V5P9
C D. Violeau           & R. Issa
C +33(0)1-30-87-78-31 // +33(0)1-30-87-84-28 
C LNHE - 2008
C
C FONCTION : ouvre les fichiers sorties et ecrit les entetes
C FUNCTION : opens the printout files and writes their headlines
C
C PROGRAMMES APPELANT : SPARTACUS2D
C CALLED BY           
C
C PROGRAMMES APPELES  : -
C CALLED PROGRAMS     
C
C----------------------------------------------------------------------
C
C Variables
C==========
C
      IMPLICIT NONE
C
      INTEGER NVARSORMAX, NVARSOR, NPOIN
      INTEGER NELEM     , KVISU  , I   
C
      DOUBLE PRECISION X1MAIL, X2MAIL, Z1MAIL, Z2MAIL
      DOUBLE PRECISION XMIN  , ZMIN  , XMAX  , ZMAX
      DOUBLE PRECISION DR
C
      INTEGER NBV (2)      , IPARAM(10)   , NPV(4)
      INTEGER IKLE(3,NELEM), IPOBO (NPOIN)
C
      REAL XINT(NPOIN), ZINT(NPOIN)
C
      CHARACTER*32  TEXTL (NVARSORMAX)
      CHARACTER*80  TITRE
      CHARACTER*60  TITRE2
      CHARACTER*18  VARIABLE
      CHARACTER*191 VARIABLE2
      CHARACTER*10  VARIABLE2P
C
C Ouvertures et ecritures en-tetes
C=================================
C Opening and headline writing
C=============================
C
      IF (KVISU.EQ.1) THEN
C
C Format Rubens
C--------------
C
C Fichier positions
C..................
C Position file
C..............
C
        OPEN (47,FORM='FORMATTED',FILE='FORT.47')
C
        WRITE (47,*) '2'
        WRITE (47,902) 'Xa'
        WRITE (47,902) 'Za'
        WRITE (47,*) '0 0 0 0 0 0 0 0 0 0'
C
C Fichiers maillage et champs
C............................
C Mesh and field files
C.....................
C
C Nombre de variables de sorties
C ++++++++++++++++++++++++++++++
C
        NVARSOR = 9
C
        OPEN (11,FORM='UNFORMATTED',FILE='FORT.11')
        OPEN (48,FORM='UNFORMATTED',FILE='FORT.48')
C
        READ  (11) TITRE
        WRITE (48) TITRE
C
        READ  (11) NBV
        DO 10 I=1,NBV(1)
          READ (11) TEXTL(I)
  10    CONTINUE
        NBV(1) = NVARSOR
        WRITE (48) NBV
C
        TEXTL(1)='VX'
        TEXTL(2)='VZ'
        TEXTL(3)='RHO'
        TEXTL(4)='P'
        TEXTL(5)='K'
        TEXTL(6)='EPS'
        TEXTL(7)='NUT'
        TEXTL(8)='S'
        TEXTL(9)='PRIV'
C
        DO 11 I=1,NBV(1)
          WRITE (48) TEXTL(I)
  11    CONTINUE        
C
        READ  (11) IPARAM
        WRITE (48) IPARAM
C
        READ  (11) NPV
        WRITE (48) NPV
C
        READ  (11) IKLE
        WRITE (48) IKLE
C
        READ  (11) IPOBO
        WRITE (48) IPOBO
C
        READ (11) XINT
        READ (11) ZINT      
C
        CLOSE (11)
C
C Mise a l'echelle du maillage
C.............................
C Mesh scaling
C.............
C
        X1MAIL= 1.E9
        X2MAIL=-1.E9
        Z1MAIL= 1.E9
        Z2MAIL=-1.E9
        DO 401 I=1,NPOIN
          IF (X1MAIL.GT.XINT(I)) X1MAIL=XINT(I)
          IF (X2MAIL.LT.XINT(I)) X2MAIL=XINT(I)
          IF (Z1MAIL.GT.ZINT(I)) Z1MAIL=ZINT(I)
          IF (Z2MAIL.LT.ZINT(I)) Z2MAIL=ZINT(I)
 401    CONTINUE
        DO 402 I=1,NPOIN
          XINT(I)=(XINT(I)-X1MAIL)/(X2MAIL-X1MAIL)
     .           *(XMAX-XMIN)+XMIN
          ZINT(I)=(ZINT(I)-Z1MAIL)/(Z2MAIL-Z1MAIL)
     .           *(ZMAX-ZMIN)+ZMIN
 402    CONTINUE
C
        WRITE (48) XINT
        WRITE (48) ZINT
C
      ELSE
C
C Format Tecplot
C---------------
C
C Fichier positions
C..................
C Position file
C..............
C
        TITRE     = 'TITLE="position des particules"'
        VARIABLE  = 'VARIABLES="X" "Z"'
C
        OPEN(47,FILE='FORT.47',FORM='FORMATTED')
C
        WRITE(47,130) TITRE
        WRITE(47,131) VARIABLE
C
C Fichier champs
C...............
C Field file
C...........
C
        TITRE2    = 'TITLE="resultats"'
        VARIABLE2 = 'VARIABLES="X(m)"  "Z(m)"
     . "Vx(m/s)" "Vz(m/s)" "rho(kg/m3)" "P(Pa)" "k(m2/s2)"
     . "eps(m2/s3)" "nut(m2/s)" "S(s-1)" "kpar" "kfluid"'
        VARIABLE2P = ' "priv"'
C
        OPEN(48,FILE='FORT.48',FORM='FORMATTED')
C
        WRITE(48,130) TITRE2
        WRITE(48,132) VARIABLE2,VARIABLE2P
C
      ENDIF
C
C Fichier suite de calcul
C------------------------
C Continued calculation file
C---------------------------
C
      OPEN(49,FILE='FORT.49',FORM='FORMATTED')
C
      WRITE(49,134) DR
C
 900  FORMAT(30(' ',D16.8))
 902  FORMAT(A)
 130  FORMAT(A60)
 131  FORMAT(A18)
 132  FORMAT(A190,A10)
 134  FORMAT(D16.8) 
C
      RETURN
      END
