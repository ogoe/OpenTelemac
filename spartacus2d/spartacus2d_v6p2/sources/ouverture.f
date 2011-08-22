!                        **********************
                          SUBROUTINE OUVERTURE
!                        **********************
!
     &   (NELEM, NPOIN, NVARSOR,NVARSORMAX, KVISU,
     &    XINT , XMIN , XMAX   ,ZINT      , ZMIN ,
     &    ZMAX , DR                              )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! DR        ! -->  ! INITIAL INTERPARTICLE SPACING                  !
! ! KVISU     ! -->  ! CHOICE INDEX FOR THE POSTPROCESSOR             !
! ! NELEM     ! -->  ! NUMBER OF MESH ELEMENTS FOR RUBENS             !
! ! NPOIN     ! -->  ! NUMBER OF MESH POINTS FOR RUBENS               !
! ! NVARSOR   ! -->  ! PRINTOUT VARIABLE NUMBER                       !
! ! NVARSORMAX! -->  ! MAXIMUM NUMBER OF PRINTOUT VARIABLES           !
! ! XINT, ZINT! -->  ! COORDINATES OF MESH POINTS                     !
! ! XMIN, XMAX! -->  ! MINIMUM AND MAXIMUM X OF THE DOMAIN            !
! ! ZMIN, ZMAX! -->  ! MINIMUM AND MAXIMUM Z OF THE DOMAIN            !
! !___________!______!________________________________________________!
!
! MODE : -->(NON MODIFIED DATA), <--(RESULT), <-->(MODIFIED DATA)
!----------------------------------------------------------------------
!
! SPARTACUS2D V5P9
! D. Violeau           & R. Issa
! +33(0)1-30-87-78-31 // +33(0)1-30-87-84-28
! LNHE - 2008
!
! FONCTION : ouvre les fichiers sorties et ecrit les entetes
! FUNCTION : opens the printout files and writes their headlines
!
! PROGRAMMES APPELANT : SPARTACUS2D
! CALLED BY
!
! PROGRAMMES APPELES  : -
! CALLED PROGRAMS
!
!----------------------------------------------------------------------
!
! Variables
!==========
!
      IMPLICIT NONE
!
      INTEGER NVARSORMAX, NVARSOR, NPOIN
      INTEGER NELEM     , KVISU  , I
!
      DOUBLE PRECISION X1MAIL, X2MAIL, Z1MAIL, Z2MAIL
      DOUBLE PRECISION XMIN  , ZMIN  , XMAX  , ZMAX
      DOUBLE PRECISION DR
!
      INTEGER NBV (2)      , IPARAM(10)   , NPV(4)
      INTEGER IKLE(3,NELEM), IPOBO (NPOIN)
!
      REAL XINT(NPOIN), ZINT(NPOIN)
!
      CHARACTER*32  TEXTL (NVARSORMAX)
      CHARACTER*80  TITRE
      CHARACTER*60  TITRE2
      CHARACTER*18  VARIABLE
      CHARACTER*191 VARIABLE2
      CHARACTER*10  VARIABLE2P
!
! Ouvertures et ecritures en-tetes
!=================================
! Opening and headline writing
!=============================
!
      IF (KVISU.EQ.1) THEN
!
! Format Rubens
!--------------
!
! Fichier positions
!..................
! Position file
!..............
!
        OPEN (47,FORM='FORMATTED',FILE='FORT.47')
!
        WRITE (47,*) '2'
        WRITE (47,902) 'Xa'
        WRITE (47,902) 'Za'
        WRITE (47,*) '0 0 0 0 0 0 0 0 0 0'
!
! Fichiers maillage et champs
!............................
! Mesh and field files
!.....................
!
! Nombre de variables de sorties
! ++++++++++++++++++++++++++++++
!
        NVARSOR = 9
!
        OPEN (11,FORM='UNFORMATTED',FILE='FORT.11')
        OPEN (48,FORM='UNFORMATTED',FILE='FORT.48')
!
        READ  (11) TITRE
        WRITE (48) TITRE
!
        READ  (11) NBV
        DO 10 I=1,NBV(1)
          READ (11) TEXTL(I)
  10    CONTINUE
        NBV(1) = NVARSOR
        WRITE (48) NBV
!
        TEXTL(1)='VX'
        TEXTL(2)='VZ'
        TEXTL(3)='RHO'
        TEXTL(4)='P'
        TEXTL(5)='K'
        TEXTL(6)='EPS'
        TEXTL(7)='NUT'
        TEXTL(8)='S'
        TEXTL(9)='PRIV'
!
        DO 11 I=1,NBV(1)
          WRITE (48) TEXTL(I)
  11    CONTINUE
!
        READ  (11) IPARAM
        WRITE (48) IPARAM
!
        READ  (11) NPV
        WRITE (48) NPV
!
        READ  (11) IKLE
        WRITE (48) IKLE
!
        READ  (11) IPOBO
        WRITE (48) IPOBO
!
        READ (11) XINT
        READ (11) ZINT
!
        CLOSE (11)
!
! Mise a l'echelle du maillage
!.............................
! Mesh scaling
!.............
!
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
     &           *(XMAX-XMIN)+XMIN
          ZINT(I)=(ZINT(I)-Z1MAIL)/(Z2MAIL-Z1MAIL)
     &           *(ZMAX-ZMIN)+ZMIN
 402    CONTINUE
!
        WRITE (48) XINT
        WRITE (48) ZINT
!
      ELSE
!
! Format Tecplot
!---------------
!
! Fichier positions
!..................
! Position file
!..............
!
        TITRE     = 'TITLE="position des particules"'
        VARIABLE  = 'VARIABLES="X" "Z"'
!
        OPEN(47,FILE='FORT.47',FORM='FORMATTED')
!
        WRITE(47,130) TITRE
        WRITE(47,131) VARIABLE
!
! Fichier champs
!...............
! Field file
!...........
!
        TITRE2    = 'TITLE="resultats"'
        VARIABLE2 = 'VARIABLES="X(m)"  "Z(m)"
     & "Vx(m/s)" "Vz(m/s)" "rho(kg/m3)" "P(Pa)" "k(m2/s2)"
     & "eps(m2/s3)" "nut(m2/s)" "S(s-1)" "kpar" "kfluid"'
        VARIABLE2P = ' "priv"'
!
        OPEN(48,FILE='FORT.48',FORM='FORMATTED')
!
        WRITE(48,130) TITRE2
        WRITE(48,132) VARIABLE2,VARIABLE2P
!
      ENDIF
!
! Fichier suite de calcul
!------------------------
! Continued calculation file
!---------------------------
!
      OPEN(49,FILE='FORT.49',FORM='FORMATTED')
!
      WRITE(49,134) DR
!
 900  FORMAT(30(' ',D16.8))
 902  FORMAT(A)
 130  FORMAT(A60)
 131  FORMAT(A18)
 132  FORMAT(A190,A10)
 134  FORMAT(D16.8)
!
      RETURN
      END