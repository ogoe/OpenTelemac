!
! TELEMAC-3D V5.1 REF. TEST CASE NH
! SOLITARY WAVE PROPAGATION OVER AN 
! UNEVEN SEABED 
!
!
!                       *****************
                        SUBROUTINE CONDIM 
!                       *****************
! 
!
!***********************************************************************
! TELEMAC-3D   V5.1         25/11/97      J-M HERVOUET(LNH) 30 87 80 18
! FORTRAN95 VERSION         MARCH 1999        JACEK A. JANKOWSKI PINXIT
!***********************************************************************
!
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
      INTEGER IPLAN, I, IPOIN2, IPOIN3
      DOUBLE PRECISION PI
      PARAMETER (PI = 3.1415926535897932384626433D0)
      DOUBLE PRECISION WX, WH, HV1, HV2, HV3, HV4, X0
!
! TIME ORIGIN
!
      AT  = 0.D0
!
! INITIALISATION DE H , LA HAUTEUR D'EAU, POUR UNE COTE NULLE.
!
      CALL OS( 'X=C     ' , H   , H , H , 0.D0)
      CALL OV( 'X=X-Y   ' , H%R , Z , Z , 0.D0, NPOIN2)
!
! *** SOLITARY WAVE ***
!
! WH WAVE HEIGHT
! WX WQUILIBRIUM WATER DEPTH
! X0 CREST INITIAL POSITION
!
      WH=2.0D0
      WX=10.0D0
      X0=80.0D0
!
      DO IPOIN2=1,NPOIN2 
        HV1=SQRT(3.0D0/4.0D0*WH/WX**3.0D0)*(X(IPOIN2)-X0)
        HV2=2.0D0/(EXP(HV1)+EXP(-HV1))
        H%R(IPOIN2)=H%R(IPOIN2) + WH*HV2**2 
      ENDDO
!
      CALL OS ('X=Y     ', HN, H, H, 0.0D0)
!
!-Mesh Initialization
      IF (NPLINT.GE.2) THEN
        CALL OV( 'X=C     ' , Z((NPLINT-1)*NPOIN2+1 : NPLINT*NPOIN2),
     &                Z, Z, COTINT , NPOIN2)
      ENDIF
!
      IF (NPLINT.GE.2) THEN
        DO IPLAN = 1,NPLINT-1
          ZSTAR%R(IPLAN) = DBLE(IPLAN-NPLINT)/DBLE(NPLINT-1)
        ENDDO
      ENDIF
!
      DO IPLAN = NPLINT,NPLAN
        ZSTAR%R(IPLAN) = DBLE(IPLAN-NPLINT)/DBLE(NPLAN-NPLINT)
      ENDDO
!
      CALL CALCOT(Z, H%R)
!
!- Velocities Initializations
!
      CALL OS( 'X=C     ' , U , U , U , 0.D0 )
      CALL OS( 'X=C     ' , V , V , V , 0.D0 )
      CALL OS( 'X=C     ' , W , W , W , 0.D0 )
!
!
! SOLITARY WAVE INITIAL VELOCITY - ANALYTICAL SOLUTION 
      DO IPLAN=1,NPLAN
        DO IPOIN2=1,NPOIN2
!
          IPOIN3 = (IPLAN-1)*NPOIN2 + IPOIN2
!
          HV1=SQRT(3.0D0/4.0D0*WH/WX**3.0D0)*(X(IPOIN3)-X0)
          HV2=2.0D0/(EXP(HV1)+EXP(-HV1))
          U%R(IPOIN3)=SQRT(GRAV*WX)*WH/WX*HV2**2.0D0

          HV3=(EXP(HV1)-EXP(-HV1))/(EXP(HV1)+EXP(-HV1))
          HV4=SQRT(3.0D0*GRAV*WX)
     &       *((SQRT(WH/WX))**3.0D0)*(Z(IPOIN3)+WX)/WX
          W%R(IPOIN3)=(HV4*HV2**2.0D0)*HV3
!
        END DO
      END DO
!
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
! TELEMAC-3D   V5.1      
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
      DO 20 I=1,NPOIN2
       IF (X(I).LE.160.D0) THEN
        ZF(I)=-10.D0
       ENDIF
       IF (X(I).GE.160.D0.AND.X(I).LE.260.D0) THEN
        ZF(I)=-10.D0-5./100.D0*(160.-X(I))
       ENDIF
       IF (X(I).GE.260.D0) THEN
        ZF(I)=-5.D0
       ENDIF
20    CONTINUE
!
!  LISSAGES EVENTUELS DU FOND
!
      IF(LISFON.GT.0) THEN
         MAS = .TRUE.
         CALL FILTER(SZF,MAS,ST1,ST2,MATR2D,'MATMAS          ',
     &               1.D0,S,S,S,S,S,S,MESH2D,MSK,MASKEL,LISFON)
      ENDIF
!
      RETURN
      END 
