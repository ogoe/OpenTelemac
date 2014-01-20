!                       *********************
                        SUBROUTINE T3D_CORFON
!                       *********************
!
     &(SZF, ST1, ST2, ZF, T1, T2, X, Y, PRIVE, NPOIN2,
     & LISFON, MSK, MASKEL, MATR2D, MESH2D, S)
!
!***********************************************************************
! TELEMAC 3D VERSION 5.2    25/11/97      J.M. JANIN  (LNH) 30 87 72 84
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
      USE DECLARATIONS_TELEMAC3D, ONLY : MESH3D
!
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
      INTEGER I
      LOGICAL MAS
      INTEGER IM,JM,J,POS_LOC
      DOUBLE PRECISION EIKON
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
!
      ENDIF
!
      IM = 47
      JM = 10
C                                                                         
C  VARIANTE FOND EN PENTE RECTILIGNE + CUVETTE                            
C                                                                         
      DO I=1,IM 
      DO J=1,JM 
C       PENTE RECTILIGNE                                                       
        POS_LOC = GLOBAL_TO_LOCAL_POINT(I+(J-1)*IM,MESH3D)
!
!       NOTE JMH: THIS IS VERY HEAVY, THERE SHOULD BE A
!                 FORMULA FUNCTION OF X.
!
        IF(POS_LOC.GT.0) THEN
          ZF(POS_LOC)=-0.6D0+0.46D0*FLOAT(I-1)/FLOAT(IM-1) 
C         BOSSE GAUSSIENNE            
          IF(I.GT.9.AND.I.LT.29) THEN
            EIKON = -(I-19)**2/20.D0 
            ZF(POS_LOC) = ZF(POS_LOC) + 0.1D0*EXP(EIKON)
          ENDIF
        ENDIF                                                 
      ENDDO                                                            
      ENDDO    
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C                       *****************************
                        DOUBLE PRECISION FUNCTION SL3
C                       *****************************
C
C
     *( I , TIME , N , ENTET )
C
C***********************************************************************
C TELEMAC 3D VERSION 5.9    12/12/00    J-M HERVOUET (LNH) 30 87 80 18
C
C***********************************************************************
C
C FONCTION  : DONNE LA VALEUR DE LA COTE DE LA SURFACE LIBRE POUR TOUTES
C             LES ENTREES A COTE IMPOSEE.
C
C-----------------------------------------------------------------------
C
C FUNCTION  : GIVES THE PRESCRIBED VALUE OF FREE SURFACE AT
C             A LIQUID BOUNDARY
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |   I            | -->| NUMBER OF LIQUID BOUNDARY
C |   N            | -->| GLOBAL NUMBER OF POINT
C |________________|____|_______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C  APPELE PAR : BORD
C
C***********************************************************************
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER          , INTENT(IN) :: I,N
      DOUBLE PRECISION , INTENT(IN) :: TIME
      LOGICAL          , INTENT(IN) :: ENTET
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CHARACTER*8 FCT
      INTEGER J
      LOGICAL DEJA,OK(99)
      DATA    DEJA /.FALSE./
      SAVE    OK,DEJA
C
      DOUBLE PRECISION PI,OMEGA,PERIODE,A
      DATA PI/3.141592653589D0/
C
C     FIRST CALL, OK INITIALISED TO .TRUE.
C
      IF(.NOT.DEJA) THEN
        DO J=1,99
          OK(J)=.TRUE.
        ENDDO
        DEJA=.TRUE.
      ENDIF
C
C-----------------------------------------------------------------------
C
C     IF FILE OF LIQUID BOUNDARIES EXISTING, ATTEMPT TO FIND
C     THE VALUE IN IT. IF YES, OK REMAINS TO .TRUE. FOR NEXT CALLS
C                      IF  NO, OK SET     TO .FALSE.
C
      IF(OK(I).AND.T3D_FILES(T3DIMP)%NAME(1:1).NE.' ') THEN
C
C       FCT WILL BE SL(1), SL(2), ETC, SL(99), DEPENDING ON I
        FCT(1:3)='SL('
        IF(I.LT.10) THEN 
          WRITE(FCT(4:4),FMT='(I1)') I
          FCT(5:8)=')   '
        ELSEIF(I.LT.100) THEN
          WRITE(FCT(4:5),FMT='(I2)') I
          FCT(6:8)=')  '
        ELSE
          CALL PLANTE(1)
          STOP 'SL3 NOT PROGRAMMED FOR MORE THAN 99 BOUNDARIES'
        ENDIF
        CALL READ_FIC_FRLIQ(SL3,FCT,TIME,T3D_FILES(T3DIMP)%LU,
     &                      ENTET,OK(I))
C
      ENDIF
C
      IF(.NOT.OK(I).OR.T3D_FILES(T3DIMP)%NAME(1:1).EQ.' ') THEN
C 
C     PROGRAMMABLE PART                              
C     SL IS TAKEN IN THE PARAMETER FILE, BUT MAY BE CHANGED 
C                                                                             
C       SL3 = COTIMP(I)
        PERIODE=600.D0
        OMEGA=2*PI/PERIODE
        A=0.55D0/2.D0
        SL3=A*(COS(OMEGA*TIME)-1.D0)
C 
      ENDIF           
C
C-----------------------------------------------------------------------
C
      RETURN
      END
