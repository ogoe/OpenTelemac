!                       *****************
                        SUBROUTINE CONDIM 
!                       *****************
!
!
!***********************************************************************
! TELEMAC-3D   V5.1          25/11/97      J-M HERVOUET(LNH) 30 87 80 18
! FORTRAN95 VERSION         MARCH 1999        JACEK A. JANKOWSKI PINXIT
!***********************************************************************
!
!      FONCTION:
!      =========
!
!    INITIALISATION DES TABLEAUX DES GRANDEURS PHYSIQUES
!
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
      INTEGER IPLAN, I, IPOIN2, IPOIN3
      DOUBLE PRECISION PI
      PARAMETER (PI = 3.1415926535897932384626433D0)
!     FOR SOLITARY WAVE
      DOUBLE PRECISION WX, WH, HV1, HV2, HV3, HV4, X0
!
!***********************************************************************
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
!
!
!     WH=2.0D0
      WH=1.D0
      WX=10.0D0
!     X0=80.0D0
      X0=150.0D0
!
      DO IPOIN2=1,NPOIN2 
        HV1=SQRT(3.0D0/4.0D0*WH/WX**3.0D0)*(X(IPOIN2)-X0)
        HV2=2.0D0/(EXP(HV1)+EXP(-HV1))
        H%R(IPOIN2)=H%R(IPOIN2) + WH*HV2**2 
      END DO
!
      CALL OS ('X=Y     ', HN, H, H, 0.D0)
!
!-----------------------------------------------------------------------
!
!     INITIALISATION DE LA COTE DU PLAN INTERMEDIAIRE DE REFERENCE.
!     PAR DEFAUT, CE PLAN EST PLACE ENTRE FOND ET SURFACE AU PRORATA
!     DU PARAMETRE NPLINT.
!
! DOUBLED; Z => Z3%R WHICH IS MESH3D%Z
!
      IF (NPLINT.GE.2) THEN
        Z( (NPLINT-1)*NPOIN2+1 : NPLINT*NPOIN2 ) = COTINT
        CALL OV( 'X=C     ' , Z((NPLINT-1)*NPOIN2+1 : NPLINT*NPOIN2),
     &                Z, Z, COTINT , NPOIN2)
      ENDIF

! ORIG. CODE
!
!      IF(NPLINT.GE.2) THEN
!        DO I=1,NPOIN2
!          Z(I,NPLINT)=COTINT
!        END DO
!      ENDIF
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
!            * ZSTAR%R(NPLINT) =  0.D0 ( PLAN INTERMEDIAIRE DE REFERENCE )
!            * ZSTAR%R(NPLAN)  =  1.D0 ( PLAN DE LA SURFACE LIBRE )
!         ET POUR TOUT I COMPRIS ENTRE 1 ET NPLAN-1
!            * ZSTAR%R(I) < ZSTAR%R(I+1)
!
!     PAR DEFAUT, LES PLANS QUASI HORIZONTAUX SONT REGULIEREMENT ESPACES
!
!***********************************************************************
!     POUR DONNER VOTRE PROPRE REPARTITION DES PLANS, MODIFIEZ LES
!     BOUCLES 5 ET 10
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
!     INITIALISATION DES VITESSES
!
      CALL OS( 'X=C     ' , U , U , U , 0.0D0 )
      CALL OS( 'X=C     ' , V , V , V , 0.0D0 )
      CALL OS( 'X=C     ' , W , W , W , 0.0D0 )
C
C SOLITARY WAVE INITIAL VELOCITY - ANALYTICAL SOLUTION 
C
      DO IPLAN=1,NPLAN
        DO IPOIN2=1,NPOIN2
!
          IPOIN3 = (IPLAN-1)*NPOIN2 + IPOIN2
!
          HV1=SQRT(3.D0/4.D0*WH/WX**3)*(X(IPOIN3)-X0)
          HV2=2.D0/(EXP(HV1)+EXP(-HV1))
          U%R(IPOIN3)=SQRT(GRAV*WX)*WH/WX*HV2**2

          HV3=(EXP(HV1)-EXP(-HV1))/(EXP(HV1)+EXP(-HV1))
          HV4=SQRT(3.D0*GRAV*WX)
     &       *((SQRT(WH/WX))**3)*(Z(IPOIN3)+WX)/WX
          W%R(IPOIN3)=(HV4*HV2**2)*HV3
!
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
!     INITIALISATION DES TRACEURS
!
      IF (NTRAC.NE.0) THEN
        CALL OS( 'X=C     ', TA, TA, TA, 0.D0)
      ENDIF
!
!
!-----------------------------------------------------------------------
!   INITIALISATION DU MODELE K-EPSILON (FACULTATIF)
!   SI VOUS LE FAITES, INDIQUEZ AKEP = .FALSE.
!
!     IF(ITURBV.EQ.3) THEN
!       AKEP = .FALSE.
!     ENDIF
!
!-----------------------------------------------------------------------
! (TO BE DELETED WHEN NO PROJECTION TWO...)
! INITIALIZE THE HYDRODYNAMIC PRESSURE FIELD TO 0.0
! (IT MAY BE APPROPRIATE TO SOLVE A POISSON EQUATION FOR IT)
!
!
! INITIALIZE THE PRESSURE FIELDS TO 0.0
! 
      IF(NONHYD) THEN
        CALL OS('X=C     ',X=DP ,C=0.D0)
        CALL OS('X=C     ',X=DPN,C=0.D0)
        WRITE (LU,*) 'CONDIM: DYNAMIC PRESSURE INITIALISED TO ZERO'
        CALL OS('X=C     ',X=PH,C=0.D0)
        WRITE (LU,*) '        HYDROSTATIC PRESSURE INITIALISED TO ZERO.'
      ENDIF
!
! PRINTOUTS ABOUT THE MESH LEVEL DISTRIBUTION
!
      WRITE(LU,*)
      WRITE(LU,1000)
1000  FORMAT(80('='))
      WRITE (LU,*) 'CONDIM: ZSTAR'
      WRITE(LU,'(/'' NODE #1 DEPTH: '',1PG13.5,'' M'',/)')  H%R(1)
      WRITE(LU,'('' POSITIONS OF THE PLANES, AT = '',F7.4,'' S''/)') AT 

      DO IPLAN=1,NPLAN
        WRITE(LU,'('' IPLAN = '',I2,''     ZSTAR = '',F9.6,      
     &  ''     MAB = '',1PG13.6)')                               
     &     IPLAN, ZSTAR%R(IPLAN), ZSTAR%R(IPLAN)*H%R(1)
      END DO
      WRITE(LU,*)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
