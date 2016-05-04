!                       *****************
                        SUBROUTINE CONDIN
!                       *****************
!
!***********************************************************************
! TELEMAC 2D VERSION 5.2         19/08/98  J-M HERVOUET TEL: 30 87 80 18
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
      DOUBLE PRECISION CC
      INTEGER I,ITRAC
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
!        CALL EXACTE(H%R,X,NPOIN)
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
                        SUBROUTINE CORFON
!                       *****************
!
!***********************************************************************
! TELEMAC 2D VERSION 5.1          01/03/90    J-M HERVOUET
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
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL MAS
      INTEGER I
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
        ZF%R(I)=-1.853658536585365D0*1.D-5*X(I)-4.0D0
      ENDDO
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    ***************************
                     SUBROUTINE PRERES_TELEMAC2D
!                    ***************************
     &    (IMP,LEO)
!
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    PREPARES THE VARIABLES WHICH WILL BE WRITTEN TO
!+                THE RESULTS FILE OR TO THE LISTING.
!
!history  J-M HERVOUET (LNHE)
!+        24/11/2009
!+        V6P0
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
!history  C. GOEURY (EDF R&D LNHE)
!+        25/07/2013
!+        V6P3
!+   Sum of HAP in oilspills has been added.
!
!history  J-M HERVOUET EDF R&D, LNHE)
!+        02/01/2014
!+        V7P0
!+   Securing bound checking in parallelism.
!
!history  J-M HERVOUET EDF R&D, LNHE)
!+        28/10/2014
!+        V7P0
!+   Initialising Lagrangian drifts for iteration 0 in case they are
!+   in outputs.
!
!history  R. ATA & J-M HERVOUET (EDF LAB, LNHE)
!+        10/06/2015
!+        V7P1
!+   Now all the variables asked for graphic printouts are written for
!+   remarkable points.
!
!history  R. ATA (EDF LAB, LNHE)
!+        11/01/2016
!+        V7P2
!+   Now preres gives instruction to bief_desimp to write graphical
!+   results (through leo and imp)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL ,INTENT(INOUT)::IMP,LEO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL DEJA1,DEJA2
!
      INTEGER LTT,N,IMAX,I,II,JJ
!
      DOUBLE PRECISION HHH,XMAX,HHPLG,COEF,ARG1
      DOUBLE PRECISION, PARAMETER:: EPSS=1.E-10
      DOUBLE PRECISION GPRDTIME,LPRDTIME,RESTE
!
      INTRINSIC MAX,SQRT,CEILING
!
      DOUBLE PRECISION P_DMAX,P_DMIN
      EXTERNAL         P_DMAX,P_DMIN
!
!-----------------------------------------------------------------------
!
      DATA DEJA1/.FALSE./
      DATA DEJA2/.FALSE./
      SAVE DEJA1,DEJA2
!
!-----------------------------------------------------------------------
!
!     THE OUTPUT VARIABLES ARE BUILT ONLY IF NECESSARY, HENCE THE
!     FOLLOWING TESTS, WHICH MUST BE THE SAME AS IN BIEF_DESIMP (BIEF LIBRARY)
!
      IMP=.FALSE.
      LEO=.FALSE.
!     THIS WILL TRIGGER THE OUTPUT OF LAST TIMESTEP
!     BUT NOT WITH PARAMETER ESTIMATION (LISPRD WOULD STAY AT 1
!     FOR FURTHER COMPUTATIONS)
!      IF(LT.EQ.NIT.AND.ESTIME(1:1).EQ.' ') THEN
!        IMP=.FALSE.
!        LEO=.FALSE.
!      ENDIF
!     Always write the intial conditions
      IF(LT.EQ.0) THEN
        IMP=.TRUE.
        LEO=.TRUE.
        COMPLEO=0
      ELSE
        IF(EQUA(1:15).NE.'SAINT-VENANT VF') THEN
!         FEM
          LTT=(LT/LISPRD)*LISPRD
          IF(LT.EQ.LTT.AND.LT.GE.PTINIL) IMP=.TRUE.
          LTT=(LT/LEOPRD)*LEOPRD
          IF(LT.EQ.LTT.AND.LT.GE.PTINIG) LEO=.TRUE.
!         FOR GRAPHICAL OUTPUTS          
          IF(LEO)COMPLEO=COMPLEO+1
        ELSE
!         FVM
          GPRDTIME=LEOPRD*DTINI
          LPRDTIME=LISPRD*DTINI
          IF(GPRDTIME.LT.EPSS.OR.LPRDTIME.LT.EPSS)THEN
            CALL PLANTE(1)
            STOP
          ENDIF
          IF(LT.GE.PTINIG)THEN
!           GRAPHIC OUTPUT
            LTT=CEILING(AT/GPRDTIME)
            RESTE=(LTT*GPRDTIME-AT)/GPRDTIME
            IF(RESTE.LT.EPSS.OR.ABS(RESTE-1.D0).LT.EPSS.OR.
!                                   CASE WHERE RESTE=1
     &        LT.EQ.NIT)THEN
              LEO=.TRUE.
              COMPLEO=COMPLEO+1
            ENDIF
            
          ENDIF
          IF(LT.GT.PTINIL)THEN
!           LISTING OUTPUT
            LTT=CEILING(AT/LPRDTIME)
            RESTE=(LTT*LPRDTIME-AT)/LPRDTIME
            IF(RESTE.LT.EPSS.OR.ABS(RESTE-1.D0).LT.EPSS.OR.
!                                   CASE WHERE RESTE=1
     &        LT.EQ.NIT)THEN
              IMP=.TRUE.
            ENDIF
          ENDIF
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
! 1)  PART WHICH MUST BE DONE EVEN IF THERE IS NO OUTPUT FOR THIS TIMESTEP
!     BUT ONLY AFTER FIRST TIMESTEP FOR GRAPHIC PRINTOUTS
!
!-----------------------------------------------------------------------
!
      IF(LT.GE.PTINIG) THEN
!
!=======================================================================
! COMPUTES THE MAXIMUM ELEVATION AND ASSOCIATED TIME
!=======================================================================
!
      IF(SORLEO(27).OR.SORIMP(27)) THEN
        IF(.NOT.DEJA1) THEN
          CALL OS('X=Y     ',X=MAXZ ,Y=ZF)
          CALL OS('X=C     ',X=TMAXZ,C=AT)
          DEJA1=.TRUE.
        ELSE
          DO N=1,NPOIN
            XMAX=H%R(N)+ZF%R(N)
!           DRY LAND EXCLUDED (TO AVOID RANDOM TIMES)
            IF(XMAX.GT.MAXZ%R(N).AND.H%R(N).GT.0.01D0) THEN
              MAXZ%R(N)=XMAX
              IF(SORLEO(28).OR.SORIMP(28)) TMAXZ%R(N)=AT
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
! COMPUTES THE MAXIMUM SPEED AND ASSOCIATED TIME
!=======================================================================
!
      IF(SORLEO(29).OR.SORIMP(29)) THEN
        IF(.NOT.DEJA2) THEN
          CALL OS('X=C     ',X=MAXV ,C=0.D0)
          CALL OS('X=C     ',X=TMAXV,C=AT)
          DEJA2=.TRUE.
        ELSE
          DO N=1,NPOIN
            XMAX=SQRT(U%R(N)**2+V%R(N)**2)
!           DRY LAND EXCLUDED (TO AVOID RANDOM TIMES)
            IF(XMAX.GT.MAXV%R(N).AND.H%R(N).GT.0.01D0) THEN
              MAXV%R(N)=XMAX
              IF(SORLEO(30).OR.SORIMP(30)) TMAXV%R(N)=AT
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
! PRINTOUTS FOR THE REMARKABLE POINTS
!=======================================================================
!
      IF(LT.EQ.NIT.AND.NPTS.GT.0) THEN
        DO I=27,30
!         BEWARE : HERE SORLEO IS USED INSTEAD OF SORIMP
          IF(SORLEO(I)) THEN
            WRITE(LU,*) ' '
            WRITE(LU,*) ' '
            WRITE(LU,*) ' '
            WRITE(LU,*) TEXTE(I)(1:16)
            WRITE(LU,*) ' '
            DO N=1,NPTS
!             IN PARALLEL POINT DOES NOT ALWAYS EXIST, MAYBE ELSEWHERE
              IF(NCSIZE.GT.1) THEN
                WRITE(LU,*) NAME_PTS(N),' : ',
     &                    P_DMIN(VARSOR%ADR(I)%P%R(LIST_PTS(N)))+
     &                    P_DMAX(VARSOR%ADR(I)%P%R(LIST_PTS(N)))
              ELSE
                WRITE(LU,*) NAME_PTS(N),' : ',
     &                                    VARSOR%ADR(I)%P%R(LIST_PTS(N))
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
!     CASE WHERE OUTINI=.TRUE. : PRIORITY ON PTINIG, VALUES FOR LT=0
!     OTHERWISE THEY WOULD NOT BE INITIALISED
      IF(SORLEO(27).OR.SORIMP(27)) CALL OS('X=Y     ',X=MAXZ ,Y=ZF)
      IF(SORLEO(28).OR.SORIMP(28)) CALL OS('X=C     ',X=TMAXZ,C=AT)
      IF(SORLEO(29).OR.SORIMP(29)) CALL OS('X=C     ',X=MAXV ,C=0.D0)
      IF(SORLEO(30).OR.SORIMP(30)) CALL OS('X=C     ',X=TMAXV,C=AT)
!
!     ENDIF FOR : IF(LT.GE.PTINIG) THEN
      ENDIF
!
!-----------------------------------------------------------------------
!
! 2)  PART WHICH MUST BE DONE ONLY IF THERE IS AN OUTPUT FOR THIS TIMESTEP
!
!-----------------------------------------------------------------------
!
!     NO PRINTOUT REQUIRED (LISTING OR RESULT FILE): EXITS
      IF(.NOT.(LEO.OR.IMP)) GO TO 1000
!
!
!=======================================================================
! COMPUTES CELERITY (IN FU, SEE BLOCK: VARSOR)
!=======================================================================
!
      IF((LEO.AND.SORLEO(3)).OR.(IMP.AND.SORIMP(3))) THEN
        CALL CPSTVC(ZF,FU)
        DO N=1,NPOIN
          FU%R(N) = SQRT ( GRAV * MAX(H%R(N),0.D0) )
        ENDDO
      ENDIF
!
!=======================================================================
! COMPUTES FREE SURFACE ELEVATION (= H + ZF, IN FV)
!=======================================================================
!
      IF((LEO.AND.SORLEO(5)).OR.(IMP.AND.SORIMP(5))) THEN
        CALL CPSTVC(ZF,FV)
        DO N=1,NPOIN
          FV%R(N) = H%R(N)+ZF%R(N)
        ENDDO
      ENDIF
!
!=======================================================================
! COMPUTES FROUDE NUMBER
!=======================================================================
!
      IF((LEO.AND.SORLEO(7)).OR.(IMP.AND.SORIMP(7))) THEN
        CALL CPSTVC(ZF,T2)
        DO N=1,NPOIN
          HHH = MAX( H%R(N) , 1.D-8 )
          T2%R(N) = SQRT (( U%R(N)**2 + V%R(N)**2 ) / ( HHH*GRAV ))
        ENDDO
      ENDIF
!
!=======================================================================
! COMPUTES FLOWRATE
!=======================================================================
!
      IF((LEO.AND.SORLEO(8)).OR.(IMP.AND.SORIMP(8))) THEN
        CALL CPSTVC(ZF,T3)
        DO N=1,NPOIN
          T3%R(N) = SQRT (U%R(N)**2 + V%R(N)**2) * H%R(N)
        ENDDO
      ENDIF
!
!=======================================================================
! COMPUTES FLOWRATE ALONG X
!=======================================================================
!
      IF((LEO.AND.SORLEO(13)).OR.(IMP.AND.SORIMP(13))) THEN
        CALL CPSTVC(ZF,T4)
        DO N=1,NPOIN
          T4%R(N)=H%R(N)*U%R(N)
        ENDDO
      ENDIF
!
!=======================================================================
! COMPUTES FLOWRATE ALONG Y
!=======================================================================
!
      IF((LEO.AND.SORLEO(14)).OR.(IMP.AND.SORIMP(14))) THEN
        CALL CPSTVC(ZF,T5)
        DO N=1,NPOIN
          T5%R(N)=H%R(N)*V%R(N)
        ENDDO
      ENDIF
!
!=======================================================================
! COMPUTES SPEED
!=======================================================================
!
      IF((LEO.AND.SORLEO(15)).OR.(IMP.AND.SORIMP(15))) THEN
        CALL OS( 'X=N(Y,Z)' , X=T6 , Y=U , Z=V )
      ENDIF
!
!=======================================================================
! COMPUTES COURANT NUMBER
!=======================================================================
!
      IF((LEO.AND.SORLEO(22)).OR.(IMP.AND.SORIMP(22))) THEN
!                             IELM
        CALL CFLPSI(T9,U,V,DT,11,MESH,MSK,MASKEL)
        CALL MAXI(XMAX,IMAX,T9%R,NPOIN)
        IF(NCSIZE.GT.1) THEN
          IF(LNG.EQ.1) WRITE(LU,78) P_DMAX(XMAX)
          IF(LNG.EQ.2) WRITE(LU,79) P_DMAX(XMAX)
        ELSE
          IF(LNG.EQ.1) WRITE(LU,78) XMAX
          IF(LNG.EQ.2) WRITE(LU,79) XMAX
        ENDIF
78      FORMAT(1X,'PRERES : NOMBRE DE COURANT MAXIMUM :',G16.7)
79      FORMAT(1X,'PRERES: MAXIMUM COURANT NUMBER: ',G16.7)
      ENDIF
!
!=======================================================================
! COMPUTES EXACT WATER DEPTH
!=======================================================================
!
      IF((LEO.AND.SORLEO(23)).OR.(IMP.AND.SORIMP(23))) THEN
        CALL EXACTE(PRIVE%ADR(1)%P%R,X,NPOIN)
      ENDIF
!=======================================================================
! COMPUTES EXACT FREE SURFACE
!=======================================================================
!
      IF((LEO.AND.SORLEO(25)).OR.(IMP.AND.SORIMP(25))) THEN
        CALL OV('X=Y+Z   ' ,PRIVE%ADR(3)%P%R,
     &                      PRIVE%ADR(1)%P%R,ZF%R,0.D0,NPOIN)
      ENDIF
!=======================================================================
! COMPUTES FRICTION SPEED
!=======================================================================
!
      IF((LEO.AND.SORLEO(31)).OR.(IMP.AND.SORIMP(31))) THEN
        CALL CPSTVC(CF,T7)
        DO N=1,NPOIN
          T7%R(N) = SQRT(0.5D0*CF%R(N)*(U%R(N)**2+V%R(N)**2))
        ENDDO
      ENDIF
!
!=======================================================================
!
1000  CONTINUE
!
!=======================================================================
! HARMONIC ANALYSIS USING LEAST MEAN ERROR SQUARE METHOD
!=======================================================================
!
      IF(NPERIAF.GT.0) CALL SPECTRE
!
!=======================================================================
!
      RETURN
      END
!                    ***************************
                     SUBROUTINE NOMVAR_TELEMAC2D
!                    ***************************
!
     &(TEXTE,TEXTPR,MNEMO,NPERIAF,NTRAC,NAMETRAC,N_NAMES_PRIV,
     & NAMES_PRIVE,SECCURRENTS)
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    GIVES THE VARIABLE NAMES FOR THE RESULTS AND GEOMETRY
!+                FILES (IN TEXTE) AND FOR THE PREVIOUS COMPUTATION
!+                RESULTS FILE (IN TEXTPR).
!+
!+                TEXTE AND TEXTPR ARE GENERALLY EQUAL EXCEPT IF THE
!+                PREVIOUS COMPUTATION COMES FROM ANOTHER SOFTWARE.
!
!history  J-M HERVOUET (LNHE)
!+        31/08/2007
!+        V5P8
!+   First version.
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
!history  D WANG & P TASSI (LNHE)
!+        10/07/2014
!+        V7P0
!+   Secondary flow correction: add variables
!+   tau_s, Omega/h and r^{-1} for visualization
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        27/07/2015
!+        V7P1
!+   Now taking into account names of private arrays given by user.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MNEMO          |<--| MNEMONIC FOR 'VARIABLES FOR GRAPHIC OUTPUTS'
!| N_NAMES_PRIV   |-->| NUMBER OF NAMES OF PRIVATE VARIABLES GIVEN
!| NAMES_PRIVE    |-->| NAME OF PRIVATE VARIABLES GIVEN BY USER
!| NAMETRAC       |-->| NAME OF TRACERS (GIVEN BY KEYWORDS)
!| NPERIAF        |-->| NUMBER OF PERIODS FOR FOURRIER ANALYSIS
!| NTRAC          |-->| NUMBER OF TRACERS
!| TEXTE          |<--| SEE ABOVE
!| TEXTPR         |<--| SEE ABOVE
!| SECCURRENTS    |-->| IF YES SECONDARY CURRENTS COMPUTED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)              :: NPERIAF,NTRAC,N_NAMES_PRIV
      CHARACTER(LEN=32), INTENT(INOUT) :: TEXTE(*),TEXTPR(*)
      CHARACTER(LEN=8),  INTENT(INOUT) :: MNEMO(*)
      CHARACTER(LEN=32), INTENT(IN)    :: NAMETRAC(32),NAMES_PRIVE(4)
      LOGICAL, INTENT(IN)              :: SECCURRENTS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=2) I_IN_2_LETTERS(34)
      DATA I_IN_2_LETTERS /'1 ','2 ','3 ','4 ','5 ','6 ','7 ','8 ','9 ',
     &                     '10','11','12','13','14','15','16','17','18',
     &                     '19','20','21','22','23','24','25','26','27',
     &                     '28','29','30','31','32','33','34'/
      INTEGER I
!
!-----------------------------------------------------------------------
!
!  ENGLISH
!
      IF(LNG.EQ.2) THEN
!
      TEXTE (1 ) = 'VELOCITY U      M/S             '
      TEXTE (2 ) = 'VELOCITY V      M/S             '
      TEXTE (3 ) = 'CELERITY        M/S             '
      TEXTE (4 ) = 'WATER DEPTH     M               '
      TEXTE (5 ) = 'FREE SURFACE    M               '
      TEXTE (6 ) = 'BOTTOM          M               '
      TEXTE (7 ) = 'FROUDE NUMBER                   '
      TEXTE (8 ) = 'SCALAR FLOWRATE M2/S            '
      TEXTE (9 ) = 'EX TRACER                       '
      TEXTE (10) = 'TURBULENT ENERG.JOULE/KG        '
      TEXTE (11) = 'DISSIPATION     WATT/KG         '
      TEXTE (12) = 'VISCOSITY       M2/S            '
      TEXTE (13) = 'FLOWRATE ALONG XM2/S            '
      TEXTE (14) = 'FLOWRATE ALONG YM2/S            '
      TEXTE (15) = 'SCALAR VELOCITY M/S             '
      TEXTE (16) = 'WIND ALONG X    M/S             '
      TEXTE (17) = 'WIND ALONG Y    M/S             '
      TEXTE (18) = 'AIR PRESSURE    PASCAL          '
      TEXTE (19) = 'BOTTOM FRICTION                 '
      TEXTE (20) = 'DRIFT ALONG X   M               '
      TEXTE (21) = 'DRIFT ALONG Y   M               '
      TEXTE (22) = 'COURANT NUMBER                  '
      TEXTE (23) = 'EXACT DEPTH     M               '
      TEXTE (24) = 'VARIABLE 24     UNIT   ??       '
      TEXTE (25) = 'EXACT ELEVATION M               '
      TEXTE (26) = 'VARIABLE 26     UNIT   ??       '
      TEXTE (27) = 'HIGH WATER MARK M               '
      TEXTE (28) = 'HIGH WATER TIME S               '
      TEXTE (29) = 'HIGHEST VELOCITYM/S             '
      TEXTE (30) = 'TIME OF HIGH VELS               '
      TEXTE (31) = 'FRICTION VEL.   M/S             '
!
! TEXTPR IS USED TO READ PREVIOUS COMPUTATION FILES.
! IN GENERAL TEXTPR=TEXTE BUT YOU CAN FOLLOW UP A COMPUTATION
! FROM ANOTHER CODE WITH DIFFERENT VARIABLE NAMES, WHICH MUST
! BE GIVEN HERE:
!
      TEXTPR (1 ) = 'VELOCITY U      M/S             '
      TEXTPR (2 ) = 'VELOCITY V      M/S             '
      TEXTPR (3 ) = 'CELERITY        M/S             '
      TEXTPR (4 ) = 'WATER DEPTH     M               '
      TEXTPR (5 ) = 'FREE SURFACE    M               '
      TEXTPR (6 ) = 'BOTTOM          M               '
      TEXTPR (7 ) = 'FROUDE NUMBER                   '
      TEXTPR (8 ) = 'SCALAR FLOWRATE M2/S            '
      TEXTPR (9 ) = 'EX TRACER                       '
      TEXTPR (10) = 'TURBULENT ENERG.JOULE/KG        '
      TEXTPR (11) = 'DISSIPATION     WATT/KG         '
      TEXTPR (12) = 'VISCOSITY       M2/S            '
      TEXTPR (13) = 'FLOWRATE ALONG XM2/S            '
      TEXTPR (14) = 'FLOWRATE ALONG YM2/S            '
      TEXTPR (15) = 'SCALAR VELOCITY M/S             '
      TEXTPR (16) = 'WIND ALONG X    M/S             '
      TEXTPR (17) = 'WIND ALONG Y    M/S             '
      TEXTPR (18) = 'AIR PRESSURE    PASCAL          '
      TEXTPR (19) = 'BOTTOM FRICTION                 '
      TEXTPR (20) = 'DRIFT ALONG X   M               '
      TEXTPR (21) = 'DRIFT ALONG Y   M               '
      TEXTPR (22) = 'COURANT NUMBER                  '
      TEXTPR (23) = 'VARIABLE 23     UNIT   ??       '
      TEXTPR (24) = 'VARIABLE 24     UNIT   ??       '
      TEXTPR (25) = 'VARIABLE 25     UNIT   ??       '
      TEXTPR (26) = 'VARIABLE 26     UNIT   ??       '
      TEXTPR (27) = 'HIGH WATER MARK M               '
      TEXTPR (28) = 'HIGH WATER TIME S               '
      TEXTPR (29) = 'HIGHEST VELOCITYM/S             '
      TEXTPR (30) = 'TIME OF HIGH VELS               '
      TEXTPR (31) = 'FRICTION VEL.   M/S             '
!
!-----------------------------------------------------------------------
!
!  FRANCAIS OU AUTRE
!
      ELSE
!
      TEXTE (1 ) = 'VITESSE U       M/S             '
      TEXTE (2 ) = 'VITESSE V       M/S             '
      TEXTE (3 ) = 'CELERITE        M/S             '
      TEXTE (4 ) = 'HAUTEUR D''EAU   M               '
      TEXTE (5 ) = 'SURFACE LIBRE   M               '
      TEXTE (6 ) = 'FOND            M               '
      TEXTE (7 ) = 'FROUDE                          '
      TEXTE (8 ) = 'DEBIT SCALAIRE  M2/S            '
      TEXTE (9 ) = 'EX TRACEUR                      '
      TEXTE (10) = 'ENERGIE TURBUL. JOULE/KG        '
      TEXTE (11) = 'DISSIPATION     WATT/KG         '
      TEXTE (12) = 'VISCOSITE TURB. M2/S            '
      TEXTE (13) = 'DEBIT SUIVANT X M2/S            '
      TEXTE (14) = 'DEBIT SUIVANT Y M2/S            '
      TEXTE (15) = 'VITESSE SCALAIREM/S             '
      TEXTE (16) = 'VENT X          M/S             '
      TEXTE (17) = 'VENT Y          M/S             '
      TEXTE (18) = 'PRESSION ATMOS. PASCAL          '
      TEXTE (19) = 'FROTTEMENT                      '
      TEXTE (20) = 'DERIVE EN X     M               '
      TEXTE (21) = 'DERIVE EN Y     M               '
      TEXTE (22) = 'NBRE DE COURANT                 '
      TEXTE (23) = 'HAUTEUR EXACTE  M               '
      TEXTE (24) = 'VARIABLE 24     UNITES ??       '
      TEXTE (25) = 'SURFACE EXACTE  M               '
      TEXTE (26) = 'VARIABLE 26     UNITES ??       '
      TEXTE (27) = 'COTE MAXIMUM    M               '
      TEXTE (28) = 'TEMPS COTE MAXI S               '
      TEXTE (29) = 'VITESSE MAXIMUM M/S             '
      TEXTE (30) = 'T VITESSE MAXI  S               '
      TEXTE (31) = 'VITESSE DE FROT.M/S             '
!
! TEXTPR SERT A LA LECTURE DES FICHIERS DE CALCULS PRECEDENTS
! A PRIORI TEXTPR=TEXTE MAIS ON PEUT ESSAYER DE FAIRE UNE SUITE
! DE CALCUL A PARTIR D'UN AUTRE CODE.
!
      TEXTPR (1 ) = 'VITESSE U       M/S             '
      TEXTPR (2 ) = 'VITESSE V       M/S             '
      TEXTPR (3 ) = 'CELERITE        M/S             '
      TEXTPR (4 ) = 'HAUTEUR D''EAU   M               '
      TEXTPR (5 ) = 'SURFACE LIBRE   M               '
      TEXTPR (6 ) = 'FOND            M               '
      TEXTPR (7 ) = 'FROUDE                          '
      TEXTPR (8 ) = 'DEBIT SCALAIRE  M2/S            '
      TEXTPR (9 ) = 'EX TRACEUR                      '
      TEXTPR (10) = 'ENERGIE TURBUL. JOULE/KG        '
      TEXTPR (11) = 'DISSIPATION     WATT/KG         '
      TEXTPR (12) = 'VISCOSITE TURB. M2/S            '
      TEXTPR (13) = 'DEBIT SUIVANT X M2/S            '
      TEXTPR (14) = 'DEBIT SUIVANT Y M2/S            '
      TEXTPR (15) = 'VITESSE SCALAIREM/S             '
      TEXTPR (16) = 'VENT X          M/S             '
      TEXTPR (17) = 'VENT Y          M/S             '
      TEXTPR (18) = 'PRESSION ATMOS. PASCAL          '
      TEXTPR (19) = 'FROTTEMENT                      '
      TEXTPR (20) = 'DERIVE EN X     M               '
      TEXTPR (21) = 'DERIVE EN Y     M               '
      TEXTPR (22) = 'NBRE DE COURANT                 '
      TEXTPR (23) = 'VARIABLE 23     UNITES ??       '
      TEXTPR (24) = 'VARIABLE 24     UNITES ??       '
      TEXTPR (25) = 'VARIABLE 25     UNITES ??       '
      TEXTPR (26) = 'VARIABLE 26     UNITES ??       '
      TEXTPR (27) = 'COTE MAXIMUM    M               '
      TEXTPR (28) = 'TEMPS COTE MAXI S               '
      TEXTPR (29) = 'VITESSE MAXIMUM M/S             '
      TEXTPR (30) = 'T VITESSE MAXI  S               '
      TEXTPR (31) = 'VITESSE DE FROT.M/S             '
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!   ALIASES FOR THE VARIABLES IN THE STEERING FILE
!
!     UVCHSBFQTKEDIJMXYPWAGLNORZ
!     VELOCITY COMPONENT U
      MNEMO(1)   = 'U       '
!     VELOCITY COMPONENT V
      MNEMO(2)   = 'V       '
!     CELERITY
      MNEMO(3)   = 'C       '
!     WATER DEPTH
      MNEMO(4)   = 'H       '
!     FREE SURFACE ELEVATION
      MNEMO(5)   = 'S       '
!     BOTTOM ELEVATION
      MNEMO(6)   = 'B       '
!     FROUDE
      MNEMO(7)   = 'F       '
!     FLOW RATE
      MNEMO(8)   = 'Q       '
!     EX TRACER
      MNEMO(9)   = '?       '
!     TURBULENT ENERGY
      MNEMO(10)   = 'K       '
!     DISSIPATION
      MNEMO(11)   = 'E       '
!     TURBULENT VISCOSITY
      MNEMO(12)   = 'D       '
!     FLOWRATE ALONG X
      MNEMO(13)   = 'I       '
!     FLOWRATE ALONG Y
      MNEMO(14)   = 'J       '
!     SPEED
      MNEMO(15)   = 'M       '
!     WIND COMPONENT X
      MNEMO(16)   = 'X       '
!     WIND COMPONENT Y
      MNEMO(17)   = 'Y       '
!     ATMOSPHERIC PRESSURE
      MNEMO(18)   = 'P       '
!     FRICTION
      MNEMO(19)   = 'W       '
!     DRIFT IN X
      MNEMO(20)   = 'A       '
!     DRIFT IN Y
      MNEMO(21)   = 'G       '
!     COURANT NUMBER
      MNEMO(22)   = 'L       '
!     VARIABLE 23
      MNEMO(23)   = 'N       '
!     VARIABLE 24
      MNEMO(24)   = 'O       '
!     VARIABLE 25
      MNEMO(25)   = 'R       '
!     VARIABLE 26
      MNEMO(26)   = 'Z       '
!     VARIABLE 27
      MNEMO(27)   = 'MAXZ    '
!     VARIABLE 28
      MNEMO(28)   = 'TMXZ    '
!     VARIABLE 29
      MNEMO(29)   = 'MAXV    '
!     VARIABLE 30
      MNEMO(30)   = 'TMXV    '
!     VARIABLE 31
      MNEMO(31)   = 'US      '
!
      MNEMO(32)   = 'TAU_S   '
!
      MNEMO(33)   = '1/R     '
!
!-----------------------------------------------------------------------
!
!     FOURIER ANALYSES
!
      IF(NPERIAF.GT.0) THEN
        DO I=1,NPERIAF
          IF(LNG.EQ.1) THEN
            TEXTE(34+NTRAC+2*(I-1)) =  'AMPLI PERIODE '
     &                         //I_IN_2_LETTERS(I)
     &                         //'M               '
            TEXTE(35+NTRAC+2*(I-1)) =  'PHASE PERIODE '
     &                         //I_IN_2_LETTERS(I)
     &                         //'DEGRES          '
            TEXTPR(34+NTRAC+2*(I-1)) =  'AMPLI PERIODE '
     &                         //I_IN_2_LETTERS(I)
     &                         //'M               '
            TEXTPR(35+NTRAC+2*(I-1)) =  'PHASE PERIODE '
     &                         //I_IN_2_LETTERS(I)
     &                         //'DEGRES          '
          ELSE
            TEXTE(34+NTRAC+2*(I-1)) =  'AMPLI PERIOD  '
     &                         //I_IN_2_LETTERS(I)
     &                         //'M               '
            TEXTE(35+NTRAC+2*(I-1)) =  'PHASE PERIOD  '
     &                         //I_IN_2_LETTERS(I)
     &                         //'DEGRES          '
            TEXTPR(34+NTRAC+2*(I-1)) =  'AMPLI PERIOD  '
     &                         //I_IN_2_LETTERS(I)
     &                         //'M               '
            TEXTPR(35+NTRAC+2*(I-1)) =  'PHASE PERIOD  '
     &                         //I_IN_2_LETTERS(I)
     &                         //'DEGRES          '
          ENDIF
          MNEMO(34+NTRAC+2*(I-1)) = 'AMPL'//I_IN_2_LETTERS(I)//'  '
          MNEMO(35+NTRAC+2*(I-1)) = 'PHAS'//I_IN_2_LETTERS(I)//'  '
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     TRACERS
!
      IF(NTRAC.GT.0) THEN
        DO I=1,NTRAC
          TEXTE(33+I)  = NAMETRAC(I)
          TEXTPR(33+I) = NAMETRAC(I)
          MNEMO(33+I)  = 'T'//I_IN_2_LETTERS(I)//'   '
        ENDDO
!       OMEGA FOR SECONDARY CURRENTS
        IF(SECCURRENTS) THEN
          TEXTE(33+NTRAC) = NAMETRAC(NTRAC)
          TEXTPR(33+NTRAC)= NAMETRAC(NTRAC)
          MNEMO(33+NTRAC) = 'OMEGA   '
        ENDIF
      ENDIF
      IF(N_NAMES_PRIV.GT.0) THEN
        DO I=1,N_NAMES_PRIV
          TEXTE(22+I)  = NAMES_PRIVE(I)
          TEXTPR(22+I) = NAMES_PRIVE(I)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    *****************
                     SUBROUTINE EXACTE
!                    *****************
!
     &(H,X,NPOIN)
!
!***********************************************************************
!
! DIRECT INTEGRATION OF THE EQUATION OF GRADUALLY VARIED FLOW
! REF
! 1.Restoration of the contact surface in FORCE-type centred schemes II
! Canestrelli, Toro 2012
! 2.Direct integration of the equation of gradually varied flow
! Venutelli 2004
!
! WARNING: THIS IS THE STEADY STATE SOLUTION
!          HOWEVER IT IS ADDED AT EVERY TIME
!          AND STOCKED IN THE OUTPUT FILE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |     H          |<-- |  WATER DEPTH                                 |
! |     X          | -->|  ABSCISSAE                                   |
! |   NPOIN        | -->|  NUMBER OF MESH POINTS                       |
! |________________|____|______________________________________________|
!**********************************************************************
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER I,J,NPOIN
      INTEGER, PARAMETER :: ITMAX = 4000
      DOUBLE PRECISION  H(NPOIN),X(NPOIN)
      DOUBLE PRECISION, PARAMETER :: UNT=1.D0/3.D0
      DOUBLE PRECISION, PARAMETER :: UNDIX=1.D0/10.D0
      DOUBLE PRECISION, PARAMETER :: UNQ=1.D0/4.D0
      DOUBLE PRECISION, PARAMETER :: S=1.8536585365*1.D-5
      DOUBLE PRECISION, PARAMETER :: N=0.0143D0,DOMAIN_L=410000.D0
      DOUBLE PRECISION, PARAMETER :: YOUT_INI=11.6D0
      DOUBLE PRECISION, PARAMETER :: EPSIL = 1.D-5
      DOUBLE PRECISION Q,YC,YN,YOUT,XOUT,DY,ETA,ETAC,ETAT
      DOUBLE PRECISION ALFA1,ALFA2,BETA1,BETA2
      DOUBLE PRECISION F1,F2,F3,F4,Z1,Z2,Z3,Z4
      DOUBLE PRECISION F1T,F2T,F3T,F4T,Z1T,Z2T,Z3T,Z4T
      DOUBLE PRECISION F,G,FT,GT,GAMMA,DXIN
      DOUBLE PRECISION XIN(ITMAX),YIN(ITMAX)
!
      INTRINSIC SQRT,ABS,LOG,ATAN
!
! VARIABLES: S -> BED SLOPE; N -> COEFFICIENT OF MANNING
!-----------------------------------------------------------------------
!
! INITIALIZE X ET YIN
!
      DO I=1,ITMAX
        XIN(I)=1.D10
        YIN(I)=0.D0
      ENDDO
!
! SCALAR FLOWRATE (m2/s) IN THE FLUME
!
      Q=6000.D0/450.D0
!
! CRITICAL WATER DEPTH
!
      YC=(Q**2/9.81D0)**UNT
!
! WATER DEPTH FOR UNIFORM REGIME
!
      YN=((Q*N)/SQRT(S))**(3.D0/5.D0)
!
! SUBCRITICAL REGIME: WE START COMPUTATION FROM DOWNSTREAM
!
! WATER DEPTH AND X(FLUME LENGTH) AT THE OUTLET (GIVEN DATA)
!
      YOUT = YOUT_INI
      XOUT = DOMAIN_L
!
! INCREMENT OF WATER DEPTH
!
      DY = 0.0008D0
      ETAC=YC/YN
      YIN(1)=YOUT
      XIN(1)=XOUT
! ALPHAs AND BETAs
      ALFA1=UNDIX*SQRT(0.5D0*(5.D0+SQRT(5.D0)))
      ALFA2=UNDIX*SQRT(0.5D0*(5.D0-SQRT(5.D0)))
!
      BETA1=1.D0-SQRT(5.D0)
      BETA2=1.D0+SQRT(5.D0)
!
      DO 10 I=2,ITMAX
!
        YIN(I)=YOUT-DY
        ETA=YIN(I)/YN
        ETAT=YOUT/YN
!
        F1=2.D0*SQRT(2.D0/(5.D0-SQRT(5.D0)))*(ETA**UNT-UNQ*BETA2)
        F4=2.D0*SQRT(2.D0/(5.D0-SQRT(5.D0)))*(ETA**UNT+UNQ*BETA2)
!
        F2=2.D0*SQRT(2.D0/(5.D0+SQRT(5.D0)))*(ETA**UNT+UNQ*BETA1)
        F3=2.D0*SQRT(2.D0/(5.D0+SQRT(5.D0)))*(ETA**UNT-UNQ*BETA1)
!
        Z1=1.D0+0.5D0*BETA2*ETA**UNT+ETA**(2.D0/3.D0)
        Z4=1.D0-0.5D0*BETA2*ETA**UNT+ETA**(2.D0/3.D0)
!
        Z2=1.D0-0.5D0*BETA1*ETA**UNT+ETA**(2.D0/3.D0)
        Z3=1.D0+0.5D0*BETA1*ETA**UNT+ETA**(2.D0/3.D0)
!
        F1T=2.D0*SQRT(2.D0/(5.D0-SQRT(5.D0)))*(ETAT**UNT-UNQ*BETA2)
        F4T=2.D0*SQRT(2.D0/(5.D0-SQRT(5.D0)))*(ETAT**UNT+UNQ*BETA2)
!
        F2T=2.D0*SQRT(2.D0/(5.D0+SQRT(5.D0)))*(ETAT**UNT+UNQ*BETA1)
        F3T=2.D0*SQRT(2.D0/(5.D0+SQRT(5.D0)))*(ETAT**UNT-UNQ*BETA1)
!
        Z1T=1.D0+0.5D0*BETA2*ETAT**UNT+ETAT**(2.D0/3.D0)
        Z4T=1.D0-0.5D0*BETA2*ETAT**UNT+ETAT**(2.D0/3.D0)
!
        Z2T=1.D0-0.5D0*BETA1*ETAT**UNT+ETAT**(2.D0/3.D0)
        Z3T=1.D0+0.5D0*BETA1*ETAT**UNT+ETAT**(2.D0/3.D0)
!
        F=ALFA1*(ATAN(F1)+ATAN(F4))-ALFA2*(ATAN(F2)+ATAN(F3))+
     &    (1.D0/40.D0)*(BETA1*(LOG(ABS(Z1))-LOG(ABS(Z4)))-
     &    BETA2*(LOG(ABS(Z2))-LOG(ABS(Z3))))-
     &    UNDIX*(LOG(ABS(ETA**UNT-1.D0))-LOG(ABS(ETA**UNT+1.D0)))
!
        G=ALFA2*(ATAN(F1)-ATAN(F4))-ALFA1*(ATAN(F2)-ATAN(F3))
     &    +(1.D0/40.0D0)*(BETA2*(LOG(ABS(Z1))+LOG(ABS(Z4)))
     &    +BETA1*(LOG(ABS(Z2))+LOG(ABS(Z3))))
     &    -UNDIX*(LOG(ABS(ETA**UNT-1.D0))-LOG(ABS(ETA**UNT+1.D0)))
!
        FT=ALFA1*(ATAN(F1T)+ATAN(F4T))-ALFA2*(ATAN(F2T)+ATAN(F3T))
     &    +(1.D0/40.D0)*(BETA1*(LOG(ABS(Z1T))-LOG(ABS(Z4T)))
     &    -BETA2*(LOG(ABS(Z2T))-LOG(ABS(Z3T))))
     &    -UNDIX*(LOG(ABS(ETAT**UNT-1.D0))-LOG(ABS(ETAT**UNT+1.D0)))
!
        GT=ALFA2*(ATAN(F1T)-ATAN(F4T))-ALFA1*(ATAN(F2T)-ATAN(F3T))
     &    +(1.D0/40.0D0)*(BETA2*(LOG(ABS(Z1))+LOG(ABS(Z4)))
     &    +BETA1*(LOG(ABS(Z2T))+LOG(ABS(Z3T))))
     &    -UNDIX*(LOG(ABS(ETAT**UNT-1.D0))-LOG(ABS(ETAT**UNT+1.D0)))
!
        GAMMA=ETA-ETAT+3.D0*(FT-F)+3.D0*(ETAC**3)*(G-GT)
        XIN(I)=XOUT+(YN/S)*GAMMA
        IF(XIN(I).LT.EPSIL) GO TO 10
        XOUT=XIN(I)
        YOUT=YIN(I)
!
10    CONTINUE
!
! LINEAR APPROXIMATION FOR ALL MESH POINTS
!
      DO I=1,NPOIN
!       FIRST OUTPUT POINTS
        IF(ABS(X(I)-DOMAIN_L).LE.EPSIL)THEN
          H(I)=YOUT_INI
          CYCLE
        ENDIF
        DO J=1,ITMAX-1
!
          IF(X(I).LT.XIN(J).AND.X(I).GT.XIN(J+1))THEN
             DXIN=XIN(J)-XIN(J+1)
             H(I)=(1.D0/DXIN)*((XIN(J)-X(I))*YIN(J+1)+
     &                         (X(I)-XIN(J+1))*YIN(J))
            CYCLE
!           DEBUG
            IF(H(I).LE.0.D0)THEN
              WRITE(LU,*)'NEGATIVE H FOR I:',I
              WRITE(LU,*)'H IS EQUAL :',H(I)
              WRITE(LU,*)'BORDED BY: ', XIN(J),XIN(J+1)
              CALL PLANTE(1)
              STOP
            ENDIF
          ENDIF
        ENDDO
      ENDDO
! DEBUG RIADH
!      CALL PLANTE(1)
!      STOP
! END DEBUG
!
!-----------------------------------------------------------------------
!
      RETURN
      END
