C                       *****************
                        SUBROUTINE CONDIN
C                       *****************
C
C***********************************************************************
C TELEMAC 2D VERSION 5.2         19/08/98  J-M HERVOUET TEL: 30 87 80 18
C
C***********************************************************************
C
C     FONCTION  : INITIALISATION DES GRANDEURS PHYSIQUES H, U, V ETC
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |                | -- |  
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C***********************************************************************
C
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION CC
      INTEGER I,ITRAC
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C  
C
C-----------------------------------------------------------------------
C
C   INITIALISATION DU TEMPS
C
      AT = 0.D0
C
C-----------------------------------------------------------------------
C
C   INITIALISATION DES VITESSES : VITESSES NULLES
C
      CALL OS( 'X=C     ' , U , U , U , 0.D0 )
      CALL OS( 'X=C     ' , V , V , V , 0.D0 )
C
C-----------------------------------------------------------------------
C
C   INITIALISATION DE H , LA HAUTEUR D'EAU
C
      IF(CDTINI(1:10).EQ.'COTE NULLE'.OR.
     *   CDTINI(1:14).EQ.'ZERO ELEVATION') THEN
        CALL OS( 'X=C     ' , H , H  , H , 0.D0 )
        CALL OS( 'X=X-Y   ' , H , ZF , H , 0.D0 )
      ELSEIF(CDTINI(1:14).EQ.'COTE CONSTANTE'.OR.
     *       CDTINI(1:18).EQ.'CONSTANT ELEVATION') THEN
        CALL OS( 'X=C     ' , H , H  , H , COTINI )
        CALL OS( 'X=X-Y   ' , H , ZF , H , 0.D0   )
      ELSEIF(CDTINI(1:13).EQ.'HAUTEUR NULLE'.OR.
     *       CDTINI(1:10).EQ.'ZERO DEPTH') THEN
        CALL OS( 'X=C     ' , H , H  , H , 0.D0  )
      ELSEIF(CDTINI(1:17).EQ.'HAUTEUR CONSTANTE'.OR.
     *       CDTINI(1:14).EQ.'CONSTANT DEPTH') THEN
        CALL OS( 'X=C     ' , H , H  , H , HAUTIN )
      ELSEIF(CDTINI(1:13).EQ.'PARTICULIERES'.OR.
     *       CDTINI(1:10).EQ.'PARTICULAR'.OR.
     *       CDTINI(1:07).EQ.'SPECIAL') THEN
C  ZONE A MODIFIER
!        CALL EXACTE(H%R,X,NPOIN)                                                           
C  FIN DE LA ZONE A MODIFIER      
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
C
C-----------------------------------------------------------------------
C
C   INITIALISATION DU TRACEUR
C
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          CALL OS( 'X=C     ' , X=T%ADR(ITRAC)%P , C=TRAC0(ITRAC) )
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C
C INITIALISATION DE LA VISCOSITE
C
      CALL OS( 'X=C     ' , VISC , VISC , VISC , PROPNU )
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C                       *****************
                        SUBROUTINE CORFON
C                       *****************
C
C***********************************************************************
C TELEMAC 2D VERSION 5.1          01/03/90    J-M HERVOUET
C***********************************************************************
C
C  USER SUBROUTINE CORFON
C
C  FUNCTION  : MODIFICATION OF THE BOTTOM TOPOGRAPHY
C
C
C-----------------------------------------------------------------------
C  ARGUMENTS USED IN THE EXAMPLE 
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|_______________________________________________
C |      ZF        |<-->| FOND A MODIFIER.
C |      X,Y,(Z)   | -->| COORDONNEES DU MAILLAGE (Z N'EST PAS EMPLOYE).
C |      A         |<-- | MATRICE
C |      T1,2      | -->| TABLEAUX DE TRAVAIL (DIMENSION NPOIN)
C |      W1        | -->| TABLEAU DE TRAVAIL (DIMENSION 3 * NELEM)
C |      NPOIN     | -->| NOMBRE DE POINTS DU MAILLAGE.
C |      PRIVE     | -->| TABLEAU PRIVE POUR L'UTILISATEUR.
C |      LISFON    | -->| NOMBRE DE LISSAGES DU FOND.
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C
C PROGRAMME APPELANT :
C PROGRAMMES APPELES : RIEN EN STANDARD
C
C***********************************************************************
C
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL MAS
      INTEGER I
C
C-----------------------------------------------------------------------
C
C  LISSAGES EVENTUELS DU FOND
C
      IF(LISFON.GT.0) THEN
C
        MAS=.TRUE.
        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     &              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      DO I=1,NPOIN
         ZF%R(I)=-1.853658536585365D0*1.D-5*X(I)-4.0D0   
      ENDDO
C-----------------------------------------------------------------------
C
      RETURN
      END          
!                    ***************************
                     SUBROUTINE PRERES_TELEMAC2D
!                    ***************************
!
!
!***********************************************************************
! TELEMAC2D   V6P1                                  21/08/2010
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
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL IMP,LEO,DEJA1,DEJA2,DEJA3
!
      INTEGER LTT,N,IMAX,I
!
      DOUBLE PRECISION HHH,XMAX,NF,PI,AMP,PHA
!
      INTRINSIC MAX,SQRT
!
      DOUBLE PRECISION P_DMAX,P_DMIN
      EXTERNAL         P_DMAX,P_DMIN
!
!-----------------------------------------------------------------------
!
      DATA DEJA1/.FALSE./
      DATA DEJA2/.FALSE./
      DATA DEJA3/.FALSE./
      SAVE DEJA1,DEJA2,DEJA3,NF
!
!-----------------------------------------------------------------------
!
!     THE OUTPUT VARIABLES ARE BUILT ONLY IF NECESSARY, HENCE THE
!     FOLLOWING TESTS, WHICH MUST BE THE SAME AS IN BIEF_DESIMP (BIEF LIBRARY)
!
!     THIS WILL TRIGGER THE OUTPUT OF LAST TIMESTEP
!     BUT NOT WITH PARAMETER ESTIMATION (LISPRD WOULD STAY AT 1
!     FOR FURTHER COMPUTATIONS)
      IF(LT.EQ.NIT.AND.ESTIME(1:1).EQ.' ') THEN
        LISPRD=1
        LEOPRD=1
      ENDIF
!
      IMP=.FALSE.
      LEO=.FALSE.
      LTT=(LT/LISPRD)*LISPRD
      IF(LT.EQ.LTT.AND.LT.GE.PTINIL) IMP=.TRUE.
      LTT=(LT/LEOPRD)*LEOPRD
      IF(LT.EQ.LTT.AND.LT.GE.PTINIG) LEO=.TRUE.
!
      IF(LT.EQ.0) THEN
        IMP=OUTINI
        LEO=OUTINI
      ENDIF
!
!-----------------------------------------------------------------------
!
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
     &                       PRIVE%ADR(1)%P%R,ZF%R,0.D0,NPOIN)
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
!
!
!                    ***************************
                     SUBROUTINE NOMVAR_TELEMAC2D
!                    ***************************
!
     &(TEXTE,TEXTPR,MNEMO,NPERIAF,NTRAC,NAMETRAC)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MNEMO          |<--| MNEMONIC FOR 'VARIABLES FOR GRAPHIC OUTPUTS'
!| NAMETRAC       |-->| NAME OF TRACERS (GIVEN BY KEYWORDS)
!| NPERIAF        |-->| NUMBER OF PERIODS FOR FOURRIER ANALYSIS
!| NTRAC          |-->| NUMBER OF TRACERS
!| TEXTE          |<--| SEE ABOVE
!| TEXTPR         |<--| SEE ABOVE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=32), INTENT(INOUT) :: TEXTE(*),TEXTPR(*)
      CHARACTER(LEN=8),  INTENT(INOUT) :: MNEMO(*)
      INTEGER, INTENT(IN)              :: NPERIAF,NTRAC
      CHARACTER(LEN=32), INTENT(IN)    :: NAMETRAC(32)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=2) I_IN_2_LETTERS(32)
      DATA I_IN_2_LETTERS /'1 ','2 ','3 ','4 ','5 ','6 ','7 ','8 ','9 ',
     &                     '10','11','12','13','14','15','16','17','18',
     &                     '19','20','21','22','23','24','25','26','27',
     &                     '28','29','30','31','32'/
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
!-----------------------------------------------------------------------
!
!     FOURIER ANALYSES
!
      IF(NPERIAF.GT.0) THEN
        DO I=1,NPERIAF
          IF(LNG.EQ.1) THEN
            TEXTE(32+NTRAC+2*(I-1)) =  'AMPLI PERIODE '
     &                         //I_IN_2_LETTERS(I)
     &                         //'M               '
            TEXTE(33+NTRAC+2*(I-1)) =  'PHASE PERIODE '
     &                         //I_IN_2_LETTERS(I)
     &                         //'DEGRES          '
            TEXTPR(32+NTRAC+2*(I-1)) =  'AMPLI PERIODE '
     &                         //I_IN_2_LETTERS(I)
     &                         //'M               '
            TEXTPR(33+NTRAC+2*(I-1)) =  'PHASE PERIODE '
     &                         //I_IN_2_LETTERS(I)
     &                         //'DEGRES          '
          ELSE
            TEXTE(32+NTRAC+2*(I-1)) =  'AMPLI PERIOD  '
     &                         //I_IN_2_LETTERS(I)
     &                         //'M               '
            TEXTE(33+NTRAC+2*(I-1)) =  'PHASE PERIOD  '
     &                         //I_IN_2_LETTERS(I)
     &                         //'DEGRES          '
            TEXTPR(32+NTRAC+2*(I-1)) =  'AMPLI PERIOD  '
     &                         //I_IN_2_LETTERS(I)
     &                         //'M               '
            TEXTPR(33+NTRAC+2*(I-1)) =  'PHASE PERIOD  '
     &                         //I_IN_2_LETTERS(I)
     &                         //'DEGRES          '
          ENDIF
          MNEMO(32+NTRAC+2*(I-1)) = 'AMPL'//I_IN_2_LETTERS(I)//'  '
          MNEMO(33+NTRAC+2*(I-1)) = 'PHAS'//I_IN_2_LETTERS(I)//'  '
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     TRACERS
!
      IF(NTRAC.GT.0) THEN
        DO I=1,NTRAC
          TEXTE(31+I)  = NAMETRAC(I)
          TEXTPR(31+I) = NAMETRAC(I)
          MNEMO(31+I)  = 'T'//I_IN_2_LETTERS(I)//'   '
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!
!                    *****************
                     SUBROUTINE EXACTE
!                    *****************
!
     &(H,X,NPOIN) 
!
!***********************************************************************  
!                                                                         
! DIRECT INTEGRATION OF THE EQUATION OF GRADUALLY
!                VARIED FLOW 
! REF 
! 1.Restoration of the contact surface in FORCE-type centred schemes II 
! Canestrelli, Toro 2012
! 2.Direct integration of the equation of gradually varied flow 
! Venutelli 2004          
!                                                                         
! ATTENTION, IL NE S'AGIT ICI QUE DE LA SOLUTION        
! PERMANENTE, QUI EST TOUTEFOIS MISE DANS LE            
! FICHIER DE RESULTATS A TOUS LES PAS DE TEMPS.         
!                                                                         
!-----------------------------------------------------------------------  
!                             ARGUMENTS                                   
! .________________.____.______________________________________________.  
! |      NOM       |MODE|                   ROLE                       |  
! |________________|____|______________________________________________|  
! |     H          |<-- |  HAUTEUR D'EAU.                              |    
! |     X          | -->|  ABSCISSAE                                   |
! |   NPOIN        | -->|  POINTS DU MAILLAGE                          |   
! |________________|____|______________________________________________|  
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)   
!**********************************************************************   
!                                                                         
      IMPLICIT NONE                                                       
!
      INTEGER I,J,NPOIN
      INTEGER, PARAMETER :: ITMAX = 4000
      DOUBLE PRECISION  H(NPOIN),X(NPOIN)
      DOUBLE PRECISION, PARAMETER :: UNT=1.D0/3.D0
      DOUBLE PRECISION, PARAMETER :: UNDIX=1.D0/10.D0
      DOUBLE PRECISION, PARAMETER :: UNQ=1.D0/4.D0
      DOUBLE PRECISION, PARAMETER :: S=1.853658536585365*1.D-5
      DOUBLE PRECISION, PARAMETER :: N=0.0143D0
      DOUBLE PRECISION Q,YC,YN,YOUT,XOUT,DY,ETA,ETAC,ETAT
      DOUBLE PRECISION ALFA1,ALFA2,BETA1,BETA2
      DOUBLE PRECISION F1,F2,F3,F4,Z1,Z2,Z3,Z4
      DOUBLE PRECISION F1T,F2T,F3T,F4T,Z1T,Z2T,Z3T,Z4T
      DOUBLE PRECISION F,G,FT,GT,GAMMA,DXIN      
      DOUBLE PRECISION XIN(ITMAX),YIN(ITMAX)                                                    
!                                                                         
      INTRINSIC SQRT,ABS,DLOG,ATAN                                                                                                 
!
! VARIABLES: S -> PENTE; N -> COEFFICIENT DE MANNING                                                                          
!----------------------------------------------------------------------- 
!
! INITIALISE X ET YIN--->Riadh
      DO I=1,ITMAX
         XIN(I)=10000000000
         YIN(I)=0.D0
      ENDDO
!
! DEBIT SCALAIRE (m2/s) DANS LE CANAL
!
      Q=6000.0D0/450.D0
!
! HAUTEUR CRITIQUE
! 
      YC=(Q**2.D0/9.81D0)**UNT
!
! HAUTEUR REGIME UNIFORME
!
      YN=((Q*N)/SQRT(S))**(3.D0/5.D0)
!
! REGIME FLUVIALE: LE POINT DE DEPART POUR LE DESSIN DU PROFIL EST L'AVAL
!
! HAUTEUR ET X(LONGUEUR DU CANAL) A LA SORTIE SONT DONNEES
!
      YOUT = 11.6D0 
      XOUT = 410000.0D0 
!
! INCREMENT HAUTEUR D'EAU
!
      DY = 0.0008D0
      ETAC=YC/YN
      YIN(1)=YOUT
      XIN(1)=XOUT
!
      DO 10 I=2,ITMAX
!
         YIN(I)=YOUT-DY
         ETA=YIN(I)/YN
         ETAT=YOUT/YN
!
         ALFA1=UNDIX*SQRT(0.5D0*(5.D0+SQRT(5.D0)))
         ALFA2=UNDIX*SQRT(0.5D0*(5.D0-SQRT(5.D0)))
!
         BETA1=1.D0-SQRT(5.D0)
         BETA1=1.D0+SQRT(5.D0)
!
         F1=2.D0*SQRT(2.D0/(5.D0-SQRT(5.D0)))*(ETA**UNT-UNQ*BETA2)
         F4=2.D0*SQRT(2.D0/(5.D0-SQRT(5.D0)))*(ETA**UNT+UNQ*BETA2)
!
         F2=2.D0*SQRT(2.D0/(5.D0+SQRT(5.D0)))*(ETA**UNT+UNQ*BETA1)
         F3=2.D0*SQRT(2.D0/(5.D0+SQRT(5.D0)))*(ETA**UNT-UNQ*BETA1)
!
         Z1=1.D0+0.5*BETA2*ETA**UNT+ETA**(2.D0/3.D0)
         Z4=1.D0-0.5*BETA2*ETA**UNT+ETA**(2.D0/3.D0)
!
         Z2=1.D0-0.5*BETA1*ETA**UNT+ETA**(2.D0/3.D0)
         Z3=1.D0+0.5*BETA1*ETA**UNT+ETA**(2.D0/3.D0)
!
         F1T=2.D0*SQRT(2.D0/(5.D0-SQRT(5.D0)))*(ETAT**UNT-UNQ*BETA2)
         F4T=2.D0*SQRT(2.D0/(5.D0-SQRT(5.D0)))*(ETAT**UNT+UNQ*BETA2)
!
         F2T=2.D0*SQRT(2.D0/(5.D0+SQRT(5.D0)))*(ETAT**UNT+UNQ*BETA1)
         F3T=2.D0*SQRT(2.D0/(5.D0+SQRT(5.D0)))*(ETAT**UNT-UNQ*BETA1)
!
         Z1T=1.D0+0.5*BETA2*ETAT**UNT+ETAT**(2.D0/3.D0)
         Z4T=1.D0-0.5*BETA2*ETAT**UNT+ETAT**(2.D0/3.D0)
!
         Z2T=1.D0-0.5*BETA1*ETAT**UNT+ETAT**(2.D0/3.D0)
         Z3T=1.D0+0.5*BETA1*ETAT**UNT+ETAT**(2.D0/3.D0)
!
         F=ALFA1*(ATAN(F1)+ATAN(F4))-ALFA2*(ATAN(F2)+ATAN(F3))+
     &     (1.D0/40.D0)*(BETA1*(DLOG(ABS(Z1))-DLOG(ABS(Z4)))-
     &     BETA2*(DLOG(ABS(Z2))-DLOG(ABS(Z3))))-
     &     UNDIX*(DLOG(ABS(ETA**UNT-1.D0))-DLOG(ABS(ETA**UNT+1.D0)))
!
         G=ALFA2*(ATAN(F1)-ATAN(F4))-ALFA1*(ATAN(F2)-ATAN(F3))
     &     +(1.D0/40.0D0)*(BETA2*(DLOG(ABS(Z1))+DLOG(ABS(Z4)))
     &     +BETA1*(DLOG(ABS(Z2))+DLOG(ABS(Z3))))
     &     -UNDIX*(DLOG(ABS(ETA**UNT-1.D0))-DLOG(ABS(ETA**UNT+1.D0)))
!
         FT=ALFA1*(ATAN(F1T)+ATAN(F4T))-ALFA2*(ATAN(F2T)+ATAN(F3T))
     &     +(1.D0/40.D0)*(BETA1*(DLOG(ABS(Z1T))-DLOG(ABS(Z4T)))
     &     -BETA2*(DLOG(ABS(Z2T))-DLOG(ABS(Z3T))))
     &     -UNDIX*(DLOG(ABS(ETAT**UNT-1.D0))-DLOG(ABS(ETAT**UNT+1.D0)))
!
         GT=ALFA2*(ATAN(F1T)-ATAN(F4T))-ALFA1*(ATAN(F2T)-ATAN(F3T))
     &     +(1.D0/40.0D0)*(BETA2*(DLOG(ABS(Z1))+DLOG(ABS(Z4)))
     &     +BETA1*(DLOG(ABS(Z2T))+DLOG(ABS(Z3T))))
     &     -UNDIX*(DLOG(ABS(ETAT**UNT-1.D0))-DLOG(ABS(ETAT**UNT+1.D0)))
!
         GAMMA=ETA-ETAT+3.D0*(FT-F)+3.D0*ETAC**3.D0*(G-GT)
         XIN(I)=XOUT+(YN/S)*GAMMA
         IF(XIN(I).LT.1.D-5) GO TO 10
         XOUT=XIN(I)
         YOUT=YIN(I)
!
10    CONTINUE
!
! APPROXIMATION LINEAIRE SUR LES POINTS DU MAILLAGE
!
      DO J=1,ITMAX-1
      !-->Riadh
         DO I=1,NPOIN
            IF(X(I).LT.XIN(J).AND.X(I).GT.XIN(J+1))THEN
               DXIN=XIN(J)-XIN(J+1)
               H(I)=(1.D0/DXIN)*((XIN(J)-X(I))*YIN(J+1)+
     &             (X(I)-XIN(J+1))*YIN(J))
            ENDIF
         ENDDO
      ENDDO 
!-----------------------------------------------------------------------
      RETURN
      END