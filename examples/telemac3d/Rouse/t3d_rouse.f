!                    *****************
                     SUBROUTINE CONDIM
!                    *****************
!
!
!***********************************************************************
! TELEMAC3D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES VELOCITY, DEPTH AND TRACERS.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/1999
!+
!+   FORTRAN95 VERSION
!
!history
!+        20/04/2007
!+
!+   ADDED INITIALISATION OF DPWAVE
!
!history
!+        23/01/2009
!+
!+   ADDED CHECK OF ZSTAR
!
!history
!+        16/03/2010
!+
!+   NEW OPTIONS FOR BUILDING THE MESH IN CONDIM, SEE BELOW
!
!history  J-M HERVOUET(LNHE)
!+        05/05/2010
!+        V6P0
!+   SUPPRESSED INITIALISATION OF DPWAVE
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
      USE INTERFACE_TELEMAC3D, EX_CONDIM => CONDIM
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      INTEGER IPLAN, I,J
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION DELTAZ,AL,AUX
!
!***********************************************************************
!
!     ORIGIN OF TIME
!
      IF(.NOT.SUIT2) AT  = 0.D0
!
!     INITIALISES H, THE WATER DEPTH
!
      IF(.NOT.SUIT2) THEN
!
      IF(CDTINI(1:10).EQ.'COTE NULLE'.OR.
     &   CDTINI(1:14).EQ.'ZERO ELEVATION') THEN
        CALL OS( 'X=C     ' ,X=H,C=0.D0)
        CALL OV( 'X=X-Y   ' , H%R , Z , Z , 0.D0 , NPOIN2 )
      ELSEIF(CDTINI(1:14).EQ.'COTE CONSTANTE'.OR.
     &       CDTINI(1:18).EQ.'CONSTANT ELEVATION') THEN
        CALL OS( 'X=C     ' ,X=H,C=COTINI)
        CALL OV( 'X=X-Y   ' , H%R , Z , Z , 0.D0 , NPOIN2 )
      ELSEIF(CDTINI(1:13).EQ.'HAUTEUR NULLE'.OR.
     &       CDTINI(1:10).EQ.'ZERO DEPTH') THEN
        CALL OS( 'X=C     ' ,X=H,C=0.D0)
      ELSEIF(CDTINI(1:17).EQ.'HAUTEUR CONSTANTE'.OR.
     &       CDTINI(1:14).EQ.'CONSTANT DEPTH') THEN
!       CALL OS( 'X=C     ' ,X=H,C=HAUTIN)
        IF(LNG.EQ.1) WRITE(LU,*) 'HAUTEUR MISE A 0.5'
        IF(LNG.EQ.2) WRITE(LU,*) 'DEPTH IS SET TO 0.5'
        CALL OS('X=C     ',X=H,C=0.5D0)
      ELSEIF(CDTINI(1:13).EQ.'PARTICULIERES'.OR.
     &       CDTINI(1:10).EQ.'PARTICULAR'.OR.
     &       CDTINI(1:07).EQ.'SPECIAL') THEN
!     USER INPUT :
!     PROGRAM HERE SPECIAL INITIAL CONDITIONS ON DEPTH
        IF(LNG.EQ.1) WRITE(LU,10)
        IF(LNG.EQ.2) WRITE(LU,11)
10      FORMAT(1X,'CONDIM : AVEC DES CONDITIONS INITIALES PARTICULIERES'
     &      ,/,1X,'         VOUS DEVEZ MODIFIER CONDIM')
11      FORMAT(1X,'CONDIM : WITH SPECIAL INITIAL CONDITIONS'
     &      ,/,1X,'         YOU HAVE TO MODIFY CONDIM')
        CALL PLANTE(1)
        STOP
!     END OF SPECIAL INITIAL CONDITIONS
!     END OF USER INPUT
      ELSE
        IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'CONDIM : CONDITION INITIALE NON PREVUE : ',CDTINI
        ENDIF
        IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'CONDIM: INITIAL CONDITION UNKNOWN: ',CDTINI
        ENDIF
        STOP
      ENDIF
      ELSE
        IF(LNG.EQ.1) WRITE(LU,*) 'HAUTEUR LUE DANS LE FICHIER BINAIRE 1'
        IF(LNG.EQ.2) WRITE(LU,*) 'DEPTH IS READ IN THE BINARY FILE 1'
      ENDIF
!
!     CLIPS H
!
      DO I=1,NPOIN2
        H%R(I)=MAX(H%R(I),0.D0)
      ENDDO
!
      CALL OS ('X=Y     ',X=HN,Y=H)
!
!-----------------------------------------------------------------------
!
!     DATA TO BUILD VERTICAL COORDINATES IN CALCOT
!
!     TRANSF IS KEYWORD "MESH TRANSFORMATION"
!     IF TRANSF = 0, SUBROUTINE CALCOT MUST BE IMPLEMENTED BY THE USER
!
!     AN EQUIVALENT OF TRANSF MUST BE GIVEN FOR EVERY PLANE:
!
!     POSSIBLE VALUES OF TRANSF_PLANE :
!
!     1 : SIGMA TRANSFORMATION WITH EVENLY SPACED PLANES
!     2 : SIGMA TRANSFORMATION WITH PROPORTIONS GIVEN IN ZSTAR
!     3 : PRESCRIBED ELEVATION GIVEN IN ZPLANE
!
!     STANDARD BELOW IS: EVENLY SPACED PLANES, NO OTHER DATA REQUIRED
!
      DO IPLAN = 1,NPLAN
        TRANSF_PLANE%I(IPLAN)=1
      ENDDO
!
!     OTHER EXAMPLES:
!
!     EXAMPLE 1: ALL PLANES WITH PRESCRIBED ELEVATION
!
!     DO IPLAN = 1,NPLAN
!       TRANSF_PLANE%I(IPLAN)=3
!     ENDDO
!     ZPLANE%R(2)=-7.D0
!     ZPLANE%R(3)=-4.D0
!     ...
!     ZPLANE%R(NPLAN-1)=-0.05D0
!
!
!     EXAMPLE 2: SIGMA TRANSFORMATION WITH GIVEN PROPORTIONS
!
!     DO IPLAN = 1,NPLAN
!       TRANSF_PLANE%I(IPLAN)=2
!     ENDDO
!     ZSTAR%R(2)=0.02D0
!     ZSTAR%R(3)=0.1D0
!     ...
!     ZSTAR%R(NPLAN-1)=0.95D0
!
!
!     EXAMPLE 3: ONE PLANE (NUMBER 4) WITH PRESCRIBED ELEVATION
!                AND SIGMA ELSEWHERE
!
!     DO IPLAN = 1,NPLAN
!       TRANSF_PLANE%I(IPLAN)=1
!     ENDDO
!     TRANSF_PLANE%I(4)=3
!     ZPLANE%R(4)=-3.D0
!
!
!     EXAMPLE 4: ONE PLANE WITH PRESCRIBED ELEVATION
!                AND 2 SIGMA TRANSFORMATIONS, WITH NPLAN=7
!                SIGMA TRANSFORMATIONS ARE MEANT BETWEEN
!                BOTTOM, FIXED ELEVATION PLANES AND FREE SURFACE
!                THE VALUES OF ZSTAR ARE LOCAL FOR EVERY
!                SIGMA TRANSFORMATION: 0. FOR LOWER FIXED PLANE
!                                      1. FOR UPPER FIXED PLANE
!
!     DO IPLAN = 1,7
!       TRANSF_PLANE%I(IPLAN)=2
!     ENDDO
!     TRANSF_PLANE%I(4)=3
!     ZPLANE%R(4)=3.D0
!     ZSTAR%R(2)=0.2D0
!     ZSTAR%R(3)=0.8D0
!     ZSTAR%R(5)=0.1D0
!     ZSTAR%R(6)=0.9D0
!
!-----------------------------------------------------------------------
!
!     CV : MAILLAGE A PAS VARIABLE
!
      DO IPLAN = 1,NPLAN
        TRANSF_PLANE%I(IPLAN)=2
      ENDDO
      AL=1.3D0
      DO IPLAN = 1,NPLAN
        ZSTAR%R(IPLAN) = (1.D0-AL**(IPLAN-1))/(1.D0-AL**(NPLAN-1))
      END DO
!
!***********************************************************************
!
!     COMPUTES ELEVATIONS
!     IF IT IS A CONTINUATION, WILL BE DONE AFTER CALLING 'SUITE'
!
      IF(DEBU) CALL CALCOT(Z,H%R)
!
!***********************************************************************
!
!     INITIALISES VELOCITIES
!
      IF(SUIT2) THEN
        DO I=1,NPLAN
          DO J=1,NPOIN2
           U%R((I-1)*NPOIN2+J)=U2D%R(J)
           V%R((I-1)*NPOIN2+J)=V2D%R(J)
          ENDDO
        ENDDO
      ELSE
!       CALL OS( 'X=0     ' , X=U )
        DO IPLAN=1,NPLAN
          DO I=1,NPOIN2
            IF(IPLAN.EQ.1) THEN
              DELTAZ=(MESH3D%Z%R(I+NPOIN2)-MESH3D%Z%R(I))
     &                /2.71828182845D0**2
            ELSE
              DELTAZ=MESH3D%Z%R(I+(IPLAN-1)*NPOIN2)-MESH3D%Z%R(I)
            ENDIF
            AUX=MAX(30.D0*DELTAZ/0.0162D0,1.D0)
            U%R(I+(IPLAN-1)*NPOIN2)=(0.0703D0/0.41D0)*LOG(AUX)
          ENDDO
        ENDDO
        CALL OS( 'X=0     ' , X=V )
      ENDIF
!
      CALL OS( 'X=0     ' , X=W )
!
!-----------------------------------------------------------------------
!
!     INITIALISES TRACERS
!
      IF(NTRAC.GT.0) THEN
        DO I=1,NTRAC
          CALL OS( 'X=C     ', X=TA%ADR(I)%P, C=TRAC0(I))
        ENDDO
      ENDIF
!
!
!-----------------------------------------------------------------------
!   INITIALISES THE K-EPSILON MODEL (OPTIONAL)
!   WHEN DONE: AKEP = .FALSE.
!
      AKEP=.TRUE.
!
!     IF(ITURBV.EQ.3) THEN
!
!       HERE INITIALISES K AND EPSILON
!
!       AKEP = .FALSE.
!     ENDIF
!
!-----------------------------------------------------------------------
!
! INITIALISES THE PRESSURE FIELDS TO 0.0
!
      IF(NONHYD) THEN
        CALL OS('X=C     ',X=DP,C=0.D0)
        WRITE (LU,*) 'CONDIM: DYNAMIC PRESSURE INITIALISED TO ZERO'
        CALL OS('X=C     ',X=PH,C=0.D0)
        WRITE (LU,*) '        HYDROSTATIC PRESSURE INITIALISED TO ZERO.'
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    *********************
                     SUBROUTINE T3D_CORFON
!                    *********************
!
     &(SZF, ST1, ST2, ZF, T1, T2, X, Y, PRIVE, NPOIN2,
     & LISFON, MSK, MASKEL, MATR2D, MESH2D, S)
!
!***********************************************************************
! TELEMAC3D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    MODIFIES THE BOTTOM TOPOGRAPHY.
!+
!+            STANDARD ACTION: SMOOTHES THE BOTTOM ELEVATION.
!+
!+           (KEYWORD:  'NUMBER OF BOTTOM SMOOTHINGS')
!
!note     EQUIVALENT TO CORFON (BIEF LIBRARY), EXCEPT THAT THIS
!+         SUBROUTINE DISTINGUISHES DATA FROM STRUCTURES.
!
!history  J.M. JANIN  (LNH)
!+        25/11/97
!+        V5P1
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
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
!| LISFON         |-->| NUMBER OF SMOOTHINGS REQUIRED
!| MASKEL         |-->| MASK OF ELEMENTS
!| MATR2D         |---|
!| MESH2D         |---|
!| MSK            |-->| IF YES, THERE ARE MASKED ELEMENTS
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| PRIVE          |-->| BLOCK OF PRIVATE ARRAYS
!| S              |---|
!| ST1            |---|
!| ST2            |---|
!| SZF            |---|
!| T2             |---|
!| X,Y            |-->| MESH COORDINATES
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
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,I
      LOGICAL MAS
!
!***********************************************************************
!
!     SMOOTHES THE BOTTOM ELEVATION
!
      IF(LISFON.GT.0) THEN
!
        MAS = .TRUE.
!
        CALL FILTER(SZF,MAS,ST1,ST2,MATR2D,'MATMAS          ',
     &              1.D0,S,S,S,S,S,S,MESH2D,MSK,MASKEL,LISFON)
      ENDIF
!
!-----------------------------------------------------------------------
!
      DO I=1,NPOIN2
        ZF(I)=-1.01D-3*MESH2D%X%R(I)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    ***************************
                     SUBROUTINE PRERES_TELEMAC3D
!                    ***************************
!
     &(LT)
!
!***********************************************************************
! TELEMAC3D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    PREPARES THE VARIABLES WHICH WILL BE WRITTEN TO
!+                THE RESULTS FILE OR TO THE LISTING.
!
!history  J-M HERVOUET (LNH)
!+        30/03/04
!+        V5P7
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
!| LT             |-->| ITERATION NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_PRERES_TELEMAC3D => PRERES_TELEMAC3D
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: LT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL LEO
!
      INTEGER LTT,I,IPLAN,I3,NODE,N
      DOUBLE PRECISION DELTAZ,U_0,U_1,V_0,V_1,C_0,C_1,ULOG,AUX
!
      INTRINSIC SQRT,MAX
!
!-----------------------------------------------------------------------
!
!     IS THERE AN OUTPUT OR NOT AT THIS TIMESTEP ?
!
      LEO=.FALSE.
      LTT=(LT/GRAPRD)*GRAPRD
      IF((LT.EQ.LTT.OR.LT.EQ.NIT).AND.LT.GE.GRADEB) LEO=.TRUE.
!
!     NO PRINT OUT, NO OUTPUT TO FILE: EXITS
      IF(.NOT.LEO) GO TO 1000
!
!
!-----------------------------------------------------------------------
!
! VARIABLES FOR 2D OUTPUTS
!
!-----------------------------------------------------------------------
!
! SPECIFIQUE PROFIL DE ROUSE
!
! ESSAI JMH
      N=46
      IF(NCSIZE.GT.1) THEN
        NODE=0
        DO I=1,NPOIN2
          IF(MESH2D%KNOLG%I(I).EQ.N) NODE=I
        ENDDO
      ELSE
        NODE=N
      ENDIF
      IF(LT.EQ.NIT.AND.NODE.GT.0) THEN
      IF(NTRAC.GT.0) THEN
        PRINT*,'        Z                      U                  ULOG'
        DO I=1,NPLAN
          IF(I.EQ.1) THEN
            DELTAZ=(MESH3D%Z%R(NODE+NPOIN2)-MESH3D%Z%R(NODE))
     &              /2.71828182845D0**2
          ELSE
            DELTAZ=MESH3D%Z%R(NODE+(I-1)*NPOIN2)-MESH3D%Z%R(NODE)
          ENDIF
          AUX=MAX(30.D0*DELTAZ/0.0162D0,1.D0)
          ULOG=(0.0703D0/0.41D0)*LOG(AUX)
          PRINT*,MESH3D%Z%R(NODE+(I-1)*NPOIN2),U%R(NODE+(I-1)*NPOIN2),
     &           ULOG
        ENDDO
        PRINT*,'         Z                  NUT VIT '
        DO I=1,NPLAN
          PRINT*,MESH3D%Z%R(NODE+(I-1)*NPOIN2),
     &           VISCVI%ADR(3)%P%R(NODE+(I-1)*NPOIN2)
        ENDDO
        PRINT*,'         Z                      C               NU TRAC'
        DO I=1,NPLAN
          PRINT*,MESH3D%Z%R(NODE+(I-1)*NPOIN2),
     &           TA%ADR(NTRAC)%P%R(NODE+(I-1)*NPOIN2),
     &           VISCTA%ADR(NTRAC)%P%ADR(3)%P%R(NODE+(I-1)*NPOIN2)
        ENDDO
      ELSE
        PRINT*,'        Z                       U                  ULOG'
        DO I=1,NPLAN
          IF(I.EQ.1) THEN
            DELTAZ=(MESH3D%Z%R(NODE+NPOIN2)-MESH3D%Z%R(NODE))
     &              /2.71828182845D0**2
          ELSE
            DELTAZ=MESH3D%Z%R(NODE+(I-1)*NPOIN2)-MESH3D%Z%R(NODE)
          ENDIF
          AUX=MAX(30.D0*DELTAZ/0.0162D0,1.D0)
          ULOG=(0.0703D0/0.41D0)*LOG(AUX)
           PRINT*,MESH3D%Z%R(NODE+(I-1)*NPOIN2),U%R(NODE+(I-1)*NPOIN2),
     &            ULOG
        ENDDO
        PRINT*,'         Z                  NUT VIT '
        DO I=1,NPLAN
          PRINT*,MESH3D%Z%R(NODE+(I-1)*NPOIN2),
     &           VISCVI%ADR(3)%P%R(NODE+(I-1)*NPOIN2)
        ENDDO
      ENDIF
      ENDIF
! FIN ESSAI JMH
!
!=======================================================================
! CELERITY OF WAVES = SQRT(GH) : PUT INTO T2_10
!=======================================================================
!
      IF(LEO.AND.SORG2D(3)) THEN
        DO I=1,NPOIN2
          T2_10%R(I)=SQRT(GRAV*MAX(H%R(I),0.D0))
        ENDDO
      ENDIF
!
!=======================================================================
! FREE SURFACE (= H + ZF) : PUT INTO T2_01
!=======================================================================
!
      IF(LEO.AND.SORG2D(5)) THEN
        CALL OS( 'X=Y+Z   ' , T2_01 , H  , ZF , 0.D0 )
      ENDIF
!
!=======================================================================
! FROUDE NUMBER = U/SQRT(GH) : PUT INTO T2_02
!=======================================================================
!
      IF(LEO.AND.SORG2D(7)) THEN
        DO I=1,NPOIN2
          T2_02%R(I)=SQRT(  (U2D%R(I)**2+V2D%R(I)**2)
     &                     /(GRAV*MAX(H%R(I),1.D-3))   )
        ENDDO
      ENDIF
!
!=======================================================================
! SCALAR DISCHARGE : PUT INTO T2_03
!=======================================================================
!
      IF(LEO.AND.SORG2D(8)) THEN
        CALL OS( 'X=N(Y,Z)' , X=T2_03 , Y=U2D, Z=V2D )
        CALL OS( 'X=XY    ' , X=T2_03 , Y=H )
      ENDIF
!
!=======================================================================
! DISCHARGE ALONG X : PUT INTO T2_04
!=======================================================================
!
      IF(LEO.AND.SORG2D(13)) THEN
        CALL OS( 'X=YZ    ' , X=T2_04 , Y=H , Z=U2D )
      ENDIF
!
!=======================================================================
! DISCHARGE ALONG Y : PUT INTO T2_05
!=======================================================================
!
      IF(LEO.AND.SORG2D(14)) THEN
        CALL OS( 'X=YZ    ' , X=T2_05 , Y=H , Z=V2D )
      ENDIF
!
!=======================================================================
! NORM OF VELOCITY : PUT INTO T2_06
!=======================================================================
!
      IF(LEO.AND.SORG2D(15)) THEN
        CALL OS( 'X=N(Y,Z)' , X=T2_06 , Y=U2D , Z=V2D )
      ENDIF
!
!=======================================================================
! SEDIMENT RELATED VARIABLES THAT ARE ONLY KNOWN AT THE END OF THE
! FIRST TIMESTEP : SETS HERE TO 0 IF LT=0
!=======================================================================
!
!     IF(LEO.AND.SORG2D(24).AND.LT.EQ.0) CALL OS( 'X=0     ' , X=EPAI)
      IF(LEO.AND.SORG2D(25).AND.LT.EQ.0) CALL OS( 'X=0     ' , X=FLUER)
      IF(LEO.AND.SORG2D(26).AND.LT.EQ.0) CALL OS( 'X=0     ' , X=PDEPO)
!
!=======================================================================
! FRICTION VELOCITY
!=======================================================================
!
      IF(LEO.AND.SORG2D(31)) THEN
        CALL OS( 'X=SQR(Y)' , X=T2_07 , Y=UETCAR )
      ENDIF
!
!=======================================================================
! SOLID DISCHARGE (ALL 3 DONE IF ONE ASKED)
!=======================================================================
!
      IF(LEO.AND.(SORG2D(32).OR.SORG2D(33).OR.SORG2D(34))) THEN
!       QS IN T2_11, QSX IN T2_12, QSY IN T2_13
!       GIVING A STRUCTURE OF LINEAR 2D VECTOR, LIKE H
        CALL CPSTVC(H,T2_11)
        CALL CPSTVC(H,T2_12)
        CALL CPSTVC(H,T2_13)
!       INITIALISES QSX AND QSY
        CALL OS('X=0     ',X=T2_12)
        CALL OS('X=0     ',X=T2_13)
        DO IPLAN=1,NPLAN-1
          DO I=1,NPOIN2
            I3=I+NPOIN2*(IPLAN-1)
            DELTAZ=Z(I3+NPOIN2)-Z(I3)
!           INTEGRATES U*C ON THE VERTICAL
            U_0=U%R(I3)
            U_1=U%R(I3+NPOIN2)
            V_0=V%R(I3)
            V_1=V%R(I3+NPOIN2)
            C_0=TA%ADR(NTRAC)%P%R(I3)
            C_1=TA%ADR(NTRAC)%P%R(I3+NPOIN2)
            T2_12%R(I)=T2_12%R(I)+DELTAZ*((U_0*C_1+U_1*C_0)/2.D0
     &                                   +(U_1-U_0)*(C_1-C_0)/3.D0)
            T2_13%R(I)=T2_13%R(I)+DELTAZ*((V_0*C_1+V_1*C_0)/2.D0
     &                                   +(V_1-V_0)*(C_1-C_0)/3.D0)
          ENDDO
        ENDDO
!       SOLID DISCHARGE IN M2/S (AS IN SISYPHE, FOR COMPARISON)
        CALL OS('X=CX    ',X=T2_12,C=1.D0/RHOS)
        CALL OS('X=CX    ',X=T2_13,C=1.D0/RHOS)
!       QS AS NORM OF QSX AND QSY
        IF(SORG2D(32)) THEN
          CALL OS( 'X=N(Y,Z)' , X=T2_11 , Y=T2_12 , Z=T2_13 )
        ENDIF
      ENDIF
!
!=======================================================================
! DEPTH-AVERAGED TRACERS (VARIABLES 34 TO 34+NTRAC)
!=======================================================================
!
      IF(NTRAC.GT.0) THEN
        DO I=1,NTRAC
          IF(LEO.AND.SORG2D(34+I)) THEN
            CALL VERMOY(TRAV2%ADR(13+I)%P%R,TRAV2%ADR(13+I)%P%R,
     &                  TA%ADR(I)%P%R,TA%ADR(I)%P%R,1,Z,
     &                  T3_01%R,T3_02%R,T3_03%R,1,NPLAN,NPOIN2,
     &                  NPLAN,OPTBAN)
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
! VARIABLES FOR 3D OUTPUTS
!
!-----------------------------------------------------------------------
!
!
!=======================================================================
! HYDROSTATIC PRESSURE
!=======================================================================
!
      IF(NONHYD.AND.LEO.AND.SORG3D(12)) THEN
        CALL PHSTAT(PH%R,DELTAR%R,Z,T3_01%R,T3_02%R,RHO0,GRAV,
     &              NPOIN3,NPOIN2,NPLAN,PRIVE)
      ENDIF
!
!=======================================================================
!
1000  CONTINUE
      RETURN
      END
!
!
!     FLUER AND FLUDPT CANCELLED IN FOLLOWING SUBROUTINES
!
!
!                    *****************
                     SUBROUTINE ERODNC
!                    *****************
!
     &(CFDEP  , WC     , HDEP     , FLUER , TOB   , DT    ,
     & NPOIN2 , NPOIN3 , KSPRATIO , AC    , RHOS  , RHO0  , HN ,
     & GRAV   , DMOY   , CREF     , ZREF  , CF    , ICQ   ,RUGOF,
     & Z, UETCAR, SETDEP)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MODELS EROSION FOR NON-COHESIVE SEDIMENTS.
!
!history  CAMILLE LEQUETTE
!+        **/06/2003
!+
!+
!
!history  J.-M. HERVOUET
!+        12/09/2007
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
!history  C. VILLARET & T. BENSON & D. KELLY (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments in sediment merged on 25/02/2014.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AC             |-->| CRITICAL SHIELDS PARAMETER
!| CF             |-->| QUADRATIC FRICTION COEFFICIENT (NOT USED)
!| CFDEP          |-->| CONCENTRATION OF FRESH DEPOSIT DES DEPOTS FRAIS
!| CREF           |<->| EQUILIBRIUM CONCENTRATION
!| DMOY           |-->| MEAN DIAMETER OF GRAINS
!| DT             |-->| TIME STEP
!| SETDEP         |-->| EXPLICIT SETTLING (INTEGER)
!| FLUER          |<->| EROSION  FLUX
!| GRAV           |-->| GRAVITY ACCELERATION
!| HDEP           |<->| THICKNESS OF FRESH DEPOSIT (FLUID MUD LAYER)
!| HN             |-->| WATER DEPTH AT TIME N
!| ICQ            |-->| FLAG FOR REFERENCE CONCENTRATION FORMULA
!| KSPRATIO       |-->| NOT USED
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| RHO0           |-->| WATER DENSITY AT REFERENCE CONCENTRATION
!| RHOS           |-->| SEDIMENT DENSITY
!| RUGOF          |<->| FRICTION COEFFICIENT ON THE BOTTOM
!| TOB            |-->| BOTTOM FRICTION
!| UETCAR         |-->| SQUARE OF THE FRICTION VELOCITY
!| WC             |-->| SETTLING VELOCITY
!|     Z          |-->| NODE COORDINATES
!| ZREF           |<->| REFERENCE ELEVATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC3D, ONLY: KARMAN,PRANDTL,FICT
!
      USE INTERFACE_TELEMAC3D, EX_ERODNC => ERODNC
!     TRIGGERS A PGI COMPILER ERROR
      USE INTERFACE_SISYPHE,ONLY:SUSPENSION_FREDSOE,SUSPENSION_VANRIJN
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN2,NPOIN3,ICQ,SETDEP
!
      DOUBLE PRECISION, INTENT(INOUT) :: HDEP(NPOIN2),FLUER(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN)    :: WC(NPOIN3)
!
      DOUBLE PRECISION, INTENT(IN)    :: DT,CFDEP,GRAV,RHOS,RHO0
      DOUBLE PRECISION, INTENT(IN)    :: KSPRATIO,AC
!
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: DMOY,TOB,CF,HN
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: CREF,ZREF,RUGOF
!
      DOUBLE PRECISION, INTENT(IN)    :: Z(NPOIN3), UETCAR(NPOIN2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN,I
      DOUBLE PRECISION USTAR, ROUSE, ROUSE_Z, DELTAZ,QS
!
      INTRINSIC MIN,MAX
!
!-----------------------------------------------------------------------
!
!  ---------------------------------------------------------------------
!  ------- COMPUTES THE REFERENCE CONCENTRATION CREF (IN G/L) ----------
!  ---------------------------------------------------------------------
!
!     CV depth to extrapolate the near bed concentrations (see also KEPLC)
!     For FV scheme use delta= Dz1/4
!     FE scheme          = Dz1/2
!
!     ZYSERMAN & FREDSOE (1994) (BY DEFAULT)
!
!     SO FAR DMOY IS A CONSTANT
!
      IF(ICQ.EQ.1) THEN
        CALL OS('X=CY    ', X=ZREF, Y=DMOY, C=2.D0)
        CALL SUSPENSION_FREDSOE(DMOY%R(1),TOB, NPOIN2,
     &                        GRAV,RHO0,RHOS,1.D-6,AC,CREF)
        CALL OS('X=CY    ', X=ZREF, Y=DMOY, C=2.D0)
      ELSEIF(ICQ.EQ.3) THEN
        CALL OS('X=CY    ', X=ZREF, Y=RUGOF, C=0.5D0)
        CALL SUSPENSION_VANRIJN(DMOY%R(1),TOB,NPOIN2,
     &                 GRAV,RHO0,RHOS,1.D-06,1.D-06,AC,CREF,ZREF)
      ENDIF
!
!     UNITS FOR CREF G/L, NOT LIKE IN SISYPHE
!
      CALL OS('X=CX    ',X=CREF,C=RHOS)
!
!     CV: Extrapolation of Rouse profile from ZREF to 1/2 or 1/4 of first grid mesh
!
      IF(SETDEP.EQ.1) THEN
        DO IPOIN =1,NPOIN2
          USTAR=MAX(SQRT(UETCAR(IPOIN)),1.D-6)
          ROUSE=PRANDTL*WC(IPOIN)/KARMAN/USTAR
!         rouse profile extrapolation up to 1/4 of the first layer
!         DELTAZ=(MESH3D%Z%R(IPOIN +NPOIN2)-MESH3D%Z%R(IPOIN))/4.D0
          DELTAZ=(Z(IPOIN +NPOIN2)-Z(IPOIN))/FICT
          ROUSE_Z=ZREF%R(IPOIN)/(HN%R(IPOIN)-ZREF%R(IPOIN))
     &        *(HN%R(IPOIN)-DELTAZ)/DELTAZ
          CREF%R(IPOIN)=CREF%R(IPOIN)*ROUSE_Z**ROUSE
        ENDDO
      ENDIF
!
!  ------------------------------------------------------------
!  -----------------     EROSION STEP    ----------------------
!  ------------------------------------------------------------
!
      DO I=1,NPOIN2
!
!       COMPUTES THE EROSION FLUX
!
!CV        FLUER(I)=-WC(I)*CREF%R(I)
        FLUER(I)= WC(I)*CREF%R(I)
!
!       QUANTITY OF SOLID IN THE LAYER BEFORE EROSION
!
!       CFDEP IN KG/M3 ( ~ 0.65 RHOS )
        QS=CFDEP*HDEP(I)
!
!       LAYER THICKNESS AFTER EROSION
!
!CV        HDEP(I)=MAX(0.D0,HDEP(I)-(FLUER(I)*DT/CFDEP))
!
!       LIMITS THE EROSION FLUX
!
!       !!!!!!!!  THIS TEST-CASE  !!!!!!!!!
!
!       FLUER(I)=MIN(FLUER(I),QS/DT)
        FLUER(I)=0.D0
!
!       !!!!!!!!  THIS TEST-CASE  !!!!!!!!!
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    *****************
                     SUBROUTINE FLUSED
!                    *****************
!
     &(ATABOF , BTABOF , ATABOS , BTABOS ,
     & LITABF , LITABS , TA     , WC     ,
     & X      , Y      , Z      , HN     ,
     & GRADZFX, GRADZFY, GRADZSX, GRADZSY,
     & TOB    , FLUDPT , FLUER  , TOCD   ,
     & NPOIN3 , NPOIN2 , NPLAN  , KLOG   ,
     & HMIN   , SEDCO  , SETDEP)
!
!***********************************************************************
! TELEMAC3D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    WRITES THE FLUXES AT THE BOTTOM AND FREE SURFACE
!+                FOR THE SEDIMENT.
!
!note     CHECKS MASS BALANCE AT THE BOTTOM AND FREE SURFACE.
!+         RESULTS IN A BOUNDARY CONDITION ON SEDIMENT FLUXES.
!
!history  C LE NORMANT (LNH)
!+        13/05/92
!+        V5P5
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
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
!history  C. VILLARET (HR-WALLINGFORD) & J-M HERVOUET (EDF LAB, LNHE)
!+        20/01/2014
!+        V7P0
!+   Erosion and deposition fluxes cancelled on tidal flats.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ATABOF         |<->| FOR BOUNDARY CONDITION (BOTTOM)
!| ATABOS         |<->| FOR BOUNDARY CONDITION (SURFACE) NOT USED
!| BTABOF         |<->| FOR BOUNDARY CONDITION (BOTTOM)
!| BTABOS         |<->| FOR BOUNDARY CONDITION (SURFACE) NOT USED
!| EXPSETDEP      |-->| EXPLICIT DEPOSITION SCHEME
!| FLUDPT         |-->| IMPLICIT DEPOSITION FLUX
!| FLUER          |<->| EROSION  FLUX FOR EACH 2D POINT
!| GRADZFX        |-->| NOT USED
!| GRADZFY        |-->| NOT USED
!| GRADZSX        |-->| NOT USED
!| GRADZSY        |-->| NOT USED
!| HMIN           |-->| MINIMUM WATER DEPTH TO PREVENT EROSION ON TIDAL FLATS
!| HN             |-->| WATER DEPTH AT TIME N
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| LITABF         |-->| FOR BOUNDARY CONDITION BOTTOM
!| LITABS         |<->| FOR BOUNDARY CONDITION SURFACE (NOT USED)
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| PDEPOT         |<->| PROBABILITY OF DEPOSIT FOR EACH 2D POINT
!| SEDCO          |-->| LOGICAL FOR COHESIVE SEDIMENT
!| SETDEP         |-->| CHOICE OF CONVECTION SCHEME FOR VERTICAL SETTLING
!| TA             |-->| CONCENTRATION OF SEDIMENTS
!| TOB            |<->| BOTTOM FRICTION
!| TOCD           |-->| CRITICAL SHEAR STRESS FOR SEDIMENT DEPOSITION
!| WC             |-->| SETTLING VELOCITY
!| X              |-->| COORDINATE
!| Y              |-->| COORDINATE
!| Z              |-->| COORDINATE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC3D, ONLY: IPBOT,SIGMAG,OPTBAN
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN3,NPOIN2,NPLAN,KLOG,SETDEP
      LOGICAL, INTENT(IN) :: SEDCO
!
!     BOTTOM
!     ****
!
!     BY POINTS
!     ----------
!
      INTEGER, INTENT(IN) :: LITABF(NPOIN2)
!
!     BY FACES
!     ---------
      DOUBLE PRECISION, INTENT(INOUT) :: ATABOF(NPOIN2), BTABOF(NPOIN2)
!
!     FREE SURFACE
!     *******
!
!     BY POINTS
!     ----------
!
      INTEGER, INTENT(INOUT) :: LITABS(NPOIN2)
!
!     BY FACES
!     ---------
      DOUBLE PRECISION, INTENT(INOUT) :: ATABOS(NPOIN2), BTABOS(NPOIN2)
!
!     OTHER ARRAYS
!
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN3), Y(NPOIN3), Z(NPOIN3)
      DOUBLE PRECISION, INTENT(IN) :: TA(NPOIN3)
      DOUBLE PRECISION, INTENT(IN) :: WC(NPOIN3)
      DOUBLE PRECISION, INTENT(IN) :: GRADZFX(NPOIN2), GRADZFY(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: GRADZSX(NPOIN2), GRADZSY(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: HN(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TOB(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUDPT(NPOIN2), FLUER(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN) :: TOCD
      DOUBLE PRECISION, INTENT(IN) :: HMIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
      INTRINSIC MAX
!
!-----------------------------------------------------------------------
!
!     COMPUTES THE DEPOSITION PROBABILITY
!
      IF(SEDCO) THEN
!
!       COHESIVE SEDIMENT (Here FLUDPT >0)
!
        DO I=1,NPOIN2
          FLUDPT(I)= WC(I)*MAX(1.D0-(TOB(I)/MAX(TOCD,1.D-6)),0.D0)
        ENDDO
!
      ELSE
!
!       NON COHESIVE SEDIMENT : PDEPOT = 1.
!
        DO I=1,NPOIN2
!         !!!!!!!!  THIS TEST-CASE  !!!!!!!!!
!         FLUDPT(I)= WC(I)
          FLUDPT(I)= 0.D0
!         !!!!!!!!  THIS TEST-CASE  !!!!!!!!!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     CORRECTION OF EROSION FLUXES ON TIDAL FLATS
!
      DO I=1,NPOIN2
        IF(HN(I).LE.HMIN) THEN
          FLUER(I)= 0.D0
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
!     COMMENTED BY JMH ON 28/02/2014 (IF IT IS REALLY A PROBLEM
!                                     WE NEED TO FIND A CLEANER SOLUTION)
!
!     PREVENTING EROSION AND DEPOSITION ON THE EXIT BOUNDARY
!     (Fixed water depth)
!
!     DO IPTFR = 1,NPTFR2
!       IF(LIHBOR%I(IPTFR).EQ.KENT) THEN
!         I = NBOR2%I(IPTFR)
!         FLUDPT(I)=0.D0
!         FLUER(I)=0.D0
!         DO IPLAN=1, NPLAN
!           I3D= I + (IPLAN-1)*NPOIN2
!           WC(I3D)=0.D0
!         ENDDO
!       ENDIF
!     ENDDO
!
!-----------------------------------------------------------------------
!
!     COMMON COMPUTATION OF THE TRACER FLUX ON THE BOTTOM
!
      IF(SETDEP.EQ.1) THEN
!
        DO I=1,NPOIN2
          IF(LITABF(I).EQ.KLOG) THEN
!           TOM : erosion and deposition are treated with advection
            ATABOF(I) = 0.D0
            BTABOF(I) = 0.D0
          ENDIF
        ENDDO
!
      ELSEIF(SIGMAG.OR.OPTBAN.EQ.1) THEN
!
        DO I=1,NPOIN2
          ATABOF(I)=0.D0
          BTABOF(I)=0.D0
          IF(LITABF(I).EQ.KLOG) THEN
!           NO EROSION AND DEPOSITION ON TIDAL FLATS
            IF(IPBOT%I(I).NE.NPLAN-1) THEN
              ATABOF(I) = - FLUDPT(I)
              BTABOF(I) = FLUER(I)
            ENDIF
          ENDIF
        ENDDO
!
      ELSE
!
        DO I=1,NPOIN2
          IF(LITABF(I).EQ.KLOG) THEN
!           NZ = 1.D0+GRADZFX(I)**2+GRADZFY(I)**2
!           NZ = -1.D0/SQRT(NZ)
!           WC
!           ATABOF(I) = - WC(I) * PDEPOT(I) * NZ
!           BTABOF(I) = - FLUER(I) * NZ
!           JMH: BEWARE, IN DIFF3D NZ IS CONSIDERED AS -1.
!                HENCE WRONG FORMULA BELOW IS ACTUALLY CORRECT
            ATABOF(I) = - FLUDPT(I)
            BTABOF(I) = FLUER(I)
          ENDIF
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     BOUNDARY CONDITION AT THE FREE SURFACE
!
!     FLUX  = 0 (SETTLING VELOCITY FLUX + DIFFUSIVE FLUX)
!
!     ALREADY DONE IN LIMI3D !!
!
!     DO I=1,NPOIN2
!       ATABOS(I)=0.D0
!       BTABOS(I)=0.D0
!     ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
