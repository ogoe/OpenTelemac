!                    *****************
                     SUBROUTINE ART_CORFON
!                    *****************
!
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MODIFIES THE BOTTOM TOPOGRAPHY.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER
!code
!+  EXAMPLE :
!+
!+      DO 10 I = 1,NPOIN
!+        ZF%R(I) = -1.D0 -0.02D0*Y(I)
!+        IF (Y(I).GE.700.D0) THEN
!+           ZF%R(I) = -15.D0
!+        ENDIF
!+10    CONTINUE
!
!history  J-M HERVOUET
!+        01/03/1990
!+        V5P1
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
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_ARTEMIS
!
      IMPLICIT NONE
!
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
!  SMOOTHING(S) OF THE BOTTOM (OPTIONAL)
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
      DO  I = 1,NPOIN
        ZF%R(I) = -0.616D0
      ENDDO
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    *****************
                     SUBROUTINE CORRXY
!                    *****************
!
     & (X,Y,NPOIN)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MODIFIES THE COORDINATES OF THE POINTS IN THE MESH.
!
!warning  USER SUBROUTINE; COMMENTED LINES ARE AN EXAMPLE
!code
!+  EXAMPLE : MULTIPLIES BY A CONSTANT (SCALES THE MESH)
!+            CHANGES THE ORIGIN
!+
!+      DO I = 1 , NPOIN
!+         X(I) = 3.D0 * X(I) + 100.D0
!+         Y(I) = 5.D0 * Y(I) - 50.D0
!+      ENDDO
!warning  DO NOT PERFORM ROTATIONS AS IT WILL CHANGE
!+            THE NUMBERING OF THE LIQUID BOUNDARIES
!
!history  EMILE RAZAFINDRAKOTO (LNHE)
!+        17/10/05
!+        V5P6
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
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| X              |<->| ABSCISSAE OF POINTS IN THE MESH
!| X,Y            |<->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CORRXY => CORRXY
      USE DECLARATIONS_SPECIAL
!
!
!     OTHER DATA ARE AVAILABLE WITH THE DECLARATIONS OF EACH PROGRAM
!
!     USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN),Y(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     THIS SUBROUTINE MUST BE MODIFIED ACCORDING TO
!     THE CALLING PROGRAM AND THE NEEDED MODIFICATION
!     BY ADDING USE DECLARATIONS_"NAME OF CALLING CODE"
!     ALL THE DATA STRUCTURE OF THIS CODE IS AVAILABLE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
!  EXAMPLE : MULTIPLIES BY A CONSTANT (SCALES THE MESH)
!            CHANGES THE ORIGIN
!
!      DO I=1,NPOIN
!        X(I)=X(I)/100
!        Y(I)=Y(I)/2000
!     ENDDO
!
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.1) THEN
        WRITE(LU,*)'CORRXY (BIEF) : PAS DE MODIFICATION DES COORDONNEES'
        WRITE(LU,*)
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*)'CORRXY (BIEF):NO MODIFICATION OF COORDINATES'
        WRITE(LU,*)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!

!                    ***************
                     SUBROUTINE BORH
!                    ***************
!
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    TAKES INTO ACCOUNT USER-SPECIFIED BOUNDARY CONDITIONS.
!+              THEY ARE GIVEN BY SEGMENT.
!
!warning  MUST BE CODED BY THE USER
!code
!+ ---------------------------------------
!+ INITIALISES THE VARIABLES (DEFAULT)
!+ ---------------------------------------
!+      TETAB%R(:)=TETAH
!+      TETAP%R(:)=0.D0
!+      ALFAP%R(:)=0.D0
!+      RP%R(:)=0.D0
!+      HB%R(:)=0.D0
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH)
!+        21/08/2000
!+        V5P1
!+
!
!history  C. DENIS (SINETICS)
!+        18/03/2010
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
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_ARTEMIS
!
      IMPLICIT NONE
!
      INTEGER I,JB
!
      DOUBLE PRECISION RADDEG,AA,HBCRIT
      DOUBLE PRECISION PI,BID
      PARAMETER( PI = 3.1415926535897932384626433D0)
!
      INTRINSIC COS,SIN
!
!-----------------------------------------------------------------------
!
! BOUNDARY CONDITIONS
! KLOG : 'SOLID' SEGMENT.
! KINC : 'INCIDENT WAVE' SEGMENT.
! KENT : 'ENTRY' SEGMENT.
! KSORT : 'EXIT' SEGMENT.
!
! ALL THE ANGLES ARE IN  DEGREES
!                         ------
! ---------------------------------------
! INITIALISES THE VARIABLES (DEFAULT)
! ---------------------------------------
!
!   WARNING : TETAB%R MUSTN'T BE MODIFIED HERE WHEN USING MULTIDIRECTIONAL WAVES
!      TETAB%R(:)=TETAH
!
      TETAP%R(:)=0.D0
      ALFAP%R(:)=0.D0
      RP%R(:)=0.D0
      HB%R(:)=0.D0
!
!-----------------------------------------------------------------------
!

      DO I=1,NPTFR
        JB=BOUNDARY_COLOUR%I(I)
!
        IF(JB.GE.821.AND.JB.LE.840) THEN
          LIHBOR%I(I)=KINC
          HB%R(I)=0.202D0
          TETAB%R(I)=0.D0
          TETAP%R(I)=0.D0
        ENDIF
        IF(JB.EQ.1) THEN
          LIHBOR%I(I)=KINC
          HB%R(I)=0.202D0
          TETAB%R(I)=0.D0
          TETAP%R(I)=0.D0
        ENDIF
!
!       PAROIS LIQUIDES -FRONTIERE LIBRE
!
        IF(JB.GE.401.AND.JB.LE.421) THEN
          LIHBOR%I(I)=KSORT
          TETAP%R(I)=0.D0
        ENDIF
!
!       PAROIS SOLIDES
        IF(JB.GE.2.AND.JB.LE.400) THEN
          LIHBOR%I(I)=KLOG
          RP%R(I)=1.D0
          TETAP%R(I)=0.D0
          ALFAP%R(I)=0.D0
        ENDIF
        IF(JB.GE.422.AND.JB.LE.820) THEN
          LIHBOR%I(I)=KLOG
          RP%R(I)=1.D0
          TETAP%R(I)=0.D0
          ALFAP%R(I)=0.D0
        ENDIF
      ENDDO


      RETURN
      END
!                    *****************
                     SUBROUTINE CONDIH
!                    *****************
!
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES THE PHYSICAL PARAMETER ARRAYS.
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH)
!+        02/06/1999
!+        V5P1
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
!history  C.PEYRARD (EDF)
!+        18/03/2014
!+        V7P0
!+   Computation of reference wave number for automatic
!+   phase calculation
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_ARTEMIS
!
      IMPLICIT NONE
!
      INTEGER I
!
      DOUBLE PRECISION COTE
      DOUBLE PRECISION PI,BID,DHTEST,T1REF,T2REF
      PARAMETER( PI = 3.1415926535897932384626433D0 )
!
      INTRINSIC SINH, SQRT
!
      DOUBLE PRECISION AMPLC

!-----------------------------------------------------------------------
!
      CALL MAJUS(CDTINI)
!
!-----------------------------------------------------------------------
!
!   INITIALISES THE WATER DEPTH H
!
      IF(INCLUS(CDTINI,'COTE NULLE').OR.
     &   INCLUS(CDTINI,'ZERO ELEVATION') ) THEN
        COTE = 0.D0
        CALL OS( 'X=C     ' , H , SBID , SBID , COTE )
        CALL OS( 'X=X-Y   ' , H , ZF  , SBID , BID  )
      ELSEIF(INCLUS(CDTINI,'COTE CONSTANTE').OR.
     &       INCLUS(CDTINI,'CONSTANT ELEVATION') ) THEN
        COTE = COTINI
        CALL OS( 'X=C     ' , H , SBID , SBID , COTE )
        CALL OS( 'X=X-Y   ' , H , ZF  , SBID , BID  )
      ELSEIF(INCLUS(CDTINI,'HAUTEUR NULLE').OR.
     &       INCLUS(CDTINI,'ZERO DEPTH') ) THEN
        CALL OS( 'X=C     ' , H , SBID , SBID , 0.D0 )
      ELSEIF(INCLUS(CDTINI,'HAUTEUR CONSTANTE').OR.
     &       INCLUS(CDTINI,'CONSTANT DEPTH') ) THEN
        CALL OS( 'X=C     ' , H , SBID , SBID , HAUTIN )
      ELSEIF(INCLUS(CDTINI,'PARTICULIERES').OR.
     &       INCLUS(CDTINI,'SPECIAL')        ) THEN
!  TO BE MODIFIED BY USER
        IF(LNG.EQ.1) WRITE(LU,10)
        IF(LNG.EQ.2) WRITE(LU,11)
10      FORMAT(1X,'CONDIH : AVEC DES CONDITIONS INITIALES PARTICULIERES'
     &         ,/,'         VOUS DEVEZ MODIFIER CONDIH')
11      FORMAT(1X,'CONDIH : WITH SPECIAL INITIAL CONDITIONS'
     &         ,/,'         YOU HAVE TO MODIFY CONDIH')
        CALL PLANTE(0)
        STOP
!  END OF CODE TO BE MODIFIED BY USER
      ELSE
        IF(LNG.EQ.1) WRITE(LU,20) CDTINI
        IF(LNG.EQ.2) WRITE(LU,21) CDTINI
20      FORMAT(1X,'CONDIH : CONDITION INITIALE INCONNUE :',/,A72)
21      FORMAT(1X,'CONDIH : UNKNOWN INITIAL CONDITION :',/,A72)
        CALL PLANTE(0)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!  CLIPS H (VALUES LOWER THAN 1.D-2 NOT ALLOWED)
!
      CALL CLIP(H,1.D-2,.TRUE.,1.D6,.FALSE.,NPOIN)
!
!-----------------------------------------------------------------------
!
!   COMPUTES THE WAVE NUMBER: K
!   USING AN EXPLICIT FORMULATION (SEE EDF'S EXCELLENT REPORT BY
!   F. DHELLEMMES 'PRECIS SUR LES VAGUES' )
!
!
      OMEGA = 2.D0*PI/PER
      CALL OS('X=CY    ', T1 , H , SBID , OMEGA**2/GRAV )
!
!     INITIALISES DHTEST
!
      DHTEST = 1.D6
!
      DO I=1,NPOIN
        T2%R(I) = 1.D0 + T1%R(I) *( 0.6522D0 +
     &                   T1%R(I) *( 0.4622D0 +
     &                   T1%R(I) *
     &                   T1%R(I) *( 0.0864D0 +
     &                   T1%R(I) *( 0.0675D0 ) )))
        T2%R(I) = SQRT( T1%R(I)*(T1%R(I) + 1.D0/T2%R(I)) )
        K%R(I)  = T2%R(I)/H%R(I)
        DHTEST  = MIN( DHTEST , H%R(I) )
      ENDDO
!
!     COMPUTE REFERENCE WAVE NUMBER KPHREF FOR AUTOMATIC PHASE CALCULATION
      IF (LPHASEAUTO) THEN
!       CHECKS THE REFERENCE DEPTH HAS BEEN GIVEN
        IF (DEPREF.LT.0D0) THEN
          IF(LNG.EQ.1) THEN
           WRITE(LU,220)
          ENDIF
          IF(LNG.EQ.2) THEN
           WRITE(LU,221)
          ENDIF
220       FORMAT(1X,'CONDIH : ERREUR. SI VOUS UTILISEZ LE CALCUL      '
     &           ,/,'         AUTOMATIQUE DES PHASES, IL FAUT         '
     &           ,/,'         RENSEIGNER UNE PROFONDEUR DE REFERENCE  '
     &           ,/,'         MOT CLEF : PROFONDEUR DE REFERENCE POUR '
     &           ,/,'         LA PHASE AUTOMATIQUE                    ')

221       FORMAT(1X,'CONDIH : ERROR. IF YOU USE AUTOMATIC PHASE       '
     &           ,/,'         CALCULATION, YOU HAVE TO GIVE A         '
     &           ,/,'         REFERENCE WATER DEPTH                   '
     &           ,/,'         KEY WORD :                              '
     &           ,/,'        REFERENCE WATER DEPTH FOR AUTOMATIC PHASE')
          CALL PLANTE(1)
          STOP
        ENDIF
        T1REF= OMEGA**2/GRAV * DEPREF
        T2REF = 1.D0 + T1REF *( 0.6522D0 +
     &                 T1REF *( 0.4622D0 +
     &                 T1REF *
     &                 T1REF *( 0.0864D0 +
     &                 T1REF *( 0.0675D0 ) )))
        T2REF = SQRT( T1REF*(T1REF + 1.D0/T2REF) )
        KPHREF  = T2REF/DEPREF
      ENDIF
!   CHECKS WHETHER H HAS BEEN CLIPPED OR NOT
!
      IF (DHTEST.LE.1.01D-2) THEN
        IF(LNG.EQ.1) WRITE(LU,120)
        IF(LNG.EQ.2) WRITE(LU,121)
120     FORMAT(1X,'CONDIH : ATTENTION !! VOUS AVEZ ATTEINT LE SEUIL '
     &         ,/,'         MINI DE HAUTEUR D''EAU (1 CM).'
     &         ,/,'         VERIFIEZ BATHY OU CONDITIONS INITIALES')
121     FORMAT(1X,'CONDIH : WARNING !! YOU REACHED MINIMUM THRESHOLD'
     &         ,/,'         FOR WATER DEPTH (1 CM). CHECK THE'
     &         ,/,'         BATHYMETRY OR INITIAL CONDITIONS')
      ENDIF
!
!-----------------------------------------------------------------------
!
!   COMPUTES PHASE VELOCITY
!
      CALL OS('X=CY    ', T1    , K     , SBID , 1.D0/OMEGA )
      CALL OS('X=1/Y   ', C     , T1    , SBID , BID        )
!
!-----------------------------------------------------------------------
!
!   COMPUTES GROUP VELOCITY
!
      DO I=1,NPOIN
        CG%R(I) = C%R(I)/2.D0 *
     &            (1.D0 + 2.D0*K%R(I)*H%R(I)/SINH(2.D0*K%R(I)*H%R(I)))
      ENDDO
!
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!         CURRENT DEFINITION ON THE DOMAIN : DEFAULT 0
!                       (EXAMPLE IS GIVEN BELOW)
!----------------------------------------------------------------------
!   => DEFINE YOUR CURRENT VALUES IN THE FOLLOWING LOOP
      IF(COURANT) THEN
        AMPLC=0D0
        DO I=1,NPOIN
        UC%R(I)=AMPLC
        VC%R(I)=AMPLC
        ENDDO
      ENDIF
!
!=====================================
! === EXAMPLE OF X,Y DEPENDENT CURRENT
!=====================================
!      IF(COURANT) THEN
!
!       AMPLC=1.D0
!       DO I=1,NPOIN
!        UC%R(I)=0.D0
!        VC%R(I)=0.D0
!        IF(X(I).GE.5.D0.AND.X(I).LT.13.D0)THEN
!          UC%R(I)=AMPLC*((X(I)-5.D0)/8.D0)
!         ELSEIF(X(I).GE.13.D0)THEN
!          UC%R(I)=AMPLC
!        ENDIF
!       ENDDO
!
!      ENDIF
      IF(COURANT) THEN
!       AMPLC=0.05 , 0.66 ou 1.
        AMPLC=1.0D0
        DO I=1,NPOIN!
        UC%R(I)=0.D0
        VC%R(I)=0.D0
        IF(X(I).GE.5.D0.AND.X(I).LT.13.D0)THEN
          UC%R(I)=AMPLC*((X(I)-5.D0)/8.D0)
        ELSEIF(X(I).GE.13.D0)THEN
          UC%R(I)=AMPLC
        ENDIF
        ENDDO
      ENDIF


!-----------------------------------------------------------------------


!
      RETURN
      END SUBROUTINE
