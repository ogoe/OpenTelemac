!                    *****************
                     SUBROUTINE NOEROD
!                    *****************
!
     & (H , ZF , ZR , Z , X , Y , NPOIN , CHOIX , NLISS )
!
!***********************************************************************
! SISYPHE   V6P3                                  21/07/2011
!***********************************************************************
!
!brief    FIXES THE NON-ERODABLE BED ELEVATION ZR.
!
!note     METHODS OF TREATMENT OF NON-ERODABLE BEDS CAN LEAD TO ZF.
!note  CHOOSE TO SMOOTH THE SOLUTION WITH NLISS > 0.
!
!history  C. LENORMANT
!+
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
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        21/06/2013
!+        V6P3
!+   Now ZR=ZF-100.D0 by default
!+   previous versions was erronneously ZR=-100.D0
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CHOIX          |-->| SELECTED METHOD FOR THE TREATMENT OF RIGID BEDS
!| H              |-->| WATER DEPTH
!| NLISS          |<->| NUMBER OF SMOOTHINGS
!| NPOIN          |-->| NUMBER OF 2D POINTS
!| X,Y            |-->| 2D COORDINATES
!| Z              |-->| FREE SURFACE
!| ZF             |-->| BED LEVEL
!| ZR             |<--| RIGID BED LEVEL
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
      INTEGER, INTENT(IN):: NPOIN , CHOIX
      INTEGER, INTENT(INOUT):: NLISS
!
      DOUBLE PRECISION, INTENT(IN)::  Z(NPOIN) , ZF(NPOIN)
      DOUBLE PRECISION , INTENT(IN)::  X(NPOIN) , Y(NPOIN), H(NPOIN)
      DOUBLE PRECISION , INTENT(INOUT)::  ZR(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     INTEGER I
!
!---------------------
! RIGID BEDS POSITION
!---------------------
!
      CALL OV('X=Y+C   ',ZR,ZF,ZF,-0.1D0,NPOIN)
!
!------------------
! SMOOTHING OPTION
!------------------
!
!     NLISS : NUMBER OF SMOOTHING IF  (ZF - ZR ) NEGATIVE
!             DEFAULT VALUE : NLISS = 0 (NO SMOOTHING)
!
      NLISS = 0
!
!--------------------------------------------------
! CONTROL (CAN BE ACTIVATED IF ZR USER DEFINED...)
!--------------------------------------------------
!
!     DO I=1,NPOIN
!       IF(ZR(I).GT.ZF(I)) THEN
!         WRITE(LU,*) 'POINT ',I,' NON ERODABLE BED HIGHER THAN BED'
!         CALL PLANTE(1)
!         STOP
!       ENDIF
!     ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    *********************
                     SUBROUTINE INIT_COMPO
!                    *********************
!
     &(NCOUCHES)
!
!***********************************************************************
! SISYPHE   V7P2
!***********************************************************************
!
!brief    INITIAL FRACTION DISTRIBUTION, STRATIFICATION,
!+                VARIATION IN SPACE.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER
!code
!+  EXAMPLE:
!+      NCOUCHES(J) = 10
!+      ES(J,1) = 1.D0
!+      ES(J,2) = 1.D0
!+      ES(J,3) = 1.D0
!+      ES(J,4) = 1.D0
!+      ES(J,5) = 1.D0
!+      ES(J,6) = 1.D0
!+      ES(J,7) = 1.D0
!+      ES(J,8) = 1.D0
!+      ES(J,9) = 1.D0
!+        DO I = 1, NSICLA
!+          DO K = 1, NCOUCHES(J)
!+          AVAIL(J,K,I) = AVA0(I)
!+          ENDDO
!+        ENDDO
!
!history  MATTHIEU GONZALES DE LINARES
!+        2002
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        2016
!+        V7P2
!+   Checking coherence of data: ZR+sediment height=ZF
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NCOUCHES       |-->| NUMBER OF LAYER FOR EACH POINT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!                                       NPOIN
      INTEGER, INTENT (INOUT)::NCOUCHES(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J
      DOUBLE PRECISION EPAI
!
!-----------------------------------------------------------------------
!
      DO J=1,NPOIN
!
!       BY DEFAULT : UNIFORM BED COMPOSITION
!
        NCOUCHES(J) = 1
        DO I = 1, NSICLA
          AVAIL(J,1,I) = AVA0(I)
        ENDDO
        ES(J,1) = ZF%R(J)-ZR%R(J)
!
!  TO BE FILLED BY THE USER
!      NCOUCHES(J) = 10
!      ES(J,1) = 1.D0
!      ES(J,2) = 1.D0
!      ES(J,3) = 1.D0
!      ES(J,4) = 1.D0
!      ES(J,5) = 1.D0
!      ES(J,6) = 1.D0
!      ES(J,7) = 1.D0
!      ES(J,8) = 1.D0
!      ES(J,9) = 1.D0
!        DO I = 1, NSICLA
!          DO K = 1, NCOUCHES(J)
!          AVAIL(J,K,I) = AVA0(I)
!          ENDDO
!        ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
!
!     CHECKING THE CONSISTENCY OF DATA
!     THE FORMULA USED HERE ZR+SED. HEIGHT = ZF CAN BE USED TO GIVE THE
!     HEIGHT OF THE LAST LAYER.
!
      DO J=1,NPOIN
        EPAI=0.D0
        DO I=1,NCOUCHES(J)
          EPAI=EPAI+ES(J,I)
        ENDDO
        IF(ABS(ZR%R(J)+EPAI-ZF%R(J)).GT.1.D-6) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'INIT_COMPO, ERREUR :'
            WRITE(LU,*) 'ZR+EPAISSEUR=',ZR%R(J)+EPAI
            WRITE(LU,*) 'ZF=',ZF%R(J),' ZR=',ZR%R(J),' EPAISSEUR=',EPAI
            WRITE(LU,*) 'AU POINT ',J
          ELSE
            WRITE(LU,*) 'INIT_COMPO, ERROR:'
            WRITE(LU,*) 'ZR+SEDIMENT HEIGHT=',ZR%R(J)+EPAI
            WRITE(LU,*) 'ZF=',ZF%R(J),' ZR=',ZR%R(J),
     &                  ' SEDIMENT HEIGHT=',EPAI
            WRITE(LU,*) 'AT POINT ',J
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       *****************
                        SUBROUTINE CONDIN
!                       *****************
!
!***********************************************************************
! TELEMAC-2D VERSION 5.9         19/08/98  J-M HERVOUET TEL: 30 87 80 18
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
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN,ITRAC
!
      DOUBLE PRECISION EIKON
!
      INTRINSIC EXP
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
      CALL OS( 'X=0     ' , X=U )
      CALL OS( 'X=0     ' , X=V )
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DE H , LA HAUTEUR D'EAU
!
      IF(CDTINI(1:10).EQ.'COTE NULLE') THEN
        CALL OS( 'X=C     ' , H , H  , H , 0.D0 )
        CALL OS( 'X=X-Y   ' , H , ZF , H , 0.D0 )
      ELSEIF(CDTINI(1:14).EQ.'COTE CONSTANTE') THEN
        CALL OS( 'X=C     ' , H , H  , H , COTINI )
        CALL OS( 'X=X-Y   ' , H , ZF , H , 0.D0   )
      ELSEIF(CDTINI(1:13).EQ.'HAUTEUR NULLE') THEN
        CALL OS( 'X=C     ' , H , H  , H , 0.D0  )
      ELSEIF(CDTINI(1:13).EQ.'PARTICULIERES') THEN
      DO IPOIN=1,NPOIN
        EIKON=( (X(IPOIN)-10.05D0)**2 + (Y(IPOIN)-10.05D0)**2 ) / 4.D0
        H%R(IPOIN) = 2.4D0 * ( 1.D0 + EXP(-EIKON) )
      ENDDO
      ELSE
        WRITE(LU,*) 'CONDIN : CONDITION INITIALE NON PREVUE : ',CDTINI
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DU TRACEUR 1
!
      IF(NTRAC.GT.0) THEN
        CALL OS( 'X=0     ' , X=T%ADR(1)%P )
        DO IPOIN=1,NPOIN
          IF((X(IPOIN)-10.05D0)**2+(Y(IPOIN)-10.05D0)**2.LT.4.D0**2)
     &    THEN
            T%ADR(1)%P%R(IPOIN) = 1.D0
          ENDIF
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
