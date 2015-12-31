!                    *****************
                     SUBROUTINE DRAGFO
!                    *****************
!
     &(FUDRAG,FVDRAG)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    ADDS THE DRAG FORCE OF VERTICAL STRUCTURES IN THE
!+                MOMENTUM EQUATION.
!code
!+  FU IS THEN USED IN THE EQUATION AS FOLLOWS :
!+
!+  DU/DT + U GRAD(U) = - G * GRAD(FREE SURFACE) +..... + FU_IMP * U
!+
!+  AND THE TERM FU_IMP * U IS TREATED IMPLICITLY.
!
!warning  USER SUBROUTINE
!
!history  J-M HERVOUET
!+        01/03/1990
!+        V5P2
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
!| FUDRAG         |<--| DRAG FORCE ALONG X
!| FVDRAG         |<--| DRAG FORCE ALONG Y
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
      TYPE(BIEF_OBJ), INTENT(INOUT) :: FUDRAG,FVDRAG
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,I,I4,NSOM,DISCLIN
      DOUBLE PRECISION UNORM,AIRE,SOM,XSOM(4),YSOM(4),X4,Y4
!     DOUBLE PRECISION, PARAMETER :: CD=1.56D0,DIAM=2.D0
      DOUBLE PRECISION, PARAMETER :: CD=1.34D0,DIAM=2.D0
      INTEGER, PARAMETER :: N=1
!
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
!
!-----------------------------------------------------------------------
!
!     COMPUTES THE MASSE INTEGRALS
!
      CALL VECTOR (T1,'=','MASBAS          ',UN%ELM,1.D0,
     &             S,S,S,S,S,S,MESH,.FALSE.,S)
!
      CALL CPSTVC(UN,FUDRAG)
      CALL CPSTVC(VN,FVDRAG)
      CALL OS('X=C     ',FUDRAG,FUDRAG,FUDRAG,0.D0)
      CALL OS('X=C     ',FVDRAG,FVDRAG,FVDRAG,0.D0)
!
!-----------------------------------------------------------------------
!
!     EXAMPLE : DRAGFORCE IS SET IN A QUADRILATERAL DEFINED BY
!               4 NODES
!     SURFACE OF 20 X 40 CENTERED ON (0,0)
!
      NSOM = 4
      XSOM(1) = -10.D0
      XSOM(2) =  10.D0
      XSOM(3) =  10.D0
      XSOM(4) = -10.D0
      YSOM(1) = -21.D0
      YSOM(2) = -21.D0
      YSOM(3) =  21.D0
      YSOM(4) =  21.D0
!
!--------------------------------------------------------------
!
!     P1 POINTS
!
      AIRE=0.D0
      DO I=1,BIEF_NBPTS(11,MESH)
!
        IF(INPOLY(X(I),Y(I),XSOM,YSOM,NSOM)) THEN
          UNORM = SQRT(UN%R(I)**2+VN%R(I)**2)
          FUDRAG%R(I) =  - 0.5D0 * N * DIAM * CD * UNORM
          FVDRAG%R(I) =  - 0.5D0 * N * DIAM * CD * UNORM
          AIRE = AIRE + T1%R(I)
        ENDIF
!
      ENDDO
!
!     QUASI-BUBBLE POINTS
!
      IF(FU%ELM.EQ.12) THEN
!
        DISCLIN=11
        CALL CHGDIS(FUDRAG,DISCLIN,12,MESH)
        CALL CHGDIS(FVDRAG,DISCLIN,12,MESH)
!
        DO IELEM = 1 , NELEM
          I4=IKLE%I(IELEM+3*NELMAX)
          X4=(X(IKLE%I(IELEM         ))+
     &        X(IKLE%I(IELEM+  NELMAX))+
     &        X(IKLE%I(IELEM+2*NELMAX)))/3.D0
          Y4=(Y(IKLE%I(IELEM         ))+
     &        Y(IKLE%I(IELEM+  NELMAX))+
     &        Y(IKLE%I(IELEM+2*NELMAX)))/3.D0
          IF(INPOLY(X4,Y4,XSOM,YSOM,NSOM)) AIRE = AIRE + T1%R(I4)
        ENDDO
!
      ENDIF
!
!     IN PARALLEL THE AREA MAY BE SPLIT INTO SEVERAL SUB-DOMAINS
!
      IF(NCSIZE.GT.0) AIRE=P_DSUM(AIRE)
!
!     NOW PREPARING THE DIVISION
!
      IF(AIRE.GT.1.D-6) THEN
        SOM = 1.D0 / AIRE
      ELSE
        IF(LNG.EQ.1) WRITE(LU,*) 'DRAGFO : AIRE DE LA ZONE NULLE'
        IF(LNG.EQ.2) WRITE(LU,*) 'DRAGFO: AREA OF ZONE EQUAL TO ZERO'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     DIVIDING BY THE AREA
!
      CALL OS('X=CX    ',X=FUDRAG,C=SOM)
      CALL OS('X=CX    ',X=FVDRAG,C=SOM)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       *****************
                        SUBROUTINE CONDIN
!                       *****************
!
!    ICI VITESSES U INITIALISEES
!
!
!***********************************************************************
! TELEMAC-2D VERSION 5.2         19/08/98  J-M HERVOUET TEL: 30 87 80 18
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
      INTEGER ITRAC
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DU TEMPS
!
      AT = 0.D0
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DES VITESSES :
!
      CALL OS( 'X=C     ' , U , U , U , 0.625D0 )
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
        IF(LNG.EQ.1) WRITE(LU,10)
        IF(LNG.EQ.2) WRITE(LU,11)
10      FORMAT(1X,'CONDIN : AVEC DES CONDITIONS INITIALES PARTICULIERES'
     &         ,/,'         VOUS DEVEZ MODIFIER CONDIN')
11      FORMAT(1X,'CONDIN : WITH SPECIAL INITIAL CONDITIONS'
     &         ,/,'         YOU HAVE TO MODIFY CONDIN')
        CALL PLANTE(1)
        STOP
!  FIN DE LA ZONE A MODIFIER
      ELSE
        IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'CONDIN : CONDITION INITIALE NON PREVUE : ',CDTINI
        ENDIF
        IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'CONDIN: INITIAL CONDITION UNKNOWN: ',CDTINI
        ENDIF
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
                        SUBROUTINE CORPOR
!                       *****************
!
     &(POROS)
!
!***********************************************************************
! PROGICIEL : TELEMAC-2D 5.1          01/03/90    J-M HERVOUET
!***********************************************************************
!
!  USER SUBROUTINE CORPOR
!
!  FUNCTION  : MODIFICATION OF THE POROSITY OF ELEMENTS
!
!
!-----------------------------------------------------------------------
!  ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|_______________________________________________
! |      POROS     |<-->| POROSITY TO BE MODIFIED.
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
      TYPE(BIEF_OBJ), INTENT(INOUT) :: POROS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION XSOM(4),YSOM(4),XX1,YY1,POR
      INTEGER NSOM,IELEM
!
!-----------------------------------------------------------------------
!
!     EXAMPLE : POROSITY IS SET TO 0.5 IN A QUADRILATERAL
!
!     Surface de 20 x 40 centree sur (0,0)
!
      NSOM = 4
      XSOM(1) = -20.D0
      XSOM(2) =  20.D0
      XSOM(3) =  20.D0
      XSOM(4) = -20.D0
      YSOM(1) = -21.D0
      YSOM(2) = -21.D0
      YSOM(3) =  21.D0
      YSOM(4) =  21.D0
!
!-----------------------------------------------------------------------
!
      CALL OS( 'X=C     ' , POROS , POROS , POROS , 1.D0 )
!
!--------------------------------------------------------------
!
      POR=19.D0/20.D0
!
      DO IELEM = 1 , NELEM
!
        XX1 = (  X(IKLE%I(IELEM)          )+
     &           X(IKLE%I(IELEM+NELMAX)   )+
     &           X(IKLE%I(IELEM+2*NELMAX) ))/3.D0
        YY1 = (  Y(IKLE%I(IELEM)          )+
     &           Y(IKLE%I(IELEM+NELMAX)   )+
     &           Y(IKLE%I(IELEM+2*NELMAX) ))/3.D0
!
        IF(INPOLY(XX1,YY1,XSOM,YSOM,NSOM)) THEN
          IF(XX1.GE.-10.D0.AND.XX1.LE.10.D0) THEN
            POROS%R(IELEM) = POR
          ELSEIF(XX1.LT.-10.D0) THEN
            POROS%R(IELEM) = POR - (1.D0-POR) * (XX1+10.D0) / 10.D0
          ELSEIF(XX1.GT.10.D0) THEN
            POROS%R(IELEM) = POR + (1.D0-POR) * (XX1-10.D0) / 10.D0
          ENDIF
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

