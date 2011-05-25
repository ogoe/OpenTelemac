C                       *****************
                        SUBROUTINE CONDIN
C                       *****************
C
C    ICI VITESSES U INITIALISEES
C
C
C***********************************************************************
C TELEMAC-2D VERSION 5.2         19/08/98  J-M HERVOUET TEL: 30 87 80 18
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
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C 
      INTEGER ITRAC 
C
C-----------------------------------------------------------------------
C
C   INITIALISATION DU TEMPS
C
      AT = 0.D0
C
C-----------------------------------------------------------------------
C
C   INITIALISATION DES VITESSES : 
C
      CALL OS( 'X=C     ' , U , U , U , 0.625D0 )
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
        IF(LNG.EQ.1) WRITE(LU,10)                                       
        IF(LNG.EQ.2) WRITE(LU,11)                                       
10      FORMAT(1X,'CONDIN : AVEC DES CONDITIONS INITIALES PARTICULIERES'
     *         ,/,'         VOUS DEVEZ MODIFIER CONDIN')                
11      FORMAT(1X,'CONDIN : WITH SPECIAL INITIAL CONDITIONS'            
     *         ,/,'         YOU HAVE TO MODIFY CONDIN')                 
        CALL PLANTE(1)                                                  
        STOP                                                            
C  FIN DE LA ZONE A MODIFIER      
      ELSE
        IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'CONDIN : CONDITION INITIALE NON PREVUE : ',CDTINI
        ENDIF
        IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'CONDIN: INITIAL CONDITION UNKNOWN: ',CDTINI
        ENDIF
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
                        SUBROUTINE DRAGFO
C                       *****************
C
     *(FUDRAG,FVDRAG)
C
C***********************************************************************
C PROGICIEL : TELEMAC-2D 5.1          01/03/90    J-M HERVOUET
C***********************************************************************
C
C  USER SUBROUTINE DRAGFO
C
C  FUNCTION  : ADDING THE DRAG FORCE OF VERTICAL STRUCTURES IN THE
C              MOMENTUM EQUATION.
C
C  FU IS THEN USED IN THE EQUATION AS FOLLOWS :
C
C  DU/DT + U GRAD(U) = - G * GRAD(FREE SURFACE) +..... + FU_IMP * U
C
C  AND THE TERM FU_IMP * U IS TREATED IMPLICITLY.
C
C-----------------------------------------------------------------------
C  ARGUMENTS 
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|_______________________________________________
C |      FU,FV     |<-->| COEFFICIENTS WHERE TO ADD THE FRICTION TERM.
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
      TYPE(BIEF_OBJ), INTENT(INOUT) :: FUDRAG,FVDRAG
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,I,I4,NSOM
      DOUBLE PRECISION UNORM,AIRE,SOM,XSOM(4),YSOM(4),X4,Y4
C     DOUBLE PRECISION, PARAMETER :: CD=1.56D0,DIAM=2.D0
      DOUBLE PRECISION, PARAMETER :: CD=1.34D0,DIAM=2.D0
      INTEGER, PARAMETER :: N=1
C
      DOUBLE PRECISION P_DSUM
      EXTERNAL P_DSUM
C
C-----------------------------------------------------------------------
C
C     CALCUL DES INTEGRALES DE MASSES
C
      CALL VECTOR (T1,'=','MASBAS          ',UN%ELM,1.D0,
     *             S,S,S,S,S,S,MESH,.FALSE.,S)
C
      CALL CPSTVC(UN,FUDRAG)
      CALL CPSTVC(VN,FVDRAG)
      CALL OS('X=C     ',FUDRAG,FUDRAG,FUDRAG,0.D0)
      CALL OS('X=C     ',FVDRAG,FVDRAG,FVDRAG,0.D0)
C
C-----------------------------------------------------------------------
C
C     EXAMPLE : DRAGFORCE IS SET IN A QUADRILATERAL DEFINED BY
C               4 POINTS
C     Surface de 20 x 40 centree sur (0,0) 
C
      NSOM = 4
      XSOM(1) = -10.D0
      XSOM(2) =  10.D0
      XSOM(3) =  10.D0
      XSOM(4) = -10.D0
      YSOM(1) = -21.D0
      YSOM(2) = -21.D0
      YSOM(3) =  21.D0
      YSOM(4) =  21.D0
C                                                                      
C--------------------------------------------------------------
C                
C     P1 POINTS
C
      AIRE=0.D0   
      DO I=1,NBPTS(11)
C
        IF(INPOLY(X(I),Y(I),XSOM,YSOM,NSOM)) THEN
          UNORM = SQRT(UN%R(I)**2+VN%R(I)**2)
          FUDRAG%R(I) =  - 0.5D0 * N * DIAM * CD * UNORM 
          FVDRAG%R(I) =  - 0.5D0 * N * DIAM * CD * UNORM           
          AIRE = AIRE + T1%R(I)
        ENDIF
C
      ENDDO
C
C     QUASI-BUBBLE POINTS
C
      IF(FU%ELM.EQ.12) THEN
C
        CALL CHGDIS(FUDRAG,11,12,MESH)
        CALL CHGDIS(FVDRAG,11,12,MESH)
C
        DO IELEM = 1 , NELEM                                                                                                                       
          I4=IKLE%I(IELEM+3*NELMAX)
          X4=(X(IKLE%I(IELEM         ))+
     *        X(IKLE%I(IELEM+  NELMAX))+
     *        X(IKLE%I(IELEM+2*NELMAX)))/3.D0
          Y4=(Y(IKLE%I(IELEM         ))+
     *        Y(IKLE%I(IELEM+  NELMAX))+
     *        Y(IKLE%I(IELEM+2*NELMAX)))/3.D0
          IF(INPOLY(X4,Y4,XSOM,YSOM,NSOM)) AIRE = AIRE + T1%R(I4)
        ENDDO                      
C
      ENDIF
C
      IF(NCSIZE.GT.1) AIRE=P_DSUM(AIRE)
C
      SOM = 1.D0 / AIRE
C
      CALL OS('X=CX    ',FUDRAG,T1,T1,SOM)  
      CALL OS('X=CX    ',FVDRAG,T1,T1,SOM)           
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C                       *****************
                        SUBROUTINE CORPOR
C                       *****************
C
     *(POROS)
C
C***********************************************************************
C PROGICIEL : TELEMAC-2D 5.1          01/03/90    J-M HERVOUET
C***********************************************************************
C
C  USER SUBROUTINE CORPOR
C
C  FUNCTION  : MODIFICATION OF THE POROSITY OF ELEMENTS
C
C
C-----------------------------------------------------------------------
C  ARGUMENTS 
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|_______________________________________________
C |      POROS     |<-->| POROSITY TO BE MODIFIED.
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
      TYPE(BIEF_OBJ), INTENT(INOUT) :: POROS
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION XSOM(4),YSOM(4),XX1,YY1,POR
      INTEGER NSOM,IELEM
C
C-----------------------------------------------------------------------
C
C     EXAMPLE : POROSITY IS SET TO 0.5 IN A QUADRILATERAL
C
C     Surface de 20 x 40 centree sur (0,0) 
C
      NSOM = 4
      XSOM(1) = -20.D0
      XSOM(2) =  20.D0
      XSOM(3) =  20.D0
      XSOM(4) = -20.D0
      YSOM(1) = -21.D0
      YSOM(2) = -21.D0
      YSOM(3) =  21.D0
      YSOM(4) =  21.D0
C                                                                       
C-----------------------------------------------------------------------
C                                                                  
      CALL OS( 'X=C     ' , POROS , POROS , POROS , 1.D0 )                   
C                                                                       
C--------------------------------------------------------------
C
      POR=19.D0/20.D0
C
      DO 4 IELEM = 1 , NELEM                                         
C                                                                            
        XX1 = (  X(IKLE%I(IELEM)          )+
     *           X(IKLE%I(IELEM+NELMAX)   )+
     *           X(IKLE%I(IELEM+2*NELMAX) ))/3.D0 
        YY1 = (  Y(IKLE%I(IELEM)          )+
     *           Y(IKLE%I(IELEM+NELMAX)   )+
     *           Y(IKLE%I(IELEM+2*NELMAX) ))/3.D0                                     
C
        IF(INPOLY(XX1,YY1,XSOM,YSOM,NSOM)) THEN
          IF(XX1.GE.-10.D0.AND.XX1.LE.10.D0) THEN
            POROS%R(IELEM) = POR
          ELSEIF(XX1.LT.-10.D0) THEN
            POROS%R(IELEM) = POR - (1.D0-POR) * (XX1+10.D0) / 10.D0
          ELSEIF(XX1.GT.10.D0) THEN
            POROS%R(IELEM) = POR + (1.D0-POR) * (XX1-10.D0) / 10.D0            
          ENDIF
        ENDIF
C                                      
4     CONTINUE                                                       
C
C-----------------------------------------------------------------------
C
      RETURN
      END
      
