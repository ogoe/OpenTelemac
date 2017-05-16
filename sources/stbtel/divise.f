!                       *****************
                        SUBROUTINE DIVISE
!                       *****************
!
     &(X,Y,IKLE,NCOLOR,NPOIN,NELEM,NELMAX,NSOM2,SOM2,INDICP,INDICE,
     & SHP,ELT,NPMAX,CORR,LEVEL)
!
!***********************************************************************
! PROGICIEL : STBTEL  V7.2                 J-M JANIN   (LNH) 30 87 72 84
! ORIGINE   : TELEMAC
!***********************************************************************
!
!     FONCTION  :  DIVISION PAR 4 DE TOUTES LES MAILLES
!
!
!
!
!
!history  J-M HERVOUET (JUBILADO)
!+        24/10/2016
!+        V7P2
!+   Optimisation in the case of splitting of all elements.
!
!history  A. LEROY (EDF LAB, LNHE)
!+        27/10/2016
!+        V7P2
!+   Adding optional variables CORR and LEVEL for the automatic
!+   refinement procedure.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   X,Y          |<-->| COORDONNEES DU MAILLAGE .
! |   IKLE         |<-->| NUMEROS GLOBAUX DES NOEUDS DE CHAQUE ELEMENT
! |   NCOLOR       |<-->| TABLEAU DES COULEURS DES POINTS DU MAILLAGE
! |   NPOIN        |<-->| NOMBRE TOTAL DE NOEUDS DU MAILLAGE
! |   NELEM        |<-->| NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
! |   NELMAX       | -->| DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!----------------------------------------------------------------------
! APPELE PAR : STBTEL
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NSOM2,NELMAX,NPMAX
      INTEGER, INTENT(INOUT) :: ELT(NPMAX)
      INTEGER, INTENT(INOUT) :: NPOIN,NELEM
      INTEGER, INTENT(INOUT) :: IKLE(NELMAX,*),INDICP(*),INDICE(*)
      INTEGER, INTENT(INOUT) :: NCOLOR(*)
      DOUBLE PRECISION, INTENT(IN) :: SOM2(10,2)
      DOUBLE PRECISION, INTENT(INOUT) :: X(*),Y(*),SHP(NPMAX,3)
!     OPTIONAL ARGUMENTS
      INTEGER, INTENT(INOUT), OPTIONAL :: CORR(NELMAX,*)
      INTEGER, INTENT(IN), OPTIONAL :: LEVEL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IPOIN,ISOM,NO1,NO2,NO3,NP1,NP2,NP3,NE1,NE2,NE3,I1,I2
      INTEGER :: ISEG,NO(3),NP(3),NEXT(3),I
      DOUBLE PRECISION DX,DY
      LOGICAL FOUND
!##> SEB @ HRW: NO DATA STATEMENT FOR TYPES WITH ALLOCATABLE COMPONENTS
!      DATA NEXT/2,3,1/
      PARAMETER ( NEXT = (/ 2,3,1 /) )
!##< SEB @ HRW
!
      INTEGER, ALLOCATABLE :: TAB(:,:,:)
!
!-----------------------------------------------------------------------
!
      IF(NSOM2.GE.3) THEN
!
!=======================================================================
!      LOOKING FOR ELEMENTS TO BE SPLIT INTO 2 OR 4, SPLITTING THEM
!      SPLITTING ELEMENTS INTO 4 OR 2 WITHIN A POLYGON
!      THIS WILL ADD DUPLICATED POINTS TO BE ELIMINATED THEN IN REMAIL
!=======================================================================
!
      DO IPOIN = 1,NPOIN
        INDICP(IPOIN) = 1
      ENDDO
        DO ISOM = 1,NSOM2
          DX = SOM2(ISOM+1,1) - SOM2(ISOM,1)
          DY = SOM2(ISOM+1,2) - SOM2(ISOM,2)
          DO IPOIN = 1,NPOIN
            IF (DX*(Y(IPOIN)-SOM2(ISOM,2)).LT.
     &          DY*(X(IPOIN)-SOM2(ISOM,1))) INDICP(IPOIN) = 0
          ENDDO
        ENDDO
!
        DO IELEM = 1,NELEM
          INDICE(IELEM) = INDICP(IKLE(IELEM,1))
     &                + 2*INDICP(IKLE(IELEM,2))
     &                + 4*INDICP(IKLE(IELEM,3))
        ENDDO
!
        IPOIN = 1
!
        DO IELEM = 1,NELEM
!
          IF(INDICE(IELEM).EQ.7) THEN
!
            NO1 = IKLE(IELEM,1)
            NO2 = IKLE(IELEM,2)
            NO3 = IKLE(IELEM,3)
!
            NP1 = NPOIN + IPOIN
            NP2 = NP1   + 1
            NP3 = NP2   + 1
!
            NE1 = NELEM + IPOIN
            NE2 = NE1   + 1
            NE3 = NE2   + 1
!
            IPOIN = IPOIN + 3
!
            X(NP1) = 0.5D0 * ( X(NO1) + X(NO2) )
            X(NP2) = 0.5D0 * ( X(NO2) + X(NO3) )
            X(NP3) = 0.5D0 * ( X(NO3) + X(NO1) )
!
            Y(NP1) = 0.5D0 * ( Y(NO1) + Y(NO2) )
            Y(NP2) = 0.5D0 * ( Y(NO2) + Y(NO3) )
            Y(NP3) = 0.5D0 * ( Y(NO3) + Y(NO1) )
!
            NCOLOR(NP1) = NCOLOR(NO1)
            NCOLOR(NP2) = NCOLOR(NO2)
            NCOLOR(NP3) = NCOLOR(NO3)
!
            IKLE(IELEM,2) = NP1
            IKLE(IELEM,3) = NP3
!
            IKLE(  NE1,1) = NP1
            IKLE(  NE1,2) = NO2
            IKLE(  NE1,3) = NP2
!
            IKLE(  NE2,1) = NP3
            IKLE(  NE2,2) = NP2
            IKLE(  NE2,3) = NO3
!
            IKLE(  NE3,1) = NP2
            IKLE(  NE3,2) = NP3
            IKLE(  NE3,3) = NP1
!
          ELSEIF (INDICE(IELEM).EQ.3.OR.
     &            INDICE(IELEM).EQ.5.OR.
     &            INDICE(IELEM).EQ.6) THEN
!
            IF(INDICE(IELEM).EQ.3) THEN
              NO1 = IKLE(IELEM,1)
              NO2 = IKLE(IELEM,2)
              NO3 = IKLE(IELEM,3)
            ELSEIF (INDICE(IELEM).EQ.5) THEN
              NO1 = IKLE(IELEM,3)
              NO2 = IKLE(IELEM,1)
              NO3 = IKLE(IELEM,2)
            ELSE
              NO1 = IKLE(IELEM,2)
              NO2 = IKLE(IELEM,3)
              NO3 = IKLE(IELEM,1)
            ENDIF
!
            NP1 = NPOIN + IPOIN
            NE1 = NELEM + IPOIN
            IPOIN = IPOIN + 1
            X(NP1) = 0.5D0 * ( X(NO1) + X(NO2) )
            Y(NP1) = 0.5D0 * ( Y(NO1) + Y(NO2) )
            NCOLOR(NP1) = NCOLOR(NO1)
            IKLE(IELEM,1) = NO1
            IKLE(IELEM,2) = NP1
            IKLE(IELEM,3) = NO3
            IKLE(  NE1,1) = NO2
            IKLE(  NE1,2) = NO3
            IKLE(  NE1,3) = NP1
!
          ENDIF
!
        ENDDO !IELEM
!
        NPOIN = NPOIN + IPOIN - 1
        NELEM = NELEM + IPOIN - 1
!
      ELSE
!
!=======================================================================
!       SPLITTING ELEMENTS INTO 4 IN ALL THE DOMAIN
!       DONE WITHOUT UNDUE DUPLICATIONS, SO NO NEED TO CALL REMAIL AFTER
!=======================================================================
!
!       INDICP : WILL BE THE NUMBER OF SEGMENTS TO WHICH A POINT BELONGS
!                AND IS ITS POINT OF SMALLEST RANK
!                SUPPOSED HERE NOT TO BE LARGER THAN 11
!
!       TAB(IPOIN,I,1) : SECOND POINT OF Ith SEGMENT STARTING WITH POINT
!                        IPOIN
!
!       TAB(IPOIN,I,2) : MIDDLE POINT OF Ith SEGMENT STARTING WITH POINT
!                        IPOIN
!
        ALLOCATE(TAB(NPOIN,11,2))
!
        DO IPOIN = 1,NPOIN
          INDICP(IPOIN) = 0
        ENDDO
!
        DO IELEM=1,NELEM
!         ALREADY EXISTING 3 POINTS
          NO(1) = IKLE(IELEM,1)
          NO(2) = IKLE(IELEM,2)
          NO(3) = IKLE(IELEM,3)
!         3 NEW POINTS
          DO ISEG=1,3
            I1=MIN(NO(ISEG),NO(NEXT(ISEG)))
            I2=MAX(NO(ISEG),NO(NEXT(ISEG)))
            FOUND=.FALSE.
            IF(INDICP(I1).GT.0) THEN
!             LOOKING FOR AN ALREADY EXISTING SEGMENT STARTING WITH POINT I1
              DO I=1,INDICP(I1)
                IF(I2.EQ.TAB(I1,I,1)) THEN
!                 FOUND!
                  NP(ISEG)=TAB(I1,I,2)
                  FOUND=.TRUE.
                  EXIT
                ENDIF
              ENDDO
            ENDIF
!           CASE OF A NEW SEGMENT
            IF(.NOT.FOUND) THEN
              INDICP(I1)=INDICP(I1)+1
!             SECOND POINT OF NEW SEGMENT
              TAB(I1,INDICP(I1),1)=I2
!             RANK OF MIDDLE POINT OF NEW SEGMENT
              NPOIN=NPOIN+1
              TAB(I1,INDICP(I1),2)=NPOIN
              NP(ISEG)=NPOIN
            ENDIF
          ENDDO
!         3 NEW NUMBERS OF ELEMENTS
          NE1 =   NELEM + IELEM
          NE2 = 2*NELEM + IELEM
          NE3 = 3*NELEM + IELEM
!         NEW ARRAYS (COORDINATES, COLOUR,...)
          DO ISEG=1,3
            X(NP(ISEG)) = 0.5D0 * ( X(NO(ISEG)) + X(NO(NEXT(ISEG))) )
            Y(NP(ISEG)) = 0.5D0 * ( Y(NO(ISEG)) + Y(NO(NEXT(ISEG))) )
            NCOLOR(NP(ISEG)) = NCOLOR(NO(ISEG))
          ENDDO
!         OLD ELEMENT NUMBER TAKEN FOR FIRST NEW ONE
          IKLE(IELEM,2) = NP(1)
          IKLE(IELEM,3) = NP(3)
!         3 OTHER NEW ELEMENTS
          IKLE(NE1,1) = NP(1)
          IKLE(NE1,2) = NO(2)
          IKLE(NE1,3) = NP(2)
          IKLE(NE2,1) = NP(3)
          IKLE(NE2,2) = NP(2)
          IKLE(NE2,3) = NO(3)
          IKLE(NE3,1) = NP(2)
          IKLE(NE3,2) = NP(3)
          IKLE(NE3,3) = NP(1)
!         FILLING SHP AND ELT FOR THE 6 POINTS IN THE TRIANGLE
!         (THE GENERAL ALGORITHM IN INTERP IS AWFULLY LONG)
!         HERE POINTS WILL BE REACHED SEVERAL TIMES BUT WELL, NEVERMIND
!         IT IS MUCH FASTER
          ELT(NO(1))=IELEM
          SHP(NO(1),1)=1.D0
          SHP(NO(1),2)=0.D0
          SHP(NO(1),3)=0.D0
          ELT(NO(2))=IELEM
          SHP(NO(2),1)=0.D0
          SHP(NO(2),2)=1.D0
          SHP(NO(2),3)=0.D0
          ELT(NO(3))=IELEM
          SHP(NO(3),1)=0.D0
          SHP(NO(3),2)=0.D0
          SHP(NO(3),3)=1.D0
          ELT(NP(1))=IELEM
          SHP(NP(1),1)=0.5D0
          SHP(NP(1),2)=0.5D0
          SHP(NP(1),3)=0.D0
          ELT(NP(2))=IELEM
          SHP(NP(2),1)=0.D0
          SHP(NP(2),2)=0.5D0
          SHP(NP(2),3)=0.5D0
          ELT(NP(3))=IELEM
          SHP(NP(3),1)=0.5D0
          SHP(NP(3),2)=0.D0
          SHP(NP(3),3)=0.5D0
        ENDDO
!
!       FOR AUTOMATIC REFINEMENT PROCEDURE
!
        IF(PRESENT(CORR).AND.PRESENT(LEVEL)) THEN
          DO IELEM=1,NELEM
            CORR(IELEM,        LEVEL) = IELEM
            CORR(IELEM+  NELEM,LEVEL) = IELEM
            CORR(IELEM+2*NELEM,LEVEL) = IELEM
            CORR(IELEM+3*NELEM,LEVEL) = IELEM
          ENDDO
        ENDIF
!
        NELEM=4*NELEM
        DEALLOCATE(TAB)
!
      ENDIF
!
!=======================================================================
!  SORTIE LISTING
!=======================================================================
!
      IF (LNG.EQ.1) WRITE(LU,40) NPOIN,NELEM
      IF (LNG.EQ.2) WRITE(LU,50) NPOIN,NELEM
40    FORMAT(//,1X,'DIVISION PAR 4 DES ELEMENTS',
     &        /,1X,'---------------------------',/,
     &        /,1X,'NOUVEAU NOMBRE DE POINTS   :',I9,
     &        /,1X,'NOUVEAU NOMBRE D''ELEMENTS  :',I9)
50    FORMAT(//,1X,'CUTTING ELEMENTS BY 4',
     &        /,1X,'---------------------',/,
     &        /,1X,'NEW NUMBER OF POINTS   : ',I9,
     &        /,1X,'NEW NUMBER OF ELEMENTS : ',I9)
!
      RETURN
      END SUBROUTINE
