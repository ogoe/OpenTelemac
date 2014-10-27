!                       *****************
                        SUBROUTINE CONDIM
!                       *****************
!
!
!***********************************************************************
! TELEMAC 3D VERSION 5.5    11/12/00      J-M HERVOUET(LNH) 30 87 80 18
! FORTRAN95 VERSION         MARCH 1999        JACEK A. JANKOWSKI PINXIT
!***********************************************************************
!
!      FONCTION:
!      =========
!
!      INITIALISATION DES TABLEAUX DES GRANDEURS PHYSIQUES
!
!-----------------------------------------------------------------------
!
!      FUNCTION:
!      =========
!
!      INITIALISATION OF VELOCITY, DEPTH AND TRACERS
!
!-----------------------------------------------------------------------
!
! SOUS-PROGRAMME APPELE PAR : TELEMAC-3D
! SOUS-PROGRAMMES APPELES : OV , (CALCOT)
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
      INTEGER IPLAN, I,J,NSEC,NFO1
      NFO1=T3D_FILES(T3DFO1)%LU
!
!***********************************************************************
!
!     TIME ORIGIN
!
      AT  = 0.D0
!
!     INITIALISATION OF H, THE WATER DEPTH
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
        CALL OS( 'X=C     ' ,X=H,C=HAUTIN)
      ELSEIF(CDTINI(1:13).EQ.'PARTICULIERES'.OR.
     &       CDTINI(1:10).EQ.'PARTICULAR'.OR.
     &       CDTINI(1:07).EQ.'SPECIAL') THEN
!     ZONE A MODIFIER
!     FOR SPECIAL INITIAL CONDITIONS ON DEPTH, PROGRAM HERE                                                     
!jaj free surface initialisation from a file and using surfini
!
      READ(NFO1,*)
      READ(NFO1,*) NSEC
      WRITE(LU,*) 'CONDIM: READING FREE SURFACE INITIALISATION FILE'
      WRITE(LU,*) 'CONDIM: NSEC = ',NSEC
      WRITE(LU,*) ' '
      WRITE(LU,'(5(1X,A15))') 
     &    'XLEFT', 'YLEFT', 'XRIGHT', 'YRIGHT', 'WATER_LEVEL'
      DO I=1,NSEC
        READ(NFO1,*) T3_01%R(I), T3_02%R(I), T3_04%R(I),
     &               T3_05%R(I), T3_03%R(I)
        T3_06%R(I) = T3_03%R(I)
        WRITE(LU,'(5(1X,G15.6))') 
     &     T3_01%R(I), T3_02%R(I), T3_04%R(I),
     &     T3_05%R(I), T3_03%R(I)
      END DO
      WRITE(LU,*) ' '
!
      WRITE(LU,*) 'CONDIM: COTINI = ',COTINI
      CALL OS( 'X=C     ' , H , H  , H , COTINI )
!
      CALL SURFINI
     & (T3_01%R, T3_02%R, T3_03%R, T3_04%R, T3_05%R, T3_06%R, 
     &  T3_07%R, T3_08%R, T3_09%R, 
     &  MESH3D%X%R, MESH3D%Y%R, H%R, ZF%R, 
     &  IT1%I, IT2%I, NSEC, NPOIN2)
!
      CALL OS( 'X=X-Y   ' , H , ZF , ZF , 0.D0 )
!
!     END OF SPECIAL INITIAL CONDITIONS                                                            
!     FIN DE LA ZONE A MODIFIER      
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
!     CLIPPING OF H
!
      DO I=1,NPOIN2
        H%R(I)=MAX(H%R(I),0.D0)
      ENDDO
!
      CALL OS ('X=Y     ',X=HN,Y=H)
!
!-----------------------------------------------------------------------
!
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
!
!
!***********************************************************************
!
!     COMPUTATION OF ELEVATIONS
!     IF IT IS A CONTINUATION, WILL BE DONE AFTER CALLING 'SUITE'
!
      IF(DEBU) CALL CALCOT(Z,H%R)
!
!***********************************************************************
!
!     INITIALISATION OF VELOCITIES
!
      IF(SUIT2) THEN       
        DO I=1,NPLAN
          DO J=1,NPOIN2
           U%R((I-1)*NPOIN2+J)=U2D%R(J)
           V%R((I-1)*NPOIN2+J)=V2D%R(J)
          ENDDO
        ENDDO
      ELSE
        CALL OS( 'X=C     ' , X=U , C=0.D0 )
        CALL OS( 'X=C     ' , X=V , C=0.D0 )
      ENDIF
!
      CALL OS( 'X=C     ' , X=W , C=0.D0 )
!
!-----------------------------------------------------------------------
!
!     TRACERS INITIALIZATION
!
      IF(NTRAC.GT.0) THEN
        DO I=1,NTRAC
          CALL OS( 'X=C     ', X=TA%ADR(I)%P, C=TRAC0(I))
        ENDDO
      ENDIF
!
!
!-----------------------------------------------------------------------
!   INITIALISATION DU MODELE K-EPSILON (FACULTATIF)
!   SI VOUS LE FAITES, INDIQUEZ AKEP = .FALSE.
!
      AKEP=.TRUE.
!
!     IF(ITURBV.EQ.3) THEN
!
!       HERE INITIALISE K AND EPSILON
!
!       AKEP = .FALSE.
!     ENDIF
!
!
!-----------------------------------------------------------------------
!
! INITIALIZE THE PRESSURE FIELDS TO 0.0
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
!
!===========================================================
! Wesel-Xanten, The Rhine River, Rhein-km 812.5 - 821.5
!===========================================================
!
      SUBROUTINE SURFINI
     &  (XLE,YLI,ZLI,XRI,YRI,ZRE,XM,YM,ZM,
     &   X,Y,ZS,ZF,IKLE,ELEM,NSEC,NPOIN2)
!
      IMPLICIT NONE 
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER, INTENT(IN) :: NSEC,NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: XLE(NSEC),YLI(NSEC),ZLI(NSEC)
      DOUBLE PRECISION, INTENT(IN)    :: XRI(NSEC),YRI(NSEC),ZRE(NSEC)
      DOUBLE PRECISION,INTENT(INOUT)::XM(2*NSEC),YM(2*NSEC),ZM(2*NSEC)
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN2), Y(NPOIN2),ZF(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: ZS(NPOIN2)
      INTEGER, INTENT(INOUT) :: IKLE(2*NSEC-2,3)
      INTEGER, INTENT(INOUT) :: ELEM(NPOIN2) 
      DOUBLE PRECISION, ALLOCATABLE :: SHP(:,:)
!
      INTEGER ISEC, I, IE
      INTEGER N1, N2, N3
      DOUBLE PRECISION A1, A2, A3, SURDET
!
      ALLOCATE(SHP(NPOIN2,3))
!
      DO ISEC = 1,NSEC
        I = (ISEC-1)*2 + 1
        XM(I)   = XLE(ISEC)
        XM(I+1) = XRI(ISEC)
        YM(I)   = YLI(ISEC)
        YM(I+1) = YRI(ISEC)
        ZM(I)   = ZLI(ISEC)
        ZM(I+1) = ZRE(ISEC)
      END DO
!
      DO IE=1,2*NSEC-3,2
        IKLE(IE,1)   = IE 
        IKLE(IE,2)   = IE+1
        IKLE(IE,3)   = IE+2
        IKLE(IE+1,1) = IE+1
        IKLE(IE+1,2) = IE+3
        IKLE(IE+1,3) = IE+2
      END DO
!
      DO I=1,NPOIN2
        ELEM(I) = 0
        SHP(I,1) = 0.0D0
        SHP(I,2) = 0.0D0
        SHP(I,3) = 0.0D0
        DO IE=1,2*NSEC-2
          N1 = IKLE(IE,1)
          N2 = IKLE(IE,2)
          N3 = IKLE(IE,3)
          A1 = (X(I)-XM(N3))*(YM(N2)-YM(N3)) 
     &       - (Y(I)-YM(N3))*(XM(N2)-XM(N3))
          A2 = (X(I)-XM(N1))*(YM(N3)-YM(N1)) 
     &       - (Y(I)-YM(N1))*(XM(N3)-XM(N1))
          A3 = (X(I)-XM(N2))*(YM(N1)-YM(N2)) 
     &       - (Y(I)-YM(N2))*(XM(N1)-XM(N2))
          IF ((A1.GE.0.).AND.(A2.GE.0.).AND.(A3.GE.0.)) THEN
            SURDET = 1.0 / ((XM(N2)-XM(N1))*(YM(N3)-YM(N1)) -
     &                      (YM(N2)-YM(N1))*(XM(N3)-XM(N1)))
            ELEM(I) = IE   
            SHP(I,1) = A1 * SURDET
            SHP(I,2) = A2 * SURDET
            SHP(I,3) = A3 * SURDET
            EXIT
          ENDIF
        END DO
      END DO
!
      DO I=1,NPOIN2
        IF (ELEM(I)==0) THEN
          WRITE (LU,*) 'SURFINI: POINT ',I,
     &        ' IS OUTSIDE THE DOMAIN FOR FREE SURFACE INITIALISATION'
          ZS(I) = ZF(I) 
        ELSE
          N1 = IKLE(ELEM(I),1)
          N2 = IKLE(ELEM(I),2)
          N3 = IKLE(ELEM(I),3)
          A1 = SHP(I,1)
          A2 = SHP(I,2)
          A3 = SHP(I,3)
          ZS(I) = A1*ZM(N1) + A2*ZM(N2) + A3*ZM(N3)
        ENDIF 
      END DO
!
      DEALLOCATE(SHP)
      RETURN
      END
