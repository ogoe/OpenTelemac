            
!===========================================================
! Wesel-Xanten, The Rhine River, Rhein-km 812.5 - 821.5
!===========================================================

      SUBROUTINE SURFINI
     &  (XLE,YLI,ZLI,XRI,YRI,ZRE,XM,YM,ZM,
     &   X,Y,ZS,ZF,IKLE,ELEM,NSEC,NPOIN2)

      IMPLICIT NONE 
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU

      INTEGER, INTENT(IN) :: NSEC,NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: XLE(NSEC),YLI(NSEC),ZLI(NSEC)
      DOUBLE PRECISION, INTENT(IN)    :: XRI(NSEC),YRI(NSEC),ZRE(NSEC)
      DOUBLE PRECISION, INTENT(INOUT) :: XM(2*NSEC),YM(2*NSEC)
      DOUBLE PRECISION, INTENT(INOUT) :: ZM(2*NSEC)
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN2), Y(NPOIN2),ZF(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: ZS(NPOIN2)
      INTEGER, INTENT(INOUT) :: IKLE(2*NSEC-2,3)
      INTEGER, INTENT(INOUT) :: ELEM(NPOIN2) 
      DOUBLE PRECISION, ALLOCATABLE :: SHP(:,:)

      INTEGER ISEC, I, IE
      INTEGER N1, N2, N3
      DOUBLE PRECISION A1, A2, A3, SURDET

      ALLOCATE(SHP(NPOIN2,3))

      DO ISEC = 1,NSEC
        I = (ISEC-1)*2 + 1
        XM(I)   = XLE(ISEC)
        XM(I+1) = XRI(ISEC)
        YM(I)   = YLI(ISEC)
        YM(I+1) = YRI(ISEC)
        ZM(I)   = ZLI(ISEC)
        ZM(I+1) = ZRE(ISEC)
      END DO

      DO IE=1,2*NSEC-3,2
        IKLE(IE,1)   = IE 
        IKLE(IE,2)   = IE+1
        IKLE(IE,3)   = IE+2
        IKLE(IE+1,1) = IE+1
        IKLE(IE+1,2) = IE+3
        IKLE(IE+1,3) = IE+2
      END DO

!      do i=1,2*nsec
!        write(lu,'(3g15.6)') xm(i), ym(i), zm(i)
!      end do
!
!      do ie=1,2*nsec-2 
!        write(lu,'(3i7)') ikle(ie,1), ikle(ie,2), ikle(ie,3)
!        write(lu,'(3g15.6)') 
!     &     xm(ikle(ie,1)), ym(ikle(ie,1)), zm(ikle(ie,1))
!        write(lu,'(3g15.6)') 
!     &     xm(ikle(ie,2)), ym(ikle(ie,2)), zm(ikle(ie,2))
!        write(lu,'(3g15.6)') 
!     &     xm(ikle(ie,3)), ym(ikle(ie,3)), zm(ikle(ie,3))
!      end do 

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

      DEALLOCATE(SHP)
      RETURN
      END
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
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I, IPOIN, NSEC,ITRAC,NFO1
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
! 
      NFO1=T2D_FILES(T2DFO1)%LU 
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

!jaj free surface initialisation from a file and using surfini

      READ(NFO1,*)
      READ(NFO1,*) NSEC
      WRITE(LU,*) 'CONDIN: READING FREE SURFACE INITIALISATION FILE'
      WRITE(LU,*) 'CONDIN: NSEC = ',NSEC
      WRITE(LU,*) ' '
      WRITE(LU,'(5(1X,A15))') 
     &    'XLEFT', 'YLEFT', 'XRIGHT', 'YRIGHT', 'WATER_LEVEL'
      DO I=1,NSEC
        READ(NFO1,*) T1%R(I), T2%R(I), T4%R(I), T5%R(I), T3%R(I)
        T6%R(I) = T3%R(I)
        WRITE(LU,'(5(1X,G15.6))') 
     &     T1%R(I), T2%R(I), T4%R(I), T5%R(I), T3%R(I)
      END DO
      WRITE(LU,*) ' '

      WRITE(LU,*) 'CONDIN: COTINI = ',COTINI
      CALL OS( 'X=C     ' , H , H  , H , COTINI )

      CALL SURFINI
     & (T1%R, T2%R, T3%R, T4%R, T5%R, T6%R, 
     &  T7%R, T8%R, T9%R, 
     &  X, Y, H%R, ZF%R, 
     &  IT1%I, IT2%I, NSEC, NPOIN)

      CALL OS( 'X=X-Y   ' , H , ZF , ZF , 0.D0 )

!      do ipoin=1,npoin
!        h%r(ipoin) = max(h%r(ipoin),hmin)
!      end do

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
