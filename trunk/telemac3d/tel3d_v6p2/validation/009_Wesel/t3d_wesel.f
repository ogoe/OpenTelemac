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
     *   CDTINI(1:14).EQ.'ZERO ELEVATION') THEN
        CALL OS( 'X=C     ' ,X=H,C=0.D0)
        CALL OV( 'X=X-Y   ' , H%R , Z , Z , 0.D0 , NPOIN2 )
      ELSEIF(CDTINI(1:14).EQ.'COTE CONSTANTE'.OR.
     *       CDTINI(1:18).EQ.'CONSTANT ELEVATION') THEN
        CALL OS( 'X=C     ' ,X=H,C=COTINI)
        CALL OV( 'X=X-Y   ' , H%R , Z , Z , 0.D0 , NPOIN2 )
      ELSEIF(CDTINI(1:13).EQ.'HAUTEUR NULLE'.OR.
     *       CDTINI(1:10).EQ.'ZERO DEPTH') THEN
        CALL OS( 'X=C     ' ,X=H,C=0.D0)
      ELSEIF(CDTINI(1:17).EQ.'HAUTEUR CONSTANTE'.OR.
     *       CDTINI(1:14).EQ.'CONSTANT DEPTH') THEN
        CALL OS( 'X=C     ' ,X=H,C=HAUTIN)
      ELSEIF(CDTINI(1:13).EQ.'PARTICULIERES'.OR.
     *       CDTINI(1:10).EQ.'PARTICULAR'.OR.
     *       CDTINI(1:07).EQ.'SPECIAL') THEN
!     ZONE A MODIFIER
!     FOR SPECIAL INITIAL CONDITIONS ON DEPTH, PROGRAM HERE                                                     
!jaj free surface initialisation from a file and using surfini
!
      read(nfo1,*)
      read(nfo1,*) nsec
      write(lu,*) 'CONDIM: reading free surface initialisation file'
      write(lu,*) 'CONDIM: nsec = ',nsec
      write(lu,*) ' '
      write(lu,'(5(1x,a15))') 
     &    'xleft', 'yleft', 'xright', 'yright', 'water_level'
      do i=1,nsec
        read(nfo1,*) t3_01%r(i), t3_02%r(i), t3_04%r(i),
     &               t3_05%r(i), t3_03%r(i)
        t3_06%r(i) = t3_03%r(i)
        write(lu,'(5(1x,g15.6))') 
     &     t3_01%r(i), t3_02%r(i), t3_04%r(i),
     &     t3_05%r(i), t3_03%r(i)
      end do
      write(lu,*) ' '
!
      write(lu,*) 'CONDIM: cotini = ',cotini
      call OS( 'X=C     ' , h , h  , h , cotini )
!
      call SURFINI
     & (t3_01%r, t3_02%r, t3_03%r, t3_04%r, t3_05%r, t3_06%r, 
     &  t3_07%r, t3_08%r, t3_09%r, 
     &  mesh3d%x%R, mesh3d%y%R, h%r, zf%r, 
     &  it1%i, it2%i, nsec, npoin2)
!
      call OS( 'X=X-Y   ' , h , zf , zf , 0.d0 )
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
      subroutine SURFINI
     &  (xle,yli,zli,xri,yri,zre,xm,ym,zm,
     &   x,y,zs,zf,ikle,elem,nsec,npoin2)
!
      implicit none 
      integer lng,lu
      common/info/lng,lu
!
      integer, intent(in) :: nsec,npoin2
      double precision, intent(in)    :: xle(nsec),yli(nsec),zli(nsec)
      double precision, intent(in)    :: xri(nsec),yri(nsec),zre(nsec)
      double precision,intent(inout)::xm(2*nsec),ym(2*nsec),zm(2*nsec)
      double precision, intent(in) :: x(npoin2), y(npoin2),zf(npoin2)
      double precision, intent(inout) :: zs(npoin2)
      integer, intent(inout) :: ikle(2*nsec-2,3)
      integer, intent(inout) :: elem(npoin2) 
      double precision, allocatable :: shp(:,:)
!
      integer isec, i, ie
      integer n1, n2, n3
      double precision a1, a2, a3, surdet
!
      allocate(shp(npoin2,3))
!
      do isec = 1,nsec
        i = (isec-1)*2 + 1
        xm(i)   = xle(isec)
        xm(i+1) = xri(isec)
        ym(i)   = yli(isec)
        ym(i+1) = yri(isec)
        zm(i)   = zli(isec)
        zm(i+1) = zre(isec)
      end do
!
      do ie=1,2*nsec-3,2
        ikle(ie,1)   = ie 
        ikle(ie,2)   = ie+1
        ikle(ie,3)   = ie+2
        ikle(ie+1,1) = ie+1
        ikle(ie+1,2) = ie+3
        ikle(ie+1,3) = ie+2
      end do
!
      do i=1,npoin2
        elem(i) = 0
        shp(i,1) = 0.0d0
        shp(i,2) = 0.0d0
        shp(i,3) = 0.0d0
        do ie=1,2*nsec-2
          n1 = ikle(ie,1)
          n2 = ikle(ie,2)
          n3 = ikle(ie,3)
          a1 = (x(i)-xm(n3))*(ym(n2)-ym(n3)) 
     &       - (y(i)-ym(n3))*(xm(n2)-xm(n3))
          a2 = (x(i)-xm(n1))*(ym(n3)-ym(n1)) 
     &       - (y(i)-ym(n1))*(xm(n3)-xm(n1))
          a3 = (x(i)-xm(n2))*(ym(n1)-ym(n2)) 
     &       - (y(i)-ym(n2))*(xm(n1)-xm(n2))
          if ((a1.ge.0.).and.(a2.ge.0.).and.(a3.ge.0.)) then
            surdet = 1.0 / ((xm(n2)-xm(n1))*(ym(n3)-ym(n1)) -
     &                      (ym(n2)-ym(n1))*(xm(n3)-xm(n1)))
            elem(i) = ie   
            shp(i,1) = a1 * surdet
            shp(i,2) = a2 * surdet
            shp(i,3) = a3 * surdet
            exit
          endif
        end do
      end do
!
      do i=1,npoin2
        if (elem(i)==0) then
          write (lu,*) 'SURFINI: Point ',i,
     &        ' is outside the domain for free surface initialisation'
          zs(i) = zf(i) 
        else
          n1 = ikle(elem(i),1)
          n2 = ikle(elem(i),2)
          n3 = ikle(elem(i),3)
          a1 = shp(i,1)
          a2 = shp(i,2)
          a3 = shp(i,3)
          zs(i) = a1*zm(n1) + a2*zm(n2) + a3*zm(n3)
        endif 
      end do
!
      deallocate(shp)
      return
      end
