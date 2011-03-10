            
!===========================================================
! Wesel-Xanten, The Rhine River, Rhein-km 812.5 - 821.5
!===========================================================

      subroutine SURFINI
     &  (xle,yli,zli,xri,yri,zre,xm,ym,zm,
     &   x,y,zs,zf,ikle,elem,nsec,npoin2)

      implicit none 
      integer lng,lu
      common/info/lng,lu

      integer, intent(in) :: nsec,npoin2
      double precision, intent(in)    :: xle(nsec),yli(nsec),zli(nsec)
      double precision, intent(in)    :: xri(nsec),yri(nsec),zre(nsec)
      double precision, intent(inout) :: xm(2*nsec),ym(2*nsec)
      double precision, intent(inout) :: zm(2*nsec)
      double precision, intent(in) :: x(npoin2), y(npoin2),zf(npoin2)
      double precision, intent(inout) :: zs(npoin2)
      integer, intent(inout) :: ikle(2*nsec-2,3)
      integer, intent(inout) :: elem(npoin2) 
      double precision, allocatable :: shp(:,:)

      integer isec, i, ie
      integer n1, n2, n3
      double precision a1, a2, a3, surdet

      allocate(shp(npoin2,3))

      do isec = 1,nsec
        i = (isec-1)*2 + 1
        xm(i)   = xle(isec)
        xm(i+1) = xri(isec)
        ym(i)   = yli(isec)
        ym(i+1) = yri(isec)
        zm(i)   = zli(isec)
        zm(i+1) = zre(isec)
      end do

      do ie=1,2*nsec-3,2
        ikle(ie,1)   = ie 
        ikle(ie,2)   = ie+1
        ikle(ie,3)   = ie+2
        ikle(ie+1,1) = ie+1
        ikle(ie+1,2) = ie+3
        ikle(ie+1,3) = ie+2
      end do

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

      deallocate(shp)
      return
      end
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
      use declarations_telemac
      USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      integer i, ipoin, nsec,ITRAC,NFO1
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C 
      NFO1=T2D_FILES(T2DFO1)%LU 
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
      CALL OS( 'X=0     ' , X=U )
      CALL OS( 'X=0     ' , X=V )
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

!jaj free surface initialisation from a file and using surfini

      read(nfo1,*)
      read(nfo1,*) nsec
      write(lu,*) 'CONDIN: reading free surface initialisation file'
      write(lu,*) 'CONDIN: nsec = ',nsec
      write(lu,*) ' '
      write(lu,'(5(1x,a15))') 
     &    'xleft', 'yleft', 'xright', 'yright', 'water_level'
      do i=1,nsec
        read(nfo1,*) t1%r(i), t2%r(i), t4%r(i), t5%r(i), t3%r(i)
        t6%r(i) = t3%r(i)
        write(lu,'(5(1x,g15.6))') 
     &     t1%r(i), t2%r(i), t4%r(i), t5%r(i), t3%r(i)
      end do
      write(lu,*) ' '

      write(lu,*) 'CONDIN: cotini = ',cotini
      call OS( 'X=C     ' , h , h  , h , cotini )

      call SURFINI
     & (t1%r, t2%r, t3%r, t4%r, t5%r, t6%r, 
     &  t7%r, t8%r, t9%r, 
     &  x, y, h%r, zf%r, 
     &  it1%i, it2%i, nsec, npoin)

      call OS( 'X=X-Y   ' , h , zf , zf , 0.d0 )

!      do ipoin=1,npoin
!        h%r(ipoin) = max(h%r(ipoin),hmin)
!      end do

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
