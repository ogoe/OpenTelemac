C                       ******************
                        subroutine SURFINI
C                       ******************
C***********************************************************************
C
C     FUNKTION  : INITIALISIERUNG EINES GENEIGTEN WASSERSPIEGELS ANHAND 
C                 EINER TABELLE (NFO1)
C
C***********************************************************************
C
     &  (xle,yli,zli,xri,yri,zre,xm,ym,zm,
     &   x,y,zs,zf,ikle,elem,nsec,npoin2)

      implicit none 
      integer lng,lu
      common/info/lng,lu

      integer, intent(in) :: nsec,npoin2
      double precision, intent(in)    :: xle(nsec),yli(nsec),zli(nsec)
      double precision, intent(in)    :: xri(nsec),yri(nsec),zre(nsec)
      double precision, intent(inout) :: xm(nsec),ym(nsec),zm(nsec)
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
C TELEMAC 2D VERSION 5.8      30/08/07  J-M HERVOUET TEL: 01 30 87 80 18
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
!RK
      use declarations_telemac
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
      INTEGER NFO1      
!RK
      integer i, ipoin, nsec
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
        CALL OS( 'X=0     ' , X=H )
        CALL OS( 'X=X-Y   ' , X=H , Y=ZF )
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
CV
      NFO1 = T2D_FILES(T2DFO1)%LU
C end modif CV      
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

C                       ************************
                        SUBROUTINE FRICTION_USER
C                       ************************
C
C***********************************************************************
C  TELEMAC-2D VERSION 5.4                 J-M HERVOUET (LNH) 30 87 80 18
C***********************************************************************
C
C 15/04/04 : subroutine written by F. Huvelin
C
C***********************************************************************
C
C     FUNKTION  : DEFINITION VON RAUHEITSZONEN (FÜR JEDEN KNOTEN) ANHAND 
C                 EINER TABELLE (NFO2) (SURFINI BELEGT SCHON NFO1)
C
C***********************************************************************
C
          ! ----------------------------------------------- !
          !       Friction zones definition (by node)       !
          ! ----------------------------------------------- !
C
C
C               TTTTT EEEEE L     EEEEE M   M   AA  CCCCC
C                 T   E     L     E     MM MM  A  A C
C                 T   EEE   L     EEE   M M M  AAAA C
C                 T   E     L     E     M   M  A  A C
C                 T   EEEEE LLLLL EEEEE M   M  A  A CCCCC
C
C
C----------------------------------------------------------------------C
C                             ARGUMENTS                                C
C .________________.____.______________________________________________C
C |      NOM       |MODE|                   ROLE                       C
C |________________|____|______________________________________________C
C |                |    |                                              C
C |________________|____|______________________________________________C
C                    <=  input value                                   C
C                    =>  output value                                  C 
C ---------------------------------------------------------------------C

      ! Special for BAW
      ! ---------------
      ! In order to use the special BAW numbering
      ! A little computation have to be done :
      ! 
      ! 1/ Change BAW numbering
      ! For each node, cut zone code as :
      ! I1 = TYP (WALD, FLUSS, ...)      (3 digits)
      ! I2 = ORT (RECHTS, MITTEL, LINKS) (1 digit )
      ! I3 = km                          (5 digits)
      ! I = I3&I2&I1 becomes I = I1&I2&I3 for treatment
      !
      ! 2/ Find in wich zone or sub-zone(von..bis) is the node
      !
      ! 3/ Set the BAW numbering of the 1st sub-zone (GNUMB(1)) to the node

!=======================================================================!
!=======================================================================!
!                    DECLARATION DES TYPES ET DIMENSIONS                !
!=======================================================================!
!=======================================================================!

      USE BIEF
      USE FRICTION_DEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      IMPLICIT NONE      
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU

      !1/ Global variables
      !-------------------

      !2/ Local variables
      !------------------
      INTEGER           :: I,K,IVAL1,IVAL2,NFILE
      INTEGER           :: NPOIN_GLOB,NPTFR_INT_GLOB
      INTEGER, EXTERNAL :: P_ISUM
CV      CHARACTER*144     :: NOMFILE

      ! Special BAW
      INTEGER :: J, I0, I1, I2, I3
      INTEGER :: ITEMP, IERR, IERR1, IERR2
      INTEGER :: ULOOP
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IZ
      ! Special BAW

      ALLOCATE(IZ(NZONES,2))
!=======================================================================!
!=======================================================================!
!                               PROGRAMME                               !
!=======================================================================!
!=======================================================================!
!      NFILE   = NFO1
!      NOMFILE = NOMFO1
!CV for v6p0
!      NFILE   = NFO2
!      NOMFILE = NOMFO2
      NFILE   = T2D_FILES(T2DFO2)%LU
CV      NOMFILE = NOMFO2

      ! File Read in order to know the number of node in the mesh
      ! (this information is unknown for a parallel computation
      ! ---------------------------------------------------------
      IF (NCSIZE > 1) THEN
         NPOIN_GLOB = 0
         REWIND(NFILE)
         DO 
            READ(NFILE,*,END=666)
            NPOIN_GLOB = NPOIN_GLOB + 1
         ENDDO
      ELSE
         NPOIN_GLOB = NPOIN
      ENDIF
666   REWIND(NFILE)


      ! NEW ZONE NUMBERING
      ! ------------------
      IERR = 0
      DO I = 1, NZONES

         ! New zone numbering for each sub-zone
         ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         DO J = 1, 2
            IZ(I,J) = FRTAB%ADR(I)%P%GNUMB(J)
            I1      = MOD(IZ(I,J),1000)
            IZ(I,J) = IZ(I,J) - I1
            I2      = MOD(IZ(I,J),10000)
            I3      = IZ(I,J) - I2
            I1      = I1*1000000
            I2      = I2*100
            I3      = I3 / 10000
            IZ(I,J) = I1 + I2 + I3
         ENDDO

         IF (IZ(I,1) > IZ(I,2)) THEN
            IERR = 1
!           CALL FLUSH(LU)
            WRITE(LU,001) I, FRTAB%ADR(I)%P%GNUMB(1),
     &                       FRTAB%ADR(I)%P%GNUMB(2)
         ENDIF
      ENDDO

      IF (IERR == 1) CALL PLANTE(0)


      ! NEW "NODE" NUMBERING
      ! --------------------
      ULOOP = 0
      DO I = 1, NPOIN_GLOB

         READ(NFILE,*,END=999,ERR=998) IVAL1, IVAL2

         IF (NCSIZE>1) THEN
            K = MESH%KNOGL%I(I)
         ELSE
            K = I
         ENDIF

         IF (K == 0) GOTO 777

         KFROPT%I(K) = IVAL2

         ! New zone numbering for each node
         ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         I0    = IVAL2
         I1    = MOD(IVAL2,1000)
         IVAL2 = IVAL2 - I1
         I2    = MOD(IVAL2,10000)
         I3    = IVAL2 - I2
         I1    = I1 * 1000000
         I2    = I2 * 100
         I3    = I3 / 10000
         IVAL2 = I1 + I2 + I3

         ! Search if the node is in a sub-zone
         ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         ITEMP = 0
         IERR1 = 0
         IERR2 = 0

         DO J = 1, NZONES
            IF ((IZ(J,1) <= IVAL2).AND.(IVAL2 <= IZ(J,2))) THEN
                  IERR1 = IERR1 + 1
                  ITEMP = J
            ENDIF
         ENDDO

         DO J = 1, NZONES
            IF ((IZ(J,1) == IVAL2).AND.(IVAL2 == IZ(J,2))) THEN
                  IERR2 = IERR2 + 1
                  ITEMP = J
            ENDIF
         ENDDO

         ! Store the mode where a zone is not defined
         ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         IF ((IERR2 == 0).AND.(IERR1 == 0)) THEN
            ULOOP = ULOOP + 1
            IT1%I(ULOOP) = I

         ! Warning message for multi-zone definition
         ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         ELSE IF (IERR2 > 1) THEN
            WRITE(LU,003) KFROPT%I(K), I, IERR2,
     &      FRTAB%ADR(ITEMP)%P%GNUMB(1), FRTAB%ADR(ITEMP)%P%GNUMB(2)

         ! Warning message for multi-zone definition
         ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         ELSE IF ((IERR1 > 1).AND.(IERR2/=1)) THEN
            WRITE(LU,004) KFROPT%I(K), I, IERR1,
     &      FRTAB%ADR(ITEMP)%P%GNUMB(1), FRTAB%ADR(ITEMP)%P%GNUMB(2)
         ENDIF

         ! Set the value of the 1st sub-zone
         ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         IF (ITEMP > 0) KFROPT%I(K) = FRTAB%ADR(ITEMP)%P%GNUMB(1)

777      CONTINUE
      ENDDO


      ! STOP THE COMPUTATION IF NODE WITHOUT ZONE
      ! -----------------------------------------
      IF (ULOOP > 0) THEN
         DO I = 1, ULOOP   
            IF (NCSIZE>1) THEN
               K = MESH%KNOGL%I(IT1%I(I))
            ELSE
               K = I
            ENDIF
!           CALL FLUSH(LU)
            WRITE(LU,002) KFROPT%I(K), IT1%I(I)
         ENDDO
         WRITE(LU,005)
         CALL PLANTE(0)
      ENDIF

      GOTO 997


      ! --------------------------------------------------------------- !
      !             ERROR MESSAGE DURING THE READING                    !
      ! --------------------------------------------------------------- !

001   FORMAT(/'DEFINITION ERROR FOR ZONE : ',i9,
     &       /'YOU MUST HAVE ',i9,' < ',i9)
002   FORMAT(/'=================================================',
     &       /'STOP IN TELEMAC-2D',
     &       /'ERROR FRICTION DEFINITION',
     &       /'ZONE : ',i9,' AND NODE : ',i9,' NOT DEFINED')
003   FORMAT(/'WARNING FRICTION DEFINITION',
     &       /'ZONE : ',i9,' AND NODE : ',i9,' IS DEFINED ',i9,
     &        ' TIMES AS SINGLE ZONE',
     &  /'THE ZONE : FROM ',i9,' TO ',i9,' IS KEPT FOR THE COMPUTATION')
004   FORMAT(/'WARNING FRICTION DEFINITION',
     &       /'THE SUB-ZONE : ',i9,' AND NODE : ',i9,' IS DEFINED IN ',
     &        i9,' ZONES'
     &  /'THE ZONE : FROM ',i9,' TO ',i9,' IS KEPT FOR THE COMPUTATION')
005   FORMAT('=================================================')

      ! --------------------------------------------------------------- !

999   CONTINUE
CV      WRITE(LU,*) 'FORMATTED DATA FILE : ',NOMFILE
      WRITE(LU,*) 'ABNORMAL END OF FILE'
      CALL PLANTE(0)

998   CONTINUE
CV      WRITE(LU,*) 'FORMATTED DATA FILE : ',NOMFILE
      WRITE(LU,*) 'READ ERROR'
      CALL PLANTE(0)

      ! --------------------------------------------------------------- !
      ! --------------------------------------------------------------- !

997   CONTINUE
!=======================================================================!
!=======================================================================!
      DEALLOCATE(IZ)
      RETURN
      END SUBROUTINE FRICTION_USER
!
!
!
 
!                       **************************
                        SUBROUTINE UTIMP_TELEMAC2D
!                       **************************
!
     &(LTL,ATL,GRADEBL,GRAPRDL,LISDEBL,LISPRDL) 
!
!***********************************************************************
! TELEMAC 2D VERSION 5.4    AUGUST 2003       JACEK A. JANKOWSKI PINXIT
! BAW KARLSRUHE                                  jacek.jankowski@baw.de
!***********************************************************************
!
!      FONCTION:
!      =========
!
!    IMPRIME D'EVENTUELLES DONNEES DEMANDEES PAR L'UTILISATEUR
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! !  NOM           !MODE!                  ROLE                        !
! !________________!____!______________________________________________!
! !  LTL           ! -->! NUMERO DU PAS DE TEMPS                       !
! !  ATL           ! -->! TEMPS DU PAS DE TEMPS                        !
! !  GRADEBL       ! -->! 1ER PAS DE TEMPS POUR LES SORTIES GRAPHIQUES !
! !  GRAPRDL       ! -->! PERIODE DE SORTIE SUR LE FICHIER DE RESULTAT !
! !  LISDEBL       ! -->! 1ER PAS DE TEMPS POUR LES SORTIES LISTING    !
! !  LISPRDL       ! -->! PERIODE DE SORTIE LISTING                    !
! !________________!____!______________________________________________!
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
! SOUS-PROGRAMME APPELE PAR : TELEMAC2D
!
! please note this subroutine is called in the same places as the 
! main output subroutine of telemac2d named DESIMP, i.e. twice: 
!
! (1) once a run, when ltl==0, independently if
!     OUTPUT OF INITIAL CONDITIONS : YES is set or not
! (2) each time step just after DESIMP-output 
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
cgl 21.02.06
cgl      use OBSERVATEUR
!      use VARIAT
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      DOUBLE PRECISION, INTENT(IN) :: ATL
      INTEGER, INTENT(IN) :: LTL,GRADEBL,GRAPRDL,LISDEBL,LISPRDL
!
      INTRINSIC MOD
!
!***********************************************************************
! USER OUTPUT  
!
cgl 21.02.06
cgl      if (mod(ltl,lisprdl)==0) call OBSERVE (lu,nrfo,1,1)
!      if (ltl>=(nit-100)) call VARIA (lu,nrbi,(ltl==nit))
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE UTIMP_TELEMAC2D






C                       ***********************
                        SUBROUTINE COORD_FINDEN
C                       ***********************
     & (AnzTestKnoten, X1, Y1, I1)
C
C***********************************************************************
C  TELEMAC-2D VERSION 5.4                 J-M HERVOUET (LNH) 30 87 80 18
C***********************************************************************
C
C 07/05/2004 : subroutine written by F. Huvelin
C
C
C
C               TTTTT EEEEE L     EEEEE M   M   AA  CCCCC
C                 T   E     L     E     MM MM  A  A C
C                 T   EEE   L     EEE   M M M  AAAA C
C                 T   E     L     E     M   M  A  A C
C                 T   EEEEE LLLLL EEEEE M   M  A  A CCCCC
C
C
C----------------------------------------------------------------------C
C                             ARGUMENTS                                C
C .________________.____.______________________________________________C
C |      NOM       |MODE|                   ROLE                       C
C |________________|____|______________________________________________C
C |                |    |                                              C
C |________________|____|______________________________________________C
C                    <=  input value                                   C
C                    =>  output value                                  C 
C ---------------------------------------------------------------------C

!=======================================================================!
!=======================================================================!
!                    DECLARATION DES TYPES ET DIMENSIONS                !
!=======================================================================!
!=======================================================================!

      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D

      IMPLICIT NONE      
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU

      !1/ Global variables
      !-------------------
      INTEGER,          INTENT(IN)  :: AnzTestKnoten
      DOUBLE PRECISION, INTENT(IN)  :: X1(AnzTestKnoten)
      DOUBLE PRECISION, INTENT(IN)  :: Y1(AnzTestKnoten)
      INTEGER,          INTENT(OUT) :: I1(AnzTestKnoten)

      !2/ Local variables
      !------------------
      INTEGER          :: J, I, K
      DOUBLE PRECISION :: Distanz, D1
!=======================================================================!
!=======================================================================!
!                               PROGRAMME                               !
!=======================================================================!
!=======================================================================!

       DO J = 1, AnzTestKnoten

         I1(J) = 0
         D1 = 100.D0

         DO I= 1, NPOIN
            Distanz = (MESH%X%R(I)-X1(J))**2+(MESH%Y%R(I)-Y1(J))**2
            IF (Distanz < D1) THEN
               D1    = Distanz
               I1(J) = I
            ENDIF
         ENDDO

         IF (NCSIZE > 1) THEN
            K = MESH%KNOLG%I(I1(J))
         ELSE
           K = I1(J)
         ENDIF
         IF (K > 0) THEN
         WRITE(LU,*)'COORDINATE : X=',X1(J),' Y=',Y1(J)
         WRITE(LU,*)'POINT NEAR : Global node N=',K,'Local Node =',I1(J)
     &                  ,' X=',MESH%X%R(I1(J)),' Y=',MESH%X%R(I1(J))
         ENDIF
      ENDDO

!=======================================================================!
!=======================================================================!
      RETURN
      END SUBROUTINE COORD_FINDEN
