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
CRK
      DOUBLE PRECISION ZSo, ZSu, xmin,xmax,ymin,ymax,ZS,DS
      DOUBLE PRECISION Yo, Yu, Xo, Xu      
      INTEGER i, ipoin, nsec,ITRAC
      
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C 
      INTEGER NFO1
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
      CALL OS( 'X=C     ' , U , U , U , 0.D0 )
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
C        CALL PLANTE(1)                                                  
C        STOP                                                            
C  FIN DE LA ZONE A MODIFIER


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
C                       ***************************
                        DOUBLE PRECISION FUNCTION Q
C                       ***************************
C
     *( I )
C
C***********************************************************************
C  TELEMAC 2D VERSION 5.2    17/08/94    J-M HERVOUET (LNH) 30 87 80 18
C
C***********************************************************************
C
C FONCTION  : DONNE LA VALEUR DU DEBIT POUR TOUTES LES ENTREES A DEBIT
C             IMPOSE.
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |   AT           | -->| TEMPS AUQUEL ON DONNE LE DEBIT               |
C |   I            | -->| RANG DE LA FRONTIERE A DEBIT IMPOSE          |
C |                |    | (1 S'IL N'Y EN A QU'UNE)                     |
C |   DEBIT        | -->| TABLEAU DES DEBITS IMPOSES.                  |
C |                |    | (LU DANS LE FICHIER DES PARAMETRES)          |
C |   HAUTEUR D'EAU| -->| TABLEAU DES HAUTEURS D'EAU.                  |
C |   NPOIN        | -->| NOMBRE DE POINTS DU TABLEAU DES HAUTEURS     |
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C  APPELE PAR : BORD
C
C***********************************************************************
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER         , INTENT(IN) :: I
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CHARACTER*8 FCT
      INTEGER N
      LOGICAL DEJA,OK(MAXFRO)
      DATA    DEJA /.FALSE./
      SAVE    OK,DEJA
C
C     FIRST CALL, OK INITIALISED TO .TRUE.
C
      IF(.NOT.DEJA) THEN
        DO N=1,MAXFRO
          OK(N)=.TRUE.
        ENDDO
        DEJA=.TRUE.
      ENDIF
C
C     IF FILE OF LIQUID BOUNDARIES EXISTING, ATTEMPT TO FIND
C     THE VALUE IN IT. IF YES, OK REMAINS TO .TRUE. FOR NEXT CALLS
C                      IF  NO, OK SET     TO .FALSE.
C
      IF(OK(I).AND.T2D_FILES(T2DIMP)%NAME(1:1).NE.' ') THEN
C
C       FCT WILL BE Q(1), Q(2), ETC, Q(99), DEPENDING ON I
        FCT(1:2)='Q('
        IF(I.LT.10) THEN 
          WRITE(FCT(3:3),FMT='(I1)') I
          FCT(4:8)=')    '
        ELSEIF(I.LT.100) THEN
          WRITE(FCT(3:4),FMT='(I2)') I
          FCT(5:8)=')   '
        ELSE
          WRITE(LU,*) 'Q NOT PROGRAMMED FOR MORE THAN 99 BOUNDARIES'
          CALL PLANTE(1)
          STOP 
        ENDIF
        CALL READ_FIC_FRLIQ(Q,FCT,AT,T2D_FILES(T2DIMP)%LU,ENTET,OK(I)) 
C
      ENDIF
C
      IF(.NOT.OK(I).OR.T2D_FILES(T2DIMP)%NAME(1:1).EQ.' ') THEN
C 
C     PROGRAMMABLE PART                              
C     Q IS TAKEN IN THE PARAMETER FILE, BUT MAY BE CHANGED 
                                                             
         IF(AT.LE.600.D0) THEN
           Q = DEBIT(I) * AT/600.D0
         ELSE
           Q = DEBIT(I)
         ENDIF             
C 
      ENDIF          
C
C-----------------------------------------------------------------------
C
      RETURN
      END

      subroutine SURFINI
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

cgl
          if(surdet.le.0.0)then
            write (lu,*) 'surdet ist kleiner null'
          endif 		
cgl

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
                        SUBROUTINE CORSTR
C                       *****************
C
C***********************************************************************
C  TELEMAC 2D VERSION 5.2    17/08/94    J-M HERVOUET (LNH) 30 87 80 18
C
C***********************************************************************
C
C      FONCTION: CORRECTION DU COEFFICIENT DE FROTTEMENT SUR LE FOND
C                QUAND IL EST VARIABLE EN TEMPS.
C
C      CE SOUS-PROGRAMME EST SIMPLEMENT UN MODELE
C      IL DOIT ETRE REMPLI PAR L'UTILISATEUR
C
C
C 
C
C-----------------------------------------------------------------------
C  EXAMPLE OF POSSIBLE ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |    CHESTR      |<-- |  COEFFICIENT DE FROTTEMENT                   |
C |    X,Y         | -->|  COORDONNEE DU MAILLAGE .                    |
C |    NPOIN       | -->|  NOMBRE DE POINTS DU MAILLAGE                |
C |    PRIVE       | -->|  TABLEAU DE TRAVAIL DEFINI DANS PRINCI       |
C |    ZF          | -->|  COTE DU FOND                                |
C |    KFROT       | -->|  LOI DE FROTTEMENT (LINEAIRE,CHEZY,STRICKLER)|
C |    FFON        | -->|  COEFFICIENT DE FROTTEMENT ASSOCIE A LA LOI  |
C |    H           | -->|  HAUTEUR D'EAU.
C |    AT          | -->|  TIME.
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C  APPELE PAR : TELMAC
C
C  SOUS-PROGRAMME APPELE :
C
C**********************************************************************
C
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER :: I
C
C-----------------------------------------------------------------------

      IF (LT == 1) THEN
         DO I = 1, NPOIN
            IF ( ABS(CHESTR%R(I) - 0.025D0) < 1.D-7 ) THEN
               CHESTR%R(I) = 0.020D0
            END IF
         ENDDO
      END IF

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
      INTEGER       :: I,K,IVAL1,IVAL2,NFILE
      INTEGER       :: NPOIN_GLOB,NPTFR_INT_GLOB,P_ISUM
      CHARACTER*144 :: NOMFILE

      EXTERNAL P_ISUM
!=======================================================================!
!=======================================================================!
!                               PROGRAMME                               !
!=======================================================================!
!=======================================================================!
      NFILE   = T2D_FILES(T2DFO1)%LU
      NOMFILE = T2D_FILES(T2DFO1)%NAME

      ! File Read in order to know the number of node in the mesh
      ! (this information is unknown for a parallel computation
      ! ---------------------------------------------------------
      NPOIN_GLOB = 0

      DO 
         READ(NFILE,*,END=666)
         NPOIN_GLOB = NPOIN_GLOB + 1
      ENDDO

666   CONTINUE      

      REWIND(NFILE)

      ! File Read
      ! ---------
      DO I = 1,NPOIN_GLOB
         READ(NFILE,*,END=999,ERR=998) IVAL1, IVAL2
         IF (NCSIZE>1) THEN
            K = MESH%KNOGL%I(I)
         ELSE
            K = I
         ENDIF
         KFROPT%I(K) = IVAL2
      ENDDO
      GOTO 997

      ! --------------------------------------------------------------- !
      !             ERROR MESSAGE DURING THE READING                    !
      ! --------------------------------------------------------------- !
999   CONTINUE
      IF(LNG.EQ.1) THEN
         WRITE(LU,*) 'FICHIER DE DONNEES FORMATE : ',NOMFILE
         WRITE(LU,*) 'FIN DE FICHIER ANORMALE'
      ENDIF
      IF(LNG.EQ.2) THEN
         WRITE(LU,*) 'FORMATTED DATA FILE : ',NOMFILE
         WRITE(LU,*) 'ABNORMAL END OF FILE'
      ENDIF
      CALL PLANTE(1)
      STOP

998      CONTINUE
      IF(LNG.EQ.1) THEN
         WRITE(LU,*) 'FICHIER DE DONNEES FORMATE : ',NOMFILE
         WRITE(LU,*) 'ERREUR DE LECTURE'
      ENDIF
      IF(LNG.EQ.2) THEN
         WRITE(LU,*) 'FORMATTED DATA FILE : ',NOMFILE
         WRITE(LU,*) 'READ ERROR'
      ENDIF
      CALL PLANTE(1)
      STOP
      ! --------------------------------------------------------------- !
      ! --------------------------------------------------------------- !

997   CONTINUE
!=======================================================================!
!=======================================================================!
      RETURN
      END SUBROUTINE FRICTION_USER
