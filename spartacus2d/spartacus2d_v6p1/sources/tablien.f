!                        ********************
                          SUBROUTINE TABLIEN
!                        ********************
!
     &   (NLIEN , NLIENMAX , NPART , NPARTQ   , NPARTQMAX, NPMAX ,
     &    NQUADX, NQUADXMAX, NQUADZ, NQUADZMAX, ILIEN    , IPARTQ,
     &    KLIST , KPER     , H     , RAB      , SUPP     , VX    ,
     &    VXAB  , VZ       , VZAB  , X        , XMIN     , XMAX  ,
     &    XAB   , Z        , ZMIN  , ZAB                         )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! H         ! -->  ! SMOOTHING LENGTH                               !
! ! ILIEN     ! <--  ! PARTICLE LINK LIST                             !
! ! IPARTQ    ! <--  ! INDEX OF PARTICLES LOCATED IN A SQUARE         !
! ! KLIST     ! -->  ! LOGICAL FOR LISTING PRINTOUT                   !
! ! KPER      ! -->  ! LOGICAL INDEX FOR PERIODICITY                  !
! ! LNG       ! -->  ! CHOICE INDEX FOR LANGUAGE                      !
! ! NLIEN     ! <--  ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
! ! NLIENMAX  ! -->  ! MAXIMUM NUMBER OF LINKS RELATIVE TO A PARTICLE !
! ! NPART     ! -->  ! TOTAL PARTICLE NUMBER                          !
! ! NPARTQ    ! <--  ! NUMBER OF PARTICLES IN A SQUARE                !
! ! NPARTQMAX ! -->  ! MAXIMUM NUMBER OF PARTICLES IN A SQUARE        !
! ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
! ! NQUADX,                                                           !
! ! NQUADZ    ! <--  ! NUMBER OF SQUARES ALONG EACH DIRECTION         !
! ! NQUADXMAX,                                                        !
! ! NQUADZMAX ! -->  ! MAXIMUM NUMBER OF SQUARES ALONG EACH DIRECTION !
! ! RAB       ! <--  ! INTERPARTICLE DISTANCE                         !
! ! SUPP      ! -->  ! KERNEL SUPPORT                                 !
! ! VX, VZ    ! -->  ! VELOCITY COMPONENTS                            !
! ! VXAB, VZAB! <--  ! VELOCITY DIFFERENCE BETWEEN PARTICLES          !
! ! X, Z      ! -->  ! PARTICLE POSITION                              !
! ! XMIN, XMAX! -->  ! MINIMUM AND MAXIMUM X OF THE DOMAIN            !
! ! XAB, ZAB  ! <--  ! COORDINATE DIFFERENCES BETWEEN PARTICLES       !
! ! ZMIN, ZMAX! -->  ! MINIMUM AND MAXIMUM Z OF THE DOMAIN            !
! !___________!______!________________________________________________!
!
! MODE : -->(NON MODIFIED DATA), <--(RESULT), <-->(MODIFIED DATA)
!----------------------------------------------------------------------
!
! SPARTACUS2D V5P9
! D. Violeau           & R. Issa
! +33(0)1-30-87-78-31 // +33(0)1-30-87-84-28
! LNHE - 2008
!
! FONCTION : determine les liens entre particules, etablit les valeurs
!            des champs XAB, ZAB, RAB, VXAB et VZAB
! FUNCTION : defines the links between particles and computed the values
!            of XAB, ZAB, RAB, VXAB and VZAB fields
!
! PROGRAMMES APPELANT : SPARTACUS2D
! CALLED BY
!
! PROGRAMMES APPELES  : -
! CALLED PROGRAMS
!
!----------------------------------------------------------------------
!
! Variables
!==========
!
      IMPLICIT NONE
!
      INTEGER NPMAX    , NPART    , NLIENMAX
      INTEGER NQUADXMAX, NQUADZMAX, NPARTQMAX
      INTEGER NQUADX   , NQUADZ
      INTEGER I        , J        , L
      INTEGER N        , M        , K
      INTEGER MMIN     , MMAX     , LNG
      INTEGER NMIN     , NMAX     , LU
      COMMON/INFO/LNG,LU
!
      LOGICAL KLIST, KPER
!
      DOUBLE PRECISION XMIN, ZMIN, H , SUPP, ETA
      DOUBLE PRECISION HT2 , XMAX, R2
!
      INTEGER ILIEN (NPMAX,NLIENMAX)
      INTEGER NPARTQ(NQUADXMAX,NQUADZMAX)
      INTEGER IPARTQ(NQUADXMAX,NQUADZMAX,NPARTQMAX)
!
      INTEGER NLIEN(NPMAX)
      INTEGER IQUAD(NPMAX)
      INTEGER JQUAD(NPMAX)
!
      DOUBLE PRECISION X (NPMAX), Z (NPMAX)
      DOUBLE PRECISION VX(NPMAX), VZ(NPMAX)
!
      DOUBLE PRECISION VXAB(NPMAX,NLIENMAX), VZAB(NPMAX,NLIENMAX)
      DOUBLE PRECISION XAB (NPMAX,NLIENMAX), ZAB (NPMAX,NLIENMAX)
      DOUBLE PRECISION RAB (NPMAX,NLIENMAX)
!
! Initialisations
!================
! Initializations
!================
!
      ETA   = 1.0D-16
      HT2= (SUPP*H)**2
!
! Construction du quadrillage
!============================
! Coarse grid construction
!=========================
!
! Nombre de carres et, pour chaque particule, indice de carre
!------------------------------------------------------------
! Number of sqaures and sqaure indexs for each particle
!------------------------------------------------------
!
      NQUADX=0
      NQUADZ=0
      DO 150 I=1,NPART
        IQUAD(I)=INT((X(I)-XMIN)/(SUPP*H))+1
        IF (IQUAD(I).GT.NQUADX) NQUADX=IQUAD(I)
        JQUAD(I)=INT((Z(I)-ZMIN)/(SUPP*H))+1
        IF (JQUAD(I).GT.NQUADZ) NQUADZ=JQUAD(I)
!
        IF (NQUADX.GT.NQUADXMAX.OR.NQUADZ.GT.NQUADZMAX) THEN
          IF (LNG.EQ.1) THEN
            PRINT*,'ERREUR : Nombre de carres maximum insuffisant : ',
     &      'NQUADX =',NQUADX,', NQUADZ =',NQUADZ
            STOP
          ELSEIF (LNG.EQ.2) THEN
            PRINT*,'ERROR: Maximum number of squares not sufficient :',
     &      'NQUADX =',NQUADX,', NQUADZ =',NQUADZ
            STOP
          ENDIF
        ENDIF
!
 150  CONTINUE
!
      IF (KLIST) THEN
        IF (LNG.EQ.1) THEN
          PRINT901,' Nombre de carres : ',NQUADX,' * ',NQUADZ
        ELSEIF (LNG.EQ.2) THEN
          PRINT901,' Number of squares : ',NQUADX,' * ',NQUADZ
        ENDIF
      ENDIF
!
! Pour chaque carre, table des particules et nombre de particules
!----------------------------------------------------------------
! For each square, particle link list and particle number
!--------------------------------------------------------
!
      DO 152 I=1,NQUADX
        DO 153 J=1,NQUADZ
          NPARTQ(I,J)=0
 153    CONTINUE
 152  CONTINUE
!
      DO 151 M=1,NPART
        I=IQUAD(M)
        J=JQUAD(M)
        L=NPARTQ(I,J)+1
        IF (L.GT.NPARTQMAX) THEN
          IF (LNG.EQ.1) THEN
            PRINT*,''
            PRINT*,'Particule numero',M
            PRINT*,'X =',X(M)
            PRINT*,'Z =',Z(M)
            PRINT*,'ERREUR : NPARTQMAX trop petit : I,J,L= ',I,J,L
            PRINT*,''
            STOP
          ELSEIF (LNG.EQ.2) THEN
            PRINT*,''
            PRINT*,'Index particle',M
            PRINT*,'X =',X(M)
            PRINT*,'Z =',Z(M)
            PRINT*,'ERROR : NPARTQMAX too small : I,J,L= ',I,J,L
            PRINT*,''
            STOP
          ENDIF
        ENDIF
        IPARTQ(I,J,L)=M
        NPARTQ(I,J)=L
 151  CONTINUE
!
! Construction du tableau de liens
!---------------------------------
! Link list construction
!-----------------------
!
      DO 154 I=1,NPART
        NLIEN(I)=0
 154  CONTINUE
!
      DO 121 I=1,NPART
        MMIN=IQUAD(I)-1
        MMAX=IQUAD(I)+1
        NMIN=JQUAD(I)-1
        NMAX=JQUAD(I)+1
        IF (MMIN.EQ.0       ) MMIN=1
        IF (MMAX.EQ.NQUADX+1) MMAX=NQUADX
        IF (NMIN.EQ.0)        NMIN=1
        IF (NMAX.EQ.NQUADZ+1) NMAX=NQUADZ
!
        DO 122 M=MMIN,MMAX
          DO 161 N=NMIN,NMAX
            DO 162 L=1,NPARTQ(M,N)
              J = IPARTQ(M,N,L)
              IF (I.LT.J) THEN
!
                R2=(X(I)-X(J))**2+(Z(I)-Z(J))**2
!
                IF ((R2-HT2).LE.ETA) THEN
                  K=NLIEN(I)+1
                  NLIEN(I)=K
                  RAB  (I,K)=SQRT(R2)
                  XAB  (I,K)=X (I)-X (J)
                  ZAB  (I,K)=Z (I)-Z (J)
                  VXAB (I,K)=VX(I)-VX(J)
                  VZAB (I,K)=VZ(I)-VZ(J)
                  ILIEN(I,K)=J
                  IF (K.GT.NLIENMAX) THEN
                    IF (LNG.EQ.1) THEN
                      PRINT*,'ERREUR : la particule',
     &                I,' possede trop de liens : NLIEN= ',NLIEN(I)
                      PRINT*,'X(I)=',X(I)
                      PRINT*,'Z(I)=',Z(I)
                      STOP
                    ELSEIF (LNG.EQ.2) THEN
                      PRINT*,'ERROR : particle',
     &                I,' has too many links : NLIEN= ',NLIEN(I)
                      PRINT*,'X(I)=',X(I)
                      PRINT*,'Z(I)=',Z(I)
                      STOP
                    ENDIF
                  ENDIF
                ENDIF
!
              ENDIF
 162        CONTINUE
 161      CONTINUE
 122    CONTINUE
!
        IF (KPER) THEN
!
! Traitement d'un ecoulement periodique
!--------------------------------------
! For a periodic flow
!--------------------
!
! Periodicite a gauche
!.....................
! "Left" periodicity
!...................
!
        IF (IQUAD(I).EQ.1) THEN
!
          DO 165 M=NQUADX-1,NQUADX
            DO 163 N=NMIN,NMAX
              DO 164 L=1,NPARTQ(M,N)
                J = IPARTQ(M,N,L)
                IF (I.LT.J) THEN
!
                  R2=(X(I)-X(J)+XMAX-XMIN)**2+(Z(I)-Z(J))**2
!
                  IF ((R2-HT2).LE.ETA) THEN
                    K=NLIEN(I)+1
                    NLIEN(I)=K
                    RAB  (I,K)=SQRT(R2)
                    XAB  (I,K)=X (I)-X (J)+XMAX-XMIN
                    ZAB  (I,K)=Z (I)-Z (J)
                    VXAB (I,K)=VX(I)-VX(J)
                    VZAB (I,K)=VZ(I)-VZ(J)
                    ILIEN(I,K)=J
                    IF (K.GT.NLIENMAX) THEN
                      IF (LNG.EQ.1) THEN
                        PRINT*,'ERREUR : la particule',
     &                  I,' possede trop de liens : NLIEN= ',NLIEN(I)
                        PRINT*,'X(I)=',X(I)
                        PRINT*,'Z(I)=',Z(I)
                        STOP
                      ELSEIF (LNG.EQ.2) THEN
                        PRINT*,'ERROR : particle',
     &                  I,' has too many links : NLIEN= ',NLIEN(I)
                        PRINT*,'X(I)=',X(I)
                        PRINT*,'Z(I)=',Z(I)
                        STOP
                        ENDIF
                     ENDIF
                  ENDIF
!
                ENDIF
 164          CONTINUE
 163        CONTINUE
 165      CONTINUE
!
        ENDIF
!
! Periodicite a droite
!.....................
! "Right" periodicity
!....................
!
        IF (IQUAD(I).GE.NQUADX-1) THEN
!
          M=1
          DO 166 N=NMIN,NMAX
            DO 167 L=1,NPARTQ(M,N)
              J = IPARTQ(M,N,L)
              IF (I.LT.J) THEN
!
                R2=(X(I)-X(J)-XMAX+XMIN)**2+(Z(I)-Z(J))**2
!
                IF ((R2-HT2).LE.ETA) THEN
                  K=NLIEN(I)+1
                  NLIEN(I)=K
                  RAB  (I,K)=SQRT(R2)
                  XAB  (I,K)=X (I)-X (J)-XMAX+XMIN
                  ZAB  (I,K)=Z (I)-Z (J)
                  VXAB (I,K)=VX(I)-VX(J)
                  VZAB (I,K)=VZ(I)-VZ(J)
                  ILIEN(I,K)=J
                    IF (K.GT.NLIENMAX) THEN
                      IF (LNG.EQ.1) THEN
                        PRINT*,'ERREUR : la particule',
     &                  I,' possede trop de liens : NLIEN= ',NLIEN(I)
                        PRINT*,'X(I)=',X(I)
                        PRINT*,'Z(I)=',Z(I)
                        STOP
                      ELSEIF (LNG.EQ.2) THEN
                        PRINT*,'ERROR : particle',
     &                  I,' has too many links : NLIEN= ',NLIEN(I)
                        PRINT*,'X(I)=',X(I)
                        PRINT*,'Z(I)=',Z(I)
                        STOP
                      ENDIF
                    ENDIF
                ENDIF
!
              ENDIF
 167        CONTINUE
 166      CONTINUE
!
        ENDIF
!
        ENDIF
!
 121  CONTINUE
!
 901  FORMAT(A,I6,A2,I6)
!
      RETURN
      END