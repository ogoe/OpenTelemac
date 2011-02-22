C                        ********************
                          SUBROUTINE TABLIEN
C                        ********************
C
     .   (NLIEN , NLIENMAX , NPART , NPARTQ   , NPARTQMAX, NPMAX ,
     .    NQUADX, NQUADXMAX, NQUADZ, NQUADZMAX, ILIEN    , IPARTQ,
     .    KLIST , KPER     , H     , RAB      , SUPP     , VX    , 
     .    VXAB  , VZ       , VZAB  , X        , XMIN     , XMAX  , 
     .    XAB   , Z        , ZMIN  , ZAB                         )
C
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! H         ! -->  ! SMOOTHING LENGTH                               !
C ! ILIEN     ! <--  ! PARTICLE LINK LIST                             !
C ! IPARTQ    ! <--  ! INDEX OF PARTICLES LOCATED IN A SQUARE         !
C ! KLIST     ! -->  ! LOGICAL FOR LISTING PRINTOUT                   !
C ! KPER      ! -->  ! LOGICAL INDEX FOR PERIODICITY                  !
C ! LNG       ! -->  ! CHOICE INDEX FOR LANGUAGE                      !
C ! NLIEN     ! <--  ! NUMBER OF LINKS RELATIVE TO A PARTICLE         !
C ! NLIENMAX  ! -->  ! MAXIMUM NUMBER OF LINKS RELATIVE TO A PARTICLE !
C ! NPART     ! -->  ! TOTAL PARTICLE NUMBER                          !
C ! NPARTQ    ! <--  ! NUMBER OF PARTICLES IN A SQUARE                !
C ! NPARTQMAX ! -->  ! MAXIMUM NUMBER OF PARTICLES IN A SQUARE        !
C ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
C ! NQUADX,                                                           !
C ! NQUADZ    ! <--  ! NUMBER OF SQUARES ALONG EACH DIRECTION         !
C ! NQUADXMAX,                                                        !
C ! NQUADZMAX ! -->  ! MAXIMUM NUMBER OF SQUARES ALONG EACH DIRECTION !
C ! RAB       ! <--  ! INTERPARTICLE DISTANCE                         !
C ! SUPP      ! -->  ! KERNEL SUPPORT                                 !
C ! VX, VZ    ! -->  ! VELOCITY COMPONENTS                            ! 
C ! VXAB, VZAB! <--  ! VELOCITY DIFFERENCE BETWEEN PARTICLES          !
C ! X, Z      ! -->  ! PARTICLE POSITION                              !
C ! XMIN, XMAX! -->  ! MINIMUM AND MAXIMUM X OF THE DOMAIN            !
C ! XAB, ZAB  ! <--  ! COORDINATE DIFFERENCES BETWEEN PARTICLES       !
C ! ZMIN, ZMAX! -->  ! MINIMUM AND MAXIMUM Z OF THE DOMAIN            !
C !___________!______!________________________________________________!
C
C MODE : -->(NON MODIFIED DATA), <--(RESULT), <-->(MODIFIED DATA)
C----------------------------------------------------------------------
C
C SPARTACUS2D V5P9
C D. Violeau           & R. Issa
C +33(0)1-30-87-78-31 // +33(0)1-30-87-84-28 
C LNHE - 2008
C
C FONCTION : determine les liens entre particules, etablit les valeurs
C            des champs XAB, ZAB, RAB, VXAB et VZAB
C FUNCTION : defines the links between particles and computed the values
C            of XAB, ZAB, RAB, VXAB and VZAB fields
C
C PROGRAMMES APPELANT : SPARTACUS2D
C CALLED BY 
C
C PROGRAMMES APPELES  : -
C CALLED PROGRAMS    
C
C----------------------------------------------------------------------
C
C Variables
C==========
C
      IMPLICIT NONE
C
      INTEGER NPMAX    , NPART    , NLIENMAX
      INTEGER NQUADXMAX, NQUADZMAX, NPARTQMAX
      INTEGER NQUADX   , NQUADZ   
      INTEGER I        , J        , L
      INTEGER N        , M        , K
      INTEGER MMIN     , MMAX     , LNG
      INTEGER NMIN     , NMAX     , LU
      COMMON/INFO/LNG,LU
C
      LOGICAL KLIST, KPER
C
      DOUBLE PRECISION XMIN, ZMIN, H , SUPP, ETA
      DOUBLE PRECISION HT2 , XMAX, R2
C
      INTEGER ILIEN (NPMAX,NLIENMAX)
      INTEGER NPARTQ(NQUADXMAX,NQUADZMAX)
      INTEGER IPARTQ(NQUADXMAX,NQUADZMAX,NPARTQMAX)
C
      INTEGER NLIEN(NPMAX)
      INTEGER IQUAD(NPMAX)
      INTEGER JQUAD(NPMAX)
C
      DOUBLE PRECISION X (NPMAX), Z (NPMAX)
      DOUBLE PRECISION VX(NPMAX), VZ(NPMAX)
C
      DOUBLE PRECISION VXAB(NPMAX,NLIENMAX), VZAB(NPMAX,NLIENMAX)
      DOUBLE PRECISION XAB (NPMAX,NLIENMAX), ZAB (NPMAX,NLIENMAX)
      DOUBLE PRECISION RAB (NPMAX,NLIENMAX)
C
C Initialisations
C================
C Initializations
C================
C
      ETA   = 1.0D-16
      HT2= (SUPP*H)**2
C
C Construction du quadrillage
C============================
C Coarse grid construction
C========================= 
C
C Nombre de carres et, pour chaque particule, indice de carre 
C------------------------------------------------------------
C Number of sqaures and sqaure indexs for each particle
C------------------------------------------------------
C
      NQUADX=0
      NQUADZ=0
      DO 150 I=1,NPART
        IQUAD(I)=INT((X(I)-XMIN)/(SUPP*H))+1
        IF (IQUAD(I).GT.NQUADX) NQUADX=IQUAD(I)
        JQUAD(I)=INT((Z(I)-ZMIN)/(SUPP*H))+1
        IF (JQUAD(I).GT.NQUADZ) NQUADZ=JQUAD(I)
C
        IF (NQUADX.GT.NQUADXMAX.OR.NQUADZ.GT.NQUADZMAX) THEN
          IF (LNG.EQ.1) THEN
            PRINT*,'ERREUR : Nombre de carres maximum insuffisant : ',
     .      'NQUADX =',NQUADX,', NQUADZ =',NQUADZ
            STOP
          ELSEIF (LNG.EQ.2) THEN
            PRINT*,'ERROR: Maximum number of squares not sufficient :',
     .      'NQUADX =',NQUADX,', NQUADZ =',NQUADZ
            STOP
          ENDIF
        ENDIF
C
 150  CONTINUE     
C
      IF (KLIST) THEN
        IF (LNG.EQ.1) THEN
          PRINT901,' Nombre de carres : ',NQUADX,' * ',NQUADZ  
        ELSEIF (LNG.EQ.2) THEN
          PRINT901,' Number of squares : ',NQUADX,' * ',NQUADZ 
        ENDIF
      ENDIF
C
C Pour chaque carre, table des particules et nombre de particules
C----------------------------------------------------------------
C For each square, particle link list and particle number
C--------------------------------------------------------    
C
      DO 152 I=1,NQUADX
        DO 153 J=1,NQUADZ
          NPARTQ(I,J)=0
 153    CONTINUE
 152  CONTINUE
C
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
C
C Construction du tableau de liens
C---------------------------------
C Link list construction
C-----------------------
C
      DO 154 I=1,NPART
        NLIEN(I)=0
 154  CONTINUE
C
      DO 121 I=1,NPART
        MMIN=IQUAD(I)-1
        MMAX=IQUAD(I)+1
        NMIN=JQUAD(I)-1
        NMAX=JQUAD(I)+1
        IF (MMIN.EQ.0       ) MMIN=1
        IF (MMAX.EQ.NQUADX+1) MMAX=NQUADX
        IF (NMIN.EQ.0)        NMIN=1
        IF (NMAX.EQ.NQUADZ+1) NMAX=NQUADZ
C
        DO 122 M=MMIN,MMAX
          DO 161 N=NMIN,NMAX
            DO 162 L=1,NPARTQ(M,N)
              J = IPARTQ(M,N,L)
              IF (I.LT.J) THEN
C
                R2=(X(I)-X(J))**2+(Z(I)-Z(J))**2
C
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
     .                I,' possede trop de liens : NLIEN= ',NLIEN(I)
                      PRINT*,'X(I)=',X(I)
                      PRINT*,'Z(I)=',Z(I)
                      STOP
                    ELSEIF (LNG.EQ.2) THEN
                      PRINT*,'ERROR : particle',
     .                I,' has too many links : NLIEN= ',NLIEN(I)
                      PRINT*,'X(I)=',X(I)
                      PRINT*,'Z(I)=',Z(I)
                      STOP
                    ENDIF
                  ENDIF
                ENDIF
C
              ENDIF
 162        CONTINUE
 161      CONTINUE
 122    CONTINUE
C
        IF (KPER) THEN
C
C Traitement d'un ecoulement periodique
C--------------------------------------
C For a periodic flow
C--------------------
C
C Periodicite a gauche
C.....................
C "Left" periodicity
C...................
C
        IF (IQUAD(I).EQ.1) THEN
C
          DO 165 M=NQUADX-1,NQUADX
            DO 163 N=NMIN,NMAX
              DO 164 L=1,NPARTQ(M,N)
                J = IPARTQ(M,N,L)
                IF (I.LT.J) THEN
C
                  R2=(X(I)-X(J)+XMAX-XMIN)**2+(Z(I)-Z(J))**2
C
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
     .                  I,' possede trop de liens : NLIEN= ',NLIEN(I)
                        PRINT*,'X(I)=',X(I)
                        PRINT*,'Z(I)=',Z(I)
                        STOP
                      ELSEIF (LNG.EQ.2) THEN
                        PRINT*,'ERROR : particle',
     .                  I,' has too many links : NLIEN= ',NLIEN(I)
                        PRINT*,'X(I)=',X(I)
                        PRINT*,'Z(I)=',Z(I)
                        STOP
                        ENDIF
                     ENDIF
                  ENDIF
C
                ENDIF
 164          CONTINUE
 163        CONTINUE
 165      CONTINUE
C
        ENDIF
C
C Periodicite a droite
C.....................
C "Right" periodicity
C....................
C
        IF (IQUAD(I).GE.NQUADX-1) THEN
C
          M=1
          DO 166 N=NMIN,NMAX
            DO 167 L=1,NPARTQ(M,N)
              J = IPARTQ(M,N,L)
              IF (I.LT.J) THEN
C
                R2=(X(I)-X(J)-XMAX+XMIN)**2+(Z(I)-Z(J))**2
C     
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
     .                  I,' possede trop de liens : NLIEN= ',NLIEN(I)
                        PRINT*,'X(I)=',X(I)
                        PRINT*,'Z(I)=',Z(I)
                        STOP
                      ELSEIF (LNG.EQ.2) THEN
                        PRINT*,'ERROR : particle',
     .                  I,' has too many links : NLIEN= ',NLIEN(I)
                        PRINT*,'X(I)=',X(I)
                        PRINT*,'Z(I)=',Z(I)  
                        STOP
                      ENDIF
                    ENDIF
                ENDIF
C
              ENDIF
 167        CONTINUE
 166      CONTINUE
C
        ENDIF
C
        ENDIF
C
 121  CONTINUE
C
 901  FORMAT(A,I6,A2,I6)
C
      RETURN
      END
