C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES THE ARRAYS WITH PHYSICAL PARAMETERS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>      <tr>
!>      <td><center> 5.0                                       </center>
!> </td><td> 25/08/2000
!> </td><td>
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 1.0                                       </center>
!> </td><td> 01/02/95
!> </td><td> F.MARCOS (LNH) 30 87 72 66
!> </td><td>
!> </td></tr>
!>  </table>

C
C#######################################################################
C
                        SUBROUTINE CONDIW
     &( AT, LT , DPI, TC1, TC2, NPC , TV1, TV2, NPV, TM1, TM2 , NPM ,
     &  NVHMA  , NVCOU )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ALPHIL         |-->| CONSTANTE DE PHILLIPS (ALPHA)
C| AT             |<--| TEMPS DU CALCUL
C| BINCOU         |-->| BINAIRE DU FICHIER DES COURANTS
C| BINMAR         |-->| BINAIRE DU FICHIER DES HAUTEURS DE LA MAREE
C| BINVEN         |-->| BINAIRE DU FICHIER DES VENTS
C| COUSTA         |-->| LOGIQUE INDIQUANT UN COURANT STATIONNAIRE
C| DDC            |-->| DATE DE DEBUT DU CALCUL
C| DONTEL         |-->| LOGIQUE INDIQUANT SI ON RECUPERE UNE DON.TEL.
C| DPI            |---| 
C| E2FMIN         |-->| VALEUR MINIMALE D'ENERGIE
C| F             |<--| DENSITE SPECTRALE D'ENERGIE
C| F1             |-->| FREQUENCE MINIMALE
C| FETCH          |-->| FETCH MOYEN
C| FPIC           |-->| FREQUENCE DE PIC JONSWAP
C| FREMAX         |-->| VALEUR MAXIMUM DE LA FREQUENCE DE PIC
C| FREQ           |<--| FREQUENCES DISCRETISEES
C| GAMMA          |-->| FACTEUR DE FORME DE PIC JONSWAP
C| GRAVIT         |-->| ACCELERATION DE LA PESANTEUR
C| HM0            |-->| HAUTEUR SIGNIFICATIVE JONSWAP
C| IDTEL          |-->| RANG DE LA DONNEE TELEMAC A RECUPERER
C| INDIC          |-->| FORMAT DU FICHIER DES COURANTS
C| INDIV          |-->| FORMAT DU FICHIER DES HAUTEURS
C| INISPE         |-->| INDICATEUR D'INITIALISATION DU SPECTRE
C| LT             |---| 
C| MAREE          |-->| LOGIQUE INDIQUANT SI ON CONSIDERE LA MAREE
C| NBOR           |-->| NUMERO GLOBAUX DES POINTS DE BORD
C| NF             |-->| NOMBRE DE FREQUENCES
C| NPC            |-->| NOMBRE DE POINTS DU FICHIER DES COURANTS
C| NPLAN          |-->| NOMBRE DE DIRECTIONS DE PROPAGATION
C| NPM            |-->| NOMBRE DE POINTS DU FICHIER DES HAUTEURS
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
C| NPOIN3         |-->| NOMBRE DE POINTS DU MAILLAGE 3D
C| NPRIV          |---| NPOIN3*NPRIV
C| NPTFR          |-->| NOMBRE DE POINTS DE BORD
C| NPTT           |-->| NUMERO DU PAS DE TEMPS DU FICHIER TELEMAC
C| NPV            |-->| NOMBRE DE POINTS DU FICHIER DES VENTS
C| NVCOU          |---| 
C| NVHMA          |---| 
C| PRIVE          |-->| TABLEAU POUR L'UTILISATEUR DE DIMENSION
C| RAISF          |-->| RAISON FREQUENTIELLE
C| SIGMAA         |-->| VALEUR DE SIGMA JONSWAP POUR F
C| SIGMAB         |-->| VALEUR DE SIGMA JONSWAP POUR F > FP
C| SPRED1         |-->| ETALEMENT DIRECTIONNEL 1 POUR FRA
C| SPRED2         |-->| ETALEMENT DIRECTIONNEL 2 POUR FRA
C| TC1            |<--| TEMPS CORRESPONDANT AU COURANT 1
C| TC2            |<--| TEMPS CORRESPONDANT AU COURANT 2
C| TETA           |<--| DIRECTIONS DE PROPAGATION
C| TETA1          |-->| DIRECTION PRINCIPALE 1 POUR FRA
C| TETA2          |-->| DIRECTION PRINCIPALE 2 POUR FRA
C| TM1            |<--| TEMPS CORRESP. A LA HAUTEUR DE LA MAREE 1
C| TM2            |<--| TEMPS CORRESP. A LA HAUTEUR DE LA MAREE 2
C| TRA31          |<->| TABLEAUX DE TRAVAIL REELS
C| TV1            |<--| TEMPS CORRESPONDANT AU VENT 1
C| TV2            |<--| TEMPS CORRESPONDANT AU VENT 2
C| UC,VC          |<--| COMPOSANTES DU CHAMP DE COURANT
C| UC1,VC1        |<--| COMPOSANTES DU CHAMP DE COURANT INFERIEUR
C| UC2,VC2        |<--| COMPOSANTES DU CHAMP DE COURANT SUPERIEUR
C| UV,VV          |<--| COMPOSANTES DU CHAMP DE VENT INITIAL
C| UV1,VV1        |<--| COMPOSANTES DU CHAMP DE VENT INFERIEUR
C| UV2,VV2        |<--| COMPOSANTES DU CHAMP DE VENT SUPERIEUR
C| VENT           |-->| LOGIQUE INDIQUANT SI ON CONSIDERE UN VENT
C| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE 2D
C| XLAMDA         |-->| FACTEUR DE PONDERATION POUR LA FRA
C| XRELC,YRELC    |-->| COORDONNEES DES POINTS DES COURANTS RELEVES
C| XRELM,YRELM    |-->| COORDONNEES DES POINTS DES HAUTEURS RELEVEES
C| XRELV,YRELV    |-->| COORDONNEES DES POINTS DES VENTS RELEVES
C| ZM             |<--| HAUTEUR DE LA MAREE INITIALE
C| ZM1,ZM2        |<--| HAUTEURS DE LA MAREE INFERIEURE ET SUPERIEURE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TOMAWAC
      USE INTERFACE_TOMAWAC, EX_CONDIW=> CONDIW
      USE BIEF
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
      DOUBLE PRECISION AT , DPI, TC1, TC2, TV1, TV2, TM1, TM2
      INTEGER          NPC,NPV,NPM,LT,NP0,NP1,NP2,NP3,NP4,NP5
      INTEGER          IPLAN, IFREQ , NVHMA, NVCOU, IBID
C
      CHARACTER*7   CHDON
C
C***********************************************************************
C
      AT  = 0.D0
      NP0 = NPOIN3_G+1
      NP1 = 2*NPOIN3_G
      NP2 = NP1+1
      NP3 = 3*NPOIN3_G
      NP4 = NP3+1
      NP5 = 4*NPOIN3_G
C
C-----------------------------------------------------------------------
C
C   INITIALISES THE TIDAL CURRENT AND WATER LEVEL
C
      IF (MAREE) THEN
        IF(LNG.EQ.1) THEN
          CHDON='COURANT'
        ELSE
          CHDON='CURRENT'
        ENDIF
C       READS IN THE TIDAL CURRRENT
        IF((WAC_FILES(WACCOF)%NAME(1:1).EQ.' ').AND.
     &                (WAC_FILES(WACCOB)%NAME(1:1).EQ.' ')) THEN
          WRITE(LU,*) 'FICHIER ANALYTIQUE POUR COURANT'
          CALL ANAMAR
     &    ( SUC%R  , SVC%R   , STRA31%R, SZM1%R   ,
     &      SZM2%R , SDZHDT%R, MESH%X%R, MESH%Y%R ,
     &      NPOIN2    , AT  , DDC  , LT         )
          WRITE(LU,*) ' '
          IF (LNG.EQ.1) THEN
            WRITE(LU,*)'PRISE EN COMPTE D''UN COURANT DE MAREE'
            WRITE(LU,*)
     &      'MAIS PAS DE FICHIER DES COURANTS (OU DONNEES TELEMAC)'
            WRITE(LU,*)
     &      '==> LE COURANT DE MAREE EST INITIALISE DANS ANAMAR'
          ELSE
            WRITE(LU,*)'USE OF TIDAL CURRENT VELOCITIES'
            WRITE(LU,*)'BUT NO CURRENT FILE (NEITHER TELEMAC DATA FILE)'
            WRITE(LU,*)
     &      '==> INITIALISATION OF TIDAL CURRENT VELOCITIES IN ANAMAR'
          ENDIF
        ELSE
          IF (WAC_FILES(WACCOF)%NAME(1:1).NE.' ') THEN
C           READS IN THE TIDAL CURRENTS FROM FORMATTED DATA FILE
            CALL LECDOI
     &      ( SUC%R  , SVC%R   , MESH%X%R, MESH%Y%R ,
     &        NPOIN2, WAC_FILES(WACCOF)%LU , BINCOU, NBOR , NPTFR,
     &        AT , DDC , TC1, TC2, NPC   ,
     &        SXRELC%R, SYRELC%R,
     &        TRA01(1:NPOIN3_G) , TRA01(NP0:NP1) , TRA01(NP2:NP3),
     &        SUC1%R , SVC1%R, SUC2%R, SVC2%R,
     &        INDIC , NPOIN3_G, CHDON, NVCOU)
          ELSE
C           READS IN THE TIDAL CURRENT FROM BINARY FILE
            CALL LECDOI
     &      ( SUC%R  , SVC%R   , MESH%X%R, MESH%Y%R ,
     &        NPOIN2, WAC_FILES(WACCOB)%LU , BINCOU, NBOR , NPTFR,
     &        AT , DDC , TC1, TC2, NPC   ,
     &        SXRELC%R, SYRELC%R,
     &        TRA01(1:NPOIN3_G) , TRA01(NP0:NP1) , TRA01(NP2:NP3),
     &        SUC1%R , SVC1%R, SUC2%R, SVC2%R,
     &        INDIC , NPOIN3_G, CHDON, NVCOU )
          ENDIF
        ENDIF
C
C       READS IN THE TIDAL WATER LEVEL
C
        IF((WAC_FILES(WACMAF)%NAME(1:1).EQ.' ').AND.
     &                       (WAC_FILES(WACMAB)%NAME(1:1).EQ.' ')) THEN
          IF((WAC_FILES(WACCOF)%NAME.NE.' ').OR.
     &       (WAC_FILES(WACCOB)%NAME.NE.' ')) THEN
            CALL ANAMAR
     &    ( SUC%R  , SVC%R   , STRA31%R, SZM1%R   ,
     &      SZM2%R , SDZHDT%R, MESH%X%R, MESH%Y%R ,
     &      NPOIN2,   AT  , DDC , LT    )
          ENDIF
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)
     &      '==> LA HAUTEUR DE LA MAREE EST INITIALISEE DANS ANAMAR'
          ELSE
            WRITE(LU,*)
     &      '==> INITIALISATION OF TIDAL WATER LEVEL IN ANAMAR'
          ENDIF
        ELSE
          IF (WAC_FILES(WACMAF)%NAME(1:1).NE.' ') THEN
            WRITE(LU,*) 'LECTURE COURANT DANS FICHIER LECHAM MAF'
            CALL LECHAM
     &      ( STRA31%R, SDZHDT%R, MESH%X%R, MESH%Y%R ,
     &        NPOIN2, WAC_FILES(WACMAF)%LU, BINMAR, NBOR  ,
     &        NPTFR, AT   , DDC , TM1  , TM2   , NPM  ,
     &        SXRELM%R , SYRELM%R ,
     &        TRA01(1:NPOIN3_G)   , SZM1%R, SZM2%R,
     &        INDIM, NPOIN3_G, IDHMA ,
     &        NVHMA )
           ELSE
            CALL LECHAM
     &      ( STRA31%R, SDZHDT%R, MESH%X%R, MESH%Y%R ,
     &        NPOIN2, WAC_FILES(WACMAB)%LU , BINMAR, NBOR  ,
     &        NPTFR, AT   , DDC , TM1  , TM2   , NPM  ,
     &        SXRELM%R , SYRELM%R ,
     &        TRA01(1:NPOIN3_G)   , SZM1%R, SZM2%R,
     &        INDIM, NPOIN3_G, IDHMA ,
     &        NVHMA )
           ENDIF
        ENDIF
        CALL OV('X=X+Y   ',SDEPTH%R,STRA31%R,ST1%R,0.D0,NPOIN2)
      ENDIF
C
C   INITIALISES THE CURRENT
C   AND READS IN A TELEMAC VARIABLE (OPTIONAL)
C
      IF ((COUSTA).OR.(DONTEL)) THEN
        IF ((WAC_FILES(WACCOF)%NAME(1:1).EQ.' ').AND.
     &                 (WAC_FILES(WACCOB)%NAME(1:1).EQ.' ')) THEN
          IF(COUSTA) THEN
             CALL ANACOS
     &      ( SUC%R, SVC%R, MESH%X%R, MESH%Y%R, NPOIN2)
             WRITE(LU,*)' '
             IF (LNG.EQ.1) THEN
               WRITE(LU,*)'PRISE EN COMPTE D''UN COURANT'
               WRITE(LU,*)
     &         'MAIS PAS DE FICHIER DES COURANTS (OU DONNEES TELEMAC)'
               WRITE(LU,*)'==> LE COURANT EST INITIALISE DANS ANACOS'
             ELSE
               WRITE(LU,*)'USE OF CURRENT VELOCITIES'
               WRITE(LU,*)
     &         'BUT NO CURRENT FILE (NEITHER TELEMAC DATA FILE)'
               WRITE(LU,*)
     &         '==> INITIALISATION OF CURRENT VELOCITIES IN ANACOS'
             ENDIF
          ELSE
             IF (LNG.EQ.1) THEN
               WRITE(LU,*)'RELECTURE D''UNE VARIABLE TELEMAC IMPOSSIBLE'
             ELSE
               WRITE(LU,*)' READING OF A TELEMAC DATA IMPOSSIBLE '
             ENDIF
             CALL PLANTE(0)
          ENDIF
        ELSE
          IF(LNG.EQ.1) THEN
            CHDON='COURANT'
          ELSE
            CHDON='CURRENT'
          ENDIF
          IF (WAC_FILES(WACCOF)%NAME(1:1).NE.' ') THEN
             CALL LECDON
     &      ( SUC%R  , SVC%R   , MESH%X%R, MESH%Y%R ,
     &        NPOIN2 , WAC_FILES(WACCOF)%LU   , BINCOU ,
     &        NBOR , NPTFR, SXRELC%R, SYRELC%R, TRA01(1:NPOIN3_G),
     &        TRA01(NP0:NP1),TRA01(NP2:NP3),TRA01(NP4:NP5),STRA31%R,
     &        IDTEL, NPTT , DONTEL, COUSTA, INDIC  , NPOIN3_G , CHDON)
          ELSE
             CALL LECDON
     &      ( SUC%R  , SVC%R   , MESH%X%R, MESH%Y%R ,
     &        NPOIN2 , WAC_FILES(WACCOB)%LU   , BINCOU ,
     &        NBOR , NPTFR, SXRELC%R, SYRELC%R, TRA01(1:NPOIN3_G),
     &        TRA01(NP0:NP1),TRA01(NP2:NP3),TRA01(NP4:NP5),STRA31%R,
     &        IDTEL, NPTT , DONTEL, COUSTA, INDIC  , NPOIN3_G , CHDON)
          ENDIF
        ENDIF
        DZHDT = 0.D0
      ENDIF
C
C-----------------------------------------------------------------------
C
C   INITIALISES THE WIND
C
      IF (VENT) THEN
        IF(LNG.EQ.1) THEN
          CHDON='VENT   '
        ELSE
          CHDON='WIND   '
        ENDIF
C
        IF ((WAC_FILES(WACVEF)%NAME(1:1).EQ.' ').AND.
     &                 (WAC_FILES(WACVEB)%NAME(1:1).EQ.' ')) THEN
          CALL ANAVEN
     &   ( SUV%R, SVV%R, MESH%X%R, MESH%Y%R ,
     &     NPOIN2,AT,DDC,VX_CTE,VY_CTE)
          WRITE(LU,*)' '
          IF (LNG.EQ.1) THEN
            WRITE(LU,*)'PRISE EN COMPTE D''UN VENT'
            WRITE(LU,*)'MAIS PAS DE FICHIER DE VENT'
            WRITE(LU,*)'==> LE VENT EST INITIALISE DANS ANAVEN'
          ELSE
            WRITE(LU,*)'USE OF WIND VELOCITIES'
            WRITE(LU,*)'BUT NO WIND FILE '
            WRITE(LU,*)'==> INITIALISATION OF WIND VELOCITIES IN ANAVEN'
          ENDIF
        ELSE
          IF (WAC_FILES(WACVEF)%NAME(1:1).NE.' ') THEN
           IF (VENSTA) THEN
             CALL LECDON
     &      ( SUV%R  , SVV%R   , MESH%X%R, MESH%Y%R ,
     &        NPOIN2 , WAC_FILES(WACVEF)%LU   , BINVEN ,
     &        NBOR , NPTFR, SXRELV%R, SYRELV%R, TRA01(1:NPOIN3_G),
     &        TRA01(NP0:NP1),TRA01(NP2:NP3),TRA01(NP4:NP5),STRA31%R,
     &        IDTEL, NPTT , .FALSE., VENSTA, INDIV  , NPOIN3_G ,CHDON)
           ELSE
            CALL LECDOI
     &      ( SUV%R , SVV%R , MESH%X%R , MESH%Y%R ,
     &        NPOIN2, WAC_FILES(WACVEF)%LU , BINVEN, NBOR , NPTFR,
     &        AT , DDC , TV1, TV2, NPV   , SXRELV%R, SYRELV%R ,
     &        TRA01(1:NPOIN3_G) , TRA01(NP0:NP1) , TRA01(NP2:NP3),
     &        SUV1%R , SVV1%R, SUV2%R, SVV2%R,
     &        INDIV , NPOIN3_G, CHDON, IBID )
           ENDIF
          ELSE
           IF (VENSTA) THEN
             CALL LECDON
     &      ( SUV%R  , SVV%R   , MESH%X%R, MESH%Y%R ,
     &        NPOIN2 , WAC_FILES(WACVEB)%LU   , BINVEN ,
     &        NBOR , NPTFR, SXRELV%R, SYRELV%R, TRA01(1:NPOIN3_G),
     &        TRA01(NP0:NP1),TRA01(NP2:NP3),TRA01(NP4:NP5),STRA31%R,
     &        IDTEL, NPTT , .FALSE., VENSTA, INDIV  , NPOIN3_G , CHDON)
           ELSE
            CALL LECDOI
     &      ( SUV%R , SVV%R , MESH%X%R , MESH%Y%R ,
     &        NPOIN2, WAC_FILES(WACVEB)%LU, BINVEN, NBOR , NPTFR,
     &        AT , DDC , TV1, TV2, NPV   , SXRELV%R, SYRELV%R ,
     &        TRA01(1:NPOIN3_G) , TRA01(NP0:NP1) , TRA01(NP2:NP3),
     &        SUV1%R , SVV1%R, SUV2%R, SVV2%R,
     &        INDIV , NPOIN3_G, CHDON, IBID )
           ENDIF
          ENDIF
         ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
C   INITIALISES TETA
C     BY DEFAULT THE DIRECTIONS OF PROPAGATION ARE EVENLY DISTRIBUTED
C
      DO IPLAN = 1,NPLAN+1
         TETA(IPLAN) = (IPLAN-1)*DPI/NPLAN
      ENDDO
C
C-----------------------------------------------------------------------
C
C
C     INITIALISES FREQ AND DFREQ, THE FREQUENCIES OF PROPAGATION
C     ARE DISTRIBUTED USING AN EXPONENTIAL LAW
C
      DO IFREQ = 1,NF
        FREQ(IFREQ) = F1*RAISF**(IFREQ-1)
      ENDDO
C
C-----------------------------------------------------------------------
C
C     INITIALISES F
C
      CALL SPEINI
     &  ( SF%R  , TRA01(1:NF)   , TRA01(NP0:NP1),
     &    SUV%R , SVV%R      , SFR%R  , STETA%R  , GRAVIT,
     &    FREMAX   , FETCH , SIGMAA, SIGMAB , GAMMA  , FPIC  , HM0   ,
     &    ALPHIL   , TETA1 , SPRED1, TETA2  , SPRED2 , XLAMDA, NPOIN2,
     &    NPLAN    , NF    , INISPE, E2FMIN , DEPTH  , FRABI  )
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
