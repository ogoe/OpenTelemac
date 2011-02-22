C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       WRITES OUT WAVE, WIND, CURRENT, BATHYMETRY, ...
!>                VARIABLES AT EACH NODE OF THE MESH.
!>                VARIES SPATIALLY IN 2D (BINARY SELAFIN FORMAT).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>DECLARATIONS_TOMAWAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DEUPI, LT, NP1, XF1
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TOMAWAC :<br>
!> @link DECLARATIONS_TOMAWAC::FREQ FREQ@endlink, 
!> @link DECLARATIONS_TOMAWAC::GRAVIT GRAVIT@endlink, 
!> @link DECLARATIONS_TOMAWAC::NF NF@endlink, 
!> @link DECLARATIONS_TOMAWAC::NPLAN NPLAN@endlink, 
!> @link DECLARATIONS_TOMAWAC::NPOIN2 NPOIN2@endlink, 
!> @link DECLARATIONS_TOMAWAC::NPOIN3 NPOIN3@endlink, 
!> @link DECLARATIONS_TOMAWAC::PROINF PROINF@endlink, 
!> @link DECLARATIONS_TOMAWAC::ROEAU ROEAU@endlink, 
!> @link DECLARATIONS_TOMAWAC::SCG SCG@endlink, 
!> @link DECLARATIONS_TOMAWAC::SCOSTE SCOSTE@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDEPTH SDEPTH@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDFR SDFR@endlink, 
!> @link DECLARATIONS_TOMAWAC::SFR SFR@endlink, 
!> @link DECLARATIONS_TOMAWAC::SORLEO SORLEO@endlink, 
!> @link DECLARATIONS_TOMAWAC::SSINTE SSINTE@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA31 STRA31@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA32 STRA32@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA33 STRA33@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA34 STRA34@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA35 STRA35@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA36 STRA36@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA37 STRA37@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA38 STRA38@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA39 STRA39@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA51 STRA51@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA52 STRA52@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA53 STRA53@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA54 STRA54@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA55 STRA55@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA56 STRA56@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA57 STRA57@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA59 STRA59@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA60 STRA60@endlink, 
!> @link DECLARATIONS_TOMAWAC::SXK SXK@endlink, 
!> @link DECLARATIONS_TOMAWAC::TAILF TAILF@endlink, 
!> @link DECLARATIONS_TOMAWAC::TRA02 TRA02@endlink, 
!> @link DECLARATIONS_TOMAWAC::TRA32 TRA32@endlink, 
!> @link DECLARATIONS_TOMAWAC::TRA33 TRA33@endlink, 
!> @link DECLARATIONS_TOMAWAC::TRA34 TRA34@endlink, 
!> @link DECLARATIONS_TOMAWAC::TRA35 TRA35@endlink, 
!> @link DECLARATIONS_TOMAWAC::TRA36 TRA36@endlink, 
!> @link DECLARATIONS_TOMAWAC::TRA37 TRA37@endlink, 
!> @link DECLARATIONS_TOMAWAC::TRA38 TRA38@endlink, 
!> @link DECLARATIONS_TOMAWAC::TRA42 TRA42@endlink, 
!> @link DECLARATIONS_TOMAWAC::TRA56 TRA56@endlink, 
!> @link DECLARATIONS_TOMAWAC::TRA57 TRA57@endlink, 
!> @link DECLARATIONS_TOMAWAC::TRA58 TRA58@endlink, 
!> @link DECLARATIONS_TOMAWAC::TRA61 TRA61@endlink, 
!> @link DECLARATIONS_TOMAWAC::TRA62 TRA62@endlink, 
!> @link DECLARATIONS_TOMAWAC::TRA63 TRA63@endlink, 
!> @link DECLARATIONS_TOMAWAC::TRA64 TRA64@endlink, 
!> @link DECLARATIONS_TOMAWAC::TRA65 TRA65@endlink, 
!> @link DECLARATIONS_TOMAWAC::TRA66 TRA66@endlink, 
!> @link DECLARATIONS_TOMAWAC::TRIGO TRIGO@endlink, 
!> @link DECLARATIONS_TOMAWAC::UV UV@endlink, 
!> @link DECLARATIONS_TOMAWAC::VENT VENT@endlink, 
!> @link DECLARATIONS_TOMAWAC::VV VV@endlink
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> FMAX, FMIN, I1, I2, IP, PIS2, RADDEG, U10
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> FPREAD(), FREM01(), FREM02(), FREMOY(), FREPIC(), RADIAT(), SPREAD(), TETMOY(), TOTNRJ(), VITFON(), WPOWER()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>WAC()

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
!>      <td><center> 1.2                                       </center>
!> </td><td> 04/07/96
!> </td><td> M. BENOIT
!> </td><td> MODIFIED
!> </td></tr>
!>      <tr>
!>      <td><center> 1.0                                       </center>
!> </td><td> 01/02/95
!> </td><td> F. MARCOS
!> </td><td> CREATED
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT
!></td><td>--></td><td>DATE COURANTE DU CALCUL
!>    </td></tr>
!>          <tr><td>BINR2D
!></td><td>--></td><td>TYPE DE BINAIRE DU FICHIER DES RESULTATS 2D
!>    </td></tr>
!>          <tr><td>CG(
!></td><td>--></td><td>TABLEAU DES VITESSES DE GROUPE
!>    </td></tr>
!>          <tr><td>COSTET(
!></td><td>--></td><td>VECTEUR DES COSINUS DES DIRECTIONS
!>    </td></tr>
!>          <tr><td>COURAN
!></td><td>--></td><td>INDICATEUR DE PRESENCE D'UN COURAN
!>    </td></tr>
!>          <tr><td>DEBRES
!></td><td>--></td><td>INDICATEUR DE PREMIERE DATE A SAUVER
!>    </td></tr>
!>          <tr><td>DEPTH(
!></td><td>--></td><td>TABLEAU DES PROFONDEURS (METRES)
!>    </td></tr>
!>          <tr><td>DEUPI
!></td><td>--></td><td>2.PI
!>    </td></tr>
!>          <tr><td>DFREQ(
!></td><td>--></td><td>TABLEAU DES PAS DE FREQUENCES
!>    </td></tr>
!>          <tr><td>F(
!></td><td>--></td><td>SPECTRE DIRECTIONNEL DE VARIANCE
!>    </td></tr>
!>          <tr><td>FREQ(
!></td><td>--></td><td>TABLEAU DES FREQUENCES DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>GRAVIT
!></td><td>--></td><td>ACCELERATION DE LA PESANTEUR
!>    </td></tr>
!>          <tr><td>IELM2
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>IKLE2(
!></td><td>--></td><td>CORRESPONDANCE NUMEROTATION LOCALE-GLOBALE
!>    </td></tr>
!>          <tr><td>ITR01
!></td><td><-></td><td>TABLEAU DE TRAVAIL DE DIMENSION 3*NELEM2
!>    </td></tr>
!>          <tr><td>LT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH(
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>NBOR(
!></td><td>--></td><td>NUMEROTATION DES POINTS FRONTIERE
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE SPATIAL 2D
!>    </td></tr>
!>          <tr><td>NF
!></td><td>--></td><td>NOMBRE DE FREQUENCES DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>NP1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE DIRECTIONS DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE SPATIAL 2D
!>    </td></tr>
!>          <tr><td>NPRIV
!></td><td>--></td><td>DIMENSION DU TABLEAU UTILISATEUR
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE  POINTS FRONTIERE
!>    </td></tr>
!>          <tr><td>NR2D
!></td><td>--></td><td>NUM. DU FICHIER DE SORTIE DES RESULTATS 2D
!>    </td></tr>
!>          <tr><td>PRIVE(
!></td><td>--></td><td>TABLEAU UTILISATEUR
!>    </td></tr>
!>          <tr><td>PROINF
!></td><td>--></td><td>INDICATEUR DE PROFONDEUR INFINIE
!>    </td></tr>
!>          <tr><td>ROEAU
!></td><td>--></td><td>MASSE VOLUMIQUE DE L'EAU
!>    </td></tr>
!>          <tr><td>SINTET(
!></td><td>--></td><td>VECTEUR DES SINUS   DES DIRECTIONS
!>    </td></tr>
!>          <tr><td>SORG2D
!></td><td>--></td><td>INDICATEUR DE SORTIE DES VARIABLES 2D
!>    </td></tr>
!>          <tr><td>T1(
!></td><td>---</td><td>TABLEAU DE TRAVAIL STRUCTURE
!>    </td></tr>
!>          <tr><td>T2(
!></td><td>---</td><td>TABLEAU DE TRAVAIL STRUCTURE
!>    </td></tr>
!>          <tr><td>T3(
!></td><td>---</td><td>TABLEAU DE TRAVAIL STRUCTURE
!>    </td></tr>
!>          <tr><td>T4(
!></td><td>---</td><td>TABLEAU DE TRAVAIL STRUCTURE
!>    </td></tr>
!>          <tr><td>TAILF
!></td><td>--></td><td>FACTEUR DE QUEUE DU SPECTRE
!>    </td></tr>
!>          <tr><td>TITCAS
!></td><td>--></td><td>TITRE DU CAS DE CALCUL
!>    </td></tr>
!>          <tr><td>TRA02(
!></td><td>---</td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>          <tr><td>TRA03(
!></td><td>---</td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>          <tr><td>TRA04(
!></td><td>---</td><td>TABLEAU DE TRAVAIL CONTENANT U*,Z0,...
!>    </td></tr>
!>          <tr><td>UX(
!></td><td>--></td><td>TABLEAU DES COMP. OUEST-EST DU COURANT
!>    </td></tr>
!>          <tr><td>UY(
!></td><td>--></td><td>TABLEAU DES COMP. SUD-NORD  DU COURANT
!>    </td></tr>
!>          <tr><td>VENT
!></td><td>--></td><td>INDICATEUR DE PRESENCE D'UN VENT
!>    </td></tr>
!>          <tr><td>VENTX(
!></td><td>---</td><td>TABLEAU DE VENT (COMP. OUEST-EST)
!>    </td></tr>
!>          <tr><td>VENTY(
!></td><td>---</td><td>TABLEAU DE VENT (COMP. SUD-NORD)
!>    </td></tr>
!>          <tr><td>W1(
!></td><td>---</td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>          <tr><td>X(
!></td><td>--></td><td>TABLEAU DES ABSCISSES DES POINTS MAILLAGE
!>    </td></tr>
!>          <tr><td>XF1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XK(
!></td><td>--></td><td>TABLEAU DES NOMBRES D'ONDE
!>    </td></tr>
!>          <tr><td>XMESH(
!></td><td>--></td><td>STRUCTURE MAILLAGE
!>    </td></tr>
!>          <tr><td>Y(
!></td><td>--></td><td>TABLEAU DES ABSCISSES DES POINTS MAILLAGE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DUMP2D
     &( LT , DEUPI , XF1 , NP1 )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| DATE COURANTE DU CALCUL
C| BINR2D         |-->| TYPE DE BINAIRE DU FICHIER DES RESULTATS 2D
C| CG(            |-->| TABLEAU DES VITESSES DE GROUPE
C| COSTET(        |-->| VECTEUR DES COSINUS DES DIRECTIONS
C| COURAN         |-->| INDICATEUR DE PRESENCE D'UN COURAN
C| DEBRES         |-->| INDICATEUR DE PREMIERE DATE A SAUVER
C| DEPTH(         |-->| TABLEAU DES PROFONDEURS (METRES)
C| DEUPI          |-->| 2.PI
C| DFREQ(         |-->| TABLEAU DES PAS DE FREQUENCES
C| F(             |-->| SPECTRE DIRECTIONNEL DE VARIANCE
C| FREQ(          |-->| TABLEAU DES FREQUENCES DE DISCRETISATION
C| GRAVIT         |-->| ACCELERATION DE LA PESANTEUR
C| IELM2          |-->| 
C| IKLE2(         |-->| CORRESPONDANCE NUMEROTATION LOCALE-GLOBALE
C| ITR01          |<->| TABLEAU DE TRAVAIL DE DIMENSION 3*NELEM2
C| LT             |---| 
C| MESH(          |-->| 
C| NBOR(          |-->| NUMEROTATION DES POINTS FRONTIERE
C| NELEM2         |-->| NOMBRE D'ELEMENTS DU MAILLAGE SPATIAL 2D
C| NF             |-->| NOMBRE DE FREQUENCES DE DISCRETISATION
C| NP1            |---| 
C| NPLAN          |-->| NOMBRE DE DIRECTIONS DE DISCRETISATION
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE SPATIAL 2D
C| NPRIV          |-->| DIMENSION DU TABLEAU UTILISATEUR
C| NPTFR          |-->| NOMBRE DE  POINTS FRONTIERE
C| NR2D           |-->| NUM. DU FICHIER DE SORTIE DES RESULTATS 2D
C| PRIVE(         |-->| TABLEAU UTILISATEUR
C| PROINF         |-->| INDICATEUR DE PROFONDEUR INFINIE
C| ROEAU          |-->| MASSE VOLUMIQUE DE L'EAU
C| SINTET(        |-->| VECTEUR DES SINUS   DES DIRECTIONS
C| SORG2D         |-->| INDICATEUR DE SORTIE DES VARIABLES 2D
C| T1(            |---| TABLEAU DE TRAVAIL STRUCTURE
C| T2(            |---| TABLEAU DE TRAVAIL STRUCTURE
C| T3(            |---| TABLEAU DE TRAVAIL STRUCTURE
C| T4(            |---| TABLEAU DE TRAVAIL STRUCTURE
C| TAILF          |-->| FACTEUR DE QUEUE DU SPECTRE
C| TITCAS         |-->| TITRE DU CAS DE CALCUL
C| TRA02(         |---| TABLEAU DE TRAVAIL
C| TRA03(         |---| TABLEAU DE TRAVAIL
C| TRA04(         |---| TABLEAU DE TRAVAIL CONTENANT U*,Z0,...
C| UX(            |-->| TABLEAU DES COMP. OUEST-EST DU COURANT
C| UY(            |-->| TABLEAU DES COMP. SUD-NORD  DU COURANT
C| VENT           |-->| INDICATEUR DE PRESENCE D'UN VENT
C| VENTX(         |---| TABLEAU DE VENT (COMP. OUEST-EST)
C| VENTY(         |---| TABLEAU DE VENT (COMP. SUD-NORD)
C| W1(            |---| TABLEAU DE TRAVAIL
C| X(             |-->| TABLEAU DES ABSCISSES DES POINTS MAILLAGE
C| XF1            |---| 
C| XK(            |-->| TABLEAU DES NOMBRES D'ONDE
C| XMESH(         |-->| STRUCTURE MAILLAGE
C| Y(             |-->| TABLEAU DES ABSCISSES DES POINTS MAILLAGE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE DECLARATIONS_TOMAWAC
      USE INTERFACE_TOMAWAC, EX_DUMP2D => DUMP2D
C
      IMPLICIT NONE
C
      INTEGER          LT    , I1    , I2    , NP1   , IP
      DOUBLE PRECISION DEUPI , RADDEG, PIS2  , U10   , FMIN  , FMAX
      DOUBLE PRECISION XF1(NP1)
C
C
      RADDEG=57.29577951D0
      PIS2=1.570796327D0
      FMIN=FREQ(1)
      FMAX=FREQ(NF)
C
C
C=====C====================================
C     C COMPUTES THE SELECTED VARIABLES
C=====C====================================
C THE ORDER IN WHICH THE VARIABLES ARE COMPUTED DOES NOT CORRESPOND TO THAT OF
C THE GRAPHICAL OUTPUT IN AN EFFORT TO LIMIT THE NUMBER OF WORKING ARRAYS.
C
C     -------------------------------RADIATION STRESSES
      I1=NPOIN3*NF+1
      I2=I1+NPOIN2*NF
      IF (.NOT.PROINF) THEN
        IF ( SORLEO(11).OR.SORLEO(12).OR.SORLEO(13).OR.
     &       SORLEO(14).OR.SORLEO(15) ) CALL RADIAT
     &( STRA51%R, STRA52%R, STRA53%R, STRA54%R, STRA55%R,
     &  SXK%R   , XF1        , SCG%R   , SDEPTH%R,
     &  TRA02(I1:I2),STRA36%R, STRA37%R, STRA38%R, STRA39%R,
     &  NPOIN2)
      ENDIF
C     -------------------------------DIRECTIONAL SPREADING
      IF (SORLEO(4)) THEN
        CALL SPREAD
     &( STRA31%R, XF1        , SCOSTE%R, SSINTE%R, NPLAN ,
     &  SFR%R   , SDFR%R  , NF         , NPOIN2     , TAILF      ,
     &  STRA34%R, STRA35%R, STRA36%R, STRA37%R, STRA38%R,
     &  STRA39%R)
      ENDIF
C     -------------------------------MEAN DIRECTION
      IF (SORLEO(3)) THEN
       CALL TETMOY
     &( STRA32%R, XF1   , SCOSTE%R, SSINTE%R, NPLAN , FREQ  ,
     &  SDFR%R  , NF    , NPOIN2     , TAILF      , STRA36%R   ,
     &  STRA37%R, STRA38%R, STRA39%R )
       IF (TRIGO) THEN
         DO IP=1,NPOIN2
           TRA32(IP)=(PIS2-TRA32(IP))*RADDEG
         ENDDO
       ELSE
         DO IP=1,NPOIN2
           TRA32(IP)=TRA32(IP)*RADDEG
         ENDDO
       ENDIF
      ENDIF
C     -------------------------------MEAN FREQUENCY FMOY
      IF (SORLEO(18).OR.SORLEO(28)) THEN
        CALL FREMOY
     &( STRA33%R, XF1   , SFR%R   , SDFR%R  , TAILF , NF  ,
     &  NPLAN      , NPOIN2, STRA38%R, STRA39%R)
        IF (SORLEO(28)) THEN
          DO IP=1,NPOIN2
            TRA61(IP)=1.D0/MIN(MAX(TRA33(IP),FMIN),FMAX)
          ENDDO
        ENDIF
      ENDIF
C     -------------------------------MEAN FREQUENCY FM01
      IF (SORLEO(19).OR.SORLEO(29)) THEN
        CALL FREM01
     &( STRA34%R, XF1   , SFR%R   , SDFR%R  , TAILF , NF  ,
     &  NPLAN      , NPOIN2, STRA38%R, STRA39%R)
        IF (SORLEO(29)) THEN
          DO IP=1,NPOIN2
            TRA62(IP)=1.D0/MIN(MAX(TRA34(IP),FMIN),FMAX)
          ENDDO
        ENDIF
      ENDIF
C     -------------------------------MEAN FREQUENCY FM02
      IF (SORLEO(20).OR.SORLEO(30)) THEN
        CALL FREM02
     &( STRA35%R, XF1   , SFR%R   , SDFR%R  , TAILF , NF  ,
     &  NPLAN      , NPOIN2, STRA38%R, STRA39%R)
        IF (SORLEO(30)) THEN
          DO IP=1,NPOIN2
            TRA63(IP)=1.D0/MIN(MAX(TRA35(IP),FMIN),FMAX)
          ENDDO
        ENDIF
      ENDIF
C     -------------------------------DISCRETE PEAK FREQUENCY
      IF (SORLEO(21).OR.SORLEO(31)) THEN
        CALL FREPIC
     &( STRA36%R, XF1   , SFR%R , NF   , NPLAN , NPOIN2,
     &  STRA38%R, STRA39%R      )
        IF (SORLEO(31)) THEN
          DO IP=1,NPOIN2
            TRA64(IP)=1.D0/MIN(MAX(TRA36(IP),FMIN),FMAX)
          ENDDO
        ENDIF
      ENDIF
C     -------------------------------PEAK FREQUENCY (READ 5TH ORDER)
      IF (SORLEO(22).OR.SORLEO(32)) THEN
        CALL FPREAD
     &( STRA56%R, XF1   , SFR%R, SDFR%R  , NF   , NPLAN ,
     &  NPOIN2     , 5.D0  , TAILF   , STRA38%R, STRA39%R  )
        IF (SORLEO(32)) THEN
          DO IP=1,NPOIN2
            TRA65(IP)=1.D0/MIN(MAX(TRA56(IP),FMIN),FMAX)
          ENDDO
        ENDIF
      ENDIF
C     -------------------------------PEAK FREQUENCY (READ 8TH ORDER)
      IF (SORLEO(23).OR.SORLEO(33)) THEN
        CALL FPREAD
     &( STRA57%R, XF1   , SFR%R , SDFR%R  , NF   , NPLAN ,
     &  NPOIN2     , 8.D0  , TAILF    , STRA38%R, STRA39%R  )
        IF (SORLEO(33)) THEN
          DO IP=1,NPOIN2
            TRA66(IP)=1.D0/MIN(MAX(TRA57(IP),FMIN),FMAX)
          ENDDO
        ENDIF
      ENDIF
C
      IF (VENT) THEN
C       -------------------------------DRAG COEFFICIENT
        IF (SORLEO(25)) THEN
          DO IP=1,NPOIN2
            U10=UV(IP)**2+VV(IP)**2
            IF (U10.GT.1.D-6) THEN
              TRA58(IP)=TRA42(IP)**2/U10
            ELSE
              TRA58(IP)=0.D0
            ENDIF
          ENDDO
        ENDIF
      ENDIF
C       -------------------------------BOTTOM SPEED
      IF (.NOT.PROINF) THEN
        IF (SORLEO(16)) THEN
          CALL VITFON
     &( STRA59%R, XF1   , SXK%R , SDEPTH%R, SDFR%R , NF   ,
     &  NPOIN2     , NPLAN , GRAVIT   , STRA39%R)
        ENDIF
      ENDIF
C     -------------------------------VARIANCE
      IF (SORLEO(1).OR.SORLEO(2)) THEN
        CALL TOTNRJ
     &( STRA37%R , XF1   , SFR%R  , SDFR%R , TAILF ,
     &  NF  , NPLAN , NPOIN2)
C     -------------------------------SIGNIFICANT WAVE HEIGHT
        IF (SORLEO(2)) THEN
          DO IP=1,NPOIN2
            TRA38(IP)=4.D0*SQRT(TRA37(IP))
          ENDDO
        ENDIF
      ENDIF
C     -------------------------------POWER PER UNIT LENGTH
      IF (SORLEO(34)) THEN
        CALL WPOWER
     &( STRA60%R, XF1   , SFR%R  , SDFR%R , SCG%R  , TAILF , NF   ,
     &  NPLAN , NPOIN2, ROEAU , GRAVIT)
      ENDIF
C
      RETURN
      END
C
C#######################################################################
C
