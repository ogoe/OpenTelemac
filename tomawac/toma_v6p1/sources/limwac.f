C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BOUNDARY CONDITIONS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  BY DEFAULT, THE BOUNDARY CONDITIONS SPECIFIED IN THE FILE
!>            DYNAM ARE DUPLICATED ON ALL THE DIRECTIONS AND FREQUENCIES

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> APHILL, AT, BINBI1, DDC, DEPTH, DEUPI, F, FBOR, FETCHL, FPICL, FPMAXL, FRA, FRABL, FREQ, GAMMAL, GRAVIT, HM0L, KENT, KSORT, LIFBOR, LIMSPE, LT, NBI1, NBOR, NF, NFO1, NPLAN, NPOIN2, NPRIV, NPTFR, PRIVE, SIGMAL, SIGMBL, SPEC, SPEULI, SPRE1L, SPRE2L, TETA, TETA1L, TETA2L, UV, VENSTA, VENT, VV, X, XLAMDL, Y
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> E2FMIN, FB_CTE, FLAG, IFF, IPLAN, IPTFR, NPB, PROF, TRAV, UV2D, VV2D
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> SPEINI()
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
!>      <td><center> 1.0                                       </center>
!> </td><td> 01/02/95
!> </td><td> F. MARCOS (LNH) 30 87 72 66
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>APHILL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>AT
!></td><td>--></td><td>TEMPS
!>    </td></tr>
!>          <tr><td>BINBI1
!></td><td>--></td><td>BINAIRE DU FICHIER BINAIRE UTILISATEUR
!>    </td></tr>
!>          <tr><td>DDC
!></td><td>--></td><td>DATE DU DEBUT DU CALCUL
!>    </td></tr>
!>          <tr><td>DEPTH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DEUPI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F
!></td><td>--></td><td>DENSITE SPECTRALE
!>    </td></tr>
!>          <tr><td>FBOR
!></td><td><-></td><td>DENSITE SPECTRALE AU BORD
!>    </td></tr>
!>          <tr><td>FETCHL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FPICL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FPMAXL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FRA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FRABL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FREQ
!></td><td>--></td><td>FREQUENCES DISCRETISEES
!>    </td></tr>
!>          <tr><td>GAMMAL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRAVIT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HM0L
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KENT
!></td><td>--></td><td>C.L. INDIQUANT UNE FRONTIERE MARITIME
!>    </td></tr>
!>          <tr><td>KSORT
!></td><td>--></td><td>C.L. INDIQUANT UNE FRONTIERE SOLIDE
!>    </td></tr>
!>          <tr><td>LIFBOR
!></td><td>--></td><td>TYPE DE CONDITION LIMITE SUR F
!>    </td></tr>
!>          <tr><td>LIMSPE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LT
!></td><td>--></td><td>NUMERO DU PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>NBI1
!></td><td>--></td><td>NUMERO DU FICHIER BINAIRE UTILISATEUR
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMEROTATION DES POINTS DE BORD 2D
!>    </td></tr>
!>          <tr><td>NF
!></td><td>--></td><td>NOMBRE DE FREQUENCES
!>    </td></tr>
!>          <tr><td>NFO1
!></td><td>--></td><td>NUMERO DU FICHIER FORMATE UTILISATEUR
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE DIRECTIONS
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS 2D
!>    </td></tr>
!>          <tr><td>NPRIV
!></td><td>--></td><td>DIMENSION DU TABLEAU PRIVE
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERE 2D
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td>--></td><td>TABLEAU DE L'UTILISATEUR
!>    </td></tr>
!>          <tr><td>SIGMAL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SIGMBL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SPEC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SPEULI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SPRE1L
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SPRE2L
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TETA
!></td><td>--></td><td>DIRECTIONS DE PROPAGATION
!>    </td></tr>
!>          <tr><td>TETA1L
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TETA2L
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VENSTA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VENT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X
!></td><td>--></td><td>ABSCISSES DES POINTS 2D
!>    </td></tr>
!>          <tr><td>XLAMDL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Y
!></td><td>--></td><td>ORDONNEES DES POINTS 2D
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE LIMWAC
     &(F     , FBOR  , LIFBOR, NPTFR , NPLAN , NF    ,  TETA , FREQ  ,
     & NPOIN2, NBOR  , AT    , LT    , DDC   , LIMSPE, FPMAXL, FETCHL,
     & SIGMAL, SIGMBL, GAMMAL, FPICL , HM0L  , APHILL, TETA1L, SPRE1L,
     & TETA2L, SPRE2L, XLAMDL, X ,Y  , KENT  , KSORT , NFO1  , NBI1  ,
     & BINBI1, UV    , VV    , SPEULI, VENT  , VENSTA, GRAVIT, DEUPI ,
     & PRIVE , NPRIV , SPEC  , FRA   , DEPTH , FRABL ,BOUNDARY_COLOUR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| APHILL         |---| 
C| AT             |-->| TEMPS
C| BINBI1         |-->| BINAIRE DU FICHIER BINAIRE UTILISATEUR
C| DDC            |-->| DATE DU DEBUT DU CALCUL
C| DEPTH          |---| 
C| DEUPI          |---| 
C| F             |-->| DENSITE SPECTRALE
C| FBOR           |<->| DENSITE SPECTRALE AU BORD
C| FETCHL         |---| 
C| FPICL          |---| 
C| FPMAXL         |---| 
C| FRA            |---| 
C| FRABL          |---| 
C| FREQ           |-->| FREQUENCES DISCRETISEES
C| GAMMAL         |---| 
C| GRAVIT         |---| 
C| HM0L           |---| 
C| KENT           |-->| C.L. INDIQUANT UNE FRONTIERE MARITIME
C| KSORT          |-->| C.L. INDIQUANT UNE FRONTIERE SOLIDE
C| LIFBOR         |-->| TYPE DE CONDITION LIMITE SUR F
C| LIMSPE         |---| 
C| LT             |-->| NUMERO DU PAS DE TEMPS
C| NBI1           |-->| NUMERO DU FICHIER BINAIRE UTILISATEUR
C| NBOR           |-->| NUMEROTATION DES POINTS DE BORD 2D
C| NF             |-->| NOMBRE DE FREQUENCES
C| NFO1           |-->| NUMERO DU FICHIER FORMATE UTILISATEUR
C| NPLAN          |-->| NOMBRE DE DIRECTIONS
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| NPRIV          |-->| DIMENSION DU TABLEAU PRIVE
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE 2D
C| PRIVE          |-->| TABLEAU DE L'UTILISATEUR
C| SIGMAL         |---| 
C| SIGMBL         |---| 
C| SPEC           |---| 
C| SPEULI         |---| 
C| SPRE1L         |---| 
C| SPRE2L         |---| 
C| TETA           |-->| DIRECTIONS DE PROPAGATION
C| TETA1L         |---| 
C| TETA2L         |---| 
C| UV             |---| 
C| VENSTA         |---| 
C| VENT           |---| 
C| VV             |---| 
C| X             |-->| ABSCISSES DES POINTS 2D
C| XLAMDL         |---| 
C| Y             |-->| ORDONNEES DES POINTS 2D
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_TOMAWAC, EX_LIMWAC => LIMWAC
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
      INTEGER NPLAN,NF,NPOIN2,NPTFR,LT,NPRIV
      INTEGER, INTENT(IN) :: BOUNDARY_COLOUR(NPTFR)
C
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF),X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION FBOR(NPTFR,NPLAN,NF),TETA(NPLAN),FREQ(NF)
      DOUBLE PRECISION UV(NPOIN2),VV(NPOIN2), SPEC(NF), FRA(NPLAN)
      DOUBLE PRECISION PRIVE(NPOIN2,NPRIV),DDC, DEPTH(NPOIN2)
      DOUBLE PRECISION HM0L,FPICL,GAMMAL,SIGMAL,SIGMBL,APHILL,FETCHL
      DOUBLE PRECISION FPMAXL,TETA1L,SPRE1L,TETA2L,SPRE2L,XLAMDL
      DOUBLE PRECISION GRAVIT,DEUPI,E2FMIN
C
      DOUBLE PRECISION AT
C
      LOGICAL SPEULI, VENT, VENSTA
C
      INTEGER NBOR(NPTFR),LIFBOR(NPTFR),NFO1,NBI1,NPB
      INTEGER KENT,KSORT,IFF,IPLAN,IPTFR,LIMSPE,FRABL
C
      DOUBLE PRECISION, ALLOCATABLE :: TRAV(:)
      DOUBLE PRECISION, ALLOCATABLE :: UV2D(:),VV2D(:),PROF(:)
      DOUBLE PRECISION, ALLOCATABLE :: FB_CTE(:,:)
      LOGICAL FLAG
C
      CHARACTER*3 BINBI1
C
      SAVE NPB,UV2D,VV2D,PROF,FB_CTE
C
C***********************************************************************
C
C   MODIFIES THE TYPE OF BOUNDARY CONDITION (OPTIONAL)
C
C   TO BE CODED BU THE USER
C
C   LIFBOR(IPTFR)=KENT OR KSORT
C
      IF (LIMSPE.EQ.0 .AND. .NOT.SPEULI) RETURN
C
      FLAG=.FALSE.
      IF (VENT .AND. (LIMSPE.EQ.1 .OR. LIMSPE.EQ.2 .OR. LIMSPE.EQ.3
     & .OR. LIMSPE.EQ.5)) FLAG=.TRUE.
C
C     THE FIRST TIME, ALLOCATES MEMORY FOR THE USEFUL ARRAYS
C     ---------------------------------------------------------------
      IF (LT.LT.1) THEN
        NPB=1
        IF (FLAG) THEN
           ALLOCATE(UV2D(1:NPTFR),VV2D(1:NPTFR))
           NPB=NPTFR
        ENDIF
        IF (LIMSPE.EQ.7 .OR. SPEULI) THEN
           ALLOCATE(PROF(1:NPTFR))
           NPB=NPTFR
        ENDIF
        IF (NPB.EQ.1) THEN
           ALLOCATE(FB_CTE(1:NPLAN,1:NF))
        ENDIF
      ENDIF
      IF (.NOT.ALLOCATED(UV2D)) ALLOCATE(UV2D(NPTFR))
      IF (.NOT.ALLOCATED(VV2D)) ALLOCATE(VV2D(NPTFR))
      IF (.NOT.ALLOCATED(PROF)) ALLOCATE(PROF(NPTFR))
      IF (.NOT.ALLOCATED(FB_CTE)) ALLOCATE(FB_CTE(1:NPLAN,1:NF))
C
C     THE FIRST TIME (AND POSSIBLY SUBSEQUENTLY IF THE WIND IS NOT
C     STATIONARY AND IF THE BOUNDARY SPECTRUM DEPENDS ON IT),
C     COMPUTES THE BOUNDARY SPECTRUM
C     ----------------------------------------------------------------
      IF (LT.LT.1 .OR. (.NOT.VENSTA.AND.FLAG) .OR. SPEULI) THEN
        IF (FLAG) THEN
          DO IPTFR=1,NPTFR
            UV2D(IPTFR)=UV(NBOR(IPTFR))
            VV2D(IPTFR)=VV(NBOR(IPTFR))
          ENDDO
        ENDIF
        IF(LIMSPE.EQ.7 .OR. SPEULI) THEN
          DO IPTFR=1,NPTFR
            PROF(IPTFR)=DEPTH(NBOR(IPTFR))
          ENDDO
        ENDIF
C
C       CALLS SPEINI
C     ----------------------------------------------------------------
        E2FMIN = 1.D-30
C
        IF (NPB.EQ.NPTFR) THEN
          CALL SPEINI
     &( FBOR  , SPEC  , FRA    , UV2D  , VV2D  , FREQ ,
     &  TETA  , GRAVIT, FPMAXL , FETCHL, SIGMAL, SIGMBL, GAMMAL, FPICL,
     &  HM0L  , APHILL, TETA1L , SPRE1L, TETA2L, SPRE2L, XLAMDL,
     &  NPTFR , NPLAN , NF     , LIMSPE, E2FMIN, PROF  , FRABL )
        ELSE
          CALL SPEINI
     &( FB_CTE, SPEC  , FRA    , UV2D  , VV2D  , FREQ ,
     &  TETA  , GRAVIT, FPMAXL , FETCHL, SIGMAL, SIGMBL, GAMMAL, FPICL,
     &  HM0L  , APHILL, TETA1L , SPRE1L, TETA2L, SPRE2L, XLAMDL,
     &  NPB   , NPLAN , NF     , LIMSPE, E2FMIN, PROF  , FRABL )
	ENDIF
C
C     ===========================================================
C     TO BE MODIFIED BY USER - RESU CAN BE CHANGED
C     ===========================================================
        IF (SPEULI) THEN
C
C        EXEMPLE DE MODIFICATION DE FRA - A MODIFIER SUIVANT VOTRE CAS
C        EXAMPLE OF MODIFICATION OF FRA - TO BE MODIFIED DEPENDING
C        ON YOUR CASE
C        ALLOCATE(TRAV(1:NF))
C
C        DO IFREQ=1,NF
C             IF (FREQ(IFF).LT.FPIC) THEN
C              TRAV(IFF)=0.4538D0*(FREQ(IFF)/FPIC)**(-2.03D0)
C           ELSE
C              TRAV(IFF)=0.4538D0*(FREQ(IFF)/FPIC)**(1.04D0)
C           ENDIF
C        ENDDO
C
C        DO IPLAN=1,NPLAN
C             DTETA=TETA(IPLAN)-TETA1
C           IF ((TETA(IPLAN)-TETA1).GT.DEUPI/2) THEN
C              DTETA=DEUPI-DTETA
C           ENDIF
C           DO IFF=1,NF
C              FRA(IPLAN)=1.D0/SQRT(DEUPI)*TRAV(IFF)*
C     *                       EXP(-DTETA**2/(2.D0*TRAV(IFF)**2))
C              DO IPTFR=1,NPTFR
C                FBOR(IPTFR,IPLAN,IFF)= SPEC(IFF)*FRA(IPLAN)
C              ENDDO
C           ENDDO
C        ENDDO
C        DEALLOCATE(TRAV)
C
C        PARTIE A SUPPRIMER SI ON FAIT DES MODIFICATIONS
C        DELETE THESE LINES IF MODIFICATIONS HAVE BEEN IMPLEMENTED
C
        IF (LNG.EQ.1) THEN
          WRITE(LU,*)'*****  ERREUR LIMWAC  ******'
          WRITE(LU,*)
     &      ' VOUS NE MODIFIEZ PAS LE SPECTRE AUX LIMITES ALORS QUE'
          WRITE(LU,*)' VOUS EN DEMANDEZ LA POSSIBILITE'
        ELSE
          WRITE(LU,*)'*****  ERROR LIMWAC  ******'
          WRITE(LU,*)
     &      ' YOU DID NOT MODIFY THE BOUNDARY SPECTRUM WHEREAS '
          WRITE(LU,*)' YOU ASK FOR THAT '
        ENDIF
        STOP
      ENDIF
C
C     ===========================================================
C     END OF USER MODIFICATIONS
C     ===========================================================
      ENDIF
C
C
C     -----------------------------------------------------------------
C     DUPLICATES THE BOUNDARY CONDITION FROM DYNAM ON ALL THE
C     DIRECTIONS AND FREQUENCIES, IF LIQUID BOUNDARY
C     -----------------------------------------------------------------
      IF (FLAG .OR. LIMSPE.EQ.7 .OR. SPEULI) THEN
        DO IPTFR=1,NPTFR
          IF (LIFBOR(IPTFR).EQ.KENT) THEN
            DO IFF=1,NF
              DO IPLAN=1,NPLAN
                F(NBOR(IPTFR),IPLAN,IFF)=FBOR(IPTFR,IPLAN,IFF)
              ENDDO
            ENDDO
          ENDIF
        ENDDO
      ELSE
        DO IPTFR=1,NPTFR
          IF (LIFBOR(IPTFR).EQ.KENT) THEN
            DO IFF=1,NF
              DO IPLAN=1,NPLAN
                F(NBOR(IPTFR),IPLAN,IFF)=FB_CTE(IPLAN,IFF)
              ENDDO
            ENDDO
          ENDIF
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
