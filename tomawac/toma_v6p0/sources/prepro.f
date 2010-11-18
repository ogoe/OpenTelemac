C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PREPARES ADVECTION.
!><br>            COMPUTES THE ADVECTION FIELD; TRACES BACK THE
!>                CHARACTERISTICS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, TOMAWAC_MPI, TOMAWAC_MPI_TOOLS
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CF, CG, COSF, COSTET, COURAN, CT, CX, CY, DEPTH, DT, DUX, DUY, DVX, DVY, DZHDT, DZX, DZY, ELT, ETA, ETAP1, FRE, FREQ, IFABOR, IKLE2, ITR01, MESH, NELEM2, NF, NPLAN, NPOIN2, NPOIN3, NRK, PROINF, PROMIN, SHF, SHP1, SHP2, SHP3, SHZ, SINTET, SPHE, SURDET, TETA, TGF, TRA01, U, V, X, XK, Y
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> TOMAWAC_MPI :<br>
!> @link TOMAWAC_MPI::IFREQ IFREQ@endlink, 
!> @link TOMAWAC_MPI::ISPDONE ISPDONE@endlink, 
!> @link TOMAWAC_MPI::NARRV NARRV@endlink, 
!> @link TOMAWAC_MPI::NCHARA NCHARA@endlink, 
!> @link TOMAWAC_MPI::NCHDIM NCHDIM@endlink, 
!> @link TOMAWAC_MPI::NFREQ NFREQ@endlink, 
!> @link TOMAWAC_MPI::NLOSTCHAR NLOSTCHAR@endlink, 
!> @link TOMAWAC_MPI::NSEND NSEND@endlink, 
!> @link TOMAWAC_MPI::RECVAGAIN RECVAGAIN@endlink, 
!> @link TOMAWAC_MPI::RECVAGAIN_4D RECVAGAIN_4D@endlink, 
!> @link TOMAWAC_MPI::RECVCHAR RECVCHAR@endlink, 
!> @link TOMAWAC_MPI::RECVCHAR_4D RECVCHAR_4D@endlink, 
!> @link TOMAWAC_MPI::SH_AGAIN SH_AGAIN@endlink, 
!> @link TOMAWAC_MPI::SH_AGAIN_4D SH_AGAIN_4D@endlink, 
!> @link TOMAWAC_MPI::SH_LOC SH_LOC@endlink, 
!> @link TOMAWAC_MPI::SH_LOC_4D SH_LOC_4D@endlink, 
!> @link TOMAWAC_MPI::TEST TEST@endlink<hr>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CAR, GOODELT, I, I1, I2, I3, I4, IEL, IER, IFF, IP, IPLAN, IPOIN, ISTAT, ITE, JF, LAST_NOMB, NARRSUM, NBB, NLOSTAGAIN, NRECV, NUMBER, NUMBERLOST, TES
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ALLOC_AGAIN(), ALLOC_AGAIN_4D(), ALLOC_LOCAL(), ALLOC_LOCAL_4D(), CONW4D(), CONWAC(), CORRECT_GOODELT(), ENVOI_AGAIN(), ENVOI_AGAIN_4D(), FINAL_ORGA_RECV(), FINAL_ORGA_RECV_4D(), GLOB_CHAR_COMM(), GLOB_CHAR_COMM_4D(), INCREM_ENVOI_RECV(), INCREM_ENVOI_RECV_4D(), INIP4D(), INIPIE(), INIT_TOMAWAC(), INIT_TOMAWAC_4D(), ORGANIZE_SENDAGAIN(), ORGANIZE_SENDAGAIN_4D(), PARCOM2(), PIED4D(), PIED4D_TOMAWAC(), PIEDS(), PIEDS4D_TOMAWAC_MPI(), PIEDS_TOMAWAC(), PIEDS_TOMAWAC_MPI(), PREP_INITIAL_SEND(), PREP_INITIAL_SEND_4D(), RESET_COUNT(), SUPP_ENVOI_AGAIN(), SUPP_ENVOI_AGAIN_4D(), WIPE_HEAPED_CHAR(), WIPE_HEAPED_CHAR_4D()
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
!> </td><td> 04/12/95
!> </td><td> F. MARCOS (LNH) 30 87 72 66
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CG
!></td><td>--></td><td>VITESSE DE GROUPE DISCRETISEE
!>    </td></tr>
!>          <tr><td>COSF
!></td><td>--></td><td>COSINUS DES LATITUDES DES POINTS 2D
!>    </td></tr>
!>          <tr><td>COSTET
!></td><td>--></td><td>COSINUS TETA
!>    </td></tr>
!>          <tr><td>COURAN
!></td><td>--></td><td>LOGIQUE INDIQUANT SI ON A UN COURANT
!>    </td></tr>
!>          <tr><td>CX,CY,CT,CF
!></td><td><-></td><td>CHAMP CONVECTEUR SELON X(OU PHI),
!>                  Y(OU LAMBDA) TETA ET F
!>    </td></tr>
!>          <tr><td>DEPTH
!></td><td>--></td><td>PROFONDEUR
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>TEMPS
!>    </td></tr>
!>          <tr><td>DUX,DUY
!></td><td>--></td><td>GRADIENT DU COURANT SELON X SELON X,Y
!>    </td></tr>
!>          <tr><td>DVX,DVY
!></td><td>--></td><td>GRADIENT DU COURANT SELON Y SELON X,Y
!>    </td></tr>
!>          <tr><td>DZHDT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DZX
!></td><td>--></td><td>GRADIENT DE FOND SELON X
!>    </td></tr>
!>          <tr><td>DZY
!></td><td>--></td><td>GRADIENT DE FOND SELON Y
!>    </td></tr>
!>          <tr><td>ELT
!></td><td><-></td><td>NUMEROS DES ELEMENTS 2D CHOISIS POUR CHAQUE
!>                  NOEUD.
!>    </td></tr>
!>          <tr><td>ETA
!></td><td><-></td><td>NUMEROS DES ETAGES CHOISIS POUR CHAQUE NOEUD.
!>    </td></tr>
!>          <tr><td>ETAP1
!></td><td><-></td><td>TABLEAU DE TRAVAIL DONNANT LE NUMERO DE
!>                  L'ETAGE SUPERIEUR
!>    </td></tr>
!>          <tr><td>FRE
!></td><td><-></td><td>NUMEROS DES FREQ. CHOISIES POUR CHAQUE NOEUD.
!>    </td></tr>
!>          <tr><td>FREQ
!></td><td>--></td><td>FREQUENCES DISCRETISEES
!>    </td></tr>
!>          <tr><td>IFABOR
!></td><td>--></td><td>NUMEROS 2D DES ELEMENTS AYANT UNE FACE COMMUNE
!>                  AVEC L'ELEMENT .  SI IFABOR
!>                  ON A UNE FACE LIQUIDE,SOLIDE,OU PERIODIQUE
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>--></td><td>TRANSITION ENTRE LES NUMEROTATIONS LOCALE
!>                  ET GLOBALE DU MAILLAGE 2D.
!>    </td></tr>
!>          <tr><td>ITR01
!></td><td><-></td><td>TABLEAU DE TRAVAIL ENTIER
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NOMBRE D ELEMENTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>NF
!></td><td>--></td><td>NOMBRE DE FREQUENCES
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS OU DE DIRECTIONS
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>NRK
!></td><td>--></td><td>NOMBRE DE SOUS PAS DE RUNGE KUTTA
!>    </td></tr>
!>          <tr><td>PROINF
!></td><td>--></td><td>LOGIQUE INDIQUANT SI ON EST EN PROF INFINIE
!>    </td></tr>
!>          <tr><td>PROMIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SHF
!></td><td><-></td><td>COORDONNEES BARYCENTRIQUES SUIVANT F DES
!>                  NOEUDS DANS LEURS FREQUENCES "FRE" ASSOCIEES.
!>    </td></tr>
!>          <tr><td>SHP1
!></td><td>---</td><td>COORDONNEES BARYCENTRIQUES 2D AU PIED DES
!>                  COURBES CARACTERISTIQUES.
!>    </td></tr>
!>          <tr><td>SHP2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SHP3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SHT
!></td><td><-></td><td>COORDONNEES BARYCENTRIQUES SUIVANT TETA DES
!>                  NOEUDS DANS LEURS ETAGES "ETA" ASSOCIES.
!>    </td></tr>
!>          <tr><td>SHZ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SINTET
!></td><td>--></td><td>SINUS TETA
!>    </td></tr>
!>          <tr><td>SPHE
!></td><td>--></td><td>LOGIQUE INDIQUANT SI ON EST EN COORD. SPHER.
!>    </td></tr>
!>          <tr><td>SURDET
!></td><td>--></td><td>INVERSE DU DETERMINANT DES ELEMENTS
!>    </td></tr>
!>          <tr><td>TETA
!></td><td>--></td><td>DIRECTIONS DISCRETISEES
!>    </td></tr>
!>          <tr><td>TGF
!></td><td>--></td><td>TANGENTES DES LATITUDES DES POINTS 2D
!>    </td></tr>
!>          <tr><td>TRA01
!></td><td><-></td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>          <tr><td>U,V
!></td><td>--></td><td>COURANT SELON X,Y
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DES POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>XK
!></td><td>--></td><td>NOMBRE D'ONDE DISCRETISE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PREPRO
     &( CX    , CY    , CT    , CF    , DT    , NRK   , X     , Y     ,
     &  TETA  , COSTET, SINTET, FREQ  , IKLE2 , IFABOR, ETAP1 , TRA01 ,
     &  SHP1  , SHP2  , SHP3  , SHZ   , SHF   , ELT   , ETA   , FRE   ,
     &  DEPTH , DZHDT , DZX   , DZY   , U     , V     , DUX   , DUY   ,
     &  DVX   , DVY   , XK    , CG    , COSF  , TGF   , ITR01 , NPOIN3,
     &  NPOIN2, NELEM2, NPLAN , NF    , SURDET, COURAN, SPHE  ,
     &  PROINF, PROMIN, MESH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CG             |-->| VITESSE DE GROUPE DISCRETISEE
C| COSF           |-->| COSINUS DES LATITUDES DES POINTS 2D
C| COSTET         |-->| COSINUS TETA
C| COURAN         |-->| LOGIQUE INDIQUANT SI ON A UN COURANT
C| CX,CY,CT,CF    |<->| CHAMP CONVECTEUR SELON X(OU PHI),
C|                |   | Y(OU LAMBDA) TETA ET F
C| DEPTH          |-->| PROFONDEUR
C| DT             |-->| TEMPS
C| DUX,DUY        |-->| GRADIENT DU COURANT SELON X SELON X,Y
C| DVX,DVY        |-->| GRADIENT DU COURANT SELON Y SELON X,Y
C| DZHDT          |---| 
C| DZX            |-->| GRADIENT DE FOND SELON X
C| DZY            |-->| GRADIENT DE FOND SELON Y
C| ELT            |<->| NUMEROS DES ELEMENTS 2D CHOISIS POUR CHAQUE
C|                |   | NOEUD.
C| ETA            |<->| NUMEROS DES ETAGES CHOISIS POUR CHAQUE NOEUD.
C| ETAP1          |<->| TABLEAU DE TRAVAIL DONNANT LE NUMERO DE
C|                |   | L'ETAGE SUPERIEUR
C| FRE            |<->| NUMEROS DES FREQ. CHOISIES POUR CHAQUE NOEUD.
C| FREQ           |-->| FREQUENCES DISCRETISEES
C| IFABOR         |-->| NUMEROS 2D DES ELEMENTS AYANT UNE FACE COMMUNE
C|                |   | AVEC L'ELEMENT .  SI IFABOR
C|                |   | ON A UNE FACE LIQUIDE,SOLIDE,OU PERIODIQUE
C| IKLE2          |-->| TRANSITION ENTRE LES NUMEROTATIONS LOCALE
C|                |   | ET GLOBALE DU MAILLAGE 2D.
C| ITR01          |<->| TABLEAU DE TRAVAIL ENTIER
C| MESH           |---| 
C| NELEM2         |-->| NOMBRE D ELEMENTS DU MAILLAGE 2D
C| NF             |-->| NOMBRE DE FREQUENCES
C| NPLAN          |-->| NOMBRE DE PLANS OU DE DIRECTIONS
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
C| NPOIN3         |-->| NOMBRE DE POINTS DU MAILLAGE 3D
C| NRK            |-->| NOMBRE DE SOUS PAS DE RUNGE KUTTA
C| PROINF         |-->| LOGIQUE INDIQUANT SI ON EST EN PROF INFINIE
C| PROMIN         |---| 
C| SHF            |<->| COORDONNEES BARYCENTRIQUES SUIVANT F DES
C|                |   | NOEUDS DANS LEURS FREQUENCES "FRE" ASSOCIEES.
C| SHP1           |---| COORDONNEES BARYCENTRIQUES 2D AU PIED DES
C|                |   | COURBES CARACTERISTIQUES.
C| SHP2           |---| 
C| SHP3           |---| 
C| SHT            |<->| COORDONNEES BARYCENTRIQUES SUIVANT TETA DES
C|                |   | NOEUDS DANS LEURS ETAGES "ETA" ASSOCIES.
C| SHZ            |---| 
C| SINTET         |-->| SINUS TETA
C| SPHE           |-->| LOGIQUE INDIQUANT SI ON EST EN COORD. SPHER.
C| SURDET         |-->| INVERSE DU DETERMINANT DES ELEMENTS
C| TETA           |-->| DIRECTIONS DISCRETISEES
C| TGF            |-->| TANGENTES DES LATITUDES DES POINTS 2D
C| TRA01          |<->| TABLEAU DE TRAVAIL
C| U,V            |-->| COURANT SELON X,Y
C| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE
C| XK             |-->| NOMBRE D'ONDE DISCRETISE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
!BD_INCKA MODIFICATION FOR PARALLEL MODE
      USE TOMAWAC_MPI_TOOLS
      USE TOMAWAC_MPI
C       USE TOMAWAC_MPI, ONLY : SH_AGAIN,RECVAGAIN,SH_LOC,RECVCHAR,
C      *                        NARRV,NCHARA,NLOSTCHAR,NSEND,TEST,
C      *                        NCHDIM,NFREQ,IFREQ,ISPDONE,INIT_TOMAWAC,
C      *                        PIEDS_TOMAWAC,PIEDS_TOMAWAC_MPI,
C      *                        WIPE_HEAPED_CHAR,PREP_INITIAL_SEND,
C      *                        GLOB_CHAR_COMM
!BD_INCKA END OF MODIFICATION
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
      INTEGER NRK,NPOIN3,NPOIN2,NELEM2,NPLAN,NF
C
      DOUBLE PRECISION CX(NPOIN3,NF) , CY(NPOIN3,NF)
      DOUBLE PRECISION CT(NPOIN3,NF) , CF(NPOIN3,NF)
      DOUBLE PRECISION SHP1(NPOIN3,NF) , SHP2(NPOIN3,NF)
      DOUBLE PRECISION SHP3(NPOIN3,NF) , SHZ(NPOIN3,NF)
      DOUBLE PRECISION SHF(NPOIN3,NF)  , DZHDT(NPOIN2)
      DOUBLE PRECISION X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION XK(NPOIN2,NF),CG(NPOIN2,NF)
      DOUBLE PRECISION TETA(NPLAN),FREQ(NF)
      DOUBLE PRECISION SINTET(NPLAN),COSTET(NPLAN)
      DOUBLE PRECISION COSF(NPOIN2),TGF(NPOIN2)
      DOUBLE PRECISION DEPTH(NPOIN2),DZX(NPOIN2),DZY(NPOIN2)
      DOUBLE PRECISION U(NPOIN2),DUX(NPOIN2),DUY(NPOIN2)
      DOUBLE PRECISION V(NPOIN2),DVX(NPOIN2),DVY(NPOIN2)
      DOUBLE PRECISION SURDET(NELEM2)
      DOUBLE PRECISION DT,TRA01(NPOIN3,8),PROMIN
      INTEGER ELT(NPOIN3,NF),ETA(NPOIN3,NF),FRE(NPOIN3,NF)
      INTEGER IKLE2(NELEM2,3),IFABOR(NELEM2,7),ETAP1(NPLAN)
      INTEGER ITR01(NPOIN3,3)
      LOGICAL COURAN,SPHE,PROINF
C      REAL WW(1)
C
      INTEGER JF,I,ISTAT,IEL,I1,I2,I3,I4
      CHARACTER*3 CAR
!BD_INCKA MODIFICATION FOR PARALLEL MODE
      INTEGER          LAST_NOMB,NLOSTAGAIN,NUMBER,IER,NRECV,NUMBERLOST
      INTEGER          ITE,IP,IPLAN,NBB,IPOIN!,GOODELT(NPOIN2,NPLAN,NF)
      INTEGER          NARRSUM,IFF
C      INTEGER P_ISUM,P_IMAX
C      EXTERNAL P_ISUM,P_IMAX
C      DOUBLE PRECISION :: TEST2(NPOIN3,NF)
      DOUBLE PRECISION :: TES(NPOIN2,NPLAN)
      INTEGER,DIMENSION(:,:,:), ALLOCATABLE :: GOODELT
      TYPE(BIEF_MESH)  ::  MESH
!BD_INCKA END OF MODIFICATION
C
C----------------------------------------------------------------------
C
      NFREQ=NF
      ALLOCATE(GOODELT(NPOIN2,NPLAN,NF))
      IF (.NOT.COURAN) THEN
C
C   -------------------------------------------------------------------
C
C   RELATIVE = ABSOLUTE => ADVECTION IN 3D
C   SEPARATES OUT THE FREQUENCIES
C
        DO 200 JF=1,NF
C
        IF (NCSIZE.LE.1) THEN
C
C      ---------------------------------------------------------------
C
C      COMPUTES THE ADVECTION FIELD
C

         CALL CONWAC
     &( CY    , CX    , CT    , XK    , CG    , COSF  , TGF   , DEPTH ,
     &  DZY   , DZX   , FREQ  , COSTET, SINTET, NPOIN2, NPLAN , JF    ,
     &  NF    , PROINF, SPHE  , PROMIN, TRA01 , TRA01(1,2)    )
C
C      ----------------------------------------------------------------
C
C      DETERMINES THE FOOT OF THE CHARACTERISTICS
C
C
          CALL INIPIE
     &( CX,CY,CT,X,Y,SHP1(1,JF),SHP2(1,JF),SHP3(1,JF),SHZ(1,JF),
     &  ELT(1,JF),ETA(1,JF),
     &  TRA01,TRA01(1,2),TRA01(1,3),TETA,IKLE2,NPOIN2,NELEM2,NPLAN,
     &  ITR01,ITR01,ITR01,NELEM2,NPOIN2,IFABOR,GOODELT(1,1,JF))
C
      DO IEL=1,NELEM2
        I1=IKLE2(IEL,1)
        I2=IKLE2(IEL,2)
        I3=IKLE2(IEL,3)
        IF ((DEPTH(I1).LT.PROMIN).AND.(DEPTH(I2).LT.PROMIN).AND.
     &      (IFABOR(IEL,1).GT.0)) IFABOR(IEL,1)=-1
        IF ((DEPTH(I2).LT.PROMIN).AND.(DEPTH(I3).LT.PROMIN).AND.
     &      (IFABOR(IEL,2).GT.0)) IFABOR(IEL,2)=-1
        IF ((DEPTH(I3).LT.PROMIN).AND.(DEPTH(I1).LT.PROMIN).AND.
     &      (IFABOR(IEL,3).GT.0)) IFABOR(IEL,3)=-1
      ENDDO
C
          CALL PIEDS
     &(CX,CY,CT,DT,NRK,X,Y,TETA,IKLE2,IFABOR,ETAP1,TRA01,TRA01(1,2),
     &  TRA01(1,3),TRA01(1,4),TRA01(1,5),TRA01(1,6),SHP1(1,JF),
     &  SHP2(1,JF),SHP3(1,JF),SHZ(1,JF),ELT(1,JF),ETA(1,JF),
     &  ITR01(1,1),NPOIN3,NPOIN2,
     &  NELEM2,NPLAN,JF,SURDET,-1,ITR01(1,2))
C
       ELSE
C
C     READS "NEEDS FOR IP"
C
!BD_INCKA MODIFICATION FOR PARALLEL MODE
         CALL CONWAC
     &( CY , CX , CT , XK , CG , COSF,TGF , DEPTH ,
     &  DZY   , DZX   , FREQ  , COSTET, SINTET, NPOIN2, NPLAN , JF    ,
     &  NF    , PROINF, SPHE  , PROMIN, TRA01 , TRA01(1,2)    )



          CALL INIPIE
     &( CX,CY,CT,X,Y,SHP1(1,JF),SHP2(1,JF),
     &  SHP3(1,JF),SHZ(1,JF),
     &  ELT(1,JF),ETA(1,JF),
     &  TRA01,TRA01(1,2),TRA01(1,3),TETA,IKLE2,NPOIN2,NELEM2,NPLAN,
     &  ITR01,ITR01,ITR01,NELEM2,NPOIN2,IFABOR,GOODELT(1,1,JF))

         IF (JF==1) THEN
           IF (ALLOCATED(SH_LOC)) THEN
               DO IFF=1,NF
             DEALLOCATE(SH_LOC(IFF)%SHP1,SH_LOC(IFF)%SHP2,
     &                  SH_LOC(IFF)%SHP3,SH_LOC(IFF)%SHZ,
     &                  SH_LOC(IFF)%SHF,SH_LOC(IFF)%ELT,
     &                  SH_LOC(IFF)%ETA,SH_LOC(IFF)%FRE)
               ENDDO
             DEALLOCATE(SH_LOC)
           ENDIF
           LAST_NOMB = 1
         ENDIF

      DO IEL=1,NELEM2
        I1=IKLE2(IEL,1)
        I2=IKLE2(IEL,2)
        I3=IKLE2(IEL,3)
        IF ((DEPTH(I1).LT.PROMIN).AND.(DEPTH(I2).LT.PROMIN).AND.
     &      (IFABOR(IEL,1).GT.0)) IFABOR(IEL,1)=-1
        IF ((DEPTH(I2).LT.PROMIN).AND.(DEPTH(I3).LT.PROMIN).AND.
     &      (IFABOR(IEL,2).GT.0)) IFABOR(IEL,2)=-1
        IF ((DEPTH(I3).LT.PROMIN).AND.(DEPTH(I1).LT.PROMIN).AND.
     &      (IFABOR(IEL,3).GT.0)) IFABOR(IEL,3)=-1
        IF ((DEPTH(I1).LT.PROMIN).AND.(DEPTH(I2).LT.PROMIN).AND.
     &      (IFABOR(IEL,1).EQ.-2)) IFABOR(IEL,1)=-1
        IF ((DEPTH(I2).LT.PROMIN).AND.(DEPTH(I3).LT.PROMIN).AND.
     &      (IFABOR(IEL,2).EQ.-2)) IFABOR(IEL,2)=-1
        IF ((DEPTH(I3).LT.PROMIN).AND.(DEPTH(I1).LT.PROMIN).AND.
     &      (IFABOR(IEL,3).EQ.-2)) IFABOR(IEL,3)=-1
      ENDDO



         CALL CORRECT_GOODELT(GOODELT(1,1,JF),NPOIN2,NPLAN,MESH)
C
         IF (.NOT.ALLOCATED(NCHARA)) ALLOCATE(NCHARA(NF),NLOSTCHAR(NF),
     &                                        NSEND(NF))
         CALL INIT_TOMAWAC(NCHARA(JF),NCHDIM,1,
     &                                       NPOIN3,LAST_NOMB)

C
         IF(.NOT.ALLOCATED(TEST)) ALLOCATE(TEST(NPOIN3,NF))
         IFREQ=JF

           CALL PIEDS_TOMAWAC
     &(CX,CY,CT,DT,NRK,X,Y,TETA,IKLE2,IFABOR,
     &  ETAP1,TRA01,TRA01(1,2),
     &  TRA01(1,3),TRA01(1,4),TRA01(1,5),TRA01(1,6),SHP1(1,JF),
     &  SHP2(1,JF),SHP3(1,JF),SHZ(1,JF),ELT(1,JF),ETA(1,JF),
     &  ITR01(1,1),NPOIN3,NPOIN2,
     &  NELEM2,NPLAN,JF,SURDET,-1,ITR01(1,2),MESH%IFAPAR%I,TEST(1,JF),
     &  NCHDIM,NCHARA(JF),MESH,GOODELT(1,1,JF))
C CHECKS WHETHER A CHARACTERISTICS CLOSE TO THE BOUNDARY EXITS AND NOT
C THE OTHER ONE. IN THIS CASE ONLY THE MAXIMUM CONTRIBUTION (FROM BOTH)
C IS CONSIDERED AND THE EXIT CHARACTERISTICS IS NOT TREATED
          DO IP = 1,NPOIN2
              DO IPLAN = 1,NPLAN
                 TES(IP,IPLAN)  =TEST(IP+NPOIN2*(IPLAN-1),JF)
              ENDDO
          ENDDO
         WHERE (TEST(:,JF).LT.0.5D0)
             SHP1(:,JF)=0.D0
             SHP2(:,JF)=0.D0
             SHP3(:,JF)=0.D0
             SHZ(:,JF) = 0.D0
         END WHERE
          DO IPLAN = 1,NPLAN
          CALL PARCOM2
     & ( TES(1,IPLAN) ,
     &   TES(1,IPLAN) ,
     &   TES(1,IPLAN) ,
     &   NPOIN2 , 1 , 2 , 1 , MESH )
          ENDDO
          DO IP = 1,NPOIN2
             DO IPLAN = 1,NPLAN
                TEST(IP+NPOIN2*(IPLAN-1),JF)=TES(IP,IPLAN)
             ENDDO
          ENDDO
C          WHERE (TEST(:,JF).GT.1.5D0)
C             SHP1(:,JF)=SHP1(:,JF)/TEST(:,JF)
C             SHP2(:,JF)=SHP2(:,JF)/TEST(:,JF)
C             SHP3(:,JF)=SHP3(:,JF)/TEST(:,JF)
C          END WHERE
C HEAPCHAR(NCHARA,NFREQ) AND HEAPCOUNT(NCSIZE,NFREQ)
C HEAPCOUNT=> NUMBER OF CHARACTERISTICS ON EACH PROCESSOR
         CALL WIPE_HEAPED_CHAR(TEST(1,JF),NPOIN3,.TRUE.,NSEND(JF),
     &                        NLOSTCHAR(JF),NCHDIM,
     &                        NCHARA(JF))

C IS NOT NECESSARILY USEFUL, CHECKS IF TEST==1, IN WHICH CASE IT IS DELETED
C FROM THE LIST OF CHARACTERISTICS BY ASSIGNING HEAPCAHR%NEPID==-1
C        DO WHILE(P_IMAX(NLOSTCHAR(JF))>0)! THERE ARE -REALLY- LOST TRACEBACKS SOMEWHERE
          CALL PREP_INITIAL_SEND(NSEND,NLOSTCHAR,NCHARA)

C CREATES THE ARRAY 'SDISP' AND ORDERS THE DATA (ASCENDING)
          CALL GLOB_CHAR_COMM ()
C SENDS SENDCHAR AND WRITES TO RECVCHAR


!
         IF(.NOT.ALLOCATED(ISPDONE)) ALLOCATE(ISPDONE(NPOIN3,NF))
         IF(.NOT.ALLOCATED(NARRV)) ALLOCATE(NARRV(NF))
         CALL ALLOC_LOCAL(NARRV(IFREQ),IFREQ,NF,NLOSTAGAIN,
     &                      NUMBERLOST,NARRSUM)

         IF (NUMBERLOST>0) THEN

       CALL PIEDS_TOMAWAC_MPI
     &(CX,CY,CT,DT,NRK,X,Y,TETA,IKLE2,IFABOR,ETAP1,
     &  TRA01,TRA01(1,2),
     &  TRA01(1,3),TRA01(1,4),TRA01(1,5),TRA01(1,6),SH_LOC(JF)%SHP1,
     &  SH_LOC(JF)%SHP2,SH_LOC(JF)%SHP3,SH_LOC(JF)%SHZ,
     &  SH_LOC(JF)%ELT,SH_LOC(JF)%ETA,
     &  NARRV(JF),NPOIN2,
     &  NELEM2,NPLAN,JF,SURDET,-1,MESH%IFAPAR%I,
     &  2,NCHARA(JF),RECVCHAR(1,JF))

        CALL ALLOC_AGAIN(NARRV(IFREQ),IFREQ,NLOSTAGAIN,NUMBERLOST,
     &                   NUMBER)
        CALL ORGANIZE_SENDAGAIN()

        CALL SUPP_ENVOI_AGAIN(IFREQ,NUMBER)

!
           ITE = 0
          DO WHILE((NUMBERLOST>0).AND.(ITE.LE.20))
           ITE= ITE + 1
          CALL ORGANIZE_SENDAGAIN()
          CALL ENVOI_AGAIN(NRECV)

          CALL PIEDS_TOMAWAC_MPI
     &(CX,CY,CT,DT,NRK,X,Y,TETA,IKLE2,IFABOR,
     &  ETAP1,TRA01,TRA01(1,2),
     &  TRA01(1,3),TRA01(1,4),TRA01(1,5),TRA01(1,6),SH_AGAIN%SHP1,
     &  SH_AGAIN%SHP2,SH_AGAIN%SHP3,SH_AGAIN%SHZ,
     &  SH_AGAIN%ELT,SH_AGAIN%ETA,
     &  NRECV,NPOIN2,
     &  NELEM2,NPLAN,JF,SURDET,-1,MESH%IFAPAR%I,
     &  2,NCHARA(JF),RECVAGAIN)
        CALL INCREM_ENVOI_RECV(IFREQ,NUMBER,NLOSTAGAIN,NUMBERLOST,
     &                         NRECV)
        ENDDO ! END OF THE DOWHILE LOOP
         CALL FINAL_ORGA_RECV(NARRV(IFREQ),IFREQ)
          ELSE
           CALL RESET_COUNT(IFREQ)
          ENDIF
C
       ENDIF
C
200   CONTINUE
C
      ELSE
C
C   ---------------------------------------------------------------
C
C   IN A RELATIVE REFERENCE SYSTEM => ADVECTION IN 4D
C   IT IS NO LONGER POSSIBLE TO SEPARATE THE FREQUENCIES OUT
C$
C       IF (IPID.GT.1) THEN
C        IF(LNG.EQ.1) THEN
C         WRITE(LU,*) 'PREPRO : PAS D EXECUTION EN PARALLELE AVEC COURANT'
C        ELSE
C         WRITE(LU,*) 'PREPRO : NO PARALLELISM WITH CURRENT'
C        ENDIF
C       STOP
C       ENDIF
C
      DO JF=1,NF
         CALL CONW4D
     &(CY,CX,CT,CF,V,U,XK,CG,COSF,TGF,DEPTH,DZHDT,DZY,DZX,DVY,DVX,
     & DUY,DUX,FREQ,COSTET,SINTET,NPOIN2,NPLAN,JF,NF,PROINF,SPHE,
     & COURAN,TRA01,TRA01(1,2))
      ENDDO
C
       IF (NCSIZE.LE.1) THEN
        DO JF=1,NF
       CALL INIP4D
     &(CX(1,JF),CY(1,JF),CT(1,JF),CF(1,JF),X,Y,
     & SHP1(1,JF),SHP2(1,JF),SHP3(1,JF),
     & SHZ(1,JF),SHF(1,JF),ELT(1,JF),ETA(1,JF),FRE(1,JF),
     & TRA01,TRA01(1,2),TRA01(1,3),TRA01(1,4),TETA,FREQ,IKLE2,
     & NPOIN2,NELEM2,NPLAN,JF,NF,IFABOR,GOODELT(1,1,JF))
C
         WRITE(LU,*) 'FREQUENCE :',JF
         CALL PIED4D
     &(CX,CY,CT,CF,DT,NRK,X,Y,TETA,FREQ,IKLE2,IFABOR,ETAP1,
     & TRA01,TRA01(1,2),TRA01(1,3),TRA01(1,4),TRA01(1,5),
     & TRA01(1,6),TRA01(1,7),TRA01(1,8),SHP1(1,JF),
     & SHP2(1,JF),SHP3(1,JF),SHZ(1,JF),SHF(1,JF),
     & ELT(1,JF),ETA(1,JF),FRE(1,JF),ITR01(1,1),NPOIN3,NPOIN2,
     & NELEM2,NPLAN,NF,SURDET,-1,ITR01(1,2))
        ENDDO
C
       ELSE

         DO JF=1,NF
       CALL INIP4D
     &(CX(1,JF),CY(1,JF),CT(1,JF),CF(1,JF),X,Y,
     & SHP1(1,JF),SHP2(1,JF),SHP3(1,JF),
     & SHZ(1,JF),SHF(1,JF),ELT(1,JF),ETA(1,JF),FRE(1,JF),
     & TRA01,TRA01(1,2),TRA01(1,3),TRA01(1,4),TETA,FREQ,IKLE2,
     & NPOIN2,NELEM2,NPLAN,JF,NF,IFABOR,GOODELT(1,1,JF))
         WRITE(LU,*) 'FREQUENCE :',JF
         IF (JF==1) THEN
           IF (ALLOCATED(SH_LOC_4D)) THEN
               DO IFF=1,NF
             DEALLOCATE(SH_LOC_4D(IFF)%SHP1,SH_LOC_4D(IFF)%SHP2,
     &                  SH_LOC_4D(IFF)%SHP3,SH_LOC_4D(IFF)%SHZ,
     &                  SH_LOC_4D(IFF)%SHF,SH_LOC_4D(IFF)%ELT,
     &                  SH_LOC_4D(IFF)%ETA,SH_LOC_4D(IFF)%FRE)
               ENDDO
             DEALLOCATE(SH_LOC_4D)
           ENDIF
           LAST_NOMB = 1
         ENDIF


         CALL CORRECT_GOODELT(GOODELT(1,1,JF),NPOIN2,NPLAN,MESH)
C
         IF (.NOT.ALLOCATED(NCHARA)) ALLOCATE(NCHARA(NF),NLOSTCHAR(NF),
     &                                        NSEND(NF))
         CALL INIT_TOMAWAC_4D(NCHARA(JF),NCHDIM,1,
     &                                       NPOIN3,LAST_NOMB)
C
         IF(.NOT.ALLOCATED(TEST)) ALLOCATE(TEST(NPOIN3,NF))
         IFREQ=JF


           CALL PIED4D_TOMAWAC
     &(CX,CY,CT,CF,DT,NRK,X,Y,TETA,FREQ,IKLE2,IFABOR,
     &  ETAP1,TRA01,TRA01(1,2),
     &  TRA01(1,3),TRA01(1,4),TRA01(1,5),TRA01(1,6),
     &  TRA01(1,7),TRA01(1,8),SHP1(1,JF),SHP2(1,JF),SHP3(1,JF),
     &  SHZ(1,JF),SHF(1,JF),ELT(1,JF),ETA(1,JF),FRE(1,JF),
     &  ITR01(1,1),NPOIN3,NPOIN2,
     &  NELEM2,NPLAN,NF,SURDET,-1,ITR01(1,2),MESH%IFAPAR%I,TEST(1,JF),
     &  NCHDIM,NCHARA(JF),MESH,GOODELT(1,1,JF),JF)

C CHECKS WHETHER A CHARACTERISTICS CLOSE TO THE BOUNDARY EXITS AND NOT
C THE OTHER ONE. IN THIS CASE ONLY THE MAXIMUM CONTRIBUTION (FROM BOTH)
C IS CONSIDERED AND THE EXIT CHARACTERISTICS IS NOT TREATED

          DO IP = 1,NPOIN2
             DO IPLAN = 1,NPLAN
                TES(IP,IPLAN)  =TEST(IP+NPOIN2*(IPLAN-1),JF)
             ENDDO
          ENDDO
         WHERE (TEST(:,JF).LT.0.5D0)
             SHP1(:,JF)=0.D0
             SHP2(:,JF)=0.D0
             SHP3(:,JF)=0.D0
             SHZ(:,JF) = 0.D0
             SHF(:,JF) = 0.D0
         END WHERE
          DO IPLAN = 1,NPLAN
          CALL PARCOM2
     & ( TES(1,IPLAN) ,
     &   TES(1,IPLAN) ,
     &   TES(1,IPLAN) ,
     &   NPOIN2 , 1 , 2 , 1 , MESH )
          ENDDO
          DO IP = 1,NPOIN2
             DO IPLAN = 1,NPLAN
                TEST(IP+NPOIN2*(IPLAN-1),JF)=TES(IP,IPLAN)
             ENDDO
          ENDDO
C          WHERE (TEST(:,JF).GT.1.5D0)
C             SHP1(:,JF)=SHP1(:,JF)/TEST(:,JF)
C             SHP2(:,JF)=SHP2(:,JF)/TEST(:,JF)
C             SHP3(:,JF)=SHP3(:,JF)/TEST(:,JF)
C          END WHERE

C HEAPCHAR(NCHARA,NFREQ) AND HEAPCOUNT(NCSIZE,NFREQ)
C HEAPCOUNT=> NUMBER OF CHARACTERISTICS ON EACH PROCESSOR
         CALL WIPE_HEAPED_CHAR_4D(TEST(1,JF),NPOIN3,.TRUE.,NSEND(JF),
     &                        NLOSTCHAR(JF),NCHDIM,
     &                        NCHARA(JF))
C IS NOT NECESSARILY USEFUL, CHECKS IF TEST==1, IN WHICH CASE IT IS DELETED
C FROM THE LIST OF CHARACTERISTICS BY ASSIGNING HEAPCAHR%NEPID==-1
C        DO WHILE(P_IMAX(NLOSTCHAR(JF))>0)! THERE ARE -REALLY- LOST TRACEBACKS SOMEWHERE
          CALL PREP_INITIAL_SEND_4D(NSEND,NLOSTCHAR,NCHARA)

C CREATES THE ARRAY 'SDISP' AND ORDERS THE DATA (ASCENDING)
          CALL GLOB_CHAR_COMM_4D ()

C SENDS SENDCHAR AND WRITES TO RECVCHAR
!
         IF(.NOT.ALLOCATED(ISPDONE)) ALLOCATE(ISPDONE(NPOIN3,NF))
         IF(.NOT.ALLOCATED(NARRV)) ALLOCATE(NARRV(NF))
         CALL ALLOC_LOCAL_4D(NARRV(IFREQ),IFREQ,NF,NLOSTAGAIN,
     &                      NUMBERLOST,NARRSUM)


         IF (NUMBERLOST>0) THEN

       CALL PIEDS4D_TOMAWAC_MPI
     &(CX,CY,CT,CF,DT,NRK,X,Y,TETA,FREQ,IKLE2,IFABOR,ETAP1,
     &  TRA01,TRA01(1,2),
     &  TRA01(1,3),TRA01(1,4),TRA01(1,5),TRA01(1,6),TRA01(1,7),
     &  TRA01(1,8),SH_LOC_4D(JF)%SHP1,SH_LOC_4D(JF)%SHP2,
     &  SH_LOC_4D(JF)%SHP3,SH_LOC_4D(JF)%SHZ,SH_LOC_4D(JF)%SHF,
     &  SH_LOC_4D(JF)%ELT,SH_LOC_4D(JF)%ETA,SH_LOC_4D(JF)%FRE,
     &  NARRV(JF),NPOIN2,
     &  NELEM2,NPLAN,NF,JF,SURDET,-1,MESH%IFAPAR%I,
     &  2,NCHARA(JF),RECVCHAR_4D(1,JF))
        CALL ALLOC_AGAIN_4D(NARRV(IFREQ),IFREQ,NLOSTAGAIN,NUMBERLOST,
     &                   NUMBER)
        CALL ORGANIZE_SENDAGAIN_4D()
        CALL SUPP_ENVOI_AGAIN_4D(IFREQ,NUMBER)
!
        ITE = 0
        DO WHILE((NUMBERLOST>0).AND.(ITE.LE.20))
          ITE= ITE + 1
          CALL ORGANIZE_SENDAGAIN_4D()
          CALL ENVOI_AGAIN_4D(NRECV)

          CALL PIEDS4D_TOMAWAC_MPI
     &(CX,CY,CT,CF,DT,NRK,X,Y,TETA,FREQ,IKLE2,IFABOR,
     &  ETAP1,TRA01,TRA01(1,2),
     &  TRA01(1,3),TRA01(1,4),TRA01(1,5),TRA01(1,6),TRA01(1,7),
     &  TRA01(1,8),SH_AGAIN_4D%SHP1,SH_AGAIN_4D%SHP2,SH_AGAIN_4D%SHP3,
     &  SH_AGAIN_4D%SHZ,SH_AGAIN_4D%SHF,
     &  SH_AGAIN_4D%ELT,SH_AGAIN_4D%ETA,SH_AGAIN_4D%FRE,
     &  NRECV,NPOIN2,
     &  NELEM2,NPLAN,NF,JF,SURDET,-1,MESH%IFAPAR%I,
     &  2,NCHARA(JF),RECVAGAIN_4D)

         CALL INCREM_ENVOI_RECV_4D(IFREQ,NUMBER,NLOSTAGAIN,NUMBERLOST,
     &                         NRECV)
        ENDDO ! END OF THE DOWHILE LOOP
        CALL FINAL_ORGA_RECV_4D(NARRV(IFREQ),IFREQ)
!
          ELSE
           CALL RESET_COUNT(IFREQ)
          ENDIF
!
         ENDDO
       ENDIF
      ENDIF
C
      DEALLOCATE(GOODELT)
C
C----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C