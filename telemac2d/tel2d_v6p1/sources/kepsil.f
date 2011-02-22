C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       DIFFUSION STEP FOR SOURCE TERMS (K-EPSILON MODEL).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AK, AKN, AKTILD, C1, C2, CF, CM2, CMU, DT, EBOR, EMAX, EMIN, EP, EPN, EPTILD, ESTAR, HN, ICONV, IELME, IELMK, INFOKE, KBOR, KDIR, KMAX, KMIN, LIMKEP, MAE, MAK, MASKEL, MASKPT, MESH, MSK, NPTFR, OPTSUP, S, SCHMIT, SIGMAE, SIGMAK, SLVEP, SLVK, SME, SMK, T1, T2, T3, TB, TE1, TE2, TM1, U, UCONV, V, VCONV, VISC
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AGGLOE, AGGLOK, C, CEPS, N, SL1, TETAK, USTAR
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CLIP(), CPSTVC(), DIRICH(), KSUPG(), LUMP(), MATRIX(), MATVEC(), OM(), OS(), PLANTE(), SOLVE(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D()

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
!>      <td><center>                                           </center>
!> </td><td> 30/05/1994
!> </td><td> L. VAN HAREN (LNH) 30 87 84 14
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.5                                       </center>
!> </td><td> 27/11/1992
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AK
!></td><td><--</td><td>ENERGIE TURBULENTE AU TEMPS T(N+1)
!>    </td></tr>
!>          <tr><td>AKN
!></td><td>--></td><td>ENERGIE TURBULENTE AU TEMPS T(N)
!>    </td></tr>
!>          <tr><td>AKTILD
!></td><td>--></td><td>ENERGIE TURBULENTE APRES CONVECTION
!>    </td></tr>
!>          <tr><td>C1,C2
!></td><td>--></td><td>CONSTANTES DU MODELE K-EPSILON
!>    </td></tr>
!>          <tr><td>CF
!></td><td>--></td><td>COEFFICIENT DE FROTTEMENT POUR K-EPSILON
!>    </td></tr>
!>          <tr><td>CM2
!></td><td>---</td><td>MATRIX
!>    </td></tr>
!>          <tr><td>CMU
!></td><td>--></td><td>CONSTANTE DU MODELE K-EPSILON
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>EMIN,EMAX
!></td><td>--></td><td>EPSILON MINIMUM ET MAXIMUM EN CAS DE CLIPPING
!>    </td></tr>
!>          <tr><td>EP
!></td><td><--</td><td>DISSIPATION TURBULENTE AU TEMPS T(N+1)
!>    </td></tr>
!>          <tr><td>EPN
!></td><td>--></td><td>DISSIPATION TURBULENTE AU TEMPS T(N)
!>    </td></tr>
!>          <tr><td>EPTILD
!></td><td>--></td><td>DISSIPATION TURBULENTE APRES CONVECTION
!>    </td></tr>
!>          <tr><td>ESTAR
!></td><td>--></td><td>CONSTANTE DU MODELE K-EPSILON
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>HAUTEUR AU TEMPS N
!>    </td></tr>
!>          <tr><td>ICONV
!></td><td>--></td><td>TYPE OF ADVECTION ON K AND EPSILON
!>                  1 : CHARACTERISTICS
!>                  2 : SUPG
!>    </td></tr>
!>          <tr><td>IELME
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELMK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>INFOKE
!></td><td>--></td><td>LOGIQUE INDIQUANT SI LES INFORMATIONS SUR LE
!>                  SOLVEUR SONT A RESTITUER
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td>--></td><td>CONSTANTE DE KARMAN
!>    </td></tr>
!>          <tr><td>KBOR,EBOR
!></td><td>--></td><td>K ET EPSILON IMPOSES AU BORD
!>    </td></tr>
!>          <tr><td>KDDL
!></td><td>--></td><td>CONDITION A LA LIMITE DE TYPE DEGRE DE LIBERTE
!>    </td></tr>
!>          <tr><td>KDIR
!></td><td>--></td><td>CONDITION A LA LIMITE DE TYPE DIRICHLET
!>    </td></tr>
!>          <tr><td>KMIN,KMAX
!></td><td>--></td><td>K MINIMUM ET MAXIMUM EN CAS DE CLIPPING
!>    </td></tr>
!>          <tr><td>KNEU
!></td><td>--></td><td>CONDITION A LA LIMITE DE TYPE NEUMANN
!>    </td></tr>
!>          <tr><td>LIMKEP
!></td><td>--></td><td>CONDITIONS AUX LIMITES SUR K ET EPSILON
!>    </td></tr>
!>          <tr><td>MAE
!></td><td>---</td><td>MATRICE DU SYSTEME A RESOUDRE POUR E
!>    </td></tr>
!>          <tr><td>MAK
!></td><td>---</td><td>MATRICE DU SYSTEME A RESOUDRE POUR K
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES ELEMENTS
!>                  =1. : NORMAL   =0. : ELEMENT MASQUE
!>    </td></tr>
!>          <tr><td>MASKPT
!></td><td>--></td><td>MASQUES PAR POINTS.
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES.
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERES
!>    </td></tr>
!>          <tr><td>OPTSUP
!></td><td>--></td><td>SUPG OPTION
!>    </td></tr>
!>          <tr><td>S
!></td><td>--></td><td>STRUCTURE BIDON
!>    </td></tr>
!>          <tr><td>SCHMIT
!></td><td>--></td><td>CONSTANTE DU MODELE K-EPSILON
!>    </td></tr>
!>          <tr><td>SIGMAE
!></td><td>--></td><td>CONSTANTE DU MODELE K-EPSILON
!>    </td></tr>
!>          <tr><td>SIGMAK
!></td><td>--></td><td>CONSTANTE DU MODELE K-EPSILON
!>    </td></tr>
!>          <tr><td>SLVEP
!></td><td>--></td><td>STRUCTURE WITH SOLVER OPTIONS FOR E
!>    </td></tr>
!>          <tr><td>SLVK
!></td><td>--></td><td>STRUCTURE WITH SOLVER OPTIONS FOR K
!>    </td></tr>
!>          <tr><td>SME
!></td><td>---</td><td>SECOND MEMBRE DU SYSTEME A RESOUDRE POUR E
!>    </td></tr>
!>          <tr><td>SMK
!></td><td>---</td><td>SECOND MEMBRE DU SYSTEME A RESOUDRE POUR K
!>    </td></tr>
!>          <tr><td>T1,2,3,4
!></td><td>---</td><td>TABLEAUX DE TRAVAIL
!>    </td></tr>
!>          <tr><td>T2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TE1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TE2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TM1
!></td><td>---</td><td>MATRICE DE DIFFUSION
!>    </td></tr>
!>          <tr><td>U , V
!></td><td>--></td><td>COMPOSANTES DE LA VITESSE
!>    </td></tr>
!>          <tr><td>UCONV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VCONV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VISC
!></td><td>--></td><td>DIFFUSION TURBULENTE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE KEPSIL
     &(AK,EP,AKTILD,EPTILD,AKN,EPN,VISC,CF,U,V,HN,UCONV,VCONV,
     & KBOR,EBOR,LIMKEP,IELMK,IELME,
     & SMK,SME,TM1,MAK,MAE,CM2,TE1,TE2,NPTFR,DT,
     & MESH,T1,T2,T3,TB,CMU,C1,C2,SIGMAK,SIGMAE,ESTAR,SCHMIT,
     & KMIN,KMAX,EMIN,EMAX,
     & INFOKE,KDIR,MSK,MASKEL,MASKPT,S,SLVK,SLVEP,ICONV,OPTSUP)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AK             |<--| ENERGIE TURBULENTE AU TEMPS T(N+1)
C| AKN            |-->| ENERGIE TURBULENTE AU TEMPS T(N)
C| AKTILD         |-->| ENERGIE TURBULENTE APRES CONVECTION
C| C1,C2          |-->| CONSTANTES DU MODELE K-EPSILON
C| CF             |-->| COEFFICIENT DE FROTTEMENT POUR K-EPSILON
C| CM2            |---| MATRIX
C| CMU            |-->| CONSTANTE DU MODELE K-EPSILON
C| DT             |-->| PAS DE TEMPS
C| EMIN,EMAX      |-->| EPSILON MINIMUM ET MAXIMUM EN CAS DE CLIPPING
C| EP             |<--| DISSIPATION TURBULENTE AU TEMPS T(N+1)
C| EPN            |-->| DISSIPATION TURBULENTE AU TEMPS T(N)
C| EPTILD         |-->| DISSIPATION TURBULENTE APRES CONVECTION
C| ESTAR          |-->| CONSTANTE DU MODELE K-EPSILON
C| HN             |-->| HAUTEUR AU TEMPS N
C| ICONV          |-->| TYPE OF ADVECTION ON K AND EPSILON
C|                |   | 1 : CHARACTERISTICS
C|                |   | 2 : SUPG
C| IELME          |---| 
C| IELMK          |---| 
C| INFOKE         |-->| LOGIQUE INDIQUANT SI LES INFORMATIONS SUR LE
C|                |   | SOLVEUR SONT A RESTITUER
C| KARMAN         |-->| CONSTANTE DE KARMAN
C| KBOR,EBOR      |-->| K ET EPSILON IMPOSES AU BORD
C| KDDL           |-->| CONDITION A LA LIMITE DE TYPE DEGRE DE LIBERTE
C| KDIR           |-->| CONDITION A LA LIMITE DE TYPE DIRICHLET
C| KMIN,KMAX      |-->| K MINIMUM ET MAXIMUM EN CAS DE CLIPPING
C| KNEU           |-->| CONDITION A LA LIMITE DE TYPE NEUMANN
C| LIMKEP         |-->| CONDITIONS AUX LIMITES SUR K ET EPSILON
C| MAE            |---| MATRICE DU SYSTEME A RESOUDRE POUR E
C| MAK            |---| MATRICE DU SYSTEME A RESOUDRE POUR K
C| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
C|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE
C| MASKPT         |-->| MASQUES PAR POINTS.
C| MESH           |---| 
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES
C| OPTSUP         |-->| SUPG OPTION
C| S             |-->| STRUCTURE BIDON
C| SCHMIT         |-->| CONSTANTE DU MODELE K-EPSILON
C| SIGMAE         |-->| CONSTANTE DU MODELE K-EPSILON
C| SIGMAK         |-->| CONSTANTE DU MODELE K-EPSILON
C| SLVEP          |-->| STRUCTURE WITH SOLVER OPTIONS FOR E
C| SLVK           |-->| STRUCTURE WITH SOLVER OPTIONS FOR K
C| SME            |---| SECOND MEMBRE DU SYSTEME A RESOUDRE POUR E
C| SMK            |---| SECOND MEMBRE DU SYSTEME A RESOUDRE POUR K
C| T1,2,3,4       |---| TABLEAUX DE TRAVAIL
C| T2             |---| 
C| T3             |---| 
C| TB             |---| 
C| TE1            |---| 
C| TE2            |---| 
C| TM1            |---| MATRICE DE DIFFUSION
C| U , V          |-->| COMPOSANTES DE LA VITESSE
C| UCONV          |---| 
C| VCONV          |---| 
C| VISC           |-->| DIFFUSION TURBULENTE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(SLVCFG), INTENT(INOUT)  :: SLVK,SLVEP
      INTEGER, INTENT(IN)          :: ICONV,NPTFR,KDIR,LIMKEP(NPTFR,2)
      INTEGER, INTENT(IN)          :: OPTSUP,IELMK,IELME
      LOGICAL, INTENT(IN)          :: INFOKE,MSK
      DOUBLE PRECISION, INTENT(IN) :: KMIN,KMAX,EMIN,EMAX,SCHMIT
      DOUBLE PRECISION, INTENT(IN) :: CMU,C1,C2,SIGMAK,SIGMAE,ESTAR
      DOUBLE PRECISION, INTENT(IN) :: DT
C     MATRIX STRUCTURES
      TYPE(BIEF_OBJ), INTENT(INOUT) :: TM1,MAK,MAE,CM2
C     VECTOR STRUCTURES
      TYPE(BIEF_OBJ), INTENT(IN)    :: UCONV,VCONV,AKN,EPN,AKTILD,EPTILD
      TYPE(BIEF_OBJ), INTENT(IN)    :: HN,VISC,U,V,MASKEL,S,MASKPT,CF
      TYPE(BIEF_OBJ), INTENT(IN)    :: KBOR,EBOR
      TYPE(BIEF_OBJ), INTENT(INOUT) :: T1,T2,T3,AK,EP,SMK,SME,TE1,TE2
C     MESH STRUCTURE
      TYPE(BIEF_MESH) :: MESH
C     BLOCK STRUCTURE
      TYPE(BIEF_OBJ) :: TB
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION C,SL1,CEPS,USTAR,AGGLOK,AGGLOE,TETAK
C
      INTEGER N
C
C-----------------------------------------------------------------------
C
      INTRINSIC SQRT,MAX
C
C-----------------------------------------------------------------------
C
C     COMPUTES THE MASS MATRIX AND THE DIFFUSION MATRIX
C
      SL1 = 1.D0/DT
C
C     -----------------------------
C     COMPUTES THE MASS MATRIX
C     -----------------------------
C
      CALL MATRIX(MAK,'M=N     ','MATMAS          ',IELMK,IELMK,
     &            SL1,S,S,S,S,S,S,MESH,MSK,MASKEL)
      CALL MATRIX(MAE,'M=N     ','MATMAS          ',IELME,IELME,
     &            SL1,S,S,S,S,S,S,MESH,MSK,MASKEL)
C
C     MASS-LUMPING TEST
C
      AGGLOK = 1.D0
      AGGLOE = 1.D0
      IF(AGGLOK.GT.0.001D0) THEN
        CALL LUMP(T1,MAK,MESH,AGGLOK)
        CALL OM( 'M=CN    ' , MAK , MAK , S  , 1.D0-AGGLOK , MESH )
        CALL OM( 'M=M+D   ' , MAK , MAK , T1 , C           , MESH )
      ENDIF
      IF(AGGLOE.GT.0.001D0) THEN
        CALL LUMP(T1,MAE,MESH,AGGLOE)
        CALL OM( 'M=CN    ' , MAE , MAE , S  , 1.D0-AGGLOE , MESH )
        CALL OM( 'M=M+D   ' , MAE , MAE , T1 , C           , MESH )
      ENDIF
C
C     --------------------------------------------------
C     CONCATENATES THE MASS MATRIX: IN T3
C     --------------------------------------------------
C
      CALL LUMP(T3,MAK,MESH,DT)
C
C     ---------------------
C     DIFFUSION MATRIX
C     ---------------------
C
      CALL MATRIX(TM1,'M=N     ','MATDIF          ',IELMK,IELMK,
     &            1.D0,S,S,S,VISC,VISC,VISC,MESH,MSK,MASKEL)
C
C***********************************************************************
C
C     EXPLICIT SOURCE TERMS: T1 FOR K, T2 FOR EPSILON                  *
C                                                                      *
C     EXPLICIT TERM FOR K :                                            *
C                                            3                         *
C                               N           U                          *
C                              K             *
C                              --   +  C  * --  +  PROD
C                              DT       K   H
C
C
C     EXPLICIT TERM FOR EPSILON:
C
C                                            4
C                                N          U              N
C                              EP            *           EP
C                              --   +  C  * --  +  C   * -- * PROD
C                              DT       E    2      E1    N
C                                           H            K
C
C
C                     2        2           2
C                  DU       DV     DU   DV           N
C      PROD = ( 2*(--) + 2*(--) + (-- + --)  ) * VISC
C                  DX       DY     DY   DX
C
C
C                           N
C                         EP
C      THE TERM  +  C1  * -- * PROD   IS WRITTEN AS :
C                          N
C                         K
C
C                               N
C                   C1 * CMU * K * PROD / VISC
C
C***********************************************************************
C
C     --------------------------------
C     TAKES ADVECTION INTO ACCOUNT
C     --------------------------------
C
      IF(ICONV.EQ.1) THEN
C
        CALL MATVEC('X=AY    ',SMK,MAK,AKTILD,C,MESH)
        CALL MATVEC('X=AY    ',SME,MAE,EPTILD,C,MESH)
C
      ELSEIF(ICONV.EQ.2) THEN
C
        CALL MATVEC('X=AY    ',SMK,MAK,AKN,C,MESH)
        CALL MATVEC('X=AY    ',SME,MAE,EPN,C,MESH)
C       CENTERED SEMI-IMPLICIT ADVECTION TERM : MATRIX
        CALL MATRIX(CM2,'M=N     ','MATVGR          ',IELMK,IELMK,
     &              1.D0,S,S,S,UCONV,VCONV,VCONV,MESH,MSK,MASKEL)
C       SUPG CONTRIBUTION
        IF(OPTSUP.EQ.1) THEN
C         CLASSICAL SUPG
          CALL KSUPG(TE1,TE2,1.D0,UCONV,VCONV,MESH)
          CALL MATRIX(CM2,'M=M+N   ','MASUPG          ',IELMK,IELMK,
     &                1.D0,TE1,TE2,S,UCONV,VCONV,VCONV,
     &                MESH,MSK,MASKEL)
        ELSEIF(OPTSUP.EQ.2) THEN
C         MODIFIED SUPG
          CALL MATRIX(CM2,'M=M+N   ','MAUGUG          ',IELMK,IELMK,
     &                0.5D0*DT,S,S,S,UCONV,VCONV,VCONV,
     &                MESH,MSK,MASKEL)
        ENDIF
C       END OF SUPG CONTRIBUTION
C       EXPLICIT RIGHT HAND SIDES
        TETAK=0.6
        CALL MATVEC( 'X=X+CAY ',SMK,CM2,AKN,TETAK-1.D0,MESH)
        CALL MATVEC( 'X=X+CAY ',SME,CM2,EPN,TETAK-1.D0,MESH)
C       ADDS SUPG MATRIX TO MAK AND MAE
        CALL OM( 'M=X(M)  ' , MAK , MAK , S , C , MESH )
        CALL OM( 'M=M+CN  ' , MAK , CM2 , S , TETAK , MESH )
        CALL OM( 'M=X(M)  ' , MAE , MAE , S , C , MESH )
        CALL OM( 'M=M+CN  ' , MAE , CM2 , S , TETAK , MESH )
C
      ELSE
C
        IF(LNG.EQ.1) WRITE(LU,100) ICONV
        IF(LNG.EQ.2) WRITE(LU,101) ICONV
100     FORMAT(1X,'KEPSIL : FORME DE LA CONVECTION INCONNUE : ',1I4)
101     FORMAT(1X,'KEPSIL: UNKNOWN TYPE OF ADVECTION:',1I4)
        CALL PLANTE(1)
        STOP
C
      ENDIF
C
C     ------------------------------------------------
C     CREATION TERM - BOTTOM FRICTION (P)
C     ------------------------------------------------
C
C     BEWARE : MISSES 1.D0/(0.5D0*CF)**0.75 TO GET TRUE CEPS
C     (TAKEN INTO ACCOUNT AFTERWARD)
C
      CEPS = C2 * SQRT(CMU) / SQRT( ESTAR*SCHMIT )
C
      CALL CPSTVC(SMK,T1)
      CALL CPSTVC(SMK,T2)
      DO N=1,T1%DIM1
         USTAR = SQRT(0.5D0*CF%R(N)*(U%R(N)**2+V%R(N)**2))
C        T1 : PKV TERM OF THE EQUATION FOR K
         T1%R(N)=USTAR**3/MAX(SQRT(0.5D0*CF%R(N))*HN%R(N),1.D-6)
C        LIMITS THE GROWTH OF K DUE TO BOTTOM FRICTION
C        TO 50% PER TIMESTEP
         T1%R(N) = MIN (T1%R(N) , EP%R(N) + 0.5D0*AK%R(N)/DT )
C        T2 : PEV TERM OF THE EQUATION FOR EPSILON
         T2%R(N) = CEPS * USTAR**4 /
     &           MAX(((0.5D0*CF%R(N))**0.75D0)*HN%R(N)**2,1.D-6)
C        LIMITS THE GROWTH OF E TO 50% PER TIMESTEP
         T2%R(N) = MIN(T2%R(N),C2*EP%R(N)**2/MAX(AK%R(N),KMIN)
     &           +0.5D0*EP%R(N)/DT )
      ENDDO
C
      CALL OS( 'X=XY    ' , T1  , T3 , T3 , C )
      CALL OS( 'X=X+Y   ' , SMK , T1 , T1 , C )
      CALL OS( 'X=XY    ' , T2  , T3 , T3 , C )
      CALL OS( 'X=X+Y   ' , SME , T2 , T2 , C )
C
C     -----------------------------------
C     CREATION TERM - SHEAR
C     -----------------------------------
C
      CALL VECTOR(T1,'=','PRODF           ',IELMK,
     &            1.D0,S,S,S,U,V,S,MESH,MSK,MASKEL)
      CALL OS( 'X=XY    ' , T1  , VISC , VISC , C )
C
C TEST JMH : LIMITS TURBULENCE FOR SHALLOW DEPTHS ( < 2CM )
C
      DO N=1,SMK%DIM1
        IF(HN%R(N).LT.0.02D0) T1%R(N)=0.D0
      ENDDO
C
C END OF TEST
C
      CALL OS( 'X=X+Y   ' , SMK , T1   , T1   , C )
C
      CALL VECTOR(T2,'=','PRODF           ',IELMK,
     &            CMU*C1,S,S,S,U,V,S,MESH,MSK,MASKEL)
      CALL OS( 'X=XY    ' , T2  , AK , AK , C )
      CALL OS( 'X=X+Y   ' , SME , T2 , T2 , C )
C
C TEST JMH : LIMITS TURBULENCE FOR SHALLOW DEPTHS ( < 2CM )
C
      DO N=1,SMK%DIM1
        SMK%R(N) = SMK%R(N) * (MIN(HN%R(N),0.02D0)/0.02D0)**2
      ENDDO
C
C END OF TEST
C
C***********************************************************************
C     IMPLICIT SOURCE TERMS : T1 FOR K , T2 FOR EPSILON                *
C                                                                      *
C     IMPLICIT TERM FOR K :           +      EP(N)/K(N) * K (N+1)      *
C     IMPLICIT TERM FOR EPSILON:      + C2 * EP(N)/K(N) * EP(N+1)      *
C***********************************************************************
C
      CALL OS( 'X=Y/Z   ',T1,EP,AK,C  ,IOPT=2,INFINI=0.D0,ZERO=KMIN)
      CALL OS( 'X=CY    ',T2,T1,T1,C2 )
C
C     ---------------------------------------------------
C     INTEGRATES THESE SOURCE TERMS IN THE MATRICES
C     ---------------------------------------------------
C
      CALL OS( 'X=XY    ' , T1 , T3 , T3 , C )
      CALL OS( 'X=XY    ' , T2 , T3 , T3 , C )
C
C     -------------------------------------------
C     ADDS TO THE DIAGONAL OF THE MASS MATRIX
C     -------------------------------------------
C
      CALL OM( 'M=M+D   ' , MAK , MAK , T1 , C , MESH )
      CALL OM( 'M=M+D   ' , MAE , MAE , T2 , C , MESH )
C
C***********************************************************************
C
C     COMBINES THE MASS AND DIFFUSION MATRICES                         *
C                                                                      *
C     MAK = MAK + TM1/SIGMAK                                           *
C     MAE = MAE + TM1/SIGMAE                                           *
C
C***********************************************************************
C
      CALL OM( 'M=M+CN  ' , MAK , TM1 , S , 1.D0/SIGMAK , MESH )
      CALL OM( 'M=M+CN  ' , MAE , TM1 , S , 1.D0/SIGMAE , MESH )
C
C***********************************************************************
C     DIRICHLET TYPE BOUNDARY CONDITIONS
C***********************************************************************
C
      CALL DIRICH(AK,MAK,SMK,KBOR,LIMKEP(1,1),TB,MESH,KDIR,MSK,MASKPT)
      CALL DIRICH(EP,MAE,SME,EBOR,LIMKEP(1,2),TB,MESH,KDIR,MSK,MASKPT)
C
C***********************************************************************
C     SOLVES THE TWO OBTAINED SYSTEMS
C***********************************************************************
C
      CALL SOLVE(AK,MAK,SMK,TB,SLVK ,INFOKE,MESH,TM1)
      CALL SOLVE(EP,MAE,SME,TB,SLVEP,INFOKE,MESH,TM1)
C
C***********************************************************************
C     CLIPS SMALL VALUES                                               *
C***********************************************************************
C
      CALL CLIP(AK,0.D0,.TRUE.,KMAX,.FALSE.,0)
      CALL CLIP(EP,EMIN,.TRUE.,EMAX,.FALSE.,0)
C
C***********************************************************************
C
      RETURN
      END
C
C#######################################################################
C