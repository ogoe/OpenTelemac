C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       WRITES OUT THE DIRECTIONAL VARIANCE SPECTRUM
!>                AT SELECTED NODES.
!>                (SERAPHIN BINARY FORMAT).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, TOMAWAC_MPI, TOMAWAC_MPI_TOOLS
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, AUXIL, B, BINSCO, DATE, DEBRES, F, FREQ, INUTIL, ISLEO, KNOLG, LT, NF, NK, NLEO, NOLEO, NPLAN, NPOIN2, NSCO, TETA, TIME, TITCAS
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> TOMAWAC_MPI :<br>
!> @link TOMAWAC_MPI::MPI_COMM_WORLD MPI_COMM_WORLD@endlink, 
!> @link TOMAWAC_MPI::MPI_INTEGER MPI_INTEGER@endlink, 
!> @link TOMAWAC_MPI::MPI_REAL8 MPI_REAL8@endlink, 
!> @link TOMAWAC_MPI::MPI_SUCCESS MPI_SUCCESS@endlink, 
!> @link TOMAWAC_MPI::MPI_UB MPI_UB@endlink<hr>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> ECRSPE_MPI : SPE_SEND
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AAT, BVARSOR, C, C0, C1, C2, C3, C4, C5, C6, CC, I, IB, IBID, IER, II, IKLE, ILEO, ISTAT, JF, K, KAMP1, KAMP2, KAMP3, KAMP4, KAMP5, KAMP6, MESHF, NELEM, NPSPE, NRECV_LEO, NSPE_RECV, NUM, SORLEO, TEXTE
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> BVARSOR_SENDRECV(), CREATE_DATASET(), GET_MPI_PARAMETERS(), SPECTRE_SEND(), TEXTE_SENDRECV(), WRITE_DATA(), WRITE_MESH()
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
!>      <td><center> 6.0                                       </center>
!> </td><td>
!> </td><td>
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.5                                       </center>
!> </td><td> 13/07/2004
!> </td><td> M. BENOIT
!> </td><td> CORRECTED A BUG IN THE DECLARATION OF IPOBO WHEN PASSED
!>           AS ARGUMENT. THE PASSED ARRAY IS NO LONGER USED AND IS
!>           RENAMED INUTIL. IPOBO IS DECLARED AS ALLOCATABLE IN THE
!>           SUBROUTINE.
!> </td></tr>
!>      <tr>
!>      <td><center> 5.2                                       </center>
!> </td><td> 07/06/2001
!> </td><td>
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.0                                       </center>
!> </td><td> 28/08/2000
!> </td><td> OPTIMER  02 98 44 24 51
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
!>          <tr><td>AUXIL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>AUXIL(
!></td><td>---</td><td>TABLEAU DE TRAVAIL POUR SPECTRE DIRECT.
!>    </td></tr>
!>          <tr><td>B
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BINSCO
!></td><td>--></td><td>BINAIRE DU FICHIER DE SORTIE DES SPECTRES
!>    </td></tr>
!>          <tr><td>DATE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DEBRES
!></td><td>--></td><td>INDICATEUR DE PREMIERE DATE A SAUVER
!>    </td></tr>
!>          <tr><td>F
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F(
!></td><td>--></td><td>SPECTRE DIRECTIONNEL DE VARIANCE
!>    </td></tr>
!>          <tr><td>FREQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FREQ(
!></td><td>--></td><td>TABLEAU DES FREQUENCES DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>INUTIL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>INUTIL(
!></td><td>---</td><td>TABLEAU NON-UTILISE (ANCIEN IPOBO)
!>    </td></tr>
!>          <tr><td>ISLEO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KNOLG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LT
!></td><td>--></td><td>NUMERO D IMRPESSION
!>    </td></tr>
!>          <tr><td>NF
!></td><td>--></td><td>NOMBRE DE FREQUENCES DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>NK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NLEO
!></td><td>--></td><td>NOMBRE DE POINTS DE SORTIE
!>    </td></tr>
!>          <tr><td>NOLEO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NOLEO(
!></td><td>--></td><td>NUMEROS DE NOEUD DES POINTS DE SORTIE
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE DIRECTIONS DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE SPATIAL 2D
!>    </td></tr>
!>          <tr><td>NSCO
!></td><td>--></td><td>NUM. DU FICHIER DE SORTIE DES SPECTRES
!>    </td></tr>
!>          <tr><td>TETA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TETA(
!></td><td>--></td><td>VECTEUR DES DIRECTIONS DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>TIME
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TITCAS
!></td><td>--></td><td>TITRE DU CAS DE CALCUL
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ECRSPE
     &( F     , B     , TETA  , NPLAN , FREQ  , NF    , NK    ,
     &  NPOIN2, AT    , LT    , AUXIL , INUTIL, NOLEO , NLEO  , NSCO  ,
     &  BINSCO, DEBRES, TITCAS, DATE  , TIME ,ISLEO ,KNOLG)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| DATE COURANTE DU CALCUL
C| AUXIL          |---| 
C| AUXIL(         |---| TABLEAU DE TRAVAIL POUR SPECTRE DIRECT.
C| B             |---| 
C| BINSCO         |-->| BINAIRE DU FICHIER DE SORTIE DES SPECTRES
C| DATE           |---| 
C| DEBRES         |-->| INDICATEUR DE PREMIERE DATE A SAUVER
C| F             |---| 
C| F(             |-->| SPECTRE DIRECTIONNEL DE VARIANCE
C| FREQ           |---| 
C| FREQ(          |-->| TABLEAU DES FREQUENCES DE DISCRETISATION
C| INUTIL         |---| 
C| INUTIL(        |---| TABLEAU NON-UTILISE (ANCIEN IPOBO)
C| ISLEO          |---| 
C| KNOLG          |---| 
C| LT             |-->| NUMERO D IMRPESSION
C| NF             |-->| NOMBRE DE FREQUENCES DE DISCRETISATION
C| NK             |---| 
C| NLEO           |-->| NOMBRE DE POINTS DE SORTIE
C| NOLEO          |---| 
C| NOLEO(         |-->| NUMEROS DE NOEUD DES POINTS DE SORTIE
C| NPLAN          |-->| NOMBRE DE DIRECTIONS DE DISCRETISATION
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE SPATIAL 2D
C| NSCO           |-->| NUM. DU FICHIER DE SORTIE DES SPECTRES
C| TETA           |---| 
C| TETA(          |-->| VECTEUR DES DIRECTIONS DE DISCRETISATION
C| TIME           |---| 
C| TITCAS         |-->| TITRE DU CAS DE CALCUL
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE TOMAWAC_MPI
      USE TOMAWAC_MPI_TOOLS
C
      IMPLICIT NONE

      COMMON/ECRSPE_MPI/SPE_SEND
      INTEGER ::SPE_SEND
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      INTEGER  NPOIN2, NLEO  , NSCO  , NF , NK , NPLAN
      INTEGER  NOLEO(NLEO)   , INUTIL(1)
      INTEGER  DATE(3),TIME(3)
      DOUBLE PRECISION AT    , AUXIL(NPLAN,NK), AAT(1)
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF) , TETA(NPLAN), FREQ(NF)
      DOUBLE PRECISION B(NPOIN2,NPLAN)
      INTEGER  LT
C
      LOGICAL DEBRES
      CHARACTER*72 TITCAS
      CHARACTER(LEN=*)   BINSCO
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER  ISTAT , II    , JF    , K     , IB(10)
      INTEGER  KAMP1 , KAMP2 , KAMP3 , KAMP4 , KAMP5 , KAMP6 , ILEO
      INTEGER  IBID(1), NELEM, NPSPE
      CHARACTER*72 C
      CHARACTER*32 TEXTE(99)
      CHARACTER*6  NUM
      CHARACTER*2  CC
      CHARACTER*1  C0    , C1    , C2    , C3    , C4    , C5    , C6
!BD_INCKA ADDS A GRID ON THE FREQUENCIES AND PLANES
      TYPE(BIEF_MESH) :: MESHF
      LOGICAL         :: SORLEO(99)

      INTEGER :: I,IER
      INTEGER, ALLOCATABLE :: IKLE(:) ! GLOBAL CONNECTIVITY
      TYPE(BIEF_OBJ)  :: BVARSOR
!BD_INCKA MODIFICATION FOR PARALLEL MODE
      INTEGER, DIMENSION(NLEO) :: NRECV_LEO
      LOGICAL         :: ISLEO(NLEO) !
      INTEGER, DIMENSION(NCSIZE) :: NSPE_RECV
      INTEGER :: KNOLG(NPOIN2)
!BD_INCKA
!BD_INCKA END OF MODIFICATION
C
      NPSPE=NF*NPLAN
      NELEM=(NF-1)*NPLAN
      SORLEO = .FALSE.
      DO ILEO=1,NLEO
          KAMP1=NOLEO(ILEO)
          IF (NCSIZE.GT.1) KAMP1=KNOLG(NOLEO(ILEO))
          KAMP2=MOD(KAMP1,100000)
          KAMP3=MOD(KAMP2,10000)
          KAMP4=MOD(KAMP3,1000)
          KAMP5=MOD(KAMP4,100)
          KAMP6=MOD(KAMP5,10)
C          IF(ILEO.GT.9) THEN
C            C0='1'
C          ELSE
C            C0='0'
C          ENDIF
C          CC=C0//CHAR(48+MOD(ILEO,10))
          CC=CHAR(48+INT(ILEO/10))//CHAR(48+MOD(ILEO,10))
          C1=CHAR(48+INT(KAMP1/100000))
          C2=CHAR(48+INT(KAMP2/10000))
          C3=CHAR(48+INT(KAMP3/1000))
          C4=CHAR(48+INT(KAMP4/100))
          C5=CHAR(48+INT(KAMP5/10))
          C6=CHAR(48+KAMP6)
          NUM=C1//C2//C3//C4//C5//C6
          TEXTE(ILEO)='F'//CC//' PT2D'//NUM//'  UNITE SI       '
          IF (.NOT.ISLEO(ILEO)) TEXTE(ILEO) =
     &     'POINT HORS MAILLAGE             '
          SORLEO(ILEO) = .TRUE.
      ENDDO
C
C=====C
C  1  C FOR THE FIRST PRINTED TIME STEP, WRITES OUT THE HEADER TO THE FILE
C=====C================================================================
      IF (DEBRES) THEN
C
C.......2.1 NAME OF THE VARIABLES
C       """""""""""""""""""""""""""""""""""""
!BD_INCKA CREATES MESHF, MESH ASSOCIATED WITH DISCRETISATION
C         IN FREQUENCY AND DIRECTION
        ALLOCATE(MESHF%TYPELM)
        ALLOCATE(MESHF%NELEM)
        ALLOCATE(MESHF%NPOIN)
        ALLOCATE(MESHF%IKLE)
        ALLOCATE(MESHF%IKLE%I(4*NELEM))
        ALLOCATE(MESHF%X)
        ALLOCATE(MESHF%Y)
        ALLOCATE(MESHF%NPTFR)
        ALLOCATE(MESHF%NBOR)
        ALLOCATE(MESHF%NBOR%I(NPSPE))
        ALLOCATE(MESHF%DIM)
        ALLOCATE(MESHF%KNOLG)
        ALLOCATE(MESHF%KNOLG%I(NPSPE))
        MESHF%NAME = 'MESH'
        MESHF%TYPELM = 20 !QUADRANGLE 2D MESH
        MESHF%NELEM  = NELEM
        MESHF%NPOIN  = NPSPE
        MESHF%DIM    = 2
        ALLOCATE(IKLE(4*NELEM))
        II=0
        DO JF=1,NF-1
          DO K=1,NPLAN
           II=II+1
           IKLE(II)=MOD(II,NPLAN)+1+(JF-1)*NPLAN
          ENDDO
        ENDDO
        DO II=1,NELEM
          IKLE(II+NELEM)=II
          IKLE(II+2*NELEM)=II+NPLAN
          IKLE(II+3*NELEM)=IKLE(II)+NPLAN
        ENDDO
        MESHF%IKLE%I=IKLE
C
C        DEALLOCATE(IKLE)
C        DEALLOCATE(IPOBO)
C
C.......2.9 WRITES OUT THE ARRAYS X AND Y
C       """"""""""""""""""""""""""""""""
        ALLOCATE(MESHF%X%R(NPLAN*NF))
        ALLOCATE(MESHF%Y%R(NPLAN*NF))
        MESHF%NPTFR = 2*NPLAN!+2*(NF-2)
        DO JF=1,NF
          DO II=1,NPLAN
            MESHF%X%R(II+NPLAN*(JF-1))=FREQ(JF)*SIN(TETA(II))
          ENDDO
        ENDDO
        DO JF=1,NF
          DO II=1,NPLAN
            MESHF%Y%R(II+NPLAN*(JF-1))=FREQ(JF)*COS(TETA(II))
          ENDDO
        ENDDO
        MESHF%NBOR%I=0
        DO II = 1,NPLAN
           MESHF%NBOR%I(II) = II
        ENDDO
        DO II = NPLAN+1,2*NPLAN
           MESHF%NBOR%I(II)=NPLAN+1+NPSPE-II
        ENDDO
        MESHF%KNOLG%I = 0
!BD_INCKA IN PARALLEL MODE, INITIALISES THE VALUES OF MPI PARAMETERS
      IF (NCSIZE.GT.1)         CALL GET_MPI_PARAMETERS(MPI_INTEGER,
     &                          MPI_REAL8,MPI_UB,
     &                          MPI_COMM_WORLD,MPI_SUCCESS)
!BD_INCKA END OF MODIFICATION
!BD_INCKA IN PARALLEL MODE, ASSOCIATES THE SUB-DOMAIN NODE NUMBERS
C WITH THE POINTS WHERE SPECTRAL OUTPUT IS REQUIRED
        IF (NCSIZE.GT.1) THEN
        CALL SPECTRE_SEND(SPE_SEND,NSPE_RECV,NLEO,ISLEO,
     &                           NRECV_LEO)
        CALL TEXTE_SENDRECV(TEXTE(1:NLEO),NLEO,NPSPE,ISLEO,NRECV_LEO)
        ENDIF
!BD_INCKA END OF MODIFICATION
        IF (((NCSIZE.GT.1).AND.(IPID==0)).OR.NCSIZE.LE.1) THEN
        ! CREATES DATA FILE USING A GIVEN FILE FORMAT : FORMAT_RES.
        ! THE DATA ARE CREATED IN THE FILE: NRES, AND IS
        ! CHARACTERISED BY A TITLE AND NAME OF OUTPUT VARIABLES
        ! CONTAINED IN THE FILE.
        CALL CREATE_DATASET(BINSCO, ! RESULTS FILE FORMAT
     &                      NSCO,   ! LU FOR RESULTS FILE
     &                      TITCAS, ! TITLE
     &                      NLEO,   ! MAX NUMBER OF OUTPUT VARIABLES
     &                      TEXTE,  ! NAMES OF OUTPUT VARIABLES
     &                      SORLEO) ! PRINT TO FILE OR NOT
        ! WRITES THE MESH IN THE OUTPUT FILE :
        ! IN PARALLEL, REQUIRES NCSIZE AND NPTIR.
        ! THE REST OF THE INFORMATION IS IN MESH.
        ! ALSO WRITES : START DATE/TIME AND COORDINATES OF THE
        ! ORIGIN.
        CALL WRITE_MESH(BINSCO, ! RESULTS FILE FORMAT
     &                  NSCO,   ! LU FOR RESULTS FILE
     &                  MESHF,  ! CHARACTERISES MESH
     &                  1,      ! NUMBER OF PLANES /NA/
     &                  DATE,   ! START DATE
     &                  TIME,   ! START TIME
     &                  0,0)    ! COORDINATES OF THE ORIGIN.
        ENDIF
C
      ENDIF
!BD_INCKA IN PARALLEL MODE, ASSOCIATES THE SUB-DOMAIN NODE NUMBERS
C WITH THE POINTS WHERE SPECTRAL OUTPUT IS REQUIRED
        IF (NCSIZE.GT.1) THEN
        CALL SPECTRE_SEND(SPE_SEND,NSPE_RECV,NLEO,ISLEO,
     &                           NRECV_LEO)
        CALL TEXTE_SENDRECV(TEXTE,NLEO,NPSPE,ISLEO,NRECV_LEO)
        ENDIF
!BD_INCKA END OF MODIFICATION
C=====C
C  3  C RECORDS THE CURRENT TIME STEP
C=====C========================================
C
C.....3.1 WRITES OUTPUT AT TIME 'AT'
C     """"""""""""""""""""""""
      AAT(1) = AT
      ALLOCATE(BVARSOR%ADR(NLEO))
      DO II=1,NLEO
        ALLOCATE(BVARSOR%ADR(II)%P)
        ALLOCATE(BVARSOR%ADR(II)%P%R(NPSPE))
        BVARSOR%ADR(II)%P%DIM1 = NPSPE
        BVARSOR%ADR(II)%P%ELM  = 21
      ENDDO
      DO ILEO=1,NLEO
        II=NOLEO(ILEO)
        DO JF=1,NF
          DO K=1,NPLAN
            BVARSOR%ADR(ILEO)%P%R(K+(JF-1)*NPLAN)=F(II,K,JF)
          ENDDO
        ENDDO
      ENDDO
!
!BD_INCKA MODIFICATION FOR PARALLEL MODE
      IF (NCSIZE.GT.1) THEN
        CALL BVARSOR_SENDRECV(BVARSOR,NLEO,NPSPE,ISLEO,NRECV_LEO)
        IF (IPID==0) CALL WRITE_DATA(BINSCO,NSCO,99,AT,LT,SORLEO,TEXTE,
     &                BVARSOR,NPSPE)
      ELSE
        CALL WRITE_DATA(BINSCO,NSCO,99,AT,LT,SORLEO,TEXTE,
     &                BVARSOR,NPSPE)
      ENDIF
!
!
      RETURN
      END
C
C#######################################################################
C