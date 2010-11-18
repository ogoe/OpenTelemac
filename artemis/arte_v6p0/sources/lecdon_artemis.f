C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       READS THE STEERING FILE THROUGH A DAMOCLES CALL.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>DECLARATIONS_ARTEMIS, DECLARATIONS_TELEMAC, INTERFACE_ARTEMIS
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CODE, FILE_DESC, NCAR, PATH
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_ARTEMIS :<br>
!> @link DECLARATIONS_ARTEMIS::ALEMON ALEMON@endlink, 
!> @link DECLARATIONS_ARTEMIS::ALEMUL ALEMUL@endlink, 
!> @link DECLARATIONS_ARTEMIS::ALFABJ ALFABJ@endlink, 
!> @link DECLARATIONS_ARTEMIS::ARTBI1 ARTBI1@endlink, 
!> @link DECLARATIONS_ARTEMIS::ARTBI2 ARTBI2@endlink, 
!> @link DECLARATIONS_ARTEMIS::ARTCAS ARTCAS@endlink, 
!> @link DECLARATIONS_ARTEMIS::ARTCLI ARTCLI@endlink, 
!> @link DECLARATIONS_ARTEMIS::ARTFO1 ARTFO1@endlink, 
!> @link DECLARATIONS_ARTEMIS::ARTFO2 ARTFO2@endlink, 
!> @link DECLARATIONS_ARTEMIS::ARTFON ARTFON@endlink, 
!> @link DECLARATIONS_ARTEMIS::ARTGEO ARTGEO@endlink, 
!> @link DECLARATIONS_ARTEMIS::ARTRBI ARTRBI@endlink, 
!> @link DECLARATIONS_ARTEMIS::ARTREF ARTREF@endlink, 
!> @link DECLARATIONS_ARTEMIS::ARTRES ARTRES@endlink, 
!> @link DECLARATIONS_ARTEMIS::ARTRFO ARTRFO@endlink, 
!> @link DECLARATIONS_ARTEMIS::ART_FILES ART_FILES@endlink, 
!> @link DECLARATIONS_ARTEMIS::BALAYE BALAYE@endlink, 
!> @link DECLARATIONS_ARTEMIS::BINGEO BINGEO@endlink, 
!> @link DECLARATIONS_ARTEMIS::BINRES BINRES@endlink, 
!> @link DECLARATIONS_ARTEMIS::CDTINI CDTINI@endlink, 
!> @link DECLARATIONS_ARTEMIS::COTINI COTINI@endlink, 
!> @link DECLARATIONS_ARTEMIS::DEFERL DEFERL@endlink, 
!> @link DECLARATIONS_ARTEMIS::DIAM50 DIAM50@endlink, 
!> @link DECLARATIONS_ARTEMIS::DIAM90 DIAM90@endlink, 
!> @link DECLARATIONS_ARTEMIS::DISESP DISESP@endlink, 
!> @link DECLARATIONS_ARTEMIS::ENTFW ENTFW@endlink, 
!> @link DECLARATIONS_ARTEMIS::ENTREG ENTREG@endlink, 
!> @link DECLARATIONS_ARTEMIS::ENTRUG ENTRUG@endlink, 
!> @link DECLARATIONS_ARTEMIS::EPSDIS EPSDIS@endlink, 
!> @link DECLARATIONS_ARTEMIS::EXPOS EXPOS@endlink, 
!> @link DECLARATIONS_ARTEMIS::FFON FFON@endlink, 
!> @link DECLARATIONS_ARTEMIS::FORMFR FORMFR@endlink, 
!> @link DECLARATIONS_ARTEMIS::FROTTE FROTTE@endlink, 
!> @link DECLARATIONS_ARTEMIS::FWCOEF FWCOEF@endlink, 
!> @link DECLARATIONS_ARTEMIS::GAMMA GAMMA@endlink, 
!> @link DECLARATIONS_ARTEMIS::GAMMAS GAMMAS@endlink, 
!> @link DECLARATIONS_ARTEMIS::GDALLY GDALLY@endlink, 
!> @link DECLARATIONS_ARTEMIS::GRAV GRAV@endlink, 
!> @link DECLARATIONS_ARTEMIS::HAUTIN HAUTIN@endlink, 
!> @link DECLARATIONS_ARTEMIS::HMIN HMIN@endlink, 
!> @link DECLARATIONS_ARTEMIS::IBREAK IBREAK@endlink, 
!> @link DECLARATIONS_ARTEMIS::INFOGR INFOGR@endlink, 
!> @link DECLARATIONS_ARTEMIS::I_ORIG I_ORIG@endlink, 
!> @link DECLARATIONS_ARTEMIS::J_ORIG J_ORIG@endlink, 
!> @link DECLARATIONS_ARTEMIS::KDALLY KDALLY@endlink, 
!> @link DECLARATIONS_ARTEMIS::KFROT KFROT@endlink, 
!> @link DECLARATIONS_ARTEMIS::LEOPRD LEOPRD@endlink, 
!> @link DECLARATIONS_ARTEMIS::LISFON LISFON@endlink, 
!> @link DECLARATIONS_ARTEMIS::LISHOU LISHOU@endlink, 
!> @link DECLARATIONS_ARTEMIS::LISPRD LISPRD@endlink, 
!> @link DECLARATIONS_ARTEMIS::LISTIN LISTIN@endlink, 
!> @link DECLARATIONS_ARTEMIS::LVMAC LVMAC@endlink, 
!> @link DECLARATIONS_ARTEMIS::MARDAT MARDAT@endlink, 
!> @link DECLARATIONS_ARTEMIS::MARTIM MARTIM@endlink, 
!> @link DECLARATIONS_ARTEMIS::MAXLU_ART MAXLU_ART@endlink, 
!> @link DECLARATIONS_ARTEMIS::MSK MSK@endlink, 
!> @link DECLARATIONS_ARTEMIS::MVEAU MVEAU@endlink, 
!> @link DECLARATIONS_ARTEMIS::MVSED MVSED@endlink, 
!> @link DECLARATIONS_ARTEMIS::NDALE NDALE@endlink, 
!> @link DECLARATIONS_ARTEMIS::NITDIS NITDIS@endlink, 
!> @link DECLARATIONS_ARTEMIS::NPALE NPALE@endlink, 
!> @link DECLARATIONS_ARTEMIS::NPRIV NPRIV@endlink, 
!> @link DECLARATIONS_ARTEMIS::OPTASS OPTASS@endlink, 
!> @link DECLARATIONS_ARTEMIS::PER PER@endlink, 
!> @link DECLARATIONS_ARTEMIS::PERDEB PERDEB@endlink, 
!> @link DECLARATIONS_ARTEMIS::PERFIN PERFIN@endlink, 
!> @link DECLARATIONS_ARTEMIS::PERPAS PERPAS@endlink, 
!> @link DECLARATIONS_ARTEMIS::PERPIC PERPIC@endlink, 
!> @link DECLARATIONS_ARTEMIS::PMAX PMAX@endlink, 
!> @link DECLARATIONS_ARTEMIS::PMIN PMIN@endlink, 
!> @link DECLARATIONS_ARTEMIS::PRODUC PRODUC@endlink, 
!> @link DECLARATIONS_ARTEMIS::PTINIG PTINIG@endlink, 
!> @link DECLARATIONS_ARTEMIS::PTINIL PTINIL@endlink, 
!> @link DECLARATIONS_ARTEMIS::REGIDO REGIDO@endlink, 
!> @link DECLARATIONS_ARTEMIS::RELAX RELAX@endlink, 
!> @link DECLARATIONS_ARTEMIS::RELDIS RELDIS@endlink, 
!> @link DECLARATIONS_ARTEMIS::RICOEF RICOEF@endlink, 
!> @link DECLARATIONS_ARTEMIS::SLVART SLVART@endlink, 
!> @link DECLARATIONS_ARTEMIS::SORIMP SORIMP@endlink, 
!> @link DECLARATIONS_ARTEMIS::SORLEO SORLEO@endlink, 
!> @link DECLARATIONS_ARTEMIS::SPHERI SPHERI@endlink, 
!> @link DECLARATIONS_ARTEMIS::STDGEO STDGEO@endlink, 
!> @link DECLARATIONS_ARTEMIS::STDRES STDRES@endlink, 
!> @link DECLARATIONS_ARTEMIS::TETAH TETAH@endlink, 
!> @link DECLARATIONS_ARTEMIS::TETMAX TETMAX@endlink, 
!> @link DECLARATIONS_ARTEMIS::TETMIN TETMIN@endlink, 
!> @link DECLARATIONS_ARTEMIS::TEXTE TEXTE@endlink, 
!> @link DECLARATIONS_ARTEMIS::TEXTPR TEXTPR@endlink, 
!> @link DECLARATIONS_ARTEMIS::TITCAS TITCAS@endlink, 
!> @link DECLARATIONS_ARTEMIS::VALID VALID@endlink, 
!> @link DECLARATIONS_ARTEMIS::VARDES VARDES@endlink, 
!> @link DECLARATIONS_ARTEMIS::VARIMP VARIMP@endlink, 
!> @link DECLARATIONS_ARTEMIS::VISCO VISCO@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ADRESS, DIMENS, DOC, I, KK, LSTOP, MNEMO, MOTCAR, MOTCLE, MOTINT, MOTLOG, MOTREA, NMAX, NOM_CAS, NOM_DIC, TROUVE
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_LECDON_ARTEMIS
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> DAMOCLE(), MAJUS(), NOMVAR_ARTEMIS(), PLANTE(), READ_SUBMIT(), SORTIE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>HOMERE_ARTEMIS()

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
!>  <tr>
!>    <td><center> 6.0                                    </center></td>
!>    <td> 20/04/1999                                              </td>
!>    <td> D. AELBRECHT (LNH) 01.30.87.74.12                       </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 17/08/1994                                              </td>
!>    <td> J-M HERVOUET (LNH) 01.30.87.80.18                       </td>
!>    <td> LINKED TO BIEF 5.0                                      </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CODE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FILE_DESC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NCAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PATH
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE LECDON_ARTEMIS
     & (FILE_DESC,PATH,NCAR,CODE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CODE           |---| 
C| FILE_DESC      |---| 
C| NCAR           |---| 
C| PATH           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
      USE INTERFACE_ARTEMIS, EX_LECDON_ARTEMIS => LECDON_ARTEMIS
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      LOGICAL LSTOP
      INTEGER I,KK
C
      CHARACTER*8 MNEMO(MAXVAR)
C
C-----------------------------------------------------------------------
C
C ARRAYS USED IN THE DAMOCLES CALL
C
      INTEGER, PARAMETER :: NMAX = 300
C
      INTEGER ADRESS(4,NMAX),DIMENS(4,NMAX)
      DOUBLE PRECISION MOTREA(NMAX)
      INTEGER          MOTINT(NMAX)
      LOGICAL          MOTLOG(NMAX)
      CHARACTER*144    MOTCAR(NMAX)
      CHARACTER*72     MOTCLE(4,NMAX,2)
      INTEGER          TROUVE(4,NMAX)
      LOGICAL DOC
      CHARACTER(LEN=250) :: NOM_CAS
      CHARACTER(LEN=250) :: NOM_DIC
C ARGUMENTS
      CHARACTER(LEN=24), INTENT(IN)     :: CODE
      CHARACTER(LEN=144), INTENT(INOUT) :: FILE_DESC(4,NMAX)
      INTEGER, INTENT(IN)               :: NCAR
      CHARACTER(LEN=250), INTENT(IN)    :: PATH
C
C END OF DECLARATIONS FOR DAMOCLES CALL :
C
C-----------------------------------------------------------------------
C
C INITIALISES THE VARIABLES FOR DAMOCLES CALL :
C
      DO 10 KK=1,NMAX
C
C       A FILENAME NOT GIVEN BY DAMOCLES WILL BE RECOGNIZED AS A WHITE SPACE
C       (IT MAY BE THAT NOT ALL COMPILERS WILL INITIALISE LIKE THAT)
C
        MOTCAR(KK)(1:1)=' '
C
        DIMENS(1,KK) = 0
        DIMENS(2,KK) = 0
        DIMENS(3,KK) = 0
        DIMENS(4,KK) = 0
C
10    CONTINUE
C
C     WRITES OUT INFO
C
      DOC = .FALSE.
C
C-----------------------------------------------------------------------
C     OPENS DICTIONNARY AND STEERING FILES
C-----------------------------------------------------------------------
C
      IF(NCAR.GT.0) THEN
C
        NOM_DIC=PATH(1:NCAR)//'ARTDICO'
        NOM_CAS=PATH(1:NCAR)//'ARTCAS'
C
      ELSE
C
        NOM_DIC='ARTDICO'
        NOM_CAS='ARTCAS'
C
      ENDIF
C
      OPEN(2,FILE=NOM_DIC,FORM='FORMATTED',ACTION='READ')
      OPEN(3,FILE=NOM_CAS,FORM='FORMATTED',ACTION='READ')
C
      CALL DAMOCLE( ADRESS , DIMENS , NMAX   , DOC     , LNG    , LU ,
     &              MOTINT , MOTREA , MOTLOG , MOTCAR  , MOTCLE ,
     &              TROUVE , 2   , 3   , .FALSE. , FILE_DESC)
C
C     DECODES 'SUBMIT' CHAINS
C
      CALL READ_SUBMIT(ART_FILES,MAXLU_ART,CODE,FILE_DESC,300)
C
C-----------------------------------------------------------------------
C
      DO I=1,MAXLU_ART
        IF(ART_FILES(I)%TELNAME.EQ.'ARTGEO') THEN
          ARTGEO=I
        ELSEIF(ART_FILES(I)%TELNAME.EQ.'ARTCAS') THEN
          ARTCAS=I
        ELSEIF(ART_FILES(I)%TELNAME.EQ.'ARTCLI') THEN
          ARTCLI=I
        ELSEIF(ART_FILES(I)%TELNAME.EQ.'ARTFON') THEN
          ARTFON=I
        ELSEIF(ART_FILES(I)%TELNAME.EQ.'ARTRES') THEN
          ARTRES=I
        ELSEIF(ART_FILES(I)%TELNAME.EQ.'ARTREF') THEN
          ARTREF=I
        ELSEIF(ART_FILES(I)%TELNAME.EQ.'ARTBI1') THEN
          ARTBI1=I
        ELSEIF(ART_FILES(I)%TELNAME.EQ.'ARTBI2') THEN
          ARTBI2=I
        ELSEIF(ART_FILES(I)%TELNAME.EQ.'ARTFO1') THEN
          ARTFO1=I
        ELSEIF(ART_FILES(I)%TELNAME.EQ.'ARTFO2') THEN
          ARTFO2=I
        ELSEIF(ART_FILES(I)%TELNAME.EQ.'ARTRBI') THEN
          ARTRBI=I
        ELSEIF(ART_FILES(I)%TELNAME.EQ.'ARTRFO') THEN
          ARTRFO=I
        ENDIF
      ENDDO
C
C-----------------------------------------------------------------------
C
C ASSIGNS THE STEERING FILE VALUES TO THE PARAMETER FORTRAN NAME :
C
C-----------------------------------------------------------------------
C
C INTEGER KEYWORDS :
C
         LEOPRD    = MOTINT( ADRESS(1, 1) )
         LISPRD    = MOTINT( ADRESS(1, 2) )
         SLVART%NITMAX    = MOTINT( ADRESS(1,3) )
         SLVART%PRECON    = MOTINT( ADRESS(1,4) )
         DISESP    = MOTINT( ADRESS(1, 5) )
         STDGEO    = MOTINT( ADRESS(1, 6) )
         STDRES    = MOTINT( ADRESS(1, 7) )
         SLVART%SLV = MOTINT( ADRESS(1,8) )
         LISFON    = MOTINT( ADRESS(1, 9) )
         NPALE     = MOTINT( ADRESS(1,10) )
         NDALE     = MOTINT( ADRESS(1,11) )
         IBREAK    = MOTINT( ADRESS(1,12) )
         NITDIS    = MOTINT( ADRESS(1,13) )
         REGIDO    = MOTINT( ADRESS(1,14) )
         FORMFR    = MOTINT( ADRESS(1,15) )
         SLVART%KRYLOV = MOTINT( ADRESS(1,16) )
         LVMAC     = MOTINT( ADRESS(1,17) )
         KFROT     = MOTINT( ADRESS(1,18) )
         OPTASS    = MOTINT( ADRESS(1,19) )
         PRODUC    = MOTINT( ADRESS(1,20) )
         MARDAT(1) = MOTINT( ADRESS(1,21) )
         MARDAT(2) = MOTINT( ADRESS(1,21) + 1 )
         MARDAT(3) = MOTINT( ADRESS(1,21) + 2 )
         MARTIM(1) = MOTINT( ADRESS(1,22) )
         MARTIM(2) = MOTINT( ADRESS(1,22) + 1 )
         MARTIM(3) = MOTINT( ADRESS(1,22) + 2 )
         NPRIV     = MOTINT( ADRESS(1,23) )
         NCSIZE    = MOTINT( ADRESS(1,24) )
C        ORIGIN COORDINATES
         I_ORIG    = MOTINT( ADRESS(1,25)   )
         J_ORIG    = MOTINT( ADRESS(1,25)+1 )
C FOR THE MOMENT WTITES TO FILE FROM THE START OF SIMULATION
         PTINIG    = 0
         PTINIL    = 0
C
C REAL KEYWORDS :
C
         PER       = MOTREA( ADRESS(2, 1) )
         TETAH     = MOTREA( ADRESS(2, 2) )
         GRAV      = MOTREA( ADRESS(2, 3) )
         SLVART%ZERO = MOTREA( ADRESS(2, 4) )
         SLVART%EPS = MOTREA( ADRESS(2, 5) )
         HMIN      = MOTREA( ADRESS(2, 6) )
         COTINI    = MOTREA( ADRESS(2, 7) )
         HAUTIN    = MOTREA( ADRESS(2, 8) )
         PERDEB    = MOTREA( ADRESS(2, 9) )
         PERFIN    = MOTREA( ADRESS(2,10) )
         PERPAS    = MOTREA( ADRESS(2,11) )
         PERPIC    = MOTREA( ADRESS(2,12) )
         GAMMA     = MOTREA( ADRESS(2,13) )
         TETMIN    = MOTREA( ADRESS(2,14) )
         TETMAX    = MOTREA( ADRESS(2,15) )
         EXPOS     = MOTREA( ADRESS(2,16) )
         RELAX     = MOTREA( ADRESS(2,17) )
         EPSDIS    = MOTREA( ADRESS(2,18) )
         RELDIS    = MOTREA( ADRESS(2,19) )
         ALFABJ    = MOTREA( ADRESS(2,20) )
         GAMMAS    = MOTREA( ADRESS(2,21) )
         KDALLY    = MOTREA( ADRESS(2,22) )
         GDALLY    = MOTREA( ADRESS(2,23) )
         VISCO     = MOTREA( ADRESS(2,24) )
         DIAM90    = MOTREA( ADRESS(2,25) )
         DIAM50    = MOTREA( ADRESS(2,26) )
         MVSED     = MOTREA( ADRESS(2,27) )
         MVEAU     = MOTREA( ADRESS(2,28) )
         FWCOEF    = MOTREA( ADRESS(2,29) )
         RICOEF    = MOTREA( ADRESS(2,30) )
         FFON      = MOTREA( ADRESS(2,31) )
         PMIN      = MOTREA( ADRESS(2,32) )
         PMAX      = MOTREA( ADRESS(2,33) )
C
C LOGICAL KEYWORDS :
C
         LISTIN    = MOTLOG( ADRESS(3, 1) )
         INFOGR    = MOTLOG( ADRESS(3, 2) )
         BALAYE    = MOTLOG( ADRESS(3, 3) )
         ALEMON    = MOTLOG( ADRESS(3, 4) )
         ALEMUL    = MOTLOG( ADRESS(3, 5) )
         DEFERL    = MOTLOG( ADRESS(3, 6) )
         FROTTE    = MOTLOG( ADRESS(3, 7) )
         ENTFW     = MOTLOG( ADRESS(3, 8) )
         ENTREG    = MOTLOG( ADRESS(3, 9) )
         ENTRUG    = MOTLOG( ADRESS(3, 10) )
         LISHOU    = MOTLOG( ADRESS(3, 11) )
         VALID     = MOTLOG( ADRESS(3, 12) )
C        SPHERICAL EQUATIONS, HARD-CODED
         SPHERI    = .FALSE.
C
C STRING KEYWORDS : SOME ARE USED BY THE LAUNCHING
C                   PROCEDURE
C
         TITCAS    = MOTCAR( ADRESS(4, 1) )(1:72)
         VARDES    = MOTCAR( ADRESS(4, 2) )(1:72)
         CALL MAJUS(VARDES)
         VARIMP    = MOTCAR( ADRESS(4, 3) )(1:72)
         CALL MAJUS(VARIMP)
C        FROM 4 TO 5: READ AND USED BY PRECOS
         ART_FILES(ARTGEO)%NAME=MOTCAR( ADRESS(4,6) )
C        NOMFOR    = MOTCAR( ADRESS(4, 7) )
C        NOMCAS    = MOTCAR( ADRESS(4, 8) )
C         ART_FILES(ARTCAS)%NAME=MOTCAR( ADRESS(4,8) )
         ART_FILES(ARTCLI)%NAME=MOTCAR( ADRESS(4,9) )
C         WRITE(*,*) 'IN LECDON ',ART_FILES(ARTGEO)%NAME
         ART_FILES(ARTRES)%NAME=MOTCAR( ADRESS(4,10) )
C        FROM 11 TO 14 : READ AND USED BY PRECOS
         ART_FILES(ARTFON)%NAME=MOTCAR( ADRESS(4,15) )
         ART_FILES(ARTBI1)%NAME=MOTCAR( ADRESS(4,16) )
         ART_FILES(ARTBI2)%NAME=MOTCAR( ADRESS(4,17) )
         ART_FILES(ARTFO1)%NAME=MOTCAR( ADRESS(4,18) )
         ART_FILES(ARTFO2)%NAME=MOTCAR( ADRESS(4,19) )
         ART_FILES(ARTRBI)%NAME=MOTCAR( ADRESS(4,20) )
         ART_FILES(ARTRFO)%NAME=MOTCAR( ADRESS(4,21) )
C        FROM 22 TO 23 : READ AND USED BY PRECOS OR NOT USED
         CDTINI    = MOTCAR( ADRESS(4,24) )(1:72)
         CALL MAJUS(CDTINI)
         BINGEO    = MOTCAR( ADRESS(4,25) )(1:3)
         CALL MAJUS(BINGEO)
         BINRES    = MOTCAR( ADRESS(4,26) )(1:3)
         CALL MAJUS(BINRES)
         ART_FILES(ARTREF)%NAME=MOTCAR( ADRESS(4,28) )
C        RESULTS FILE FORMAT
         ART_FILES(ARTRES)%FMT = MOTCAR( ADRESS(4,30) )(1:8)
         CALL MAJUS(ART_FILES(ARTRES)%FMT)
C        REFERENCE FILE FORMAT
         ART_FILES(ARTREF)%FMT = MOTCAR( ADRESS(4,31) )(1:8)
         CALL MAJUS(ART_FILES(ARTREF)%FMT)
C        BINARY FILE 1 FORMAT
         ART_FILES(ARTBI1)%FMT = MOTCAR( ADRESS(4,32) )(1:8)
         CALL MAJUS(ART_FILES(ARTBI1)%FMT)
C        BINARY FILE 2 FORMAT
         ART_FILES(ARTBI2)%FMT = MOTCAR( ADRESS(4,33) )(1:8)
         CALL MAJUS(ART_FILES(ARTBI2)%FMT)
C
      IF(LISTIN) THEN
         IF(LNG.EQ.1) WRITE(LU,101)
         IF(LNG.EQ.2) WRITE(LU,102)
      ENDIF
101   FORMAT(1X,/,19X, '********************************************',/,
     &            19X, '*               LECDON:                    *',/,
     &            19X, '*        APRES APPEL DE DAMOCLES           *',/,
     &            19X, '*     VERIFICATION DES DONNEES LUES        *',/,
     &            19X, '*     SUR LE FICHIER DES PARAMETRES        *',/,
     &            19X, '********************************************',/)
102   FORMAT(1X,/,19X, '********************************************',/,
     &            19X, '*               LECDON:                    *',/,
     &            19X, '*        AFTER CALLING DAMOCLES            *',/,
     &            19X, '*        CHECKING OF DATA  READ            *',/,
     &            19X, '*         IN THE STEERING FILE             *',/,
     &            19X, '********************************************',/)
C
C-----------------------------------------------------------------------
C  NAME OF THE VARIABLES FOR THE RESULTS AND GEOMETRY FILES :
C-----------------------------------------------------------------------
C
C LOGICAL ARRAY FOR OUTPUT
C
      CALL NOMVAR_ARTEMIS(TEXTE,TEXTPR,MNEMO)
      CALL SORTIE(VARDES , MNEMO , 100 , SORLEO )
      CALL SORTIE(VARIMP , MNEMO , 100 , SORIMP )
C
C-----------------------------------------------------------------------
C
C IN THE CASE OF A PERIOD SWEEPING, THE FIRST PERIOD TO BE COMPUTED
C IS PERDEB
C
      IF (BALAYE) PER = PERDEB
C
C-----------------------------------------------------------------------
C
C IF NOTHING IS TO BE WRITTEN TO THE LISTING FILE, THERE SHOULD BE
C NO INFORMATION WRITTEN ABOUT THE SOLVEUR
C
      IF (.NOT.LISTIN) INFOGR = .FALSE.
C
C-----------------------------------------------------------------------
C
C FOR RANDOM SEA COMPUTATIONS, INHIBITS THE GRAPHICAL AND LISTING
C OUTPUT OF THE PHASES, SPEEDS, FREE SURFACE ELEVATION, AND
C POTENTIAL BECAUSE THEY DO NOT MEAN ANYTHING.
C
C BUT WRITES OUT AN AVERAGE WAVE NUMBER, AN AVERAGE PHASE CELERITY
C AND AN AVERAGE GROUP VELOCITY, COMPUTED FROM THE MEAN WAVE
C PERIOD T01. ALSO WRITES OUT AN AVERAGE INCIDENCE
C
C MOREOVER, CHECKS THAT THE NUMBER OF DISCRETISED PERIODS AND
C DIRECTIONS IS AT LEAST 1
C
      IF(ALEMON .OR. ALEMUL) THEN
C
C       2 : PHASE
C
        SORLEO( 2) = .FALSE.
        SORIMP( 2) = .FALSE.
C
C       3 AND 4 : U0 AND V0
C
        SORLEO( 3) = .FALSE.
        SORIMP( 3) = .FALSE.
        SORLEO( 4) = .FALSE.
        SORIMP( 4) = .FALSE.
C
C       5 : FREE SURFACE

C
        SORLEO( 5) = .FALSE.
        SORIMP( 5) = .FALSE.
C
C       11 AND 12 : REAL AND IMAGINARY PARTS OF THE POTENTIAL
C
        SORLEO(11) = .FALSE.
        SORIMP(11) = .FALSE.
        SORLEO(12) = .FALSE.
        SORIMP(12) = .FALSE.
C
        IF (NPALE.LE.0) NPALE = 1
        IF (NDALE.LE.0) NDALE = 1
C
      ELSE
C
C       17, 18 AND 19 : T01, T02 AND TM
C
        SORLEO(17) = .FALSE.
        SORIMP(17) = .FALSE.
        SORLEO(18) = .FALSE.
        SORIMP(18) = .FALSE.
        SORLEO(19) = .FALSE.
        SORIMP(19) = .FALSE.
      ENDIF
C
C NO PERIOD SWEEPING FOR RANDOM SEAS
C
      IF (ALEMON .OR. ALEMUL) BALAYE = .FALSE.
C
C NDALE=1 FOR MONO-DIRECTIONAL SEAS
C
      IF (ALEMON .AND. .NOT.ALEMUL) NDALE = 1
C
C-----------------------------------------------------------------------
C
C  DOES NOT WRITE OUT 'PRIVE' VARIABLES IF THEY'VE NOT BEEN ALLOCATED
C
      LSTOP = .FALSE.
      DO 15 I=1,4
        IF ((SORLEO(12+I).OR.SORIMP(12+I)).AND.(NPRIV.LT.I)) THEN
          IF (LNG.EQ.1) WRITE(LU,16) I,NPRIV
          IF (LNG.EQ.2) WRITE(LU,17) I,NPRIV
          LSTOP = .TRUE.
        ENDIF
 15   CONTINUE
      IF (LSTOP) STOP
 16   FORMAT(1X,'LA VARIABLE PRIVEE ',1I1,' NE PEUT ETRE UTILISEE '
     &      ,1X,'CAR ELLE N''EST PAS ALLOUEE.',/
     &      ,1X,'AUGMENTER ''NPRIV'' (ACTUELLEMENT ',1I1,' ).',/)
 17   FORMAT(1X,'PRIVATE ARRAY ',1I1,' CANNOT BE USED '
     &      ,1X,'BECAUSE IT WAS NOT ALLOCATED.',/
     &      ,1X,'CHECK ''NPRIV''  (AT THE TIME BEING ',1I1,' ).',/)
C
C-----------------------------------------------------------------------
C
C  WRITES OUT THE TITLE
C
      IF(LISTIN) THEN
         IF(LNG.EQ.1) WRITE(LU,3000) TITCAS
         IF(LNG.EQ.2) WRITE(LU,3001) TITCAS
3000     FORMAT(/1X,'TITRE DE L''ETUDE :',1X,A72,/)
3001     FORMAT(/1X,'NAME OF THE STUDY :',1X,A72,/)
      ENDIF
C
C-----------------------------------------------------------------------
C
C NO TIDAL FLATS ==> MSK = .FALSE.
C
      MSK = .FALSE.
C
C-----------------------------------------------------------------------
C
C COMPULSORY CHOICE OF KEYWORD FOR THE DIRECT SOLVEUR
C
      IF( (SLVART%SLV.EQ.8).AND.(OPTASS.NE.3) ) THEN
          IF(LNG.EQ.1) WRITE(LU,3002)
          IF(LNG.EQ.2) WRITE(LU,3003)
C
3002    FORMAT(1X,'AVEC SOLVEUR DIRECT, STOCKAGE PAR SEGMENT',/,1X,
     &             'OBLIGATOIRE',///)
3003    FORMAT(1X,'WITH DIRECT SYSTEM SOLVER, EDGE-BASED STORAGE',/,1X,
     &             'IS MANDATORY',///)
          CALL PLANTE(1)
          STOP
      ENDIF
C
C USE OF DIRECT SOLVEUR IS NOT POSSIBLE WITH PARALLELISM
C
      IF(NCSIZE.GT.1) THEN
        IF(SLVART%SLV.EQ.8) THEN
           IF(LNG.EQ.1) WRITE(LU,3004)
           IF(LNG.EQ.2) WRITE(LU,3005)
3004       FORMAT(1X,'AVEC PARALLELISME,',/,1X,
     &             'PAS DE SOLVEUR DIRECT',///)
3005       FORMAT(1X,'WITH PARALLELISM,',/,1X,
     &             'NO DIRECT SYSTEM SOLVER',///)
           CALL PLANTE(1)
           STOP
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C