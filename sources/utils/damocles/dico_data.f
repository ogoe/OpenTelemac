      ! brief set of function to read/write a dictionary
      MODULE DICO_DATA
      ! brief Max size of aide
      INTEGER, PARAMETER :: MAXAIDELEN = 2400
      ! brief type for a keyword
      TYPE KEYWORD
        ! param Name of the ley in French and English
        CHARACTER(LEN=72) :: KNOM(2)
        ! param Type fo the key word 1:integer 2:real  3:logical  4: String
        INTEGER         :: KTYPE
        ! param Index of the keyword
        INTEGER         :: KINDEX
        ! param Name of the variable containing the keyword in the Fortran code
        CHARACTER(LEN=144) :: MNEMO
        ! To Be defined
        ! param Size of the keyword 0: 1: 2:
        INTEGER         :: TAILLE
        ! param String containing information on file keyword
        CHARACTER(LEN=144) :: SUBMIT
        ! param Default value in frecnh and in english
        CHARACTER(LEN=300) :: DEFAUT(2)
        ! param List of values for the keyword
        CHARACTER(LEN=200) :: CHOIX(2)
        ! param Classification of the keyword
        CHARACTER(LEN=144) :: RUBRIQUE(2,3)
        ! param
        CHARACTER(LEN=144) :: COMPOSE
        ! param
        CHARACTER(LEN=144) :: COMPORT
        ! param
        CHARACTER(LEN=144) :: CONTROLE
        ! param
        CHARACTER(LEN=144) :: APPARENCE
        ! param Level of the keyword (TODO: Find if it is used)
        INTEGER :: NIVEAU
        ! param help on the keyword in french and in english
        CHARACTER(LEN=MAXAIDELEN) :: AIDE(2)
      END TYPE
!
      ! brief Max number of keyword per type
      INTEGER, PARAMETER :: NMAX=110
      ! brief Max number of rubrique
      INTEGER, PARAMETER :: RMAX=50
!     brief Listing canal
      INTEGER, PARAMETER :: LU=6
!
      TYPE(KEYWORD) :: MYDICO(NMAX*4)
      INTEGER :: NKEY=0
!
      CHARACTER(LEN=144) :: RUBRIQUE(RMAX,2)
      INTEGER :: NRUB(2)
!
      CONTAINS
      ! brief Sorting function
      !
      ! param N Size of the array
      ! param A Array to sort
      ! param B Reordering array
      SUBROUTINE SHELL_STRING
!
     &                          (N, A, B)
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)              :: N
      CHARACTER(LEN=144), INTENT(INOUT)  :: A(N)
      INTEGER, INTENT(OUT)             :: B(N)
!
      INTEGER                          :: I, J, INC
      CHARACTER(LEN=144)               :: V
      INTEGER                          :: W
!
      INTEGER                          :: ALPHA
!
      ALPHA=2
!
      DO I=1,N
        B(I)=I
      ENDDO
!
      INC=1
 1    INC=ALPHA*INC+1
      IF (INC.LE.N) GOTO 1
 2    CONTINUE
        INC=INC/ALPHA
        DO I=INC+1,N
          V=A(I)
          W=B(I)
          J=I
 3        IF (A(J-INC).GT.V) THEN
            A(J)=A(J-INC)
            B(J)=B(J-INC)
            J=J-INC
            IF (J.LE.INC) GOTO 4
          GOTO 3
          ENDIF
 4        A(J)=V
          B(J)=W
        ENDDO
!
      IF (INC.GT.1) GOTO 2
!
      RETURN
      END SUBROUTINE
      !
      ! brief write an integer into a string
      !
      ! param string The output string
      ! param i the integer to write
      ! param pos the position of the first non blank character
      SUBROUTINE INT2STR(STRING,I,POS)
      IMPLICIT NONE
      !
      CHARACTER(LEN=3),INTENT(INOUT) :: STRING
      INTEGER,INTENT(IN) :: I
      INTEGER,INTENT(INOUT) :: POS
      !
      WRITE(STRING,'(I3)') I
      POS=1
      DO
        IF(STRING(POS:POS).NE.' ') EXIT
        POS=POS+1
      ENDDO
      END SUBROUTINE
      !
      ! brief Fill the array rubrique that contains the list of the rtubriques
      !
      SUBROUTINE CHECK_INDEX()
      IMPLICIT NONE
      !
      INTEGER IKEY,ITYP,I,IERR,J
      INTEGER NTYP(4),OLD_NTYP(4)
      INTEGER MAX_IDX(4)
      INTEGER ITYP2(4)
      INTEGER,ALLOCATABLE :: ITYP2KEY(:,:)
      LOGICAL,ALLOCATABLE :: IDX_USED(:,:)
      CHARACTER(LEN=144) :: USED_IDX(4)
      CHARACTER(LEN=3) :: I2S
      INTEGER :: LAST_TRUE,IDX
      !
      ! Count the number of keyword by type
      !
      NTYP = 0
      MAX_IDX = 0
      DO IKEY=1,NKEY
        ITYP = MYDICO(IKEY)%KTYPE
        NTYP(ITYP) = NTYP(ITYP) + 1
        MAX_IDX(ITYP) = MAX(MAX_IDX(ITYP),MYDICO(IKEY)%KINDEX)
      ENDDO
      WRITE(*,*) '---- INDEX INFORMATIONS ----'
      WRITE(*,*) 'NUMBER OF KEY WORD BY TYPE AND MAX INDEX:'
      WRITE(*,*) 'FOR INTEGER: ',NTYP(1),MAX_IDX(1)
      WRITE(*,*) 'FOR REAL   : ',NTYP(2),MAX_IDX(2)
      WRITE(*,*) 'FOR LOGICAL: ',NTYP(3),MAX_IDX(3)
      WRITE(*,*) 'FOR STRING : ',NTYP(4),MAX_IDX(4)
      ALLOCATE(ITYP2KEY(MAXVAL(NTYP),4),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'ITYP2KEY')
      OLD_NTYP = NTYP
      NTYP = 0
      ! Computting the ityp2key array
      ! Loop on all types
      DO I=1,4
        ! Loop on all indexes
        DO IDX=1,MAX_IDX(I)
          ! Identifying the key associated to the idx
          DO IKEY=1,NKEY
            ITYP = MYDICO(IKEY)%KTYPE
            IF(I.NE.ITYP) CYCLE
            IF(MYDICO(IKEY)%KINDEX.NE.IDX) CYCLE
            NTYP(ITYP) = NTYP(ITYP) + 1
            ITYP2KEY(NTYP(ITYP),ITYP) = IKEY
            EXIT
          ENDDO
        ENDDO
      ENDDO
!
      DO I=1,4
        IF(NTYP(I).NE.OLD_NTYP(I)) THEN
          WRITE(*,*) 'ERROR ON INDEX FOR TYPE',I
          CALL PLANTE(1)
        ENDIF
      ENDDO
      !
      ALLOCATE(IDX_USED(MAXVAL(MAX_IDX),4),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'IDX_USED')
      IDX_USED = .FALSE.
      DO IKEY=1,NKEY
        IDX_USED(MYDICO(IKEY)%KINDEX,MYDICO(IKEY)%KTYPE) = .TRUE.
      ENDDO
      DO ITYP=1,4
        USED_IDX(ITYP) = REPEAT(' ',144)
        IF(NTYP(ITYP).EQ.0) CYCLE
        LAST_TRUE = 1
        LAST_TRUE = MYDICO(ITYP2KEY(1,ITYP))%KINDEX
        CALL INT2STR(I2S,LAST_TRUE,I)
        USED_IDX(ITYP)(1:1) = I2S(I:3)
        DO J=2,NTYP(ITYP)
          IKEY = ITYP2KEY(J,ITYP)
          IDX = MYDICO(IKEY)%KINDEX
          IF(IDX.EQ.LAST_TRUE+1) THEN
            LAST_TRUE = IDX
          ELSE
            CALL INT2STR(I2S,LAST_TRUE,I)
            USED_IDX(ITYP) = TRIM(USED_IDX(ITYP)) // '-' // I2S(I:3)
            LAST_TRUE = IDX
            CALL INT2STR(I2S,LAST_TRUE,I)
            USED_IDX(ITYP) = TRIM(USED_IDX(ITYP)) // ',' // I2S(I:3)
          ENDIF
        ENDDO
        IF(IDX_USED(MAX_IDX(ITYP),ITYP)
     &     .AND.IDX_USED(MAX_IDX(ITYP)-1,ITYP)) THEN
          CALL INT2STR(I2S,MAX_IDX(ITYP),I)
          USED_IDX(ITYP) = TRIM(USED_IDX(ITYP)) // '-' // I2S(I:3)
        ENDIF
      ENDDO
      WRITE(*,*) ''
      WRITE(*,*) 'INTEGER INDEX USED: ',TRIM(USED_IDX(1)),
     &           ' OUT OF ',NTYP(1)
      WRITE(*,*) 'REAL INDEX USED: ',TRIM(USED_IDX(2)),
     &           ' OUT OF ',NTYP(2)
      WRITE(*,*) 'LOGICAL INDEX USED: ',TRIM(USED_IDX(3)),
     &           ' OUT OF ',NTYP(3)
      WRITE(*,*) 'STRING INDEX USED: ',TRIM(USED_IDX(4)),
     &           ' OUT OF ',NTYP(4)
      WRITE(*,*) ''
      !
      END SUBROUTINE
      !
      ! brief Fill the array rubrique that contains the list of the rtubriques
      !
      SUBROUTINE IDENTIFY_RUBRIQUE()
      IMPLICIT NONE
      !
      INTEGER :: I,J,IKEY,LNG
      LOGICAL :: ALREADY_IN
      !
      !  Loop on all languge
      DO LNG=1,2
        NRUB(LNG) = 0
        ! Get the first rubriques
        DO I=1,3
          IF(MYDICO(1)%RUBRIQUE(LNG,I).NE.' ') THEN
            NRUB(LNG) = NRUB(LNG) + 1
            RUBRIQUE(NRUB(LNG),LNG) = MYDICO(1)%RUBRIQUE(LNG,I)
          ENDIF
        ENDDO
        DO IKEY=1,NKEY
          DO I=1,3
            IF(MYDICO(IKEY)%RUBRIQUE(LNG,I).NE.' ') THEN
              ! Check if keyword already found
              ALREADY_IN = .FALSE.
              DO J=1,NRUB(LNG)
                IF(MYDICO(IKEY)%RUBRIQUE(LNG,I)
     &             .EQ.RUBRIQUE(J,LNG)) THEN
                  ALREADY_IN = .TRUE.
                  EXIT
                ENDIF
              ENDDO
              ! If new rubrique adding it to the rubrique array
              IF(.NOT.ALREADY_IN) THEN
!         write(666+lng,*) 'Adding: ',trim(myDico(ikey)%rubrique(lng,i))
                NRUB(LNG) = NRUB(LNG) + 1
                RUBRIQUE(NRUB(LNG),LNG) = MYDICO(IKEY)%RUBRIQUE(LNG,I)
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDDO
      END SUBROUTINE
      ! brief write in canal the default value in Latex
      !+      form as neatly as possible
      !
      ! param nfic file canal
      ! param ikey key index in myDico
      ! param lng language of the key (1:french,2:english)
      SUBROUTINE WRITE_DEFAULT(NFIC,IKEY,LNG)
      IMPLICIT NONE
      !
      INTEGER,INTENT(IN) :: IKEY
      INTEGER,INTENT(IN) :: NFIC
      INTEGER, INTENT(IN) :: LNG
      !
      INTEGER I, LENGTH, IDB, IDE, IDX
      CHARACTER(LEN=300) :: STRING
!
      ! If the default values are strings we add quote
      IF(MYDICO(IKEY)%KTYPE.EQ.4) THEN
        STRING = MYDICO(IKEY)%DEFAUT(LNG)
        LENGTH = LEN(TRIM(MYDICO(IKEY)%DEFAUT(LNG)))
        ! If they are too many values printing each on a new line
        IF(LENGTH.GT.70) THEN
          ! First value printing additional text
          IDB = 1
          IDE = INDEX(STRING(1:LENGTH),';')
          WRITE(NFIC,'(3A)') "DEFAULT VALUE : & '",
     &                     STRING(IDB:IDE),"\\"
          DO
            IDB = IDE + 1
            IDX = INDEX(STRING(IDB:LENGTH),';')
            IDE = IDX + IDB - 1
            IF(IDX.EQ.0.OR.IDB.GT.LENGTH) EXIT
            WRITE(NFIC,'(3A)') "                & ",
     &                       STRING(IDB:IDE),"\\"
          ENDDO
            WRITE(NFIC,'(3A)') "                & ",
     &                       STRING(IDB:LENGTH),"'\\"
        ELSE
          ! Normal one line printing with quote
          WRITE(NFIC,'(3A)') "DEFAULT VALUE : & '",
     &                     TRIM(MYDICO(IKEY)%DEFAUT(LNG)),"'\\"
        ENDIF
      ELSE
        ! Normal one line printing without quote
        WRITE(NFIC,'(3A)') "DEFAULT VALUE : & ",
     &                     TRIM(MYDICO(IKEY)%DEFAUT(LNG)),"\\"
      ENDIF
      !
      END SUBROUTINE
      ! brief Return true if rubrique(irub) is in keyword ikey rubrique
      !
      ! param ikey key index in myDico
      ! param irub rubrique in rubrique
      ! param lng language of the key (1:french,2:english)
      SUBROUTINE HAS_RUBRIQUE(HAS,IKEY,IRUB,LNG)
      IMPLICIT NONE
      !
      INTEGER,INTENT(IN) :: IKEY
      INTEGER,INTENT(IN) :: IRUB
      INTEGER, INTENT(IN) :: LNG
      LOGICAL,INTENT(INOUT) :: HAS
      !
      INTEGER I
      !
      HAS = .FALSE.
      DO I=1,3
        ! Looping on each sub rubrique
        IF(MYDICO(IKEY)%RUBRIQUE(LNG,I).EQ.RUBRIQUE(IRUB,LNG)) THEN
          HAS = .TRUE.
          RETURN
        ENDIF
      ENDDO
      END SUBROUTINE
      ! brief Identify what reserved key is the ligne associated to
      !
      ! param chaine key to identify
      ! param ilong length of chaine
      ! param numero number of the key
      ! param lng language of the key (1:french,2:english)
      SUBROUTINE IDENTIFY_KEY(CHAINE,ILONG,NUMERO,LNG)
      IMPLICIT NONE
      !
      CHARACTER(LEN=*),INTENT(IN) :: CHAINE
      INTEGER,INTENT(INOUT) :: ILONG
      INTEGER, INTENT(INOUT) :: NUMERO
      INTEGER, INTENT(INOUT) :: LNG
      !
      CHARACTER*9 MOTPRO(15)
      DATA MOTPRO /'NOM','TYPE','INDEX','TAILLE','DEFAUT','AIDE',
     & 'CHOIX','RUBRIQUE','NIVEAU','MNEMO','COMPOSE','COMPORT',
     & 'CONTROLE','APPARENCE','SUBMIT'/
!     LENGTH OF THE PROTECTED WORDS
      INTEGER LONPRO(15),I
      DATA LONPRO /3,4,5,6,6,4,5,8,6,5,7,7,8,9,6/
      !
      LNG = 1
      NUMERO = 0
      ! Check if it is an english key 1 at the end
      IF (CHAINE(ILONG:ILONG).EQ.'1') THEN
        ILONG = ILONG - 1
        LNG = 2
      ENDIF
      ! Search in the list of known keyword
      DO I=1,15
        IF (ILONG.EQ.LONPRO(I)) THEN
          IF (CHAINE(1:ILONG).EQ.MOTPRO(I)(1:ILONG)) THEN
            NUMERO = I
            EXIT
          ENDIF
        ENDIF
      ENDDO ! I
      !
      END SUBROUTINE

      ! brief Fill the myDico structure by reading the dictionary
      !
      ! param filename name of the dictionary file
      ! param
      SUBROUTINE READ_DICTIONARY(FILENAME)
      !
      IMPLICIT NONE
      !
      CHARACTER(LEN=144), INTENT(IN) :: FILENAME
      CHARACTER(LEN=MAXAIDELEN) :: TMP
      INTEGER :: IKEY,IERR,LNG,I
      !
      INTEGER          LCAR,ICOL,JCOL,ILONG,NUMERO,I2
      INTEGER          NBMOT,LONGU
      INTEGER          ORDRE
      INTEGER          NIGN
      LOGICAL          DYNAM,AIDLNG,VUMOT
      LOGICAL          ARRET,EXECMD
      CHARACTER*1      PTVIRG,QUOTE
      CHARACTER*9      TYPE
      CHARACTER*72     LIGNE
      CHARACTER*144    TYPE2
      !
      LOGICAL :: ERREUR, RETOUR
      COMMON / DCRARE / ERREUR , RETOUR

      INTEGER :: NLIGN, LONGLI
      COMMON / DCMLIG / NLIGN , LONGLI

      INTEGER :: NFIC
      COMMON / DCCHIE / NFIC

      INTEGER :: INDX,NTYP,ITAI,NMOT(4),DEFLU
      COMMON / DCNGE  / INDX,NTYP,ITAI,NMOT,DEFLU

      CHARACTER(LEN=72)     PARAM
      COMMON / DCNGEC / PARAM
!
      CHARACTER(LEN=144),EXTERNAL :: MYCARLU
      CHARACTER(LEN=MAXAIDELEN), EXTERNAL :: MYAIDELU
      INTEGER, EXTERNAL :: INTLU
      LOGICAL, EXTERNAL :: LOGLU
      DOUBLE PRECISION, EXTERNAL  :: REALU
      INTEGER, EXTERNAL :: NEXT,PREV,PREVAL,LONGLU
      !
!
      NFIC = 666
      WRITE(*,*) '---- READING PROCESS ----'
      WRITE(*,*) 'READING: ',TRIM(FILENAME)
      OPEN(NFIC,FILE=FILENAME,IOSTAT=IERR)
      CALL CHECK_CALL(IERR,'OPEN:DICO')
      !
      ARRET   = .FALSE.
      ERREUR  = .FALSE.
      RETOUR  = .FALSE.
      DYNAM   = .FALSE.
      EXECMD  = .FALSE.
      AIDLNG  = .FALSE.
      VUMOT   = .FALSE.
      LONGLI  = 72
      PTVIRG  = ';'
      QUOTE   = ''''
!     TABUL   = CHAR(9)
      NBMOT   = 0
      NIGN    = 0
      ORDRE   = 0
      PARAM  = ' '
      LONGU  = 0
      NTYP   = -100
      INDX   =  123456
      ITAI   = -100
      DEFLU  = 0
!
      ICOL   = LONGLI
      NLIGN = 0
!
! SEEKS THE FIRST NON-WHITE CHARACTER (IGNORES COMMENTED LINES) :
!
      ICOL = NEXT(ICOL+1,LIGNE)
      IKEY = 0
!
100   CONTINUE
      ! New keyword
!
! IF REACHED THE END OF FILE :
!
      IF(RETOUR) GO TO 900
!
! LOCATES THE COMMANDS STARTING WITH &
!
      IF ( LIGNE(ICOL:ICOL).EQ.'&' ) THEN
        WRITE(*,*) 'SKIPPING COMMAND: ',LIGNE
        ICOL = PREVAL(ICOL+1,LIGNE,' ',CHAR(9),' ')
        ICOL = NEXT(ICOL+1,LIGNE)
        GO TO 100
      ENDIF
!
      I2 = PREVAL(ICOL+1,LIGNE,'=',':','=')
!     CASE WHERE '=' IS ON THE FOLLOWING LINE
      IF(I2.GT.LONGLI) I2=LONGLI
      JCOL = PREV  (I2,LIGNE)
      ILONG = JCOL - ICOL + 1
!
!     write(*,*) 'treating: *',ligne,"*"
      ! Identify the type of keyword
      CALL IDENTIFY_KEY(LIGNE,ILONG,NUMERO,LNG)
!
!     STOPS IF THE WORD IS UNKNOWN
      IF(NUMERO.LE.0) THEN
        WRITE(*,*) 'UNKNOWN KEY: ',LIGNE(ICOL:JCOL)
        CALL PLANTE(1)
        STOP
      ENDIF
!     New keyword
      IF((NUMERO.EQ.1).AND.(LNG.EQ.1)) THEN
        IKEY = IKEY + 1
        ! Initialising the keyword structure
        MYDICO(IKEY)%KNOM(1) = REPEAT(' ',LEN(MYDICO(IKEY)%KNOM(1)))
        MYDICO(IKEY)%KNOM(2) = REPEAT(' ',LEN(MYDICO(IKEY)%KNOM(2)))
        MYDICO(IKEY)%KTYPE = 0
        MYDICO(IKEY)%KINDEX = -1
        MYDICO(IKEY)%MNEMO = REPEAT(' ',LEN(MYDICO(IKEY)%MNEMO))
        MYDICO(IKEY)%TAILLE = -1
        MYDICO(IKEY)%SUBMIT = REPEAT(' ',LEN(MYDICO(IKEY)%SUBMIT))
        MYDICO(IKEY)%DEFAUT(1) = REPEAT(' ',LEN(MYDICO(IKEY)%DEFAUT(1)))
        MYDICO(IKEY)%DEFAUT(2) = REPEAT(' ',LEN(MYDICO(IKEY)%DEFAUT(2)))
        MYDICO(IKEY)%CHOIX(1) = REPEAT(' ',LEN(MYDICO(IKEY)%CHOIX(1)))
        MYDICO(IKEY)%CHOIX(2) = REPEAT(' ',LEN(MYDICO(IKEY)%CHOIX(2)))
        DO I=1,3
        MYDICO(IKEY)%RUBRIQUE(1,I) =
     &            REPEAT(' ',LEN(MYDICO(IKEY)%RUBRIQUE(1,I)))
        MYDICO(IKEY)%RUBRIQUE(2,I) =
     &            REPEAT(' ',LEN(MYDICO(IKEY)%RUBRIQUE(2,I)))
        ENDDO
        MYDICO(IKEY)%SUBMIT = REPEAT(' ',LEN(MYDICO(IKEY)%SUBMIT))
        MYDICO(IKEY)%COMPOSE = REPEAT(' ',LEN(MYDICO(IKEY)%COMPOSE))
        MYDICO(IKEY)%COMPORT = REPEAT(' ',LEN(MYDICO(IKEY)%COMPORT))
        MYDICO(IKEY)%CONTROLE = REPEAT(' ',LEN(MYDICO(IKEY)%CONTROLE))
        MYDICO(IKEY)%APPARENCE = REPEAT(' ',LEN(MYDICO(IKEY)%APPARENCE))
        MYDICO(IKEY)%NIVEAU = 0
        MYDICO(IKEY)%AIDE(1) = REPEAT(' ',LEN(MYDICO(IKEY)%AIDE(1)))
        MYDICO(IKEY)%AIDE(2) = REPEAT(' ',LEN(MYDICO(IKEY)%AIDE(2)))
      ENDIF
!
      ICOL = PREVAL(ICOL+1,LIGNE,'=',':','=')
!     CASE WHERE '=' IS ON THE FOLLOWING LINE
      IF(ICOL.GT.LONGLI) THEN
        ICOL  = NEXT(LONGLI,LIGNE)
        IF(RETOUR) GO TO 900
      ENDIF
!
!
!    2) RESERVED KEYWORDS:
!
!    RESERVED KEYWORDS CURRENTLY ARE:
!
!           'NOM'       :NUMERO = 1  (DE TYPE CARACTERE)
!           'TYPE'      :NUMERO = 2  (DE TYPE CARACTERE)
!           'INDEX'     :NUMERO = 3  (DE TYPE ENTIER)
!           'TAILLE'    :NUMERO = 4  (DE TYPE ENTIER)
!           'DEFAUT'    :NUMERO = 5  (DE TYPE VARIABLE)
!           'AIDE'      :NUMERO = 6  (DE TYPE CARACTERE)
!           'CHOIX'     :NUMERO = 7  (DE TYPE VARIABLE)
!           'RUBRIQUE'  :NUMERO = 8  (DE TYPE CARACTERE)
!           'NIVEAU'    :NUMERO = 9  (DE TYPE ENTIER)
!           'MNEMO'     :NUMERO = 10 (DE TYPE CARACTERE)
!           'COMPOSE'   :NUMERO = 11 (DE TYPE CARACTERE)
!           'COMPORT'   :NUMERO = 12 (DE TYPE CARACTERE)
!           'CONTROLE'  :NUMERO = 13 (DE TYPE ENTIER)
!           'APPARENCE' :NUMERO = 14 (DE TYPE CARACTERE)
!           'SUBMIT'    :NUMERO = 15 (DE TYPE CARACTERE)
!
!      NAME
!
          IF(NUMERO.EQ.1) THEN
!
! SHOULD NOT COUNT THE SAME WORD IN SEVERAL LANGUAGES SEVERAL TIMES
! COUNTED ONLY ONCE IN FIRST FOUND LANGUAGE
!
            IF (.NOT.(VUMOT)) NBMOT = NBMOT + 1
!
            ORDRE = 1
!
! SIGNALS THAT THIS NEW KEYWORD WAS ALREADY ENCOUNTERED IN ANOTHER LANGUAGE
            IF (.NOT.(VUMOT)) VUMOT=.TRUE.
!
!           NAME OF THE KEYWORD
            TMP= MYCARLU(LCAR,ICOL,LIGNE,QUOTE,LEN(PARAM))
            LONGU = LCAR
            MYDICO(IKEY)%KNOM(LNG)=TMP(1:MIN(72,LONGU))
!
            ICOL = NEXT(ICOL+1,LIGNE)
!
!    TYPE
!
          ELSE IF(NUMERO.EQ.2) THEN
            VUMOT = .FALSE.
            IF (ORDRE.NE.1) GOTO 1500
            ORDRE=2
            TYPE2= MYCARLU(LCAR,ICOL,LIGNE,QUOTE,LEN(TYPE))
            TYPE=TYPE2(1:MIN(LCAR,9))
            IF(TYPE(1:6).EQ.'ENTIER'
     &      .OR.TYPE(1:8).EQ.'INTEGER') THEN
              NTYP = 1
            ELSEIF(TYPE(1:4).EQ.'REEL'
     &      .OR.TYPE(1:4).EQ.'REAL') THEN
              NTYP = 2
            ELSEIF(TYPE(1:7).EQ.'LOGIQUE'
     &      .OR.TYPE(1:7).EQ.'LOGICAL') THEN
              NTYP = 3
            ELSEIF(TYPE(1:9).EQ.'CARACTERE'
     &      .OR.TYPE(1:6).EQ.'STRING') THEN
              NTYP = 4
            ELSE
!           ERROR: UNKNOWN TYPE
              IF(LNG.EQ.1) WRITE (LU,1002) LIGNE
              IF(LNG.EQ.2) WRITE (LU,1003) LIGNE
1002          FORMAT(1X,A72,/,1X,'TYPE INCONNU SUR CETTE LIGNE')
1003          FORMAT(1X,A72,/,1X,'UNKNOWN TYPE ON THIS LINE')
              CALL PLANTE(1)
              STOP
            ENDIF
            MYDICO(IKEY)%KTYPE = NTYP
            ICOL = NEXT(ICOL+1,LIGNE)
!
!    INDEX
!
          ELSE IF(NUMERO.EQ.3) THEN
            IF (ORDRE.NE.2) GOTO 1500
            ORDRE=3
            INDX = INTLU(ICOL,LIGNE)
            ICOL = NEXT(ICOL+1,LIGNE)
!
! CASE INDEX=-1 : WORD FOR EDAMOX CONSTRUCTION, TO KEEP
!
            IF (INDX.EQ.-1) THEN
              NIGN = NIGN + 1
              IF (NIGN.GT.100) THEN
                IF (LNG.EQ.1) THEN
                  WRITE(LU,*) 'TROP DE MOTS RESERVES POUR EDAMOX',
     &                        ' (100 AU MAXIMUM)'
                ELSEIF (LNG.EQ.2) THEN
                  WRITE(LU,*) 'TOO MANY WORDS FOR EDAMOX',
     &                        ' (MAX=100)'
                ENDIF
                ERREUR = .TRUE.
                GO TO 900
              ENDIF
            ENDIF
            MYDICO(IKEY)%KINDEX = INDX
!
!    SIZE
!
          ELSE IF(NUMERO.EQ.4) THEN
            IF (ORDRE.NE.3) GOTO 1500
            ORDRE=4
            MYDICO(IKEY)%TAILLE = INTLU(ICOL,LIGNE)
            ICOL = NEXT(ICOL+1,LIGNE)
!
!    DEFAULT VALUE
!    FOR ARRAYS, IT IS NOT NECESSARY TO SET ALL VALUES
!
          ELSE IF(NUMERO.EQ.5) THEN
!
!
            IF (ORDRE.LT.3.OR.ORDRE.GT.6) GOTO 1500
            ORDRE=6
            MYDICO(IKEY)%DEFAUT(LNG) = MYCARLU(LCAR,ICOL,LIGNE,QUOTE,
     &                     LEN(MYDICO(IKEY)%DEFAUT(LNG)))

!
            ICOL = NEXT(ICOL+1,LIGNE)

            DO
              IF(MYDICO(IKEY)%TAILLE.LE.1) EXIT
              ! Check if there are multiple values (array)
              IF(ICOL.GT.LONGLI) THEN
                ICOL = LONGLI
              ELSE
                IF(LIGNE(ICOL:ICOL).NE.PTVIRG) THEN
                  EXIT
                ENDIF
              ENDIF
              MYDICO(IKEY)%DEFAUT(LNG) = TRIM(MYDICO(IKEY)%DEFAUT(LNG))
     &            //PTVIRG//   MYCARLU(LCAR,ICOL,LIGNE,QUOTE,
     &                       LEN(MYDICO(IKEY)%DEFAUT(LNG)))

!
              ICOL = NEXT(ICOL+1,LIGNE)
            ENDDO
!
!    HELP
!
          ELSE IF(NUMERO.EQ.6) THEN
!
            MYDICO(IKEY)%AIDE(LNG) =
     &               MYAIDELU(ICOL,LIGNE)
!
!
!    'CHOIX' 'RUBRIQUE' 'NIVEAU' 'MNEMO' 'COMPOSE' 'COMPORT' 'CONTROLE' 'APPARENCE'
!    NUMBER 7 TO 14 INCLUDED
!
          ELSE IF(NUMERO.EQ.7) THEN
!
            MYDICO(IKEY)%CHOIX(LNG) = MYCARLU(LCAR,ICOL,LIGNE,QUOTE,
     &                     LEN(MYDICO(IKEY)%CHOIX(LNG)))

!
            ICOL = NEXT(ICOL+1,LIGNE)
            DO
              ! Check if there are multiple values (array)
              IF(ICOL.GT.LONGLI) THEN
                ICOL = LONGLI
              ELSE
                IF(LIGNE(ICOL:ICOL).NE.PTVIRG) THEN
                  EXIT
                ENDIF
              ENDIF
              MYDICO(IKEY)%CHOIX(LNG) =
     &            TRIM(MYDICO(IKEY)%CHOIX(LNG))
     &            //PTVIRG//MYCARLU(LCAR,ICOL,LIGNE,QUOTE,
     &                       LEN(MYDICO(IKEY)%CHOIX(LNG)))

!
              ICOL = NEXT(ICOL+1,LIGNE)
            ENDDO
!
          ELSE IF(NUMERO.EQ.8) THEN
!
            I = 1
            MYDICO(IKEY)%RUBRIQUE(LNG,I)= MYCARLU(LCAR,ICOL,LIGNE,QUOTE,
     &                     LEN(MYDICO(IKEY)%RUBRIQUE(LNG,I)))

!
            ICOL = NEXT(ICOL+1,LIGNE)
            DO
              ! Check if there are multiple values (array)
              IF(ICOL.GT.LONGLI) THEN
                ICOL = LONGLI
              ELSE
                IF(LIGNE(ICOL:ICOL).NE.PTVIRG) THEN
                  EXIT
                ENDIF
              ENDIF
              I = I + 1
              MYDICO(IKEY)%RUBRIQUE(LNG,I) =
     &            MYCARLU(LCAR,ICOL,LIGNE,QUOTE,
     &                       LEN(MYDICO(IKEY)%RUBRIQUE(LNG,I)))

!
              ICOL = NEXT(ICOL+1,LIGNE)
            ENDDO
!
          ELSE IF(NUMERO.EQ.9) THEN
!
            MYDICO(IKEY)%NIVEAU = INTLU(ICOL,LIGNE)
            ICOL = NEXT(ICOL+1,LIGNE)
!
          ELSE IF(NUMERO.EQ.10) THEN
!
            MYDICO(IKEY)%MNEMO =
     &               MYCARLU(LCAR,ICOL,LIGNE,QUOTE,
     &                     LEN(MYDICO(IKEY)%MNEMO))
            ICOL = NEXT(ICOL+1,LIGNE)
!
          ELSE IF(NUMERO.EQ.11) THEN
!
            MYDICO(IKEY)%COMPOSE =
     &               MYCARLU(LCAR,ICOL,LIGNE,QUOTE,
     &                     LEN(MYDICO(IKEY)%COMPOSE))
            ICOL = NEXT(ICOL+1,LIGNE)
!
          ELSE IF(NUMERO.EQ.12) THEN
!
            MYDICO(IKEY)%COMPORT =
     &               MYAIDELU(ICOL,LIGNE)
!
          ELSE IF(NUMERO.EQ.13) THEN
!
            MYDICO(IKEY)%CONTROLE =
     &               MYAIDELU(ICOL,LIGNE)
!
          ELSE IF(NUMERO.EQ.14) THEN
!
            MYDICO(IKEY)%APPARENCE =
     &               MYCARLU(LCAR,ICOL,LIGNE,QUOTE,
     &                     LEN(MYDICO(IKEY)%APPARENCE))
            ICOL = NEXT(ICOL+1,LIGNE)
            DO
              ! Check if there are multiple values (array)
              IF(ICOL.GT.LONGLI) THEN
                ICOL = LONGLI
              ELSE
                IF(LIGNE(ICOL:ICOL).NE.PTVIRG) THEN
                  EXIT
                ENDIF
              ENDIF
              MYDICO(IKEY)%APPARENCE =
     &            TRIM(MYDICO(IKEY)%SUBMIT)
     &            //PTVIRG//MYCARLU(LCAR,ICOL,LIGNE,QUOTE,
     &                       LEN(MYDICO(IKEY)%APPARENCE))

!
              ICOL = NEXT(ICOL+1,LIGNE)
            ENDDO
!
!    DEFINES A SUBMIT TYPE
          ELSE IF (NUMERO .EQ. 15) THEN
            IF (ORDRE.NE.3.AND.ORDRE.NE.4) GOTO 1500
            ORDRE=5
!
!
            MYDICO(IKEY)%SUBMIT = MYCARLU(LCAR,ICOL,LIGNE,QUOTE,
     &                     LEN(MYDICO(IKEY)%SUBMIT))

!
            ICOL = NEXT(ICOL+1,LIGNE)
            DO
              ! Check if there are multiple values (array)
              IF(ICOL.GT.LONGLI) THEN
                ICOL = LONGLI
              ELSE
                IF(LIGNE(ICOL:ICOL).NE.PTVIRG) THEN
                  EXIT
                ENDIF
              ENDIF
              MYDICO(IKEY)%SUBMIT =
     &            TRIM(MYDICO(IKEY)%SUBMIT)
     &            //PTVIRG//MYCARLU(LCAR,ICOL,LIGNE,QUOTE,
     &                       LEN(MYDICO(IKEY)%SUBMIT))

!
              ICOL = NEXT(ICOL+1,LIGNE)
            ENDDO
!
        ENDIF
!
!
      GO TO 100
900   CONTINUE
      IF(ERREUR) THEN
        WRITE(LU,*)' '
        WRITE(LU,*)'-------------------------------'
        IF(LNG.EQ.1) THEN
        WRITE(LU,*)'- ERREUR DANS LE DICTIONNAIRE -'
        ENDIF
        IF(LNG.EQ.2) THEN
        WRITE(LU,*)'- ERROR IN THE DICTIONARY     -'
        ENDIF
        WRITE(LU,*)'-------------------------------'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     TRUE END: 2 FILES READ OR 2 FILES IN 1 READ
      CLOSE(NFIC)
      WRITE(*,*) ''
      WRITE(*,*) 'TOTAL NUMBER OF KEY IN THE DICTIONARY: ',IKEY
      WRITE(*,*) ''
      NKEY = IKEY

      ! List all rubriques
      CALL IDENTIFY_RUBRIQUE()
      ! Verification for user
      WRITE(*,*) '---- CHECKING RUBRIQUES ----'
      WRITE(*,*) 'CHECK OF TRANSLATION FOR RUBRIQUE'
      WRITE(*,*) 'NRUB',NRUB
      IF(NRUB(1).NE.NRUB(2)) THEN
        WRITE(*,*) 'WARNING: NOT THE SAME NUMBER OF RUBRIQUES ',
     &             'IN FRENCH AND ENGLISH'
      ENDIF
      WRITE(*,*) 'LIST OF RUBRIQUES IN BOTH LANGUAGES'
      DO I=1,MINVAL(NRUB)
        WRITE(*,*) TRIM(RUBRIQUE(I,1))," = ",TRIM(RUBRIQUE(I,2))
      ENDDO
      WRITE(*,*) ''
      CALL CHECK_INDEX()
!
      RETURN
!
! TREATS ERRORS OF DECLARATION ORDER IN THE DICTIONARY
!
1500  ERREUR=.TRUE.
      WRITE(LU,'(/,1X,A72,/)') LIGNE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'A LA LIGNE ',NLIGN,
     &               ', ORDRE DE DEFINITION OBLIGATOIRE NON RESPECTE'
        WRITE(LU,*)
        WRITE(LU,*) 'L''ORDRE ATTENDU EST LE SUIVANT :'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'AT LINE ',NLIGN,', PRIORITY ORDER NOT RESPECTED'
        WRITE(LU,*)
        WRITE(LU,*) 'EXPECTED ORDER IS :'
      ENDIF
      WRITE(LU,*) 'NOM, TYPE, INDEX, (TAILLE), (SUBMIT), (DEFAUT)'
      CALL PLANTE(1)
      STOP
      GOTO 900

      END SUBROUTINE
      ! brief Dump the myDico structure
      !
      ! param filename name of the output file
      ! param
      SUBROUTINE DUMP_DICTIONARY(FILENAME)
      !
      IMPLICIT NONE
      !
      CHARACTER(LEN=144), INTENT(IN) :: FILENAME
      INTEGER :: NFIC,I,J,IERR
!
      NFIC = 666
      WRITE(*,*) '---- DUMPING PROCESS ----'
      WRITE(*,*) 'DUMPING IN : ',TRIM(FILENAME)
      OPEN(NFIC,FILE=TRIM(FILENAME),IOSTAT=IERR)
      CALL CHECK_CALL(IERR,'DUMP_DICTIONARY')
      WRITE(*,*) ''
      WRITE(*,*) 'TOTAL NUMBER OF KEY IN THE DICTIONARY: ',NKEY
      WRITE(*,*) ''
      ! Loop on all the keywords
      DO I=1,NKEY
        WRITE(NFIC,'(A,A,A)') "NOM = '",TRIM(MYDICO(I)%KNOM(1)),"'"
        WRITE(NFIC,'(A,A,A)') "NOM1 = '",TRIM(MYDICO(I)%KNOM(2)),"'"
        SELECT CASE(MYDICO(I)%KTYPE)
        CASE(1) ! INTEGER
        WRITE(NFIC,'(A)') "TYPE = INTEGER"
        CASE(2) ! REAL
        WRITE(NFIC,'(A)') "TYPE = REAL"
        CASE(3) ! LOGICAL
        WRITE(NFIC,'(A)') "TYPE = LOGICAL"
        CASE(4) ! CHARACTER
        WRITE(NFIC,'(A)') "TYPE = STRING"
        END SELECT
        WRITE(NFIC,'(A,I3)') "INDEX = ",MYDICO(I)%KINDEX
        IF(MYDICO(I)%TAILLE.NE.-1) THEN
          WRITE(NFIC,'(A,I1)') "TAILLE = ",MYDICO(I)%TAILLE
        ENDIF
        IF(MYDICO(I)%SUBMIT(1:1).NE." ") THEN
          WRITE(NFIC,'(A,A,A)') "SUBMIT = '",TRIM(MYDICO(I)%SUBMIT),"'"
        ENDIF
        WRITE(NFIC,'(A,A)') "DEFAUT = ",TRIM(MYDICO(I)%DEFAUT(1))
        WRITE(NFIC,'(A,A)') "DEFAUT1 = ",TRIM(MYDICO(I)%DEFAUT(2))
        WRITE(NFIC,'(A,A,A)') "MNEMO = '",TRIM(MYDICO(I)%MNEMO),"'"
        IF(MYDICO(I)%CONTROLE(1:1).NE.' ') THEN
          WRITE(NFIC,'(A,A)') "CONTROLE = ",
     &                       TRIM(MYDICO(I)%CONTROLE)
        ENDIF
        IF(MYDICO(I)%CHOIX(1)(1:1).NE." ") THEN
          WRITE(NFIC,'(A,A,A)') "CHOIX = '",TRIM(MYDICO(I)%CHOIX(1)),"'"
        ENDIF
        IF(MYDICO(I)%CHOIX(2)(1:1).NE." ") THEN
          WRITE(NFIC,'(A,A,A)') "CHOIX1 = '",
     &           TRIM(MYDICO(I)%CHOIX(2)),"'"
        ENDIF
        IF(MYDICO(I)%APPARENCE(1:1).NE." ") THEN
          WRITE(NFIC,'(A)') "APPARENCE = "
          WRITE(NFIC,'(A,A,A)') "'",TRIM(MYDICO(I)%APPARENCE),"'"
        ENDIF
        WRITE(NFIC,'(A,3(A,A,A))') "RUBRIQUE = ",
     &             ("'",TRIM(MYDICO(I)%RUBRIQUE(1,J)),"';",J=1,3)
        WRITE(NFIC,'(A,3(A,A,A))') "RUBRIQUE1 = ",
     &             ("'",TRIM(MYDICO(I)%RUBRIQUE(2,J)),"';",J=1,3)
        IF(MYDICO(I)%COMPOSE(1:1).NE." ") THEN
          WRITE(NFIC,'(A,A,A)') "COMPOSE = '",
     &          TRIM(MYDICO(I)%COMPOSE),"'"
        ENDIF
        IF(MYDICO(I)%COMPORT(1:1).NE." ") THEN
          WRITE(NFIC,'(A)') "COMPORT ="
          WRITE(NFIC,'(A,A,A)') "'",TRIM(MYDICO(I)%COMPORT),"'"
        ENDIF
        WRITE(NFIC,'(A,I1)') "NIVEAU = ",MYDICO(I)%NIVEAU
        WRITE(NFIC,'(A)') "AIDE ="
        WRITE(NFIC,'(A,A,A)') "'",TRIM(MYDICO(I)%AIDE(1)),"'"
        WRITE(NFIC,'(A)') "AIDE1 ="
        WRITE(NFIC,'(A,A,A)') "'",TRIM(MYDICO(I)%AIDE(2)),"'"
        WRITE(NFIC,'(A)') "/"
      ENDDO
      CLOSE(NFIC)
!
      END SUBROUTINE
      ! brief Dump the myDico structure
      !
      ! param filename name of the output file
      ! param
      SUBROUTINE WRITE2LATEX(FILENAME,LNG)
      !
      IMPLICIT NONE
      !
      CHARACTER(LEN=144), INTENT(IN) :: FILENAME
      INTEGER, INTENT(IN) :: LNG
      !
      INTEGER :: NFIC,I,J,IERR,IKEY,IRUB,ILNG
      LOGICAL :: HAS
      CHARACTER(LEN=144), ALLOCATABLE :: TO_SORT1(:),TO_SORT2(:)
      INTEGER, ALLOCATABLE :: ORDERED_KEY1(:),ORDERED_KEY2(:)
      INTEGER, ALLOCATABLE :: ORDERED_RUB(:)
!
      NFIC = 666
      WRITE(*,*) '---- LATEX PROCESS ----'
      WRITE(*,*) 'WRITING IN : ',TRIM(FILENAME)
      OPEN(NFIC,FILE=TRIM(FILENAME),IOSTAT=IERR)
      CALL CHECK_CALL(IERR,'WRITE2LATEX')
      WRITE(*,*) ''
      WRITE(*,*) 'TOTAL NUMBER OF KEY IN THE DICTIONARY: ',NKEY
      WRITE(*,*) ''
      !
      ! First Chapter the list of all keywords with informations
      !
      WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
      IF(LNG.EQ.1) THEN
        WRITE(NFIC,'(A)') '\chapter{Liste detaille des mots clefs}'
      ELSE
        WRITE(NFIC,'(A)') '\chapter{Detail list of keywords}'
      ENDIF
      WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
      WRITE(NFIC,'(A)') ' '
      !
      ! Sorting key words by alpahbetical order for each language
      !
      ALLOCATE(TO_SORT1(NKEY),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'TO_SORT1')
      ALLOCATE(TO_SORT2(NKEY),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'TO_SORT2')
      ALLOCATE(ORDERED_KEY1(NKEY),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'ORDERED_KEY1')
      ALLOCATE(ORDERED_KEY2(NKEY),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'ORDERED_KEY2')
      ALLOCATE(ORDERED_RUB(NKEY),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'ORDERED_RUB')
      ! Initialising list of keywords
      DO IKEY=1,NKEY
        TO_SORT1(IKEY) = REPEAT(' ',144)
        TO_SORT2(IKEY) = REPEAT(' ',144)
        TO_SORT1(IKEY)(1:72) = MYDICO(IKEY)%KNOM(LNG)
        TO_SORT2(IKEY)(1:72) = MYDICO(IKEY)%KNOM(3-LNG)
      ENDDO
      ! Sorting
      CALL SHELL_STRING(NKEY,TO_SORT1,ORDERED_KEY1)
      CALL SHELL_STRING(NKEY,TO_SORT2,ORDERED_KEY2)
      DEALLOCATE(TO_SORT1)
      DEALLOCATE(TO_SORT2)
      ! Looping on ordered keywords
      DO I=1,NKEY
        IKEY = ORDERED_KEY1(I)
        ! Name of the keywords
        WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
        WRITE(NFIC,'(3A)') "\section{",TRIM(MYDICO(IKEY)%KNOM(LNG)),"}"
        WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
        WRITE(NFIC,'(A)') " "
        ! The other informations are in an array
        WRITE(NFIC,'(A)') "\begin{tabular}{ll}"
        ! Type
        SELECT CASE(MYDICO(IKEY)%KTYPE)
        CASE(1) ! INTEGER
          IF(LNG.EQ.1) THEN
            WRITE(NFIC,'(A)') "Type : & Entier\\"
          ELSE
            WRITE(NFIC,'(A)') "Type : & Integer\\"
          ENDIF
        CASE(2) ! REAL
          IF(LNG.EQ.1) THEN
            WRITE(NFIC,'(A)') "Type : & Réel\\"
          ELSE
            WRITE(NFIC,'(A)') "Type : & Real\\"
          ENDIF
        CASE(3) ! LOGICAL
          IF(LNG.EQ.1) THEN
            WRITE(NFIC,'(A)') "Type : & Logique\\"
          ELSE
            WRITE(NFIC,'(A)') "Type : & Logical\\"
          ENDIF
        CASE(4) ! CHARACTER
          IF(LNG.EQ.1) THEN
            WRITE(NFIC,'(A)') "Type : & Caractère\\"
          ELSE
            WRITE(NFIC,'(A)') "Type : & String\\"
          ENDIF
        END SELECT
        ! Size
        IF(LNG.EQ.1) THEN
          WRITE(NFIC,'(A,I2,A)') "Taille : & ",MYDICO(IKEY)%TAILLE,"\\"
        ELSE
          WRITE(NFIC,'(A,I2,A)')
     &           "Dimension : & ",MYDICO(IKEY)%TAILLE,"\\"
        ENDIF
        ! Mnemo
        WRITE(NFIC,'(A)') "Mnemo & ",TRIM(MYDICO(IKEY)%MNEMO),"\\"
        ! Default values
        CALL WRITE_DEFAULT(NFIC,IKEY,LNG)
        ! And the name of the keyword in the other language
        IF(LNG.EQ.1) THEN
          WRITE(NFIC,'(3A)') "Mot cles anglais : & ",
     &                         TRIM(MYDICO(IKEY)%KNOM(2)),"\\"
        ELSE
          WRITE(NFIC,'(3A)') "French keyword : & \telkey{",
     &                         TRIM(MYDICO(IKEY)%KNOM(1)),"}\\"
        ENDIF
        write(nfic,'(a)') "\end{tabular}"
        WRITE(NFIC,'(A)') "\\"
        ! The help informations
        IF(MYDICO(IKEY)%AIDE(LNG)(1:3).EQ.'  ') THEN
          WRITE(NFIC,'(A)') 'TODO: WRITE HELP FOR THAT KEYWORD'
        ELSE
          WRITE(*,*) TRIM(MYDICO(IKEY)%AIDE(LNG))
          WRITE(NFIC,'(A)') TRIM(MYDICO(IKEY)%AIDE(LNG))
        ENDIF
        WRITE(NFIC,'(A)') "%"
      ENDDO
!
      ! Sorting rubriques
      ALLOCATE(TO_SORT1(NRUB(LNG)),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'TO_SORT1')
      DO IRUB=1,NRUB(LNG)
        TO_SORT1(IRUB) = REPEAT(' ',144)
        TO_SORT1(IRUB) = RUBRIQUE(IRUB,LNG)
      ENDDO
      CALL SHELL_STRING(NRUB(LNG),TO_SORT1,ORDERED_RUB)
      DEALLOCATE(TO_SORT1)
!
      !
      ! Keywsords by rubriques
      !
      WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
      IF(LNG.EQ.1) THEN
        write(nfic,'(a)') '\chapter{Liste des mots clefs par rubrique}'
      ELSE
        WRITE(NFIC,'(A)')
     &    '\chapter{List of keywords classified according to type}'
      ENDIF
      WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
      WRITE(NFIC,'(A)') ' '

      ! Ordering rubriques
      ! Loop on rubriques
      DO I=1,NRUB(LNG)
        IRUB = ORDERED_RUB(I)
        WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
        WRITE(NFIC,'(3A)') "\section{",TRIM(RUBRIQUE(IRUB,LNG)),"}"
        WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
        WRITE(NFIC,'(A)') ' '
        DO J=1,NKEY
          IKEY = ORDERED_KEY1(J)
          CALL HAS_RUBRIQUE(HAS,IKEY,IRUB,LNG)
          IF(HAS) THEN
            WRITE(NFIC,'(3a)') "\telkey{",
     &               TRIM(MYDICO(IKEY)%KNOM(LNG)),"}\\"
          ENDIF
        ENDDO
        WRITE(NFIC,'(A)') " "
      ENDDO
!
      WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
      IF(LNG.EQ.1) THEN
        WRITE(NFIC,'(A)') '\chapter{Glossaire}'
      ELSE
        WRITE(NFIC,'(A)') '\chapter{Glossary}'
      ENDIF
      WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
      WRITE(NFIC,'(A)') ' '
      !
      ! Section list of keywords lng -> 3-lng
      !
      WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
      IF(LNG.EQ.1) THEN
        WRITE(NFIC,'(A)') '\section{Glossaire Francais/Anglais}'
      ELSE
        WRITE(NFIC,'(A)') '\section{English/French glossary}'
      ENDIF
      WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
      WRITE(NFIC,'(A)') ' '
      ! Keywords are written in a longatble
      WRITE(NFIC,'(A)') '\begin{longtable}'//
     &                 '{|p{0.5\linewidth}|p{0.5\linewidth}|}'
      WRITE(NFIC,'(A)') '\hline'
      DO I=1,NKEY
        IKEY = ORDERED_KEY1(I)
        WRITE(NFIC,'(5A)') "\telkey{",TRIM(MYDICO(IKEY)%KNOM(LNG)),
     &                  "} & \telkey{",TRIM(MYDICO(IKEY)%KNOM(3-LNG)),
     &                  "}\\"
        WRITE(NFIC,'(A)') '\hline'
      ENDDO
      WRITE(NFIC,'(A)') '\end{longtable}'
      WRITE(NFIC,'(A)') '%'
      !
      ! Section list of keywords 3-lng -> lng
      !
      WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
      IF(LNG.EQ.1) THEN
        WRITE(NFIC,'(A)') '\section{Glossaire Anglais/Francais}'
      ELSE
        WRITE(NFIC,'(A)') '\section{French/English glossary}'
      ENDIF
      WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
      WRITE(NFIC,'(A)') ' '
      ! Keywords are written in a longatble
      WRITE(NFIC,'(A)') '\begin{longtable}'//
     &                 '{|p{0.5\linewidth}|p{0.5\linewidth}|}'
      WRITE(NFIC,'(A)') '\hline'
      DO I=1,NKEY
        IKEY = ORDERED_KEY2(I)
        WRITE(NFIC,'(5A)') "\telkey{",TRIM(MYDICO(IKEY)%KNOM(3-LNG)),
     &                  "} & \telkey{",TRIM(MYDICO(IKEY)%KNOM(LNG)),
     &                  "}\\"
        WRITE(NFIC,'(A)') '\hline'
      ENDDO
      WRITE(NFIC,'(A)') '\end{longtable}'

      CLOSE(NFIC)
!
      END SUBROUTINE
      END MODULE
