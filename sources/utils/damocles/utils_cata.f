!                    *****************
                     MODULE UTILS_CATA
!                    *****************
!
!***********************************************************************
! DAMOCLES   V7P2
!***********************************************************************
!
!brief Module containing the function to write a List of keyword
!+     into a Eficas Catalog
!
!history  S.E.BOURBAN (HRW)
!+        16/03/2017
!+        V7P3
!+   Reformatting some of the comments -- A lot more is to be done.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DICO_DATA
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
! Array containing true if a key word was written in the catalogue
!
      LOGICAL, ALLOCATABLE :: KEY_WRITTEN(:)
!
      CONTAINS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! Build the Eficas catalog into string from DEFAUT in
! dictionary
!     param ikey id of the keyword to handle
!     param lng The language to handle
!
      CHARACTER(LEN=KEYWORD_LEN) FUNCTION CATA_TYP(IKEY, LNG)
!
!-----------------------------------------------------------------------
        IMPLICIT NONE
!
        INTEGER, INTENT(IN) :: IKEY
        INTEGER, INTENT(IN) :: LNG
!
        INTEGER I
        CHARACTER(LEN=1) :: TYP
!
!-----------------------------------------------------------------------
        CATA_TYP=REPEAT(' ',KEYWORD_LEN)
        IF(MYDICO(IKEY)%KTYPE.EQ.1) THEN
          CATA_TYP = "'I'"
          TYP = 'I'
        ELSE IF(MYDICO(IKEY)%KTYPE.EQ.2) THEN
          CATA_TYP = "'R'"
          TYP = 'R'
        ELSE IF(MYDICO(IKEY)%KTYPE.EQ.3) THEN
          CATA_TYP = 'bool'
        ELSE IF(MYDICO(IKEY)%KTYPE.EQ.4) THEN
          CATA_TYP = "'TXM'"
! If the keyword is a file changing type accordingly
          IF(MYDICO(IKEY)%SUBMIT(1:1).NE.' ') THEN
! Check if it is an input or output file
            IF (INDEX(MYDICO(IKEY)%SUBMIT,'LIT').NE.0) THEN
              CATA_TYP="('Fichier','All Files (*)')"
            ELSE
              CATA_TYP="('Fichier','All Files (*)','Sauvegarde')"
            ENDIF
          ENDIF
        ELSE
! SHOULD NOT HAPPEN
          WRITE(*,*) 'AY CARAMBA'
          WRITE(*,*) IKEY, MYDICO(IKEY)%KNOM(LNG)
          CALL PLANTE(1)
          STOP
        ENDIF
! If we have a into switching to TXM
        IF(MYDICO(IKEY)%CHOIX(LNG)(1:1).NE.' ') THEN
            CATA_TYP = "'TXM'"
        ENDIF
! Defining size of variables
        IF(MYDICO(IKEY)%APPARENCE(1:4).EQ.'LIST') THEN
          CATA_TYP = TRIM(CATA_TYP)//", max='**'"
        ELSE IF (MYDICO(IKEY)%APPARENCE(1:5).EQ.'TUPLE') THEN
          CATA_TYP = "Tuple(2), min= 2, max='**' ,"//
     &               "validators = VerifTypeTuple(('"//
     &                TYP//"','"//TYP//"'))"
        ELSE IF (MYDICO(IKEY)%APPARENCE(1:7).EQ.'DYNLIST') THEN
          CATA_TYP = TRIM(CATA_TYP)//", min=0, max='**'"
        ELSE
          IF (MYDICO(IKEY)%TAILLE.GT.1) THEN
            WRITE(CATA_TYP,'(A,I2,A,I2)') TRIM(CATA_TYP)//", min=",
     &                        MYDICO(IKEY)%TAILLE,", max=",
     &                        MYDICO(IKEY)%TAILLE
          ENDIF
        ENDIF
!-----------------------------------------------------------------------
!
      END FUNCTION
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! Returns a Python Boola associated to a logical in dico
!
!     param ikey id of the keyword to handle
!     param lng The language to handle
!
      CHARACTER(LEN=5) FUNCTION LOGI2BOOL(STRING)
!
!-----------------------------------------------------------------------
        IMPLICIT NONE
!
        CHARACTER(LEN=*), INTENT(IN) :: STRING
!
!-----------------------------------------------------------------------
        SELECT CASE (TRIM(STRING))
        CASE('NO','NON','FAUX','FALSE','.FALSE.','0')
          LOGI2BOOL = 'False'
        CASE('VRAI','OUI','TRUE','YES','.TRUE.','1')
          LOGI2BOOL = 'True '
        CASE DEFAULT
          WRITE(*,*) 'Unknown value for boolean: ',TRIM(STRING)
          CALL PLANTE(1)
        END SELECT
!-----------------------------------------------------------------------
!
      END FUNCTION
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! Build the Eficas catalog into string from DEFAUT in dictionary
!
!     param ikey id of the keyword to handle
!     param lng The language to handle
!
      CHARACTER(LEN=DEFAUT_LEN) FUNCTION CATA_DEFAUT(IKEY, LNG)
!
!-----------------------------------------------------------------------
        IMPLICIT NONE
!
        INTEGER, INTENT(IN) :: IKEY
        INTEGER, INTENT(IN) :: LNG
!
        INTEGER I
        INTEGER :: MYLEN,POS_WORD,POS_EGAL,IDX
        CHARACTER(LEN=KEYWORD_LEN) :: TMP
!
!-----------------------------------------------------------------------
        CATA_DEFAUT = REPEAT(' ',DEFAUT_LEN)
! HANDLE DEFAULT VALUES FOR HASH
      IF(MYDICO(IKEY)%HASH_ID(1,LNG)(1:1).NE.' '.AND.
     &   MYDICO(IKEY)%DEFAUT(EN)(1:1).NE.' ') THEN
          IF(MYDICO(IKEY)%TAILLE.NE.1) THEN
! LOOP ON VALUES
            MYLEN = LEN(TRIM(MYDICO(IKEY)%DEFAUT(LNG)))
            CATA_DEFAUT = "["
            IDX = 1
            POS_WORD = 1
            DO WHILE(IDX.NE.0)
              IDX = INDEX(MYDICO(IKEY)%DEFAUT(LNG)(POS_WORD:MYLEN),";")
! CHECK IF IT IS THE LAST ONE
              IF(IDX.EQ.0) THEN
! NEED TO USE A TMP STRING OTHERWISE THE WHOLE STRING IS
! PASSED
                TMP = REPEAT(' ',KEYWORD_LEN)
                TMP(1:(MYLEN-POS_WORD)+1) =
     &                MYDICO(IKEY)%DEFAUT(LNG)(POS_WORD:MYLEN)
                CATA_DEFAUT = TRIM(CATA_DEFAUT)//
     &                      TRIM(GET_HASH_VALUE(IKEY,TMP,LNG))
     &                      // "]"
              ELSE
                TMP = REPEAT(' ',KEYWORD_LEN)
                TMP(1:(IDX-1)) =
     &                MYDICO(IKEY)%DEFAUT(LNG)(POS_WORD:POS_WORD+IDX-2)
                CATA_DEFAUT = TRIM(CATA_DEFAUT) //
     &                    TRIM(GET_HASH_VALUE(IKEY,TMP,LNG))
     &                    //","
              ENDIF
              POS_WORD = POS_WORD +  IDX
            ENDDO
          ELSE
            CATA_DEFAUT = GET_HASH_VALUE(IKEY,
     &                                   MYDICO(IKEY)%DEFAUT(LNG),LNG)
          ENDIF
! CAS IF WE HAVE A STRING
        ELSE IF(MYDICO(IKEY)%KTYPE.EQ.4) THEN
          CATA_DEFAUT = "'"//TRIM(MYDICO(IKEY)%DEFAUT(LNG))//"'"
        ELSE IF(MYDICO(IKEY)%KTYPE.EQ.3) THEN
          IF(MYDICO(IKEY)%TAILLE.NE.1) THEN
! LOOP ON VALUES
            MYLEN = LEN(TRIM(MYDICO(IKEY)%DEFAUT(LNG)))
            CATA_DEFAUT = "["
            IDX = 1
            POS_WORD = 1
            DO WHILE(IDX.NE.0)
              IDX = INDEX(MYDICO(IKEY)%DEFAUT(LNG)(POS_WORD:MYLEN),";")
! CHECK IF IT IS THE LAST ONE
              IF(IDX.EQ.0) THEN
                CATA_DEFAUT = TRIM(CATA_DEFAUT)//
     &   LOGI2BOOL(MYDICO(IKEY)%DEFAUT(LNG)(POS_WORD:MYLEN))
     &                      // "]"
              ELSE
                CATA_DEFAUT = TRIM(CATA_DEFAUT) //
     &   LOGI2BOOL(MYDICO(IKEY)%DEFAUT(LNG)(POS_WORD:POS_WORD+IDX-2))
     &                    //","
              ENDIF
              POS_WORD = POS_WORD +  IDX
            ENDDO
          ELSE
            CATA_DEFAUT = LOGI2BOOL(MYDICO(IKEY)%DEFAUT(LNG))
          ENDIF
        ELSE
          IF(MYDICO(IKEY)%TAILLE.NE.1) THEN
            ! LOOP ON VALUES
            MYLEN = LEN(TRIM(MYDICO(IKEY)%DEFAUT(LNG)))
            CATA_DEFAUT = "["
            IDX = 1
            POS_WORD = 1
            DO WHILE(IDX.NE.0)
              IDX = INDEX(MYDICO(IKEY)%DEFAUT(LNG)(POS_WORD:MYLEN),";")
              ! CHECK IF IT IS THE LAST ONE
              IF(IDX.EQ.0) THEN
                CATA_DEFAUT = TRIM(CATA_DEFAUT)//
     &                    MYDICO(IKEY)%DEFAUT(LNG)(POS_WORD:MYLEN)
     &                    // "]"
              ELSE
                CATA_DEFAUT = TRIM(CATA_DEFAUT) //
     &                 MYDICO(IKEY)%DEFAUT(LNG)(POS_WORD:POS_WORD+IDX-2)
     &                  //","
              ENDIF
              POS_WORD = POS_WORD +  IDX
            ENDDO
          ELSE
            CATA_DEFAUT = TRIM(MYDICO(IKEY)%DEFAUT(LNG))
          ENDIF
        ENDIF
!-----------------------------------------------------------------------
!
      END FUNCTION
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! Build the Eficas catalog into string from CHOIX in dictionary
!
! param ikey id of the keyword to handle
! param lng The language to handle
!
      CHARACTER(LEN=CHOIX_LEN) FUNCTION CATA_INTO(IKEY, LNG)
!
!-----------------------------------------------------------------------
        IMPLICIT NONE
!
        INTEGER, INTENT(IN) :: IKEY
        INTEGER, INTENT(IN) :: LNG
!
        INTEGER :: MYLEN,POS_WORD,POS_EGAL,IDX
        INTEGER :: I
        CHARACTER(LEN=CHOIX_LEN) :: CHOIX
!
!-----------------------------------------------------------------------
        POS_WORD = 1
        I = 1
        CHOIX = MYDICO(IKEY)%CHOIX(LNG)
        MYLEN = LEN(TRIM(CHOIX))
! Here we have two cases:
! 1 - The list is of type '1=str';'2=str';...
!     In which case we must identify the pairs
!     And create the array of values in mydico
! 2 - It is a basic string list '"str"';'"str"';.??
        POS_EGAL = INDEX(CHOIX(POS_WORD:MYLEN),"=")
        IF (POS_EGAL.EQ.0) THEN
! CASE 2
          CATA_INTO = REPEAT(' ',CHOIX_LEN)
          IDX = 1
          DO WHILE(IDX.NE.0)
            IDX = INDEX(CHOIX(POS_WORD:MYLEN),";")
! CHECK IF IT IS THE LAST ONE
            IF(IDX.EQ.0) THEN
              CATA_INTO = TRIM(CATA_INTO) // "'" //
     &                    CHOIX(POS_WORD:MYLEN)// "'"
            ELSE
              CATA_INTO = TRIM(CATA_INTO) // "'" //
     &                    CHOIX(POS_WORD:POS_WORD+IDX-2)//"',"
            ENDIF
            POS_WORD = POS_WORD +  IDX
          ENDDO
        ELSE
! CASE 1
          CATA_INTO = REPEAT(' ',CHOIX_LEN)
          DO WHILE(POS_EGAL.NE.0)
            IF(I.GT.MAXENUM) THEN
              WRITE(*,*) 'Increase maxENUM'
              CALL PLANTE(1)
              STOP
            ENDIF
            IDX = INDEX(CHOIX(POS_EGAL:MYLEN),";")
            MYDICO(IKEY)%HASH_ID(I,LNG) = CHOIX(POS_WORD:(POS_EGAL-1))
            POS_WORD = POS_EGAL +  IDX - 1
            POS_EGAL = POS_EGAL + 1
! CHECK IF IT IS THE LAST ONE
            IF(IDX.EQ.0) THEN
              MYDICO(IKEY)%HASH_VAL(I,LNG) = CHOIX(POS_EGAL:MYLEN)
              CATA_INTO = TRIM(CATA_INTO) // CHOIX(POS_EGAL:MYLEN)
              POS_EGAL = 0
            ELSE
              MYDICO(IKEY)%HASH_VAL(I,LNG) = CHOIX(POS_EGAL:POS_WORD-1)
              CATA_INTO = TRIM(CATA_INTO) //
     &                    CHOIX(POS_EGAL:POS_WORD-1)//','
              POS_WORD = POS_WORD + 1
              POS_EGAL = POS_WORD + INDEX(CHOIX(POS_WORD:MYLEN),'=') -1
            ENDIF
            I = I + 1
          ENDDO
        ENDIF
!-----------------------------------------------------------------------
!
      END FUNCTION
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! Return a string that replace "'" and " " by "_" from the given string
!
!     param string The string to modify
!
      CHARACTER(LEN=KEYWORD_LEN) FUNCTION CATA_NAME(STRING)
!
!-----------------------------------------------------------------------
        IMPLICIT NONE
!
        CHARACTER(LEN=KEYWORD_LEN),INTENT(IN) :: STRING
!
        INTEGER MYLEN,I
!
!-----------------------------------------------------------------------
        MYLEN = LEN(TRIM(STRING))
        CATA_NAME = REPEAT(' ',KEYWORD_LEN)
        DO I=1,MYLEN
          IF(STRING(I:I).EQ."'".OR.
     &       STRING(I:I).EQ."-".OR.
     &       STRING(I:I).EQ.",".OR.
     &       STRING(I:I).EQ."(".OR.
     &       STRING(I:I).EQ.")".OR.
     &       STRING(I:I).EQ." ") THEN
            CATA_NAME(I:I) = "_"
          ELSE
            CATA_NAME(I:I) = STRING(I:I)
          ENDIF
        ENDDO
        ! IF FIRST LETTER IS A NUMBER CHANGING IT
        IF(CATA_NAME(1:1).EQ."0") THEN
          CATA_NAME(1:1) = "A"
        ELSE IF(CATA_NAME(1:1).EQ."1") THEN
          CATA_NAME(1:1) = "Z"
        ELSE IF(CATA_NAME(1:1).EQ."2") THEN
          CATA_NAME(1:1) = "E"
        ELSE IF(CATA_NAME(1:1).EQ."3") THEN
          CATA_NAME(1:1) = "R"
        ELSE IF(CATA_NAME(1:1).EQ."4") THEN
          CATA_NAME(1:1) = "T"
        ELSE IF(CATA_NAME(1:1).EQ."5") THEN
          CATA_NAME(1:1) = "Y"
        ELSE IF(CATA_NAME(1:1).EQ."6") THEN
          CATA_NAME(1:1) = "U"
        ELSE IF(CATA_NAME(1:1).EQ."7") THEN
          CATA_NAME(1:1) = "1"
        ELSE IF(CATA_NAME(1:1).EQ."8") THEN
          CATA_NAME(1:1) = "O"
        ELSE IF(CATA_NAME(1:1).EQ."9") THEN
          CATA_NAME(1:1) = "P"
        ENDIF
        RETURN
!-----------------------------------------------------------------------
!
      END FUNCTION
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! brief Write in Python the enum for the keyword with CHOIX in
! form id:"str"
!
      SUBROUTINE WRITE_ENUM(FILENAME)
!
!-----------------------------------------------------------------------
        IMPLICIT NONE
!
        CHARACTER(LEN=144),INTENT(IN) :: FILENAME
!
        INTEGER :: NFIC
        INTEGER :: LNG,IERR
        INTEGER :: IKEY,I
        LOGICAL ISSTR
        INTEGER :: MYLEN(2),POS_WORD(2),IDX(2)
        CHARACTER(LEN=CHOIX_LEN) :: CHOIX(2)
!
!-----------------------------------------------------------------------
        NFIC = 666
        OPEN(NFIC,FILE=TRIM(FILENAME),IOSTAT=IERR)
        CALL CHECK_CALL(IERR,'CATA_DICTIONARY')
!
! English
!
        WRITE(NFIC,'(A)') '#/usr/bin/env python'
        WRITE(NFIC,'(A)') '# -*- coding: latin-1 -*-'
        WRITE(NFIC,'(A)') 'TelemacdicoEn = {'
        DO IKEY = 1,NKEY
! CHECK IF THE KEYWORD HAS AN ENUM
          IF(MYDICO(IKEY)%HASH_ID(1,EN)(1:1).NE.' ') THEN
            WRITE(NFIC,'(A)') "'"//
     &         TRIM(CATA_NAME(MYDICO(IKEY)%KNOM(EN)))//"' : {"
            I = 1
            SELECT CASE (MYDICO(IKEY)%HASH_ID(I,EN)(1:1))
              CASE ("-","0","1","2","3","4","5","6","7","8","9")
                ISSTR = .FALSE.
              CASE DEFAULT
                ISSTR = .TRUE.
            END SELECT
            DO WHILE(MYDICO(IKEY)%HASH_ID(I,EN)(1:1).NE.' ')
              IF(ISSTR) THEN
                WRITE(NFIC,'(4X,A)') "'"//
     &                 TRIM(MYDICO(IKEY)%HASH_ID(I,EN))//
     &                 "':" // TRIM(MYDICO(IKEY)%HASH_VAL(I,EN)) // ","
              ELSE
                WRITE(NFIC,'(4X,A)')
     &                 TRIM(MYDICO(IKEY)%HASH_ID(I,EN))//
     &                 ":" // TRIM(MYDICO(IKEY)%HASH_VAL(I,EN)) // ","
              ENDIF
              I = I + 1
            ENDDO
            WRITE(NFIC,'(2X,A)') "},"
          ENDIF
        ENDDO
        WRITE(NFIC,'(A)') '}'
!
! French
!
        WRITE(NFIC,'(A)') 'TelemacdicoFr = {'
        DO IKEY = 1,NKEY
! CHECK IF THE KEYWORD HAS AN ENUM
          IF(MYDICO(IKEY)%HASH_ID(1,FR)(1:1).NE.' ') THEN
            WRITE(NFIC,'(A)') "'"//
     &         TRIM(CATA_NAME(MYDICO(IKEY)%KNOM(EN)))//"' : {"
            I = 1
            SELECT CASE (MYDICO(IKEY)%HASH_ID(I,FR)(1:1))
              CASE ("-","0","1","2","3","4","5","6","7","8","9")
                ISSTR = .FALSE.
              CASE DEFAULT
                ISSTR = .TRUE.
            END SELECT
            DO WHILE(MYDICO(IKEY)%HASH_ID(I,FR)(1:1).NE.' ')
              IF(ISSTR) THEN
                WRITE(NFIC,'(4X,A)') "'"//
     &                 TRIM(MYDICO(IKEY)%HASH_ID(I,FR))//
     &                 "':" // TRIM(MYDICO(IKEY)%HASH_VAL(I,FR)) // ","
              ELSE
                WRITE(NFIC,'(4X,A)')
     &                 TRIM(MYDICO(IKEY)%HASH_ID(I,FR))//
     &                 ":" // TRIM(MYDICO(IKEY)%HASH_VAL(I,FR)) // ","
              ENDIF
              I = I + 1
            ENDDO
            WRITE(NFIC,'(2X,A)') "},"
          ENDIF
        ENDDO
        WRITE(NFIC,'(A)') '}'
        WRITE(NFIC,'(A)') ''
! Writing Python dictionary for correspondance name-fr -> name_cata
        WRITE(NFIC,'(A)') 'DicoCasFrToCata = {'
        DO IKEY = 1,NKEY
          WRITE(NFIC,'(2X,5A)') '"', TRIM(MYDICO(IKEY)%KNOM(FR)),'":"',
     &                      TRIM(CATA_NAME(MYDICO(IKEY)%KNOM(EN))),'",'
        ENDDO
        WRITE(NFIC,'(A)') '}'
        WRITE(NFIC,'(A)') ''

! Writing Python dictionary for correspondance name-en -> name_cata
        WRITE(NFIC,'(A)') 'DicoCasEnToCata = {'
        DO IKEY = 1,NKEY
          WRITE(NFIC,'(2X,5A)') "'", TRIM(MYDICO(IKEY)%KNOM(EN)),"':'",
     &                      TRIM(CATA_NAME(MYDICO(IKEY)%KNOM(EN))),"',"
        ENDDO
        WRITE(NFIC,'(A)') '}'

        WRITE(NFIC, '(A)') 'DicoEnumCasFrToEnumCasEn = {'
        DO IKEY = 1,NKEY
          ! If we have a keyword with a choix made of string and = in the
          ! choix
          IF (MYDICO(IKEY)%KTYPE.EQ.4 .AND.
     &        MYDICO(IKEY)%CHOIX(FR)(1:1).NE.' '.AND.
     &        INDEX(MYDICO(IKEY)%CHOIX(FR),"=").EQ.0) THEN
            WRITE(NFIC, '(A)')
     &           "'"//TRIM(CATA_NAME(MYDICO(IKEY)%KNOM(EN)))//"':{"
            ! Identiying each values in the CHOIX string
            IDX(:) = 1
            POS_WORD(:) = 1
            CHOIX(FR) = MYDICO(IKEY)%CHOIX(FR)
            CHOIX(EN) = MYDICO(IKEY)%CHOIX(EN)
            MYLEN(FR) = LEN(TRIM(CHOIX(FR)))
            MYLEN(EN) = LEN(TRIM(CHOIX(EN)))
            DO WHILE(IDX(FR).NE.0)
              DO I=1,2
                IDX(I) = INDEX(CHOIX(I)(POS_WORD(I):MYLEN(I)),";")
              ENDDO
              IF(IDX(FR).EQ.0) THEN
                WRITE(NFIC, '(A)') '  "'//
     &             CHOIX(FR)(POS_WORD(FR):MYLEN(FR))//'":"'//
     &             CHOIX(EN)(POS_WORD(EN):MYLEN(EN))//'",'
              ELSE
                WRITE(NFIC, '(A)') '  "'//
     &    CHOIX(FR)(POS_WORD(FR):POS_WORD(FR)+IDX(FR)-2)//'":"'//
     &    CHOIX(EN)(POS_WORD(EN):POS_WORD(EN)+IDX(EN)-2)//'",'
              ENDIF
              DO I=1,2
                POS_WORD(I) = POS_WORD(I) + IDX(I)
              ENDDO
            ENDDO
            WRITE(NFIC, '(A)') "},"
            WRITE(NFIC, '(A)') ''
          ENDIF
        ENDDO
        WRITE(NFIC, '(A)') "}"
!

        CLOSE(NFIC)
!-----------------------------------------------------------------------
!
      END SUBROUTINE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! brief Write the translation files
!
      SUBROUTINE WRITE_TS(PATH)
!
!-----------------------------------------------------------------------
        IMPLICIT NONE
!
        CHARACTER(LEN=144),INTENT(IN) :: PATH
        INTEGER NFIC, IERR, IKEY, I
        CHARACTER(LEN=144) :: FILENAME
!
!-----------------------------------------------------------------------
        NFIC = 666
        FILENAME = REPEAT(' ',144)
        FILENAME = TRIM(PATH)//'cata_name2eng_name.ts'

        OPEN(NFIC,FILE=TRIM(FILENAME),IOSTAT=IERR)
        CALL CHECK_CALL(IERR,'CATA_DICTIONARY')

        WRITE(NFIC,'(A)') '<?xml version="1.0" encoding="utf-8"?>'
        WRITE(NFIC,'(A)')
     &                  '<!DOCTYPE TS><TS version="1.1" language="en">'
        WRITE(NFIC,'(A)') '<context>'
        WRITE(NFIC,'(4X,A)') '<name>@defaut</name>'
! Loop on keywords
        DO IKEY=1,NKEY
          WRITE(NFIC,'(4X,A)') '<message>'
          WRITE(NFIC,'(8X,A)') '<source>'//
     &               TRIM(CATA_NAME(MYDICO(IKEY)%KNOM(EN)))//
     &               '</source>'
          WRITE(NFIC,'(8X,A)') '<translation>'//
     &            TRIM(MYDICO(IKEY)%KNOM(EN))//'</translation>'
          WRITE(NFIC,'(4X,A)') '</message>'
!          IF(MYDICO(IKEY)%HASH_ID(1,EN)(1:1).NE.' ') THEN
!            I = 1
!            DO WHILE(MYDICO(IKEY)%HASH_ID(I,EN)(1:1).NE.' '
!     &               .AND.I.LE.MAXENUM)
!              WRITE(NFIC,'(4X,A)') '<message>'
!              WRITE(NFIC,'(8X,A)') '<source>'//
!     &               TRIM(CATA_NAME(MYDICO(IKEY)%HASH_VAL(I,EN)))//
!     &               '</source>'
!              WRITE(NFIC,'(8X,A)') '<translation>'//
!     &           TRIM(MYDICO(IKEY)%HASH_VAL(I,EN))//
!     &                             '</translation>'
!              WRITE(NFIC,'(4X,A)') '</message>'
!              I = I + 1
!            ENDDO
!          ENDIF
        ENDDO
!
        WRITE(NFIC,'(A)') '</context>'
        WRITE(NFIC,'(A)') '</TS>'
        CLOSE(NFIC)
!
! French
!
        FILENAME = REPEAT(' ',144)
        FILENAME = TRIM(PATH)//'cata_name2fra_name.ts'

        OPEN(NFIC,FILE=TRIM(FILENAME),IOSTAT=IERR)
        CALL CHECK_CALL(IERR,'CATA_DICTIONARY')

        WRITE(NFIC,'(A)') '<?xml version="1.0" encoding="utf-8"?>'
        WRITE(NFIC,'(A)')
     &                  '<!DOCTYPE TS><TS version="1.1" language="en">'
        WRITE(NFIC,'(A)') '<context>'
        WRITE(NFIC,'(4X,A)') '<name>@defaut</name>'
! Loop on keywords
        DO IKEY=1,NKEY
          WRITE(NFIC,'(4X,A)') '<message>'
          WRITE(NFIC,'(8X,A)') '<source>'//
     &               TRIM(CATA_NAME(MYDICO(IKEY)%KNOM(EN)))//
     &               '</source>'
          WRITE(NFIC,'(8X,A)') '<translation>'//
     &            TRIM(MYDICO(IKEY)%KNOM(FR))//'</translation>'
          WRITE(NFIC,'(4X,A)') '</message>'
!          IF(MYDICO(IKEY)%HASH_ID(1,FR)(1:1).NE.' ') THEN
!            I = 1
!            DO WHILE(MYDICO(IKEY)%HASH_ID(I,FR)(1:1).NE.' '
!     &               .AND.I.LE.MAXENUM)
!              WRITE(NFIC,'(4X,A)') '<message>'
!              WRITE(NFIC,'(8X,A)') '<source>'//
!     &               TRIM(CATA_NAME(MYDICO(IKEY)%HASH_VAL(I,EN))) //
!     &               '</source>'
!              WRITE(NFIC,'(8X,A)') '<translation>'//
!     &           TRIM(MYDICO(IKEY)%HASH_VAL(I,FR))//
!     &                             '</translation>'
!              WRITE(NFIC,'(4X,A)') '</message>'
!              I = I + 1
!            ENDDO
!          ENDIF
        ENDDO
!
        WRITE(NFIC,'(A)') '</context>'
        WRITE(NFIC,'(A)') '</TS>'
        CLOSE(NFIC)
!-----------------------------------------------------------------------
!
      END SUBROUTINE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! brief Write a rubrique in a cata
!
!     param nfic File descriptor
!     param irub Idx of the rubrique
!     param level Indentation level
!
      SUBROUTINE WRITE_BEGIN_RUBRIQUE(NFIC,IRUB,LEVEL)
!
!-----------------------------------------------------------------------
        IMPLICIT NONE
!
        INTEGER, INTENT(IN) :: IRUB
        INTEGER, INTENT(IN) :: NFIC
        INTEGER, INTENT(IN) :: LEVEL
!
        CHARACTER(LEN=STRING_LEN) :: RUB
        LOGICAL :: MANDATORY_RUB
        CHARACTER(LEN=1) :: RUB_STATUS
!
!-----------------------------------------------------------------------
        RUB = CATA_NAME(RUBRIQUE(EN,IRUB,LEVEL))
! Defining if rubrique is mandatory or optional
        MANDATORY_RUB = (RUBRIQUE_INFO(IRUB,LEVEL) == 'o')
!
        IF(LEVEL.EQ.1) THEN
          WRITE(NFIC,'(A)') '# '//REPEAT(' ',4*(LEVEL-1))//
     &                      REPEAT('-',71)
          WRITE(NFIC,'(A)') REPEAT(' ',4*(LEVEL-1))//
     &              TRIM(RUB)//' = PROC(nom= "'//
     &              TRIM(RUB)//
     &              '",op = None,'
          WRITE(NFIC,'(A)') '# '//REPEAT(' ',4*(LEVEL-1))//
     &                      REPEAT('-',71)
          IF(MANDATORY_RUB) THEN
            WRITE(NFIC,'(A)') '    UIinfo = {"groupes": ("CACHE")},'
          ENDIF
        ELSE
          IF(MANDATORY_RUB) THEN
            RUB_STATUS = 'o'
          ELSE
            RUB_STATUS = 'f'
          ENDIF
          WRITE(NFIC,'(A)') '# '//REPEAT(' ',4*(LEVEL-1)-2)//
     &                      REPEAT('-',35)
          WRITE(NFIC,'(A)') REPEAT(' ',4*(LEVEL-1))//
     &              TRIM(RUB)//" = FACT(statut='"//RUB_STATUS//"',"
          WRITE(NFIC,'(a)') '# '//REPEAT(' ',4*(level-1)-2)//
     &                      REPEAT('-',35)
        ENDIF
!-----------------------------------------------------------------------
!
      END SUBROUTINE WRITE_BEGIN_RUBRIQUE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! brief Write a rubrique in a cata
!
!     param nfic File descriptor
!     param irub Index of the rubrique
!     param level Indentation level
!
      SUBROUTINE WRITE_END_RUBRIQUE(NFIC,IRUB,LEVEL)
!
!-----------------------------------------------------------------------
        IMPLICIT NONE
!
        INTEGER, INTENT(IN) :: IRUB
        INTEGER, INTENT(IN) :: NFIC
        INTEGER, INTENT(IN) :: LEVEL
!
!-----------------------------------------------------------------------
        IF(LEVEL.EQ.1) THEN
          WRITE(NFIC,'(A)') REPEAT(' ',4*(LEVEL-1))//')'
        ELSE
          WRITE(NFIC,'(A)') REPEAT(' ',4*(LEVEL-1))//'),'
        ENDIF
!-----------------------------------------------------------------------
!
      END SUBROUTINE WRITE_END_RUBRIQUE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! brief Write a conditional bloc in a cata
!
!     param nfic File descriptor
!     param ikey Index of the keyword associated with the bloc
!     param icond Index of the condition
!     param level Indentation level
!     param lng Language of output
!
      SUBROUTINE WRITE_BEGIN_BLOC(NFIC,IKEY,ICOND,LEVEL,LNG)
!
        IMPLICIT NONE
!
        INTEGER, INTENT(IN) :: NFIC
        INTEGER, INTENT(IN) :: IKEY
        INTEGER, INTENT(IN) :: ICOND
        INTEGER, INTENT(IN) :: LEVEL
        INTEGER, INTENT(IN) :: LNG
!
        CHARACTER(LEN=KEYWORD_LEN) MYCATA_NAME
!
!-----------------------------------------------------------------------
        MYCATA_NAME = CATA_NAME(MYDICO(IKEY)%KNOM(LNG))
        !
        WRITE(NFIC,'(A)') '# '//REPEAT(' ',4*(LEVEL)-2)//
     &                    REPEAT('-',35)
! using max(icond,1) cause when it is an internal condition icond
! equal 0
        WRITE(NFIC,'(a)') REPEAT(' ',4*(level))//
     &           'b_'// TRIM(MYCATA_NAME)//CHAR(ICOND+70)//
     &           ' = BLOC(condition="'//
     &           TRIM(MYDICO(IKEY)%COND(MAX(ICOND,1)))//'",'
        WRITE(NFIC,'(a)') '# '//REPEAT(' ',4*(LEVEL)-2)//
     &                    REPEAT('-',35)
!-----------------------------------------------------------------------
!
      END SUBROUTINE WRITE_BEGIN_BLOC
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! brief Write a conditional bloc in a cata
!
!     param nfic File descriptor
!     param ikey Index of the keyword associated with the bloc
!     param icond Index of the condition
!     param level Indentation level
!     param lng Language of output
!
      SUBROUTINE WRITE_END_BLOC(NFIC,IKEY,ICOND,LEVEL,LNG)
!
!-----------------------------------------------------------------------
        IMPLICIT NONE
!
        INTEGER, INTENT(IN) :: NFIC
        INTEGER, INTENT(IN) :: IKEY
        INTEGER, INTENT(IN) :: ICOND
        INTEGER, INTENT(IN) :: LEVEL
        INTEGER, INTENT(IN) :: LNG
!
!-----------------------------------------------------------------------
        WRITE(NFIC,'(A)') REPEAT(' ',4*(LEVEL))//'),'
!-----------------------------------------------------------------------
!
      END SUBROUTINE WRITE_END_BLOC
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! brief Write a consigne in a cata
!
! param nfic File descriptor
! param ikey Index of the keyword associated with the consigne
! param icond Index of the condition
! param level Indentation level
! param lng Language of output
!
      SUBROUTINE WRITE_CONSIGNE(NFIC,IKEY,ICOND,LEVEL,LNG)
!
!-----------------------------------------------------------------------
        IMPLICIT NONE
!
        INTEGER, INTENT(IN) :: NFIC
        INTEGER, INTENT(IN) :: IKEY
        INTEGER, INTENT(IN) :: ICOND
        INTEGER, INTENT(IN) :: LEVEL
        INTEGER, INTENT(IN) :: LNG
!
        CHARACTER(LEN=KEYWORD_LEN) MYCATA_NAME
!
        MYCATA_NAME = CATA_NAME(MYDICO(IKEY)%KNOM(LNG))
!
        WRITE(NFIC,'(A)') '# '//REPEAT(' ',4*(LEVEL)-2)//
     &                    REPEAT('-',35)
        WRITE(NFIC,'(A)') REPEAT(' ',4*LEVEL)//
     &     'Consigne = SIMP(statut ="o", homo="information", typ="TXM",'
        WRITE(NFIC,'(A)') '# '//REPEAT(' ',4*(LEVEL)-2)//
     &                    REPEAT('-',35)
        WRITE(NFIC,'(A)') REPEAT(' ',4*(LEVEL+1))//'defaut = "'//
     &                    TRIM(MYDICO(IKEY)%CONSIGNE(ICOND,LNG))//'"),'
!-----------------------------------------------------------------------
!
      END SUBROUTINE WRITE_CONSIGNE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! Write a keyword in a cata
!
!     param nfic File descriptor
!     param ikey Key word id
!     param level Indentation level
!     param lng Language of ouput
!
      RECURSIVE SUBROUTINE WRITE_KEYWORD2CATA(NFIC,IKEY,LEVEL,LNG)
!
!-----------------------------------------------------------------------
        IMPLICIT NONE
!
        INTEGER, INTENT(IN) :: IKEY
        INTEGER, INTENT(IN) :: NFIC
        INTEGER, INTENT(IN) :: LEVEL
        INTEGER, INTENT(IN) :: LNG
!
        CHARACTER(LEN=KEYWORD_LEN) :: CAT_NAME
        CHARACTER(LEN=3) CATA_STAT
        CHARACTER(LEN=72) CATA_FILE
        CHARACTER(LEN=CHOIX_LEN) :: MYCATA_INTO
        INTEGER :: MYLEN,IERR,I
        INTEGER :: ICOND,IDEP
!
!-----------------------------------------------------------------------
        IF(KEY_WRITTEN(IKEY)) THEN
          RETURN
        ELSE
          KEY_WRITTEN(IKEY) = .TRUE.
        ENDIF
        CAT_NAME = CATA_NAME(MYDICO(IKEY)%KNOM(LNG))
!
! Check if the keyword it selfs has a condition
        IF(MYDICO(IKEY)%COND(1)(1:1).NE.' '.AND.
     &     MYDICO(IKEY)%DEPEN(1,1).EQ.0) THEN
! WRITING THE KEYWORD INSIDE A CONDITIONAL BLOC
          CALL WRITE_BEGIN_BLOC(NFIC,IKEY,0,LEVEL,LNG)
          CALL WRITE_KEYWORD2CATA(NFIC,IKEY,LEVEL+1,LNG)
          CALL WRITE_END_BLOC(NFIC,IKEY,0,LEVEL,LNG)
        ENDIF
        !
        IF(MYDICO(IKEY)%NIVEAU.EQ.0) THEN
          CATA_STAT="'o'"
        ELSE
          CATA_STAT="'f'"
        ENDIF
!
! Building list of values
        IF(MYDICO(IKEY)%CHOIX(LNG)(1:1).NE.' ') THEN
          MYCATA_INTO = CATA_INTO(IKEY,3-LNG)
          MYCATA_INTO = CATA_INTO(IKEY,LNG)
        ENDIF

! Name and status (mandatory or optional)
        WRITE(NFIC,'(a)') '# '//REPEAT(' ',4*(level)-2)//
     &                    REPEAT('-',35)
        WRITE(NFIC,'(A)') REPEAT(' ',4*LEVEL)//TRIM(CAT_NAME)//
     &                ' = SIMP(statut ='//
     &             CATA_STAT//","
        WRITE(NFIC,'(a)') '# '//REPEAT(' ',4*(level)-2)//
     &                    REPEAT('-',35)
! Type of the entry
        WRITE(NFIC,'(A)') REPEAT(' ',4*(LEVEL+1))//'typ = '//
     &                    TRIM(CATA_TYP(IKEY,LNG))//","
! List of values if there is one
        IF(MYDICO(IKEY)%CHOIX(LNG)(1:1).NE.' ') THEN
          WRITE(NFIC,'(A)') REPEAT(' ',4*(LEVEL+1))//'into = ['//
     &                      TRIM(MYCATA_INTO)//"],"
        ENDIF
! Default value
        IF(MYDICO(IKEY)%DEFAUT(1).NE.'OBLIGATOIRE') THEN
          WRITE(NFIC,'(A)') REPEAT(' ',4*(LEVEL+1))//"defaut = "//
     &                      TRIM(CATA_DEFAUT(IKEY,LNG))//","
        ENDIF
! Help in french
        WRITE(NFIC,'(A)') REPEAT(' ',4*(LEVEL+1))//'fr = """'//
     &             TRIM(MYDICO(IKEY)%AIDE(FR))//'""",'
! Help in English
        WRITE(NFIC,'(A)') REPEAT(' ',4*(LEVEL+1))//'ang = """'//
     &             TRIM(MYDICO(IKEY)%AIDE(EN))//'""",'
        WRITE(NFIC,'(A)') REPEAT(' ',4*(LEVEL))//"),"

! Check if the keyword has dependencies
        IF(MYDICO(IKEY)%COND(1)(1:1).NE.' '.AND.
     &     MYDICO(IKEY)%DEPEN(1,1).NE.0) THEN
          DO ICOND=1,MAXCOND
            IF (MYDICO(IKEY)%COND(ICOND)(1:1).EQ.' ') EXIT
            CALL WRITE_BEGIN_BLOC(NFIC,IKEY,ICOND,LEVEL,LNG)
            DO IDEP=1,MAXDEP
! We have a consigne
              IF (MYDICO(IKEY)%DEPEN(ICOND,IDEP).LT.0) THEN
                CALL WRITE_CONSIGNE(NFIC,IKEY,
     &                       ICOND,LEVEL+1,LNG)
              ELSE IF (MYDICO(IKEY)%DEPEN(ICOND,IDEP).EQ.0) THEN
                EXIT
              ELSE
                CALL WRITE_KEYWORD2CATA(NFIC,
     &                                MYDICO(IKEY)%DEPEN(ICOND,IDEP),
     &                                LEVEL+1,LNG)
              ENDIF
            ENDDO
            CALL WRITE_END_BLOC(NFIC,IKEY,ICOND,LEVEL,LNG)
          ENDDO
        ENDIF
!
!-----------------------------------------------------------------------
      END SUBROUTINE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! Write an Eficas Catalog from the dictionary
!
!     param filename Name of the Catalog file
!     param code_name Name of Code for which the catalogue is written
!
      SUBROUTINE WRITE2CATA(FILENAME, CODE_NAME)
!
!-----------------------------------------------------------------------
        IMPLICIT NONE
!
        CHARACTER(LEN=144), INTENT(IN) :: FILENAME
      CHARACTER(LEN=10), INTENT(IN) :: CODE_NAME
!
        INTEGER :: NFIC,IERR
        INTEGER :: IRUB1
        INTEGER :: IRUB2
        INTEGER :: IRUB3
        INTEGER :: IKEY
        INTEGER LEVEL,LNG
!
!-----------------------------------------------------------------------
        NFIC = 666
        LNG = EN
        ALLOCATE(KEY_WRITTEN(NKEY),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'KEY_WRITTEN')
        KEY_WRITTEN(:) = .FALSE.
        WRITE(*,*) '---- EFICAS CATALOG PROCESS ----'
        WRITE(*,*) 'WRITING IN : ',TRIM(FILENAME)
        OPEN(NFIC,FILE=TRIM(FILENAME),IOSTAT=IERR)
        CALL CHECK_CALL(IERR,'CATA_DICTIONARY')
        WRITE(NFIC,'(a)') ''
        WRITE(NFIC,'(a)') '# coding: utf-8'
        WRITE(NFIC,'(a)') ''
        WRITE(NFIC,'(a)') 'from Accas import *'
        WRITE(NFIC,'(a)') 'class DateJJMMAAAA:'
        WRITE(NFIC,'(a)') '  def __init__(self):'
        WRITE(NFIC,'(a)') '    self.ntuple=3'
        WRITE(NFIC,'(a)') ''
        WRITE(NFIC,'(a)') '  def __convert__(self,valeur):'
        WRITE(NFIC,'(a)') '    if type(valeur) == types.StringType: '//
     &                'return None'
        WRITE(NFIC,'(a)') '    if len(valeur) != self.ntuple: '//
     &                'return None'
        WRITE(NFIC,'(a)') '    return valeur'
        WRITE(NFIC,'(a)') ''
        WRITE(NFIC,'(a)') '  def info(self):'
        WRITE(NFIC,'(a)') '    return "Date : jj/mm/aaaa "'
        WRITE(NFIC,'(a)') ''
        WRITE(NFIC,'(a)') '  __repr__=info'
        WRITE(NFIC,'(a)') '  __str__=info'
        WRITE(NFIC,'(a)') ''
        WRITE(NFIC,'(a)') 'class grma(GEOM):'
        WRITE(NFIC,'(a)') '  pass'
        WRITE(NFIC,'(a)') ''
        WRITE(NFIC,'(a)') 'import types'
        WRITE(NFIC,'(a)') 'class Tuple:'
        WRITE(NFIC,'(a)') '  def __init__(self,ntuple):'
        WRITE(NFIC,'(a)') '    self.ntuple=ntuple'
        WRITE(NFIC,'(a)') ''
        WRITE(NFIC,'(a)') '  def __convert__(self,valeur):'
        WRITE(NFIC,'(a)') '    if type(valeur) == types.StringType:'
        WRITE(NFIC,'(a)') '      return None'
        WRITE(NFIC,'(a)') '    if len(valeur) != self.ntuple:'
        WRITE(NFIC,'(a)') '      return None'
        WRITE(NFIC,'(a)') '    return valeur'
        WRITE(NFIC,'(a)') ''
        WRITE(NFIC,'(a)') '  def info(self):'
        WRITE(NFIC,'(a)') '    return "Tuple de %s elements" % '//
     &                'self.ntuple'
        WRITE(NFIC,'(a)') ''
        WRITE(NFIC,'(a)') ''
        WRITE(NFIC,'(a)') ''
        WRITE(NFIC,'(a)') "JdC = JDC_CATA (code = '"//
     &                   TRIM(CODE_NAME)//"',"
        WRITE(NFIC,'(a)') '                execmodul = None,'
        WRITE(NFIC,'(a)') '                )'
        WRITE(NFIC,'(a)') '# '//REPEAT("=",71)
        WRITE(NFIC,'(a)') '# Catalog entry for the MAP function : '//
     &                'c_pre_interfaceBody_mesh'
        WRITE(NFIC,'(a)') '# '//REPEAT("=",71)
        WRITE(NFIC,'(a)') ''

        DO IKEY=1,NKEY
          IF(MYDICO(IKEY)%KNOM(EN)(1:8).EQ.'RELEASE') THEN
            WRITE(NFIC,'(a)') 'VERSION_CATALOGUE="'//
     &                     TRIM(MYDICO(IKEY)%DEFAUT(EN))//'"'
          ENDIF
        ENDDO
! Loop on rubriques
        DO IRUB1=1,NRUB(LNG,1)
          CALL WRITE_BEGIN_RUBRIQUE(NFIC,IRUB1,1)
          DO IKEY=1,NKEY
            ! IDENTIFYING KEYWWORDS THAT ARE 1 1
            IF(HAS_RUBRIQUE(IKEY,IRUB1,1,LNG).AND.
     &         (MYDICO(IKEY)%RUBRIQUE(LNG,2)(1:1).EQ.' ')) THEN
              CALL WRITE_KEYWORD2CATA(NFIC,IKEY,1,LNG)
            ENDIF
          ENDDO
! LEVEL 2
! LOOP ON RUBRIQUES
          DO IRUB2=1,NRUB(LNG,2)
            LEVEL = 2
            IF(RUB1_DEP(IRUB1,IRUB2)) THEN
              CALL WRITE_BEGIN_RUBRIQUE(NFIC,IRUB2,2)
              DO IKEY=1,NKEY
! IDENTIFYING KEYWWORDS THAT ARE 2 1
                IF(HAS_RUBRIQUE(IKEY,IRUB1,1,LNG).AND.
     &             HAS_RUBRIQUE(IKEY,IRUB2,2,LNG).AND.
     &             (MYDICO(IKEY)%RUBRIQUE(LNG,3)(1:1).EQ.' ')) THEN
                  CALL WRITE_KEYWORD2CATA(NFIC,IKEY,2,LNG)
                ENDIF
              ENDDO
! LEVEL 3
! LOOP ON RUBRIQUES
              DO IRUB3=1,NRUB(LNG,3)
                LEVEL = 3
                IF(RUB2_DEP(IRUB1,IRUB2,IRUB3)) THEN
                  CALL WRITE_BEGIN_RUBRIQUE(NFIC,IRUB3,3)
                  DO IKEY=1,NKEY
! IDENTIFYING KEYWWORDS THAT ARE 3 1
                    IF(HAS_RUBRIQUE(IKEY,IRUB1,1,LNG).AND.
     &                 HAS_RUBRIQUE(IKEY,IRUB2,2,LNG).AND.
     &                 HAS_RUBRIQUE(IKEY,IRUB3,3,LNG)) THEN
                      CALL WRITE_KEYWORD2CATA(NFIC,IKEY,3,LNG)
                    ENDIF
                  ENDDO
                  CALL WRITE_END_RUBRIQUE(NFIC,IRUB3,3)
                ENDIF
              ENDDO ! LEVEL 3
              CALL WRITE_END_RUBRIQUE(NFIC,IRUB2,2)
            ENDIF
          ENDDO ! LEVEL 2
          CALL WRITE_END_RUBRIQUE(NFIC,IRUB1,1)
        ENDDO ! LEVEL 1

        WRITE(NFIC,'(A)') 'Ordre_Des_Commandes = ('
        DO IRUB1 =1,NRUB(LNG,1)-1
          WRITE(NFIC,'(A)')
     &       "'"//TRIM(CATA_NAME(RUBRIQUE(LNG,IRUB1,1)))//"',"
        ENDDO
        WRITE(NFIC,'(A)')
     &       "'"//TRIM(CATA_NAME(RUBRIQUE(LNG,NRUB(LNG,1),1)))//"')"
        WRITE(NFIC,'(A)') 'Classement_Commandes_Ds_Arbre = ('
        DO IRUB1 =1,NRUB(LNG,1)-1
          WRITE(NFIC,'(A)')
     &       "'"//TRIM(CATA_NAME(RUBRIQUE(LNG,IRUB1,1)))//"',"
        ENDDO
        WRITE(NFIC,'(A)')
     &       "'"//TRIM(CATA_NAME(RUBRIQUE(LNG,NRUB(LNG,1),1)))//"')"
        CLOSE(NFIC)
        DEALLOCATE(KEY_WRITTEN)
!
      END SUBROUTINE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      END MODULE UTILS_CATA
