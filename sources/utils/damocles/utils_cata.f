      !brief Module containing the function to write a List of keyword
      !+     into a Eficas Catalog
      MODULE UTILS_CATA
      USE DICO_DATA
      IMPLICIT NONE
      ! brief Array containin true if a key word was written in the
      !+ catalog
      LOGICAL, ALLOCATABLE :: KEY_WRITTEN(:)
      CONTAINS
      ! brief Build the Eficas catalog into string from DEFAUT in
      ! dictionary
      !
      ! param ikey id of the keyword to handle
      ! param lng The language to handle
      character(len=KEYWORD_LEN) function cata_typ(ikey, lng)
      !
      implicit none
      !
      integer, intent(in) :: ikey
      integer, intent(in) :: lng
      !
      integer i
      !
      cata_typ=repeat(' ',KEYWORD_LEN)
      if(mydico(ikey)%ktype.eq.1) then
        cata_typ = "'I'"
      else if(mydico(ikey)%ktype.eq.2) then
        cata_typ = "'R'"
      else if(mydico(ikey)%ktype.eq.3) then
        cata_typ = 'bool'
      else if(mydico(ikey)%ktype.eq.4) then
        cata_typ = "'TXM'"
        ! If the keyword is a file changing type accordingly
        IF(mydico(ikey)%submit(1:1).ne.' ') then
          ! Check if it is an input or output file
          if (INDEX(mydico(ikey)%submit,'LIT').ne.0) then
            cata_typ="('Fichier','All Files (*)')"
          else
            cata_typ="('Fichier','All Files (*)','Sauvegarde')"
          endif
        endif
      else
        ! Should not happen
        write(*,*) 'Ay caramba'
        write(*,*) ikey, mydico(ikey)%knom(lng)
        call plante(1)
        stop
      endif
      ! If we have a into switching to TXM
      if(mydico(ikey)%choix(lng)(1:1).ne.' ') then
          cata_typ = "'TXM'"
      endif
      ! Defining size of variable
      if(mydico(ikey)%niveau.eq.666) then
        ! TODO: Remove that dirty hack
        cata_typ = trim(cata_typ)//", max='**'"
      else
        if(mydico(ikey)%taille.eq.0) then

          cata_typ = trim(cata_typ)//", min=0, max='**'"
        else if (mydico(ikey)%taille.gt.1) then
          write(cata_typ,'(A,I2,A,I2)') trim(cata_typ)//", min=",
     &                      mydico(ikey)%taille,", max=",
     &                      mydico(ikey)%taille
        endif
      endif
      end function
      !
      ! brief Build the Eficas catalog into string from DEFAUT in
      ! dictionary
      !
      ! param ikey id of the keyword to handle
      ! param lng The language to handle
      character(len=DEFAUT_LEN) function cata_defaut(ikey, lng)
      !
      implicit none
      !
      integer, intent(in) :: ikey
      integer, intent(in) :: lng
      !
      integer i
      integer :: mylen,pos_word,pos_egal,idx
      character(len=keyword_len) :: tmp
      !
      cata_defaut = repeat(' ',DEFAUT_LEN)
      ! Handle default values for hash
      if(mydico(ikey)%hash_id(1,lng)(1:1).ne.' ') then
        if(mydico(ikey)%taille.ne.1) then
          ! Loop on values
          mylen = len(trim(mydico(ikey)%defaut(lng)))
          cata_defaut = "["
          idx = 1
          pos_word = 1
          do while(idx.ne.0)
            idx = index(mydico(ikey)%defaut(lng)(pos_word:mylen),";")
            ! Check if it is the last one
            if(idx.eq.0) then
              ! Need to use a tmp string otherwise the whole string is
              ! passed
              tmp = repeat(' ',keyword_len)
              tmp(1:(mylen-pos_word)+1) =
     &              mydico(ikey)%defaut(lng)(pos_word:mylen)
              cata_defaut = trim(cata_defaut)//
     &                    trim(get_hash_value(ikey,tmp,lng))
     &                    // "]"
            else
              tmp = repeat(' ',keyword_len)
              tmp(1:(idx-1)) =
     &              mydico(ikey)%defaut(lng)(pos_word:pos_word+idx-2)
              cata_defaut = trim(cata_defaut) //
     &                  trim(get_hash_value(ikey,tmp,lng))
     &                  //","
            endif
            pos_word = pos_word +  idx
          enddo
        else
          cata_defaut = get_hash_value(ikey,
     &                                 mydico(ikey)%defaut(lng),lng)
        endif
      ! Cas if we have a string
      else if(mydico(ikey)%ktype.eq.4) then
        cata_defaut = "'"//trim(mydico(ikey)%defaut(lng))//"'"
      else if(mydico(ikey)%ktype.eq.3) then
        select case (trim(mydico(ikey)%defaut(lng)))
        case('NO','NON','FAUX','FALSE','.FALSE.','0')
          cata_defaut = 'False'
        case('VRAI','OUI','TRUE','YES','.TRUE.','1')
          cata_defaut = 'True'
        case default
          write(*,*) 'Unknown value for boolean: ',
     &               trim(mydico(ikey)%defaut(lng))
          call plante(1)
        end select
      else
        if(mydico(ikey)%taille.ne.1) then
          ! loop on values
          mylen = len(trim(mydico(ikey)%defaut(lng)))
          cata_defaut = "["
          idx = 1
          pos_word = 1
          do while(idx.ne.0)
            idx = index(mydico(ikey)%defaut(lng)(pos_word:mylen),";")
            ! Check if it is the last one
            if(idx.eq.0) then
              cata_defaut = trim(cata_defaut)//
     &                    mydico(ikey)%defaut(lng)(pos_word:mylen)
     &                    // "]"
            else
              cata_defaut = trim(cata_defaut) //
     &                 mydico(ikey)%defaut(lng)(pos_word:pos_word+idx-2)
     &                  //","
            endif
            pos_word = pos_word +  idx
          enddo
        else
          cata_defaut = trim(mydico(ikey)%defaut(lng))
        endif
      endif
      end function
      ! brief Build the Eficas catalog into string from CHOIX in
      ! dictionary
      !
      ! param ikey id of the keyword to handle
      ! param lng The language to handle
      character(len=CHOIX_LEN) function cata_into(ikey, lng)
      !
      implicit none
      !
      integer, intent(in) :: ikey
      integer, intent(in) :: lng
      !
      integer :: mylen,pos_word,pos_egal,idx
      integer :: i
      character(len=CHOIX_LEN) :: choix
      !
      pos_word = 1
      i = 1
      choix = mydico(ikey)%choix(lng)
      mylen = len(trim(choix))
      ! Here we have two cases:
      ! 1 - The list is of type '1=str';'2=str';...
      !     In which case we must identify the pairs
      !     And create the array of values in mydico
      ! 2 - It is a basic string list '"str"';'"str"';.??
      pos_egal = index(choix(pos_word:mylen),"=")
      if (pos_egal.eq.0) then
        ! Case 2
        cata_into = repeat(' ',CHOIX_LEN)
        idx = 1
        do while(idx.ne.0)
          idx = index(choix(pos_word:mylen),";")
          ! Check if it is the last one
          if(idx.eq.0) then
            cata_into = trim(cata_into) // "'" // choix(pos_word:mylen)
     &                  // "'"
          else
            cata_into = trim(cata_into) // "'" //
     &                  choix(pos_word:pos_word+idx-2)//"',"
          endif
          pos_word = pos_word +  idx
        enddo
      else
        ! Case 1
        cata_into = repeat(' ',CHOIX_LEN)
        do while(pos_egal.ne.0)
          if(i.gt.maxENUM) then
            write(*,*) 'Increase maxENUM'
            call plante(1)
            stop
          endif
          idx = index(choix(pos_egal:mylen),";")
          mydico(ikey)%hash_id(i,lng) = choix(pos_word:(pos_egal-1))
          pos_word = pos_egal +  idx - 1
          pos_egal = pos_egal + 1
          ! Check if it is the last one
          if(idx.eq.0) then
            mydico(ikey)%hash_val(i,lng) = choix(pos_egal:mylen)
            cata_into = trim(cata_into) // choix(pos_egal:mylen)
            pos_egal = 0
          else
            mydico(ikey)%hash_val(i,lng) = choix(pos_egal:pos_word-1)
            cata_into = trim(cata_into) //
     &                  choix(pos_egal:pos_word-1)//','
            pos_word = pos_word + 1
            pos_egal = pos_word + index(choix(pos_word:mylen),'=') -1
          endif
          i = i + 1
        enddo
      endif
      end function
      ! brief Return a string that replace "'" and " " by "_" from the
      ! given string
      !
      ! param string The string to modify
      character(len=KEYWORD_LEN) function cata_name(string)
      !
      implicit none
      !
      character(len=KEYWORD_LEN),intent(in) :: string
      !
      integer mylen,i
      mylen = len(trim(string))
      cata_name = repeat(' ',KEYWORD_LEN)
      do i=1,mylen
        if(string(i:i).eq."'".or.
     &     string(i:i).eq."-".or.
     &     string(i:i).eq." ") then
          cata_name(i:i) = "_"
        else
          cata_name(i:i) = string(i:i)
        endif
      enddo
      return
      end function
      ! brief Write in Python the enum for the keyword with CHOIX in
      ! form id:"str"
      !
      subroutine write_enum()
      !
      implicit none
      !
      integer :: nfic
      integer :: lng,ierr
      integer :: ikey,i
      logical isstr
      character(len=144) :: rub1
      !
      nfic = 666
      rub1 = repeat(' ',144)
      rub1 = 'enum_Telemac_auto.py'
      OPEN(NFIC,FILE=TRIM(rub1),IOSTAT=IERR)
      CALL CHECK_CALL(IERR,'CATA_DICTIONARY')
      !
      ! English
      !
      write(nfic,'(A)') '#/usr/bin/env python'
      write(nfic,'(A)') '# -*- coding: latin-1 -*-'
      write(nfic,'(A)') 'TelemacdicoEn = {'
      do ikey = 1,nkey
        ! Check if the keyword has an enum
        if(mydico(ikey)%hash_id(1,EN)(1:1).ne.' ') then
          write(nfic,'(A)') "'"//
     &       trim(cata_name(mydico(ikey)%knom(2)))//"' : {"
          i = 1
          select case (mydico(ikey)%hash_id(i,EN)(1:1))
            case ("-","0","1","2","3","4","5","6","7","8","9")
              isstr = .False.
            case default
              isstr = .true.
          end select
          do while(mydico(ikey)%hash_id(i,EN)(1:1).ne.' ')
            if(isstr) then
              write(nfic,'(4X,A)') "'"//
     &               trim(mydico(ikey)%hash_id(i,EN))//
     &               "':" // trim(mydico(ikey)%hash_val(i,EN)) // ","
            else
              write(nfic,'(4X,A)')
     &               trim(mydico(ikey)%hash_id(i,EN))//
     &               ":" // trim(mydico(ikey)%hash_val(i,EN)) // ","
            endif
            i = i + 1
          enddo
          write(nfic,'(2X,A)') "},"
        endif
      enddo
      write(nfic,'(A)') '}'
      !
      ! French
      !
      write(nfic,'(A)') 'TelemacdicoFr = {'
      do ikey = 1,nkey
        ! Check if the keyword has an enum
        if(mydico(ikey)%hash_id(1,FR)(1:1).ne.' ') then
          write(nfic,'(A)') "'"//
     &       trim(cata_name(mydico(ikey)%knom(EN)))//"' : {"
          i = 1
          select case (mydico(ikey)%hash_id(i,FR)(1:1))
            case ("-","0","1","2","3","4","5","6","7","8","9")
              isstr = .False.
            case default
              isstr = .true.
          end select
          do while(mydico(ikey)%hash_id(i,FR)(1:1).ne.' ')
            if(isstr) then
              write(nfic,'(4X,A)') "'"//
     &               trim(mydico(ikey)%hash_id(i,FR))//
     &               "':" // trim(mydico(ikey)%hash_val(i,FR)) // ","
            else
              write(nfic,'(4X,A)')
     &               trim(mydico(ikey)%hash_id(i,FR))//
     &               ":" // trim(mydico(ikey)%hash_val(i,FR)) // ","
            endif
            i = i + 1
          enddo
          write(nfic,'(2X,A)') "},"
        endif
      enddo
      write(nfic,'(A)') '}'
      write(nfic,'(A)') ''


      ! Writing Python dictionary for correspondance name-fr -> name_cata
      write(nfic,'(A)') 'DicoCasFrToCata = {'
      do ikey = 1,nkey
        write(nfic,'(2X,5A)') '"', trim(mydico(ikey)%knom(FR)),'":"',
     &                    trim(cata_name(mydico(ikey)%knom(EN))),'",'
      enddo
      write(nfic,'(A)') '}'
      write(nfic,'(A)') ''

      ! Writing Python dictionary for correspondance name-en -> name_cata
      write(nfic,'(A)') 'DicoCasEnToCata = {'
      do ikey = 1,nkey
        write(nfic,'(2X,5A)') "'", trim(mydico(ikey)%knom(EN)),"':'",
     &                    trim(cata_name(mydico(ikey)%knom(EN))),"',"
      enddo
      write(nfic,'(A)') '}'


      close(nfic)
      end subroutine
      ! brief Write the translation files
      !
      subroutine write_ts()
      !
      implicit none
      !
      integer nfic, ierr, ikey, i
      character(len=144) :: filename
      character(len=*) :: to_lower
      external to_lower
      nfic = 666
      filename = repeat(' ',144)
      filename = 'cata_name2eng_name.ts'

      OPEN(NFIC,FILE=TRIM(filename),IOSTAT=IERR)
      CALL CHECK_CALL(IERR,'CATA_DICTIONARY')

      write(nfic,'(A)') '<?xml version="1.0" encoding="utf-8"?>'
      write(nfic,'(A)') '<!DOCTYPE TS><TS version="1.1" language="en">'
      write(nfic,'(A)') '<context>'
      write(nfic,'(4X,A)') '<name>@defaut</name>'
      ! Loop on keywords
      do ikey=1,nkey
        write(nfic,'(4X,A)') '<message>'
        write(nfic,'(8X,A)') '<source>'//
     &             trim(cata_name(mydico(ikey)%knom(EN)))//
     &             '</source>'
        write(nfic,'(8X,A)') '<translation>'//
     &          trim(mydico(ikey)%knom(EN))//'</translation>'
        write(nfic,'(4X,A)') '</message>'
        if(mydico(ikey)%hash_id(1,EN)(1:1).ne.' ') then
          i = 1
          do while(mydico(ikey)%hash_id(i,EN)(1:1).ne.' ')
            write(nfic,'(4X,A)') '<message>'
            write(nfic,'(8X,A)') '<source>'//
     &             trim(cata_name(mydico(ikey)%hash_val(i,EN)))//
     &             '</source>'
            write(nfic,'(8X,A)') '<translation>'//
     &         trim(mydico(ikey)%hash_val(i,EN))//
     &                           '</translation>'
            write(nfic,'(4X,A)') '</message>'
            i = i + 1
          enddo
        endif
      enddo
      !
      write(nfic,'(A)') '</context>'
      write(nfic,'(A)') '</TS>'
      close(nfic)

      !
      ! French
      !
      filename = repeat(' ',144)
      filename = 'cata_name2fra_name.ts'

      OPEN(NFIC,FILE=TRIM(filename),IOSTAT=IERR)
      CALL CHECK_CALL(IERR,'CATA_DICTIONARY')

      write(nfic,'(A)') '<?xml version="1.0" encoding="utf-8"?>'
      write(nfic,'(A)') '<!DOCTYPE TS><TS version="1.1" language="en">'
      write(nfic,'(A)') '<context>'
      write(nfic,'(4X,A)') '<name>@defaut</name>'
      ! Loop on keywords
      do ikey=1,nkey
        write(nfic,'(4X,A)') '<message>'
        write(nfic,'(8X,A)') '<source>'//
     &             trim(cata_name(mydico(ikey)%knom(EN)))//
     &             '</source>'
        write(nfic,'(8X,A)') '<translation>'//
     &          trim(mydico(ikey)%knom(FR))//'</translation>'
        write(nfic,'(4X,A)') '</message>'
        if(mydico(ikey)%hash_id(1,FR)(1:1).ne.' ') then
          i = 1
          do while(mydico(ikey)%hash_id(i,FR)(1:1).ne.' ')
            write(nfic,'(4X,A)') '<message>'
            write(nfic,'(8X,A)') '<source>'//
     &             trim(cata_name(mydico(ikey)%hash_val(i,EN))) //
     &             '</source>'
            write(nfic,'(8X,A)') '<translation>'//
     &         trim(mydico(ikey)%hash_val(i,FR))//
     &                           '</translation>'
            write(nfic,'(4X,A)') '</message>'
            i = i + 1
          enddo
        endif
      enddo
      !
      write(nfic,'(A)') '</context>'
      write(nfic,'(A)') '</TS>'
      close(nfic)

      endsubroutine
      ! brief Write a rubrique in a cata
      !
      ! param nfic File descriptor
      ! param rub Name of the rubrique in cata form
      ! param level Indentation level
      subroutine write_begin_rubrique(nfic,rub,level)
      !
      implicit none
      !
      character(len=144), intent(in) :: rub
      integer, intent(in) :: nfic
      integer, intent(in) :: level
      !
      if(level.eq.1) then
        write(nfic,'(a)') '# '//repeat(' ',4*(level-1))//
     &                    repeat('-',71)
        write(nfic,'(a)') repeat(' ',4*(level-1))//
     &            trim(RUB)//' = PROC(nom= "'//
     &            trim(RUB)//
     &            '",op = None,'
        write(nfic,'(a)') '# '//repeat(' ',4*(level-1))//
     &                    repeat('-',71)
      else
        write(nfic,'(a)') '# '//repeat(' ',4*(level-1)-2)//
     &                    repeat('-',35)
        write(nfic,'(a)') repeat(' ',4*(level-1))//
     &            trim(RUB)//" = FACT(statut='f',"
        write(nfic,'(a)') '# '//repeat(' ',4*(level-1)-2)//
     &                    repeat('-',35)
      endif
      end subroutine write_begin_rubrique
      ! brief Write a rubrique in a cata
      !
      ! param nfic File descriptor
      ! param rub Name of the rubrique in cata form
      ! param level Indentation level
      subroutine write_end_rubrique(nfic,rub,level)
      !
      implicit none
      !
      character(len=144), intent(in) :: rub
      integer, intent(in) :: nfic
      integer, intent(in) :: level
      !
      if(level.eq.1) then
        write(nfic,'(a)') repeat(' ',4*(level-1))//')'
      else
        write(nfic,'(a)') repeat(' ',4*(level-1))//'),'
      endif
      end subroutine write_end_rubrique
      ! brief Write a conditional bloc in a cata
      !
      ! param nfic File descriptor
      ! param ikey Index of the keyword associated with the bloc
      ! param icond Index of the condition
      ! param level Indentation level
      ! param lng Language of output
      subroutine write_begin_bloc(nfic,ikey,icond,level,lng)
      !
      implicit none
      !
      integer, intent(in) :: nfic
      integer, intent(in) :: ikey
      integer, intent(in) :: icond
      integer, intent(in) :: level
      integer, intent(in) :: lng
      !
      character(len=KEYWORD_LEN) mycata_name
      !
      mycata_name = cata_name(mydico(ikey)%knom(lng))
      !
      write(nfic,'(a)') '# '//repeat(' ',4*(level)-2)//
     &                  repeat('-',35)
      ! using max(icond,1) cause when it is an internal condition icond
      ! equal 0
      write(nfic,'(a)') repeat(' ',4*(level))//
     &         'b_'// trim(mycata_name)//char(icond+70)//
     &         ' = BLOC(condition="'//
     &         trim(mydico(ikey)%cond(max(icond,1)))//'",'
      write(nfic,'(a)') '# '//repeat(' ',4*(level)-2)//
     &                  repeat('-',35)
      end subroutine write_begin_bloc
      ! brief Write a conditional bloc in a cata
      !
      ! param nfic File descriptor
      ! param ikey Index of the keyword associated with the bloc
      ! param icond Index of the condition
      ! param level Indentation level
      ! param lng Language of output
      subroutine write_end_bloc(nfic,ikey,icond,level,lng)
      !
      implicit none
      !
      integer, intent(in) :: nfic
      integer, intent(in) :: ikey
      integer, intent(in) :: icond
      integer, intent(in) :: level
      integer, intent(in) :: lng
      !
      write(nfic,'(a)') repeat(' ',4*(level))//'),'
      end subroutine write_end_bloc
      !
      ! brief Write a keyword in a cata
      !
      ! param nfic File descriptor
      ! param ikey Key word id
      ! param level Indentation level
      ! param lng Language of ouput
      recursive subroutine write_keyword2cata(nfic,ikey,level,lng)
      !
      implicit none
      !
      integer, intent(in) :: ikey
      integer, intent(in) :: nfic
      integer, intent(in) :: level
      integer, intent(in) :: lng
      !
      character(len=KEYWORD_LEN) :: cat_name
      character(len=3) cata_stat
      character(len=72) cata_file
      character(len=CHOIX_LEN) :: mycata_into
      integer :: mylen,ierr,i
      integer :: icond,idep
      !
      if(key_written(ikey)) then
        return
      else
        key_written(ikey) = .true.
      endif
      cat_name = cata_name(mydico(ikey)%knom(lng))
      !
      ! Check if the keyword it selfs has a condition
      if(mydico(ikey)%cond(1)(1:1).ne.' '.and.
     &   mydico(ikey)%depen(1,1).eq.0) then
        ! Writing the keyword inside a conditional bloc
        call write_begin_bloc(nfic,ikey,0,level,lng)
        call write_keyword2cata(nfic,ikey,level+1,lng)
        call write_end_bloc(nfic,ikey,0,level,lng)
      endif
      !
      if(mydico(ikey)%niveau.eq.0) then
        cata_stat="'o'"
      else
        cata_stat="'f'"
      endif
      !
      ! Building list of values
      if(mydico(ikey)%choix(lng)(1:1).ne.' ') then
        mycata_into = cata_into(ikey,3-lng)
        mycata_into = cata_into(ikey,lng)
      endif

      ! Name and status (mandatory or optional)
      write(nfic,'(a)') '# '//repeat(' ',4*(level)-2)//
     &                  repeat('-',35)
      WRITE(nfic,'(a)') repeat(' ',4*level)//trim(cat_name)//
     &              ' = SIMP(statut ='//
     &           cata_stat//","
      write(nfic,'(a)') '# '//repeat(' ',4*(level)-2)//
     &                  repeat('-',35)
      ! Type of the entry
      WRITE(nfic,'(a)') repeat(' ',4*(level+1))//'typ = '//
     &                  trim(cata_typ(ikey,lng))//","
      ! List of values if there is one
      if(mydico(ikey)%choix(lng)(1:1).ne.' ') then
        WRITE(nfic,'(a)') repeat(' ',4*(level+1))//'into = ['//
     &                    trim(mycata_into)//"],"
      endif
      ! Default value
      if(mydico(ikey)%defaut(1).ne.'OBLIGATOIRE') then
        WRITE(nfic,'(a)') repeat(' ',4*(level+1))//"defaut = "//
     &                    trim(cata_defaut(ikey,lng))//","
      endif
      ! Help in french
      WRITE(nfic,'(a)') repeat(' ',4*(level+1))//'fr = """'//
     &           trim(mydico(ikey)%aide(FR))//'""",'
      ! Help in English
      WRITE(nfic,'(a)') repeat(' ',4*(level+1))//'ang = """'//
     &           trim(mydico(ikey)%aide(EN))//'""",'
      write(nfic,'(a)') repeat(' ',4*(level))//"),"

      ! Check if the keyword has dependencies
      if(mydico(ikey)%cond(1)(1:1).ne.' '.and.
     &   mydico(ikey)%depen(1,1).ne.0) then
        do icond=1,MAXCOND
          if (mydico(ikey)%cond(icond)(1:1).eq.' ') exit
          call write_begin_bloc(nfic,ikey,icond,level,lng)
          do idep=1,MAXDEP
            if (mydico(ikey)%depen(icond,idep).eq.0) exit
            call write_keyword2cata(nfic,
     &                              mydico(ikey)%depen(icond,idep),
     &                              level+1,lng)
          enddo
          call write_end_bloc(nfic,ikey,icond,level,lng)
        enddo
      endif
      end subroutine
      !
      ! brief Write an Eficas Catalog from the dictionary
      !
      ! param filename Name of the Catalog file
      SUBROUTINE WRITE2CATA(FILENAME)
      !
      implicit none
      !
      character(len=144), intent(in) :: filename
      !
      integer :: nfic,IERR
      INTEGER :: IRUB1
      INTEGER :: IRUB2
      INTEGER :: IRUB3
      CHARACTER(LEN=144) RUB1,RUB2,RUB3
      integer :: ikey
      INTEGER LEVEL,LNG
      NFIC = 666
      LNG = EN
      rub1 = repeat(' ',144)
      rub1 = 'telemac2d.dico.dep'
      allocate(key_written(nkey),stat=ierr)
      call check_allocate(ierr,'key_written')
      key_written = .false.
      call read_dependencies(rub1)
      WRITE(*,*) '---- EFICAS CATALOG PROCESS ----'
      WRITE(*,*) 'WRITING IN : ',TRIM(FILENAME)
      OPEN(NFIC,FILE=TRIM(FILENAME),IOSTAT=IERR)
      CALL CHECK_CALL(IERR,'CATA_DICTIONARY')
      write(nfic,'(a)') ''
      write(nfic,'(a)') '# coding: utf-8'
      write(nfic,'(a)') ''
      write(nfic,'(a)') 'from Accas import *'
      write(nfic,'(a)') 'class DateJJMMAAAA:'
      write(nfic,'(a)') '  def __init__(self):'
      write(nfic,'(a)') '    self.ntuple=3'
      write(nfic,'(a)') ''
      write(nfic,'(a)') '  def __convert__(self,valeur):'
      write(nfic,'(a)') '    if type(valeur) == types.StringType: '//
     &              'return None'
      write(nfic,'(a)') '    if len(valeur) != self.ntuple: return None'
      write(nfic,'(a)') '    return valeur'
      write(nfic,'(a)') ''
      write(nfic,'(a)') '  def info(self):'
      write(nfic,'(a)') '    return "Date : jj/mm/aaaa "'
      write(nfic,'(a)') ''
      write(nfic,'(a)') '  __repr__=info'
      write(nfic,'(a)') '  __str__=info'
      write(nfic,'(a)') ''
      write(nfic,'(a)') 'class grma(GEOM):'
      write(nfic,'(a)') '  pass'
      write(nfic,'(a)') ''
      write(nfic,'(a)') 'import types'
      write(nfic,'(a)') 'class Tuple:'
      write(nfic,'(a)') '  def __init__(self,ntuple):'
      write(nfic,'(a)') '    self.ntuple=ntuple'
      write(nfic,'(a)') ''
      write(nfic,'(a)') '  def __convert__(self,valeur):'
      write(nfic,'(a)') '    if type(valeur) == types.StringType:'
      write(nfic,'(a)') '      return None'
      write(nfic,'(a)') '    if len(valeur) != self.ntuple:'
      write(nfic,'(a)') '      return None'
      write(nfic,'(a)') '    return valeur'
      write(nfic,'(a)') ''
      write(nfic,'(a)') '  def info(self):'
      write(nfic,'(a)')'    return "Tuple de %s elements" % self.ntuple'
      write(nfic,'(a)') ''
      write(nfic,'(a)') ''
      write(nfic,'(a)') ''
      write(nfic,'(a)') "JdC = JDC_CATA (code = 'TELEMAC',"
      write(nfic,'(a)') '                execmodul = None,'
      write(nfic,'(a)') '                )'
      write(nfic,'(a)') '# '//repeat("=",71)
      write(nfic,'(a)') '# Catalog entry for the MAP function : '//
     &              'c_pre_interfaceBody_mesh'
      write(nfic,'(a)') '# '//repeat("=",71)
      write(nfic,'(a)') ''

      ! Loop on rubriques
      DO IRUB1=1,NRUB(LNG,1)
        RUB1 = cata_name(RUBRIQUE(LNG,IRUB1,1))
        call write_begin_rubrique(nfic,rub1,1)
        DO IKEY=1,NKEY
          ! Identifying keywwords that are 1 1
          IF(HAS_RUBRIQUE(IKEY,IRUB1,1,LNG).AND.
     &       (MYDICO(IKEY)%RUBRIQUE(LNG,2)(1:1).EQ.' ')) THEN
            call write_keyword2cata(nfic,ikey,1,lng)
          ENDIF
        ENDDO
        ! LEVEL 2
        ! Loop on rubriques
        DO Irub2=1,NRUB(LNG,2)
          LEVEL = 2
          RUB2 = cata_name(RUBRIQUE(LNG,IRUB2,2))
          IF(RUB1_DEP(IRUB1,IRUB2)) THEN
            call write_begin_rubrique(nfic,rub2,2)
            DO IKEY=1,NKEY
              ! Identifying keywwords that are 2 1
              IF(HAS_RUBRIQUE(IKEY,IRUB1,1,LNG).AND.
     &           HAS_RUBRIQUE(IKEY,IRUB2,2,LNG).AND.
     &           (MYDICO(IKEY)%RUBRIQUE(LNG,3)(1:1).EQ.' ')) THEN
                call write_keyword2cata(nfic,ikey,2,lng)
              ENDIF
            ENDDO
            ! LEVEL 3
            ! Loop on rubriques
            DO IRUB3=1,NRUB(LNG,3)
              LEVEL = 3
              RUB3 = cata_name(RUBRIQUE(LNG,IRUB3,3))
              IF(RUB2_DEP(IRUB1,IRUB2,IRUB3)) THEN
                call write_begin_rubrique(nfic,rub3,3)
                DO IKEY=1,NKEY
                  ! Identifying keywwords that are 3 1
                  IF(HAS_RUBRIQUE(IKEY,IRUB1,1,LNG).AND.
     &               HAS_RUBRIQUE(IKEY,IRUB2,2,LNG).AND.
     &               HAS_RUBRIQUE(IKEY,IRUB3,3,LNG)) THEN
                    call write_keyword2cata(nfic,ikey,3,lng)
                  ENDIF
                ENDDO
                call write_end_rubrique(nfic,rub3,3)
              ENDIF
            ENDDO ! LEVEL 3
            call write_end_rubrique(nfic,rub2,2)
          ENDIF
        ENDDO ! LEVEL 2
        call write_end_rubrique(nfic,rub1,1)
      ENDDO ! LEVEL 1

      write(nfic,'(A)') 'Ordre_des_commandes = ('
      DO irub1 =1,nrub(lng,1)-1
        write(nfic,'(A)')
     &     "'"//trim(cata_name(RUBRIQUE(lng,irub1,1)))//"',"
      enddo
      write(nfic,'(A)')
     &   "'"//trim(cata_name(RUBRIQUE(lng,nrub(lng,1),1)))//"')"
      CLOSE(nfic)
      deallocate(key_written)

      ! Writing enum.py
      call write_enum()
      ! Writing ts
      call write_ts()
      END SUBROUTINE
      END MODULE UTILS_CATA
