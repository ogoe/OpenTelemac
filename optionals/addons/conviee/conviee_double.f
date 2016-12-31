C-------------------------DIFFSEL---------------------------
C
C    Conversion d'un fichier SELAFIN entre les formats
C        IEEE_little_endian et IEEE_big_endian
C
C
C    S. Aunay  - Septembre 1998
C
C------------------------DIFFSEL------------------DeltaCAD--

      IMPLICIT NONE 
      integer nvar,i
      integer   itab(200000)
      integer   it(10)
      DOUBLE PRECISION xtab(200000)
      DOUBLE PRECISION r1

      CHARACTER*80  tit80
      integer     if1, nbrec,j,il1,k
      integer       ndp, npoin, ii,nelem
      character*32  c_32
      character*100 ficnam1, ficnam2

C------ Nom des 2 fichiers à comparer (SELAFIN)

      print*,'Nom du fichier a convertir '
      read(*,*) ficnam1
!
      do 20 i=100,1,-1
      if (ficnam1(i:i).ne.' ') goto 30
20    continue
      goto 850
30    continue
      il1=i
!
      print*,'Format initial du fichier : 1=LITTLE_ENDIAN, 2=BIG_ENDIAN'
      read(5,*) if1
      if ( (if1.ne.1) .and. (if1.ne.2) ) goto 820

      print*,' '
      print*,'   Conversion du fichier SELAFIN : "', ficnam1(1:il1),'"'
      print*,' '

C------ Ouverture des fichiers :
      
50    continue
      if (if1.eq.1) then
        ficnam2=ficnam1(1:il1)//'_BIG'
        il1=il1+4
        open (unit=1, file=ficnam1, form='unformatted', 
     1        convert='LITTLE_ENDIAN',ERR=800, STATUS='OLD')
        open (unit=2, file=ficnam2, form='unformatted', 
     1        convert='BIG_ENDIAN',ERR=810, STATUS='REPLACE')
      print*, '   du format IEEE_Little_Endian'
      print*, '   au format IEEE_BIG_Endian'
      print*, '       --> dans le fichier "', ficnam2(1:il1),'"'

      else
        ficnam2=ficnam1(1:il1)//'_LITTLE'
        il1=il1+7
        open (unit=1, file=ficnam1, form='unformatted', 
     1        convert='BIG_ENDIAN',ERR=800, STATUS='OLD')
        open (unit=2, file=ficnam2, form='unformatted', 
     1        convert='LITTLE_ENDIAN',ERR=810, STATUS='REPLACE')
      print*, '   du format IEEE_Big_Endian'
      print*, '   au format IEEE_Little_Endian'
      print*, '       --> dans le fichier "', ficnam2(1:il1),'"'

      endif      

      nbrec=0

C------ #1 : Titre
      
	read  (1, err=830) tit80
	write (2, err=840) tit80
	nbrec=nbrec+1

C------ #2 : NBV_1 et NBV_2

      read  (1, err=830) i, j
      write (2, err=840) i, j
      nbrec=nbrec+1

C------ #3 : Noms et unités

	nvar = i + j
      do 100 i=1, nvar
      read (1, err=830) C_32
      write(2, err=840) C_32
      nbrec=nbrec+1
100   continue

C------ #4 : 1,0,0,0,0,0,0,0,0,0,0

      read (1, err=830)  ( it(k), k=1,10)
      write(2, err=840)  ( it(k), k=1,10)
      nbrec=nbrec+1
C
      if (it(10) .eq. 1) then
      read (1, err=830)  ( it(k), k=1,6)
      write(2, err=840)  ( it(k), k=1,6)
	nbrec=nbrec+1
      endif

C------ #5 : NELEM, NPOIN, NDP, 1

      read  (1, err=830) nelem, npoin, ndp, ii
      write (2, err=840) nelem, npoin, ndp, ii
      nbrec=nbrec+1

C------ #6 : IKLE

      read  (1, err=830) (itab(i), i=1, nelem*ndp)
      write (2, err=840) (itab(i), i=1, nelem*ndp)
      nbrec=nbrec+1

C------ #7 : IPOBO

      read  (1, err=830) (itab(i), i=1, npoin)
      write (2, err=840) (itab(i), i=1, npoin)
      nbrec=nbrec+1

C------ #8 : X

      read  (1, err=830) (xtab(i), i=1, npoin)
      write (2, err=840) (xtab(i), i=1, npoin)
      nbrec=nbrec+1

C------ #9 : Y

      read  (1, err=830) (xtab(i), i=1, npoin)
      write (2, err=840) (xtab(i), i=1, npoin)
      nbrec=nbrec+1

C------ #10 : t

500   CONTINUE

       read (1, err=830, end=900) r1
       write(2, err=840) r1
	 nbrec=nbrec+1

C------ #9 : NVAR vecteurs

      if ( nvar .lt. 1) goto 500
      do 600 j=1,nvar
        read  (1, err=830) (xtab(i), i=1, npoin)
        write (2, err=840) (xtab(i), i=1, npoin)
        nbrec=nbrec+1
600   continue

      goto 500

C------- Erreurs

800   stop 'ERREUR : Ouverture du fichier initial impossible.'
810   stop 'ERREUR : Ouverture du fichier converti impossible.'
820   stop 'ERREUR : format de fichier invalide.'
830   print*, 'ERREUR : lecture du fichier initial, record ',nbrec+1
      stop 
840   print*, 'ERREUR : ecriture du fichier final, record ',nbrec+1
      stop 
850   stop 'ERREUR : nom du fichier initial invalide.'

C------- Fin : fermeture des fichiers

900   continue
	close (1)
	close (2)
      print*,' '
      print*,'   Fin : ',nbrec,' enregistrements convertis.'

      end
 
