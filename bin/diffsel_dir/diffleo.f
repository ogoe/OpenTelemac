C-------------------------DIFFLEO---------------------------
C
C    Comparaison de 2 fichiers LEONARD.
C
C    Affichage des écarts max si fichiers différents.
C
C
C       "fic.asc"    -->  ASC2SEL  --> "fic.bin"
C
C
C
C    S. Aunay  - Aout 1998
C
C------------------------DIFFLEO------------------DeltaCAD--

c	IMPLICIT NONE 
	integer nvar,i


      dimension itab(100000)
      dimension jtab(100000)
	dimension it1(10), it2(10)
      real  xtab(100000)
      real  ytab(100000)
	real r1, r2, epsilon, epsref, epsmax

      CHARACTER*72 TITRE,tit1,tit2
	integer i1,i2,j1,j2

      character*32 c1_32,c2_32
      character*30 ficnam1, ficnam2

C------ Nom des 2 fichiers à comparer (LEONARD)

      print*,'Nom du fichier selafin num 1 '
      accept*, ficnam1

      ficnam2='                                '
	print*,'Nom du fichier selafin num 2 '
      accept*, ficnam2
	if ( ficnam2(1:1) .eq. '=' ) then
	  ficnam2='lnh\'//ficnam1
	endif

	epsref = 1.E-10
	epsmax = 0.
      print*,'                    Fichiers LEONARD : '
      print*,'      - ', ficnam1
      print*,'      - ', ficnam2
      print*,' '

C------ Exploitation des fichiers 
      
      open (unit=2, file=ficnam2, form='unformatted')
      open (unit=1, file=ficnam1, form='unformatted')


C------ #1 : Titre
      
	read (1) tit1
	read (2) tit2
	if ( tit1 .eq. tit2 ) then
        print *, ' #1 ... OK'
	else
        STOP ' #2 ... ERREUR : different'
	endif


C------ #2 : NBV_1 et NBV_2

      read (1) i1, i2
      read (2) j1, j2
	if (i1 .ne. j1) print*, ' #2 ... ERREUR I1=',i1,
     1                                   ',  J1=',j1
	if (i2 .ne. j2) print*, ' #2 ... ERREUR I2=',i2,
     1                                   ',  J2=',j2

	print *, ' #2 ... OK'
C------ #3 : Noms et unités

	nvar = i1 + i2
      do 100 i=1, nvar
      read(1) C1_32
      read(2) C2_32

	if (c1_32 .ne. c2_32) then
          print*, ' #3 ... ERREUR C1_32=',c1_32,
     2                      ', C2_32=',c2_32
	    stop
	                      endif

100   continue
	print *, ' #3 ... OK,   nvar=',nvar

C------ #4 : IM1, JM1, IM1, JM1, 1

      read (1) im1, jm1
      read (2) im2, jm2
	if ( im1 .ne. im2)
     1     stop ' #4 ... ERREUR : im1<>im2'
      if ( jm1 .ne. jm2 )
     1     stop ' #4 ... ERREUR : jm1<>jm2'
	print *,' #5 ... OK,  im=',im1,',  jm=',jm1


C------ #5 : 1,0,0,0,0,0,0,0,0,0,0

      read(1)  ( it1(k), k=1,10)
      read(2)  ( it2(k), k=1,10)
	do 20 k=1, 10
	if (it1(k) .ne.it2(k)) then
	  print*, ' #5 ... ERREUR it1=',it1(k),
     1                     ',   it2=',it2(k)
	stop
	                       endif
20	continue
C
      if (it1(10) .eq. 1) then
      read(1)  ( it1(k), k=1,6)
      read(2)  ( it2(k), k=1,6)
	do 30 k=1, 10
	if (it1(k) .ne. it2(k)) then
	  print*, ' #5 ... ERREUR it1=',it1(k),
     1                     ',   it2=',it2(k)
	stop
	                       endif
30	continue
					  endif
	print *, ' #5 ... OK'
555   continue


C------ #6 : X

C
      npoin1=jm1*im1
C
      read (1) (xtab(i), i=1, npoin1)
      read (2) (ytab(i), i=1, npoin1)
	epsilon=0.
	do 52 k=1, npoin1
	if (abs(xtab(k)-ytab(k)) .gt. epsmax)
     1      epsmax=abs(xtab(k)-ytab(k))
	if (abs(xtab(k)-ytab(k)) .gt. epsilon) 
     1	  epsilon = abs(xtab(k)-ytab(k) )                                 
52	continue
	if (epsilon .gt. epsref) then
	  print*, ' #6 ... ERREUR : epsilon = ', epsilon
	  stop 
	                         endif

	print *, ' #6 ... OK'

C------ #7 : Y

      read (1) (xtab(i), i=1, npoin1)
      read (2) (ytab(i), i=1, npoin1)
	epsilon=0.
	do 200 k=1, npoin1
	if (abs(xtab(k)-ytab(k)) .gt. epsmax)
     1      epsmax=abs(xtab(k)-ytab(k))
	if (abs(xtab(k)-ytab(k)) .gt. epsilon) 
     1	  epsilon = abs(xtab(k)-ytab(k) )                                 
200	continue
	if (epsilon .gt. epsref) then
	  print*, ' #7 ... ERREUR : epsilon = ', epsilon
	  stop 
	                         endif
	print *, ' #7 ... OK'


C------ #8 : INDIC

      read (1) (itab(i), i=1, npoin1)
      read (2) (jtab(i), i=1, npoin1)
	epsilon=0.
	do 50 k=1, npoin1
	if (abs(itab(k)-jtab(k)) .gt. epsmax)
     1      epsmax=abs(itab(k)-jtab(k))
	if (abs(itab(k)-jtab(k)) .gt. epsilon) 
     1	  epsilon = abs(itab(k)-jtab(k) )                                 
50	continue
	if (epsilon .gt. epsref) then
	  print*, ' #8 ... ERREUR : epsilon = ', epsilon
	  stop 
	                         endif
	print *, ' #8 ... OK'

C------ #10 : t

800   CONTINUE

c      read(1, 1100, end=9999) r1
       read(1, end=9999) r1
       read(2) r2
	 if (abs(r1-r2) .gt. epsref) then
	   print*, '# 10 ... ERREUR : epsilon = ', abs(r1-r2), ', 
     1           t1=', r1, ', t2=',r2
	   stop
	 endif
	 print*, '#10 ... t1=t2=', r1


C------ #9 : NVAR vecteurs

      if ( nvar .lt. 1) goto 800
      do 850 j=1,nvar

        read (1) (xtab(i), i=1, npoin1)
        read (2) (ytab(i), i=1, npoin1)
        epsilon=0.
        do 54 k=1, npoin1
	  if (abs(xtab(k)-ytab(k)) .gt. epsmax) 
     1      epsmax=abs(xtab(k)-ytab(k))
	  if (abs(xtab(k)-ytab(k)) .gt. epsilon) 
     1    epsilon = abs(xtab(k)-ytab(k) )                                 
54	  continue
	  if (epsilon .gt. epsref) then
	    print*, ' #11 ... ERREUR : epsilon = ', epsilon, ', t = ', r1
c	    stop 
	                           endif
850   continue

      goto 800


C------- Fin : fermeture des fichiers

9999  CONTINUE
      print*,' '
	print*, '   -> epsilon max global : ', epsmax
	close (1)
	close (2)

      end
 
 
 
