!                    *********************
                     SUBROUTINE PRE4_MUMPS
!                    *********************
!
     &(NPOIN,NSEGB,GLOSEGB,DAB1,DAB2,DAB3,DAB4,XAB1,XAB2,XAB3,XAB4,
     & XX1,XX2,CVB1,CVB2,INFOGR,TYPEXT1,TYPEXT2,TYPEXT3,TYPEXT4,
     & KNOLG,NPOIN_TOT)
!
!***********************************************************************
! MUMPSVOID   V7P1
!***********************************************************************
!
!brief    CALLS THE DIRECT SOLVER MUMPS.
!+        IF MUMPS IS NOT INSTALLED : EMPTY SUBROUTINES ARE USED INSTEAD.
!
!history  E. RAZAFINDRAKOTO (LNH)
!+        20/11/2006
!+
!+
!
!history  C. DENIS (SINETICS)
!+        14/10/2009
!+        V5P9
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        23/02/2015
!+        V7P1
!+   Arguments TYPEXT1,2,3,4 added, for calling SD_FABSG4
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CVB1,CVB2      |-->| SECONDS MEMBRES
!| DAB1           |---|
!| DAB2           |---|
!| DAB3           |---|
!| DAB4           |---|
!| GLOSEGB        |---|
!| INFOGR         |-->| IF, YES INFORMATIONS ON LISTING
!| NPOIN          |-->| NOMBRE D'INCONNUES
!| NSEGB          |-->| NOMBRE DE SEGMENTS
!| TYPEXT         |-->| TYPE OF EXTRA-DIAGONAL TERMS OF MATRICES
!| XAB1           |---|
!| XAB2           |---|
!| XAB3           |---|
!| XAB4           |---|
!| XX1,XX2        |<--| SOLUTIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN,NSEGB,NPOIN_TOT
      INTEGER, INTENT(IN) :: GLOSEGB(NSEGB*2),KNOLG(*)
      LOGICAL, INTENT(IN) :: INFOGR
      DOUBLE PRECISION, INTENT(IN)    :: DAB1(NPOIN),DAB2(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: DAB3(NPOIN),DAB4(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: XAB1(NSEGB),XAB2(NSEGB)
      DOUBLE PRECISION, INTENT(IN)    :: XAB3(NSEGB),XAB4(NSEGB)
      DOUBLE PRECISION, INTENT(INOUT) :: XX1(NPOIN),XX2(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: CVB1(NPOIN),CVB2(NPOIN)
      CHARACTER(LEN=1), INTENT(IN)    :: TYPEXT1,TYPEXT2
      CHARACTER(LEN=1), INTENT(IN)    :: TYPEXT3,TYPEXT4
!
#if defined(HAVE_MUMPS)
      INTEGER NPBLK,NSEGBLK,I
!
      INTEGER, ALLOCATABLE          :: GLOSEG4(:)
      DOUBLE PRECISION, ALLOCATABLE :: XA(:),DA(:)
      DOUBLE PRECISION, ALLOCATABLE :: RHS(:),XINC(:)     
!
      INTEGER SIZE_GLOSEG4,SIZE_DA,SIZE_XA,SIZE_RHS,SIZE_XINC
!
      DATA SIZE_GLOSEG4/0/
      DATA SIZE_DA     /0/
      DATA SIZE_XA     /0/
      DATA SIZE_RHS    /0/
      DATA SIZE_XINC   /0/
!
      SAVE
!
!-----------------------------------------------------------------------
!
      NPBLK=NPOIN*2
      NSEGBLK=4*NSEGB+NPOIN 
!    
      IF(SIZE_GLOSEG4.EQ.0) THEN      
        ALLOCATE(GLOSEG4(2*NSEGBLK))
        SIZE_GLOSEG4=    2*NSEGBLK
      ELSEIF(            2*NSEGBLK.GT.SIZE_GLOSEG4) THEN
        DEALLOCATE(GLOSEG4)
        ALLOCATE(GLOSEG4(2*NSEGBLK))
        SIZE_GLOSEG4=    2*NSEGBLK
      ENDIF 
      IF(SIZE_DA.EQ.0) THEN
        ALLOCATE(DA(NPBLK))      
        SIZE_DA=    NPBLK
      ELSEIF(       NPBLK.GT.SIZE_DA) THEN
        DEALLOCATE(DA)
        ALLOCATE(DA(NPBLK))
        SIZE_DA=    NPBLK
      ENDIF                
      IF(SIZE_XA.EQ.0) THEN      
        ALLOCATE(XA(2*NSEGBLK))
        SIZE_XA=    2*NSEGBLK
      ELSEIF(       2*NSEGBLK.GT.SIZE_XA) THEN
        DEALLOCATE(XA)
        ALLOCATE(XA(2*NSEGBLK))
        SIZE_XA=    2*NSEGBLK
      ENDIF              
      IF(SIZE_RHS.EQ.0) THEN
        ALLOCATE(RHS(NPBLK))
        SIZE_RHS=    NPBLK
      ELSEIF(        NPBLK.GT.SIZE_RHS) THEN
        DEALLOCATE(RHS)
        ALLOCATE(RHS(NPBLK))
        SIZE_RHS=    NPBLK
      ENDIF 
      IF(SIZE_XINC.EQ.0) THEN
        ALLOCATE(XINC(NPBLK))
        SIZE_XINC=    NPBLK
      ELSEIF(         NPBLK.GT.SIZE_XINC) THEN
        DEALLOCATE(XINC)
        ALLOCATE(XINC(NPBLK))
        SIZE_XINC=    NPBLK
      ENDIF 
!
!-----------------------------------------------------------------------
!       
!     1. SECOND MEMBRE DU SYSTEME
!     ===========================
!
      DO I=1,NPOIN
        RHS(I)      = CVB1(I)
        RHS(I+NPOIN)= CVB2(I)
      ENDDO
!              
!     2. CONSTRUCTION STOCKAGE SEGMENT MATRICE BLOCK (DE 4)
!     ===================================================== 
!
      CALL SD_STRSG4(NPOIN,NSEGB,GLOSEGB,NPBLK,NSEGBLK,GLOSEG4)
!     
      CALL SD_FABSG4(NPOIN,NSEGB,DAB1,DAB2,DAB3,DAB4,
     &               XAB1,XAB2,XAB3,XAB4,NPBLK,NSEGBLK,DA,XA,
     &               TYPEXT1,TYPEXT2,TYPEXT3,TYPEXT4)
! 
!     3. RESOLUTION : APPEL A MUMPS
!     =============================
! 
!     NOTE JMH 23/02/2015 : THE ARGUMENTS GIVEN TO SOLVE_MUMPS ARE THOSE
!                           OF SOLVE_MUMPS_PAR !!!!
!                           AND SOLVE_MUMPS IS VOID !!!!!
!   
!     CALL SOLVE_MUMPS(NPBLK,NSEGBLK,GLOSEG4,NSEGBLK,DA,XA,
!    &                 XINC,RHS,INFOGR,TYPEXT,KNOLG,
!    &                 NPOIN_TOT,IPID)
!
      CALL SOLVE_MUMPS_PAR(NPBLK,NSEGBLK,GLOSEG4,NSEGBLK,DA,XA,
     &                     XINC,RHS,INFOGR,TYPEXT,KNOLG,
     &                     NPOIN_TOT,IPID)
!
!     4. RECUPERATION DES INCONNUES
!     ============================= 
! 
      DO I=1,NPOIN
        XX1(I)= XINC(I)
        XX2(I)= XINC(I+NPOIN)
      ENDDO
!   
#else
      IF(LNG.EQ.1) WRITE(LU,2018)
      IF(LNG.EQ.2) WRITE(LU,2019)
2018  FORMAT(1X,'MUMPS NON INSTALLE SUR CE SYSTEME,',/,1X,
     &     'CHOISIR UNE AUTRE METHODE',///)
2019  FORMAT(1X,'MUMPS NOT INSTALLED IN THIS SYSTEM',/,1X,
     &     'CHOOSE OTHER METHOD ',///)
      CALL PLANTE(1)
      STOP
#endif
!
!-----------------------------------------------------------------------
!
      RETURN
      END
