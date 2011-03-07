!                    ********************
                     SUBROUTINE SD_STRSG4
!                    ********************
!
     &(NPOIN,NSEG,GLOSEGB,NPBLK,NSEGBLK,GLOSEG4)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE SEGMENTS OF THE MATRIX IN A SINGLE BLOCK.
!
!history  E. RAZAFINDRAKOTO (LNH)
!+        20/11/06
!+        V5P7
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| GLOSEG4        |<--| IF, YES INFORMATIONS ON LISTING
!| GLOSEGB        |-->| NUMEROS GLOBAUX DES POINTS DES SEGMENTS
!| NPBLK          |-->| COMME NPOIN MAIS POUR LE BLOC
!| NPOIN          |-->| NOMBRE D'INCONNUES D'UNE MATRICE DU BLOC
!| NSEG           |-->| NOMBRE DE SEGMENTS
!| NSEGBLK        |-->| COMME NSEG MAIS POUR LE BLOC
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_SD_STRSG4 => SD_STRSG4
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NSEGBLK,NPBLK,NSEG,NPOIN
      INTEGER, INTENT(IN)    :: GLOSEGB(NSEG,2)
      INTEGER, INTENT(INOUT) :: GLOSEG4(2*NSEGBLK)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,ISEG,JSEG
!
!----------------------------------------------
!     INFO :      NPBLK = NPOIN*NBLOC
!                 NSEGBLK=NSEG*4 + 2*NPOIN
!----------------------------------------------
!
!     MATRIX ASSEMBLES TOTAL BLOCKS:
!
!----------------------------------------------
!
      JSEG=0
!
!     BLOCK 1
!     ------
!
      DO ISEG=1,NSEG
         JSEG=JSEG+1
         GLOSEG4(JSEG)= GLOSEGB(ISEG,1)
         GLOSEG4(JSEG+NSEGBLK)= GLOSEGB(ISEG,2)
      ENDDO
!
!     BLOCKS 2 AND 3 (EXTRA-DIAG)
!     ------------------------
!
      DO I=1,NPOIN
         JSEG=JSEG+1
         GLOSEG4(JSEG)= I
         GLOSEG4(JSEG+NSEGBLK)= I+NPOIN
      ENDDO
!
      DO ISEG=1,NSEG
         JSEG=JSEG+1
         GLOSEG4(JSEG)= GLOSEGB(ISEG,1)
         GLOSEG4(JSEG+NSEGBLK)= GLOSEGB(ISEG,2)+ NPOIN
         JSEG=JSEG+1
         GLOSEG4(JSEG)= GLOSEGB(ISEG,2)
         GLOSEG4(JSEG+NSEGBLK)= GLOSEGB(ISEG,1)+ NPOIN
      ENDDO
!
!     BLOCK 4 (EXTRA)
!     --------------
!
      DO ISEG=1,NSEG
         JSEG=JSEG+1
         GLOSEG4(JSEG)         = GLOSEGB(ISEG,1)+NPOIN
         GLOSEG4(JSEG+NSEGBLK) = GLOSEGB(ISEG,2)+NPOIN
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END