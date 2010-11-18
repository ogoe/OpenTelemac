C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS THE SEGMENTS OF THE MATRIX IN A SINGLE BLOCK.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> GLOSEG4, GLOSEGB, NPBLK, NPOIN, NSEG, NSEGBLK
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, ISEG, JSEG
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SD_STRSG4
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SD_SOLVE_4()

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
!>      <td><center> 5.7                                       </center>
!> </td><td> 20/11/06
!> </td><td> E. RAZAFINDRAKOTO (LNH) 01 30 87 74 03
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>GLOSEG4
!></td><td><--</td><td>IF, YES INFORMATIONS ON LISTING
!>    </td></tr>
!>          <tr><td>GLOSEGB
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DES SEGMENTS
!>    </td></tr>
!>          <tr><td>NPBLK
!></td><td>--></td><td>COMME NPOIN MAIS POUR LE BLOC
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE D'INCONNUES D'UNE MATRICE DU BLOC
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td>--></td><td>NOMBRE DE SEGMENTS
!>    </td></tr>
!>          <tr><td>NSEGBLK
!></td><td>--></td><td>COMME NSEG MAIS POUR LE BLOC
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                          SUBROUTINE SD_STRSG4
     &(NPOIN,NSEG,GLOSEGB,NPBLK,NSEGBLK,GLOSEG4)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| GLOSEG4        |<--| IF, YES INFORMATIONS ON LISTING
C| GLOSEGB        |-->| NUMEROS GLOBAUX DES POINTS DES SEGMENTS
C| NPBLK          |-->| COMME NPOIN MAIS POUR LE BLOC
C| NPOIN          |-->| NOMBRE D'INCONNUES D'UNE MATRICE DU BLOC
C| NSEG           |-->| NOMBRE DE SEGMENTS
C| NSEGBLK        |-->| COMME NSEG MAIS POUR LE BLOC
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_SD_STRSG4 => SD_STRSG4
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NSEGBLK,NPBLK,NSEG,NPOIN
      INTEGER, INTENT(IN)    :: GLOSEGB(NSEG,2)
      INTEGER, INTENT(INOUT) :: GLOSEG4(2*NSEGBLK)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,ISEG,JSEG
C
C----------------------------------------------
C     INFO :      NPBLK = NPOIN*NBLOC
C                 NSEGBLK=NSEG*4 + 2*NPOIN
C----------------------------------------------
C
C     MATRIX ASSEMBLES TOTAL BLOCKS:
C
C----------------------------------------------
C
      JSEG=0
C
C     BLOCK 1
C     ------
C
      DO ISEG=1,NSEG
         JSEG=JSEG+1
         GLOSEG4(JSEG)= GLOSEGB(ISEG,1)
         GLOSEG4(JSEG+NSEGBLK)= GLOSEGB(ISEG,2)
      ENDDO
C
C     BLOCKS 2 AND 3 (EXTRA-DIAG)
C     ------------------------
C
      DO I=1,NPOIN
         JSEG=JSEG+1
         GLOSEG4(JSEG)= I
         GLOSEG4(JSEG+NSEGBLK)= I+NPOIN
      ENDDO
C
      DO ISEG=1,NSEG
         JSEG=JSEG+1
         GLOSEG4(JSEG)= GLOSEGB(ISEG,1)
         GLOSEG4(JSEG+NSEGBLK)= GLOSEGB(ISEG,2)+ NPOIN
         JSEG=JSEG+1
         GLOSEG4(JSEG)= GLOSEGB(ISEG,2)
         GLOSEG4(JSEG+NSEGBLK)= GLOSEGB(ISEG,1)+ NPOIN
      ENDDO
C
C     BLOCK 4 (EXTRA)
C     --------------
C
      DO ISEG=1,NSEG
         JSEG=JSEG+1
         GLOSEG4(JSEG)         = GLOSEGB(ISEG,1)+NPOIN
         GLOSEG4(JSEG+NSEGBLK) = GLOSEGB(ISEG,2)+NPOIN
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C