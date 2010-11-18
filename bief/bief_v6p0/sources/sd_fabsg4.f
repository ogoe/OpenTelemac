C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS THE MATRIX IN A SINGLE BLOCK.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DA, DAB1, DAB2, DAB3, DAB4, NPBLK, NPOIN, NSEG, NSEGBLK, XA, XAB1, XAB2, XAB3, XAB4
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, ISEG, JSEG
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SD_FABSG4
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
!>          <tr><td>CVB1,CVB2
!></td><td>--></td><td>SECONDS MEMBRES
!>    </td></tr>
!>          <tr><td>DA,XA
!></td><td>--></td><td>DIAGONALES ET TERMES EXTRA-DIAGONAUX DES
!>                  MATRICES
!>    </td></tr>
!>          <tr><td>DAB1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DAB2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DAB3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DAB4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>INFOGR
!></td><td>--></td><td>IF, YES INFORMATIONS ON LISTING
!>    </td></tr>
!>          <tr><td>NPBLK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE D'INCONNUES D'UNE MATRICE DU BLOC
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td>--></td><td>NOMBRE DE SEGMENTS
!>    </td></tr>
!>          <tr><td>NSEGBLK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XAB1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XAB2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XAB3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XAB4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XX1,XX2
!></td><td><--</td><td>SOLUTIONS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                          SUBROUTINE SD_FABSG4
     & (NPOIN,NSEG,DAB1,DAB2,DAB3,DAB4,XAB1,XAB2,XAB3,XAB4,
     &  NPBLK,NSEGBLK,DA,XA)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CVB1,CVB2      |-->| SECONDS MEMBRES
C| DA,XA          |-->| DIAGONALES ET TERMES EXTRA-DIAGONAUX DES
C|                |   | MATRICES
C| DAB1           |---| 
C| DAB2           |---| 
C| DAB3           |---| 
C| DAB4           |---| 
C| INFOGR         |-->| IF, YES INFORMATIONS ON LISTING
C| NPBLK          |---| 
C| NPOIN          |-->| NOMBRE D'INCONNUES D'UNE MATRICE DU BLOC
C| NSEG           |-->| NOMBRE DE SEGMENTS
C| NSEGBLK        |---| 
C| XAB1           |---| 
C| XAB2           |---| 
C| XAB3           |---| 
C| XAB4           |---| 
C| XX1,XX2        |<--| SOLUTIONS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_SD_FABSG4 => SD_FABSG4
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NSEGBLK,NPBLK,NSEG,NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: XAB1(NSEG),XAB2(NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: XAB3(NSEG),XAB4(NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: DAB1(NPOIN),DAB2(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: DAB3(NPOIN),DAB4(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: XA(2*NSEGBLK),DA(NPBLK)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,ISEG,JSEG
C
C----------------------------------------------
C     INFO :      NPBLK   = NPOIN*NBLOC
C                 NSEGBLK = NSEG*4 + 2*NPOIN
C----------------------------------------------
C
C
C
C-------------------
C 1.  BLOCK DIAGONAL
C-------------------
C
      DO I=1,NPOIN
         DA(I) = DAB1(I)
         DA(I+NPOIN) = DAB4(I)
      ENDDO
C
C---------------------------
C 2.   EXTRADIAGONAL TERMS
C---------------------------
C
C
C     BLOCK 1
C     ------
C
      JSEG=0
      DO ISEG=1,NSEG
         JSEG=JSEG+1
         XA(JSEG)        =XAB1(ISEG)
         XA(JSEG+NSEGBLK)=XAB1(ISEG)
      ENDDO
C
C     BLOCKS 2 AND 3 (EXTRA-DIAG)
C     ------------------------
      DO I=1,NPOIN
         JSEG=JSEG+1
         XA(JSEG)        =DAB2(I)
         XA(JSEG+NSEGBLK)=DAB3(I)
      ENDDO
C
      DO ISEG=1,NSEG
         JSEG=JSEG+1
         XA(JSEG)        =XAB2(ISEG)
         XA(JSEG+NSEGBLK)=XAB3(ISEG)
         JSEG=JSEG+1
         XA(JSEG)        =XAB2(ISEG)
         XA(JSEG+NSEGBLK)=XAB3(ISEG)
      ENDDO
C
C     BLOCK 4 (EXTRA)
C     --------------
      DO ISEG=1,NSEG
         JSEG=JSEG+1
         XA(JSEG)        =XAB4(ISEG)
         XA(JSEG+NSEGBLK)=XAB4(ISEG)
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C