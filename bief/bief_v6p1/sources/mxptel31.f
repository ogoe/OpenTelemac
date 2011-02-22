C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       GIVES THE MAXIMUM NUMBER OF NEIGHBOURS
!>                FOR A GIVEN MESH NODE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IKLES, MXELVS, MXPTVS, NELEM, NPOIN
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, IKL, IKLJ, IND_ELEM, IPOIN, J, K, NVOIS, VOIS
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MXPTEL31
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>MXPTEL()

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
!>      <td><center> 5.5                                       </center>
!> </td><td> 31/08/2009
!> </td><td> LAM MINH-PHUONG; F. DECUNG
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IKLES
!></td><td>--></td><td>TABLE DE CONNECTIVITE (DU FORMAT SELAFIN)
!>    </td></tr>
!>          <tr><td>MXELVS
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS VOISINS.
!>    </td></tr>
!>          <tr><td>MXPTVS
!></td><td><--</td><td>NOMBRE MAXIMUM DE POINTS VOISINS.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                          SUBROUTINE MXPTEL31
     & (NELEM,NPOIN,MXELVS,IKLES,MXPTVS)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IKLES          |-->| TABLE DE CONNECTIVITE (DU FORMAT SELAFIN)
C| MXELVS         |-->| NOMBRE MAXIMUM D'ELEMENTS VOISINS.
C| MXPTVS         |<--| NOMBRE MAXIMUM DE POINTS VOISINS.
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE.
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_MXPTEL31 => MXPTEL31
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)                       :: NELEM
      INTEGER, INTENT(IN)                       :: NPOIN
      INTEGER, INTENT(IN)                       :: MXELVS
      INTEGER, INTENT(IN), DIMENSION(4,NELEM)   :: IKLES
      INTEGER, INTENT(OUT)                      :: MXPTVS
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER                            :: IPOIN,I,J,K,IKLJ,IKL
      INTEGER                            :: NVOIS
      INTEGER,DIMENSION(:),ALLOCATABLE   :: VOIS
      INTEGER,DIMENSION(:,:),ALLOCATABLE :: IND_ELEM
!
      ALLOCATE(VOIS(3*MXELVS))
      ALLOCATE(IND_ELEM(NPOIN,MXELVS+1))
!
!-----------------------------------------------------------------------
!
C IND_ELEM GIVES THE NUMBER OF ELEMENTS AROUND A NODE AND THEIR NUMBERS
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      DO I = 1, NPOIN
        IND_ELEM(I,1) = 0
      ENDDO
!
      DO J=1, 4
        DO I=1,NELEM
          IKL = IKLES(J,I)
          IND_ELEM(IKL,1)=IND_ELEM(IKL,1)+1
          IND_ELEM(IKL,IND_ELEM(IKL,1)+1)=I
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      MXPTVS = 0
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
C LOOP ON ALL THE NODES OF THE MESH      !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!
      DO IPOIN = 1, NPOIN
!
        NVOIS   = 1
        VOIS(1) = IPOIN
!
C       INITIALISES VOIS TO 0
!
        DO I = 1, 3*MXELVS
          VOIS(I) = 0
        ENDDO
!
C       FILLS IN VOIS, WHICH CONTAINS THE NUMBERS OF ALL THE NODES
C       NEIGHBOURING IPOIN
!
        DO J = 1,4
          DO I = 2, IND_ELEM(IPOIN,1)+1
            IKLJ = IKLES(J,IND_ELEM(IPOIN,I))
            DO K = 1, NVOIS
              IF ( VOIS(K) == IKLJ ) EXIT
            ENDDO
            IF( K > NVOIS ) THEN
              NVOIS       = NVOIS + 1
              VOIS(NVOIS) = IKLJ
            ENDIF
          ENDDO
        ENDDO
!
        NVOIS = NVOIS - 1
        IF( MXPTVS < NVOIS) MXPTVS = NVOIS
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      DEALLOCATE (VOIS)
      DEALLOCATE (IND_ELEM)
!
      RETURN
      END
C
C#######################################################################
C