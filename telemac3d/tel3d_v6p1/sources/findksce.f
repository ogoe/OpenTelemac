C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       FINDS THE CLOSEST GRID POINTS AMONGST THE PLANES
!>                OF THE 3D GRID.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> INFO, ISCE, KSCE, NPLAN, NPOIN2, NSCE, Z, ZSCE
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DISTANCE, I, K, TEMPO
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC3D()

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
!> </td><td> 01/08/2006
!> </td><td> J-M HERVOOUET (LNHE) 01 30 87 80 18
!> </td><td> CORRECTED FOR PARALLELISM
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 13/10/2000
!> </td><td> C. GUILBAUD
!> </td><td> ORIGINAL
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>INFO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ISCE
!></td><td>--></td><td>ADRESSES DES POINTS DANS LE MAILLAGE 2D.
!>    </td></tr>
!>          <tr><td>KSCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS DU MAILLAGE 3D.
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D.
!>    </td></tr>
!>          <tr><td>NSCE
!></td><td>--></td><td>NOMBRE DE POINTS DONNES.
!>    </td></tr>
!>          <tr><td>Z
!></td><td>--></td><td>COORDONNEES DES POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>ZSCE
!></td><td>--></td><td>COORDONNEES DES POINTS DONNES.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FINDKSCE
     &(NPOIN2,NPLAN,Z,NSCE,ISCE,ZSCE,KSCE,INFO)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| INFO           |---| 
C| ISCE           |-->| ADRESSES DES POINTS DANS LE MAILLAGE 2D.
C| KSCE           |---| 
C| NPLAN          |-->| NOMBRE DE PLANS DU MAILLAGE 3D.
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D.
C| NSCE           |-->| NOMBRE DE POINTS DONNES.
C| Z             |-->| COORDONNEES DES POINTS DU MAILLAGE
C| ZSCE           |-->| COORDONNEES DES POINTS DONNES.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NPOIN2,NPLAN,NSCE
      INTEGER, INTENT(IN)    :: ISCE(NSCE)
      INTEGER, INTENT(INOUT) :: KSCE(NSCE)
C
      DOUBLE PRECISION, INTENT(IN) :: Z(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: ZSCE(NSCE)
C
      LOGICAL, INTENT(IN) :: INFO
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K,I
C
      DOUBLE PRECISION DISTANCE,TEMPO
C
      INTRINSIC ABS
C
C-----------------------------------------------------------------------
C
      DO I=1,NSCE
C
        DISTANCE=1.D10
        KSCE(I)=0
C
        IF(ISCE(I).GT.0) THEN
C
          DO K=1,NPLAN
            TEMPO=ABS(ZSCE(I)-Z(ISCE(I),K))
            IF(TEMPO.LT.DISTANCE) THEN
              DISTANCE=TEMPO
              KSCE(I)=K
            ENDIF
          ENDDO
C
C-----------------------------------------------------------------------
C
          IF(INFO.AND.LNG.EQ.1) THEN
            WRITE(LU,*) 'POINT SOURCE ',I,' ASSIMILE AU PLAN ',KSCE(I)
            WRITE(LU,*) 'SITUE A ',DISTANCE,' METRES'
          ENDIF
          IF(INFO.AND.LNG.EQ.2) THEN
            WRITE(LU,*) 'SOURCE POINT ',I,' PUT ON PLANE ',KSCE(I)
            WRITE(LU,*) 'LOCATED AT ',DISTANCE,' METRES'
          ENDIF
C
        ENDIF
C
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C