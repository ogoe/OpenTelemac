C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES THE BOUNDARY CONDITIONS FOR THE DIFFUSION
!>                SOURCE TERM STEP OF THE K-EPSILON MODEL.
!><br>            CASE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  LIKBOR AND LIEBOR ARE BUILT FROM LIUBOR

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> KADH, KENT, KLOG, KSORT, LIEBOF, LIEBOL, LIEBOS, LIKBOF, LIKBOL, LIKBOS, LIUBOF, LIUBOL, LIUBOS, NPLAN, NPOIN2, NPTFR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IPLAN, IPOIN2, IPTFR
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
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>      <tr>
!>      <td><center> 6.0                                       </center>
!> </td><td> 25/11/97
!> </td><td> L. VAN HAREN (LNH) 30 87 84 14
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>KADH
!></td><td>--></td><td>CONVENTION POUR UNE PAROI AVEC ADHERENCE
!>    </td></tr>
!>          <tr><td>KENT
!></td><td>--></td><td>CONVENTION POUR UN POINT A VALEUR IMPOSEE
!>    </td></tr>
!>          <tr><td>KLOG
!></td><td>--></td><td>CONVENTION POUR UNE PAROI LOGARITHMIQUE
!>    </td></tr>
!>          <tr><td>KSORT
!></td><td>--></td><td>CONVENTION POUR UN POINT A VALEUR LIBRE
!>    </td></tr>
!>          <tr><td>LIEBOF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIEBOL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIEBOR
!></td><td><--</td><td>TYPES DE CONDITIONS AUX LIMITES POUR EP
!>    </td></tr>
!>          <tr><td>LIEBOS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIKBOF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIKBOL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIKBOR
!></td><td><--</td><td>TYPES DE CONDITIONS AUX LIMITES POUR AK
!>    </td></tr>
!>          <tr><td>LIKBOS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIUBOF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIUBOL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIUBOR
!></td><td>--></td><td>TYPES DE CONDITIONS AUX LIMITES SUR U
!>    </td></tr>
!>          <tr><td>LIUBOS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS  DU MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERES DU MAILLAGE 2D
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE KEPICL
     & (LIKBOF,LIEBOF,LIUBOF,LIKBOL,LIEBOL,LIUBOL,LIKBOS,LIEBOS,LIUBOS,
     &  NPTFR,NPLAN,NPOIN2,KENT,KSORT,KADH,KLOG)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| KADH           |-->| CONVENTION POUR UNE PAROI AVEC ADHERENCE
C| KENT           |-->| CONVENTION POUR UN POINT A VALEUR IMPOSEE
C| KLOG           |-->| CONVENTION POUR UNE PAROI LOGARITHMIQUE
C| KSORT          |-->| CONVENTION POUR UN POINT A VALEUR LIBRE
C| LIEBOF         |---| 
C| LIEBOL         |---| 
C| LIEBOR         |<--| TYPES DE CONDITIONS AUX LIMITES POUR EP
C| LIEBOS         |---| 
C| LIKBOF         |---| 
C| LIKBOL         |---| 
C| LIKBOR         |<--| TYPES DE CONDITIONS AUX LIMITES POUR AK
C| LIKBOS         |---| 
C| LIUBOF         |---| 
C| LIUBOL         |---| 
C| LIUBOR         |-->| TYPES DE CONDITIONS AUX LIMITES SUR U
C| LIUBOS         |---| 
C| NPLAN          |-->| NOMBRE DE PLANS  DU MAILLAGE 3D
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES DU MAILLAGE 2D
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NPTFR, NPLAN, NPOIN2
      INTEGER, INTENT(IN)    :: KENT, KSORT, KADH, KLOG
      INTEGER, INTENT(IN)    :: LIUBOF(NPOIN2), LIUBOS(NPOIN2)
      INTEGER, INTENT(IN)    :: LIUBOL(NPTFR,NPLAN)
      INTEGER, INTENT(INOUT) :: LIKBOF(NPOIN2), LIKBOS(NPOIN2)
      INTEGER, INTENT(INOUT) :: LIKBOL(NPTFR,NPLAN)
      INTEGER, INTENT(INOUT) :: LIEBOF(NPOIN2), LIEBOS(NPOIN2)
      INTEGER, INTENT(INOUT) :: LIEBOL(NPTFR,NPLAN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IPTFR, IPLAN, IPOIN2
!
!-----------------------------------------------------------------------
!
C     LATERAL BOUNDARIES :
!
!-----------------------------------------------------------------------
!
      DO IPLAN = 1,NPLAN
        DO IPTFR = 1,NPTFR
          IF(LIUBOL(IPTFR,IPLAN).EQ.KENT) THEN
            LIKBOL(IPTFR,IPLAN) = KENT
            LIEBOL(IPTFR,IPLAN) = KENT
          ELSE
            LIKBOL(IPTFR,IPLAN) = KSORT
            LIEBOL(IPTFR,IPLAN) = KSORT
          ENDIF
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
C     BOTTOM
!
!-----------------------------------------------------------------------
!
      DO IPOIN2 = 1,NPOIN2
        IF(LIUBOF(IPOIN2).EQ.KSORT) THEN
          LIKBOF(IPOIN2) = KSORT
          LIEBOF(IPOIN2) = KSORT
        ELSE
          LIKBOF(IPOIN2) = KENT
          LIEBOF(IPOIN2) = KENT
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
C     FREE SURFACE
!
!-----------------------------------------------------------------------
!
      DO IPOIN2 = 1,NPOIN2
        LIKBOS(IPOIN2) = KSORT
        LIEBOS(IPOIN2) = KSORT
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C