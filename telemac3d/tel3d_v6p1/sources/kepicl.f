

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES THE BOUNDARY CONDITIONS FOR THE DIFFUSION
!>                SOURCE TERM STEP OF THE K-EPSILON MODEL.
!><br>            CASE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  LIKBOR AND LIEBOR ARE BUILT FROM LIUBOR

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
      INTEGER, INTENT(IN)    :: LIUBOL(NPTFR*NPLAN*2)
      INTEGER, INTENT(INOUT) :: LIKBOF(NPOIN2), LIKBOS(NPOIN2)
      INTEGER, INTENT(INOUT) :: LIKBOL(NPTFR*NPLAN*2)
      INTEGER, INTENT(INOUT) :: LIEBOF(NPOIN2), LIEBOS(NPOIN2)
      INTEGER, INTENT(INOUT) :: LIEBOL(NPTFR*NPLAN*2)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IPTFR, IPLAN, IPOIN2,NPTFR3
!
!-----------------------------------------------------------------------
!
!     LATERAL BOUNDARIES :
!
!-----------------------------------------------------------------------
!
      NPTFR3=NPLAN*NPTFR
!
      DO IPTFR=1,NPTFR3
        IF(LIUBOL(IPTFR).EQ.KENT) THEN
          LIKBOL(IPTFR) = KENT
          LIEBOL(IPTFR) = KENT
        ELSE
          LIKBOL(IPTFR) = KSORT
          LIEBOL(IPTFR) = KSORT
        ENDIF
!       SAVING VALUES IN THE SECOND DIMENSION (SEE POINT_TELEMAC3D)
        LIKBOL(IPTFR+NPTFR3) = KSORT
        LIEBOL(IPTFR+NPTFR3) = KSORT       
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
