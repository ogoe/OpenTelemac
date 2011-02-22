C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief      WRITES ON THE LISTING HEADINGS FOR THE VARIOUS STAGES
!>               OF THE PROGRAM.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, IETAPE, LT
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> FR, GB, H, J, M, S
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BILAN(), TELEMAC2D()

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
!>      <td><center> 5.8                                       </center>
!> </td><td> 05/09/2007
!> </td><td> J-M HERVOUET (LNH) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT , LT
!></td><td>--></td><td>TEMPS , NUMERO DU PAS DE TEMPS.
!>    </td></tr>
!>          <tr><td>IETAPE
!></td><td>--></td><td>INDICATEUR D'AVANCEMENT DANS LE PROGRAMME.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ENTETE
     &(IETAPE,AT,LT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT , LT        |-->| TEMPS , NUMERO DU PAS DE TEMPS.
C| IETAPE         |-->| INDICATEUR D'AVANCEMENT DANS LE PROGRAMME.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, INTENT(IN) :: AT
      INTEGER, INTENT(IN)          :: LT,IETAPE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION S
C
      INTEGER J,H,M
C
      CHARACTER*32 FR(13),GB(13)
C
      INTRINSIC INT
C
C-----------------------------------------------------------------------
C
      DATA FR /     '                                ' ,
     &              '                                ' ,
     &              '     ETAPE DE CONVECTION        ' ,
     &              '       MODELE K-EPSILON         ' ,
     &              'ETAPE DE DIFFUSION DES TRACEURS ' ,
     &              ' ETAPE DE DIFFUSION-PROPAGATION ' ,
     &              '      BILAN DE VOLUME D''EAU     ' ,
     &              ' BILAN FINAL DE VOLUME D''EAU ' ,
     &              '  TEMPS :                       ' ,
     &              ' SECONDES                       ' ,
     &              'ITERATION                       ' ,
     &              '     DERIVE DE FLOTTEUR(S)      ' ,
     &              '   DERIVE(S) LAGRANGIENNE(S)    ' /
      DATA GB /     '                                ' ,
     &              '                                ' ,
     &              '        ADVECTION STEP          ' ,
     &              '        K-EPSILON MODEL         ' ,
     &              '   DIFFUSION OF TRACERS STEP    ' ,
     &              '  DIFFUSION-PROPAGATION STEP    ' ,
     &              '     BALANCE OF WATER VOLUME    ' ,
     &              ' FINAL BALANCE OF WATER VOLUME  ' ,
     &              '    TIME:                       ' ,
     &              ' SECONDS                        ' ,
     &              'ITERATION                       ' ,
     &              '       DRIFT OF DROGUE(S)       ' ,
     &              '      LAGRANGIAN DRIFT(S)       ' /
C
C-----------------------------------------------------------------------
C
C  DECOMPOSITION OF TIME IN DAYS, HOURS, MINUTES AND SECONDS
C
      S = AT
      J = INT(AT/86400.D0)
      S = S - 86400.D0 * J
      H = INT(S/3600.D0)
      S = S - 3600.D0 * H
      M = INT(S/60.D0)
      S = S - 60.D0 * M
C
C-----------------------------------------------------------------------
C
C   PRINTS TIME AND ITERATIONS
C
      IF (IETAPE.EQ.1.OR.IETAPE.EQ.2) THEN
C
        IF(J.NE.0) THEN
          IF(LNG.EQ.1) WRITE(LU,10) FR(11),LT,FR(9),J,H,M,S,AT
          IF(LNG.EQ.2) WRITE(LU,11) GB(11),LT,GB(9),J,H,M,S,AT
        ELSEIF(H.NE.0) THEN
          IF(LNG.EQ.1) WRITE(LU,20) FR(11),LT,FR(9),H,M,S,AT
          IF(LNG.EQ.2) WRITE(LU,20) GB(11),LT,GB(9),H,M,S,AT
        ELSEIF(M.NE.0) THEN
          IF(LNG.EQ.1) WRITE(LU,30) FR(11),LT,FR(9),M,S,AT
          IF(LNG.EQ.2) WRITE(LU,30) GB(11),LT,GB(9),M,S,AT
        ELSE
          IF(LNG.EQ.1) WRITE(LU,40) FR(11),LT,FR(9),S
          IF(LNG.EQ.2) WRITE(LU,40) GB(11),LT,GB(9),S
        ENDIF
C
C   PRINTS TITLES FOR EACH STAGES
C
      ELSE
C
        IF(LNG.EQ.1) WRITE(LU,200) FR(IETAPE)
        IF(LNG.EQ.2) WRITE(LU,200) GB(IETAPE)
C
      ENDIF
C
C-----------------------------------------------------------------------
C
10     FORMAT(/,80('='),/,1X,A10,I8,A10,
     &     1I4,' J ',1I2,' H ',1I2,' MIN ',F8.4,' S',3X,'(',F14.4,' S)')
11     FORMAT(/,80('='),/,1X,A10,I8,A10,
     &     1I4,' D ',1I2,' H ',1I2,' MN ',F8.4,' S',3X,'(',F14.4,' S)')
20     FORMAT(/,80('='),/,1X,A10,I8,A10,1I2,' H ',1I2,' MIN ',F8.4,' S',
     &                                               3X,'(',F14.4,' S)')
30     FORMAT(/,80('='),/,1X,A10,I8,A10,1I2,' MN ',F8.4,' S',
     &                                               3X,'(',F14.4,' S)')
40     FORMAT(/,80('='),/,1X,A10,I8,A10,F8.4,' S')
200    FORMAT(80('-'),/,18X,A32)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C