!                    *****************
                     SUBROUTINE ENTETE
!                    *****************
!
     &(IETAPE,AT,LT)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    WRITES ON THE LISTING HEADINGS FOR THE VARIOUS STAGES
!+               OF THE PROGRAM.
!
!history  J-M HERVOUET (LNH)
!+        05/09/2007
!+        V5P8
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
!| AT             |-->| TIME IN SECONDS
!| IETAPE         |-->| FRACTIONAL STEP IN THE ALGORITHM
!| LT             |-->| TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: AT
      INTEGER, INTENT(IN)          :: LT,IETAPE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION S
!
      INTEGER J,H,M
!
      CHARACTER*32 FR(13),GB(13)
!
      INTRINSIC INT
!
!-----------------------------------------------------------------------
!
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
!
!-----------------------------------------------------------------------
!
!  DECOMPOSITION OF TIME IN DAYS, HOURS, MINUTES AND SECONDS
!
      S = AT
      J = INT(AT/86400.D0)
      S = S - 86400.D0 * J
      H = INT(S/3600.D0)
      S = S - 3600.D0 * H
      M = INT(S/60.D0)
      S = S - 60.D0 * M
!
!-----------------------------------------------------------------------
!
!   PRINTS TIME AND ITERATIONS
!
      IF (IETAPE.EQ.1.OR.IETAPE.EQ.2) THEN
!
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
!
!   PRINTS TITLES FOR EACH STAGES
!
      ELSE
!
        IF(LNG.EQ.1) WRITE(LU,200) FR(IETAPE)
        IF(LNG.EQ.2) WRITE(LU,200) GB(IETAPE)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
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
!
!-----------------------------------------------------------------------
!
      RETURN
      END
