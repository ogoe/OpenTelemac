C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       WRITES THE BINARY RESULTS FILE TO SELAFIN FORMAT
!>                WITH INFORMATION ON THE TRAJECTORIES OF THE
!>                FLOATS.

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
!> </td><td> 26/08/99
!> </td><td> JMH
!> </td><td> CALL TO FMTSEL REPLACED BY ECRGEO (NOT TESTED)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>      <tr>
!>      <td><center> 5.1                                       </center>
!> </td><td> 27/11/92
!> </td><td> J-M JANIN (LNH) 30 87 72 84
!> </td><td>
!> </td></tr>
!>  </table>

C
C#######################################################################
C
                        SUBROUTINE SFLO3D
     &(XFLOT,YFLOT,ZFLOT,IKLFLO,TRAFLO,DEBFLO,FINFLO,NFLOT,NITFLO,
     & FLOPRD,NREBI,LISTIN,TITCAS,BINRES,NOMRBI,NIT,
     & I_ORIG,J_ORIG)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BINRES         |-->| TYPE DE BINAIRE DU FICHIER DE RESULTATS
C| DEBFLO         |-->| NUMEROS DES PAS DE TEMPS DE LARGAGE DE
C|                |   | CHAQUE FLOTTEUR.
C| FINFLO         |-->| NUMEROS DES PAS DE TEMPS DE FIN DE CALCUL DE
C|                |   | DERIVE POUR CHAQUE FLOTTEUR.
C| FLOPRD         |-->| NOMBRE DE PAS DE TEMPS ENTRE 2 ENREGITREMENTS
C|                |   | DES POSITIONS SUCCESSIVES DES FLOTTEURS.
C| IKLFLO         |---| TABLE DE CONNECTIVITE BIDON UTILISEE POUR LA
C|                |   | SORTIE DES TRAJECTOIRES SOUS FORME DE MAILLAGE
C| I_ORIG         |---| 
C| J_ORIG         |---| 
C| LISTIN         |-->| SORTIES SUR LISTING OU NON
C| NFLOT          |-->| NOMBRE DE FLOTTEURS.
C| NIT            |-->| NOMBRE DE PAS DE TEMPS
C| NITFLO         |-->| NOMBRE MAXIMAL D'ENREGISTREMENTS DES
C|                |   | POSITIONS SUCCESSIVES DES FLOTTEURS.
C| NOMRBI         |-->| NOM DU FICHIER DE RESULTATS BINAIRE SUP.
C| NREBI          |-->| FICHIER DE RESULTATS BINAIRE SUPPLEMENTAIRE
C|                |   | POUR STOCKER LES TRAJECTOIRES DE FLOTTEURS
C| TITCAS         |-->| TITRE DU FICHIER CAS
C| TRAFLO         |---| TABLEAU DE TRAVAIL UTILISE DANS FMTSEL
C| XFLOT,YFLOT    |-->| POSITIONS SUCCESSIVES DES FLOTTEURS.
C| ZFLOT          |---| 
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
      INTEGER, INTENT(IN) :: NFLOT,NIT,NITFLO,NREBI,FLOPRD
      INTEGER, INTENT(IN) :: I_ORIG,J_ORIG
!
      INTEGER, INTENT(INOUT) :: IKLFLO(3*NITFLO*NFLOT)
      INTEGER, INTENT(INOUT) :: TRAFLO(3*NITFLO*NFLOT)
      INTEGER, INTENT(INOUT) :: DEBFLO(NFLOT), FINFLO(NFLOT)
!
      DOUBLE PRECISION, INTENT(INOUT) :: XFLOT(NITFLO*NFLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: YFLOT(NITFLO*NFLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: ZFLOT(NITFLO*NFLOT)
!
      CHARACTER(LEN=72), INTENT(IN) :: TITCAS, NOMRBI
      CHARACTER(LEN=3),  INTENT(IN) :: BINRES
      LOGICAL, INTENT(IN) :: LISTIN
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CHARACTER(LEN=32) TEXTE(26)
      LOGICAL DRAPO(26), DEPAR, SUIT, ECRI
!
      INTEGER IIT,NVAR,ISTAT,BIDI(1),DATE(3),TIME(3)
      INTEGER IDEB,IFIN,IFLOT,NELFLO,IELFLO
      DOUBLE PRECISION A(2)
      CHARACTER(LEN=32) BIDC2,BIDC(1)
!
      DATA DRAPO /.TRUE.,25*.FALSE./
      DATA TEXTE(1) /'COTE_RELATIVE                   '/
!
!-----------------------------------------------------------------------
!
      IF(NOMRBI(1:1).NE.' ') THEN
!
         DEPAR = .FALSE.
         SUIT  = .FALSE.
         ECRI  = .TRUE.
         NELFLO= 0
!
         DO IFLOT=1,NFLOT
           IF(DEBFLO(IFLOT).LT.NIT) THEN
             IDEB=(DEBFLO(IFLOT)-1)/FLOPRD + 1
             IFIN=(MIN0(FINFLO(IFLOT),NIT)-1)/FLOPRD + 1
             DO IIT=IDEB,IFIN
               NELFLO = NELFLO + 1
               XFLOT (NELFLO) = XFLOT (NITFLO*(IFLOT-1)+IIT)
               YFLOT (NELFLO) = YFLOT (NITFLO*(IFLOT-1)+IIT)
               ZFLOT (NELFLO) = ZFLOT (NITFLO*(IFLOT-1)+IIT)
               IKLFLO(NELFLO) = NELFLO + 1
             ENDDO
             IKLFLO(NELFLO) = NELFLO - 1
           ENDIF
         ENDDO
!
         IF(NELFLO.NE.0) THEN
!
            DO IELFLO=1,NELFLO
               IKLFLO(  NELFLO+IELFLO) = IKLFLO(IELFLO)
               IKLFLO(2*NELFLO+IELFLO) = IKLFLO(IELFLO)
               IKLFLO(IELFLO) = IELFLO
            ENDDO
!
C  STANDARD SELAFIN
!
            DATE(1) = 0
            DATE(2) = 0
            DATE(3) = 0
            TIME(1) = 0
            TIME(2) = 0
            TIME(3) = 0
C
C   NOTE JMH: THERE IS CONFUSION BETWEEN NBOR AND IKLE, BOTH GIVEN AS
C   IKLFLO. NBOR IS ACTUALLY ONLY USED TO BUILD IPOBO FOR SELAFIN FORMAT,
C   NOT USED BY RUBENS. IE MUST WORK BY LUCK.
C   AND NELFLO,NELFLO REPRESENTS NELEM,NPTFR !!!
            CALL ECRGEO(XFLOT,YFLOT,NELFLO,IKLFLO,
     &                  NREBI,NVAR,TEXTE,BIDC,0,
     &                  TITCAS,DRAPO,26,IKLFLO,NELFLO,
     &                  NELFLO,3,DATE,TIME,
     &                  0     ,0     ,BIDI,
C    *                  NCSIZE,NPTIR,KNOLG)  PARALLELISM NOT IMPLEMENTED
     &                  I3=I_ORIG,I4=J_ORIG)
!
C WRITES THE TIME
!
            A(1) = 0.D0
            CALL ECRI2(A,BIDI,BIDC2,1,'R4',NREBI,BINRES,ISTAT)
!
C WRITES THE ELEVATION
!
            CALL ECRI2(ZFLOT,BIDI,BIDC2,NELFLO,'R4',NREBI,BINRES,ISTAT)
!
         ELSE
!
            IF (LNG.EQ.1) WRITE(LU,11) NFLOT
            IF (LNG.EQ.2) WRITE(LU,12) NFLOT
!
         ENDIF
!
      ELSE
!
         IF (LNG.EQ.1) WRITE(LU,21)
         IF (LNG.EQ.2) WRITE(LU,22)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
11    FORMAT(' ATTENTION : VOUS AVEZ PREVU',I4,' FLOTTEURS',/,
     &       ' MAIS AUCUN N''A ETE LARGUE',/,
     &       ' AVANT LE DERNIER PAS DE TEMPS')
12    FORMAT(' ATTENTION : YOU ASKED FOR',I4,' FLOATS',/,
     &       ' BUT NONE OF THEM HAS BEEN RELEASED',/,
     &       ' SINCE THE LAST TIME STEP')
!
21    FORMAT(' LE FICHIER DE DERIVES DE FLOTTEURS N''A PU ETRE CREE',/,
     &       ' IL FAUT FOURNIR DANS LE FICHIER DES PARAMETRES',/,
     &       ' UN NOM DE FICHIER DE RESULTATS BINAIRE')
22    FORMAT(' THE FILE OF FLOAT DRIFTS COULD NOT BE FIELD,',/,
     &       ' YOU SHOULD INCLUDE IN THE STEERING FILE',/,
     &       ' A NAME OF BINARY RESULTS FILE')
!
!-----------------------------------------------------------------------
!
      RETURN
      END

C
C#######################################################################
C
