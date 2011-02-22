C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       WRITES OUT THE BINARY RESULTS FILE (SELAFIN FORMAT) WITH
!>                THE DATA ON TRAJECTORY OF THE FLOATING BODIES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BINRES, DATE, DEBFLO, FINFLO, FLOPRD, IKLFLO, I_ORIG, J_ORIG, MAXVAR, MESH, NFLOT, NIT, NITFLO, NOMRBI, NRBI, TIME, TITCAS, XFLOT, YFLOT
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink, 
!> @link BIEF_DEF::NPTIR NPTIR@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A, BID32, BIDC, BIDI, DRAPO, IDEB, IELFLO, IFIN, IFLOT, IIT, ISTAT, NELFLO, NVAR, TEXTE
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ECRGEO(), ECRI2()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D()

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
!> </td><td> 08/03/2007
!> </td><td> J-M JANIN (LNH) 30 87 72 84
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>BINRES
!></td><td>--></td><td>TYPE DE BINAIRE DU FICHIER DE RESULTATS
!>    </td></tr>
!>          <tr><td>DATE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DEBFLO
!></td><td>--></td><td>NUMEROS DES PAS DE TEMPS DE LARGAGE DE
!>                  CHAQUE FLOTTEUR.
!>    </td></tr>
!>          <tr><td>FINFLO
!></td><td>--></td><td>NUMEROS DES PAS DE TEMPS DE FIN DE CALCUL DE
!>                  DERIVE POUR CHAQUE FLOTTEUR.
!>    </td></tr>
!>          <tr><td>FLOPRD
!></td><td>--></td><td>NOMBRE DE PAS DE TEMPS ENTRE 2 ENREGITREMENTS
!>                  DES POSITIONS SUCCESSIVES DES FLOTTEURS.
!>    </td></tr>
!>          <tr><td>IKLFLO
!></td><td>---</td><td>TABLE DE CONNECTIVITE BIDON UTILISEE POUR LA
!>                  SORTIE DES TRAJECTOIRES SOUS FORME DE MAILLAGE
!>    </td></tr>
!>          <tr><td>I_ORIG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>J_ORIG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MAXVAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NFLOT
!></td><td>--></td><td>NOMBRE DE FLOTTEURS.
!>    </td></tr>
!>          <tr><td>NIT
!></td><td>--></td><td>NOMBRE DE PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>NITFLO
!></td><td>--></td><td>NOMBRE MAXIMAL D'ENREGISTREMENTS DES
!>                  POSITIONS SUCCESSIVES DES FLOTTEURS.
!>    </td></tr>
!>          <tr><td>NOMRBI
!></td><td>--></td><td>NOM DU FICHIER DE RESULTATS BINAIRE SUP.
!>    </td></tr>
!>          <tr><td>NRBI
!></td><td>--></td><td>FICHIER DE RESULTATS BINAIRE SUPPLEMENTAIRE
!>                  POUR STOCKER LES TRAJECTOIRES DE FLOTTEURS
!>    </td></tr>
!>          <tr><td>TIME
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TITCAS
!></td><td>--></td><td>TITRE DU FICHIER CAS
!>    </td></tr>
!>          <tr><td>XFLOT,YFLOT
!></td><td>--></td><td>POSITIONS SUCCESSIVES DES FLOTTEURS.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE SORFLO
     &(XFLOT,YFLOT,IKLFLO,DEBFLO,FINFLO,NFLOT,NITFLO,FLOPRD,
     & NRBI,TITCAS,BINRES,NOMRBI,NIT,MAXVAR,DATE,TIME,MESH,
     & I_ORIG,J_ORIG)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BINRES         |-->| TYPE DE BINAIRE DU FICHIER DE RESULTATS
C| DATE           |---| 
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
C| MAXVAR         |---| 
C| MESH           |---| 
C| NFLOT          |-->| NOMBRE DE FLOTTEURS.
C| NIT            |-->| NOMBRE DE PAS DE TEMPS
C| NITFLO         |-->| NOMBRE MAXIMAL D'ENREGISTREMENTS DES
C|                |   | POSITIONS SUCCESSIVES DES FLOTTEURS.
C| NOMRBI         |-->| NOM DU FICHIER DE RESULTATS BINAIRE SUP.
C| NRBI           |-->| FICHIER DE RESULTATS BINAIRE SUPPLEMENTAIRE
C|                |   | POUR STOCKER LES TRAJECTOIRES DE FLOTTEURS
C| TIME           |---| 
C| TITCAS         |-->| TITRE DU FICHIER CAS
C| XFLOT,YFLOT    |-->| POSITIONS SUCCESSIVES DES FLOTTEURS.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: I_ORIG,J_ORIG,FLOPRD,NITFLO
      INTEGER, INTENT(IN)             :: NFLOT,NRBI,NIT,MAXVAR
      INTEGER, INTENT(IN)             :: DATE(3),TIME(3)
      INTEGER, INTENT(INOUT)          :: IKLFLO(3*NITFLO*NFLOT)
      INTEGER, INTENT(IN)             :: DEBFLO(NFLOT),FINFLO(NFLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: XFLOT(NITFLO*NFLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: YFLOT(NITFLO*NFLOT)
      CHARACTER(LEN=72), INTENT(IN)   :: TITCAS,NOMRBI
      CHARACTER(LEN=3), INTENT(IN)    :: BINRES
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IIT,NVAR,ISTAT,IDEB,IFIN,IFLOT,NELFLO,IELFLO
C
      INTEGER BIDI(1)
C
      DOUBLE PRECISION A(2)
C
      LOGICAL DRAPO(26)
C
      CHARACTER*32 TEXTE(26),BID32(26)
      CHARACTER*3  BIDC
C
      DATA DRAPO /.TRUE.,25*.FALSE./
      DATA TEXTE(1) /'MAILLAGE                        '/
C
C-----------------------------------------------------------------------
C
      IF(NOMRBI(1:1).NE.' ') THEN
C
         NELFLO= 0
C
         DO 10 IFLOT=1,NFLOT
            IF (DEBFLO(IFLOT).LT.NIT) THEN
               IDEB=(DEBFLO(IFLOT)-1)/FLOPRD + 1
               IFIN=(MIN0(FINFLO(IFLOT),NIT)-1)/FLOPRD + 1
               DO 20 IIT=IDEB,IFIN
                  NELFLO = NELFLO + 1
                  XFLOT (NELFLO) = XFLOT (NITFLO*(IFLOT-1)+IIT)
                  YFLOT (NELFLO) = YFLOT (NITFLO*(IFLOT-1)+IIT)
                  IKLFLO(NELFLO) = NELFLO + 1
20             CONTINUE
               IKLFLO(NELFLO) = NELFLO - 1
            ENDIF
10       CONTINUE
C
         IF(NELFLO.NE.0) THEN
C
            DO 30 IELFLO=1,NELFLO
               IKLFLO(  NELFLO+IELFLO) = IKLFLO(IELFLO)
               IKLFLO(2*NELFLO+IELFLO) = IKLFLO(IELFLO)
               IKLFLO(IELFLO) = IELFLO
30          CONTINUE
C
C           SELAFIN FORMAT
C
            CALL ECRGEO(XFLOT,YFLOT,NELFLO,IKLFLO,
     &                  NRBI,NVAR,TEXTE,BID32,0,TITCAS,DRAPO,26,
     &                  IKLFLO,NELFLO,NELFLO,3,DATE,TIME,
     &                  NCSIZE,NPTIR,MESH%KNOLG%I,
     &                  I3=I_ORIG,I4=J_ORIG)
C
C           WRITES OUT THE TIME STEP
C
            A(1) = 0.D0
            CALL ECRI2(A,BIDI,BIDC,1,'R4',NRBI,BINRES,ISTAT)
C
C           WRITES OUT DUMMY RECORDS IN PLACE OF SIMULATION RESULTS
C           (USES XFLOT BECAUSE NEEDS AN ARRAY OF REALS)
C
            CALL ECRI2 (XFLOT,BIDI,BIDC,NELFLO,'R4',NRBI,BINRES,ISTAT)
C
         ELSE
C
            IF(LNG.EQ.1) WRITE(LU,11) NFLOT
            IF(LNG.EQ.2) WRITE(LU,12) NFLOT
11          FORMAT(1X,'ATTENTION : VOUS AVEZ PREVU',I4,' FLOTTEURS',/,
     &             1X,'MAIS AUCUN N''A ETE LARGUE',/,
     &             1X,'AVANT LE DERNIER PAS DE TEMPS')
12          FORMAT(1X,'ATTENTION : YOU ASKED FOR',I4,' FLOATS',/,
     &             1X,'BUT NONE OF THEM HAS BEEN RELEASED',/,
     &             1X,'SINCE THE LAST TIME STEP')
C
         ENDIF
C
      ELSE
C
         IF(LNG.EQ.1) WRITE(LU,21)
         IF(LNG.EQ.2) WRITE(LU,22)
21       FORMAT(1X,'LE FICHIER DE DERIVES DE FLOTTEURS',/,
     &          1X,'N''A PU ETRE CONSTITUE,',/,
     &          1X,'IL FAUT FOURNIR DANS LE FICHIER DES PARAMETRES',/,
     &          1X,'UN NOM DE FICHIER DE RESULTATS BINAIRE')
22       FORMAT(1X,'THE FILE OF FLOAT DRIFTS COULD NOT BE FIELD,',/,
     &          1X,'YOU SHOULD INCLUDE IN THE STEERING FILE',/,
     &          1X,'A NAME OF BINARY RESULTS FILE')
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C