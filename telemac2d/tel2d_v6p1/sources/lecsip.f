C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       READS THE DATA FOR SIPHONS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ALTSCE, ANGSCE, CESCE, CSSCE, DELSCE, ENTSIP, IFIC, LSCE, MAXSCE, NSIPH, RELAXS, SECSCE, SORSIP
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ALT1, ALT2, ANG1, ANG2, CE1, CE2, CS1, CS2, DELTA1, DELTA2, L12, N, PI, S12
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
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
!>      <td><center> 5.2                                       </center>
!> </td><td> 03/10/1996
!> </td><td> J.-M. HERVOUET (LNH) 30 87 80 18
!> </td><td> MODIFIED
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 19/04/1996
!> </td><td> V. GUINOT   (LHF)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ALTSCE
!></td><td><--</td><td>COTE DES ENTREES ET SORTIES DE BUSES
!>    </td></tr>
!>          <tr><td>ANGSCE
!></td><td><--</td><td>ANGLE DES BUSES AVEC L'AXE OX.
!>    </td></tr>
!>          <tr><td>CESCE
!></td><td><--</td><td>COEFFICIENTS DE PERTE DE CHARGE
!>                  LORS D'UN FONCTIONNEMENT EN ENTREE.
!>    </td></tr>
!>          <tr><td>CSSCE
!></td><td><--</td><td>COEFFICIENTS DE PERTE DE CHARGE
!>                  LORS D'UN FONCTIONNEMENT EN SORTIE.
!>    </td></tr>
!>          <tr><td>DELSCE
!></td><td><--</td><td>ANGLE DES BUSES AVEC LA VERTICALE
!>    </td></tr>
!>          <tr><td>ENTSIP
!></td><td><--</td><td>INDICES DANS LA NUMEROTATION DES SOURCES
!>    </td></tr>
!>          <tr><td>IFIC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LSCE
!></td><td><--</td><td>PERTE DE CHARGE LINEAIRE DE LA CONDUITE.
!>    </td></tr>
!>          <tr><td>MAXSCE
!></td><td>--></td><td>NOMBRE MAXIMUM DE POINTS SOURCES.
!>    </td></tr>
!>          <tr><td>NPSIPH
!></td><td><--</td><td>NOMBRE DE SIPHONS.
!>    </td></tr>
!>          <tr><td>NSIPH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>RELAXS
!></td><td><--</td><td>COEFFICIENT DE RELAXATION.
!>    </td></tr>
!>          <tr><td>SECSCE
!></td><td><--</td><td>SECTION DES SIPHONS (NUMEROTATION DES SOURCES)
!>    </td></tr>
!>          <tr><td>SORSIP
!></td><td><--</td><td>INDICES DANS LA NUMEROTATION DES SOURCES
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE LECSIP
     & (RELAXS,NSIPH,ENTSIP,SORSIP,SECSCE,
     &  ALTSCE,CSSCE,CESCE,DELSCE,ANGSCE,LSCE,MAXSCE,IFIC)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ALTSCE         |<--| COTE DES ENTREES ET SORTIES DE BUSES
C| ANGSCE         |<--| ANGLE DES BUSES AVEC L'AXE OX.
C| CESCE          |<--| COEFFICIENTS DE PERTE DE CHARGE
C|                |   | LORS D'UN FONCTIONNEMENT EN ENTREE.
C| CSSCE          |<--| COEFFICIENTS DE PERTE DE CHARGE
C|                |   | LORS D'UN FONCTIONNEMENT EN SORTIE.
C| DELSCE         |<--| ANGLE DES BUSES AVEC LA VERTICALE
C| ENTSIP         |<--| INDICES DANS LA NUMEROTATION DES SOURCES
C| IFIC           |---| 
C| LSCE           |<--| PERTE DE CHARGE LINEAIRE DE LA CONDUITE.
C| MAXSCE         |-->| NOMBRE MAXIMUM DE POINTS SOURCES.
C| NPSIPH         |<--| NOMBRE DE SIPHONS.
C| NSIPH          |---| 
C| RELAXS         |<--| COEFFICIENT DE RELAXATION.
C| SECSCE         |<--| SECTION DES SIPHONS (NUMEROTATION DES SOURCES)
C| SORSIP         |<--| INDICES DANS LA NUMEROTATION DES SOURCES
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: MAXSCE,IFIC
      INTEGER, INTENT(INOUT) :: ENTSIP(*),SORSIP(*),NSIPH
      DOUBLE PRECISION, INTENT(INOUT) :: RELAXS
      DOUBLE PRECISION, INTENT(INOUT) :: SECSCE(*),ALTSCE(*),CSSCE(*)
      DOUBLE PRECISION, INTENT(INOUT) :: DELSCE(*),ANGSCE(*)
      DOUBLE PRECISION, INTENT(INOUT) :: CESCE(*),LSCE(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER N
C
      DOUBLE PRECISION DELTA1,DELTA2,S12,ALT1,ALT2
      DOUBLE PRECISION ANG1,ANG2,CS1,CS2,CE1,CE2,L12
C
      DOUBLE PRECISION PI
      PARAMETER(PI=3.141592653589D0)
C
C-----------------------------------------------------------------------
C
      READ(IFIC,*,END=900)
      READ(IFIC,*,ERR=998) RELAXS,N
      READ(IFIC,*,END=900)
C
C     CHECKS SIZES
C
      IF(N.GT.MAXSCE/2) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LECSIP : NOMBRE DE SIPHONS : ',N
          WRITE(LU,*) '         TROP GRAND'
          WRITE(LU,*) '         LE MAXIMUM EST :',MAXSCE/2
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*) 'LECSIP : NUMBER OF CULVERTS:',N
          WRITE(LU,*) '         EXCEEDIND THE MAXIMUM OF: ',MAXSCE/2
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
C
C     COHERENCE WITH THE STEERING FILE
C
      IF(N.NE.NSIPH) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LECSIP : NOMBRE DE SIPHONS : ',N
          WRITE(LU,*) '         DIFFERENT DE LA VALEUR DONNEE DANS LE'
          WRITE(LU,*) '         FICHIER DES PARAMETRES :',NSIPH
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*) 'LECSIP : NUMBER OF CULVERTS:',N
          WRITE(LU,*) '         DIFFERENT FROM THE ONE GIVEN IN THE'
          WRITE(LU,*) '         PARAMETER FILE: ',NSIPH
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
C
      DO 10 N=1,NSIPH
        READ(IFIC,*,ERR=997) ENTSIP(N),SORSIP(N),DELTA1,DELTA2,
     &                       CE1,CE2,CS1,CS2,S12,L12,ALT1,ALT2,ANG1,ANG2
        DELSCE(ENTSIP(N)) = DELTA1*PI/180.D0
        DELSCE(SORSIP(N)) = DELTA2*PI/180.D0
        CESCE(ENTSIP(N))  = CE1
        CESCE(SORSIP(N))  = CE2
        CSSCE(ENTSIP(N))  = CS1
        CSSCE(SORSIP(N))  = CS2
        SECSCE(ENTSIP(N)) = S12
        SECSCE(SORSIP(N)) = S12
        LSCE(ENTSIP(N))   = L12
        LSCE(SORSIP(N))   = L12
        ALTSCE(ENTSIP(N)) = ALT1
        ALTSCE(SORSIP(N)) = ALT2
        ANGSCE(ENTSIP(N)) = ANG1*PI/180.D0
        ANGSCE(SORSIP(N)) = ANG2*PI/180.D0
10    CONTINUE
C
      GO TO 1000
C
C-----------------------------------------------------------------------
C     ERROR MESSAGES
C-----------------------------------------------------------------------
C
998   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSIP : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES FORMATE 1'
        WRITE(LU,*) '         2EME LIGNE DU FICHIER NON CONFORME.'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSIP : READ ERROR ON THE'
        WRITE(LU,*) '         FORMATTED DATA FILE 1'
        WRITE(LU,*) '         AT LINE 2'
      ENDIF
      GO TO 2000
C
997   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSIP : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES FORMATE 1'
        WRITE(LU,*) '         POUR LE SIPHON ',N
        WRITE(LU,*) '         DONNEES ILLISIBLES'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSIP : READ ERROR ON THE'
        WRITE(LU,*) '         FORMATTED DATA FILE 1'
        WRITE(LU,*) '         FOR CULVERT NUMBER ',N
        WRITE(LU,*) '         THE DATA CANNOT BE READ'
      ENDIF
      GO TO 2000
C
900   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSIP : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES FORMATE 1'
        WRITE(LU,*) '         FIN DE FICHIER PREMATUREE'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSIP : READ ERROR ON THE'
        WRITE(LU,*) '         FORMATTED DATA FILE 1'
        WRITE(LU,*) '         UNEXPECTED END OF FILE'
      ENDIF
C
2000  CONTINUE
C
      NSIPH = 0
C
1000  CONTINUE
C
      IF(NSIPH.EQ.0) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*)
          WRITE(LU,*)'LECSIP : ERREUR DE LECTURE'
          WRITE(LU,*)'         AUCUN SIPHON NE SERA'
          WRITE(LU,*)'         PRIS EN COMPTE.'
          WRITE(LU,*)
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*)
          WRITE(LU,*)'LECSIP : READ ERROR'
          WRITE(LU,*)'         NO CULVERT WILL BE TAKEN'
          WRITE(LU,*)'         INTO ACCOUNT'
          WRITE(LU,*)
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C