C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       VALIDATES THE RESULTS AGAINST AN ANALYTICAL SOLUTION
!>                OR AGAINST RESULTS IN THE COMPUTATION REFERENCE FILE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  THE LAST TIMESTEP ONLY IS COMPARED.

!>  @note  EXCEPT FOR THE BOTTOM, ASSUMES THAT THE REFERENCE
!>         FILE DOES HOLD THE VARIABLES TO BE COMPARED.

!>  @warning  THIS SUBROUTINE MUST BE MODIFIED ACCORDING TO EACH
!>            PARTICULAR CASE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ACOMPARER, IT, MAXIT, MAXTAB, NP, REFFORMAT, RESFORMAT, TEXTREF, TEXTRES, UREF, URES, VARREF, VARRES
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ERMAX, ERR, FINDREF, FINDRES, HIST, I, IERMAX, IREF, IRES, IVAR, TIMEREF, TIMERES
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> BIEF_SUITE(), P_DMAX()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ARTEMIS(), SISYPHE(), TELEMAC2D(), TELEMAC3D(), WAC()

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
!>      <td><center> 6.0                                       </center>
!> </td><td> 05/08/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ACOMPARER
!></td><td>--></td><td>TABLEAU DES VARIABLE A LIRE DANS LE FICHIER
!>                  DES RESULTATS DU CLCUL PRECEDENT
!>    </td></tr>
!>          <tr><td>BINPRE
!></td><td>--></td><td>TYPE DE BINAIRE DU FICHIER DE RESULTATS DU
!>                  CALCUL PRECEDENT
!>    </td></tr>
!>          <tr><td>IT
!></td><td>--></td><td>NUMERO DU PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>MAXIT
!></td><td>--></td><td>NOMBRE MAXIMUM D'ITERATIONS DU PRESENT CALCUL
!>    </td></tr>
!>          <tr><td>MAXTAB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NP
!></td><td>--></td><td>NOMBRE DE POINTS A VERIFIER.
!>    </td></tr>
!>          <tr><td>REFFORMAT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>RESFORMAT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>STDPRE
!></td><td>--></td><td>STANDARD DU FICHIER DE RESULTATS DU CALCUL
!>                  PRECEDENT
!>    </td></tr>
!>          <tr><td>STDRES
!></td><td>--></td><td>STANDARD DU FICHIER DE DESSIN.
!>    </td></tr>
!>          <tr><td>TEXTPR
!></td><td>--></td><td>NOMS DES VARIABLES DU CALCUL PRECEDENT.
!>    </td></tr>
!>          <tr><td>TEXTREF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TEXTRES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UREF
!></td><td>--></td><td>UNITE LOGIQUE DU FICHIER DE REFERENCE
!>    </td></tr>
!>          <tr><td>URES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VARREF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VARRES
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE BIEF_VALIDA
     &(VARREF,TEXTREF,UREF,REFFORMAT,VARRES,TEXTRES,URES,RESFORMAT,
     & MAXTAB,NP,IT,MAXIT,ACOMPARER)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ACOMPARER      |-->| TABLEAU DES VARIABLE A LIRE DANS LE FICHIER
C|                |   | DES RESULTATS DU CLCUL PRECEDENT
C| BINPRE         |-->| TYPE DE BINAIRE DU FICHIER DE RESULTATS DU
C|                |   | CALCUL PRECEDENT
C| IT             |-->| NUMERO DU PAS DE TEMPS
C| MAXIT          |-->| NOMBRE MAXIMUM D'ITERATIONS DU PRESENT CALCUL
C| MAXTAB         |---| 
C| NP             |-->| NOMBRE DE POINTS A VERIFIER.
C| REFFORMAT      |---| 
C| RESFORMAT      |---| 
C| STDPRE         |-->| STANDARD DU FICHIER DE RESULTATS DU CALCUL
C|                |   | PRECEDENT
C| STDRES         |-->| STANDARD DU FICHIER DE DESSIN.
C| TEXTPR         |-->| NOMS DES VARIABLES DU CALCUL PRECEDENT.
C| TEXTREF        |---| 
C| TEXTRES        |---| 
C| UREF           |-->| UNITE LOGIQUE DU FICHIER DE REFERENCE
C| URES           |---| 
C| VARREF         |---| 
C| VARRES         |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF    !, EX_VALIDA => VALIDA
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NP,MAXTAB,IT,MAXIT,URES,UREF
      INTEGER, INTENT(IN) :: ACOMPARER(MAXTAB)
C
      CHARACTER(LEN=32), INTENT(IN) :: TEXTREF(MAXTAB),TEXTRES(MAXTAB)
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: VARREF,VARRES
      CHARACTER(LEN=*), INTENT(IN)  :: REFFORMAT,RESFORMAT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IVAR,I,IREF,IRES,IERMAX
C
      DOUBLE PRECISION TIMEREF,TIMERES,ERMAX,HIST(1),ERR
C
      INTRINSIC MAX
C
      DOUBLE PRECISION P_DMAX
      EXTERNAL         P_DMAX
C
C-----------------------------------------------------------------------
C
      INTEGER FINDREF(500),FINDRES(500)
      IF(MAXTAB.GT.500) THEN
        WRITE(LU,*) 'WRONG SIZE OF FINDREF AND FINDRES IN BIEF_VALIDA'
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(IT.EQ.MAXIT) THEN
C
C  CALLS SUITE TO READ THE REFERENCE FILE
C
      IF(LNG.EQ.1) WRITE(LU,10)
      IF(LNG.EQ.2) WRITE(LU,11)
      CALL BIEF_SUITE(VARREF,VARREF,IREF,UREF,REFFORMAT,HIST,0,NP,
     &                TIMEREF,TEXTREF,TEXTREF,0,FINDREF,ACOMPARER,
     &                .TRUE.,.TRUE.,MAXTAB)
C
C  CALLS SUITE TO READ THE RESULTS FILE
C
      IF(LNG.EQ.1) WRITE(LU,12)
      IF(LNG.EQ.2) WRITE(LU,13)
      CALL BIEF_SUITE(VARRES,VARRES,IRES,URES,RESFORMAT,HIST,0,NP,
     &                TIMERES,TEXTRES,TEXTRES,0,FINDRES,ACOMPARER,
     &                .TRUE.,.TRUE.,MAXTAB)
C
C-----------------------------------------------------------------------
C
      IF(LNG.EQ.1) WRITE(LU,14)
      IF(LNG.EQ.2) WRITE(LU,15)
C
      IF(ABS(TIMERES-TIMEREF).GT.1.D-4) THEN
        IF(LNG.EQ.1) WRITE(LU,16)
        IF(LNG.EQ.2) WRITE(LU,17)
      ENDIF
      IF(IRES.NE.IREF) THEN
        IF(LNG.EQ.1) WRITE(LU,18)
        IF(LNG.EQ.2) WRITE(LU,19)
      ENDIF
C
C-----------------------------------------------------------------------
C
C     LOOP ON THE VARIABLES TO COMPARE
C
      DO IVAR=1,MAXTAB
C
        IF(ACOMPARER(IVAR).EQ.1) THEN
C
C       COMPARES THE VARIABLE IVAR
C
          IF(FINDREF(IVAR).EQ.1.AND.FINDRES(IVAR).EQ.1) THEN
C
            ERMAX = 0.D0
            IERMAX = 1
            DO I = 1 , NP
              ERR=ABS(VARREF%ADR(IVAR)%P%R(I)-VARRES%ADR(IVAR)%P%R(I))
              IF(ERR.GT.ERMAX) THEN
                ERMAX=ERR
                IERMAX=I
              ENDIF
            ENDDO
C
            IF(NCSIZE.GT.1) ERMAX=P_DMAX(ERMAX)
            IF(LNG.EQ.1) WRITE(LU,60) TEXTRES(IVAR)(1:16),ERMAX
            IF(LNG.EQ.2) WRITE(LU,61) TEXTRES(IVAR)(1:16),ERMAX
C
          ELSEIF(FINDREF(IVAR).EQ.1) THEN
C
            IF(LNG.EQ.1) WRITE(LU,70) TEXTRES(IVAR)(1:16)
            IF(LNG.EQ.2) WRITE(LU,71) TEXTRES(IVAR)(1:16)
C
          ENDIF
C
        ENDIF
C
      ENDDO
C
      IF(LNG.EQ.1) WRITE(LU,50)
      IF(LNG.EQ.2) WRITE(LU,51)
C
      ENDIF
C
C-----------------------------------------------------------------------
C
10    FORMAT(1X,////,1X,80('='),/,
     &       25X,' PROCEDURE DE VALIDATION ',/,
     &       1X,80('-'),//,
     &       1X,' 1) RELECTURE DU FICHIER DE REFERENCE :',/,
     &       1X,' --------------------------------------',/)
11    FORMAT(1X,////,1X,80('='),/,
     &       25X,' VALIDATION PROCEDURE ',/,
     &       1X,80('-'),//,
     &       1X,' 1) READING THE REFERENCE FILE :',/,
     &       1X,' ------------------------------',/)
12    FORMAT(1X,///,
     &       1X,' 2) RELECTURE DU FICHIER DE RESULTATS :',/,
     &       1X,' --------------------------------------',/)
13    FORMAT(1X,///,
     &       1X,' 2) READING THE RESULTS FILE :',/,
     &       1X,' --------------------------------',/)
14    FORMAT(1X,///,
     &       1X,' 3) COMPARAISON :',/,
     &       1X,' ----------------',/)
15    FORMAT(1X,///,
     &       1X,' 3) COMPARISON:',/,
     &       1X,' --------------',/)
16    FORMAT(1X,///,
     &       1X,' ATTENTION : TEMPS DIFFERENTS',/,
     &       1X,' ----------------------------',/)
17    FORMAT(1X,///,
     &       1X,' BEWARE: TIMES ARE DIFFERENT',/,
     &       1X,' ---------------------------',/)
18    FORMAT(1X,///,
     &       1X,' ATTENTION : NUMEROS D''ENREGISTREMENT DIFFERENTS',/,
     &       1X,' ------------------------------------------------',/)
19    FORMAT(1X,///,
     &       1X,' BEWARE: RECORD NUMBERS ARE DIFFERENT',/,
     &       1X,' ------------------------------------',/)
C
50    FORMAT(1X,80('-'),/,23X,'FIN DU COMPTE-RENDU DE VALIDATION',/,
     &       1X,80('='),////)
51    FORMAT(1X,80('-'),/,23X,'END OF VALIDATION REPORT',/,
     &       1X,80('='),////)
C
60    FORMAT(1X,'VARIABLE : ',A16,'  DIFFERENCE : ',G16.7,/)
61    FORMAT(1X,'VARIABLE: ' ,A16,'  DIFFERENCE: ',G16.7,/)
C
70    FORMAT(1X,'VARIABLE : ',A16,'  NON TROUVEE',/)
71    FORMAT(1X,'VARIABLE: ' ,A16,'  NOT FOUND'  ,/)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
