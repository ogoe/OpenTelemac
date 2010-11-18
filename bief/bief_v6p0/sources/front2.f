C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       IDENTIFIES AND NUMBERS THE LIQUID AND SOLID BOUNDARIES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  SOLID BOUNDARIES ARE INDICATED WITH LIHBOR(K) = KLOG
!>         FOR A BOUNDARY NODE NUMBER K.
!>         A SEGMENT CONNECTING A LIQUID AND A SOLID NODE IS
!>         CONSIDERED TO BE SOLID.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DEBLIQ, DEBSOL, DEJAVU, FINLIQ, FINSOL, KLOG, KP1BOR, LIHBOR, LISTIN, LIUBOR, MAXFRO, NBOR, NFRLIQ, NFRSOL, NPOIN, NPTFR, NUMLIQ, X, Y
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> EPS, IDEP, K, KPREV, L1, L2, L3, LIQ1, LIQD, LIQF, MAXNS, MINNS, NILE, NS, SOL1, SOLD, SOLF, YMIN
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ARTEMIS(), SISYPHE(), TELEMAC2D(), TELEMAC3D()

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
!>      <td><center> 5.6                                       </center>
!> </td><td> 27/02/04
!> </td><td> J-M HERVOUET  30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DEBLIQ
!></td><td><--</td><td>DEBUTS DES FRONTIERES LIQUIDES
!>    </td></tr>
!>          <tr><td>DEBSOL
!></td><td><--</td><td>DEBUTS DES FRONTIERES SOLIDES
!>    </td></tr>
!>          <tr><td>DEJAVU
!></td><td>---</td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>          <tr><td>FINLIQ
!></td><td><--</td><td>FINS DES FRONTIERES LIQUIDES
!>    </td></tr>
!>          <tr><td>FINSOL
!></td><td><--</td><td>FINS DES FRONTIERES SOLIDES
!>    </td></tr>
!>          <tr><td>KLOG
!></td><td>--></td><td>LIHBOR(K)=KLOG : FRONTIERE SOLIDE
!>    </td></tr>
!>          <tr><td>KP1BOR
!></td><td>--></td><td>NUMEROS DES EXTREMITES DES SEGMENTS DE BORD
!>                  DANS LA NUMEROTATION DES POINTS DE BORD
!>    </td></tr>
!>          <tr><td>LIHBOR
!></td><td>--></td><td>CONDITIONS AUX LIMITES SUR H
!>    </td></tr>
!>          <tr><td>LISTIN
!></td><td>--></td><td>IMPRESSIONS SUR LISTING (OU NON)
!>    </td></tr>
!>          <tr><td>LIUBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MAXFRO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DE BORD
!>    </td></tr>
!>          <tr><td>NFRLIQ
!></td><td><--</td><td>NOMBRE DE FRONTIERES LIQUIDES
!>    </td></tr>
!>          <tr><td>NFRSOL
!></td><td><--</td><td>NOMBRE DE FRONTIERES SOLIDES
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERE
!>    </td></tr>
!>          <tr><td>NUMLIQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X , Y
!></td><td>--></td><td>COORDONNEES DU MAILLAGE.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FRONT2
     &(NFRLIQ,NFRSOL,DEBLIQ,FINLIQ,DEBSOL,FINSOL,LIHBOR,LIUBOR,
     & X,Y,NBOR,KP1BOR,DEJAVU,NPOIN,NPTFR,KLOG,LISTIN,NUMLIQ,MAXFRO)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DEBLIQ         |<--| DEBUTS DES FRONTIERES LIQUIDES
C| DEBSOL         |<--| DEBUTS DES FRONTIERES SOLIDES
C| DEJAVU         |---| TABLEAU DE TRAVAIL
C| FINLIQ         |<--| FINS DES FRONTIERES LIQUIDES
C| FINSOL         |<--| FINS DES FRONTIERES SOLIDES
C| KLOG           |-->| LIHBOR(K)=KLOG : FRONTIERE SOLIDE
C| KP1BOR         |-->| NUMEROS DES EXTREMITES DES SEGMENTS DE BORD
C|                |   | DANS LA NUMEROTATION DES POINTS DE BORD
C| LIHBOR         |-->| CONDITIONS AUX LIMITES SUR H
C| LISTIN         |-->| IMPRESSIONS SUR LISTING (OU NON)
C| LIUBOR         |---| 
C| MAXFRO         |---| 
C| NBOR           |-->| NUMEROS GLOBAUX DES POINTS DE BORD
C| NFRLIQ         |<--| NOMBRE DE FRONTIERES LIQUIDES
C| NFRSOL         |<--| NOMBRE DE FRONTIERES SOLIDES
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE
C| NUMLIQ         |---| 
C| X , Y          |-->| COORDONNEES DU MAILLAGE.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)  :: NPOIN,NPTFR,KLOG,MAXFRO
      INTEGER, INTENT(OUT) :: NFRLIQ,NFRSOL
      INTEGER, INTENT(OUT) :: DEBLIQ(MAXFRO),FINLIQ(MAXFRO)
      INTEGER, INTENT(OUT) :: DEBSOL(MAXFRO),FINSOL(MAXFRO)
      INTEGER , INTENT(IN) :: LIHBOR(NPTFR),LIUBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN) , Y(NPOIN)
      INTEGER, INTENT(IN) :: NBOR(NPTFR),KP1BOR(NPTFR)
      INTEGER, INTENT(OUT) :: DEJAVU(NPTFR)
      LOGICAL, INTENT(IN) :: LISTIN
      INTEGER, INTENT(OUT) :: NUMLIQ(NPTFR)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K,KPREV,IDEP,SOL1,LIQ1,L1,L2,L3,NILE
C
      LOGICAL SOLF,LIQF,SOLD,LIQD
C
      DOUBLE PRECISION MINNS,MAXNS,EPS,YMIN,NS
C
      INTRINSIC ABS
C
C-----------------------------------------------------------------------
C
C  INITIALISES
C
C  DEJAVU : MARKS WITH 1 THE POINTS THAT HAVE ALREADY BEEN TREATED
C  NILE   : NUMBER OF ISLANDS
C
      DO 10 K=1,NPTFR
        DEJAVU(K) = 0
10    CONTINUE
C
      NILE = 0
      IDEP = 1
      NFRLIQ = 0
      NFRSOL = 0
C
C-----------------------------------------------------------------------
C
C  COMES BACK TO LABEL 20 IF THERE IS AT LEAST 1 ISLAND
C
20    CONTINUE
C
C  LOOKS FOR THE SOUTH-WESTERNMOST POINT (THERE CAN BE MORE THAN 1)
C
      MINNS = X(NBOR(IDEP)) + Y(NBOR(IDEP))
      MAXNS = MINNS
      YMIN  = Y(NBOR(IDEP))
C
      DO 30 K = 1 , NPTFR
      IF(DEJAVU(K).EQ.0) THEN
        NS = X(NBOR(K)) + Y(NBOR(K))
        IF(NS.LT.MINNS) THEN
         IDEP = K
         MINNS = NS
         YMIN = Y(NBOR(K))
        ENDIF
        IF(NS.GT.MAXNS) MAXNS = NS
      ENDIF
30    CONTINUE
C
      EPS = (MAXNS-MINNS) * 1.D-4
C
C  SELECTS THE SOUTHERNMOST POINT FROM THE SOUTH-WESTERNMOST CANDIDATES
C
      DO 40 K = 1 , NPTFR
      IF(DEJAVU(K).EQ.0) THEN
        NS = X(NBOR(K)) + Y(NBOR(K))
        IF(ABS(MINNS-NS).LT.EPS) THEN
          IF(Y(NBOR(K)).LT.YMIN) THEN
           IDEP = K
           YMIN = Y(NBOR(K))
          ENDIF
        ENDIF
      ENDIF
40    CONTINUE
C
C-----------------------------------------------------------------------
C
C  NUMBERS AND LOCATES THE CONTOUR BOUNDARIES STARTING
C  AT POINT IDEP
C
C  SOLD = .TRUE. : THE BOUNDARY STARTING AT IDEP IS SOLID
C  LIQD = .TRUE. : THE BOUNDARY STARTING AT IDEP IS LIQUID
C  SOLF = .TRUE. : THE BOUNDARY ENDING AT IDEP IS SOLID
C  LIQF = .TRUE. : THE BOUNDARY ENDING AT IDEP IS LIQUID
C  LIQ1 : NUMBER OF THE 1ST LIQUID BOUNDARY OF THE CONTOUR
C  SOL1 : NUMBER OF THE 1ST SOLID BOUNDARY OF THE CONTOUR
C
      K = IDEP
C
      SOL1 = 0
      LIQ1 = 0
      LIQF = .FALSE.
      SOLF = .FALSE.
C
C TYPE OF THE 1ST SEGMENT
C
C     LAW OF PREDOMINANCE SOLID OVER LIQUID
      IF(LIHBOR(K).EQ.KLOG.OR.LIHBOR(KP1BOR(K)).EQ.KLOG) THEN
C       THE 1ST SEGMENT IS SOLID
        NFRSOL = NFRSOL + 1
        SOL1 = NFRSOL
        SOLD = .TRUE.
        LIQD = .FALSE.
      ELSE
C       THE 1ST SEGMENT IS LIQUID
        NFRLIQ = NFRLIQ + 1
        LIQ1 = NFRLIQ
        LIQD = .TRUE.
        SOLD = .FALSE.
      ENDIF
C
      DEJAVU(K) = 1
      KPREV = K
      K = KP1BOR(K)
C
50    CONTINUE
C
C LOOKS FOR TRANSITION POINTS FROM THE POINT FOLLOWING IDEB
C
C ALSO LOOKS FOR ISOLATED POINTS TO DETECT THE ERRORS IN
C THE DATA
C
      L1 = LIHBOR(KPREV)
      L2 = LIHBOR(K)
      L3 = LIHBOR(KP1BOR(K))
C
      IF(L1.EQ.KLOG.AND.L2.NE.KLOG.AND.L3.NE.KLOG) THEN
C     SOLID-LIQUID TRANSITION AT POINT K
        NFRLIQ = NFRLIQ + 1
        FINSOL(NFRSOL) = K
        DEBLIQ(NFRLIQ) = K
        LIQF = .TRUE.
        SOLF = .FALSE.
      ELSEIF(L1.NE.KLOG.AND.L2.NE.KLOG.AND.L3.EQ.KLOG) THEN
C     LIQUID-SOLID TRANSITION AT POINT K
        NFRSOL = NFRSOL + 1
        FINLIQ(NFRLIQ) = K
        DEBSOL(NFRSOL) = K
        LIQF = .FALSE.
        SOLF = .TRUE.
      ELSEIF(L1.NE.KLOG.AND.L2.NE.KLOG.AND.L3.NE.KLOG) THEN
C     LIQUID-LIQUID TRANSITIONS AT POINT K
        IF(L2.NE.L3.OR.LIUBOR(K).NE.LIUBOR(KP1BOR(K))) THEN
          FINLIQ(NFRLIQ) = K
          NFRLIQ = NFRLIQ + 1
          DEBLIQ(NFRLIQ) = KP1BOR(K)
        ENDIF
      ELSEIF(L1.EQ.KLOG.AND.L2.NE.KLOG.AND.L3.EQ.KLOG) THEN
C     ERROR IN THE DATA
        IF(LNG.EQ.1) WRITE(LU,102) K
        IF(LNG.EQ.2) WRITE(LU,103) K
        CALL PLANTE(1)
        STOP
      ELSEIF(L1.NE.KLOG.AND.L2.EQ.KLOG.AND.L3.NE.KLOG) THEN
C     ERROR IN THE DATA
        IF(LNG.EQ.1) WRITE(LU,104) K
        IF(LNG.EQ.2) WRITE(LU,105) K
        CALL PLANTE(1)
        STOP
      ENDIF
C
      DEJAVU(K) = 1
      KPREV = K
      K = KP1BOR(K)
      IF(K.NE.IDEP) GO TO 50
C
C  CASE WHERE THE BOUNDARY TYPE CHANGES AT IDEP
C
      IF(SOLF) THEN
C       THE LAST CONTOUR BOUNDARY WAS SOLID
        IF(SOLD) THEN
C         THE FIRST CONTOUR BOUNDARY WAS SOLID
          DEBSOL(SOL1) = DEBSOL(NFRSOL)
          NFRSOL = NFRSOL - 1
        ELSEIF(LIQD) THEN
C         THE FIRST CONTOUR BOUNDARY WAS LIQUID
          DEBLIQ(LIQ1) = IDEP
          FINSOL(NFRSOL) = IDEP
        ENDIF
C
      ELSEIF(LIQF) THEN
C       THE LAST CONTOUR BOUNDARY WAS LIQUID
        IF(LIQD) THEN
C         THE FIRST CONTOUR BOUNDARY WAS LIQUID
          DEBLIQ(LIQ1) = DEBLIQ(NFRLIQ)
          NFRLIQ = NFRLIQ - 1
        ELSEIF(SOLD) THEN
C         THE FIRST CONTOUR BOUNDARY WAS SOLID
          DEBSOL(SOL1) = IDEP
          FINLIQ(NFRLIQ) = IDEP
        ENDIF
C
      ELSE
C     CASE WHERE THE WHOLE CONTOUR HAS THE SAME TYPE
        IF(SOL1.NE.0) THEN
          DEBSOL(SOL1) = IDEP
          FINSOL(SOL1) = IDEP
        ELSEIF(LIQ1.NE.0) THEN
          DEBLIQ(LIQ1) = IDEP
          FINLIQ(LIQ1) = IDEP
        ELSE
          IF(LISTIN.AND.LNG.EQ.1) THEN
           WRITE(LU,'(1X,A)') 'CAS IMPOSSIBLE DANS FRONT2'
          ENDIF
          IF(LISTIN.AND.LNG.EQ.2) THEN
           WRITE(LU,'(1X,A)') 'IMPOSSIBLE CASE IN FRONT2'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
C  CHECKS WHETHER THERE ARE OTHER CONTOURS LEFT:
C
      DO 60 K = 1 , NPTFR
        IF(DEJAVU(K).EQ.0) THEN
          IDEP = K
          NILE = NILE + 1
          GO TO 20
        ENDIF
60    CONTINUE
C
C-----------------------------------------------------------------------
C
      DO 79 K=1,NPTFR
        NUMLIQ(K)=0
79    CONTINUE
C
C  PRINTS OUT THE RESULTS AND COMPUTES NUMLIQ
C
      IF(NILE.NE.0.AND.LISTIN.AND.LNG.EQ.1) WRITE(LU,69) NILE
      IF(NILE.NE.0.AND.LISTIN.AND.LNG.EQ.2) WRITE(LU,169) NILE
C
      IF(NFRLIQ.NE.0) THEN
        IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,70) NFRLIQ
        IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,170) NFRLIQ

        DO 80 K = 1, NFRLIQ
C
C  MARKS THE NUMBERS OF THE LIQUID BOUNDARIES
C
          L1=DEBLIQ(K)
          NUMLIQ(L1)=K
707       L1=KP1BOR(L1)
          NUMLIQ(L1)=K
          IF(L1.NE.FINLIQ(K)) GO TO 707
C
C  END OF MARKING
C
          IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,90)
     &                            K,DEBLIQ(K),NBOR(DEBLIQ(K)),
     &                            X(NBOR(DEBLIQ(K))),Y(NBOR(DEBLIQ(K))),
     &                            FINLIQ(K),NBOR(FINLIQ(K)),
     &                            X(NBOR(FINLIQ(K))),Y(NBOR(FINLIQ(K)))
          IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,190)
     &                            K,DEBLIQ(K),NBOR(DEBLIQ(K)),
     &                            X(NBOR(DEBLIQ(K))),Y(NBOR(DEBLIQ(K))),
     &                            FINLIQ(K),NBOR(FINLIQ(K)),
     &                            X(NBOR(FINLIQ(K))),Y(NBOR(FINLIQ(K)))
80      CONTINUE
      ENDIF
C
      IF(NFRSOL.NE.0) THEN
        IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,100) NFRSOL
        IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,101) NFRSOL
        DO 110 K = 1, NFRSOL
          IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,90)
     &                            K,DEBSOL(K),NBOR(DEBSOL(K)),
     &                            X(NBOR(DEBSOL(K))),Y(NBOR(DEBSOL(K))),
     &                            FINSOL(K),NBOR(FINSOL(K)),
     &                            X(NBOR(FINSOL(K))),Y(NBOR(FINSOL(K)))
          IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,190)
     &                            K,DEBSOL(K),NBOR(DEBSOL(K)),
     &                            X(NBOR(DEBSOL(K))),Y(NBOR(DEBSOL(K))),
     &                            FINSOL(K),NBOR(FINSOL(K)),
     &                            X(NBOR(FINSOL(K))),Y(NBOR(FINSOL(K)))
110     CONTINUE
      ENDIF
C
C-----------------------------------------------------------------------
C
C  FORMATS
C
69    FORMAT(/,1X,'IL Y A ',1I3,' ILE(S) DANS LE DOMAINE')
169   FORMAT(/,1X,'THERE IS ',1I3,' ISLAND(S) IN THE DOMAIN')
70    FORMAT(/,1X,'IL Y A ',1I3,' FRONTIERE(S) LIQUIDE(S) :')
170   FORMAT(/,1X,'THERE IS ',1I3,' LIQUID BOUNDARIES:')
100   FORMAT(/,1X,'IL Y A ',1I3,' FRONTIERE(S) SOLIDE(S) :')
101   FORMAT(/,1X,'THERE IS ',1I3,' SOLID BOUNDARIES:')
102   FORMAT(/,1X,'FRONT2 : ERREUR AU POINT DE BORD ',1I5,
     &       /,1X,'         POINT LIQUIDE ENTRE DEUX POINTS SOLIDES')
103   FORMAT(/,1X,'FRONT2 : ERROR AT BOUNDARY POINT ',1I5,
     &       /,1X,'         LIQUID POINT BETWEEN TWO SOLID POINTS')
104   FORMAT(/,1X,'FRONT2 : ERREUR AU POINT DE BORD ',1I5,
     &       /,1X,'         POINT SOLIDE ENTRE DEUX POINTS LIQUIDES')
105   FORMAT(/,1X,'FRONT2 : ERROR AT BOUNDARY POINT ',1I5,
     &       /,1X,'         SOLID POINT BETWEEN TWO LIQUID POINTS')
90    FORMAT(/,1X,'FRONTIERE ',1I3,' : ',/,1X,
     &            ' DEBUT AU POINT DE BORD ',1I4,
     &            ' , DE NUMERO GLOBAL ',1I6,/,1X,
     &            ' ET DE COORDONNEES : ',G16.7,3X,G16.7,
     &       /,1X,' FIN AU POINT DE BORD ',1I4,
     &            ' , DE NUMERO GLOBAL ',1I6,/,1X,
     &            ' ET DE COORDONNEES : ',G16.7,3X,G16.7)
190   FORMAT(/,1X,'BOUNDARY ',1I3,' : ',/,1X,
     &            ' BEGINS AT BOUNDARY POINT: ',1I4,
     &            ' , WITH GLOBAL NUMBER: ',1I6,/,1X,
     &            ' AND COORDINATES: ',G16.7,3X,G16.7,
     &       /,1X,' ENDS AT BOUNDARY POINT: ',1I4,
     &            ' , WITH GLOBAL NUMBER: ',1I6,/,1X,
     &            ' AND COORDINATES: ',G16.7,3X,G16.7)
C
C-----------------------------------------------------------------------
C
      IF(NFRSOL.GT.MAXFRO.OR.NFRLIQ.GT.MAXFRO) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'FRONT2 : DEPASSEMENT DE TABLEAUX'
          WRITE(LU,*) '         AUGMENTER MAXFRO DANS LE CODE APPELANT'
          WRITE(LU,*) '         A LA VALEUR ',MAX(NFRSOL,NFRLIQ)
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'FRONT2: SIZE OF ARRAYS EXCEEDED'
          WRITE(LU,*) '        INCREASE MAXFRO IN THE CALLING PROGRAM'
          WRITE(LU,*) '        UP TO THE VALUE ',MAX(NFRSOL,NFRLIQ)
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C