C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INTERPOLATES THE BOTTOM ELEVATIONS FROM A SET OF
!>                POINTS ON THE MESH NODES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DM, KP1BOR, NBOR, NP, NPOIN, NPTFR, X, XRELV, Y, YRELV, ZF, ZRELV
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DIFX, DIFY, DIST, DIST1, DIST2, DIST3, DIST4, I, INUM, N, OK1, OK2, OK3, OK4, X1, X2, X3, X4, Y1, Y2, Y3, Y4, ZCADR1, ZCADR2, ZCADR3, ZCADR4, ZDEN, ZNUM
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_FASP
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CROSFR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>FOND(), INIVEN(), LECDOI(), LECDON(), LECHAM(), NOUDON(), NOUMAR()

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
!>      <td><center> 5.9                                       </center>
!> </td><td> 20/03/08
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DM
!></td><td>--></td><td>DISTANCE MINIMUM A LA COTE TOLEREE POUR
!>                  ACCEPTER UN POINT RELEVE.
!>    </td></tr>
!>          <tr><td>KP1BOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMEROTATION GLOBALE DES POINTS DE BORD
!>    </td></tr>
!>          <tr><td>NP
!></td><td>--></td><td>NOMBRE DE POINTS RELEVES
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS DE BORD.
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DU MAILLAGE
!>    </td></tr>
!>          <tr><td>XRELV
!></td><td>--></td><td>ABCISSES DES POINTS RELEVES
!>    </td></tr>
!>          <tr><td>YRELV
!></td><td>--></td><td>ORDONNEES DES POINTS RELEVES
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZRELV
!></td><td>--></td><td>COTES DU FOND DES POINTS RELEVES
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FASP
     &(X,Y,ZF,NPOIN,XRELV,YRELV,ZRELV,NP,NBOR,KP1BOR,NPTFR,DM)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DM             |-->| DISTANCE MINIMUM A LA COTE TOLEREE POUR
C|                |   | ACCEPTER UN POINT RELEVE.
C| KP1BOR         |---| 
C| NBOR           |-->| NUMEROTATION GLOBALE DES POINTS DE BORD
C| NP             |-->| NOMBRE DE POINTS RELEVES
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE.
C| NPTFR          |-->| NOMBRE DE POINTS DE BORD.
C| X,Y            |-->| COORDONNEES DU MAILLAGE
C| XRELV          |-->| ABCISSES DES POINTS RELEVES
C| YRELV          |-->| ORDONNEES DES POINTS RELEVES
C| ZF             |---| 
C| ZRELV          |-->| COTES DU FOND DES POINTS RELEVES
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_FASP => FASP
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN,NP,NPTFR
      INTEGER, INTENT(IN) :: NBOR(NPTFR),KP1BOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)  :: X(NPOIN),Y(NPOIN),DM
      DOUBLE PRECISION, INTENT(IN)  :: XRELV(NP),YRELV(NP),ZRELV(NP)
      DOUBLE PRECISION, INTENT(OUT) :: ZF(NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER N,INUM,I
C
      DOUBLE PRECISION DIST1,DIST2,DIST3,DIST4
      DOUBLE PRECISION ZCADR1,ZCADR2,ZCADR3,ZCADR4
      DOUBLE PRECISION DIFX,DIFY,DIST,X1,Y1,X2,Y2,X3,Y3,X4,Y4
      DOUBLE PRECISION ZNUM,ZDEN
C
      LOGICAL OK1,OK2,OK3,OK4
C
C-----------------------------------------------------------------------
C
C  LOOP ON THE MESH NODES:
C
      DO 100 I = 1 , NPOIN
C
C     INTERPOLATES THE BOTTOM FROM 4 QUADRANTS
C
C ---->  INITIALISES:
C
      DIST1=1.D12
      DIST2=1.D12
      DIST3=1.D12
      DIST4=1.D12
C
      OK1 = .FALSE.
      OK2 = .FALSE.
      OK3 = .FALSE.
      OK4 = .FALSE.
C
      ZCADR1=0.D0
      ZCADR2=0.D0
      ZCADR3=0.D0
      ZCADR4=0.D0
C
C --------->  LOOP ON THE SET OF POINTS (THERE ARE NP):
      DO 30 N=1,NP
           DIFX = XRELV(N)-X(I)
           DIFY = YRELV(N)-Y(I)
           DIST = DIFX*DIFX + DIFY*DIFY
C
             IF ( DIST.LT.1.D-6 ) DIST=1.D-6
C ->QUADRANT 1 :
               IF( DIFX.LE.0.D0.AND.DIFY.LE.0.D0) THEN
                 IF(DIST.LE.DIST1)THEN
                      X1=XRELV(N)
                      Y1=YRELV(N)
                      DIST1=DIST
                      ZCADR1=ZRELV(N)
                      OK1 = .TRUE.
                 ENDIF
C ->QUADRANT 2 :
              ELSE IF( DIFX.GE.0.D0.AND.DIFY.LE.0.D0) THEN
                 IF(DIST.LE.DIST2)THEN
                      X2=XRELV(N)
                      Y2=YRELV(N)
                      DIST2=DIST
                      ZCADR2=ZRELV(N)
                      OK2 = .TRUE.
                 ENDIF
C ->QUADRANT 3 :
              ELSE IF( DIFX.GE.0.D0.AND.DIFY.GE.0.D0) THEN
                 IF(DIST.LE.DIST3)THEN
                      X3=XRELV(N)
                      Y3=YRELV(N)
                      DIST3=DIST
                      ZCADR3=ZRELV(N)
                      OK3 = .TRUE.
                 ENDIF
C ->QUADRANT 4 :
              ELSE IF( DIFX.LE.0.D0.AND.DIFY.GE.0.D0) THEN
                 IF(DIST.LE.DIST4)THEN
                      X4=XRELV(N)
                      Y4=YRELV(N)
                      DIST4=DIST
                      ZCADR4=ZRELV(N)
                      OK4 = .TRUE.
                 ENDIF
              ENDIF
 30        CONTINUE
C
C --------->  END OF LOOP ON THE SET OF POINTS
C
      IF(OK1) CALL CROSFR(X(I),Y(I),X1,Y1,X,Y,NPOIN,NBOR,KP1BOR,
     &                    NPTFR,DM,OK1)
      IF(OK2) CALL CROSFR(X(I),Y(I),X2,Y2,X,Y,NPOIN,NBOR,KP1BOR,
     &                    NPTFR,DM,OK2)
      IF(OK3) CALL CROSFR(X(I),Y(I),X3,Y3,X,Y,NPOIN,NBOR,KP1BOR,
     &                    NPTFR,DM,OK3)
      IF(OK4) CALL CROSFR(X(I),Y(I),X4,Y4,X,Y,NPOIN,NBOR,KP1BOR,
     &                    NPTFR,DM,OK4)
C
         ZNUM = 0.D0
         ZDEN = 0.D0
         INUM = 0
         IF(OK1) THEN
          ZNUM = ZNUM + ZCADR1/DIST1
          ZDEN = ZDEN + 1.D0/DIST1
          INUM = INUM + 1
         ENDIF
         IF(OK2) THEN
          ZNUM = ZNUM + ZCADR2/DIST2
          ZDEN = ZDEN + 1.D0/DIST2
          INUM = INUM + 1
         ENDIF
         IF(OK3) THEN
          ZNUM = ZNUM + ZCADR3/DIST3
          ZDEN = ZDEN + 1.D0/DIST3
          INUM = INUM + 1
         ENDIF
         IF(OK4) THEN
          ZNUM = ZNUM + ZCADR4/DIST4
          ZDEN = ZDEN + 1.D0/DIST4
          INUM = INUM + 1
         ENDIF
C
         IF(INUM.NE.0) THEN
C         ZF : WATER DEPTH AT THE POINT
          ZF(I)=ZNUM/ZDEN
         ELSE
          ZF(I) = -1.D6
         ENDIF
C
100   CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C