C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief    DECODES A REAL, FROM COLUMN ICOL+1 OF THE LINE.
!>             MOVES THE POINTER ICOL TO THE LAST DECODED CHARACTER.
!>             ACCEPTS F FORMAT OR E FORMAT.
!>             ACCEPTS REALS WITH DECIMAL POINTS; ACCEPTS ',' FOR '.'.
!>             IF THE STRING IS NOT COMPLETE, GOES TO THE NEXT LINE
!>             IF NEED BE.
!>             MOVES THE POINTER ICOL TO THE LAST DECODED CHARACTER
!>             OR TO ICOL=0 IF THE NEXT LINE WAS READ.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note      PORTABILITY : IBM,CRAY,HP,SUN

!>  @warning  IF THE VALUE READ IS NOT A REAL, COULD YIELD A
!>            NON-CONTROLLED ERROR BY THE PROGRAM

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ICOL, LIGNE
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> DCINFO : LNG, LU<hr>
!> DCRARE : ERREUR, RETOUR<hr>
!> DCMLIG : NLIGN, LONGLI<hr>
!> DCCHIE : NFIC
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CDEB, CDEB2, CODE, FORMA, FORMAE, I, I1, I2, I3, IFDECI, ILDECI, ILONG, IPOINT, JD1, JD2, LIGNE2, LISUIV, LLDECI, LLONG, LUFIC, RSIGNE, RVAL, TABUL, VUPOIN
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> NEXT(), PREVAL()
!>   </td></tr>
!>     <tr><th> Unknown(s)
!>    </th><td> DLOG10
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DAMOC()

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
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 15/12/1993                                              </td>
!>    <td> O. QUIQUEMPOIX (LNH)  30.87.78.70                       </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center> 5.1                                    </center></td>
!>    <td> 30/09/1993                                              </td>
!>    <td> J.M. HERVOUET (LNH); A. YESSAYAN                        </td>
!>    <td>                                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ERREUR
!></td><td><-></td><td>SORT AVEC LA VALEUR .TRUE. EN CAS D'ERREUR
!>    </td></tr>
!>          <tr><td>ICOL
!></td><td><-></td><td>POSITION COURANTE DU POINTEUR DANS LA LIGNE
!>    </td></tr>
!>          <tr><td>LIGNE
!></td><td><-></td><td>LIGNE EN COURS DE DECODAGE
!>    </td></tr>
!>          <tr><td>LNG
!></td><td>--></td><td>NUMERO DE LA LANGUE DE DECODAGE
!>    </td></tr>
!>          <tr><td>LONGLI
!></td><td>--></td><td>LONGUEUR DES LIGNES
!>    </td></tr>
!>          <tr><td>LU
!></td><td>--></td><td>NUMERO DE L'UNITE LOGIQUE DES SORTIES
!>    </td></tr>
!>          <tr><td>NFIC
!></td><td>--></td><td>NUMERO DE CANAL DU FICHIER EN COURS DE LECT.
!>    </td></tr>
!>          <tr><td>NLIGN
!></td><td><-></td><td>NUMERO DE LA LIGNE TRAITEE DANS LE FICHIER LU
!>    </td></tr>
!>          <tr><td>RETOUR
!></td><td><-></td><td>SORT AVEC LA VALEUR .TRUE. EN CAS DE FIN DE
!>                  FIN DE FICHIER OU D'ERREUR DE LECTURE.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        DOUBLE PRECISION FUNCTION REALU
     &( ICOL , LIGNE )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ERREUR         |<->| SORT AVEC LA VALEUR .TRUE. EN CAS D'ERREUR
C| ICOL           |<->| POSITION COURANTE DU POINTEUR DANS LA LIGNE
C| LIGNE          |<->| LIGNE EN COURS DE DECODAGE
C| LNG            |-->| NUMERO DE LA LANGUE DE DECODAGE
C| LONGLI         |-->| LONGUEUR DES LIGNES
C| LU             |-->| NUMERO DE L'UNITE LOGIQUE DES SORTIES
C| NFIC           |-->| NUMERO DE CANAL DU FICHIER EN COURS DE LECT.
C| NLIGN          |<->| NUMERO DE LA LIGNE TRAITEE DANS LE FICHIER LU
C| RETOUR         |<->| SORT AVEC LA VALEUR .TRUE. EN CAS DE FIN DE
C|                |   | FIN DE FICHIER OU D'ERREUR DE LECTURE.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C
      INTEGER          ICOL
      CHARACTER*(*)    LIGNE
C
      INTEGER          NEXT,PREVAL
      EXTERNAL NEXT,PREVAL
C
      INTEGER          LNG,LU
      INTEGER          NLIGN,LONGLI
      INTEGER          NFIC
      LOGICAL          ERREUR , RETOUR
C
C-----------------------------------------------------------------------
C
      INTRINSIC DLOG10,DBLE,INT,CHAR
C
      INTEGER          I,I1,I2,ILONG,IPOINT,IFDECI,ILDECI,JD1,JD2,I3
      LOGICAL          FORMAE,LUFIC,LISUIV,VUPOIN
      CHARACTER*1      CODE,CDEB,CDEB2,TABUL
      CHARACTER*3      LLONG,LLDECI
      CHARACTER*72     FORMA,LIGNE2
      DOUBLE PRECISION RSIGNE , RVAL
C
C-----------------------------------------------------------------------
C
      COMMON / DCINFO / LNG,LU
      COMMON / DCRARE / ERREUR , RETOUR
      COMMON / DCMLIG / NLIGN , LONGLI
      COMMON / DCCHIE / NFIC
C
C***********************************************************************
C
      LUFIC = .FALSE.
      LISUIV = .FALSE.
      VUPOIN = .FALSE.
      TABUL = CHAR(9)
C
      I1     = NEXT( ICOL+1 , LIGNE )
C
C     //// DETERMINES THE FORMAT: F OR E ////
C
      FORMAE = .FALSE.
C
C     //// DECODES THE SIGN IF NEED BE ////
C
      RSIGNE = +1.D0
      IF ( LIGNE(I1:I1).EQ.'-' ) THEN
           RSIGNE = -1.D0
           I1     =   NEXT ( I1+1      , LIGNE )
      ELSE IF ( LIGNE(I1:I1).EQ.'+' ) THEN
           RSIGNE = +1.D0
           I1     =   NEXT ( I1+1      , LIGNE )
      ENDIF
C
C     //// SEEKS THE FIRST WHITE CHARACTER FOLLOWING THE NUMBER ////
C                       OR A SEPARATOR ';'
C
      I2     = PREVAL (  I1  , LIGNE ,  ' ' , ';' ,TABUL)
C
C     CASE WHERE THE REAL DOES NOT FINISH ON THE LINE                                                                                                                                                                                                                                                                                                                                                                                                                                              LINE
C
      IF (I2.GT.LONGLI) THEN
         LUFIC=.TRUE.
         READ(NFIC,END=900,ERR=998,FMT='(A)') LIGNE2
         CDEB = LIGNE2(1:1)
         CDEB2 = LIGNE2(2:2)
C
         IF ((CDEB.EQ.'0'.OR.CDEB.EQ.'1'.OR.CDEB.EQ.'2'.OR.
     &        CDEB.EQ.'3'.OR.CDEB.EQ.'4'.OR.CDEB.EQ.'5'.OR.
     &        CDEB.EQ.'6'.OR.CDEB.EQ.'7'.OR.CDEB.EQ.'8'.OR.
     &        CDEB.EQ.'9'.OR.CDEB.EQ.'.'.OR.CDEB.EQ.'+'.OR.
     &        CDEB.EQ.'-'.OR.CDEB.EQ.',')
C
     &      .OR.
C
C CASE WHERE IT DEPENDS ON THE SECOND CHARACTER OF THE FOLLOWING LINE
C
     &      ((CDEB.EQ.'E'.OR.CDEB.EQ.'E'.OR.CDEB.EQ.'D'.OR.
     &        CDEB.EQ.'D')
     &      .AND.
     &      ( CDEB2.EQ.'0'.OR.CDEB2.EQ.'1'.OR.CDEB2.EQ.'2'.OR.
     &        CDEB2.EQ.'3'.OR.CDEB2.EQ.'4'.OR.CDEB2.EQ.'5'.OR.
     &        CDEB2.EQ.'6'.OR.CDEB2.EQ.'7'.OR.CDEB2.EQ.'8'.OR.
     &        CDEB2.EQ.'9'.OR.CDEB2.EQ.'+'.OR.CDEB2.EQ.'-'    )))
C
     &      THEN
C
            LISUIV = .TRUE.
            I3=1
            I3=PREVAL(I3,LIGNE2 , ' ' , ';' ,TABUL)
            IF (I1.LE.LONGLI) THEN
              LIGNE = LIGNE(I1:LONGLI)//LIGNE2(1:I3)
            ELSE
              LIGNE = LIGNE2(1:I3)
            ENDIF
            I2 = LONGLI-I1+1+I3
            I1 = 1
         ENDIF
       ENDIF
       GOTO 910
C
 900  CONTINUE
      RETOUR = .TRUE.
 910  CONTINUE
C
C     ILONG: LENGTH OF THE REAL
      ILONG  = I2 - I1
      IPOINT = I2 - 1
      IFDECI = I2 - 1
      DO 100 I = I1 , I2-1
C          ACCEPTS '.' AND ','
           IF ( LIGNE(I:I).EQ.'.' ) THEN
                IPOINT = I
                VUPOIN=.TRUE.
           ELSEIF ( LIGNE(I:I).EQ.',' ) THEN
                LIGNE(I:I)='.'
                IPOINT = I
                VUPOIN=.TRUE.
           ELSE IF (LIGNE(I:I).EQ.'E'.OR.LIGNE(I:I).EQ.'E' ) THEN
C          ACCEPTS BOTH FORMATS E AND D
                FORMAE = .TRUE.
                IFDECI = I-1
           ELSE IF ( LIGNE(I:I).EQ.'D'.OR.LIGNE(I:I).EQ.'D') THEN
                LIGNE(I:I)='E'
                FORMAE = .TRUE.
                IFDECI = I-1
           ENDIF
  100 CONTINUE
C
C     //// NUMBER OF DECIMAL POINTS ///
C
      IF (VUPOIN) THEN
        ILDECI = IFDECI - IPOINT
      ELSE
        ILDECI = 0
      ENDIF
C
C     //// DECODING FORMAT ////
C
      CODE = 'F'
      IF ( FORMAE ) CODE = 'E'
      JD1 = 3 - INT(DLOG10(DBLE(ILONG)))
      WRITE (LLONG,'(I3)') ILONG
      JD2 = 3
      IF ( ILDECI.GT.0 ) JD2 = 3-INT(DLOG10(DBLE(ILDECI)))
      WRITE (LLDECI,'(I3)') ILDECI
      IF ( I1.GT.1 ) THEN
           WRITE ( FORMA , 1010 )  I1-1,CODE,LLONG(JD1:3),LLDECI(JD2:3)
      ELSE
           WRITE ( FORMA , 1020 )  CODE,LLONG(JD1:3),LLDECI(JD2:3)
      ENDIF
C
1010  FORMAT('(',I3,'X,',A1,A,'.',A,')' )
1020  FORMAT('(',A1,A,'.',A,')' )
C
C     ////  DECODES ////
C
      READ  ( LIGNE , FORMA , ERR=995 ) RVAL
      REALU = RSIGNE * RVAL
C
C     //// UPDATES THE POINTER ////
C
      IF (LUFIC) THEN
        NLIGN = NLIGN + 1
        LIGNE = LIGNE2
        IF (LISUIV) THEN
          ICOL = I3-1
        ELSE
          ICOL = 0
        ENDIF
      ELSE
        ICOL = I2 - 1
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
C
C TREATS THE ERRORS DUE TO THE INTERNAL READ FOR CONVERSION
C
995   CONTINUE
      IF(LNG.EQ.1) WRITE(6,996) NLIGN
      IF(LNG.EQ.2) WRITE(6,1996) NLIGN
      WRITE(6,*) LIGNE
996   FORMAT(1X,'ERREUR LIGNE ',1I6,', UN REEL EST ATTENDU : ',/)
1996  FORMAT(1X,'ERREUR LINE ',1I6,', REAL EXPECTED : ',/)
      ERREUR=.TRUE.
      RETURN
C
C TREATS THE ERRORS DUE TO FILE MISREADING
C
998   CONTINUE
      IF(LNG.EQ.1) WRITE(6,999)  NFIC,NLIGN+1
      IF(LNG.EQ.2) WRITE(6,1999) NFIC,NLIGN+1
999   FORMAT(1X,'UNITE LOGIQUE ',1I2,'   ERREUR LIGNE ',1I6)
1999  FORMAT(1X,'LOGICAL UNIT ',1I2,'   ERROR LINE ',1I6)
      RETOUR = .TRUE.
      RETURN
C
      END
C
C#######################################################################
C