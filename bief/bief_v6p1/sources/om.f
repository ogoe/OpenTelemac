C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       OPERATIONS ON MATRICES.
!>  @code
!>   D: DIAGONAL MATRIX
!>   C: CONSTANT<br>
!>   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!>   PERFORMED ON MATRICES M AND N, D AND C.<br>
!>   THE RESULT IS MATRIX M.<br>
!>      OP = 'M=N     '  : COPIES N IN M
!>      OP = 'M=CN    '  : MULTIPLIES N BY C
!>      OP = 'M=M+CN  '  : ADDS CN TO M
!>      OP = 'M=MD    '  : M X D
!>      OP = 'M=DM    '  : D X M
!>      OP = 'M=DMD   '  : D X M X D
!>      OP = 'M=0     '  : SETS M TO 0 (TO BE CHECKED)
!>      OP = 'M=X(M)  '  : NOT SYMMETRICAL FORM OF M
!>      OP = 'M=TN    '  : COPIES TRANSPOSE OF N IN M
!>      OP = 'M=MSK(M)'  : MASKS M EXTRADIAGONAL TERMS
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  BE CAREFUL IF A NEW OPERATION IS ADDED TO THE LIST

!>  @warning  IF OP CONTAINS N, IT THEN MEANS THAT MATRIX N IS USED

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> C, D, M, MESH, N, OP
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELM1, IELM2, IELN1, IELN2, IKLE, MDIAGX, NDIAGM, NDIAGN, NDIAGX, NELEM, NELMAX, NETAGE, NPOIN, NPTFR, NPTFX, NSEG1, NSEG2, SIZXN, STOM, STON, SZMXN, TYPDIM, TYPDIN, TYPEXM, TYPEXN
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_OM, IKLE
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CPSTMT(), DIM1_EXT(), DIM2_EXT(), DIMENS(), INCLUS(), NBSEG(), OM0101(), OM1101(), OM1111(), OM1112(), OM1113(), OM1201(), OM1211(), OM1302(), OM1311(), OM2121(), OM3181(), OM4111(), OM4121(), OM4141(), OM5111(), OM5161(), OMSEG(), OMSEGBOR(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BERKHO(), CVDFTR(), DIFF3D(), DIRI01(), DIRI04(), DIRI09(), GSEBE(), KEPSIL(), MATBOU(), MATRIX(), PREBD4(), PREBD9(), PRECD1(), PRECD4(), PRECD9(), PRECON(), PROPAG(), PROPAG_ADJ(), ROTNE0(), TELEMAC3D(), WAVE_EQUATION()

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
!> </td><td> 05/02/2010
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18; F  LEPEINTRE (LNH) 30 87 78 54
!> </td><td> CALL TO OMSEGBOR MODIFIED, OMSEGPAR SUPPRESSED
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 13/02/2008
!> </td><td> ALGIANE FROEHLY
!> </td><td> ADDED OM1113 AND OM1311
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>C
!></td><td>--></td><td>CONSTANTE DONNEE
!>    </td></tr>
!>          <tr><td>D
!></td><td>--></td><td>MATRICE DIAGONALE : PEUT ETRE UNE STRUCTURE
!>                  OU UN TABLEAU PROVISOIREMENT (VOIR TEST SUR D)
!>    </td></tr>
!>          <tr><td>DM
!></td><td>--></td><td>DIAGONALE DE M
!>    </td></tr>
!>          <tr><td>DN
!></td><td>--></td><td>DIAGONALE DE N
!>    </td></tr>
!>          <tr><td>IELM1
!></td><td>--></td><td>TYPE D'ELEMENT
!>    </td></tr>
!>          <tr><td>IELM2
!></td><td>--></td><td>TYPE D'ELEMENT
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>CORRESPONDANCE NUMEROTATIONS LOCALE ET GLOBALE
!>    </td></tr>
!>          <tr><td>M
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>N
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>OP
!></td><td>--></td><td>OPERATION A EFFECTUER
!>    </td></tr>
!>          <tr><td>TYPDIM
!></td><td>--></td><td>TYPE DE LA DIAGONALE DE M ('Q','I','0')
!>    </td></tr>
!>          <tr><td>TYPDIN
!></td><td>--></td><td>TYPE DE LA DIAGONALE DE N ('Q','I','0')
!>    </td></tr>
!>          <tr><td>TYPEXM
!></td><td>--></td><td>TYPE DE TERMES EXTRADIAGONAUX ('Q','S','0')
!>    </td></tr>
!>          <tr><td>TYPEXN
!></td><td>--></td><td>TYPE DE TERMES EXTRADIAGONAUX
!>    </td></tr>
!>          <tr><td>XM
!></td><td>--></td><td>TERMES EXTRA-DIAGONAUX DE M
!>    </td></tr>
!>          <tr><td>XN
!></td><td>--></td><td>TERMES EXTRA-DIAGONAUX DE N
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE OM
     &( OP , M , N , D , C , MESH )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C             |-->| CONSTANTE DONNEE
C| D             |-->| MATRICE DIAGONALE : PEUT ETRE UNE STRUCTURE
C|                |   | OU UN TABLEAU PROVISOIREMENT (VOIR TEST SUR D)
C| DM             |-->| DIAGONALE DE M
C| DN             |-->| DIAGONALE DE N
C| IELM1          |-->| TYPE D'ELEMENT
C| IELM2          |-->| TYPE D'ELEMENT
C| IKLE           |-->| CORRESPONDANCE NUMEROTATIONS LOCALE ET GLOBALE
C| M             |---| 
C| MESH           |---| 
C| N             |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| OP             |-->| OPERATION A EFFECTUER
C| TYPDIM         |-->| TYPE DE LA DIAGONALE DE M ('Q','I','0')
C| TYPDIN         |-->| TYPE DE LA DIAGONALE DE N ('Q','I','0')
C| TYPEXM         |-->| TYPE DE TERMES EXTRADIAGONAUX ('Q','S','0')
C| TYPEXN         |-->| TYPE DE TERMES EXTRADIAGONAUX
C| XM             |-->| TERMES EXTRA-DIAGONAUX DE M
C| XN             |-->| TERMES EXTRA-DIAGONAUX DE N
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_OM => OM
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, INTENT(IN)    :: C
      CHARACTER(LEN=8), INTENT(IN)    :: OP
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: M
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: N,D
      TYPE(BIEF_MESH) , INTENT(IN)    :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELM1,IELM2,IELN1,IELN2,NELEM,NELMAX
      INTEGER NDIAGM,NDIAGN,NDIAGX,NSEG1,NSEG2
      INTEGER STOM,STON,NPOIN,NPTFR,NPTFX,MDIAGX
      INTEGER SIZXN,SZMXN,NETAGE
C
      CHARACTER*1 TYPDIM,TYPEXM,TYPDIN,TYPEXN
C
      INTEGER, DIMENSION(:), POINTER :: IKLE
C
C-----------------------------------------------------------------------
C
C  CASE WHERE THE STRUCTURE OF M BECOMES THAT OF N
C

      IF(OP(3:8).EQ.'N     '.OR.OP(3:8).EQ.'CN    ') THEN
        CALL CPSTMT(N,M)
      ELSEIF(OP(3:8).EQ.'TN    ') THEN
        CALL CPSTMT(N,M,TRANS=.TRUE.)
      ENDIF
C
C  EXTRACTS THE CHARACTERISTICS OF MATRIX M
C
      TYPDIM = M%TYPDIA
      TYPEXM = M%TYPEXT
      STOM = M%STO
      NDIAGM = M%D%DIM1
      MDIAGX = M%D%MAXDIM1
      IELM1 = M%ELMLIN
      IELM2 = M%ELMCOL
C
      IF(OP(3:8).EQ.'X(M)  ') THEN
        IF(M%ELMLIN.NE.M%ELMCOL) THEN
          IF(LNG.EQ.1) WRITE(LU,900) M%NAME
          IF(LNG.EQ.2) WRITE(LU,901) M%NAME
900       FORMAT(1X,'OM (BIEF) : M (NOM REEL : ',A6,') NON CARREE')
901       FORMAT(1X,'OM (BIEF) : M (REAL NAME: ',A6,') NOT SQUARE')
          IF(LNG.EQ.1) WRITE(LU,700)
          IF(LNG.EQ.2) WRITE(LU,701)
700       FORMAT(1X,'            EST DEJA NON SYMETRIQUE')
701       FORMAT(1X,'            IS ALREADY NON SYMMETRICAL')
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(M%X%MAXDIM2*M%X%MAXDIM1.LT.
     &      BIEF_DIM1_EXT(IELM1,IELM2,STOM,'Q',MESH)*
     &      BIEF_DIM2_EXT(IELM1,IELM2,STOM,'Q',MESH)    ) THEN
            IF(LNG.EQ.1) WRITE(LU,400) M%NAME
            IF(LNG.EQ.2) WRITE(LU,401) M%NAME
            IF(LNG.EQ.1) WRITE(LU,800)
            IF(LNG.EQ.2) WRITE(LU,801)
800         FORMAT(1X,'            POUR DEVENIR NON SYMETRIQUE')
801         FORMAT(1X,'            TO BECOME NON SYMMETRICAL')
            CALL PLANTE(1)
            STOP
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
C  EXTRACTS THE CHARACTERISTICS OF MATRIX N (OPTIONAL)
      IF(INCLUS(OP,'N')) THEN
        TYPDIN = N%TYPDIA
        TYPEXN = N%TYPEXT
        STON   = N%STO
        NDIAGN = N%D%DIM1
        NDIAGX = N%D%MAXDIM1

C
        SIZXN = N%X%DIM1
        SZMXN = N%X%MAXDIM1
C
C 07/02/03 : DIVISION BY NDIAGN=0 AVOIDED (SUBDOMAIN WITHOUT BOUNDARY POINTS
C            IN PARALLEL). COURTESY OLIVER GOETHEL (HANNOVER UNIVERSITY)
C
        IF(NDIAGN.GT.0) THEN
          NETAGE = NDIAGM/NDIAGN - 1
        ELSE
          NETAGE = 0
        ENDIF
C
        IELN1 = N%ELMLIN
        IELN2 = N%ELMCOL
        IF(NDIAGN.GT.MDIAGX) THEN
         IF(LNG.EQ.1) WRITE(LU,400) M%NAME
         IF(LNG.EQ.2) WRITE(LU,401) M%NAME
400      FORMAT(1X,'OM (BIEF) : M (NOM REEL : ',A6,') TROP PETITE')
401      FORMAT(1X,'OM (BIEF) : M (REAL NAME: ',A6,') TOO SMALL')
         STOP
        ENDIF
      ELSE
        IELN1 = IELM1
        IELN2 = IELM2
        STON = STOM
      ENDIF
C
C-----------------------------------------------------------------------
C
C  DEPLOYMENT OF THE MESH STRUCTURE
C
C     STANDARD MATRIX
      IF(DIMENS(IELM1).EQ.MESH%DIM) THEN
        IKLE=>MESH%IKLE%I
        NELEM = MESH%NELEM
        NELMAX= MESH%NELMAX
      ELSE
C     BOUNDARY MATRIX
        IKLE=>MESH%IKLBOR%I
        NELEM  = MESH%NELEB
        NELMAX = MESH%NELEBX
      ENDIF
C
      NPOIN= MESH%NPOIN
      NPTFR= MESH%NPTFR
      NPTFX= MESH%NPTFRX
C
C-----------------------------------------------------------------------
C
C  TRADITIONAL EBE STORAGE:
C
      IF(STOM.EQ.1.AND.STON.EQ.1) THEN
C
      IF(IELM1.EQ.1.AND.IELM2.EQ.1) THEN
C
C     ELEMENTS WITH 2 POINTS
C

      CALL OM0101(OP , M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                 N%D%R,TYPDIN,N%X%R,TYPEXN, D%R,C,
     &                 IKLE,NELEM,NELMAX,NDIAGM)


C
C     ELEMENTS WITH 3 POINTS
C
      ELSEIF( (IELM1.EQ.2 .AND.IELM2.EQ.2 ) .OR.
     &        (IELM1.EQ.11.AND.IELM2.EQ.11) .OR.
     &        (IELM1.EQ.61.AND.IELM2.EQ.61) .OR.
     &        (IELM1.EQ.81.AND.IELM2.EQ.81)       ) THEN
C
        IF( (IELN1.EQ.2 .AND.IELN2.EQ.2 ) .OR.
     &      (IELN1.EQ.11.AND.IELN2.EQ.11) .OR.
     &      (IELN1.EQ.61.AND.IELN2.EQ.61) .OR.
     &      (IELN1.EQ.81.AND.IELN2.EQ.81)         ) THEN
C
          CALL OM1111(OP , M%D%R,TYPDIM,M%X%R,TYPEXM ,
     &                     N%D%R,TYPDIN,N%X%R,TYPEXN,D%R,C,
     &                     IKLE,NELEM,NELMAX,NDIAGM)
C
        ELSEIF(IELN1.EQ.1.AND.IELN2.EQ.1) THEN
C
          CALL OM1101(OP , M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                     N%D%R,TYPDIN,N%X%R,TYPEXN, C,
     &                     MESH%NULONE%I,MESH%NELBOR%I,
     &                     MESH%NBOR%I,
     &                     NELMAX,NDIAGM,NDIAGN,NDIAGX)
C
        ELSE
          IF (LNG.EQ.1) WRITE(LU,100) M%NAME
          IF (LNG.EQ.2) WRITE(LU,101) M%NAME
          IF (LNG.EQ.1) WRITE(LU,200) IELM1,IELM2
          IF (LNG.EQ.2) WRITE(LU,201) IELM1,IELM2
          IF (LNG.EQ.1) WRITE(LU,150) N%NAME
          IF (LNG.EQ.2) WRITE(LU,151) N%NAME
          IF (LNG.EQ.1) WRITE(LU,250) IELN1,IELN2
          IF (LNG.EQ.2) WRITE(LU,251) IELN1,IELN2
          IF (LNG.EQ.1) WRITE(LU,300)
          IF (LNG.EQ.2) WRITE(LU,301)
          CALL PLANTE(1)
          STOP
        ENDIF
C
C     ELEMENTS WITH 4 POINTS
C
      ELSEIF( (IELM1.EQ.21.AND.IELM2.EQ.21) .OR.
     &        (IELM1.EQ.71.AND.IELM2.EQ.71) .OR.
     &        (IELM1.EQ.31.AND.IELM2.EQ.31) .OR.
     &        (IELM1.EQ.51.AND.IELM2.EQ.51) .OR.
     &        (IELM1.EQ.12.AND.IELM2.EQ.12)      ) THEN
C
        IF(   (IELN1.EQ.21.AND.IELN2.EQ.21) .OR.
     &        (IELN1.EQ.71.AND.IELN2.EQ.71) .OR.
     &        (IELN1.EQ.31.AND.IELN2.EQ.31) .OR.
     &        (IELN1.EQ.51.AND.IELN2.EQ.51) .OR.
     &        (IELN1.EQ.12.AND.IELN2.EQ.12)      ) THEN
C
          CALL OM2121(OP , M%D%R,TYPDIM,M%X%R,TYPEXM ,
     &                N%D%R,TYPDIN,N%X%R,TYPEXN, D%R,C,
     &                IKLE,NELEM,NELMAX,NDIAGM)
        ELSEIF(  (IELM1.EQ.12.AND.IELM2.EQ.12) .AND.
     &           (IELN1.EQ.1 .AND.IELN2.EQ.1 )   ) THEN
C
          CALL OM1201(OP , M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                     N%D%R,TYPDIN,N%X%R,TYPEXN, C,
     &                     MESH%NULONE%I,MESH%NELBOR%I,
     &                     MESH%NBOR%I,
     &                     NELMAX,NDIAGM,NDIAGN,NDIAGX)
        ELSEIF(( (IELM1.EQ.51.AND.IELM2.EQ.51) .AND.
     &           (IELN1.EQ.61.AND.IELN2.EQ.61))   ) THEN
C         PRISMS SPLIT IN TETRAHEDRONS M INTERIOR MATRIX
C                                      N LATERAL BOUNDARY MATRIX
          CALL OM5161(OP,M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                N%D%R,TYPDIN,N%X%R,TYPEXN,C,
     &                MESH%NULONE%I,MESH%NELBOR%I,MESH%NBOR%I,
     &                NELMAX,NDIAGN,SIZXN,SZMXN)
C
        ELSEIF(( (IELM1.EQ.31.AND.IELM2.EQ.31) .AND.
     &           (IELN1.EQ.81.AND.IELN2.EQ.81))   ) THEN
C         NOT STRUCTURED TETRAHEDRONS    M INTERIOR MATRIX
C                                        N LATERAL BOUNDARY MATRIX
          CALL OM3181(OP,M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                N%D%R,TYPDIN,N%X%R,TYPEXN,C,
     &                MESH%NULONE%I,MESH%NELBOR%I,MESH%NBOR%I,
     &                NELMAX,NDIAGN,MESH%NELEB,SZMXN)
C
        ELSEIF(  (IELM1.EQ.51.AND.IELM2.EQ.51) .AND.
     &           (IELN1.EQ.11.AND.IELN2.EQ.11)   ) THEN
C         PRISMS SPLIT IN TETRAHEDRONS M INTERIOR MATRIX
C                                      N BOTTOM (NB) OR SURFACE (NS) BOUNDARY MATRIX
C                                      OPERATIONS M+NB AND M+NS
          CALL OM5111(OP , M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                N%D%R,TYPDIN,N%X%R,TYPEXN, C,
     &                NDIAGN,NDIAGX,SIZXN,SZMXN,NETAGE,NELMAX)
C
        ELSE
          IF (LNG.EQ.1) WRITE(LU,100) M%NAME
          IF (LNG.EQ.2) WRITE(LU,101) M%NAME
          IF (LNG.EQ.1) WRITE(LU,200) IELM1,IELM2
          IF (LNG.EQ.2) WRITE(LU,201) IELM1,IELM2
          IF (LNG.EQ.1) WRITE(LU,150) N%NAME
          IF (LNG.EQ.2) WRITE(LU,151) N%NAME
          IF (LNG.EQ.1) WRITE(LU,250) IELN1,IELN2
          IF (LNG.EQ.2) WRITE(LU,251) IELN1,IELN2
          IF (LNG.EQ.1) WRITE(LU,300)
          IF (LNG.EQ.2) WRITE(LU,301)
          CALL PLANTE(1)
          STOP
        ENDIF
C
C     RECTANGULAR MATRICES WITH 3 AND 4 POINTS
C
      ELSEIF(IELM1.EQ.11.AND.IELM2.EQ.12) THEN
C
        IF((IELN1.EQ.11.AND.IELN2.EQ.12).OR.
     &     (IELN1.EQ.12.AND.IELN2.EQ.11.AND.OP(1:4).EQ.'M=TN')) THEN
          CALL OM1112(OP , M%D%R,TYPDIM,M%X%R,TYPEXM ,
     &                     N%D%R,TYPDIN,N%X%R,TYPEXN, D%R,C,
     &                     IKLE,NELEM,NELMAX,NDIAGM)
        ELSE
          IF (LNG.EQ.1) WRITE(LU,100) M%NAME
          IF (LNG.EQ.2) WRITE(LU,101) M%NAME
          IF (LNG.EQ.1) WRITE(LU,200) IELM1,IELM2
          IF (LNG.EQ.2) WRITE(LU,201) IELM1,IELM2
          IF (LNG.EQ.1) WRITE(LU,150) N%NAME
          IF (LNG.EQ.2) WRITE(LU,151) N%NAME
          IF (LNG.EQ.1) WRITE(LU,250) IELN1,IELN2
          IF (LNG.EQ.2) WRITE(LU,251) IELN1,IELN2
          IF (LNG.EQ.1) WRITE(LU,300)
          IF (LNG.EQ.2) WRITE(LU,301)
          CALL PLANTE(1)
          STOP
        ENDIF
C
C     RECTANGULAR MATRICES WITH 4 AND 3 POINTS
C
      ELSEIF(IELM1.EQ.12.AND.IELM2.EQ.11) THEN
C
        IF((IELN1.EQ.12.AND.IELN2.EQ.11).OR.
     &     (IELN1.EQ.11.AND.IELN2.EQ.12.AND.OP(1:4).EQ.'M=TN')) THEN
          CALL OM1211(OP , M%D%R,TYPDIM,M%X%R,TYPEXM ,
     &                N%D%R,TYPDIN,N%X%R,TYPEXN, D%R,C,
     &                IKLE,NELEM,NELMAX,NDIAGM)
        ELSE
          IF (LNG.EQ.1) WRITE(LU,100) M%NAME
          IF (LNG.EQ.2) WRITE(LU,101) M%NAME
          IF (LNG.EQ.1) WRITE(LU,200) IELM1,IELM2
          IF (LNG.EQ.2) WRITE(LU,201) IELM1,IELM2
          IF (LNG.EQ.1) WRITE(LU,150) N%NAME
          IF (LNG.EQ.2) WRITE(LU,151) N%NAME
          IF (LNG.EQ.1) WRITE(LU,250) IELN1,IELN2
          IF (LNG.EQ.2) WRITE(LU,251) IELN1,IELN2
          IF (LNG.EQ.1) WRITE(LU,300)
          IF (LNG.EQ.2) WRITE(LU,301)
          CALL PLANTE(1)
          STOP
        ENDIF
C
C     ELEMENTS WITH 6 POINTS
C
      ELSEIF( (IELM1.EQ.41.AND.IELM2.EQ.41).OR.
     &        (IELM1.EQ.13.AND.IELM2.EQ.13)      ) THEN
C
        IF( (IELN1.EQ.41.AND.IELN2.EQ.41).OR.
     &      (IELN1.EQ.13.AND.IELN2.EQ.13)        ) THEN
C
          CALL OM4141(OP , M%D%R,TYPDIM,M%X%R,TYPEXM ,
     &                N%D%R,TYPDIN,N%X%R,TYPEXN, D%R,C,
     &                IKLE,NELEM,NELMAX,NDIAGM)
C
        ELSEIF(IELN1.EQ.71.AND.IELN2.EQ.71) THEN
C
          CALL OM4121(OP,M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                   N%D%R,TYPDIN,N%X%R,TYPEXN,C,
     &                   MESH%NULONE%I,MESH%NELBOR%I,MESH%NBOR%I,
     &                   NELMAX,NDIAGN,SIZXN,SZMXN)
C
        ELSEIF(IELN1.EQ.11.AND.IELN2.EQ.11) THEN
C
          CALL OM4111(OP , M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                     N%D%R,TYPDIN,N%X%R,TYPEXN, C,
     &                     NDIAGN,NDIAGX,SIZXN,SZMXN,NETAGE,NELMAX)
C
        ELSEIF(  (IELM1.EQ.13.AND.IELM2.EQ.13) .AND.
     &           (IELN1.EQ.2 .AND.IELN2.EQ.2 )   ) THEN
C
          CALL OM1302(OP , M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                     N%D%R,TYPDIN,N%X%R,TYPEXN, C,
     &                     MESH%NULONE%I,MESH%NELBOR%I,
     &                     MESH%NBOR%I,
     &                     NELMAX,NDIAGM,NPTFR,NPTFX)
C
        ELSE
          IF (LNG.EQ.1) WRITE(LU,100) M%NAME
          IF (LNG.EQ.2) WRITE(LU,101) M%NAME
          IF (LNG.EQ.1) WRITE(LU,200) IELM1,IELM2
          IF (LNG.EQ.2) WRITE(LU,201) IELM1,IELM2
          IF (LNG.EQ.1) WRITE(LU,150) N%NAME
          IF (LNG.EQ.2) WRITE(LU,151) N%NAME
          IF (LNG.EQ.1) WRITE(LU,250) IELN1,IELN2
          IF (LNG.EQ.2) WRITE(LU,251) IELN1,IELN2
          IF (LNG.EQ.1) WRITE(LU,300)
          IF (LNG.EQ.2) WRITE(LU,301)
          CALL PLANTE(1)
          STOP
        ENDIF
C
C     RECTANGULAR MATRICES WITH 3 AND 6 POINTS
C
      ELSEIF(IELM1.EQ.11.AND.IELM2.EQ.13) THEN
C
        IF((IELN1.EQ.11.AND.IELN2.EQ.13).OR.
     &     (IELN1.EQ.13.AND.IELN2.EQ.11.AND.OP(1:4).EQ.'M=TN')) THEN
          CALL OM1113(OP , M%D%R,TYPDIM,M%X%R,TYPEXM ,
     &                     N%D%R,TYPDIN,N%X%R,TYPEXN, D%R,C,
     &                     IKLE,NELEM,NELMAX,NDIAGM)
        ELSE
          IF (LNG.EQ.1) WRITE(LU,100) M%NAME
          IF (LNG.EQ.2) WRITE(LU,101) M%NAME
          IF (LNG.EQ.1) WRITE(LU,200) IELM1,IELM2
          IF (LNG.EQ.2) WRITE(LU,201) IELM1,IELM2
          IF (LNG.EQ.1) WRITE(LU,150) N%NAME
          IF (LNG.EQ.2) WRITE(LU,151) N%NAME
          IF (LNG.EQ.1) WRITE(LU,250) IELN1,IELN2
          IF (LNG.EQ.2) WRITE(LU,251) IELN1,IELN2
          IF (LNG.EQ.1) WRITE(LU,300)
          IF (LNG.EQ.2) WRITE(LU,301)
          CALL PLANTE(1)
          STOP
        ENDIF
C
C     RECTANGULAR MATRICES WITH 6 AND 3 POINTS
C
      ELSEIF(IELM1.EQ.13.AND.IELM2.EQ.11) THEN
C
        IF((IELN1.EQ.13.AND.IELN2.EQ.11).OR.
     &     (IELN1.EQ.11.AND.IELN2.EQ.13.AND.OP(1:4).EQ.'M=TN')) THEN
          CALL OM1311(OP , M%D%R,TYPDIM,M%X%R,TYPEXM ,
     &                N%D%R,TYPDIN,N%X%R,TYPEXN, D%R,C,
     &                IKLE,NELEM,NELMAX,NDIAGM)
        ELSE
          IF (LNG.EQ.1) WRITE(LU,100) M%NAME
          IF (LNG.EQ.2) WRITE(LU,101) M%NAME
          IF (LNG.EQ.1) WRITE(LU,200) IELM1,IELM2
          IF (LNG.EQ.2) WRITE(LU,201) IELM1,IELM2
          IF (LNG.EQ.1) WRITE(LU,150) N%NAME
          IF (LNG.EQ.2) WRITE(LU,151) N%NAME
          IF (LNG.EQ.1) WRITE(LU,250) IELN1,IELN2
          IF (LNG.EQ.2) WRITE(LU,251) IELN1,IELN2
          IF (LNG.EQ.1) WRITE(LU,300)
          IF (LNG.EQ.2) WRITE(LU,301)
          CALL PLANTE(1)
          STOP
        ENDIF
C
C  COMBINATION OF IELM1 AND IELM2 NOT IMPLEMENTED: ERROR
C
      ELSE
         IF (LNG.EQ.1) WRITE(LU,100) M%NAME
         IF (LNG.EQ.2) WRITE(LU,101) M%NAME
         IF (LNG.EQ.1) WRITE(LU,200) IELM1,IELM2
         IF (LNG.EQ.2) WRITE(LU,201) IELM1,IELM2
         IF (LNG.EQ.1) WRITE(LU,410) STOM,STON
         IF (LNG.EQ.2) WRITE(LU,411) STOM,STON
         IF (LNG.EQ.1) WRITE(LU,300)
         IF (LNG.EQ.2) WRITE(LU,301)
         CALL PLANTE(1)
         STOP
      ENDIF
C
      ELSEIF(STOM.EQ.3.AND.STON.EQ.3) THEN
C
C  STORAGE BY SEGMENT
C
        IF(M%ELMCOL.NE.N%ELMCOL.OR.M%ELMLIN.NE.N%ELMLIN) THEN
          WRITE(LU,*) 'M ET N DE STRUCTURES DIFFERENTES'
          CALL PLANTE(1)
          STOP
        ENDIF
C
        NSEG1 = BIEF_NBSEG(M%ELMLIN,MESH)
        NSEG2 = BIEF_NBSEG(M%ELMCOL,MESH)
C
C       IN LINEAR-QUADRATIC RECTANGULAR MATRICES, PURELY QUADRATIC
C       SEGMENTS ARE NOT CONSIDERED (NUMBER 13,14 AND 15, SO 3 PER ELEMENT)
C
        IF(IELM1.EQ.11.AND.IELM2.EQ.13) THEN
          NSEG2=NSEG2-3*NELEM
        ELSEIF(IELM1.EQ.13.AND.IELM2.EQ.11) THEN
          NSEG1=NSEG1-3*NELEM
        ENDIF
C
C       IN LINEAR-QUADRATIC RECTANGULAR MATRICES, PURELY QUADRATIC
C       SEGMENTS ARE NOT CONSIDERED (NUMBER 13,14 AND 15, SO 3 PER ELEMENT)
C
        CALL OMSEG(OP , M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                  N%D%R,TYPDIN,N%X%R,TYPEXN,D%R,C,
     &                  NDIAGM,NSEG1,NSEG2,MESH%GLOSEG%I,
     &                  MESH%GLOSEG%MAXDIM1)
C
      ELSEIF(STOM.EQ.3.AND.STON.EQ.1) THEN
C
C       EDGE-BASED STORAGE FOR M AND EBE FOR N
C       THIS CAN HAPPEN ONLY WHEN N IS A BOUNDARY MATRIX
C
        IF(  (M%ELMLIN.EQ.11.AND.M%ELMCOL.EQ.11.AND.
     &        N%ELMLIN.EQ.1 .AND.N%ELMCOL.EQ.1) .OR.
     &       (M%ELMLIN.EQ.12.AND.M%ELMCOL.EQ.12.AND.
     &        N%ELMLIN.EQ.1 .AND.N%ELMCOL.EQ.1) .OR.
     &       (M%ELMLIN.EQ.13.AND.M%ELMCOL.EQ.13.AND.
     &        N%ELMLIN.EQ.1 .AND.N%ELMCOL.EQ.1) .OR.
     &       (M%ELMLIN.EQ.13.AND.M%ELMCOL.EQ.13.AND.
     &        N%ELMLIN.EQ.2 .AND.N%ELMCOL.EQ.2)      ) THEN
C
          NSEG1 = BIEF_NBSEG(M%ELMLIN,MESH)
          NSEG2 = BIEF_NBSEG(M%ELMCOL,MESH)
C
          CALL OMSEGBOR(OP , M%D%R,TYPDIM,M%X%R,TYPEXM,
     &                       N%D%R,TYPDIN,N%X%R,TYPEXN,D%R,C,
     &                       NDIAGM,NSEG1,NSEG2,MESH%NBOR%I,
     &                       MESH%KP1BOR%I,NPTFR,
     &                       M%ELMLIN,N%ELMLIN,
     &                       BIEF_NBSEG(11,MESH))
C
        ELSE
          WRITE(LU,*) 'OM : UNEXPECTED CASE IN SEGMENT STORAGE'
          WRITE(LU,*) '     M%ELMLIN=',M%ELMLIN
          WRITE(LU,*) '     M%ELMCOL=',M%ELMCOL
          WRITE(LU,*) '     M%NAME=',M%NAME
          WRITE(LU,*) '     N%ELMLIN=',N%ELMLIN
          WRITE(LU,*) '     N%ELMCOL=',N%ELMCOL
          WRITE(LU,*) '     N%NAME=',N%NAME
          WRITE(LU,*) '     IMPLEMENTATION MISSING'
          CALL PLANTE(1)
          STOP
        ENDIF
C
C  COMBINATION OF IELM1 AND IELM2 NOT IMPLEMENTED: ERROR
C
C     ELSE
C        IF (LNG.EQ.1) WRITE(LU,100) M%NAME
C        IF (LNG.EQ.2) WRITE(LU,101) M%NAME
C        IF (LNG.EQ.1) WRITE(LU,200) IELM1,IELM2
C        IF (LNG.EQ.2) WRITE(LU,201) IELM1,IELM2
C        IF (LNG.EQ.1) WRITE(LU,410) STOM,STON
C        IF (LNG.EQ.2) WRITE(LU,411) STOM,STON
C        IF (LNG.EQ.1) WRITE(LU,300)
C        IF (LNG.EQ.2) WRITE(LU,301)
C        CALL PLANTE(1)
C        STOP
C     ENDIF
C
      ELSE
C
C  STORAGE COMBINATION NOT IMPLEMENTED
C
         IF (LNG.EQ.1) WRITE(LU,100) M%NAME
         IF (LNG.EQ.2) WRITE(LU,101) M%NAME
         IF (LNG.EQ.1) WRITE(LU,410) STOM,STON
         IF (LNG.EQ.2) WRITE(LU,411) STOM,STON
         IF (LNG.EQ.1) WRITE(LU,300)
         IF (LNG.EQ.2) WRITE(LU,301)
         CALL PLANTE(1)
         STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C  RE-ENCODES THE NEW TYPE
C
      M%TYPDIA = TYPDIM
      M%TYPEXT = TYPEXM
      IF(OP(3:8).EQ.'X(M)  ') THEN
        M%X%DIM1=BIEF_DIM1_EXT(IELM1,IELM2,STOM,'Q',MESH)
        M%X%DIM2=BIEF_DIM2_EXT(IELM1,IELM2,STOM,'Q',MESH)
      ENDIF
C
C-----------------------------------------------------------------------
C
100      FORMAT(1X,'OM (BIEF) : MATRICE M (NOM REEL : ',A6,')')
150      FORMAT(1X,'OM (BIEF) : MATRICE N (NOM REEL : ',A6,')')
200      FORMAT(1X,'            IELM1 = ',1I6,' IELM2 = ',1I6)
250      FORMAT(1X,'            IELN1 = ',1I6,' IELN2 = ',1I6)
300      FORMAT(1X,'            CAS NON PREVU')
410      FORMAT(1X,'ET STOCKAGES   M  : ',1I6,'    N  : ',1I6)
C
101      FORMAT(1X,'OM (BIEF) : MATRIX  M (REAL NAME:',A6,')')
151      FORMAT(1X,'OM (BIEF) : MATRIX  N (REAL NAME:',A6,')')
201      FORMAT(1X,'            IELM1 = ',1I6,' IELM2 = ',1I6)
251      FORMAT(1X,'            IELN1 = ',1I6,' IELN2 = ',1I6)
301      FORMAT(1X,'            THIS CASE IS NOT IMPLEMENTED')
411      FORMAT(1X,'AND STORAGES   M  : ',1I6,' STON  : ',1I6)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
