C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       WRITES THE GEOMETRY FILE TO SELAFIN FORMAT.
!>  @code
!>    LIST OF THE RECORDS IN THE GEOMETRY FILE:<br>
!>      1    : TITLE
!>      2    : NUMBER OF FUNCTIONS READ FROM GRID 1 AND GRID 2
!>      3    : NAME AND UNIT OF THE VARIABLES
!>      4    : 1,0,0,0,0,0,0,0,0,0
!>      5    : NELEM,NPOIN,NDP,1
!>      6    : IKLE
!>      7    : IPOBO ARRAY OF DIMENSION NPOIN
!>             0 FOR INTERIOR POINTS, A NUMBER OTHERWISE
!>      8    : X
!>      9    : Y<br>
!>    WHAT FOLLOWS IS NOT DONE IN FM3SEL<br>
!>     10    : TIME
!>     11    : VARIABLES DECLARED IN 3 (IN THE ORDER OF THE DECLARATIONS)
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  JMH 01/12/2003 : VARCLA,NVARCL ARE NO LONGER USED.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DATE, I3, I4, IKLE, KNOLG, NBOR, NCSIZE, NDP, NELEM, NFIC, NPLAN, NPOIN, NPTFR, NPTIR, NSOR, NVAR, NVARCL, SORLEO, TEXTE, TIME, TITRE, VARCLA, X, Y
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CBID, ERR, I, IB, IBID, IELEM, IKLES, IPOBO, ISTAT, TITSEL, XBID, YA_IKLES, YA_IPOBO
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ECRI2()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>HOMERE_ADJ_T2D(), SFLO3D(), SORFLO(), TELEMAC2D()

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
!> </td><td> 27/12/05
!> </td><td> J-M HERVOUET (LNH) 01 30 71 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DATE,TIME
!></td><td>--></td><td>DATE (3 INTEGERS) AND TIME (3 INTEGERS)
!>    </td></tr>
!>          <tr><td>I3,I4
!></td><td>--></td><td>INTEGERS, WILL BE PUT IN FILE IN POSITION 3
!>                  AND 4 OF THE ARRAY OF 10 INTEGERS
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td><-></td><td>TABLE DE CONNECTIVITE (I.E. PASSAGE DE LA
!>                  NUMEROTATION LOCALE DES POINTS D'UN ELEMENT
!>                  A LA NUMEROTATION GLOBALE
!>    </td></tr>
!>          <tr><td>KNOLG
!></td><td>--></td><td>GLOBAL NUMBERS OF LOCAL POONTS IN PARALLEL
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMEROTAION GLOBALE DES POINTS DE BORD.
!>    </td></tr>
!>          <tr><td>NCSIZE
!></td><td>--></td><td>NUMBER OF PROCESSORS
!>    </td></tr>
!>          <tr><td>NDP
!></td><td><-></td><td>NOMBRE DE SOMMETS PAR ELEMENT.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td><-></td><td>NOMBRE D'ELEMENTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NFIC
!></td><td>--></td><td>NUMERO DE CANAL DU FICHIER A LIRE OU ECRIRE.
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NUMBER OF PLANES (3D MESHES IN PRISMS)
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td><-></td><td>NOMBRE DE POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td><-></td><td>NOMBRE DE POINTS FRONTIERE DU DOMAINE.
!>    </td></tr>
!>          <tr><td>NPTIR
!></td><td>--></td><td>NUMBER OF INTERFACE POINTS IN PARALLEL
!>    </td></tr>
!>          <tr><td>NSOR
!></td><td>--></td><td>DIMENSION DE SORLEO ET SORIMP
!>    </td></tr>
!>          <tr><td>NVAR
!></td><td><-></td><td>NOMBRE DE VARIABLES DANS LE FICHIER
!>    </td></tr>
!>          <tr><td>NVARCL
!></td><td>--></td><td>NOMBRE DE VARIABLES CLANDESTINES.
!>    </td></tr>
!>          <tr><td>SORLEO
!></td><td>--></td><td>VARIABLES QUE L'ON SOUHAITE ECRIRE DANS LE
!>                  FICHIER (TABLEAU DE 26 LOGIQUES)
!>    </td></tr>
!>          <tr><td>STAND
!></td><td>--></td><td>NON UTILISE
!>    </td></tr>
!>          <tr><td>STD
!></td><td>--></td><td>BINAIRE DU FICHIER (STD, IBM, I3E)
!>    </td></tr>
!>          <tr><td>TEXTE
!></td><td><-></td><td>NOMS ET UNITES DES VARIABLES.
!>    </td></tr>
!>          <tr><td>TITRE
!></td><td><-></td><td>TITRE DU FICHIER.
!>    </td></tr>
!>          <tr><td>VARCLA
!></td><td>--></td><td>TABLEAU AVEC LES NOMS DES VARIABLES CLANDESTINES.
!>    </td></tr>
!>          <tr><td>W
!></td><td>--></td><td>TABLEAU DE TRAVAIL CONSIDERE ICI COMME REEL
!>                  DE TAILLE NPOIN.
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td><-></td><td>COORDONNEES DU MAILLAGE.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ECRGEO
     &(X,Y,NPOIN,NBOR,NFIC,NVAR,TEXTE,VARCLA,NVARCL,
     & TITRE,SORLEO,NSOR,IKLE,NELEM,NPTFR,NDP,DATE,TIME,
     & NCSIZE,NPTIR,KNOLG,NPLAN,I3,I4)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DATE,TIME      |-->| DATE (3 INTEGERS) AND TIME (3 INTEGERS)
C| I3,I4          |-->| INTEGERS, WILL BE PUT IN FILE IN POSITION 3
C|                |   | AND 4 OF THE ARRAY OF 10 INTEGERS
C| IKLE           |<->| TABLE DE CONNECTIVITE (I.E. PASSAGE DE LA
C|                |   | NUMEROTATION LOCALE DES POINTS D'UN ELEMENT
C|                |   | A LA NUMEROTATION GLOBALE
C| KNOLG          |-->| GLOBAL NUMBERS OF LOCAL POONTS IN PARALLEL
C| NBOR           |-->| NUMEROTAION GLOBALE DES POINTS DE BORD.
C| NCSIZE         |-->| NUMBER OF PROCESSORS
C| NDP            |<->| NOMBRE DE SOMMETS PAR ELEMENT.
C| NELEM          |<->| NOMBRE D'ELEMENTS DU MAILLAGE.
C| NFIC           |-->| NUMERO DE CANAL DU FICHIER A LIRE OU ECRIRE.
C| NPLAN          |-->| NUMBER OF PLANES (3D MESHES IN PRISMS)
C| NPOIN          |<->| NOMBRE DE POINTS DU MAILLAGE.
C| NPTFR          |<->| NOMBRE DE POINTS FRONTIERE DU DOMAINE.
C| NPTIR          |-->| NUMBER OF INTERFACE POINTS IN PARALLEL
C| NSOR           |-->| DIMENSION DE SORLEO ET SORIMP
C| NVAR           |<->| NOMBRE DE VARIABLES DANS LE FICHIER
C| NVARCL         |-->| NOMBRE DE VARIABLES CLANDESTINES.
C| SORLEO         |-->| VARIABLES QUE L'ON SOUHAITE ECRIRE DANS LE
C|                |   | FICHIER (TABLEAU DE 26 LOGIQUES)
C| STAND          |-->| NON UTILISE
C| STD            |-->| BINAIRE DU FICHIER (STD, IBM, I3E)
C| TEXTE          |<->| NOMS ET UNITES DES VARIABLES.
C| TITRE          |<->| TITRE DU FICHIER.
C| VARCLA         |-->| TABLEAU AVEC LES NOMS DES VARIABLES CLANDESTINES.
C| W             |-->| TABLEAU DE TRAVAIL CONSIDERE ICI COMME REEL
C|                |   | DE TAILLE NPOIN.
C| X,Y            |<->| COORDONNEES DU MAILLAGE.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN,NFIC,NVARCL,NSOR,NELEM,NPTFR,NDP
      INTEGER, INTENT(OUT) :: NVAR
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*)
C                                    IKLE(NELEM,NDP)
      INTEGER, INTENT(IN) :: NBOR(*),IKLE(*)
      CHARACTER(LEN=32), INTENT(IN) :: TEXTE(*),VARCLA(NVARCL)
C                                            NSOR      NSOR+NVARCL
      CHARACTER(LEN=72), INTENT(IN) :: TITRE
      LOGICAL, INTENT(IN) :: SORLEO(*)
      INTEGER, INTENT(IN) :: NCSIZE,NPTIR
      INTEGER, INTENT(IN) :: TIME(3),DATE(3)
      INTEGER, INTENT(IN) :: KNOLG(NPOIN)
      INTEGER, INTENT(IN), OPTIONAL :: NPLAN,I3,I4
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION XBID(2)
C
      INTEGER IB(10),ISTAT,I,IBID(1),IELEM,ERR
C
      INTEGER, ALLOCATABLE :: IPOBO(:),IKLES(:)
C
      LOGICAL YA_IPOBO,YA_IKLES
C
      CHARACTER*2 CBID
      CHARACTER*80 TITSEL
C
C-----------------------------------------------------------------------
C
      YA_IPOBO = .FALSE.
      YA_IKLES = .FALSE.
C
C   GOES TO THE BEGINNING OF THE FILE
C
      REWIND NFIC
C
C   LEC/ECR 1   : NAME OF GEOMETRY FILE
C
      TITSEL = TITRE // 'SERAPHIN'
      CALL ECRI2(XBID,IBID,TITSEL,80,'CH',NFIC,'STD',ISTAT)
C
C   LEC/ECR 2   : NUMBER OF DISCRETISATION FUNCTIONS 1 AND 2
C
      IB(1)=0
      IB(2)=0
      DO 91 I=1,NSOR
        IF(SORLEO(I)) IB(1) = IB(1) + 1
91    CONTINUE
      CALL ECRI2(XBID,IB,CBID,2,'I ',NFIC,'STD',ISTAT)
      NVAR =  IB(1)  +  IB(2)
C
C   LEC/ECR 3 : NAME AND UNIT OF THE VARIABLES
C
      IF(NVAR.GE.1) THEN
        DO I=1,NSOR
          IF(SORLEO(I)) THEN
           CALL ECRI2(XBID,IBID,TEXTE(I)(1:32),32,'CH',NFIC,'STD',ISTAT)
          ENDIF
        ENDDO
C       IF(NVARCL.NE.0) THEN
C         DO I=1,NVARCL
C         CALL ECRI2(XBID,IBID,VARCLA(I)(1:32),32,'CH',NFIC,'STD',ISTAT)
C         ENDDO
C       ENDIF
      ENDIF
C
C   LEC/ECR 4   : LIST OF 10 INTEGER PARAMETERS
C
        IB(1) = 1
        DO 29 I = 2,10
         IB(I) = 0
29      CONTINUE
C
C       ORIGIN COORDINATES IN METRES
C
        IF(PRESENT(I3)) IB(3)=I3
        IF(PRESENT(I4)) IB(4)=I4
C
C       NUMBER OF PLANES IN 3D
C
        IF(PRESENT(NPLAN)) IB(7)=NPLAN
C
CPARA   MARKING TO INTRODUCE THE READING OF KNOLG
        IF(NCSIZE.GT.1) THEN
          IB(8)=NPTFR
          IB(9)=NPTIR
        ENDIF
CPARA END
C   IS THE DATE PASSED OVER?
        IF(DATE(1)+DATE(2)+DATE(3)+TIME(1)+TIME(2)+TIME(3).NE.0) THEN
         IB(10) = 1
        ENDIF
C   WRITES THE ARRAY OF 10 PARAMETERS
        CALL ECRI2(XBID,IB,CBID,10,'I ',NFIC,'STD',ISTAT)
C   PASSES THE DATE
        IF(IB(10).EQ.1) THEN
          IB(1)=DATE(1)
          IB(2)=DATE(2)
          IB(3)=DATE(3)
          IB(4)=TIME(1)
          IB(5)=TIME(2)
          IB(6)=TIME(3)
          CALL ECRI2(XBID,IB,CBID,6,'I ',NFIC,'STD',ISTAT)
        ENDIF
C
C   LEC/ECR 5 : 4 INTEGERS
C
      IF(NDP.NE.4) THEN
        IB(1) = NELEM
      ELSE
C       TETRAHEDRONS REGROUPED INTO PRISMS
        IB(1)=NELEM/3
      ENDIF
      IB(2) = NPOIN
      IF(NDP.NE.4) THEN
        IB(3) = NDP
      ELSE
C       TETRAHEDRONS REGROUPED INTO PRISMS
        IB(3) = 6
      ENDIF
      IB(4) = 1
      CALL ECRI2(XBID,IB,CBID,4,'I ',NFIC,'STD',ISTAT)
C
C   LEC/ECR 6 : IKLE
C
      IF(NDP.NE.4) THEN
        ALLOCATE(IKLES(NELEM*NDP),STAT=ERR)
      ELSE
C       TETRAHEDRONS REGROUPED INTO PRISMS
        ALLOCATE(IKLES(NELEM*2)  ,STAT=ERR)
      ENDIF
      IF(ERR.NE.0) STOP 'ECRGEO : ALLOCATION DE IKLES'
      YA_IKLES = .TRUE.
C     INVERTS IKLE  IN IKLES FOR SELAFIN
      IF(NDP.NE.4) THEN
        DO I      = 1,NDP
          DO IELEM  = 1,NELEM
            IKLES((IELEM-1)*NDP+I) = IKLE((I-1)*NELEM+IELEM)
          ENDDO
        ENDDO
      ELSE
C     TETRAHEDRONS REGROUPED INTO PRISMS
        DO IELEM  = 1,NELEM/3
          IKLES((IELEM-1)*6+1) = IKLE(      IELEM)
          IKLES((IELEM-1)*6+2) = IKLE(NELEM+IELEM)
          IKLES((IELEM-1)*6+3) = IKLE(NELEM+IELEM)
          IKLES((IELEM-1)*6+4) = IKLE(      IELEM)+NPOIN/NPLAN
          IKLES((IELEM-1)*6+5) = IKLE(NELEM+IELEM)+NPOIN/NPLAN
          IKLES((IELEM-1)*6+6) = IKLE(NELEM+IELEM)+NPOIN/NPLAN
        ENDDO
      ENDIF
C
      IF(NDP.NE.4) THEN
      CALL ECRI2(XBID,IKLES,CBID,NELEM*NDP,'I ',NFIC,'STD',ISTAT)
      ELSE
C     TETRAHEDRONS REGROUPED INTO PRISMS
      CALL ECRI2(XBID,IKLES,CBID,NELEM*2,'I ',NFIC,'STD',ISTAT)
      ENDIF
C
C   LEC/ECR 7 : IPOBO (FILES IN SCALAR MODE)
C
      IF(IB(8).EQ.0.AND.IB(9).EQ.0) THEN
C
        ALLOCATE(IPOBO(NPOIN),STAT=ERR)
        IF(ERR.NE.0) STOP 'ECRGEO : ALLOCATION DE IPOBO'
        YA_IPOBO = .TRUE.
        DO 40 I=1,NPOIN
         IPOBO(I) = 0
40      CONTINUE
C       ONLY LATERAL BOUNDARY POINTS WITH PRISMS
        DO 41 I =1,NPTFR
         IPOBO(NBOR(I)) = I
41      CONTINUE
        CALL ECRI2(XBID,IPOBO,CBID,NPOIN,'I ',NFIC,'STD',ISTAT)
C
      ENDIF
C
      IF(IB(8).NE.0.OR.IB(9).NE.0) THEN
C
C   LEC/ECR  7.1 KNOLG (PARALLEL MODE ONLY)
C
      CALL ECRI2(XBID,KNOLG,CBID,NPOIN,'I ',NFIC,'STD',ISTAT)
C
      ENDIF
C
C   LEC/ECR 8 AND 9: X AND Y COORDINATES OF THE MESH POINTS
C
      CALL ECRI2(X   ,IBID,CBID,NPOIN,'R4',NFIC,'STD',ISTAT)
      CALL ECRI2(Y   ,IBID,CBID,NPOIN,'R4',NFIC,'STD',ISTAT)
C
C-----------------------------------------------------------------------
C
      IF(YA_IPOBO) DEALLOCATE(IPOBO)
      IF(YA_IKLES) DEALLOCATE(IKLES)
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C