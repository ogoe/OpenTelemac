!                    *********************
                     SUBROUTINE PROPAG_ADJ
!                    *********************
!
     &(UCONV,VCONV,CONVV,H0,C0,COTOND,PATMOS,ATMOS,
     & HPROP,UN,VN,HN,UTILD,VTILD,HTILD,DH,DU,DV,DHN,VISC,VISC_S,FU,FV,
     & SMH,MESH,ZF,AM1,AM2,AM3,BM1,BM2,CM1,CM2,TM1,A23,A32,MBOR,
     & CV1,CV2,CV3,W1,UBOR,VBOR,AUBOR,HBOR,DIRBOR,
     & TE1,TE2,TE3,TE4,TE5,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,
     & LIMPRO,MASK,GRAV,ROEAU,CF,DIFVIT,IORDRH,IORDRU,LT,AT,DT,
     & TETAH,TETAHC,TETAU,TETAD,
     & AGGLOC,AGGLOU,KDIR,INFOGR,KFROT,ICONVF,
     & PRIVE,ISOUSI,BILMAS,MASSES,YASMH,OPTBAN,CORCON,
     & OPTSUP,MSK,MASKEL,MASKPT,RO,ROVAR,
     & MAT,RHS,UNK,TB,S,BD,PRECCU,SOLSYS,CFLMAX,OPDVIT,OPTSOU,
     & NFRLIQ,SLVPRO,EQUA,VERTIC,
     & ADJO,UD,VD,HD,U,V,H,UU,VV,HH,UIT1,VIT1,HIT1,PP,QQ,RR,
     & TAM1,TAM2,TAM3,TBM1,TBM2,TCM1,
     & TCM2,MATADJ,UNKADJ,ALPHA1,ALPHA2,ALPHA3,ADJDIR,ESTIME,OPTCOST,
     & NIT,NVARRES,VARSOR,
     & NRES,NREF,ALIRE,TROUVE,MAXVAR,VARCL,VARCLA,
     & TEXTE,TEXREF,TEXRES,W,OUTINI,CHESTR,KARMAN,NDEF,
     & ITURB,LISRUG,LINDNER,SB,DP,SP,CHBORD,CFBOR,HFROT,UNSV2D)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE RIGHT HAND SIDE OF THE ADJOINT SYSTEM
!+                IN MATRIX FORM.
!code
!+        T      N-1    T     N-1    T     N-1        *
!+         AM1  P     +  CM1 Q     +  CM2 R     =  CV1
!+
!+        T     N-1     T     N-1                     *
!+         BM1 P      +  AM2 Q                  =  CV2
!+
!+        T     N-1                  T     N-1        *
!+         BM2 P                   +  AM3 R     =  CV3
!
!history  J-M HERVOUET (LNHE)     ; C MOULIN (LNH)
!+        24/04/1997
!+        V5P5
!+   
!
!history  A LEOPARDI (UNINA)
!+        18/09/2000
!+        
!+   
!
!history  
!+        13/11/2000
!+        
!+   COMPLETE VERSION 
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
!| A23,A32        |<->| MATRICES
!| ADJDIR         |---| 
!| ADJO           |---| 
!| AGGLOC         |-->| COEFFICIENT DE MASS-LUMPING SUR H
!| AGGLOU         |-->| COEFFICIENT DE MASS-LUMPING SUR U
!| ALIRE          |---| 
!| ALPHA1         |---| 
!| ALPHA2         |---| 
!| ALPHA3         |---| 
!| AM2            |---| 
!| AM3            |---| 
!| ATMOS          |-->| LOGIQUE INDIQUANT SI PATMOS EST REMPLI.
!| AUBOR          |-->| CONDITIONS AUX LIMITES SUR LE FROTTEMENT.
!| BD             |---| 
!| BILMAS         |-->| INDIQUE SI ON FAIT LE BILAN DE MASSE
!| BM2            |---| 
!| C0             |-->| CELERITE DE REFERENCE
!| CF             |---| 
!| CFBOR          |---| 
!| CFLMAX         |---| 
!| CHBORD         |---| 
!| CHESTR         |-->| COEFFICIENT DE FROTTEMENT AU FOND.
!| CM1            |---| 
!| CM2            |---| 
!| CONVV          |-->| LOGIQUES INDIQUANT LES VARIABLES QUE L'ON
!|                |   | VEUT CONVECTER
!|                |   | CONVV(1):U,V CONVV(2):H
!| CORCON         |-->| CORRECTION DE CONTINUITE SUR LES POINTS A
!|                |   | HAUTEUR IMPOSEE (ON CORRIGE LES VITESSES)
!| COTOND         |<--| EXPRESSION DE CU/G DANS LA THEORIE DE L'ONDE
!| CV1,CV2,CV3    |<->| SECONDS MEMBRES DU SYSTEME.
!| DH,DHN         |<--| STOCKAGE DE LA VARIABLE DH  (DHN AU TEMPS N)
!| DIFVIT         |-->| INDIQUE S'IL FAUT FAIRE LA DIFFUSION DE U,V
!| DIRBOR         |---| 
!| DP             |---| 
!| DU,DV          |<--| STOCKAGE DES QCCROISSEMENTS EN U ET V
!| EQUA           |---| 
!| ESTIME         |---| 
!| FU,FV          |<->| TERMES SOURCES TRAITES EN P1
!| GRAV           |-->| CONSTANTE DE GRAVITE .
!| H0             |---| 
!| HBOR           |-->| CONDITIONS AUX LIMITES SUR H.
!| HD             |---| 
!| HFROT          |---| 
!| HH             |---| 
!| HIT1           |---| 
!| HPROP          |-->| HAUTEUR DE PROPAGATION
!| HTILD          |---| 
!| ICONVF         |-->| FORME DE LA CONVECTION
!|                |   | TABLEAU DE 4 VALEURS ENTIERES POUR :
!|                |   | ICONVF(1) : U ET V
!|                |   | ICONVF(2) : H
!|                |   | ICONVF(3) : TRACEUR
!|                |   | ICONVF(4) : K ET EPSILON
!| INFOGR         |-->| INFORMATIONS SUR LE GRADIENT (LOGIQUE)
!| IORDRH         |-->| ORDRE DU TIR INITIAL POUR H
!| IORDRU         |-->| ORDRE DU TIR INITIAL POUR U
!| ISOUSI         |-->| NUMERO DE LA SOUS-ITERATION DANS LE PAS
!|                |   | DE TEMPS.
!| ITURB          |---| 
!| KARMAN         |-->| CONSTANTE DE KARMAN.
!| KDIR           |-->| CONDITION A LA LIMITE DE TYPE DIRICHLET
!| KFROT          |-->| LOI DE FROTTEMENT SUR LE FOND
!| LIMPRO         |-->| TYPES DE CONDITIONS AUX LIMITES
!| LINDNER        |---| 
!| LISRUG         |---| 
!| LT,AT,DT       |-->| NUMERO D'ITERATION, TEMPS, PAS DE TEMPS
!| MASK           |-->| BLOC DE MASQUES POUR LES SEGMENTS :
!|                |   | MASK(MSK1): 1. SI KDIR SUR U 0. SINON
!|                |   | MASK(MSK2): 1. SI KDIR SUR V 0. SINON
!|                |   | MASK(MSK3): 1. SI KDDL SUR U 0. SINON
!|                |   | MASK(MSK4): 1. SI KDDL SUR V 0. SINON
!|                |   | MASK(MSK6): 1. SI KNEU SUR V 0. SINON
!|                |   | MASK(MSK7): 1. SI KOND 0. SINON
!|                |   | MASK(MSK9): 1. SI KDIR SUR H (POINT)
!| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
!|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE
!| MASKPT         |-->| MASQUES PAR POINTS.
!| MASSES         |-->| MASSE CREEE PAR TERME SOURCE PENDANT
!|                |   | LE PAS DE TEMPS.
!| MAT            |---| 
!| MATADJ         |---| 
!| MAXVAR         |---| 
!| MBOR           |---| 
!| MESH           |---| 
!| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
!| NDEF           |---| 
!| NFRLIQ         |---| 
!| NIT            |---| 
!| NREF           |---| 
!| NRES           |---| 
!| NVARRES        |---| 
!| OPDVIT         |---| 
!| OPTBAN         |-->| OPTION DE TRAITEMENT DES BANCS DECOUVRANTS
!|                |   | NON UTILISE POUR L'INSTANT :
!| OPTCOST        |---| 
!| OPTSOU         |---| 
!| OPTSUP         |---| 
!| OUTINI         |---| 
!| PATMOS         |-->| TABLEAU DE VALEURS DE LA PRESSION ATMOSPHER.
!| PP             |---| 
!| PRECCU         |---| 
!| PRIVE          |-->| TABLEAU DE TRAVAIL DEFINI DANS PRINCI
!| QQ             |---| 
!| RHS            |---| 
!| RO             |-->| MASSE VOLUMIQUE SI ELLE VARIABLE
!| ROEAU          |-->| MASSE VOLUMIQUE DE L'EAU.
!| ROVAR          |-->| OUI SI LA MASSE VOLUMIQUE EST VARIABLE.
!| RR             |---| 
!| S              |-->| STRUCTURE BIDON
!| SB             |---| 
!| SLVPRO         |---| 
!| SMH            |-->| TERMES SOURCES DE L'EQUATION DE CONTINUITE
!| SOLSYS         |---| 
!| SP             |---| 
!| T1             |---| 
!| T10            |---| 
!| T11            |---| 
!| T2             |---| 
!| T3             |---| 
!| T4             |---| 
!| T5             |---| 
!| T6             |---| 
!| T7             |---| 
!| T8             |---| 
!| T9             |---| 
!| TAM1           |---| 
!| TAM2           |---| 
!| TAM3           |---| 
!| TB             |---| 
!| TBM1           |---| 
!| TBM2           |---| 
!| TCM1           |---| 
!| TCM2           |---| 
!| TE1            |---| 
!| TE2            |---| 
!| TE3            |---| 
!| TE4            |---| 
!| TE5            |---| 
!| TETAD          |-->| IMPLICITATION SUR LA DIFFUSION (=1.)
!| TETAH          |-->| IMPLICITATION SUR H DANS L'EQUATION SUR U
!| TETAHC         |-->| IMPLICITATION SUR H DANS LA CONTINUITE
!| TETAU          |-->| IMPLICITATION SUR U ET V
!| TEXREF         |---| 
!| TEXRES         |---| 
!| TEXTE          |---| 
!| TM1            |<->| MATRICE
!| TROUVE         |---| 
!| UBOR           |-->| CONDITIONS AUX LIMITES SUR U.
!| UCONV,VCONV    |-->| CHAMP CONVECTEUR
!| UD             |---| 
!| UIT1           |---| 
!| UN,VN,HN       |-->| VALEURS A L' ETAPE N.
!| UNK            |---| 
!| UNKADJ         |---| 
!| UNSV2D         |---| 
!| UTILD          |---| 
!| UU             |---| 
!| VARCL          |---| 
!| VARCLA         |---| 
!| VARSOR         |---| 
!| VBOR           |-->| CONDITIONS AUX LIMITES SUR V.
!| VD             |---| 
!| VERTIC         |---| 
!| VISC           |-->| VISCOSITE TURBULENTE .
!| VISC_S         |---| 
!| VIT1           |---| 
!| VTILD          |---| 
!| VV             |---| 
!| W              |---| 
!| W1             |<->| TABLEAU DE TRAVAIL.
!| YASMH          |-->| INDIQUE SI ON PREND EN COMPTE SMH
!| ZF             |-->| COTE DU FONT AU NOEUD DE MAILLAGE .
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_PROPAG_ADJ => PROPAG_ADJ
      USE DECLARATIONS_TELEMAC2D, ONLY : KFROTL
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: LT,OPTSUP(4),KDIR,KFROT,ICONVF(4)
      INTEGER, INTENT(IN) :: IORDRH,IORDRU,ISOUSI,OPTBAN,OPTSOU,SOLSYS
      INTEGER, INTENT(IN) :: OPDVIT,NFRLIQ,LISRUG,ITURB,OPTCOST
      INTEGER, INTENT(IN)    :: NIT,NRES,NREF,MAXVAR,HFROT
      INTEGER, INTENT(INOUT) :: NVARRES,TROUVE(*),ALIRE(*)
      LOGICAL, INTENT(IN)    :: BILMAS,ATMOS,DIFVIT,INFOGR,CONVV(4),MSK
      LOGICAL, INTENT(IN)    :: YASMH,ROVAR,PRECCU,VERTIC,ADJO,CORCON
      LOGICAL, INTENT(IN)    :: OUTINI,LINDNER
      DOUBLE PRECISION, INTENT(IN)    :: TETAU,TETAD,TETAH,AGGLOC,AGGLOU
      DOUBLE PRECISION, INTENT(IN)    :: TETAHC,AT,DT,GRAV,ROEAU,CFLMAX
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN,NDEF,DP,SP
      DOUBLE PRECISION, INTENT(INOUT) :: MASSES,SB
      TYPE(SLVCFG), INTENT(INOUT)     :: SLVPRO
      CHARACTER(LEN=20), INTENT(IN)   :: EQUA
      TYPE(BIEF_OBJ), INTENT(IN)      :: UCONV,VCONV,SMH,UN,VN,HN
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: RO
      TYPE(BIEF_OBJ), INTENT(IN)      :: UTILD,VTILD,PATMOS,CF,UNSV2D
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: U,V,H,CV1,CV2,CV3,PRIVE,DH,DHN
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: DU,DV,FU,FV,VISC,VISC_S,HTILD
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: UBOR,VBOR,HBOR,AUBOR
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKEL,MASKPT,ZF
      TYPE(BIEF_OBJ), INTENT(IN)      :: HPROP,H0,C0,COTOND,LIMPRO
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T1,T2,T3,T4,T5,T6,T7,T8,T9,T10
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T11
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TE1,TE2,TE3,TE4,TE5
!     STRUCTURES OF MATRICES
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TAM1,TAM2,TAM3,TBM1
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TBM2,TCM1,TCM2
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: AM1,AM2,AM3,BM1,BM2,CM1,CM2,TM1
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: A23,A32,MBOR
!
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: MASK,MAT,RHS,UNK,TB,BD,DIRBOR
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: CHESTR
      TYPE (BIEF_OBJ), INTENT(INOUT)  :: HD,UD,VD,ALPHA1,ALPHA2,ALPHA3
      TYPE (BIEF_OBJ), INTENT(INOUT)  :: HH,UU,VV,UIT1,VIT1,HIT1
      TYPE (BIEF_OBJ), INTENT(INOUT)  :: PP,QQ,RR,CHBORD,CFBOR
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: W1
      TYPE(BIEF_OBJ), INTENT(IN)      :: S
      REAL,  INTENT(INOUT)            :: W(*)
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: VARSOR
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: MATADJ,UNKADJ,ADJDIR,VARCL
      CHARACTER(LEN=72), INTENT(IN)   :: ESTIME
      CHARACTER(LEN=32), INTENT(INOUT):: VARCLA(10),TEXTE(*)
      CHARACTER(LEN=32), INTENT(INOUT):: TEXREF(*),TEXRES(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ITER,I,IELMU,IELMH
      INTEGER UDIR,UDDL,UNEU,HOND,UNONNEU,VDIR,VDDL
!
      DOUBLE PRECISION Z(1),SL1,SL1U,C,AT1,HIST(1)
!
      LOGICAL MSKGRA
!
      CHARACTER*16 FORMULE
!
!-----------------------------------------------------------------------
!
! FH-FRDATA
      DOUBLE PRECISION, PARAMETER :: VK = 1.D-6
! FH-FRDATA
!-----------------------------------------------------------------------
!
      IELMH=HH%ELM
      IELMU=UU%ELM
!
!  ADDRESSES OF THE ARRAYS IN THE MASKING BLOCK: MASK
!
      UDIR = 1
      VDIR = 2
      UDDL = 3
      VDDL = 4
      UNEU = 5
!     VNEU = 6
      HOND = 7
      UNONNEU = 8
!
!-----------------------------------------------------------------------
!
      IF(SOLSYS.NE.1) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'TRAITEMENT DU SYSTEME LINEAIRE : ',SOLSYS
          WRITE(LU,*) 'CAS NON PREVU EN MODE ESTIMATION'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'TREATMENT OF THE LINEAR SYSTEM : ',SOLSYS
          WRITE(LU,*) 'UNEXPECTED CASE IN ESTIMATION MODE'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     COMPUTES MATRIX FOR ADJOINT SYSTEM
!
      CALL OM( 'M=TN    ' , TAM1, AM1, S, C, MESH )
      CALL OM( 'M=TN    ' , TAM2, AM2, S, C, MESH )
      CALL OM( 'M=TN    ' , TAM3, AM3, S, C, MESH )
      CALL OM( 'M=TN    ' , TBM1, BM1, S, C, MESH )
      CALL OM( 'M=TN    ' , TBM2, BM2, S, C, MESH )
      CALL OM( 'M=TN    ' , TCM1, CM1, S, C, MESH )
      CALL OM( 'M=TN    ' , TCM2, CM2, S, C, MESH )
!
!=======================================================================
!
!     COMPUTES RIGHT HAND SIDES FOR ADJOINT SYSTEM
!
!=======================================================================
!
!     NB: HIT1, UIT1, VIT1 ARE DIRECT VARIABLES AT TIME IT+1
!         HH  , UU  , VV   ARE DIRECT VARIABLES AT TIME IT
!         HN  , UN  , VN   ARE DIRECT VARIABLES AT TIME IT-1
!
!
!           IT    IT    IT
!  TERMS 2 W   ( X   - M   ) OR EQUIVALENT DEPENDING ON THE COST FUNCTION
!           IP    IP    IP
!
!     INITIALISES CV1, CV2 AND CV3
!     IN STEADY STATE MODE, WEIGHTS ALPHA1, ALPHA2 AND ALPHA3 ARE
!     INITIALISED BY A CALL TO "MESURES" IN HOMERE_T2D_ADJ, IN THE LOOP
!     FOR THE COMPUTATION OF THE COST FUNCTION. THEN THEY ARE CANCELLED
!     AT THE END OF THIS ROUTINE
!
      CALL COST_FUNCTION(C,OPTCOST,'RHS')
!
!-----------------------------------------------------------------------
!
!  PREPARES FRICTION TERMS AND VECTOR T1 EQUAL TO 0
!
!     T10 = MASS MATRIX LUMPED / COS(SLOPE)
      CALL SLOPES(TE3,ZF,MESH)
      CALL VECTOR(T10,'=','MASBAS          ',IELMU,1.D0,T2,
     &                T2,T2,T2,T2,T2,MESH,.TRUE.,TE3)
!     FU IN T11 (AND FV=FU)
!
!     T2 WILL HOLD CF AT ITERATION IT+1
!
      CALL CPSTVC(CF,T2)
!
!FH-FRDATA
!     CALL COEFRO(T2,HH,UU,VV,KARMAN,KFROT,CHESTR,GRAV,MESH,T1)
      CALL FRICTION_UNIF(MESH,HH,UU,VV,CHESTR,S,KFROT,KFROTL,0,LISRUG,
     &                   .FALSE.,SB,NDEF,DP,SP,VK,KARMAN,GRAV,
     &                   T1,T2,CHBORD,T2,CFBOR)
!FH-FRDATA
!
      CALL FRICTI(T11,T3,T4,T5,UU,VV,HH,T2,MESH,T6,T7,VERTIC,UNSV2D,
     &            MSK,MASKEL,HFROT)
!     CALL FRICTI(T11,T3,T4,T5,UU,VV,HH,CF,MESH,T6,VERTIC)
!
!     FINAL FU OF PHD IN T11
      CALL OS('X=XY    ', T11 , T10 , T10 , C )
!
!     T1 : 0 VECTOR
      CALL CPSTVC(HH,T1)
      CALL OS('X=C     ' , T1 , T1 , T1 , 0.D0)
!
!-----------------------------------------------------------------------
!
!  COMPUTES CV1 : CV1 = CV1 + T11+T12+T13+T14+T15+T16+T17
!
!-----------------------------------------------------------------------
!  TERM T11 (3 PARTS)
!-----------------------------------------------------------------------
!
!  T11_1 : M/DT  * H
!                   ADJ
!     TERM 1B OF AL
      CALL MATRIX(AM1,'M=N     ','MATMAS          ',IELMH,IELMH,
     &            1.D0/DT,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
      CALL MATVEC('X=X+AY  ', CV1 , AM1 , PP , C , MESH)
!
!  T11_2 : ADVECTION
!
!  T11_3 :
!
!     8B OF AL
!     CALL MATRIX(BM1,'M=N     ','MATGRF         X',IELMH,IELMH,
!    *           (TETAU-1.D0),UU,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
!     CALL MATVEC('X=X+AY  ', CV1 , BM1 , PN , C , MESH)
!     9B OF AL
!     CALL MATRIX(BM2,'M=N     ','MATGRF         Y',IELMH,IELMH,
!    *           (TETAU-1.D0),VV,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
!     CALL MATVEC('X=X+AY  ', CV1 , BM2 , PN , C , MESH)
!
!     CORRECTED VERSION JMH: NOW ICONVF(2) IS ALWAYS 5
!
!     IF(ICONVF(2).EQ.5.OR.ICONVF(2).EQ.8) THEN
        FORMULE='MATFGR          '
!     ELSE
!       FORMULE='MATGRF          '
!     ENDIF
!
      FORMULE(16:16)='X'
      CALL MATRIX(BM1,'M=N     ',FORMULE,IELMH,IELMU,
     &           (TETAU-1.D0),PP,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
      CALL MATVEC('X=X+AY  ', CV1 , BM1 , UU , C , MESH)
      FORMULE(16:16)='Y'
      CALL MATRIX(BM2,'M=N     ',FORMULE,IELMH,IELMU,
     &           (TETAU-1.D0),PP,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
      CALL MATVEC('X=X+AY  ', CV1 , BM2 , VV , C , MESH)
!
!                           H
!  T11_4 : BOUNDARY TERM TB1
!                           ADJ
!
!     JMH : I HAVE -1.D0 INSTEAD OF TETAU-1.D0, BUT THEN HE SUBTRACTS TETAU ??
!           SAME THING EXCEPT FOR INCIDENT WAVE ???
      CALL VECTOR(T2,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            TETAU-1.D0,PP,T1,T1,UU,VV,T1,MESH,.TRUE.,
     &            MASK%ADR(8)%P)
      CALL OSDB('X=X+Y   ' , CV1 , T2 , T2 , C , MESH)
!     DIRICHLET ON VELOCITY :
      CALL VECTOR(T2,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            -TETAU,PP,T1,T1,UU,VV,T1,MESH,
     &            .TRUE.,MASK%ADR(UDIR)%P)
      CALL OSDB('X=X+Y   ' , CV1 , T2 , T2 , C , MESH)
!     FREE FLOW ON VELOCITY :
      CALL VECTOR(T2,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            -TETAU,PP,T1,T1,UU,VV,T1,MESH,
     &            .TRUE.,MASK%ADR(UDDL)%P)
      CALL OSDB('X=X+Y   ' , CV1 , T2 , T2 , C , MESH)
!
!-----------------------------------------------------------------------
!  TERM T12
!-----------------------------------------------------------------------
!
!     TERM 2B OF AL
      CALL MATVEC('X=X+CAY ',CV1,TCM1,QQ,(TETAH-1.D0)/TETAH,MESH)
!
!-----------------------------------------------------------------------
!  TERM T13
!-----------------------------------------------------------------------
!
!     TERM 3B OF AL
      CALL MATVEC('X=X+CAY ',CV1,TCM2,RR,(TETAH-1.D0)/TETAH,MESH)
!
!-----------------------------------------------------------------------
!  TERM T14
!-----------------------------------------------------------------------
!
!     TERM 1C OF AL
!     VERSION EB+AL, NOTE JMH : DON'T AGREE
!     CALL MATRIX(BM1,'M=N     ','MATGRF         X',IELMH,IELMH,
!    *            -TETAU,UN,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
!     CALL MATVEC('X=X+AY  ', CV1 , BM1 , PN , C , MESH)
!
!     VERSION JMH+CC
!                        START OF FORMULATION MADE FOR T11_3
      FORMULE(16:16)='X'
      CALL MATRIX(BM1,'M=N     ',FORMULE,IELMH,IELMU,
     &            -TETAU,PP,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
      CALL MATVEC('X=X+AY  ', CV1 , BM1 , UIT1 , C , MESH)
!
!-----------------------------------------------------------------------
!  TERM T15 + T17
!-----------------------------------------------------------------------
!
!          IT+1   IT+1    IT+1    IT+1
!     T3= U    * Q     + V     * R
!
      CALL OS('X=YZ    ', T3 , UIT1 , QQ , C )
      CALL OS('X=X+YZ  ', T3 , VIT1 , RR , C )
!
!     T4=-(4/3)/H OR -1/H
      CALL OS('X=1/Y   ', T4 , HH , HH , C ,2,0.D0,1.D-6)
      IF(KFROT.EQ.3) THEN
        CALL OS('X=CX    ', T4 , T4 , T4 , -4.D0/3.D0)
      ELSEIF(KFROT.EQ.2) THEN
        CALL OS('X=CX    ', T4 , T4 , T4 , -1.D0     )
      ELSE
        IF(LNG.EQ.1) WRITE(LU,*) 'LOI NON TRAITEE POUR L''ESTIMATION'
        IF(LNG.EQ.2) WRITE(LU,*) 'WRONG FRICTION LAW FOR ESTIMATION'
        CALL PLANTE(1)
        STOP
      ENDIF
      CALL OS('X=XY    ', T4 , T11 , T11 , C )
!     AND IF T3 IS QUASI-BUBBLE?
      CALL OS('X=X+YZ  ', CV1 , T4 , T3  , C )
!
!-----------------------------------------------------------------------
!  TERM T16
!-----------------------------------------------------------------------
!
!     TERM 2C OF AL
!     VERSION EB+AL, NOTE JMH : DON'T AGREE
!     CALL MATRIX(BM2,'M=N     ','MATGRF         Y',IELMH,IELMH,
!    *            -TETAU,VN,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
!     CALL MATVEC('X=X+AY  ', CV1 , BM2 , PN , C , MESH)
!     VERSION JMH+CC
!                        START OF FORMULATION MADE FOR T11_3
      FORMULE(16:16)='Y'
      CALL MATRIX(BM2,'M=N     ',FORMULE,IELMH,IELMU,
     &            -TETAU,PP,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
      CALL MATVEC('X=X+AY  ', CV1 , BM2 , VIT1 , C , MESH)
!
!-----------------------------------------------------------------------
!
!  COMPUTES CV2 AND CV3 :
!
!-----------------------------------------------------------------------
!  TERM  T21
!-----------------------------------------------------------------------
!
!  T21_1 : ADVECTION
!
!  T21_2 : (5B OF EB+AL)
!
      FORMULE(16:16)='X'
      CALL MATRIX(BM1,'M=N     ',FORMULE,IELMH,IELMU,
     &            1.D0,HH,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
      CALL MATVEC('X=X+CTAY',CV2,BM1,PP,TETAU-1.D0,MESH)
! OLD PROGRAMMING (TBM1 FROM HN AT AN INCORRECT TIMESTEP)
!     CALL MATVEC('X=X+CAY ',CV2,TBM1,PP,(TETAU-1.D0)/TETAU,MESH)
!
!                           U
!  T21_3 : BOUNDARY TERM TB1
!                           ADJ
!
!     JMH : I HAVE 1.D0 INSTEAD OF TETAU-1.D0
      CALL VECTOR(T2,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            TETAU-1.D0,PP,T1,T1,HH,T1,T1,MESH,.TRUE.,
     &            MASK%ADR(8)%P)
      CALL OSDB('X=X+Y   ' , CV2 , T2 , T2 , C , MESH)
!     DIRICHLET ON VELOCITY U:
      CALL VECTOR(T2,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            -TETAU,PP,T1,T1,HH,T1,T1,MESH,
     &            .TRUE.,MASK%ADR(UDIR)%P)
      CALL OSDB('X=X+Y   ' , CV2 , T2 , T2 , C , MESH)
!     FREE FLOW ON U:
      CALL VECTOR(T2,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            -TETAU,PP,T1,T1,HH,T1,T1,MESH,
     &            .TRUE.,MASK%ADR(UDDL)%P)
      CALL OSDB('X=X+Y   ' , CV2 , T2 , T2 , C , MESH)
!
!-----------------------------------------------------------------------
! TERM  T31
!-----------------------------------------------------------------------
!
!  T31_1 : ADVECTION
!
!  T31_2 : (7B OF EB+AL)
!
      FORMULE(16:16)='Y'
      CALL MATRIX(BM2,'M=N     ',FORMULE,IELMH,IELMU,
     &            1.D0,HH,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
      CALL MATVEC('X=X+CTAY',CV3,BM2,PP,TETAU-1.D0,MESH)
! OLD PROGRAMMING (TBM2 FROM HN AT AN INCORRECT TIMESTEP)
!     CALL MATVEC('X=X+CAY ',CV3,TBM2,PP,(TETAU-1.D0)/TETAU,MESH)
!
!                           V
!  T31_3 : BOUNDARY TERM TB1
!                           ADJ
!
!     JMH : I HAVE 1.D0 INSTEAD OF TETAU-1.D0
      CALL VECTOR(T4,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            TETAU-1.D0,PP,T1,T1,T1,HH,T1,MESH,.TRUE.,
     &            MASK%ADR(8)%P)
      CALL OSDB('X=X+Y   ' , CV3 , T4 , T4 , C , MESH)
!     DIRICHLET ON VELOCITY V:
      CALL VECTOR(T4,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            -TETAU,PP,T1,T1,T1,HH,T1,MESH,
     &            .TRUE.,MASK%ADR(VDIR)%P)
      CALL OSDB('X=X+Y   ' , CV3 , T4 , T4 , C , MESH)
!     FREE FLOW ON V:
      CALL VECTOR(T4,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            -TETAU,PP,T1,T1,T1,HH,T1,MESH,
     &            .TRUE.,MASK%ADR(VDDL)%P)
      CALL OSDB('X=X+Y   ' , CV3 , T4 , T4 , C , MESH)
!
!-----------------------------------------------------------------------
! TERM  T22 (2 PARTS)
!-----------------------------------------------------------------------
!
!     TERM 4B OF AL       T
!     AM2 IS MASS/DT AT TIME IT+1
      CALL MATRIX(AM2,'M=N     ','MATMAS          ',IELMU,IELMU,
     &            1.D0/DT,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
      CALL MATVEC('X=X+AY  ', CV2 , AM2 , QQ , C , MESH)
!
!     MISSES AN ADVECTION TERM
!
!
!-----------------------------------------------------------------------
! TERM  T32 (2 PARTS)
!-----------------------------------------------------------------------
!
!     TERM 6B OF AL
!     AM2 IS MASS/DT AT TIME IT+1
      CALL MATVEC('X=X+AY  ', CV3 , AM2 , RR , C , MESH)
!
!     MISSES AN ADVECTION TERM
!
!-----------------------------------------------------------------------
! TERMS  T23 AND T33
!-----------------------------------------------------------------------
!
!   ADVECTION : NOT YET IMPLEMENTED
!
!-----------------------------------------------------------------------
! TERM  T24+T25 AND T34+T35
!-----------------------------------------------------------------------
!
!     T5=U/(U^2+V^2)  C     T6=V/(U^2+V^2)
      CALL OS('X=YZ    ', T7 , UU , UU , C    )
      CALL OS('X=X+YZ  ', T7 , VV , VV , C    )
      CALL OS('X=+(Y,C)', T7 , T7 , T7 , 1.D-6)
      CALL OS('X=Y/Z   ', T5 , UU , T7 , C    )
      CALL OS('X=Y/Z   ', T6 , VV , T7 , C    )
!
!     ADD TERMS TO CV2, CV3
!
!     T3=U*Q+V*R (ALREADY DONE)
      CALL OS('X=XY    ', T5 , T11 , T11 , C )
      CALL OS('X=XY    ', T6 , T11 , T11 , C )
      CALL OS('X=X+YZ  ', CV2 , T5 , T3  , C )
      CALL OS('X=X+YZ  ', CV3 , T6 , T3  , C )
!
!=======================================================================
!
!     END OF COMPUTATION OF RIGHT HAND SIDE FOR ADJOINT SYSTEM
!
!=======================================================================
!
!
!     DIRICHLET CONDITIONS FOR ADJOINT VARIABLES
      CALL OS ('X=C     ',ADJDIR,ADJDIR,ADJDIR,0.D0)
      CALL DIRICH(UNKADJ,MATADJ,RHS,ADJDIR,LIMPRO%I,
     &            TB,MESH,KDIR,MSK,MASKPT)
!
      CALL SOLVE(UNKADJ,MATADJ,RHS,TB,SLVPRO,INFOGR,MESH,TM1)
!
!     CONTRIBUTION TO COST FUNCTION
!
      CALL COST_FUNCTION(C,OPTCOST,'GRD')
!
!     PREPARES NEXT TIMESTEP
!
      CALL OS( 'X=Y     ' , HIT1 , HH  , HH  , C )
      CALL OS( 'X=Y     ' , UIT1 , UU  , UU  , C )
      CALL OS( 'X=Y     ' , VIT1 , VV  , VV  , C )
!
      IF(     INCLU2(ESTIME,'PERMANENT')
     &    .OR.INCLU2(ESTIME,'STEADY'   )  ) THEN
!
!      STEADY STATE : DOES NOT UPDATE DATA AND RESULTS,
!                     ONLY LAST TIMESTEP CONSIDERED
!
!      CALL OS( 'X=C     ' , ALPHA1 , ALPHA1 , ALPHA1 , 0.D0 )
!      CALL OS( 'X=C     ' , ALPHA2 , ALPHA2 , ALPHA2 , 0.D0 )
!      CALL OS( 'X=C     ' , ALPHA3 , ALPHA3 , ALPHA3 , 0.D0 )
!      U AND V MODIFIED BY BORD, RESET HERE (H USEFUL ??)
       CALL OS( 'X=Y     ' , H , HN  , HN  , C )
       CALL OS( 'X=Y     ' , U , UN  , UN  , C )
       CALL OS( 'X=Y     ' , V , VN  , VN  , C )
!
      ELSE
!
!      UNSTEADY STATE : UPDATES DATA AND RESULTS
!
       IF(LT.LT.NIT) THEN
!
!       HIT,.., HH,.. IN INITIAL CONDITIONS, SEE PROPIN_ADJ
        CALL OS( 'X=Y     ' , HH , HN  , HN  , C )
        CALL OS( 'X=Y     ' , UU , UN  , UN  , C )
        CALL OS( 'X=Y     ' , VV , VN  , VN  , C )
!
!       READS TELEMAC2D RESULTS (RESULTS FILE - UNIT NRES)
!       SEE ALSO CONDIN_ADJ
!
        DO I=1,2*(NVARRES+1)
          BACKSPACE NRES
        ENDDO
        CALL LITENR(VARSOR,VARCL,NRES,'STD',
     &       HIST,0,MESH%NPOIN,AT1,TEXTE,
     &       TEXRES,NVARRES,VARCLA,0,TROUVE,ALIRE,W,.FALSE.,MAXVAR)
!
!       READS THE MEASUREMENTS (REFERENCE FILE - UNIT NREF)
!
        ITER=NIT-LT
        IF(OUTINI) ITER=ITER+1
        CALL MESURES(ITER,AT-DT)
!
       ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END